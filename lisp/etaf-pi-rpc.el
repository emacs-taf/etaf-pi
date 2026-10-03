;;; etaf-pi-rpc.el --- Asynchronous Pi JSONL transport -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Pipes, not terminals.  No renderer or modal input runs in the process filter.
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(cl-defstruct (etaf-pi--connection (:constructor etaf-pi--make-connection))
  process stderr (tail "") (serial 0) pending listener closed failure)

(defun etaf-pi--deliver (callback value)
  "Deliver VALUE to CALLBACK without breaking transport consumption."
  (when callback
    (condition-case err (funcall callback value)
      (error (message "ETAF Pi callback: %s" (error-message-string err))))))

(defun etaf-pi--emit (connection record)
  "Deliver RECORD to CONNECTION's listener."
  (etaf-pi--deliver (etaf-pi--connection-listener connection) record))

(defun etaf-pi--finish-request (connection id record)
  "Resolve one request ID with RECORD exactly once."
  (when-let* ((entry (gethash id (etaf-pi--connection-pending connection))))
    (remhash id (etaf-pi--connection-pending connection))
    (cancel-timer (cdr entry))
    (etaf-pi--deliver (car entry) record)))

(defun etaf-pi--fail-pending (connection reason)
  "Finish outstanding requests on CONNECTION with REASON."
  (let (ids)
    (maphash (lambda (id _value) (push id ids))
             (etaf-pi--connection-pending connection))
    (dolist (id ids)
      (etaf-pi--finish-request
       connection id (list :type "response" :id id :success :false
                           :error reason)))))

(defun etaf-pi--filter (connection chunk)
  "Decode CHUNK on LF boundaries, retaining CONNECTION's incomplete tail."
  (unless (etaf-pi--connection-closed connection)
    (let ((text (concat (etaf-pi--connection-tail connection) chunk))
          (start 0) lines)
      (while (string-match "\n" text start)
        (push (substring text start (match-beginning 0)) lines)
        (setq start (match-end 0)))
      ;; Store residual bytes before calling consumers, which may send commands.
      (setf (etaf-pi--connection-tail connection) (substring text start))
      (condition-case err
          (progn
            (when (> (length (etaf-pi--connection-tail connection)) (* 16 1024 1024))
              (error "RPC record exceeds 16 MiB"))
            (dolist (line (nreverse lines))
              (unless (string-empty-p (string-trim line))
                (let ((record (json-parse-string (string-trim-right line "\r")
                                                 :object-type 'plist :array-type 'list
                                                 :null-object nil :false-object :false)))
                  (if (equal (plist-get record :type) "response")
                      (etaf-pi--finish-request connection (plist-get record :id) record)
                    (etaf-pi--emit connection record))))))
        (error
         (setf (etaf-pi--connection-failure connection) (error-message-string err))
         (etaf-pi--emit connection (list :type "client_error" :error (error-message-string err)))
         (etaf-pi-rpc-close connection))))))

(defun etaf-pi-rpc-start (directory listener &optional command)
  "Start Pi in DIRECTORY, delivering events to LISTENER; return a connection.
COMMAND is an argv list, defaulting to (\"pi\" \"--mode\" \"rpc\").
No shell is used.  RPC responses go to request callbacks, events to LISTENER."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (connection (etaf-pi--make-connection
                      :listener listener :pending (make-hash-table :test #'equal)))
         (stderr (generate-new-buffer " *ETAF Pi stderr*")))
    (setf (etaf-pi--connection-stderr connection) stderr)
    (condition-case err
        (let ((process
               (make-process
                :name "etaf-pi" :command (or command '("pi" "--mode" "rpc"))
                :connection-type 'pipe :coding 'utf-8-unix :noquery t :stderr stderr
                :filter (lambda (_process chunk) (etaf-pi--filter connection chunk))
                :sentinel
                (lambda (process event)
                  (when (memq (process-status process) '(exit signal failed))
                    (etaf-pi--fail-pending connection (string-trim event))
                    (unless (etaf-pi--connection-closed connection)
                      (setf (etaf-pi--connection-failure connection) (string-trim event))
                      (etaf-pi--emit connection (list :type "client_exit"
                                                    :error (string-trim event)
                                                    :exitCode (process-exit-status process)))))))))
          (setf (etaf-pi--connection-process connection) process)
          (when-let* ((stderr-process (get-buffer-process stderr)))
            (set-process-query-on-exit-flag stderr-process nil)
            (set-process-filter
             stderr-process
             (lambda (_process text)
               (when (buffer-live-p stderr)
                 (with-current-buffer stderr
                   (goto-char (point-max)) (insert text)
                   (when (> (buffer-size) 65536)
                     (delete-region (point-min) (- (point-max) 65536))))))))
          connection)
      (error (kill-buffer stderr) (signal (car err) (cdr err))))))

(defun etaf-pi-rpc-request (connection type &optional fields callback timeout)
  "Send TYPE and FIELDS on CONNECTION; call CALLBACK with its response.
Return a request id.  TIMEOUT defaults to 30 seconds and never retries a write.
A timeout reports an unknown outcome, not rollback."
  (unless (and (not (etaf-pi--connection-closed connection))
               (process-live-p (etaf-pi--connection-process connection)))
    (user-error "Pi 未连接；请重新打开会话"))
  (let* ((id (format "emacs-%d" (cl-incf (etaf-pi--connection-serial connection))))
         (timer (run-at-time
                 (or timeout 30) nil
                 (lambda ()
                   (etaf-pi--finish-request
                    connection id (list :type "response" :id id :success :false
                                        :error "RPC 超时；结果未知，未自动重试")))))
         (record (append (list :id id :type type) fields)))
    (puthash id (cons callback timer) (etaf-pi--connection-pending connection))
    (condition-case err
        (process-send-string (etaf-pi--connection-process connection)
                             (concat (json-serialize record :null-object nil :false-object :false) "\n"))
      (error (etaf-pi--finish-request connection id
                                      (list :type "response" :id id :success :false
                                            :error (error-message-string err)))))
    id))

(defun etaf-pi-rpc-respond (connection id fields)
  "Answer extension dialog ID on CONNECTION with FIELDS.
FIELDS is (:value STRING), (:confirmed BOOL), or (:cancelled t).
Use :false for JSON false.  This sends no ordinary RPC request."
  (when (and (not (etaf-pi--connection-closed connection))
             (process-live-p (etaf-pi--connection-process connection)))
    (process-send-string
     (etaf-pi--connection-process connection)
     (concat (json-serialize (append (list :type "extension_ui_response" :id id) fields)
                             :null-object nil :false-object :false) "\n"))))

(defun etaf-pi-rpc-info (connection)
  "Return CONNECTION's observable liveness, pending count, error and stderr."
  (let ((process (etaf-pi--connection-process connection))
        (stderr (etaf-pi--connection-stderr connection)))
    (list :alive (and (not (etaf-pi--connection-closed connection))
                      process (process-live-p process) t)
          :pid (and process (process-id process))
          :pending (hash-table-count (etaf-pi--connection-pending connection))
          :error (etaf-pi--connection-failure connection)
          :stderr (if (buffer-live-p stderr)
                      (with-current-buffer stderr
                        (buffer-substring-no-properties (max (point-min) (- (point-max) 8192))
                                                        (point-max))) ""))))

(defun etaf-pi-rpc-close (connection)
  "Close CONNECTION idempotently; EOF first, terminate this process after 2s."
  (unless (etaf-pi--connection-closed connection)
    (setf (etaf-pi--connection-closed connection) t)
    (etaf-pi--fail-pending connection "连接已关闭")
    (let ((process (etaf-pi--connection-process connection))
          (stderr (etaf-pi--connection-stderr connection)))
      (when (process-live-p process) (process-send-eof process))
      (run-at-time 2 nil
                   (lambda ()
                     (when (process-live-p process) (delete-process process))
                     (when (buffer-live-p stderr) (kill-buffer stderr)))))))

(provide 'etaf-pi-rpc)
;;; etaf-pi-rpc.el ends here
