;;; etaf-pi-tests.el --- Public client and protocol acceptance -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Offline subprocess tests replay recorded wire events. No API keys or Pi needed.
;;; Code:
(require 'ert)
(require 'etaf-pi)
(require 'json)
(defconst etaf-pi-test--command
  (list "python3" (expand-file-name "fixtures/fake-pi.py"
                                   (file-name-directory (or load-file-name buffer-file-name)))))
(defun etaf-pi-test--wait (predicate &optional seconds)
  "Wait bounded SECONDS for observable PREDICATE."
  (let ((end (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) end)) (sleep-for .02))
    (should (funcall predicate))))

(ert-deftest etaf-pi-test-stream-and-framing ()
  "Fragmented UTF-8/CRLF records reconstruct and settle a real captured stream."
  (let ((state (etaf-pi-state-create)) events response connection)
    (unwind-protect
        (progn
          (setq connection (etaf-pi-rpc-start
                            temporary-file-directory
                            (lambda (event) (push event events) (etaf-pi-state-accept state event))
                            etaf-pi-test--command))
          (etaf-pi-rpc-request connection "prompt" '(:message "你好\u2028世界")
                               (lambda (record) (setq response record)))
          (etaf-pi-test--wait (lambda () (cl-find "agent_settled" events :key (lambda (e) (plist-get e :type)) :test #'equal)))
          (should (eq (plist-get response :success) t))
          (should (equal (plist-get (etaf-pi-state-info state) :phase) "就绪"))
          (should (= (cl-count "assistant" (etaf-pi-state-messages state)
                              :test #'equal :key (lambda (m) (plist-get m :role))) 1))
          (should (equal (plist-get (cl-find "user" (etaf-pi-state-messages state)
                                           :test #'equal :key (lambda (m) (plist-get m :role))) :content)
                         "你好\u2028世界"))
          (should-not (plist-get (etaf-pi-rpc-info connection) :error)))
      (when connection (etaf-pi-rpc-close connection)))))

(ert-deftest etaf-pi-test-correlation-and-timeout ()
  "Replies correlate by id, with timeout rather than hanging or retrying."
  (let ((connection (etaf-pi-rpc-start temporary-file-directory #'ignore etaf-pi-test--command)) a b timeout)
    (unwind-protect
        (progn
          (etaf-pi-rpc-request connection "delay" '(:value "first") (lambda (r) (setq a r)))
          (etaf-pi-rpc-request connection "echo" '(:value "second") (lambda (r) (setq b r)))
          (etaf-pi-rpc-request connection "hang" nil (lambda (r) (setq timeout r)) .15)
          (etaf-pi-test--wait (lambda () (and a b timeout)))
          (should (equal (plist-get (plist-get a :data) :value) "first"))
          (should (equal (plist-get (plist-get b :data) :value) "second"))
          (should (eq (plist-get timeout :success) :false))
          (should (equal (plist-get timeout :command) "hang"))
          (should (= 0 (plist-get (etaf-pi-rpc-info connection) :pending))))
      (etaf-pi-rpc-close connection))))

(ert-deftest etaf-pi-test-failure-lifecycle ()
  "Malformed output and process exit are observable; pending requests finish."
  (dolist (command '("bad_json" "exit_now"))
    (let (events result connection)
      (unwind-protect
          (progn
            (setq connection (etaf-pi-rpc-start temporary-file-directory (lambda (e) (push e events)) etaf-pi-test--command))
            (etaf-pi-rpc-request connection command nil (lambda (r) (setq result r)))
            (etaf-pi-test--wait (lambda () (and result events)))
            (should (eq (plist-get result :success) :false))
            (should (plist-get (etaf-pi-rpc-info connection) :error)))
        (when connection (etaf-pi-rpc-close connection))))))

(ert-deftest etaf-pi-test-authoritative-end-and-retry ()
  "Block indexes remain distinct; final messages replace deltas, only settled ends a run."
  (let ((state (etaf-pi-state-create)))
    (dolist (event '((:type "agent_start")
                     (:type "message_start" :message (:role "assistant" :content nil))
                     (:type "message_update" :assistantMessageEvent (:type "thinking_delta" :contentIndex 0 :delta "思考"))
                     (:type "message_update" :assistantMessageEvent (:type "text_delta" :contentIndex 1 :delta "draft"))))
      (etaf-pi-state-accept state event))
    (should (= 2 (length (plist-get (car (etaf-pi-state-messages state)) :content))))
    (etaf-pi-state-accept state '(:type "message_end" :message (:role "assistant" :content ((:type "text" :text "final")))))
    (should (= 1 (length (etaf-pi-state-messages state))))
    (should (equal (plist-get (car (plist-get (car (etaf-pi-state-messages state)) :content)) :text) "final"))
    (etaf-pi-state-accept state '(:type "agent_end" :willRetry t))
    (should (equal (plist-get (etaf-pi-state-info state) :phase) "回复中"))
    (etaf-pi-state-accept state '(:type "agent_settled"))
    (should (equal (plist-get (etaf-pi-state-info state) :phase) "就绪"))
    (etaf-pi-state-accept state '(:type "agent_start"))
    (etaf-pi-state-accept state '(:type "message_end" :message (:role "assistant" :stopReason "aborted" :content nil)))
    (etaf-pi-state-accept state '(:type "agent_settled"))
    (should-not (plist-get (etaf-pi-state-info state) :error))
    (should (equal (plist-get (etaf-pi-state-info state) :notice) "已停止"))))

(ert-deftest etaf-pi-test-dialog-and-shutdown ()
  "Dialog replies match ids, false stays false, shutdown is idempotent."
  (let (events connection)
    (unwind-protect
        (progn
          (setq connection (etaf-pi-rpc-start temporary-file-directory (lambda (e) (push e events)) etaf-pi-test--command))
          (etaf-pi-rpc-request connection "prompt" '(:message "/fixture-dialog"))
          (etaf-pi-test--wait (lambda () events))
          (let ((id (plist-get (car events) :id)))
            (etaf-pi-rpc-respond connection id '(:confirmed :false)))
          (etaf-pi-test--wait (lambda () (equal (plist-get (car events) :type) "notify_test")))
          (should (eq (plist-get (plist-get (car events) :answer) :confirmed) :false))
          (etaf-pi-rpc-close connection)
          (etaf-pi-rpc-close connection)
          (should-not (plist-get (etaf-pi-rpc-info connection) :alive))
          (should-error (etaf-pi-rpc-request connection "get_state") :type 'user-error))
      (when connection (etaf-pi-rpc-close connection)))))

(ert-deftest etaf-pi-test-native-client-workflow ()
  "Open, send, switch model, answer, resume and close through public APIs."
  (let ((etaf-pi-command etaf-pi-test--command) buffer)
    (unwind-protect
        (progn
          (setq buffer (etaf-pi-open temporary-file-directory t))
          (with-current-buffer buffer
            (etaf-pi-test--wait (lambda () (equal (plist-get (etaf-pi-info) :phase) "就绪")))
            (etaf-pi-send "你好")
            (etaf-pi-test--wait (lambda () (and (equal (plist-get (etaf-pi-info) :phase) "就绪")
                                               (string-match-p "工具 · read" (buffer-substring-no-properties (point-min) (point-max))))))
            (etaf-pi-select-model "fixture/alternate")
            (etaf-pi-test--wait (lambda () (equal (plist-get (plist-get (plist-get (etaf-pi-info) :session) :model) :id) "alternate")))
            (etaf-pi-select-thinking "high")
            (etaf-pi-test--wait (lambda () (equal (plist-get (plist-get (etaf-pi-info) :session) :thinkingLevel) "high")))
            (etaf-pi-send "/fixture-dialog")
            (etaf-pi-test--wait (lambda () (plist-get (etaf-pi-info) :dialogs)))
            (etaf-pi-answer nil t)
            (should-not (plist-get (etaf-pi-info) :dialogs))
            (etaf-pi-resume "/tmp/cancelled.jsonl")
            (sleep-for .15)
            (should-not (equal (plist-get (plist-get (etaf-pi-info) :session) :sessionFile) "/tmp/cancelled.jsonl"))
            (etaf-pi-new-session)
            (etaf-pi-test--wait (lambda () (equal (plist-get (plist-get (etaf-pi-info) :session) :sessionFile) "/tmp/pi-new.jsonl")))
            (should-error (etaf-pi-send "/settings") :type 'user-error)
            (etaf-pi-close))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (with-current-buffer buffer (etaf-pi-close))))))

(ert-deftest etaf-pi-test-composer-draft-and-send ()
  "The composer edits in place; a cancelled draft survives and an acked one clears."
  (let ((etaf-pi-command etaf-pi-test--command) owner)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq owner (etaf-pi-open temporary-file-directory t))
            (etaf-pi-test--wait (lambda () (equal (plist-get (etaf-pi-info) :phase) "就绪")))
            (with-current-buffer owner
              (etaf-pi-edit)
              (insert "未发送的草稿")
              (etaf-text-input-end t))
            (should (equal (plist-get (plist-get (etaf-pi-info) :composer) :text)
                           "未发送的草稿"))
            (with-current-buffer owner
              (etaf-pi-edit)
              (should (etaf-text-input-active-p (etaf-runtime-for-buffer owner)))
              (insert "\n第二行")
              (etaf-text-input-end))
            (etaf-pi-test--wait
             (lambda ()
               (and (equal (plist-get (etaf-pi-info) :phase) "就绪")
                    (equal (plist-get (plist-get (etaf-pi-info) :composer) :text) "")
                    (with-current-buffer owner
                      (string-match-p "工具 · read"
                                      (buffer-substring-no-properties (point-min) (point-max)))))))
        (when (buffer-live-p owner) (with-current-buffer owner (etaf-pi-close))))))))

(ert-deftest etaf-pi-test-draft-failure-and-concurrent-edit ()
  "Failed sends and text edited before the ack are never discarded."
  (let ((etaf-pi-command etaf-pi-test--command) owner)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq owner (etaf-pi-open temporary-file-directory t))
            (etaf-pi-test--wait (lambda () (equal (plist-get (etaf-pi-info) :phase) "就绪")))
            (with-current-buffer owner
              (etaf-pi-edit) (insert "拒绝发送") (etaf-text-input-end))
            (etaf-pi-test--wait (lambda () (with-current-buffer owner (plist-get (etaf-pi-info) :error))))
            (should (equal (plist-get (plist-get (etaf-pi-info) :composer) :text) "拒绝发送"))
            (should-not (plist-get (plist-get (etaf-pi-info) :composer) :sending))
            ;; A delayed acknowledgement must not clear text edited meanwhile.
            (with-current-buffer owner
              (etaf-pi-edit "延迟接收") (etaf-text-input-end)
              (etaf-pi-edit) (insert "\n继续编辑"))
            (etaf-pi-test--wait
             (lambda () (not (plist-get (plist-get (etaf-pi-info) :composer) :sending))))
            (should (equal (plist-get (plist-get (etaf-pi-info) :composer) :text)
                           "延迟接收\n继续编辑"))
            (with-current-buffer owner (etaf-text-input-end t)))
        (when (buffer-live-p owner) (with-current-buffer owner (etaf-pi-close)))))))

(ert-deftest etaf-pi-test-acknowledgement-preserves-active-edit ()
  "A delayed acceptance must not disturb an active editing session."
  (let ((etaf-pi-command etaf-pi-test--command) owner)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq owner (etaf-pi-open temporary-file-directory t))
            (etaf-pi-test--wait (lambda () (equal (plist-get (etaf-pi-info) :phase) "就绪")))
            (with-current-buffer owner
              (etaf-pi-edit "延迟接收")
              (etaf-text-input-end)
              (etaf-pi-edit)
              (insert " changed"))
            (etaf-pi-test--wait
             (lambda () (not (plist-get (plist-get (etaf-pi-info) :composer) :sending))))
            (with-current-buffer owner
              (should (etaf-text-input-active-p (etaf-runtime-for-buffer owner)))
              (should (equal (plist-get (plist-get (etaf-pi-info) :composer) :text) "延迟接收 changed"))
              (etaf-text-input-end t)))
        (when (buffer-live-p owner) (with-current-buffer owner (etaf-pi-close)))))))

(ert-deftest etaf-pi-test-composer-keys-and-session-switch-guard ()
  "Shift+Enter adds a line, Enter sends, and a pending session switch blocks sends."
  (let ((etaf-pi-command etaf-pi-test--command) owner)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq owner (etaf-pi-open temporary-file-directory t))
            (etaf-pi-test--wait (lambda () (equal (plist-get (etaf-pi-info) :phase) "就绪")))
            (with-current-buffer owner
              (etaf-pi-edit)
              (insert "第一行") (execute-kbd-macro (kbd "<S-return>")) (insert "第二行")
              (should (equal (plist-get (plist-get (etaf-pi-info) :composer) :text)
                             "第一行\n第二行"))
              (execute-kbd-macro (kbd "RET")))
            (etaf-pi-test--wait
             (lambda () (equal (plist-get (plist-get (etaf-pi-info) :composer) :text) "")))
            (with-current-buffer owner
              (etaf-pi-test--wait (lambda () (let ((info (etaf-pi-info)))
                                             (and (equal (plist-get info :phase) "就绪")
                                                  (not (plist-get info :render-pending))))))
              (etaf-pi-new-session)
              (should (plist-get (etaf-pi-info) :session-changing))
              (should-error (etaf-pi-send "切换时不发送") :type 'user-error)
              (etaf-pi-test--wait (lambda () (not (plist-get (etaf-pi-info) :session-changing))))))
        (when (buffer-live-p owner) (with-current-buffer owner (etaf-pi-close)))))))

(ert-deftest etaf-pi-test-reopen-and-switch-owned-panes ()
  "Reopening from input reuses the client; another client does not nest panes."
  (let ((etaf-pi-command etaf-pi-test--command) first second count)
    (save-window-excursion
      (unwind-protect
          (progn
            (setq first (etaf-pi-open temporary-file-directory))
            (setq count (length (window-list)))
            (etaf-pi-edit)
            (should (eq first (etaf-pi-open temporary-file-directory)))
            (should (= count (length (window-list))))
            (setq second (etaf-pi-open temporary-file-directory t))
            (should (= count (length (window-list))))
            (should-not (get-buffer-window first t))
            (should (eq first (etaf-pi-open temporary-file-directory)))
            (should (= count (length (window-list))))
            (should-not (get-buffer-window second t)))
        (dolist (buffer (list first second))
          (when (buffer-live-p buffer) (with-current-buffer buffer (etaf-pi-close))))))))

(ert-deftest etaf-pi-test-reentrant-listener-preserves-wire-order ()
  "A listener yielding for subprocess output cannot overtake queued wire records."
  (let (connection order)
    (unwind-protect
        (progn
          (setq connection
                (etaf-pi-rpc-start
                 temporary-file-directory
                 (lambda (event)
                   (when (equal (plist-get event :type) "test_event")
                     (setq order (append order (list (plist-get event :sequence))))
                     (when (= (plist-get event :sequence) 1)
                       (etaf-pi-rpc-request connection "echo" nil
                                            (lambda (_) (setq order (append order '(echo)))))
                       (let ((deadline (+ (float-time) .1)))
                         (while (< (float-time) deadline) (accept-process-output nil .01))))))
                 etaf-pi-test--command))
          (etaf-pi-rpc-request connection "burst")
          (etaf-pi-test--wait (lambda () (= (length order) 3)))
          (should (equal order '(1 2 echo))))
      (when connection (etaf-pi-rpc-close connection)))))

(provide 'etaf-pi-tests)
;;; etaf-pi-tests.el ends here
