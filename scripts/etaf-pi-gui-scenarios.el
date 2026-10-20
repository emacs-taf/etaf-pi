;;; etaf-pi-gui-scenarios.el --- Native conversation acceptance -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Run in an owned graphical instance. Uses real command-loop keys and shared
;; control activation, with an offline protocol peer. Never raises a frame.
;;; Code:
(require 'cl-lib)
(require 'etaf-pi)
(require 'emacs-gui-verifier)
(defconst etaf-pi-gui--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defvar etaf-pi-gui--buffer nil)

(defun etaf-pi-gui--main ()
  "Select only the owned transcript window."
  (if-let* ((window (get-buffer-window etaf-pi-gui--buffer t)))
      (select-window window)
    (switch-to-buffer etaf-pi-gui--buffer)))

(defun etaf-pi-gui--type (key text)
  "Invoke KEY and enter TEXT through the actual minibuffer command loop."
  (execute-kbd-macro (vconcat (kbd key) (string-to-vector text) (kbd "RET"))))

(defun etaf-pi-gui--focus (ref)
  "Focus REF in its visible application surface or navigation pane."
  (let* ((info (with-current-buffer etaf-pi-gui--buffer (etaf-pi-info)))
         (navigation (plist-get info :navigation))
         (buffer (if (and navigation
                          (memq ref '(pi-new pi-resume pi-follow pi-help)))
                     (get-buffer navigation)
                   etaf-pi-gui--buffer)))
    (select-window (get-buffer-window buffer t))
    (etaf-focus (etaf-runtime-for-buffer buffer) ref)))

(defun etaf-pi-gui--press (ref)
  "Activate shared control REF using ETAF focus plus RET."
  (etaf-pi-gui--focus ref)
  (execute-kbd-macro (kbd "RET")))

(defun etaf-pi-gui--idle (_)
  "Check settled backend facts rather than a timer delay."
  (and (equal (plist-get (etaf-pi-info) :phase) "就绪")
       (= (plist-get (plist-get (etaf-pi-info) :connection) :pending) 0)))

(defun etaf-pi-gui--geometry ()
  "Check complete canvas, line heights and no horizontal overflow.
A native session edits raw buffer text ahead of its next publication, so its
transient pixel metrics are exempt from the published-canvas checks."
  (let* ((window (get-buffer-window etaf-pi-gui--buffer))
         (size (and window (window-text-pixel-size window)))
         (editing (and window (plist-get (plist-get (etaf-pi-info) :composer) :active))))
    (and window
         (equal (plist-get (etaf-pi-info) :phase) (plist-get (etaf-pi-info) :controls-phase))
         (or (not (equal (plist-get (etaf-pi-info) :phase) "就绪"))
             (not (plist-get (etaf-pi-info) :render-pending)))
         (equal (plist-get (etaf-pi-info) :viewport)
                (list (window-body-width window) (window-body-height window)))
         (or editing (etaf-gui-verifier-canvas-filled-p etaf-pi-gui--buffer))
         (or editing (<= (car size) (window-body-width window t)))
         (= (window-hscroll window) 0)
         (let ((nav (plist-get (etaf-pi-info) :navigation)))
           (or (not nav) (etaf-gui-verifier-canvas-filled-p (get-buffer nav)))))))

(defun etaf-pi-gui--action (id execute &optional ready screenshot)
  "Build action ID with EXECUTE and observable READY, optionally SCREENSHOT."
  (let (capture-ready-at)
   (etaf-gui-verifier-action-create
   :id id :screenshot screenshot
   :execute (lambda (context)
              (etaf-pi-gui--main) (funcall execute context)
              (setf (etaf-gui-verifier-context-target-buffer context) (window-buffer (selected-window))))
   :settled-p
   (lambda (context)
     (with-current-buffer etaf-pi-gui--buffer
       (when (and (or (not ready) (funcall ready context)) (etaf-pi-gui--geometry))
         ;; NS can retain stale pixels on obscured, unfocused frames. Request a
         ;; redraw for capture only, never focus, and never count it as latency.
         (cond
          ((not screenshot) t)
          ((not capture-ready-at)
           (redraw-display)
           ;; Emacs geometry precedes the NS backing-store resize. Yield for
           ;; its redraw before window-scoped capture; not an input benchmark.
           (setq capture-ready-at (+ (float-time) 1.0)) nil)
          (t (>= (float-time) capture-ready-at))))))
   :assertions
   (lambda (_)
     (with-current-buffer etaf-pi-gui--buffer
       (append (list (etaf-gui-verifier-assert "no backend/render error" (not (plist-get (etaf-pi-info) :error)))
                     (etaf-gui-verifier-assert "canvas fits window" (etaf-pi-gui--geometry))
                     (etaf-gui-verifier-assert
                      "composer lives in the conversation surface"
                      (let* ((info (etaf-pi-info)) (composer (plist-get info :composer))
                             (runtime (etaf-runtime-for-buffer etaf-pi-gui--buffer)))
                        (and (eq (plist-get composer :ref) 'pi-composer)
                             (etaf-host-ref-bounds runtime 'pi-composer)
                             (etaf-host-ref-bounds runtime 'pi-send))))
                     (etaf-gui-verifier-assert
                      "sidebar remains full height"
                      (let* ((name (plist-get (etaf-pi-info) :navigation))
                             (nav (and name (get-buffer-window name t))))
                        (or (not nav) (= (window-total-height nav) (1- (frame-height)))))))
               (etaf-gui-verifier-line-height-assertions etaf-pi-gui--buffer)))))))

(defun etaf-pi-gui-run (directory)
  "Run the offline product workflow and save evidence to DIRECTORY."
  (let* ((etaf-pi-command (list "python3" (expand-file-name "tests/fixtures/fake-pi.py" etaf-pi-gui--root)))
         (original-frame (selected-frame))
         (frame (make-frame '((name . "Pi / GUI acceptance") (no-focus-on-map . t))))
         (scenario
          (etaf-gui-verifier-scenario-create
           :name "etaf-pi-product" :claim "Native composer, retained conversation and responsive controls"
           :initialize
           (lambda (context)
             (select-frame frame) (delete-frame original-frame t)
             (set-frame-size frame 156 48)
             (setq etaf-pi-gui--buffer (etaf-pi-open etaf-pi-gui--root t))
             (setf (etaf-gui-verifier-context-target-buffer context) etaf-pi-gui--buffer))
           :actions
           (append
            (list
             (etaf-pi-gui--action "empty" #'ignore #'etaf-pi-gui--idle t)
             (etaf-pi-gui--action
              "composer" (lambda (_) (etaf-pi-gui--press 'pi-composer))
              (lambda (_) (plist-get (plist-get (etaf-pi-info) :composer) :active)) t)
             (etaf-pi-gui--action
              "send-multiline" (lambda (_)
                                 ;; Typed keys reach the native session; Lisp
                                 ;; insertion would bypass it.
                                 (execute-kbd-macro
                                  (vconcat "检查一下这段对话的排版。" (kbd "S-<return>")
                                           "保留代码示例，说明输入与工具的交互。"))
                                 (execute-kbd-macro (kbd "C-c C-c")))
              (lambda (_)
                (and (etaf-pi-gui--idle nil)
                     (equal (plist-get (plist-get (etaf-pi-info) :composer) :text) "")
                     (string-match-p "✔ read +完成" (buffer-substring-no-properties (point-min) (point-max))))) t)
             (etaf-pi-gui--action
              "draft-kept" (lambda (_)
                             (execute-kbd-macro (kbd "i"))
                             (execute-kbd-macro (string-to-vector "未发送的草稿"))
                             (execute-kbd-macro (kbd "C-c C-k")))
              (lambda (_)
                (let ((composer (plist-get (etaf-pi-info) :composer)))
                  (and (not (plist-get composer :active))
                       (equal (plist-get composer :text) "未发送的草稿")))))
             (etaf-pi-gui--action
              "draft-restore" (lambda (_)
                                (etaf-pi-gui--press 'pi-composer)
                                (unless (equal (plist-get (plist-get (etaf-pi-info) :composer) :text)
                                               "未发送的草稿")
                                  (error "Draft was lost")))
              (lambda (_) (plist-get (plist-get (etaf-pi-info) :composer) :active)))
             (etaf-pi-gui--action
              "send-button" (lambda (_)
                              (execute-kbd-macro (kbd "C-c C-k"))
                              (etaf-pi-gui--press 'pi-send))
              (lambda (_) (and (etaf-pi-gui--idle nil)
                               (equal (plist-get (plist-get (etaf-pi-info) :composer) :text) ""))))
             (etaf-pi-gui--action
              "edited-draft-survives-ack"
              (lambda (_)
                (etaf-pi-gui--press 'pi-composer)
                (execute-kbd-macro (string-to-vector "延迟接收"))
                (execute-kbd-macro (kbd "C-c C-c"))
                (execute-kbd-macro (kbd "i"))
                (execute-kbd-macro (string-to-vector "新的输入"))
                ;; End the session so republished layout is part of the check.
                (execute-kbd-macro (kbd "C-c C-k")))
              (lambda (_)
                (let ((composer (plist-get (etaf-pi-info) :composer)))
                  (and (etaf-pi-gui--idle nil)
                       (not (plist-get composer :sending))
                       (not (plist-get composer :active))
                       (equal (plist-get composer :text) "延迟接收新的输入")))) t)
             (etaf-pi-gui--action
              "model" (lambda (_) (etaf-pi-gui--focus 'pi-model)
                         (etaf-pi-gui--type "RET" "fixture/offline")) #'etaf-pi-gui--idle)
             (etaf-pi-gui--action
              "thinking" (lambda (_) (etaf-pi-gui--focus 'pi-thinking)
                            (etaf-pi-gui--type "RET" "high"))
              (lambda (_) (equal (plist-get (plist-get (etaf-pi-info) :session) :thinkingLevel) "high")))
             (etaf-pi-gui--action
              "dialog-request" (lambda (_)
                                 (etaf-pi-edit "/fixture-dialog")
                                 (execute-kbd-macro (kbd "C-c C-c")))
              (lambda (_) (plist-get (etaf-pi-info) :dialogs)) t)
             (etaf-pi-gui--action
              "dialog-decline" (lambda (_)
                                 (etaf-pi-gui--focus 'pi-answer)
                                 (if (or (eq (indirect-function 'yes-or-no-p) (indirect-function 'y-or-n-p))
                                         (bound-and-true-p use-short-answers))
                                     (execute-kbd-macro (kbd "RET n"))
                                   (etaf-pi-gui--type "RET" "no")))
              (lambda (_) (null (plist-get (etaf-pi-info) :dialogs))))
             (etaf-pi-gui--action
              "tool-details" (lambda (_)
                               (etaf-pi-gui--press 'pi-tools)
                               (unless (string-match-p "第一行" (buffer-substring-no-properties (point-min) (point-max)))
                                 (error "Full tool output missing"))
                               (execute-kbd-macro (kbd "q"))))
             (etaf-pi-gui--action "history-wheel" (lambda (_)
                                                   (let ((main (get-buffer-window etaf-pi-gui--buffer)))
                                                     (execute-kbd-macro
                                                      (vector (list 'wheel-up (posn-at-point (window-start main) main) 1)))))
                                  (lambda (_) (not (plist-get (etaf-pi-info) :following))) t)
             (etaf-pi-gui--action "follow-button" (lambda (_) (etaf-pi-gui--press 'pi-follow))
                                  (lambda (_) (plist-get (etaf-pi-info) :following)))
             (etaf-pi-gui--action
              "start-interruptible-task" (lambda (_) (execute-kbd-macro (kbd "i"))
                                          (execute-kbd-macro (string-to-vector "持续任务"))
                                          (execute-kbd-macro (kbd "RET")))
              (lambda (_)
                (and (equal (plist-get (etaf-pi-info) :phase) "回复中")
                     (string-match-p "停止" (buffer-substring-no-properties (point-min) (point-max))))))
             (etaf-pi-gui--action "stop-button" (lambda (_) (etaf-pi-gui--press 'pi-abort)) #'etaf-pi-gui--idle))
            (mapcar
             (lambda (size)
               (etaf-pi-gui--action
                (format "layout-%dx%d" (car size) (cadr size))
                (lambda (_) (set-frame-size frame (car size) (cadr size)))
                (lambda (_) (and (= (frame-width frame) (car size)) (= (frame-height frame) (cadr size)))) t))
             '((150 44) (100 40) (58 36) (85 22) (156 48)))
            (list
             (etaf-pi-gui--action "new-session" (lambda (_) (etaf-pi-gui--press 'pi-new))
                                  (lambda (_) (equal (plist-get (plist-get (etaf-pi-info) :session) :sessionFile) "/tmp/pi-new.jsonl")))
             (etaf-pi-gui--action "resume" (lambda (_)
                                            (etaf-pi-edit "/resume /tmp/pi-fixture.jsonl")
                                            (execute-kbd-macro (kbd "C-c C-c")))
                                  (lambda (_) (equal (plist-get (plist-get (etaf-pi-info) :session) :sessionFile) "/tmp/pi-fixture.jsonl")))
             (etaf-pi-gui--action
              "help" (lambda (_)
                       (let ((windows (window-list)))
                         (etaf-pi-gui--press 'pi-help)
                         (unless (and (eq (window-buffer) etaf-pi-gui--buffer)
                                      (equal windows (window-list))
                                      (string-match-p
                                       "快捷键 · 只读"
                                       (buffer-substring-no-properties (point-min) (point-max))))
                           (error "Help left the client surface"))
                         (execute-kbd-macro (kbd "/"))
                         (execute-kbd-macro "?")
                         (execute-kbd-macro (kbd "<escape>"))
                         (unless (string-match-p
                                  "快捷键 · 只读"
                                  (buffer-substring-no-properties (point-min) (point-max)))
                           (error "Search Escape closed two layers"))
                         (execute-kbd-macro (kbd "<escape>"))
                         (when (etaf-help-close)
                           (error "Help did not close")))))))
           :completion (lambda (_) (with-current-buffer etaf-pi-gui--buffer
                                      (not (plist-get (etaf-pi-info) :error)))))))
    (unwind-protect
        (etaf-gui-verifier-run scenario directory)
      (when (buffer-live-p etaf-pi-gui--buffer)
        (with-current-buffer etaf-pi-gui--buffer
          (let ((text (buffer-substring-no-properties (point-min) (point-max))) (info (etaf-pi-info)))
            (with-temp-file (expand-file-name "client.txt" directory) (insert (format "%S\n\n%s" info text))))
          (etaf-pi-close))))))
(provide 'etaf-pi-gui-scenarios)
;;; etaf-pi-gui-scenarios.el ends here
