;;; etaf-pi-app.el --- Native Pi session buffers -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Owns processes, timers, windows and commands.  Filters only update state;
;; coalesced paints touch visible retained Hosts.  Dialogs require user action.
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-pi-rpc)
(require 'etaf-pi-model)
(require 'etaf-pi-views)

(defgroup etaf-pi nil "Native Pi client." :group 'tools)
(defcustom etaf-pi-command '("pi" "--mode" "rpc")
  "Pi argv, without a shell.  Tests can bind a protocol-compatible executable."
  :type '(repeat string) :group 'etaf-pi)
(defcustom etaf-pi-refresh-interval 0.08
  "Minimum seconds between coalesced streaming paints."
  :type 'number :group 'etaf-pi)
(defvar etaf-pi-input-history nil)
(defvar-local etaf-pi--state nil)
(defvar-local etaf-pi--connection nil)
(defvar-local etaf-pi--paint-timer nil)
(defvar-local etaf-pi--painted nil)
(defvar-local etaf-pi--shape nil)
(defvar-local etaf-pi--line-cache nil)
(defvar-local etaf-pi--follow t)
(defvar-local etaf-pi--offset 0)
(defvar-local etaf-pi--total-lines 0)
(defvar-local etaf-pi--disposed nil)
(defvar-local etaf-pi--editor-owner nil)
(defvar-local etaf-pi--editor-dialog nil)
(defvar-local etaf-pi--composer nil)
(defvar-local etaf-pi--editor-sending nil)
(defvar-local etaf-pi--page-rows 20)

(defun etaf-pi--current ()
  "Require a client buffer."
  (unless etaf-pi--state (user-error "当前不是 ETAF Pi 会话"))
  etaf-pi--state)

(defun etaf-pi--schedule-paint ()
  "Schedule one paint; no timers run while idle."
  (unless (or etaf-pi--disposed etaf-pi--paint-timer)
    (let ((buffer (current-buffer)))
      (setq etaf-pi--paint-timer
            (run-at-time (max 0.02 etaf-pi-refresh-interval) nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq etaf-pi--paint-timer nil)
                               (unless etaf-pi--disposed
                                 (condition-case err (etaf-pi--paint)
                                   (error
                                    (setf (etaf-pi--state-error etaf-pi--state)
                                          (format "显示失败：%s" (error-message-string err)))
                                    (message "ETAF Pi: %s" (error-message-string err)))))))))))))

(defun etaf-pi--event (buffer record)
  "Consume RECORD for BUFFER without modal prompts or rendering in filters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless etaf-pi--disposed
        (when (and (equal (plist-get record :type) "extension_ui_request")
                   (numberp (plist-get record :timeout)))
          (setq record (plist-put record :deadline (+ (float-time) (/ (plist-get record :timeout) 1000.0)))))
        (etaf-pi-state-accept etaf-pi--state record)
        (etaf-pi--schedule-paint)
        (when (equal (plist-get record :type) "agent_settled")
          (etaf-pi-refresh))))))

(defun etaf-pi--request (type &optional fields after timeout)
  "Send TYPE/FIELDS; project reply before AFTER callback."
  (etaf-pi--current)
  (let ((buffer (current-buffer)))
    (etaf-pi-rpc-request
     etaf-pi--connection type fields
     (lambda (record)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (unless etaf-pi--disposed
             (etaf-pi-state-accept etaf-pi--state record)
             (when (equal (plist-get record :command) "get_messages")
               (clrhash etaf-pi--line-cache))
             (etaf-pi--schedule-paint)
             (when (and (equal (plist-get record :command) "prompt")
                        (buffer-live-p etaf-pi--composer))
               (with-current-buffer etaf-pi--composer
                 (setq etaf-pi--editor-sending nil)
                 (force-mode-line-update)))
             (when (and after (eq (plist-get record :success) t))
               (funcall after record)))))) timeout)))

(defun etaf-pi-refresh ()
  "Refresh authoritative session state, usage and messages asynchronously."
  (interactive)
  (etaf-pi--request "get_state")
  (etaf-pi--request "get_session_stats")
  (etaf-pi--request "get_messages"))

(defun etaf-pi--summary (width)
  "Build the status line at WIDTH cells."
  (let* ((state etaf-pi--state)
         (session (etaf-pi--state-session state))
         (model (plist-get session :model))
         (stats (etaf-pi--state-stats state))
         (context (plist-get stats :contextUsage))
         (usage (if (numberp (plist-get context :percent))
                    (format " · 上下文 %.1f%%" (plist-get context :percent)) ""))
         (base (format "%s · %s · 思考 %s%s · $%.4f%s%s"
                       (etaf-pi--state-phase state)
                       (or (plist-get model :name) (plist-get model :id) "加载模型")
                       (or (plist-get session :thinkingLevel) "…") usage
                       (or (plist-get stats :cost) 0.0)
                       (if (> (etaf-pi--state-queue state) 0)
                           (format " · 排队 %d" (etaf-pi--state-queue state)) "")
                       (if (etaf-pi--state-dialogs state) " · 按 a 处理确认" ""))))
    (truncate-string-to-width base width nil nil "…")))

(defun etaf-pi--transcript (width)
  "Return wrapped transcript at WIDTH, caching immutable completed messages."
  (let (lines)
    (dolist (message (etaf-pi--state-messages etaf-pi--state))
      (let* ((cached (gethash message etaf-pi--line-cache))
             (rendered (if (and cached (= (car cached) width)) (cdr cached)
                         (etaf-pi--message-lines message width))))
        (puthash message (cons width rendered) etaf-pi--line-cache)
        (push rendered lines)))
    (setq lines (apply #'append (nreverse lines)))
    (when (etaf-pi--state-partial etaf-pi--state)
      (let ((partial (etaf-pi--partial-message etaf-pi--state)))
        (setq lines (append lines (etaf-pi--message-lines partial width)))))
    (dolist (tool (etaf-pi--state-tools etaf-pi--state))
      (setq lines (append lines (etaf-pi--wrap
                                (format "工具 · %s · %s" (plist-get tool :toolName)
                                        (if (equal (plist-get tool :type) "tool_execution_end") "完成" "执行中")) width))))
    (when (null lines)
      (setq lines
            (append (list "" (etaf-pi--ink "从一个想法开始。" :ink nil t) "")
                    (etaf-pi--wrap "描述你想做的事，把实现、检查和验证交给 Pi。" width)
                    (list "" (etaf-pi--ink "例如" :muted))
                    (etaf-pi--wrap "帮我理解这个项目，或一起实现一个新功能。" width)
                    (list "" (etaf-pi--ink "按 i 写消息，支持多行与粘贴代码。" :muted)))))
    (dolist (notice (list (etaf-pi--state-error etaf-pi--state)
                           (etaf-pi--state-notice etaf-pi--state)))
      (when (and notice (not (string-empty-p notice)))
        (setq lines (append lines '("") (etaf-pi--wrap notice width)))))
    lines))

(defun etaf-pi--footer (width)
  "Describe current work or history position within WIDTH cells."
  (let* ((state etaf-pi--state)
         (text (cond
                ((etaf-pi--state-error state) (concat "出现问题 · " (etaf-pi--state-error state) " · R 重连"))
                ((etaf-pi--state-dialogs state) "等待你的确认 · 按 a 查看，不会自动批准")
                ((not etaf-pi--follow) (format "查看历史 · %d / %d 行 · g 回到最新" (1+ etaf-pi--offset) etaf-pi--total-lines))
                ((> (etaf-pi--state-queue state) 0) (format "继续处理 · %d 条消息等待发送" (etaf-pi--state-queue state)))
                ((not (equal (etaf-pi--state-phase state) "就绪"))
                 (concat (etaf-pi--state-phase state) " · 你可以继续输入，也可以随时停止"))
                (t "就绪 · i 写消息   k 看历史   : 命令"))))
    (etaf-pi--ink (truncate-string-to-width text width nil nil "…")
                  (cond ((etaf-pi--state-error state) :error)
                        ((etaf-pi--state-dialogs state) :accent) (t :muted)))))

(defun etaf-pi--paint (&optional rebuild)
  "Paint the visible transcript. REBUILD replaces only the layout structure."
  (let* ((window (get-buffer-window (current-buffer) t))
         (width (max 28 (if window (window-body-width window) 100)))
         (height (max 9 (if window (window-body-height window) 35)))
         (rows (max 1 (- height 8)))
         ;; The renderer reserves edge cells. Leave another cell for font rounding.
         (measure (min 92 (max 16 (- width 10))))
         (all (etaf-pi--transcript measure))
         (start (if etaf-pi--follow (max 0 (- (length all) rows))
                  (min etaf-pi--offset (max 0 (- (length all) rows)))))
         (lines (cl-subseq all start (min (length all) (+ start rows))))
         (status (etaf-pi--summary (- width 8)))
         (runtime (etaf-runtime-for-buffer (current-buffer))))
    (setq etaf-pi--offset start etaf-pi--total-lines (length all) etaf-pi--page-rows rows)
    (setq lines (append lines (make-list (- rows (length lines)) "")))
    (let ((footer (etaf-pi--footer (- width 8))))
      (if (or rebuild (null runtime) (not (equal etaf-pi--shape (list width height))))
          (let ((buffer (current-buffer)))
            (etaf-mount
             buffer
             (etaf-pi--shell-view
              (file-name-nondirectory (directory-file-name default-directory))
              status lines width measure footer
              (list :compose (lambda ()
                               (with-current-buffer buffer
                                 (if (and (buffer-live-p etaf-pi--composer) (> (buffer-size etaf-pi--composer) 0))
                                     (with-current-buffer etaf-pi--composer (etaf-pi-editor-send))
                                   (etaf-pi-edit))))
                    :answer (lambda () (with-current-buffer buffer (call-interactively #'etaf-pi-answer)))
                    :abort (lambda () (with-current-buffer buffer (etaf-pi-abort)))
                    :model (lambda () (with-current-buffer buffer (etaf-pi-select-model)))
                    :thinking (lambda () (with-current-buffer buffer (etaf-pi-select-thinking)))
                    :new (lambda () (with-current-buffer buffer (etaf-pi-new-session)))
                    :resume (lambda () (with-current-buffer buffer (call-interactively #'etaf-pi-resume)))
                    :tools (lambda () (with-current-buffer buffer (etaf-pi-show-tools)))
                    :help #'etaf-pi-help
                    :follow (lambda () (with-current-buffer buffer (etaf-pi-follow)))))
             (list :viewport-height height))
            (setq etaf-pi--shape (list width height)))
        (let (updates)
          (unless (equal-including-properties status (car etaf-pi--painted))
            (push (cons 'etaf-pi-status status) updates))
          (unless (equal-including-properties footer (cadr etaf-pi--painted))
            (push (cons 'etaf-pi-footer footer) updates))
          (unless (and (= (length lines) (length (cddr etaf-pi--painted)))
                       (cl-every #'equal-including-properties lines (cddr etaf-pi--painted)))
            (push (cons 'etaf-pi-transcript
                        (mapconcat (lambda (line) (if (string-empty-p line) " " line)) lines "\n")) updates))
          (when updates (etaf-runtime-update-host-contents runtime (nreverse updates)))))
      (setq etaf-pi--painted (append (list status footer) lines)))
    (setq-local cursor-type nil)
    (when window
      (set-window-hscroll window 0)
      (set-window-start window (point-min)))))

(defun etaf-pi-info ()
  "Return current client facts, including backend liveness and visible viewport."
  (etaf-pi--current)
  (append (etaf-pi-state-info etaf-pi--state)
          (list :connection (when etaf-pi--connection (etaf-pi-rpc-info etaf-pi--connection))
                :directory default-directory :offset etaf-pi--offset
                :total-lines etaf-pi--total-lines :following etaf-pi--follow
                :viewport (copy-sequence etaf-pi--shape)
                :composer (when (buffer-live-p etaf-pi--composer)
                            (with-current-buffer etaf-pi--composer
                              (list :buffer (buffer-name) :characters (buffer-size)
                                    :visible (and (get-buffer-window (current-buffer) t) t)
                                    :sending etaf-pi--editor-sending))))))

(defun etaf-pi-send (text &optional behavior)
  "Send TEXT or a supported slash command.  BEHAVIOR is steer or followUp.
Running prompts default to followUp, so typing never drops an instruction."
  (etaf-pi--current)
  (unless (string-empty-p (string-trim text))
    (setq etaf-pi--follow t)
    (if (string-prefix-p "/" text)
        (etaf-pi--slash text)
      (setf (etaf-pi--state-draft etaf-pi--state) text)
      (etaf-pi--request
       "prompt" (list :message text :streamingBehavior (or behavior "followUp"))
       (lambda (_)
         (when (equal (etaf-pi--state-draft etaf-pi--state) text)
           (setf (etaf-pi--state-draft etaf-pi--state) nil))
         (when (buffer-live-p etaf-pi--composer)
           (with-current-buffer etaf-pi--composer
             (when (equal text (buffer-substring-no-properties (point-min) (point-max)))
               (erase-buffer) (set-buffer-modified-p nil))))) 120))))

(defun etaf-pi-compose (text)
  "Read and send TEXT through native minibuffer editing with command completion."
  (interactive
   (progn
     (etaf-pi--current)
     (list (completing-read
            "Pi > "
            (append '("/new" "/resume" "/model" "/thinking" "/abort" "/compact" "/quit" "/help")
                    (mapcar (lambda (command) (concat "/" (plist-get command :name)))
                            (etaf-pi--state-commands etaf-pi--state)))
            nil nil (etaf-pi--state-draft etaf-pi--state) 'etaf-pi-input-history))))
  (etaf-pi-send text))

(defun etaf-pi--slash (text)
  "Dispatch TEXT without leaking unsupported TUI commands into a model prompt."
  (let* ((words (split-string text " +" t)) (name (car words)) (arg (string-join (cdr words) " ")))
    (pcase name
      ("/new" (etaf-pi-new-session))
      ("/resume" (if (string-empty-p arg) (call-interactively #'etaf-pi-resume) (etaf-pi-resume arg)))
      ("/model" (etaf-pi-select-model (unless (string-empty-p arg) arg)))
      ("/thinking" (etaf-pi-select-thinking (unless (string-empty-p arg) arg)))
      ("/abort" (etaf-pi-abort))
      ("/compact" (etaf-pi--require-idle) (etaf-pi--request "compact" nil (lambda (_) (etaf-pi-refresh)) 300))
      ("/quit" (etaf-pi-close))
      ("/help" (etaf-pi-help))
      (_ (if (cl-find (substring name 1) (etaf-pi--state-commands etaf-pi--state)
                      :test #'equal :key (lambda (command) (plist-get command :name)))
             (etaf-pi--request "prompt" (list :message text :streamingBehavior "followUp") nil 120)
           (user-error "未知命令 %s；按 TAB 选择已加载的命令" name))))))

(defun etaf-pi-abort ()
  "Clear queued messages before aborting, preserving recovered text as a draft."
  (interactive)
  (etaf-pi--request
   "clear_queue" nil
   (lambda (record)
     (let ((data (plist-get record :data)))
       (let ((recovered (string-join (append (plist-get data :steering) (plist-get data :followUp)) "\n\n")))
         (unless (string-empty-p recovered)
           (setf (etaf-pi--state-draft etaf-pi--state)
                 (string-join (delq nil (list (etaf-pi--state-draft etaf-pi--state) recovered)) "\n\n"))
           (when (buffer-live-p etaf-pi--composer)
             (with-current-buffer etaf-pi--composer
               (goto-char (point-max))
               (unless (= (buffer-size) 0) (insert "\n\n"))
               (insert recovered))))))
     (etaf-pi--request "abort" nil (lambda (_) (etaf-pi-refresh)) 60))))

(defun etaf-pi--require-idle ()
  "Reject a session mutation during active work."
  (unless (equal (etaf-pi--state-phase (etaf-pi--current)) "就绪")
    (user-error "Pi 仍在工作；先按 C-c C-k 停止")))

(defun etaf-pi-new-session ()
  "Start a new Pi session without deleting the previous saved session."
  (interactive)
  (etaf-pi--require-idle)
  (etaf-pi--request "new_session" nil (lambda (_) (clrhash etaf-pi--line-cache) (etaf-pi-refresh)) 120))

(defun etaf-pi-resume (file)
  "Resume FILE, a Pi session JSONL file.  Never write it directly."
  (interactive
   (list (read-file-name "恢复 Pi 会话："
                         (let ((file (plist-get (etaf-pi--state-session (etaf-pi--current)) :sessionFile)))
                           (if file (file-name-directory file) (expand-file-name "~/.pi/agent/sessions/")))
                         nil t)))
  (etaf-pi--require-idle)
  (let ((target (expand-file-name file)))
    (dolist (buffer (buffer-list))
      (when (and (not (eq buffer (current-buffer)))
                 (buffer-local-value 'etaf-pi--state buffer))
        (with-current-buffer buffer
          (when (equal target (plist-get (etaf-pi--state-session etaf-pi--state) :sessionFile))
            (user-error "这个会话已在 %s 打开" (buffer-name))))))
    (etaf-pi--request "switch_session" (list :sessionPath target)
                      (lambda (_) (clrhash etaf-pi--line-cache) (etaf-pi-refresh)) 120)))

(defun etaf-pi-select-model (&optional name)
  "Select model NAME (provider/id), or ask through native completion."
  (interactive)
  (etaf-pi--require-idle)
  (let* ((models (etaf-pi--state-models etaf-pi--state))
         (options (mapcar (lambda (model) (concat (plist-get model :provider) "/" (plist-get model :id))) models))
         (selected (or name (completing-read "模型：" options nil t)))
         (slash (string-match "/" selected)))
    (unless slash (user-error "模型应写作 provider/id"))
    (etaf-pi--request "set_model" (list :provider (substring selected 0 slash) :modelId (substring selected (1+ slash)))
                      (lambda (_) (etaf-pi-refresh) (etaf-pi--request "get_available_thinking_levels")))))

(defun etaf-pi-select-thinking (&optional level)
  "Select reasoning LEVEL, prompting from the backend-supported set when nil."
  (interactive)
  (etaf-pi--require-idle)
  (etaf-pi--request "set_thinking_level"
                    (list :level (or level (completing-read "思考级别：" (etaf-pi--state-levels etaf-pi--state) nil t)))
                    (lambda (_) (etaf-pi--request "get_state"))))

(defun etaf-pi-answer (&optional value cancel)
  "Answer the next extension dialog with VALUE, or CANCEL it.
Interactively use native completion/confirmation. No unattended approval."
  (interactive)
  (etaf-pi--current)
  (let* ((dialog (car (etaf-pi--state-dialogs etaf-pi--state)))
         (method (plist-get dialog :method)) (deadline (plist-get dialog :deadline))
         (title (concat (or (plist-get dialog :title) "Pi") " ")))
    (unless dialog (user-error "没有待处理的确认"))
    (if (and deadline (> (float-time) deadline))
        (progn (pop (etaf-pi--state-dialogs etaf-pi--state))
               (etaf-pi--schedule-paint) (user-error "这个确认已经超时，没有放行"))
      (if (and (called-interactively-p 'any) (equal method "editor"))
          (etaf-pi-edit (plist-get dialog :prefill) dialog)
        (condition-case nil
            (let ((answer
                   (if cancel '(:cancelled t)
                     (if (equal method "confirm")
                         (list :confirmed
                               (if (if (called-interactively-p 'any)
                                       (let ((use-dialog-box nil))
                                         (yes-or-no-p (concat title (or (plist-get dialog :message) "") " ")))
                                     value) t :false))
                       (list :value (or value
                                        (if (equal method "select")
                                            (completing-read title (plist-get dialog :options) nil t)
                                          (read-string title))))))))
              (if (and deadline (> (float-time) deadline))
                  (setf (etaf-pi--state-notice etaf-pi--state) "确认已过期，没有发送回答")
                (etaf-pi-rpc-respond etaf-pi--connection (plist-get dialog :id) answer))
              (pop (etaf-pi--state-dialogs etaf-pi--state)))
          (quit (etaf-pi-rpc-respond etaf-pi--connection (plist-get dialog :id) '(:cancelled t))
                (pop (etaf-pi--state-dialogs etaf-pi--state))))
        (etaf-pi--schedule-paint)))))

(defvar etaf-pi-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'etaf-pi-editor-send)
    (define-key map (kbd "C-c C-k") #'etaf-pi-editor-cancel)
    (define-key map (kbd "C-<return>") #'etaf-pi-editor-send)
    (define-key map (kbd "M-TAB") #'completion-at-point) map))

(defun etaf-pi--editor-changed (&rest _)
  "Keep the unsent draft in its owner without republishing the transcript."
  (when (and (not etaf-pi--editor-dialog) (buffer-live-p etaf-pi--editor-owner))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer etaf-pi--editor-owner
        (when etaf-pi--state (setf (etaf-pi--state-draft etaf-pi--state) text))))))

(defun etaf-pi--complete-command ()
  "Offer Pi command completion at the beginning of the native draft."
  (when (and (buffer-live-p etaf-pi--editor-owner)
             (save-excursion (goto-char (point-min)) (looking-at "/[^ \n]*"))
             (<= (point) (match-end 0)))
    (let ((commands (with-current-buffer etaf-pi--editor-owner
                      (etaf-pi--state-commands etaf-pi--state))))
      (list (point-min) (point)
            (append '("/new" "/resume" "/model" "/thinking" "/abort" "/compact" "/help")
                    (mapcar (lambda (item) (concat "/" (plist-get item :name))) commands))))))

(define-derived-mode etaf-pi-editor-mode text-mode "Pi 输入"
  "Native multiline draft. RET adds a line; C-c C-c sends without leaving it."
  (setq-local header-line-format
              (propertize "  消息  /  描述任务，或粘贴代码" 'face
                          (list :foreground (etaf-pi-color :accent) :background (etaf-pi-color :surface))))
  (setq-local mode-line-format
              '(:eval (propertize
                       (if etaf-pi--editor-sending "  正在发送… 文本会在确认接收后清空"
                         (if (< (window-body-width) 78) "  C-c C-c 发送 · C-c C-k 返回"
                           "  C-c C-c 发送    RET 换行    C-c C-k 返回    M-TAB 补全"))
                       'face (list :foreground (etaf-pi-color :muted) :background (etaf-pi-color :surface)))))
  (setq-local left-margin-width 2 right-margin-width 2 window-size-fixed 'height)
  (setq-local completion-at-point-functions '(etaf-pi--complete-command))
  (face-remap-set-base 'default (list :background (etaf-pi-color :surface) :foreground (etaf-pi-color :ink)))
  (when (bound-and-true-p display-line-numbers-mode) (display-line-numbers-mode -1))
  (visual-line-mode 1)
  (add-hook 'after-change-functions #'etaf-pi--editor-changed nil t))

(defun etaf-pi-edit (&optional text dialog)
  "Open native multiline editor with TEXT.
DIALOG optionally identifies a Pi editor request."
  (interactive)
  (etaf-pi--current)
  (let* ((owner (current-buffer))
         (initial (or text (etaf-pi--state-draft etaf-pi--state) ""))
         (buffer (get-buffer-create (format "*Pi %s: %s*" (if dialog "确认" "输入") (buffer-name owner))))
         (window (get-buffer-window owner t)))
    (unless dialog (setq etaf-pi--composer buffer))
    (with-current-buffer buffer
      (unless (derived-mode-p 'etaf-pi-editor-mode)
        (etaf-pi-editor-mode) (insert initial) (set-buffer-modified-p nil))
      (setq etaf-pi--editor-owner owner etaf-pi--editor-dialog dialog))
    (cond
     ((get-buffer-window buffer t) (select-window (get-buffer-window buffer t)))
     ((and window (>= (window-total-height window) 21))
      (let ((input (split-window window -7 'below)))
        (set-window-buffer input buffer)
        (select-window input)))
     (t (let ((display-buffer-overriding-action '((display-buffer-same-window)))) (pop-to-buffer buffer))))
    (with-current-buffer owner (etaf-pi--schedule-paint))))

(defun etaf-pi--editor-finish (send)
  "Submit when SEND, or hide the native draft without discarding it."
  (let ((owner etaf-pi--editor-owner) (dialog etaf-pi--editor-dialog)
        (text (buffer-substring-no-properties (point-min) (point-max))) (editor (current-buffer)))
    (unless (buffer-live-p owner) (user-error "所属 Pi 会话已关闭；文本仍在此缓冲区"))
    (when etaf-pi--editor-sending (user-error "正在等 Pi 确认接收，文本仍在此处"))
    (when (and send (string-empty-p (string-trim text))) (user-error "先写一点内容再发送"))
    (with-current-buffer owner
      (if dialog
          (progn
            (unless (equal (plist-get dialog :id)
                           (plist-get (car (etaf-pi--state-dialogs etaf-pi--state)) :id))
              (user-error "确认已失效；文本仍保留在编辑缓冲区"))
            (etaf-pi-answer text (not send)))
        (if send (etaf-pi-send text) (setf (etaf-pi--state-draft etaf-pi--state) text))))
    (if (and send (not dialog) (not (string-prefix-p "/" text)))
        (progn
          (setq etaf-pi--editor-sending t)
          (when-let* ((window (get-buffer-window editor t))) (select-window window)))
      (when (and send (not dialog))
        (with-current-buffer editor (erase-buffer) (set-buffer-modified-p nil)))
      (let ((window (get-buffer-window editor t)))
        (when (and window (get-buffer-window owner (window-frame window)))
          (delete-window window)))
      (let ((display-buffer-overriding-action '((display-buffer-same-window)))) (pop-to-buffer owner))
      (when dialog (with-current-buffer editor (set-buffer-modified-p nil)) (kill-buffer editor))
      (with-current-buffer owner (etaf-pi--schedule-paint)))))
(defun etaf-pi-editor-send () "Submit the native editor." (interactive) (etaf-pi--editor-finish t))
(defun etaf-pi-editor-cancel () "Return without submitting; keep a draft." (interactive) (etaf-pi--editor-finish nil))

(defun etaf-pi-page-up (&optional lines)
  "Read the previous viewport, or LINES, without following new output."
  (interactive) (etaf-pi--current)
  (setq etaf-pi--follow nil etaf-pi--offset (max 0 (- etaf-pi--offset (or lines (max 1 (1- etaf-pi--page-rows))))))
  (etaf-pi--paint))
(defun etaf-pi-page-down (&optional lines)
  "Read the next viewport, or LINES, resuming follow mode at the end."
  (interactive) (etaf-pi--current)
  (cl-incf etaf-pi--offset (or lines (max 1 (1- etaf-pi--page-rows))))
  (when (>= (+ etaf-pi--offset etaf-pi--page-rows) etaf-pi--total-lines)
    (setq etaf-pi--follow t))
  (etaf-pi--paint))
(defun etaf-pi-follow () "Follow the latest output." (interactive) (setq etaf-pi--follow t) (etaf-pi--paint))

(defun etaf-pi-show-tools ()
  "Open full tool output in a searchable native read-only buffer."
  (interactive) (etaf-pi--current)
  (let ((messages (etaf-pi-state-messages etaf-pi--state))
        (buffer (get-buffer-create (format "*Pi 工具: %s*" (buffer-name)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (item messages)
          (when (equal (plist-get item :role) "toolResult")
            (insert (format "\n%s [%s]\n\n%s\n" (plist-get item :toolName) (plist-get item :toolCallId)
                            (etaf-pi--content-text (plist-get item :content))))))
        (when (= (buffer-size) 0) (insert "还没有工具输出。")))
      (special-mode) (goto-char (point-min)))
    (let ((display-buffer-overriding-action '((display-buffer-same-window))))
      (pop-to-buffer buffer))))

(defun etaf-pi-help ()
  "Show native-client commands and RPC compatibility limits."
  (interactive)
  (with-help-window "*Pi 帮助*"
    (princ "ETAF Pi\n\ni / e  底部多行输入；RET 换行，C-c C-c 发送\n:  快速命令（TAB 补全）\nM-TAB  在输入区补全命令\na  回答待处理确认；C-g 取消而非批准\nC-c C-k  停止并收回队列\nj/k 或 PageDown/PageUp  翻阅历史\ng  跟随最新回复\nt  完整工具输出\nm  模型   T  思考级别\nn  新会话   r  恢复会话   R  断线重连\nq  关闭当前客户端\n\n/new /resume /model /thinking /abort /compact /quit /help\nSkills、模板和扩展命令经 Pi 的 get_commands 发现。\nRPC 不支持终端专用 custom() 弹窗、主题和替换输入编辑器。\n会话、认证和技能由原来的 Pi 管理。\n")))

(defun etaf-pi-reconnect ()
  "Reconnect a stopped backend, resuming the last known saved session."
  (interactive) (etaf-pi--current)
  (when (and etaf-pi--connection (plist-get (etaf-pi-rpc-info etaf-pi--connection) :alive))
    (user-error "Pi 仍在运行，不重复启动"))
  (when etaf-pi--connection (etaf-pi-rpc-close etaf-pi--connection))
  (let* ((buffer (current-buffer))
         (file (plist-get (etaf-pi--state-session etaf-pi--state) :sessionFile))
         (command (append etaf-pi-command (when file (list "--session" file)))))
    (setq etaf-pi--connection
          (etaf-pi-rpc-start default-directory (lambda (event) (etaf-pi--event buffer event)) command))
    (setf (etaf-pi--state-error etaf-pi--state) nil)
    (etaf-pi-refresh)
    (etaf-pi--request "get_commands")
    (etaf-pi--request "get_available_models")
    (etaf-pi--request "get_available_thinking_levels")))

(defun etaf-pi--dispose ()
  "Release only this buffer's resources."
  (unless etaf-pi--disposed
    (setq etaf-pi--disposed t)
    (when etaf-pi--paint-timer (cancel-timer etaf-pi--paint-timer) (setq etaf-pi--paint-timer nil))
    (when etaf-pi--connection (etaf-pi-rpc-close etaf-pi--connection))
    (when (buffer-live-p etaf-pi--composer)
      (with-current-buffer etaf-pi--composer
        (if (> (buffer-size) 0)
            (message "未发送的草稿保留在 %s" (buffer-name))
          (when-let* ((window (get-buffer-window (current-buffer) t)))
            (unless (one-window-p t (window-frame window)) (delete-window window)))
          (kill-buffer (current-buffer)))))
    (when-let* ((runtime (etaf-runtime-for-buffer (current-buffer)))) (etaf-unmount runtime))))

(defun etaf-pi-close ()
  "Close the current client and its child; keep the saved Pi conversation."
  (interactive) (etaf-pi--current)
  (etaf-pi--dispose) (kill-buffer (current-buffer)))

(defvar etaf-pi-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (pair '(("i" . etaf-pi-edit) ("e" . etaf-pi-edit) (":" . etaf-pi-compose) ("a" . etaf-pi-answer)
                    ("C-c C-k" . etaf-pi-abort) ("m" . etaf-pi-select-model) ("T" . etaf-pi-select-thinking)
                    ("n" . etaf-pi-new-session) ("r" . etaf-pi-resume) ("g" . etaf-pi-follow)
                    ("j" . etaf-pi-page-down) ("k" . etaf-pi-page-up)
                    ("<next>" . etaf-pi-page-down) ("<prior>" . etaf-pi-page-up)
                    ("t" . etaf-pi-show-tools) ("R" . etaf-pi-reconnect)
                    ("?" . etaf-pi-help) ("q" . etaf-pi-close)))
      (define-key map (kbd (car pair)) (cdr pair)))
    (define-key map [wheel-up] (lambda () (interactive) (etaf-pi-page-up 3)))
    (define-key map [wheel-down] (lambda () (interactive) (etaf-pi-page-down 3)))
    (define-key map (kbd "C-v") #'etaf-pi-page-down)
    (define-key map (kbd "M-v") #'etaf-pi-page-up)
    map))
(defvar etaf-pi--command-maps (list (cons 'etaf-pi--state etaf-pi-mode-map)))

(define-derived-mode etaf-pi-mode special-mode "ETAF Pi"
  "Native Pi session with retained ETAF transcript and Emacs input.")

;;;###autoload
(defun etaf-pi-open (&optional directory new)
  "Open a Pi client at DIRECTORY, defaulting to the current project.
Reuse its buffer unless NEW is non-nil.  Return the client buffer immediately."
  (interactive (list nil current-prefix-arg))
  (let* ((directory (file-name-as-directory (expand-file-name
                                            (or directory (when-let* ((project (project-current nil)))
                                                            (project-root project)) default-directory))))
         (_valid (unless (file-directory-p directory) (user-error "目录不存在：%s" directory)))
         (name (format "*Pi: %s*" (abbreviate-file-name (directory-file-name directory))))
         (buffer (if new (generate-new-buffer name) (get-buffer-create name)))
         (display-buffer-overriding-action '((display-buffer-same-window))))
    (pop-to-buffer buffer)
    (unless (buffer-local-value 'etaf-pi--state buffer)
      (with-current-buffer buffer
        (etaf-pi-mode)
        (setq default-directory directory etaf-pi--state (etaf-pi-state-create)
              etaf-pi--line-cache (make-hash-table :test #'eq))
        (setq-local truncate-lines t mode-line-format nil)
        (setq-local scroll-margin 0 hscroll-margin 0)
        (setq-local cursor-type nil)
        (when (bound-and-true-p display-line-numbers-mode) (display-line-numbers-mode -1))
        (setq-local emulation-mode-map-alists (cons 'etaf-pi--command-maps emulation-mode-map-alists))
        (use-local-map etaf-pi-mode-map)
        (add-hook 'kill-buffer-hook #'etaf-pi--dispose nil t)
        (add-hook 'window-size-change-functions (lambda (_frame) (etaf-pi--schedule-paint)) nil t)
        (face-remap-set-base 'default (list :background (etaf-pi-color :bg) :foreground (etaf-pi-color :ink)))
        (etaf-pi--paint t)
        (condition-case err
            (progn
              (setq etaf-pi--connection (etaf-pi-rpc-start directory (lambda (record) (etaf-pi--event buffer record)) etaf-pi-command))
              (etaf-pi-refresh)
              (etaf-pi--request "get_commands")
              (etaf-pi--request "get_available_models")
              (etaf-pi--request "get_available_thinking_levels"))
          (error (setf (etaf-pi--state-phase etaf-pi--state) "启动失败"
                        (etaf-pi--state-error etaf-pi--state) (error-message-string err))
                 (etaf-pi--schedule-paint)))))
    (when (called-interactively-p 'any)
      (with-current-buffer buffer (etaf-pi-edit)))
    buffer))
(provide 'etaf-pi-app)
;;; etaf-pi-app.el ends here
