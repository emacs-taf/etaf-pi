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
(require 'etaf-pi-panes)

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
(defvar-local etaf-pi--draft nil)
(defvar-local etaf-pi--composer-dialog nil)
(defvar-local etaf-pi--sending nil)
(defvar-local etaf-pi--page-rows 20)
(defvar-local etaf-pi--rows nil
  "Settled window body rows the layout is committed to.
Lines a minibuffer borrows never reach it; the application fit records the
next real size.")
(defvar-local etaf-pi--layout-facts nil)
(defvar-local etaf-pi--actions nil)
(defvar-local etaf-pi--session-editing nil)
(defvar-local etaf-pi--controls-painted nil)
(defvar-local etaf-pi--session-changing nil)
(defvar-local etaf-pi--controls-phase nil)
(defvar-local etaf-pi--main-window nil)

(defun etaf-pi--current ()
  "Require a client buffer."
  (unless etaf-pi--state (user-error "当前不是 ETAF Pi 会话"))
  etaf-pi--state)

;; The keys are the shell's root `:keys': help lists them with these labels,
;; and a focused control or a native composer session keeps its own keys.
(defconst etaf-pi--bindings
  '(("i" etaf-pi-edit "编辑消息")
    ("e" etaf-pi-edit "编辑消息")
    (":" etaf-pi-compose "快速命令")
    ("a" etaf-pi-answer "处理待确认请求")
    ("C-c C-k" etaf-pi-abort "停止并收回待发消息")
    ("m" etaf-pi-select-model "选择模型")
    ("T" etaf-pi-select-thinking "选择思考级别")
    ("n" etaf-pi-new-session "新建会话")
    ("r" etaf-pi-resume "恢复会话")
    ("g" etaf-pi-follow "跟随最新回复")
    ("<next>" etaf-pi-page-down "历史下一页")
    ("<prior>" etaf-pi-page-up "历史上一页")
    ("C-v" etaf-pi-page-down "历史下一页")
    ("M-v" etaf-pi-page-up "历史上一页")
    ("<wheel-up>" etaf-pi--wheel-up "向上浏览历史")
    ("<wheel-down>" etaf-pi--wheel-down "向下浏览历史")
    ("t" etaf-pi-show-tools "完整工具输出")
    ("R" etaf-pi-reconnect "重新连接")
    ("C-c h" etaf-pi-help "查看快捷键")
    ("q" etaf-pi-close "关闭客户端"))
  "Client keys as (KEY COMMAND LABEL) entries of the shell's root `:keys'.")

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
        (when (equal (plist-get record :type) "set_editor_text")
          (etaf-pi--set-draft (or (etaf-pi--state-draft etaf-pi--state) "")))
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
             (when (member (plist-get record :command) '("new_session" "switch_session"))
               (setq etaf-pi--session-changing nil))
             (when (equal (plist-get record :command) "get_messages")
               (clrhash etaf-pi--line-cache))
             (etaf-pi--schedule-paint)
             (when (equal (plist-get record :command) "prompt")
               (setq etaf-pi--sending nil)
               (etaf-pi--schedule-paint))
             (when (and after (eq (plist-get record :success) t))
               (funcall after record)))))) timeout)))

(defun etaf-pi-refresh ()
  "Refresh authoritative session state, usage and messages asynchronously."
  (interactive)
  (etaf-pi--request "get_state")
  (etaf-pi--request "get_session_stats")
  (etaf-pi--request "get_messages"))

(defun etaf-pi--refresh-theme (&optional _theme)
  "Rebuild visible clients after the Emacs theme or its color mode changes.
A hidden client rebuilds when it is shown again."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and etaf-pi--state (not etaf-pi--disposed) (get-buffer-window buffer t))
        (etaf-pi--paint t)))))

(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'etaf-pi--refresh-theme))

(defun etaf-pi--summary (width)
  "Build the status line at WIDTH cells."
  (let* ((state etaf-pi--state)
         (stats (etaf-pi--state-stats state))
         (context (plist-get stats :contextUsage))
         (usage (if (numberp (plist-get context :percent))
                    (format " · 上下文 %.1f%%" (plist-get context :percent)) ""))
         (base (format "%s%s · $%.4f%s%s"
                       (if etaf-pi--session-changing "正在切换会话" (etaf-pi--state-phase state)) usage
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
            (append (list "" "")
                    (list (etaf-pi--center (etaf-pi--ink "◆  Pi" :accent nil t) width))
                    (list "")
                    (list (etaf-pi--center (etaf-pi--ink "有什么可以帮你？" :ink nil t) width))
                    (list "")
                    (list (etaf-pi--center (etaf-pi--ink "描述任务，或粘贴需要一起处理的代码。" :muted) width))
                    (list "")
                    (list (etaf-pi--center
                           (concat (etaf-pi--ink " 解释这段代码 " :ink :surface-2)
                                   "  " (etaf-pi--ink " 跑一遍测试 " :ink :surface-2)
                                   "  " (etaf-pi--ink " 总结今天的改动 " :ink :surface-2))
                           width)))))
    (dolist (notice (list (etaf-pi--state-error etaf-pi--state)
                           (etaf-pi--state-notice etaf-pi--state)))
      (when (and notice (not (string-empty-p notice)))
        (setq lines (append lines '("") (etaf-pi--wrap notice width)))))
    lines))

(defun etaf-pi--make-actions (buffer)
  "Bind catalog controls to their owning conversation BUFFER."
  (list :compose (lambda ()
                   (with-current-buffer buffer
                     (let ((runtime (etaf-runtime-for-buffer buffer)))
                       (cond
                        ((and runtime (etaf-text-input-active-p runtime))
                         (etaf-text-input-end))
                        ((string-empty-p (string-trim (or (etaf-value etaf-pi--draft) "")))
                         (etaf-pi-edit))
                        (t (etaf-pi-send (etaf-value etaf-pi--draft)))))))
        :answer (lambda () (with-current-buffer buffer (call-interactively #'etaf-pi-answer)))
        :abort (lambda () (with-current-buffer buffer (etaf-pi-abort)))
        :model (lambda () (with-current-buffer buffer (etaf-pi-select-model)))
        :thinking (lambda () (with-current-buffer buffer (etaf-pi-select-thinking)))
        :new (lambda () (with-current-buffer buffer (etaf-pi-new-session)))
        :resume (lambda () (with-current-buffer buffer (call-interactively #'etaf-pi-resume)))
        :tools (lambda () (with-current-buffer buffer (etaf-pi-show-tools)))
        :help (lambda () (with-current-buffer buffer (etaf-pi-help)))
        :follow (lambda ()
                  (with-current-buffer buffer
                    (when-let* ((window (get-buffer-window buffer t))) (select-window window))
                    (etaf-pi-follow)))))

(defun etaf-pi--height (window)
  "Return the body rows the layout is committed to in WINDOW."
  (max 6 (or etaf-pi--rows (window-body-height window))))

(defun etaf-pi--fit (window)
  "Commit WINDOW's settled body rows and repaint a mounted client."
  (setq etaf-pi--rows (window-body-height window))
  (when (etaf-runtime-for-buffer (current-buffer))
    (etaf-pi--schedule-paint)))

(defun etaf-pi--frame (window)
  "Return this client's paint facts for WINDOW.
The plist holds the shell view inputs, the visible transcript slice and the
shape and control signatures that decide between a rebuild and patches."
  (let* ((title (file-name-nondirectory (directory-file-name default-directory)))
         (session (etaf-pi--state-session etaf-pi--state))
         (model (plist-get session :model))
         (busy (and (member (etaf-pi--state-phase etaf-pi--state)
                            '("回复中" "重试中" "压缩中")) t))
         (pending (length (etaf-pi--state-dialogs etaf-pi--state)))
         (width (max 28 (window-body-width window)))
         (height (etaf-pi--height window))
         (layout (etaf-pi--layout width height)) (rows (max 1 (- height 10)))
         (all (etaf-pi--transcript (plist-get layout :measure)))
         (start (if etaf-pi--follow (max 0 (- (length all) rows))
                  (min etaf-pi--offset (max 0 (- (length all) rows)))))
         (lines (cl-subseq all start (min (length all) (+ start rows))))
         (composer (list :draft etaf-pi--draft
                         :placeholder "发消息，或输入 / 选择命令"
                         :rows 3
                         :completion (list #'etaf-pi--complete-command)
                         :submit #'etaf-pi--composer-submit
                         :cancel #'etaf-pi--composer-cancel
                         :model (or (plist-get model :name) (plist-get model :id) "选择模型")
                         :thinking (or (plist-get session :thinkingLevel) "off")
                         :busy busy :pending pending :sending etaf-pi--sending
                         :width width :actions etaf-pi--actions)))
    (setq etaf-pi--layout-facts layout etaf-pi--offset start
          etaf-pi--total-lines (length all) etaf-pi--page-rows rows)
    (list :title title :status (etaf-pi--summary (- width 8))
          :lines (cons (if etaf-pi--follow "" (etaf-pi--ink "查看历史 · g 回到最新" :muted))
                       (append lines (make-list (- rows (length lines)) "")))
          :layout layout
          :navigation (and (buffer-live-p etaf-pi--navigation)
                           (get-buffer-window etaf-pi--navigation t))
          :composer composer
          :controls (list (plist-get composer :model) (plist-get composer :thinking)
                          busy pending etaf-pi--sending)
          :shape (list width height))))

(defun etaf-pi--frame-view (frame)
  "Return the shell view for FRAME and record its shape and controls."
  (setq etaf-pi--shape (plist-get frame :shape)
        etaf-pi--controls-painted (plist-get frame :controls))
  (etaf-pi--shell-view (plist-get frame :title) (plist-get frame :status)
                       (plist-get frame :lines) (plist-get frame :layout)
                       (plist-get frame :navigation) (plist-get frame :composer)
                       etaf-pi--actions etaf-pi--bindings))

(defun etaf-pi--view (buffer)
  "Return client BUFFER's first view, laid out for its display window."
  (with-current-buffer buffer
    (let ((window (get-buffer-window buffer t)))
      (if (not window)
          (etaf-view (box))
        (let ((frame (etaf-pi--frame window)))
          (setq etaf-pi--painted (list :status (plist-get frame :status)
                                       :lines (plist-get frame :lines)))
          (etaf-pi--frame-view frame))))))

(defun etaf-pi--paint (&optional rebuild)
  "Update visible chat content, composer and navigation chrome.
A changed shape, color mode or control row, or REBUILD, mounts the view
again; otherwise only the status and transcript Hosts are patched."
  (etaf-pi--ensure-navigation etaf-pi--actions)
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (let ((mode (etaf-pi--palette-mode)))
      ;; Cached transcript lines carry the colors of the mode they were
      ;; wrapped in.
      (unless (eq mode (etaf-value etaf-pi--palette))
        (clrhash etaf-pi--line-cache))
      (let* ((frame (etaf-pi--frame window))
             (status (plist-get frame :status))
             (lines (plist-get frame :lines))
             (runtime (etaf-runtime-for-buffer (current-buffer)))
             (rebuildp (or rebuild (null runtime)
                           (not (equal etaf-pi--shape (plist-get frame :shape)))
                           (not (eq mode (etaf-value etaf-pi--palette)))
                           (not (equal (plist-get frame :controls) etaf-pi--controls-painted))))
             (editing (and runtime (etaf-text-input-active-p runtime))))
        ;; A native session owns the buffer text.  Its ending flush republishes
        ;; the field, but retained cohorts describe pre-session text, so the
        ;; next paint after it ends rebuilds instead of patching.
        (cond
         (editing (setq etaf-pi--session-editing t))
         ((or rebuildp etaf-pi--session-editing)
          (setq etaf-pi--session-editing nil)
          (etaf-mount (current-buffer) (etaf-pi--frame-view frame)
                      (list :viewport-height (etaf-pi--height window)))
          ;; The canvas of the client and its sidebar follow this ref.
          (unless (eq mode (etaf-value etaf-pi--palette))
            (setf (etaf-value etaf-pi--palette) mode)))
         (t
          (let (updates)
            (unless (equal-including-properties status (plist-get etaf-pi--painted :status))
              (push (cons 'etaf-pi-status status) updates))
            (unless (and (= (length lines) (length (plist-get etaf-pi--painted :lines)))
                         (cl-every #'equal-including-properties lines (plist-get etaf-pi--painted :lines)))
              (push (cons 'etaf-pi-transcript
                          (mapconcat (lambda (line) (if (string-empty-p line) " " line)) lines "\n")) updates))
            (when (and runtime updates)
              (etaf-runtime-update-host-contents runtime (nreverse updates))))))
        (setq etaf-pi--painted (list :status status :lines lines))
        (set-window-hscroll window 0) (set-window-start window (point-min)))))
  (setq etaf-pi--controls-phase (etaf-pi--state-phase etaf-pi--state)))

(defun etaf-pi-info ()
  "Return current client facts, including backend liveness and visible viewport."
  (etaf-pi--current)
  (append (etaf-pi-state-info etaf-pi--state)
          (list :connection (when etaf-pi--connection (etaf-pi-rpc-info etaf-pi--connection))
                :directory default-directory :offset etaf-pi--offset
                :total-lines etaf-pi--total-lines :following etaf-pi--follow
                :session-changing etaf-pi--session-changing
                :render-pending (and etaf-pi--paint-timer t)
                :controls-phase etaf-pi--controls-phase
                :viewport (copy-sequence etaf-pi--shape)
                :layout (copy-sequence etaf-pi--layout-facts)
                :navigation (and (buffer-live-p etaf-pi--navigation) (get-buffer-window etaf-pi--navigation t) (buffer-name etaf-pi--navigation))
                :composer (let ((runtime (etaf-runtime-for-buffer (current-buffer))))
                            (list :ref 'pi-composer
                                  :active (and runtime (and (etaf-text-input-active-p runtime) t))
                                  :text (and etaf-pi--draft (etaf-value etaf-pi--draft))
                                  :dialog (and etaf-pi--composer-dialog t)
                                  :sending (and etaf-pi--sending t))))))

(defun etaf-pi-send (text &optional behavior)
  "Send TEXT or a supported slash command.  BEHAVIOR is steer or followUp.
Running prompts default to followUp, so typing never drops an instruction.
A command clears the composer; an accepted prompt clears only text that the
composer still holds unchanged, so concurrent edits and failed sends survive."
  (etaf-pi--current)
  (when etaf-pi--session-changing (user-error "会话正在切换，请稍候再发送"))
  (unless (string-empty-p (string-trim text))
    (setq etaf-pi--follow t)
    (if (string-prefix-p "/" text)
        (progn (etaf-pi--set-draft "") (etaf-pi--slash text))
      (setq etaf-pi--sending t)
      (etaf-pi--schedule-paint)
      (etaf-pi--request
       "prompt" (list :message text :streamingBehavior (or behavior "followUp"))
       (lambda (_)
         (when (and (equal (etaf-value etaf-pi--draft) text)
                    (not (etaf-text-input-active-p (etaf-runtime-for-buffer (current-buffer)))))
           (etaf-pi--set-draft ""))
         (etaf-pi--schedule-paint))
       120))))

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
            nil nil (and etaf-pi--draft (etaf-value etaf-pi--draft)) 'etaf-pi-input-history))))
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
           (let ((current (and etaf-pi--draft (etaf-value etaf-pi--draft))))
             (etaf-pi--set-draft
              (string-join (delq nil (list (unless (string-empty-p (or current "")) current)
                                           recovered))
                           "\n\n"))))))
     (etaf-pi--request "abort" nil (lambda (_) (etaf-pi-refresh)) 60))))

(defun etaf-pi--require-idle ()
  "Reject a session mutation during active work."
  (unless (and (not etaf-pi--session-changing) (equal (etaf-pi--state-phase (etaf-pi--current)) "就绪"))
    (user-error "Pi 仍在工作；先按 C-c C-k 停止")))

(defun etaf-pi--change-session (command fields)
  "Send session COMMAND with FIELDS while preserving drafts during the switch."
  (setq etaf-pi--session-changing t)
  (etaf-pi--schedule-paint)
  (condition-case err
      (etaf-pi--request command fields (lambda (_) (clrhash etaf-pi--line-cache) (etaf-pi-refresh)) 120)
    (error (setq etaf-pi--session-changing nil) (signal (car err) (cdr err)))))

(defun etaf-pi-new-session ()
  "Start a new Pi session without deleting the previous saved session."
  (interactive)
  (etaf-pi--require-idle)
  (etaf-pi--change-session "new_session" nil))

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
    (etaf-pi--change-session "switch_session" (list :sessionPath target))))

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

(defun etaf-pi--complete-command ()
  "Offer Pi command completion for a slash command in the composer."
  (let ((end (point)) (line-start (line-beginning-position)))
    (save-excursion
      (goto-char line-start)
      (skip-chars-forward " \t")
      (let ((start (point)))
        (when (and (<= start end)
                   (string-match-p "\\`/[^ \t\n]*\\'"
                                   (buffer-substring-no-properties start end)))
          (list start end
                (append '("/new" "/resume" "/model" "/thinking" "/abort" "/compact" "/quit" "/help")
                        (mapcar (lambda (item) (concat "/" (plist-get item :name)))
                                (etaf-pi--state-commands etaf-pi--state)))
                :exclusive 'no))))))

(defun etaf-pi--set-draft (text)
  "Set this client's composer TEXT and republish the field."
  (when (and (buffer-live-p (current-buffer)) etaf-pi--draft)
    (unless (equal (etaf-value etaf-pi--draft) text)
      (setf (etaf-value etaf-pi--draft) text)
      (when-let* ((runtime (etaf-runtime-for-buffer (current-buffer))))
        (etaf-runtime-flush runtime)))))

(defun etaf-pi--composer-submit (text)
  "Handle composer TEXT: answer the pending dialog or send a prompt."
  (let ((dialog etaf-pi--composer-dialog))
    (setq etaf-pi--composer-dialog nil)
    (if dialog (etaf-pi-answer text nil) (etaf-pi-send text))
    (etaf-pi--schedule-paint)))

(defun etaf-pi--composer-cancel (text)
  "Stop editing while keeping TEXT as the draft.
A dialog editor cancels its dialog instead of keeping the text."
  (if etaf-pi--composer-dialog
      (progn (setq etaf-pi--composer-dialog nil) (etaf-pi-answer nil t))
    (etaf-pi--set-draft text))
  (etaf-pi--schedule-paint))

(defun etaf-pi-edit (&optional text dialog)
  "Focus the in-layout composer, optionally prefilled with TEXT.
DIALOG marks a Pi editor request answered by the next submission."
  (interactive)
  (etaf-pi--current)
  (setq etaf-pi--composer-dialog dialog)
  (when (or text dialog) (etaf-pi--set-draft (or text "")))
  (let ((runtime (etaf-runtime-for-buffer (current-buffer))))
    (when runtime (etaf-text-input-begin runtime 'pi-composer))))

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

(defun etaf-pi--direction (direction)
  "Scroll the transcript with DIRECTION and return the result symbol."
  (pcase direction
    ('down (etaf-pi-page-down) 'handled)
    ('up (etaf-pi-page-up) 'handled)
    (_ 'release)))
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
  "Show read-only command help inside the current Pi client buffer.
Return the client buffer; no separate help buffer or window is created."
  (interactive)
  (etaf-pi--current)
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (select-window window))
  (etaf-help (etaf-runtime-for-buffer (current-buffer))))

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
  "Release only this buffer's resources; the application unmounts its view."
  (unless etaf-pi--disposed
    (setq etaf-pi--disposed t)
    (when etaf-pi--paint-timer (cancel-timer etaf-pi--paint-timer) (setq etaf-pi--paint-timer nil))
    (when etaf-pi--connection (etaf-pi-rpc-close etaf-pi--connection))
    (etaf-pi--dispose-panes)
    (when (and etaf-pi--draft (not (string-empty-p (etaf-value etaf-pi--draft))))
      (message "未发送的草稿保留在 %s" (buffer-name)))))

(defun etaf-pi-close ()
  "Close the current client and its child; keep the saved Pi conversation."
  (interactive) (etaf-pi--current)
  (etaf-app-close))

(defun etaf-pi--wheel (event direction)
  "Scroll EVENT's message window by DIRECTION without stealing input focus."
  (let ((window (and (consp event) (posn-window (event-start event)))))
    (if (window-live-p window)
        (with-selected-window window
          (if (< direction 0) (etaf-pi-page-up (- direction)) (etaf-pi-page-down direction)))
      (if (< direction 0) (etaf-pi-page-up (- direction)) (etaf-pi-page-down direction)))))

(defun etaf-pi--wheel-up (event)
  "Browse three older lines in the window of wheel EVENT."
  (interactive (list last-input-event))
  (etaf-pi--wheel event -3))

(defun etaf-pi--wheel-down (event)
  "Browse three newer lines in the window of wheel EVENT."
  (interactive (list last-input-event))
  (etaf-pi--wheel event 3))

(defun etaf-pi--buffer-owner (buffer)
  "Find the conversation owning BUFFER, including native input and chrome."
  (when (buffer-live-p buffer)
    (let ((owner (etaf-app-state buffer)))
      (cond ((buffer-local-value 'etaf-pi--state buffer) buffer)
            ((and (bufferp owner) (buffer-live-p owner)) owner)))))

(defun etaf-pi--hide-client-panes (owner)
  "Hide OWNER's navigation pane without stopping its backend or drafts."
  (when (buffer-live-p owner)
    (with-current-buffer owner (etaf-pi--remove-pane-window etaf-pi--navigation))))

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
         (existing (and (not new) (etaf-app-buffer name))))
    (if (and existing (buffer-local-value 'etaf-pi--state existing))
        (progn (etaf-pi--show existing) existing)
      (etaf-pi--start (if (get-buffer name) (generate-new-buffer-name name) name)
                      directory))))

(defun etaf-pi--show (buffer)
  "Display client BUFFER in its remembered window or the selected one.
Another client leaving that window takes its navigation pane with it."
  (let ((display-buffer-overriding-action '((display-buffer-same-window)))
        (previous (etaf-pi--buffer-owner (window-buffer (selected-window))))
        (visible (get-buffer-window buffer t))
        (remembered (buffer-local-value 'etaf-pi--main-window buffer)))
    (when (and previous (not (eq previous buffer))) (etaf-pi--hide-client-panes previous))
    (cond
     ((window-live-p visible) (select-window visible))
     ((window-live-p remembered)
      (let ((other (etaf-pi--buffer-owner (window-buffer remembered))))
        (when (and other (not (eq other buffer))) (etaf-pi--hide-client-panes other)))
      (if (window-live-p remembered)
          (progn (select-window remembered) (switch-to-buffer buffer))
        (pop-to-buffer buffer)))
     (t (pop-to-buffer buffer)))
    (with-current-buffer buffer
      (setq etaf-pi--main-window (get-buffer-window buffer t)))))

(defun etaf-pi--start (name directory)
  "Open a new client application NAME for DIRECTORY and start its backend."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (setq default-directory directory etaf-pi--state (etaf-pi-state-create)
            etaf-pi--line-cache (make-hash-table :test #'eq)
            etaf-pi--draft (etaf-ref "")
            etaf-pi--palette (etaf-ref (etaf-pi--palette-mode))
            etaf-pi--actions (etaf-pi--make-actions buffer)))
    ;; The first view is laid out for the window the client shows in, so
    ;; the client and its sidebar are displayed before it is built.  Later
    ;; layouts mount again from `etaf-pi--paint'.
    (etaf-pi--show buffer)
    (with-current-buffer buffer (etaf-pi--ensure-navigation etaf-pi--actions))
    (let ((palette (buffer-local-value 'etaf-pi--palette buffer)))
      (etaf-app-open
       name (etaf-pi--view buffer)
       :state (buffer-local-value 'etaf-pi--state buffer)
       :cursor nil :display nil
       :background (lambda () (etaf-pi-color :bg (etaf-value palette)))
       :foreground (lambda () (etaf-pi-color :ink (etaf-value palette)))
       :on-fit #'etaf-pi--fit
       :on-close #'etaf-pi--dispose
       :mount-options (lambda (window)
                        (and window (list :viewport-height (etaf-pi--height window))))))
    (with-current-buffer buffer
      (condition-case err
          (progn
            (setq etaf-pi--connection (etaf-pi-rpc-start directory (lambda (record) (etaf-pi--event buffer record)) etaf-pi-command))
            (etaf-pi-refresh)
            (etaf-pi--request "get_commands")
            (etaf-pi--request "get_available_models")
            (etaf-pi--request "get_available_thinking_levels"))
        (error (setf (etaf-pi--state-phase etaf-pi--state) "启动失败"
                     (etaf-pi--state-error etaf-pi--state) (error-message-string err))
               (etaf-pi--schedule-paint))))
    buffer))
(provide 'etaf-pi-app)
;;; etaf-pi-app.el ends here
