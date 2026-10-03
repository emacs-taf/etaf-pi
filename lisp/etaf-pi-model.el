;;; etaf-pi-model.el --- Pi wire events to presentation state -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Pure projection.  Pi, not this model, owns persistent conversations.
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(cl-defstruct (etaf-pi--state (:constructor etaf-pi--make-state))
  messages partial (blocks (make-hash-table :test #'eql)) tools
  session stats (phase "连接中") error commands models levels dialogs
  draft (queue 0) notice (version 0))

(defun etaf-pi-state-create ()
  "Create an independent headless Pi event projection."
  (etaf-pi--make-state))

(defun etaf-pi--content-text (content &optional thinking)
  "Project CONTENT blocks, including THINKING when requested."
  (if (stringp content) content
    (mapconcat
     (lambda (block)
       (pcase (plist-get block :type)
         ("text" (or (plist-get block :text) ""))
         ("thinking" (if thinking (concat "思考：" (or (plist-get block :thinking) "")) ""))
         ("image" "[图片]")
         (_ ""))) content "\n")))

(defun etaf-pi--set-phase (state)
  "Set STATE's activity label from session facts."
  (setf (etaf-pi--state-phase state)
        (cond ((eq (plist-get (etaf-pi--state-session state) :isCompacting) t) "压缩中")
              ((eq (plist-get (etaf-pi--state-session state) :isStreaming) t) "回复中")
              (t "就绪"))))

(defun etaf-pi-state-accept (state record)
  "Apply wire RECORD to STATE, returning STATE.
Final messages replace streaming drafts; agent_end does not mean settled.
Unknown records are ignored.  Failures remain visible in the state info."
  (cl-incf (etaf-pi--state-version state))
  (pcase (plist-get record :type)
    ("response"
     (if (not (eq (plist-get record :success) t))
         (setf (etaf-pi--state-error state) (or (plist-get record :error) "请求失败"))
       (let ((data (plist-get record :data)))
         (pcase (plist-get record :command)
           ("get_state" (setf (etaf-pi--state-session state) data) (etaf-pi--set-phase state))
           ("get_messages"
            (setf (etaf-pi--state-messages state) (copy-sequence (plist-get data :messages))
                  (etaf-pi--state-partial state) nil)
            (clrhash (etaf-pi--state-blocks state)))
           ("get_session_stats" (setf (etaf-pi--state-stats state) data))
           ("get_commands" (setf (etaf-pi--state-commands state) (plist-get data :commands)))
           ("get_available_models" (setf (etaf-pi--state-models state) (plist-get data :models)))
           ("get_available_thinking_levels" (setf (etaf-pi--state-levels state) (plist-get data :levels)))
           ((or "new_session" "switch_session")
            (unless (eq (plist-get data :cancelled) t)
              (setf (etaf-pi--state-messages state) nil (etaf-pi--state-partial state) nil
                    (etaf-pi--state-tools state) nil (etaf-pi--state-error state) nil)
              (clrhash (etaf-pi--state-blocks state))))))))
    ("agent_start" (setf (etaf-pi--state-phase state) "回复中" (etaf-pi--state-error state) nil))
    ("agent_settled" (setf (etaf-pi--state-phase state) "就绪"))
    ("message_start"
     (when (equal (plist-get (plist-get record :message) :role) "assistant")
       (setf (etaf-pi--state-partial state) (copy-tree (plist-get record :message)))
       (clrhash (etaf-pi--state-blocks state))))
    ("message_update"
     (let* ((event (plist-get record :assistantMessageEvent))
            (index (or (plist-get event :contentIndex) 0))
            (old (gethash index (etaf-pi--state-blocks state)))
            (kind (plist-get event :type)))
       (when (member kind '("text_delta" "thinking_delta" "text_end" "thinking_end"))
         (let* ((thinking (string-prefix-p "thinking" kind))
                (key (if thinking :thinking :text))
                (text (if (string-suffix-p "_end" kind)
                          (plist-get event :content)
                        (concat (plist-get old key) (plist-get event :delta)))))
           (puthash index (list :type (if thinking "thinking" "text") key (or text ""))
                    (etaf-pi--state-blocks state))))))
    ("message_end"
     (let ((message (plist-get record :message)))
       (setf (etaf-pi--state-messages state)
             (append (etaf-pi--state-messages state) (list message)))
       (when (equal (plist-get message :role) "assistant")
         (setf (etaf-pi--state-partial state) nil)
         (clrhash (etaf-pi--state-blocks state))
         (when (member (plist-get message :stopReason) '("error" "aborted"))
           (setf (etaf-pi--state-error state) (or (plist-get message :errorMessage) "回复已中断"))))
       (when (equal (plist-get message :role) "toolResult")
         (setf (etaf-pi--state-tools state)
               (cl-remove (plist-get message :toolCallId) (etaf-pi--state-tools state)
                          :key (lambda (tool) (plist-get tool :toolCallId)) :test #'equal)))))
    ((or "tool_execution_start" "tool_execution_update" "tool_execution_end")
     (let* ((id (plist-get record :toolCallId))
            (previous (cl-find id (etaf-pi--state-tools state)
                               :key (lambda (tool) (plist-get tool :toolCallId)) :test #'equal)))
       (setf (etaf-pi--state-tools state)
             (append (cl-remove id (etaf-pi--state-tools state)
                                :key (lambda (tool) (plist-get tool :toolCallId)) :test #'equal)
                     (list (append record (list :args (or (plist-get record :args)
                                                          (plist-get previous :args)))))))))
    ("queue_update"
     (setf (etaf-pi--state-queue state)
           (+ (length (plist-get record :steering)) (length (plist-get record :followUp)))))
    ("compaction_start" (setf (etaf-pi--state-phase state) "压缩中"))
    ("compaction_end"
     (when (plist-get record :errorMessage)
       (setf (etaf-pi--state-error state) (plist-get record :errorMessage))))
    ("auto_retry_start" (setf (etaf-pi--state-phase state) "重试中"))
    ("auto_retry_end"
     (when (plist-get record :finalError)
       (setf (etaf-pi--state-error state) (plist-get record :finalError))))
    ((or "client_exit" "client_error")
     (setf (etaf-pi--state-phase state) "已断开" (etaf-pi--state-error state) (plist-get record :error)))
    ("extension_error" (setf (etaf-pi--state-error state) (plist-get record :error)))
    ("extension_ui_request"
     (pcase (plist-get record :method)
       ((or "select" "confirm" "input" "editor")
        (setf (etaf-pi--state-dialogs state) (append (etaf-pi--state-dialogs state) (list record))))
       ("notify" (setf (etaf-pi--state-notice state) (plist-get record :message)))
       ("set_editor_text" (setf (etaf-pi--state-draft state) (plist-get record :text)))
       ("setStatus" (setf (etaf-pi--state-notice state) (plist-get record :statusText)))
       ("setTitle" (setf (etaf-pi--state-notice state) (plist-get record :title)))
       ("setWidget" (setf (etaf-pi--state-notice state) (mapconcat #'identity (plist-get record :widgetLines) " · "))))))
  state)

(defun etaf-pi--partial-message (state)
  "Project just STATE's live message without copying completed history."
  (when (etaf-pi--state-partial state)
    (let (indices)
      (maphash (lambda (key _value) (push key indices)) (etaf-pi--state-blocks state))
      (list :role "assistant" :stopReason "pending"
            :content (mapcar (lambda (index) (copy-tree (gethash index (etaf-pi--state-blocks state))))
                             (sort indices #'<))))))

(defun etaf-pi-state-messages (state)
  "Return a copy of STATE's authoritative messages followed by its live draft."
  (append (copy-tree (etaf-pi--state-messages state))
          (when-let* ((partial (etaf-pi--partial-message state))) (list partial))))

(defun etaf-pi-state-info (state)
  "Return copied UI facts for STATE; callers cannot mutate the model."
  (copy-tree (list :phase (etaf-pi--state-phase state) :error (etaf-pi--state-error state)
                   :session (etaf-pi--state-session state) :stats (etaf-pi--state-stats state)
                   :queue (etaf-pi--state-queue state) :dialogs (etaf-pi--state-dialogs state)
                   :notice (etaf-pi--state-notice state))))
(provide 'etaf-pi-model)
;;; etaf-pi-model.el ends here
