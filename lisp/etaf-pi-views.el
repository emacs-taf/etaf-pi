;;; etaf-pi-views.el --- Conversation surfaces and shared controls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Pure, bounded views. A retained transcript Host avoids rebuilding chrome.
;; Native Emacs owns the adjacent editor; ETAF never imitates an editable area.
;;; Code:
(require 'cl-lib)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-pi-theme)

(defun etaf-pi--wrap (text width)
  "Wrap TEXT at WIDTH display cells, preserving text and empty lines."
  (let (lines)
    (dolist (line (split-string (replace-regexp-in-string "\t" "    " text t t) "\n" nil))
      (while (> (string-width line) width)
        (let* ((piece (truncate-string-to-width line width))
               (count (max 1 (length piece))))
          (push (substring line 0 count) lines)
          (setq line (substring line count))))
      (push line lines))
    (nreverse lines)))

(defun etaf-pi--ink (text color &optional background bold)
  "Give TEXT semantic COLOR, optional BACKGROUND and BOLD emphasis."
  (propertize text 'face
              (append (list :foreground (etaf-pi-color color))
                      (when background (list :background (etaf-pi-color background)))
                      (when bold (list :weight 'bold)))))

(defun etaf-pi--surface-line (text width color background &optional bold)
  "Fit a padded TEXT line to WIDTH with semantic COLOR and BACKGROUND."
  (etaf-pi--ink (concat "  " text (make-string (max 0 (- width 2 (string-width text))) ?\s))
                color background bold))

(defun etaf-pi--markdown-lines (text width)
  "Render readable Markdown TEXT at WIDTH, keeping code verbatim.
This intentionally supports headings, emphasis and fenced code, not HTML."
  (let (code result)
    (dolist (line (split-string text "\n" nil))
      (cond
       ((string-match "\\`[ \t]*```\\(.*\\)" line)
        (setq code (not code))
        (push (etaf-pi--surface-line
               (if code (let ((lang (string-trim (match-string 1 line))))
                          (if (string-empty-p lang) "代码" lang)) "")
               width :muted :surface-2) result))
       (code
        (dolist (part (etaf-pi--wrap line (- width 4)))
          (push (etaf-pi--surface-line part width :ink :surface-2) result)))
       (t
        (let ((heading (string-match "\\`#+ +" line)))
          (when heading (setq line (substring line (match-end 0))))
          (setq line (replace-regexp-in-string
                      "\\*\\*\\([^*\n]+\\)\\*\\*"
                      (lambda (match) (propertize (substring match 2 -2) 'face 'bold)) line t t))
          (setq line (replace-regexp-in-string
                      "`\\([^`\n]+\\)`"
                      (lambda (match) (etaf-pi--ink (substring match 1 -1) :accent :surface-2)) line t t))
          (dolist (part (etaf-pi--wrap line width))
            (push (if heading (etaf-pi--ink part :ink nil t) part) result))))))
    (nreverse result)))

(defun etaf-pi--message-lines (message width)
  "Project one MESSAGE into WIDTH-cell conversation lines."
  (let* ((role (plist-get message :role))
         (text (etaf-pi--content-text (plist-get message :content)))
         (pending (equal (plist-get message :stopReason) "pending")))
    (pcase role
      ("user"
       (append (list (etaf-pi--surface-line "你" width :accent :accent-soft t))
               (mapcar (lambda (line) (etaf-pi--surface-line line width :ink :accent-soft))
                       (etaf-pi--wrap text (- width 4))) (list "")))
      ("assistant"
       (unless (and (not pending) (string-empty-p (string-trim text)))
         (append (list (etaf-pi--ink (if pending "PI  /  正在回复" "PI  /  助手") :accent nil t) "")
               (if (string-empty-p (string-trim text))
                   (list (etaf-pi--ink (if pending "正在思考…" "已发起工具调用") :muted))
                 (etaf-pi--markdown-lines text width)) (list "" ""))))
      ("toolResult"
       (let ((failed (eq (plist-get message :isError) t)))
         (list (etaf-pi--surface-line
                (truncate-string-to-width
                 (format "工具 · %s · %s" (plist-get message :toolName) (if failed "失败" "完成")) (- width 4))
                width (if failed :error :muted) :surface)
               (etaf-pi--surface-line
                (truncate-string-to-width (or (car (split-string text "\n")) "") (- width 4) nil nil "…")
                width :muted :surface) "")))
      ((or "compactionSummary" "branchSummary")
       (append (list (etaf-pi--ink "上下文摘要" :muted))
               (etaf-pi--wrap (or (plist-get message :summary) "") width) (list "")))
      ("custom" (unless (eq (plist-get message :display) :false)
                  (append (etaf-pi--wrap text width) (list "")))))))

(defun etaf-pi--control (label ref action &optional primary)
  "Compose shared button LABEL at REF with ACTION."
  (etaf-node 'etaf-button
             (list :label label :id ref :ref ref :on-press action
                   :variant (if primary 'primary 'ghost)
                   :color (etaf-pi-color (if primary :button-ink :muted))
                   :bgcolor (etaf-pi-color (if primary :accent :surface))
                   :padding '((lh 0) (ch 1)) :font-weight 'normal)
             nil))

(defun etaf-pi--shell-view (title status lines width measure footer actions)
  "Render TITLE, STATUS, LINES and ACTIONS within WIDTH, using MEASURE cells.
FOOTER carries live status; fixed one-row slots prevent accidental line growth."
  (etaf-view
   (column :width 'stretch :background-color (etaf-pi-color :bg)
     (box :height '(lh 1) :background-color (etaf-pi-color :surface) "")
     (flex :flex-direction 'row :width 'stretch :gap '((lh 0) (ch 1))
           :padding '((lh 0) (ch 2)) :background-color (etaf-pi-color :surface)
       (box :flex-basis '(ch 0) :flex-grow 1 :height '(lh 1) :wrap-mode 'none :overflow 'hidden
         (text :font-weight 'bold :color (etaf-pi-color :ink)
           (expr (concat "PI   /   " (truncate-string-to-width title (max 6 (- width 40)) nil nil "…")))))
       (expr (etaf-pi--control "新会话" 'pi-new (plist-get actions :new)))
       (expr (when (>= width 76) (etaf-pi--control "恢复" 'pi-resume (plist-get actions :resume))))
       (expr (when (>= width 54) (etaf-pi--control "工具" 'pi-tools (plist-get actions :tools))))
       (expr (etaf-pi--control "帮助" 'pi-help (plist-get actions :help))))
     (box :height '(lh 1) :width 'stretch :wrap-mode 'none :overflow 'hidden
          :padding '((lh 0) (ch 2)) :background-color (etaf-pi-color :surface)
       (text :ref 'etaf-pi-status :id 'etaf-pi-status :color (etaf-pi-color :muted) (expr status)))
     (box :height '(lh 1) "")
     (flex :flex-direction 'row :width 'stretch :justify-content 'center
       (box :width (list 'ch (+ measure 2)) :wrap-mode 'none :overflow 'hidden
         (text :id 'etaf-pi-transcript :ref 'etaf-pi-transcript :color (etaf-pi-color :ink)
           (expr (mapconcat (lambda (line) (if (string-empty-p line) " " line)) lines "\n")))))
     (box :height '(lh 1) "")
     (box :height '(lh 1) :width 'stretch :wrap-mode 'none :overflow 'hidden
          :padding '((lh 0) (ch 2)) :background-color (etaf-pi-color :surface)
       (text :ref 'etaf-pi-footer :id 'etaf-pi-footer :color (etaf-pi-color :muted) (expr footer)))
     (flex :flex-direction 'row :width 'stretch :gap '((lh 0) (ch 1))
           :padding '((lh 0) (ch 2)) :background-color (etaf-pi-color :surface)
       (expr (etaf-pi--control "发送" 'pi-compose (plist-get actions :compose) t))
       (expr (etaf-pi--control "确认" 'pi-answer (plist-get actions :answer)))
       (expr (etaf-pi--control "停止" 'pi-abort (plist-get actions :abort)))
       (expr (when (>= width 54) (etaf-pi--control "模型" 'pi-model (plist-get actions :model))))
       (expr (when (>= width 76) (etaf-pi--control "思考" 'pi-thinking (plist-get actions :thinking))))
       (expr (when (>= width 100) (etaf-pi--control "最新" 'pi-follow (plist-get actions :follow)))))
     (box :height '(lh 1) :background-color (etaf-pi-color :surface) ""))))

(provide 'etaf-pi-views)
;;; etaf-pi-views.el ends here
