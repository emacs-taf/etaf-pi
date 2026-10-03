;;; etaf-pi-views.el --- Chat, navigation and compact composer views -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Pure catalog composition. The message stream owns one retained content Host;
;; native editing sits above a separate two-row ETAF action bar.
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
        (let* ((piece (truncate-string-to-width line width)) (count (max 1 (length piece))))
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
  "Pad TEXT to WIDTH using semantic COLOR and BACKGROUND."
  (etaf-pi--ink (concat "  " text (make-string (max 0 (- width 2 (string-width text))) ?\s))
                color background bold))

(defun etaf-pi--markdown-lines (text width)
  "Render headings, emphasis, inline code and fenced code from TEXT at WIDTH."
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
       (let* ((bubble (min (- width 2) (max 20 (min (floor (* width .82)) (+ 4 (string-width text))))))
              (inset (make-string (- width bubble) ?\s)))
         (append (list "")
                 (mapcar (lambda (line)
                           (concat inset (etaf-pi--surface-line line bubble :ink :accent-soft)))
                         (etaf-pi--wrap text (- bubble 4))) (list "" ""))))
      ("assistant"
       (unless (and (not pending) (string-empty-p (string-trim text)))
         (append (list (etaf-pi--ink (if pending "Pi · 正在回复" "Pi") :accent nil t) "")
                 (if (string-empty-p (string-trim text)) (list (etaf-pi--ink "正在思考…" :muted))
                   (etaf-pi--markdown-lines text width)) (list "" ""))))
      ("toolResult"
       (list (etaf-pi--ink
              (truncate-string-to-width
               (format "工具 · %s · %s" (plist-get message :toolName)
                       (if (eq (plist-get message :isError) t) "失败" "完成")) width)
              (if (eq (plist-get message :isError) t) :error :muted))
             (etaf-pi--ink (truncate-string-to-width
                            (concat "  " (or (car (split-string text "\n")) "")) width nil nil "…") :muted) ""))
      ((or "compactionSummary" "branchSummary")
       (append (list (etaf-pi--ink "上下文摘要" :muted))
               (etaf-pi--wrap (or (plist-get message :summary) "") width) (list "")))
      ("custom" (unless (eq (plist-get message :display) :false)
                  (append (etaf-pi--wrap text width) (list "")))))))

(defun etaf-pi--control (label ref action &optional primary disabled)
  "Compose a catalog button LABEL at REF."
  (etaf-node 'etaf-button
             (list :label label :id ref :ref ref :on-press action :disabled disabled
                   :variant (if primary 'primary 'ghost)
                   :color (etaf-pi-color (if primary :button-ink :muted))
                   :bgcolor (etaf-pi-color (if primary :accent :surface))
                   :padding '((lh 0) (ch 1)) :font-weight 'normal)
             nil))

(defun etaf-pi--layout (width height)
  "Resolve conversation geometry; native panes own navigation and input."
  (list :width width :height height :left 0 :right 0 :center (- width 2)
        :measure (min 92 (max 16 (- width 8)))))

(defun etaf-pi--shell-view (title status lines layout navigation actions)
  "Compose a quiet conversation header and its retained message stream."
  (let* ((width (plist-get layout :width)) (height (plist-get layout :height))
         (measure (plist-get layout :measure)) (left (/ (- width 2 measure) 2)))
    (etaf-view
     (box :position 'relative :width 'stretch :height (list 'lh height) :background-color (etaf-pi-color :bg)
       (column :position 'absolute :z-index 0 :left '(ch 2) :top '(lh 1)
               :width (list 'ch (- width 6)) :height '(lh 2)
         (flex :flex-direction 'row :width 'stretch :gap '((lh 0) (ch 1))
           (box :flex-grow 1 :flex-basis '(ch 0) :height '(lh 1) :wrap-mode 'none :overflow 'hidden
             (text :color (etaf-pi-color :ink) :font-weight 'bold
               (expr (truncate-string-to-width title (max 8 (- width 32)) nil nil "…"))))
           (expr (unless navigation (etaf-pi--control "新对话" 'pi-new (plist-get actions :new))))
           (expr (etaf-pi--control "工具" 'pi-tools (plist-get actions :tools)))
           (expr (unless navigation (etaf-pi--control "历史" 'pi-resume (plist-get actions :resume)))))
         (box :width 'stretch :height '(lh 1) :wrap-mode 'none
           (text :ref 'etaf-pi-status :id 'etaf-pi-status :color (etaf-pi-color :muted) (expr status))))
       (box :position 'absolute :z-index 0 :left (list 'ch left) :top '(lh 4)
            :width (list 'ch (+ measure 1)) :height (list 'lh (length lines))
            :background-color (etaf-pi-color :bg) :wrap-mode 'none :overflow 'hidden
         (text :id 'etaf-pi-transcript :ref 'etaf-pi-transcript :color (etaf-pi-color :ink)
           (expr (mapconcat (lambda (line) (if (string-empty-p line) " " line)) lines "\n"))))))))

(defun etaf-pi--sidebar-view (title height actions)
  "Render full-height conversation navigation, without dashboard cards."
  (etaf-view
   (box :position 'relative :width 'stretch :height (list 'lh height) :background-color (etaf-pi-color :surface)
     (column :position 'absolute :z-index 0 :left '(ch 2) :top '(lh 1) :width '(ch 17)
       (text :color (etaf-pi-color :ink) :font-weight 'bold "Pi")
       (box :height '(lh 2) "")
       (expr (etaf-pi--control "新对话" 'pi-new (plist-get actions :new)))
       (box :height '(lh 1) "")
       (expr (etaf-pi--control "历史会话" 'pi-resume (plist-get actions :resume)))
       (box :height '(lh 2) "")
       (text :color (etaf-pi-color :muted) "当前对话")
       (box :height '(lh 1) "")
       (etaf-button :aria-label title :ref 'pi-follow :id 'pi-follow :on-press (plist-get actions :follow)
                    :variant 'secondary :padding '((lh 0) (ch 1))
                    :color (etaf-pi-color :accent) :bgcolor (etaf-pi-color :accent-soft)
         (box :width '(ch 14) :height '(lh 1) :wrap-mode 'none
           (text (expr (truncate-string-to-width title 14 nil nil "…"))))))
     (box :position 'absolute :z-index 0 :left '(ch 2) :top (list 'lh (- height 3)) :width '(ch 17)
       (expr (etaf-pi--control "帮助" 'pi-help (plist-get actions :help)))))))

(defun etaf-pi--composer-view (model thinking busy pending sending width actions)
  "Render selectors below native input, with send/stop on the trailing edge."
  (etaf-view
   (box :position 'relative :width 'stretch :height '(lh 2) :background-color (etaf-pi-color :surface)
     (flex :position 'absolute :z-index 0 :left '(ch 1) :top '(lh 0)
           :width (list 'ch (- width 4)) :height '(lh 2) :flex-direction 'row :align-items 'center
           :gap '((lh 0) (ch 1))
       (etaf-button :ref 'pi-model :id 'pi-model :on-press (plist-get actions :model)
                    :disabled (or busy sending) :variant 'ghost :color (etaf-pi-color :muted)
                    :bgcolor (etaf-pi-color :surface) :font-weight 'normal
         (flex :flex-direction 'row :gap '((lh 0) (ch 1))
           (text (expr (truncate-string-to-width model (if (< width 70) 16 22) nil nil "…")))
           (etaf-ui-icon :name 'chevron-down :set 'svg :color (etaf-pi-color :muted))))
       (expr (when (>= width 74)
               (etaf-pi--control (concat "思考 " (or (cdr (assoc thinking '(("off" . "标准") ("high" . "深入") ("max" . "最高")))) thinking))
                                'pi-thinking (plist-get actions :thinking) nil (or busy sending))))
       (expr (when (> pending 0)
               (etaf-pi--control "待确认" 'pi-answer (plist-get actions :answer))))
       (box :flex-grow 1 :flex-basis '(ch 0) :min-width '(ch 0) "")
       (expr (when busy (etaf-pi--control "停止" 'pi-abort (plist-get actions :abort))))
       (etaf-button :ref 'pi-compose :id 'pi-compose :on-press (plist-get actions :compose)
                    :disabled sending :aria-label "发送消息" :help-echo "Enter 发送 · Shift+Enter 换行"
                    :padding '((lh 0) (ch 1)) :font-weight 'normal
                    :bgcolor (etaf-pi-color :accent) :color (etaf-pi-color :button-ink)
         (etaf-ui-icon :name (if sending 'loading 'chevron-up) :set 'svg :lines 2 :min-width 3
                       :color (etaf-pi-color :button-ink)))))))

(provide 'etaf-pi-views)
;;; etaf-pi-views.el ends here
