;;; etaf-pi-panes.el --- Owned native panes for chat and input -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; The conversation, sidebar and composer toolbar are ETAF views. The text
;; editor is a real Emacs buffer, between the conversation and its toolbar.
;; Only windows displaying these owned buffers are created or removed.
;;; Code:
(require 'cl-lib)
(require 'etaf)
(require 'etaf-pi-views)
(defvar-local etaf-pi--pane-owner nil)
(defvar-local etaf-pi--navigation nil)
(defvar-local etaf-pi--navigation-window nil)
(defvar-local etaf-pi--composer-bar nil)
(defvar-local etaf-pi--navigation-painted nil)
(defvar-local etaf-pi--bar-painted nil)
(defvar-local etaf-pi--input-area nil)
(defvar-local etaf-pi--input-buffer nil)

(define-derived-mode etaf-pi--pane-mode special-mode "Pi"
  "Read-only application chrome."
  (setq-local mode-line-format nil header-line-format nil cursor-type nil)
  (setq-local truncate-lines t)
  (when (bound-and-true-p display-line-numbers-mode) (display-line-numbers-mode -1))
  (face-remap-set-base 'default (list :background (etaf-pi-color :surface) :foreground (etaf-pi-color :ink))))

(defun etaf-pi--pane-buffer (kind)
  "Create an owned pane buffer named KIND for this conversation."
  (let ((owner (current-buffer))
        (buffer (generate-new-buffer (format " *Pi %s: %s*" kind (buffer-name)))))
    (with-current-buffer buffer (etaf-pi--pane-mode) (setq etaf-pi--pane-owner owner))
    buffer))

(defun etaf-pi--remove-pane-window (buffer)
  "Remove displayed windows of BUFFER, never the last window of a frame."
  (when (buffer-live-p buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (unless (one-window-p t (window-frame window)) (delete-window window)))))

(defun etaf-pi--owned-window-tree-p (window buffers)
  "Return non-nil when every leaf below WINDOW belongs to BUFFERS."
  (if (window-live-p window)
      (memq (window-buffer window) buffers)
    (let ((child (window-child window)) (owned t))
      (while child
        (unless (etaf-pi--owned-window-tree-p child buffers) (setq owned nil))
        (setq child (window-next-sibling child)))
      owned)))

(defun etaf-pi--ensure-navigation ()
  "Keep a full-height conversation sidebar only when the owned area fits."
  (when-let* ((main (get-buffer-window (current-buffer) t)))
    (let* ((nav (and (buffer-live-p etaf-pi--navigation) (get-buffer-window etaf-pi--navigation t)))
           (columns (+ (window-total-width main) (if nav (window-total-width nav) 0)))
           (show (and (>= columns 110) (>= (frame-height (window-frame main)) 26))))
      (cond
       ((and show (not nav) (not (window-live-p etaf-pi--navigation-window)))
        (unless (buffer-live-p etaf-pi--navigation)
          (setq etaf-pi--navigation (etaf-pi--pane-buffer "会话")))
        (let* ((area (if (and etaf-pi--input-area (window-valid-p etaf-pi--input-area)
                              (etaf-pi--owned-window-tree-p etaf-pi--input-area
                               (list (current-buffer) etaf-pi--input-buffer etaf-pi--composer-bar)))
                         etaf-pi--input-area main))
               (window (split-window area -22 'left)))
          (set-window-buffer window etaf-pi--navigation)
          (setq etaf-pi--navigation-window window)
          (with-current-buffer etaf-pi--navigation (setq-local window-size-fixed 'width))))
       ((and nav (not show)) (etaf-pi--remove-pane-window etaf-pi--navigation))))))

(defun etaf-pi--show-input (editor)
  "Show EDITOR below this conversation, followed by its ETAF action bar."
  (setq etaf-pi--input-buffer editor)
  (let ((main (get-buffer-window (current-buffer) t)))
    (cond
     ((get-buffer-window editor t) (select-window (get-buffer-window editor t)))
     ((and main (>= (window-total-height main) 17))
      (let* ((window-min-height 1)
             (input (split-window main -6 'below))
             (bar (split-window input -2 'below)))
        (unless (buffer-live-p etaf-pi--composer-bar)
          (setq etaf-pi--composer-bar (etaf-pi--pane-buffer "输入操作")))
        (set-window-buffer input editor)
        (set-window-buffer bar etaf-pi--composer-bar)
        (with-current-buffer etaf-pi--composer-bar (setq-local window-size-fixed 'height))
        (set-window-parameter bar 'no-other-window t)
        (setq etaf-pi--input-area (window-parent main))
        (select-window input)))
     (t (let ((display-buffer-overriding-action '((display-buffer-same-window)))) (pop-to-buffer editor))))))

(defun etaf-pi--hide-input (editor)
  "Hide EDITOR and its toolbar, preserving their buffers."
  (etaf-pi--remove-pane-window etaf-pi--composer-bar)
  (etaf-pi--remove-pane-window editor)
  (setq etaf-pi--input-area nil))

(defun etaf-pi--sync-pane-views (title model thinking busy pending sending actions)
  "Synchronize chrome only when its visible state or geometry changed."
  (when-let* ((window (and (buffer-live-p etaf-pi--navigation)
                           (get-buffer-window etaf-pi--navigation t))))
    (let* ((height (window-body-height window))
           (signature (list title height (window-body-width window))))
      (unless (equal signature etaf-pi--navigation-painted)
        (etaf-mount etaf-pi--navigation (etaf-pi--sidebar-view title height actions))
        (with-current-buffer etaf-pi--navigation (setq-local cursor-type nil))
        (setq etaf-pi--navigation-painted signature))))
  (when-let* ((window (and (buffer-live-p etaf-pi--composer-bar)
                           (get-buffer-window etaf-pi--composer-bar t))))
    (let* ((width (window-body-width window))
           (signature (list width model thinking busy pending sending)))
      (unless (equal signature etaf-pi--bar-painted)
        (etaf-mount etaf-pi--composer-bar
                    (etaf-pi--composer-view model thinking busy pending sending width actions))
        (with-current-buffer etaf-pi--composer-bar (setq-local cursor-type nil))
        (setq etaf-pi--bar-painted signature)))))

(defun etaf-pi--dispose-panes ()
  "Release only the owned sidebar and composer toolbar."
  (dolist (buffer (list etaf-pi--navigation etaf-pi--composer-bar))
    (when (buffer-live-p buffer)
      (etaf-pi--remove-pane-window buffer)
      (when-let* ((runtime (etaf-runtime-for-buffer buffer))) (etaf-unmount runtime))
      (kill-buffer buffer)))
  (setq etaf-pi--navigation nil etaf-pi--navigation-window nil etaf-pi--composer-bar nil))

(provide 'etaf-pi-panes)
;;; etaf-pi-panes.el ends here
