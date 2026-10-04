;;; etaf-pi-panes.el --- Owned native pane for conversation navigation -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; The conversation and its composer are one ETAF surface. The optional
;; navigation sidebar is a second owned ETAF buffer in a real window; only
;; windows displaying owned buffers are created or removed.
;;; Code:
(require 'cl-lib)
(require 'etaf)
(require 'etaf-pi-views)
(defvar-local etaf-pi--pane-owner nil)
(defvar-local etaf-pi--navigation nil)
(defvar-local etaf-pi--navigation-window nil)
(defvar-local etaf-pi--navigation-painted nil)

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
        (let ((window (split-window main -22 'left)))
          (set-window-buffer window etaf-pi--navigation)
          (setq etaf-pi--navigation-window window)
          (with-current-buffer etaf-pi--navigation (setq-local window-size-fixed 'width))))
       ((and nav (not show)) (etaf-pi--remove-pane-window etaf-pi--navigation))))))

(defun etaf-pi--sync-navigation (title actions)
  "Repaint the visible navigation sidebar when TITLE, size or ACTIONS changed."
  (when-let* ((window (and (buffer-live-p etaf-pi--navigation)
                           (get-buffer-window etaf-pi--navigation t))))
    (let* ((height (window-body-height window))
           (signature (list title height (window-body-width window))))
      (unless (equal signature etaf-pi--navigation-painted)
        (etaf-mount etaf-pi--navigation (etaf-pi--sidebar-view title height actions))
        (with-current-buffer etaf-pi--navigation (setq-local cursor-type nil))
        (setq etaf-pi--navigation-painted signature)))))

(defun etaf-pi--dispose-panes ()
  "Release only the owned navigation buffer and its window."
  (when (buffer-live-p etaf-pi--navigation)
    (etaf-pi--remove-pane-window etaf-pi--navigation)
    (when-let* ((runtime (etaf-runtime-for-buffer etaf-pi--navigation))) (etaf-unmount runtime))
    (kill-buffer etaf-pi--navigation))
  (setq etaf-pi--navigation nil etaf-pi--navigation-window nil))

(provide 'etaf-pi-panes)
;;; etaf-pi-panes.el ends here
