;;; etaf-pi-panes.el --- Owned native pane for conversation navigation -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; The conversation and its composer are one ETAF surface. The optional
;; navigation sidebar is a second owned ETAF application in a real window;
;; only windows displaying owned buffers are created or removed.
;;; Code:
(require 'cl-lib)
(require 'etaf)
(require 'etaf-pi-views)
(defvar-local etaf-pi--navigation nil)
(defvar-local etaf-pi--navigation-window nil)
(defvar-local etaf-pi--palette nil
  "Ref holding the color mode this client last mounted its views with.")

(defun etaf-pi--remove-pane-window (buffer)
  "Remove displayed windows of BUFFER, never the last window of a frame."
  (when (buffer-live-p buffer)
    (dolist (window (get-buffer-window-list buffer nil t))
      (unless (one-window-p t (window-frame window)) (delete-window window)))))

(defun etaf-pi--open-navigation (main actions)
  "Open this conversation's navigation application left of window MAIN.
ACTIONS target the conversation.  The sidebar follows its window's settled
rows and the client's palette reactively; its state is the owning buffer."
  (let* ((owner (current-buffer))
         (title (file-name-nondirectory (directory-file-name default-directory)))
         (palette etaf-pi--palette)
         (rows (etaf-ref (window-body-height main))))
    (etaf-app-open
     (generate-new-buffer-name (format " *Pi 会话: %s*" (buffer-name)))
     (lambda ()
       ;; The view reads the palette so a theme change repaints its colors.
       (etaf-value palette)
       (etaf-pi--sidebar-view title (etaf-value rows) actions))
     :state owner :cursor nil
     :background (lambda () (etaf-pi-color :surface (etaf-value palette)))
     :foreground (lambda () (etaf-pi-color :ink (etaf-value palette)))
     :on-fit (lambda (window) (setf (etaf-value rows) (window-body-height window)))
     :display (lambda (buffer)
                (let ((window (split-window main -22 'left)))
                  (set-window-buffer window buffer)
                  (with-current-buffer buffer (setq-local window-size-fixed 'width))
                  (with-current-buffer owner (setq etaf-pi--navigation-window window)))))))

(defun etaf-pi--ensure-navigation (actions)
  "Keep a full-height conversation sidebar only when the owned area fits.
ACTIONS bind the sidebar's controls to this conversation."
  (when-let* ((main (get-buffer-window (current-buffer) t)))
    (let* ((nav (and (buffer-live-p etaf-pi--navigation) (get-buffer-window etaf-pi--navigation t)))
           (columns (+ (window-total-width main) (if nav (window-total-width nav) 0)))
           (show (and (>= columns 110) (>= (frame-height (window-frame main)) 26))))
      (cond
       ((and show (not nav) (not (window-live-p etaf-pi--navigation-window)))
        (if (buffer-live-p etaf-pi--navigation)
            (let ((window (split-window main -22 'left)))
              (set-window-buffer window etaf-pi--navigation)
              (setq etaf-pi--navigation-window window))
          (setq etaf-pi--navigation (etaf-pi--open-navigation main actions))))
       ((and nav (not show)) (etaf-pi--remove-pane-window etaf-pi--navigation))))))

(defun etaf-pi--dispose-panes ()
  "Release only the owned navigation application and its window."
  (when (buffer-live-p etaf-pi--navigation)
    (etaf-pi--remove-pane-window etaf-pi--navigation)
    (kill-buffer etaf-pi--navigation))
  (setq etaf-pi--navigation nil etaf-pi--navigation-window nil))

(provide 'etaf-pi-panes)
;;; etaf-pi-panes.el ends here
