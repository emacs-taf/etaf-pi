;;; etaf-pi-theme.el --- Light and dark semantic palette of etaf-pi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; One palette read through `etaf-pi-color', so a palette change can never
;; leak a half-updated set of colors.  Every entry is a (LIGHT . DARK) pair
;; resolved against the frame's background mode, so the client follows the
;; user's Emacs theme instead of hard-coding one.  Replace the values with
;; your own and keep every view reading colors through this function.

;;; Code:

(require 'etaf)

(defconst etaf-pi-palette
  '((:bg          . ("#f7f8fc" . "#0f1218"))
    (:surface     . ("#ffffff" . "#161a22"))
    (:surface-2   . ("#eef0f6" . "#1e2430"))
    (:ink         . ("#232a33" . "#e6eaf2"))
    (:muted       . ("#68738a" . "#9aa7bd"))
    (:accent      . ("#6d4ddb" . "#b6a5fa"))
    (:accent-soft . ("#ece7fd" . "#262a3f"))
    (:button-ink  . ("#ffffff" . "#171323"))
    (:border      . ("#e3e6ee" . "#2c3446"))
    (:ok          . ("#177a58" . "#8bd5be"))
    (:error       . ("#c5263f" . "#f1a0ab"))
    (:code-bg     . ("#f2f4f9" . "#12161f"))
    (:user-bg     . ("#efeafd" . "#222a44")))
  "Semantic palette of the application.
Each value is a (LIGHT . DARK) pair; `etaf-pi-color' resolves the pair
against the frame's background mode.")

(defun etaf-pi--palette-mode ()
  "Return the color mode the client should paint with."
  (etaf-theme-current-mode))

(defun etaf-pi-color (key)
  "Return the palette color for KEY in the current color mode."
  (let ((value (alist-get key etaf-pi-palette)))
    (cond
     ((stringp value) value)
     ((and (consp value) (stringp (car value)) (stringp (cdr value)))
      (if (eq (etaf-pi--palette-mode) 'dark) (cdr value) (car value)))
     (t (if (eq (etaf-pi--palette-mode) 'dark) "#e6edf3" "#1f2328")))))

(defun etaf-pi--theme-tokens ()
  "Supply the shared catalog with the workbench's semantic palette."
  (etaf-theme-resolve-palette
   (list :ui-fg (etaf-pi-color :ink) :ui-bg (etaf-pi-color :bg)
         :ui-muted (etaf-pi-color :muted) :ui-border (etaf-pi-color :border)
         :ui-muted-fg (etaf-pi-color :muted)
         :ui-panel-fg (etaf-pi-color :ink)
         :ui-panel-bg (etaf-pi-color :surface-2)
         :ui-panel-border (etaf-pi-color :border)
         :ui-success-fg (etaf-pi-color :ok) :ui-success-bg (etaf-pi-color :accent-soft)
         :ui-danger-fg (etaf-pi-color :error)
         :ui-accent (etaf-pi-color :accent) :ui-accent-fg (etaf-pi-color :button-ink)
         :ui-accent-soft-bg (etaf-pi-color :accent-soft)
         :ui-progress-track (etaf-pi-color :surface-2))
   (etaf-pi--palette-mode)))

(provide 'etaf-pi-theme)
;;; etaf-pi-theme.el ends here
