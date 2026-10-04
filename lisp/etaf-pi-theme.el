;;; etaf-pi-theme.el --- Semantic palette of etaf-pi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; One palette read through `etaf-pi-color', so a palette change can never
;; leak a half-updated set of colors.  Replace the values with your own and
;; keep every view reading colors through this function.

;;; Code:

(defconst etaf-pi-palette
  '((:bg . "#10131b")
    (:surface . "#191e2a")
    (:surface-2 . "#202737")
    (:ink . "#e3e7f1")
    (:muted . "#99a6bd")
    (:accent . "#b6a5fa")
    (:accent-soft . "#25283d")
    (:button-ink . "#171323")
    (:border . "#343c50")
    (:ok . "#8bd5be")
    (:error . "#f1a0ab"))
  "Semantic palette of the application.")

(defun etaf-pi-color (key)
  "Return the palette color for KEY."
  (or (alist-get key etaf-pi-palette)
      (if (eq (frame-parameter nil 'background-mode) 'dark) "#e6edf3" "#1f2328")))

(defun etaf-pi--theme-tokens ()
  "Supply the shared catalog with the workbench's semantic palette."
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
        :ui-progress-track (etaf-pi-color :surface-2)))

(provide 'etaf-pi-theme)
;;; etaf-pi-theme.el ends here
