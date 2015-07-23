;;; Commentary:
;; Meant to be evaluated by `emacsclient' on the go.

;;; Code:

(defun frame-theme (term-theme x-theme)
  "A tiny function to set themes based on the environment (terminal/X1).
TERM-THEME: the theme to set when within a terminal
X-THEME: the theme to set when within X11"
  (setq color-theme-is-global nil) ;; make `load-theme' non-global

  (if (display-graphic-p)
      (load-theme x-theme t)
    (load-theme term-theme t)))

(provide 'frame-theme)
;;; frame-theme.el ends here
