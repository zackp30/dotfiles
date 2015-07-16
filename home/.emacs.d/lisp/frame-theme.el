;; Meant to be evaluated by `emacsclient' on the go.

;;; Code:

(defun frame-theme (term-theme x-theme & disp)
  (setq color-theme-is-global nil) ;; make `load-theme' non-global

  (if disp
      (load-theme x-theme t)
    (load-theme term-theme t)))

(provide 'frame-theme)
;;; frame-theme.el ends here
