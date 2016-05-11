;;; Presentations in Org

(defun pres-vis ()
  (org-show-subtree)
  (org-narrow-to-subtree)
  (org-show-entry)
  (org-show-block-all))

(defun pres-previous ()
  (interactive)
  (widen)
  (org-previous-visible-heading 1)
  (pres-vis))

(defun pres-next ()
  (interactive)
  (widen)
  (org-next-visible-heading 1)
  (pres-vis))

(define-minor-mode pres-mode
  "Toggle pres-mode"
  :init-value nil
  :lighter " Pres "
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-s <") 'pres-previous)
            (define-key map (kbd "M-s >") 'pres-next)
            map)
  (unless pres-mode
    (widen)))

(define-key org-mode-map (kbd "M-s C-s s") 'pres-mode)
