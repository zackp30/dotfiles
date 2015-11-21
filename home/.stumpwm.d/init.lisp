(in-package :stumpwm)
(setf *normal-border-width* 2)

(set-focus-color "orange")

(set-unfocus-color "palegreen3")

(defcommand reinit () ()
  (run-commands "reload" "loadrc"))

(defcommand rofi () ()
  (run-shell-command "rofi -show run"))

(defcommand chromium () () ;; "chrome" on xieshaij, chromium elsehwhere; TODO: Other machines.
  (run-shell-command "CHROME_DEVEL_SANDBOX=~/chrome-linux/chrome_sandbox ~/chrome-linux/chrome"))

(set-prefix-key (kbd "C-z"))

(define-key *root-map* (kbd "c") "exec urxvt-256color") ;; terminal
(define-key *root-map* (kbd "C-c") "chromium") ;; browser

(define-key *root-map* (kbd "C-z") "fnext")
(define-key *root-map* (kbd "C-x") "fprev")

(define-key *root-map* (kbd "C-q") "reinit") ;; reload config
(define-key *root-map* (kbd "p") "rofi") ;; dmenu-like utility

(setq *mouse-focus-policy* :sloppy)

(loop for i from 0 to 9 do (run-commands (format nil "gnewbg ~a" i))) ;; 10 workspaces

(defcommand (fprev tile-group) () ()
            "Cycle through the frame tree to the prev frame."
            (focus-prev-frame (current-group)))
