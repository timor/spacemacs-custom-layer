(defconst timor-packages '(
                           exwm
                           winner
                           evil-lisp-state
                           ivy
                           projectile
                           company
                           ))

(defun timor/post-init-exwm ()
  (spacemacs/exwm-bind-switch-to-or-run-command "s-f" "Firefox" "firefox")
  (spacemacs/exwm-bind-command "<XF86MonBrightnessUp>" "xbacklight -inc 5")
  (spacemacs/exwm-bind-command "<XF86MonBrightnessDown>" "xbacklight -dec 5")
  (exwm-input-set-key (kbd "<s-tab>") 'spacemacs/alternate-buffer))

(defun timor/post-init-evil-lisp-state ()
  (define-key evil-lisp-state-map (kbd "SPC") spacemacs-default-map))

(defun timor/post-init-winner ()
  (dolist (n '("*Help*" "*inferior-lisp*"))
    (delete n winner-boring-buffers)))

(defun timor/post-init-ivy ()
  (setq ivy-initial-inputs-alist '()))

(defun timor/post-init-projectile ()
  (spacemacs/set-leader-keys "ps" 'projectile-save-project-buffers)
  )

(defun timor/post-init-company ()
  (spacemacs|disable-company 'eshell-mode)
  )
