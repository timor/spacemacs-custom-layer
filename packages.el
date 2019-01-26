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
  (spacemacs/exwm-bind-command "<XF86MonBrightnessUp>" "light -A 5")
  (spacemacs/exwm-bind-command "<XF86MonBrightnessDown>" "light -U 5")
  (exwm-input-set-key (kbd "<s-tab>") 'spacemacs/alternate-buffer))

(defun timor/post-init-evil-lisp-state ()
  (with-eval-after-load 'evil-lisp-state
    (define-key evil-lisp-state-map (kbd "SPC") spacemacs-default-map)
    (define-key evil-lisp-state-map (kbd "'") (lambda () (interactive) (avy-goto-char ?\()))
    (define-key evil-lisp-state-map (kbd "O") 'evil-open-above)
    (define-key evil-lisp-state-map (kbd "o") 'evil-open-below)
    (define-key evil-lisp-state-map (kbd "f") 'evil-find-char)
    (define-key evil-lisp-state-map (kbd "F") 'evil-find-char-backward)
    (define-key evil-lisp-state-map (kbd ";") 'evil-repeat-find-char)
    (define-key evil-lisp-state-map (kbd "*") 'spacemacs/enter-ahs-forward)
  )

(defun timor/post-init-winner ()
  (with-eval-after-load 'winner
    (dolist (n '("*Help*" "*inferior-lisp*"))
      (delete n winner-boring-buffers))))

(defun timor/post-init-ivy ()
  (setq ivy-initial-inputs-alist '()))

(defun timor/post-init-projectile ()
  (spacemacs/set-leader-keys "ps" 'projectile-save-project-buffers)
  )

(defun timor/post-init-company ()
  (spacemacs|disable-company 'eshell-mode)
  )
