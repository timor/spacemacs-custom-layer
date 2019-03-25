(defconst timor-packages '(
                           exwm
                           winner
                           evil-lisp-state
                           ivy
                           projectile
                           company
                           fuel
                           ))

(defun timor/post-init-exwm ()
  (spacemacs/exwm-bind-switch-to-or-run-command "s-f" "Firefox" "firefox")
  (spacemacs/exwm-bind-command "<XF86MonBrightnessUp>" "light -A 5")
  (spacemacs/exwm-bind-command "<XF86MonBrightnessDown>" "light -U 5")
  (exwm-input-set-key (kbd "<s-tab>") 'spacemacs/alternate-buffer))

(defun timor/sp-wrap-as-string (&optional arg)
  "Wrap a symbol with string quotes."
  (interactive "P")
  (sp-wrap-with-pair "\""))

(defun timor/post-init-evil-lisp-state ()
  (with-eval-after-load 'evil-lisp-state
    (define-key evil-lisp-state-map (kbd "j") (evil-lisp-state-enter-command timor/next-open-paren))
    (define-key evil-lisp-state-map (kbd "SPC") spacemacs-default-map)
    (define-key evil-lisp-state-map (kbd "'") (lambda () (interactive) (avy-goto-char ?\()))
    (define-key evil-lisp-state-map (kbd "q") 'sp-indent-defun)
    (define-key evil-lisp-state-map (kbd "O") 'evil-open-above)
    (define-key evil-lisp-state-map (kbd "o") 'evil-open-below)
    (define-key evil-lisp-state-map (kbd "A") 'evil-append-li)
    (define-key evil-lisp-state-map (kbd "f") 'evil-find-char)
    (define-key evil-lisp-state-map (kbd "C") 'timor/change-sexp)
    (define-key evil-lisp-state-map (kbd "F") 'evil-find-char-backward)
    (define-key evil-lisp-state-map (kbd ";") 'evil-repeat-find-char)
    (define-key evil-lisp-state-map (kbd "*") 'spacemacs/enter-ahs-forward)
    (define-key evil-lisp-state-map (kbd "\"") 'timor/sp-wrap-as-string)
    (define-key evil-insert-state-map [escape] 'timor/evil-escape-dwim))
  )

(defun timor/post-init-evil ()
  (evil-define-text-object evil-inner-line (count &optional beg end type)
    "Define the current line contents, from first non-blank to
    last non-blank as text object"
    (list (save-excursion (evil-first-non-blank) (point))
          (save-excursion (evil-last-non-blank) (1+ (point)))))
  (define-key evil-inner-text-objects-map "l" 'evil-inner-line))

(defun timor/post-init-winner ()
  (with-eval-after-load 'winner
    (dolist (n '("*Help*" "*inferior-lisp*"))
      (delete n winner-boring-buffers))))

(defun timor/post-init-ivy ()
  (setq ivy-initial-inputs-alist '())
  (spacemacs/set-leader-keys "/" 'timor/search-project-auto))

(defun timor/post-init-projectile ()
  (spacemacs/set-leader-keys "ps" 'projectile-save-project-buffers)
  )

(defun timor/post-init-company ()
  (spacemacs|disable-company 'eshell-mode)
  )

(defun timor/post-init-fuel ()
  (message "DEBUG(timor-layer): post init fuel")
  (with-eval-after-load 'fuel-listener
    (message "DEBUG(timor-layer): eval after load fuel-listener-mode")
    (define-key fuel-listener-mode-map (kbd "<C-return>") 'timor/fuel-send-with-dup))
  (add-hook 'fuel-listener-mode-hook 'timor/fuel-fix-sp-single-quote)
  (add-hook 'factor-mode-hook 'timor/fuel-fix-sp-single-quote)
  (with-eval-after-load 'smartparens
    (loop for mode in '(fuel-listener-mode factor-mode) do
         (loop for char across "[{(" do
               (sp-local-pair mode (string char) nil :post-handlers '(:add " | ")))))

  ;; These modes have their own bindings
  (evil-set-initial-state 'fuel-debug-mode 'insert)
  (evil-set-initial-state 'fuel-debug-uses-mode-hook 'insert)
  )
