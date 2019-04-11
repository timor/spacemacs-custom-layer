(defconst timor-packages '(
                           exwm
                           winner
                           evil-lisp-state
                           ivy
                           projectile
                           company
                           fuel
                           evil
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
  (define-key evil-inner-text-objects-map "l" 'evil-inner-line)
  (define-key evil-inner-text-objects-map "d" 'evil-inner-defun)

(evil-define-text-object evil-inner-defun (count &optional beg end type)
  (save-excursion
    (mark-defun)
    (evil-range (region-beginning) (region-end) type :expanded t)))

(evil-define-text-object evil-inner-line (count &optional beg end type)
  "Define the current line contents, from first non-blank to
    last non-blank as text object"
  (list (save-excursion (evil-first-non-blank) (point))
        (save-excursion (evil-last-non-blank) (1+ (point)))))

  )

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
  (with-eval-after-load 'fuel-listener
    (define-key fuel-listener-mode-map (kbd "<C-return>") 'timor/fuel-send-with-dup))
  (add-hook 'fuel-listener-mode-hook 'timor/fuel-fix-sp-single-quote)
  (add-hook 'factor-mode-hook 'timor/fuel-fix-sp-single-quote)
  (with-eval-after-load 'smartparens
    (loop for char across "[{(" do
          (sp-local-pair '(factor-mode fuel-listener-mode) (string char) nil
                         :post-handlers '(:add timor//fuel-mode-sp-post-handler))))

  ;; These modes have their own bindings
  ;; (evil-set-initial-state 'fuel-debug-mode 'insert)
  ;; (evil-set-initial-state 'fuel-debug-uses-mode 'insert)
  (with-eval-after-load 'ivy
    (spacemacs/set-leader-keys-for-major-mode 'factor-mode
      "il" 'timor/fuel-mode-insert-from-listener-input-ring
      "tA" 'timor/fuel-test-vocab-refresh))
  (with-eval-after-load 'fuel-help
    (define-key fuel-help-mode-map (kbd "o") 'link-hint-open-link)
    ))
