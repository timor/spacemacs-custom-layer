(defconst timor-packages '(
                           edit-server
                           exwm
                           winner
                           evil-lisp-state
                           ivy
                           projectile
                           company
                           notmuch
                           fuel
                           evil
                           outshine
                           vterm
                           weechat
                           link-hint
                           bluetooth
                           counsel-projectile
                           auto-highlight-symbol
                           swiper
                           osm
                           ))

(defun timor/init-edit-server ()
  (use-package edit-server
    :init (edit-server-start)
    :config (setq edit-server-default-major-mode 'markdown-mode)))

(defun timor/post-init-swiper ()
  (add-to-list 'savehist-ignored-variables 'swiper-history))

(defun timor/init-bluetooth ()
    (use-package bluetooth
      :defer t
      :init
      (progn
        (spacemacs/set-leader-keys "ob" 'bluetooth-list-devices))))

(defun timor/init-osm ()
  (use-package osm
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "oo" 'osm-home
        )))
    :config
    (evilified-state-evilify osm-mode osm-mode-map
      "j" 'osm-down
      "J" 'osm-down-down
      "k" 'osm-up
      "K" 'osm-up-up
      "h" 'osm-left
      "H" 'osm-left-left
      "l" 'osm-right
      "L" 'osm-right-right
      "g" 'osm-home
      "/" 'counsel-osm-view
      "s" 'counsel-osm
      "v" 'osm-server
      "=" 'osm-zoom-in
      ))

(defun timor/init-weechat ()
  (use-package weechat
    :defer t
    :init
    (progn
      (defvar weechat-formatting-regex
        (rx-let ((attr (in "*!/_|"))   ;NOTE:  is not documented
                 (std  (= 2 digit))
                 (astd (seq attr (= 2 digit)))
                 (ext  (seq "@" (= 5 digit)))
                 (aext (seq "@" attr (= 5 digit))))
          (rx
           (or (seq ""
                    (or std
                        ext
                        (seq "F" (or std astd ext aext))
                        (seq "B" (or std ext))
                        (seq "*" (or std
                                     astd
                                     ext
                                     aext
                                     (seq (or std astd ext aext)
                                          ","
                                          (or std astd ext aext))))
                        (seq "b" (in "-FDB#_il"))
                        ""))
               (seq "" attr)
               (seq "" attr)
               ""))))
      ;; (autoload 'rx-form)
      )
    ))

(defun timor/post-init-vterm ()
  ;; (add-hook 'vterm-mode-hook 'timor//vterm-setup-evil-hooks)
  (spacemacs/set-leader-keys-for-major-mode 'vterm-mode "t" 'vterm-copy-mode)
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'visual 'vterm-mode-map
      "[[" 'vterm-previous-prompt
      "]]" 'vterm-next-prompt
      )))

(defun timor/post-init-counsel-projectile ()
  (with-eval-after-load 'counsel-projectile
    (counsel-projectile-modify-action
     'counsel-projectile-switch-project-action
     '((default counsel-projectile-switch-project-action-vc)))
    (add-to-list 'spacemacs--ivy-file-actions
                 '("x" counsel-find-file-extern "open externally") t)))

(defun timor/post-init-link-hint ()
  (spacemacs/set-leader-keys "xo" 'link-hint-open-link))

(defun timor/post-init-exwm ()
  (exwm/bind-switch-to-or-run-command "s-f" "Vivaldi-stable" "vivaldi")
  (exwm/bind-command "<XF86MonBrightnessUp>" "light -A 5")
  (exwm/bind-command "<XF86MonBrightnessDown>" "light -U 5")
  (exwm/bind-switch-to-or-run-command "s-v" "Pavucontrol" "pavucontrol")
  (exwm-input-set-key (kbd "<s-tab>") 'spacemacs/alternate-buffer)
  (exwm-input-set-key (kbd "s-p") 'counsel-projectile-switch-project)
  )

(defun timor/sp-wrap-as-string (&optional arg)
  "Wrap a symbol with string quotes."
  (interactive "P")
  (sp-wrap-with-pair "\""))

(defun timor/post-init-evil-lisp-state ()
  (with-eval-after-load 'evil-lisp-state
    (define-key evil-lisp-state-map (kbd "j")
      (evil-lisp-state-enter-command timor/next-open-paren))
    (define-key evil-lisp-state-map (kbd "SPC") spacemacs-default-map)
    (define-key evil-lisp-state-map (kbd "'") (lambda () (interactive) (avy-goto-char ?\()))
    (define-key evil-lisp-state-map (kbd "q") 'sp-indent-defun)
    (define-key evil-lisp-state-map (kbd "O") 'evil-open-above)
    (define-key evil-lisp-state-map (kbd "o") 'evil-open-below)
    (define-key evil-lisp-state-map (kbd "A") 'evil-append-line)
    (define-key evil-lisp-state-map (kbd "f") 'evil-find-char)
    (define-key evil-lisp-state-map (kbd "C") 'timor/change-sexp)
    (define-key evil-lisp-state-map (kbd "F") 'evil-find-char-backward)
    (define-key evil-lisp-state-map (kbd ";") 'evil-repeat-find-char)
    (define-key evil-lisp-state-map (kbd "*") 'spacemacs/enter-ahs-forward)
    (define-key evil-lisp-state-map (kbd "\"") 'timor/sp-wrap-as-string)
    (define-key evil-lisp-state-map (kbd "g") 'spacemacs/jump-to-definition)
    (define-key evil-lisp-state-map (kbd "G") 'spacemacs/jump-to-definition-other-window)
    (define-key evil-lisp-state-map (kbd "x") 'sp-kill-sexp)
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
  (spacemacs/set-leader-keys "p'" 'timor/projectile-run-term-with-default-shell)
  )

(defun timor/post-init-company ()
  (spacemacs|disable-company 'eshell-mode)
  )

(defun timor/post-init-fuel ()
  (with-eval-after-load 'fuel-listener
    (define-key fuel-listener-mode-map (kbd "<C-return>") 'timor/fuel-send-keep-inputs)
    (define-key fuel-listener-mode-map (kbd "C-d") 'timor/fuel-send-drop))
  (add-hook 'fuel-listener-mode-hook 'timor/fuel-fix-sp-single-quote)
  (spacemacs/set-leader-keys-for-major-mode 'factor-mode "hV" 'fuel-help-vocab)
  (with-eval-after-load 'smartparens
    (loop for char across "[{(" do
          (sp-local-pair '(factor-mode fuel-listener-mode) (string char) nil
                         :post-handlers '(:add timor//fuel-mode-sp-post-handler)
                         :pre-handlers '(:add timor//fuel-mode-sp-pre-handler)))
    (add-hook 'factor-mode-hook 'timor/fuel-fix-sp-single-quote)
    (add-hook 'factor-mode-hook 'timor/fuel-setup-lisp-state)
    ;; Does not work with smartparens mode because cannot insert ';' anymore...
    ;; (sp-local-pair '(factor-mode fuel-listener-mode) ":" " ;" :actions '(insert autoskip navigate)
    ;;                :post-handlers '(timor//factor-mode-colon-post-insert))
    ;; (sp-local-pair '(factor-mode fuel-listener-mode) "::" " ;")
    (sp-local-pair '(factor-mode fuel-listener-mode) "[ " " ]")
    (sp-local-pair '(factor-mode fuel-listener-mode) "{ " " }")
    (sp-local-pair '(factor-mode fuel-listener-mode) "( " " )")
    )

  ;; These modes have their own bindings
  ;; (evil-set-initial-state 'fuel-debug-mode 'insert)
  ;; (evil-set-initial-state 'fuel-debug-uses-mode 'insert)
  (with-eval-after-load 'ivy
    (spacemacs/set-leader-keys-for-major-mode 'factor-mode
      "il" 'timor/fuel-mode-insert-from-listener-input-ring
      "tA" 'timor/fuel-test-vocab-refresh))
  (with-eval-after-load 'fuel-help
    (define-key fuel-help-mode-map (kbd "o") 'link-hint-open-link)
    (define-key fuel-help-mode-map (kbd "H") 'fuel-help)
    (define-key fuel-help-mode-map (kbd "J") 'forward-button)
    (define-key fuel-help-mode-map (kbd "K") 'backward-button)
    )
  (with-eval-after-load 'factor-mode
    (define-advice delete-indentation (:before (arg) factor-join-rescue-whitespace)
      (timor/factor-delete-indentation-before arg))
    (define-advice fixup-whitespace (:around (oldfun) factor-fixup-rescue-whitespace)
      (timor/factor-fixup-whitespace-around oldfun))))

(defun timor/post-init-outshine ()
  (with-eval-after-load 'outshine
    (spacemacs/set-leader-keys
      "aO." 'spacemacs/outshine-transient-state/body)))

(defun timor/post-init-notmuch ()
  (with-eval-after-load 'exwm
    (exwm-input-set-key (kbd "s-i") (lambda() (interactive) (notmuch-search "tag:flagged OR tag:unread"))))
  (with-eval-after-load 'message
    (add-hook 'message-setup-hook
              'timor/message-compose-maybe-sign))
  (when (not (functionp 'notmuch-show-insert-part-application/pkcs7-mime))
    (fset 'notmuch-show-insert-part-application/pkcs7-mime
          'timor/notmuch-show-insert-part-application/pkcs7-mime)
    (fset 'notmuch-show-insert-part-application/x-pkcs7-mime
          'timor/notmuch-show-insert-part-application/pkcs7-mime))
  (with-eval-after-load 'mm-decode
    (add-to-list 'mm-inline-media-tests '("application/pkcs7-signature" timor/mm-display-pkcs7-signature timor/mm-display-pkcs7-p))
    (add-to-list 'mm-inline-media-tests '("application/x-pkcs7-signature" timor/mm-display-pkcs7-signature timor/mm-display-pkcs7-p))
    ;; Needed to prevent notmuch from ignoring mm-handlers for application/*
    (add-to-list 'mm-inline-override-types "foo/bogus")))

(defun timor/post-init-auto-highlight-symbol ()
  (with-eval-after-load 'auto-highlight-symbol
    (setq ahs-include '((factor-mode . "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?<>'()-]+$")) )))
