(defun timor/user-config ()
  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/declare-prefix "ok" "keyboard layout")
  (spacemacs/set-leader-keys "okd" (lambda ()
				     (interactive)
				     (start-process-shell-command "xkb" nil "setxkbmap de")
				     (message "switched to de layout")))
  (spacemacs/set-leader-keys "oku" (lambda ()
				     (interactive)
				     (start-process-shell-command "xkb" nil "setxkbmap us")
				     (message "switched to us layout")))
  (spacemacs/set-leader-keys "br" 'rename-buffer)
  (evil-set-initial-state 'term-mode 'emacs))

;; TODO Buggy.  Sometimes switches to other states
(defun timor/evil-escape-dwim ()
  "Switch to previous state.  If the current state is the same as
  the previous state, switch to normal state instead.  If already
  in the normal state, stay in normal state."
  (interactive)
  (unless (eq evil-state 'normal)
    (if (eq evil-previous-state evil-state)
        (evil-change-state 'normal)
      (evil-change-to-previous-state))))

(defun timor/search-project-auto (&optional search-all)
  "Replacement for spacemacs/search-project-auto that does not
try to be too clever. If called with prefix-argument, also search
hidden files and follow links."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (counsel-auto-command (caar spacemacs--counsel-commands))
         (search-directory
          (or project-root
              (read-directory-name "Start from directory: ")))
         (projectile-search-cmd (intern (concat "counsel-projectile-" counsel-auto-command)))
         (directory-search-cmd (intern (concat "counsel-" (caar spacemacs--counsel-commands))))
         (extra-args (when search-all "--hidden --follow")))
    (if project-root
        (funcall projectile-search-cmd extra-args)
      (funcall directory-search-cmd nil search-directory extra-args))))

(defun timor/change-sexp (&optional arg)
  (interactive "P")
  (sp-kill-sexp arg)
  (evil-insert 1))

(defun timor/next-open-paren (&optional count)
  (interactive "p")
  (let ((pos (save-excursion
               (when (looking-at-p "[[({]")
                 (forward-char 1))
               (re-search-forward "[[({]" nil t count))))
    (when pos
      (goto-char pos)
      (forward-char -1))))

(defun timor/fuel-fix-sp-single-quote ()
  (sp--remove-local-pair "'"))

(defun timor/fuel-send-with-dup ()
  (interactive)
  (comint-send-string nil "dup ")
  (comint-send-input))

(defun timor/fuel-send-keep-inputs ()
  "Send input, wrapping it with preserving.  Requires combinators.smart vocabulary."
  (interactive)
  (comint-send-string nil "[ ")
  (comint-send-input)
  (comint-send-string nil " ] preserving\n")
  )

(defun timor/fuel-walk ()
  "Send input enclosed in call to walker. Needs UI running in
  thread to work correctly!"
  (interactive)
  (comint-send-string nil "[ ")
  (comint-send-input)
  (comint-send-string nil " ] walk \n"))

(defun timor/fuel-send-drop (&optional n)
  "Drop argument from stack, with n prefix, drop n arguments"
  (interactive "p")
  (comint-send-string nil (format "%d [ drop ] times \n" n)))

(defun timor/fuel-maybe-fixup-whitespace ()
  "Intended to be called when skipping out of a pair empty pair, but
  need to prevent [words] from turning into [words ]."
  (looking-back (rx (syntax open-parenthesis) (* (group whitespace)) (* any)))
  (if (match-string 1)
      (progn
        ;; (message "yay!")
        (cycle-spacing -1 nil 'single-shot))
    (progn
      ;; (message "woot!")
      (delete-horizontal-space))))

(defun timor//fuel-mode-sp-post-handler (id action context)
  ;; (message "Action: %s" action)
  (when (eq action 'insert)
    (insert "  ")
    (backward-char 1)))

(defun timor//factor-mode-colon-post-insert (id action context)
  (when (eq action 'insert))
  )

(defun timor//fuel-mode-sp-pre-handler (id action context)
  )

(defun timor/fuel-test-vocab-refresh ()
  "Like `fuel-test-vocab', but send a refresh-all to the listener
  before that, invalidating all removed definitions"
  (interactive)
  (comint-send-string (fuel-listener--process)
                      "refresh-all\n")
  (call-interactively 'fuel-test-vocab))

(defun timor/fuel-mode-insert-from-listener-input-ring ()
  (interactive)
  (unless fuel-listener--buffer
    (user-error "Cannot find fuel listener buffer"))
  (let* ((inputs
          (cl-delete-duplicates (mapcar 'substring-no-properties
                                     (ring-elements
                                      (buffer-local-value 'comint-input-ring fuel-listener--buffer)))
                             :test 'string-equal))
         (input (ivy-read "Insert recent listener input: " inputs :require-match t)))
    (insert input)))

(defun timor/factor-sp-wrap-square (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "[ "))

(defun timor/fuel-setup-lisp-state ()
  (define-key evil-lisp-state-local-map (kbd "w") (evil-lisp-state-enter-command timor/factor-sp-wrap-square)))

(defun timor/factor-delete-indentation-before (arg)
  "Intended for advising `delete-indentation' to always insert a
blank at the beginning of the next line before join, so that a
`fixup-whitespace' will be able to deal with that correctly."
  (when (eq major-mode 'factor-mode)
    (save-excursion
      (when arg (forward-line 1))
      (end-of-line)
      (insert ?\s))))

(defun timor/factor-fixup-whitespace-around (oldfun)
  "Intended for advising `fixup-whitespace' in factor mode to stop gobbling up whitespace before delimiters."
  (if (eq major-mode 'factor-mode)
      (let ((had-whitespace-closing (looking-at-p (rx (1+ space) (syntax close-parenthesis))))
            (had-whitespace-opening (save-excursion
                                      (forward-char -1)
                                      (looking-at-p (rx (syntax open-parenthesis) (1+ space))))))
        (when (or had-whitespace-closing had-whitespace-opening)
          (message "DEBUG: maybe rescue whitespace"))
        (funcall oldfun)
        (if (and had-whitespace-closing (save-excursion
                                          (forward-char -1)
                                          (looking-at-p (rx (not space) (syntax close-parenthesis)))))
            (progn
              (message "DEBUG: re-insert space")
              (insert ?\s))
          (when (and had-whitespace-opening (save-excursion
                                              (forward-char -1)
                                              (looking-at-p
                                               (rx (syntax open-parenthesis) (not space)))))
            (message "DEBUG: re-insert space")
            (insert ?\s))))
    (funcall oldfun)))


(defun timor//vterm-maybe-exit-copy-mode-in-exit-hook ()
  "Determine whether to resume vterm output.

Intended to be run when exiting normal state or visual state into
neither of these.
"
  (case evil-next-state
    ((normal visual))
    (t (vterm-copy-mode 0))))

(defun timor//vterm-enter-copy-mode ()
  (vterm-copy-mode 1))

(defun timor//vterm-setup-evil-hooks ()
  (add-hook 'evil-visual-state-entry-hook 'timor//vterm-enter-copy-mode nil t)
  (add-hook 'evil-normal-state-entry-hook 'timor//vterm-enter-copy-mode nil t)
  (add-hook 'evil-visual-state-exit-hook
            'timor//vterm-maybe-exit-copy-mode-in-exit-hook nil t)
  (add-hook 'evil-normal-state-exit-hook
            'timor//vterm-maybe-exit-copy-mode-in-exit-hook nil t))
