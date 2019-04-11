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
  (evil-set-initial-state 'term-mode 'emacs)
  )

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

(defun timor//fuel-mode-sp-post-handler (id action context)
  (when (eq action 'insert)
    (insert "  ")
    (backward-char 1)))

(defun timor/fuel-mode-insert-from-listener-input-ring ()
  (interactive)
  (unless fuel-listener--buffer
    (user-error "Cannot find fuel listener buffer"))
  (let* ((inputs
          (delete-duplicates (mapcar 'substring-no-properties
                                     (ring-elements
                                      (buffer-local-value 'comint-input-ring fuel-listener--buffer)))
                             :test 'string-equal))
         (input (ivy-read "Insert recent listener input: " inputs :require-match t)))
    (insert input)))
