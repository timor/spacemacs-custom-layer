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

(defun timor/change-sexp (&optional arg)
  (interactive "P")
  (sp-kill-sexp arg)
  (evil-insert 1))
