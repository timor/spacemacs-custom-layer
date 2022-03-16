;; Source: https://emacs.stackexchange.com/questions/21651/when-split-window-sensibly-is-called-how-to-make-the-decision-depend-on-the-wid
(defun timor//sensible-window-split (&optional window)
  (let ((window (or window (selected-window))))
    (cond
     ((and (> (window-width window)
              (* 2 (window-height window)))
           (window-splittable-p window 'horizontal))
      (with-selected-window window
        (split-window-right)))
     ((window-splittable-p window)
      (with-selected-window window
        (split-window-below))))))

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
  (setq split-window-preferred-function #'timor//sensible-window-split)
  )

(defun timor//projectile-shell-run-function ()
  (intern (concat "projectile-run-" (symbol-name shell-default-shell))))

(defun timor/projectile-run-term-with-default-shell (&rest args)
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form (timor//projectile-shell-run-function)))))
  (apply (timor//projectile-shell-run-function) args))

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

(defun timor/message-compose-maybe-sign ()
  "Default smime signing."
  (when mml-secure-smime-sign-with-sender
    (mml-secure-message-sign-smime)))

(defun timor/notmuch-show-insert-part-application/pkcs7-mime (msg part content-type
							                                              nth depth button)
  (let* ((encstatus-plist (car (plist-get part :encstatus)))
         (encstatus (plist-get encstatus-plist :status)))
    (notmuch-crypto-insert-encstatus-button encstatus-plist)
    (if (not (string= encstatus "bad"))
        (notmuch-show-insert-part-multipart/signed msg
                                                   (car (plist-get part
								                                                   :content))
                                                   content-type
                                                   nth
                                                   depth
                                                   button))))
(defun timor/mm-display-pkcs7-p (handle)
  (executable-find "openssl"))

(defun timor/mm-display-pkcs7-signature (handle)
  (mm-insert-inline
   handle
   (mm-with-unibyte-buffer
     (insert (mm-get-part handle))
     (call-process-region (point-min) (point-max) "openssl" t t nil
                          "pkcs7" "-inform" "der" "-noout" "-print_certs")
     (buffer-string))))

(defun counsel-osm--parse-reply ()
  (mapcar (lambda (x)
            `(,(format "%s (%s° %s°)"
                       (alist-get 'display_name x)
                       (alist-get 'lat x)
                       (alist-get 'lon x))
              ,(string-to-number (alist-get 'lat x))
              ,(string-to-number (alist-get 'lon x))
              ,@(mapcar #'string-to-number (alist-get 'boundingbox x))))
          (let ((json-object-type 'alist)
                (json-array-type 'list))
            (json-read))))

(defvar counsel-osm--history ())

(defvar counsel-osm--request-timer nil)
(defun counsel-osm-function-1 (input)
  (or
   (ivy-more-chars)
   (let* ((view-box (plist-get (ivy-state-extra-props ivy-last)
                               :counsel-osm--search-box))
          (query-params `(("format" . "json")
                          ("q" . ,input)))
          (params (if view-box
                      (append query-params
                              `(("bounded" . "1")
                                ("viewbox" . ,(format "%s,%s,%s,%s"
                                                      (first view-box)
                                                      (second view-box)
                                                      (third view-box)
                                                      (fourth view-box)))))
                    query-params)))
     (request
       "https://nominatim.openstreetmap.org/search"
       :type "GET"
       :params params
       :parser #'counsel-osm--parse-reply
       :error (cl-function
               (lambda (&key error-thrown &allow-other-keys)
                 (message "Counsel-OSM-Request error: %s" error-thrown)))
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (ivy-update-candidates
                    (mapcar (lambda(x)
                              (propertize (car x) 'osm-goto-args (cdr x)))
                            data))))
       :timeout 30))))

(defun counsel-osm-function (input)
  (when counsel-osm--request-timer
    (cancel-timer counsel-osm--request-timer))
  (setq counsel-osm--request-timer
        (run-with-timer 1 nil #'counsel-osm-function-1 input))
  0)

(defun counsel-osm-action (selected)
  (let ((x (get-text-property 0 'osm-goto-args selected)))
    (osm-goto (car x) (cadr x)
              (apply #'osm--boundingbox-to-zoom (cddr x)))))

(defun counsel-osm ()
  (interactive)
  (ivy-read "Search OSM: " #'counsel-osm-function
            :action #'counsel-osm-action
            :dynamic-collection t
            :require-match t
            :history 'counsel-osm--history
            :caller 'counsel-osm
            ))

(defun osm--viewbox ()
  (let* ((cx osm--x)
         (cy osm--y)
         (z osm--zoom)
         (x1 (- cx osm--wx))
         (y1 (- cy osm--wy))
         (x2 (+ cx osm--wx))
         (y2 (+ cy osm--wy)))
    (list
     (osm--x-to-lon x1 z)
     (osm--y-to-lat y1 z)
     (osm--x-to-lon x2 z)
     (osm--y-to-lat y2 z))))

(defun counsel-osm--view-update ()
  (with-ivy-window
    (let* ((text (ivy-state-current ivy-last))
           (data (get-text-property 0 'osm-goto-args text))
           (x (osm--lon-to-x (second data) osm--zoom))
           (y (osm--lat-to-y (first data) osm--zoom))
           )
      (osm--put-transient-pin 'osm-center x y text)
      (osm--update)
      )))

(defun counsel-osm-view ()
  (interactive)
  (ivy-read "Search Current Map View: " #'counsel-osm-function
            :action #'counsel-osm-action
            :dynamic-collection t
            :require-match t
            :history 'counsel-osm--history
            :caller 'counsel-osm
            :update-fn #'counsel-osm--view-update
            :extra-props `(:counsel-osm--search-box ,(osm--viewbox))))
