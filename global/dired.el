
;; Used with dired single which is gone now for dired+
;; (defun my-dired-init ()
;;   "Bunch of stuff to run for dired, either immediately or when it's
;;    loaded."
;;   ;; <add other stuff here>
;;   (define-key dired-mode-map [return] 'joc-dired-single-buffer)
;;   (define-key dired-mode-map [f] 'joc-dired-single-buffer)
;;   (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
;;   (define-key dired-mode-map "^"
;;     (function
;;      (lambda nil (interactive) (joc-dired-single-buffer ".."))))
;;   (define-key dired-mode-map "-"
;;     (function
;;      (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;; ;; if dired's already loaded, then the keymap will be bound
;; (if (boundp 'dired-mode-map)
;;     ;; we're good to go; just add our bindings
;;     (my-dired-init)
;;   ;; it's not loaded yet, so add our bindings to the load-hook
;;   (add-hook 'dired-load-hook 'my-dired-init))


;; remap 'o' in dired mode to open a file
(defun dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))
(define-key dired-mode-map "O" 'dired-open-mac)

(defun find-grep-dired-do-search (dir regexp)
  "First perform `find-grep-dired', and wait for it to finish.
    Then, using the same REGEXP as provided to `find-grep-dired',
    perform `dired-do-search' on all files in the *Find* buffer."
  (interactive "DFind-grep (directory): \nsFind-grep (grep regexp): ")
  (find-grep-dired dir regexp)
  (while (get-buffer-process (get-buffer "*Find*"))
    (sit-for 1))
  (with-current-buffer "*Find*"
    (dired-toggle-marks)
    (dired-do-search regexp)))
(defalias 'dired-grep-awesome 'find-grep-dired-do-search)

;; Failed attempt
;; Overwriting function from dired.el to have pretty face on current line
;; Move to first char of filename on this line.
;; Returns position (point) or nil if no filename on this line."
;; (defun dired-move-to-filename (&optional raise-error eol)
;;   "Move to the beginning of the filename on the current line.
;; Return the position of the beginning of the filename, or nil if none found."
;;   ;; This is the UNIX version.
;;   (or eol (setq eol (line-end-position)))
;;   (beginning-of-line)
;;   ;; First try assuming `ls --dired' was used.
;;   (let ((change (next-single-property-change (point) 'dired-filename nil eol)))
;;     (cond
;;      ((and change (< change eol))
;;       (goto-char change))
;;      ((re-search-forward directory-listing-before-filename-regexp eol t)
;;       (goto-char (match-end 0)))
;;      ((re-search-forward dired-permission-flags-regexp eol t)
;;       ;; Ha!  There *is* a file.  Our regexp-from-hell just failed to find it.
;;       (if raise-error
;;           (error "Unrecognized line!  Check directory-listing-before-filename-regexp"))
;;       (beginning-of-line)
;;       nil)
;;      (raise-error
;;       (error "No file on this line"))))
;;   (add-text-properties (line-beginning-position) (line-end-position)
;;                        '(mouse-face highlight
;;                                     help-echo "mouse-2: visit this file in other window"))
;;   (put-text-property (line-beginning-position) (line-end-position)
;;                      'font-lock-face 'italics))




;; Allows recursive deletes
(setq dired-recursive-deletes 'top)

;; File sizes are for humans
(setq dired-listing-switches "-lah")
(setq dired-auto-revert-buffer t)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;;(add-hook 'dired-load-hook (lambda () (toggle-diredp-find-file-reuse-dir 1)))
(toggle-diredp-find-file-reuse-dir 1)
;;(add-hook 'dired-mode-hook (lambda () (toggle-diredp-find-file-reuse-dir 1)))
