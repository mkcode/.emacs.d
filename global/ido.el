;; Don't open Completion buffer unless explicitly asked for with '?'
;; Prefer completing in minibufer
(setq completion-auto-help nil)
(icomplete-mode 0)


(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq resize-mini-windows 'grow-only)
(setq max-mini-window-height 0.5)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'never)
(setq ido-enable-tramp-completion t)
(setq ido-auto-merge-delay-time 20) ;; don't really like this feature
(setq ido-max-prospects 30)
(setq ido-enable-last-directory-history t)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired nil) ;; put . as the first item
(setq ido-use-filename-at-point nil)

;;(setq ido-completion-buffer nil) ;; don't use the full buffer for completions

;; Display ido results vertically, rather than horizontally

(setq ido-decorations (quote ("\n-------------------------------------------------\n  ==>  " "" "\n       " "\n     ..." "[" "] >>" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(defun ido-run ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "Run:  "
     (all-completions "" obarray 'commandp)))))

(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))
