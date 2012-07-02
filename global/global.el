

;; Make sure all backups (the ~ files) only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; copy/paste with C-c and C-v and C-x, check out C-RET too
;;(cua-mode)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; under mac, have Command as Meta and keep Option for localized input
(if is-mac
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Emacs (G)UI
(setq inhibit-startup-message t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))        ; No scroll bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))            ; No tool bar
(set-default 'cursor-type 'bar)                             ; Bar Cursor
(if is-mac
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; on to the visual settings
(setq inhibit-splash-screen t)		; no splash screen, thanks
(global-hl-line-mode 1)			; highlight current line
(line-number-mode 1)			; have line numbers and
(column-number-mode 1)			; column numbers in the mode line

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; Gotta see matching parens
(show-paren-mode t)

;; Emacs Terminal
;;(if (not window-system)
;;  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) )

(setq confirm-nonexistent-file-or-buffer nil)

;; Caption is pathname/temp buffer name
(setq frame-title-format
 '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

;; Don't wrap
(set-default 'truncate-lines t)

;; Ask for 'y' or 'n', not 'yes' or 'no
(fset 'yes-or-no-p 'y-or-n-p)

;; Save files in .emacs rather than aquamacs folder
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
(setq custom-file (expand-file-name "~/.emacs.d/global/customize.el"))

(cond
 (is-mac
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
 (is-windows
  (setq file-name-coding-system 'sjis)
  (setq locale-coding-system 'utf-8))
 (t
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)))

(add-hook 'kill-emacs-query-functions
          (lambda ()
            (if (file-newer-than-file-p (concat user-emacs-directory "init.el") (concat user-emacs-directory "init.elc"))
                (byte-compile-file (concat user-emacs-directory "init.el")))
            (byte-recompile-directory (concat user-emacs-directory "global") 0)
            (byte-recompile-directory (concat user-emacs-directory "vendor") 0)
            ))
 
;; 起動時間計測 目標は常に 3000ms 圏内(dump-emacs すれば可能だがしてない)
(when (or is-emacs23 is-emacs24)
  (defun message-startup-time ()
    (message "Emacs loaded in %dms"
             (/ (- (+ (third after-init-time) (* 1000000 (second after-init-time)))
                   (+ (third before-init-time) (* 1000000 (second before-init-time))))
                1000)))
  (add-hook 'after-init-hook 'message-startup-time))

;; Run a single instance of emacs, which accepts all 'open file' requests
(if window-system
  (server-start) )
;; Does not popup the annoying message when killing a client buffer
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)



