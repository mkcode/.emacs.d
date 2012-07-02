
;;  No brainers. Consistant with terminal
(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key global-map (kbd "C-?") 'help-command)
(define-key global-map "\C-w" 'backward-kill-word)
(global-set-key "\C-t" 'kill-region) ;; think Take rather than Wipe
(define-key global-map "\C-y" 'popup-kill-ring) ;; awesome kill ring recall

;; http://stackoverflow.com/questions/2298811/how-to-turn-off-alternative-enter-with-ctrlm-in-linux
;;(global-unset-key (kbd "<return>")) ;; unbind RET, I like it diferent from the return key
;;(global-set-key (kbd "<return>") 'newline) ;; unbind RET, I like it diferent from the return key
;;(define-key global-map (kbd "C-m") 'skip-to-next-line)
;;(global-unset-key "\C-i") ;; unbind TAB, I like it diferent from the tab key

(define-key global-map (kbd "C-S-]") 'shift-right)
(define-key global-map (kbd "C-S-[") 'shift-left)

(if is-aquamacs
    (define-key global-map (kbd "C-S-p") (lambda () (interactive)(aquamacs-previous-line 10))))
(if is-aquamacs
    (define-key global-map (kbd "C-S-n") (lambda () (interactive)(aquamacs-next-line 10))))

;; Eval related
(define-prefix-command 'eval-commands-map)
(define-key global-map (kbd "C-x C-e") 'eval-commands-map)
(define-key eval-commands-map (kbd "e") 'eval-region)
(define-key eval-commands-map (kbd "C-e") 'eval-region)
(define-key eval-commands-map (kbd "l") 'eval-last-sexp)
(define-key eval-commands-map (kbd "p") 'pp-eval-last-sexp)
(define-key eval-commands-map (kbd "r") 'eval-and-replace)

;; Super Cool Mode
(define-prefix-command 'current-file-map)
(define-key global-map (kbd "C-j") 'current-file-map)
(define-key current-file-map (kbd "s") 'imenu)
(define-key current-file-map (kbd "a") (lambda () (interactive)(auto-complete '(ac-source-etags))))
(define-key current-file-map (kbd "C-s") 'imenu)
(define-key current-file-map (kbd "C-a") (lambda () (interactive)(auto-complete '(ac-source-etags))))
(define-key current-file-map (kbd "c") 'comment-or-uncomment-region-or-line)
(define-key current-file-map (kbd "C-c") 'comment-or-uncomment-region-or-line)
(define-key current-file-map (kbd "d") 'duplicate-line)
(define-key current-file-map (kbd "C-d") 'duplicate-line)
(define-key current-file-map (kbd "l") 'insert-arrow)
(define-key current-file-map (kbd "C-l") 'insert-arrow)


;; can't miss M-x
;; Currently using smex, can change back to 'ido-run
(global-set-key "\C-xm" 'smex)
(global-set-key "\C-cm" 'smex)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

;; prefer dired over dumping dir list to buffer
;; Dired single provides this mapped to cx cd
(define-key global-map "\C-x\C-d" 'dired)
(define-key dired-mode-map "-" 'dired-up-directory)
o
(define-key global-map "\C-xn" 'shell-toggle)
(add-hook 'term-mode-hook (lambda () (local-set-key "\C-xn" 'shell-toggle)))

(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-x C-g") 'magit-status)

;; Projectile related
(define-key projector-mode-map (kbd "C-c p j") 'ido-find-file-in-tag-files)
(define-key projector-mode-map (kbd "C-c p c") 'find-tag)
(define-key projector-mode-map (kbd "C-c p C-c") 'find-tag)
(define-key projector-mode-map (kbd "C-c p o") 'open-project)
(define-key projector-mode-map (kbd "C-c p h") 'projector-open-root)

;; Make open project work at startup
(define-prefix-command 'p-map)
(define-key global-map (kbd "C-c p") 'p-map)
(define-key p-map (kbd "o") 'open-project)



;; Ido related
(define-key ido-file-completion-map "\C-n" 'ido-next-work-file)
(define-key ido-file-completion-map "\C-p" 'ido-prev-work-file)

(add-hook 'ido-setup-hook (lambda ()
                            (define-key ido-completion-map "\C-n" 'previous-next-element)
                            (define-key ido-completion-map "\C-p" 'previous-history-element)))
;; (define-key ido-completion-map (kbd "<return>") 'ido-exit-minibuffer)))

;; copy mac style for mousing
(if is-aquamacs
    (define-key osx-key-mode-map "\M-c" 'cua-copy-region))

;; buffers
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; auto-complete trigger
;;(define-key global-map (kbd "C-i") 'auto-complete)
;;(define-key ruby-mode-map (kbd "C-i") 'auto-complete)

;; I stopped using sr-speedbar... C-x C-j does everything I really need
;; sr-speedbar related
;;(global-set-key (kbd "C-x j") 'sr-speedbar-select-window)
;;(global-set-key (kbd "C-x J") 'sr-speedbar-toggle)
;;(define-key speedbar-file-key-map (kbd "r") 'sr-speedbar-refresh-toggle)
;;(define-key speedbar-file-key-map (kbd "-") 'speedbar-up-directory)

;; indent whole buffer
(define-key global-map (kbd "<C-tab>") 'iwb)