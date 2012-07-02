

(require 'auto-complete-etags)

(setq-default ac-sources '(ac-source-filename
		   ac-source-yasnippet
		   ac-source-abbrev
		   ac-source-dictionary
		   ac-source-words-in-same-mode-buffers))


;; Start it only when tab is pressed
;;(setq ac-auto-start nil)
;;(ac-set-trigger-key "C-i")

(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(define-key global-map (kbd "TAB") 'ac-complete)

(setq ac-show-menu-immediately-on-auto-complete t)
;;(setq ac-auto-show-menu nil)
(setq ac-auto-show-menu t)

(setq ac-use-menu-map t)

;;(define-key ac-menu-map (kbd "TAB") 'auto-complete)
;;(define-key ac-menu-map (kbd "RET") 'auto-complete)

(setq ac-stop-flymake-on-completing t)
(setq ac-expand-on-auto-complete t)

(add-to-list 'ac-modes 'haml-mode)
(add-to-list 'ac-modes 'scss-mode)

(setq ac-use-comphist t)
(setq ac-comphist-file "~/.emacs.d/expansion/auto-complete-history.dat")

(define-key ac-menu-map (kbd "C-j") (lambda () (interactive)(auto-complete '(ac-source-etags))))



