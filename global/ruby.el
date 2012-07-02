;; Replace $RSENSE_HOME with the directory where RSense was installed in full path
;; Example for UNIX-like systems
;; (setq rsense-home "/home/tomo/opt/rsense-0.2")
;; or
;; (setq rsense-home (expand-file-name "~/opt/rsense-0.2"))
;; Example for Windows
;; (setq rsense-home "C:\\rsense-0.2")

(setq rsense-home (expand-file-name "~/.emacs.d/ext/rsense"))
(setq rsense-rurema-home (expand-file-name "~/ext/ruby-rurema"))

(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

;; For some reason, this was really strange, the mode hook var was all jumbled.
;; (add-hook 'ruby-mode-hook (lambda ()
;;                             (add-to-list 'ac-sources 'ac-source-rsense-method)
;;                             (add-to-list 'ac-sources 'ac-source-rsense-constant)
;;                             (ruby-electric-mode nil)
;;                             (define-key ruby-mode-map (kbd "TAB") 'ac-complete)
;;                             (define-key ruby-mode-map (kbd "C-j") 'current-file-map)))

(setq ruby-mode-hook (lambda ()
                       (inf-ruby-keys)
                       ;; (ruby-electric-mode)
                       ;; (ruby-electric-mode nil)
                       (ac-ruby-mode-setup)
                       ;; (flymake-ruby-load)
                       (add-to-list 'ac-sources 'ac-source-rsense-method)
                       (add-to-list 'ac-sources 'ac-source-rsense-constant)
                       (define-key ruby-mode-map (kbd "TAB") 'ac-complete)
                       (define-key ruby-mode-map (kbd "C-j") 'current-file-map)))








