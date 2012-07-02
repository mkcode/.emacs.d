;;
;; Emacs init.el File
;; Heavily based upon el-get
;; Chris Ewald
;;

;;(require 'cl)	       ;; common lisp goodies, loop
(require 'shell)     ;; for some reason, this needs to be required

;; What system is being used?
(setq is-*nix (if (string-match "linux" (symbol-name system-type)) t))
(setq is-windows (if (string-match "windows" (symbol-name system-type)) t))
(setq is-mac (if (string-match "darwin" (symbol-name system-type)) t))
(setq is-aquamacs (if (boundp 'aquamacs-version) t))
(setq is-emacs22 (equal emacs-major-version 22))
(setq is-emacs23 (equal emacs-major-version 23))
(setq is-emacs24 (equal emacs-major-version 24))

;; el-get will automatically load files in the format init-PCKGNAME.el
;; in the user-package-dir
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-user-package-directory "~/.emacs.d/el-get-inits")
(el-get 'sync)

;; el-get doesn't have everything
;; get vendored elisp outside of el-get
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'rcodetools)
(require 'eyedropper)
(require 'projector)
(require 'completing-help)
(require 'shell-toggle-patched)
;;(require 'sr-speedbar)
(require 'javascript-mode) ;; bundled with aquamacs and required for haml-mode

;;(add-to-list 'load-path "~/.emacs.d/vendor/helm")
;;(require 'helm-config)
;;(helm-mode 1)

;; Global Configs
(add-to-list 'load-path "~/.emacs.d/global")
(load "env")
(load "defuns")
(load "global")
(load "ido")
(load "dired")
(load "javascript")
(load "shell")
(load "orgmode")
(load "uniquify")
(load "tabs")
(load "tags")
(load "auto-complete")
;;(load "sr-speedbar") ;; Don't really like this anymore
(load "ruby")
(load "projector")
(load "help")
(load "appearance")
(load "key-bindings")
(if is-aquamacs
  (load "aquamacs-custom"))

