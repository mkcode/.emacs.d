




(setq color-theme-is-global t)
(setq color-theme-is-cumulative t)
(setq color-theme-load-all-themes nil)
;;(setq color-theme-install-hook nil) ;; Get the stupid aquamacs autofaces out of here
(require 'color-theme-railscasts)

;; Aquamacs really likes to fuck with color-theme stuff. By using window setup hook,
;; I ensure that the themes are loaded as if by M-x
(add-hook 'window-setup-hook (lambda ()
                               (color-theme-twilight)
                               (color-theme-arjen)
                               (color-theme-railscasts)
                               (set-face-foreground 'ido-subdir "#FFC66D")
                               (set-face-foreground 'ido-only-match "yellow")
                               (set-face-bold-p 'ido-first-match t)
                               (set-face-underline 'ido-first-match nil)
                               ;; (set-face-attribute 'ido-first-match nil :height 120)
                               (set-face-background 'hl-line "#232923")
                               ;; (set-face-attribute 'minibuffer-prompt nil :height 120)
                               (set-face-attribute 'minibuffer nil :font "monaco")
                               (set-face-attribute 'minibuffer nil :height 160)
                               (set-face-attribute 'echo-area nil :font "monaco")
                               (set-face-attribute 'echo-area nil :height 120)
                               (set-face-background 'cursor "#ff3e96")
                               ))

(blink-cursor-mode nil)
(setq cursor-type '(bar . 2))

(fringe-mode 'right-only)

(if is-aquamacs
    (tabbar-mode -1))

