



(defun smex-prepare-ido-bindings ()
  (define-key ido-completion-map (kbd "C-? f") 'smex-describe-function)
  (define-key ido-completion-map (kbd "M-.") 'smex-find-function)
  (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line))
