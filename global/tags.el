;; Should be using etags instead of ctags



;; Create tags function for creating TAGS file
(setq path-to-ctags "/opt/local/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name dir-name))
  )


(setq tags-file-name "TAGS")


;; Consider:  https://github.com/mattkeller/etags-update


