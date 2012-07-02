(defvar shell-toggle-shell-buffer nil
  "Buffer of the shell")

(defvar shell-toggle-goto-eob t
  "*If non-nil `shell-toggle' will move point to the end of the shell-buffer                              
whenever the `shell-toggle' switched to the shell-buffer.                                                 
                                                                                                          
When `shell-toggle-cd' is called the point is allways moved to the end of the                             
shell-buffer")

(defvar shell-toggle-automatic-cd t
  "*If non-nil `shell-toggle-cd' will send the \"cd\" command to the shell.                               
If nil `shell-toggle-cd' will only insert the \"cd\" command in the                                       
shell-buffer.  Leaving it to the user to press RET to send the command to                                 
the shell.")

(defvar shell-toggle-launch-shell 'shell-toggle-ansi-term
  "*The command to run to launch a shell.                                                                 
                                                                                                          
This must be a elisp function returning a buffer. (The newly created                                      
shell buffer)                                                                                             
                                                                                                          
Currently supported are 'shell and 'shell-toggle-ansi-term, and                                           
'shell-toggle-eshell")

;;;###autoload                                                                                            
(defvar shell-toggle-term-shell-to-launch nil
  "*Command invoked by the terminal emulation when shell-toggle use                                       
the ansi-term mode. nil means use the default shell.")

(defun shell-toggle-run-this-shell ()
  "Don't configure this variable. Use                                                                     
shell-toggle-term-shell-to-launch instead"
  (or shell-toggle-term-shell-to-launch
      (getenv "SHELL"))
  )

;;;###autoload                                                                                            
(defun shell-toggle-ansi-term ()
  (ansi-term (shell-toggle-run-this-shell)))

;;;###autoload                                                                                            
(defun shell-toggle-eshell ()
  (eshell))

;;;###autoload                                                                                            
(defvar shell-toggle-leave-buffer-hook nil
  "Hook ran before leaving the buffer to switch to the shell")

;;;###autoload                                                                                            
(defvar shell-toggle-goto-shell-hook nil
  "Hook ran after switching to the shell buffer")


;;; ======================================================================                                
;;; Commands:                                                                                             

;;;###autoload                                                                                            
(defun shell-toggle-this-is-the-shell-buffer () (interactive)
  (setq shell-toggle-shell-buffer (current-buffer)))

;;;###autoload                                                                                            
(defun shell-toggle-cd ()
  "Calls `shell-toggle' with a prefix argument.  See command `shell-toggle'"
  (interactive)
  (shell-toggle t))

;;;###autoload                                                                                            
(defun shell-toggle (make-cd)
  "Toggles between the shell buffer and whatever buffer you are editing.                                  
With a prefix ARG also insert a \"cd DIR\" command into the shell, where DIR is                           
the directory of the current buffer.                                                                      
                                                                                                          
Call twice in a row to get a full screen window for the shell buffer.                                     
                                                                                                          
When called in the shell buffer returns you to the buffer you were editing                                
before caling the first time.                                                                             
                                                                                                          
Options: `shell-toggle-goto-eob'"
  (interactive "P")

 ;; Try to decide on one of three possibilities:                                                         
  ;; If not in shell-buffer, switch to it.                                                                
  ;; If in shell-buffer and called twice in a row, delete other windows                                   
  ;; If in shell-buffer and not called twice in a row, return to state before                             
  ;;  going to the shell-buffer                                                                           
  (if (eq (current-buffer) shell-toggle-shell-buffer)
      (if (and (or (eq last-command 'shell-toggle)
                   (eq last-command 'shell-toggle-cd))
               (not (eq (count-windows) 1)))
          (delete-other-windows)
        (shell-toggle-buffer-return-from-shell))
    (shell-toggle-buffer-goto-shell make-cd)))

;;; ======================================================================                                
;;; Internal functions and declarations                                                                   

(defvar shell-toggle-pre-shell-win-conf nil
  "Contains the window configuration before the shell buffer was selected")



(defun shell-toggle-buffer-return-from-shell ()
  "Restores the window configuration used before switching the shell buffer.                              
If no configuration has been stored, just burry the shell buffer."
  (if (window-configuration-p shell-toggle-pre-shell-win-conf)
      (progn
        (set-window-configuration shell-toggle-pre-shell-win-conf)
        (setq shell-toggle-pre-shell-win-conf nil)
        (bury-buffer shell-toggle-shell-buffer))
    (bury-buffer))
  )

(defun shell-toggle-buffer-goto-shell (make-cd)
  "Switches other window to the shell buffer.  If no shell buffer exists                                  
start a new shell and switch to it in other window.  If argument MAKE-CD is                               
non-nil, insert a \"cd DIR\" command into the shell, where DIR is the directory                           
of the current buffer.                                                                                    
                                                                                                          
Stores the window configuration before creating and/or switching window."
  (setq shell-toggle-pre-shell-win-conf
        (current-window-configuration))
  (run-hooks 'shell-toggle-leave-buffer-hook)
  (let ((shell-buffer shell-toggle-shell-buffer)
        (cd-command
         ;; Find out which directory we are in (the method differs for                                    
         ;; different buffers)                                                                            
         (or (and make-cd
                  (buffer-file-name)
                  (file-name-directory (buffer-file-name))
                  (concat "cd " (file-name-directory (buffer-file-name))))
             (and make-cd
                  list-buffers-directory
                  (concat "cd " list-buffers-directory)))))

    ;; Switch to an existing shell if one exists, otherwise switch to another                             
    ;; window and start a new shell                                                                       
    (run-hooks 'shell-toggle-leave-buffer-hook)
    (if (buffer-live-p shell-buffer)
	(switch-to-buffer-other-window shell-buffer)
    (shell-toggle-buffer-switch-to-other-window)
      ;; Sometimes an error is generated when I call `shell'                                              
      ;; (it has to do with my shell-mode-hook which inserts text into the                                
      ;; newly created shell-buffer and thats not allways a good idea).                                   
      (condition-case the-error
          (setq shell-toggle-shell-buffer
                (funcall shell-toggle-launch-shell))
        (error (switch-to-buffer shell-toggle-shell-buffer))))
    (if (or cd-command shell-toggle-goto-eob)
        (goto-char (point-max)))
    (if (not (get-buffer-process (current-buffer)))
        (setq shell-toggle-shell-buffer
              (funcall shell-toggle-launch-shell)))
    (if cd-command
        (progn
          (if (eq shell-toggle-launch-shell 'shell)
              (progn
                (insert " ")
                (beginning-of-line)
                (kill-line)))
          (insert cd-command)
          (if shell-toggle-automatic-cd
              (cond ((eq shell-toggle-launch-shell 'shell)
                     (comint-send-input))
                    ((eq shell-toggle-launch-shell
                         'shell-toggle-ansi-term)
                     (term-send-input))
   ((eq shell-toggle-launch-shell
                         'shell-toggle-eshell)
                     (eshell-send-input))
                    (t (message "Shell type not recognized")))
            )
          ))
    (run-hooks 'shell-toggle-goto-shell-hook)
    ))

(defun shell-toggle-buffer-switch-to-other-window ()
  "Switches to other window.  If the current window is the only window in the                             
current frame, create a new window and switch to it.                                                      
                                                                                                          
\(This is less intrusive to the current window configuration than                                         
`switch-buffer-other-window')"
  (let ((this-window (selected-window)))
    (other-window 1)
    ;; If we did not switch window then we only have one window and need to                               
    ;; create a new one.                                                                                  
    (if (eq this-window (selected-window))
        (progn
          (split-window-vertically)
          (other-window 1)))))


(provide 'shell-toggle-patched)
