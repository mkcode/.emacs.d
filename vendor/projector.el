;;; projector.el --- Manage and navigate projects in Emacs easily

;; Copyright (C) 2011
;; Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Projector
;; Git: git://github.com/bbatsov/projector.git
;; Version: 0.4
;; Created: 2011-31-07
;; Keywords: project, convenience
;; EmacsWiki: Projector

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides easy project management and navigation. The
;; concept of a project is pretty basic - just a folder containing
;; special file. Currently git, mercurial and bazaar repos are
;; considered projects by default. If you want to mark a folder
;; manually as a project just create an empty .projector file in
;; it. Some of projector's features:
;;
;; * jump to a file in project
;; * jump to a project buffer
;; * multi-occur in project buffers
;; * grep in project
;; * regenerate project etags
;;; Installation:
;;
;; (require 'projector)
;; (projector-global-mode) ;; to enable in all buffers
;;
;; To enable projector only in select modes:
;;
;; (add-hook 'ruby-mode-hook #'(lambda () (projector-mode)))
;;
;;; Usage:
;;
;; Here's a list of the interactive Emacs Lisp functions, provided by projector:
;;
;; * projector-jump-to-project-file (C-c p j)
;; * projector-grep-in-project (C-c p f)
;; * projector-replace-in-project (C-c p r)
;; * projector-switch-to-buffer (C-c p b)
;; * projector-multi-occur (C-c p o)
;; * projector-regenerate-tags (C-c p t)
;; * projector-invalidate-project-cache (C-c p i)
;;
;;; Code:

;; requires
(require 'cl)
(require 'easymenu)
(require 'thingatpt)

;; variables
(defvar projector-project-root-files '(".git" ".svn" ".hg" ".bzr" ".projector")
  "A list of files considered to mark the root of a project")

(defvar projector-ignored-file-extenstions '("class" "o" "so" "elc")
  "A list of file extensions ignored by projector.")

(defvar projector-projects-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project file names to speed up related operations")

;; The following is adapted from etags-select.el

(defvar projector-language-file-suffix-alist
  '(( "asp"        . ( "*.asp" "*.asa" ))
    ( "awk"        . ( "*.awk" "*.gawk" "*.mawk"))
    ( "c"          . ( "*.c" "*.h" ))
    ( "c++"        . ( "*.c++" "*.cc" "*.cp" "*.cpp"  "*.cxx" "*.h" "*.h++" "*.hh" "*.hp" "*.hpp" "*.hxx" "*.c"  "*.C" "*.h" "*.H"))
    ( "c#"         . ( "*.cs" ))
    ( "java"       . ( "*.java " ))
    ( "lisp"       . (  "*.cl" "*.clisp" "*.el" "*.l" "*.lisp" "*.lsp" "*.ml"))
    ( "python"     . ( "*.py" "*.python" ))
    ( "Ruby"       . ( "*.rb" "*.ru" ".rake" "" ))
    ( "javascript" . ( "*.js" ))
    ( "SQL"        . (  "*.sql" ))
    ( "Tcl"        . ( "*.tcl" "*.tk" "*.wish" "*.itcl" )))
  "Association list defining file masks for languages")

(defvar ectags-command "xctags"
  "Name of the exuberant ctags executable on your system")

(defun make-suffix-clauses (languages)
  (mapcar (lambda (l)
            (mapcar (lambda (s)
                      (concat  " -iname \"" s "\""))
                    (cdr (assoc-string l projector-language-file-suffix-alist))))
          (split-string languages)))

(defun make-shell-command-prefix (directory)
  (concat "find " (expand-file-name directory)))

(defun make-tag-file-name (directory)
  (expand-file-name (concat directory (string directory-sep-char) "TAGS")))

(defun ectag-directory-command (directory languages)
  "Produce a command needed to scan the given directory for files
   of the given language and produce tags"
  (let*
      ((suffix-clauses
        (car (make-suffix-clauses languages)))
       (shell-command-prefix
        (make-shell-command-prefix directory))
       (shell-command-suffix
        (concat " | " ectags-command " -e -o "  (make-tag-file-name directory) " --verbose --excmd=n --extra=+fq --fields=+afiKlmnsSzt --file-scope=no -L -")))
    (concat shell-command-prefix
            (car suffix-clauses)
            (apply 'concat
                   (mapcar (lambda (s)
                             (concat " -o" s))
                           (cdr suffix-clauses)))
            shell-command-suffix)))

(defun projector-tag-directory (&optional lang)
  "Prompt for a directory and a langage and create a tag file."
  (interactive)
  ;; prompt for directory
  (if (not (stringp lang)) (setq lang "lisp c c++"))
  (let ((tag-directory (projector-get-project-root))
        (tag-languages (completing-read "Languages to tag? "
                                        projector-language-file-suffix-alist nil nil)))
    (add-to-list 'compilation-error-regexp-alist
                 '("^\\([^:]+\\) confusing argument declarations beginning at line \\([0-9]+\\))" 1 2))
    (call-process-shell-command (ectag-directory-command tag-directory tag-languages)
                                nil "*Create Tags Output*" nil ))
  (kill-buffer "TAGS"))

(defun projector-open-root ()
  "Go to the project root"
  (interactive)
  (find-file (projector-get-project-root))
)

(defun projector-tag-directory-all-rails ()
  "Prompt for a directory and a langage and create a tag file."
  (interactive)
  (let ((ptar-commands (concat (format "cd %s \n" (projector-get-project-root))
                               "bundle package \n"
                               "gem unpack vendor/cache/*.gem --target=vendor/cache/GEMS")))
    (call-process-shell-command ptar-commands nil "*Create Tags Output*" nil)
    (projector-tag-directory "Ruby")))

;;;; END part adpated from etags-select.el

(defun projector-invalidate-project-cache ()
  "Removes the current project's files from `projector-projects-cache'"
  (interactive)
  (let ((project-root (projector-get-project-root)))
    (remhash project-root projector-projects-cache)
    (message "Invalidated Projector cache for %s" project-root)))

(defun projector-get-project-root ()
  (loop for file in projector-project-root-files
        when (locate-dominating-file default-directory file)
        do (return it)))

(defun projector-get-project-files (directory)
  "List the files in DIRECTORY and in its sub-directories."
  ;; check for a cache hit first
  (let ((files-list (gethash directory projector-projects-cache)))
    ;; cache miss
    (unless files-list
      ;; while we are in the current directory
      (dolist (current-file (directory-files directory t) files-list)
        (cond
         ((and (file-directory-p current-file)
               (string= (expand-file-name current-file) current-file)
               (not (projector-ignored-p current-file)))
          (setq files-list (append files-list (projector-get-project-files current-file))))
         ((and (string= (expand-file-name current-file) current-file)
               (not (file-directory-p current-file))
               (not (projector-ignored-extension-p current-file)))
          (setq files-list (cons current-file files-list)))))
      ;; we cache the resulting list of files
      (when (string= directory (projector-get-project-root))
        (puthash directory files-list projector-projects-cache)))
    files-list))

(defun projector-get-project-buffers ()
  (let ((project-files (projector-get-project-files (projector-get-project-root)))
        (buffer-files (mapcar 'buffer-file-name (buffer-list))))
    (mapcar 'get-file-buffer (intersection project-files buffer-files :test 'string=))))

(defun projector-get-project-buffer-names ()
  (mapcar 'buffer-name (projector-get-project-buffers)))

(defun projector-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (ido-completing-read "Jump to project buffer: " (projector-get-project-buffer-names))))

(defun projector-multi-occur ()
  (interactive)
  (multi-occur (projector-get-project-buffers)
               (car (occur-read-primary-args))))

(defun projector-hashify-files (files-list)
  (let ((files-table (make-hash-table :test 'equal))
        (files-to-uniquify nil))
    (dolist (current-file files-list files-table)
      (let ((basename (file-name-nondirectory current-file)))
        (if (gethash basename files-table)
            (progn
              (puthash (uniquify-file current-file) current-file files-table)
              (when basename (push basename files-to-uniquify)))
          (puthash basename current-file files-table))))
    ;; uniquify remaining files
    (dolist (current-file (remove-duplicates files-to-uniquify :test 'string=))
      (puthash (uniquify-file (gethash current-file files-table)) (gethash current-file files-table) files-table)
      (remhash current-file files-table))
    files-table))

(defun uniquify-file (filename)
  (let ((filename-parts (reverse (split-string filename "/"))))
    (format "%s/%s" (second filename-parts) (first filename-parts))))

(defun projector-ignored-p (file)
  (loop for ignored in projector-project-root-files
        when (string= (expand-file-name (concat (projector-get-project-root) ignored)) file)
        do (return t)
        finally (return nil)))

(defun projector-ignored-extension-p (file)
  (let ((ext (file-name-extension file)))
    (member ext projector-ignored-file-extenstions)))

(defun projector-jump-to-project-file ()
  (interactive)
  (let* ((project-files (projector-hashify-files
                         (projector-get-project-files (projector-get-project-root))))
         (file (ido-completing-read "Jump to project file: "
                                    (loop for k being the hash-keys in project-files collect k))))
    (find-file (gethash file project-files))))

(defun projector-grep-in-project ()
  (interactive)
  (let ((search-regexp (if (and transient-mark-mode mark-active)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search for: " (thing-at-point 'symbol))))
        (root-dir (projector-get-project-root)))
    (grep-compute-defaults)
    (rgrep search-regexp "* .*" root-dir)))

(defalias 'projector-regenerate-tags 'projector-tag-directory)
(defun projector-regenerate-tags-old ()
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projector-get-project-root)))
    (cd project-root)
    (shell-command (format "xctags -Re %s --excmd=n" project-root))
    (cd current-dir)
    (visit-tags-table project-root)))

(defun projector-replace-in-project ()
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projector-get-project-root))
        (old-text (read-string "Replace: " (thing-at-point 'symbol)))
        (new-text (read-string "With: ")))
    (shell-command (format "find %s -type d -name .git -prune -o -print| xargs perl -p -i -e 's/%s/%s/g'" project-root old-text new-text))))

(defvar projector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p j") 'projector-jump-to-project-file)
    (define-key map (kbd "C-c p f") 'projector-grep-in-project)
    (define-key map (kbd "C-c p b") 'projector-switch-to-buffer)
    (define-key map (kbd "C-c p o") 'projector-multi-occur)
    (define-key map (kbd "C-c p r") 'projector-replace-in-project)
    (define-key map (kbd "C-c p i") 'projector-invalidate-project-cache)
    (define-key map (kbd "C-c p t") 'projector-regenerate-tags)
    (define-key map (kbd "C-c p S-t") 'projector-tag-directory-all-rails)
    map)
  "Keymap for Projector mode.")

(easy-menu-define projector-mode-menu projector-mode-map
  "Menu for Projector mode"
  '("Projector"
    ("Navigating"
     ["Jump to file" projector-jump-to-project-file]
     ["Jump to buffer" projector-switch-to-buffer])

    ("Find & Replace"
     ["Find in project" projector-grep-in-project]
     ["Replace in project" projector-replace-in-project]
     ["Multi-occur in project" projector-multi-occur])

    ("General"
     ["Invalidate cache" projector-invalidate-project-cache]
     ["Regenerate etags" projector-regenerate-tags])))

;; Helm integration
;; (defun helm-c-projector-list ()
;;   "Generates a list of files in the current project"
;;   (projector-get-project-files
;;    (projector-get-project-root)))

;; (defvar helm-c-projector-cache nil)

;; (defvar helm-c-source-projector
;;   `((name . "Projector")
;;     (init . (lambda ()
;;               (setq helm-c-projector-cache
;;                     (helm-c-projector-list))))
;;     ;; Needed for filenames with capitals letters.
;;     (disable-shortcuts)
;;     (candidates . helm-c-projector-cache)
;;     (volatile)
;;     (keymap . ,helm-generic-files-map)
;;     (help-message . helm-generic-file-help-message)
;;     (mode-line . helm-generic-file-mode-line-string)
;;     (match helm-c-match-on-basename)
;;     (type . file))
;;   "Helm source definition")

;; (defun helm-projector ()
;;   "Example function for calling Helm with the projector file source.

;; Use this function as example and create your own list of Helm sources.
;; "
;;   (interactive)
;;   (helm-other-buffer 'helm-c-source-projector "*helm projector*"))

;; define minor mode
(define-globalized-minor-mode projector-global-mode projector-mode projector-on)

(defun projector-on ()
  (when (projector-get-project-root)
    (projector-mode 1)))

(defun projector-off ()
  (easy-menu-remove))

(define-minor-mode projector-mode "Minor mode to assist project management and navigation."
  :lighter " Projector"
  :keymap projector-mode-map
  (if projector-mode
      ;; on start
      (easy-menu-add projector-mode-menu projector-mode-map)
    ;; on stop
    (projector-off)))

(provide 'projector)
;;; projector.el ends here
