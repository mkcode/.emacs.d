
;;   "*Map filename to tag file(s)

;; Example:

;; (setq etags-table-alist
;;       (list
;;        \'(\"/home/me/Projects/foo/.*\\\\.[ch]$\" \"/home/me/Projects/lib1/TAGS\" \"/home/me/Projects/lib2/TAGS\")
;;        \'(\"/home/me/Projects/bar/.*\\\\.py$\" \"/home/me/Projects/python/common/TAGS\")
;;        \'(\".*\\\\.[ch]$\" \"/usr/local/include/TAGS\")
;;        ))

;; A file named, for example, \"/home/me/Projects/foo/main.c\" would set the
;; `tags-table-list' to a list of:

;; \"/home/me/Projects/lib1/TAGS\"
;; \"/home/me/Projects/lib2/TAGS\"
;; \"/usr/local/include/TAGS\"

;; and possibly a local tags file at the head of the list if `etags-table-search-up-depth'
;; is non-nil.  You can use \\&, \\1, etc. in the tag file names to substitute pieces
;; captured with \\(\\) in the key.

(require 'etags-table)

(setq etags-table-alist
      (list
       '("~/.emacs.d/expansion/tags/.*")
       ))

(setq etags-table-search-up-depth 7)
