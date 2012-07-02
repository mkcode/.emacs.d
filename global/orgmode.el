

;; Keybindings
(define-key org-mode-map "\C-c\C-h" 'org-mark-ring-goto)

;; Export Docbooks and PDFs
;; Use with (org-export-as-docbook-pdf[-and-open])
(setq path-to-docbook "/usr/local/Cellar/docbook/5.0/docbook/xsl/1.76.1/fo/docbook.xsl")
(setq org-export-docbook-xslt-proc-command (concat "xsltproc --output %s " path-to-docbook " %s"))
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
