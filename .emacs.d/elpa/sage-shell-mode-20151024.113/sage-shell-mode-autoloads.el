;;; sage-shell-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "sage-shell-mode" "sage-shell-mode.el" (22180
;;;;;;  24597 443378 154000))
;;; Generated autoloads from sage-shell-mode.el

(defvaralias 'sage-shell:command 'sage-shell:sage-executable)

(defvaralias 'sage-shell:add-to-texinputs-p 'sage-shell-sagetex:add-to-texinputs-p)

(autoload 'sage-shell:run-sage "sage-shell-mode" "\


\(fn CMD)" t nil)

(autoload 'sage-shell:run-new-sage "sage-shell-mode" "\


\(fn CMD)" t nil)

(autoload 'sage-shell:sage-mode "sage-shell-mode" "\


\(fn)" t nil)

(defvar sage-shell:func-alias-alist '((sage-shell:sage-mode . sage-mode) (sage-shell:run-sage . run-sage) (sage-shell:run-new-sage . run-new-sage)))

(defvar sage-shell:var-alias-alist '((sage-shell:sage-mode-map . sage-mode-map) (sage-shell:sage-mode-hook . sage-mode-hook) (sage-shell:sage-mode-syntax-table . sage-mode-syntax-table) (sage-shell:sage-mode-abbrev-table . sage-mode-abbrev-table)))

(defun sage-shell:define-alias nil "\
Define aliases as follows:
| Original name                     | Alias                  |
|-----------------------------------+------------------------|
| sage-shell:sage-mode              | sage-mode              |
| sage-shell:sage-mode-map          | sage-mode-map          |
| sage-shell:sage-mode-hook         | sage-mode-hook         |
| sage-shell:sage-mode-syntax-table | sage-mode-syntax-table |
| sage-shell:sage-mode-abbrev-table | sage-mode-abbrev-table |
| sage-shell:run-sage               | run-sage               |
| sage-shell:run-new-sage           | run-new-sage           |
|-----------------------------------+------------------------|
" (interactive) (dolist (c sage-shell:func-alias-alist) (defalias (cdr c) (car c))) (dolist (c sage-shell:var-alias-alist) (defvaralias (cdr c) (car c))))

(add-to-list 'auto-mode-alist (cons "\\.sage$" 'sage-shell:sage-mode))

(autoload 'sage-shell-sagetex:load-file "sage-shell-mode" "\
Load a .sagetex.sage file to an existing Sage process.

\(fn FILENAME)" t nil)

(autoload 'sage-shell-sagetex:load-current-file "sage-shell-mode" "\


\(fn)" t nil)

(defalias 'sage-shell:sagetex-load-file 'sage-shell-sagetex:load-file)

(autoload 'sage-shell-sagetex:compile-file "sage-shell-mode" "\
This command runs LaTeX on the current file, loads the
.sagetex.sage file to an existing Sage process and runs LaTeX
again. See the documentation of
`sage-shell-sagetex:latex-command' and
`sage-shell-sagetex:auctex-command-name' for the customization.

\(fn F)" t nil)

(autoload 'sage-shell-sagetex:compile-current-file "sage-shell-mode" "\


\(fn)" t nil)

(autoload 'sage-shell-sagetex:run-latex-and-load-file "sage-shell-mode" "\
This command runs LaTeX and loads a .sagetex.sage file to the
exisiting Sage process.

\(fn F)" t nil)

(autoload 'sage-shell-sagetex:run-latex-and-load-current-file "sage-shell-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("sage-shell-mode-pkg.el") (22180 24597
;;;;;;  488698 854000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sage-shell-mode-autoloads.el ends here
