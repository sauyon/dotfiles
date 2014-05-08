(require 'package)

;; EDE - never use this, maybe I should get rid of it...
(global-ede-mode t)
;; #2spacetabmasterrace
(setq-default tab-width 2
							indent-tabs-mode t
							c-basic-offset 2
							css-indent-offset 2
							js-indent-level 2
							js2-basic-offset 2
							python-indent-offset 2)
;; miscellaneous stuff
(setq-default show-trailing-whitespace t
							fill-column 80)
;; Nobody needs this
(global-unset-key "\C-z")

;; Packages!
(setq package-archives
			'(("gnu" . "http://elpa.gnu.org/packages/")
				("marmalade" . "http://marmalade-repo.org/packages/")
				("melpa" . "http://melpa.milkbox.net/packages/")))

(defun my-bell-function ()
  (unless (memq this-command
								'(isearch-abort abort-recursive-edit exit-minibuffer
																keyboard-quit mwheel-scroll down up next-line previous-line
																backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(eval-after-load "tramp"
	'(progn
		 (defvar sudo-tramp-prefix
			 "/sudo:"
			 (concat "Prefix to be used by sudo commands when building tramp path "))
		 (defun sudo-file-name (filename)
			 (set 'splitname (split-string filename ":"))
			 (if (> (length splitname) 1)
					 (progn (set 'final-split (cdr splitname))
									(set 'sudo-tramp-prefix "/sudo:")
									)
				 (progn (set 'final-split splitname)
								(set 'sudo-tramp-prefix (concat sudo-tramp-prefix "root@localhost:")))
				 )
			 (set 'final-fn (concat sudo-tramp-prefix (mapconcat (lambda (e) e) final-split ":")))
			 (message "splitname is %s" splitname)
			 (message "sudo-tramp-prefix is %s" sudo-tramp-prefix)
			 (message "final-split is %s" final-split)
			 (message "final-fn is %s" final-fn)
			 (message "%s" final-fn)
			 )

		 (defun sudo-find-file (filename &optional wildcards)
			 "Calls find-file with filename with sudo-tramp-prefix prepended"
			 (interactive "fFind file with sudo ")
			 (let ((sudo-name (sudo-file-name filename)))
				 (apply 'find-file
								(cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

		 (defun sudo-reopen-file ()
			 "Reopen file as root by prefixing its name with sudo-tramp-prefix and by clearing buffer-read-only"
			 (interactive)
			 (let*
					 ((file-name (expand-file-name buffer-file-name))
						(sudo-name (sudo-file-name file-name)))
				 (progn
					 (setq buffer-file-name sudo-name)
					 (rename-buffer sudo-name)
					 (setq buffer-read-only nil)
					 (message (concat "File name set to " sudo-name)))))

		 (global-set-key (kbd "C-x C-r") 'sudo-find-file)
		 (global-set-key (kbd "C-c o s") 'sudo-reopen-file)))

(load "tramp")
