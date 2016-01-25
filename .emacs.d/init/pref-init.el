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

(global-auto-complete-mode t)

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
