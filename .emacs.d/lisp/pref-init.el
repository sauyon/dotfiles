(require 'package)

;; EDE - never use this, maybe I should get rid of it...
(global-ede-mode t)
;; (global-auto-complete-mode t)

(setq-default
;; #2spacetabmasterrace
 indent-tabs-mode t
 tab-width 2
 c-basic-offset 2
 css-indent-offset 2
 js-indent-level 2
 js2-basic-offset 2
 web-mode-markup-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-indent-style 2
 sh-basic-offset 2
 sh-indentation 2
 python-indent-offset 2
 ;; miscellaneous stuff
 show-trailing-whitespace t
 fill-column 80)

;; Nobody needs this
(global-unset-key "\C-z")
;; This doesn't need to be in a separate file :thinking:
(global-set-key (kbd "<f8>") 'recompile)

;; Don't fucking ring bells on things I do all the time
(defun my-bell-function ()
  (unless (memq this-command
								'(isearch-abort abort-recursive-edit exit-minibuffer
																keyboard-quit mwheel-scroll down up next-line previous-line
																backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)
