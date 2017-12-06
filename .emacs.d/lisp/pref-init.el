;; Nobody needs this
(global-unset-key "\C-z")
;; This doesn't need to be in a separate file :thinking:
(global-set-key (kbd "<f8>") 'recompile)
;; Why isn't this default? (??????)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
;; This makes sense in c-like languages (everything I write)
(global-set-key (kbd "M-{") 'beginning-of-defun)
(global-set-key (kbd "M-}") 'end-of-defun)
;; underscores
(global-set-key (kbd "M-SPC") "_")
;; root file finding
(global-set-key (kbd "C-x C-r") 'find-file-root)
;; this should also be default
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))

;; Don't fucking ring bells on things I do all the time
(defun my-bell-function ()
  (unless (memq this-command
								'(isearch-abort abort-recursive-edit exit-minibuffer
																keyboard-quit mwheel-scroll down up next-line previous-line
																backward-char forward-char))
    (ding)))
