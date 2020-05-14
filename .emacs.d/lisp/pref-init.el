;; Nobody needs this
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-z C-z") 'my-suspend-frame)

;; This doesn't need to be in a separate file :thinking:
(global-set-key (kbd "<f8>") 'recompile)
;; Why isn't this default? (??????)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
;; (global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 10)))
;; (global-set-key (kbd "M-n") (lambda () (interactive) (next-line 10)))
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
;; this should also be the default (thanks `technomancy/better-defaults')
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;; ebib
(global-set-key (kbd "C-c e") 'ebib)

;; print 'blah instead of (quote blah) in customize
(advice-add 'custom-save-all :around
            (lambda (orig)
              (let ((print-quoted t))
                (funcall orig))))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(autoload 'use-package "use-package" "Use package" t)

(setq skeleton-further-elements '((abbrev-mode nil)))

(define-skeleton mathop-skeleton
  "something"
  "ignored"
  ("something" "#+LATEX_HEADER: \\DeclareMathOperator{\\" (setq skeletonmathop (skeleton-read "op: "))
  "}{\\mathsf{" skeletonmathop "}}\n"_))
(global-set-key (kbd "C-c o") 'mathop-skeleton)

;; (require 'math-symbol-lists)
;; (quail-define-package "math" "UTF-8" "Ω" t)
;; (quail-define-rules ; add whatever extra rules you want to define here...
;;  ("\\from"    #X2190)
;;  ("\\to"      #X2192)
;;  ("\\lhd"     #X22B2)
;;  ("\\rhd"     #X22B3)
;;  ("\\unlhd"   #X22B4)
;;  ("\\unrhd"   #X22B5))
;; (mapc (lambda (x)
;;         (if (cddr x)
;;             (quail-defrule (cadr x) (car (cddr x)))))
;;       (append math-symbol-list-basic math-symbol-list-extended))

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

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
