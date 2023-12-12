;; Nobody needs this
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-z C-z") 'my-suspend-frame)

(global-set-key (kbd "C-j") 'lsp-find-definition)

;; We don't need to be stuck with C-i as tab in 202x;
(define-key input-decode-map [(control ?i)] [control-i])
(define-key input-decode-map [(control ?I)] [(shift control-i)])

; This doesn't need to be in a separate file :thinking:
(global-set-key (kbd "<f8>") 'recompile)
(global-set-key (kbd "<f5>") 'revert-buffer)
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
;; consult > ibuffer
(global-set-key (kbd "C-x C-b") 'consult-project-buffer)
(global-set-key (kbd "C-x b") 'consult-buffer)
;; ebib
;; (global-set-key (kbd "C-c e") 'ebib)
;; projectile
(global-set-key (kbd "M-r") 'projectile-command-map)
;; embark
;; (global-set-key (kbd "M-o") 'embark-act)

;; consult is legendary
(global-set-key (kbd "M-y") 'consult-yank-replace)

(global-set-key (kbd "C-x C-e") 'consult-kmacro)

(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s g") 'consult-git-grep)

(global-set-key (kbd "M-g M-g") 'consult-goto-line)

(global-set-key (kbd "C-,") 'consult-mark)
(global-set-key (kbd "C-x C-,") 'consult-global-mark)

(global-set-key (kbd "C-.") 'consult-outline)

(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "M-g f") 'avy-goto-line)

(eval-after-load "isearch"
  '(define-key isearch-mode-map (kbd "C-'") 'avy-isearch))

;; print 'blah instead of (quote blah) in customize
(advice-add 'custom-save-all :around
            (lambda (orig)
              (let ((print-quoted t))
                (funcall orig))))

(defhydra hydra-outline (global-map "M-o")
    "
^Hide (shift)/Show^           ^Move^
^^^^^^^^----------------------------------------------
_a_: all                      _u_: up heading
_b_: body                     _n_: next heading
_e_: entry                    _p_: previous heading
_l_: subtree body             _f_: forward
_c_: children                 _b_: back
_o_: other (hide only)
"
  ("A" outline-hide-sublevels "hide sublevels")
  ("B" outline-hide-body "hide body")
  ("E" outline-hide-entry "hide entry")
  ("L" outline-hide-leaves "hide leaves")
  ("C" outline-hide-subtree)
  ("a" outline-show-all "show all")
  ("e" outline-show-entry "show entry")
  ("c" outline-show-children "show children")
  ("l" outline-show-branches "show branches")
  ("s" outline-show-subtree "show subtree")
  ("o" outline-hide-other)
  ("u" outline-up-heading)
  ("n" outline-next-visible-heading)
  ("p" outline-previous-visible-heading)
  ("f" outline-forward-same-level)
  ("b" outline-backward-same-level)
  )
                                        ; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")
                                        ; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
                                        ; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
                                        ; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level

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

;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode 1)

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
