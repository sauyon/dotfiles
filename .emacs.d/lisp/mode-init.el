(require 'font-lock)

;; Haskell mode ----------------------------------------------------------------

(add-hook 'haskell-mode-hook
          (lambda () (haskell-indentation-mode t)))

;; Markdown mode ---------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Go mode ---------------------------------------------------------------------

(autoload 'go-mode "go-mode" "Go mode" t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; Python mode -----------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))
(add-hook 'python-mode-hook
          (lambda ()
            (push '("->" . ?→) prettify-symbols-alist)
            (push '("in" . ?∈) prettify-symbols-alist)
            (push '("not" . ?¬) prettify-symbols-alist)
            (setq tab-width 4)))

;; Android mode ----------------------------------------------------------------

(autoload 'android-mode "android-mode" "Android mode" t)

;; Flyspell mode ---------------------------------------------------------------

(dolist (hook '(text-mode-hook
                latex-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook
                log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; YAML mode ----------------------------------------------------------

(autoload 'yaml-mode "yaml-mode" "YAML mode" t)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; SCSS mode ----------------------------------------------------------

(autoload 'scss-mode "scss-mode" "SCSS mode" t)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(add-hook 'scss-mode-hook (lambda () (setq scss-compile-at-save nil)))

;; Shell script mode --------------------------------------------------

(add-to-list 'auto-mode-alist '(".bash_aliases\\'" . sh-mode))

;; pkgbuild mode
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode))

;; js2 mode -----------------------------------------------------------

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Cmake mode ---------------------------------------------------------

(autoload 'cmake-mode "cmake-mode" "Cmake mode." t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; Web mode -----------------------------------------------------------

(autoload 'web-mode "web-mode" "Web mode." t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))

;; C mode ------------------------------------------------------------

(autoload 'google-c-style "google-c-style" "Google C style." t)
(setq c-default-style "user")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook
          (lambda ()
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '("!=" . ?≠) prettify-symbols-alist)))

;; C++ mode -----------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

;; Modes where trailing whitespace being red is annoying --------------

(add-hook 'eww-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'compilation-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'diff-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'fundamental-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

;; Rust mode ----------------------------------------------------------

(autoload 'rust-mode "rust-mode" "Rust mode." t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode t
                      rust-format-on-save t)))

;; Dot mode -----------------------------------------------------------

(autoload 'graphviz-dot-mode "dot-mode" "Dot mode." t)

;; Nix mode -----------------------------------------------------------

(autoload 'nix-mode "nix-mode" "Nix mode." t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; Org mode -----------------------------------------------------------

;; (define-key org-mode-map (kbd "C-c [") 'org-reftex-citation)
(add-hook 'org-mode-hook
          (lambda ()
            (require 'ox-latex)
            (require 'org-ref)

            (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
            (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
            (setq show-trailing-whitespace nil)
            (push '("\vee" . ?∨) prettify-symbols-alist)
            (push '("\wedge" . ?∧) prettify-symbols-alist)
            ))
(add-hook 'org-mode-hook 'latex-unicode-simplified)

;; LaTeX mode ---------------------------------------------------------

(add-hook 'latex-mode-hook 'latex-unicode-simplified)
(autoload 'latex-pretty-symbols "latex-pretty-symbols")
(autoload 'latex-unicode-simplified "latex-pretty-symbols")

;; HideShow mode ------------------------------------------------------

(add-hook 'hs-minor-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c S") 'hs-show-all)
            (local-set-key (kbd "C-c H") 'hs-hide-all)
            (local-set-key (kbd "C-c s") 'hs-show-block)
            (local-set-key (kbd "C-c h") 'hs-hide-block)))

;; Semmle QL mode -----------------------------------------------------

(autoload 'ql-mode "semmle" "Semmle QL mode." t)
(add-to-list 'auto-mode-alist
             '("\\.qll?\\'" . ql-mode))


(defun semmle--enable-flyspell()
  (flyspell-prog-mode)
  (ispell-change-dictionary "american" t)
  )

(defun semmle--infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq-local indent-tabs-mode nil))
    (if (> tab-count space-count) (setq-local indent-tabs-mode t))))


(add-hook 'qhelp-mode-hook 'semmle--infer-indentation-style)
(add-hook 'qhelp-mode-hook 'semmle--enable-flyspell)

(add-hook 'ql-mode-hook 'semmle--infer-indentation-style)
(add-hook 'ql-mode-hook 'semmle--enable-flyspell)

;; (smart-jump-setup-default-registers)
