(require 'font-lock)
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Haskell mode ----------------------------------------------------------------

(add-hook 'haskell-mode-hook
					(lambda ()
						(setq indent-tabs-mode nil)
						(haskell-indentation-mode t)))

;; Ragel mode ------------------------------------------------------------------

(autoload 'ragel-mode "ragel-mode" "Ragel mode" t)
(add-to-list 'auto-mode-alist '("\\.rl\\'" . ragel-mode))


;; Multi-term mode -------------------------------------------------------------

(add-hook 'term-mode-hook
					(lambda ()
						(setq show-trailing-whitespace nil)
						(setq term-bind-key-alist
									(list
									 (quote ("C-c C-c" . term-interrupt-subjob)
													("C-p" . term-send-up)
													("C-n" . term-send-down)
													("C-s" . isearch-forward)
													("C-r" . isearch-backward)
													("C-m" . term-send-raw)
													("M-f" . term-send-forward-word)
													("M-b" . term-send-backward-word)
													("M-o" . term-send-backspace)
													("M-p" . term-send-up)
													("M-n" . term-send-down)
													("M-M" . term-send-forward-kill-word)
													("M-N" . term-send-backward-kill-word)
													("M-r" . term-send-reverse-search-history)
													("M-," . term-send-input)
													("M-." . comint-dynamic-complete))))))

;; Markdown mode ---------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Go mode ---------------------------------------------------------------------

(autoload 'go-mode "go-mode" "Go mode" t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook
					(lambda () (add-hook 'before-save-hook #'gofmt-before-save)))

;; Python mode -----------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))
(add-hook 'python-mode-hook
					(lambda ()
						(setq indent-tabs-mode t
									tab-width (default-value 'tab-width))))

;; Android mode ----------------------------------------------------------------

(autoload 'android-mode "android-mode" "Android mode" t)

;; Flyspell mode ---------------------------------------------------------------

(dolist (hook '(text-mode-hook
								latex-mode-hook))
	(add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook
								log-edit-mode-hook))
	(add-hook hook (lambda () (flyspell-mode -1))))

;; Shell-script mode -----------------------------------------------------------

(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . sh-mode))
(add-hook 'sh-mode-hook
					(lambda ()
						(setq sh-basic-offset 2
									sh-indentation 2)))

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

;; js2 mode -----------------------------------------------------------

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Cmake mode ---------------------------------------------------------

(autoload 'cmake-mode "cmake-mode" "Cmake mode." t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(defun cmake-rename-buffer ()
	"Renames a CMakeLists.txt buffer to cmake-<directory name>."
	(interactive)
																				;(print (concat "buffer-filename = " (buffer-file-name)))
																				;(print (concat "buffer-name     = " (buffer-name)))
	(when (and (buffer-file-name) (string-match "CMakeLists.txt" (buffer-name)))
																				;(setq file-name (file-name-nondirectory (buffer-file-name)))
		(setq parent-dir (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
																				;(print (concat "parent-dir = " parent-dir))
		(setq new-buffer-name (concat "cmake-" parent-dir))
																				;(print (concat "new-buffer-name= " new-buffer-name))
		(rename-buffer new-buffer-name t)
		)
	)

(add-hook 'cmake-mode-hook 'cmake-rename-buffer)

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

(add-hook 'web-mode-hook (lambda ()
													 (setq web-mode-markup-indent-offset 2
																 web-mode-markup-indent-offset 2
																 web-mode-css-indent-offset 2
																 web-mode-code-indent-offset 2
																 web-mode-indent-style 2
																 indent-tabs-mode t)
													 (smart-tabs-mode t)))

;; CC mode ------------------------------------------------------------
(setq c-default-style "linux")

;; C++ mode -----------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

(defun --copy-face (new-face face)
	"Define NEW-FACE from existing FACE."
	(copy-face face new-face)
	(eval `(defvar ,new-face nil))
	(set new-face new-face))

(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
						 'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
						 'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
						 'font-lock-comment-face)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


(add-hook
 'c++-mode-hook
 '(lambda()
		;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
		;; matters.
		(font-lock-add-keywords
		 nil '(;;  new C++11 keywords
					 ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
					 ;; hexadecimal numbers
					 ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
					 ;; integer/float/scientific numbers
					 ("\\<[-+]?[0-9]*\\.?[0-9]+\\([uUlL]{,3}\\|[eE][-+]?[0-9]+\\)?[fFlL]?\\>" . font-lock-constant-face)
					 ;; c++11 string literals
					 ("\\([LuU]R?\\|u8R?\\)\".*?\"" 1 font-lock-keyword-face)
					 ("R\"\\([^ [:cntrl:]\\\\()]\\{0,16\\}\\)(\\(.\\|\\n\\)*?\\()\\1\\)\""
						(0 font-lock-string-face t) (3 font-lock-variable-name-face t))
					 ("\\(R\\)\"\\([^ [:cntrl:]\\\\()]\\{0,16\\}(\\)"
						(1 font-lock-keyword-face t) (2 font-lock-variable-name-face t))
					 ))
		) t)
