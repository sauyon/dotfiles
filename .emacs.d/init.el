(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ;; Initializations ---------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-engine 'luatex)
 '(TeX-view-program-list '(("MuPDF" ("mupdf-x11 %o"))))
 '(TeX-view-program-selection
	 '(((output-dvi has-no-display-manager) "dvi2tty") ((output-dvi style-pstricks) "dvips and gv")
		 (output-dvi "xdvi") (output-pdf "MuPDF") (output-html "xdg-open")))
 '(aidermacs-architect-model nil)
 '(aidermacs-default-model "gemini-2.5-pro")
 '(auth-source-save-behavior nil)
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.3)
 '(company-tooltip-limit 15)
 '(completion-category-overrides '((file (styles basic partial-completion))))
 '(css-indent-offset 2)
 '(cursor-color nil)
 '(custom-enabled-themes '(leuven-dark))
 '(custom-safe-themes
	 '("e92fb54405a68be14b7c216b25705d92f1d3a86c4022c85d1445fe44448d676b" default))
 '(default-frame-alist '((vertical-scroll-bars) (font . "NotoSansMono")))
 '(display-line-numbers 'relative)
 '(display-line-numbers-width 4)
 '(doom-modeline-mode t)
 '(dumb-jump-mode t)
 '(enable-recursive-minibuffers t)
 '(face-font-family-alternatives
	 '(("Sans Serif") ("Monospace" "courier" "fixed")
		 ("Monospace Serif" "Courier 10 Pitch" "Consolas" "Courier Std" "FreeMono" "Nimbus Mono L"
			"courier" "fixed")
		 ("courier" "CMU Typewriter Text" "fixed") ("Sans Serif" "helv" "helvetica" "arial" "fixed")
		 ("helv" "helvetica" "arial" "fixed")))
 '(fill-column 100)
 '(font-use-system-font nil)
 '(foreground-color nil)
 '(global-auto-complete-mode nil)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-display-line-numbers-mode t)
 '(global-prettify-symbols-mode t)
 '(go-mode-hook
	 '((lambda nil (add-hook 'before-save-hook #'gofmt-before-save) (setq indent-tabs-mode t))))
 '(golden-ratio-mode t)
 '(helm-mode t)
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_US")
 '(ispell-program-name "/usr/bin/hunspell")
 '(js-indent-level 2)
 '(load-prefer-newer t)
 '(lsp-keymap-prefix "M-i")
 '(menu-bar-mode nil)
 '(minibuffer-prompt-properties
	 '(read-only t face minibuffer-prompt read-only t cursor-intangible t))
 '(nix-prettify-global-mode t)
 '(org-babel-load-languages '((dot . t) (ditaa . t) (emacs-lisp . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/org/notes.org")
 '(org-ditaa-jar-path
	 "/nix/store/9raw2s5pbw29ha2a3lhyl8i9nzai9cpv-ditaa-0.9/lib/ditaa.jar")
 '(org-latex-classes
	 '(("article" "\\documentclass[11pt]{article}" ("\\section{%s}" . "\\section*{%s}")
			("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
			("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
		 ("report" "\\documentclass[11pt]{report}" ("\\part{%s}" . "\\part*{%s}")
			("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}")
			("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
		 ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}")
			("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}")
			("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
		 ("thesis" "\\documentclass[titlepage,a4paper,12pt]{article}"
			("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}")
			("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
 '(org-latex-compiler "lualatex")
 '(org-latex-default-packages-alist
	 '(("AUTO" "inputenc" t) ("T1" "fontenc" t) ("" "fixltx2e" nil) ("" "graphicx" t)
		 ("" "longtable" nil) ("" "float" nil) ("" "wrapfig" nil) ("" "rotating" nil)
		 ("normalem" "ulem" t) ("" "amsmath" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t)
		 ("" "amssymb" t) ("" "hyperref" nil) ("" "enumitem" nil) "\\tolerance=1000"))
 '(org-latex-listings 'minted)
 '(org-latex-minted-langs
	 '((emacs-lisp "common-lisp") (cc "c++") (cperl "perl") (shell-script "bash") (caml "ocaml")
		 (csp "text")))
 '(org-latex-minted-options '(("fontsize" "\\footnotesize")))
 '(org-latex-packages-alist '(("" "minted" t)))
 '(org-latex-pdf-process
	 '("latexmk -shell-escape -pdf -pdflatex=%latex -view=none -f %f"))
 '(org-latex-src-block-backend 'minted)
 '(org-list-allow-alphabetical t)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(org-src-lang-modes
	 '(("dot" . graphviz-dot) ("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
		 ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql) ("calc" . fundamental) ("C" . c)
		 ("cpp" . c++) ("C++" . c++) ("screen" . shell-script)))
 '(org-src-tab-acts-natively t)
 '(package-archives
	 '(("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
	 '(ac-haskell-proc ace-isearch ace-jump-mode ack aidermacs android-mode auto-dictionary bicycle
										 bison-mode cargo cargo-mode cmake-mode combobulate company-go company-lsp
										 company-shell consult-lsp copilot dap-mode dash dict-tree dired-git-info
										 dired-icon dired-preview dired-ranger dirvish dockerfile-mode doom-themes
										 dumb-jump editorconfig elvish-mode embark-consult f fill-column-indicator
										 flycheck-haskell flycheck-rust go-dlv go-errcheck go-guru golden-ratio
										 google-c-style graphviz-dot-mode groovy-mode haskell-mode hcl-mode helm-swoop
										 idle-highlight json-mod json-mode json-par jsonian latex-math-preview
										 latex-pretty-symbols latex-unicode-math-mode lsp-pyright lsp-treemacs lsp-ui
										 lua-mode magit magit-find-file magit-gh-pulls marginalia mmm-mode mo-git-blame
										 nix-mode nix-update nixos-options orderless outline-toc pkgbuild-mode
										 pretty-sha-path projectile projectile-ripgrep protobuf-mode quelpa
										 quelpa-use-package rustic scala-mode2 scss-mode shfmt smali-mode smart-jump
										 smart-tabs-mode swiper systemd terraform-mode toml-mode vertico
										 vertico-posframe vterm ws-butler yaml-mode))
 '(pcap-mode-tshark-executable nil)
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path '("~/devel/"))
 '(ql-mode-target-language "javascript" t)
 '(ql-ql-location "/home/sauyon/devel/maxcode/ql")
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(require-final-newline t)
 '(ring-bell-function 'my-bell-function)
 '(rust-format-on-save nil)
 '(rust-indent-offset 2)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(smart-jump-default-mode-list
	 '(cc-mode csharp-mode clojure-mode eglot elisp-mode elixir-mode elm-mode erlang-mode go-mode
						 haskell-mode (js2-mode rjsx-mode) lisp-mode lispy lua-mode lsp-mode python ruby-mode
						 rust-mode scala-mode scheme swift-mode typescript-mode web-mode))
 '(standard-indent 2)
 '(tab-width 2)
 '(test-hook nil)
 '(tool-bar-mode nil)
 '(tool-bar-style 'text)
 '(user-full-name "Sauyon Lee")
 '(vertico-mode t)
 '(vertico-posframe-mode t)
 '(ws-butler-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#25202a" :foreground "#cfccd2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 121 :width normal :foundry "GOOG" :family "NotoSansM Nerd Font"))))
 '(variable-pitch ((t (:inherit default :foundry "GOOG" :family "NotoSans Nerd Font")))))

(load "root-find")
(load "pref-init")
(load "mode-init")

;; (unless package-archive-contents (package-refresh-contents))
;; (package-install-selected-packages)
(put 'set-goal-column 'disabled nil)
