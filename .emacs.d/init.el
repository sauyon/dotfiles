(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ;; Initializations ---------------------------------------------

(load "root-find")
(load "pref-init")
(load "mode-init")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-engine 'luatex)
 '(TeX-view-program-list '(("MuPDF" ("mupdf-x11 %o"))))
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "MuPDF")
     (output-html "xdg-open")))
 '(background-color nil)
 '(background-mode dark)
 '(c-basic-offset 2)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.3)
 '(company-tooltip-limit 15)
 '(css-indent-offset 2)
 '(cursor-color nil)
 '(custom-enabled-themes '(solarized-dark))
 '(custom-safe-themes
   '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default))
 '(default-frame-alist
    '((vertical-scroll-bars)
      (font . "Noto Mono for Powerline-12")))
 '(display-line-numbers 'relative)
 '(display-line-numbers-width 4)
 '(dumb-jump-mode nil)
 '(fill-column 100)
 '(font-use-system-font nil)
 '(foreground-color nil)
 '(global-auto-complete-mode nil)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-display-line-numbers-mode t)
 '(global-prettify-symbols-mode t)
 '(go-mode-hook
   '((lambda nil
       (add-hook 'before-save-hook #'gofmt-before-save)
       (setq indent-tabs-mode t))))
 '(helm-mode t)
 '(ido-mode nil nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_US")
 '(ispell-program-name "/usr/bin/hunspell")
 '(ivy-explorer-mode t nil (ivy-explorer))
 '(ivy-mode t)
 '(js-indent-level 2)
 '(load-prefer-newer t)
 '(nix-prettify-global-mode t)
 '(odasa-library-paths
   '(("javascript" lambda
      (ql)
      (concat ql "/javascript/ql/src"))
     ("python" lambda
      (ql)
      (concat ql "/python/ql/src"))
     ("java" lambda
      (ql)
      (concat ql "/java/ql/src"))
     ("cpp" lambda
      (ql)
      (concat ql "/cpp/ql/src"))
     ("csharp" lambda
      (ql)
      (concat ql "/csharp/ql/src"))))
 '(odasa-project-data-location "/home/sauyon/semmle/data/")
 '(odasa-project-location "/home/sauyon/semmle/projects/")
 '(odasa-source-location "/home/sauyon/devel/maxcode/")
 '(odasa-stable-source-location "/home/sauyon/devel/code/")
 '(odasa-trap-cache "/home/sauyon/.cache/semmle/traps")
 '(org-babel-load-languages '((dot . t) (ditaa . t) (emacs-lisp . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/org/notes.org")
 '(org-ditaa-jar-path
   "/nix/store/9raw2s5pbw29ha2a3lhyl8i9nzai9cpv-ditaa-0.9/lib/ditaa.jar")
 '(org-latex-classes
   '(("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("thesis" "\\documentclass[titlepage,a4paper,12pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
 '(org-latex-compiler "lualatex")
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t)
     ("T1" "fontenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "amssymb" t)
     ("" "hyperref" nil)
     ("" "enumitem" nil)
     "\\tolerance=1000"))
 '(org-latex-listings 'minted)
 '(org-latex-minted-langs
   '((emacs-lisp "common-lisp")
     (cc "c++")
     (cperl "perl")
     (shell-script "bash")
     (caml "ocaml")
     (csp "text")))
 '(org-latex-minted-options '(("fontsize" "\\footnotesize")))
 '(org-latex-packages-alist '(("" "minted" t)))
 '(org-latex-pdf-process
   '("latexmk -shell-escape -pdf -pdflatex=%latex -view=none -f %f"))
 '(org-list-allow-alphabetical t)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(org-src-lang-modes
   '(("dot" . graphviz-dot)
     ("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("dot" . fundamental)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)))
 '(org-src-tab-acts-natively t)
 '(package-archives
   '(("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(idle-highlight pkgbuild-mode solarized-theme lsp-ivy ivy-explorer flyspell-correct-ivy yaml-mode dired-icon nix-update use-package lsp-ui company-shell company-lsp groovy-mode go-guru company-go nix-mode fill-column-indicator smart-jump semmle lua-mode graphviz-dot-mode dumb-jump ws-butler latex-math-preview latex-unicode-math-mode cargo google-c-style bison-mode go-errcheck go-dlv latex-pretty-symbols smali-mode ac-haskell-proc android-mode auto-dictionary cmake-mode dict-tree flycheck-haskell flycheck-rust haskell-mode json-mod json-mode magit magit-find-file magit-gh-pulls mmm-mode mo-git-blame nixos-options pretty-sha-path protobuf-mode scala-mode2 scss-mode smart-tabs-mode systemd toml-mode))
 '(pcap-mode-tshark-executable nil)
 '(ql-mode-target-language "javascript" t)
 '(ql-ql-location "/home/sauyon/devel/maxcode/ql")
 '(require-final-newline t)
 '(ring-bell-function 'my-bell-function)
 '(rust-format-on-save nil)
 '(rust-indent-offset 2)
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(smart-jump-default-mode-list
   '(cc-mode csharp-mode clojure-mode eglot elisp-mode elixir-mode elm-mode erlang-mode go-mode haskell-mode
             (js2-mode rjsx-mode)
             lisp-mode lispy lua-mode lsp-mode python ruby-mode rust-mode scala-mode scheme swift-mode typescript-mode web-mode))
 '(standard-indent 2)
 '(tab-width 2)
 '(test-hook nil)
 '(tool-bar-mode nil)
 '(tool-bar-style 'text)
 '(user-full-name "Sauyon Lee")
 '(ws-butler-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#839496" :inverse-video nil)))))

;; (unless package-archive-contents (package-refresh-contents))
;; (package-install-selected-packages)
