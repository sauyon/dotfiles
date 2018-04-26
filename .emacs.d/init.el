(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Initializations ---------------------------------------------

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
 '(cursor-color nil)
 '(custom-enabled-themes '(solarized-dark))
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(fill-column 100)
 '(foreground-color nil)
 '(global-prettify-symbols-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
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
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
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
 '(org-latex-packages-alist '(("" "minted" t) ("margin=2cm" "geometry" nil)))
 '(org-latex-pdf-process
   '("latexmk -shell-escape -pdf -pdflatex=lualatex -view=none -f %f"))
 '(org-list-allow-alphabetical t)
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
   '(ws-butler latex-math-preview latex-unicode-math-mode cargo google-c-style bison-mode rust-mode go-errcheck go-dlv go-complete latex-pretty-symbols smali-mode 2048-game ac-clang ac-emoji ac-haskell-proc achievements ac-html ac-ispell ac-ja ac-js2 ac-math ac-mozc ac-python android-mode auto-complete-sage auto-dictionary cmake-mode color-theme csv-mode dict-tree edit-server ensime flycheck-haskell flycheck-kotlin flycheck-rust go-autocomplete graphviz-dot-mode hardcore-mode haskell-mode helm-nixos-options jabber json-mod json-mode magit magit-find-file magit-gh-pulls mmm-mode mo-git-blame multi-term nixos-options oberon pkgbuild-mode pretty-sha-path protobuf-mode scala-mode scala-mode2 scss-mode smart-tabs-mode solarized-theme systemd toml-mode tuareg web-mode yaml-mode zeitgeist))
 '(ring-bell-function 'my-bell-function)
 '(rust-indent-offset 2)
 '(show-trailing-whitespace t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tool-bar-style 'text)
 '(user-full-name "Sauyon Lee")
 '(ws-butler-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
