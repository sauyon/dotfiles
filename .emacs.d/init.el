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
 '(TeX-engine (quote luatex))
 '(TeX-view-program-list (quote (("MuPDF" ("mupdf-x11 %o")))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "MuPDF")
     (output-html "xdg-open"))))
 '(background-color nil)
 '(background-mode dark)
 '(c-basic-offset 2)
 '(cursor-color nil)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fill-column 100)
 '(foreground-color nil)
 '(global-prettify-symbols-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/org/notes.org")
 '(org-latex-listings (quote minted))
 '(org-latex-packages-alist (quote (("" "minted" t) ("margin=2cm" "geometry" nil))))
 '(org-latex-pdf-process
   (quote
    ("latexmk -shell-escape -pdf -pdflatex=lualatex -view=none -f %f")))
 '(org-list-allow-alphabetical t)
 '(org-src-fontify-natively t)
 '(org-src-lang-modes
   (quote
    (("dot" . graphviz-dot)
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
     ("screen" . shell-script))))
 '(org-src-tab-acts-natively t)
 '(org-structure-template-alist
   (quote
    (("s" "#+BEGIN_SRC ?

#+END_SRC" "<src lang=\"?\">

</src>")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE" "<example>
?
</example>")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE" "<quote>
?
</quote>")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE" "<verse>
?
</verse>")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM" "<verbatim>
?
</verbatim>")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER" "<center>
?
</center>")
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX" "<literal style=\"latex\">
?
</literal>")
     ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
     ("h" "#+BEGIN_HTML
?
#+END_HTML" "<literal style=\"html\">
?
</literal>")
     ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
     ("a" "#+BEGIN_ASCII
?
#+END_ASCII" "")
     ("A" "#+ASCII: " "")
     ("i" "#+INDEX: ?" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">"))))
 '(package-archives
   (quote
    (("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (latex-math-preview latex-unicode-math-mode cargo google-c-style bison-mode rust-mode go-errcheck go-dlv go-complete latex-pretty-symbols smali-mode 2048-game ac-clang ac-emoji ac-haskell-proc achievements ac-html ac-ispell ac-ja ac-js2 ac-math ac-mozc ac-python android-mode auto-complete-sage auto-dictionary cmake-mode color-theme csv-mode dict-tree edit-server ensime flycheck-haskell flycheck-kotlin flycheck-rust go-autocomplete graphviz-dot-mode hardcore-mode haskell-mode helm-nixos-options jabber json-mod json-mode magit magit-find-file magit-gh-pulls mmm-mode mo-git-blame multi-term nixos-options oberon pkgbuild-mode pretty-sha-path protobuf-mode scala-mode scala-mode2 scss-mode smart-tabs-mode solarized-theme systemd toml-mode tuareg web-mode yaml-mode zeitgeist)))
 '(ring-bell-function (quote my-bell-function))
 '(rust-indent-offset 2)
 '(show-trailing-whitespace t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tool-bar-style (quote text))
 '(user-full-name "Sauyon Lee"))
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
