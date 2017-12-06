(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Initializations ---------------------------------------------

(load "root-find")
(load "pref-init")
(load "mode-init")

(setq inhibit-startup-screen t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(background-color nil)
 '(background-mode dark)
 '(cursor-color nil)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(foreground-color nil)
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   (quote
    (google-c-style bison-mode rust-mode go-errcheck go-dlv go-complete latex-pretty-symbols smali-mode 2048-game ac-clang ac-emoji ac-haskell-proc achievements ac-html ac-ispell ac-ja ac-js2 ac-math ac-mozc ac-python android-mode auto-complete-sage auto-dictionary cmake-mode color-theme csv-mode dict-tree edit-server ensime flycheck-haskell flycheck-kotlin flycheck-rust go-autocomplete graphviz-dot-mode hardcore-mode haskell-mode helm-nixos-options jabber json-mod json-mode magit magit-find-file magit-gh-pulls mmm-mode mo-git-blame multi-term nixos-options oberon pkgbuild-mode pretty-sha-path protobuf-mode scala-mode scala-mode2 scss-mode smart-tabs-mode solarized-theme systemd toml-mode tuareg web-mode yaml-mode zeitgeist)))
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
