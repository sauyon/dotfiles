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
 '(foreground-color nil)
 '(package-selected-packages
   (quote
    (2048-game ac-clang ac-emoji ac-haskell-proc achievements ac-html ac-ispell ac-ja ac-js2 ac-math ac-mozc ac-python android-mode auto-complete-sage auto-dictionary cmake-mode color-theme csv-mode dict-tree e edit-server ensime ess flycheck flycheck-haskell flycheck-kotlin flycheck-rust go-autocomplete go-mode graphviz-dot-mode hardcore-mode haskell-mode helm-nixos-options jabber js2-mode json-mod json-mode magit magit-find-file magit-gh-pulls mmm-mode mo-git-blame multi-term nixos-options oberon p4 pdf-tools pkgbuild-mode pretty-sha-path protobuf-mode scala-mode scala-mode2 scss-mode smart-tabs-mode solarized-theme systemd toml-mode tuareg web-mode yaml-mode zeitgeist)))
 '(user-full-name "Sauyon Lee"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'solarized-dark t)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
