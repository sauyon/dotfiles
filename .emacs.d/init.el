(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Initializations ---------------------------------------------

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
		(hardcore-mode haskell-mode jabber js2-mode graphviz-dot-mode zeitgeist yaml-mode web-mode solarized-theme smart-tabs- mode scss-mode scala-mode2 protobuf-mode pkgbuild-mode pdf-tools p4 oberon multi-term mmm-mode json-mod e go-mode go-autocomplete flycheck-haskell edit-server dict-tree csv-mode color-theme cmake-mode auto-dictionary au to-complete-sage android-mode achievements ac-python ac-mozc ac-math ac-js2 ac-ja ac-ispell ac-html ac-haskell-proc ess ac-emoji ac-clang 2048-game toml-mode smart-tabs-mode tuareg ensime scala-mode solarized-theme pkgbuild-mode go-autocomplete go-mode zeitgeist yaml-mode web-mode scala-mode2 multi-term mmm-mode json-mode)))
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
