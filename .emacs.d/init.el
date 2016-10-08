(add-to-list 'load-path "~/.emacs.d/init/")

;; Initializations ---------------------------------------------

(load "smart-tabs-init")
(load "mode-init")
(load "keybind-init")
(load "pref-init")

(package-initialize)

(load-theme 'solarized-dark t)

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
 '(custom-safe-themes
	 (quote
		("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(foreground-color nil)
 '(package-selected-packages
	 (quote
		(zeitgeist yaml-mode web-mode solarized-theme smart-tabs-mode scss-mode scala-mode2 protobuf-mode pkgbuild-mode pdf-tools pandoc-mode p4 oberon multi-term mmm-mode json-mode go-mode go-autocomplete flycheck-haskell edit-server dict-tree csv-mode color-theme cmake-mode auto-dictionary auto-complete-sage android-mode achievements ac-python ac-mozc ac-math ac-js2 ac-ja ac-ispell ac-html ac-haskell-process ac-emoji ac-clang 2048-game)))
 '(send-mail-function (quote mailclient-send-it))
 '(user-full-name "Sauyon Lee"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
