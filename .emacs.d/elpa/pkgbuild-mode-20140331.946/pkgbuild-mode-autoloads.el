;;; pkgbuild-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pkgbuild-mode) "pkgbuild-mode" "pkgbuild-mode.el"
;;;;;;  (21355 14351 550399 962000))
;;; Generated autoloads from pkgbuild-mode.el

(autoload 'pkgbuild-mode "pkgbuild-mode" "\
Major mode for editing PKGBUILD files. This is much like shell-script-mode mode.
 Turning on pkgbuild mode calls the value of the variable `pkgbuild-mode-hook'
with no args, if that value is non-nil.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("/PKGBUILD\\'" . pkgbuild-mode))

;;;***

;;;### (autoloads nil nil ("pkgbuild-mode-pkg.el") (21355 14351 656889
;;;;;;  273000))

;;;***

(provide 'pkgbuild-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pkgbuild-mode-autoloads.el ends here
