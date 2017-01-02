(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Other packages!
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(unless (file-exists-p package-user-dir)
	(package-refresh-contents))

(ensure-package-installed
 'achievements
 'android-mode
 'color-theme
 'edit-server
 'go-mode
 'json-mode
 'markdown-mode
 'pandoc-mode
 'pdf-tools
 'pkgbuild-mode
 'scala-mode2
 'scss-mode
 'smart-tabs-mode
 'solarized-theme
 'web-mode)
