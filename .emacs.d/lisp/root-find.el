(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root (file)
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive
   (let* ( ;; We bind the variable `file-name-history' locally so we can
          ;; use a separate history list for "root" files.
          (file-name-history find-file-root-history)
          (name (or buffer-file-name default-directory))
          (tramp (and (tramp-tramp-file-p name)
                      (tramp-dissect-file-name name)))
          path dir)

     ;; If called from a "root" file, we need to fix up the path.
     (when tramp
       (setq path (tramp-file-name-localname tramp)
             dir (file-name-directory path)))

     (list (read-file-name "Find file (sudo): " dir path))))
  (require 'tramp)
  (setq ret (find-file (concat find-file-root-prefix (expand-file-name file))))
  ;; If this all succeeded save our new history list.
  (setq find-file-root-history file-name-history)
  ;; allow some user customization
  (run-hooks 'find-file-root-hook)
  ret)
