(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
	      '(indent-relative
		indent-relative-maybe))
	(setq indent-tabs-mode nil))
    ad-do-it))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
	(indent-tabs-mode
	 (save-excursion
	   (beginning-of-line)
	   (while (looking-at "\t*\\( +\\)\t+")
	     (replace-match "" nil nil nil 1)))
	 (setq tab-width tab-width)
	 (let ((tab-width fill-column)
	       (,offset fill-column)
	       (wstart (window-start)))
	   (unwind-protect
	       (progn ad-do-it)
	     (set-window-start (selected-window) wstart))))
	(t
	 ad-do-it)))))

(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)
(smart-tabs-advice js2-indent-line js2-basic-offset)
(smart-tabs-advice cperl-indent-line cperl-indent-level)
(smart-tabs-advice ruby-indent-line ruby-indent-level)
(setq ruby-indent-tabs-mode t)
(smart-tabs-advice vhdl-indent-line vhdl-basic-offset)
(setq vhdl-indent-tabs-mode t)
(smart-tabs-advice python-indent-line python-indent-offset)
