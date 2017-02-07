;;; csp-mode.el --- major mode for editing CSP specification in Emacs
;;;                 using FDR2 syntax

;; Copyright (C) 1996-2001 Olaf Bergmann, Markus Dahlweid, Uwe Schulze

;; Original Author: Olaf Bergmann   <bergmann@tzi.de>
;; Changes by:      Markus Dahlweid <dahlweid@tzi.de>
;;                  Uwe Schulze     <uschulze@tzi.de>
;;                  Raymond Scholz  <rscholz@tzi.de>
;;
;; Maintainer:      Raymond Scholz  <rscholz@zonix.de>
;;
;; Version: 1.0.1
;; Keywords: CSP, specification, formal methods

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; There is an Texinfo file that describes this package.  The GNU
;; General Public License is included in that file.  You should read
;; it to get the most from this package.

;;; Commentary:

;; The latest version can always be found at
;;
;;    <URL:http://www.zonix.de/div/el/csp-mode/>
;;

;; See INSTALL for installation instructions.

(defvar csp-mode-abbrev-table nil
  "Abbrev table in use in csp-mode buffers.")
(define-abbrev-table 'csp-mode-abbrev-table ())

(defvar csp-mode-map ()
  "Keymap used in csp mode.")
(if csp-mode-map
    ()
  (setq csp-mode-map (make-sparse-keymap))
  (define-key csp-mode-map ";"        'electric-csp-semi)
  (define-key csp-mode-map [?\C->]    'electric-csp-arrow)
  (define-key csp-mode-map "\C-j"     'electric-csp-terminate-line)
  (define-key csp-mode-map "\t"       'electric-csp-tab)

  (define-key csp-mode-map "\C-ce"   'electric-csp-external-choice)
  (define-key csp-mode-map "\C-cl"   'electric-csp-interleave)
  (define-key csp-mode-map "\C-c~"   'electric-csp-internal-choice)
  (define-key csp-mode-map "\C-ci"   'electric-csp-internal-choice)
  (define-key csp-mode-map "\C-c|"   'electric-csp-sync)
  (define-key csp-mode-map "\C-cp"   'electric-csp-sync)
  (define-key csp-mode-map "\C-c{"   'electric-csp-channel-set)
  (define-key csp-mode-map "\C-cs"   'electric-csp-channel-set)

;  (define-key csp-mode-map "\M-\C-h"  'csp-mark-defun)
  (define-key csp-mode-map "\C-c\C-c" 'csp-comment-area)
  (define-key csp-mode-map "\C-c\C-u" 'csp-uncomment-area)
  (define-key csp-mode-map "\C-c\C-v" 'csp-validate)
;  (define-key csp-mode-map "\M-\C-a"  'csp-beg-of-defun)
;  (define-key csp-mode-map "\M-\C-e"  'csp-end-of-defun)
;  (define-key csp-mode-map "\C-c\C-d" 'csp-goto-defun)
)

(defvar csp-keywords
  '("STOP" "SKIP" "CHAOS" "if" "then" "else" "channel" "datatype"
    "nametype" "let" "ldot" "lambda" "within" "and" "or" "not"
    "transparent" "true" "false"))

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
(defconst csp-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.!?,_]*'*\\>")
(defconst csp-name-re "\\<\\([a-zA-Z][a-zA-Z_0-9_]*'*\\([ \t]*([ \ta-zA-Z0-9_\\+\\-\\*/%&|'().]+\\(,[ \ta-zA-Z0-9_\\+\\-\\*/%&|'().]+\\)*)\\)?\\)")
(defconst csp-declaration-re
  (concat "^[ \t]*\\(\\<\\(channel\\|datatype\\|nametype\\|transparent\\)\\>\\|"
	  csp-name-re "[ \t]*\\(\\(=[^=]\\)\\|:\\)\\)"))
(defconst csp-defun-re       (concat "^[ \t]*" csp-name-re "[ \t]*=[^=]"))
(defconst csp-assert-re   "[ \t]*\\<\\(assert\\)\\>")
(defconst csp-sub-block-re   "(*\\<\\(then\\|else\\)\\>")
(defconst csp-noindent-re    "\\<\\(then\\|else\\)\\>")
(defconst csp-nosemi-re      "\\<\\(then\\|else\\)\\>")
(defconst csp-autoindent-lines-re "\\<\\(else\\)\\>")
(defconst csp-logical-op-re    "\\<\\(and\\|or\\|not\\)\\>")
(defconst csp-boolean-re    "\\<\\(true\\|false\\)\\>")
(defconst csp-logical-op-re    "\\<\\(and\\|or\\|not\\)\\>")
(defconst csp-op-re    "\\<\\(and\\|or\\|not\\)\\>\\|<=\\|>=\\|@\\|-?>\\|<-?\\|*\\|==\\|!=\\|!\\|\\\\\\|/\\|:\\|;\\|?\\|#\\|&\\||~|\\|\\[\\]\\|\\[\\[\\|\\]\\]\\|\\[|?\\||?\\]\\|{|?\\||?}\\|||?|?\\|\\^\\|\\+\\|\\%\\|\\[\\([TF]\\|FD\\)=")

(defvar csp-mode-syntax-table nil
  "Syntax table in use in csp-mode buffers.")

(if csp-mode-syntax-table
    ()
  (setq csp-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "."     csp-mode-syntax-table)
  (modify-syntax-entry ?/ "."      csp-mode-syntax-table)
  (modify-syntax-entry ?( "()"     csp-mode-syntax-table)
  (modify-syntax-entry ?) ")("     csp-mode-syntax-table)
  (modify-syntax-entry ?* "."      csp-mode-syntax-table)
  (modify-syntax-entry ?{ "(} 1"   csp-mode-syntax-table)
  (modify-syntax-entry ?} "){ 4"   csp-mode-syntax-table)
  (modify-syntax-entry ?+ "."      csp-mode-syntax-table)
  (modify-syntax-entry ?- ". 123b" csp-mode-syntax-table)
  (modify-syntax-entry ?= "."      csp-mode-syntax-table)
  (modify-syntax-entry ?% "."      csp-mode-syntax-table)
  (modify-syntax-entry ?< "."      csp-mode-syntax-table)
  (modify-syntax-entry ?> "."      csp-mode-syntax-table)
  (modify-syntax-entry ?& "."      csp-mode-syntax-table)
  (modify-syntax-entry ?| "."      csp-mode-syntax-table)
  (modify-syntax-entry ?[ "."      csp-mode-syntax-table)
  (modify-syntax-entry ?] "."      csp-mode-syntax-table)
  (modify-syntax-entry ?_ "w"      csp-mode-syntax-table)
  (modify-syntax-entry ?\' "w"     csp-mode-syntax-table)
  (modify-syntax-entry ?? "."      csp-mode-syntax-table)
  (modify-syntax-entry ?! "."      csp-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"   csp-mode-syntax-table))


(setq csp-font-lock-keywords-1
      (list
       (cons "\\<\\(assert\\|channel\\|datatype\\|nametype\\|transparent\\|[Ii]nter\\|[Uu]nion\\|diff\\|card\\)\\>" 'font-lock-type-face)
       (cons "{-\\(\\([^-]*\\)\\|\\(-+[^-}]+\\)\\)*-}" 'font-lock-comment-face)))

(setq csp-font-lock-keywords-2
      (append csp-font-lock-keywords-1
	      (list
	       (cons (concat csp-sub-block-re "\\|\\<if\\>\\|"
			     csp-boolean-re "\\|"
			     "\\<\\(let\\|within\\|lambda\\|ldot\\)\\>")
		     'font-lock-keyword-face)
	       (cons "\\<\\(SKIP\\|STOP\\|CHAOS\\)\\>"
		     'font-lock-function-name-face)
	       (cons csp-defun-re '(1 'font-lock-function-name-face)))))

(setq csp-font-lock-keywords-3
      (append csp-font-lock-keywords-2
	      (list
	       (cons "\\[\\([TF]\\|FD\\)=" 'font-lock-reference-face)
	       (cons csp-op-re 'font-lock-reference-face)
	       (cons "\\<[a-zA-Z0-9_]\\(\\([a-zA-Z0-9.!?,_]*'+\\)\\|\\([a-zA-Z0-9.!?,_]*[a-zA-Z0-9!?,_]\\)\\)?\\>\\(\\.\\.?\\)\\<[a-zA-Z0-9_][a-zA-Z_0-9.!?,_]*'*\\>" '(4 'font-lock-reference-face)))))

(setq csp-font-lock-keywords csp-font-lock-keywords-2)

(defvar csp-indent-level 4
  "*Indentation of csp statements with respect to containing block.")

(defvar csp-untabify t
  "*Non-nil means each TAB in csp mode will be converted into spaces.")

;;(defvar csp-auto-newline t
(defvar csp-auto-newline nil
  "*Non-nil means automatically newline after semicolons and the punctuation
mark after an end.")

(defvar csp-tab-always-indent t
  "*Non-nil means TAB in csp mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defvar csp-validate-error-regexps
  '(("\\(error\\|warning\\) at \\([^,]+\\), line \\([0-9]+\\)" 2 3))
  "Alist of regexps to recognize error messages from csp-validate.")

(defvar csp-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[csp-validate] is run.")

(defvar csp-validate-command
  "csp_parser"
  "*The command to validate buffer.
The file name of current buffer's file will be appended to this,
separated by a space.")

(defvar csp-saved-validate-command nil
  "The command last used to validate in this buffer.")

;;;
;;;  Macros
;;;

(defsubst csp-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst csp-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defsubst csp-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (csp-get-beg-of-line) (point)))))


;;;###autoload
(defun csp-mode ()
  "Major mode for editing CSP code written for fdr. \\<csp-mode-map>
TAB indents for csp code.  Delete converts tabs to spaces as it moves back.
LFD terminates current line after indenting it correctly. The next line is indented as well.

Other useful functions are:

\\[csp-comment-area]\t- Put marked area in a comment.
\\[csp-uncomment-area]\t- Uncomment an area commented with \
\\[csp-comment-area] or \"--\".
\\[electric-csp-semi]\t- If csp-auto-newline is set, will insert a semicolon at current position and terminate current line indenting the next.
\\[electric-csp-arrow]\t- Same as before, but inserts an arrow at current position.

Variables controlling indentation/edit style:

 csp-indent-level      (default 4)
    Indentation of csp statements with respect to containing block.
 csp-auto-newline      (default nil)
    Non-nil means automatically newline after semicolons and the arrow
    mark after an end.
 csp-tab-always-indent (default t)
    Non-nil means TAB in csp mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 csp-untabify          (default t)
    Non-nil means TAB in csp mode will always be converted into spaces.

See also the user variables csp-type-keywords, csp-start-keywords and
csp-separator-keywords.

Turning on csp mode calls the value of the variable csp-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map csp-mode-map)
  (setq major-mode 'csp-mode)
  (setq mode-name "csp-mode")
  (setq local-abbrev-table csp-mode-abbrev-table)
  (set-syntax-table csp-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'csp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (make-local-variable 'comment-start)
  (setq comment-start " --")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+ *")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(csp-font-lock-keywords nil t))
  (run-hooks 'csp-mode-hook))

;;;
;;;  Electric functions
;;;
(defun electric-csp-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, indent current line
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (csp-indent-line))
  (delete-horizontal-space) ; Removes trailing whitespaces
  (newline)
  ;; Indent next line
  (csp-indent-line))

;;;
;;; Interactive functions
;;;
(defun csp-comment-area (start end)
  "Put the region into a csp comment."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (< (point) end)
      (if (not (looking-at "^[ \t]*--"))
	  (insert "--"))
      (forward-line 1))))


(defun csp-uncomment-area ()
  "Uncomment a commented area."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "^[ \t]*--"))
	()
      (if (re-search-backward
	   "^[ \t]*\\([^- \t]+\\|\\(-\\([^-]\\|$\\)\\)\\)" (point-min) t)
	  ()
	(goto-char (point-min)))
      (if (not (looking-at "^[ \t]*--"))
	  (forward-line 1))
      (skip-chars-forward " \t")
      (while (looking-at "--")
	(delete-backward-char -2)
	(forward-line 1)
	(skip-chars-forward " \t")))))

;;;
;;; Indentation
;;;
(defconst csp-indent-alist
  '((block . (+ ind csp-indent-level))
;  '((block . (+ ind-levl csp-indent-level))
;    (declaration . (+ ind csp-indent-level))
    (declaration . ind)
    (paramlist . (csp-indent-paramlist t))
    (comment . (csp-indent-comment t))
    (defun . ind) (contexp . ind)
    (unknown . 0) (string . 0)
    (assertion . 0)))

(defun csp-indent-to (col)
  "Indent from current point to column col. Tabs will be converted to sapces unless csp-untabify is set to nil."
  (let ((oldpos (point)))
    (indent-to col)
    (if csp-untabify
	(untabify oldpos (point)))))

(defun csp-insert-tab ()
  "Inserts as many spaces as a TAB-character represents at current position unless csp-untabify is set to nil."
  (let ((oldpos (point)))
    (insert "\t")
    (if csp-untabify
	(untabify oldpos (point)))))

(defun csp-indent-level ()
  "Return the indent-level the current statement has.
Do not count declarations or definitions."
  (save-excursion
    (let ((re " \t"))
      (beginning-of-line)
      (if (looking-at csp-defun-re)
	  (search-forward "=" nil t)
	(if (looking-at (concat "^[ \t]*" csp-name-re "[ \t]*:"))
	    (search-forward ":" nil t)
	  (setq re " \t()")))
      (skip-chars-forward re)
    (current-column))))

(defun csp-indent-comment (&optional arg)
  "Indent current line as comment.
If optional arg is non-nil, just return the
column number the line should be indented to."
    (let* ((stcol (save-excursion
		    (re-search-backward "--" nil t)
		    (1+ (current-column)))))
      (if arg stcol
	(delete-horizontal-space)
	(csp-indent-to stcol))))

(defun csp-indent-declaration (&optional arg start end)
  "Indent current lines as declaration."
  (let ((pos (point-marker)))
    (if (and (not (or arg start)) (not (csp-declaration-beg)))
	()
      (let ((stpos (if start start
		       (forward-word 2) (backward-word 1) (point)))
	    (edpos (set-marker (make-marker)
			       (if end end
				 (max (progn (csp-declaration-end)
					     (point))
				      pos))))
	    ind)

	(goto-char stpos)
	;; Indent lines
	(if arg
	    (while (<= (point) (marker-position edpos))
	      (beginning-of-line)
	      (delete-horizontal-space)
		(csp-indent-to (+ arg csp-indent-level))
	      (forward-line 1)))

    ;; If arg - move point
    (if arg (forward-line -1)
      (goto-char (marker-position pos)))))))

(defun csp-indent-line (&optional state)
  "Indent current line as a csp statement."
  (let* ((indent-str (csp-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str)))
	 (branch (nth 1 (if state
			    state
			  (csp-get-branch (point) (parse-partial-sexp (point-min) (point)))))))
    (delete-horizontal-space)
    ;; Some things should not be indented
    (if (and (eq type 'declaration) (looking-at csp-declaration-re))
	()
      ;; Other things should have no extra indent
      (if (and (eq type 'block) branch)
	  (let* ((nest 1)
		 (noind (looking-at csp-noindent-re))
		 (ind-levl (save-excursion (if (not branch)
					       0
					     (goto-char branch)
					     (current-column)))))
	    (if noind
		(csp-indent-to ind-levl)
	      (csp-indent-to (+ ind-levl csp-indent-level))))
	;; But most lines are treated this way:
	(csp-indent-to (eval (cdr (assoc type csp-indent-alist))))
	))))

(defun csp-declaration-beg ()
  (numberp (re-search-backward csp-declaration-re (csp-get-beg-of-line 0) t)))


(defun csp-declaration-end ()
  (numberp (re-search-forward csp-declaration-re (save-excursion (end-of-line 2) (point)) t)))

(defsubst csp-within-comment ()
  (save-excursion
    (nth 4 (parse-partial-sexp (csp-get-beg-of-line) (point)))))

(defun csp-get-branch (pos state)
  "Get regexp and position for then- or else-branch pos is in. If it is not in an if-statement return nil."
  (let ((re nil)
	(thenpos nil)
	(elsepos nil)
	(ifpos   nil)
	(ps      nil)
	(minpos (if (nth 1 state) (nth 1 state) (point-min))))
    (setq thenpos (save-excursion
		    (goto-char pos)
		    (catch 'p (let ((res nil))
				(while t
				  (setq res (re-search-backward "\\<then\\>" minpos t))
				  (if (not res)
				      (throw 'p nil)
				    (if (= (car state) (car (parse-partial-sexp (point-min) (point))))
					(throw 'p res)))))))
	  elsepos (save-excursion
		    (goto-char pos)
		    (if (looking-at "\\<else\\>")
			(point)
		      (catch 'p (while t
				  (let ((res (re-search-backward "\\<else\\>" minpos t)))
				    (if (not res)
					(throw 'p nil)
				      (if (= (car state) (car (parse-partial-sexp (point-min) (point))))
					  (throw 'p res)))))))))
    (cond (thenpos (if elsepos
		       (if (> thenpos elsepos)
			   (setq re "\\<then\\>" ps thenpos)
			 (setq re "\\<else\\>" ps elsepos))
		     (setq re "\\<then\\>" ps thenpos)))
	  (elsepos (setq re "\\<else\\>" ps elsepos))) ;thenpos must be nil !
    (if ps
	(save-excursion
	  (goto-char ps)
	  (setq ifpos (catch 'p (let ((res nil) (nest 0))
				  (while t
				    (setq res (re-search-backward (concat re "\\|\\<if\\>") minpos t))
				    (if (not res)
					(throw 'p nil)
				      (if (not (= (car state) (car (parse-partial-sexp (point-min) (point)))))
					  ()
					(if (not (looking-at "\\<if\\>"))
					    (setq nest (1+ nest))
					  (if (= nest 0)
					      (throw 'p res)
					    (setq nest (1- nest))))))))))))
    (list re ifpos thenpos elsepos)))

(defun csp-calculate-indent ()
  "Calculate the indent of the current csp line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
	   (oldpos (point))
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (nest 0) (par 0) (complete (looking-at (concat csp-declaration-re "\\|" csp-assert-re)))
	   (elsed (looking-at "[ \t]*else\\>"))
	   (nestpos nil)
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> (car state) 0)
			  (setq nestpos (csp-get-branch oldpos state))
			  (if (nth 1 nestpos)
			      (progn (setq nest 1)
				     (throw 'nesting 'block))
			    (goto-char (scan-lists (point) -1 1))
			    (setq par (1+ (current-column)))
			    (throw 'nesting 'declaration)))
			 ((looking-at "--")
			  (throw 'nesting 'unknown)))
		   ;; Loop until correct indent is found
		   (while t
		     (backward-sexp 1)
		     (cond (;--comment start
			    (looking-at comment-start)
			    (throw 'nesting 'unknown))
		           (;--Declaration part
			    (looking-at csp-declaration-re)
			    (if (save-excursion
				  (goto-char oldpos)
				  (forward-line -1)
				  (looking-at "^[ \t]*$"))
				(throw 'nesting 'unknown)
			      (throw 'nesting 'declaration)))
			   (;--assertion
			    (looking-at csp-assert-re)
			    (throw 'nesting 'assertion))
			   (;--If, else or while statement
			    (and (not complete)
				 (looking-at csp-sub-block-re))
			    (throw 'nesting 'block))
			   (;--Found complete statement
			    (save-excursion (forward-sexp 1)
					    (looking-at csp-declaration-re))
			    (setq complete t))
			   (;--No known statements
			    (bobp)
			    (throw 'nesting 'unknown))
			   )))))

      ;; Return type of block and indent level.
      (if (> par 0)                               ; Unclosed Parenthesis
	  (list 'contexp par)
	(if (> nest 0)
	    (progn
	      (goto-char (nth 1 nestpos))
	      (list type (csp-indent-level)))
	  (list type (csp-indent-level)))))))

(defun csp-validate (command)
  "Validate CSP code.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the line in the CSP program that caused it."
  (interactive
   (list (read-string "Validate command: "
		      (or csp-saved-validate-command
			  (concat csp-validate-command
				  " "
				  (let ((name (buffer-file-name)))
				    (and name
					 (file-name-nondirectory name))))))))
  (setq csp-saved-validate-command command)
  (if csp-offer-save
      (save-some-buffers nil nil))
  (compile-internal command "No more errors" "CSP validation"
		    nil
		    csp-validate-error-regexps))

(defun electric-csp-tab ()
  "Function called when TAB is pressed in csp mode."
  (interactive)
  ;; Do nothing if within a string
  (if (or (csp-within-string) (csp-within-comment))
      (csp-insert-tab)
    ;; If csp-tab-always-indent, indent the beginning of the line.
    (if csp-tab-always-indent
	(save-excursion
	  (beginning-of-line)
	  (csp-indent-line))
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (bolp))
	  (csp-indent-line)
	(csp-insert-tab)))
    (csp-indent-command)))

(defun csp-indent-command ()
  "Indent for special part of code."
  (let* ((indent-str (csp-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str))))
    (cond ((eq type 'declaration)
	   (csp-indent-declaration)))
    (if (looking-at "[ \t]+$")
	(skip-chars-forward " \t"))))

(defun electric-csp-semi ()
  "Insert `;' reindent the line."
  (interactive)
  (insert last-command-char)
  (save-excursion
      (beginning-of-line)
      (csp-indent-line))
  (if csp-auto-newline
      (electric-csp-terminate-line)))

(defun electric-csp-arrow ()
  "Insert ` ->' reindent the line."
  (interactive)
  (insert " ->")
  (save-excursion
      (beginning-of-line)
      (csp-indent-line))
  (if csp-auto-newline
      (electric-csp-terminate-line)))

(defun csp-generic-insert-op (s)
  "Insert arg1 if preceding char is space or TAB, space followed by arg1 otherwise."
  (let ((ie nil))
    (backward-char 1)
    (if (looking-at "[ \t]")
	(setq ie s)
      (setq ie (concat " " s)))
    (forward-char 1)
  (insert ie)))

(defun electric-csp-external-choice ()
  "Insert `[]' at current position."
  (interactive)
  (csp-generic-insert-op "[] "))

(defun electric-csp-internal-choice ()
  "Insert `|~|' at current position."
  (interactive)
  (csp-generic-insert-op "|~| "))

(defun electric-csp-interleave ()
  "Insert `|||' at current position."
  (interactive)
  (csp-generic-insert-op "||| "))

(defun electric-csp-sync ()
  "Insert `[|  |]' at current position."
  (interactive)
  (csp-generic-insert-op "[|  |] ")
  (backward-char 4))

(defun electric-csp-channel-set ()
  "Insert `{|  |}' at current position."
  (interactive)
  (csp-generic-insert-op "{|  |} ")
  (backward-char 4))

(defun electric-csp-renaming ()
  "Insert `[[  ]]' at current position."
  (interactive)
  (csp-generic-insert-op "[[  ]] ")
  (backward-char 4))

(provide 'csp-mode)

;;; csp-mode.el ends here
