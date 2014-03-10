(define-generic-mode 'ragel-mode
	'(?#) ;; comments
	'(
		;; keywords
		"machine" "action" "access" "context" "include" "import" "export" "prepush" "postpop"
		"when" "inwhen" "outwhen" "err" "lerr" "eof" "from" "to"
		"alphtype" "getkey" "write"
		;; rules
		"any" "ascii" "extend" "alpha" "digit" "alnum" "lower" "upper"
		"xdigit" "cntrl" "graph" "print" "punct" "space" "zlen" "empty"
		;; inline code matching
		"fpc" "fc" "fcurs" "fbuf" "fblen" "ftargs" "fstack"
		"fhold" "fgoto" "fcall" "fret" "fentry" "fnext" "fexec" "fbreak"
		)
	'(
		;; literals
																				;("\\([^\\)]*\\)" . font-lock-constant-face)
																				;("\\[[[^\\]]*\\]" . font-lock-constant-face)
		("\(\"\\?'\"\'|\\?\"'\|'[^']*'\|\"[^\"]*\"\)" . font-lock-constant-face)

		;; Numbers
		("[0-9][0-9]*" . font-lock-constant-face)
		("0x[0-9a-fA-F][0-9a-fA-F]*" . font-lock-constant-face)

		;; Operators
		("[>$%@]" . font-lock-constant-face)
		("<>\|<" . font-lock-constant-face)
		;; ("[>\<$%@][!\^/*~]" . font-lock-constant-face)
		;; ("[>$%]?" . font-lock-constant-face)
		;; ("<>[!\^/*~]" . font-lock-constant-face)
		("=>" . font-lock-constant-face)
		("->" . font-lock-constant-face)

		(":>" . font-lock-constant-face)
		(":>>" . font-lock-constant-face)
		("<:" . font-lock-constant-face)
		)
	nil ;'(".rl\\'")
	nil
	"Generic mode for mmm-mode editing .rl files.")

(mmm-add-classes
 '((embedded-ragel
		:submode ragel-mode
		:save-matches 1
		:case-fold-search 1
		:face mmm-declaration-submode-face
		:front "%%{"
		:include-front t
		;; :front-offset (end-of-line 1)
		:back "}%%"
		:include-back t
		;; :back-offset (end-of-line 1)
		;; line match? "%%[^{]" "%%$"
		)))
(mmm-add-mode-ext-class 'c++-mode nil 'embedded-ragel)
(mmm-add-mode-ext-class 'cperl-mode nil 'embedded-ragel)
(mmm-add-mode-ext-class 'c-mode nil 'embedded-ragel)

(add-to-list 'mmm-c-derived-modes 'embedded-ragel)
