;;; heap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (make-heap) "heap" "heap.el" (20899 63237 905013
;;;;;;  958000))
;;; Generated autoloads from heap.el

(autoload 'make-heap "heap" "\
Create an empty heap with comparison function COMPARE-FUNCTION.

COMPARE-FUNCTION takes two arguments, A and B, and returns
non-nil or nil. To implement a max-heap, it should return non-nil
if A is greater than B. To implemenet a min-heap, it should
return non-nil if A is less than B.

Optional argument INITIAL-SIZE sets the initial size of the heap,
defaulting to 10. Optional argument RESIZE-FACTOR sets the factor
by which the heap's size is increased if it runs out of space,
defaulting to 2.

\(fn COMPARE-FUNCTION &optional INITIAL-SIZE RESIZE-FACTOR)" nil nil)

(defalias 'heap-create 'make-heap)

;;;***

;;;### (autoloads nil nil ("heap-pkg.el") (20899 63238 35031 814000))

;;;***

(provide 'heap-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; heap-autoloads.el ends here
