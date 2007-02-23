;;;
;;; Bootstrap file for adding an IR1 attribute.
;;;

(in-package :c)

(handler-bind ((error (lambda (c)
			(declare (ignore c))
			(invoke-restart 'continue))))
  (def-boolean-attribute ir1
    call
    unsafe
    unwind
    any
    foldable
    flushable
    movable
    predicate
    recursive
    explicit-check
    dynamic-extent-closure-safe))

;;; End of file.
