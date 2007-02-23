;;; -*- Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/interr.lisp,v 1.13 2006/06/30 18:41:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file defines all of the internal errors.  How they are handled is
;;; defined in .../code/interr.lisp.  How they are signaled depends on the
;;; machine.
;;; 
;;; Written by William Lott.
;;;
(in-package "KERNEL")

(export '(error-number-or-lose))


(defun error-number-or-lose (name)
  (or (position name (c:backend-internal-errors c:*backend*) :key #'car)
      (error "Unknown internal error: ~S" name)))


(eval-when (compile eval)

(defmacro define-internal-errors (&rest errors)
  (let ((info (mapcar #'(lambda (x)
			  (if x
			      (cons (symbolicate (first x) "-ERROR")
				    (second x))
			      '(nil . "unused")))
		      errors)))
    `(progn
       (export ',(remove nil (mapcar #'car info)))
       (setf (c:backend-internal-errors c:*target-backend*)
	     ',(coerce info 'vector))
       nil)))

); eval-when


(define-internal-errors
  (unknown
   "Unknown.  System lossage.")
  (object-not-function
   "Object is not of type FUNCTION.")
  (object-not-list
   "Object is not of type LIST.")
  (object-not-bignum
   "Object is not of type BIGNUM.")
  (object-not-ratio
   "Object is not of type RATIO.")
  (object-not-single-float
   "Object is not of type SINGLE-FLOAT.")
  (object-not-double-float
   "Object is not of type DOUBLE-FLOAT.")
  #+long-float
  (object-not-long-float
   "Object is not of type LONG-FLOAT.")
  #+double-double
  (object-not-double-double-float
   "Object is not of type DOUBLE-DOUBLE-FLOAT.")
  (object-not-simple-string
   "Object is not of type SIMPLE-STRING.")
  (object-not-simple-bit-vector
   "Object is not of type SIMPLE-BIT-VECTOR.")
  (object-not-simple-vector
   "Object is not of type SIMPLE-VECTOR.")
  (object-not-fixnum
   "Object is not of type FIXNUM.")
  (object-not-function-or-symbol
   "Object is not of type FUNCTION or SYMBOL.")
  (object-not-vector
   "Object is not of type VECTOR.")
  (object-not-string
   "Object is not of type STRING.")
  (object-not-bit-vector
   "Object is not of type BIT-VECTOR.")
  (object-not-array
   "Object is not of type ARRAY.")
  (object-not-number
   "Object is not of type NUMBER.")
  (object-not-rational
   "Object is not of type RATIONAL.")
  (object-not-float
   "Object is not of type FLOAT.")
  (object-not-real
   "Object is not of type REAL.")
  (object-not-integer
   "Object is not of type INTEGER.")
  (object-not-cons
   "Object is not of type CONS.")
  (object-not-symbol
   "Object is not of type SYMBOL.")
  (undefined-symbol
   "Undefined symbol.")
  (object-not-coercable-to-function
   "Object is not coercable to type FUNCTION.")
  (invalid-argument-count
   "Invalid argument count.")
  (bogus-argument-to-values-list
   "Bogus argument to VALUES-LIST.")
  (unbound-symbol
   "Unbound symbol.")
  nil
  (object-not-sap
   "Object is not a System Area Pointer (SAP).")
  (invalid-unwind
   "Attempt to RETURN-FROM a block that no longer exists.")
  (unseen-throw-tag
   "Attempt to THROW to a non-existent tag.")
  (division-by-zero
   "Attempt to divide by zero.")
  (object-not-type
   "Object is of the wrong type.")
  (odd-keyword-arguments
   "Odd number of keyword arguments.")
  (unknown-keyword-argument
   "Unknown keyword.")
  nil
  nil
  (invalid-array-index
   "Invalid array index.")
  (wrong-number-of-indices
   "Wrong number of indices.")
  (object-not-simple-array
   "Object is not of type SIMPLE-ARRAY.")
  (object-not-signed-byte-32
   "Object is not of type (SIGNED-BYTE 32).")
  (object-not-unsigned-byte-32
   "Object is not of type (UNSIGNED-BYTE 32).")
  (object-not-simple-array-unsigned-byte-2
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 2) (*)).")
  (object-not-simple-array-unsigned-byte-4
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 4) (*)).")
  (object-not-simple-array-unsigned-byte-8
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).")
  (object-not-simple-array-unsigned-byte-16
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*)).")
  (object-not-simple-array-unsigned-byte-32
   "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)).")
  (object-not-simple-array-signed-byte-8
   "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 8) (*)).")
  (object-not-simple-array-signed-byte-16
   "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 16) (*)).")
  (object-not-simple-array-signed-byte-30
   "Object is not of type (SIMPLE-ARRAY FIXNUM (*)).")
  (object-not-simple-array-signed-byte-32
   "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)).")
  (object-not-simple-array-single-float
   "Object is not of type (SIMPLE-ARRAY SINGLE-FLOAT (*)).")
  (object-not-simple-array-double-float
   "Object is not of type (SIMPLE-ARRAY DOUBLE-FLOAT (*)).")
  #+long-float
  (object-not-simple-array-long-float
   "Object is not of type (SIMPLE-ARRAY LONG-FLOAT (*)).")
  #+double-double
  (object-not-simple-array-double-double-float
   "Object is not of type (SIMPLE-ARRAY DOUBLE-DOUBLE-FLOAT (*)).")
  (object-not-simple-array-complex-single-float
   "Object is not of type (SIMPLE-ARRAY (COMPLEX SINGLE-FLOAT) (*)).")
  (object-not-simple-array-complex-double-float
   "Object is not of type (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)).")
  #+long-float
  (object-not-simple-array-complex-long-float
   "Object is not of type (SIMPLE-ARRAY (COMPLEX LONG-FLOAT) (*)).")
  #+double-double
  (object-not-simple-array-complex-double-double-float
   "Object is not of type (SIMPLE-ARRAY (COMPLEX DOUBLE-DOUBLE-FLOAT) (*)).")
  (object-not-complex
   "Object is not of type COMPLEX.")
  (object-not-complex-rational
   "Object is not of type (COMPLEX RATIONAL).")
  (object-not-complex-float
   "Object is not of type (COMPLEX FLOAT).")
  (object-not-complex-single-float
   "Object is not of type (COMPLEX SINGLE-FLOAT).")
  (object-not-complex-double-float
   "Object is not of type (COMPLEX DOUBLE-FLOAT).")
  #+long-float
  (object-not-complex-long-float
   "Object is not of type (COMPLEX LONG-FLOAT).")
  #+double-double
  (object-not-complex-double-double-float
   "Object is not of type (COMPLEX DOUBLE-DOUBLE-FLOAT)")
  (object-not-weak-pointer
   "Object is not a WEAK-POINTER.")
  (object-not-instance
   "Object is not a INSTANCE.")
  (object-not-base-char
   "Object is not of type BASE-CHAR.")
  (nil-function-returned
   "Function with declared result type NIL returned.")
  (layout-invalid
   "Layout is invalid (instance obsolete.)")
  #+linkage-table
  (undefined-foreign-symbol
   "No value for foreign symbol."))
