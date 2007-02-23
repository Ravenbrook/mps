;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/vm-typetran.lisp,v 1.17 2006/06/30 18:41:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;
;;; This file contains the implementation specific type transformation magic.
;;; Basically, the various non-standard predicates that can be used in typep
;;; transformations.
;;; 
;;; Written by William Lott.
;;;

(in-package "C")


;;;; Internal predicates:
;;;
;;;    These type predicates are used to implement simple cases of typep.  They
;;; shouldn't be used explicitly.

(define-type-predicate base-char-p base-char)
(define-type-predicate bignump bignum)
(define-type-predicate complex-double-float-p (complex double-float))
(define-type-predicate complex-single-float-p (complex single-float))
#+long-float
(define-type-predicate complex-long-float-p (complex long-float))
#+double-double
(define-type-predicate complex-double-double-float-p (complex double-double-float))
(define-type-predicate double-float-p double-float)
(define-type-predicate fixnump fixnum)
(define-type-predicate long-float-p long-float)
#+double-double
(define-type-predicate double-double-float-p double-double-float)
(define-type-predicate ratiop ratio)
(define-type-predicate short-float-p short-float)
(define-type-predicate single-float-p single-float)
(define-type-predicate simple-array-p simple-array)
(define-type-predicate simple-array-unsigned-byte-2-p
		       (simple-array (unsigned-byte 2) (*)))
(define-type-predicate simple-array-unsigned-byte-4-p
		       (simple-array (unsigned-byte 4) (*)))
(define-type-predicate simple-array-unsigned-byte-8-p
		       (simple-array (unsigned-byte 8) (*)))
(define-type-predicate simple-array-unsigned-byte-16-p
		       (simple-array (unsigned-byte 16) (*)))
(define-type-predicate simple-array-unsigned-byte-32-p
		       (simple-array (unsigned-byte 32) (*)))
(define-type-predicate simple-array-signed-byte-8-p
		       (simple-array (signed-byte 8) (*)))
(define-type-predicate simple-array-signed-byte-16-p
		       (simple-array (signed-byte 16) (*)))
(define-type-predicate simple-array-signed-byte-30-p
		       (simple-array (signed-byte 30) (*)))
(define-type-predicate simple-array-signed-byte-32-p
		       (simple-array (signed-byte 32) (*)))
(define-type-predicate simple-array-single-float-p
		       (simple-array single-float (*)))
(define-type-predicate simple-array-double-float-p
		       (simple-array double-float (*)))
#+long-float
(define-type-predicate simple-array-long-float-p
		       (simple-array long-float (*)))
#+(and t double-double)
(define-type-predicate simple-array-double-double-float-p
		       (simple-array double-double-float (*)))

(define-type-predicate simple-array-complex-single-float-p
		       (simple-array (complex single-float) (*)))
(define-type-predicate simple-array-complex-double-float-p
		       (simple-array (complex double-float) (*)))
#+long-float
(define-type-predicate simple-array-complex-long-float-p
		       (simple-array (complex long-float) (*)))
#+(and t double-double)
(define-type-predicate simple-array-complex-double-double-float-p
		       (simple-array (complex double-double-float) (*)))
(define-type-predicate system-area-pointer-p system-area-pointer)
(define-type-predicate unsigned-byte-32-p (unsigned-byte 32))
(define-type-predicate signed-byte-32-p (signed-byte 32))
(define-type-predicate weak-pointer-p weak-pointer)
(define-type-predicate scavenger-hook-p scavenger-hook)
(define-type-predicate code-component-p code-component)
(define-type-predicate lra-p lra)
(define-type-predicate fdefn-p fdefn)

;;; Unlike the un-%'ed versions, these are true type predicates, accepting any
;;; type object.
;;;
(define-type-predicate %standard-char-p standard-char)
