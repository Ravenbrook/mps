;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/rt/array.lisp,v 1.6 2003/08/03 11:27:50 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/assembly/rt/array.lisp,v 1.6 2003/08/03 11:27:50 gerd Exp $
;;;
;;; This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;;

(in-package "RT")


(define-assembly-routine (allocate-vector
			  (:policy :fast-safe)
			  (:translate allocate-vector)
			  (:arg-types positive-fixnum
				      positive-fixnum
				      positive-fixnum))
			 ((:arg type any-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:arg words any-reg a2-offset)
			  (:res result descriptor-reg a0-offset)

			  (:temp ndescr non-descriptor-reg nl0-offset)
			  (:temp alloc word-pointer-reg ocfp-offset)
			  (:temp vector descriptor-reg cname-offset))
  (pseudo-atomic (ndescr)
    (load-symbol-value alloc *allocation-pointer*)
    (inst cal vector alloc vm:other-pointer-type)
    (inst cal alloc alloc (+ (1- (ash 1 vm:lowtag-bits))
			     (* vm:vector-data-offset vm:word-bytes)))
    (inst cas alloc alloc words)
    (inst li ndescr (lognot vm:lowtag-mask))
    (inst n alloc ndescr)
    (move ndescr type)
    (inst sr ndescr vm:word-shift)
    (storew ndescr vector 0 vm:other-pointer-type)
    (storew length vector vm:vector-length-slot vm:other-pointer-type)
    (store-symbol-value alloc *allocation-pointer*))
  (load-symbol-value ndescr *internal-gc-trigger*)
  (inst tlt ndescr alloc)
  (move result vector))



;;;; Hash primitives

(defparameter sxhash-simple-substring-entry (gen-label))

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:res result any-reg a0-offset)
			  (:temp length any-reg a1-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nargs-offset)
			  (:temp temp non-descriptor-reg ocfp-offset))
  (declare (ignore result accum data temp))
  (inst bx sxhash-simple-substring-entry)
  (loadw length string vm:vector-length-slot vm:other-pointer-type))


(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nargs-offset)
			  (:temp temp non-descriptor-reg ocfp-offset)
			  (:temp lip interior-reg lip-offset))
  (emit-label sxhash-simple-substring-entry)
  ;; Get a stack slot to save the return-pc.
  (inst inc csp-tn word-bytes)
  ;; Save the return-pc as a byte offset from the component start, shifted
  ;; left to look like a fixnum.
  (inst s lip code-tn)
  (inst sl lip 2)
  (storew lip csp-tn (* -1 word-bytes))
  ;; Compute start of string as interior pointer.
  (inst a lip string (- (* vector-data-offset word-bytes) other-pointer-type))
  (inst bx test)
  (inst li accum 0)
  LOOP
  (inst x accum data)
  (move temp accum)
  (inst sl temp 27)
  (inst sr accum 5)
  (inst o accum temp)
  (inst inc lip 4)
  TEST
  (inst s length (fixnumize 4))
  (inst bncx :lt loop)
  (loadw data lip)
  (inst a length (fixnumize 4))
  (inst bc :eq done)
  (inst neg length)
  (inst sl length 1)
  (inst nilz length #x1f)
  (inst sr data length)
  (inst x accum data)
  DONE
  ;; Give it fixnum low-tag bits.
  (inst sl accum 3)
  (move result accum)
  ;; Make it positive.
  (inst sr result 1)

  (loadw lip csp-tn (* -1 word-bytes))
  (inst dec csp-tn word-bytes)
  (inst sr lip 2)
  (inst cas lip lip code-tn))
