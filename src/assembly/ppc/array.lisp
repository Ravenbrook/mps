;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/assembly/ppc/array.lisp,v 1.7 2006/01/18 15:21:26 rtoy Exp $
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; 
(in-package "PPC")


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
			  (:temp pa-flag non-descriptor-reg nl3-offset)
			  (:temp vector descriptor-reg a3-offset)
			  #+gencgc
			  (:temp temp non-descriptor-reg nl2-offset))
  (pseudo-atomic (pa-flag)
    (inst addi ndescr words (* (1+ vm:vector-data-offset) vm:word-bytes))
    (inst clrrwi ndescr ndescr lowtag-bits)
    (allocation vector ndescr other-pointer-type
		#+gencgc :temp-tn #+gencgc temp
		#+gencgc :flag-tn #+gencgc pa-flag)
    (inst srwi ndescr type vm:word-shift)
    (storew ndescr vector 0 vm:other-pointer-type)
    (storew length vector vm:vector-length-slot vm:other-pointer-type))
  ;; Like the sparc port, we need to touch the last word to make sure
  ;; this page is paged in.  Do we need to touch all pages?
  ;;
  ;; FIXME: This seems to cause some problems.  Turn it off for now.
  ;; (storew zero-tn alloc-tn -1)
  (move result vector))



;;;; Hash primitives

#+assembler
(defparameter sxhash-simple-substring-entry (gen-label))

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:res result any-reg a0-offset)

			  (:temp length any-reg a1-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp temp non-descriptor-reg nl2-offset)
			  (:temp offset non-descriptor-reg nl3-offset))

  (declare (ignore result accum data temp offset))

  (loadw length string vm:vector-length-slot vm:other-pointer-type)
  (inst b sxhash-simple-substring-entry))


#+nil
(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)

			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp temp non-descriptor-reg nl2-offset)
			  (:temp offset non-descriptor-reg nl3-offset))
  (emit-label sxhash-simple-substring-entry)

  (inst li offset (- (* vector-data-offset word-bytes) other-pointer-type))
  (move accum zero-tn)
  (inst b test)

  LOOP

  (inst xor accum accum data)
  (inst slwi temp accum 27)
  (inst srwi accum accum 5)
  (inst or accum accum temp)
  (inst addi offset offset 4)

  TEST

  (inst subic. length length (fixnumize 4))
  (inst lwzx data string offset)
  (inst bge loop)

  (inst addic. length length (fixnumize 4))
  (inst neg length length)
  (inst beq done)
  (inst slwi length length 1)
  (inst srw data data length)
  (inst xor accum accum data)

  DONE

  (inst slwi result accum 5)
  (inst srwi result result 3))

;; One-at-a-time hash algorithm.  See assembly/sparc/array.lisp for a
;; description.

(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)

			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp temp non-descriptor-reg nl2-offset)
			  (:temp offset non-descriptor-reg nl3-offset))
  (emit-label sxhash-simple-substring-entry)

  (inst li offset (- (* vm:vector-data-offset vm:word-bytes) vm:other-pointer-type))
  (move accum zero-tn)
  (inst b test)

  LOOP

  ;; hash += key[i]
  (inst add accum accum data)
  ;; hash += (hash << 10)
  (inst slwi temp accum 10)
  (inst add accum accum temp)
  
  ;; hash ^= (hash >> 6)
  (inst srwi temp accum 6)
  (inst xor accum accum temp)
  (inst addi offset offset 1)

  TEST

  (inst subic. length length (fixnumize 1))
  (inst lbzx data string offset)
  (inst bge loop)
  
  ;; hash += (hash << 3)
  (inst slwi temp accum 3)
  (inst add accum accum temp)
  
  ;; hash ^= (hash >> 11)
  (inst srwi temp accum 11)
  (inst xor accum accum temp)
  
  ;; hash += (hash << 15);
  (inst slwi temp accum 15)
  (inst add accum accum temp)
  
  ;; Make the result a positive fixnum.  Shifting it left, then right
  ;; does what we want, and extracts the bits we need.
  (inst slwi accum accum 3)
  (inst srwi result accum 1))
