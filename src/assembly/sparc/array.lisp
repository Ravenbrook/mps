;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/sparc/array.lisp,v 1.10 2004/04/07 02:47:53 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/assembly/sparc/array.lisp,v 1.10 2004/04/07 02:47:53 rtoy Exp $
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; 
(in-package "SPARC")


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
			  (:temp gc-temp non-descriptor-reg nl1-offset)
			  (:temp vector descriptor-reg a3-offset))
  (pseudo-atomic ()
    (inst add ndescr words (* (1+ vm:vector-data-offset) vm:word-bytes))
    (inst andn ndescr vm:lowtag-mask)
    (allocation vector ndescr other-pointer-type :temp-tn gc-temp)
    #+gencgc
    (progn
      ;; ndescr points to one word past the end of the allocated
      ;; space.  Fill the last word with a zero.
      (inst add ndescr vector)
      (storew zero-tn ndescr -1 vm:other-pointer-type))
    (inst srl ndescr type vm:word-shift)
    (storew ndescr vector 0 vm:other-pointer-type)
    (storew length vector vm:vector-length-slot vm:other-pointer-type))
  ;; This makes sure the zero byte at the end of a string is paged in so
  ;; the kernel doesn't bitch if we pass it the string.
  ;;
  ;; This used to write to the word after the last allocated word.  I
  ;; (RLT) made it write to the last allocated word, which is where
  ;; the zero-byte of the string is.  Look at the deftransform for
  ;; make-array in array-tran.lisp.  For strings we always allocate
  ;; enough space to hold the zero-byte.
  #-gencgc
  (storew zero-tn alloc-tn -1)
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

  (inst b sxhash-simple-substring-entry)
  (loadw length string vm:vector-length-slot vm:other-pointer-type))


;; Implement the one-at-a-time algorithm designed by Bob Jenkins
;; (see <http://burtleburtle.net/bob/hash/doobs.html> for some
;; more information).
;;
;; For completeness, here is the hash function, in C, from that web
;; page.  ub4 is an unsigned 32-bit integer.

#||
ub4 one_at_a_time(char *key, ub4 len)
{
  ub4   hash, i;
  for (hash=0, i=0; i<len; ++i)
  {
    hash += key[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return (hash & mask);
} 

||#


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
  (inst b test)
  (move accum zero-tn)

  LOOP

  ;; hash += key[i]
  (inst add accum data)
  ;; hash += (hash << 10)
  (inst slln temp accum 10)
  (inst add accum temp)
  ;; hash ^= (hash >> 6)
  (inst srln temp accum 6)
  (inst xor accum temp)
  (inst add offset 1)
  
  TEST

  (inst subcc length (fixnumize 1))
  (inst b :ge loop)
  (inst ldub data string offset)

  ;; hash += (hash << 3)
  (inst slln temp accum 3)
  (inst add accum temp)
  ;; hash ^= (hash >> 11)
  (inst srln temp accum 11)
  (inst xor accum temp)
  ;; hash += (hash << 15)
  (inst slln temp accum 15)
  (inst add accum temp)
  
  ;;(inst li temp most-positive-fixnum)
  ;;(inst and accum temp)
  ;; Make it a fixnum result

  ;; Make the result a positive fixnum.  Shifting it left, then right
  ;; does what we want, and extracts the bits we need.
  (inst slln accum (1+ vm:fixnum-tag-bits))
  (inst srln result accum 1))
