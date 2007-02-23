;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/assembly/amd64/array.lisp,v 1.3 2004/07/27 23:28:41 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains various array operations that are too expensive
;;; (in space) to do inline.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski -- Spring 1995.
;;;
(in-package :amd64)


;;;; Allocation

(define-assembly-routine (allocate-vector
			  (:policy :fast-safe)
			  (:translate allocate-vector)
			  (:arg-types positive-fixnum
				      positive-fixnum
				      positive-fixnum))
			 ((:arg type unsigned-reg rax-offset)
			  (:arg length any-reg rbx-offset)
			  (:arg words any-reg rcx-offset)
			  (:temp temp any-reg r11-offset)
			  (:res result descriptor-reg rdx-offset))
  ;; compute the number of bytes
  ;; words is a fixnum
  (inst lea result (make-ea :byte :base words :index words :disp
			    (+ 15 (* vector-data-offset word-bytes))))
  (inst and result (lognot 15)) ; alignment
  (pseudo-atomic
   (allocation result result temp)
   (inst lea result (make-ea :byte :base result :disp other-pointer-type))
   (storew type result 0 other-pointer-type)
   (storew length result vector-length-slot other-pointer-type))
  (inst ret))



;;;; Hash primitives

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg rbx-offset)
			  (:res result any-reg rdx-offset)

			  (:temp length any-reg rdi-offset)
			  (:temp rsi unsigned-reg rsi-offset)
			  (:temp rcx unsigned-reg rcx-offset)
			  (:temp rax unsigned-reg rax-offset))
  (declare (ignore result rsi rcx rax))
  (loadw length string vector-length-slot other-pointer-type)
  (inst jmp (make-fixup 'sxhash-simple-substring :assembly-routine)))

(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg rbx-offset)
			  (:arg length any-reg rdi-offset)
			  (:res result any-reg rdx-offset)

			  (:temp rsi unsigned-reg rsi-offset)
			  (:temp rcx unsigned-reg rcx-offset)
			  (:temp rax unsigned-reg rax-offset))
  ;; Compute a pointer to where we are going to be extracting the bits.
  (inst lea rsi	(make-ea :byte :base string
			 :disp (- (* vector-data-offset word-bytes)
				  other-pointer-type)))
  ;; Initialize the result.
  (inst xor result result)
  ;; Get the count.  If it's zero, blow out.
  (inst mov rcx length)
  (inst jrcxz done)
  ;; Convert it into count of the number of full words.  If zero, then skip
  ;; to the part that handles the tail.
  (inst shr rcx 5) ; 2 tag bits + 3
  (inst jrcxz do-extra)
  ;; Clear the direction flag, so we advance through memory.
  (inst cld)

  LOOP
  ;; Merge each successive word with the result.
  (inst lods rax)			; load 64-bits into rax and (+8 rsi)

  (inst rol result 5)
  (inst xor result rax)
  (inst loop loop)

  DO-EXTRA
  ;; Now we have to take care of any bytes that don't make up a full word.
  ;; First, check to see how many of them there are.  If zero, blow out of
  ;; here.  Otherwise, multiply by 8.
  (inst mov rcx length)
  (inst and rcx (fixnumize 7))
  (inst jrcxz done)
  ;; some bytes are left
  (inst shl rcx 1)

  ;; Grab the last word.
  (inst lods rax)

  ;; Convert the count into a mask.  The count is multiplied by 8, so we just
  ;; shift -1 left, which shifts count*8 zeros into the low order end.  We
  ;; then invert that, ending up with a mask of count*8 ones.
  (inst mov rsi -1)
  (inst shl rsi :cl)
  (inst not rsi)
  ;; Use the mask to strip off the bits we arn't interested in, and merge
  ;; the remaining bits with the result.
  (inst and rax rsi)
  (inst rol result 5)
  (inst xor result rax)

  DONE

  ;; Force result to be a 32-bit positive fixnum because hash-vector is a
  ;; 32-bit vector. This hash function is not good. We need to use the
  ;; upper half of the word and change the hash-vector.
  (inst and result #x7ffffffc)
  (inst ret))
