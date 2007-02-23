;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/mips/array.lisp,v 1.25 2003/08/03 11:27:51 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;; 
(in-package "MIPS")

#-gengc
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
			  (:temp pa-flag non-descriptor-reg nl4-offset))
  ;; This is kinda sleezy, changing words like this.  But we can because
  ;; the vop thinks it is temporary.
  (inst addu words (+ (1- (ash 1 lowtag-bits))
		      (* vector-data-offset word-bytes)))
  (inst li ndescr (lognot lowtag-mask))
  (inst and words ndescr)
  (inst srl ndescr type word-shift)

  (pseudo-atomic (pa-flag)
    (inst or result alloc-tn other-pointer-type)
    (inst addu alloc-tn words)
    (storew ndescr result 0 other-pointer-type)
    (storew length result vector-length-slot other-pointer-type)))


;;;; Hash primitives

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:res result any-reg a0-offset)

			  (:temp length any-reg a1-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp byte non-descriptor-reg nl2-offset)
			  #-gengc
			  (:temp retaddr non-descriptor-reg nl3-offset))

  ;; These are needed after we jump into sxhash-simple-substring.
  (progn result lip accum data byte #-gengc retaddr)

  (inst j (make-fixup 'sxhash-simple-substring :assembly-routine))
  (loadw length string vector-length-slot other-pointer-type))

(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nl1-offset)
			  (:temp byte non-descriptor-reg nl2-offset)
			  #-gengc
			  (:temp retaddr non-descriptor-reg nl3-offset))

  ;; Save the return address
  #-gengc (inst subu retaddr lip code-tn)

  ;; Get a pointer to the data.
  (inst addu lip string
	(- (* vector-data-offset word-bytes) other-pointer-type))
  (inst b test)
  (move accum zero-tn)

  loop

  (inst and byte data #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 8)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 16)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst srl byte data 24)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  (inst addu lip lip 4)

  test

  (inst addu length length (fixnumize -4))
  (inst lw data lip 0)
  (inst bgez length loop)
  (inst nop)

  (inst addu length length (fixnumize 3))
  (inst beq length zero-tn one-more)
  (inst addu length length (fixnumize -1))
  (inst beq length zero-tn two-more)
  (inst addu length length (fixnumize -1))
  (inst bne length zero-tn done)
  (inst nop)

  #+sgi  (inst srl byte data 8)
  #+pmax (inst srl byte data 16)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  two-more

  #+sgi  (inst srl byte data 16)
  #+pmax (inst srl byte data 8)
  (inst and byte byte #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  one-more

  #+sgi (inst srl data data 24)
  (inst and byte data #xff)
  (inst xor accum accum byte)
  (inst sll byte accum 5)
  (inst srl accum accum 27)
  (inst or accum accum byte)

  done

  (inst sll result accum 5)
  (inst srl result result 3)

  ;; Restore the return address.
  #-gengc (inst addu lip code-tn retaddr))
