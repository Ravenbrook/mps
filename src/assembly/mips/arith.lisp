;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/mips/arith.lisp,v 1.17 2003/08/03 11:27:51 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;
;;; Written by William Lott.
;;;

(in-package "MIPS")


(define-assembly-routine (generic-+
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate +)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  #-gengc (:temp lra descriptor-reg lra-offset)
			  #+gengc (:temp ra any-reg ra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FUN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)
  (inst add res x y)
  #+gengc
  (progn
    (inst addu lip ra (* 2 word-bytes))
    (inst j lip)
    (inst nop))
  #-gengc
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-function-offset 'two-arg-+))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn))


(define-assembly-routine (generic--
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate -)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  #-gengc (:temp lra descriptor-reg lra-offset)
			  #+gengc (:temp ra any-reg ra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FUN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)
  (inst sub res x y)
  #+gengc
  (progn
    (inst addu lip ra (* 2 word-bytes))
    (inst j lip)
    (inst nop))
  #-gengc
  (lisp-return lra lip :offset 2)

  DO-STATIC-FUN
  (inst lw lip null-tn (static-function-offset 'two-arg--))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn))


(define-assembly-routine (generic-*
			  (:cost 25)
			  (:return-style :full-call)
			  (:translate *)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lo non-descriptor-reg nl1-offset)
			  (:temp hi non-descriptor-reg nl2-offset)
			  #-gengc (:temp pa-flag non-descriptor-reg nl4-offset)
			  (:temp lip interior-reg lip-offset)
			  #-gengc (:temp lra descriptor-reg lra-offset)
			  #+gengc (:temp ra any-reg ra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FUN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FUN)
  (inst nop)

  ;; Remove the tag from one arg so that the result will have the correct
  ;; fixnum tag.
  (inst sra temp x 2)
  (inst mult temp y)
  (inst mflo res)
  (inst mfhi hi)
  ;; Check to see if the result will fit in a fixnum.  (I.e. the high word
  ;; is just 32 copies of the sign bit of the low word).
  (inst sra temp res 31)
  (inst xor temp hi)
  (inst beq temp DONE)
  ;; Shift the double word hi:res down two bits into hi:low to get rid of the
  ;; fixnum tag.
  (inst srl lo res 2)
  (inst sll temp hi 30)
  (inst or lo temp)
  (inst sra hi 2)

  ;; Do we need one word or two?  Assume two.
  (inst sra temp lo 31)
  (inst xor temp hi)
  (inst bne temp two-words)
  ;; Assume a two word header.
  (inst li temp (logior (ash 2 type-bits) bignum-type))

  ;; Only need one word, fix the header.
  (inst li temp (logior (ash 1 type-bits) bignum-type))

  #-gengc
  (pseudo-atomic (pa-flag :extra (pad-data-block (+ 1 bignum-digits-offset)))
    (inst or res alloc-tn other-pointer-type)
    (storew temp res 0 other-pointer-type))
  #+gengc
  (without-scheduling ()
    (inst or res alloc-tn other-pointer-type)
    (storew temp alloc-tn)
    (inst addu alloc-tn (pad-data-block (+ 1 bignum-digits-offset))))

  (storew lo res bignum-digits-offset other-pointer-type)

  ;; Out of here
  #+gengc
  (progn
    (inst addu lip ra (* 2 word-bytes))
    (inst j lip)
    (inst nop))
  #-gengc
  (lisp-return lra lip :offset 2)


  TWO-WORDS
  #-gengc
  (pseudo-atomic (pa-flag :extra (pad-data-block (+ 2 bignum-digits-offset)))
    (inst or res alloc-tn other-pointer-type)
    (storew temp res 0 other-pointer-type))
  #+gengc
  (without-scheduling ()
    (inst or res alloc-tn other-pointer-type)
    (storew temp alloc-tn)
    (inst addu alloc-tn (pad-data-block (+ 2 bignum-digits-offset))))

  (storew lo res bignum-digits-offset other-pointer-type)
  (storew hi res (1+ bignum-digits-offset) other-pointer-type)

  ;; Out of here
  #+gengc
  (progn
    (inst addu lip ra (* 2 word-bytes))
    (inst j lip)
    (inst nop))
  #-gengc
  (lisp-return lra lip :offset 2)


  DO-STATIC-FUN
  (inst lw lip null-tn (static-function-offset 'two-arg-*))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn)

  DONE)



;;;; Comparison routines.

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cmp not-p)
       `(define-assembly-routine (,name
				  (:cost 10)
				  (:return-style :full-call)
				  (:policy :safe)
				  (:translate ,translate)
				  (:save-p t))
				 ((:arg x (descriptor-reg any-reg) a0-offset)
				  (:arg y (descriptor-reg any-reg) a1-offset)
				  
				  (:res res descriptor-reg a0-offset)
				  
				  (:temp temp non-descriptor-reg nl0-offset)
				  (:temp lip interior-reg lip-offset)
				  (:temp nargs any-reg nargs-offset)
				  (:temp ocfp any-reg ocfp-offset))
	  (inst and temp x 3)
	  (inst bne temp DO-STATIC-FN)
	  (inst and temp y 3)
	  (inst beq temp DO-COMPARE)
	  ,cmp
	  
	  DO-STATIC-FN
	  (inst lw lip null-tn (static-function-offset ',static-fn))
	  (inst li nargs (fixnumize 2))
	  (inst move ocfp cfp-tn)
	  (inst j lip)
	  (inst move cfp-tn csp-tn)
	  
	  DO-COMPARE
	  (inst ,(if not-p 'bne 'beq) temp done)
	  (inst move res null-tn)
	  (load-symbol res t)
	  DONE)))

  (define-cond-assem-rtn generic-< < two-arg-< (inst slt temp x y) nil)
  (define-cond-assem-rtn generic-> > two-arg-> (inst slt temp y x) nil))


(define-assembly-routine (generic-eql
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate eql)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)
			  
			  (:res res descriptor-reg a0-offset)
			  
			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  #-gengc (:temp lra descriptor-reg lra-offset)
			  #+gengc (:temp ra any-reg ra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst beq x y RETURN-T)
  (inst and temp x 3)
  (inst beq temp RETURN-NIL)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FN)
  (inst nop)

  RETURN-NIL
  (inst move res null-tn)
  #+gengc
  (progn
    (inst addu lip ra (* 2 word-bytes))
    (inst j lip)
    (inst nop))
  #-gengc
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst lw lip null-tn (static-function-offset 'eql))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate =)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)
			  
			  (:res res descriptor-reg a0-offset)
			  
			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  #-gengc (:temp lra descriptor-reg lra-offset)
			  #+gengc (:temp ra any-reg ra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FN)
  (inst nop)
  (inst beq x y RETURN-T)

  (inst move res null-tn)
  #+gengc
  (progn
    (inst addu lip ra (* 2 word-bytes))
    (inst j lip)
    (inst nop))
  #-gengc
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst lw lip null-tn (static-function-offset 'two-arg-=))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-/=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate /=)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)
			  
			  (:res res descriptor-reg a0-offset)
			  
			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  #-gengc (:temp lra descriptor-reg lra-offset)
			  #+gengc (:temp ra any-reg ra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst and temp x 3)
  (inst bne temp DO-STATIC-FN)
  (inst and temp y 3)
  (inst bne temp DO-STATIC-FN)
  (inst nop)
  (inst beq x y RETURN-NIL)

  (load-symbol res t)
  #+gengc
  (progn
    (inst addu lip ra (* 2 word-bytes))
    (inst j lip)
    (inst nop))
  #-gengc
  (lisp-return lra lip :offset 2)

  DO-STATIC-FN
  (inst lw lip null-tn (static-function-offset 'two-arg-=))
  (inst li nargs (fixnumize 2))
  (inst move ocfp cfp-tn)
  (inst j lip)
  (inst move cfp-tn csp-tn)

  RETURN-NIL
  (inst move res null-tn))
