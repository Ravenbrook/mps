;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/ppc/move.lisp,v 1.7 2005/06/15 03:13:51 rtoy Exp $
;;;
;;;    This file contains the SPARC VM definition of operand loading/saving and
;;; the Move VOP.
;;;
;;; Written by Rob MacLachlan.
;;; SPARC conversion by William Lott.
;;;
(in-package "PPC")


(define-move-function (load-immediate 1) (vop x y)
  ((null immediate zero)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst lr y (fixnumize val)))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val))
      (character
       (inst lr y (logior (ash (char-code val) type-bits)
			  base-char-type))))))

(define-move-function (load-number 1) (vop x y)
  ((immediate zero)
   (signed-reg unsigned-reg))
  (inst lr y (tn-value x)))

(define-move-function (load-base-char 1) (vop x y)
  ((immediate) (base-char-reg))
  (inst li y (char-code (tn-value x))))

(define-move-function (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (inst lr y (sap-int (tn-value x))))

(define-move-function (load-constant 5) (vop x y)
  ((constant) (descriptor-reg))
  (loadw y code-tn (tn-offset x) other-pointer-type))

(define-move-function (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-function (load-number-stack 5) (vop x y)
  ((base-char-stack) (base-char-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (let ((nfp (current-nfp-tn vop)))
    (loadw y nfp (tn-offset x))))

(define-move-function (store-stack 5) (vop x y)
  ((any-reg descriptor-reg) (control-stack))
  (store-stack-tn y x))

(define-move-function (store-number-stack 5) (vop x y)
  ((base-char-reg) (base-char-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (let ((nfp (current-nfp-tn vop)))
    (storew x nfp (tn-offset y))))


;;;; The Move VOP:
;;;
(define-vop (move)
  (:args (x :target y
	    :scs (any-reg descriptor-reg zero null)
	    :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))

(define-move-vop move :move
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;; Make Move the check VOP for T so that type check generation doesn't think
;;; it is a hairy type.  This also allows checking of a few of the values in a
;;; continuation to fall out.
;;;
(primitive-type-vop move (:check) t)

;;;    The Move-Argument VOP is used for moving descriptor values into another
;;; frame for argument or known value passing.
;;;
(define-vop (move-argument)
  (:args (x :target y
	    :scs (any-reg descriptor-reg zero null))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (move y x))
      (control-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-argument :move-argument
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))



;;;; ILLEGAL-MOVE

;;; This VOP exists just to begin the lifetime of a TN that couldn't be written
;;; legally due to a type error.  An error is signalled before this VOP is
;;; so we don't need to do anything (not that there would be anything sensible
;;; to do anyway.)
;;;
(define-vop (illegal-move)
  (:args (x) (type))
  (:results (y))
  (:ignore y)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 666
    (error-call vop object-not-type-error x type)))



;;;; Moves and coercions:

;;; These MOVE-TO-WORD VOPs move a tagged integer to a raw full-word
;;; representation.  Similarly, the MOVE-FROM-WORD VOPs converts a raw integer
;;; to a tagged bignum or fixnum.

;;; Arg is a fixnum, so just shift it.  We need a type restriction because some
;;; possible arg SCs (control-stack) overlap with possible bignum arg SCs.
;;;
(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 1
    (inst srawi y x 2)))
;;;
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; Arg is a non-immediate constant, load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "constant load")
  (:generator 1
    (inst lr y (tn-value x))))
;;;
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))


;;; Arg is a fixnum or bignum, figure out which and load if necessary.
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (let ((done (gen-label)))
      (inst andi. temp x 3)
      (sc-case y
	(signed-reg
	 (inst srawi y x 2))
	(unsigned-reg
	 (inst srwi y x 2)))
      
      (inst beq done)
      (loadw y x bignum-digits-offset other-pointer-type)
      
      (emit-label done))))
;;;
(define-move-vop move-to-word/integer :move
  (descriptor-reg) (signed-reg unsigned-reg))



;;; Result is a fixnum, so we can just shift.  We need the result type
;;; restriction because of the control-stack ambiguity noted above.
;;;
(define-vop (move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "fixnum tagging")
  (:generator 1
    (inst slwi y x 2)))
;;;
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))


;;; Result may be a bignum, so we have to check.  Use a worst-case cost to make
;;; sure people know they may be number consing.
;;;
(define-vop (move-from-signed)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "signed word to integer coercion")
  (:generator 20
    (move x arg)
    (let ((done (gen-label)))
      (inst mtxer zero-tn)              ; clear sticky overflow bit in XER, CR0
      (inst addo temp x x)              ; set XER OV if top two bits differ
      (inst addo. temp temp temp)       ; set CR0 SO if any top three bits differ
      (inst slwi y x 2)                 ; assume fixnum (tagged ok, maybe lost some high bits)
      (inst bns done)
      
      (with-fixed-allocation (y pa-flag temp bignum-type (1+ bignum-digits-offset))
	(storew x y bignum-digits-offset other-pointer-type))
      (emit-label done))))
;;;
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))


;;; Check for fixnum, and possibly allocate one or two word bignum result.  Use
;;; a worst-case cost to make sure people know they may be number consing.
;;;
(define-vop (move-from-unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (move x arg)
    (let ((done (gen-label))
	  (one-word (gen-label)))
      (inst srawi. temp x 29)
      (inst slwi y x 2)
      (inst beq done)

      ;; We always allocate 2 words even if we don't need it.  (The
      ;; copying GC will take care of freeing the unused extra word.)
      (with-fixed-allocation
	  (y pa-flag temp bignum-type (+ 2 bignum-digits-offset))
	(inst cmpwi x 0)
	(inst li temp (logior (ash 1 type-bits) bignum-type))
	(inst bge one-word)
	(inst li temp (logior (ash 2 type-bits) bignum-type))
	(emit-label one-word)
	(storew temp y 0 other-pointer-type)
	(storew x y bignum-digits-offset other-pointer-type))
      (emit-label done))))
;;;
(define-move-vop move-from-unsigned :move
  (unsigned-reg) (descriptor-reg))


;;; Move untagged numbers.
;;;
(define-vop (word-move)
  (:args (x :target y
	    :scs (signed-reg unsigned-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (signed-reg unsigned-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:note "word integer move")
  (:generator 0
    (move y x)))
;;;
(define-move-vop word-move :move
  (signed-reg unsigned-reg) (signed-reg unsigned-reg))


;;; Move untagged number arguments/return-values.
;;;
(define-vop (move-word-argument)
  (:args (x :target y
	    :scs (signed-reg unsigned-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note "word integer argument move")
  (:generator 0
    (sc-case y
      ((signed-reg unsigned-reg)
       (move y x))
      ((signed-stack unsigned-stack)
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-word-argument :move-argument
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged number to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (signed-reg unsigned-reg) (any-reg descriptor-reg))
