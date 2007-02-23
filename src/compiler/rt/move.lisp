;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/move.lisp,v 1.7 2003/08/03 11:27:47 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/move.lisp,v 1.7 2003/08/03 11:27:47 gerd Exp $
;;;
;;; This file contains the IBM RT VM definition of operand loading/saving and the
;;; Move VOP.
;;;
;;; Written by Rob MacLachlan.
;;; MIPS conversion by William Lott.
;;; IBM RT conversion by William Lott and Bill Chiles.
;;;

(in-package "RT")


(define-move-function (load-immediate 1) (vop x y)
  ((null immediate)
   (any-reg word-pointer-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst li y (fixnumize val)))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val))
      (character
       (inst li y (logior (ash (char-code val) type-bits)
			  base-char-type))))))

(define-move-function (load-number 1) (vop x y)
  ((immediate)
   (signed-reg unsigned-reg))
  (inst li y (tn-value x)))

(define-move-function (load-base-char 1) (vop x y)
  ((immediate) (base-char-reg))
  (inst li y (char-code (tn-value x))))

(define-move-function (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (inst li y (sap-int (tn-value x))))

(define-move-function (load-constant 5) (vop x y)
  ((constant) (descriptor-reg))
  (loadw y code-tn (tn-offset x) other-pointer-type))

(define-move-function (load-stack 5) (vop x y)
  ((control-stack) (any-reg word-pointer-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-function (load-number-stack 5) (vop x y)
  ((base-char-stack) (base-char-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (load-stack-tn y x vop))

(define-move-function (store-stack 5) (vop x y)
  ((any-reg word-pointer-reg descriptor-reg) (control-stack))
  (store-stack-tn x y))

(define-move-function (store-number-stack 5) (vop x y)
  ((base-char-reg) (base-char-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (store-stack-tn x y vop))

(define-move-function (word-pointer-copy 1) (vop x y)
  ((word-pointer-reg) (any-reg)
   (any-reg) (word-pointer-reg))
  (move y x))



;;;; The Move VOP:

(define-vop (move)
  (:args (x :target y
	    :scs (any-reg word-pointer-reg descriptor-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (any-reg word-pointer-reg descriptor-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))

(define-move-vop move :move
  (any-reg word-pointer-reg descriptor-reg)
  (any-reg word-pointer-reg descriptor-reg))

;;; Make Move the check VOP for T so that type check generation doesn't think
;;; it is a hairy type.  This also allows checking of a few of the values in a
;;; continuation to fall out.
;;;
(primitive-type-vop move (:check) t)

;;; MOVE-ARGUMENT -- VOP.
;;;
;;; This is used for moving descriptor values into another frame for argument
;;; or known value passing.
;;;
(define-vop (move-argument)
  (:args (x :target y
	    :scs (any-reg word-pointer-reg descriptor-reg))
	 (fp :scs (word-pointer-reg)
	     :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg word-pointer-reg descriptor-reg)
       (move y x))
      (control-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-argument :move-argument
  (any-reg word-pointer-reg descriptor-reg)
  (any-reg word-pointer-reg descriptor-reg))



;;;; ILLEGAL-MOVE

;;; ILLEGAL-MOVE -- VOP.
;;;
;;; This VOP exists just to begin the lifetime of a TN that couldn't be written
;;; legally due to a type error.  An error is signalled before this VOP is so
;;; we don't need to do anything (not that there would be anything sensible to
;;; do anyway.)
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

;;; MOVE-TO-WORD/FIXNUM -- VOP.
;;;
;;; Arg is a fixnum, so just put it in a non-descriptor register and shift it.
;;;
(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg) :target y))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 2
    (move y x)
    (inst sar y 2)))
;;;
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; MOVE-TO-WORD-C -- VOP.
;;; 
;;; Arg is a non-immediate constant, load it.
;;;
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "constant load")
  (:generator 1
    (inst li y (tn-value x))))
;;;
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; MOVE-TO-WORD/INTEGER -- VOP.
;;;
;;; Arg is a fixnum or bignum, figure out which and load if necessary.
;;;
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg) :to :save))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 8
    (let ((done (gen-label)))
      (inst nilz temp x 3)
      (move y x)
      (inst bcx :eq done)
      (inst sar y 2)
      ;; If it's a bignum, throw away what we computed in y.
      (loadw y x bignum-digits-offset other-pointer-type)
      (emit-label done))))
;;;
(define-move-vop move-to-word/integer :move
  (descriptor-reg) (signed-reg unsigned-reg))


;;; MOVE-FROM-WORD/FIXNUM -- VOP.
;;;
;;; Since the result is know to be a fixnum, we can shift in the tag bits
;;; without fear of needing a bignum.
;;;
(define-vop (move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg) :target temp))
  (:temporary (:scs (non-descriptor-reg)
		    :from (:argument 0) :to (:result 0) :target y)
	      temp)
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "fixnum tagging")
  (:generator 3
    (move temp x)
    (inst sl temp 2)
    (move y temp)))
;;;
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

;;; MOVE-FROM-SIGNED -- VOP.
;;;
;;; Result may be a bignum, so we have to check whether shifting to make room
;;; for tag bits results in a fixnum.  Use a worst-case cost to make sure
;;; people know they may be number consing.
;;;
(define-vop (move-from-signed)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:scs (word-pointer-reg)) alloc)
  (:note "signed word to integer coercion")
  (:generator 25
    (move x arg)
    (let ((fixnum (gen-label))
	  (done (gen-label)))
      (move temp x)
      (inst sar temp 29)
      (inst bcx :eq fixnum)
      (inst not temp)
      (inst bc :eq fixnum)
      
      (with-fixed-allocation (y temp alloc bignum-type 2)
	(storew x y bignum-digits-offset other-pointer-type))
      (inst b done)
      
      (emit-label fixnum)
      (move y x)
      (inst sl y 2)
      (emit-label done))))
;;;
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))


;;; MOVE-FROM-UNSIGNED -- VOP.
;;;
;;; Check for fixnum, and possibly allocate one or two word bignum result.  Use
;;; a worst-case cost to make sure people know they may be number consing.
;;;
(define-vop (move-from-unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:temporary (:scs (word-pointer-reg)) alloc)
  (:note "unsigned word to integer coercion")
  (:generator 21
    (move x arg)
    (let ((done (gen-label))
	  (bignum (gen-label))
	  (one-word (gen-label)))
      (move temp x)
      (inst sar temp 29)
      (inst bnc :eq bignum)
      (inst sl x 2)
      (move y x)
      (emit-label done)

      (assemble (*elsewhere*)
	(emit-label bignum)
	(pseudo-atomic (temp)
	  (load-symbol-value alloc *allocation-pointer*)
	  (inst cal y alloc other-pointer-type)
	  (inst cal alloc alloc (pad-data-block (1+ bignum-digits-offset)))
	  (inst c x 0)
	  (inst bncx :lt one-word)
	  (inst li temp (logior (ash 1 type-bits) bignum-type))
	  (inst cal alloc alloc
		(- (pad-data-block (+ 2 bignum-digits-offset))
		   (pad-data-block (1+ bignum-digits-offset))))
	  (inst li temp (logior (ash 2 type-bits) bignum-type))
	  (emit-label one-word)
	  (store-symbol-value alloc *allocation-pointer*)
	  (storew temp y 0 other-pointer-type)
	  (storew x y bignum-digits-offset other-pointer-type))
	(load-symbol-value temp *internal-gc-trigger*)
	(inst tlt temp alloc)
	(inst b done)))))
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
	 (fp :scs (word-pointer-reg)
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
