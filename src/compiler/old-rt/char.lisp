;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the RT VM definition of character operations.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Moves and coercions:

;;; Move untagged string-char values.
;;;
(define-vop (string-char-move)
  (:args (x :target y
	    :scs (string-char-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (string-char-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (unless (location= x y)
      (inst lr y x))))
;;;
(define-move-vop string-char-move :move
  (string-char-reg) (string-char-reg))


;;; Move untagged string-char arguments/return-values.
;;;
(define-vop (move-string-char-argument)
  (:args (x :target y
	    :scs (string-char-reg))
	 (fp :scs (descriptor-reg)
	     :load-if (not (sc-is y string-char-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (string-char-reg
       (unless (location= x y)
	 (inst lr y x)))
      (string-char-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-string-char-argument :move-argument
  (string-char-reg) (string-char-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged string-char to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (string-char-reg) (any-reg descriptor-reg))


;;; Move a tagged string char to an untagged representation.
;;;
(define-vop (move-to-string-char)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (string-char-reg)))
  (:generator 1
    (inst nilz y x system:%character-code-mask)))
;;;
(define-move-vop move-to-string-char :move
  (any-reg descriptor-reg) (string-char-reg))


;;; Move an untagged string char to a tagged representation.
;;;
(define-vop (move-from-string-char)
  (:args (x :scs (string-char-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst oiu y x (ash system:%string-char-type clc::type-shift-16))))
;;;
(define-move-vop move-from-string-char :move
  (string-char-reg) (any-reg descriptor-reg))


;;;; Other operations:

(define-vop (char-code)
  (:args (ch :scs (string-char-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:arg-types string-char)
  (:translate char-code)
  (:policy :fast-safe)
  (:generator 0
    (unless (location= ch res)
      (inst lr res ch))))

(define-vop (code-char)
  (:args (code :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (string-char-reg)))
  (:result-types string-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:generator 0
    (unless (location= code res)
      (inst lr res code))))


(define-vop (pointer-compare)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition)
  (:generator 3
    (inst cl x y)
    (if not-p
	(inst bnb condition target)
	(inst bb condition target))))


;;; For comparison of string-chars, we require both operands to be in the
;;; untagged string-char-reg representation.  This will be a pessimization if
;;; both operands are tagged, but this won't happen often, and not in
;;; performance-critical cases.
;;;
(define-vop (string-char-compare pointer-compare)
  (:args (x :scs (string-char-reg))
	 (y :scs (string-char-reg)))
  (:arg-types string-char string-char))

(define-vop (fast-char=/string-char string-char-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char</string-char string-char-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char>/string-char string-char-compare)
  (:translate char>)
  (:variant :gt))

;;; If we don't know that both operands are string-chars, then we just compare
;;; the whole boxed object.  This assume that the hairy character type code is
;;; greater than the string-char type, since a string-char must always be less
;;; than a hairy char.
;;;
(define-vop (char-compare pointer-compare)
  (:variant-cost 5))

(define-vop (fast-char= char-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char< char-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char> char-compare)
  (:translate char>)
  (:variant :gt))
