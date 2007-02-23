;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the RT VM definition of operand loading/saving and
;;; the Move VOP.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Move functions:

(define-move-function (load-immediate 1) (node x y)
  ((short-immediate unsigned-immediate immediate random-immediate)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (loadi y val))
      (null
       (inst cau y zero-tn clc::nil-16))
      ((member t)
       (inst cau y zero-tn clc::t-16)))))

(define-move-function (load-string-char 1) (node x y)
  ((immediate-string-char) (string-char-reg))
  (loadi y (char-code (tn-value x))))

(define-move-function (load-tagged-string-char 2) (node x y)
  ((immediate-string-char) (any-reg descriptor-reg))
  (loadi y (char-code (tn-value x)))
  (inst oiu y y (ash system:%string-char-type clc::type-shift-16)))

(define-move-function (load-constant 5) (node x y)
  ((constant) (any-reg descriptor-reg))
  (load-slot y env-tn (tn-offset x)))

(define-move-function (load-stack 5) (node x y)
  ((stack) (any-reg descriptor-reg)
   (string-char-stack) (string-char-reg))
  (load-stack-tn y x))

(define-move-function (store-stack 5) (node x y)
  ((any-reg descriptor-reg) (stack)
   (string-char-reg) (string-char-stack))
  (store-stack-tn y x))


;;;; Move and Move-Argument VOPs:


;;;    The Move VOP is used for doing arbitrary moves when there is no
;;; type-specific move/coerce operation.
;;;
(define-vop (move)
  (:args (x :target y
	    :scs (any-reg descriptor-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (unless (location= x y)
      (inst lr y x))))


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
	    :scs (any-reg descriptor-reg))
	 (fp :scs (descriptor-reg)
	     :load-if (not (sc-is y any-reg descriptor-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (unless (location= x y)
	 (inst lr y x)))
      (stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-argument :move-argument
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))
