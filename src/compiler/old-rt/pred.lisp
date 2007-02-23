;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of predicate VOPs for the RT.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst bnb :pz dest)))


;;;; Conditional VOPs:

;if-true (???), if-eql, ...

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst c x y)
    (if not-p
	(inst bnb :eq target)
	(inst bb :eq target))))


(define-vop (if-eql two-arg-conditional-miscop)
  (:variant 'eql :eq)
  (:translate eql))
