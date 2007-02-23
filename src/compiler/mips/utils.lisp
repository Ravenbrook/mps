;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/utils.lisp,v 1.2 1994/10/31 04:44:16 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains various useful utilities for generating MIPS code.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; 

(in-package "MIPS")



;;;; Three Way Comparison

(defun three-way-comparison (x y condition flavor not-p target temp)
  (ecase condition
    (:eq
     (if not-p
	 (inst bne x y target)
	 (inst beq x y target)))
    (:lt
     (ecase flavor
       (:unsigned
	(inst sltu temp x y))
       (:signed
	(inst slt temp x y)))
     (if not-p
	 (inst beq temp zero-tn target)
	 (inst bne temp zero-tn target)))
    (:gt
     (ecase flavor
       (:unsigned
	(inst sltu temp y x))
       (:signed
	(inst slt temp y x)))
     (if not-p
	 (inst beq temp zero-tn target)
	 (inst bne temp zero-tn target))))
  (inst nop))


;;;; Pseudo-atomic support.

(defun start-pseudo-atomic ()
  ;; I don't think that we need to clear the interrupted slot.  It should
  ;; be clear already.
  ;(storew zero-tn mutator-tn mutator-pseudo-atomic-interrupted-slot)
  (storew csp-tn mutator-tn mutator-pseudo-atomic-atomic-slot))

(defun end-pseudo-atomic (ndescr)
  (let ((label (gen-label)))
    (storew zero-tn mutator-tn mutator-pseudo-atomic-atomic-slot)
    (loadw ndescr mutator-tn mutator-pseudo-atomic-interrupted-slot)
    (inst beq ndescr label)
    (inst nop)
    (inst break pending-interrupt-trap)
    (emit-label label)))



;;;; write-list support.

(defun check-pointer-ages-p (vop value)
  (and (sc-is value descriptor-reg)
       (let ((option (assoc :check-pointer-ages
			    (c::lexenv-options
			     (c::node-lexenv
			      (c::vop-node vop))))))
	 (or (null option)
	     (cdr option)))))
