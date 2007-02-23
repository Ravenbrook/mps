;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/amd64/pred.lisp,v 1.2 2004/07/06 20:22:07 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of predicate VOPs for the x86.
;;;
;;; Written by William Lott.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997.
;;; 

(in-package :amd64)


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst jmp dest)))


;;;; Conditional VOPs:

;;; Note: a constant-tn is NOT allowed in CMP.
(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg control-stack constant)
	    :load-if (not (and (sc-is x immediate)
			       (sc-is y any-reg descriptor-reg
				      control-stack constant))))
	 (y :scs (any-reg descriptor-reg immediate)
	    :load-if (not (and (sc-is x any-reg descriptor-reg immediate)
			       (sc-is y control-stack constant)))))
  (:temporary (:sc any-reg) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (cond 
     ((sc-is y immediate)
      (let ((val (tn-value y)))
	(etypecase val
	  (integer
	   (if (and (zerop val) (sc-is x any-reg descriptor-reg))
	       (inst test x x) ; smaller
	     (inst cmp x (fixnumize val))))
	  (symbol
	   (inst cmp x (+ nil-value (static-symbol-offset val))))
	  (character
	   (inst cmp x (logior (ash (char-code val) type-bits)
			       base-char-type))))))
     ((sc-is x immediate) ; and y not immediate
      ;; Swap the order to fit the compare instruction.
      (let ((val (tn-value x)))
	(etypecase val
	  (integer
	   (if (and (zerop val) (sc-is y any-reg descriptor-reg))
	       (inst test y y) ; smaller
	     (inst cmp y (fixnumize val))))
	  (symbol
	   (inst cmp y (+ nil-value (static-symbol-offset val))))
	  (character
	   (inst cmp y (logior (ash (char-code val) type-bits)
			       base-char-type))))))

     ;; y is a constant tn
     ((and (tn-p y) (eq (sb-name (sc-sb (tn-sc y)))
			'constant))
      (inst mov-imm temp (make-fixup nil
				     :code-object
				     (- (* (tn-offset y) word-bytes)
					other-pointer-type)))
      (inst cmp x (make-ea :qword :base temp)))

     ;; x is a constant tn
     ((and (tn-p x) (eq (sb-name (sc-sb (tn-sc x)))
			'constant))
      (inst mov-imm temp (make-fixup nil
				     :code-object
				     (- (* (tn-offset x) word-bytes)
					other-pointer-type)))
      (inst cmp (make-ea :qword :base temp) y))

     (t
      (inst cmp x y)))
    
    (inst jmp (if not-p :ne :e) target)))
