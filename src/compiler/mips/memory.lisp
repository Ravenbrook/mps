;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/memory.lisp,v 1.16 1994/10/31 04:44:16 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the MIPS definitions of some general purpose memory
;;; reference VOPs inherited by basic memory reference operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "MIPS")


;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg null zero)))
  (:variant-vars offset lowtag)
  #+gengc (:info remember)
  (:policy :fast-safe)
  (:generator 4
    #+gengc
    (if remember
	(storew-and-remember-slot value object offset lowtag)
	(storew value object offset lowtag))
    #-gengc
    (storew value object offset lowtag)))

;;; Slot-Ref and Slot-Set are used to define VOPs like Closure-Ref, where the
;;; offset is constant at compile time, but varies for different uses.  We add
;;; in the stardard g-vector overhead.
;;;
(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (loadw value object (+ base offset) lowtag)))
;;;
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg null zero)))
  (:variant-vars base lowtag)
  (:info offset #+gengc remember)
  (:generator 4
    #+gengc
    (if remember
	(storew-and-remember-slot value object (+ base offset) lowtag)
	(storew value object (+ base offset) lowtag))
    #-gengc
    (storew value object (+ base offset) lowtag)))
