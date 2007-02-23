;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/x86/subprim.lisp,v 1.5 2003/08/03 11:27:45 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Linkage information for standard static functions, and random vops.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; 
(in-package :x86)



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg control-stack) :target ptr))
  (:arg-types list)
  (:temporary (:sc unsigned-reg :offset eax-offset) eax)
  (:temporary (:sc descriptor-reg :from (:argument 0)) ptr)
  (:results (count :scs (any-reg)))
  (:result-types positive-fixnum)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 40
    ;; Move OBJECT into a temp we can bash on, and initialize the count.
    (move ptr object)
    (inst xor count count)
    ;; If we are starting with NIL, then it's real easy.
    (inst cmp ptr nil-value)
    (inst jmp :e done)
    ;; Note: we don't have to test to see if the original argument is a
    ;; list, because this is a :fast-safe vop.
    LOOP
    ;; Get the CDR and boost the count.
    (loadw ptr ptr cons-cdr-slot list-pointer-type)
    (inst add count (fixnumize 1))
    ;; If we hit NIL, then we are done.
    (inst cmp ptr nil-value)
    (inst jmp :e done)
    ;; Otherwise, check to see if we hit the end of a dotted list.  If
    ;; not, loop back for more.
    (move eax ptr)
    (inst and al-tn lowtag-mask)
    (inst cmp al-tn list-pointer-type)
    (inst jmp :e loop)
    ;; It's dotted all right.  Flame out.
    (error-call vop object-not-list-error ptr)
    ;; We be done.
    DONE))

(define-vop (fast-length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg control-stack) :target ptr))
  (:arg-types list)
  (:temporary (:sc descriptor-reg :from (:argument 0)) ptr)
  (:results (count :scs (any-reg)))
  (:result-types positive-fixnum)
  (:policy :fast)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 30
    ;; Get a copy of OBJECT in a register we can bash on, and
    ;; initialize COUNT.
    (move ptr object)
    (inst xor count count)
    ;; If we are starting with NIL, we be done.
    (inst cmp ptr nil-value)
    (inst jmp :e done)
    ;; Indirect the next cons cell, and boost the count.
    LOOP
    (loadw ptr ptr cons-cdr-slot list-pointer-type)
    (inst add count (fixnumize 1))
    ;; If we arn't done, go back for more.
    (inst cmp ptr nil-value)
    (inst jmp :ne loop)
    DONE))


(define-static-function length (object) :translate length)

