;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/assembly/x86/support.lisp,v 1.2 1998/06/16 18:20:43 pw Exp $")
;;;
;;; **********************************************************************
;;; 
;;; This file contains the machine specific support routines needed by
;;; the file assembler.
;;;
;;; Written by William Lott.
;;;
(in-package :x86)

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (values
      `((inst call (make-fixup ',name :assembly-routine)))
      nil))
    (:full-call
     (values
      `((note-this-location ,vop :call-site)
	(inst call (make-fixup ',name :assembly-routine))
	(note-this-location ,vop :single-value-return)
	(move esp-tn ebx-tn))
      '((:save-p :compute-only))))
    (:none
     (values 
      `((inst jmp (make-fixup ',name :assembly-routine)))
      nil))))

(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `(inst ret))
    (:full-call
     `(
       (inst pop eax-tn)

       (inst add eax-tn 2)
       (inst jmp eax-tn)))
    (:none)))
