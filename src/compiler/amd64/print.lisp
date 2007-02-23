;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/amd64/print.lisp,v 1.2 2004/10/19 19:15:31 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the print VOP, which is used while booting the kernel
;;; core to keep the user entertained.
;;;
;;; Written by William Lott.
;;; Enhancements/debugging by Douglas T. Crosher 1996.
;;;
(in-package :amd64)

(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg :offset rax-offset :target result
		   :from :eval :to :result) rax)
  (:temporary (:sc unsigned-reg :offset rdi-offset) rdi) ; from/to?
  ;; c temporary register
  (:temporary (:sc unsigned-reg :offset r10-offset
		   :from :eval :to :result) r10)
  ;; c temporary register
  (:temporary (:sc unsigned-reg :offset r11-offset
		   :from :eval :to :result) r11)
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:ignore args r10 r11)
  (:generator 100
    (move rdi object) ;; C arg 1
    ;; LEA can't be used because it can only do sign-extended 32 bit argument.
    ;; We need to specify immediate here because we don't want to be indirected
    (inst mov-imm rax (make-fixup (extern-alien-name "debug_print") :foreign))
    (inst call (make-fixup (extern-alien-name "call_into_c") :foreign))
    (move result rax)))
