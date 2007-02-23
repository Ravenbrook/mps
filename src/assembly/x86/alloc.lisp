;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/x86/alloc.lisp,v 1.5 1998/02/19 19:37:25 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle allocating simple objects.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski -- Spring 1995.
;;;
(in-package :x86)


;;;; From from signed/unsigned

#+assembler ; we don't want a vop for this one.
(define-assembly-routine
    (move-from-signed)
    ((:temp eax unsigned-reg eax-offset)
     (:temp ebx unsigned-reg ebx-offset))
  (inst mov ebx eax)
  (inst shl ebx 1)
  (inst jmp :o bignum)
  (inst shl ebx 1)
  (inst jmp :o bignum)
  (inst ret)
  BIGNUM

  (with-fixed-allocation (ebx bignum-type (+ bignum-digits-offset 1))
    (storew eax ebx bignum-digits-offset other-pointer-type))

  (inst ret))

#+assembler ; we don't want a vop for this one either.
(define-assembly-routine
  (move-from-unsigned)
  ((:temp eax unsigned-reg eax-offset)
   (:temp ebx unsigned-reg ebx-offset))

  (inst test eax #xe0000000)
  (inst jmp :nz bignum)
  ;; Fixnum
  (inst mov ebx eax)
  (inst shl ebx 2)
  (inst ret)

  BIGNUM
  ;;; Note: On the mips port space for a two word bignum is always
  ;;; allocated and the header size is set to either one or two words
  ;;; as appropriate. On the mips port this is faster, and smaller
  ;;; inline, but produces more garbage. The inline x86 version uses
  ;;; the same approach, but here we save garbage and allocate the
  ;;; smallest possible bignum.
  (inst jmp :ns one-word-bignum)
  (inst mov ebx eax)

  ;; Two word bignum
  (with-fixed-allocation (ebx bignum-type (+ bignum-digits-offset 2))
    (storew eax ebx bignum-digits-offset other-pointer-type))
  (inst ret)
  
  ONE-WORD-BIGNUM
  (with-fixed-allocation (ebx bignum-type (+ bignum-digits-offset 1))
    (storew eax ebx bignum-digits-offset other-pointer-type))
  (inst ret))
