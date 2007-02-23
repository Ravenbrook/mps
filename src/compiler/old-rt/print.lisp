;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Debugging versions of Print, Write-String and Read-Char that call
;;; miscops.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(define-vop (print one-arg-miscop)
  (:variant 'print))

(define-vop (read-char zero-arg-miscop)
  (:variant 'read-char))

(define-vop (write-string three-arg-miscop)
  (:variant 'write-string))

(define-vop (halt zero-arg-miscop)
  (:variant 'clc::halt))
