;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains some parameterizations of various VM attributes for
;;; the RT.  This file is separate from other stuff so that it can be compiled
;;; and loaded earlier.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(eval-when (compile load eval)

;;; Maximum number of SCs allowed.
;;;
(defconstant sc-number-limit 32)

;;; The number of references that a TN must have to offset the overhead of
;;; saving the TN across a call.
;;;
(defconstant register-save-penalty 3)


;;;; Assembler parameters:

;;; The length of the smallest addressable unit on the target
;;; machine.
;;;
(defparameter *word-length* 8)

;;; Convert-Byte-List  --  Internal
;;; 
;;; Convert-Byte-List take a list of byte specifiers that define a field
;;; and returns a list of byte specifiers that can be used to take apart
;;; the value to be placed in that field.  This is somewhat architecture
;;; dependent because of differences in byte ordering conventions.

(defun convert-byte-list (byte-list)
  (let ((offset (byte-position (first byte-list))))
    (mapcar #'(lambda (byte)
		(byte (byte-size byte) (- (byte-position byte) offset)))
	    byte-list)))


;;;; Other parameters:

;;; The number representing the fasl-code format emit code in.
;;;
(defparameter target-fasl-code-format 6)

;;; The version string for the implementation dependent code.
;;;
(defparameter vm-version "IBM RT PC/Mach 0.0")


;;; The byte ordering of the target implementation.
;;;
(defparameter target-byte-order :big-endian)

;;;
;;; The native byte ordering (should come from somewhere else once
;;; bootstrapped.)
(defconstant native-byte-order :big-endian)

;;; The byte ordering of the target implementation.
;;;
(defparameter target-byte-order :big-endian)

;;;
;;; The native byte ordering (should come from somewhere else once
;;; bootstrapped.)
(defconstant native-byte-order :big-endian)

); Eval-When (Compile Load Eval)
