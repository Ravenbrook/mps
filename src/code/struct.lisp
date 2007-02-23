;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/struct.lisp,v 1.21 2002/02/04 17:22:15 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains structure definitions that need to be compiled early
;;; for bootstrapping reasons.
;;;
(in-package "LISP")

;;;; The stream structure:

(defconstant in-buffer-length 512 "The size of a stream in-buffer.")
(deftype in-buffer-type ()
  `(simple-array (unsigned-byte 8) (,in-buffer-length)))

;;; Change the kind of lisp-stream to :instance so that the defstruct
;;; doesn't flame out.
;;; 
(eval-when (compile eval)
  (setf (info type kind 'lisp-stream) :instance))

(defstruct (lisp-stream (:print-function %print-stream))
  ;;
  ;; Buffered input.
  (in-buffer nil :type (or in-buffer-type null))
  (in-index in-buffer-length :type index)	; Index into in-buffer
  (in #'ill-in :type function)			; Read-Char function
  (bin #'ill-bin :type function)		; Byte input function
  (n-bin #'ill-n-bin :type function)		; N-Byte input function
  (out #'ill-out :type function)		; Write-Char function
  (bout #'ill-bout :type function)		; Byte output function
  (sout #'ill-out :type function)		; String output function
  (misc #'do-nothing :type function))		; Less used methods

(declaim (inline streamp))
(defun streamp (stream)
  (typep stream 'stream))

;;; Alien value structures:

(in-package "ALIEN")

(defstruct (alien-value
	    (:print-function %print-alien-value))
  (sap (required-argument) :type system-area-pointer)
  (type (required-argument) :type alien-type))
