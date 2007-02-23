;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/rt-vm.lisp,v 1.7 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the RT specific runtime stuff.
;;;
(in-package "RT")
(use-package "SYSTEM")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "UNIX")

(export '(fixup-code-object internal-error-arguments
	  sigcontext-register sigcontext-float-register
	  sigcontext-floating-point-modes extern-alien-name))


;;;; The sigcontext structure.

(def-alien-type sigcontext
  (struct nil
    (sc-onstack unsigned-long)
    (sc-mask unsigned-long)
    (sc-floatsave system-area-pointer)
    (sc-sp system-area-pointer)
    (sc-fp system-area-pointer)
    (sc-ap system-area-pointer)
    (sc-pc system-area-pointer) ; IBM calls it the iar.
    (sc-icscs unsigned-long)
    (sc-saveiar system-area-pointer)
    (sc-regs (array unsigned-long 16))))



;;;; Add machine specific features to *features*

(pushnew :ibm-pc-rt *features*)
(pushnew :ibmrt *features*)
(pushnew :rt *features*)



;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "IBM PC/RT")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "IBM PC/RT")



;;; FIXUP-CODE-OBJECT -- Interface
;;;
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset) (type (unsigned-byte 32) fixup))
  (system:without-gcing
   (let ((sap (sap+ (kernel:code-instructions code) offset)))
     (ecase kind
       (:cal
	(setf (sap-ref-16 sap 2)
	      (ldb (byte 16 0) fixup)))
       (:cau
	(let ((high (ldb (byte 16 16) fixup)))
	  (setf (sap-ref-16 sap 2)
		(if (logbitp 15 fixup) (1+ high) high))))
       (:ba
	(unless (zerop (ash fixup -24))
	  (warn "#x~8,'0X out of range for branch-absolute." fixup))
	(setf (sap-ref-8 sap 1)
	      (ldb (byte 8 16) fixup))
	(setf (sap-ref-16 sap 2)
	      (ldb (byte 16 0) fixup)))))))



;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (with-alien ((scp (* sigcontext) scp))
    (let ((pc (slot scp 'sc-pc)))
      (declare (type system-area-pointer pc))
      (let* ((length (sap-ref-8 pc 4))
	     (vector (make-array length :element-type '(unsigned-byte 8))))
	(declare (type (unsigned-byte 8) length)
		 (type (simple-array (unsigned-byte 8) (*)) vector))
	(copy-from-system-area pc (* vm:byte-bits 5)
			       vector (* vm:word-bits
					 vm:vector-data-offset)
			       (* length vm:byte-bits))
	(let* ((index 0)
	       (error-number (c::read-var-integer vector index)))
	  (collect ((sc-offsets))
	    (loop
	      (when (>= index length)
		(return))
	      (sc-offsets (c::read-var-integer vector index)))
	    (values error-number (sc-offsets))))))))



;;;; Sigcontext accessing stuff.

;;; SIGCONTEXT-REGISTER -- Internal.
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;
(defun sigcontext-register (scp index)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (deref (slot scp 'sc-regs) index)))

(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (setf (deref (slot scp 'sc-regs) index) new)
    new))

(defsetf sigcontext-register %set-sigcontext-register)


;;; SIGCONTEXT-FLOAT-REGISTER  --  Internal
;;;
;;; Like SIGCONTEXT-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;;
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp)
	   (ignore scp index))
  ;; ### Some day we should figure out how to do this right.
  (ecase format
    (single-float 0.0s0)
    (double-float 0.0d0)))
;;;
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp)
	   (ignore scp index format))
  ;; ### Some day we should figure out how to do this right.
  new-value)
;;;
(defsetf sigcontext-float-register %set-sigcontext-float-register)


;;; SIGCONTEXT-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a sigcontext pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;
(defun sigcontext-floating-point-modes (scp)
  (declare (ignore scp))
  ;; ### Some day we should figure out how to do this right.
  0)




;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).  On the RT,
;;; we prepend an underscore.
;;; 
(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  (concatenate 'string "_" name))

