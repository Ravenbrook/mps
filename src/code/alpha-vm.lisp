;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/alpha-vm.lisp,v 1.4 2003/03/06 14:13:07 pmai Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/code/alpha-vm.lisp,v 1.4 2003/03/06 14:13:07 pmai Exp $
;;;
;;; This file contains the Alpha specific runtime stuff.
;;;
(in-package "ALPHA")
(use-package "SYSTEM")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "UNIX")

(export '(fixup-code-object internal-error-arguments
	  sigcontext-program-counter sigcontext-register
	  sigcontext-float-register sigcontext-floating-point-modes
	  extern-alien-name sanctify-for-execution))


;;;; The sigcontext structure.

(def-alien-type sigcontext
  (struct nil
    (sc-onstack unsigned-long)
    (sc-mask unsigned-long)
    (sc-pc system-area-pointer)
    (sc-ps unsigned-long)
    (sc-regs (array unsigned-long 32))
    (sc-ownedfp unsigned-long)
    (sc-fpregs (array unsigned-long 32))
    (sc-fpcr unsigned-long)
    (sc-fp-control unsigned-long)
    (sc-reserved1 unsigned-long)
    (sc-reserved2 unsigned-long)
    (sc-reserved3 unsigned-long)
    (sc-reserved4 unsigned-long)
    (sc-traparg-a0 unsigned-long)
    (sc-traparg-a1 unsigned-long)
    (sc-traparg-a2 unsigned-long)
    (sc-fp-trap-pc unsigned-long)  ; imprecise pc
    (sc-fp-trigger-sum unsigned-long)
    (sc-fp-trigger-inst unsigned-long)))



;;;; Add machine specific features to *features*

(pushnew :alpha *features*)



;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "DECstation")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "DECstation")



;;; FIXUP-CODE-OBJECT -- Interface
;;;
(defun fixup-code-object (code offset value kind)
  (unless (zerop (rem offset word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (system:without-gcing
   (let ((sap (truly-the system-area-pointer
			 (%primitive c::code-instructions code))))
     (ecase kind
       (:jmp-hint
	(assert (zerop (ldb (byte 2 0) value)))
	#+nil
	(setf (sap-ref-16 sap offset)
	      (logior (sap-ref-16 sap offset) (ldb (byte 14 0) (ash value -2)))))
       (:bits-63-48
	(let* ((value (if (logbitp 15 value) (+ value (ash 1 16)) value))
	       (value (if (logbitp 31 value) (+ value (ash 1 32)) value))
	       (value (if (logbitp 47 value) (+ value (ash 1 48)) value)))
	  (setf (sap-ref-8 sap offset) (ldb (byte 8 48) value))
	  (setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 56) value))))
       (:bits-47-32
	(let* ((value (if (logbitp 15 value) (+ value (ash 1 16)) value))
	       (value (if (logbitp 31 value) (+ value (ash 1 32)) value)))
	  (setf (sap-ref-8 sap offset) (ldb (byte 8 32) value))
	  (setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 40) value))))
       (:ldah
	(let ((value (if (logbitp 15 value) (+ value (ash 1 16)) value)))
	  (setf (sap-ref-8 sap offset) (ldb (byte 8 16) value))
	  (setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 24) value))))
       (:lda
	(setf (sap-ref-8 sap offset) (ldb (byte 8 0) value))
	(setf (sap-ref-8 sap (1+ offset)) (ldb (byte 8 8) value)))))))



;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (declare (type (alien (* sigcontext)) scp))
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


;;;; Sigcontext access functions.

;;; SIGCONTEXT-PROGRAM-COUNTER -- Interface.
;;;
(defun sigcontext-program-counter (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (slot scp 'sc-pc)))

;;; SIGCONTEXT-REGISTER -- Interface.
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


;;; SIGCONTEXT-FLOAT-REGISTER  --  Interface.
;;;
;;; Like SIGCONTEXT-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;;
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'sc-fpregs))))
      (ecase format
	(single-float (system:sap-ref-single sap (* index vm:word-bytes)))
	(double-float (system:sap-ref-double sap (* index vm:word-bytes)))))))
;;;
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'sc-fpregs))))
      (ecase format
	(single-float
	 (setf (sap-ref-single sap (* index vm:word-bytes)) new-value))
	(double-float
	 (setf (sap-ref-double sap (* index vm:word-bytes)) new-value))))))
;;;
(defsetf sigcontext-float-register %set-sigcontext-float-register)


;;; SIGCONTEXT-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a sigcontext pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;
(defun sigcontext-floating-point-modes (scp)
  (declare (type (alien (* sigcontext)) scp))
   (with-alien ((scp (* sigcontext) scp))
    (slot scp 'sc-fpcr)))



;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).  On the MIPS,
;;; we don't do anything.
;;; 
(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  name)

#+(and (or linux (and freebsd elf)) (not linkage-table))
(defun lisp::foreign-symbol-address-aux (name flavor)
  (declare (ignore flavor))
  (multiple-value-bind (value found)
      (gethash name lisp::*foreign-symbols* 0)
    (if found
	value
	(multiple-value-bind (value found)
	    (gethash
	     (concatenate 'string "PVE_stub_" name)
	     lisp::*foreign-symbols* 0)
	  (if found
	      value
	      (let ((value (system:alternate-get-global-address name)))
		(when (zerop value)
		  (error "Unknown foreign symbol: ~S" name))
		value))))))



;;; SANCTIFY-FOR-EXECUTION -- Interface.
;;;
;;; Do whatever is necessary to make the given code component executable.
;;; 
(defun sanctify-for-execution (component)
  (declare (ignore component))
  (%primitive istream-memory-barrier))
