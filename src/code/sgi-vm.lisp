;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/sgi-vm.lisp,v 1.2 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/code/sgi-vm.lisp,v 1.2 1994/10/31 04:11:27 ram Exp $
;;;
;;; This file contains the SGI specific runtime stuff.
;;;
(in-package "MIPS")
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
    (sc-regmask unsigned-int)
    (sc-status unsigned-int)
    (sc-pc-high unsigned-int)
    (sc-pc system-area-pointer)
    (sc-regs (array unsigned-int 64)) ; 64 bit slots, so deref (* 2)+1
    (sc-fpregs (array unsigned-int 64))
    (sc-ownedfp unsigned-int)
    (sc-fpc-csr unsigned-int)
    (sc-fpc-eir unsigned-int)
    (sc-ssflags unsigned-int)
    (sc-mdhi-high unsigned-int)
    (sc-mdhi unsigned-int)
    (sc-mdlo-high unsigned-int)
    (sc-mdlo unsigned-int)
    (sc-cause-high unsigned-int)
    (sc-cause unsigned-int)
    (sc-badvaddr-high unsigned-int)
    (sc-badvaddr system-area-pointer)
    (sc-triggersave-high unsigned-int)
    (sc-triggersave unsigned-int)
    (sc-sigset (array unsigned-int 4))
    (sc-pad (array unsigned-int 64))))


;;;; Add machine specific features to *features*

(pushnew :sgi *features*)



;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "SGI")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "SGI")



;;; FIXUP-CODE-OBJECT -- Interface
;;;
(defun fixup-code-object (code offset fixup kind)
  (unless (zerop (rem offset word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (system:without-gcing
   (let ((sap (truly-the system-area-pointer
			 (%primitive c::code-instructions code))))
     (ecase kind
       (:jump
	(assert (zerop (ash fixup -28)))
	(setf (ldb (byte 26 0) (system:sap-ref-32 sap offset))
	      (ash fixup -2)))
       (:lui
	(setf (sap-ref-16 sap (+ offset 2))
	      (+ (ash fixup -16)
		 (if (logbitp 15 fixup) 1 0))))
       (:addi
	(setf (sap-ref-16 sap (+ offset 2))
	      (ldb (byte 16 0) fixup)))))))


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
      (when (logbitp 31 (slot scp 'sc-cause))
	(setf pc (sap+ pc 4)))
      (when (= (sap-ref-8 pc 4) 255)
	(setf pc (sap+ pc 1)))
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
    (deref (slot scp 'sc-regs) (1+ (* index 2)))))

(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (setf (deref (slot scp 'sc-regs) (1+ (* index 2))) new)
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
	(single-float (system:sap-ref-single sap (+ 4 (* index vm:word-bytes
							 2))))
	(double-float (system:sap-ref-double sap (+ 4 (* index vm:word-bytes
							 2))))))))
;;;
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'sc-fpregs))))
      (ecase format
	(single-float
	 (setf (sap-ref-single sap (+ 4 (* index vm:word-bytes 2))) new-value))
	(double-float
	 (setf (sap-ref-double sap (+ 4 (* index vm:word-bytes 2)))
			       new-value))))))
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
    (slot scp 'sc-fpc-csr)))



;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).  On the MIPS,
;;; we don't do anything.
;;; 
(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  name)



;;; SANCTIFY-FOR-EXECUTION -- Interface.
;;;
;;; Do whatever is necessary to make the given code component executable.
;;; 
(defun sanctify-for-execution (component)
  (without-gcing
    (alien-funcall (extern-alien "sanctify_for_execution"
				 (function void
					   system-area-pointer
					   unsigned-long))
		   (code-instructions component)
		   (* (code-header-ref component code-code-size-slot)
		      word-bytes)))
  nil)

