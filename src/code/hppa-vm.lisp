;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/hppa-vm.lisp,v 1.7 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the HPPA specific runtime stuff.
;;;
(in-package "HPPA")
(use-package "SYSTEM")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "UNIX")

(export '(fixup-code-object internal-error-arguments
	  sigcontext-program-counter sigcontext-register
	  sigcontext-float-register sigcontext-floating-point-modes
	  extern-alien-name sanctify-for-execution))


;;;; The sigcontext structure.

#+hpux
(def-alien-type save-state
  (struct nil
    (regs (array unsigned-long 32))
    (ss-cr11 unsigned-long)
    (ss-pcoq-head unsigned-long)
    (ss-pcsq-head unsigned-long)
    (ss-pcoq-tail unsigned-long)
    (ss-pcsq-tail unsigned-long)
    (ss_cr15 unsigned-long)
    (ss_cr19 unsigned-long)
    (ss_cr20 unsigned-long)
    (ss_cr21 unsigned-long)
    (ss_cr22 unsigned-long)
    (ss_cpustate unsigned-long)
    (ss_sr4 unsigned-long)
    (ss_sr0 unsigned-long)
    (ss_sr1 unsigned-long)
    (ss_sr2 unsigned-long)
    (ss_sr3 unsigned-long)
    (ss_sr5 unsigned-long)
    (ss_sr6 unsigned-long)
    (ss_sr7 unsigned-long)
    (ss_cr0 unsigned-long)
    (ss_cr8 unsigned-long)
    (ss_cr9 unsigned-long)
    (ss_cr10 unsigned-long)
    (ss_cr12 unsigned-long)
    (ss_cr13 unsigned-long)
    (ss_cr24 unsigned-long)
    (ss_cr25 unsigned-long)
    (ss_cr26 unsigned-long)
    (ss_mpsfu_high unsigned-long)
    (ss_mpsfu_low unsigned-long)
    (ss_mpsfu_ovflo unsigned-long)
    (ss_pad unsigned-long)
    (fpregs (array unsigned-long 32)))) ; union here

#+hpux
(def-alien-type siglocal
  (struct nil
    (sl-syscall unsigned-long)
    (sl-onstack unsigned-long)
    (sl-mask unsigned-long)
    (sl-syscall-action unsigned-char)
    (sl-eosys unsigned-char)
    (sl-error unsigned-short)
    (sl-rval1 unsigned-long)
    (sl-rval2 unsigned-long)
    (sl-arg (array unsigned-long 4)) ; ### ?
    (sl-ss save-state)))

#+hpux
(def-alien-type sigcontext
  (struct nil
    (sc-sl siglocal)
    ; the rest of this structure left out (since save-state not complete?)
    ))

#+MACH
(def-alien-type save-state
  (struct nil
    (regs (array unsigned-long 32))
    (filler (array unsigned-long 32))
    (fpregs (array unsigned-long 32))))

#+MACH
(def-alien-type sigcontext
  (struct nil
    (sc-onstack unsigned-long)
    (sc-mask unsigned-long)
    (sc-sp system-area-pointer)
    (sc-fp system-area-pointer)
    (sc-ap (* save-state))
    (sc-pcsqh unsigned-long)
    (sc-pcoqh unsigned-long)
    (sc-pcsqt unsigned-long)
    (sc-pcoqt unsigned-long)
    (sc-ps unsigned-long)))


;;;; Add machine specific features to *features*

(pushnew :hppa *features*)



;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "HPPA")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "HPPA")



;;; FIXUP-CODE-OBJECT -- Interface
;;;
(defun fixup-code-object (code offset value kind)
  (unless (zerop (rem offset word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (system:without-gcing
   (let* ((sap (truly-the system-area-pointer
			  (%primitive c::code-instructions code)))
	  (inst (sap-ref-32 sap offset)))
     (setf (sap-ref-32 sap offset)
	   (ecase kind
	     (:load
	      (logior (ash (ldb (byte 11 0) value) 1)
		      (logand inst #xffffc000)))
	     (:load-short
	      (let ((low-bits (ldb (byte 11 0) value)))
		(assert (<= 0 low-bits (1- (ash 1 4))))
		(logior (ash low-bits 17)
			(logand inst #xffe0ffff))))
	     (:hi
	      (logior (ash (ldb (byte 5 13) value) 16)
		      (ash (ldb (byte 2 18) value) 14)
		      (ash (ldb (byte 2 11) value) 12)
		      (ash (ldb (byte 11 20) value) 1)
		      (ldb (byte 1 31) value)
		      (logand inst #xffe00000)))
	     (:branch
	      (let ((bits (ldb (byte 9 2) value)))
		(assert (zerop (ldb (byte 2 0) value)))
		(logior (ash bits 3)
			(logand inst #xffe0e002)))))))))



;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((pc (sigcontext-program-counter scp)))
      (declare (type system-area-pointer pc))
      (let* ((length (sap-ref-8 pc 4))
	     (vector (make-array length :element-type '(unsigned-byte 8))))
	(declare (type (unsigned-byte 8) length)
		 (type (simple-array (unsigned-byte 8) (*)) vector))
	(copy-from-system-area pc (* byte-bits 5)
			       vector (* word-bits
					 vector-data-offset)
			       (* length byte-bits))
	(let* ((index 0)
	       (error-number (c::read-var-integer vector index)))
	  (collect ((sc-offsets))
	    (loop
	      (when (>= index length)
		(return))
	      (sc-offsets (c::read-var-integer vector index)))
	    (values error-number (sc-offsets))))))))


;;;; Sigcontext access functions.

;;; SIGCONTEXT-PROGRAM-COUNTER -- Interface
;;;
(defun sigcontext-program-counter (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    #+hpux
    (int-sap (logandc2 (slot (slot (slot scp 'sc-sl) 'sl-ss) 'ss-pcoq-head) 3))
    #+MACH
    (int-sap (logandc2 (slot scp 'sc-pcoqh) 3))
    ))

;;; SIGCONTEXT-REGISTER -- Interface
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;
(defun sigcontext-register (scp index)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    #+hpux
    (deref (slot (slot (slot scp 'sc-sl) 'sl-ss) 'regs) index)
    #+MACH
    (deref (slot (slot scp 'sc-ap) 'regs) index)
    ))

(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    #+hpux
    (setf (deref (slot (slot (slot scp 'sc-sl) 'sl-ss) 'regs) index) new)
    #+MACH
    (setf (deref (slot (slot scp 'sc-ap) 'regs) index) new)
    new))

(defsetf sigcontext-register %set-sigcontext-register)


;;; SIGCONTEXT-FLOAT-REGISTER  --  Interface
;;;
;;; Like SIGCONTEXT-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;;
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp))
  (error "sigcontext-float-register not implemented." scp index format)
  #+nil
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot scp 'sc-fpregs))))
      (ecase format
	(single-float (system:sap-ref-single sap (* index vm:word-bytes)))
	(double-float (system:sap-ref-double sap (* index vm:word-bytes)))))))
;;;
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  (error "%set-sigcontext-float-register not implemented."
	 scp index format new-value)
  #+nil
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
  (error "sigcontext-floating-point-modes not implimented." scp)
  #+nil
  (with-alien ((scp (* sigcontext) scp))
    (slot scp 'sc-fpc-csr)))



;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).  On the HPPA
;;; we just leave it alone.
;;; 
(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  name)



;;; SANCTIFY-FOR-EXECUTION -- Interface.
;;;
;;; Do whatever is necessary to make the given code component executable.
;;; On the PA-RISC, this means flushing the data cache and purging the
;;; inst cache.
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
