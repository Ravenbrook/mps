;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/ppc-vm.lisp,v 1.6 2006/01/18 15:21:26 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the PPC specific runtime stuff.
;;;
(in-package "PPC")
(use-package "SYSTEM")
(use-package "UNIX")

(export '(fixup-code-object internal-error-arguments
	  sigcontext-program-counter sigcontext-register
	  sigcontext-float-register sigcontext-floating-point-modes
	  extern-alien-name sanctify-for-execution
	  sigcontext-lr))


;;;; The sigcontext structure.
;;;; Note that the layout of this thing matches the word offsets PT_xxx, not
;;;; (necessarily) the pt_regs structure.
#-darwin
(def-alien-type sigcontext-regs
  (struct nil
    (regs (array unsigned-long 32))
    (pc system-area-pointer)
    (msr unsigned-long)
    (orig-r3 unsigned-long)
    (ctr unsigned-long)
    (lr unsigned-long)
    (xer unsigned-long)
    (ccr unsigned-long)
    (mq unsigned-long)
    (trap unsigned-long)
    (dar unsigned-long)
    (dsisr unsigned-long)
    (result unsigned-long)
    (pad (array unsigned-long 4))
    (fpregs (array unsigned-long 64))
    (pad2 unsigned-long)
    (fpscr unsigned-long)))

;; In reality on darwin this is an instance of struct mcontext, which
;; contains 4 sub-structures, all defined in mach/thread_status.h.
;; For our purposes we just combine this into one structure, defining
;; types and layout as we see fit.
#+darwin
(def-alien-type sigcontext-regs
  (struct nil
    (dar unsigned-long)
    (dsisr unsigned-long)
    (exception unsigned-long)
    (pad1 unsigned-long)
    (pad2 (array unsigned-long 4))
    (pc system-area-pointer)
    (msr unsigned-long)
    (regs (array unsigned-long 32))
    (cr unsigned-long)
    (xer unsigned-long)
    (lr unsigned-long)
    (ctr unsigned-long)
    (mq unsigned-long)
    (vrsave unsigned-long)
    (fpregs (array unsigned-long 64))
    (pad3 unsigned-long)
    (fpscr unsigned-long)
    (vrregs (array unsigned-long 128))
    (vscr (array unsigned-long 4))
    (pad4 (array unsigned-long 4))
    (vrvalid unsigned-long)
    (pad5 (array unsigned-long 7))))

#-darwin
(def-alien-type sigcontext
  (struct nil
    (sc-unused (array unsigned-long 4))
    (sc-signal int)
    (sc-handler unsigned-long)
    (sc-oldmask unsigned-long)
    (sc-regs (* sigcontext-regs))))

;; Use the ucontext_t structure for Darwin
#+darwin
(def-alien-type sigcontext
  (struct nil
    (sc-onstack int)
    (sc-mask int)
    (sc-ss-sp int)
    (sc-ss-size int)
    (sc-ss-flags int)
    (sc-link int)
    (sc-mcsize int)
    (sc-regs (* sigcontext-regs))))


;;;; Add machine specific features to *features*

(pushnew :ppc *features*)



;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "PowerPC")

(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "who-knows?")



;;; FIXUP-CODE-OBJECT -- Interface
;;;
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (unless (zerop (rem offset ppc:word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (system:without-gcing
   (let ((sap (truly-the system-area-pointer
			 (%primitive c::code-instructions code))))
     (ecase kind
       (:b
	(error "Can't deal with CALL fixups, yet."))
       (:ba
	(setf (ldb (byte 24 2) (sap-ref-32 sap offset))
	      (ash fixup -2)))
       (:ha
	(let* ((h (ldb (byte 16 16) fixup))
	       (l (ldb (byte 16 0) fixup)))
	  ; Compensate for possible sign-extension when the low half
	  ; is added to the high.  We could avoid this by ORI-ing
	  ; the low half in 32-bit absolute loads, but it'd be
	  ; nice to be able to do:
	  ;  lis rX,foo@ha
	  ;  lwz rY,foo@l(rX)
	  ; and lwz/stw and friends all use a signed 16-bit offset.
	  (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
		 (if (logbitp 15 l) (ldb (byte 16 0) (1+ h)) h))))
       (:l
	(setf (ldb (byte 16 0) (sap-ref-32 sap offset))
	      (ldb (byte 16 0) fixup)))))))



;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (declare (type (alien (* sigcontext)) scp))
  (let* ((pc (with-alien ((scp (* sigcontext) scp))
	       (slot (slot scp 'sc-regs) 'pc)))
	 (bad-inst (sap-ref-32 pc 0))
	 (op (ldb (byte 16 16) bad-inst)))
    (declare (type system-area-pointer pc))
    (cond ((= op (logior (ash 3 10) (ash 6 5)))
	   (args-for-unimp-inst scp))
	  ((and (= (ldb (byte 6 10) op) 3)
		(= (ldb (byte 5 5) op) 24))
	   (let* ((regnum (ldb (byte 5 0) op))
		  (prev (sap-ref-32 (int-sap (- (sap-int pc) 4)) 0)))
	     (if (and (= (ldb (byte 6 26) prev) 3)
		      (= (ldb (byte 5 21) prev) 0))
		 (values (ldb (byte 16 0) prev)
			 (list (c::make-sc-offset ppc:any-reg-sc-number
						  (ldb (byte 5 16) prev))))
		 (values #.(kernel:error-number-or-lose
			    'kernel:invalid-argument-count-error)
		     (list (c::make-sc-offset ppc:any-reg-sc-number regnum))))))

	  (t
	   (values #.(error-number-or-lose 'unknown-error) nil)))))

(defun args-for-unimp-inst (scp)
  (declare (type (alien (* sigcontext)) scp))
  (let* ((pc (with-alien ((scp (* sigcontext) scp))
	       (slot (slot scp 'sc-regs) 'pc)))
	 (length (sap-ref-8 pc 4))
	 (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system-area-pointer pc)
	     (type (unsigned-byte 8) length)
	     (type (simple-array (unsigned-byte 8) (*)) vector))
    (copy-from-system-area pc (* ppc:byte-bits 5)
			   vector (* ppc:word-bits
				     ppc:vector-data-offset)
			   (* length ppc:byte-bits))
    (let* ((index 0)
	   (error-number (c::read-var-integer vector index)))
      (collect ((sc-offsets))
	       (loop
		(when (>= index length)
		  (return))
		(sc-offsets (c::read-var-integer vector index)))
	       (values error-number (sc-offsets))))))



;;;; Sigcontext access functions.

;;; SIGCONTEXT-PROGRAM-COUNTER -- Interface.
;;;
(defun sigcontext-program-counter (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
	       (slot (slot scp 'sc-regs) 'pc)))

;;; SIGCONTEXT-REGISTER -- Interface.
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;
(defun sigcontext-register (scp index)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (deref (slot (slot scp 'sc-regs) 'regs) index)))

(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (setf (deref (slot (slot scp 'sc-regs) 'regs) index) new)
    new))

(defsetf sigcontext-register %set-sigcontext-register)

;; Extract the LR register from the sigcontext.
(defun sigcontext-lr (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (slot (slot scp 'sc-regs) 'lr)))

;;; SIGCONTEXT-FLOAT-REGISTER  --  Interface
;;;
;;; Like SIGCONTEXT-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;;
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot (slot scp 'sc-regs) 'fpregs))))
      (ecase format
	(single-float (float (system:sap-ref-double sap (* index 2 ppc:word-bytes)) 0.0d0))
	(double-float (system:sap-ref-double sap (* index 2 ppc:word-bytes)))))))
;;;
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((sap (alien-sap (slot (slot scp 'sc-regs) 'fpregs))))
      (ecase format
	(single-float
	 (setf (sap-ref-double sap (* index 2 ppc:word-bytes))
	       (coerce new-value 'double-float)))
	(double-float
	 (setf (sap-ref-double sap (* index 2 ppc:word-bytes)) new-value))))))
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
    (slot (slot scp 'sc-regs) 'fpscr)))



;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).  On the SPARC,
;;; we prepend an underscore.
;;; 
(defun extern-alien-name (name)
  (declare (type simple-base-string name))
  #+darwin
  (concatenate 'string "_" name)
  #-darwin
  (concatenate 'string "" name))

#-linkage-table
(defun lisp::foreign-symbol-address-aux (name flavor)
  (declare (ignore flavor))
  (multiple-value-bind (value found)
      (gethash name lisp::*foreign-symbols* 0)
    (if found
        value
        (multiple-value-bind (value found)
            (gethash  
             (concatenate 'string "ldso_stub__" name)
             lisp::*foreign-symbols* 0)
          (if found 
              value
              (let ((value (system:alternate-get-global-address name)))
                (when (zerop value)
                  (error "Unknown foreign symbol: ~S" name))
                value))))))

#+linkage-table
(progn
(defun lisp::foreign-symbol-address-aux (name flavor)
  (let ((entry-num (lisp::register-foreign-linkage name flavor)))
    (+ #.vm:target-foreign-linkage-space-start
       (* entry-num vm:target-foreign-linkage-entry-size))))

(defun lisp::find-foreign-symbol (addr)
  (declare (type (unsigned-byte 32) addr))
  (when (>= addr vm:target-foreign-linkage-space-start)
    (let ((entry (/ (- addr vm:target-foreign-linkage-space-start)
		    vm:target-foreign-linkage-entry-size)))
      (when (< entry (lisp::foreign-linkage-symbols))
	(lisp::foreign-linkage-entry entry)))))
)


;;; SANCTIFY-FOR-EXECUTION -- Interface.
;;;
;;; Do whatever is necessary to make the given code component executable.
;;; On the 601, we have less to do than on some other PowerPC chips.
;;; This should what needs to be done in the general case.
;;; 
(defun sanctify-for-execution (component)
  (without-gcing
    (alien-funcall (extern-alien "ppc_flush_icache"
				 (function void
					   system-area-pointer
					   unsigned-long))
		   (code-instructions component)
		   (* (code-header-ref component code-code-size-slot)
		      word-bytes)))
  nil)

;;; Enable/Disable scavenging of the read-only space.
(defvar *scavenge-read-only-space* nil)
