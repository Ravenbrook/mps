;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: X86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/amd64-vm.lisp,v 1.2 2004/10/19 20:26:36 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the AMD64 specific runtime stuff.
;;;
;;; Code movement fixups by Douglas T. Crosher, 1997.
;;; Thread support by Douglas T. Crosher, 1999.
;;;

(in-package "AMD64")
(use-package "SYSTEM")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "UNIX")
(use-package "KERNEL")

(export '(fixup-code-object internal-error-arguments
	  sigcontext-program-counter sigcontext-register
	  sigcontext-float-register sigcontext-floating-point-modes
	  extern-alien-name sanctify-for-execution))


;;;; The sigcontext structure.
;;;; Add machine specific features to *features*

(pushnew :amd64 *features*)



#+linux
(def-alien-type nil 
  (struct fpreg
          (significand (array unsigned-short 4))
          (exponent unsigned-short)))
#+linux
(def-alien-type  nil
   (struct fpstate
        (cw  unsigned-long)
        (sw  unsigned-long)
        (tag  unsigned-long)
        (ipoff  unsigned-long)
        (cssel  unsigned-long)
        (dataoff  unsigned-long)
        (datasel unsigned-long)
        (fpreg (array (struct fpreg) 8))
        (status unsigned-long)))

;;; for FreeBSD
#+(and freebsd (not freebsd4))
(def-alien-type sigcontext
    (struct nil
	(sc-onstack unsigned-int)
	(sc-mask    unsigned-int)
	(sc-sp      unsigned-int)
	(sc-fp	    unsigned-int)
	(sc-isp	    unsigned-int)
	(sc-pc	    unsigned-int)
	(sc-efl     unsigned-int)		; sc_ps
	(sc-es	    unsigned-int)
	(sc-ds	    unsigned-int)
	(sc-cs	    unsigned-int)
	(sc-ss	    unsigned-int)
	(sc-edi	    unsigned-int)
	(sc-esi	    unsigned-int)
	(sc-ebx	    unsigned-int)
	(sc-edx	    unsigned-int)
	(sc-ecx	    unsigned-int)
	(sc-eax	    unsigned-int)))

;;; FreeBSD 4.0 has a sigcontext that looks more like Linux.
#+freebsd4
(def-alien-type sigcontext
    (struct nil
	(sc-mask    (array unsigned-int 4))
	(sc-onstack unsigned-int)
	(sc-gs      unsigned-int)
	(sc-fs      unsigned-int)
	(sc-es	    unsigned-int)
	(sc-ds	    unsigned-int)
	(sc-edi	    unsigned-int)
	(sc-esi	    unsigned-int)
	(sc-fp	    unsigned-int)
	(sc-isp     unsigned-int)	
	(sc-ebx	    unsigned-int)
	(sc-edx	    unsigned-int)
	(sc-ecx	    unsigned-int)
	(sc-eax	    unsigned-int)
	(trapno     unsigned-int)
	(err        unsigned-int)
	(sc-pc      unsigned-int)
	(sc-cs	    unsigned-int)
	(sc-efl     unsigned-int)		; sc_ps
	(sc-sp      unsigned-int)	
	(sc-ss	    unsigned-int)))

;;; OpenBSD/NetBSD also have sigcontext structs that look more like Linux.
#+openbsd
(def-alien-type sigcontext
    (struct nil
	(sc-gs      unsigned-int)
	(sc-fs      unsigned-int)
	(sc-es	    unsigned-int)
	(sc-ds	    unsigned-int)
	(sc-edi	    unsigned-int)
	(sc-esi	    unsigned-int)
	(sc-fp	    unsigned-int) ;; ebp
	(sc-ebx	    unsigned-int)
	(sc-edx	    unsigned-int)
	(sc-ecx	    unsigned-int)
	(sc-eax	    unsigned-int)
	(sc-pc      unsigned-int)
	(sc-cs	    unsigned-int)
	(sc-efl     unsigned-int)		; sc_ps
	(sc-sp      unsigned-int)	
	(sc-ss	    unsigned-int)
	(sc-onstack unsigned-int)
	(sc-mask    unsigned-int)
	(sc-trapno  unsigned-int)
	(sc-err     unsigned-int)
	))

#+netbsd
(def-alien-type sigcontext
    (struct nil
	(sc-gs      unsigned-int)
	(sc-fs      unsigned-int)
	(sc-es	    unsigned-int)
	(sc-ds	    unsigned-int)
	(sc-edi	    unsigned-int)
	(sc-esi	    unsigned-int)
	(sc-fp	    unsigned-int) ;; ebp
	(sc-ebx	    unsigned-int)
	(sc-edx	    unsigned-int)
	(sc-ecx	    unsigned-int)
	(sc-eax	    unsigned-int)
	(sc-pc      unsigned-int)
	(sc-cs	    unsigned-int)
	(sc-efl     unsigned-int)		; sc_ps
	(sc-sp      unsigned-int)	
	(sc-ss	    unsigned-int)
	(sc-onstack unsigned-int)
	;; Old NetBSD 1.3 signal mask
	(sc-oldmask unsigned-int)
	(sc-trapno  unsigned-int)
	(sc-err     unsigned-int)
	;; New signal mask (post NetBSD 1.3)
	(sc-mask    (array unsigned-int 4))
	))

;; For Linux...
#+linux
(def-alien-type sigcontext
    (struct nil
	    (sc-r8 unsigned-long)
	    (sc-r9 unsigned-long)
	    (sc-r10 unsigned-long)
	    (sc-r11 unsigned-long)
	    (sc-r12 unsigned-long)
	    (sc-r13 unsigned-long)
	    (sc-r14 unsigned-long)
	    (sc-r15 unsigned-long)
	    (sc-rdi unsigned-long)
	    (sc-rsi unsigned-long)
	    (rbp unsigned-long)
	    (sc-rbx unsigned-long)
	    (sc-rdx unsigned-long)
	    (sc-rax unsigned-long)
	    (sc-rcx unsigned-long)
	    (sc-sp unsigned-long)
	    (sc-pc unsigned-long)
	    (sc-efl unsigned-long)
	    (sc-cs unsigned-short)
	    (gs unsigned-short)
	    (fs unsigned-short)
	    (__pad0 unsigned-short)
	    (err unsigned-long)
	    (trapno unsigned-long)
	    (sc-mask unsigned-long)
	    (cr2 unsigned-long)
	    (fpstate (* (struct fpstate)))
	    (__reserved1 (array unsigned-long 8))))


;;;; MACHINE-TYPE and MACHINE-VERSION

#-cross-compiler
(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "AMD x86-64")


#-cross-compiler
(defun machine-version ()
  "Returns a string describing the version of the local machine."
  "AMD x86-64")



;;; Fixup-Code-Object -- Interface
;;;
;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
;;;
;;; Add a fixup offset to the vector of fixup offsets for the given
;;; code object.
;;;
;;; Counter to measure the storage overhead.
(defvar *num-fixups* 0)
;;; XXX
(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (flet ((add-fixup (code offset)
	   ;; Although this could check for and ignore fixups for code
	   ;; objects in the read-only and static spaces, this should
	   ;; only be the case when *enable-dynamic-space-code* is
	   ;; True.
	   (when lisp::*enable-dynamic-space-code*
	     (incf *num-fixups*)
	     (let ((fixups (code-header-ref code code-constants-offset)))
	       (cond ((typep fixups '(simple-array (unsigned-byte 32) (*)))
		      (let ((new-fixups
			     (adjust-array fixups (1+ (length fixups))
					   :element-type '(unsigned-byte 32))))
			(setf (aref new-fixups (length fixups)) offset)
			(setf (code-header-ref code code-constants-offset)
			      new-fixups)))
		     (t
		      (unless (or (eq (get-type fixups) vm:unbound-marker-type)
				  (zerop fixups))
			(format t "** Init. code FU = ~s~%" fixups))
		      (setf (code-header-ref code code-constants-offset)
			    (make-array 1 :element-type '(unsigned-byte 32)
					:initial-element offset))))))))
    (system:without-gcing
     (let* ((sap (truly-the system-area-pointer
			    (kernel:code-instructions code)))
	    (obj-start-addr (logand (kernel:get-lisp-obj-address code)
				    #xfffffff8))
	    #+nil (const-start-addr (+ obj-start-addr (* 5 4)))
	    (code-start-addr (sys:sap-int (kernel:code-instructions code)))
	    (ncode-words (kernel:code-header-ref code 1))
	    (code-end-addr (+ code-start-addr (* ncode-words 8))))
       (unless (member kind '(:absolute :relative))
	 (error "Unknown code-object-fixup kind ~s." kind))
       (ecase kind
	 (:absolute
	  ;; Word at sap + offset contains a value to be replaced by
	  ;; adding that value to fixup.
	  (setf (sap-ref-32 sap offset) (+ fixup (sap-ref-32 sap offset)))
	  ;; Record absolute fixups that point within the code object.
	  (when (> code-end-addr (sap-ref-32 sap offset) obj-start-addr)
	    (add-fixup code offset)))
	 (:relative
	  ;; Fixup is the actual address wanted.
	  ;;
	  ;; Record relative fixups that point outside the code
	  ;; object.
	  (when (or (< fixup obj-start-addr) (> fixup code-end-addr))
	    (add-fixup code offset))
	  ;; Replace word with value to add to that loc to get there.
	  (let* ((loc-sap (+ (sap-int sap) offset))
		 (rel-val (- fixup loc-sap 4)))
	    (declare (type (unsigned-byte 32) loc-sap)
		     (type (signed-byte 32) rel-val))
	    (setf (signed-sap-ref-32 sap offset) rel-val))))))
    nil))

;;; Do-Load-Time-Code-Fixups
;;;
;;; Add a code fixup to a code object generated by new-genesis. The
;;; fixup has already been applied, it's just a matter of placing the
;;; fixup in the code's fixup vector if necessary.
;;;
#+gencgc
(defun do-load-time-code-fixup (code offset fixup kind)
  (flet ((add-load-time-code-fixup (code offset)
	   (let ((fixups (code-header-ref code vm:code-constants-offset)))
	     (cond ((typep fixups '(simple-array (unsigned-byte 32) (*)))
		    (let ((new-fixups
			   (adjust-array fixups (1+ (length fixups))
					 :element-type '(unsigned-byte 32))))
		      (setf (aref new-fixups (length fixups)) offset)
		      (setf (code-header-ref code vm:code-constants-offset)
			    new-fixups)))
		   (t
		    (unless (or (eq (get-type fixups) vm:unbound-marker-type)
				(zerop fixups))
		      (%primitive print "** Init. code FU"))
		    (setf (code-header-ref code vm:code-constants-offset)
			  (make-array 1 :element-type '(unsigned-byte 32)
				      :initial-element offset)))))))
    (let* ((sap (truly-the system-area-pointer
			   (kernel:code-instructions code)))
	   (obj-start-addr
	    (logand (kernel:get-lisp-obj-address code) #xfffffff8))
	   (code-start-addr (sys:sap-int (kernel:code-instructions code)))
	   (ncode-words (kernel:code-header-ref code 1))
	   (code-end-addr (+ code-start-addr (* ncode-words 8))))
      (ecase kind
	(:absolute
	 ;; Record absolute fixups that point within the
	 ;; code object.
	 (when (> code-end-addr (sap-ref-32 sap offset) obj-start-addr)
	   (add-load-time-code-fixup code offset)))
	(:relative
	 ;; Record relative fixups that point outside the
	 ;; code object.
	 (when (or (< fixup obj-start-addr) (> fixup code-end-addr))
	   (add-load-time-code-fixup code offset)))))))


;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;; 
(defun internal-error-arguments (scp)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((pc (int-sap (slot scp 'sc-pc))))
      (declare (type system-area-pointer pc))
      ;; using INT3 the pc is .. INT3 <here> code length bytes...
      (let* ((length (sap-ref-8 pc 1))
	     (vector (make-array length :element-type '(unsigned-byte 8))))
	(declare (type (unsigned-byte 8) length)
		 (type (simple-array (unsigned-byte 8) (*)) vector))
	(copy-from-system-area pc (* vm:byte-bits 2)
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
    (int-sap (slot scp 'sc-pc))))

;;; SIGCONTEXT-REGISTER -- Interface.
;;;
;;; An escape register saves the value of a register for a frame that someone
;;; interrupts.  
;;;

(defun sigcontext-register (scp index)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (case index				; ugly -- I know.
      (#.eax-offset (slot scp 'sc-eax))
      (#.ecx-offset (slot scp 'sc-ecx))
      (#.edx-offset (slot scp 'sc-edx))
      (#.ebx-offset (slot scp 'sc-ebx))
      (#.esp-offset (slot scp 'sc-sp))
      (#.ebp-offset (slot scp #-linux 'sc-fp #+linux 'ebp))
      (#.esi-offset (slot scp 'sc-esi))
      (#.edi-offset (slot scp 'sc-edi)))))


(defun %set-sigcontext-register (scp index new)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (case index
      (#.eax-offset (setf (slot scp 'sc-eax) new))
      (#.ecx-offset (setf (slot scp 'sc-ecx) new))
      (#.edx-offset (setf (slot scp 'sc-edx) new))
      (#.ebx-offset (setf (slot scp 'sc-ebx) new))
      (#.esp-offset (setf (slot scp 'sc-sp)  new))
      (#.ebp-offset (setf (slot scp #-linux 'sc-fp #+linux 'ebp)  new))
      (#.esi-offset (setf (slot scp 'sc-esi) new))
      (#.edi-offset (setf (slot scp 'sc-edi) new))))
  new)

(defsetf sigcontext-register %set-sigcontext-register)


;;; SIGCONTEXT-FLOAT-REGISTER  --  Interface
;;;
;;; Like SIGCONTEXT-REGISTER, but returns the value of a float register.
;;; Format is the type of float to return.
;;;
#+linux
(defun sigcontext-float-register (scp index format)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((reg-sap (alien-sap (deref (slot (deref (slot scp 'fpstate) 0)
					   'fpreg)
				     index))))
      (coerce (sys:sap-ref-long reg-sap 0) format))))

;;; Not supported on Free/OpenBSD because the floating point state is not
;;; saved.  For now we assume this is true for all modern BSDs
#+BSD
(defun sigcontext-float-register (scp index format)
  (declare (ignore scp index))
  (coerce 0l0 format))

#+linux
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (type (alien (* sigcontext)) scp))
  (with-alien ((scp (* sigcontext) scp))
    (let ((reg-sap (alien-sap (deref (slot (deref (slot scp 'fpstate) 0)
					    'fpreg)
				     index))))
      (declare (ignorable reg-sap))
      #+not-yet
      (setf (sys:sap-ref-long reg-sap 0) (coerce new-value 'long-float))
      (coerce new-value format))))

;;; Not supported on Free/OpenBSD.
#+BSD
(defun %set-sigcontext-float-register (scp index format new-value)
  (declare (ignore scp index))
  (coerce new-value format))

;;;
(defsetf sigcontext-float-register %set-sigcontext-float-register)

;;; SIGCONTEXT-FLOATING-POINT-MODES  --  Interface
;;;
;;;    Given a sigcontext pointer, return the floating point modes word in the
;;; same format as returned by FLOATING-POINT-MODES.
;;;

#+BSD
(defun sigcontext-floating-point-modes (scp)
  (declare (type (alien (* sigcontext)) scp)
	   (ignore scp))
  ;; This is broken until some future release of FreeBSD/OpenBSD!!!
  (floating-point-modes))
  
#+linux
(defun sigcontext-floating-point-modes (scp)
  (declare (type (alien (* sigcontext)) scp))
  (let ((cw (slot (deref (slot scp 'fpstate) 0) 'cw))
	(sw (slot (deref (slot scp 'fpstate) 0) 'sw)))
    ;;(format t "cw = ~4x~%sw = ~4x~%" cw sw)
    ;; NOT TESTED -- clear sticky bits to clear interrupt condition
    (setf (slot (deref (slot scp 'fpstate) 0) 'sw) (logandc2 sw #x3f))
    ;;(format t "new sw = ~x~%" (slot (deref (slot scp 'fpstate) 0) 'sw))
    ;; simulate floating-point-modes VOP
    (logior (ash (logand sw #xffff) 16) (logxor (logand cw #xffff) #x3f))))


;;; EXTERN-ALIEN-NAME -- interface.
;;;
;;; The loader uses this to convert alien names to the form they occure in
;;; the symbol table (for example, prepending an underscore).
;;;
(defun extern-alien-name (name)
  (declare (type simple-string name))
  #+(and bsd (not elf))
  (concatenate 'string "_" name)
  #-(and bsd (not elf))
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
;;; Do whatever is necessary to make the given code component
;;; executable - nothing on the x86.
;;; 
(defun sanctify-for-execution (component)
  (declare (ignore component))
  nil)
 
;;; FLOAT-WAIT
;;;
;;; This is used in error.lisp to insure floating-point  exceptions
;;; are properly trapped. The compiler translates this to a VOP.
;;;
(defun float-wait()
  (float-wait))

;;; FLOAT CONSTANTS
;;;
;;; These are used by the FP move-from-{single|double} VOPs rather
;;; than the i387 load constant instructions to avoid consing in some
;;; cases. Note these are initialise by genesis as they are needed
;;; early.
;;;
(defvar *fp-constant-0s0*)
(defvar *fp-constant-1s0*)
(defvar *fp-constant-0d0*)
(defvar *fp-constant-1d0*)
;;; The long-float constants.
(defvar *fp-constant-0l0*)
(defvar *fp-constant-1l0*)
(defvar *fp-constant-pi*)
(defvar *fp-constant-l2t*)
(defvar *fp-constant-l2e*)
(defvar *fp-constant-lg2*)
(defvar *fp-constant-ln2*)

;;; Enable/Disable scavenging of the read-only space.
(defvar *scavenge-read-only-space* nil)

;;; The current alien stack pointer; saved/restored for non-local
;;; exits.
(defvar *alien-stack*)

;;; Support for the MT19937 random number generator. The update
;;; function is implemented as an assembly routine. This definition is
;;; transformed to a call to this routine allowing its use in byte
;;; compiled code.
;;;
(defun random-mt19937 (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state))
  (random-mt19937 state))


;;;; Useful definitions for writing thread safe code.

(in-package "KERNEL")

(export '(atomic-push-symbol-value atomic-pop-symbol-value
	  atomic-pusha atomic-pushd atomic-push-vector))

(defun %instance-set-conditional (object slot test-value new-value)
  (declare (type instance object)
	   (type index slot))
  "Atomically compare object's slot value to test-value and if EQ store
   new-value in the slot. The original value of the slot is returned."
  (%instance-set-conditional object slot test-value new-value))

(defun set-symbol-value-conditional (symbol test-value new-value)
  (declare (type symbol symbol))
  "Atomically compare symbol's value to test-value and if EQ store
  new-value in symbol's value slot and return the original value."
  (set-symbol-value-conditional symbol test-value new-value))

(defun rplaca-conditional (cons test-value new-value)
  (declare (type cons cons))
  "Atomically compare the car of CONS to test-value and if EQ store
  new-value its car and return the original value."
  (rplaca-conditional cons test-value new-value))

(defun rplacd-conditional (cons test-value new-value)
  (declare (type cons cons))
  "Atomically compare the cdr of CONS to test-value and if EQ store
  new-value its cdr and return the original value."
  (rplacd-conditional cons test-value new-value))

(defun data-vector-set-conditional (vector index test-value new-value)
  (declare (type simple-vector vector))
  "Atomically compare an element of vector to test-value and if EQ store
  new-value the element and return the original value."
  (data-vector-set-conditional vector index test-value new-value))

(defmacro atomic-push-symbol-value (val symbol)
  "Thread safe push of val onto the list in the symbol global value."
  (ext:once-only ((n-val val))
    (let ((new-list (gensym))
	  (old-list (gensym)))
      `(let ((,new-list (cons ,n-val nil)))
	 (loop
	  (let ((,old-list ,symbol))
	    (setf (cdr ,new-list) ,old-list)
	    (when (eq (set-symbol-value-conditional
		       ',symbol ,old-list ,new-list)
		      ,old-list)
	      (return ,new-list))))))))

(defmacro atomic-pop-symbol-value (symbol)
  "Thread safe pop from the list in the symbol global value."
  (let ((new-list (gensym))
	(old-list (gensym)))
    `(loop
      (let* ((,old-list ,symbol)
	     (,new-list (cdr ,old-list)))
	(when (eq (set-symbol-value-conditional
		   ',symbol ,old-list ,new-list)
		  ,old-list)
	  (return (car ,old-list)))))))

(defmacro atomic-pusha (val cons)
  "Thread safe push of val onto the list in the car of cons."
  (once-only ((n-val val)
	      (n-cons cons))
    (let ((new-list (gensym))
	  (old-list (gensym)))
      `(let ((,new-list (cons ,n-val nil)))
	 (loop
	  (let ((,old-list (car ,n-cons)))
	    (setf (cdr ,new-list) ,old-list)
	    (when (eq (rplaca-conditional ,n-cons ,old-list ,new-list)
		      ,old-list)
	      (return ,new-list))))))))

(defmacro atomic-pushd (val cons)
  "Thread safe push of val onto the list in the cdr of cons."
  (once-only ((n-val val)
	      (n-cons cons))
    (let ((new-list (gensym))
	  (old-list (gensym)))
      `(let ((,new-list (cons ,n-val nil)))
	 (loop
	  (let ((,old-list (cdr ,n-cons)))
	    (setf (cdr ,new-list) ,old-list)
	    (when (eq (rplacd-conditional ,n-cons ,old-list ,new-list)
		      ,old-list)
	      (return ,new-list))))))))

(defmacro atomic-push-vector (val vect index)
  "Thread safe push of val onto the list in the vector element."
  (once-only ((n-val val)
	      (n-vect vect)
	      (n-index index))
    (let ((new-list (gensym))
	  (old-list (gensym)))
      `(let ((,new-list (cons ,n-val nil)))
	 (loop
	  (let ((,old-list (svref ,n-vect ,n-index)))
	    (setf (cdr ,new-list) ,old-list)
	    (when (eq (data-vector-set-conditional
		       ,n-vect ,n-index ,old-list ,new-list)
		      ,old-list)
	      (return ,new-list))))))))

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
