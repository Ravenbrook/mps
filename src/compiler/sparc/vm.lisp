;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/vm.lisp,v 1.26 2006/06/30 18:41:32 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition for the SPARC.
;;;
;;; Written by William Lott.
;;;
(in-package "SPARC")


;;;; Define the registers

(eval-when (compile eval)

(defmacro defreg (name offset)
  (let ((offset-sym (symbolicate name "-OFFSET")))
    `(eval-when (compile eval load)
       (defconstant ,offset-sym ,offset)
       (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

(defmacro defregset (name &rest regs)
  `(eval-when (compile eval load)
     (defconstant ,name
       (list ,@(mapcar #'(lambda (name) (symbolicate name "-OFFSET")) regs)))))

); eval-when (compile eval)


(eval-when (compile load eval)

(defvar *register-names* (make-array 32 :initial-element nil))

); eval-when (compile load eval)


;; Globals.  These are difficult to extract from a sigcontext.
(defreg zero 0)				; %g0
(defreg alloc 1)			; %g1
(defreg null 2)				; %g2
(defreg csp 3)				; %g3
(defreg cfp 4)				; %g4
(defreg bsp 5)				; %g5
;; %g6 and %g7 are supposed to be reserved for the system.  %g7 is
;; apparently used by thread libraries, and on Solaris 10, is always
;; used, even for apps that aren't threaded.  %g6 doesn't seem to be
;; currently used.

;; NOTE: We need some otherwise unused register for the
;; define-move-function functions so we can access things where the
;; offset of the object is too large to fit in the offset part of an
;; instruction.  It needs to be fixed because these move functions
;; don't have any other args to use.  And since the register allocator
;; doesn't know about such uses, we can't use it for anything else.
(defreg gtemp 26)			; %i2, aka L0

;; Outs.  These get clobbered when we call into C.
(defreg nl0 8)				; %o0
(defreg nl1 9)				; %o1
(defreg nl2 10)				; %o2
(defreg nl3 11)				; %o3
(defreg nl4 12)				; %o4
(defreg nl5 13)				; %o5
(defreg nsp 14)				; %o6, aka %sp
(defreg nargs 15)			; %o7

;; Locals.  These are preserved when we call into C.
(defreg a0 16)				; %l0
(defreg a1 17)				; %l1
(defreg a2 18)				; %l2
(defreg a3 19)				; %l3
(defreg a4 20)				; %l4
(defreg a5 21)				; %l5
(defreg ocfp 22)			; %l6
(defreg lra 23)				; %l7

;; Ins.  These are preserved just like locals.
(defreg cname 24)			; %i0
(defreg lexenv 25)			; %i1
;; WARNING: Register L0 is used as a global temp for
;; define-move-functions to access stack variables when needed.  We
;; can't use it for anything else!
#+nil
(defreg l0 26)				; %i2

(defreg nfp 27)				; %i3
(defreg cfunc 28)			; %i4
(defreg code 29)			; %i5
;; We can't touch reg 30 (aka %i6, aka %fp) if we ever want to return
(defreg lip 31)				; %i7

(eval-when (compile load eval)
  ;; Registers %g6, %g7, and %fp are not defined above so we can't
  ;; access them by accident. However, it's useful to be able to
  ;; disassemble code that uses these registers.  (Callbacks use %fp).
  ;; So manually give names to these registers
  (setf (aref *register-names* 6) "G6")
  (setf (aref *register-names* 7) "G7")
  (setf (aref *register-names* 26) "L0")
  (setf (aref *register-names* 30) "FP")
  )

(defregset non-descriptor-regs
  nl0 nl1 nl2 nl3 nl4 nl5 cfunc nargs nfp)

;; These are the non-descriptor-regs that can be used to hold 64-bit
;; integers in v8plus mode.  Basically any %o register, except we
;; don't want NSP.
#+(and sparc-v9 sparc-v8plus)
(defregset non-descriptor64-regs
  nl0 nl1 nl2 nl3 nl4 nl5 nargs)

(defregset descriptor-regs
  a0 a1 a2 a3 a4 a5 ocfp lra cname lexenv)

(defregset register-arg-offsets
  a0 a1 a2 a3 a4 a5)



;;;; SB and SC definition:

(define-storage-base registers :finite :size 32)
(define-storage-base float-registers :finite :size 64)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;; 
(defmacro define-storage-classes (&rest classes)
  (do ((forms (list 'progn)
	      (let* ((class (car classes))
		     (sc-name (car class))
		     (constant-name (intern (concatenate 'simple-string
							 (string sc-name)
							 "-SC-NUMBER"))))
		(list* `(define-storage-class ,sc-name ,index
			  ,@(cdr class))
		       `(defconstant ,constant-name ,index)
		       `(export ',constant-name)
		       forms)))
       (index 0 (1+ index))
       (classes classes (cdr classes)))
      ((null classes)
       (nreverse forms))))

(define-storage-classes

  ;; Non-immediate contstants in the constant pool
  (constant constant)

  ;; ZERO and NULL are in registers.
  (zero immediate-constant)
  (null immediate-constant)

  ;; Anything else that can be an immediate.
  (immediate immediate-constant)


  ;; **** The stacks.

  ;; The control stack.  (Scanned by GC)
  (control-stack control-stack)

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack) ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack) ; (unsigned-byte 32)
  (base-char-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
  (double-stack non-descriptor-stack
		:element-size 2 :alignment 2) ; double floats.
  #+long-float
  (long-stack non-descriptor-stack :element-size 4 :alignment 4) ; long floats.
  #+double-double
  (double-double-stack non-descriptor-stack :element-size 4 :alignment 2)
  ;; complex-single-floats
  (complex-single-stack non-descriptor-stack :element-size 2)
  ;; complex-double-floats.
  (complex-double-stack non-descriptor-stack :element-size 4 :alignment 2)
  #+long-float
  ;; complex-long-floats.
  (complex-long-stack non-descriptor-stack :element-size 8 :alignment 4)

  #+double-double
  (complex-double-double-stack non-descriptor-stack :element-size 8 :alignment 4)
  
  ;; **** Things that can go in the integer registers.

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg
   registers
   :locations #.(append non-descriptor-regs descriptor-regs)
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations #.descriptor-regs
   :constant-scs (constant null immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Non-Descriptor characters
  (base-char-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (base-char-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space)
  (sap-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations #.non-descriptor-regs)

  ;; 64-bit signed and unsigned integers

  #+(and sparc-v9 sparc-v8plus)
  (signed64-stack non-descriptor-stack :element-size 2 :alignment 2)
  #+(and sparc-v9 sparc-v8plus)
  (unsigned64-stack non-descriptor-stack :element-size 2 :alignment 2)

  ;; 64-bit signed or unsigned integers.  Since CMUCL is still a
  ;; 32-bit app, the v8plus ABI says that only the global and out
  ;; registers can be 64-bit.  (Because task switches will only
  ;; preserve all 64 bits of these registers.  The other registers get
  ;; stored on the stack and thus only preserve 32 bits.)  Since the
  ;; globals are already spoken for, only the out registers are
  ;; available.
  #+(and sparc-v9 sparc-v8plus)
  (signed64-reg registers
    :locations #.non-descriptor64-regs
    :constant-scs (zero immediate)
    :save-p t
    :alternate-scs (signed64-stack))
  #+(and sparc-v9 sparc-v8plus)
  (unsigned64-reg registers
    :locations #.non-descriptor64-regs
    :constant-scs (zero immediate)
    :save-p t
    :alternate-scs (unsigned64-stack))

  ;; Pointers to the interior of objects.  Used only as an temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   :reserve-locations (28 29 30 31)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations #.(loop for i from 0 to #-sparc-v9 31 #+sparc-v9 63
		      by 2 collect i)
   :element-size 2 :alignment 2
   :reserve-locations (28 30)
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  ;; Non-Descriptor double-floats.
  #+long-float
  (long-reg float-registers
   :locations #.(loop for i from 0 to #-sparc-v9 31 #+sparc-v9 63
		      by 4 collect i)
   :element-size 4 :alignment 4
   :reserve-locations (28)
   :constant-scs ()
   :save-p t
   :alternate-scs (long-stack))

  ;; Non-descriptor double-double floats
  #+double-double
  (double-double-reg float-registers
   :locations #.(loop for i from 0 below #-sparc-v9 32 #+sparc-v9 64
		   by 4 collect i)
   :element-size 4 :alignment 4
   :constant-scs ()
   :save-p t
   :alternate-scs (double-double-stack))
  
  (complex-single-reg float-registers
   :locations #.(loop for i from 0 to 31 by 2 collect i)
   :element-size 2 :alignment 2
   :reserve-locations (28 30)
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
   :locations #.(loop for i from 0 to #-sparc-v9 31 #+sparc-v9 63
		      by 4 collect i)
   :element-size 4 :alignment 4
   :reserve-locations (28)
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-stack))

  #+long-float
  (complex-long-reg float-registers
   :locations #.(loop for i from 0 to #-sparc-v9 31 #+sparc-v9 63
		      by 8 collect i)
   :element-size 8 :alignment 8
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-long-stack))

  #+double-double
  (complex-double-double-reg float-registers
   :locations #.(loop for i from 0 below #-sparc-v9 32 #+sparc-v9 64
		      by 8 collect i)
   :element-size 8 :alignment 8
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-double-stack))


  ;; A catch or unwind block.
  (catch-block control-stack :element-size vm:catch-block-size)
  
  )



;;;; Make some random tns for important registers.

(eval-when (compile eval)

(defmacro defregtn (name sc)
  (let ((offset-sym (symbolicate name "-OFFSET"))
	(tn-sym (symbolicate name "-TN")))
    `(defparameter ,tn-sym
       (make-random-tn :kind :normal
		       :sc (sc-or-lose ',sc)
		       :offset ,offset-sym))))

); eval-when (compile eval)

(defregtn zero any-reg)
(defregtn null descriptor-reg)
(defregtn code descriptor-reg)
(defregtn alloc any-reg)

(defregtn nargs any-reg)
(defregtn bsp any-reg)
(defregtn csp any-reg)
(defregtn cfp any-reg)
(defregtn ocfp any-reg)
(defregtn nsp any-reg)
(defregtn gtemp any-reg) 



;;; Immediate-Constant-SC  --  Interface
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     (sc-number-or-lose 'zero *backend*))
    (null
     (sc-number-or-lose 'null *backend*))
    ((or fixnum system-area-pointer character)
     (sc-number-or-lose 'immediate *backend*))
    (symbol
     (if (static-symbol-p value)
	 (sc-number-or-lose 'immediate *backend*)
	 nil))))


;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(defconstant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(defconstant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))

(eval-when (compile load eval)

;;; Offsets of special stack frame locations
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 6)

;;; Names to use for the argument registers.
;;; 
(defconstant register-arg-names '(a0 a1 a2 a3 a4 a5))

); Eval-When (Compile Load Eval)


;;; A list of TN's describing the register arguments.
;;;
(defparameter register-arg-tns
  (mapcar #'(lambda (n)
	      (make-random-tn :kind :normal
			      :sc (sc-or-lose 'descriptor-reg)
			      :offset n))
	  register-arg-offsets))

;;; SINGLE-VALUE-RETURN-BYTE-OFFSET
;;;
;;; This is used by the debugger.
;;;
(export 'single-value-return-byte-offset)
(defconstant single-value-return-byte-offset 8)


;;; LOCATION-PRINT-NAME  --  Interface
;;;
;;;    This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
;;;
(def-vm-support-routine location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
	(offset (tn-offset tn)))
    (ecase sb
      (registers (or (svref *register-names* offset)
		     (format nil "R~D" offset)))
      (float-registers (format nil "F~D" offset))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))
