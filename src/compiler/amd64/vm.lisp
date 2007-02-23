;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: python-x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/amd64/vm.lisp,v 1.2 2004/07/06 20:23:30 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition noise for amd64.
;;;
;;; Written by William Lott
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996, 1997, 1998.
;;;

(in-package :amd64)


;;;; Register specs

(eval-when (compile eval)

(defmacro defreg (name offset size)
  (let ((offset-sym (symbolicate name "-OFFSET"))
	(names-vector (symbolicate "*" size "-REGISTER-NAMES*")))
    `(progn
       (eval-when (compile eval load)
	 (defconstant ,offset-sym ,offset))
       (setf (svref ,names-vector ,offset-sym) ,(symbol-name name)))))

(defmacro defregset (name &rest regs)
  `(eval-when (compile eval load)
     (defconstant ,name
       (list ,@(mapcar #'(lambda (name) (symbolicate name "-OFFSET")) regs)))))

); eval-when 

;;; Byte registers.
;;;
;;; Note: the encoding here is different than that used by the chip.  We
;;; use this encoding so that the compiler thinks that AX (and EAX) overlap
;;; AL and AH instead of AL and CL.
;;; 
(eval-when (compile load eval)
  (defvar *byte-register-names* (make-array 8 :initial-element nil)))
;;; what about the other 8-bit registers?
(defreg al 0 :byte)
(defreg ah 1 :byte)
(defreg cl 2 :byte)
(defreg ch 3 :byte)
(defreg dl 4 :byte)
(defreg dh 5 :byte)
(defreg bl 6 :byte)
(defreg bh 7 :byte)
;;;
(defregset byte-regs al ah cl ch dl dh bl bh)

;;; Word registers.
;;;
(eval-when (compile load eval)
  (defvar *word-register-names* (make-array 32 :initial-element nil)))
;;;
(defreg ax 0 :word)
(defreg cx 2 :word)
(defreg dx 4 :word)
(defreg bx 6 :word)
(defreg sp 8 :word)
(defreg bp 10 :word)
(defreg si 12 :word)
(defreg di 14 :word)
(defreg r8w 16 :word)
(defreg r9w 18 :word)
(defreg r10w 20 :word)
(defreg r11w 22 :word)
(defreg r12w 24 :word)
(defreg r13w 26 :word)
(defreg r14w 28 :word)
(defreg r15w 30 :word)
;;;
(defregset word-regs ax cx dx bx si di r8w r9w r10w r11w r12w r13w r14w r15w)

;;; Double Word registers.
;;;
(eval-when (compile load eval)
  (defvar *dword-register-names* (make-array 32 :initial-element nil)))
;;;
(defreg eax 0 :dword)
(defreg ecx 2 :dword)
(defreg edx 4 :dword)
(defreg ebx 6 :dword)
(defreg esp 8 :dword)
(defreg ebp 10 :dword)
(defreg esi 12 :dword)
(defreg edi 14 :dword)
(defreg r8d 16 :dword)
(defreg r9d 18 :dword)
(defreg r10d 20 :dword)
(defreg r11d 22 :dword)
(defreg r12d 24 :dword)
(defreg r13d 26 :dword)
(defreg r14d 28 :dword)
(defreg r15d 30 :dword)
;;;
(defregset dword-regs eax ecx edx ebx esi edi r8d r9d r10d r11d r12d r13d r14d
	   r15d)

;;; Quad Word registers.
;;;
(eval-when (compile load eval)
  (defvar *qword-register-names* (make-array 32 :initial-element nil)))
;;;
(defreg rax 0 :qword)
(defreg rcx 2 :qword)
(defreg rdx 4 :qword)
(defreg rbx 6 :qword)
(defreg rsp 8 :qword)
(defreg rbp 10 :qword)
(defreg rsi 12 :qword)
(defreg rdi 14 :qword)
(defreg r8 16 :qword)
(defreg r9 18 :qword)
(defreg r10 20 :qword)
(defreg r11 22 :qword)
(defreg r12 24 :qword)
(defreg r13 26 :qword)
(defreg r14 28 :qword)
(defreg r15 30 :qword)
;;;
(defregset qword-regs rax rcx rdx rbx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)

;;; added by jrd
(eval-when (compile load eval)
  (defvar *float-register-names* (make-array 8 :initial-element nil)))
(defreg fr0 0 :float)
(defreg fr1 1 :float)
(defreg fr2 2 :float)
(defreg fr3 3 :float)
(defreg fr4 4 :float)
(defreg fr5 5 :float)
(defreg fr6 6 :float)
(defreg fr7 7 :float)
(defregset float-regs fr0 fr1 fr2 fr3 fr4 fr5 fr6 fr7)


;;;; SB definitions.

;;; Despite the fact that there are only 8 different registers, we consider
;;; them 16 in order to describe the overlap of byte registers.  The only
;;; thing we need to represent is what registers overlap.  Therefore, we
;;; consider bytes to take one unit, and words, dwords or qwords to take two.  We
;;; don't need to tell the difference between words, dwords, and qwords, because
;;; you can't put two words in a dword.

(define-storage-base registers :finite :size 32)

;;;
;;; jrd changed this from size 1 to size 8.  it doesn't seem to make much
;;; sense to use the 387's idea of a stack.  8 separate registers is easier
;;; to deal with.
;;; (define-storage-base float-registers :finite :size 1)
(define-storage-base float-registers :finite :size 8)

(define-storage-base stack :unbounded :size 8)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
(define-storage-base noise :unbounded :size 2)



;;;; SC definitions.

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;; 
(defmacro define-storage-classes (&rest classes)
  (collect ((forms))
    (let ((index 0))
      (dolist (class classes)
	(let* ((sc-name (car class))
	       (constant-name (symbolicate sc-name "-SC-NUMBER")))
	  (forms `(define-storage-class ,sc-name ,index
		    ,@(cdr class)))
	  (forms `(defconstant ,constant-name ,index))
	  (forms `(export ',constant-name))
	  (incf index))))
    `(progn
       ,@(forms))))

(define-storage-classes

  ;; Non-immediate constants in the constant pool
  (constant constant)

  ;; Some FP constants can be generated in the i387 silicon.
  (fp-constant immediate-constant)
    
  (immediate immediate-constant)

  ;; **** The stacks.

  ;; The control stack.
  (control-stack stack)			; may be pointers, scanned by GC

  ;; The non-descriptor stacks.
  (signed-stack stack)			; (signed-byte 32)
  (unsigned-stack stack)		; (unsigned-byte 32)
  (base-char-stack stack)		; non-descriptor characters.
  (sap-stack stack)			; System area pointers.
  (single-stack stack)			; single-floats
  (double-stack stack :element-size 2)	; double-floats.
  #+long-float
  (long-stack stack :element-size 3)	; long-floats.
  (complex-single-stack stack :element-size 2)	; complex-single-floats
  (complex-double-stack stack :element-size 4)	; complex-double-floats
  #+long-float
  (complex-long-stack stack :element-size 6)	; complex-long-floats

  ;; **** Magic SCs.

  (ignore-me noise)

  ;; **** Things that can go in the integer registers.

  ;; We don't have to distinguish between descriptor and
  ;; non-descriptor registers, because of the conservative GC.
  ;; Therefore, we only use different scs to distinguish between
  ;; descriptor and non-descriptor values and to specify size.


  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg registers
	   :locations #.qword-regs
	   :element-size 2
	   :constant-scs (immediate)
	   :save-p t
	   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
		  :locations #.qword-regs
		  :element-size 2
		  :constant-scs (constant immediate)
		  :save-p t
		  :alternate-scs (control-stack))

  ;; Non-Descriptor characters
  (base-char-reg registers
		 :locations #.byte-regs
		 :reserve-locations (#.ah-offset #.al-offset)
		 :constant-scs (immediate)
		 :save-p t
		 :alternate-scs (base-char-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space)
  (sap-reg registers
	   :locations #.qword-regs
	   :element-size 2
	   :constant-scs (immediate)
	   :save-p t
	   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
	      :locations #.qword-regs
	      :element-size 2
	      :constant-scs (immediate)
	      :save-p t
	      :alternate-scs (signed-stack))
  (unsigned-reg registers
		:locations #.qword-regs
		:element-size 2
		:constant-scs (immediate)
		:save-p t
		:alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (dword-reg registers
	     :locations #.dword-regs
	     :element-size 2
	     )
  (word-reg registers
	    :locations #.word-regs
	    :element-size 2
	    )
  (byte-reg registers
	    :locations #.byte-regs
	    )

  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
	      :locations (0 1 2 3 4 5 6 7)
	      :constant-scs (fp-constant)
	      :save-p t
	      :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
	      :locations (0 1 2 3 4 5 6 7)
	      :constant-scs (fp-constant)
	      :save-p t
	      :alternate-scs (double-stack))

  ;; Non-Descriptor long-floats.
  #+long-float
  (long-reg float-registers
	    :locations (0 1 2 3 4 5 6 7)
	    :constant-scs (fp-constant)
	    :save-p t
	    :alternate-scs (long-stack))

  (complex-single-reg float-registers
		      :locations (0 2 4 6)
		      :element-size 2
		      :constant-scs ()
		      :save-p t
		      :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
		      :locations (0 2 4 6)
		      :element-size 2
		      :constant-scs ()
		      :save-p t
		      :alternate-scs (complex-double-stack))

  #+long-float
  (complex-long-reg float-registers
		    :locations (0 2 4 6)
		    :element-size 2
		    :constant-scs ()
		    :save-p t
		    :alternate-scs (complex-long-stack))

  ;; A catch or unwind block.
  (catch-block stack :element-size vm:catch-block-size)
  )

(eval-when (compile load eval)

(defconstant byte-sc-names '(base-char-reg byte-reg base-char-stack))
(defconstant word-sc-names '(word-reg))
(defconstant dword-sc-names '(dword-reg))
(defconstant qword-sc-names
  '(any-reg descriptor-reg sap-reg signed-reg unsigned-reg control-stack
    signed-stack unsigned-stack sap-stack single-stack constant))

;;;
;;; added by jrd.  I guess the right thing to do is to treat floats
;;; as a separate size...
;;;
;;; These are used to (at least) determine operand size.
(defconstant float-sc-names '(single-reg))
(defconstant double-sc-names '(double-reg double-stack))
)



;;;; Random TNs for the various registers.

(eval-when (compile eval)
  (defmacro def-random-reg-tns (sc-name &rest reg-names)
    (collect ((forms))
      (dolist (reg-name reg-names)
	(let ((reg-tn-name (symbolicate reg-name "-TN"))
	      (reg-offset-name (symbolicate reg-name "-OFFSET")))
	  ;;(forms `(export ',reg-tn-name))
	  (forms `(defparameter ,reg-tn-name
		    (make-random-tn :kind :normal
				    :sc (sc-or-lose ',sc-name)
				    :offset ,reg-offset-name)))))
      `(progn ,@(forms)))))

(def-random-reg-tns unsigned-reg rax rbx rcx rdx rbp rsp rdi rsi r8 r9 r10 r11
		    r12 r13 r14 r15)
(def-random-reg-tns dword-reg eax ebx ecx edx ebp esp edi esi r8d r9d r10d r11d
		    r12d r13d r14d r15d)
(def-random-reg-tns word-reg ax bx cx dx bp sp di si r8w r9w r10w r11w r12w r13w
		    r14w r15w)
(def-random-reg-tns byte-reg al ah bl bh cl ch dl dh)

;; added by jrd
(def-random-reg-tns single-reg fr0 fr1 fr2 fr3 fr4 fr5 fr6 fr7)

;; Added by pw.

(defparameter fp-constant-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'fp-constant)
		  :offset 31))		; Offset doesn't get used.


;;; Immediate-Constant-SC
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((or fixnum system-area-pointer character)
     (sc-number-or-lose 'immediate *backend*))
    (bignum
     ;; during cross compilation, a fixnum in 64-bit may be a bignum in 32-bit
     (when (<= target-most-negative-fixnum value target-most-positive-fixnum)
       (sc-number-or-lose 'immediate *backend*)))
    (symbol
     (when (static-symbol-p value)
       (sc-number-or-lose 'immediate *backend*)))
    (single-float
     (when (or (eql value 0f0) (eql value 1f0))
       (sc-number-or-lose 'fp-constant *backend*)))
    (double-float
     (when (or (eql value 0d0) (eql value 1d0))
       (sc-number-or-lose 'fp-constant *backend*)))
    #+long-float
    (long-float
     (when (or (eql value 0l0) (eql value 1l0)
	       (eql value pi)
	       (eql value (log 10l0 2l0))
	       (eql value (log 2.718281828459045235360287471352662L0 2l0))
	       (eql value (log 2l0 10l0))
	       (eql value (log 2l0 2.718281828459045235360287471352662L0)))
       (sc-number-or-lose 'fp-constant *backend*)))))


;;;; Function Call Parameters

;;; Offsets of special stack frame locations
(defconstant ocfp-save-offset 0)
(defconstant return-pc-save-offset 1)
(defconstant code-save-offset 2)

;;; names of these things seem to have changed.  these aliases by jrd
(defconstant lra-save-offset return-pc-save-offset)

(defconstant cfp-offset rbp-offset)	; pfw - needed by stuff in /code
					; related to sigcontext stuff

;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 3)

;;; Names, Offsets, and TNs to use for the argument registers.
;;; 
(defconstant register-arg-names '(rdx rdi rsi))
(defregset register-arg-offsets rdx rdi rsi)
(defparameter register-arg-tns (list rdx-tn rdi-tn rsi-tn))

;;; SINGLE-VALUE-RETURN-BYTE-OFFSET
;;;
;;; This is used by the debugger.
;;;
(export 'single-value-return-byte-offset)
(defconstant single-value-return-byte-offset 2)


;;; LOCATION-PRINT-NAME  --  Interface
;;;
;;;    This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
;;;
(def-vm-support-routine location-print-name (tn)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
	 (sb (sb-name (sc-sb sc)))
	 (offset (tn-offset tn)))
    (ecase sb
      (registers
       (let* ((sc-name (sc-name sc))
	      (name-vec (cond ((member sc-name byte-sc-names)
			       *byte-register-names*)
			      ((member sc-name word-sc-names)
			       *word-register-names*)
			      ((member sc-name dword-sc-names)
			       *dword-register-names*)
			      ((member sc-name qword-sc-names)
			       *qword-register-names*))))
	 (or (and name-vec
		  (< -1 offset (length name-vec))
		  (svref name-vec offset))
	     (format nil "<Unknown Reg: off=~D, sc=~A>" offset sc-name))))
      (float-registers (format nil "FR~D" offset))
      (stack (format nil "S~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed")
      (noise (symbol-name (sc-name sc))))))

;; low 8-bit registers
(defun 64-bit-to-8-bit-tn (tn)
  (case (tn-offset tn)
    (#.rax-offset al-tn)
    (#.rbx-offset bl-tn)
    (#.rcx-offset cl-tn)
    (#.rdx-offset dl-tn)))

(defun 64-bit-to-16-bit-tn (tn)
  (ecase (tn-offset tn)
    (#.rax-offset ax-tn)
    (#.rbx-offset bx-tn)
    (#.rcx-offset cx-tn)
    (#.rdx-offset dx-tn)
    (#.rsi-offset si-tn)
    (#.rdi-offset di-tn)
    (#.rbp-offset bp-tn)
    (#.rsp-offset sp-tn)))

(defun 64-bit-to-32-bit-tn (qword-tn)
  (ecase (tn-offset qword-tn)
    (#.rax-offset eax-tn)
    (#.rbx-offset ebx-tn)
    (#.rcx-offset ecx-tn)
    (#.rdx-offset edx-tn)
    (#.rsi-offset esi-tn)
    (#.rdi-offset edi-tn)
    (#.r8-offset r8d-tn)
    (#.r9-offset r9d-tn)
    (#.r10-offset r10d-tn)
    (#.r11-offset r11d-tn)
    (#.r12-offset r12d-tn)
    (#.r13-offset r13d-tn)
    (#.r14-offset r14d-tn)
    (#.r15-offset r15d-tn)))
