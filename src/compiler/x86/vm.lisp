;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: python-x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/x86/vm.lisp,v 1.11 2006/06/30 18:41:32 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM defintion noise for the x86.
;;;
;;; Written by William Lott
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996, 1997, 1998.
;;;

(in-package :x86)


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
;;; Note: the encoding here is different then that used by the chip.  We
;;; use this encoding so that the compiler thinks that AX (and EAX) overlap
;;; AL and AH instead of AL and CL.
;;; 
(eval-when (compile load eval)
  (defvar *byte-register-names* (make-array 8 :initial-element nil)))
;;;
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
  (defvar *word-register-names* (make-array 16 :initial-element nil)))
;;;
(defreg ax 0 :word)
(defreg cx 2 :word)
(defreg dx 4 :word)
(defreg bx 6 :word)
(defreg sp 8 :word)
(defreg bp 10 :word)
(defreg si 12 :word)
(defreg di 14 :word)
;;;
(defregset word-regs ax cx dx bx si di)

;;; Double Word registers.
;;;
(eval-when (compile load eval)
  (defvar *dword-register-names* (make-array 16 :initial-element nil)))
;;;
(defreg eax 0 :dword)
(defreg ecx 2 :dword)
(defreg edx 4 :dword)
(defreg ebx 6 :dword)
(defreg esp 8 :dword)
(defreg ebp 10 :dword)
(defreg esi 12 :dword)
(defreg edi 14 :dword)
;;;
(defregset dword-regs eax ecx edx ebx esi edi)

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
;;; consider bytes to take one unit, and words or dwords to take two.  We
;;; don't need to tell the difference between words and dwords, because
;;; you can't put two words in a dword.

(define-storage-base registers :finite :size 16)

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

  ;; Non-immediate contstants in the constant pool
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
  #+double-double
  (double-double-stack stack :element-size 4)	; double-double-float
  #+long-float
  (long-stack stack :element-size 3)	; long-floats.
  (complex-single-stack stack :element-size 2)	; complex-single-floats
  (complex-double-stack stack :element-size 4)	; complex-double-floats
  #+long-float
  (complex-long-stack stack :element-size 6)	; complex-long-floats
  #+double-double
  (complex-double-double-stack stack :element-size 8)	; complex-double-double-floats
  
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
	   :locations #.dword-regs
	   :element-size 2
;	   :reserve-locations (#.eax-offset)
	   :constant-scs (immediate)
	   :save-p t
	   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
		  :locations #.dword-regs
		  :element-size 2
;		  :reserve-locations (#.eax-offset)
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
	   :locations #.dword-regs
	   :element-size 2
;	   :reserve-locations (#.eax-offset)
	   :constant-scs (immediate)
	   :save-p t
	   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
	      :locations #.dword-regs
	      :element-size 2
;	      :reserve-locations (#.eax-offset)
	      :constant-scs (immediate)
	      :save-p t
	      :alternate-scs (signed-stack))
  (unsigned-reg registers
		:locations #.dword-regs
		:element-size 2
;		:reserve-locations (#.eax-offset)
		:constant-scs (immediate)
		:save-p t
		:alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (word-reg registers
	    :locations #.word-regs
	    :element-size 2
;	    :reserve-locations (#.ax-offset)
	    )
  (byte-reg registers
	    :locations #.byte-regs
;	    :reserve-locations (#.al-offset #.ah-offset)
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

  #+double-double
  (double-double-reg float-registers
		     :locations (0 2 4 6)
		     :element-size 2
		     :constant-scs ()
		     :save-p t
		     :alternate-scs (double-double-stack))
  
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
  #+double-double
  (complex-double-double-reg float-registers
		      :locations (0 4)
		      :element-size 4
		      :constant-scs ()
		      :save-p t
		      :alternate-scs (complex-double-double-stack))

  ;; A catch or unwind block.
  (catch-block stack :element-size vm:catch-block-size)
  )

(eval-when (compile load eval)

(defconstant byte-sc-names '(base-char-reg byte-reg base-char-stack))
(defconstant word-sc-names '(word-reg))
(defconstant dword-sc-names
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

(def-random-reg-tns unsigned-reg eax ebx ecx edx ebp esp edi esi)
(def-random-reg-tns word-reg ax bx cx dx bp sp di si)
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

(defconstant cfp-offset ebp-offset)	; pfw - needed by stuff in /code
					; related to sigcontext stuff

;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 3)

;;; Names, Offsets, and TNs to use for the argument registers.
;;; 
(defconstant register-arg-names '(edx edi esi))
(defregset register-arg-offsets edx edi esi)
(defparameter register-arg-tns (list edx-tn edi-tn esi-tn))

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
			       *dword-register-names*))))
	 (or (and name-vec
		  (< -1 offset (length name-vec))
		  (svref name-vec offset))
	     (format nil "<Unknown Reg: off=~D, sc=~A>" offset sc-name))))
      (float-registers (format nil "FR~D" offset))
      (stack (format nil "S~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed")
      (noise (symbol-name (sc-name sc))))))
