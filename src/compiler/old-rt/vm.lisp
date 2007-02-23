;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the VM definition for the RT.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; SB and SC definition:

(define-storage-base registers :finite :size 16)
(define-storage-base stack :unbounded :size 8)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)

;;;
;;; Non-immediate constants in the constant pool.
(define-storage-class constant 6 constant)

;;; Immediate numeric constants.  A constant TN will be created in the
;;; most restrictive SC possible:
;;;
;;;    short-immediate = (unsigned-byte 4)
;;;        Self-explanatory: short-immediate operands on the RT are unsigned.
;;;
;;;    unsigned-immediate = (unsigned-byte 15)
;;;        Used for unsigned immediate operands.  15 instead of 16 so that this
;;;        is a subtype of Immediate.
;;;
;;;    immediate = (integer #x7FFF #x-7FFF)
;;;        The funny lower bound guarantees that the negation of an immediate
;;;        is still an immediate.
;;;
(define-storage-class short-immediate 7 immediate-constant)
(define-storage-class unsigned-immediate 8 immediate-constant)
(define-storage-class immediate 9 immediate-constant)

;;; Allows simple loading of unboxed string-chars.
;;;
(define-storage-class immediate-string-char 10 immediate-constant)

;;; Objects that are easier to create using immediate loads than to fetch
;;; from the constant pool, but which aren't directly usable as immediate
;;; operands.  These are only recognized by move VOPs.
;;;
(define-storage-class random-immediate 11 immediate-constant)

;;;
;;; Descriptor objects stored on the stack.
(define-storage-class stack 4 stack)

;;;
;;; Untagged string-chars stored on the stack.
(define-storage-class string-char-stack 5 stack)

;;;
;;; Objects that can be stored in any register (immediate objects).
(define-storage-class any-reg 0 registers
  :locations (0 1 2 3 4 5 7 8 9 10 11 15)
  :constant-scs (constant short-immediate unsigned-immediate immediate
			  immediate-string-char random-immediate)
  :save-p t
  :alternate-scs (stack))

;;;
;;; Pointer objects that must be seen by GC.
(define-storage-class descriptor-reg 1 registers
  :locations (1 3 4 5 7 8 9 10 11 15)
  :constant-scs (constant short-immediate unsigned-immediate immediate
			  immediate-string-char random-immediate)
  :save-p t
  :alternate-scs (stack))

;;;
;;; Objects that must not be seen by GC (unboxed numbers).
(define-storage-class non-descriptor-reg 2 registers
  :locations (0 2))

;;;
;;; String-chars represented without tag bits (looks like a fixnum).
(define-storage-class string-char-reg 3 registers
  :locations (0 1 2 3 4 5 7 8 9 10 11 15)
  :constant-scs (immediate-string-char)
  :save-p t
  :alternate-scs (string-char-stack))

;;; A catch or unwind block.
;;;
(define-storage-class catch-block 12 stack
  :element-size system:%catch-block-size)


;;;; Primitive type definition:
;;;
;;;    For now, the primitive types we support are primarily for discrimination
;;; rather than representation selection.

(def-primitive-type t (descriptor-reg))
(defvar *any-primitive-type* (primitive-type-or-lose 't))

(def-primitive-type simple-string (descriptor-reg))
(def-primitive-type simple-vector (descriptor-reg))
(def-primitive-type simple-bit-vector (descriptor-reg))

(def-primitive-type string-char (string-char-reg any-reg))
(def-primitive-type fixnum (any-reg))
(def-primitive-type short-float (any-reg))
(def-primitive-type long-float (descriptor-reg))
(def-primitive-type bignum (descriptor-reg))
(def-primitive-type ratio (descriptor-reg))
(def-primitive-type complex (descriptor-reg))
(def-primitive-type function (descriptor-reg))

;;; We make List a primitive type, since it is useful for discrimination of
;;; generic sequence function arguments.  This means that Cons and Symbol can't
;;; be primitive types.
(def-primitive-type list (descriptor-reg))

;;; Dummy primitive type for catch block TNs.
(def-primitive-type catch-block (catch-block) :type nil)


;;; Primitive-Type-Of  --  Interface
;;;
;;;    Return the most restrictive primitive type that contains Object.
;;;
(defun primitive-type-of (object)
  (let ((type (ctype-of object)))
    (cond ((not (member-type-p type)) (primitive-type type))
	  ((equal (member-type-members type) '(nil))
	   (primitive-type-or-lose 'list))
	  (t *any-primitive-type*))))


;;; PRIMITIVE-TYPE  --  Interface
;;;
;;;    Return the primitive type corresponding to a type descriptor structure.
;;; If a fixnum or an interesting simple vector, then return the appropriate
;;; type, otherwise return *any-primitive-type*.  The second value is true when
;;; the primitive type is exactly equivalent to the argument Lisp type.
;;;
;;; In a bootstrapping situation, we should be careful to use the correct
;;; values for the system parameters.
;;;
;;; Note: DEFUN-CACHED caches this translation; If the primitive type
;;; translation is ever changed (due to hardware configuration switches, etc.),
;;; then PRIMITIVE-TYPE-CACHE-CLEAR must be called to clear old cached
;;; information.
;;;
(defun-cached (primitive-type
	       :hash-function (lambda (x)
				(logand (cache-hash-eq x) #x1FF))
	       :hash-bits 9
	       :values 2
	       :default (values nil :empty))
	      ((type eq))
  (declare (type ctype type))
  (etypecase type
    (named-type
     (case (named-type-name type)
       ((t bignum ratio string-char function)
	(values (primitive-type-or-lose (named-type-name type)) t))
       (cons
	(values (primitive-type-or-lose 'list) nil))
       (standard-char
	(values (primitive-type-or-lose 'string-char) nil))
       (t
	(values *any-primitive-type* nil))))
    (member-type
     (let* ((members (member-type-members type))
	    (res (primitive-type-of (first members))))
       (dolist (mem (rest members) (values res nil))
	 (unless (eq (primitive-type-of mem) res)
	   (return (values *any-primitive-type* nil))))))
    (numeric-type
     (if (not (eq (numeric-type-complexp type) :real))
	 (values *any-primitive-type* nil)
	 (case (numeric-type-class type)
	   (integer
	    (let ((lo (numeric-type-low type))
		  (hi (numeric-type-high type)))
	      (if (and hi lo
		       (>= lo most-negative-fixnum)
		       (<= hi most-positive-fixnum))
		  (values (primitive-type-or-lose 'fixnum)
			  (and (= lo most-negative-fixnum)
			       (= hi most-positive-fixnum)))
		  (values *any-primitive-type* nil))))
	   (t
	    (values *any-primitive-type* nil)))))
    (array-type
     (if (array-type-complexp type)
	 (values *any-primitive-type* nil)
	 (let ((dims (array-type-dimensions type))
	       (etype (array-type-specialized-element-type type)))
	   (if (and (consp dims) (null (rest dims)))
	       (let ((len-wild (eq (first dims) '*)))
		 (case (type-specifier etype)
		   (string-char
		    (values (primitive-type-or-lose 'simple-string)
			    len-wild))
		   (bit
		    (values (primitive-type-or-lose 'simple-bit-vector)
			    len-wild))
		   ((t)
		    (values (primitive-type-or-lose 'simple-vector)
			    len-wild))
		   (t
		    (values *any-primitive-type* nil))))
	       (values *any-primitive-type* nil)))))
    (union-type
     (if (type= type (specifier-type 'list))
	 (values (primitive-type-or-lose 'list) t)
	 (let ((types (union-type-types type)))
	   (multiple-value-bind (res exact)
				(primitive-type (first types))
	     (dolist (type (rest types) (values res exact))
	       (multiple-value-bind (ptype ptype-exact)
				    (primitive-type type)
		 (unless ptype-exact (setq exact nil))
		 (unless (eq ptype res)
		   (return (values *any-primitive-type* nil)))))))))
    (function-type (values (primitive-type-or-lose 'function) t))
    (ctype
     (values *any-primitive-type* nil))))


;;;; Totally magical registers:

;;; Pointer to the current frame: 
(defparameter fp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset 13))

;;; Pointer to the current constant pool:
(defparameter env-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset 14))

;;; Pointer to the stack top:
(defparameter sp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset 6))

;;; Binding stack pointer:
(defparameter bs-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset 12))

;;; TN with 0 offset, used for 0/reg args when we want 0.
(defparameter zero-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset 0))


;;;; Side-effect classes:

(def-boolean-attribute vop
  ;;
  ;; Nothing for now...
  any
  )


;;;; Constant creation:

;;; Immediate-Constant-SC  --  Interface
;;;
;;;    If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(defun immediate-constant-sc (value)
  (typecase value
    ((unsigned-byte 4)
     (sc-number-or-lose 'short-immediate))
    ((unsigned-byte 15)
     (sc-number-or-lose 'unsigned-immediate))
    ((integer #x-7FFF #x7FFF)
     (sc-number-or-lose 'immediate))
    ((or fixnum (member nil t))
     (sc-number-or-lose 'random-immediate))
    (t
     ;;
     ;; ### hack around bug in (typep x 'string-char)
     (if (and (characterp value) (string-char-p value))
	 (sc-number-or-lose 'immediate-string-char)
	 nil))))


;;;; Function call parameters:
;;;

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (sc-number-or-lose 'descriptor-reg))
(defconstant stack-arg-scn (sc-number-or-lose 'stack))

;;; Offsets of special registers...
;;;
(eval-when (compile load eval)
  (defconstant return-pc-offset 15)
  (defconstant env-offset 14)
  (defconstant argument-count-offset 0)
  (defconstant argument-pointer-offset 11)
  (defconstant old-fp-offset 10)
  (defconstant sp-offset 6)
  (defconstant call-name-offset 9))

;;; Offsets of special stack frame locations...
;;;
(eval-when (compile load eval)
  (defconstant old-fp-save-offset 0)
  (defconstant return-pc-save-offset 1)
  (defconstant env-save-offset 2))


(defparameter nargs-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset argument-count-offset))

(defparameter old-fp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset old-fp-offset))
  
(defparameter pc-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset return-pc-offset))


(eval-when (compile load eval)

;;; The offsets within the register-arg SC that we pass values in, first
;;; value first.
;;;
(defconstant register-arg-offsets '(1 3 5))

;;; The number of arguments/return values passed in registers.
;;;
(defconstant register-arg-count 3)

;;; The fourth register argument (used only in miscop calls).
;;;
(defconstant a3-offset 4)

); Eval-When (Compile Load Eval)


;;; A list of TNs describing the register arguments.
;;;
(defparameter register-argument-tns
  (mapcar #'(lambda (n)
	      (make-random-tn :kind :normal
			      :sc (sc-or-lose 'descriptor-reg)
			      :offset n))
	  register-arg-offsets))


;;; LOCATION-PRINT-NAME  --  Interface
;;;
;;;    This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
;;;
(defun location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn)))))
    (if (eq sb 'registers)
	(svref '#(NL0 A0 NL1 A1 A3 A2 SP L0 L1 L2 L3 L4
		      BS CONT ENV PC)
	       (tn-offset tn))
	(format nil "~A~D" (char (string sb) 0) (tn-offset tn)))))
