;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/vm.lisp,v 1.9 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition for the IBM RT.
;;;
;;; Written by William Lott, Rob Maclachlan, and Bill Chiles.
;;;

(in-package "RT")



;;;; SB and SC definition:

(define-storage-base registers :finite :size 16)
(define-storage-base mc68881-float-registers :finite :size 8)
(define-storage-base AFPA-float-registers :finite :size 64)
(define-storage-base FPA-float-registers :finite :size 16)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
;; These are constants in components.
(define-storage-base constant :non-packed)
;; Anything I can cookup out of nowhere and store somewhere.
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
		       `(eval-when (compile load eval)
			  (defconstant ,constant-name ,index))
		       `(export ',constant-name)
		       forms)))
       (index 0 (1+ index))
       (classes classes (cdr classes)))
      ((null classes)
       (nreverse forms))))

(define-storage-classes

  ;; Non-immediate contstants in the constant pool
  (constant constant)


  (immediate immediate-constant)
  (null immediate-constant)


  ;; The control stack.  (Scanned by GC)
  (control-stack control-stack)

  ;; The non-descriptor stack SC's.
  (signed-stack non-descriptor-stack) ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack) ; (unsigned-byte 32)
  (base-char-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
  (double-stack non-descriptor-stack :element-size 2) ; double floats.


  ;; **** Things that can go in the non-descriptor registers.

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg registers
   :locations (9 10 11 12 13 14 0 2 3 4)
   :constant-scs (immediate)
   :reserve-locations (0 2 3 4)
   :save-p t
   :alternate-scs (control-stack))

  ;; Descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations (9 10 11 12 13 14)
   ;; Immediate (and constant) for moving NULL around (at least).
   :constant-scs (constant immediate null)
   :save-p t
   :alternate-scs (control-stack))

  ;; Non-Descriptor characters.
  (base-char-reg registers
   :locations (0 2 3 4)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (base-char-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space).
  (sap-reg registers
   ;; Exclude R0 here because the instructions we would like to use with sap
   ;; TN's use R0 as the constant zero instead of using the contents of R0.
   :locations (2 3 4)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
   :locations (0 2 3 4)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations (0 2 3 4)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations (0 2 3 4))

  ;; Word-aligned pointers that cannot be in R0.  Used for temporaries and to
  ;; hold stack pointers.
  (word-pointer-reg registers
   :locations (2 3 4 9 10 11 12 13 14)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointers to the interior of objects.  Used only as an temporary.
  (interior-reg registers
   :locations (15))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor mc68881-single-floats.
  (mc68881-single-reg mc68881-float-registers
   :locations (0 1 2 3 4 5 6 7)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))
  ;; Non-Descriptor mc68881-double-floats.
  (mc68881-double-reg mc68881-float-registers
   :locations (0 1 2 3 4 5 6 7)
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  ;; Non-Descriptor FPA-single-floats.
  (FPA-single-reg FPA-float-registers
   ;; 14 and 15 are status and exception registers.
   :locations (0 1 2 3 4 5 6 7 8 9 10 11 12 13)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))
  ;; Non-Descriptor FPA-double-floats.
  (FPA-double-reg FPA-float-registers
   :locations (0 2 4 6 8 10 12) ;14 and 15 are status and exception registers.
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  ;; Non-Descriptor AFPA-single-floats.  0,1 reserved for loading "immediate"
  ;; operands.
  (AFPA-single-reg AFPA-float-registers
   :locations (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
	       24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44
	       45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))
  ;; Non-Descriptor AFPA-double-floats.  0 reserved for loading "immediate"
  ;; operands.
  (AFPA-double-reg AFPA-float-registers
   :locations (2 4 6 8 10 12 14 16 18 20 22 24 26 28
	       30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))


  ;; A catch or unwind block.
  (catch-block control-stack :element-size vm:catch-block-size))


(export '(single-reg-sc-number double-reg-sc-number))
(defconstant single-reg-sc-number
  (list mc68881-single-reg-sc-number
	FPA-single-reg-sc-number
	AFPA-single-reg-sc-number))
(defconstant double-reg-sc-number
  (list mc68881-double-reg-sc-number
	FPA-double-reg-sc-number
	AFPA-double-reg-sc-number))


;;;; Primitive Type Definitions

;;; *any-primitive-type*
;;;
;;; Other VOP/VM definition files use this when writing interface code for the
;;; compiler.
;;;
(def-primitive-type t (descriptor-reg))
(defvar *any-primitive-type* (primitive-type-or-lose 't))
(setf (c:backend-any-primitive-type c:*target-backend*)
      (c:primitive-type-or-lose 't))

;;; Primitive integer types that fit in registers.
;;;
(def-primitive-type positive-fixnum (any-reg signed-reg unsigned-reg)
  :type (unsigned-byte 29))
(def-primitive-type unsigned-byte-31 (signed-reg unsigned-reg descriptor-reg)
  :type (unsigned-byte 31))
(def-primitive-type unsigned-byte-32 (unsigned-reg descriptor-reg)
  :type (unsigned-byte 32))
(def-primitive-type fixnum (any-reg signed-reg)
  :type (signed-byte 30))
(def-primitive-type signed-byte-32 (signed-reg descriptor-reg)
  :type (signed-byte 32))
(def-primitive-type word-pointer (word-pointer-reg descriptor-reg)
  :type fixnum)


;;; *word-pointer-type*
;;;
(defvar *word-pointer-type* (primitive-type-or-lose 'word-pointer))


;;; *fixnum-primitive-type*
;;;
;;; Other VOP/VM definition files use this when writing interface code for the
;;; compiler.
;;;
(defvar *fixnum-primitive-type* (primitive-type-or-lose 'fixnum))

(def-primitive-type-alias tagged-num (:or positive-fixnum fixnum))
(def-primitive-type-alias unsigned-num (:or unsigned-byte-32
					    unsigned-byte-31
					    positive-fixnum))
(def-primitive-type-alias signed-num (:or signed-byte-32
					  fixnum
					  unsigned-byte-31
					  positive-fixnum))

;;; Other primitive immediate types.
(def-primitive-type base-char (base-char-reg any-reg))

;;; Primitive pointer types.
;;; 
(def-primitive-type function (descriptor-reg))
(def-primitive-type list (descriptor-reg))
(def-primitive-type structure (descriptor-reg))

;;; Primitive other-pointer number types.
;;; 
(def-primitive-type bignum (descriptor-reg))
(def-primitive-type ratio (descriptor-reg))
(def-primitive-type complex (descriptor-reg))
(def-primitive-type mc68881-single-float (mc68881-single-reg descriptor-reg)
  :type single-float)
(def-primitive-type mc68881-double-float (mc68881-double-reg descriptor-reg)
  :type double-float)
(def-primitive-type FPA-single-float (FPA-single-reg descriptor-reg)
  :type single-float)
(def-primitive-type FPA-double-float (FPA-double-reg descriptor-reg)
  :type double-float)
(def-primitive-type AFPA-single-float (AFPA-single-reg descriptor-reg)
  :type single-float)
(def-primitive-type AFPA-double-float (AFPA-double-reg descriptor-reg)
  :type double-float)
(def-primitive-type any-single-float (descriptor-reg)
  :type single-float)
(def-primitive-type any-double-float (descriptor-reg)
  :type double-float)

;;; Primitive other-pointer array types.
;;; 
(def-primitive-type simple-string (descriptor-reg) :type simple-base-string)
(def-primitive-type simple-bit-vector (descriptor-reg))
(def-primitive-type simple-vector (descriptor-reg))
(def-primitive-type simple-array-unsigned-byte-2 (descriptor-reg)
  :type (simple-array (unsigned-byte 2) (*)))
(def-primitive-type simple-array-unsigned-byte-4 (descriptor-reg)
  :type (simple-array (unsigned-byte 4) (*)))
(def-primitive-type simple-array-unsigned-byte-8 (descriptor-reg)
  :type (simple-array (unsigned-byte 8) (*)))
(def-primitive-type simple-array-unsigned-byte-16 (descriptor-reg)
  :type (simple-array (unsigned-byte 16) (*)))
(def-primitive-type simple-array-unsigned-byte-32 (descriptor-reg)
  :type (simple-array (unsigned-byte 32) (*)))
(def-primitive-type simple-array-single-float (descriptor-reg)
  :type (simple-array single-float (*)))
(def-primitive-type simple-array-double-float (descriptor-reg)
  :type (simple-array double-float (*)))

;;; Note: The complex array types are not included, because it is pointless to
;;; restrict VOPs to them.

;;; Other primitive other-pointer types.
;;; 
(def-primitive-type system-area-pointer (sap-reg descriptor-reg))
(def-primitive-type weak-pointer (descriptor-reg))

;;; Random primitive types that don't exist at the LISP level.
;;; 
(def-primitive-type random (non-descriptor-reg) :type nil)
(def-primitive-type interior (interior-reg) :type nil)
(def-primitive-type catch-block (catch-block) :type nil)




;;;; PRIMITIVE-TYPE-OF and friends.

;;; PRIMITIVE-TYPE-OF  --  Interface.
;;;
;;; Return the most restrictive primitive type that contains Object.
;;;
(def-vm-support-routine primitive-type-of (object)
  (let ((type (ctype-of object)))
    (cond ((not (member-type-p type)) (primitive-type type))
	  ((equal (member-type-members type) '(nil))
	   (primitive-type-or-lose 'list))
	  (t
	   *any-primitive-type*))))

;;; 
(defvar *simple-array-primitive-types*
  '((base-char . simple-string)
    (string-char . simple-string)
    (bit . simple-bit-vector)
    ((unsigned-byte 2) . simple-array-unsigned-byte-2)
    ((unsigned-byte 4) . simple-array-unsigned-byte-4)
    ((unsigned-byte 8) . simple-array-unsigned-byte-8)
    ((unsigned-byte 16) . simple-array-unsigned-byte-16)
    ((unsigned-byte 32) . simple-array-unsigned-byte-32)
    (single-float . simple-array-single-float)
    (double-float . simple-array-double-float)
    (t . simple-vector))
  "An a-list for mapping simple array element types to their
  corresponding primitive types.")

(defvar *target-float-hardware*)

;;; PRIMITIVE-TYPE -- Internal Interface.
;;;
;;; Return the primitive type corresponding to a type descriptor
;;; structure. The second value is true when the primitive type is
;;; exactly equivalent to the argument Lisp type.
;;;
;;; In a bootstrapping situation, we should be careful to use the
;;; correct values for the system parameters.
;;;
(def-vm-support-routine primitive-type (type)
  (declare (type ctype type))
  (macrolet ((any () '(values *any-primitive-type* nil))
	     (exactly (type) `(values (primitive-type-or-lose ',type) t))
	     (part-of (type) `(values (primitive-type-or-lose ',type) nil)))
    (etypecase type
      (numeric-type
       (let ((lo (numeric-type-low type))
	     (hi (numeric-type-high type)))
	 (case (numeric-type-complexp type)
	   (:real
	    (case (numeric-type-class type)
	      (integer
	       (cond ((and hi lo)
		      (dolist (spec
			       '((positive-fixnum 0 #.(1- (ash 1 29)))
				 (unsigned-byte-31 0 #.(1- (ash 1 31)))
				 (unsigned-byte-32 0 #.(1- (ash 1 32)))
				 (fixnum #.(ash -1 29) #.(1- (ash 1 29)))
				 (signed-byte-32 #.(ash -1 31)
						 #.(1- (ash 1 31))))
			       (if (or (< hi (ash -1 29))
				       (> lo (1- (ash 1 29))))
				   (part-of bignum)
				   (any)))
			(let ((type (car spec))
			      (min (cadr spec))
			      (max (caddr spec)))
			  (when (<= min lo hi max)
			    (return (values (primitive-type-or-lose type)
					    (and (= lo min) (= hi max))))))))
		     ((or (and hi (< hi most-negative-fixnum))
			  (and lo (> lo most-positive-fixnum)))
		      (part-of bignum))
		     (t
		      (any))))
	      (float
	       (float-primitive-type lo hi type))
	      (t
	       (any))))
	   (:complex
	    (part-of complex))
	   (t
	    (any)))))
      (array-type
       (if (array-type-complexp type)
	   (any)
	   (let* ((dims (array-type-dimensions type))
		  (etype (array-type-specialized-element-type type))
		  (type-spec (type-specifier etype))
		  (ptype (cdr (assoc type-spec *simple-array-primitive-types*
				     :test #'equal))))
	     (if (and (consp dims) (null (rest dims)) ptype)
		 (values (primitive-type-or-lose ptype) (eq (first dims) '*))
		 (any)))))
      (union-type
       (if (type= type (specifier-type 'list))
	   (exactly list)
	   (let ((types (union-type-types type)))
	     (multiple-value-bind (res exact)
				  (primitive-type (first types))
	       (dolist (type (rest types) (values res exact))
		 (multiple-value-bind (ptype ptype-exact)
				      (primitive-type type)
		   (unless ptype-exact (setq exact nil))
		   (unless (eq ptype res)
		     (return (any)))))))))
      (member-type
       (let* ((members (member-type-members type))
	      (res (primitive-type-of (first members))))
	 (dolist (mem (rest members) (values res nil))
	   (unless (eq (primitive-type-of mem) res)
	     (return (values *any-primitive-type* nil))))))
      (named-type
       (case (named-type-name type)
	 ((t bignum ratio complex function system-area-pointer weak-pointer
	     structure)
	  (values (primitive-type-or-lose (named-type-name type)) t))
	 ((character base-char string-char)
	  (exactly base-char))
	 (standard-char
	  (part-of base-char))
	 (cons
	  (part-of list))
	 (t
	  (any))))
      (function-type
       (exactly function))
      (structure-type
       (part-of structure))
      (ctype
       (any)))))

;;; FLOAT-PRIMITIVE-TYPE -- Internal.
;;;
(defun float-primitive-type (lo hi type)
  (let ((exact (and (null lo) (null hi))))
    (case (numeric-type-format type)
      ((short-float single-float)
       (ecase *target-float-hardware*
	 (:mc68881
	  (values (primitive-type-or-lose 'mc68881-single-float) exact))
	 (:fpa
	  (values (primitive-type-or-lose 'fpa-single-float) exact))
	 (:afpa
	  (values (primitive-type-or-lose 'afpa-single-float) exact))
	 (:any
	  (values (primitive-type-or-lose 'any-single-float) exact))))
      ((double-float long-float)
       (ecase *target-float-hardware*
	 (:mc68881
	  (values (primitive-type-or-lose 'mc68881-double-float) exact))
	 (:fpa
	  (values (primitive-type-or-lose 'fpa-double-float) exact))
	 (:afpa
	  (values (primitive-type-or-lose 'afpa-double-float) exact))
	 (:any
	  (values (primitive-type-or-lose 'any-double-float) exact))))
      (t
       (values *any-primitive-type* nil)))))



;;;; Magical Registers

;;; Other VOP/VM definition files use the definitions on this page when writing
;;; interface code for the compiler.
;;;

(eval-when (compile eval load)
  (defconstant nargs-offset 0)
  (defconstant nsp-offset 1)
  (defconstant nl0-offset 2)
  (defconstant ocfp-offset 3)
  (defconstant nfp-offset 4)
  (defconstant csp-offset 5)
  (defconstant cfp-offset 6)
  (defconstant code-offset 7)
  (defconstant null-offset 8)
  (defconstant cname-offset 9)
  (defconstant lexenv-offset 10)
  (defconstant lra-offset 11)
  (defconstant a0-offset 12)
  (defconstant a1-offset 13)
  (defconstant a2-offset 14)
  (defconstant lip-offset 15))

;;; Lisp-interior-pointer register.
;;;
(defparameter lip-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset lip-offset))

;;; Nil.
;;;
(defparameter null-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset null-offset))


;;; Frame Pointer.
;;;
(defparameter cfp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset cfp-offset))


;;; Control stack pointer.
;;;
(defparameter csp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset csp-offset))

;;; Number stack pointer.
;;;
(defparameter nsp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset nsp-offset))

;;; Code Pointer.
;;;
(defparameter code-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset code-offset))

;;; Random non-descriptor tn
;;;
(defparameter nl0-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'non-descriptor-reg)
		  :offset nl0-offset))



;;;; Side-Effect Classes

(def-boolean-attribute vop
  any)


;;;; Constants

;;; IMMEDIATE-CONSTANT-SC  --  Interface.
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((or fixnum character system-area-pointer)
     (sc-number-or-lose 'immediate))
    (null
     (sc-number-or-lose 'null))
    (symbol
     (if (static-symbol-p value)
	 (sc-number-or-lose 'immediate)
	 nil))))



;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
;;; Other VOP/VM definition files use this when writing interface code for the
;;; compiler.
;;;
(defconstant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(defconstant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(defconstant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))


(eval-when (compile load eval)

;;; Offsets of special stack frame locations.
;;;
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

); Eval-When (Compile Load Eval)  


(defparameter nargs-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset nargs-offset))

(defparameter ocfp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset ocfp-offset))

(defparameter lra-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset lra-offset))


(eval-when (compile load eval)

;;; The number of arguments/return values passed in registers.
;;;
;;; Other VOP/VM definition files use this when writing interface code for the
;;; compiler.
;;;
(defconstant register-arg-count 3)

;;; The offsets within the register-arg SC where we supply values, first value
;;; first.
;;;
;;; Other VOP/VM definition files use this when writing interface code for the
;;; compiler.
;;;
(defconstant register-arg-offsets '(12 13 14))

;;; Names to use for the argument registers.
;;; 
(defconstant register-arg-names '(a0 a1 a2))

); Eval-When (Compile Load Eval)


;;; A list of TN's describing the register arguments.
;;;
(defparameter register-arg-tns
  (mapcar #'(lambda (n)
	      (make-random-tn :kind :normal
			      :sc (sc-or-lose 'descriptor-reg)
			      :offset n))
	  register-arg-offsets))



;;;; LOCATION-PRINT-NAME.

(defconstant register-names #("NARGS" "NSP" "NL0" "OCFP" "NFP"
			      "CSP" "CFP" "CODE" "NULL" "CNAME" "LEXENV"
			      "LRA" "A0" "A1" "A2" "LIP"))

;;; LOCATION-PRINT-NAME  --  Interface.
;;;
;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
;;;
(def-vm-support-routine location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
	(offset (tn-offset tn)))
    (ecase sb
      (registers (svref register-names (tn-offset tn)))
      ((mc68881-float-registers FPA-float-registers AFPA-float-registers)
       (format nil "F~D" offset))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))
