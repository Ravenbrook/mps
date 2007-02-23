;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/primtype.lisp,v 1.25 2006/06/30 18:41:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the machine independent aspects of the object
;;; representation and primitive types.
;;;
;;; Written by William Lott.
;;; Alpha conversion by Sean Hallgren.
;;;
(in-package "VM")


;;;; Primitive Type Definitions

;;; *Anything*
;;; 
(def-primitive-type t (descriptor-reg))
(defvar *any-primitive-type* (primitive-type-or-lose 't))
(setf (backend-any-primitive-type *target-backend*) *any-primitive-type*)

;;; Primitive integer types that fit in registers.
;;;
(def-primitive-type positive-fixnum (any-reg signed-reg unsigned-reg)
  :type (unsigned-byte #-amd64 29 #+amd64 61))
#-(or alpha amd64)
(def-primitive-type unsigned-byte-31 (signed-reg unsigned-reg descriptor-reg)
  :type (unsigned-byte 31))
#-(or alpha amd64)
(def-primitive-type unsigned-byte-32 (unsigned-reg descriptor-reg)
  :type (unsigned-byte 32))
#+(or alpha amd64)
(def-primitive-type unsigned-byte-63 (signed-reg unsigned-reg descriptor-reg)
  :type (unsigned-byte 63))
#+(or alpha amd64)
(def-primitive-type unsigned-byte-64 (unsigned-reg descriptor-reg)
  :type (unsigned-byte 64))
(def-primitive-type fixnum (any-reg signed-reg)
  :type (signed-byte #-amd64 30 #+amd64 62))
#-(or alpha amd64)
(def-primitive-type signed-byte-32 (signed-reg descriptor-reg)
  :type (signed-byte 32))
#+(or alpha amd64)
(def-primitive-type signed-byte-64 (signed-reg descriptor-reg)
  :type (signed-byte 64))


(defvar *fixnum-primitive-type* (primitive-type-or-lose 'fixnum))

(def-primitive-type-alias tagged-num (:or positive-fixnum fixnum))
(def-primitive-type-alias unsigned-num (:or #-(or alpha amd64) unsigned-byte-32
					    #-(or alpha amd64) unsigned-byte-31
					    #+(or alpha amd64) unsigned-byte-64
					    #+(or alpha amd64) unsigned-byte-63
					    positive-fixnum))
(def-primitive-type-alias signed-num (:or #-(or alpha amd64) signed-byte-32
					  #+(or alpha amd64) signed-byte-64
					  fixnum
					  #-(or alpha amd64) unsigned-byte-31
					  #+(or alpha amd64) unsigned-byte-63
					  positive-fixnum))
#+(and sparc-v9 sparc-v8plus)
(progn
(def-primitive-type unsigned-byte-63 (signed64-reg unsigned64-reg descriptor-reg)
  :type (unsigned-byte 63))

(def-primitive-type signed-byte-64 (signed64-reg descriptor-reg)
  :type (signed-byte 64))

(def-primitive-type unsigned-byte-64 (unsigned64-reg descriptor-reg)
  :type (unsigned-byte 64))

(def-primitive-type-alias signed64-num (:or signed-byte-64
					    unsigned-byte-63))

(def-primitive-type-alias unsigned64-num (:or unsigned-byte-64
					      unsigned-byte-63))

)

;;; Other primitive immediate types.
(def-primitive-type base-char (base-char-reg any-reg))

;;; Primitive pointer types.
;;; 
(def-primitive-type function (descriptor-reg))
(def-primitive-type list (descriptor-reg))
(def-primitive-type instance (descriptor-reg))

(def-primitive-type funcallable-instance (descriptor-reg))

;;; Primitive other-pointer number types.
;;;
(def-primitive-type bignum (descriptor-reg))
(def-primitive-type ratio (descriptor-reg))
(def-primitive-type complex (descriptor-reg))
(def-primitive-type single-float (single-reg descriptor-reg))
(def-primitive-type double-float (double-reg descriptor-reg))
#+long-float
(def-primitive-type long-float (long-reg descriptor-reg))

#+double-double
(def-primitive-type double-double-float (double-double-reg descriptor-reg))

(def-primitive-type complex-single-float (complex-single-reg descriptor-reg)
  :type (complex single-float))
(def-primitive-type complex-double-float (complex-double-reg descriptor-reg)
  :type (complex double-float))
#+long-float
(def-primitive-type complex-long-float (complex-long-reg descriptor-reg)
  :type (complex long-float))
#+double-double
(def-primitive-type complex-double-double-float (complex-double-double-reg descriptor-reg)
  :type (complex double-double-float))

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
(def-primitive-type simple-array-signed-byte-8 (descriptor-reg)
  :type (simple-array (signed-byte 8) (*)))
(def-primitive-type simple-array-signed-byte-16 (descriptor-reg)
  :type (simple-array (signed-byte 16) (*)))
(def-primitive-type simple-array-signed-byte-30 (descriptor-reg)
  :type (simple-array (signed-byte 30) (*)))
(def-primitive-type simple-array-signed-byte-62 (descriptor-reg)
  :type (simple-array (signed-byte 62) (*)))
(def-primitive-type simple-array-signed-byte-32 (descriptor-reg)
  :type (simple-array (signed-byte 32) (*)))
(def-primitive-type simple-array-single-float (descriptor-reg)
  :type (simple-array single-float (*)))
(def-primitive-type simple-array-double-float (descriptor-reg)
  :type (simple-array double-float (*)))
#+double-double
(def-primitive-type simple-array-double-double-float (descriptor-reg)
  :type (simple-array double-double-float (*)))
#+long-float
(def-primitive-type simple-array-long-float (descriptor-reg)
  :type (simple-array long-float (*)))
(def-primitive-type simple-array-complex-single-float (descriptor-reg)
  :type (simple-array (complex single-float) (*)))
(def-primitive-type simple-array-complex-double-float (descriptor-reg)
  :type (simple-array (complex double-float) (*)))
#+long-float
(def-primitive-type simple-array-complex-long-float (descriptor-reg)
  :type (simple-array (complex long-float) (*)))
#+double-double
(def-primitive-type simple-array-complex-double-double-float (descriptor-reg)
  :type (simple-array (complex double-double-float) (*)))

;;; Note: The complex array types are not inclueded, 'cause it is pointless to
;;; restrict VOPs to them.

;;; Other primitive other-pointer types.
;;; 
(def-primitive-type system-area-pointer (sap-reg descriptor-reg))
(def-primitive-type weak-pointer (descriptor-reg))

;;; Random primitive types that don't exist at the LISP level.
;;; 
(def-primitive-type catch-block (catch-block) :type nil)



;;;; Primitive-type-of and friends.

;;; Primitive-Type-Of  --  Interface
;;;
;;;    Return the most restrictive primitive type that contains Object.
;;;
(def-vm-support-routine primitive-type-of (object)
  (let ((type (ctype-of object)))
    (cond ((not (member-type-p type)) (primitive-type type))
	  ((equal (member-type-members type) '(nil))
	   (primitive-type-or-lose 'list *backend*))
	  (t
	   *any-primitive-type*))))

;;; 
(defvar *simple-array-primitive-types*
  '((base-char . simple-string)
    (bit . simple-bit-vector)
    ((unsigned-byte 2) . simple-array-unsigned-byte-2)
    ((unsigned-byte 4) . simple-array-unsigned-byte-4)
    ((unsigned-byte 8) . simple-array-unsigned-byte-8)
    ((unsigned-byte 16) . simple-array-unsigned-byte-16)
    ((unsigned-byte 32) . simple-array-unsigned-byte-32)
    ((signed-byte 8) . simple-array-signed-byte-8)
    ((signed-byte 16) . simple-array-signed-byte-16)
    (fixnum . #-amd64 simple-array-signed-byte-30
     #+amd64 simple-array-signed-byte-62)
    ((signed-byte 32) . simple-array-signed-byte-32)
    (single-float . simple-array-single-float)
    (double-float . simple-array-double-float)
    #+long-float (long-float . simple-array-long-float)
    #+double-double
    (double-double-float . simple-array-double-double-float)
    ((complex single-float) . simple-array-complex-single-float)
    ((complex double-float) . simple-array-complex-double-float)
    #+long-float
    ((complex long-float) . simple-array-complex-long-float)
    #+double-double ((complex double-double-float) . simple-array-complex-double-double-float)
    (t . simple-vector))
  "An a-list for mapping simple array element types to their
  corresponding primitive types.")


;;; Return the primitive type corresponding to a type descriptor
;;; structure. The second value is true when the primitive type is
;;; exactly equivalent to the argument Lisp type.
;;;
;;; In a bootstrapping situation, we should be careful to use the
;;; correct values for the system parameters.
;;;
;;; We need an aux function because we need to use both def-vm-support-routine
;;; and defun-cached.
;;; 
(def-vm-support-routine primitive-type (type)
  (primitive-type-aux type))
;;;
(defun-cached (primitive-type-aux
	       :hash-function (lambda (x)
				(logand (cache-hash-eq x) #x1FF))
	       :hash-bits 9
	       :values 2
	       :default (values nil :empty))
	      ((type eq))
  (declare (type ctype type))
  (macrolet ((any () '(values *any-primitive-type* nil))
	     (exactly (type)
	       `(values (primitive-type-or-lose ',type *backend*) t))
	     (part-of (type)
	       `(values (primitive-type-or-lose ',type *backend*) nil)))
    (flet ((maybe-numeric-type-union (t1 t2)
	     (let ((t1-name (c::primitive-type-name t1))
		   (t2-name (c::primitive-type-name t2)))
	       (case t1-name
		 (positive-fixnum
		  (if (or (eq t2-name 'fixnum)
			  (eq t2-name #-(or alpha amd64) 'signed-byte-32
				      #+(or alpha amd64) 'signed-byte-64)
			  (eq t2-name #-(or alpha amd64) 'unsigned-byte-31
				      #+(or alpha amd64) 'unsigned-byte-63)
			  (eq t2-name #-(or alpha amd64) 'unsigned-byte-32
				      #+(or alpha amd64) 'unsigned-byte-64)
			  #+(and sparc-v9 sparc-v8plus)
			  (eq t2-name 'signed-byte-64)
			  #+(and sparc-v9 sparc-v8plus)
			  (eq t2-name 'unsigned-byte-64)
			  )
		      t2))
		 (fixnum
		  (case t2-name
		    (#-(or alpha amd64) signed-byte-32
		       #+(or alpha amd64) signed-byte-64 t2)

		    #+(and sparc-v9 sparc-v8plus)
		    (signed-byte-64 t2)

		    (#-(or alpha amd64) unsigned-byte-31
		       #+(or alpha amd64) unsigned-byte-63 
		     (primitive-type-or-lose
		      #-(or alpha amd64) 'signed-byte-32
		      #+(or alpha amd64) 'signed-byte-64
		      *backend*))))
		 
		 (#-(or alpha amd64) signed-byte-32
		    #+(or alpha amd64) signed-byte-64
		  (if (eq t2-name #-(or alpha amd64) 'unsigned-byte-31
				  #+(or alpha amd64) 'unsigned-byte-63)
		      t1))
		 #+(and sparc-v9 sparc-v8plus)
		 (signed-byte-64
		  t1)

		 (#-(or alpha amd64) unsigned-byte-31
		    #+(or alpha amd64) unsigned-byte-63
		  (if (eq t2-name #-(or alpha amd64) 'unsigned-byte-32
				  #+(or alpha amd64) 'unsigned-byte-64)
		      t2))))))
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
				  '((positive-fixnum 0 #.(1- (ash 1 #-amd64 29
								  #+amd64 61)))

				    #-(or alpha amd64)
				    (unsigned-byte-31 0 #.(1- (ash 1 31)))

				    #-(or alpha amd64)
				    (unsigned-byte-32 0 #.(1- (ash 1 32)))

				    #+(or alpha amd64 (and sparc-v9 sparc-v8plus))
				    (unsigned-byte-63 0 #.(1- (ash 1 63)))

				    #+(or alpha amd64 (and sparc-v9 sparc-v8plus))
				    (unsigned-byte-64 0 #.(1- (ash 1 64)))

				    (fixnum #.(ash -1 #-amd64 29 #+amd64 61)
				     #.(1- (ash 1 #-amd64 29 #+amd64 61)))

				    #-(or alpha amd64)
				    (signed-byte-32 #.(ash -1 31)
						    #.(1- (ash 1 31)))

				    #+(or alpha amd64 (and sparc-v9 sparc-v8plus))
				    (signed-byte-64 #.(ash -1 63)
						    #.(1- (ash 1 63))))
				 (if (or (< hi (ash -1 #-amd64 29 #+amd64 61))
					 (> lo (1- (ash 1 #-amd64 29 #+amd64 61))))
				     (part-of bignum)
				     (any)))
			  (let ((type (car spec))
				(min (cadr spec))
				(max (caddr spec)))
			    (when (<= min lo hi max)
			      (return (values
				       (primitive-type-or-lose type *backend*)
				       (and (= lo min) (= hi max))))))))
		       ((or (and hi (< hi most-negative-fixnum))
			    (and lo (> lo most-positive-fixnum)))
			(part-of bignum))
		       (t
			(any))))
		(float
		 (let ((exact (and (null lo) (null hi))))
		   (case (numeric-type-format type)
		     ((short-float single-float)
		      (values (primitive-type-or-lose 'single-float *backend*)
			      exact))
		     ((double-float #-long-float long-float)
		      (values (primitive-type-or-lose 'double-float *backend*)
			      exact))
		     #+long-float
		     (long-float
		      (values (primitive-type-or-lose 'long-float *backend*)
			      exact))
		     #+double-double
		     (double-double-float
		      (values (primitive-type-or-lose 'double-double-float *backend*)
			      exact))
		     (t
		      (any)))))
		(t
		 (any))))
	     (:complex
	      (if (eq (numeric-type-class type) 'float)
		  (let ((exact (and (null lo) (null hi))))
		    (case (numeric-type-format type)
		      ((short-float single-float)
		       (values (primitive-type-or-lose 'complex-single-float
						       *backend*)
			       exact))
		      ((double-float #-long-float long-float)
		       (values (primitive-type-or-lose 'complex-double-float
						       *backend*)
			       exact))
		      #+long-float
		      (long-float
		       (values (primitive-type-or-lose 'complex-long-float
						       *backend*)
			       exact))
		      #+double-double
		      (double-double-float
		       (values (primitive-type-or-lose 'complex-double-double-float
						       *backend*)
			       exact))
		      (t
		       (part-of complex))))
		  (part-of complex)))
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
		   (values (primitive-type-or-lose ptype *backend*)
			   (eq (first dims) '*))
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
		       (let ((new-ptype
			      (or (maybe-numeric-type-union res ptype)
				  (maybe-numeric-type-union ptype res))))
			 (if new-ptype
			     (setq res new-ptype)
			     (return (any)))))))))))
	(member-type
	 (let* ((members (member-type-members type))
		(res (primitive-type-of (first members))))
	   (dolist (mem (rest members) (values res nil))
	     (let ((ptype (primitive-type-of mem)))
	       (unless (eq ptype res)
		 (let ((new-ptype (or (maybe-numeric-type-union res ptype)
				      (maybe-numeric-type-union ptype res))))
		   (if new-ptype
		       (setq res new-ptype)
		       (return (any)))))))))
	(named-type
	 (ecase (named-type-name type)
	   ((t *) (values *any-primitive-type* t))
	   ((nil) (any))))
	(kernel::built-in-class
	 (case (%class-name type)
	   ((complex function instance system-area-pointer weak-pointer)
	    (values (primitive-type-or-lose (%class-name type) *backend*) t))
	   (funcallable-instance
	    (part-of function))
	   (base-char
	    (exactly base-char))
	   (t
	    (any))))
	(function-type
	 (exactly function))
	(cons-type
	 (part-of list))
	(class
	 (if (csubtypep type (specifier-type 'function))
	     (part-of function)
	     (part-of instance)))
	(ctype
	 (any))))))
