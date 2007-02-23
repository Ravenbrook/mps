;;; -*- Mode: Lisp; Package: LISP; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/pred.lisp,v 1.60 2006/06/30 18:41:22 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Predicate functions for CMU Common Lisp.
;;;
;;; Written by William Lott.
;;;

(in-package "KERNEL")
(export '(%instancep instance fixnump bignump bitp ratiop weak-pointer-p
	  %typep class-cell-typep))

#+double-double
(export '(double-double-float-p))

(in-package "SYSTEM")
(export '(system-area-pointer system-area-pointer-p))

(in-package "LISP")

(export '(typep null symbolp atom consp listp numberp integerp rationalp
	  floatp complexp characterp stringp bit-vector-p vectorp
	  simple-vector-p simple-string-p simple-bit-vector-p arrayp
	  functionp compiled-function-p eq eql equal equalp not
	  type-of upgraded-array-element-type realp
	  ;; Names of types...
	  array atom bignum bit bit-vector character
	  compiled-function complex cons double-float
	  fixnum float function integer keyword list long-float nil
	  null number ratio rational real sequence short-float signed-byte
	  simple-array simple-bit-vector simple-string simple-vector
	  single-float standard-char base-char string symbol t
	  unsigned-byte vector satisfies))



;;;; Primitive predicates.  These must be supported by the compiler.

(eval-when (compile eval)
  (defparameter primitive-predicates
    '(array-header-p
      arrayp
      atom
      base-char-p
      bignump
      bit-vector-p
      characterp
      code-component-p
      consp
      compiled-function-p
      complexp
      complex-double-float-p
      complex-float-p
      #+long-float complex-long-float-p
      #+double-double complex-double-double-float-p
      complex-rational-p
      complex-single-float-p
      #+double-double double-double-float-p
      double-float-p
      fdefn-p
      fixnump
      floatp
      functionp
      integerp
      listp
      long-float-p
      lra-p
      not
      null
      numberp
      rationalp
      ratiop
      realp
      scavenger-hook-p
      short-float-p
      simple-array-p
      simple-bit-vector-p
      simple-string-p
      simple-vector-p
      single-float-p
      stringp
      %instancep
      symbolp
      system-area-pointer-p
      weak-pointer-p
      vectorp
      unsigned-byte-32-p
      signed-byte-32-p
      simple-array-unsigned-byte-2-p
      simple-array-unsigned-byte-4-p
      simple-array-unsigned-byte-8-p
      simple-array-unsigned-byte-16-p
      simple-array-unsigned-byte-32-p
      simple-array-signed-byte-8-p
      simple-array-signed-byte-16-p
      simple-array-signed-byte-30-p
      simple-array-signed-byte-32-p
      simple-array-single-float-p
      simple-array-double-float-p
      #+long-float simple-array-long-float-p
      #+double-double simple-array-double-double-float-p
      simple-array-complex-single-float-p
      simple-array-complex-double-float-p
      #+long-float simple-array-complex-long-float-p
      #+double-double simple-array-complex-double-double-float-p
      )))

(macrolet
    ((frob ()
       `(progn
	  ,@(mapcar #'(lambda (pred)
			`(defun ,pred (object)
			   ,(format nil
				    "Return T if OBJECT is a~:[~;n~] ~(~A~) ~
				     and NIL otherwise."
				    (find (schar (string pred) 0) "AEIOUaeiou")
				    (string pred))
			   (,pred object)))
		    primitive-predicates))))
  (frob))


;;;; TYPE-OF -- public.
;;;
;;; Return the specifier for the type of object.  This is not simply
;;; (type-specifier (ctype-of object)) because ctype-of has different goals
;;; than type-of.  In particular, speed is more important than precision, and
;;; it is not permitted to return member types.
;;; 
(defun type-of (object)
  "Return the type of OBJECT."
  (typecase object
    ((or array complex)
     (type-specifier (ctype-of object)))
    (integer
     `(integer ,object ,object))
    ((member t)
     'boolean)
    (keyword
     'keyword)
    (standard-char
     'standard-char)
    (t
     (let* ((class (layout-class (layout-of object)))
	    (name (%class-name class)))
       (if (%instancep object)
	   (if (eq name 'alien-internals:alien-value)
	       `(alien:alien ,(alien-internals:unparse-alien-type
			       (alien-internals:alien-value-type object)))
	       (let ((proper-name (class-proper-name class)))
		 (if (kernel::class-p proper-name)
		     (%class-pcl-class proper-name)
		     proper-name)))
	   name)))))


;;;; UPGRADED-ARRAY-ELEMENT-TYPE  --  public
;;;
(defun upgraded-array-element-type (spec &optional environment)
  "Return the element type that will actually be used to implement an array
   with the specifier :ELEMENT-TYPE Spec."
  ;; Type expansion (TYPE-EXPAND) currently doesn't handle environments.
  (declare (ignore environment))
  (type-specifier
   (array-type-specialized-element-type
    (specifier-type `(array ,spec)))))

;;;; SUBTYPEP -- public.
;;;
;;; Just parse the type specifiers and call csubtype.
;;; 
(defun subtypep (type1 type2 &optional environment)
  "Return two values indicating the relationship between type1 and type2:
  T and T: type1 definitely is a subtype of type2.
  NIL and T: type1 definitely is not a subtype of type2.
  NIL and NIL: who knows?"
  (declare (ignore environment))
  (csubtypep (specifier-type type1) (specifier-type type2)))


;;;; TYPEP:

(declaim (start-block typep %typep class-cell-typep))

;;; TYPEP -- public.
;;;
;;; Just call %typep
;;; 
(defun typep (object type &optional environment)
  "Return T iff OBJECT is of type TYPE."
  (declare (ignore environment))
  (%typep object type))

  
;;; %TYPEP -- internal.
;;;
;;; The actual typep engine.  The compiler only generates calls to this
;;; function when it can't figure out anything more intelligent to do.
;;; 
(defun %typep (object specifier)
  (%%typep object
	   (if (ctype-p specifier)
	       specifier
	       (specifier-type specifier))))
;;;
(defun %%typep (object type)
  (declare (type ctype type))
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((* t) t)
       ((nil) nil)))
    (numeric-type
     (and (numberp object)
	  (let ((num (if (complexp object) (realpart object) object)))
	    (ecase (numeric-type-class type)
	      (integer (integerp num))
	      (rational (rationalp num))
	      (float
	       (ecase (numeric-type-format type)
		 (short-float (typep num 'short-float))
		 (single-float (typep num 'single-float))
		 (double-float (typep num 'double-float))
		 (long-float (typep num 'long-float))
		 (double-double-float (typep num 'double-double-float))
		 ((nil) (floatp num))))
	      ((nil) t)))
	  (flet ((bound-test (val)
		   (let ((low (numeric-type-low type))
			 (high (numeric-type-high type)))
		     (and (cond ((null low) t)
				((listp low) (> val (car low)))
				(t (>= val low)))
			  (cond ((null high) t)
				((listp high) (< val (car high)))
				(t (<= val high)))))))
	    (ecase (numeric-type-complexp type)
	      ((nil) t)
	      (:complex
	       (and (complexp object)
		    (bound-test (realpart object))
		    (bound-test (imagpart object))))
	      (:real
	       (and (not (complexp object))
		    (bound-test object)))))))
    (array-type
     (and (arrayp object)
	  (ecase (array-type-complexp type)
	    ((t) (not (typep object 'simple-array)))
	    ((nil) (typep object 'simple-array))
	    ((* :maybe) t))
	  (or (eq (array-type-dimensions type) '*)
	      (do ((want (array-type-dimensions type) (cdr want))
		   (got (array-dimensions object) (cdr got)))
		  ((and (null want) (null got)) t)
		(unless (and want got
			     (or (eq (car want) '*)
				 (= (car want) (car got))))
		  (return nil))))
	  (if (unknown-type-p (array-type-element-type type))
	      ;; better to fail this way than to get bogosities like
	      ;;   (TYPEP (MAKE-ARRAY 11) '(ARRAY SOME-UNDEFINED-TYPE)) => T
	      (error "~@<unknown element type in array type: ~2I~_~S~:>"
		     (type-specifier type))
	      t)
	  (or (eq (array-type-element-type type) *wild-type*)
	      (values (type= (array-type-specialized-element-type type)
			     (specifier-type (array-element-type
					      object)))))))
    (member-type
     (if (member object (member-type-members type)) t))
    (kernel::class
     (class-typep (layout-of object) type object))
    (union-type
     (some (lambda (type) (%%typep object type))
	   (union-type-types type)))
    (intersection-type
     (every (lambda (type) (%%typep object type))
	    (intersection-type-types type)))
    (cons-type
     (and (consp object)
	  (%%typep (car object) (cons-type-car-type type))
	  (%%typep (cdr object) (cons-type-cdr-type type))))
    (unknown-type
     ;; Parse it again to make sure it's really undefined.
     (let ((reparse (specifier-type (unknown-type-specifier type))))
       (if (typep reparse 'unknown-type)
	   (error "Unknown type specifier: ~S"
		  (unknown-type-specifier reparse))
	   (%%typep object reparse))))
    (negation-type
     (not (%%typep object (negation-type-type type))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
	    (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	 (and
	  (every (lambda (spec) (%%typep object (specifier-type spec)))
		 (rest hairy-spec)))
	 ;; Note: it should be safe to skip OR here, because union
	 ;; types can always be represented as UNION-TYPE in general
	 ;; or other CTYPEs in special cases; we never need to use
	 ;; HAIRY-TYPE for them.
	 (not
	  (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
	    (error "Invalid type specifier: ~S" hairy-spec))
	  (not (%%typep object (specifier-type (cadr hairy-spec)))))
	 (satisfies
	  (unless (and (listp hairy-spec) (= (length hairy-spec) 2))
	    (error "Invalid type specifier: ~S" hairy-spec))
	  (let ((fn (cadr hairy-spec)))
	    (if (funcall (typecase fn
			   (function fn)
			   (symbol (symbol-function fn))
			   (t
			    (coerce fn 'function)))
			 object)
		t
		nil))))))
    (alien-type-type
     (alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (function-type
     (error "Function types are not a legal argument to TYPEP:~%  ~S"
	    (type-specifier type)))))


;;; CLASS-CELL-TYPEP  --  Interface
;;;
;;;    Do type test from a class cell, allowing forward reference and
;;; redefinition.
;;;
(defun class-cell-typep (obj-layout cell object)
  (let ((class (class-cell-class cell)))
    (unless class
      (error "Class has not yet been defined: ~S" (class-cell-name cell)))
    (class-typep obj-layout class object)))


;;; CLASS-TYPEP  --  Internal
;;;
;;;    Test whether Obj-Layout is from an instance of Class.
;;;
(defun class-typep (obj-layout class object)
  (declare (optimize speed))
  (when (layout-invalid obj-layout)
    (if (and (typep (kernel::class-of object) 'kernel::standard-class) object)
	(setq obj-layout (pcl::check-wrapper-validity object))
	(error "TYPEP on obsolete object (was class ~S)."
	       (class-proper-name (layout-class obj-layout)))))
  (let ((layout (%class-layout class))
	(obj-inherits (layout-inherits obj-layout)))
    (when (layout-invalid layout)
      (error "Class is currently invalid: ~S" class))
    (or (eq obj-layout layout)
	(dotimes (i (length obj-inherits) nil)
	  (when (eq (svref obj-inherits i) layout)
	    (return t))))))

(declaim (end-block))


;;;; Equality predicates.

;;; EQ -- public.
;;;
;;; Real simple, 'cause the compiler takes care of it.
;;; 

(defun eq (obj1 obj2)
  "Return T if OBJ1 and OBJ2 are the same object, otherwise NIL."
  (eq obj1 obj2))


;;; EQUAL -- public.
;;;
(defun equal (x y)
  "Returns T if X and Y are EQL or if they are structured components
  whose elements are EQUAL.  Strings and bit-vectors are EQUAL if they
  are the same length and have indentical components.  Other arrays must be
  EQ to be EQUAL."
  (cond ((eql x y) t)
	((consp x)
	 (and (consp y)
	      (equal (car x) (car y))
	      (equal (cdr x) (cdr y))))
	((stringp x)
	 (and (stringp y) (string= x y)))
	((pathnamep x)
	 (and (pathnamep y) (pathname= x y)))
	((bit-vector-p x)
	 (and (bit-vector-p y)
	      (= (the fixnum (length x))
		 (the fixnum (length y)))
	      (do ((i 0 (1+ i))
		   (length (length x)))
		  ((= i length) t)
		(declare (fixnum i))
		(or (= (the fixnum (bit x i))
		       (the fixnum (bit y i)))
		    (return nil)))))
	(t nil)))

;;; EQUALP -- public.
;;; 
(defun equalp (x y)
  "Just like EQUAL, but more liberal in several respects.
  Numbers may be of different types, as long as the values are identical
  after coercion.  Characters may differ in alphabetic case.  Vectors and
  arrays must have identical dimensions and EQUALP elements, but may differ
  in their type restriction."
  (cond ((eq x y) t)
	((characterp x) (and (characterp y) (char-equal x y)))
	((numberp x) (and (numberp y) (= x y)))
	((consp x)
	 (and (consp y)
	      (equalp (car x) (car y))
	      (equalp (cdr x) (cdr y))))
	((pathnamep x)
	 (and (pathnamep y) (pathname= x y)))
	((hash-table-p x)
	 (and (hash-table-p y)
	      (eql (hash-table-count x) (hash-table-count y))
	      (eql (hash-table-test x) (hash-table-test y))
	      (with-hash-table-iterator (next x)
		(loop
		 (multiple-value-bind (more x-key x-value)
		     (next)
		   (cond (more
			  (multiple-value-bind (y-value foundp)
			      (gethash x-key y)
			    (unless (and foundp (equalp x-value y-value))
			      (return nil))))
			 (t
			  (return t))))))))
	((%instancep x)
	 (let* ((layout-x (%instance-layout x))
		(len (layout-length layout-x)))
	   (and (%instancep y)
		(eq layout-x (%instance-layout y))
		(structure-class-p (layout-class layout-x))
		(do ((i 1 (1+ i)))
		    ((= i len) t)
		  (declare (fixnum i))
		  (let ((x-el (%instance-ref x i))
			(y-el (%instance-ref y i)))
		    (unless (or (eq x-el y-el)
				(equalp x-el y-el))
		      (return nil)))))))
	((vectorp x)
	 (let ((length (length x)))
	   (and (vectorp y)
		(= length (length y))
		(dotimes (i length t)
		  (let ((x-el (aref x i))
			(y-el (aref y i)))
		    (unless (or (eq x-el y-el)
				(equalp x-el y-el))
		      (return nil)))))))
	((arrayp x)
	 (and (arrayp y)
	      (= (array-rank x) (array-rank y))
	      (dotimes (axis (array-rank x) t)
		(unless (= (array-dimension x axis)
			   (array-dimension y axis))
		  (return nil)))
	      (dotimes (index (array-total-size x) t)
		(let ((x-el (row-major-aref x index))
		      (y-el (row-major-aref y index)))
		  (unless (or (eq x-el y-el)
			      (equalp x-el y-el))
		    (return nil))))))
	(t nil)))
