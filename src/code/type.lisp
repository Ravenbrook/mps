;;; -*- Mode: Lisp; Package: KERNEL; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/type.lisp,v 1.74 2006/07/18 18:31:38 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the definition of non-CLASS types (e.g. subtypes of
;;; interesting BUILT-IN-CLASSes) and the interfaces to the type system.
;;; Common Lisp type specifiers are parsed into a somewhat canonical internal
;;; type representation that supports type union, intersection, etc.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "KERNEL")
(use-package "ALIEN-INTERNALS")

(export '(function-type-nargs code-component code-component-p lra lra-p))
(export '(make-alien-type-type alien-type-type
	  alien-type-type-p alien-type-type-alien-type
	  *unparse-function-type-simplify*))
(import 'c-call:void)
(export 'void)

(in-package "EXTENSIONS")
(export '(constant-argument instance *use-implementation-types*))

(in-package "KERNEL")

(export '(extract-function-type))

(with-cold-load-init-forms)

;;; Structures & Type Classes

(define-type-class values)
(define-type-class function)
(define-type-class constant values)
(define-type-class named)
(define-type-class hairy)
(define-type-class negation)
(define-type-class number)
(define-type-class array)
(define-type-class member)
(define-type-class union)
(define-type-class intersection)
(define-type-class alien)
(define-type-class cons)

;;; The Args-Type structure is used both to represent Values types and
;;; and Function types.
;;;
(defstruct (args-type (:include ctype)
		      (:print-function %print-type))
  ;;
  ;; Lists of the type for each required and optional argument.
  (required nil :type list)
  (optional nil :type list)
  ;;
  ;; The type for the rest arg.  NIL if there is no rest arg.
  (rest nil :type (or ctype null))
  ;;
  ;; True if keyword arguments are specified.
  (keyp nil :type boolean)
  ;;
  ;; List of key-info structures describing the keyword arguments.
  (keywords nil :type list)
  ;;
  ;; True if other keywords are allowed.
  (allowp nil :type boolean))

(defstruct (key-info (:pure t))
  ;;
  ;; The keyword.
  (name (required-argument) :type symbol)
  ;;
  ;; Type of this argument.
  (type (required-argument) :type ctype))

(defstruct (values-type
	    (:include args-type
		      (:class-info (type-class-or-lose 'values)))
	    (:print-function %print-type)))

(declaim (freeze-type values-type))

(defstruct (function-type
	    (:include args-type
		      (:class-info (type-class-or-lose 'function)))
	    (:print-function %print-type))
  ;;
  ;; True if the arguments are unrestrictive, i.e. *.
  (wild-args nil :type boolean)
  ;;
  ;; Type describing the return values.  This is a values type
  ;; when multiple values were specified for the return.
  (returns (required-argument) :type ctype))

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARGUMENT "type
;;; specifier", which is only meaningful in function argument type specifiers
;;; used within the compiler.
;;;
(defstruct (constant-type (:include ctype
				    (:class-info (type-class-or-lose 'constant)))
			  (:print-function %print-type))
  ;;
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (required-argument) :type ctype))

;;; The NAMED-TYPE is used to represent *, T and NIL.  These types must be
;;; super or sub types of all types, not just classes and * & NIL aren't
;;; classes anyway, so it wouldn't make much sense to make them built-in
;;; classes.
;;;
(defstruct (named-type (:include ctype
				 (:class-info (type-class-or-lose 'named)))
		       (:print-function %print-type))
  (name nil :type symbol))

;;; The Hairy-Type represents anything too wierd to be described reasonably or
;;; to be useful, such as AND, NOT and SATISFIES and unknown types.  We just
;;; remember the original type spec.
;;;
(defstruct (hairy-type (:include ctype
				 (:class-info (type-class-or-lose 'hairy))
				 (:enumerable t))
		       (:print-function %print-type)
		       (:pure nil))
  ;;
  ;; The Common Lisp type-specifier.
  (specifier nil :type t))

(defstruct (negation-type (:include ctype
				    (:class-info (type-class-or-lose 'negation))
				    ;; FIXME: is this right?  It's
				    ;; what they had before, anyway
				    (:enumerable t))
			  (:copier nil)
			  (:pure nil))
  (type (required-argument) :type ctype))

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet defined).
;;; We make this distinction since we don't want to complain about types that
;;; are hairy but defined.
;;;
(defstruct (unknown-type (:include hairy-type)))

;;; The Numeric-Type is used to represent all numeric types, including things
;;; such as FIXNUM.
(defstruct (numeric-type (:include ctype
				   (:class-info (type-class-or-lose 'number)))
			 (:constructor %make-numeric-type)
			 (:print-function %print-type))
  ;;
  ;; The kind of numeric type we have.  NIL if not specified (just NUMBER or
  ;; COMPLEX).
  (class nil :type (member integer rational float nil))
  ;;
  ;; Format for a float type.  NIL if not specified or not a float.  Formats
  ;; which don't exist in a given implementation don't appear here.
  (format nil :type (or float-format null))
  ;;
  ;; Is this a complex numeric type?  Null if unknown (only in NUMBER.)
  (complexp :real :type (member :real :complex nil))
  ;;
  ;; The upper and lower bounds on the value.  If null, there is no bound.  If
  ;; a list of a number, the bound is exclusive.  Integer types never have
  ;; exclusive bounds.
  (low nil :type (or number cons null))
  (high nil :type (or number cons null)))

(defun type-bound-number (x)
  (if (consp x)
      (destructuring-bind (result) x result)
      x))

(defun make-numeric-type (&key class format (complexp :real) low high
			       enumerable)
  ;; if interval is empty
  (if (and low
	   high
	   (if (or (consp low) (consp high)) ; if either bound is exclusive
	       (>= (type-bound-number low) (type-bound-number high))
	       (> low high)))
      *empty-type*
      (multiple-value-bind (canonical-low canonical-high)
	  (case class
	    (integer
	     ;; INTEGER types always have their LOW and HIGH bounds
	     ;; represented as inclusive, not exclusive values.
	     (values (if (consp low)
			 (1+ (type-bound-number low))
			 low)
		     (if (consp high)
			 (1- (type-bound-number high))
			 high)))
	    (t 
	     ;; no canonicalization necessary
	     (values low high)))
	(when (and (eq class 'rational)
		   (integerp canonical-low)
		   (integerp canonical-high)
		   (= canonical-low canonical-high))
	  (setf class 'integer))
	(%make-numeric-type :class class
			    :format format
			    :complexp complexp
			    :low canonical-low
			    :high canonical-high
			    :enumerable enumerable))))
 
(defun modified-numeric-type (base
			      &key
			      (class      (numeric-type-class      base))
			      (format     (numeric-type-format     base))
			      (complexp   (numeric-type-complexp   base))
			      (low        (numeric-type-low        base))
			      (high       (numeric-type-high       base))
			      (enumerable (numeric-type-enumerable base)))
  (make-numeric-type :class class
		     :format format
		     :complexp complexp
		     :low low
		     :high high
		     :enumerable enumerable))

;;; The Array-Type is used to represent all array types, including things such
;;; as SIMPLE-STRING.
;;;
(defstruct (array-type (:include ctype
				 (:class-info (type-class-or-lose 'array)))
		       (:print-function %print-type))
  ;;
  ;; The dimensions of the array.  * if unspecified.  If a dimension is
  ;; unspecified, it is *.
  (dimensions '* :type (or list (member *)))
  ;;
  ;; Is this not a simple array type?
  (complexp :maybe :type (member t nil :maybe))
  ;;
  ;; The element type as originally specified.
  (element-type (required-argument) :type ctype)
  ;;
  ;; The element type as it is specialized in this implementation.
  (specialized-element-type *wild-type* :type ctype))

;;; The Member-Type represents uses of the MEMBER type specifier.  We bother
;;; with this at this level because MEMBER types are fairly important and union
;;; and intersection are well defined.

(defstruct (member-type (:include ctype
				  (:class-info (type-class-or-lose 'member))
				  (:enumerable t))
			(:constructor %make-member-type (members))
			(:print-function %print-type)
			(:pure nil))
  ;;
  ;; The things in the set, with no duplications.
  (members nil :type list))

(defun make-member-type (&key members)
  (declare (type list members))
  ;; make sure that we've removed duplicates
  (assert (= (length members) (length (remove-duplicates members))))
  ;; if we have a pair of zeros (e.g. 0.0d0 and -0.0d0), then we can
  ;; canonicalize to (DOUBLE-FLOAT 0.0d0 0.0d0), because numeric
  ;; ranges are compared by arithmetic operators (while MEMBERship is
  ;; compared by EQL).  -- CSR, 2003-04-23
  (let ((singlep (subsetp '(-0.0f0 0.0f0) members))
	(doublep (subsetp '(-0.0d0 0.0d0) members))
	#+long-float
	(longp (subsetp '(-0.0l0 0.0l0) members)))
    (if (or singlep doublep #+long-float longp)
	(let (union-types)
	  (when singlep
	    (push (ctype-of 0.0f0) union-types)
	    (setf members (set-difference members '(-0.0f0 0.0f0))))
	  (when doublep
	    (push (ctype-of 0.0d0) union-types)
	    (setf members (set-difference members '(-0.0d0 0.0d0))))
	  #+long-float
	  (when longp
	    (push (ctype-of 0.0l0) union-types)
	    (setf members (set-difference members '(-0.0l0 0.0l0))))
	  (assert (not (null union-types)))
	  (make-union-type (if (null members)
			       union-types
			       (cons (%make-member-type members)
				     union-types))))
	(%make-member-type members))))

;;; The Union-Type represents uses of the OR type specifier which can't be
;;; canonicalized to something simpler.  Canonical form:
;;;
;;; 1] There is never more than one Member-Type component.
;;; 2] There are never any Union-Type components.
;;;
(defstruct (union-type (:include ctype
				 (:class-info (type-class-or-lose 'union)))
		       (:constructor %make-union-type (enumerable types))
		       (:print-function %print-type))
  ;;
  ;; The types in the union.
  (types nil :type list :read-only t))

(defun make-union-type (types)
  (declare (list types))
  (%make-union-type (every #'type-enumerable types) types))

(defstruct (intersection-type
	     (:include ctype
		       (:class-info (type-class-or-lose 'intersection)))
	     (:constructor make-intersection-type (enumerable types))
	     (:print-function %print-type))
  (types nil :type list :read-only t))

(defstruct (alien-type-type
	    (:include ctype
		      (:class-info (type-class-or-lose 'alien)))
	    (:print-function %print-type)
	    (:constructor %make-alien-type-type (alien-type)))
  (alien-type nil :type alien-type))

;;; The Cons-Type is used to represent cons types.
;;;
(defun type-*-to-t (type)
  (if (type= type *wild-type*)
      *universal-type*
      type))

(defstruct (cons-type (:include ctype
				(:class-info (type-class-or-lose 'cons)))
		      (:constructor
		       ;; ANSI says that for CAR and CDR subtype
		       ;; specifiers '* is equivalent to T. In order
		       ;; to avoid special cases in SUBTYPEP and
		       ;; possibly elsewhere, we slam all CONS-TYPE
		       ;; objects into canonical form w.r.t. this
		       ;; equivalence at creation time.
		       %make-cons-type (car-raw-type
					cdr-raw-type
					&aux
					(car-type (type-*-to-t car-raw-type))
					(cdr-type (type-*-to-t cdr-raw-type))))
		      (:print-function %print-type)
		      (:copier nil))
  ;; the CAR and CDR element types (to support ANSI (CONS FOO BAR) types)
  ;;
  ;; FIXME: Most or all other type structure slots could also be :READ-ONLY.
  (car-type (required-argument) :type ctype :read-only t)
  (cdr-type (required-argument) :type ctype :read-only t))

(defun make-cons-type (car-type cdr-type)
  (if (or (eq car-type *empty-type*)
	  (eq cdr-type *empty-type*))
      *empty-type*
      (%make-cons-type car-type cdr-type)))



;;;
(defvar *use-implementation-types* t
  "*Use-Implementation-Types* is a semi-public flag which determines how
   restrictive we are in determining type membership.  If two types are the
   same in the implementation, then we will consider them them the same when
   this switch is on.  When it is off, we try to be as restrictive as the
   language allows, allowing us to detect more errors.  Currently, this only
   affects array types.")

(cold-load-init (setq *use-implementation-types* t))
(declaim (type boolean *use-implementation-types*))

;;; DELEGATE-COMPLEX-{SUBTYPEP-ARG2,INTERSECTION}  --  Interface
;;;
;;;    These functions are used as method for types which need a complex
;;; subtypep method to handle some superclasses, but cover a subtree of the
;;; type graph (i.e. there is no simple way for any other type class to be a
;;; subtype.)  There are always still complex ways, namely UNION and MEMBER
;;; types, so we must give TYPE1's method a chance to run, instead of
;;; immediately returning NIL, T.
;;;
(defun delegate-complex-subtypep-arg2 (type1 type2)
  (let ((subtypep-arg1
	 (type-class-complex-subtypep-arg1
	  (type-class-info type1))))
    (if subtypep-arg1
	(funcall subtypep-arg1 type1 type2)
	(values nil t))))

(defun delegate-complex-intersection (type1 type2)
  (let ((method (type-class-complex-intersection (type-class-info type1))))
    (if (and method (not (eq method #'delegate-complex-intersection)))
	(funcall method type2 type1)
	(hierarchical-intersection2 type1 type2))))

;;; HAS-SUPERCLASSES-COMPLEX-SUBTYPEP-ARG1  --  Internal
;;;
;;;    Used by DEFINE-SUPERCLASSES to define the SUBTYPE-ARG1 method.  Info is
;;; a list of conses (SUPERCLASS-CLASS . {GUARD-TYPE-SPECIFIER | NIL}).  Will
;;; never be called with a hairy type as type2, since the hairy type type2
;;; method gets first crack.
;;;
(defun has-superclasses-complex-subtypep-arg1 (type1 type2 info)
  ;; If TYPE2 might be concealing something related to our class
  ;; hierarchy
  (if (type-might-contain-other-types-p type2)
      ;; too confusing, gotta punt
      (values nil nil)
      ;; ordinary case expected by old CMU CL code, where the taxonomy
      ;; of TYPE2's representation accurately reflects the taxonomy of
      ;; the underlying set
      (values
       (and (typep type2 'kernel::class)
	    (dolist (x info nil)
	      (when (or (not (cdr x))
			(csubtypep type1 (specifier-type (cdr x))))
		(return
		 (or (eq type2 (car x))
		     (let ((inherits (layout-inherits
				      (%class-layout (car x)))))
		       (dotimes (i (length inherits) nil)
			 (when (eq type2 (layout-class (svref inherits i)))
			   (return t)))))))))
       t)))

(eval-when (compile eval)
;;; DEFINE-SUPERCLASSES  --  Interface
;;;
;;;    Takes a list of specs of the form (superclass &optional guard).
;;; Consider one spec (with no guard): any instance of type-class is also a
;;; subtype of SUPERCLASS and of any of its superclasses.  If there are
;;; multiple specs, then some will have guards.  We choose the first spec whose
;;; guard is a supertype of TYPE1 and use its superclass.  In effect, a
;;; sequence of guards G0, G1, G2 is actually G0, (and G1 (not G0)),
;;; (and G2 (not (or G0 G1))).
;;;
(defmacro define-superclasses (type-class &rest specs)
  (let ((info
	 (mapcar #'(lambda (spec)
		     (destructuring-bind (super &optional guard)
					 spec
		       (cons (kernel::find-class super) guard)))
		 specs)))
    `(cold-load-init
      (setf (type-class-complex-subtypep-arg1
	     (type-class-or-lose ',type-class))
	    #'(lambda (type1 type2)
		(has-superclasses-complex-subtypep-arg1 type1 type2 ',info)))
       
       (setf (type-class-complex-subtypep-arg2
	      (type-class-or-lose ',type-class))
	     #'delegate-complex-subtypep-arg2)
       
       (setf (type-class-complex-intersection
	      (type-class-or-lose ',type-class))
	     #'delegate-complex-intersection))))

); eval-when (compile eval)

(declaim (inline reparse-unknown-type))
(defun reparse-unknown-type (type)
  (if (unknown-type-p type)
      (specifier-type (type-specifier type))
      type))

(declaim (inline swapped-args-fun))
(defun swapped-args-fun (fun)
  (declare (type function fun))
  (lambda (x y)
    (funcall fun y x)))

(defun equal-but-no-car-recursion (x y)
  (cond
    ((eql x y) t)
    ((consp x)
     (and (consp y)
	  (eql (car x) (car y))
	  (equal-but-no-car-recursion (cdr x) (cdr y))))
    (t nil)))

(defun any/type (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (values nil certain?))
      (multiple-value-bind (sub-value sub-certain?) (funcall op thing i)
	(if sub-certain?
	    (when sub-value (return (values t t)))
	    (setf certain? nil))))))

(defun every/type (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (if certain? (values t t) (values nil nil)))
      (multiple-value-bind (sub-value sub-certain?) (funcall op thing i)
	(if sub-certain?
	    (unless sub-value (return (values nil t)))
	    (setf certain? nil))))))

(defun invoke-complex-=-other-method (type1 type2)
  (let* ((type-class (type-class-info type1))
	 (method-fun (type-class-complex-= type-class)))
    (if method-fun
	(funcall (the function method-fun) type2 type1)
	(values nil t))))

(defun invoke-complex-subtypep-arg1-method (type1 type2 &optional subtypep win)
  (let* ((type-class (type-class-info type1))
	 (method-fun (type-class-complex-subtypep-arg1 type-class)))
    (if method-fun
	(funcall (the function method-fun) type1 type2)
	(values subtypep win))))

(declaim (inline type-might-contain-other-types-p))
(defun type-might-contain-other-types-p (type)
  (or (hairy-type-p type)
      (negation-type-p type)
      (union-type-p type)
      (intersection-type-p type)))


;;;; Function and Values types.
;;;
;;;    Pretty much all of the general type operations are illegal on VALUES
;;; types, since we can't discriminate using them, do SUBTYPEP, etc.  FUNCTION
;;; types are acceptable to the normal type operations, but are generally
;;; considered to be equivalent to FUNCTION.  These really aren't true types in
;;; any type theoretic sense, but we still parse them into CTYPE structures for
;;; two reasons:
;;; -- Parsing and unparsing work the same way, and indeed we can't tell
;;;    whether a type is a function or values type without parsing it.
;;; -- Many of the places that can be annotated with real types can also be
;;;    annotated function or values types.


(define-type-method (values :simple-subtypep :complex-subtypep-arg1)
    (type1 type2)
  (declare (ignore type2))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type1)))

(define-type-method (values :complex-subtypep-arg2)
    (type1 type2)
  (declare (ignore type1))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type2)))

(define-type-method (values :unparse) (type)
  (cons 'values (unparse-args-types type)))

;;; TYPE=-LIST  --  Internal
;;;
;;;    Return true if List1 and List2 have the same elements in the same
;;; positions according to TYPE=.  We return NIL, NIL if there is an uncertain
;;; comparison. 
;;;
(defun type=-list (list1 list2)
  (declare (list list1 list2))
  (do ((types1 list1 (cdr types1))
       (types2 list2 (cdr types2)))
      ((or (null types1) (null types2))
       (if (or types1 types2)
	   (values nil t)
	   (values t t)))
    (multiple-value-bind (val win)
	(type= (first types1) (first types2))
      (unless win
	(return (values nil nil)))
      (unless val
	(return (values nil t))))))

(define-type-method (values :simple-=) (type1 type2)
  (let ((rest1 (args-type-rest type1))
	(rest2 (args-type-rest type2)))
    (cond ((or (args-type-keyp type1) (args-type-keyp type2)
	       (args-type-allowp type1) (args-type-allowp type2))
	   (values nil nil))
	  ((and rest1 rest2 (type/= rest1 rest2))
	   (type= rest1 rest2))
	  ((or rest1 rest2)
	   (values nil t))
	  (t
	   (multiple-value-bind (req-val req-win)
	       (type=-list (values-type-required type1)
			   (values-type-required type2))
	     (multiple-value-bind (opt-val opt-win)
		 (type=-list (values-type-optional type1)
			     (values-type-optional type2))
	       (values (and req-val opt-val) (and req-win opt-win))))))))

;;; A flag that we can bind to cause complex function types to be unparsed as
;;; FUNCTION.  Useful when we want a type that we can pass to TYPEP.
;;;
(defvar *unparse-function-type-simplify*)
(cold-load-init (setq *unparse-function-type-simplify* nil))

(define-type-method (function :unparse) (type)
  (if *unparse-function-type-simplify*
      'function
      (list 'function
	    (if (function-type-wild-args type)
		'*
		(unparse-args-types type))
	    (type-specifier
	     (function-type-returns type)))))

;;; Since all function types are equivalent to FUNCTION, they are all subtypes
;;; of each other.
;;;
(define-type-method (function :simple-subtypep) (type1 type2)
 (flet ((fun-type-simple-p (type)
          (not (or (function-type-rest type)
                   (function-type-keyp type))))
        (every-csubtypep (types1 types2)
          (loop
             for a1 in types1
             for a2 in types2
             do (multiple-value-bind (res sure-p)
                    (csubtypep a1 a2)
                  (unless res (return (values res sure-p))))
             finally (return (values t t)))))
   (macrolet ((3and (x y)
                `(multiple-value-bind (val1 win1) ,x
                   (if (and (not val1) win1)
                       (values nil t)
                       (multiple-value-bind (val2 win2) ,y
                         (if (and val1 val2)
                             (values t t)
                             (values nil (and win2 (not val2)))))))))
     (3and (values-subtypep (function-type-returns type1)
                            (function-type-returns type2))
           (cond ((function-type-wild-args type2) (values t t))
                 ((function-type-wild-args type1)
                  (cond ((function-type-keyp type2) (values nil nil))
                        ((not (function-type-rest type2)) (values nil t))
                        ((not (null (function-type-required type2))) (values nil t))
                        (t (3and (type= *universal-type* (function-type-rest type2))
                                 (every/type #'type= *universal-type*
                                             (function-type-optional type2))))))
                 ((not (and (fun-type-simple-p type1)
                            (fun-type-simple-p type2)))
                  (values nil nil))
                 (t (multiple-value-bind (min1 max1) (function-type-nargs type1)
                      (multiple-value-bind (min2 max2) (function-type-nargs type2)
                        (cond ((or (> max1 max2) (< min1 min2))
                               (values nil t))
                              ((and (= min1 min2) (= max1 max2))
                               (3and (every-csubtypep (function-type-required type1)
                                                      (function-type-required type2))
                                     (every-csubtypep (function-type-optional type1)
                                                      (function-type-optional type2))))
                              (t (every-csubtypep
                                  (concatenate 'list
                                               (function-type-required type1)
                                               (function-type-optional type1))
                                  (concatenate 'list
                                               (function-type-required type2)
                                               (function-type-optional type2)))))))))))))

(define-superclasses function (function))

;;; The union or intersection of two FUNCTION types is FUNCTION.
;;;
(define-type-method (function :simple-union) (type1 type2)
  (if (type= type1 type2)
      type1
      (specifier-type 'function)))

(define-type-method (function :simple-intersection) (type1 type2)
  (if (type= type1 type2)
      type1
      (values (specifier-type 'function) t)))


;;; ### Not very real, but good enough for redefining transforms according to
;;; type:
;;;
(define-type-method (function :simple-=) (type1 type2)
  (values (equalp type1 type2) t))


(define-type-method (constant :unparse) (type)
  `(constant-argument ,(type-specifier (constant-type-type type))))

(define-type-method (constant :simple-=) (type1 type2)
  (type= (constant-type-type type1) (constant-type-type type2)))

(def-type-translator constant-argument (type)
  (make-constant-type :type (specifier-type type)))


;;; Parse-Args-Types  --  Internal
;;;
;;;    Given a lambda-list like values type specification and a Args-Type
;;; structure, fill in the slots in the structure accordingly.  This is used
;;; for both FUNCTION and VALUES types.
;;;
(defun parse-args-types (lambda-list result)
  (declare (list lambda-list) (type args-type result))
  (multiple-value-bind (required optional restp rest keyp keys allowp aux)
      (parse-lambda-list lambda-list)
    (when aux
      (simple-program-error "&Aux in a FUNCTION or VALUES type: ~S."
                            lambda-list))
    (setf (args-type-required result)
	  (mapcar #'single-value-specifier-type required))
    (setf (args-type-optional result)
	  (mapcar #'single-value-specifier-type optional))
    (setf (args-type-rest result)
	  (if restp (single-value-specifier-type rest) nil))
    (setf (args-type-keyp result) keyp)
    (collect ((key-info))
      (dolist (key keys)
	(when (or (atom key) (/= (length key) 2))
	  (simple-program-error
	   "Keyword type description is not a two-list: ~S." key))
	(let ((kwd (first key)))
	  (when (find kwd (key-info) :key #'key-info-name)
	    (simple-program-error "Repeated keyword ~S in lambda list: ~S."
                                  kwd lambda-list))
	  (key-info (make-key-info
		     :name kwd
		     :type (single-value-specifier-type (second key))))))
      (setf (args-type-keywords result) (key-info)))
    (setf (args-type-allowp result) allowp)))

;;; Unparse-Args-Types  --  Internal
;;;
;;;    Return the lambda-list like type specification corresponding
;;; to a Args-Type.
;;;
(defun unparse-args-types (type)
  (declare (type args-type type) (values list))
  (collect ((result))

    (dolist (arg (args-type-required type))
      (result (type-specifier arg)))

    (when (args-type-optional type)
      (result '&optional)
      (dolist (arg (args-type-optional type))
	(result (type-specifier arg))))

    (when (args-type-rest type)
      (result '&rest)
      (result (type-specifier (args-type-rest type))))

    (when (args-type-keyp type)
      (result '&key)
      (dolist (key (args-type-keywords type))
	(result (list (key-info-name key)
		      (type-specifier (key-info-type key))))))

    (when (args-type-allowp type)
      (result '&allow-other-keys))

    (result)))

(def-type-translator function (&optional (args '*) (result '*))
  (let ((res (make-function-type :returns (values-specifier-type result))))
    (if (eq args '*)
	(setf (function-type-wild-args res) t)
	(parse-args-types args res))
    res))

(def-type-translator values (&rest values)
  (let ((res (make-values-type)))
    (parse-args-types values res)
    ;;
    ;; Signal an error if the spec has &KEY or &ALLOW-OTHER-KEYS.
    ;; Actually, CLHS lists &ALLOW-OTHER-KEYS without listing &KEYS,
    ;; but keys clearly don't make any sense.
    (when (or (values-type-keyp res) (values-type-allowp res))
      (simple-program-error "&KEY or &ALLOW-OTHER-KEYS in values type: ~s"
			    res))
    res))


;;;; Values types interfaces:
;;;
;;;    We provide a few special operations that can be meaningfully used on
;;; values types (as well as on any other type.)
;;;

;;; Single-Value-Type  --  Interface
;;;
;;;    Return the type of the first value indicated by Type.  This is used by
;;; people who don't want to have to deal with values types. If the first
;;; values is an optional or rest argument then return the union with the null
;;; type. If the first values is a keyword then give up and return the
;;; universal type.
;;;
(defun single-value-type (type)
  (declare (type ctype type))
  (cond ((values-type-p type)
	 (or (car (args-type-required type))
             (if (args-type-optional type)
                 (type-union (car (args-type-optional type))
			     (specifier-type 'null)))
	     (args-type-rest type)
             (specifier-type 'null)))
	((eq type *wild-type*)
	 *universal-type*)
	(t
	 type)))

;;; FUNCTION-TYPE-NARGS  --  Interface
;;;
;;;    Return the minmum number of arguments that a function can be called
;;; with, and the maximum number or NIL.  If not a function type, return
;;; NIL, NIL.
;;;
(defun function-type-nargs (type)
  (declare (type ctype type))
  (if (function-type-p type)
      (let ((fixed (length (args-type-required type))))
	(if (or (args-type-rest type)
		(args-type-keyp type)
		(args-type-allowp type))
	    (values fixed nil)
	    (values fixed (+ fixed (length (args-type-optional type))))))
      (values nil nil)))


;;; Values-Types  --  Interface
;;;
;;;    Determine if Type corresponds to a definite number of values.  The first
;;; value is a list of the types for each value, and the second value is the
;;; number of values.  If the number of values is not fixed, then return NIL
;;; and :Unknown.
;;;
(defun values-types (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*)
	 (values nil :unknown))
	((not (values-type-p type))
	 (values (list type) 1))
	((or (args-type-optional type)
	     (args-type-rest type)
	     (args-type-keyp type)
	     (args-type-allowp type))
	 (values nil :unknown))
	(t
	 (let ((req (args-type-required type)))
	   (values (mapcar #'single-value-type req) (length req))))))


;;; Values-Type-Types  --  Internal
;;;
;;;    Return two values:
;;; 1] A list of all the positional (fixed and optional) types.
;;; 2] The rest type (if any).  If keywords allowed, *universal-type*.  If no
;;;    keywords or rest, *empty-type*.
;;;
(defun values-type-types (type &optional (default-type *empty-type*))
  (declare (type values-type type))
  (values (append (args-type-required type)
		  (args-type-optional type))
	  (cond ((args-type-keyp type) *universal-type*)
		((args-type-rest type))
		(t
		 default-type))))

;;; Fixed-Values-Op  --  Internal
;;;
;;;    Return a list of Operation applied to the types in Types1 and Types2,
;;; padding with Rest2 as needed.  Types1 must not be shorter than Types2.  The
;;; second value is T if Operation always returned a true second value.
;;;
(defun fixed-values-op (types1 types2 rest2 operation)
  (declare (list types1 types2) (type ctype rest2) (type function operation))
  (let ((exact t))
    (values (mapcar (lambda (t1 t2)
		      (multiple-value-bind (res win)
			  (funcall operation t1 t2)
			(unless win
			  (setq exact nil))
			res))
		    types1
		    (append types2
			    (make-list (- (length types1) (length types2))
				       :initial-element rest2)))
	    exact)))


;;; Coerce-To-Values  --  Internal
;;;
;;; If Type isn't a values type, then make it into one:
;;;    <type>  ==>  (values type)
;;;
(defun coerce-to-values (type)
  (declare (type ctype type))
  (if (values-type-p type)
      type
      (make-values-type :required (list type))))

;;; Args-Type-Op  --  Internal
;;;
;;;    Do the specified Operation on Type1 and Type2, which may be any type,
;;; including Values types.  With values types such as:
;;;    (values a0 a1)
;;;    (values b0 b1)
;;;
;;; We compute the more useful result:
;;;    (values (<operation> a0 b0) (<operation> a1 b1))
;;;
;;; Rather than the precise result:
;;;    (<operation> (values a0 a1) (values b0 b1))
;;;
;;; This has the virtue of always keeping the values type specifier outermost,
;;; and retains all of the information that is really useful for static type
;;; analysis.  We want to know what is always true of each value independently.
;;; It is worthless to know that IF the first value is B0 then the second will
;;; be B1.
;;;
;;; If the values count signatures differ, then we produce result with the
;;; required value count chosen by Nreq when applied to the number of required
;;; values in type1 and type2.  Any &key values become &rest T (anyone who uses
;;; keyword values deserves to lose.)
;;;
;;; The second value is true if the result is definitely empty or if Operation
;;; returned true as its second value each time we called it.  Since we
;;; approximate the intersection of values types, the second value being true
;;; doesn't mean the result is exact.
;;;
(defun args-type-op (type1 type2 operation nreq default-type)
  (declare (type ctype type1 type2 default-type)
 	   (type function operation nreq))
  (when (eq type1 type2)
    (values type1 t))
  (if (or (values-type-p type1) (values-type-p type2))
      (let ((type1 (coerce-to-values type1))
	    (type2 (coerce-to-values type2)))
	(multiple-value-bind (types1 rest1)
            (values-type-types type1 default-type)
	  (multiple-value-bind (types2 rest2)
              (values-type-types type2 default-type)
	    (multiple-value-bind (rest rest-exact)
		(funcall operation rest1 rest2)
	      (multiple-value-bind (res res-exact)
		  (if (< (length types1) (length types2))
		      (fixed-values-op types2 types1 rest1 operation)
		      (fixed-values-op types1 types2 rest2 operation))
		(let* ((req (funcall nreq
				     (length (args-type-required type1))
				     (length (args-type-required type2))))
		       (required (subseq res 0 req))
		       (opt (subseq res req))
		       (opt-last (position rest opt :test-not #'type=
					   :from-end t)))
		  (if (find *empty-type* required :test #'type=)
		      (values *empty-type* t)
		      (values (make-values-type
			       :required required
			       :optional (if opt-last
					     (subseq opt 0 (1+ opt-last))
					     ())
			       :rest (if (eq rest default-type) nil rest))
			      (and rest-exact res-exact)))))))))
      (funcall operation type1 type2)))

;;; Values-Type-Union, Values-Type-Intersection  --  Interface
;;;
;;;    Do a union or intersection operation on types that might be values
;;; types.  The result is optimized for utility rather than exactness, but it
;;; is guaranteed that it will be no smaller (more restrictive) than the
;;; precise result.
;;;
(defun-cached (values-type-union :hash-function type-cache-hash
				 :hash-bits 8
				 :default nil
				 :init-form cold-load-init)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 *wild-type*) (eq type2 *wild-type*)) *wild-type*)
	((eq type1 *empty-type*) type2)
	((eq type2 *empty-type*) type1)
	(t
	 (values (args-type-op type1 type2 #'type-union #'min *empty-type*)))))
;;;
(defun-cached (values-type-intersection :hash-function type-cache-hash
					:hash-bits 8
					:values 2
					:default (values nil :empty)
					:init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 *wild-type*) (values type2 t))
	((eq type2 *wild-type*) (values type1 t))
	(t
	 (args-type-op type1 type2
		       #'type-intersection
		       #'max
		       (specifier-type 'null)))))


;;; Values-Types-Intersect  --  Interface
;;;
;;;    Like Types-Intersect, except that it sort of works on values types.
;;; Note that due to the semantics of Values-Type-Intersection, this might
;;; return {T, T} when there isn't really any intersection (?).
;;;
(defun values-types-intersect (type1 type2)
  (cond ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
	 (values t t))
	((or (values-type-p type1) (values-type-p type2))
	 (multiple-value-bind (res win) (values-type-intersection type1 type2)
	   (values (not (eq res *empty-type*))
		   win)))
	(t
	 (types-intersect type1 type2))))


;;; Values-Subtypep  --  Interface
;;;
;;;    A subtypep-like operation that can be used on any types, including
;;; values types.
;;;
(defun-cached (values-subtypep :hash-function type-cache-hash
			       :hash-bits 8
			       :values 2
			       :default (values nil :empty)
			       :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type2 *wild-type*) (values t t))
	((eq type1 *wild-type*)
	 (values (eq type2 *universal-type*) t))
	((not (values-types-intersect type1 type2))
	 (values nil t))
	(t
	 (if (or (values-type-p type1) (values-type-p type2))
	     (let ((type1 (coerce-to-values type1))
		   (type2 (coerce-to-values type2)))
	       (multiple-value-bind (types1 rest1) (values-type-types type1)
		 (multiple-value-bind (types2 rest2) (values-type-types type2)
		   (cond ((< (length (values-type-required type1))
			     (length (values-type-required type2)))
			  (values nil t))
			 ((< (length types1) (length types2))
			  (values nil nil))
			 ((or (values-type-keyp type1)
			      (values-type-keyp type2))
			  (values nil nil))
			 (t
			  (do ((t1 types1 (rest t1))
			       (t2 types2 (rest t2)))
			      ((null t2)
			       (csubtypep rest1 rest2))
			    (multiple-value-bind (res win-p)
				(csubtypep (first t1) (first t2))
			      (unless win-p
				(return (values nil nil)))
			      (unless res
				(return (values nil t))))))))))
	     (csubtypep type1 type2)))))
						       

;;;; Type method interfaces:

;;; Csubtypep  --  Interface
;;;
;;;    Like subtypep, only works on Type structures.
;;;
(defun-cached (csubtypep :hash-function type-cache-hash
			 :hash-bits 8
			 :values 2
			 :default (values nil :empty)
			 :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 type2)
	     (eq type1 *empty-type*)
	     (eq type2 *wild-type*))
	 (values t t))
	((eq type1 *wild-type*)
	 (values nil t))
	(t
	 (invoke-type-method :simple-subtypep :complex-subtypep-arg2
			     type1 type2
			     :complex-arg1 :complex-subtypep-arg1))))

(declaim (start-block))

;;; Type=  --  Interface
;;;
;;;    If two types are definitely equivalent, return true.  The second value
;;; indicates whether the first value is definitely correct.  This should only
;;; fail in the presence of Hairy types.
;;;
(defun-cached (type= :hash-function type-cache-hash
		     :hash-bits 8
		     :values 2
		     :default (values nil :empty)
		     :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
      (values t t)
      (invoke-type-method :simple-= :complex-= type1 type2)))


;;; TYPE/=  --  Interface
;;;
;;;    Not exactly the negation of TYPE=, since when the relationship is
;;; uncertain, we still return NIL, NIL.  This is useful in cases where the
;;; conservative assumption is =.
;;;
(defun type/= (type1 type2)
  (declare (type ctype type1 type2))
  (multiple-value-bind (res win) (type= type1 type2)
    (if win
	(values (not res) t)
	(values nil nil))))

(declaim (end-block))

;;; Type-Union  --  Interface
;;;
;;;    Find a type which includes both types.  Any inexactness is represented
;;; by the fuzzy element types; we return a single value that is precise to the
;;; best of our knowledge.  This result is simplified into the canonical form,
;;; thus is not a UNION type unless there is no other way to represent the
;;; result.
;;;
(defun type-union (&rest input-types)
  (%type-union input-types))

(defun-cached (%type-union :hash-bits 8
                           :hash-function (lambda (x)
                                            (logand (sxhash x) #xff)))
    ((input-types equal))
  (let ((simplified (simplify-unions input-types)))
    (cond ((null simplified) *empty-type*)
	  ((null (cdr simplified)) (car simplified))
	  (t (make-union-type simplified)))))

(defun simplify-big-integer-union (first rest)
  ;;
  (let ((lowest (numeric-type-low first))
	(highest (numeric-type-high first)))
    (dolist (type rest)
      (multiple-value-bind (type-lo type-hi)
	  (values (numeric-type-low type)
		  (numeric-type-high type))
	(if (and (numberp lowest) (numberp type-lo))
	    (setf lowest (min lowest type-lo))
	    (setf lowest nil))
	(if (and (numberp highest) (numberp type-hi))
	    (setf highest (max highest type-hi))
	    (setf highest nil))))
    (list (specifier-type `(integer ,(or lowest '*) ,(or highest '*))))))
	

(defparameter *union-length-threshold* 50
  "The maximum length of a union of integer types before we take a
  short cut and return a simpler union.")
	    
(defun simplify-unions (types)
  (when types
    (multiple-value-bind (first rest)
	(if (union-type-p (car types))
	    (values (car (union-type-types (car types)))
		    (append (cdr (union-type-types (car types)))
			    (cdr types)))
	    (values (car types) (cdr types)))
      (cond
	((and (> (length rest) *union-length-threshold*)
	      (every #'(lambda (x)
			 (and (numeric-type-p x)
			      (eq (numeric-type-class x) 'integer)))
		     (cons first rest)))
	 ;; FIXME: We sometimes spend huge amounts of time computing
	 ;; the union of a bunch of disjoint integer types.  This is a
	 ;; hack to shortcut that.  If the union is long enough and
	 ;; they're all integer types, we give up and try to return an
	 ;; interval that is a superset of each type.
	 (simplify-big-integer-union first rest))
	(t
	 (let ((rest (simplify-unions rest)) u)
	   (dolist (r rest (cons first rest))
	     (when (setq u (type-union2 first r))
	       (return (simplify-unions (nsubstitute u r rest)))))))))))

(defun-cached (type-union2 :hash-function type-cache-hash
			   :hash-bits 8
			   :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (setq type1 (reparse-unknown-type type1))
  (setq type2 (reparse-unknown-type type2))
  (cond ((eq type1 type2) type1)
	((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t
	 (flet ((1way (x y)
		  (invoke-type-method :simple-union :complex-union
				      x y
				      :default nil)))
	   (or (1way type1 type2)
	       (1way type2 type1))))))

;;; Return as restrictive and simple a type as we can discover that is
;;; no more restrictive than the intersection of TYPE1 and TYPE2. At
;;; worst, we arbitrarily return one of the arguments as the first
;;; value (trying not to return a hairy type).
(defun type-approx-intersection2 (type1 type2)
  (cond ((type-intersection2 type1 type2))
	((hairy-type-p type1) type2)
	(t type1)))

;;; Type-Intersection  --  Interface
;;;
;;;    Return as restrictive a type as we can discover that is no more
;;; restrictive than the intersection of Type1 and Type2.  The second value is
;;; true if the result is exact.  At worst, we randomly return one of the
;;; arguments as the first value (trying not to return a hairy type).
;;;
(defun type-intersection (&rest input-types)
  (%type-intersection input-types))

(defun-cached (%type-intersection :hash-bits 8
                                  :hash-function (lambda (x)
                                                   (logand (sxhash x) #xff)))
    ((input-types equal))
  (let ((simplified (simplify-intersections input-types)))
    ;(declare (type (vector ctype) simplified))
    ;; We want to have a canonical representation of types (or failing
    ;; that, punt to HAIRY-TYPE). Canonical representation would have
    ;; intersections inside unions but not vice versa, since you can
    ;; always achieve that by the distributive rule. But we don't want
    ;; to just apply the distributive rule, since it would be too easy
    ;; to end up with unreasonably huge type expressions. So instead
    ;; we try to generate a simple type by distributing the union; if
    ;; the type can't be made simple, we punt to HAIRY-TYPE.
    (if (and (cdr simplified) (some #'union-type-p simplified))
	(let* ((first-union (find-if #'union-type-p simplified))
	       (other-types (remove first-union simplified))
	       (distributed (maybe-distribute-one-union first-union other-types)))
	  (if distributed
	      (apply #'type-union distributed)
	      (make-hairy-type
	       :specifier `(and ,@(mapcar #'type-specifier simplified)))))
	(cond
	  ((null simplified) *universal-type*)
	  ((null (cdr simplified)) (car simplified))
	  (t (make-intersection-type
	      (some #'type-enumerable simplified)
	      simplified))))))

(defun simplify-intersections (types)
  (when types
    (multiple-value-bind (first rest)
	(if (intersection-type-p (car types))
	    (values (car (intersection-type-types (car types)))
		    (append (cdr (intersection-type-types (car types)))
			    (cdr types)))
	    (values (car types) (cdr types)))
      (let ((rest (simplify-intersections rest)) u)
	(dolist (r rest (cons first rest))
	  (when (setq u (type-intersection2 first r))
	    (return (simplify-intersections (nsubstitute u r rest)))))))))

(defun-cached (type-intersection2 :hash-function type-cache-hash
				  :hash-bits 8
				  :init-form cold-load-init)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (setq type1 (reparse-unknown-type type1))
  (setq type2 (reparse-unknown-type type2))
  (cond ((eq type1 type2)
	 ;; FIXME: For some reason, this doesn't catch e.g. type1 =
	 ;; type2 = (SPECIFIER-TYPE
	 ;; 'SOME-UNKNOWN-TYPE). Investigate. - CSR, 2002-04-10
	 type1)
	((or (intersection-type-p type1)
	     (intersection-type-p type2))
	 ;; Intersections of INTERSECTION-TYPE should have the
	 ;; INTERSECTION-TYPE-TYPES values broken out and intersected
	 ;; separately. The full TYPE-INTERSECTION function knows how
	 ;; to do that, so let it handle it.
	 (type-intersection type1 type2))
	;;
	;; (AND (FUNCTION (T) T) GENERIC-FUNCTION) for instance, but
	;; not (AND (FUNCTION (T) T) (FUNCTION (T) T)).
	((let ((function (specifier-type 'function)))
	   (or (and (function-type-p type1)
		    (not (or (function-type-p type2) (eq function type2)))
		    (csubtypep type2 function)
		    (not (csubtypep function type2)))
	       (and (function-type-p type2)
		    (not (or (function-type-p type1) (eq function type1)))
		    (csubtypep type1 function)
		    (not (csubtypep function type1)))))
	 nil)
	(t
	 (flet ((1way (x y)
		  (invoke-type-method :simple-intersection
				      :complex-intersection
				      x y
				      :default :no-type-method-found)))
	   (let ((xy (1way type1 type2)))
	     (or (and (not (eql xy :no-type-method-found)) xy)
		 (let ((yx (1way type2 type1)))
		   (or (and (not (eql yx :no-type-method-found)) yx)
		       (cond ((and (eql xy :no-type-method-found)
				   (eql yx :no-type-method-found))
			      *empty-type*)
			     (t
			      (assert (and (not xy) (not yx)))
			      nil))))))))))

(defun maybe-distribute-one-union (union-type types)
  (let* ((intersection (apply #'type-intersection types))
	 (union (mapcar (lambda (x) (type-intersection x intersection))
			(union-type-types union-type))))
    (if (notany (lambda (x)
		  (or (hairy-type-p x)
		      (intersection-type-p x)))
		union)
	union
	nil)))

;;; Types-Intersect  --  Interface
;;;
;;;    The first value is true unless the types don't intersect.  The second
;;; value is true if the first value is definitely correct.  NIL is considered
;;; to intersect with any type.  If T is a subtype of either type, then we also
;;; return T, T.  This way we consider hairy types to intersect with T.
;;;
(defun types-intersect (type1 type2)
  (declare (type ctype type1 type2))
  (if (or (eq type1 *empty-type*) (eq type2 *empty-type*))
      (values t t)
      (let ((intersection2 (type-intersection2 type1 type2)))
	(cond ((not intersection2)
	       (if (or (csubtypep *universal-type* type1)
		       (csubtypep *universal-type* type2))
		   (values t t)
		   (values t nil)))
	      ((eq intersection2 *empty-type*) (values nil t))
	      (t (values t t))))))

;;; Type-Specifier  --  Interface
;;;
;;;    Return a Common Lisp type specifier corresponding to this type.
;;;
(defun type-specifier (type)
  (declare (type ctype type))
  (funcall (type-class-unparse (type-class-info type)) type))


;;; VALUES-SPECIFIER-TYPE  --  Interface
;;;
;;;    Return the type structure corresponding to a type specifier.  We pick
;;; off Structure types as a special case.
;;;
;;; Note: VALUES-SPECIFIER-TYPE-CACHE-CLEAR must be called whenever a type is
;;; defined (or redefined).
;;;
(defun-cached (values-specifier-type
	       :hash-function (lambda (x)
				(the fixnum
				     (logand (the fixnum (cache-hash-eq x))
					     #x3FF)))
	       :hash-bits 10
	       :init-form cold-load-init)
    ((orig equal-but-no-car-recursion))
  (or (info type builtin orig)
      (let ((spec (type-expand orig)))
	(cond
	 ((and (not (eq spec orig))
	       (info type builtin spec)))
	 ((eq (info type kind spec) :instance)
	  (kernel::find-class spec))
	 ((typep spec 'kernel::class)
	  (if (typep spec 'kernel::built-in-class)
	      (or (built-in-class-translation spec) spec)
	      spec))
	 (t
	  (let* ((lspec (if (atom spec) (list spec) spec))
		 (fun (info type translator (car lspec))))
	    (cond (fun
		   (funcall fun lspec))
		  ((or (and (consp spec) (symbolp (car spec)))
		       (symbolp spec))
		   (when *type-system-initialized*
		     (signal 'parse-unknown-type :specifier spec))
		   ;;
		   ;; Inhibit caching...
		   (return-from values-specifier-type
				(make-unknown-type :specifier spec)))
		  (t
		   (simple-program-error "Bad thing to be a type specifier: ~S."
                                         spec)))))))))

;;; SPECIFIER-TYPE  --  Interface
;;;
;;;    Like VALUES-SPECIFIER-TYPE, except that we guarantee to never return a
;;; VALUES type.
;;; 
(defun specifier-type (x)
  (let ((res (values-specifier-type x)))
    (when (values-type-p res)
      (simple-program-error "VALUES type illegal in this context:~%  ~S" x))
    res))

(defun single-value-specifier-type (x)
  (let ((res (specifier-type x)))
    (if (eq res *wild-type*)
        *universal-type*
        res)))

;;; Type-Expand  --  Interface
;;;
;;;    Similar to Macroexpand, but expands deftypes.  We don't bother returning
;;; a second value.
;;;
(defun type-expand (form)
  (let ((def (cond ((symbolp form)
		    (info type expander form))
		   ((and (consp form) (symbolp (car form)))
		    (info type expander (car form)))
		   (t nil))))
    (if def
	(type-expand (funcall def (if (consp form) form (list form))))
	form)))


;;; Precompute-Types  --  Interface
;;;
;;;    Take a list of type specifiers, compute the translation and define it as
;;; a builtin type.
;;;
(defun precompute-types (specs)
  (declare (list specs))
  (dolist (spec specs)
    (let ((res (specifier-type spec)))
      (unless (unknown-type-p res)
	(setf (info type builtin spec) res)
	(setf (info type kind spec) :primitive)))))


;;;; Builtin types.

(defvar *wild-type*)
(defvar *empty-type*)
(defvar *universal-type*)

(cold-load-init
 (macrolet ((frob (name var)
	      `(progn
		 (setq ,var (make-named-type :name ',name))
		 (setf (info type kind ',name) :primitive)
		 (setf (info type builtin ',name) ,var))))
   (frob * *wild-type*)
   (frob nil *empty-type*)
   (frob t *universal-type*)))

(define-type-method (named :simple-=) (type1 type2)
  ;; FIXME: BUG 85: This assertion failed when I added it in
  ;; sbcl-0.6.11.13. It probably shouldn't fail; but for now it's
  ;; just commented out.
  ;;(aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (values (eq type1 type2) t))

(define-type-method (named :complex-=) (type1 type2)
  (cond
    ((and (eq type2 *empty-type*)
	  (intersection-type-p type1)
	  ;; not allowed to be unsure on these... FIXME: keep the list
	  ;; of CL types that are intersection types once and only
	  ;; once.
	  (not (or (type= type1 (specifier-type 'ratio))
		   (type= type1 (specifier-type 'keyword)))))
     ;; things like (AND (EQL 0) (SATISFIES ODDP)) or (AND FUNCTION
     ;; STREAM) can get here.  In general, we can't really tell
     ;; whether these are equal to NIL or not, so
     (values nil nil))
    ((type-might-contain-other-types-p type1)
     (invoke-complex-=-other-method type1 type2))
    (t (values nil t))))

(define-type-method (named :simple-subtypep) (type1 type2)
  (assert (not (eq type1 *wild-type*))) ; * isn't really a type.
  (values (or (eq type1 *empty-type*) (eq type2 *wild-type*)) t))

(define-type-method (named :complex-subtypep-arg1) (type1 type2)
  ;; This AVER causes problems if we write accurate methods for the
  ;; union (and possibly intersection) types which then delegate to
  ;; us; while a user shouldn't get here, because of the odd status of
  ;; *wild-type* a type-intersection executed by the compiler can. -
  ;; CSR, 2002-04-10
  ;;
  ;; (aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (cond ((eq type1 *empty-type*)
	 t)
	(;; When TYPE2 might be the universal type in disguise
	 (type-might-contain-other-types-p type2)
	 ;; Now that the UNION and HAIRY COMPLEX-SUBTYPEP-ARG2 methods
	 ;; can delegate to us (more or less as CALL-NEXT-METHOD) when
	 ;; they're uncertain, we can't just barf on COMPOUND-TYPE and
	 ;; HAIRY-TYPEs as we used to. Instead we deal with the
	 ;; problem (where at least part of the problem is cases like
	 ;;   (SUBTYPEP T '(SATISFIES FOO))
	 ;; or
	 ;;   (SUBTYPEP T '(AND (SATISFIES FOO) (SATISFIES BAR)))
	 ;; where the second type is a hairy type like SATISFIES, or
	 ;; is a compound type which might contain a hairy type) by
	 ;; returning uncertainty.
	 (values nil nil))
	(t
	 ;; By elimination, TYPE1 is the universal type.
	 (assert (or (eq type1 *wild-type*) (eq type1 *universal-type*)))
	 ;; This case would have been picked off by the SIMPLE-SUBTYPEP
	 ;; method, and so shouldn't appear here.
	 (assert (not (eq type2 *universal-type*)))
	 ;; Since TYPE2 is not EQ *UNIVERSAL-TYPE* and is not the
	 ;; universal type in disguise, TYPE2 is not a superset of TYPE1.
	 (values nil t))))

(define-type-method (named :complex-subtypep-arg2) (type1 type2)
  (assert (not (eq type2 *wild-type*))) ; * isn't really a type.
  (cond ((eq type2 *universal-type*)
	 (values t t))
	((type-might-contain-other-types-p type1)
	 ;; those types can be *EMPTY-TYPE* or *UNIVERSAL-TYPE* in
	 ;; disguise.  So we'd better delegate.
	 (invoke-complex-subtypep-arg1-method type1 type2))
	(t
	 ;; FIXME: This seems to rely on there only being 2 or 3
	 ;; NAMED-TYPE values, and the exclusion of various
	 ;; possibilities above. It would be good to explain it and/or
	 ;; rewrite it so that it's clearer.
	 (values (not (eq type2 *empty-type*)) t))))

(define-type-method (named :complex-intersection) (type1 type2)
  ;; FIXME: This assertion failed when I added it in sbcl-0.6.11.13.
  ;; Perhaps when bug 85 is fixed it can be reenabled.
  ;;(aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (hierarchical-intersection2 type1 type2))

(define-type-method (named :complex-union) (type1 type2)
  ;; Perhaps when bug 85 is fixed this can be reenabled.
  ;;(aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (hierarchical-union2 type1 type2))

(define-type-method (named :unparse) (x)
  (named-type-name x))


;;;; Hairy and unknown types:

(define-type-method (hairy :unparse) (x)
  (hairy-type-specifier x))

(define-type-method (hairy :simple-subtypep) (type1 type2)
  (let ((hairy-spec1 (hairy-type-specifier type1))
	(hairy-spec2 (hairy-type-specifier type2)))
    (cond ((equal-but-no-car-recursion hairy-spec1 hairy-spec2)
	   (values t t))
	  (t
	   (values nil nil)))))

(define-type-method (hairy :complex-subtypep-arg2) (type1 type2)
  (invoke-complex-subtypep-arg1-method type1 type2))

(define-type-method (hairy :complex-subtypep-arg1) (type1 type2)
  (declare (ignore type1 type2))
  (values nil nil))

(define-type-method (hairy :complex-=) (type1 type2)
  (if (and (unknown-type-p type2)
           (let* ((specifier2 (unknown-type-specifier type2))
                  (name2 (if (consp specifier2)
                             (car specifier2)
                             specifier2)))
             (info type kind name2)))
      (let ((type2 (specifier-type (unknown-type-specifier type2))))
        (if (unknown-type-p type2)
            (values nil nil)
            (type= type1 type2)))
  (values nil nil)))

(define-type-method (hairy :simple-intersection :complex-intersection) 
    (type1 type2)
  (if (type= type1 type2)
      type1
      nil))

(define-type-method (hairy :simple-union) 
    (type1 type2)
  (if (type= type1 type2)
      type1
      nil))

(define-type-method (hairy :simple-=) (type1 type2)
  (if (equal-but-no-car-recursion (hairy-type-specifier type1)
				  (hairy-type-specifier type2))
      (values t t)
      (values nil nil)))


(def-type-translator satisfies (&whole whole fun)
  (declare (ignore fun))
  ;; Check legality of arguments.
  (destructuring-bind (satisfies predicate-name) whole
    (declare (ignore satisfies))
    (unless (symbolp predicate-name)
      (error 'simple-type-error
	     :datum predicate-name
	     :expected-type 'symbol
	     :format-control "The SATISFIES predicate name is not a symbol: ~S"
	     :format-arguments (list predicate-name))))
  ;; Create object.
  (make-hairy-type :specifier whole))


;;;; Negation Types

(define-type-method (negation :unparse) (x)
  `(not ,(type-specifier (negation-type-type x))))

(define-type-method (negation :simple-subtypep) (type1 type2)
  (csubtypep (negation-type-type type2) (negation-type-type type1)))

(define-type-method (negation :complex-subtypep-arg2) (type1 type2)
  (let* ((complement-type2 (negation-type-type type2))
	 (intersection2 (type-intersection type1 complement-type2)))
    (if intersection2
	;; FIXME: if uncertain, maybe try arg1?
	(type= intersection2 *empty-type*)
	(invoke-complex-subtypep-arg1-method type1 type2))))

(define-type-method (negation :complex-subtypep-arg1) (type1 type2)
  ;; "Incrementally extended heuristic algorithms tend inexorably toward the
  ;; incomprehensible." -- http://www.unlambda.com/~james/lambda/lambda.txt
  ;;
  ;; You may not believe this. I couldn't either. But then I sat down
  ;; and drew lots of Venn diagrams. Comments involving a and b refer
  ;; to the call (subtypep '(not a) 'b) -- CSR, 2002-02-27.
  (block nil
    ;; (Several logical truths in this block are true as long as
    ;; b/=T. As of sbcl-0.7.1.28, it seems impossible to construct a
    ;; case with b=T where we actually reach this type method, but
    ;; we'll test for and exclude this case anyway, since future
    ;; maintenance might make it possible for it to end up in this
    ;; code.)
    (multiple-value-bind (equal certain)
	(type= type2 *universal-type*)
      (unless certain
	(return (values nil nil)))
      (when equal
	(return (values t t))))
    (let ((complement-type1 (negation-type-type type1)))
      ;; Do the special cases first, in order to give us a chance if
      ;; subtype/supertype relationships are hairy.
      (multiple-value-bind (equal certain) 
	  (type= complement-type1 type2)
	;; If a = b, ~a is not a subtype of b (unless b=T, which was
	;; excluded above).
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      ;; KLUDGE: ANSI requires that the SUBTYPEP result between any
      ;; two built-in atomic type specifiers never be uncertain. This
      ;; is hard to do cleanly for the built-in types whose
      ;; definitions include (NOT FOO), i.e. CONS and RATIO. However,
      ;; we can do it with this hack, which uses our global knowledge
      ;; that our implementation of the type system uses disjoint
      ;; implementation types to represent disjoint sets (except when
      ;; types are contained in other types).  (This is a KLUDGE
      ;; because it's fragile. Various changes in internal
      ;; representation in the type system could make it start
      ;; confidently returning incorrect results.) -- WHN 2002-03-08
      (unless (or (type-might-contain-other-types-p complement-type1)
		  (type-might-contain-other-types-p type2))
	;; Because of the way our types which don't contain other
	;; types are disjoint subsets of the space of possible values,
	;; (SUBTYPEP '(NOT AA) 'B)=NIL when AA and B are simple (and B
	;; is not T, as checked above).
	(return (values nil t)))
      ;; The old (TYPE= TYPE1 TYPE2) branch would never be taken, as
      ;; TYPE1 and TYPE2 will only be equal if they're both NOT types,
      ;; and then the :SIMPLE-SUBTYPEP method would be used instead.
      ;; But a CSUBTYPEP relationship might still hold:
      (multiple-value-bind (equal certain)
	  (csubtypep complement-type1 type2)
	;; If a is a subtype of b, ~a is not a subtype of b (unless
	;; b=T, which was excluded above).
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      (multiple-value-bind (equal certain)
	  (csubtypep type2 complement-type1)
	;; If b is a subtype of a, ~a is not a subtype of b.  (FIXME:
	;; That's not true if a=T. Do we know at this point that a is
	;; not T?)
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      ;; old CSR comment ca. 0.7.2, now obsoleted by the SIMPLE-CTYPE?
      ;; KLUDGE case above: Other cases here would rely on being able
      ;; to catch all possible cases, which the fragility of this type
      ;; system doesn't inspire me; for instance, if a is type= to ~b,
      ;; then we want T, T; if this is not the case and the types are
      ;; disjoint (have an intersection of *empty-type*) then we want
      ;; NIL, T; else if the union of a and b is the *universal-type*
      ;; then we want T, T. So currently we still claim to be unsure
      ;; about e.g. (subtypep '(not fixnum) 'single-float).
      ;;
      ;; OTOH we might still get here:
      (values nil nil))))

(define-type-method (negation :complex-=) (type1 type2)
  ;; (NOT FOO) isn't equivalent to anything that's not a negation
  ;; type, except possibly a type that might contain it in disguise.
  (declare (ignore type2))
  (if (type-might-contain-other-types-p type1)
      (values nil nil)
      (values nil t)))

(define-type-method (negation :simple-intersection) (type1 type2)
  (let ((not1 (negation-type-type type1))
	(not2 (negation-type-type type2)))
    (cond
      ((csubtypep not1 not2) type2)
      ((csubtypep not2 not1) type1)
      ;; Why no analagous clause to the disjoint in the SIMPLE-UNION2
      ;; method, below?  The clause would read
      ;;
      ;; ((EQ (TYPE-UNION NOT1 NOT2) *UNIVERSAL-TYPE*) *EMPTY-TYPE*)
      ;;
      ;; but with proper canonicalization of negation types, there's
      ;; no way of constructing two negation types with union of their
      ;; negations being the universal type.
      (t
       (assert (not (eq (type-union not1 not2) *universal-type*)))
       nil))))

(define-type-method (negation :complex-intersection) (type1 type2)
  (cond
    ((csubtypep type1 (negation-type-type type2)) *empty-type*)
    ((eq (type-intersection type1 (negation-type-type type2)) *empty-type*)
     type1)
    (t nil)))

(define-type-method (negation :simple-union) (type1 type2)
  (let ((not1 (negation-type-type type1))
	(not2 (negation-type-type type2)))
    (cond
      ((csubtypep not1 not2) type1)
      ((csubtypep not2 not1) type2)
      ((eq (type-intersection not1 not2) *empty-type*)
       *universal-type*)
      (t nil))))

(define-type-method (negation :complex-union) (type1 type2)
  (cond
    ((csubtypep (negation-type-type type2) type1) *universal-type*)
    ((eq (type-intersection type1 (negation-type-type type2)) *empty-type*)
     type2)
    (t nil)))

(define-type-method (negation :simple-=) (type1 type2)
  (type= (negation-type-type type1) (negation-type-type type2)))

(def-type-translator not (typespec)
  (let* ((not-type (specifier-type typespec))
	 (spec (type-specifier not-type)))
    (cond
      ;; canonicalize (NOT (NOT FOO))
      ((and (listp spec) (eq (car spec) 'not))
       (specifier-type (cadr spec)))
      ;; canonicalize (NOT NIL) and (NOT T)
      ((eq not-type *empty-type*) *universal-type*)
      ((eq not-type *universal-type*) *empty-type*)
      ((and (numeric-type-p not-type)
	    (null (numeric-type-low not-type))
	    (null (numeric-type-high not-type)))
       (make-negation-type :type not-type))
      ((numeric-type-p not-type)
       (type-union
	(make-negation-type
	 :type (modified-numeric-type not-type :low nil :high nil))
	(cond
	  ((null (numeric-type-low not-type))
	   (modified-numeric-type
	    not-type
	    :low (let ((h (numeric-type-high not-type)))
		   (if (consp h) (car h) (list h)))
	    :high nil))
	  ((null (numeric-type-high not-type))
	   (modified-numeric-type
	    not-type
	    :low nil
	    :high (let ((l (numeric-type-low not-type)))
		    (if (consp l) (car l) (list l)))))
	  (t (type-union
	      (modified-numeric-type
	       not-type
	       :low nil
	       :high (let ((l (numeric-type-low not-type)))
		       (if (consp l) (car l) (list l))))
	      (modified-numeric-type
	       not-type
	       :low (let ((h (numeric-type-high not-type)))
		      (if (consp h) (car h) (list h)))
	       :high nil))))))
      ((intersection-type-p not-type)
       (apply #'type-union
	      (mapcar #'(lambda (x)
			  (specifier-type `(not ,(type-specifier x))))
		      (intersection-type-types not-type))))
      ((union-type-p not-type)
       (apply #'type-intersection
	      (mapcar #'(lambda (x)
			  (specifier-type `(not ,(type-specifier x))))
		      (union-type-types not-type))))
      ((member-type-p not-type)
       (let ((members (member-type-members not-type)))
	 (if (some #'floatp members)
	     (let (floats)
	       (dolist (pair '((0.0f0 . -0.0f0) (0.0d0 . -0.0d0)
			       #+long-float (0.0l0 . -0.0l0)))
		 (when (member (car pair) members)
		   (assert (not (member (cdr pair) members)))
		   (push (cdr pair) floats)
		   (setf members (remove (car pair) members)))
		 (when (member (cdr pair) members)
		   (assert (not (member (car pair) members)))
		   (push (car pair) floats)
		   (setf members (remove (cdr pair) members))))
	       (apply #'type-intersection
		      (if (null members)
			  *universal-type*
			  (make-negation-type
			   :type (make-member-type :members members)))
		      (mapcar
		       (lambda (x)
			 (let ((type (ctype-of x)))
			   (type-union
			    (make-negation-type
			     :type (modified-numeric-type type
							  :low nil :high nil))
			    (modified-numeric-type type
						   :low nil :high (list x))
			    (make-member-type :members (list x))
			    (modified-numeric-type type
						   :low (list x) :high nil))))
		       floats)))
	     (make-negation-type :type not-type))))
      ((and (cons-type-p not-type)
	    (eq (cons-type-car-type not-type) *universal-type*)
	    (eq (cons-type-cdr-type not-type) *universal-type*))
       (make-negation-type :type not-type))
      ((cons-type-p not-type)
       (type-union
	(make-negation-type :type (specifier-type 'cons))
	(cond
	  ((and (not (eq (cons-type-car-type not-type) *universal-type*))
		(not (eq (cons-type-cdr-type not-type) *universal-type*)))
	   (type-union
	    (make-cons-type
	     (specifier-type `(not ,(type-specifier
				     (cons-type-car-type not-type))))
	     *universal-type*)
	    (make-cons-type
	     *universal-type*
	     (specifier-type `(not ,(type-specifier
				     (cons-type-cdr-type not-type)))))))
	  ((not (eq (cons-type-car-type not-type) *universal-type*))
	   (make-cons-type
	    (specifier-type `(not ,(type-specifier
				    (cons-type-car-type not-type))))
	    *universal-type*))
	  ((not (eq (cons-type-cdr-type not-type) *universal-type*))
	   (make-cons-type
	    *universal-type*
	    (specifier-type `(not ,(type-specifier
				    (cons-type-cdr-type not-type))))))
	  (t (error "Weird CONS type ~S" not-type)))))
      (t (make-negation-type :type not-type)))))


;;;; Numeric types.

;;; A list of all the float formats, in order of decreasing precision.
;;;
(eval-when (compile load eval)
  (defconstant float-formats
    '(#+double-double double-double-float
      long-float double-float single-float short-float)))

;;; The type of a float format.
;;;
(deftype float-format () `(member ,@float-formats))


(define-type-method (number :simple-=) (type1 type2)
  (values
   (and (eq (numeric-type-class type1) (numeric-type-class type2))
	(eq (numeric-type-format type1) (numeric-type-format type2))
	(eq (numeric-type-complexp type1) (numeric-type-complexp type2))
	(equalp (numeric-type-low type1) (numeric-type-low type2))
	(equalp (numeric-type-high type1) (numeric-type-high type2)))
   t))

(define-type-method (number :unparse) (type)
  (let* ((complexp (numeric-type-complexp type))
	 (low (numeric-type-low type))
	 (high (numeric-type-high type))
	 (base (case (numeric-type-class type)
		 (integer 'integer)
		 (rational 'rational)
		 (float (or (numeric-type-format type) 'float))
		 (t 'real))))
    (let ((base+bounds
	   (cond ((and (eq base 'integer) high low)
		  (let ((high-count (logcount high))
			(high-length (integer-length high)))
		    (cond ((= low 0)
			   (cond ((= high 0) '(integer 0 0))
				 ((= high 1) 'bit)
				 ((and (= high-count high-length)
				       (plusp high-length))
				  `(unsigned-byte ,high-length))
				 (t
				  `(mod ,(1+ high)))))
			  ((and (= low vm:target-most-negative-fixnum)
				(= high vm:target-most-positive-fixnum))
			   'fixnum)
			  ((and (= low (lognot high))
				(= high-count high-length)
				(> high-count 0))
			   `(signed-byte ,(1+ high-length)))
			  (t
			   `(integer ,low ,high)))))
		 (high `(,base ,(or low '*) ,high))
		 (low
		  (if (and (eq base 'integer) (= low 0))
		      'unsigned-byte
		      `(,base ,low)))
		 (t base))))
      (ecase complexp
	(:real
	 base+bounds)
	(:complex
	 (if (eq base+bounds 'real)
	     'complex
	     `(complex ,base+bounds)))
	((nil)
	 (assert (eq base+bounds 'real))
	 'number)))))

;;; Numeric-Bound-Test  --  Internal
;;;
;;;    Return true if X is "less than or equal" to Y, taking open bounds into
;;; consideration.  Closed is the predicate used to test the bound on a closed
;;; interval (e.g. <=), and Open is the predicate used on open bounds (e.g. <).
;;; Y is considered to be the outside bound, in the sense that if it is
;;; infinite (NIL), then the test suceeds, whereas if X is infinite, then the
;;; test fails (unless Y is also infinite).
;;;
;;;    This is for comparing bounds of the same kind, e.g. upper and upper.
;;; Use Numeric-Bound-Test* for different kinds of bounds.
;;;
(defmacro numeric-bound-test (x y closed open)
  `(cond ((not ,y) t)
	 ((not ,x) nil)
	 ((consp ,x)
	  (if (consp ,y)
	      (,closed (car ,x) (car ,y))
	      (,closed (car ,x) ,y)))
	 (t
	  (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))

;;; Numeric-Bound-Test*  --  Internal
;;;
;;;    Used to compare upper and lower bounds.  This is different from the
;;; same-bound case:
;;; -- Since X = NIL is -infinity, whereas y = NIL is +infinity, we return true
;;;    if *either* arg is NIL.
;;; -- an open inner bound is "greater" and also squeezes the interval, causing
;;;    us to use the Open test for those cases as well.
;;;
(defmacro numeric-bound-test* (x y closed open)
  `(cond ((not ,y) t)
	 ((not ,x) t)
	 ((consp ,x)
	  (if (consp ,y)
	      (,open (car ,x) (car ,y))
	      (,open (car ,x) ,y)))
	 (t
	  (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))

;;; Numeric-Bound-Max  --  Internal
;;;
;;;    Return whichever of the numeric bounds X and Y is "maximal" according to
;;; the predicates Closed (e.g. >=) and Open (e.g. >).  This is only meaningful
;;; for maximizing like bounds, i.e. upper and upper.  If Max-P is true, then
;;; we return NIL if X or Y is NIL, otherwise we return the other arg.
;;;
(defmacro numeric-bound-max (x y closed open max-p)
  (once-only ((n-x x)
	      (n-y y))
    `(cond ((not ,n-x) ,(if max-p nil n-y))
	   ((not ,n-y) ,(if max-p nil n-x))
	   ((consp ,n-x)
	    (if (consp ,n-y)
		(if (,closed (car ,n-x) (car ,n-y)) ,n-x ,n-y)
		(if (,open (car ,n-x) ,n-y) ,n-x ,n-y)))
	   (t
	    (if (consp ,n-y)
		(if (,open (car ,n-y) ,n-x) ,n-y ,n-x)
		(if (,closed ,n-y ,n-x) ,n-y ,n-x))))))

(define-type-method (number :simple-subtypep) (type1 type2)
  (let ((class1 (numeric-type-class type1))
	(class2 (numeric-type-class type2))
	(complexp2 (numeric-type-complexp type2))
	(format2 (numeric-type-format type2))
	(low1 (numeric-type-low type1))
	(high1 (numeric-type-high type1))
	(low2 (numeric-type-low type2))
	(high2 (numeric-type-high type2)))
    ;; If one is complex and the other isn't, they are disjoint.
    (cond ((not (or (eq (numeric-type-complexp type1) complexp2)
		    (null complexp2)))
	   (values nil t))
	  ;; If the classes are specified and different, the types are
	  ;; disjoint unless type2 is rational and type1 is integer.
	  ;; [ or type1 is INTEGER and type2 is of the form (RATIONAL
	  ;; X X) for integral X, but this is dealt with in the
	  ;; canonicalization inside MAKE-NUMERIC-TYPE ]
	  ((not (or (eq class1 class2)
		    (null class2)
		    (and (eq class1 'integer) (eq class2 'rational))))
	   (values nil t))
	  ;; If the float formats are specified and different, the types
	  ;; are disjoint.
	  ((not (or (eq (numeric-type-format type1) format2)
		    (null format2)))
	   (values nil t))
	  ;; Check the bounds.
	  ((and (numeric-bound-test low1 low2 >= >)
		(numeric-bound-test high1 high2 <= <))
	   (values t t))
	  (t
	   (values nil t)))))

(define-superclasses number (generic-number))

;;; NUMERIC-TYPES-ADJACENT  --  Internal
;;;
;;;    If the high bound of Low is adjacent to the low bound of High, then
;;; return True, otherwise NIL.
;;;
(defun numeric-types-adjacent (low high)
  (let ((low-bound (numeric-type-high low))
	(high-bound (numeric-type-low high)))
    (cond ((not (and low-bound high-bound)) nil)
	  ((and (consp low-bound) (consp high-bound)) nil)
	  ((consp low-bound)
	   (let ((low-value (car low-bound)))
	     (or (eql low-value high-bound)
		 (and (eql low-value -0f0) (eql high-bound 0f0))
		 (and (eql low-value 0f0) (eql high-bound -0f0))
		 (and (eql low-value -0d0) (eql high-bound 0d0))
		 (and (eql low-value 0d0) (eql high-bound -0d0)))))
	  ((consp high-bound)
	   (let ((high-value (car high-bound)))
	     (or (eql high-value low-bound)
		 (and (eql high-value -0f0) (eql low-bound 0f0))
		 (and (eql high-value 0f0) (eql low-bound -0f0))
		 (and (eql high-value -0d0) (eql low-bound 0d0))
		 (and (eql high-value 0d0) (eql low-bound -0d0)))))
	  ((and (eq (numeric-type-class low) 'integer)
		(eq (numeric-type-class high) 'integer))
	   (eql (1+ low-bound) high-bound))
	  (t
	   nil))))


;;; NUMBER :SIMPLE-UNION method  -- Internal
;;;
;;; Return the a numeric type that is a supertype for both type1 and type2.
;;; 
;;; ### Note: we give up early, so keep from dropping lots of information on
;;; the floor by returning overly general types.
;;;
(define-type-method (number :simple-union) (type1 type2)
  (declare (type numeric-type type1 type2))
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t
	 (let ((class1 (numeric-type-class type1))
	       (format1 (numeric-type-format type1))
	       (complexp1 (numeric-type-complexp type1))
	       (class2 (numeric-type-class type2))
	       (format2 (numeric-type-format type2))
	       (complexp2 (numeric-type-complexp type2)))
	   (cond
	     ((and (eq class1 class2)
		   (eq format1 format2)
		   (eq complexp1 complexp2)
		   (or (numeric-types-intersect type1 type2)
		       (numeric-types-adjacent type1 type2)
		       (numeric-types-adjacent type2 type1)))
	      (make-numeric-type
	       :class class1
	       :format format1
	       :complexp complexp1
	       :low (numeric-bound-max (numeric-type-low type1)
				       (numeric-type-low type2)
				       <= < t)
	       :high (numeric-bound-max (numeric-type-high type1)
					(numeric-type-high type2)
					>= > t)))
	     ;; FIXME: These two clauses are almost identical, and the
	     ;; consequents are in fact identical in every respect.
	     ((and (eq class1 'rational)
		   (eq class2 'integer)
		   (eq format1 format2)
		   (eq complexp1 complexp2)
		   (integerp (numeric-type-low type2))
		   (integerp (numeric-type-high type2))
		   (= (numeric-type-low type2) (numeric-type-high type2))
		   (or (numeric-types-adjacent type1 type2)
		       (numeric-types-adjacent type2 type1)))
	      (make-numeric-type
	       :class 'rational
	       :format format1
	       :complexp complexp1
	       :low (numeric-bound-max (numeric-type-low type1)
				       (numeric-type-low type2)
				       <= < t)
	       :high (numeric-bound-max (numeric-type-high type1)
					(numeric-type-high type2)
					>= > t)))
	     ((and (eq class1 'integer)
		   (eq class2 'rational)
		   (eq format1 format2)
		   (eq complexp1 complexp2)
		   (integerp (numeric-type-low type1))
		   (integerp (numeric-type-high type1))
		   (= (numeric-type-low type1) (numeric-type-high type1))
		   (or (numeric-types-adjacent type1 type2)
		       (numeric-types-adjacent type2 type1)))
	      (make-numeric-type
	       :class 'rational
	       :format format1
	       :complexp complexp1
	       :low (numeric-bound-max (numeric-type-low type1)
				       (numeric-type-low type2)
				       <= < t)
	       :high (numeric-bound-max (numeric-type-high type1)
					(numeric-type-high type2)
					>= > t)))
	     (t nil))))))

(cold-load-init
  (setf (info type kind 'number) :primitive)
  (setf (info type builtin 'number)
	(make-numeric-type :complexp nil)))

(def-type-translator complex (&optional (typespec '*))
  (labels ((not-numeric ()
	     (error "The component type for COMPLEX is not numeric: ~S"
		    typespec))
	   (not-real ()
	     (error "The component type for COMPLEX is not real: ~S"
		    typespec))
	   (complex1 (component-type)
	     (unless (numeric-type-p component-type)
	       (not-numeric))
	     (when (eq (numeric-type-complexp component-type) :complex)
	       (not-real))
	     (modified-numeric-type component-type :complexp :complex))
	   (complex-union (component)
	     (unless (numberp component)
	       (not-numeric))
	     ;; KLUDGE: This TYPECASE more or less does
	     ;; (UPGRADED-COMPLEX-PART-TYPE (TYPE-OF COMPONENT)),
	     ;; (plus a small hack to treat (EQL COMPONENT 0) specially)
	     ;; but uses logic cut and pasted from the DEFUN of
	     ;; UPGRADED-COMPLEX-PART-TYPE. That's fragile, because
	     ;; changing the definition of UPGRADED-COMPLEX-PART-TYPE
	     ;; would tend to break the code here. Unfortunately,
	     ;; though, reusing UPGRADED-COMPLEX-PART-TYPE here
	     ;; would cause another kind of fragility, because
	     ;; ANSI's definition of TYPE-OF is so weak that e.g.
	     ;; (UPGRADED-COMPLEX-PART-TYPE (TYPE-OF 1/2)) could
	     ;; end up being (UPGRADED-COMPLEX-PART-TYPE 'REAL)
	     ;; instead of (UPGRADED-COMPLEX-PART-TYPE 'RATIONAL).
	     ;; So using TYPE-OF would mean that ANSI-conforming
	     ;; maintenance changes in TYPE-OF could break the code here.
	     ;; It's not clear how best to fix this. -- WHN 2002-01-21,
	     ;; trying to summarize CSR's concerns in his patch
	     (typecase component
	       (complex (error "The component type for COMPLEX (EQL X) ~
                                    is complex: ~S"
			       component))
	       ((eql 0) (specifier-type nil)) ; as required by ANSI
	       (single-float (specifier-type '(complex single-float)))
	       (double-float (specifier-type '(complex double-float)))
	       #+long-float
	       (long-float (specifier-type '(complex long-float)))
	       #+double-double
	       (double-double-float (specifier-type '(complex double-double-float)))
	       (rational (specifier-type '(complex rational)))
	       (t (specifier-type '(complex real))))))
    (let ((ctype (specifier-type typespec)))
      (typecase ctype
	(numeric-type
	 (if (csubtypep ctype (specifier-type 'rational))
	     (complex1 (specifier-type 'rational))
	     (complex1 ctype)))
	(union-type (apply #'type-union
			   ;; FIXME: This code could suffer from
			   ;; (admittedly very obscure) cases of
			   ;; bug 145 e.g. when TYPE is
			   ;;   (OR (AND INTEGER (SATISFIES ODDP))
			   ;;       (AND FLOAT (SATISFIES FOO))
			   ;; and not even report the problem very well.
			   (mapcar #'complex1
				   (union-type-types ctype))))
	(member-type
	 ;; MEMBER-TYPE is almost the same as UNION-TYPE, but there's
	 ;; a gotcha: (COMPLEX (EQL 0)) is unclear to me (rtoy).  For
	 ;; now if the typespec is a subtype of rational, we create
	 ;; (COMPLEX RATIONAL).
	 (if (csubtypep ctype (specifier-type 'rational))
	     (complex1 (specifier-type 'rational))
	     (apply #'type-union
		    (mapcar #'complex-union
			    (member-type-members ctype)))))
	(named-type
	 (cond ((eq (named-type-name ctype) '*)
		;; (COMPLEX *) is the same as (COMPLEX REAL) for us.
		(apply #'type-union
		       (mapcar #'complex1
			       (union-type-types (specifier-type 'real)))))
	       ((eq (named-type-name ctype) nil)
		;; (COMPLEX NIL) is the NIL type
		*empty-type*)
	       (t
		(not-real))))
	(t
	 (multiple-value-bind (subtypep certainly)
	     (csubtypep ctype (specifier-type 'real))
	   (if (and (not subtypep) certainly)
	       (not-real)
	       ;; ANSI just says that TYPESPEC is any subtype of
	       ;; type REAL, not necessarily a NUMERIC-TYPE. In
	       ;; particular, at this point TYPESPEC could legally be
	       ;; an intersection type like (AND REAL (SATISFIES ODDP)),
	       ;; in which case we fall through the logic above and
	       ;; end up here, stumped.
	       (error "~@<(known bug #145): The type ~S is too hairy to be 
                         used for a COMPLEX component.~:@>"
		      typespec))))))))

;;; Check-Bound  --  Internal
;;;
;;;    Check that X is a well-formed numeric bound of the specified Type.
;;; If X is *, return NIL, otherwise return the bound.
;;;
(defmacro check-bound (x type)
  `(cond ((eq ,x '*) nil)
	 ((or (typep ,x ',type)
	      (and (consp ,x) (typep (car ,x) ',type) (null (cdr ,x))))
	  ,x)
	 (t
	  (simple-program-error "Bound is not *, a ~A or a list of a ~A: ~S"
	                        ',type ',type ,x))))

(def-type-translator integer (&optional low high)
  (let* ((l (check-bound low integer))
	 (lb (if (consp l) (1+ (car l)) l))
	 (h (check-bound high integer))
	 (hb (if (consp h) (1- (car h)) h)))
    (if (and hb lb (< hb lb))
	;; This used to signal an error when the lb > hb, but the CLHS
	;; doesn't say that this is an error, so we silently accept it
	;; (as the empty type).
	*empty-type*
	(make-numeric-type :class 'integer  :complexp :real
			   :enumerable (not (null (and l h)))
			   :low lb
			   :high hb))))

(defmacro def-bounded-type (type class format)
  `(def-type-translator ,type (&optional low high)
     (let ((lb (check-bound low ,type))
	   (hb (check-bound high ,type)))
       ;; We used to signal an error here if the lower bound was
       ;; greater then the upper, but the CLHS doesn't say we should,
       ;; so we silently accept it as the empty type.
       (if (numeric-bound-test* lb hb <= <)
	   (make-numeric-type :class ',class :format ',format :low lb :high hb)
	   *empty-type*))))

(def-bounded-type rational rational nil)


(deftype mod (n)
  (unless (and (integerp n) (> n 0))
    (simple-program-error "Bad N specified for MOD type specifier: ~S." n))
  `(integer 0 ,(1- n)))

(deftype signed-byte (&optional s)
  (cond ((eq s '*) 'integer)
	((and (integerp s) (> s 0))
	 (let ((bound (ash 1 (1- s))))
	   `(integer ,(- bound) ,(1- bound))))
	(t
	 (simple-program-error 
	  "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))

(deftype unsigned-byte (&optional s)
  (cond ((eq s '*) '(integer 0))
	((and (integerp s) (> s 0))
	 `(integer 0 ,(1- (ash 1 s))))
	(t
	 (simple-program-error 
	  "Bad size specified for UNSIGNED-BYTE type specifier: ~S." s))))


;;; Unlike CMU CL, we represent the types FLOAT and REAL as
;;; UNION-TYPEs of more primitive types, in order to make
;;; type representation more unique, avoiding problems in the
;;; simplification of things like
;;;   (subtypep '(or (single-float -1.0 1.0) (single-float 0.1))
;;;             '(or (real -1 7) (single-float 0.1) (single-float -1.0 1.0)))
;;; When we allowed REAL to remain as a separate NUMERIC-TYPE,
;;; it was too easy for the first argument to be simplified to
;;; '(SINGLE-FLOAT -1.0), and for the second argument to be simplified
;;; to '(OR (REAL -1 7) (SINGLE-FLOAT 0.1)) and then for the
;;; SUBTYPEP to fail (returning NIL,T instead of T,T) because
;;; the first argument can't be seen to be a subtype of any of the
;;; terms in the second argument.
;;;
;;; The old CMU CL way was:
;;;   (!def-bounded-type float float nil)
;;;   (!def-bounded-type real nil nil)
;;;
;;; FIXME: If this new way works for a while with no weird new
;;; problems, we can go back and rip out support for separate FLOAT
;;; and REAL flavors of NUMERIC-TYPE. The new way was added in
;;; sbcl-0.6.11.22, 2001-03-21.
;;;
;;; FIXME: It's probably necessary to do something to fix the
;;; analogous problem with INTEGER and RATIONAL types. Perhaps
;;; bounded RATIONAL types should be represented as (OR RATIO INTEGER).
(defun coerce-bound (bound type inner-coerce-bound-fun)
  (declare (type function inner-coerce-bound-fun))
  (cond ((eql bound '*)
	 bound)
	((consp bound)
	 (destructuring-bind (inner-bound) bound
	   (list (funcall inner-coerce-bound-fun inner-bound type))))
	(t
	 (funcall inner-coerce-bound-fun bound type))))

(defun inner-coerce-real-bound (bound type)
  (ecase type
    (rational (rationalize bound))
    (float (if (floatp bound)
	       bound
	       ;; Coerce to the widest float format available, to
	       ;; avoid unnecessary loss of precision:
	       (coerce bound 'long-float)))))

(defun coerced-real-bound (bound type)
  (coerce-bound bound type #'inner-coerce-real-bound))

(defun inner-coerce-float-bound (bound type)
  (if (and (floatp bound) (not (typep bound type)))
      ;; Return NIL if we can't coerce the floating-point bound to the
      ;; given type.  Typically, a large number that won't fit in a
      ;; single-float.
      (ignore-errors (coerce bound type))
      (coerce bound type)))

(defun coerced-float-bound (bound type)
  (let ((bound (coerce-bound bound type #'inner-coerce-float-bound)))
    ;; If the resulting bound is NIL or '(NIL), convert that to '* to
    ;; mean unbounded.
    (if (and (listp bound)
	     (or (null bound)
		 (null (car bound))))
	'*
	bound)))

(def-type-translator real (&optional (low '*) (high '*))
  (specifier-type `(or (float ,(coerced-real-bound  low 'float)
			      ,(coerced-real-bound high 'float))
		       (rational ,(coerced-real-bound  low 'rational)
				 ,(coerced-real-bound high 'rational)))))

(def-type-translator float (&optional (low '*) (high '*))
  (specifier-type 
   `(or (single-float ,(coerced-float-bound  low 'single-float)
		      ,(coerced-float-bound high 'single-float))
	(double-float ,(coerced-float-bound  low 'double-float)
		      ,(coerced-float-bound high 'double-float))
	#+long-float ,(error "stub: no long float support yet")
	;; Can't add bound for this yet.  Don't know why.
	#+double-double
	(double-double-float #+nil ,(coerced-float-bound  low 'double-double-float)
			     #+nil ,(coerced-float-bound high 'double-double-float)))))

(defmacro define-float-format (f)
  `(def-bounded-type ,f float ,f))

(define-float-format short-float)
(define-float-format single-float)
(define-float-format double-float)
(define-float-format long-float)
#+double-double
(define-float-format double-double-float)

(defun numeric-types-intersect (type1 type2)
  (declare (type numeric-type type1 type2))
  (let* ((class1 (numeric-type-class type1))
	 (class2 (numeric-type-class type2))
	 (complexp1 (numeric-type-complexp type1))
	 (complexp2 (numeric-type-complexp type2))
	 (format1 (numeric-type-format type1))
	 (format2 (numeric-type-format type2))
	 (low1 (numeric-type-low type1))
	 (high1 (numeric-type-high type1))
	 (low2 (numeric-type-low type2))
	 (high2 (numeric-type-high type2)))
    ;; If one is complex and the other isn't, then they are disjoint.
    (cond ((not (or (eq complexp1 complexp2)
		    (null complexp1) (null complexp2)))
	   nil)
	  ;; If either type is a float, then the other must either be
	  ;; specified to be a float or unspecified. Otherwise, they
	  ;; are disjoint.
	  ((and (eq class1 'float)
		(not (member class2 '(float nil)))) nil)
	  ((and (eq class2 'float)
		(not (member class1 '(float nil)))) nil)
	  ;; If the float formats are specified and different, the
	  ;; types are disjoint.
	  ((not (or (eq format1 format2) (null format1) (null format2)))
	   nil)
	  (t
	   ;; Check the bounds. This is a bit odd because we must
	   ;; always have the outer bound of the interval as the
	   ;; second arg.
	   (if (numeric-bound-test high1 high2 <= <)
	       (or (and (numeric-bound-test low1 low2 >= >)
			(numeric-bound-test* low1 high2 <= <))
		   (and (numeric-bound-test low2 low1 >= >)
			(numeric-bound-test* low2 high1 <= <)))
	       (or (and (numeric-bound-test* low2 high1 <= <)
			(numeric-bound-test low2 low1 >= >))
		   (and (numeric-bound-test high2 high1 <= <)
			(numeric-bound-test* high2 low1 >= >))))))))

;;; Round-Numeric-Bound  --  Internal
;;;
;;;    Take the numeric bound X and convert it into something that can be used
;;; as a bound in a numeric type with the specified Class and Format.  If up-p
;;; is true, then we round up as needed, otherwise we round down.  Up-p true
;;; implies that X is a lower bound, i.e. (N) > N.
;;;
;;; This is used by Numeric-Type-Intersection to mash the bound into the
;;; appropriate type number.  X may only be a float when Class is Float.
;;;
;;; ### Note: it is possible for the coercion to a float to overflow or
;;; underflow.  This happens when the bound doesn't fit in the specified
;;; format.  In this case, we should really return the appropriate
;;; {Most | Least}-{Positive | Negative}-XXX-Float float of desired format.
;;; But these conditions aren't currently signalled in any useful way.
;;;
;;; Also, when converting an open rational bound into a float we should
;;; probably convert it to a closed bound of the closest float in the specified
;;; format.  In general, open float bounds are fucked.
;;;
(defun round-numeric-bound (x class format up-p)
  (if x
      (let ((cx (if (consp x) (car x) x)))
	(ecase class
	  ((nil rational) x)
	  (integer
	   (if (and (consp x) (integerp cx))
	       (if up-p (1+ cx) (1- cx))
	       (if up-p (ceiling cx) (floor cx))))
	  (float
	   (let ((res (if format (coerce cx format) (float cx))))
	     (if (consp x) (list res) res)))))
      nil))


;;; Number :Simple-Intersection type method  --  Internal
;;;
;;;    Handle the case of Type-Intersection on two numeric types.  We use
;;; Types-Intersect to throw out the case of types with no intersection.  If an
;;; attribute in Type1 is unspecified, then we use Type2's attribute, which
;;; must be at least as restrictive.  If the types intersect, then the only
;;; attributes that can be specified and different are the class and the
;;; bounds.
;;;
;;;    When the class differs, we use the more restrictive class.  The only
;;; interesting case is rational/integer, since rational includes integer.
;;;
;;;    We make the result lower (upper) bound the maximum (minimum) of the
;;; argument lower (upper) bounds.  We convert the bounds into the
;;; appropriate numeric type before maximizing.  This avoids possible confusion
;;; due to mixed-type comparisons (but I think the result is the same).
;;;
(define-type-method (number :simple-intersection) (type1 type2)
  (declare (type numeric-type type1 type2))
  (if (numeric-types-intersect type1 type2)
      (let* ((class1 (numeric-type-class type1))
	     (class2 (numeric-type-class type2))
	     (class (ecase class1
		      ((nil) class2)
		      ((integer float) class1)
		      (rational (if (eq class2 'integer)
				       'integer
				       'rational))))
	     (format (or (numeric-type-format type1)
			 (numeric-type-format type2))))
	(make-numeric-type
	 :class class
	 :format format
	 :complexp (or (numeric-type-complexp type1)
		       (numeric-type-complexp type2))
	 :low (numeric-bound-max
	       (round-numeric-bound (numeric-type-low type1)
				    class format t)
	       (round-numeric-bound (numeric-type-low type2)
				    class format t)
	       > >= nil)
	 :high (numeric-bound-max
		(round-numeric-bound (numeric-type-high type1)
				     class format nil)
		(round-numeric-bound (numeric-type-high type2)
				     class format nil)
		< <= nil)))
      *empty-type*))

;;; Float-Format-Max  --  Interface
;;;
;;;    Given two float formats, return the one with more precision.  If either
;;; one is null, return NIL.
;;;
(defun float-format-max (f1 f2)
  (when (and f1 f2)
    (dolist (f float-formats (error "Bad float format: ~S." f1))
      (when (or (eq f f1) (eq f f2))
	(return f)))))


;;; Numeric-Contagion  --  Interface
;;;
;;;    Return the result of an operation on Type1 and Type2 according to the
;;; rules of numeric contagion.  This is always NUMBER, some float format
;;; (possibly complex) or RATIONAL.  Due to rational canonicalization, there
;;; isn't much we can do here with integers or rational complex numbers.
;;;
;;;    If either argument is not a Numeric-Type, then return NUMBER.  This is
;;; useful mainly for allowing types that are technically numbers, but not a
;;; Numeric-Type. 
;;;
(defun numeric-contagion (type1 type2)
  (if (and (numeric-type-p type1) (numeric-type-p type2))
      (let ((class1 (numeric-type-class type1))
	    (class2 (numeric-type-class type2))
	    (format1 (numeric-type-format type1))
	    (format2 (numeric-type-format type2))
	    (complexp1 (numeric-type-complexp type1))
	    (complexp2 (numeric-type-complexp type2)))
	(cond ((or (null complexp1)
		   (null complexp2))
	       (specifier-type 'number))
	      ((eq class1 'float)
	       (make-numeric-type
		:class 'float
		:format (ecase class2
			  (float (float-format-max format1 format2))
			  ((integer rational) format1)
			  ((nil)
			   ;; A double-float with any real number is a
			   ;; double-float.
			   #-(and long-float (not double-double))
			   (if (eq format1 'double-float) 'double-float nil)
			   ;; A long-float with any real number is a
			   ;; long-float.
			   #+(and long-float (not double-double))
			   (if (eq format1 'long-float) 'long-float nil)
			   #+(and (not long-float) double-double)
			   (if (eq format1 'double-double-float) 'double-double-float nil)
			   ))
		:complexp (if (or (eq complexp1 :complex)
				  (eq complexp2 :complex))
			      :complex
			      :real)))
	      ((eq class2 'float) (numeric-contagion type2 type1))
	      ((and (eq complexp1 :real) (eq complexp2 :real))
	       (make-numeric-type
		:class (and class1 class2 'rational)
		:complexp :real))
	      (t
	       (specifier-type 'number))))
      (specifier-type 'number)))


;;;; Array types:

;;; Specialized-Element-Type-Maybe  --  Internal
;;;
;;;      What this does depends on the setting of the
;;; *use-implementation-types* switch.  If true, return the specialized element
;;; type, otherwise return the original element type.
;;;
(defun specialized-element-type-maybe (type)
  (declare (type array-type type))
  (if *use-implementation-types*
      (array-type-specialized-element-type type)
      (array-type-element-type type)))

(define-type-method (array :simple-=) (type1 type2)
  (if (or (unknown-type-p (array-type-element-type type1))
	  (unknown-type-p (array-type-element-type type2)))
      (multiple-value-bind (equalp certainp)
	  (type= (array-type-element-type type1)
		 (array-type-element-type type2))
	;; by its nature, the call to TYPE= should never return NIL,
	;; T, as we don't know what the UNKNOWN-TYPE will grow up to
	;; be.  -- CSR, 2002-08-19
	(assert (not (and (not equalp) certainp)))
	(values equalp certainp))
      (values (and (equal (array-type-dimensions type1)
			  (array-type-dimensions type2))
		   (eq (array-type-complexp type1)
		       (array-type-complexp type2))
		   (type= (specialized-element-type-maybe type1)
			  (specialized-element-type-maybe type2)))
	      t)))

(define-type-method (array :unparse) (type)
  (let ((dims (array-type-dimensions type))
	(eltype (type-specifier (array-type-element-type type)))
	(complexp (array-type-complexp type)))
    (cond ((eq dims '*)
	   (if (eq eltype '*)
	       (if complexp 'array 'simple-array)
	       (if complexp `(array ,eltype) `(simple-array ,eltype))))
	  ((= (length dims) 1)
	   (if complexp
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'bit-vector)
		     (base-char 'base-string)
		     (character 'string)
		     (* 'vector)
		     (t `(vector ,eltype)))
		   (case eltype
		     (bit `(bit-vector ,(car dims)))
		     (base-char `(base-string ,(car dims)))
		     (character `(string ,(car dims)))
		     (t `(vector ,eltype ,(car dims)))))
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'simple-bit-vector)
		     (base-char 'simple-base-string)
		     (character 'simple-string)
		     ((t) 'simple-vector)
		     (t `(simple-array ,eltype (*))))
		   (case eltype
		     (bit `(simple-bit-vector ,(car dims)))
		     (base-char `(simple-base-string ,(car dims)))
		     (character `(simple-string ,(car dims)))
		     ((t) `(simple-vector ,(car dims)))
		     (t `(simple-array ,eltype ,dims))))))
	  (t
	   (if complexp
	       `(array ,eltype ,dims)
	       `(simple-array ,eltype ,dims))))))

(define-type-method (array :simple-subtypep) (type1 type2)
  (let ((dims1 (array-type-dimensions type1))
	(dims2 (array-type-dimensions type2))
	(complexp2 (array-type-complexp type2)))
    (cond (;; not subtypep unless dimensions are compatible
	   (not (or (eq dims2 '*)
		    (and (not (eq dims1 '*))
			 ;; (sbcl-0.6.4 has trouble figuring out that
			 ;; DIMS1 and DIMS2 must be lists at this
			 ;; point, and knowing that is important to
			 ;; compiling EVERY efficiently.)
			 (= (length (the list dims1))
			    (length (the list dims2)))
			 (every (lambda (x y)
				  (or (eq y '*) (eql x y)))
				(the list dims1)
				(the list dims2)))))
	   (values nil t))
	  ;; not subtypep unless complexness is compatible
	  ((not (or (eq complexp2 :maybe)
		    (eq (array-type-complexp type1) complexp2)))
	   (values nil t))
	  ;; Since we didn't fail any of the tests above, we win
	  ;; if the TYPE2 element type is wild.
	  ((eq (array-type-element-type type2) *wild-type*)
	   (values t t))
	  (;; Since we didn't match any of the special cases above, we
	   ;; can't give a good answer unless both the element types
	   ;; have been defined.
	   (or (unknown-type-p (array-type-element-type type1))
	       (unknown-type-p (array-type-element-type type2)))
	   (values nil nil))
	  (;; Otherwise, the subtype relationship holds iff the
	   ;; types are equal, and they're equal iff the specialized
	   ;; element types are identical.
	   t
	   (values (type= (specialized-element-type-maybe type1)
			  (specialized-element-type-maybe type2))
		   t)))))

(define-superclasses array
  (string string)
  (vector vector)
  (array))

(defun array-types-intersect (type1 type2)
  (declare (type array-type type1 type2))
  (let ((dims1 (array-type-dimensions type1))
	(dims2 (array-type-dimensions type2))
	(complexp1 (array-type-complexp type1))
	(complexp2 (array-type-complexp type2)))
    ;; See whether dimensions are compatible.
    (cond ((not (or (eq dims1 '*) (eq dims2 '*)
		    (and (= (length dims1) (length dims2))
			 (every (lambda (x y)
				  (or (eq x '*) (eq y '*) (= x y)))
				dims1 dims2))))
	   (values nil t))
	  ;; See whether complexpness is compatible.
	  ((not (or complexp1
		    complexp2
		    (eq complexp1 complexp2)))
	   (values nil t))
	  ;; Old comment:
	  ;;
	  ;;   If either element type is wild, then they intersect.
	  ;;   Otherwise, the types must be identical.
	  ;;
	  ;; FIXME: There seems to have been a fair amount of
	  ;; confusion about the distinction between requested element
	  ;; type and specialized element type; here is one of
	  ;; them. If we request an array to hold objects of an
	  ;; unknown type, we can do no better than represent that
	  ;; type as an array specialized on wild-type.  We keep the
	  ;; requested element-type in the -ELEMENT-TYPE slot, and
	  ;; *WILD-TYPE* in the -SPECIALIZED-ELEMENT-TYPE.  So, here,
	  ;; we must test for the SPECIALIZED slot being *WILD-TYPE*,
	  ;; not just the ELEMENT-TYPE slot.  Maybe the return value
	  ;; in that specific case should be T, NIL?  Or maybe this
	  ;; function should really be called
	  ;; ARRAY-TYPES-COULD-POSSIBLY-INTERSECT?  In any case, this
	  ;; was responsible for bug #123, and this whole issue could
	  ;; do with a rethink and/or a rewrite.  -- CSR, 2002-08-21
	  ((or (eq (array-type-specialized-element-type type1) *wild-type*)
	       (eq (array-type-specialized-element-type type2) *wild-type*)
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))
	   (values t t))
	  (t
	   (values nil t)))))

(define-type-method (array :simple-intersection) (type1 type2)
  (declare (type array-type type1 type2))
  (if (array-types-intersect type1 type2)
      (let ((dims1 (array-type-dimensions type1))
	    (dims2 (array-type-dimensions type2))
	    (complexp1 (array-type-complexp type1))
	    (complexp2 (array-type-complexp type2))
	    (eltype1 (array-type-element-type type1))
	    (eltype2 (array-type-element-type type2)))
	(specialize-array-type
	 (make-array-type
	  :dimensions (cond ((eq dims1 '*) dims2)
			    ((eq dims2 '*) dims1)
			    (t (mapcar (lambda (x y) (if (eq x '*) y x))
				       dims1 dims2)))
	  :complexp (if (eq complexp1 :maybe) complexp2 complexp1)
	  :element-type (if (eq eltype1 *wild-type*) eltype2 eltype1))))
      *empty-type*))

;;; Check-Array-Dimensions  --  Internal
;;;
;;;    Check a supplied dimension list to determine if it is legal.
;;;
(defun check-array-dimensions (dims)
  (typecase dims
    ((member *) dims)
    (integer
     (when (minusp dims)
       (simple-program-error
        "Arrays can't have a negative number of dimensions: ~D." dims))
     (when (>= dims array-rank-limit)
       (simple-program-error "Array type has too many dimensions: ~S." dims))
     (make-list dims :initial-element '*))
    (list
     (when (>= (length dims) array-rank-limit)
       (simple-program-error "Array type has too many dimensions: ~S." dims))
     (dolist (dim dims)
       (unless (eq dim '*)
	 (unless (and (integerp dim)
		      (>= dim 0) (< dim array-dimension-limit))
	   (simple-program-error "Bad dimension in array type: ~S." dim))))
     dims)
    (t
     (simple-program-error
      "Array dimensions is not a list, integer or *:~%  ~S" dims))))

(def-type-translator array (&optional (element-type '*) (dimensions '*))
  (specialize-array-type
   (make-array-type :dimensions (check-array-dimensions dimensions)
		    :complexp :maybe
		    :element-type (specifier-type element-type))))

(def-type-translator simple-array (&optional (element-type '*) (dimensions '*))
  (specialize-array-type
   (make-array-type :dimensions (check-array-dimensions dimensions)
		    :element-type (specifier-type element-type)
		    :complexp nil)))

(deftype vector (&optional element-type size)
  `(array ,element-type (,size)))

(deftype simple-vector (&optional size)
  `(simple-array t (,size)))

(deftype base-string (&optional size)
  `(array base-char (,size)))

(deftype simple-base-string (&optional size)
  `(simple-array base-char (,size)))

(deftype string (&optional size)
  `(or (array character (,size))
       (base-string ,size)))

(deftype simple-string (&optional size)
  `(or (simple-array character (,size))
       (simple-base-string ,size)))

(deftype bit-vector (&optional size)
  `(array bit (,size)))

(deftype simple-bit-vector (&optional size)
  `(simple-array bit (,size)))


;;;; Member types.

(define-type-method (member :unparse) (type)
  (let ((members (member-type-members type)))
    (cond
      ((equal members '(nil)) 'null)
      ((type= type (specifier-type 'standard-char)) 'standard-char)
      (t `(member ,@members)))))

(define-type-method (member :simple-subtypep) (type1 type2)
  (values (subsetp (member-type-members type1) (member-type-members type2))
	  t))

(define-type-method (member :complex-subtypep-arg1) (type1 type2)
  (every/type (swapped-args-fun #'ctypep)
	      type2
	      (member-type-members type1)))

;;; We punt if the odd type is enumerable and intersects with the member type.
;;; If not enumerable, then it is definitely not a subtype of the member type.
;;;
(define-type-method (member :complex-subtypep-arg2) (type1 type2)
  (cond ((not (type-enumerable type1)) (values nil t))
	((types-intersect type1 type2)
	 (invoke-complex-subtypep-arg1-method type1 type2))
	(t (values nil t))))

(define-type-method (member :simple-intersection) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (cond ((subsetp mem1 mem2) type1)
	  ((subsetp mem2 mem1) type2)
	  (t
	   (let ((res (intersection mem1 mem2)))
	     (if res
		 (make-member-type :members res)
		 *empty-type*))))))

(define-type-method (member :complex-intersection) (type1 type2)
  (block punt
    (collect ((members))
      (let ((mem2 (member-type-members type2)))
        (dolist (member mem2)
	  (multiple-value-bind (val win) (ctypep member type1)
	    (unless win
	      (return-from punt nil))
	    (when val (members member))))
	(cond ((subsetp mem2 (members)) type2)
	      ((null (members)) *empty-type*)
	      (t
	       (make-member-type :members (members))))))))

;;; We don't need a :COMPLEX-UNION, since the only interesting case is a union
;;; type, and the member/union interaction is handled by the union type
;;; method.
(define-type-method (member :simple-union) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (cond ((subsetp mem1 mem2) type2)
	  ((subsetp mem2 mem1) type1)
	  (t
	   (make-member-type :members (union mem1 mem2))))))

(define-type-method (member :simple-=) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (values (and (subsetp mem1 mem2)
		 (subsetp mem2 mem1))
	    t)))

(define-type-method (member :complex-=) (type1 type2)
  (if (type-enumerable type1)
      (multiple-value-bind (val win) (csubtypep type2 type1)
	(if (or val (not win))
	    (values nil nil)
	    (values nil t)))
      (values nil t)))

(def-type-translator member (&rest members)
  (if members
      (collect ((non-numbers) (numbers))
	(dolist (m (remove-duplicates members))
	  (if (and (numberp m)
		   (not (and (floatp m) (zerop m))))
	      (numbers (ctype-of m))
	      (non-numbers m)))
	(apply #'type-union
	       (if (non-numbers)
		   (make-member-type :members (non-numbers))
		   *empty-type*)
	       (numbers)))
      *empty-type*))


;;;; Union types:

(define-type-method (union :unparse) (type)
  (declare (type ctype type))
  (cond
    ((type= type (specifier-type 'list)) 'list)
    ((type= type (specifier-type 'float)) 'float)
    ((type= type (specifier-type 'real)) 'real)
    ((type= type (specifier-type 'sequence)) 'sequence)
    ((type= type (specifier-type 'bignum)) 'bignum)
    ((type= type (specifier-type 'complex)) 'complex)
    (t `(or ,@(mapcar #'type-specifier (union-type-types type))))))

(define-type-method (union :simple-=) (type1 type2)
  (multiple-value-bind (subtype certain?)
      (csubtypep type1 type2)
    (if subtype
	(csubtypep type2 type1)
	(if certain?
	    (values nil t)
	    (multiple-value-bind (subtype certain?)
		(csubtypep type2 type1)
	      (declare (ignore subtype))
	      (values nil certain?))))))

(define-type-method (union :complex-=) (type1 type2)
  (declare (ignore type1))
  (if (some #'type-might-contain-other-types-p 
	    (union-type-types type2))
      (values nil nil)
      (values nil t)))

(defun union-simple-subtypep (type1 type2)
  (every/type (swapped-args-fun #'union-complex-subtypep-arg2)
	      type2
	      (union-type-types type1)))

(define-type-method (union :simple-subtypep) (type1 type2)
  (union-simple-subtypep type1 type2))

(defun union-complex-subtypep-arg1 (type1 type2)
  (every/type (swapped-args-fun #'csubtypep)
	      type2
	      (union-type-types type1)))

(define-type-method (union :complex-subtypep-arg1) (type1 type2)
  (union-complex-subtypep-arg1 type1 type2))

(defun union-complex-subtypep-arg2 (type1 type2)
  (multiple-value-bind (sub-value sub-certain?)
      (progn
	(assert (union-type-p type2))
	(assert (not (union-type-p type1)))
	(type= type1
	       (apply #'type-union
		      (mapcar (lambda (x) (type-intersection type1 x))
			      (union-type-types type2)))))
    (if sub-certain?
	(values sub-value sub-certain?)
	(invoke-complex-subtypep-arg1-method type1 type2))))

(define-type-method (union :complex-subtypep-arg2) (type1 type2)
  (union-complex-subtypep-arg2 type1 type2))

(define-type-method (union :simple-intersection :complex-intersection)
    (type1 type2)
  (assert (union-type-p type2))
  (cond ((and (union-type-p type1)
	      (union-simple-subtypep type1 type2)) type1)
	((and (union-type-p type1)
	      (union-simple-subtypep type2 type1)) type2)
	((and (not (union-type-p type1))
	      (union-complex-subtypep-arg2 type1 type2))
	 type1)
	((and (not (union-type-p type1))
	      (union-complex-subtypep-arg1 type2 type1))
	 type2)
	(t 
	 (let ((accumulator *empty-type*))
	   (dolist (t2 (union-type-types type2) accumulator)
	     (setf accumulator
		   (type-union accumulator
			       (type-intersection type1 t2))))))))

(def-type-translator or (&rest type-specifiers)
  (apply #'type-union
	 (mapcar #'specifier-type
		 type-specifiers)))


;;;; Intersection Types

(define-type-method (intersection :unparse) (type)
  (declare (type ctype type))
  (or (find type '(ratio keyword) :key #'specifier-type :test #'type=)
      `(and ,@(mapcar #'type-specifier (intersection-type-types type)))))

;;; shared machinery for type equality: true if every type in the set
;;; TYPES1 matches a type in the set TYPES2 and vice versa
(defun type=-set (types1 types2)
  (flet (;; true if every type in the set X matches a type in the set Y
	 (type<=-set (x y)
	   (declare (type list x y))
	   (every (lambda (xelement)
		    (position xelement y :test #'type=))
		  x)))
    (values (and (type<=-set types1 types2)
		 (type<=-set types2 types1))
	    t)))

(define-type-method (intersection :simple-=) (type1 type2)
  (type=-set (intersection-type-types type1)
	     (intersection-type-types type2)))

(defun %intersection-complex-subtypep-arg1 (type1 type2)
  (type= type1 (type-intersection type1 type2)))

(defun %intersection-simple-subtypep (type1 type2)
  (every/type #'%intersection-complex-subtypep-arg1
	      type1
	      (intersection-type-types type2)))

(define-type-method (intersection :simple-subtypep) (type1 type2)
  (%intersection-simple-subtypep type1 type2))
  
(define-type-method (intersection :complex-subtypep-arg1) (type1 type2)
  (%intersection-complex-subtypep-arg1 type1 type2))

(defun %intersection-complex-subtypep-arg2 (type1 type2)
  (every/type #'csubtypep type1 (intersection-type-types type2)))

(define-type-method (intersection :complex-subtypep-arg2) (type1 type2)
  (%intersection-complex-subtypep-arg2 type1 type2))

(define-type-method (intersection :simple-union :complex-union)
    (type1 type2)
  (assert (intersection-type-p type2))
  (cond ((and (intersection-type-p type1)
	      (%intersection-simple-subtypep type1 type2)) type2)
	((and (intersection-type-p type1)
	      (%intersection-simple-subtypep type2 type1)) type1)
	((and (not (intersection-type-p type1))
	      (%intersection-complex-subtypep-arg2 type1 type2))
	 type2)
	((and (not (intersection-type-p type1))
	      (%intersection-complex-subtypep-arg1 type2 type1))
	 type1)
	((and (csubtypep type2 (specifier-type 'ratio))
	      (numeric-type-p type1)
	      (csubtypep type1 (specifier-type 'integer))
	      (csubtypep type2
			 (make-numeric-type
			  :class 'rational
			  :complexp nil
			  :low (if (null (numeric-type-low type1))
				   nil
				   (list (1- (numeric-type-low type1))))
			  :high (if (null (numeric-type-high type1))
				    nil
				    (list (1+ (numeric-type-high type1)))))))
	 (type-union type1
		     (apply #'type-intersection
			    (remove (specifier-type '(not integer))
				    (intersection-type-types type2)
				    :test #'type=))))
	(t
	 (let ((accumulator *universal-type*))
	   (do ((t2s (intersection-type-types type2) (cdr t2s)))
	       ((null t2s) accumulator)
	     (let ((union (type-union type1 (car t2s))))
	       (when (union-type-p union)
		 (if (and (eq accumulator *universal-type*)
			  (null (cdr t2s)))
		     (return union)
		     (return nil)))
	       (setf accumulator
		     (type-intersection accumulator union))))))))

(def-type-translator and (&rest type-specifiers)
  (apply #'type-intersection
	 (mapcar #'specifier-type
		 type-specifiers)))


;;;; Alien-type types

(define-type-method (alien :unparse) (type)
  `(alien ,(unparse-alien-type (alien-type-type-alien-type type))))

(define-type-method (alien :simple-subtypep) (type1 type2)
  (values (alien-subtype-p (alien-type-type-alien-type type1)
			   (alien-type-type-alien-type type2))
	  t))

(define-superclasses alien (alien-value))

(define-type-method (alien :simple-=) (type1 type2)
  (let ((alien-type-1 (alien-type-type-alien-type type1))
	(alien-type-2 (alien-type-type-alien-type type2)))
    (values (or (eq alien-type-1 alien-type-2)
		(alien-type-= alien-type-1 alien-type-2))
	    t)))


(def-type-translator alien (&optional (alien-type nil))
  (typecase alien-type
    (null
     (make-alien-type-type))
    (alien-type
     (make-alien-type-type alien-type))
    (t
     (make-alien-type-type (parse-alien-type alien-type)))))

(defun make-alien-type-type (&optional alien-type)
  (if alien-type
      (let ((lisp-rep-type (compute-lisp-rep-type alien-type)))
	(if lisp-rep-type
	    (specifier-type lisp-rep-type)
	    (%make-alien-type-type alien-type)))
      *universal-type*))


;;;; Cons types:

(def-type-translator cons (&optional (car-type-spec '*) (cdr-type-spec '*))
  (let ((car-type (specifier-type car-type-spec))
	(cdr-type (specifier-type cdr-type-spec)))
    (make-cons-type car-type cdr-type)))
 
(define-type-method (cons :unparse) (type)
  (let ((car-eltype (type-specifier (cons-type-car-type type)))
	(cdr-eltype (type-specifier (cons-type-cdr-type type))))
    (if (and (member car-eltype '(t *))
	     (member cdr-eltype '(t *)))
	'cons
	`(cons ,car-eltype ,cdr-eltype))))
 
(define-type-method (cons :simple-=) (type1 type2)
  (declare (type cons-type type1 type2))
  (and (type= (cons-type-car-type type1) (cons-type-car-type type2))
       (type= (cons-type-cdr-type type1) (cons-type-cdr-type type2))))
 
(define-type-method (cons :simple-subtypep) (type1 type2)
  (declare (type cons-type type1 type2))
  (multiple-value-bind (val-car win-car)
      (csubtypep (cons-type-car-type type1) (cons-type-car-type type2))
    (multiple-value-bind (val-cdr win-cdr)
	(csubtypep (cons-type-cdr-type type1) (cons-type-cdr-type type2))
      (if (and val-car val-cdr)
	  (values t (and win-car win-cdr))
	  (values nil (or win-car win-cdr))))))
 
(define-type-method (cons :simple-union) (type1 type2)
  (declare (type cons-type type1 type2))
  (let ((car-type1 (cons-type-car-type type1))
	(car-type2 (cons-type-car-type type2))
	(cdr-type1 (cons-type-cdr-type type1))
	(cdr-type2 (cons-type-cdr-type type2)))
    ;; UGH.  -- CSR, 2003-02-24
    (macrolet ((frob-car (car1 car2 cdr1 cdr2)
		 `(type-union
		   (make-cons-type ,car1 (type-union ,cdr1 ,cdr2))
		   (make-cons-type
		    (type-intersection ,car2
		     (specifier-type
		      `(not ,(type-specifier ,car1))))
		    ,cdr2))))
      (cond ((type= car-type1 car-type2)
	     (make-cons-type car-type1
			     (type-union cdr-type1 cdr-type2)))
	    ((type= cdr-type1 cdr-type2)
	     (make-cons-type (type-union car-type1 car-type2)
			     cdr-type1))
	    ((csubtypep car-type1 car-type2)
	     (frob-car car-type1 car-type2 cdr-type1 cdr-type2))
	    ((csubtypep car-type2 car-type1)
	     (frob-car car-type2 car-type1 cdr-type2 cdr-type1))
	    ;; Don't put these in -- consider the effect of taking the
	    ;; union of (CONS (INTEGER 0 2) (INTEGER 5 7)) and
	    ;; (CONS (INTEGER 0 3) (INTEGER 5 6)).
	    #+nil
	    ((csubtypep cdr-type1 cdr-type2)
	     (frob-cdr car-type1 car-type2 cdr-type1 cdr-type2))
	    #+nil
	    ((csubtypep cdr-type2 cdr-type1)
	     (frob-cdr car-type2 car-type1 cdr-type2 cdr-type1))))))
	    
(define-type-method (cons :simple-intersection) (type1 type2)
  (declare (type cons-type type1 type2))
  (let ((car-int2 (type-intersection2 (cons-type-car-type type1)
				      (cons-type-car-type type2)))
	(cdr-int2 (type-intersection2 (cons-type-cdr-type type1)
				      (cons-type-cdr-type type2))))
    (cond ((and car-int2 cdr-int2)
	   (make-cons-type car-int2 cdr-int2))
	  (car-int2
	   (make-cons-type car-int2
			   (type-intersection (cons-type-cdr-type type1)
					      (cons-type-cdr-type type2))))
	  (cdr-int2
	   (make-cons-type (type-intersection (cons-type-car-type type1)
					      (cons-type-car-type type2))
			   cdr-int2)))))


;;; TYPE-DIFFERENCE  --  Interface
;;;
;;;    Return the type that describes all objects that are in X but not in Y.
;;; If we can't determine this type, then return NIL.
;;;
;;;    For now, we only are clever dealing with union and member types.  If
;;; either type is not a union type, then we pretend that it is a union of just
;;; one type.  What we do is remove from X all the types that are a subtype any
;;; type in Y.  If any type in X intersects with a type in Y but is not a
;;; subtype, then we give up.
;;;
;;;    We must also special-case any member type that appears in the union.  We
;;; remove from X's members all objects that are TYPEP to Y.  If Y has any
;;; members, we must be careful that none of those members are CTYPEP to any
;;; of Y's non-member types.  We give up in this case, since to compute that
;;; difference we would have to break the type from X into some collection of
;;; types that represents the type without that particular element.  This seems
;;; too hairy to be worthwhile, given its low utility.
;;;
(defun type-difference (x y)
  (let ((x-types (if (union-type-p x) (union-type-types x) (list x)))
	(y-types (if (union-type-p y) (union-type-types y) (list y))))
    (collect ((res))
      (dolist (x-type x-types)
	(if (member-type-p x-type)
	    (collect ((members))
	      (dolist (mem (member-type-members x-type))
		(multiple-value-bind (val win) (ctypep mem y)
		  (unless win (return-from type-difference nil))
		  (unless val
		    (members mem))))
	      (when (members)
		(res (make-member-type :members (members)))))
	    (dolist (y-type y-types (res x-type))
	      (multiple-value-bind (val win) (csubtypep x-type y-type)
		(unless win (return-from type-difference nil))
		(when val (return))
		(when (types-intersect x-type y-type)
		  (return-from type-difference nil))))))
      (let ((y-mem (find-if #'member-type-p y-types)))
	(when y-mem
	  (let ((members (member-type-members y-mem)))
	    (dolist (x-type x-types)
	      (unless (member-type-p x-type)
		(dolist (member members)
		  (multiple-value-bind (val win) (ctypep member x-type)
		    (when (or (not win) val)
		      (return-from type-difference nil)))))))))
      (apply #'type-union (res)))))


;;;; Miscellaneous interfaces:

;;; CLEAR-TYPE-CACHES  --  Interface
;;;
;;;    Clear memoization of all type system operations that can be altered by
;;; type definition/redefinition.
;;;
(defun clear-type-caches ()
  (when *type-system-initialized*
    (dolist (sym '(values-specifier-type-cache-clear
		   values-type-union-cache-clear
		   type-union2-cache-clear
		   values-subtypep-cache-clear
		   csubtypep-cache-clear
		   type-intersection2-cache-clear
		   values-type-intersection-cache-clear))
      (funcall (symbol-function sym))))
  (undefined-value))


;;; CTypep  --  Interface
;;;
;;;    If Type is a type that we can do a compile-time test on, then return the
;;; whether the object is of that type as the first value and second value
;;; true.  Otherwise return NIL, NIL.
;;;
;;; We give up on unknown types, pick off FUNCTION and UNION types.  For
;;; structure types, we require that the type be defined in both the current
;;; and compiler environments, and that the INCLUDES be the same.
;;;
(defun ctypep (obj type)
  (declare (type ctype type))
  (etypecase type
    ((or numeric-type named-type member-type array-type
	 kernel::built-in-class cons-type)
     (values (%typep obj type) t))
    (class
     (if (if (csubtypep type (specifier-type 'funcallable-instance))
	     (funcallable-instance-p obj)
	     (%instancep obj))
	 (if (eq (%class-layout type)
		 (info type compiler-layout (%class-name type)))
	     (values (typep obj type) t)
	     (values nil nil))
	 (values nil t)))
    (union-type
     (any/type #'ctypep obj (union-type-types type)))
    (intersection-type
     (every/type #'ctypep obj (intersection-type-types type)))
    (function-type
     (values (functionp obj) t))
    (unknown-type
     (values nil nil))
    (alien-type-type
     (values (alien-typep obj (alien-type-type-alien-type type)) t))
    (negation-type
     (multiple-value-bind (res win)
	 (ctypep obj (negation-type-type type))
       (if win
	   (values (not res) t)
	   (values nil nil))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
	    (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	 (and
	  (if (atom hairy-spec)
	      (values t t)
	      (dolist (spec (cdr hairy-spec) (values t t))
		(multiple-value-bind (res win)
		    (ctypep obj (specifier-type spec))
		  (unless win (return (values nil nil)))
		  (unless res (return (values nil t)))))))
	 (not
	  (multiple-value-bind
	      (res win)
	      (ctypep obj (specifier-type (cadr hairy-spec)))
	    (if win
		(values (not res) t)
		(values nil nil))))
	 (satisfies
	  (let ((fun (second hairy-spec)))
	    (declare (type symbol fun))
	    (if (fboundp fun)
		(values (not (null (ignore-errors (funcall fun obj)))) t)
		(values nil nil)))))))))


;;; EXTRACT-FUNCTION-TYPE  --  Interface
;;;
;;;    Pull the type specifier out of a function object.
;;;
(defun extract-function-type (fun)
  (if (eval:interpreted-function-p fun)
      (eval:interpreted-function-type fun)
      (typecase fun
	(byte-function (byte-function-type fun))
	(byte-closure (byte-function-type (byte-closure-function fun)))
	(t
	 (specifier-type (%function-type (%closure-function fun)))))))


;;; Ctype-Of  --  Interface
;;;
;;;    Like Type-Of, only returns a Type structure instead of a type
;;; specifier.  We try to return the type most useful for type checking, rather
;;; than trying to come up with the one that the user might find most
;;; informative.
;;;
(defun-cached (ctype-of
	       :hash-function (lambda (x)
				(the fixnum
				     (logand (the fixnum (cache-hash-eq x))
					     #x1FF)))
	       :hash-bits 9
	       :init-form cold-load-init)
	      ((x eq))
  (declare (values ctype))
  (typecase x
    (function
     (if (funcallable-instance-p x)
	 (kernel::class-of x)
	 (extract-function-type x)))
    (symbol
     (make-member-type :members (list x)))
    (number
     (ctype-of-number x))
    (array
     (let ((etype (specifier-type (array-element-type x))))
       (make-array-type :dimensions (array-dimensions x)
			:complexp (not (typep x 'simple-array))
			:element-type etype
			:specialized-element-type etype)))
    (cons
     (make-cons-type *universal-type* *universal-type*))
    (t
     (kernel::class-of x))))

(defun ctype-of-number (x)
  (let ((num (if (complexp x) (realpart x) x)))
    (multiple-value-bind (complexp low high)
	(if (complexp x)
	    (let ((imag (imagpart x)))
	      (values :complex (min num imag) (max num imag)))
	    (values :real num num))
      (make-numeric-type :class (etypecase num
				  ;; We used to have an INTEGER type
				  ;; here, but is it really necessary?
				  ;; I (rtoy) think RATIONAL is as
				  ;; good.
				  (rational 'rational)
				  (float 'float))
			 :format (and (floatp num) (float-format-name num))
			 :complexp complexp
			 :low low
			 :high high))))

;;; Clear this cache on GC so that we don't hold onto too much garbage.
;;;
(pushnew 'ctype-of-cache-clear *before-gc-hooks*)


;;;; Standard Deftypes.

(deftype bit () '(integer 0 1))

(deftype compiled-function () 'function)

(deftype atom () '(not cons))

(deftype extended-char ()
  "Type of characters that aren't base-char's.  None in CMU CL."
  '(and character (not base-char)))

(deftype standard-char ()
  "Type corresponding to the charaters required by the standard."
  '(member #\NEWLINE #\SPACE #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\,
	   #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\=
	   #\> #\?  #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	   #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\]
	   #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	   #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{
	   #\| #\} #\~))

(deftype keyword ()
  "Type for any keyword symbol."
  '(and symbol (satisfies keywordp)))

(deftype eql (n) `(member ,n))


;;;; Some types that we use in defining the standard functions:
;;;

;;;
;;; A type specifier.  INSTANCE stands for either KERNEL::CLASS
;;; or LISP:CLASS (CLOS class), but since LISP:CLASS isn't a type
;;; until PCL is loaded, that's not expressable.
;;;
(deftype type-specifier () '(or list symbol instance))
;;;
;;; An index into an array.   Also used for sequence index. 
(deftype index () `(integer 0 (,array-dimension-limit)))
;;;
;;; Array rank, total size...
(deftype array-rank () `(integer 0 (,array-rank-limit)))
(deftype array-total-size () `(integer 0 (,array-total-size-limit)))
;;;
;;; Some thing legal in an evaluated context.
(deftype form () t)
;;;
;;; Maclisp compatibility...
(deftype stringlike () '(or string symbol))
(deftype stringable () '(or string symbol character))
;;;
;;; Save a little typing...
(deftype truth () '(member t))
;;;
;;; A thing legal in places where we want the name of a file.
(deftype filename () '(or string pathname))
;;;
;;; A legal arg to pathname functions.
(deftype pathnamelike () '(or string pathname stream))
;;;
;;; A thing returned by the irrational functions.  We assume that they never
;;; compute a rational result.
(deftype irrational () '(or float (complex float)))
;;;
;;; Character components:
(deftype char-code () `(integer 0 (,char-code-limit)))
;;;
;;; A consed sequence result.  If a vector, is a simple array.
(deftype consed-sequence () '(or list (simple-array * (*))))
;;;
;;; The :end arg to a sequence...
(deftype sequence-end () '(or null index))
;;;
;;; The :count arg to a sequence...
(deftype sequence-count () '(or null integer))
;;;
;;; A valid argument to a stream function...
(deftype streamlike () '(or stream (member nil t)))
;;;
;;; A thing that can be passed to funcall & friends.
(deftype callable () '(or function symbol))

;;; Until we decide if and how to wedge this into the type system, make it
;;; equivalent to t.
;;;
(deftype void () t)


;;;; Cold loading initializations.

(emit-cold-load-defuns "TYPE")
