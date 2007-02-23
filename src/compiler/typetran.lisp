;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/typetran.lisp,v 1.45 2005/02/07 17:27:16 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains stuff that implements the portable IR1 semantics of
;;; type tests.  The main thing we do is convert complex type tests into
;;; simpler code that can be compiled inline.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")


;;;; Type predicate translation:
;;;
;;;    We maintain a bidirectional association between type predicates and the
;;; tested type.  The presence of a predicate in this association implies that
;;; it is desirable to implement tests of this type using the predicate.  This
;;; is true both of very simple types.  These are either predicates that the
;;; back end is likely to have special knowledge about, or predicates so
;;; complex that the only reasonable implentation is via function call.
;;;
;;;    Some standard types (such as SEQUENCE) are best tested by letting the
;;; TYPEP source transform do its thing with the expansion.  These types (and
;;; corresponding predicates) are not maintained in this association.  In this
;;; case, there need not be any predicate function unless it is required by
;;; Common Lisp.
;;;
;;;    The mappings between predicates and type structures is stored in the
;;; backend structure, so that different backends can support different sets
;;; of predicates.
;;;

;;; Define-Type-Predicate  --  Interface
;;;
(defmacro define-type-predicate (name type)
  "Define-Type-Predicate Name Type
  Establish an association between the type predicate Name and the
  corresponding Type.  This causes the type predicate to be recognized for
  purposes of optimization."
  `(%define-type-predicate ',name ',type))
;;;
(defun %define-type-predicate (name specifier)
  (let ((type (specifier-type specifier)))
    (setf (gethash name (backend-predicate-types *target-backend*)) type)
    (setf (backend-type-predicates *target-backend*)
	  (cons (cons type name)
		(remove name (backend-type-predicates *target-backend*)
			:key #'cdr)))
    (%deftransform name '(function (t) *) #'fold-type-predicate)
    name))


;;;; IR1 transforms:

;;; Typep IR1 transform  --  Internal
;;;
;;;    If we discover the type argument is constant during IR1 optimization,
;;; then give the source transform another chance.  The source transform can't
;;; pass, since we give it an explicit constant.  At worst, it will convert to
;;; %Typep, which will prevent spurious attempts at transformation (and
;;; possible repeated warnings.) 
;;;
(deftransform typep ((object type))
  (unless (constant-continuation-p type)
    (give-up "Can't open-code test of non-constant type."))
  `(typep object ',(continuation-value type)))


;;; IR1-Transform-Type-Predicate  --  Internal
;;;
;;;    If the continuation Object definitely is or isn't of the specified type,
;;; then return T or NIL as appropriate.  Otherwise quietly Give-Up.
;;;
(defun ir1-transform-type-predicate (object type)
  (declare (type continuation object) (type ctype type))
  (let ((otype (continuation-type object)))
    (cond ((not (types-intersect otype type)) nil)
	  ((csubtypep otype type) t)
	  ((eq type *empty-type*) nil)
	  (t (give-up)))))


;;; %Typep IR1 transform  --  Internal
;;;
;;;    Flush %Typep tests whose result is known at compile time.
;;;
(deftransform %typep ((object type))
  (unless (constant-continuation-p type) (give-up))
  (ir1-transform-type-predicate
   object
   (specifier-type (continuation-value type))))

;;; Fold-Type-Predicate IR1 transform  --  Internal
;;;
;;;    This is the IR1 transform for simple type predicates.  It checks whether
;;; the single argument is known to (not) be of the appropriate type, expanding
;;; to T or NIL as apprporiate.
;;;
(deftransform fold-type-predicate ((object) * * :node node :defun-only t)
  (let ((ctype (gethash (leaf-name
			 (ref-leaf
			  (continuation-use
			   (basic-combination-fun node))))
			(backend-predicate-types *backend*))))
    (assert ctype)
    (ir1-transform-type-predicate object ctype)))


;;; FIND-CLASS IR1 Transform  --  Internal
;;;
;;;    If FIND-CLASS is called on a constant class, locate the CLASS-CELL at
;;; load time.
;;; 
(deftransform kernel::find-class ((name) ((constant-argument symbol)) *
				  :when :both)
  (let* ((name (continuation-value name))
	 (cell (find-class-cell name)))
    `(or (class-cell-class ',cell)
	 (error "Class not yet defined: ~S" ',name))))

;;;; Standard type predicates:

(defun define-standard-type-predicates ()
  (define-type-predicate arrayp array)
  ; No atom.  Use (not cons) deftype.
  (define-type-predicate bit-vector-p bit-vector)
  (define-type-predicate characterp character)
  (define-type-predicate compiled-function-p compiled-function)
  (define-type-predicate complexp complex)
  (define-type-predicate complex-rational-p (complex rational))
  (define-type-predicate complex-float-p (complex float))
  (define-type-predicate consp cons)
  (define-type-predicate floatp float)
  (define-type-predicate functionp function)
  (define-type-predicate integerp integer)
  (define-type-predicate keywordp keyword)
  (define-type-predicate listp list)
  (define-type-predicate null null)
  (define-type-predicate numberp number)
  (define-type-predicate rationalp rational)
  (define-type-predicate realp real)
  (define-type-predicate simple-bit-vector-p simple-bit-vector)
  (define-type-predicate simple-string-p simple-string)
  (define-type-predicate simple-vector-p simple-vector)
  (define-type-predicate stringp string)
  (define-type-predicate %instancep instance)
  (define-type-predicate funcallable-instance-p funcallable-instance)
  (define-type-predicate symbolp symbol)
  (define-type-predicate vectorp vector))

(define-standard-type-predicates)



;;;; Transforms for type predicates not implemented primitively:
;;;
;;; See also VM dependent transforms.

(def-source-transform atom (x)
  `(not (consp ,x)))


;;;; Typep source transform:

;;; Transform-Numeric-Bound-Test  --  Internal
;;;
;;;    Return a form that tests the variable N-Object for being in the binds
;;; specified by Type.  Base is the name of the base type, for declaration.  We
;;; make safety locally 0 to inhibit any checking of this assertion.
;;;
(defun transform-numeric-bound-test (n-object type base)
  (declare (type numeric-type type))
  (let ((low (numeric-type-low type))
	(high (numeric-type-high type)))
    `(locally
       (declare (optimize (safety 0)))
       (and ,@(when low
		(if (consp low)
		    `((> (the ,base ,n-object) ,(car low)))
		    `((>= (the ,base ,n-object) ,low))))
	    ,@(when high
		(if (consp high)
		    `((< (the ,base ,n-object) ,(car high)))
		    `((<= (the ,base ,n-object) ,high))))))))

;;; Source-Transform-Numeric-Typep  --  Internal
;;;
;;;    Do source transformation of a test of a known numeric type.  We can
;;; assume that the type doesn't have a corresponding predicate, since those
;;; types have already been picked off.  In particular, Class must be
;;; specified, since it is unspecified only in NUMBER and COMPLEX.  Similarly,
;;; we assume that Complexp is always specified.
;;;
;;;    For non-complex types, we just test that the number belongs to the base
;;; type, and then test that it is in bounds.  When Class is Integer, we check
;;; to see if the range is no bigger than FIXNUM.  If so, we check for FIXNUM
;;; instead of INTEGER.  This allows us to use fixnum comparison to test the
;;; bounds.
;;;
;;;    For complex types, we must test for complex, then do the above on both
;;; the real and imaginary parts.  When Class is float, we need only check the
;;; type of the realpart, since the format of the realpart and the imagpart
;;; must be the same.
;;;
(defun source-transform-numeric-typep (object type)
  (let* ((class (numeric-type-class type))
	 (base (ecase class
		 (integer (containing-integer-type type))
		 (rational 'rational)
		 (float (or (numeric-type-format type) 'float))
		 ((nil) 'real))))
    (once-only ((n-object object))
      (ecase (numeric-type-complexp type)
	(:real
	 `(and (typep ,n-object ',base)
	       ,(transform-numeric-bound-test n-object type base)))
	(:complex
	 `(and (complexp ,n-object)
	       ,(once-only ((n-real `(realpart (the complex ,n-object)))
			    (n-imag `(imagpart (the complex ,n-object))))
		  `(progn
		     ,n-imag ; ignorable
		     (and (typep ,n-real ',base)
			  ,@(when (eq class 'integer)
			      `((typep ,n-imag ',base)))
			  ,(transform-numeric-bound-test n-real type base)
			  ,(transform-numeric-bound-test n-imag type
							 base))))))))))


;;; Source-Transform-Hairy-Typep  --  Internal
;;;
;;;    Do the source transformation for a test of a hairy type.  AND, SATISFIES
;;; and NOT are converted into the obvious code.  We convert unknown types to
;;; %TYPEP, emitting an efficiency note if appropriate.
;;;
(defun source-transform-hairy-typep (object type)
  (declare (type hairy-type type))
  (let ((spec (hairy-type-specifier type)))
    (cond ((unknown-type-p type)
	   (when (policy nil (> speed brevity))
	     (compiler-note "Can't open-code test of unknown type ~S."
			    (type-specifier type)))
	   `(%typep ,object ',spec))
	  (t
	   (ecase (first spec)
	     (satisfies `(if (funcall #',(second spec) ,object) t nil))
	     ((not and)
	      (once-only ((n-obj object))
		`(,(first spec) ,@(mapcar #'(lambda (x) 
					      `(typep ,n-obj ',x))
					  (rest spec))))))))))

(defun source-transform-negation-typep (object type)
  (declare (type negation-type type))
  (let ((spec (type-specifier (negation-type-type type))))
    `(not (typep ,object ',spec))))

;;; Source-Transform-Union-Typep  --  Internal
;;;
;;;    Do source transformation for Typep of a known union type.  If a union
;;; type contains LIST, then we pull that out and make it into a single LISTP
;;; call.  Note that if SYMBOL is in the union, then LIST will be a subtype
;;; even without there being any (member NIL).  We just drop through to the
;;; general code in this case, rather than trying to optimize it.
;;;
(defun source-transform-union-typep (object type)
  (let* ((types (union-type-types type))
	 (cons-type (specifier-type 'cons))
	 (mtype (find-if #'member-type-p types))
	 (members (when mtype (member-type-members mtype))))
    (cond ((and mtype 
		(memq nil members)
		(memq cons-type types))
	   (once-only ((n-obj object))
	     `(or (listp ,n-obj)
		  (typep ,n-obj 
			 '(or ,@(mapcar #'type-specifier
				        (remove cons-type
					        (remove mtype types)))
			   (member ,@(remove nil members)))))))
	  (t
	   (once-only ((n-obj object))
	     `(or ,@(mapcar #'(lambda (x)
				`(typep ,n-obj ',(type-specifier x)))
			    types)))))))

(defun source-transform-intersection-typep (object type)
  (once-only ((n-obj object))
    `(and ,@(mapcar (lambda (x)
		      `(typep ,n-obj ',(type-specifier x)))
		    (intersection-type-types type)))))


;;; Source-Transform-Cons-Typep  --  Internal
;;;
;;; If necessary recurse to check the cons type.
;;;
(defun source-transform-cons-typep (object type)
  (let* ((car-type (cons-type-car-type type))
	 (cdr-type (cons-type-cdr-type type)))
    (let ((car-test-p (not (or (type= car-type *wild-type*)
			       (type= car-type (specifier-type t)))))
	  (cdr-test-p (not (or (type= cdr-type *wild-type*)
			       (type= cdr-type (specifier-type t))))))
      (if (and (not car-test-p) (not cdr-test-p))
	  `(consp ,object)
	  (once-only ((n-obj object))
	    `(and (consp ,n-obj)
		  ,@(if car-test-p
			`((typep (car ,n-obj)
				 ',(type-specifier car-type))))
		  ,@(if cdr-test-p
			`((typep (cdr ,n-obj)
				 ',(type-specifier cdr-type))))))))))


;;; FIND-SUPERTYPE-PREDICATE  --  Internal
;;;
;;;    Return the predicate and type from the most specific entry in
;;; *TYPE-PREDICATES* that is a supertype of Type.
;;;
(defun find-supertype-predicate (type)
  (declare (type ctype type))
  (let ((res nil)
	(res-type nil))
    (dolist (x (backend-type-predicates *backend*))
      (let ((stype (car x)))
	(when (and (csubtypep type stype)
		   (or (not res-type)
		       (csubtypep stype res-type)))
	  (setq res-type stype)
	  (setq res (cdr x)))))
    (values res res-type)))


;;; TEST-ARRAY-DIMENSIONS  --  Internal
;;;
;;;    Return forms to test that Obj has the rank and dimensions specified by
;;; Type, where Stype is the type we have checked against (which is the same
;;; but for dimensions.)
;;;
(defun test-array-dimensions (obj type stype)
  (declare (type array-type type stype))
  (let ((obj `(truly-the ,(type-specifier stype) ,obj))
	(dims (array-type-dimensions type)))
    (unless (eq dims '*)
      (collect ((res))
	(when (eq (array-type-dimensions stype) '*)
	  (res `(= (array-rank ,obj) ,(length dims))))

	(do ((i 0 (1+ i))
	     (dim dims (cdr dim)))
	    ((null dim))
	  (let ((dim (car dim)))
	    (unless (eq dim '*)
	      (res `(= (array-dimension ,obj ,i) ,dim)))))
	(res)))))


;;; SOURCE-TRANSFORM-ARRAY-TYPEP  --  Internal
;;;
;;;    If we can find a type predicate that tests for the type w/o dimensions,
;;; then use that predicate and test for dimensions.  Otherwise, just do
;;; %TYPEP.
;;;
(defun source-transform-array-typep (obj type)
  (multiple-value-bind (pred stype) (find-supertype-predicate type)
    (if (and (array-type-p stype)
	     ;; (If the element type hasn't been defined yet, it's
	     ;; not safe to assume here that it will eventually
	     ;; have (UPGRADED-ARRAY-ELEMENT-TYPE type)=T, so punt.)
	     (not (unknown-type-p (array-type-element-type type)))
	     (type= (array-type-specialized-element-type stype)
		    (array-type-specialized-element-type type))
	     (eq (array-type-complexp stype) (array-type-complexp type)))
	(once-only ((n-obj obj))
	  `(and (,pred ,n-obj)
		,@(test-array-dimensions n-obj type stype)))
	`(%typep ,obj ',(type-specifier type)))))

;;; Instance typep IR1 transform  --  Internal
;;;
;;;    Transform a type test against some instance type. The type test is
;;; flushed if the result is known at compile time.  If not properly named,
;;; error.  If sealed and has no subclasses, just test for layout-EQ.  If a
;;; structure then test for layout-EQ and then a general test based on
;;; layout-inherits. If safety is important, then we also check if the layout
;;; for the object is invalid and signal an error if so.  Otherwise, look up
;;; the indirect class-cell and call CLASS-CELL-TYPEP at runtime.
;;;
(deftransform %instance-typep ((object spec) (* *) * :node node :when :both)
  (assert (constant-continuation-p spec))
  (let* ((spec (continuation-value spec))
	 (class (specifier-type spec))
	 (name (%class-name class))
	 (otype (continuation-type object))
	 (layout (let ((res (info type compiler-layout name)))
		   (if (and res (member (layout-invalid res) '(:compiler nil)))
		       res
		       nil))))
    (cond
      ;; Flush tests whose result is known at compile time.
      ((not (types-intersect otype class)) 'nil)
      ((csubtypep otype class) 't)
      ;; If not properly named, error.
      ((not (and name (eq (kernel::find-class name) class)))
       (compiler-error "Can't compile TYPEP of anonymous or undefined ~
			class:~%  ~S"
		       class))
      (t
       ;; Delay the type transform to give type propagation a chance.
       (delay-transform node :constraint)
       ;; Otherwise transform the type test.
       (multiple-value-bind
	     (pred get-layout)
	   (cond
	     ((csubtypep class (specifier-type 'funcallable-instance))
	      (values 'funcallable-instance-p '%funcallable-instance-layout))
	     ((csubtypep class (specifier-type 'instance))
	      (values '%instancep '%instance-layout))
	     (t
	      (values '(lambda (x) (declare (ignore x)) t) 'layout-of)))
	 (cond
	   ((and (eq (%class-state class) :sealed) layout
		 (not (%class-subclasses class)))
	    ;; Sealed and has no subclasses.
	    (let ((n-layout (gensym)))
	      `(and (,pred object)
		    (let ((,n-layout (,get-layout object)))
		      ,@(when (policy nil (>= safety speed))
			      `((when (layout-invalid ,n-layout)
				  (%layout-invalid-error object ',layout))))
		      (eq ,n-layout ',layout)))))
	   ((and (typep class 'basic-structure-class) layout)
	    ;; Structure type tests; hierarchical layout depths.
	    (let ((idepth (layout-inheritance-depth layout))
		  (n-layout (gensym)))
	      `(and (,pred object)
		    (let ((,n-layout (,get-layout object)))
		      ,@(when (policy nil (>= safety speed))
			      `((when (layout-invalid ,n-layout)
				  (%layout-invalid-error object ',layout))))
		      (if (eq ,n-layout ',layout)
			  t
			  (and (> (layout-inheritance-depth ,n-layout) ,idepth)
			       (locally (declare (optimize (safety 0)))
				 (eq (svref (layout-inherits ,n-layout) 
					    ,idepth)
				     ',layout))))))))
	   ((and layout (>= (layout-inheritance-depth layout) 0))
	    ;; Hierarchical layout depths.
	    (let ((idepth (layout-inheritance-depth layout))
		  (n-layout (gensym))
		  (n-inherits (gensym)))
	      `(and (,pred object)
		    (let ((,n-layout (,get-layout object)))
		      ,@(when (policy nil (>= safety speed))
			      `((when (layout-invalid ,n-layout)
				  (%layout-invalid-error object ',layout))))
		      (if (eq ,n-layout ',layout)
			  t
			  (let ((,n-inherits (layout-inherits ,n-layout)))
			    (declare (optimize (safety 0)))
			    (and (> (length ,n-inherits) ,idepth)
				 (eq (svref ,n-inherits ,idepth)
				     ',layout))))))))
	   (t
	    `(and (,pred object)
		  (class-cell-typep (,get-layout object)
				    ',(find-class-cell name)
				    object)))))))))


;;; Source-Transform-Typep  --  Internal
;;;
;;;    If the specifier argument is a quoted constant, then we consider
;;; converting into a simple predicate or other stuff.  If the type is
;;; constant, but we can't transform the call, then we convert to %Typep.  We
;;; only pass when the type is non-constant.  This allows us to recognize
;;; between calls that might later be transformed sucessfully when a constant
;;; type is discovered.  We don't give an efficiency note when we pass, since
;;; the IR1 transform will give one if necessary and appropriate.
;;;
;;; If the type is Type= to a type that has a predicate, then expand to that
;;; predicate.  Otherwise, we dispatch off of the type's type.  These
;;; transformations can increase space, but it is hard to tell when, so we
;;; ignore policy and always do them.  When byte-compiling, we only do
;;; transforms that have potential for control simplification. Instance type
;;; tests are converted to %instance-typep to allow type propagation.
;;;
(def-source-transform typep (object spec)
  (if (and (consp spec) (eq (car spec) 'quote))
      (let ((type (specifier-type (cadr spec))))
	(or (let ((pred (cdr (assoc type (backend-type-predicates *backend*)
				    :test #'type=))))
	      (when pred `(,pred ,object)))
	    (typecase type
	      (hairy-type
	       (source-transform-hairy-typep object type))
	      (negation-type
	       (source-transform-negation-typep object type))
	      (union-type
	       (source-transform-union-typep object type))
	      (intersection-type
	       (source-transform-intersection-typep object type))
	      (member-type
	       `(member ,object ',(member-type-members type)))
	      (args-type
	       (compiler-warning "Illegal type specifier for Typep: ~S."
				 (cadr spec))
	       `(%typep ,object ,spec))
	      (t nil))
	    (and (not (byte-compiling))
		 (typecase type
		   (numeric-type
		    (source-transform-numeric-typep object type))
		   (class
		    (let ((function (specifier-type 'function)))
		      (if (and (csubtypep type function)
			       (not (csubtypep function type)))
			`(%typep ,object ,spec)
			`(%instance-typep ,object ,spec))))
		   (array-type
		    (source-transform-array-typep object type))
		   (cons-type
		    (source-transform-cons-typep object type))
		   (t nil)))
	    `(%typep ,object ,spec)))
      (values nil t)))
