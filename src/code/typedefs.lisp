;;; -*- Package: KERNEL; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/typedefs.lisp,v 1.14 2006/06/30 18:41:22 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the definition of the CTYPE (Compiler TYPE) structure
;;; and related macros used for manipulating it.  This is sort of a mini object
;;; system with rather odd dispatching rules.  Other compile-time definitions
;;; needed by multiple files are also here.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "KERNEL")
(export '(ctype typedef-init))

;;; These are the Common Lisp defined type specifier symbols.  These are the
;;; things which can be used as declarations without requiring the use of TYPE.
(defconstant type-specifier-symbols
  '(array atom bignum bit bit-vector character compiled-function
    complex cons double-float extended-char fixnum float function
    hash-table integer keyword list long-float nil null number package
    pathname random-state ratio rational real readtable sequence
    short-float simple-array simple-bit-vector simple-string simple-vector
    single-float standard-char stream string base-char symbol t vector
    #+double-double double-double-float))


;;; Def-Type-Translator  --  Interface
;;;
;;;    Define the translation from a type-specifier to a type structure for
;;; some particular type.  Syntax is identical to DEFTYPE.
;;;
(defmacro def-type-translator (name arglist &body body)
  (check-type name symbol)
  (let ((whole (gensym)))
    (multiple-value-bind
	(body local-decs)
	(lisp::parse-defmacro arglist whole body name 'def-type-translator
			      :default-default ''*)
      `(progn
	 (cold-load-init
	   (setf (info type translator ',name)
		 #'(lambda (,whole) ,@local-decs (block ,name ,body))))
	 ',name))))


;;; Defvars for these come later, after we have enough stuff defined.
(declaim (special *wild-type* *universal-type* *empty-type*))


;;;; Cold load hack magic.

(eval-when (compile load eval)

(defparameter cold-type-init-forms nil
  "Forms that must happen before top level forms are run.")

(defmacro with-cold-load-init-forms ()
  '(eval-when (compile eval)
     (setq cold-type-init-forms nil)))

(defmacro cold-load-init (&rest forms)
  (if (and (consp forms) (consp (car forms)) (eq (caar forms) 'eval-when))
      (let ((when (cadar forms))
	    (eval-when-forms (cddar forms)))
	(unless (= (length forms) 1)
	  (warn "Can't cold-load-init other forms along with an eval-when."))
	(when (member 'load when)
	  (setf cold-type-init-forms
		(nconc cold-type-init-forms (copy-list eval-when-forms))))
	`(eval-when ,(remove 'load when)
	   ,@eval-when-forms))
      (progn
	(setf cold-type-init-forms
	      (nconc cold-type-init-forms (copy-list forms)))
	nil)))

(defmacro emit-cold-load-defuns (prefix)
  (let ((index 0))
    (collect ((defuns)
	      (calls))
      (loop
	(unless cold-type-init-forms (return))
	(let ((num-forms (min 10 (length cold-type-init-forms)))
	      (name (intern (format nil "~A-INIT-~D" prefix (incf index)))))
	  (defuns `(defun ,name ()
		     ,@(subseq cold-type-init-forms 0 num-forms)))
	  (setf cold-type-init-forms (nthcdr num-forms cold-type-init-forms))
	  (calls (list name))))
      `(progn
	 ,@(defuns)
	 (defun ,(intern (format nil "~A-INIT" prefix)) ()
	   ,@(calls)
	   nil)))))

); eval-when

;; Use this definition if you are trying to use this interactivly.
#+nil
(defmacro cold-load-init (&rest forms)
  `(progn ,@forms))


;;;; Type classes:
;;;
;;;    The TYPE-CLASS structure represents the "kind" of a type.  It mainly
;;; contains functions which are methods on that kind of type, but is also use
;;; in EQ comparisons to determined if two types have the "same kind".

(defvar *type-classes*)
(cold-load-init
  (unless (boundp '*type-classes*)
    (setq *type-classes* (make-hash-table :test #'eq))))

;;; TYPE-CLASS-OR-LOSE  --  Interface
;;;
(defun type-class-or-lose (name)
  (or (gethash name *type-classes*)
      (error "~S is not a defined type class." name)))

;;; MUST-SUPPLY-THIS  --  Interface
;;;
(defun must-supply-this (&rest foo)
  (error "Missing type method for ~S" foo))


(defstruct (type-class
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<TYPE-CLASS ~S>" (type-class-name s)))))
			     
  ;;
  ;; Name of this type class, used to resolve references at load time.
  (name nil :type symbol)
  ;;
  ;; Dyadic type methods.  If the classes of the two types are EQ, then we call
  ;; the SIMPLE-xxx method.  If the classes are not EQ, and either type's class
  ;; has a COMPLEX-xxx method, then we call it.
  ;;
  ;; Although it is undefined which method will get precedence when both types
  ;; have a complex method, the complex method can assume that the second arg
  ;; always is in its class, and the first always is not.  The arguments to
  ;; commutative operations will be swapped if the first argument has a complex
  ;; method.
  ;;
  ;; Since SUBTYPEP is not commutative, we have two complex methods.  the ARG1
  ;; method is only called when the first argument is in its class, and the
  ;; ARG2 method is only called when called when the second type is.  If either
  ;; is specified, both must be.
  (simple-subtypep #'must-supply-this :type function)
  (complex-subtypep-arg1 nil :type (or function null))
  (complex-subtypep-arg2 nil :type (or function null))
  ;;
  ;; SIMPLE-UNION combines two types of the same class into a single type of
  ;; that class.  If the result is a two-type union, then return NIL.
  ;; VANILLA-UNION returns whichever argument is a supertype of the other, or
  ;; NIL.
  (simple-union #'hierarchical-union2 :type function)
  (complex-union nil :type (or function null))
  ;;
  ;; The default intersection methods assume that if one type is a subtype of
  ;; the other, then that type is the intersection.
  (simple-intersection #'hierarchical-intersection2 :type function)
  (complex-intersection nil :type (or function null))
  ;;
  (simple-= #'must-supply-this :type function)
  (complex-= nil :type (or function null))
  ;;
  ;; Function which returns a Common Lisp type specifier representing this
  ;; type.
  (unparse #'must-supply-this :type function)
  
  #|
  Not used, and not really right.  Probably we want a TYPE= alist for the
  unary operations, since there are lots of interesting unary predicates that
  aren't equivalent to an entire class
  ;;
  ;; Names of functions used for testing the type of objects in this type
  ;; class.  UNARY-PREDICATE takes just the object, whereas PREDICATE gets
  ;; passed both the object and the CTYPE.  Normally one or the other will be
  ;; supplied for any type that can be passed to TYPEP; there is no point in
  ;; supplying both.
  (unary-typep nil :type (or symbol null))
  (typep nil :type (or symbol null))
  ;;
  ;; Like TYPEP, UNARY-TYPEP except these functions coerce objects to this
  ;; type.
  (unary-coerce nil :type (or symbol null))
  (coerce :type (or symbol null))
  |#
  )


(eval-when (compile load eval)

(defconstant type-class-function-slots
  '((:simple-subtypep . type-class-simple-subtypep)
    (:complex-subtypep-arg1 . type-class-complex-subtypep-arg1)
    (:complex-subtypep-arg2 . type-class-complex-subtypep-arg2)
    (:simple-union . type-class-simple-union)
    (:complex-union . type-class-complex-union)
    (:simple-intersection . type-class-simple-intersection)
    (:complex-intersection . type-class-complex-intersection)
    (:simple-= . type-class-simple-=)
    (:complex-= . type-class-complex-=)
    (:unparse . type-class-unparse)))

(defconstant type-class-symbol-slots
  '((:unary-typep . type-class-unary-typep)
    (:typep . type-class-typep)
    (:unary-coerce . type-class-unary-coerce)
    (:coerce . type-class-coerce)))
  

;;; CLASS-FUNCTION-SLOT-OR-LOSE  --  Interface
;;;
(defun class-function-slot-or-lose (name)
  (or (cdr (assoc name type-class-function-slots))
      (error "~S is not a defined type class method." name)))

); Eval-When (Compile Load Eval)


;;; DEFINE-TYPE-METHOD  --  Interface
;;;
(defmacro define-type-method ((class method &rest more-methods)
			      lambda-list &body body)
  "DEFINE-TYPE-METHOD (Class-Name Method-Name+) Lambda-List Form*"
  (let ((name (symbolicate CLASS "-" method "-TYPE-METHOD")))
    `(progn
       (defun ,name ,lambda-list ,@body)
       (cold-load-init
	 ,@(mapcar #'(lambda (method)
		       `(setf (,(class-function-slot-or-lose method)
			       (type-class-or-lose ',class))
			      #',name))
		   (cons method more-methods)))
       (undefined-value))))


;;; DEFINE-TYPE-CLASS  --  Interface
;;;
(defmacro define-type-class (name &optional inherits)
  "DEFINE-TYPE-CLASS Name [Inherits]"
  `(cold-load-init
     ,(once-only ((n-class (if inherits
			       `(copy-type-class (type-class-or-lose ',inherits))
			       '(make-type-class))))
	`(progn
	   (setf (type-class-name ,n-class) ',name)
	   (setf (gethash ',name *type-classes*) ,n-class)
	   (undefined-value)))))


;;; INVOKE-TYPE-METHOD  --  Interface
;;;
;;;    Invoke a type method on TYPE1 and TYPE2.  If the two types have the same
;;; class, invoke the simple method.  Otherwise, invoke any complex method.  If
;;; there isn't a distinct complex-arg1 method, then swap the arguments when
;;; calling type1's method.  If no applicable method, return DEFAULT.
;;;
(defmacro invoke-type-method (simple complex-arg2 type1 type2 &key
				     (default '(values nil t))
				     complex-arg1)
  (let ((simple (class-function-slot-or-lose simple))
	(cslot1 (class-function-slot-or-lose (or complex-arg1 complex-arg2)))
	(cslot2 (class-function-slot-or-lose complex-arg2)))
    (once-only ((n-type1 type1)
		(n-type2 type2))
      (once-only ((class1 `(type-class-info ,n-type1))
		  (class2 `(type-class-info ,n-type2)))
	`(if (eq ,class1 ,class2)
	     (funcall (,simple ,class1) ,n-type1 ,n-type2)
	     ,(once-only ((complex1 `(,cslot1 ,class1))
			  (complex2 `(,cslot2 ,class2)))
		`(cond (,complex2 (funcall ,complex2 ,n-type1 ,n-type2))
		       (,complex1
			,(if complex-arg1
			     `(funcall ,complex1 ,n-type1 ,n-type2)
			     `(funcall ,complex1 ,n-type2 ,n-type1)))
		       (t ,default))))))))


;;; The XXX-Type structures include the CTYPE structure for some slots that
;;; apply to all types.
;;;
(defstruct (ctype (:conc-name type-)
		  (:constructor make-type)
		  (:make-load-form-fun make-type-load-form)
		  (:pure t))
  ;;
  ;; The class of this type.
  (class-info (required-argument) :type type-class)
  ;;
  ;; True if this type has a fixed number of members, and as such could
  ;; possibly be completely specified in a MEMBER type.  This is used by the
  ;; MEMBER type methods.
  (enumerable nil :type (member t nil) :read-only t))

;;; %Print-Type  --  Internal
;;;
;;;    The print-function for all type structures.
;;;
(defun %print-type (s stream d)
  (declare (ignore d))
  (format stream "#<~A ~S>" (type-of s) (type-specifier s)))

;;; Make-Type-Load-Form -- Internal
;;;
;;; Just dump it as a specifier, and convert it back upon loading.
;;;
(defun make-type-load-form (type)
  `(specifier-type ',(type-specifier type)))


;;;; Utilities:

;;; ANY-TYPE-OP, EVERY-TYPE-OP  --  Interface
;;;
;;;    Like ANY and EVERY, except that we handle two-arg uncertain predicates.
;;; If the result is uncertain, then we return Default from the block PUNT.
;;; If LIST-FIRST is true, then the list element is the first arg, otherwise
;;; the second.
;;;
(defmacro any-type-op (op thing list &key (default '(values nil nil))
			  list-first)
  (let ((n-this (gensym))
	(n-thing (gensym))
	(n-val (gensym))
	(n-win (gensym))
	(n-uncertain (gensym)))
    `(let ((,n-thing ,thing)
	   (,n-uncertain nil))
       (dolist (,n-this ,list
			(if ,n-uncertain
			    (return-from PUNT ,default)
			    nil))
	 (multiple-value-bind (,n-val ,n-win)
			      ,(if list-first
				   `(,op ,n-this ,n-thing)
				   `(,op ,n-thing ,n-this))
	   (unless ,n-win (setq ,n-uncertain t))
	   (when ,n-val (return t)))))))
;;;
(defmacro every-type-op (op thing list &key (default '(values nil nil))
			    list-first)
  (let ((n-this (gensym))
	(n-thing (gensym))
	(n-val (gensym))
	(n-win (gensym)))
    `(let ((,n-thing ,thing))
       (dolist (,n-this ,list t)
	 (multiple-value-bind (,n-val ,n-win)
			      ,(if list-first
				   `(,op ,n-this ,n-thing)
				   `(,op ,n-thing ,n-this))
	   (unless ,n-win (return-from PUNT ,default))
	   (unless ,n-val (return nil)))))))


  
;;; VANILLA-INTERSECTION  --  Interface
;;;
;;;    Compute the intersection for types that intersect only when one is a
;;; hierarchical subtype of the other.
;;;
(defun vanilla-intersection (type1 type2)
  (multiple-value-bind (stp1 win1)
		       (csubtypep type1 type2)
    (multiple-value-bind (stp2 win2)
			 (csubtypep type2 type1)
      (cond (stp1 (values type1 t))
	    (stp2 (values type2 t))
	    ((and win1 win2) (values *empty-type* t))
	    (t
	     (values type1 nil))))))


;;; VANILLA-UNION  --  Interface
;;;
(defun vanilla-union (type1 type2)
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t nil)))

(defun hierarchical-intersection2 (type1 type2)
  (multiple-value-bind (subtypep1 win1) (csubtypep type1 type2)
    (multiple-value-bind (subtypep2 win2) (csubtypep type2 type1)
      (cond (subtypep1 type1)
	    (subtypep2 type2)
	    ((and win1 win2) *empty-type*)
	    (t nil)))))

(defun hierarchical-union2 (type1 type2)
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t nil)))

;;; TYPE-CACHE-HASH  --  Interface
;;;
;;;    EQ hash two things (types) down to 8 bits.
;;;
(defmacro type-cache-hash (type1 type2)
  `(the fixnum
	(logand (the fixnum
		     (logxor (the fixnum
				  (ash (cache-hash-eq ,type1) -3))
			     (the fixnum (cache-hash-eq ,type2))))
		#xFF)))


;;;; Cold loading initializations.

(emit-cold-load-defuns "TYPEDEF")

