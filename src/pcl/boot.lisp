;;;-*-Mode: LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************

(file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/boot.lisp,v 1.73 2005/08/18 16:55:00 rtoy Exp $")

(in-package :pcl)

#|

The CommonLoops evaluator is meta-circular.  

Most of the code in PCL is methods on generic functions, including most of
the code that actually implements generic functions and method lookup.

So, we have a classic bootstrapping problem.   The solution to this is to
first get a cheap implementation of generic functions running, these are
called early generic functions.  These early generic functions and the
corresponding early methods and early method lookup are used to get enough
of the system running that it is possible to create real generic functions
and methods and implement real method lookup.  At that point (done in the
file FIXUP) the function fix-early-generic-functions is called to convert
all the early generic functions to real generic functions.

The cheap generic functions are built using the same funcallable-instance
objects real generic-functions are made out of.  This means that as PCL
is being bootstrapped, the cheap generic function objects which are being
created are the same objects which will later be real generic functions.
This is good because:
  - we don't cons garbage structure
  - we can keep pointers to the cheap generic function objects
    during booting because those pointers will still point to
    the right object after the generic functions are all fixed
    up



This file defines the defmethod macro and the mechanism used to expand it.
This includes the mechanism for processing the body of a method.  defmethod
basically expands into a call to load-defmethod, which basically calls
add-method to add the method to the generic-function.  These expansions can
be loaded either during bootstrapping or when PCL is fully up and running.

An important effect of this structure is it means we can compile files with
defmethod forms in them in a completely running PCL, but then load those files
back in during bootstrapping.  This makes development easier.  It also means
there is only one set of code for processing defmethod.  Bootstrapping works
by being sure to have load-method be careful to call only primitives which
work during bootstrapping.

|#

(declaim (notinline make-a-method
		    add-named-method		      
		    ensure-generic-function-using-class
		    add-method
		    remove-method
		    ))

(defvar *early-functions*
	'((make-a-method early-make-a-method
			 real-make-a-method)
	  (add-named-method early-add-named-method
			    real-add-named-method)))

;;;
;;; For each of the early functions, arrange to have it point to its early
;;; definition.  Do this in a way that makes sure that if we redefine one
;;; of the early definitions the redefinition will take effect.  This makes
;;; development easier.
;;;
;;; The function which generates the redirection closure is pulled out into
;;; a separate piece of code because of a bug in ExCL which causes this not
;;; to work if it is inlined.
;;;
#-loadable-pcl
(eval-when (:load-toplevel :execute)

  (defun redirect-early-function-internal (real early)
    (setf (gdefinition real)
	  (set-function-name
	   (lambda (&rest args)
	     (apply (the function (symbol-function early)) args))
	   real)))

  (dolist (fns *early-functions*)
    (let ((name (car fns))
	  (early-name (cadr fns)))
      (redirect-early-function-internal name early-name)))
)


;;;
;;; *generic-function-fixups* is used by fix-early-generic-functions to
;;; convert the few functions in the bootstrap which are supposed to be
;;; generic functions but can't be early on.
;;; 
(defvar *generic-function-fixups*
    '((add-method
	((generic-function method)		        ;lambda-list
	 (standard-generic-function method)	        ;specializers
	 real-add-method))			        ;method-function
      (remove-method
	((generic-function method)
	 (standard-generic-function method)
	 real-remove-method))
      (get-method
        ((generic-function qualifiers specializers &optional (errorp t))
	 (standard-generic-function t t)
	 real-get-method))
      (ensure-generic-function-using-class
	((generic-function function-specifier
			   &key generic-function-class environment
			   &allow-other-keys)
	 (generic-function t)
	 real-ensure-gf-using-class--generic-function)
        ((generic-function function-specifier
			   &key generic-function-class environment
			   &allow-other-keys)
	 (null t)
	 real-ensure-gf-using-class--null))
      (make-method-lambda
       ((proto-generic-function proto-method lambda-expression environment)
	(standard-generic-function standard-method t t)
	real-make-method-lambda))
      (make-method-initargs-form
       ((proto-generic-function proto-method lambda-expression lambda-list environment)
	(standard-generic-function standard-method t t t)
	real-make-method-initargs-form))
      (compute-effective-method
       ((generic-function combin applicable-methods)
	(generic-function standard-method-combination t)
	standard-compute-effective-method))
      ))


;;;
;;;
;;;
(defun tell-compiler-about-gf (function-specifier lambda-list)
  ;; Supress any undefined function warnings from compiler.
  ;; I had originally lifted some code from c::%%defun but this
  ;; seems to do the job just as well.
  (proclaim-defgeneric function-specifier lambda-list))

;;;
;;; ANSI 3.4.2, Generic Function Lambda Lists
;;;

(defun parse-generic-function-lambda-list (lambda-list)
  ;; This is like PARSE-LAMBDA-LIST, but returns an additional
  ;; value AUXP which is true if LAMBDA-LIST contains any &aux keyword.
  (multiple-value-bind (required optional restp rest keyp keys
				 allow-other-keys-p aux)
      (parse-lambda-list lambda-list)
    (values required optional restp rest keyp keys allow-other-keys-p
	    (or aux (member '&aux lambda-list :test #'eq)) aux)))

(defun check-generic-function-lambda-list (function-specifier lambda-list)
  (multiple-value-bind (required optional restp rest keyp keys
				 allow-other-keys-p auxp aux)
      (parse-generic-function-lambda-list lambda-list)
    (declare (ignore restp rest keyp aux allow-other-keys-p))
    (labels ((lambda-list-error (format-control &rest format-arguments)
	       (simple-program-error
		(format nil "~~@<Generic function ~a: ~?.~~@:>"
			function-specifier
			format-control format-arguments)))
	     (check-required-parameter (parameter)
	       (unless (symbolp parameter)
		 (lambda-list-error
		  "Invalid generic function parameter name ~a"
		  parameter)))
	     (check-key-or-optional-parameter (parameter)
	       (unless (or (symbolp parameter)
			   (and (consp parameter)
				(symbolp (car parameter))))
		 (lambda-list-error
		  "Invalid generic function parameter name ~a"
		  parameter))
	       (when (and (consp parameter)
			  (not (null (cdr parameter))))
		 (lambda-list-error
		  "Optional and key parameters of generic functions ~
                   may not have default values or supplied-p ~
                   parameters: ~<~s~>" parameter))))
      (when auxp
	(lambda-list-error
	 "~s is not allowed in generic function lambda lists" '&aux))
      (mapc #'check-required-parameter required)
      (mapc #'check-key-or-optional-parameter optional)
      (mapc #'check-key-or-optional-parameter keys))))

(defmacro defgeneric (function-specifier lambda-list &body options)
  (check-generic-function-lambda-list function-specifier lambda-list)
  (expand-defgeneric function-specifier lambda-list options))

(defun expand-defgeneric (function-specifier lambda-list options)
  (let ((initargs ())
	(methods ()))
    (labels ((loose (format-control &rest format-arguments)
	       (simple-program-error
		(format nil "~~@<Generic function ~~s: ~?.~~@:>"
			format-control format-arguments)
		function-specifier))
	     (duplicate-option (name)
	       (loose "The option ~s appears more than once" name))
	     (check-declaration (declaration-specifiers)
	       (loop for specifier in declaration-specifiers
		     when (and (consp specifier)
			       (member (car specifier)
				       '(special ftype function inline
					 notinline declaration)
				       :test #'eq)) do
		     (loose "Declaration specifier ~s is not allowed"
			    specifier)))
	     (check-argument-precedence-order (precedence)
	       (let ((required (parse-lambda-list lambda-list)))
		 (when (set-difference required precedence)
		   (loose "Argument precedence order must list all ~
                           required parameters and only those: ~s"
			  precedence))
		 (when (/= (length (remove-duplicates precedence))
			   (length precedence))
		   (loose "Duplicate parameter names in argument ~
                           precedence order: ~s"
			  precedence))))
	     (initarg (key &optional (new nil new-supplied-p))
	       (if new-supplied-p
		   (setf (getf initargs key) new)
		   (getf initargs key))))

      (when (and (symbolp function-specifier)
		 (special-operator-p function-specifier))
	(loose "Special operators cannot be made generic functions"))
      
      (dolist (option options)
	(case (car option)
	  (:argument-precedence-order
	   (when (initarg :argument-precedence-order)
	     (duplicate-option :argument-precedence-order))
	   (check-argument-precedence-order (cdr option))
	   (initarg :argument-precedence-order `',(cdr option)))
	  (declare
	   (check-declaration (cdr option))
	   (initarg :declarations
		    (append (cdr option) (initarg :declarations))))
	  (:documentation
	   (if (initarg :documentation)
	       (duplicate-option :documentation)
	       (initarg :documentation `',(cadr option))))
	  (:method-combination
	   (if (initarg :method-combination)
	       (duplicate-option :method-combination)
	       (initarg :method-combination `',(cdr option))))
	  (:generic-function-class
	   (if (initarg :generic-function-class)
	       (duplicate-option :generic-function-class)
	       (initarg :generic-function-class `',(cadr option))))
	  (:method-class
	   (if (initarg :method-class)
	       (duplicate-option :method-class)
	       (initarg :method-class `',(cadr option))))
	  (:method
	      (push `(push (defmethod ,function-specifier ,@(cdr option))
			   (plist-value #',function-specifier
					'method-description-methods))
		    methods))
	  (t		  ;unsuported things must get a 'program-error
	   (loose "Unsupported option ~s" option))))

	(let ((declarations (initarg :declarations)))
	  (when declarations (initarg :declarations `',declarations))))

      (tell-compiler-about-gf function-specifier lambda-list)

    `(progn 
       (proclaim-defgeneric ',function-specifier ',lambda-list)
       ,(make-top-level-form
	 `(defgeneric ,function-specifier)
	 '(:load-toplevel :execute)
	 `(load-defgeneric ',function-specifier ',lambda-list 
	   :definition-source (c::source-location)
	   ,@initargs))
       ,@methods
       `,(function ,function-specifier))))

(defun load-defgeneric (function-specifier lambda-list &rest initargs)
  ;;
  ;; Remove methods defined by a previous DEFGENERIC (CLHS 7.6.1).
  (when (and (fboundp function-specifier)
	     (generic-function-p (gdefinition function-specifier)))
    (loop with gf = (gdefinition function-specifier)
	  for method in (plist-value gf 'method-description-methods) do
	    (remove-method gf method)
	  finally
	    (setf (plist-value gf 'method-description-methods) nil)))
  ;;
  (apply #'ensure-generic-function
	 function-specifier
	 :lambda-list lambda-list
	 initargs))


;;;
;;;
;;;
(defmacro defmethod (&rest args &environment env)
  (multiple-value-bind (name qualifiers lambda-list body)
      (parse-defmethod args)
    (multiple-value-bind (proto-gf proto-method)
	(prototypes-for-make-method-lambda name)
      (expand-defmethod name proto-gf proto-method
			qualifiers lambda-list body env))))

(defun prototypes-for-make-method-lambda (name)
  (if (not (eq *boot-state* 'complete))	  
      (values nil nil)
      (let ((gf? (and (fboundp name)
		      (gdefinition name))))
	(if (or (null gf?)
		(not (generic-function-p gf?)))
	    (values (class-prototype (find-class 'standard-generic-function))
		    (class-prototype (find-class 'standard-method)))
	    (values gf?
		    (class-prototype (or (generic-function-method-class gf?)
					 (find-class 'standard-method))))))))

;;;
;;; takes a name which is either a generic function name or a list specifying
;;; a setf generic function (like: (SETF <generic-function-name>)).  Returns
;;; the prototype instance of the method-class for that generic function.
;;;
;;; If there is no generic function by that name, this returns the default
;;; value, the prototype instance of the class STANDARD-METHOD.  This default
;;; value is also returned if the spec names an ordinary function or even a
;;; macro.  In effect, this leaves the signalling of the appropriate error
;;; until load time.
;;;
;;; NOTE that during bootstrapping, this function is allowed to return NIL.
;;; 
(defun method-prototype-for-gf (name)      
  (let ((gf? (and (fboundp name)
		  (gdefinition name))))
    (cond ((neq *boot-state* 'complete) nil)
	  ((or (null gf?)
	       (not (generic-function-p gf?)))	        ;Someone else MIGHT
						        ;error at load time.
	   (class-prototype (find-class 'standard-method)))
	  (t
	    (class-prototype (or (generic-function-method-class gf?)
				 (find-class 'standard-method)))))))


(defvar *inline-access*)
(defvar *method-source-info*)

(defvar *inline-methods-in-emfs* t
  "If true, allow inlining of methods in effective methods.")

(defun expand-defmethod (name proto-gf proto-method qualifiers
			 lambda-list body env)
  (let ((*inline-access* ()))
    (multiple-value-bind (method-lambda unspecialized-lambda-list specializers)
	(add-method-declarations name qualifiers lambda-list body env)
      (let* ((auto-compile-p (auto-compile-p name qualifiers specializers))
	     (*method-source-info*
	      (when (and auto-compile-p
			 (or (null env)
			     (and (null (c::lexenv-functions env))
				  (null (c::lexenv-variables env)))))
		(list (copy-tree body) lambda-list))))
	(multiple-value-bind (method-function-lambda initargs)
	    (make-method-lambda proto-gf proto-method method-lambda env)
	  (when (and *inline-access*
		     auto-compile-p
		     env
		     (or (c::lexenv-functions env)
			 (c::lexenv-variables env)))
	    (setq *method-source-info* nil)
	    (warn "~@<Defining method ~s ~s ~s using inline slot access in a ~
                   non-null lexical environment means that it cannot be ~
                   automatically recompiled.~@:>"
		  name qualifiers lambda-list))
	  (let* ((method-name `(method ,name ,@qualifiers ,specializers))
		 (initargs-form (make-method-initargs-form 
				proto-gf proto-method
				method-function-lambda initargs env)))
	    (tell-compiler-about-gf name unspecialized-lambda-list)
	    `(progn
	       (proclaim-defgeneric ',name ',unspecialized-lambda-list)
	       ;;
	       ;; Set inlining information in the global enviroment
	       ;; for the fast function if there is an INLINE
	       ;; declaration for the method name, which is the name
	       ;; of the "slow" function.  FIXME: Maybe the function
	       ;; names should be METHOD and SLOW-METHOD.
	       ,@(let ((inline (and *inline-methods-in-emfs*
				    (info function inlinep method-name))))
		   (when inline
		     (let ((fast-name (cons 'fast-method (cdr method-name)))
			   (lambda (getf (cdr initargs-form) :fast-function)))
		       `((setf (info function inlinep ',fast-name)
			       ',inline
			       (info function inline-expansion ',fast-name)
			       ',lambda)))))
	       ;;
	       ;; This expands to a LOAD-DEFMETHOD with compiled or
	       ;; interpreted lambdas for the method functions.  The
	       ;; LOAD-DEFMETHOD will construct the method metaobject
	       ;; with initargs from INITARGS-FORM etc.  FIXME: We
	       ;; could as well produce DEFUNs here, now that we have
	       ;; generalized function names.
	       ,(make-defmethod-form name qualifiers specializers
				     unspecialized-lambda-list
				     (if proto-method
					 (class-name (class-of proto-method))
					 'standard-method)
				     initargs-form
				     (getf (getf initargs :plist)
					   :pv-table-symbol)))))))))

(defun interned-symbol-p (x)
  (and (symbolp x) (symbol-package x)))

(defun make-defmethod-form (name qualifiers specializers
			    unspecialized-lambda-list method-class-name
			    initargs-form &optional pv-table-symbol)
  (let (fn fn-lambda)
    (if (and (interned-symbol-p (if (consp name)
				    (and (eq (car name) 'setf) (cadr name))
				    name))
	     (every #'interned-symbol-p qualifiers)
	     (every (lambda (s)
		      (if (consp s)
			  (and (eq (car s) 'eql) 
			       (constantp (cadr s))
			       (let ((sv (eval (cadr s))))
				 (or (interned-symbol-p sv)
				     (integerp sv)
				     (and (characterp sv)
					  (standard-char-p sv)))))
			  (interned-symbol-p s)))
		    specializers)
	     (consp initargs-form)
	     (eq (car initargs-form) 'list*)
	     (memq (cadr initargs-form) '(:function :fast-function))
	     (consp (setq fn (caddr initargs-form)))
	     (eq (car fn) 'function)
	     (consp (setq fn-lambda (cadr fn)))
	     (eq (car fn-lambda) 'lambda))
	(let* ((specls (mapcar (lambda (specl)
				 (if (consp specl)
				     `(,(car specl) ,(eval (cadr specl)))
				     specl))
			       specializers))
	       (mname `(,(if (eq (cadr initargs-form) :function)
			     'method 'fast-method)
			,name ,@qualifiers ,specls)))
	  `(eval-when (:load-toplevel :execute)
	     (defun ,mname ,(cadr fn-lambda)
	       ,@(cddr fn-lambda))
	     ,(make-defmethod-form-internal 
	       name qualifiers `',specls
	       unspecialized-lambda-list method-class-name
	       `(list* ,(cadr initargs-form) #',mname ,@(cdddr initargs-form))
	       pv-table-symbol)))
	(make-top-level-form 
	 `(defmethod ,name ,@qualifiers ,specializers)
	 '(:load-toplevel :execute)
	 (make-defmethod-form-internal 
	  name qualifiers 
	  `(list ,@(mapcar (lambda (specializer)
			     (if (consp specializer)
				 ``(,',(car specializer) ,,(cadr specializer))
				 `',specializer))
		    specializers))
	  unspecialized-lambda-list method-class-name
	  initargs-form
	  pv-table-symbol)))))

(defun make-defmethod-form-internal (name qualifiers specializers-form
				     unspecialized-lambda-list
				     method-class-name initargs-form
				     &optional pv-table-symbol)
  `(load-defmethod
    ',method-class-name
    ',name
    ',qualifiers
    ,specializers-form
    ',unspecialized-lambda-list
    (list* :definition-source (c::source-location) ,initargs-form)
    ;;Paper over a bug in KCL by passing the cache-symbol
    ;;here in addition to in the list.
    ',pv-table-symbol
    ',*inline-access*
    ',(when *inline-access* *method-source-info*)))

(defmacro make-method-function (method-lambda &environment env)
  (make-method-function-internal method-lambda env))

(defun make-method-function-internal (method-lambda &optional env)
  (multiple-value-bind (proto-gf proto-method)
	(prototypes-for-make-method-lambda nil)
    (multiple-value-bind (method-function-lambda initargs)
	(make-method-lambda proto-gf proto-method method-lambda env)
      (make-method-initargs-form proto-gf proto-method
				 method-function-lambda initargs env))))

(defun add-method-declarations (name qualifiers lambda-list body env)
  (multiple-value-bind (parameters unspecialized-lambda-list specializers)
      (parse-specialized-lambda-list lambda-list)
    (declare (ignore parameters))
    (multiple-value-bind (real-body declarations documentation)
	(system:parse-body body env)
      (values `(lambda ,unspecialized-lambda-list
		 ,@(when documentation `(,documentation))
		 (declare (method-name ,(list name qualifiers specializers)))
		 (declare (method-lambda-list ,@lambda-list))
		 ,@declarations
		 ,@real-body)
	      unspecialized-lambda-list specializers))))

#+loadable-pcl
(defmethod make-method-initargs-form ((proto-gf standard-generic-function)
				      (proto-mothed standard-method)
				      method-lambda initargs env)
  (unless (eq (car-safe method-lambda) 'lambda)
    (error "The method-lambda argument to make-method-function, ~S,~
            is not a lambda form" method-lambda))
  (make-method-initargs-form-internal method-lambda initargs env))

#-loadable-pcl
(defun real-make-method-initargs-form (proto-gf proto-method 
				       method-lambda initargs env)
  (declare (ignore proto-gf proto-method))
  (unless (and (consp method-lambda) (eq (car method-lambda) 'lambda))
    (error "~@<The ~s argument to ~s, ~s, is not a lambda form.~@:>"
	   'method-lambda 'make-method-lambda method-lambda))
  (make-method-initargs-form-internal method-lambda initargs env))

#-loadable-pcl
(unless (fboundp 'make-method-initargs-form)
  (setf (gdefinition 'make-method-initargs-form)
	(symbol-function 'real-make-method-initargs-form)))

#+loadable-pcl
(defmethod make-method-lambda ((proto-gf standard-generic-function)
			       (proto-method standard-method)
			       method-lambda env)
  (make-method-lambda-internal method-lambda env))

#-loadable-pcl
(defun real-make-method-lambda (proto-gf proto-method method-lambda env)
  (declare (ignore proto-gf proto-method))
  (make-method-lambda-internal method-lambda env))

(defun get-declaration (name declarations &optional default)
  (dolist (d declarations default)
    (dolist (form (cdr d))
      (when (and (consp form) (eq (car form) name))
	(return-from get-declaration (cdr form))))))

(defun gf-key-p (gf-name)
  (let ((info (and gf-name (info function type gf-name))))
    (and (kernel::function-type-p info)
	 (kernel::function-type-keyp info))))

(defun make-method-lambda-internal (method-lambda &optional env)
  (unless (and (consp method-lambda) (eq (car method-lambda) 'lambda))
    (error "~@<The ~s argument to ~s, ~s, is not a lambda form.~@:>"
	   'method-lambda 'make-method-lambda method-lambda))
  (multiple-value-bind (real-body declarations documentation)
      (system:parse-body (cddr method-lambda) env)
    (let* ((name-decl (get-declaration 'method-name declarations))
	   (sll-decl (get-declaration 'method-lambda-list declarations))
	   (method-name (when (consp name-decl) (car name-decl)))
	   (generic-function-name (when method-name (car method-name)))
	   (specialized-lambda-list (or sll-decl (cadr method-lambda))))
      (multiple-value-bind (params lambda-list specializers)
	  (parse-specialized-lambda-list specialized-lambda-list)
	;;
	(let* ((required-params
		(loop for p in params and s in specializers collect p))
	       ;;
	       ;; Determine which of the required parameters are
	       ;; assigned to.  We can't optimize slot access etc. for
	       ;; such parameters because we can't easily tell what they are
	       ;; actually bound to at each point where they are used.
	       (assigned-params
		(let ((assigned #+nil
			(assigned-method-params
			 method-lambda required-params env)))
		  (when assigned
		    (warn 'kernel:simple-style-warning
			  :format-control
			  "Assignment to method parameter~p ~{~s~^, ~} ~
                           might prevent CLOS optimizations"
			  :format-arguments
			  (list (length assigned) assigned)))
		  assigned))
	       ;;
	       ;; Parameters not assigned to can be declared to be
	       ;; of their respective type, which might give some
	       ;; useful output from the compiler.
	       (class-declarations
		(loop for p in params and s in specializers
		      when (and (symbolp s) (neq s t))
			collect `(class ,p ,s) into decls
		      finally (return `(declare ,@decls))))
	       ;;
	       ;; Slot access etc. through parameters not assigned to
	       ;; can be optimized.
	       (optimizable-params
		(set-difference required-params assigned-params))
	       ;;
	       (slots (mapcar #'list optimizable-params))
	       (calls (list nil))
	       (block-name (nth-value 1 (valid-function-name-p
					 generic-function-name)))
	       ;;
	       ;; Remove the documentation string and insert the
	       ;; appropriate class declarations.  The documentation
	       ;; string is removed to make it easy for us to insert
	       ;; new declarations later, they will just go after the
	       ;; cadr of the method lambda.  The class declarations
	       ;; are inserted to communicate the class of the method's
	       ;; arguments to the code walk.
	       (method-lambda
		`(lambda ,lambda-list
		   (declare (ignorable ,@required-params))
		   ,class-declarations
		   ,@declarations
		   (block ,block-name
		     ,@real-body)))
	       (constant-value-p (and (null (cdr real-body))
				      (constantp (car real-body))))
	       (constant-value (and constant-value-p
				    (eval (car real-body))))
	       (plist (if (and constant-value-p
			       (or (typep constant-value
					  '(or number character))
				   (and (symbolp constant-value)
					(symbol-package constant-value))))
			  (list :constant-value constant-value)
			  ()))
	       (applyp (dolist (p lambda-list nil)
			 (cond ((memq p '(&optional &rest &key))
				(return t))
			       ((eq p '&aux)
				(return nil))))))
	  (multiple-value-bind (walked-lambda call-next-method-p closurep
					      next-method-p-p)
	      ;;
	      ;; Process the method lambda, possibly optimizing forms
	      ;; appearing in it.
	      (walk-method-lambda method-lambda optimizable-params env 
				  slots calls)
	    (multiple-value-bind (walked-lambda-body walked-declarations
						     walked-documentation)
		(system:parse-body (cddr walked-lambda) env)
	      (declare (ignore walked-documentation))
	      (when (or next-method-p-p call-next-method-p)
		(setq plist (list* :needs-next-methods-p t plist)))
	      ;;
	      ;; If slot-value, set-slot-value, slot-boundp optimizations
	      ;; have been done in WALK-METHOD-LAMBDA, wrap a PV-BINDING
	      ;; form around the lambda-body, which gives the code in
	      ;; the lambda body access to the PV table, required parameters
	      ;; and slot name lists.
	      (when (or (some #'cdr slots) (cdr calls))
		(multiple-value-bind (slot-name-lists call-list)
		    (slot-name-lists-from-slots slots calls)
		  (let ((pv-table-symbol (make-symbol "pv-table")))
		    ;;
		    ;; PV-TABLE-SYMBOL's symbol-value is later set
		    ;; to an actual PV table for the method
		    ;; function; see INITIALIZE-METHOD-FUNCTION.
		    (setq plist 
			  `(,@(when slot-name-lists 
				`(:slot-name-lists ,slot-name-lists))
			      ,@(when call-list
				  `(:call-list ,call-list))
			      :pv-table-symbol ,pv-table-symbol
			      ,@plist))
		    (setq walked-lambda-body
			  `((pv-binding (,optimizable-params ,slot-name-lists
							     ,pv-table-symbol)
					,@walked-lambda-body))))))
	      ;;
	      ;; When the lambda-list contains &KEY but not
	      ;; &ALLOW-OTHER-KEYS, insert an &ALLOW-OTHER-KEYS
	      ;; into the lambda-list.  This corresponds to
	      ;; CLHS 7.6.4 which says that methods are effectively
	      ;; called as if :ALLOW-OTHER-KEYS T were supplied.
	      (when (and (memq '&key lambda-list)
			 (not (memq '&allow-other-keys lambda-list)))
		(let ((aux (memq '&aux lambda-list)))
		  (setq lambda-list (nconc (ldiff lambda-list aux)
					   (list '&allow-other-keys)
					   aux))))
	      ;;
	      ;; First value is the resulting lambda.  Second value
	      ;; is a list of initargs for the method instance being
	      ;; created.
	      (values `(lambda (.method-args. .next-methods.)
			 (simple-lexical-method-functions
			  (,lambda-list .method-args. .next-methods.
					:method-name-declaration ,name-decl
					:call-next-method-p ,call-next-method-p
					:next-method-p-p ,next-method-p-p
					:closurep ,closurep
					:applyp ,applyp)
			  ,@walked-declarations
			  ,@walked-lambda-body))
		      `(,@(when plist 
			    `(:plist ,plist))
			  ,@(when documentation 
			      `(:documentation ,documentation)))))))))))

#-loadable-pcl
(unless (fboundp 'make-method-lambda)
  (setf (gdefinition 'make-method-lambda)
	(symbol-function 'real-make-method-lambda)))

(defmacro simple-lexical-method-functions
    ((lambda-list method-args next-methods &rest lmf-options) 
     &body body)
  `(let ((,method-args ,method-args)
	 (,next-methods ,next-methods))
     (declare (ignorable ,method-args ,next-methods))
     (bind-simple-lexical-method-macros (,method-args ,next-methods)
       (bind-lexical-method-functions (,@lmf-options)
         (bind-args (,lambda-list ,method-args)
	   ,@body)))))

(defmacro fast-lexical-method-functions
    ((lambda-list next-method-call args rest-arg &rest lmf-options)
     &body body)
 `(bind-fast-lexical-method-macros (,args ,rest-arg ,next-method-call)
    (bind-lexical-method-functions (,@lmf-options)
      (bind-args (,(nthcdr (length args) lambda-list) ,rest-arg)
        ,@body))))

(defun call-no-next-method (method-name-declaration &rest args)
  (destructuring-bind (name qualifiers specializers)
      (car method-name-declaration)
    (let ((method (find-method (gdefinition name) qualifiers specializers)))
      (apply #'no-next-method (method-generic-function method) method args))))

(defmacro bind-simple-lexical-method-macros ((method-args next-methods)
					     &body body)
  `(macrolet ((call-next-method-bind (&body body)
		`(let ((.next-method. (car ,',next-methods))
		       (,',next-methods (cdr ,',next-methods)))
		   (declare (ignorable .next-method. ,',next-methods))
		   ,@body))
	      (with-rebound-original-arguments ((cnm-p) &body body)
		(declare (ignore cnm-p))
		`(let () ,@body))
	      (check-cnm-args-body (method-name-declaration cnm-args)
		`(%check-cnm-args ,cnm-args ,',method-args
				  ',method-name-declaration))
	      (call-next-method-body (method-name-declaration cnm-args)
		`(if .next-method.
		     (funcall (if (std-instance-p .next-method.)
				  (method-function .next-method.)
				  .next-method.) ; for early methods
			      (or ,cnm-args ,',method-args)
		              ,',next-methods)
		     (apply #'call-no-next-method ',method-name-declaration
                            (or ,cnm-args ,',method-args))))
	      (next-method-p-body ()
	        `(not (null .next-method.))))
     ,@body))

(defstruct method-call
  (function #'identity :type function)
  call-method-args)

(declaim (freeze-type method-call))

(defmacro invoke-method-call1 (function args cm-args)
  `(let ((.function. ,function)
	 (.args. ,args)
	 (.cm-args. ,cm-args))
     (if (and .cm-args. (null (cdr .cm-args.)))
	 (funcall (the function .function.) .args. (car .cm-args.))
	 (apply (the function .function.) .args. .cm-args.))))

(defmacro invoke-method-call (method-call restp &rest required-args+rest-arg)
  `(invoke-method-call1 (method-call-function ,method-call)
                        ,(if restp
			     `(list* ,@required-args+rest-arg)
			     `(list ,@required-args+rest-arg))
                        (method-call-call-method-args ,method-call)))

(defstruct fast-method-call
  (function #'identity :type function)
  pv-cell
  next-method-call
  arg-info)

(declaim (freeze-type fast-method-call))

(defmacro invoke-fast-method-call (method-call &rest required-args+rest-arg)
  `(function-funcall (fast-method-call-function ,method-call)
		     (fast-method-call-pv-cell ,method-call)
		     (fast-method-call-next-method-call ,method-call)
		     ,@required-args+rest-arg))

(defstruct fast-instance-boundp
  (index 0 :type fixnum))

(declaim (freeze-type fast-instance-boundp))

(defmacro invoke-effective-method-function-fast
    (emf restp &rest required-args+rest-arg)
  (declare (ignore restp))
  `(progn
     (invoke-fast-method-call ,emf ,@required-args+rest-arg)))

(defmacro invoke-effective-method-function
    (emf restp &rest required-args+rest-arg)
  (assert (constantp restp))
  (setq restp (eval restp))
  `(progn
     (etypecase ,emf
       (fast-method-call
	(invoke-fast-method-call ,emf ,@required-args+rest-arg))
       ;;
       ,@(when (and (null restp) (= 1 (length required-args+rest-arg)))
	  `((fixnum
	     (let* ((.slots. (get-slots-or-nil
			      ,(car required-args+rest-arg)))
		    (value (when .slots. (%slot-ref .slots. ,emf))))
	       (if (eq value +slot-unbound+)
		   (slot-unbound-internal ,(car required-args+rest-arg) ,emf)
		   value)))))
       ;;
       ;; This generates code in innocent methods that Python can
       ;; prove not to succeed, for example:
       ;;
       ;; (defclass foo () ())
       ;;
       ;; (defmethod foo3 ((x foo) y)
       ;;   (format y "Its methods are:~%")
       ;;  (call-next-method))
       ;;
       ;; The CALL-NEXT-METHOD contains an INVOKE-EFFECTIVE-M-F.  From
       ;; the use of Y in FORMAT, Python deduces what Y can be at the
       ;; point of the INVOKE-EFFECTIVE-METHOD-FUNCTION, namely (OR
       ;; (MEMBER NIL T) FUNCTION), and that's not something for which
       ;; the code below is known to succeed.  So, it prints a note.
       ;;
       ;; Since the user can't do much about this, and we can't
       ;; either, without tremendous effort, let's suppress warnings
       ;; here.
       ,@(when (and (null restp) (= 2 (length required-args+rest-arg)))
	   (destructuring-bind (new-value object) required-args+rest-arg
	     `((fixnum
		(locally
		    (declare (optimize (ext:inhibit-warnings 3)))
		  (setf (%slot-ref (get-slots-or-nil ,object) ,emf)
			,new-value))))))
       ;;
       (method-call
	(invoke-method-call ,emf ,restp ,@required-args+rest-arg))
       (function
	,(if restp
	     `(apply (the function ,emf) ,@required-args+rest-arg)
	     `(funcall (the function ,emf) ,@required-args+rest-arg))))))

(defun invoke-emf (emf args)
  (etypecase emf
    (fast-method-call
     (let* ((arg-info (fast-method-call-arg-info emf))
	    (restp (cdr arg-info))
	    (nreq (car arg-info)))
       (if restp
	   (let* ((rest-args (nthcdr nreq args))
		  (req-args (ldiff args rest-args)))
	     (apply (fast-method-call-function emf)
		    (fast-method-call-pv-cell emf)
		    (fast-method-call-next-method-call emf)
		    (nconc req-args (list rest-args))))
	   (cond ((null args)
		  (if (eql nreq 0) 
		      (invoke-fast-method-call emf)
		      (internal-program-error emf "Wrong number of args.")))
		 ((null (cdr args))
		  (if (eql nreq 1) 
		      (invoke-fast-method-call emf (car args))
		      (internal-program-error emf "Wrong number of args.")))
		 ((null (cddr args))
		  (if (eql nreq 2) 
		      (invoke-fast-method-call emf (car args) (cadr args))
		      (internal-program-error emf "Wrong number of args.")))
		 (t
		  (apply (fast-method-call-function emf)
			 (fast-method-call-pv-cell emf)
			 (fast-method-call-next-method-call emf)
			 args))))))
    (method-call 
     (apply (method-call-function emf)
	    args
	    (method-call-call-method-args emf)))
    (fixnum 
     (cond ((null args)
	    (internal-program-error emf "1 or 2 args expected."))
	   ((null (cdr args))
	    (let ((value (%slot-ref (get-slots (car args)) emf)))
	      (if (eq value +slot-unbound+)
		  (slot-unbound-internal (car args) emf)
		  value)))
	   ((null (cddr args))
	    (setf (%slot-ref (get-slots (cadr args)) emf)
		  (car args)))
	   (t
	    (internal-program-error emf "1 or 2 args expected."))))
    (fast-instance-boundp
     (if (or (null args) (cdr args))
	 (internal-program-error emf "1 arg expected.")
	 (not (eq (%slot-ref (get-slots (car args)) 
			     (fast-instance-boundp-index emf))
		  +slot-unbound+))))
    (function
     (apply emf args))))

(defmacro bind-fast-lexical-method-macros ((args rest-arg next-method-call)
					   &body body)
  (let* ((all-params (append args (when rest-arg `(,rest-arg))))
	 (bindings (mapcar (lambda (x) `(,x ,x)) all-params)))
    `(macrolet ((call-next-method-bind (&body body)
		  `(let () ,@body))
		;;
		;; Rebind method parameters around BODY if CNM-P is true.
		;; CNM-P true means there is a CALL-NEXT-METHOD in BODY.
		;;
		;; The problem with rebinding parameters is that this
		;; can use parameters that are declared to be ignored
		;; in the method body, leading to spurious warnings.
		;; For now, we deal with this by translating IGNORE to
		;; IGNORABLE.
		(with-rebound-original-arguments ((cnm-p) &body body)
		  (if cnm-p
		      `(let ,',bindings
			 (declare (ignorable ,@',all-params))
			 ,@body)
		      `(let () ,@body)))
		;;
		(check-cnm-args-body (method-name-declaration cnm-args)
		  `(%check-cnm-args ,cnm-args (list ,@',args)
				    ',method-name-declaration))
		;;
		(call-next-method-body (method-name-declaration cnm-args)
		  `(if ,',next-method-call
		       ,(if (and (null ',rest-arg)
				 (consp cnm-args)
				 (eq (car cnm-args) 'list))
			    `(invoke-effective-method-function
			      ,',next-method-call nil
			      ,@(cdr cnm-args))
			    (let ((call `(invoke-effective-method-function
					  ,',next-method-call 
					  ,',(not (null rest-arg))
					  ,@',args 
					  ,@',(when rest-arg `(,rest-arg)))))
			      `(if ,cnm-args
				   (bind-args ((,@',args ,@',(when rest-arg
							       `(&rest ,rest-arg)))
					       ,cnm-args)
					      ,call)
				   ,call)))
		       ,(if (and (null ',rest-arg)
				 (consp cnm-args)
				 (eq (car cnm-args) 'list))
			    `(call-no-next-method ',method-name-declaration
						  ,@(cdr cnm-args))
			    `(call-no-next-method ',method-name-declaration
						  ,@',args
						  ,@',(when rest-arg
							`(,rest-arg))))))
		(next-method-p-body ()
		  `(not (null ,',next-method-call))))
       ,@body)))

(defmacro bind-lexical-method-functions 
    ((&key call-next-method-p next-method-p-p closurep applyp
	   method-name-declaration)
     &body body)
  (declare (ignore next-method-p-p closurep applyp))
  `(call-next-method-bind
    (flet ((call-next-method (&rest cnm-args)
	     (check-cnm-args-body ,method-name-declaration cnm-args)
	     (call-next-method-body ,method-name-declaration cnm-args))
	   (next-method-p ()
	     (next-method-p-body)))
      (declare (ignorable #'call-next-method #'next-method-p))
      (with-rebound-original-arguments (,call-next-method-p)
	,@body))))

;;;
;;; The standard says it's an error if CALL-NEXT-METHOD is called with
;;; arguments, and the set of methods applicable to those arguments is
;;; different from the set of methods applicable to the original
;;; method arguments.  (According to Barry Margolin, this rule was
;;; probably added to ensure that before and around methods are always
;;; run before primary methods.)
;;;
;;; This could be optimized for the case that the generic function
;;; doesn't have hairy methods, does have standard method combination,
;;; is a standard generic function, there are no methods defined on it
;;; for COMPUTE-APPLICABLE-METHODS and probably a lot more of such
;;; preconditions.  That looks hairy and is probably not worth it,
;;; because this check will never be fast.
;;;
(defun %check-cnm-args (cnm-args orig-args method-name-declaration)
  (when cnm-args
    (let* ((gf (fdefinition (caar method-name-declaration)))
	   (omethods (compute-applicable-methods gf orig-args))
	   (nmethods (compute-applicable-methods gf cnm-args)))
      (unless (equal omethods nmethods)
	(error "~@<The set of methods ~s applicable to argument~p ~
                ~{~s~^, ~} to call-next-method is different from ~
                the set of methods ~s applicable to the original ~
                method argument~p ~{~s~^, ~}.~@:>"
	       nmethods (length cnm-args) cnm-args omethods
	       (length orig-args) orig-args)))))

;;;
;;; Remove the above check, unless in safe code.
;;;
(in-package :c)

(defknown pcl::%check-cnm-args (t t t) t
  (movable foldable flushable))

(deftransform pcl::%check-cnm-args ((x y z) (t t t) t :policy (< safety 3))
  nil)
	  
(in-package :pcl)
	
(defun too-many-args ()
  (simple-program-error "Too many arguments."))

(declaim (inline get-key-arg))
(defun get-key-arg (keyword list)
  (car (get-key-arg1 keyword list)))

(defun get-key-arg1 (keyword list)
  (loop for (key . more) on list by #'cddr
	when (eq key keyword) return more))

(defmacro bind-args ((lambda-list args) &body body)
  (let ((state 'required))
    (collect ((bindings) (ignorable (list '.args.)))
      (dolist (var lambda-list)
	(if (memq var lambda-list-keywords)
	    (ecase var
	      ((&optional &key &rest &aux)
	       (setq state var))
	      (&allow-other-keys))
	    (case state
	      (required
	       (bindings `(,var (pop .args.))))
	      (&optional
	       (etypecase var
		 (symbol
		  (bindings `(,var (when .args. (pop .args.)))))
		 (cons
		  (destructuring-bind (var &optional default
					   (supplied nil supplied-p))
		      var
		    (when supplied-p (bindings `(,supplied .args.)))
		    (bindings `(,var (if .args. (pop .args.) ,default)))))))
	      (&rest
	       (bindings `(,var .args.)))
	      (&key
	       (etypecase var
		 (symbol
		  (bindings `(,var (get-key-arg ,(make-keyword var) .args.))))
		 (cons
		  (destructuring-bind (var &optional default
					   (supplied nil supplied-p))
		      var
		    (multiple-value-bind (key var)
			(if (consp var)
			    (destructuring-bind (key var) var
			      (values key var))
			    (values (make-keyword var) var))
		      (bindings `(.key. (get-key-arg1 ',key .args.)))
		      (when supplied-p (bindings `(,supplied .key.)))
		      (bindings `(,var (if .key. (car .key.) ,default))))))))
	      (&aux
	       (bindings var)))))
      (when (eq state '&optional)
	(bindings '(.dummy. (unless (null .args.) (too-many-args))))
	(ignorable '.dummy.))
      `(let* ((.args. ,args) ,@(bindings))
	 (declare (ignorable ,@(ignorable)))
	 ,@body))))

(defun walk-method-lambda (method-lambda required-parameters env slots calls)
  (let ((call-next-method-p nil)   ;flag indicating that call-next-method
				   ;should be in the method definition
	(closurep nil)		   ;flag indicating that #'call-next-method
				   ;was seen in the body of a method
	(next-method-p-p nil))     ;flag indicating that next-method-p
				   ;should be in the method definition
    (flet ((walk-function (form context env)
	     (cond ((not (eq context :eval))
		    form)
		   ((not (listp form))
		    form)
		   ((eq (car form) 'call-next-method)
		    (setq call-next-method-p t)
		    form)
		   ((eq (car form) 'next-method-p)
		    (setq next-method-p-p t)
		    form)
		   ((and (eq (car form) 'function)
			 (cond ((eq (cadr form) 'call-next-method)
				(setq call-next-method-p t)
				(setq closurep t)
				form)
			       ((eq (cadr form) 'next-method-p)
				(setq next-method-p-p t)
				(setq closurep t)
				form)
			       (t nil))))
		   ;;
		   ((and (memq (car form)
			       '(slot-value slot-boundp set-slot-value))
			 (constantp (caddr form)))
		    (optimize-slot-access form env required-parameters slots))
		   ;;
		   ((and (eq (car form) 'apply)
			 (consp (cadr form))
			 (eq (car (cadr form)) 'function)
			 (info-gf-name-p (cadr (cadr form))))
		    (optimize-gf-call form required-parameters calls env))
		   ;;
		   ((and (symbolp (car form))
			 (null (cddr form))
			 (info-accessor-p (car form)))
		    (optimize-slot-reader form required-parameters slots env))
		   ;;
		   ((and (eq (car form) 'setf)
			 (consp (cadr form))
			 (= (length form) 3)
			 (= (length (cadr form)) 2)
			 (info-accessor-p `(setf ,(caadr form))))
		    (optimize-slot-writer form required-parameters slots env))
		   ;;
		   ((and (valid-function-name-p (car form))
			 (info-gf-name-p (car form)))
		    (optimize-gf-call form required-parameters calls env))
		   (t
		    form))))
	  
      (let ((walked-lambda (walk-form method-lambda env #'walk-function)))
	(values walked-lambda
		call-next-method-p closurep next-method-p-p)))))

;;;
;;; If VAR is the name of a required method parameter in
;;; REQUIRED-PARAMS, or a variable rebinding of such a parameter in
;;; lexical environment ENV, or a form (THE ... <param>) for such a
;;; parameter, return the method parameter's name.  Otherwise return
;;; NIL.
;;;
(defun method-parameter (var required-params env)
  (let ((var (if (eq (car-safe var) 'the) (third var) var)))
    (when (symbolp var)
      (let* ((vr (caddr (variable-declaration 'variable-rebinding var env)))
	     (var (or vr var))
	     (param (car (memq var required-params))))
	;;
	;; (defmethod foo ((class integer))
        ;;   (flet ((bar ()
	;;            (loop for class in (reverse class) do
	;;	              (print class))))
        ;;     (bar)))
	;;
	;; results in two lexical vars being recorded in env inside
	;; the flet/loop, one for the method parameter, and one from
	;; a let generated by the loop.
	(when param
	  (let ((lexvars (walker::env-lexical-variables env)))
	    (when (= (count var lexvars :key #'car) 1)
	      param)))))))

;;;
;;; Return a list of those parameters from REQUIRED-PARAMS which
;;; are assigned to in METHOD-LAMBDA.
;;;
(defun assigned-method-params (method-lambda required-params env)
  (let ((assigned-params ()))
    (flet ((walk (form context env)
	     (when (and (eq context :eval)
			(memq (car-safe form) '(setq setf)))
	       (loop for var in (cdr form) by #'cddr
		     as param = (method-parameter var required-params env)
		     when param do
		       (pushnew param assigned-params)))
	     form))
      (walk-form method-lambda env #'walk)
      assigned-params)))

(defun generic-function-name-p (name)
  (and (valid-function-name-p name)
       (fboundp name)
       (if (eq *boot-state* 'complete)
	   (standard-generic-function-p (gdefinition name))
	   (funcallable-instance-p (gdefinition name)))))



(defvar *method-function-plist* (make-hash-table :test 'eq))
(defvar *mf1* nil) (defvar *mf1p* nil) (defvar *mf1cp* nil)
(defvar *mf2* nil) (defvar *mf2p* nil) (defvar *mf2cp* nil)

(defun method-function-plist (method-function)
  (unless (eq method-function *mf1*)
    (rotatef *mf1* *mf2*)
    (rotatef *mf1p* *mf2p*)
    (rotatef *mf1cp* *mf2cp*))
  (unless (or (eq method-function *mf1*) (null *mf1cp*))
    (setf (gethash *mf1* *method-function-plist*) *mf1p*))
  (unless (eq method-function *mf1*)
    (setf *mf1* method-function
	  *mf1cp* nil
	  *mf1p* (gethash method-function *method-function-plist*)))
  *mf1p*)

(defun (setf method-function-plist)
       (val method-function)
  (unless (eq method-function *mf1*)
    (rotatef *mf1* *mf2*)
    (rotatef *mf1cp* *mf2cp*)
    (rotatef *mf1p* *mf2p*))
  (unless (or (eq method-function *mf1*) (null *mf1cp*))
    (setf (gethash *mf1* *method-function-plist*) *mf1p*))
  (setf *mf1* method-function
	*mf1cp* t
	*mf1p* val))

(defun method-function-get (method-function key &optional default)
  (getf (method-function-plist method-function) key default))

(defun (setf method-function-get)
       (val method-function key)
  (setf (getf (method-function-plist method-function) key) val))

(defun method-function-pv-table (method-function)
  (method-function-get method-function :pv-table))

(defun method-function-method (method-function)
  (method-function-get method-function :method))

(defun method-function-needs-next-methods-p (method-function)
  (method-function-get method-function :needs-next-methods-p t))

;;;
;;; Return a method function name of METHOD.  If FAST-FUNCTION
;;; is true, return the fast method function name, otherwise
;;; return the slow method function name.
;;;
(defun method-function-name (method &optional (fast-function t))
  (let ((fn (slot-value method
			(if fast-function 'fast-function 'function))))
    (assert (functionp fn))
    (method-function-get fn :name)))


(defun load-defmethod (class name quals specls ll initargs
		       &optional pv-table-symbol inline-access
		       method-info)
  (setq initargs (copy-tree initargs))
  (let ((method-spec (or (getf initargs :method-spec)
			 (make-method-spec name quals specls))))
    (setf (getf initargs :method-spec) method-spec)
    (load-defmethod-internal class name quals specls ll initargs
			     pv-table-symbol inline-access
			     method-info)))

(defvar *compile-interpreted-methods-p* t
  "When true, compile interpreted method functions.")

(defun load-defmethod-internal
    (method-class gf-name qualifiers specializers lambda-list 
     initargs pv-table-symbol inline-access method-info)
  (when pv-table-symbol
    (setf (getf (getf initargs :plist) :pv-table-symbol)
	  pv-table-symbol))
  ;;
  ;; Optionally compile method functions if they are interpreted.
  (when *compile-interpreted-methods-p*
    (flet ((maybe-compile (key)
	     (let ((fn (getf initargs key)))
	       (when (and (eval:interpreted-function-p fn)
			  ;; Can't compile closures.
			  (null (eval:interpreted-function-closure fn)))
		 (let* ((type (ecase key
				(:fast-function 'fast-method)
				(:function 'method)))
			(name `(,type ,gf-name ,@qualifiers ,specializers)))
		   (compile name fn)
		   (setf (getf initargs key) (fdefinition name)))))))
      (maybe-compile :fast-function)
      (maybe-compile :function)))
  ;;
  (let ((method (apply #'add-named-method
		       gf-name qualifiers specializers lambda-list initargs)))
    ;;
    (record-inline-access-info method inline-access method-info)
    ;;
    (unless (or (eq method-class 'standard-method)
		(eq (find-class method-class nil) (class-of method)))
      (format *error-output*
	      "~&~@<At the time the method with qualifiers ~S and ~
               specializers ~S on the generic function ~S ~
               was compiled, the method class for that generic function was ~
               ~S.  But, the method class is now ~S, this ~
               may mean that this method was compiled improperly.~@:>"
	      qualifiers specializers gf-name
	      method-class (class-name (class-of method))))
    method))

(defun make-method-spec (gf-spec qualifiers unparsed-specializers)
  `(method ,gf-spec ,@qualifiers ,unparsed-specializers))

(defun initialize-method-function (initargs &optional return-function-p method)
  (let* ((mf (getf initargs :function))
	 (method-spec (getf initargs :method-spec))
	 (plist (getf initargs :plist))
	 (pv-table-symbol (getf plist :pv-table-symbol))
	 (pv-table nil)
	 (mff (getf initargs :fast-function)))
    (flet ((set-mf-property (p v)
	     (when mf
	       (setf (method-function-get mf p) v))
	     (when mff
	       (setf (method-function-get mff p) v))))
      (when method-spec
	(when mf
	  (setq mf (set-function-name mf method-spec)))
	(when mff
	  (let ((name (cons 'fast-method (cdr method-spec))))
	    (set-function-name mff name)
	    (unless mf
	      (set-mf-property :name name)))))
      (when plist
	(let ((snl (getf plist :slot-name-lists))
	      (cl (getf plist :call-list)))
	  (when (or snl cl)
	    (setq pv-table (intern-pv-table :slot-name-lists snl
					    :call-list cl))
	    (when pv-table (set pv-table-symbol pv-table))
	    (set-mf-property :pv-table pv-table)))    
	(loop (when (null plist) (return nil))
	      (set-mf-property (pop plist) (pop plist)))      
	(when method
	  (set-mf-property :method method))    
	(when return-function-p
	  (or mf (method-function-from-fast-function mff)))))))



(defun analyze-lambda-list (lambda-list)
  (multiple-value-bind (required optional restp rest keyp keys
				 allow-other-keys-p aux)
      (parse-lambda-list lambda-list t)
    (declare (ignore rest aux))
    (flet ((keyword-parameter-keyword (x)
	     (let ((key (if (atom x) x (car x))))
	       (if (atom key)
		   (make-keyword key)
		   (car key)))))
      (values (length required) (length optional)
	      keyp restp allow-other-keys-p
	      (mapcar #'keyword-parameter-keyword keys)
	      keys))))

(defun ftype-declaration-from-lambda-list (lambda-list name)
  (multiple-value-bind (nrequired noptional keysp restp allow-other-keys-p
				  keywords keyword-parameters)
      (analyze-lambda-list lambda-list)
    (declare (ignore keyword-parameters))
    (let* ((old (c::info function type name))
	   (old-ftype (if (c::function-type-p old) old nil))
	   (old-restp (and old-ftype (c::function-type-rest old-ftype)))
	   (old-keys (and old-ftype
			  (mapcar #'c::key-info-name
				  (c::function-type-keywords old-ftype))))
	   (old-keysp (and old-ftype (c::function-type-keyp old-ftype)))
	   (old-allowp (and old-ftype (c::function-type-allowp old-ftype)))
	   (keywords (union old-keys keywords)))
      `(function ,(append (make-list nrequired :initial-element t)
			  (when (plusp noptional)
			    (append '(&optional)
				    (make-list noptional :initial-element t)))
			  (when (or restp old-restp)
			    '(&rest t))
			  (when (or keysp old-keysp)
			    (append '(&key)
				    (mapcar (lambda (key)
					      `(,key t))
					    keywords)
				    (when (or allow-other-keys-p old-allowp)
				      '(&allow-other-keys)))))
		 *))))

(defun proclaim-defgeneric (spec lambda-list)
  (let ((decl `(ftype ,(ftype-declaration-from-lambda-list lambda-list spec)
		      ,spec)))
    (set-gf-info spec lambda-list)
    (proclaim decl)))

;;;; Early generic-function support
;;;
;;;
(defvar *early-generic-functions* ())

(defun ensure-generic-function (function-specifier
				&rest all-keys
				&key environment
				&allow-other-keys)
  (declare (ignore environment))
  (let ((existing (and (fboundp function-specifier)
		       (gdefinition function-specifier))))
    (when (and existing
	       (eq *boot-state* 'complete)
	       (null (generic-function-p existing)))
      (generic-clobbers-function function-specifier)
      (setq existing nil))
    (apply #'ensure-generic-function-using-class
	   existing function-specifier all-keys)))

(defun generic-clobbers-function (function-specifier)
  (restart-case
      (simple-program-error
       "~@<~S already names an ordinary function or a macro.  ~
	If you want to replace it with a generic function, you should remove ~
        the existing definition beforehand.~@:>"
       function-specifier)
    (continue ()
      :report (lambda (stream)
		(format stream "~@<Discard the existing definition of ~S.~@:>"
			function-specifier))
      (fmakunbound function-specifier))))

(defvar *sgf-wrapper* 
  (boot-make-wrapper (early-class-size 'standard-generic-function)
		     'standard-generic-function))

(defvar *sgf-slots-init*
  (mapcar (lambda (canonical-slot)
	    (if (memq (getf canonical-slot :name) '(arg-info source))
		+slot-unbound+
		(let ((initfunction (getf canonical-slot :initfunction)))
		  (if initfunction
		      (funcall initfunction)
		      +slot-unbound+))))
	  (early-collect-inheritance 'standard-generic-function)))

(defvar *sgf-method-class-index* 
  (bootstrap-slot-index 'standard-generic-function 'method-class))

(defun early-gf-p (x)
  (and (fsc-instance-p x)
       (eq (slot-ref (get-slots x) *sgf-method-class-index*)
	   +slot-unbound+)))

(defvar *sgf-methods-index* 
  (bootstrap-slot-index 'standard-generic-function 'methods))

(defmacro early-gf-methods (gf)
  `(slot-ref (get-slots ,gf) *sgf-methods-index*))

(defvar *sgf-arg-info-index* 
  (bootstrap-slot-index 'standard-generic-function 'arg-info))

(defmacro early-gf-arg-info (gf)
  `(slot-ref (get-slots ,gf) *sgf-arg-info-index*))

(defvar *sgf-dfun-state-index* 
  (bootstrap-slot-index 'standard-generic-function 'dfun-state))

(defstruct (arg-info
	     (:conc-name nil)
	     (:constructor make-arg-info ()))
  (arg-info-lambda-list :no-lambda-list)
  arg-info-precedence
  arg-info-metatypes
  arg-info-number-optional
  arg-info-key/rest-p
  arg-info-keywords ;nil         no keyword or rest allowed
	            ;(k1 k2 ..)  each method must accept these keyword arguments
	            ;T           must have &key or &rest

  gf-info-simple-accessor-type ; nil, reader, writer, boundp
  (gf-precompute-dfun-and-emf-p nil) ; set by set-arg-info

  gf-info-static-c-a-m-emf
  (gf-info-c-a-m-emf-std-p t)
  gf-info-fast-mf-p)

(declaim (freeze-type arg-info))

(defun arg-info-valid-p (arg-info)
  (not (null (arg-info-number-optional arg-info))))

(defun arg-info-applyp (arg-info)
  (or (plusp (arg-info-number-optional arg-info))
      (arg-info-key/rest-p arg-info)))

(defun arg-info-number-required (arg-info)
  (length (arg-info-metatypes arg-info)))

(defun arg-info-nkeys (arg-info)
  (count-if (lambda (x) (neq x t)) (arg-info-metatypes arg-info)))

(defun set-arg-info (gf &key new-method (lambda-list nil lambda-list-p)
		     argument-precedence-order)
  (let* ((arg-info (if (eq *boot-state* 'complete)
		       (gf-arg-info gf)
		       (early-gf-arg-info gf)))
	 (methods (if (eq *boot-state* 'complete)
		      (generic-function-methods gf)
		      (early-gf-methods gf)))
	 (was-valid-p (integerp (arg-info-number-optional arg-info)))
	 (first-p (and new-method (null (cdr methods)))))
    (when (and (not lambda-list-p) methods)      
      (setq lambda-list (gf-lambda-list gf)))
    (when (or lambda-list-p
	      (and first-p (eq (arg-info-lambda-list arg-info) :no-lambda-list)))
      (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
	  (analyze-lambda-list lambda-list)
	(when (and methods (not first-p))
	  (let ((gf-nreq (arg-info-number-required arg-info))
		(gf-nopt (arg-info-number-optional arg-info))
		(gf-key/rest-p (arg-info-key/rest-p arg-info)))
	    (unless (and (= nreq gf-nreq)
			 (= nopt gf-nopt)
			 (eq (or keysp restp) gf-key/rest-p))
	      (error "~@<The lambda-list ~S is incompatible with ~
                      existing methods of ~S.~@:>"
		     lambda-list gf))))
	(when lambda-list-p
	  (setf (arg-info-lambda-list arg-info) lambda-list))
	(when (or lambda-list-p argument-precedence-order
		  (null (arg-info-precedence arg-info)))
	  (setf (arg-info-precedence arg-info)
		(compute-precedence lambda-list nreq
				    argument-precedence-order)))
	(setf (arg-info-metatypes arg-info) (make-list nreq))
	(setf (arg-info-number-optional arg-info) nopt)
	(setf (arg-info-key/rest-p arg-info) (not (null (or keysp restp))))
	(setf (arg-info-keywords arg-info) 
	      (if lambda-list-p
		  (if allow-other-keys-p t keywords)
		  (arg-info-key/rest-p arg-info)))))
    (when new-method
      (check-method-arg-info gf arg-info new-method))
    (set-arg-info1 gf arg-info new-method methods was-valid-p first-p)
    arg-info))

(declaim (inline generic-function-name*))
(defun generic-function-name* (gf)
  (if (early-gf-p gf)
      (early-gf-name gf)
      (generic-function-name gf)))

(declaim (inline generic-function-methods*))
(defun generic-function-methods* (gf)
  (if (early-gf-p gf)
      (early-gf-methods gf)
      (generic-function-methods gf)))

(declaim (inline gf-arg-info*))
(defun gf-arg-info* (gf)
  (if (early-gf-p gf)
      (early-gf-arg-info gf)
      (gf-arg-info gf)))

(declaim (inline method-lambda-list*))
(defun method-lambda-list* (method)
  (if (consp method)
      (early-method-lambda-list method)
      (method-lambda-list method)))

(defun compute-precedence (lambda-list nreq argument-precedence-order)
  (if (null argument-precedence-order)
      (let ((list nil))
	(dotimes (i nreq list)
	  (declare (fixnum i))
	  (push (- (1- nreq) i) list)))
      (mapcar (lambda (x) (position x lambda-list))
	      argument-precedence-order)))

(defun check-method-arg-info (gf arg-info method)
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list (method-lambda-list* method))
    (flet ((lose (format-control &rest format-args)
	     (simple-program-error
	      (format nil "~~@<Attempt to add the method ~~S to the generic ~
                           function ~~S, but ~?.~~@:>"
		      format-control format-args)
	      method gf))
	   (compare (x y)
	     (if (> x y) "more" "fewer")))
      (let ((gf-nreq (arg-info-number-required arg-info))
	    (gf-nopt (arg-info-number-optional arg-info))
	    (gf-key/rest-p (arg-info-key/rest-p arg-info))
	    (gf-keywords (arg-info-keywords arg-info)))
	(unless (= nreq gf-nreq)
	  (lose "the method has ~A required arguments than the ~
                 generic function"
		(compare nreq gf-nreq)))
	(unless (= nopt gf-nopt)
	  (lose "the method has ~S optional arguments than the ~
                 generic function"
		(compare nopt gf-nopt)))
	(unless (eq (or keysp restp) gf-key/rest-p)
	  (lose "the method and generic function differ in whether ~
                 they accept rest or keyword arguments"))
	(when (consp gf-keywords)
	  (unless (or (and restp (not keysp))
		      allow-other-keys-p
		      (every (lambda (k) (memq k keywords)) gf-keywords))
	    (lose "the method does not accept each of the keyword ~
                   arguments ~S"
		  gf-keywords)))))))

(defun set-arg-info1 (gf arg-info new-method methods was-valid-p first-p)  
  (let* ((existing-p (and methods (cdr methods) new-method))
	 (nreq (length (arg-info-metatypes arg-info)))
	 (metatypes (if existing-p
			(arg-info-metatypes arg-info)
			(make-list nreq)))
	 (type (if existing-p
		   (gf-info-simple-accessor-type arg-info)
		   nil)))
    (when (arg-info-valid-p arg-info)
      (dolist (method (if new-method (list new-method) methods))
	(let* ((specializers (if (or (eq *boot-state* 'complete)
				     (not (consp method)))
				 (method-specializers method)
				 (early-method-specializers method t)))
	       (class (if (or (eq *boot-state* 'complete) (not (consp method)))
			  (class-of method)
			  (early-method-class method)))
	       (new-type (when (and class
				    (or (not (eq *boot-state* 'complete))
					(eq (generic-function-method-combination gf)
					    *standard-method-combination*)))
			   (cond ((eq class *the-class-standard-reader-method*)
				  'reader)
				 ((eq class *the-class-standard-writer-method*)
				  'writer)
				 ((eq class *the-class-standard-boundp-method*)
				  'boundp)))))
	  (setq metatypes (mapcar #'raise-metatype metatypes specializers))
	  (setq type (cond ((null type) new-type)
			   ((eq type new-type) type)
			   (t nil)))))
      (setf (arg-info-metatypes arg-info) metatypes)
      (setf (gf-info-simple-accessor-type arg-info) type)))
  (when (or (not was-valid-p) first-p)
    (multiple-value-bind (c-a-m-emf std-p)
	(if (early-gf-p gf)
	    (values t t)
	    (compute-applicable-methods-emf gf))
      (setf (gf-info-static-c-a-m-emf arg-info) c-a-m-emf)
      (setf (gf-info-c-a-m-emf-std-p arg-info) std-p)
      (unless (gf-info-c-a-m-emf-std-p arg-info)
	(setf (gf-info-simple-accessor-type arg-info) t))))
  ;;
  ;; Let dfuns and emfs be pre-computed for "normal" PCL methods, not
  ;; the ones generated in the course of optimizations.
  (unless was-valid-p
    (let ((name (if (eq *boot-state* 'complete)
		    (generic-function-name gf)
		    (early-gf-name gf))))
      (setf (gf-precompute-dfun-and-emf-p arg-info)
	    (multiple-value-bind (valid sym)
		(valid-function-name-p name)
	      (and valid
		   (not (pcl-internal-function-name-p name))
		   (symbolp sym)
		   (let ((pkg (symbol-package sym))
			 (pcl *the-pcl-package*))
		     (or (eq pkg pcl)
			 (memq pkg (package-use-list pcl)))))))))
  ;;
  (setf (gf-info-fast-mf-p arg-info)
	 (or (not (eq *boot-state* 'complete))
	     (let* ((method-class (generic-function-method-class gf))
		    (methods (compute-applicable-methods 
			      #'make-method-lambda
			      (list gf (class-prototype method-class)
				    '(lambda) nil))))
	       (and methods (null (cdr methods))
		    (let ((specls (method-specializers (car methods))))
		      (and (classp (car specls))
			   (eq 'standard-generic-function
			       (class-name (car specls)))
			   (classp (cadr specls))
			   (eq 'standard-method
			       (class-name (cadr specls)))))))))
  arg-info)

;;;
;;; This is the early definition of ensure-generic-function-using-class.
;;; 
;;; The static-slots field of the funcallable instances used as early generic
;;; functions is used to store the early methods and early discriminator code
;;; for the early generic function.  The static slots field of the fins
;;; contains a list whose:
;;;    CAR    -   a list of the early methods on this early gf
;;;    CADR   -   the early discriminator code for this method
;;;
#-loadable-pcl
(defun ensure-generic-function-using-class
    (existing spec &rest keys
     &key (lambda-list nil lambda-list-p) argument-precedence-order
     definition-source
     &allow-other-keys)
  (declare (ignore keys))
  (cond ((and existing (early-gf-p existing))
	 existing)
	((assoc spec *generic-function-fixups* :test #'equal)
	 (if existing
	     (make-early-gf spec lambda-list lambda-list-p existing
			    argument-precedence-order)
	     (error "~@<The function ~S is not already defined.~@:>" spec)))
	(existing
	 (error "~@<~S should be on the list ~S.~@:>" spec
		'*generic-function-fixups*))
	(t
	 (pushnew spec *early-generic-functions* :test #'equal)
	 (make-early-gf spec lambda-list lambda-list-p nil
			argument-precedence-order definition-source))))

(defun make-early-gf (spec &optional lambda-list lambda-list-p function
		      argument-precedence-order source)
  (let ((fin (allocate-funcallable-instance *sgf-wrapper* *sgf-slots-init*)))
    (set-funcallable-instance-function 
     fin 
     (or function
	 (if (eq spec 'print-object)
	     #'(kernel:instance-lambda (instance stream)
		 (print-unreadable-object (instance stream :identity t)
		   (format stream "std-instance")))
	     #'(kernel:instance-lambda (&rest args)
		 (declare (ignore args))
		 (error "~@<The function of the funcallable instance ~S ~
			 has not been set.~@:>" fin)))))
    (setf (gdefinition spec) fin)
    (bootstrap-set-slot 'standard-generic-function fin 'name spec)
    (bootstrap-set-slot 'standard-generic-function fin 'source source)
    (set-function-name fin spec)
    (let ((arg-info (make-arg-info)))
      (setf (early-gf-arg-info fin) arg-info)
      (when lambda-list-p
	(if argument-precedence-order
	    (set-arg-info fin
			  :lambda-list lambda-list
			  :argument-precedence-order
			  argument-precedence-order)
	    (set-arg-info fin :lambda-list lambda-list))))
    fin))

;;;
;;; When loading PCL on top of itself, generic dispatch functions and
;;; effective methods would normally be recomputed when methods are
;;; defined.  This is a problem because many generic functions are
;;; called in the implementation of dispatch and effective method
;;; functions (the meta-circularity).  These generic functions cannot
;;; be called until their dispatch functions and effective methods are
;;; computed.
;;;
;;; We prevent here the setting of GF-DFUN-STATE to NIL.  This has the
;;; effect that COMPUTE-DISCRIMINATING-FUNCTION uses the existing
;;; discriminating function of the PCL on top of which the new PCL is
;;; loaded.  The generic functions affected are collected in
;;; *GENERIC-FUNCTIONS-TO-RECOMPUTE* for later fixing.
;;;

(defvar *loading-pcl-p* nil)

(defvar *generic-functions-to-recompute* ())

(defun set-dfun (gf &optional dfun cache info)
  (when cache
    (setf (cache-owner cache) gf))
  (let ((new-state (if (and dfun (or cache info))
		       (list* dfun cache info)
		       dfun)))
    (if (eq *boot-state* 'complete)
	#+loadable-pcl
	(if (and *loading-pcl-p* (null new-state))
	    (pushnew gf *generic-functions-to-recompute*)
	    (setf (gf-dfun-state gf) new-state))
	#-loadable-pcl
	(setf (gf-dfun-state gf) new-state)
	(setf (slot-ref (get-slots gf) *sgf-dfun-state-index*) new-state)))
  dfun)

(defun recompute-generic-functions ()
  (let ((*loading-pcl-p* nil))
    (mapc #'update-dfun *generic-functions-to-recompute*)))

(defun gf-dfun-cache (gf)
  (let ((state (if (eq *boot-state* 'complete)
		   (gf-dfun-state gf)
		   (slot-ref (get-slots gf) *sgf-dfun-state-index*))))
    (typecase state
      (function nil)
      (cons (cadr state)))))

(defun gf-dfun-info (gf)
  (let ((state (if (eq *boot-state* 'complete)
		   (gf-dfun-state gf)
		   (slot-ref (get-slots gf) *sgf-dfun-state-index*))))
    (typecase state
      (function nil)
      (cons (cddr state)))))

(defvar *sgf-name-index* 
  (bootstrap-slot-index 'standard-generic-function 'name))

(defun early-gf-name (gf)
  (slot-ref (get-slots gf) *sgf-name-index*))

(defun gf-lambda-list-from-method (method)
  (multiple-value-bind (parameters unspecialized-lambda-list)
      (parse-specialized-lambda-list (method-lambda-list* method))
    (declare (ignore parameters))
    (let ((aux (memq '&aux unspecialized-lambda-list)))
      (ldiff unspecialized-lambda-list aux))))

(defun gf-lambda-list (gf)
  (let ((arg-info (if (eq *boot-state* 'complete)
		      (gf-arg-info gf)
		      (early-gf-arg-info gf))))
    (if (eq :no-lambda-list (arg-info-lambda-list arg-info))
	(let ((methods (if (eq *boot-state* 'complete)
			   (generic-function-methods gf)
			   (early-gf-methods gf))))
	  (if (null methods)
	      (internal-error "~@<No way to determine the lambda list~@:>")
	      (gf-lambda-list-from-method (car (last methods)))))
	(arg-info-lambda-list arg-info))))

(defmacro real-ensure-gf-internal (gf-class all-keys env)
  `(progn
     (cond ((symbolp ,gf-class)
	    (setq ,gf-class (find-class ,gf-class t ,env)))
	   ((classp ,gf-class))
	   (t
	    (error "~@<The ~s argument (~S) was neither a class nor a ~
                    symbol naming a class.~@:>"
		   :generic-function-class ,gf-class)))
     (remf ,all-keys :generic-function-class)
     (remf ,all-keys :environment)
     (let ((combin (getf ,all-keys :method-combination '.shes-not-there.)))
       (unless (eq combin '.shes-not-there.)
	 (setf (getf ,all-keys :method-combination)
	       (find-method-combination (class-prototype ,gf-class)
					(car combin)
					(cdr combin)))))
     (let ((method-class (getf ,all-keys :method-class '.shes-not-there.)))
       (unless (eq method-class '.shes-not-there.)
	 (setf (getf ,all-keys :method-class)
	       (find-class method-class t ,env))))))

#+loadable-pcl
(progn
  (defmethod ensure-generic-function-using-class
      ((existing generic-function)
       function-specifier
       &rest all-keys
       &key environment
       (lambda-list nil lambda-list-p)
       (generic-function-class 'standard-generic-function gf-class-p)
       &allow-other-keys)
    (real-ensure-gf-internal generic-function-class all-keys environment)
    (unless (or (null gf-class-p)
		(eq (class-of existing) generic-function-class))
      (change-class existing generic-function-class))
    (prog1
	(apply #'reinitialize-instance existing all-keys)
      (when lambda-list-p
	(proclaim-defgeneric function-specifier lambda-list))))
     
  (defmethod ensure-generic-function-using-class
      ((existing null)
       function-specifier
       &rest all-keys
       &key environment (lambda-list nil lambda-list-p)
       (generic-function-class 'standard-generic-function)
       &allow-other-keys)
    (declare (ignore existing))
    (real-ensure-gf-internal generic-function-class all-keys environment)
    (prog1
	(setf (gdefinition function-specifier)
	      (apply #'make-instance generic-function-class 
		     :name function-specifier all-keys))
      (when lambda-list-p
	(proclaim-defgeneric function-specifier lambda-list)))))

;;;
;;; These two are like the methods above, but used during bootstrapping.
;;; During the bootstrapping procedure they are added as methods
;;; to ENSURE-GENERIC-FUNCTION-USING-CLASS.
;;;

#-loadable-pcl
(progn
  (defun real-ensure-gf-using-class--generic-function
      (existing
       function-specifier
       &rest all-keys
       &key environment (lambda-list nil lambda-list-p)
       (generic-function-class 'standard-generic-function gf-class-p)
       &allow-other-keys)
    (real-ensure-gf-internal generic-function-class all-keys environment)
    (unless (or (null gf-class-p)
		(eq (class-of existing) generic-function-class))
      (change-class existing generic-function-class))
    (prog1
	(apply #'reinitialize-instance existing all-keys)
      (when lambda-list-p
	(proclaim-defgeneric function-specifier lambda-list))))

  (defun real-ensure-gf-using-class--null
      (existing
       function-specifier
       &rest all-keys
       &key environment (lambda-list nil lambda-list-p)
       (generic-function-class 'standard-generic-function)
       &allow-other-keys)
    (declare (ignore existing))
    (real-ensure-gf-internal generic-function-class all-keys environment)
    (prog1
	(setf (gdefinition function-specifier)
	      (apply #'make-instance generic-function-class 
		     :name function-specifier all-keys))
      (when lambda-list-p
	(proclaim-defgeneric function-specifier lambda-list)))))



(defun get-generic-function-info (gf)
  ;; values   nreq applyp metatypes nkeys arg-info
  (multiple-value-bind (applyp metatypes arg-info)
      (let* ((arg-info (gf-arg-info* gf))
	     (metatypes (arg-info-metatypes arg-info)))
	(values (arg-info-applyp arg-info)
		metatypes
		arg-info))
    (values (length metatypes) applyp metatypes
	    (count-if (lambda (x) (neq x t)) metatypes)
	    arg-info)))

#-loadable-pcl
(defun early-make-a-method (class qualifiers arglist specializers initargs doc
			    &optional slot-name)
  (initialize-method-function initargs)
  (let ((parsed ())
	(unparsed ()))
    ;; Figure out whether we got class objects or class names as the
    ;; specializers and set parsed and unparsed appropriately.  If we
    ;; got class objects, then we can compute unparsed, but if we got
    ;; class names we don't try to compute parsed.
    ;; 
    ;; Note that the use of not symbolp in this call to every should be
    ;; read as 'classp' we can't use classp itself because it doesn't
    ;; exist yet.
    (if (every (lambda (s) (not (symbolp s))) specializers)
	(setq parsed specializers
	      unparsed (mapcar (lambda (s)
				 (if (eq s t) t (class-name s)))
			       specializers))
	(setq unparsed specializers
	      parsed ()))
    (list :early-method		  ;This is an early method dammit!
	  
	  (getf initargs :function)
	  (getf initargs :fast-function)
	  
	  parsed                  ;The parsed specializers.  This is used
				  ;by early-method-specializers to cache
				  ;the parse.  Note that this only comes
				  ;into play when there is more than one
				  ;early method on an early gf.
	  
	  (list class             ;A list to which real-make-a-method
		qualifiers        ;can be applied to make a real method
		arglist           ;corresponding to this early one.
		unparsed
		initargs
		doc
		slot-name)
	  )))

(defun #+loadable-pcl make-a-method #-loadable-pcl real-make-a-method
    (class qualifiers lambda-list specializers initargs doc
     &optional slot-name)
  (setq specializers (parse-specializers specializers))
  (apply #'make-instance class 
	 :qualifiers qualifiers
	 :lambda-list lambda-list
	 :specializers specializers
	 :documentation doc
	 :slot-name slot-name
	 :allow-other-keys t
	 initargs))

(defun early-method-function (early-method)
  (values (cadr early-method) (caddr early-method)))

(defun early-method-class (early-method)
  (find-class (car (fifth early-method))))

(defun early-method-standard-accessor-p (early-method)
  (let ((class (first (fifth early-method))))
    (or (eq class 'standard-reader-method)
        (eq class 'standard-writer-method)
        (eq class 'standard-boundp-method))))

(defun early-method-standard-accessor-slot-name (early-method)
  (seventh (fifth early-method)))

;;;
;;; Fetch the specializers of an early method.  This is basically just a
;;; simple accessor except that when the second argument is t, this converts
;;; the specializers from symbols into class objects.  The class objects
;;; are cached in the early method, this makes bootstrapping faster because
;;; the class objects only have to be computed once.
;;; NOTE:
;;;  the second argument should only be passed as T by early-lookup-method.
;;;  this is to implement the rule that only when there is more than one
;;;  early method on a generic function is the conversion from class names
;;;  to class objects done.
;;;  the corresponds to the fact that we are only allowed to have one method
;;;  on any generic function up until the time classes exist.
;;;  
(defun early-method-specializers (early-method &optional objectsp)
  (if (and (listp early-method)
	   (eq (car early-method) :early-method))
      (cond ((eq objectsp t)
	     (or (fourth early-method)
		 (setf (fourth early-method)
		       (mapcar #'find-class (cadddr (fifth early-method))))))
	    (t
	     (cadddr (fifth early-method))))
      (error "~S is not an early-method." early-method)))

(defun early-method-qualifiers (early-method)
  (cadr (fifth early-method)))

(defun early-method-lambda-list (early-method)
  (caddr (fifth early-method)))

(defun early-add-named-method (generic-function-name
			       qualifiers
			       specializers
			       arglist
			       &rest initargs)
  (let* ((gf (ensure-generic-function generic-function-name))
	 (existing
	   (dolist (m (early-gf-methods gf))
	     (when (and (equal (early-method-specializers m) specializers)
			(equal (early-method-qualifiers m) qualifiers))
	       (return m))))
	 (new (make-a-method 'standard-method
			     qualifiers
			     arglist
			     specializers
			     initargs
			     ())))
    (when existing (remove-method gf existing))
    (add-method gf new)
    new))

;;;
;;; This is the early version of add-method.  Later this will become a
;;; generic function.  See fix-early-generic-functions which has special
;;; knowledge about add-method.
;;;
#-loadable-pcl
(progn
  (defun add-method (generic-function method)
    (when (not (fsc-instance-p generic-function))
      (error "Early add-method didn't get a funcallable instance."))
    (when (not (and (listp method) (eq (car method) :early-method)))
      (error "Early add-method didn't get an early method."))
    (push method (early-gf-methods generic-function))
    (set-arg-info generic-function :new-method method)
    (unless (assoc (early-gf-name generic-function) *generic-function-fixups*
		   :test #'equal)
      (update-dfun generic-function))
    generic-function)

  ;;
  ;; This is the early version of remove method.
  ;;
  (defun remove-method (generic-function method)
    (when (not (fsc-instance-p generic-function))
      (error "Early remove-method didn't get a funcallable instance."))
    (when (not (and (listp method) (eq (car method) :early-method)))
      (error "Early remove-method didn't get an early method."))
    (setf (early-gf-methods generic-function)
	  (remove method (early-gf-methods generic-function)))
    (set-arg-info generic-function)
    (unless (assoc (early-gf-name generic-function) *generic-function-fixups*
		   :test #'equal)
      (update-dfun generic-function))
    generic-function)

  ;;
  ;; And the early version of get-method.
  ;;
  (defun get-method (generic-function qualifiers specializers
		     &optional (errorp t))
    (if (early-gf-p generic-function)
	(or (dolist (m (early-gf-methods generic-function))
	      (when (and (or (equal (early-method-specializers m nil)
				    specializers)
			     (equal (early-method-specializers m t)
				    specializers))
			 (equal (early-method-qualifiers m) qualifiers))
		(return m)))
	    (if errorp
		(error "Can't get early method.")
		nil))
	(real-get-method generic-function qualifiers specializers errorp))))

(defvar *fegf-debug-p* nil)

(defun fix-early-generic-functions (&optional (noisyp t *fegf-debug-p*))
  (declare (ignore noisyp))
  (let ((accessors nil))
    ;; Rearrange *early-generic-functions* to speed up fix-early-generic-functions.
    (dolist (early-gf-spec *early-generic-functions*)
      (when (every #'early-method-standard-accessor-p
		   (early-gf-methods (gdefinition early-gf-spec)))
	(push early-gf-spec accessors)))
    (dolist (spec (nconc accessors
			 '(accessor-method-slot-name
			   generic-function-methods
			   method-specializers
			   specializerp
			   specializer-type
			   specializer-class
			   slot-definition-location
			   slot-definition-name
			   class-slots
			   gf-arg-info
			   class-precedence-list
			   slot-boundp-using-class
			   (setf slot-value-using-class)
			   slot-value-using-class
			   structure-class-p
			   standard-class-p
			   funcallable-standard-class-p
			   specializerp)))
      (setq *early-generic-functions* 
	    (cons spec (delete spec *early-generic-functions* :test #'equal))))

    (dolist (early-gf-spec *early-generic-functions*)
      (let* ((gf (gdefinition early-gf-spec))
	     (methods (mapcar (lambda (early-method)
				(let ((args (copy-list (fifth early-method))))
				  (setf (fourth args)
					(early-method-specializers early-method t))
				  (apply #'real-make-a-method args)))
			      (early-gf-methods gf))))
	(setf (generic-function-method-class gf) *the-class-standard-method*)
	(setf (generic-function-method-combination gf) *standard-method-combination*)
	(set-methods gf methods)))
	  
    (dolist (fns *early-functions*)
      (setf (gdefinition (car fns)) (symbol-function (caddr fns))))
      
    (dolist (fixup *generic-function-fixups*)
      (let* ((fspec (car fixup))
	     (gf (gdefinition fspec))
	     (methods (mapcar (lambda (method)
				(let* ((lambda-list (first method))
				       (specializers (second method))
				       (method-fn-name (third method))
				       (fn-name (or method-fn-name fspec))
				       (fn (symbol-function fn-name))
				       (initargs 
					(list :function
					      (set-function-name
					       (lambda (args next-methods)
						 (declare (ignore next-methods))
						 (apply fn args))
					       `(call ,fn-name)))))
				  (declare (type function fn))
				  (make-a-method 'standard-method
						 ()
						 lambda-list
						 specializers
						 initargs
						 nil)))
			      (cdr fixup))))
	(setf (generic-function-method-class gf) *the-class-standard-method*)
	(setf (generic-function-method-combination gf) *standard-method-combination*)
	(set-methods gf methods)))))


;;;
;;; parse-defmethod is used by defmethod to parse the &rest argument into
;;; the 'real' arguments.  This is where the syntax of defmethod is really
;;; implemented.
;;; 
(defun parse-defmethod (form)
  (declare (list form))
  (loop with original-form = form and name = (pop form)
	while (and (car form) (atom (car form)))
	  collect (pop form) into qualifiers
	finally
	  (let ((lambda-list (pop form)))
	    (when (and (null lambda-list)
		       (consp (car form))
		       (consp (caar form)))
	      (error "~@<Qualifiers must be non-null atoms: ~s~@:>"
		     original-form))
	    (return (values name qualifiers lambda-list form)))))

(defun parse-specializers (specializers)
  (declare (list specializers))
  (flet ((parse (spec)
	   (let ((result (specializer-from-type spec)))
	     (if (specializerp result)
		 result
		 (if (symbolp spec)
		     (error "~@<~S used as a specializer, ~
                             but is not the name of a class.~@:>"
			    spec)
		     (error "~S is not a legal specializer." spec))))))
    (mapcar #'parse specializers)))

(defun unparse-specializers (specializers-or-method)
  (if (listp specializers-or-method)
      (flet ((unparse (spec)
	       (if (specializerp spec)
                   (let ((type (specializer-type spec)))
                     (if (and (consp type)
                              (eq (car type) 'class))
                         (let* ((class (cadr type))
                                (class-name (class-name class)))
                           (if (eq class (find-class class-name nil))
                               class-name
                               type))
                         type))
		   (error "~S is not a legal specializer." spec))))
	(mapcar #'unparse specializers-or-method))
      (unparse-specializers (method-specializers specializers-or-method))))

(defun parse-method-or-spec (spec &optional (errorp t))
  (let (gf method name temp)
    (if (method-p spec)	
	(setq method spec
	      gf (method-generic-function method)
	      temp (and gf (generic-function-name gf))
	      name (if temp
		       (make-method-spec temp (method-qualifiers method)
					 (unparse-specializers
					  (method-specializers method)))
		       (make-symbol (format nil "~S" method))))
	(multiple-value-bind (gf-spec quals specls)
	    (parse-defmethod spec)
	  (and (setq gf (and (or errorp (fboundp gf-spec))
			     (gdefinition gf-spec)))
	       (let ((nreq (arg-info-number-required
			    (if (eq *boot-state* 'complete)
				(gf-arg-info gf)
				(early-gf-arg-info gf)))))
		 (setq specls (append (parse-specializers specls)
				      (make-list (- nreq (length specls))
						 :initial-element
						 *the-class-t*)))
		 (and 
		   (setq method (get-method gf quals specls errorp))
		   (setq name (make-method-spec gf-spec quals
						(unparse-specializers specls))))))))
    (values gf method name)))


;;;
;;; The next two functions are part of AMOP.
;;;
(defun extract-lambda-list (specialized-lambda-list)
  (multiple-value-bind (ignore1 lambda-list ignore2)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2))
    lambda-list))

(defun extract-specializer-names (specialized-lambda-list)
  (multiple-value-bind (ignore1 ignore2 specializers)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore ignore1 ignore2))
    specializers))

(defun parse-specialized-lambda-list (lambda-list)
  (multiple-value-bind (required optional restp rest keyp keys
				 allow-other-keys-p aux)
      (parse-lambda-list lambda-list t)
    (collect ((req) (spec))
      (dolist (x required)
	(req (if (consp x) (car x) x))
	(spec (if (consp x) (cadr x) t)))
      (values (mapcar (lambda (x) (if (consp x) (car x) x))
		      (append required optional (and restp (list rest))
			      keys aux))
	      `(,@(req)
		  ,@(when optional `(&optional ,@optional))
		  ,@(when restp `(&rest ,rest))
		  ,@(when keyp `(&key ,@keys))
		  ,@(when allow-other-keys-p `(&allow-other-keys))
		  ,@(when aux `(&aux ,@aux)))
	      (spec)
	      (req)))))


#-loadable-pcl
(eval-when (:load-toplevel :execute)
  (/show "PCL boot state EARLY")
  (setq *boot-state* 'early))

(defmacro with-slots (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       ,@(let ((instance (if (and (consp instance)
				  (eq (car instance) 'the))
                             (third instance)
                             instance)))
	   (and (symbolp instance)
                `((declare (variable-rebinding ,in ,instance)))))
       (symbol-macrolet ,(mapcar (lambda (slot-entry)
				   (let ((variable-name 
					  (if (symbolp slot-entry)
					      slot-entry
					      (car slot-entry)))
					 (slot-name
					  (if (symbolp slot-entry)
					      slot-entry
					      (cadr slot-entry))))
				     `(,variable-name
				       (slot-value ,in ',slot-name))))
				 slots)
			,@body))))

(defmacro with-accessors (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       ,@(let ((instance (if (and (consp instance)
				  (eq (car instance) 'the))
                             (third instance)
                             instance)))
	   (and (symbolp instance)
                `((declare (variable-rebinding ,in ,instance)))))
       (symbol-macrolet ,(mapcar (lambda (slot-entry)
				   (let ((variable-name (car slot-entry))
					 (accessor-name (cadr slot-entry)))
				     `(,variable-name
				       (,accessor-name ,in))))
			       slots)
          ,@body))))
