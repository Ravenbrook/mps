;;; Copyright (C) 2002, 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

;;; ***************
;;; Overview  *****
;;; ***************
;;;
;;; Compiler macro for MAKE-INSTANCE, and load-time generation of
;;; optimized instance constructor functions.
;;;
;;; ********************
;;; Entry Points  ******
;;; ********************
;;;
;;; UPDATE-CTORS must be called when methods are added/removed,
;;; classes are changed, etc., which affect instance creation.
;;;
;;; PRECOMPILE-CTORS can be called to precompile constructor functions
;;; for classes whose definitions are known at the time the function
;;; is called.

(file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/ctor.lisp,v 1.17 2004/08/06 17:21:54 rtoy Exp $")

(in-package "PCL")

;;; ******************
;;; Utilities  *******
;;; ******************

(defun quote-plist-keys (plist)
  (loop for (key . more) on plist by #'cddr
	if (null more) do
	  (error "Not a property list: ~S" plist)
	else
	  collect `(quote ,key)
	  and collect (car more)))


(defun plist-keys (plist &key test)
  (loop for (key . more) on plist by #'cddr
	if (null more) do
	  (error "~@<Not a property list: ~S.~@:>" plist)
	else if (or (null test) (funcall test key))
	  collect key))


;;; *****************
;;; CTORS   *********
;;; *****************
;;;
;;; Ctors are funcallable instances whose initial function is a
;;; function computing an optimized constructor function when called.
;;; When the optimized function is computed, the function of the
;;; funcallable instance is set to it.
;;;
(defstruct (ctor (:include pcl-funcallable-instance)
		 (:type kernel:funcallable-structure)
		 (:constructor %make-ctor))
  ;;
  ;; The function name whose function definition is this CTOR.
  (function-name nil :type list)
  ;;
  ;; The name of the class for which this CTOR constructs instances.
  ;; We have to know the name here because only when the constructor
  ;; function is actually called, we can assume that the class is
  ;; actually defined (and throw an error if not).
  (class-name nil :type symbol)
  ;;
  ;; The actual class object iff the optimized constructor function
  ;; has been built and set as the funcallable-instance function
  ;; of this ctor.
  (class nil :type (or null class))
  ;;
  ;; Property list of keywords and either constant values, from the
  ;; MAKE-INSTANCE form for which this CTOR was constructed, or
  ;; constructor function parameter names for non-constant values.
  (initargs () :type list)
  ;;
  ;; The state of the ctor object.  NIL if the the constructor function
  ;; hasn't been generated yet.  OPTIMIZED if an optimized constructor
  ;; function has been generated.  FALLBACK if an unoptimized constructor
  ;; function has been generated.
  (state 'initial :type (member initial optimized fallback)))

;;; List of all defined ctors.

(defvar *all-ctors* ())

(defun make-ctor-parameter-list (ctor)
  (loop for (key value) on (ctor-initargs ctor) by #'cddr
	unless (constantp value) collect value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function-name-syntax ctor (name)
    (when (symbolp (cadr name))
      (values t (cadr name))))
  
  (define-function-name-syntax make-instance (name)
    (when (symbolp (cadr name))
      (values t (cadr name)))))

(defun make-ctor-function-name (class-name initargs)
  (list* 'ctor class-name initargs))

;;;
;;; Reset CTOR to use a default function that will compute an
;;; optimized constructor function when called.
;;;
(defun install-initial-constructor (ctor &key force-p)
  (when (or force-p (ctor-class ctor))
    (setf (ctor-class ctor) nil)
    (setf (ctor-state ctor) 'initial)
    (setf (kernel:funcallable-instance-function ctor)
	  #'(kernel:instance-lambda (&rest args)
	      (install-optimized-constructor ctor)
	      (apply ctor args)))
    (setf (kernel:%funcallable-instance-info ctor 1)
	  (ctor-function-name ctor))))

;;;
;;; Keep this a separate function for testing.
;;;
(defun ensure-ctor (function-name class-name initargs)
  (unless (fboundp function-name)
    (make-ctor function-name class-name initargs)))

;;;
;;; Keep this a separate function for testing.
;;;
(defun make-ctor (function-name class-name initargs)
  (let ((ctor (%make-ctor :function-name function-name
			  :class-name class-name
			  :initargs initargs)))
    (push ctor *all-ctors*)
    (setf (fdefinition function-name) ctor)
    (install-initial-constructor ctor :force-p t)
    ctor))


;;; ***********************************************
;;; Compile-Time Expansion of MAKE-INSTANCE *******
;;; ***********************************************

(define-compiler-macro make-instance (&whole form &rest args)
  (declare (ignore args))
  (or (make-instance->constructor-call form)
      form))

(defun make-instance->constructor-call (form)
  (destructuring-bind (fn class-name &rest args) form
    (declare (ignore fn))
    (labels ((constant-symbol-p (form)
	       (and (constantp form)
		    (let ((constant (eval form)))
		      (and (symbolp constant)
			   (not (null (symbol-package constant)))))))
	     ;;
	     ;; Return the name of parameter number I of a constructor
	     ;; function.
	     (parameter-name (i)
	       (let ((ps #(.p0. .p1. .p2. .p3. .p4. .p5.)))
		 (if (array-in-bounds-p ps i)
		     (aref ps i)
		     (make-.variable. 'p i))))
	     ;;
	     ;; Check if CLASS-NAME is a constant symbol.  Give up if
	     ;; not.
	     (check-class ()
	       (unless (and class-name (constant-symbol-p class-name))
		 (return-from make-instance->constructor-call nil)))
	     ;;
	     ;; Check if ARGS are suitable for an optimized constructor.
	     ;; Return NIL from the outer function if not.
	     (check-args ()
	       (loop for (key . more) on args by #'cddr do
		       (when (or (null more)
				 (not (constant-symbol-p key))
				 (eq :allow-other-keys (eval key)))
			 (return-from make-instance->constructor-call nil)))))
      (check-class)
      (check-args)
      ;;
      ;; Collect a plist of initargs and constant values/parameter names
      ;; in INITARGS.  Collect non-constant initialization forms in
      ;; VALUE-FORMS.
      (multiple-value-bind (initargs value-forms)
	  (loop for (key value) on args by #'cddr and i from 0
		collect (eval key) into initargs
		if (constantp value)
		  collect value into initargs
		else
	          collect (parameter-name i) into initargs
		  and collect value into value-forms
		finally
		  (return (values initargs value-forms)))
	(let* ((class-name (eval class-name))
	       (function-name (make-ctor-function-name class-name initargs)))
	  ;;
	  ;; Prevent compiler warnings for calling the ctor.
	  (c::define-function-name function-name)
	  (when (eq (info function where-from function-name) :assumed)
	    (setf (info function where-from function-name) :defined)
	    (when (info function assumed-type function-name)
	      (setf (info function assumed-type function-name) nil)))
	  ;;
	  ;; Return code constructing a ctor at load time, which, when
	  ;; called, will set its funcallable instance function to an
	  ;; optimized constructor function.
	  `(let ((.x. (load-time-value
		       (ensure-ctor ',function-name ',class-name ',initargs))))
	     (declare (ignore .x.))
	     (funcall (function ,function-name) ,@value-forms)))))))


;;; **************************************************
;;; Load-Time Constructor Function Generation  *******
;;; **************************************************

;;;
;;; The system-supplied primary INITIALIZE-INSTANCE and
;;; SHARED-INITIALIZE methods.  One cannot initialized these variables
;;; to the right values here because said functions don't exist yet
;;; when this file is first loaded.
;;;
(defvar *the-system-ii-method* nil)
(defvar *the-system-si-method* nil)

(defun install-optimized-constructor (ctor)
   (labels ((lambda-name (ctor)
	      (cons 'make-instance
		    (cdr (ctor-function-name ctor))))
	    (install (optimized-p)
	      (without-package-locks
	       (multiple-value-bind (lambda fn)
		   (if optimized-p
		       (constructor-function-form ctor)
		       (fallback-generator ctor))
		 (setf (kernel:funcallable-instance-function ctor)
		       (or fn
			   (letf (((compiler-macro-function 'make-instance) nil))
			     (compile-lambda lambda :name (lambda-name ctor)))))))))
     (let ((class (find-class (ctor-class-name ctor) nil)))
       (setf (ctor-class ctor) class)
       (cond ((null class)
	      (install nil))
	     (t
	      (unless (class-finalized-p class)
		(finalize-inheritance class))
	      (pushnew ctor (plist-value class 'ctors))
	      (install (null *cold-boot-state*)))))))

(defun constructor-function-form (ctor)
  (let* ((class (ctor-class ctor))
	 (proto (class-prototype class))
         (make-instance-methods
	  (compute-applicable-methods #'make-instance (list class)))
         (allocate-instance-methods
	  (compute-applicable-methods #'allocate-instance (list class)))
         (ii-methods
	  (compute-applicable-methods #'initialize-instance (list proto)))
         (si-methods
	  (compute-applicable-methods #'shared-initialize (list proto t))))
    ;;
    ;; Cannot initialize these variables earlier because the generic
    ;; functions don't exist when PCL is built.
    (when (null *the-system-si-method*)
      (setq *the-system-si-method*
	    (find-method #'shared-initialize
			 () (list *the-class-slot-object* *the-class-t*)))
      (setq *the-system-ii-method*
	    (find-method #'initialize-instance
			 () (list *the-class-slot-object*))))
    ;;
    ;; Note that when there are user-defined applicable methods on
    ;; MAKE-INSTANCE and/or ALLOCATE-INSTANCE, these will show up
    ;; together with the system-defined ones in what
    ;; COMPUTE-APPLICABLE-METHODS returns.
    (or (and (not (condition-class-p class))
	     (null (cdr make-instance-methods))
	     (null (cdr allocate-instance-methods))
	     (null (check-initargs class (plist-keys (ctor-initargs ctor))
				   (append ii-methods si-methods) nil nil))
	     (not (around-or-nonstandard-primary-method-p
		   ii-methods *the-system-ii-method*))
	     (not (around-or-nonstandard-primary-method-p
		   si-methods *the-system-si-method*))
	     (optimizing-generator ctor ii-methods si-methods))
	(fallback-generator ctor))))

(defun around-or-nonstandard-primary-method-p
    (methods &optional standard-method)
  (loop with primary-checked-p = nil
	for method in methods
	as qualifiers = (method-qualifiers method)
	when (or (eq :around (car qualifiers))
		 (and (null qualifiers)
		      (not primary-checked-p)
		      (not (null standard-method))
		      (not (eq standard-method method))))
	  return t
	when (null qualifiers) do
	  (setq primary-checked-p t)))

(defun fallback-generator (ctor)
  (setf (ctor-state ctor) 'fallback)
  (if (null *cold-boot-state*)
      (if (ctor-class ctor)
	  `(kernel:instance-lambda ,(make-ctor-parameter-list ctor)
	     (make-instance ,(ctor-class ctor) ,@(ctor-initargs ctor)))
	  `(kernel:instance-lambda ,(make-ctor-parameter-list ctor)
	     (make-instance ',(ctor-class-name ctor) ,@(ctor-initargs ctor))))
      (let ((class (ctor-class-name ctor))
	    (params (copy-list (ctor-initargs ctor)))
	    (value-cells ()))
	(loop for cell on (cdr params) by #'cddr do
		(if (constantp (car cell))
		    (setf (car cell) (eval (car cell)))
		    (push cell value-cells)))
	(setq value-cells (nreverse value-cells))
	(values nil
		#'(kernel:instance-lambda (&rest args)
		    (dolist (cell value-cells)
		      (setf (car cell) (pop args)))
		    (apply #'make-instance class params))))))

(defun optimizing-generator (ctor ii-methods si-methods)
  (multiple-value-bind (body before-method-p)
      (fake-initialization-emf ctor ii-methods si-methods)
    (setf (ctor-state ctor) 'optimized)
    `(kernel:instance-lambda ,(make-ctor-parameter-list ctor)
       ,(wrap-in-allocate-forms ctor body before-method-p))))

;;;
;;; Return a form wrapped around BODY that allocates an instance
;;; constructed by CTOR.  BEFORE-METHOD-P set means we have to run
;;; before-methods, in which case we initialize instance slots to
;;; +SLOT-UNBOUND+.  The resulting form binds the local variables
;;; .INSTANCE. to the instance.  For non-structure instances,
;;; .SLOTS. is bound to the instance's slot vector around BODY.
;;;
(defun wrap-in-allocate-forms (ctor body before-method-p)
  (let ((class (ctor-class ctor)))
    (if (structure-class-p class)
	(let ((allocation-function (class-defstruct-constructor class)))
	  `(let ((.instance. (,allocation-function)))
	     ,body
	     .instance.))
	(let ((wrapper (class-wrapper class))
	      (allocation-function (raw-instance-allocator class))
	      (slots-fetcher (slots-fetcher class)))
	  (if (eq allocation-function 'allocate-standard-instance)
	      `(let ((.instance. (%%allocate-instance--class))
		     (.slots. (make-array ,(kernel:layout-length wrapper)
					  ,@(when before-method-p
					      '(:initial-element
						+slot-unbound+)))))
		 (setf (std-instance-wrapper .instance.) ,wrapper)
		 (setf (std-instance-slots .instance.) .slots.)
		 ,body
		 .instance.)
	      `(let* ((.instance. (,allocation-function ,wrapper))
		      (.slots. (,slots-fetcher .instance.)))
		 ,body
		 .instance.))))))

;;;
;;; Return a form that is sort of an effective method comprising all
;;; calls to INITIALIZE-INSTANCE and SHARED-INITIALIZE that would
;;; normally have taken place when calling MAKE-INSTANCE.
;;;
(defun fake-initialization-emf (ctor ii-methods si-methods)
  (multiple-value-bind (ii-around ii-before ii-primary ii-after)
      (standard-sort-methods ii-methods)
    (declare (ignore ii-primary))
    (multiple-value-bind (si-around si-before si-primary si-after)
	(standard-sort-methods si-methods)
      (declare (ignore si-primary))
      (assert (and (null ii-around) (null si-around)))
      (let ((initargs (ctor-initargs ctor))
	    (slot-inits (slot-init-forms ctor (or ii-before si-before))))
	(flet ((method-calls (methods args)
		 (loop for method in methods
		       as fn = (method-function method)
		       collect `(funcall (truly-the function ,fn) ,args ()))))
	  (values
	   `(let (,@(when (or ii-before ii-after)
		    `((.ii-args.
		       (list .instance. ,@(quote-plist-keys initargs)))))
		  ,@(when (or si-before si-after)
		    `((.si-args.
		       (list .instance. t ,@(quote-plist-keys initargs))))))
	      ,@(method-calls ii-before '.ii-args.)
	      ,@(method-calls si-before '.si-args.)
	      ,slot-inits
	      ,@(method-calls si-after '.si-args.)
	      ,@(method-calls ii-after '.ii-args.))
	   (or ii-before si-before)))))))

;;;
;;; Return four values from APPLICABLE-METHODS: around methods, before
;;; methods, the applicable primary method, and applicable after
;;; methods.  Before and after methods are sorted in the order they
;;; must be called.
;;;
(defun standard-sort-methods (applicable-methods)
  (loop for method in applicable-methods
	as qualifiers = (method-qualifiers method)
	if (null qualifiers)
	  collect method into primary
	else if (eq :around (car qualifiers))
	  collect method into around
	else if (eq :after (car qualifiers))
	  collect method into after
	else if (eq :before (car qualifiers))
	  collect method into before
	finally
	  (return (values around before (first primary) (nreverse after)))))


;;;
;;; Return the declared type of slot TYPE, or T if we don't want
;;; to use slot types.
;;;
(declaim (inline slot-type-or-t))
(defun slot-type-or-t (slot)
  (declare (special *use-slot-types-p*))
  (if *use-slot-types-p*
      (slot-definition-type slot)
      t))

;;;
;;; Return a form initializing instance and class slots of an object
;;; costructed by CTOR.  The variable .SLOTS. is assumed to bound to a
;;; non-structure instance's slot vector.  The variable .INSTANCE. is
;;; assumed to be bound in case of a structure instance.
;;;
;;; BEFORE-METHOD-P T means before-methods will be called, which, for
;;; not-structures, means that 1) other code will initialize instance
;;; slots to +SLOT-UNBOUND+ before the before-methods are run, and 2)
;;; that we have to check if these before-methods have set slots.
;;; For structures the consequences are likewise.
;;;
(defun slot-init-forms (ctor before-method-p)
  (let* ((class (ctor-class ctor))
	 (structure-p (structure-class-p class))
	 (initargs (ctor-initargs ctor))
	 (initkeys (plist-keys initargs))
	 (slot-vector
	  (unless structure-p
	    (make-array (kernel:layout-length (class-wrapper class))
			:initial-element nil)))
	 (class-inits ())
	 (structure-inits ())
	 (default-inits ())
	 (default-initargs (class-default-initargs class))
	 ;;
	 ;; Note that the locations are actually defstruct slot
	 ;; accessors for structures.
	 (locations (compute-initarg-locations
		     class
		     (append initkeys (mapcar #'car default-initargs)))))
    (labels ((locations (key)
	       (cdr (assoc key locations :test #'eq)))

	     (initialized-p (location)
	       (cond (structure-p
		      (assq location structure-inits))
		     ((integerp location)
		      (not (null (aref slot-vector location))))
		     (t
		      (assoc location class-inits :test #'eq))))

	     (class-init (location type val slot-type)
	       (assert (consp location))
	       (assert (not structure-p))
	       (unless (initialized-p location)
		 (push (list location type val slot-type) class-inits)))

	     (instance-init (location type val slot-type)
	       (unless (initialized-p location)
		 (cond (structure-p
			(assert (symbolp location))
			(push (list location type val slot-type)
			      structure-inits))
		       (t
			(assert (integerp location))
			(setf (aref slot-vector location)
			      (list type val slot-type))))))

	     (default-init-variable-name (i)
	       (let ((ps #(.d0. .d1. .d2. .d3. .d4. .d5.)))
		 (if (array-in-bounds-p ps i)
		     (aref ps i)
		     (make-.variable. 'd i)))))
      ;;
      ;; Loop over supplied initargs and values and record which
      ;; instance and class slots they initialize.
      (loop for (key value) on initargs by #'cddr
	    as type = (if (constantp value) 'constant 'param) do
	      (loop for (location . slot-type) in (locations key) do
		      (if (consp location)
			  (class-init location type value slot-type)
			  (instance-init location type value slot-type))))
      ;;
      ;; Loop over default initargs of the class, recording
      ;; initializations of slots that have not been initialized
      ;; above.  Default initargs which are not in the supplied
      ;; initargs are treated as if they were appended to supplied
      ;; initargs, that is, their values must be evaluated even
      ;; if not actually used for initializing a slot.
      (loop for (key initfn initform) in default-initargs and i from 0
	    unless (memq key initkeys) do
	      (let* ((type (if (constantp initform) 'constant 'var))
		     (init (if (eq type 'var) initfn initform)))
		(when (eq type 'var)
		  (let ((init-var (default-init-variable-name i)))
		    (setq init init-var)
		    (push (cons init-var initfn) default-inits)))
		(loop for (location . slot-type) in (locations key) do
			(if (consp location)
			    (class-init location type init slot-type)
			    (instance-init location type init slot-type)))))
      ;;
      ;; Loop over all slots of the class, filling in the rest from
      ;; slot initforms.
      (loop for slotd in (class-slots class)
	    as location
	      = (if structure-p
		    (slot-definition-defstruct-accessor-symbol slotd)
		    (slot-definition-location slotd))
	    as allocation = (slot-definition-allocation slotd)
	    as initfn = (slot-definition-initfunction slotd)
	    as slot-type = (slot-type-or-t slotd)
	    as initform = (slot-definition-initform slotd) 
	    if (eq allocation :class) do
	      (when (and initfn (not (initialized-p location)))
		(if (constantp initform)
		    (class-init location 'constant-if-unbound
				initform slot-type)
		    (class-init location 'initfn-if-unbound
				initfn slot-type)))
	    else do
	      (unless (or (and (not structure-p)
			       (null initfn))
			  (initialized-p location))
		(if (constantp initform)
		    (instance-init location 'initform initform slot-type)
		    (instance-init location 'initform/initfn initfn
				   slot-type))))
      ;;
      ;; Generate the forms for initializing instance and class slots.
      (let ((instance-init-forms
	     (delete nil
		     (if structure-p
			 (structure-init-forms structure-inits before-method-p)
			 (instance-init-forms slot-vector before-method-p))))
	    (class-init-forms
	     (unless structure-p
	       (collect ((forms))
		 (dolist (init class-inits (forms))
		   (destructuring-bind (location type value slot-type) init
		     (forms
		      (ecase type
			(constant
			 `(setf (cdr ',location)
				(the ,slot-type ',(eval value))))
			((param var)
			 `(setf (cdr ',location)
				(the ,slot-type ,value)))
			(initfn
			 `(setf (cdr ',location)
				(the ,slot-type (funcall ,value))))
			(initfn-if-unbound
			 `(when (eq +slot-unbound+ (cdr ',location))
			    (setf (cdr ',location)
				  (the ,slot-type (funcall ,value)))))
			(constant-if-unbound
			 `(when (eq +slot-unbound+ (cdr ',location))
			    (setf (cdr ',location)
				  (the ,slot-type ',(eval value)))))))))))))
	(multiple-value-bind (vars bindings)
	    (loop for (var . initfn) in (nreverse default-inits)
		  collect var into vars
		  collect `(,var (funcall ,initfn)) into bindings
		  finally (return (values vars bindings)))
	  `(let ,bindings
	     (declare (ignorable ,@vars) (optimize (safety 3)))
	     ,@instance-init-forms
	     ,@class-init-forms))))))

;;;
;;; Return an alist of lists (KEY LOCATION ...) telling, for each key
;;; in INITKEYS, which locations the initarg initializes.  For
;;; structures, defstruct slot accessor symbols are used as location.
;;; CLASS is the class of the instance being initialized.
;;;
(defun compute-initarg-locations (class initkeys)
  (let ((get-location-fn (if (structure-class-p class)
			     #'slot-definition-defstruct-accessor-symbol
			     #'slot-definition-location)))
    (loop with slots = (class-slots class)
	  for key in initkeys
	  collect
	    (loop for slot in slots
		  if (memq key (slot-definition-initargs slot))
		    collect (cons (funcall get-location-fn slot)
				  (slot-type-or-t slot))
		    into locations
		  else
		    collect slot into remaining-slots
		  finally
		    (setq slots remaining-slots)
		    (return (cons key locations))))))

;;;
;;; Return a list of forms for initializing instance slots as
;;; described by SLOT-VECTOR.  Each element of SLOT-VECTOR is a list
;;; (TYPE VALUE SLOT-TYPE).  See function SLOT-INIT-FORMS.
;;; BEFORE-METHOD-P true means before-methods are run; we can assume
;;; in that case that all slots are initialized to +SLOT-UNBOUND+ when
;;; the instance is allocated.
;;;
(defun instance-init-forms (slot-vector before-method-p)
  (loop for slot-entry across slot-vector and i from 0
	as (type value slot-type) = slot-entry
	collect
	  (ecase type
	    ((nil)
	     (unless before-method-p
	       `(setf (svref .slots. ,i) +slot-unbound+)))
	    ((param var)
	     `(setf (svref .slots. ,i) (the ,slot-type ,value)))
	    (initfn
	     `(setf (svref .slots. ,i)
		    (the ,slot-type (funcall ,value))))
	    (initform/initfn
	     (if before-method-p
		 `(when (eq (svref .slots. ,i) +slot-unbound+)
		    (setf (svref .slots. ,i)
			  (the ,slot-type (funcall ,value))))
		 `(setf (svref .slots. ,i)
			(the ,slot-type (funcall ,value)))))
	    (initform
	     (if before-method-p
		 `(when (eq (svref .slots. ,i) +slot-unbound+)
		    (setf (svref .slots. ,i)
			  (the ,slot-type ',(eval value))))
		 `(setf (svref .slots. ,i)
			(the ,slot-type ',(eval value)))))
	    (constant
	     `(setf (svref .slots. ,i)
		    (the ,slot-type ',(eval value)))))))

;;;
;;; Like INSTANCE-INIT-FORMS, but for a structure instance.
;;; INITS is a list describing the slots to initialize.
;;;
(defun structure-init-forms (inits before-method-p)
  (loop for (accessor type value slot-type) in inits
	collect
	  (ecase type
	    ((nil)
	     (if before-method-p
		 `(when (eq (,accessor .instance.) '.unbound.)
		    (setf (,accessor .instance.) nil))
		 `(setf (,accessor .instance.) nil)))
	    ((param var)
	     `(setf (,accessor .instance.) (the ,slot-type ,value)))
	    (initfn
	     `(setf (,accessor .instance.)
		    (the ,slot-type (funcall ,value))))
	    (initform/initfn
	     (if before-method-p
		 `(when (eq (,accessor .instance.) '.unbound.)
		    (setf (,accessor .instance.)
			  (the ,slot-type (funcall ,value))))
		 `(setf (,accessor .instance.)
			(the ,slot-type (funcall ,value)))))
	    (initform
	     (if before-method-p
		 `(when (eq (,accessor .instance.) '.unbound.)
		    (setf (,accessor .instance.)
			  (the ,slot-type ',(eval value))))
		 `(setf (,accessor .instance.)
			(the ,slot-type ',(eval value)))))
	    (constant
	     `(setf (,accessor .instance.)
		    (the ,slot-type ',(eval value)))))))


;;; *******************************
;;; External Entry Points  ********
;;; *******************************

(defun update-ctors (reason &key class name generic-function method)
  (labels ((reset (class &optional ri-cache-p (ctors-p t))
	     (when ctors-p
	       (dolist (ctor (plist-value class 'ctors))
		 (install-initial-constructor ctor)))
	     (when ri-cache-p
	       (setf (plist-value class 'ri-initargs) ()))
	     (dolist (subclass (class-direct-subclasses class))
	       (reset subclass ri-cache-p ctors-p))))
    (ecase reason
      ;;
      ;; CLASS must have been specified.
      (finalize-inheritance
       (reset class t))
      ;;
      ;; NAME must have been specified.
      (setf-find-class
       (loop for ctor in *all-ctors*
	     when (eq (ctor-class-name ctor) name) do
	     (when (ctor-class ctor)
	       (reset (ctor-class ctor)))
	     (loop-finish)))
      ;;
      ;; GENERIC-FUNCTION and METHOD must have been specified.
      ((add-method remove-method)
       (flet ((class-of-1st-method-param (method)
		(type-class (first (method-specializers method)))))
	 (case (generic-function-name generic-function)
	   ((make-instance allocate-instance initialize-instance
			   shared-initialize)
	    (reset (class-of-1st-method-param method) t t))
	   ((reinitialize-instance)
	    (reset (class-of-1st-method-param method) t nil))))))))

(defun precompile-ctors ()
  (dolist (ctor *all-ctors*)
    (when (null (ctor-class ctor))
      (let ((class (find-class (ctor-class-name ctor) nil)))
	(when (and class (class-finalized-p class))
	  (install-optimized-constructor ctor))))))

;;;
;;; Try to call a CTOR of class CLASS to construct an instance of CLASS
;;; with given initargs.  Value is an instance of CLASS if a suitable
;;; ctor is found, NIL otherwise.
;;;
;;; This is called from MAKE-INSTANCE.
;;;
(defun call-ctor (class initargs)
  (flet (;;
	 ;; Return two values ARGS, MATCH-P.  ARGS is a list of values
	 ;; from INITARGS with which the ctor can be invoked.  MATCH-P
	 ;; true means the ctor can be used.
	 (call-args (ctor)
	   (collect ((args))
	     (let ((ctail (ctor-initargs ctor))
		   (itail initargs))
	       (loop
		  (when (or (null ctail) (null itail))
		    (return (values (args) (and (null ctail) (null itail)))))
		  (unless (eq (pop ctail) (pop itail))
		    (return nil))
		  (let ((ival (pop itail)) (cval (pop ctail)))
		    (if (constantp cval)
			(unless (eql cval ival)
			  (return nil))
			(args ival))))))))
    ;;
    ;; Loop over all ctors of CLASS looking for a ctor that can be
    ;; used to construct an instance with the given initargs.  If one
    ;; is found, invoke it and return its value.
    (dolist (ctor (plist-value class 'ctors))
      (when (eq (ctor-state ctor) 'optimized)
	(multiple-value-bind (args match-p)
	    (call-args ctor)
	  (when match-p
	    (return (apply ctor args))))))))

;;; 
;;; REINITIALIZE-INSTANCE initargs checking with memoization.
;;; INSTANCE is the instance being reinitialized, INITARGS are the
;;; initargs passed to REINITIALIZE-INSTANCE.
;;;
;;; The function UPDATE-CTORS clears the memoization cache.
;;;
(defun check-ri-initargs (instance initargs)
  (let* ((class (class-of instance))
	 (keys (plist-keys initargs))
	 (cached (assoc keys (plist-value class 'ri-initargs)
			:test #'equal))
	 (invalid-keys
	  (if (consp cached)
	      (cdr cached)
	      (let ((invalid (check-initargs
			      class initargs
			      (list (list* 'reinitialize-instance
					   instance initargs)
				    (list* 'shared-initialize
					   instance nil initargs))
			      t nil)))
		(setf (plist-value class 'ri-initargs)
		      (acons keys invalid cached))
		invalid))))
    (when invalid-keys
      (invalid-initargs-error class invalid-keys))))

;;; end of ctor.lisp
