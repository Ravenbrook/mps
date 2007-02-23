;;;-*-Mode:LISP; Package:PCL  -*-
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

(in-package :pcl)

#-(or loadable-pcl bootable-pcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (eq *boot-state* 'complete)
    (error "~@<Trying to load (or compile) PCL in an environment in which it ~
            has already been loaded.  This doesn't work, you will have to ~
            get a fresh lisp (reboot) and then load PCL.~@:>"))
  
  (when *boot-state*
    (cerror "Try loading (or compiling) PCL anyways."
	    "~@<Trying to load (or compile) PCL in an environment in which it ~
             has already been partially loaded.  This may not work, you may ~
             need to get a fresh lisp (reboot) and then load PCL.~@:>")))

;;;
;;; These are retained only for backward compatibility.  They
;;; are no longer used, and may be deleted at some point.
;;;
(defvar *defclass-times*   () "Obsolete, don't use.")
(defvar *defmethod-times*  () "Obsolete, don't use.")
(defvar *defgeneric-times* () "Obsolete, don't use.")


;;;
;;; If symbol names a function which is traced or advised, return the
;;; unadvised, traced etc. definition.  This lets me get at the generic
;;; function object even when it is traced.
;;;
;;; Note that FDEFINITION takes care of encapsulations.  PROFILE
;;; isn't using encapsulations, so it has to be treated specially.
;;;
(declaim (inline gdefinition))

(defun gdefinition (name)
  (fdefinition name))

;;;
;;; If symbol names a function which is traced or advised, redefine
;;; the `real' definition without affecting the advise.
;;
(defun (setf gdefinition) (new-definition name)
  (c::%%defun name new-definition nil)
  (c::note-name-defined name :function)
  new-definition)


(declaim (special *the-class-t* 
		  *the-class-vector* *the-class-symbol*
		  *the-class-string* *the-class-sequence*
		  *the-class-rational* *the-class-ratio*
		  *the-class-number* *the-class-null* *the-class-list*
		  *the-class-integer* *the-class-float* *the-class-cons*
		  *the-class-complex* *the-class-character*
		  *the-class-bit-vector* *the-class-array*
		  *the-class-stream*

		  *the-class-slot-object*
		  *the-class-structure-object*
		  *the-class-standard-object*
		  *the-class-funcallable-standard-object*
		  *the-class-class*
		  *the-class-generic-function*
		  *the-class-built-in-class*
		  *the-class-slot-class*
		  *the-class-std-class*
		  *the-class-condition-class*
		  *the-class-structure-class*
		  *the-class-standard-class*
		  *the-class-funcallable-standard-class*
		  *the-class-method*
		  *the-class-standard-method*
		  *the-class-standard-reader-method*
		  *the-class-standard-writer-method*
		  *the-class-standard-boundp-method*
		  *the-class-standard-generic-function*
		  *the-class-standard-effective-slot-definition*
		  
		  *the-eslotd-standard-class-slots*
		  *the-eslotd-funcallable-standard-class-slots*))

(declaim (special *the-wrapper-of-t*
		  *the-wrapper-of-vector* *the-wrapper-of-symbol*
		  *the-wrapper-of-string* *the-wrapper-of-sequence*
		  *the-wrapper-of-rational* *the-wrapper-of-ratio*
		  *the-wrapper-of-number* *the-wrapper-of-null*
		  *the-wrapper-of-list* *the-wrapper-of-integer*
		  *the-wrapper-of-float* *the-wrapper-of-cons*
		  *the-wrapper-of-complex* *the-wrapper-of-character*
		  *the-wrapper-of-bit-vector* *the-wrapper-of-array*))

;;;; Type specifier hackery:

;;; internal to this file.
(defun coerce-to-class (class &optional make-forward-referenced-class-p)
  (if (symbolp class)
      (or (find-class class (not make-forward-referenced-class-p))
	  (ensure-class class))
      class))

;;; Interface
(defun specializer-from-type (type &aux args)
  (when (consp type)
    (setq args (cdr type) type (car type)))
  (cond ((symbolp type)
	 (or (and (null args) (find-class type))
	     (ecase type
	       (class    (coerce-to-class (car args)))
	       (class-eq (class-eq-specializer (coerce-to-class (car args))))
	       (eql      (intern-eql-specializer (car args))))))
	((and (null args) (typep type 'kernel::class))
	 (or (kernel:%class-pcl-class type)
	     (ensure-non-standard-class (kernel:%class-name type))))
	((specializerp type) type)))

;;; interface
(defun type-from-specializer (specl)
  (cond ((eq specl t)
	 t)
	((consp specl)
         (unless (member (car specl) '(class class-eq eql))
           (error "~@<~S is not a legal specializer type.~@:>" specl))
         specl)
        ((progn
	   (when (symbolp specl)
	     ;;maybe (or (find-class specl nil) (ensure-class specl)) instead?
	     (setq specl (find-class specl)))
	   (or (not (eq *boot-state* 'complete))
	       (specializerp specl)))
	 (specializer-type specl))
        (t
         (error "~@<~s is neither a type nor a specializer.~@:>" specl))))

(defun type-class (type)
  (declare (special *the-class-t*))
  (setq type (type-from-specializer type))
  (if (atom type)
      (if (eq type t)
	  *the-class-t*
	  (internal-error "Bad argument to type-class."))
      (case (car type)
        (eql (class-of (cadr type)))
        (class-eq (cadr type))
        (class (cadr type)))))

(defun class-eq-type (class)
  (specializer-type (class-eq-specializer class)))

(defun inform-type-system-about-std-class (name)
  ;; This should only be called if metaclass is standard-class.
  ;; Compiler problems have been seen if the metaclass is 
  ;; funcallable-standard-class and this is called from the defclass macro
  ;; expander. However, bootstrap-meta-braid calls this for funcallable-
  ;; standard-class metaclasses but *boot-state* is not 'complete then.
  ;;
  ;; The only effect of this code is to ensure a lisp:standard-class class
  ;; exists so as to avoid undefined-function compiler warnings. The
  ;; skeleton class will be replaced at load-time with the correct object.
  ;; Earlier revisions (<= 1.17) of this function were essentially NOOPs.
  (declare (ignorable name))
  (when (and (eq *boot-state* 'complete)
	     (null (kernel::find-class name nil)))
    (setf (kernel::find-class name)
	  (kernel::make-standard-class :name name))))

;;; Internal to this file.
;;;
;;; These functions are a pale imitiation of their namesake.  They accept
;;; class objects or types where they should.
;;; 
(defun *normalize-type (type)
  (cond ((consp type)
         (if (member (car type) '(not and or))
             `(,(car type) ,@(mapcar #'*normalize-type (cdr type)))
             (if (null (cdr type))
                 (*normalize-type (car type))
                 type)))
        ((symbolp type)
         (let ((class (find-class type nil)))
           (if class
               (let ((type (specializer-type class)))
		 (if (listp type) type `(,type)))
               `(,type))))
        ((or (not (eq *boot-state* 'complete))
	     (specializerp type))
	 (specializer-type type))
        (t
         (error "~s is not a type." type))))

;;; internal to this file...
(defun convert-to-system-type (type)
  (case (car type)
    ((not and or) `(,(car type) ,@(mapcar #'convert-to-system-type
					  (cdr type))))
    ((class class-eq) ; class-eq is impossible to do right
     (kernel:layout-class (class-wrapper (cadr type))))
    (eql type)
    (t (if (null (cdr type))
	   (car type)
	   type))))


;;; *SUBTYPEP  --  Interface
;;;
;Writing the missing NOT and AND clauses will improve
;the quality of code generated by generate-discrimination-net, but
;calling subtypep in place of just returning (values nil nil) can be
;very slow.  *subtypep is used by PCL itself, and must be fast.
(defun *subtypep (type1 type2)
  (if (equal type1 type2)
      (values t t)
      (if (eq *boot-state* 'early)
	  (values (eq type1 type2) t)
	  (let ((*in-precompute-effective-methods-p* t)) 
	    (declare (special *in-precompute-effective-methods-p*))
            ;; *in-precompute-effective-methods-p* is not a good name.
	    ;; It changes the way class-applicable-using-class-p works.
	    (setq type1 (*normalize-type type1))
	    (setq type2 (*normalize-type type2))
	    (case (car type2)
	      (not
	       (values nil nil)) ; Should improve this.
	      (and
	       (values nil nil)) ; Should improve this.
	      ((eql wrapper-eq class-eq class)
	       (multiple-value-bind (app-p maybe-app-p)
		   (specializer-applicable-using-type-p type2 type1)
		 (values app-p (or app-p (not maybe-app-p)))))
	      (t
	       (subtypep (convert-to-system-type type1)
			 (convert-to-system-type type2))))))))


(defvar *built-in-class-symbols* ())
(defvar *built-in-wrapper-symbols* ())

(defun get-built-in-class-symbol (class-name)
  (or (cadr (assq class-name *built-in-class-symbols*))
      (let ((symbol (symbolicate* *the-pcl-package*
				  '*the-class- class-name '*)))
	(push (list class-name symbol) *built-in-class-symbols*)
	symbol)))

(defun get-built-in-wrapper-symbol (class-name)
  (or (cadr (assq class-name *built-in-wrapper-symbols*))
      (let ((symbol (symbolicate* *the-pcl-package*
				  '*the-wrapper-of- class-name '*)))
	(push (list class-name symbol) *built-in-wrapper-symbols*)
	symbol)))




(pushnew 'class *variable-declarations*)
(pushnew 'variable-rebinding *variable-declarations*)

(defvar *name->class->slotd-table* (make-hash-table))

(defun slot-name->class-table (slot-name)
  (or (gethash slot-name *name->class->slotd-table*)
      (setf (gethash slot-name *name->class->slotd-table*)
	    (make-hash-table :test 'eq :size 5))))

(defvar *standard-method-combination*)



(defun make-class-predicate-name (name)
  `(class-predicate ,name))

(defun plist-value (object name)
  (getf (object-plist object) name))

(defun (setf plist-value) (new-value object name)
  (if new-value
      (setf (getf (object-plist object) name) new-value)
      (progn
        (remf (object-plist object) name)
        nil)))



(defvar *built-in-classes*
  ;;
  ;; name       supers     subs                     cdr of cpl
  ;; prototype
  '(;(t         ()         (number sequence array character symbol) ())
    (number     (t)        (complex float rational) (t))
    (complex    (number)   ()                       (number t)
     #c(1 1))
    (float      (number)   ()                       (number t)
     1.0)
    (rational   (number)   (integer ratio)          (number t))
    (integer    (rational) ()                       (rational number t)
     1)
    (ratio      (rational) ()                       (rational number t)
     1/2)

    (sequence   (t)        (list vector)            (t))
    (list       (sequence) (cons null)              (sequence t))
    (cons       (list)     ()                       (list sequence t)
     (nil))
    

    (array      (t)        (vector)                 (t)
     #2A((NIL)))
    (vector     (array
		 sequence) (string bit-vector)      (array sequence t)
     #())
    (string     (vector)   ()                       (vector array sequence t)
     "")
    (bit-vector (vector)   ()                       (vector array sequence t)
     #*1)
    (character  (t)        ()                       (t)
     #\c)
   
    (symbol     (t)        (null)                   (t)
     symbol)
    (null       (symbol 
		 list)     ()                       (symbol list sequence t)
     nil)))

(labels ((direct-supers (class)
	   (if (typep class 'kernel::built-in-class)
	       (kernel:built-in-class-direct-superclasses class)
	       (let ((inherits (kernel:layout-inherits
				(kernel:%class-layout class))))
		 (list (svref inherits (1- (length inherits)))))))
	 (direct-subs (class)
	   (collect ((res))
	     (let ((subs (kernel:%class-subclasses class)))
	       (when subs
		 (do-hash (sub v subs)
		   (declare (ignore v))
		   (when (member class (direct-supers sub))
		     (res sub)))))
	     (res))))
  (collect ((res))
    (dolist (bic kernel::built-in-classes)
      (let* ((name (car bic))
	     (class (kernel::find-class name)))
	(unless (member name '(t kernel:instance kernel:funcallable-instance
				 function stream))
	  (res `(,name
		 ,(mapcar #'kernel:%class-name (direct-supers class))
		 ,(mapcar #'kernel:%class-name (direct-subs class))
		 ,(let ((found (assoc name *built-in-classes*)))
		    (if found (fifth found) 42)))))))
    (setq *built-in-classes* (res))))


;;;
;;; The classes that define the kernel of the metabraid.
;;;
(defclass t () ()
  (:metaclass built-in-class))

(defclass kernel:instance (t) ()
  (:metaclass built-in-class))

(defclass function (t) ()
  (:metaclass built-in-class))

(defclass kernel:funcallable-instance (function) ()
  (:metaclass built-in-class))

(defclass stream (kernel:instance) ()
  (:metaclass built-in-class))

(defclass slot-object (t) ()
  (:metaclass slot-class))

;;;
;;; In a host Lisp with intact PCL, the DEFCLASS below would normally
;;; generate a DEFSTRUCT with :INCLUDE SLOT-OBJECT.  SLOT-OBJECT is
;;; not a structure, so this would give an error.  Likewise,
;;; KERNEL:INSTANCE is a BUILT-IN-CLASS, not a structure class, so
;;; this would give an error, too.
;;;
;;; When PCL is bootstrapped normally, *BOOT-STATE* is not COMPLETE at
;;; this point, which means that a DEFSTRUCT is not done, because
;;; EXPAND-DEFCLASS looks at the boot state.
;;;
;;; I've modified EXPAND-DEFCLASS accordingly to not do a DEFSTRUCT
;;; when a loadable or bootable PCL is built.
;;;
(defclass structure-object (slot-object kernel:instance) ()
  (:metaclass structure-class))

(defstruct (dead-beef-structure-object
	     (:constructor |STRUCTURE-OBJECT class constructor|)))

(defclass standard-object (slot-object) ())
(defclass metaobject (standard-object) ())

(defclass funcallable-standard-object (standard-object
				       kernel:funcallable-instance)
  ()
  (:metaclass funcallable-standard-class))

(defclass specializer (metaobject) 
  ((type
    :initform nil
    :reader specializer-type)))

(defclass definition-source-mixin (standard-object)
  ((source
    :initform *load-pathname*
    :reader definition-source
    :initarg :definition-source)))

(defclass plist-mixin (standard-object)
  ((plist
    :initform ()
    :accessor object-plist)))

(defclass documentation-mixin (plist-mixin) ())

(defclass dependent-update-mixin (plist-mixin) ())

;;;
;;; The class CLASS is a specified basic class.  It is the common superclass
;;; of any kind of class.  That is any class that can be a metaclass must
;;; have the class CLASS in its class precedence list.
;;; 
(defclass class (documentation-mixin dependent-update-mixin
				     definition-source-mixin
				     specializer
				     kernel:instance)
  ((name
    :initform nil
    :initarg  :name
    :accessor class-name)
   (class-eq-specializer
    :initform nil
    :reader class-eq-specializer)
   (direct-superclasses
    :initform ()
    :reader class-direct-superclasses)
   (direct-subclasses
    :initform ()
    :reader class-direct-subclasses)
   (direct-methods
    :initform (cons nil nil))
   (predicate-name
    :initform nil
    :reader class-predicate-name)
   (finalized-p
    :initform nil
    :reader class-finalized-p)))

;;;
;;; The class PCL-CLASS is an implementation-specific common superclass of
;;; all specified subclasses of the class CLASS.
;;; 
(defclass pcl-class (class)
  ((class-precedence-list
    :reader class-precedence-list)
   (can-precede-list
    :initform ()
    :reader class-can-precede-list)
   (incompatible-superclass-list
    :initform ()
    :accessor class-incompatible-superclass-list)
   (wrapper
    :initform nil
    :reader class-wrapper)
   (prototype
    :initform nil
    :reader class-prototype)))

(defclass slot-class (pcl-class)
  ((direct-slots
    :initform ()
    :accessor class-direct-slots)
   (slots
    :initform ()
    :accessor class-slots)))

;;;
;;; The class STD-CLASS is an implementation-specific common superclass of
;;; the classes STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.
;;; 
(defclass std-class (slot-class) ())

(defclass standard-class (std-class) ())

(defclass funcallable-standard-class (std-class) ())
    
(defclass forward-referenced-class (pcl-class) ())

(defclass built-in-class (pcl-class) ())

(defclass structure-class (slot-class)
  ((defstruct-form
     :initform ()
     :accessor class-defstruct-form)
   (defstruct-constructor
     :initform nil
     :accessor class-defstruct-constructor)
   (from-defclass-p
    :initform nil
    :initarg :from-defclass-p)))
     
(defclass condition (slot-object kernel:instance) ()
  (:metaclass condition-class))

(defclass condition-class (slot-class) ())

(defclass specializer-with-object (specializer) ())

(defclass exact-class-specializer (specializer) ())

;;;
;;; Extension specializing on the exact specified class.  You must set
;;; pcl::*allow-experimental-specializers-p* to use this extension.
;;;
;;; (defclass foo () ())
;;; (defclass bar (foo) ())
;;;
;;; (setq pcl::*allow-experimental-specializers-p* t)
;;; (defmethod m (x) nil)
;;; (defmethod m ((x (pcl::class-eq 'foo))) t)
;;;
;;; (m (make-instance 'foo)) => t
;;; (m (make-instance 'bar)) => nil
;;;

(defclass class-eq-specializer (exact-class-specializer
				specializer-with-object)
  ((object
    :initarg :class
    :reader specializer-class
    :reader specializer-object)))

(defclass eql-specializer (exact-class-specializer specializer-with-object)
  ((object
    :initarg :object
    :reader specializer-object 
    :reader eql-specializer-object)))

(defvar *eql-specializer-table* (make-hash-table :test 'eql))

;;;
;;; When compiled with an intact PCL, the MAKE-INSTANCE in the function
;;; below will generate an optimized constructor, and a LOAD-TIME-VALUE
;;; creating it.  That means CTOR must be initialized before this file
;;; is.
;;;
(defun intern-eql-specializer (object)
  (or (gethash object *eql-specializer-table*)
      (setf (gethash object *eql-specializer-table*)
	    (make-instance 'eql-specializer :object object))))


;;;
;;; Slot definitions.
;;;
(defclass slot-definition (metaobject) 
  ((name
    :initform nil
    :initarg :name
    :accessor slot-definition-name)
   (initform
    :initform nil
    :initarg :initform
    :accessor slot-definition-initform)
   (initfunction
    :initform nil
    :initarg :initfunction
    :accessor slot-definition-initfunction)
   (readers
    :initform nil
    :initarg :readers
    :accessor slot-definition-readers)
   (writers
    :initform nil
    :initarg :writers
    :accessor slot-definition-writers)
   (initargs
    :initform nil
    :initarg :initargs
    :accessor slot-definition-initargs)
   (type
    :initform t
    :initarg :type
    :accessor slot-definition-type)
   (documentation
    :initform ""
    :initarg :documentation)
   (class
    :initform nil
    :initarg :class
    :accessor slot-definition-class)))

(defclass standard-slot-definition (slot-definition)
  ((allocation
    :initform :instance
    :initarg :allocation
    :accessor slot-definition-allocation)
   (allocation-class
    :documentation "For class slots, the class defininig the slot.
For inherited class slots, this is the superclass from which the slot
was inherited."
    :initform nil
    :initarg :allocation-class
    :accessor slot-definition-allocation-class)))

(defclass structure-slot-definition (slot-definition)
  ((defstruct-accessor-symbol 
     :initform nil
     :initarg :defstruct-accessor-symbol
     :accessor slot-definition-defstruct-accessor-symbol)
   (internal-reader-function 
     :initform nil
     :initarg :internal-reader-function
     :accessor slot-definition-internal-reader-function)
   (internal-writer-function 
     :initform nil
     :initarg :internal-writer-function
     :accessor slot-definition-internal-writer-function)))

(defclass condition-slot-definition (standard-slot-definition)
  ())

(defclass direct-slot-definition (slot-definition)
  ())

(defclass effective-slot-definition (slot-definition)
  ((reader-function ; (lambda (object) ...)
    :accessor slot-definition-reader-function)
   (writer-function ; (lambda (new-value object) ...)
    :accessor slot-definition-writer-function)
   (boundp-function ; (lambda (object) ...)
    :accessor slot-definition-boundp-function)
   (accessor-flags
    :initform 0)))

(defclass standard-direct-slot-definition (standard-slot-definition
					   direct-slot-definition)
  ())

(defclass standard-effective-slot-definition (standard-slot-definition
					      effective-slot-definition)
  ((location
    :initform nil
    :accessor slot-definition-location)))

(defclass structure-direct-slot-definition (structure-slot-definition
					    direct-slot-definition)
  ())

(defclass condition-direct-slot-definition (condition-slot-definition
					    direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition
					       effective-slot-definition)
  ())

(defclass condition-effective-slot-definition (condition-slot-definition
					       effective-slot-definition)
  ())

(defclass method (metaobject) ())

(defclass standard-method (method definition-source-mixin documentation-mixin)
  ((generic-function
    :initform nil	
    :accessor method-generic-function)
   (specializers
    :initform ()
    :initarg  :specializers
    :reader method-specializers)
   (lambda-list
    :initform ()
    :initarg  :lambda-list
    :reader method-lambda-list)
   (function
    :initform nil
    :initarg :function)	
   (fast-function
    :initform nil
    :initarg :fast-function
    :reader method-fast-function)))

(defclass standard-accessor-method (standard-method)
  ((slot-name
    :initform nil
    :initarg :slot-name
    :reader accessor-method-slot-name)
   (slot-definition
    :initform nil
    :initarg :slot-definition
    :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())

(defclass standard-writer-method (standard-accessor-method) ())

(defclass standard-boundp-method (standard-accessor-method) ())

(defclass generic-function (dependent-update-mixin
			    definition-source-mixin
			    documentation-mixin
			    metaobject
			    funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))
    
(defclass standard-generic-function (generic-function)
  ((name
    :initform nil
    :initarg :name
    :accessor generic-function-name)
   (methods
    :initform ()
    :accessor generic-function-methods)
   (method-class
    :initarg :method-class
    :accessor generic-function-method-class)
   (method-combination
    :initarg :method-combination
    :accessor generic-function-method-combination)
   (arg-info
    :initform (make-arg-info)
    :reader gf-arg-info)
   (dfun-state
    :initform ()
    :accessor gf-dfun-state)
   (pretty-arglist
    :initform ()
    :accessor gf-pretty-arglist)
   (declarations
    :initform ()
    :initarg :declarations
    :reader generic-function-declarations))
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class *the-class-standard-method*
    :method-combination *standard-method-combination*))

(defclass method-combination (metaobject) ())

(defclass standard-method-combination
    (definition-source-mixin method-combination)
  ((type
    :reader method-combination-type
    :initarg :type)
   (documentation
    :reader method-combination-documentation
    :initarg :documentation)
   (options
    :reader method-combination-options
    :initarg :options)))

(defclass long-method-combination (standard-method-combination)
  ((function
    :initarg :function
    :reader long-method-combination-function)
   (args-lambda-list
    :initarg :args-lambda-list
    :reader long-method-combination-args-lambda-list)))

(defclass seal (standard-object)
  ((quality
    :initarg :quality
    :reader seal-quality)))

(defparameter *early-class-predicates*
  '((specializer specializerp)
    (exact-class-specializer exact-class-specializer-p)
    (class-eq-specializer class-eq-specializer-p)
    (eql-specializer eql-specializer-p)
    (class classp)
    (slot-class slot-class-p)
    (std-class std-class-p)
    (standard-class standard-class-p)
    (funcallable-standard-class funcallable-standard-class-p)
    (structure-class structure-class-p)
    (condition-class condition-class-p)
    (forward-referenced-class forward-referenced-class-p)
    (method method-p)
    (standard-method standard-method-p)
    (standard-accessor-method standard-accessor-method-p)
    (standard-reader-method standard-reader-method-p)
    (standard-writer-method standard-writer-method-p)
    (standard-boundp-method standard-boundp-method-p)
    (generic-function generic-function-p)
    (standard-generic-function standard-generic-function-p)
    (method-combination method-combination-p)
    (long-method-combination long-method-combination-p)))


