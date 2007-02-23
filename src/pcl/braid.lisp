;;;-*-Mode:LISP; Package:(PCL (LISP WALKER)); Base:10; Syntax:Common-lisp -*-
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
  "$Header: /project/cmucl/cvsroot/src/pcl/braid.lisp,v 1.48 2003/08/25 20:10:41 gerd Exp $")

;;;
;;; Bootstrapping the meta-braid.
;;;
;;; The code in this file takes the early definitions that have been saved
;;; up and actually builds those class objects.  This work is largely driven
;;; off of those class definitions, but the fact that STANDARD-CLASS is the
;;; class of all metaclasses in the braid is built into this code pretty
;;; deeply.
;;;
;;; 

(in-package :pcl)

(defun allocate-standard-instance (wrapper &optional (slots-init nil slots-init-p))
  (declare #.*optImize-speed*)
  (let* ((no-of-slots (kernel:layout-length wrapper))
	 (instance (%%allocate-instance--class))
	 (slots (make-array no-of-slots)))
    (declare (fixnum no-of-slots))
    (if slots-init-p
	(loop for i below no-of-slots
	      for init-value in slots-init do
		(setf (%svref slots i) init-value))
	(loop for i below no-of-slots do
		(setf (%svref slots i) +slot-unbound+)))
    (setf (std-instance-wrapper instance) wrapper)
    (setf (std-instance-slots instance) slots)
    instance))

(defmacro allocate-funcallable-instance-slots (wrapper &optional 
					       slots-init-p slots-init)
  `(let ((no-of-slots (wrapper-no-of-instance-slots ,wrapper)))
     ,(if slots-init-p
	  `(if ,slots-init-p
	       (make-array no-of-slots :initial-contents ,slots-init)
	       (make-array no-of-slots :initial-element +slot-unbound+))
	  `(make-array no-of-slots :initial-element +slot-unbound+))))

(defun allocate-funcallable-instance (wrapper &optional
				      (slots-init nil slots-init-p))
  (let ((fin (allocate-funcallable-instance-1)))
    (set-funcallable-instance-function
     fin
     #'(kernel:instance-lambda (&rest args)
         (declare (ignore args))
	 (error "~@<The function of the funcallable instance ~S ~
                 has not been set.~@:>"
		fin)))
    (setf (fsc-instance-wrapper fin) wrapper
	  (fsc-instance-slots fin) (allocate-funcallable-instance-slots
				    wrapper slots-init-p slots-init))
    fin))


;;;
;;; bootstrap-meta-braid
;;;
;;; This function builds the base metabraid from the early class definitions.
;;;   
(defmacro initial-classes-and-wrappers (&rest classes)
  `(progn
     ,@(mapcar (lambda (class)
		 (let ((wr (symbolicate* *the-pcl-package* class '-wrapper)))
		   `(setf ,wr ,(if (eq class 'standard-generic-function)
				   '*sgf-wrapper*
				   `(boot-make-wrapper
				     (early-class-size ',class)
				     ',class))
			  ,class (allocate-standard-instance
				  ,(if (eq class 'standard-generic-function)
				       'funcallable-standard-class-wrapper
				       'standard-class-wrapper))
			  (wrapper-class ,wr) ,class
			  (find-class ',class) ,class)))
	      classes)))		        

(defun set-class-translation (class name)
  (let ((kernel-class (kernel::find-class name nil)))
    (etypecase kernel-class
      (null)
      (kernel::built-in-class
       (let ((translation (kernel::built-in-class-translation kernel-class)))
	 (setf (info type translator class)
	       (if translation
		   (lambda (spec) (declare (ignore spec)) translation)
		   (lambda (spec) (declare (ignore spec)) kernel-class)))))
      (kernel::class
       (setf (info type translator class)
	     (lambda (spec) (declare (ignore spec)) kernel-class))))))

(defun bootstrap-meta-braid ()
  (let* ((*create-classes-from-internal-structure-definitions-p* nil)
	 standard-class-wrapper standard-class
	 funcallable-standard-class-wrapper funcallable-standard-class
	 slot-class-wrapper slot-class
	 built-in-class-wrapper built-in-class
	 structure-class-wrapper structure-class
	 condition-class-wrapper condition-class
	 standard-direct-slot-definition-wrapper standard-direct-slot-definition
	 standard-effective-slot-definition-wrapper standard-effective-slot-definition
	 class-eq-specializer-wrapper class-eq-specializer
	 standard-generic-function-wrapper standard-generic-function)
    (initial-classes-and-wrappers 
     standard-class funcallable-standard-class
     slot-class built-in-class structure-class condition-class
     standard-direct-slot-definition standard-effective-slot-definition 
     class-eq-specializer standard-generic-function)
    ;;
    ;; First, make a class metaobject for each of the early classes.  For
    ;; each metaobject we also set its wrapper.  Except for the class T,
    ;; the wrapper is always that of STANDARD-CLASS.
    ;; 
    (dolist (definition *early-class-definitions*)
      (let* ((name (ecd-class-name definition))
	     (meta (ecd-metaclass definition))
	     (wrapper (ecase meta
			(slot-class slot-class-wrapper)
			(standard-class standard-class-wrapper)
			(funcallable-standard-class funcallable-standard-class-wrapper)
			(built-in-class built-in-class-wrapper)
			(structure-class structure-class-wrapper)
			(condition-class condition-class-wrapper)))
             (class (or (find-class name nil)
			(allocate-standard-instance wrapper))))
	(when (or (eq meta 'standard-class)
		  (eq meta 'funcallable-standard-class))
	  (inform-type-system-about-std-class name))
        (setf (find-class name) class)))
    ;;
    ;;
    ;;
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
	    (meta (ecd-metaclass definition))
	    (source (ecd-source definition))
	    (direct-supers (ecd-superclass-names definition))
	    (direct-slots  (ecd-canonical-slots definition))
	    (other-initargs (ecd-other-initargs definition)))
	(let ((direct-default-initargs
	       (getf other-initargs :direct-default-initargs)))
	  (multiple-value-bind (slots cpl default-initargs direct-subclasses)
	      (early-collect-inheritance name)
	    (let* ((class (find-class name))
		   (wrapper (cond ((eq class slot-class)
				   slot-class-wrapper)
				  ((eq class standard-class) 
				   standard-class-wrapper)
				  ((eq class funcallable-standard-class) 
				   funcallable-standard-class-wrapper)
				  ((eq class standard-direct-slot-definition) 
				   standard-direct-slot-definition-wrapper)
				  ((eq class standard-effective-slot-definition) 
				   standard-effective-slot-definition-wrapper)
				  ((eq class built-in-class)  
				   built-in-class-wrapper)
				  ((eq class structure-class)
				   structure-class-wrapper)
				  ((eq class condition-class)
				   condition-class-wrapper)
				  ((eq class class-eq-specializer)
				   class-eq-specializer-wrapper)
				  ((eq class standard-generic-function)
				   standard-generic-function-wrapper)
				  (t
				   (boot-make-wrapper (length slots) name))))
		   (proto nil))
	      (when (eq name t) (setq *the-wrapper-of-t* wrapper))
	      (set (symbolicate* *the-pcl-package* '*the-class- name '*)
		   class)
	      (dolist (slot slots)
		(unless (eq (getf slot :allocation :instance) :instance)
		  (error "~@<Slot allocation ~S is not supported ~
                          in bootstrap.~@:>"
			 (getf slot :allocation))))
	      
	      (when (typep wrapper 'wrapper)
		(setf (wrapper-instance-slots-layout wrapper)
		      (mapcar #'canonical-slot-name slots))
		(setf (wrapper-class-slots wrapper)
		      ()))
	      
	      (setq proto (if (eq meta 'funcallable-standard-class)
			      (allocate-funcallable-instance wrapper)
			      (allocate-standard-instance wrapper)))
	    
	      (setq direct-slots
		    (bootstrap-make-slot-definitions 
		     name class direct-slots
		     standard-direct-slot-definition-wrapper nil))
	      (setq slots
		    (bootstrap-make-slot-definitions 
		     name class slots
		     standard-effective-slot-definition-wrapper t))
	      
	      (case meta
		((standard-class funcallable-standard-class)
		 (bootstrap-initialize-class 
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto
		  direct-slots slots direct-default-initargs default-initargs))
		(built-in-class		; *the-class-t*
		 (bootstrap-initialize-class 
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto))
		(slot-class		; *the-class-slot-object*
		 (bootstrap-initialize-class 
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto))
		(structure-class	; *the-class-structure-object*
		 (bootstrap-initialize-class 
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper))
		(condition-class
		 (bootstrap-initialize-class
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper))))))))

    (let* ((smc-class (find-class 'standard-method-combination))
	   (smc-wrapper (bootstrap-get-slot 'standard-class smc-class 'wrapper))
	   (smc (allocate-standard-instance smc-wrapper)))
      (flet ((set-slot (name value)
	       (bootstrap-set-slot 'standard-method-combination smc name value)))
	(set-slot 'source *load-pathname*)
	(set-slot 'type 'standard)
	(set-slot 'documentation "The standard method combination.")
	(set-slot 'options ()))
      (setq *standard-method-combination* smc))))

;;;
;;; Initialize a class metaobject.
;;;
(defun bootstrap-initialize-class
       (metaclass-name class name
        class-eq-wrapper source direct-supers direct-subclasses cpl wrapper
	&optional (proto nil proto-p)
	direct-slots slots direct-default-initargs default-initargs)
  (flet ((classes (names)
	   (mapcar #'find-class names))
	 (set-slot (slot-name value)
	   (bootstrap-set-slot metaclass-name class slot-name value)))
    (set-slot 'name name)
    (set-slot 'finalized-p t)
    (set-slot 'source source)
    (set-slot 'type (if (eq class (find-class t))
			t
			`(class ,class)))
    (set-slot 'class-eq-specializer 
	      (let ((spec (allocate-standard-instance class-eq-wrapper)))
		(bootstrap-set-slot 'class-eq-specializer spec 'type 
				    `(class-eq ,class))
		(bootstrap-set-slot 'class-eq-specializer spec 'object
				    class)
		spec))
    (set-slot 'class-precedence-list (classes cpl))
    (set-slot 'can-precede-list (classes (cdr cpl)))
    (set-slot 'incompatible-superclass-list nil)
    (set-slot 'direct-superclasses (classes direct-supers))
    (set-slot 'direct-subclasses (classes direct-subclasses))
    (set-slot 'direct-methods (cons nil nil))
    (set-slot 'wrapper wrapper)
    (set-slot 'predicate-name (or (cadr (assoc name *early-class-predicates*))
				  (make-class-predicate-name name)))
    (set-slot 'plist
	      `(,@(and direct-default-initargs
		       `(direct-default-initargs ,direct-default-initargs))
		,@(and default-initargs
		       `(default-initargs ,default-initargs))))
    (when (memq metaclass-name '(standard-class funcallable-standard-class
				 structure-class slot-class condition-class))
      (set-slot 'direct-slots direct-slots)
      (set-slot 'slots slots))
    ;;
    ;; For all direct superclasses SUPER of CLASS, make sure CLASS is
    ;; a direct subclass of SUPER.  Note that METACLASS-NAME doesn't
    ;; matter here for the slot DIRECT-SUBCLASSES, since every class
    ;; inherits the slot from class CLASS.
    (dolist (super direct-supers)
      (let* ((super (find-class super))
	     (subclasses (bootstrap-get-slot metaclass-name super
					     'direct-subclasses)))
	(cond ((eq +slot-unbound+ subclasses)
	       (setf (bootstrap-get-slot metaclass-name super 'direct-subclasses)
		     (list class)))
	      ((not (memq class subclasses))
	       (setf (bootstrap-get-slot metaclass-name super 'direct-subclasses)
		     (cons class subclasses))))))
    ;;
    (case metaclass-name
      (structure-class
       (let ((constructor-sym '|STRUCTURE-OBJECT class constructor|))
	 (set-slot 'predicate-name (or (cadr (assoc name *early-class-predicates*))
				       (make-class-predicate-name name)))
	 (set-slot 'defstruct-form 
		   `(defstruct (structure-object (:constructor ,constructor-sym))))
	 (set-slot 'defstruct-constructor constructor-sym)
	 (set-slot 'from-defclass-p t)    
	 (set-slot 'plist nil)
	 (set-slot 'prototype (funcall constructor-sym))))
      (condition-class
       (set-slot 'prototype (make-condition name)))
      (t
       (set-slot 'prototype
		 (if proto-p proto (allocate-standard-instance wrapper)))))
    class))

(defun bootstrap-make-slot-definitions (name class slots wrapper effective-p)
  (loop for index from 0 and slot in slots collect
	(bootstrap-make-slot-definition name class slot wrapper
					effective-p index)))

(defun bootstrap-make-slot-definition (name class slot wrapper effective-p index)  
  (let* ((slotd-class-name (if effective-p
			       'standard-effective-slot-definition
			       'standard-direct-slot-definition))
	 (slotd (allocate-standard-instance wrapper))
	 (slot-name (getf slot :name)))
    (flet ((get-val (name) (getf slot name))
	   (set-val (name val) (bootstrap-set-slot slotd-class-name slotd name val)))
      (set-val 'name         slot-name)
      (set-val 'initform     (get-val :initform))
      (set-val 'initfunction (get-val :initfunction))      
      (set-val 'initargs     (get-val :initargs))
      (set-val 'readers      (get-val :readers))
      (set-val 'writers      (get-val :writers))
      (set-val 'allocation   :instance)
      (set-val 'type         (or (get-val :type) t))
      (set-val 'documentation (or (get-val :documentation) ""))
      (set-val 'class        class)
      (when effective-p
	(set-val 'location index)
	(let ((fsc-p nil))
	  (set-val 'reader-function (make-optimized-std-reader-method-function 
				     fsc-p slot-name index))
	  (set-val 'writer-function (make-optimized-std-writer-method-function 
				     fsc-p slot-name index))
	  (set-val 'boundp-function (make-optimized-std-boundp-method-function 
				     fsc-p slot-name index)))
	(set-val 'accessor-flags 7)
	(setf (gethash class (slot-name->class-table slot-name)) slotd))
      (when (and (eq name 'standard-class)
		 (eq slot-name 'slots) effective-p)
	(setq *the-eslotd-standard-class-slots* slotd))
      (when (and (eq name 'funcallable-standard-class)
		 (eq slot-name 'slots) effective-p)
	(setq *the-eslotd-funcallable-standard-class-slots* slotd))
      slotd)))

(defun bootstrap-accessor-definitions (early-p)
  (let ((*early-p* early-p))
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
	    (meta (ecd-metaclass definition)))
	(unless (eq meta 'built-in-class)
	  (let ((direct-slots  (ecd-canonical-slots definition)))
	    (dolist (slotd direct-slots)
	      (let ((slot-name (getf slotd :name))
		    (readers (getf slotd :readers))
		    (writers (getf slotd :writers)))
		(bootstrap-accessor-definitions1 name slot-name readers writers nil)
		(bootstrap-accessor-definitions1 
		 'slot-object
		 slot-name
		 (list (slot-reader-name slot-name))
		 (list (slot-writer-name slot-name))
		 (list (slot-boundp-name slot-name)))))))))))

(defun bootstrap-accessor-definition (class-name accessor-name slot-name type)
  (multiple-value-bind (accessor-class make-method-function arglist specls doc)
      (ecase type
	(reader (values 'standard-reader-method #'make-std-reader-method-function
			(list class-name) (list class-name)
			"automatically generated reader method"))
	(writer (values 'standard-writer-method #'make-std-writer-method-function
			(list 'new-value class-name) (list t class-name)
			"automatically generated writer method"))
	(boundp (values 'standard-boundp-method #'make-std-boundp-method-function
			(list class-name) (list class-name)
			"automatically generated boundp method")))
    (let ((gf (ensure-generic-function accessor-name)))
      (if (find specls (early-gf-methods gf) 
		:key #'early-method-specializers
		:test #'equal)
	  (unless (assoc accessor-name *generic-function-fixups*
			 :test #'equal)
	    (update-dfun gf))
	  (add-method gf
		      (make-a-method accessor-class
				     ()
				     arglist specls
				     (funcall make-method-function
					      class-name slot-name)
				     doc
				     slot-name))))))

(defun bootstrap-accessor-definitions1 (class-name slot-name readers writers boundps)
  (flet ((do-reader-definition (reader)
	   (bootstrap-accessor-definition class-name reader slot-name 'reader))
	 (do-writer-definition (writer)
	   (bootstrap-accessor-definition class-name writer slot-name 'writer))
	 (do-boundp-definition (boundp)
	   (bootstrap-accessor-definition class-name boundp slot-name 'boundp)))
    (dolist (reader readers) (do-reader-definition reader))
    (dolist (writer writers) (do-writer-definition writer))
    (dolist (boundp boundps) (do-boundp-definition boundp))))

(defun bootstrap-class-predicates (early-p)
  (let ((*early-p* early-p))
    (dolist (definition *early-class-definitions*)
      (let* ((name (ecd-class-name definition))
	     (class (find-class name)))
	(setf (find-class-predicate name)
	      (make-class-predicate class (class-predicate-name class)))))))

(defun bootstrap-built-in-classes ()
  ;;
  ;; First make sure that all the supers listed in *built-in-class-lattice*
  ;; are themselves defined by *built-in-class-lattice*.  This is just to
  ;; check for typos and other sorts of brainos.
  (dolist (e *built-in-classes*)
    (dolist (super (cadr e))
      (unless (or (eq super t)
		  (assq super *built-in-classes*))
	(error "In *built-in-classes*: ~S has ~S as a superclass,~%~
                but ~S is not itself a class in *built-in-classes*."
	       (car e) super super))))
  ;;
  ;; In the first pass, we create a skeletal object to be bound to the
  ;; class name.
  (let* ((built-in-class (find-class 'built-in-class))
	 (built-in-class-wrapper (class-wrapper built-in-class)))
    (dolist (e *built-in-classes*)
      (let ((class (allocate-standard-instance built-in-class-wrapper)))
	(setf (find-class (car e)) class))))
  ;;
  ;; In the second pass, we initialize the class objects.
  (let ((class-eq-wrapper (class-wrapper (find-class 'class-eq-specializer))))
    (dolist (e *built-in-classes*)
      (destructuring-bind (name supers subs prototype) e
	(let* ((class (find-class name))
	       (lclass (kernel::find-class name))
	       (wrapper (kernel:%class-layout lclass)))
	  (set (get-built-in-class-symbol name) class)
	  (set (get-built-in-wrapper-symbol name) wrapper)
	  (setf (kernel:%class-pcl-class lclass) class)
	  (let* ((kernel-class (kernel::find-class name))
		 (cpl (kernel:std-compute-class-precedence-list kernel-class))
		 (cpl-class-names (mapcar #'kernel:%class-name cpl)))
	    (bootstrap-initialize-class 'built-in-class class
					name class-eq-wrapper nil
					supers subs cpl-class-names
					wrapper prototype))))))
  ;;
  (dolist (e *built-in-classes*)
    (let* ((name (car e))
	   (class (find-class name)))
      (setf (find-class-predicate name)
	    (make-class-predicate class (class-predicate-name class))))))


;;;
;;;
;;;

(defmacro wrapper-of-macro (x)
  `(kernel:layout-of ,x))

(defun class-of (x)
  (wrapper-class* (wrapper-of-macro x)))

(declaim (inline wrapper-of))
(defun wrapper-of (x)
  (wrapper-of-macro x))

(defun eval-form (form)
  (lambda () (eval form)))

;;;
;;; Make a non-STANDARD-CLASS for the thing with name NAME.  The thing
;;; may currently either be a structure or a condition, in which
;;; case we construct either a STRUCTURE-CLASS or a CONDITION-CLASS
;;; for the corresponding KERNEL::CLASS.
;;; 
(defun ensure-non-standard-class (name &optional existing-class)
  (flet ((ensure (metaclass slots)
	   (let* ((class (kernel::find-class name))
		  (kernel-supers (kernel:%class-direct-superclasses class))
		  (supers (mapcar #'kernel:%class-name kernel-supers)))
             (without-package-locks
	      (ensure-class-using-class
	       existing-class name :metaclass metaclass :name name
	       :direct-superclasses supers
	       :direct-slots slots))))
	 (slot-initargs-from-structure-slotd (slotd)
	   (let ((accessor (structure-slotd-accessor-symbol slotd)))
	     `(:name ,(structure-slotd-name slotd)
	       :defstruct-accessor-symbol ,accessor
	       ,@(when (fboundp accessor)
		   `(:internal-reader-function
		     ,(structure-slotd-reader-function slotd)
		     :internal-writer-function
		     ,(structure-slotd-writer-function name slotd)))
	       :type ,(or (structure-slotd-type slotd) t)
	       :initform ,(structure-slotd-init-form slotd)
	       :initfunction ,(eval-form (structure-slotd-init-form slotd)))))
	 (slot-initargs-from-condition-slotd (slot)
	   `(:name ,(conditions::condition-slot-name slot)
	     :initargs ,(conditions::condition-slot-initargs slot)
	     :readers ,(conditions::condition-slot-readers slot)
	     :writers ,(conditions::condition-slot-writers slot)
	     ,@(when (conditions::condition-slot-initform-p slot)
		 (let ((form-or-fn (conditions::condition-slot-initform slot)))
		   (if (functionp form-or-fn)
		       `(:initfunction ,form-or-fn)
		       `(:initform ,form-or-fn
			 :initfunction ,(lambda () form-or-fn)))))
	     :allocation ,(conditions::condition-slot-allocation slot)
	     :documentation ,(conditions::condition-slot-documentation slot))))
    (cond ((structure-type-p name)
	   (ensure 'structure-class
		   (mapcar #'slot-initargs-from-structure-slotd
			   (structure-type-slot-description-list name))))
	  ((condition-type-p name)
	   (let ((class (kernel::find-class name)))
	     (ensure 'condition-class
		     (mapcar #'slot-initargs-from-condition-slotd
			     (conditions::condition-class-slots class)))))
	  (t
	   (error "~@<~S is not the name of a class.~@:>" name)))))

(defun reinitialize-structure-class (kernel-class)
  (let ((class (kernel:%class-pcl-class kernel-class)))
    (when class 
      (ensure-non-standard-class (class-name class) class))))

(when (boundp 'kernel::*defstruct-hooks*)
  (pushnew 'reinitialize-structure-class
	   kernel::*defstruct-hooks*))


(defun method-function-returning-nil (args next-methods)
  (declare (ignore args next-methods))
  nil)

(defun method-function-returning-t (args next-methods)
  (declare (ignore args next-methods))
  t)

(defun make-class-predicate (class name)
  (let* ((gf (without-package-locks (ensure-generic-function name)))
	 (mlist (if (eq *boot-state* 'complete)
		    (generic-function-methods gf)
		    (early-gf-methods gf))))
    (unless mlist
      (unless (eq class *the-class-t*)
	(let* ((default-method-function #'method-function-returning-nil)
	       (default-method-initargs (list :function
					      default-method-function))
	       (default-method (make-a-method 'standard-method
					      ()
					      (list 'object)
					      (list *the-class-t*)
					      default-method-initargs
					      "class predicate default method")))
	  (setf (method-function-get default-method-function :constant-value) nil)
	  (add-method gf default-method)))
      (let* ((class-method-function #'method-function-returning-t)
	     (class-method-initargs (list :function
					  class-method-function))
	     (class-method (make-a-method 'standard-method
					  ()
					  (list 'object)
					  (list class)
					  class-method-initargs
					  "class predicate class method")))
	(setf (method-function-get class-method-function :constant-value) t)
	(add-method gf class-method)))
    gf))


;;; Set inherits from CPL and register layout.  This actually installs the
;;; class in the lisp type system.
;;;
(defun update-lisp-class-layout (class layout)
  (let ((kernel-class (kernel:layout-class layout)))
    (unless (eq (kernel:%class-layout kernel-class) layout)
      (let ((cpl (class-precedence-list class)))
	(setf (kernel:layout-inherits layout)
	      (kernel:order-layout-inherits
	       (map 'simple-vector #'class-wrapper
		    (reverse (rest cpl))))))
      (kernel:register-layout layout :invalidate t)
      ;;
      ;; Subclasses of formerly forward-referenced-class may be unknown
      ;; to lisp:find-class and also anonymous. This functionality moved
      ;; here from (setf find-class).
      (let ((name (class-name class)))
	(setf (kernel::find-class name) kernel-class
	      (kernel:%class-name kernel-class) name)))))

(defun lisp::sxhash-instance (instance)
  (get-hash instance))

#-loadable-pcl
(eval-when (:load-toplevel :execute)
  (/show "Bootstrapping PCL")
  (clrhash *find-class*)
  (/show "  meta braid")
  (bootstrap-meta-braid)
  (/show "  accessor definitions 1")
  (bootstrap-accessor-definitions t)
  (/show "  class predicates 1")
  (bootstrap-class-predicates t)
  (/show "  accessor definitions 2")
  (bootstrap-accessor-definitions nil)
  (/show "  class predicates 2")
  (bootstrap-class-predicates nil)
  (/show "  built-in classes")
  (bootstrap-built-in-classes)

  (/show "  class layouts")
  (do-hash (name x *find-class*)
    (let* ((class (find-class-from-cell name x))
	   (layout (class-wrapper class))
	   (lclass (kernel:layout-class layout))
	   (lclass-pcl-class (kernel:%class-pcl-class lclass))
	   (olclass (kernel::find-class name nil)))
      (if lclass-pcl-class
	  (assert (eq class lclass-pcl-class))
	  (setf (kernel:%class-pcl-class lclass) class))

      (update-lisp-class-layout class layout)

      (if olclass
	  (assert (eq lclass olclass))
	  (setf (kernel::find-class name) lclass))
      
      (set-class-translation class name)))

  (/show "Boot state BRAID reached")
  (setq *boot-state* 'braid))

(define-condition no-applicable-method-error (type-error)
  ((function :reader no-applicable-method-function :initarg :function)
   (arguments :reader no-applicable-method-arguments :initarg :arguments))
  (:report (lambda (condition stream)
             (format stream "~@<No matching method for the generic function ~
                             ~S, when called with arguments ~S.~@:>"
                     (no-applicable-method-function condition)
                     (no-applicable-method-arguments condition)))))

(defmethod no-applicable-method (generic-function &rest args)
  (cerror "Retry call to ~S."
          'no-applicable-method-error
          :function generic-function
          :arguments args)
  (apply generic-function args))

(define-condition no-next-method-error (type-error)
  ((method :reader no-next-method-method :initarg :method)
   (arguments :reader no-next-method-arguments :initarg :arguments))
  (:report (lambda (condition stream)
             (format stream
		     "~@<In method ~S: No next method for arguments ~S.~@:>"
                     (no-next-method-method condition)
                     (no-next-method-arguments condition)))))

(defmethod no-next-method ((generic-function standard-generic-function)
			   (method standard-method)
			   &rest args)
  (cerror "Try again." 'no-next-method-error :method method :arguments args)
  (apply generic-function args))

(define-condition no-primary-method-error (no-applicable-method-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~@<Generic function ~S: ~
                             No primary method given arguments ~S~@:>"
                     (no-applicable-method-function condition)
                     (no-applicable-method-arguments condition)))))

(defmethod no-primary-method ((generic-function standard-generic-function)
			      &rest args)
  (cerror "Try again." 'no-primary-method-error
	  :function generic-function :arguments args)
  (apply generic-function args))

(defun %no-primary-method (gf args)
  (apply #'no-primary-method gf args))

(defun %invalid-qualifiers (gf combin args methods)
  (invalid-qualifiers gf combin args methods))

(defmethod invalid-qualifiers ((gf generic-function) combin args methods)
  (if (null (cdr methods))
      (error "~@<In a call to ~s with arguments ~:s: ~
              The method ~s has invalid qualifiers for method ~
              combination ~s.~@:>"
	 gf args (car methods) combin)
      (error "~@<In a call to ~s with arguments ~:s: ~
              The methods ~{~s~^, ~} have invalid qualifiers for ~
              method combination ~s.~@:>"
	 gf args methods combin)))
