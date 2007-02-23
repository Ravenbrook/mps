;;;-*-Mode:LISP; Package:PCL; Base:10; Syntax:Common-lisp -*-
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
;;;

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/std-class.lisp,v 1.80 2005/07/07 16:44:27 rtoy Exp $")

(in-package :pcl)

(defmethod slot-accessor-function ((slotd effective-slot-definition) type)
  (ecase type
    (reader (slot-definition-reader-function slotd))
    (writer (slot-definition-writer-function slotd))
    (boundp (slot-definition-boundp-function slotd))))

(defmethod (setf slot-accessor-function) (function 
					  (slotd effective-slot-definition) type)
  (ecase type
    (reader (setf (slot-definition-reader-function slotd) function))
    (writer (setf (slot-definition-writer-function slotd) function))
    (boundp (setf (slot-definition-boundp-function slotd) function))))

(defconstant *slotd-reader-function-std-p* 1)
(defconstant *slotd-writer-function-std-p* 2)
(defconstant *slotd-boundp-function-std-p* 4)
(defconstant *slotd-all-function-std-p* 7)

(defmethod slot-accessor-std-p ((slotd effective-slot-definition) type)
  (let ((flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum flags))
    (if (eq type 'all)
	(eql *slotd-all-function-std-p* flags)
	(let ((mask (ecase type
		      (reader *slotd-reader-function-std-p*)
		      (writer *slotd-writer-function-std-p*)
		      (boundp *slotd-boundp-function-std-p*))))
	  (declare (type fixnum mask))
	  (not (zerop (the fixnum (logand mask flags))))))))

(defmethod (setf slot-accessor-std-p) (value (slotd effective-slot-definition) type)
  (let ((mask (ecase type
		(reader *slotd-reader-function-std-p*)
		(writer *slotd-writer-function-std-p*)
		(boundp *slotd-boundp-function-std-p*)))
	(flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum mask flags))
    (setf (slot-value slotd 'accessor-flags)
	  (if value
	      (the fixnum (logior mask flags))
	      (the fixnum (logand (the fixnum (lognot mask)) flags)))))
  value)

(defmethod initialize-internal-slot-functions ((slotd effective-slot-definition))
  (let* ((name (slot-value slotd 'name))
	 (class (slot-value slotd 'class)))
    (setf (gethash class (slot-name->class-table name)) slotd)
    (dolist (type '(reader writer boundp))
      (let* ((gf-name (ecase type
			      (reader 'slot-value-using-class)
			      (writer '(setf slot-value-using-class))
			      (boundp 'slot-boundp-using-class)))
	     (gf (gdefinition gf-name)))
	(compute-slot-accessor-info slotd type gf)))
    (initialize-internal-slot-gfs name)))

;;;
;;; Compute an effective method for SLOT-VALUE-USING-CLASS, (SETF
;;; SLOT-VALUE-USING-CLASS) or SLOT-BOUNDP-USING-CLASS for reading/
;;; writing/testing effective slot SLOTD.
;;;
;;; TYPE is one of the symbols READER, WRITER or BOUNDP, depending on
;;; GF.  Store the effective method in the effective slot definition
;;; object itself; these GFs have special dispatch functions calling
;;; effective methods directly retrieved from effective slot
;;; definition objects, as an optimization.
;;;
;;; FIXME: Change the function name to COMPUTE-SVUC-SLOTD-FUNCTION,
;;; or some such.
;;;
(defmethod compute-slot-accessor-info ((slotd effective-slot-definition)
				       type gf)
  (let* ((name (slot-value slotd 'name))
	 (class (slot-value slotd 'class))
	 (old-slotd (find-slot-definition class name))
	 (old-std-p (and old-slotd (slot-accessor-std-p old-slotd 'all))))
    (multiple-value-bind (function std-p)
	(if (eq *boot-state* 'complete)
	    (get-accessor-method-function gf type class slotd)
	    (get-optimized-std-accessor-method-function class slotd type))
      (setf (slot-accessor-std-p slotd type) std-p)
      (setf (slot-accessor-function slotd type) function))
    (when (and old-slotd
	       (not (eq old-std-p (slot-accessor-std-p slotd 'all))))
      (record-pv-update-info slotd))))

(defmethod slot-definition-allocation ((slotd structure-slot-definition))
  :instance)



(defmethod shared-initialize :after ((object documentation-mixin)
                                     slot-names
                                     &key (documentation nil documentation-p))
  (declare (ignore slot-names))
  (when documentation-p
    (setf (documentation object nil) documentation)))

(defmethod documentation (object doc-type)
  (declare (ignore object doc-type))
  nil)

(defmethod (setf documentation) (new-value object doc-type)
  (declare (ignore new-value doc-type))
  (error "~@<Can't change the documentation of ~S.~@:>" object))

(defmethod documentation ((object documentation-mixin) doc-type)
  (declare (ignore doc-type))
  (plist-value object 'documentation))

(defmethod (setf documentation) (new-value (object documentation-mixin) doc-type)
  (declare (ignore doc-type))
  (setf (plist-value object 'documentation) new-value))

(defmethod documentation ((slotd standard-slot-definition) doc-type)
  (declare (ignore doc-type))
  (slot-value slotd 'documentation))

(defmethod (setf documentation) (new-value (slotd standard-slot-definition) doc-type)
  (declare (ignore doc-type))
  (setf (slot-value slotd 'documentation) new-value))

(defmethod (setf documentation) (doc (gf generic-function) type)
  (declare (ignore type))
  (setf (info function documentation (generic-function-name gf))
	doc))


;;;
;;; Various class accessors that are a little more complicated than can be
;;; done with automatically generated reader methods.
;;;
(defmethod class-prototype ((class std-class))
  (with-slots (prototype) class
    (or prototype (setq prototype (allocate-instance class)))))

(defmethod class-prototype ((class structure-class))
  (with-slots (prototype wrapper defstruct-constructor) class
    (or prototype 
	(setq prototype 
	      (if defstruct-constructor 
		  (allocate-instance class)
		  (allocate-standard-instance wrapper))))))

(defmethod class-prototype ((class condition-class))
  (with-slots (prototype) class
    (or prototype (setf prototype (allocate-instance class)))))

(defmethod class-direct-default-initargs ((class slot-class))
  (plist-value class 'direct-default-initargs))

(defmethod class-default-initargs ((class slot-class))
  (plist-value class 'default-initargs))

(defmethod class-constructors ((class slot-class))
  (plist-value class 'constructors))

(defmethod class-slot-cells ((class std-class))
  (plist-value class 'class-slot-cells))


;;;
;;; Class accessors that are even a little bit more complicated than those
;;; above.  These have a protocol for updating them, we must implement that
;;; protocol.
;;; 

;;;
;;; Maintaining the direct subclasses backpointers.  The update methods are
;;; here, the values are read by an automatically generated reader method.
;;; 
(defmethod add-direct-subclass ((class class) (subclass class))
  (check-seals class 'add-direct-subclass)
  (with-slots (direct-subclasses) class
    (pushnew subclass direct-subclasses)
    subclass))

(defmethod remove-direct-subclass ((class class) (subclass class))
  (check-seals class 'remove-direct-subclass)
  (with-slots (direct-subclasses) class
    (setq direct-subclasses (remove subclass direct-subclasses))
    subclass))

;;;
;;; Maintaining the direct-methods and direct-generic-functions backpointers.
;;;
;;; There are four generic functions involved, each has one method for the
;;; class case and another method for the damned EQL specializers. All of
;;; these are specified methods and appear in their specified place in the
;;; class graph.
;;;
;;;   ADD-DIRECT-METHOD
;;;   REMOVE-DIRECT-METHOD
;;;   SPECIALIZER-DIRECT-METHODS
;;;   SPECIALIZER-DIRECT-GENERIC-FUNCTIONS
;;;
;;; In each case, we maintain one value which is a cons.  The car is the list
;;; methods.  The cdr is a list of the generic functions.  The cdr is always
;;; computed lazily.
;;;

(defmethod add-direct-method ((specializer class) (method method))
  (with-slots (direct-methods) specializer
    (setf (car direct-methods) (adjoin method (car direct-methods))	;PUSH
	  (cdr direct-methods) ()))
  method)

(defmethod remove-direct-method ((specializer class) (method method))
  (with-slots (direct-methods) specializer
    (setf (car direct-methods) (remove method (car direct-methods))
	  (cdr direct-methods) ()))
  (remove-inline-access-method specializer method)
  method)

(defmethod specializer-direct-methods ((specializer class))
  (with-slots (direct-methods) specializer
    (car direct-methods)))

(defmethod specializer-direct-generic-functions ((specializer class))
  (with-slots (direct-methods) specializer
    (or (cdr direct-methods)
	(setf (cdr direct-methods)
	      (loop for method in (car direct-methods)
		    for generic-function = (method-generic-function method)
		    unless (member generic-function collected :test #'eq)
 		      collect generic-function into collected
		    finally (return collected))))))


;;;
;;; This hash table is used to store the direct methods and direct generic
;;; functions of EQL specializers.  Each value in the table is the cons.
;;; 
(defvar *eql-specializer-methods* (make-hash-table :test 'eql))
(defvar *class-eq-specializer-methods* (make-hash-table :test 'eq))

(defmethod specializer-method-table ((specializer eql-specializer))
  *eql-specializer-methods*)

(defmethod specializer-method-table ((specializer class-eq-specializer))
  *class-eq-specializer-methods*)

(defmethod add-direct-method ((specializer specializer-with-object) (method method))
  (let* ((object (specializer-object specializer))
	 (table (specializer-method-table specializer))
	 (entry (gethash object table)))
    (unless entry
      (setq entry
	    (setf (gethash object table)
		  (cons nil nil))))
    (setf (car entry) (adjoin method (car entry))
	  (cdr entry) ())
    method))

(defmethod remove-direct-method ((specializer specializer-with-object) (method method))
  (let* ((object (specializer-object specializer))
	 (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (setf (car entry) (remove method (car entry))
	    (cdr entry) ()))
    method))

(defmethod specializer-direct-methods ((specializer specializer-with-object))  
  (car (gethash (specializer-object specializer)
		(specializer-method-table specializer))))

(defmethod specializer-direct-generic-functions ((specializer specializer-with-object))
  (let* ((object (specializer-object specializer))
	 (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (or (cdr entry)
	  (setf (cdr entry)
		(loop for method in (car entry)
		      for generic-function = (method-generic-function method)
		      unless (member generic-function collected :test #'eq)
		      collect generic-function into collected
		      finally (return collected)))))))

(defun map-specializers (function)
  (map-all-classes (lambda (class)
		     (funcall function (class-eq-specializer class))
		     (funcall function class)))
  (maphash (lambda (object methods)
	     (declare (ignore methods))
	     (intern-eql-specializer object))
	   *eql-specializer-methods*)
  (maphash (lambda (object specl)
	     (declare (ignore object))
	     (funcall function specl))
	   *eql-specializer-table*)
  nil)

(defun map-all-classes (function &optional (root t))
  (let ((braid-p (memq *boot-state* '(braid complete)))
	(root (if (symbolp root) (find-class root) root)))
    (labels ((map-class (class)
	       (mapc #'map-class
		     (if braid-p
			 (class-direct-subclasses class)
			 (early-class-direct-subclasses class)))
	       (funcall function class)))
      (map-class root))))

(defun map-all-generic-functions (function)
  (let ((all-generic-functions (make-hash-table :test 'eq)))
    (map-specializers (lambda (specl)
			(dolist (gf (specializer-direct-generic-functions specl))
			  (unless (gethash gf all-generic-functions)
			    (setf (gethash gf all-generic-functions) t)
			    (funcall function gf))))))
  nil)

(defmethod shared-initialize :after ((specl class-eq-specializer) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value specl 'type) `(class-eq ,(specializer-class specl))))

(defmethod shared-initialize :after ((specl eql-specializer) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value specl 'type) `(eql ,(specializer-object specl)))
  ;; Tell the type system about this eql specializer type.
  (setf (info type translator specl)
	(constantly (kernel:make-member-type :members (list (specializer-object specl))))))



(defun real-load-defclass (name metaclass-name supers slots other)
  (apply #'ensure-class name :metaclass metaclass-name
	 :direct-superclasses supers
	 :direct-slots slots
	 other))

(setf (gdefinition 'load-defclass) #'real-load-defclass)

;;;
;;; The DEFCLASS spec says that this redefines a class with proper
;;; name NAME, where S is the proper name of class C if S =
;;; (CLASS-NAME C) and (FIND-CLASS S) = C.
;;;
(defun ensure-class (name &rest all)
  (let ((class (find-class name nil)))
    (when (and class (neq name (class-name class)))
      (setq class nil))
    (apply #'ensure-class-using-class class name all)))

(defmethod ensure-class-using-class ((class null) name &rest args &key)
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (inform-type-system-about-class (class-prototype meta) name)
    (let ((class (apply #'make-instance meta :name name initargs)))
      (setf (find-class name) class)
      (inform-type-system-about-class class name)
      class)))

(defmethod ensure-class-using-class ((class pcl-class) name &rest args &key)
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (unless (eq (class-of class) meta)
      (apply #'change-class class meta initargs))
    (apply #'reinitialize-instance class initargs)
    (setf (find-class name) class)
    (inform-type-system-about-class class name)
    class))

(defmethod class-predicate-name ((class t))
  'function-returning-nil)

(defun ensure-class-values (class args)
  (let* ((initargs (copy-list args))
	 (unsupplied (list 1))
	 (supplied-meta   (getf initargs :metaclass unsupplied))
	 (supplied-supers (getf initargs :direct-superclasses unsupplied))
	 (meta (cond ((neq supplied-meta unsupplied)
		      (if (classp supplied-meta)
			  supplied-meta
			  (find-class supplied-meta)))
		     ((or (null class)
			  (forward-referenced-class-p class))
		      *the-class-standard-class*)
		     (t
		      (class-of class)))))  
    (flet ((fix-super (super)
	     (cond ((classp super)
		    super)
		   ((legal-class-name-p super)
		    (or (find-class super nil)
			(make-instance 'forward-referenced-class
				       :name super)))
		   (t
		    (simple-program-error
		     "~@<~S is not a class or a legal class name.~@:>"
		     super)))))
      ;;
      ;; CLHS: signal PROGRAM-ERROR, if
      ;; (a) there are any duplicate slot names
      ;; (b) any of the slot options :ALLOCATION, :INITFORM, :TYPE, or
      ;; :DOCUMENTATION appears more than one in a single slot description.
      (loop for (slot . more) on (getf initargs :direct-slots)
	    for slot-name = (getf slot :name)
	    if (some (lambda (s) (eq slot-name (getf s :name))) more) do
	      (simple-program-error
	       "~@<More than one direct slot with name ~S.~@:>"
	       slot-name)
	    else do
	      (loop for (option value . more) on slot by #'cddr
		    if (and (member option '(:allocation :type :initform
					     :documentation))
			    (not (eq unsupplied
				     (getf more option unsupplied)))) do
		      (simple-program-error
		       "~@<Duplicate slot option ~S for slot ~S.~@:>"
		       option slot-name)
		    else if (and (eq option :readers)
				 (notevery #'symbolp value)) do
		      (simple-program-error
		       "~@<Slot ~S: slot reader names must be symbols.~@:>"
		       slot-name)
		    else if (and (eq option :initargs)
				 (notevery #'symbolp value)) do
		      (simple-program-error
		       "~@<Slot ~S: initarg names must be symbols.~@:>"
		       slot-name)))
      ;;
      ;; CLHS: signal PROGRAM-ERROR, if an initialization argument name
      ;; appears more than once in :DEFAULT-INITARGS class option.
      (loop for (initarg . more) on (getf initargs :direct-default-initargs)
	    for name = (car initarg) 
	    when (some (lambda (a) (eq (car a) name)) more) do
	      (simple-program-error
	       "~@<Duplicate initialization argument ~
                name ~S in ~s of class ~A.~@:>"
	       name :default-initargs class))
      ;;
      (loop for (arg value) on initargs by #'cddr
	    count (eq arg :metaclass) into metaclass
	    count (eq arg :direct-default-initargs) into default-initargs
	    when (or (> metaclass 1) (> default-initargs 1)) do
	      (simple-program-error
	       "~@<Class ~S: More than one ~S specified~@:>"
	       class (if (eq arg :direct-default-initargs)
			 :default-initargs arg)))
      (remf initargs :metaclass)
      (remf initargs :direct-superclasses)
      ;;
      (values meta
	      (nconc (when (neq supplied-supers unsupplied)
		       (list :direct-superclasses
			     (mapcar #'fix-super supplied-supers)))
		     initargs)))))


;;;
;;;
;;;
(defmethod shared-initialize :after
	   ((class std-class)
	    slot-names
	    &key (direct-superclasses nil direct-superclasses-p)
		 (direct-slots nil direct-slots-p)
		 (direct-default-initargs nil direct-default-initargs-p)
	         (predicate-name nil predicate-name-p))
  (declare (ignore slot-names))
  (cond ((or direct-superclasses-p
	     (null (slot-value class 'direct-superclasses)))
	 (setq direct-superclasses
	       (or direct-superclasses
		   (list (if (funcallable-standard-class-p class)
			     *the-class-funcallable-standard-object*
			     *the-class-standard-object*))))
	 (dolist (superclass direct-superclasses)
	   (unless (validate-superclass class superclass)
	     (error "~@<The class ~S was specified as a ~
		     super-class of the class ~S, ~
		     but the meta-classes ~S and ~S are incompatible.  ~
		     Define a method for ~S to avoid this error.~@:>"
		     superclass class (class-of superclass) (class-of class)
		     'validate-superclass)))
	 (setf (slot-value class 'direct-superclasses) direct-superclasses))
	(t
	 (setq direct-superclasses (slot-value class 'direct-superclasses))))
  (setq direct-slots
	(if direct-slots-p
	    (setf (slot-value class 'direct-slots)
		  (mapcar (lambda (pl) (make-direct-slotd class pl))
			  direct-slots))
	    (slot-value class 'direct-slots)))
  (if direct-default-initargs-p
      (setf (plist-value class 'direct-default-initargs)
	    direct-default-initargs)
      (setq direct-default-initargs
	    (plist-value class 'direct-default-initargs)))
  ;;
  ;; Initialize shared slots.  A class may inherit initforms for
  ;; shared slots from superclasses.  Such initializations are
  ;; done in UPDATE-CLASS-SLOT-VALUES.
  (collect ((cells))
    (dolist (dslotd direct-slots)
      (when (eq (slot-definition-allocation dslotd) :class)
	(let ((initfn (slot-definition-initfunction dslotd)))
	  (cells (cons (slot-definition-name dslotd)
		       (if initfn
			   (funcall initfn)
			   +slot-unbound+))))))
    (setf (plist-value class 'class-slot-cells) (cells)))
  ;;
  (setq predicate-name (if predicate-name-p
			   (setf (slot-value class 'predicate-name)
				 (car predicate-name))
			   (or (slot-value class 'predicate-name)
			       (setf (slot-value class 'predicate-name)
				     (make-class-predicate-name (class-name class))))))
  (add-direct-subclasses class direct-superclasses)
  (make-class-predicate class predicate-name)
  (update-class class nil)
  (add-slot-accessors class direct-slots)
  (make-preliminary-layout class))

(defmethod shared-initialize :after ((class forward-referenced-class)
				     slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (make-preliminary-layout class))

;;;
;;; Give CLASS a preliminary layout, if it doesn't have a layout
;;; already.  This is done to make CLASS known to the type system
;;; before the class is finalized, and is a consequence of the class
;;; schizophrenia we are suffering from.
;;;
(defvar *allow-forward-referenced-classes-in-cpl-p* nil)

(defun make-preliminary-layout (class)
  (flet ((compute-preliminary-cpl (root)
	   (let ((*allow-forward-referenced-classes-in-cpl-p* t))
	     (compute-class-precedence-list root))))
    (unless (class-finalized-p class)
      (let ((name (class-name class)))
	(setf (find-class name) class)
	(inform-type-system-about-class class name)
	(let ((layout (make-wrapper 0 class))
	      (kernel-class (kernel::find-class name)))
	  (setf (kernel:layout-class layout) kernel-class)
	  (setf (kernel:%class-pcl-class kernel-class) class)
	  (setf (slot-value class 'wrapper) layout)
	  (let ((cpl (compute-preliminary-cpl class)))
	    (setf (kernel:layout-inherits layout)
		  (kernel:order-layout-inherits
		   (map 'simple-vector #'class-wrapper
			(reverse (rest cpl))))))
	  (kernel:register-layout layout :invalidate t)
	  (setf (kernel:%class-layout kernel-class) layout)
	  (mapc #'make-preliminary-layout (class-direct-subclasses class)))))))

(defmethod shared-initialize :before ((class class) slot-names &key name)
  (declare (ignore slot-names name))
  (setf (slot-value class 'type) `(class ,class))
  (setf (slot-value class 'class-eq-specializer)
	(make-instance 'class-eq-specializer :class class)))

(defmethod reinitialize-instance :before ((class slot-class)
					  &key direct-superclasses)
  (remove-direct-subclasses class direct-superclasses)
  (remove-slot-accessors    class (class-direct-slots class)))

(defmethod reinitialize-instance :after ((class slot-class)
					 &rest initargs
					 &key)
  (map-dependents class
		  (lambda (dependent)
		    (apply #'update-dependent class dependent initargs))))

(defmethod shared-initialize :after 
      ((class structure-class)
       slot-names
       &key (direct-superclasses nil direct-superclasses-p)
            (direct-slots nil direct-slots-p)
            direct-default-initargs
            (predicate-name nil predicate-name-p))
  (declare (ignore slot-names direct-default-initargs))
  (if direct-superclasses-p
      (setf (slot-value class 'direct-superclasses)
	    (or direct-superclasses
		(setq direct-superclasses
		      (and (not (eq (class-name class) 'structure-object))
			   (list *the-class-structure-object*)))))
      (setq direct-superclasses (slot-value class 'direct-superclasses)))
  (let* ((name (class-name class))
	 (from-defclass-p (slot-value class 'from-defclass-p))
	 (defstruct-p (or from-defclass-p (not (structure-type-p name)))))
    (if direct-slots-p
	(setf (slot-value class 'direct-slots)
	      (setq direct-slots
		    (mapcar (lambda (pl)
			      (when defstruct-p
				(let* ((slot-name (getf pl :name))
				       (accessor
					(symbolicate*
					 *package*
					 (if (symbol-package name)
					     (package-name (symbol-package name))
					     "")
					 "::" name " structure class " slot-name)))
				  (setq pl (list* :defstruct-accessor-symbol accessor
						  pl))))
			      (make-direct-slotd class pl))
			    direct-slots)))
	(setq direct-slots (slot-value class 'direct-slots)))
    (if defstruct-p
	(let* ((include (car (slot-value class 'direct-superclasses)))
	       (conc-name (symbolicate* *package*
					(if (symbol-package name)
					    (package-name (symbol-package name))
					    "")
					"::" name " structure class "))
	       ;;
	       ;; It's not possible to use a generalized name for the
	       ;; constructor function.  It shouldn't matter though, I think,
	       ;; like for the slot names above, because this stuff is not
	       ;; supposed to be used by users directly.
	       (constructor
		(symbolicate* *package* conc-name " constructor"))
	       (defstruct `(defstruct (,name
					,@(when include
					    `((:include ,(class-name include))))
					(:predicate nil)
					(:conc-name ,conc-name)
					(:constructor ,constructor ()))
			     ;;
			     ;; Use a temporary unbound marker that lets
			     ;; SHARED-INITIALIZE recognize if a before-method
			     ;; has written to a slot.
			     ,@(mapcar (lambda (slot)
					 `(,(slot-definition-name slot)
					    '.unbound.))
				       direct-slots)))
	       (reader-names (mapcar (lambda (slotd)
				       (list 'slot-accessor name
					     (slot-definition-name slotd)
					     'reader))
				     direct-slots))
	       (writer-names (mapcar (lambda (slotd)
				       (list 'slot-accessor name
					     (slot-definition-name slotd)
					     'writer))
				     direct-slots))
	       (readers-init 
		(mapcar (lambda (slotd reader-name)
			  (let ((accessor 
				 (slot-definition-defstruct-accessor-symbol slotd)))
			    `(defun ,reader-name (obj)
			       (declare (type ,name obj))
			       (,accessor obj))))
			direct-slots reader-names))
	       (writers-init 
		(mapcar (lambda (slotd writer-name)
			  (let ((accessor 
				 (slot-definition-defstruct-accessor-symbol slotd)))
			    `(defun ,writer-name (nv obj)
			       (declare (type ,name obj))
			       (setf (,accessor obj) nv))))
			direct-slots writer-names))
	       (defstruct-form
		`(progn
		   ,defstruct
		   ,@readers-init ,@writers-init)))
	  (unless (structure-type-p name) (eval defstruct-form))
	  (mapc (lambda (dslotd reader-name writer-name)
		  (let* ((reader (when (fboundp reader-name)
				   (gdefinition reader-name)))
			 (writer (when (fboundp writer-name)
				   (gdefinition writer-name))))
		    (setf (slot-value dslotd 'internal-reader-function) reader)
		    (setf (slot-value dslotd 'internal-writer-function) writer)))
		direct-slots reader-names writer-names)
	  (setf (slot-value class 'defstruct-form) defstruct-form)
	  (setf (slot-value class 'defstruct-constructor) constructor))
	;;
	;; ALLOCATE-INSTANCE is supposed to work with structures
	;; defined with DEFSTRUCT.
	(with-slots (defstruct-constructor) class
	  (setq defstruct-constructor
		(make-defstruct-allocation-function class)))))
  ;;
  (add-direct-subclasses class direct-superclasses)
  (setf (slot-value class 'class-precedence-list) 
	(compute-class-precedence-list class))
  (setf (slot-value class 'slots) (compute-slots class))
  (let ((lclass (kernel::find-class (class-name class))))
    (setf (kernel:%class-pcl-class lclass) class)
    (setf (slot-value class 'wrapper) (kernel:%class-layout lclass)))
  (setf (slot-value class 'finalized-p) t)
  (update-pv-table-cache-info class)
  (setq predicate-name (if predicate-name-p
			   (setf (slot-value class 'predicate-name)
				 (car predicate-name))
			   (or (slot-value class 'predicate-name)
			       (setf (slot-value class 'predicate-name)
				     (make-class-predicate-name (class-name class))))))
  (make-class-predicate class predicate-name)
  (add-slot-accessors class direct-slots))

;;;
;;; Return a closure for allocating an uninitialized structure
;;; instance of class CLASS.
;;; 
(defun make-defstruct-allocation-function (class)
  (let ((dd (get-structure-dd (class-name class))))
    (lambda ()
      (let ((instance (kernel::%make-instance (kernel::dd-length dd)))
	    (raw-index (kernel::dd-raw-index dd)))
	(setf (kernel::%instance-layout instance)
	      (kernel::compiler-layout-or-lose (kernel::dd-name dd)))
	(when raw-index
	  (setf (kernel::%instance-ref instance raw-index)
		(make-array (kernel::dd-raw-length dd)
			    :element-type '(unsigned-byte 32))))
	instance))))
    
(defmethod direct-slot-definition-class ((class structure-class)
					 &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))

(defmethod finalize-inheritance ((class structure-class))
  nil) ; always finalized
 
(defun add-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'add))

(defun remove-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'remove))

(defun fix-slot-accessors (class dslotds add/remove)
  (flet ((fix (gfspec name r/w)
	   (let ((gf (ensure-generic-function gfspec)))
	     (case r/w
	       (r (if (eq add/remove 'add)
		      (add-reader-method class gf name)
		      (remove-reader-method class gf)))
	       (w (if (eq add/remove 'add)
		      (add-writer-method class gf name)
		      (remove-writer-method class gf)))))))
    (dolist (dslotd dslotds)
      (let ((slot-name (slot-definition-name dslotd)))
	(dolist (r (slot-definition-readers dslotd))
	  (fix r slot-name 'r))
	(dolist (w (slot-definition-writers dslotd))
	  (fix w slot-name 'w))))))


(defun add-direct-subclasses (class new)
  (dolist (n new)
    (unless (memq class (class-direct-subclasses class))
      (add-direct-subclass n class))))

(defun remove-direct-subclasses (class new)
  (let ((old (class-direct-superclasses class)))
    (dolist (o (set-difference old new))
      (remove-direct-subclass o class))))


;;;
;;;
;;;
(defmethod finalize-inheritance ((class std-class))
  (update-class class t))

(defmethod finalize-inheritance ((class forward-referenced-class))
  (simple-program-error
   "~@<Forward-referenced classes cannot be finalized: ~A.~@:>"
   class))


;;;
;;; Called by :after shared-initialize whenever a class is initialized or 
;;; reinitialized.  The class may or may not be finalized.
;;; 
(defun update-class (class finalizep)
  ;;
  ;; Calling UPDATE-SLOTS below sets the class wrapper of CLASS, which
  ;; makes the class finalized.  When UPDATE-CLASS isn't called from
  ;; FINALIZE-INHERITANCE, make sure that this finalization invokes
  ;; FINALIZE-INHERITANCE as per AMOP.  Note that we can't simply
  ;; delay the finalization when CLASS has no forward referenced
  ;; superclasses because that causes bootstrap problems.
  (when (and (not (or finalizep (class-finalized-p class)))
	     (not (class-has-a-forward-referenced-superclass-p class)))
    (finalize-inheritance class)
    (return-from update-class))
  ;;
  (when (or finalizep
	    (class-finalized-p class)
	    (not (class-has-a-forward-referenced-superclass-p class)))
    (setf (find-class (class-name class)) class)
    (update-cpl class (compute-class-precedence-list class))
    (update-slots class (compute-slots class))
    (update-gfs-of-class class)
    (update-inits class (compute-default-initargs class))
    (update-shared-slot-values class)
    (update-ctors 'finalize-inheritance :class class))
  ;;
  (unless finalizep
    (dolist (sub (class-direct-subclasses class))
      (update-class sub nil))))

;;;
;;; Set values of shared slots from initforms inherited from
;;; superclasses, which can't be done before the cpl is known.
;;; 
(defun update-shared-slot-values (class)
  (dolist (slot (class-slots class))
    (when (eq (slot-definition-allocation slot) :class)
      (let ((cell (assq (slot-definition-name slot) (class-slot-cells class))))
	(when (and cell (eq +slot-unbound+ (cdr cell)))
	  (let ((initfn (slot-definition-initfunction slot)))
	    (when initfn
	      (setf (cdr cell) (funcall initfn)))))))))

(defun update-cpl (class cpl)
  (if (class-finalized-p class)
      (unless (and (equal (class-precedence-list class) cpl)
		   (loop for c in cpl never
			   (loop for s in (class-direct-slots c) thereis
				   (eq (slot-definition-allocation s)
				       :class))))
	;; Need to have the cpl setup before update-lisp-class-layout
	;; is called on CMUCL.
	(setf (slot-value class 'class-precedence-list) cpl)
	(force-cache-flushes class))
      (setf (slot-value class 'class-precedence-list) cpl))
  (update-class-can-precede-p cpl))
  
(defun update-class-can-precede-p (cpl)
  (loop for (class . rest) on cpl do
	  (with-slots (can-precede-list) class
	    (setq can-precede-list
		  (union rest can-precede-list :test #'eq)))))

(defun class-can-precede-p (class1 class2)
  (member class2 (class-can-precede-list class1)))

(defun update-slots (class eslotds)
  (collect ((instance-slots) (class-slots))
    (dolist (eslotd eslotds)
      (case (slot-definition-allocation eslotd)
	(:instance (instance-slots eslotd))
	(:class (class-slots eslotd))))
    ;;
    ;; If there is a change in the shape of the instances then the
    ;; old class is now obsolete.
    (let* ((nlayout (mapcar #'slot-definition-name
			    (sort (instance-slots) #'<
				  :key #'slot-definition-location)))
	   (nslots (length nlayout))
	   (nwrapper-class-slots (compute-class-slots (class-slots)))
	   (owrapper (when (class-finalized-p class)
		       (class-wrapper class)))
	   (olayout (when owrapper
		      (wrapper-instance-slots-layout owrapper)))
	   (nwrapper
	    (cond ((null owrapper)
		   (make-wrapper nslots class))
		  ;;
		  ;; We cannot reuse the old wrapper easily when it
		  ;; has class slot cells, even if these cells are
		  ;; EQUAL to the ones used in the new wrapper.  The
		  ;; class slot cells of OWRAPPER may be referenced
		  ;; from caches, and if we don't change the wrapper,
		  ;; the caches won't notice that something has
		  ;; changed.  We could do something here manually,
		  ;; but I don't think it's worth it.
		  ((and (equal nlayout olayout)
			(null (wrapper-class-slots owrapper)))
		   owrapper)
		  (t
		   ;;
		   ;; This will initialize the new wrapper to have the same
		   ;; state as the old wrapper.  We will then have to change
		   ;; that.  This may seem like wasted work (it is), but the
		   ;; spec requires that we call make-instances-obsolete.
		   (make-instances-obsolete class)
		   (class-wrapper class)))))

      (with-slots (wrapper slots finalized-p) class
	(update-lisp-class-layout class nwrapper)
	(setf slots eslotds
	      (wrapper-instance-slots-layout nwrapper) nlayout
	      (wrapper-class-slots nwrapper) nwrapper-class-slots
	      (wrapper-no-of-instance-slots nwrapper) nslots
	      wrapper nwrapper
	      finalized-p t))

      (unless (eq owrapper nwrapper)
	(update-inline-access class)
	(update-pv-table-cache-info class)
	(maybe-update-standard-class-locations class)))))

(defun compute-class-slots (eslotds)
  (loop for eslotd in eslotds
	for name = (slot-definition-name eslotd)
	and class = (slot-definition-class eslotd)
	collect (assoc name (class-slot-cells class))))

(defun update-gfs-of-class (class)
  (when (and (class-finalized-p class)
	     (let ((cpl (class-precedence-list class)))
	       (or (member *the-class-slot-class* cpl)
		   (member *the-class-standard-effective-slot-definition* cpl))))
    (let ((gf-table (make-hash-table :test 'eq)))
      (labels ((collect-gfs (class)
		 (dolist (gf (specializer-direct-generic-functions class))
		   (setf (gethash gf gf-table) t))
		 (mapc #'collect-gfs (class-direct-superclasses class))))
	(collect-gfs class)
	(maphash (lambda (gf ignore)
		   (declare (ignore ignore))
		   (update-gf-dfun class gf))
		 gf-table)))))

(defun update-inits (class inits)
  (setf (plist-value class 'default-initargs) inits))


;;;
;;;
;;;
(defmethod compute-default-initargs ((class slot-class))
  (let ((initargs (loop for c in (class-precedence-list class)
			append (class-direct-default-initargs c))))
    (delete-duplicates initargs :test #'eq :key #'car :from-end t)))

;;;
;;; Protocols for constructing direct and effective slot definitions.
;;;
;;; 
;;;
;;;
(defmethod direct-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition))

(defun make-direct-slotd (class initargs)
  (let ((initargs (list* :class class initargs)))
    (apply #'make-instance
	   (apply #'direct-slot-definition-class class initargs)
	   initargs)))

;;;
;;; As specified, we must call COMPUTE-EFFECTIVE-SLOT-DEFINITION once
;;; for each different slot name we find in our superclasses.  Each
;;; call receives the class and a list of the dslotds with that name.
;;; The list is in most-specific-first order.
;;;
(defmethod compute-slots ((class std-class))
  (collect ((names/slots) (effective))
    (dolist (c (reverse (class-precedence-list class)))
      (dolist (slot (class-direct-slots c))
	(let* ((name (slot-definition-name slot))
	       (entry (assq name (names/slots))))
	  (if entry
	      (push slot (cdr entry))
	      (names/slots (list name slot))))))
    (dolist (e (names/slots) (effective))
      (effective (compute-effective-slot-definition class (car e) (cdr e))))))

;;;
;;; These are the specified AMOP methods.
;;;

(defmethod compute-slots ((class standard-class))
  (call-next-method))

(defmethod compute-slots :around ((class standard-class))
  (let ((eslotds (call-next-method))
	(location -1)
	(slot-names ()))
    (dolist (eslotd eslotds eslotds)
      (let ((allocation (slot-definition-allocation eslotd))
	    (name (slot-definition-name eslotd)))
	;;
	;; Users are free to override COMPUTE-SLOTS, and they are
	;; arguably free to MAKE-INSTANCE effective slot definition
	;; metaobjects.  Such objects won't be initialized completely,
	;; from the standpoint of PCL, so we have to fix them.
	;;
	;; If such an effective slot definition has the same name as
	;; another slot, we certainly lose, at least if it's a class
	;; slot.  I'm deliberately ignoring that for now.
	;;
	;; Note that COMPUTE-SLOTS can be called multiple times.
	;; We don't want to create a new class slot cell every time
	;; around.
	(when (null (slot-definition-class eslotd))
	  (setf (slot-definition-class eslotd) class)
	  (when (eq allocation :class)
	    (unless (assq name (class-slot-cells class))
	      (push (cons name +slot-unbound+)
		    (plist-value class 'class-slot-cells)))
	    (setf (slot-definition-allocation-class eslotd) class)))
	;;
	;; Assign slot locations.
	(setf (slot-definition-location eslotd)
	    (case allocation
	      (:instance
	       (incf location))
	      (:class
	       (let* ((from-class (slot-definition-allocation-class eslotd))
		      (cell (assq name (class-slot-cells from-class))))
		 (assert (consp cell))
		 cell))))
	(initialize-internal-slot-functions eslotd)))))

(defmethod compute-slots ((class funcallable-standard-class))
  (call-next-method))

(defmethod compute-slots :around ((class funcallable-standard-class))
  (labels (;;
	   ;; Return a list of the names of instance slots in SLOTDS.
	   (instance-slot-names (slotds)
	     (loop for e in slotds
		   when (eq (slot-definition-allocation e) :instance)
		     collect (slot-definition-name e)))
	   ;;
	   ;; This sorts slots so that slots of classes later in the CPL
	   ;; come before slots of other classes.  This is crucial for
	   ;; funcallable instances because it ensures that the slots of
	   ;; FUNCALLABLE-STANDARD-OBJECT, which includes the slots of
	   ;; KERNEL:FUNCALLABLE-INSTANCE, come first, which in turn
	   ;; makes it possible to treat FUNCALLABLE-STANDARD-OBJECT as
	   ;; a funcallable instance.
	   (compute-layout (eslotds)
	     (loop with first = ()
		   with names = (instance-slot-names eslotds)
		   for class in (reverse (class-precedence-list class)) do
		     (loop for ss in (class-slots class)
			   as name = (slot-definition-name ss) 
			   when (member name names) do
			     (push name first)
			     (setq names (delete name names)))
		   finally (return (nreverse (nconc names first))))))
    ;;
    (let ((all-slotds (call-next-method))
	  (instance-slots ())
	  (class-slots ()))
      (loop for slotd in all-slotds do
	      (case (slot-definition-allocation slotd)
		(:instance (push slotd instance-slots))
		(:class    (push slotd class-slots))))
      (loop with layout = (compute-layout instance-slots)
	    for slotd in instance-slots do
	      (setf (slot-definition-location slotd) 
		    (position (slot-definition-name slotd) layout))
	      (initialize-internal-slot-functions slotd))
      (loop for slotd in class-slots
	    as name = (slot-definition-name slotd)
	    as from-class = (slot-definition-allocation-class slotd) do
	      (setf (slot-definition-location slotd) 
		    (assoc name (class-slot-cells from-class)))
	      (assert (consp (slot-definition-location slotd)))
	      (initialize-internal-slot-functions slotd))
      all-slotds)))

(defmethod compute-slots ((class structure-class))
  (mapcan (lambda (superclass)
	    (mapcar (lambda (dslotd)
		      (compute-effective-slot-definition
		       class (slot-definition-name dslotd) (list dslotd)))
		    (class-direct-slots superclass)))
	  (reverse (slot-value class 'class-precedence-list))))

(defmethod compute-slots :around ((class structure-class))
  (let ((eslotds (call-next-method)))
    (mapc #'initialize-internal-slot-functions eslotds)
    eslotds))

(defvar *allow-funny-slot-names* nil)

(defmethod initialize-internal-slot-functions :around
    ((slotd structure-effective-slot-definition))
  (let ((*allow-funny-slot-names* t))
    (call-next-method)))
  
(defmethod compute-effective-slot-definition
    ((class slot-class) slot-name dslotds)
  (declare (ignore slot-name))
  (let* ((initargs (compute-effective-slot-definition-initargs class dslotds))
	 (class (apply #'effective-slot-definition-class class initargs)))
    (apply #'make-instance class initargs)))

(defmethod effective-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition))

(defmethod effective-slot-definition-class ((class structure-class)
					    &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod compute-effective-slot-definition-initargs 
    ((class slot-class) direct-slotds)
  (let* ((name nil)
	 (initfunction nil)
	 (initform nil)
	 (initargs nil)
	 (allocation nil)
	 (allocation-class nil)
	 (type t)
	 (namep  nil)
	 (initp  nil)
	 (allocp nil))

    (dolist (slotd direct-slotds)
      (when slotd
	(unless namep
	  (setq name (slot-definition-name slotd)
		namep t))
	(unless initp
	  (when (slot-definition-initfunction slotd)
	    (setq initform (slot-definition-initform slotd)
		  initfunction (slot-definition-initfunction slotd)
		  initp t)))
	(unless allocp
	  (setq allocation (slot-definition-allocation slotd)
		allocation-class (slot-definition-class slotd)
		allocp t))
	(setq initargs (append (slot-definition-initargs slotd) initargs))
	(let ((slotd-type (slot-definition-type slotd)))
	  (setq type (cond ((eq type t) slotd-type)
			   ((*subtypep type slotd-type) type)
			   (t `(and ,type ,slotd-type)))))))
    (list :name name
	  :initform initform
	  :initfunction initfunction
	  :initargs initargs
	  :allocation allocation
	  :allocation-class allocation-class
	  :type type
	  :class class)))

(defmethod compute-effective-slot-definition-initargs :around
    ((class structure-class) direct-slotds)
  (let ((slotd (car direct-slotds)))
    (list* :defstruct-accessor-symbol
	   (slot-definition-defstruct-accessor-symbol slotd)
	   :internal-reader-function
	   (slot-definition-internal-reader-function slotd)
	   :internal-writer-function
	   (slot-definition-internal-writer-function slotd)
	   (call-next-method))))

;;;
;;; NOTE: For bootstrapping considerations, these can't use make-instance
;;;       to make the method object.  They have to use make-a-method which
;;;       is a specially bootstrapped mechanism for making standard methods.
;;;
(defmethod reader-method-class ((class slot-class) direct-slot &rest initargs)
  (declare (ignore direct-slot initargs))
  (find-class 'standard-reader-method))

(defmethod add-reader-method ((class slot-class) generic-function slot-name)
  (add-method generic-function 
	      (make-a-method 'standard-reader-method
			     ()
			     (list (or (class-name class) 'object))
			     (list class)
			     (make-reader-method-function class slot-name)
			     "automatically generated reader method"
			     slot-name)))

(defmethod writer-method-class ((class slot-class) direct-slot &rest initargs)
  (declare (ignore direct-slot initargs))
  (find-class 'standard-writer-method))

(defmethod add-writer-method ((class slot-class) generic-function slot-name)
  (add-method generic-function
	      (make-a-method 'standard-writer-method
			     ()
			     (list 'new-value (or (class-name class) 'object))
			     (list *the-class-t* class)
			     (make-writer-method-function class slot-name)
			     "automatically generated writer method"
			     slot-name)))

(defmethod add-boundp-method ((class slot-class) generic-function slot-name)
  (add-method generic-function 
	      (make-a-method 'standard-boundp-method
			     ()
			     (list (or (class-name class) 'object))
			     (list class)
			     (make-boundp-method-function class slot-name)
			     "automatically generated boundp method"
			     slot-name)))

(defmethod remove-reader-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function () (list class) nil)))
    (when method
      (remove-method generic-function method))))

(defmethod remove-writer-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function ()
			    (list *the-class-t* class) nil)))
    (when method
      (remove-method generic-function method))))

(defmethod remove-boundp-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function () (list class) nil)))
    (when method
      (remove-method generic-function method))))


;;;
;;; make-reader-method-function and make-write-method function are NOT part of
;;; the standard protocol.  They are however useful, PCL makes uses makes use
;;; of them internally and documents them for PCL users.
;;;
;;; *** This needs work to make type testing by the writer functions which
;;; *** do type testing faster.  The idea would be to have one constructor
;;; *** for each possible type test.  In order to do this it would be nice
;;; *** to have help from inform-type-system-about-class and friends.
;;;
;;; *** There is a subtle bug here which is going to have to be fixed.
;;; *** Namely, the simplistic use of the template has to be fixed.  We
;;; *** have to give the optimize-slot-value method the user might have
;;; *** defined for this metclass a chance to run.
;;;
(defmethod make-reader-method-function ((class slot-class) slot-name)
  (make-std-reader-method-function (class-name class) slot-name))

(defmethod make-writer-method-function ((class slot-class) slot-name)
  (make-std-writer-method-function (class-name class) slot-name))

(defmethod make-boundp-method-function ((class slot-class) slot-name)
  (make-std-boundp-method-function (class-name class) slot-name))


;;;; inform-type-system-about-class
;;;
;;; This is NOT part of the standard protocol.  It is an internal mechanism
;;; which PCL uses to *try* and tell the type system about class definitions.
;;; In a more fully integrated implementation of CLOS, the type system would
;;; know about class objects and class names in a more fundamental way and
;;; the mechanism used to inform the type system about new classes would be
;;; different.
;;;
(defmethod inform-type-system-about-class ((class std-class) name)
  ;; Maybe add skeleton lisp:standard-class to avoid undefined-function
  ;; compiler warnings. Not otherwise needed in this implementation.
  (inform-type-system-about-std-class name)
  (set-class-translation class name))

(defmethod inform-type-system-about-class ((class funcallable-standard-class)
					   name)
  (set-class-translation class name))

(defmethod inform-type-system-about-class ((class structure-class) name)
  (set-class-translation class name))

(defmethod inform-type-system-about-class ((class condition-class) name)
  (set-class-translation class name))

(defmethod inform-type-system-about-class ((class forward-referenced-class)
					   name)
  (inform-type-system-about-std-class name)
  (set-class-translation class name))


(defmethod compatible-meta-class-change-p (class proto-new-class)
  (eq (class-of class) (class-of proto-new-class)))

(defmethod validate-superclass ((class class) (new-super class))
  (or (eq new-super *the-class-t*)
      (eq (class-of class) (class-of new-super))))

(defmethod validate-superclass ((class standard-class) (new-super std-class))
  (let ((new-super-meta-class (class-of new-super)))
    (or (eq new-super-meta-class *the-class-std-class*)
	(eq (class-of class) new-super-meta-class))))


;;;
;;; Force the flushing of caches by creating a new wrapper for CLASS,
;;; if necessary.
;;;
;;; If the LAYOUT-INVALID slot of CLASS's wrapper is
;;;
;;; -- (:FLUSH <new wrapper>) or (:OBSOLETE <new wrapper>), there's
;;;    nothing to do.  The new wrapper has already been created.
;;;
;;; -- :INVALID, then it has been set to that value by a previous
;;;    call to REGISTER-LAYOUT for a superclass S of CLASS; S's
;;;    wrapper has been invalidated together with its subclasses.  In
;;;    this case, CLASS's caches must obviously be flushed, too, like
;;;    S's.  So, make a new wrapper for CLASS, and translate kernel's
;;;    :INVALID to PCL (:FLUSH <new wrapper>).  UPDATE-SLOTS can later
;;;    decide if it wants to make this (:OBSOLETE ...).
;;;
;;; -- NIL, then the wrapper is still valid, in which case we do
;;;    the same as for :INVALID, but for the obviously slightly
;;;    different reason.
;;;
(defun force-cache-flushes (class)
  (let* ((owrapper (class-wrapper class)))
    (when (or (not (invalid-wrapper-p owrapper))
	      (eq :invalid (kernel:layout-invalid owrapper)))
      (let ((nwrapper (make-wrapper (wrapper-no-of-instance-slots owrapper)
				    class)))
	(setf (wrapper-instance-slots-layout nwrapper)
	      (wrapper-instance-slots-layout owrapper))
	(setf (wrapper-class-slots nwrapper)
	      (wrapper-class-slots owrapper))
	(with-pcl-lock
	  (update-lisp-class-layout class nwrapper)
	  (setf (slot-value class 'wrapper) nwrapper)
	  (flet ((obsolete-super-p ()
		   (some (lambda (layout)
			   (eq (car-safe (kernel:layout-invalid layout))
			       :obsolete))
			 (kernel:layout-inherits owrapper))))
	    (invalidate-wrapper owrapper
				(if (obsolete-super-p) :obsolete :flush)
				nwrapper)))))))

(defun flush-cache-trap (owrapper nwrapper instance)
  (declare (ignore owrapper))
  (cond ((std-instance-p instance)
	 (setf (std-instance-wrapper instance) nwrapper))
	((fsc-instance-p instance)
	 (setf (fsc-instance-wrapper instance) nwrapper))
	(t
	 (internal-error "Internal error."))))


;;;
;;; make-instances-obsolete can be called by user code.  It will cause the
;;; next access to the instance (as defined in 88-002R) to trap through the
;;; update-instance-for-redefined-class mechanism.
;;; 
(defmethod make-instances-obsolete ((class std-class))
  (let* ((owrapper (class-wrapper class))
	 (nwrapper (make-wrapper (wrapper-no-of-instance-slots owrapper)
				 class)))
      (setf (wrapper-instance-slots-layout nwrapper)
	    (wrapper-instance-slots-layout owrapper))
      (setf (wrapper-class-slots nwrapper)
	    (wrapper-class-slots owrapper))
      (with-pcl-lock
	(update-lisp-class-layout class nwrapper)
	(setf (slot-value class 'wrapper) nwrapper)
	(invalidate-wrapper owrapper :obsolete nwrapper)
	class)))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class)))


;;;
;;; obsolete-instance-trap is the internal trap that is called when we see
;;; an obsolete instance.  The times when it is called are:
;;;   - when the instance is involved in method lookup
;;;   - when attempting to access a slot of an instance
;;;
;;; It is not called by class-of, wrapper-of, or any of the low-level instance
;;; access macros.
;;;
;;; Of course these times when it is called are an internal implementation
;;; detail of PCL and are not part of the documented description of when the
;;; obsolete instance update happens.  The documented description is as it
;;; appears in 88-002R.
;;;
;;; This has to return the new wrapper, so it counts on all the methods on
;;; obsolete-instance-trap-internal to return the new wrapper.  It also does
;;; a little internal error checking to make sure that the traps are only
;;; happening when they should, and that the trap methods are computing
;;; apropriate new wrappers.
;;; 

;;; obsolete-instance-trap might be called on structure instances
;;; after a structure is redefined.  In most cases, obsolete-instance-trap
;;; will not be able to fix the old instance, so it must signal an
;;; error.  The hard part of this is that the error system and debugger
;;; might cause obsolete-instance-trap to be called again, so in that
;;; case, we have to return some reasonable wrapper, instead.

(defvar *in-obsolete-instance-trap* nil)

(defvar *the-wrapper-of-structure-object* 
  (class-wrapper (find-class 'structure-object)))

(define-condition obsolete-structure (error)
  ((datum :reader obsolete-structure-datum :initarg :datum))
  (:report
   (lambda (condition stream)
     ;; Don't try to print the structure, since it probably
     ;; won't work.
     (format stream "~@<Obsolete structure error in ~S ~
		     for a structure of type ~S.~@:>"
	     (conditions::condition-function-name condition)
	     (type-of (obsolete-structure-datum condition))))))

(defun obsolete-instance-trap (owrapper nwrapper instance)
  (if (not (pcl-instance-p instance))
      (if *in-obsolete-instance-trap*
          *the-wrapper-of-structure-object* 
           (let ((*in-obsolete-instance-trap* t))
	     (error 'obsolete-structure :datum instance)))
      (let* ((class (wrapper-class* nwrapper))
	     (copy (allocate-instance class)) ;??? allocate-instance ???
	     (olayout (wrapper-instance-slots-layout owrapper))
	     (nlayout (wrapper-instance-slots-layout nwrapper))
	     (oslots (get-slots instance))
	     (nslots (get-slots copy))
	     (oclass-slots (wrapper-class-slots owrapper))
	     (added ())
	     (discarded ())
	     (plist ()))
        ;;
        ;; Collect inherited class slots.  Note that LAYOUT-INHERITS is
        ;; ordered from most general to most specific.
        (loop for layout across (reverse (kernel:layout-inherits owrapper))
              when (typep layout 'wrapper) do
                (loop for slot in (wrapper-class-slots layout) do
                        (pushnew slot oclass-slots :key #'car)))
        ;;
	;; local  --> local        transfer 
	;; local  --> shared       discard
	;; local  -->  --          discard
	;; shared --> local        transfer
	;; shared --> shared       discard
	;; shared -->  --          discard
	;;  --    --> local        add
	;;  --    --> shared        --
	;;
	;; Go through all the old local slots.
	;;
	(loop for name in olayout and opos from 0
	      as npos = (posq name nlayout)
	      if npos do
	        (setf (slot-ref nslots npos)
		      (slot-ref oslots opos))
	      else do
		(push name discarded)
		(unless (eq (slot-ref oslots opos) +slot-unbound+)
		  (setf (getf plist name)
			(slot-ref oslots opos))))
	;;
	;; Go through all the old shared slots.
	;;
	(loop for (name . val) in oclass-slots
	      for npos = (posq name nlayout)
	      if npos do
	        (setf (slot-ref nslots npos) val)
	      else do
	        (push name discarded)
		(unless (eq val +slot-unbound+)
		  (setf (getf plist name) val)))
	;;
	;; Go through all the new local slots to compute the added slots.
	;; 
	(dolist (nlocal nlayout)
	  (unless (or (memq nlocal olayout)
		      (assq nlocal oclass-slots))
	    (push nlocal added)))
      
	(swap-wrappers-and-slots instance copy)

	(update-instance-for-redefined-class instance
					     added
					     discarded
					     plist)
	nwrapper)))


;;;
;;;
;;;
(defun change-class-internal (instance new-class initargs)
  (let* ((old-class (class-of instance))
	 (copy (allocate-instance new-class))
	 (new-wrapper (get-wrapper copy))
	 (old-wrapper (class-wrapper old-class))
	 (old-layout (wrapper-instance-slots-layout old-wrapper))
	 (new-layout (wrapper-instance-slots-layout new-wrapper))
	 (old-slots (get-slots instance))
	 (new-slots (get-slots copy))
	 (old-class-slots (wrapper-class-slots old-wrapper)))

    ;;
    ;; "The values of local slots specified by both the class Cto and
    ;; Cfrom are retained.  If such a local slot was unbound, it remains
    ;; unbound."
    ;;
    (loop for new-slot in new-layout and new-position from 0
	  for old-position = (posq new-slot old-layout)
	  when old-position do
	    (setf (slot-ref new-slots new-position)
		  (slot-ref old-slots old-position)))
    ;;
    ;; "The values of slots specified as shared in the class Cfrom and
    ;; as local in the class Cto are retained."
    ;;
    (loop for (name . val) in old-class-slots
	  for new-position = (posq name new-layout)
	  when new-position do
	    (setf (slot-ref new-slots new-position) val))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    (swap-wrappers-and-slots instance copy)

    (apply #'update-instance-for-different-class copy instance initargs)
    instance))

(defmethod change-class ((instance standard-object)
			 (new-class standard-class)
			 &rest initargs &key)
  (change-class-internal instance new-class initargs))

(defmethod change-class ((instance funcallable-standard-object)
			 (new-class funcallable-standard-class)
			 &rest initargs &key)
  (change-class-internal instance new-class initargs))

(defmethod change-class ((instance standard-object)
			 (new-class funcallable-standard-class)
			 &rest initargs &key)
  (declare (ignore initargs))
  (error "~@<Can't change the class of ~S to ~S ~
          because it isn't already an instance with metaclass ~S.~@:>"
	 instance new-class 'standard-class))

(defmethod change-class ((instance funcallable-standard-object)
			 (new-class standard-class)
			 &rest initargs &key)
  (declare (ignore initargs))
  (error "~@<Can't change the class of ~S to ~S ~
          because it isn't already an instance with metaclass ~S.~@:>"
	 instance new-class 'funcallable-standard-class))

(defmethod change-class ((instance t) (new-class-name symbol)
			 &rest initargs &key)
  (apply #'change-class instance (find-class new-class-name) initargs))



;;;
;;; The metaclass BUILT-IN-CLASS
;;;
;;; This metaclass is something of a weird creature.  By this point, all
;;; instances of it which will exist have been created, and no instance
;;; is ever created by calling MAKE-INSTANCE.
;;;
;;; But, there are other parts of the protcol we must follow and those
;;; definitions appear here.
;;; 
(defmethod shared-initialize :before
	   ((class built-in-class) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (error "Attempt to initialize or reinitialize a built-in class."))

(defmethod class-direct-slots            ((class built-in-class)) ())
(defmethod class-slots                   ((class built-in-class)) ())
(defmethod class-direct-default-initargs ((class built-in-class)) ())
(defmethod class-default-initargs        ((class built-in-class)) ())

(defmethod validate-superclass ((c class) (s built-in-class))
  (or (eq s *the-class-t*)
      (eq s *the-class-stream*)))



;;;
;;;
;;;

(macrolet ((frob (method)
	   `(defmethod ,method ((class forward-referenced-class))
	      (declare (ignore class))
	      ())))
  (frob class-direct-slots)
  (frob class-direct-default-initargs))

(macrolet ((frob (method)
	     `(defmethod ,method ((class forward-referenced-class))
		(error "~@<~S called for forward referenced class ~S.~@:>"
		       ',method class))))
  (frob class-default-initargs)
  (frob class-precedence-list)
  (frob class-slots))
  
(defmethod validate-superclass ((c slot-class)
				(f forward-referenced-class))
  t)


;;;
;;;
;;;

(defmethod add-dependent ((metaobject dependent-update-mixin) dependent)
  (pushnew dependent (plist-value metaobject 'dependents)))

(defmethod remove-dependent ((metaobject dependent-update-mixin) dependent)
  (setf (plist-value metaobject 'dependents)
	(delete dependent (plist-value metaobject 'dependents))))

(defmethod map-dependents ((metaobject dependent-update-mixin) function)
  (dolist (dependent (plist-value metaobject 'dependents))
    (funcall function dependent)))

(defmethod update-dependent (metaobject dependent &rest initargs)
  (declare (ignore initargs)))


;;;
;;; Conditions
;;;
(defmethod class-default-initargs ((class condition-class)) ())
(defmethod class-direct-default-initargs ((class condition-class)) ())

(defmethod shared-initialize :after ((class condition-class) slot-names
				     &key direct-slots direct-superclasses)
  (declare (ignore slot-names))
  (let ((kernel-class (kernel::find-class (class-name class))))
    (with-slots (wrapper class-precedence-list prototype predicate-name
			 (direct-supers direct-superclasses))
	class
      (setf (slot-value class 'direct-slots)
	    (mapcar (lambda (pl) (make-direct-slotd class pl))
		    direct-slots))
      (setf (slot-value class 'finalized-p) t)
      (setf (kernel:%class-pcl-class kernel-class) class)
      (setq direct-supers direct-superclasses)
      (setq wrapper (kernel:%class-layout kernel-class))
      (setq class-precedence-list (compute-class-precedence-list class))
      (add-direct-subclasses class direct-superclasses)
      (setq predicate-name (make-class-predicate-name (class-name class)))
      (make-class-predicate class predicate-name)
      (setf (slot-value class 'slots) (compute-slots class))))
  ;;
  ;; We don't ADD-SLOT-ACCESSORS here because we don't want to
  ;; override condition accessors with generic functions.  We do this
  ;; differently.
  (update-pv-table-cache-info class))
				     
(defmethod direct-slot-definition-class
    ((class condition-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'condition-direct-slot-definition))

(defmethod effective-slot-definition-class
    ((class condition-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'condition-effective-slot-definition))

(defmethod finalize-inheritance ((class condition-class))
  nil)

(defmethod compute-effective-slot-definition
    ((class condition-class) slot-name dslotds)
  (let ((slotd (call-next-method)))
    (setf (slot-definition-reader-function slotd)
	  (lambda (x)
	    (handler-case 
		(conditions::condition-reader-function x slot-name)
	      (error () (values (slot-unbound class x slot-name))))))
    (setf (slot-definition-writer-function slotd)
	  (lambda (v x)
	    (conditions::condition-writer-function x v slot-name)))
    (setf (slot-definition-boundp-function slotd)
	  (lambda (x)
	    (multiple-value-bind (v c)
		(ignore-errors
		  (conditions::condition-reader-function x slot-name))
	      (declare (ignore v))
	      (null c))))
    slotd))

(defmethod compute-slots ((class condition-class))
  (mapcan (lambda (superclass)
	    (mapcar (lambda (dslotd)
		      (compute-effective-slot-definition
		       class (slot-definition-name dslotd) (list dslotd)))
		    (class-direct-slots superclass)))
	  (reverse (slot-value class 'class-precedence-list))))

(defmethod compute-slots :around ((class condition-class))
  (let ((eslotds (call-next-method)))
    (mapc #'initialize-internal-slot-functions eslotds)
    eslotds))
