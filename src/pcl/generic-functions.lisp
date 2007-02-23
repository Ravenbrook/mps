;;;-*-Mode:LISP; Package:PCL; Base:10; Syntax:Common-lisp -*-

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/generic-functions.lisp,v 1.28 2003/08/25 20:10:41 gerd Exp $")
;;;

(in-package :pcl)

;;; class predicates
(defgeneric class-eq-specializer-p (object))
;          (t)
;          (class-eq-specializer)

(defgeneric classp (object))
;          (t)
;          (class)

(defgeneric eql-specializer-p (object))
;          (t)
;          (eql-specializer)

(defgeneric exact-class-specializer-p (object))
;          (t)
;          (exact-class-specializer)

(defgeneric forward-referenced-class-p (object))
;          (t)
;          (forward-referenced-class)

(defgeneric funcallable-standard-class-p (object))
;          (t)
;          (funcallable-standard-class)

(defgeneric generic-function-p (object))
;          (t)
;          (generic-function)

(defgeneric legal-lambda-list-p (object x))
;          (standard-method t)

(defgeneric method-combination-p (object))
;          (t)
;          (method-combination)

(defgeneric method-p (object))
;          (t)
;          (method)

(defgeneric short-method-combination-p (object))
;          (short-method-combination)
;          (t)

(defgeneric slot-class-p (object))
;          (t)
;          (slot-class)

(defgeneric specializerp (object))
;          (t)
;          (specializer)

(defgeneric standard-accessor-method-p (object))
;          (t)
;          (standard-accessor-method)

(defgeneric standard-boundp-method-p (object))
;          (t)
;          (standard-boundp-method)

(defgeneric standard-class-p (object))
;          (t)
;          (standard-class)

(defgeneric standard-generic-function-p (object))
;          (t)
;          (standard-generic-function)

(defgeneric standard-method-p (object))
;          (t)
;          (standard-method)

(defgeneric standard-reader-method-p (object))
;          (t)
;          (standard-reader-method)

(defgeneric standard-writer-method-p (object))
;          (t)
;          (standard-writer-method)

(defgeneric structure-class-p (object))
(defgeneric condition-class-p (object))

;;; readers
(defgeneric accessor-method-slot-definition (standard-accessor-method))
;          (standard-accessor-method)

(defgeneric class-can-precede-list (pcl-class))
;          (pcl-class)

(defgeneric class-defstruct-constructor (structure-class))
;          (structure-class)

(defgeneric class-defstruct-form (structure-class))
;          (structure-class)

(defgeneric class-direct-subclasses (class))
;          (class)

(defgeneric class-direct-superclasses (class))
;          (class)

(defgeneric class-eq-specializer (class))
;          (class)

(defgeneric class-incompatible-superclass-list (pcl-class))
;          (pcl-class)

(defgeneric class-name (class))
;          (class)

(defgeneric class-precedence-list (pcl-class))
;          (pcl-class)

(defgeneric class-predicate-name (class))
;          (class)

(defgeneric class-wrapper (pcl-class))
;          (pcl-class)

(defgeneric definition-source (definition-source-mixin))
;          (definition-source-mixin)

(defgeneric eql-specializer-object (eql-specializer))
;          (eql-specializer)

(defgeneric generic-function-method-class (standard-generic-function))
;          (standard-generic-function)

(defgeneric generic-function-method-combination (standard-generic-function))
;          (standard-generic-function)

(defgeneric generic-function-methods (standard-generic-function))
;          (standard-generic-function)

(defgeneric generic-function-name (standard-generic-function))
;          (standard-generic-function)

(defgeneric generic-function-argument-precedence-order (generic-function))
;          (standard-generic-function)

(defgeneric gf-arg-info (standard-generic-function))
;          (standard-generic-function)

(defgeneric gf-dfun-state (standard-generic-function))
;          (standard-generic-function)

(defgeneric gf-pretty-arglist (standard-generic-function))
;          (standard-generic-function)

(defgeneric long-method-combination-function (long-method-combination))
;          (long-method-combination)

(defgeneric method-combination-documentation (standard-method-combination))
;          (standard-method-combination)

(defgeneric method-combination-options (standard-method-combination))
;          (standard-method-combination)

(defgeneric method-combination-type (standard-method-combination))
;          (standard-method-combination)

(defgeneric method-fast-function (standard-method))
;          (standard-method)

(defgeneric method-generic-function (standard-method))
;          (traced-method)
;          (standard-method)

(defgeneric object-plist (plist-mixin))
;          (plist-mixin)

(defgeneric short-combination-identity-with-one-argument (short-method-combination))
;          (short-method-combination)

(defgeneric short-combination-operator (short-method-combination))
;          (short-method-combination)

(defgeneric slot-definition-boundp-function (effective-slot-definition))
;          (effective-slot-definition)

(defgeneric slot-definition-class (slot-definition))
;          (slot-definition)

(defgeneric slot-definition-defstruct-accessor-symbol (structure-slot-definition))
;          (structure-slot-definition)

(defgeneric slot-definition-initargs (slot-definition))
;          (slot-definition)

(defgeneric slot-definition-initform (slot-definition))
;          (slot-definition)

(defgeneric slot-definition-initfunction (slot-definition))
;          (slot-definition)

(defgeneric slot-definition-internal-reader-function (structure-slot-definition))
;          (structure-slot-definition)

(defgeneric slot-definition-internal-writer-function (structure-slot-definition))
;          (structure-slot-definition)

(defgeneric slot-definition-location (standard-effective-slot-definition))
;          (standard-effective-slot-definition)

(defgeneric slot-definition-name (slot-definition))
;          (slot-definition)

(defgeneric slot-definition-reader-function (effective-slot-definition))
;          (effective-slot-definition)

(defgeneric slot-definition-readers (slot-definition))
;          (slot-definition)

(defgeneric slot-definition-type (slot-definition))
;          (slot-definition)

(defgeneric slot-definition-writer-function (effective-slot-definition))
;          (effective-slot-definition)

(defgeneric slot-definition-writers (slot-definition))
;          (slot-definition)

(defgeneric specializer-object (class-eq-specializer))
;          (eql-specializer)
;          (class-prototype-specializer)
;          (class-eq-specializer)

(defgeneric specializer-type (specializer))
;          (specializer)


;;; writers
(defgeneric (setf class-defstruct-constructor) (new-value structure-class))
;          (t structure-class)

(defgeneric (setf class-defstruct-form) (new-value structure-class))
;          (t structure-class)

(defgeneric (setf class-direct-slots) (new-value slot-class))
;          (t slot-class)

(defgeneric (setf class-incompatible-superclass-list) (new-value pcl-class))
;          (t pcl-class)

(defgeneric (setf class-name) (new-value class))
;          (t class)

(defgeneric (setf class-slots) (new-value slot-class))
;          (t slot-class)

(defgeneric (setf generic-function-method-class) (new-value standard-generic-function))
;          (t standard-generic-function)

(defgeneric (setf generic-function-method-combination) (new-value standard-generic-function))
;          (t standard-generic-function)

(defgeneric (setf generic-function-methods) (new-value standard-generic-function))
;          (t standard-generic-function)

(defgeneric (setf generic-function-name) (new-value standard-generic-function))
;          (t standard-generic-function)

(defgeneric (setf gf-dfun-state) (new-value standard-generic-function))
;          (t standard-generic-function)

(defgeneric (setf gf-pretty-arglist) (new-value standard-generic-function))
;          (t standard-generic-function)

(defgeneric (setf method-generic-function) (new-value standard-method))
;          (t traced-method)
;          (t standard-method)

(defgeneric (setf object-plist) (new-value plist-mixin))
;          (t plist-mixin)

(defgeneric (setf slot-definition-allocation) (new-value standard-slot-definition))
;          (t standard-slot-definition)

(defgeneric (setf slot-definition-boundp-function) (new-value effective-slot-definition))
;          (t effective-slot-definition)

(defgeneric (setf slot-definition-class) (new-value slot-definition))
;          (t slot-definition)

(defgeneric (setf slot-definition-defstruct-accessor-symbol) (new-value structure-slot-definition))
;          (t structure-slot-definition)

(defgeneric (setf slot-definition-initargs) (new-value slot-definition))
;          (t slot-definition)

(defgeneric (setf slot-definition-initform) (new-value slot-definition))
;          (t slot-definition)

(defgeneric (setf slot-definition-initfunction) (new-value slot-definition))
;          (t slot-definition)

(defgeneric (setf slot-definition-internal-reader-function) (new-value structure-slot-definition))
;          (t structure-slot-definition)

(defgeneric (setf slot-definition-internal-writer-function) (new-value structure-slot-definition))
;          (t structure-slot-definition)

(defgeneric (setf slot-definition-location) (new-value standard-effective-slot-definition))
;          (t standard-effective-slot-definition)

(defgeneric (setf slot-definition-name) (new-value slot-definition))
;          (t slot-definition)

(defgeneric (setf slot-definition-reader-function) (new-value effective-slot-definition))
;          (t effective-slot-definition)

(defgeneric (setf slot-definition-readers) (new-value slot-definition))
;          (t slot-definition)

(defgeneric (setf slot-definition-type) (new-value slot-definition))
;          (t slot-definition)

(defgeneric (setf slot-definition-writer-function) (new-value effective-slot-definition))
;          (t effective-slot-definition)

(defgeneric (setf slot-definition-writers) (new-value slot-definition))
;          (t slot-definition)


;;; 1 argument 
(defgeneric accessor-method-class (method))
;          (standard-accessor-method)
;          (standard-writer-method)

(defgeneric accessor-method-slot-name (m))
;          (traced-method)
;          (standard-accessor-method)

(defgeneric class-constructors (class))
;          (slot-class)

(defgeneric class-default-initargs (class))
;          (slot-class)
;          (built-in-class)

(defgeneric class-direct-default-initargs (class))
;          (slot-class)
;          (built-in-class)

(defgeneric class-direct-slots (class))
;          (slot-class)
;          (built-in-class)

(defgeneric class-finalized-p (class))
;          (pcl-class)

(defgeneric class-prototype (class))
;          (pcl-class)
;          (std-class)
;          (structure-class)

(defgeneric class-slot-cells (class))
;          (std-class)

(defgeneric class-slots (class))
;          (slot-class)
;          (built-in-class)

(defgeneric compute-class-precedence-list (root))
;          (slot-class)

(defgeneric compute-default-initargs (class))
;          (slot-class)

(defgeneric compute-discriminating-function (gf))
;          (standard-generic-function)

(defgeneric compute-discriminating-function-arglist-info (generic-function))
;          (standard-generic-function)

(defgeneric compute-slots (class))
;          (std-class)
;  :around (std-class)
;          (structure-class)
;  :around (structure-class)

(defgeneric default-initargs (class initargs defaults))

(defgeneric finalize-inheritance (class))
;          (structure-class)
;          (std-class)

(defgeneric function-keywords (method))
;          (standard-method)

(defgeneric generic-function-lambda-list (gf))
;          (generic-function)

(defgeneric gf-fast-method-function-p (gf))
;          (standard-generic-function)

(defgeneric initialize-internal-slot-functions (slotd))
;          (effective-slot-definition)

(defgeneric make-instances-obsolete (class))
;          (std-class)
;          (symbol)

(defgeneric method-function (method))
;          (traced-method)
;          (standard-method)

(defgeneric method-lambda-list (m))
;          (traced-method)
;          (standard-method)

(defgeneric method-qualifiers (m))
;          (traced-method)
;          (standard-method)

(defgeneric method-specializers (m))
;          (traced-method)
;          (standard-method)

(defgeneric raw-instance-allocator (class))
;          (standard-class)
;          (funcallable-standard-class)

(defgeneric slot-definition-allocation (slotd))
;          (standard-slot-definition)
;          (structure-slot-definition)

(defgeneric slots-fetcher (class))
;          (standard-class)
;          (funcallable-standard-class)

(defgeneric specializer-class (specializer))
;          (class-prototype-specializer)
;          (class-eq-specializer)
;          (class)
;          (eql-specializer)

(defgeneric specializer-direct-generic-functions (specializer))
;          (class)
;          (specializer-with-object)

(defgeneric specializer-direct-methods (specializer))
;          (class)
;          (specializer-with-object)

(defgeneric specializer-method-table (specializer))
;          (eql-specializer)
;          (class-eq-specializer)

(defgeneric update-constructors (class))
;          (slot-class)
;          (class)

(defgeneric wrapper-fetcher (class))
;          (standard-class)
;          (funcallable-standard-class)


;;; 2 arguments 
(defgeneric add-dependent (metaobject dependent))
;          (dependent-update-mixin t)

(defgeneric add-direct-method (specializer method))
;          (class method)
;          (specializer-with-object method)

(defgeneric add-direct-subclass (class subclass))
;          (class class)

(defgeneric add-method (generic-function method))
;          (standard-generic-function method)

(defgeneric change-class (instance new-class-name &rest initargs
				   &key &allow-other-keys))
;          (standard-object standard-class)
;          (funcallable-standard-object funcallable-standard-class)
;          (t symbol)

(defgeneric compatible-meta-class-change-p (class proto-new-class))
;          (t t)

(defgeneric compute-applicable-methods (generic-function arguments))
;          (generic-function t)

(defgeneric compute-applicable-methods-using-classes (generic-function classes))
;          (generic-function t)

(defgeneric compute-effective-slot-definition (class slot-name dslotds))

(defgeneric compute-effective-slot-definition-initargs (class direct-slotds))
;          (slot-class t)
;  :around (structure-class t)

(defgeneric describe-object (object stream))
;          (class t)
;          (standard-generic-function t)
;          (slot-object t)
;          (t t)

(defgeneric direct-slot-definition-class (class &rest initargs))
;          (structure-class t)
;          (std-class t)

(defgeneric effective-slot-definition-class (class &rest initargs))
;          (std-class t)
;          (structure-class t)

(defgeneric inform-type-system-about-class (class name))
;          (std-class t)
;          (structure-class t)

(defgeneric legal-documentation-p (object x))
;          (standard-method t)

(defgeneric legal-method-function-p (object x))
;          (standard-method t)

(defgeneric legal-qualifier-p (object x))
;          (standard-method t)

(defgeneric legal-qualifiers-p (object x))
;          (standard-method t)

(defgeneric legal-slot-name-p (object x))
;          (standard-method t)

(defgeneric legal-specializer-p (object x))
;          (standard-method t)

(defgeneric legal-specializers-p (object x))
;          (standard-method t)

(defgeneric make-boundp-method-function (class slot-name))
;          (slot-class t)

(defgeneric make-reader-method-function (class slot-name))
;          (slot-class t)
;          (funcallable-standard-class t)

(defgeneric make-writer-method-function (class slot-name))
;          (slot-class t)
;          (funcallable-standard-class t)

(defgeneric map-dependents (metaobject function))
;          (dependent-update-mixin t)

(defgeneric no-next-method (generic-function method &rest args))
(defgeneric no-primary-method (generic-function &rest args))

;(defgeneric maybe-update-constructors (generic-function method))
;           (generic-function method)

(defgeneric print-object (mc stream))
;          (t t)
;          (class t)
;          (slot-definition t)
;          (standard-method t)
;          (standard-accessor-method t)
;          (generic-function t)
;          (standard-method-combination t)

(defgeneric remove-boundp-method (class generic-function))
;          (slot-class t)

(defgeneric remove-dependent (metaobject dependent))
;          (dependent-update-mixin t)

(defgeneric remove-direct-method (specializer method))
;          (class method)
;          (specializer-with-object method)

(defgeneric remove-direct-subclass (class subclass))
;          (class class)

(defgeneric remove-method (generic-function method))
;          (standard-generic-function method)

(defgeneric remove-reader-method (class generic-function))
;          (slot-class t)

(defgeneric remove-writer-method (class generic-function))
;          (slot-class t)

(defgeneric same-specializer-p (specl1 specl2))
;          (specializer specializer)
;          (class class)
;          (class-eq-specializer class-eq-specializer)
;          (eql-specializer eql-specializer)

(defgeneric slot-accessor-function (slotd type))
;          (effective-slot-definition t)

(defgeneric slot-accessor-std-p (slotd type))
;          (effective-slot-definition t)

(defgeneric slots-to-inspect (class object))
;          (slot-class slot-object)

(defgeneric update-gf-dfun (class gf))
;          (std-class t)

(defgeneric validate-superclass (fsc class))
;          (class class)
;          (class built-in-class)
;          (slot-class forward-referenced-class)
;          (funcallable-standard-class standard-class)

(defgeneric (setf documentation) (new-value slotd doc-type)
  (:argument-precedence-order doc-type slotd new-value))
;          (t t)
;          (t documentation-mixin)
;          (t standard-slot-definition)

(defgeneric documentation (slotd doc-type)
  (:argument-precedence-order doc-type slotd))
;          (t)
;          (documentation-mixin)
;          (standard-slot-definition)


;;; 3 arguments 
(defgeneric add-boundp-method (class generic-function slot-name))
;          (slot-class t t)

(defgeneric add-reader-method (class generic-function slot-name))
;          (slot-class t t)

(defgeneric add-writer-method (class generic-function slot-name))
;          (slot-class t t)

;;;
;;; According to AMOP, COMPUTE-EFFECTIVE-METHOD should return two
;;; values.  Alas, the second value is only vaguely described in AMOP,
;;; and, when asked on 2002-10-18, Gregor Kiczales said he couldn't
;;; remember what the second value was supposed to be.  So, PCL's
;;; COMPUTE-EFFECTIVE-METHOD returns one value as do Allegro and
;;; Lispworks.
;;;
(defgeneric compute-effective-method (generic-function combin applicable-methods))
;          (generic-function long-method-combination t)
;          (generic-function short-method-combination t)
;          (generic-function standard-method-combination t)

(defgeneric compute-slot-accessor-info (slotd type gf))
;          (effective-slot-definition t t)

(defgeneric find-method-combination (generic-function type options))
;          (generic-function (eql progn) t)
;          (generic-function (eql or) t)
;          (generic-function (eql nconc) t)
;          (generic-function (eql min) t)
;          (generic-function (eql max) t)
;          (generic-function (eql list) t)
;          (generic-function (eql append) t)
;          (generic-function (eql and) t)
;          (generic-function (eql +) t)
;          (generic-function (eql standard) t)

(defgeneric (setf slot-accessor-function) (function slotd type))
;          (t effective-slot-definition t)

(defgeneric (setf slot-accessor-std-p) (value slotd type))
;          (t effective-slot-definition t)

(defgeneric slot-boundp-using-class (class object slotd))

(defgeneric slot-makunbound-using-class (class object slotd))

(defgeneric slot-unbound (class instance slot-name))
;          (t t t)

(defgeneric slot-value-using-class (class object slotd))


;;; 4 arguments 
(defgeneric make-method-lambda (proto-generic-function proto-method lambda-expression environment))
;          (standard-generic-function standard-method t t)

(defgeneric (setf slot-value-using-class) (new-value class object slotd))


;;; 5 arguments 
(defgeneric make-method-initargs-form (proto-generic-function proto-method lambda-expression lambda-list environment))
;          (standard-generic-function standard-method t t t)


;;; optional arguments  
(defgeneric get-method (generic-function qualifiers specializers &optional errorp))
;          (standard-generic-function t t)

(defgeneric find-method (generic-function qualifiers specializers &optional errorp))
;          (standard-generic-function t t)

(defgeneric remove-named-method (generic-function-name argument-specifiers &optional extra))
;          (t t)

(defgeneric slot-missing (class instance slot-name operation &optional new-value))
;          (t t t t)


;;; keyword arguments  
(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys))
;          (standard-class)
;          (structure-class)
;          (funcallable-standard-class)

(defgeneric ensure-class-using-class (class name &rest args &key &allow-other-keys))
;          (t null)
;          (t pcl-class)

(defgeneric ensure-generic-function-using-class (generic-function function-specifier &key &allow-other-keys))
;          (null t)
;          (generic-function t)

(defgeneric initialize-instance (gf &key &allow-other-keys))
;          (slot-object)
;  :after  (standard-generic-function)

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))
;          (symbol)
;          (class)

(defgeneric no-applicable-method (generic-function &rest args))
;          (t)

(defgeneric reader-method-class (class direct-slot &rest initargs))
;          (slot-class t)

(defgeneric reinitialize-instance (gf &rest args &key &allow-other-keys))

(defgeneric shared-initialize (generic-function slot-names
						&key &allow-other-keys))

(defgeneric update-dependent (metaobject dependent &rest initargs))

(defgeneric update-instance-for-different-class
    (previous current &rest initargs &key &allow-other-keys))

(defgeneric update-instance-for-redefined-class
    (instance added-slots discarded-slots property-list &rest initargs
	      &key &allow-other-keys))

(defgeneric writer-method-class (class direct-slot &rest initargs))

(defgeneric invalid-qualifiers (gf combin args methods))


