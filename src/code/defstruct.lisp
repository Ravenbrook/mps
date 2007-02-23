;;; -*- Mode: Lisp; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/defstruct.lisp,v 1.96 2005/10/21 17:56:07 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Defstruct structure definition package (Mark III).
;;; Written by Rob MacLachlan, William Lott and Skef Wholey.
;;;
(in-package "LISP")
(export '(defstruct copy-structure structure-object))
(in-package "KERNEL")
(export '(default-structure-print make-structure-load-form 
	  %compiler-defstruct %%compiler-defstruct
	  %compiler-only-defstruct
	  %make-instance
	  %instance-length %instance-ref %instance-set %instance-layout
	  %set-instance-layout
	  %make-funcallable-instance %funcallable-instance-info
	  %set-funcallable-instance-info
	  funcallable-instance-function funcallable-structure
	  funcallable-instance-p
	  %raw-ref-single %raw-set-single
	  %raw-ref-double %raw-set-double
	  defstruct-description dd-name dd-default-constructor dd-copier
	  dd-predicate dd-slots dd-length dd-type dd-raw-index dd-raw-length
	  defstruct-slot-description dsd-name dsd-%name dsd-accessor dsd-type
	  dsd-index dsd-raw-type dsd-read-only undefine-structure
	  *ansi-defstruct-options-p*))


(defparameter *ANSI-defstruct-options-p* nil
  "Controls compiling DEFSTRUCT :print-function and :print-method
   options according to ANSI spec. MUST be NIL to compile CMUCL & PCL")

;;;; Structure frobbing primitives.

(defun %make-instance (length)
  "Allocate a new instance with LENGTH data slots."
  (declare (type index length))
  (%make-instance length))

(defun %instance-length (instance)
  "Given an instance, return its length."
  (declare (type instance instance))
  (%instance-length instance))

(defun %instance-ref (instance index)
  "Return the value from the INDEXth slot of INSTANCE.  This is SETFable."
  (%instance-ref instance index))

(defun %instance-set (instance index new-value)
  "Set the INDEXth slot of INSTANCE to NEW-VALUE."
  (setf (%instance-ref instance index) new-value))

(defun %raw-ref-single (vec index)
  (declare (type index index))
  (%raw-ref-single vec index))

(defun %raw-ref-double (vec index)
  (declare (type index index))
  (%raw-ref-double vec index))

#+long-float
(defun %raw-ref-long (vec index)
  (declare (type index index))
  (%raw-ref-long vec index))

(defun %raw-set-single (vec index val)
  (declare (type index index))
  (%raw-set-single vec index val))

(defun %raw-set-double (vec index val)
  (declare (type index index))
  (%raw-set-double vec index val))

#+long-float
(defun %raw-set-long (vec index val)
  (declare (type index index))
  (%raw-set-long vec index val))

(defun %raw-ref-complex-single (vec index)
  (declare (type index index))
  (%raw-ref-complex-single vec index))

(defun %raw-ref-complex-double (vec index)
  (declare (type index index))
  (%raw-ref-complex-double vec index))

#+long-float
(defun %raw-ref-complex-long (vec index)
  (declare (type index index))
  (%raw-ref-complex-long vec index))

(defun %raw-set-complex-single (vec index val)
  (declare (type index index))
  (%raw-set-complex-single vec index val))

(defun %raw-set-complex-double (vec index val)
  (declare (type index index))
  (%raw-set-complex-double vec index val))

#+long-float
(defun %raw-set-complex-long (vec index val)
  (declare (type index index))
  (%raw-set-complex-long vec index val))

(defun %instance-layout (instance)
  (%instance-layout instance))

(defun %set-instance-layout (instance new-value)
  (%set-instance-layout instance new-value))

(defun %make-funcallable-instance (len layout)
   (%make-funcallable-instance len layout))

(defun funcallable-instance-p (x) (funcallable-instance-p x))

(defun %funcallable-instance-info (fin i)
  (%funcallable-instance-info fin i))

(defun %set-funcallable-instance-info (fin i new-value)
  (%set-funcallable-instance-info fin i new-value))

(defun funcallable-instance-function (fin)
  (%funcallable-instance-lexenv fin))

;;; The heart of the magic of funcallable instances.  The function for a FIN
;;; must be a magical INSTANCE-LAMBDA form.  When called (as with any other
;;; function), we grab the code pointer, and call it, leaving the original
;;; function object in LEXENV (in case it was a closure).  If it is actually a
;;; FIN, then we need to do an extra indirection with
;;; funcallable-instance-lexenv to get at any closure environment.  This extra
;;; indirection is set up when accessing the closure environment of an
;;; INSTANCE-LAMBDA.  Note that the original FIN pointer is lost, so if the
;;; called function wants to get at the original object to do some slot
;;; accesses, it must close over the FIN object.
;;;
;;; If we set the FIN function to be a FIN, we directly copy across both the
;;; code pointer and the lexenv, since that code pointer (for an
;;; instance-lambda) is expecting that lexenv to be accessed.  This effectively
;;; pre-flattens what would otherwise be a chain of indirections.  Lest this
;;; sound like an excessively obscure case, note that it happens when PCL
;;; dispatch functions are byte-compiled.  
;;;
;;; The only loss is that if someone accesses the
;;; funcallable-instance-function, then won't get a FIN back.  This probably
;;; doesn't matter, since PCL only sets the FIN function.  And the only reason
;;; that interpreted functions are FINs instead of bare closures is for
;;; debuggability.
;;;
(defun (setf funcallable-instance-function) (new-value fin)
  (setf (%funcallable-instance-function fin)
	(%closure-function new-value))
  (setf (%funcallable-instance-lexenv fin)
	(if (funcallable-instance-p new-value)
	    (%funcallable-instance-lexenv new-value)
	    new-value)))

(defsetf %instance-ref %instance-set)
(defsetf %raw-ref-single %raw-set-single)
(defsetf %raw-ref-double %raw-set-double)
#+long-float
(defsetf %raw-ref-long %raw-set-long)
(defsetf %raw-ref-complex-single %raw-set-complex-single)
(defsetf %raw-ref-complex-double %raw-set-complex-double)
#+long-float
(defsetf %raw-ref-complex-long %raw-set-complex-long)
(defsetf %instance-layout %set-instance-layout)
(defsetf %funcallable-instance-info %set-funcallable-instance-info)


;;; This version of Defstruct is implemented using Defstruct, and is free of
;;; Maclisp compatability nonsense.  For bootstrapping, you're on your own.

;;; The DEFSTRUCT-DESCRIPTION structure holds compile-time information about a
;;; structure type.
;;;
(defstruct (defstruct-description
             (:conc-name dd-)
             (:print-function print-defstruct-description)
	     (:make-load-form-fun :just-dump-it-normally)
	     (:pure t)
	     (:constructor make-defstruct-description (name)))
  ;;
  ;; name of the structure
  (name (required-argument) :type symbol)
  ;;
  ;; documentation on the structure
  (doc nil :type (or string null))
  ;;
  ;; prefix for slot names.  If NIL, none.
  (conc-name (concat-pnames name '-) :type (or symbol null))
  ;;
  ;; The name of the primary standard keyword constructor, or NIL if none.
  (default-constructor nil :type (or symbol null))
  ;;
  ;; All the explicit :CONSTRUCTOR specs, with name defaulted.
  (constructors () :type list)
  ;;
  ;; name of copying function
  (copier (concat-pnames 'copy- name) :type (or symbol null))
  ;;
  ;; Name of type predictate
  (predicate (concat-pnames name '-p) :type (or symbol null))
  ;;
  ;; The arguments to the :INCLUDE option, or NIL if no included structure.
  (include nil :type list)
  ;;
  ;; The arguments to the :ALTERNATE-METACLASS option (an extension used to
  ;; define structure-like objects with an arbitrary superclass and that may
  ;; not have STRUCTURE-CLASS as the metaclass.)  Syntax is:
  ;;    (superclass-name metaclass-name metaclass-constructor)
  ;;
  (alternate-metaclass nil :type list)
  ;;
  ;; list of defstruct-slot-description objects for all slots (including
  ;; included ones.)
  (slots () :type list)
  ;;
  ;; Number of elements we've allocated (see also raw-length.)
  (length 0 :type index)
  ;;
  ;; General kind of implementation.
  (type 'structure :type (member structure vector list
				 funcallable-structure))
  ;;
  ;; The next three slots are for :TYPE'd structures (which aren't classes,
  ;; CLASS-STRUCTURE-P = NIL)
  ;;
  ;; Vector element type.
  (element-type 't)
  ;;
  ;; T if :NAMED was explicitly specified, Nil otherwise.
  (named nil :type boolean)
  ;;
  ;; Any INITIAL-OFFSET option on this direct type.
  (offset nil :type (or index null))
  ;;
  ;; The argument to the PRINT-FUNCTION option, or NIL if none.  If we see an
  ;; explicit (:PRINT-FUNCTION) option, then this is DEFAULT-STRUCTURE-PRINT.
  ;; See also BASIC-STRUCTURE-CLASS-PRINTER.  Only for classed structures.
  ;;
  (print-function nil :type (or cons symbol null))
  ;;
  ;; The next four slots are only meaningful in real default structures (TYPE =
  ;; STRUCTURE).
  ;;
  ;; Make-load-form function option.  See also STRUCTURE-CLASS-LOAD-FORM-MAKER.
  (make-load-form-fun nil :type (or symbol cons null))
  ;;
  ;; The index of the raw data vector and the number of words in it.  NIL and 0
  ;; if not allocated yet.
  (raw-index nil :type (or index null))
  (raw-length 0 :type index)
  ;;
  ;; Value of the :PURE option, or :UNSPECIFIED.  Only meaningful if
  ;; CLASS-STRUCTURE-P = T.
  (pure :unspecified :type (member t nil :substructure :unspecified))
   ;;
   ;; a list of (NAME . INDEX) pairs for accessors of included structures
   (inherited-accessor-alist () :type list))

(defun print-defstruct-description (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Defstruct-Description for ~S>" (dd-name structure)))

;;; DEFSTRUCT-SLOT-DESCRIPTION  holds compile-time information about structure
;;; slots.
;;;
(defstruct (defstruct-slot-description
             (:conc-name dsd-)
             (:print-function print-defstruct-slot-description)
	     (:pure t)
	     (:make-load-form-fun :just-dump-it-normally))
  ;;
  ;; The name of the slot, a symbol.
  name
  ;;
  ;; its position in the implementation sequence
  (index (required-argument) :type fixnum)
  ;;
  ;; Name of accessor.
  (accessor nil)
  default			; default value expression
  (type t)			; declared type specifier
  ;;
  ;; If a raw slot, what it holds.  T means not raw.
  (raw-type t :type (member t single-float double-float #+long-float long-float
			    complex-single-float complex-double-float
			    #+long-float complex-long-float
			    unsigned-byte))
  (read-only nil :type (member t nil)))

(defun print-defstruct-slot-description (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Defstruct-Slot-Description for ~S>" (dsd-name structure)))

(defun dsd-%name (dsd)
  (symbol-name (dsd-name dsd)))

;;; CLASS-STRUCTURE-P  --  Internal
;;;
;;;    Return true if Defstruct is a structure with a class.
;;;
(defun class-structure-p (defstruct)
  (member (dd-type defstruct) '(structure funcallable-structure)))


;;; COMPILER-LAYOUT-OR-LOSE  --  Internal
;;;
;;;    Return the compiler layout for Name.  Must be a structure-like class.
;;;
(defun compiler-layout-or-lose (name)
  (let ((res (info type compiler-layout name)))
    (cond ((not res)
	   (error "Class not yet defined or was undefined: ~S" name))
	  ((not (typep (layout-info res) 'defstruct-description))
	   (error "Class is not a structure class: ~S" name))
	  (t res))))

(defun dd-maybe-make-print-method (defstruct)
  ;; Maybe generate CLOS DEFMETHOD forms for :print-function/:print-object.
  (let ((print-function-value (dd-print-function defstruct)))
    (when (consp print-function-value)
      (let ((kind (car print-function-value))
	    (function (cdr print-function-value)))
	(unless (eq kind 'lambda)
	  (setf (dd-print-function defstruct) nil)
	  (let* ((class (dd-name defstruct))
		 (func (if (symbolp function) `',function `#',function)))
	    ;; We can only generate this code if CLOS is loaded. Maybe should
	    ;; signal an error instead of quietly ignoring the defmethod?
	    `((when (fboundp 'print-object)
		(defmethod print-object ((object ,class) stream)
		  (funcall
		   ,func object stream
		   ,@(when (or (eq kind :print-function)
			       (eq function 'default-structure-print))
		       '(*current-level*))))))))))))


;;; The legendary macro itself.

;;; DEFINE-CLASS-METHODS  --  Internal
;;;
;;; Return a list of forms to install print and make-load-form funs, mentioning
;;; them in the expansion so that they can be compiled.
;;;

(defun define-class-methods (defstruct)
  (let* ((name (dd-name defstruct))
	 (pom (dd-maybe-make-print-method defstruct)))
    `(,@(let ((pf (dd-print-function defstruct)))
	  (when pf
	    `((setf (basic-structure-class-print-function (find-class ',name))
		    ,(if (symbolp pf)
			 `',pf
			 `#',pf)))))
      ,@(let ((mlff (dd-make-load-form-fun defstruct)))
	  (when mlff
	    `((setf (structure-class-make-load-form-fun (find-class ',name))
		    ,(if (symbolp mlff)
			 `',mlff
			 `#',mlff)))))
      ,@(let ((pure (dd-pure defstruct)))
	  (cond ((eq pure 't)
		 `((setf (layout-pure (%class-layout (find-class ',name)))
		    t)))
		((eq pure :substructure)
		 `((setf (layout-pure (%class-layout (find-class ',name)))
		    0)))))
      ,@(let ((def-con (dd-default-constructor defstruct)))
	  (when (and def-con (not (dd-alternate-metaclass defstruct)))
	    `((setf (structure-class-constructor (find-class ',name))
		    #',def-con))))
      ,@pom)))

#+ORIGINAL
(defun define-class-methods (defstruct)
  (let ((name (dd-name defstruct)))
    `(,@(let ((pf (dd-print-function defstruct)))
	  (when pf
	    `((setf (basic-structure-class-print-function (find-class ',name))
		    ,(if (symbolp pf)
			 `',pf
			 `#',pf)))))
      ,@(let ((mlff (dd-make-load-form-fun defstruct)))
	  (when mlff
	    `((setf (structure-class-make-load-form-fun (find-class ',name))
		    ,(if (symbolp mlff)
			 `',mlff
			 `#',mlff)))))
      ,@(let ((pure (dd-pure defstruct)))
	  (cond ((eq pure 't)
		 `((setf (layout-pure (%class-layout (find-class ',name)))
		    t)))
		((eq pure :substructure)
		 `((setf (layout-pure (%class-layout (find-class ',name)))
		    0)))))
      ,@(let ((def-con (dd-default-constructor defstruct)))
	  (when (and def-con (not (dd-alternate-metaclass defstruct)))
	    `((setf (structure-class-constructor (find-class ',name))
		    #',def-con)))))))

(defun accessor-inherited-data (name defstruct)
  (assoc name (dd-inherited-accessor-alist defstruct) :test #'eq))


;;; DEFSTRUCT  --  Public
;;;
(defmacro defstruct (name-and-options &rest slot-descriptions)
  "DEFSTRUCT {Name | (Name Option*)} {Slot | (Slot [Default] {Key Value}*)}
   Define the structure type Name.  Instances are created by MAKE-<name>, which
   takes keyword arguments allowing initial slot values to the specified.
   A SETF'able function <name>-<slot> is defined for each slot to read&write
   slot values.  <name>-p is a type predicate.

   Popular DEFSTRUCT options (see manual for others):

   (:CONSTRUCTOR Name)
   (:PREDICATE Name)
       Specify an alternate name for the constructor or predicate.

   (:CONSTRUCTOR Name Lambda-List)
       Explicitly specify the name and arguments to create a BOA constructor
       (which is more efficient when keyword syntax isn't necessary.)

   (:INCLUDE Supertype Slot-Spec*)
       Make this type a subtype of the structure type Supertype.  The optional
       Slot-Specs override inherited slot options.

   Slot options:

   :TYPE Type-Spec
       Asserts that the value of this slot is always of the specified type.

   :READ-ONLY {T | NIL}
       If true, no setter function is defined for this slot."

  (let* ((defstruct (parse-name-and-options
		     (if (atom name-and-options)
			 (list name-and-options)
			 name-and-options)))
	 (name (dd-name defstruct))
	 (pkg (symbol-package name)))
    (when (and lisp::*enable-package-locked-errors*
	       pkg
	       (ext:package-definition-lock pkg))
      (restart-case
	  (error 'lisp::package-locked-error
		 :package pkg
		 :format-control "defining structure ~A"
		 :format-arguments (list name))
	(continue ()
	  :report "Ignore the lock and continue")
	(unlock-package ()
	  :report "Disable package's definition lock then continue"
	  (setf (ext:package-definition-lock pkg) nil))
        (unlock-all ()
          :report "Unlock all packages, then continue"
          (lisp::unlock-all-packages))))
    (when (info declaration recognized name)
      (error "Defstruct already names a declaration: ~S." name))
    (when (stringp (car slot-descriptions))
      (setf (dd-doc defstruct) (pop slot-descriptions)))
    (dolist (slot slot-descriptions)
      (allocate-1-slot defstruct (parse-1-dsd defstruct slot)))
    (if (class-structure-p defstruct)
	(let ((inherits (inherits-for-structure defstruct)))
	  `(progn
	     (%defstruct ',defstruct ',inherits)
	     (%compiler-only-defstruct ',defstruct ',inherits)
	     ,@(when (eq (dd-type defstruct) 'structure)
		 `((%compiler-defstruct ',defstruct)))
	     ,@(define-raw-accessors defstruct)
	     ,@(define-constructors defstruct)
	     ,@(define-class-methods defstruct)
	   ',name))
	`(progn
	   (eval-when (compile load eval)
	     (setf (info typed-structure info ',name) ',defstruct))
	   ,@(define-constructors defstruct)
	   ,@(define-predicate defstruct)
	   ,@(define-accessors defstruct)
	   ,@(define-copier defstruct)
	   ',name))))
	   

;;;; Parsing:

;;; PARSE-1-OPTION  --  Internal
;;;
;;;    Parse a single defstruct option and store the results in Defstruct.
;;;
(defun parse-1-option (option defstruct)
  (let ((args (rest option))
	(name (dd-name defstruct)))
    (case (first option)
      (:conc-name
       (destructuring-bind (&optional conc-name)
	   args
	 (setf (dd-conc-name defstruct)
	       (if (symbolp conc-name)
		   conc-name
		   (make-symbol (string conc-name))))))
      (:constructor
       (destructuring-bind (&optional (cname (concat-pnames 'make- name))
				      &rest stuff)
			   args
	 (push (cons cname stuff) (dd-constructors defstruct))))
      (:copier
       (destructuring-bind (&optional (copier (concat-pnames 'copy- name)))
			   args
	 (setf (dd-copier defstruct) copier)))
      (:predicate
       (destructuring-bind (&optional (pred (concat-pnames name '-p)))
			   args
	 (setf (dd-predicate defstruct) pred)))
      (:include
       (when (dd-include defstruct)
	 (error "Can't have more than one :INCLUDE option."))
       (setf (dd-include defstruct) args))
      (:alternate-metaclass
       (setf (dd-alternate-metaclass defstruct) args))
      ((:print-function :print-object)
       (destructuring-bind (&optional (fun 'default-structure-print)) args
	 (setf (dd-print-function defstruct)
	       (if *ANSI-defstruct-options-p*
		   (cons (first option) fun)
		   fun))))
      (:type
       (destructuring-bind (type) args
	 (cond ((eq type 'funcallable-structure)
		(setf (dd-type defstruct) type))
	       ((member type '(list vector))
		(setf (dd-element-type defstruct) 't)
		(setf (dd-type defstruct) type))
	       ((and (consp type) (eq (first type) 'vector))
		(destructuring-bind (vector vtype) type
		  (declare (ignore vector))
		  (setf (dd-element-type defstruct) vtype)
		  (setf (dd-type defstruct) 'vector)))
	       (t
		(error "~S is a bad :TYPE for Defstruct." type)))))
      (:named
       (error "The Defstruct option :NAMED takes no arguments."))
      (:initial-offset
       (destructuring-bind (offset) args
	 (setf (dd-offset defstruct) offset)))
      (:make-load-form-fun
       (destructuring-bind (fun) args
	 (setf (dd-make-load-form-fun defstruct) fun)))
      (:pure
       (destructuring-bind (fun) args
	 (setf (dd-pure defstruct) fun)))
      (t (error "Unknown DEFSTRUCT option~%  ~S" option)))))

#+ORIGINAL
(defun parse-1-option (option defstruct)
  (let ((args (rest option))
	(name (dd-name defstruct)))
    (case (first option)
      (:conc-name
       (destructuring-bind (conc-name) args
	 (setf (dd-conc-name defstruct)
	       (if (symbolp conc-name)
		   conc-name
		   (make-symbol (string conc-name))))))
      (:constructor
       (destructuring-bind (&optional (cname (concat-pnames 'make- name))
				      &rest stuff)
			   args
	 (push (cons cname stuff) (dd-constructors defstruct))))
      (:copier
       (destructuring-bind (&optional (copier (concat-pnames 'copy- name)))
			   args
	 (setf (dd-copier defstruct) copier)))
      (:predicate
       (destructuring-bind (&optional (pred (concat-pnames name '-p)))
			   args
	 (setf (dd-predicate defstruct) pred)))
      (:include
       (when (dd-include defstruct)
	 (error "Can't have more than one :INCLUDE option."))
       (setf (dd-include defstruct) args))
      (:alternate-metaclass
       (setf (dd-alternate-metaclass defstruct) args))
      (:print-function
       (destructuring-bind (&optional (fun 'default-structure-print)) args
	 (setf (dd-print-function defstruct) fun)))
      (:type
       (destructuring-bind (type) args
	 (cond ((eq type 'funcallable-structure)
		(setf (dd-type defstruct) type))
	       ((member type '(list vector))
		(setf (dd-element-type defstruct) 't)
		(setf (dd-type defstruct) type))
	       ((and (consp type) (eq (first type) 'vector))
		(destructuring-bind (vector vtype) type
		  (declare (ignore vector))
		  (setf (dd-element-type defstruct) vtype)
		  (setf (dd-type defstruct) 'vector)))
	       (t
		(error "~S is a bad :TYPE for Defstruct." type)))))
      (:named
       (error "The Defstruct option :NAMED takes no arguments."))
      (:initial-offset
       (destructuring-bind (offset) args
	 (setf (dd-offset defstruct) offset)))
      (:make-load-form-fun
       (destructuring-bind (fun) args
	 (setf (dd-make-load-form-fun defstruct) fun)))
      (:pure
       (destructuring-bind (fun) args
	 (setf (dd-pure defstruct) fun)))
      (t (error "Unknown DEFSTRUCT option~%  ~S" option)))))


;;; PARSE-NAME-AND-OPTIONS  --  Internal
;;;
;;;    Given name and options, return a DD holding that info.
;;;
(defun parse-name-and-options (name-and-options)
  (destructuring-bind (name &rest options) name-and-options
    (let ((defstruct (make-defstruct-description name)))
      (dolist (option options)
	(cond ((consp option)
	       (parse-1-option option defstruct))
	      ((eq option :named)
	       (setf (dd-named defstruct) t))
	      ((member option '(:constructor :copier :predicate :named
				:conc-name))
	       (parse-1-option (list option) defstruct))
	      (t
	       (error "Unrecognized DEFSTRUCT option: ~S" option))))

      (case (dd-type defstruct)
	(structure
	 (when (dd-offset defstruct)
	   (error "Can't specify :OFFSET unless :TYPE is specified."))
	 (unless (dd-include defstruct)
	   (incf (dd-length defstruct))))
	(funcallable-structure)
	(t
	 (when (dd-print-function defstruct)
	   (warn "Silly to specify :PRINT-FUNCTION with :TYPE."))
	 (when (dd-make-load-form-fun defstruct)
	   (warn "Silly to specify :MAKE-LOAD-FORM-FUN with :TYPE."))
	 (when (dd-named defstruct) (incf (dd-length defstruct)))
	 (let ((offset (dd-offset defstruct)))
	   (when offset (incf (dd-length defstruct) offset)))))

      (when (dd-include defstruct)
	(do-inclusion-stuff defstruct))

      defstruct)))


;;;; Stuff to parse slot descriptions.

;;; PARSE-1-DSD  --  Internal
;;;
;;;    Parse a slot description for DEFSTRUCT, add it to the description and
;;; return it.  If supplied, ISLOT is a pre-initialized DSD that we modify to
;;; get the new slot.  This is supplied when handling included slots.
;;;
(defun parse-1-dsd (defstruct spec &optional
		     (islot (make-defstruct-slot-description
			     :name nil :index 0 :type t)))
  (multiple-value-bind (name default default-p type type-p read-only ro-p)
      (cond ((consp spec)
	     (destructuring-bind (name &optional (default nil default-p)
				       &key (type nil type-p)
				       (read-only nil ro-p))
		 spec
	       (values name default default-p type type-p read-only ro-p)))
	    (t
	     (when (keywordp spec)
	       (warn "Keyword slot name indicates probable syntax ~
		      error in DEFSTRUCT -- ~S."
		     spec))
	     spec))
    (when (find name (dd-slots defstruct) :test #'string= :key #'dsd-%name)
      (error 'simple-program-error
	     :format-control "Duplicate slot name ~S."
	     :format-arguments (list name)))
    (setf (dsd-name islot) name)
    (setf (dd-slots defstruct) (nconc (dd-slots defstruct) (list islot)))
    (setf (dsd-accessor islot) (concat-pnames (dd-conc-name defstruct) name))
    (when default-p
      (setf (dsd-default islot) default))
    (when type-p
      (setf (dsd-type islot)
	    (if (eq (dsd-type islot) 't)
		type
		`(and ,(dsd-type islot) ,type))))
    (when ro-p
      (if read-only
	  (setf (dsd-read-only islot) t)
	  (when (dsd-read-only islot)
	    (error "Slot ~S must be read-only in subtype ~S." name
		   (dsd-name islot)))))
    islot))


;;; ALLOCATE-1-SLOT  --  Internal
;;;
;;;    Allocate storage for a DSD in Defstruct.  This is where we decide if a
;;; slot is raw or not.  If raw, and we haven't allocated a raw-index yet for
;;; the raw data vector, then do it.  Raw objects are aligned on the unit of
;;; their size.
;;;
(defun allocate-1-slot (defstruct dsd)
  (let ((type (dsd-type dsd)))
    (multiple-value-bind
	(raw-type words)
	(cond ((not (eq (dd-type defstruct) 'structure))
	       (values nil nil))
	      ((and (subtypep type '(unsigned-byte 32))
		    (not (subtypep type 'fixnum)))
	       (values 'unsigned-byte 1))
	      ((subtypep type 'single-float)
	       (values 'single-float 1))
	      ((subtypep type 'double-float)
	       (values 'double-float 2))
	      #+long-float
	      ((subtypep type 'long-float)
	       (values 'long-float #+x86 3 #+sparc 4))
	      ((subtypep type '(complex single-float))
	       (values 'complex-single-float 2))
	      ((subtypep type '(complex double-float))
	       (values 'complex-double-float 4))
	      #+long-float
	      ((subtypep type '(complex long-float))
	       (values 'complex-long-float #+x86 6 #+sparc 8))
	      (t (values nil nil)))

      (cond ((not raw-type)
	     (setf (dsd-index dsd) (dd-length defstruct))
	     (incf (dd-length defstruct)))
	    (t
	     (unless (dd-raw-index defstruct)
	       (setf (dd-raw-index defstruct) (dd-length defstruct))
	       (incf (dd-length defstruct)))
	     (let ((off (rem (dd-raw-length defstruct) words)))
	       (unless (zerop off)
		 (incf (dd-raw-length defstruct) (- words off))))
	     (setf (dsd-raw-type dsd) raw-type)
	     (setf (dsd-index dsd) (dd-raw-length defstruct))
	     (incf (dd-raw-length defstruct) words)))))

  (undefined-value))


;;; DO-INCLUSION-STUFF  --  Internal
;;;
;;;    Process any included slots pretty much like they were specified.  Also
;;; inherit various other attributes (print function, etc.)
;;;
(defun do-inclusion-stuff (defstruct)
  (destructuring-bind (included-name &rest modified-slots)
		      (dd-include defstruct)
    (let* ((type (dd-type defstruct))
	   (included-structure
	    (if (class-structure-p defstruct)
		(layout-info (compiler-layout-or-lose included-name))
		(typed-structure-info-or-lose included-name))))
      (unless (and (eq type (dd-type included-structure))
		   (type= (specifier-type (dd-element-type included-structure))
			  (specifier-type (dd-element-type defstruct))))
	(error ":TYPE option mismatch between structures ~S and ~S."
	       (dd-name defstruct) included-name))
      
      (incf (dd-length defstruct) (dd-length included-structure))
      (when (class-structure-p defstruct)
	(unless (dd-print-function defstruct)
	  (setf (dd-print-function defstruct)
		(dd-print-function included-structure)))
	(unless (dd-make-load-form-fun defstruct)
	  (setf (dd-make-load-form-fun defstruct)
		(dd-make-load-form-fun included-structure)))
	(let ((mc (rest (dd-alternate-metaclass included-structure))))
	  (when (and mc (not (dd-alternate-metaclass defstruct)))
	    (setf (dd-alternate-metaclass defstruct)
		  (cons included-name mc))))
	(when (eq (dd-pure defstruct) :unspecified)
	  (setf (dd-pure defstruct) (dd-pure included-structure)))
	(setf (dd-raw-index defstruct) (dd-raw-index included-structure))
	(setf (dd-raw-length defstruct) (dd-raw-length included-structure)))

      (setf (dd-inherited-accessor-alist defstruct)
	    (dd-inherited-accessor-alist included-structure))
      
      (dolist (islot (dd-slots included-structure))
	(let* ((iname (dsd-name islot))
	       (modified (or (find iname modified-slots
				   :key #'(lambda (x) (if (atom x) x (car x)))
				   :test #'string=)
			     `(,iname))))
	  ;;
	  ;; We stash away an alist of accessors to parents' slots
	  ;; that have already been created to avoid conflicts later
	  ;; so that structures with :INCLUDE and :CONC-NAME (and
	  ;; other edge cases) can work as specified.
	  (when (dsd-accessor islot)
	    ;; the "oldest" (i.e. highest up the tree of inheritance)
	    ;; will prevail, so don't push new ones on if they
	    ;; conflict.
	    (pushnew (cons (dsd-accessor islot) (dsd-index islot))
		     (dd-inherited-accessor-alist defstruct)
		     :test #'eq :key #'car))
	  (parse-1-dsd defstruct modified
		       (copy-defstruct-slot-description islot)))))))



;;;; Constructors:

(defun typed-structure-info-or-lose (name)
  (or (info typed-structure info name)
      (error ":TYPE'd defstruct ~S not found for inclusion." name)))

;;; %GET-COMPILER-LAYOUT  --  Internal
;;;
;;; Delay looking for compiler-layout until the constructor is being compiled,
;;; since it doesn't exist until after the eval-when (compile) is compiled.
;;;
(defmacro %get-compiler-layout (name)
  `',(compiler-layout-or-lose name))

;;; FIND-NAME-INDICES  --  Internal
;;;
;;;      Returns a list of pairs (name . index).  Used for :TYPE'd constructors
;;; to find all the names that we have to splice in & where.  Note that these
;;; types don't have a layout, so we can't look at LAYOUT-INHERITS.
;;;
(defun find-name-indices (defstruct)
  (collect ((res))
    (let ((infos ()))
      (do ((info defstruct
		 (typed-structure-info-or-lose (first (dd-include info)))))
	  ((not (dd-include info))
	   (push info infos))
	(push info infos))
      
      (let ((i 0))
	(dolist (info infos)
	  (incf i (or (dd-offset info) 0))
	  (when (dd-named info)
	    (res (cons (dd-name info) i)))
	  (setq i (dd-length info)))))

    (res)))

  
;;; CREATE-{STRUCTURE,VECTOR,LIST}-CONSTRUCTOR  --  Internal
;;;
;;;    These functions are called to actually make a constructor after we have
;;; processed the arglist.  The correct variant (according to the DD-TYPE)
;;; should be called.  The function is defined with the specified name and
;;; arglist.  Vars and Types are used for argument type declarations.  Values
;;; are the values for the slots (in order.)
;;;
;;; This is split four ways because:
;;; 1] list & vector structures need "name" symbols stuck in at various weird
;;;    places, whereas STRUCTURE structures have a LAYOUT slot.
;;; 2] We really want to use LIST to make list structures, instead of
;;;    MAKE-LIST/(SETF ELT).
;;; 3] STRUCTURE structures can have raw slots that must also be allocated and
;;;    indirectly referenced.  We use SLOT-ACCESSOR-FORM to compute how to set
;;;    the slots, which deals with raw slots.
;;; 4] funcallable structures are weird.
;;;
(defun create-vector-constructor
       (defstruct cons-name arglist vars types values) 
  (let ((temp (gensym))
	(etype (dd-element-type defstruct)))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar #'(lambda (var type) `(type (and ,type ,etype) ,var))
			  vars types))
       (let ((,temp (make-array ,(dd-length defstruct)
				:element-type ',(dd-element-type defstruct))))
	 ,@(mapcar #'(lambda (x)
		       `(setf (aref ,temp ,(cdr x))  ',(car x)))
		   (find-name-indices defstruct))
	 ,@(mapcar #'(lambda (dsd value)
		       `(setf (aref ,temp ,(dsd-index dsd)) ,value))
		   (dd-slots defstruct) values)
	 ,temp))))
;;;
(defun create-list-constructor
       (defstruct cons-name arglist vars types values) 
  (let ((vals (make-list (dd-length defstruct) :initial-element nil)))
    (dolist (x (find-name-indices defstruct))
      (setf (elt vals (cdr x)) `',(car x)))
    (loop for dsd in (dd-slots defstruct) and val in values do
      (setf (elt vals (dsd-index dsd)) val))

    `(defun ,cons-name ,arglist
       (declare ,@(mapcar #'(lambda (var type) `(type ,type ,var))
			  vars types))
       (list ,@vals))))
;;;
(defun create-structure-constructor
       (defstruct cons-name arglist vars types values)
  (let* ((temp (gensym))
	 (raw-index (dd-raw-index defstruct))
	 (n-raw-data (when raw-index (gensym))))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar #'(lambda (var type) `(type ,type ,var))
			  vars types))
       (let ((,temp (truly-the ,(dd-name defstruct)
			       (%make-instance ,(dd-length defstruct))))
	     ,@(when n-raw-data
		 `((,n-raw-data
		    (make-array ,(dd-raw-length defstruct)
				:element-type '(unsigned-byte 32))))))
	 (setf (%instance-layout ,temp)
	       (%get-compiler-layout ,(dd-name defstruct)))
	 ,@(when n-raw-data
	     `((setf (%instance-ref ,temp ,raw-index) ,n-raw-data)))
	 ,@(mapcar #'(lambda (dsd value)
		       (multiple-value-bind
			   (accessor index data)
			   (slot-accessor-form defstruct dsd temp n-raw-data)
			 `(setf (,accessor ,data ,index) ,value)))
		   (dd-slots defstruct)
		   values)
	 ,temp))))
;;;
(defun create-fin-constructor
       (defstruct cons-name arglist vars types values) 
  (let ((temp (gensym)))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar #'(lambda (var type) `(type ,type ,var))
			  vars types))
       (let ((,temp (truly-the
		     ,(dd-name defstruct)
		     (%make-funcallable-instance
		      ,(dd-length defstruct)
		      (%get-compiler-layout ,(dd-name defstruct))))))
	 ,@(mapcar #'(lambda (dsd value)
		       `(setf (%funcallable-instance-info
			       ,temp ,(dsd-index dsd))
			      ,value))
		   (dd-slots defstruct) values)
	 ,temp))))


;;; CREATE-KEYWORD-CONSTRUCTOR   --  Internal
;;;
;;;    Create a default (non-BOA) keyword constructor.
;;;
(defun create-keyword-constructor (defstruct creator)
  (collect ((arglist (list '&key))
	    (types)
	    (vals))
    (dolist (slot (dd-slots defstruct))
      (let ((dum (gensym))
	    (name (dsd-name slot)))
	(arglist `((,(intern (string name) "KEYWORD") ,dum)
		   ,(dsd-default slot)))
	(types (dsd-type slot))
	(vals dum)))
    (funcall creator
	     defstruct (dd-default-constructor defstruct)
	     (arglist) (vals) (types) (vals))))


;;; CREATE-BOA-CONSTRUCTOR  --  Internal
;;;
;;;    Given a structure and a BOA constructor spec, call Creator with the
;;; appropriate args to make a constructor.
;;;
(defun create-boa-constructor (defstruct boa creator)
  (multiple-value-bind (req opt restp rest keyp keys allowp aux)
		       (kernel:parse-lambda-list (second boa))
    (collect ((arglist)
	      (vars)
	      (types))
      (labels ((get-slot (name)
		 (let ((res (find name (dd-slots defstruct) :test #'string=
				  :key #'dsd-name)))
		   (if res
		       (values (dsd-type res) (dsd-default res))
		       (values t nil))))
	       (do-default (arg)
		 (multiple-value-bind (type default) (get-slot arg)
		   (arglist `(,arg ,default))
		   (vars arg)
		   (types type))))
	(dolist (arg req)
	  (arglist arg)
	  (vars arg)
	  (types (get-slot arg)))
	
	(when opt
	  (arglist '&optional)
	  (dolist (arg opt)
	    (if (consp arg)
		(destructuring-bind (name &optional
					  (def (nth-value 1 (get-slot name)))
					  (supplied-test nil supplied-test-p))
		    arg
		  (arglist
		   `(,name ,def ,@(if supplied-test-p `(,supplied-test) nil)))
		  (vars name)
		  (types (get-slot name)))
		(do-default arg))))

	(when restp
	  (arglist '&rest rest)
	  (vars rest)
	  (types 'list))

	(when keyp
	  (arglist '&key)
	  (dolist (arg keys)
	    (if (consp arg)
		(destructuring-bind
		      (name-spec &optional
				 (def nil def-p)
				 (supplied-test nil supplied-test-p))
		    arg
		  (let ((name (if (consp name-spec)
				  (destructuring-bind (key var) name-spec
				    (declare (ignore key))
				    var)
				  name-spec)))
		    (multiple-value-bind (type slot-def) (get-slot name)
		      (arglist
		       `(,name-spec
			 ,(if def-p def slot-def)
			 ,@(if supplied-test-p `(,supplied-test) nil)))
		      (vars name)
		      (types type))))
		(do-default arg))))

	(when allowp (arglist '&allow-other-keys))

	(when aux
	  (arglist '&aux)
	  (dolist (arg aux)
	    (let* ((arg (if (consp arg) arg (list arg)))
		   (var (first arg)))
	      (arglist arg)
	      (vars var)
	      (types (get-slot var))))))

      (funcall creator defstruct (first boa)
	       (arglist) (vars) (types)
	       (mapcar #'(lambda (slot)
			   (or (find (dsd-name slot) (vars) :test #'string=)
			       (dsd-default slot)))
		       (dd-slots defstruct))))))


;;; DEFINE-CONSTRUCTORS  --  Internal
;;;
;;;    Grovel the constructor options, and decide what constructors (if any) to
;;; create.
;;;
(defun define-constructors (defstruct)
  (let ((no-constructors nil)
	(boas ())
	(defaults ())
	(creator (ecase (dd-type defstruct)
		   (structure #'create-structure-constructor)
		   (funcallable-structure #'create-fin-constructor)
		   (vector #'create-vector-constructor)
		   (list #'create-list-constructor))))
    (dolist (constructor (dd-constructors defstruct))
      (destructuring-bind (name &optional (boa-ll nil boa-p))
			  constructor
	(declare (ignore boa-ll))
	(cond ((not name) (setq no-constructors t))
	      (boa-p (push constructor boas))
	      (t (push name defaults)))))

    (when no-constructors
      (when (or defaults boas)
	(error "(:CONSTRUCTOR NIL) combined with other :CONSTRUCTORs."))
      (return-from define-constructors ()))

    (unless (or defaults boas)
      (push (concat-pnames 'make- (dd-name defstruct)) defaults))

    (collect ((res))
      (when defaults
	(let ((cname (first defaults)))
	  (setf (dd-default-constructor defstruct) cname)
	  (res (create-keyword-constructor defstruct creator))
	  (dolist (other-name (rest defaults))
	    (res `(setf (fdefinition ',other-name) (fdefinition ',cname)))
	    (res `(declaim (ftype function ',other-name))))))
      
      (dolist (boa boas)
	(res (create-boa-constructor defstruct boa creator)))

      (res))))

;;;; Slot accessors for raw slots:

;;; SLOT-ACCESSOR-FORM  --  Internal
;;;
;;;     Return info about how to read/write a slot in the value stored in
;;; Object.  This is also used by constructors (we can't use the accessor
;;; function, since some slots are read-only.)  If supplied, Data is a variable
;;; holding the raw-data vector.
;;; 
;;; Values:
;;; 1] Accessor function name (setfable)
;;; 2] Index to pass to accessor.
;;; 3] Object form to pass to accessor.
;;;
(defun slot-accessor-form (defstruct slot &optional (object 'object) data)
  (let ((rtype (dsd-raw-type slot)))
    (values
     (ecase rtype
       (single-float '%raw-ref-single)
       (double-float '%raw-ref-double)
       #+long-float
       (long-float '%raw-ref-long)
       (complex-single-float '%raw-ref-complex-single)
       (complex-double-float '%raw-ref-complex-double)
       #+long-float
       (complex-long-float '%raw-ref-complex-long)
       (unsigned-byte 'aref)
       ((t)
	(if (eq (dd-type defstruct) 'funcallable-structure)
	    '%funcallable-instance-info
	    '%instance-ref)))
     (case rtype
       #+long-float
       (complex-long-float
	(truncate (dsd-index slot) #+x86 6 #+sparc 8))
       #+long-float
       (long-float
	(truncate (dsd-index slot) #+x86 3 #+sparc 4))
       (double-float
	(ash (dsd-index slot) -1))
       (complex-double-float
	(ash (dsd-index slot) -2))
       (complex-single-float
	(ash (dsd-index slot) -1))
       (t
	(dsd-index slot)))
     (cond
      ((eq rtype 't) object)
      (data)
      (t
       `(truly-the (simple-array (unsigned-byte 32) (*))
		   (%instance-ref ,object ,(dd-raw-index defstruct))))))))


;;; dsd-inherited-p  --  Internal
;;;
;;; True when the defstruct slot has been inherited from an included
;;; structure.
;;;
(defun dsd-inherited-p (defstruct slot)
  (assoc (dsd-accessor slot) (dd-inherited-accessor-alist defstruct) :test #'eq))

;;; DEFINE-RAW-ACCESSORS  --  Internal
;;;
;;;    Define readers and writers for raw slots as inline functions.  We use
;;; the special RAW-REF operations to store floats in the raw data vector.  We
;;; also define FIN accessors here.
;;;
(defun define-raw-accessors (defstruct)
  (let ((name (dd-name defstruct)))
    (collect ((res))
      (dolist (slot (dd-slots defstruct))
	(let ((stype (dsd-type slot))
	      (aname (dsd-accessor slot)))
	  (multiple-value-bind (accessor offset data)
	      (slot-accessor-form defstruct slot)
	    (unless (or (dsd-inherited-p defstruct slot)
			(eq accessor '%instance-ref))
	      (res `(declaim (inline ,aname)))
	      (res `(declaim (ftype (function (,name) ,stype) ,aname)))
	      (res `(defun ,aname (object)
		      (truly-the ,stype (,accessor ,data ,offset))))
	      (unless (dsd-read-only slot)
		(res `(declaim (inline (setf ,aname))))
		(res `(declaim (ftype (function (,stype ,name) ,stype)
				      (setf ,aname))))
		(res
		 `(defun (setf ,aname) (new-value object)
		    (setf (,accessor ,data ,offset) new-value)
		    new-value)))))))

      (when (eq (dd-type defstruct) 'funcallable-structure)
	(let ((pred (dd-predicate defstruct)))
	  (when pred
	    (res `(declaim (inline ,pred)))
	    (res `(defun ,pred (x) (typep x ',name))))))

      (res))))


;;;; Typed (non-class) structures:

;;; DD-LISP-TYPE  --  Internal
;;;
;;;    Return a type specifier we can use for testing :TYPE'd structures.
;;;
(defun dd-lisp-type (defstruct)
  (ecase (dd-type defstruct)
    (list 'list)
    (vector `(simple-array ,(dd-element-type defstruct)
			   (*)))))

;;; DEFINE-ACCESSORS  --  Internal
;;;
;;;    Returns a list of function definitions for accessing and setting the
;;; slots of the a typed Defstruct.  The functions are proclaimed to be inline,
;;; and the types of their arguments and results are declared as well.  We
;;; count on the compiler to do clever things with Elt.
;;;
(defun define-accessors (defstruct)
  (collect ((stuff))
    (let ((ltype (dd-lisp-type defstruct)))
      (dolist (slot (dd-slots defstruct))
	(let* ((aname (dsd-accessor slot))
	       (index (dsd-index slot))
	       (slot-type `(and ,(dsd-type slot)
			    ,(dd-element-type defstruct)))
	       (inherited (accessor-inherited-data aname defstruct)))
	  (cond ((not inherited)
		 (stuff `(declaim (inline ,aname (setf ,aname))))
		 (stuff `(defun ,aname (structure)
			  (declare (type ,ltype structure))
			  (the ,slot-type (elt structure ,index))))
		 (unless (dsd-read-only slot)
		   (stuff
		    `(defun (setf ,aname) (new-value structure)
		      (declare (type ,ltype structure) (type ,slot-type new-value))
		      (setf (elt structure ,index) new-value)))))
		((not (= (cdr inherited) index))
		 (warn 'simple-style-warning
		       :format-control
		       "~@<Non-overwritten accessor ~S does not access ~
                        slot with name ~S (accessing an inherited slot ~
                        instead).~:@>"
		       :format-arguments (list aname (dsd-%name slot))))))
	))
    (stuff)))


;;; Define-Copier returns the definition for a copier function of a typed
;;; Defstruct if one is desired.
(defun define-copier (defstruct)
  (when (dd-copier defstruct)
    `((setf (fdefinition ',(dd-copier defstruct)) #'copy-seq)
      (declaim (ftype function ,(dd-copier defstruct))))))


;;; Define-Predicate returns a definition for a predicate function if one is
;;; desired.  Rather vaguely specified w.r.t. inclusion.
;;;
(defun define-predicate (defstruct)
  (let ((name (dd-name defstruct))
	(pred (dd-predicate defstruct)))
    (when (and pred (dd-named defstruct))
      (let ((ltype (dd-lisp-type defstruct))
	    (index (cdr (car (last (find-name-indices defstruct))))))
	(if (eq ltype 'list)
	    `((defun ,pred (object)
		(and (typep object 'list)
		     (defstruct-list-p ,index object ',name))))
	    `((defun ,pred (object)
		(and (typep object 'vector)
		     (array-in-bounds-p object ,index)
		     (eq (aref object ,index) ',name)))))))))

;; A predicate to determine if the given list is a defstruct object of
;; :type list.  This used to be done using (eq (nth index object)
;; name), but that fails if the (nth index object) doesn't work
;; because object is not a proper list.
(defun defstruct-list-p (index list name)
  ;; Basically do (nth index list), but don't crash if the list is not
  ;; a proper list.
  (declare (type index index)
	   (type list list))
  (do ((i index (1- i))
       (result list (cdr result)))
      ((or (atom result) (not (plusp i)))
       (unless (atom result)
	 (eq (car result) name)))
    (declare (type index i))))



;;;; Load time support for default structures (%DEFSTRUCT)
;;;
;;;    In the normal case of structures that have a real type (i.e. no :Type
;;; option was specified), we want to optimize things for space as well as
;;; speed, since there can be thousands of defined slot accessors.
;;;
;;;    What we do is defined the accessors and copier as closures over
;;; general-case code.  Since the compiler will normally open-code accessors,
;;; the (minor) efficiency penalty is not a concern.

;;; Typep-To-Layout  --  Internal
;;;
;;;    Return true if Obj is an object of the structure type corresponding to
;;; Layout.  This is called by the accessor closures, which have a handle on
;;; the type's layout.
;;;
(declaim (inline typep-to-layout))
(defun typep-to-layout (obj layout &optional no-error)
  (declare (type layout layout) (optimize (speed 3) (safety 0)))
  (when (layout-invalid layout)
    (error "Obsolete structure accessor function called."))
  (and (%instancep obj)
       (let ((depth (layout-inheritance-depth layout))
	     (obj-layout (%instance-layout obj)))
	 (cond ((eq obj-layout layout) t)
	       ((layout-invalid obj-layout)
		(if no-error
		    nil
		    (error 'layout-invalid
			   :expected-type (layout-class obj-layout)
			   :datum obj)))
	       (t
		(and (> (layout-inheritance-depth obj-layout) depth)
		     (eq (svref (layout-inherits obj-layout) depth)
			 layout)))))))


;;; STRUCTURE-SLOT-SETTER, STRUCTURE-SLOT-ACCESSOR  --  Internal
;;;
;;;    Return closures to do slot access (set), according to Layout and DSD.
;;; We check types, then do the access.  This is only used for normal slots
;;; (not raw.)
;;;
(defun structure-slot-accessor (layout dsd)
  (let ((class (layout-class layout)))
    (if (typep class 'basic-structure-class)
	#'(lambda (structure)
	    (declare (optimize (speed 3) (safety 0)))
	    (unless (typep-to-layout structure layout)
	      (error 'simple-type-error
		     :datum structure
		     :expected-type class
		     :format-control "Structure for accessor ~S is not a ~S:~% ~S"
		     :format-arguments (list (dsd-accessor dsd)
					     (%class-name class)
					     structure)))
	    (%instance-ref structure (dsd-index dsd)))
	#'(lambda (structure)
	    (declare (optimize (speed 3) (safety 0)))
	    (unless (%typep structure class)
	      (error 'simple-type-error
		     :datum structure
		     :expected-type class
		     :format-control "Structure for accessor ~S is not a ~S:~% ~S"
		     :format-arguments (list (dsd-accessor dsd) class
					     structure)))
	    (%instance-ref structure (dsd-index dsd))))))
;;;
(defun structure-slot-setter (layout dsd)
  (let ((class (layout-class layout)))
    (if (typep class 'basic-structure-class)
	#'(lambda (new-value structure)
	    (declare (optimize (speed 3) (safety 0)))
	    (unless (typep-to-layout structure layout)
	      (error 'simple-type-error
		     :datum structure
		     :expected-type class
		     :format-control "Structure for setter ~S is not a ~S:~% ~S"
		     :format-arguments (list `(setf ,(dsd-accessor dsd))
					     (%class-name class)
					     structure)))
	    (unless (%typep new-value (dsd-type dsd))
	      (error 'simple-type-error
		     :datum new-value
		     :expected-type (dsd-type dsd)
		     :format-control "New-Value for setter ~S is not a ~S:~% ~S."
		     :format-arguments (list `(setf ,(dsd-accessor dsd))
					     (dsd-type dsd)
					     new-value)))
	    (setf (%instance-ref structure (dsd-index dsd)) new-value))
	#'(lambda (new-value structure)
	    (declare (optimize (speed 3) (safety 0)))
	    (unless (%typep structure class)
	      (error 'simple-type-error
		     :datum structure
		     :expected-type class
		     :format-control "Structure for setter ~S is not a ~S:~% ~S"
		     :format-arguments (list `(setf ,(dsd-accessor dsd))
					     (%class-name class)
					     structure)))
	    (unless (%typep new-value (dsd-type dsd))
	      (error 'simple-type-error
		     :datum new-value
		     :expected-type (dsd-type dsd)
		     :format-control "New-Value for setter ~S is not a ~S:~% ~S."
		     :format-arguments (list `(setf ,(dsd-accessor dsd))
					     (dsd-type dsd)
					     new-value)))
	    (setf (%instance-ref structure (dsd-index dsd)) new-value)))))


;;;
;;; Used for updating CLOS structure classes.  Hooks are called
;;; with one argument, the kernel::class.
;;;
(defvar *defstruct-hooks* nil)

;;; %Defstruct  --  Internal
;;;
;;;    Do miscellaneous (LOAD EVAL) time actions for the structure described by
;;; Info.  Create the class & layout, checking for incompatible redefinition.
;;; Define setters, accessors, copier, predicate, documentation, instantiate
;;; definition in load-time env.  This is only called for default structures.
;;;
(defun %defstruct (info inherits)
  (declare (type defstruct-description info))
  (multiple-value-bind (class layout old-layout)
      (ensure-structure-class info inherits "current" "new")
    (cond ((not old-layout)
	   (unless (eq (%class-layout class) layout)
	     (register-layout layout)))
	  (t
	   (let ((old-info (layout-info old-layout)))    
	     (when (defstruct-description-p old-info)
	       (dolist (slot (dd-slots old-info))
		 (unless (dsd-inherited-p old-info slot)
		   (let ((aname (dsd-accessor slot)))
		     (fmakunbound aname)
		     (unless (dsd-read-only slot)
		       (fmakunbound `(setf ,aname))))))))
	   (%redefine-defstruct class old-layout layout)
	   (setq layout (%class-layout class))))

    (setf (find-class (dd-name info)) class)
    
    (unless (eq (dd-type info) 'funcallable-structure)
      (dolist (slot (dd-slots info))
	(unless (or (dsd-inherited-p info slot)
		    (not (eq (dsd-raw-type slot) 't)))
	  (let* ((aname (dsd-accessor slot))
		 (inherited (accessor-inherited-data aname info)))
	    (unless inherited
	      (setf (symbol-function aname)
		    (structure-slot-accessor layout slot))
	      (unless (dsd-read-only slot)
		(setf (fdefinition `(setf ,aname))
		      (structure-slot-setter layout slot)))))
	  
	  ))

      (when (dd-predicate info)
	(setf (symbol-function (dd-predicate info))
	      #'(lambda (object)
		  (declare (optimize (speed 3) (safety 0)))
		  (typep-to-layout object layout t))))

      (when (dd-copier info)
	(setf (symbol-function (dd-copier info))
	      #'(lambda (structure)
		  (declare (optimize (speed 3) (safety 0)))
		  (unless (typep-to-layout structure layout)
		    (error 'simple-type-error
			   :datum structure
			   :expected-type class
			   :format-control "Structure for copier is not a ~S:~% ~S"
			   :format-arguments (list class structure)))
		  (copy-structure structure))))

      (when (boundp '*defstruct-hooks*)
	(dolist (fn *defstruct-hooks*)
	  (funcall fn class)))))

  (when (dd-doc info)
    (setf (documentation (dd-name info) 'type) (dd-doc info)))

  (undefined-value))


;;;; Redefinition stuff:

;;; ENSURE-STRUCTURE-CLASS  --  Internal
;;;
;;;    Called when we are about to define a structure class.  Returns a
;;; (possibly new) class object and the layout which should be used for the new
;;; definition (may be the current layout, and also might be an uninstalled
;;; forward referenced layout.)  The third value is true if this is an
;;; incompatible redefinition, in which case it is the old layout.
;;;
(defun ensure-structure-class (info inherits old-context new-context
				    &optional compiler-layout)
  (multiple-value-bind
      (class old-layout)
      (destructuring-bind (&optional name (class 'kernel::structure-class)
				     (constructor 'make-structure-class))
			  (dd-alternate-metaclass info)
	(declare (ignore name))
	(insured-find-class (dd-name info)
			    (if (eq class 'kernel::structure-class)
				#'(lambda (x) (typep x 'kernel::structure-class))
				#'(lambda (x) (typep x (find-class class))))
			    (fdefinition constructor)))
    (setf (%class-direct-superclasses class)
	  (if (eq (dd-name info) 'lisp-stream)
	      ;; Hack to add stream as a superclass mixin to lisp-streams.
	      (list (layout-class (svref inherits (1- (length inherits))))
		    (layout-class (svref inherits (- (length inherits) 2))))
	      (list (layout-class (svref inherits (1- (length inherits)))))))
    (let ((new-layout (make-layout :class class
				   :inherits inherits
				   :inheritance-depth (length inherits)
				   :length (dd-length info)
				   :info info))
	  (old-layout (or compiler-layout old-layout)))
      (cond
       ((not old-layout)
	(values class new-layout nil))
       ((not *type-system-initialized*)
	(setf (layout-info old-layout) info)
	(values class old-layout nil))
       ((redefine-layout-warning old-layout old-context
				 new-layout new-context)
	(values class new-layout old-layout))
       (t
	(let ((old-info (layout-info old-layout)))
	  (typecase old-info
	    ((or defstruct-description)
	     (cond ((redefine-structure-warning class old-info info)
		    (values class new-layout old-layout))
		   (t
		    (setf (layout-info old-layout) info)
		    (values class old-layout nil))))
	    (null
	     (setf (layout-info old-layout) info)
	     (values class old-layout nil))
	    (t
	     (warn "Shouldn't happen!  Some strange thing in LAYOUT-INFO:~
		    ~%  ~S"
		   old-layout)
	     (values class new-layout old-layout)))))))))
	    

;;; COMPARE-SLOTS  --  Internal
;;;
;;;    Compares the slots of Old and New, returning 3 lists of slot names:
;;; 1] Slots which have moved,
;;; 2] Slots whose type has changed,
;;; 3] Deleted slots. 
;;;
(defun compare-slots (old new)
  (let* ((oslots (dd-slots old))
	 (nslots (dd-slots new))
	 (onames (mapcar #'dsd-name oslots))
	 (nnames (mapcar #'dsd-name nslots)))
    (collect ((moved)
	      (retyped))
      (dolist (name (intersection onames nnames))
	(let ((os (find name oslots :key #'dsd-name))
	      (ns (find name nslots :key #'dsd-name)))
	  (unless (subtypep (dsd-type ns) (dsd-type os))
	    (retyped name))
	  (unless (and (= (dsd-index os) (dsd-index ns))
		       (eq (dsd-raw-type os) (dsd-raw-type ns)))
	    (moved name))))
      (values (moved)
	      (retyped)
	      (set-difference onames nnames)))))


;;; REDEFINE-STRUCTURE-WARNING  --  Internal
;;;
;;;    Give a warning and return true if we are redefining a structure with
;;; different slots than in the currently loaded version.
;;;
(defun redefine-structure-warning (class old new)
  (declare (type defstruct-description old new)
	   (type kernel::class class)
	   (ignore class))
  (let ((name (dd-name new)))
    (multiple-value-bind (moved retyped deleted)
			 (compare-slots old new)
      (when (or moved retyped deleted)
	(warn 
	 "Incompatibly redefining slots of structure class ~S~@
	  Make sure any uses of affected accessors are recompiled:~@
	  ~@[  These slots were moved to new positions:~%    ~S~%~]~
	  ~@[  These slots have new incompatible types:~%    ~S~%~]~
	  ~@[  These slots were deleted:~%    ~S~%~]"
	 name moved retyped deleted)
	t))))


;;; %REDEFINE-DEFSTRUCT  --  Internal
;;;
;;;    This function is called when we are incompatibly redefining a structure
;;; Class to have the specified New-Layout.  We signal an error with some
;;; proceed options and return the layout that should be used.
;;;
#+bootstrap-dynamic-extent
(defun %redefine-defstruct (class old-layout new-layout)
  (declare (type class class) (type layout old-layout new-layout))
  (register-layout new-layout :invalidate nil
		   :destruct-layout old-layout))

#-bootstrap-dynamic-extent
(defun %redefine-defstruct (class old-layout new-layout)
  (declare (type class class) (type layout old-layout new-layout))
  (let ((name (class-proper-name class)))
    (restart-case
	(error "Redefining class ~S incompatibly with the current ~
		definition."
	       name)
      (continue ()
	:report "Invalidate already loaded code and instances, use new definition."
	(warn "Previously loaded ~S accessors will no longer work." name)
	(register-layout new-layout))
      (clobber-it ()
	:report "Assume redefinition is compatible, allow old code and instances."
	(warn "Any old ~S instances will be in a bad way.~@
	       I hope you know what you're doing..."
	      name)
	(register-layout new-layout :invalidate nil
			 :destruct-layout old-layout))))
  (undefined-value))


;;; UNDEFINE-STRUCTURE  --  Interface
;;;
;;;    Blow away all the compiler info for the structure CLASS.
;;; Iterate over this type, clearing the compiler structure
;;; type info, and undefining all the associated functions.
;;; 
(defun undefine-structure (class)
  (let ((info (layout-info (%class-layout class))))
    (when (defstruct-description-p info)
      (let ((type (dd-name info)))
	(setf (info type compiler-layout type) nil)
	(undefine-function-name (dd-copier info))
	(undefine-function-name (dd-predicate info))
	(dolist (slot (dd-slots info))
	  (unless (dsd-inherited-p info slot)
	    (let ((aname (dsd-accessor slot)))
	      (unless (accessor-inherited-data aname info)
		(undefine-function-name aname)
		(unless (dsd-read-only slot)
		  (undefine-function-name `(setf ,aname))))
	      
	      ))))
      ;;
      ;; Clear out the SPECIFIER-TYPE cache so that subsequent references are
      ;; unknown types.
      (values-specifier-type-cache-clear)))
  (undefined-value))


;;;; Compiler stuff:

;;; DEFINE-DEFSTRUCT-NAME  --  Internal
;;;
;;;    Like DEFINE-FUNCTION-NAME, but we also set the kind to :DECLARED and
;;; blow away any ASSUMED-TYPE.  Also, if the thing is a slot accessor
;;; currently, quietly unaccessorize it.  And if there are any undefined
;;; warnings, we nuke them.
;;;
(defun define-defstruct-name (name)
  (when name
    (when (info function accessor-for name)
      (setf (info function accessor-for name) nil))
    (define-function-name name)
    (note-name-defined name :function)
    (setf (info function where-from name) :declared)
    (when (info function assumed-type name)
      (setf (info function assumed-type name) nil)))
  (undefined-value))


;;; INHERITS-FOR-STRUCTURE  --  Internal
;;;
;;;    This function is called at macroexpand time to compute the INHERITS
;;; vector for a structure type definition.
;;;
(defun inherits-for-structure (info)
  (declare (type defstruct-description info))
  (let* ((include (dd-include info))
	 (superclass-opt (dd-alternate-metaclass info))
	 (super
	  (if include
	      (compiler-layout-or-lose (first include))
	      (%class-layout (find-class (or (first superclass-opt)
					    'structure-object))))))
    (if (eq (dd-name info) 'lisp-stream)
	;; Hack to added the stream class as a mixin for lisp-streams.
	(concatenate 'simple-vector (layout-inherits super)
		     (vector super (%class-layout (find-class 'stream))))
	(concatenate 'simple-vector (layout-inherits super) (vector super)))))

;;; %COMPILER-ONLY-DEFSTRUCT  --  Internal
;;;
;;;    This function is called at compile-time to do the compile-time-only
;;; actions for defining a structure type.  It installs the class in the type
;;; system in a similar way to %DEFSTRUCT, but is quieter and safer in the case
;;; of redefinition.  Eval-when doesn't do the right thing when nested or
;;; non-top-level, so this is magically called by the compiler.
;;;
;;;    Basically, this function avoids trashing the compiler by only actually
;;; defining the class if there is no current definition.  Instead, we just set
;;; the INFO TYPE COMPILER-LAYOUT.
;;;
(defun %compiler-only-defstruct (info inherits)
  (multiple-value-bind
      (class layout old-layout)
      (multiple-value-bind
	  (clayout clayout-p)
	  (info type compiler-layout (dd-name info))
	(ensure-structure-class info inherits
				(if clayout-p "previously compiled" "current")
				"compiled"
				clayout))
    (cond
     (old-layout
      (undefine-structure (layout-class old-layout))
      (when (and (%class-subclasses class)
		 (not (eq layout old-layout)))
	(collect ((subs))
	  (do-hash (class layout (%class-subclasses class))
	    (declare (ignore layout))
	    (undefine-structure class)
	    (subs (class-proper-name class)))
	  (when (subs)
	    (warn "Removing old subclasses of ~S:~%  ~S"
		  (%class-name class) (subs))))))
     (t
      (unless (eq (%class-layout class) layout)
	(register-layout layout :invalidate nil))
      (setf (find-class (dd-name info)) class)))
    
    (setf (info type compiler-layout (dd-name info)) layout))
  
  (undefined-value))


;;; %%Compiler-Defstruct  --  External
;;;
;;;    This function does the (compile load eval) time actions for updating the
;;; compiler's global meta-information to represent the definition of the
;;; structure described by Info.  This primarily amounts to setting up info
;;; about the accessor and other implicitly defined functions.  The
;;; constructors are explicitly defined by top-level code.
;;;
(defun %%compiler-defstruct (info)
  (declare (type defstruct-description info))
  (let* ((name (dd-name info))
	 (class (find-class name)))
    (let ((copier (dd-copier info)))
      (when copier
	(proclaim `(ftype (function (,name) ,name) ,copier))))
    
    (let ((pred (dd-predicate info)))
      (when pred
	(define-defstruct-name pred)
	(setf (info function inlinep pred) :inline)
	(setf (info function inline-expansion pred)
	      `(lambda (x) (typep x ',name)))))

    (dolist (slot (dd-slots info))
      (let* ((aname (dsd-accessor slot))
	     (setf-fun `(setf ,aname))
 	     (inherited (and aname (accessor-inherited-data aname info))))

	(cond (inherited
	       (unless (= (cdr inherited) (dsd-index slot))
		 (warn 'simple-style-warning
		       :format-control
		       "~@<Non-overwritten accessor ~S does not access ~
                        slot with name ~S (accessing an inherited slot ~
                        instead).~:@>"
		       :format-arguments (list aname (dsd-%name slot)))))
	      (t
	       (unless (or (dsd-inherited-p info slot)
			   (not (eq (dsd-raw-type slot) 't)))
		 (define-defstruct-name aname)
		 (setf (info function accessor-for aname) class)
		 (unless (dsd-read-only slot)
		   (define-defstruct-name setf-fun)
		   (setf (info function accessor-for setf-fun) class)))))
	
	)))
  
  (undefined-value))

(setf (symbol-function '%compiler-defstruct) #'%%compiler-defstruct)


;;; COPY-STRUCTURE  --  Public
;;;
;;;    Copy any old kind of structure.
;;;
(defun copy-structure (structure)
  "Return a copy of Structure with the same (EQL) slot values."
  (declare (type structure-object structure) (optimize (speed 3) (safety 0)))
  (let* ((len (%instance-length structure))
	 (res (%make-instance len))
	 (layout (%instance-layout structure)))
    (declare (type index len))
    (when (layout-invalid layout)
      (error "Copying an obsolete structure:~%  ~S" structure))
    
    (dotimes (i len)
      (declare (type index i))
      (setf (%instance-ref res i)
	    (%instance-ref structure i)))
    
    (let ((raw-index (dd-raw-index (layout-info layout))))
      (when raw-index
	(let* ((data (%instance-ref structure raw-index))
	       (raw-len (length data))
	       (new (make-array raw-len :element-type '(unsigned-byte 32))))
	  (declare (type (simple-array (unsigned-byte 32) (*)) data))
	  (setf (%instance-ref res raw-index) new)
	  (dotimes (i raw-len)
	    (setf (aref new i) (aref data i))))))
    
    res))


;;; Default print and make-load-form methods.

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (if (funcallable-instance-p structure)
      (print-unreadable-object (structure stream :identity t :type t)
	(write-string "Funcallable Structure" stream))
      (let* ((type (%instance-layout structure))
	     (name (%class-name (layout-class type)))
	     (dd (layout-info type)))
	(cond
	  ((and (null (dd-slots dd)) *print-level* (>= *current-level* *print-level*))
	   ;; The CLHS entry for *PRINT-LENGTH* says "If an object to
	   ;; be recursively printed has components and is at a level
	   ;; equal to or greater than the value of *print-level*,
	   ;; then the object is printed as ``#''."
	   ;;
	   ;; So, if it has no components, and we're at *PRINT-LEVEL*,
	   ;; we print out #(S<name>).
	   (write-string "#S(" stream)
	   (prin1 name stream)
	   (write-char #\) stream))
	  (*print-pretty*
	   (pprint-logical-block (stream nil :prefix "#S(" :suffix ")")
	     (prin1 name stream)
	     (let ((slots (dd-slots dd)))
	       (when slots
		 (write-char #\space stream)
		 (pprint-indent :block 2 stream)
		 (pprint-newline :linear stream)
		 (loop
		    (pprint-pop)
		    (let ((slot (pop slots)))
		      (write-char #\: stream)
		      (output-symbol-name (dsd-%name slot) stream)
		      (write-char #\space stream)
		      (pprint-newline :miser stream)
		      (output-object (funcall (fdefinition (dsd-accessor slot))
					      structure)
				     stream)
		      (when (null slots)
			(return))
		      (write-char #\space stream)
		      (pprint-newline :linear stream)))))))
	  (t
	   (descend-into (stream)
	     (write-string "#S(" stream)
	     (prin1 name stream)
	     (do ((index 0 (1+ index))
		  (slots (dd-slots dd) (cdr slots)))
		 ((or (null slots)
		      (and (not *print-readably*) (eql *print-length* index)))
		  (if (null slots)
		      (write-string ")" stream)
		      (write-string " ...)" stream)))
	       (declare (type index index))
	       (write-char #\space stream)
	       (write-char #\: stream)
	       (let ((slot (first slots)))
		 (output-symbol-name (dsd-%name slot) stream)
		 (write-char #\space stream)
		 (output-object (funcall (fdefinition (dsd-accessor slot))
					 structure)
				stream)))))))))

(defun make-structure-load-form (structure)
  (declare (type structure-object structure))
  (let* ((class (layout-class (%instance-layout structure)))
	 (fun (structure-class-make-load-form-fun class)))
    (etypecase fun
      ((member :just-dump-it-normally :ignore-it)
       fun)
      (null
       (error "Structures of type ~S cannot be dumped as constants."
	      (%class-name class)))
      (function
       (funcall fun structure))
      (symbol
       (funcall (symbol-function fun) structure)))))
