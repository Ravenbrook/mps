;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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
  "$Header: /project/cmucl/cvsroot/src/pcl/methods.lisp,v 1.44 2005/07/07 16:44:27 rtoy Exp $")

(in-package :pcl)

;;;; *********************
;;;; PRINT-OBJECT  *******
;;;; *********************

(defun named-object-print-function (instance stream
				    &optional (extra nil extra-p))
  (print-unreadable-object (instance stream :identity t)
    (if extra-p					
	(format stream "~A ~S ~:S"
		(class-name (class-of instance))
		(slot-value-or-default instance 'name)
		extra)
	(format stream "~A ~S"
		(class-name (class-of instance))
		(slot-value-or-default instance 'name)))))

(defmethod print-object (instance stream)
  (print-unreadable-object (instance stream :identity t)
    (let ((name (class-name (class-of instance))))
      (if name
	  (format stream "~S" name)
	  (format stream "Instance")))))

(defmethod print-object ((class class) stream)
  (named-object-print-function class stream))

(defmethod print-object ((slotd slot-definition) stream)
  (named-object-print-function slotd stream))

(defmethod print-object ((mc standard-method-combination) stream)
  (print-unreadable-object (mc stream :identity t)
    (format stream
	    "Method-Combination ~S ~S"
	    (slot-value-or-default mc 'type)
	    (slot-value-or-default mc 'options))))

(defmethod print-object ((method standard-method) stream)
  (print-unreadable-object (method stream :identity t)
    (if (slot-boundp method 'generic-function)
	(let ((gf (method-generic-function method))
	      (class-name (class-name (class-of method))))
	  (format stream "~A ~S ~{~S ~}~:S"
		  class-name
		  (and gf (generic-function-name gf))
		  (method-qualifiers method)
		  (unparse-specializers method)))
	(call-next-method))))

(defmethod print-object ((method standard-accessor-method) stream)
  (print-unreadable-object (method stream :identity t)
    (if (slot-boundp method 'generic-function)
	(let ((gf (method-generic-function method))
	      (class-name (class-name (class-of method))))
	  (format stream "~A ~S, slot:~S, ~:S"
		  class-name
		  (and gf (generic-function-name gf))
		  (accessor-method-slot-name method)
		  (unparse-specializers method)))
	(call-next-method))))

(defmethod print-object ((gf standard-generic-function) stream)
  (named-object-print-function
    gf
    stream
    (if (slot-boundp gf 'methods)
	(list (length (generic-function-methods gf)))
	"?")))


;;;
;;;
;;;
(defmethod shared-initialize :after ((slotd structure-slot-definition)
				     slot-names 
				     &key (allocation :instance)
				     allocation-class)
  (declare (ignore slot-names allocation-class))
  (unless (eq allocation :instance)
    (error "~@<Structure slots must have ~s allocation.~@:>" :instance)))


;;;
;;; METHODS
;;;
;;; Methods themselves are simple inanimate objects.  Most properties of
;;; methods are immutable, methods cannot be reinitialized.  The following
;;; properties of methods can be changed:
;;;   METHOD-GENERIC-FUNCTION
;;;   METHOD-FUNCTION            ??
;;;   
;;;

(defmethod method-function ((method standard-method))
  (or (slot-value method 'function)
      (let ((fmf (slot-value method 'fast-function)))
	;; The :before shared-initialize method prevents this
	(unless fmf
	  (internal-error "~@<~S doesn't seem to have a method function.~@:>"
			  method))
	(setf (slot-value method 'function)
	      (method-function-from-fast-function fmf)))))

(defmethod method-qualifiers ((method standard-method))
  (plist-value method 'qualifiers))

(defmethod accessor-method-class ((method standard-accessor-method))
  (car (slot-value method 'specializers)))

(defmethod accessor-method-class ((method standard-writer-method))
  (cadr (slot-value method 'specializers)))


;;;
;;; INITIALIZATION
;;;
;;; Error checking is done in before methods.  Because of the simplicity of
;;; standard method objects the standard primary method can fill the slots.
;;;
;;; Methods are not reinitializable.
;;; 

(defvar *allow-experimental-specializers-p* t)

(defvar *the-class-generic-function*
  (find-class 'generic-function))

(defvar *the-class-standard-generic-function*
  (find-class 'standard-generic-function))


(defmethod reinitialize-instance ((method standard-method) &rest initargs &key)
  (declare (ignore initargs))
  (error "~@<Attempt to reinitialize the method ~S.  ~
          Method objects cannot be reinitialized.~@:>"
	 method))

(defmethod shared-initialize :before ((method standard-method)
				      slot-names
				      &key qualifiers
					   lambda-list
					   specializers
					   function
				           fast-function
					   documentation)
  (declare (ignore slot-names))
  (flet ((lose (initarg value string)
	   (error "~@<When initializing the method ~S, ~
                   the ~S initialization argument was ~S, ~
                   which ~A.~@:>"
		  method initarg value string)))
    (let ((check-qualifiers    (legal-qualifiers-p method qualifiers))
	  (check-lambda-list   (legal-lambda-list-p method lambda-list))
	  (check-specializers  (legal-specializers-p method specializers))
	  (check-function      (legal-method-function-p
				method
				(or function fast-function)))
	  (check-documentation (legal-documentation-p method documentation)))
      (unless (eq check-qualifiers t)
	(lose :qualifiers qualifiers check-qualifiers))
      (unless (eq check-lambda-list t)
	(lose :lambda-list lambda-list check-lambda-list))
      (unless (eq check-specializers t)
	(lose :specializers specializers check-specializers))
      (unless (eq check-function t)
	(lose :function function check-function))
      (unless (eq check-documentation t)
	(lose :documentation documentation check-documentation)))))

(defmethod legal-documentation-p ((object standard-method) x)
  (if (or (null x) (stringp x))
      t
      "is not a string or NULL"))

(defmethod legal-lambda-list-p ((object standard-method) x)
  (declare (ignore x))
  t)

(defmethod legal-method-function-p ((object standard-method) x)
  (if (functionp x)
      t
      "is not a function"))

(defmethod legal-qualifiers-p ((object standard-method) x)
  (dolist (q x t)
    (let ((ok (legal-qualifier-p object q)))
      (unless (eq ok t)
	(return-from legal-qualifiers-p
	  (format nil "Contains ~S which ~A" q ok))))))

(defmethod legal-qualifier-p ((object standard-method) x)
  (if (and x (atom x))
      t
      "is not a non-null atom"))

(defmethod legal-slot-name-p ((object standard-method) x)
  (cond ((not (symbolp x))
	 "is not a symbol and so cannot be bound")
	;;
	;; Structure slots names can be any symbol.
	(*allow-funny-slot-names*)
	((keywordp x)
	 "is a keyword and so cannot be bound")
	((memq x '(t nil))
	 "cannot be bound")
	((constantp x)
	 "is a constant and so cannot be bound")
	(t t)))

(defmethod legal-specializers-p ((object standard-method) x)
  (dolist (s x t)
    (let ((ok (legal-specializer-p object s)))
      (unless (eq ok t)
	(return-from legal-specializers-p
	  (format nil "Contains ~S which ~A" s ok))))))

(defmethod legal-specializer-p ((object standard-method) x)
  (if (if *allow-experimental-specializers-p*
	  (specializerp x)
	  (or (classp x)
	      (eql-specializer-p x)))
      t
      "is neither a class object nor an eql specializer"))

(defmethod shared-initialize :before ((method standard-accessor-method)
				      slot-names
				      &key slot-name slot-definition)
  (declare (ignore slot-names))
  (unless slot-definition
    (let ((legalp (legal-slot-name-p method slot-name)))
      (unless (eq legalp t)
	(error "The value of the ~s initarg, ~s, ~A."
	       :slot-name slot-name legalp)))))

(defmethod shared-initialize :after ((method standard-method) slot-names
				     &rest initargs
				     &key qualifiers method-spec plist)
  (declare (ignore slot-names method-spec plist))
  (initialize-method-function initargs nil method)
  (setf (plist-value method 'qualifiers) qualifiers))

(defmethod shared-initialize :after ((method standard-accessor-method)
				     slot-names
				     &key)
  (declare (ignore slot-names))
  (with-slots (slot-name slot-definition)
    method
    (unless slot-definition
      (let ((class (accessor-method-class method)))
	(when (slot-class-p class)
	  (setq slot-definition (find slot-name (class-direct-slots class)
				      :key #'slot-definition-name)))))
    (when (and slot-definition (null slot-name))
      (setq slot-name (slot-definition-name slot-definition)))))

(defmethod shared-initialize :before
	   ((gf standard-generic-function)
	    slot-names
	    &key (name nil namep)
		 (lambda-list () lambda-list-p)
		 argument-precedence-order
		 declarations
		 documentation
		 (method-class nil method-class-supplied-p)
		 (method-combination nil method-combination-supplied-p))
  (declare (ignore slot-names
		   declarations argument-precedence-order documentation
		   lambda-list lambda-list-p))

  (when namep
    (set-function-name gf name))
		   
  (flet ((initarg-error (initarg value string)
	   (error (format nil "~~@<When initializing the generic-function ~S: ~
                               The ~S initialization argument was ~A.  ~
                               It must be ~A.~~@:>"
			  gf initarg value string))))
    (cond (method-class-supplied-p
	   (when (symbolp method-class)
	     (setq method-class (find-class method-class)))
	   (unless (and (classp method-class)
			(*subtypep (class-eq-specializer method-class)
				   *the-class-method*))
	     (initarg-error :method-class method-class "a method class"))
	   (setf (slot-value gf 'method-class) method-class))
	  ((slot-boundp gf 'method-class))
	  (t
	   (initarg-error :method-class "not supplied" "a method class")))
    (cond (method-combination-supplied-p
	   (unless (method-combination-p method-combination)
	     (initarg-error :method-combination method-combination
			    "a method combination object")))
	  ((slot-boundp gf 'method-combination))
	  (t
	   (initarg-error :method-combination
			  "not supplied"
			  "a method combination object")))))


;;;
;;; These three are scheduled for demolition.
;;; 
(defmethod remove-named-method (gf-name argument-specifiers &optional extra)
  (let (gf method)
    (cond ((or (not (fboundp gf-name))
	       (not (generic-function-p (setq gf (gdefinition gf-name)))))
	   (error "~@<~S does not name a generic function.~@:>" gf-name))
	  ((null (setq method
		       (get-method gf extra
				   (parse-specializers argument-specifiers)
				   nil)))
	   (error "~@<There is no method for the generic function ~S ~
                   matching argument specifiers ~S.~@:>"
		  gf argument-specifiers))
	  (t
	   (remove-method gf method)))))

(defun #+loadable-pcl add-named-method
       #-loadable-pcl real-add-named-method
    (gf-name qualifiers specializers lambda-list &rest other-initargs)
  ;; What about changing the class of the generic-function if there is
  ;; one.  Whose job is that anyways.  Do we need something kind of
  ;; like class-for-redefinition?
  (let* ((gf (ensure-generic-function gf-name))
	 (specs (parse-specializers specializers))
	 (proto (method-prototype-for-gf gf-name))
	 (new (apply #'make-instance (class-of proto)
		     :qualifiers qualifiers
		     :specializers specs
		     :lambda-list lambda-list
		     other-initargs)))
    (add-method gf new)
    new))

	

#+loadable-pcl
(defmethod get-method ((gf standard-generic-function)
		       qualifiers specializers &optional (errorp t))
  (loop with nspecializers = (length specializers)
	for method in (generic-function-methods gf)
	as quals = (method-qualifiers method)
	as specs = (method-specializers method)
	when (and (equal quals qualifiers)
		  (= (length specs) nspecializers)
		  (every #'same-specializer-p specs specializers)) do
	  (return-from get-method method))
  (when errorp
    (error "~@<No method on ~S with qualifiers ~S and ~
           specializers ~S.~@:>"
	   gf qualifiers specializers)))

#-loadable-pcl
(defun real-get-method (gf qualifiers specializers &optional (errorp t))
  (loop with nspecializers = (length specializers)
	for method in (generic-function-methods gf)
	as quals = (method-qualifiers method)
	as specs = (method-specializers method)
	when (and (equal quals qualifiers)
		  (= (length specs) nspecializers)
		  (every #'same-specializer-p specs specializers)) do
	  (return-from real-get-method method))
  (when errorp
    (error "~@<No method on ~S with qualifiers ~S and ~
            specializers ~S.~@:>"
	   gf qualifiers specializers)))

(defmethod find-method ((gf standard-generic-function)
			qualifiers specializers &optional (errorp t))
  (let ((nreq (count-gf-required-parameters gf)))
    (when (/= (length specializers) nreq)
      (error "~@<The generic function ~s takes ~d required argument~p.~@:>"
	     gf nreq nreq))
    (real-get-method gf qualifiers (parse-specializers specializers) errorp)))
  
;;;
;;;
;;;

(defun set-methods (gf methods)
  (setf (generic-function-methods gf) nil)
  (loop (when (null methods) (return gf))
	(real-add-method gf (pop methods) methods)))

(defun update-gf-dependents (gf action method)
  (map-dependents gf
		  (lambda (dependent)
		    (update-dependent gf dependent action method))))

(defun real-add-method (gf method &optional skip-dfun-update-p)
  (when (method-generic-function method)
    (error "~@<The method ~S is already part of the generic ~
            function ~S.  It can't be added to another generic ~
            function until it is removed from the first one.~@:>"
	   method (method-generic-function method)))
  ;;
  (flet (;;
	 ;; Return T if METHOD-A and METHOD-B have similar lambda
	 ;; lists, that is, when they are considered the same
	 ;; when redefining methods.
	 (similar-lambda-lists-p (method-a method-b)
	   (multiple-value-bind (a-nreq a-nopt a-keyp a-restp)
	       (analyze-lambda-list (method-lambda-list method-a))
	     (multiple-value-bind (b-nreq b-nopt b-keyp b-restp)
		 (analyze-lambda-list (method-lambda-list method-b))
	       (and (= a-nreq b-nreq) (= a-nopt b-nopt)
		    (eq (or a-keyp a-restp) (or b-keyp b-restp)))))))
    (let* ((qualifiers (method-qualifiers method))
	   (specializers (method-specializers method))
	   (old (get-method gf qualifiers specializers nil)))
      ;;
      ;; If there is already a method like this one then we must
      ;; get rid of it before proceeding.  Note that we call the
      ;; generic function remove-method to remove it rather than
      ;; doing it in some internal way.
      (when (and old (similar-lambda-lists-p old method))
	(remove-method gf old))
      ;;
      ;; Add the method.
      (setf (method-generic-function method) gf)
      (push method (generic-function-methods gf))
      (dolist (specializer specializers)
	(add-direct-method specializer method))
      ;;
      ;; SET-ARG-INFO signals an error if the new method has an
      ;; incongruent lambda list, but the function depends on the
      ;; new method already having been added to the gf.  So kluge
      ;; around a bit.
      (let ((remove-again-p t))
	(unwind-protect
	     (progn
	       (set-arg-info gf :new-method method)
	       (setq remove-again-p nil))
	  (when remove-again-p
	    (remove-method gf method))))
      ;;
      ;; Warn about invalid qualifiers, if we can.
      (let ((mc (generic-function-method-combination gf)))
	(cond ((eq mc *standard-method-combination*)
	       (when (and qualifiers
			  (or (cdr qualifiers)
			      (not (memq (car qualifiers)
					 '(:around :before :after)))))
		 (warn "~@<Method ~s contains invalid qualifiers for ~
                        the standard method combination.~@:>"
		       method)))
	      ((short-method-combination-p mc)
	       (let ((mc-name (method-combination-type mc)))
		 (when (or (/= (length qualifiers) 1)
			   (and (neq (car qualifiers) :around)
				(neq (car qualifiers) mc-name)))
		   (warn "~@<Method ~s contains invalid qualifiers for ~
                          the method combination ~s.~@:>"
			 method mc-name))))))
      ;;
      (unless skip-dfun-update-p
	(update-ctors 'add-method :generic-function gf :method method)
	(update-dfun gf))
      (update-accessor-pvs 'add-method gf method old)
      (update-gf-dependents gf 'add-method method)
      gf)))
  
(defun real-remove-method (gf method)
  (when (eq gf (method-generic-function method))
    (let* ((methods (generic-function-methods gf))
	   (new-methods (remove method methods)))	      
      (setf (method-generic-function method) nil)
      (setf (generic-function-methods gf) new-methods)
      (dolist (specializer (method-specializers method))
	(remove-direct-method specializer method))
      (set-arg-info gf)
      (update-ctors 'remove-method :generic-function gf :method method)
      (update-dfun gf)
      (update-accessor-pvs 'remove-method gf method)
      (update-gf-dependents gf 'remove-method method)))
  gf)


;;;; **************************
;;;; Generic Functions  *******
;;;; **************************

(defmethod generic-function-argument-precedence-order
    ((gf standard-generic-function))
  (assert (eq *boot-state* 'complete))
  (loop with arg-info = (gf-arg-info gf)
	with lambda-list = (arg-info-lambda-list arg-info)
	for argument-position in (arg-info-precedence arg-info)
	collect (nth argument-position lambda-list)))

(defmethod generic-function-lambda-list ((gf generic-function))
  (gf-lambda-list gf))

(defmethod gf-fast-method-function-p ((gf standard-generic-function))
  (gf-info-fast-mf-p (slot-value gf 'arg-info)))

(defmethod initialize-instance :after ((gf standard-generic-function)
				       &key (lambda-list nil lambda-list-p)
				       argument-precedence-order)
  (with-slots (arg-info) gf
    (if lambda-list-p
	(set-arg-info gf 
		      :lambda-list lambda-list
		      :argument-precedence-order argument-precedence-order)
	(set-arg-info gf))
    (when (arg-info-valid-p arg-info)
      (update-dfun gf))))

(defmethod reinitialize-instance :around
    ((gf standard-generic-function) &rest args &key
     (lambda-list nil lambda-list-p)
     (argument-precedence-order nil argument-precedence-order-p))
  (let ((old-mc (generic-function-method-combination gf)))
    (prog1 (call-next-method)
      ;;
      ;; Remove cached emf entries if the method combination might
      ;; have changed in a way that makes re-computing emfs necesasry.
      ;; EQ catches more cases than strictly necessary, but it is good
      ;; enough.
      (unless (eq old-mc (generic-function-method-combination gf))
	(flush-effective-method-cache gf))
      ;;
      ;; Update the gf's arg-info.
      (if lambda-list-p
	  (if argument-precedence-order-p
	      (set-arg-info gf :lambda-list lambda-list
			    :argument-precedence-order
			    argument-precedence-order)
	      (set-arg-info gf :lambda-list lambda-list))
	  (set-arg-info gf))
      ;;
      ;; Update the gf's dfun, if possible.
      (when (and (arg-info-valid-p (gf-arg-info gf))
		 (not (null args))
		 (or lambda-list-p (cddr args)))
	  (update-dfun gf))
      ;;
      ;; Update dependent objects.
      (map-dependents gf (lambda (dependent)
			   (apply #'update-dependent gf dependent args))))))


(defun compute-applicable-methods-function (gf arguments)
  (values (compute-applicable-methods-using-types 
	   gf
	   (types-from-arguments gf arguments 'eql))))
  
(defmethod compute-applicable-methods ((gf generic-function) arguments)
  (values (compute-applicable-methods-using-types 
	   gf
	   (types-from-arguments gf arguments 'eql))))

(defmethod compute-applicable-methods-using-classes ((gf generic-function)
						     classes)
  (compute-applicable-methods-using-types 
   gf
   (types-from-arguments gf classes 'class-eq)))

(defun proclaim-incompatible-superclasses (classes)
  (let ((classes (mapcar (lambda (class)
			   (if (symbolp class)
			       (find-class class)
			       class))
			 classes)))
    (dolist (class classes)
      (dolist (other-class classes)
	(unless (eq class other-class)
	  (pushnew other-class (class-incompatible-superclass-list class)))))))

(defun superclasses-compatible-p (class1 class2)
  (let ((cpl1 (cpl-or-nil class1))
	(cpl2 (cpl-or-nil class2)))
    (dolist (sc1 cpl1 t)
      (dolist (ic (class-incompatible-superclass-list sc1))
	(when (memq ic cpl2)
	  (return-from superclasses-compatible-p nil))))))

(mapc
 #'proclaim-incompatible-superclasses
 '(;; superclass class
   (built-in-class std-class structure-class) ; direct subclasses of pcl-class
   (standard-class funcallable-standard-class)
   ;; superclass metaobject
   (class eql-specializer class-eq-specializer method method-combination
    generic-function slot-definition)
   ;; metaclass built-in-class
   (number sequence character   	; direct subclasses of t, but not array
    standard-object structure-object)   ;                         or symbol
   (number array character symbol	; direct subclasses of t, but not sequence
    standard-object structure-object)
   (complex float rational)		; direct subclasses of number
   (integer ratio)			; direct subclasses of rational
   (list vector)			; direct subclasses of sequence
   (cons null)				; direct subclasses of list
   (string bit-vector)			; direct subclasses of vector
   ))




(defmethod same-specializer-p ((specl1 specializer) (specl2 specializer))
  nil)

(defmethod same-specializer-p ((specl1 class) (specl2 class))
  (eq specl1 specl2))

(defmethod specializer-class ((specializer class))
  specializer)

(defmethod same-specializer-p ((specl1 class-eq-specializer)
			       (specl2 class-eq-specializer))
  (eq (specializer-class specl1) (specializer-class specl2)))

(defmethod same-specializer-p ((specl1 eql-specializer)
			       (specl2 eql-specializer))
  (eq (specializer-object specl1) (specializer-object specl2)))

(defmethod specializer-class ((specializer eql-specializer))
  (class-of (slot-value specializer 'object)))

(defvar *in-gf-arg-info-p* nil)
(setf (gdefinition 'arg-info-reader)
      (let ((mf (initialize-method-function
		 (make-internal-reader-method-function
		  'standard-generic-function 'arg-info)
		 t)))
	(lambda (&rest args) (funcall mf args nil))))

(defun types-from-arguments (gf arguments &optional type-modifier)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-function-info gf)
    (declare (ignore applyp metatypes nkeys))
    (loop repeat nreq
	  if arguments
	    if type-modifier
	      collect `(,type-modifier ,(pop arguments)) into types
	    else
	      collect (pop arguments) into types
	  else do
	    (error "~@<Generic function ~S requires at least ~D arguments.~@:>"
		   (generic-function-name gf) nreq)
	  finally
	    (return (values types arg-info)))))

(defun get-wrappers-from-classes (nkeys wrappers classes metatypes)
  (let* ((w wrappers) (w-tail w) (mt-tail metatypes))
    (dolist (class (if (listp classes) classes (list classes)))
      (unless (eq t (car mt-tail))
	(let ((c-w (class-wrapper class)))
	  (unless c-w (return-from get-wrappers-from-classes nil))
	  (if (eql nkeys 1)
	      (setq w c-w)
	      (setf (car w-tail) c-w
		    w-tail (cdr w-tail)))))
      (setq mt-tail (cdr mt-tail)))
    w))

(defun sdfun-for-caching (gf classes)
  (let ((types (mapcar #'class-eq-type classes)))
    (multiple-value-bind (methods all-applicable-and-sorted-p)
	(compute-applicable-methods-using-types gf types)
      (let ((generator (get-secondary-dispatch-function1 
			gf methods types nil t
			all-applicable-and-sorted-p)))
	(make-callable gf methods generator nil
		       (mapcar #'class-wrapper classes))))))

(defun value-for-caching (gf classes)
  (let ((methods (compute-applicable-methods-using-types 
		   gf (mapcar #'class-eq-type classes))))
    (method-function-get (or (method-fast-function (car methods))
			     (method-function (car methods)))
			 :constant-value)))

(defun default-secondary-dispatch-function (gf)
  (lambda (&rest args)
    (let ((methods (compute-applicable-methods gf args)))
      (if methods
	  (let ((emf (get-effective-method-function gf methods)))
	    (invoke-emf emf args))
	  (apply #'no-applicable-method gf args)))))

(defvar *std-cam-methods* nil)

(defun compute-applicable-methods-emf (gf)  
  (if (eq *boot-state* 'complete)
      (let* ((cam (gdefinition 'compute-applicable-methods))
	     (cam-methods (compute-applicable-methods-using-types
			   cam (list `(eql ,gf) t))))
	(unless *std-cam-methods*
	  (setq *std-cam-methods*
		(compute-applicable-methods-using-types
		 cam (list `(eql ,cam) t))))
	(values (get-effective-method-function cam cam-methods)
		(every #'eq cam-methods *std-cam-methods*)))
      (values #'compute-applicable-methods-function t)))

(defun compute-applicable-methods-emf-std-p (gf)
  (gf-info-c-a-m-emf-std-p (gf-arg-info gf)))

(defvar *old-c-a-m-gf-methods* nil)

(defun update-all-c-a-m-gf-info (c-a-m-gf)
  (let ((methods (generic-function-methods c-a-m-gf)))
    (if (and *old-c-a-m-gf-methods*
	     (every (lambda (old-method)
		      (member old-method methods))
		    *old-c-a-m-gf-methods*))
	(let ((gfs-to-do nil)
	      (gf-classes-to-do nil))
	  (dolist (method methods)
	    (unless (member method *old-c-a-m-gf-methods*)
	      (let ((specl (car (method-specializers method))))
		(if (eql-specializer-p specl)
		    (pushnew (specializer-object specl) gfs-to-do)
		    (pushnew (specializer-class specl) gf-classes-to-do)))))
	  (map-all-generic-functions 
	   (lambda (gf)
	     (when (or (member gf gfs-to-do)
		       ;; FIXME: Always returning NIL cannot be right, or?
		       (dolist (class gf-classes-to-do nil)
			 (member class (class-precedence-list (class-of gf)))))
	       (update-c-a-m-gf-info gf)))))
	(map-all-generic-functions #'update-c-a-m-gf-info))
    (setq *old-c-a-m-gf-methods* methods)))

(defun update-gf-info (gf)
  (update-c-a-m-gf-info gf)
  (update-gf-simple-accessor-type gf))

(defun update-c-a-m-gf-info (gf)
  (unless (early-gf-p gf)
    (multiple-value-bind (c-a-m-emf std-p)
	(compute-applicable-methods-emf gf)
      (let ((arg-info (gf-arg-info gf)))
	(setf (gf-info-static-c-a-m-emf arg-info) c-a-m-emf)
	(setf (gf-info-c-a-m-emf-std-p arg-info) std-p)))))

(defun update-gf-simple-accessor-type (gf)
  (let ((arg-info (gf-arg-info gf)))
    (setf (gf-info-simple-accessor-type arg-info)
	  (let* ((methods (generic-function-methods gf))
		 (class (and methods (class-of (car methods))))
		 (type (and class (cond ((eq class *the-class-standard-reader-method*)
					 'reader)
					((eq class *the-class-standard-writer-method*)
					 'writer)
					((eq class *the-class-standard-boundp-method*)
					 'boundp)))))
	    (when (and (gf-info-c-a-m-emf-std-p arg-info)
		       type
		       (dolist (method (cdr methods) t)
			 (unless (eq class (class-of method)) (return nil)))
		       (eq (generic-function-method-combination gf)
			   *standard-method-combination*))
	      type)))))

;;;
;;; Called from COMPUTE-DISCRIMINATING-FUNCTION for
;;; SLOT-VALUE-USING-CLASS, (SETF SLOT-VALUE-USING-CLASS) and
;;; SLOT-BOUNDP-USING-CLASS.  GF is one of these functions, TYPE is
;;; one of the symbols READER, WRITER, or BOUNDP.
;;;
;;; Note that these gfs have unusual, fixed dispatch functions calling
;;; effective methods stored in effective slot definitions, which are
;;; set in COMPUTE-SLOT-ACCESSOR-INFO.
;;;
;;; FIXME: It looks to me like *NEW-CLASS* could be deleted, but
;;; I'm not 100% sure.
;;; 
(defun update-slot-value-gf-info (gf type)
  (unless *new-class*
    (update-std-or-str-methods gf type))
  (when (and (standard-svuc-method type)
	     (structure-svuc-method type))
    (flet ((update-class (class)
	     (when (class-finalized-p class)
	       (dolist (slotd (class-slots class))
		 (compute-slot-accessor-info slotd type gf)))))
      (if *new-class*
	  (update-class *new-class*)
	  (map-all-classes #'update-class 'slot-object)))))

;;;
;;; Return two values.  First value is a function to be stored in
;;; effective slot definition SLOTD for reading it with
;;; SLOT-VALUE-USING-CLASS, setting it with (SETF
;;; SLOT-VALUE-USING-CLASS) or testing it with
;;; SLOT-BOUNDP-USING-CLASS.  GF is one of these generic functions,
;;; TYPE is one of the symbols READER, WRITER, BOUNDP.  CLASS is
;;; SLOTD's class.
;;;
;;; Second value is true if the function returned is one of the
;;; optimized standard functions for the purpose, which are used
;;; when only standard methods are applicable.
;;;
;;; FIXME: Change all these wacky function names to something sane.
;;; 
(defun get-accessor-method-function (gf type class slotd)
  (let* ((std-method (standard-svuc-method type))
	 (str-method (structure-svuc-method type))
	 (types1 `((eql ,class) (class-eq ,class) (eql ,slotd)))
	 (types (if (eq type 'writer) `(t ,@types1) types1))
	 (methods (compute-applicable-methods-using-types gf types))
	 (std-p (null (cdr methods))))
    (values
     (if std-p
	 (get-optimized-std-accessor-method-function class slotd type)
	 (let* ((optimized-std-fn
		 (get-optimized-std-slot-value-using-class-method-function
		  class slotd type))
		(method-alist
		 `((,(car (or (member std-method methods)
			      (member str-method methods)
			      (internal-error "In get-accessor-method-function.")))
		     ,optimized-std-fn)))
		(wrappers
		 ;;
		 ;; This used to be wrapped in 
		 ;; 
		 ;; (unless (and (eq type 'writer)
		 ;;	      (every (lambda (x)
		 ;;		      (eq (car (method-specializers x))
		 ;;			  *the-class-t*))
		 ;;		    methods))
		 ;;
		 ;; but that looks wrong because WRAPPERS nil signals
		 ;; to GET-SECONDARY-DISPATCH-FUNCTION that we are NOT
		 ;; generating code for an emf, which is wrong because
		 ;; we are.
		 ;;
		 ;; See the message from Kevin Rosenberg <kevin@rosenberg.net>
		 ;; to cmucl-imp from Tue, 22 Apr 2003 13:28:23 -0600
		 ;; and the following thread for a test case where this
		 ;; causes problems.
		 ;;
		 ;; gerd, 2003-04-25
		 (let ((wrappers (list (wrapper-of class)
				       (class-wrapper class)
				       (wrapper-of slotd))))
		   (if (eq type 'writer)
		       (cons (class-wrapper *the-class-t*) wrappers)
		       wrappers)))
		(sdfun (get-secondary-dispatch-function 
			gf methods types method-alist wrappers)))
	   (get-accessor-from-svuc-method-function class slotd sdfun type)))
     std-p)))

(defun get-accessor-from-svuc-method-function (class slotd sdfun name)
  (macrolet ((emf-funcall (emf &rest args)
	       `(invoke-effective-method-function ,emf nil ,@args)))
    (let ((function
	   (ecase name
	     (reader
	      (lambda (instance)
		(emf-funcall sdfun class instance slotd)))
	     (writer
	      (lambda (nv instance)
		(emf-funcall sdfun nv class instance slotd)))
	     (boundp
	      (lambda (instance)
		(emf-funcall sdfun class instance slotd))))))
      (set-function-name function `(,name ,(class-name class)
					  ,(slot-definition-name slotd)))
      function)))

(defvar *standard-slot-value-using-class-method* nil)
(defvar *standard-setf-slot-value-using-class-method* nil)
(defvar *standard-slot-boundp-using-class-method* nil)
(defvar *structure-slot-value-using-class-method* nil)
(defvar *structure-setf-slot-value-using-class-method* nil)
(defvar *structure-slot-boundp-using-class-method* nil)
(defvar *condition-slot-value-using-class-method* nil)
(defvar *condition-setf-slot-value-using-class-method* nil)
(defvar *condition-slot-boundp-using-class-method* nil)

(defun standard-svuc-method (type)
  (case type
    (reader *standard-slot-value-using-class-method*)
    (writer *standard-setf-slot-value-using-class-method*)
    (boundp *standard-slot-boundp-using-class-method*)))

(defun set-standard-svuc-method (type method)
  (case type
    (reader (setq *standard-slot-value-using-class-method* method))
    (writer (setq *standard-setf-slot-value-using-class-method* method))
    (boundp (setq *standard-slot-boundp-using-class-method* method))))

(defun structure-svuc-method (type)
  (case type
    (reader *structure-slot-value-using-class-method*)
    (writer *structure-setf-slot-value-using-class-method*)
    (boundp *structure-slot-boundp-using-class-method*)))

(defun set-structure-svuc-method (type method)
  (case type
    (reader (setq *structure-slot-value-using-class-method* method))
    (writer (setq *structure-setf-slot-value-using-class-method* method))
    (boundp (setq *structure-slot-boundp-using-class-method* method))))

(defun condition-svuc-method (type)
  (case type
    (reader *condition-slot-value-using-class-method*)
    (writer *condition-setf-slot-value-using-class-method*)
    (boundp *condition-slot-boundp-using-class-method*)))

(defun set-condition-svuc-method (type method)
  (case type
    (reader (setq *condition-slot-value-using-class-method* method))
    (writer (setq *condition-setf-slot-value-using-class-method* method))
    (boundp (setq *condition-slot-boundp-using-class-method* method))))

(defun update-std-or-str-methods (gf type)
  (dolist (method (generic-function-methods gf))
    (let ((specls (method-specializers method)))
      (when (and (or (not (eq type 'writer))
		     (eq (pop specls) *the-class-t*))
		 (every #'classp specls))
	(cond ((and (eq (class-name (car specls))
			'std-class)
		    (eq (class-name (cadr specls)) 
			'standard-object)
		    (eq (class-name (caddr specls)) 
			'standard-effective-slot-definition))
	       (set-standard-svuc-method type method))
	      ((and (eq (class-name (car specls))
			'condition-class)
		    (eq (class-name (cadr specls)) 
			'condition)
		    (eq (class-name (caddr specls)) 
			'condition-effective-slot-definition))
	       (set-condition-svuc-method type method))
	      ((and (eq (class-name (car specls))
			'structure-class)
		    (eq (class-name (cadr specls))
			'structure-object)
		    (eq (class-name (caddr specls)) 
			'structure-effective-slot-definition))
	       (set-structure-svuc-method type method)))))))

(defun mec-all-classes-internal (spec precompute-p)
  (cons (specializer-class spec)
	(and (classp spec)
	     precompute-p
	     (not (or (eq spec *the-class-t*)
		      (eq spec *the-class-slot-object*)
		      (eq spec *the-class-standard-object*)
		      (eq spec *the-class-structure-object*)))
	     (let ((sc (class-direct-subclasses spec)))
	       (when sc
		 (mapcan (lambda (class)
			   (mec-all-classes-internal class precompute-p))
			 sc))))))

(defun mec-all-classes (spec precompute-p)
  (let ((classes (mec-all-classes-internal spec precompute-p)))
    (if (null (cdr classes))
	classes
	(let* ((a-classes (cons nil classes))
	       (tail classes))
	  (loop (when (null (cdr tail))
		  (return (cdr a-classes)))
		(let ((class (cadr tail))
		      (ttail (cddr tail)))
		  (if (dolist (c ttail nil)
			(when (eq class c) (return t)))
		      (setf (cdr tail) (cddr tail))
		      (setf tail (cdr tail)))))))))

(defun mec-all-class-lists (spec-list precompute-p)
  (if (null spec-list)
      (list nil)
      (let* ((car-all-classes (mec-all-classes (car spec-list) precompute-p))
	     (all-class-lists (mec-all-class-lists (cdr spec-list) precompute-p)))
	(mapcan (lambda (list)
		  (mapcar (lambda (c) (cons c list)) car-all-classes))
		all-class-lists))))

(defun make-emf-cache (gf valuep cache classes-list new-class)
  (let* ((arg-info (gf-arg-info gf))
	 (nkeys (arg-info-nkeys arg-info))
	 (metatypes (arg-info-metatypes arg-info))
	 (wrappers (unless (eq nkeys 1) (make-list nkeys)))
	 (precompute-p (gf-precompute-dfun-and-emf-p arg-info))
	 (default '(default)))
    (flet ((add-class-list (classes)
	     (when (or (null new-class) (memq new-class classes))
	       (let ((wrappers (get-wrappers-from-classes 
				nkeys wrappers classes metatypes)))
		 (when (and wrappers
			    (eq default (probe-cache cache wrappers default)))
		   (let ((value (cond ((eq valuep t)
				       (sdfun-for-caching gf classes))
				      ((eq valuep :constant-value)
				       (value-for-caching gf classes)))))
		     (setq cache (fill-cache cache wrappers value))))))))
      (if classes-list
	  (mapc #'add-class-list classes-list)
	  (dolist (method (generic-function-methods gf))
	    (mapc #'add-class-list
		  (mec-all-class-lists (method-specializers method) precompute-p))))
      cache)))


(defmacro class-test (arg class)
  (cond ((eq class *the-class-t*)
	 t)
	((eq class *the-class-slot-object*)
	 `(not (lisp:typep (kernel::class-of ,arg) 'kernel::built-in-class)))
	((eq class *the-class-standard-object*)
	 `(std-instance-p ,arg))
	((eq class *the-class-funcallable-standard-object*)
	 `(fsc-instance-p ,arg))
	;; TYPEP is now sometimes faster than doing memq of the cpl
	(t
	 `(typep ,arg ',(class-name class)))))

(defmacro class-eq-test (arg class)
  `(eq (class-of ,arg) ',class))

(defmacro eql-test (arg object)
  `(eql ,arg ',object))

(defun dnet-methods-p (form)
  (and (consp form)
       (or (eq (car form) 'methods)
	   (eq (car form) 'unordered-methods))))

(defmacro scase (arg &rest clauses) ; This is case, but without gensyms
  `(let ((.case-arg. ,arg))
     (cond ,@(mapcar (lambda (clause)
		       (list* (cond ((null (car clause))
				     nil)
				    ((consp (car clause))
				     (if (null (cdar clause))
					 `(eql .case-arg. ',(caar clause))
					 `(member .case-arg. ',(car clause))))
				    ((member (car clause) '(t otherwise))
				     `t)
				    (t
				     `(eql .case-arg. ',(car clause))))
			      nil
			      (cdr clause)))
		     clauses))))

(defmacro mcase (arg &rest clauses) `(scase ,arg ,@clauses))

(defun generate-discrimination-net (gf methods types sorted-p)
  (let* ((arg-info (gf-arg-info gf))
	 (precedence (arg-info-precedence arg-info)))
    (generate-discrimination-net-internal 
     gf methods types
     (lambda (methods known-types)
       (if (or sorted-p
	       (block one-order-p
		 (let ((sorted-methods nil))
		   (map-all-orders 
		    (copy-list methods) precedence
		    (lambda (methods)
		      (when sorted-methods (return-from one-order-p nil))
		      (setq sorted-methods methods)))
		   (setq methods sorted-methods))
		 t))
	   `(methods ,methods ,known-types)
	   `(unordered-methods ,methods ,known-types)))
     (lambda (position type true-value false-value)
       (let ((arg (dfun-arg-symbol position)))
	 (if (eq (car type) 'eql)
	     (let* ((false-case-p (and (consp false-value)
				       (or (eq (car false-value) 'scase)
					   (eq (car false-value) 'mcase))
				       (eq arg (cadr false-value))))
		    (false-clauses (if false-case-p
				       (cddr false-value)
				       `((t ,false-value))))
		    (case-sym (if (and (dnet-methods-p true-value)
				       (if false-case-p
					   (eq (car false-value) 'mcase)
					   (dnet-methods-p false-value)))
				  'mcase
				  'scase))
		    (type-sym `(,(cadr type))))
	       `(,case-sym ,arg
			   (,type-sym ,true-value)
			   ,@false-clauses))
	     `(if ,(let ((arg (dfun-arg-symbol position)))
			(case (car type)
			  (class    `(class-test    ,arg ,(cadr type)))
			  (class-eq `(class-eq-test ,arg ,(cadr type)))))
		  ,true-value
		  ,false-value))))
     #'identity)))

(defun class-from-type (type)
  (if (or (atom type) (eq (car type) t))
      *the-class-t*
      (case (car type)
	(and (dolist (type (cdr type) *the-class-t*)
	       (when (and (consp type) (not (eq (car type) 'not)))
		 (return (class-from-type type)))))
	(not *the-class-t*)
        (eql (class-of (cadr type)))
        (class-eq (cadr type))
        (class (cadr type)))))

(defun precompute-effective-methods (gf caching-p)
  (let* ((arg-info (gf-arg-info gf))
	 (methods (generic-function-methods gf))
	 (precedence (arg-info-precedence arg-info))
	 (*in-precompute-effective-methods-p* t)
	 (classes-list nil))
    (generate-discrimination-net-internal 
     gf methods nil
     (lambda (methods known-types)
       (when methods
	 (let ((no-eql-specls-p (not (methods-contain-eql-specializer-p methods))))
	   (map-all-orders 
	    methods precedence
	    (lambda (methods)
	      (get-secondary-dispatch-function1 
	       gf methods known-types
	       nil caching-p no-eql-specls-p))))))
     (lambda (position type true-value false-value)
       (declare (ignore position type true-value false-value))
       nil)
     (lambda (type)
       (if (and (consp type) (eq (car type) 'eql))
	   `(class-eq ,(class-of (cadr type)))
	   type)))
    classes-list))

; we know that known-type implies neither new-type nor `(not ,new-type) 
(defun augment-type (new-type known-type)
  (if (or (eq known-type t)
	  (eq (car new-type) 'eql))
      new-type
      (let ((so-far (if (and (consp known-type) (eq (car known-type) 'and))
			(cdr known-type)
			(list known-type))))
	(unless (eq (car new-type) 'not)
	  (setq so-far
		(mapcan (lambda (type)
			  (unless (*subtypep new-type type)
			    (list type)))
			so-far)))
	(if (null so-far)
	    new-type
	    `(and ,new-type ,@so-far)))))

;;;
;;; Given a set of methods METHDOS of generic function GF and a list
;;; of the types of actual arguments TYPES, call METHODS-FUNCTION with
;;; two arguments: a list of possibly applicable methods and a list of
;;; type expressions that can be used on actual arguments for testing
;;; if a method is applicable.
;;;
;;; TEST-FUNCTION is a function to determine if a method is possibly
;;; applicable, if SPECIALIZER-APPLICABLE-USING-TYPE-P can't tell.
;;;
;;; TYPE-FUNCTION is a function to transform a method specializer's
;;; type.
;;;
;;; Or something.
;;;
;;; Rewrite this to a control flow that one can follow!
;;;
(defun generate-discrimination-net-internal 
    (gf methods types methods-function test-function type-function)
  (let* ((arg-info (gf-arg-info gf))
	 (precedence (arg-info-precedence arg-info))
	 (nreq (arg-info-number-required arg-info))
	 (metatypes (arg-info-metatypes arg-info)))
    (labels ((do-column (precedence contenders known-types)
	       (if precedence
		   (let* ((position (car precedence))
			  (known-type (or (nth position types) t)))
		     (if (eq (nth position metatypes) t)
			 (do-column (cdr precedence) contenders
				    (cons (cons position known-type) known-types))
			 (do-methods precedence contenders 
				     known-type () known-types)))
		   (funcall methods-function contenders 
			    (let ((k-t (make-list nreq)))
			      (dolist (index+type known-types)
				(setf (nth (car index+type) k-t) (cdr index+type)))
			      k-t))))
	     (do-methods (precedence contenders known-type winners known-types)
	       ;;
               ;; <contenders>
	       ;;   is a (sorted) list of methods that must be discriminated
               ;; <known-type>
	       ;;   is the type of this argument, constructed from tests already made.
               ;; <winners>
	       ;;   is a (sorted) list of methods that are potentially applicable
	       ;;   after the discrimination has been made.
	       ;;   
               (if (null contenders)
		   (do-column (cdr precedence) winners
			      (cons (cons (car precedence) known-type) known-types))
                   (let* ((position (car precedence))
			  (method (car contenders))
			  (specl (nth position (method-specializers method)))
                          (type (funcall type-function (type-from-specializer specl))))
		     (multiple-value-bind (app-p maybe-app-p)
			 (specializer-applicable-using-type-p type known-type)
		       (flet ((determined-to-be (truth-value)
				(if truth-value app-p (not maybe-app-p)))
			      (do-if (truth &optional implied)
				(let ((ntype (if truth type `(not ,type))))
				  (do-methods precedence
				    (cdr contenders)
				    (if implied
					known-type
					(augment-type ntype known-type))
				    (if truth
					(append winners `(,method))
					winners)
				    known-types))))
			 (cond ((determined-to-be nil) (do-if nil t))
			       ((determined-to-be t)   (do-if t   t))
			       (t (funcall test-function position type 
					   (do-if t) (do-if nil))))))))))
      (do-column precedence methods ()))))

;;;
;;; These constants are the maximum number of case clauses for which
;;; to use the :ASSOC implementation of MCASE.  For larger numbers, a
;;; :HASH-TABLE is used.
;;;
(defconstant +eq-case-table-limit+ 15)
(defconstant +case-table-limit+ 10)

;;;
;;; Value is a list (EQ-P IMPL) for use as the last two parameters to
;;; MLOOKUP.  EQ-P true means all case clauses in CASE-LIST can be
;;; implemented using EQ.  IMPL is one of the symbols :ASSOC, :SIMPLE,
;;; :HASH-TABLE for the mcase implementation to use.
;;;
(defun compute-mcase-parameters (case-list)
  (unless (eq t (caar (last case-list)))
    (internal-error "The key for the last case arg to mcase was not T."))
  (let* ((eq-p (loop for case in case-list
		     always (or (eq (car case) t)
				(symbolp (caar case)))))
	 (limit (if eq-p +eq-case-table-limit+ +case-table-limit+))
	 (len (1- (length case-list)))
	 (type (cond ((= len 1)      :simple)
		     ((<= len limit) :assoc)
		     (t              :hash-table))))
    (list eq-p type)))

;;;
;;; Value is a form for looking up KEY in INFO, returning DEFAULT if
;;; not found.  EQ-P true means compare keys with EQ.  TYPE is a
;;; symbol describing the type of lookup to use, one of :SIMPLE,
;;; :ASSOC, or :HASH-TABLE.
;;;
(defmacro mlookup (key info default eq-p type)
  (ecase type
    (:simple
     `(if (,(if eq-p 'eq 'eql) ,key (car ,info))
          (cdr ,info)
          ,default))
    (:assoc
     `(dolist (e ,info ,default)
        (when (,(if eq-p 'eq 'eql) (car e) ,key)
	  (return (cdr e)))))
    (:hash-table
     `(gethash ,key ,info ,default))))

;;;
;;; The following three functions are used for the GET-FUNCTION
;;; mechanism, when used on lambdas containing discriminating nets.
;;;
(defun net-test-converter (form)
  (if (atom form)
      (default-test-converter form)
      (case (car form)
	((invoke-effective-method-function invoke-fast-method-call)
	 '.call.)
	(methods
	 '.methods.)
	(unordered-methods
	 '.umethods.)
	(mcase
	 `(mlookup ,(cadr form) nil nil ,@(compute-mcase-parameters (cddr form))))
	(t (default-test-converter form)))))

(defun net-code-converter (form)
  (if (atom form)
      (default-code-converter form)
      (case (car form)
	((methods unordered-methods)
	 (let ((gensym (gensym)))
	   (values gensym (list gensym))))
	(mcase
	 (let ((mp (compute-mcase-parameters (cddr form)))
	       (gensym (gensym)) (default (gensym)))
	   (values `(mlookup ,(cadr form) ,gensym ,default ,@mp)
		   (list gensym default))))
	(t
	 (default-code-converter form)))))

(defun net-constant-converter (form gf)
  (flet ((methods-converter (form gf)
	   (case (car-safe form)
	     (methods
	      (cons '.methods.
		    (get-effective-method-function1 gf (cadr form))))
	     (unordered-methods
	      (default-secondary-dispatch-function gf)))))
    (or (let ((c (methods-converter form gf)))
	  (when c (list c)))
	(if (atom form)
	    (default-constant-converter form)
	    (case (car form)
	      (mcase
	       (let* ((mp (compute-mcase-parameters (cddr form)))
		      (list (mapcar (lambda (clause)
				      (let ((key (car clause))
					    (meth (cadr clause)))
					(cons (if (consp key) (car key) key)
					      (methods-converter
					       meth gf))))
				    (cddr form)))
		      (default (car (last list))))
		 (list (list* :mcase mp (nbutlast list))
		       (cdr default))))
	      (t
	       (default-constant-converter form)))))))

;;;
;;;
;;;
(defun compute-secondary-dispatch-function (gf net &optional 
					    method-alist wrappers)
  (function-funcall (compute-secondary-dispatch-function1 gf net)
		    method-alist wrappers))

(defun compute-secondary-dispatch-function1 (gf net &optional function-p)
  (when (and (eq (car net) 'methods) (not function-p))
    (return-from compute-secondary-dispatch-function1
      (get-effective-method-function1 gf (cadr net))))
  ;;
  (let* ((name (generic-function-name gf))
	 (arg-info (gf-arg-info gf))
	 (metatypes (arg-info-metatypes arg-info))
	 (applyp (arg-info-applyp arg-info))
	 (fmc-arg-info (cons (length metatypes) applyp))
	 (arglist (if function-p
		      (make-dfun-lambda-list metatypes applyp)
		      (make-fast-method-call-lambda-list metatypes applyp))))
    (multiple-value-bind (cfunction constants)
	(get-function1 `(,(if function-p
			      'kernel:instance-lambda
			      'lambda)			   
			  ,arglist
			  ,@(unless function-p
				    `((declare (ignore .pv-cell.
						       .next-method-call.))))
			  (locally (declare #.*optimize-speed*)
			    (let ((emf ,net))
			      ,(make-emf-call metatypes applyp 'emf))))
		       #'net-test-converter
		       #'net-code-converter
		       (lambda (form)
			 (net-constant-converter form gf)))
      (lambda (method-alist wrappers)
	(let* ((alist (list nil))
	       (alist-tail alist))
	  (dolist (constant constants)
	    (let* ((a (or (dolist (a alist nil)
			    (when (eq (car a) constant)
			      (return a)))
			  (cons constant
				(or (convert-table
				     constant method-alist wrappers)
				    (convert-methods
				     constant method-alist wrappers)))))
		   (new (list a)))
	      (setf (cdr alist-tail) new)
	      (setf alist-tail new)))
	  (let ((function (apply cfunction (mapcar #'cdr (cdr alist)))))
	    (if function-p
		function
		(make-fast-method-call
		 :function (set-function-name function
					      `(discriminator ,name))
		 :arg-info fmc-arg-info))))))))

(defun convert-methods (constant method-alist wrappers)
  (if (eq (car-safe constant) '.methods.)
      (funcall (cdr constant) method-alist wrappers)
      constant))

(defun convert-table (constant method-alist wrappers)
  (when (eq (car-safe constant) :mcase)
    (let ((alist (mapcar (lambda (k+m)
			   (cons (car k+m)
				 (convert-methods (cdr k+m)
						  method-alist wrappers)))
			 (cddr constant)))
	  (mp (cadr constant)))
      (ecase (cadr mp)
	(:simple
	 (car alist))
	(:assoc
	 alist)
	(:hash-table
	 (let ((table (make-hash-table :test (if (car mp) 'eq 'eql))))
	   (dolist (k+m alist)
	     (setf (gethash (car k+m) table) (cdr k+m)))
	   table))))))


;;;
;;; The value returned by compute-discriminating-function is a function
;;; object.  It is called a discriminating function because it is called
;;; when the generic function is called and its role is to discriminate
;;; on the arguments to the generic function and then call appropriate
;;; method functions.
;;; 
;;; A discriminating function can only be called when it is installed as
;;; the funcallable instance function of the generic function for which
;;; it was computed.
;;;
;;; More precisely, if compute-discriminating-function is called with an
;;; argument <gf1>, and returns a result <df1>, that result must not be
;;; passed to apply or funcall directly.  Rather, <df1> must be stored as
;;; the funcallable instance function of the same generic function <gf1>
;;; (using set-funcallable-instance-function).  Then the generic function
;;; can be passed to funcall or apply.
;;;
;;; An important exception is that methods on this generic function are
;;; permitted to return a function which itself ends up calling the value
;;; returned by a more specific method.  This kind of `encapsulation' of
;;; discriminating function is critical to many uses of the MOP.
;;; 
;;; As an example, the following canonical case is legal:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     (let ((std (call-next-method)))
;;;       (lambda (arg)
;;;            (print (list 'call-to-gf gf arg))
;;;            (funcall std arg))))
;;;
;;; Because many discriminating functions would like to use a dynamic
;;; strategy in which the precise discriminating function changes with
;;; time it is important to specify how a discriminating function is
;;; permitted itself to change the funcallable instance function of the
;;; generic function.
;;;
;;; Discriminating functions may set the funcallable instance function
;;; of the generic function, but the new value must be generated by making
;;; a call to COMPUTE-DISCRIMINATING-FUNCTION.  This is to ensure that any
;;; more specific methods which may have encapsulated the discriminating
;;; function will get a chance to encapsulate the new, inner discriminating
;;; function.
;;;
;;; This implies that if a discriminating function wants to modify itself
;;; it should first store some information in the generic function proper,
;;; and then call compute-discriminating-function.  The appropriate method
;;; on compute-discriminating-function will see the information stored in
;;; the generic function and generate a discriminating function accordingly.
;;;
;;; The following is an example of a discriminating function which modifies
;;; itself in accordance with this protocol:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     (lambda (arg)
;;;         (cond (<some condition>
;;;                <store some info in the generic function>
;;;                (set-funcallable-instance-function
;;;                  gf
;;;                  (compute-discriminating-function gf))
;;;                (funcall gf arg))
;;;               (t
;;;                <call-a-method-of-gf>))))
;;;
;;; Whereas this code would not be legal:
;;;
;;;   (defmethod compute-discriminating-function ((gf my-generic-function))
;;;     (lambda (arg)
;;;         (cond (<some condition>
;;;                (set-funcallable-instance-function
;;;                  gf
;;;                  (lambda (a) ..))
;;;                (funcall gf arg))
;;;               (t
;;;                <call-a-method-of-gf>))))
;;;
;;; NOTE:  All the examples above assume that all instances of the class
;;;        my-generic-function accept only one argument.
;;;
;;;
;;;
;;;
(defun slot-value-using-class-dfun (class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-reader-function slotd) object))

(defun setf-slot-value-using-class-dfun (new-value class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-writer-function slotd) new-value object))

(defun slot-boundp-using-class-dfun (class object slotd)
  (declare (ignore class))
  (function-funcall (slot-definition-boundp-function slotd) object))

(defmethod compute-discriminating-function ((gf standard-generic-function))
  (with-slots (dfun-state arg-info) gf
    (typecase dfun-state
      (null (let ((name (generic-function-name gf)))
	      (when (eq name 'compute-applicable-methods)
		(update-all-c-a-m-gf-info gf))
	      (cond ((eq name 'slot-value-using-class)
		     (update-slot-value-gf-info gf 'reader)
		     #'slot-value-using-class-dfun)
		    ((equal name '(setf slot-value-using-class))
		     (update-slot-value-gf-info gf 'writer)
		     #'setf-slot-value-using-class-dfun)
		    ((eq name 'slot-boundp-using-class)
		     (update-slot-value-gf-info gf 'boundp)
		     #'slot-boundp-using-class-dfun)
		    ((gf-precompute-dfun-and-emf-p arg-info)
		     (make-final-dfun gf))
		    (t
		     (make-initial-dfun gf)))))
      (function dfun-state)
      (cons (car dfun-state)))))

(defmethod update-gf-dfun ((class std-class) gf)
  (let ((*new-class* class)
	(arg-info (gf-arg-info gf)))
    (when (gf-precompute-dfun-and-emf-p arg-info)
      (multiple-value-bind (dfun cache info)
	  (make-final-dfun-internal gf)
	(set-dfun gf dfun cache info)
	(update-dfun gf dfun cache info)))))

;;;
;;;
;;;
(defmethod function-keywords ((method standard-method))
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list (method-lambda-list method))
    (declare (ignore nreq nopt keysp restp))
    (values keywords allow-other-keys-p)))

(defmethod (setf class-name) :before (new-name (class class))
  (let ((kernel-class (kernel::find-class (class-name class))))
    (setf (kernel:%class-name kernel-class) new-name)))
