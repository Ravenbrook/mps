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

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/defclass.lisp,v 1.30 2004/04/06 20:44:03 rtoy Exp $")
;;;

(in-package :pcl)

;;;
;;; MAKE-TOP-LEVEL-FORM is used by all PCL macros that appear `at top-level'.
;;;
;;; The original motiviation for this function was to deal with the bug in
;;; the Genera compiler that prevents lambda expressions in top-level forms
;;; other than DEFUN from being compiled.
;;;
;;; Now this function is used to grab other functionality as well.  This
;;; includes:
;;;   - Preventing the grouping of top-level forms.  For example, a
;;;     DEFCLASS followed by a DEFMETHOD may not want to be grouped
;;;     into the same top-level form.
;;;   - Telling the programming environment what the pretty version
;;;     of the name of this form is.  This is used by WARN.
;;; 
(defun make-top-level-form (name times form)
  (flet ((definition-name ()
	   (if (and (listp name)
		    (memq (car name) '(defmethod defclass class method
				       method-combination)))
	       (format nil "~A~{ ~S~}" (car name) (cdr name))
	       (format nil "~S" name))))
    (definition-name)
    (if (or (member 'compile times)
	    (member :compile-toplevel times))
        `(eval-when ,times ,form)
        form)))



;;; 
;;; Like the DEFMETHOD macro, the expansion of the DEFCLASS macro is fixed.
;;; DEFCLASS always expands into a call to LOAD-DEFCLASS.  Until the meta-
;;; braid is set up, LOAD-DEFCLASS has a special definition which simply
;;; collects all class definitions up, when the metabraid is initialized it
;;; is done from those class definitions.
;;;
;;; After the metabraid has been setup, and the protocol for defining classes
;;; has been defined, the real definition of LOAD-DEFCLASS is installed by the
;;; file defclass.lisp
;;; 
(defmacro defclass (name direct-superclasses direct-slots
		    &rest options &environment env)
  (without-package-locks
   (expand-defclass name direct-superclasses direct-slots options env)))

;;; The following functions were moved here from slots-boot.lisp
;;; because expand-defclass now depends on them to generate ftype
;;; declarations that inform the compiler about the existence of 
;;; the relevant GFs at run-time, in order to squelch spurious
;;; warnings.

(defun slot-reader-name (slot-name)
  (list 'slot-accessor :global slot-name 'reader))

(defun slot-writer-name (slot-name)
  (list 'slot-accessor :global slot-name 'writer))
  
(defun slot-boundp-name (slot-name)
  (list 'slot-accessor :global slot-name 'boundp))

;;; The defclass expander proper

(defun expand-defclass (name supers slots options env)
  (declare (special *boot-state* *the-class-structure-class*))
  (setq slots (copy-tree slots)
	options (copy-tree options))
  ;;
  (when (eq *boot-state* 'complete)
    (dolist (super supers)
      (check-seals super 'expand-defclass)))
  ;;
  (let ((metaclass 'standard-class)
	(info-slots (copy-tree slots)))
    ;; TBD - ANSI compliant class and slot option checking.
    (dolist (option options)
      (if (not (listp option))
          (simple-program-error "~S is not a legal defclass option."
				option)
          (when (eq (car option) :metaclass)
            (unless (legal-class-name-p (cadr option))
              (simple-program-error
	       "~@<The value of the ~s option (~s) is not a legal ~
	        class name.~@:>"
	       :metaclass (cadr option)))
	    (setq metaclass (cadr option))
	    (setf options (remove option options))
	    (return t))))

    (let ((*initfunctions* ())
          (*readers* ())
          (*writers* ())
	  (*slots* ()))
      (declare (special *initfunctions* *readers* *writers* *slots*))
      (let ((canonical-slots
	      (mapcar (lambda (spec)
			(canonicalize-slot-specification name spec))
		      slots))
	    (other-initargs
	      (mapcar (lambda (option)
			(canonicalize-defclass-option name option))
		      options))
	    (defstruct-p
	     (and (eq *boot-state* 'complete)
		  (not (or (featurep :loadable-pcl)
			   (featurep :bootable-pcl)))
		  (let ((mclass (find-class metaclass nil)))
		    (and mclass
			 (*subtypep mclass 
				    *the-class-structure-class*))))))
        (let ((defclass-form 
	       (make-top-level-form
		`(defclass ,name)
		(if (define-class-at-compile-time-p env name supers)
		    '(:compile-toplevel :load-toplevel :execute)
		    '(:load-toplevel :execute))
		`(progn
		   ,@(mapcar (lambda (x)
			       `(declaim (ftype (function (t) t) ,x)))
			     *readers*)
		   ,@(mapcar (lambda (x)
			       `(declaim (ftype (function (t t) t) ,x)))
			     *writers*)
		   ,@(mapcar (lambda (x)
			       `(declaim (ftype (function (t) t)
						,(slot-reader-name x)
						,(slot-boundp-name x))
					 (ftype (function (t t) t)
						,(slot-writer-name x))))
			     *slots*)
		   (let ,(mapcar #'cdr *initfunctions*)
		     (load-defclass ',name
				    ',metaclass
				    ',supers
				    (list ,@canonical-slots)
				    (list 
				     :definition-source (c::source-location)
				     ,@(apply #'append 
					      (when defstruct-p
						'(:from-defclass-p t))
					      other-initargs))))))))
          (if defstruct-p
              (progn
                (eval defclass-form) ; define the class now, so that
                `(progn              ; the defstruct can be compiled.
                   ,(class-defstruct-form (find-class name))
                   ,defclass-form))
	      (progn
		(set-class-info name metaclass info-slots)
		(when (eq *boot-state* 'complete)
		  (inform-type-system-about-std-class name))
		defclass-form)))))))

(defun true (&rest ignore) (declare (ignore ignore)) t)
(defun false (&rest ignore) (declare (ignore ignore)) nil)
(defun zero (&rest ignore) (declare (ignore ignore)) 0)

(defun make-initfunction (initform)
  (declare (special *initfunctions*))
  (cond ((or (eq initform t)
	     (equal initform ''t))
	 '(function true))
	((or (eq initform nil)
	     (equal initform ''nil))
	 '(function false))
	((or (eql initform 0)
	     (equal initform ''0))
	 '(function zero))
	(t
	 (let ((entry (assoc initform *initfunctions* :test #'equal)))
	   (unless entry
	     (setq entry (list initform
			       (gensym)
			       `(function (lambda () ,initform))))
	     (push entry *initfunctions*))
	   (cadr entry)))))

(defun canonicalize-slot-specification (class-name spec)
  (declare (special *readers* *writers* *slots*))
  (cond ((and (symbolp spec)
	      (not (keywordp spec))
	      (not (memq spec '(t nil))))		   
	 (push spec *slots*)
	 `'(:name ,spec))
	((not (consp spec))
	 (error "~@<~S is not a legal slot specification.~@:>" spec))
	((null (cdr spec))
	 (push (car spec) *slots*)
	 `'(:name ,(car spec)))
	((null (cddr spec))
	 (error "~@<In the class definintion of ~s, the slot specification ~s ~
                 is obsolete.  Convert it to ~s.~@:>"
		class-name spec
		(list (car spec) :initform (cadr spec))))
	(t
	 (let* ((name (pop spec))
		(readers ())
		(writers ())
		(initargs ())
		(unsupplied (list nil))
		(initform (getf spec :initform unsupplied)))
	   (push name *slots*)
	   (doplist (key val) spec
	     (case key
	       (:accessor (push val readers)
			  (push `(setf ,val) writers))
	       (:reader   (push val readers))
	       (:writer   (push val writers))
	       (:initarg  (push val initargs))))
	   (loop (unless (remf spec :accessor) (return)))
	   (loop (unless (remf spec :reader)   (return)))
	   (loop (unless (remf spec :writer)   (return)))
	   (loop (unless (remf spec :initarg)  (return)))
           (setq *writers* (append writers *writers*))
           (setq *readers* (append readers *readers*))
	   (setq spec `(:name     ',name
			:readers  ',readers
			:writers  ',writers
			:initargs ',initargs
			',spec))
	   (if (eq initform unsupplied)
	       `(list* ,@spec)
	       `(list* :initfunction ,(make-initfunction initform) ,@spec))))))
						
(defun canonicalize-defclass-option (class-name option)  
  (declare (ignore class-name))
  (case (car option)
    (:default-initargs
      (let ((canonical ()))
	(let (key val (tail (cdr option)))
	  (loop (when (null tail) (return nil))
		(setq key (pop tail)
		      val (pop tail))
		(push ``(,',key ,,(make-initfunction val) ,',val) canonical))
	  `(':direct-default-initargs (list ,@(nreverse canonical))))))
    (:documentation
      `(',(car option) ',(cadr option)))
    (otherwise
     `(',(car option) ',(cdr option)))))


;;;
;;; This is the early definition of load-defclass.  It just collects up all
;;; the class definitions in a list.  Later, in the file braid1.lisp, these
;;; are actually defined.
;;;


;;;
;;; Each entry in *early-class-definitions* is an early-class-definition.
;;; 
;;;
(defparameter *early-class-definitions* ())

(defun early-class-definition (class-name)
  (or (find class-name *early-class-definitions* :key #'ecd-class-name)
      (error "~@<~S is not a class in *early-class-definitions*.~@:>"
	     class-name)))

(defun make-early-class-definition
       (name source metaclass
	superclass-names canonical-slots other-initargs)
  (list 'early-class-definition
	name source metaclass
	superclass-names canonical-slots other-initargs))
  
(defun ecd-class-name        (ecd) (nth 1 ecd))
(defun ecd-source            (ecd) (nth 2 ecd))
(defun ecd-metaclass         (ecd) (nth 3 ecd))
(defun ecd-superclass-names  (ecd) (nth 4 ecd))
(defun ecd-canonical-slots   (ecd) (nth 5 ecd))
(defun ecd-other-initargs    (ecd) (nth 6 ecd))

(defvar *early-class-slots* nil)

(defun canonical-slot-name (canonical-slot)
  (getf canonical-slot :name))

(defun early-class-slots (class-name)
  (cdr (or (assoc class-name *early-class-slots*)
	   (let ((a (cons class-name
			  (mapcar #'canonical-slot-name
				  (early-collect-inheritance class-name)))))
	     (push a *early-class-slots*)
	     a))))

(defun early-class-size (class-name)
  (length (early-class-slots class-name)))

(defun early-collect-inheritance (class-name)
  (let ((cpl (early-collect-cpl class-name)))
    (values (early-collect-slots cpl)
	    cpl
	    (early-collect-default-initargs cpl)
	    (loop for definition in *early-class-definitions*
		  when (memq class-name (ecd-superclass-names definition))
		    collect (ecd-class-name definition)))))

(defun early-collect-slots (cpl)
  (let* ((definitions (mapcar #'early-class-definition cpl))
	 (super-slots (mapcar #'ecd-canonical-slots definitions))
	 (slots (apply #'append (reverse super-slots))))
    (dolist (s1 slots)
      (let ((name1 (canonical-slot-name s1)))
	(dolist (s2 (cdr (memq s1 slots)))
	  (when (eq name1 (canonical-slot-name s2))
	    (error "~@<More than one early class defines a slot with the ~
                    name ~S.  This can't work because the bootstrap ~
                    object system doesn't know how to compute effective ~
                    slots.~@:>"
		   name1)))))
    slots))

(defun early-collect-cpl (class-name)
  (labels ((walk (c)
	     (let* ((definition (early-class-definition c))
		    (supers (ecd-superclass-names definition)))
	       (cons c
		     (apply #'append (mapcar #'early-collect-cpl supers))))))
    (remove-duplicates (walk class-name) :from-end nil :test #'eq)))

(defun early-collect-default-initargs (cpl)
  (let ((default-initargs ()))
    (dolist (class-name cpl)
      (let* ((definition (early-class-definition class-name))
	     (others (ecd-other-initargs definition)))
	(loop (when (null others) (return nil))
	      (let ((initarg (pop others)))
		(cond ((eq initarg :direct-default-initargs)
		       (setq default-initargs
			     (nconc default-initargs (reverse (pop others)))))
		      (t
		       (cerror "Discard it."
			       "~@<The defclass option ~S is not supported by ~
                                 the bootstrap object system.~@:>"
			       initarg)
		       (pop others)))))))
    (reverse default-initargs)))

(defun bootstrap-slot-index (class-name slot-name)
  (or (position slot-name (early-class-slots class-name))
      (internal-error "Slot ~S not found in class ~S" slot-name class-name)))

;;;
;;; bootstrap-get-slot and bootstrap-set-slot are used to access and change
;;; the values of slots during bootstrapping.  During bootstrapping, there
;;; are only two kinds of objects whose slots we need to access, CLASSes
;;; and SLOT-DEFINITIONs.  The first argument to these functions tells whether the
;;; object is a CLASS or a SLOT-DEFINITION.
;;;
;;; Note that the way this works it stores the slot in the same place in
;;; memory that the full object system will expect to find it later.  This
;;; is critical to the bootstrapping process, the whole changeover to the
;;; full object system is predicated on this.
;;;
;;; One important point is that the layout of standard classes and standard
;;; slots must be computed the same way in this file as it is by the full
;;; object system later.
;;; 
(defmacro bootstrap-get-slot (type object slot-name)
  `(slot-ref (get-slots ,object) (bootstrap-slot-index ,type ,slot-name)))

(defun bootstrap-set-slot (type object slot-name new-value)
  (setf (bootstrap-get-slot type object slot-name) new-value))

(defun early-class-name (class)
  (bootstrap-get-slot 'class class 'name))

(defun early-class-precedence-list (class)
  (bootstrap-get-slot 'pcl-class class 'class-precedence-list))

(defun early-class-name-of (instance)
  (early-class-name (class-of instance)))

(defun early-class-slotds (class)
  (bootstrap-get-slot 'slot-class class 'slots))

(defun early-slot-definition-name (slotd)
  (bootstrap-get-slot 'standard-effective-slot-definition slotd 'name))

(defun early-slot-definition-location (slotd)
  (bootstrap-get-slot 'standard-effective-slot-definition slotd 'location))

(defun early-accessor-method-slot-name (method)
  (bootstrap-get-slot 'standard-accessor-method method 'slot-name))

(unless (fboundp 'class-name-of)
  (setf (symbol-function 'class-name-of)
	(symbol-function 'early-class-name-of)))
  
(defun early-class-direct-subclasses (class)
  (bootstrap-get-slot 'class class 'direct-subclasses))

(declaim (notinline load-defclass))
(defun load-defclass (name metaclass supers canonical-slots canonical-options)
  (setq supers  (copy-tree supers)
	canonical-slots   (copy-tree canonical-slots)
	canonical-options (copy-tree canonical-options))
  (when (eq metaclass 'standard-class)
    (inform-type-system-about-std-class name))
  (let ((source (getf canonical-options :definition-source)))
    (remf canonical-options :definition-source)
    (let ((ecd
	   (make-early-class-definition name
					source
					metaclass
					supers
					canonical-slots
					canonical-options))
	  (existing
	   (find name *early-class-definitions* :key #'ecd-class-name)))
      (setq *early-class-definitions*
	    (cons ecd (remove existing *early-class-definitions*)))
      ecd)))

