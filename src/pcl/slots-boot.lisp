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
  "$Header: /project/cmucl/cvsroot/src/pcl/slots-boot.lisp,v 1.26 2005/01/27 14:45:58 rtoy Exp $")
;;;

(in-package :pcl)

;;;
;;; Called via LOAD-TIME-VALUE to make sure that the slot accessor
;;; function with name GF-NAME is a generic function, and has a
;;; method that will call SLOT-MISSING if appropriate.
;;;
;;; TYPE is the type of the accessor method, one of the symbols
;;; READER, WRITER, or BOUNDP.  SLOT-NAME is the name of the slot
;;; accessed.
;;;
(defun ensure-accessor (type gf-name slot-name)
  (labels ((slot-missing-fn (slot-name type)
	     (let* ((method-type (ecase type
				   (slot-value 'reader-method)
				   (setf 'writer-method)
				   (slot-boundp 'boundp-method)))
		    (initargs
		     (ecase type
		       (slot-value
			(make-method-function
			 (lambda (obj)
			   (values (slot-missing (class-of obj) obj slot-name
						 'slot-value)))))
		       (slot-boundp
			(make-method-function
			 (lambda (obj)
			   (not (not (slot-missing (class-of obj) obj slot-name
						   'slot-boundp))))))
		       (setf
			(make-method-function
			 (lambda (val obj)
			   (slot-missing (class-of obj) obj slot-name
					 'setf val)
			   val)))))
		    (initargs (copy-tree initargs)))
	       (setf (getf (getf initargs :plist) :slot-name-lists)
		     (list (list nil slot-name)))
	       (setf (getf (getf initargs :plist) :pv-table-symbol)
		     (gensym))
	       (list* :method-spec (list method-type 'slot-object slot-name)
		      initargs)))
	   (add-slot-missing-method (gf slot-name type)
	     (multiple-value-bind (class lambda-list specializers)
		 (ecase type
		   (slot-value
		    (values 'standard-reader-method
			    '(object)
			    (list *the-class-slot-object*)))
		   (slot-boundp
		    (values 'standard-boundp-method
			    '(object)
			    (list *the-class-slot-object*)))
		   (setf
		    (values 'standard-writer-method
			    '(new-value object)
			    (list *the-class-t* *the-class-slot-object*))))
	       (add-method gf (make-a-method class
					     ()
					     lambda-list
					     specializers
					     (slot-missing-fn slot-name type)
					     "generated slot-missing method"
					     slot-name)))))
    (unless (fboundp gf-name)
      (let ((gf (ensure-generic-function gf-name)))
	;;
	;; Initialize this here, In case the LOAD-TIME-VALUE is
	;; executed before INITIALIZE-INTERNAL-SLOT-FUNCTIONS had a
	;; chance to run.
	(slot-name->class-table slot-name)
	(setf (plist-value gf 'slot-missing-method) t)
	(ecase type
	  (reader (add-slot-missing-method gf slot-name 'slot-value))
	  (boundp (add-slot-missing-method gf slot-name 'slot-boundp))
	  (writer (add-slot-missing-method gf slot-name 'setf)))))
      t))

(defmacro accessor-slot-value (object slot-name)
  (assert (constantp slot-name))
  (let* ((slot-name (eval slot-name))
	 (reader-name (slot-reader-name slot-name)))
    `(let ((.ignore. (load-time-value (ensure-accessor 'reader ',reader-name
						       ',slot-name))))
       (declare (ignore .ignore.))
       (funcall #',reader-name ,object))))

(defmacro accessor-set-slot-value (object slot-name new-value &environment env)
  (assert (constantp slot-name))
  (setq object (macroexpand object env))
  (setq slot-name (macroexpand slot-name env))
  (let* ((slot-name (eval slot-name))
	 (bindings (unless (or (constantp new-value) (atom new-value))
		     (let ((object-var (gensym)))
		       (prog1 `((,object-var ,object))
			 (setq object object-var)))))
	 (writer-name (slot-writer-name slot-name))
	 (form `(let ((.ignore. (load-time-value
				 (ensure-accessor 'writer ',writer-name
						  ',slot-name)))
		      (.new-value. ,new-value))
		  (declare (ignore .ignore.))
		  (funcall #',writer-name .new-value. ,object)
		  .new-value.)))
    (if bindings
	`(let ,bindings ,form)
	form)))

(defmacro accessor-slot-boundp (object slot-name)
  (assert (constantp slot-name))
  (let* ((slot-name (eval slot-name))
	 (boundp-name (slot-boundp-name slot-name)))
    `(let ((.ignore. (load-time-value
		      (ensure-accessor 'boundp ',boundp-name ',slot-name))))
       (declare (ignore .ignore.))
       (funcall #',boundp-name ,object))))

(defun make-structure-slot-boundp-function (slotd)
  (declare (ignore slotd))
  (lambda (object) (declare (ignore object)) t))

(defun get-optimized-std-accessor-method-function (class slotd name)
  (cond ((structure-class-p class)
	 (ecase name
	   (reader (slot-definition-internal-reader-function slotd))
	   (writer (slot-definition-internal-writer-function slotd))
	   (boundp (make-structure-slot-boundp-function slotd))))
	((condition-class-p class)
	 (ecase name
	   (reader (slot-definition-reader-function slotd))
	   (writer (slot-definition-writer-function slotd))
	   (boundp (slot-definition-boundp-function slotd))))
	(t
	 (let* ((fsc-p (cond ((standard-class-p class) nil)
			     ((funcallable-standard-class-p class) t)
			     (t (error "~@<~S is not a standard-class.~@:>"
				       class))))
		(slot-name (slot-definition-name slotd))
		(index (slot-definition-location slotd))
		(function (ecase name
			    (reader #'make-optimized-std-reader-method-function)
			    (writer #'make-optimized-std-writer-method-function)
			    (boundp #'make-optimized-std-boundp-method-function)))
		(value (funcall function fsc-p slot-name index)))
	   (declare (type function function))
	   (values value index)))))

(defun make-optimized-std-reader-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (if fsc-p
		 (lambda (instance)
		   (check-obsolete-instance instance)
		   (let ((value (%slot-ref (fsc-instance-slots instance) index)))
		     (if (eq value +slot-unbound+)
			 (values (slot-unbound (class-of instance) instance slot-name))
			 value)))
		 (lambda (instance)
		   (check-obsolete-instance instance)
		   (let ((value (%slot-ref (std-instance-slots instance) index)))
		     (if (eq value +slot-unbound+)
			 (values (slot-unbound (class-of instance) instance slot-name))
			 value)))))
     (cons   (lambda (instance)
	       (check-obsolete-instance instance)
	       (let ((value (cdr index)))
		 (if (eq value +slot-unbound+)
		     (values (slot-unbound (class-of instance) instance slot-name))
		     value))))
     (null   (lambda (instance)
	       (check-obsolete-instance instance)
	       (error "~@<Slot ~S in class ~S ~
                       does not have standard allocation.~@:>"
		      slot-name (class-of instance)))))
   `(reader ,slot-name)))

(defun make-optimized-std-writer-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (if fsc-p
		 (lambda (nv instance)
		   (check-obsolete-instance instance)
		   (setf (%slot-ref (fsc-instance-slots instance) index) nv))
		 (lambda (nv instance)
		   (check-obsolete-instance instance)
		   (setf (%slot-ref (std-instance-slots instance) index) nv))))
     (cons   (lambda (nv instance)
	       (check-obsolete-instance instance)
	       (setf (cdr index) nv)))
     (null   (lambda (instance)
	       (check-obsolete-instance instance)
	       (error "~@<Slot ~S in class ~S ~
                       does not have standard allocation.~@:>"
		      slot-name (class-of instance)))))
   `(writer ,slot-name)))

(defun make-optimized-std-boundp-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (if fsc-p
		 (lambda (instance)
		   (check-obsolete-instance instance)
		   (not (eq (%slot-ref (fsc-instance-slots instance) index)
			    +slot-unbound+)))
		 (lambda (instance)
		   (check-obsolete-instance instance)
		   (not (eq (%slot-ref (std-instance-slots instance) index)
			    +slot-unbound+)))))
     (cons   (lambda (instance)
	       (check-obsolete-instance instance)
	       (not (eq (cdr index) +slot-unbound+))))
     (null   (lambda (instance)
	       (check-obsolete-instance instance)
	       (error "~@<Slot ~S in class ~S ~
                       does not have standard allocation.~@:>"
		      slot-name (class-of instance)))))
   `(boundp ,slot-name)))

(defun make-optimized-structure-slot-value-using-class-method-function (function)
  (declare (type function function))
  (lambda (class object slotd)
    (declare (ignore class slotd))
    (funcall function object)))

(defun make-optimized-structure-setf-slot-value-using-class-method-function (function)
  (declare (type function function))
  (lambda (nv class object slotd)
    (declare (ignore class slotd))
    (funcall function nv object)))

(defun make-optimized-structure-slot-boundp-using-class-method-function ()
  (lambda (class object slotd)
    (declare (ignore class object slotd))
    t))

(defun get-optimized-std-slot-value-using-class-method-function (class slotd name)
  (cond ((structure-class-p class)
	 (ecase name
	   (reader
	    (make-optimized-structure-slot-value-using-class-method-function
	     (slot-definition-internal-reader-function slotd)))
	   (writer
	    (make-optimized-structure-setf-slot-value-using-class-method-function
	     (slot-definition-internal-writer-function slotd)))
	   (boundp
	    (make-optimized-structure-slot-boundp-using-class-method-function))))
	((condition-class-p class)
	 (ecase name
	   (reader
	    (let ((function (slot-definition-reader-function slotd)))
	      (lambda (class object slotd)
		(declare (ignore class slotd))
		(funcall function object))))
	   (writer
	    (let ((function (slot-definition-writer-function slotd)))
	      (lambda (nv class object slotd)
		(declare (ignore class slotd))
		(funcall function nv object))))
	   (boundp
	    (let ((function (slot-definition-boundp-function slotd)))
	      (lambda (class object slotd)
		(declare (ignore class slotd))
		(funcall function object))))))
	(t
	 (let* ((fsc-p (cond ((standard-class-p class) nil)
			     ((funcallable-standard-class-p class) t)
			     (t (error "~@<~S is not a standard-class.~@:>" class))))
		(slot-name (slot-definition-name slotd))
		(index (slot-definition-location slotd))
		(function 
		 (ecase name
		   (reader 
		    #'make-optimized-std-slot-value-using-class-method-function)
		   (writer 
		    #'make-optimized-std-setf-slot-value-using-class-method-function)
		   (boundp 
		    #'make-optimized-std-slot-boundp-using-class-method-function))))
	   (declare (type function function))
	   (values (funcall function fsc-p slot-name index) index)))))

(defun make-optimized-std-slot-value-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (etypecase index
    (fixnum (if fsc-p
		(lambda (class instance slotd)
		  (declare (ignore slotd))
		  (check-obsolete-instance instance)
		  (let ((value (%slot-ref (fsc-instance-slots instance) index)))
		    (if (eq value +slot-unbound+)
			(values (slot-unbound class instance slot-name))
			value)))
		(lambda (class instance slotd)
		  (declare (ignore slotd))
		  (check-obsolete-instance instance)
		  (let ((value (%slot-ref (std-instance-slots instance) index)))
		    (if (eq value +slot-unbound+)
			(values (slot-unbound class instance slot-name))
			value)))))
    (cons   (lambda (class instance slotd)
	      (declare (ignore slotd))
	      (check-obsolete-instance instance)
	      (let ((value (cdr index)))
		(if (eq value +slot-unbound+)
		    (values (slot-unbound class instance slot-name))
		    value))))
    (null   (lambda (class instance slotd)
	      (declare (ignore slotd))
	      (check-obsolete-instance instance)
	      (error "~@<Slot ~S in class ~S ~
                      does not have standard allocation.~@:>"
		     slot-name class)))))

(defun make-optimized-std-setf-slot-value-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (declare (ignore slot-name))
  (etypecase index
    (fixnum (if fsc-p
		(lambda (nv class instance slotd)
		  (declare (ignore class slotd))
		  (check-obsolete-instance instance)
		  (setf (%slot-ref (fsc-instance-slots instance) index) nv))
		(lambda (nv class instance slotd)
		  (declare (ignore class slotd))
		  (check-obsolete-instance instance)
		  (setf (%slot-ref (std-instance-slots instance) index) nv))))
    (cons   (lambda (nv class instance slotd)
	      (declare (ignore class slotd))
	      (check-obsolete-instance instance)
	      (setf (cdr index) nv)))
    (null   (lambda (class instance slotd)
	      (declare (ignore slotd))
	      (check-obsolete-instance instance)
	      (error "~@<Slot ~S in class ~S ~
                      does not have standard allocation.~@:>"
		     slot-name class)))))

(defun make-optimized-std-slot-boundp-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (declare (ignore slot-name))
  (etypecase index
    (fixnum (if fsc-p
		(lambda (class instance slotd)
		  (declare (ignore class slotd))
		  (check-obsolete-instance instance)
		  (not (eq (%slot-ref (fsc-instance-slots instance) index)
			   +slot-unbound+)))
		(lambda (class instance slotd)
		  (declare (ignore class slotd))
		  (check-obsolete-instance instance)
		  (not (eq (%slot-ref (std-instance-slots instance) index)
			   +slot-unbound+)))))
    (cons   (lambda (class instance slotd)
	      (declare (ignore class slotd))
	      (check-obsolete-instance instance)
	      (not (eq (cdr index) +slot-unbound+))))
    (null   (lambda (class instance slotd)
	      (declare (ignore slotd))
	      (check-obsolete-instance instance)
	      (error "~@<Slot ~S in class ~S ~
                      does not have standard allocation.~@:>"
		     slot-name class)))))

(defun make-internal-reader-method-function (class-name slot-name)
  (list* :method-spec `(internal-reader-method ,class-name ,slot-name)
	 (make-method-function
	  (lambda (instance)
	    (let ((wrapper (get-instance-wrapper-or-nil instance)))
	      (if wrapper
		  (let* ((class (wrapper-class* wrapper))
			 (index (or (instance-slot-index wrapper slot-name)
				    (assq slot-name (wrapper-class-slots wrapper)))))
		    (typecase index
		      (fixnum 	
		       (let ((value (%slot-ref (get-slots instance) index)))
			 (if (eq value +slot-unbound+)
			     (values (slot-unbound (class-of instance) instance slot-name))
			     value)))
		      (cons
		       (let ((value (cdr index)))
			 (if (eq value +slot-unbound+)
			     (values (slot-unbound (class-of instance) instance slot-name))
			     value)))
		      (t
		       (error "~@<The wrapper for class ~S does not have ~
                               the slot ~S.~@:>"
			      class slot-name))))
		  (slot-value instance slot-name)))))))


(defun make-std-reader-method-function (class-name slot-name)
  (let* ((pv-table-symbol (gensym))
	 (initargs (copy-tree
		    (make-method-function
		     (lambda (instance)
		       (pv-binding1 (.pv. .calls.
					  (symbol-value pv-table-symbol)
					  (instance) (.slots.))
			 (pv-slot-value
			  class-name slot-name .slots. 0 t nil
			  (slot-value instance slot-name))))))))
    (setf (getf (getf initargs :plist) :slot-name-lists)
	  (list (list nil slot-name)))
    (setf (getf (getf initargs :plist) :pv-table-symbol) pv-table-symbol)
    (list* :method-spec `(reader-method ,class-name ,slot-name)
	   initargs)))

(defun make-std-writer-method-function (class-name slot-name)
  (let* ((pv-table-symbol (gensym))
	 (initargs (copy-tree
		    (make-method-function
		     (lambda (nv instance)
		       (pv-binding1 (.pv. .calls.
					  (symbol-value pv-table-symbol)
					  (instance) (.slots.))
			 (pv-set-slot-value
			  class-name slot-name .slots. 0 nv nil
			  (setf (slot-value instance slot-name) nv))))))))
    (setf (getf (getf initargs :plist) :slot-name-lists)
	  (list nil (list nil slot-name)))
    (setf (getf (getf initargs :plist) :pv-table-symbol) pv-table-symbol)
    (list* :method-spec `(writer-method ,class-name ,slot-name)
	   initargs)))

(defun make-std-boundp-method-function (class-name slot-name)
  (let* ((pv-table-symbol (gensym))
	 (initargs (copy-tree
		    (make-method-function
		     (lambda (instance)
		       (pv-binding1 (.pv. .calls.
					  (symbol-value pv-table-symbol)
					  (instance) (.slots.))
			  (pv-slot-boundp
			   class-name slot-name .slots. 0
			   (slot-boundp instance slot-name))))))))
    (setf (getf (getf initargs :plist) :slot-name-lists)
	  (list (list nil slot-name)))
    (setf (getf (getf initargs :plist) :pv-table-symbol) pv-table-symbol)
    (list* :method-spec `(boundp-method ,class-name ,slot-name)
	   initargs)))

(defun initialize-internal-slot-gfs (slot-name &optional type)
  (when (or (null type) (eq type 'reader))
    (let* ((name (slot-reader-name slot-name))
	   (gf (ensure-generic-function name))
	   (methods (generic-function-methods gf)))
      (when (or (null methods)
		(plist-value gf 'slot-missing-method))
	(setf (plist-value gf 'slot-missing-method) nil)
	(add-reader-method *the-class-slot-object* gf slot-name))))
  (when (or (null type) (eq type 'writer))
    (let* ((name (slot-writer-name slot-name))
	   (gf (ensure-generic-function name))
	   (methods (generic-function-methods gf)))
      (when (or (null methods)
		(plist-value gf 'slot-missing-method))
	(setf (plist-value gf 'slot-missing-method) nil)
	(add-writer-method *the-class-slot-object* gf slot-name))))
  (when (or (null type) (eq type 'boundp))
    (let* ((name (slot-boundp-name slot-name))
	   (gf (ensure-generic-function name))
	   (methods (generic-function-methods gf)))
      (when (or (null methods)
		(plist-value gf 'slot-missing-method))
	(setf (plist-value gf 'slot-missing-method) nil)
	(add-boundp-method *the-class-slot-object* gf slot-name))))
  nil)


