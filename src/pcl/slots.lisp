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
  "$Header: /project/cmucl/cvsroot/src/pcl/slots.lisp,v 1.28 2004/10/05 21:59:44 rtoy Exp $")
;;;

(in-package :pcl)

;;; ANSI CL condition for unbound slots.

(define-condition unbound-slot (cell-error)
  ((instance :reader unbound-slot-instance :initarg :instance))
  (:report (lambda (condition stream)
	     (format stream "~@<The slot ~S is unbound in the object ~S.~@:>"
		     (cell-error-name condition)
		     (unbound-slot-instance condition)))))

(defmethod wrapper-fetcher ((class standard-class))
  'std-instance-wrapper)

(defmethod slots-fetcher ((class standard-class))
  'std-instance-slots)

(defmethod raw-instance-allocator ((class standard-class))
  'allocate-standard-instance)

;;;
;;; These four functions work on std-instances and fsc-instances.  These are
;;; instances for which it is possible to change the wrapper and the slots.
;;;
;;; For these kinds of instances, most specified methods from the instance
;;; structure protocol are promoted to the implementation-specific class
;;; std-class.  Many of these methods call these four functions.
;;;

(defun swap-wrappers-and-slots (i1 i2)
  (with-pcl-lock
   (if (std-instance-p i1)
       (let ((w1 (std-instance-wrapper i1))
	     (s1 (std-instance-slots i1)))
	 (setf (std-instance-wrapper i1) (std-instance-wrapper i2))
	 (setf (std-instance-slots i1) (std-instance-slots i2))
	 (setf (std-instance-wrapper i2) w1)
	 (setf (std-instance-slots i2) s1))
       (progn
	 (assert (fsc-instance-p i1))
	 (let ((w1 (fsc-instance-wrapper i1))
	       (s1 (fsc-instance-slots i1)))
	   (setf (fsc-instance-wrapper i1) (fsc-instance-wrapper i2))
	   (setf (fsc-instance-slots i1) (fsc-instance-slots i2))
	   (setf (fsc-instance-wrapper i2) w1)
	   (setf (fsc-instance-slots i2) s1))))))


(defun find-slot-definition (class slot-name)
  (dolist (slot (class-slots class) nil)
    (when (eq slot-name (slot-definition-name slot))
      (return slot))))

(declaim (ftype (function (t symbol) t) slot-value))
(defun slot-value (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(values (slot-missing class object slot-name 'slot-value))
	(slot-value-using-class class object slot-definition))))

(declaim (ftype (function (t symbol t) t) set-slot-value))
(defun set-slot-value (object slot-name new-value)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'setf new-value)
	(setf (slot-value-using-class class object slot-definition) 
	      new-value))
    new-value))

(define-compiler-macro slot-value (&whole form object slot-name)
  (if (and (constantp slot-name)
	   (interned-symbol-p (eval slot-name)))
      `(accessor-slot-value ,object ,slot-name)
      form))

(define-compiler-macro set-slot-value (&whole form object slot-name value)
  (if (and (constantp slot-name)
	   (interned-symbol-p (eval slot-name)))
      `(accessor-set-slot-value ,object ,slot-name ,value)
      form))

(declaim (ftype (function (t symbol) boolean) slot-boundp))
(defun slot-boundp (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(not (not (slot-missing class object slot-name 'slot-boundp)))
	(slot-boundp-using-class class object slot-definition))))

(define-compiler-macro slot-boundp (&whole form object slot-name)
  (if (and (constantp slot-name)
	   (interned-symbol-p (eval slot-name)))
      `(accessor-slot-boundp ,object ,slot-name)
      form))

(declaim (ftype (function (t symbol) t) slot-makunbound))
(defun slot-makunbound (object slot-name)
  (let* ((class (class-of object))
         (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
        (slot-missing class object slot-name 'slot-makunbound)
        (slot-makunbound-using-class class object slot-definition))
    object))

(declaim (ftype (function (t symbol) boolean) slot-exists-p))
(defun slot-exists-p (object slot-name)
  (let ((class (class-of object)))
    (not (null (find-slot-definition class slot-name)))))

;;;
;;; This isn't documented, but is used within PCL in a number of print
;;; object methods (see named-object-print-function).
;;; 
(defun slot-value-or-default (object slot-name &optional (default "unbound"))
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))


;;;
;;; 
;;; 
(defun standard-instance-access (instance location)
  (%slot-ref (std-instance-slots instance) location))

(defun funcallable-standard-instance-access (instance location)
  (%slot-ref (fsc-instance-slots instance) location))

(defmethod slot-value-using-class ((class std-class)
                                   (object standard-object)
                                   (slotd standard-effective-slot-definition))
  (check-obsolete-instance object)
  (let* ((location (slot-definition-location slotd))
	 (value (typecase location
		  (fixnum 
		   (cond ((std-instance-p object)
			  (%slot-ref (std-instance-slots object) location))
			 ((fsc-instance-p object)
			  (%slot-ref (fsc-instance-slots object) location))
			 (t (internal-error "What kind of instance is this?"))))
		  (cons
		   (cdr location))
		  (t
		   (error "~@<The slot ~s has neither ~s nor ~s ~
                           allocation, so it can't be read by the default ~s ~
                           method.~@:>"
			  slotd :instance :class 'slot-value-using-class)))))
    (if (eq value +slot-unbound+)
	(values (slot-unbound class object (slot-definition-name slotd)))
	value)))

(defmethod (setf slot-value-using-class)
	   (new-value (class std-class)
		      (object standard-object)
		      (slotd standard-effective-slot-definition))
  (check-obsolete-instance object)
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum 
       (cond ((std-instance-p object)
	      (setf (%slot-ref (std-instance-slots object) location) new-value))
	     ((fsc-instance-p object)
	      (setf (%slot-ref (fsc-instance-slots object) location) new-value))
	     (t (internal-error "What kind of instance is this?"))))
      (cons
       (setf (cdr location) new-value))
      (t
       (error "~@<The slot ~s has neither ~s nor ~s allocation, ~
               so it can't be written by the default ~s method.~@:>"
	      slotd :instance :class '(setf slot-value-using-class))))))

(defmethod slot-boundp-using-class
	   ((class std-class) 
	    (object standard-object) 
	    (slotd standard-effective-slot-definition))
  (check-obsolete-instance object)
  (let* ((location (slot-definition-location slotd))
	 (value (typecase location
		  (fixnum 
		   (cond ((std-instance-p object)
			  (%slot-ref (std-instance-slots object) location))
			 ((fsc-instance-p object)
			  (%slot-ref (fsc-instance-slots object) location))
			 (t (internal-error "What kind of instance is this?"))))
		  (cons
		   (cdr location))
		  (t
		   (error "~@<The slot ~s has neither ~s nor ~s ~
                           allocation, so it can't be read by the default ~s ~
			   method.~@:>"
			  slotd :instance :class 'slot-boundp-using-class)))))
    (not (eq value +slot-unbound+))))

(defmethod slot-makunbound-using-class
	   ((class std-class)
	    (object standard-object) 
	    (slotd standard-effective-slot-definition))
  (check-obsolete-instance object)
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum 
       (cond ((std-instance-p object)
	      (setf (%slot-ref (std-instance-slots object) location) +slot-unbound+))
	     ((fsc-instance-p object)
	      (setf (%slot-ref (fsc-instance-slots object) location) +slot-unbound+))
	     (t (internal-error "What kind of instance is this?"))))
      (cons
       (setf (cdr location) +slot-unbound+))
      (t
       (error "~@<The slot ~s has neither ~s nor ~s allocation, ~
               so it can't be written by the default ~s method.~@:>"
	      slotd :instance :class 'slot-makunbound-using-class))))
  object)

(defmethod slot-value-using-class
    ((class structure-class)
     (object structure-object)
     (slotd structure-effective-slot-definition))
  (let ((function (slot-definition-internal-reader-function slotd)))
    (declare (type function function))
    (funcall function object)))

(defmethod (setf slot-value-using-class)
    (new-value (class structure-class)
	       (object structure-object)
	       (slotd structure-effective-slot-definition))
  (let ((function (slot-definition-internal-writer-function slotd)))
    (declare (type function function))
    (funcall function new-value object)))

(defmethod slot-boundp-using-class
	   ((class structure-class) 
	    (object structure-object)
	    (slotd structure-effective-slot-definition))
	   t)

(defmethod slot-makunbound-using-class
	   ((class structure-class)
	    (object structure-object)
	    (slotd structure-effective-slot-definition))
  (error "Structure slots cannot be unbound."))


(defmethod slot-value-using-class
    ((class condition-class)
     (object condition)
     (slotd condition-effective-slot-definition))
  (let ((fn (slot-definition-reader-function slotd)))
    (declare (type function fn))
    (funcall fn object)))

(defmethod (setf slot-value-using-class)
    (new-value
     (class condition-class)
     (object condition)
     (slotd condition-effective-slot-definition))
  (let ((fn (slot-definition-writer-function slotd)))
    (declare (type function fn))
    (funcall fn new-value object)))

(defmethod slot-boundp-using-class
    ((class condition-class)
     (object condition)
     (slotd condition-effective-slot-definition))
  (let ((fn (slot-definition-boundp-function slotd)))
    (declare (type function fn))
    (funcall fn object)))

(defmethod slot-makunbound-using-class ((class condition-class) object slot)
  (declare (ignore object slot))
  (error "Condition slots cannot be unbound."))


(defmethod slot-missing
	   ((class t) instance slot-name operation &optional new-value)
  (error
   (format nil "~~@<When attempting to ~A, the slot ~S is missing ~
                from the object ~S.~~@:>"
	   (ecase operation
	     (slot-value "read the slot's value (slot-value)")
	     (setf (format nil "set the slot's value to ~S (setf of slot-value)"
			   new-value))
	     (slot-boundp "test to see if slot is bound (slot-boundp)")
	     (slot-makunbound "make the slot unbound (slot-makunbound)"))
	   slot-name
	   instance)))

(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :name slot-name :instance instance))

(defun slot-unbound-internal (instance position)
  (values
   (slot-unbound (class-of instance) instance 
		 (etypecase position
		   (fixnum 
		    (nth position 
			 (wrapper-instance-slots-layout (wrapper-of instance))))
		   (cons
		    (car position))))))


(defmethod allocate-instance ((class standard-class) &rest initargs &key)
  (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (allocate-standard-instance (class-wrapper class)))

(defmethod allocate-instance ((class structure-class) &rest initargs &key)
  (declare (ignore initargs))
  (let ((constructor (class-defstruct-constructor class)))
    (if constructor
	(funcall constructor)
	(error "~@<Can't allocate an instance of class ~S.~@:>"
	       (class-name class)))))

(defmethod allocate-instance ((class condition-class) &rest initargs &key)
  (declare (ignore initargs))
  (make-condition (class-name class)))
