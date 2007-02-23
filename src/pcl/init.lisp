;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
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
  "$Header: /project/cmucl/cvsroot/src/pcl/init.lisp,v 1.24 2003/05/25 14:33:49 gerd Exp $")

;;;
;;; This file defines the initialization and related protocols.
;;; 

(in-package :pcl)

(defmethod make-instance ((class symbol) &rest initargs &key)
  (apply #'make-instance (find-class class) initargs))

(defmethod make-instance ((class class) &rest initargs &key)
  ;;
  ;; Try to use an optimized constructor if there is one.
  (let ((instance (call-ctor class initargs)))
    (when instance
      (return-from make-instance instance)))
  ;;
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((class-default-initargs (class-default-initargs class)))
    (when class-default-initargs
      (setf initargs (default-initargs class initargs class-default-initargs)))
    (when initargs
      (when (and (eq *boot-state* 'complete)
	         (not (getf initargs :allow-other-keys)))
        (let ((class-proto (class-prototype class)))
          (check-initargs
	   class initargs
	   (append (compute-applicable-methods
		    #'allocate-instance (list class))
		   (compute-applicable-methods 
		    #'initialize-instance (list class-proto))
		   (compute-applicable-methods 
		    #'shared-initialize (list class-proto t)))))))
    (let ((instance (apply #'allocate-instance class initargs)))
      (apply #'initialize-instance instance initargs)
      instance)))

(defmethod default-initargs ((class slot-class) supplied-initargs
			     class-default-initargs)
  (loop for (key fn) in class-default-initargs
	when (eq (getf supplied-initargs key '.not-there.) '.not-there.)
	  append (list key (funcall fn)) into default-initargs
	finally
	  (return (append supplied-initargs default-initargs))))

(defmethod initialize-instance ((instance slot-object) &rest initargs &key)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance slot-object) &rest initargs &key)
  (check-ri-initargs instance initargs)
  (apply #'shared-initialize instance nil initargs)
  instance)

(defmethod update-instance-for-different-class
    ((previous standard-object) (current standard-object) &rest initargs &key)
  ;; First we must compute the newly added slots.  The spec defines
  ;; newly added slots as "those local slots for which no slot of
  ;; the same name exists in the previous class."
  (let ((added-slots '())
	(current-slotds (class-slots (class-of current)))
	(previous-slot-names (mapcar #'slot-definition-name
				     (class-slots (class-of previous)))))
    (dolist (slotd current-slotds)
      (if (and (not (memq (slot-definition-name slotd) previous-slot-names))
	       (eq (slot-definition-allocation slotd) :instance))
	  (push (slot-definition-name slotd) added-slots)))
    (check-initargs
     (class-of current) initargs
     (list (list* 'update-instance-for-different-class previous current initargs)
	   (list* 'shared-initialize current added-slots initargs)))
    (apply #'shared-initialize current added-slots initargs)))

(defmethod update-instance-for-redefined-class
    ((instance standard-object)
     added-slots
     discarded-slots
     property-list
     &rest initargs &key)
  (check-initargs
   (class-of instance) initargs
   (list (list* 'update-instance-for-redefined-class
		instance added-slots discarded-slots property-list initargs)
	 (list* 'shared-initialize instance added-slots initargs)))
  (apply #'shared-initialize instance added-slots initargs))

;;;
;;; initialize the instance's slots in a two step process
;;;   (1) A slot for which one of the initargs in initargs can set
;;;       the slot, should be set by that initarg.  If more than
;;;       one initarg in initargs can set the slot, the leftmost
;;;       one should set it.
;;;
;;;   (2) Any slot not set by step 1, may be set from its initform
;;;       by step 2.  Only those slots specified by the slot-names
;;;       argument are set.  If slot-names is:
;;;       T
;;;            any slot not set in step 1 is set from its
;;;            initform
;;;       <list of slot names>
;;;            any slot in the list, and not set in step 1
;;;            is set from its initform
;;;
;;;       ()
;;;            no slots are set from initforms
;;;

(defmethod shared-initialize ((instance slot-object) slot-names
			      &rest initargs &key)
  (let ((structure-p (structure-class-p (class-of instance))))
    (flet ((initialize-slot-from-initarg (class instance slotd)
	     (let ((slot-initargs (slot-definition-initargs slotd)))
	       (doplist (initarg value) initargs
			(when (memq initarg slot-initargs)
			  (setf (slot-value-using-class class instance slotd)
				value)
			  (return t)))))
	   (initialize-slot-from-initfunction (class instance slotd)
	     ;; CLHS: If a before method stores something in a slot,
	     ;; that slot won't be initialized from its :INITFORM, if any.
	     (if structure-p
		 (when (eq '.unbound.
			   (slot-value-using-class class instance slotd))
		   (let ((initfn (slot-definition-initfunction slotd)))
		     (setf (slot-value-using-class class instance slotd)
			   (if (null initfn)
			       nil
			       (funcall initfn)))))
		 (unless (slot-boundp-using-class class instance slotd)
		   (let ((initfn (slot-definition-initfunction slotd)))
		     (unless (null initfn)
		       (setf (slot-value-using-class class instance slotd)
			     (funcall initfn))))))))
      (let ((class (class-of instance)))
	(dolist (slotd (class-slots class))
	  (unless (initialize-slot-from-initarg class instance slotd)
	    (when (or (eq slot-names t)
		      (memq (slot-definition-name slotd) slot-names))
	      (initialize-slot-from-initfunction class instance slotd)))))
      instance)))


;;;
;;; if initargs are valid return nil, otherwise signal an error
;;; If ERROR-P is false, return a list of invalid initkeys.
;;;
(defun check-initargs (class initargs call-list
		       &optional (plist-p t) (error-p t))
  (multiple-value-bind (legal allow-other-keys)
      (valid-initargs class call-list)
    (unless allow-other-keys
      (let ((invalid-keys ()))
	(if plist-p
	    (unless (getf initargs :allow-other-keys)
	      (doplist (key val) initargs
	        (unless (or (memq key legal)
			    ;; :ALLOW-OTHER-KEYS NIL
			    (eq key :allow-other-keys))
		  (push key invalid-keys))))
	    (unless (memq :allow-other-keys initargs)
	      (dolist (key initargs)
		(unless (memq key legal)
		  (push key invalid-keys)))))
	(when (and invalid-keys error-p)
	  (invalid-initargs-error class invalid-keys))
	invalid-keys))))

(defun valid-initargs (class call-list)
  (let ((methods (mapcan (lambda (call)
			   (if (consp call)
			       (copy-list (compute-applicable-methods
					   (gdefinition (car call))
					   (cdr call)))
			       (list call)))
			 call-list))
	(legal (apply #'append (mapcar #'slot-definition-initargs
				       (class-slots class)))))
    ;; Add to the set of slot-filling initargs the set of
    ;; initargs that are accepted by the methods.  If at
    ;; any point we come across &allow-other-keys, we can
    ;; just quit.
    (dolist (method methods)
      (multiple-value-bind (nreq nopt keysp restp allow-other-keys keys)
	  (analyze-lambda-list (method-lambda-list* method))
	(declare (ignore nreq nopt keysp restp))
	(when allow-other-keys
	  (return-from valid-initargs (values nil t)))
	(setq legal (append keys legal))))
    (values legal nil)))

(defun invalid-initargs-error (class invalid-keys)
  (simple-program-error "~@<Invalid initialization argument~P ~2I~_~
                         ~<~{~S~^, ~}~@:> ~I~_in call for class ~S.~:>"
			(length invalid-keys)
			(list invalid-keys)
			class))
