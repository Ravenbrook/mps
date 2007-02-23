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
;;;

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/env.lisp,v 1.26 2003/06/18 09:23:09 gerd Exp $")
;;;
;;; Basic environmental stuff.
;;;

(in-package :pcl)

;;;
;;;
;;;

;;; ANSI compliance wants default structure printer to use #S(...) format.
(defmethod print-object ((object structure-object) stream)
  (lisp::default-structure-print object stream 0))

;;; Condition printing
(defmethod print-object ((object condition) stream)
  (conditions::real-print-condition object stream))

(defgeneric describe-object (object stream))

(defmethod describe-object (object stream)
  (describe object stream))

(defmethod describe-object ((object slot-object) stream)
  (let* ((class (class-of object))
	 (slotds (slots-to-inspect class object))
	 (max-slot-name-length 0)
	 (instance-slotds ())
	 (class-slotds ())
	 (other-slotds ()))
    (flet ((adjust-slot-name-length (name)
	     (setq max-slot-name-length
		   (max max-slot-name-length
			(length (the string (symbol-name name))))))
	   (describe-slot (name value &optional (allocation () alloc-p))
	     (if alloc-p
		 (format stream
			 "~% ~A ~S ~VT  ~S"
			 name allocation (+ max-slot-name-length 7) value)
		 (format stream
			 "~% ~A~VT  ~S"
			 name max-slot-name-length value))))
      ;; Figure out a good width for the slot-name column.
      (dolist (slotd slotds)
	(adjust-slot-name-length (slot-definition-name slotd))
	(case (slot-definition-allocation slotd)
	  (:instance (push slotd instance-slotds))
	  (:class    (push slotd class-slotds))
	  (otherwise (push slotd other-slotds))))
      (setq max-slot-name-length  (min (+ max-slot-name-length 3) 30))
      (format stream "~%~S is an instance of class ~S:" object class)

      (when instance-slotds
	(format stream "~% The following slots have :INSTANCE allocation:")
	(dolist (slotd (nreverse instance-slotds))
	  (describe-slot (slot-definition-name slotd)
			 (slot-value-or-default object (slot-definition-name slotd)))))

      (when class-slotds
	(format stream "~% The following slots have :CLASS allocation:")
	(dolist (slotd (nreverse class-slotds))
	  (describe-slot (slot-definition-name slotd)
			 (slot-value-or-default object (slot-definition-name slotd)))))

      (when other-slotds 
	(format stream "~% The following slots have allocation as shown:")
	(dolist (slotd (nreverse other-slotds))
	  (describe-slot (slot-definition-name slotd)
			 (slot-value-or-default object (slot-definition-name slotd))
			 (slot-definition-allocation slotd))))
      (values))))

(defmethod slots-to-inspect ((class slot-class) (object slot-object))
  (class-slots class))

(defvar *describe-metaobjects-as-objects-p* nil)

(defun method-specialized-lambda-list (method)
  (loop with specializers = (unparse-specializers method)
	for elt in (method-lambda-list method)
	collect (if specializers
		    (list elt (pop specializers))
		    elt)))

(defmethod describe-object ((gf standard-generic-function) stream)
  (format stream "~A is a generic function.~%" gf)
  (let* ((gf-name (generic-function-name gf))
	 (doc (documentation gf-name 'function)))
    (format stream "Its lambda-list is:~%  ~S~%"
	    (generic-function-lambda-list gf))
    (when doc
      (format stream "Generic function documentation:~%  ~s~%" doc))
    (format stream "Its methods are:~%")
    (loop for method in (generic-function-methods gf) and i from 1
	  as doc = (plist-value method 'documentation) do
	    (format stream "  ~d: ~a ~@[~{~s ~}~]~:s~%"
		    i gf-name (method-qualifiers method)
		    (method-specialized-lambda-list method))
	    (when doc
	      (format stream "    Method documentation: ~s~%" doc)))
    (when *describe-metaobjects-as-objects-p*
      (call-next-method))))

;;;
;;;
;;;
(defmethod describe-object ((class class) stream)
  (flet ((pretty-class (c) (or (class-name c) c)))
    (macrolet ((ft (string &rest args) `(format stream ,string ,@args)))
      (ft "~&~@<~S is a class, it is an instance of ~S.~@:>~%"
	  class (pretty-class (class-of class)))
      (let ((name (class-name class)))
	(if name
	    (if (eq class (find-class name nil))
		(ft "Its proper name is ~S.~%" name)
		(ft "Its name is ~S, but this is not a proper name.~%" name))
	    (ft "It has no name (the name is NIL).~%")))
      (ft "The direct superclasses are: ~:S, and the direct~%~
           subclasses are: ~:S.  The class is ~:[not ~;~]finalized.  ~
           The class precedence list is:~%~S~%~
           There are ~D methods specialized for this class."
	  (mapcar #'pretty-class (class-direct-superclasses class))
	  (mapcar #'pretty-class (class-direct-subclasses class))
	  (class-finalized-p class)
	  (mapcar #'pretty-class (cpl-or-nil class))
	  (length (specializer-direct-methods class)))
      (unless (typep class 'condition-class)
	(loop initially (ft "~&Its direct slots are:~%")
	      for slotd in (class-direct-slots class)
	      as name = (slot-definition-name slotd)
	      as doc = (slot-value slotd 'documentation) do
		(ft "  ~a, documentation ~s~%" name doc)))))
  (when *describe-metaobjects-as-objects-p*
    (call-next-method)))

(defun describe-package (object stream)
  (unless (packagep object) (setq object (find-package object)))
  (format stream "~&~S is a ~S.~%" object (type-of object))
  (let ((nick (package-nicknames object)))
    (when nick
      (format stream "You can also call it~@[ ~{~S~^, ~} or~] ~S.~%"
	      (butlast nick) (first (last nick)))))  
  (let* ((internal (lisp::package-internal-symbols object))
	 (internal-count (- (lisp::package-hashtable-size internal)
				  (lisp::package-hashtable-free internal)))
	 (external (lisp::package-external-symbols object))
	 (external-count (- (lisp::package-hashtable-size external)
				  (lisp::package-hashtable-free external))))
    (format stream "It has ~D internal and ~D external symbols (~D total).~%"
	    internal-count external-count (+ internal-count external-count)))
  (let ((used (package-use-list object)))
    (when used
      (format stream "It uses the packages ~{~S~^, ~}.~%"
	      (mapcar #'package-name used))))
  (let ((users (package-used-by-list object)))
    (when users
      (format stream "It is used by the packages ~{~S~^, ~}.~%"
	      (mapcar #'package-name users)))))

(defmethod describe-object ((object package) stream)
  (describe-package object stream))

(defmethod describe-object ((object hash-table) stream)
  (format stream "~&~S is an ~a hash table."
	  object
	  (lisp::hash-table-test object))
  (format stream "~&Its size is ~d buckets."
	  (lisp::hash-table-size object))
  (format stream "~&Its rehash-size is ~d."
	  (lisp::hash-table-rehash-size object))
  (format stream "~&Its rehash-threshold is ~d."
	  (hash-table-rehash-threshold object))
  (format stream "~&It currently holds ~d entries."
	  (lisp::hash-table-number-entries object)))



;;;
;;; Value is a list of all (possible) method function names of
;;; generic function GF.
;;;
(defun debug::all-method-function-names (gf)
  (loop with gf = (if (symbolp gf) (gdefinition gf) gf)
	for method in (generic-function-methods gf)
	as name = (nth-value 2 (parse-method-or-spec method))
	collect name
	collect (list* 'fast-method (cdr name))))

(defun debug::all-method-functions-in-package (pkg)
  (let ((gfs ()))
    (map-all-generic-functions
     (lambda (gf)
       (multiple-value-bind (valid base)
	   (valid-function-name-p (generic-function-name gf))
	 (declare (ignore valid))
	 (when (and (symbolp base)
		    (eq (symbol-package base) pkg))
	   (push gf gfs)))))
    (loop for gf in gfs nconc (debug::all-method-function-names gf))))

;;;
;;; Reinitialize method function NAME from its fdefinitions.
;;;
(defun profile::reinitialize-method-function (name)
  (multiple-value-bind (gf method method-name)
      (parse-method-or-spec (cdr name))
    (declare (ignore gf method-name))
    (with-slots (function fast-function) method
      (ecase (car name)
	(method
	 (when function
	   (setq function (fdefinition name))))
	(fast-method
	 (when fast-function
	   (let* ((new (fdefinition name))
		  (plist (method-function-plist new)))
	     ;;
	     ;; This is necessary so that, for instance, the arg-info of
	     ;; the function can be determined.
	     (unless plist
	       (setf (method-function-plist new)
		     (method-function-plist fast-function)))
	     (setq fast-function new))))))))

(defmacro undefmethod (&rest args)
  `(undefmethod-1 ',args))

(defun undefmethod-1 (args)
  (multiple-value-bind (gf method)
      (parse-method-or-spec args)
    (when (and gf method)
      (remove-method gf method)
      method)))


(pushnew :pcl *features*)
(pushnew :portable-commonloops *features*)
(pushnew :pcl-structures *features*)
(pushnew :gerds-pcl *features*)

(when (find-package "OLD-PCL")
  (setf (symbol-function (find-symbol "PRINT-OBJECT" :old-pcl))
        (symbol-function 'pcl::print-object)))


;;;; MAKE-LOAD-FORM

(export '(lisp::make-load-form lisp::make-load-form-saving-slots) "CL")

(defgeneric make-load-form (object &optional environment))

(macrolet ((define-default-method (class)
	     `(defmethod make-load-form ((object ,class) &optional env)
		(declare (ignore env))
		(error "~@<Default ~s method for ~s called.~@>"
		       'make-load-form object))))
  (define-default-method condition)
  (define-default-method standard-object))

(defmethod make-load-form ((object structure-object) &optional environment)
  (declare (ignore environment))
  (kernel:make-structure-load-form object))

(defmethod make-load-form ((object wrapper) &optional env)
  (declare (ignore env))
  (let ((pname (kernel:class-proper-name (kernel:layout-class object))))
    (unless pname
      (error "~@<Can't dump wrapper for anonymous class ~S.~@:>"
	     (kernel:layout-class object)))
    `(kernel:%class-layout (kernel::find-class ',pname))))

(defmethod make-load-form ((class class) &optional env)
  (declare (ignore env))
  (let ((name (class-name class)))
    (unless (and name (eq (find-class name nil) class))
      (error "~@<Can't use anonymous or undefined class as constant: ~S~:@>"
	     class))
    `(find-class ',name)))

(defun make-load-form-saving-slots (object &key slot-names environment)
  (declare (ignore environment))
  (let ((class (class-of object)))
    (collect ((inits))
      (dolist (slot (class-slots class))
	(let ((slot-name (slot-definition-name slot)))
	  (when (or (memq slot-name slot-names)
		    (and (null slot-names)
			 (eq :instance (slot-definition-allocation slot))))
	    (if (slot-boundp-using-class class object slot)
		(let ((value (slot-value-using-class class object slot)))
		  (inits `(setf (slot-value ,object ',slot-name) ',value)))
		(inits `(slot-makunbound ,object ',slot-name))))))
      (values `(allocate-instance (find-class ',(class-name class)))
	      `(progn .,(inits))))))

