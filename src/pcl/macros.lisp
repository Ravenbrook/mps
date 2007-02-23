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
  "$Header: /project/cmucl/cvsroot/src/pcl/macros.lisp,v 1.29 2003/06/18 09:23:09 gerd Exp $")
;;;
;;; Macros global variable definitions, and other random support stuff used
;;; by the rest of the system.
;;;
;;; For simplicity (not having to use eval-when a lot), this file must be
;;; loaded before it can be compiled.
;;;

(in-package :pcl)

(declaim (declaration class variable-rebinding method-name
		      method-lambda-list))

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; (CLASS-PREDICATE <CLASS-NAME>
  (define-function-name-syntax class-predicate (name)
    (when (symbolp (cadr name))
      (values t (cadr name))))

  ;; (SLOT-ACCESSOR <CLASS> <SLOT> <READER/WRITER/BOUNDP>)
  ;; <CLASS> is :GLOBAL for functions used by ACCESSOR-SLOT-VALUE etc.
  (define-function-name-syntax slot-accessor (name)
    (values (and (symbolp (cadr name))
		 (consp (cddr name))
		 (symbolp (caddr name))
		 (consp (cdddr name))
		 (member (cadddr name) '(reader writer boundp)))
	    (caddr name)))

  ;; (METHOD NAME QUALIFIERS (SPECIALIZERS))
  (define-function-name-syntax method (name)
    (valid-function-name-p (cadr name)))

  ;; (FAST-METHOD NAME QUALIFIERS (SPECIALIZERS))
  (define-function-name-syntax fast-method (name)
    (valid-function-name-p (cadr name)))

  ;; (EFFECTIVE-METHOD GF-NAME METHOD-SPEC ...)
  (define-function-name-syntax effective-method (name)
    (valid-function-name-p (cadr name))))

(defun pcl-internal-function-name-p (name)
  (and (consp name)
       (memq (car name)
	     '(class-predicate slot-accessor
	       method fast-method effective-method))))

(import '(lisp::make-keyword))

(defmacro posq (item list)
  `(position ,item ,list :test #'eq))

(defmacro neq (x y)
  `(not (eq ,x ,y)))

(declaim (inline car-safe))
(defun car-safe (obj)
  (when (consp obj)
    (car obj)))

(defmacro doplist ((key val) plist &body body &environment env)
  (multiple-value-bind (bod decls doc)
      (system:parse-body body env)
    (declare (ignore doc))
    `(let ((.plist-tail. ,plist) ,key ,val)
       ,@decls
       (loop (when (null .plist-tail.) (return nil))
	     (setq ,key (pop .plist-tail.))
	     (when (null .plist-tail.)
	       (error "Malformed plist in doplist, odd number of elements."))
	     (setq ,val (pop .plist-tail.))
	     (progn ,@bod)))))


;;;
;;; FIND-CLASS
;;;
;;; This is documented in the CLOS specification.
;;;
(defvar *find-class* (make-hash-table :test #'eq))

(defun function-returning-nil (x)
  (declare (ignore x))
  nil)

(defmacro find-class-cell-class (cell)
  `(car ,cell))

(defmacro find-class-cell-predicate (cell)
  `(cadr ,cell))

(defmacro make-find-class-cell (class-name)
  (declare (ignore class-name))
  '(list* nil #'function-returning-nil nil))

(defun find-class-cell (symbol &optional dont-create-p)
  (or (gethash symbol *find-class*)
      (unless dont-create-p
	(unless (legal-class-name-p symbol)
	  (error "~@<~S is not a legal class name.~@:>" symbol))
	(setf (gethash symbol *find-class*) (make-find-class-cell symbol)))))

(defvar *create-classes-from-internal-structure-definitions-p* t)

(defun find-class-from-cell (symbol cell &optional (errorp t))
  (or (find-class-cell-class cell)
      (and *create-classes-from-internal-structure-definitions-p*
           (or (condition-type-p symbol) (structure-type-p symbol))
           (ensure-non-standard-class symbol))
      (cond ((null errorp) nil)
	    ((legal-class-name-p symbol)
	     (error "No class named ~S." symbol))
	    (t
	     (error "~S is not a legal class name." symbol)))))

(defun find-class-predicate-from-cell (symbol cell &optional (errorp t))
  (unless (find-class-cell-class cell)
    (find-class-from-cell symbol cell errorp))
  (find-class-cell-predicate cell))

(defun legal-class-name-p (x)
  (symbolp x))

(defun find-class (symbol &optional (errorp t) environment)
  "Returns the PCL class metaobject named by SYMBOL. An error of type
   SIMPLE-ERROR is signaled if the class does not exist unless ERRORP
   is NIL in which case NIL is returned. SYMBOL cannot be a keyword."
  (declare (ignore environment))
  (find-class-from-cell symbol (find-class-cell symbol t) errorp))

(defun find-class-predicate (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-predicate-from-cell 
   symbol (find-class-cell symbol errorp) errorp))

(defvar *boot-state* nil) ; duplicate defvar to defs.lisp

;;;
;;; When compiling #+BOOTABLE, *BOOT-STATE* is COMPLETE because that's
;;; the setting of the host PCL.  We'd could use something like
;;; *COMPILE-STATE* to tell the compiler macro when it should optimize
;;; or not in such a setting.  For simplicity we just don't optimize
;;; in the bootable PCL.
;;; 
(define-compiler-macro find-class (&whole form symbol
					  &optional (errorp t) environment)
  (declare (ignore environment))
  (if (and (constantp symbol) 
	   (legal-class-name-p (eval symbol))
	   (constantp errorp)
	   (member *boot-state* '(braid complete))
	   (not (intersection '(:loadable-pcl :bootable-pcl) *features*)))
      (let ((symbol (eval symbol))
	    (errorp (not (null (eval errorp))))
	    (class-cell (make-symbol "CLASS-CELL")))	
	`(let ((,class-cell (load-time-value (find-class-cell ',symbol))))
	   (or (find-class-cell-class ,class-cell)
	       ,(if errorp
		    `(find-class-from-cell ',symbol ,class-cell t)
		    `(and (kernel:class-cell-class 
			   ',(kernel:find-class-cell symbol))
			  (find-class-from-cell ',symbol ,class-cell nil))))))
      form))

(defun (setf find-class) (new-value name &optional errorp environment)
  (declare (ignore errorp environment))
  (if (legal-class-name-p name)
      (let ((cell (find-class-cell name)))
	(setf (find-class-cell-class cell) new-value)
	(when (and (eq *boot-state* 'complete) (null new-value))
	  (setf (kernel::find-class name) nil))
	(when (memq *boot-state* '(complete braid))
	  (when (and new-value (class-wrapper new-value))
	    (setf (find-class-cell-predicate cell)
		  (fdefinition (class-predicate-name new-value))))
	  (update-ctors 'setf-find-class :class new-value :name name))
	new-value)
      (error "~S is not a legal class name." name)))

(defun (setf find-class-predicate) (new-value symbol)
  (if (legal-class-name-p symbol)
      (setf (find-class-cell-predicate (find-class-cell symbol)) new-value)
      (error "~S is not a legal class name." symbol)))

(defmacro function-funcall (form &rest args)
  `(funcall (the function ,form) ,@args))

(defmacro function-apply (form &rest args)
  `(apply (the function ,form) ,@args))


(defsetf slot-value set-slot-value)

(defvar *cold-boot-state* nil)

#+pcl-debug
(defmacro %print (&rest args)
  `(when *cold-boot-state*
     (system:%primitive print ,@args)))

#-pcl-debug
(defmacro %print (&rest args)
  (declare (ignore args)))

#+bootable-pcl
(defmacro /show (msg)
  `(system:%primitive print ,msg))

#-bootable-pcl
(defmacro /show (&rest args)
  (declare (ignore args)))

