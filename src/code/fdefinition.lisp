;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/fdefinition.lisp,v 1.26 2005/05/11 12:15:05 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions that hack on the global function namespace (primarily
;;; concerned with SETF functions here).  Also, function encapsulation
;;; and routines that set and return definitions disregarding whether
;;; they might be encapsulated.
;;;
;;; Written by Rob MacLachlan
;;; Modified by Bill Chiles (wrote encapsulation stuff) 
;;; Modified more by William Lott (added ``fdefn'' objects)
;;;

(in-package "EXTENSIONS")

(export '(encapsulate unencapsulate encapsulated-p
	  basic-definition argument-list *setf-fdefinition-hook*
	  define-function-name-syntax valid-function-name-p))


(in-package "KERNEL")

(export '(fdefn make-fdefn fdefn-p fdefn-name fdefn-function fdefn-makunbound
	  fdefn-or-lose %coerce-to-function raw-definition))


(in-package "LISP")

(export '(fdefinition fboundp fmakunbound))



;;;; Function names.

(defvar *valid-function-names* ())

(defun %define-function-name-syntax (name syntax-checker)
  (let ((found (assoc name *valid-function-names* :test #'eq)))
    (if found
	(setf (cdr found) syntax-checker)
	(setq *valid-function-names*
	      (acons name syntax-checker *valid-function-names*)))))

(defmacro define-function-name-syntax (name (var) &body body)
  "Define (NAME ...) to be a valid function name whose syntax is checked
  by BODY.  In BODY, VAR is bound to an actual function name of the
  form (NAME ...) to check.  BODY should return two values.
  First value true means the function name is valid.  Second value
  is the name, a symbol, of the function for use in the BLOCK of DEFUNs
  and in similar situations."
  (let ((syntax-checker (symbolicate '%check- name '-function-name)))
    `(progn
       (defun ,syntax-checker (,var) ,@body)
       (%define-function-name-syntax ',name #',syntax-checker))))

(defun valid-function-name-p (name)
  "First value is true if NAME has valid function name syntax.
  Second value is the name, a symbol, to use as a block name in DEFUNs
  and in similar situations."
  (typecase name
    (cons
     (cond
       ((and (symbolp (car name))
	     (consp (cdr name)))
	(let ((syntax-checker (cdr (assoc (car name) *valid-function-names*
					  :test #'eq))))
	  (if syntax-checker
	      (funcall syntax-checker name)
	      (values nil name))))
       (t
	(values nil name))))
    (symbol (values t name))
    (otherwise (values nil name))))

(define-function-name-syntax setf (name)
  (destructuring-bind (setf fn &rest rest) name
    (declare (ignore setf))
    (if rest
	(values nil name)
	(typecase fn
	  (symbol
	   (values t fn))
	  (cons
	   (cond ((eq 'setf (car fn))
		  (values nil fn))
		 (t
		  (valid-function-name-p fn))))
	  (otherwise
	   (values nil fn))))))

(define-function-name-syntax :macro (name)
  (when (eql 2 (length name))
    (valid-function-name-p (second name))))

(define-function-name-syntax :compiler-macro (name)
  (when (eql 2 (length name))
    (valid-function-name-p (second name))))

(define-function-name-syntax flet (name)
  (valid-function-name-p (cadr name)))

(define-function-name-syntax labels (name)
  (valid-function-name-p (cadr name)))


;;;; Fdefinition (fdefn) objects.

(defun make-fdefn (name)
  (make-fdefn name))

(defun fdefn-name (fdefn)
  (declare (type fdefn fdefn))
  (fdefn-name fdefn))

(defun fdefn-function (fdefn)
  (declare (type fdefn fdefn)
	   (values (or function null)))
  (fdefn-function fdefn))

(defun (setf fdefn-function) (fun fdefn)
  (declare (type function fun)
	   (type fdefn fdefn)
	   (values function))
  (setf (fdefn-function fdefn) fun))

(defun fdefn-makunbound (fdefn)
  (declare (type fdefn fdefn))
  (fdefn-makunbound fdefn))


;;; FDEFN-INIT -- internal interface.
;;;
;;; This function is called by %INITIAL-FUNCTION after the globaldb has been
;;; initialized, but before anything else.  We need to install these fdefn
;;; objects into the globaldb *before* any top level forms run, or we will
;;; end up with two different fdefn objects being used for the same function
;;; name.  *INITIAL-FDEFN-OBJECTS* is set up by GENESIS.
;;;
(defvar *initial-fdefn-objects*)

(defun fdefn-init ()
  (setq *valid-function-names* nil)
  (dolist (fdefn *initial-fdefn-objects*)
    (setf (info function definition (fdefn-name fdefn)) fdefn))
  (makunbound '*initial-fdefn-objects*))

;;; FDEFINITION-OBJECT -- internal interface.
;;;
(defun fdefinition-object (name create)
  "Return the fdefn object for NAME.  If it doesn't already exist and CREATE
   is non-NIL, create a new (unbound) one."
  (declare (values (or fdefn null)))
  (multiple-value-bind (valid-name-p fname)
      (valid-function-name-p name)
    (unless valid-name-p
      (error 'simple-type-error
	     :datum fname
	     :expected-type '(satisfies valid-function-name-p)
	     :format-control "Invalid function name: ~S"
	     :format-arguments (list name))))
  (let ((fdefn (info function definition name)))
    (if (and (null fdefn) create)
	(setf (info function definition name) (make-fdefn name))
	fdefn)))

(declaim (inline fdefn-or-lose))
(defun fdefn-or-lose (name)
  "Return the FDEFN of NAME.  Signal an error if there is none
   or if it's function is null."
  (let ((fdefn (fdefinition-object name nil)))
    (unless (and fdefn (fdefn-function fdefn))
      (error 'undefined-function :name name))
    fdefn))

;;; %COERCE-TO-FUNCTION -- public.
;;;
;;; The compiler emits calls to this when someone tries to funcall a symbol.
;;;
(defun %coerce-to-function (name)
  "Returns the definition for name, including any encapsulations.  Settable
   with SETF."
  (fdefn-function (fdefn-or-lose name)))

;;; RAW-DEFINITION -- public.
;;;
;;; Just another name for %coerce-to-function.
;;; 
(declaim (inline raw-definition))
(defun raw-definition (name)
  (declare (optimize (inhibit-warnings 3)))
  ;; We know that we are calling %coerce-to-function, so don't tell us about
  ;; it.
  (%coerce-to-function name))

(defun (setf raw-definition) (function name)
  (let ((fdefn (fdefinition-object name t)))
    (setf (fdefn-function fdefn) function)))


;;;; FDEFINITION.

(defun fdefinition (function-name)
  "Return FUNCTION-NAME's global function definition.
   If FUNCTION-NAME is fwrapped, return the primary function definition
   stored in the innermost fwrapper."
  (let* ((fdefn (fdefn-or-lose function-name))
	 (last (fwrappers:last-fwrapper fdefn)))
      (if last
	  (fwrappers:fwrapper-next last)
	  (fdefn-function fdefn))))

(defvar *setf-fdefinition-hook* nil
  "This holds functions that (SETF FDEFINITION) invokes before storing the
   new value.  These functions take the function name and the new value.")

(defun %set-fdefinition (function-name new-value)
  "Set FUNCTION-NAME's global function definition to NEW-VALUE.
   If FUNCTION-NAME is fwrapped, set the primary function stored
   in the innermost fwrapper."
  (declare (type function new-value) (optimize (safety 1)))
  (let ((fdefn (fdefinition-object function-name t)))
    (when (boundp '*setf-fdefinition-hook*)
      (dolist (f *setf-fdefinition-hook*)
	(funcall f function-name new-value)))
    (let ((last (fwrappers:last-fwrapper fdefn)))
      (if last
	  (setf (fwrappers:fwrapper-next last) new-value)
	  (setf (fdefn-function fdefn) new-value)))))

(defsetf fdefinition %set-fdefinition)



;;;; FBOUNDP and FMAKUNBOUND.

(defun fboundp (name)
  "Return true if name has a global function definition."
  (let ((fdefn (fdefinition-object name nil)))
    (and fdefn (fdefn-function fdefn) t)))

(defun fmakunbound (name)
  "Make Name have no global function definition."
  (let ((fdefn (fdefinition-object name nil)))
    (when fdefn
      (fdefn-makunbound fdefn)))
  (kernel:undefine-function-name name)
  name)
