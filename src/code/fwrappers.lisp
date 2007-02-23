;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

(ext:file-comment "$Header: /project/cmucl/cvsroot/src/code/fwrappers.lisp,v 1.5 2004/01/09 04:34:17 toy Exp $")

(in-package :fwrappers)

(defstruct (fwrapper
	     (:alternate-metaclass kernel:funcallable-instance
				   kernel:funcallable-structure-class
				   kernel:make-funcallable-structure-class)
	     (:type kernel:funcallable-structure)
	     (:constructor make-fwrapper (constructor type user-data))
	     (:print-function print-fwrapper))
  "A funcallable instance used to implement fwrappers.
   The CONSTRUCTOR slot is a function defined with DEFINE-FWRAPPER.
   This function returns an instance closure closing over an 
   fwrapper object, which is installed as the funcallable-instance
   function of the fwrapper object."
  (next		#'null		:type function)
  (type		nil		:type t)
  (constructor	#'null		:type function)
  (user-data	nil		:type t))

(defun print-fwrapper (fwrapper stream depth)
  "Print-function for struct FWRAPPER."
  (declare (ignore depth))
  (print-unreadable-object (fwrapper stream :type t :identity t)
    (format stream "~s" (fwrapper-type fwrapper))))

(declaim (inline fwrapper-or-nil))
(defun fwrapper-or-nil (fun)
  "Return FUN if it is an fwrapper or nil if it isn't."
  (and (functionp fun)
       ;; Necessary for cold-load reasons.
       (= (get-type fun) vm:funcallable-instance-header-type)
       (fwrapper-p fun)
       fun))

(defmacro do-fwrappers ((var fdefn &optional result) &body body)
  "Evaluate BODY with VAR bound to consecutive fwrappers of
   FDEFN.  Return RESULT at the end."
  `(loop for ,var = (fwrapper-or-nil (fdefn-function ,fdefn))
	 then (fwrapper-or-nil (fwrapper-next ,var))
	 while ,var do (locally ,@body)
	 finally (return ,result)))

(declaim (inline last-fwrapper))
(defun last-fwrapper (fdefn)
  "Return tha last encapsulation of FDEFN or NIL if none."
  (do-fwrappers (f fdefn)
    (when (null (fwrapper-or-nil (fwrapper-next f)))
      (return f))))

(defun push-fwrapper (f function-name)
  "Prepend encapsulation F to the definition of FUNCTION-NAME.
   Signal an error if FUNCTION-NAME is an undefined function."
  (declare (type fwrapper f))
  (let ((fdefn (fdefn-or-lose function-name)))
    (setf (fwrapper-next f) (fdefn-function fdefn))
    (setf (fdefn-function fdefn) f)))

(defun delete-fwrapper (f function-name)
  "Remove fwrapper F from the definition of FUNCTION-NAME."
  (set-fwrappers function-name
		 (delete f (list-fwrappers function-name))))

(defun list-fwrappers (function-name)
  "Return a list of all fwrappers of FUNCTION-NAME, ordered
   from outermost to innermost."
  (collect ((result))
    (do-fwrappers (f (fdefn-or-lose function-name) (result))
      (result f))))

(defun set-fwrappers (function-name fwrappers)
  "Set FUNCTION-NAMES's fwrappers to elements of the list
   FWRAPPERS, which is assumed to be ordered from outermost to
   innermost.  FWRAPPERS null means remove all fwrappers."
  (let ((fdefn (fdefn-or-lose function-name))
	(primary-function (fdefinition function-name)))
    (setf (fdefn-function fdefn) primary-function)
    (dolist (f (reverse fwrappers))
      (push-fwrapper f function-name))))

(defun fwrap (function-name constructor &key type user-data)
  "Wrap the function named FUNCTION-NAME in an fwrapper of type TYPE,
   created by calling CONSTRUCTOR.  CONSTRUCTOR is a function
   defined with DEFINE-FWRAPPER, or the name of such a function.
   Return the fwrapper created.  USER-DATA is arbitrary data to be
   associated with the fwrapper.  It is accessible in wrapper
   functions defined with DEFINE-FWRAPPER as (FWRAPPER-USER-DATA
   FWRAPPER)."
  (let ((f (make-fwrapper (coerce constructor 'function) type user-data)))
    (update-fwrapper f)
    (push-fwrapper f function-name)))

(defun funwrap (function-name &key (type nil type-p) test)
  "Remove fwrappers from the function named FUNCTION-NAME.
   If TYPE is supplied, remove fwrappers whose type is equal to TYPE.
   If TEST is supplied, remove fwrappers satisfying TEST.
   If both are not specified, remove all fwrappers."
  (collect ((new))
    (when (or type-p test)
      (do-fwrappers (f (fdefn-or-lose function-name))
	(when (or (not type-p) (not (equal type (fwrapper-type f))))
	  (when (or (null test) (not (funcall test f)))
	    (new f))))
      (set-fwrappers function-name (new)))))

(defun update-fwrapper (f)
  "Update the funcallable instance function of fwrapper F from its
   constructor."
  (setf (kernel:funcallable-instance-function f)
	(funcall (fwrapper-constructor f) f)))

(defun update-fwrappers (function-name &key (type nil type-p) test)
  "Update fwrapper function definitions of FUNCTION-NAME.
   If TYPE is supplied, update fwrappers whose type is equal to TYPE.
   If TEST is supplied, update fwrappers satisfying TEST."
  (do-fwrappers (f (fdefn-or-lose function-name))
    (when (or (not type-p) (not (equal type (fwrapper-type f))))
      (when (or (null test) (not (funcall test f)))
	(update-fwrapper f)))))

(defun find-fwrapper (function-name &key (type nil type-p) test)
  "Find an fwrapper of FUNCTION-NAME.
   If TYPE is supplied, find an fwrapper whose type is equal to TYPE.
   If TEST is supplied, find an fwrapper satisfying TEST."
  (do-fwrappers (f (fdefn-or-lose function-name))
    (when (or (not type-p) (equal type (fwrapper-type f)))
      (when (or (null test) (funcall test f))
	(return f)))))

(defmacro define-fwrapper (name lambda-list &body body &environment env)
  "Like DEFUN, but define a function wrapper.
   In BODY, the symbol FWRAPPERS:FWRAPPERS refers to the currently
   executing fwrapper.  FWRAPPERS:CALL-NEXT-FUNCTION can be used
   in BODY to call the next fwrapper or the primary function.  When
   called with no arguments, CALL-NEXT-FUNCTION invokes the next
   function with the original args to the fwrapper, otherwise it
   invokes the next function with the supplied args."
  (expand-define-fwrapper name lambda-list body env))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-define-fwrapper (name lambda-list body env)
    "Return the expansion of a DEFINE-FWRAPPER."
    (multiple-value-bind (required optional restp rest keyp keys allowp
				   aux morep)
	(kernel:parse-lambda-list lambda-list)
      (when morep
	(error "&MORE not supported in fwrapper lambda lists"))
      (multiple-value-bind (body declarations documentation)
	  (system:parse-body body env t)
	(multiple-value-bind (lambda-list call-next)
	    (flet ((make-call-next (restp)
		     `(multiple-value-call (fwrapper-next fwrapper)
			(values ,@required)
			,(if restp
			     `(values-list ,rest)
			     `(c:%more-arg-values .c. 0 .n.))))
		   (make-lambda-list (optp restp)
		     `(,@required
		       ,@(when (and optp optional) `(&optional ,@optional))
		       ,@(cond ((eq restp t) `(&rest ,rest))
			       ((null restp) `(c:&more .c. .n.))
			       (t nil))
		       ,@(when keyp `(&key))
		       ,@(when (and optp keys) `(,@keys))
		       ,@(when allowp `(&allow-other-keys))
		       ,@(when aux `(&aux ,@aux)))))
	      (if (or restp keys optional)
		  (multiple-value-bind (used-p rest-used-p)
		      (uses-vars-p `(progn . ,body) optional keys rest env)
		    (values (make-lambda-list used-p rest-used-p)
			    (make-call-next rest-used-p)))
		  (values (make-lambda-list nil :none)
			  `(funcall (fwrapper-next fwrapper) ,@required))))
	  `(defun ,name (fwrapper)
	     ,@(when documentation `(,documentation))
	     #'(kernel:instance-lambda ,lambda-list
		 ,@declarations
		 (flet ((%call-next-function ()
			  ,call-next))
		   (declare (ignorable #'%call-next-function))
		   (macrolet ((call-next-function (&rest args)
				(if args
				    `(funcall (fwrapper-next fwrapper) ,@args)
				    `(%call-next-function))))
		     ,@body))))))))

  (defun uses-vars-p (body optionals keys rest env)
    "First value is true if BODY refers to any of the variables in
     OPTIONALS, KEYS or REST, which are what KERNEL:PARSE-LAMBDA-LIST
     returns.  Second value is true if BODY refers to REST."
    (collect ((vars))
      (dolist (v (append optionals keys))
	(etypecase v
	  (cons
	   (destructuring-bind (v &optional value supplied-p) v
	     (declare (ignore value))
	     (vars (if (consp v) (cadr v) v))
	     (when supplied-p (vars supplied-p))))
	  (symbol
	   (vars v))))
      (when rest
	(vars rest))
      (let (used-p rest-used-p)
	(flet ((walk (form context env)
		 (when (and (eq context :eval)
			    (symbolp form)
			    (memq form (vars))
			    (not (walker:variable-lexical-p form env)))
		   (setq used-p t)
		   (setq rest-used-p (or rest-used-p (eq form rest))))
		 form))
	  (walker:walk-form body env #'walk)
	  (values used-p rest-used-p)))))
  )


;;;
;;; Compatibility with old encapsulation API.
;;;

(define-fwrapper encapsulation-fwrapper (&rest args)
  "Fwrapper for old-style encapsulations."
  (let ((basic-definition (fwrapper-next fwrapper))
	(argument-list args))
    (declare (special basic-definition argument-list))
    (eval (fwrapper-user-data fwrapper))))

(defun encapsulate (name type body)
  "This function is deprecated; use fwrappers instead."
  (fwrap name #'encapsulation-fwrapper :type type :user-data body))

(defun unencapsulate (name type)
  "This function is deprecated; use fwrappers instead."
  (funwrap name :type type))

(defun encapsulated-p (name type)
  "This function is deprecated; use fwrappers instead."
  (not (null (find-fwrapper name :type type))))

;;; end of file
