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
  "$Header: /project/cmucl/cvsroot/src/pcl/defcombin.lisp,v 1.27 2005/05/16 13:12:59 rtoy Exp $")

(in-package :pcl)

;;;
;;; DEFINE-METHOD-COMBINATION
;;;

(defmacro define-method-combination (&whole form &rest args)
  (declare (ignore args))
  (if (and (cddr form)
	   (listp (caddr form)))
      (expand-long-defcombin form)
      (expand-short-defcombin form)))


;;;
;;; Implementation of INVALID-METHOD-ERROR and METHOD-COMBINATION-ERROR
;;;
;;; See combin.lisp for rest of the implementation.  This method is
;;; defined here because compute-effective-method is still a function
;;; in combin.lisp.
;;;
(defmethod compute-effective-method :around
    ((generic-function generic-function)
     (method-combination method-combination)
     applicable-methods)
  (declare (ignore applicable-methods))
  (flet ((real-invalid-method-error (method format-string &rest args)
	   (declare (ignore method))
	   (apply #'error format-string args))
	 (real-method-combination-error (format-string &rest args)
	   (apply #'error format-string args)))
    (let ((*invalid-method-error* #'real-invalid-method-error)
	  (*method-combination-error* #'real-method-combination-error))
      (call-next-method))))



;;; ***********************************
;;; STANDARD method combination *******
;;; ***********************************

;;; The STANDARD method combination type is implemented directly by
;;; the class STANDARD-METHOD-COMBINATION.  The method on
;;; COMPUTE-EFFECTIVE-METHOD does standard method combination directly
;;; and is defined by hand in the file combin.lisp.  The method for
;;; FIND-METHOD-COMBINATION must appear in this file for bootstrapping
;;; reasons.
;;;
;;; A commented out copy of this definition appears in combin.lisp.
;;; If you change this definition here, be sure to change it there
;;; also.
;;;
(defmethod find-method-combination ((generic-function generic-function)
				    (type (eql 'standard))
				    options)
  (when options
    (method-combination-error
      "The standard method combination accepts no options."))
  *standard-method-combination*)


;;; *******************************
;;; Short Method Combinations *****
;;; *******************************
;;;
;;; Short method combinations all follow the same rule for computing the
;;; effective method.  So, we just implement that rule once.  Each short
;;; method combination object just reads the parameters out of the object
;;; and runs the same rule.

(defclass short-method-combination (standard-method-combination)
     ((operator
	:reader short-combination-operator
	:initarg :operator)
      (identity-with-one-argument
	:reader short-combination-identity-with-one-argument
	:initarg :identity-with-one-argument))
  (:predicate-name short-method-combination-p))

(defun expand-short-defcombin (whole)
  (let* ((type (cadr whole))
	 (documentation
	   (getf (cddr whole) :documentation ""))
	 (identity-with-one-arg
	   (getf (cddr whole) :identity-with-one-argument nil))
	 (operator 
	   (getf (cddr whole) :operator type)))
    (make-top-level-form `(define-method-combination ,type)
			 '(:load-toplevel :execute)
      `(load-short-defcombin
	 ',type ',operator ',identity-with-one-arg 
	',documentation (c::source-location)))))

(defun set-random-documentation (type doc-type doc)
  (let ((pair (assoc doc-type
		     (info random-documentation stuff type))))
    (if pair
	(setf (cdr pair) doc)
	(push (cons doc-type doc)
	      (info random-documentation stuff type)))))

(defun load-short-defcombin (type operator ioa doc source)
  (let* ((specializers
	   (list (find-class 'generic-function)
		 (intern-eql-specializer type)
		 *the-class-t*))
	 (old-method
	   (get-method #'find-method-combination () specializers nil))
	 (new-method nil))
    (setq new-method
	  (make-instance 'standard-method
	    :qualifiers ()
	    :specializers specializers
	    :lambda-list '(generic-function type options)
	    :function (lambda (args nms &rest cm-args)
			(declare (ignore nms cm-args))
			(apply 
			 (lambda (gf type options)
			   (declare (ignore gf))
			   (make-short-method-combination
			       type options operator ioa new-method doc))
			 args))
	    :definition-source source))
    (when old-method
      (remove-method #'find-method-combination old-method))
    (add-method #'find-method-combination new-method)
    (set-random-documentation type 'method-combination doc)
    type))

(defun make-short-method-combination (type options operator ioa method doc)
  (cond ((null options) (setq options '(:most-specific-first)))
	((equal options '(:most-specific-first)))
	((equal options '(:most-specific-last)))
	(t
	 (method-combination-error
	   "~@<Invalid options to a short method combination type.  ~
            The method combination type ~S accepts one option which ~
            must be either ~s or ~s.~@:>"
	   type :most-specific-first :most-specific-last)))
  (make-instance 'short-method-combination
		 :type type
		 :options options
		 :operator operator
		 :identity-with-one-argument ioa
		 :definition-source method
		 :documentation doc))

(defmethod compute-effective-method ((gf generic-function)
				     (combin short-method-combination)
				     applicable-methods)
  (let ((type (method-combination-type combin))
	(operator (short-combination-operator combin))
	(ioa (short-combination-identity-with-one-argument combin))
	(order (car (method-combination-options combin)))
	(around ())
	(primary ())
	(invalid ()))
    (dolist (m applicable-methods)
      (let ((qualifiers (method-qualifiers m)))
	(labels ((lose (method why)
		 (invalid-method-error
		   method
		   "~@<The method ~S ~A.  ~
                    The method combination type ~S was defined with the ~
                    short form of ~s and so requires all methods have ~
		    either the single qualifier ~S or the single qualifier ~
		    ~s.~@:>"
		   method why type 'define-method-combination type
		   :around))
		 (invalid-method (method why)
		   (if *in-precompute-effective-methods-p*
		       (push method invalid)
		       (lose method why))))
	  (cond ((null qualifiers)
		 (invalid-method m "has no qualifiers"))
		((cdr qualifiers)
		 (invalid-method m "has more than one qualifier"))
		((eq (car qualifiers) :around)
		 (push m around))
		((eq (car qualifiers) type)
		 (push m primary))
		(t
		 (invalid-method m "has an illegal qualifier"))))))
    (setq around (nreverse around))
    (unless (eq order :most-specific-last)
      (setq primary (nreverse primary)))
    (let ((main-method
	    (if (and (null (cdr primary))
		     (not (null ioa)))
		`(call-method ,(car primary) ())
		`(,operator ,@(mapcar (lambda (m) `(call-method ,m ()))
				      primary)))))
      (cond (invalid
	     `(%invalid-qualifiers ',gf ',combin .args. ',invalid))
	    ((null primary)
	     `(%no-primary-method ',gf .args.))
	    ((null around)
	     main-method)
	    (t
	     `(call-method ,(car around)
			   (,@(cdr around) (make-method ,main-method))))))))


;;; ******************************
;;; Long Method Combinations *****
;;; ******************************

(defun expand-long-defcombin (form)
  (let ((type (cadr form))
	(lambda-list (caddr form))
	(method-group-specifiers (cadddr form))
	(body (cddddr form))
	(arguments-option ())
	(gf-var nil))
    (when (and (consp (car body)) (eq (caar body) :arguments))
      (setq arguments-option (cdr (pop body))))
    (when (and (consp (car body)) (eq (caar body) :generic-function))
      (setq gf-var (cadr (pop body))))
    (multiple-value-bind (documentation function)
	(make-long-method-combination-function
	  type lambda-list method-group-specifiers arguments-option gf-var
	  body)
      (make-top-level-form `(define-method-combination ,type)
			   '(:load-toplevel :execute)
	`(load-long-defcombin ',type ',documentation #',function
			      ',arguments-option (c::source-location))))))

(defun load-long-defcombin (type doc function args-lambda-list source)
  (let* ((specializers
	   (list (find-class 'generic-function)
		 (intern-eql-specializer type)
		 *the-class-t*))
	 (old-method
	   (get-method #'find-method-combination () specializers nil))
	 (new-method
	   (make-instance 'standard-method
	     :qualifiers ()
	     :specializers specializers
	     :lambda-list '(generic-function type options)
	     :function (lambda (args nms &rest cm-args)
			 (declare (ignore nms cm-args))
			 (apply
			  (lambda (generic-function type options)
			    (declare (ignore generic-function))
			    (make-instance 'long-method-combination
					   :type type
					   :options options
					   :function function
					   :args-lambda-list args-lambda-list
					   :documentation doc))
			  args))
	 :definition-source source)))
    (when old-method
      (remove-method #'find-method-combination old-method))
    (add-method #'find-method-combination new-method)
    (set-random-documentation type 'method-combination doc)
    type))

(defmethod compute-effective-method ((generic-function generic-function)
				     (combin long-method-combination)
				     applicable-methods)
  (funcall (long-method-combination-function combin)
	   generic-function combin applicable-methods))

(defun make-long-method-combination-function
       (type ll method-group-specifiers arguments-option gf-var body)
  (declare (ignore type))
  (multiple-value-bind (real-body declarations documentation)
      ;; Note that PARSE-BODY ignores its second arg ENVIRONMENT.
      (system:parse-body body nil)
    (let ((wrapped-body
	    (wrap-method-group-specifier-bindings method-group-specifiers
						  declarations
						  real-body)))
      (when gf-var
	(push `(,gf-var .generic-function.) (cadr wrapped-body)))
      (when arguments-option
	(setq wrapped-body
	      (deal-with-arguments-option wrapped-body arguments-option)))
      (when ll
	(setq wrapped-body
	      `(apply (lambda ,ll ,wrapped-body)
		      (method-combination-options .method-combination.))))
      (values
	documentation
	`(lambda (.generic-function. .method-combination. .applicable-methods.)
	   (declare (ignorable .generic-function. .method-combination.
			       .applicable-methods.))
	   (block .long-method-combination-function. ,wrapped-body))))))

(defun wrap-method-group-specifier-bindings
       (method-group-specifiers declarations real-body)
  (let ((names ())
	(specializer-caches ())
	(cond-clauses ())
	(required-checks ())
	(order-cleanups ()))
    (dolist (method-group-specifier method-group-specifiers)
      (multiple-value-bind (name tests description order required)
	  (parse-method-group-specifier method-group-specifier)
	(declare (ignore description))
	(let ((specializer-cache (gensym)))
	  (push name names)
	  (push specializer-cache specializer-caches)
	  (push `((or ,@tests)
		      (if (and (equal ,specializer-cache .specializers.)
			       (not (null .specializers.)))
			  (return-from .long-method-combination-function.
			    '(error "~@<More than one method of type ~S ~
                                     with the same specializers.~@:>"
			            ',name))
			   (setq ,specializer-cache .specializers.))
		      (push .method. ,name))
		cond-clauses)
	  (when required
	    (push `(when (null ,name)
		     (return-from .long-method-combination-function.
		       '(error "No ~S methods." ',name)))
		  required-checks))
	  (loop (unless (and (constantp order)
			     (neq order (setq order (eval order))))
		  (return t)))
	  (push (cond ((eq order :most-specific-first)
		       `(setq ,name (nreverse ,name)))
		      ((eq order :most-specific-last) ())
		      (t
		       `(ecase ,order
			 (:most-specific-first
			  (setq ,name (nreverse ,name)))
			 (:most-specific-last))))
		order-cleanups))))
    `(let (,@(nreverse names) ,@(nreverse specializer-caches))
        ,@declarations
        (dolist (.method. .applicable-methods.)
	  (let ((.qualifiers. (method-qualifiers .method.))
		(.specializers. (method-specializers .method.)))
	    (declare (ignorable .qualifiers. .specializers.))
	    (cond ,@(nreverse cond-clauses))))
      ,@(nreverse required-checks)
      ,@(nreverse order-cleanups)
      ,@real-body)))

(defun parse-method-group-specifier (method-group-specifier)
  (loop with name = (pop method-group-specifier)
	for rest on method-group-specifier
	for pattern = (car rest)
	until (memq pattern '(:description :order :required))
	collect pattern into patterns
	collect (parse-qualifier-pattern name pattern) into tests
	finally
	  (return (values name
			  tests
			  (getf rest :description
				(make-default-method-group-description
				 (nreverse patterns)))
			  (getf rest :order :most-specific-first)
			  (getf rest :required nil)))))

(defun parse-qualifier-pattern (name pattern)
  (cond ((eq pattern '()) `(null .qualifiers.))
	((eq pattern '*) t)
	((symbolp pattern) `(,pattern .qualifiers.))
	((listp pattern) `(qualifier-check-runtime ',pattern .qualifiers.))
	(t (error "~@<In the method group specifier ~S, ~
                   ~S isn't a valid qualifier pattern.~@:>"
		  name pattern))))

(defun qualifier-check-runtime (pattern qualifiers)
  (loop (cond ((and (null pattern) (null qualifiers))
	       (return t))
	      ((eq pattern '*) (return t))
	      ((and pattern qualifiers
		    (let ((element (car pattern)))
		      (or (eq element (car qualifiers))
			  (eq element '*))))
	       (pop pattern)
	       (pop qualifiers))	      
	      (t (return nil)))))

(defun make-default-method-group-description (patterns)
  (if (cdr patterns)
      (format nil
	      "methods matching one of the patterns: ~{~S, ~} ~S"
	      (butlast patterns) (car (last patterns)))
      (format nil
	      "methods matching the pattern: ~S"
	      (car patterns))))

;;;
;;; Return a form that deals with the :ARGUMENTS lambda-list of a long
;;; method combination.  WRAPPED-BODY is the body of the method
;;; combination so far, and ARGS-LAMBDA-LIST is the args-lambda-list
;;; of the method combination.
;;;
(defun deal-with-arguments-option (wrapped-body args-lambda-list)
  (let ((intercept-rebindings
	 (loop for arg in args-lambda-list
	       as var = (if (consp arg) (car arg) arg)
	       unless (symbolp var) do
		 (error "Invalid parameter specifier: ~s" arg)
	       unless (memq arg lambda-list-keywords)
	         collect `(,var ',var)))
	(nreq 0)
	(nopt 0)
	whole)
    ;;
    ;; Count the number of required and optional parameters in
    ;; ARGS-LAMBDA-LIST into NREQ and NOPT, and set WHOLE to the
    ;; name of a &WHOLE parameter, if any.
    (loop with state = 'required
	  for arg in args-lambda-list do
	    (if (memq arg lambda-list-keywords)
		(setq state arg)
		(case state
		  (required (incf nreq))
		  (&optional (incf nopt))
		  (&whole (setq whole arg state 'required)))))
    ;;
    ;; This assumes that the cadr of the WRAPPED-BODY is a let, and it
    ;; injects let-bindings of the form (ARG 'SYM) for all variables
    ;; of the argument-lambda-list; SYM is a gensym.
    (assert (memq (first wrapped-body) '(let let*)))
    (setf (second wrapped-body)
	  (append intercept-rebindings (second wrapped-body)))
    ;;
    ;; Be sure to fill out the args lambda list so that it can be too
    ;; short if it wants to.
    (unless (or (memq '&rest args-lambda-list)
		(memq '&allow-other-keys args-lambda-list))
      (let ((aux (memq '&aux args-lambda-list)))
	(setq args-lambda-list
	      (append (ldiff args-lambda-list aux)
		      (if (memq '&key args-lambda-list)
			  '(&allow-other-keys)
			  '(&rest .ignore.))
		      aux))))
    ;;
    ;; .GENERIC-FUNCTION. is bound to the generic function in the
    ;; method combination function, and .GF-ARGS. is bound to the
    ;; generic function arguments in effective method functions
    ;; created for generic functions having a method combination that
    ;; uses :ARGUMENTS.
    ;;
    ;; The DESTRUCTURING-BIND binds the parameters of the
    ;; ARGS-LAMBDA-LIST to actual generic function arguments.  Because
    ;; ARGS-LAMBDA-LIST may be shorter or longer than the generic
    ;; function's lambda list, which is only known at run time, this
    ;; destructuring has to be done on a slighly modified list of
    ;; actual arguments, from which values might be stripped or added.
    ;;
    ;; Using one of the variable names in the body inserts a symbol
    ;; into the effective method, and running the effective method
    ;; produces the value of actual argument that is bound to the
    ;; symbol.
    `(let ((inner-result. ,wrapped-body)
	   (gf-lambda-list (generic-function-lambda-list .generic-function.)))
       `(destructuring-bind ,',args-lambda-list
	    (frob-combined-method-args
	     .gf-args. ',gf-lambda-list
	     ,',nreq ,',nopt)
	  ,,(when (memq '.ignore. args-lambda-list)
	      ''(declare (ignore .ignore.)))
	  ;; If there is a &WHOLE in the args-lambda-list, let
	  ;; it result in the actual arguments of the generic-function
	  ;; not the frobbed list.
	  ,,(when whole
	      ``(setq ,',whole .gf-args.))
	  ,inner-result.))))

;;;
;;; Partition VALUES into three sections required, optional, and the
;;; rest, according to required, optional, and other parameters in
;;; LAMBDA-LIST.  Make the required and optional sections NREQ and
;;; NOPT elements long by discarding values or adding NILs.  Value is
;;; the concatenated list of required and optional sections, and what
;;; is left as rest from VALUES.
;;;
(defun frob-combined-method-args (values lambda-list nreq nopt)
  (loop with section = 'required
	for arg in lambda-list
	if (memq arg lambda-list-keywords) do
	  (setq section arg)
	  (unless (eq section '&optional)
	    (loop-finish))
	else if (eq section 'required)
	  count t into nr
	  and collect (pop values) into required
	else if (eq section '&optional)
	  count t into no
	  and collect (pop values) into optional
	finally
	  (flet ((frob (list n m)
		   (cond ((> n m) (butlast list (- n m)))
			 ((< n m) (nconc list (make-list (- m n))))
			 (t list))))
	    (return (nconc (frob required nr nreq)
			   (frob optional no nopt)
			   values)))))

