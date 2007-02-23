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
  "$Header: /project/cmucl/cvsroot/src/pcl/combin.lisp,v 1.25 2005/07/26 13:18:21 rtoy Exp $")

(in-package "PCL")

;;;
;;; In the following:
;;;
;;; Something "callable" is either a function, a FAST-METHOD-CALL or
;;; a METHOD-CALL instance, which can all be "invoked" by PCL.
;;;
;;; A generator for a "callable" is a function (closure) taking two
;;; arguments METHOD-ALIST and WRAPPERS and returning a callable.
;;; 


;;; *********************************************
;;; The STANDARD method combination type  *******
;;; *********************************************
;;;
;;; This is coded by hand (rather than with DEFINE-METHOD-COMBINATION)
;;; for bootstrapping and efficiency reasons.  Note that the
;;; definition of the find-method-combination-method appears in the
;;; file defcombin.lisp, this is because EQL methods can't appear in
;;; the bootstrap.
;;;
;;; This code must conform to the code in the file defcombin, look
;;; there for more details.
;;;

;;;
;;; When adding a method to COMPUTE-EFFECTIVE-METHOD for the standard
;;; method combination, COMPUTE-EFFECTIVE-METHOD is called for
;;; determining the effective method of COMPUTE-EFFECTIVE-METHOD.
;;; That's a chicken and egg problem.  It's solved in dfun.lisp by
;;; always calling STANDARD-COMPUTE-EFFECTIVE-METHOD for the case of
;;; COMPUTE-EFFECTIVE-METHOD.
;;;
;;; A similar problem occurs with all generic functions used to compute
;;; an effective method.  For example, if a method for METHOD-QUALIFIERS
;;; is performed, the generic function METHOD-QUALIFIERS will be called,
;;; and it's not ready for use.
;;;
;;; That's actually the well-known meta-circularity of PCL.
;;;
;;; Can we use an existing definition in the compiling PCL, if any,
;;; until the effective method is ready?
;;;
#+loadable-pcl
(defmethod compute-effective-method ((gf generic-function)
				     (combin standard-method-combination)
				     applicable-methods)
  (standard-compute-effective-method gf combin applicable-methods))
  
#-loadable-pcl
(defun compute-effective-method (gf combin applicable-methods)
  (standard-compute-effective-method gf combin applicable-methods))

(defun standard-compute-effective-method (gf combin applicable-methods)
  (collect ((before) (primary) (after) (around) (invalid))
    (labels ((lose (method why)
	       (invalid-method-error
		method
		"~@<The method ~S ~A.  ~
                 Standard method combination requires all methods to have ~
                 one of the single qualifiers ~s, ~s and ~s or to have ~
                 no qualifier at all.~@:>"
		method why :around :before :after))
	     (invalid-method (method why)
	       (declare (special *in-precompute-effective-methods-p*))
	       (if *in-precompute-effective-methods-p*
		   (invalid method)
		   (lose method why))))
      (dolist (m applicable-methods)
	(let ((qualifiers (if (listp m)
			      (early-method-qualifiers m)
			      (method-qualifiers m))))
	  (cond ((null qualifiers)
		 (primary m))
		((cdr qualifiers)
		 (invalid-method m "has more than one qualifier"))
		((eq (car qualifiers) :around)
		 (around m))
		((eq (car qualifiers) :before)
		 (before m))
		((eq (car qualifiers) :after)
		 (after m))
		(t
		 (invalid-method m "has an invalid qualifier")))))
      (cond ((invalid)
	     `(%invalid-qualifiers ',gf ',combin .args. ',(invalid)))
	    ((null (primary))
	     `(%no-primary-method ',gf .args.))
	    ((and (null (before)) (null (after)) (null (around)))
	     ;;
	     ;; By returning a single CALL-METHOD form here, we enable
	     ;; an important implementation-specific optimization, which
	     ;; uses fast-method functions directly for effective method
	     ;; functions.  (Which is also the reason emfs have a
	     ;; lambda-list like fast method functionts.)
	     ;;
	     ;; This cannot be done if the gf requires keyword argument
	     ;; checking as in CLHS 7.6.5 because we can't tell in
	     ;; method functions if they are used as emfs only.  If they
	     ;; are not used as emfs only, they should accept any keyword
	     ;; arguments, per CLHS 7.6.4, for instance.
	     (let ((call-method `(call-method ,(first (primary))
					      ,(rest (primary)))))
	       (if (emfs-must-check-applicable-keywords-p gf)
		   `(progn ,call-method)
		   call-method)))
	    (t
	     (let ((main-effective-method
		    (if (or (before) (after))
			`(multiple-value-prog1
			     (progn
			       ,(make-call-methods (before))
			       (call-method ,(first (primary)) ,(rest (primary))))
			   ,(make-call-methods (reverse (after))))
			`(call-method ,(first (primary)) ,(rest (primary))))))
	       (if (around)
		   `(call-method ,(first (around))
				 (,@(rest (around))
				    (make-method ,main-effective-method)))
		   main-effective-method)))))))

(defvar *invalid-method-error*
	(lambda (&rest args)
	  (declare (ignore args))
	  (error
	   "~@<~s was called outside the dynamic scope ~
            of a method combination function (inside the body of ~
            ~s or a method on the generic function ~s).~@:>"
	   'invalid-method-error 'define-method-combination
	   'compute-effective-method)))

(defvar *method-combination-error*
	(lambda (&rest args)
	  (declare (ignore args))
	  (error
	   "~@<~s was called outside the dynamic scope ~
            of a method combination function (inside the body of ~
            ~s or a method on the generic function ~s).~@:>"
	   'method-combination-error 'define-method-combination
	   'compute-effective-method)))

(defun invalid-method-error (&rest args)
  (apply *invalid-method-error* args))

(defun method-combination-error (&rest args)
  (apply *method-combination-error* args))

(defmacro call-method (&rest args)
  (declare (ignore args))
  ;;
  ;; Hack: The PROGN is here so that RESTART-CASE doesn't see the
  ;; ERROR.  See MUNGE-RESTART-CASE-EXPRESSION in code:error.lisp.
  `(progn (error "~@<~S used outside of a effective method form.~@:>" 'call-method)))

(defmacro call-method-list (&rest calls)
  `(progn ,@calls))

(defun make-call-methods (methods)
  `(call-method-list
    ,@(mapcar (lambda (method) `(call-method ,method ())) methods)))


;;; ****************************************************
;;; Translating effective method bodies to Code  *******
;;; ****************************************************

(defun get-callable (gf form method-alist wrappers)
  (funcall (callable-generator gf form method-alist wrappers)
	   method-alist wrappers))

(defun callable-generator (gf form method-alist-p wrappers-p)
  (if (eq 'call-method (car-safe form))
      (callable-generator-for-call-method gf form)
      (callable-generator-for-emf gf form method-alist-p wrappers-p)))

;;;
;;; If the effective method is just a call to CALL-METHOD, this opens
;;; up the possibility of just using the method function of the method
;;; as the effective method function.
;;;
;;; But we have to be careful.  If that method function will ask for
;;; the next methods we have to provide them.  We do not look to see
;;; if there are next methods, we look at whether the method function
;;; asks about them.  If it does, we must tell it whether there are
;;; or aren't to prevent the leaky next methods bug.
;;; 
(defun callable-generator-for-call-method (gf form)
  (let* ((cm-args (cdr form))
	 (fmf-p (and (or (not (eq *boot-state* 'complete))
			 (gf-fast-method-function-p gf))
		     (null (cddr cm-args))))
	 (method (car cm-args))
	 (cm-args1 (cdr cm-args)))
    (lambda (method-alist wrappers)
      (callable-for-call-method gf method cm-args1 fmf-p method-alist
				wrappers))))

(defun callable-for-call-method (gf method cm-args fmf-p method-alist wrappers)
  (cond ((null method)
	 nil)
	((if (listp method)
	     (eq (car method) :early-method)
	     (method-p method))
	 (get-method-callable method cm-args gf fmf-p method-alist wrappers))
	((eq 'make-method (car-safe method))
	 (get-callable gf (cadr method) method-alist wrappers))
	(t
	 method)))

;;;
;;; Return a FAST-METHOD-CALL structure, a METHOD-CALL structure, or a
;;; method function for calling METHOD.
;;;
(defun get-method-callable (method cm-args gf fmf-p method-alist wrappers)
  (multiple-value-bind (mf real-mf-p fmf pv-cell)
      (get-method-function method method-alist wrappers)
    (cond (fmf
	   (let* ((next-methods (car cm-args))
		  (next (callable-for-call-method gf (car next-methods)
						  (list* (cdr next-methods)
							 (cdr cm-args))
						  fmf-p method-alist wrappers))
		  (arg-info (method-function-get fmf :arg-info)))
	     (make-fast-method-call :function fmf
				    :pv-cell pv-cell
				    :next-method-call next
				    :arg-info arg-info)))
	  (real-mf-p
	   (make-method-call :function mf :call-method-args cm-args))
	  (t mf))))

(defun get-method-function (method method-alist wrappers)
  (let ((fn (cadr (assoc method method-alist))))
    (if fn
	(values fn nil nil nil)
	(multiple-value-bind (mf fmf)
	    (if (listp method)
		(early-method-function method)
		(values nil (method-fast-function method)))
	  (let ((pv-table (and fmf (method-function-pv-table fmf))))
	    (if (and fmf
		     (not (and pv-table (pv-table-computing-cache-p pv-table)))
		     (or (null pv-table) wrappers))
		(let* ((pv-wrappers (when pv-table 
				      (pv-wrappers-from-all-wrappers
				       pv-table wrappers)))
		       (pv-cell (when (and pv-table pv-wrappers)
				  (pv-table-lookup pv-table pv-wrappers))))
		  (values mf t fmf pv-cell))
		(values 
		 (or mf (if (listp method)
			    (setf (cadr method)
				  (method-function-from-fast-function fmf))
			    (method-function method)))
		 t nil nil)))))))


;;;
;;; Return a closure returning a FAST-METHOD-CALL instance for the
;;; call of an effective method of generic function GF with body
;;; BODY.
;;;
(defun callable-generator-for-emf (gf body method-alist-p wrappers-p)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-function-info gf)
    (declare (ignore nkeys arg-info))
    (let* ((name (generic-function-name* gf))
	   (fmc-info (cons nreq applyp))
	   (effective-method-lambda (make-effective-method-lambda gf body)))
      (multiple-value-bind (cfunction constants)
	  (get-function1 effective-method-lambda
			 (lambda (form)
			   (memf-test-converter form gf method-alist-p
						wrappers-p))
			 (lambda (form)
			   (memf-code-converter form gf metatypes applyp
						method-alist-p wrappers-p))
			 (lambda (form)
			   (memf-constant-converter form gf)))
	(lambda (method-alist wrappers)
	  (declare (special *applicable-methods*))
	  (multiple-value-bind (valid-keys keyargs-start)
	      (when (memq '.valid-keys. constants)
		(compute-applicable-keywords gf *applicable-methods*))
	    (flet ((compute-constant (constant)
		     (if (consp constant)
			 (case (car constant)
			   (.meth.
			    (funcall (cdr constant) method-alist wrappers))
			   (.meth-list.
			    (mapcar (lambda (fn)
				      (funcall fn method-alist wrappers))
				    (cdr constant)))
			   (t constant))
			 (case constant
			   (.keyargs-start. keyargs-start)
			   (.valid-keys. valid-keys)
			   (t constant)))))
	      (let ((fn (apply cfunction
			       (mapcar #'compute-constant constants))))
		(set-function-name fn `(effective-method ,name))
		(make-fast-method-call :function fn :arg-info fmc-info)))))))))

;;;
;;; Return true if emfs of generic function GF must do keyword
;;; argument checking with CHECK-APPLICABLE-KEYWORDS.
;;;
;;; We currently do this if the generic function type has &KEY, which
;;; should be the case if the gf or any method has &KEY.  It would be
;;; possible to avoid the check if it also has &ALLOW-OTHER-KEYS, iff
;;; method functions do checks of their own, which is ugly to do,
;;; so we don't.
;;;
(defun emfs-must-check-applicable-keywords-p (gf)
  (let ((type (info function type (generic-function-name* gf))))
    (and (kernel::function-type-p type)
	 (kernel::function-type-keyp type))))

;;;
;;; Compute which keyword args are valid in a call of generic function
;;; GF with applicable methods METHODS.  See also CLHS 7.6.5.
;;;
;;; First value is either a list of valid keywords or T meaning all
;;; keys are valid.  Second value is the number of optional arguments
;;; that GF takes.  This number is used as an offset in the supplied
;;; args .DFUN-REST-ARG. in CHECK-APPLICABLE-KEYWORDS.
;;;
(defun compute-applicable-keywords (gf methods)
  (let ((any-keyp nil))
    (flet ((analyze (lambda-list)
	     (multiple-value-bind (nreq nopt keyp restp allowp keys)
		 (analyze-lambda-list lambda-list)
	       (declare (ignore nreq restp))
	       (when keyp
		 (setq any-keyp t))
	       (values nopt allowp keys))))
      (multiple-value-bind (nopt allowp keys)
	  (analyze (generic-function-lambda-list gf))
	(if allowp
	    (setq keys t)
	    (dolist (method methods)
	      (multiple-value-bind (n allowp method-keys)
		  (analyze (method-lambda-list* method))
		(declare (ignore n))
		(if allowp
		    (return (setq keys t))
		    (setq keys (union method-keys keys))))))
	;;
	;; It shouldn't happen thet neither the gf nor any method has
	;; &KEY, when this method is called.  Let's handle the case
	;; anyway, just for generality.
	(values (if any-keyp keys t) nopt)))))

;;;
;;; Check ARGS for invalid keyword arguments, beginning at position
;;; START in ARGS.  VALID-KEYS is a list of valid keywords.  VALID-KEYS
;;; being T means all keys are valid.
;;;
(defun check-applicable-keywords (args start valid-keys)
  (let ((allow-other-keys-seen nil)
	(allow-other-keys nil)
	(args (nthcdr start args)))
    (collect ((invalid))
      (loop
	 (when (null args)
	   (when (and (invalid) (not allow-other-keys))
	     (simple-program-error
	      "~@<Invalid keyword argument~p ~{~s~^, ~}.  ~
               Valid keywords are: ~{~s~^, ~}.~@:>"
	      (length (invalid))
	      (invalid)
	      valid-keys))
	   (return))
	 (let ((key (pop args)))
	   (cond ((not (symbolp key))
		  (invalid-keyword-argument key))
		 ((null args)
		  (odd-number-of-keyword-arguments))
		 ((eq key :allow-other-keys)
		  (unless allow-other-keys-seen
		    (setq allow-other-keys-seen t
			  allow-other-keys (car args))))
		 ((eq t valid-keys))
		 ((not (memq key valid-keys))
		  (invalid key))))
	 (pop args)))))

(defun odd-number-of-keyword-arguments ()
  (simple-program-error "Odd number of keyword arguments."))

(defun invalid-keyword-argument (key)
  (simple-program-error "Invalid keyword argument ~s" key))

;;;
;;; Return a lambda-form for an effective method of generic function
;;; GF with body BODY.
;;;
(defun make-effective-method-lambda (gf body)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-function-info gf)
    (declare (ignore nreq nkeys arg-info))
    ;;
    ;; Note that emfs use the same lambda-lists as fast method
    ;; functions, although they don't need all the arguments that a
    ;; fast method function needs, because this makes it possible to
    ;; use fast method functions directly as emfs.  This is achieved
    ;; by returning a single CALL-METHOD form from the method
    ;; combination.
    (let ((ll (make-fast-method-call-lambda-list metatypes applyp))
	  (check-applicable-keywords
	   (when (and applyp (emfs-must-check-applicable-keywords-p gf))
	     '((check-applicable-keywords))))
	  (error-p
	   (memq (first body) '(%no-primary-method %invalid-qualifiers)))
	  (mc-args-p
	   (when (eq *boot-state* 'complete)
	     ;; Otherwise the METHOD-COMBINATION slot is not bound.
	     (let ((combin (generic-function-method-combination gf)))
	       (and (long-method-combination-p combin)
		    (long-method-combination-args-lambda-list combin))))))
      (cond (error-p
	     `(lambda (.pv-cell. .next-method-call. &rest .args.)
		(declare (ignore .pv-cell. .next-method-call.))
		,body))
	    (mc-args-p
	     (let* ((required (dfun-arg-symbol-list metatypes))
		    (gf-args (if applyp
				 `(list* ,@required .dfun-rest-arg.)
				 `(list ,@required))))
	       `(lambda ,ll
		  (declare (ignore .pv-cell. .next-method-call.))
		  (let ((.gf-args. ,gf-args))
		    (declare (ignorable .gf-args.))
		    ,@check-applicable-keywords
		    ,body))))
	    (t
	     `(lambda ,ll
		(declare (ignore .pv-cell. .next-method-call.))
		,@check-applicable-keywords
		,body))))))

;;;
;;; Return true if a fast-method-call to METHOD can be inlined.
;;;
;;; We don't generate funcalls for standard accessor methods because
;;; they have a fast function, but that's not what is actually to be
;;; called.  What is called is a closure over MAKE-STD-*-METHOD-FUNCTION.
;;;
(defun inlinable-method-p (method)
  (and (eq *boot-state* 'complete)
       *inline-methods-in-emfs*
       (not (standard-accessor-method-p method))))

;;;
;;; Return a form for calling METHOD's fast function.  METATYPES is a
;;; list of metatypes, whose length is used to figure out the names of
;;; required emf parameters.  REST? true means the method has a &rest
;;; arg.  CALLABLE-VAR is the name of a closed-over variable
;;; containing a FAST-METHOD-CALL instance corresponding to the
;;; method invocation.
;;;
(defun make-direct-call (method metatypes rest? callable-var)
  (let* ((fn-name (method-function-name method))
	 (fn `(the function #',fn-name))
	 (cell `(fast-method-call-pv-cell ,callable-var))
	 (next `(fast-method-call-next-method-call ,callable-var))
	 (req (dfun-arg-symbol-list metatypes)))
    (assert (fboundp fn-name))
    `(funcall ,fn ,cell ,next ,@req ,@(when rest? `(.dfun-rest-arg.)))))

;;;
;;; Return the list of methods from a CALL-METHOD-LIST form.
;;;
(defun call-method-list-methods (call-method-list)
  (loop for call-method-form in (cdr call-method-list)
	collect (second call-method-form)))

;;;
;;; Compute a key from FORM.  This function is called via the
;;; GET-FUNCTION mechanism on forms of an emf lambda.  Values returned
;;; that are not EQ to FORM are considered keys.  All keys are
;;; collected and serve GET-FUNCTION as a key in its table of already
;;; computed functions.  That is, if two emf lambdas produce the same
;;; key, a previously compiled function can be used.
;;;
(defun memf-test-converter (form gf method-alist-p wrappers-p)
  (flet ((method-key (method)
	   (cond ((inlinable-method-p method)
		  (method-function-name method))
		 ((eq (get-method-call-type gf form method-alist-p wrappers-p)
		      'fast-method-call)
		  '.fast-call-method.)
		 (t '.call-method.))))
    (case (car-safe form)
      ;;
      (call-method
       (if (eq (get-method-call-type gf form method-alist-p wrappers-p)
	       'fast-method-call)
	   (method-key (second form))
	   '.call-method.))
      ;;
      (call-method-list
       (mapcar #'method-key (call-method-list-methods form)))
      ;;
      (check-applicable-keywords
       'check-applicable-keywords)
      (t
       (default-test-converter form)))))

;;;
;;; This function is called via the GET-FUNCTION mechanism on forms of
;;; an emf lambda.  First value returned replaces FORM in the emf
;;; lambda.  Second value is a list of variable names that become
;;; closure variables.
;;;
(defun memf-code-converter (form gf metatypes rest? method-alist-p
			    wrappers-p)
  (labels ((make-call (call-type method metatypes rest? callable-var)
	     (if (and (eq call-type 'fast-method-call)
		      (inlinable-method-p method))
		 (make-direct-call method metatypes rest? callable-var)
		 (make-emf-call metatypes rest? callable-var call-type)))
	   
	   (make-calls (call-type methods metatypes rest? list-var)
	     `(let ((.list. ,list-var))
		(declare (ignorable .list.))
		,@(loop for method in methods collect
			  `(let ((.call. (pop .list.)))
			     ,(make-call call-type method metatypes
					 rest? '.call.))))))
    (case (car-safe form)
      ;;
      ;; (CALL-METHOD <method-object> &optional <next-methods>)
      (call-method
       (let ((method (cadr form))
	     (callable-var (gensym))
	     (call-type (get-method-call-type gf form method-alist-p
					      wrappers-p)))
	 (values (make-call call-type method metatypes rest? callable-var)
		 (list callable-var))))
      ;;
      ;; (CALL-METHOD-LIST <call-method-form>*)
      ;; where each CALL-METHOD form is (CALL-METHOD <method>)
      (call-method-list
       (let ((list-var (gensym))
	     (call-type (get-method-list-call-type gf form method-alist-p
						   wrappers-p))
	     (methods (call-method-list-methods form)))
	 (values (make-calls call-type methods metatypes rest? list-var)
		 (list list-var))))
      ;;
      (check-applicable-keywords
       (values `(check-applicable-keywords .dfun-rest-arg.
					   .keyargs-start. .valid-keys.)
	       '(.keyargs-start. .valid-keys.)))
      (t
       (default-code-converter form)))))

(defun memf-constant-converter (form gf)
  (case (car-safe form)
    (call-method
     (list (cons '.meth.
		 (callable-generator-for-call-method gf form))))
    (call-method-list
     (list (cons '.meth-list.
		 (mapcar (lambda (form)
			   (callable-generator-for-call-method gf form))
			 (cdr form)))))
    (check-applicable-keywords
     '(.keyargs-start. .valid-keys.))
    (t
     (default-constant-converter form))))

(defun get-method-list-call-type (gf form method-alist-p wrappers-p)
  (if (every (lambda (form)
	       (eq 'fast-method-call
		   (get-method-call-type gf form method-alist-p wrappers-p)))
	     (cdr form))
      'fast-method-call
      t))

(defun get-method-call-type (gf form method-alist-p wrappers-p)
  (if (eq 'call-method (car-safe form))
      (destructuring-bind (method &rest cm-args) (cdr form)
	(declare (ignore cm-args))
	(when method
	  (if (if (listp method)
		  (eq (car method) :early-method)
		  (method-p method))
	      (if method-alist-p
		  t
		  (multiple-value-bind (mf fmf)
		      (if (listp method)
			  (early-method-function method)
			  (values nil (method-fast-function method)))
		    (declare (ignore mf))
		    (let ((pv-table (and fmf (method-function-pv-table fmf))))
		      (if (and fmf (or (null pv-table) wrappers-p))
			  'fast-method-call
			  'method-call))))
	      (if (eq 'make-method (car-safe method))
		  (get-method-call-type gf (cadr method) method-alist-p
					wrappers-p)
		  (type-of method)))))
      'fast-method-call))


;;; **************************************
;;; Generating Callables for EMFs  *******
;;; **************************************

;;;
;;; Turned off until problems with method tracing caused by it are
;;; solved (reason unknown).  Will be needed once inlining of methods
;;; in effective methods and inlining of effective method in callers
;;; gets accute.
;;; 
(defvar *named-emfs-p* nil)

;;;
;;; Return a callable object for an emf of generic function GF, with
;;; applicable methods METHODS.  GENERATOR is a function returned from
;;; CALLABLE-GENERATOR.  Call it with two args METHOD-ALIST and
;;; WRAPPERS to obtain the actual callable.
;;;
(defvar *applicable-methods*)

(defun make-callable (gf methods generator method-alist wrappers)
  (let* ((*applicable-methods* methods)
	 (callable (function-funcall generator method-alist wrappers)))
    (when *named-emfs-p*
      (let ((fn (etypecase callable
		  (fast-method-call (fast-method-call-function callable))
		  (method-call (method-call-function callable))
		  (function callable))))
	(setf (fdefinition (make-emf-name gf methods)) fn)))
    callable))

;;;
;;; Return a name for an effective method of generic function GF,
;;; composed of applicable methods METHODS.
;;;
;;; In general, the name cannot be based on the methods alone, because
;;; that doesn't take method combination arguments into account.
;;;
;;; It is possible to do better for the standard method combination,
;;; though.  The current name format is
;;;
;;;  (EFFECTIVE-METHOD gf-name around-methods before-methods
;;;       primary-method after-methods)
;;;
;;; where each method is a list (METHOD qualifiers specializers).
;;;
(defvar *emf-name-table* (make-hash-table :test 'equal))

(defun make-emf-name (gf methods)
  (let* ((early-p (early-gf-p gf))
	 (gf-name (generic-function-name* gf))
	 (emf-name
	  (if (or early-p
		  (eq (generic-function-method-combination gf)
		      *standard-method-combination*))
	      (let (primary around before after)
		(dolist (m methods)
		  (let ((qual (if early-p
				  (early-method-qualifiers m)
				  (method-qualifiers m)))
			(specl (if early-p
				   (early-method-specializers m)
				   (unparse-specializers
				    (method-specializers m)))))
		    (case (car-safe qual)
		      (:around (push `(method :around ,specl) around))
		      (:before (push `(method :before ,specl) before))
		      (:after (push `(method :after ,specl) after))
		      (t (push `(method ,specl) primary)))))
		`(effective-method ,gf-name
				   ,@(nreverse around)
				   ,@(nreverse before)
				   ,@(list (last primary))
				   ,@after))
	      `(effective-method ,gf-name ,(gensym)))))
    (or (gethash emf-name *emf-name-table*)
	(setf (gethash emf-name *emf-name-table*) emf-name))))

;;; end of combin.lisp
