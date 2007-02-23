;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/eval.lisp,v 1.42 2005/07/13 17:19:33 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "LISP")
(export '(eval constantp quote proclaim
	  eval-when progn prog1 prog2 let let*
	  do do* dotimes dolist progv and or cond if the
	  macro-function special-operator-p *macroexpand-hook*
	  macroexpand-1 macroexpand block return-from
	  compiler-macro-function
	  return function setq psetq apply funcall
	  progv flet labels macrolet
	  mapcar maplist mapc mapl mapcan mapcon
	  tagbody prog prog* go 
	  values multiple-values-limit
	  values-list multiple-value-list multiple-value-call
	  multiple-value-prog1 multiple-value-bind multiple-value-setq
	  catch unwind-protect throw defun
	  lambda-list-keywords call-arguments-limit lambda-parameters-limit
	  function-lambda-expression
          ;;
          ;; Declaration symbols referenced in the cold load.
          declare special 
	  ;;
	  ;; Magical markers...
	  lambda &optional &rest &key &aux &body &whole
	  &allow-other-keys &environment))

#| Not implemented:
*evalhook* *applyhook* evalhook applyhook 
|#

(export '(eval::interpreted-function-p
	  eval::interpreted-function-lambda-expression
	  eval::interpreted-function
	  eval::interpreted-function-arglist
	  eval::interpreted-function-closure)
	"EVAL")
(import '(eval::*eval-stack-top*))

(in-package "SYSTEM")
(export '(parse-body find-if-in-closure))

(in-package "EXTENSIONS")
(export '(*top-level-auto-declare*
	  compiler-macroexpand-1 compiler-macroexpand))

(in-package "KERNEL")
(export '(invoke-macroexpand-hook))

(in-package "LISP")


(defconstant lambda-list-keywords
  '(&optional &rest &key &aux &body &whole &allow-other-keys &environment
    &parse-body c:&more)
  "Keywords that you can put in a lambda-list, supposing you should want
  to do such a thing.")

(defconstant call-arguments-limit most-positive-fixnum
  "The exclusive upper bound on the number of arguments which may be passed
  to a function, including rest args.")

(defconstant lambda-parameters-limit most-positive-fixnum
  "The exclusive upper bound on the number of parameters which may be specifed
  in a given lambda list.  This is actually the limit on required and optional
  parameters.  With &key and &aux you can get more.")

(defconstant multiple-values-limit most-positive-fixnum
  "The exclusive upper bound on the number of multiple-values that you can
  have.")


;;;;

(in-package "EVAL")

;;; This is defined here so that the printer &c can call
;;; interpreted-function-p before the full interpreter is loaded.

;;; Interpreted function.
;;;
(defstruct (interpreted-function
	    (:alternate-metaclass kernel:funcallable-instance
				  kernel:funcallable-structure-class
				  kernel:make-funcallable-structure-class)
	    (:type kernel:funcallable-structure)
	    (:constructor %make-interpreted-function)
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d)) 
	       (print-unreadable-object (s stream :identity t)
		 (lisp::output-interpreted-function s stream)))))
  ;;
  ;; The name of this interpreted function, or NIL if none specified.
  (%name nil)
  ;;
  ;; This function's debug arglist.
  (arglist nil)
  ;;
  ;; A lambda that can be converted to get the definition.
  (lambda nil)
  ;;
  ;; If this function has been converted, then this is the XEP.  If this is
  ;; false, then the function is not in the cache (or is in the process of
  ;; being removed.)
  (definition nil :type (or c::clambda null))
  ;;
  ;; The number of consequtive GCs that this function has been unused.  This is
  ;; used to control cache replacement.
  (gcs 0 :type c::index)
  ;;
  ;; True if Lambda has been converted at least once, and thus warnings should
  ;; be suppressed on additional conversions.
  (converted-once nil)
  ;;
  ;; For a closure, the closure date vector.
  (closure nil :type (or null simple-vector)))


;;;; EVAL and friends.
(in-package "LISP")

;;;
;;; This needs to be initialized in the cold load, since the top-level catcher
;;; will always restore the initial value.
(defvar *eval-stack-top* 0)

(declaim (type (member :warn t nil) *top-level-auto-declare*))

(defvar *top-level-auto-declare* :warn
  "This variable controls whether assignments to unknown variables at top-level
   (or in any other call to EVAL of SETQ) will implicitly declare the variable
   SPECIAL.  These values are meaningful:
     :WARN  -- Print a warning, but declare the variable special (the default.)
      T     -- Quietly declare the variable special.
      NIL   -- Never declare the variable, giving warnings on each use.")
  

;;; EVAL  --  Public
;;;
;;;    Pick off a few easy cases, and call INTERNAL-EVAL for the rest.
;;;
(defun eval (original-exp)
  "Evaluates its single arg in a null lexical environment, returns the
  result or results."
  (declare (optimize (safety 1)))
  (let ((exp (macroexpand original-exp)))
    (typecase exp
      (symbol
       (ecase (info variable kind exp)
	 (:constant
	  (values (info variable constant-value exp)))
	 ((:special :global)
	  (symbol-value exp))
	 ((:alien :macro)
	  (eval:internal-eval original-exp))))
      (list
       (let ((name (first exp))
	     (args (1- (length exp))))
	 (case name
	   (function
	    (unless (= args 1)
	      (error 'simple-program-error
		     :format-control "Wrong number of args to FUNCTION:~% ~S."
		     :format-arguments (list exp)))
	    (let ((name (second exp)))
	      (cond ((consp name)
		     (if (valid-function-name-p name)
			 (fdefinition name)
			 (eval:make-interpreted-function name)))
		    ((macro-function name)
		     (error 'simple-type-error
			    :datum name
			    :expected-type '(not (satisfies macro-function))
			    :format-control "~S is a macro."
			    :format-arguments (list name)))
		    ((special-operator-p name)
		     (error 'simple-type-error
			    :datum name
			    :expected-type '(not
					     (satisfies special-operator-p))
			    :format-control "~S is a special operator."
			    :format-arguments (list name)))
		    (t
		     (fdefinition name)))))
	   (quote
	    (unless (= args 1)
	      (error 'simple-program-error
		     :format-control "Wrong number of args to QUOTE:~% ~S."
		     :format-arguments (list exp)))
	    (second exp))
	   (setq
	    (unless (evenp args)
	      (error 'simple-program-error
		     :format-control "Odd number of args to SETQ:~% ~S."
		     :format-arguments (list exp)))
	    (unless (zerop args)
	      (do ((name (cdr exp) (cddr name)))
		  ((null name)
		   (do ((args (cdr exp) (cddr args)))
		       ((null (cddr args))
			;; We duplicate the call to SET so that the correct
			;; value gets returned.
			(set (first args) (eval (second args))))
		     (set (first args) (eval (second args)))))
		(let ((symbol (first name)))
		  (case (info variable kind symbol)
		    (:special)
		    (:global
		     (case *top-level-auto-declare*
		       (:warn
			(warn "Declaring ~S special." symbol))
		       ((t))
		       ((nil)
			(return (eval:internal-eval original-exp))))
		     (proclaim `(special ,symbol)))
		    (t
		     (return (eval:internal-eval original-exp))))))))
	   ((progn)
	    (when (> args 0)
	      (dolist (x (butlast (rest exp)) (eval (car (last exp))))
		(eval x))))
	   ((eval-when)
	    (if (and (> args 0)
		     (or (member 'eval (second exp))
			 (member :execute (second exp))))
		(when (> args 1)
		  (dolist (x (butlast (cddr exp)) (eval (car (last exp))))
		    (eval x)))
		(eval:internal-eval original-exp)))
	   (t
	    (if (and (symbolp name)
		     (eq (info function kind name) :function))
		(collect ((args))
		  (dolist (arg (rest exp))
		    (args (eval arg)))
		  (apply (symbol-function name) (args)))
		(eval:internal-eval original-exp))))))
      (t
       exp))))


;;; Dummy stubs for EVAL:INTERNAL-EVAL and EVAL:MAKE-INTERPRETED-FUNCTION in
;;; case the compiler isn't loaded yet.
;;;
(declaim (notinline eval:internal-eval))
(defun eval:internal-eval (form &optional quietly env)
  (declare (ignore quietly env))
  (error "Attempt to evaluation a complex expression:~%     ~S~@
	  This expression must be compiled, but the compiler is not loaded."
	 form))
;;;
(declaim (notinline eval:make-interpreted-function))
(defun eval:make-interpreted-function (x)
  (error "EVAL called on #'(lambda (x) ...) when the compiler isn't loaded:~
	  ~%     ~S~%"
	 x))


;;; FUNCTION-LAMBDA-EXPRESSION  --  Public
;;;
;;;    If interpreted, use the interpreter interface.  Otherwise, see if it was
;;; compiled with COMPILE.  If that fails, check for an inline expansion.
;;;
(defun function-lambda-expression (fun)
  "Given a function, return three values:
   1] A lambda expression that could be used to define the function, or NIL if
      the definition isn't available.
   2] NIL if the function was definitely defined in a null lexical environment,
      and T otherwise.
   3] Some object that \"names\" the function.  Although this is allowed to be
      any object, CMU CL always returns a valid function name or a string."
  (declare (type function fun))
  (if (eval:interpreted-function-p fun)
      (eval:interpreted-function-lambda-expression fun)
      (let* ((fun (%function-self fun))
	     (name (%function-name fun))
	     (code (di::function-code-header fun))
	     (info (kernel:%code-debug-info code)))
	(if info
	    (let ((source (first (c::compiled-debug-info-source info))))
	      (cond ((and (eq (c::debug-source-from source) :lisp)
			  (eq (c::debug-source-info source) fun))
		     (values (second (svref (c::debug-source-name source) 0))
			     nil name))
		    ((stringp name)
		     (values nil t name))
		    (t
		     (let ((exp (info function inline-expansion name)))
		       (if exp
			   (values exp nil name)
			   (values nil t name))))))
	    (values nil t name)))))


;;; FIND-IF-IN-CLOSURE  --  Interface
;;;
;;;    Like FIND-IF, only we do it on a compiled closure's environment.
;;;
(defun find-if-in-closure (test fun)
  (dotimes (index (1- (get-closure-length fun)))
    (let ((elt (%closure-index-ref fun index)))
      (when (funcall test elt)
	(return elt)))))


;;;; Syntactic environment access:

(defun special-operator-p (symbol)
  "If the symbol globally names a special form, returns T, otherwise NIL."
  (declare (symbol symbol))
  (eq (info function kind symbol) :special-form))

(defvar *macroexpand-hook* 'funcall
  "The value of this variable must be a function that can take three
  arguments, a macro expander function, the macro form to be expanded,
  and the lexical environment to expand in.  The function should
  return the expanded form.  This function is called by MACROEXPAND-1
  whenever a runtime expansion is needed.  Initially this is set to
  FUNCALL.")

;;; INVOKE-MACROEXPAND-HOOK -- public.
;;;
;;; The X3J13 cleanup FUNCTION-TYPE:X3J13-MARCH-88 specifies that:
;;; 
;;; "7. Clarify that the value of *MACROEXPAND-HOOK* is first coerced to a
;;;     function before being called as the expansion interface hook by
;;;     MACROEXPAND-1."
;;;
;;; This is a handy utility function that does just such a coercion.  It also
;;; stores the result back in *macroexpand-hook* so we don't have to coerce
;;; it again.
;;; 
(defun invoke-macroexpand-hook (fun form env)
  "Invoke *MACROEXPAND-HOOK* on FUN, FORM, and ENV after coercing it to
   a function."
  (unless (functionp *macroexpand-hook*)
    (setf *macroexpand-hook*
	  (coerce *macroexpand-hook* 'function)))
  (funcall *macroexpand-hook* fun form env))

(defun macro-function (symbol &optional env)
  "If SYMBOL names a macro in ENV, returns the expansion function,
   else returns NIL.  If ENV is unspecified or NIL, use the global
   environment only."
  (declare (symbol symbol))
  (let* ((fenv (when env (c::lexenv-functions env)))
	 (local-def
	  (cdr (assoc symbol fenv
		      :key #'(lambda (e)
			       (nth-value 1 (valid-function-name-p e)))))))
    (cond (local-def
	   (if (and (consp local-def) (eq (car local-def) 'MACRO))
	       (cdr local-def)
	       nil))
	  ((eq (info function kind symbol) :macro)
	   (values (info function macro-function symbol)))
	  (t
	   nil))))

;; This is supposed to take an optional env arg.  But what should we
;; do with that?  I (rtoy) don't know, so we do nothing right now.
(defun (setf macro-function) (function symbol &optional env)
  (declare (symbol symbol) (type function function))

  (when (eq (info function kind symbol) :special-form)
    (error "~S names a special form." symbol))

  (setf (info function kind symbol) :macro)
  (setf (info function macro-function symbol) function)
  (setf (symbol-function symbol)
	#'(lambda (&rest args) (declare (ignore args))
	    (error 'simple-undefined-function
		   :name symbol
		   :format-control "Cannot funcall macro functions.")))
  function)

;;; Macroexpand-1  --  Public
;;;
;;;    The Env is a LEXENV or NIL (the null environment.)
;;;
(defun macroexpand-1 (form &optional env)
  "If form is a macro (or symbol macro), expands it once.  Returns two values,
   the expanded form and a T-or-NIL flag indicating whether the form was, in
   fact, a macro.  Env is the lexical environment to expand in, which defaults
   to the null environment."
  (cond ((and (consp form) (symbolp (car form)))
	 (let ((def (macro-function (car form) env)))
	   (if def
	       (values (invoke-macroexpand-hook def form env) t)
	       (values form nil))))
	((symbolp form)
	 (let* ((venv (when env (c::lexenv-variables env)))
		(local-def (cdr (assoc form venv))))
	   (cond ((and (consp local-def)
		       (eq (car local-def) 'macro))
		  (values (cdr local-def) t))
		 ((eq (info variable kind form) :macro)
		  (values (info variable macro-expansion form) t))
		 (t
		  (values form nil)))))
	(t
	 (values form nil))))

(defun macroexpand (form &optional env)
  "Repetitively call MACROEXPAND-1 until the form can no longer be expanded.
   Returns the final resultant form, and T if it was expanded.  ENV is the
   lexical environment to expand in, or NIL (the default) for the null
   environment."
  (labels ((frob (form expanded)
	     (multiple-value-bind
		 (new-form newly-expanded)
		 (macroexpand-1 form env)
	       (if newly-expanded
		   (frob new-form t)
		   (values new-form expanded)))))
    (frob form nil)))

(defun compiler-macro-function (name &optional env)
  "If NAME names a compiler-macro, returns the expansion function,
   else returns NIL.  Note: if the name is shadowed in ENV by a local
   definition, or declared NOTINLINE, NIL is returned.  Can be
   set with SETF."
  (let ((found (and env
		    (cdr (assoc name (c::lexenv-functions env)
				:test #'equal)))))
    (unless (eq (cond ((c::defined-function-p found)
		       (c::defined-function-inlinep found))
		      (found :notinline)
		      (t
		       (info function inlinep name)))
		:notinline)
      (values (info function compiler-macro-function name)))))

(defun (setf compiler-macro-function) (function name &optional env)
  (declare (type (or symbol list) name)
	   (type (or function null) function))
  (when (eq (info function kind name) :special-form)
    (error "~S names a special form." name))
  (setf (info function compiler-macro-function name) function)
  function)

;;; While these have been dropped from the spec, and we don't use them
;;; internally, we implement them anyway, for the benefit of a user
;;; trying to debug his compiler macros.

(defun compiler-macroexpand-1 (form &optional env)
  "If FORM is a function call for which a compiler-macro has been defined,
   invoke the expander function using *macroexpand-hook* and return the
   results and T.  Otherwise, return the original form and NIL."
  (let ((fun (and (consp form) (compiler-macro-function (car form) env))))
    (if fun
	(let ((result (invoke-macroexpand-hook fun form env)))
	  (values result (not (eq result form))))
	(values form nil))))

(defun compiler-macroexpand (form &optional env)
  "Repetitively call COMPILER-MACROEXPAND-1 until the form can no longer be
   expanded.  ENV is the lexical environment to expand in, or NIL (the
   default) for the null environment."
  (labels ((frob (form expanded)
	     (multiple-value-bind
		 (new-form newly-expanded)
		 (compiler-macroexpand-1 form env)
	       (if newly-expanded
		   (frob new-form t)
		   (values new-form expanded)))))
    (frob form env)))

(defun constantp (object &optional environment)
  "True of any Lisp object that has a constant value: types that eval to
  themselves, keywords, constants, and list whose car is QUOTE."
  (declare (ignore environment))
  (typecase object
    (number t)
    (character t)
    (array t)
    (symbol
     (eq (info variable kind object) :constant))
    (list (eq (car object) 'quote))
    (function t)
    (instance t)))


;;; Function invocation:

(defun apply (function arg &rest args)
  "Applies FUNCTION to a list of arguments produced by evaluating ARGS in
  the manner of LIST*.  That is, a list is made of the values of all but the
  last argument, appended to the value of the last argument, which must be a
  list."
  (cond ((atom args)
	 (apply function arg))
	((atom (cdr args))
	 (apply function (cons arg (car args))))
	(t (do* ((a1 args a2)
		 (a2 (cdr args) (cdr a2)))
		((atom (cdr a2))
		 (rplacd a1 (car a2))
		 (apply function (cons arg args)))))))


(defun funcall (function &rest arguments)
  "Calls Function with the given Arguments."
  (apply function arguments))



;;; Multiple-Value forms:

(defun values (&rest values)
  "Returns all of its arguments, in order, as values."
  (values-list values))

(defun values-list (list)
  "Returns all of the elements of List, in order, as values."
  (values-list list))
