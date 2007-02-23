;;; -*- Package: debug -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/ntrace.lisp,v 1.36 2006/10/02 13:40:55 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This is a tracing facility based on breakpoints.
;;;
;;; Written by Rob MacLachlan and Bill Chiles.
;;;
;;; **********************************************************************
;;;
(in-package "LISP")

(export '(trace untrace))

(in-package "DEBUG")

(export '(*trace-values* *max-trace-indentation* *trace-encapsulate-default*
	  *trace-encapsulate-package-names*))

(use-package :fwrappers)

(defvar *trace-values* nil
  "This is bound to the returned values when evaluating :BREAK-AFTER and
   :PRINT-AFTER forms.")

(defvar *max-trace-indentation* 40
  "If the trace indentation exceeds this value, then indentation restarts at
   0.")

(defvar *trace-encapsulate-default* :default
  "The default value for the :ENCAPSULATE option to trace.")

(defvar *trace-encapsulate-package-names*
  '("LISP"
    "COMMON-LISP"
    "CONDITIONS"
    "DEBUG"
    "EXTENSIONS"
    "FORMAT"
    "KERNEL"
    "LOOP"
    "PRETTY-PRINT"
    "SYSTEM"
    "COMPILER"
    "TRACE")
  "List of package names.  Encapsulate functions from these packages
   by default.  This should at least include the packages of functions
   used by TRACE, directly or indirectly.")


;;;; Internal state:

;;; A hash-table that maps each traced function to the TRACE-INFO.  The entry
;;; for a closure is the shared function-entry object.
;;;
(defvar *traced-functions* (make-hash-table :test #'eq))

;;; The TRACE-INFO structure represents all the information we need to trace a
;;; given function.
;;;
(defstruct (trace-info
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (print-unreadable-object (s stream)
		 (format stream "Trace-Info ~S" (trace-info-what s)))))
	    (:make-load-form-fun :just-dump-it-normally))
  ;;
  ;; The original representation of the thing traced.
  (what nil :type (or function cons symbol))
  ;;
  ;; True if What is a function name whose definition we should track.
  (named nil)
  ;;
  ;; True if tracing is to be done by encapsulation rather than breakpoints.
  ;; T implies Named.
  (encapsulated *trace-encapsulate-default*)
  ;;
  ;; True if this trace has been untraced.
  (untraced nil)
  ;;
  ;; Breakpoints we set up to trigger tracing.
  (start-breakpoint nil :type (or di:breakpoint null))
  (end-breakpoint nil :type (or di:breakpoint null))
  ;;
  ;; The list of function names for wherein.  NIL means unspecified.
  (wherein nil :type list)
  ;;
  ;; Like wherein, but only if the caller is in the list.
  (wherein-only nil :type list)
  ;;
  ;; The following slots represent the forms that we are supposed to evaluate
  ;; on each iteration.  Each form is represented by a cons (Form . Function),
  ;; where the Function is the cached result of coercing Form to a function.
  ;; Forms which use the current environment are converted with
  ;; PREPROCESS-FOR-EVAL, which gives us a one-arg function.  
  ;; Null environment forms also have one-arg functions, but the argument is
  ;; ignored.  NIL means unspecified (the default.)
  ;;
  ;; Current environment forms:
  (condition nil)
  (break nil)
  ;;
  ;; List of current environment forms:
  (print () :type list)
  ;;
  ;; Null environment forms.
  (condition-after nil)
  (break-after nil)
  ;;
  ;; List of null environment forms
  (print-after () :type list))

;;; This is a list of conses (function-end-cookie . condition-satisfied),
;;; which we use to note distinct dynamic entries into functions.  When we
;;; enter a traced function, we add a entry to this list holding the new
;;; end-cookie and whether the trace condition was statisfied.  We must save
;;; the trace condition so that the after breakpoint knows whether to print.
;;; The length of this list tells us the indentation to use for printing TRACE
;;; messages.
;;;
;;; This list also helps us synchronize the TRACE facility dynamically for
;;; detecting non-local flow of control.  Whenever execution hits a
;;; :function-end breakpoint used for TRACE'ing, we look for the
;;; function-end-cookie at the top of *traced-entries*.  If it is not there, we
;;; discard any entries that come before our cookie.
;;;
;;; When we trace using encapsulation, we bind this variable and add
;;; (nil . condition-satisfied), so a NIL "cookie" marks an encapsulated
;;; tracing.
;;;
(defvar *traced-entries* ())
(declaim (list *traced-entries*))

;;; This variable is used to discourage infinite recursions when some trace
;;; action invokes a function that is itself traced.  In this case, we quietly
;;; ignore the inner tracing.
;;;
(defvar *in-trace* nil)


;;;; Utilities:

;;; TRACE-FDEFINITION  --  Internal
;;;
;;;    Given a function name, a function or a macro name, return the raw
;;; definition and some information.  "Raw"  means that if the result is a
;;; closure, we strip off the closure and return the bare code.  The second
;;; value is T if the argument was a function name.  The third value is one of
;;; :COMPILED, :COMPILED-CLOSURE, :INTERPRETED, :INTERPRETED-CLOSURE and
;;; :FUNCALLABLE-INSTANCE.
;;;
(defun trace-fdefinition (x)
  (multiple-value-bind (res named-p)
      (typecase x
	(symbol
	 (cond ((special-operator-p x)
		(error "Can't trace special form ~S." x))
	       ((macro-function x))
	       (t
		(values (fdefinition x) t))))
	(function x)
	((cons (member flet labels))
	 ;; An extended function name for flet/labels functions.
	 (values x t))
	(t (values (fdefinition x) t)))
    (if (eval:interpreted-function-p res)
	(values res named-p (if (eval:interpreted-function-closure res)
				:interpreted-closure :interpreted))
	(case (kernel:get-type res)
	  (#.vm:closure-header-type
	   (values (kernel:%closure-function res) named-p :compiled-closure))
	  (#.vm:funcallable-instance-header-type
	   (values res named-p :funcallable-instance))
	  (t (values res named-p :compiled))))))


;;; TRACE-REDEFINED-UPDATE  --  Internal
;;;
;;;    When a function name is redefined, and we were tracing that name, then
;;; untrace the old definition and trace the new one.
;;;
(defun trace-redefined-update (fname new-value)
  (when (fboundp fname)
    (let* ((fun (trace-fdefinition fname))
	   (info (gethash fun *traced-functions*)))
      (when (and info (trace-info-named info))
	(untrace-1 fname)
	(trace-1 fname info new-value)))))
;;;
(push #'trace-redefined-update ext:*setf-fdefinition-hook*)


;;; COERCE-FORM, COERCE-FORM-LIST  --  Internal
;;;
;;;    Annotate some forms to evaluate with pre-converted functions.  Each form
;;; is really a cons (exp . function).  Loc is the code location to use for
;;; the lexical environment.  If Loc is NIL, evaluate in the null environment.
;;; If Form is NIL, just return NIL.
;;;
(defun coerce-form (form loc)
  (when form
    (let ((exp (car form)))
      (if (di:code-location-p loc)
	  (let ((fun (di:preprocess-for-eval exp loc)))
	    (cons exp
		  #'(lambda (frame)
		      (let ((*current-frame* frame))
			(funcall fun frame)))))
	  (let* ((bod (ecase loc
			((nil) exp)
			(:encapsulated
			 `(flet ((debug:arg (n)
				   (declare (special argument-list))
				   (elt argument-list n)))
			    (declare (ignorable #'debug:arg))
			    ,exp))))
		 (fun (coerce `(lambda () ,bod) 'function)))
	    (cons exp
		  #'(lambda (frame)
		      (declare (ignore frame))
		      (let ((*current-frame* nil))
			(funcall fun)))))))))
;;;
(defun coerce-form-list (forms loc)
  (mapcar #'(lambda (x) (coerce-form x loc)) forms))

	      
;;; PRINT-TRACE-INDENTATION  --  Internal
;;;
;;;    Print indentation according to the number of trace entries.  Entries
;;; whose condition was false don't count.
;;;
(defun print-trace-indentation ()
  (let ((depth 0))
    (dolist (entry *traced-entries*)
      (when (cdr entry) (incf depth)))
    (format t "~@V,0T~D: "
	    (+ (mod (* depth 2) (- *max-trace-indentation* 2)) 2)
	    depth)))


;;; TRACE-WHEREIN-P -- Internal.
;;;
;;;    Return true if one of the Names appears on the stack below Frame.
;;;
(defun trace-wherein-p (frame names)
  (do ((frame (di:frame-down frame) (di:frame-down frame)))
      ((not frame) nil)
    (let ((frame-name (di:debug-function-name (di:frame-debug-function frame))))
      (when (member frame-name names :test #'equal)
	(return t)))))

;;; TRACE-WHEREIN-ONLY-P -- Internal
;;;
;;;    Like, TRACE-WHEREIN-ONLY-P, except true only if the last stack
;;;    frame Frame has the given name.
(defun trace-wherein-only-p (frame name)
  (let ((caller-frame (di::frame-down frame)))
    (when caller-frame
      (let ((frame-name (di:debug-function-name (di:frame-debug-function caller-frame))))
	(member frame-name name :test #'equal)))))

;;; TRACE-PRINT  --  Internal
;;;
;;;    Handle print and print-after options.
;;;
(defun trace-print (frame forms)
  (dolist (ele forms)
    (fresh-line)
    (print-trace-indentation)
    (format t "~S = ~S" (car ele) (funcall (cdr ele) frame))))

;;; TRACE-MAYBE-BREAK  --  Internal
;;;
;;;    Test a break option, and break if true.
;;;
(defun trace-maybe-break (info break where frame)
  (when (and break (funcall (cdr break) frame))
    (di:flush-frames-above frame)
    (let ((*stack-top-hint* frame))
      (break "Breaking ~A traced call to ~S:" where
	     (trace-info-what info)))))

;;; DISCARD-INVALID-ENTRIES  --  Internal
;;;
;;;    This function discards any invalid cookies on our simulated stack.
;;; Encapsulated entries are always valid, since we bind *traced-entries* in
;;; the encapsulation.
;;;
(defun discard-invalid-entries (frame)
  (loop
    (when (or (null *traced-entries*)
	      (let ((cookie (caar *traced-entries*)))
		(or (not cookie)
		    (di:function-end-cookie-valid-p frame cookie))))
      (return))
    (pop *traced-entries*)))


;;;; Hook functions:

;;; TRACE-START-BREAKPOINT-FUN -- Internal.
;;;
;;;    Return a closure that can be used for a function start breakpoint hook
;;; function and a closure that can be used as the FUNCTION-END-COOKIE
;;; function.  The first communicates the sense of the Condition to the second
;;; via a closure variable.
;;;
(defun trace-start-breakpoint-fun (info)
  (let (conditionp)
    (values
     #'(lambda (frame bpt)
	 (declare (ignore bpt))
	 (discard-invalid-entries frame)
	 (let ((condition (trace-info-condition info))
	       (wherein (trace-info-wherein info))
	       (wherein-only (trace-info-wherein-only info)))
	   (setq conditionp
		 (and (not *in-trace*)
		      (or (not condition)
			  (funcall (cdr condition) frame))
		      (or (not wherein)
			  (trace-wherein-p frame wherein))
		      (or (not wherein-only)
			  (trace-wherein-only-p frame wherein-only)))))
	 
	 (when conditionp
	   (let ((*print-length* (or *debug-print-length* *print-length*))
		 (*print-level* (or *debug-print-level* *print-level*))
		 (kernel:*current-level* 0)
		 (*standard-output* *trace-output*)
		 (*in-trace* t))
	     (fresh-line)
	     (print-trace-indentation)
	     (if (trace-info-encapsulated info)
		 (locally (declare (special basic-definition argument-list))
		   (prin1 `(,(trace-info-what info) ,@argument-list)))
		 (print-frame-call frame :verbosity 1))
	     (terpri)
	     (trace-print frame (trace-info-print info)))
	   (trace-maybe-break info (trace-info-break info) "before" frame)))

     #'(lambda (frame cookie)
	 (declare (ignore frame))
	 (push (cons cookie conditionp) *traced-entries*)))))


;;; TRACE-END-BREAKPOINT-FUN  --  Internal
;;; 
;;;    This prints a representation of the return values delivered.  First,
;;; this checks to see that cookie is at the top of *traced-entries*; if it is
;;; not, then we need to adjust this list to determine the correct indentation
;;; for output.  We then check to see if the function is still traced and that
;;; the condition succeeded before printing anything.
;;;
(defun trace-end-breakpoint-fun (info)
  #'(lambda (frame bpt *trace-values* cookie)
      (declare (ignore bpt))
      (unless (eq cookie (caar *traced-entries*))
	(setf *traced-entries*
	      (member cookie *traced-entries* :key #'car)))
      
      (let ((entry (pop *traced-entries*)))
	(when (and (not (trace-info-untraced info))
		   (or (cdr entry)
		       (let ((cond (trace-info-condition-after info)))
			 (and cond (funcall (cdr cond) frame)))))
	  (let ((*print-length* (or *debug-print-length* *print-length*))
		(*print-level* (or *debug-print-level* *print-level*))
		(kernel:*current-level* 0)
		(*standard-output* *trace-output*)
		(*in-trace* t))
	    (fresh-line)
	    (pprint-logical-block (*standard-output* nil)
	      (print-trace-indentation)
	      (pprint-indent :current 2)
	      (format t "~S returned" (trace-info-what info))
	      (dolist (v *trace-values*)
		(write-char #\space)
		(pprint-newline :linear)
		(prin1 v)))
	    (terpri)
	    (trace-print frame (trace-info-print-after info)))
	  (trace-maybe-break info (trace-info-break-after info)
			     "after" frame)))))


;;; TRACE-FWRAPPER  --  Internal
;;;
;;;    This function is called by the trace encapsulation.  It calls the
;;; breakpoint hook functions with NIL for the breakpoint and cookie, which
;;; we have cleverly contrived to work for our hook functions.
;;;
(define-fwrapper trace-fwrapper (&rest args)
  (let* ((info (fwrapper-user-data fwrapper))
	 (name (trace-info-what info))
	 (fdefn (lisp::fdefinition-object name nil))
	 (basic-definition (fwrapper-next fwrapper))
	 (argument-list args))
    (declare (special basic-definition argument-list))
    (letf (((lisp::fdefn-function fdefn) basic-definition))
      (multiple-value-bind (start cookie)
	  (trace-start-breakpoint-fun info)
	(let ((frame (di:frame-down (di:top-frame))))
	  (funcall start frame nil)
	  (let ((*traced-entries* *traced-entries*))
	    (funcall cookie frame nil)
	    (let ((vals (multiple-value-list (call-next-function))))
	      (funcall (trace-end-breakpoint-fun info) frame nil vals nil)
	      (values-list vals))))))))


;;; TRACE-1 -- Internal.
;;;
;;;    Trace one function according to the specified options.  We copy the
;;; trace info (it was a quoted constant), fill in the functions, and then
;;; install the breakpoints or encapsulation.
;;;
;;;    If non-null, Definition is the new definition of a function that we are
;;; automatically retracing; this 
;;;
(defun trace-1 (function-or-name info &optional definition)
  (multiple-value-bind (fun named kind)
      (if definition
	  (values definition t
		  (nth-value 2 (trace-fdefinition definition)))
	  (trace-fdefinition function-or-name))
    (when (gethash fun *traced-functions*)
      (warn "Function ~S already TRACE'd, retracing it." function-or-name)
      (untrace-1 fun))
    
    (let* ((debug-fun (di:function-debug-function fun))
	   (encapsulated
	    (if (eq (trace-info-encapsulated info) :default)
		(let ((encapsulate-p
		       (or (not (di::can-set-function-end-breakpoint-p debug-fun))
			   (encapsulate-by-package-p function-or-name))))
		  (ecase kind
		    (:compiled
		     encapsulate-p)
		    (:compiled-closure
		     (unless (functionp function-or-name)
		       (warn "Tracing shared code for ~S:~%  ~S"
			     function-or-name fun))
		     encapsulate-p)
		    ((:interpreted :interpreted-closure
				   :funcallable-instance)
		     t)))
		(trace-info-encapsulated info)))
	   (loc (if encapsulated
		    :encapsulated
		    (di:debug-function-start-location debug-fun)))
	   (info (make-trace-info
		  :what function-or-name
		  :named named
		  :encapsulated encapsulated
		  :wherein (trace-info-wherein info)
		  :wherein-only (trace-info-wherein-only info)
		  :condition (coerce-form (trace-info-condition info) loc)
		  :break (coerce-form (trace-info-break info) loc)
		  :print (coerce-form-list (trace-info-print info) loc)
		  :break-after (coerce-form (trace-info-break-after info) nil)
		  :condition-after
		  (coerce-form (trace-info-condition-after info) nil)
		  :print-after
		  (coerce-form-list (trace-info-print-after info) nil))))

      (flet ((verify-wherein (wherein type)
	       (dolist (wherein (trace-info-wherein info))
		 (multiple-value-bind (validp block-name)
		     (ext:valid-function-name-p wherein)
		   (unless (or (stringp block-name)
			       (fboundp block-name))
		     (warn "~S name is not a defined global function: ~S"
			   type wherein))))))
	(verify-wherein (trace-info-wherein info) :wherein)
	(verify-wherein (trace-info-wherein-only info) :wherein-only))
      

      (cond
       (encapsulated
	(unless named
	  (error "Can't use encapsulation to trace anonymous function ~S."
		 fun))
	(when (listp fun)
	  (error "Can't use encapsulation to trace local flet/labels function ~S."
		 fun))
	(fwrap function-or-name #'trace-fwrapper :type 'trace
	       :user-data info))
       (t
	(multiple-value-bind
	    (start-fun cookie-fun)
	    (trace-start-breakpoint-fun info)
	  (let ((start (di:make-breakpoint start-fun debug-fun
					   :kind :function-start))
		(end (di:make-breakpoint
		      (trace-end-breakpoint-fun info)
		      debug-fun :kind :function-end
		      :function-end-cookie cookie-fun)))
	    (setf (trace-info-start-breakpoint info) start)
	    (setf (trace-info-end-breakpoint info) end)
	    ;;
	    ;; The next two forms must be in the order in which they appear,
	    ;; since the start breakpoint must run before the function-end
	    ;; breakpoint's start helper (which calls the cookie function.)
	    ;; One reason is that cookie function requires that the CONDITIONP
	    ;; shared closure variable be initialized.
	    (di:activate-breakpoint start)
	    (di:activate-breakpoint end)))))

      (setf (gethash fun *traced-functions*) info)))

  function-or-name)

;;;
;;; Return true if FUNCTION-OR-NAME's package indicates that TRACE
;;; should use encapsulation instead of function-end breakpoints.
;;;
(defun encapsulate-by-package-p (function-or-name)
  (multiple-value-bind (valid block)
      (valid-function-name-p function-or-name)
    (when (and valid (symbolp block))
      (let* ((pkg (symbol-package block))
	     (pkg-name (and pkg (package-name pkg))))
	(member pkg-name *trace-encapsulate-package-names* :test #'equal)))))


;;;; The TRACE macro:

;;;  PARSE-TRACE-OPTIONS  --  Internal
;;;
;;;    Parse leading trace options off of Specs, modifying Info accordingly.
;;; The remaining portion of the list is returned when we encounter a plausible
;;; function name.
;;;
(defun parse-trace-options (specs info)
  (let ((current specs))
    (flet ((collect-names (value)
	     (collect ((new-names))
	       (dolist (name (if (listp (car value)) (car value) value))
		 (cond ((and (consp name) (eq (car name) 'method)
			     (ext:valid-function-name-p name))
			;; This needs to be coordinated with how the
			;; debugger prints method names.  So this is
			;; what this code does.  Any method qualifiers
			;; appear as a list in the debugger.  No
			;; qualifiers show up as NIL.  We also take the
			;; method and add a pcl::fast-method in case the
			;; method wasn't compiled.  (Do we need to do this?)
			(let ((method (cond ((atom (third name))
					     `(,(second name) (,(third name)) ,@(cdddr name)))
					    (t
					     `(,(second name) nil ,@(cddr name))))))
			  (new-names `(pcl::fast-method ,@method))
			  (new-names `(method ,@method))))
		       (t
			(new-names name))))
	       (new-names))))
      (loop
	 (when (endp current) (return))
	 (let ((option (first current))
	       (value (cons (second current) nil)))
	   (case option
	     (:condition (setf (trace-info-condition info) value))
	     (:condition-after
	      (setf (trace-info-condition info) (cons nil nil))
	      (setf (trace-info-condition-after info) value))
	     (:condition-all
	      (setf (trace-info-condition info) value)
	      (setf (trace-info-condition-after info) value))
	     (:wherein
	      (setf (trace-info-wherein info) (collect-names value)))
	     (:wherein-only
	      (setf (trace-info-wherein-only info) (collect-names value)))
	     (:encapsulate
	      (setf (trace-info-encapsulated info) (car value)))
	     (:break (setf (trace-info-break info) value))
	     (:break-after (setf (trace-info-break-after info) value))
	     (:break-all
	      (setf (trace-info-break info) value)
	      (setf (trace-info-break-after info) value))
	     (:print
	      (setf (trace-info-print info)
		    (append (trace-info-print info) (list value))))
	     (:print-after
	      (setf (trace-info-print-after info)
		    (append (trace-info-print-after info) (list value))))
	     (:print-all
	      (setf (trace-info-print info)
		    (append (trace-info-print info) (list value)))
	      (setf (trace-info-print-after info)
		    (append (trace-info-print-after info) (list value))))
	     (t (return)))
	   (pop current)
	   (unless current
	     (error "Missing argument to ~S TRACE option." option))
	   (pop current)))
      current)))


;;; EXPAND-TRACE  --  Internal
;;;
;;;    Compute the expansion of TRACE in the non-trivial case (arguments
;;; specified.)  If there are no :FUNCTION specs, then don't use a LET.  This
;;; allows TRACE to be used without the full interpreter.
;;;
(defun expand-trace (specs)
  (collect ((binds)
	    (forms))
    (let* ((global-options (make-trace-info))
	   (current (parse-trace-options specs global-options)))
      (loop
	(when (endp current) (return))
	(let ((name (pop current))
	      (options (copy-trace-info global-options)))
	  (cond
	   ((eq name :function)
	    (let ((temp (gensym)))
	      (binds `(,temp ,(pop current)))
	      (forms `(trace-1 ,temp ',options))))
	   ;;
	   ;; Generic function -> trace all method functions.
	   ((eq name :methods)
	    (let ((tem (gensym)))
	      (binds `(,tem ,(pop current)))
	      (forms `(dolist (name (all-method-function-names ,tem))
			(when (fboundp name)
			  (trace-1 name ',options))))))
	   ((and (keywordp name)
		 (not (or (fboundp name) (macro-function name))))
	    (error "Unknown TRACE option: ~S" name))
	   ;;
	   ;; Method name -> trace method functions.
	   ((and (consp name) (eq (car name) 'method))
	    (when (fboundp name)
	      (forms `(trace-1 ',name ',options)))
	    (let ((name `(pcl::fast-method ,@(cdr name))))
	      (when (fboundp name)
		(forms `(trace-1 ',name ',options)))))
	   (t
	    (forms `(trace-1 ',name ',options))))
	  (setq current (parse-trace-options current options)))))

    (if (binds)
	`(let ,(binds) (list ,@(forms)))
	`(list ,@(forms)))))


;;; %LIST-TRACED-FUNCTIONS  --  Internal
;;;
(defun %list-traced-functions ()
  (loop for x being each hash-value in *traced-functions*
        collect (trace-info-what x)))


;;; TRACE -- Public.
;;;
(defmacro trace (&rest specs)
  "TRACE {Option Global-Value}* {Name {Option Value}*}*
   TRACE is a debugging tool that prints information when specified functions
   are called.  In its simplest form:
       (trace Name-1 Name-2 ...)

   CLOS methods can be traced by specifying a name of the form
   (METHOD {Qualifier}* ({Specializer}*)).

   TRACE causes a printout on *TRACE-OUTPUT* each time that one of the named
   functions is entered or returns (the Names are not evaluated.)  The output
   is indented according to the number of pending traced calls, and this trace
   depth is printed at the beginning of each line of output.

   Options allow modification of the default behavior.  Each option is a pair
   of an option keyword and a value form.  Options may be interspersed with
   function names.  Options only affect tracing of the function whose name they
   appear immediately after.  Global options are specified before the first
   name, and affect all functions traced by a given use of TRACE.

   The following options are defined:

   :CONDITION Form
   :CONDITION-AFTER Form
   :CONDITION-ALL Form
       If :CONDITION is specified, then TRACE does nothing unless Form
       evaluates to true at the time of the call.  :CONDITION-AFTER is
       similar, but suppresses the initial printout, and is tested when the
       function returns.  :CONDITION-ALL tries both before and after.

   :WHEREIN Names
       If specified, Names is a function name or list of names.  TRACE does
       nothing unless a call to one of those functions encloses the call to
       this function (i.e. it would appear in a backtrace.)  Anonymous
       functions have string names like \"DEFUN FOO\".
   :WHEREIN-ONLY Names
       Like :WHEREIN, but only if the immediate caller is one of Names,
       instead of being any where in a backtrace.

   :BREAK Form
   :BREAK-AFTER Form
   :BREAK-ALL Form
       If specified, and Form evaluates to true, then the debugger is invoked
       at the start of the function, at the end of the function, or both,
       according to the respective option.

   :PRINT Form
   :PRINT-AFTER Form
   :PRINT-ALL Form
       In addition to the usual printout, the result of evaluating FORM is
       printed at the start of the function, at the end of the function, or
       both, according to the respective option.  Multiple print options cause
       multiple values to be printed.

   :FUNCTION Function-Form
       This is a not really an option, but rather another way of specifying
       what function to trace.  The Function-Form is evaluated immediately,
       and the resulting function is traced.

   :METHODS Function-Form
       This is a not really an option, but rather a way of specifying
       that all methods of a generic functions should be traced.  The
       Function-Form is evaluated immediately, and the methods of the resulting
       generic function are traced.

   :ENCAPSULATE {:DEFAULT | T | NIL}
       If T, the tracing is done via encapsulation (redefining the function
       name) rather than by modifying the function.  :DEFAULT is the default,
       and means to use encapsulation for interpreted functions and funcallable
       instances, breakpoints otherwise.  When encapsulation is used, forms are
       *not* evaluated in the function's lexical environment, but DEBUG:ARG can
       still be used.

   :CONDITION, :BREAK and :PRINT forms are evaluated in the lexical environment
   of the called function; DEBUG:VAR and DEBUG:ARG can be used.  The -AFTER and
   -ALL forms are evaluated in the null environment."
  (if specs
      (expand-trace specs)
      '(%list-traced-functions)))


;;;; Untracing:

;;; UNTRACE-1  --  Internal
;;;
;;;    Untrace one function.
;;;
(defun untrace-1 (function-or-name)
  (let* ((fun (trace-fdefinition function-or-name))
	 (info (gethash fun *traced-functions*)))
    (cond ((not info)
	   (warn "Function is not TRACE'd -- ~S." function-or-name))
	  (t
	   (cond ((trace-info-encapsulated info)
		  (funwrap (trace-info-what info) :type 'trace))
		 (t
		  (di:delete-breakpoint (trace-info-start-breakpoint info))
		  (di:delete-breakpoint (trace-info-end-breakpoint info))))
	   (setf (trace-info-untraced info) t)
	   (remhash fun *traced-functions*)))))

;;; UNTRACE-ALL  --  Internal
;;;
;;;    Untrace all traced functions.
;;;
(defun untrace-all ()
  (dolist (fun (%list-traced-functions))
    (untrace-1 fun))
  t)

(defmacro untrace (&rest specs)
  "Removes tracing from the specified functions.  With no args, untraces all
   functions."
  (if specs
      (collect ((res))
	(let ((current specs))
	  (loop
	    (unless current (return))
	    (let ((name (pop current)))
	      (cond ((eq name :function)
		     (res `(untrace-1 ,(pop current))))
		    ;;
		    ;; Method name -> untrace existing method functions.
		    ((and (consp name)
			  (eq (car name) 'method))
		     (when (fboundp name)
		       (res `(untrace-1 ',name)))
		     (let ((name `(pcl::fast-method ,@(cdr name))))
		       (when (fboundp name)
			 (res `(untrace-1 ',name)))))
		    ;;
		    ;; Generic function -> untrace all method functions.
		    ((eq name :methods)
		     (res
		      `(dolist (name (all-method-function-names ,(pop current)))
			 (when (fboundp name)
			   (untrace-1 name)))))
		    (t
		     (res `(untrace-1 ',name))))))
	  `(progn ,@(res) t)))
      '(untrace-all)))
