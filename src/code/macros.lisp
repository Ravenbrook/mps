;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/macros.lisp,v 1.109 2006/04/13 13:48:58 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the macros that are part of the standard
;;; Spice Lisp environment.
;;;
;;; Written by Scott Fahlman and Rob MacLachlan.
;;; Modified by Bill Chiles to adhere to the wall.
;;;
(in-package "LISP")
(export '(defvar defparameter defconstant when unless setf
	  defsetf psetf shiftf rotatef push pushnew pop
	  incf decf remf case typecase with-open-file
	  with-open-stream with-input-from-string with-output-to-string
	  locally etypecase ctypecase ecase ccase
	  get-setf-expansion define-setf-expander
          define-modify-macro destructuring-bind nth-value
          otherwise ; Sacred to CASE and related macros.
	  define-compiler-macro))

(in-package "EXTENSIONS")
(export '(do-anonymous collect iterate))

(in-package "LISP")


;;; Parse-Body  --  Public
;;;
;;;    Parse out declarations and doc strings, *not* expanding macros.
;;; Eventually the environment arg should be flushed, since macros can't expand
;;; into declarations anymore.
;;;
(defun parse-body (body environment &optional (doc-string-allowed t))
  "This function is to parse the declarations and doc-string out of the body of
  a defun-like form.  Body is the list of stuff which is to be parsed.
  Environment is ignored.  If Doc-String-Allowed is true, then a doc string
  will be parsed out of the body and returned.  If it is false then a string
  will terminate the search for declarations.  Three values are returned: the
  tail of Body after the declarations and doc strings, a list of declare forms,
  and the doc-string, or NIL if none."
  (declare (ignore environment))
  (let ((decls ())
	(doc nil))
    (do ((tail body (cdr tail)))
	((endp tail)
	 (values tail (nreverse decls) doc))
      (let ((form (car tail)))
	(cond ((and (stringp form) (cdr tail))
	       (if doc-string-allowed
		   (setq doc form
			 ;; Only one doc string is allowed.
			 doc-string-allowed nil)
		   (return (values tail (nreverse decls) doc))))
	      ((not (and (consp form) (symbolp (car form))))
	       (return (values tail (nreverse decls) doc)))
	      ((eq (car form) 'declare)
	       (push form decls))
	      (t
	       (return (values tail (nreverse decls) doc))))))))


;;;; DEFMACRO:

;;; Defmacro  --  Public
;;;
;;;    Parse the definition and make an expander function.  The actual
;;; definition is done by %defmacro which we expand into.
;;;
(defmacro defmacro (name lambda-list &body body)
  (when lisp::*enable-package-locked-errors*
    (multiple-value-bind (valid block-name)
        (ext:valid-function-name-p name)
      (declare (ignore valid))
      (let ((package (symbol-package block-name)))
        (when package
          (when (ext:package-definition-lock package)
            (restart-case
                (error 'lisp::package-locked-error
                       :package package
                       :format-control "defining macro ~A"
                       :format-arguments (list name))
              (continue ()
                :report "Ignore the lock and continue")
              (unlock-package ()
                :report "Disable the package's definition-lock then continue"
                (setf (ext:package-definition-lock package) nil))
              (unlock-all ()
                :report "Unlock all packages, then continue"
                (lisp::unlock-all-packages))))))))
  (let ((whole (gensym "WHOLE-"))
	(environment (gensym "ENV-")))
    (multiple-value-bind
	(body local-decs doc)
	(parse-defmacro lambda-list whole body name 'defmacro
			:environment environment)
      (let ((def `(lambda (,whole ,environment)
		    ,@local-decs
		    (block ,name
		      ,body))))
	`(progn
	   (eval-when (:compile-toplevel)
	     (c::do-macro-compile-time ',name #',def))
	   (eval-when (:load-toplevel :execute)
	     (c::%defmacro ',name #',def ',lambda-list ,doc)))))))


;;; %Defmacro, %%Defmacro  --  Internal
;;;
;;;    Defmacro expands into %Defmacro which is a function that is treated
;;; magically the compiler.  After the compiler has gotten the information it
;;; wants out of macro definition, it compiles a call to %%Defmacro which
;;; happens at load time.  We have a %Defmacro function which just calls
;;; %%Defmacro in order to keep the interpreter happy.
;;;
;;;    Eventually %%Defmacro should deal with clearing old compiler information
;;; for the functional value.
;;;
(defun c::%defmacro (name definition lambda-list doc)
  (assert (eval:interpreted-function-p definition))
  (setf (eval:interpreted-function-name definition) name)
  (setf (eval:interpreted-function-arglist definition) lambda-list)
  (c::%%defmacro name definition doc))
;;;
(defun c::%%defmacro (name definition doc)
  (clear-info function where-from name)
  (setf (macro-function name) definition)
  (setf (documentation name 'function) doc)
  name)



;;;; DEFINE-COMPILER-MACRO

(defmacro define-compiler-macro (name lambda-list &body body)
  "Define a compiler-macro for NAME."
  (let ((whole (gensym "WHOLE-"))
	(environment (gensym "ENV-")))
    (multiple-value-bind
	(body local-decs doc)
	(parse-defmacro lambda-list whole body name 'define-compiler-macro
			:environment environment)
      (let ((def `(lambda (,whole ,environment)
		    ,@local-decs
		    (block ,name
		      ,body))))
	`(progn
	   (eval-when (:compile-toplevel)
	     (c::do-compiler-macro-compile-time ',name #',def))
	   (eval-when (:load-toplevel :execute)
	     (c::%define-compiler-macro ',name #',def ',lambda-list ,doc)))))))


(defun c::%define-compiler-macro (name definition lambda-list doc)
  (assert (eval:interpreted-function-p definition))
  (setf (eval:interpreted-function-name definition)
	(let ((*print-case* :upcase))
	  (format nil "DEFINE-COMPILER-MACRO ~S" name)))
  (setf (eval:interpreted-function-arglist definition) lambda-list)
  (c::%%define-compiler-macro name definition doc))
;;;
(defun c::%%define-compiler-macro (name definition doc)
  (setf (compiler-macro-function name) definition)
  (setf (documentation name 'compiler-macro) doc)
  name)



;;;; DEFINE-SYMBOL-MACRO

;;; define-symbol-macro  --  Public
;;;
(defmacro define-symbol-macro (name expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-symbol-macro ',name ',expansion)))
;;;
(defun %define-symbol-macro (name expansion)
  (unless (symbolp name)
    (error 'simple-type-error :datum name :expected-type 'symbol
	   :format-control "Symbol macro name is not a symbol: ~S."
	   :format-arguments (list name)))
  (ecase (info variable kind name)
    ((:macro :global nil)
     (setf (info variable kind name) :macro)
     (setf (info variable macro-expansion name) expansion))
    (:special
     (error 'simple-program-error
	    :format-control "Symbol macro name already declared special: ~S."
	    :format-arguments (list name)))
    (:constant
     (error 'simple-program-error
	    :format-control "Symbol macro name already declared constant: ~S."
	    :format-arguments (list name))))
  name)
    

;;; DEFTYPE is a lot like DEFMACRO.

(defmacro deftype (name arglist &body body)
  "Syntax like DEFMACRO, but defines a new type."
  (unless (symbolp name)
    (simple-program-error "~S -- Type name not a symbol." name))
  (and lisp::*enable-package-locked-errors*
       (symbol-package name)
       (ext:package-definition-lock (symbol-package name))
       (restart-case
           (error 'lisp::package-locked-error
                  :package (symbol-package name)
                  :format-control "defining type ~A"
                  :format-arguments (list name))
         (continue ()
           :report "Ignore the lock and continue")
         (unlock-package ()
           :report "Disable package's definition-lock then continue"
           (setf (ext:package-definition-lock (symbol-package name)) nil))
         (unlock-all ()
           :report "Unlock all packages, then continue"
           (lisp::unlock-all-packages))))
  (let ((whole (gensym "WHOLE-")))
    (multiple-value-bind (body local-decs doc)
			 (parse-defmacro arglist whole body name 'deftype
					 :default-default ''*)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (%deftype ',name
		   #'(lambda (,whole)
		       ,@local-decs
		       (block ,name ,body))
		   ,@(when doc `(,doc)))))))
;;;
(defun %deftype (name expander &optional doc)
  (when (info declaration recognized name)
    (error "Deftype already names a declaration: ~S." name))
  (ecase (info type kind name)
    (:primitive
     (when *type-system-initialized*
       (error "Illegal to redefine standard type: ~S." name)))
    (:instance
     (warn "Redefining class ~S to be a DEFTYPE." name)
     (undefine-structure (layout-info (%class-layout (kernel::find-class name))))
     (setf (class-cell-class (find-class-cell name)) nil)
     (setf (info type compiler-layout name) nil)
     (setf (info type kind name) :defined))
    (:defined)
    ((nil)
     (setf (info type kind name) :defined)))

  (setf (info type expander name) expander)
  (when doc
    (setf (documentation name 'type) doc))
  ;; ### Bootstrap hack -- we need to define types before %note-type-defined
  ;; is defined.
  (when (fboundp 'c::%note-type-defined)
    (c::%note-type-defined name))
  name)


;;; And so is DEFINE-SETF-EXPANDER.

(defparameter defsetf-error-string "Setf expander for ~S cannot be called with ~S args.")

(defmacro define-setf-expander (access-fn lambda-list &body body)
  "Syntax like DEFMACRO, but creates a Setf-Expansion generator.  The body
  must be a form that returns the five magical values."
  (unless (symbolp access-fn)
    (simple-program-error "~S -- Access-function name not a symbol in DEFINE-SETF-EXPANDER."
	   access-fn))

  (let ((whole (gensym "WHOLE-"))
	(environment (gensym "ENV-")))
    (multiple-value-bind (body local-decs doc)
			 (parse-defmacro lambda-list whole body access-fn
					 'define-setf-expander
					 :environment environment)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (%define-setf-macro
	  ',access-fn
	  #'(lambda (,whole ,environment)
	      ,@local-decs
	      (block ,access-fn ,body))
	  nil
	  ',doc)))))

(defmacro define-setf-method (&rest stuff)
  "Obsolete, use define-setf-expander."
  `(define-setf-expander ,@stuff))


;;; %DEFINE-SETF-MACRO  --  Internal
;;;
;;;    Do stuff for defining a setf macro.
;;;
(defun %define-setf-macro (name expander inverse doc)
  (cond ((not (fboundp `(setf ,name))))
	((info function accessor-for name)
	 (warn "Defining setf macro for destruct slot accessor; redefining as ~
	        a normal function:~%  ~S"
	       name)
	 (c::define-function-name name))
	((not (eq (symbol-package name) (symbol-package 'aref)))
	 (warn "Defining setf macro for ~S, but ~S is fbound."
	       name `(setf ,name))))
  (when (or inverse (info setf inverse name))
    (setf (info setf inverse name) inverse))
  (when (or expander (info setf expander name))
    (setf (info setf expander name) expander))
  (when doc
    (setf (documentation name 'setf) doc))
  name)
  

;;;; Destructuring-bind

(defmacro destructuring-bind (lambda-list arg-list &rest body)
  "Bind the variables in LAMBDA-LIST to the contents of ARG-LIST."
  (let* ((arg-list-name (gensym "ARG-LIST-")))
    (multiple-value-bind
	(body local-decls)
	(parse-defmacro lambda-list arg-list-name body nil 'destructuring-bind
			:annonymousp t :doc-string-allowed nil)
      `(let ((,arg-list-name ,arg-list))
	 ,@local-decls
	 ,body))))


;;;; Defun, Defvar, Defparameter, Defconstant:

;;; Defun  --  Public
;;;
;;;    Very similar to Defmacro, but simpler.  We don't have to parse the
;;; lambda-list.
;;;
(defmacro defun (&whole source name lambda-list &parse-body (body decls doc))
  (multiple-value-bind (valid block-name)
      (valid-function-name-p name)
    (declare (ignore valid))
    (let ((def `(lambda ,lambda-list
		  ,@decls
		  (block ,block-name ,@body))))
      `(c::%defun ',name #',def ,doc ',source))))


;;; %Defun, %%Defun  --  Internal
;;;
;;;    Similar to %Defmacro, ...
;;;
(defun c::%%defun (name def doc &optional inline-expansion)
  (setf (fdefinition name) def)
  (when doc
    (if (and (consp name) (eq (first name) 'setf))
	(setf (documentation (second name) 'setf) doc)
	(setf (documentation name 'function) doc)))
  (c::define-function-name name)
  (when (eq (info function where-from name) :assumed)
    (setf (info function where-from name) :defined)
    (when (info function assumed-type name)
      (setf (info function assumed-type name) nil)))
  (when (or inline-expansion
	    (info function inline-expansion name))
    (setf (info function inline-expansion name) inline-expansion))
  name)

(defun c::%defun (name def doc source)
  (declare (ignore source))
  (assert (eval:interpreted-function-p def))
  (setf (eval:interpreted-function-name def) name)
  (let ((inline-expansion nil))
    (when (memq (info function inlinep name) '(:inline :maybe-inline))
      (multiple-value-bind (lambda-expression closure-p)
	  (function-lambda-expression def)
	(unless closure-p
	  (setq inline-expansion lambda-expression))))
    (c::%%defun name def doc inline-expansion)))

;;; DEFCONSTANT  --  Public
;;;
(defmacro defconstant (var val &optional doc)
  "For defining global constants at top level.  The DEFCONSTANT says that the
  value is constant and may be compiled into code.  If the variable already has
  a value, and this is not equal to the init, an error is signalled.  The third
  argument is an optional documentation string for the variable."
  `(progn
     (eval-when (:compile-toplevel)
       (c::do-defconstant-compile-time ',var ,val ',doc))
     (eval-when (:load-toplevel :execute)
       (c::%%defconstant ',var ,val ',doc (c::source-location)))))

(defun set-defvar-source-location (name source-location)
  (setf (info :source-location :defvar name) source-location))

;;; %Defconstant, %%Defconstant  --  Internal
;;;
;;;    Like the other %mumbles except that we currently actually do something
;;; interesting at load time, namely checking if the constant is being
;;; redefined.
;;;
(defun c::%defconstant (name value doc)
  (c::%%defconstant name value doc nil))
;;;
(defun c::%%defconstant (name value doc source-location)
  (when doc
    (setf (documentation name 'variable) doc))
  (when (boundp name)
    (unless (equalp (symbol-value name) value)
      (cerror "Go ahead and change the value."
	      "Constant ~S being redefined." name)))
  (setf (symbol-value name) value)
  (setf (info variable kind name) :constant)
  (clear-info variable constant-value name)
  (set-defvar-source-location name source-location)
  name)


(defmacro defvar (var &optional (val nil valp) (doc nil docp))
  "For defining global variables at top level.  Declares the variable
  SPECIAL and, optionally, initializes it.  If the variable already has a
  value, the old value is not clobbered.  The third argument is an optional
  documentation string for the variable."
  `(progn
    (declaim (special ,var))
     ,@(when valp
	 `((unless (boundp ',var)
	     (setq ,var ,val))))
    ,@(when docp
	`((setf (documentation ',var 'variable) ',doc)))
    (set-defvar-source-location ',var (c::source-location))
    ',var))

(defmacro defparameter (var val &optional (doc nil docp))
  "Defines a parameter that is not normally changed by the program,
  but that may be changed without causing an error.  Declares the
  variable special and sets its value to VAL.  The third argument is
  an optional documentation string for the parameter."
  `(progn
    (declaim (special ,var))
    (setq ,var ,val)
    ,@(when docp
	`((setf (documentation ',var 'variable) ',doc)))
    (set-defvar-source-location ',var (c::source-location))
    ',var))


;;;; ASSORTED CONTROL STRUCTURES


(defmacro when (test &body forms)
  "First arg is a predicate.  If it is non-null, the rest of the forms are
  evaluated as a PROGN."
  `(cond (,test nil ,@forms)))

(defmacro unless (test &rest forms)
  "First arg is a predicate.  If it is null, the rest of the forms are
  evaluated as a PROGN."
  `(cond ((not ,test) nil ,@forms)))


(defmacro return (&optional (value nil))
  `(return-from nil ,value))

(defmacro prog (varlist &parse-body (body decls))
  `(block nil
     (let ,varlist
       ,@decls
       (tagbody ,@body))))

(defmacro prog* (varlist &parse-body (body decls))
  `(block nil
     (let* ,varlist
       ,@decls
       (tagbody ,@body))))


;;; Prog1, Prog2  --  Public
;;;
;;;    These just turn into a Let.
;;;
(defmacro prog1 (result &rest body)
  (let ((n-result (gensym)))
    `(let ((,n-result ,result))
       ,@body
       ,n-result)))
;;;
(defmacro prog2 (form1 result &rest body)
  `(prog1 (progn ,form1 ,result) ,@body))


;;; And, Or  --  Public
;;;
;;;    AND and OR are defined in terms of IF.
;;;
(defmacro and (&rest forms)
  (cond ((endp forms) t)
	((endp (rest forms)) (first forms))
	(t
	 `(if ,(first forms)
	      (and ,@(rest forms))
	      nil))))
;;;
(defmacro or (&rest forms)
  (cond ((endp forms) nil)
	((endp (rest forms)) (first forms))
	(t
	 (let ((n-result (gensym)))
	   `(let ((,n-result ,(first forms)))
	      (if ,n-result
		  ,n-result
		  (or ,@(rest forms))))))))


;;; Cond  --  Public
;;;
;;;    COND also turns into IF.
;;;
(defmacro cond (&rest clauses)
  (if (endp clauses)
      nil
      (let ((clause (first clauses)))
	(when (atom clause)
	  (error "Cond clause is not a list: ~S." clause))
	(let ((test (first clause))
	      (forms (rest clause)))
	  (if (endp forms)
	      (let ((n-result (gensym)))
		`(let ((,n-result ,test))
		   (if ,n-result
		       ,n-result
		       (cond ,@(rest clauses)))))
	      `(if ,test
		   (progn ,@forms)
		   (cond ,@(rest clauses))))))))


;;;; Multiple value macros:

;;; Multiple-Value-XXX  --  Public
;;;
;;;    All the multiple-value receiving forms are defined in terms of
;;; Multiple-Value-Call.
;;;
(defmacro multiple-value-setq (varlist value-form)
  (unless (and (listp varlist) (every #'symbolp varlist))
    (simple-program-error "Varlist is not a list of symbols: ~S." varlist))
  (if varlist
      `(values (setf (values ,@varlist) ,value-form))
      `(values ,value-form)))

;;;
(defmacro multiple-value-bind (varlist value-form &body body)
  (unless (and (listp varlist) (every #'symbolp varlist))
    (simple-program-error  "Varlist is not a list of symbols: ~S." varlist))
  (if (= (length varlist) 1)
      `(let ((,(car varlist) ,value-form))
	 ,@body)
      (let ((ignore (gensym)))
	`(multiple-value-call #'(lambda (&optional ,@(mapcar #'list varlist) &rest ,ignore)
				  (declare (ignore ,ignore))
				  ,@body)
	   ,value-form))))
;;;
(defmacro multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))


(defmacro nth-value (n form)
  "Evaluates FORM and returns the Nth value (zero based).  This involves no
  consing when N is a trivial constant integer."
  (if (integerp n)
      (let ((dummy-list nil)
            (keeper (gensym "KEEPER-")))
        ;; We build DUMMY-LIST, a list of variables to bind to useless
        ;; values, then we explicitly IGNORE those bindings and return
        ;; KEEPER, the only thing we're really interested in right now.
        (dotimes (i n)
          (push (gensym "IGNORE-") dummy-list))
        `(multiple-value-bind (,@dummy-list ,keeper)
                              ,form
           (declare (ignore ,@dummy-list))
           ,keeper))
      (once-only ((n n))
	`(case (the (values fixnum &rest t) ,n)
	   (0 (nth-value 0 ,form))
	   (1 (nth-value 1 ,form))
	   (2 (nth-value 2 ,form))
	   (T (nth (the (values fixnum &rest t) ,n)
			(multiple-value-list ,form)))))))


;;;; SETF and friends.

;;; Note: The expansions for SETF and friends sometimes create needless
;;; LET-bindings of argument values.  The compiler will remove most of
;;; these spurious bindings, so SETF doesn't worry too much about creating
;;; them. 

;;; The inverse for a generalized-variable reference function is stored in
;;; one of two ways:
;;;
;;; A SETF inverse property corresponds to the short form of DEFSETF.  It is
;;; the name of a function takes the same args as the reference form, plus a
;;; new-value arg at the end.
;;;
;;; A SETF method expander is created by the long form of DEFSETF or
;;; by DEFINE-SETF-EXPANDER.  It is a function that is called on the reference
;;; form and that produces five values: a list of temporary variables, a list
;;; of value forms, a list of the single store-value form, a storing function,
;;; and an accessing function.

(defun get-setf-expansion (form &optional environment)
  "Returns five values needed by the SETF machinery: a list of temporary
   variables, a list of values with which to fill them, a list of temporaries
   for the new values, the setting function, and the accessing function."
  (let (temp)
    (cond ((symbolp form)
	   (multiple-value-bind
	       (expansion expanded)
	       (macroexpand-1 form environment)
	     (if expanded
		 (get-setf-expansion expansion environment)
		 (let ((new-var (gensym)))
		   (values nil nil (list new-var)
			   `(setq ,form ,new-var) form)))))
	  ;;
	  ;; Local functions inhibit global setf methods...
	  ((and environment
		(let ((name (car form)))
		  (dolist (x (c::lexenv-functions environment) nil)
		    (when (and (eq (car x) name)
			       (not (c::defined-function-p (cdr x))))
		      (return t)))))
	   (expand-or-get-setf-inverse form environment))
	  ((setq temp (info setf inverse (car form)))
	   (get-setf-method-inverse form `(,temp) nil))
	  ((setq temp (info setf expander (car form)))
	   (funcall temp form environment))
	  (t
	   (expand-or-get-setf-inverse form environment)))))

(defun get-setf-method-multiple-value (form &optional env)
  "Obsolete: use GET-SETF-EXPANSION."
  (get-setf-expansion form env))

;;;
;;; If a macro, expand one level and try again.  If not, go for the
;;; SETF function.
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind
      (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
	(get-setf-expansion expansion environment)
	(get-setf-method-inverse form `(funcall #'(setf ,(car form)))
				 t))))


(defun get-setf-method-inverse (form inverse setf-function)
  (let ((new-var (gensym))
	(vars nil)
	(vals nil))
    (dolist (x (cdr form))
      (push (gensym) vars)
      (push x vals))
    (setq vals (nreverse vals))
    (values vars vals (list new-var)
	    (if setf-function
		`(,@inverse ,new-var ,@vars)
		`(,@inverse ,@vars ,new-var))
	    `(,(car form) ,@vars))))


(defun get-setf-method (form &optional environment)
  "Obsolete: use GET-SETF-EXPANSION and handle multiple store values."
  (multiple-value-bind
      (temps value-forms store-vars store-form access-form)
      (get-setf-expansion form environment)
    (when (cdr store-vars)
      (error "GET-SETF-METHOD used for a form with multiple store ~
	      variables:~%  ~S" form))
    (values temps value-forms store-vars store-form access-form)))


(defun defsetter (fn rest)
  (let ((arglist (car rest))
	(arglist-var (gensym "ARGS-"))
	(new-var (car (cadr rest))))
    (multiple-value-bind
	(body local-decs doc)
	(parse-defmacro arglist arglist-var (cddr rest) fn 'defsetf)
      (values 
       `(lambda (,arglist-var ,new-var)
	  ,@local-decs
	  ,body)
       doc))))


(defmacro defsetf (access-fn &rest rest)
  "Associates a SETF update function or macro with the specified access
  function or macro.  The format is complex.  See the manual for
  details."
  (cond ((not (listp (car rest)))
	 `(eval-when (load compile eval)
	    (%define-setf-macro ',access-fn nil ',(car rest)
				,(when (and (car rest) (stringp (cadr rest)))
				   `',(cadr rest)))))
	((and (cdr rest) (listp (cadr rest)))
	 (destructuring-bind
	     (lambda-list (&rest store-variables) &body body)
	     rest
	   (let ((arglist-var (gensym "ARGS-"))
		 (access-form-var (gensym "ACCESS-FORM-"))
		 (env-var (gensym "ENVIRONMENT-")))
	     (multiple-value-bind
		 (body local-decs doc)
		 (parse-defmacro `(,lambda-list ,@store-variables)
				 arglist-var body access-fn 'defsetf
				 :annonymousp t)
	       `(eval-when (load compile eval)
		  (%define-setf-macro
		   ',access-fn
		   #'(lambda (,access-form-var ,env-var)
		       (declare (ignore ,env-var))
		       (%defsetf ,access-form-var ,(length store-variables)
				 #'(lambda (,arglist-var)
				     ,@local-decs
				     (block ,access-fn
				       ,body))))
		   nil
		   ',doc))))))
	(t
	 (error "Ill-formed DEFSETF for ~S." access-fn))))

(defun %defsetf (orig-access-form num-store-vars expander)
  (collect ((subforms) (subform-vars) (subform-exprs) (store-vars))
    (dolist (subform (cdr orig-access-form))
      (if (constantp subform)
	  (subforms subform)
	  (let ((var (gensym)))
	    (subforms var)
	    (subform-vars var)
	    (subform-exprs subform))))
    (dotimes (i num-store-vars)
      (store-vars (gensym)))
    (values (subform-vars)
	    (subform-exprs)
	    (store-vars)
	    (funcall expander (cons (subforms) (store-vars)))
	    `(,(car orig-access-form) ,@(subforms)))))


;;; SETF  --  Public
;;;
;;;    Except for atoms, we always call GET-SETF-METHOD, since it has some
;;; non-trivial semantics.  But when there is a setf inverse, and G-S-M uses
;;; it, then we return a call to the inverse, rather than returning a hairy let
;;; form.  This is probably important mainly as a convenince in allowing the
;;; use of setf inverses without the full interpreter.
;;;
(defmacro setf (&rest args &environment env)
  "Takes pairs of arguments like SETQ.  The first is a place and the second
  is the value that is supposed to go into that place.  Returns the last
  value.  The place argument may be any of the access forms for which SETF
  knows a corresponding setting form."
  (let ((nargs (length args)))
    (cond
     ((= nargs 2)
      (let ((place (first args))
	    (value-form (second args)))
	(if (atom place)
	    `(setq ,place ,value-form)
	    (multiple-value-bind (dummies vals newval setter getter)
				 (get-setf-expansion place env)
	      (declare (ignore getter))
	      (let ((inverse (info setf inverse (car place))))
		(if (and inverse (eq inverse (car setter)))
		    `(,inverse ,@(cdr place) ,value-form)
		    `(let* (,@(mapcar #'list dummies vals))
		       (multiple-value-bind ,newval ,value-form
			 ,setter))))))))
     ((oddp nargs) 
      (error "Odd number of args to SETF."))
     (t
      (do ((a args (cddr a)) (l nil))
	  ((null a) `(progn ,@(nreverse l)))
	(setq l (cons (list 'setf (car a) (cadr a)) l)))))))

(defmacro psetf (&rest args &environment env)
  "This is to SETF as PSETQ is to SETQ.  Args are alternating place
  expressions and values to go into those places.  All of the subforms and
  values are determined, left to right, and only then are the locations
  updated.  Returns NIL."
  (collect ((let*-bindings) (mv-bindings) (setters))
    (do ((a args (cddr a)))
	((endp a))
      (if (endp (cdr a))
	  (simple-program-error "Odd number of args to PSETF."))
      (multiple-value-bind
	  (dummies vals newval setter getter)
	  (get-setf-expansion (car a) env)
	(declare (ignore getter))
	(let*-bindings (mapcar #'list dummies vals))
	(mv-bindings (list newval (cadr a)))
	(setters setter)))
    (labels ((thunk (let*-bindings mv-bindings)
	       (if let*-bindings
		   `(let* ,(car let*-bindings)
		      (multiple-value-bind ,@(car mv-bindings)
			,(thunk (cdr let*-bindings) (cdr mv-bindings))))
		   `(progn ,@(setters) nil))))
      (thunk (let*-bindings) (mv-bindings)))))

(defmacro shiftf (&rest args &environment env)
  "One or more SETF-style place expressions, followed by a single
   value expression.  Evaluates all of the expressions in turn, then
   assigns the value of each expression to the place on its left,
   returning the value of the leftmost."
  (when args
    (collect ((let*-bindings) (mv-bindings) (setters) (getters))
      ;; The last arg isn't necessarily a place, so we have to handle
      ;; that separately.
      (dolist (arg (butlast args))
	(multiple-value-bind
	      (temps subforms store-vars setter getter)
	    (get-setf-expansion arg env)
	  (loop
	      for temp in temps
	      for subform in subforms
	      do (let*-bindings `(,temp ,subform)))
	  (mv-bindings store-vars)
	  (setters setter)
	  (getters getter)))
      ;; Handle the last arg specially here.  Just put something to
      ;; force the setter so the setter for the previous var gets set,
      ;; and the getter is just the last arg itself.
      (setters nil)
      (getters (car (last args)))
	
      (labels ((thunk (mv-bindings getters)
		 (if mv-bindings
		     `((multiple-value-bind
			     ,(car mv-bindings)
			   ,(car getters)
			 ,@(thunk (cdr mv-bindings) (cdr getters))))
		     `(,@(butlast (setters))))))
	`(let* ,(let*-bindings)
	  (multiple-value-bind ,(car (mv-bindings))
	      ,(car (getters))
	    ,@(thunk (mv-bindings) (cdr (getters)))
	    (values ,@(car (mv-bindings)))))))))

(defmacro rotatef (&rest args &environment env)
  "Takes any number of SETF-style place expressions.  Evaluates all of the
   expressions in turn, then assigns to each place the value of the form to
   its right.  The rightmost form gets the value of the leftmost.
   Returns NIL."
  (when args
    (collect ((let*-bindings) (mv-bindings) (setters) (getters))
      (dolist (arg args)
	(multiple-value-bind
	    (temps subforms store-vars setter getter)
	    (get-setf-expansion arg env)
	  (loop
	    for temp in temps
	    for subform in subforms
	    do (let*-bindings `(,temp ,subform)))
	  (mv-bindings store-vars)
	  (setters setter)
	  (getters getter)))
      (setters nil)
      (getters (car (getters)))
      (labels ((thunk (mv-bindings getters)
		 (if mv-bindings
		     `((multiple-value-bind
			   ,(car mv-bindings)
			   ,(car getters)
			 ,@(thunk (cdr mv-bindings) (cdr getters))))
		     (setters))))
	`(let* ,(let*-bindings)
	   ,@(thunk (mv-bindings) (cdr (getters))))))))


(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  "Creates a new read-modify-write macro like PUSH or INCF."
  (let ((other-args nil)
	(rest-arg nil)
	(env (gensym))
	(reference (gensym)))
	     
    ;; Parse out the variable names and rest arg from the lambda list.
    (do ((ll lambda-list (cdr ll))
	 (arg nil))
	((null ll))
      (setq arg (car ll))
      (cond ((eq arg '&optional))
	    ((eq arg '&rest)
	     (if (symbolp (cadr ll))
		 (setq rest-arg (cadr ll))
		 (error "Non-symbol &rest arg in definition of ~S." name))
	     (if (null (cddr ll))
		 (return nil)
		 (error "Illegal stuff after &rest arg in Define-Modify-Macro.")))
	    ((memq arg '(&key &allow-other-keys &aux))
	     (error "~S not allowed in Define-Modify-Macro lambda list." arg))
	    ((symbolp arg)
	     (push arg other-args))
	    ((and (listp arg) (symbolp (car arg)))
	     (push (car arg) other-args))
	    (t (error "Illegal stuff in lambda list of Define-Modify-Macro."))))
    (setq other-args (nreverse other-args))
    `(defmacro ,name (,reference ,@lambda-list &environment ,env)
       ,doc-string
       (multiple-value-bind (dummies vals newval setter getter)
	 (get-setf-method ,reference ,env)
	 (do ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil (cons (list (car d) (car v)) let-list)))
	     ((null d)
	      (push 
	       (list (car newval)
		     ,(if rest-arg
			  `(list* ',function getter ,@other-args ,rest-arg)
			  `(list ',function getter ,@other-args)))
	       let-list)
	      `(let* ,(nreverse let-list)
		 ,setter)))))))

(defmacro push (obj place &environment env)
  "Takes an object and a location holding a list.  Conses the object onto
  the list, returning the modified list.  OBJ is evaluated before PLACE."

  ;; This special case for place being a symbol isn't strictly needed.
  ;; It's so we can do push (and pushnew) with a kernel.core.
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (cons ,obj ,place))
      (multiple-value-bind (dummies vals newval setter getter)
	  (get-setf-expansion place env)
	(cond
	  ((cdr newval)
	   ;; Handle multiple values
	   (let ((g (mapcar #'(lambda (x)
				(declare (ignore x))
				(gensym))
			    (rest obj))))
	     `(multiple-value-bind ,g
		  ,obj
		(let* (,@(mapcar #'list dummies vals))
		  (multiple-value-bind ,newval
		      (values ,@(mapcar #'(lambda (a b)
					     (list 'cons a b))
					 g (rest getter)))
		    ,setter)))))
	  (t
	   ;; A single value
	   (let ((g (gensym)))
	     `(let* ((,g ,obj)
		     ,@(mapcar #'list dummies vals)
		     (,@newval (cons ,g ,getter)))
	       ,setter)))))))

(defmacro pushnew (obj place &rest keys &environment env)
  "Takes an object and a location holding a list.  If the object is already
  in the list, does nothing.  Else, conses the object onto the list.  Returns
  NIL.  If there is a :TEST keyword, this is used for the comparison."
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (adjoin ,obj ,place ,@keys))
      (multiple-value-bind (vars vals stores setter getter)
	  (get-setf-expansion place env)
	(cond
	  ((cdr stores)
	   ;; Multiple values
	   (let ((g (mapcar #'(lambda (x)
				(declare (ignore x))
				(gensym))
			    (rest obj))))
	     `(multiple-value-bind ,g
		  ,obj
		(let* (,@(mapcar #'list vars vals))
		  (multiple-value-bind ,stores
		      (values ,@(mapcar #'(lambda (a b)
					    `(adjoin ,a ,b ,@keys))
					g (rest getter)))
		  ,setter)))))
	  (t
	   ;; Single value
	   (let ((g (gensym)))
	     `(let* ((,g ,obj)
		     ,@(mapcar #'list vars vals)
		     (,@stores (adjoin ,g ,getter ,@keys)))
		,setter)))))))

(defmacro pop (place &environment env)
  "The argument is a location holding a list.  Pops one item off the front
  of the list and returns it."
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(prog1 (car ,place)
	      (setq ,place (cdr ,place)))
      (multiple-value-bind (dummies vals newval setter getter)
	  (get-setf-method place env)
	(do* ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil))
	     ((null d)
	      (push (list (car newval) getter) let-list)
	      `(let* ,(nreverse let-list)
		(prog1 (car ,(car newval))
		  (setq ,(car newval) (cdr ,(car newval)))
		  ,setter)))
	  (push (list (car d) (car v)) let-list)))))


;;; we can't use DEFINE-MODIFY-MACRO because of ANSI 5.1.3
(defmacro incf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number. This number is
  incremented by the second argument, DELTA, which defaults to 1."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (+ ,getter ,d)))
         ,setter))))

(defmacro decf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number. This number is
  decremented by the second argument, DELTA, which defaults to 1."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-method place env)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (- ,getter ,d)))
         ,setter))))

(defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
  to hold a property list or ().  This list is destructively altered to
  remove the property specified by the indicator.  Returns T if such a
  property was present, NIL if not."
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (local1 (gensym))
	  (local2 (gensym)))
	 ((null d)
	  ;; See ANSI 5.1.3 for why we do out-of-order evaluation
	  (push (list ind-temp indicator) let-list)
	  (push (list (car newval) getter) let-list)
	  `(let* ,(nreverse let-list)
	     (do ((,local1 ,(car newval) (cddr ,local1))
		  (,local2 nil ,local1))
		 ((atom ,local1) nil)
	       (cond ((atom (cdr ,local1))
		      (error "Odd-length property list in REMF."))
		     ((eq (car ,local1) ,ind-temp)
		      (cond (,local2
			     (rplacd (cdr ,local2) (cddr ,local1))
			     (return t))
			    (t (setq ,(car newval) (cddr ,(car newval)))
			       ,setter
			       (return t))))))))
      (push (list (car d) (car v)) let-list))))


;;; The built-in DEFSETFs.

(defsetf car %rplaca)
(defsetf cdr %rplacd)
(defsetf caar (x) (v) `(%rplaca (car ,x) ,v))
(defsetf cadr (x) (v) `(%rplaca (cdr ,x) ,v))
(defsetf cdar (x) (v) `(%rplacd (car ,x) ,v))
(defsetf cddr (x) (v) `(%rplacd (cdr ,x) ,v))
(defsetf caaar (x) (v) `(%rplaca (caar ,x) ,v))
(defsetf cadar (x) (v) `(%rplaca (cdar ,x) ,v))
(defsetf cdaar (x) (v) `(%rplacd (caar ,x) ,v))
(defsetf cddar (x) (v) `(%rplacd (cdar ,x) ,v))
(defsetf caadr (x) (v) `(%rplaca (cadr ,x) ,v))
(defsetf caddr (x) (v) `(%rplaca (cddr ,x) ,v))
(defsetf cdadr (x) (v) `(%rplacd (cadr ,x) ,v))
(defsetf cdddr (x) (v) `(%rplacd (cddr ,x) ,v))
(defsetf caaaar (x) (v) `(%rplaca (caaar ,x) ,v))
(defsetf cadaar (x) (v) `(%rplaca (cdaar ,x) ,v))
(defsetf cdaaar (x) (v) `(%rplacd (caaar ,x) ,v))
(defsetf cddaar (x) (v) `(%rplacd (cdaar ,x) ,v))
(defsetf caadar (x) (v) `(%rplaca (cadar ,x) ,v))
(defsetf caddar (x) (v) `(%rplaca (cddar ,x) ,v))
(defsetf cdadar (x) (v) `(%rplacd (cadar ,x) ,v))
(defsetf cdddar (x) (v) `(%rplacd (cddar ,x) ,v))
(defsetf caaadr (x) (v) `(%rplaca (caadr ,x) ,v))
(defsetf cadadr (x) (v) `(%rplaca (cdadr ,x) ,v))
(defsetf cdaadr (x) (v) `(%rplacd (caadr ,x) ,v))
(defsetf cddadr (x) (v) `(%rplacd (cdadr ,x) ,v))
(defsetf caaddr (x) (v) `(%rplaca (caddr ,x) ,v))
(defsetf cadddr (x) (v) `(%rplaca (cdddr ,x) ,v))
(defsetf cdaddr (x) (v) `(%rplacd (caddr ,x) ,v))
(defsetf cddddr (x) (v) `(%rplacd (cdddr ,x) ,v))

(defsetf first %rplaca)
(defsetf second (x) (v) `(%rplaca (cdr ,x) ,v))
(defsetf third (x) (v) `(%rplaca (cddr ,x) ,v))
(defsetf fourth (x) (v) `(%rplaca (cdddr ,x) ,v))
(defsetf fifth (x) (v) `(%rplaca (cddddr ,x) ,v))
(defsetf sixth (x) (v) `(%rplaca (cdr (cddddr ,x)) ,v))
(defsetf seventh (x) (v) `(%rplaca (cddr (cddddr ,x)) ,v))
(defsetf eighth (x) (v) `(%rplaca (cdddr (cddddr ,x)) ,v))
(defsetf ninth (x) (v) `(%rplaca (cddddr (cddddr ,x)) ,v))
(defsetf tenth (x) (v) `(%rplaca (cdr (cddddr (cddddr ,x))) ,v))
(defsetf rest %rplacd)

(defsetf elt %setelt)
(defsetf aref %aset)
(defsetf row-major-aref %set-row-major-aref)
(defsetf svref %svset)
(defsetf char %charset)
(defsetf bit %bitset)
(defsetf schar %scharset)
(defsetf sbit %sbitset)
(defsetf %array-dimension %set-array-dimension)
(defsetf %raw-bits %set-raw-bits)
(defsetf symbol-value set)
(defsetf symbol-function fset)
(defsetf symbol-plist %set-symbol-plist)
(defsetf nth %setnth)
(defsetf fill-pointer %set-fill-pointer)
(defsetf search-list %set-search-list)

(defsetf sap-ref-8 %set-sap-ref-8)
(defsetf signed-sap-ref-8 %set-signed-sap-ref-8)
(defsetf sap-ref-16 %set-sap-ref-16)
(defsetf signed-sap-ref-16 %set-signed-sap-ref-16)
(defsetf sap-ref-32 %set-sap-ref-32)
(defsetf signed-sap-ref-32 %set-signed-sap-ref-32)
(defsetf sap-ref-64 %set-sap-ref-64)
(defsetf signed-sap-ref-64 %set-signed-sap-ref-64)
(defsetf sap-ref-sap %set-sap-ref-sap)
(defsetf sap-ref-single %set-sap-ref-single)
(defsetf sap-ref-double %set-sap-ref-double)
#+long-float
(defsetf sap-ref-long %set-sap-ref-long)

(define-setf-expander getf (place prop &optional default &environment env)
  (multiple-value-bind (temps values stores set get)
		       (get-setf-method place env)
    (let ((newval (gensym))
	  (ptemp (gensym))
	  (def-temp (if default (gensym))))
      (values `(,@temps ,ptemp ,@(if default `(,def-temp)))
	      `(,@values ,prop ,@(if default `(,default)))
	      `(,newval)
	      `(let ((,(car stores) (%putf ,get ,ptemp ,newval)))
		 ,set
		 ,newval)
	      `(getf ,get ,ptemp ,@(if default `(,def-temp)))))))

(define-setf-expander get (symbol prop &optional default)
  (let ((symbol-temp (gensym))
	(prop-temp (gensym))
	(def-temp (gensym))
	(newval (gensym)))
    (values `(,symbol-temp ,prop-temp ,@(if default `(,def-temp)))
	    `(,symbol ,prop ,@(if default `(,default)))
	    (list newval)
	    `(%put ,symbol-temp ,prop-temp ,newval)
	    `(get ,symbol-temp ,prop-temp ,@(if default `(,def-temp))))))

(define-setf-expander gethash (key hashtable &optional default)
  (let ((key-temp (gensym))
	(hashtable-temp (gensym))
	(default-temp (gensym))
	(new-value-temp (gensym)))
    (values
     `(,key-temp ,hashtable-temp ,@(if default `(,default-temp)))
     `(,key ,hashtable ,@(if default `(,default)))
     `(,new-value-temp)
     `(%puthash ,key-temp ,hashtable-temp ,new-value-temp)
     `(gethash ,key-temp ,hashtable-temp ,@(if default `(,default-temp))))))

(defsetf subseq (sequence start &optional (end nil)) (v)
  `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
	  ,v))


;;; Evil hack invented by the gnomes of Vassar Street (though not as evil as
;;; it used to be.)  The function arg must be constant, and is converted to an
;;; APPLY of ther SETF function, which ought to exist.
;;;
(define-setf-expander apply (function &rest args)
  (unless (and (listp function)
	       (= (list-length function) 2)
	       (eq (first function) 'function)
	       (symbolp (second function)))
    (error "Setf of Apply is only defined for function args like #'symbol."))
  (let ((function (second function))
	(new-var (gensym))
	(vars nil))
    (dolist (x args)
      (declare (ignore x))
      (push (gensym) vars))
    (values vars args (list new-var)
	    `(apply #'(setf ,function) ,new-var ,@vars)
	    `(apply #',function ,@vars))))


;;; Special-case a BYTE bytespec so that the compiler can recognize it.
;;;
(define-setf-expander ldb (bytespec place &environment env)
  "The first argument is a byte specifier.  The second is any place form
  acceptable to SETF.  Replaces the specified byte of the number in this
  place with bits from the low-order end of the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
    (if (and (consp bytespec) (eq (car bytespec) 'byte))
	(let ((n-size (gensym))
	      (n-pos (gensym))
	      (n-new (gensym)))
	  (values (list* n-size n-pos dummies)
		  (list* (second bytespec) (third bytespec) vals)
		  (list n-new)
		  `(let ((,(car newval) (dpb ,n-new (byte ,n-size ,n-pos)
					     ,getter)))
		     ,setter
		     ,n-new)
		  `(ldb (byte ,n-size ,n-pos) ,getter)))
	(let ((btemp (gensym))
	      (gnuval (gensym)))
	  (values (cons btemp dummies)
		  (cons bytespec vals)
		  (list gnuval)
		  `(let ((,(car newval) (dpb ,gnuval ,btemp ,getter)))
		     ,setter
		     ,gnuval)
		  `(ldb ,btemp ,getter))))))


(define-setf-expander mask-field (bytespec place &environment env)
  "The first argument is a byte specifier.  The second is any place form
  acceptable to SETF.  Replaces the specified byte of the number in this place
  with bits from the corresponding position in the new value."
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
    (let ((btemp (gensym))
	  (gnuval (gensym)))
      (values (cons btemp dummies)
	      (cons bytespec vals)
	      (list gnuval)
	      `(let ((,(car newval) (deposit-field ,gnuval ,btemp ,getter)))
		 ,setter
		 ,gnuval)
	      `(mask-field ,btemp ,getter)))))


(define-setf-expander the (type place &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
      (values dummies
	      vals
	      newval
	      (subst `(the ,type ,(car newval)) (car newval) setter)
	      `(the ,type ,getter))))

(define-setf-expander values (&rest places &environment env)
  (collect ((setters) (getters))
    (let ((all-dummies '())
	  (all-vals '())
	  (newvals '()))
      (dolist (place places)
	(multiple-value-bind (dummies vals newval setter getter)
	    (get-setf-expansion place env)
	  ;; ANSI CL 5.1.2.3 explains that extra places are set to
	  ;; nil.
	  (setf all-dummies (append all-dummies dummies (cdr newval)))
	  (setf all-vals (append all-vals vals
				 (mapcar (constantly nil) (cdr newval))))
	  (setf newvals (append newvals (list (car newval))))
	  (setters setter)
	  (getters getter)))
      (values all-dummies all-vals newvals
	      `(values ,@(setters)) `(values ,@(getters))))))


;;;; CASE, TYPECASE, & Friends.

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; CASE-BODY returns code for all the standard "case" macros.  Name is the
;;; macro name, and keyform is the thing to case on.  Multi-p indicates whether
;;; a branch may fire off a list of keys; otherwise, a key that is a list is
;;; interpreted in some way as a single key.  When multi-p, test is applied to
;;; the value of keyform and each key for a given branch; otherwise, test is
;;; applied to the value of keyform and the entire first element, instead of
;;; each part, of the case branch.  When errorp, no t or otherwise branch is
;;; permitted, and an ERROR form is generated.  When proceedp, it is an error
;;; to omit errorp, and the ERROR form generated is executed within a
;;; RESTART-CASE allowing keyform to be set and retested.
;;;
;;; If ALLOW-OTHERWISE, then we allow T and OTHERWISE clauses and also
;;; generate an ERROR form.  (This is for CCASE and ECASE which allow
;;; using T and OTHERWISE as regular keys.)
;;;
(defun case-body (name keyform cases multi-p test errorp proceedp &optional allow-otherwise)
  (let ((keyform-value (gensym))
	(clauses ())
	(keys ()))
    (do* ((case-list cases (cdr case-list))
	  (case (first case-list) (first case-list)))
	 ((null case-list))
      (cond ((atom case)
	     (error "~S -- Bad clause in ~S." case name))
	    ((and (not allow-otherwise)
		  (memq (car case) '(t otherwise)))
	     (cond ((null (cdr case-list))
		    ;; The CLHS says OTHERWISE clause is an OTHERWISE clause
		    ;; only if it's the last case.  Otherwise, it's just a
		    ;; normal clause.
		    (if errorp
			(error "No default clause allowed in ~S: ~S" name case)
			(push `(t nil ,@(rest case)) clauses)))
		   ((and (eq name 'case))
		    (error "T and OTHERWISE may not be used as key designators for ~A" name))
		   ((eq (first case) t)
		    ;; The key T is normal clause, because it's not
		    ;; the last clause.
		    (push (first case) keys)
		    (push `((,test ,keyform-value
			    ',(first case)) nil ,@(rest case)) clauses))))
	    ((and multi-p (listp (first case)))
	     (setf keys (append (first case) keys))
	     (push `((or ,@(mapcar #'(lambda (key)
				       `(,test ,keyform-value ',key))
				   (first case)))
		     nil ,@(rest case))
		   clauses))
	    (t
	     (when (and allow-otherwise
			(memq (car case) '(t otherwise)))
	       (warn "Bad style to use T or OTHERWISE in ECASE or CCASE"))
	     (push (first case) keys)
	     (push `((,test ,keyform-value
			    ',(first case)) nil ,@(rest case)) clauses))))
    (case-body-aux name keyform keyform-value clauses keys errorp proceedp
		   allow-otherwise
		   `(,(if multi-p 'member 'or) ,@keys))))

;;; CASE-BODY-AUX provides the expansion once CASE-BODY has groveled all the
;;; cases.  Note: it is not necessary that the resulting code signal
;;; case-failure conditions, but that's what KMP's prototype code did.  We call
;;; CASE-BODY-ERROR, because of how closures are compiled.  RESTART-CASE has
;;; forms with closures that the compiler causes to be generated at the top of
;;; any function using the case macros, regardless of whether they are needed.
;;;
(defun case-body-aux (name keyform keyform-value clauses keys
		      errorp proceedp allow-otherwise expected-type)
  (if proceedp
      (let ((block (gensym))
	    (again (gensym)))
	`(let ((,keyform-value ,keyform))
	   (block ,block
	     (tagbody
	      ,again
	      (return-from
	       ,block
	       (cond ,@(nreverse clauses)
		     (t
		      (setf ,keyform-value
			    (setf ,keyform
				  (case-body-error
				   ',name ',keyform ,keyform-value
				   ',expected-type ',keys)))
		      (go ,again))))))))
      `(let ((,keyform-value ,keyform))
	 ,keyform-value ; prevent warnings when key not used eg (case key (t))
	 (cond
	  ,@(nreverse clauses)
	  ,@(if (or errorp allow-otherwise)
		`((t (error 'conditions::case-failure
			    :name ',name
			    :datum ,keyform-value
			    :expected-type ',expected-type
			    :possibilities ',keys))))))))

); eval-when

(defun case-body-error (name keyform keyform-value expected-type keys)
  (restart-case
      (error 'conditions::case-failure
	     :name name
	     :datum keyform-value
	     :expected-type expected-type
	     :possibilities keys)
    (store-value (value)
      :report (lambda (stream)
		(format stream "Supply a new value for ~S." keyform))
      :interactive read-evaluated-form
      value)))


(defmacro case (keyform &body cases)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'eql nil nil))

(defmacro ccase (keyform &body cases)
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If none of the keys matches then a correctable error is
  signalled."
  (case-body 'ccase keyform cases t 'eql nil t t))

(defmacro ecase (keyform &body cases)
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If none of the keys matches then an error is signalled."
  (case-body 'ecase keyform cases t 'eql nil nil t))

(defmacro typecase (keyform &body cases)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body 'typecase keyform cases nil 'typep nil nil))

(defmacro ctypecase (keyform &body cases)
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep nil t t))

(defmacro etypecase (keyform &body cases)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep nil nil t))


;;;; ASSERT and CHECK-TYPE.

;;; ASSERT is written this way, to call ASSERT-ERROR, because of how closures
;;; are compiled.  RESTART-CASE has forms with closures that the compiler
;;; causes to be generated at the top of any function using ASSERT, regardless
;;; of whether they are needed.
;;;
(defmacro assert (test-form &optional places datum &rest arguments)
  "Signals an error if the value of test-form is nil.  Continuing from this
   error using the CONTINUE restart will allow the user to alter the value of
   some locations known to SETF, starting over with test-form.  Returns nil."
  `(loop
     (when ,test-form (return nil))
     (assert-error ',test-form ',places ,datum ,@arguments)
     ,@(mapcar #'(lambda (place)
		   `(setf ,place (assert-prompt ',place ,place)))
	       places)))

(defun assert-error (assertion places datum &rest arguments)
  (let ((cond (if datum
		  (conditions::coerce-to-condition
		   datum arguments
		   'simple-error 'error)
		  (make-condition 'simple-error
				  :format-control "The assertion ~S failed."
				  :format-arguments (list assertion)))))
  (restart-case (error cond)
    (continue ()
      :report (lambda (stream) (assert-report places stream))
      nil))))


(defun assert-report (names stream)
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}."
	      (length names) names)
      (format stream ".")))

(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~
		  ~%Do you want to supply a new value? "
		   name value)
	 (format *query-io* "~&Type a form to be evaluated:~%")
	 (flet ((read-it () (eval (read *query-io*))))
	   (if (symbolp name) ;help user debug lexical variables
	       (progv (list name) (list value) (read-it))
	       (read-it))))
	(t value)))


;;; CHECK-TYPE is written this way, to call CHECK-TYPE-ERROR, because of how
;;; closures are compiled.  RESTART-CASE has forms with closures that the
;;; compiler causes to be generated at the top of any function using
;;; CHECK-TYPE, regardless of whether they are needed.  Because it would be
;;; nice if this were cheap to use, and some things can't afford this excessive
;;; consing (e.g., READ-CHAR), we bend backwards a little.
;;;

(defmacro check-type (place type &optional type-string)
  "Signals an error of type type-error if the contents of place are not of the
   specified type.  If an error is signaled, this can only return if
   STORE-VALUE is invoked.  It will store into place and start over."
  (let ((place-value (gensym)))
    `(loop
       (let ((,place-value ,place))
	 (when (typep ,place-value ',type) (return nil))
	 (setf ,place
	       (check-type-error ',place ,place-value ',type ,type-string))))))

(defun check-type-error (place place-value type type-string)
  (let ((cond (if type-string
		  (make-condition 'simple-type-error
				  :datum place-value :expected-type type
				  :format-control
				  "The value of ~S is ~S, which is not ~A."
				  :format-arguments
				  (list place place-value type-string))
		  (make-condition 'simple-type-error
				  :datum place-value :expected-type type
				  :format-control
				  "The value of ~S is ~S, which is not of type ~S."
				  :format-arguments
				  (list place place-value type)))))
    (restart-case (error cond)
      (store-value (value)
	:report (lambda (stream)
		  (format stream "Supply a new value of ~S."
			  place))
	:interactive read-evaluated-form
	value))))

;;; READ-EVALUATED-FORM is used as the interactive method for restart cases
;;; setup by the Common Lisp "casing" (e.g., CCASE and CTYPECASE) macros
;;; and by CHECK-TYPE.
;;;
(defun read-evaluated-form ()
  (format *query-io* "~&Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))


;;;; With-XXX
(defmacro with-open-file ((var &rest open-args) &parse-body (forms decls))
  "Bindspec is of the form (Stream File-Name . Options).  The file whose
   name is File-Name is opened using the Options and bound to the variable
   Stream. If the call to open is unsuccessful, the forms are not
   evaluated.  The Forms are executed, and when they 
   terminate, normally or otherwise, the file is closed."
  (let ((abortp (gensym)))
    `(let ((,var (open ,@open-args))
	   (,abortp t))
       ,@decls
       (unwind-protect
	   (multiple-value-prog1
	       (progn ,@forms)
	     (setq ,abortp nil))
	 (when ,var
	   (close ,var :abort ,abortp))))))


(defmacro with-open-stream ((var stream) &parse-body (forms decls))
  "The form stream should evaluate to a stream.  VAR is bound
   to the stream and the forms are evaluated as an implicit
   progn.  The stream is closed upon exit."
  (let ((abortp (gensym)))
    `(let ((,var ,stream)
	   (,abortp t))
       ,@decls
       (unwind-protect
	 (multiple-value-prog1
	  (progn ,@forms)
	  (setq ,abortp nil))
	 (when ,var
	   (close ,var :abort ,abortp))))))


(defmacro with-input-from-string ((var string &key index start end)
				  &parse-body (forms decls))
  "Binds the Var to an input stream that returns characters from String and
  executes the body.  See manual for details."
  ;; The once-only inhibits compiler note for unreachable code when 'end' is true.
  (once-only ((string string))
    `(let ((,var
	    ,(cond ((null end)
		    `(make-string-input-stream ,string ,(or start 0)))
		   ((symbolp end)
		    `(if ,end
			 (make-string-input-stream ,string ,(or start 0) ,end)
			 (make-string-input-stream ,string ,(or start 0))))
		   (t
		    `(make-string-input-stream ,string ,(or start 0) ,end)))))
       ,@decls
       (unwind-protect
	    (multiple-value-prog1
		(progn ,@forms)
	      ,@(when index
		   `((setf ,index (string-input-stream-current ,var)))))
	 (close ,var)))))


(defmacro with-output-to-string ((var &optional string &key element-type)
				 &parse-body (forms decls))
  "If STRING is specified, it must be a string with a fill pointer;
   the output is incrementally appended to the string (as if by use of
   VECTOR-PUSH-EXTEND)."
  (declare (ignore element-type))
  (if string
      `(let ((,var (make-fill-pointer-output-stream ,string)))
	 ,@decls
	 (unwind-protect
	   (progn ,@forms)
	   (close ,var)))
      `(let ((,var (make-string-output-stream)))
	 ,@decls
	 (unwind-protect
	   (progn ,@forms)
	   (close ,var))
	 (get-output-stream-string ,var))))


;;;; Iteration macros:

;; Helper for dotimes.  Extract any declarations for the dotimes
;; counter and create a similar declaration for our dummy loop
;; counter.  Skip over special declarations, though, because we don't
;; want to make the dummy counter special.
;;
;; Returns two values:
;; 1.  Set of declarations for the dotimes loop counter that would be
;;     suitable for use in the result-form of the loop,
;; 2.  Declarations suitable for the dummy loop counter.
(defun dotimes-extract-var-decls (var counter count decls)
  (let (var-decls counter-decls)
    (dolist (decl decls)
      (dolist (d (cdr decl))
	(when (member var (cdr d))
	  (cond ((eq (car d) 'type)
		 (push `(type ,(second d) ,var) var-decls)
		 (push `(type ,(second d) ,counter) counter-decls))
		((eq (car d) 'special)
		 ;; Declare var special, but not the counter
		 (push `(,(car d) ,var) var-decls))
		(t
		 (push `(,(car d) ,var) var-decls)
		 (push `(,(car d) ,counter) counter-decls))))))
    (unless counter-decls
      (setf counter-decls (if (numberp count)
			      `((type (integer 0 ,count) ,counter))
			      `((type unsigned-byte ,counter)))))
    (values (if var-decls
		`((declare ,@(nreverse var-decls)))
		nil)
	    `((declare ,@(nreverse counter-decls))))))
	      

;;; Make sure we iterate the given number of times, independent of
;;; what the body might do to the index variable.  We do this by
;;; repeatedly binding the var in the body and also in the result
;;; form.  We also spuriously reference the var in case the body or
;;; result form don't reference the var either.  (Mostly modeled on
;;; the dolist macro below.)
(defmacro dotimes ((var count &optional (result nil)) &body body)
  (let ((count-var (gensym "CTR-")))
    (multiple-value-bind (forms decls)
	(parse-body body nil nil)
      (multiple-value-bind (var-decls ctr-decls)
	  (dotimes-extract-var-decls var count-var count decls)
	(cond ((numberp count)
	       `(do ((,count-var 0 (1+ ,count-var)))
		    ((>= ,count-var ,count)
		     (let ((,var ,count-var))
		       ,@var-decls
		       ,var
		       ,result))
		  ,@ctr-decls
		  (let ((,var ,count-var))
		    ,@decls
		    ,var
		    (tagbody
		       ,@forms))))
	      (t (let ((v1 (gensym)))
		   `(do ((,count-var 0 (1+ ,count-var))
			 (,v1 ,count))
			((>= ,count-var ,v1)
			 (let ((,var ,count-var))
			   ,@var-decls
			   ,var
			   ,result))
		      ,@ctr-decls
		      (let ((,var ,count-var))
			,@decls
			,var
			(tagbody
			   ,@forms))))))))))


;;; We repeatedly bind the var instead of setting it so that we never give the
;;; var a random value such as NIL (which might conflict with a declaration).
;;; If there is a result form, we introduce a gratitous binding of the variable
;;; to NIL w/o the declarations, then evaluate the result form in that
;;; environment.  We spuriously reference the gratuitous variable, since we
;;; don't want to use IGNORABLE on what might be a special var.
;;;
(defmacro dolist ((var list &optional (result nil)) &body body)
  (multiple-value-bind (forms decls)
      (parse-body body nil nil)
    (let ((n-list (gensym)))
      `(do* ((,n-list ,list (cdr ,n-list)))
	    ((endp ,n-list)
	     ,@(if (constantp result)
		   `(,result)
		   `((let ((,var nil))
		       ,@decls
		       ,var
		       ,result))))
	(let ((,var (car ,n-list)))
	  ,@decls
	  (tagbody
	     ,@forms))))))


(defmacro do (varlist endlist &parse-body (body decls))
  "DO ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct.  Each Var is initialized in parallel to the value of the
  specified Init form.  On subsequent iterations, the Vars are assigned the
  value of the Step form (if any) in paralell.  The Test is evaluated before
  each evaluation of the body Forms.  When the Test is true, the Exit-Forms
  are evaluated as a PROGN, with the result being the value of the DO.  A block
  named NIL is established around the entire expansion, allowing RETURN to be
  used as an laternate exit mechanism."

  (do-do-body varlist endlist body decls 'let 'psetq 'do nil))


(defmacro do* (varlist endlist &parse-body (body decls))
  "DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct.  Each Var is initialized sequentially (like LET*) to the
  value of the specified Init form.  On subsequent iterations, the Vars are
  sequentially assigned the value of the Step form (if any).  The Test is
  evaluated before each evaluation of the body Forms.  When the Test is true,
  the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO.  A block named NIL is established around the entire expansion,
  allowing RETURN to be used as an laternate exit mechanism."
  (do-do-body varlist endlist body decls 'let* 'setq 'do* nil))


;;;; Miscellaneous macros:

(defmacro psetq (&rest pairs)
  "PSETQ {var value}*
   Set the variables to the values, like SETQ, except that assignments
   happen in parallel, i.e. no assignments take place until all the
   forms have been evaluated."
  ;; Given the possibility of symbol-macros, we delegate to PSETF
  ;; which knows how to deal with them, after checking that syntax is
  ;; compatible with PSETQ.
  (do ((pair pairs (cddr pair)))
      ((endp pair) `(psetf ,@pairs))
    (unless (symbolp (car pair))
      (error 'simple-program-error
             :format-control "variable ~S in PSETQ is not a SYMBOL"
             :format-arguments (list (car pair))))))


;;; LAMBDA -- from the ANSI spec.
;;;
(defmacro lambda (&whole form &rest bvl-decls-and-body)
  (declare (ignore bvl-decls-and-body))
  `#',form) 



;;;; With-Compilation-Unit:

;;; True if we are within a WITH-COMPILATION-UNIT form, which normally causes
;;; nested uses to be NOOPS.
;;;
(defvar *in-compilation-unit* nil)

;;; Count of the number of compilation units dynamically enclosed by the
;;; current active WITH-COMPILATION-UNIT that were unwound out of.
;;;
(defvar *aborted-compilation-units*)

(declaim (special c::*context-declarations*))


;;; EVALUATE-DECLARATION-CONTEXT  --  Internal
;;;
;;;    Recursively descend the context form, returning true if this subpart
;;; matches the specified context.
;;;
(defun evaluate-declaration-context (context name parent)
  (multiple-value-bind (valid base)
      (valid-function-name-p name)
    (let ((package (and valid (symbolp base) (symbol-package base))))
      (if (atom context)
	  (multiple-value-bind (ignore how)
	      (if package
		  (find-symbol (symbol-name base) package)
		  (values nil nil))
	    (declare (ignore ignore))
	    (case context
	      (:internal (eq how :internal))
	      (:external (eq how :external))
	      (:uninterned (and (symbolp base) (not package)))
	      (:anonymous (not name))
	      (:macro (eq parent 'defmacro))
	      (:function (member parent '(defun labels flet function)))
	      (:global (member parent '(defun defmacro function)))
	      (:local (member parent '(labels flet)))
	      (t
	       (error "Unknown declaration context: ~S." context))))
	  (case (first context)
	    (:or
	     (loop for x in (rest context)
		   thereis (evaluate-declaration-context x name parent)))
	    (:and
	     (loop for x in (rest context)
		   always (evaluate-declaration-context x name parent)))
	    (:not
	     (evaluate-declaration-context (second context) name parent))
	    (:member
	     (member name (rest context) :test #'equal))
	    (:match
	     (let ((name (concatenate 'string "$" (string base) "$")))
	       (loop for x in (rest context)
		     thereis (search (string x) name))))
	    (:package
	     (and package
		  (loop for x in (rest context)
			thereis (eq (find-package (string x)) package))))
	    (t
	     (error "Unknown declaration context: ~S." context)))))))

  
;;; PROCESS-CONTEXT-DECLARATIONS  --  Internal
;;;
;;;    Given a list of context declaration specs, return a new value for
;;; C::*CONTEXT-DECLARATIONS*.
;;;
(defun process-context-declarations (decls)
  (append
   (mapcar
    #'(lambda (decl)
	(unless (>= (length decl) 2)
	  (error "Context declaration spec should have context and at ~
	  least one DECLARE form:~%  ~S" decl))
	#'(lambda (name parent)
	    (when (evaluate-declaration-context (first decl) name parent)
	      (rest decl))))
    decls)
   c::*context-declarations*))


;;; With-Compilation-Unit  --  Public
;;;
(defmacro with-compilation-unit (options &body body)
  "WITH-COMPILATION-UNIT ({Key Value}*) Form*
  This form affects compilations that take place within its dynamic extent.  It
  is intended to be wrapped around the compilation of all files in the same
  system.  These keywords are defined:
    :OVERRIDE Boolean-Form
        One of the effects of this form is to delay undefined warnings 
        until the end of the form, instead of giving them at the end of each
        compilation.  If OVERRIDE is NIL (the default), then the outermost
        WITH-COMPILATION-UNIT form grabs the undefined warnings.  Specifying
        OVERRIDE true causes that form to grab any enclosed warnings, even if
        it is enclosed by another WITH-COMPILATION-UNIT.
    :OPTIMIZE Decl-Form
        Decl-Form should evaluate to an OPTIMIZE declaration specifier.  This
        declaration changes the `global' policy for compilations within the
        body.
    :OPTIMIZE-INTERFACE Decl-Form
        Like OPTIMIZE, except that it specifies the value of the CMU extension
        OPTIMIZE-INTERFACE policy (which controls argument type and syntax
        checking.)
    :CONTEXT-DECLARATIONS List-of-Context-Decls-Form
        This is a CMU extension which allows compilation to be controlled
        by pattern matching on the context in which a definition appears.  The
        argument should evaluate to a list of lists of the form:
            (Context-Spec Declare-Form+)
        In the indicated context, the specified declare forms are inserted at
        the head of each definition.  The declare forms for all contexts that
	match are appended together, with earlier declarations getting
	predecence over later ones.  A simple example:
            :context-declarations
            '((:external (declare (optimize (safety 2)))))
        This will cause all functions that are named by external symbols to be
        compiled with SAFETY 2.  The full syntax of context specs is:
	:INTERNAL, :EXTERNAL
	    True if the symbols is internal (external) in its home package.
	:UNINTERNED
	    True if the symbol has no home package.
	:ANONYMOUS
	    True if the function doesn't have any interesting name (not
	    DEFMACRO, DEFUN, LABELS or FLET).
	:MACRO, :FUNCTION
	    :MACRO is a global (DEFMACRO) macro.  :FUNCTION is anything else.
	:LOCAL, :GLOBAL
	    :LOCAL is a LABELS or FLET.  :GLOBAL is anything else.
	(:OR Context-Spec*)
	    True in any specified context.
	(:AND Context-Spec*)
	    True only when all specs are true.
	(:NOT Context-Spec)
	    True when the spec is false.
        (:MEMBER Name*)
	    True when the name is one of these names (EQUAL test.)
	(:MATCH Pattern*)
	    True when any of the patterns is a substring of the name.  The name
	    is wrapped with $'s, so $FOO matches names beginning with FOO,
	    etc."
  (let ((override nil)
	(optimize nil)
	(optimize-interface nil)
	(context-declarations nil)
	(n-fun (gensym))
	(n-abort-p (gensym)))
    (when (oddp (length options))
      (error "Odd number of key/value pairs: ~S." options))
    (do ((opt options (cddr opt)))
	((null opt))
      (case (first opt)
	(:override
	 (setq override (second opt)))
	(:optimize
	 (setq optimize (second opt)))
	(:optimize-interface
	 (setq optimize-interface (second opt)))
	(:context-declarations
	 (setq context-declarations (second opt)))
	(t
	 (warn "Ignoring unknown option: ~S." (first opt)))))

    `(flet ((,n-fun ()
	      (let (,@(when optimize
			`((c::*default-cookie*
			   (c::process-optimize-declaration
			    ,optimize c::*default-cookie*))))
		    ,@(when optimize-interface
			`((c::*default-interface-cookie*
			   (c::process-optimize-declaration
			    ,optimize-interface
			    c::*default-interface-cookie*))))
		    ,@(when context-declarations
			`((c::*context-declarations*
			   (process-context-declarations
			    ,context-declarations)))))
		,@body)))
       (if (or ,override (not *in-compilation-unit*))
	   (let ((c::*undefined-warnings* nil)
		 (c::*compiler-error-count* 0)
		 (c::*compiler-warning-count* 0)
		 (c::*compiler-note-count* 0)
		 (*in-compilation-unit* t)
		 (*aborted-compilation-units* 0)
		 (,n-abort-p t))
	     (handler-bind ((c::parse-unknown-type
			     #'(lambda (c)
				 (c::note-undefined-reference
				  (kernel:parse-unknown-type-specifier c)
				  :type))))
	       (unwind-protect
		   (multiple-value-prog1
		       (,n-fun)
		     (setq ,n-abort-p nil))
		 (c::print-summary ,n-abort-p *aborted-compilation-units*))))
	   (let ((,n-abort-p t))
	     (unwind-protect
		 (multiple-value-prog1
		     (,n-fun)
		   (setq ,n-abort-p nil))
	       (when ,n-abort-p
		 (incf *aborted-compilation-units*))))))))
