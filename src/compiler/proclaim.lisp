;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/proclaim.lisp,v 1.43 2005/10/21 17:56:07 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains load-time support for declaration processing.  It is
;;; split off from the compiler so that the compiler doesn'thave to be in the
;;; cold load.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

(in-package "EXTENSIONS")
(export '(inhibit-warnings freeze-type optimize-interface constant-function
	  float-accuracy))
(in-package "KERNEL")
(export '(note-name-defined define-function-name undefine-function-name
	  *type-system-initialized* %note-type-defined))
(in-package "LISP")
(export '(declaim proclaim))
(in-package "C")
(export '(&more))

;;; True if the type system has been properly initialized, and thus is o.k. to
;;; use.
;;;
(defvar *type-system-initialized* nil)

;;; The Cookie holds information about the compilation environment for a node.
;;; See the Lexenv definition for a description of how it is used.
;;;
(defstruct cookie
  (speed nil :type (or (rational 0 3) null))
  (space nil :type (or (rational 0 3) null))
  (safety nil :type (or (rational 0 3) null))
  (cspeed nil :type (or (rational 0 3) null))
  (brevity nil :type (or (rational 0 3) null))
  (debug nil :type (or (rational 0 3) null))
  ;; float-accuracy is intended for x86 FPU.  It has no effect on
  ;; other architectures. float-accuracy controls the rounding-mode.
  ;; A value of 3 means that when calling out to C, we set the x86 FPU
  ;; rounding mode to 64-bits.  Otherwise the rounding-mode is left
  ;; unchanged.  By setting the rounding-mode to 53-bit,
  ;; double-float-epsilon really is double-float-epsilon.  However
  ;; calling out to C with 53-bit mode affects the accuracy of
  ;; floating-point functions, which normally want 64-bit.
  (float-accuracy 3 :type (or (rational 0 3) null)))


;;; The *default-cookie* represents the current global compiler policy
;;; information.  Whenever the policy is changed, we copy the structure so that
;;; old uses will still get the old values.  *default-interface-cookie* holds
;;; any values specified by an OPTIMIZE-INTERFACE declaration.
;;;
(declaim (type cookie *default-cookie* *default-interface-cookie*))
(defvar *default-cookie*)
(defvar *default-interface-cookie*)

;;; PROCLAIM-INIT -- sorta interface.
;;;
;;; %Initial-function (in lispinit) calls this after running all the
;;; initial top level forms to reset the cookies.  We also use it in place
;;; of supplying initial values in the DEFVARs above so that we don't
;;; have to put the initial default cookie in two places.
;;; 
(defun proclaim-init ()
  (setf *default-cookie*
	(make-cookie :safety 1 :speed 1 :space 1 :cspeed 1
		     :brevity 1 :debug 2
		     :float-accuracy 3))
  (setf *default-interface-cookie*
	(make-cookie)))
;;;
(proclaim-init)

;;; A list of UNDEFINED-WARNING structures representing the calls to unknown
;;; functions.  This is bound by WITH-COMPILATION-UNIT.
;;;
(defvar *undefined-warnings*)
(declaim (list *undefined-warnings*))

;;; NOTE-NAME-DEFINED  --  Interface
;;;
;;;    Delete any undefined warnings for Name and Kind.  We do the BOUNDP check
;;; because this function can be called when not in a compilation unit (as when
;;; loading top-level forms.)
;;;
(defun note-name-defined (name kind)
  (when (boundp '*undefined-warnings*)
    (setq *undefined-warnings*
	  (delete-if #'(lambda (x)
			 (and (equal (undefined-warning-name x) name)
			      (eq (undefined-warning-kind x) kind)))
		     *undefined-warnings*)))
  (undefined-value))


;;; Parse-Lambda-List  --  Interface
;;;
;;;    Break a lambda-list into its component parts.  We return eleven values:
;;;  1] A list of the required args.
;;;  2] A list of the optional arg specs.
;;;  3] True if a rest arg was specified.
;;;  4] The rest arg.
;;;  5] A boolean indicating whether keywords args are present.
;;;  6] A list of the keyword arg specs.
;;;  7] True if &allow-other-keys was specified.
;;;  8] A list of the &aux specifiers.
;;;  9] True if a more arg was specified.
;;; 10] The &more context var
;;; 11] The &more count var
;;;
;;; The top-level lambda-list syntax is checked for validity, but the arg
;;; specifiers are just passed through untouched.  If something is wrong, we
;;; use Compiler-Error, aborting compilation to the last recovery point.
;;;
(defun parse-lambda-list (list)
  (declare (list list)
	   (values list list boolean t boolean list boolean list boolean t t))
  (collect ((required)
	    (optional)
	    (keys)
	    (aux))
    (let ((restp nil)
	  (rest nil)
	  (morep nil)
	  (more-context nil)
	  (more-count nil)
	  (keyp nil)
	  (allowp nil)
	  (state :required))
      (dolist (arg list)
        ;; check for arguments that have the syntactic form of a
        ;; keyword argument without being a recognized lambda-list keyword
        (when (and (symbolp arg)
                   (let ((name (symbol-name arg)))
                     (and (/= (length name) 0)
                          (char= (char name 0) #\&))))
          (unless (member arg lambda-list-keywords)
            (compiler-note
             "~S uses lambda-list keyword naming convention, but is not a recognized lambda-list keyword."
             arg)))
	(if (member arg lambda-list-keywords)
	    (ecase arg
	      (&optional
	       (unless (eq state :required)
		 (compiler-error "Misplaced &optional in lambda-list: ~S." list))
	       (setq state '&optional))
	      (&rest
	       (unless (member state '(:required &optional))
		 (compiler-error "Misplaced &rest in lambda-list: ~S." list))
	       (setq state '&rest))
	      (&more
	       (unless (member state '(:required &optional))
		 (compiler-error "Misplaced &more in lambda-list: ~S." list))
	       (setq morep t  state '&more-context))
	      (&key
	       (unless (member state '(:required &optional :post-rest
						 :post-more))
		 (compiler-error "Misplaced &key in lambda-list: ~S." list))
	       (setq keyp t)
	       (setq state '&key))
	      (&allow-other-keys
	       (unless (eq state '&key)
		 (compiler-error "Misplaced &allow-other-keys in lambda-list: ~S." list))
	       (setq allowp t  state '&allow-other-keys))
	      (&aux
	       (when (member state '(&rest &more-context &more-count))
		 (compiler-error "Misplaced &aux in lambda-list: ~S." list))
	       (setq state '&aux)))
	    (case state
	      (:required (required arg))
	      (&optional (optional arg))
	      (&rest
	       (setq restp t  rest arg  state :post-rest))
	      (&more-context
	       (setq more-context arg  state '&more-count))
	      (&more-count
	       (setq more-count arg  state :post-more))
	      (&key (keys arg))
	      (&aux (aux arg))
	      (t
	       (compiler-error "Found garbage in lambda-list when expecting a keyword: ~S." arg)))))

      (when (eq state '&rest)
	(compiler-error "&rest not followed by required variable."))
      
      (values (required) (optional) restp rest keyp (keys) allowp (aux)
	      morep more-context more-count))))


;;; Check-Function-Name  --  Interface
;;;
;;;    Check that Name is a valid function name, returning the name if OK, and
;;; doing an error if not.  In addition to checking for basic well-formedness,
;;; we also check that symbol names are not NIL or the name of a special form.
;;;
(defun check-function-name (name)
  (typecase name
    (list
     (unless (valid-function-name-p name)
       (compiler-error "Illegal function name: ~S." name))
     name)
    (symbol
     (when (eq (info function kind name) :special-form)
       (compiler-error "Special form is an illegal function name: ~S." name))
     name)
    (t
     (compiler-error "Illegal function name: ~S." name))))


;;; NOTE-IF-SETF-FUNCTION-AND-MACRO  --  Interface
;;;
;;;    Called to do something about SETF functions that overlap with setf
;;; macros.  Perhaps we should interact with the user to see if the macro
;;; should be blown away, but for now just give a warning.  Due to the weak
;;; semantics of the (SETF FUNCTION) name, we can't assume that they aren't
;;; just naming a function (SETF FOO) for the heck of it.  Name is already
;;; known to be well-formed.
;;;
(defun note-if-setf-function-and-macro (name)
  (when (consp name)
    (when (or (info setf inverse name)
	      (info setf expander name))
      (compiler-warning
       "Defining as a SETF function a name that already has a SETF macro:~
       ~%  ~S"
       name)))
  (undefined-value))


;;; Define-Function-Name  --  Interface
;;;
;;;    Check the legality of a function name that is being introduced.
;;; -- If it names a macro, then give a warning and blast the macro
;;;    information.
;;; -- If it is a structure slot accessor, give a warning and blast the
;;;    structure. 
;;; -- Check for conflicting setf macros.
;;;
(defun define-function-name (name)
  (check-function-name name)
  (ecase (info function kind name)
    (:function
     (let ((for (info function accessor-for name)))
       (when for
	 (compiler-warning
	  "Undefining structure type:~%  ~S~@
	   so that this slot accessor can be redefined:~%  ~S"
	  (%class-name for) name)
	 (undefine-structure for)
	 (setf (info function kind name) :function))))
    (:macro
     (compiler-warning "~S previously defined as a macro." name)
     (setf (info function kind name) :function)
     (setf (info function where-from name) :assumed)
     (clear-info function macro-function name))
    ((nil)
     (setf (info function kind name) :function)))
  

  (note-if-setf-function-and-macro name)
  name)


;;; UNDEFINE-FUNCTION-NAME  --  Interface
;;;
;;;    Make Name no longer be a function name: clear everything back to the
;;; default.
;;;
(defun undefine-function-name (name)
  (when name
    (macrolet ((frob (type &optional val)
		 `(unless (eq (info function ,type name) ,val)
		    (setf (info function ,type name) ,val))))
      (frob info)
      (frob type (specifier-type 'function))
      (frob where-from :assumed)
      (frob inlinep)
      (frob kind)
      (frob accessor-for)
      (frob inline-expansion)
      (frob source-transform)
      (frob assumed-type)))
  (undefined-value))


;;; Process-Optimize-Declaration  --  Interface
;;;
;;;    Return a new cookie containing the policy information represented by the
;;; optimize declaration Spec.  Any parameters not specified are defaulted from
;;; Cookie.
;;;
(defun process-optimize-declaration (spec cookie)
  (declare (list spec) (type cookie cookie) (values cookie))
  (let ((res (copy-cookie cookie)))
    (dolist (quality (cdr spec))
      (let ((quality (if (atom quality) (list quality 3) quality)))
	(if (and (consp (cdr quality)) (null (cddr quality))
		 (typep (second quality) 'real) (<= 0 (second quality) 3))
	    (let ((value (rational (second quality))))
	      (case (first quality)
		(speed (setf (cookie-speed res) value))
		(space (setf (cookie-space res) value))
		(safety (setf (cookie-safety res) value))
		(compilation-speed (setf (cookie-cspeed res) value))
		((inhibit-warnings brevity) (setf (cookie-brevity res) value))
		((debug-info debug) (setf (cookie-debug res) value))
		(float-accuracy (setf (cookie-float-accuracy res) value))
		(t
		 (compiler-warning "Unknown optimization quality ~S in ~S."
				   (car quality) spec))))
	    (compiler-warning
	     "Malformed optimization quality specifier ~S in ~S."
	     quality spec))))
    res))


;;; DECLAIM  --  Public
;;;

(defmacro declaim (&rest specs)
  "DECLAIM Declaration*
  Do a declaration for the global environment."
  `(progn
     (eval-when (:load-toplevel :execute)
       ,@(mapcar #'(lambda (x)
		     `(proclaim ',x))
		 specs))
     (eval-when (:compile-toplevel)
       ,@(mapcar #'(lambda (x)
		     `(%declaim ',x))
		 specs))))

(defun %declaim (x)
  (proclaim x))

(defvar *proclamation-hooks* nil)

;;; PROCLAIM  --  Public
;;;
;;;    This function is the guts of proclaim, since it does the global
;;; environment updating.
;;;
(defun proclaim (form)
  (unless (consp form)
    (error "Malformed PROCLAIM spec: ~S." form))

  (when (boundp '*proclamation-hooks*)
    (dolist (hook *proclamation-hooks*)
      (funcall hook form)))
  
  (let ((kind (first form))
	(args (rest form)))
    (case kind
      (special
       (dolist (name args)
	 (unless (symbolp name)
	   (error "Variable name is not a symbol: ~S." name))
	 (unless (or (member (info variable kind name) '(:global :special))
		     ;; If we are still in cold-load, and the package system
		     ;; is not set up, the global db will claim all variables
		     ;; are keywords/constants, because all symbols have the
		     ;; same package nil.  Proceed normally in this case:
		     (null (symbol-package :end)))
	   (cond
	     ((eq name 'nil)
	      (error "Nihil ex nihil, can't declare ~S special." name))
	     ((eq name 't)
	      (error "Veritas aeterna, can't declare ~S special." name))
	     ((keywordp name)
	      (error "Can't declare ~S special, it is a keyword." name))
	     (t
	      (cerror "Proceed anyway."
		      "Trying to declare ~S special, which is ~A." name
		      (ecase (info variable kind name)
			(:constant "a constant")
			(:alien "an alien variable")
			(:macro "a symbol macro"))))))
	 (clear-info variable constant-value name)
	 (setf (info variable kind name) :special)))
      (type
       (when *type-system-initialized*
	 (let ((type (specifier-type (first args))))
	   (dolist (name (rest args))
	     (unless (symbolp name)
	       (error "Variable name is not a symbol: ~S." name))
	     (setf (info variable type name) type)
	     (setf (info variable where-from name) :declared)))))
      (ftype
       (when *type-system-initialized*
	 (let ((type (specifier-type (first args))))
	   (unless (csubtypep type (specifier-type 'function))
	     (error "Declared functional type is not a function type: ~S."
		    (first args)))
	   (dolist (name (rest args))
	     (cond ((info function accessor-for name)
		    (warn "Ignoring FTYPE declaration for slot accesor:~%  ~S"
			  name))
		   (t
		    (define-function-name name)
		    (note-name-defined name :function)
		    (setf (info function type name) type)
		    (setf (info function where-from name) :declared)))))))
      (freeze-type
       (dolist (type args)
	 (let ((class (specifier-type type)))
	   (when (typep class 'kernel::class)
	     (setf (%class-state class) :sealed)
	     (let ((subclasses (%class-subclasses class)))
	       (when subclasses
		 (do-hash (subclass layout subclasses)
		   (declare (ignore layout))
		   (setf (%class-state subclass) :sealed))))))))
      (function
       ;;
       ;; Handle old-style FUNCTION declaration, which is a shorthand for
       ;; FTYPE.
       (when *type-system-initialized*
	 (if (and (<= 2 (length args) 3) (listp (second args)))
	     (proclaim `(ftype (function . ,(rest args)) ,(first args)))
	     (proclaim `(type function . ,args)))))
      (optimize
       (setq *default-cookie*
	     (process-optimize-declaration form *default-cookie*)))
      (optimize-interface
       (setq *default-interface-cookie*
	     (process-optimize-declaration form *default-interface-cookie*)))
      ((inline notinline maybe-inline)
       (dolist (name args)
	 (define-function-name name)
	 (setf (info function inlinep name)
	       (case kind
		 (inline :inline)
		 (notinline :notinline)
		 (maybe-inline :maybe-inline)))))
      (constant-function
       (let ((info (make-function-info
		    :attributes (ir1-attributes movable foldable flushable
						unsafe))))
	 (dolist (name args)
	   (define-function-name name)
	   (setf (info function info name) info))))
      (declaration
       (dolist (decl args)
	 (unless (symbolp decl)
	   (error "Declaration to be RECOGNIZED is not a symbol: ~S." decl))
	 (when (info type kind decl)
	   (error "Declaration already names a type: ~S." decl))
	 (setf (info declaration recognized decl) t)))
      ((start-block end-block)) ; ignore.
      (t
       (cond ((member kind type-specifier-symbols)
	      (proclaim `(type . ,form)))
	     ((or (info type kind kind)
		  (and (consp kind) (info type translator (car kind))))
	      (proclaim `(type . ,form)))
	     ((not (info declaration recognized kind))
	      (warn "Unrecognized proclamation: ~S." form))))))
  (undefined-value))


;;; %NOTE-TYPE-DEFINED  --  Interface
;;;
;;;    Note that the type Name has been (re)defined, updating the undefined
;;; warnings and VALUES-SPECIFIER-TYPE cache.
;;; 
(defun %note-type-defined (name)
  (declare (symbol name))
  (note-name-defined name :type)
  (when (boundp 'kernel::*values-specifier-type-cache-vector*)
    (values-specifier-type-cache-clear))
  (undefined-value))


;;;; Dummy definitions of COMPILER-ERROR, etc.
;;;
;;;    Until the compiler is properly loaded, we make the compiler error
;;; functions synonyms for the obvious standard error function.
;;;

(defun compiler-error (string &rest args)
  (apply #'error string args))

(defun compiler-warning (string &rest args)
  (apply #'warn string args))

(defun compiler-note (string &rest args)
  (apply #'warn string args))

(defun compiler-error-message (string &rest args)
  (apply #'warn string args))

