;;; -*- Package: C -*-
(in-package :c)


;;; boot.lisp for proclaim/declaim fix. 3/3/2001

(setf (info function ir1-convert 'proclaim) nil)

;;; A version of this has to exist for the second build to work.

(defun %declaim (what)
  (proclaim what))

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

(defun proclaim (form)
  (unless (consp form)
    (error "Malformed PROCLAIM spec: ~S." form))
  
  (let ((kind (first form))
	(args (rest form)))
    (case kind
      (special
       (dolist (name args)
	 (unless (symbolp name)
	   (error "Variable name is not a symbol: ~S." name))
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
	   (when (typep class 'class)
	     (setf (class-state class) :sealed)
	     (let ((subclasses (class-subclasses class)))
	       (when subclasses
		 (do-hash (subclass layout subclasses)
		   (declare (ignore layout))
		   (setf (class-state subclass) :sealed))))))))
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
    ',var))
