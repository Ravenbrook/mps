
(setf lisp::*enable-package-locked-errors* nil)
(in-package :c)

;;; Used to record the source-location of definitions.
;;;
(define-info-class source-location)
(define-info-type source-location defvar (or form-numbers null) nil)


(in-package :lisp)
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
