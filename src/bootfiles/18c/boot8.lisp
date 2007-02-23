(in-package "C")

(defun do-defconstant-compile-time (name value doc)
  (unless (symbolp name)
    (compiler-error "Constant name is not a symbol: ~S." name))
  (when (eq name t)
    (compiler-error "Can't change T."))
  (when (eq name nil)
    (compiler-error "Nihil ex nihil (Can't change NIL)."))
  (when (keywordp name)
    (compiler-error "Can't change the value of keywords."))

  (let ((kind (info variable kind name)))
    (case kind
      (:constant
       (unless (equalp value (info variable constant-value name))
	 (compiler-warning "Redefining constant ~S as:~%  ~S"
			   name value)))
      (:global)
      (t
       (compiler-warning "Redefining ~(~A~) ~S to be a constant."
			 kind name))))

  (setf (info variable kind name) :constant)
  (setf (info variable where-from name) :defined)
  (setf (info variable constant-value name) value)
  (remhash name *free-variables*))

(in-package "LISP")

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
       (c::%%defconstant ',var ,val ',doc))))
