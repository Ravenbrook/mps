(in-package "EXTENSIONS")

(export '(define-function-name-syntax valid-function-name-p))

(in-package "LISP")

;;;; Function names.

(defvar *valid-function-names* ())

(defun %define-function-name-syntax (name syntax-checker)
  (let ((found (assoc name *valid-function-names* :test #'eq)))
    (if found
	(setf (cdr found) syntax-checker)
	(setq *valid-function-names*
	      (acons name syntax-checker *valid-function-names*)))))

(defmacro define-function-name-syntax (name (var) &body body)
  "Define (NAME ...) to be a valid function name whose syntax is checked
  by BODY.  In BODY, VAR is bound to an actual function name of the
  form (NAME ...) to check.  BODY should return two values.
  First value true means the function name is valid.  Second value
  is the name, a symbol, of the function for use in the BLOCK of DEFUNs
  and in similar situations."
  (let ((syntax-checker (symbolicate '%check- name '-function-name)))
    `(progn
       (defun ,syntax-checker (,var) ,@body)
       (%define-function-name-syntax ',name #',syntax-checker))))

(defun valid-function-name-p (name)
  "First value is true if NAME has valid function name syntax.
  Second value is the name, a symbol, to use as a block name in DEFUNs
  and in similar situations."
  (typecase name
    (cons
     (when (and (symbolp (car name))
		(consp (cdr name)))
       (let ((syntax-checker (cdr (assoc (car name) *valid-function-names*
					 :test #'eq))))
	 (when syntax-checker
	   (funcall syntax-checker name)))))
    (symbol (values t name))
    (otherwise nil)))

(define-function-name-syntax setf (name)
  (destructuring-bind (setf fn &rest rest) name
    (declare (ignore setf))
    (when (null rest)
      (typecase fn
	(symbol
	 (values t fn))
	(cons
	 (unless (eq 'setf (car fn))
	   (valid-function-name-p fn)))))))
