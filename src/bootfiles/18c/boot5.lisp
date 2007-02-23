(in-package "C")

(defun do-macro-compile-time (name def)
  (unless (symbolp name)
    (compiler-error "Macro name is not a symbol: ~S." name))

  (ecase (info function kind name)
    ((nil))
    (:function
     (remhash name *free-functions*)
     (undefine-function-name name)
     (compiler-warning
      "Defining ~S to be a macro when it was ~(~A~) to be a function."
      name (info function where-from name)))
    (:macro)
    (:special-form
     (compiler-error "Attempt to redefine special form ~S as a macro." name)))

  (setf (info function kind name) :macro)
  (setf (info function where-from name) :defined)
  (when *compile-time-define-macros*
    (setf (info function macro-function name) (coerce def 'function))))

(def-ir1-translator %defmacro ((name def lambda-list doc) start cont
			       :kind :function)
  (let ((name (eval name))
	(def (second def))) ; Don't want to make a function just yet...

    (let* ((*current-path* (revert-source-path 'defmacro))
	   (fun (ir1-convert-lambda def name 'defmacro)))
      (setf (leaf-name fun)
	    (concatenate 'string "DEFMACRO " (symbol-name name)))
      (setf (functional-arg-documentation fun) (eval lambda-list))

      (ir1-convert start cont `(%%defmacro ',name ,fun ,doc)))

    (when *compile-print*
      (compiler-mumble "Converted ~S.~%" name))))

(defun do-compiler-macro-compile-time (name def)
  (when (eq (info function kind name) :special-form)
    (compiler-error "Attempt to define a compiler-macro for special form ~S."
		    name))
  (when *compile-time-define-macros*
    (setf (info function compiler-macro-function name)
	  (coerce def 'function))))

(def-ir1-translator %define-compiler-macro ((name def lambda-list doc)
					    start cont
					    :kind :function)
  (let ((name (eval name))
	(def (second def))) ; Don't want to make a function just yet...
    (let* ((*current-path* (revert-source-path 'define-compiler-macro))
	   (fun (ir1-convert-lambda def name 'define-compiler-macro)))
      (setf (leaf-name fun)
	    (let ((*print-case* :upcase))
	      (format nil "DEFINE-COMPILER-MACRO ~S" name)))
      (setf (functional-arg-documentation fun) (eval lambda-list))

      (ir1-convert start cont `(%%define-compiler-macro ',name ,fun ,doc)))

    (when *compile-print*
      (compiler-mumble "Converted ~S.~%" name))))

(in-package "LISP")

(defmacro defmacro (name lambda-list &body body)
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
