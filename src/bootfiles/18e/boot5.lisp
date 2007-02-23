;;;
;;; Bootfile for changes in macro lambda-list processing.
;;; Destructuring in &REST, &BODY, and &WHOLE.  Addition
;;; of EXT:&PARSE-BODY.
;;;

(in-package :lisp)

(defun parse-defmacro-lambda-list
       (lambda-list arg-list-name name error-kind error-fun
		    &optional top-level env-illegal env-arg-name)
  (let ((path (if top-level `(cdr ,arg-list-name) arg-list-name))
	(now-processing :required)
	(maximum 0)
	(minimum 0)
	(keys ())
	rest-name restp allow-other-keys-p env-arg-used)
    ;; This really strange way to test for '&whole is neccessary because member
    ;; does not have to work on dotted lists, and dotted lists are legal
    ;; in lambda-lists.
    (when (and (do ((list lambda-list (cdr list)))
		   ((atom list) nil)
		 (when (eq (car list) '&whole) (return t)))
	       (not (eq (car lambda-list) '&whole)))
      (simple-program-error "&Whole must appear first in ~S lambda-list."
                            error-kind))
    (do ((rest-of-args lambda-list (cdr rest-of-args)))
	((atom rest-of-args)
	 (cond ((null rest-of-args) nil)
	       ;; Varlist is dotted, treat as &rest arg and exit.
	       (t (push-let-binding rest-of-args path nil)
		  (setf restp :dotted))))
      (let ((var (car rest-of-args)))
	(cond ((eq var '&whole)
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      ;; For compiler macros, we have to do something
		      ;; special in case the form has a car eq to
		      ;; funcall, as specified in the CLHS.  In this
		      ;; case, we skip over the funcall and pretend
		      ;; that the rest of the form is the actual form.
		      ;;
		      ;; This is a gross hack because we look at
		      ;; error-kind to figure out if we're defining a
		      ;; compiler macro or not.
		      (when (eq error-kind 'define-compiler-macro)
			(push-let-binding arg-list-name arg-list-name
			  t
			  `(progn
			    (not (and (listp ,arg-list-name)
				  (eq 'funcall (car ,arg-list-name)))))
			  `(progn
			    (setf ,arg-list-name (cdr ,arg-list-name)))))
		      (push-let-binding (car rest-of-args) arg-list-name nil))
		     ((and (cdr rest-of-args) (consp (cadr rest-of-args)))
		      (pop rest-of-args)
		      (let* ((destructuring-lambda-list (car rest-of-args))
			     (sub (gensym "WHOLE-SUBLIST")))
			(push-sub-list-binding
			 sub arg-list-name destructuring-lambda-list
			 name error-kind error-fun)
			(parse-defmacro-lambda-list
			 destructuring-lambda-list sub name error-kind error-fun)))
		     (t
		      (defmacro-error "&WHOLE" error-kind name))))
	      ((eq var '&environment)
	       (cond (env-illegal
		      (simple-program-error "&environment not valid with ~S."
                                            error-kind))
		     ((not top-level)
		      (simple-program-error
		       "&environment only valid at top level of lambda-list.")))
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (push-let-binding (car rest-of-args) env-arg-name nil)
		      (setf env-arg-used t))
		     (t
		      (defmacro-error "&ENVIRONMENT" error-kind name))))
	      ;;
	      ;; This branch implements an extension to Common Lisp
	      ;; that was formerly implemented for &body.  In place of
	      ;; a symbol following &body, there could be a list of up
	      ;; to three elements which will be bound to the body,
	      ;; declarations, and doc-string of the body.
	      ((eq var 'ext::&parse-body)
	       (unless (and (cdr rest-of-args)
			    (consp (cadr rest-of-args))
			    (symbolp (caadr rest-of-args)))
		 (simple-program-error "Invalid ~a" 'ext::&parse-body))
		(setf rest-of-args (cdr rest-of-args))
		(setf restp t)
		(let ((body-name (caar rest-of-args))
		      (declarations-name (cadar rest-of-args))
		      (doc-string-name (caddar rest-of-args))
		      (parse-body-values (gensym)))
		  (push-let-binding
		   parse-body-values
		   `(multiple-value-list
		     (parse-body ,path ,env-arg-name
				 ,(not (null doc-string-name))))
		   t)
		  (setf env-arg-used t)
		  (when body-name
		    (push-let-binding body-name
				      `(car ,parse-body-values) nil))
		  (when declarations-name
		    (push-let-binding declarations-name
				      `(cadr ,parse-body-values) nil))
		  (when doc-string-name
		    (push-let-binding doc-string-name
				      `(caddr ,parse-body-values) nil))))
	      ;;
	      ((member var '(&rest &body))
	       (cond ((and (cddr rest-of-args)
			   (not (member (caddr rest-of-args) lambda-list-keywords)))
		      (defmacro-error (symbol-name var) error-kind name))
		     ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setf rest-of-args (cdr rest-of-args))
		      (setf restp t)
		      (push-let-binding (car rest-of-args) path nil))
		     ((and (cdr rest-of-args) (consp (cadr rest-of-args)))
		      (pop rest-of-args)
		      (setq restp t)
		      (let* ((destructuring-lambda-list (car rest-of-args))
			     (sub (gensym "REST-SUBLIST")))
			(push-sub-list-binding sub path destructuring-lambda-list
			 name error-kind error-fun)
			(parse-defmacro-lambda-list
			 destructuring-lambda-list sub name error-kind error-fun)))
		     (t
		      (defmacro-error (symbol-name var) error-kind name))))
	      ((eq var '&optional)
	       (setf now-processing :optionals))
	      ((eq var '&key)
	       (setf now-processing :keywords)
	       (setf rest-name (gensym "KEYWORDS-"))
	       (push rest-name *ignorable-vars*)
	       (setf restp t)
	       (push-let-binding rest-name path t))
	      ((eq var '&allow-other-keys)
	       (setf allow-other-keys-p t))
	      ((eq var '&aux)
	       (setf now-processing :auxs))
	      ((listp var)
	       (case now-processing
		 (:required
		  (let ((sub-list-name (gensym "SUBLIST-")))
		    (push-sub-list-binding sub-list-name `(car ,path) var
					   name error-kind error-fun)
		    (parse-defmacro-lambda-list var sub-list-name name
						error-kind error-fun))
		  (setf path `(cdr ,path))
		  (incf minimum)
		  (incf maximum))
		 (:optionals
		  (when (> (length var) 3)
		    (cerror "Ignore extra noise."
			    "More than variable, initform, and suppliedp ~
			    in &optional binding - ~S"
			    var))
		  (push-optional-binding (car var) (cadr var) (caddr var)
					 `(not (null ,path)) `(car ,path)
					 name error-kind error-fun)
		  (setf path `(cdr ,path))
		  (incf maximum))
		 (:keywords
		  (let* ((keyword-given (consp (car var)))
			 (variable (if keyword-given
				       (cadar var)
				       (car var)))
			 (keyword (if keyword-given
				      (caar var)
				      (make-keyword variable)))
			 (supplied-p (caddr var)))
		    (push-optional-binding variable (cadr var) supplied-p
					   `(keyword-supplied-p ',keyword
								,rest-name)
					   `(lookup-keyword ',keyword
							    ,rest-name)
					   name error-kind error-fun)
		    (push keyword keys)))
		 (:auxs (push-let-binding (car var) (cadr var) nil))))
	      ((symbolp var)
	       (case now-processing
		 (:required
		  (incf minimum)
		  (incf maximum)
		  (push-let-binding var `(car ,path) nil)
		  (setf path `(cdr ,path)))
		 (:optionals
		  (incf maximum)
		  (push-let-binding var `(car ,path) nil `(not (null ,path)))
		  (setf path `(cdr ,path)))
		 (:keywords
		  (let ((key (make-keyword var)))
		    (push-let-binding var `(lookup-keyword ,key ,rest-name)
				      nil)
		    (push key keys)))
		 (:auxs
		  (push-let-binding var nil nil))))
	      (t
	       (simple-program-error "Non-symbol in lambda-list - ~S." var)))))
    ;; Generate code to check the number of arguments, unless dotted
    ;; in which case length will not work.
    (unless (eq restp :dotted)
       (push `(unless (<= ,minimum
			  (length (the list ,(if top-level
						 `(cdr ,arg-list-name)
					       arg-list-name)))
			  ,@(unless restp
				    (list maximum)))
		      ,(let ((arg (if top-level
				      `(cdr ,arg-list-name)
				    arg-list-name)))
			 (if (eq error-fun 'error)
			     `(do-arg-count-error ',error-kind ',name ,arg
						  ',lambda-list ,minimum
						  ,(unless restp maximum))
			   `(,error-fun 'defmacro-ll-arg-count-error
				 :kind ',error-kind
				 ,@(when name `(:name ',name))
				 :argument ,arg
				 :lambda-list ',lambda-list
				 :minimum ,minimum
				 ,@(unless restp `(:maximum ,maximum))))))
	     *arg-tests*))
    (if keys
	(let ((problem (gensym "KEY-PROBLEM-"))
	      (info (gensym "INFO-")))
	  (push `(multiple-value-bind
		     (,problem ,info)
		     (verify-keywords ,rest-name ',keys ',allow-other-keys-p)
		   (when ,problem
		     (,error-fun
		      'defmacro-ll-broken-key-list-error
		      :kind ',error-kind
		      ,@(when name `(:name ',name))
		      :problem ,problem
		      :info ,info)))
		*arg-tests*)))
    (values env-arg-used minimum (if (null restp) maximum nil))))

;;; end of file
