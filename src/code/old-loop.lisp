;;; -*- Package: LOOP -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/old-loop.lisp,v 1.10 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Loop facility, written by William Lott.
;;; 
(in-package "LOOP")

(in-package "LISP")
(export '(loop loop-finish))

(in-package "LOOP")


;;;; Specials used during the parse.

;;; These specials hold the different parts of the result as we are generating
;;; them.
;;; 
(defvar *loop-name*)
(defvar *outside-bindings*)
(defvar *prologue*)
(defvar *inside-bindings*)
(defvar *body-forms*)
(defvar *iteration-forms*)
(defvar *epilogue*)
(defvar *result-var*)
(defvar *return-value*)
(defvar *default-return-value*)
(defvar *accumulation-variables*)

;;; This special holds the remaining stuff we need to parse.
;;; 
(defvar *remaining-stuff*)

;;; This special holds a value that is EQ only to itself.
;;; 
(defvar *magic-cookie* (list '<magic-cookie>))


;;;; Utility functions/macros used by the parser.

(proclaim '(inline maybe-car maybe-cdr))

(defun maybe-car (thing)
  (if (consp thing) (car thing) thing))

(defun maybe-cdr (thing)
  (if (consp thing) (cdr thing) thing))


(defmacro loop-keyword-p (thing keyword &rest more-keywords)
  `(let ((thing ,thing))
     (and (symbolp thing)
	  (let ((name (symbol-name thing)))
	    (or ,@(mapcar #'(lambda (keyword)
			      `(string= name ,keyword))
			  (cons keyword more-keywords)))))))

(defun preposition-p (prep)
  (when (loop-keyword-p (car *remaining-stuff*) prep)
    (pop *remaining-stuff*)
    t))


(defun splice-in-subform (form subform)
  (if (eq form *magic-cookie*)
      subform
      (labels ((sub-splice-in-subform (form path)
		 (cond ((atom form)
			nil)
		       ((member form path)
			nil)
		       ((eq (car form) *magic-cookie*)
			(setf (car form) subform)
			t)
		       (t
			(let ((new-path (cons form path)))
			  (or (sub-splice-in-subform (car form) new-path)
			      (sub-splice-in-subform (cdr form) new-path)))))))
	(if (sub-splice-in-subform form nil)
	    form
	    (error "Couldn't find the magic cookie in:~% ~S~%Loop is broken."
		   form)))))

(defmacro queue-var (where name type &key
			   (initer nil initer-p) (stepper nil stepper-p))
  `(push (list ,name ,type ,initer-p ,initer ,stepper-p ,stepper)
	 ,where))

(defvar *default-values* '(nil 0 0.0)
  "The different possible default values.  When we need a default value, we
  use the first value in this list that is typep the desired type.")

(defun pick-default-value (var type)
  (if (consp var)
      (cons (pick-default-value (car var) (maybe-car type))
	    (pick-default-value (cdr var) (maybe-cdr type)))
      (dolist (default *default-values*
		       (error "Cannot default variables of type ~S ~
		               (for variable ~S)."
			      type var))
	(when (typep default type)
	  (return default)))))

(defun only-simple-types (type-spec)
  (if (atom type-spec)
      (member type-spec '(fixnum float t nil))
      (and (only-simple-types (car type-spec))
	   (only-simple-types (cdr type-spec)))))


(defun build-let-expression (vars)
  (if (null vars)
      (values *magic-cookie* *magic-cookie*)
      (let ((inside nil)
	    (outside nil)
	    (steppers nil)
	    (sub-lets nil))
	(dolist (var vars)
	  (labels
	      ((process (name type initial-p initial stepper-p stepper)
	         (cond ((atom name)
			(cond ((not stepper-p)
			       (push (list type name initial) outside))
			      ((not initial-p)
			       (push (list type name stepper) inside))
			      (t
			       (push (list type name initial) outside)
			       (setf steppers
				     (nconc steppers (list name stepper))))))
		       ((and (car name) (cdr name))
			(let ((temp (gensym (format nil "TEMP-FOR-~A-" name))))
			  (process temp 'list initial-p initial
				   stepper-p stepper)
			  (push (if stepper-p
				    (list (car name)
					  (maybe-car type)
					  nil nil
					  t `(car ,temp))
				    (list (car name)
					  (maybe-car type)
					  t `(car ,temp)
					  nil nil))
				sub-lets)
			  (push (if stepper-p
				    (list (cdr name)
					  (maybe-cdr type)
					  nil nil
					  t `(cdr ,temp))
				    (list (car name)
					  (maybe-cdr type)
					  t `(cdr ,temp)
					  nil nil))
				sub-lets)))
		       ((car name)
			(process (car name)
				 (maybe-car type)
				 initial-p `(car ,initial)
				 stepper-p `(car ,stepper)))
		       ((cdr name)
			(process (cdr name)
				 (maybe-cdr type)
				 initial-p `(cdr ,initial)
				 stepper-p `(cdr ,stepper))))))
	    (process (first var) (second var) (third var)
		     (fourth var) (fifth var) (sixth var))))
	(when steppers
	  (push (cons 'psetq steppers)
		*iteration-forms*))
	(multiple-value-bind
	    (sub-outside sub-inside)
	    (build-let-expression sub-lets)
	  (values (build-bindings outside sub-outside)
		  (build-bindings inside sub-inside))))))

(defun build-bindings (vars guts)
  (if (null vars)
      guts
      `(let ,(mapcar #'cdr vars)
	 (declare ,@(mapcar #'build-declare vars))
	 ,guts)))

(defun build-declare (var)
  `(type ,(car var) ,(cadr var)))



;;;; LOOP itself.

(defmacro loop (&rest stuff)
  "General iteration facility.  See the manual for details, 'cause it's
  very confusing."
  (if (some #'atom stuff)
      (parse-loop stuff)
      (let ((repeat (gensym "REPEAT-"))
	    (out-of-here (gensym "OUT-OF-HERE-")))
	`(block nil
	   (tagbody
	    ,repeat
	    (macrolet ((loop-finish () `(go ,out-of-here)))
	      ,@stuff)
	    (go ,repeat)
	    ,out-of-here)))))



;;;; The parser.

;;; Top level parser.  Bind the specials, and call the other parsers.
;;; 
(defun parse-loop (stuff)
  (let* ((*prologue* nil)
	 (*outside-bindings* *magic-cookie*)
	 (*inside-bindings* *magic-cookie*)
	 (*body-forms* nil)
	 (*iteration-forms* nil)
	 (*epilogue* nil)
	 (*result-var* nil)
	 (*return-value* nil)
	 (*default-return-value* nil)
	 (*accumulation-variables* nil)
	 (*remaining-stuff* stuff)
	 (name (parse-named)))
    (loop
      (when (null *remaining-stuff*)
	(return))
      (let ((clause (pop *remaining-stuff*)))
	(cond ((not (symbolp clause))
	       (error "Invalid clause, ~S, must be a symbol." clause))
	      ((loop-keyword-p clause "INITIALLY")
	       (setf *prologue* (nconc *prologue* (parse-expr-list))))
	      ((loop-keyword-p clause "FINALLY")
	       (parse-finally))
	      ((loop-keyword-p clause "WITH")
	       (parse-with))
	      ((loop-keyword-p clause "FOR" "AS")
	       (parse-for-as))
	      ((loop-keyword-p clause "REPEAT")
	       (parse-repeat))
	      (t
	       (push clause *remaining-stuff*)
	       (return)))))
    (loop
      (when (null *remaining-stuff*)
	(return))
      (let ((clause (pop *remaining-stuff*)))
	(cond ((not (symbolp clause))
	       (error "Invalid clause, ~S, must be a symbol." clause))
	      ((loop-keyword-p clause "INITIALLY")
	       (setf *prologue* (nconc *prologue* (parse-expr-list))))
	      ((loop-keyword-p clause "FINALLY")
	       (parse-finally))
	      ((loop-keyword-p clause "WHILE")
	       (setf *body-forms*
		     (nconc *body-forms*
			    `((unless ,(pop *remaining-stuff*)
				(loop-finish))))))
	      ((loop-keyword-p clause "UNTIL")
	       (setf *body-forms*
		     (nconc *body-forms*
			    `((when ,(pop *remaining-stuff*) (loop-finish))))))
	      ((loop-keyword-p clause "ALWAYS")
	       (setf *body-forms*
		     (nconc *body-forms*
			    `((unless ,(pop *remaining-stuff*)
				(return-from ,name nil)))))
	       (setf *default-return-value* t))
	      ((loop-keyword-p clause "NEVER")
	       (setf *body-forms*
		     (nconc *body-forms*
			    `((when ,(pop *remaining-stuff*)
				(return-from ,name nil)))))
	       (setf *default-return-value* t))
	      ((loop-keyword-p clause "THEREIS")
	       (setf *body-forms*
		     (nconc *body-forms*
			    (let ((temp (gensym "THEREIS-")))
			      `((let ((,temp ,(pop *remaining-stuff*)))
				  (when ,temp
				    (return-from ,name ,temp))))))))
	      (t
	       (push clause *remaining-stuff*)
	       (or (maybe-parse-unconditional)
		   (maybe-parse-conditional)
		   (maybe-parse-accumulation)
		   (error "Unknown clause, ~S" clause))))))
    (let ((again-tag (gensym "AGAIN-"))
	  (end-tag (gensym "THIS-IS-THE-END-")))
      `(block ,name
	 ,(splice-in-subform
	   *outside-bindings*
	   `(macrolet ((loop-finish () '(go ,end-tag)))
	      (tagbody
	       ,@*prologue*
	       ,again-tag
	       ,(splice-in-subform
		 *inside-bindings*
		 `(progn
		    ,@*body-forms*
		    ,@(nreverse *iteration-forms*)))
	       (go ,again-tag)
	       ,end-tag
	       ,@*epilogue*
	       (return-from ,name
			    ,(or *return-value*
				 *default-return-value*
				 *result-var*)))))))))

(defun parse-named ()
  (when (loop-keyword-p (car *remaining-stuff*) "NAMED")
    (pop *remaining-stuff*)
    (if (symbolp (car *remaining-stuff*))
	(pop *remaining-stuff*)
	(error "Loop name ~S is not a symbol." (car *remaining-stuff*)))))


(defun parse-expr-list ()
  (let ((results nil))
    (loop
      (when (atom (car *remaining-stuff*))
	(return (nreverse results)))
      (push (pop *remaining-stuff*) results))))

(defun parse-finally ()
  (let ((sub-clause (pop *remaining-stuff*)))
    (if (loop-keyword-p sub-clause "RETURN")
	(cond ((not (null *return-value*))
	       (error "Cannot specify two FINALLY RETURN clauses."))
	      ((null *remaining-stuff*)
	       (error "FINALLY RETURN must be followed with an expression."))
	      (t
	       (setf *return-value* (pop *remaining-stuff*))))
	(progn
	  (unless (loop-keyword-p sub-clause "DO" "DOING")
	    (push sub-clause *remaining-stuff*))
	  (setf *epilogue* (nconc *epilogue* (parse-expr-list)))))))

(defun parse-with ()
  (let ((vars nil))
    (loop
      (multiple-value-bind (var type) (parse-var-and-type-spec)
	(let ((initial
	       (if (loop-keyword-p (car *remaining-stuff*) "=")
		   (progn
		     (pop *remaining-stuff*)
		     (pop *remaining-stuff*))
		   (list 'quote
			 (pick-default-value var type)))))
	  (queue-var vars var type :initer initial)))
      (if (loop-keyword-p (car *remaining-stuff*) "AND")
	  (pop *remaining-stuff*)
	  (return)))
    (multiple-value-bind
	(outside inside)
	(build-let-expression vars)
      (setf *outside-bindings*
	    (splice-in-subform *outside-bindings* outside))
      (setf *inside-bindings*
	    (splice-in-subform *inside-bindings* inside)))))

(defun parse-var-and-type-spec ()
  (values (pop *remaining-stuff*)
	  (parse-type-spec t)))

(defun parse-type-spec (default)
  (cond ((preposition-p "OF-TYPE")
	 (pop *remaining-stuff*))
	((and *remaining-stuff*
	      (only-simple-types (car *remaining-stuff*)))
	 (pop *remaining-stuff*))
	(t
	 default)))



;;;; FOR/AS stuff.

;;; These specials hold the vars that need to be bound for this FOR/AS clause
;;; and all of the FOR/AS clauses connected with AND.  All the *for-as-vars*
;;; are bound in parallel followed by the *for-as-sub-vars*.
;;; 
(defvar *for-as-vars*)
(defvar *for-as-sub-vars*)

;;; These specials hold any extra termination tests.  *for-as-term-tests* are
;;; processed after the *for-as-vars* are bound, but before the
;;; *for-as-sub-vars*.  *for-as-sub-term-tests* are processed after the
;;; *for-as-sub-vars*.

(defvar *for-as-term-tests*)
(defvar *for-as-sub-term-tests*)


(defun parse-for-as ()
  (let ((*for-as-vars* nil)
	(*for-as-term-tests* nil)
	(*for-as-sub-vars* nil)
	(*for-as-sub-term-tests* nil))
    (loop
      (multiple-value-bind (name type) (parse-var-and-type-spec)
	(let ((sub-clause (pop *remaining-stuff*)))
	  (cond ((loop-keyword-p sub-clause "FROM" "DOWNFROM" "UPFROM"
				 "TO" "DOWNTO" "UPTO" "BELOW" "ABOVE")
		 (parse-arithmetic-for-as sub-clause name type))
		((loop-keyword-p sub-clause "IN")
		 (parse-in-for-as name type))
		((loop-keyword-p sub-clause "ON")
		 (parse-on-for-as name type))
		((loop-keyword-p sub-clause "=")
		 (parse-equals-for-as name type))
		((loop-keyword-p sub-clause "ACROSS")
		 (parse-across-for-as name type))
		((loop-keyword-p sub-clause "BEING")
		 (parse-being-for-as name type))
		(t
		 (error "Invalid FOR/AS subclause: ~S" sub-clause)))))
      (if (loop-keyword-p (car *remaining-stuff*) "AND")
	  (pop *remaining-stuff*)
	  (return)))
    (multiple-value-bind
	(outside inside)
	(build-let-expression *for-as-vars*)
      (multiple-value-bind
	  (sub-outside sub-inside)
	  (build-let-expression *for-as-sub-vars*)
	(setf *outside-bindings*
	      (splice-in-subform *outside-bindings*
				 (splice-in-subform outside sub-outside)))
	(let ((inside-body
	       (if *for-as-term-tests*
		   `(if (or ,@(nreverse *for-as-term-tests*))
			(loop-finish)
			,*magic-cookie*)
		   *magic-cookie*))
	      (sub-inside-body
	       (if *for-as-sub-term-tests*
		   `(if (or ,@(nreverse *for-as-sub-term-tests*))
			(loop-finish)
			,*magic-cookie*)
		   *magic-cookie*)))
	  (setf *inside-bindings*
		(splice-in-subform
		 *inside-bindings*
		 (splice-in-subform
		  inside
		  (splice-in-subform
		   inside-body
		   (splice-in-subform
		    sub-inside
		    sub-inside-body))))))))))

(defun parse-arithmetic-for-as (sub-clause name type)
  (unless (atom name)
    (error "Cannot destructure arithmetic FOR/AS variables: ~S" name))
  (let (start stop (inc 1) dir exclusive-p)
    (cond ((loop-keyword-p sub-clause "FROM")
	   (setf start (pop *remaining-stuff*)))
	  ((loop-keyword-p sub-clause "DOWNFROM")
	   (setf start (pop *remaining-stuff*))
	   (setf dir :down))
	  ((loop-keyword-p sub-clause "UPFROM")
	   (setf start (pop *remaining-stuff*))
	   (setf dir :up))
	  (t
	   (push sub-clause *remaining-stuff*)))
    (cond ((preposition-p "TO")
	   (setf stop (pop *remaining-stuff*)))
	  ((preposition-p "DOWNTO")
	   (setf stop (pop *remaining-stuff*))
	   (if (eq dir :up)
	       (error "Can't mix UPFROM and DOWNTO in ~S." name)
	       (setf dir :down)))
	  ((preposition-p "UPTO")
	   (setf stop (pop *remaining-stuff*))
	   (if (eq dir :down)
	       (error "Can't mix DOWNFROM and UPTO in ~S." name)
	       (setf dir :up)))
	  ((preposition-p "ABOVE")
	   (setf stop (pop *remaining-stuff*))
	   (setf exclusive-p t)
	   (if (eq dir :up)
	       (error "Can't mix UPFROM and ABOVE in ~S." name)
	       (setf dir :down)))
	  ((preposition-p "BELOW")
	   (setf stop (pop *remaining-stuff*))
	   (setf exclusive-p t)
	   (if (eq dir :down)
	       (error "Can't mix DOWNFROM and BELOW in ~S." name)
	       (setf dir :up))))
    (when (preposition-p "BY")
      (setf inc (pop *remaining-stuff*)))
    (when (and (eq dir :down) (null start))
      (error "No default starting value for decremental stepping."))
    (let ((temp (gensym "TEMP-AMOUNT-")))
      (queue-var *for-as-sub-vars* temp type :initer inc)
      (queue-var *for-as-sub-vars* name type
		 :initer (or start 0)
		 :stepper `(,(if (eq dir :down) '- '+) ,name ,temp))
      (when stop
	(let ((stop-var (gensym "STOP-VAR-")))
	  (queue-var *for-as-sub-vars* stop-var type :initer stop)
	  (push (list (if (eq dir :down)
			  (if exclusive-p '<= '<)
			  (if exclusive-p '>= '>))
		      name stop-var)
		*for-as-sub-term-tests*))))))

(defun parse-in-for-as (name type)
  (let* ((temp (gensym "LIST-"))
	 (initer (pop *remaining-stuff*))
	 (stepper (if (preposition-p "BY")
		      `(funcall ,(pop *remaining-stuff*) ,temp)
		      `(cdr ,temp))))
    (queue-var *for-as-vars* temp 'list :initer initer :stepper stepper)
    (queue-var *for-as-sub-vars* name type :stepper `(car ,temp))
    (push `(null ,temp) *for-as-sub-term-tests*)))

(defun parse-on-for-as (name type)
  (let* ((temp (if (atom name) name (gensym "LIST-")))
	 (initer (pop *remaining-stuff*))
	 (stepper (if (preposition-p "BY")
		      `(funcall ,(pop *remaining-stuff*) ,temp)
		      `(cdr ,temp))))
    (cond ((atom name)
	   (queue-var *for-as-sub-vars* name type
		      :initer initer :stepper stepper)
	   (push `(endp ,name) *for-as-sub-term-tests*))
	  (t
	   (queue-var *for-as-vars* temp type
		      :initer initer :stepper stepper)
	   (queue-var *for-as-sub-vars* name type :stepper temp)
	   (push `(endp ,temp) *for-as-term-tests*)))))

(defun parse-equals-for-as (name type)
  (let ((initer (pop *remaining-stuff*)))
    (if (preposition-p "THEN")
	(queue-var *for-as-sub-vars* name type
		   :initer initer :stepper (pop *remaining-stuff*))
	(queue-var *for-as-vars* name type :stepper initer))))

(defun parse-across-for-as (name type)
  (let* ((temp (gensym "VECTOR-"))
	 (length (gensym "LENGTH-"))
	 (index (gensym "INDEX-")))
    (queue-var *for-as-vars* temp `(vector ,type)
	       :initer (pop *remaining-stuff*))
    (queue-var *for-as-sub-vars* length 'fixnum
	       :initer `(length ,temp))
    (queue-var *for-as-vars* index 'fixnum :initer 0 :stepper `(1+ ,index))
    (queue-var *for-as-sub-vars* name type :stepper `(aref ,temp ,index))
    (push `(>= ,index ,length) *for-as-term-tests*)))

(defun parse-being-for-as (name type)
  (let ((clause (pop *remaining-stuff*)))
    (unless (loop-keyword-p clause "EACH" "THE")
      (error "BEING must be followed by either EACH or THE, not ~S"
	     clause)))
  (let ((clause (pop *remaining-stuff*)))
    (cond ((loop-keyword-p clause "HASH-KEY" "HASH-KEYS"
 			   "HASH-VALUE" "HASH-VALUES")
	   (let ((prep (pop *remaining-stuff*)))
	     (unless (loop-keyword-p prep "IN" "OF")
	       (error "~A must be followed by either IN or OF, not ~S"
		      (symbol-name clause) prep)))
	   (let ((table (pop *remaining-stuff*))
		 (iterator (gensym (format nil "~A-ITERATOR-" name)))
		 (exists-temp (gensym (format nil "~A-EXISTS-TEMP-" name)))
		 (key-temp (gensym (format nil "~A-KEY-TEMP-" name)))
		 (value-temp (gensym (format nil "~A-VALUE-TEMP-" name))))
	     (setf *outside-bindings*
		   (splice-in-subform
		    *outside-bindings*
		    `(with-hash-table-iterator (,iterator ,table)
					       ,*magic-cookie*)))
	     (multiple-value-bind
		 (using using-type)
		 (when (preposition-p "USING")
		   ;; ### This is wrong.
		   (parse-var-and-type-spec))
	       (multiple-value-bind
		   (key-var key-type value-var value-type)
		   (if (loop-keyword-p clause "HASH-KEY" "HASH-KEYS")
		       (values name type using using-type)
		       (values using using-type name type))
		 (setf *inside-bindings*
		       (splice-in-subform
			*inside-bindings*
			`(multiple-value-bind
			     (,exists-temp ,key-temp ,value-temp)
			     (,iterator)
			   ,@(unless (and key-var value-var)
			       `((declare (ignore ,@(if (null key-var)
							(list key-temp))
						  ,@(if (null value-var)
							(list value-temp))))))
			   ,*magic-cookie*)))
		 (push `(not ,exists-temp) *for-as-term-tests*)
		 (when key-var
		   (queue-var *for-as-sub-vars* key-var key-type
			      :stepper key-temp))
		 (when value-var
		   (queue-var *for-as-sub-vars* value-var value-type
			      :stepper value-temp))))))
	  ((loop-keyword-p clause "SYMBOL" "PRESENT-SYMBOL" "EXTERNAL-SYMBOL"
			   "SYMBOLS" "PRESENT-SYMBOLS" "EXTERNAL-SYMBOLS")
	   (let ((package
		  (if (or (preposition-p "IN")
			  (preposition-p "OF"))
		      (pop *remaining-stuff*)
		      '*package*))
		 (iterator (gensym (format nil "~A-ITERATOR-" name)))
		 (exists-temp (gensym (format nil "~A-EXISTS-TEMP-" name)))
		 (symbol-temp (gensym (format nil "~A-SYMBOL-TEMP-" name))))
	     (setf *outside-bindings*
		   (splice-in-subform
		    *outside-bindings*
		    `(with-package-iterator
			 (,iterator
			  ,package
			  ,@(cond ((loop-keyword-p clause "SYMBOL" "SYMBOLS")
				   '(:internal :external :inherited))
				  ((loop-keyword-p clause "PRESENT-SYMBOL"
						   "PRESENT-SYMBOLS")
				   '(:internal))
				  ((loop-keyword-p clause "EXTERNAL-SYMBOL"
						   "EXTERNAL-SYMBOLS")
				   '(:external))
				  (t
				   (error "Don't know how to deal with ~A?  ~
				           Bug in LOOP?" clause))))
		       ,*magic-cookie*)))
	     (setf *inside-bindings*
		   (splice-in-subform
		    *inside-bindings*
		    `(multiple-value-bind
			 (,exists-temp ,symbol-temp)
			 (,iterator)
		       ,*magic-cookie*)))
	     (push `(not ,exists-temp) *for-as-term-tests*)
	     (queue-var *for-as-sub-vars* name type :stepper symbol-temp)))
	  (t
	   (error
	    "Unknown sub-clause, ~A, for BEING.  Must be one of:~%  ~
	     HASH-KEY HASH-KEYS HASH-VALUE HASH-VALUES SYMBOL SYMBOLS~%  ~
	     PRESENT-SYMBOL PRESENT-SYMBOLS EXTERNAL-SYMBOL EXTERNAL-SYMBOLS"
	    (symbol-name clause))))))



;;;;

(defun parse-repeat ()
  (let ((temp (gensym "REPEAT-")))
    (setf *outside-bindings*
	  (splice-in-subform *outside-bindings*
			     `(let ((,temp ,(pop *remaining-stuff*)))
				,*magic-cookie*)))
    (setf *inside-bindings*
	  (splice-in-subform *inside-bindings*
			     `(if (minusp (decf ,temp))
				  (loop-finish)
				  ,*magic-cookie*)))))


(defun maybe-parse-unconditional ()
  (cond ((loop-keyword-p (car *remaining-stuff*) "DO" "DOING")
	 (pop *remaining-stuff*)
	 (setf *body-forms* (nconc *body-forms* (parse-expr-list)))
	 t)
	((loop-keyword-p (car *remaining-stuff*) "RETURN")
	 (pop *remaining-stuff*)
	 (setf *body-forms*
	       (nconc *body-forms* `((return ,(pop *remaining-stuff*)))))
	 t)))


(defun maybe-parse-conditional ()
  (let ((clause (pop *remaining-stuff*)))
    (cond ((loop-keyword-p clause "IF" "WHEN")
	   (parse-conditional (pop *remaining-stuff*))
	   t)
	  ((loop-keyword-p clause "UNLESS")
	   (parse-conditional `(not ,(pop *remaining-stuff*)))
	   t)
	  (t
	   (push clause *remaining-stuff*)
	   nil))))

(defun parse-conditional (condition)
  (let ((clauses (parse-and-clauses))
	(else-clauses (when (preposition-p "ELSE")
			(parse-and-clauses))))
    (setf *body-forms*
	  (nconc *body-forms*
		 `((if ,condition
		       (progn
			 ,@clauses)
		       (progn
			 ,@else-clauses)))))
    (preposition-p "END")))

(defun parse-and-clauses ()
  (let ((*body-forms* nil))
    (loop
      (or (maybe-parse-unconditional)
	  (maybe-parse-conditional)
	  (maybe-parse-accumulation)
	  (error "Invalid clause for inside a conditional: ~S"
		 (car *remaining-stuff*)))
      (unless (preposition-p "AND")
	(return *body-forms*)))))


;;;; Assumulation stuff

(defun maybe-parse-accumulation ()
  (when (loop-keyword-p (car *remaining-stuff*)
		       "COLLECT" "COLLECTING"
		       "APPEND" "APPENDING" "NCONC" "NCONCING"
		       "COUNT" "COUNTING" "SUM" "SUMMING"
		       "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING")
    (parse-accumulation)
    t))

(defun parse-accumulation ()
  (let* ((clause (pop *remaining-stuff*))
	 (expr (pop *remaining-stuff*))
	 (var (if (preposition-p "INTO")
		  (pop *remaining-stuff*)
		  (or *result-var*
		      (setf *result-var*
			    (gensym (concatenate 'simple-string
						 (string clause)
						 "-"))))))
	 (info (assoc var *accumulation-variables*))
	 (type nil)
	 (initial nil))
    (cond ((loop-keyword-p clause "COLLECT" "COLLECTING" "APPEND" "APPENDING"
			   "NCONC" "NCONCING")
	   (setf initial nil)
	   (setf type 'list)
	   (let ((aux-var
		  (or (caddr info)
		      (let ((aux-var (gensym "LAST-")))
			(setf *outside-bindings*
			      (splice-in-subform *outside-bindings*
						 `(let ((,var nil)
							(,aux-var nil))
						    (declare (type list
								   ,var
								   ,aux-var))
						    ,*magic-cookie*)))
			(if (null info)
			    (push (setf info (list var 'list aux-var))
				  *accumulation-variables*)
			    (setf (cddr info) (list aux-var)))
			aux-var)))
		 (value
		  (cond ((loop-keyword-p clause "COLLECT" "COLLECTING")
			 `(list ,expr))
			((loop-keyword-p clause "APPEND" "APPENDING")
			 `(copy-list ,expr))
			((loop-keyword-p clause "NCONC" "NCONCING")
			 expr)
			(t
			 (error "Bug in loop?")))))
	     (setf *body-forms*
		   (nconc *body-forms*
			  `((cond ((null ,var)
				   (setf ,var ,value)
				   (setf ,aux-var (last ,var)))
				  (t
				   (nconc ,aux-var ,value)
				   (setf ,aux-var (last ,aux-var)))))))))
	  ((loop-keyword-p clause "COUNT" "COUNTING")
	   (setf type (parse-type-spec 'unsigned-byte))
	   (setf initial 0)
	   (setf *body-forms*
		 (nconc *body-forms*
			`((when ,expr (incf ,var))))))
	  ((loop-keyword-p clause "SUM" "SUMMING")
	   (setf type (parse-type-spec 'number))
	   (setf initial 0)
	   (setf *body-forms*
		 (nconc *body-forms*
			`((incf ,var ,expr)))))
	  ((loop-keyword-p clause "MAXIMIZE" "MAXIMIZING")
	   (setf type `(or null ,(parse-type-spec 'number)))
	   (setf initial nil)
	   (setf *body-forms*
		 (nconc *body-forms*
			(let ((temp (gensym "MAX-TEMP-")))
			  `((let ((,temp ,expr))
			      (when (or (null ,var)
					(> ,temp ,var))
				(setf ,var ,temp))))))))
	  ((loop-keyword-p clause "MINIMIZE" "MINIMIZING")
	   (setf type `(or null ,(parse-type-spec 'number)))
	   (setf initial nil)
	   (setf *body-forms*
		 (nconc *body-forms*
			(let ((temp (gensym "MIN-TEMP-")))
			  `((let ((,temp ,expr))
			      (when (or (null ,var)
					(< ,temp ,var))
				(setf ,var ,temp))))))))
	  (t
	   (error "Invalid accumulation clause: ~S" clause)))
    (cond (info
	   (unless (equal type (cadr info))
	     (error "Attempt to use ~S for both types ~S and ~S."
		    var type (cadr info))))
	  (t
	   (push (list var type) *accumulation-variables*)
	   (setf *outside-bindings*
		 (splice-in-subform *outside-bindings*
				    `(let ((,var ,initial))
				       (declare (type ,type ,var))
				       ,*magic-cookie*)))))))
