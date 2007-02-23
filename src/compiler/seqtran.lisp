;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/seqtran.lisp,v 1.31 2005/04/21 18:57:43 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains optimizers for list and sequence functions.
;;;
;;; Written by Rob MacLachlan.  Some code adapted from the old seqtran file,
;;; written by Wholey and Fahlman.
;;;
(in-package "C")


(defun mapper-transform (fn arglists accumulate take-car)
  (once-only ((fn fn))
    (collect ((do-clauses)
	      (args-to-fn)
	      (tests))
      (let ((n-first (gensym)))
	(dolist (a (if accumulate
		       arglists
		       `(,n-first ,@(rest arglists))))
	  (let ((v (gensym)))
	    (do-clauses `(,v ,a (cdr ,v)))
	    (tests `(endp ,v))
	    (args-to-fn (if take-car `(car ,v) v))))
	(let ((call `(funcall ,fn . ,(args-to-fn)))
	      (endtest `(or ,@(tests))))
	  (ecase accumulate
	    (:nconc
	     (let ((temp (gensym))
		   (map-result (gensym)))
	       `(let ((,map-result (list nil)))
		  (declare (dynamic-extent ,map-result))
		  (do-anonymous ((,temp ,map-result) . ,(do-clauses))
				 (,endtest (cdr ,map-result))
		    (setq ,temp (last (nconc ,temp ,call)))))))
	    (:list
	     (let ((temp (gensym))
		   (map-result (gensym)))
	       `(let ((,map-result (list nil)))
		  (declare (dynamic-extent ,map-result))
		  (do-anonymous ((,temp ,map-result) . ,(do-clauses))
				 (,endtest (cdr ,map-result))
		    (rplacd ,temp (setq ,temp (list ,call)))))))
	    ((nil)
	     `(let ((,n-first ,(first arglists)))
	        (do-anonymous ,(do-clauses)
			      (,endtest ,n-first) ,call)))))))))

(def-source-transform mapc (function list &rest more-lists)
  (mapper-transform function (cons list more-lists) nil t))

(def-source-transform mapcar (function list &rest more-lists)
  (mapper-transform function (cons list more-lists) :list t))

(def-source-transform mapcan (function list &rest more-lists)
  (mapper-transform function (cons list more-lists) :nconc t))

(def-source-transform mapl (function list &rest more-lists)
  (mapper-transform function (cons list more-lists) nil nil))

(def-source-transform maplist (function list &rest more-lists)
  (mapper-transform function (cons list more-lists) :list nil))

(def-source-transform mapcon (function list &rest more-lists)
  (mapper-transform function (cons list more-lists) :nconc nil))


;;; MAP-INTO
(deftransform map-into ((result fun &rest seqs)
                        (vector * &rest *)
                        *)
  "open code"
  (let ((seqs-names (mapcar (lambda (x)
                              (declare (ignore x))
                              (gensym))
                            seqs)))
    `(lambda (result fun ,@seqs-names)
       (let ((length (array-dimension result 0))
             (i 0))
         (declare (type index i))
         (declare (ignorable i))
         ,(cond ((null seqs)
                 `(dotimes (j length (setq i length))
                    (setf (aref result j) (funcall fun))))
                (t
                 `(block nil
                    (map nil
                         (lambda (,@seqs-names)
                           (when (= i length) (return))
                           (setf (aref result i)
                                 (funcall fun ,@seqs-names))
                           (incf i))
                         ,@seqs-names))))
         (when (array-has-fill-pointer-p result)
           (setf (fill-pointer result) i))
         result))))

(deftransform elt ((s i) ((simple-array * (*)) *) * :when :both)
  '(aref s i))

(deftransform elt ((s i) (list *) * :when :both :policy (< safety 3))
  '(nth i s))

(deftransform %setelt ((s i v) ((simple-array * (*)) * *) * :when :both)
  '(%aset s i v))

(deftransform %setelt ((s i v) (list * *) * :policy (< safety 3))
  '(setf (car (nthcdr i s)) v))


(dolist (name '(member memq))
  (deftransform name ((e l &key (test #'eql)) '* '* :node node :when :both
		      :eval-name t)
    (unless (constant-continuation-p l) (give-up))
    
    (let ((val (continuation-value l)))
      (unless (policy node
		      (or (= speed 3)
			  (and (>= speed space)
			       (<= (length val) 5))))
	(give-up))
      
      (labels ((frob (els)
		 (if els
		     `(if (funcall test e ',(car els))
			  ',els
			  ,(frob (cdr els)))
		     'nil)))
	(frob val)))))

(dolist (x '((delete delq)
	     (assoc assq)
	     (member memq)))
  (destructuring-bind (fun eq-fun) x
    (deftransform fun ((item list &key test) '(t list &rest t) '*
			:eval-name t)
      "convert to EQ test"
      (cond (test
	     (unless (continuation-function-is test '(eq))
	       (give-up)))
	    ((types-intersect (continuation-type item)
			      (specifier-type 'number))
	     (give-up "Item might be a number")))
      `(,eq-fun item list))))

(deftransform delete-if ((pred list) (t list))
  "inline expand"
  '(do ((x list (cdr x))
	(splice '()))
       ((endp x) list)
     (cond ((funcall pred (car x))
	    (if (null splice) 
		(setq list (cdr x))
		(rplacd splice (cdr x))))
	   (T (setq splice x)))))

(deftransform fill ((seq item &key (start 0) (end (length seq)))
		    (simple-array t &key (:start t) (:end index)))
  "open code"
  '(do ((i start (1+ i)))
       ((= i end) seq)
     (declare (type index i))
     (setf (aref seq i) item)))

(deftransform position ((item list &key (test #'eql)) (t list))
  "open code"
  '(do ((i 0 (1+ i))
	(l list (cdr l)))
       ((endp l) nil)
     (declare (type index i))
     (when (funcall test item (car l)) (return i))))

(deftransform position ((item vec &key (test #'eql) (start 0)
			      (end (length vec)))
			(t simple-array &key (:start t) (:end index)))
  "open code"
  '(do ((i start (1+ i)))
       ((= i end) nil)
     (declare (type index i))
     (when (funcall test item (aref vec i)) (return i))))

;;; Names of predicates that compute the same value as CHAR= when applied to
;;; characters.
;;; 
(defconstant char=-functions '(eql equal char=))

(deftransform search ((string1 string2 &key (start1 0) end1 (start2 0) end2
			       test)
		      (simple-string simple-string &rest t))
  (unless (or (not test)
	      (continuation-function-is test char=-functions))
    (give-up))
  '(lisp::%sp-string-search string1 start1 (or end1 (length string1))
			    string2 start2 (or end2 (length string2))))
			      
(deftransform position ((item sequence &key from-end test (start 0) end)
			(t simple-string &rest t))
  (unless (or (not test)
	      (continuation-function-is test char=-functions))
    (give-up))
  `(and (typep item 'character)
	(,(if (constant-value-or-lose from-end)
	      'lisp::%sp-reverse-find-character
	      'lisp::%sp-find-character)
	 sequence start (or end (length sequence))
	 item)))


(deftransform find ((item sequence &key from-end (test #'eql) (start 0) end)
		    (t simple-string &rest t))
  (let ((index (gensym)))
    `(let ((,index (position item sequence
			     ,@(when from-end `(:from-end from-end))
			     :test test :start start :end end)))
      (if ,index
	  (elt sequence ,index)
	  nil))))


;;;; Utilities:


;;; CONTINUATION-FUNCTION-IS  --  Interface
;;;
;;;    Return true if Cont's only use is a non-notinline reference to a global
;;; function with one of the specified Names.
;;;
(defun continuation-function-is (cont names)
  (declare (type continuation cont) (list names))
  (let ((use (continuation-use cont)))
    (and (ref-p use)
	 (let ((leaf (ref-leaf use)))
	   (and (global-var-p leaf)
		(eq (global-var-kind leaf) :global-function)
		(not (null (member (leaf-name leaf) names :test #'equal))))))))


;;; CONSTANT-VALUE-OR-LOSE  --  Interface
;;;
;;;    If Cont is a constant continuation, the return the constant value.  If
;;; it is null, then return default, otherwise quietly GIVE-UP.
;;; ### Probably should take an ARG and flame using the NAME.
;;;
(defun constant-value-or-lose (cont &optional default)
  (declare (type (or continuation null) cont))
  (cond ((not cont) default)
	((constant-continuation-p cont)
	 (continuation-value cont))
	(t
	 (give-up))))

#|
;;; MAKE-ARG, ARG-CONT, ARG-NAME  --  Interface
;;;
;;;    This is a frob whose job it is to make it easier to pass around the
;;; arguments to IR1 transforms.  It bundles together the name of the argument
;;; (which should be referenced in any expansion), and the continuation for
;;; that argument (or NIL if unsupplied.)
;;;
(defstruct (arg (:constructor %make-arg (name cont)))
  (name nil :type symbol)
  (cont nil :type (or continuation null)))
;;;
(defmacro make-arg (name)
  `(%make-arg ',name ,name))

;;; DEFAULT-ARG  --  Interface
;;;
;;;    If Arg is null or its CONT is null, then return Default, otherwise
;;; return Arg's NAME.
;;;
(defun default-arg (arg default)
  (declare (type (or arg null) arg))
  (if (and arg (arg-cont arg))
      (arg-name arg)
      default))


;;; ARG-CONSTANT-VALUE  --  Interface
;;;
;;;    If Arg is null or has no CONT, return the default.  Otherwise, Arg's
;;; CONT must be a constant continuation whose value we return.  If not, we
;;; give up.
;;;
(defun arg-constant-value (arg default)
  (declare (type (or arg null) arg))
  (if (and arg (arg-cont arg))
      (let ((cont (arg-cont arg)))
	(unless (constant-continuation-p cont)
	  (give-up "Argument is not constant: ~S." (arg-name arg)))
	(continuation-value from-end))
      default))


;;; ARG-EQL  --  Internal
;;;
;;;    If Arg is a constant and is EQL to X, then return T, otherwise NIL.  If
;;; Arg is NIL or its CONT is NIL, then compare to the default.
;;;
(defun arg-eql (arg default x)
  (declare (type (or arg null) x))
  (if (and arg (arg-cont arg))
      (let ((cont (arg-cont arg)))
	(and (constant-continuation-p cont)
	     (eql (continuation-value cont) x)))
      (eql default x)))


(defstruct iterator
  ;;
  ;; The kind of iterator.
  (kind nil (member :normal :result))
  ;;
  ;; A list of LET* bindings to create the initial state.
  (binds nil :type list)
  ;;
  ;; A list of declarations for Binds.
  (decls nil :type list)
  ;;
  ;; A form that returns the current value.  This may be set with SETF to set
  ;; the current value.
  (current (error "Must specify CURRENT."))
  ;;
  ;; In a :Normal iterator, a form that tests whether there is a current value.
  (done nil)
  ;;
  ;; In a :Result iterator, a form that truncates the result at the current
  ;; position and returns it.
  (result nil)
  ;;
  ;; A form that returns the initial total number of values.  The result is
  ;; undefined after NEXT has been evaluated.
  (length (error "Must specify LENGTH."))
  ;;
  ;; A form that advances the state to the next value.  It is an error to call
  ;; this when the iterator is Done.
  (next (error "Must specify NEXT.")))


;;; Type of an index var that can go negative (in the from-end case.)
(deftype neg-index ()
  `(integer -1 ,most-positive-fixnum))


;;; MAKE-SEQUENCE-ITERATOR  --  Interface
;;;
;;;    Return an ITERATOR structure describing how to iterate over an arbitrary
;;; sequence.  Sequence is a variable bound to the sequence, and Type is the
;;; type of the sequence.  If true, INDEX is a variable that should be bound to
;;; the index of the current element in the sequence.
;;;
;;;    If we can't tell whether the sequence is a list or a vector, or whether
;;; the iteration is forward or backward, then GIVE-UP.
;;;
(defun make-sequence-iterator (sequence type &key start end from-end index)
  (declare (symbol sequence) (type ctype type)
	   (type (or arg null) start end from-end)
	   (type (or symbol null) index))
  (let ((from-end (arg-constant-value from-end nil)))
    (cond ((csubtypep type (specifier-type 'vector))
	   (let* ((n-stop (gensym))
		  (n-idx (or index (gensym)))
		  (start (default-arg 0 start))
		  (end (default-arg `(length ,sequence) end)))
	     (make-iterator
	      :kind :normal
	      :binds `((,n-idx ,(if from-end `(1- ,end) ,start))
		       (,n-stop ,(if from-end `(1- ,start) ,end)))
	      :decls `((type neg-index ,n-idx ,n-stop))
	      :current `(aref ,sequence ,n-idx)
 	      :done `(,(if from-end '<= '>=) ,n-idx ,n-stop)
	      :next `(setq ,n-idx
			   ,(if from-end `(1- ,n-idx) `(1+ ,n-idx)))
	      :length (if from-end
			  `(- ,n-idx ,n-stop)
			  `(- ,n-stop ,n-idx)))))
	  ((csubtypep type (specifier-type 'list))
	   (let* ((n-stop (if (and end (not from-end)) (gensym) nil))
		  (n-current (gensym))
		  (start-p (not (arg-eql start 0 0)))
		  (end-p (not (arg-eql end nil nil)))
		  (start (default-arg start 0))
		  (end (default-arg end nil)))
	     (make-iterator
	      :binds `((,n-current
			,(if from-end
			     (if (or start-p end-p)
				 `(nreverse (subseq ,sequence ,start
						    ,@(when end `(,end))))
				 `(reverse ,sequence))
			     (if start-p
				 `(nthcdr ,start ,sequence)
				 sequence)))
		       ,@(when n-stop
			   `((,n-stop (nthcdr (the index
						   (- ,end ,start))
					      ,n-current))))
		       ,@(when index
			   `((,index ,(if from-end `(1- ,end) start)))))
	      :kind :normal
	      :decls `((list ,n-current ,n-end)
		       ,@(when index `((type neg-index ,index))))
	      :current `(car ,n-current)
	      :done `(eq ,n-current ,n-stop)
	      :length `(- ,(or end `(length ,sequence)) ,start)
	      :next `(progn
		       (setq ,n-current (cdr ,n-current))
		       ,@(when index
			   `((setq ,n-idx
				   ,(if from-end
					`(1- ,index)
					`(1+ ,index)))))))))
	  (t
	   (give-up "Can't tell whether sequence is a list or a vector.")))))


;;; MAKE-RESULT-SEQUENCE-ITERATOR  --  Interface
;;;
;;;    Make an iterator used for constructing result sequences.  Name is a
;;; variable to be bound to the result sequence.  Type is the type of result
;;; sequence to make.  Length is an expression to be evaluated to get the
;;; maximum length of the result (not evaluated in list case.)
;;;
(defun make-result-sequence-iterator (name type length)
  (declare (symbol name) (type ctype type))

;;; COERCE-FUNCTIONS  --  Interface
;;;
;;;    Defines each Name as a local macro that will call the value of the
;;; Fun-Arg with the given arguments.  If the argument isn't known to be a
;;; function, give them an efficiency note and reference a coerced version.
;;;
(defmacro coerce-functions (specs &body body)
  "COERCE-FUNCTIONS ({(Name Fun-Arg Default)}*) Form*"
  (collect ((binds)
	    (defs))
    (dolist (spec specs)
      `(let ((body (progn ,@body))
	     (n-fun (arg-name ,(second spec)))
	     (fun-cont (arg-cont ,(second spec))))
	 (cond ((not fun-cont)
		`(macrolet ((,',(first spec) (&rest args)
			     `(,',',(third spec) ,@args)))
		   ,body))
	       ((not (csubtypep (continuation-type fun-cont)
				(specifier-type 'function)))
		(when (policy *compiler-error-context* (> speed brevity))
		  (compiler-note
		   "~S may not be a function, so must coerce at run-time."
		   n-fun))
		(once-only ((n-fun `(if (functionp ,n-fun)
					,n-fun
					(symbol-function ,n-fun))))
		  `(macrolet ((,',(first spec) (&rest args)
			       `(funcall ,',n-fun ,@args)))
		     ,body)))
	       (t
		`(macrolet ((,',(first spec) (&rest args)
			      `(funcall ,',n-fun ,@args)))
		   ,body)))))))


;;; WITH-SEQUENCE-TEST  --  Interface
;;;
;;;    Wrap code around the result of the body to define Name as a local macro
;;; that returns true when its arguments satisfy the test according to the Args
;;; Test and Test-Not.  If both Test and Test-Not are supplied, abort the
;;; transform.
;;;
(defmacro with-sequence-test ((name test test-not) &body body)
  `(let ((not-p (arg-cont ,test-not)))
     (when (and (arg-cont ,test) not-p)
       (abort-transform "Both ~S and ~S supplied." (arg-name ,test)
			(arg-name ,test-not)))
     (coerce-functions ((,name (if not-p ,test-not ,test) eql))
       ,@body)))

|#

;;;; Hairy sequence transforms:



;;;; String operations:

;;; STRINGxxx transform  --  Internal
;;;
;;;    We transform the case-sensitive string predicates into a non-keyword
;;; version.  This is an IR1 transform so that we don't have to worry about
;;; changing the order of evaluation.
;;;
(dolist (stuff '((string< string<*)
		 (string> string>*)
		 (string<= string<=*)
		 (string>= string>=*)
		 (string= string=*)
		 (string/= string/=*)))
  (destructuring-bind (fun pred*) stuff
    (deftransform fun ((string1 string2 &key (start1 0) end1
				(start2 0) end2)
		       '* '* :eval-name t)
      `(,pred* string1 string2 start1 end1 start2 end2))))


;;; STRING-xxx* transform  --  Internal
;;;
;;;    Return a form that tests the free variables STRING1 and STRING2 for the
;;; ordering relationship specified by Lessp and Equalp.  The start and end are
;;; also gotten from the environment.  Both strings must be simple strings.
;;;
#+(or)
(dolist (stuff '((string<* t nil)
		 (string<=* t t)
		 (string>* nil nil)
		 (string>=* nil t)))
  (destructuring-bind (name lessp equalp) stuff
    (deftransform name ((string1 string2 start1 end1 start2 end2)
			'(simple-string simple-string t t t t) '*
			:eval-name t)
      `(let* ((end1 (if (not end1) (length string1) end1))
	      (end2 (if (not end2) (length string2) end2))
	      (index (lisp::%sp-string-compare
		      string1 start1 end1 string2 start2 end2)))
	 (if index
	     (cond ((= index ,(if lessp 'end1 'end2)) index)
		   ((= index ,(if lessp 'end2 'end1)) nil)
		   ((,(if lessp 'char< 'char>)
		     (schar string1 index)
		     (schar string2
			    (truly-the index
				       (+ index
					  (truly-the fixnum
						     (- start2 start1))))))
		    index)
		   (t nil))
	     ,(if equalp 'end1 'nil))))))

(dolist (stuff '((string<* t nil)
		 (string<=* t t)
		 (string>* nil nil)
		 (string>=* nil t)))
  (destructuring-bind (name lessp equalp) stuff
    (deftransform name ((string1 string2 start1 end1 start2 end2)
			'(simple-string simple-string t t t t) '*
			:eval-name t)
      `(let* ((end1 (if (not end1) (length string1) end1))
	      (end2 (if (not end2) (length string2) end2))
	      (index (lisp::%sp-string-compare
		      string1 start1 end1 string2 start2 end2)))
	 ;; FIXME: This is basically the macro body from
	 ;; string<>=*-body in string.lisp, except I manually replaced
	 ;; the offset1 var with 0 and simplified.  We should combine
	 ;; these into one macro.
	 (if index
	     (cond ((= index end1)
		    ,(if lessp
			 `index
			 `nil))
		   ((= (+ index (- start2 start1))
		       end2)
		    ,(if lessp
			 `nil
			 `index))
		   ((,(if lessp 'char< 'char>)
		     (schar string1 index)
		     (schar string2
			    (truly-the index
				       (+ index
					  (truly-the fixnum
						     (- start2 start1))))))
		    index)
		   (t nil))
	     ,(if equalp `end1 'nil))))))


(dolist (stuff '((string=* not)
		 (string/=* identity)))
  (destructuring-bind (name result-fun) stuff
    (deftransform name ((string1 string2 start1 end1 start2 end2)
			'(simple-string simple-string t t t t) '*
			:eval-name t)
      `(,result-fun
	(lisp::%sp-string-compare
	 string1 start1 (or end1 (length string1))
	 string2 start2 (or end2 (length string2)))))))



;;;; CONS assessor derive type optimizers.

(defoptimizer (car derive-type) ((cons))
  (let ((type (continuation-type cons)))
    (cond ((eq type *null-type*)
	   *null-type*)
	  ((cons-type-p type)
	   (cons-type-car-type type)))))

(defoptimizer (cdr derive-type) ((cons))
  (let ((type (continuation-type cons)))
    (cond ((eq type *null-type*)
	   *null-type*)
	  ((cons-type-p type)
	   (cons-type-cdr-type type)))))

(defoptimizer (map derive-type) ((output-spec function seq &rest more-seq))
  ;; The result type of MAP is OUTPUT-SPEC, except when OUTPUT-SPEC is
  ;; NIL, in which case the result-type is NULL.
  (when (constant-continuation-p output-spec)
    (let ((spec (continuation-value output-spec)))
      (if spec
	  (specifier-type spec)
	  (specifier-type 'null)))))

(defun output-spec-sequence (output-spec)
  ;; Check that the given output-spec is a subtype of sequence.  If
  ;; so, the result is the output-spec.  Give a warning if it's not a
  ;; sequence specifier.
  (when (constant-continuation-p output-spec)
    ;; Only handle constant continuations.
    (let ((spec (continuation-value output-spec)))
      (if (subtypep spec 'sequence)
	  (specifier-type spec)
	  (compiler-warning "Specified output type ~S is not a sequence type" spec)))))

(defoptimizer (concatenate derive-type) ((output-spec  seq &rest more-seq))
  ;; The result type of CONCATENATE is OUTPUT-SPEC, but check to see
  ;; if it makes sense.
  (output-spec-sequence output-spec))

(defoptimizer (make-sequence derive-type) ((output-spec  seq &rest more-seq))
  ;; The result type of MAKE-SEQUENCE is OUTPUT-SPEC, but check to see
  ;; if it makes sense.
  (output-spec-sequence output-spec))
