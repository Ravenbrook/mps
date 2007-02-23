;;; -*- Log: code.log; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/interr.lisp,v 1.45 2006/06/30 18:41:22 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions and macros to define and deal with internal errors (i.e.
;;; problems that can be signaled from assembler code).
;;;
;;; Written by William Lott.
;;;

(in-package "KERNEL")

(export '(infinite-error-protect find-caller-name *maximum-error-depth*
	  #+stack-checking red-zone-hit #+stack-checking yellow-zone-hit
          #+heap-overflow-check dynamic-space-overflow-error-hit
          #+heap-overflow-check dynamic-space-overflow-warning-hit))



;;;; Internal Errors

(defvar *internal-errors*
  (macrolet ((frob ()
	       (map 'vector #'cdr (c:backend-internal-errors c:*backend*))))
    (frob)))


(eval-when (compile eval)

(defmacro deferr (name args &rest body)
  (let* ((rest-pos (position '&rest args))
	 (required (if rest-pos (subseq args 0 rest-pos) args))
	 (fp (gensym))
	 (sigcontext (gensym))
	 (sc-offsets (gensym))
	 (temp (gensym))
	 (fn-name (symbolicate name "-HANDLER")))
    `(progn
       (defun ,fn-name (name ,fp ,sigcontext ,sc-offsets)
	 (declare (ignorable name ,fp ,sigcontext ,sc-offsets))
	 (macrolet ((set-value (var value)
		      (let ((pos (position var ',required)))
			(unless pos
			  (error "~S isn't one of the required args."
				 var))
			`(let ((,',temp ,value))
			   (di::sub-set-debug-var-slot
			    ,',fp (nth ,pos ,',sc-offsets)
			    ,',temp ,',sigcontext)
			   (setf ,var ,',temp)))))
	   (let (,@(let ((offset -1))
		     (mapcar #'(lambda (var)
				 `(,var (di::sub-access-debug-var-slot
					 ,fp
					 (nth ,(incf offset)
					      ,sc-offsets)
					 ,sigcontext)))
			     required))
		   ,@(when rest-pos
		       `((,(nth (1+ rest-pos) args)
			  (mapcar #'(lambda (sc-offset)
				      (di::sub-access-debug-var-slot
				       ,fp
				       sc-offset
				       ,sigcontext))
				  (nthcdr ,rest-pos ,sc-offsets))))))
	     ,@body)))
       (setf (svref *internal-errors* ,(error-number-or-lose name))
	     #',fn-name))))

) ; Eval-When (Compile Eval)



(deferr unknown-error (&rest args)
  (error "Unknown error:~{ ~S~})" args))

(deferr object-not-function-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'function))

(deferr object-not-list-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'list))

(deferr object-not-bignum-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'bignum))

(deferr object-not-ratio-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'ratio))

(deferr object-not-single-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'single-float))

(deferr object-not-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'double-float))

#+long-float
(deferr object-not-long-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'long-float))

#+double-double
(deferr object-not-double-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'double-double-float))

(deferr object-not-simple-string-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-string))

(deferr object-not-simple-bit-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-bit-vector))

(deferr object-not-simple-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-vector))

(deferr object-not-fixnum-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'fixnum))

(deferr object-not-function-or-symbol-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(or function symbol)))

(deferr object-not-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'vector))

(deferr object-not-string-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'string))

(deferr object-not-bit-vector-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'bit-vector))

(deferr object-not-array-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'array))

(deferr object-not-number-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'number))

(deferr object-not-rational-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'rational))

(deferr object-not-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'float))

(deferr object-not-real-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'real))

(deferr object-not-integer-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'integer))

(deferr object-not-cons-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'cons))

(deferr object-not-symbol-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'symbol))

(deferr undefined-symbol-error (fdefn-or-symbol)
  (error 'undefined-function
	 :function-name name
	 :name (etypecase fdefn-or-symbol
		 (symbol fdefn-or-symbol)
		 (fdefn (fdefn-name fdefn-or-symbol)))))

(deferr object-not-coercable-to-function-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'coercable-to-function))

(deferr invalid-argument-count-error (nargs)
  (error 'simple-program-error
	 :function-name name
	 :format-control "Invalid number of arguments: ~S"
	 :format-arguments (list nargs)))

(deferr bogus-argument-to-values-list-error (list)
  (error 'simple-type-error
	 :function-name name
	 :datum list
	 :expected-type 'list
	 :format-control "Attempt to use VALUES-LIST on a dotted-list:~%  ~S"
	 :format-arguments (list list)))

(deferr unbound-symbol-error (symbol)
  (error 'unbound-variable :function-name name :name symbol))

(deferr object-not-base-char-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'base-char))

(deferr object-not-sap-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'system-area-pointer))

(deferr invalid-unwind-error ()
  (error 'simple-control-error
	 :function-name name
	 :format-control
	 "Attempt to RETURN-FROM a block or GO to a tag that no longer exists"))

(deferr unseen-throw-tag-error (tag)
  (error 'simple-control-error
	 :function-name name
	 :format-control "Attempt to THROW to a tag that does not exist: ~S"
	 :format-arguments (list tag)))

(deferr nil-function-returned-error (function)
  (error 'simple-control-error
	 :function-name name
	 :format-control
	 "Function with declared result type NIL returned:~%  ~S"
	 :format-arguments (list function)))

(deferr division-by-zero-error (this that)
  (error 'division-by-zero
	 :function-name name
	 :operation 'division
	 :operands (list this that)))

(deferr object-not-type-error (object type)
  (error (if (and (%instancep object)
		  (layout-invalid (%instance-layout object)))
	     'layout-invalid
	     'type-error)
	 :function-name name
	 :datum object
	 :expected-type type))

(deferr layout-invalid-error (object layout)
  (error 'layout-invalid
	 :function-name name
	 :datum object
	 :expected-type (layout-class layout)))

(deferr odd-keyword-arguments-error ()
  (error 'simple-program-error
	 :function-name name
	 :format-control "Odd number of keyword arguments."))

(deferr unknown-keyword-argument-error (key)
  (error 'simple-program-error
	 :function-name name
	 :format-control "Unknown keyword: ~S"
	 :format-arguments (list key)))

(deferr invalid-array-index-error (array bound index)
  (error 'type-error
	 :function-name name
	 :datum index
	 :expected-type `(integer 0 (,bound))
	 :format-control
	 (cond ((zerop bound)
		"Invalid array index, ~D for ~S.  Array has no elements.")
	       ((minusp index)
		"Invalid array index, ~D for ~S.  Should have greater than or equal to 0.")
	       (t
		"Invalid array index, ~D for ~S.  Should have been less than ~D"))
	 :format-arguments (list index array bound)))

(deferr object-not-simple-array-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'simple-array))

(deferr object-not-signed-byte-32-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(signed-byte 32)))

(deferr object-not-unsigned-byte-32-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(unsigned-byte 32)))

(deferr object-not-simple-array-unsigned-byte-2-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 2) (*))))

(deferr object-not-simple-array-unsigned-byte-4-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 4) (*))))

(deferr object-not-simple-array-unsigned-byte-8-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 8) (*))))

(deferr object-not-simple-array-unsigned-byte-16-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 16) (*))))

(deferr object-not-simple-array-unsigned-byte-32-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (unsigned-byte 32) (*))))

(deferr object-not-simple-array-signed-byte-8-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (signed-byte 8) (*))))

(deferr object-not-simple-array-signed-byte-16-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (signed-byte 16) (*))))

(deferr object-not-simple-array-signed-byte-30-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (signed-byte 30) (*))))

(deferr object-not-simple-array-signed-byte-32-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (signed-byte 32) (*))))

(deferr object-not-simple-array-single-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array single-float (*))))

(deferr object-not-simple-array-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array double-float (*))))

#+double-double
(deferr object-not-simple-array-double-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array double-double-float (*))))

(deferr object-not-simple-array-complex-single-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (complex single-float) (*))))

(deferr object-not-simple-array-complex-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (complex double-float) (*))))

#+long-float
(deferr object-not-simple-array-complex-long-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (complex long-float) (*))))

#+double-double
(deferr object-not-simple-array-complex-double-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(simple-array (complex double-double-float) (*))))

(deferr object-not-complex-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'complex))

(deferr object-not-complex-rational-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex rational)))

(deferr object-not-complex-single-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex single-float)))

(deferr object-not-complex-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex double-float)))

#+long-float
(deferr object-not-complex-long-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex long-float)))

#+double-double
(deferr object-not-complex-double-double-float-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type '(complex double-double-float)))

(deferr object-not-weak-pointer-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'weak-pointer))

(deferr object-not-instance-error (object)
  (error 'type-error
	 :function-name name
	 :datum object
	 :expected-type 'instance))

#+linkage-table
(deferr undefined-foreign-symbol-error (symbol)
  (error 'simple-program-error
         :function-name name
	 :format-control "Undefined foreign symbol: ~S"
	 :format-arguments (list symbol)))


;;; INFINITE-ERROR-PROTECT is used by ERROR and friends to keep us out of
;;; hyperspace.
;;;
(defmacro infinite-error-protect (&rest forms)
  `(if (and (boundp '*error-system-initialized*)
	    (numberp *current-error-depth*))
       (let ((*current-error-depth* (1+ *current-error-depth*)))
	 (if (> *current-error-depth* *maximum-error-depth*)
	     (error-error "Help! " *current-error-depth* " nested errors.  "
			  "KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.")
	     (progn ,@forms)))
       (%primitive halt)))

;;; Track the depth of recursive errors.
;;;
(defvar *maximum-error-depth* 10
  "The maximum number of nested errors allowed.  Internal errors are
   double-counted.")
(defvar *current-error-depth* 0 "The current number of nested errors.")

;;; These specials are used by ERROR-ERROR to track the success of recovery
;;; attempts.
;;;
(defvar *error-error-depth* 0)
(defvar *error-throw-up-count* 0)

;;; This protects against errors that happen before we run this top-level form.
;;;
(defvar *error-system-initialized* t)

;;; ERROR-ERROR can be called when the error system is in trouble and needs
;;; to punt fast.  Prints a message without using format.  If we get into
;;; this recursively, then halt.
;;;
(defun error-error (&rest messages)
  (let ((*error-error-depth* (1+ *error-error-depth*)))
    (when (> *error-throw-up-count* 50)
      (%primitive halt)
      (throw 'lisp::top-level-catcher nil))
    (case *error-error-depth*
      (1)
      (2
       (lisp::stream-init))
      (3
       (incf *error-throw-up-count*)
       (throw 'lisp::top-level-catcher nil))
      (t
       (%primitive halt)
       (throw 'lisp::top-level-catcher nil)))

    (with-standard-io-syntax
      (let ((*print-readably* nil))
	(dolist (item messages) (princ item *terminal-io*))
	(debug:internal-debug)))))


;;;; Fetching errorful function name.

;;; Used to prevent infinite recursive lossage when we can't find the caller
;;; for some reason.
;;;
(defvar *finding-name* nil)

;;; FIND-CALLER-NAME  --  Internal
;;;
(defun find-caller-name ()
  (if *finding-name*
      (values "<error finding name>" nil)
      (handler-case
	  (let* ((*finding-name* t)
		 (frame (di:frame-down (di:frame-down (di:top-frame))))
		 (name (di:debug-function-name
			(di:frame-debug-function frame))))
	    (di:flush-frames-above frame)
	    (values name frame))
	(error ()
	  (values "<error finding name>" nil))
	(di:debug-condition ()
	  (values "<error finding name>" nil)))))


(defun find-interrupted-name ()
  (if *finding-name*
      (values "<error finding name>" nil)
      (handler-case
	  (let ((*finding-name* t))
	    (do ((frame (di:top-frame) (di:frame-down frame)))
		((null frame)
		 (values "<error finding name>" nil))
	      (when (and (di::compiled-frame-p frame)
			 (di::compiled-frame-escaped frame))
		(di:flush-frames-above frame)
		(return (values (di:debug-function-name
				 (di:frame-debug-function frame))
				frame)))))
	(error ()
	  (values "<error finding name>" nil))
	(di:debug-condition ()
	  (values "<error finding name>" nil)))))


;;;; internal-error signal handler.

(defun internal-error (scp continuable)
  (declare (type system-area-pointer scp) (ignore continuable))
  (infinite-error-protect
   (let ((scp (locally
		(declare (optimize (inhibit-warnings 3)))
		(alien:sap-alien scp (* unix:sigcontext)))))
     (multiple-value-bind
	 (error-number arguments)
	 (vm:internal-error-arguments scp)
       (multiple-value-bind
	   (name debug:*stack-top-hint*)
	   (find-interrupted-name)
	 (let ((fp (int-sap (vm:sigcontext-register scp vm::cfp-offset)))
	       (handler (and (< -1 error-number (length *internal-errors*))
			     (svref *internal-errors* error-number))))
	   (cond ((null handler)
		  (error 'simple-error
			 :function-name name
			 :format-control
			 "Unknown internal error, ~D?  args=~S"
			 :format-arguments
			 (list error-number
			       (mapcar #'(lambda (sc-offset)
					   (di::sub-access-debug-var-slot
					    fp sc-offset scp))
				       arguments))))
		 ((not (functionp handler))
		  (error 'simple-error
			 :function-name name
			 :format-control
			 "Internal error ~D: ~A.  args=~S"
			 :format-arguments
			 (list error-number
			       handler
			       (mapcar #'(lambda (sc-offset)
					   (di::sub-access-debug-var-slot
					    fp sc-offset scp))
				       arguments))))
		 (t
		  (funcall handler name fp scp arguments)))))))))

;;;
;;; Called from C when the yellow control stack guard zone is hit.
;;; The yellow zone is unprotected in the C code prior to calling this
;;; function, to give some room for debugging.  The red zone is still
;;; protected.
;;;
#+stack-checking
(defun yellow-zone-hit ()
  (let ((debug:*stack-top-hint* nil))
    (format *error-output*
	    "~2&~@<A control stack overflow has occurred: ~
            the program has entered the yellow control stack guard zone.  ~
            Please note that you will be returned to the Top-Level if you ~
            enter the red control stack guard zone while debugging.~@:>~2%")
    (infinite-error-protect (error 'stack-overflow))))

;;;
;;; Called from C when the red control stack guard zone is hit.  We
;;; could ABORT here, which would usually take us back to the debugger
;;; or top-level, and add code to the restarts re-protecting the red
;;; zone (which can't be done here because we're still in the red
;;; zone).  Using ABORT is too dangerous because users may be using
;;; abort restarts which don't do the necessary re-protecting of the
;;; red zone, and would thus render CMUCL unprotected.
;;;
#+stack-checking
(defun red-zone-hit ()
  (format *error-output*
	  "~2&~@<Fatal control stack overflow.  You have entered ~
           the red control stack guard zone while debugging.  ~
           Returning to Top-Level.~@:>~2%")
  (throw 'lisp::top-level-catcher nil))

#+heap-overflow-check
(defun dynamic-space-overflow-warning-hit ()
  (let ((debug:*stack-top-hint* nil))
    ;; Don't reserve any more pages
    (setf lisp::reserved-heap-pages 0)
    (format *error-output*
	    "~2&~@<Imminent dynamic space overflow has occurred:  ~
            Only a small amount of dynamic space is available now. ~
            Please note that you will be returned to the Top-Level without ~
            warning if you run out of space while debugging.~@:>~%")
    (infinite-error-protect (error 'heap-overflow))))

#+heap-overflow-check
(defun dynamic-space-overflow-error-hit ()
  (throw 'lisp::top-level-catcher nil))

