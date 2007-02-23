;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/byte-interp.lisp,v 1.45 2006/06/30 18:41:22 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the noise to interpret byte-compiled stuff.
;;;
;;; Written by William Lott
;;;
(in-package "C")

(in-package "KERNEL")
(export '(byte-function byte-function-name initialize-byte-compiled-function
			byte-closure byte-closure-function
			byte-closure-data byte-function-or-closure
			byte-function-type
			*eval-stack* *eval-stack-top*))
(in-package "C")

;;; We need at least this level of debug-info in order for the local
;;; declaration in with-debugger-info to take effect.
;;;
(declaim (optimize (debug 2)))


;;;; Types.

(deftype stack-pointer ()
  `(integer 0 ,(1- most-positive-fixnum)))

(defconstant max-pc (1- (ash 1 24)))

(deftype pc ()
  `(integer 0 ,max-pc))

(deftype return-pc ()
  `(integer ,(- max-pc) ,max-pc))


;;; These dummies are defined here, to make a build without compiler
;;; work.

(defun %dynamic-extent (kind sp)
  (declare (ignore kind sp)))

(defun %dynamic-extent-start ())

(defun %dynamic-extent-end (kind sp)
  (declare (ignore kind sp)))


;;;; Byte functions:

;;; Abstract class represents any type of byte-compiled function.
;;;
(defstruct (byte-function-or-closure
	    (:alternate-metaclass kernel:funcallable-instance
				  kernel:funcallable-structure-class
				  kernel:make-funcallable-structure-class)
	    (:type kernel:funcallable-structure)))

;;; Represents a byte-compiled closure.
;;;
(defstruct (byte-closure
	    (:include byte-function-or-closure)
	    (:constructor make-byte-closure (function data))
	    (:type kernel:funcallable-structure)
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d)) 
	       (print-unreadable-object (s stream :identity t)
		 (format stream "Byte closure ~S"
			 (byte-function-name (byte-closure-function s)))))))
  ;;
  ;; Byte function that we call.
  (function (required-argument) :type byte-function)
  ;;
  ;; Closure data vector.
  (data (required-argument) :type simple-vector))


;;; Any non-closure byte function (including the hidden function object for a
;;; closure.)
;;;
(defstruct (byte-function (:include byte-function-or-closure)
			  (:print-function %print-byte-function)
			  (:type kernel:funcallable-structure))
  ;;
  ;; The component that this XEP is an entry point into.  NIL until
  ;; LOAD or MAKE-CORE-BYTE-COMPONENT fills it in.  They count on this being
  ;; the first slot.
  (component nil :type (or null kernel:code-component))
  ;;
  ;; Debug name of this function.
  (name nil))

;;; Fixed-argument byte function.
;;;
(defstruct (simple-byte-function (:include byte-function)
				 (:type kernel:funcallable-structure))
  ;;
  ;; The number of arguments expected.
  (num-args 0 :type (integer 0 #.call-arguments-limit))
  ;;
  ;; The start of the function.
  (entry-point 0 :type index))


;;; Variable arg-count byte function.
;;;
(defstruct (hairy-byte-function (:include byte-function)
				(:type kernel:funcallable-structure))
  ;;
  ;; The minimum and maximum number of args, ignoring &rest and &key.
  (min-args 0 :type (integer 0 #.call-arguments-limit))
  (max-args 0 :type (integer 0 #.call-arguments-limit))
  ;;
  ;; List of the entry points for min-args, min-args+1, ... max-args.
  (entry-points nil :type list)
  ;;
  ;; The entry point to use when there are more than max-args.  Only filled
  ;; in where okay.  In other words, only when &rest or &key is specified.
  (more-args-entry-point nil :type (or null (unsigned-byte 24)))
  ;;
  ;; The number of ``more-arg'' args.
  (num-more-args 0 :type (integer 0 #.call-arguments-limit))
  ;;
  ;; True if there is a rest-arg, :MORE if there is &MORE.
  (rest-arg-p nil :type (member t nil :more))
  ;;
  ;; True if there are keywords.  Note: keywords might still be NIL because
  ;; having &key with no keywords is valid and should result in
  ;; allow-other-keys processing.  If :allow-others, then allow other keys.
  (keywords-p nil :type (member t nil :allow-others))
  ;;
  ;; List of keyword arguments.  Each element is a list of:
  ;;   key, default, supplied-p.
  (keywords nil :type list))

(defun %print-byte-function (s stream d)
  (declare (ignore d))
  (print-unreadable-object (s stream :identity t)
    (format stream "Byte function ~S" (byte-function-name s))))

(declaim (freeze-type byte-function-or-closure))


;;; BYTE-FUNCTION-TYPE  --  Interface
;;;
;;;    Return a function type approximating the type of a byte compiled
;;; function.  We really only capture the arg signature.
;;;
(defun byte-function-type (x)
  (specifier-type
   (etypecase x
     (simple-byte-function
      `(function ,(make-list (simple-byte-function-num-args x)
			     :initial-element 't)
		 *))
     (hairy-byte-function
      (collect ((res))
	(let ((min (hairy-byte-function-min-args x))
	      (max (hairy-byte-function-max-args x)))
	  (dotimes (i min) (res 't))
	  (when (> max min)
	    (res '&optional)
	    (dotimes (i (- max min))
	      (res 't))))
	(when (hairy-byte-function-rest-arg-p x)
	  (res '&rest 't))
	(ecase (hairy-byte-function-keywords-p x)
	  ((t :allow-others)
	   (res '&key)
	   (dolist (key (hairy-byte-function-keywords x))
		   (res `(,(car key) t)))
	   (if (eql (hairy-byte-function-keywords-p x) :allow-others)
	       (res '&allow-other-keys)))
	  ((nil)))
	`(function ,(res) *))))))


;;;; The stack.

(defvar *eval-stack* (make-array 100)
  "This is the interpreter's evaluation stack.")
(declaim (type simple-vector *eval-stack*))

(defvar *eval-stack-top* 0
  "This is the next free element of the interpreter's evaluation stack.")
(declaim (type index *eval-stack-top*))

(defmacro current-stack-pointer () '*eval-stack-top*)

(declaim (inline eval-stack-ref))
(defun eval-stack-ref (offset)
  (declare (type stack-pointer offset))
  (svref eval::*eval-stack* offset))

(declaim (inline (setf eval-stack-ref)))
(defun (setf eval-stack-ref) (new-value offset)
  (declare (type stack-pointer offset))
  (setf (svref eval::*eval-stack* offset) new-value))

(defun push-eval-stack (value)
  (let ((len (length (the simple-vector eval::*eval-stack*)))
	(sp (current-stack-pointer)))
    (when (= len sp)
      (let ((new-stack (make-array (ash len 1))))
	(replace new-stack eval::*eval-stack* :end1 len :end2 len)
	(setf eval::*eval-stack* new-stack)))
    (setf (current-stack-pointer) (1+ sp))
    (setf (eval-stack-ref sp) value)))

(defun allocate-eval-stack (amount)
  (let* ((len (length (the simple-vector eval::*eval-stack*)))
	 (sp (current-stack-pointer))
	 (new-sp (+ sp amount)))
    (declare (type index sp new-sp))
    (when (>= new-sp len)
      (let ((new-stack (make-array (ash new-sp 1))))
	(replace new-stack eval::*eval-stack* :end1 len :end2 len)
	(setf eval::*eval-stack* new-stack)))
    (setf (current-stack-pointer) new-sp)
    (let ((stack eval::*eval-stack*))
      (do ((i sp (1+ i)))
	  ((= i new-sp))
	(setf (svref stack i) '#:uninitialized))))
  (undefined-value))

(defun pop-eval-stack ()
  (let* ((new-sp (1- (current-stack-pointer)))
	 (value (eval-stack-ref new-sp)))
    (setf (current-stack-pointer) new-sp)
    value))

(defmacro multiple-value-pop-eval-stack ((&rest vars) &body body)
  (declare (optimize (inhibit-warnings 3)))
  (let ((num-vars (length vars))
	(index -1)
	(new-sp-var (gensym "NEW-SP-"))
	(decls nil))
    (loop
      (unless (and (consp body) (consp (car body)) (eq (caar body) 'declare))
	(return))
      (push (pop body) decls))
    `(let ((,new-sp-var (- (current-stack-pointer) ,num-vars)))
       (declare (type stack-pointer ,new-sp-var))
       (let ,(mapcar #'(lambda (var)
			 `(,var (eval-stack-ref
				 (+ ,new-sp-var ,(incf index)))))
		     vars)
	 ,@(nreverse decls)
	 (setf (current-stack-pointer) ,new-sp-var)
	 ,@body))))

(defun stack-copy (dest src count)
  (declare (type stack-pointer dest src count))
  (let ((stack *eval-stack*))
    (if (< dest src)
	(dotimes (i count)
	  (setf (svref stack dest) (svref stack src))
	  (incf dest)
	  (incf src))
	(do ((si (1- (+ src count))
		 (1- si))
	     (di (1- (+ dest count))
		 (1- di)))
	    ((< si src))
	  (declare (fixnum si di))
	  (setf (svref stack di) (svref stack si)))))
  (undefined-value))


;;;; Component access magic.

(declaim (inline component-ref))
(defun component-ref (component pc)
  (declare (type code-component component)
	   (type pc pc))
  (system:sap-ref-8 (code-instructions component) pc))

(declaim (inline (setf component-ref)))
(defun (setf component-ref) (value component pc)
  (declare (type (unsigned-byte 8) value)
	   (type code-component component)
	   (type pc pc))
  (setf (system:sap-ref-8 (code-instructions component) pc) value))

(declaim (inline component-ref-signed))
(defun component-ref-signed (component pc)
  (let ((byte (component-ref component pc)))
    (if (logbitp 7 byte)
	(logior (ash -1 8) byte)
	byte)))

(declaim (inline component-ref-24))
(defun component-ref-24 (component pc)
  (logior (ash (component-ref component pc) 16)
	  (ash (component-ref component (1+ pc)) 8)
	  (component-ref component (+ pc 2))))


;;;; Debugging support.

;;; WITH-DEBUGGER-INFO -- internal.
;;;
;;; This macro binds three magic variables.  When the debugger notices that
;;; these three variables are bound, it makes a byte-code frame out of the
;;; supplied information instead of a compiled frame.  We set each var in
;;; addition to binding it so the compiler doesn't optimize away the binding.
;;;
(defmacro with-debugger-info ((component pc fp) &body body)
  `(let ((%byte-interp-component ,component)
	 (%byte-interp-pc ,pc)
	 (%byte-interp-fp ,fp))
     (declare (optimize (debug 3)))
     (setf %byte-interp-component %byte-interp-component)
     (setf %byte-interp-pc %byte-interp-pc)
     (setf %byte-interp-fp %byte-interp-fp)
     ,@body))


(defun byte-install-breakpoint (component pc)
  (declare (type code-component component)
	   (type pc pc)
	   (values (unsigned-byte 8)))
  (let ((orig (component-ref component pc)))
    (setf (component-ref component pc)
	  #.(logior byte-xop
		    (xop-index-or-lose 'breakpoint)))
    orig))

(defun byte-remove-breakpoint (component pc orig)
  (declare (type code-component component)
	   (type pc pc)
	   (type (unsigned-byte 8) orig)
	   (values (unsigned-byte 8)))
  (setf (component-ref component pc) orig))

(defun byte-skip-breakpoint (component pc fp orig)
  (declare (type code-component component)
	   (type pc pc)
	   (type stack-pointer fp)
	   (type (unsigned-byte 8) orig))
  (byte-interpret-byte component fp pc orig))




;;;; System constants

;;; A table mapping system constant indices to run-time values.  We don't
;;; reference the compiler variable at load time, since the interpreter is
;;; loaded first.
;;;
(defparameter system-constants
  (let ((res (make-array 256)))
    (dolist (x '#.(collect ((res))
		    (do-hash (key value *system-constant-codes*)
		      (res (cons key value)))
		    (res)))
      (let ((key (car x))
	    (value (cdr x)))
	(setf (svref res value)
	      (if (and (consp key) (eq (car key) '%fdefinition-marker%))
		  (lisp::fdefinition-object (cdr key) t)
		  key))))
    res))


;;;; Byte compiled function constructors/extractors.

(defun initialize-byte-compiled-function (xep)
  (declare (type byte-function xep))
  (push xep (code-header-ref (byte-function-component xep)
			     vm:code-trace-table-offset-slot))
  (setf (funcallable-instance-function xep)
	#'(instance-lambda (&more context count)
	    (let ((old-sp (current-stack-pointer)))
	      (declare (type stack-pointer old-sp))
	      (dotimes (i count)
		(push-eval-stack (%more-arg context i)))
	      (invoke-xep nil 0 old-sp 0 count xep))))
  xep)

(defun make-byte-compiled-closure (xep closure-vars)
  (declare (type byte-function xep)
	   (type simple-vector closure-vars))
  (let ((res (make-byte-closure xep closure-vars)))
    (setf (funcallable-instance-function res)
	  #'(instance-lambda (&more context count)
	      (let ((old-sp (current-stack-pointer)))
		(declare (type stack-pointer old-sp))
		(dotimes (i count)
		  (push-eval-stack (%more-arg context i)))
		(invoke-xep nil 0 old-sp 0 count
			    (byte-closure-function res)
			    (byte-closure-data res)))))
    res))


;;;; Inlines.

(eval-when (compile eval)
  (setq ext:*inline-expansion-limit* 400))

(defmacro expand-into-inlines ()
  (declare (optimize (inhibit-warnings 3)))
  (iterate build-dispatch
	   ((bit 4)
	    (base 0))
    (if (minusp bit)
	(let ((info (svref *inline-functions* base)))
	  (if info
	      (let* ((spec (type-specifier
			    (inline-function-info-type info)))
		     (arg-types (second spec))
		     (result-type (third spec))
		     (args (mapcar #'(lambda (x)
				       (declare (ignore x))
				       (gensym))
				   arg-types))
		     (func
		      `(the ,result-type
			    (,(inline-function-info-interpreter-function info)
			     ,@args))))
		`(multiple-value-pop-eval-stack ,args
		   (declare ,@(mapcar #'(lambda (type var)
					  `(type ,type ,var))
				      arg-types args))
		   ,(if (and (consp result-type)
			     (eq (car result-type) 'values))
			(let ((results
			       (mapcar #'(lambda (x)
					   (declare (ignore x))
					   (gensym))
				       (cdr result-type))))
			  `(multiple-value-bind
			       ,results ,func
			     ,@(mapcar #'(lambda (res)
					   `(push-eval-stack ,res))
				       results)))
			`(push-eval-stack ,func))))
	      `(error "Unknown inline function, id=~D" ,base)))
	`(if (zerop (logand byte ,(ash 1 bit)))
	     ,(build-dispatch (1- bit) base)
	     ,(build-dispatch (1- bit) (+ base (ash 1 bit)))))))


(declaim (inline value-cell-setf))
(defun value-cell-setf (value cell)
  (value-cell-set cell value)
  value)

(declaim (inline setf-symbol-value))
(defun setf-symbol-value (value symbol)
  (setf (symbol-value symbol) value))

(declaim (inline %setf-instance-ref))
(defun %setf-instance-ref (new-value instance index)
  (setf (%instance-ref instance index) new-value))

(eval-when (compile)

(defmacro %byte-symbol-value (x)
  `(let ((x ,x))
     (unless (boundp x)
       (with-debugger-info (component pc fp)
	 (error "Unbound variable: ~S" x)))
     (symbol-value x)))

(defmacro %byte-car (x)
  `(let ((x ,x))
     (unless (listp x)
       (with-debugger-info (component pc fp)
	 (error 'simple-type-error :datum x :expected-type 'list
		:format-control "Non-list argument to CAR: ~S"
		:format-arguments (list x))))
     (car x)))

(defmacro %byte-cdr (x)
  `(let ((x ,x))
     (unless (listp x)
       (with-debugger-info (component pc fp)
	 (error 'simple-type-error :datum x :expected-type 'list
		:format-control "Non-list argument to CDR: ~S"
		:format-arguments (list x))))
     (cdr x)))

); eval-when (compile)

(declaim (inline %byte-special-bind))
(defun %byte-special-bind (value symbol)
  (system:%primitive bind value symbol)
  (values))

(declaim (inline %byte-special-unbind))
(defun %byte-special-unbind ()
  (system:%primitive unbind)
  (values))

;;; obsolete...
(declaim (inline cons-unique-tag))
(defun cons-unique-tag ()
  (list '#:%unique-tag%))


;;;; Two-arg function stubs:
;;;
;;; We have two-arg versions of some n-ary functions that are normally
;;; open-coded.

(defun two-arg-char= (x y) (char= x y))
(defun two-arg-char< (x y) (char< x y))
(defun two-arg-char> (x y) (char> x y))
(defun two-arg-char-equal (x y) (char-equal x y))
(defun two-arg-char-lessp (x y) (char-lessp x y))
(defun two-arg-char-greaterp (x y) (char-greaterp x y))
(defun two-arg-string= (x y) (string= x y))
(defun two-arg-string< (x y) (string< x y))
(defun two-arg-string> (x y) (string> x y))


;;;; Misc primitive stubs:

(macrolet ((frob (name &optional (args '(x)))
	     `(defun ,name ,args (,name ,@args))))
  (frob %CODE-CODE-SIZE)
  (frob %CODE-DEBUG-INFO)
  (frob %CODE-ENTRY-POINTS)
  (frob %FUNCALLABLE-INSTANCE-FUNCTION)
  (frob %FUNCALLABLE-INSTANCE-LAYOUT)
  (frob %FUNCALLABLE-INSTANCE-LEXENV)
  (frob %FUNCTION-NEXT)
  (frob %FUNCTION-SELF)
  (frob %SET-FUNCALLABLE-INSTANCE-FUNCTION (fin new-val))
  (frob %SXHASH-SIMPLE-SUBSTRING (str count)))


;;;; Funny functions:

;;; used by both byte and IR1 interpreters.
;;;
(defun %progv (vars vals fun)
  (progv vars vals
    (funcall fun)))


;;;; XOPs

;;; Extension operations (XOPs) are random magic things that the byte
;;; interpreter needs to do, but can't be represented as a function call.
;;; When the byte interpreter encounters an XOP in the byte stream, it
;;; tail-calls the corresponding XOP routine extracted from *byte-xops*.
;;; The XOP routine can do whatever it wants, probably re-invoking the
;;; byte interpreter.

;;; Fetch and 8/24 bit operand out of the code stream.
;;;
(eval-when (compile eval)
  (defmacro with-extended-operand ((component pc operand new-pc)
				   &body body)
    (once-only ((n-component component)
		(n-pc pc))
      `(multiple-value-bind
	   (,operand ,new-pc)
	   (let ((,operand (component-ref ,n-component ,n-pc)))
	     (if (= ,operand #xff)
		 (values (component-ref-24 ,n-component (1+ ,n-pc))
			 (+ ,n-pc 4))
		 (values ,operand (1+ ,n-pc))))
	 (declare (type index ,operand ,new-pc))
	 ,@body))))


;;; UNDEFINED-XOP -- internal.
;;;
;;; If a real XOP hasn't been defined, this gets invoked and signals an
;;; error.  This shouldn't happen in normal operation.
;;;
(defun undefined-xop (component old-pc pc fp)
  (declare (ignore component old-pc pc fp))
  (error "Undefined XOP."))

;;; *BYTE-XOPS* -- Simple vector of the XOP functions.
;;; 
(declaim (type (simple-vector 256) *byte-xops*))
(defvar *byte-xops*
  (make-array 256 :initial-element #'undefined-xop))

;;; DEFINE-XOP -- internal.
;;;
;;; Define a XOP function and install it in *BYTE-XOPS*.
;;; 
(eval-when (compile eval)
  (defmacro define-xop (name lambda-list &body body)
    (let ((defun-name (symbolicate "BYTE-" name "-XOP")))
      `(progn
	 (defun ,defun-name ,lambda-list
	   ,@body)
	 (setf (aref *byte-xops* ,(xop-index-or-lose name)) #',defun-name)
	 ',defun-name))))

;;; BREAKPOINT -- Xop.
;;;
;;; This is spliced in by the debugger in order to implement breakpoints.
;;; 
(define-xop breakpoint (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc)
	   (ignore pc)
	   (type stack-pointer fp))
  ;; Invoke the debugger.
  (with-debugger-info (component old-pc fp)
    (di::handle-breakpoint component old-pc fp))
  ;; Retry the breakpoint XOP in case it was replaced with the original
  ;; displaced byte-code.
  (byte-interpret component old-pc fp))

;;; DUP -- Xop.
;;;
;;; This just duplicates whatever is on the top of the stack.
;;;
(define-xop dup (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let ((value (eval-stack-ref (1- (current-stack-pointer)))))
    (push-eval-stack value))
  (byte-interpret component pc fp))

;;; MAKE-CLOSURE -- Xop.
;;; 
(define-xop make-closure (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let* ((num-closure-vars (pop-eval-stack))
	 (closure-vars (make-array num-closure-vars)))
    (declare (type index num-closure-vars)
	     (type simple-vector closure-vars))
    (iterate frob ((index (1- num-closure-vars)))
      (unless (minusp index)
	(setf (svref closure-vars index) (pop-eval-stack))
	(frob (1- index))))
    (push-eval-stack (make-byte-compiled-closure (pop-eval-stack)
						 closure-vars)))
  (byte-interpret component pc fp))

(define-xop merge-unknown-values (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (labels ((grovel (remaining-blocks block-count-ptr)
	     (declare (type index remaining-blocks)
		      (type stack-pointer block-count-ptr))
	     (declare (values index stack-pointer))
	     (let ((block-count (eval-stack-ref block-count-ptr)))
	       (declare (type index block-count))
	       (if (= remaining-blocks 1)
		   (values block-count block-count-ptr)
		   (let ((src (- block-count-ptr block-count)))
		     (declare (type index src))
		     (multiple-value-bind
			 (values-above dst)
			 (grovel (1- remaining-blocks) (1- src))
		       (stack-copy dst src block-count)
		       (values (+ values-above block-count)
			       (+ dst block-count))))))))
    (multiple-value-bind
	(total-count end-ptr)
	(grovel (pop-eval-stack) (1- (current-stack-pointer)))
      (setf (eval-stack-ref end-ptr) total-count)
      (setf (current-stack-pointer) (1+ end-ptr))))
  (byte-interpret component pc fp))

(define-xop default-unknown-values (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let* ((desired (pop-eval-stack))
	 (supplied (pop-eval-stack))
	 (delta (- desired supplied)))
    (declare (type index desired supplied)
	     (type fixnum delta))
    (cond ((minusp delta)
	   (incf (current-stack-pointer) delta))
	  ((plusp delta)
	   (dotimes (i delta)
	     (push-eval-stack nil)))))
  (byte-interpret component pc fp))

;;; THROW -- XOP
;;;
;;; %THROW is compiled down into this xop.  The stack contains the tag, the
;;; values, and then a count of the values.  We special case various small
;;; numbers of values to keep from consing if we can help it.
;;;
;;; Basically, we just extract the values and the tag and then do a throw.
;;; The native compiler will convert this throw into whatever is necessary
;;; to throw, so we don't have to duplicate all that cruft.
;;;
(define-xop throw (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc)
	   (ignore pc)
	   (type stack-pointer fp))
  (let ((num-results (pop-eval-stack)))
    (declare (type index num-results))
    (case num-results
      (0
       (let ((tag (pop-eval-stack)))
	 (with-debugger-info (component old-pc fp)
	   (throw tag (values)))))
      (1
       (multiple-value-pop-eval-stack
	   (tag result)
	 (with-debugger-info (component old-pc fp)
	   (throw tag result))))
      (2
       (multiple-value-pop-eval-stack
	   (tag result0 result1)
	 (with-debugger-info (component old-pc fp)
	   (throw tag (values result0 result1)))))
      (t
       (let ((results nil))
	 (dotimes (i num-results)
	   (push (pop-eval-stack) results))
	 (let ((tag (pop-eval-stack)))
	   (with-debugger-info (component old-pc fp)
	     (throw tag (values-list results)))))))))

;;; CATCH -- XOP
;;;
;;; This is used for both CATCHes and BLOCKs that are closed over.  We
;;; establish a catcher for the supplied tag (from the stack top), and
;;; recursivly enter the byte interpreter.  If the byte interpreter exits,
;;; it must have been because of a BREAKUP (see below), so we branch (by
;;; tail-calling the byte interpreter) to the pc returned by BREAKUP.
;;; If we are thrown to, then we branch to the address encoded in the 3 bytes
;;; following the catch XOP.
;;; 
(define-xop catch (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let ((new-pc (block nil
		  (let ((results
			 (multiple-value-list
			  (catch (pop-eval-stack)
			    (return (byte-interpret component (+ pc 3) fp))))))
		    (let ((num-results 0))
		      (declare (type index num-results))
		      (dolist (result results)
			(push-eval-stack result)
			(incf num-results))
		      (push-eval-stack num-results))
		    (component-ref-24 component pc)))))
    (byte-interpret component new-pc fp)))

;;; BREAKUP -- XOP
;;;
;;; Blow out of the dynamically nested CATCH or TAGBODY.  We just return the
;;; pc following the BREAKUP XOP and the drop-through code in CATCH or
;;; TAGBODY will do the correct thing.
;;;
(define-xop breakup (component old-pc pc fp)
  (declare (ignore component old-pc fp)
	   (type pc pc))
  pc)

;;; RETURN-FROM -- XOP
;;;
;;; This is exactly like THROW, except that the tag is the last thing on
;;; the stack instead of the first.  This is used for RETURN-FROM (hence the
;;; name).
;;; 
(define-xop return-from (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc)
	   (ignore pc)
	   (type stack-pointer fp))
  (let ((tag (pop-eval-stack))
	(num-results (pop-eval-stack)))
    (declare (type index num-results))
    (case num-results
      (0
       (with-debugger-info (component old-pc fp)
	 (throw tag (values))))
      (1
       (let ((value (pop-eval-stack)))
	 (with-debugger-info (component old-pc fp)
	   (throw tag value))))
      (2
       (multiple-value-pop-eval-stack
	   (result0 result1)
	 (with-debugger-info (component old-pc fp)
	   (throw tag (values result0 result1)))))
      (t
       (let ((results nil))
	 (dotimes (i num-results)
	   (push (pop-eval-stack) results))
	 (with-debugger-info (component old-pc fp)
	   (throw tag (values-list results))))))))

;;; TAGBODY -- XOP
;;;
;;; Similar to CATCH, except for TAGBODY.  One significant difference is that
;;; when thrown to, we don't want to leave the dynamic extent of the tagbody
;;; so we loop around and re-enter the catcher.  We keep looping until BREAKUP
;;; is used to blow out.  When that happens, we just branch to the pc supplied
;;; by BREAKUP.
;;;
(define-xop tagbody (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let* ((tag (pop-eval-stack))
	 (new-pc (block nil
		   (loop
		     (setf pc
			   (catch tag
			     (return (byte-interpret component pc fp))))))))
    (byte-interpret component new-pc fp)))

;;; GO -- XOP
;;;
;;; Yup, you guessed it.  This XOP implements GO.  There are no values to
;;; pass, so we don't have to mess with them, and multiple exits can all be
;;; using the same tag so we have to pass the pc we want to go to.
;;;
(define-xop go (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc pc)
	   (type stack-pointer fp))
  (let ((tag (pop-eval-stack))
	(new-pc (component-ref-24 component pc)))
    (with-debugger-info (component old-pc fp)
      (throw tag new-pc))))

;;; UNWIND-PROTECT -- XOP
;;;
;;; Unwind-protects are handled significantly different in the byte compiler
;;; and the native compiler.  Basically, we just use the native-compiler's
;;; unwind-protect, and let it worry about continuing the unwind.
;;; 
(define-xop unwind-protect (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let ((new-pc nil))
    (unwind-protect
	(setf new-pc (byte-interpret component (+ pc 3) fp))
      (unless new-pc
	;; The cleanup function expects 3 values to be one the stack, so
	;; we have to put something there.
	(push-eval-stack nil)
	(push-eval-stack nil)
	(push-eval-stack nil)
	;; Now run the cleanup code.
	(byte-interpret component (component-ref-24 component pc) fp)))
    (byte-interpret component new-pc fp)))


(define-xop fdefn-function-or-lose (component old-pc pc fp)
  (let* ((fdefn (pop-eval-stack))
	 (fun (fdefn-function fdefn)))
    (declare (type fdefn fdefn))
    (cond (fun
	   (push-eval-stack fun)
	   (byte-interpret component pc fp))
	  (t
	   (with-debugger-info (component old-pc fp)
	     (error 'undefined-function :name (fdefn-name fdefn)))))))


;;; Used to insert placeholder arguments for unused arguments to local calls.
;;;
(define-xop push-n-under (component old-pc pc fp)
  (declare (ignore old-pc))
  (with-extended-operand (component pc howmany new-pc)
    (let ((val (pop-eval-stack)))
      (allocate-eval-stack howmany)
      (push-eval-stack val))
    (byte-interpret component new-pc fp)))



;;;; Type checking:

;;;
;;; These two hashtables map between type specifiers and type predicate
;;; functions that test those types.  They are initialized according to the
;;; standard type predicates of the target system.
;;;
(defvar *byte-type-predicates* (make-hash-table :test #'equal))
(defvar *byte-predicate-types* (make-hash-table :test #'eq))

(loop for (type predicate) in
          '#.(loop for (type . predicate) in
	           (backend-type-predicates *target-backend*)
	       collect `(,(type-specifier type) ,predicate))
      do
  (let ((fun (fdefinition predicate)))
    (setf (gethash type *byte-type-predicates*) fun)
    (setf (gethash fun *byte-predicate-types*) type)))
	    

;;; LOAD-TYPE-PREDICATE  --  Internal
;;;
;;;    Called by the loader to convert a type specifier into a type predicate
;;; (as used by the TYPE-CHECK XOP.)  If it is a structure type with a
;;; predicate or has a predefined predicate, then return the predicate
;;; function, otherwise return the CTYPE structure for the type.
;;;
(defun load-type-predicate (desc)
  (or (gethash desc *byte-type-predicates*)
      (let ((type (specifier-type desc)))
	(if (typep type 'kernel::structure-class)
	    (let ((info (layout-info (%class-layout type))))
	      (if (and info (eq (dd-type info) 'structure))
		  (let ((pred (dd-predicate info)))
		    (if (and pred (fboundp pred))
			(fdefinition pred)
			type))
		  type))
	    type))))

  
;;; TYPE-CHECK -- Xop.
;;;
;;;    Check the type of the value on the top of the stack.  The type is
;;; designated by an entry in the constants.  If the value is a function, then
;;; it is called as a type predicate.  Otherwise, the value is a CTYPE object,
;;; and we call %TYPEP on it.
;;;
(define-xop type-check (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc pc)
	   (type stack-pointer fp))
  (with-extended-operand (component pc operand new-pc)
    (let ((value (eval-stack-ref (1- (current-stack-pointer))))
	  (type (code-header-ref component
				 (+ operand vm:code-constants-offset))))
      (unless (if (functionp type)
		  (funcall type value)
		  (%typep value type))
	(with-debugger-info (component old-pc fp)
	  (error 'type-error
		 :datum value
		 :expected-type (if (functionp type)
				    (gethash type *byte-predicate-types*)
				    (type-specifier type))))))
    
    (byte-interpret component new-pc fp)))


;;;; The byte-interpreter.


;;; The various operations are encoded as follows.
;;;
;;; 0000xxxx push-local op
;;; 0001xxxx push-arg op   [push-local, but negative]
;;; 0010xxxx push-constant op
;;; 0011xxxx push-system-constant op
;;; 0100xxxx push-int op
;;; 0101xxxx push-neg-int op
;;; 0110xxxx pop-local op
;;; 0111xxxx pop-n op
;;; 1000nxxx call op
;;; 1001nxxx tail-call op
;;; 1010nxxx multiple-call op
;;; 10110xxx local-call
;;; 10111xxx local-tail-call
;;; 11000xxx local-multiple-call
;;; 11001xxx return
;;; 1101000r branch
;;; 1101001r if-true
;;; 1101010r if-false
;;; 1101011r if-eq
;;; 11011xxx Xop
;;; 11100000
;;;    to    various inline functions.
;;; 11111111
;;;
;;; This encoding is rather hard wired into BYTE-INTERPRET due to the binary
;;; dispatch tree.
;;; 

(declaim (start-block byte-interpret byte-interpret-byte
		      invoke-xep invoke-local-entry-point))

(defvar *byte-trace* nil)

;;; BYTE-INTERPRET -- Internal Interface.
;;;
;;; Main entry point to the byte interpreter.
;;; 
(defun byte-interpret (component pc fp)
  (declare (type code-component component)
	   (type pc pc)
	   (type stack-pointer fp))
  (byte-interpret-byte component pc fp (component-ref component pc)))

;;; BYTE-INTERPRET-BYTE -- Internal.
;;;
;;; This is seperated from BYTE-INTERPRET so we can continue from a breakpoint
;;; without having to replace the breakpoint with the original instruction
;;; and arrange to somehow put the breakpoint back after executing the
;;; instruction.  We just leave the breakpoint there, and calls this function
;;; with the byte the breakpoint displaced.
;;;
(defun byte-interpret-byte (component pc fp byte)
  (declare (type code-component component)
	   (type pc pc)
	   (type stack-pointer fp)
	   (type (unsigned-byte 8) byte)
	   (optimize (speed 3) (safety 0)))
  #+nil
  (locally (declare (optimize (inhibit-warnings 3)))
    (when *byte-trace*
      (let ((*byte-trace* nil))
	(format *trace-output*
		"pc=~D, fp=~D, sp=~D, byte=#b~,'0X, frame:~%    ~S~%"
		pc fp (current-stack-pointer) byte
		(subseq eval::*eval-stack* fp (current-stack-pointer))))))
  (if (not (logbitp 7 byte))
      ;; Some stack operation.  No matter what, we need the operand,
      ;; so compute it.
      (multiple-value-bind
	  (operand new-pc)
	  (let ((operand (logand byte #xf)))
	    (if (= operand #xf)
		(let ((operand (component-ref component (1+ pc))))
		  (if (= operand #xff)
		      (values (component-ref-24 component (+ pc 2))
			      (+ pc 5))
		      (values operand (+ pc 2))))
		(values operand (1+ pc))))
	(if (not (logbitp 6 byte))
	    (push-eval-stack (if (not (logbitp 5 byte))
				 (if (not (logbitp 4 byte))
				     (eval-stack-ref (+ fp operand))
				     (eval-stack-ref (- fp operand 5)))
				 (if (not (logbitp 4 byte))
				     (code-header-ref
				      component
				      (+ operand vm:code-constants-offset))
				     (svref system-constants operand))))
	    (if (not (logbitp 5 byte))
		(push-eval-stack (if (not (logbitp 4 byte))
				     operand
				     (- (1+ operand))))
		(if (not (logbitp 4 byte))
		    (setf (eval-stack-ref (+ fp operand)) (pop-eval-stack))
		    (if (zerop operand)
			(let ((operand (pop-eval-stack)))
			  (declare (type index operand))
			  (decf (current-stack-pointer) operand))
			(decf (current-stack-pointer) operand)))))
	(byte-interpret component new-pc fp))
      (if (not (logbitp 6 byte))
	  ;; Some kind of call.
	  (let ((args (let ((args (logand byte #x07)))
			(if (= args #x07)
			    (pop-eval-stack)
			    args))))
	    (if (not (logbitp 5 byte))
		(let ((named (not (not (logbitp 3 byte)))))
		  (if (not (logbitp 4 byte))
		      ;; Call for single value.
		      (do-call component pc (1+ pc) fp args named)
		      ;; Tail call.
		      (do-tail-call component pc fp args named)))
		(if (not (logbitp 4 byte))
		    ;; Call for multiple-values.
		    (do-call component pc (- (1+ pc)) fp args
			     (not (not (logbitp 3 byte))))
		    (if (not (logbitp 3 byte))
			;; Local call
			(do-local-call component pc (+ pc 4) fp args)
			;; Local tail-call
			(do-tail-local-call component pc fp args)))))
	  (if (not (logbitp 5 byte))
	      ;; local-multiple-call, Return, branch, or Xop.
	      (if (not (logbitp 4 byte))
		  ;; local-multiple-call or return.
		  (if (not (logbitp 3 byte))
		      ;; Local-multiple-call.
		      (do-local-call component pc (- (+ pc 4)) fp
				     (let ((args (logand byte #x07)))
				       (if (= args #x07)
					   (pop-eval-stack)
					   args)))
		      ;; Return.
		      (let ((num-results
			     (let ((num-results (logand byte #x7)))
			       (if (= num-results 7)
				   (pop-eval-stack)
				   num-results))))
			(do-return fp num-results)))
		  ;; Branch or Xop.
		  (if (not (logbitp 3 byte))
		      ;; Branch.
		      (if (if (not (logbitp 2 byte))
			      (if (not (logbitp 1 byte))
				  t
				  (pop-eval-stack))
			      (if (not (logbitp 1 byte))
				  (not (pop-eval-stack))
				  (multiple-value-pop-eval-stack
				   (val1 val2)
				   (eq val1 val2))))
			  ;; Branch taken.
			  (byte-interpret
			   component
			   (if (not (logbitp 0 byte))
			       (component-ref-24 component (1+ pc))
			       (+ pc 2
				  (component-ref-signed component (1+ pc))))
			   fp)
			  ;; Branch not taken.
			  (byte-interpret component
					  (if (not (logbitp 0 byte))
					      (+ pc 4)
					      (+ pc 2))
					  fp))
		      ;; Xop.
		      (multiple-value-bind
			  (sub-code new-pc)
			  (let ((operand (logand byte #x7)))
			    (if (= operand #x7)
				(values (component-ref component (+ pc 1))
					(+ pc 2))
				(values operand (1+ pc))))
			(funcall (the function (svref *byte-xops* sub-code))
				 component pc new-pc fp))))
	      ;; Random inline function.
	      (progn
		(expand-into-inlines)
		(byte-interpret component (1+ pc) fp))))))

(defun do-local-call (component pc old-pc old-fp num-args)
  (declare (type pc pc)
	   (type return-pc old-pc)
	   (type stack-pointer old-fp)
	   (type (integer 0 #.call-arguments-limit) num-args))
  (invoke-local-entry-point component (component-ref-24 component (1+ pc))
			    component old-pc
			    (- (current-stack-pointer) num-args)
			    old-fp))

(defun do-tail-local-call (component pc fp num-args)
  (declare (type code-component component) (type pc pc)
	   (type stack-pointer fp)
	   (type index num-args))
  (let ((old-fp (eval-stack-ref (- fp 1)))
	(old-sp (eval-stack-ref (- fp 2)))
	(old-pc (eval-stack-ref (- fp 3)))
	(old-component (eval-stack-ref (- fp 4)))
	(start-of-args (- (current-stack-pointer) num-args)))
    (stack-copy old-sp start-of-args num-args)
    (setf (current-stack-pointer) (+ old-sp num-args))
    (invoke-local-entry-point component (component-ref-24 component (1+ pc))
			      old-component old-pc old-sp old-fp)))

(defun invoke-local-entry-point (component target old-component old-pc old-sp
					   old-fp &optional closure-vars)
  (declare (type pc target)
	   (type return-pc old-pc)
	   (type stack-pointer old-sp old-fp)
	   (type (or null simple-vector) closure-vars))
  (when closure-vars
    (iterate more ((index (1- (length closure-vars))))
      (unless (minusp index)
	(push-eval-stack (svref closure-vars index))
	(more (1- index)))))
  (push-eval-stack old-component)
  (push-eval-stack old-pc)
  (push-eval-stack old-sp)
  (push-eval-stack old-fp)
  (multiple-value-bind
      (stack-frame-size entry-pc)
      (let ((byte (component-ref component target)))
	(if (= byte 255)
	    (values (component-ref-24 component (1+ target)) (+ target 4))
	    (values (* byte 2) (1+ target))))
    (declare (type pc entry-pc))
    (let ((fp (current-stack-pointer)))
      (allocate-eval-stack stack-frame-size)
      (byte-interpret component entry-pc fp))))


;;; BYTE-APPLY  --  Internal
;;;
;;;    Call a function with some arguments popped off of the interpreter stack,
;;; and restore the SP to the specifier value.
;;;
(defun byte-apply (function num-args restore-sp)
  (declare (function function) (type index num-args))
  (let ((start (- (current-stack-pointer) num-args)))
    (declare (type stack-pointer start))
    (macrolet ((frob ()
		 `(case num-args
		    ,@(loop for n below 8
			collect `(,n (call-1 ,n)))
		    (t
		     (let ((args ())
			   (end (+ start num-args)))
		       (declare (type stack-pointer end))
		       (do ((i (1- end) (1- i)))
			   ((< i start))
			 (declare (fixnum i))
			 (push (eval-stack-ref i) args))
		       (setf (current-stack-pointer) restore-sp)
		       (apply function args)))))
	       (call-1 (n)
		 (collect ((binds)
			   (args))
		   (dotimes (i n)
		     (let ((dum (gensym)))
		       (binds `(,dum (eval-stack-ref (+ start ,i))))
		       (args dum)))
		   `(let ,(binds)
		      (setf (current-stack-pointer) restore-sp)
		      (funcall function ,@(args))))))
      (frob))))


(defun do-call (old-component call-pc ret-pc old-fp num-args named)
  (declare (type code-component old-component)
	   (type pc call-pc)
	   (type return-pc ret-pc)
	   (type stack-pointer old-fp)
	   (type (integer 0 #.call-arguments-limit) num-args)
	   (type (member t nil) named))
  (let* ((old-sp (- (current-stack-pointer) num-args 1))
	 (fun-or-fdefn (eval-stack-ref old-sp))
	 (function (if named
		       (or (fdefn-function fun-or-fdefn)
			   (with-debugger-info (old-component call-pc old-fp)
			     (error 'undefined-function
				    :name (fdefn-name fun-or-fdefn))))
		       fun-or-fdefn)))
    (declare (type stack-pointer old-sp)
	     (type (or function fdefn) fun-or-fdefn)
	     (type function function))
    (typecase function
      (byte-function
       (invoke-xep old-component ret-pc old-sp old-fp num-args function))
      (byte-closure
       (invoke-xep old-component ret-pc old-sp old-fp num-args
		   (byte-closure-function function)
		   (byte-closure-data function)))
      (t
       (cond ((minusp ret-pc)
	      (let* ((ret-pc (- ret-pc))
		     (results
		      (multiple-value-list
		       (with-debugger-info
			   (old-component ret-pc old-fp)
			 (byte-apply function num-args old-sp)))))
		(dolist (result results)
		  (push-eval-stack result))
		(push-eval-stack (length results))
		(byte-interpret old-component ret-pc old-fp)))
	     (t
	      (push-eval-stack
	       (with-debugger-info
		   (old-component ret-pc old-fp)
		 (byte-apply function num-args old-sp)))
	      (byte-interpret old-component ret-pc old-fp)))))))


(defun do-tail-call (component pc fp num-args named)
  (declare (type code-component component)
	   (type pc pc)
	   (type stack-pointer fp)
	   (type (integer 0 #.call-arguments-limit) num-args)
	   (type (member t nil) named))
  (let* ((start-of-args (- (current-stack-pointer) num-args))
	 (fun-or-fdefn (eval-stack-ref (1- start-of-args)))
	 (function (if named
		       (or (fdefn-function fun-or-fdefn)
			   (with-debugger-info (component pc fp)
			     (error 'undefined-function
				    :name (fdefn-name fun-or-fdefn))))
		       fun-or-fdefn))
	 (old-fp (eval-stack-ref (- fp 1)))
	 (old-sp (eval-stack-ref (- fp 2)))
	 (old-pc (eval-stack-ref (- fp 3)))
	 (old-component (eval-stack-ref (- fp 4))))
    (declare (type stack-pointer old-fp old-sp start-of-args)
	     (type return-pc old-pc)
	     (type (or fdefn function) fun-or-fdefn)
	     (type function function))
    (typecase function
      (byte-function
       (stack-copy old-sp start-of-args num-args)
       (setf (current-stack-pointer) (+ old-sp num-args))
       (invoke-xep old-component old-pc old-sp old-fp num-args function))
      (byte-closure
       (stack-copy old-sp start-of-args num-args)
       (setf (current-stack-pointer) (+ old-sp num-args))
       (invoke-xep old-component old-pc old-sp old-fp num-args
		   (byte-closure-function function)
		   (byte-closure-data function)))
      (t
       ;; We are tail-calling native code.
       (cond ((null old-component)
	      ;; We were called by native code.
	      (byte-apply function num-args old-sp))
	     ((minusp old-pc)
	      ;; We were called for multiple values.  So return multiple
	      ;; values.
	      (let* ((old-pc (- old-pc))
		     (results
		      (multiple-value-list
		       (with-debugger-info
			(old-component old-pc old-fp)
			(byte-apply function num-args old-sp)))))
		(dolist (result results)
		  (push-eval-stack result))
		(push-eval-stack (length results))
		(byte-interpret old-component old-pc old-fp)))
	     (t
	      ;; We were called for one value.  So return one value.
	      (push-eval-stack
	       (with-debugger-info
		   (old-component old-pc old-fp)
		 (byte-apply function num-args old-sp)))
	      (byte-interpret old-component old-pc old-fp)))))))

(defvar *byte-trace-calls* nil)

(defun invoke-xep (old-component ret-pc old-sp old-fp num-args xep
				 &optional closure-vars)
  (declare (type (or null code-component) old-component)
	   (type index num-args)
	   (type return-pc ret-pc)
	   (type stack-pointer old-sp old-fp)
	   (type byte-function xep)
	   (type (or null simple-vector) closure-vars))
  (when *byte-trace-calls*
    (let ((*byte-trace-calls* nil)
	  (*byte-trace* nil)
	  (*print-level* debug:*debug-print-level*)
	  (*print-length* debug:*debug-print-length*)
	  (sp (current-stack-pointer)))
      (format *trace-output*
	      "~&Invoke-XEP: ocode= ~S[~D]~%  ~
	       osp= ~D, ofp= ~D, nargs= ~D, SP= ~D:~%  ~
	       Fun= ~S ~@[~S~]~%  Args= ~S~%"
	      old-component ret-pc old-sp old-fp num-args sp
	      xep closure-vars (subseq *eval-stack* (- sp num-args) sp))
      (force-output *trace-output*)))

  (let ((entry-point
	 (cond
	  ((typep xep 'simple-byte-function)
	   (unless (eql (simple-byte-function-num-args xep) num-args)
	     (with-debugger-info (old-component ret-pc old-fp)
	       (simple-program-error "Wrong number of arguments.")))
	   (simple-byte-function-entry-point xep))
	  (t
	   (let ((min (hairy-byte-function-min-args xep))
		 (max (hairy-byte-function-max-args xep)))
	     (cond
	      ((< num-args min)
	       (with-debugger-info (old-component ret-pc old-fp)
		 (simple-program-error "Not enough arguments.")))
	      ((<= num-args max)
	       (nth (- num-args min) (hairy-byte-function-entry-points xep)))
	      ((null (hairy-byte-function-more-args-entry-point xep))
	       (with-debugger-info (old-component ret-pc old-fp)
		 (simple-program-error "Too many arguments.")))
	      (t
	       (let* ((more-args-supplied (- num-args max))
		      (sp (current-stack-pointer))
		      (more-args-start (- sp more-args-supplied))
		      (restp (hairy-byte-function-rest-arg-p xep))
		      (rest (and restp
				 (do ((index (1- sp) (1- index))
				      (result nil
					      (cons (eval-stack-ref index)
						    result)))
				     ((< index more-args-start) result)
				   (declare (fixnum index))))))
		 (declare (type index more-args-supplied)
			  (type stack-pointer more-args-start))
		 (cond
		  ((not (hairy-byte-function-keywords-p xep))
		   (assert restp)
		   (setf (current-stack-pointer) (1+ more-args-start))
		   (setf (eval-stack-ref more-args-start) rest)
		   (when (eq :more restp)
		     (incf (current-stack-pointer))
		     (setf (eval-stack-ref (1+ more-args-start))
			   (length rest))))
		  (t
		   (unless (evenp more-args-supplied)
		     (with-debugger-info (old-component ret-pc old-fp)
		       (simple-program-error "Odd number of keyword arguments.")))
		   ;;
		   ;; If there are keyword args, then we need to leave the
		   ;; defaulted and supplied-p values where the more args
		   ;; currently are.  There might be more or fewer.  And also,
		   ;; we need to flatten the parsed args with the defaults
		   ;; before we scan the keywords.  So we copy all the more
		   ;; args to a temporary area at the end of the stack.
		   (let* ((num-more-args
			   (hairy-byte-function-num-more-args xep))
			  (new-sp (+ more-args-start num-more-args))
			  (temp (max sp new-sp))
			  (temp-sp (+ temp more-args-supplied))
			  (keywords (hairy-byte-function-keywords xep)))
		     (declare (type index temp)
			      (type stack-pointer new-sp temp-sp))
		     (allocate-eval-stack (- temp-sp sp))
		     (stack-copy temp more-args-start more-args-supplied)
		     (when restp
		       (setf (eval-stack-ref more-args-start) rest)
		       (incf more-args-start)
		       (when (eq :more restp)
			 (setf (eval-stack-ref (1+ more-args-start))
			       (length rest))
			 (incf more-args-start)))
		     (let ((index more-args-start))
		       (dolist (keyword keywords)
			 (setf (eval-stack-ref index) (cadr keyword))
			 (incf index)
			 (when (caddr keyword)
			   (setf (eval-stack-ref index) nil)
			   (incf index))))
		     (let ((index temp-sp)
			   (allow (eq (hairy-byte-function-keywords-p xep)
				      :allow-others))
			   (bogus-key nil)
			   (bogus-key-p nil))
		       (declare (type fixnum index))
		       (loop
			 (decf index 2)
			 (when (< index temp)
			   (return))
			 (let ((key (eval-stack-ref index))
			       (value (eval-stack-ref (1+ index))))
			   (if (eq key :allow-other-keys)
			       (setf allow value)
			       (let ((target more-args-start))
				 (declare (type stack-pointer target))
				 (dolist (keyword keywords
						  (setf bogus-key key
							bogus-key-p t))
				   (cond ((eq (car keyword) key)
					  (setf (eval-stack-ref target) value)
					  (when (caddr keyword)
					    (setf (eval-stack-ref (1+ target))
						  t))
					  (return))
					 ((caddr keyword)
					  (incf target 2))
					 (t
					  (incf target))))))))
		       (when (and bogus-key-p (not allow))
			 (with-debugger-info (old-component ret-pc old-fp)
			   (simple-program-error "Unknown keyword: ~S"
						 bogus-key))))
		     (setf (current-stack-pointer) new-sp)))))
	       (hairy-byte-function-more-args-entry-point xep))))))))
    (declare (type pc entry-point))
    (invoke-local-entry-point (byte-function-component xep) entry-point
			      old-component ret-pc old-sp old-fp
			      closure-vars)))

(defun do-return (fp num-results)
  (declare (type stack-pointer fp) (type index num-results))
  (let ((old-component (eval-stack-ref (- fp 4))))
    (typecase old-component
      (code-component
       ;; Returning to more byte-interpreted code.
       (do-local-return old-component fp num-results))
      (null
       ;; Returning to native code.
       (let ((old-sp (eval-stack-ref (- fp 2))))
	 (case num-results
	   (0
	    (setf (current-stack-pointer) old-sp)
	    (values))
	   (1
	    (let ((result (pop-eval-stack)))
	      (setf (current-stack-pointer) old-sp)
	      result))
	   (t
	    (let ((results nil))
	      (dotimes (i num-results)
		(push (pop-eval-stack) results))
	      (setf (current-stack-pointer) old-sp)
	      (values-list results))))))
      (t
       ;; ### Function end breakpoint?
       (error "function-end breakpoints not supported.")))))

(defun do-local-return (old-component fp num-results)
  (declare (type stack-pointer fp) (type index num-results))
  (let ((old-fp (eval-stack-ref (- fp 1)))
	(old-sp (eval-stack-ref (- fp 2)))
	(old-pc (eval-stack-ref (- fp 3))))
    (declare (type (signed-byte 25) old-pc))
    (if (plusp old-pc)
	;; Wants single value.
	(let ((result (if (zerop num-results)
			  nil
			  (eval-stack-ref (- (current-stack-pointer)
					     num-results)))))
	  (setf (current-stack-pointer) old-sp)
	  (push-eval-stack result)
	  (byte-interpret old-component old-pc old-fp))
	;; Wants multiple values.
	(progn
	  (stack-copy old-sp (- (current-stack-pointer) num-results)
		      num-results)
	  (setf (current-stack-pointer) (+ old-sp num-results))
	  (push-eval-stack num-results)
	  (byte-interpret old-component (- old-pc) old-fp)))))

(declaim (end-block byte-interpret byte-interpret-byte invoke-xep))

