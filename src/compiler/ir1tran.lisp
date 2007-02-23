;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ir1tran.lisp,v 1.173 2006/05/23 20:35:01 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains code which does the translation from Lisp code to the
;;; first intermediate representation (IR1).
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

(export '(*compile-time-define-macros* *converting-for-interpreter*
	  *suppress-values-declaration*))

(in-package "EXT")
(export '(truly-the maybe-inline *derive-function-types*))

(in-package "LISP")
(export '(ignorable dynamic-extent symbol-macrolet))

(in-package "KERNEL")
(export '(lambda-with-environment instance-lambda))

(in-package "C")


(declaim (special *compiler-error-bailout*))


;;; The lexical environment we are currently converting in.  See the LEXENV
;;; structure.
;;;
(defvar *lexical-environment*)
(declaim (type lexenv *lexical-environment*))

;;; That variable is used to control the context-sensitive declarations
;;; mechanism (see WITH-COMPILATION-UNIT).  Each entry is a function which is
;;; called with the function name and parent form name.  If it returns non-nil,
;;; then that is a list of DECLARE forms which should be inserted at the head
;;; of the body.
;;;
(defvar *context-declarations* ())
(declaim (list *context-declarations*))

;;; *free-variables* translates from the names of variables referenced globally
;;; to the Leaf structures for them.  *free-functions* is like
;;; *free-variables*, only it deals with function names.
;;;
;;; We must preserve the property that a proclamation for a global thing
;;; only affects the code after it.  This takes some work, since a proclamation
;;; may appear in the middle of a block being compiled.  If there are
;;; references before the proclaim, then we copy the current entry before
;;; modifying it.  Code converted before the proclaim sees the old Leaf, while
;;; code after it sees the new Leaf.
;;;
(defvar *free-variables*)
(defvar *free-functions*)
(declaim (hash-table *free-variables* *free-functions*))

;;; We use the same Constant structure to represent all equal anonymous
;;; constants.  This hashtable translates from constants to the Leafs that
;;; represent them.
;;;
(defvar *constants*)
(declaim (hash-table *constants*))

(defvar *coalesce-constants*)
(declaim (type (member t nil) *coalesce-constants*))

;;; *SOURCE-PATHS* is a hashtable from source code forms to the path taken
;;; through the source to reach the form.  This provides a way to keep track of
;;; the location of original source forms, even when macroexpansions and other
;;; arbitary permutations of the code happen.  This table is initialized by
;;; calling FIND-SOURCE-PATHS on the original source.
;;;
(declaim (hash-table *source-paths*))
(defvar *source-paths*)

;;; *CURRENT-COMPONENT* is the Component structure which we link blocks into as
;;; we generate them.  This just serves to glue the emitted blocks together
;;; until local call analysis and flow graph canonicalization figure out what
;;; is really going on.  We need to keep track of all the blocks generated so
;;; that we can delete them if they turn out to be unreachable.
;;;
(declaim (type (or component null) *current-component*))
(defvar *current-component*)

;;; *CURRENT-PATH* is the source path of the form we are currently translating.
;;; See NODE-SOURCE-PATH in the NODE structure.
;;;
(declaim (list *current-path*))
(defvar *current-path* nil)

;;; *CONVERTING-FOR-INTERPRETER* is true when we are creating IR1 to be
;;; interpreted rather than compiled.  This inhibits source transformations and
;;; stuff.
;;;
(defvar *converting-for-interpreter* nil)

;;; *COMPILE-TIME-DEFINE-MACROS* is true when we want DEFMACRO definitions to
;;; be installed in the compilation environment as interpreted functions.  We
;;; set this to false when compiling some parts of the system.
;;;
(defvar *compile-time-define-macros* t)

;;; Stack (alist) of names of currently compiled functions and counters
;;; how often a name has been used.
;;;
(declaim (list *current-function-names*))
(defvar *current-function-names* ())

(defvar *derive-function-types* t
  "If true, argument and result type information derived from compilation of
  DEFUNs is used when compiling calls to that function.  If false, only
  information from FTYPE proclamations will be used.")

;;;
;;; Returns non-nil if X and Y name the same function. This is more
;;; involved than a simple call to EQ due to support for generalized
;;; function names. In the IR1 representation, the local function INNER
;;; in the following
;;;
;;;   (defun outer (a)
;;;      (flet ((inner (x) (random x)))
;;;         (declaim (inline inner))
;;;         (inner a)))
;;;
;;; will have a lambda-name of (FLET INNER OUTER). Some parts of the
;;; compiler may search for it under the name INNER.
;;;
(defun function-name-eqv-p (x y)
  (or (equal x y)
      (and (consp y)
           (member (car y) '(flet labels))
           (equal x (cadr y)))))


;;; *ALLOW-DEBUG-CATCH-TAG* controls whether we should allow the
;;; insertion a (CATCH ...) around code to allow the debugger
;;; return-from-frame (RETURN) command to work.
(defvar *allow-debug-catch-tag* t)

;;; This is for debugging the return-from-frame functionality. These are
;;; supposed to go away in the future. --jwr
(defvar *print-debug-tag-conversions* nil)
(defvar *print-debug-tag-converted-bodies* nil)


;;;; Dynamic-Extent

(defvar *trust-dynamic-extent-declarations* nil
  "If NIL, never trust dynamic-extent declarations.

   If T, always trust dynamic-extent declarations.

   Otherwise, the value of this variable must be a function of four
   arguments SAFETY, SPACE, SPEED, and DEBUG.  If the function returns
   true when called, dynamic-extent declarations are trusted,
   otherwise they are not trusted.")

(defvar *dynamic-extent-trace* nil)

(defun trust-dynamic-extent-declaration-p
    (&optional (lexenv *lexical-environment*))
  (declare (type lexenv lexenv))
  (let ((trust *trust-dynamic-extent-declarations*))
    (if (functionp trust)
	(let ((cookie (lexenv-cookie lexenv)))
	  (funcall trust (cookie-safety cookie) (cookie-space cookie)
		   (cookie-speed cookie) (cookie-debug cookie)))
	trust)))


(defun process-dynamic-extent-declaration (spec vars fvars lexenv)
  (declare (list spec vars fvars) (type lexenv lexenv))
  (if (trust-dynamic-extent-declaration-p lexenv)
      (collect ((dynamic-extent))
	(dolist (name (cdr spec))
	  (cond ((symbolp name)
		 (let* ((bound-var (find-in-bindings vars name))
			(var (or bound-var
				 (lexenv-find name variables)
				 (find-free-variable name))))
		   (if (leaf-p var)
		       (if bound-var
			   (setf (leaf-dynamic-extent var) t)
			   (dynamic-extent var)))))
		((and (consp name)
		      (eq (car name) 'function)
		      (null (cddr name))
		      (valid-function-name-p (cadr name)))
		 (let* ((fn-name (cadr name))
			(fn (find fn-name fvars
				  :key #'leaf-name
				  :test #'function-name-eqv-p)))
		   (if fn
		       (setf (leaf-dynamic-extent fn) t)
		       (dynamic-extent (find-lexically-apparent-function
					fn-name
					"in a dynamic-extent declaration")))))
		(t
		 (compiler-warning
		  "~@<Invalid name ~s in a dynamic-extent declaration.~@:>"
		  name))))
	(if (dynamic-extent)
	    (make-lexenv :default lexenv :dynamic-extent (dynamic-extent))
	    lexenv))
      lexenv))
  
;;;
;;; Value is true if some dynamic-extent allocation can be done in the
;;; initialization of variables Vars with values Vals.
;;;
(defun dynamic-extent-allocation-p (vars vals)
  (loop for var in vars and val in vals
	thereis (and (leaf-dynamic-extent var)
		     (consp val)
		     (memq (car val) '(list list* cons)))))

;;;
;;; Return a list of indices for arguments in Args which might end up
;;; as dynamic-extent closures.  We can't tell for sure here because
;;; environment analysis runs after IR1 conversion, so we don't know
;;; yet what are closures and what not.
;;;
(defun dynamic-extent-closure-args (args)
  (declare (list args))
  (flet ((find-local-function (name)
	   (let ((var (lexenv-find-function name)))
	     (when (leaf-p var)
	       var))))
    (let ((dynamic-extent (lexenv-dynamic-extent *lexical-environment*)))
      (collect ((indices))
	(do* ((i 0 (1+ i))
	      (tail args (cdr tail))
	      (arg (car tail) (car tail)))
	     ((null tail))
	  (when (and (consp arg)
		     (eq (car arg) 'function)
		     (valid-function-name-p (cadr arg))
		     (let ((fn (find-local-function (cadr arg))))
		       (and fn
			    (or (leaf-dynamic-extent fn)
				(and (functional-p fn)
				     (or (memq fn dynamic-extent)
					 (memq (functional-entry-function fn)
					       dynamic-extent)))))))
	    (indices i)))
	(indices)))))

;;;
;;; Evaluate Body wrapped in a dynamic-extent cleanup.
;;; FIXME: Maybe don't %dynamic-extent-start if kind = :rest.
;;;
(defun gen-%dynamic-extent (kind)
  `(%dynamic-extent ,kind (%dynamic-extent-start)))

(defmacro with-dynamic-extent ((start cont nnext-cont kind) &body body)
  `(progn
     (continuation-starts-block ,cont)
     (let ((.cleanup. (make-cleanup :kind :dynamic-extent))
	   (.next-cont. (make-continuation))
	   (,nnext-cont (make-continuation)))
       (ir1-convert ,start .next-cont. (gen-%dynamic-extent ,kind))
       (setf (cleanup-mess-up .cleanup.) (continuation-use .next-cont.))
       (let ((*lexical-environment* (make-lexenv :cleanup .cleanup.)))
	 (ir1-convert .next-cont. ,nnext-cont '(%cleanup-point))
	 (locally ,@body)))))


;;;; Namespace management utilities:

(declaim (start-block find-free-function find-lexically-apparent-function))

;;; Find-Free-Really-Function  --  Internal
;;;
;;;    Return a Global-Var structure usable for referencing the global function
;;; Name.
;;;
(defun find-free-really-function (name &optional context)
  (unless (info function kind name)
    (setf (info function kind name) :function)
    (setf (info function where-from name) :assumed))
  
  (let ((where (info function where-from name)))
    (when (eq where :assumed)
      (note-undefined-reference name :function context))
    (make-global-var :kind :global-function  :name name
		     :type (if (or *derive-function-types*
				   (eq where :declared))
			       (info function type name)
			       (specifier-type 'function))
		     :where-from where)))


;;; Find-Structure-Slot-Accessor  --  Internal
;;;
;;;    Return a Slot-Accessor structure usable for referencing the slot
;;; accessor Name.  Class is the structure class.
;;;
(defun find-structure-slot-accessor (class name)
  (declare (type kernel::class class))
  (let* ((info (layout-info
		(or (info type compiler-layout (%class-name class))
		    (%class-layout class))))
	 (accessor (if (listp name) (cadr name) name))
	 (slot (find accessor (kernel:dd-slots info)
		     :key #'kernel:dsd-accessor))
	 (type (kernel:dd-name info))
	 (slot-type (kernel:dsd-type slot)))
    (assert slot () "Can't find slot ~S." type)
    (make-slot-accessor
     :name name
     :type (specifier-type
	    (if (listp name)
		`(function (,slot-type ,type) ,slot-type)
		`(function (,type) ,slot-type)))
     :for class
     :slot slot)))


;;; Find-Free-Function  --  Internal
;;;
;;;    If NAME is already entered in *free-functions*, then return the value.
;;; Otherwise, make a new Global-Var using information from the global
;;; environment and enter it in *free-functions*.  If NAME names a macro or
;;; special form, then we error out using the supplied CONTEXT which indicates
;;; what we were trying to do that demanded a function.
;;;
(defun find-free-function (name context)
  (declare (string context))
  (declare (values global-var))
  (or (gethash name *free-functions*)
      (ecase (info function kind name)
	(:macro
	 (compiler-error "Found macro name ~S ~A." name context))
	(:special-form
	 (compiler-error "Found special-form name ~S ~A." name context))
	((:function nil)
	 (check-function-name name)
	 (note-if-setf-function-and-macro name)
	 (let ((expansion (info function inline-expansion name))
	       (inlinep (info function inlinep name)))
	   (setf (gethash name *free-functions*)
		 (if (or expansion inlinep)
		     (make-defined-function
		      :name name
		      :inline-expansion expansion
		      :inlinep inlinep
		      :where-from (info function where-from name)
		      :type (info function type name))
		     (let ((info (info function accessor-for name)))
		       (etypecase info
			 (null
			  (find-free-really-function name context))
			 (kernel::structure-class
			  (find-structure-slot-accessor info name))
			 (class
			  (if (typep (layout-info (info type compiler-layout
							(%class-name info)))
				     'defstruct-description)
			      (find-structure-slot-accessor info name)
			      (find-free-really-function name context))))))))))))


;;; Find-Lexically-Apparent-Function  --  Internal
;;;
;;;    Return the Leaf structure for the lexically apparent function definition
;;; of NAME.
;;;
(defun find-lexically-apparent-function (name context)
  (declare (string context) (values leaf))
  (let ((var (lexenv-find-function name)))
    (cond (var
	   (unless (leaf-p var)
	     (assert (and (consp var) (eq (car var) 'macro)))
	     (compiler-error "Found macro name ~S ~A." name context))
	   var)
	  (t
	   (find-free-function name context)))))

(declaim (end-block))

;;; Find-Free-Variable  --  Internal
;;;
;;;    Return the Leaf node for a global variable reference to Name.  If Name
;;; is already entered in *free-variables*, then we just return the
;;; corresponding value.  Otherwise, we make a new leaf using information from
;;; the global environment and enter it in *free-variables*.  If the variable
;;; is unknown, then we emit a warning.
;;;
(defun find-free-variable (name)
  (declare (values (or leaf cons heap-alien-info)))
  (unless (symbolp name)
    (compiler-error "Variable name is not a symbol: ~S." name))
  (or (gethash name *free-variables*)
      (let ((kind (info variable kind name))
	    (type (info variable type name))
	    (where-from (info variable where-from name)))
	(when (and (eq where-from :assumed) (eq kind :global))
	  (note-undefined-reference name :variable))
	;;
	;; Paper over the fact that unknown types aren't removed
	;; from the info database when a types gets defined.
	(when (unknown-type-p type)
	  (setq type (specifier-type (type-specifier type)))
	  (setf (info variable type name) type))

	(setf (gethash name *free-variables*)
	      (case kind
		(:alien
		 (info variable alien-info name))
		(:macro
		 (let ((expansion (info variable macro-expansion name)))
		   `(MACRO . (the ,(type-specifier type) ,expansion))))
		(:constant
		 (multiple-value-bind (val valp)
		     (info variable constant-value name)
		   (if valp
		       (make-constant :value val :name name
				      :type (ctype-of val)
				      :where-from where-from)
		       (make-global-var :kind kind :name name :type type
					:where-from where-from))))
		(t
		 (make-global-var :kind kind :name name :type type
				  :where-from where-from)))))))


;;; MAYBE-EMIT-MAKE-LOAD-FORMS  --  internal
;;;
;;; Grovel over CONSTANT checking for any sub-parts that need to be processed
;;; with MAKE-LOAD-FORM.  We have to be careful, because CONSTANT might be
;;; circular.  We also check that the constant (and any subparts) are dumpable
;;; at all.
;;; 
(defconstant list-to-hash-table-threshold 32)
;;;
(defun maybe-emit-make-load-forms (constant)
  (let ((things-processed nil)
	(count 0))
    (declare (type (or list hash-table) things-processed)
	     (type (integer 0 #.(1+ list-to-hash-table-threshold)) count)
	     (inline member))
    (labels ((grovel (value)
	       (unless (typep value
			      '(or unboxed-array symbol number character))
		 (etypecase things-processed
		   (list
		    (when (member value things-processed :test #'eq)
		      (return-from grovel nil))
		    (push value things-processed)
		    (incf count)
		    (when (> count list-to-hash-table-threshold)
		      (let ((things things-processed))
			(setf things-processed
			      (make-hash-table :test #'eq))
			(dolist (thing things)
			  (setf (gethash thing things-processed) t)))))
		   (hash-table
		    (when (gethash value things-processed)
		      (return-from grovel nil))
		    (setf (gethash value things-processed) t)))
		 (typecase value
		   (cons
		    (grovel (car value))
		    (grovel (cdr value)))
		   (simple-vector
		    (dotimes (i (length value))
		      (grovel (svref value i))))
		   ((vector t)
		    (dotimes (i (length value))
		      (grovel (aref value i))))
		   ((simple-array t)
		    ;; Even though the (array t) branch does the exact same
		    ;; thing as this branch we do this seperate so that
		    ;; the compiler can use faster versions of array-total-size
		    ;; and row-major-aref.
		    (dotimes (i (array-total-size value))
		      (grovel (row-major-aref value i))))
		   ((array t)
		    (dotimes (i (array-total-size value))
		      (grovel (row-major-aref value i))))
		   (instance
		    (when (emit-make-load-form value)
		      (dotimes (i (%instance-length value))
			(grovel (%instance-ref value i)))))
		   (t
		    (compiler-error
		     "Cannot dump objects of type ~S into fasl files."
		     (type-of value)))))))
      (grovel constant)))
  (undefined-value))


;;;; Some flow-graph hacking utilities:

(eval-when (:compile-toplevel :execute)
;;; IR1-Error-Bailout  --  Internal
;;;
;;;    Bind *compiler-error-bailout* to a function that throws out of the body
;;; and converts a proxy form instead.
;;;
(defmacro ir1-error-bailout
	  ((start cont form
	    &optional
	    (proxy ``(error 'simple-program-error
		       :format-control "Execution of a form compiled with errors:~% ~S"
		       :format-arguments (list ',,form))))
	   &body body)
  (let ((skip (gensym)))
    `(block ,skip
       (catch 'ir1-error-abort 
	 (let ((*compiler-error-bailout*
		#'(lambda () (throw 'ir1-error-abort nil))))
	   ,@body
	   (return-from ,skip nil)))
       (ir1-convert ,start ,cont ,proxy))))

); eval-when (:compile-toplevel :execute)


;;; Prev-Link  --  Internal
;;;
;;;    This function sets up the back link between the node and the
;;; continuation which continues at it. 
;;;
(declaim (inline prev-link))
(defun prev-link (node cont)
  (declare (type node node) (type continuation cont))
  (assert (not (continuation-next cont)))
  (setf (continuation-next cont) node)
  (setf (node-prev node) cont))


;;; Use-Continuation  --  Internal
;;;
;;;    This function is used to set the continuation for a node, and thus
;;; determine what recieves the value and what is evaluated next.  If the
;;; continuation has no block, then we make it be in the block that the node is
;;; in.  If the continuation heads its block, we end our block and link it to
;;; that block.  If the continuation is not currently used, then we set the
;;; derived-type for the continuation to that of the node, so that a little
;;; type propagation gets done.
;;;
;;;    We also deal with a bit of THE's semantics here: we weaken the assertion
;;; on Cont to be no stronger than the assertion on Cont in our scope.  See the
;;; THE IR1-CONVERT method.
;;;
(declaim (inline use-continuation))
(defun use-continuation (node cont)
  (declare (type node node) (type continuation cont))
  (let ((node-block (continuation-block (node-prev node))))
    (case (continuation-kind cont)
      (:unused
       (setf (continuation-block cont) node-block)
       (setf (continuation-kind cont) :inside-block)
       (setf (continuation-use cont) node)
       (setf (node-cont node) cont))
      (t
       (%use-continuation node cont)))))
;;;
(defun %use-continuation (node cont)
  (declare (type node node) (type continuation cont) (inline member))
  (let ((block (continuation-block cont))
	(node-block (continuation-block (node-prev node))))
    (assert (eq (continuation-kind cont) :block-start))
    (assert (not (block-last node-block)) () "~S has already ended."
	    node-block)
    (setf (block-last node-block) node)
    (assert (null (block-succ node-block)) () "~S already has successors."
	    node-block)
    (setf (block-succ node-block) (list block))
    (assert (not (member node-block (block-pred block) :test #'eq)) ()
	    "~S is already a predecessor of ~S." node-block block)
    (push node-block (block-pred block))
    (add-continuation-use node cont)
    (unless (eq (continuation-asserted-type cont) *wild-type*)
      (let ((new (values-type-union (continuation-asserted-type cont)
				    (or (lexenv-find cont type-restrictions)
					*wild-type*))))
	(when (type/= new (continuation-asserted-type cont))
	  (setf (continuation-asserted-type cont) new)
	  (reoptimize-continuation cont))))))


;;;; Exported functions:

;;; IR1-Top-Level  --  Interface
;;;
;;;    This function takes a form and the top-level form number for that form,
;;; and returns a lambda representing the translation of that form in the
;;; current global environment.  The lambda is top-level lambda that can be
;;; called to cause evaluation of the forms.  This lambda is in the initial
;;; component.  If For-Value is T, then the value of the form is returned from
;;; the function, otherwise NIL is returned.
;;;
;;;    This function may have arbitrary effects on the global environment due
;;; to processing of Proclaims and Eval-Whens.  All syntax error checking is
;;; done, with erroneous forms being replaced by a proxy which signals an error
;;; if it is evaluated.  Warnings about possibly inconsistent or illegal
;;; changes to the global environment will also be given.
;;;
;;;    We make the initial component and convert the form in a progn (and an
;;; optional NIL tacked on the end.)  We then return the lambda.  We bind all
;;; of our state variables here, rather than relying on the global value (if
;;; any) so that IR1 conversion will be reentrant.  This is necessary for
;;; eval-when processing, etc.
;;;
;;;    The hashtables used to hold global namespace info must be reallocated
;;; elsewhere.  Note also that *lexical-environment* is not bound, so that
;;; local macro definitions can be introduced by enclosing code.
;;;
(defun ir1-top-level (form path for-value)
  (declare (list path))
  (let* ((*current-path* path)
	 (component (make-empty-component))
	 (*current-component* component))
    (setf (component-name component) "initial component")
    (setf (component-kind component) :initial)
    (let* ((forms (if for-value `(,form) `(,form nil)))
	   (res (ir1-convert-lambda-body forms ())))
      (setf (leaf-name res) "Top-Level Form")
      (setf (functional-entry-function res) res)
      (setf (functional-arg-documentation res) ())
      (setf (functional-kind res) :top-level)
      res)))


;;; *CURRENT-FORM-NUMBER* is used in FIND-SOURCE-PATHS to compute the form
;;; number to associate with a source path.  This should be bound to 0 around
;;; the processing of each truly top-level form.
;;;
(declaim (type index *current-form-number*))
(defvar *current-form-number*)

;;; Find-Source-Paths  --  Interface
;;;
;;;    This function is called on freshly read forms to record the initial
;;; location of each form (and subform.)  Form is the form to find the paths
;;; in, and TLF-Num is the top-level form number of the truly top-level form.
;;;
;;;    This gets a bit interesting when the source code is circular.  This can
;;; (reasonably?) happen in the case of circular list constants. 
;;;
(defun find-source-paths (form tlf-num)
  (declare (type index tlf-num))
  (let ((*current-form-number* 0))
    (sub-find-source-paths form (list tlf-num)))
  (undefined-value))
;;;
(defun sub-find-source-paths (form path)
  (unless (gethash form *source-paths*)
    (setf (gethash form *source-paths*)
	  (list* 'original-source-start *current-form-number* path))
    (incf *current-form-number*)
    (let ((pos 0)
	  (subform form)
	  (trail form))
      (declare (fixnum pos))
      (macrolet ((frob ()
		   '(progn
		      (when (atom subform) (return))
		      (let ((fm (car subform)))
			(when (consp fm)
			  (sub-find-source-paths fm (cons pos path)))
			(incf pos))
		      (setq subform (cdr subform))
		      (when (eq subform trail) (return)))))
	(loop
	  (frob)
	  (frob)
	  (setq trail (cdr trail)))))))




;;;; IR1-CONVERT, macroexpansion and special-form dispatching.

(declaim (start-block ir1-convert ir1-convert-progn-body
		      ir1-convert-combination-args reference-leaf
		      reference-constant))

;;; IR1-Convert  --  Interface
;;;
;;;    Translate Form into IR1.  The code is inserted as the Next of the
;;; continuation Start.  Cont is the continuation which receives the value of
;;; the Form to be translated.  The translators call this function recursively
;;; to translate their subnodes.
;;;
;;;    As a special hack to make life easier in the compiler, a Leaf
;;; IR1-converts into a reference to that leaf structure.  This allows the
;;; creation using backquote of forms that contain leaf references, without
;;; having to introduce dummy names into the namespace.
;;;
(defun ir1-convert (start cont form)
  (declare (type continuation start cont))
  (ir1-error-bailout (start cont form)
    (let ((*current-path* (or (gethash form *source-paths*)
			      (cons form *current-path*))))
      (if (atom form)
	  (cond ((and (symbolp form) (not (keywordp form)))
		 (ir1-convert-variable start cont form))
		((leaf-p form)
		 (reference-leaf start cont form))
		(t
		 (reference-constant start cont form)))
	  (let ((fun (car form)))
	    (cond
	     ((symbolp fun)
	      (let ((lexical-def (lexenv-find-function fun)))
		(typecase lexical-def
		  (null
		   (when (eq fun 'declare)
		     (compiler-error "Misplaced declaration."))
		   (ir1-convert-global-functoid start cont form))
		  (functional
		   (ir1-convert-local-combination start cont form lexical-def))
		  (global-var
		   (ir1-convert-srctran start cont lexical-def form))
		  (t
		   (assert (and (consp lexical-def)
				(eq (car lexical-def) 'macro)))
		   (ir1-convert start cont
				(careful-expand-macro (cdr lexical-def)
						      form))))))
	     ((or (atom fun) (not (eq (car fun) 'lambda)))
	      (compiler-error "Illegal function call."))
	     (t
	      (ir1-convert-combination start cont form
				       ;; TODO: check this case --jwr
				       (ir1-convert-lambda fun 
							   nil ; name
							   nil ; parent-form
							   t
							   'ir1-convert)))))))))


;;; Reference-Constant  --  Internal
;;;
;;; Generate a reference to a manifest constant, creating a new leaf if
;;; necessary.  If we are producing a fasl-file, make sure MAKE-LOAD-FORM
;;; gets used on any parts of the constant that it needs to be.
;;;
(defun reference-constant (start cont value)
  (declare (type continuation start cont) (inline find-constant))
  (ir1-error-bailout
      (start cont value
       '(error "Attempt to reference undumpable constant."))
    (when (and (producing-fasl-file)
	       (not (typep value '(or symbol number character string))))
      (maybe-emit-make-load-forms value))
    (let* ((leaf (find-constant value))
	   (res (make-ref (leaf-type leaf) leaf)))
      (push res (leaf-refs leaf))
      (prev-link res start)
      (use-continuation res cont)))
  (undefined-value))


;;; MAYBE-REANALYZE-FUNCTION  --  Internal
;;;
;;;    Add Fun to the COMPONENT-REANALYZE-FUNCTIONS.  Fun is returned.
;;;
(defun maybe-reanalyze-function (fun)
  (declare (type functional fun))
  (when (typep fun '(or optional-dispatch clambda))
    (pushnew fun (component-reanalyze-functions *current-component*)))
  fun)


;;; Reference-Leaf  --  Internal
;;;
;;;    Generate a Ref node for a Leaf, frobbing the Leaf structure as needed.
;;; If the leaf is a defined function which has already been converted, and is
;;; not :NOTINLINE, then reference the functional instead.
;;;
(defun reference-leaf (start cont leaf)
  (declare (type continuation start cont) (type leaf leaf))
  (let* ((leaf (or (and (defined-function-p leaf)
			(not (eq (defined-function-inlinep leaf)
				 :notinline))
			(let ((fun (defined-function-functional leaf)))
			  (when (and fun (not (functional-kind fun)))
			    (maybe-reanalyze-function fun))))
		   leaf))
	 (res (make-ref (or (lexenv-find leaf type-restrictions)
			    (leaf-type leaf))
			leaf)))
    (push res (leaf-refs leaf))
    (setf (leaf-ever-used leaf) t)
    (prev-link res start)
    (use-continuation res cont)))


;;; IR1-Convert-Variable  --  Internal
;;;
;;;    Convert a reference to a symbolic constant or variable.  If the symbol
;;; is entered in the LEXENV-VARIABLES we use that definition, otherwise we
;;; find the current global definition.  This is also where we pick off symbol
;;; macro and Alien variable references.
;;;
(defun ir1-convert-variable (start cont name)
  (declare (type continuation start cont) (symbol name))
  (let ((var (or (lexenv-find name variables) (find-free-variable name))))
    (etypecase var
      (leaf
       (when (lambda-var-p var)
	 (when (lambda-var-ignorep var)
	   (compiler-note "Reading an ignored variable: ~S." name))
	 ;;
	 ;; FIXME: There's a quirk somewhere when recording this
	 ;; dependency, which I don't have to time to debug right now.
	 ;; Redefining a function like this:
	 ;;
	 ;; (defun foo ())
	 ;;
	 ;; (let ((foo #'foo))
	 ;;   (defun foo () (funcall foo)))
	 ;;
	 ;; leads to infinite recursion because the funcall uses
	 ;; FOO's fdefn object instead of the local variable's value.
	 ;; -- Gerd, 2003-11-04
	 #+nil
	 (note-dfo-dependency start var))
       (reference-leaf start cont var))
      (cons
       (assert (eq (car var) 'MACRO))
       (ir1-convert start cont (cdr var)))
      (heap-alien-info
       (ir1-convert start cont `(%heap-alien ',var)))))
  (undefined-value))


;;; IR1-Convert-Global-Functoid  --  Internal
;;;
;;;    Convert anything that looks like a special-form, global function or
;;; macro call.
;;;
(defun ir1-convert-global-functoid (start cont form)
  (declare (type continuation start cont) (list form))
  (let* ((fun (first form))
	 (translator (info function ir1-convert fun))
	 (cmacro (info function compiler-macro-function fun)))
    (cond
     (translator (funcall translator start cont form))
     ((and cmacro (not *converting-for-interpreter*)
	   (not (eq (info function inlinep fun) :notinline)))
      (let ((res (careful-expand-macro cmacro form)))
	(if (eq res form)
	    (ir1-convert-global-functoid-no-cmacro start cont form fun)
	    (ir1-convert start cont res))))
     (t
      (ir1-convert-global-functoid-no-cmacro start cont form fun)))))


;;; IR1-Convert-Global-Functoid-No-Cmacro  --  Internal
;;;
;;;     Handle the case of where the call was not a compiler macro, or was a
;;; compiler macro and passed.
;;;
(defun ir1-convert-global-functoid-no-cmacro (start cont form fun)
  (declare (type continuation start cont) (list form))
  (ecase (info function kind fun)
    (:macro
     (when c:*record-xref-info*
       (let ((leb (lexenv-blocks *lexical-environment*)))
         (unless (or (null leb) (null (caar leb)))
           (xref:register-xref :macroexpands fun
                               (xref:make-xref-context :name (caar leb))))))
     (ir1-convert start cont
		  (careful-expand-macro (info function macro-function fun)
					form)))
    ((nil :function)
     (ir1-convert-srctran start cont (find-free-function fun "")
			  form))))


;;; Careful-Expand-Macro  --  Internal
;;;
;;;    Trap errors during the macroexpansion.
;;;
(defun careful-expand-macro (fun form)
  (handler-case (invoke-macroexpand-hook fun form *lexical-environment*)
    (error (condition)
	   (compiler-error "(during macroexpansion)~%~A"
			   condition))))


;;;; Conversion utilities:

;;; IR1-Convert-Progn-Body  --  Internal
;;;
;;;    Convert a bunch of forms, discarding all the values except the last.
;;; If there aren't any forms, then translate a NIL.
;;;
(defun ir1-convert-progn-body (start cont body)
  (declare (type continuation start cont) (list body))
  (if (endp body)
      (reference-constant start cont nil)
      (let ((this-start start)
	    (forms body))
	(loop
	  (let ((form (car forms)))
	    (when (endp (cdr forms))
	      (ir1-convert this-start cont form)
	      (return))
	    (let ((this-cont (make-continuation)))
	      (ir1-convert this-start this-cont form)
	      (setq this-start this-cont  forms (cdr forms))))))))


;;;; Converting combinations:

;;; IR1-Convert-Combination  --  Internal
;;;
;;;    Convert a function call where the function (Fun) is a Leaf.  We return
;;; the Combination node so that we can poke at it if we want to.
;;;
(defun ir1-convert-combination (start cont form fun)
  (declare (type continuation start cont) (list form) (type leaf fun)
	   (values combination))
  (let ((indices (dynamic-extent-closure-args (cdr form))))
    (if indices
	(with-dynamic-extent (start cont nnext-cont :closure)
	  (when *dynamic-extent-trace*
	    (format t "~&dynamic-extent args ~:s in ~s~%" indices form))
	  (let ((fun-cont (make-continuation)))
	    (reference-leaf nnext-cont fun-cont fun)
	    (ir1-convert-combination-args fun-cont cont (cdr form) indices)))
	(let ((fun-cont (make-continuation)))
	  (reference-leaf start fun-cont fun)
	  (ir1-convert-combination-args fun-cont cont (cdr form))))))
      

;;; IR1-Convert-Combination-Args  --  Internal
;;;
;;;    Convert the arguments to a call and make the Combination node.  Fun-Cont
;;; is the continuation which yields the function to call.  Form is the source
;;; for the call.  Args is the list of arguments for the call, which defaults
;;; to the cdr of source.  We return the Combination node.
;;;
(defun ir1-convert-combination-args (fun-cont cont args
				     &optional dynamic-extent-args)
  (declare (type continuation fun-cont cont) (list args))
  (let ((node (make-combination fun-cont)))
    (setf (continuation-dest fun-cont) node)
    (assert-continuation-type
     fun-cont (values-specifier-type '(values (or function symbol) &rest t)))
    (collect ((arg-conts))
      (let ((this-start fun-cont)
	    (lambda-vars (let* ((use (continuation-use fun-cont))
				(leaf (when use (ref-leaf use))))
			   (when (lambda-p leaf)
			     (lambda-vars leaf))))
	    (i 0))
	(dolist (arg args)
	  (let ((this-cont (make-continuation node)))
	    (when (or (and lambda-vars
			   (leaf-dynamic-extent (pop lambda-vars)))
		      (member i dynamic-extent-args))
	      (setf (continuation-dynamic-extent this-cont) t))
	    (ir1-convert this-start this-cont arg)
	    (setq this-start this-cont)
	    (arg-conts this-cont)
	    (incf i)))
	(prev-link node this-start)
	(use-continuation node cont)
	(setf (combination-args node) (arg-conts))))
    node))


;;; IR1-CONVERT-SRCTRAN  --  Internal
;;;
;;;    Convert a call to a global function.  If not :NOTINLINE, then we do
;;; source transforms and try out any inline expansion.  If there is no
;;; expansion, but is :INLINE, then give an efficiency note (unless a known
;;; function which will quite possibly be open-coded.)   Next, we go to
;;; ok-combination conversion.
;;;
(defun ir1-convert-srctran (start cont var form)
  (declare (type continuation start cont) (type global-var var))
  (let ((inlinep (when (defined-function-p var)
		   (defined-function-inlinep var))))
    (cond
     ((eq inlinep :notinline)
      (ir1-convert-combination start cont form var))
     (*converting-for-interpreter*
      (ir1-convert-combination-checking-type start cont form var))
     (t
      (let ((transform (info function source-transform (leaf-name var))))
	(cond
	 (transform
	  (multiple-value-bind (result pass)
			       (funcall transform form)
	    (if pass
		(ir1-convert-maybe-predicate start cont form var)
		(ir1-convert start cont result))))
	 (t
	  (ir1-convert-maybe-predicate start cont form var))))))))


;;; IR1-CONVERT-MAYBE-PREDICATE  --  Internal
;;;
;;;    If the function has the Predicate attribute, and the CONT's DEST isn't
;;; an IF, then we convert (IF <form> T NIL), ensuring that a predicate always
;;; appears in a conditional context.
;;;
;;;    If the function isn't a predicate, then we call
;;; IR1-CONVERT-COMBINATION-CHECKING-TYPE.
;;;
(defun ir1-convert-maybe-predicate (start cont form var)
  (declare (type continuation start cont) (list form) (type global-var var))
  (let ((info (info function info (leaf-name var))))
    (if (and info
	     (ir1-attributep (function-info-attributes info) predicate)
	     (not (if-p (continuation-dest cont))))
	(ir1-convert start cont `(if ,form t nil))
	(ir1-convert-combination-checking-type start cont form var))))


;;; IR1-CONVERT-COMBINATION-CHECKING-TYPE  --  Internal
;;;
;;;    Actually really convert a global function call that we are allowed to
;;; early-bind.
;;;
;;; If we know the function type of the function, then we check the call for
;;; syntactic legality with respect to the declared function type.  If it is
;;; impossible to determine whether the call is correct due to non-constant
;;; keywords, then we give up, marking the call as :FULL to inhibit further
;;; error messages.  We return true when the call is legal.
;;;
;;; If the call is legal, we also propagate type assertions from the function
;;; type to the arg and result continuations.  We do this now so that IR1
;;; optimize doesn't have to redundantly do the check later so that it can do
;;; the type propagation.
;;;
;;;
(defun ir1-convert-combination-checking-type (start cont form var)
  (declare (type continuation start cont) (list form) (type leaf var))
  (let* ((node (ir1-convert-combination start cont form var))
	 (fun-cont (basic-combination-fun node))
	 (type (leaf-type var)))
    (when (validate-call-type node type t)
      (setf (continuation-%derived-type fun-cont) type)
      (setf (continuation-reoptimize fun-cont) nil)
      (setf (continuation-%type-check fun-cont) nil)))

  (undefined-value))


;;; IR1-CONVERT-LOCAL-COMBINATION  --  Internal
;;;
;;;    Convert a call to a local function.  If the function has already been
;;; let converted, then throw FUN to LOCAL-CALL-LOSSAGE.  This should only
;;; happen when we are converting inline expansions for local functions during
;;; optimization.
;;;
(defun ir1-convert-local-combination (start cont form fun)
  (if (functional-kind fun)
      (throw 'local-call-lossage fun)
      (ir1-convert-combination start cont form
			       (maybe-reanalyze-function fun))))


;;;; PROCESS-DECLARATIONS:

(declaim (start-block process-declarations make-new-inlinep
		      find-in-bindings))

;;; Find-In-Bindings  --  Internal
;;;
;;;    Given a list of Lambda-Var structures and a variable name, return the
;;; structure for that name, or NIL if it isn't found.  We return the *last*
;;; variable with that name, since let* bindings may be duplicated, and
;;; declarations always apply to the last.
;;;
(defun find-in-bindings (vars name)
  (declare (list vars) (symbol name) (values (or lambda-var list)))
  (let ((found nil))
    (dolist (var vars)
      (cond ((leaf-p var)
	     (when (eq (leaf-name var) name)
	       (setq found var))
	     (let ((info (lambda-var-arg-info var)))
	       (when info
		 (let ((supplied-p (arg-info-supplied-p info)))
		   (when (and supplied-p
			      (eq (leaf-name supplied-p) name))
		     (setq found supplied-p))))))
	    ((and (consp var) (eq (car var) name))
	     (setf found (cdr var)))))
    found))


;;; Process-Type-Declaration  --  Internal
;;;
;;;    Called by Process-Declarations to deal with a variable type declaration.
;;; If a lambda-var being bound, we intersect the type with the vars type,
;;; otherwise we add a type-restriction on the var.  If a symbol macro, we just
;;; wrap a THE around the expansion.
;;;
(defun process-type-declaration (decl res vars)
  (declare (list decl vars) (type lexenv res))
  (let ((type (specifier-type (first decl))))
    (collect ((restr nil cons)
	      (new-vars nil cons))
      (dolist (var-name (rest decl))
	(let* ((bound-var (find-in-bindings vars var-name))
	       (var (or bound-var
			(lexenv-find var-name variables)
			(find-free-variable var-name))))
	  (etypecase var
	    (leaf
	     (flet ((process (var bound-var)
		      (let* ((old-type (or (lexenv-find var type-restrictions)
					   (leaf-type var)))
			     (int (if (or (function-type-p type)
					  (function-type-p old-type))
				      type
				      (type-intersection old-type type))))
			(cond ((eq int *empty-type*)
			       (unless (policy nil (= brevity 3))
				 (compiler-warning
				  "Conflicting type declarations ~
				   ~S and ~S for ~S."
				  (type-specifier old-type)
				  (type-specifier type)
				  var-name)))
			      (bound-var
			       (setf (leaf-type bound-var) int))
			      (t
			       (restr (cons var int)))))))
	       (process var bound-var)
	       (when (lambda-var-p var)
		 (let ((special (lambda-var-specvar var)))
		   (when special
		     (process special nil))))))
	    (cons
	     (assert (eq (car var) 'MACRO))
	     (new-vars `(,var-name . (MACRO . (the ,(first decl)
						   ,(cdr var))))))
	    (heap-alien-info
	     (compiler-error "Can't declare type of Alien variable: ~S."
			     var-name)))))

      (if (or (restr) (new-vars))
	  (make-lexenv :default res
		       :type-restrictions (restr)
		       :variables (new-vars))
	  res))))


;;; Process-Ftype-Declaration  --  Internal
;;;
;;;    Somewhat similar to PROCESS-TYPE-DECLARATION, but handles declarations
;;; for function variables.  In addition to allowing declarations for functions
;;; being bound, we must also deal with declarations that constrain the type of
;;; lexically apparent functions.
;;;
(defun process-ftype-declaration (spec res names fvars)
  (declare (list names fvars) (type lexenv res)
	   (type (or symbol list) spec))
  (let ((type (specifier-type spec)))
    (collect ((res nil cons))
      (dolist (name names)
	(let ((found (find name fvars :key #'leaf-name :test #'function-name-eqv-p)))
	  (cond
	   (found
	    (setf (leaf-type found) type)
	    (assert-definition-type found type
				    :warning-function #'compiler-note
				    :where "FTYPE declaration"))
	   (t
	    (res (cons (find-lexically-apparent-function
			name "in a function type declaration")
		       type))))))
      (if (res)
	  (make-lexenv :default res  :type-restrictions (res))
	  res))))


;;; PROCESS-SPECIAL-DECLARATION  --  Internal
;;;
;;;    Process a special declaration, returning a new LEXENV.  A non-bound
;;; special declaration is instantiated by throwing a special variable into the
;;; variables.
;;;
(defun process-special-declaration (spec res vars)
  (declare (list spec vars) (type lexenv res))
  (collect ((new-venv nil cons))
    (dolist (name (cdr spec))
      (let ((var (find-in-bindings vars name)))
	(etypecase var
	  (cons
	   (assert (eq (car var) 'MACRO))
	   (compiler-error "Declaring symbol-macro ~S special." name))
	  (lambda-var
	   (when (lambda-var-ignorep var)
	     (compiler-note "Ignored variable ~S is being declared special."
			    name))
	   (setf (lambda-var-specvar var)
		 (specvar-for-binding name)))
	  (null
	   (unless (assoc name (new-venv) :test #'eq)
	     (new-venv (cons name (specvar-for-binding name))))))))
    (if (new-venv)
	(make-lexenv :default res  :variables (new-venv))
	res)))


;;; MAKE-NEW-INLINEP  --  Internal
;;;
;;;    Return a DEFINED-FUNCTION which copies a global-var but for its inlinep.
;;;
(defun make-new-inlinep (var inlinep)
  (declare (type global-var var) (type inlinep inlinep))
  (let ((res (make-defined-function
	      :name (leaf-name var)
	      :where-from (leaf-where-from var)
	      :type (leaf-type var)
	      :inlinep inlinep)))
    (when (defined-function-p var)
      (setf (defined-function-inline-expansion res)
	    (defined-function-inline-expansion var))
      (setf (defined-function-functional res)
	    (defined-function-functional var)))
    res))


(defconstant inlinep-translations
  '((inline . :inline)
    (notinline . :notinline)
    (maybe-inline . :maybe-inline)))


;;; PROCESS-INLINE-DECLARATION  --  Internal
;;;
;;;    Parse an inline/notinline declaration.  If a local function we are
;;; defining, set its INLINEP.  If a global function, add a new FENV entry.
;;;
(defun process-inline-declaration (spec res fvars)
  (let ((sense (cdr (assoc (first spec) inlinep-translations :test #'eq)))
	(new-fenv ()))
    (dolist (name (rest spec))
      (let ((fvar (find name fvars
			:key #'(lambda (x)
				 ;; FVARS doesn't always contain a
				 ;; LEAF.  Sometimes it comes from a
				 ;; macrolet.
				 (and (leaf-p x)
				      (leaf-name x)))
			:test #'function-name-eqv-p)))
	(if fvar
	    (setf (functional-inlinep fvar) sense)
	    (let ((found
		   (find-lexically-apparent-function
		    name "in an inline or notinline declaration")))
	      (etypecase found
		(functional
		 (when (policy nil (>= speed brevity))
		   (compiler-note "Ignoring ~A declaration not at ~
				   definition of local function:~%  ~S"
				  sense name)))
		(global-var
		 (push (cons name (make-new-inlinep found sense))
		       new-fenv)))))))
    
    (if new-fenv
	(make-lexenv :default res  :functions new-fenv)
	res)))


;;; FIND-IN-BINDINGS-OR-FBINDINGS  --  Internal
;;;
;;;    Like FIND-IN-BINDINGS, but looks for #'foo in the fvars.
;;;
(defun find-in-bindings-or-fbindings (name vars fvars)
  (declare (list vars fvars))
  (if (consp name)
      (destructuring-bind (wot fn-name) name
	(unless (eq wot 'function)
	  (compiler-error "Unrecognizable function or variable name: ~S"
			  name))
	(find fn-name fvars
	      :key #'leaf-name
	      :test #'function-name-eqv-p))
      (find-in-bindings vars name)))


;;; PROCESS-IGNORE-DECLARATION  --  Internal
;;;
;;;    Process an ignore/ignorable declaration, checking for various losing
;;; conditions.
;;;
(defun process-ignore-declaration (spec vars fvars)
  (declare (list spec vars fvars))
  (dolist (name (rest spec))
    (let ((var (find-in-bindings-or-fbindings name vars fvars)))
      (cond
       ((not var)
	(if (or (lexenv-find name variables)
		(lexenv-find-function name))
	    (compiler-note "Ignoring free ignore declaration for ~S." name)
	    (compiler-warning "Ignore declaration for unknown variable ~S."
			      name)))
       ((and (consp var)
	     (eq (car var) 'macro)
	     ;; Var is '(macro foo). Why must foo be a cons?  This
	     ;; causes (symbol-macrolet ((a 42)) (declare (ignorable
	     ;; a)) ...) to get a type error from a test below because
	     ;; var is '(macro . 42) in this case.
	     #+nil
	     (consp (cdr var)))
	;; Just ignore the ignore decl.
	)
       ((functional-p var)
	(setf (leaf-ever-used var) t))
       ((lambda-var-specvar var)
	(compiler-note "Declaring special variable ~S to be ignored." name))
       ((eq (first spec) 'ignorable)
	(setf (leaf-ever-used var) t))
       (t
	(setf (lambda-var-ignorep var) t)))))
  (undefined-value))

(defvar *suppress-values-declaration* nil
  "If true, processing of the VALUES declaration is inhibited.")

;;; PROCESS-1-DECLARATION  --  Internal
;;;
;;;    Process a single declaration spec, agumenting the specified LEXENV
;;; Res and returning it as a result.  Vars and Fvars are as described in
;;; PROCESS-DECLARATIONS.
;;;
(defun process-1-declaration (spec res vars fvars cont)
  (declare (list spec vars fvars) (type lexenv res) (type continuation cont))
  (case (first spec)
    (special (process-special-declaration spec res vars))
    (ftype
     (unless (cdr spec)
       (compiler-error "No type specified in FTYPE declaration: ~S." spec))
     (process-ftype-declaration (second spec) res (cddr spec) fvars))
    (function
     ;;
     ;; Handle old style FUNCTION declaration, which is an abbreviation for
     ;; FTYPE.  Args are name, arglist, result type.
     (cond ((and (<= 3 (length spec) 4) (listp (third spec)))
	    (process-ftype-declaration `(function ,@(cddr spec)) res
				       (list (second spec))
				       fvars))
	   (t
	    (process-type-declaration spec res vars))))
    ((inline notinline maybe-inline)
     (process-inline-declaration spec res fvars))
    ((ignore ignorable)
     (process-ignore-declaration spec vars fvars)
     res)
    (optimize
     (make-lexenv
      :default res
      :cookie (process-optimize-declaration spec (lexenv-cookie res))))
    (optimize-interface
     (make-lexenv
      :default res
      :interface-cookie (process-optimize-declaration
			 spec
			 (lexenv-interface-cookie res))))
    (type
     (process-type-declaration (cdr spec) res vars))
    (pcl::class
     (process-type-declaration (list (third spec) (second spec)) res vars))
    (values
     (if *suppress-values-declaration*
	 res
	 (let ((types (cdr spec)))
	   (do-the-stuff (values-specifier-type (if (eql (length types) 1)
						    (car types)
						    `(values ,@types)))
			 cont res 'values))))
    (dynamic-extent
     (process-dynamic-extent-declaration spec vars fvars res))
    (t
     (let ((what (first spec)))
       (cond ((member what type-specifier-symbols)
	      (process-type-declaration spec res vars))
	     ((and (not (and (symbolp what)
			     (string= (symbol-name what) "CLASS"))) ; pcl hack
		   (or (info type kind what)
		       (and (consp what) (info type translator (car what)))))
	      (compiler-note "Abbreviated type declaration: ~S." spec)
	      (process-type-declaration spec res vars))
	     ((info declaration recognized what)
	      res)
	     (t
	      (compiler-warning "Unrecognized declaration: ~S." spec)
	      res))))))


;;; Process-Declarations  --  Interface
;;;
;;;    Use a list of Declare forms to annotate the lists of Lambda-Var and
;;; Functional structures which are being bound.  In addition to filling in
;;; slots in the leaf structures, we return a new LEXENV which reflects
;;; pervasive special and function type declarations, (not)inline declarations
;;; and optimize declarations.  Cont is the continuation affected by VALUES
;;; declarations.
;;;
;;; This is also called in main.lisp when PROCESS-FORM handles a use of
;;; LOCALLY.
;;;
(defun process-declarations (decls vars fvars cont &optional
				   (env *lexical-environment*))
  (declare (list decls vars fvars) (type continuation cont))
  (dolist (decl decls)
    (dolist (spec (rest decl))
      (unless (consp spec)
	(compiler-error "Malformed declaration specifier ~S in ~S."
			spec decl))
      
      (setq env (process-1-declaration spec env vars fvars cont))))
  env)

;;; Specvar-For-Binding  --  Internal
;;;
;;;    Return the Specvar for Name to use when we see a local SPECIAL
;;; declaration.  If there is a global variable of that name, then check that
;;; it isn't a constant and return it.  Otherwise, create an anonymous
;;; GLOBAL-VAR.
;;;
(defun specvar-for-binding (name)
  (cond ((not (eq (info variable where-from name) :assumed))
	 (let ((found (find-free-variable name)))
	   (when (heap-alien-info-p found)
	     (compiler-error "Declaring an alien variable to be special: ~S"
			     name))
	   (when (or (not (global-var-p found))
		     (eq (global-var-kind found) :constant))
	     (compiler-error "Declaring a constant to be special: ~S." name))
	   found))
	(t
	 (make-global-var :kind :special  :name name  :where-from :declared))))


;;;; Lambda hackery:  

(declaim (start-block ir1-convert-lambda ir1-convert-lambda-body
		      ir1-convert-aux-bindings varify-lambda-arg
		      ir1-convert-dynamic-extent-bindings))

;;; Varify-Lambda-Arg  --  Internal
;;;
;;;    Verify that a thing is a legal name for a variable and return a Var
;;; structure for it, filling in info if it is globally special.  If it is
;;; losing, we punt with a Compiler-Error.  Names-So-Far is an alist of names
;;; which have previously been bound.  If the name is in this list, then we
;;; error out.
;;;
(defun varify-lambda-arg (name names-so-far)
  (declare (list names-so-far) (values lambda-var)
	   (inline member))
  (unless (symbolp name)
    (compiler-error "Lambda-variable is not a symbol: ~S." name))
  (when (member name names-so-far :test #'eq)
    (compiler-error "Repeated variable in lambda-list: ~S." name))
  (let ((kind (info variable kind name)))
    (when (or (keywordp name) (eq kind :constant))
      (compiler-error "Name of lambda-variable is a constant: ~S." name))
    (if (eq kind :special)
	(let ((specvar (find-free-variable name)))
	  (make-lambda-var :name name
			   :type (leaf-type specvar)
			   :where-from (leaf-where-from specvar)
			   :specvar specvar))
	(make-lambda-var :name name))))


;;; Make-Keyword  --  Internal
;;;
;;;    Make the keyword for a keyword arg, checking that the keyword isn't
;;; already used by one of the Vars.
;;;
(defun make-keyword (symbol vars keywordify)
  (declare (symbol symbol) (list vars) (values symbol))
  (let ((key (if (and keywordify (not (keywordp symbol)))
		 (intern (symbol-name symbol) "KEYWORD")
		 symbol)))
    (dolist (var vars)
      (let ((info (lambda-var-arg-info var)))
	(when (and info
		   (eq (arg-info-kind info) :keyword)
		   (eq (arg-info-keyword info) key))
	  (compiler-error "Multiple uses of keyword ~S in lambda-list." key))))
    key))


;;; IR1-wrap-for-debug -- Internal
;;;
;;; Wrap a piece of code in a catch form, so that we can later throw to it 
;;; in the debugger, to return a value.
;;;
(defun ir1-wrap-for-debug (body)
  (let ((new-body `((catch (make-symbol "CMUCL-DEBUG-CATCH-TAG")
		      ,@body))))
    (when (and *compile-print* 
	       *print-debug-tag-conversions*
	       *print-debug-tag-converted-bodies*)
      (format t "new-body: ~S~%" new-body))
    new-body))


;;; IR1-Convert-Lambda  --  Internal
;;;
;;;    Convert a Lambda into a Lambda or Optional-Dispatch leaf.  NAME and
;;; PARENT-FORM are context that is used to drive the context sensitive
;;; declaration mechanism.  If we find an entry in *CONTEXT-DECLARATIONS* that
;;; matches this context (by returning a non-null value) then we add it into
;;; the local declarations.
;;;
(defun ir1-convert-lambda (form &optional
				name 
				parent-form
				allow-debug-catch-tag
				caller)
  (unless (consp form)
    (compiler-error "Found a ~S when expecting a lambda expression:~%  ~S"
		    (type-of form) form))
  (unless (eq (car form) 'lambda)
    (compiler-error "Expecting a lambda, but form begins with ~S:~%  ~S"
		    (car form) form))
  (unless (and (consp (cdr form)) (listp (cadr form)))
    (compiler-error "Lambda-list absent or not a list:~%  ~S" form))

  (multiple-value-bind (vars keyp allow-other-keys aux-vars aux-vals)
      (find-lambda-vars (cadr form))
    (multiple-value-bind (body decls)
	(system:parse-body (cddr form) *lexical-environment* t)
      (let* ((*allow-debug-catch-tag* (and *allow-debug-catch-tag* 
					   allow-debug-catch-tag))
	     (new-body (if (and parent-form
				*allow-debug-catch-tag*
				(policy nil (= debug 3))) ; TODO: check the policy settings --jwr
			   (progn
			     (when (and *compile-print* *print-debug-tag-conversions*)
			       (format t "ir1-convert-lambda: called by: ~S, parent-form: ~S~%" 
				       caller parent-form))
			     (ir1-wrap-for-debug body))
			   body))
	     (*current-function-names*
	      (if (member parent-form
			  '(flet labels defun defmacro define-compiler-macro)
			  :test #'eq)
		  (list name)
		  *current-function-names*))
	     (context-decls
	      (and parent-form
		   (loop for fun in *context-declarations*
			 append (funcall (the function fun)
					 name parent-form))))
	     (cont (make-continuation))
	     (*lexical-environment*
	      (process-declarations (append context-decls decls)
				    (append aux-vars vars)
				    nil cont))
	     (res (if (or (find-if #'lambda-var-arg-info vars) keyp)
		      (ir1-convert-hairy-lambda new-body vars keyp
						allow-other-keys
						aux-vars aux-vals cont)
		      (ir1-convert-lambda-body new-body vars aux-vars aux-vals
					       t cont))))
	(setf (functional-inline-expansion res) form)
	(setf (functional-arg-documentation res) (cadr form))
	(setf (leaf-name res)
	      (or name
		  ;; PCL-generated lambdas end up here without an explicit NAME.
		  ;; To avoid ending up with IR1 lambda-nodes that are unnamed,
		  ;; we extract a name from the "method-name" declaration that
		  ;; is inserted by PCL. A cleaner solution would be to add a
		  ;; NAMED-LAMBDA IR1 translator, similar to that used in SBCL,
		  ;; and make PCL use that instead of LAMBDA.
		  (let ((decl (find 'pcl::method-name decls :key 'caadr)))
		    (and decl
			 (eq 'declare (first decl))
			 (cons 'pcl::method (cadadr decl))))))
	res))))


;;; Find-Lambda-Vars  --  Internal
;;;
;;;    Parse a lambda-list into a list of Var structures, stripping off any aux
;;; bindings.  Each arg name is checked for legality, and duplicate names are
;;; checked for.  If an arg is globally special, the var is marked as :special
;;; instead of :lexical.  Keyword, optional and rest args are annotated with an
;;; arg-info structure which contains the extra information.  If we hit
;;; something losing, we bug out with Compiler-Error.  These values are
;;; returned:
;;;  1] A list of the var structures for each top-level argument.
;;;  2] A flag indicating whether &key was specified.
;;;  3] A flag indicating whether other keyword args are allowed.
;;;  4] A list of the &aux variables.
;;;  5] A list of the &aux values.
;;;
(defun find-lambda-vars (list)
  (declare (list list) (values list boolean boolean list list))
  (multiple-value-bind
      (required optional restp rest keyp keys allowp aux
		morep more-context more-count)
      (parse-lambda-list list)
    (collect ((vars)
	      (names-so-far)
	      (aux-vars)
	      (aux-vals))
      ;;
      ;; Parse-Default deals with defaults and supplied-p args for optionals
      ;; and keywords args.
      (flet ((parse-default (spec info)
	       (when (consp (cdr spec))
		 (setf (arg-info-default info) (second spec))
		 (when (consp (cddr spec))
		   (let* ((supplied-p (third spec))
			  (supplied-var (varify-lambda-arg supplied-p (names-so-far))))
		     (setf (arg-info-supplied-p info) supplied-var)
		     (names-so-far supplied-p)
		     (when (> (length (the list spec)) 3)
		       (compiler-error "Arg specifier is too long: ~S." spec)))))))
	
	(dolist (name required)
	  (let ((var (varify-lambda-arg name (names-so-far))))
	    (vars var)
	    (names-so-far name)))
	
	(dolist (spec optional)
	  (if (atom spec)
	      (let ((var (varify-lambda-arg spec (names-so-far))))
		(setf (lambda-var-arg-info var) (make-arg-info :kind :optional))
		(vars var)
		(names-so-far spec))
	      (let* ((name (first spec))
		     (var (varify-lambda-arg name (names-so-far)))
		     (info (make-arg-info :kind :optional)))
		(setf (lambda-var-arg-info var) info)
		(vars var)
		(names-so-far name)
		(parse-default spec info))))
	
	(when restp
	  (let ((var (varify-lambda-arg rest (names-so-far))))
	    (setf (lambda-var-arg-info var) (make-arg-info :kind :rest))
	    (vars var)
	    (names-so-far rest)))

	(when morep
	  (let ((var (varify-lambda-arg more-context (names-so-far))))
	    (setf (lambda-var-arg-info var)
		  (make-arg-info :kind :more-context))
	    (vars var)
	    (names-so-far more-context))
	  (let ((var (varify-lambda-arg more-count (names-so-far))))
	    (setf (lambda-var-arg-info var)
		  (make-arg-info :kind :more-count))
	    (vars var)
	    (names-so-far more-count)))
	
	(dolist (spec keys)
	  (cond
	   ((atom spec)
	    (let ((var (varify-lambda-arg spec (names-so-far))))
	      (setf (lambda-var-arg-info var)
		    (make-arg-info :kind :keyword
				   :keyword (make-keyword spec (vars) t)))
	      (vars var)
	      (names-so-far spec)))
	   ((atom (first spec))
	    (let* ((name (first spec))
		   (var (varify-lambda-arg name (names-so-far)))
		   (info (make-arg-info
			  :kind :keyword
			  :keyword (make-keyword name (vars) t))))
	      (setf (lambda-var-arg-info var) info)
	      (vars var)
	      (names-so-far name)
	      (parse-default spec info)))
	   (t
	    (let ((head (first spec)))
	      (unless (= (length (the list head)) 2)
		(error "Malformed keyword arg specifier: ~S." spec))
	      (let* ((name (second head))
		     (var (varify-lambda-arg name (names-so-far)))
		     (info (make-arg-info
			    :kind :keyword
			    :keyword (make-keyword (first head) (vars) nil))))
		(setf (lambda-var-arg-info var) info)
		(vars var)
		(names-so-far name)
		(parse-default spec info))))))
	
	(dolist (spec aux)
	  (cond ((atom spec)
		 (let ((var (varify-lambda-arg spec nil)))
		   (aux-vars var)
		   (aux-vals nil)
		   (names-so-far spec)))
		(t
		 (unless (<= 1 (length spec) 2)
		   (compiler-error "Malformed &aux binding specifier: ~S."
				   spec))
		 (let* ((name (first spec))
			(var (varify-lambda-arg name nil)))
		   (aux-vars var)
		   (aux-vals (second spec))
		   (names-so-far name)))))
	  
	(values (vars) keyp allowp (aux-vars) (aux-vals))))))


;;; IR1-Convert-Aux-Bindings  --  Internal
;;;
;;;    Similar to IR1-Convert-Progn-Body except that we sequentially bind each
;;; Aux-Var to the corresponding Aux-Val before converting the body.  If there
;;; are no bindings, just convert the body, otherwise do one binding and
;;; recurse on the rest.
;;;
;;;    If Interface is true, then we convert bindings with the interface
;;; policy.  For real &aux bindings, and implicit aux bindings introduced by
;;; keyword bindings, this is always true.  It is only false when LET* directly
;;; calls this function.
;;;
(defun ir1-convert-aux-bindings (start cont body aux-vars aux-vals interface)
  (declare (type continuation start cont) (list body aux-vars aux-vals))
  (if (null aux-vars)
      (ir1-convert-progn-body start cont body)
      (let ((fun-cont (make-continuation))
	    (fun (ir1-convert-lambda-body body (list (first aux-vars))
					  (rest aux-vars) (rest aux-vals)
					  interface)))
	(reference-leaf start fun-cont fun)
	(let ((*lexical-environment*
	       (if interface
		   (make-lexenv
		    :cookie (make-interface-cookie *lexical-environment*))
		   *lexical-environment*)))
	  (ir1-convert-combination-args fun-cont cont
					(list (first aux-vals))))))
  (values))

(defun ir1-convert-dynamic-extent-bindings (start cont body aux-vars
					    aux-vals interface)
  (declare (type continuation start cont) (list body aux-vars aux-vals))
  (if (dynamic-extent-allocation-p aux-vars aux-vals)
      (with-dynamic-extent (start cont nnext-cont :bind)
	(ir1-convert-aux-bindings nnext-cont cont body aux-vars
				  aux-vals interface))
      (ir1-convert-aux-bindings start cont body aux-vars aux-vals interface))
  (values))


;;; IR1-Convert-Special-Bindings  --  Internal
;;;
;;;    Similar to IR1-Convert-Progn-Body except that code to bind the Specvar
;;; for each Svar to the value of the variable is wrapped around the body.  If
;;; there are no special bindings, we just convert the body, otherwise we do
;;; one special binding and recurse on the rest.
;;;
;;;    We make a cleanup and introduce it into the lexical environment.  If
;;; there are multiple special bindings, the cleanup for the blocks will end up
;;; being the innermost one.  We force Cont to start a block outside of this
;;; cleanup, causing cleanup code to be emitted when the scope is exited.
;;;
(defun ir1-convert-special-bindings (start cont body aux-vars aux-vals
				     interface svars)
  (declare (type continuation start cont)
	   (list body aux-vars aux-vals svars))
  (cond
   ((null svars)
    (ir1-convert-dynamic-extent-bindings start cont body aux-vars aux-vals
					 interface))
   (t
    (continuation-starts-block cont)
    (let ((cleanup (make-cleanup :kind :special-bind))
	  (var (first svars))
	  (next-cont (make-continuation))
	  (nnext-cont (make-continuation)))
      (ir1-convert start next-cont
		   `(%special-bind ',(lambda-var-specvar var) ,var))
      (setf (cleanup-mess-up cleanup) (continuation-use next-cont))
      (let ((*lexical-environment* (make-lexenv :cleanup cleanup)))
	(ir1-convert next-cont nnext-cont '(%cleanup-point))
	(ir1-convert-special-bindings nnext-cont cont body aux-vars aux-vals
				      interface (rest svars))))))
  (values))


;;; IR1-Convert-Lambda-Body  --  Internal
;;;
;;;    Create a lambda node out of some code, returning the result.  The
;;; bindings are specified by the list of var structures Vars.  We deal with
;;; adding the names to the Lexenv-Variables for the conversion.  The result is
;;; added to the New-Functions in the *Current-Component* and linked to the
;;; component head and tail.
;;;
;;; We detect special bindings here, replacing the original Var in the lambda
;;; list with a temporary variable.  We then pass a list of the special vars to
;;; IR1-Convert-Special-Bindings, which actually emits the special binding
;;; code.
;;;
;;; We ignore any Arg-Info in the Vars, trusting that someone else is dealing
;;; with &nonsense.
;;;
;;; Aux-Vars is a list of Var structures for variables that are to be
;;; sequentially bound.  Each Aux-Val is a form that is to be evaluated to get
;;; the initial value for the corresponding Aux-Var.  Interface is a flag as T
;;; when there are real aux values (see let* and ir1-convert-aux-bindings.)
;;;
(defun ir1-convert-lambda-body (body vars &optional aux-vars aux-vals
				     interface result)
  (declare (list body vars aux-vars aux-vals)
	   (type (or continuation null) result))
  (let* ((bind (make-bind))
	 (lambda (make-lambda :vars vars  :bind bind))
	 (result (or result (make-continuation)))
	 (dynamic-extent-rest nil))
    (setf (lambda-home lambda) lambda)
    (collect ((svars)
	      (new-venv nil cons))

      (dolist (var vars)
	(setf (lambda-var-home var) lambda)
	(let ((specvar (lambda-var-specvar var)))
	  (cond (specvar
		 (svars var)
		 (new-venv (cons (leaf-name specvar) specvar)))
		(t
		 (new-venv (cons (leaf-name var) var)))))
	(let ((info (lambda-var-arg-info var)))
	  (when (and info
		     (eq :rest (arg-info-kind info))
		     (leaf-dynamic-extent var))
	    (setq dynamic-extent-rest var))))

      (let* ((*lexical-environment*
	      (make-lexenv :variables (new-venv)  :lambda lambda
			   :cleanup nil)))
	(setf (bind-lambda bind) lambda)
	(setf (node-lexenv bind) *lexical-environment*)

	(let ((cont1 (make-continuation))
	      (cont2 (make-continuation)))
	  (continuation-starts-block cont1)
	  (prev-link bind cont1)
	  (use-continuation bind cont2)
	  (if dynamic-extent-rest
	      (with-dynamic-extent (cont2 result nnext-cont :rest)
		(ir1-convert-special-bindings nnext-cont result body
					      aux-vars aux-vals
					      interface (svars)))
	      (ir1-convert-special-bindings cont2 result body aux-vars aux-vals
					    interface (svars))))

	(let ((block (continuation-block result)))
	  (when block
	    (let ((return (make-return :result result
				       :lambda lambda))
		  (tail-set (make-tail-set :functions (list lambda)))
		  (dummy (make-continuation)))
	      (setf (lambda-tail-set lambda) tail-set)
	      (setf (lambda-return lambda) return)
	      (setf (continuation-dest result) return)
	      (setf (block-last block) return)
	      (prev-link return result)
	      (use-continuation return dummy))
	    (link-blocks block (component-tail *current-component*))))))

    (link-blocks (component-head *current-component*) (node-block bind))
    (push lambda (component-new-functions *current-component*))
    lambda))


;;; Convert-Optional-Entry  --  Internal
;;;
;;;    Create the actual entry-point function for an optional entry point.  The
;;; lambda binds copies of each of the Vars, then calls Fun with the argument
;;; Vals and the Defaults.  Presumably the Vals refer to the Vars by name.  The
;;; Vals are passed in in reverse order.
;;;
;;;    If any of the copies of the vars are referenced more than once, then we
;;; mark the corresponding var as Ever-Used to inhibit "defined but not read"
;;; warnings for arguments that are only used by default forms.
;;;
;;;    We bind *lexical-environment* to change the policy over to the interface
;;; policy.
;;;
(defun convert-optional-entry (fun vars vals defaults)
  (declare (type clambda fun) (list vars vals defaults))
  (let* ((fvars (reverse vars))
	 (arg-vars (mapcar #'(lambda (var)
			       (make-lambda-var
				:name (leaf-name var)
				:type (leaf-type var)
				:where-from (leaf-where-from var)
				:specvar (lambda-var-specvar var)))
			   fvars))
	 (*lexical-environment*
	  (make-lexenv :cookie (make-interface-cookie *lexical-environment*)))
	 (fun
	  (ir1-convert-lambda-body
	   `((%funcall ,fun ,@(reverse vals) ,@defaults))
	   arg-vars)))
    (mapc #'(lambda (var arg-var)
	      (when (cdr (leaf-refs arg-var))
		(setf (leaf-ever-used var) t)))
	  fvars arg-vars)
    fun))


;;; Generate-Optional-Default-Entry  --  Internal
;;;
;;;    This function deals with supplied-p vars in optional arguments.  If the
;;; there is no supplied-p arg, then we just call IR1-Convert-Hairy-Args on the
;;; remaining arguments, and generate a optional entry that calls the result.
;;; If there is a supplied-p var, then we add it into the default vars and
;;; throw a T into the entry values.  The resulting entry point function is
;;; returned.
;;;
(defun generate-optional-default-entry (res default-vars default-vals
					    entry-vars entry-vals
					    vars supplied-p-p body
					    aux-vars aux-vals cont)
  (declare (type optional-dispatch res)
	   (list default-vars default-vals entry-vars entry-vals vars body
		 aux-vars aux-vals)
	   (type (or continuation null) cont))
  (let* ((arg (first vars))
	 (arg-name (leaf-name arg))
	 (info (lambda-var-arg-info arg))
	 (supplied-p (arg-info-supplied-p info))
	 (ep (if supplied-p
		 (ir1-convert-hairy-args
		  res
		  (list* supplied-p arg default-vars)
		  (list* (leaf-name supplied-p) arg-name default-vals)
		  (cons arg entry-vars)
		  (list* t arg-name entry-vals)
		  (rest vars) t body aux-vars aux-vals cont)
		 (ir1-convert-hairy-args 
		  res
		  (cons arg default-vars)
		  (cons arg-name default-vals)
		  (cons arg entry-vars)
		  (cons arg-name entry-vals)
		  (rest vars) supplied-p-p body aux-vars aux-vals cont))))
		 
    (convert-optional-entry ep default-vars default-vals
			    (if supplied-p
				(list (arg-info-default info) nil)
				(list (arg-info-default info))))))


;;; Convert-More-Entry  --  Internal
;;;
;;;    Create the More-Entry function for the Optional-Dispatch Res.
;;; Entry-Vars and Entry-Vals describe the fixed arguments.  Rest is the var
;;; for any Rest arg.  Keys is a list of the keyword arg vars.
;;;
;;;    The most interesting thing that we do is parse keywords.  We create a
;;; bunch of temporary variables to hold the result of the parse, and then loop
;;; over the supplied arguments, setting the appropriate temps for the supplied
;;; keyword.  Note that it is significant that we iterate over the keywords in
;;; reverse order --- this implements the CL requirement that (when a keyword
;;; appears more than once) the first value is used.
;;;
;;;    If there is no supplied-p var, then we initialize the temp to the
;;; default and just pass the temp into the main entry.  Since non-constant
;;; keyword args are forcibly given a supplied-p var, we know that the default
;;; is constant, and thus safe to evaluate out of order.
;;;
;;;    If there is a supplied-p var, then we create temps for both the value
;;; and the supplied-p, and pass them into the main entry, letting it worry
;;; about defaulting.
;;;
;;;    We deal with :allow-other-keys by delaying unknown keyword errors until
;;; we have scanned all the keywords.
;;;
;;;    When converting the function, we bind *lexical-environment* to change
;;; the compilation policy over to the interface policy, so that keyword args
;;; will be checked even when type checking isn't on in general.
;;;
(defun convert-more-entry (res entry-vars entry-vals rest morep keys)
  (declare (type optional-dispatch res) (list entry-vars entry-vals keys))
  (collect ((arg-vars)
	    (arg-vals (reverse entry-vals))
	    (temps)
	    (body))
    
    (dolist (var (reverse entry-vars))
      (arg-vars (make-lambda-var
		 :name (leaf-name var)
		 :type (leaf-type var)
		 :where-from (leaf-where-from var))))

    (let* ((n-context (gensym "N-CONTEXT-"))
	   (context-temp (make-lambda-var :name n-context))
	   (n-count (gensym "N-COUNT-"))
	   (count-temp (make-lambda-var :name n-count
					:type (specifier-type 'index)))
	   (*lexical-environment*
	    (make-lexenv :cookie
			 (make-interface-cookie *lexical-environment*))))
	    
      (arg-vars context-temp count-temp)

      (when rest
	(arg-vals `(%listify-rest-args ,n-context ,n-count
				       ,(leaf-dynamic-extent rest))))
      (when morep
	(arg-vals n-context)
	(arg-vals n-count))

      (when (optional-dispatch-keyp res)
	(let ((n-index (gensym "N-INDEX-"))
	      (n-key (gensym "N-KEY-"))
	      (n-value-temp (gensym "N-VALUE-TEMP"))
	      (n-allowp (gensym "N-ALLOWP-"))
	      (n-losep (gensym "N-LOSEP-"))
	      (allowp (or (optional-dispatch-allowp res)
			  (policy nil (zerop safety)))))
	  
	  (temps `(,n-index (1- ,n-count)) n-key n-value-temp)
	  (body `(declare (fixnum ,n-index) (ignorable ,n-key ,n-value-temp)))

	  (collect ((tests))
	    (dolist (key keys)
	      (let* ((info (lambda-var-arg-info key))
		     (default (arg-info-default info))
		     (keyword (arg-info-keyword info))
		     (supplied-p (arg-info-supplied-p info))
		     (n-value (gensym "N-VALUE-")))
		(temps `(,n-value ,default))
		(cond (supplied-p
		       (let ((n-supplied (gensym "N-SUPPLIED-")))
			 (temps n-supplied)
			 (arg-vals n-value n-supplied)
			 (tests `((eq ,n-key ',keyword)
				  (setq ,n-supplied t)
				  (setq ,n-value ,n-value-temp)))))
		      (t
		       (arg-vals n-value)
		       (tests `((eq ,n-key ',keyword)
				(setq ,n-value ,n-value-temp)))))))

	    (unless allowp
	      (temps n-allowp n-losep)
	      (tests `((eq ,n-key :allow-other-keys)
		       (setq ,n-allowp ,n-value-temp)))
	      (tests `(t
		       (setq ,n-losep ,n-index))))

	    (body
	     `(when (oddp ,n-count)
		(%odd-keyword-arguments-error)))

	    (body
	     `(locally
		(declare (optimize (safety 0)))
		(loop
		  (when (minusp ,n-index) (return))
		  (setf ,n-value-temp (%more-arg ,n-context ,n-index))
		  (decf ,n-index)
		  (setq ,n-key (%more-arg ,n-context ,n-index))
		  (cond ,@(tests))
		  (decf ,n-index))))

	    (unless allowp
	      (body `(when (and ,n-losep (not ,n-allowp))
		       (%unknown-keyword-argument-error
			(%more-arg ,n-context ,n-losep))))))))
      
      (let ((ep (ir1-convert-lambda-body
		 `((let ,(temps)
		     ,@(body)
		     (%funcall ,(optional-dispatch-main-entry res)
			       . ,(arg-vals))))
		 (arg-vars))))
	(setf (optional-dispatch-more-entry res) ep))))

  (undefined-value))


;;; IR1-Convert-More  --  Internal
;;;
;;;    Called by IR1-Convert-Hairy-Args when we run into a rest or keyword arg.
;;; The arguments are similar to that function, but we split off any rest arg
;;; and pass it in separately.  Rest is the rest arg var, or NIL if there is no
;;; rest arg.  Keys is a list of the keyword argument vars.
;;;
;;;    When there are keyword arguments, we introduce temporary gensym
;;; variables to hold the values while keyword defaulting is in progress to get
;;; the required sequential binding semantics.
;;;
;;;    This gets interesting mainly when there are keyword arguments with
;;; supplied-p vars or non-constant defaults.  In either case, pass in a
;;; supplied-p var.  If the default is non-constant, we introduce an IF in the
;;; main entry that tests the supplied-p var and decides whether to evaluate
;;; the default or not.  In this case, the real incoming value is NIL, so we
;;; must union NULL with the declared type when computing the type for the main
;;; entry's argument.
;;;
(defun ir1-convert-more (res default-vars default-vals entry-vars entry-vals
			     rest more-context more-count keys supplied-p-p
			     body aux-vars aux-vals cont)
  (declare (type optional-dispatch res)
	   (list default-vars default-vals entry-vars entry-vals keys body
		 aux-vars aux-vals)
	   (type (or continuation null) cont))
  (collect ((main-vars (reverse default-vars))
	    (main-vals default-vals cons)
	    (bind-vars)
	    (bind-vals))
    (when rest
      (main-vars rest)
      (main-vals '()))
    (when more-context
      (main-vars more-context)
      (main-vals nil)
      (main-vars more-count)
      (main-vals 0))

    (dolist (key keys)
      (let* ((info (lambda-var-arg-info key))
	     (default (arg-info-default info))
	     (hairy-default (not (constantp default)))
	     (supplied-p (arg-info-supplied-p info))
	     (n-val (with-standard-io-syntax
                      (make-symbol (format nil "~A-DEFAULTING-TEMP"
                                           (leaf-name key)))))
	     (key-type (leaf-type key))
	     (val-temp (make-lambda-var
			:name n-val
			:type (if hairy-default
				  (type-union key-type (specifier-type 'null))
				  key-type))))
	(main-vars val-temp)
	(bind-vars key)
	(cond ((or hairy-default supplied-p)
	       (let* ((n-supplied (gensym))
		      (supplied-temp (make-lambda-var :name n-supplied)))
		 (unless supplied-p
		   (setf (arg-info-supplied-p info) supplied-temp))
		 (when hairy-default
		   (setf (arg-info-default info) nil))
		 (main-vars supplied-temp)
		 (cond (hairy-default
			(main-vals nil nil)
			(bind-vals `(if ,n-supplied ,n-val ,default)))
		       (t
			(main-vals default nil)
			(bind-vals n-val)))
		 (when supplied-p
		   (bind-vars supplied-p)
		   (bind-vals n-supplied))))
	      (t
	       (main-vals (arg-info-default info))
	       (bind-vals n-val)))))

    (let* ((main-entry (ir1-convert-lambda-body body (main-vars)
						(append (bind-vars) aux-vars)
						(append (bind-vals) aux-vals)
						t
						cont))
	   (last-entry (convert-optional-entry main-entry default-vars
					       (main-vals) ())))
      (setf (optional-dispatch-main-entry res) main-entry)
      (convert-more-entry res entry-vars entry-vals rest more-context keys)

      (push (if supplied-p-p
		(convert-optional-entry last-entry entry-vars entry-vals ())
		last-entry)
	    (optional-dispatch-entry-points res))
      last-entry)))


;;; IR1-Convert-Hairy-Args  --  Internal
;;;
;;;    This function generates the entry point functions for the
;;; optional-dispatch Res.  We accomplish this by recursion on the list of
;;; arguments, analyzing the arglist on the way down and generating entry
;;; points on the way up.
;;;
;;;    Default-Vars is a reversed list of all the argument vars processed so
;;; far, including supplied-p vars.  Default-Vals is a list of the names of the
;;; Default-Vars.
;;;
;;;    Entry-Vars is a reversed list of processed argument vars, excluding
;;; supplied-p vars.  Entry-Vals is a list things that can be evaluated to get
;;; the values for all the vars from the Entry-Vars.  It has the var name for
;;; each required or optional arg, and has T for each supplied-p arg.
;;;
;;;    Vars is a list of the Lambda-Var structures for arguments that haven't
;;; been processed yet.  Supplied-p-p is true if a supplied-p argument has
;;; already been processed; only in this case are the Default-XXX and Entry-XXX
;;; different.
;;;
;;;    The result at each point is a lambda which should be called by the above
;;; level to default the remaining arguments and evaluate the body.  We cause
;;; the body to be evaluated by converting it and returning it as the result
;;; when the recursion bottoms out.
;;;
;;;    Each level in the recursion also adds its entry point function to the
;;; result Optional-Dispatch.  For most arguments, the defaulting function and
;;; the entry point function will be the same, but when supplied-p args are
;;; present they may be different.
;;;
;;;     When we run into a rest or keyword arg, we punt out to
;;; IR1-Convert-More, which finishes for us in this case.
;;;
(defun ir1-convert-hairy-args (res default-vars default-vals
				   entry-vars entry-vals
				   vars supplied-p-p body aux-vars
				   aux-vals cont)
  (declare (type optional-dispatch res)
	   (list default-vars default-vals entry-vars entry-vals vars body
		 aux-vars aux-vals)
	   (type (or continuation null) cont))
  (cond ((not vars)
	 (if (optional-dispatch-keyp res)
	     ;;
	     ;; Handle &key with no keys...
	     (ir1-convert-more res default-vars default-vals
			       entry-vars entry-vals
			       nil nil nil vars supplied-p-p body aux-vars
			       aux-vals cont)
	     (let ((fun (ir1-convert-lambda-body body (reverse default-vars)
						 aux-vars aux-vals t cont)))
	       (setf (optional-dispatch-main-entry res) fun)
	       (push (if supplied-p-p
			 (convert-optional-entry fun entry-vars entry-vals ())
			 fun)
		     (optional-dispatch-entry-points res))
	       fun)))
	((not (lambda-var-arg-info (first vars)))
	 (let* ((arg (first vars))
		(nvars (cons arg default-vars))
		(nvals (cons (leaf-name arg) default-vals)))
	   (ir1-convert-hairy-args res nvars nvals nvars nvals
				   (rest vars) nil body aux-vars aux-vals
				   cont)))
	(t
	 (let* ((arg (first vars))
		(info (lambda-var-arg-info arg))
		(kind (arg-info-kind info)))
	   (ecase kind
	     (:optional
	      (let ((ep (generate-optional-default-entry
			 res default-vars default-vals
			 entry-vars entry-vals vars supplied-p-p body
			 aux-vars aux-vals cont)))
		(push (if supplied-p-p
			  (convert-optional-entry ep entry-vars entry-vals ())
			  ep)
		      (optional-dispatch-entry-points res))
		ep))
	     (:rest
	      (ir1-convert-more res default-vars default-vals
				entry-vars entry-vals
				arg nil nil (rest vars) supplied-p-p body
				aux-vars aux-vals cont))
	     (:more-context
	      (ir1-convert-more res default-vars default-vals
				entry-vars entry-vals
				nil arg (second vars) (cddr vars) supplied-p-p
				body aux-vars aux-vals cont))
	     (:keyword
	      (ir1-convert-more res default-vars default-vals
				entry-vars entry-vals
				nil nil nil vars supplied-p-p body aux-vars
				aux-vals cont)))))))


;;; IR1-Convert-Hairy-Lambda  --  Internal
;;;
;;;     This function deals with the case where we have to make an
;;; Optional-Dispatch to represent a lambda.  We cons up the result and call
;;; IR1-Convert-Hairy-Args to do the work.  When it is done, we figure out the
;;; min-args and max-args. 
;;;
(defun ir1-convert-hairy-lambda (body vars keyp allowp aux-vars aux-vals cont)
  (declare (list body vars aux-vars aux-vals) (type continuation cont))
  (let ((res (make-optional-dispatch :arglist vars  :allowp allowp
				     :keyp keyp))
	(min (or (position-if #'lambda-var-arg-info vars) (length vars))))
    (push res (component-new-functions *current-component*))
    (ir1-convert-hairy-args res () () () () vars nil body aux-vars aux-vals
			    cont)
    (setf (optional-dispatch-min-args res) min)
    (setf (optional-dispatch-max-args res)
	  (+ (1- (length (optional-dispatch-entry-points res))) min))

    (flet ((frob (ep)
	     (when ep
	       (setf (functional-kind ep) :optional)
	       (setf (leaf-ever-used ep) t)
	       (setf (lambda-optional-dispatch ep) res))))
      (dolist (ep (optional-dispatch-entry-points res)) (frob ep))
      (frob (optional-dispatch-more-entry res))
      (frob (optional-dispatch-main-entry res)))
      
    res))

    

(declaim (end-block))

;;;; Control special forms:

(def-ir1-translator progn ((&rest forms) start cont)
  "Progn Form*
  Evaluates each Form in order, returing the values of the last form.  With no
  forms, returns NIL."
  (ir1-convert-progn-body start cont forms))

(def-ir1-translator if ((test then &optional else) start cont)
  "If Predicate Then [Else]
  If Predicate evaluates to non-null, evaluate Then and returns its values,
  otherwise evaluate Else and return its values.  Else defaults to NIL."
  (let* ((pred (make-continuation))
	 (then-cont (make-continuation))
	 (then-block (continuation-starts-block then-cont))
	 (else-cont (make-continuation))
	 (else-block (continuation-starts-block else-cont))
	 (dummy-cont (make-continuation))
	 (node (make-if :test pred
			:consequent then-block  :alternative else-block)))
    (setf (continuation-dest pred) node)
    (ir1-convert start pred test)
    (prev-link node pred)
    (use-continuation node dummy-cont)
    
    (let ((start-block (continuation-block pred)))
      (setf (block-last start-block) node)
      (continuation-starts-block cont)
      
      (link-blocks start-block then-block)
      (link-blocks start-block else-block)
      
      (ir1-convert then-cont cont then)
      (ir1-convert else-cont cont else))))


;;;; Block and Tagbody:
;;;
;;;    We make an Entry node to mark the start and a :Entry cleanup to
;;; mark its extent.  When doing Go or Return-From, we emit an Exit node.
;;; 

;;; Block IR1 convert  --  Internal
;;;
;;;    Make a :entry cleanup and emit an Entry node, then convert the body in
;;; the modified environment.  We make Cont start a block now, since if it was
;;; done later, the block would be in the wrong environment.
;;;
(def-ir1-translator block ((name &rest forms) start cont)
  "Block Name Form*
  Evaluate the Forms as a PROGN.  Within the lexical scope of the body,
  (RETURN-FROM Name Value-Form) can be used to exit the form, returning the
  result of Value-Form."
  (unless (symbolp name)
    (compiler-error "Block name is not a symbol: ~S." name))
  (continuation-starts-block cont)
  (let* ((dummy (make-continuation))
	 (entry (make-entry))
	 (cleanup (make-cleanup :kind :block  :mess-up entry)))
    (push entry (lambda-entries (lexenv-lambda *lexical-environment*)))
    (setf (entry-cleanup entry) cleanup)
    (prev-link entry start)
    (use-continuation entry dummy)
    (let* ((cont-ref (make-cont-ref :cont cont))
	   (*lexical-environment*
	    (make-lexenv :blocks (list (cons name (list entry cont-ref)))
			 :cleanup cleanup)))
      (push cont-ref (continuation-refs cont))
      (ir1-convert-progn-body dummy cont forms))))

;;; We make Cont start a block just so that it will have a block assigned.
;;; People assume that when they pass a continuation into IR1-Convert as Cont,
;;; it will have a block when it is done.
;;;
(def-ir1-translator return-from ((name &optional value)
				 start cont)
  "Return-From Block-Name Value-Form
  Evaluate the Value-Form, returning its values from the lexically enclosing
  BLOCK Block-Name.  This is constrained to be used only within the dynamic
  extent of the BLOCK."
  (continuation-starts-block cont)
  (let* ((found (or (lexenv-find name blocks)
		    (compiler-error "Return for unknown block: ~S." name)))
	 (value-cont (make-continuation))
	 (entry (first found))
	 (exit (make-exit :entry entry  :value value-cont)))
    (push exit (entry-exits entry))
    (setf (continuation-dest value-cont) exit)
    (ir1-convert start value-cont value)
    (prev-link exit value-cont)
    (note-dfo-dependency start entry)
    (use-continuation exit (cont-ref-cont (second found)))))


;;; Parse-Tagbody  --  Internal
;;;
;;;    Return a list of the segments of a tagbody.  Each segment looks like
;;; (<tag> <form>* (go <next tag>)).  That is, we break up the tagbody into
;;; segments of non-tag statements, and explicitly represent the drop-through
;;; with a GO.  The first segment has a dummy NIL tag, since it represents code
;;; before the first tag.  The last segment (which may also be the first
;;; segment) ends in NIL rather than a GO.
;;;
(defun parse-tagbody (body)
  (declare (list body))
  (collect ((segments))
    (let ((current (cons nil body)))
      (loop
	(let ((tag-pos (position-if-not #'listp current :start 1)))
	  (unless tag-pos
	    (segments `(,@current nil))
	    (return))
	  (let ((tag (elt current tag-pos)))
	    (when (assoc tag (segments))
	      (compiler-error "Repeated tagbody tag: ~S." tag))
	    (unless (or (symbolp tag) (integerp tag))
	      (compiler-error "Illegal tagbody statement: ~S." tag))	      
	    (segments `(,@(subseq current 0 tag-pos) (go ,tag))))
	  (setq current (nthcdr tag-pos current)))))
    (segments)))
  

;;; Tagbody IR1 convert  --  Internal
;;;
;;;    Set up the cleanup, emitting the entry node.  Then make a block for each
;;; tag, building up the tag list for LEXENV-TAGS as we go.  Finally, convert
;;; each segment with the precomputed Start and Cont values.
;;;
(def-ir1-translator tagbody ((&rest statements) start cont)
  "Tagbody {Tag | Statement}*
  Define tags for used with GO.  The Statements are evaluated in order
  (skipping Tags) and NIL is returned.  If a statement contains a GO to a
  defined Tag within the lexical scope of the form, then control is transferred
  to the next statement following that tag.  A Tag must an integer or a
  symbol.  A statement must be a list.  Other objects are illegal within the
  body."
  (continuation-starts-block cont)
  (let* ((dummy (make-continuation))
	 (entry (make-entry))
	 (segments (parse-tagbody statements))
	 (cleanup (make-cleanup :kind :tagbody  :mess-up entry)))
    (push entry (lambda-entries (lexenv-lambda *lexical-environment*)))
    (setf (entry-cleanup entry) cleanup)
    (prev-link entry start)
    (use-continuation entry dummy)
    
    (collect ((tags)
	      (starts)
	      (conts))
      (starts dummy)
      (dolist (segment (rest segments))
	(let* ((tag-cont (make-continuation))
	       (tag-cont-ref (make-cont-ref :cont tag-cont)))
	  (conts tag-cont)
	  (starts tag-cont)
	  (continuation-starts-block tag-cont)
	  (tags (list (car segment) entry tag-cont-ref))))
      (conts cont)
      
      (let ((*lexical-environment*
	     (make-lexenv :cleanup cleanup :tags (tags))))
	(mapc #'(lambda (segment start cont)
		  (ir1-convert-progn-body start cont (rest segment)))
	      segments (starts) (conts))))))


;;; Go IR1 convert  --  Internal
;;;
;;;    Emit an Exit node without any value.
;;;
(def-ir1-translator go ((tag) start cont)
  "Go Tag
  Transfer control to the named Tag in the lexically enclosing TAGBODY.  This
  is constrained to be used only within the dynamic extent of the TAGBODY."
  (continuation-starts-block cont)
  (let* ((found (or (lexenv-find tag tags :test #'eql)
		    (compiler-error "Go to nonexistent tag: ~S." tag)))
	 (entry (first found))
	 (exit (make-exit :entry entry)))
    (push exit (entry-exits entry))
    (prev-link exit start)
    (note-dfo-dependency start entry)
    (use-continuation exit (cont-ref-cont (second found)))))


;;;; Translators for compiler-magic special forms:

(def-ir1-translator compiler-let ((bindings &rest body) start cont)
  (collect ((vars)
	    (values))
    (dolist (bind bindings)
      (typecase bind
	(symbol
	 (vars bind)
	 (values nil))
	(list
	 (unless (= (length bind) 2)
	   (compiler-error "Bad compiler-let binding spec: ~S." bind))
	 (vars (first bind))
	 (values (eval (second bind))))
	(t
	 (compiler-error "Bad compiler-let binding spec: ~S." bind))))
    (progv (vars) (values)
      (ir1-convert-progn-body start cont body))))


;;; DO-EVAL-WHEN-STUFF  --  Interface
;;;
;;;    Do stuff to do an EVAL-WHEN.  This is split off from the IR1 convert
;;; method so that it can be shared by the special-case top-level form
;;; processing code.  We play with the dynamic environment and eval stuff, then
;;; call Fun with a list of forms to be processed at load time.
;;;

(defun do-eval-when-stuff (situations body fun &optional toplevel-p)
  (when (or (not (listp situations))
	    (set-difference situations
			    '(compile load eval
			      :compile-toplevel :load-toplevel :execute)))
    (compiler-error "Bad Eval-When situation list: ~S." situations))

  (if toplevel-p
      ;; Can only get here from compile-file
      (progn
	(when (intersection '(compile :compile-toplevel) situations)
	  (eval `(progn ,@body)))
	;; Maybe generate code for load-time or run-time eval
	(if (or (intersection '(:load-toplevel load) situations)
		(and *converting-for-interpreter*
		     (intersection '(:execute eval) situations)))
	    (funcall fun body)
	    (funcall fun '(nil))))
      ;; Not toplevel, only :execute counts.
      (if (intersection '(eval :execute) situations)
	  (funcall fun body)
	  (funcall fun '(nil)))))
  
(def-ir1-translator eval-when ((situations &rest body) start cont)
  "EVAL-WHEN (Situation*) Form*
  Evaluate the Forms in the specified Situations, any of :COMPILE-TOPLEVEL,
  :LOAD-TOPLEVEL, :EXECUTE."
  (do-eval-when-stuff situations body
		      #'(lambda (forms)
			  (ir1-convert-progn-body start cont forms))))


(defun make-macrolet-environment (lexenv)
  (flet ((local-function-p (fun)
	   (functional-p (cdr fun)))
         (local-variable-p (var)
	   ;; ??? What is that CT-A-VAL structure that's mentioned in the
	   ;; description of the VARIABLES slot of LEXENV structure?
	   ;; Maybe this stuff has to be removed, too.
	   (leaf-p (cdr var))))
    (let ((env (copy-lexenv lexenv)))
      ;; CLHS says in the text of its MACROLET description that
      ;; consequences are undefined if a local macro definition refers
      ;; to local variable or function bindings.  A later example
      ;; clearly implies these are not accessible.  SBCL apparently
      ;; follows the example in CLHS.  Let's do the same.
      (setf (lexenv-functions env)
	    (remove-if #'local-function-p (lexenv-functions env)))
      (setf (lexenv-variables env)
	    (remove-if #'local-variable-p (lexenv-variables env)))
      (setf (lexenv-blocks env) nil)
      (setf (lexenv-tags env) nil)
      env)))

;;; DO-MACROLET-STUFF  --  Interface
;;;
;;;    Like DO-EVAL-WHEN-STUFF, only do a macrolet.  Fun is not passed any
;;; arguments.
;;;
(defun do-macrolet-stuff (definitions fun &optional decls (cont (make-continuation)))
  (declare (list definitions) (type function fun))
  (let ((whole (gensym))
	(environment (gensym)))
    (collect ((new-fenv))
      (dolist (def definitions)
	(let* ((name (first def))
	       (*current-function-names* (list name))
	       (arglist (second def))
	       (body (cddr def)))
	  (multiple-value-bind
	      (body local-decs)
	      (lisp::parse-defmacro arglist whole body name 'macrolet
				    :environment environment)
	    (unless (symbolp name)
	      (compiler-error "Macro name ~S is not a symbol." name))
	    (unless (listp arglist)
	      (compiler-error "Local macro ~S has argument list that is not a list: ~S."
			      name arglist))
	    (when (< (length def) 3)
	      (compiler-error
	       "Local macro ~S is too short to be a legal definition." name))
	    (new-fenv `(,(first def) macro .
			,(eval:internal-eval
			  `(lambda (,whole ,environment)
			     ,@local-decs
			     (block ,name ,body))
			  t
			  (make-macrolet-environment *lexical-environment*)))))))

      (let* ((*lexical-environment* (make-lexenv :functions (new-fenv)))
	     (*lexical-environment* (process-declarations decls nil (new-fenv) cont)))
	(funcall fun))))

  (undefined-value))


(def-ir1-translator macrolet ((definitions &parse-body (body decls)) start cont)
  "MACROLET ({(Name Lambda-List Form*)}*) Body-Form*
  Evaluate the Body-Forms in an environment with the specified local macros
  defined.  Name is the local macro name, Lambda-List is the DEFMACRO style
  destructuring lambda list, and the Forms evaluate to the expansion."
  (do-macrolet-stuff definitions
		     #'(lambda ()
			 (ir1-convert-progn-body start cont body))
		     decls
		     cont))


;;; COMPILER-OPTION-BIND
;;; 
(def-ir1-translator compiler-option-bind ((bindings &body body) start cont)
  "Compiler-Option-Bind ({(Name Value-Form)}*) Body-Form*
   Establish the specified compiler options for the (lexical) duration of
   the body.  The Value-Forms are evaluated at compile time."
  (let ((*lexical-environment*
	 (make-lexenv :options
		      (mapcar #'(lambda (binding)
				  (unless (and (listp binding)
					       (cdr binding)
					       (listp (cdr binding))
					       (null (cddr binding)))
				    (compiler-error "Bogus binding for ~
						     COMPILER-OPTION-BIND: ~S"
						    binding))
				  (cons (car binding)
					(eval (cadr binding))))
			      bindings))))
    (ir1-convert-progn-body start cont body)))


;;;; %Primitive:
;;;
;;;    Uses of %primitive are either expanded into Lisp code or turned into a
;;; funny function.
;;;

;;; Eval-Info-Args  --  Internal
;;;
;;;    Carefully evaluate a list of forms, returning a list of the results.
;;;
(defun eval-info-args (args)
  (declare (list args))
  (handler-case (mapcar #'eval args)
    (error (condition)
      (compiler-error "Lisp error during evaluation of info args:~%~A"
		      condition))))

;;; A hashtable that translates from primitive names to translation functions.
;;;
(defvar *primitive-translators* (make-hash-table :test #'eq))

;;; IR1-Convert-%Primitive  --  Internal
;;;
;;;    If there is a primitive translator, then we expand the call.  Otherwise,
;;; we convert to the %%Primitive funny function.  The first argument is the
;;; template, the second is a list of the results of any codegen-info args, and
;;; the remaining arguments are the runtime arguments.
;;;
;;;    We do a bunch of error checking now so that we don't bomb out with a
;;; fatal error during IR2 conversion.
;;;
(def-ir1-translator system:%primitive ((&whole form name &rest args)
				       start cont)
  
  (unless (symbolp name)
    (compiler-error "%Primitive name is not a symbol: ~S." name))

  (let* ((name (intern (symbol-name name)
		       (or (find-package "OLD-C")
			   (find-package "C"))))
	 (translator (gethash name *primitive-translators*)))
    (if translator
	(ir1-convert start cont (funcall translator (cdr form)))
	(let* ((template (or (gethash name (backend-template-names *backend*))
			     (compiler-error "Undefined primitive name: ~A."
					     name)))
	       (required (length (template-arg-types template)))
	       (info (template-info-arg-count template))
	       (min (+ required info))
	       (nargs (length args)))
	  (if (template-more-args-type template)
	      (when (< nargs min)
		(compiler-error "Primitive called with ~R argument~:P, ~
	    		         but wants at least ~R."
				nargs min))
	      (unless (= nargs min)
		(compiler-error "Primitive called with ~R argument~:P, ~
				 but wants exactly ~R."
				nargs min)))

	  (when (eq (template-result-types template) :conditional)
	    (compiler-error "%Primitive used with a conditional template."))

	  (when (template-more-results-type template)
	    (compiler-error
	     "%Primitive used with an unknown values template."))
	  
	  (ir1-convert start cont
		      `(%%primitive ',template
				    ',(eval-info-args
				       (subseq args required min))
				    ,@(subseq args 0 required)
				    ,@(subseq args min)))))))


;;;; Quote and Function:

(def-ir1-translator quote ((thing) start cont)
  "QUOTE Value
  Return Value without evaluating it."
  (reference-constant start cont thing))


(def-ir1-translator function ((thing) start cont)
  "FUNCTION Name
  Return the lexically apparent definition of the function Name.  Name may also
  be a lambda."
  (flet ((reference-it ()
	   (let ((fn (find-lexically-apparent-function
		      thing "as the argument to FUNCTION")))
	     (reference-leaf start cont fn))))
    (if (consp thing)
	(case (car thing)
	  ((lambda)			
	   ;; we set allow-debug-catch-tag here, needed for CLOS --jwr
	   (reference-leaf start cont (ir1-convert-lambda thing nil 'function t)))
	  ((instance-lambda)
	   ;; TODO: check if we should allow-debug-catch-tag here --jwr
	   (let ((res (ir1-convert-lambda `(lambda ,@(cdr thing))
					  nil ; name
					  'function)))
	     (setf (getf (functional-plist res) :fin-function) t)
	     (reference-leaf start cont res)))
	  (t
	   (if (valid-function-name-p thing)
	       (reference-it)
	       (compiler-error "Illegal function name: ~S" thing))))
	(reference-it))))


;;;; Funcall:
;;;
;;; FUNCALL is implemented on %FUNCALL, which can only call functions (not
;;; symbols).  It is directly used in some places where the call should always
;;; be open-coded even if FUNCALL is :NOTINLINE.
;;;

(deftransform funcall ((function &rest args) * * :when :both)
  (collect ((arg-names))
    (dolist (arg args)
      (declare (ignore arg))
      (arg-names (gensym "FUNCALL-ARG-")))
    `(lambda (function ,@(arg-names))
       (%funcall ,(if (csubtypep (continuation-type function)
				 (specifier-type 'function))
		      'function
		      '(if (functionp function)
			   function
			   (%coerce-to-function function)))
		 ,@(arg-names)))))

(def-ir1-translator %funcall ((function &rest args) start cont)
  (let ((fun-cont (make-continuation)))
    (ir1-convert start fun-cont function)
    (assert-continuation-type
     fun-cont (values-specifier-type '(values function &rest t)))
    (ir1-convert-combination-args fun-cont cont args)))

;;; This source transform exists to reduce the amount of work for the compiler.
;;; If the called function is a FUNCTION form, then convert directly to
;;; %FUNCALL, instead of waiting around for type inference.
;;;
(def-source-transform funcall (function &rest args)
  (if (and (consp function) (eq (car function) 'function))
      `(%funcall ,function ,@args)
      (values nil t)))

(deftransform %coerce-to-function ((thing) * * :when :both)
  (give-up "Might be a symbol, so must call FDEFINITION at runtime."))


;;;; Symbol macros:

(def-ir1-translator symbol-macrolet ((specs &parse-body (body decls))
				     start cont)
  "SYMBOL-MACROLET ({(Name Expansion)}*) Decl* Form*
  Define the Names as symbol macros with the given Expansions.  Within the
  body, references to a Name will effectively be replaced with the Expansion."
  (collect ((res))
    (dolist (spec specs)
      (unless (= (length spec) 2)
	(compiler-error "Malformed symbol macro binding: ~S." spec))
      (let ((name (first spec))
	    (def (second spec)))
	(unless (symbolp name)
	  (compiler-error "Symbol macro name is not a symbol: ~S." name))
	(let ((kind (info variable kind name)))
	  (when (member kind '(:special :constant))
	    (compiler-error "Attempt to bind a special or constant variable with SYMBOL-MACROLET: ~S." name)))
	(when (assoc name (res) :test #'eq)
	  (compiler-warning "Repeated name in SYMBOL-MACROLET: ~S." name))
	(res `(,name . (MACRO . ,def)))))

    (let* ((*lexical-environment* (make-lexenv :variables (res)))
	   (*lexical-environment* (process-declarations decls (res) nil cont)))
      (ir1-convert-progn-body start cont body))))


;;;; Proclaim:
;;;
;;;    Proclaim changes the global environment, so we must special-case it if
;;; we are to keep the information in the *FREE-xxx* variables up to date.
;;; When there is a var structure we disown it by replacing it with an updated
;;; copy.  Uses of the variable which were translated before the PROCLAIM will
;;; get the old version, while subsequent references will get the updated
;;; information. 


;;; Get-Old-Vars  --  Internal
;;;
;;;    Look up some symbols in *free-variables*, returning the var structures
;;; for any which exist.  If any of the names aren't symbols, we complain.
;;;
(defun get-old-vars (names)
  (declare (list names) (values list))
  (collect ((vars))
    (dolist (name names (vars))
      (unless (symbolp name)
	(compiler-error "Name is not a symbol: ~S." name))
      (let ((old (gethash name *free-variables*)))
	(when old (vars old))))))


;;; Process-Type-Proclamation  --  Internal
;;;
;;;    Replace each old var entry with one having the new type.  If the new
;;; type doesn't intersect with the old type, give a warning.  
;;;
;;;    We also check that the old type of each variable intersects with the new
;;; one, giving a warning if not.  This isn't as serious as conflicting local
;;; declarations, since we assume a redefinition semantics rather than an
;;; intersection semantics.
;;;
(defun process-type-proclamation (spec names)
  (declare (list names))
  (let ((type (specifier-type spec)))
    (unless (policy nil (= brevity 3))
      (dolist (name names)
	(let ((old-type (info variable type name)))
	  (unless (types-intersect type old-type)
	    (compiler-warning
	     "New proclaimed type ~S for ~S conflicts with old type ~S."
	     (type-specifier type) name (type-specifier old-type))))))

    (dolist (var (get-old-vars names))
      (let ((name (leaf-name var)))
	(setf (gethash name *free-variables*)
	      (etypecase var
		(global-var
		 (make-global-var :name name :type type :where-from :declared
				  :kind (global-var-kind var)))
		(constant
		 (make-constant :name name :type type :where-from :declared
				:value (constant-value var)))))))))



;;; Process-1-Ftype-Proclamation  --  Internal
;;;
;;; Update function type info cached in *free-functions*.  If:
;;; -- there is a GLOBAL-VAR, then just update the type and remove the name
;;;    from the list of undefined functions.  Someday we should check for
;;;    incompatible redeclaration.
;;; -- there is a FUNCTIONAL, then apply the type assertion to that function.
;;;    This will only happen during block compilation.
;;;
(defun process-1-ftype-proclamation (name type)
  (let ((var (gethash (define-function-name name) *free-functions*)))
    (etypecase var
      (null)
      (slot-accessor)
      (global-var
       (setf (gethash name *free-functions*)
	     (let ((kind (global-var-kind var)))
	       (if (defined-function-p var)
		   (make-defined-function
		    :name name :type type :where-from :declared :kind kind
		    :inlinep (defined-function-inlinep var)
		    :inline-expansion (defined-function-inline-expansion var)
		    :functional (defined-function-functional var))
		   (make-global-var :name name :type type :where-from :declared
				    :kind kind))))
       (when (defined-function-p var)
	 (let ((fun (defined-function-functional var)))
	   (when fun
	     (assert-definition-type fun type
				     :warning-function #'compiler-note
				     :where "this declaration")))))))
  (undefined-value))


;;; Process-Ftype-Proclamation  --  Internal
;;;
(defun process-ftype-proclamation (spec names)
  (declare (list names))
  (let ((type (specifier-type spec)))
    (unless (csubtypep type (specifier-type 'function))
      (compiler-error
       "Declared functional type is not a function type: ~S." spec))
    (dolist (name names)
      (process-1-ftype-proclamation name type))))


;;; PROCESS-INLINE-PROCLAMATION  --  Internal
;;;
;;;    Similar in effect to FTYPE, but change the :INLINEP.  Copying the
;;; global-var ensures that when we substitute a functional for a global var
;;; (i.e. for DEFUN) that we won't clobber any uses declared :NOTINLINE.
;;;
(defun process-inline-proclamation (kind funs)
  (dolist (name funs)
    (let ((var (gethash (define-function-name name) *free-functions*)))
      (etypecase var
	(null)
	(slot-accessor)
	(global-var
	 (setf (gethash name *free-functions*)
	       (make-new-inlinep var
				 (cdr (assoc kind inlinep-translations)))))))))

(defknown %declaim (list) void)

(def-ir1-translator %declaim ((what) start cont :kind :function)
  (if (constantp what)
      (let ((form (eval what)))
	(unless (consp form)
	  (compiler-error "Malformed PROCLAIM spec: ~S." form))

	(let ((identifier (first form))
	      (args (rest form))
	      (ignore nil))
	  (case identifier
	    (special
	     (dolist (old (get-old-vars (rest form)))
	       (let ((name (leaf-name old)))
		 (when (or (constant-p old)
			   (eq (global-var-kind old) :constant))
		   (compiler-error
		    "Attempt to proclaim constant ~S to be special." name))

		 (ecase (global-var-kind old)
		   (:special)
		   (:global
		    (setf (gethash name *free-variables*)
			  (make-global-var :name name :type (leaf-type old)
					   :where-from (leaf-where-from old)
					   :kind :special)))))))
	    (type
	     (when (endp args)
	       (compiler-error "Malformed TYPE proclamation: ~S." form))
	     (process-type-proclamation (first args) (rest args)))
	    (function
	     (when (endp args)
	       (compiler-error "Malformed FUNCTION proclamation: ~S." form))
	     (process-ftype-proclamation `(function . ,(rest args))
					 (list (first args))))
	    (ftype
	     (when (endp args)
	       (compiler-error "Malformed FTYPE proclamation: ~S." form))
	     (process-ftype-proclamation (first args) (rest args)))
	    ((inline notinline maybe-inline)
	     (process-inline-proclamation identifier args))
	    ;;
	    ;; No non-global state to be updated.
	    ((optimize optimize-interface declaration freeze-type
		       constant-function))
	    ;;
	    ;; Totally ignore these operations at non-top-level.
	    ((start-block end-block)
	     (setq ignore t))
	    (t
	     (cond ((member identifier type-specifier-symbols)
		    (process-type-proclamation identifier args))
		   ((info declaration recognized identifier)
		    (setq ignore t))
		   (t
		    (setq ignore t)
		    (compiler-warning "Unrecognized proclamation: ~S."
				      form)))))
	  
	  (unless ignore
	    (proclaim form))
	  (if ignore
	      (ir1-convert start cont nil)
	      (ir1-convert start cont `(proclaim ,what)))))
      (ir1-convert start cont `(proclaim ,what))))


;;; %Compiler-Defstruct IR1 Convert  --  Internal
;;;
;;;    This is a frob that DEFSTRUCT expands into to establish the compiler
;;; semantics.  The other code in the expansion and %%COMPILER-DEFSTRUCT do
;;; most of the work, we just clear all of the functions out of
;;; *FREE-FUNCTIONS* to keep things in synch.  %%COMPILER-DEFSTRUCT is also
;;; called at load-time.
;;;
(def-ir1-translator kernel:%compiler-defstruct
		    ((info) start cont :kind :function)
  (let* ((info (eval info)))
    (kernel:%%compiler-defstruct info)
    (dolist (slot (kernel:dd-slots info))
      (let ((fun (kernel:dsd-accessor slot)))
	(remhash fun *free-functions*)
	(unless (kernel:dsd-read-only slot)
	  (remhash `(setf ,fun) *free-functions*))))
    (remhash (kernel:dd-predicate info) *free-functions*)
    (remhash (kernel:dd-copier info) *free-functions*)
    (ir1-convert start cont `(kernel:%%compiler-defstruct ',info))))

;;; %COMPILER-ONLY-DEFSTRUCT  IR1 Convert  --  Internal
;;;
;;;    Don't actually compile anything, instead call the function now.  Use
;;; EVAL so this can be compiled... 
;;; 
(def-ir1-translator kernel:%compiler-only-defstruct
		    ((info inherits) start cont :kind :function)
  (eval `(kernel:%compiler-only-defstruct ,info ,inherits))
  (reference-constant start cont nil))
 

;;;; Let and Let*:
;;;
;;;    Let and Let* can't be implemented as macros due to the fact that
;;; any pervasive declarations also affect the evaluation of the arguments.

;;; Extract-Let-Variables  --  Internal
;;;
;;;    Given a list of binding specifiers in the style of Let, return:
;;;  1] The list of var structures for the variables bound.
;;;  2] The initial value form for each variable.
;;;
;;; The variable names are checked for legality and globally special variables
;;; are marked as such.  Context is the name of the form, for error reporting
;;; purposes.
;;;
(defun extract-let-variables (bindings context)
  (declare (list bindings) (symbol context) (values list list list))
  (collect ((vars)
	    (vals)
	    (names))
    (flet ((get-var (name)
	     (varify-lambda-arg name
				(if (eq context 'let*)
				    nil
				    (names)))))
      (dolist (spec bindings)
	(cond ((atom spec)
	       (let ((var (get-var spec)))
		 (vars var)
		 (names (cons spec var)) 
		 (vals nil)))
	      (t
	       (unless (<= 1 (length spec) 2)
		 (compiler-error "Malformed ~S binding spec: ~S."
				 context spec))
	       (let* ((name (first spec))
		      (var (get-var name)))
		 (vars var)
		 (names name)
		 (vals (second spec)))))))

    (values (vars) (vals) (names))))

(def-ir1-translator let ((bindings &parse-body (body decls))
			 start cont)
  "LET ({(Var [Value]) | Var}*) Declaration* Form*
  During evaluation of the Forms, Bind the Vars to the result of evaluating the
  Value forms.  The variables are bound in parallel after all of the Values are
  evaluated."
  (multiple-value-bind (vars values)
      (extract-let-variables bindings 'let)
    (let ((*lexical-environment* (process-declarations decls vars nil cont)))
      (if (dynamic-extent-allocation-p vars values)
	  (with-dynamic-extent (start cont nnext-cont :bind)
	    (let ((fun-cont (make-continuation))
		  (fun (ir1-convert-lambda-body body vars)))
	      (reference-leaf nnext-cont fun-cont fun)
	      (ir1-convert-combination-args fun-cont cont values)))
	  (let ((fun-cont (make-continuation))
		(fun (ir1-convert-lambda-body body vars)))
	    (reference-leaf start fun-cont fun)
	    (ir1-convert-combination-args fun-cont cont values))))))

(def-ir1-translator locally ((&parse-body (body decls))
                            start cont)
  "LOCALLY Declaration* Form*
   Sequentially evaluates a body of Form's in a lexical environment
   where the given Declaration's have effect."
  (let* ((*lexical-environment* (process-declarations decls nil nil cont)))
    (ir1-convert-progn-body start cont body)))

(def-ir1-translator let* ((bindings &parse-body (body decls))
			  start cont)
  "LET* ({(Var [Value]) | Var}*) Declaration* Form*
  Similar to LET, but the variables are bound sequentially, allowing each Value
  form to reference any of the previous Vars."
  (multiple-value-bind (vars values)
      (extract-let-variables bindings 'let*)
    (let ((*lexical-environment* (process-declarations decls vars nil cont)))
      (ir1-convert-dynamic-extent-bindings start cont body vars values nil))))


;;;; Flet and Labels:

;;; Extract-Flet-Variables  --  Internal
;;;
;;;    Given a list of local function specifications in the style of Flet,
;;; return lists of the function names and of the lambdas which are their
;;; definitions.
;;;
;;; The function names are checked for legality.  Context is the name of the
;;; form, for error reporting.
;;;
(defun extract-flet-variables (definitions context)
  (declare (list definitions) (symbol context) (values list list))
  (flet ((local-function-name (name)
	   (let ((base (list context name))
		 (current *current-function-names*))
	     (if current
		 (let ((entry (assoc name (cdr current) :test #'equal)))
		   (cond (entry
			  (nconc base (list (incf (cdr entry)))
				 (list (car current))))
			 (t
			  (push (cons name 0) (cdr current))
			  (nconc base (list (car current))))))
		 base))))
    (collect ((names) (defs))
       (dolist (def definitions)
	 (when (or (atom def) (< (length def) 2))
	   (compiler-error "Malformed ~S definition spec: ~S." context def))
	 (let* ((name (check-function-name (first def)))
		(block-name (nth-value 1 (valid-function-name-p (first def))))
		(local-name (local-function-name name)))
	   (names local-name)
	   (multiple-value-bind (body decls)
	       (system:parse-body (cddr def) *lexical-environment* t)
	     (defs `(lambda ,(second def)
		      ,@decls
		      (block ,block-name
			. ,body))))))
       (values (names) (defs)))))


(def-ir1-translator flet ((definitions &parse-body (body decls))
			  start cont)
  "FLET ({(Name Lambda-List Declaration* Form*)}*) Declaration* Body-Form*
  Evaluate the Body-Forms with some local function definitions.   The bindings
  do not enclose the definitions; any use of Name in the Forms will refer to
  the lexically apparent function definition in the enclosing environment."
  (multiple-value-bind (names defs)
      (extract-flet-variables definitions 'flet)
    (let* ((fvars (mapcar #'(lambda (n d)
			      ;; There is no point in allowing debug
			      ;; catch tag to be inserted here, since in
			      ;; CMUCL (as opposed to SBCL) flet
			      ;; function calls do not make it to the
			      ;; debugger anyway? --jwr
			      (ir1-convert-lambda d n 'flet))
			  names defs))
	   (*lexical-environment*
	    (make-lexenv :default (process-declarations decls nil fvars cont)
			 :functions (pairlis names fvars))))
      (ir1-convert-progn-body start cont body))))


;;; For Labels, we have to create dummy function vars and add them to the
;;; function namespace while converting the functions.  We then modify all the
;;; references to these leaves so that they point to the real functional
;;; leaves.  We also backpatch the FENV so that if the lexical environment is
;;; used for inline expansion we will get the right functions.
;;;
(def-ir1-translator labels ((definitions &parse-body (body decls)) start cont)
  "LABELS ({(Name Lambda-List Declaration* Form*)}*) Declaration* Body-Form*
  Evaluate the Body-Forms with some local function definitions.  The bindings
  enclose the new definitions, so the defined functions can call themselves or
  each other."
  (multiple-value-bind (names defs)
		       (extract-flet-variables definitions 'labels)
    (let* ((new-fenv (loop for name in names
		       collect (cons name (make-functional :name name))))
	   (real-funs 
	    (let ((*lexical-environment* (make-lexenv :functions new-fenv)))
	      (mapcar #'(lambda (n d)
			  ;; see comments in (def-ir1-translator flet)
			  ;; on why we don't set allow-debug-catch-tag
			  ;; to t here --jwr
			  (ir1-convert-lambda d n 'labels))
		      names defs))))

      (loop for real in real-funs and env in new-fenv do
	(let ((dum (cdr env)))
	  (substitute-leaf real dum)
	  (setf (cdr env) real)))

      (let ((*lexical-environment*
	     (make-lexenv
	      :default (process-declarations decls nil real-funs cont)
	      :functions (pairlis names real-funs))))
	(ir1-convert-progn-body start cont body)))))


;;;; THE

;;; DO-THE-STUFF  --  Internal
;;;
;;;    Do stuff to recognize a THE or VALUES declaration.  Cont is the
;;; continuation that the assertion applies to, Type is the type specifier and
;;; Lexenv is the current lexical environment.  Name is the name of the
;;; declaration we are doing, for use in error messages.
;;;
;;;    This is somewhat involved, since a type assertion may only be made on a
;;; continuation, not on a node.  We can't just set the continuation asserted
;;; type and let it go at that, since there may be parallel THE's for the same
;;; continuation, i.e.:
;;;     (if ...
;;;         (the foo ...)
;;;         (the bar ...))
;;;
;;; In this case, our representation can do no better than the union of these
;;; assertions.  And if there is a branch with no assertion, we have nothing at
;;; all.  We really need to recognize scoping, since we need to be able to
;;; discern between parallel assertions (which we union) and nested ones (which
;;; we intersect).
;;;
;;; We represent the scoping by throwing our innermost (intersected) assertion
;;; on Cont into the TYPE-RESTRICTIONS.  As we go down, we intersect our
;;; assertions together.  If Cont has no uses yet, we have not yet bottomed out
;;; on the first COND branch; in this case we optimistically assume that this
;;; type will be the one we end up with, and set the ASSERTED-TYPE to it.
;;; We can never get better than the type that we have the first time we bottom
;;; out.  Later THE's (or the absence thereof) can only weaken this result.
;;;
;;; We make this work by getting USE-CONTINUATION to do the unioning across
;;; COND branches.  We can't do it here, since we don't know how many branches
;;; there are going to be.
;;;
(defun do-the-stuff (ctype cont lexenv name)
  (declare (type ctype ctype) (type continuation cont) (type lexenv lexenv))
  (let* ((old-type (or (lexenv-find cont type-restrictions)
		       *wild-type*))
	 (intersects (values-types-intersect old-type ctype))
	 (int (values-type-intersection old-type ctype))
	 (new (if intersects int old-type)))
    (when (null (find-uses cont))
      (setf (continuation-asserted-type cont) new))
    (when (and (not intersects)
	       (not (policy nil (= brevity 3))))
      (compiler-warning
       "Type ~S in ~S declaration conflicts with enclosing assertion:~%   ~S"
       (type-specifier ctype) name (type-specifier old-type)))
    (make-lexenv :type-restrictions `((,cont . ,new))
		 :default lexenv)))


;;; THE IR1 Convert  --  Internal
;;;
;;; A THE declaration for a single value type is internally converted
;;; into (values &optional type &rest t) as this is the commonly
;;; expected behavior.
;;;
(def-ir1-translator the ((type value) start cont)
  "THE Type Form
  Assert that Form evaluates to the specified type (which may be a VALUES
  type.)"
  (let ((ctype (values-specifier-type type)))
    (if (values-type-p ctype)
	(let ((rest (values-type-rest ctype)))
	  (when (or (null rest)
		    (eq rest (specifier-type nil)))
	    (setq ctype
		  (make-values-type
		   :required (copy-list (values-type-required ctype))
		   :optional (values-type-optional ctype)
		   :rest (if (null rest) *universal-type* nil)))))
	(setq ctype (make-values-type :required (list ctype)
				      :rest *universal-type*)))
    (let ((*lexical-environment*
	   (do-the-stuff ctype cont *lexical-environment* 'the)))
      (ir1-convert start cont value))))

;;; Truly-The IR1 convert  --  Internal
;;;
;;;    Since the Continuation-Derived-Type is computed as the union of its
;;; uses's types, setting it won't work.  Instead we must intersect the type
;;; with the uses's Derived-Type.
;;;
(def-ir1-translator truly-the ((type value) start cont)
  "Truly-The Type Value
  Like the THE special form, except that it believes whatever you tell it.  It
  will never generate a type check, but will cause a warning if the compiler
  can prove the assertion is wrong."
  (declare (inline member))
  (let ((type (values-specifier-type type))
	(old (find-uses cont)))
    (ir1-convert start cont value)
    (do-uses (use cont)
      (unless (member use old :test #'eq)
	(derive-node-type use type)))))


;;;; Setq
;;;
;;;    If there is a definition in LEXENV-VARIABLES, just set that, otherwise
;;; look at the global information.  If the name is for a constant, then error
;;; out.

(def-ir1-translator setq ((&whole source &rest things) start cont)
  "SETQ {Var Value}*
  Set the variables to the values.  If more than one pair is supplied, the
  assignments are done sequentially.  If Var names a symbol macro, SETF the
  expansion."
  (let ((len (length things)))
    (when (oddp len)
      (compiler-error "Odd number of args to SETQ: ~S." source))
    (if (= len 2)
	(let* ((name (first things))
	       (leaf (or (lexenv-find name variables)
			 (find-free-variable name))))
	  (etypecase leaf
	    (leaf
	     (when (or (constant-p leaf)
		       (and (global-var-p leaf)
			    (eq (global-var-kind leaf) :constant)))
	       (compiler-error "Attempt to set constant ~S." name))
	     (when (lambda-var-p leaf)
	       (when (lambda-var-ignorep leaf)
		 (compiler-note "Setting an ignored variable: ~S." name))
	       (note-dfo-dependency start leaf))
	     (set-variable start cont leaf (second things)))
	    (cons
	     (assert (eq (car leaf) 'MACRO))
	     (ir1-convert start cont `(setf ,(cdr leaf) ,(second things))))
	    (heap-alien-info
	     (ir1-convert start cont
			  `(%set-heap-alien ',leaf ,(second things))))))
	(collect ((sets))
	  (do ((thing things (cddr thing)))
	      ((endp thing)
	       (ir1-convert-progn-body start cont (sets)))
	    (sets `(setq ,(first thing) ,(second thing))))))))


;;; Set-Variable  --  Internal
;;;
;;;    Kind of like Reference-Leaf, but we generate a Set node.  This
;;; should only need to be called in Setq.
;;;
(defun set-variable (start cont var value)
  (declare (type continuation start cont) (type basic-var var))
  (let ((dest (make-continuation))
	(opt-type (make-values-type :required (list (leaf-type var))
				    :rest *universal-type*)))
    (setf (continuation-asserted-type dest) opt-type)
    (ir1-convert start dest value)
    (let ((res (make-set :var var :value dest)))
      (setf (continuation-dest dest) res)
      (setf (leaf-ever-used var) t)
      (push res (basic-var-sets var))
      (prev-link res dest)
      (use-continuation res cont))))


;;;; Catch, Throw and Unwind-Protect:
;;;

;;; Throw  --  Public
;;;
;;;    Although throw could be a macro, it seems this would cause unnecessary
;;; confusion.  We turn THROW into a multiple-value-call of a magical function,
;;; since as as far as IR1 is concerned, it has no interesting properties other
;;; than receiving multiple-values.
;;;
(def-ir1-translator throw ((tag result) start cont)
  "Throw Tag Form
  Do a non-local exit, return the values of Form from the CATCH whose tag
  evaluates to the same thing as Tag."
  (ir1-convert start cont
	       `(multiple-value-call #'%throw ,tag ,result)))


;;; This is a special special form used to instantiate a cleanup as the current
;;; cleanup within the body.  Kind is a the kind of cleanup to make, and
;;; Mess-Up is a form that does the mess-up action.  We make the MESS-UP be the
;;; USE of the Mess-Up form's continuation, and introduce the cleanup into the
;;; lexical environment.  We back-patch the Entry-Cleanup for the current
;;; cleanup to be the new cleanup, since this inner cleanup is the interesting
;;; one.
;;;
(def-ir1-translator %within-cleanup ((kind mess-up &body body) start cont)
  (let ((dummy (make-continuation))
	(dummy2 (make-continuation)))
    (ir1-convert start dummy mess-up)
    (let* ((mess-node (continuation-use dummy))
	   (cleanup (make-cleanup :kind kind  :mess-up mess-node))
	   (old-cup (lexenv-cleanup *lexical-environment*))
	   (*lexical-environment* (make-lexenv :cleanup cleanup)))
      (setf (entry-cleanup (cleanup-mess-up old-cup)) cleanup)
      (ir1-convert dummy dummy2 '(%cleanup-point))
      (ir1-convert-progn-body dummy2 cont body))))


;;; This is a special special form that makes an "escape function" which
;;; returns unknown values from named block.  We convert the function, set its
;;; kind to :Escape, and then reference it.  The :Escape kind indicates that
;;; this function's purpose is to represent a non-local control transfer, and
;;; that it might not actually have to be compiled.
;;;
;;; Note that environment analysis replaces references to escape functions
;;; with references to the corresponding NLX-Info structure.
;;;
(def-ir1-translator %escape-function ((tag) start cont)
  (let ((fun (ir1-convert-lambda
	      `(lambda ()
		 (return-from ,tag (%unknown-values))))))
    (setf (functional-kind fun) :escape)
    (reference-leaf start cont fun)))


;;; Yet another special special form.  This one looks up a local function and
;;; smashes it to a :Cleanup function, as well as referencing it.
;;;
(def-ir1-translator %cleanup-function ((name) start cont)
  (let ((fun (lexenv-find-function name)))
    (assert (lambda-p fun))
    (setf (functional-kind fun) :cleanup)
    (reference-leaf start cont fun)))


;;; Catch  --  Public
;;;
;;;    Catch could be a macro, but it's somewhat tasteless to expand into
;;; implementation-dependent special forms.
;;;
;;;    We represent the possibility of the control transfer by making an
;;; "escape function" that does a lexical exit, and instantiate the cleanup
;;; using %within-cleanup.
;;;
(def-ir1-translator catch ((tag &body body) start cont)
  "Catch Tag Form*
  Evaluates Tag and instantiates it as a catcher while the body forms are
  evaluated in an implicit PROGN.  If a THROW is done to Tag within the dynamic
  scope of the body, then control will be transferred to the end of the body
  and the thrown values will be returned."
  (ir1-convert
   start cont
   (let ((exit-block (gensym)))
     `(block ,exit-block
	(%within-cleanup
	    :catch
	    (%catch (%escape-function ,exit-block) ,tag)
	  ,@body)))))


;;; Unwind-Protect  --  Public
;;;
;;;    Unwind-Protect is similar to Catch, but more hairy.  We make the cleanup
;;; forms into a local function so that they can be referenced both in the case
;;; where we are unwound and in any local exits.  We use %Cleanup-Function on
;;; this to indicate that reference by %Unwind-Protect isn't "real", and thus
;;; doesn't cause creation of an XEP.
;;;
(def-ir1-translator unwind-protect ((protected &body cleanup) start cont)
  "Unwind-Protect Protected Cleanup*
  Evaluate the form Protected, returning its values.  The cleanup forms are
  evaluated whenever the dynamic scope of the Protected form is exited (either
  due to normal completion or a non-local exit such as THROW)."
  (ir1-convert
   start cont
   (let ((cleanup-fun (gensym))
	 (drop-thru-tag (gensym))
	 (exit-tag (gensym))
	 (next (gensym))
	 (start (gensym))
	 (count (gensym)))
     `(flet ((,cleanup-fun () ,@cleanup nil))
	(block ,drop-thru-tag
	  (multiple-value-bind
	      (,next ,start ,count)
	      (block ,exit-tag
		(%within-cleanup
		    :unwind-protect
		    (%unwind-protect (%escape-function ,exit-tag)
				     (%cleanup-function ,cleanup-fun))
		  (return-from ,drop-thru-tag ,protected)))
	    (,cleanup-fun)
	    (%continue-unwind ,next ,start ,count)))))))


;;;; MV stuff.

;;; If there are arguments, multiple-value-call turns into an MV-Combination.
;;;
;;; If there are no arguments, then we convert to a normal combination,
;;; ensuring that a MV-Combination always has at least one argument.  This can
;;; be regarded as an optimization, but it is more important for simplifying
;;; compilation of MV-Combinations.
;;;
(def-ir1-translator multiple-value-call ((fun &rest args) start cont)
  "MULTIPLE-VALUE-CALL Function Values-Form*
  Call Function, passing all the values of each Values-Form as arguments,
  values from the first Values-Form making up the first argument, etc."
  (let* ((fun-cont (make-continuation))
	 (node (if args
		   (make-mv-combination fun-cont)
		   (make-combination fun-cont))))
    (ir1-convert start fun-cont
		 (if (and (consp fun) (eq (car fun) 'function))
		     fun
		     (once-only ((fun fun))
		       `(if (functionp ,fun)
			    ,fun
			    (%coerce-to-function ,fun)))))
    (setf (continuation-dest fun-cont) node)
    (assert-continuation-type
     fun-cont (values-specifier-type '(values function &rest t)))
    (collect ((arg-conts))
      (let ((this-start fun-cont))
	(dolist (arg args)
	  (let ((this-cont (make-continuation node)))
	    (ir1-convert this-start this-cont arg)
	    (setq this-start this-cont)
	    (arg-conts this-cont)))
	(prev-link node this-start)
	(use-continuation node cont)
	(setf (basic-combination-args node) (arg-conts))))))


;;; IR1 convert Multiple-Value-Prog1  --  Internal
;;;
;;; Multiple-Value-Prog1 is represented implicitly in IR1 by having a the
;;; result code use result continuation (CONT), but transfer control to the
;;; evaluation of the body.  In other words, the result continuation isn't
;;; Immediately-Used-P by the nodes that compute the result.
;;;
;;; In order to get the control flow right, we convert the result with a dummy
;;; result continuation, then convert all the uses of the dummy to be uses of
;;; CONT.  If a use is an Exit, then we also substitute CONT for the dummy in
;;; the corresponding Entry node so that they are consistent.  Note that this
;;; doesn't amount to changing the exit target, since the control destination
;;; of an exit is determined by the block successor; we are just indicating the
;;; continuation that the result is delivered to.
;;;
;;; We then convert the body, using another dummy continuation in its own block
;;; as the result.  After we are done converting the body, we move all
;;; predecessors of the dummy end block to CONT's block.
;;;
;;; Note that we both exploit and maintain the invariant that the CONT to an
;;; IR1 convert method either has no block or starts the block that control
;;; should transfer to after completion for the form.  Nested MV-Prog1's work
;;; because during conversion of the result form, we use dummy continuation
;;; whose block is the true control destination.
;;;
(def-ir1-translator multiple-value-prog1 ((result &rest forms) start cont)
  "MULTIPLE-VALUE-PROG1 Values-Form Form*
  Evaluate Values-Form and then the Forms, but return all the values of
  Values-Form." 
  (continuation-starts-block cont)
  (let* ((dummy-result (make-continuation))
	 (dummy-start (make-continuation))
	 (cont-block (continuation-block cont)))
    (continuation-starts-block dummy-start)
    (ir1-convert start dummy-start result)

    (substitute-continuation-uses cont dummy-start)

    (continuation-starts-block dummy-result)
    (ir1-convert-progn-body dummy-start dummy-result forms)
    (let ((end-block (continuation-block dummy-result)))
      (dolist (pred (block-pred end-block))
	(unlink-blocks pred end-block)
	(link-blocks pred cont-block))
      (assert (not (continuation-dest dummy-result)))
      (delete-continuation dummy-result)
      (remove-from-dfo end-block))))



;;;; Interface to defining macros:
;;;
;;;    DEFMACRO, DEFUN and DEFCONSTANT expand into calls to %DEFxxx functions
;;; so that we get a chance to see what is going on.  We define IR1 translators
;;; for these functions which look at the definition and then generate a call
;;; to the %%DEFxxx function. 
;;;


;;; REVERT-SOURCE-PATH  --  Internal
;;;
;;;    Return a new source path with any stuff intervening between the current
;;; path and the first form beginning with Name stripped off.  This is used to
;;; hide the guts of DEFmumble macros to prevent annoying error messages.
;;; 
(defun revert-source-path (name)
  (do ((path *current-path* (cdr path)))
      ((null path) *current-path*)
    (let ((first (first path)))
      (when (or (eq first name)
		(eq first 'original-source-start))
	(return path)))))


;;; Warn about incompatible or illegal definitions and add the macro to the
;;; compiler environment.  This is split up into compile-time processing and
;;; compilation for load-time effect so that macro definitions, which are split
;;; into :compile-toplevel and (:load-toplevel :execute) parts by the DEFMACRO
;;; macro,  will behave properly when not at top level (i.e., no compile time
;;; effect). 
;;;
;;; Someday we could check for macro arguments being incompatibly redefined.
;;; Doing this right will involve finding the old macro lambda-list and
;;; comparing it with the new one.
;;;
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
      (setf (leaf-name fun) (list :macro name))
      (setf (functional-arg-documentation fun) (eval lambda-list))

      (ir1-convert start cont `(%%defmacro ',name ,fun ,doc)))

    (when *compile-print*
      (compiler-mumble "~&; Converted ~S.~%" name))))


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
	   (fun (ir1-convert-lambda def 
				    name 
				    'define-compiler-macro)))
      (setf (leaf-name fun) (list :compiler-macro name))
      (setf (functional-arg-documentation fun) (eval lambda-list))

      (ir1-convert start cont `(%%define-compiler-macro ',name ,fun ,doc)))

    (when *compile-print*
      (compiler-mumble "~&; Converted ~S.~%" name))))


;;; Update the global environment to correspond to the new definition.
;;;
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
  (setf (info variable documentation name) doc)
  (remhash name *free-variables*))

;;;; Defining global functions:


;;; IR1-CONVERT-INLINE-LAMBDA  --  Interface
;;;
;;;    Convert Fun as a lambda in the null environment, but use the current
;;; compilation policy.  Note that Fun may be a LAMBDA-WITH-ENVIRONMENT, so we
;;; may have to augment the environment to reflect the state at the definition
;;; site.
;;;
(defun ir1-convert-inline-lambda (fun &optional name parent-form)
  (destructuring-bind (declarations macros symbol-macros &rest body)
		      (if (eq (car fun) 'lambda-with-environment)
			  (cdr fun)
			  `(() () () . ,(cdr fun)))
    (let ((*lexical-environment*
	   (make-lexenv
	    :default
	    (process-declarations declarations nil nil
				  (make-continuation)
				  (make-null-environment))
	    :variables (copy-list symbol-macros)
	    :functions
	    (mapcar #'(lambda (x)
			`(,(car x) .
			  (macro . ,(coerce (cdr x) 'function))))
		    macros)
	    :cookie (lexenv-cookie *lexical-environment*)
	    :interface-cookie
	    (lexenv-interface-cookie *lexical-environment*))))
      (ir1-convert-lambda `(lambda ,@body) name parent-form))))


;;; INLINE-SYNTACTIC-CLOSURE-LAMBDA  --  Internal
;;;
;;;    Return a lambda that has been "closed" with respect to Env, returning a
;;; LAMBDA-WITH-ENVIRONMENT if there are interesting macros or declarations.
;;; If there is something too complex (like a lexical variable) in the
;;; environment, then we return NIL.
;;;
(defun inline-syntactic-closure-lambda
       (lambda &optional (env *lexical-environment*))
  (let ((variables (lexenv-variables env))
	(functions (lexenv-functions env))
	(decls ())
	(symmacs ())
	(macros ()))
    (cond ((or (lexenv-blocks env) (lexenv-tags env)) nil)
	  ((and (null variables) (null functions))
	   lambda)
	  ((dolist (x variables nil)
	     (let ((name (car x))
		   (what (cdr x)))
	       (when (eq x (assoc name variables :test #'eq))
		 (typecase what
		   (cons
		    (assert (eq (car what) 'macro))
		    (push x symmacs))
		   (global-var
		    (assert (eq (global-var-kind what) :special))
		    (push `(special ,name) decls))
		   (t (return t))))))
	   nil)
	  ((dolist (x functions nil)
	     (let ((name (car x))
		   (what (cdr x)))
               ;; emarsden2003-04-09 change to #'FUNCTION-NAME-EQV-P ?
	       (when (eq x (assoc name functions :test #'equal))
		 (typecase what
		   (cons
		    (push (cons name
				(function-lambda-expression (cdr what)))
			  macros))
		   (global-var
		    (when (defined-function-p what)
		      (push `(,(car (rassoc (defined-function-inlinep what)
					    inlinep-translations))
			      ,name)
			    decls)))
		   (t (return t))))))
	   nil)
	  (t
	   `(lambda-with-environment ,decls ,macros ,symmacs
				     . ,(rest lambda))))))


;;; GET-DEFINED-FUNCTION  --  Internal
;;;
;;;    Get a DEFINED-FUNCTION object for a function we are about to define.  If
;;; the function has been forward referenced, then substitute for the previous
;;; references.
;;;
(defun get-defined-function (name)
  (let* ((name (define-function-name name))
	 (found (find-free-function name "")))
    (note-name-defined name :function)
    (cond ((not (defined-function-p found))
	   ;;
	   ;; This assertion is wrong in block compilation mode, for
	   ;; instance
	   ;;
	   ;; (defun foo (x) (bar x))
	   ;; (declaim (inline bar))
	   ;; (defun bar (x) x)
	   ;;
	   ;; (assert (not (info function inlinep name)))
	   (let* ((where-from (leaf-where-from found))
		  (res (make-defined-function
			:name name
			:where-from (if (eq where-from :declared)
					:declared :defined)
			:type (leaf-type found))))
	     (substitute-leaf res found)
	     (setf (gethash name *free-functions*) res)))
	  ;;
	  ;; If *FREE-FUNCTIONS* has a previously converted definition for this
	  ;; name, then blow it away and try again.
	  ((defined-function-functional found)
	   (remhash name *free-functions*)
	   (get-defined-function name))
	  (t found))))


;;; ASSERT-NEW-DEFINITION  --  Internal
;;;
;;;    Check a new global function definition for consistency with previous
;;; declaration or definition, and assert argument/result types if appropriate.
;;; This assertion is suppressed by the EXPLICIT-CHECK attribute, which is
;;; specified on functions that check their argument types as a consequence of
;;; type dispatching.  This avoids redundant checks such as NUMBERP on the args
;;; to +, etc.
;;;
(defun assert-new-definition (var fun)
  (let ((type (leaf-type var))
	(for-real (eq (leaf-where-from var) :declared))
	(info (info function info (leaf-name var))))
    (assert-definition-type
     fun type
     :error-function #'compiler-warning
     :warning-function (cond (info #'compiler-warning)
			     (for-real #'compiler-note)
			     (t nil))
     :really-assert
     (and for-real
	  (not (and info
		    (ir1-attributep (function-info-attributes info)
				    explicit-check))))
     :where (if for-real
		"previous declaration"
		"previous definition"))))


;;; IR1-CONVERT-LAMBDA-FOR-DEFUN  --  Interface
;;;
;;;    Convert a lambda doing all the basic stuff we would do if we were
;;; converting a DEFUN.  This is used both by the %DEFUN translator and for
;;; global inline expansion.
;;;
;;; Unless a :INLINE function, we temporarily clobber the inline expansion.
;;; This prevents recursive inline expansion of opportunistic pseudo-inlines.
;;;
(defun ir1-convert-lambda-for-defun
       (lambda var expansion converter parent-form)
  (declare (cons lambda) (function converter) (type defined-function var))
  (let ((var-expansion (defined-function-inline-expansion var)))
    (unless (eq (defined-function-inlinep var) :inline)
      (setf (defined-function-inline-expansion var) nil))
    (let* ((name (leaf-name var))
	   ;; converter is either ir1-convert-lambda or ir1-convert-inline-lambda
	   ;; we allow-debug-catch-tag if it's not inline --jwr
	   (fun (if (eq converter #'ir1-convert-inline-lambda)
		    (funcall converter 
			     lambda 
			     name 
			     parent-form)
		    (funcall converter 
			     lambda 
			     name 
			     parent-form
			     t)))
	   (function-info (info function info name)))
      (setf (functional-inlinep fun) (defined-function-inlinep var))
      (assert-new-definition var fun)
      (setf (defined-function-inline-expansion var) var-expansion)
      ;;
      ;; If definitely not an interpreter stub, then substitute for any
      ;; old references.
      (unless (or (eq (defined-function-inlinep var) :notinline)
		  (not *block-compile*)
		  (and function-info
		       (or (function-info-transforms function-info)
			   (function-info-templates function-info)
			   (function-info-ir2-convert function-info))))
	(substitute-leaf fun var)
	;;
	;; If in a simple environment, then we can allow backward references
	;; to this function from following top-level forms.
	(when expansion (setf (defined-function-functional var) fun)))
      fun)))
	

;;; %DEFUN IR1 convert  --  Internal
;;;
;;; Convert the definition and install it in the global environment with a
;;; LABELS-like effect.  If the lexical environment is not null, then we only
;;; install the definition during the processing of this DEFUN, ensuring that
;;; the function cannot be called outside of the correct environment.  If the
;;; function is globally NOTINLINE, then that inhibits even local substitution.
;;; Also, emit top-level code to install the definition.
;;;
;;; This is one of the major places where the semantics of block compilation is
;;; handled.  Substitution for global names is totally inhibited if
;;; *block-compile* is NIL.  And if *block-compile* is true and entry points
;;; are specified, then we don't install global definitions for non-entry
;;; functions (effectively turning them into local lexical functions.)
;;;
(def-ir1-translator %defun ((name def doc source) start cont
			    :kind :function)
  (declare (ignore source))
  (let* ((name (eval name))
	 (lambda (second def))
	 (*current-path* (revert-source-path 'defun))
	 (expansion (unless (eq (info function inlinep name) :notinline)
		      (inline-syntactic-closure-lambda lambda))))
    ;;
    ;; If not in a simple environment or :notinline, then discard any forward
    ;; references to this function.
    (unless expansion (remhash name *free-functions*))
    
    (let* ((var (get-defined-function name))
	   (save-expansion (and (member (defined-function-inlinep var)
					'(:inline :maybe-inline))
				expansion)))
      (setf (defined-function-inline-expansion var) expansion)
      (setf (info function inline-expansion name) save-expansion)
      ;;
      ;; If there is a type from a previous definition, blast it, since it is
      ;; obsolete.
      (when (eq (leaf-where-from var) :defined)
	(setf (leaf-type var) (specifier-type 'function)))
      
      (let ((fun (ir1-convert-lambda-for-defun lambda var expansion
					       #'ir1-convert-lambda
					       'defun)))
	(ir1-convert
	 start cont
	 (if (and *block-compile* *entry-points*
		  (not (member name *entry-points* :test #'equal)))
	     `',name
	     `(%%defun ',name ,fun ,doc
		       ,@(when save-expansion `(',save-expansion)))))

	(when *compile-print*
	  (compiler-mumble "~&; Converted ~S.~%" name))))))
