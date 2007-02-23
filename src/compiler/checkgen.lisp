;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/checkgen.lisp,v 1.34 2005/12/09 15:50:20 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file implements type check generation.  This is a phase that runs
;;; at the very end of IR1.  If a type check is too complex for the back end to
;;; directly emit in-line, then we transform the check into an explicit
;;; conditional using TYPEP.
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package "C")


;;;; Cost estimation:


;;; Function-Cost  --  Internal
;;;
;;;    Return some sort of guess about the cost of a call to a function.  If
;;; the function has some templates, we return the cost of the cheapest one,
;;; otherwise we return the cost of CALL-NAMED.  Calling this with functions
;;; that have transforms can result in relatively meaningless results
;;; (exaggerated costs.)
;;;
;;; We randomly special-case NULL, since it does have a source tranform and is
;;; interesting to us.
;;;
(defun function-cost (name)
  (declare (symbol name))
  (let ((info (info function info name))
	(call-cost (template-cost (template-or-lose 'call-named *backend*))))
    (if info
	(let ((templates (function-info-templates info)))
	  (if templates
	      (template-cost (first templates))
	      (case name
		(null (template-cost (template-or-lose 'if-eq *backend*)))
		(t call-cost))))
	call-cost)))

  
;;; Type-Test-Cost  --  Internal
;;;
;;;    Return some sort of guess for the cost of doing a test against TYPE.
;;; The result need not be precise as long as it isn't way out in space.  The
;;; units are based on the costs specified for various templates in the VM
;;; definition.
;;;
(defun type-test-cost (type)
  (declare (type ctype type))
  (or (let ((check (type-check-template type)))
	(if check
	    (template-cost check)
	    (let ((found (cdr (assoc type (backend-type-predicates *backend*)
				     :test #'type=))))
	      (if found
		  (+ (function-cost found) (function-cost 'eq))
		  nil))))
      (typecase type
	(union-type
	 (collect ((res 0 +)) 
	   (dolist (mem (union-type-types type))
	     (res (type-test-cost mem)))
	   (res)))
	(intersection-type
	 (collect ((res 0 +)) 
	   (dolist (mem (intersection-type-types type))
	     (res (type-test-cost mem)))
	   (res)))
	(member-type
	 (* (length (member-type-members type))
	    (function-cost 'eq)))
	(numeric-type
	 (* (if (numeric-type-complexp type) 2 1)
	    (function-cost
	     (if (csubtypep type (specifier-type 'fixnum)) 'fixnump 'numberp))
	    (+ 1
	       (if (numeric-type-low type) 1 0)
	       (if (numeric-type-high type) 1 0))))
	(cons-type
	 (+ (type-test-cost (specifier-type 'cons))
	    (function-cost 'car)
	    (type-test-cost (cons-type-car-type type))
	    (function-cost 'cdr)
	    (type-test-cost (cons-type-cdr-type type))))
	(t
	 (function-cost 'typep)))))


;;;; Checking strategy determination:


;;; MAYBE-WEAKEN-CHECK  --  Internal
;;;
;;;    Return the type we should test for when we really want to check for
;;; Type.   If speed, space or compilation speed is more important than safety,
;;; then we return a weaker type if it is easier to check.  First we try the
;;; defined type weakenings, then look for any predicate that is cheaper.
;;;
;;;    If the supertype is equal in cost to the type, we prefer the supertype.
;;; This produces a closer approximation of the right thing in the presence of
;;; poor cost info.
;;;
(defun maybe-weaken-check (type cont)
  (declare (type ctype type) (type continuation cont))
  (cond ((policy (continuation-dest cont)
		 (<= speed safety) (<= space safety) (<= cspeed safety))
	 type)
	(t
	 (let ((min-cost (type-test-cost type))
	       (min-type type)
	       (found-super nil))
	   (dolist (x (backend-type-predicates *backend*))
	     (let ((stype (car x)))
	       (when (and (csubtypep type stype)
			  (not (union-type-p stype))) ;Not #!% COMMON type.
		 (let ((stype-cost (type-test-cost stype)))
		   (when (or (< stype-cost min-cost)
			     (type= stype type))
		     (setq found-super t)
		     (setq min-type stype  min-cost stype-cost))))))
	   (if found-super
	       min-type
	       *universal-type*)))))


;;; NO-FUNCTION-TYPES  --  Internal
;;;
;;;    Mash any complex function types to FUNCTION.
;;;
(defun no-function-types (types)
  (declare (type list types))
  (mapcar #'(lambda (type)
	      (if (function-type-p type)
		  (specifier-type 'function)
		  type))
	  types))


;;; Values-types-asserted  --  Internal
;;;
;;;    Like values-types, but when an argument is proven to be delivered,
;;; convert asserted optional and rest arguments to required arguments. This
;;; makes it clear that these required arguments may all be type checked.
;;;
(defun values-types-asserted (atype ptype)
  (declare (type ctype atype ptype))
  (flet ((give-up ()
	   (return-from values-types-asserted (values nil :unknown))))
    (cond ((eq atype *wild-type*)
	   (give-up))
	  ((not (values-type-p atype))
	   (values (list atype) 1))
	  ((or (values-type-keyp atype)
	       (values-type-allowp atype))
	   (give-up))
	  ;;
	  ;; FIXME: Values type checking is done with a form like
	  ;;
	  ;; (multiple-value-bind (x y z) <form>
	  ;;   <type checks for x y z>
	  ;;   (values x y z))
	  ;;
	  ;; see Make-Type-Check-Form.  This has the unfortunate
	  ;; effect of chopping values when <form> actually returns
	  ;; more values than are being checked.  The downside of
	  ;; including this is that it produces a lot of notes.
	  #+nil
	  ((or (eq *wild-type* ptype)
	       (and (values-type-p ptype)
		    (or (values-type-optional ptype)
			(values-type-rest ptype))))
	   (give-up))
	  (t
	   (let* ((ptype (kernel::coerce-to-values ptype))
		  (preq (values-type-required ptype))
		  (popt (values-type-optional ptype))
		  (prest (values-type-rest ptype)))
	     ;;
	     ;; FIXME: ptype = * is not handled right, I think
	     ;; because * = (VALUES &REST T).  It never was
	     ;; handled right.  Gerd 2003-05-08.
	     (collect ((types))
	       (dolist (type (values-type-required atype))
		 (if (or (pop preq) (pop popt) prest)
		     (types (single-value-type type))
		     (give-up)))
	       (dolist (type (values-type-optional atype))
		 (if (pop preq)
		     (types (single-value-type type))
		     (give-up)))
	       (let ((arest (values-type-rest atype)))
		 (when arest
		   (loop with rest-type = (single-value-type arest)
			 for arg = (pop preq) while arg do
			   (types rest-type))
		   (when (or popt prest)
		     (give-up))))
	       (values (types) (length (types)))))))))


;;; Switch to disable check complementing, for evaluation.
;;;
(defvar *complement-type-checks* t)

;;; MAYBE-NEGATE-CHECK  --  Internal
;;;
;;;    Cont is a continuation we are doing a type check on and Types is a list
;;; of types that we are checking its values against.  If we have proven
;;; that Cont generates a fixed number of values, then for each value, we check
;;; whether it is cheaper to then difference between the proven type and
;;; the corresponding type in Types.  If so, we opt for a :HAIRY check with
;;; that test negated.  Otherwise, we try to do a simple test, and if that is
;;; impossible, we do a hairy test with non-negated types.  If true,
;;; Force-Hairy forces a hairy type check.
;;;
;;;    When doing a non-negated check, we call MAYBE-WEAKEN-CHECK to weaken the
;;; test to a convenient supertype (conditional on policy.)  If debug-info is
;;; not particularly important (debug <= 1) or speed is 3, then we allow
;;; weakened checks to be simple, resulting in less informative error messages,
;;; but saving space and possibly time.
;;;
(defun maybe-negate-check (cont types force-hairy)
  (declare (type continuation cont) (list types))
  (multiple-value-bind
      (ptypes count)
      (values-types (continuation-proven-type cont))
    (if (eq count :unknown)
	(if (and (every #'type-check-template types) (not force-hairy))
	    (values :simple types)
	    (values :hairy
		    (mapcar #'(lambda (x)
				(list nil (maybe-weaken-check x cont) x))
			    types)))
	(let ((res (mapcar #'(lambda (p c)
			       (if (csubtypep p c)
				   (list nil *universal-type* c)
				   (let ((diff (type-difference p c))
					 (weak (maybe-weaken-check c cont)))
				     (if (and diff
					      (< (type-test-cost diff)
						 (type-test-cost weak))
					      *complement-type-checks*)
					 (list t diff c)
					 (list nil weak c)))))
			   (no-function-types ptypes) types)))
	  (cond ((or force-hairy (find-if #'first res))
		 (values :hairy res))
		((every #'type-check-template types)
		 (values :simple types))
		((policy (continuation-dest cont)
			 (or (<= debug 1) (and (= speed 3) (/= debug 3))))
		 (let ((weakened (mapcar #'second res)))
		   (if (every #'type-check-template weakened)
		       (values :simple weakened)
		       (values :hairy res))))
		(t
		 (values :hairy res)))))))
	    

;;; CONTINUATION-CHECK-TYPES  --  Interface
;;;
;;; Determines whether Cont's assertion is:
;;;  -- Checkable by the back end (:SIMPLE), or
;;;  -- Not checkable by the back end, but checkable via an explicit test in
;;;     type check conversion (:HAIRY), or
;;;  -- not reasonably checkable at all (:TOO-HAIRY).
;;;
;;; A type is checkable if it either represents a fixed number of values (as
;;; determined by VALUES-TYPES), or it is the assertion for an MV-Bind.  A type
;;; is simply checkable if all the type assertions have a TYPE-CHECK-TEMPLATE.
;;; In this :SIMPLE case, the second value is a list of the type restrictions
;;; specified for the leading positional values.
;;;
;;; We force a check to be hairy even when there are fixed values if we are in
;;; a context where we may be forced to use the unknown values convention
;;; anyway.  This is because IR2tran can't generate type checks for unknown
;;; values continuations but people could still be depending on the check being
;;; done.  We only care about EXIT and RETURN (not MV-COMBINATION) since these
;;; are the only contexts where the ultimate values receiver 
;;;
;;; In the :HAIRY case, the second value is a list of triples of the form:
;;;    (Not-P Type Original-Type)
;;;
;;; If true, the Not-P flag indicates a test that the corresponding value is
;;; *not* of the specified Type.  Original-Type is the type asserted on this
;;; value in the continuation, for use in error messages.  When Not-P is true,
;;; this will be different from Type.
;;;
;;; This allows us to take what has been proven about Cont's type into
;;; consideration.  If it is cheaper to test for the difference between the
;;; derived type and the asserted type, then we check for the negation of this
;;; type instead.
;;;
;;; When the proven type represents an unknown number of values, but Cont's
;;; destination receives only a single value, a :hairy type check is
;;; generated for the single-values-type of the asserted type.
;;;
(defun continuation-check-types (cont &optional force-hairy)
  (declare (type continuation cont))
  (let ((atype (continuation-asserted-type cont))
	(dest (continuation-dest cont))
	(proven (continuation-proven-type cont)))
    (assert (not (eq atype *wild-type*)))
    (multiple-value-bind (types count)
	(values-types-asserted atype proven)
      (cond ((not (eq count :unknown))
	     (let ((types (no-function-types types)))
	       (if (or (exit-p dest)
		       (and (return-p dest)
			    (multiple-value-bind
				  (ignore count)
				(values-types (return-result-type dest))
			      (declare (ignore ignore))
			      (eq count :unknown))))
		   (maybe-negate-check cont types t)
		   (maybe-negate-check cont types force-hairy))))
	    #+nil
	    ((eq *wild-type* proven)
	     (values :too-hairy nil))
	    ((and (mv-combination-p dest)
		  (eq (basic-combination-kind dest) :local))
	     (assert (values-type-p atype))
	     (maybe-negate-check cont (append (args-type-required atype)
					      (args-type-optional atype))
				 force-hairy))
	    ((or (exit-p dest) (return-p dest) (mv-combination-p dest))
	     (values :too-hairy nil))
	    (t
	     (maybe-negate-check cont (list (single-value-type atype)) t))))))


;;; Probable-Type-Check-P  --  Internal
;;;
;;;    Return true if Cont is a continuation whose type the back end is likely
;;; to want to check.  Since we don't know what template the back end is going
;;; to choose to implement the continuation's DEST, we use a heuristic.  We
;;; always return T unless:
;;;  -- Nobody uses the value, or
;;;  -- Safety is totally unimportant, or
;;;  -- the continuation is an argument to an unknown function, or
;;;  -- the continuation is an argument to a known function that has no
;;;     IR2-Convert method or :fast-safe templates that are compatible with the
;;;     call's type.
;;;
;;; We must only return nil when it is *certain* that a check will not be done,
;;; since if we pass up this chance to do the check, it will be too late.  The
;;; penalty for being too conservative is duplicated type checks.
;;;
;;; If there is a compile-time type error, then we always return true unless
;;; the DEST is a full call.  With a full call, the theory is that the type
;;; error is probably from a declaration in (or on) the callee, so the callee
;;; should be able to do the check.  We want to let the callee do the check,
;;; because it is possible that the error is really in the callee, not the
;;; caller.  We don't want to make people recompile all calls to a function
;;; when they were originally compiled with a bad declaration (or an old type
;;; assertion derived from a definition appearing after the call.)
;;;
(defun probable-type-check-p (cont)
  (declare (type continuation cont))
  (let ((dest (continuation-dest cont)))
    (cond ((eq (continuation-type-check cont) :error)
	   (if (and (combination-p dest) (eq (combination-kind dest) :error))
	       (policy dest (= safety 3))
	       t))
	  ((or (not dest)
	       (policy dest (zerop safety)))
	   nil)
	  ((basic-combination-p dest)
	   (let ((kind (basic-combination-kind dest)))
	     (cond ((eq cont (basic-combination-fun dest)) t)
		   ((eq kind :local) t)
		   ((member kind '(:full :error)) nil)
		   ((function-info-ir2-convert kind) t)
		   (t
		    (dolist (template (function-info-templates kind) nil)
		      (when (eq (template-policy template) :fast-safe)
			(multiple-value-bind
			    (val win)
			    (valid-function-use dest (template-type template))
			  (when (or val (not win)) (return t)))))))))
	  (t t))))


;;; Make-Type-Check-Form  --  Internal
;;;
;;;    Return a form that we can convert to do a hairy type check of the
;;; specified Types.  Types is a list of the format returned by
;;; Continuation-Check-Types in the :HAIRY case.  In place of the actual
;;; value(s) we are to check, we use 'Dummy.  This constant reference is later
;;; replaced with the actual values continuation.
;;;
;;; Note that we don't attempt to check for required values being unsupplied.
;;; Such checking is impossible to efficiently do at the source level because
;;; our fixed-values conventions are optimized for the common MV-Bind case.
;;;
;;; We can always use Multiple-Value-Bind, since the macro is clever about
;;; binding a single variable.
;;;
(defun make-type-check-form (types)
  (collect ((temps))
    (dotimes (i (length types))
      (temps (gensym)))

    `(multiple-value-bind ,(temps)
			  'dummy
       ,@(mapcar #'(lambda (temp type)
		     (let* ((spec
			     (let ((*unparse-function-type-simplify* t))
			       (type-specifier (second type))))
			    (test (if (first type) `(not ,spec) spec)))
		       `(unless (typep ,temp ',test)
			  (%type-check-error
			   ,temp
			   ',(type-specifier (third type))))))
		 (temps) types)
       (values ,@(temps)))))
  

;;; Convert-Type-Check  --  Internal
;;;
;;;    Splice in explicit type check code immediately before the node that its
;;; Cont's Dest.  This code receives the value(s) that were being passed to
;;; Cont, checks the type(s) of the value(s), then passes them on to Cont.
;;; We:
;;;  -- Ensure that Cont starts a block, so that we can freely manipulate its
;;;     uses.
;;;  -- Make a new continuation and move Cont's uses to it.  Set type set
;;;     Type-Check in Cont to :DELETED to indicate that the check has been
;;;     done.
;;;  -- Make the Dest node start its block so that we can splice in the type
;;;     check code.
;;;  -- Splice in a new block before the Dest block, giving it all the Dest's
;;;     predecessors. 
;;;  -- Convert the check form, using the new block start as Start and a dummy
;;;     continuation as Cont.
;;;  -- Set the new block's start and end cleanups to the *start* cleanup of
;;;     Prev's block.  This overrides the incorrect default from
;;;     With-IR1-Environment.
;;;  -- Finish off the dummy continuation's block, and change the use to a use
;;;     of Cont.  (we need to use the dummy continuation to get the control
;;;     transfer right, since we want to go to Prev's block, not Cont's.)
;;;     Link the new block to Prev's block.
;;;  -- Substitute the new continuation for the dummy placeholder argument.
;;;     Since no let conversion has been done yet, we can find the placeholder.
;;;     The [mv-]combination node from the mv-bind in the check form will be
;;;     the Use of the new check continuation.  We substitute for the first
;;;     argument of this node.
;;;  -- Invoke local call analysis to convert the call to a let.
;;;
(defun convert-type-check (cont types)
  (declare (type continuation cont) (list types))
  (with-ir1-environment (continuation-dest cont)
    (ensure-block-start cont)    
    (let* ((new-start (make-continuation))
	   (dest (continuation-dest cont))
	   (prev (node-prev dest)))
      (continuation-starts-block new-start)
      (substitute-continuation-uses new-start cont)
      (setf (continuation-%type-check cont) :deleted)
      
      (when (continuation-use prev)
	(node-ends-block (continuation-use prev)))
      
      (let* ((prev-block (continuation-block prev))
	     (new-block (continuation-block new-start))
	     (dummy (make-continuation)))
	(dolist (block (block-pred prev-block))
	  (change-block-successor block prev-block new-block))
	(ir1-convert new-start dummy (make-type-check-form types))
	(assert (eq (continuation-block dummy) new-block))

	(let ((node (continuation-use dummy)))
	  (setf (block-last new-block) node)
	  (delete-continuation-use node)
	  (add-continuation-use node cont))
	(link-blocks new-block prev-block))
      
      (let* ((node (continuation-use cont))
	     (args (basic-combination-args node))
	     (victim (first args)))
	(assert (and (= (length args) 1)
		     (eq (constant-value
			  (ref-leaf
			   (continuation-use victim)))
			 'dummy)))
	(substitute-continuation new-start victim)))

    (local-call-analyze *current-component*))
  
  (undefined-value))


;;; DO-TYPE-WARNING  --  Internal
;;;
;;;    Emit a type warning for Node.  If the value of node is being used for a
;;; variable binding, we figure out which one for source context.  If the value
;;; is a constant, we print it specially.  We ignore nodes whose type is NIL,
;;; since they are supposed to never return.
;;;
(defun do-type-warning (node)
  (declare (type node node))
  (let* ((*compiler-error-context* node)
	 (cont (node-cont node))
	 (atype-spec (type-specifier (continuation-asserted-type cont)))
	 (dtype (node-derived-type node))
	 (dest (continuation-dest cont))
	 (what (when (and (combination-p dest)
			  (eq (combination-kind dest) :local))
		 (let ((lambda (combination-lambda dest))
		       (pos (eposition cont (combination-args dest))))
		   (format nil "~:[A possible~;The~] binding of ~S"
			   (and (continuation-use cont)
				(eq (functional-kind lambda) :let))
			   (leaf-name (elt (lambda-vars lambda) pos)))))))
    (cond ((eq dtype *empty-type*))
	  ((and (ref-p node) (constant-p (ref-leaf node)))
	   (compiler-warning "~:[This~;~:*~A~] is not a ~<~%~9T~:;~S:~>~%  ~S"
			     what atype-spec (constant-value (ref-leaf node))))
	  (t
	   (compiler-warning
	    "~:[Result~;~:*~A~] is a ~S, ~<~%~9T~:;not a ~S.~>"
	    what (type-specifier dtype) atype-spec))))
  (undefined-value))


;;; MARK-ERROR-CONTINUATION  --  Internal
;;;
;;;    Mark Cont as being a continuation with a manifest type error.  We set
;;; the kind to :ERROR, and clear any FUNCTION-INFO if the continuation is an
;;; argument to a known call.  The last is done so that the back end doesn't
;;; have to worry about type errors in arguments to known functions.  This
;;; clearing is inhibited for things with IR2-CONVERT methods, since we can't
;;; do a full call to funny functions.
;;;
(defun mark-error-continuation (cont)
  (declare (type continuation cont))
  (setf (continuation-%type-check cont) :error)
  (let ((dest (continuation-dest cont)))
    (when (and (combination-p dest)
	       (let ((kind (basic-combination-kind dest)))
		 (or (eq kind :full) 
		     (and (function-info-p kind)
			  (not (function-info-ir2-convert kind))))))
      (setf (basic-combination-kind dest) :error)))
  (undefined-value))


;;; Generate-Type-Checks  --  Interface
;;;
;;;    Loop over all blocks in Component that have TYPE-CHECK set, looking for
;;; continuations with TYPE-CHECK T.  We do two mostly unrelated things: detect
;;; compile-time type errors and determine if and how to do run-time type
;;; checks.
;;;
;;;    If there is a compile-time type error, then we mark the continuation and
;;; emit a warning if appropriate.  This part loops over all the uses of the
;;; continuation, since after we convert the check, the :DELETED kind will
;;; inhibit warnings about the types of other uses.
;;;
;;;    If a continuation is too complex to be checked by the back end, or is
;;; better checked with explicit code, then convert to an explicit test.
;;; Assertions that can checked by the back end are passed through.  Assertions
;;; that can't be tested are flamed about and marked as not needing to be
;;; checked.
;;;
;;;    If we determine that a type check won't be done, then we set TYPE-CHECK
;;; to :NO-CHECK.  In the non-hairy cases, this is just to prevent us from
;;; wasting time coming to the same conclusion again on a later iteration.  In
;;; the hairy case, we must indicate to LTN that it must choose a safe
;;; implementation, since IR2 conversion will choke on the check.
;;;
;;;    The generation of the type checks is delayed until all the type
;;; check decisions have been made because the generation of the type
;;; checks creates new nodes who's derived types aren't always updated
;;; which may lead to inappropriate template choices due to the
;;; modification of argument types.
;;;
(defun generate-type-checks (component)
  (collect ((conts))
    (do-blocks (block component)
      (when (block-type-check block)
        (do-nodes (node cont block)
	  (let ((type-check (continuation-type-check cont)))
	    (unless (member type-check '(nil :error :deleted))
	      (let ((atype (continuation-asserted-type cont)))
		(do-uses (use cont)
		  (unless (values-types-intersect (node-derived-type use)
						  atype)
		    (mark-error-continuation cont)
		    (unless (policy node (= brevity 3))
		      (do-type-warning use))))))
	    (when (and (eq type-check t)
		       (not *byte-compiling*))
	      (if (probable-type-check-p cont)
		  (conts cont)
		  (setf (continuation-%type-check cont) :no-check)))))

	(setf (block-type-check block) nil)))

    (dolist (cont (conts))
      (multiple-value-bind (check types)
	  (continuation-check-types
	   cont
	   (and (eq (continuation-%type-check cont) :error)
		(policy (continuation-dest cont) (= safety 3))))
	(ecase check
	  (:simple)
	  (:hairy
	   (convert-type-check cont types))
	  (:too-hairy
	   (let* ((context (continuation-dest cont))
		  (*compiler-error-context* context))
	     (when (policy context (>= safety brevity))
	       (compiler-note
		"Type assertion too complex to check:~% ~S."
		(type-specifier (continuation-asserted-type cont)))))
	   (setf (continuation-%type-check cont) :deleted))))))

  (undefined-value))
