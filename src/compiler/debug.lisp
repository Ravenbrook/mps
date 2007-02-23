;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/debug.lisp,v 1.35 2002/12/07 18:19:33 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Utilities for debugging the compiler.  Currently contains only stuff for
;;; checking the consistency of the IR1.
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package "C")

(export '(label-id))


(defvar *args* ()
  "This variable is bound to the format arguments when an error is signalled
  by Barf or Burp.")

(defvar *ignored-errors* (make-hash-table :test #'equal))

;;; Barf  --  Interface
;;;
;;;    A definite inconsistency has been detected.  Signal an error with
;;; *args* bound to the list of the format args.
;;;
(defun barf (string &rest *args*)
  (declare (string string))
  (unless (gethash string *ignored-errors*)
    (restart-case
	(apply #'error string *args*)
      (continue ()
	:report "Ignore this error.")
      (ignore-all ()
	:report "Ignore this and all future occurrences of this error."
	(setf (gethash string *ignored-errors*) t)))))

(defvar *burp-action* :warn
  "Action taken by the Burp function when a possible compiler bug is detected.
  One of :Warn, :Error or :None.")

(declaim (type (member :warn :error :none) *burp-action*))

;;; Burp  --  Interface
;;;
;;;    Called when something funny but possibly correct is noticed.  Otherwise
;;; similar to Barf.
;;;
(defun burp (string &rest *args*)
  (declare (string string))
  (ecase *burp-action*
    (:warn (apply #'warn string *args*))
    (:error (apply #'cerror "press on anyway." string *args*))
    (:none)))


;;; *Seen-Blocks* is a hashtable with true values for all blocks which appear
;;; in the DFO for one of the specified components.
;;;
(defvar *seen-blocks*)

;;; *Seen-Functions* is similar, but records all the lambdas we reached by
;;; recursing on top-level functions.
;;;
(defvar *seen-functions*)


;;; Check-Node-Reached  --  Internal
;;;
;;;    Barf if Node is in a block which wasn't reached during the graph
;;; walk.
;;;
(defun check-node-reached (node)
  (declare (type node node))
  (unless (gethash (continuation-block (node-prev node)) *seen-blocks*)
    (barf "~S was not reached." node)))


;;; Check-IR1-Consistency  --  Interface
;;;
;;;    Check everything that we can think of for consistency.  When a definite
;;; inconsistency is detected, we Barf.  Possible problems just cause us to
;;; Burp.  Our argument is a list of components, but we also look at the
;;; *free-variables*, *free-functions* and *constants*.
;;;
;;;    First we do a pre-pass which finds all the blocks and lambdas, testing
;;; that they are linked together properly and entering them in hashtables.
;;; Next, we iterate over the blocks again, looking at the actual code and
;;; control flow.  Finally, we scan the global leaf hashtables, looking for
;;; lossage.
;;;
(defun check-ir1-consistency (components)
  (declare (list components))
  (let ((*seen-blocks* (make-hash-table :test #'eq))
	(*seen-functions* (make-hash-table :test #'eq)))
    (dolist (c components)
      (let* ((head (component-head c))
	     (tail (component-tail c)))
	(unless (and (null (block-pred head)) (null (block-succ tail)))
	  (barf "~S malformed." c))

	(do ((prev nil block)
	     (block head (block-next block)))
	    ((null block)
	     (unless (eq prev tail)
	       (barf "Wrong Tail for DFO, ~S in ~S." prev c)))
	  (setf (gethash block *seen-blocks*) t)
	  (unless (eq (block-prev block) prev)
	    (barf "Bad Prev for ~S, should be ~S." block prev))
	  (unless (or (eq block tail)
		      (eq (block-component block) c))
	    (barf "~S is not in ~S." block c)))
#|
	(when (or (loop-blocks c) (loop-inferiors c))
	  (do-blocks (block c :both)
	    (setf (block-flag block) nil))
	  (check-loop-consistency c nil)
	  (do-blocks (block c :both)
	    (unless (block-flag block)
	      (barf "~S was not in any loop." block))))
|#
	))

    (check-function-consistency components)

    (dolist (c components)
      (do ((block (block-next (component-head c)) (block-next block)))
	  ((null (block-next block)))
	(check-block-consistency block)))


    (maphash #'(lambda (k v)
		 (declare (ignore k))
                 (cond ((alien::heap-alien-info-p v)
                        ;; objects of type HEAP-ALIEN-INFO may be
                        ;; present in *FREE-VARIABLES* due to the way
                        ;; that the alien machinery automatically
                        ;; dereferences an alien variable's name to
                        ;; its value. Just ignore them.
                        t)
                       ((leaf-p v)
		 (unless (or (constant-p v)
			     (and (global-var-p v)
				  (member (global-var-kind v)
					  '(:global :special :constant))))
                          (barf "Strange *FREE-VARIABLES* entry: ~S." v))
		 (dolist (n (leaf-refs v))
		   (check-node-reached n))
		 (when (basic-var-p v)
		   (dolist (n (basic-var-sets v))
		     (check-node-reached n))))
                       (t
                        (barf "Member of *FREE-VARIABLES* is of unexpected type: ~S" v))))
	     *free-variables*)

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (unless (constant-p v)
		   (barf "Strange *constants* entry: ~S." v))
		 (dolist (n (leaf-refs v))
		   (check-node-reached n)))
	     *constants*)

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (unless (or (functional-p v)
			     (and (global-var-p v)
				  (eq (global-var-kind v) :global-function)))
		   (barf "Strange *free-functions* entry: ~S." v))
		 (dolist (n (leaf-refs v))
		   (check-node-reached n)))
	     *free-functions*))
  (values))


;;;; Function consistency checking:

;;; Observe-Functional  --  Internal
;;;
(defun observe-functional (x)
  (declare (type functional x))
  (when (gethash x *seen-functions*)
    (barf "~S seen more than once." x))
  (unless (eq (functional-kind x) :deleted)
    (setf (gethash x *seen-functions*) t)))


;;; Check-Function-Reached  --  Internal
;;;
;;;    Check that the specified function has been seen. 
;;;
(defun check-function-reached (fun where)
  (declare (type functional fun))
  (unless (gethash fun *seen-functions*)
    (barf "Unseen function ~S in ~S." fun where)))


;;; Check-Function-Stuff  --  Internal
;;;
;;;    In a lambda, check that the associated nodes are in seen blocks.  In an
;;; optional dispatch, check that the entry points were seen.  If the function
;;; is deleted, ignore it.
;;;
(defun check-function-stuff (functional)
  (ecase (functional-kind functional)
    (:external
     (let ((fun (functional-entry-function functional)))
       (check-function-reached fun functional)
       (when (functional-kind fun)
	 (barf "Function for XEP ~S has kind." functional))
       (unless (eq (functional-entry-function fun) functional)
	 (barf "Bad back-pointer in function for XEP ~S." functional))))
    ((:let :mv-let :assignment)
     (check-function-reached (lambda-home functional) functional)
     (when (functional-entry-function functional)
       (barf "Let ~S has entry function." functional))
     (unless (member functional (lambda-lets (lambda-home functional)))
       (barf "Let ~S not in Lets for Home." functional))
     (unless (eq (functional-kind functional) :assignment)
       (when (rest (leaf-refs functional))
	 (barf "Let ~S has multiple refernces." functional)))
     (when (lambda-lets functional)
       (barf "Lets in a Let: ~S." functional)))
    (:optional
     (when (functional-entry-function functional)
       (barf ":Optional ~S has an ENTRY-FUNCTION." functional))
     (let ((ef (lambda-optional-dispatch functional)))
       (check-function-reached ef functional)
       (unless (or (member functional (optional-dispatch-entry-points ef))
		   (eq functional (optional-dispatch-more-entry ef))
		   (eq functional (optional-dispatch-main-entry ef)))
	 (barf ":Optional ~S not an e-p for its OPTIONAL-DISPATCH ~S." 
	       functional ef))))
    (:top-level
     (unless (eq (functional-entry-function functional) functional)
       (barf "Entry-Function is ~S isn't a self-pointer." functional)))
    ((nil :escape :cleanup)
     (let ((ef (functional-entry-function functional)))
       (when ef
	 (check-function-reached ef functional)
	 (unless (eq (functional-kind ef) :external)
	   (barf "Entry-Function in ~S isn't an XEP: ~S." functional ef)))))
    (:deleted
     (return-from check-function-stuff)))
  
  (case (functional-kind functional)
    ((nil :optional :external :top-level :escape :cleanup)
     (when (lambda-p functional)
       (dolist (fun (lambda-lets functional))
	 (unless (eq (lambda-home fun) functional)
	   (barf "Home in ~S not ~S." fun functional))
	 (check-function-reached fun functional))
       (unless (eq (lambda-home functional) functional)
	 (barf "Home not self-pointer in ~S." functional)))))
  
  (etypecase functional
    (clambda
     (when (lambda-bind functional)
       (check-node-reached (lambda-bind functional)))
     (when (lambda-return functional)
       (check-node-reached (lambda-return functional)))
     
     (dolist (var (lambda-vars functional))
       (dolist (ref (leaf-refs var))
	 (check-node-reached ref))
       (dolist (set (basic-var-sets var))
	 (check-node-reached set))
       (unless (eq (lambda-var-home var) functional)
	 (barf "HOME in ~S should be ~S." var functional))))
    (optional-dispatch
     (dolist (ep (optional-dispatch-entry-points functional))
       (check-function-reached ep functional))
     (let ((more (optional-dispatch-more-entry functional)))
       (when more (check-function-reached more functional)))
     (check-function-reached (optional-dispatch-main-entry functional)
			     functional))))


;;; Check-Function-Consistency  --  Internal
;;;
(defun check-function-consistency (components)
  (dolist (c components)
    (dolist (fun (component-new-functions c))
      (observe-functional fun))
    (dolist (fun (component-lambdas c))
      (when (eq (functional-kind fun) :external)
	(let ((ef (functional-entry-function fun)))
	  (when (optional-dispatch-p ef)
	    (observe-functional ef))))
      (observe-functional fun)
      (dolist (let (lambda-lets fun))
	(observe-functional let))))

  (dolist (c components)
    (dolist (fun (component-new-functions c))
      (check-function-stuff fun))
    (dolist (fun (component-lambdas c))
      (when (eq (functional-kind fun) :deleted)
	(barf "Deleted lambda ~S in Lambdas for ~S." fun c))
      (check-function-stuff fun)
      (dolist (let (lambda-lets fun))
	(check-function-stuff let)))))


;;;; Loop consistency checking:

#|
;;; Check-Loop-Consistency  --  Internal
;;;
;;;    Descend through the loop nesting and check that the tree is well-formed
;;; and that all blocks in the loops are known blocks.  We also mark each block
;;; that we see so that we can do a check later to detect blocks that weren't
;;; in any loop.
;;;
(defun check-loop-consistency (loop superior)
  (declare (type loop loop) (type (or loop null) superior))
  (unless (eq (loop-superior loop) superior)
    (barf "Wrong superior in ~S, should be ~S." loop superior))
  (when (and superior
	     (/= (loop-depth loop) (1+ (loop-depth superior))))
    (barf "Wrong depth in ~S." loop))

  (dolist (tail (loop-tail loop))
    (check-loop-block tail loop))
  (dolist (exit (loop-exits loop))
    (check-loop-block exit loop))
  (check-loop-block (loop-head loop) loop)
  (unless (eq (block-loop (loop-head loop)) loop)
    (barf "Head of ~S is not directly in the loop." loop))

  (do ((block (loop-blocks loop) (block-loop-next block)))
      ((null block))
    (setf (block-flag block) t)
    (unless (gethash block *seen-blocks*)
      (barf "Unseen block ~S in Blocks for ~S." block loop))
    (unless (eq (block-loop block) loop)
      (barf "Wrong Loop in ~S, should be ~S." block loop)))

  (dolist (inferior (loop-inferiors loop))
    (check-loop-consistency inferior loop)))


;;; Check-Loop-Block  --  Internal
;;;
;;;    Check that Block is either in Loop or an inferior.
;;;
(defun check-loop-block (block loop)
  (declare (type block block) (type loop loop))
  (unless (gethash block *seen-blocks*)
    (barf "Unseen block ~S in loop info for ~S." block loop))
  (labels ((walk (l)
	     (if (eq (block-loop block) l)
		 t
		 (dolist (inferior (loop-inferiors l) nil)
		   (when (walk inferior) (return t))))))
    (unless (walk loop)
      (barf "~S in loop info for ~S but not in the loop." block loop))))

|#


;;; Check-Block-Consistency  --  Internal
;;;
;;;    Check a block for consistency at the general flow-graph level, and call
;;; Check-Node-Consistency on each node to locally check for semantic
;;; consistency.
;;;
(defun check-block-consistency (block)
  (declare (type cblock block))

  (dolist (pred (block-pred block))
    (unless (gethash pred *seen-blocks*)
      (barf "Unseen predecessor ~S in ~S." pred block))
    (unless (member block (block-succ pred))
      (barf "Bad predecessor link ~S in ~S." pred block)))

  (let* ((fun (block-home-lambda block))
	 (fun-deleted (eq (functional-kind fun) :deleted))
	 (this-cont (block-start block))
	 (last (block-last block)))
    (unless fun-deleted
      (check-function-reached fun block))
    (when (not this-cont)
      (barf "~S has no START." block))
    (when (not last)
      (barf "~S has no LAST." block))
    (unless (eq (continuation-kind this-cont) :block-start)
      (barf "Start of ~S has wrong kind." block))

    (let ((use (continuation-use this-cont))
	  (uses (block-start-uses block)))
      (when (and (null use) (= (length uses) 1))
	(barf "~S has unique use, but no USE." this-cont))
      (dolist (node uses)
	(unless (eq (node-cont node) this-cont)
	  (barf "Use ~S for START in ~S has wrong CONT." node block))
	(check-node-reached node)))

    (let* ((last-cont (node-cont last))
	   (cont-block (continuation-block last-cont))
	   (dest (continuation-dest last-cont)))
      (ecase (continuation-kind last-cont)
	(:deleted)
	(:deleted-block-start
	 (let ((dest (continuation-dest last-cont)))
	   (when dest
	     (check-node-reached dest)))
	 (unless (member last (block-start-uses cont-block))
	   (barf "Last in ~S is missing from uses of its Cont." block)))
	(:block-start
	 (check-node-reached (continuation-next last-cont))
	 (unless (member last (block-start-uses cont-block))
	   (barf "Last in ~S is missing from uses of its Cont." block)))
	(:inside-block
	 (unless (eq cont-block block)
	   (barf "Cont of Last in ~S is in a different block." block))
	 (unless (eq (continuation-use last-cont) last)
	   (barf "Use is not Last in Cont of Last in ~S." block))
	 (when (continuation-next last-cont)
	   (barf "Cont of Last has a Next in ~S." block))))

      (when dest
	(check-node-reached dest)))

    (loop	
      (unless (eq (continuation-block this-cont) block)
	(barf "BLOCK in ~S should be ~S." this-cont block))
      
      (let ((dest (continuation-dest this-cont)))
	(when dest
	  (check-node-reached dest)))
      
      (let ((node (continuation-next this-cont)))
	(unless (node-p node)
	  (barf "~S has strange next." this-cont))
	(unless (eq (node-prev node) this-cont)
	  (barf "PREV in ~S should be ~S." node this-cont))

	(unless fun-deleted
	  (check-node-consistency node))
	
	(let ((cont (node-cont node)))
	  (when (not cont)
	    (barf "~S has no CONT." node))
	  (when (eq node last) (return))
	  (unless (eq (continuation-kind cont) :inside-block)
	    (barf "Interior continuation ~S in ~S has wrong kind." cont block))
	  (unless (continuation-next cont)
	    (barf "~S has no NEXT." cont))
	  (unless (eq (continuation-use cont) node)
	    (barf "USE in ~S should be ~S." cont node))
	  (setq this-cont cont))))
	
    (check-block-successors block)))


;;; Check-Block-Successors  --  Internal
;;;
;;;    Check that Block is properly terminated.  Each successor must be
;;; accounted for by the type of the last node.
;;;
(defun check-block-successors (block)
  (declare (type cblock block))
  (let ((last (block-last block))
	(succ (block-succ block)))

    (let* ((comp (block-component block)))
      (dolist (b succ)
	(unless (gethash b *seen-blocks*)
	  (barf "Unseen successor ~S in ~S." b block))
	(unless (member block (block-pred b))
	  (barf "Bad successor link ~S in ~S." b block))
	(unless (eq (block-component b) comp)
	  (barf "Successor ~S in ~S is in a different component." b block))))
    
    (typecase last
      (cif
       (unless (<= 1 (length succ) 2)
	 (barf "~S ends in an IF, but doesn't have one or two succesors."
	       block))
       (unless (member (if-consequent last) succ) 
	 (barf "CONSEQUENT for ~S isn't in SUCC for ~S." last block))
       (unless (member (if-alternative last) succ)
	 (barf "ALTERNATIVE for ~S isn't in SUCC for ~S." last block)))
      (creturn
       (unless (if (eq (functional-kind (return-lambda last)) :deleted)
		   (null succ)
		   (and (= (length succ) 1)
			(eq (first succ)
			    (component-tail (block-component block)))))
	 (barf "Strange successors for RETURN in ~S." block)))
      (exit
       (unless (<= (length succ) 1)
	 (barf "EXIT node has strange number of successors: ~S." last)))
      (t
       (unless (or (= (length succ) 1) (node-tail-p last)
		   (and (block-delete-p block) (null succ)))
	 (barf "~S ends in normal node, but doesn't have one successor."
	       block))))))


;;;; Node consistency checking:

;;; Check-Dest  --  Internal
;;;
;;;    Check that the Dest for CONT is the specified NODE.  We also mark the
;;; block that CONT is in as having been Seen.
;;;
(defun check-dest (cont node)
  (declare (type continuation cont) (type node node))
  (let ((kind (continuation-kind cont)))
    (ecase kind
      (:deleted
       (unless (block-delete-p (node-block node))
	 (barf "DEST ~S of deleted continuation ~S is not DELETE-P."
	       cont node)))
      (:deleted-block-start
       (unless (eq (continuation-dest cont) node)
	 (barf "DEST for ~S should be ~S." cont node)))
      ((:inside-block :block-start)
       (unless (gethash (continuation-block cont) *seen-blocks*)
	 (barf "~S receives ~S, which is in an unknown block." node cont))
       (unless (eq (continuation-dest cont) node)
	 (barf "DEST for ~S should be ~S." cont node))))))



;;; Check-Node-Consistency  --  Internal
;;;
;;;    This function deals with checking for consistency the type-dependent
;;; information in a node.
;;;
(defun check-node-consistency (node)
  (declare (type node node))
  (etypecase node
    (ref
     (let ((leaf (ref-leaf node)))
       (when (functional-p leaf)
	 (if (eq (functional-kind leaf) :top-level-xep)
	     (unless (eq (component-kind (block-component (node-block node)))
			 :top-level)
	       (barf ":TOP-LEVEL-XEP ref in non-top-level component: ~S."
		     node))
	     (check-function-reached leaf node)))))
    (basic-combination
     (check-dest (basic-combination-fun node) node)
     (dolist (arg (basic-combination-args node))
       (cond
	(arg (check-dest arg node))
	((not (and (eq (basic-combination-kind node) :local)
		   (combination-p node)))
	 (barf "Flushed arg not in local call: ~S." node))
	(t
	 (let ((fun (ref-leaf (continuation-use
			       (basic-combination-fun node)))))
	   (when (leaf-refs (elt (lambda-vars fun)
				 (position arg (basic-combination-args node))))
	     (barf "Flushed arg for referenced var in ~S." node))))))

     (let ((dest (continuation-dest (node-cont node))))
       (when (and (return-p dest)
		  (eq (basic-combination-kind node) :local)
		  (not (eq (lambda-tail-set (combination-lambda node))
			   (lambda-tail-set (return-lambda dest)))))
	 (barf "Tail local call to function with different tail set:~%  ~S"
	       node))))
    (cif
     (check-dest (if-test node) node)
     (unless (eq (block-last (node-block node)) node)
       (barf "IF not at block end: ~S" node)))
    (cset
     (check-dest (set-value node) node))
    (bind
     (check-function-reached (bind-lambda node) node))
    (creturn
     (check-function-reached (return-lambda node) node)
     (check-dest (return-result node) node)
     (unless (eq (block-last (node-block node)) node)
       (barf "RETURN not at block end: ~S" node)))
    (entry
     (unless (member node (lambda-entries (node-home-lambda node)))
       (barf "~S not in Entries for its home lambda." node))
     (dolist (exit (entry-exits node))
       (unless (node-deleted exit)
	 (check-node-reached node))))
    (exit
     (let ((entry (exit-entry node))
	   (value (exit-value node)))
       (cond (entry
	      (check-node-reached entry)
	      (unless (member node (entry-exits entry))
		(barf "~S not in its ENTRY's EXITS." node))
	      (when value
		(check-dest value node)))
	     (t
	      (when value
		(barf "~S has VALUE but no ENTRY." node)))))))
       
  (undefined-value))


;;;; IR2 consistency checking:


;;; Check-TN-Refs  --  Internal
;;;
;;;    Check for some kind of consistency in some Refs linked together by
;;; TN-Ref-Across.  VOP is the VOP that the references are in.  Write-P is the
;;; value of Write-P that should be present.  Count is the minimum number of
;;; operands expected.  If More-P is true, then any larger number will also be
;;; accepted.  What is a string describing the kind of operand in error
;;; messages.
;;;
(defun check-tn-refs (refs vop write-p count more-p what)
  (let ((vop-refs (vop-refs vop)))
    (do ((ref refs (tn-ref-across ref))
	 (num 0 (1+ num)))
	((null ref)
	 (when (< num count)
	   (barf "Should be at least ~D ~A in ~S, but are only ~D."
		 count what vop num))
	 (when (and (not more-p) (> num count))
	   (barf "Should be ~D ~A in ~S, but are ~D."
		 count what vop num)))
      (unless (eq (tn-ref-vop ref) vop)
	(barf "VOP is ~S isn't ~S." ref vop))
      (unless (eq (tn-ref-write-p ref) write-p)
	(barf "Write-P in ~S isn't ~S." vop write-p))
      (unless (find-in #'tn-ref-next-ref ref vop-refs)
	(barf "~S not found in Refs for ~S." ref vop))
      (unless (find-in #'tn-ref-next ref
		       (if (tn-ref-write-p ref)
			   (tn-writes (tn-ref-tn ref))
			   (tn-reads (tn-ref-tn ref))))
	(barf "~S not found in reads/writes for its TN." ref))

      (let ((target (tn-ref-target ref)))
	(when target
	  (unless (eq (tn-ref-write-p target) (not (tn-ref-write-p ref)))
	    (barf "Target for ~S isn't complementary write-p." ref))
	  (unless (find-in #'tn-ref-next-ref target vop-refs)
	    (barf "Target for ~S isn't in Refs for ~S." ref vop)))))))


;;; Check-VOP-Refs  --  Internal
;;;
;;;    Verify the sanity of the VOP-Refs slot in VOP.  This involves checking
;;; that each referenced TN appears as an argument, result or temp, and also
;;; basic checks for the plausibility of the specified ordering of the refs.
;;; 
(defun check-vop-refs (vop)
  (declare (type vop vop))
  (do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
      ((null ref))
    (cond
     ((find-in #'tn-ref-across ref (vop-args vop)))
     ((find-in #'tn-ref-across ref (vop-results vop)))
     ((not (eq (tn-ref-vop ref) vop))
      (barf "VOP in ~S isn't ~S." ref vop))
     ((find-in #'tn-ref-across ref (vop-temps vop)))
     ((tn-ref-write-p ref)
      (barf "Stray ref that isn't a read: ~S." ref))
     (t
      (let* ((tn (tn-ref-tn ref))
	     (temp (find-in #'tn-ref-across tn (vop-temps vop)
			    :key #'tn-ref-tn)))
	(unless temp
	  (barf "Stray ref with no corresponding temp write: ~S." ref))
	(unless (find-in #'tn-ref-next-ref temp (tn-ref-next-ref ref))
	  (barf "Read is after write for temp ~S in refs of ~S."
		tn vop))))))
  (undefined-value))


;;; Check-IR2-Block-Consistency  --  Internal
;;;
;;;    Check the basic sanity of the VOP linkage, then call some other
;;; functions to check on the TN-Refs.  We grab some info out of the VOP-Info
;;; to tell us what to expect.
;;; [### Check that operand type restrictions are met?]
;;;
(defun check-ir2-block-consistency (2block)
  (declare (type ir2-block 2block))
  (do ((vop (ir2-block-start-vop 2block)
	    (vop-next vop))
       (prev nil vop))
      ((null vop)
       (unless (eq prev (ir2-block-last-vop 2block))
	 (barf "Last VOP in ~S shoule be ~S." 2block prev)))
    (unless (eq (vop-prev vop) prev)
      (barf "Prev in ~S should be ~S." vop prev))

    (unless (eq (vop-block vop) 2block)
      (barf "Block in ~S should be ~S." vop 2block))

    (check-vop-refs vop)

    (let* ((info (vop-info vop))
	   (atypes (template-arg-types info))
	   (rtypes (template-result-types info)))
      (check-tn-refs (vop-args vop) vop nil
		     (count-if-not #'(lambda (x)
				       (and (consp x)
					    (eq (car x) :constant)))
				   atypes)
		     (template-more-args-type info) "args")
      (check-tn-refs (vop-results vop) vop t
		     (if (eq rtypes :conditional) 0 (length rtypes))
		     (template-more-results-type info) "results")
      (check-tn-refs (vop-temps vop) vop t 0 t "temps")
      (unless (= (length (vop-codegen-info vop))
		 (template-info-arg-count info))
	(barf "Wrong number of codegen info args in ~S." vop))))
  (undefined-value))


;;; Check-IR2-Consistency  --  Interface
;;;
;;;    Check stuff about the IR2 representation of COMPONENT.  This assumes the
;;; sanity of the basic flow graph.
;;;
;;; [### Also grovel global TN data structures?  Assume pack not
;;; done yet?  Have separate CHECK-TN-CONSISTENCY for pre-pack and
;;; CHECK-PACK-CONSISTENCY for post-pack?]
;;;
(defun check-ir2-consistency (component)
  (declare (type component component))
  (do-ir2-blocks (block component)
    (check-ir2-block-consistency block))
  (undefined-value))


;;;; Lifetime analysis checking:

;;; Pre-Pack-TN-Stats  --  Interface
;;;
;;;    Dump some info about how many TNs there, and what the conflicts data
;;; structures are like.
;;;
(defun pre-pack-tn-stats (component &optional (stream *compiler-error-output*))
  (declare (type component component))
  (let ((wired 0)
	(global 0)
	(local 0)
	(confs 0)
	(unused 0)
	(const 0)
	(temps 0)
	(environment 0)
	(comp 0))
    (do-packed-tns (tn component)
      (let ((reads (tn-reads tn))
	    (writes (tn-writes tn)))
	(when (and reads writes
		   (not (tn-ref-next reads)) (not (tn-ref-next writes))
		   (eq (tn-ref-vop reads) (tn-ref-vop writes)))
	  (incf temps)))
      (when (tn-offset tn)
	(incf wired))
      (unless (or (tn-reads tn) (tn-writes tn))
	(incf unused))
      (cond ((eq (tn-kind tn) :component)
	     (incf comp))
	    ((tn-global-conflicts tn)
	     (case (tn-kind tn)
	       ((:environment :debug-environment) (incf environment))
	       (t (incf global)))
	     (do ((conf (tn-global-conflicts tn)
			(global-conflicts-tn-next conf)))
		 ((null conf))
	       (incf confs)))
	    (t
	     (incf local))))

    (do ((tn (ir2-component-constant-tns (component-info component))
	     (tn-next tn)))
	((null tn))
      (incf const))

    (format stream
     "~%TNs: ~D local, ~D temps, ~D constant, ~D env, ~D comp, ~D global.~@
       Wired: ~D, Unused: ~D.  ~D block~:P, ~D global conflict~:P.~%"
       local temps const environment comp global wired unused
       (ir2-block-count component)
       confs))
  (undefined-value))


;;; Check-More-TN-Entry  --  Internal
;;;
;;;    If the entry in Local-TNs for TN in BLOCK is :MORE, then do some checks
;;; for the validity of the usage.
;;;
(defun check-more-tn-entry (tn block)
  (let* ((vop (ir2-block-start-vop block))
	 (info (vop-info vop)))
    (macrolet ((frob (more-p ops)
		 `(and (,more-p info)
		       (find-in #'tn-ref-across tn (,ops vop)
				:key #'tn-ref-tn))))
      (unless (and (eq vop (ir2-block-last-vop block))
		   (or (frob template-more-args-type vop-args)
		       (frob template-more-results-type vop-results)))
	(barf "Strange :More LTN entry for ~S in ~S." tn block))))
  (undefined-value))


;;; Check-TN-Conflicts  --  Internal
;;;
(defun check-tn-conflicts (component)
  (do-packed-tns (tn component)
    (unless (or (not (eq (tn-kind tn) :normal))
		(tn-reads tn)
		(tn-writes tn))
      (barf "No references to ~S." tn))

    (unless (tn-sc tn) (barf "~S has no SC." tn))

    (let ((conf (tn-global-conflicts tn))
	  (kind (tn-kind tn)))
      (cond
       ((eq kind :component)
	(unless (member tn (ir2-component-component-tns
			    (component-info component)))
	  (barf "~S not in Component-TNs for ~S." tn component)))
       (conf
	(do ((conf conf (global-conflicts-tn-next conf))
	     (prev nil conf))
	    ((null conf))
	  (unless (eq (global-conflicts-tn conf) tn)
	    (barf "TN in ~S should be ~S." conf tn))

	  (unless (eq (global-conflicts-kind conf) :live)
	    (let* ((block (global-conflicts-block conf))
		   (ltn (svref (ir2-block-local-tns block)
			       (global-conflicts-number conf))))
	      (cond ((eq ltn tn))
		    ((eq ltn :more) (check-more-tn-entry tn block))
		    (t
		     (barf "~S wrong in LTN map for ~S." conf tn)))))

	  (when prev
	    (unless (> (ir2-block-number (global-conflicts-block conf))
		       (ir2-block-number (global-conflicts-block prev)))
	      (barf "~S and ~S out of order." prev conf)))))
       ((member (tn-kind tn) '(:constant :specified-save)))
       (t
	(let ((local (tn-local tn)))
	  (unless local
	    (barf "~S has no global conflicts, but isn't local either." tn))
	  (unless (eq (svref (ir2-block-local-tns local)
			     (tn-local-number tn))
		      tn)
	    (barf "~S wrong in LTN map." tn))
	  (do ((ref (tn-reads tn) (tn-ref-next ref)))
	      ((null ref))
	    (unless (eq (vop-block (tn-ref-vop ref)) local)
	      (barf "~S has references in blocks other than its Local block."
		    tn)))
	  (do ((ref (tn-writes tn) (tn-ref-next ref)))
	      ((null ref))
	    (unless (eq (vop-block (tn-ref-vop ref)) local)
	      (barf "~S has references in blocks other than its Local block."
		    tn))))))))
  (undefined-value))


;;; Check-Block-Conflicts  --  Internal
;;;
(defun check-block-conflicts (component)
  (do-ir2-blocks (block component)
    (do ((conf (ir2-block-global-tns block)
	       (global-conflicts-next conf))
	 (prev nil conf))
	((null conf))
      (when prev
	(unless (> (tn-number (global-conflicts-tn conf))
		   (tn-number (global-conflicts-tn prev)))
	  (barf "~S and ~S out of order in ~S." prev conf block)))
      
      (unless (find-in #'global-conflicts-tn-next
		       conf
		       (tn-global-conflicts
			(global-conflicts-tn conf)))
	(barf "~S missing from global conflicts of its TN." conf)))
    
    (let ((map (ir2-block-local-tns block)))
      (dotimes (i (ir2-block-local-tn-count block))
	(let ((tn (svref map i)))
	  (unless (or (eq tn :more)
		      (null tn)
		      (tn-global-conflicts tn)
		      (eq (tn-local tn) block))
	    (barf "Strange TN ~S in LTN map for ~S." tn block)))))))


;;; Check-Environment-Lifetimes  --  Internal
;;;
;;;    All TNs live at the beginning of an environment must be passing
;;; locations associated with that environment.  We make an exception for wired
;;; TNs in XEP functions, since we randomly reference wired TNs to access the
;;; full call passing locations.
;;;
(defun check-environment-lifetimes (component)
  (dolist (fun (component-lambdas component))
    (let* ((env (lambda-environment fun))
	   (2env (environment-info env))
	   (vars (lambda-vars fun))
	   (closure (ir2-environment-environment 2env))
	   (pc (ir2-environment-return-pc-pass 2env))
	   (fp (ir2-environment-old-fp 2env))
	   (2block (block-info
		    (node-block
		     (lambda-bind
		      (environment-function env))))))
      (do ((conf (ir2-block-global-tns 2block)
		 (global-conflicts-next conf)))
	  ((null conf))
	(let ((tn (global-conflicts-tn conf)))
	  (unless (or (eq (global-conflicts-kind conf) :write)
		      (eq tn pc)
		      (eq tn fp)
		      (and (external-entry-point-p fun)
			   (tn-offset tn))
		      (member (tn-kind tn) '(:environment :debug-environment))
		      (member tn vars :key #'leaf-info)
		      (member tn closure :key #'cdr))
	    (barf "Strange TN live at head of ~S: ~S." env tn))))))
  (undefined-value))


;;; Check-Life-Consistency  --  Interface
;;;
;;;    Check for some basic sanity in the TN conflict data structures, and also
;;; check that no TNs are unexpectedly live at environment entry.
;;;
(defun check-life-consistency (component)
  (check-tn-conflicts component)
  (check-block-conflicts component)
  (check-environment-lifetimes component))


;;;; Pack consistency checking:

;;; CHECK-PACK-CONSISTENCY  --  Interface
;;;
(defun check-pack-consistency (component)
  (flet ((check (scs ops)
	   (do ((scs scs (cdr scs))
		(op ops (tn-ref-across op)))
	       ((null scs))
	     (let ((load-tn (tn-ref-load-tn op)))
	       (unless (eq (svref (car scs)
				  (sc-number
				   (tn-sc
				    (or load-tn (tn-ref-tn op)))))
			   t)
		 (barf "Operand restriction not satisfied: ~S." op))))))
    (do-ir2-blocks (block component)
      (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
	  ((null vop))
	(let ((info (vop-info vop)))
	  (check (vop-info-result-load-scs info) (vop-results vop))
	  (check (vop-info-arg-load-scs info) (vop-args vop))))))
  (undefined-value))


;;;; Data structure dumping routines:

;;; Continuation-Number, Number-Continuation, ID-TN, TN-ID  --  Interface
;;;
;;;    When we print Continuations and TNs, we assign them small numeric IDs so
;;; that we can get a handle on anonymous objects given a printout. Note that
;;; the variables are bound by the with-debug-counters macro which needs to be
;;; consistent with the definitions here.
;;;
(macrolet ((frob (counter vto vfrom fto ffrom)
	     `(progn
		(defvar ,vto (make-hash-table :test #'eq))
		(defvar ,vfrom (make-hash-table :test #'eql))
		(declaim (hash-table ,vto ,vfrom))
		(defvar ,counter 0)
		(declaim (fixnum ,counter))
		
		(defun ,fto (x)
		  (if (boundp ',vto)
		      (or (gethash x ,vto)
			  (let ((num (incf ,counter)))
			    (setf (gethash num ,vfrom) x)
			    (setf (gethash x ,vto) num)))
		      "???"))		;Silly placeholder value
		(defun ,ffrom (num)
		  (when (boundp ',vfrom)
		    (values (gethash num ,vfrom)))))))
  (frob *continuation-number*
        *continuation-numbers* *number-continuations*
        cont-num num-cont)
  (frob *tn-id*
        *tn-ids* *id-tns*
        tn-id id-tn)
  (frob *label-id*
        *id-labels* *label-ids*
        label-id id-label))

;;; Print-Leaf  --  Internal
;;;
;;;    Print out a terse one-line description of a leaf.
;;;
(defun print-leaf (leaf &optional (stream *standard-output*))
  (declare (type leaf leaf) (type stream stream))
  (etypecase leaf
    (lambda-var (prin1 (leaf-name leaf) stream))
    (constant (format stream "'~S" (constant-value leaf)))
    (global-var
     (format stream "~S {~A}" (leaf-name leaf) (global-var-kind leaf)))
    (clambda
      (format stream "lambda ~S ~S" (leaf-name leaf)
	      (mapcar #'leaf-name (lambda-vars leaf))))
    (optional-dispatch
     (format stream "optional-dispatch ~S" (leaf-name leaf)))
    (functional
     (assert (eq (functional-kind leaf) :top-level-xep))
     (format stream "TL-XEP ~S"
	     (let ((info (leaf-info leaf)))
	       (etypecase info
		 (entry-info (entry-info-name info))
		 (byte-lambda-info :byte-compiled-entry)))))))

;;; Block-Or-Lose  --  Interface
;;;
;;;    Attempt to find a block given some thing that has to do with it.
;;;
(defun block-or-lose (thing)
  (declare (values cblock))
  (ctypecase thing
    (cblock thing)
    (ir2-block (ir2-block-block thing))
    (vop (block-or-lose (vop-block thing)))
    (tn-ref (block-or-lose (tn-ref-vop thing)))
    (continuation (continuation-block thing))
    (node (node-block thing))
    (component (component-head thing))
#|    (cloop (loop-head thing))|#
    (integer (continuation-block (num-cont thing)))
    (functional (node-block (lambda-bind (main-entry thing))))
    (null (error "Bad thing: ~S." thing))
    (symbol (block-or-lose (gethash thing *free-functions*)))))


;;; Print-Continuation  --  Internal
;;;
;;;    Print cN.
;;;
(defun print-continuation (cont)
  (declare (type continuation cont))
  (format t " c~D" (cont-num cont))
  (undefined-value))


;;; Print-Nodes  --  Interface
;;;
;;;    Print out the nodes in Block in a format oriented toward representing
;;; what the code does. 
;;;
(defun print-nodes (block)
  (setq block (block-or-lose block))
  (format t "~%block start c~D" (cont-num (block-start block)))

  (let ((last (block-last block)))
    (terpri)
    (do ((cont (block-start block) (node-cont (continuation-next cont))))
	(())
      (let ((node (continuation-next cont)))
	(format t "~3D: " (cont-num (node-cont node)))
	(etypecase node
	  (ref (print-leaf (ref-leaf node)))
	  (basic-combination
	   (let ((kind (basic-combination-kind node)))
	     (format t "~(~A ~A~) c~D"
		     (if (function-info-p kind) "known" kind)
		     (type-of node)
		     (cont-num (basic-combination-fun node)))
	     (dolist (arg (basic-combination-args node))
	       (if arg
		   (print-continuation arg)
		   (format t " <none>")))))
	  (cset
	   (write-string "set ")
	   (print-leaf (set-var node))
	   (print-continuation (set-value node)))
	  (cif
	   (format t "if c~D" (cont-num (if-test node)))
	   (print-continuation (block-start (if-consequent node)))
	   (print-continuation (block-start (if-alternative node))))
	  (bind
	   (write-string "bind ")
	   (print-leaf (bind-lambda node)))
	  (creturn
	   (format t "return c~D " (cont-num (return-result node)))
	   (print-leaf (return-lambda node)))
	  (entry
	   (format t "entry ~S" (entry-exits node)))
	  (exit
	   (let ((value (exit-value node)))
	     (cond (value
		    (format t "exit c~D" (cont-num value)))
		   ((exit-entry node)
		    (format t "exit <no value>"))
		   (t
		    (format t "exit <degenerate>"))))))
	(terpri)
	(when (eq node last) (return)))))

  (let ((succ (block-succ block)))
    (format t "successors~{ c~D~}~%"
	    (mapcar #'(lambda (x) (cont-num (block-start x))) succ)))
  (values))


;;; Print-TN  --  Internal
;;;
;;;    Print a useful representation of a TN.  If the TN has a leaf, then do a
;;; PRINT-LEAF on that, otherwise print a generated ID.
;;;
(defun print-tn (tn &optional (stream *standard-output*))
  (declare (type tn tn))
  (let ((leaf (tn-leaf tn)))
    (cond (leaf
	   (print-leaf leaf stream)
	   (format stream "!~D" (tn-id tn)))
	  (t
	   (format stream "t~D" (tn-id tn))))
    (when (and (tn-sc tn) (tn-offset tn))
      (format stream "[~A]" (location-print-name tn)))))


;;; Print-Operands  --  Internal
;;;
;;;    Print the TN-REFS representing some operands to a VOP, linked by
;;; TN-REF-ACROSS.
;;;
(defun print-operands (refs)
  (declare (type (or tn-ref null) refs))
  (pprint-logical-block (*standard-output* nil)
    (do ((ref refs (tn-ref-across ref)))
	((null ref))
      (let ((tn (tn-ref-tn ref))
	    (ltn (tn-ref-load-tn ref)))
	(cond ((not ltn)
	       (print-tn tn))
	      (t
	       (print-tn tn)
	       (princ (if (tn-ref-write-p ref) #\< #\>))
	       (print-tn ltn)))
	(princ #\space)
	(pprint-newline :fill)))))


;;; Print-Vop -- internal
;;;
;;; Print the VOP, putting args, info and results on separate lines, if
;;; necessary.
;;;
(defun print-vop (vop)
  (pprint-logical-block (*standard-output* nil)
    (princ (vop-info-name (vop-info vop)))
    (princ #\space)
    (pprint-indent :current 0)
    (print-operands (vop-args vop))
    (pprint-newline :linear)
    (when (vop-codegen-info vop)
      (princ (with-output-to-string (stream)
	       (let ((*print-level* 1)
		     (*print-length* 3))
		 (format stream "{~{~S~^ ~}} " (vop-codegen-info vop)))))
      (pprint-newline :linear))
    (when (vop-results vop)
      (princ "=> ")
      (print-operands (vop-results vop))))
  (terpri))

;;; Print-IR2-Block  --  Internal
;;;
;;;    Print the VOPs in the specified IR2 block.
;;;
(defun print-ir2-block (block)
  (declare (type ir2-block block))
  (cond
   ((eq (block-info (ir2-block-block block)) block)
    (format t "~%IR2 block start c~D~%"
	    (cont-num (block-start (ir2-block-block block))))
    (let ((label (ir2-block-%label block)))
      (when label
	(format t "L~D:~%" (label-id label)))))
   (t
    (format t "<overflow>~%")))

  (do ((vop (ir2-block-start-vop block)
	    (vop-next vop))
       (number 0 (1+ number)))
      ((null vop))
    (format t "~D: " number)
    (print-vop vop)))


;;; Print-VOPs  --  Interface
;;;
;;;    Like Print-Nodes, but dumps the IR2 representation of the code in Block.
;;;
(defun print-vops (block)
  (setq block (block-or-lose block))
  (let ((2block (block-info block)))
    (print-ir2-block 2block)
    (do ((b (ir2-block-next 2block) (ir2-block-next b)))
	((not (eq (ir2-block-block b) block)))
      (print-ir2-block b)))
  (values))


;;; Print-IR2-Blocks  --  Interface
;;;
;;;    Scan the IR2 blocks in emission order.
;;;
(defun print-ir2-blocks (thing)
  (do-ir2-blocks (block (block-component (block-or-lose thing)))
    (print-ir2-block block))
  (values))


;;; Print-Blocks  --  Interface
;;;
;;;    Do a Print-Nodes on Block and all blocks reachable from it by successor
;;; links.
;;;
(defun print-blocks (block)
  (setq block (block-or-lose block))
  (do-blocks (block (block-component block) :both)
    (setf (block-flag block) nil))
  (labels ((walk (block)
	     (unless (block-flag block)
	       (setf (block-flag block) t)
	       (when (block-start block)
		 (print-nodes block))
	       (dolist (block (block-succ block))
		 (walk block)))))
    (walk block))
  (values))


;;; Print-All-Blocks  --  Interface
;;;
;;;    Print all blocks in Block's component in DFO.
;;;
(defun print-all-blocks (thing)
  (do-blocks (block (block-component (block-or-lose thing)))
    (handler-case (print-nodes block)
      (error (condition)
        (format t "~&~A...~%" condition))))
  (values))


(defvar *list-conflicts-table*)

;;; Add-Always-Live-TNs  --  Internal
;;;
;;;    Add all Always-Live TNs in Block to the conflicts.  TN is ignored when
;;; it appears in the global conflicts.
;;;
(defun add-always-live-tns (block tn)
  (declare (type ir2-block block) (type tn tn))
  (do ((conf (ir2-block-global-tns block)
	     (global-conflicts-next conf)))
      ((null conf))
    (when (eq (global-conflicts-kind conf) :live)
      (let ((btn (global-conflicts-tn conf)))
	(unless (eq btn tn)
	  (setf (gethash btn *list-conflicts-table*) t)))))
  (undefined-value))


;;; Add-All-Local-TNs  --  Internal
;;;
;;;    Add all local TNs in block to the conflicts.
;;;
(defun add-all-local-tns (block)
  (declare (type ir2-block block))
  (let ((ltns (ir2-block-local-tns block)))
    (dotimes (i (ir2-block-local-tn-count block))
      (setf (gethash (svref ltns i) *list-conflicts-table*) t)))
  (undefined-value))


;;; Listify-Conflicts-Table  --  Internal
;;;
;;;    Make a list out of all of the recorded conflicts.
;;;
(defun listify-conflicts-table ()
  (collect ((res))
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (when k
		   (res k)))
	     *list-conflicts-table*)
    (res)))
  

;;;  List-Conflicts  --  Interface
;;;
(defun list-conflicts (tn)
  "Return a list of a the TNs that conflict with TN.  Sort of, kind of.  For
  debugging use only.  Probably doesn't work on :COMPONENT TNs."
  (assert (member (tn-kind tn) '(:normal :environment :debug-environment)))
  (let ((confs (tn-global-conflicts tn)))
    (cond (confs
	   (let ((*list-conflicts-table* (make-hash-table :test #'eq)))
	     (do ((conf confs (global-conflicts-tn-next conf)))
		 ((null conf))
	       (let ((block (global-conflicts-block conf)))
		 (add-always-live-tns block tn)
		 (if (eq (global-conflicts-kind conf) :live)
		     (add-all-local-tns block)
		     (let ((bconf (global-conflicts-conflicts conf))
			   (ltns (ir2-block-local-tns block)))
		       (dotimes (i (ir2-block-local-tn-count block))
			 (when (/= (sbit bconf i) 0)
			   (setf (gethash (svref ltns i)
					  *list-conflicts-table*)
				 t)))))))
	     (listify-conflicts-table)))
	  (t
	   (let* ((block (tn-local tn))
		  (ltns (ir2-block-local-tns block))
		  (confs (tn-local-conflicts tn)))
	     (collect ((res))
	       (dotimes (i (ir2-block-local-tn-count block))
		 (when (/= (sbit confs i) 0)
		   (let ((tn (svref ltns i)))
		     (when (and tn (not (eq tn :more))
				(not (tn-global-conflicts tn)))
		       (res tn)))))
	       (do ((gtn (ir2-block-global-tns block)
			 (global-conflicts-next gtn)))
		   ((null gtn))
		 (when (or (eq (global-conflicts-kind gtn) :live)
			   (/= (sbit confs (global-conflicts-number gtn)) 0))
		   (res (global-conflicts-tn gtn))))
	       (res)))))))


;;; Nth-VOP  --  Interface
;;;
(defun nth-vop (thing n)
  "Return the Nth VOP in the IR2-Block pointed to by Thing."
  (let ((block (block-info (block-or-lose thing))))
    (do ((i 0 (1+ i))
	 (vop (ir2-block-start-vop block) (vop-next vop)))
	((= i n) vop))))
