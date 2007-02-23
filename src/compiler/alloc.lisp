;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/alloc.lisp,v 1.14 2003/08/11 14:22:44 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Some storage allocation hacks for the compiler.
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package "C")


;;; A hack we we to defeat compile-time type checking in deinitializing slots
;;; where there isn't any convenient legal value.
;;;
(defvar *undefined* '**undefined**)


;;; ZAP-IN  --  Internal
;;;
;;;    A convenient macro for iterating over linked lists when we are
;;; clobbering the next link as we go.
;;;
(defmacro zap-in ((var in slot) &body body)
  (let ((next (gensym)))
    `(let ((,var ,in) ,next)
       (when ,var
	 (loop
	   (setq ,next (,slot ,var))
	   ,@body
	   (unless ,next (return))
	   (setq ,var ,next))))))


;;; DEFALLOCATERS  --  Internal
;;;
;;;    Define some structure freelisting operations.
;;;
#-gencgc
(defmacro defallocators (&rest specs)
  "defallocators {((name lambda-list [real-lambda-list]) thread-slot
                   (deinit-form*)
		   (reinit-form*))}*"
  (collect ((hook-forms)
	    (forms))
    (dolist (spec specs)
      (let* ((names (first spec))
	     (name (first names))
	     (lambda-list (second names))
	     (real-lambda-list (or (third names) lambda-list))
	     (fun-name (symbolicate "MAKE-" name))
	     (unfun-name (symbolicate "UNMAKE-" name))
	     (real-fun-name (symbolicate "REALLY-MAKE-" name))
	     (var-name (symbolicate "*" name "-FREE-LIST*"))
	     (slot (second spec)))
	(forms `(proclaim '(inline ,fun-name ,unfun-name)))
	(forms `(defvar ,var-name nil))
	(forms `(defun ,fun-name ,lambda-list
		  (declare (optimize (safety 0)))
		  (let ((structure ,var-name))
		    (cond (structure
			   (setq ,var-name (,slot structure))
			   ,@(fourth spec)
			   structure)
			  (t
			   (,real-fun-name ,@real-lambda-list))))))
	
	(forms `(defun ,unfun-name (structure)
		  (declare (optimize (safety 0)))
		  ,@(third spec)
		  #+nil
		  (when (find-in #',slot structure ,var-name)
		    (error "~S already deallocated!" structure))
		  (setf (,slot structure) ,var-name)
		  (setq ,var-name structure)))

	(hook-forms
	 `(progn
	    (zap-in (structure ,var-name ,slot)
	      (setf (,slot structure) nil))
	    (setq ,var-name nil)))))

    `(progn
       ,@(forms)
       (defun defallocator-deallocation-hook ()
	 (declare (optimize (safety 0)))
	 ,@(hook-forms))
       (pushnew 'defallocator-deallocation-hook ext:*before-gc-hooks*))))

#+gencgc
(defmacro defallocators (&rest specs)
  "defallocators {((name lambda-list [real-lambda-list]) thread-slot
                   (deinit-form*)
		   (reinit-form*))}*"
  (collect ((forms))
    (dolist (spec specs)
      (destructuring-bind ((name lambda-list
				 &optional (real-lambda-list lambda-list))
			   thread-slot deinit-forms reinit-forms)
	  spec
	(declare (ignore thread-slot reinit-forms))
	(let ((fun-name (symbolicate "MAKE-" name))
	      (unfun-name (symbolicate "UNMAKE-" name))
	      (var-name (symbolicate "*" name "-FREE-LIST*"))
	      (real-fun-name (symbolicate "REALLY-MAKE-" name)))
	  (forms `(proclaim '(inline ,fun-name ,unfun-name)))
	  (forms `(defvar ,var-name nil))
	  (forms `(defun ,fun-name ,lambda-list
		    (,real-fun-name ,@real-lambda-list)))
	  (forms `(defun ,unfun-name (structure)
		    (declare (optimize (safety 0)))
		    ,@deinit-forms)))))
    `(progn
       ,@(forms))))

(defmacro node-deinits ()
  '(progn
     (setf (node-derived-type structure) *wild-type*)
     (setf (node-lexenv structure) *undefined*)
     (setf (node-source-path structure) nil)
     (setf (node-reoptimize structure) t)
     (setf (node-cont structure) nil)
     (setf (node-prev structure) nil)
     (setf (node-tail-p structure) nil)))

(defmacro node-inits ()
  '(progn
     (setf (node-lexenv structure) *lexical-environment*)
     (setf (node-source-path structure) *current-path*)))


(defallocators
  ((continuation (&optional dest) (dest)) continuation-info
   ((setf (continuation-kind structure) :unused)
    (setf (continuation-dest structure) nil)
    (setf (continuation-next structure) nil)
    (setf (continuation-asserted-type structure) *wild-type*)
    (setf (continuation-%derived-type structure) nil)
    (setf (continuation-use structure) nil)
    (setf (continuation-refs structure) nil)
    (setf (continuation-block structure) nil)
    (setf (continuation-reoptimize structure) t)
    (setf (continuation-%type-check structure) t))
   ((setf (continuation-info structure) nil)
    (setf (continuation-dest structure) dest)))
  
  ((block (start)) block-next
   ((setf (block-pred structure) nil)
    (setf (block-succ structure) nil)
    (setf (block-start structure) nil)
    (setf (block-start-uses structure) nil)
    (setf (block-last structure) nil)
    (setf (block-next structure) nil)
    (setf (block-prev structure) nil)
    (setf (block-flags structure)
	  (block-attributes reoptimize flush-p type-check type-asserted
			    test-modified))
    (setf (block-kill structure) nil)
    (setf (block-gen structure) nil)
    (setf (block-in structure) nil)
    (setf (block-out structure) nil)
    (setf (block-flag structure) nil)
    (setf (block-info structure) nil)
    (setf (block-test-constraint structure) nil))
   ((setf (block-component structure) *current-component*)
    (setf (block-start structure) start)))
  
  ((ref (derived-type leaf)) node-source-path
   ((node-deinits)
    (setf (ref-leaf structure) *undefined*))
   ((node-inits)
    (setf (node-derived-type structure) derived-type)
    (setf (ref-leaf structure) leaf)))
  
  ((combination (fun)) node-source-path
   ((node-deinits)
    (setf (basic-combination-fun structure) *undefined*)
    (setf (basic-combination-args structure) nil)
    (setf (basic-combination-kind structure) :full)
    (setf (basic-combination-info structure) nil))
   ((node-inits)
    (setf (basic-combination-fun structure) fun)))
  
  ((ir2-block (block)) ir2-block-next
   ((setf (ir2-block-number structure) nil)
    (setf (ir2-block-block structure) *undefined*)
    (setf (ir2-block-prev structure) nil)
    (setf (ir2-block-pushed structure) nil)
    (setf (ir2-block-popped structure) nil)
    (setf (ir2-block-start-stack structure) nil)
    (setf (ir2-block-end-stack structure) nil)
    (setf (ir2-block-start-vop structure) nil)
    (setf (ir2-block-last-vop structure) nil)
    (setf (ir2-block-local-tn-count structure) 0)
    (let ((ltns (ir2-block-local-tns structure)))
      (dotimes (i local-tn-limit)
	(setf (svref ltns i) nil)))
    (clear-ltn-bit-vector (ir2-block-written structure))
    (clear-ltn-bit-vector (ir2-block-live-in structure))
    (setf (ir2-block-global-tns structure) nil)
    (setf (ir2-block-%label structure) nil)
    (setf (ir2-block-locations structure) nil))
   ((setf (ir2-block-next structure) nil)
    (setf (ir2-block-block structure) block)))
  
  ((vop (block node info args results)) vop-next
   ((setf (vop-block structure) *undefined*)
    (setf (vop-prev structure) nil)
    (setf (vop-args structure) nil)
    (setf (vop-results structure) nil)
    (setf (vop-temps structure) nil)
    (setf (vop-refs structure) nil)
    (setf (vop-codegen-info structure) nil)
    (setf (vop-node structure) nil)
    (setf (vop-save-set structure) nil))
   ((setf (vop-next structure) nil)
    (setf (vop-block structure) block)
    (setf (vop-node structure) node)
    (setf (vop-info structure) info)
    (setf (vop-args structure) args)
    (setf (vop-results structure) results)))
  
  ((tn-ref (tn write-p)) tn-ref-next
   ((setf (tn-ref-tn structure) *undefined*)
    (setf (tn-ref-vop structure) nil)
    (setf (tn-ref-next-ref structure) nil)
    (setf (tn-ref-across structure) nil)
    (setf (tn-ref-target structure) nil)
    (setf (tn-ref-load-tn structure) nil))
   ((setf (tn-ref-next structure) nil)
    (setf (tn-ref-tn structure) tn)
    (setf (tn-ref-write-p structure) write-p)))
  
  ((tn (number kind primitive-type sc)) tn-next
   ((setf (tn-leaf structure) nil)
    (setf (tn-reads structure) nil)
    (setf (tn-writes structure) nil)
    (setf (tn-next* structure) nil)
    (setf (tn-local structure) nil)
    (setf (tn-local-number structure) nil)
    ;;
    ;; Has been clobbered in global TNs...
    (if (tn-global-conflicts structure)
	(setf (tn-local-conflicts structure)
	      (make-array local-tn-limit :element-type 'bit
			  :initial-element 0))
	(clear-ltn-bit-vector (tn-local-conflicts structure)))
    
    (setf (tn-global-conflicts structure) nil)
    (setf (tn-current-conflict structure) nil)
    (setf (tn-save-tn structure) nil)
    (setf (tn-offset structure) nil)
    (setf (tn-environment structure) nil)
    (setf (tn-cost structure) 0))
   ((setf (tn-next structure) nil)
    (setf (tn-number structure) number)
    (setf (tn-kind structure) kind)
    (setf (tn-primitive-type structure) primitive-type)
    (setf (tn-sc structure) sc)))
  
  ((global-conflicts (kind tn block number)) global-conflicts-next
   ((setf (global-conflicts-block structure) *undefined*)
    (clear-ltn-bit-vector (global-conflicts-conflicts structure))
    (setf (global-conflicts-tn structure) *undefined*)
    (setf (global-conflicts-tn-next structure) nil))
   ((setf (global-conflicts-next structure) nil)
    (setf (global-conflicts-kind structure) kind)
    (setf (global-conflicts-tn structure) tn)
    (setf (global-conflicts-block structure) block)
    (setf (global-conflicts-number structure) number))))


;;; NUKE-IR2-COMPONENT  --  Interface
;;;
;;;    Destroy the connectivity of the IR2 as much as possible in an attempt to
;;; reduce GC lossage.
;;;
(defun nuke-ir2-component (component)
  (declare (type component component))
  (zap-in (block (block-info (component-head component)) ir2-block-next)
    (zap-in (conf (ir2-block-global-tns block) global-conflicts-next)
      (unmake-global-conflicts conf))

    (zap-in (vop (ir2-block-start-vop block) vop-next)
      (zap-in (ref (vop-refs vop) tn-ref-next-ref)
	(let ((ltn (tn-ref-load-tn ref)))
	  (when ltn
	    (unmake-tn ltn)))
	(unmake-tn-ref ref))
      (unmake-vop vop))
    (unmake-ir2-block block))
    
  (let ((2comp (component-info component)))
    (macrolet ((blast (slot)
		 `(progn
		    (zap-in (tn (,slot 2comp) tn-next)
		      (unmake-tn tn))
		    (setf (,slot 2comp) nil))))
      (blast ir2-component-normal-tns)
      (blast ir2-component-restricted-tns)
      (blast ir2-component-wired-tns)
      (blast ir2-component-constant-tns))
    (setf (ir2-component-component-tns 2comp) nil)
    (setf (ir2-component-nfp 2comp) nil)
    (setf (ir2-component-values-receivers 2comp) nil)
    (setf (ir2-component-constants 2comp) '#())
    (setf (ir2-component-format 2comp) nil)
    (setf (ir2-component-entries 2comp) nil))

  (undefined-value))


;;; MACERATE-IR1-COMPONENT  --  Interface
;;;
(defun macerate-ir1-component (component)
  (declare (type component component) (optimize (safety 0)))
  (zap-in (block (component-head component) block-next)
    (when (block-start block)
      (let ((cont (block-start block))
	    (last-cont (node-cont (block-last block)))
	    next-cont
	    node)
	(loop
	  (setq node (continuation-next cont))
	  (setq next-cont (node-cont node))
	  (typecase node
	    (ref (unmake-ref node))
	    (combination (unmake-combination node))
	    (t
	     (let ((structure node))
	       (node-deinits))))
	  (unmake-continuation cont)
	  (when (eq next-cont last-cont)
	    (when (eq (continuation-block next-cont) block)
	      (unmake-continuation next-cont))
	    (return))
	  (setq cont next-cont))))
    (unmake-block block))

  (dolist (fun (component-lambdas component))
    (setf (leaf-refs fun) nil)
    (let ((tails (lambda-tail-set fun)))
      (when tails
	(setf (tail-set-info tails) nil)))
    (let ((env (lambda-environment fun)))
      (setf (environment-info env) nil)
      (setf (environment-function env) *undefined*)
      (dolist (nlx (environment-nlx-info env))
	(setf (nlx-info-info nlx) nil)))
    (macrolet ((frob (fun)
		 `(progn
		    (dolist (var (lambda-vars ,fun))
		      (setf (leaf-refs var) nil)
		      (setf (basic-var-sets var) nil)
		      (setf (leaf-info var) nil))
		    (setf (lambda-vars ,fun) nil)
		    (setf (lambda-environment ,fun) nil))))
      (frob fun)
      (dolist (let (lambda-lets fun))
	(frob let)
	(setf (lambda-home let) nil))
      (setf (lambda-lets fun) nil))))
