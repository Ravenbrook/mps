;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ir1final.lisp,v 1.24 2004/12/06 17:03:56 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file implements the IR1 finalize phase, which checks for various
;;; semantic errors.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")


;;; Note-Failed-Optimization  --  Internal
;;;
;;;    Give the user grief about optimizations that we weren't able to do.  It
;;; is assumed that they want to hear, or there wouldn't be any entries in the
;;; table.  If the node has been deleted or is no longer a known call, then do
;;; nothing; some other optimization must have gotten to it.
;;;
(defun note-failed-optimization (node failures)
  (declare (type combination node) (list failures))
  (unless (or (node-deleted node)
	      (not (function-info-p (combination-kind node))))
    (let ((*compiler-error-context* node))
      (dolist (failure failures)
	(let ((what (cdr failure))
	      (note (transform-note (car failure))))
	  (cond
	   ((consp what)
	    (efficiency-note "Unable to ~A because:~%~6T~?"
			     note (first what) (rest what)))
	   ((valid-function-use node what
				:argument-test #'types-intersect
				:result-test #'values-types-intersect)
	    (collect ((messages))
	      (flet ((frob (string &rest stuff)
		       (messages string)
		       (messages stuff)))
		(valid-function-use node what
				    :warning-function #'frob
				    :error-function #'frob))
	      
	      (efficiency-note "Unable to ~A due to type uncertainty:~@
	                      ~{~6T~?~^~&~}"
			       note (messages))))))))))


;;; FINALIZE-XEP-DEFINITION  --  Internal
;;;
;;; For each named function with an XEP, note the definition of that name, and
;;; add derived type information to the info environment.  We also delete the
;;; FUNCTIONAL from *FREE-FUNCTIONS* to eliminate the possibility that new
;;; references might be converted to it.
;;;
(defun finalize-xep-definition (fun)
  (let* ((leaf (functional-entry-function fun))
	 (name (leaf-name leaf))
	 (dtype (definition-type leaf)))
    (setf (leaf-type leaf) dtype)
    (when (ext:valid-function-name-p name)
      (let* ((where (info function where-from name))
	     (*compiler-error-context* (lambda-bind (main-entry leaf)))
	     (global-def (gethash name *free-functions*))
	     (global-p
	      (and (defined-function-p global-def)
		   (eq (defined-function-functional global-def) leaf))))
	(note-name-defined name :function)
	(when global-p
	  (remhash name *free-functions*))
	(ecase where
	  (:assumed
	   (let ((approx-type (info function assumed-type name)))
	     (when (and approx-type (function-type-p dtype))
	       (valid-approximate-type approx-type dtype))
	     (setf (info function type name) dtype)
	     (setf (info function assumed-type name) nil))
	   (setf (info function where-from name) :defined))
	  (:declared
	   (let ((type (info function type name)))
	     (when (and (function-type-p type)
			(function-type-p dtype))
	       (let ((type-returns (function-type-returns type))
		     (dtype-returns (function-type-returns dtype))
		     (*error-function* #'compiler-warning))
		 (unless (values-types-intersect type-returns dtype-returns)
		   (note-lossage "The result type from previous declaration:~%  ~S~@
				  conflicts with the result type:~%  ~S"
				 (type-specifier type-returns)
				 (type-specifier dtype-returns)))))))
	  (:defined
	   (when global-p
	     (setf (info function type name) dtype)))))))
  (undefined-value))


;;; NOTE-ASSUMED-TYPES  --  Internal
;;;
;;;    Find all calls in Component to assumed functions and update the assumed
;;; type information.  This is delayed until now so that we have the best
;;; possible information about the actual argument types.
;;;
(defun note-assumed-types (component name var)
  (when (and (eq (leaf-where-from var) :assumed)
	     (not (and (defined-function-p var)
		       (eq (defined-function-inlinep var) :notinline)))
	     (eq (info function where-from name) :assumed)
	     (eq (info function kind name) :function))
    (let ((atype (info function assumed-type name)))
      (dolist (ref (leaf-refs var))
	(let ((dest (continuation-dest (node-cont ref))))
	  (when (and (eq (block-component (node-block ref)) component)
		     (combination-p dest)
		     (eq (continuation-use (basic-combination-fun dest)) ref))
	    (setq atype (note-function-use dest atype)))))
      (setf (info function assumed-type name) atype))))


;;; IR1-FINALIZE  --  Interface
;;;
;;;    Do miscellaneous things that we want to do once all optimization has
;;; been done:
;;;  -- Record the derived result type before the back-end trashes the
;;;     flow graph.
;;;  -- Note definition of any entry points.
;;;  -- Note any failed optimizations.
;;; 
(defun ir1-finalize (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (case (functional-kind fun)
      (:external
       (finalize-xep-definition fun))
      ((nil)
       (setf (leaf-type fun) (definition-type fun)))))
  
  (maphash #'note-failed-optimization
	   (component-failed-optimizations component))
  
  (maphash #'(lambda (k v)
	       (note-assumed-types component k v))
	   *free-functions*)
  (undefined-value))
