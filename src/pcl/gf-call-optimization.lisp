;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

(file-comment "$Header: /project/cmucl/cvsroot/src/pcl/gf-call-optimization.lisp,v 1.6 2003/10/29 12:14:35 gerd Exp $")

(in-package "PCL")

;;; ***************************************************
;;; General Generic Function Call Optimization  *******
;;; ***************************************************
;;;
;;; This optimizes calls to generic functions to calls of effective
;;; method functions, which are stored in the pv-table cache of a
;;; method.
;;;
;;; Each method has a pv table associated with it, whose cache maps
;;; the wrappers of actual method arguments to pv-cells.  The cdr of
;;; these pv-cells is a vector of effective methods, one for each
;;; pv-optimized generic function call in the pv-table's call-list.
;;; The local variable .CALLS. is bound to the call vector in a method
;;; function.

(defvar *optimize-gf-calls-p* t)

(defmacro callsref (call-vector i)
  `(%svref ,call-vector ,i))

(define-walker-template call-optimization-barrier)
(defmacro call-optimization-barrier (x) x)

(defun optimize-gf-call (form required-method-parameters calls env)
  (flet ((make-call-optimization-barrier (form)
	   `(,(first form) (call-optimization-barrier ,(second form))
	      ,@(cddr form)))
	 (call-optimization-barrier-p (form)
	   (let ((arg (second form)))
	     (and (consp arg) (eq 'call-optimization-barrier (car arg))))))
    (when (and *optimize-gf-calls-p*
	       (eq *boot-state* 'complete)
	       (not (eq 'apply (first form)))
	       (not (call-optimization-barrier-p form)))
      (let ((call (make-pv-call form required-method-parameters env)))
	(when call
	  (let ((entry (assoc call calls :test #'equal))
		(pv-offset-form (list 'pv-offset -1)))
	    (when (null entry)
	      (push (setq entry (cons call nil)) (cdr calls)))
	    (push pv-offset-form (cdr entry))
	    ;;
	    (let* ((nreq (length (cdr call)))
		   (gf-info (gf-info (car call)))
		   ;;
		   ;; According to MAKE-FAST-METHOD-CALL-LAMBDA-LIST, a
		   ;; fast method function has a rest arg if the generic
		   ;; function's applyp flag is set.
		   (rest-p (gf-info-applyp gf-info))
		   (rest (when rest-p (nthcdr nreq (cdr form))))
		   (required-args (when rest-p (ldiff (cdr form) rest))))
	      (setq form
		    `(let ((.emf. (callsref .calls. ,pv-offset-form)))
		       (declare #.*optimize-speed*)
		       (cond ((fast-method-call-p .emf.)
			      ,@(if rest-p
				    `((invoke-fast-method-call
				       .emf. ,@required-args
				       (list ,@rest)))
				    `((invoke-fast-method-call
				       .emf. ,@(cdr form)))))
			     (.emf.
			      (funcall (lambda (&rest args)
					 (invoke-emf .emf. args))
				       ,@(cdr form)))
			     (t
			      ,(make-call-optimization-barrier form))))))))))
    form))

;;;
;;; Value is NIL if FORM is not optimizable.  Otherwise, a list
;;; (GF-NAME <pos0> <pos1> ...) is returned.  GF-NAME is the name of
;;; the generic function being called, and each <posN> is the index of
;;; a method parameter used as argument N to the generic function.
;;;
(defun make-pv-call (form required-params env)
  (let* ((gf-name (first form))
	 (gf-info (gf-info gf-name))
	 (nreq (gf-info-nreq gf-info)))
    (flet (;;
	   ;; If VAR is the name of a specialized method parameter,
	   ;; or a rebinding of it, return the method parameter's name,
	   ;; otherwise return NIL.
	   (specialized-method-parameter (var)
	     (let ((param (method-parameter var required-params env)))
	       (when param
		 (let ((cname (caddr (variable-declaration 'class param
							   env))))
		   (when (and cname (not (eq t cname)))
		     param))))))
      ;;
      ;; Check the generic function call argument list.  If all
      ;; arguments are required method parameter, and no method
      ;; parameter is used more than once, we can optimize the call.
      ;; Method parameters may not appear twice in the argument list
      ;; because we can only have one wrapper for each method
      ;; parameter the pv-table cache.
      (unless (case nreq
		(0 t)
		((1 2) (info-accessor-p gf-name))
		(t nil))
	(loop with optimizable-p = t
	      for arg in (rest form) and i below nreq
	      as param = (specialized-method-parameter arg)
	      as pos = (and param (posq param required-params))
	      when (or (null param) (member pos posns)) do
		(setq optimizable-p nil)
		(loop-finish)
	      collect pos into posns
	      finally
		(return (and optimizable-p (cons gf-name posns))))))))

(defun compute-calls (table call-list all-method-wrappers)
  ;;
  ;; Small quirk here: set this flag so that GET-METHOD-FUNCTION
  ;; won't try to use the PV cache while it's being computed.
  (letf (((pv-table-computing-cache-p table) t))
    (collect ((emfs))
      (dolist (call call-list (coerce (emfs) 'simple-vector))
	(emfs (compute-call call all-method-wrappers))))))

(defun compute-call (call all-method-wrappers)
  (let ((gf-name (car call)))
    ;;
    ;; We have to check that GF-NAME is actually a generic function
    ;; here.  There can concievably be cases where GF-NAME appears to
    ;; be a generic function when files are compiled, but isn't, or
    ;; isn't yet, when compiled files are loaded.
    (when (and (fboundp gf-name)
	       (generic-function-p (gdefinition gf-name)))
      (loop with all-wrappers =
	      (if (atom all-method-wrappers)
		  (list all-method-wrappers)
		  all-method-wrappers)
	      with gf = (gdefinition gf-name)
	      for wrapper-index in (cdr call)
	      as wrapper = (nth wrapper-index all-wrappers)
	      as class = (wrapper-class* wrapper)
	      as type = `(class-eq ,class)
	      collect wrapper into wrappers
	      collect class into classes
	      collect type into types
	      finally
	      (return (cache-miss-values-internal gf (gf-arg-info gf)
						  wrappers classes types
						  'caching))))))

(defun update-pv-calls-for-gf (gf &optional gf-removed-p)
  (when (eq *boot-state* 'complete)
    (let ((call-key `(call ,(generic-function-name gf))))
      (do-pv-tables (table call-key)
	(let ((cache (pv-table-cache table)))
	  (when cache
	    (map-cache (lambda (wrappers pv-cell)
			 (let ((call-vector (cdr pv-cell)))
			   (do-pv-calls (call index table)
			     (setf (callsref call-vector index)
				   (if gf-removed-p
				       nil
				       (compute-call call wrappers))))))
		       cache))))
      (when gf-removed-p
	(remhash call-key *pv-key->pv-tables*)))))

;;; end of gf-call-optimization.lisp
