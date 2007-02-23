;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/fngen.lisp,v 1.13 2003/06/03 10:28:23 gerd Exp $")

(in-package :pcl)

;;;
;;; GET-FUNCTION is the main user interface to this code. It is like
;;; COMPILE-LAMBDA, only more efficient. It achieves this efficiency by 
;;; reducing the number of times that the compiler needs to be called.  
;;; Calls to GET-FUNCTION in which the lambda forms differ only by constants 
;;; can use the same piece of compiled code.  (For example, dispatch dfuns and 
;;; combined method functions can often be shared, if they differ only 
;;; by referring to different methods.)
;;;
;;; If GET-FUNCTION is called with a lambda expression only, it will return 
;;; a corresponding function. The optional constant-converter argument
;;; can be a function which will be called to convert each constant appearing
;;; in the lambda to whatever value should appear in the function.
;;;
;;; There are three internal functions which operate on the lambda argument 
;;; to GET-FUNCTION:
;;;   compute-test converts the lambda into a key to be used for lookup,
;;;   compute-code is used by get-new-function-generator-internal to
;;;                generate the actual lambda to be compiled, and
;;;   compute-constants is used to generate the argument list that is
;;;                to be passed to the compiled function.
;;;
;;; Whether the returned function is actually compiled depends on whether
;;; the compiler is present (see COMPILE-LAMBDA) and whether this shape of
;;; code was precompiled.
;;;

(defun get-function1 (lambda &optional
		      (test-converter #'default-test-converter)
		      (code-converter #'default-code-converter)
		      (constant-converter #'default-constant-converter))
  (values (the function (get-function-generator lambda test-converter
						code-converter))
	  (compute-constants lambda constant-converter)))

(defun default-constantp (form)
  (and (constantp form)
       (not (typep (eval form) '(or symbol fixnum)))))

(defun default-test-converter (form)
  (if (default-constantp form)
      '.constant.
      form))

(defun default-code-converter  (form)
  (if (default-constantp form)
      (let ((gensym (gensym))) (values gensym (list gensym)))
      form))

(defun default-constant-converter (form)
  (if (default-constantp form)
      (list (eval form))
      nil))


(defstruct (fgen (:constructor %make-fgen))
  test
  gensyms
  generator
  generator-lambda
  system)

;;;
;;; *fgens* is a list of all the function generators we have so far.  Each 
;;; element is a FGEN structure as implemented below.  Don't ever touch this
;;; list by hand, use STORE-FGEN.
;;;
(defvar *fgens* ())

(defun lookup-fgen (test)
  (find test (the list *fgens*) :key #'fgen-test :test #'equal))

(defun store-fgen (fgen)
  (let ((old (lookup-fgen (fgen-test fgen))))
    (if old
	(setf (fgen-generator old) (fgen-generator fgen)
	      (fgen-system old) (or (fgen-system old)
				    (fgen-system fgen)))
	(setq *fgens* (nconc *fgens* (list fgen))))))

(defun make-fgen (test gensyms generator generator-lambda system)
  (%make-fgen :test test :gensyms gensyms :generator generator
	      :generator-lambda generator-lambda :system system))



(defun get-function-generator (lambda test-converter code-converter)
  (let* ((test (compute-test lambda test-converter))
	 (fgen (lookup-fgen test)))
    (if fgen
	(fgen-generator fgen)
	(get-new-function-generator lambda test code-converter))))

(defun get-new-function-generator (lambda test code-converter)
  (multiple-value-bind (gensyms generator-lambda)
      (get-new-function-generator-internal lambda code-converter)
    (let* ((generator (compile-lambda generator-lambda))
	   (fgen (make-fgen test gensyms generator generator-lambda nil)))
      (store-fgen fgen)
      generator)))

(defun get-new-function-generator-internal (lambda code-converter)
  (multiple-value-bind (code gensyms)
      (compute-code lambda code-converter)
    (values gensyms `(lambda ,gensyms (function ,code)))))

(defun compute-test (lambda test-converter)
  (let ((walk-form-expand-macros-p t))
    (walk-form lambda
	       nil
	       (lambda (f c e)
		 (declare (ignore e))
		 (if (neq c :eval)
		     f
		     (let ((converted (funcall test-converter f)))
		       (values converted (neq converted f))))))))

(defun compute-code (lambda code-converter)
  (let ((walk-form-expand-macros-p t)
	(gensyms ()))
    (values (walk-form lambda
		       nil
		       (lambda (f c e)
			 (declare (ignore e))
			 (if (neq c :eval)
			     f
			     (multiple-value-bind (converted gens)
				 (funcall code-converter f)
			       (when gens
				 (setq gensyms (append gensyms gens)))
			       (values converted (neq converted f))))))
	      gensyms)))

(defun compute-constants (lambda constant-converter)
  (let ((walk-form-expand-macros-p t) ; doesn't matter here.
	(collected ()))
    (walk-form lambda
	       nil
	       (lambda (f c e)
		 (declare (ignore e))
		 (if (eq c :eval)
		     (let ((consts (funcall constant-converter f)))
		       (if consts
			   (progn
			     (setq collected (append collected consts))
			     (values f t))
			   f))
		     f)))
    collected))


;;;
;;;
;;;
(defmacro precompile-function-generators (&optional system)
  (let ((index -1))
    `(progn ,@(let ((collected ()))
		(dolist (fgen *fgens* (nreverse collected))
		  (when (or (null (fgen-system fgen))
			    (eq (fgen-system fgen) system))
		    (when system
		      (setf (fgen-system fgen) system))
		    (push
		     (make-top-level-form
		      `(precompile-function-generators ,system ,(incf index))
		      '(:load-toplevel)
		      `(load-function-generator
			',(fgen-test fgen)
			',(fgen-gensyms fgen)
			(function ,(fgen-generator-lambda fgen))
			',(fgen-generator-lambda fgen)
			',system))
		     collected)))))))

(defun load-function-generator (test gensyms generator generator-lambda system)
  (store-fgen (make-fgen test gensyms generator generator-lambda system)))

(defun flush-emf-cache (&optional gf)
  "Flush cached emf functions.  If GF is supplied, it should be a
   generic function metaobject or the name of a generic function, and
   this function flushes all cached emfs for the given generic
   function.  If GF is not supplied, all cached emfs are flushed."
  (let ((gf-name (if (generic-function-p gf)
		     (generic-function-name gf)
		     gf)))
    (collect ((new))
      (dolist (f *fgens* (setq *fgens* (new)))
	(when (notany (lambda (x)
			(and (consp x)
			     (eq (car x) 'fast-method)
			     (or (null gf-name)
				 (equal gf-name (cadr x)))))
		      (fgen-test f))
	  (new f))))))

