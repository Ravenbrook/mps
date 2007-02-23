;;; -*- Package: Kernel -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/setf-funs.lisp,v 1.6 1998/07/19 00:22:19 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Stuff to automatically generate SETF functions for all the standard
;;; functions that are currently implemented with setf macros.
;;;
(in-package "KERNEL")

(eval-when (compile eval)

(defun compute-one-setter (name type)
  (let* ((args (second type))
	 (res (type-specifier
	       (single-value-type
		(values-specifier-type (third type)))))
	 (arglist (loop repeat (1+ (length args)) collect (gensym))))
    (cond
     ((null (intersection args lambda-list-keywords))
      `(defun (setf ,name) ,arglist
	 (declare ,@(mapcar #'(lambda (arg type)
				`(type ,type ,arg))
			    arglist
			    (cons res args)))
	 (setf (,name ,@(rest arglist)) ,(first arglist))))
     (t
      (warn "Hairy setf expander for function ~S." name)
      nil))))
       

(defmacro define-setters (packages &rest ignore)
  (collect ((res))
    (dolist (pkg packages)
      (do-external-symbols (sym pkg)
	(when (and (fboundp sym)
		   (eq (info function kind sym) :function)
		   (or (info setf inverse sym)
		       (info setf expander sym))
		   (not (member sym ignore)))
	  (let ((type (type-specifier (info function type sym))))
	    (assert (consp type))
	    (res `(declaim (inline (setf ,sym))))
	    (res (compute-one-setter sym type))))))
    `(progn ,@(res))))

); eval-when (compile eval)

(define-setters ("LISP")
  ;; Semantically silly...
  getf apply ldb mask-field logbitp subseq values
  ;; Have explicit redundant definitions...
  setf bit sbit get aref gethash)
