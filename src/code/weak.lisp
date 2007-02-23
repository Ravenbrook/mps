;;; -*- Mode: Lisp; Package: EXTENSIONS; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/weak.lisp,v 1.6 2003/06/18 14:29:24 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/code/weak.lisp,v 1.6 2003/06/18 14:29:24 gerd Exp $
;;;
;;; Weak Pointer Support.
;;;
;;; Written by Christopher Hoover.
;;; 

(in-package "EXTENSIONS")

(export '(weak-pointer weak-pointer-p make-weak-pointer weak-pointer-value))

(defun make-weak-pointer (object)
  "Allocates and returns a weak pointer which points to OBJECT."
  (declare (values weak-pointer))
  (make-weak-pointer object))

(declaim (inline weak-pointer-value))
(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, returns the value of WEAK-POINTER and T.
   If the referent of WEAK-POINTER has been garbage collected, returns
   the values NIL and NIL."
  (declare (type weak-pointer weak-pointer)
	   (values t (member t nil)))
  ;; We don't need to wrap this with a without-gcing, because once we have
  ;; extracted the value, our reference to it will keep the weak pointer
  ;; from becoming broken.  We just have to make sure the compiler won't
  ;; reorder these primitives.
  (let ((value (c::%weak-pointer-value weak-pointer))
	(broken (c::%weak-pointer-broken weak-pointer)))
    (values value (not broken))))

(declaim (inline (setf weak-pointer-value)))
(defun (setf weak-pointer-value) (object weak-pointer)
  "Updates WEAK-POINTER to point to a new object."
  (declare (type weak-pointer weak-pointer))
  (c::%set-weak-pointer-broken weak-pointer nil)
  (c::%set-weak-pointer-value weak-pointer object))

;;; For the interpreter..

(defun c::%weak-pointer-value (w)
  (declare (type weak-pointer w))
  (c::%weak-pointer-value w))

(defun c::%weak-pointer-broken (w)
  (declare (type weak-pointer w))
  (c::%weak-pointer-broken w))

(defun c::%set-weak-pointer-value (w v)
  (declare (type weak-pointer w))
  (c::%set-weak-pointer-value w v))

(defun c::%set-weak-pointer-broken (w v)
  (declare (type weak-pointer w) (type boolean v))
  (c::%set-weak-pointer-broken w v))
