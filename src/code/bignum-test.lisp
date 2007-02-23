;;;; -*- Package: Bignum -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/bignum-test.lisp,v 1.4 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Some stuff to check that bignum operations are retuning the correct
;;; results.
;;; 
(in-package "BIGNUM")

(defvar *in-bignum-wrapper* nil)

(defmacro def-bignum-wrapper (name lambda-list &body body)
  (let ((var-name (ext:symbolicate "*OLD-" name "*"))
	(wrap-name (ext:symbolicate "WRAP-" name))
	(args (mapcar #'(lambda (x)
			  (if (listp x) (car x) x))
		      (remove-if #'(lambda (x)
				     (member x lambda-list-keywords))
				 lambda-list))))
    `(progn
       (defvar ,var-name (fdefinition ',name))
       (defun ,wrap-name ,lambda-list
	 (if *in-bignum-wrapper*
	     (funcall ,var-name ,@args)
	     (let ((*in-bignum-wrapper* t))
	       ,@body)))
       (setf (fdefinition ',name) #',wrap-name))))

(defun big= (x y)
  (= (if (typep x 'bignum)
	 (%normalize-bignum x (%bignum-length x))
	 x)
     (if (typep y 'bignum)
	 (%normalize-bignum y (%bignum-length y))
	 y)))

(def-bignum-wrapper add-bignums (x y)
  (let ((res (funcall *old-add-bignums* x y)))
    (assert (big= (- res y) x))
    res))

(def-bignum-wrapper multiply-bignums (x y)
  (let ((res (funcall *old-multiply-bignums* x y)))
    (if (zerop x)
	(assert (zerop res))
	(multiple-value-bind (q r) (truncate res x)
	  (assert (and (zerop r) (big= q y)))))
    res))

(def-bignum-wrapper negate-bignum (x &optional (fully-normalized t))
  (let ((res (funcall *old-negate-bignum* x fully-normalized)))
    (assert (big= (- res) x))
    res))

(def-bignum-wrapper subtract-bignum (x y)
  (let ((res (funcall *old-subtract-bignum* x y)))
    (assert (big= (+ res y) x))
    res))

(def-bignum-wrapper multiply-bignum-and-fixnum (x y)
  (let ((res (funcall *old-multiply-bignum-and-fixnum* x y)))
    (if (zerop x)
	(assert (zerop res))
	(multiple-value-bind (q r) (truncate res x)
	  (assert (and (zerop r) (big= q y)))))
    res))

(def-bignum-wrapper multiply-fixnums (x y)
  (let ((res (funcall *old-multiply-fixnums* x y)))
    (if (zerop x)
	(assert (zerop res))
	(multiple-value-bind (q r) (truncate res x)
	  (assert (and (zerop r) (big= q y)))))
    res))

(def-bignum-wrapper bignum-ashift-right (x shift)
  (let ((res (funcall *old-bignum-ashift-right* x shift)))
    (assert (big= (ash res shift) (logand x (ash -1 shift))))
    res))

(def-bignum-wrapper bignum-ashift-left (x shift)
  (let ((res (funcall *old-bignum-ashift-left* x shift)))
    (assert (big= (ash res (- shift)) x))
    res))

(def-bignum-wrapper bignum-truncate (x y)
  (multiple-value-bind (q r)
		       (funcall *old-bignum-truncate* x y)
    (assert (big= (+ (* q y) r) x))
    (values q r)))

(def-bignum-wrapper bignum-compare (x y)
  (let ((res (funcall *old-bignum-compare* x y)))
    (assert (big= (signum (- x y)) res))
    res))
