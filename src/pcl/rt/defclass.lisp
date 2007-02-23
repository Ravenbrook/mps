;;; Copyright (C) 2002 Gerd Moellmann <gerd.moellmann@t-online.de>
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

#+cmu
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/defclass.lisp,v 1.6 2003/04/18 10:06:23 gerd Exp $")

(in-package "PCL-TEST")

(deftest defclass-subtypep.0
    (progn
      (defclass st0 () ())
      (defclass st1 () ())
      (subtypep 'st1 'st0))
  nil t)

(deftest defclass-subtypep.1
    (progn
      (defclass st1 (st0) ())
      (subtypep 'st1 'st0))
  t t)

(deftest defclass-subtypep.2
    (progn
      (defclass st1 () ())
      (subtypep 'st1 'st0))
  nil t)
     
(defvar *instance* nil)
(defvar *update-instance-result* nil)

(defclass st2 ()
  ((a :initform 0 :accessor a)))

(defclass st3 ()
  ((b :initform 0 :accessor b)))

(deftest update-instance-for-redefined-class.0
    (progn
      (setq *instance* (make-instance 'st3))
      t)
  t)

(defmethod update-instance-for-redefined-class :after
    ((instance st3) added-slots discarded-slots property-list &rest initargs)
  (setq *update-instance-result*
	(list instance added-slots discarded-slots property-list initargs)))

(deftest update-instance-for-redefined-class.1
    (progn
      (defclass st3 (st2)
	((b :initform 0 :accessor b)))
      (values (slot-value *instance* 'b)
	      (eq *instance* (first *update-instance-result*))
	      (rest *update-instance-result*)))
  0 t ((a) nil nil nil))

(deftest update-instance-for-redefined-class.2
    (progn
      (defclass st3 ()
	((b :initform 0 :accessor b)))
      (values (slot-value *instance* 'b)
	      (eq *instance* (first *update-instance-result*))
	      (rest *update-instance-result*)))
  0 t (nil (a) (a 0) nil))

(deftest defclass-sxhash.0
    (let ((i1 (make-instance 'st2))
	  (i2 (make-instance 'st2)))
      (/= (sxhash i1) (sxhash i2)))
  t)

(deftest generic-function-sxhash.0
    (/= (sxhash #'allocate-instance)
	(sxhash #'make-instance))
  t)

(deftest defclass-redefinition.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass rd0 () ())
	  (defclass rd1 (rd0) ())
	  (defclass rd2 () ())
	  (defclass rd0 (rd2) ())
	  (make-instance 'rd1))
      (values (not (null r)) (null c)))
  t t)

;;; This failed to compile in an old version, that's why it's here.

(deftest defclass-inherited-class-slots.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ics0 ()
	    ((a :allocation :class :accessor ics0-a)))
	  (defclass ics1 (ics0)
	    ())
	  (make-instance 'ics1))
      (values (not (null r)) (null c)))
  t t)

(defmacro define-defclass-syntax-test (name class-body &rest options)
  `(deftest ,name
       (multiple-value-bind (r c)
	   (ignore-errors
	     (defclass dc0 ()
	       ,class-body ,@options))
	 (declare (ignore r))
	 (typep c 'error))
     t))

;; CLHS: allocation should be :class or :instance
(define-defclass-syntax-test defclass.0 ((a :allocation :foo)))

;; Reader names should be symbols.
(define-defclass-syntax-test defclass.1 ((a :reader (setf a))))

;;; initarg names must be symbols.
(define-defclass-syntax-test defclass.2 ((a :initarg 1)))

;; Duplicate :default-initargs is an error.
(define-defclass-syntax-test defclass.3 ()
  (:default-initargs :a 1)
  (:default-initargs :b 2))

;; Duplicate :metaclass.
(define-defclass-syntax-test defclass.4 ()
  (:metaclass pcl::funcallable-standard-class)
  (:metaclass 1))

;; class option that is not implemented locally -> error
(define-defclass-syntax-test defclass.5 ()
  (:foo t))

(deftest defclass-structure-class.0
  (multiple-value-bind (r c)
      (ignore-errors
	(defclass dscl.0 ()
	  (a b)
	  (:metaclass pcl::structure-class))
	t)
    (values r (null c)))
  t t)

(deftest defclass-structure-class.1
  (multiple-value-bind (r c)
      (ignore-errors
	(make-instance 'dscl.0)
	t)
    (values r (null c)))
  t t)

;;;
;;; The change of DFR1 from forward-referenced to standard class
;;; caused problems at some point, which were fixed by passing
;;; initargs to CHANGE-CLASS in ENSURE-CLASS-USING-CLASS.
;;;
(deftest defclass-forward-referenced-class.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr0 (dfr1 dfr2) ())
	  (defclass dfr1 (dfr3 dfr4) ())
	  t)
      (values r (null c)))
  t t)

(deftest defclass-forward-referenced-class.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr.c1 (dfr.c2) ())
	  (defclass dfr.c2 (dfr.c3) ())
	  (defclass dfr.c3 () ())
	  (make-instance 'dfr.c1)
	  t)
      (values r (null c)))
  t t)

;;;
;;; TYPEP and SUBTYPEP used to fail with forward-referenced/unfinalized
;;; classes.
;;;
(deftest defclass-types.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr5 (dfr6) ())
	  (typep t (find-class 'dfr6)))
      (values r (null c)))
  nil t)

(deftest defclass-types.2
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr7 (dfr8) ())
	  (multiple-value-list
	   (subtypep (find-class 'dfr7) (find-class 'dfr8))))
      (values r (null c)))
  (t t) t)

(deftest defclass-types.3
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr7 (dfr8) ())
	  (multiple-value-list
	   (subtypep (find-class 'dfr8) (find-class 'dfr7))))
      (values r (null c)))
  (nil t) t)

(deftest defclass-types.4
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr9 (dfr10) ())
	  (defclass dfr11 (dfr9 dfr12) ())
	  (append
	   (multiple-value-list
	    (subtypep (find-class 'dfr9) (find-class 'dfr10)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr11) (find-class 'dfr10)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr11) (find-class 'dfr9)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr11) (find-class 'dfr12)))))
      (values r (null c)))
  (t t t t t t t t) t)

(deftest defclass-types.5
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr13 () ())
	  (defclass dfr14 (dfr15 dfr13) ())
	  (defclass dfr16 (dfr14 dfr17) ())
	  (append
	   (multiple-value-list
	    (subtypep (find-class 'dfr16) (find-class 'dfr14)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr16) (find-class 'dfr17)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr16) (find-class 'dfr15)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr16) (find-class 'dfr13)))))
      (values r (null c)))
  (t t t t t t t t) t)

(deftest defclass-types.6
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dfr20 (dfr21) ())
	  (defclass dfr21 (dfr22) ())
	  (append 
	   (multiple-value-list
	    (subtypep (find-class 'dfr20) (find-class 'dfr21)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr21) (find-class 'dfr22)))
	   (multiple-value-list
	    (subtypep (find-class 'dfr20) (find-class 'dfr22)))))
      (values r (null c)))
  (t t t t t t) t)
