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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/ctor.lisp,v 1.7 2003/10/23 10:29:33 gerd Exp $")

(in-package "PCL-TEST")

(deftest plist-keys.0
    (pcl::plist-keys '())
  nil)

(deftest plist-keys.1
    (pcl::plist-keys '(:a 1 :b 2))
  (:a :b))

(deftest plist-keys.2
    (multiple-value-bind (result condition)
	(ignore-errors (pcl::plist-keys '(:a)))
      (values result (typep condition 'condition)))
  nil
  t)
      
(deftest make-instance->constructor-call.0
    (pcl::make-instance->constructor-call '(make-instance 'foo a x))
  nil)

(deftest make-instance->constructor-call.1
    (pcl::make-instance->constructor-call '(make-instance foo :a x))
  nil)

(deftest make-instance->constructor-call.2
    (pcl::make-instance->constructor-call '(make-instance 'foo x))
  nil)

(deftest make-instance->constructor-call.4
    (pcl::make-instance->constructor-call '(make-instance 1))
  nil)

(deftest make-instance->constructor-call.5
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t ())

(deftest make-instance->constructor-call.6
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x 1 :y 2)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t ())

(deftest make-instance->constructor-call.7
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x x :y 2)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t (x))

(deftest make-instance->constructor-call.8
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x x :y y)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t (x y))

(deftest make-instance->constructor-call.9
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x x :y 1)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t (x))

(deftest make-instance->constructor-call.10
    (let* ((form (pcl::make-instance->constructor-call
		  '(make-instance 'foo :x x :y 1 :z z)))
	   (call (car (last form))))
      (values (eq (first call) 'funcall)
	      (cddr call)))
  t (x z))

(deftest make-ctor.0
    (let ((ctor (pcl::make-ctor '(pcl::ctor bar) 'bar '(:x 1 :y 2))))
      (values (pcl::ctor-function-name ctor)
	      (pcl::ctor-class-name ctor)
	      (pcl::ctor-initargs ctor)))
  (pcl::ctor bar)
  bar
  (:x 1 :y 2))

(defclass foo ()
  ((a :initarg :a :initform 1)
   (b :initarg :b :initform 2)))

(defun call-generator (generator function-name class-name args)
  (declare (ignore function-name))
  (let* ((ctor
	   (pcl::make-ctor (list 'pcl::ctor class-name) class-name args))
	 (class (find-class class-name))
	 (proto (pcl::class-prototype class))
	 (ii (pcl::compute-applicable-methods
	      #'initialize-instance (list proto)))
	 (si (pcl::compute-applicable-methods
	      #'shared-initialize (list proto t))))
    (setf (pcl::ctor-class ctor) class)
    (if (eq generator #'pcl::fallback-generator)
	(funcall generator ctor)
	(funcall generator ctor ii si))))
     
(deftest fallback-generator.0
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '(:a 0 :b 1))))
      (values (second fn)
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  ()
  pcl::standard-class
  (:a 0 :b 1))

(deftest fallback-generator.1
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '(:a 0))))
      (values (second fn)
	      (first (third fn))
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  ()
  make-instance
  pcl::standard-class
  (:a 0))

(deftest fallback-generator.2
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '())))
      (values (second fn)
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  ()
  pcl::standard-class
  ())

(deftest fallback-generator.3
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '(:a .p0.))))
      (values (second fn)
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  (.p0.)
  pcl::standard-class
  (:a .p0.))

(deftest fallback-generator.4
    (let ((fn (call-generator #'pcl::fallback-generator
			      'make-foo 'foo '(:a a :b b))))
      (values (second fn)
	      (type-of (second (third fn)))
	      (nthcdr 2 (third fn))))
  (a b)
  pcl::standard-class
  (:a a :b b))

;;; These depend on the actual slot definition location computation,
;;; which may be different in my PCL than in the CVS PCL.

(deftest compute-initarg-locations.0
    (let ((class (find-class 'foo)))
      (pcl::compute-initarg-locations class '(:a :b)))
  ((:a (0 . t)) (:b (1 . t))))

(defclass foo2 (foo)
  ((c :initarg :a)))

(deftest compute-initarg-locations.1
    (let ((class (find-class 'foo2)))
      (pcl::compute-initarg-locations class '(:a :b)))
  ((:a (0 . t) (2 . t)) (:b (1 . t))))

(defclass foo3 (foo)
  ((c :initarg :a :allocation :class)))

;;;
;;; This test must be compiled for the case that PCL::+SLOT-UNBOUND+
;;; is a symbol macro calling PCL::MAKE-UNBOUND-MARKER, otherwise
;;; we'll get a complaint that C::%%PRIMITIVE is not defined.
;;;
(define-compiled-test compute-initarg-locations.2
    (let ((class (find-class 'foo3)))
      (subst 'unbound pcl::+slot-unbound+
	     (pcl::compute-initarg-locations class '(:a :b))))
  ((:a (0 . t) ((c . unbound) . t)) (:b (1 . t))))

(defclass foo4 ()
  ((a :initarg :a :initarg :both)
   (b :initarg :b :initarg :both)))

(deftest compute-initarg-locations.3
    (let ((class (find-class 'foo4)))
      (pcl::compute-initarg-locations class '(:both :a :b)))
  ((:both (0 . t) (1 . t)) (:a) (:b)))

(deftest compute-initarg-locations.4
    (let ((class (find-class 'foo4)))
      (pcl::compute-initarg-locations class '(:a :both)))
  ((:a (0 . t)) (:both (1 . t))))

(deftest slot-init-forms.0
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo '(:a a :b b))))
      (setf (pcl::ctor-class ctor) (find-class 'foo))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t a))
    (setf (svref pcl::.slots. 1) (the t b))))

(deftest slot-init-forms.1
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo '(:a a))))
      (setf (pcl::ctor-class ctor) (find-class 'foo))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t a))
    (setf (svref pcl::.slots. 1) (the t '2))))

(defclass foo5 ()
  ((a :initarg :a :initform 0)
   (b :initarg :b)))

(deftest slot-init-forms.2
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo5 '(:a a))))
      (setf (pcl::ctor-class ctor) (find-class 'foo5))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t a))
    (setf (svref pcl::.slots. 1) pcl::+slot-unbound+)))

(defclass foo5a ()
  ((a :initarg :a :initform 0)
   (b :initarg :b :initform 0)))

(deftest slot-init-forms.2a
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo5a '())))
      (setf (pcl::ctor-class ctor) (find-class 'foo5a))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t '0))
    (setf (svref pcl::.slots. 1) (the t '0))))

(defclass foo6 ()
  ((a :initarg :a :initform 0 :allocation :class)
   (b :initarg :b)))

(deftest slot-init-forms.3
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo6 '(:a a))))
      (setf (pcl::ctor-class ctor) (find-class 'foo6))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) pcl::+slot-unbound+)
    (setf (cdr '(a . 0)) (the t a))))

(defun foo ()
  (error "should never be called"))

(defclass foo7 ()
  ((a :initarg :a :initform (foo))
   (b :initarg :b)))

(deftest slot-init-forms.4
    (let* ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo7 '())))
      (setf (pcl::ctor-class ctor) (find-class 'foo7))
      (let ((form (pcl::slot-init-forms ctor nil)))
	(destructuring-bind (let vars declare setf1 setf2) form
	  (declare (ignore let vars declare))
	  (values setf2 (second setf1) (first (third (third setf1)))
		  (functionp (second (third (third setf1))))))))
  (setf (svref pcl::.slots. 1) pcl::+slot-unbound+)
  (svref pcl::.slots. 0)
  funcall
  t)

(deftest slot-init-forms.5
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo '(:a '(foo)))))
      (setf (pcl::ctor-class ctor) (find-class 'foo))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t '(foo)))
    (setf (svref pcl::.slots. 1) (the t '2))))

(deftest slot-init-forms.6
    (let ((ctor (pcl::make-ctor
		 (list 'pcl::ctor 'make-foo)
		 'foo '(:a 'x))))
      (setf (pcl::ctor-class ctor) (find-class 'foo))
      (pcl::slot-init-forms ctor nil))
  (let ()
    (declare (ignorable) (optimize (safety 3)))
    (setf (svref pcl::.slots. 0) (the t 'x))
    (setf (svref pcl::.slots. 1) (the t '2))))

(defmethod bar1 ((x integer))
  (* x 2))

(defmethod bar2 ((x integer)) x)
(defmethod bar2 :around ((x integer)) x)

(deftest around-or-nonstandard-primary-method-p.0
    (pcl::around-or-nonstandard-primary-method-p
     (pcl::compute-applicable-methods #'bar2 (list 1)))
  t)

(defmethod bar3 ((x integer)) x)
(defmethod bar3 :after ((x integer)) x)

(deftest around-or-nonstandard-primary-method-p.1
    (pcl::around-or-nonstandard-primary-method-p
     (pcl::compute-applicable-methods #'bar3 (list 1)))
  nil)

(deftest optimizing-generator.0
    (let ((fn (call-generator #'pcl::optimizing-generator
			      'make-foo 'foo '(:a 0 :b 1))))
      (second fn))
  ())

(defun construct (class-name initargs &rest args)
  (let* ((form (call-generator #'pcl::optimizing-generator
			       'some-function-name
			       class-name
			       initargs))
	 (fn (pcl::compile-lambda form)))
    (apply fn args)))

(deftest optimizing-generator.1
    (with-slots (a b) (construct 'foo '(:a 0 :b 1))
      (values a b))
  0 1)

(deftest optimizing-generator.2
    (with-slots (a b) (construct 'foo '())
      (values a b))
  1 2)

(defclass g1 ()
  ((a :initform 0)
   (b)))

(deftest optimizing-generator.3
    (let ((instance (construct 'g1 '())))
      (values (slot-value instance 'a)
	      (slot-boundp instance 'b)))
  0 nil)

