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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/inline-access.lisp,v 1.3 2003/05/20 20:21:26 gerd Exp $")

(in-package "PCL-TEST")

(defun test-walk (form test-function &optional env)
  (let ((result nil))
    (flet ((walk-function (form context env)
	     (declare (ignore context))
	     (when (and (consp form) (eq (car form) 'test))
	       (push (funcall test-function env) result))
	     form))
      (walker:walk-form form env #'walk-function)
      (nreverse result))))

(defmacro define-declaration-test (name declaration test &key values)
  `(deftest ,name
       (test-walk '(defun dummy () ,declaration (test))
		  (lambda (env) ,test))
     ,@values))

(define-declaration-test slot-declaration.0
    (declare (ext:slots (slot-boundp xx)))
  (pcl::slot-declaration env 'slot-boundp 'xx)
  :values ((t)))

(define-declaration-test slot-declaration.1
    (declare (ext:slots (inline xx)))
  (pcl::slot-declaration env 'inline 'xx)
  :values ((t)))

(define-declaration-test slot-declaration.2
    (declare (ext:slots (inline (xx))))
  (pcl::slot-declaration env 'inline 'xx)
  :values ((t)))

(define-declaration-test slot-declaration.3
    (declare (ext:slots (inline (xx a))))
  (pcl::slot-declaration env 'inline 'xx 'a)
  :values ((t)))

(define-declaration-test slot-declaration.4
    (declare (ext:slots (inline (xx a))))
  (pcl::slot-declaration env 'inline 'xx 'b)
  :values ((nil)))

(define-declaration-test slot-declaration.5
    (declare (ext:slots (inline (xx a) yy)))
  (pcl::slot-declaration env 'inline 'yy)
  :values ((t)))

(define-declaration-test slot-declaration.6
    (declare (ext:slots (inline (xx a) (yy a))))
  (pcl::slot-declaration env 'inline 'yy 'a)
  :values ((t)))

(define-declaration-test slot-declaration.7
    (declare (ext:slots (inline (xx a) (yy a))))
  (pcl::slot-declaration env 'inline 'yy 'b)
  :values ((nil)))

(deftest global-slot-declaration.0
    (progn
      (proclaim '(ext:slots (slot-boundp gsd)))
      (not (null (pcl::slot-declaration nil 'slot-boundp 'gsd))))
  t)

(deftest global-slot-declaration.1
    (progn
      (proclaim '(ext:slots (inline (gsd gsd-a))))
      (not (null (pcl::slot-declaration nil 'inline 'gsd 'gsd-a))))
  t)

(deftest auto-compile-declaration.0
    (progn
      (proclaim '(ext:auto-compile acd))
      (pcl::auto-compile-p 'acd nil nil))
  t)

(deftest auto-compile-declaration.1
    (progn
      (proclaim '(ext:auto-compile acd))
      (pcl::auto-compile-p 'acd '(:around) '(t t)))
  t)

(deftest auto-compile-declaration.2
    (progn
      (proclaim '(ext:not-auto-compile acd))
      (proclaim '(ext:auto-compile (acd :around (t t))))
      (values (pcl::auto-compile-p 'acd nil nil)
	      (pcl::auto-compile-p 'acd nil '(t t))
	      (pcl::auto-compile-p 'acd '(:around) '(t t))))
  nil nil t)

(deftest auto-compile-declaration.3
    (progn
      (proclaim '(ext:auto-compile acd))
      (proclaim '(ext:not-auto-compile (acd :around (t t))))
      (values (pcl::auto-compile-p 'acd nil nil)
	      (pcl::auto-compile-p 'acd nil '(t t))
	      (pcl::auto-compile-p 'acd '(:around) '(t t))))
  t t nil)

(deftest auto-compile-declaration.4
    (progn
      (proclaim '(ext:auto-compile))
      (proclaim '(ext:not-auto-compile acd))
      (values (pcl::auto-compile-p 'foo nil nil)
	      (pcl::auto-compile-p 'acd nil '(t t))
	      (pcl::auto-compile-p 'acd '(:around) '(t t))))
  t nil nil)

(deftest auto-compile-declaration.5
    (progn
      (proclaim '(ext:auto-compile (setf acd)))
      (pcl::auto-compile-p '(setf acd) '(:around) '(t t)))
  t)


(declaim (ext:slots (inline sacc.0)))

(defclass sacc.0 ()
  ((a :initform 0 :initarg :a :accessor sacc.0-a)))

(defclass sacc.1 (sacc.0)
  ((b :initform 0 :initarg :b :accessor sacc.1-b)
   (a :initform 0 :initarg :a :accessor sacc.0-a)))

(defmethod sacc.0.0 ((x sacc.0))
  (slot-value x 'a))

(defmethod sacc.0.1 ((x sacc.0))
  (sacc.0-a x))

(defmethod sacc.0.2 ((x sacc.0) nv)
  (setf (slot-value x 'a) nv))

(defmethod sacc.0.3 ((x sacc.0) nv)
  (setf (sacc.0-a x) nv))

(defun method-using-inline-access-p (class-name method-name qualifiers
				     specializers)
  (let ((method (find-method (fdefinition method-name) qualifiers
			     specializers)))
    (car (member class-name (pcl::plist-value method 'pcl::inline-access)
		 :test #'eq))))

(deftest inline-access-p.0
    (and (method-using-inline-access-p 'sacc.0 'sacc.0.0 nil '(sacc.0))
	 (method-using-inline-access-p 'sacc.0 'sacc.0.1 nil '(sacc.0))
	 (method-using-inline-access-p 'sacc.0 'sacc.0.2 nil '(sacc.0 t))
	 (method-using-inline-access-p 'sacc.0 'sacc.0.3 nil '(sacc.0 t)))
  sacc.0)

(deftest inline-access-p.1
    (let ((methods (pcl::methods-using-inline-slot-access
		    (pcl::find-class 'sacc.0))))
      (length methods))
  4)

(deftest inline-access.0
    (sacc.0.0 (make-instance 'sacc.0))
  0)

(deftest inline-access.1
    (let ((instance (make-instance 'sacc.0 :a 11)))
      (values (sacc.0.0 instance)
	      (sacc.0.1 instance)))
  11 11)

(deftest inline-access.2
    (let ((instance (make-instance 'sacc.0 :a 11)))
      (sacc.0.2 instance 10)
      (slot-value instance 'a))
  10)

(deftest inline-access.3
    (let ((instance (make-instance 'sacc.0 :a 11)))
      (sacc.0.3 instance 10)
      (slot-value instance 'a))
  10)

(defmacro define-warning-test (name (value) &body body)
  `(deftest ,name
       (let (warning)
	 (flet ((note-warning (c)
		  (declare (ignore c))
		  (setq warning t)
		  (muffle-warning)))
	   (handler-bind ((warning #'note-warning))
	     ,@body)
	   warning))
     ,value))

(define-warning-test warn.0 (t) (warn "Test the test"))  
(define-warning-test warn.1 (nil) nil)

(define-warning-test inline-warn.0 (nil)
  (defclass sacc.0 ()
    ((a :initform 0 :initarg :a :accessor sacc.0-a))))

(define-warning-test inline-warn.1 (t)
  (defclass sacc.0 ()
    ((a :initform 0 :initarg :a :accessor sacc.0-a)
     (b :initform 0))))

(define-warning-test inline-warn.2 (t)
  (progn
    (defmethod inline-warn.2.method ((x sacc.1))
      (declare (pcl::slots (inline sacc.1)))
      (slot-value x 'b))
    (defclass sacc.0 ()
      ((a :initform 0 :initarg :a :accessor sacc.0-a)))))
      
