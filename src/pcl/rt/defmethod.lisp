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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/defmethod.lisp,v 1.5 2003/05/30 09:33:32 gerd Exp $")

(in-package "PCL-TEST")

(defmethod dm0 (x)
  x)

(defmethod dm1 (x &rest y)
  (list x y))

(defmethod dm2 (x &optional y)
  (list x y))

(defmacro define-defmethod-test (name method qual lambda-list
				 &rest values)
  `(deftest ,name
     (multiple-value-bind (r c)
	 (ignore-errors
	   (defmethod ,method ,@(when qual `(,qual)) ,lambda-list
	     #+cmu (declare (optimize (ext:inhibit-warnings 3)))
	     nil))
       (values (typep r 'method)
	       (typep c 'error)
	       (length (pcl:generic-function-methods #',method))))
     ,@values))

(defmacro define-defmethod-test-1 (name method qual lambda-list)
  `(define-defmethod-test ,name ,method ,qual ,lambda-list nil t 1))

(define-defmethod-test-1 defmethod.0 dm0 nil (x y))
(define-defmethod-test-1 defmethod.1 dm0 nil (x &rest y))
(define-defmethod-test-1 defmethod.2 dm0 nil (x &key y))
(define-defmethod-test-1 defmethod.4 dm0 :before (x y))
(define-defmethod-test-1 defmethod.5 dm0 :before (x &rest y))
(define-defmethod-test-1 defmethod.6 dm0 :before (x &key y))
(define-defmethod-test defmethod.7 dm0 nil (x) t nil 1)
    
(define-defmethod-test-1 defmethod.10 dm1 nil (x y))
(define-defmethod-test-1 defmethod.11 dm1 nil (x))
(define-defmethod-test defmethod.12 dm1 nil (x &key y) t nil 1)
(define-defmethod-test defmethod.13 dm1 nil (x &key y z) t nil 1)
(define-defmethod-test defmethod.14 dm1 nil (x &rest y) t nil 1)

(define-defmethod-test-1 defmethod.20 dm2 nil (x))
(define-defmethod-test-1 defmethod.21 dm2 nil (x &optional y z))
(define-defmethod-test-1 defmethod.22 dm2 nil (x &key y))

;;;
;;; A forward-referenced class used as specializer signaled an
;;; error at some point.
;;;
(deftest defmethod-forwared-referenced.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dm.3 () ())
	  (defclass dm.4 (dm.forward) ())
	  (defmethod dm.5 ((x dm.3)) x)
	  (defmethod dm.5 ((x dm.4)) x)
	  t)
      (values r (null c)))
  t t)
      
(deftest defmethod-forwared-referenced.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dm.3 () ())
	  (defclass dm.4 (dm.forward) ())
	  (defmethod dm.5 ((x dm.3)) x)
	  (defmethod dm.5 ((x dm.4)) x)
	  (dm.5 (make-instance 'dm.3))
	  t)
      (values r (null c)))
  t t)

(deftest defmethod-metacircle.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass dmm.0 () ())
	  (defclass dmm.1 () ())
	  (defclass dmm.0+1 (dmm.0 dmm.1) ())
	  (defmethod dmm.0 ((x dmm.0) (y dmm.1)) 1)
	  (defmethod dmm.0 ((x dmm.1) (y dmm.0)) 2)
	  (dmm.0 (make-instance 'dmm.0+1) (make-instance 'dmm.0+1))
	  (defmethod dmm.0 ((x dmm.0+1) (y dmm.0+1)) 3)
	  (dmm.0 (make-instance 'dmm.0+1) (make-instance 'dmm.0+1)))
      (values r (null c)))
  3 t)

(deftest defmethod-setf-fdefinition.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defgeneric dsf.0 (x))
	  (defmethod dsf.0 ((x integer)) x)
	  (setf (fdefinition 'dsf.1) #'dsf.0)
	  (defmethod dsf.1 ((x string)) x)
	  (list (length (mop:generic-function-methods #'dsf.0))
		(equal (mop:generic-function-methods #'dsf.1)
		       (mop:generic-function-methods #'dsf.0))))
      (values r (null c)))
  (2 t) t)

(deftest defmethod-setf-fdefinition.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (defgeneric dsf.2 (x))
	  (defmethod dsf.2 ((x integer)) x)
	  (setf (fdefinition 'dsf.3) #'dsf.2)
	  (defmethod dsf.3 ((x integer)) x)
	  (list (length (mop:generic-function-methods #'dsf.2))
		(equal (mop:generic-function-methods #'dsf.3)
		       (mop:generic-function-methods #'dsf.2))))
      (values r (null c)))
  (1 t) t)
