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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/pv.lisp,v 1.2 2003/03/22 16:15:14 gerd Exp $")

(in-package "PCL-TEST")

;;;**************************
;;; With Optimization  ******
;;; *************************

#+gerds-pcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq pcl::*optimize-gf-calls-p* t))

(defclass pv0 ()
  ((a :accessor pv0-a :initform 0)))

(defmethod pv0.0 ((x pv0))
  1)

(defmethod pv0.1 ((x pv0) &rest r)
  (car r))

(defmethod pv0.2 ((x pv0) &key k)
  k)

(defmethod pv0.3 ((x pv0) &optional o)
  o)

(defmethod pv0.4 ((x pv0) (y pv0))
  1)

(defmethod call-pv0 ((x pv0))
  (list (pv0.0 x)
	(pv0.1 x 2)
	(pv0.2 x :k 3) (pv0.2 x)
	(pv0.3 x 1) (pv0.3 x)
	(pv0.4 x x)))

(deftest pv-gf-call-optimized.0
    (ignore-errors (call-pv0 (make-instance 'pv0)))
  (1 2 3 nil 1 nil 1))

(defclass pv0.1 (pv0) ())

(defmethod pv0.0 ((x pv0.1))
  (call-next-method))

(defmethod pv0.1 ((x pv0.1) &rest r)
  (declare (ignorable r))
  (call-next-method))

(defmethod pv0.2 ((x pv0.1) &key k)
  (declare (ignorable k))
  (call-next-method))

(defmethod pv0.3 ((x pv0.1) &optional o)
  (declare (ignorable o))
  (call-next-method))

(defmethod pv0.4 ((x pv0.1) (y pv0.1))
  (call-next-method))

(defmethod call-pv0 ((x pv0.1))
  (call-next-method))

(deftest pv-gf-call-optimized.1
    (ignore-errors (call-pv0 (make-instance 'pv0.1)))
  (1 2 3 nil 1 nil 1))

(deftest pv-gf-call-optimized.2
    (ignore-errors (call-pv0 (make-instance 'pv0)))
  (1 2 3 nil 1 nil 1))


;;;*****************************
;;; Without Optimization  ******
;;; ****************************

#+gerds-pcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq pcl::*optimize-gf-calls-p* nil))

(defclass pv1 ()
  ((a :accessor pv1-a :initform 0)))

(defmethod pv1.0 ((x pv1))
  1)

(defmethod pv1.1 ((x pv1) &rest r)
  (car r))

(defmethod pv1.2 ((x pv1) &key k)
  k)

(defmethod pv1.3 ((x pv1) &optional o)
  o)

(defmethod call-pv1 ((x pv1))
  (list (pv1.0 x)
	(pv1.1 x 2)
	(pv1.2 x :k 3) (pv1.2 x)
	(pv1.3 x 1) (pv1.3 x)))

(deftest pv-gf-call.1
    (call-pv1 (make-instance 'pv1))
  (1 2 3 nil 1 nil))

