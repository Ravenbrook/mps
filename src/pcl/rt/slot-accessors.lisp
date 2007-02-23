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

;;; What if accessors with the same name are declared for different
;;; direct slots?  Should there be a warning?  ACL gives none.  LW
;;; gives an error.
 
#+cmu
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/slot-accessors.lisp,v 1.2 2003/03/22 16:15:14 gerd Exp $")

(in-package "PCL-TEST")

(defclass sa0 ()
  ((a :accessor a-of :initarg :a)))

(deftest slot-accessor.0
    (let ((instance (make-instance 'sa0 :a 0)))
      (a-of instance))
  0)

(deftest slot-accessor.1
    (let ((instance (make-instance 'sa0)))
      (setf (a-of instance) 1)
      (a-of instance))
  1)

(defmethod sa0.0 ((x sa0))
  (a-of x))

(deftest slot-accessor.2
    (let ((instance (make-instance 'sa0)))
      (setf (a-of instance) 2)
      (sa0.0 instance))
  2)

;;; Redefining the class should update the PV table cache of
;;; method SA0.0 so that is reads the right slot.

(deftest slot-accessor.3
    (progn
      (defclass sa0 ()
	((c :accessor c-of)
	 (a :accessor a-of :initarg :a)
	 (b :accessor b-of)))
      (sa0.0 (make-instance 'sa0 :a 42)))
  42)

(defclass sa1 (sa0)
  ((b :accessor a-of :initarg :b)))
      
(deftest slot-accessor.4
    (let ((instance (make-instance 'sa1 :b 0)))
      (sa0.0 instance))
  0)

(defclass sa2 (sa0)
  ())

(defmethod (setf a-of) (new-value (obj sa2))
  (setf (slot-value obj 'a) (* 2 new-value)))

(defmethod sa2.0 ((obj sa2))
  (setf (a-of obj) 42))

(deftest slot-accessor.5
    (let ((instance (make-instance 'sa2)))
      (sa2.0 instance))
  84)

(defclass sa3 ()
  ())

(defmethod (setf foo-of) (n (obj sa3))
  n)

(defmethod sa3.0 ((obj sa3))
  (setf (foo-of obj) 11))

(deftest slot-accessor.6
    (let ((instance (make-instance 'sa3)))
      (sa3.0 instance))
  11)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sa4 ()
    ((a :initform 0 :accessor sa4-a))))

(defmethod sa4.0 ((x sa4))
  (sa4-a x))

(deftest slot-accessor.7
    (sa4.0 (make-instance 'sa4))
  0)

(deftest slot-accessor.8
    (progn
      (defun sa4-a (x)
	(declare (ignore x))
	11)
      (prog1
	  (sa4.0 (make-instance 'sa4))
	(fmakunbound 'sa4-a)))
  11)


