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

(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/reinitialize-instance.lisp,v 1.2 2003/03/22 16:15:14 gerd Exp $")
 
(in-package "PCL-TEST")

(deftest reinitialize-instance.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri0 () ((a :initarg :a)))
	  (reinitialize-instance (make-instance 'ri0) :a 1))
      (values (null r) (typep c 'error)))
  nil nil)

(deftest reinitialize-instance.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri1 () ())
	  (reinitialize-instance (make-instance 'ri1) :a 1))
      (values (null r) (typep c 'error)))
  t t)

(deftest reinitialize-instance.2
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri2 () ())
	  (defmethod shared-initialize ((x ri2) slots &rest initargs &key a)
	    (declare (ignore slots initargs a)))
	  (reinitialize-instance (make-instance 'ri2) :a 1))
      (values (null r) (typep c 'error)))
  nil nil)

(deftest reinitialize-instance.3
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri3 () ())
	  (defmethod reinitialize-instance :after ((x ri3) &rest initargs
						   &key a)
	    (declare (ignore initargs a)))
	  (reinitialize-instance (make-instance 'ri3) :a 1))
      (values (null r) (typep c 'error)))
  nil nil)

(deftest reinitialize-instance.4
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri4 () ())
	  (defmethod reinitialize-instance :after ((x ri4) &rest initargs
						   &key a &allow-other-keys)
	    (declare (ignore initargs a)))
	  (reinitialize-instance (make-instance 'ri4) :a 1 :b 2))
      (values (null r) (typep c 'error)))
  nil nil)

(deftest reinitialize-instance.5
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass ri5 () ())
	  (reinitialize-instance (make-instance 'ri4)
				 :a 1 :b 2 :allow-other-keys t))
      (values (null r) (typep c 'error)))
  nil nil)
