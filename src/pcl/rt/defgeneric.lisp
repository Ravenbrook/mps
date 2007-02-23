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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/defgeneric.lisp,v 1.4 2003/04/14 21:45:22 gerd Exp $")

(in-package "PCL-TEST")

(defmacro define-gf-lambda-list-test (name lambda-list)
  `(deftest ,name
       (multiple-value-bind (r c)
	   (ignore-errors
	     (defgeneric g ,lambda-list))
	 (values (null r) (typep c 'error)))
     t t))

(define-gf-lambda-list-test defgeneric-lambda-list.0 (a &optional (b 1)))
(define-gf-lambda-list-test defgeneric-lambda-list.1 (a &key (b 1)))
(define-gf-lambda-list-test defgeneric-lambda-list.2 ((a gf-class)))

;;;
;;; CMUCL died with an illegal instruction when creating an instance
;;; of the following class, due to a slot layout that was incompatible
;;; with that of funcallable instances.
;;;
(defclass gf-class (standard-generic-function)
  ()
  (:metaclass pcl::funcallable-standard-class))

(deftest defgeneric-generic-function-class.0
    (progn
      (defgeneric g (a b c)
	(:generic-function-class gf-class))
      t)
  t)

;;;
;;; This used to enter a vicious metacircle.
;;;
(deftest method-class.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defclass method-class.0 (mop:standard-method) ())
	  (defgeneric method-class.0.gf (x)
	    (:method-class method-class.0))
	  (defmethod method-class.0.gf ((x integer)) x)
	  (method-class.0.gf 1))
      (values r (null c)))
  1 t)
