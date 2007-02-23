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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/method-combination.lisp,v 1.2 2003/03/22 16:15:14 gerd Exp $")

(in-package "PCL-TEST")

;;; ********************************
;;; Method Group Specifiers ********
;;; ********************************

(define-method-combination mgs0 (x)
  ((primary () :required t))
  (progn
    x
    `(call-method ,(first primary))))

;;; This should simply not signal an error as it did in 18d.

(deftest method-group-specifiers.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (defgeneric mgs0 (obj)
	    (:method-combination mgs0 1))
	  (defmethod mgs0 (obj)
	    obj)
	  (mgs0 1))
      (values r c))
  1 nil)


;;; **************************
;;; :generic-function  *******
;;; **************************


;;; *******************
;;; :arguments  *******
;;; *******************

(defvar *result* nil)

(defvar *mca0-value*
  (define-method-combination mca0 ()
    ((methods *))
    (:arguments x y &optional opt)
    (:generic-function gf)
    `(progn
       (setq *result* (list (pcl:generic-function-name ,gf) ,x ,y ,opt))
       (call-method ,(first methods)))))

(defgeneric mca0 (a)
  (:method-combination mca0)
  (:method (a) a))

(defgeneric mca1 (a b)
  (:method-combination mca0)
  (:method (a b) (list a b)))

(defgeneric mca2 (a &optional b)
  (:method-combination mca0)
  (:method (a &optional b) (list a b)))

(defgeneric mca3 (&optional b)
  (:method-combination mca0)
  (:method (&optional b) b))

(deftest method-combination.0
    *mca0-value*
  mca0)

(deftest method-combination-arguments.0
    (multiple-value-bind (r c)
	(ignore-errors (mca0 1) *result*)
      (values r (null c)))
  (mca0 1 nil nil) t)

(deftest method-combination-arguments.1
    (multiple-value-bind (r c)
	(ignore-errors (mca1 1 2) *result*)
      (values r (null c)))
  (mca1 1 2 nil) t)

(deftest method-combination-arguments.2
    (multiple-value-bind (r c)
	(ignore-errors (mca2 1) *result*)
      (values r (null c)))
  (mca2 1 nil nil) t)

(deftest method-combination-arguments.3
    (multiple-value-bind (r c)
	(ignore-errors (mca2 1 2) *result*)
      (values r (null c)))
  (mca2 1 nil 2) t)

(deftest method-combination-arguments.4
    (multiple-value-bind (r c)
	(ignore-errors (mca3) *result*)
      (values r (null c)))
  (mca3 nil nil nil) t)

(deftest method-combination-arguments.5
    (multiple-value-bind (r c)
	(ignore-errors (mca3 1) *result*)
      (values r (null c)))
  (mca3 nil nil 1) t)

(define-method-combination mca1 ()
  ((methods *))
  (:arguments x y &rest r)
  (:generic-function gf)
  `(progn
     (setq *result* (list (pcl:generic-function-name ,gf) ,x ,y ,r))
     (call-method ,(first methods))))

(defgeneric mca1.0 (&rest b)
  (:method-combination mca1)
  (:method (&rest b) b))

(deftest method-combination-arguments.6
    (multiple-value-bind (r c)
	(ignore-errors (mca1.0) *result*)
      (values r (null c)))
  (mca1.0 nil nil nil) t)

(deftest method-combination-arguments.7
    (multiple-value-bind (r c)
	(ignore-errors (mca1.0 1) *result*)
      (values r (null c)))
  (mca1.0 nil nil (1)) t)

(define-method-combination mca2 ()
  ((methods *))
  (:arguments &key a b)
  (:generic-function gf)
  `(progn
     (setq *result* (list (pcl:generic-function-name ,gf) ,a ,b))
     (call-method ,(first methods))))

(defgeneric mca2.0 (&key a b)
  (:method-combination mca2)
  (:method (&key (a 0) (b 1)) (list a b)))

(deftest method-combination-arguments.8
    (multiple-value-bind (r c)
	(ignore-errors (mca2.0) *result*)
      (values r (null c)))
  (mca2.0 nil nil) t)

(deftest method-combination-arguments.9
    (multiple-value-bind (r c)
	(ignore-errors (mca2.0 :a 1) *result*)
      (values r (null c)))
  (mca2.0 1 nil) t)

(deftest method-combination-arguments.10
    (multiple-value-bind (r c)
	(ignore-errors (mca2.0 :b 1) *result*)
      (values r (null c)))
  (mca2.0 nil 1) t)

(deftest method-combination-arguments.11
    (multiple-value-bind (r c)
	(ignore-errors (mca2.0 :b 1 :a 0) *result*)
      (values r (null c)))
  (mca2.0 0 1) t)

(define-method-combination mca3 ()
   ((methods *))
   (:arguments &whole w x &key k)
   (:generic-function gf)
   `(progn
      (setq *result* (list (pcl:generic-function-name ,gf) ,w ,x ,k))
      (call-method ,(first methods))))

(defgeneric mca3.0 (x &key k)
  (:method-combination mca3)
  (:method (x &key k) (list x k)))

(deftest method-combination-arguments.12
    (multiple-value-bind (r c)
	(ignore-errors (mca3.0 1) *result*)
      (values r (null c)))
  (mca3.0 (1) 1 nil) t)

(deftest method-combination-arguments.13
    (multiple-value-bind (r c)
	(ignore-errors (mca3.0 1 :k 2) *result*)
      (values r (null c)))
  (mca3.0 (1 :k 2) 1 2) t)
