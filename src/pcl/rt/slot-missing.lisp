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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/slot-missing.lisp,v 1.3 2003/06/17 11:08:00 gerd Exp $")

(in-package "PCL-TEST")

;;; in method (pv table optimization)
;;; in compiled defun
;;; uncompiled.

(defmacro define-sm-test (name (instance class) access &rest values)
  (let* ((*print-case* :upcase)
	 (*print-pretty* nil)
	 (*print-gensym* t)
	 (method-name (intern (format nil "~S.METHOD" name)))
	 (method-test (intern (format nil "~S.METHOD-TEST" name)))
	 (compiled-test (intern (format nil "~S.COMPILED" name))))
    `(progn
       (defmethod ,method-name ((,instance ,class))
	 ,access)
       (deftest ,name
	   (multiple-value-bind (r c)
	       (let ((,instance (make-instance ',class)))
		 (ignore-errors ,access))
	     (values r (typep c 'condition)))
	 ,@values)
       (deftest ,method-test
	   (multiple-value-bind (r c)
	       (let ((,instance (make-instance ',class)))
		 (ignore-errors (,method-name ,instance)))
	     (values r (typep c 'condition)))
	 ,@values)
       (define-compiled-test ,compiled-test
	   (multiple-value-bind (r c)
	       (let ((,instance (make-instance ',class)))
		 (ignore-errors ,access))
	     (values r (typep c 'condition)))
	 ,@values))))

(defclass sm0 () ())

(define-sm-test slot-missing.0 (instance sm0)
  (slot-value instance 'a)
  nil t)

(define-sm-test slot-missing.1 (instance sm0)
  (setf (slot-value instance 'a) 1)
  nil t)

(define-sm-test slot-missing.2 (instance sm0)
  (slot-boundp instance 'a)
  nil t)

(defclass sm1 () ())

(defvar *sm-result* nil)

(defmethod slot-missing (class (obj sm1) slot-name operation
			 &optional new-value)
  (setq *sm-result* (list slot-name operation new-value)))

(define-sm-test slot-missing.3 (instance sm1)
  (progn
    (slot-value instance 'a)
    *sm-result*)
  (a slot-value nil) nil)

(define-sm-test slot-missing.4 (instance sm1)
  (progn
    (setf (slot-value instance 'a) 1)
    *sm-result*)
  (a setf 1) nil)

(define-sm-test slot-missing.5 (instance sm1)
  (progn
    (slot-boundp instance 'a)
    *sm-result*)
  (a slot-boundp nil) nil)
