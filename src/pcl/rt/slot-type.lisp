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

(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/slot-type.lisp,v 1.2 2003/03/22 16:15:14 gerd Exp $")
 
(in-package "PCL-TEST")

#+gerds-pcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq pcl::*use-slot-types-p* t))

;;; Check that we check slot types, at least sometimes.

(defclass stype ()
  ((a :type fixnum :initform 0 :initarg :a)))

(defmethod stype.0 ((obj stype))
  (slot-value obj 'a))

(defmethod stype.1 ((obj stype) value)
  (setf (slot-value obj 'a) value))

(deftest slot-type.0
    (multiple-value-bind (r c)
	(ignore-errors
	  (stype.0 (make-instance 'stype :a 1)))
      (values r (null c)))
  1 t)

(deftest slot-type.1
    (multiple-value-bind (r c)
	(ignore-errors
	  (stype.0 (make-instance 'stype :a 1.0)))
      (values r (typep c 'error)))
  nil t)

(deftest slot-type.2
    (multiple-value-bind (r c)
	(ignore-errors
	  (stype.1 (make-instance 'stype) 1))
      (values r (typep c 'error)))
  1 nil)

(deftest slot-type.3
    (multiple-value-bind (r c)
	(ignore-errors
	  (stype.1 (make-instance 'stype) 1.0))
      (values r (typep c 'error)))
  nil t)

