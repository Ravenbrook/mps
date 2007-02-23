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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/slot-boundp.lisp,v 1.2 2003/03/22 16:15:14 gerd Exp $")

(in-package "PCL-TEST")

(defclass sbp0 ()
  ((a :initarg :a :initform 0)
   (b :initarg :b)
   (c :allocation :class)))

(defmethod sbp0.0 ((x sbp0) slot)
  (null (slot-boundp x slot)))

(deftest slot-boundp.0
    (null (slot-boundp (make-instance 'sbp0) 'a))
  nil)

(define-compiled-test slot-boundp.1
    (null (slot-boundp (make-instance 'sbp0) 'a))
  nil)

(deftest slot-boundp.2
    (null (slot-boundp (make-instance 'sbp0) 'b))
  t)

(define-compiled-test slot-boundp.3
    (multiple-value-bind (r c)
	(ignore-errors (slot-boundp (make-instance 'sbp0) 'b))
      (values (null r) c))
  t nil)

(deftest slot-boundp.4
    (null (slot-boundp (make-instance 'sbp0) 'c))
  t)

(define-compiled-test slot-boundp.5
    (null (slot-boundp (make-instance 'sbp0) 'c))
  t)

(deftest slot-boundp.6
    (sbp0.0 (make-instance 'sbp0) 'b)
  t)

(deftest slot-boundp.7
    (sbp0.0 (make-instance 'sbp0 :a 2) 'a)
  nil)
