;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
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

(file-comment "$Header: /project/cmucl/cvsroot/src/pcl/seal.lisp,v 1.3 2003/05/04 13:11:21 gerd Exp $")

(in-package "PCL")

(define-condition sealed-error (simple-program-error)
  ()
  (:report (lambda (condition stream)
	     (format stream
		     (format nil "~~@<Sealing error: ~?~~@:>"
			     (simple-condition-format-control condition)
			     (simple-condition-format-arguments condition))))))

(defun sealed-error (format-control &rest args)
  (error 'sealed-error :format-control format-control
	 :format-arguments args))

(defgeneric check-seal (object action seal))
(defgeneric applicable-seal-p (seal action))
(defgeneric make-seal (type name action spec))
(defgeneric seal-quality->type (quality))


;;; *****************************************
;;; Adding, Removing, Checking Seals  *******
;;; *****************************************

(defmacro seal (name &rest specifiers)
  `(progn
     (eval-when (:load-toplevel :execute)
       (%seal ',name ',specifiers 'load))
     (eval-when (:compile-toplevel)
       (%seal ',name ',specifiers 'compile))))

(defun %seal (name specifiers time)
  (loop for spec in specifiers
	as quality = (if (consp spec) (car spec) spec)
	as type = (seal-quality->type quality)
	as seal = (make-seal type name quality spec) do
	  (ecase time
	    (compile
	     (push seal (seal-info-seals (seal-info-or-make name))))
	    (load
	     (let ((object (ecase type
			     (class (find-class name))
			     (generic-function (gdefinition name)))))
	       (push seal (plist-value object 'seals)))))))

(defun unseal (object)
  (let ((name nil))
    (when (symbolp object)
      (setq name object)
      (let ((class (find-class object nil)))
	(when class
	  (setf (plist-value object 'seals) nil))))
    (when (generic-function-name-p object)
      (setq name object)
      (setf (plist-value (gdefinition object) 'seals) nil))
    (typecase object
      (class (setq name (class-name object)))
      (generic-function (setq name (generic-function-name object))))
    (when name
      (let ((info (seal-info name)))
	(when info
	  (setf (seal-info-seals info) nil))))))

(defun check-seals (object action)
  (let ((seals (if (or (symbolp object) (consp object))
		   (let ((info (seal-info object)))
		     (when info
		       (seal-info-seals info)))
		   (plist-value object 'seals))))
    (dolist (seal seals)
      (when (applicable-seal-p seal action)
	(check-seal seal object action)))))


;;; ***********************
;;; Built-in Seals  *******
;;; ***********************

(defvar *seal-quality->type*
  '((:subclasses . class)
    (:methods . generic-function)))

(defvar *seal-quality->actions*
  '((:subclasses . (add-direct-subclass remove-direct-subclass
		    expand-defclass))))

(defmethod seal-quality->type (quality)
  (or (cdr (assq quality *seal-quality->type*))
      (error "~@<Invalid sealing specifier ~s.~@:>" quality)))

(defmethod make-seal (type name quality spec)
  (declare (ignore type name spec))
  (make-instance 'seal :quality quality))

(defmethod applicable-seal-p ((seal seal) action)
  (memq action (cdr (assq (seal-quality seal) *seal-quality->actions*))))

(defmethod check-seal ((seal seal) object action)
  (declare (ignore action))
  (sealed-error "~s is sealed wrt ~a" object (seal-quality seal)))

;;; end of seal.lisp
