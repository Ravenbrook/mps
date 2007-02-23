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
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/make-instance.lisp,v 1.3 2003/04/22 13:16:35 gerd Exp $")

(in-package "PCL-TEST")


;;; *********************
;;; MAKE-INSTANCE  ******
;;; *********************

;;; Test forms in DEFTEST are not compiled, that is, a compiler
;;; macro won't be used in them.  Also, we want tests using
;;; both the optimized constructor functions, and the default.

(defmacro define-mi-test (name form &key values opt-values)
  (let ((optimized-name
	 (let ((*print-case* :upcase)
	       (*print-pretty* nil)
	       (*print-gensym* t))
	   (intern (format nil "~S.OPT" name))))
	(optimized-values (or opt-values values)))
    `(progn
       (defun ,name ()
	 (macrolet ((mi (&rest args)
		      `(funcall #'make-instance ,@args)))
	   ,form))
       (defun ,optimized-name ()
	 (macrolet ((mi (&rest args)
		      `(make-instance ,@args)))
	   ,form))
       (deftest ,name (,name) ,@values)
       (deftest ,optimized-name (,optimized-name)
	 ,@optimized-values))))
    

(defclass m1 ()
  ((a :initarg :a :initarg :both :initform 1)
   (b :initarg :b :initarg :both :initform 2)))

(define-mi-test make-instance.0
    (with-slots (a b) (mi 'm1)
      (values a b))
  :values (1 2))
    
(define-mi-test make-instance.1
    (with-slots (a b) (mi 'm1 :a 3)
      (values a b))
  :values (3 2))
    
(define-mi-test make-instance.2
    (with-slots (a b) (mi 'm1 :b 3)
      (values a b))
  :values (1 3))

(define-mi-test make-instance.3
    (with-slots (a b) (mi 'm1 :b 3 :a 4)
      (values a b))
  :values (4 3))
    
(define-mi-test make-instance.4
    (with-slots (a b) (mi 'm1 :both (list nil))
      (eq a b))
  :values (t))

(defclass m2 (m1)
  ((a :initarg :a :initform 3)))

;;; Overriding slot in subclass -> new initform should be used.

(define-mi-test make-instance.5
    (with-slots (a b) (mi 'm2)
      (values a b))
  :values (3 2))

;;; :BOTH should be inherited by slot A.

(define-mi-test make-instance.6
    (with-slots (a b) (mi 'm2 :both 11)
      (values a b))
  :values (11 11))

(defclass m3 (m2)
  ((a :allocation :class :initform nil)))

;;; Class slot should not be overwritten when there's no initarg for it.
;;; Note that slot A overrides an instance slot A in M2 which itself
;;; overrides an instance slot in M1.

(define-mi-test make-instance.7
    (progn
      (setf (slot-value (pcl:class-prototype (pcl:find-class 'm3)) 'a) 1)
      (with-slots (a b) (mi 'm3)
	(values a b)))
  :values (1 2))

;;; Class slot should be written when there is an initarg for it.

(define-mi-test make-instance.8
    (with-slots (a) (mi 'm3 :a 11)
      a)
  :values (11))

;;; Class slot should be written when there is an initarg for it.

(define-mi-test make-instance.9
    (with-slots (a b) (mi 'm3 :both 12)
      (values a b))
  :values (12 12))

(define-mi-test make-instance.10
    (with-slots (a b) (mi 'm3 :both 13)
      (values a b))
  :values (13 13))

;;; Invalid initialization arguments

(define-mi-test make-instance.11
    (multiple-value-bind (r c)
	(ignore-errors (mi 'm3 :hansi t))
      (values r (typep c 'condition)))
  :values (nil t))

(define-mi-test make-instance.12
    (multiple-value-bind (r c)
	(ignore-errors (mi 'm3 :hansi t :allow-other-keys t))
      (values (slot-value r 'b) (typep c 'condition)))
  :values (2 nil))

;;; Default initargs

(defclass m5 (m1)
  ()
  (:default-initargs :a 'a :b 'b))

(define-mi-test make-instance.13
    (with-slots (a b) (mi 'm5)
      (values a b))
  :values (a b))

(defclass m6 (m5)
  ()
  (:default-initargs :a 'c))

(define-mi-test make-instance.14
    (with-slots (a b) (mi 'm6)
      (values a b))
  :values (c b))

(defclass m7 (m6)
  ((a :allocation :class :initform nil)))

(define-mi-test make-instance.15
    (with-slots (a b) (mi 'm7)
      (values a b))
  :values (c b))

;;; Lexical environment.

(let ((x 0))
  (defclass m8 ()
    ((a :initform (incf x))))
  (defun reset-counter ()
    (setq x 0)))

(define-mi-test make-instance.16
    (progn
      (reset-counter)
      (loop for i below 5
	    collect (slot-value (mi 'm8) 'a)))
  :values ((1 2 3 4 5)))

(defclass m9 ()
  ((a :initarg :a)
   (b :initarg :b)
   (c :initarg :c)
   (d :initarg :d)))

(define-mi-test make-instance.17
    (let* ((x 'x)
	   (instance (mi 'm9 :a () :b x :c '(baz bar foo)
			 :d (lambda () ()))))
      (with-slots (a b c) instance
	(values a b c)))
  :values (nil x (baz bar foo)))

;; After and before methods.

(defclass m10 ()
  ((a :initform 0 :initarg :a)
   (b :initarg :b)
   (c :initform 2 :initarg :c))
  (:default-initargs :c 1))

(defvar *result* ())

(defmethod initialize-instance :before ((x m10) &rest args)
  (declare (ignore args))
  (push (list 'm10 :before (slot-boundp x 'a)
	      (slot-boundp x 'b) (slot-boundp x 'c))
	*result*))

(define-mi-test make-instance.18
    (progn
      (setq *result* ())
      (with-slots (a b c) (mi 'm10 :b 42)
	(values *result* a b c)))
  :values (((m10 :before nil nil nil)) 0 42 1))

(defclass m11 (m10)
  ()
  (:default-initargs :c 11))

(defmethod initialize-instance :before ((x m11) &rest args)
  (declare (ignore args))
  (push (list 'm11 :before (slot-boundp x 'a)
	      (slot-boundp x 'b)
	      (slot-boundp x 'c))
	*result*))

(defmethod initialize-instance :after ((x m11) &rest args)
  (declare (ignore args))
  (push (list 'm11 :after (slot-boundp x 'a)
	      (slot-boundp x 'b)
	      (slot-boundp x 'c))
	*result*))

(define-mi-test make-instance.19
    (progn
      (setq *result* ())
      (with-slots (a b c) (mi 'm11 :b 42)
	(values *result* a b c)))
  :values (((m11 :after t t t)
	    (m10 :before nil nil nil)
	    (m11 :before nil nil nil))
	   0 42 11))

(defclass m12 (m10)
  ()
  (:default-initargs :c 13))

(defmethod initialize-instance :before ((x m12) &rest args)
  (declare (ignore args))
  (setf (slot-value x 'a) 77))

(define-mi-test make-instance.20
    (progn
      (setq *result* ())
      (with-slots (a b c) (mi 'm12 :b 42)
	(values *result* a b c)))
  :values (((m10 :before t nil nil))
	   77 42 13))

(define-mi-test make-instance.21
    (progn
      (setq *result* ())
      (with-slots (a b c) (mi 'm12 :b 41 :c 67)
	(values *result* a b c)))
  :values (((m10 :before t nil nil))
	   77 41 67))

;;; :ALLOW-OTHER-KEYS

(define-mi-test make-instance.22
    (let ((obj (ignore-errors (mi 'm12 :b 41 :allow-other-keys t))))
      (when obj
	(with-slots (a b c) obj
	  (values a b c))))
  :values (77 41 13))


(define-mi-test make-instance.23
    (let ((obj (ignore-errors (mi 'm12 :b 41 :x 11 :allow-other-keys t))))
      (when obj
	(with-slots (a b c) obj
	  (values a b c))))
  :values (77 41 13))

(define-mi-test make-instance.24
    (multiple-value-bind (r c)
	(ignore-errors (mi 'm12 :b 41 :x 11))
      (values r (typep c 'condition)))
  :values (nil t))

(define-mi-test make-instance.25
    (multiple-value-bind (r c)
	(ignore-errors (mi 'm12 :b 41 :x 11 :allow-other-keys nil))
      (values r (typep c 'condition)))
  :values (nil t))

;; Create a constructor, than rename the package of the class it was
;; defined for.

(defpackage "%CTOR"
  (:use "COMMON-LISP"))

(in-package "%CTOR")

(defclass p1 ()
  ((a :initform 0)))

(defun f1 ()
  (make-instance 'p1))

(in-package "PCL-TEST")

(define-mi-test make-instance.26
    (progn
      (rename-package "%CTOR" "%CTOR2")
      (let* ((f (find-symbol "F1" "%CTOR2"))
	     (a (find-symbol "A" "%CTOR2"))
	     (i (funcall f)))
	(prog1
	    (slot-value i a)
	  (rename-package "%CTOR2" "%CTOR"))))
  :values (0))

(defclass stru.0 ()
  ((a :initarg :a :accessor a-accessor)
   (b :initform 2 :reader b-reader))
  (:metaclass structure-class))

(defclass stru.1 (stru.0)
  ((c :initarg :c :writer c-writer :accessor c-accessor))
  (:metaclass structure-class))

(define-mi-test make-instance.27
    (with-slots (a b) (mi 'stru.0)
      (values a b))
  :values (nil 2))

(define-mi-test make-instance.28
    (with-slots (a b) (mi 'stru.0 :a 1)
      (values a b))
  :values (1 2))

(define-mi-test make-instance.29
    (with-slots (a b c) (mi 'stru.1)
      (values a b c))
  :values (nil 2 nil))

(define-mi-test make-instance.30
    (with-slots (a b c) (mi 'stru.1 :a 1 :c 3)
      (values a b c))
  :values (1 2 3))

(deftest make-instance.31
    (let ((*m30* nil))
      (declare (special *m30*))
      (defclass m30 () ())
      (defclass m31 (m30) ())
      (defun f () (make-instance 'm31))
      (compile 'f)
      (f)
      (defmethod initialize-instance :before ((x m30) &rest args)
	(declare (ignore args))
	(declare (special *m30*))
	(setq *m30* t))
      (f)
      *m30*)
  t)

(defclass mi13 ()
  ((s1 :initarg :s1a :initarg :s1b :reader s1)
   (s2 :initarg :s2 :reader s2)))

(define-mi-test make-instance.32
    (with-slots (s1 s2) 
	(make-instance 'mi13 :s2 'a :s1a 'b :s2 'x :s1a 'y :s1b 'z)
      (values s1 s2))
  :values (b a))

;; (setf find-class), class redefinitions
