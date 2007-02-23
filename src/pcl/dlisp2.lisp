;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/dlisp2.lisp,v 1.12 2003/05/04 13:11:21 gerd Exp $")
;;;

(in-package :pcl)

(defun emit-reader/writer-function (reader/writer 1-or-2-class class-slot-p)
  (values
   (ecase reader/writer
     (:reader (ecase 1-or-2-class
		(1 (if class-slot-p
		       (emit-reader/writer-macro :reader 1 t)
		       (emit-reader/writer-macro :reader 1 nil)))
		(2 (if class-slot-p
		       (emit-reader/writer-macro :reader 2 t)
		       (emit-reader/writer-macro :reader 2 nil)))))
     (:writer (ecase 1-or-2-class
		(1 (if class-slot-p
		       (emit-reader/writer-macro :writer 1 t)
		       (emit-reader/writer-macro :writer 1 nil)))
		(2 (if class-slot-p
		       (emit-reader/writer-macro :writer 2 t)
		       (emit-reader/writer-macro :writer 2 nil)))))
     (:boundp (ecase 1-or-2-class
		(1 (if class-slot-p
		       (emit-reader/writer-macro :boundp 1 t)
		       (emit-reader/writer-macro :boundp 1 nil)))
		(2 (if class-slot-p
		       (emit-reader/writer-macro :boundp 2 t)
		       (emit-reader/writer-macro :boundp 2 nil))))))
   nil))

(defun emit-one-or-n-index-reader/writer-function
    (reader/writer cached-index-p class-slot-p)
  (values
   (ecase reader/writer
     (:reader
      (if cached-index-p
	  (if class-slot-p
	      (emit-one-or-n-index-reader/writer-macro :reader t t)
	      (emit-one-or-n-index-reader/writer-macro :reader t nil))
	  (if class-slot-p
	      (emit-one-or-n-index-reader/writer-macro :reader nil t)
	      (emit-one-or-n-index-reader/writer-macro :reader nil nil))))
     (:writer
      (if cached-index-p
	  (if class-slot-p
	      (emit-one-or-n-index-reader/writer-macro :writer t t)
	      (emit-one-or-n-index-reader/writer-macro :writer t nil))
	  (if class-slot-p
	      (emit-one-or-n-index-reader/writer-macro :writer nil t)
	      (emit-one-or-n-index-reader/writer-macro :writer nil nil))))
     (:boundp
      (if cached-index-p
	  (if class-slot-p
	      (emit-one-or-n-index-reader/writer-macro :boundp t t)
	      (emit-one-or-n-index-reader/writer-macro :boundp t nil))
	  (if class-slot-p
	      (emit-one-or-n-index-reader/writer-macro :boundp nil t)
	      (emit-one-or-n-index-reader/writer-macro :boundp nil nil)))))
   nil))

(defun emit-checking-or-caching-function (cached-emf-p return-value-p
					  metatypes applyp)
  (values (emit-checking-or-caching-function-preliminary
	   cached-emf-p return-value-p metatypes applyp)
	  t))

(defun emit-checking-or-caching-function-preliminary
    (cached-emf-p return-value-p metatypes applyp)
  (declare (ignore applyp))
  (if cached-emf-p
      (lambda (cache miss-fn)
	(declare (type function miss-fn))
	#'(kernel:instance-lambda (&rest args)
            (declare #.*optimize-speed*)
	    (with-dfun-wrappers (args metatypes)
	      (dfun-wrappers invalid-wrapper-p)
	      (apply miss-fn args)
	      (if invalid-wrapper-p
		  (apply miss-fn args)
		  (let ((emf (probe-cache cache dfun-wrappers 'not-in-cache)))
		    (if (eq emf 'not-in-cache)
			(apply miss-fn args)
			(if return-value-p
			    emf
			    (invoke-emf emf args))))))))
      (lambda (cache emf miss-fn)
	(declare (type function miss-fn))
	#'(kernel:instance-lambda (&rest args)
	    (declare #.*optimize-speed*)
	    (with-dfun-wrappers (args metatypes)
	      (dfun-wrappers invalid-wrapper-p)
	      (apply miss-fn args)
	      (if invalid-wrapper-p
		  (apply miss-fn args)
		  (let ((found-p (not (eq 'not-in-cache
					  (probe-cache cache dfun-wrappers
						       'not-in-cache)))))
		    (if found-p
			(invoke-emf emf args)
			(if return-value-p
			    t
			    (apply miss-fn args))))))))))


(defun emit-default-only-function (metatypes applyp)
  (declare (ignore metatypes applyp))
  (values (lambda (emf)
	    (lambda (&rest args)
	      (invoke-emf emf args)))
	  t))
