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
  "$Header: /project/cmucl/cvsroot/src/pcl/dlisp3.lisp,v 1.8 2003/05/04 13:11:21 gerd Exp $")
;;;

(in-package :pcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter checking-or-caching-list
  '((T NIL (CLASS) NIL)
    (T NIL (CLASS CLASS) NIL)
    (T NIL (CLASS CLASS CLASS) NIL)
    (T NIL (CLASS CLASS T) NIL)
    (T NIL (CLASS CLASS T T) NIL)
    (T NIL (CLASS CLASS T T T) NIL)
    (T NIL (CLASS T) NIL)
    (T NIL (CLASS T T) NIL)
    (T NIL (CLASS T T T) NIL)
    (T NIL (CLASS T T T T) NIL)
    (T NIL (CLASS T T T T T) NIL)
    (T NIL (CLASS T T T T T T) NIL)
    (T NIL (T CLASS) NIL)
    (T NIL (T CLASS T) NIL)
    (T NIL (T T CLASS) NIL)
    (T NIL (CLASS) T)
    (T NIL (CLASS CLASS) T)
    (T NIL (CLASS T) T)
    (T NIL (CLASS T T) T)
    (T NIL (CLASS T T T) T)
    (T NIL (T CLASS) T)
    (T T (CLASS) NIL)
    (T T (CLASS CLASS) NIL)
    (T T (CLASS CLASS CLASS) NIL)
    (NIL NIL (CLASS) NIL)
    (NIL NIL (CLASS CLASS) NIL)
    (NIL NIL (CLASS CLASS T) NIL)
    (NIL NIL (CLASS CLASS T T) NIL)
    (NIL NIL (CLASS T) NIL)
    (NIL NIL (T CLASS T) NIL)
    (NIL NIL (CLASS) T)
    (NIL NIL (CLASS CLASS) T))))

;;; Rather than compiling the constructors here, just tickle the range
;;; of shapes defined above, leaving the generation of the
;;; constructors to precompile-dfun-constructors.
;;;
(dolist (key checking-or-caching-list)
  (destructuring-bind (cached-emf-p return-value-p metatypes applyp) key
    (multiple-value-bind (args generator)
	(if cached-emf-p
	    (if return-value-p
		(values (list metatypes) 'emit-constant-value)
		(values (list metatypes applyp) 'emit-caching))
	    (if return-value-p
		(values (list metatypes) 'emit-in-checking-p)
		(values (list metatypes applyp) 'emit-checking)))
      (apply #'get-dfun-constructor generator args))))
