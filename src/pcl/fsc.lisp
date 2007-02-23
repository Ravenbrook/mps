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
  "$Header: /project/cmucl/cvsroot/src/pcl/fsc.lisp,v 1.13 2003/05/04 13:11:21 gerd Exp $")
;;;
;;; This file contains the definition of the FUNCALLABLE-STANDARD-CLASS
;;; metaclass.  Much of the implementation of this metaclass is actually
;;; defined on the class STD-CLASS.  What appears in this file is a modest
;;; number of simple methods related to the low-level differences in the
;;; implementation of standard and funcallable-standard instances.
;;;
;;; As it happens, none of these differences are the ones reflected in
;;; the MOP specification; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS
;;; share all their specified methods at STD-CLASS.
;;; 
;;; 
;;; workings of this metaclass and the standard-class metaclass.
;;; 

(in-package :pcl)

(defmethod wrapper-fetcher ((class funcallable-standard-class))
  'fsc-instance-wrapper)

(defmethod slots-fetcher ((class funcallable-standard-class))
  'fsc-instance-slots)

(defmethod raw-instance-allocator ((class funcallable-standard-class))
  'allocate-funcallable-instance)

;;;
;;;
;;;

(defmethod validate-superclass ((fsc funcallable-standard-class)
				(new-super std-class))
  (let ((new-super-meta-class (class-of new-super)))
    (or (eq new-super-meta-class *the-class-std-class*)
	(eq (class-of fsc) new-super-meta-class))))

(defmethod allocate-instance ((class funcallable-standard-class)
			      &rest initargs &key)
  (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (allocate-funcallable-instance (class-wrapper class)))

(defmethod make-reader-method-function ((class funcallable-standard-class)
					slot-name)
  (make-std-reader-method-function (class-name class) slot-name))

(defmethod make-writer-method-function ((class funcallable-standard-class)
					slot-name)
  (make-std-writer-method-function (class-name class) slot-name))

