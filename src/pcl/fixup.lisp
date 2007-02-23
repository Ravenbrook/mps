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

(in-package :pcl)

#-loadable-pcl
(progn
  (/show "Fixing generic functions")
  (fix-early-generic-functions)
  (/show "PCL boot state COMPLETE")
  (setq *boot-state* 'complete))

(/show "Computing standard slot locations")
(compute-standard-slot-locations)

(defun print-std-instance (instance stream depth)
  (declare (ignore depth))
  (print-object instance stream))

(defmethod print-object ((ctor ctor) stream)
  (print-unreadable-object (ctor stream :type t :identity t)
    (format stream "~s ~s ~s"
	    (ctor-class-name ctor)
	    (ctor-initargs ctor)
	    (ctor-state ctor))))

(/show "Converting condition accessors")
(when (fboundp 'conditions::make-early-condition-accessors-generic)
  (let ((*compile-print* nil))
    (conditions::make-early-condition-accessors-generic)))
