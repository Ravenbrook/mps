;;;-*-Mode:LISP; Package:(PCL Lisp 1000); Base:10; Syntax:Common-lisp -*-
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
  "$Header: /project/cmucl/cvsroot/src/pcl/fin.lisp,v 1.22 2003/05/07 17:14:24 gerd Exp $")
;;;

  ;;   
;;;;;; FUNCALLABLE INSTANCES
  ;;

#|

Generic functions are instances with meta class funcallable-standard-class.
Instances with this meta class are called funcallable-instances (FINs for
short).  They behave something like lexical closures in that they have data
associated with them (which is used to store the slots) and are funcallable.
When a funcallable instance is funcalled, the function that is invoked is
called the funcallable-instance-function.  The funcallable-instance-function
of a funcallable instance can be changed.

This file implements low level code for manipulating funcallable instances.

It is possible to implement funcallable instances in pure Common Lisp.  A
simple implementation which uses lexical closures as the instances and a
hash table to record that the lexical closures are funcallable instances
is easy to write.  Unfortunately, this implementation adds significant
overhead:

   to generic-function-invocation (1 function call)
   to slot-access (1 function call or one hash table lookup)
   to class-of a generic-function (1 hash-table lookup)

In addition, it would prevent the funcallable instances from being garbage
collected.  In short, the pure Common Lisp implementation really isn't
practical.

Instead, PCL uses a specially tailored implementation for each Common Lisp and
makes no attempt to provide a purely portable implementation.  The specially
tailored implementations are based on the lexical closure's provided by that
implementation and are fairly short and easy to write.

Some of the implementation dependent code in this file was originally written
by someone in the employ of the vendor of that Common Lisp.  That code is
explicitly marked saying who wrote it.

|#

(in-package :pcl)

;;;
;;; The first part of the file contains the implementation dependent code to
;;; implement funcallable instances.  Each implementation must provide the
;;; following functions and macros:
;;; 
;;;    ALLOCATE-FUNCALLABLE-INSTANCE-1 ()
;;;       should create and return a new funcallable instance.  The
;;;       funcallable-instance-data slots must be initialized to NIL.
;;;       This is called by allocate-funcallable-instance and by the
;;;       bootstrapping code.
;;;
;;;    FUNCALLABLE-INSTANCE-P (x)
;;;       the obvious predicate.  This should be an INLINE function.
;;;       it must be funcallable, but it would be nice if it compiled
;;;       open.
;;;
;;;    SET-FUNCALLABLE-INSTANCE-FUNCTION (fin new-value)
;;;       change the fin so that when it is funcalled, the new-value
;;;       function is called.  Note that it is legal for new-value
;;;       to be copied before it is installed in the fin, specifically
;;;       there is no accessor for a FIN's function so this function
;;;       does not have to preserve the actual new value.  The new-value
;;;       argument can be any funcallable thing, a closure, lambda
;;;       compiled code etc.  This function must coerce those values
;;;       if necessary.
;;;       NOTE: new-value is almost always a compiled closure.  This
;;;             is the important case to optimize.
;;;
;;;    FUNCALLABLE-INSTANCE-DATA-1 (fin data-name)
;;;       should return the value of the data named data-name in the fin.
;;;       data-name is one of the symbols in the list which is the value
;;;       of funcallable-instance-data.  Since data-name is almost always
;;;       a quoted symbol and funcallable-instance-data is a constant, it
;;;       is possible (and worthwhile) to optimize the computation of
;;;       data-name's offset in the data part of the fin.
;;;       This must be SETF'able.
;;;       

(declaim (notinline called-fin-without-function))
(defun called-fin-without-function (&rest args)
  (declare (ignore args))
  (error "~@<Attempt to funcall a funcallable instance without first ~
          setting its function.~@:>"))


;;;; Implementation of funcallable instances for CMU Common Lisp:
;;;
(defstruct (pcl-funcallable-instance
	    (:alternate-metaclass kernel:funcallable-instance
				  kernel:random-pcl-class
				  kernel:make-random-pcl-class)
	    (:type kernel:funcallable-structure)
	    (:constructor allocate-funcallable-instance-1 ())
	    (:conc-name nil))
  ;;
  ;; PCL wrapper is in the layout slot.
  ;;
  ;; PCL data vector.
  (pcl-funcallable-instance-slots nil)
  ;;
  ;; The debug-name for this function.
  (funcallable-instance-name nil)
  ;;
  ;; Hash code.
  (hash-code (get-instance-hash-code) :type fixnum))

;;; Note: returns true for non-pcl funcallable structures.
(import 'kernel:funcallable-instance-p)


;;; SET-FUNCALLABLE-INSTANCE-FUNCTION  --  Interface
;;;
;;;    Set the function that is called when FIN is called.
;;;
(defun set-funcallable-instance-function (fin new-value)
  (declare (type function new-value))
  (assert (funcallable-instance-p fin))
  (setf (kernel:funcallable-instance-function fin) new-value))

