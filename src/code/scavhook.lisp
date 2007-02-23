;;; -*- Package: EXT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/scavhook.lisp,v 1.4 1997/11/04 15:05:37 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file implements the ``Scavenger Hook'' extension.
;;;
;;; Written by William Lott
;;;

(in-package "EXT")

(export '(scavenger-hook scavenger-hook-p make-scavenger-hook
	  scavenger-hook-value scavenger-hook-function))

#+(or gengc gencgc)
(progn

(defun scavenger-hook-p (object)
  "Returns T if OBJECT is a scavenger-hook, and NIL if not."
  (scavenger-hook-p object))

(defun make-scavenger-hook (&key value (function (required-argument)))
  "Create a new scavenger-hook with the specified VALUE and FUNCTION.  For
   as long as the scavenger-hook is alive, the scavenger in the garbage
   collector will note whenever VALUE is moved, and arrange for FUNCTION
   to be funcalled."
  (declare (type function function))
  (c::%make-scavenger-hook value function))

(defun scavenger-hook-value (scavhook)
  "Returns the VALUE being monitored by SCAVHOOK.  Can be setf."
  (declare (type scavenger-hook scavhook))
  (scavenger-hook-value scavhook))

(defun (setf scavenger-hook-value) (value scavhook)
  (declare (type scavenger-hook scavhook))
  (setf (scavenger-hook-value scavhook) value))

(defun scavenger-hook-function (scavhook)
  "Returns the FUNCTION invoked when the monitored value is moved.  Can be
   setf."
  (declare (type scavenger-hook scavhook))
  (scavenger-hook-function scavhook))

(defun (setf scavenger-hook-function) (function scavhook)
  (declare (type function function)
	   (type scavenger-hook scavhook))
  (setf (scavenger-hook-function scavhook) function))

); #+gengc progn

#-(or gengc gencgc)
(progn

(defstruct (scavhook
	    (:conc-name scavenger-hook-)
	    (:constructor %make-scavenger-hook (%value function last-addr))
	    (:print-function %print-scavenger-hook))
  ;;
  ;; The value we are monitoring.
  (%value nil :type t)
  ;;
  ;; The function to invoke when the value gets scavenged (i.e. moved)
  (function nil :type function)
  ;;
  ;; The address of where it was last time.
  (last-addr 0 :type (unsigned-byte #.vm:word-bits)))

(defun %print-scavenger-hook (scavhook stream depth)
  (declare (ignore depth))
  (print-unreadable-object (scavhook stream :identity t :type t)))

(eval-when (compile eval)
  (setf (info type kind 'scavenger-hook) :defined)
  (setf (info type builtin 'scavenger-hook) nil))

(deftype scavenger-hook ()
  'scavhook)

(defvar *scavenger-hooks* nil)

(defun make-scavenger-hook (&key value (function (required-argument)))
  (without-gcing
    (let ((scavhook
	   (%make-scavenger-hook value function
				 (kernel:get-lisp-obj-address value))))
      (push (make-weak-pointer scavhook) *scavenger-hooks*)
      scavhook)))

(declaim (inline scavenger-hook-p))
(defun scavenger-hook-p (thing)
  (scavhook-p thing))

(declaim (inline scavenger-hook-value))
(defun scavenger-hook-value (scavhook)
  (scavenger-hook-%value scavhook))

(defun (setf scavenger-hook-value) (value scavhook)
  (without-gcing
    (setf (scavenger-hook-%value scavhook) value)
    (setf (scavenger-hook-last-addr scavhook)
	  (kernel:get-lisp-obj-address value)))
  value)

(defun scavhook-after-gc-hook ()
  (do ((prev nil)
       (next *scavenger-hooks* (cdr next)))
      ((null next))
    (multiple-value-bind (scavhook valid) (weak-pointer-value (car next))
      (cond (valid
	     (let* ((value (scavenger-hook-value scavhook))
		    (addr (kernel:get-lisp-obj-address value)))
	       (unless (= addr (scavenger-hook-last-addr scavhook))
		 (setf (scavenger-hook-last-addr scavhook) addr)
		 (funcall (scavenger-hook-function scavhook))))
	     (setf prev next))
	    (prev
	     (setf (cdr prev) (cdr next)))
	    (t
	     (setf *scavenger-hooks* (cdr next)))))))

(pushnew 'scavhook-after-gc-hook *after-gc-hooks*)

); #-gengc progn
