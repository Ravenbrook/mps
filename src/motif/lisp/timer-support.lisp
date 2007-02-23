;;; -*- Mode: CLtL -*-

(ext:file-comment "$Header: /project/cmucl/cvsroot/src/motif/lisp/timer-support.lisp,v 1.3 1994/10/28 22:35:00 ram Exp $")

;;; timers-support.lisp --
;;; Extension to CMUCL Motif Interface. Adding missing call for
;;; 'XtAppAddTimeOut'.
;;;
;;; Copyright (C) Marco Antoniotti 1994
;;;
;;; Author: Marco Antoniotti
;;; 
;;; Address: Robotics Laboratory
;;;          Courant Institute of Mathematical Sciences
;;;          719 Broadway, Room 1220
;;;          New York, NY, 10003


(in-package "TOOLKIT")

;;;----------------------------------------------------------------------------
;;; Timers

(defvar *timers-count* 0
  "The serial number associated to new timers")


(defstruct (timer (:print-function print-timer)
		  (:constructor make-timer
				(callback-proc
				 &optional args
				 &aux (id (incf *timers-count*))))
		  ;; &rest for 'args' would be nicer, but it would
		  ;; mess up the actual interface functions.
		  )
  (id 0 :type fixnum)
  (interval-id 0 :type (unsigned-byte 32))
  callback-proc
  (args () :type list)
  )


(defun print-timer (timer stream level)
  (declare (ignore level))
  (print-unreadable-object
   (timer stream :type t :identity t)
   (format stream "ID: ~D, INTERVAL-ID ~D, PROC: ~S"
	   (timer-id timer)
	   (timer-interval-id timer)
	   (timer-callback-proc timer))))


;;;----------------------------------------------------------------------------
;;; Timer Tables --
;;; Represented as alists. It should be enough. I still need a wrapper
;;; though, in order to make a better interface.

(defstruct (timer-table (:constructor make-timer-table))
  (alist () :type list))


(defun find-timer (timer-id timer-table)
  (when timer-table
    (cdr (assoc timer-id (timer-table-alist timer-table) :test #'=))))
(declaim (inline make-timer-table))


(defun add-timer (timer timer-table)
  (setf (timer-table-alist timer-table)
	(acons (timer-id timer) timer (timer-table-alist timer-table))))
(declaim (inline add-timer))


(defun remove-timer (timer-id timer-table)
  (setf (timer-table-alist timer-table)
	(delete timer-id (timer-table-alist timer-table)
		:key #'car
		:test #'=)))
(declaim (inline remove-timer))

;;;----------------------------------------------------------------------------
;;; handle-timeout --
   
(defun handle-timeout (reply)
  "Handles a TIMEOUT CALLBACK coming from the server"
  (unwind-protect
       (let* ((timer-id (toolkit-read-value reply))
	      (interval-id (toolkit-read-value reply))
	      (timer-table (motif-connection-timer-table
			    *motif-connection*))
	      ;; The above line is actually dangerous, given that I do
	      ;; not read the value of the connection from the reply.
	      (timer (find-timer timer-id timer-table))
	      )
	
	 (if timer
	     (apply (timer-callback-proc timer)
		    timer-id
		    interval-id
		    (timer-args timer))
	     (warn "Timer ~D does not exist anymore." timer-id)))
    (unless (motif-connection-terminated *motif-connection*)
      (terminate-callback)
      (invoke-deferred-actions))
    (destroy-message reply)))


;;;----------------------------------------------------------------------------
;;; Interface

(defun add-timeout (interval timer-callback &rest args)
  (declare (type (signed-byte 32) interval))
  (let ((new-timer (make-timer timer-callback args)))
    (add-timer new-timer (motif-connection-timer-table *motif-connection*))
    (setf (timer-interval-id new-timer)
	  (%add-timeout interval (timer-id new-timer)))
    new-timer))


(defun remove-timeout (timer)
  (%remove-timeout (timer-interval-id timer))
  (remove-timer (timer-id timer)
		(motif-connection-timer-table *motif-connection*))
  t
  )
