;;;; -*- Mode: Lisp ; Package: Toolkit-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please
;;; contact Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/internals.lisp,v 1.9 2004/03/23 12:16:49 emarsden Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; This file contains internal functions required to support the Motif
;;; toolkit in Lisp.
;;;

(in-package "TOOLKIT-INTERNALS")



;;;; Special TOOLKIT-ERROR

(define-condition toolkit-error (error) ())

(define-condition simple-toolkit-error (toolkit-error simple-condition) ()
  (:documentation "An error has occurred in the X Toolkit code.")
  (:report (lambda (condition stream)
	     (declare (stream stream))
	     (format stream "A Toolkit error has occurred.~%~?"
		     (simple-condition-format-control condition)
		     (simple-condition-format-arguments condition)))))

(define-condition toolkit-eof-error (toolkit-error)
  ((string :reader toolkit-eof-error-string :initarg :string))
  (:report (lambda (condition stream)
	     (write-line (toolkit-eof-error-string condition) stream))))

;;; TOOLKIT-ERROR  -- Internal
;;; TOOLKIT-CERROR -- Internal
;;;
;;; These functions act just like ERROR and CERROR except that they signal
;;; a TOOLKIT-ERROR instead of a SIMPLE-ERROR.  This is mainly intended for
;;; a graphical debugger which must stop attempting graphical interaction
;;; when a toolkit error occurs.
;;;
(defun toolkit-error (string &rest args)
  (error 'simple-toolkit-error :format-control string :format-arguments args))

(defun toolkit-cerror (continue-string string &rest args)
  (cerror continue-string 'simple-toolkit-error
	  :format-control string :format-arguments args))



;;;; Internal communication functions

(defvar *xt-tcp-port* 8000)

(defun connect-to-host (host pid)
  (declare (type (or null simple-string) host))
  (handler-case
      (if host
	  (handler-case
	      (ext:connect-to-inet-socket host *xt-tcp-port*)
	    (error ()
	      (ext:connect-to-inet-socket host (+ *xt-tcp-port*
						  (unix:unix-getuid)))))
	  (handler-case
	      (ext:connect-to-unix-socket
	       (if pid
		   (format nil "/tmp/.motif_socket-p~D" pid)
		   (format nil "/tmp/.motif_socket-u~D" (unix:unix-getuid))))
	    (error ()
	      (ext:connect-to-unix-socket "/tmp/.motif_socket"))))
    (error ()
      (toolkit-error "Unable to connect to Motif server."))))

(declaim (inline wait-for-input wait-for-input-or-timeout))
(defun wait-for-input (fd)
  (system:wait-until-fd-usable fd :input))

(defun wait-for-input-or-timeout (fd interval)
  (system:wait-until-fd-usable fd :input interval))



;;;; Toolkit connection stuff

;;; These will be dynamically bound in the context of the event handlers.
(defvar *motif-connection*)
(defvar *x-display*)

(defstruct (motif-connection
	    (:print-function print-motif-connection)
	    (:constructor make-motif-connection (fd)))
  fd
  (display-name "" :type simple-string)
  display
  (serial 1 :type fixnum)
  (terminated nil :type (member t nil))
  (close-hook nil :type (or symbol function))
  ;;
  ;; This maps widget ids (unsigned-byte 32)'s into widget structures
  ;; It has to have :test #'eql, not #'eq, because otherwise result widgets
  ;; from calls aren't necessarily found in the table. An example is
  ;; (get-values foo :menu-bar)
  (widget-table (make-hash-table :test #'eql) :type hash-table)
  (function-table (make-array 32 :element-type '(or symbol function)
			      :adjustable t :fill-pointer 0))
  (callback-table (make-hash-table :test #'equal) :type hash-table)
  (protocol-table (make-hash-table :test #'equal) :type hash-table)
  (event-table (make-hash-table :test #'equal) :type hash-table)
  ;; This table tracks all the misc. id's we get from the server
  ;; ie. xm-strings, translations, accelerators, font-lists
  ;; Needs to be #'eql for same reasons as widget-table
  (id-table (make-hash-table :test #'eql) :type hash-table))

(defun print-motif-connection (c stream d)
  (declare (ignore d)
	   (stream stream))
  (format stream "#<X Toolkit Connection, fd=~a>"
	  (motif-connection-fd c)))



;;;; Internal structure definitions

(defstruct (widget
	    (:print-function print-widget)
	    (:constructor make-widget (id)))
  (id          0 :type (unsigned-byte 32))
  (type      nil :type symbol)
  (parent    nil :type (or null widget))
  (children  nil :type list)
  (callbacks nil :type list)
  (protocols nil :type list)
  (events    nil :type list)
  (user-data nil))

;; A toolkit object is simply a wrapper for a pointer passed from the server
;; process.  The TYPE field allows us to discriminate the type of the pointer
;; but still treat all pointers in the same way (ie. instead of having separate
;; tables for xmstring, font-list, etc.)
;;
(defstruct (motif-object
	    (:print-function print-motif-object)
	    (:constructor make-motif-object (id)))
  (id 0 :type (unsigned-byte 32))
  (type nil :type symbol))

(defun print-widget (w stream d)
  (declare (ignore d)
	   (stream stream))
  (format stream "#<X Toolkit Widget: ~A ~X>" (widget-type w) (widget-id w)))

(defun print-motif-object (obj stream d)
  (declare (ignore d)
	   (stream stream))
  (format stream "#<Motif object: ~A ~X>"
	  (motif-object-type obj) (motif-object-id obj)))

;;; Some handy type abbreviations for motif-object
(deftype xmstring () 'motif-object)
(deftype font-list () 'motif-object)
(deftype translations () 'motif-object)
(deftype accelerators () 'motif-object)



;;;; Tables for tracking stuff

(defvar *toolkit-string-table* (make-array 350 :element-type 'simple-string
					   :adjustable t :fill-pointer 0))

(defun find-widget (id)
  (declare (type (unsigned-byte 32) id))
  (let* ((widget-table (motif-connection-widget-table *motif-connection*))
	 (widget (gethash id widget-table)))
    (unless widget
      (setf widget (make-widget id))
      (setf (gethash id widget-table) widget))
    widget))

(defun find-motif-object (id type)
  (declare (type (unsigned-byte 32) id)
	   (symbol type))
  (let* ((table (motif-connection-id-table *motif-connection*))
	 (object (gethash id table)))
    (unless object
      (setf object (make-motif-object id))
      (setf (gethash id table) object))
    (setf (motif-object-type object) type)
    object))



;;;; Various helpful goodies

;;; Converts a symbol into a resource class
;;;    ex.  :label-string ===> LabelString
(defun symbol-class (symbol)
  (delete #\- (string-capitalize (symbol-name symbol))))

;;; Converts a symbol into a resource base-name.
;;;    ex.  :label-string ===> labelString
(defun symbol-resource (symbol)
  (let ((resource (symbol-class symbol)))
    (setf (schar resource 0) (char-downcase (schar resource 0)))
    resource))

;;; Converts a symbol into an atom.
(defun symbol-atom (symbol)
  (intern (substitute #\_ #\- (symbol-name symbol)) :KEYWORD))

(defun widget-add-child (parent child)
  (declare (type widget parent child))
  (setf (widget-parent child) parent)
  (push child (widget-children parent)))
