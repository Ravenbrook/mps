;;;; -*- Mode: Lisp ; Package: Toolkit -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/main.lisp,v 1.2 1994/10/31 04:54:48 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; Various high-level interface functions for use with the Motif toolkit.
;;;

(in-package "TOOLKIT")



;;; These are just randomly placed here at the moment.
(defconstant string-default-charset ""
  "The default character set used in building Motif compound strings.")

(defun font-list-add-component (flist charset font-spec)
  (let ((font (xlib:open-font *x-display* font-spec)))
    (xlib:display-force-output *x-display*)
    (font-list-add flist font charset)))

(defun build-simple-font-list (name font-spec)
  (let ((font (xlib:open-font *x-display* font-spec)))
    (xlib:display-force-output *x-display*)
    (font-list-create font name)))


(defun build-font-list (specs)
  (let* ((first (car specs))
	 (flist (build-simple-font-list (car first) (cadr first)))
	 (specs (cdr specs)))
    (dolist (spec specs)
      (setf flist (font-list-add-component flist (car spec) (cadr spec))))
    flist))



;;;; Some standard useful callbacks

(defun quit-application ()
  "Standard function for quitting an X Toolkit application."
  (quit-server)
  (close-motif-connection *motif-connection*)
  (throw 'lisp::top-level-catcher nil))

(defun quit-application-callback (widget call-data)
  "Standard callback for quitting an X Toolkit application."
  (declare (ignore widget call-data))
  (quit-application))

(defun destroy-callback (widget call-data &rest targets)
  (declare (ignore call-data))
  (if targets
      (dolist (target targets)
	(destroy-widget target))
      (destroy-widget widget)))

(defun manage-callback (widget call-data &rest targets)
  (declare (ignore call-data))
  (if targets
      (apply #'manage-children targets)
      (manage-child widget)))

(defun unmanage-callback (widget call-data &rest targets)
  (declare (ignore call-data))
  (if targets
      (apply #'unmanage-children targets)
      (unmanage-child widget)))

(defun popup-callback (widget call-data kind &rest targets)
  (declare (ignore call-data))
  (if targets
      (dolist (target targets)
	(popup target kind))
      (popup widget kind)))

(defun popdown-callback (widget call-data &rest targets)
  (declare (ignore call-data))
  (if targets
      (dolist (target targets)
	(popdown target))
      (popdown widget)))



;;;; A convenient (and CLM compatible) way to start Motif applications

(defun run-motif-application (init-function
			      &key
			      (init-args nil)
			      (application-class "Lisp")
			      (application-name "lisp")
			      (server-host *default-server-host*)
			      (display *default-display*)
			      (sync-clx *debug-mode*))
  (declare (ignore sync-clx))
  (let ((connection (open-motif-connection server-host display
					   application-name
					   application-class)))
    (with-motif-connection (connection)
      (apply init-function init-args))
    connection))
