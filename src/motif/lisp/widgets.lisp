;;;; -*- Mode: Lisp ; Package: Toolkit -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/widgets.lisp,v 1.4 1998/03/20 11:33:44 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; These functions provide a nice interface to the underlying primitives
;;; for manipulating widgets.
;;;

(in-package "TOOLKIT")



;;;; Functions for dealing with widget resources

;;; The resource list arguments are of the form:
;;;      ( ... :<resource-name> <resource-value> ...)
;;; eg.  (:label-string "Hello world")

;;; This function takes a resource list and converts the resource symbols
;;; into their appropriate string names
(defun convert-resource-list (resources)
  (when (cdr resources)
    (setf (car resources) (symbol-resource (car resources)))
    (convert-resource-list (cddr resources))))

(defun convert-resource-names (resources)
  (when resources
    (setf (car resources) (symbol-resource (car resources)))
    (convert-resource-names (cdr resources))))

(declaim (inline set-values get-values))
(defun set-values (widget &rest resources)
  "Set the resource values of the specified widget."
  (declare (type widget widget))
  (convert-resource-list resources)
  (%set-values widget resources))

(defun get-values (widget &rest names)
  "Access the resource values of the specified widget."
  (declare (type widget widget))
  (convert-resource-names names)
  (%get-values widget names))

(defmacro with-resource-values ((widget names bindings) &body forms)
  `(progn
     (convert-resource-names ,names)
     (multiple-value-bind ,bindings
			  (values-list (%get-values ,widget ,names))
       ,@forms)))



;;;; Functions for creating/destroying widgets

(declaim (inline create-managed-widget create-widget create-application-shell
		 create-popup-shell destroy-widget))
(defun create-managed-widget (name class parent &rest resources)
  "Creates a new widget and automatically manages it."
  (declare (simple-string name)
	   (keyword class)
	   (type widget parent))
  (convert-resource-list resources)
  (%create-managed-widget name class parent resources))

(defun create-widget (name class parent &rest resources)
  "Creates a new widget."
  (declare (simple-string name)
	   (keyword class)
	   (type widget parent))
  (convert-resource-list resources)
  (%create-widget name class parent resources))

(defun create-application-shell (&rest resources)
  "Creates an application shell."
  (convert-resource-list resources)
  (%create-application-shell resources))

(defun create-popup-shell (name class parent &rest resources)
  "Creates a popup shell."
  (declare (simple-string name)
	   (keyword class)
	   (type widget parent))
  (convert-resource-list resources)
  (%create-popup-shell name class parent resources))

(defun internal-destroy-widget (widget)
  (declare (type widget widget))
  (let ((id (widget-id widget)))
    ;; Destroy all storage for widget's children
    (dolist (child (widget-children widget))
      (internal-destroy-widget child))
    ;; Remove widget's entry in the widget table
    (remhash id (motif-connection-widget-table *motif-connection*))
    ;; Destroy all callback information
    (dolist (cback-name (widget-callbacks widget))
      (remhash (cons id (symbol-resource cback-name))
	       (motif-connection-callback-table *motif-connection*)))
    ;; Destroy all protocol callback information
    (dolist (entry (widget-protocols widget))
      (let ((prop (car entry))
	    (proto (cdr entry)))
	(remhash (list (widget-id widget) prop proto)
		 (motif-connection-protocol-table *motif-connection*))))
    ;; Destroy all event handler information
    (dolist (event-class (widget-events widget))
      (remhash (cons (widget-id widget) event-class)
	       (motif-connection-event-table *motif-connection*)))))

(defun destroy-widget (widget)
  "Destroys the specified widget and all its descendents."
  (declare (type widget widget))
  (let ((parent (widget-parent widget)))
   (when parent
      (setf (widget-children parent)
            (delete widget (widget-children parent)))))
  (%destroy-widget widget)              ;do this first because callbacks
                                        ;require our state to still be around
  (internal-destroy-widget widget))

(declaim (inline manage-children unmanage-children))
(defun manage-children (&rest widgets)
  "Manage multiple children of the same parent."
  (%manage-children widgets))

(defun unmanage-children (&rest widgets)
  "Unmanage multiple children of the same parent."
  (%unmanage-children widgets))



;;;; Motif widget creation convenience functions

(defvar *convenience-auto-manage* nil
  "Controls whether widget convenience functions will automatically manage the
   widgets which they create.")

(eval-when (compile eval)
  (defmacro def-widget-maker (class)
    (let ((fn-name (read-from-string (format nil "CREATE-~a" class))))
      `(progn
	 (declaim (inline ,fn-name))
	 (defun ,fn-name (parent name &rest resources)
	   ,(format nil "Creates a new ~a widget." class)
	   (declare (type widget parent)
		    (simple-string name))
	   (convert-resource-list resources)
	   (if *convenience-auto-manage*
	       (%create-managed-widget name ,class parent resources)
	       (%create-widget name ,class parent resources))))))
  
  (defmacro def-creation-wrapper (class)
    (let ((fn-name (read-from-string (format nil "CREATE-~a" class)))
	  (other (read-from-string (format nil "%CREATE-~a" class))))
      `(progn
	 (declaim (inline ,fn-name))
	 (defun ,fn-name (parent name &rest resources)
	   ,(format nil "Creates a new ~a widget." class)
	   (declare (type widget parent)
		    (simple-string name))
	   (convert-resource-list resources)
	   (,other parent name resources)))))
  ) ;; EVAL-WHEN


(def-widget-maker :arrow-button)
(def-widget-maker :arrow-button-gadget)
(def-widget-maker :bulletin-board)
(def-widget-maker :cascade-button)
(def-widget-maker :cascade-button-gadget)
(def-widget-maker :command)
(def-widget-maker :dialog-shell)
(def-widget-maker :drawing-area)
(def-widget-maker :drawn-button)
(def-widget-maker :file-selection-box)
(def-widget-maker :form)
(def-widget-maker :frame)
(def-widget-maker :label)
(def-widget-maker :label-gadget)
(def-widget-maker :list)
(def-widget-maker :main-window)
(def-widget-maker :menu-shell)
(def-widget-maker :message-box)
(def-widget-maker :paned-window)
(def-widget-maker :push-button)
(def-widget-maker :push-button-gadget)
(def-widget-maker :row-column)
(def-widget-maker :scale)
(def-widget-maker :scroll-bar)
(def-widget-maker :scrolled-window)
(def-widget-maker :selection-box)
(def-widget-maker :separator)
(def-widget-maker :separator-gadget)
(def-widget-maker :text)
(def-widget-maker :text-field)
(def-widget-maker :toggle-button)
(def-widget-maker :toggle-button-gadget)


(def-creation-wrapper :menu-bar)
(def-creation-wrapper :option-menu)
(def-creation-wrapper :radio-box)
(def-creation-wrapper :warning-dialog)
(def-creation-wrapper :bulletin-board-dialog)
(def-creation-wrapper :error-dialog)
(def-creation-wrapper :file-selection-dialog)
(def-creation-wrapper :form-dialog)
(def-creation-wrapper :information-dialog)
(def-creation-wrapper :message-dialog)
(def-creation-wrapper :popup-menu)
(def-creation-wrapper :prompt-dialog)
(def-creation-wrapper :pulldown-menu)
(def-creation-wrapper :question-dialog)
(def-creation-wrapper :scrolled-list)
(def-creation-wrapper :scrolled-text)
(def-creation-wrapper :selection-dialog)
(def-creation-wrapper :working-dialog)



;;;; Misc. widget management functions

(declaim (inline menu-position))
(defun menu-position (widget event)
  "Positions a popup menu according to the position in the given XEvent."
  (declare (type widget widget)
	   (type (or (unsigned-byte 32) toolkit-event) event))

  (%menu-position widget
		    (if (typep event 'toolkit-event)
			(xti:event-handle event)
			event)))
