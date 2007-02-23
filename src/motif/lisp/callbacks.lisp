;;;; -*- Mode: Lisp ; Package: Toolkit -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/callbacks.lisp,v 1.5 2003/04/11 12:09:10 pmai Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; This file contains all the functions which handle callbacks dispatched
;;; from the C server.
;;;

(in-package "TOOLKIT")



;;;; Functions for registering deferred actions

(defvar *callback-deferred-action* nil)

(defmacro with-callback-deferred-actions (&body forms)
  `(setf *callback-deferred-action* #'(lambda () ,@forms)))

(declaim (inline invoke-deferred-actions))
(defun invoke-deferred-actions ()
  (when *callback-deferred-action*
    (let ((action *callback-deferred-action*))
      (setf *callback-deferred-action* nil)
      (funcall action))))



;;;; Functions which track all registered callbacks
;;;
;;; The actual toolkit functions are provided by functions
;;; %add-callback, %remove-callback, etc.
;;;
;;; The callback hash table is keyed on:
;;;      (widget-id . callback-name)
;;; The callback data is (fn . client-data)

(defun add-callback (widget sym-name fn &rest args)
  "Registers a callback function on the specified widget."
  (declare (type widget widget)
	   (type (or symbol function) fn)
	   (keyword sym-name))
  (let* ((name (symbol-resource sym-name))
	 (table (motif-connection-callback-table *motif-connection*))
	 (key (cons (widget-id widget) name))
	 (data (cons fn args)))

    (unless (member sym-name (widget-callbacks widget))
      (%add-callback widget name)
      (push sym-name (widget-callbacks widget)))
    (setf (gethash key table) (cons data (gethash key table)))))


;; The 'args' list must be EQUAL to the args passed when the callback was
;; created for this callback to be removed.

(defun remove-callback (widget sym-name fn &rest args)
  "Removes a callback function from the specified widget."
  (declare (type widget widget)
	   (type (or symbol function) fn)
	   (keyword sym-name))
  (let* ((name (symbol-resource sym-name))
	 (table (motif-connection-callback-table *motif-connection*))
	 (key (cons (widget-id widget) name))
	 (data (cons fn args))
	 (new-list (delete data (gethash key table) :test #'equal)))

    (setf (gethash key table) new-list)
    (unless new-list
      (%remove-callback widget name)
      (setf (widget-callbacks widget)
	    (delete sym-name (widget-callbacks widget) :test #'eq)))))

(defun remove-all-callbacks (widget sym-name)
  "Removes all callback functions on the specified widget."
  (declare (type widget widget)
	   (keyword sym-name))
  (let* ((name (symbol-resource sym-name))
	 (table (motif-connection-callback-table *motif-connection*))
	 (key (cons (widget-id widget) name)))
    (%remove-callback widget name)
    (setf (gethash key table) nil)
    (setf (widget-callbacks widget)
	  (delete sym-name (widget-callbacks widget) :test #'eq))))

(defun handle-callback (reply)
  (unwind-protect
      (let* ((widget (toolkit-read-value reply))
	     (name (toolkit-read-value reply))
	     (table (motif-connection-callback-table *motif-connection*))
	     (calldata (read-callback-info widget reply)))
	;; Invoke the callback function
	(dolist (callback (gethash (cons (widget-id widget) name) table))
	  (apply (car callback)
		 widget
		 calldata
		 (cdr callback))))
    (unless (motif-connection-terminated *motif-connection*)
      (terminate-callback)
      (invoke-deferred-actions))
    (destroy-message reply)))



;;;; Functions which deal with protocol callbacks
;;;
;;; The protocol table is keyed on:
;;;           (widget property protocol)
;;; The data stored in the table is:
;;;           (fn . call-data)

(defun add-protocol-callback (widget property protocol fn &rest args)
  "Registers a protocol callback function on the specified widget."
  (declare (type widget widget)
	   (type keyword property protocol)
	   (type (or symbol function) fn))
  (let* ((property (xti:symbol-atom property))
         (protocol (xti:symbol-atom protocol))
         (table (motif-connection-protocol-table *motif-connection*))
	 (key (list (widget-id widget) property protocol))
	 (data (cons fn args)))

    (let ((entry (cons property protocol)))
      (unless (member entry (widget-protocols widget))
	(push entry (widget-protocols widget))
	(%add-protocol-callback widget property protocol)))
    (setf (gethash key table) (cons data (gethash key table)))))

(defun remove-protocol-callback (widget property protocol fn &rest args)
  "Removes a protocol callback function on the specified widget."
  (declare (type widget widget)
	   (type keyword property protocol)
	   (type (or symbol function) fn))
  (let* ((property (xti:symbol-atom property))
         (protocol (xti:symbol-atom protocol))
         (table (motif-connection-protocol-table *motif-connection*))
	 (key (list (widget-id widget) property protocol))
	 (data (cons fn args))
	 (new-list (delete data (gethash key table) :test #'equal)))
    (setf (gethash key table) new-list)
    (unless new-list
      (%remove-protocol-callback widget property protocol)
      (setf (widget-protocols widget)
	    (delete (cons property protocol) (widget-protocols widget)
		    :test #'equal)))))

;; (declaim (inline add-wm-protocol-callback remove-wm-protocol-callback))
(defun add-wm-protocol-callback (widget protocol fn &rest args)
  "Registers a window manager protocol callback function on the specified
   widget."
  (declare (type widget widget)
	   (keyword protocol)
	   (type (or symbol function) fn))
  (apply #'add-protocol-callback widget :wm-protocols protocol fn args))

(defun remove-wm-protocol-callback (widget protocol fn &rest args)
  "Removes a window manager protocol callback function on the specified
   widget."
  (declare (type widget widget)
	   (keyword protocol)
	   (type (or symbol function) fn))
  (apply #'remove-protocol-callback widget :wm-protocols protocol fn args))

(defun handle-protocol (reply)
  (unwind-protect
      (let* ((widget (toolkit-read-value reply))
	     (property (toolkit-read-value reply))
	     (protocol (toolkit-read-value reply))
	     (event (toolkit-read-value reply))
	     (table (motif-connection-protocol-table *motif-connection*))
	     (calldata (make-any-callback :reason :cr-protocols :event event)))
	(dolist (callback (gethash (list (widget-id widget) property protocol)
				   table))
	  (apply (car callback)
		 widget
		 calldata
		 (cdr callback))))
    (unless (motif-connection-terminated *motif-connection*)
      (terminate-callback)
      (invoke-deferred-actions))
    (destroy-message reply)))

				   

;;;; Functions for dealing with call-data info

;;; These structures are used to hold the various callback information.
;;; When the server begins processing a callback, it will dump the callback
;;; data into the message in slot order.  The client will unpack the data
;;; and create a callback structure which will be passed to the Lisp
;;; callback as the call-data.  It reason field, possibly together with the
;;; widget class, will be enough to determine what callback structure is
;;; appropriate.  The event slot is the (XEvent *) received in C.  If the
;;; client wants access to the event, there will be some sort of macro such
;;; as (with-event-info ((<callback-struct>) ... <slots to bind>)  ....) or
;;; something.  This will be added later.
(defstruct (any-callback
	    (:print-function print-callback))
  (reason :cr-none :type keyword)
  (event  0 :type (unsigned-byte 32)))

(defun print-callback (callback stream d)
  (declare (ignore d)
	   (stream stream))
  (format stream "#<Motif Callback -- ~a>" (any-callback-reason callback)))

(defstruct (button-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-button-callback (reason event click-count)))
  (click-count 0 :type (unsigned-byte 32))) ; Not always valid data here.

(defstruct (drawing-area-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-drawing-area-callback (reason event window)))
  window)

(defstruct (drawn-button-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-drawn-button-callback
			  (reason event window click-count)))
  window
  (click-count 0 :type (unsigned-byte 32))) ; Not always valid data here.

;;; RowColumnCallbackStruct is weird and probably not necessary

(defstruct (scroll-bar-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-scroll-bar-callback (reason event value pixel)))
  (value 0 :type fixnum)
  (pixel 0 :type fixnum))

(defstruct (toggle-button-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-toggle-button-callback (reason event set)))
  (set nil :type (member t nil)))

;;; ListCallbackStruct is fairly complex
(defstruct (list-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-list-callback (reason event item item-position)))
  (item nil :type (or null xmstring))
  (item-position 0 :type fixnum)
  (selected-items nil :type list)  ;; a list of strings (maybe array?)
  (selected-item-positions nil :type list) ;; of integers
  (selection-type 0 :type fixnum))

;; used for selection-box and command callbacks
(defstruct (selection-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-selection-callback (reason event value)))
  (value nil :type (or null xmstring)))

(defstruct (file-selection-callback
	    (:include selection-callback)
	    (:print-function print-callback)
	    (:constructor make-file-selection-callback
			  (reason event value mask dir pattern)))
  (mask nil :type (or null xmstring))
  (dir nil :type (or null xmstring))
  (pattern nil :type (or null xmstring)))

(defstruct (scale-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-scale-callback (reason event value)))
  (value 0 :type fixnum))

(defstruct (text-callback
	    (:include any-callback)
	    (:print-function print-callback)
	    (:constructor make-text-callback
			  (reason event)))
  (doit t :type (member t nil))
  (curr-insert 0 :type fixnum)
  (new-insert 0 :type fixnum)
  (start-pos 0 :type fixnum)
  (end-pos 0 :type fixnum)
  (text "" :type simple-string)
  format ;; ***** Don't yet know what this is
)

(defun read-callback-info (widget reply)
  (let* ((reason (toolkit-read-value reply))
	 (event (toolkit-read-value reply)))
    (case (widget-type widget)
      ((:arrow-button :arrow-button-gadget :push-button :push-button-gadget)
       (make-button-callback reason event (toolkit-read-value reply)))
      (:drawing-area
       (make-drawing-area-callback reason event (toolkit-read-value reply)))
      (:drawn-button
       (let* ((window (toolkit-read-value reply))
	      (count (toolkit-read-value reply)))
	 (make-drawn-button-callback reason event window count)))
      (:scroll-bar
       (let* ((value (toolkit-read-value reply))
	      (pixel (toolkit-read-value reply)))
	 (make-scroll-bar-callback reason event value pixel)))
      ((:toggle-button :toggle-button-gadget)
       (make-toggle-button-callback reason event (toolkit-read-value reply)))
      (:list
       (let* ((item (toolkit-read-value reply))
	      (item-position (toolkit-read-value reply))
	      (info (make-list-callback reason event item item-position)))
	 (when (or (eq reason :cr-multiple-select)
		   (eq reason :cr-extended-select))
	   (setf (list-callback-selected-items info)
		 (toolkit-read-value reply))
	   (setf (list-callback-selected-item-positions info)
		 (toolkit-read-value reply))
	   (setf (list-callback-selection-type info)
		 (toolkit-read-value reply)))
	 info))
      (:text
       (let ((info (make-text-callback reason event)))
	 (when (member reason '(:cr-losing-focus :cr-modifying-text-value
						 :cr-moving-insert-cursor))
	   (setf (text-callback-doit info) (toolkit-read-value reply))
	   (setf (text-callback-curr-insert info) (toolkit-read-value reply))
	   (setf (text-callback-new-insert info) (toolkit-read-value reply))
	   
	   (case reason
	     (:cr-losing-focus
	      (setf (text-callback-start-pos info) (toolkit-read-value reply))
	      (setf (text-callback-end-pos info) (toolkit-read-value reply)))
	     (:cr-modifying-text-value
	      (setf (text-callback-start-pos info) (toolkit-read-value reply))
	      (setf (text-callback-end-pos info) (toolkit-read-value reply))
	      (setf (text-callback-text info) (toolkit-read-value reply))
	      (setf (text-callback-format info) (toolkit-read-value reply))
	      )))
	 info))
      ((:selection-box :command)
       (make-selection-callback reason event (toolkit-read-value reply)))
      (:file-selection-box
       (let* ((value (toolkit-read-value reply))
	      (mask (toolkit-read-value reply))
	      (dir (toolkit-read-value reply))
	      (pattern (toolkit-read-value reply)))
	 (make-file-selection-callback reason event
				       value mask dir pattern)))
      (:scale
       (make-scale-callback reason event (toolkit-read-value reply)))
      (t nil))))

(defmacro with-callback-event ((event cback) &body forms)
  `(let ((,event (transport-event (any-callback-event ,cback))))
     ,@forms))



;;;; Action table support

(defmacro with-action-event ((event handle) &body forms)
  `(let ((,event (transport-event ,handle)))
     ,@forms))

(defun handle-action (reply)
  (let* ((widget (toolkit-read-value reply))
	 (event-handle (toolkit-read-value reply))
	 (fun-name (toolkit-read-value reply)))
    (unwind-protect
	(funcall (read-from-string fun-name) widget event-handle)
      (unless (motif-connection-terminated *motif-connection*)
	(terminate-callback)
	(invoke-deferred-actions))
      (destroy-message reply))))



;;;; Functions for managing event handlers

(defun add-event-handler (widget event-mask non-maskable function &rest args)
  "Registers an event handler function on the specified widget."
  (declare (type widget widget)
	   (type (or symbol list) event-mask)
	   (type (member t nil) non-maskable)
	   (type (or symbol function) function))
  (when (symbolp event-mask)
    (setf event-mask (list event-mask)))
  (let ((table (motif-connection-event-table *motif-connection*))
	(data (cons function args)))
    (dolist (event-class event-mask)
      (let ((mask (xlib:make-event-mask event-class))
	    (key (cons (widget-id widget) event-class)))

	(unless (member data (gethash key table) :test #'equal)
	  (push data (gethash key table)))
	(unless (member event-class (widget-events widget))
	  (push event-class (widget-events widget))
	  (%add-event-handler widget mask nil))))
    (when non-maskable
      (let ((mask 0) ; NoEventMask
	    (key (cons (widget-id widget) :non-maskable-mask)))
	(unless (member data (gethash key table) :test #'equal)
	  (push data (gethash key table)))
	(unless (member :non-maskable-mask (widget-events widget))
	  (push :non-maskable-mask (widget-events widget))
	  (%add-event-handler widget mask t))))))

(defun remove-event-handler (widget event-mask non-maskable function &rest args)
  "Removes an event handler function on the specified widget."
  (declare (type widget widget)
	   (type (or symbol list) event-mask)
	   (type (member t nil) non-maskable)
	   (type (or symbol function) function))
  (when (symbolp event-mask)
    (setf event-mask (list event-mask)))
  (let ((table (motif-connection-event-table *motif-connection*))
	(data (cons function args)))
    (dolist (event-class event-mask)
      (let* ((mask (xlib:make-event-mask event-class))
	     (key (cons (widget-id widget) event-class))
	     (new-list (delete data (gethash key table) :test #'equal)))
	(setf (gethash key table) new-list)
	(unless new-list
	  (setf (widget-events widget)
		(delete event-class (widget-events widget)))
	  (%remove-event-handler widget mask nil))))
    (when non-maskable
      (let* ((mask 0) ; NoEventMask
	     (key (cons (widget-id widget) :non-maskable-mask))
	     (new-list (delete data (gethash key table) :test #'equal)))
	(setf (gethash key table) new-list)
	(unless new-list
	  (setf (widget-events widget)
		(delete :non-maskable-mask (widget-events widget)))
	  (%remove-event-handler widget mask t))))))

(defun handle-event (reply)
  (unwind-protect
      (let* ((widget (toolkit-read-value reply))
	     (mask (toolkit-read-value reply))
	     (nonmaskable (toolkit-read-value reply))
	     (event (toolkit-read-value reply))
	     (table (motif-connection-event-table *motif-connection*))
	     (event-class))

	(setf event-class (if nonmaskable
			      :non-maskable-mask
			      (car (xlib:make-event-keys mask))))
	(dolist (handler (gethash (cons (widget-id widget) event-class) table))
	  (apply (car handler)
		 widget
		 event
		 (cdr handler))))
    (unless (motif-connection-terminated *motif-connection*)
      (terminate-callback)
      (invoke-deferred-actions))
    (destroy-message reply)))
