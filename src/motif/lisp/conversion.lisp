;;;; -*- Mode: Lisp ; Package: Toolkit-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/conversion.lisp,v 1.5 2000/02/15 11:59:24 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; These are the functions necessary for reading/writing Lisp data objects
;;; into messages for communicating with the C server.
;;;

(in-package "TOOLKIT-INTERNALS")



;;;; Type definitions

;;; This table maps normal type tags into the symbol for their type.
;;; Symbol types include :xid, :int, :widget, etc.
(defvar *type-table* (make-array 80 :element-type 'cons))

(defvar *enum-table* (make-hash-table :test #'eq))



;;;; Functions for extracting/creating typed data in messages

(declaim (inline combine-type-and-data))
(defun combine-type-and-data (type data)
  (declare (type (unsigned-byte 24) data)
	   (keyword type))
  (let ((tag (get type :xtk-type-tag)))
    (declare (type (unsigned-byte 8) tag))
    (logior (ash tag 24) data)))

(declaim (inline extract-type-and-data))
(defun extract-type-and-data (stuff)
  (declare (type (unsigned-byte 32) stuff))
  (let ((tag (ash stuff -24))
	(data (logand stuff #x00FFFFFF)))
    (declare (type (unsigned-byte 8) tag)
	     (type (unsigned-byte 24) data))
    (values tag data)))



;;;; Interface functions for transporting resource values

;;; In non-CMU Lisps, this can be converted into a (declaim (inline ...))
;;; and add a (declaim (notinline ..)) after the function.
; (declaim (maybe-inline toolkit-write-value))

(defun toolkit-write-value (message value &optional (type t))
  (typecase value
    (widget (message-write-widget message value))
    (motif-object (message-write-motif-object message value))
    (simple-string 
     (if (eq type :atom)
	 (message-write-atom message value)
	 (message-write-string message value)))
    (xlib:font (message-write-xid message (xlib:font-id value) :font))
    (xlib:cursor (message-write-xid message (xlib:cursor-id value) :cursor))
    ((unsigned-byte 24)
     (message-put-dblword message (combine-type-and-data :short value)))
    ((or (signed-byte 32) (unsigned-byte 32))
     (message-put-dblword message (combine-type-and-data :int 0))
     (message-put-dblword message value))
    ((member t nil)
     (if (eq type t)
	 (message-write-boolean message value)
	 ;; It's a null list
	 (message-put-dblword message
			      (combine-type-and-data type 0))))
    (list
     (case type
       (:resource-list (message-write-resource-list message value))
       (:resource-names (message-write-resource-names message value))
       (:widget-list (message-write-widget-list message value))
       (:xm-string-table (message-write-xmstring-table message value))
       (:string-table (message-write-string-table message value))
       (:int-list (message-write-int-list message value))
       (t (toolkit-error "Illegal list type -- ~a for ~s" type value))))
    (symbol
     (cond
      ((eq type :atom) (message-write-atom message value))
      ((get value :widget-class) (message-write-widget-class message value))
      ((get value :enum-value) (message-write-enum message value))
      (t (message-write-function message value))))
    (function (message-write-function message value))
    (xlib:color (message-write-color message value))
    (t
     (toolkit-error "Unsupported argument type -- ~a for ~a"
		    (type-of value) value))))

(declaim (inline lookup-function))
(defun lookup-function (fid)
  (aref (motif-connection-function-table *motif-connection*) fid))

(defun toolkit-read-value (message)
  (multiple-value-bind (tag data)
		       (extract-type-and-data (message-get-dblword message))
    (declare (type (unsigned-byte 8) tag)
	     (type (unsigned-byte 24) data))
    (let ((type (cdr (svref *type-table* tag))))
      (case type
	(:widget (find-widget (message-get-dblword message)))
	(:xm-string (find-motif-object (message-get-dblword message) type))
	(:short  data)
	(:atom (xlib:atom-name *x-display* (message-get-dblword message)))
	(:boolean (not (zerop data)))
	(:int (message-get-dblword message))
	(:xid (get-xresource (message-get-dblword message) tag))
	(:function (lookup-function data))
	(:string-token (svref *toolkit-string-table* data))
	((:resource-list :xm-string-table :string-table :int-list)
	 (message-read-list message data))
	(:widget-list (break "Shouldn't be reading WidgetList."))
	(:string (message-read-string message data))
	(:translation-table
	 (find-motif-object (message-get-dblword message) type))
	(:accelerator-table
	 (find-motif-object (message-get-dblword message) type))
	(:font-list (find-motif-object (message-get-dblword message) type))
	(:event (construct-event message))
	(:float (vm::make-single-float (message-get-dblword message)))
	(:color (message-read-color message data))
	(t  ;; assume an enumerated value
	 (let ((table (gethash type *enum-table*)))
	   (unless table
	     (toolkit-error "Unknown or illegal value type -- ~a" type))
	   (cdr (assoc data table))))))))



;;;; Functions for reading/writing strings

(defun packet-write-string (packet string start length)
  (declare (simple-string string)
	   (fixnum start length))
  (kernel:copy-to-system-area string
			      (+ (the fixnum (* start vm:byte-bits))
				 (the fixnum
				      (* vm:vector-data-offset vm:word-bits)))
			      (packet-head packet)
			      (* (packet-fill packet) vm:byte-bits)
			      (* length vm:byte-bits))
  (incf (packet-fill packet) length)
  (incf (packet-length packet) length))

(defun packet-read-string (packet string start length)
  (declare (simple-string string)
	   (fixnum start length))
  (kernel:copy-from-system-area (packet-head packet)
				(* (packet-fill packet) vm:byte-bits)
				string
				(+ (the fixnum (* start vm:byte-bits))
				   (the fixnum
					(* vm:vector-data-offset vm:word-bits)))
				(* length vm:byte-bits))
  (incf (packet-fill packet) length))

(defun message-read-string (message length)
  (declare (fixnum length))
  ;; length includes the '\0'
  (let ((string (make-string (1- length) :initial-element #\Space))
	(packet (message-fill-packet message)))
    (declare (simple-string string))
    (cond ((= 1 (message-packet-count message))
	   (packet-read-string packet string 0 length))
	  ((< length (- *packet-size* *header-size*))
	   (setq packet
		 (setf (message-fill-packet message)
		       (cadr (member packet (message-packet-list message)))))
	   (packet-read-string packet string 0 length))
	  (t
	   ;; This should be optimized to remove the overhead of
	   ;; message-get-byte
	   (dotimes (i (length string))
	       (setf (aref string i)(code-char (message-get-byte message))))
	     (message-get-byte message))) ; the '\0'
    (let ((packet (message-fill-packet message)))
      (let ((pad (- 4 (mod (packet-fill packet) 4))))
      (unless (> pad 3)
	(dotimes (i pad)
	  (packet-get-byte packet))))
    string)))

(defun message-write-string (message string)
  (declare (simple-string string))
  (let ((token (position string *toolkit-string-table* :test #'string=)))
    (if token
	(message-put-dblword message
			     (combine-type-and-data :string-token token))
	(let ((length (1+ (length string))))
	  ;; We put the type header here so that we'll spill over into a new
	  ;; packet if necessary
	  (message-put-dblword message (combine-type-and-data :string length))
	  (let ((packet (message-fill-packet message)))
	    (cond
	     ((< (+ length (packet-length packet)) *packet-size*)
	      (packet-write-string packet string 0 length))
	     ((< length (- *packet-size* *header-size*))
	      (message-add-packet message)
	      (setf packet (message-fill-packet message))
	      (packet-write-string (message-fill-packet message)
				   string 0 length))
	     (t
	      (do ((next 0)
		   (len (min length (- *packet-size* (packet-length packet)))
			(min length (- *packet-size* (packet-length packet)))))
		  ((not (plusp length)))
		(packet-write-string packet string next len)
		(incf next len)
		(decf length len)
		(when (plusp length)
		  (message-add-packet message)
		  (setf packet (message-fill-packet message))))))
	    (let ((pad (- 4 (mod (packet-fill packet) 4))))
	      (unless (> pad 3)
		(dotimes (i pad)
		  (packet-put-byte packet 0)))))))))



;;;; Functions for writing most other types

(defun message-write-widget (message widget)
  (message-put-dblword message (combine-type-and-data :widget 0))
  (message-put-dblword message (widget-id widget)))

(defun message-write-widget-list (message list)
  (declare (list list))
  (let ((length (length list)))
    (message-put-dblword message
			 (combine-type-and-data :widget-list length))
    (dolist (widget list)
      (message-write-widget message widget))))

(defun message-write-widget-class (message widget-class)
  (let ((val (get widget-class :widget-class)))
    (message-put-dblword message
			 (combine-type-and-data :widget-class val))))

(defun message-write-xid (message id type)
  (message-put-dblword message (combine-type-and-data type 0))
  (message-put-dblword message id))

(defun message-write-atom (message atom)
  (declare (type xlib:xatom atom))
  (let ((id (xlib:find-atom *x-display* atom)))
    (unless id
      (setf id (xlib:intern-atom *x-display* atom))
      (xlib:display-force-output *x-display*))
    (message-put-dblword message (combine-type-and-data :atom 0))
    (message-put-dblword message id)))

(defun message-write-enum (message enum)
  (let ((val (get enum :enum-value)))
    (message-put-dblword message (combine-type-and-data :enum val))))

(defun message-write-boolean (message value)
  (let ((intval (if value 1 0)))
    (message-put-dblword message (combine-type-and-data :boolean intval))))

(defun message-write-function (message fn)
  (let ((fn-id (find-function-id fn)))
    (message-put-dblword message (combine-type-and-data :function fn-id))))

(defun message-write-motif-object (message obj)
  (message-put-dblword message
		       (combine-type-and-data (motif-object-type obj) 0))
  (message-put-dblword message (motif-object-id obj)))

(defun message-write-int-list (message list)
  (declare (list list))
  (let ((length (length list)))
    (message-put-dblword message
			 (combine-type-and-data :int-list length))
    (dolist (int list)
      (message-put-dblword message (combine-type-and-data :int 0))
      (message-put-dblword message int))))

(defun message-write-xmstring-table (message list)
  (declare (list list))
  (let ((length (length list)))
    (message-put-dblword message
			 (combine-type-and-data :xm-string-table length))
    (dolist (string list)
      (typecase string
	(simple-string (message-write-string message string))
	(xmstring (message-write-motif-object message string))
	(t (toolkit-error "Invalid entry in XmStringTable -- ~s" string))))))

(defun message-write-string-table (message list)
  (declare (list list))
  (let ((length (length list)))
    (message-put-dblword message
			 (combine-type-and-data :string-table length))
    (dolist (string list)
      (message-write-string message string))))

(defun  message-write-resource-names (message list)
  (declare (list list))
  (let ((length (length list)))
    (message-put-dblword message
			 (combine-type-and-data :resource-names length))
    (dolist (name list)
      (message-write-string message name))))

(defun message-write-resource-list (message list)
  (declare (list list))
  (let ((length (length list)))
    (unless (zerop (mod length 2))
      (toolkit-error "Resource list of odd length -- ~a" list))
    (message-put-dblword message
			 (combine-type-and-data :resource-list length))
    (loop
      (let ((name (first list))
	    (value (second list))
	    (rest (cddr list)))
	(message-write-string message name)
	(toolkit-write-value message value)
	(unless rest (return))
	(setf list rest)))))

(defun message-read-list (message length)
  (declare (fixnum length))
  (let ((list))
    (dotimes (i length)
      (push (toolkit-read-value message) list))
    (nreverse list)))

(defun message-read-color (message red)
  (let* ((green (message-get-word message))
	 (blue (message-get-word message)))
    (xlib:make-color :blue (the single-float (/ blue 65535.0))
		     :red (the single-float (/ red 65535.0))
		     :green (the single-float (/ green 65535.0)))))

(defun message-write-color (message color)
  (let ((green (round (* (xlib:color-green color) 65535)))
	(red (round (* (xlib:color-red color) 65535)))
	(blue (round (* (xlib:color-blue color)) 65535)))
    (declare (type (unsigned-byte 16) green red blue))
    (message-put-dblword message
			 (combine-type-and-data :color red))
    (message-put-word message green)
    (message-put-word message blue)))
		     


;;;; Functions for handling resource ID's

(defun find-function-id (fn)
  (declare (type (or symbol function) fn))
  (let* ((fn-table (motif-connection-function-table *motif-connection*))
	 (pos (position fn fn-table)))
    (declare (vector fn-table))
    (unless pos
      (setf pos (vector-push-extend fn fn-table)))
    pos))

(defun get-xresource (xid tag)
  (let ((kind (car (svref *type-table* tag))))
    (case kind
      (:window (xlib::lookup-window *x-display* xid))
      (:pixmap (xlib::lookup-pixmap *x-display* xid))
      (:cursor (xlib::lookup-cursor *x-display* xid))
      (:colormap (xlib::lookup-colormap *x-display* xid))
      (:font (xlib::lookup-font *x-display* xid))
      (t
       (toolkit-error "Unknown X resource type -- ~a" kind)))))

(defun construct-event (message)
  (let ((packet (message-fill-packet message)))
    (alien:sap-alien (system:sap+ (packet-head packet)
				  (packet-fill packet))
		     xevent)))
