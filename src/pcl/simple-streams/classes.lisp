;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/simple-streams/classes.lisp,v 1.5 2005/10/06 13:49:26 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Base class and generic function definitions for simple-streams

(in-package "STREAM")

(eval-when (:compile-toplevel)
  (pushnew 'compile pcl::*defclass-times*)
  (pushnew 'compile pcl::*defgeneric-times*))


;;;; Types for buffer and strategy functions

(deftype simple-stream-buffer ()
  '(or sys:system-area-pointer (kernel:simple-unboxed-array (*))))

(deftype blocking ()
  `(member t nil :bnb))

(deftype j-listen-fn ()
  '(function (simple-stream) boolean))

(deftype j-read-char-fn ()
  '(function (simple-stream boolean t boolean) t)) ; may return EOF-VALUE

(deftype j-read-chars-fn ()
  '(function (simple-stream string (or character null) fixnum fixnum blocking)
	     (values fixnum &optional (member nil t :eof))))

(deftype j-write-char-fn ()
  '(function ((or character null) simple-stream) (or character null)))

(deftype j-write-chars-fn ()
  '(function (string simple-stream fixnum fixnum) t)) ; return chars-written?

(deftype j-unread-char-fn ()
  '(function (simple-stream t) t)) ; "relaxed" arg is boolean?  what return?

;;;; Base simple-stream classes

(def-stream-class simple-stream (standard-object stream)
  ((%flags :initform 0 :type fixnum)
   (plist :initform nil :type list :accessor stream-plist)

   (j-listen :initform #'lisp::ill-in-any :type j-listen-fn)
   (j-read-char :initform #'lisp::ill-in-any :type j-read-char-fn)
   (j-read-chars :initform #'lisp::ill-in-any :type j-read-chars-fn)
   (j-unread-char :initform #'lisp::ill-in-any :type j-unread-char-fn)
   (j-write-char :initform #'lisp::ill-out-any :type j-write-char-fn) ;@@
   (j-write-chars :initform #'lisp::ill-out-any :type j-write-chars-fn) ;@@

   (oc-state :initform nil)
   (co-state :initform nil)
   (external-format :initform (find-external-format :default))

   (input-handle :initform nil :initarg :input-handle
		 :type (or null fixnum stream)
		 :accessor stream-input-handle)
   (output-handle :initform nil :initarg :output-handle
		  :type (or null fixnum stream)
		  :accessor stream-output-handle)
   (control-in :initform nil :type (or null simple-vector))
   (control-out :initform nil :type (or null simple-vector))

   (melded-stream :type (or null simple-stream))
   (melding-base :type (or null simple-stream))

   (encapsulated-char-read-size :initform 0 :type fixnum)
   (last-char-read-size :initform 0 :type fixnum)
   (charpos :initform 0 :type (or null integer)
	    :accessor stream-line-column)
   (record-end :initform nil :type (or null fixnum))

   (buffer :initform nil :type (or simple-stream-buffer null))
   (buffpos :initform 0 :type fixnum)
   (buffer-ptr :initform 0 :type fixnum)
   (buf-len :initform 0 :type fixnum)

   (pending :initform nil :type list)
   (handler :initform nil :type (or null lisp::handler))))

(def-stream-class single-channel-simple-stream (simple-stream)
  ((mode :initform 0 :type fixnum)))

(def-stream-class dual-channel-simple-stream (simple-stream)
  ((out-buffer :initform nil :type (or simple-stream-buffer null))
   (outpos :initform 0 :type fixnum)
   (max-out-pos :initform 0 :type fixnum)
   (mode :initform 0 :type fixnum)))

(def-stream-class string-simple-stream (simple-stream)
  ())


;;;; Generic function definitions

(defgeneric device-open (stream options)
  (:documentation "Write me"))

(defgeneric device-close (stream abort)
  (:documentation "Write me"))

(defgeneric device-buffer-length (stream)
  (:documentation "Write me"))

(defgeneric device-file-position (stream)
  (:documentation "Write me"))

(defgeneric (setf device-file-position) (value stream)
  (:argument-precedence-order stream value)
  (:documentation "Write me"))

(defgeneric device-file-length (stream)
  (:documentation "Write me"))

(defgeneric device-read (stream buffer start end blocking)
  (:documentation "Write me"))

(defgeneric device-clear-input (stream buffer-only)
  (:documentation "Write me"))

(defgeneric device-write (stream buffer start end blocking)
  (:documentation "Write me"))

(defgeneric device-clear-output (stream)
  (:documentation "Write me"))

(defgeneric device-finish-record (stream blocking action)
  (:documentation "Write me"))


(defmethod shared-initialize :after ((instance simple-stream) slot-names
				     &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names))
  (unless (slot-boundp instance 'melded-stream)
    (setf (slot-value instance 'melded-stream) instance)
    (setf (slot-value instance 'melding-base) instance))
  (unless (device-open instance initargs)
    (device-close instance t)))

(defmethod print-object ((object simple-stream) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (cond ((not (any-stream-instance-flags object :simple))
	   (princ "Invalid " stream))
	  ((not (any-stream-instance-flags object :input :output))
	   (princ "Closed " stream)))
    (format stream "~:(~A~)" (type-of object))))

(defmethod device-close :around ((stream simple-stream) abort)
  (with-stream-class (simple-stream stream)
    (when (any-stream-instance-flags stream :input :output)
      (when (any-stream-instance-flags stream :output)
	(ignore-errors (if abort
			   (clear-output stream)
			   (finish-output stream))))
      (call-next-method)
      (setf (sm input-handle stream) nil
	    (sm output-handle stream) nil)
      (remove-stream-instance-flags stream :input :output)
      (ext:cancel-finalization stream)
      (setf (stream-external-format stream) :void))))

(defmethod device-close ((stream simple-stream) abort)
  (declare (ignore abort))
  t)

(defmethod device-buffer-length ((stream simple-stream))
  4096)

(defmethod device-file-position ((stream simple-stream))
  (with-stream-class (simple-stream stream)
    (sm buffpos stream)))

(defmethod (setf device-file-position) (value (stream simple-stream))
  (with-stream-class (simple-stream stream)
    (setf (sm buffpos stream) value)))

(defmethod device-file-length ((stream simple-stream))
  nil)

(defmethod (setf stream-external-format) :before (value (stream simple-stream))
  ;; (unless (eq value (sm external-format stream))
  ;;   flush out the existing external-format
  )

(defmethod (setf stream-external-format) :after
    (value (stream single-channel-simple-stream))
  (compose-encapsulating-streams stream value)
  (install-single-channel-character-strategy (melding-stream stream)
					     value nil))

(defmethod (setf stream-external-format) :after
    (value (stream dual-channel-simple-stream))
  (compose-encapsulating-streams stream value)
  (install-dual-channel-character-strategy (melding-stream stream) value))


(defmethod device-read ((stream single-channel-simple-stream) buffer
                        start end blocking)
  (read-octets stream buffer start end blocking))

(defmethod device-read ((stream dual-channel-simple-stream) buffer
                        start end blocking)
  (read-octets stream buffer start end blocking))

(defmethod device-clear-input ((stream simple-stream) buffer-only)
  (declare (ignore buffer-only))
  nil)

(defmethod device-write ((stream single-channel-simple-stream) buffer
                         start end blocking)
  ;; buffer may be :flush to force/finish-output
  (when (or (and (null buffer) (not (eql start end)))
	    (eq buffer :flush))
    (with-stream-class (single-channel-simple-stream stream)
      (setf buffer (sm buffer stream))
      (setf end (sm buffpos stream))))
  (write-octets stream buffer start end blocking))

(defmethod device-write ((stream dual-channel-simple-stream) buffer
                         start end blocking)
  ;; buffer may be :flush to force/finish-output
  (when (or (and (null buffer) (not (eql start end)))
	    (eq buffer :flush))
    (with-stream-class (dual-channel-simple-stream stream)
      (setf buffer (sm out-buffer stream))
      (setf end (sm outpos stream))))
  (write-octets stream buffer start end blocking))

(defmethod device-clear-output ((stream simple-stream))
  nil)


;;;; Fixups when Gray streams support is present

(when (find-class 'ext:fundamental-stream nil)
  (defmethod stream-element-type ((stream stream:simple-stream))
    '(unsigned-byte 8))
  (defmethod open-stream-p ((stream stream:simple-stream))
    (any-stream-instance-flags stream :input :output))
  (defmethod close ((stream stream:simple-stream) &key abort)
    (stream:device-close stream abort))
  (defmethod input-stream-p ((stream stream:simple-stream))
    (any-stream-instance-flags stream :input))
  (defmethod output-stream-p ((stream stream:simple-stream))
    (any-stream-instance-flags stream :output))
) ; WHEN
