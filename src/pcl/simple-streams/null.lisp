;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/simple-streams/null.lisp,v 1.2 2003/06/18 09:23:08 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Definition of Null-Simple-Stream

(in-package "STREAM")

(export '(null-simple-stream))

(def-stream-class null-simple-stream (single-channel-simple-stream)
  ())

(declaim (ftype j-read-char-fn null-read-char))
(defun null-read-char (stream eof-error-p eof-value blocking)
  (declare (ignore blocking))
  (lisp::eof-or-lose stream eof-error-p eof-value))

(declaim (ftype j-read-chars-fn null-read-chars))
(defun null-read-chars (stream string search start end blocking)
  (declare (ignore stream string search start end blocking))
  (values 0 :eof))

(declaim (ftype j-unread-char-fn null-unread-char))
(defun null-unread-char (stream relaxed)
  (declare (ignore stream relaxed)))

(declaim (ftype j-write-char-fn null-write-char))
(defun null-write-char (character stream)
  (declare (ignore stream))
  character)

(declaim (ftype j-write-chars-fn null-write-chars))
(defun null-write-chars (string stream start end)
  (declare (ignore string stream))
  (- end start))

(declaim (ftype j-listen-fn null-listen))
(defun null-listen (stream)
  (declare (ignore stream))
  nil)

(defmethod device-open ((stream null-simple-stream) options)
  (with-stream-class (null-simple-stream stream)
    (add-stream-instance-flags stream :simple :input :output)
    ;;(install-single-channel-character-strategy
    ;; stream (getf options :external-format :default) nil)
    (setf (sm j-read-char stream) #'null-read-char
	  (sm j-read-chars stream) #'null-read-chars
	  (sm j-unread-char stream) #'null-unread-char
	  (sm j-write-char stream) #'null-write-char
	  (sm j-write-chars stream) #'null-write-chars
	  (sm j-listen stream) #'null-listen))
  stream)

(defmethod device-buffer-length ((stream null-simple-stream))
  256)

(defmethod device-write ((stream null-simple-stream) buffer
			 start end blocking)
  (declare (ignore buffer blocking))
  (- end start))
