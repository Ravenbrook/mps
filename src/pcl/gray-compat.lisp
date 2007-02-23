;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/gray-compat.lisp,v 1.1 2003/06/06 16:27:05 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Gray streams compatibility functions for simple-streams

(in-package "STREAM")

(defvar *enable-gray-compat-warnings* nil)

(defmacro define-gray-stream-method (name lambda-list &body body)
  `(defmethod ,name ,lambda-list
     (when *enable-gray-compat-warnings*
       (warn "Called ~S on a simple-stream" ',name))
     ,@body))

(define-gray-stream-method ext:stream-advance-to-column ((stream
							  simple-stream)
							 column)
  (let ((current (charpos stream)))
    (when current
      (dotimes (i (- column current))
	(write-char #\Space stream)))))

(define-gray-stream-method ext:stream-line-length ((stream simple-stream))
  nil)

(define-gray-stream-method ext:stream-file-position ((stream simple-stream)
						     &optional position)
  (if position
      (file-position stream position)
      (file-position stream)))

(define-gray-stream-method ext:stream-clear-output ((stream simple-stream))
  (clear-output stream))

(define-gray-stream-method ext:stream-write-byte ((stream simple-stream)
						  integer)
  (write-byte integer stream))

(define-gray-stream-method ext:stream-finish-output ((stream simple-stream))
  (finish-output stream))

(define-gray-stream-method ext:stream-listen ((stream simple-stream))
  (listen stream))

(define-gray-stream-method ext:stream-write-string ((stream simple-stream)
						    string
						    &optional (start 0) end)
  (write-string string stream
		:start start :end (or end (length string))))

(define-gray-stream-method ext:stream-write-char ((stream simple-stream)
						  character)
  (write-char character stream))

(define-gray-stream-method ext:stream-line-column ((stream simple-stream))
  (charpos stream))

(define-gray-stream-method ext:stream-file-length ((stream simple-stream))
  (file-length stream))

(define-gray-stream-method ext:stream-unread-char ((stream simple-stream)
						   character)
  (unread-char character stream))

(define-gray-stream-method ext:stream-read-sequence ((stream simple-stream)
						     seq
						     &optional (start 0) end)
  (read-sequence seq stream :start start :end end))

(define-gray-stream-method ext:stream-read-line ((stream simple-stream))
  (read-line stream nil :eof))

(define-gray-stream-method ext:stream-peek-char ((stream simple-stream))
  (peek-char nil stream nil :eof))

(define-gray-stream-method ext:stream-read-char-no-hang ((stream
							  simple-stream))
  (read-char-no-hang stream nil :eof))

(define-gray-stream-method ext:stream-read-char ((stream simple-stream))
  (read-char stream nil :eof))

(define-gray-stream-method ext:stream-clear-input ((stream simple-stream))
  (clear-input stream))

(define-gray-stream-method ext:stream-start-line-p ((stream simple-stream))
  (= (charpos stream) 0))

(define-gray-stream-method ext:stream-terpri ((stream simple-stream))
  (write-char #\Newline stream))

(define-gray-stream-method ext:stream-write-sequence ((stream simple-stream)
						      seq
						      &optional (start 0)
						      end)
  (write-sequence seq stream :start start :end end))

(define-gray-stream-method ext:stream-fresh-line ((stream simple-stream))
  (fresh-line stream))

(define-gray-stream-method ext:stream-read-byte ((stream simple-stream))
  (read-byte stream nil :eof))

(define-gray-stream-method ext:stream-force-output ((stream simple-stream))
  (force-output stream))

(provide :gray-compat)
