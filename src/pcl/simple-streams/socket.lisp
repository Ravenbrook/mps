;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/simple-streams/socket.lisp,v 1.2 2003/06/26 13:27:43 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Socket-simple-stream and socket-base-simple-stream

(in-package "STREAM")

(export '(socket-simple-stream socket-base-simple-stream))

(def-stream-class socket-simple-stream (dual-channel-simple-stream)
  ())

(def-stream-class socket-base-simple-stream (dual-channel-simple-stream)
  ())

(defmethod device-open ((stream socket-simple-stream) options)
  (let ((remote-host (getf options :remote-host))
	(remote-port (getf options :remote-port)))
    (unless (and remote-host remote-port)
      (error "~S requires :remote-host and :remote-port arguments"
	     'socket-simple-stream))
    (with-stream-class (socket-simple-stream stream)
      (ecase (getf options :direction :input)
	(:input (add-stream-instance-flags stream :input))
	(:output (add-stream-instance-flags stream :output))
	(:io (add-stream-instance-flags stream :input :output)))
      (let ((fd (ext:connect-to-inet-socket remote-host remote-port :stream)))
	(when fd
	  (add-stream-instance-flags stream :dual :simple)
	  (when (any-stream-instance-flags stream :input)
	    (setf (sm input-handle stream) fd)
	    (unless (sm buffer stream)
	      (let ((length (device-buffer-length stream)))
		(setf (sm buffer stream) (allocate-buffer length)
		      (sm buffpos stream) 0
		      (sm buffer-ptr stream) 0
		      (sm buf-len stream) length))))
	  (when (any-stream-instance-flags stream :output)
	    (setf (sm output-handle stream) fd)
	    (unless (sm out-buffer stream)
	      (let ((length (device-buffer-length stream)))
		(setf (sm out-buffer stream) (allocate-buffer length)
		      (sm outpos stream) 0
		      (sm max-out-pos stream) length)))
	    (setf (sm control-out stream) *std-control-out-table*))
	  (ext:finalize stream
			(lambda ()
			  (ext:close-socket fd)
			  (format *terminal-io* "~&;;; ** closed socket ~D~%"
				  fd)))
	  ;; this should be done with (setf stream-external-format)
	  (let ((efmt (getf options :external-format :default)))
	    (compose-encapsulating-streams stream efmt)
	    (install-dual-channel-character-strategy (melding-stream stream)
						     efmt))
	  stream)))))

(defmethod device-close ((stream socket-simple-stream) abort)
  (with-stream-class (socket-simple-stream stream)
    (ext:close-socket (or (sm input-handle stream)
			  (sm output-handle stream)))
    (when (sm buffer stream)
      (free-buffer (sm buffer stream))
      (setf (sm buffer stream) nil))
    (when (sm out-buffer stream)
      (free-buffer (sm out-buffer stream))
      (setf (sm out-buffer stream) nil))))

(defmethod device-open ((stream socket-base-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-write ((stream socket-base-simple-stream) buffer
                         start end blocking)
  ;; @@2
  (call-next-method))
