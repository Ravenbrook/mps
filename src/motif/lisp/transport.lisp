;;;; -*- Mode: Lisp ; Package: Toolkit-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/transport.lisp,v 1.4 2000/02/15 11:59:24 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; Code for transporting packets and messages between the C toolkit server
;;; and the Lisp client.
;;;

(in-package "TOOLKIT-INTERNALS")



;;;; Data structures

(defconstant *header-size* 12)

(defstruct (packet
	    (:print-function print-packet)
	    (:constructor make-packet (head)))
  (head nil :type system:system-area-pointer)
  (fill *header-size* :type fixnum)
  (next nil :type (or null packet)))

(defstruct (message
	    (:print-function print-message)
	    (:constructor %make-message))
  (packet-count 0 :type fixnum)
  (serial (the fixnum 0) :type (unsigned-byte 32))
  (fill-packet nil :type (or null packet))
  ;;
  ;; NOTE:  This list contains the constituent packets in REVERSE order
  ;; while the message is being constructed.  When it is finished, the list
  ;; is reversed.
  (packet-list nil :type list))

(defun print-packet (p stream d)
  (declare (ignore p d))
  (write-string "#<X Toolkit Packet>" stream))

(defun print-message (r stream d)
  (declare (ignore d)
	   (stream stream))
  (format stream "#<X Toolkit Message - serial ~d>"(message-serial r)))

(defconstant *packet-size* 4096)

(deftype packet-index () `(integer 0 ,*packet-size*))



;;;; Memory management and packet accessors

(defmacro packet-serial (packet)
  `(the (signed-byte 29) (system:signed-sap-ref-32 (packet-head ,packet) 0)))

(defmacro packet-sequence-number (packet)
  `(system:signed-sap-ref-16 (packet-head ,packet) 4))

(defmacro packet-sequence-length (packet)
  `(system:signed-sap-ref-16 (packet-head ,packet) 6))

(defmacro packet-length (packet)
  `(the packet-index (system:signed-sap-ref-32 (packet-head ,packet) 8)))


;;; Free-list for keeping empty packet husks around.
(defvar *free-packets* nil)

(defun create-packet ()
  (if *free-packets*
      (let ((packet *free-packets*))
	(setf *free-packets* (packet-next packet))
	(setf (packet-length packet) *header-size*)
	(setf (packet-fill packet) *header-size*)
	packet)
      (let* ((buffer (system:allocate-system-memory *packet-size*))
	     (packet (make-packet buffer)))
	(setf (packet-length packet) *header-size*)
	packet)))

(defun destroy-packet (packet)
  (setf (packet-next packet) *free-packets*)
  (setf *free-packets* packet))

(declaim (inline make-message))
(defun make-message (serial)
  (declare (type (unsigned-byte 29) serial))
  (let ((message (%make-message)))
    (setf (message-serial message) serial)
    message))




;;;; Functions to stuff things into packets

(macrolet ((def-packet-writer (name size)
	     (let ((sap-ref (ecase size
			      (1 'system:sap-ref-8)
			      (2 'system:sap-ref-16)
			      (4 'system:sap-ref-32)))
		   (bits (* size 8)))
	       `(defun ,name (packet data)
		  (declare (type (or (signed-byte ,bits)
		                     (unsigned-byte ,bits)) data))
		  (let ((fill (system:sap+ (packet-head packet)
					   (packet-fill packet))))
		    (setf (,sap-ref fill 0) data)
		    (incf (packet-fill packet) ,size)
		    (incf (packet-length packet) ,size)))))
	   (def-packet-reader (name size)
	     (let ((sap-ref (ecase size
			      (1 'system:sap-ref-8)
			      (2 'system:sap-ref-16)
			      (4 'system:sap-ref-32)))
		   (bits (* size 8)))
	       `(defun ,name (packet)
		  (let* ((fill (system:sap+ (packet-head packet)
					    (packet-fill packet)))
			 (data (,sap-ref fill 0)))
		    (declare (type (or (signed-byte ,bits)
		                       (unsigned-byte ,bits)) data))
		    (incf (packet-fill packet) ,size)
		    data)))))
  (def-packet-writer packet-put-byte 1)
  (def-packet-writer packet-put-word 2)
  (def-packet-writer packet-put-dblword 4)

  (def-packet-reader packet-get-byte 1)
  (def-packet-reader packet-get-word 2)
  (def-packet-reader packet-get-dblword 4))



;;;; Message management and accessors

(defun create-message (serial)
  (let ((message (make-message serial)))
    (message-add-packet message)
    message))

(defun destroy-message (message)
  (dolist (packet (message-packet-list message))
    (destroy-packet packet)))


(defun message-add-packet (message)
  (let ((packet (create-packet)))
    (push packet (message-packet-list message))
    (setf (message-fill-packet message) packet)
    (incf (message-packet-count message))
    (setf (packet-sequence-number packet) (message-packet-count message))
    ;; PACKET-SEQUENCE-LENGTH will be set when the message is sent
    (setf (packet-serial packet) (message-serial message))))


(macrolet ((def-message-writer (name size)
	     (let ((packet-ref (ecase size
				 (1 'packet-put-byte)
				 (2 'packet-put-word)
				 (4 'packet-put-dblword)))
		   (bits (* size 8)))
	       `(defun ,name (message data)
		  (declare (type (signed-byte ,bits) data))
		  (when (> (packet-length (message-fill-packet message))
			   (- *packet-size* ,size 1))
		    (message-add-packet message))
		  (,packet-ref (message-fill-packet message) data))))
	   (def-message-reader (name size)
	     (let ((packet-ref (ecase size
				 (1 'packet-get-byte)
				 (2 'packet-get-word)
				 (4 'packet-get-dblword))))
	       `(defun ,name (message)
		  (unless (< (packet-fill (message-fill-packet message))
			     (- *packet-size* ,size -1))
		    ;;
		    ;; This is REALLY gross
		    (setf (message-fill-packet message)
			  (cadr (member (message-fill-packet message)
					(message-packet-list message)))))
		  (,packet-ref (message-fill-packet message))))))

  (def-message-writer message-put-byte 1)
  (def-message-writer message-put-word 2)
  (def-message-writer message-put-dblword 4)

  ;; These accessors should only be used in deciphering complete messages.
  ;; Hence, it is assumed that the message IS complete (ie. the packets are
  ;; in normal order).
  (def-message-reader message-get-byte 1)
  (def-message-reader message-get-word 2)
  (def-message-reader message-get-dblword 4))



;;;; Transmission functions

(defun read-some-bytes (socket packet count)
  (declare (type packet-index count))
  (loop
    (when (zerop count) (return))
    (multiple-value-bind
	(bytes-read errnum)
	(unix:unix-read socket (system:sap+ (packet-head packet)
					    (packet-fill packet)) count)
      (declare (type (or null fixnum) bytes-read))
      (unless bytes-read
	(toolkit-error "Encountered error reading packet: ~a"
		       (unix:get-unix-error-msg errnum)))
      (when (zerop bytes-read)
	(error 'toolkit-eof-error :string "Hit EOF while reading packet"))
      (decf count (the fixnum bytes-read))
      (incf (packet-fill packet) (the fixnum bytes-read)))))

(defun write-some-bytes (socket packet)
  (let ((fill 0)
	(count (packet-length packet)))
    (declare (type packet-index fill count))
    (loop
      (when (zerop count) (return))
      (multiple-value-bind
	  (bytes-sent errnum)
	  (unix:unix-write socket (system:sap+ (packet-head packet) fill)
			   0 count)
	(declare (type (or null fixnum) bytes-sent))
	(unless bytes-sent
	  (toolkit-error "Encountered error writing packet: ~a"
			 (unix:get-unix-error-msg errnum)))
	(when (zerop (the fixnum bytes-sent))
	  (error 'toolkit-eof-error :string "Hit EOF while sending packet."))
	(decf count (the fixnum bytes-sent))
	(incf fill (the fixnum bytes-sent))))))

(defun check-packet-sanity (packet)
  (format t "Packet serial is ~a~%" (packet-serial packet))
  (format t "Packet current is ~a~%" (packet-sequence-number packet))
  (format t "Packet total is ~a~%" (packet-sequence-length packet))
  (format t "Packet length is ~a~%" (packet-length packet)))

(declaim (inline transmit-packet receive-packet))
(defun transmit-packet (packet socket)
  (write-some-bytes socket packet ))

(defun receive-packet (socket)
  (let ((packet (create-packet)))
    (setf (packet-fill packet) 0)
    (read-some-bytes socket packet *header-size*)
    (read-some-bytes socket packet (- (packet-length packet) *header-size*))
    (setf (packet-fill packet) *header-size*)
    packet))

(defun transmit-message (message socket)
  ;; First, reverse the packet list so that the packets go out in the right
  ;; order
  (setf (message-packet-list message) (nreverse (message-packet-list message)))
  (let ((packet-count (message-packet-count message)))
    (dolist (packet (message-packet-list message))
      (setf (packet-sequence-length packet) packet-count)
      (transmit-packet packet socket))))

;;; An a-list of (serial . incomplete message)
(defvar *pending-msgs* nil)

(defun kill-deferred-message (packet)
  (declare (ignore packet))
  (warn "Cannot yet handle killing deferred messages."))

(defun defer-packet (packet)
  (let* ((serial (packet-serial packet))
	 (found (assoc serial *pending-msgs*))
	 (message (or (cdr found) (make-message serial))))
    (push packet (message-packet-list message))
    (incf (message-packet-count message))
    (cond ((= (message-packet-count message)(packet-sequence-length packet))
	   (setq *pending-msgs* (delete found *pending-msgs*))
	   (setf (message-packet-list message)
		 ;; this can be nreverse if messages really arrive in order
		 (sort (message-packet-list message) #'<
		       :key (lambda(pkt)(packet-sequence-number pkt))))
	   (setf (message-fill-packet message)
		 (first (message-packet-list message)))
	   message)
	  (t (unless found
	       (setq *pending-msgs* (acons serial message *pending-msgs*)))
	     nil))))

(defun receive-message (socket)
  (loop
    (let* ((first (receive-packet socket))
	   (count (packet-sequence-length first)))
      (cond
       ((zerop count) (kill-deferred-message first))
       ((= count 1)
	(let ((message (make-message (packet-serial first))))
	  (setf (message-packet-count message) 1)
	  (push first (message-packet-list message))
	  (setf (message-fill-packet message) first)
	  (return message)))
       (t
	(let ((message (defer-packet first)))
	  (when message
	    (return message))))))))


;;;; Functions for handling requests

(defun create-next-message ()
  (let ((message (create-message (motif-connection-serial
				  *motif-connection*))))
    (incf (motif-connection-serial *motif-connection*))
    message))

(defun prepare-request (request-op options arg-count)
  (declare (type (unsigned-byte 16) request-op)
	   (type (unsigned-byte 8) arg-count))
  (let ((message (create-next-message)))
    (message-put-word message request-op)
    (message-put-byte message (if (eq options :confirm) 1 0))
    (message-put-byte message arg-count)
    message))
