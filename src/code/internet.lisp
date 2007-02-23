;;; -*- Log: code.log; Package: extensions -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/internet.lisp,v 1.51 2006/03/17 02:56:45 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains an interface to internet domain sockets.
;;;
;;; Written by William Lott.
;;;

(in-package "EXTENSIONS")

(use-package "ALIEN")
(use-package "C-CALL")

(export '(htonl ntohl htons ntohs lookup-host-entry host-entry
	  host-entry-name host-entry-aliases host-entry-addr-list
	  host-entry-addr ip-string create-unix-socket connect-to-unix-socket
	  create-unix-listener accept-unix-connection create-inet-socket
	  connect-to-inet-socket create-inet-listener accept-tcp-connection
	  close-socket ipproto-tcp ipproto-udp inaddr-any add-oob-handler
	  remove-oob-handler remove-all-oob-handlers
	  send-character-out-of-band

	  inet-recvfrom inet-sendto inet-shutdown
	  shut-rd shut-wr shut-rdwr))


#-svr4
(defconstant sock-stream 1)
#+svr4
(defconstant sock-stream 2)
#-svr4
(defconstant sock-dgram 2)
#+svr4
(defconstant sock-dgram 1)
#-svr4
(defconstant sock-raw 3)
#+svr4
(defconstant sock-raw 4)
#+linux
(defconstant sock-rdm 4)
#+linux
(defconstant sock-seqpacket 5)
#+linux
(defconstant sock-packet 10)

(defconstant af-unix 1)        ; Unix domain sockets
(defconstant af-inet 2)        ; Internet IP Protocol
#+linux
(progn
  (defconstant af-ax25 3)      ; Amateur Radio AX.25
  (defconstant af-ipx 4)       ; Novell IPX
  (defconstant af-appletalk 5) ; Appletalk DDP
  (defconstant af-netrom 6)    ; Amateur radio NetROM
  (defconstant af-bridge 7)    ; Multiprotocol bridge
  (defconstant af-aal5 9)      ; Reserved for Werner's ATM
  (defconstant af-x25 9)       ; Reserved for X.25 project
  (defconstant af-inet6 10)    ; IP version 6
  (defconstant af-max 12))
 
(defconstant msg-oob 1)
(defconstant msg-peek 2)
(defconstant msg-dontroute 4)
#+linux
(defconstant msg-proxy 16)

;; For errors with socket stuff.
(define-condition socket-error (simple-error)
  ((errno :reader socket-errno
	  :initarg :errno
	  :initform 0)))
  
  
(defvar *internet-protocols*
  '((:stream    6 #.sock-stream)
    (:datagram 17 #.sock-dgram))
  "AList of socket kinds and protocol values.")

(defun internet-protocol (kind)
  (when (eq kind :data-gram) ; Sep-2000. Remove someday.
    (warn "Internet protocol :DATA-GRAM is deprecated. Using :DATAGRAM")
    (setq kind :datagram))
  (let ((entry (assoc kind *internet-protocols*)))
    (unless entry
      (error "Invalid kind (~S) for internet domain sockets." kind))
    (values (cadr entry)
	    (caddr entry))))


(defmacro maybe-byte-swap (var bytes)
  (ecase (c:backend-byte-order c:*backend*)
    (:big-endian
     var)
    (:little-endian
     (let ((ldbs nil))
       (dotimes (i bytes `(logior ,@ldbs))
	 (push `(ash (ldb (byte 8 ,(* i 8)) ,var)
		     ,(* (- bytes 1 i) 8))
	       ldbs))))))

(declaim (inline htonl ntohl htons ntohs))

(defun htonl (x)
  (maybe-byte-swap x 4))
(defun ntohl (x)
  (maybe-byte-swap x 4))
(defun htons (x)
  (maybe-byte-swap x 2))
(defun ntohs (x)
  (maybe-byte-swap x 2))


;;;; Host entry operations.

;;; Note the IP addresses are stored in host order.
(defstruct host-entry
  name
  aliases
  addr-type
  addr-list)

(defun host-entry-addr (host)
  (declare (type host-entry host))
  (car (host-entry-addr-list host)))

#-BSD
(def-alien-type unix-sockaddr
  (struct nil
    (family #-(or linux alpha) short #+(or linux alpha)  unsigned-short)
    (path (array char 108))))
#|
struct  sockaddr_un {
        u_char  sun_len;                /* sockaddr len including null */
        u_char  sun_family;             /* AF_UNIX */
        char    sun_path[104];          /* path name (gag) */
};
|#

#+BSD
(def-alien-type unix-sockaddr
    (struct nil
      (sun-len unsigned-char)
      (family unsigned-char)
      (path (array char 104))))
#|
/*
 * Socket address, internet style.
 */
struct sockaddr_in {
        u_char  sin_len;
        u_char  sin_family;
        u_short sin_port;
        struct  in_addr sin_addr;
        char    sin_zero[8];
};
struct in_addr {
        u_long s_addr;
};

|#
#+BSD
(def-alien-type inet-sockaddr
    (struct nil
      (sin-len unsigned-char)
      (family  unsigned-char)
      (port    unsigned-short)
      (addr    unsigned-long)
      (zero    (array char 8))))

#-BSD
(def-alien-type inet-sockaddr
  (struct nil
    (family #-alpha short #+alpha unsigned-short)
    (port unsigned-short)
    (addr #-alpha unsigned-long #+alpha unsigned-int)
    (zero (array char 8))))


(def-alien-type hostent
  (struct nil
    (name c-string)
    (aliases (* c-string))
    (addrtype int)
    (length int)
    (addr-list (* (* (unsigned 32))))))

(def-alien-routine "gethostbyname" (* hostent)
  (name c-string))

(def-alien-routine "gethostbyaddr" (* hostent)
  (addr unsigned-long :copy)
  (len int)
  (type int))

#+(and x86 linux)
(def-alien-routine get-h-errno c-call:int)

#-(and x86 linux)
(progn
  (def-alien-variable "h_errno" c-call:int)
  (defun get-h-errno ()
    h-errno))

(defun lookup-host-entry (host)
  "Return a host-entry for the given host. The host may be an address
  string or an IP address in host order."
  (declare (type (or host-entry string (unsigned-byte 32)) host)
	   (optimize (inhibit-warnings 3)))
  (if (typep host 'host-entry)
      host
      (with-alien
	  ((hostent (* hostent) 
		    (etypecase host
		      (string
		       (gethostbyname host))
		      ((unsigned-byte 32)
		       (gethostbyaddr (htonl host) 4 af-inet)))))
	(if (zerop (sap-int (alien-sap hostent)))
	    (values nil (get-h-errno))
	    (values
	     (make-host-entry
	      :name (slot hostent 'name)
	      :aliases
	      (collect ((results))
		(iterate repeat ((index 0))
		  (declare (type kernel:index index))
		  (cond ((or (zerop (sap-int (alien-sap (slot hostent 'aliases))))
			     (zerop (deref (cast (slot hostent 'aliases)
						 (* (unsigned #-alpha 32
							      #+alpha 64)))
					   index)))
			 (results))
			(t
			 (results (deref (slot hostent 'aliases) index))
			 (repeat (1+ index))))))
	      :addr-type (slot hostent 'addrtype)
	      :addr-list
	      (collect ((results))
		(iterate repeat ((index 0))
		  (declare (type kernel:index index))
		  (cond ((zerop (deref (cast (slot hostent 'addr-list)
					     (* (unsigned #-alpha 32 #+alpha 64)))
				       index))
			 (results))
			(t
			 (results 
			  (ntohl (deref (deref (slot hostent 'addr-list) index))))
			 (repeat (1+ index)))))))
	     t)))))

(defun ip-string (addr)
  (format nil "~D.~D.~D.~D"
	  (ldb (byte 8 24) addr)
	  (ldb (byte 8 16) addr)
	  (ldb (byte 8 8) addr)
	  (ldb (byte 8 0) addr)))

(defun create-unix-socket (&optional (kind :stream))
  (multiple-value-bind (proto type)
		       (internet-protocol kind)
    (declare (ignore proto))
    (let ((socket (unix:unix-socket af-unix type 0)))
      (when (minusp socket)
	(error 'socket-error
	       :format-control "Error creating socket: ~A"
	       :format-arguments (list (unix:get-unix-error-msg))
	       :errno (unix:unix-errno)))
      socket)))

(defun connect-to-unix-socket (path &optional (kind :stream))
  (declare (simple-string path))
  (let ((socket (create-unix-socket kind)))
    (with-alien ((sockaddr unix-sockaddr))
      (setf (slot sockaddr 'family) af-unix)
      (kernel:copy-to-system-area path
				  (* vm:vector-data-offset vm:word-bits)
				  (alien-sap (slot sockaddr 'path))
				  0
				  (* (1+ (length path)) vm:byte-bits))
      (when (minusp (unix:unix-connect socket
				       (alien-sap sockaddr)
				       (alien-size unix-sockaddr :bytes)))
	(unix:unix-close socket)
	(error 'socket-error
	       :format-control "Error connecting socket to [~A]: ~A"
	       :format-arguments (list path (unix:get-unix-error-msg))
	       :errno (unix:unix-errno)))
      socket)))

(defun create-unix-listener (path &optional (kind :stream)
                             &key (backlog 5))
  (declare (simple-string path))
  (let ((socket (create-unix-socket kind)))
    (with-alien ((sockaddr unix-sockaddr)) ;; I'm here (MSM)
      (setf (slot sockaddr 'family) af-unix)
      (kernel:copy-to-system-area path
				  (* vm:vector-data-offset vm:word-bits)
				  (alien-sap (slot sockaddr 'path))
				  0
				  (* (1+ (length path)) vm:byte-bits))
      (when (minusp (unix:unix-bind socket
				    (alien-sap sockaddr)
				    (+ (alien-size inet-sockaddr :bytes)
				       (length path))))
	(unix:unix-close socket)
	(error "Error binding socket to path ~a: ~a"
	       path
	       (unix:get-unix-error-msg))))
    (when (eq kind :stream)
      (when (minusp (unix:unix-listen socket backlog))
	(unix:unix-close socket)
	(error "Error listening to socket: ~A" (unix:get-unix-error-msg))))
    socket))

(defun accept-unix-connection (unconnected)
  (declare (fixnum unconnected))
  #+MP (mp:process-wait-until-fd-usable unconnected :input)
  (with-alien ((sockaddr unix-sockaddr))
    (let ((connected (unix:unix-accept unconnected
				       (alien-sap sockaddr)
				       (alien-size unix-sockaddr :bytes))))
      (when (minusp connected)
	(error "Error accepting a connection: ~A" (unix:get-unix-error-msg)))
      (values connected (slot sockaddr 'path)))))

(defun create-inet-socket (&optional (kind :stream))
  (multiple-value-bind (proto type)
		       (internet-protocol kind)
    (let ((socket (unix:unix-socket af-inet type proto)))
      (when (minusp socket)
	(error 'socket-error
	       :format-control"Error creating socket: ~A"
	       :format-arguments (list (unix:get-unix-error-msg))
	       :errno (unix:unix-errno)))
      socket)))

(defun connect-to-inet-socket (host port &optional (kind :stream))
  "The host may be an address string or an IP address in host order."
  (let* ((addr (if (stringp host)
		   (host-entry-addr (or (lookup-host-entry host)
					(error "Unknown host: ~S." host)))
		   host))
	 (socket (create-inet-socket kind)))
    (with-alien ((sockaddr inet-sockaddr))
      (setf (slot sockaddr 'family) af-inet)
      (setf (slot sockaddr 'port) (htons port))
      (setf (slot sockaddr 'addr) (htonl addr))
      (when (minusp (unix:unix-connect socket
				       (alien-sap sockaddr)
				       (alien-size inet-sockaddr :bytes)))
        ;; unix-close may affect errno; save the current value(s)
        (let ((errno (unix:unix-errno))
              (errmsg (unix:get-unix-error-msg)))
          (unix:unix-close socket)
          (error 'socket-error
                 :format-control "Error connecting socket to [~A:~A]: ~A"
                 :format-arguments (list (if (stringp host)
                                             host
					     (ip-string addr))
                                         port
                                         errmsg)
                 :errno errno)))
      socket)))

;; An attempt to rewrite connect-to-inet-socket in such a way that
;; connection attempts that take a long time will not be stuck in the
;; connect(2) system call preventing CMUCL from running other lisp
;; threads.
;;
;; The strategy here is to put the socket in non-blocking mode, and
;; then call connect, which should immediately return.  Then there are
;; three cases to handle:
;;
;;   I.   The connect failed immediately.
;;   II.  The connect succeeded immediately.
;;   III. The connect returned without finishing.
;;
;; Cases I and II are simple enough, we just return the socket or
;; signal an error.
;;
;; In case III, we call SYSTEM:WAIT-UNTIL-FD-IS-USABLE to block just
;; the current thread until the socket is writeable (which signals
;; that the connect has finished, one way or the other).
;;
;; Once WAIT-UNTIL-FD-IS-USABLE returns, we put the socket back into
;; blocking mode and try to determine whether the connect succeeded or
;; failed (and why it failed).  We use an approach described at
;; <http://cr.yp.to/docs/connect.html>, which is attributed to Douglas
;; C. Schmidt and Ken Keys.
;;
;; First call getpeername, and if it returns 0 the connect succeeded,
;; otherwise it failed. If it failed, we try to read a single
;; character from the socket. We know it can't work, but it should
;; cause errno set to the real reason for the failure.

(defun connect-to-inet-socket/non-blocking (host port &optional (kind :stream))
   "The host may be an address string or an IP address in host order."
   (let ((addr (if (stringp host)
                 (host-entry-addr (or (lookup-host-entry host)
                                      (error "Unknown host: ~S." host)))
                 host))
         (socket (create-inet-socket kind)))
     (labels ((set-blocking (socket)
                (unix:unix-fcntl socket unix:f-setfl
                                 (logior (unix:unix-fcntl socket unix:f-getfl 0)
                                         unix:fndelay)))
              (unset-blocking (socket)
                (unix:unix-fcntl socket unix:f-setfl
                                 (logandc2 (unix:unix-fcntl socket unix:f-getfl 0)
                                           unix:fndelay)))
              (dotted-quad (ipaddr)
                (let ((naddr (htonl ipaddr)))
                  (format nil "~D.~D.~D.~D"
                          (ldb (byte 8 0) naddr)
                          (ldb (byte 8 8) naddr)
                          (ldb (byte 8 16) naddr)
                          (ldb (byte 8 24) naddr))))
              (connect-error (addr reason errno)
                (error 'socket-error
                       :format-control "Error connecting socket to [~A:~A]: ~A"
                       :format-arguments (list addr port reason)
                       :errno errno)))
       (set-blocking socket)
       (with-alien ((sockaddr inet-sockaddr)
                    (length (alien:array unsigned 1)))
         (setf (slot sockaddr 'family) af-inet)
         (setf (slot sockaddr 'port) (htons port))
         (setf (slot sockaddr 'addr) (htonl addr))
         (let ((retval (unix:unix-connect socket
                                          (alien-sap sockaddr)
                                          (alien-size inet-sockaddr :bytes))))
           (cond ((< retval -1)
                  ;; connect failed
                  (let ((reason (unix:get-unix-error-msg))
			(errno (unix:unix-errno)))
                    (unix:unix-close socket)
                    (connect-error (if (stringp host) host (dotted-quad addr))
				   reason errno)))
                 ((= retval -1)
                  ;; connect is in progress
                  (system:wait-until-fd-usable socket :output)
                  (unset-blocking socket)
                  ;; OK, it's done, check whether it worked
                  (when (minusp
                         (unix:unix-getpeername socket (alien-sap sockaddr)
                                                (alien-sap length)))
                    (unix:unix-close socket)
                    ;; It didn't, so let's find out why
                    (unix:unix-read socket (alien-sap length) 1)
                    (connect-error (if (stringp host) host (dotted-quad addr))
                                   (unix:get-unix-error-msg)
                                   (unix:unix-errno)))
                  socket)
                 (t
                  ;; connect succeeded
                  (unset-blocking socket)
                  socket)))))))

;;; Socket levels.
(defconstant sol-socket #+linux 1 #+(or solaris bsd hpux irix) #xffff)

;;; Socket options.
(defconstant so-reuseaddr #+linux 2 #+(or solaris bsd hpux irix) 4)

(defun get-socket-option (socket level optname)
  "Get an integer value socket option."
  (declare (type unix:unix-fd socket)
	   (type (signed-byte 32) level optname))
  (with-alien ((optval signed))
    (if (minusp (unix:unix-getsockopt socket level optname
				      (alien-sap (addr optval)) 4))
	(values nil (unix:unix-errno))
	(values optval 0))))

(defun set-socket-option (socket level optname optval)
  "Set an integer value socket option."
  (declare (type unix:unix-fd socket)
	   (type (signed-byte 32) level optname optval))
  (with-alien ((optval signed optval))
    (if (minusp (unix:unix-setsockopt socket level optname
				      (alien-sap (addr optval)) 4))
	(values nil (unix:unix-errno))
	(values optval 0))))

(defun create-inet-listener (port &optional (kind :stream)
				  &key
				  (host 0)
				  reuse-address
				  (backlog 5)
				  )
  (let ((socket (create-inet-socket kind))
        (addr (if (stringp host)
		  (host-entry-addr (or (lookup-host-entry host)
				       (error 'socket-error
					      :format-control "Unknown host: ~S."
					      :format-arguments (list host)
                                              :errno (unix:unix-errno))))
		  host)))
    (when reuse-address
      (multiple-value-bind (optval errno)
	  (set-socket-option socket sol-socket so-reuseaddr 1)
	(or optval (error 'socket-error
			  :format-control "Error ~S setting socket option on socket ~D."
			  :format-arguments (list (unix:get-unix-error-msg errno)
						  socket)
			  :errno errno))))
    (with-alien ((sockaddr inet-sockaddr))
      (setf (slot sockaddr 'family) af-inet)
      (setf (slot sockaddr 'port) (htons port))
      (setf (slot sockaddr 'addr) (htonl addr))
      (when (minusp (unix:unix-bind socket
				    (alien-sap sockaddr)
				    (alien-size inet-sockaddr :bytes)))
	(let ((errno (unix:unix-errno)))
	  (unix:unix-close socket)
	  (error 'socket-error
		 :format-control "Error binding socket to port ~A: ~A"
		 :format-arguments (list port
					 (unix:get-unix-error-msg))
		 :errno errno))))
    (when (eq kind :stream)
      (when (minusp (unix:unix-listen socket backlog))
	(let ((errno (unix:unix-errno)))
	  (unix:unix-close socket)
	  (error 'socket-error
		 :format-control "Error listening to socket: ~A"
		 :format-arguments (list (unix:get-unix-error-msg))
		 :errno errno))))
    socket))

(defun accept-tcp-connection (unconnected)
  (declare (fixnum unconnected))
  #+MP (mp:process-wait-until-fd-usable unconnected :input)
  (with-alien ((sockaddr inet-sockaddr))
    (let ((connected (unix:unix-accept unconnected
				       (alien-sap sockaddr)
				       (alien-size inet-sockaddr :bytes))))
      (let ((errno (unix:unix-errno)))
	(when (minusp connected)
	  (error 'socket-error
		 :format-control "Error accepting a connection: ~A"
		 :format-arguments (list (unix:get-unix-error-msg))
		 :errno errno))
	(values connected (ntohl (slot sockaddr 'addr)))))))

(defun close-socket (socket)
  (multiple-value-bind (ok err)
		       (unix:unix-close socket)
    (unless ok
      (error 'socket-error
	     :format-control "Error closing socket: ~A"
	     :format-arguments (list (unix:get-unix-error-msg err))
	     :errno (unix:unix-errno))))
  (undefined-value))

(defun get-peer-host-and-port (fd)
  "Return the peer host address and port in host order."
  (with-alien ((sockaddr inet-sockaddr)
	       (length (alien:array unsigned 1)))
    (setf (deref length 0) (alien-size inet-sockaddr :bytes))
    (when (minusp (unix:unix-getpeername fd (alien-sap sockaddr)
					 (alien-sap length)))
      (error "Error ~s getting peer host and port on FD ~d."
	     (unix:get-unix-error-msg (unix:unix-errno)) fd))
    (values (ext:ntohl (slot sockaddr 'addr))
	    (ext:ntohs (slot sockaddr 'port)))))

(defun get-socket-host-and-port (fd)
  (with-alien ((sockaddr inet-sockaddr)
	       (length (alien:array unsigned 1)))
    (setf (deref length 0) (alien-size inet-sockaddr :bytes))
    (when (minusp (unix:unix-getsockname fd (alien-sap sockaddr)
					 (alien-sap length)))
      (error "Error ~s getting socket host and port on FD ~d."
	     (unix:get-unix-error-msg (unix:unix-errno)) fd))
    (values (ext:ntohl (slot sockaddr 'addr))
	    (ext:ntohs (slot sockaddr 'port)))))


;;;; Out of Band Data.

;;; Two level AList. First levels key is the file descriptor, second levels
;;; key is the character. The datum is the handler to call.

(defvar *oob-handlers* nil)

;;; SIGURG-HANDLER -- internal
;;;
;;;   Routine that gets called whenever out-of-band data shows up. Checks each
;;; file descriptor for any oob data. If there is any, look for a handler for
;;; that character. If any are found, funcall them.

(defun sigurg-handler (signo code scp)
  (declare (ignore signo code scp))
  (let ((buffer (make-string 1))
	(handled nil))
    (declare (simple-string buffer))
    (dolist (handlers *oob-handlers*)
      (declare (list handlers))
      (cond ((minusp (unix:unix-recv (car handlers) buffer 1 msg-oob))
	     (cerror "Ignore it"
		     "Error recving oob data on ~A: ~A"
		     (car handlers)
		     (unix:get-unix-error-msg)))
	    (t
	     (setf handled t)
	     (let ((char (schar buffer 0))
		   (handled nil))
	       (declare (base-char char))
	       (dolist (handler (cdr handlers))
		 (declare (list handler))
		 (when (eql (car handler) char)
		   (funcall (cdr handler))
		   (setf handled t)))
	       (unless handled
		 (cerror "Ignore it"
			 "No oob handler defined for ~S on ~A"
			 char
			 (car handlers)))))))
    (unless handled
      (cerror "Ignore it"
	      "Got a SIGURG, but couldn't find any out-of-band data.")))
  (undefined-value))

;;; ADD-OOB-HANDLER -- public
;;;
;;;   First, check to see if we already have any handlers for this file
;;; descriptor. If so, just add this handler to them. If not, add this
;;; file descriptor to *OOB-HANDLERS*, make sure our interrupt handler is
;;; installed, and that the given file descriptor is "owned" by us (so sigurg
;;; will be delivered.)

(defun add-oob-handler (fd char handler)
  "Arrange to funcall HANDLER when CHAR shows up out-of-band on FD."
  (declare (integer fd)
	   (base-char char))
  (let ((handlers (assoc fd *oob-handlers*)))
    (declare (list handlers))
    (cond (handlers
	   (push (cons char handler)
		 (cdr handlers)))
	  (t
	   (push (list fd
		       (cons char
			     handler))
		 *oob-handlers*)
	   (system:enable-interrupt unix:sigurg #'sigurg-handler)
	   #-hpux
	   (unix:unix-fcntl fd unix:f-setown (unix:unix-getpid))
	   #+hpux
	   (unix:siocspgrp fd (unix:unix-getpid)))))
  (values))

;;; REMOVE-OOB-HANDLER -- public
;;;
;;;   Delete any handlers for the given char from the list of handlers for the
;;; given file descriptor. If there are no more, nuke the entry for the file
;;; descriptor.

(defun remove-oob-handler (fd char)
  "Remove any handlers for CHAR on FD."
  (declare (integer fd)
	   (base-char char))
  (let ((handlers (assoc fd *oob-handlers*)))
    (declare (list handlers))
    (when handlers
      (let ((remaining (delete char (cdr handlers)
			       :test #'eql
			       :key #'car)))
	(declare (list remaining))
	(if remaining
	  (setf (cdr handlers) remaining)
	  (setf *oob-handlers*
		(delete fd *oob-handlers*
			:test #'eql
			:key #'car))))))
  (values))

;;; REMOVE-ALL-OOB-HANDLERS -- public
;;;
;;;   Delete the entry for the given file descriptor.

(defun remove-all-oob-handlers (fd)
  "Remove all handlers for FD."
  (declare (integer fd))
  (setf *oob-handlers*
	(delete fd *oob-handlers*
		:test #'eql
		:key #'car))
  (values))

;;; SEND-CHARACTER-OUT-OF-BAND -- public
;;;
;;;   Sends CHAR across FD out of band.

(defun send-character-out-of-band (fd char)
  (declare (integer fd)
	   (base-char char))
  (let ((buffer (make-string 1 :initial-element char)))
    (declare (simple-string buffer))
    (when (minusp (unix:unix-send fd buffer 1 msg-oob))
      (error "Error sending ~S OOB to across ~A: ~A"
	     char
	     fd
	     (unix:get-unix-error-msg)))))

(defun inet-recvfrom (fd buffer size &key (flags 0))
  "A packaging of the unix recvfrom call.  Returns three values:
bytecount, source address as integer, and source port.  bytecount
can of course be negative, to indicate faults."
  #+mp (mp:process-wait-until-fd-usable fd :input)
  (with-alien ((sockaddr inet-sockaddr))
    (let* ((bytecount (unix:unix-recvfrom fd buffer size flags
					  (alien-sap sockaddr)
					  (alien-size inet-sockaddr :bytes))))
      (values bytecount (ntohl (slot sockaddr 'addr)) (ntohs (slot sockaddr 'port))))))

(defun inet-sendto (fd buffer size addr port &key (flags 0))
  "A packaging of the unix sendto call.  Return value like sendto"
    (with-alien ((sockaddr inet-sockaddr))
      (setf (slot sockaddr 'family) af-inet)
      (setf (slot sockaddr 'port) (htons port))
      (setf (slot sockaddr 'addr) (htonl addr))
      (unix:unix-sendto fd
			 buffer
			 size
			 flags
			 (alien-sap sockaddr)
			 (alien-size inet-sockaddr :bytes))))

(defconstant shut-rd 0)
(defconstant shut-wr 1)
(defconstant shut-rdwr 2)

(defun inet-shutdown (fd level)
  (when (minusp (unix:unix-shutdown fd level))
    (error 'socket-error
	   :format-control "Error on shutdown of socket: ~A"
	   :format-arguments (list (unix:get-unix-error-msg))
	   :errno (unix:unix-errno))))

