;;;; -*- Mode: Lisp ; Package: Toolkit -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/interface-glue.lisp,v 1.5 1998/01/07 12:18:58 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; Functions which provide the glue for the interface between the C server
;;; and the Lisp client.

(in-package "TOOLKIT")



;;;; Functions for handling server requests

(defvar reply-table
  (vector :confirm :values :callback :event :error :warning :protocol :action))

(defun wait-for-server-reply (fd)
  (wait-for-input fd)
  (loop
    (let ((reply (dispatch-server-reply fd)))
      (when reply (return reply)))))

;;; From Timothy Miller, Jan-98 -- 
;;; wait-for-server-reply is only ever called (currently) when waiting for a
;;;:confirm reply, and is currently the only place that cares about the return
;;;value from this function. Hence, we make the return value for everything else
;;;nil to avoid mysterious timing problems with actions or whatever getting
;;;called while lisp is trying to execute requests.

(defun dispatch-server-reply (fd)
  (let* ((reply (receive-message fd))
	 (kind (svref reply-table (xti::message-get-dblword reply))))
    (case kind
      ((:confirm :values) reply)
      (:callback (handle-callback reply) nil)
      (:protocol (handle-protocol reply) nil)
      (:action   (handle-action reply)   nil)
      (:event    (handle-event reply)    nil)
      (:error
       (let ((errmsg (toolkit-read-value reply)))
	 (destroy-message reply)
	 (toolkit-error "Toolkit Server -- ~a" errmsg)))
      (:warning
       (let ((errmsg (toolkit-read-value reply)))
	 (destroy-message reply)
	 (warn "Toolkit Server -- ~a" errmsg)
	 ;; Point out that this was not really a reply
	 nil))
      (t 
       (destroy-message reply)
       (toolkit-error "Invalid reply type: ~d" kind)))))


(defun send-request-to-server (message options)
  (let ((fd (motif-connection-fd *motif-connection*)))
    (unwind-protect
	(transmit-message message fd)
      (destroy-message message))
    (when (eq options :confirm) (wait-for-server-reply fd))))



;;;; Functions for handling server connections

(defvar *default-server-host* nil
  "Name of machine where the Motif server resides.  Using the value NIL
   causes a local connection to be made.")

(defvar *default-display* nil
  "If non-nil, the display which will be opened.  Otherwise, the DISPLAY
   environment variable is consulted.")

(defvar *debug-mode* nil
  "Controls whether the client is in debugging mode.")

(defvar *active-handlers* nil
  "An alist of (fd . handler) for active X toolkit connections.")

(defvar *default-timeout-interval* 15
  "Default time, in seconds, which the Lisp process will wait for input on
   the toolkit connection before assuming a timeout has occured.")

(defvar *clm-binary-directory* "library:"
  "Directory in which the Motif server resides.")

(defvar *clm-binary-name* "motifd"
  "Name of the Motif server executable.")

(defun add-toolkit-handler (conn)
  (let ((fd (motif-connection-fd conn)))
    (when (assoc fd *active-handlers*)
      (break "There is already a handler for fd=~d " fd))
    (push (cons fd
		(system:add-fd-handler
		 fd :input
		 #'(lambda (this-fd)
		     (declare (ignore this-fd))
		     (toolkit-handler conn))))
	  *active-handlers*)))

(defun remove-toolkit-handler (fd)
  (let ((handler (cdr (assoc fd *active-handlers*))))
    (unless handler
      (toolkit-error "Cannot remove handler (fd=~d) because there is none." fd))
    (system:remove-fd-handler handler)
    (setf *active-handlers* (remove fd *active-handlers*
				    :key #'car :test #'=))))

(defvar *local-motif-server* nil)

(defun local-server-status-hook (process)
  (let ((status (ext:process-status process)))
    (when (or (eq status :exited)
	      (eq status :signaled))
      (setf *local-motif-server* nil))))

(defun verify-local-server-exists ()
  (unless (probe-file (merge-pathnames *clm-binary-name*
				       *clm-binary-directory*))
    (toolkit-error "Unable to locate the Motif server."))
  (when (or (not *local-motif-server*)
	    (and *local-motif-server*
		 (not (ext:process-alive-p *local-motif-server*))))
    (let ((process (ext:run-program (merge-pathnames *clm-binary-name*
						     *clm-binary-directory*)
				    '()
				    :wait nil
				    :status-hook #'local-server-status-hook)))
      (unless (and process (ext:process-alive-p process))
	(toolkit-error "Could not start local Motif server process."))
      ;;
      ;; Wait until the server has started up
      (loop
	(when (probe-file (format nil "/tmp/.motif_socket-u~a"
				  (unix:unix-getuid)))
	  (return))
	(sleep 1))
      (setf *local-motif-server* process))))

(defun open-motif-connection (host dpy-name app-name app-class &optional pid)
  (declare (simple-string app-name app-class))
  (unless (or host pid)
    (verify-local-server-exists))
  (let* ((socket (connect-to-host host pid))
	 (tmp (system:allocate-system-memory 4))
	 (conn (make-motif-connection socket))
	 (display (or dpy-name (cdr (assoc :display *environment-list*))))
	 (clx-dpy (open-clx-display display))
	 (greeting (create-message 0)))

    (unless display
      (toolkit-error "No display name available."))

    ;;
    ;; Fairly gross means of sending the swap information to the server
    (setf (system:sap-ref-16 tmp 0) 1)
    (unwind-protect
	(unix:unix-write socket tmp 0 2)
      (system:deallocate-system-memory tmp 4))

    (toolkit-write-value greeting display)
    (toolkit-write-value greeting app-name)
    (toolkit-write-value greeting app-class)
    (transmit-message greeting socket)
    (destroy-message greeting)

    (setf (motif-connection-display-name conn) display)
    (setf (motif-connection-display conn) clx-dpy)
    (add-toolkit-handler conn)
    conn))

(defun close-motif-connection (connection)
  (unless (motif-connection-terminated connection)
    (let ((fd (motif-connection-fd connection))
	  (hook (motif-connection-close-hook connection)))
      (when hook
	(funcall hook connection))
      (setf (motif-connection-terminated connection) t)
      (remove-toolkit-handler fd)
      (close-socket fd)
      (xlib:close-display (motif-connection-display connection)))))

(defmacro with-motif-connection ((connection) &body forms)
  `(let ((*motif-connection* ,connection)
	 (*x-display* (motif-connection-display ,connection)))
     (handler-case
	 (restart-case
	     (progn ,@forms)
	   (continue ()
	             :report "Ignore problem and continue."
	     ())
	   (kill-app ()
 	             :report "Close current application."
	     (quit-server)     
	     (close-motif-connection ,connection)))
       (toolkit-eof-error (cond)
	 (format t "~%Connection to server broken: ~a" cond)
         (close-motif-connection ,connection)
	 (signal cond)))))

(defmacro with-clx-requests (&body forms)
  `(unwind-protect
       (progn ,@forms)
     (xlib:display-force-output *x-display*)))

;;; This is the functions which listens for input from the server and calls
;;; the dispatcher when it detects incoming data.
(defun toolkit-handler (connection)
  (unless (motif-connection-terminated connection)
    (with-motif-connection (connection)
      (let ((fd (motif-connection-fd connection)))
	(cond
	 ((wait-for-input-or-timeout fd *default-timeout-interval*)
	  (dispatch-server-reply fd))
	 (t
	  (warn "Got timeout on fd=~d" fd)))
	(xlib:display-force-output *x-display*)))))
