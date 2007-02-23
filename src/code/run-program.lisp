;;; -*- Package: Extensions; Log: code.log  -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/run-program.lisp,v 1.26 2003/02/25 17:22:06 emarsden Exp $")
;;;
;;; **********************************************************************
;;;
;;; RUN-PROGRAM and friends.  Facility for running unix programs from inside
;;; a lisp.
;;; 
;;; Written by Jim Healy and Bill Chiles, November 1987, using an earlier
;;; version written by David McDonald.
;;;
;;; Completely re-written by William Lott, July 1989 - January 1990.
;;;

(in-package "EXTENSIONS")

(export '(run-program process-status process-exit-code process-core-dumped
	  process-wait process-kill process-input process-output process-plist
	  process-pty process-error process-status-hook process-alive-p
	  process-close process-pid process-p))


;;;; Import WAIT3 from unix.

(alien:def-alien-routine ("wait3" c-wait3) c-call:int
  (status c-call:int :out)
  (options c-call:int)
  (rusage c-call:int))

(eval-when (load eval compile)
  (defconstant wait-wnohang #-svr4 1 #+svr4 #o100)
  (defconstant wait-wuntraced #-svr4 2 #+svr4 4)
  (defconstant wait-wstopped #-svr4 #o177 #+svr4 wait-wuntraced))

(defun wait3 (&optional do-not-hang check-for-stopped)
  "Return any available status information on child processed. "
  (multiple-value-bind (pid status)
		       (c-wait3 (logior (if do-not-hang
					  wait-wnohang
					  0)
					(if check-for-stopped
					  wait-wuntraced
					  0))
				0)
    (cond ((or (minusp pid)
	       (zerop pid))
	   nil)
	  ((eql (ldb (byte 8 0) status)
		wait-wstopped)
	   (values pid
		   :stopped
		   (ldb (byte 8 8) status)))
	  ((zerop (ldb (byte 7 0) status))
	   (values pid
		   :exited
		   (ldb (byte 8 8) status)))
	  (t
	   (let ((signal (ldb (byte 7 0) status)))
	     (values pid
		     (if (or (eql signal unix:sigstop)
			     (eql signal unix:sigtstp)
			     (eql signal unix:sigttin)
			     (eql signal unix:sigttou))
		       :stopped
		       :signaled)
		     signal
		     (not (zerop (ldb (byte 1 7) status)))))))))



;;;; Process control stuff.

(defvar *active-processes* nil
  "List of process structures for all active processes.")

(defstruct (process (:print-function %print-process))
  pid			    ; PID of child process.
  %status		    ; Either :RUNNING, :STOPPED, :EXITED, or :SIGNALED.
  exit-code		    ; Either exit code or signal
  core-dumped		    ; T if a core image was dumped.
  pty			    ; Stream to child's pty or nil.
  input			    ; Stream to child's input or nil.
  output		    ; Stream from child's output or nil.
  error			    ; Stream from child's error output or nil.
  status-hook		    ; Closure to call when PROC changes status.
  plist			    ; Place for clients to stash tings.
  cookie		    ; List of the number of pipes from the subproc.
  )

(defun %print-process (proc stream depth)
  (declare (ignore depth))
  (format stream "#<process ~D ~S>"
	  (process-pid proc)
	  (process-status proc)))

;;; PROCESS-STATUS -- Public.
;;;
(defun process-status (proc)
  "Return the current status of process.  The result is one of :running,
   :stopped, :exited, :signaled."
  (declare (type process proc))
  (get-processes-status-changes)
  (process-%status proc))


;;; PROCESS-WAIT -- Public.
;;;
(defun process-wait (proc &optional check-for-stopped)
  "Wait for PROC to quit running for some reason.  Returns PROC."
  (declare (type process proc))
  (loop
    (case (process-status proc)
      (:running)
      (:stopped
       (when check-for-stopped
	 (return)))
      (t
       (when (zerop (car (process-cookie proc)))
	 (return))))
    (system:serve-all-events 1))
  proc)


#-hpux
;;; FIND-CURRENT-FOREGROUND-PROCESS -- internal
;;;
;;; Finds the current foreground process group id.
;;; 
(defun find-current-foreground-process (proc)
  (alien:with-alien ((result c-call:int))
    (multiple-value-bind
	(wonp error)
	(unix:unix-ioctl (system:fd-stream-fd (ext:process-pty proc))
			 unix:TIOCGPGRP
			 (alien:alien-sap (alien:addr result)))
      (unless wonp
	(error "TIOCPGRP ioctl failed: ~S"
	       (unix:get-unix-error-msg error)))
      result))
  (process-pid proc))


;;; PROCESS-KILL -- public
;;;
;;; Hand a process a signal.
;;;
(defun process-kill (proc signal &optional (whom :pid))
  "Hand SIGNAL to PROC.  If whom is :pid, use the kill Unix system call.  If
   whom is :process-group, use the killpg Unix system call.  If whom is
   :pty-process-group deliver the signal to whichever process group is currently
   in the foreground."
  (declare (type process proc))
  (let ((pid (ecase whom
	       ((:pid :process-group)
		(process-pid proc))
	       (:pty-process-group
		#-hpux
		(find-current-foreground-process proc)))))
    (multiple-value-bind
	(okay errno)
	(case whom
	  #+hpux
	  (:pty-process-group
	   (unix:unix-ioctl (system:fd-stream-fd (process-pty proc))
			    unix:TIOCSIGSEND
			    (system:int-sap
			     (unix:unix-signal-number signal))))
	  ((:process-group #-hpux :pty-process-group)
	   (unix:unix-killpg pid signal))
	  (t
	   (unix:unix-kill pid signal)))
      (cond ((not okay)
	     (values nil errno))
	    ((and (eql pid (process-pid proc))
		  (= (unix:unix-signal-number signal) unix:sigcont))
	     (setf (process-%status proc) :running)
	     (setf (process-exit-code proc) nil)
	     (when (process-status-hook proc)
	       (funcall (process-status-hook proc) proc))
	     t)
	    (t
	     t)))))


;;; PROCESS-ALIVE-P -- public
;;;
;;; Returns T if the process is still alive, NIL otherwise.
;;; 
(defun process-alive-p (proc)
  "Returns T if the process is still alive, NIL otherwise."
  (declare (type process proc))
  (let ((status (process-status proc)))
    (if (or (eq status :running)
	    (eq status :stopped))
      t
      nil)))

;;; PROCESS-CLOSE -- public
;;;
;;; Close all the streams held open by PROC.
;;; 
(defun process-close (proc)
  "Close all streams connected to PROC and stop maintaining the status slot."
  (declare (type process proc))
  (macrolet ((frob (stream abort)
	       `(when ,stream (close ,stream :abort ,abort))))
    (frob (process-pty    proc)   t) ; Don't FLUSH-OUTPUT to dead process.
    (frob (process-input  proc)   t) ; 'cause it will generate SIGPIPE.
    (frob (process-output proc) nil)
    (frob (process-error  proc) nil))
  (system:without-interrupts
   (setf *active-processes* (delete proc *active-processes*)))
  proc)

;;; SIGCHLD-HANDLER -- Internal.
;;;
;;; This is the handler for sigchld signals that RUN-PROGRAM establishes.
;;;
(defun sigchld-handler (ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  (get-processes-status-changes))

;;; GET-PROCESSES-STATUS-CHANGES -- Internal.
;;;
(defun get-processes-status-changes ()
  (loop
    (multiple-value-bind (pid what code core)
			 (wait3 t t)
      (unless pid
	(return))
      (let ((proc (find pid *active-processes* :key #'process-pid)))
	(when proc
	  (setf (process-%status proc) what)
	  (setf (process-exit-code proc) code)
	  (setf (process-core-dumped proc) core)
	  (when (process-status-hook proc)
	    (funcall (process-status-hook proc) proc))
	  (when (or (eq what :exited)
		    (eq what :signaled))
	    (system:without-interrupts
	      (setf *active-processes*
		    (delete proc *active-processes*)))))))))



;;;; RUN-PROGRAM and close friends.

(defvar *close-on-error* nil
  "List of file descriptors to close when RUN-PROGRAM exits due to an error.")
(defvar *close-in-parent* nil
  "List of file descriptors to close when RUN-PROGRAM returns in the parent.")
(defvar *handlers-installed* nil
  "List of handlers installed by RUN-PROGRAM.")


;;; FIND-A-PTY -- internal
;;;
;;;   Finds a pty that is not in use. Returns three values: the file descriptor
;;; for the master side of the pty, the file descriptor for the slave side of
;;; the pty, and the name of the tty device for the slave side.
;;; 
#-irix
(defun find-a-pty ()
  "Returns the master fd, the slave fd, and the name of the tty"
  (dolist (char '(#\p #\q))
    (dotimes (digit 16)
      (let* ((master-name (format nil "/dev/pty~C~X" char digit))
	     (master-fd (unix:unix-open master-name
					unix:o_rdwr
					#o666)))
	(when master-fd
	  (let* ((slave-name (format nil "/dev/tty~C~X" char digit))
		 (slave-fd (unix:unix-open slave-name
					   unix:o_rdwr
					   #o666)))
	    (when slave-fd
	      ; Maybe put a vhangup here?
              #-glibc2
	      (alien:with-alien ((stuff (alien:struct unix:sgttyb)))
		(let ((sap (alien:alien-sap stuff)))
		  (unix:unix-ioctl slave-fd unix:TIOCGETP sap)
		  (setf (alien:slot stuff 'unix:sg-flags) #o300) ; EVENP|ODDP
		  (unix:unix-ioctl slave-fd unix:TIOCSETP sap)
		  (unix:unix-ioctl master-fd unix:TIOCGETP sap)
		  (setf (alien:slot stuff 'unix:sg-flags)
			(logand (alien:slot stuff 'unix:sg-flags)
				(lognot 8))) ; ~ECHO
		  (unix:unix-ioctl master-fd unix:TIOCSETP sap)))
	      (return-from find-a-pty
			   (values master-fd
				   slave-fd
				   slave-name)))
	  (unix:unix-close master-fd))))))
  (error "Could not find a pty."))

#+irix
(alien:def-alien-routine ("_getpty" c-getpty) c-call:c-string
  (fildes c-call:int :out)
  (oflag c-call:int)
  (mode c-call:int)
  (nofork c-call:int))

#+irix
(defun find-a-pty ()
  "Returns the master fd, the slave fd, and the name of the tty"
  (multiple-value-bind (line master-fd)
    (c-getpty (logior unix:o_rdwr unix:o_ndelay) #o600 0)
    (let* ((slave-name line)
	   (slave-fd (unix:unix-open slave-name unix:o_rdwr #o666)))
      (when slave-fd
	; Maybe put a vhangup here?
        #-glibc2
	(alien:with-alien ((stuff (alien:struct unix:sgttyb)))
          (let ((sap (alien:alien-sap stuff)))
	    (unix:unix-ioctl slave-fd unix:TIOCGETP sap)
	    (setf (alien:slot stuff 'unix:sg-flags) #o300) ; EVENP|ODDP
	    (unix:unix-ioctl slave-fd unix:TIOCSETP sap)
	    (unix:unix-ioctl master-fd unix:TIOCGETP sap)
	    (setf (alien:slot stuff 'unix:sg-flags)
		  (logand (alien:slot stuff 'unix:sg-flags)
			  (lognot 8))) ; ~ECHO
	    (unix:unix-ioctl master-fd unix:TIOCSETP sap)))
	(return-from find-a-pty
		     (values master-fd
			     slave-fd
			     slave-name))))
    (unix:unix-close master-fd))
  (error "Could not find a pty."))

;;; OPEN-PTY -- internal
;;;
(defun open-pty (pty cookie)
  (when pty
    (multiple-value-bind
	(master slave name)
	(find-a-pty)
      (push master *close-on-error*)
      (push slave *close-in-parent*)
      (when (streamp pty)
	(multiple-value-bind (new-fd errno) (unix:unix-dup master)
	  (unless new-fd
	    (error "Could not UNIX:UNIX-DUP ~D: ~A"
		   master (unix:get-unix-error-msg errno)))
	  (push new-fd *close-on-error*)
	  (copy-descriptor-to-stream new-fd pty cookie)))
      (values name
	      (system:make-fd-stream master :input t :output t)))))


(defmacro round-bytes-to-words (n)
  `(logand (the fixnum (+ (the fixnum ,n) 3)) (lognot 3)))

(defun string-list-to-c-strvec (string-list)
  ;;
  ;; Make a pass over string-list to calculate the amount of memory
  ;; needed to hold the strvec.
  (let ((string-bytes 0)
	;; We need an extra for the null, and an extra 'cause exect clobbers
	;; argv[-1].
	(vec-bytes (* #-alpha 4 #+alpha 8 (+ (length string-list) 2))))
    (declare (fixnum string-bytes vec-bytes))
    (dolist (s string-list)
      (check-type s simple-string)
      (incf string-bytes (round-bytes-to-words (1+ (length s)))))
    ;;
    ;; Now allocate the memory and fill it in.
    (let* ((total-bytes (+ string-bytes vec-bytes))
	   (vec-sap (system:allocate-system-memory total-bytes))
	   (string-sap (sap+ vec-sap vec-bytes))
	   (i #-alpha 4 #+alpha 8))
      (declare (type (and unsigned-byte fixnum) total-bytes i)
	       (type system:system-area-pointer vec-sap string-sap))
      (dolist (s string-list)
	(declare (simple-string s))
	(let ((n (length s)))
	  ;; 
	  ;; Blast the string into place
	  (kernel:copy-to-system-area (the simple-string s)
				      (* vm:vector-data-offset vm:word-bits)
				      string-sap 0
				      (* (1+ n) vm:byte-bits))
	  ;; 
	  ;; Blast the pointer to the string into place
	  (setf (sap-ref-sap vec-sap i) string-sap)
	  (setf string-sap (sap+ string-sap (round-bytes-to-words (1+ n))))
	  (incf i #-alpha 4 #+alpha 8)))
      ;; Blast in last null pointer
      (setf (sap-ref-sap vec-sap i) (int-sap 0))
      (values vec-sap (sap+ vec-sap #-alpha 4 #+alpha 8) total-bytes))))


(defmacro with-c-strvec ((var str-list) &body body)
  (let ((sap (gensym "SAP-"))
	(size (gensym "SIZE-")))
    `(multiple-value-bind
	 (,sap ,var ,size)
	 (string-list-to-c-strvec ,str-list)
       (unwind-protect
	   (progn
	     ,@body)
	 (system:deallocate-system-memory ,sap ,size)))))

(alien:def-alien-routine spawn c-call:int
  (program c-call:c-string)
  (argv (* c-call:c-string))
  (envp (* c-call:c-string))
  (pty-name c-call:c-string)
  (stdin c-call:int)
  (stdout c-call:int)
  (stderr c-call:int))


;;; RUN-PROGRAM -- public
;;;
;;;   RUN-PROGRAM uses fork and execve to run a different program. Strange
;;; stuff happens to keep the unix state of the world coherent.
;;;
;;; The child process needs to get its input from somewhere, and send its
;;; output (both standard and error) to somewhere. We have to do different
;;; things depending on where these somewheres really are.
;;;
;;; For input, there are five options:
;;; - T: Just leave fd 0 alone. Pretty simple.
;;; - "file": Read from the file. We need to open the file and pull the
;;; descriptor out of the stream. The parent should close this stream after
;;; the child is up and running to free any storage used in the parent.
;;; - NIL: Same as "file", but use "/dev/null" as the file.
;;; - :STREAM: Use unix-pipe to create two descriptors. Use system:make-fd-stream
;;; to create the output stream on the writeable descriptor, and pass the
;;; readable descriptor to the child. The parent must close the readable
;;; descriptor for EOF to be passed up correctly.
;;; - a stream: If it's a fd-stream, just pull the descriptor out of it.
;;; Otherwise make a pipe as in :STREAM, and copy everything across.
;;;
;;; For output, there are n options:
;;; - T: Leave descriptor 1 alone.
;;; - "file": dump output to the file.
;;; - NIL: dump output to /dev/null.
;;; - :STREAM: return a stream that can be read from.
;;; - a stream: if it's a fd-stream, use the descriptor in it. Otherwise, copy
;;; stuff from output to stream.
;;;
;;; For error, there are all the same options as output plus:
;;; - :OUTPUT: redirect to the same place as output.
;;;
;;; RUN-PROGRAM returns a process struct for the process if the fork worked,
;;; and NIL if it did not.
;;;
(defun run-program (program args
		    &key (env *environment-list*) (wait t) pty input
		    if-input-does-not-exist output (if-output-exists :error)
		    (error :output) (if-error-exists :error) status-hook)
  "RUN-PROGRAM creates a new process and runs the unix program in the
   file specified by the simple-string PROGRAM.  ARGS are the standard
   arguments that can be passed to a Unix program, for no arguments
   use NIL (which means just the name of the program is passed as arg 0).

   RUN-PROGRAM will either return NIL or a PROCESS structure.  See the CMU
   Common Lisp Users Manual for details about the PROCESS structure.

   The keyword arguments have the following meanings:
     :env -
        An A-LIST mapping keyword environment variables to simple-string
	values.
     :wait -
        If non-NIL (default), wait until the created process finishes.  If
        NIL, continue running Lisp until the program finishes.
     :pty -
        Either T, NIL, or a stream.  Unless NIL, the subprocess is established
	under a PTY.  If :pty is a stream, all output to this pty is sent to
	this stream, otherwise the PROCESS-PTY slot is filled in with a stream
	connected to pty that can read output and write input.
     :input -
        Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
	input for the current process is inherited.  If NIL, /dev/null
	is used.  If a pathname, the file so specified is used.  If a stream,
	all the input is read from that stream and send to the subprocess.  If
	:STREAM, the PROCESS-INPUT slot is filled in with a stream that sends 
	its output to the process. Defaults to NIL.
     :if-input-does-not-exist (when :input is the name of a file) -
        can be one of:
           :error - generate an error.
           :create - create an empty file.
           nil (default) - return nil from run-program.
     :output -
        Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
	output for the current process is inherited.  If NIL, /dev/null
	is used.  If a pathname, the file so specified is used.  If a stream,
	all the output from the process is written to this stream. If
	:STREAM, the PROCESS-OUTPUT slot is filled in with a stream that can
	be read to get the output. Defaults to NIL.
     :if-output-exists (when :output is the name of a file) -
        can be one of:
           :error (default) - generates an error if the file already exists.
           :supersede - output from the program supersedes the file.
           :append - output from the program is appended to the file.
           nil - run-program returns nil without doing anything.
     :error and :if-error-exists - 
        Same as :output and :if-output-exists, except that :error can also be
	specified as :output in which case all error output is routed to the
	same place as normal output.
     :status-hook -
        This is a function the system calls whenever the status of the
        process changes.  The function takes the process as an argument."

  ;; Make sure the interrupt handler is installed.
  (system:enable-interrupt unix:sigchld #'sigchld-handler)
  ;; Make sure all the args are okay.
  (unless (every #'simple-string-p args)
    (error "All args to program must be simple strings -- ~S." args))
  ;; Pre-pend the program to the argument list.
  (push (namestring program) args)
  ;; Clear random specials used by GET-DESCRIPTOR-FOR to communicate cleanup
  ;; info.  Also, establish proc at this level so we can return it.
  (let (*close-on-error* *close-in-parent* *handlers-installed* proc)
    (unwind-protect
	(let ((pfile (unix-namestring (merge-pathnames program "path:") t t))
	      (cookie (list 0)))
	  (unless pfile
	    (error "No such program: ~S" program))
	  (multiple-value-bind
	      (stdin input-stream)
	      (get-descriptor-for input cookie :direction :input
				  :if-does-not-exist if-input-does-not-exist)
	    (multiple-value-bind
		(stdout output-stream)
		(get-descriptor-for output cookie :direction :output
                                    :if-does-not-exist :create
				    :if-exists if-output-exists)
	      (multiple-value-bind
		  (stderr error-stream)
		  (if (eq error :output)
		      (values stdout output-stream)
		      (get-descriptor-for error cookie :direction :output
                                          :if-does-not-exist :create
					  :if-exists if-error-exists))
		(multiple-value-bind (pty-name pty-stream)
				     (open-pty pty cookie)
		  ;; Make sure we are not notified about the child death before
		  ;; we have installed the process struct in *active-processes*
		  (system:without-interrupts
		    (with-c-strvec (argv args)
		      (with-c-strvec
			  (envp (mapcar #'(lambda (entry)
					    (concatenate
					     'string
					     (symbol-name (car entry))
					     "="
					     (cdr entry)))
					env))
			(let ((child-pid
			       (without-gcing
				(spawn pfile argv envp pty-name
				       stdin stdout stderr))))
			  (when (< child-pid 0)
			    (error "Could not fork child process: ~A"
				   (unix:get-unix-error-msg)))
			  (setf proc (make-process :pid child-pid
						   :%status :running
						   :pty pty-stream
						   :input input-stream
						   :output output-stream
						   :error error-stream
						   :status-hook status-hook
						   :cookie cookie))
			     (push proc *active-processes*))))))))))
      (dolist (fd *close-in-parent*)
	(unix:unix-close fd))
      (unless proc
	(dolist (fd *close-on-error*)
	  (unix:unix-close fd))
	(dolist (handler *handlers-installed*)
	  (system:remove-fd-handler handler))))
    (when (and wait proc)
      (process-wait proc))
    proc))

;;; COPY-DESCRIPTOR-TO-STREAM -- internal
;;;
;;;   Installs a handler for any input that shows up on the file descriptor.
;;; The handler reads the data and writes it to the stream.
;;; 
(defun copy-descriptor-to-stream (descriptor stream cookie)
  (incf (car cookie))
  (let ((string (make-string 256))
	handler)
    (setf handler
	  (system:add-fd-handler descriptor :input
	    #'(lambda (fd)
		(declare (ignore fd))
		(loop
		  (unless handler
		    (return))
		  (multiple-value-bind
		      (result readable/errno)
		      (unix:unix-select (1+ descriptor) (ash 1 descriptor)
					0 0 0)
		    (cond ((null result)
			   (error "Could not select on sub-process: ~A"
				  (unix:get-unix-error-msg readable/errno)))
			  ((zerop result)
			   (return))))
		  (alien:with-alien ((buf (alien:array c-call:char 256)))
		    (multiple-value-bind
			(count errno)
			(unix:unix-read descriptor (alien-sap buf) 256)
		      (cond ((or (and (null count)
				      (eql errno unix:eio))
				 (eql count 0))
			     (system:remove-fd-handler handler)
			     (setf handler nil)
			     (decf (car cookie))
			     (unix:unix-close descriptor)
			     (return))
			    ((null count)
			     (system:remove-fd-handler handler)
			     (setf handler nil)
			     (decf (car cookie))
			     (error "Could not read input from sub-process: ~A"
				    (unix:get-unix-error-msg errno)))
			    (t
			     (kernel:copy-from-system-area
			      (alien-sap buf) 0
			      string (* vm:vector-data-offset vm:word-bits)
			      (* count vm:byte-bits))
			     (write-string string stream
					   :end count)))))))))))

;;; GET-DESCRIPTOR-FOR -- internal
;;;
;;;   Find a file descriptor to use for object given the direction. Returns
;;; the descriptor. If object is :STREAM, returns the created stream as the
;;; second value.
;;; 
(defun get-descriptor-for (object cookie &rest keys &key direction
				  &allow-other-keys)
  (cond ((eq object t)
	 ;; No new descriptor is needed.
	 (values -1 nil))
	((eq object nil)
	 ;; Use /dev/null.
	 (multiple-value-bind
	     (fd errno)
	     (unix:unix-open "/dev/null"
			     (case direction
			       (:input unix:o_rdonly)
			       (:output unix:o_wronly)
			       (t unix:o_rdwr))
			     #o666)
	   (unless fd
	     (error "Could not open \"/dev/null\": ~A"
		    (unix:get-unix-error-msg errno)))
	   (push fd *close-in-parent*)
	   (values fd nil)))
	((eq object :stream)
	 (multiple-value-bind
	     (read-fd write-fd)
	     (unix:unix-pipe)
	   (unless read-fd
	     (error "Could not create pipe: ~A"
		    (unix:get-unix-error-msg write-fd)))
	   (case direction
	     (:input
	      (push read-fd *close-in-parent*)
	      (push write-fd *close-on-error*)
	      (let ((stream (system:make-fd-stream write-fd :output t)))
		(values read-fd stream)))
	     (:output
	      (push read-fd *close-on-error*)
	      (push write-fd *close-in-parent*)
	      (let ((stream (system:make-fd-stream read-fd :input t)))
		(values write-fd stream)))
	     (t
	      (unix:unix-close read-fd)
	      (unix:unix-close write-fd)
	      (error "Direction must be either :INPUT or :OUTPUT, not ~S"
		     direction)))))
	((or (pathnamep object) (stringp object))
	 (with-open-stream (file (apply #'open object keys))
	   (multiple-value-bind
	       (fd errno)
	       (unix:unix-dup (system:fd-stream-fd file))
	     (cond (fd
		    (push fd *close-in-parent*)
		    (values fd nil))
		   (t
		    (error "Could not duplicate file descriptor: ~A"
			   (unix:get-unix-error-msg errno)))))))
	((system:fd-stream-p object)
	 (values (system:fd-stream-fd object) nil))
	((streamp object)
	 (ecase direction
	   (:input
	    (dotimes (count
		      256
		      (error "Could not open a temporary file in /tmp"))
	      (let* ((name (format nil "/tmp/.run-program-~D" count))
		     (fd (unix:unix-open name
					 (logior unix:o_rdwr
						 unix:o_creat
						 unix:o_excl)
					 #o666)))
		(unix:unix-unlink name)
		(when fd
		  (let ((newline (string #\Newline)))
		    (loop
		      (multiple-value-bind
			  (line no-cr)
			  (read-line object nil nil)
			(unless line
			  (return))
			(unix:unix-write fd line 0 (length line))
			(if no-cr
			  (return)
			  (unix:unix-write fd newline 0 1)))))
		  (unix:unix-lseek fd 0 unix:l_set)
		  (push fd *close-in-parent*)
		  (return (values fd nil))))))
	   (:output
	    (multiple-value-bind (read-fd write-fd)
				 (unix:unix-pipe)
	      (unless read-fd
		(error "Cound not create pipe: ~A"
		       (unix:get-unix-error-msg write-fd)))
	      (copy-descriptor-to-stream read-fd object cookie)
	      (push read-fd *close-on-error*)
	      (push write-fd *close-in-parent*)
	      (values write-fd nil)))))
	(t
	 (error "Invalid option to run-program: ~S" object))))

