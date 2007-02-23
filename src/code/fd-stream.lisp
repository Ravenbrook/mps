;;; -*- Log: code.log; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/fd-stream.lisp,v 1.84 2006/02/27 16:06:34 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Streams for UNIX file descriptors.
;;;
;;; Written by William Lott, July 1989 - January 1990.
;;; Some tuning by Rob MacLachlan.
;;; 
;;; **********************************************************************


(in-package "SYSTEM")

(export '(fd-stream fd-stream-p fd-stream-fd make-fd-stream
          io-timeout beep *beep-function* output-raw-bytes
	  *tty* *stdin* *stdout* *stderr*
	  binary-text-stream))


(in-package "EXTENSIONS")

(export '(*backup-extension*))


(in-package "LISP")

(export '(file-stream file-string-length))


;;;; Buffer manipulation routines.

(defvar *available-buffers* ()
  "List of available buffers.  Each buffer is an sap pointing to
  bytes-per-buffer of memory.")

(defconstant bytes-per-buffer (* 4 1024)
  "Number of bytes per buffer.")

;; This limit is rather arbitrary
(defconstant max-stream-element-size 1024
  "The maximum supported byte size for a stream element-type.")

;;; NEXT-AVAILABLE-BUFFER -- Internal.
;;;
;;; Returns the next available buffer, creating one if necessary.
;;;
(declaim (inline next-available-buffer))
;;;
(defun next-available-buffer ()
  (if *available-buffers*
      (pop *available-buffers*)
      (allocate-system-memory bytes-per-buffer)))


;;;; The FD-STREAM structure.

;;;; Superclass defined by the ANSI Spec
(defstruct (file-stream
	     (:include lisp-stream)
	     (:constructor nil)
	     (:copier nil)))

(defstruct (fd-stream
	    (:print-function %print-fd-stream)
	    (:constructor %make-fd-stream)
	    (:include file-stream
		      (misc #'fd-stream-misc-routine)))

  (name nil)		      ; The name of this stream
  (file nil)		      ; The file this stream is for
  ;;
  ;; The backup file namestring for the old file, for :if-exists :rename or
  ;; :rename-and-delete.
  (original nil :type (or simple-string null))
  (delete-original nil)	      ; for :if-exists :rename-and-delete
  ;;
  ;;; Number of bytes per element.
  (element-size 1 :type index)
  (element-type 'base-char)   ; The type of element being transfered.
  (fd -1 :type fixnum)	      ; The file descriptor
  ;;
  ;; Controls when the output buffer is flushed.
  (buffering :full :type (member :full :line :none))
  ;;
  ;; Character position if known.
  (char-pos nil :type (or index null))
  ;;
  ;; T if input is waiting on FD.  :EOF if we hit EOF.
  (listen nil :type (member nil t :eof))
  ;;
  ;; The input buffer.
  (unread nil)
  (ibuf-sap nil :type (or system-area-pointer null))
  (ibuf-length nil :type (or index null))
  (ibuf-head 0 :type index)
  (ibuf-tail 0 :type index)

  ;; The output buffer.
  (obuf-sap nil :type (or system-area-pointer null))
  (obuf-length nil :type (or index null))
  (obuf-tail 0 :type index)

  ;; Output flushed, but not written due to non-blocking io.
  (output-later nil)
  (handler nil)
  ;;
  ;; Timeout specified for this stream, or NIL if none.
  (timeout nil :type (or index null))
  ;;
  ;; Pathname of the file this stream is opened to (returned by PATHNAME.)
  (pathname nil :type (or pathname null)))

(defun %print-fd-stream (fd-stream stream depth)
  (declare (ignore depth) (stream stream))
  (format stream "#<Stream for ~A>"
	  (fd-stream-name fd-stream)))

;; CMUCL extension.  This is a FD-STREAM, but it allows reading and
;; writing of 8-bit characters and unsigned bytes from the stream.
(defstruct (binary-text-stream
	     (:print-function %print-binary-text-stream)
	     (:constructor %make-binary-text-stream)
	     (:include fd-stream)))

(defun %print-binary-text-stream (fd-stream stream depth)
  (declare (ignore depth) (stream stream))
  (format stream "#<Binary-text Stream for ~A>"
	  (fd-stream-name fd-stream)))

(define-condition io-timeout (stream-error)
  ((direction :reader io-timeout-direction :initarg :direction))
  (:report
   (lambda (condition stream)
     (declare (stream stream))
     (format stream "Timeout ~(~A~)ing ~S."
	     (io-timeout-direction condition)
	     (stream-error-stream condition)))))


;;;; Output routines and related noise.

(defvar *output-routines* ()
  "List of all available output routines. Each element is a list of the
  element-type output, the kind of buffering, the function name, and the number
  of bytes per element.")

;;; DO-OUTPUT-LATER -- internal
;;;
;;;   Called by the server when we can write to the given file descriptor.
;;; Attempt to write the data again. If it worked, remove the data from the
;;; output-later list. If it didn't work, something is wrong.
;;;
(defun do-output-later (stream)
  (let* ((stuff (pop (fd-stream-output-later stream)))
	 (base (car stuff))
	 (start (cadr stuff))
	 (end (caddr stuff))
	 (reuse-sap (cadddr stuff))
	 (length (- end start)))
    (declare (type index start end length))
    (multiple-value-bind
	(count errno)
	(unix:unix-write (fd-stream-fd stream)
			 base
			 start
			 length)
      (cond ((not count)
	     (if (= errno unix:ewouldblock)
		 (error "Write would have blocked, but SERVER told us to go.")
		 (error "While writing ~S: ~A"
			stream (unix:get-unix-error-msg errno))))
	    ((eql count length) ; Hot damn, it worked.
	     (when reuse-sap
	       (push base *available-buffers*)))
	    ((not (null count)) ; Sorta worked.
	     (push (list base
			 (the index (+ start count))
			 end)
		   (fd-stream-output-later stream))))))
  (unless (fd-stream-output-later stream)
    (system:remove-fd-handler (fd-stream-handler stream))
    (setf (fd-stream-handler stream) nil)))

;;; OUTPUT-LATER -- internal
;;;
;;;   Arrange to output the string when we can write on the file descriptor.
;;;
(defun output-later (stream base start end reuse-sap)
  (cond ((null (fd-stream-output-later stream))
	 (setf (fd-stream-output-later stream)
	       (list (list base start end reuse-sap)))
	 (setf (fd-stream-handler stream)
	       (system:add-fd-handler (fd-stream-fd stream)
				      :output
				      #'(lambda (fd)
					  (declare (ignore fd))
					  (do-output-later stream)))))
	(t
	 (nconc (fd-stream-output-later stream)
		(list (list base start end reuse-sap)))))
  (when reuse-sap
    (let ((new-buffer (next-available-buffer)))
      (setf (fd-stream-obuf-sap stream) new-buffer)
      (setf (fd-stream-obuf-length stream) bytes-per-buffer)))) 

;;; DO-OUTPUT -- internal
;;;
;;;   Output the given noise. Check to see if there are any pending writes. If
;;; so, just queue this one. Otherwise, try to write it. If this would block,
;;; queue it.
;;;
(defun do-output (stream base start end reuse-sap)
  (declare (type fd-stream stream)
	   (type (or system-area-pointer (simple-array * (*))) base)
	   (type index start end))
  (if (not (null (fd-stream-output-later stream))) ; something buffered.
      (progn
	(output-later stream base start end reuse-sap)
	;; ### check to see if any of this noise can be output
	)
      (let ((length (- end start)))
	(multiple-value-bind
	      (count errno)
	    (unix:unix-write (fd-stream-fd stream) base start length)
	  (cond ((not count)
		 (if (= errno unix:ewouldblock)
		     (output-later stream base start end reuse-sap)
		     (error 'simple-stream-error
                            :stream stream
                            :format-control "while writing: ~A"
			    :format-arguments (list (unix:get-unix-error-msg errno)))))
		((not (eql count length))
		 (output-later stream base (the index (+ start count))
			       end reuse-sap)))))))


;;; FLUSH-OUTPUT-BUFFER -- internal
;;;
;;;   Flush any data in the output buffer.
;;;
(defun flush-output-buffer (stream)
  (let ((length (fd-stream-obuf-tail stream)))
    (unless (= length 0)
      (do-output stream (fd-stream-obuf-sap stream) 0 length t)
      (setf (fd-stream-obuf-tail stream) 0))))

;;; DEF-OUTPUT-ROUTINES -- internal
;;;
;;;   Define output routines that output numbers size bytes long for the
;;; given bufferings. Use body to do the actual output.
;;;
(defmacro def-output-routines ((name size &rest bufferings) &body body)
  (declare (optimize (speed 1)))
  (cons 'progn
	(mapcar
	    #'(lambda (buffering)
		(let ((function
		       (intern (let ((*print-case* :upcase))
				 (format nil name (car buffering))))))
		  `(progn
		     (defun ,function (stream byte)
		       ,(unless (eq (car buffering) :none)
			  `(when (< (fd-stream-obuf-length stream)
				    (+ (fd-stream-obuf-tail stream)
				       ,size))
			     (flush-output-buffer stream)))
		       ;;
		       ;; If there is any input read from UNIX but not
		       ;; supplied to the user of the stream, reposition
		       ;; to the real file position as seen from Lisp.
		       ,(unless (eq (car buffering) :none)
			  `(when (> (fd-stream-ibuf-tail stream)
				    (fd-stream-ibuf-head stream))
			     (file-position stream (file-position stream))))
		       ,@body
		       (incf (fd-stream-obuf-tail stream) ,size)
		       ,(ecase (car buffering)
			  (:none
			   `(flush-output-buffer stream))
			  (:line
			   `(when (eq (char-code byte) (char-code #\Newline))
			      (flush-output-buffer stream)))
			  (:full
			   ))
		       (values))
		     (setf *output-routines*
			   (nconc *output-routines*
				  ',(mapcar
					#'(lambda (type)
					    (list type
						  (car buffering)
						  function
						  size))
				      (cdr buffering)))))))
	  bufferings)))

(def-output-routines ("OUTPUT-CHAR-~A-BUFFERED"
		      1
		      (:none character)
		      (:line character)
		      (:full character))
  (if (char= byte #\Newline)
      (setf (fd-stream-char-pos stream) 0)
      (incf (fd-stream-char-pos stream)))
  (setf (sap-ref-8 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	(char-code byte)))

(def-output-routines ("OUTPUT-UNSIGNED-BYTE-~A-BUFFERED"
		      1
		      (:none (unsigned-byte 8))
		      (:full (unsigned-byte 8)))
  (setf (sap-ref-8 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-SIGNED-BYTE-~A-BUFFERED"
		      1
		      (:none (signed-byte 8))
		      (:full (signed-byte 8)))
  (setf (signed-sap-ref-8 (fd-stream-obuf-sap stream)
			  (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-UNSIGNED-SHORT-~A-BUFFERED"
		      2
		      (:none (unsigned-byte 16))
		      (:full (unsigned-byte 16)))
  (setf (sap-ref-16 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-SIGNED-SHORT-~A-BUFFERED"
		      2
		      (:none (signed-byte 16))
		      (:full (signed-byte 16)))
  (setf (signed-sap-ref-16 (fd-stream-obuf-sap stream)
			   (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-UNSIGNED-LONG-~A-BUFFERED"
		      4
		      (:none (unsigned-byte 32))
		      (:full (unsigned-byte 32)))
  (setf (sap-ref-32 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-SIGNED-LONG-~A-BUFFERED"
		      4
		      (:none (signed-byte 32))
		      (:full (signed-byte 32)))
  (setf (signed-sap-ref-32 (fd-stream-obuf-sap stream)
			   (fd-stream-obuf-tail stream))
	byte))


;;; OUTPUT-RAW-BYTES -- public
;;;
;;;   Does the actual output. If there is space to buffer the string, buffer
;;; it. If the string would normally fit in the buffer, but doesn't because
;;; of other stuff in the buffer, flush the old noise out of the buffer and
;;; put the string in it. Otherwise we have a very long string, so just
;;; send it directly (after flushing the buffer, of course).
;;;
(defun output-raw-bytes (stream thing &optional start end)
  "Output THING to stream.  THING can be any kind of vector or a sap.  If THING
  is a SAP, END must be supplied (as length won't work)."
  (let ((start (or start 0))
	(end (or end (length (the (simple-array * (*)) thing)))))
    (declare (type index start end))
    ;;
    ;; If there is any input read from UNIX but not
    ;; supplied to the user of the stream, reposition
    ;; to the real file position as seen from Lisp.
    (when (> (fd-stream-ibuf-tail stream)
	     (fd-stream-ibuf-head stream))
      (file-position stream (file-position stream)))
    (let* ((len (fd-stream-obuf-length stream))
	   (tail (fd-stream-obuf-tail stream))
	   (space (- len tail))
	   (bytes (- end start))
	   (newtail (+ tail bytes)))
      (cond ((minusp bytes) ; Error case
	     (cerror "Just go on as if nothing happened..."
		     "~S called with :END before :START!"
		     'output-raw-bytes))
	    ((zerop bytes)) ; Easy case
	    ((<= bytes space)
	     (if (system-area-pointer-p thing)
		 (system-area-copy thing
				   (* start vm:byte-bits)
				   (fd-stream-obuf-sap stream)
				   (* tail vm:byte-bits)
				   (* bytes vm:byte-bits))
		 (copy-to-system-area thing
				      (+ (* start vm:byte-bits)
					 (* vm:vector-data-offset vm:word-bits))
				      (fd-stream-obuf-sap stream)
				      (* tail vm:byte-bits)
				      (* bytes vm:byte-bits)))
	     (setf (fd-stream-obuf-tail stream) newtail))
	    ((<= bytes len)
	     (flush-output-buffer stream)
	     (if (system-area-pointer-p thing)
		 (system-area-copy thing
				   (* start vm:byte-bits)
				   (fd-stream-obuf-sap stream)
				   0
				   (* bytes vm:byte-bits))
		 (copy-to-system-area thing
				      (+ (* start vm:byte-bits)
					 (* vm:vector-data-offset vm:word-bits))
				      (fd-stream-obuf-sap stream)
				      0
				      (* bytes vm:byte-bits)))
	     (setf (fd-stream-obuf-tail stream) bytes))
	    (t
	     (flush-output-buffer stream)
	     (do-output stream thing start end nil))))))

;;; FD-SOUT -- internal
;;;
;;;   Routine to use to output a string. If the stream is unbuffered, slam
;;; the string down the file descriptor, otherwise use OUTPUT-RAW-BYTES to
;;; buffer the string. Update charpos by checking to see where the last newline
;;; was.
;;;
;;;   Note: some bozos (the FASL dumper) call write-string with things other
;;; than strings. Therefore, we must make sure we have a string before calling
;;; position on it.
;;; 
(defun fd-sout (stream thing start end)
  (let ((start (or start 0))
	(end (or end (length (the vector thing)))))
    (declare (type index start end))
    (if (stringp thing)
	(let ((last-newline (and (find #\newline (the simple-string thing)
				       :start start :end end)
				 (position #\newline (the simple-string thing)
					   :from-end t
					   :start start
					   :end end))))
	  (ecase (fd-stream-buffering stream)
	    (:full
	     (output-raw-bytes stream thing start end))
	    (:line
	     (output-raw-bytes stream thing start end)
	     (when last-newline
	       (flush-output-buffer stream)))
	    (:none
	     (do-output stream thing start end nil)))
	  (if last-newline
	      (setf (fd-stream-char-pos stream)
		    (- end last-newline 1))
	      (incf (fd-stream-char-pos stream)
		    (- end start))))
	(ecase (fd-stream-buffering stream)
	  ((:line :full)
	   (output-raw-bytes stream thing start end))
	  (:none
	   (do-output stream thing start end nil))))))

(defmacro output-wrapper ((stream size buffering) &body body)
  (let ((stream-var (gensym)))
    `(let ((,stream-var ,stream))
      ,(unless (eq (car buffering) :none)
	 `(when (< (fd-stream-obuf-length ,stream-var)
	           (+ (fd-stream-obuf-tail ,stream-var)
		       ,size))
            (flush-output-buffer ,stream-var)))
      ,(unless (eq (car buffering) :none)
	 `(when (> (fd-stream-ibuf-tail ,stream-var)
		   (fd-stream-ibuf-head ,stream-var))
            (file-position ,stream-var (file-position ,stream-var))))
    
      ,@body
      (incf (fd-stream-obuf-tail ,stream-var) ,size)
      ,(ecase (car buffering)
	 (:none
	  `(flush-output-buffer ,stream-var))
	 (:line
	  `(when (eq (char-code byte) (char-code #\Newline))
	     (flush-output-buffer ,stream-var)))
	 (:full))
      (values))))

;;; PICK-OUTPUT-ROUTINE -- internal
;;;
;;;   Find an output routine to use given the type and buffering. Return as
;;; multiple values the routine, the real type transfered, and the number of
;;; bytes per element.
;;;
(defun pick-output-routine (type buffering)
  (dolist (entry *output-routines*)
    (when (and (subtypep type (car entry))
	       (eq buffering (cadr entry)))
      (return-from pick-output-routine
	(values (symbol-function (caddr entry))
		(car entry)
		(cadddr entry)))))
  ;; KLUDGE: also see comments in PICK-INPUT-ROUTINE
  (loop for i from 40 by 8 to max-stream-element-size ; ARB (KLUDGE)
	if (subtypep type `(unsigned-byte ,i))
	do (return-from pick-output-routine
	     (values
	      (ecase buffering
		(:none
		 (lambda (stream byte)
		   (output-wrapper (stream (/ i 8) (:none))
		     (loop for j from 0 below (/ i 8)
			   do (setf (sap-ref-8 
				     (fd-stream-obuf-sap stream)
				     (+ j (fd-stream-obuf-tail stream)))
				    (ldb (byte 8 (- i 8 (* j 8))) byte))))))
		(:full
		 (lambda (stream byte)
		   (output-wrapper (stream (/ i 8) (:full))
		     (loop for j from 0 below (/ i 8)
			   do (setf (sap-ref-8 
				     (fd-stream-obuf-sap stream)
				     (+ j (fd-stream-obuf-tail stream)))
				    (ldb (byte 8 (- i 8 (* j 8))) byte)))))))
	      `(unsigned-byte ,i)
	      (/ i 8))))
  (loop for i from 40 by 8 to max-stream-element-size ; ARB (KLUDGE)
	if (subtypep type `(signed-byte ,i))
	do (return-from pick-output-routine
	     (values
	      (ecase buffering
		(:none
		 (lambda (stream byte)
		   (output-wrapper (stream (/ i 8) (:none))
		     (loop for j from 0 below (/ i 8)
			   do (setf (sap-ref-8 
				     (fd-stream-obuf-sap stream)
				     (+ j (fd-stream-obuf-tail stream)))
				    (ldb (byte 8 (- i 8 (* j 8))) byte))))))
		(:full
		 (lambda (stream byte)
		   (output-wrapper (stream (/ i 8) (:full))
		     (loop for j from 0 below (/ i 8)
			   do (setf (sap-ref-8 
				     (fd-stream-obuf-sap stream)
				     (+ j (fd-stream-obuf-tail stream)))
				    (ldb (byte 8 (- i 8 (* j 8))) byte)))))))
	      `(signed-byte ,i)
	      (/ i 8)))))

;;;; Input routines and related noise.

(defvar *input-routines* ()
  "List of all available input routines. Each element is a list of the
  element-type input, the function name, and the number of bytes per element.")

;;; DO-INPUT -- internal
;;;
;;;   Fills the input buffer, and returns the first character. Throws to
;;; eof-input-catcher if the eof was reached. Drops into system:server if
;;; necessary.
;;;
(defun do-input (stream)
  (let ((fd (fd-stream-fd stream))
	(ibuf-sap (fd-stream-ibuf-sap stream))
	(buflen (fd-stream-ibuf-length stream))
	(head (fd-stream-ibuf-head stream))
	(tail (fd-stream-ibuf-tail stream)))
    (declare (type index head tail))
    (unless (zerop head)
      (cond ((eql head tail)
	     (setf head 0)
	     (setf tail 0)
	     (setf (fd-stream-ibuf-head stream) 0)
	     (setf (fd-stream-ibuf-tail stream) 0))
	    (t
	     (decf tail head)
	     (system-area-copy ibuf-sap (* head vm:byte-bits)
			       ibuf-sap 0 (* tail vm:byte-bits))
	     (setf head 0)
	     (setf (fd-stream-ibuf-head stream) 0)
	     (setf (fd-stream-ibuf-tail stream) tail))))
    (setf (fd-stream-listen stream) nil)
    (multiple-value-bind
	  (count errno)
	(alien:with-alien ((read-fds (alien:struct unix:fd-set)))
	  (unix:fd-zero read-fds)
	  (unix:fd-set fd read-fds)
	  (unix:unix-fast-select (1+ fd) (alien:addr read-fds) nil nil 0 0))
      ;; Wait if input is not available or if interrupted.
      (when (or (eql count 0)
		(and (not count) (eql errno unix:eintr)))
	(unless #-mp (system:wait-until-fd-usable
		      fd :input (fd-stream-timeout stream))
		#+mp (mp:process-wait-until-fd-usable
		      fd :input (fd-stream-timeout stream))
	  (error 'io-timeout :stream stream :direction :read))))
    (multiple-value-bind
	  (count errno)
	(unix:unix-read fd
			(system:int-sap (+ (system:sap-int ibuf-sap) tail))
			(- buflen tail))
      (cond ((null count)
	     ;; What kinds of errors do we want to look at and what do
	     ;; we want them to do?
	     (cond ((eql errno unix:ewouldblock)
		    (unless #-mp (system:wait-until-fd-usable
				  fd :input (fd-stream-timeout stream))
			    #+mp (mp:process-wait-until-fd-usable
				  fd :input (fd-stream-timeout stream))
			    (error 'io-timeout :stream stream :direction :read))
		    (do-input stream))
		   ((eql errno unix:econnreset)
		    (error 'socket-error
			   :format-control "Socket connection reset: ~A"
			   :format-arguments (list (unix:get-unix-error-msg errno))
			   :errno errno))
		   (t
		    (error "Error reading ~S: ~A"
			   stream
			   (unix:get-unix-error-msg errno)))))
	    ((zerop count)
	     (setf (fd-stream-listen stream) :eof)
	     (throw 'eof-input-catcher nil))
	    (t
	     (incf (fd-stream-ibuf-tail stream) count))))))
			
;;; INPUT-AT-LEAST -- internal
;;;
;;;   Makes sure there are at least ``bytes'' number of bytes in the input
;;; buffer. Keeps calling do-input until that condition is met.
;;;
(defmacro input-at-least (stream bytes)
  (let ((stream-var (gensym))
	(bytes-var (gensym)))
    `(let ((,stream-var ,stream)
	   (,bytes-var ,bytes))
       (loop
	 (when (>= (- (fd-stream-ibuf-tail ,stream-var)
		      (fd-stream-ibuf-head ,stream-var))
		   ,bytes-var)
	   (return))
	 (do-input ,stream-var)))))

;;; INPUT-WRAPPER -- intenal
;;;
;;;   Macro to wrap around all input routines to handle eof-error noise.
;;;
(defmacro input-wrapper ((stream bytes eof-error eof-value &optional type) &body read-forms)
  (let ((stream-var (gensym))
	(element-var (gensym)))
    `(let ((,stream-var ,stream))
       (if (fd-stream-unread ,stream-var)
	   (prog1
	       ,(if (eq type 'character) 
		    `(fd-stream-unread ,stream-var)
		    `(char-code (fd-stream-unread ,stream-var)))
	     (setf (fd-stream-unread ,stream-var) nil)
	     (setf (fd-stream-listen ,stream-var) nil))
	   (let ((,element-var
		  (catch 'eof-input-catcher
		    (input-at-least ,stream-var ,bytes)
		    ,@read-forms)))
	     (cond (,element-var
		    (incf (fd-stream-ibuf-head ,stream-var) ,bytes)
		    ,element-var)
		   (t
		    (eof-or-lose ,stream-var ,eof-error ,eof-value))))))))

;;; DEF-INPUT-ROUTINE -- internal
;;;
;;;   Defines an input routine.
;;;
(defmacro def-input-routine (name
			     (type size sap head)
			     &rest body)
  `(progn
     (defun ,name (stream eof-error eof-value)
       (input-wrapper (stream ,size eof-error eof-value ,type)
	 (let ((,sap (fd-stream-ibuf-sap stream))
	       (,head (fd-stream-ibuf-head stream)))
	   ,@body)))
     (setf *input-routines*
	   (nconc *input-routines*
		  (list (list ',type ',name ',size))))))

;;; INPUT-CHARACTER -- internal
;;;
;;;   Routine to use in stream-in slot for reading string chars.
;;;
(def-input-routine input-character
		   (character 1 sap head)
  (code-char (sap-ref-8 sap head)))

;;; INPUT-UNSIGNED-8BIT-BYTE -- internal
;;;
;;;   Routine to read in an unsigned 8 bit number.
;;;
(def-input-routine input-unsigned-8bit-byte
		   ((unsigned-byte 8) 1 sap head)
  (sap-ref-8 sap head))

;;; INPUT-SIGNED-8BIT-BYTE -- internal
;;;
;;;   Routine to read in a signed 8 bit number.
;;;
(def-input-routine input-signed-8bit-number
		   ((signed-byte 8) 1 sap head)
  (signed-sap-ref-8 sap head))

;;; INPUT-UNSIGNED-16BIT-BYTE -- internal
;;;
;;;   Routine to read in an unsigned 16 bit number.
;;;
(def-input-routine input-unsigned-16bit-byte
		   ((unsigned-byte 16) 2 sap head)
  (sap-ref-16 sap head))

;;; INPUT-SIGNED-16BIT-BYTE -- internal
;;;
;;;   Routine to read in a signed 16 bit number.
;;;
(def-input-routine input-signed-16bit-byte
		   ((signed-byte 16) 2 sap head)
  (signed-sap-ref-16 sap head))

;;; INPUT-UNSIGNED-32BIT-BYTE -- internal
;;;
;;;   Routine to read in a unsigned 32 bit number.
;;;
(def-input-routine input-unsigned-32bit-byte
		   ((unsigned-byte 32) 4 sap head)
  (sap-ref-32 sap head))

;;; INPUT-SIGNED-32BIT-BYTE -- internal
;;;
;;;   Routine to read in a signed 32 bit number.
;;;
(def-input-routine input-signed-32bit-byte
		   ((signed-byte 32) 4 sap head)
  (signed-sap-ref-32 sap head))

;;; PICK-INPUT-ROUTINE -- internal
;;;
;;;   Find an input routine to use given the type. Return as multiple values
;;; the routine, the real type transfered, and the number of bytes per element.
;;;
(defun pick-input-routine (type)
  (dolist (entry *input-routines*)
    (when (subtypep type (car entry))
      (return-from pick-input-routine
	(values (symbol-function (cadr entry))
		(car entry)
		(caddr entry)))))
  ;; FIXME: let's do it the hard way, then (but ignore things like
  ;; endianness, efficiency, and the necessary coupling between these
  ;; and the output routines).  -- CSR, 2004-02-09
  (loop for i from 40 by 8 to max-stream-element-size ; ARB (well, KLUDGE really)
	if (subtypep type `(unsigned-byte ,i))
	do (return-from pick-input-routine
	     (values
	      (lambda (stream eof-error eof-value)
		(input-wrapper (stream (/ i 8) eof-error eof-value)
		  (let ((sap (fd-stream-ibuf-sap stream))
			(head (fd-stream-ibuf-head stream)))
		    (loop for j from 0 below (/ i 8)
			  with result = 0
			  do (setf result
				   (+ (* 256 result)
				      (sap-ref-8 sap (+ head j))))
			  finally (return result)))))
	      `(unsigned-byte ,i)
	      (/ i 8))))
  (loop for i from 40 by 8 to max-stream-element-size ; ARB (well, KLUDGE really)
	if (subtypep type `(signed-byte ,i))
	do (return-from pick-input-routine
	     (values
	      (lambda (stream eof-error eof-value)
		(input-wrapper (stream (/ i 8) eof-error eof-value)
		  (let ((sap (fd-stream-ibuf-sap stream))
			(head (fd-stream-ibuf-head stream)))
		    (loop for j from 0 below (/ i 8)
			  with result = 0
			  do (setf result
				   (+ (* 256 result)
				      (sap-ref-8 sap (+ head j))))
		          finally (return (if (logbitp (1- i) result)
					      (dpb result (byte i 0) -1)
					      result))))))
	      `(signed-byte ,i)
	      (/ i 8)))))

;;; STRING-FROM-SAP -- internal
;;;
;;;   Returns a string constructed from the sap, start, and end.
;;;
(defun string-from-sap (sap start end)
  (declare (type index start end))
  (let* ((length (- end start))
	 (string (make-string length)))
    (copy-from-system-area sap (* start vm:byte-bits)
			   string (* vm:vector-data-offset vm:word-bits)
			   (* length vm:byte-bits))
    string))

#|
;;; FD-STREAM-READ-N-BYTES -- internal
;;;
;;; This version waits using server.  I changed to the non-server version
;;; because it allows this method to be used by CLX w/o confusing serve-event.
;;; The non-server method is also significantly more efficient for large
;;; reads. -- Ram
;;;
;;; The n-bin routine.
;;; 
(defun fd-stream-read-n-bytes (stream buffer start requested eof-error-p)
  (declare (type stream stream) (type index start requested))
  (let* ((sap (fd-stream-ibuf-sap stream))
	 (elsize (fd-stream-element-size stream))
	 (offset (* elsize start))
	 (bytes (* elsize requested))
	 (result
	  (catch 'eof-input-catcher
	    (loop
	      (input-at-least stream 1)
	      (let* ((head (fd-stream-ibuf-head stream))
		     (tail (fd-stream-ibuf-tail stream))
		     (available (- tail head))
		     (copy (min available bytes)))
		(if (typep buffer 'system-area-pointer)
		    (system-area-copy sap (* head vm:byte-bits)
				      buffer (* offset vm:byte-bits)
				      (* copy vm:byte-bits))
		    (copy-from-system-area sap (* head vm:byte-bits)
					   buffer (+ (* offset vm:byte-bits)
						     (* vm:vector-data-offset
							vm:word-bits))
					   (* copy vm:byte-bits)))
		(incf (fd-stream-ibuf-head stream) copy)
		(incf offset copy)
		(decf bytes copy))
	      (when (zerop bytes)
		(return requested))))))
    (or result
	(eof-or-lose stream eof-error-p 
		     (- requested (/ bytes elsize))))))
|#


;;; FD-STREAM-READ-N-BYTES -- internal
;;;
;;;    The N-Bin method for FD-STREAMs.  This doesn't use the SERVER; it blocks
;;; in UNIX-READ.  This allows the method to be used to implementing reading
;;; for CLX.  It is generally used where there is a definite amount of reading
;;; to be done, so blocking isn't too problematical.
;;;
;;;    We copy buffered data into the buffer.  If there is enough, just return.
;;; Otherwise, we see if the amount of additional data needed will fit in the
;;; stream buffer.  If not, inhibit GCing (so we can have a SAP into the Buffer
;;; argument), and read directly into the user supplied buffer.  Otherwise,
;;; read a buffer-full into the stream buffer and then copy the amount we need
;;; out.
;;;
;;;    We loop doing the reads until we either get enough bytes or hit EOF.  We
;;; must loop when eof-errorp is T because some streams (like pipes) may return
;;; a partial amount without hitting EOF.
;;;
(defun fd-stream-read-n-bytes (stream buffer start requested eof-error-p)
  (declare (type stream stream) (type index start requested))
  (let* ((sap (fd-stream-ibuf-sap stream))
	 (offset start)
	 (head (fd-stream-ibuf-head stream))
	 (tail (fd-stream-ibuf-tail stream))
	 (available (- tail head))
	 (copy (min requested available)))
    (declare (type index offset head tail available copy))
    ;;
    ;; If something has been unread, put that at buffer + start,
    ;; and read the rest to start + 1.
    (when (fd-stream-unread stream)
      (etypecase buffer
	(system-area-pointer
	 (assert (= 1 (fd-stream-element-size stream)))
	 (setf (sap-ref-8 buffer start) (char-code (read-char stream))))
	(string 
	 (setf (aref buffer start) (read-char stream)))
	(vector
	 (setf (aref buffer start) (char-code(read-char stream)))))
      (return-from fd-stream-read-n-bytes
	(1+ (fd-stream-read-n-bytes stream buffer (1+ start) (1- requested)
				    eof-error-p))))
    ;;
    (unless (zerop copy)
      (if (typep buffer 'system-area-pointer)
	  (system-area-copy sap (* head vm:byte-bits)
			    buffer (* offset vm:byte-bits)
			    (* copy vm:byte-bits))
	  (copy-from-system-area sap (* head vm:byte-bits)
				 buffer (+ (* offset vm:byte-bits)
					   (* vm:vector-data-offset
					      vm:word-bits))
				 (* copy vm:byte-bits)))
      (incf (fd-stream-ibuf-head stream) copy))
    (cond
     ((or (= copy requested)
	  (and (not eof-error-p) (/= copy 0)))
      copy)
     (t
      (setf (fd-stream-ibuf-head stream) 0)
      (setf (fd-stream-ibuf-tail stream) 0)
      (setf (fd-stream-listen stream) nil)
      (let ((now-needed (- requested copy))
	    (len (fd-stream-ibuf-length stream)))
	(declare (type index now-needed len))
	(cond
	 ((> now-needed len)
	  ;;
	  ;; If the desired amount is greater than the stream buffer size, then
	  ;; read directly into the destination, incrementing the start
	  ;; accordingly.  In this case, we never leave anything in the stream
	  ;; buffer.
	  (system:without-gcing
	    (loop
	      (multiple-value-bind
		  (count err)
		  (unix:unix-read (fd-stream-fd stream)
				  (sap+ (if (typep buffer 'system-area-pointer)
					    buffer
					    (vector-sap buffer))
					(+ offset copy))
				  now-needed)
		(declare (type (or index null) count))
		(unless count
		  (error "Error reading ~S: ~A" stream
			 (unix:get-unix-error-msg err)))
		(decf now-needed count)
		(if eof-error-p
		    (when (zerop count)
		      (error 'end-of-file :stream stream))
		    (return (- requested now-needed)))
		(when (zerop now-needed) (return requested))
		(incf offset count)))))
	 (t
	  ;;
	  ;; If we want less than the buffer size, then loop trying to fill the
	  ;; stream buffer and copying what we get into the destination.  When
	  ;; we have enough, we leave what's left in the stream buffer.
	  (loop
	    (multiple-value-bind
		(count err)
		(unix:unix-read (fd-stream-fd stream) sap len)
	      (declare (type (or index null) count))
	      (unless count
		(error "Error reading ~S: ~A" stream
		       (unix:get-unix-error-msg err)))
	      (when (and eof-error-p (zerop count))
		(error 'end-of-file :stream stream))

	      (let* ((copy (min now-needed count))
		     (copy-bits (* copy vm:byte-bits))
		     (buffer-start-bits
		      (* (+ offset available) vm:byte-bits)))
		(declare (type index copy copy-bits buffer-start-bits))
		(if (typep buffer 'system-area-pointer)
		    (system-area-copy sap 0
				      buffer buffer-start-bits
				      copy-bits)
		    (copy-from-system-area sap 0 
					   buffer (+ buffer-start-bits
						     (* vm:vector-data-offset
							vm:word-bits))
					   copy-bits))

		(decf now-needed copy)
		(when (or (zerop now-needed) (not eof-error-p))
		  (setf (fd-stream-ibuf-head stream) copy)
		  (setf (fd-stream-ibuf-tail stream) count)
		  (return (- requested now-needed)))
		(incf offset copy)))))))))))


;;;; Utility functions (misc routines, etc)

;;; SET-ROUTINES -- internal
;;;
;;;   Fill in the various routine slots for the given type. Input-p and
;;; output-p indicate what slots to fill. The buffering slot must be set prior
;;; to calling this routine.
;;;

(defun set-routines (stream type input-p output-p buffer-p &key binary-stream-p)
  (let ((target-type (case type
		       ((:default unsigned-byte)
			'(unsigned-byte 8))
		       (signed-byte
			'(signed-byte 8))
		       (t
			type)))
	(input-type nil)
	(output-type nil)
	(input-size nil)
	(output-size nil))
    
    (when (fd-stream-obuf-sap stream)
      (push (fd-stream-obuf-sap stream) *available-buffers*)
      (setf (fd-stream-obuf-sap stream) nil))
    (when (fd-stream-ibuf-sap stream)
      (push (fd-stream-ibuf-sap stream) *available-buffers*)
      (setf (fd-stream-ibuf-sap stream) nil))
    
    (when input-p
      (multiple-value-bind
	  (routine type size)
	  (pick-input-routine target-type)
	(unless routine
	  (error "Could not find any input routine for ~S" target-type))
	(setf (fd-stream-ibuf-sap stream) (next-available-buffer))
	(setf (fd-stream-ibuf-length stream) bytes-per-buffer)
	(setf (fd-stream-ibuf-tail stream) 0)
	(if (subtypep type 'character)
	    (setf (fd-stream-in stream) routine
		  (fd-stream-bin stream) #'ill-bin)
	    (setf (fd-stream-in stream) (if (and binary-stream-p
						 (eql size 1))
					    (pick-input-routine 'character) 
					    #'ill-in)
		  (fd-stream-bin stream) routine))
	(when (or (eql size 1)
		  (eql size 2)
		  (eql size 4))
	  ;; Support for n-byte operations on 8-, 16-, and 32-bit streams
	  (setf (fd-stream-n-bin stream) #'fd-stream-read-n-bytes)
	  (when (and buffer-p (eql size 1)
		     (or (eq type 'unsigned-byte)
			 (eq type :default)))
	    ;; We only create this buffer for streams of type
	    ;; (unsigned-byte 8).  Because there's no buffer, the
	    ;; other element-types will dispatch to the appropriate
	    ;; input (output) routine in fast-read-byte.
	    (setf (lisp-stream-in-buffer stream)
		  (make-array in-buffer-length
			      :element-type '(unsigned-byte 8)))))
	(setf input-size size)
	(setf input-type type)))

    (when output-p
      (multiple-value-bind
	  (routine type size)
	  (pick-output-routine target-type (fd-stream-buffering stream))
	(unless routine
	  (error "Could not find any output routine for ~S buffered ~S."
		 (fd-stream-buffering stream)
		 target-type))
	(setf (fd-stream-obuf-sap stream) (next-available-buffer))
	(setf (fd-stream-obuf-length stream) bytes-per-buffer)
	(setf (fd-stream-obuf-tail stream) 0)
	(if (subtypep type 'character)
	  (setf (fd-stream-out stream) routine
		(fd-stream-bout stream) #'ill-bout)
	  (setf (fd-stream-out stream)
		(or (if (eql size 1)
		      (pick-output-routine 'base-char
					   (fd-stream-buffering stream)))
		    #'ill-out)
		(fd-stream-bout stream) routine))
	(setf (fd-stream-sout stream)
	      (if (eql size 1) #'fd-sout #'ill-out))
	(setf (fd-stream-char-pos stream) 0)
	(setf output-size size)
	(setf output-type type)))

    (when (and input-size output-size
	       (not (eq input-size output-size)))
      (error "Element sizes for input (~S:~S) and output (~S:~S) differ?"
	     input-type input-size
	     output-type output-size))
    (setf (fd-stream-element-size stream)
	  (or input-size output-size))

    (setf (fd-stream-element-type stream)
	  (cond ((equal input-type output-type)
		 input-type)
		((null output-type)
		 input-type)
		((null input-type)
		 output-type)
		((subtypep input-type output-type)
		 input-type)
		((subtypep output-type input-type)
		 output-type)
		(t
		 (error "Input type (~S) and output type (~S) are unrelated?"
			input-type
			output-type))))))

;;; REVERT-FILE -- internal
;;;
;;;   Revert a file, if possible; otherwise do nothing.  Used during
;;; CLOSE when the abort flag is set.
;;;
(defun revert-file (filename original)
  (declare (type simple-base-string filename)
	   (type (or simple-base-string null) original))
  (when original
    (multiple-value-bind (okay err)
	(unix:unix-rename original filename)
      (unless okay
	  (cerror "Go on as if nothing bad happened."
		  "Could not restore ~S to its original contents: ~A"
		  filename (unix:get-unix-error-msg err))))))

;;; DELETE-ORIGINAL -- internal
;;;
;;;   Delete a backup file.  Used during CLOSE.
;;;
(defun delete-original (filename original)
  (declare (type simple-base-string filename)
	   (type (or simple-base-string null) original))
  (when original
    (multiple-value-bind (okay err) (unix:unix-unlink original)
      (unless okay
	(cerror "Go on as if nothing bad happened."
		"Could not delete ~S during close of ~S: ~A"
		original filename (unix:get-unix-error-msg err))))))

;;; FD-STREAM-MISC-ROUTINE -- input
;;;
;;;   Handle the various misc operations on fd-stream.
;;;
(defun fd-stream-misc-routine (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:listen 
     (or (not (eql (fd-stream-ibuf-head stream)
		   (fd-stream-ibuf-tail stream)))
	 (fd-stream-listen stream)
	 (setf (fd-stream-listen stream)
	       (eql (alien:with-alien ((read-fds (alien:struct unix:fd-set)))
		      (unix:fd-zero read-fds)
		      (unix:fd-set (fd-stream-fd stream) read-fds)
		      (unix:unix-fast-select (1+ (fd-stream-fd stream))
					     (alien:addr read-fds) nil nil
					     0 0))
		    1))))
    (:unread
     (setf (fd-stream-unread stream) arg1)
     (setf (fd-stream-listen stream) t))
    (:close
     (cond (arg1
	    ;; We got us an abort on our hands.
	    (when (fd-stream-handler stream)
		  (system:remove-fd-handler (fd-stream-handler stream))
		  (setf (fd-stream-handler stream) nil))
	    (when (and (fd-stream-file stream) (fd-stream-obuf-sap stream))
	      (revert-file (fd-stream-file stream)
			   (fd-stream-original stream))))
	   (t
	    (fd-stream-misc-routine stream :finish-output)
	    (when (fd-stream-delete-original stream)
	      (delete-original (fd-stream-file stream)
			       (fd-stream-original stream)))))
     (when (fboundp 'cancel-finalization)
       (cancel-finalization stream))
     (unix:unix-close (fd-stream-fd stream))
     (when (fd-stream-obuf-sap stream)
       (push (fd-stream-obuf-sap stream) *available-buffers*)
       (setf (fd-stream-obuf-sap stream) nil))
     (when (fd-stream-ibuf-sap stream)
       (push (fd-stream-ibuf-sap stream) *available-buffers*)
       (setf (fd-stream-ibuf-sap stream) nil))
     (lisp::set-closed-flame stream))
    (:clear-input
     (setf (fd-stream-unread stream) nil)
     (setf (fd-stream-ibuf-head stream) 0)
     (setf (fd-stream-ibuf-tail stream) 0)
     (catch 'eof-input-catcher
       (loop
	(multiple-value-bind
	      (count errno)
	    (alien:with-alien ((read-fds (alien:struct unix:fd-set)))
	      (unix:fd-zero read-fds)
	      (unix:fd-set (fd-stream-fd stream) read-fds)
	      (unix:unix-fast-select (1+ (fd-stream-fd stream))
				     (alien:addr read-fds) nil nil 0 0))
	  (cond ((eql count 1)
		 (do-input stream)
		 (setf (fd-stream-ibuf-head stream) 0)
		 (setf (fd-stream-ibuf-tail stream) 0))
		((and (not count) (eql errno unix:eintr)))
		(t
		 (return t)))))))
    (:force-output
     (flush-output-buffer stream))
    (:finish-output
     (flush-output-buffer stream)
     (do ()
	 ((null (fd-stream-output-later stream)))
       (system:serve-all-events)))
    (:element-type
     (fd-stream-element-type stream))
    (:interactive-p
     (unix:unix-isatty (fd-stream-fd stream)))
    (:line-length
     80)
    (:charpos
     (fd-stream-char-pos stream))
    (:file-length
     (unless (fd-stream-file stream)
       (error 'simple-type-error
	      :datum stream
	      :expected-type 'file-stream
	      :format-control "~s is not a stream associated with a file."
	      :format-arguments (list stream)))
     (multiple-value-bind
	 (okay dev ino mode nlink uid gid rdev size
	       atime mtime ctime blksize blocks)
	 (unix:unix-fstat (fd-stream-fd stream))
       (declare (ignore ino nlink uid gid rdev
			atime mtime ctime blksize blocks))
       (unless okay
	 (error 'simple-file-error
                :format-control "Error fstating ~S: ~A"
		:format-arguments (list stream (unix:get-unix-error-msg dev))))
       (if (zerop mode)
	   nil
	   (values (truncate size (fd-stream-element-size stream))))))
    (:file-position
     (fd-stream-file-position stream arg1))))


;;; FD-STREAM-FILE-POSITION -- internal.
;;;
(defun fd-stream-file-position (stream &optional newpos)
  (declare (type fd-stream stream)
	   (type (or (integer 0) (member nil :start :end)) newpos))
  (if (null newpos)
      (system:without-interrupts
	;; First, find the position of the UNIX file descriptor in the file.
	(multiple-value-bind
	      (posn errno)
	    (unix:unix-lseek (fd-stream-fd stream) 0 unix:l_incr)
	  (declare (type (or (integer 0) null) posn))
	  (cond (posn
		 ;; Adjust for buffered output:
		 ;;  If there is any output buffered, the *real* file position
		 ;; will be larger than reported by lseek because lseek
		 ;; obviously cannot take into account output we have not
		 ;; sent yet.
		 (dolist (later (fd-stream-output-later stream))
		   (incf posn (- (the index (caddr later))
				 (the index (cadr later)))))
		 (incf posn (fd-stream-obuf-tail stream))
		 ;; Adjust for unread input:
		 ;;  If there is any input read from UNIX but not supplied to
		 ;; the user of the stream, the *real* file position will
		 ;; smaller than reported, because we want to look like the
		 ;; unread stuff is still available.
		 (decf posn (- (fd-stream-ibuf-tail stream)
			       (fd-stream-ibuf-head stream)))
		 (when (fd-stream-unread stream)
		   (decf posn))
		 ;; Divide bytes by element size.
		 (truncate posn (fd-stream-element-size stream)))
		((eq errno unix:espipe)
		 nil)
		(t
		 (system:with-interrupts
		   (error "Error lseek'ing ~S: ~A"
			  stream
			  (unix:get-unix-error-msg errno)))))))
      (let ((offset 0)
	    origin)
	(declare (type (integer 0) offset))
	;; Make sure we don't have any output pending, because if we move the
	;; file pointer before writing this stuff, it will be written in the
	;; wrong location.
	(flush-output-buffer stream)
	(do ()
	    ((null (fd-stream-output-later stream)))
	  (system:serve-all-events))
	;; Clear out any pending input to force the next read to go to the
	;; disk.
	(setf (fd-stream-unread stream) nil)
	(setf (fd-stream-ibuf-head stream) 0)
	(setf (fd-stream-ibuf-tail stream) 0)
	;; Trash cached value for listen, so that we check next time.
	(setf (fd-stream-listen stream) nil)
	;; Now move it.
	(cond ((eq newpos :start)
	       (setf offset 0
		     origin unix:l_set))
	      ((eq newpos :end)
	       (setf offset 0
		     origin unix:l_xtnd))
	      ((typep newpos '(integer 0))
	       (setf offset (* newpos (fd-stream-element-size stream))
		     origin unix:l_set))
	      (t
	       (error "Invalid position given to file-position: ~S" newpos)))
	(multiple-value-bind
	    (posn errno)
	    (unix:unix-lseek (fd-stream-fd stream) offset origin)
	  (cond (posn
		 t)
		((eq errno unix:espipe)
		 nil)
		(t
		 (error "Error lseek'ing ~S: ~A"
			stream
			(unix:get-unix-error-msg errno))))))))



;;;; Creation routines (MAKE-FD-STREAM and OPEN)

;;; MAKE-FD-STREAM -- Public.
;;;
;;; Returns a FD-STREAM on the given file.
;;;
(defun make-fd-stream (fd
		       &key
		       (input nil input-p)
		       (output nil output-p)
		       (element-type 'base-char)
		       (buffering :full)
		       timeout
		       file
		       original
		       delete-original
		       pathname
		       input-buffer-p
		       (name (if file
				 (format nil "file ~S" file)
				 (format nil "descriptor ~D" fd)))
		       auto-close
		       binary-stream-p)
  (declare (type index fd) (type (or index null) timeout)
	   (type (member :none :line :full) buffering))
  "Create a stream for the given unix file descriptor.
  If input is non-nil, allow input operations.
  If output is non-nil, allow output operations.
  If neither input nor output are specified, default to allowing input.
  Element-type indicates the element type to use (as for open).
  Buffering indicates the kind of buffering to use.
  Timeout (if true) is the number of seconds to wait for input.  If NIL (the
    default), then wait forever.  When we time out, we signal IO-TIMEOUT.
  File is the name of the file (will be returned by PATHNAME).
  Name is used to identify the stream when printed."
  (cond ((not (or input-p output-p))
	 (setf input t))
	((not (or input output))
	 (error "File descriptor must be opened either for input or output.")))
  (let ((stream (if binary-stream-p
		    (%make-binary-text-stream :fd fd
					      :name name
					      :file file
					      :original original
					      :delete-original delete-original
					      :pathname pathname
					      :buffering buffering
					      :timeout timeout)
		    (%make-fd-stream :fd fd
				     :name name
				     :file file
				     :original original
				     :delete-original delete-original
				     :pathname pathname
				     :buffering buffering
				     :timeout timeout))))
    (set-routines stream element-type input output input-buffer-p
		  :binary-stream-p binary-stream-p)
    (when (and auto-close (fboundp 'finalize))
      (finalize stream
		#'(lambda ()
		    (unix:unix-close fd)
		    (format *terminal-io* "** Closed ~A~%" name)
		    (when original
		      (revert-file file original)))))
    stream))


;;; PICK-BACKUP-NAME -- internal
;;;
;;; Pick a name to use for the backup file.
;;;
(defvar *backup-extension* ".BAK"
  "This is a string that OPEN tacks on the end of a file namestring to produce
   a name for the :if-exists :rename-and-delete and :rename options.  Also,
   this can be a function that takes a namestring and returns a complete
   namestring.")
;;;
(defun pick-backup-name (name)
  (declare (type simple-string name))
  (let ((ext *backup-extension*))
    (etypecase ext
      (simple-string (concatenate 'simple-string name ext))
      (function (funcall ext name)))))

;;; NEXT-VERSION -- internal
;;;
;;; Find the next available versioned name for a file.
;;;
(defun next-version (name)
  (declare (type simple-string name))
  (let* ((sep (position #\/ name :from-end t))
	 (base (if sep (subseq name 0 (1+ sep)) ""))
	 (dir (unix:open-dir base)))
    (multiple-value-bind (name type version)
	(extract-name-type-and-version name (if sep (1+ sep) 0) (length name))
      (let ((version (if (symbolp version) 1 (1+ version)))
	    (match (if type
		       (concatenate 'string name "." type ".~")
		       (concatenate 'string name ".~"))))
	(when dir
	  (unwind-protect
	       (loop
		 (let ((name (unix:read-dir dir)))
		   (cond ((null name) (return))
			 ((and (> (length name) (length match))
			       (string= name match :end1 (length match)))
			  (multiple-value-bind (v e)
			      (parse-integer name :start (length match)
						  :junk-allowed t)
			    (when (and v
				       (= (length name) (1+ e))
				       (char= (schar name e) #\~))
			      (setq version (max version (1+ v)))))))))
	    (unix:close-dir dir)))
	(concatenate 'string base
		     match (quick-integer-to-string version) "~")))))

;;; ASSURE-ONE-OF -- internal
;;;
;;; Assure that the given arg is one of the given list of valid things.
;;; Allow the user to fix any problems.
;;; 
(defun assure-one-of (item list what)
  (unless (member item list)
    (loop
      (cerror "Enter new value for ~*~S"
	      "~S is invalid for ~S. Must be one of~{ ~S~}"
	      item
	      what
	      list)
      (format (the stream *query-io*) "Enter new value for ~S: " what)
      (force-output *query-io*)
      (setf item (read *query-io*))
      (when (member item list)
	(return))))
  item)

;;; DO-OLD-RENAME  --  Internal
;;;
;;;    Rename Namestring to Original.  First, check if we have write access,
;;; since we don't want to trash unwritable files even if we technically can.
;;; We return true if we succeed in renaming.
;;;
(defun do-old-rename (namestring original)
  (unless (unix:unix-access namestring unix:w_ok)
    (cerror "Try to rename it anyway." "File ~S is not writable." namestring))
  (multiple-value-bind
      (okay err)
      (unix:unix-rename namestring original)
    (cond (okay t)
	  (t
	   (cerror "Use :SUPERSEDE instead."
		   "Could not rename ~S to ~S: ~A."
		   namestring
		   original
		   (unix:get-unix-error-msg err))
	   nil))))

;;; FD-OPEN  --  Internal
;;;
;;;    Open a file.
;;;
(defun fd-open (pathname direction if-exists if-exists-given
                          if-does-not-exist if-does-not-exist-given)
  (declare (type pathname pathname)
           (type (member :input :output :io :probe) direction)
           (type (member :error :new-version :rename :rename-and-delete
                         :overwrite :append :supersede nil) if-exists)
           (type (member :error :create nil) if-does-not-exist))
  (multiple-value-bind (input output mask)
      (ecase direction
        (:input (values t nil unix:o_rdonly))
        (:output (values nil t unix:o_wronly))
        (:io (values t t unix:o_rdwr))
        (:probe (values t nil unix:o_rdonly)))
    (declare (type index mask))
    ;; Process if-exists argument if we are doing any output.
    (cond (output
           (unless if-exists-given
             (setf if-exists
                   (if (eq (pathname-version pathname) :newest)
                       :new-version
                       :error)))
           (case if-exists
             ((:error nil)
              (setf mask (logior mask unix:o_excl)))
             ((:new-version :rename :rename-and-delete)
              (setf mask (logior mask unix:o_creat)))
             (:supersede
              (setf mask (logior mask unix:o_trunc)))))
          (t
           (setf if-exists nil)))     ; :ignore-this-arg
    
    (unless if-does-not-exist-given
      (setf if-does-not-exist
            (cond ((eq direction :input) :error)
                  ((and output
                        (member if-exists '(:overwrite :append)))
                   :error)
                  ((eq direction :probe)
                   nil)
                  (t
                   :create))))
    (if (eq if-does-not-exist :create)
        (setf mask (logior mask unix:o_creat)))
    
    (let ((name (cond ((unix-namestring pathname input))
                      ((and input (eq if-does-not-exist :create))
                       (unix-namestring pathname nil)))))
      (let ((original (cond ((eq if-exists :new-version)
			     (next-version name))
			    ((member if-exists '(:rename :rename-and-delete))
			     (pick-backup-name name))))
            (delete-original (eq if-exists :rename-and-delete))
            (mode #o666))
        (when original
          ;; We are doing a :rename or :rename-and-delete.
          ;; Determine if the file already exists, make sure the original
          ;; file is not a directory and keep the mode
          (let ((exists
                 (and name
                      (multiple-value-bind
                            (okay err/dev inode orig-mode)
                          (unix:unix-stat name)
                        (declare (ignore inode)
                                 (type (or index null) orig-mode))
                        (cond
                          (okay
                           (when (and output (= (logand orig-mode #o170000)
                                                #o40000))
                             (error 'simple-file-error
                                 :pathname pathname
                                 :format-control
                                 "Cannot open ~S for output: Is a directory."
                                 :format-arguments (list name)))
                           (setf mode (logand orig-mode #o777))
                           t)
                          ((eql err/dev unix:enoent)
                           nil)
                          (t
                           (error 'simple-file-error
                                  :pathname pathname
                                  :format-control "Cannot find ~S: ~A"
                                  :format-arguments
                                    (list name
                                      (unix:get-unix-error-msg err/dev)))))))))
            (unless (and exists
                         (do-old-rename name original))
              (setf original nil)
              (setf delete-original nil)
              ;; In order to use SUPERSEDE instead, we have
              ;; to make sure unix:o_creat corresponds to
              ;; if-does-not-exist.  unix:o_creat was set
              ;; before because of if-exists being :rename.
              (unless (eq if-does-not-exist :create)
                (setf mask (logior (logandc2 mask unix:o_creat)
                                   unix:o_trunc)))
              (setf if-exists :supersede))))
        
        ;; Okay, now we can try the actual open.
        (loop
          (multiple-value-bind (fd errno)
              (if name
                  (unix:unix-open name mask mode)
                  (values nil unix:enoent))
            (cond ((fixnump fd)
		   (when (eq if-exists :append)
		     (unix:unix-lseek fd 0 unix:l_xtnd)) ; SEEK_END
                   (return (values fd name original delete-original)))
                  ((eql errno unix:enoent)
                   (case if-does-not-exist
                     (:error
                       (cerror "Return NIL."
                               'simple-file-error
                               :pathname pathname
                               :format-control "Error opening ~S, ~A."
                               :format-arguments
                                   (list pathname
                                         (unix:get-unix-error-msg errno))))
                     (:create
                       (cerror "Return NIL."
                               'simple-file-error
                               :pathname pathname
                               :format-control
                                   "Error creating ~S, path does not exist."
                               :format-arguments (list pathname))))
                   (return nil))
                  ((eql errno unix:eexist)
                   (unless (eq nil if-exists)
                     (cerror "Return NIL."
                             'simple-file-error
                             :pathname pathname
                             :format-control "Error opening ~S, ~A."
                             :format-arguments
                                 (list pathname
                                       (unix:get-unix-error-msg errno))))
                   (return nil))
                  ((eql errno unix:eacces)
                   (cerror "Try again."
                           'simple-file-error
                           :pathname pathname
                           :format-control "Error opening ~S, ~A."
                           :format-arguments
                               (list pathname
                                     (unix:get-unix-error-msg errno))))
                  (t
                   (cerror "Return NIL."
                           'simple-file-error
                           :pathname pathname
                           :format-control "Error opening ~S, ~A."
                           :format-arguments
                               (list pathname
                                     (unix:get-unix-error-msg errno)))
                   (return nil)))))))))

;;; OPEN-FD-STREAM  --  Internal
;;;
;;;    Open an fd-stream connected to a file.
;;;
(defun open-fd-stream (pathname &key (direction :input)
				(element-type 'base-char)
				(if-exists nil if-exists-given)
				(if-does-not-exist nil if-does-not-exist-given)
				(external-format :default)
		                class)
  (declare (type pathname pathname)
           (type (member :input :output :io :probe) direction)
           (type (member :error :new-version :rename :rename-and-delete
                         :overwrite :append :supersede nil) if-exists)
           (type (member :error :create nil) if-does-not-exist)
           (ignore external-format))
  (multiple-value-bind (fd namestring original delete-original)
      (fd-open pathname direction if-exists if-exists-given
	       if-does-not-exist if-does-not-exist-given)
    (when fd
      (case direction
	((:input :output :io)
	 ;; We use the :class option to tell us if we want a
	 ;; binary-text stream or not.
	 (make-fd-stream fd
			 :input (member direction '(:input :io))
			 :output (member direction '(:output :io))
			 :element-type element-type
			 :file namestring
			 :original original
			 :delete-original delete-original
			 :pathname pathname
			 :input-buffer-p t
			 :auto-close t
			 :binary-stream-p class))
	(:probe
	 (let ((stream (%make-fd-stream :name namestring :fd fd
					:pathname pathname
					:element-type element-type)))
	   (close stream)
	   stream))))))

;;; OPEN -- public
;;;
;;;   Open the given file.
;;;
(defun open (filename &rest options
		      &key (direction :input)
			   (element-type 'base-char element-type-given)
			   (if-exists nil if-exists-given)
			   (if-does-not-exist nil if-does-not-exist-given)
			   (external-format :default)
			   class mapped input-handle output-handle
		      &allow-other-keys
		      &aux ; Squelch assignment warning.
		      (direction direction)
		      (if-does-not-exist if-does-not-exist)
		      (if-exists if-exists))
  "Return a stream which reads from or writes to Filename.
  Defined keywords:
   :direction - one of :input, :output, :io, or :probe
   :element-type - Type of object to read or write, default BASE-CHAR
   :if-exists - one of :error, :new-version, :rename, :rename-and-delete,
                       :overwrite, :append, :supersede or nil
   :if-does-not-exist - one of :error, :create or nil
   :external-format - :default
  See the manual for details."
  (declare (ignore external-format input-handle output-handle))

  ;; OPEN signals a file-error if the filename is wild.
  (when (wild-pathname-p filename)
    (error 'file-error :pathname filename))
  
  ;; First, make sure that DIRECTION is valid. Allow it to be changed if not.
  (setq direction
	(assure-one-of direction
		       '(:input :output :io :probe)
		       :direction))
  (setf (getf options :direction) direction)

  (when (and if-exists-given (member direction '(:output :io)))
    (setq if-exists
	  (assure-one-of if-exists
			 '(:error :new-version :rename
			   :rename-and-delete :overwrite
			   :append :supersede nil)
			 :if-exists))
    (setf (getf options :if-exists) if-exists))

  (when if-does-not-exist-given
    (setq if-does-not-exist
	  (assure-one-of if-does-not-exist
			 '(:error :create nil)
			 :if-does-not-exist))
    (setf (getf options :if-does-not-exist) if-does-not-exist))

  (let ((filespec (pathname filename))
	(options (copy-list options))
	(class (or class 'fd-stream)))
    (cond ((eq class 'fd-stream)
	   (remf options :class)
           (remf options :mapped)
           (remf options :input-handle)
           (remf options :output-handle)
           (apply #'open-fd-stream filespec options))
	  ((eq class 'binary-text-stream)
	   ;; Like fd-stream, but binary and text allowed.  This is
	   ;; indicated by leaving the :class option around for
	   ;; open-fd-stream to see.
           (remf options :mapped)
           (remf options :input-handle)
           (remf options :output-handle)
	   (apply #'open-fd-stream filespec options))
	  ((subtypep class 'stream:simple-stream)
	   (when element-type-given
             (cerror "Do it anyway."
		     "Can't create simple-streams with an element-type."))
           (when (and (eq class 'stream:file-simple-stream) mapped)
             (setq class 'stream:mapped-file-simple-stream)
             (setf (getf options :class) 'stream:mapped-file-simple-stream))
           (when (subtypep class 'stream:file-simple-stream)
             (when (eq direction :probe)
               (setq class 'stream:probe-simple-stream)))
           (apply #'make-instance class :filename filespec options))
	  ((subtypep class 'ext:fundamental-stream)
	   (remf options :class)
           (remf options :mapped)
           (remf options :input-handle)
           (remf options :output-handle)
	   (let ((stream (apply #'open-fd-stream filespec options)))
	     (when stream
	       (make-instance class :lisp-stream stream))))
	  (t
	   (error "Unable to open streams of class ~S." class)))))

;;;; Initialization.

(defvar *tty* nil
  "The stream connected to the controlling terminal or NIL if there is none.")
(defvar *stdin* nil
  "The stream connected to the standard input (file descriptor 0).")
(defvar *stdout* nil
  "The stream connected to the standard output (file descriptor 1).")
(defvar *stderr* nil
  "The stream connected to the standard error output (file descriptor 2).")

;;; STREAM-INIT -- internal interface
;;;
;;; Called when the cold load is first started up.
;;; 
(defun stream-init ()
  (stream-reinit)
  (setf *terminal-io* (make-synonym-stream '*tty*))
  (setf *standard-output* (make-synonym-stream '*stdout*))
  (setf *standard-input*
	(make-two-way-stream (make-synonym-stream '*stdin*)
			     *standard-output*))
  (setf *error-output* (make-synonym-stream '*stderr*))
  (setf *query-io* (make-synonym-stream '*terminal-io*))
  (setf *debug-io* *query-io*)
  (setf *trace-output* *standard-output*)
  nil)

;;; STREAM-REINIT -- internal interface
;;;
;;; Called whenever a saved core is restarted.
;;; 
(defun stream-reinit ()
  (setf *available-buffers* nil)
  (setf *stdin*
	(make-fd-stream 0 :name "Standard Input" :input t :buffering :line))
  (setf *stdout*
	(make-fd-stream 1 :name "Standard Output" :output t :buffering :line))
  (setf *stderr*
	(make-fd-stream 2 :name "Standard Error" :output t :buffering :line))
  (let ((tty (and (not *batch-mode*)
		  (unix:unix-open "/dev/tty" unix:o_rdwr #o666))))
    (setf *tty*
	  (if tty
	      (make-fd-stream tty :name "the Terminal" :input t :output t
			      :buffering :line :auto-close t)
	      (make-two-way-stream *stdin* *stdout*))))
  nil)


;;;; Beeping.

(defun default-beep-function (stream)
  (write-char #\bell stream)
  (finish-output stream))

(defvar *beep-function* #'default-beep-function
  "This is called in BEEP to feep the user.  It takes a stream.")

(defun beep (&optional (stream *terminal-io*))
  (funcall *beep-function* stream))


;;; File-Name  --  internal interface
;;;
;;;    Kind of like File-Position, but is an internal hack used by the filesys
;;; stuff to get and set the file name.
;;;
(defun file-name (stream &optional new-name)
  (typecase stream
    (stream:simple-stream
     (if new-name
	 (stream::%file-rename stream new-name)
	 (stream::%file-name stream)))
    (fd-stream
     (cond (new-name
	    (setf (fd-stream-pathname stream) new-name)
	    (setf (fd-stream-file stream)
		  (unix-namestring new-name nil))
	    t)
	   (t
	    (fd-stream-pathname stream))))))


;;;; Degenerate international character support:

(defun file-string-length (stream object)
  (declare (type (or string character) object)
	   (type (or file-stream broadcast-stream stream:simple-stream) stream))
  "Return the delta in Stream's FILE-POSITION that would be caused by writing
   Object to Stream.  Non-trivial only in implementations that support
   international character sets."
  (typecase stream
    (stream:simple-stream (stream::%file-string-length stream object))
    (broadcast-stream
     ;; CLHS says we must return 1 in this case
     1)
    (t
     (etypecase object
       (character 1)
       (string (length object))))))
