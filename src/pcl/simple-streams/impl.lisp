;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/simple-streams/impl.lisp,v 1.7 2004/12/06 17:07:05 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Implementations of standard Common Lisp functions for simple-streams

(in-package "STREAM")

(defun %uninitialized (stream)
  (error "~S has not been initialized." stream))

(defun %check (stream kind)
  (declare (type simple-stream stream)
	   (optimize (speed 3) (space 1) (debug 0) (safety 0)))
  (with-stream-class (simple-stream stream)
    (cond ((not (any-stream-instance-flags stream :simple))
	   (%uninitialized stream))
	  ((and (eq kind :open)
		(not (any-stream-instance-flags stream :input :output)))
	   (lisp::closed-flame stream))
	  ((and (or (eq kind :input) (eq kind :io))
		(not (any-stream-instance-flags stream :input)))
	   (lisp::ill-in-any stream))
	  ((and (or (eq kind :output) (eq kind :io))
		(not (any-stream-instance-flags stream :output)))
	   (lisp::ill-out-any stream)))))

#+count-sm
(progn
  (defvar *sm-r-count* (make-hash-table))
  (defvar *sm-w-count* (make-hash-table))
  (defun %sm (slot object)
    (incf (gethash slot *sm-r-count* 0))
    (slot-value object slot))
  (defun (setf %sm) (value slot object)
    (incf (gethash slot *sm-w-count* 0))
    (setf (slot-value object slot) value)))

(defun %input-stream-p (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream nil)
    (any-stream-instance-flags stream :input)))

(defun %output-stream-p (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream nil)
    (any-stream-instance-flags stream :output)))

(defun %open-stream-p (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream nil)
    (any-stream-instance-flags stream :input :output)))

(defun %interactive-stream-p (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (any-stream-instance-flags stream :interactive)))

(defun %interactive-stream-y (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (add-stream-instance-flags stream :interactive)))

(defun %interactive-stream-n (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (remove-stream-instance-flags stream :interactive)))

(defun %stream-external-format (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (sm external-format stream)))

(defun %charpos (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (sm charpos stream)))

(defun %file-position (stream position)
  (declare (type simple-stream stream)
	   (type (or (integer 0 *) (member nil :start :end)) position))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (if position
	;; Adjust current position
	(let ((position (case position (:start 0) (:end -1)
			      (otherwise position))))
	  (simple-stream-dispatch stream
	    ;; single-channel-simple-stream
	    (when (sc-dirty-p stream)
	      (flush-buffer stream t))
	    ;; dual-channel-simple-stream
	    (with-stream-class (dual-channel-simple-stream stream)
	      (when (> (sm outpos stream) 0)
		(device-write stream :flush 0 nil t)))
	    ;; string-simple-stream
	    nil)

	  (setf (sm last-char-read-size stream) 0)
	  (setf (sm buffpos stream) 0   ; set pointer to 0 to force a read
		(sm buffer-ptr stream) 0)
	  (setf (sm charpos stream) nil)
	  (remove-stream-instance-flags stream :eof)
	  (setf	(device-file-position stream) position))
	;; Just report current position
	(let ((posn (device-file-position stream)))
	  (when posn
	    (when (sm handler stream)
	      (dolist (queued (sm pending stream))
		(incf posn (- (the lisp::index (third queued))
			      (the lisp::index (second queued))))))
	    (simple-stream-dispatch stream
	      ;; single-channel-simple-stream
	      (case (sm mode stream)
		((0 3)			; read, read-modify
		 (decf posn (- (sm buffer-ptr stream) (sm buffpos stream))))
		(1			; write
		 (incf posn (sm buffpos stream))))
	    ;; dual-channel-simple-stream
	    (with-stream-class (dual-channel-simple-stream stream)
	      (incf posn (sm outpos stream))
	      (when (>= (sm buffer-ptr stream) 0)
		(decf posn (- (sm buffer-ptr stream) (sm buffpos stream)))))
	    ;; string-simple-stream
	    nil))
	posn))))

(defun %file-length (stream)
  (declare (type simple-stream stream))
  (%check stream :open)
  (device-file-length stream))

(defun %file-name (stream)
  (declare (type simple-stream stream))
  (%check stream nil)
  (typecase stream
    (file-simple-stream
     (with-stream-class (file-simple-stream stream)
       (sm pathname stream)))
    (probe-simple-stream
     (with-stream-class (probe-simple-stream stream)
       (%check stream nil)
       (sm pathname stream)))
    (otherwise
     nil)))

(defun %file-rename (stream new-name)
  (declare (type simple-stream stream))
  (%check stream nil)
  (if (typep stream 'file-simple-stream)
      (with-stream-class (file-simple-stream stream)
	(setf (sm pathname stream) new-name)
	(setf (sm filename stream) (ext:unix-namestring new-name nil))
	t)
      nil))

(defun %file-string-length (stream object)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    ;; FIXME: need to account for compositions on the stream...
    (let ((count 0))
      (flet ((fn (octet)
	       (declare (ignore octet))
	       (incf count)))
	(etypecase object
	  (character
	   (let ((x nil))
	     (char-to-octets (sm external-format stream) object x #'fn)))
	  (string
	   (let ((x nil)
		 (ef (sm external-format stream)))
	     (dotimes (i (length object))
	       (declare (type lisp::index i))
	       (char-to-octets ef (char object i) x #'fn))))))
      count)))

(defun %read-line (stream eof-error-p eof-value recursive-p)
  (declare (optimize (speed 3) (space 1) (safety 0) (debug 0))
	   (type simple-stream stream)
	   (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %read-line
	(lisp::eof-or-lose stream eof-error-p eof-value)))
    ;; for interactive streams, finish output first to force prompt
    (when (and (any-stream-instance-flags stream :output)
	       (any-stream-instance-flags stream :interactive))
      (%finish-output stream))
    (let* ((encap (sm melded-stream stream)) ; encapsulating stream
	   (cbuf (make-string 80))	; current buffer
	   (bufs (list cbuf))		; list of buffers
	   (tail bufs)			; last cons of bufs list
	   (index 0)			; current index in current buffer
	   (total 0))			; total characters
      (declare (type simple-stream encap)
	       (type simple-base-string cbuf)
	       (type cons bufs tail)
	       (type lisp::index index total))
      (loop
	(multiple-value-bind (chars done)
	    (funcall-stm-handler j-read-chars encap cbuf
				 #\Newline index (length cbuf) t)
	  (declare (type lisp::index chars))
	  (incf index chars)
	  (incf total chars)
	  (when (and (eq done :eof) (zerop total))
	    (if eof-error-p
		(error 'end-of-file :stream stream)
		(return (values eof-value t))))
	  (when done
	    ;; If there's only one buffer in use, return it directly
	    (when (null (cdr bufs))
	      (return (values (lisp::shrink-vector cbuf total)
			      (eq done :eof))))
	    ;; If total fits in final buffer, use it
	    (when (<= total (length cbuf))
	      (replace cbuf cbuf :start1 (- total index) :end2 index)
	      (let ((idx 0))
		(declare (type lisp::index idx))
		(do ((list bufs (cdr list)))
		    ((eq list tail))
		  (let ((buf (car list)))
		    (declare (type simple-base-string buf))
		    (replace cbuf buf :start1 idx)
		    (incf idx (length buf)))))
	      (return (values (lisp::shrink-vector cbuf total)
			      (eq done :eof))))
	    ;; Allocate new string of appropriate length
	    (let ((string (make-string total))
		  (index 0))
	      (declare (type lisp::index index))
	      (dolist (buf bufs)
		(declare (type simple-base-string buf))
		(replace string buf :start1 index)
		(incf index (length buf)))
	      (return  (values string (eq done :eof)))))
	  (when (>= index (length cbuf))
	    (setf cbuf (make-string (the lisp::index (* 2 index))))
	    (setf index 0)
	    (setf (cdr tail) (cons cbuf nil))
	    (setf tail (cdr tail))))))))

(defun %read-char (stream eof-error-p eof-value recursive-p blocking-p)
  (declare (type simple-stream stream)
	   (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %read-char
	(lisp::eof-or-lose stream eof-error-p eof-value)))
    ;; for interactive streams, finish output first to force prompt
    (when (and (any-stream-instance-flags stream :output)
	       (any-stream-instance-flags stream :interactive))
      (%finish-output stream))
    (funcall-stm-handler j-read-char (sm melded-stream stream)
			 eof-error-p eof-value blocking-p)))

(defun %unread-char (stream character)
  (declare (type simple-stream stream) (ignore character))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (if (zerop (sm last-char-read-size stream))
	(error "Nothing to unread.")
	(progn
	  (funcall-stm-handler j-unread-char (sm melded-stream stream) nil)
	  (remove-stream-instance-flags stream :eof)
	  (setf (sm last-char-read-size stream) 0)))))

(defun %peek-char (stream peek-type eof-error-p eof-value recursive-p)
  (declare (type simple-stream stream)
	   (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %peek-char
	(lisp::eof-or-lose stream eof-error-p eof-value)))
    (let* ((encap (sm melded-stream stream))
	   (char (funcall-stm-handler j-read-char encap
				     eof-error-p stream t)))
      (cond ((eq char stream) eof-value)
	    ((characterp peek-type)
	     (do ((char char (funcall-stm-handler j-read-char encap
						  eof-error-p
						  stream t)))
		 ((or (eq char stream) (char= char peek-type))
		  (unless (eq char stream)
		    (funcall-stm-handler j-unread-char encap t))
		  (if (eq char stream) eof-value char))))
	    ((eq peek-type t)
	     (do ((char char (funcall-stm-handler j-read-char encap
						  eof-error-p
						  stream t)))
		 ((or (eq char stream)
		      (not (lisp::whitespace-char-p char)))
		  (unless (eq char stream)
		    (funcall-stm-handler j-unread-char encap t))
		  (if (eq char stream) eof-value char))))
	    (t
	     (funcall-stm-handler j-unread-char encap t)
	     char)))))

(defun %listen (stream width)
  (declare (type simple-stream stream))
  ;; WIDTH is number of octets which must be available; any value
  ;; other than 1 is treated as 'character.
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %listen nil))
    (if (not (or (eql width 1) (null width)))
	(funcall-stm-handler j-listen (sm melded-stream stream))
	(or (< (sm buffpos stream) (sm buffer-ptr stream))
	    ;; Note: should try DEVICE-EXTEND for more on buffer streams
	    (when (>= (sm mode stream) 0) ;; device-connected @@ single-channel
	      (let ((lcrs (sm last-char-read-size stream)))
		(unwind-protect
		     (progn
		       (setf (sm last-char-read-size stream) (1+ lcrs))
		       (plusp (refill-buffer stream nil)))
		  (setf (sm last-char-read-size stream) lcrs))))))))

(defun %clear-input (stream buffer-only)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (setf (sm buffpos stream) 0
	  (sm buffer-ptr stream) 0
	  (sm last-char-read-size stream) 0
	  #|(sm unread-past-soft-eof stream) nil|#)
    #| (setf (sm reread-count stream) 0)  on dual-channel streams? |#
    )
  (device-clear-input stream buffer-only))

(defun %read-byte (stream eof-error-p eof-value)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (if (any-stream-instance-flags stream :eof)
	(lisp::eof-or-lose stream eof-error-p eof-value)
	(simple-stream-dispatch-2 stream
	  ;; single/dual-channel-simple-stream
	  (read-byte-internal stream eof-error-p eof-value t)
	  ;; string-simple-stream
	  (with-stream-class (string-simple-stream stream)
	    (let ((encap (sm input-handle stream)))
	      (unless encap
		(error 'simple-type-error
		       :datum stream
		       :expected-type 'stream
		       :format-control "Can't read-byte on string streams"
		       :format-arguments '()))
	      (prog1
		  (read-byte encap eof-error-p eof-value)
		(setf (sm last-char-read-size stream) 0
		      (sm encapsulated-char-read-size stream) 0))))))))

(defun %write-char (stream character)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (funcall-stm-handler-2 j-write-char character (sm melded-stream stream))))

(defun %fresh-line (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (when (/= (or (sm charpos stream) 1) 0)
      (funcall-stm-handler-2 j-write-char #\Newline (sm melded-stream stream))
      t)))

(defun %write-string (stream string start end)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (funcall-stm-handler-2 j-write-chars string (sm melded-stream stream)
			   start end)))

(defun %line-length (stream)
  (declare (type simple-stream stream))
  (%check stream :output)
  ;; implement me
  nil)


(defun %finish-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (when (sm handler stream)
      (do ()
	  ((null (sm pending stream)))
	(sys:serve-all-events)))
    (device-write stream :flush 0 nil t)
    (simple-stream-dispatch stream
      ;; single-channel-simple-stream
      (setf (sm buffpos stream) 0)
      ;; dual-channel-simple-stream
      (with-stream-class (dual-channel-simple-stream stream)
	(setf (sm outpos stream) 0))
      ;; string-simple-stream
      nil))
  nil)

(defun %force-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (device-write stream :flush 0 nil nil)
    (simple-stream-dispatch stream
      ;; single-channel-simple-stream
      (setf (sm buffpos stream) 0)
      ;; dual-channel-simple-stream
      (with-stream-class (dual-channel-simple-stream stream)
	(setf (sm outpos stream) 0))
      ;; string-simple-stream
      nil))
  nil)

(defun %clear-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (when (sm handler stream)
      (sys:remove-fd-handler (sm handler stream))
      (setf (sm handler stream) nil
	    (sm pending stream) nil))
    (simple-stream-dispatch stream
      ;; single-channel-simple-stream
      (with-stream-class (single-channel-simple-stream stream)
	(case (sm mode stream)
	  (1 (setf (sm buffpos stream) 0))
	  (3 (setf (sm mode stream) 0))))
      ;; dual-channel-simple-stream
      (setf (sm outpos stream) 0)
      ;; string-simple-stream
      nil)
    (device-clear-output stream)))

(defun %write-byte (stream integer)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (simple-stream-dispatch stream
      ;; single-channel-simple-stream
      (with-stream-class (single-channel-simple-stream stream)
	(let ((ptr (sm buffpos stream)))
	  (when (>= ptr (sm buf-len stream))
	    (setf ptr (flush-buffer stream t)))
	  (setf (sm buffpos stream) (1+ ptr))
	  (setf (bref (sm buffer stream) ptr) integer)
	  (sc-set-dirty stream)))
      ;; dual-channel-simple-stream
      (with-stream-class (dual-channel-simple-stream stream)
	(let ((ptr (sm outpos stream)))
	  (when (>= ptr (sm max-out-pos stream))
	    (setf ptr (flush-out-buffer stream t)))
	  (setf (sm outpos stream) (1+ ptr))
	  (setf (bref (sm out-buffer stream) ptr) integer)))
      ;; string-simple-stream
      (with-stream-class (string-simple-stream stream)
	(let ((encap (sm output-handle stream)))
	  (unless encap
	    (error 'simple-type-error
		   :datum stream
		   :expected-type 'stream
		   :format-control "Can't write-byte on string streams."
		   :format-arguments '()))
	  (write-byte integer encap))))))

(defun %read-sequence (stream seq start end partial-fill)
  (declare (type simple-stream stream)
	   (type sequence seq)
	   (type lisp::index start)
	   (type (or null lisp::index) end)
	   (type boolean partial-fill))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %read-sequence 0))
    (when (and (not (any-stream-instance-flags stream :dual :string))
               (sc-dirty-p stream))
      (flush-buffer stream t))
    (etypecase seq
      (string
       (funcall-stm-handler j-read-chars (sm melded-stream stream) seq nil
			    start (or end (length seq))
			    (if partial-fill :bnb t)))
      ((or (simple-array (unsigned-byte 8) (*))
	   (simple-array (signed-byte 8) (*)))
       (when (any-stream-instance-flags stream :string)
         (error "Can't read into byte sequence from a string stream."))       
       ;; "read-vector" equivalent, but blocking if partial-fill is NIL
       ;; FIXME: this could be implemented faster via buffer-copy
       (loop with encap = (sm melded-stream stream)
            for index from start below (or end (length seq))
            for byte = (read-byte-internal encap nil nil t)
              then (read-byte-internal encap nil nil partial-fill)
            while byte
            do (setf (bref seq index) byte)
            finally (return index)))
      ;; extend to work on other sequences: repeated read-byte
      )))

(defun %write-sequence (stream seq start end)
  (declare (type simple-stream stream)
	   (type sequence seq)
	   (type lisp::index start)
	   (type (or null lisp::index) end))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (etypecase seq
      (string
       (funcall-stm-handler-2 j-write-chars seq (sm melded-stream stream)
			      start (or end (length seq))))
      ((or (simple-array (unsigned-byte 8) (*))
	   (simple-array (signed-byte 8) (*)))
       ;; "write-vector" equivalent
       (setf (sm charpos stream) nil)
       (simple-stream-dispatch stream
         ;; single-channel-simple-stream
         (with-stream-class (single-channel-simple-stream stream)
            (loop with max-ptr fixnum = (sm buf-len stream)
                  for src-pos fixnum = start then (+ src-pos count)
                  for src-rest fixnum = (- (or end (length seq)) src-pos)
                  while (> src-rest 0) ; FIXME: this is non-ANSI
                  for ptr fixnum = (let ((ptr (sm buffpos stream)))
                                     (if (>= ptr max-ptr)
                                         (flush-buffer stream t)
                                         ptr))
                  for buf-rest fixnum = (- max-ptr ptr)
                  for count fixnum = (min buf-rest src-rest)
                  do (progn (setf (sm mode stream) 1)
                            (setf (sm buffpos stream) (+ ptr count))
                            (buffer-copy seq src-pos (sm buffer stream) ptr count))))
         ;; dual-channel-simple-stream
         (with-stream-class (dual-channel-simple-stream stream)
            (loop with max-ptr fixnum = (sm max-out-pos stream)
                  for src-pos fixnum = start then (+ src-pos count)
                  for src-rest fixnum = (- (or end (length seq)) src-pos)
                  while (> src-rest 0) ; FIXME: this is non-ANSI
                  for ptr fixnum = (let ((ptr (sm outpos stream)))
                                     (if (>= ptr max-ptr)
                                         (flush-out-buffer stream t)
                                         ptr))
                  for buf-rest fixnum = (- max-ptr ptr)
                  for count fixnum = (min buf-rest src-rest)
                  do (progn (setf (sm outpos stream) (+ ptr count))
                            (buffer-copy seq src-pos (sm out-buffer stream) ptr count))))
         ;; string-simple-stream
         (error 'simple-type-error
                 :datum stream
                 :expected-type 'stream
                 :format-control "Can't write a byte sequence to a string stream."
                 :format-arguments '()))
       )
      ;; extend to work on other sequences: repeated write-byte
      ))
  seq)

(defun read-no-hang-p (stream)
  (declare (type (or integer stream) stream))
  (etypecase stream
    (integer (sys:wait-until-fd-usable stream :input 0))
    (simple-stream (%check stream :input)
                   (when (any-stream-instance-flags stream :eof)
                     (return-from read-no-hang-p nil))
                   (simple-stream-dispatch stream
                     ;; single-channel-simple-stream
                     nil
                     ;; dual-channel-simple-stream
                     ;; if record-end is -1, call device-finish-record
                     ;; return NIL if it returns NIL, T if it returns :EOF
                     nil
                     ;; string-simple-stream
                     nil))
    (stream (listen stream))))

(defun write-no-hang-p (stream)
  (declare (type (or integer stream) stream))
  (etypecase stream
    (integer (sys:wait-until-fd-usable stream :output 0))
    (simple-stream #| ... |#)
    (stream #| ... |#)))

(defun read-vector (vector stream &key (start 0) end (endian-swap :byte-8))
  (declare (type (kernel:simple-unboxed-array (*)) vector)
	   (type stream stream))
  ;; START and END are octet offsets, not vector indices!  [Except for strings]
  ;; Return value is index of next octet to be read into (i.e., start+count)
  (etypecase stream
    (simple-stream
     (with-stream-class (simple-stream stream)
       (if (stringp vector)
	   (let* ((start (or start 0))
		  (end (or end (length vector)))
		  (encap (sm melded-stream stream))
		  (char (funcall-stm-handler j-read-char encap nil nil t)))
	     (when char
	       (setf (schar vector start) char)
	       (incf start)
	       (+ start (funcall-stm-handler j-read-chars encap vector nil
					     start end nil))))
	   (do* ((j-read-byte (if (any-stream-instance-flags stream :string)
				  (error "Can't READ-BYTE on string streams.")
				  #'read-byte-internal))
		 (encap (sm melded-stream stream))
		 (index (or start 0) (1+ index))
		 (end (or end (* (length vector) (vector-elt-width vector))))
		 (endian-swap (endian-swap-value vector endian-swap))
		 (flag t nil))
		((>= index end) index)
	     (let ((byte (funcall j-read-byte encap nil nil flag)))
	       (unless byte (return index))
	       (setf (bref vector (logxor index endian-swap)) byte))))))

    ((or lisp-stream fundamental-stream)
     (unless (typep vector '(or string
			     (simple-array (signed-byte 8) (*))
			     (simple-array (unsigned-byte 8) (*))))
       (error "Wrong vector type for read-vector on stream not of type simple-stream."))
     (read-sequence vector stream :start (or start 0) :end end))))

