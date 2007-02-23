;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/streams.lisp,v 1.5 1998/05/04 01:27:20 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains definitions of various types of streams used
;;; in Hemlock.  They are implementation dependant, but should be
;;; portable to all implementations based on Spice Lisp with little
;;; difficulty.
;;;
;;; Written by Skef Wholey and Rob MacLachlan.
;;;

(in-package "HEMLOCK-INTERNALS")

(export '(make-hemlock-output-stream
	  hemlock-region-stream hemlock-region-stream-p
	  hemlock-output-stream make-hemlock-region-stream
	  hemlock-output-stream-p make-kbdmac-stream
	  modify-kbdmac-stream))

(defstruct (hemlock-output-stream
	    (:include sys:lisp-stream
		      (:misc #'hemlock-output-misc))
	    (:print-function %print-hemlock-output-stream)
	    (:constructor internal-make-hemlock-output-stream ()))
  ;;
  ;; The mark we insert at.
  mark)

(defun %print-hemlock-output-stream (s stream d)
  (declare (ignore d s))
  (write-string "#<Hemlock output stream>" stream))

(defun make-hemlock-output-stream (mark &optional (buffered :line))
  "Returns an output stream whose output will be inserted at the Mark.
  Buffered, which indicates to what extent the stream may be buffered
  is one of the following:
   :None  -- The screen is brought up to date after each stream operation.
   :Line  -- The screen is brought up to date when a newline is written.
   :Full  -- The screen is not updated except explicitly via Force-Output."
  (modify-hemlock-output-stream (internal-make-hemlock-output-stream) mark
                                buffered))


(defun modify-hemlock-output-stream (stream mark buffered)
  (unless (and (markp mark)
	       (memq (mark-kind mark) '(:right-inserting :left-inserting)))
    (error "~S is not a permanent mark." mark))
  (setf (hemlock-output-stream-mark stream) mark)
  (case buffered
    (:none
     (setf (lisp::lisp-stream-out stream) #'hemlock-output-unbuffered-out
	   (lisp::lisp-stream-sout stream) #'hemlock-output-unbuffered-sout))
    (:line
     (setf (lisp::lisp-stream-out stream) #'hemlock-output-line-buffered-out
	   (lisp::lisp-stream-sout stream) #'hemlock-output-line-buffered-sout))
    (:full
     (setf (lisp::lisp-stream-out stream) #'hemlock-output-buffered-out
	   (lisp::lisp-stream-sout stream) #'hemlock-output-buffered-sout))
    (t
     (error "~S is a losing value for Buffered." buffered)))
  stream)

(defmacro with-left-inserting-mark ((var form) &body forms)
  (let ((change (gensym)))
    `(let* ((,var ,form)
	    (,change (eq (mark-kind ,var) :right-inserting)))
       (unwind-protect
	   (progn
	     (when ,change
	       (setf (mark-kind ,var) :left-inserting))
	     ,@forms)
	 (when ,change
	   (setf (mark-kind ,var) :right-inserting))))))

(defun hemlock-output-unbuffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)
    (redisplay-windows-from-mark mark)))

(defun hemlock-output-unbuffered-sout (stream string start end)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)
    (redisplay-windows-from-mark mark)))

(defun hemlock-output-buffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)))

(defun hemlock-output-buffered-sout (stream string start end)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)))

(defun hemlock-output-line-buffered-out (stream character)
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-character mark character)
    (when (char= character #\newline)
      (redisplay-windows-from-mark mark))))

(defun hemlock-output-line-buffered-sout (stream string start end)
  (declare (simple-string string))
  (with-left-inserting-mark (mark (hemlock-output-stream-mark stream))
    (insert-string mark string start end)
    (when (find #\newline string :start start :end end)
      (redisplay-windows-from-mark mark))))

(defun hemlock-output-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos (mark-charpos (hemlock-output-stream-mark stream)))
    (:line-length
     (let* ((buffer (line-buffer (mark-line (hemlock-output-stream-mark stream)))))
       (when buffer
	 (do ((w (buffer-windows buffer) (cdr w))
	      (min most-positive-fixnum (min (window-width (car w)) min)))
	     ((null w)
	      (if (/= min most-positive-fixnum) min))))))
    ((:finish-output :force-output)
     (redisplay-windows-from-mark (hemlock-output-stream-mark stream)))
    (:close (setf (hemlock-output-stream-mark stream) nil))
    (:element-type 'base-char)))

(defstruct (hemlock-region-stream
	    (:include sys:lisp-stream
		      (:in #'region-in)
		      (:misc #'region-misc))
	    (:print-function %print-region-stream)
	    (:constructor internal-make-hemlock-region-stream (region mark)))
  ;;
  ;; The region we read from.
  region
  ;;
  ;; The mark pointing to the next character to read.
  mark)

(defun %print-region-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<Hemlock region stream>" stream))

(defun make-hemlock-region-stream (region)
  "Returns an input stream that will return successive characters from the
  given Region when asked for input."
  (internal-make-hemlock-region-stream
   region (copy-mark (region-start region) :right-inserting)))

(defun modify-hemlock-region-stream (stream region)
  (setf (hemlock-region-stream-region stream) region)
  (let* ((mark (hemlock-region-stream-mark stream))
	 (start (region-start region))
	 (start-line (mark-line start)))
    ;; Make sure it's dead.
    (delete-mark mark)
    (setf (mark-line mark) start-line  (mark-charpos mark) (mark-charpos start))
    (push mark (line-marks start-line)))
  stream)

(defun region-in (stream eof-errorp eof-value)
  (let ((mark (hemlock-region-stream-mark stream)))
    (cond ((mark< mark
		  (region-end (hemlock-region-stream-region stream)))
	   (prog1 (next-character mark) (mark-after mark)))
	  (eof-errorp (error "~A hit end of file." stream)) 
	  (t eof-value))))

(defun region-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:listen (mark< (hemlock-region-stream-mark stream)
		    (region-end (hemlock-region-stream-region stream))))
    (:clear-input (move-mark
                   (hemlock-region-stream-mark stream)
                   (region-end (hemlock-region-stream-region stream))))
    (:unread
     (let ((mark (hemlock-region-stream-mark stream)))
       (unless (mark> mark
		      (region-start (hemlock-region-stream-region stream)))
	 (error "Nothing to unread."))
       (unless (char= arg1 (previous-character mark))
	 (error "Unreading something not read: ~S" arg1))
       (mark-before mark)))
    (:file-position
     (let ((start (region-start (hemlock-region-stream-region stream)))
	   (mark (hemlock-region-stream-mark stream)))
       (cond (arg1
	      (move-mark mark start)
	      (character-offset mark arg1))
	     (t
	      (count-characters (region start mark))))))
    (:close
     (delete-mark (hemlock-region-stream-mark stream))
     (setf (hemlock-region-stream-region stream) nil))
    (:element-type 'base-char)))

;;;; Stuff to support keyboard macros.

(defstruct (kbdmac-stream
	    (:include editor-input
		      (:get #'kbdmac-get)
		      (:unget #'kbdmac-unget)
		      (:listen #'kbdmac-listen))
	    (:constructor make-kbdmac-stream ()))
  buffer    ; The simple-vector that holds the characters.
  index)    ; Index of the next character.

(defun kbdmac-get (stream ignore-abort-attempts-p)
  (declare (ignore ignore-abort-attempts-p))
  (let ((index (kbdmac-stream-index stream)))
    (setf (kbdmac-stream-index stream) (1+ index))
    (setq *last-key-event-typed*
	  (svref (kbdmac-stream-buffer stream) index))))

(defun kbdmac-unget (ignore stream)
  (declare (ignore ignore))
  (if (plusp (kbdmac-stream-index stream))
      (decf (kbdmac-stream-index stream))
      (error "Nothing to unread.")))

(defun kbdmac-listen (stream)
  (declare (ignore stream))
  t)

;;; MODIFY-KBDMAC-STREAM  --  Internal
;;;
;;;    Bash the kbdmac-stream Stream so that it will return the Input.
;;;
(defun modify-kbdmac-stream (stream input)
  (setf (kbdmac-stream-index stream) 0)
  (setf (kbdmac-stream-buffer stream) input)
  stream)
