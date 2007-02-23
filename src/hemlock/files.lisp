;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/files.lisp,v 1.4 1994/10/31 04:50:12 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Hemlock File manipulation functions.
;;; Written by Skef Wholey, Horribly Hacked by Rob MacLachlan.
;;;

(in-package "HEMLOCK-INTERNALS")

(export '(read-file write-file))



;;;; Utility functions.

(defun find-char-from-sap (sap start end char)
  (declare (type system-area-pointer sap)
	   (type (integer 0 (#.most-positive-fixnum)) start end)
	   (type base-char char))
  (do ((index start (1+ index))
       (code (char-code char)))
      ((>= index end) nil)
    (declare (type (integer 0 #.most-positive-fixnum) index)
	     (type (unsigned-byte 8) code))
    (when (= (sap-ref-8 sap index) code)
      (return index))))


;;; Read-File:

(defun read-file (pathname mark)
  "Inserts the contents of the file named by Pathname at the Mark."
  (with-mark ((mark mark :left-inserting))
    (let* ((tn (truename pathname))
	   (name (namestring tn))
	   (sap nil)
	   (size 0))
      (declare (fixnum size)
	       (type (or null system-area-pointer) sap))
      (multiple-value-bind (fd err) (unix:unix-open name unix:o_rdonly 0)
	(when fd
	  (multiple-value-bind (res dev ino mode nlnk uid gid rdev len)
			       (unix:unix-fstat fd)
	    (declare (ignore ino mode nlnk uid gid rdev))
	    (cond ((null res)
		   (setq err dev))
		  (t
		   (setf sap (system:allocate-system-memory len))
		   (setf size len)
		   (multiple-value-bind
		       (bytes err3)
		       (unix:unix-read fd sap len)
		     (if (or (null bytes) (not (= len bytes)))
			 (setq err err3)
			 (setq err nil))))))
	  (unix:unix-close fd))
	(when err
	  (error "Reading file ~A, unix error ~A."
		 name (unix:get-unix-error-msg err)))
	(when (zerop size)
	  (return-from read-file nil))
	(let* ((first-line (mark-line mark))
	       (buffer (line-%buffer first-line))
	       (index (find-char-from-sap sap 0 size #\newline)))
	  (declare (type (or null (integer 0 (#.most-positive-fixnum))) index))
	  (modifying-buffer buffer)
	  (let* ((len (or index size))
		 (chars (make-string len)))
	    (%primitive byte-blt sap 0 chars 0 len)
	    (insert-string mark chars))
	  (when index
	    (insert-character mark #\newline)
	    (do* ((old-index (1+ index) (1+ index))
		  (index (find-char-from-sap sap old-index size #\newline)
			 (find-char-from-sap sap old-index size #\newline))
		  (number (+ (line-number first-line) line-increment)
			  (+ number line-increment))
		  (previous first-line))
		 ((not index)
		  (let* ((length (- size old-index))
			 (chars (make-string length))
			 (line (mark-line mark)))
		    (declare (fixnum length))
		    (%primitive byte-blt sap old-index chars 0 length)
		    (insert-string mark chars)
		    (setf (line-next previous) line)
		    (setf (line-previous line) previous)
		    (do ((line line (line-next line))
			 (number number (+ number line-increment)))
			((null line))
		      (declare (fixnum number))
		      (setf (line-number line) number))))
	      (declare (fixnum number old-index))
	      (let ((line (make-line
			   :previous previous
			   :%buffer buffer
			   :number number
			   :chars (system:sap+ sap old-index)
			   :buffered-p
			   (the fixnum (- (the fixnum index) old-index)))))
		(setf (line-next previous) line)
		(setq previous line))) nil))))))


;;; Hackish stuff for disgusting speed:

(defun read-buffered-line (line)
  (let* ((len (line-buffered-p line))
  	 (chars (make-string len)))
    (%primitive byte-blt (line-%chars line) 0 chars 0 len)
    (setf (line-buffered-p line) nil)
    (setf (line-chars line) chars)))



;;; Write-File:

(defun write-file (region pathname &key append
			  (keep-backup (value ed::keep-backup-files))
			  access)
  "Writes the characters in region to the file named by pathname.  This writes
   region using a stream opened with :if-exists :rename-and-delete, unless
   either append or keep-backup is supplied.  If append is supplied, this
   writes the file opened with :if-exists :append.  If keep-backup is supplied,
   this writes the file opened with :if-exists :rename.  This signals an error
   if both append and keep-backup are supplied.  Access is an implementation
   dependent value that is suitable for setting pathname's access or protection
   bits."
  (let ((if-exists-action (cond ((and keep-backup append)
				 (error "Cannot supply non-nil values for ~
				         both keep-backup and append."))
				(keep-backup :rename)
				(append :append)
				(t :rename-and-delete))))
    (with-open-file (file pathname :direction :output
			  :element-type 'base-char
			  :if-exists if-exists-action)
      (close-line)
      (fast-write-file region file))
    (when access
      (multiple-value-bind
	  (winp code)
	  ;; Must do a TRUENAME in case the file has never been written.
	  ;; It may have Common Lisp syntax that Unix can't handle.
	  ;; If this is ever moved to the beginning of this function to use
	  ;; Unix CREAT to create the file protected initially, they TRUENAME
	  ;; will signal an error, and LISP::PREDICT-NAME will have to be used.
	  (unix:unix-chmod (namestring (truename pathname)) access)
	(unless winp
	  (error "Could not set access code: ~S"
		 (unix:get-unix-error-msg code)))))))

(defun fast-write-file (region file)
  (let* ((start (region-start region))
	 (start-line (mark-line start))
	 (start-charpos (mark-charpos start))
	 (end (region-end region))
	 (end-line (mark-line end))
	 (end-charpos (mark-charpos end)))
    (if (eq start-line end-line)
      (write-string (line-chars start-line) file
		    :start start-charpos :end end-charpos)
      (let* ((first-length (- (line-length start-line) start-charpos))
	     (length (+ first-length end-charpos 1)))
	(do ((line (line-next start-line) (line-next line)))
	    ((eq line end-line))
	  (incf length (1+ (line-length line))))
	(let ((sap (system:allocate-system-memory length)))
	  (unwind-protect
	      (macrolet ((chars (line)
				`(if (line-buffered-p ,line)
				     (line-%chars ,line)
				     (line-chars ,line))))
		(system:%primitive byte-blt
				   (chars start-line) start-charpos
				   sap 0 first-length)
		(setf (system:sap-ref-8 sap first-length)
		      (char-code #\newline))
		(let ((offset (1+ first-length)))
		  (do ((line (line-next start-line)
			     (line-next line)))
		      ((eq line end-line))
		    (let ((end (+ offset (line-length line))))
		      (system:%primitive byte-blt
					 (chars line) 0
					 sap offset end)
		      (setf (system:sap-ref-8 sap end)
			    (char-code #\newline))
		      (setf offset (1+ end))))
		  (unless (zerop end-charpos)
		    (system:%primitive byte-blt
				       (chars end-line) 0
				       sap offset
				       (+ offset end-charpos))))
		(multiple-value-bind
		    (okay errno)
		    (unix:unix-write (system:fd-stream-fd file)
				     sap 0 length)
		  (unless okay
		    (error "Could not write ~S: ~A"
			   file
			   (unix:get-unix-error-msg errno)))))
	    (system:deallocate-system-memory sap length)))))))
