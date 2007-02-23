;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/htext2.lisp,v 1.4 2004/03/28 15:48:11 emarsden Exp $")
;;;
;;; **********************************************************************
;;;
;;; More Hemlock Text-Manipulation functions.
;;; Written by Skef Wholey.
;;;
;;; The code in this file implements the non-insert/delete functions in the
;;; "Doing Stuff and Going Places" chapter of the Hemlock Design document.
;;;

(in-package "HEMLOCK-INTERNALS")

(export '(region-to-string string-to-region line-to-region
			   previous-character next-character count-lines
			   count-characters line-start line-end buffer-start
			   buffer-end move-mark mark-before mark-after
			   character-offset line-offset region-bounds
			   set-region-bounds *print-region*))



(defun region-to-string (region)
  "Returns a string containing the characters in the given Region."
  (close-line)
  (let* ((dst-length (count-characters region))
	 (string (make-string dst-length))
	 (start-mark (region-start region))
	 (end-mark (region-end region))
	 (start-line (mark-line start-mark))
	 (end-line (mark-line end-mark))
	 (start-charpos (mark-charpos start-mark)))
    (declare (simple-string string))
    (if (eq start-line end-line)
	(%sp-byte-blt (line-chars start-line) start-charpos string 0
		      dst-length)
	(let ((index ()))
	  (let* ((line-chars (line-chars start-line))
		 (dst-end (- (length line-chars) start-charpos)))
	    (declare (simple-string line-chars))
	    (%sp-byte-blt line-chars start-charpos string 0 dst-end)
	    (setf (char string dst-end) #\newline)
	    (setq index (1+ dst-end)))
	  (do* ((line (line-next start-line) (line-next line))
		(chars (line-chars line) (line-chars line)))
	       ((eq line end-line)
		(%sp-byte-blt (line-chars line) 0 string index dst-length))
	    (declare (simple-string chars))
	    (%sp-byte-blt (line-chars line) 0 string index
			  (incf index (length chars)))
	    (setf (char string index) #\newline)
	    (setq index (1+ index)))))
    string))

(defun string-to-region (string)
  "Returns a region containing the characters in the given String."
  (let* ((string (if (simple-string-p string)
		     string (coerce string 'simple-string)))
	 (end (length string)))
    (declare (simple-string string))
    (do* ((index 0)
	  (buffer (incf *disembodied-buffer-counter*))
	  (previous-line)
	  (line (make-line :%buffer buffer))
	  (first-line line))
	 (())
      (let ((right-index (%sp-find-character string index end #\newline)))
	(cond (right-index
	       (let* ((length (- right-index index))
		      (chars (make-string length)))
		 (%sp-byte-blt string index chars 0 length)
		 (setf (line-chars line) chars))
	       (setq index (1+ right-index))
	       (setq previous-line line)
	       (setq line (make-line :%buffer buffer))
	       (setf (line-next previous-line) line)
	       (setf (line-previous line) previous-line))
	      (t
	       (let* ((length (- end index))
		      (chars (make-string length)))
		 (%sp-byte-blt string index chars 0 length)
		 (setf (line-chars line) chars))
	       (return (renumber-region
			(internal-make-region
			 (mark first-line 0 :right-inserting)
			 (mark line (length (line-chars line))
			       :left-inserting))))))))))

(defun line-to-region (line)
  "Returns a region containing the specified line."
  (internal-make-region (mark line 0 :right-inserting)
			(mark line (line-length* line) :left-inserting)))

(defun previous-character (mark)
  "Returns the character immediately before the given Mark."
  (let ((line (mark-line mark))
	(charpos (mark-charpos mark)))
    (if (= charpos 0)
	(if (line-previous line)
	    #\newline
	    nil)
	(if (eq line open-line)
	    (char (the simple-string open-chars)
		  (if (<= charpos left-open-pos)
		      (1- charpos)
		      (1- (+ right-open-pos (- charpos left-open-pos)))))
	    (schar (line-chars line) (1- charpos))))))

(defun next-character (mark)
  "Returns the character immediately after the given Mark."
  (let ((line (mark-line mark))
	(charpos (mark-charpos mark)))
    (if (eq line open-line)
	(if (= charpos (- line-cache-length (- right-open-pos left-open-pos)))
	    (if (line-next line)
		#\newline
		nil)
	    (schar open-chars
		   (if (< charpos left-open-pos)
		       charpos
		       (+ right-open-pos (- charpos left-open-pos)))))
	(let ((chars (line-chars line)))
	  (if (= charpos (strlen chars))
	      (if (line-next line)
		  #\newline
		  nil)
	      (schar chars charpos))))))

;;; %Set-Next-Character  --  Internal
;;;
;;;    This is the setf form for Next-Character.  Since we may change a
;;; character to or from a newline, we must be prepared to split and
;;; join lines.  We cannot just delete  a character and insert the new one
;;; because the marks would not be right.
;;;
(defun %set-next-character (mark character)
  (let* ((line (mark-line mark))
	 (buffer (line-%buffer line))
	 (next (line-next line)))
    (modifying-buffer buffer
      (modifying-line line mark)
      (cond ((= right-open-pos line-cache-length)
	     ;; The mark is at the end of the line.
	     (unless next
	       (error "~S has no next character, so it cannot be set." mark))
	     (unless (char= character #\newline)
	       ;; If the character is no longer a newline then mash two
	       ;; lines together.
	       (let ((chars (line-chars next)))
		 (declare (simple-string chars))
		 (setq right-open-pos (- line-cache-length (length chars)))
		 (when (<= right-open-pos left-open-pos)
		   (grow-open-chars (* (+ (length chars) left-open-pos 1) 2)))
		 (%sp-byte-blt chars 0 open-chars right-open-pos 
			       line-cache-length)
		 (setf (schar open-chars left-open-pos) character)
		 (incf left-open-pos))
	       (move-some-marks (charpos next line) 
				(+ charpos left-open-pos))
	       (setq next (line-next next))
	       (setf (line-next line) next)
	       (when next (setf (line-previous next) line))))
	    ((char= character #\newline)
	     ;; The char is being changed to a newline, so we must split lines.
	     (incf right-open-pos)
	     (let* ((len (- line-cache-length right-open-pos))	   
		    (chars (make-string len))
		    (new (make-line :chars chars  :previous line 
				    :next next  :%buffer buffer)))
	       (%sp-byte-blt open-chars right-open-pos chars 0 len)
	       (maybe-move-some-marks* (charpos line new) left-open-pos
				       (- charpos left-open-pos 1))
	       (setf (line-next line) new)
	       (when next (setf (line-previous next) new))
	       (setq right-open-pos line-cache-length)
	       (number-line new)))
	    (t
	     (setf (char (the simple-string open-chars) right-open-pos)
		   character)))))
  character)

;;; %Set-Previous-Character  --  Internal
;;;
;;;    The setf form for Previous-Character.  We just Temporarily move the
;;; mark back one and call %Set-Next-Character.
;;;
(defun %set-previous-character (mark character)
  (unless (mark-before mark)
    (error "~S has no previous character, so it cannot be set." mark))
  (%set-next-character mark character)
  (mark-after mark)
  character)

(defun count-lines (region)
  "Returns the number of lines in the region, first and last lines inclusive."
  (do ((line (mark-line (region-start region)) (line-next line))
       (count 1 (1+ count))
       (last-line (mark-line (region-end region))))
      ((eq line last-line) count)))

(defun count-characters (region)
  "Returns the number of characters in the region."
  (let* ((start (region-start region))
	 (end (region-end region))
	 (first-line (mark-line start))
	 (last-line (mark-line end)))
    (if (eq first-line last-line)
	(- (mark-charpos end) (mark-charpos start))
	(do ((line (line-next first-line) (line-next line))
	     (count (1+ (- (line-length* first-line) (mark-charpos start)))))
	    ((eq line last-line)
	     (+ count (mark-charpos end)))
	  (setq count (+ 1 count (line-length* line)))))))

(defun line-start (mark &optional line)
  "Changes the Mark to point to the beginning of the Line and returns it.
  Line defaults to the line Mark is on."
  (when line
    (change-line mark line))
  (setf (mark-charpos mark) 0)
  mark)

(defun line-end (mark &optional line)
  "Changes the Mark to point to the end of the line and returns it.
  Line defaults to the line Mark is on."
  (if line
      (change-line mark line)
      (setq line (mark-line mark)))
  (setf (mark-charpos mark) (line-length* line))
  mark)

(defun buffer-start (mark &optional (buffer (line-buffer (mark-line mark))))
  "Change Mark to point to the beginning of Buffer, which defaults to
  the buffer Mark is currently in."
  (unless buffer (error "Mark ~S does not point into a buffer." mark))
  (move-mark mark (buffer-start-mark buffer)))

(defun buffer-end (mark &optional (buffer (line-buffer (mark-line mark))))
  "Change Mark to point to the end of Buffer, which defaults to
  the buffer Mark is currently in."
  (unless buffer (error "Mark ~S does not point into a buffer." mark))
  (move-mark mark (buffer-end-mark buffer)))

(defun move-mark (mark new-position)
  "Changes the Mark to point to the same position as New-Position."
  (let ((line (mark-line new-position)))
    (change-line mark line))
  (setf (mark-charpos mark) (mark-charpos new-position))
  mark)

(defun mark-before (mark)
  "Changes the Mark to point one character before where it currently points.
  NIL is returned if there is no previous character."
  (let ((charpos (mark-charpos mark)))
    (cond ((zerop charpos)
	   (let ((prev (line-previous (mark-line mark))))
	     (when prev
	       (always-change-line mark prev)
	       (setf (mark-charpos mark) (line-length* prev))
	       mark)))
	  (t
	   (setf (mark-charpos mark) (1- charpos))
	   mark))))

(defun mark-after (mark)
  "Changes the Mark to point one character after where it currently points.
  NIL is returned if there is no previous character."
  (let ((line (mark-line mark))
	(charpos (mark-charpos mark)))
    (cond ((= charpos (line-length* line))
	   (let ((next (line-next line)))
	     (when next
	       (always-change-line mark next)
	       (setf (mark-charpos mark) 0)
	       mark)))
	  (t
	   (setf (mark-charpos mark) (1+ charpos))
	   mark))))

(defun character-offset (mark n)
  "Changes the Mark to point N characters after (or -N before if N is negative)
  where it currently points.  If there aren't N characters before (or after)
  the mark, Nil is returned."
  (let ((charpos (mark-charpos mark)))
    (if (< n 0)
	(let ((n (- n)))
	  (if (< charpos n)
	      (do ((line (line-previous (mark-line mark)) (line-previous line))
		   (n (- n charpos 1)))
		  ((null line) nil)
		(let ((length (line-length* line)))
		  (cond ((<= n length)
			 (always-change-line mark line)
			 (setf (mark-charpos mark) (- length n))
			 (return mark))
			(t
			 (setq n (- n (1+ length)))))))
	      (progn (setf (mark-charpos mark) (- charpos n))
		     mark)))
	(let* ((line (mark-line mark))
	       (length (line-length* line)))
	  (if (> (+ charpos n) length)
	      (do ((line (line-next line) (line-next line))
		   (n (- n (1+ (- length charpos)))))
		  ((null line) nil)
		(let ((length (line-length* line)))
		  (cond ((<= n length)
			 (always-change-line mark line)
			 (setf (mark-charpos mark) n)
			 (return mark))
			(t
			 (setq n (- n (1+ length)))))))
	      (progn (setf (mark-charpos mark) (+ charpos n))
		     mark))))))

(defun line-offset (mark n &optional charpos)
  "Changes to Mark to point N lines after (-N before if N is negative) where
  it currently points.  If there aren't N lines after (or before) the Mark,
  Nil is returned."
  (if (< n 0)
      (do ((line (mark-line mark) (line-previous line))
	   (n n (1+ n)))
	  ((null line) nil)
	(when (= n 0)
	  (always-change-line mark line)
	  (setf (mark-charpos mark)
		(if charpos
		    (min (line-length line) charpos)
		    (min (line-length line) (mark-charpos mark))))
	  (return mark)))
      (do ((line (mark-line mark) (line-next line))
	   (n n (1- n)))
	  ((null line) nil)
	(when (= n 0)
	  (change-line mark line)
	  (setf (mark-charpos mark)
		(if charpos
		    (min (line-length line) charpos)
		    (min (line-length line) (mark-charpos mark))))
	  (return mark)))))

;;; region-bounds  --  Public
;;;
(defun region-bounds (region)
  "Return as multiple-value the start and end of Region."
  (values (region-start region) (region-end region)))

(defun set-region-bounds (region start end)
  "Set the start and end of Region to the marks Start and End."
  (let ((sl (mark-line start))
	(el (mark-line end)))
    (when (or (neq (line-%buffer sl) (line-%buffer el))
	      (> (line-number sl) (line-number el))
	      (and (eq sl el) (> (mark-charpos start) (mark-charpos end))))
      (error "Marks ~S and ~S cannot be made into a region." start end))
    (setf (region-start region) start  (region-end region) end))
  region)


;;;; Debugging stuff.

(defun slf (string)
  "For a good time, figure out what this function does, and why it was written."
  (delete #\linefeed (the simple-string string)))

(defun %print-whole-line (structure stream)
  (cond ((eq structure open-line)
	 (write-string open-chars stream :end left-open-pos)
	 (write-string open-chars stream :start right-open-pos
		       :end line-cache-length))
	(t
	 (write-string (line-chars structure) stream))))

(defun %print-before-mark (mark stream)
  (if (mark-line mark)
      (let* ((line (mark-line mark))
	     (chars (line-chars line))
	     (charpos (mark-charpos mark))
	     (length (line-length line)))
	(declare (simple-string chars))
	(cond ((or (> charpos length) (< charpos 0))
	       (write-string "{bad mark}" stream))
	      ((eq line open-line)
	       (cond ((< charpos left-open-pos)
		      (write-string open-chars stream :end charpos))
		     (t
		      (write-string open-chars stream :end left-open-pos)
		      (let ((p (+ charpos (- right-open-pos left-open-pos))))
			(write-string open-chars stream  :start right-open-pos
				      :end p)))))
	      (t
	       (write-string chars stream :end charpos))))
      (write-string "{deleted mark}" stream)))


(defun %print-after-mark (mark stream)
  (if (mark-line mark)
      (let* ((line (mark-line mark))
	     (chars (line-chars line))
	     (charpos (mark-charpos mark))
	     (length (line-length line)))
	(declare (simple-string chars))
	(cond ((or (> charpos length) (< charpos 0))
	       (write-string "{bad mark}" stream))
	      ((eq line open-line)
	       (cond ((< charpos left-open-pos)
		      (write-string open-chars stream  :start charpos
				    :end left-open-pos)
		      (write-string open-chars stream  :start right-open-pos
				    :end line-cache-length))
		     (t
		      (let ((p (+ charpos (- right-open-pos left-open-pos))))
			(write-string open-chars stream :start p
				      :end line-cache-length)))))
	      (t
	       (write-string chars stream  :start charpos  :end length))))
      (write-string "{deleted mark}" stream)))

(defun %print-hline (structure stream d)
  (declare (ignore d))
  (write-string "#<Hemlock Line \"" stream)
  (%print-whole-line structure stream)
  (write-string "\">" stream))

(defun %print-hmark (structure stream d)
  (declare (ignore d))
  (write-string "#<Hemlock Mark \"" stream)
  (%print-before-mark structure stream)
  (write-string "/\\" stream)
  (%print-after-mark structure stream)
  (write-string "\">" stream))  

(defvar *print-region* 10
  "The number of lines to print out of a region, or NIL if none.")

(defun %print-hregion (region stream d)
  (declare (ignore d))
  (write-string "#<Hemlock Region \"" stream)
  (let* ((start (region-start region))
	 (end (region-end region))
	 (first-line (mark-line start))
	 (last-line (mark-line end)))
    (cond
     ((not (and (linep first-line) (linep last-line)
		(eq (line-%buffer first-line) (line-%buffer last-line))
		(mark<= start end)))
      (write-string "{bad region}" stream))
     (*print-region*
      (cond ((eq first-line last-line)
	     (let ((cs (mark-charpos start))
		   (ce (mark-charpos end))
		   (len (line-length first-line)))
	       (cond
		((or (< cs 0) (> ce len))
		 (write-string "{bad region}" stream))
		((eq first-line open-line)
		 (let ((gap (- right-open-pos left-open-pos)))
		   (cond
		    ((<= ce left-open-pos)
		     (write-string open-chars stream  :start cs  :end ce))
		    ((>= cs left-open-pos)
		     (write-string open-chars stream  :start (+ cs gap)
				   :end (+ ce gap)))
		    (t
		     (write-string open-chars stream :start cs
				   :end left-open-pos)
		     (write-string open-chars stream :start right-open-pos
				   :end (+ gap ce))))))
		(t
		 (write-string (line-chars first-line) stream  :start cs
			       :end ce)))))
	    (t
	     (%print-after-mark start stream)
	     (write-char #\/ stream)
	     (do ((line (line-next first-line) (line-next line))
		  (last-line (mark-line end))
		  (cnt *print-region* (1- cnt)))
		 ((or (eq line last-line)
		      (when (zerop cnt) (write-string "..." stream) t))
		  (%print-before-mark end stream))
	       (%print-whole-line line stream)
	       (write-char #\/ stream)))))
     (t
      (write-string "{mumble}" stream))))
  (write-string "\">" stream))

(defun %print-hbuffer (structure stream d)
  (declare (ignore d))
  (write-string "#<Hemlock Buffer \"" stream)
  (write-string (buffer-name structure) stream)
  (write-string "\">" stream))
