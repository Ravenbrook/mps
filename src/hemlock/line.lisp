;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/line.lisp,v 1.4 2001/03/13 15:49:54 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains definitions for the Line structure, and some 
;;; functions and macros to manipulate them.
;;;
;;;    This stuff was allowed to become implementation dependant because
;;; you make thousands of lines, so speed is real important.  In some
;;; implementations (the Perq for example) it may be desirable to 
;;; not actually cons the strings in the line objects until someone
;;; touches them, and just keep a pointer in the line to where the file 
;;; is mapped in memory.  Such lines are called "buffered".  This stuff
;;; links up with the file-reading stuff and the line-image building stuff.
;;;
(in-package "HEMLOCK-INTERNALS")
(export '(line linep line-previous line-next line-plist line-signature))

(setf (documentation 'linep 'function)
  "Returns true if its argument is a Hemlock line object, Nil otherwise.")
(setf (documentation 'line-previous 'function)
  "Return the Hemlock line that precedes this one, or Nil if there is no
  previous line.")
(setf (documentation 'line-next 'function)
  "Return the Hemlock line that follows this one, or Nil if there is no
  next line.")
(setf (documentation 'line-plist 'function)
  "Return a line's property list.  This may be manipulated with Setf and Getf.")


;;;; The line object:

(declaim (inline %make-line))
(defstruct (line (:print-function %print-hline)
		 (:constructor %make-line)
		 (:predicate linep))
  "A Hemlock line object.  See Hemlock design document for details."
  ;;
  ;; Something that represents the contents of the line.  This is
  ;; guaranteed to change (as compared by EQL) whenver the contents of the
  ;; line changes, but might at arbitarary other times.  There are
  ;; currently about three different cases:
  ;;
  ;; Normal:
  ;;    A simple string holding the contents of the line.
  ;;
  ;; A cached line:
  ;;    The line is eq to Open-Line, and the actual contents are in the
  ;;    line cache.  The %Chars may be either the original contents or a
  ;;    negative fixnum.
  ;;
  ;; A buffered line:
  ;;    The line hasn't been touched since it was read from a file, and the
  ;;    actual contents are in some system I/O area.  This is indicated by
  ;;    the Line-Buffered-P slot being true.  In buffered lines on the RT,
  ;;    the %Chars slot contains the system-area-pointer to the beginning
  ;;    of the characters.
  (%chars "")
  ;;
  ;; Pointers to the next and previous lines in the doubly linked list of
  ;; line structures.
  previous
  next
  ;;
  ;; A list of all the permanent marks pointing into this line.
  (marks ())
  ;;
  ;; The buffer to which this line belongs, or a *disembodied-buffer-count*
  ;; if the line is not in any buffer.
  %buffer
  ;;
  ;; A non-negative integer (fixnum) that represents the ordering of lines
  ;; within continguous range of lines (a buffer or disembuffered region).
  ;; The number of the Line-Next is guaranteed to be strictly greater than
  ;; our number, and the Line-Previous is guaranteed to be strictly less.
  (number 0)
  ;;
  ;; The line property list, used by user code to annotate the text.
  plist
  ;;
  ;; A slot that indicates whether this line is a buffered line, and if so
  ;; contains information about how the text is stored.  On the RT, this is
  ;; the length of the text pointed to by the Line-%Chars.
  #+Buffered-Lines
  (buffered-p ()))

;;; Make Line-Chars the same as Line-%Chars on implementations without
;;; buffered lines.
;;;
#-Buffered-Lines
(defmacro line-chars (x)
  `(line-%chars ,x))


;;; If buffered lines are supported, then we create the string
;;; representation for the characters when someone uses Line-Chars.  People
;;; who are prepared to handle buffered lines or who just want a signature
;;; for the contents can use Line-%chars directly.
;;;
#+Buffered-Lines
(defmacro line-chars (line)
  `(the simple-string (if (line-buffered-p ,line)
			  (read-buffered-line ,line)
			  (line-%chars ,line))))
;;;
#+Buffered-Lines
(defsetf line-chars %set-line-chars)
;;;
#+Buffered-Lines
(defmacro %set-line-chars (line chars)
  `(setf (line-%chars ,line) ,chars))


;;; Line-Signature  --  Public
;;;
;;;    We can just return the Line-%Chars.
;;;
(declaim (inline line-signature))
(defun line-signature (line)
  "This function returns an object which serves as a signature for a line's
  contents.  It is guaranteed that any modification of text on the line will
  result in the signature changing so that it is not EQL to any previous value.
  Note that the signature may change even when the text hasn't been modified, but
  this probably won't happen often."
  (line-%chars line))


;;; Return a copy of Line in buffer Buffer with the same chars.  We use
;;; this macro where we want to copy a line because it takes care of
;;; the case where the line is buffered.
;;;
(defmacro %copy-line (line &key previous number %buffer)
  `(make-line :chars (line-%chars ,line)
	      :previous ,previous
	      :number ,number
	      :%buffer ,%buffer
	      #+Buffered-Lines :buffered-p
	      #+Buffered-Lines (line-buffered-p ,line)))

;;; Hide the fact that the slot isn't really called CHARS.
;;;
(defmacro make-line (&rest keys)
  `(%make-line ,@(substitute :%chars :chars keys)))

(defmacro line-length* (line)
  "Returns the number of characters on the line, but it's a macro!"
  `(cond ((eq ,line open-line)
	  (+ left-open-pos (- line-cache-length right-open-pos)))
	 ((line-buffered-p ,line))
	 (t
	  (length (the simple-string (line-%chars ,line))))))
