;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/charmacs.lisp,v 1.4 1994/10/31 04:50:12 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Implementation specific character-hacking macros and constants.
;;;
(in-package "HEMLOCK-INTERNALS")
(export ' (syntax-char-code-limit search-char-code-limit do-alpha-chars))

;;; This file contains various constants and macros which are implementation or
;;; ASCII dependant.  It contains some versions of CHAR-CODE which do not check
;;; types and ignore the top bit so that various structures can be allocated
;;; 128 long instead of 256, and we don't get errors if a loser visits a binary
;;; file.
;;;
;;; There are so many different constants and macros implemented the same.
;;; This is to separate various mechanisms; for example, in principle the
;;; char-code-limit for the syntax functions is independant of that for the
;;; searching functions
;;;



;;;; Stuff for the Syntax table functions (syntax)

(defconstant syntax-char-code-limit char-code-limit
  "The highest char-code which a character argument to the syntax
  table functions may have.")

(defmacro syntax-char-code (char)
  `(char-code ,char))


;;;; Stuff used by the searching primitives (search)
;;;
(defconstant search-char-code-limit 128
  "The exclusive upper bound on significant char-codes for searching.")
(defmacro search-char-code (ch)
  `(logand (char-code ,ch) #x+7F))
;;;
;;;    search-hash-code must be a function with the following properties:
;;; given any character it returns a number between 0 and 
;;; search-char-code-limit, and the same hash code must be returned 
;;; for the upper and lower case forms of each character.
;;;    In ASCII this is can be done by ANDing out the 5'th bit.
;;;
(defmacro search-hash-code (ch)
  `(logand (char-code ,ch) #x+5F))

;;; Doesn't do anything special, but it should fast and not waste any time
;;; checking type and whatnot.
(defmacro search-char-upcase (ch)
  `(char-upcase (the base-char ,ch)))



;;;; DO-ALPHA-CHARS.

;;; ALPHA-CHARS-LOOP loops from start-char through end-char binding var
;;; to the alphabetic characters and executing body.  Note that the manual
;;; guarantees lower and upper case char codes to be separately in order,
;;; but other characters may be interspersed within that ordering.
(defmacro alpha-chars-loop (var start-char end-char result body)
  (let ((n (gensym))
	(end-char-code (gensym)))
    `(do ((,n (char-code ,start-char) (1+ ,n))
	  (,end-char-code (char-code ,end-char)))
	 ((> ,n ,end-char-code) ,result)
       (let ((,var (code-char ,n)))
	 (when (alpha-char-p ,var)
	   ,@body)))))

(defmacro do-alpha-chars ((var kind &optional result) &rest forms)
  "(do-alpha-chars (var kind [result]) . body).  Kind is one of
   :lower, :upper, or :both, and var is bound to each character in
   order as specified under character relations in the manual.  When
   :both is specified, lowercase letters are processed first."
  (case kind
    (:both
     `(progn (alpha-chars-loop ,var #\a #\z nil ,forms)
	     (alpha-chars-loop ,var #\A #\Z ,result ,forms)))
    (:lower
     `(alpha-chars-loop ,var #\a #\z ,result ,forms))
    (:upper
     `(alpha-chars-loop ,var #\A #\Z ,result ,forms))
    (t (error "Kind argument not one of :lower, :upper, or :both -- ~S."
	      kind))))
