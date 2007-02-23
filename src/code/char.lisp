;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/char.lisp,v 1.15 2004/08/27 12:28:29 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Character functions for Spice Lisp.  Part of the standard Spice Lisp
;;; environment.
;;;
;;; This file assumes the use of ASCII codes and the specific character formats
;;; used in Spice Lisp and Vax Common Lisp.  It is optimized for performance
;;; rather than for portability and elegance, and may have to be rewritten if
;;; the character representation is changed.
;;;
;;; Written by Guy Steele.
;;; Rewritten by David Dill.
;;; Hacked up for speed by Scott Fahlman.
;;; Font support flushed and type hackery rewritten by Rob MacLachlan.
;;;
(in-package "LISP")

(export '(char-code-limit standard-char-p graphic-char-p 
	  alpha-char-p upper-case-p lower-case-p both-case-p digit-char-p
	  alphanumericp char= char/= char< char> char<= char>= char-equal
	  char-not-equal char-lessp char-greaterp char-not-greaterp
	  char-not-lessp character char-code code-char char-upcase
	  char-downcase digit-char char-int char-name name-char))


;;; Compile some trivial character operations via inline expansion:
;;;
(declaim (inline standard-char-p graphic-char-p alpha-char-p
		 upper-case-p lower-case-p both-case-p alphanumericp
		 char-int))

(declaim (maybe-inline digit-char-p digit-weight))

(defconstant char-code-limit 256
  "The upper exclusive bound on values produced by CHAR-CODE.")

(deftype char-code ()
  `(integer 0 (,char-code-limit)))


(macrolet ((frob (char-names-list)
	     (collect ((results))
	       (dolist (code char-names-list)
		 (destructuring-bind (ccode names)
		     code
		   (dolist (name names)
		     (results (cons name (code-char ccode))))))
	       `(defparameter char-name-alist ',(results)
  "This is the alist of (character-name . character) for characters with
  long names.  The first name in this list for a given character is used
  on typeout and is the preferred form for input."))))
  (frob ((#x00 ("Null" "^@" "Nul"))
	 (#x01 ("^A" "Soh"))
	 (#x02 ("^B" "Stx"))
	 (#x03 ("^C" "Etx"))
	 (#x04 ("^D" "Eot"))
	 (#x05 ("^E" "Enq"))
	 (#x06 ("^F" "Ack"))
	 (#x07 ("Bell" "^g" "Bel"))
	 (#x08 ("Backspace" "^h" "Bs"))
	 (#x09 ("Tab" "^i" "Ht"))
	 (#x0A ("Newline" "Linefeed" "^j" "Lf" "Nl" ))
	 (#x0B ("Vt" "^k"))
	 (#x0C ("Page" "^l" "Form" "Formfeed" "Ff" "Np"))
	 (#x0D ("Return" "^m" "Cr"))
	 (#x0E ("^N" "So"))
	 (#x0F ("^O" "Si"))
	 (#x10 ("^P" "Dle"))
	 (#x11 ("^Q" "Dc1"))
	 (#x12 ("^R" "Dc2"))
	 (#x13 ("^S" "Dc3"))
	 (#x14 ("^T" "Dc4"))
	 (#x15 ("^U" "Nak"))
	 (#x16 ("^V" "Syn"))
	 (#x17 ("^W" "Etb"))
	 (#x18 ("^X" "Can"))
	 (#x19 ("^Y" "Em"))
	 (#x1A ("^Z" "Sub"))
	 (#x1B ("Escape" "^[" "Altmode" "Esc" "Alt"))
	 (#x1C ("Fs" "^\\"))
	 (#x1D ("Gs" "^]"))
	 (#x1E ("Rs" "^^"))
	 (#x1F ("Us" "^_"))
	 (#x20 ("Space" "Sp"))
	 (#x7f ("Rubout" "Delete" "Del")))))


;;;; Accessor functions:

(defun char-code (char)
  "Returns the integer code of CHAR."
  (etypecase char
    (base-char (char-code (truly-the base-char char)))))


(defun char-int (char)
  "Returns the integer code of CHAR.  This is the same as char-code, as
   CMU Common Lisp does not implement character bits or fonts."
  (char-code char))


(defun code-char (code)
  "Returns the character with the code CODE."
  (declare (type char-code code))
  (code-char code))


(defun character (object)
  "Coerces its argument into a character object if possible.  Accepts
  characters, strings and symbols of length 1."
  (flet ((do-error (control args)
	   (error 'simple-type-error
		  :datum object
		  ;;?? how to express "symbol with name of length 1"?
		  :expected-type '(or character (string 1))
		  :format-control control
		  :format-arguments args)))
    (typecase object
      (character object)
      (string (if (= 1 (length (the string object)))
		  (char object 0)
		  (do-error
		   "String is not of length one: ~S" (list object))))
      (symbol (if (= 1 (length (symbol-name object)))
		  (schar (symbol-name object) 0)
		  (do-error
		   "Symbol name is not of length one: ~S" (list object))))
      (t (do-error "~S cannot be coerced to a character." (list object))))))


(defun char-name (char)
  "Given a character object, char-name returns the name for that
  object (a symbol)."
  (car (rassoc char char-name-alist)))


(defun name-char (name)
  "Given an argument acceptable to string, name-char returns a character
  object whose name is that symbol, if one exists.  Otherwise, () is returned."
  (cdr (assoc (string name) char-name-alist :test #'string-equal)))




;;;; Predicates:

(defun standard-char-p (char)
  "The argument must be a character object.  Standard-char-p returns T if the
   argument is a standard character -- one of the 95 ASCII printing characters
   or <return>."
  (declare (character char))
  (and (typep char 'base-char)
       (let ((n (char-code (the base-char char))))
	 (or (< 31 n 127)
	     (= n 10)))))

(defun %standard-char-p (thing)
  "Return T if and only if THING is a standard-char.  Differs from
  standard-char-p in that THING doesn't have to be a character."
  (and (characterp thing) (standard-char-p thing)))

(defun graphic-char-p (char)
  "The argument must be a character object.  Graphic-char-p returns T if the
  argument is a printing character (space through ~ in ASCII), otherwise
  returns ()."
  (declare (character char))
  (and (typep char 'base-char)
       (< 31
	  (char-code (the base-char char))
	  127)))


(defun alpha-char-p (char)
  "The argument must be a character object.  Alpha-char-p returns T if the
   argument is an alphabetic character, A-Z or a-z; otherwise ()."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123))))


(defun upper-case-p (char)
  "The argument must be a character object; upper-case-p returns T if the
   argument is an upper-case character, () otherwise."
  (declare (character char))
  (< 64
     (char-code char)
     91))


(defun lower-case-p (char)
  "The argument must be a character object; lower-case-p returns T if the 
   argument is a lower-case character, () otherwise."
  (declare (character char))
  (< 96
     (char-code char)
     123))


(defun both-case-p (char)
  "The argument must be a character object.  Both-case-p returns T if the
  argument is an alphabetic character and if the character exists in
  both upper and lower case.  For ASCII, this is the same as Alpha-char-p."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123))))


(defun digit-char-p (char &optional (radix 10.))
  "If char is a digit in the specified radix, returns the fixnum for
  which that digit stands, else returns NIL.  Radix defaults to 10
  (decimal)."
  (declare (character char) (type (integer 2 36) radix))
  (let ((m (- (char-code char) 48)))
    (declare (fixnum m))
    (cond ((<= radix 10.)
	   ;; Special-case decimal and smaller radices.
	   (if (and (>= m 0) (< m radix))  m  nil))
	  ;; Digits 0 - 9 are used as is, since radix is larger.
	  ((and (>= m 0) (< m 10)) m)
	  ;; Check for upper case A - Z.
	  ((and (>= (setq m (- m 7)) 10) (< m radix)) m)
	  ;; Also check lower case a - z.
	  ((and (>= (setq m (- m 32)) 10) (< m radix)) m)
	  ;; Else, fail.
	  (t nil))))


(defun alphanumericp (char)
  "Given a character-object argument, alphanumericp returns T if the
   argument is either numeric or alphabetic."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 47 m 58) (< 64 m 91) (< 96 m 123))))


(defun char= (character &rest more-characters)
  "Returns T if all of its arguments are the same character."
  (do ((clist more-characters (cdr clist)))
      ((atom clist) T)
    (unless (eq (car clist) character) (return nil))))


(defun char/= (character &rest more-characters)
  "Returns T if no two of its arguments are the same character."
  (do* ((head character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (do* ((l list (cdr l)))                  ;inner loop returns T 
		 ((atom l) T)			     ; iff head /= rest.
	      (if (eq head (car l)) (return nil)))
      (return nil))))


(defun char< (character &rest more-characters)
  "Returns T if its arguments are in strictly increasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (< (char-int c)
	       (char-int (car list)))
      (return nil))))


(defun char> (character &rest more-characters)
  "Returns T if its arguments are in strictly decreasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (> (char-int c)
	       (char-int (car list)))
      (return nil))))


(defun char<= (character &rest more-characters)
  "Returns T if its arguments are in strictly non-decreasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (<= (char-int c)
		(char-int (car list)))
      (return nil))))


(defun char>= (character &rest more-characters)
  "Returns T if its arguments are in strictly non-increasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (>= (char-int c)
		(char-int (car list)))
      (return nil))))



;;; Equal-Char-Code is used by the following functions as a version of char-int
;;;  which loses font, bits, and case info.

(defmacro equal-char-code (character)
  `(let ((ch (char-code ,character)))
     (if (< 96 ch 123) (- ch 32) ch)))



(defun char-equal (character &rest more-characters)
  "Returns T if all of its arguments are the same character.
  Font, bits, and case are ignored."
  (do ((clist more-characters (cdr clist)))
      ((atom clist) T)
    (unless (= (equal-char-code (car clist))
	       (equal-char-code character))
      (return nil))))


(defun char-not-equal (character &rest more-characters)
  "Returns T if no two of its arguments are the same character.
   Font, bits, and case are ignored."
  (do* ((head character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (do* ((l list (cdr l)))
		 ((atom l) T)
	      (if (= (equal-char-code head)
		     (equal-char-code (car l)))
		  (return nil)))
      (return nil))))


(defun char-lessp (character &rest more-characters)
  "Returns T if its arguments are in strictly increasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (< (equal-char-code c)
	       (equal-char-code (car list)))
      (return nil))))


(defun char-greaterp (character &rest more-characters)
  "Returns T if its arguments are in strictly decreasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (> (equal-char-code c)
	       (equal-char-code (car list)))
      (return nil))))


(defun char-not-greaterp (character &rest more-characters)
  "Returns T if its arguments are in strictly non-decreasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (<= (equal-char-code c)
		(equal-char-code (car list)))
      (return nil))))


(defun char-not-lessp (character &rest more-characters)
  "Returns T if its arguments are in strictly non-increasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (>= (equal-char-code c)
		(equal-char-code (car list)))
      (return nil))))




;;;; Miscellaneous functions:

(defun char-upcase (char)
  "Returns CHAR converted to upper-case if that is possible."
  (declare (character char))
  (if (lower-case-p char)
      (code-char (- (char-code char) 32))
      char))

(defun char-downcase (char)
  "Returns CHAR converted to lower-case if that is possible."
  (declare (character char))
  (if (upper-case-p char)
      (code-char (+ (char-code char) 32))
      char))

(defun digit-char (weight &optional (radix 10))
  "All arguments must be integers.  Returns a character object that
  represents a digit of the given weight in the specified radix.  Returns
  NIL if no such character exists.  The character will have the specified
  font attributes."
  (declare (type (integer 2 36) radix) (type unsigned-byte weight))
  (and (typep weight 'fixnum)
       (>= weight 0) (< weight radix) (< weight 36)
       (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight)))))
