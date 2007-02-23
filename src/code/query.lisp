;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/query.lisp,v 1.5 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Querying the user.
;;; Written by Walter van Roggen, 27 December 1982.
;;; Brought up to date and fixed somewhat by Rob MacLachlan.
;;; Modified by Bill Chiles.
;;;
;;; These functions are part of the standard Spice Lisp environment.
;;;
;;; **********************************************************************
;;;

(in-package "LISP")

(export '(y-or-n-p yes-or-no-p))

(defun query-readline ()
  (force-output *query-io*)
  (string-trim " 	" (read-line *query-io*)))


;;; Y-OR-N-P  --  Public.
;;;
(defun y-or-n-p (&optional format-string &rest arguments)
  "Y-OR-N-P prints the message, if any, and reads characters from *QUERY-IO*
   until the user enters y or Y as an affirmative, or either n or N as a
   negative answer.  It ignores preceding whitespace and asks again if you
   enter any other characters."
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (loop
    (let* ((line (query-readline))
	   (ans (if (string= line "")
		    #\? ;Force CASE below to issue instruction.
		    (schar line 0))))
      (unless (whitespacep ans)
	(case ans
	  ((#\y #\Y) (return t))
	  ((#\n #\N) (return nil))
	  (t
	   (write-line "Type \"y\" for yes or \"n\" for no. " *query-io*)
	   (when format-string
	     (apply #'format *query-io* format-string arguments))
	   (force-output *query-io*)))))))

;;; YES-OR-NO-P  --  Public.
;;;
;;; This is similar to Y-OR-N-P, but it clears the input buffer, beeps, and
;;; uses READ-LINE to get "YES" or "NO".
;;;
(defun yes-or-no-p (&optional format-string &rest arguments)
  "YES-OR-NO-P is similar to Y-OR-N-P, except that it clears the 
   input buffer, beeps, and uses READ-LINE to get the strings 
   YES or NO."
  (clear-input *query-io*)
  (beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (do ((ans (query-readline) (query-readline)))
      (())
    (cond ((string-equal ans "YES") (return t))
	  ((string-equal ans "NO") (return nil))
	  (t
	   (write-line "Type \"yes\" for yes or \"no\" for no. " *query-io*)
	   (when format-string
	     (apply #'format *query-io* format-string arguments))))))
