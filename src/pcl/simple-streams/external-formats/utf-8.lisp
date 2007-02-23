;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/simple-streams/external-formats/utf-8.lisp,v 1.1 2003/06/26 13:27:43 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; A UTF-8 external format for simple-streams

(define-external-format :utf-8
    (octets-to-char (state input unput)
      (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
      ;; return (values character octets-used state)
      (let ((code (funcall input)))
	(cond ((< code #x80)
	       (values (code-char code) 1 nil))
	      ((= (logand code #b11000000) #b10000000)
	       (error "UTF-8 desync"))	; incf count, retry with next octet
	      ((= (logand code #b11100000) #b11000000)
	       (let ((next (funcall input)))
		 (if (= (logand next #b11000000) #b10000000)
		     (let ((code (dpb (ldb (byte 6 0) code) (byte 6 6)
				      (ldb (byte 6 0) next))))
		       (if (< code #x100)
			   (values (code-char code) 2 nil)
			   (values #\? 2 nil)))
		     (progn
		       (warn "UTF-8: short read")
		       (funcall unput 1)
		       (values #\? 1 nil)))))
	      ((= (logand code #b11110000) #b11100000)
	       (dotimes (i 2) (funcall input))
	       (values #\? 3 nil))
	      ((= (logand code #b11111000) #b11110000)
	       (dotimes (i 3) (funcall input))
	       (values #\? 4 nil))
	      ((= (logand code #b11111100) #b11111000)
	       (dotimes (i 4) (funcall input))
	       (values #\? 5 nil))
	      ((= (logand code #b11111110) #b11111100)
	       (dotimes (i 5) (funcall input))
	       (values #\? 6 nil))
	      (t
	       (error "Illegal UTF-8 character")))))
  (char-to-octets (char state output)
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    ;; return state
    (let ((code (char-code char)))
      (if (< code #x80)
	  (funcall output code)
	  (progn
	    (funcall output (logior #b11000000 (ldb (byte 2 6) code)))
	    (funcall output (logior #b10000000 (ldb (byte 6 0) code))))))
    nil))
