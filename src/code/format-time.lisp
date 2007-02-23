;;; -*- Mode: Lisp; Package: Extensions; Log: code.log -*-

;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/format-time.lisp,v 1.8 2001/12/06 23:39:35 pmai Exp $")
;;;
;;; **********************************************************************

;;; Really slick time printing routines built upon the Common Lisp
;;; format function.

;;; Written by Jim Healy, September 1987. 

;;; **********************************************************************

(in-package :extensions)

(export '(format-universal-time format-decoded-time))

(defconstant abbrev-weekday-table
  '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defconstant long-weekday-table
  '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"
     "Sunday"))

(defconstant abbrev-month-table
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
     "Dec"))

(defconstant long-month-table
  '#("January" "February" "March" "April" "May" "June" "July" "August"
     "September" "October" "November" "December"))

;;; The timezone-table is incomplete but workable.

(defconstant timezone-table
  '#("GMT" "" "" "" "" "EST" "CST" "MST" "PST"))

(defconstant daylight-table
  '#(nil nil nil nil nil "EDT" "CDT" "MDT" "PDT"))

;;; Valid-Destination-P ensures the destination stream is okay
;;; for the Format function.

(defun valid-destination-p (destination)
  (or (not destination)
      (eq destination 't)
      (streamp destination)
      (and (stringp destination)
	   (array-has-fill-pointer-p destination))))

;;; Format-Universal-Time - External.

(defun format-universal-time (destination universal-time
					  &key (timezone nil)
					  (style :short)
					  (date-first t)
					  (print-seconds t)
					  (print-meridian t)
					  (print-timezone t)
					  (print-weekday t))
  "Format-Universal-Time formats a string containing the time and date
   given by universal-time in a common manner.  The destination is any
   destination which can be accepted by the Format function.  The
   timezone keyword is an integer specifying hours west of Greenwich.
   The style keyword can be :short (numeric date), :long (months and
   weekdays expressed as words), :abbreviated (like :long but words
   are abbreviated), :rfc1123 (conforming to RFC 1123), :government
   (of the form \"XX Mon XX XX:XX:XX\"), or :iso8601 (conforming to
   ISO 8601), which is the recommended way of printing date and time.
   The keyword date-first, if nil, will print the time first instead of
   the date (the default).  The print- keywords, if nil, inhibit the
   printing of the obvious part of the time/date."
  (unless (valid-destination-p destination)
    (error "~A: Not a valid format destination." destination))
  (unless (integerp universal-time)
    (error "~A: Universal-Time should be an integer." universal-time))
  (when timezone
    (unless (and (rationalp timezone) (<= -24 timezone 24))
      (error "~A: Timezone should be a rational between -24 and 24." timezone))
    (unless (zerop (rem timezone 1/3600))
      (error "~A: Timezone is not a second (1/3600) multiple." timezone)))

  (multiple-value-bind (secs mins hours day month year dow dst tz)
		       (if timezone
			   (decode-universal-time universal-time timezone)
			   (decode-universal-time universal-time))
    (declare (fixnum secs mins hours day month year dow))
    (let ((time-string "~2,'0D:~2,'0D")
	  (date-string
	   (case style
	     (:short "~D/~D/~2,'0D")			;;  MM/DD/YY
	     ((:abbreviated :long) "~A ~D, ~D")		;;  Month DD, YYYY
	     (:rfc1123 "~2,'0D ~A ~4,'0D")		;;  DD Mon YYYY
	     (:government "~2,'0D ~:@(~A~) ~2,'0D")	;;  DD MON YY
	     (:iso8601 "~4,'0D-~2,'0D-~2,'0D")          ;;  YYYY-MM-DD
	     (t
	      (error "~A: Unrecognized :style keyword value." style))))
	  (time-args
	   (case style
	     ((:rfc1123 :iso8601) (list mins hours))
	     (t (list mins (max (mod hours 12) (1+ (mod (1- hours) 12)))))))
	  (date-args (case style
		       (:short
			(list month day (mod year 100)))
		       (:abbreviated
			(list (svref abbrev-month-table (1- month)) day year))
		       (:long
			(list (svref long-month-table (1- month)) day year))
		       (:rfc1123
			(list day (svref abbrev-month-table (1- month)) year))
		       (:government
			(list day (svref abbrev-month-table (1- month))
			      (mod year 100)))
		       (:iso8601
			(list year month day))))
	  (timezone-name (case style
			   (:rfc1123 (timezone-rfc1123-name dst tz))
			   (:iso8601 (timezone-iso8601-name dst tz))
			   (t (timezone-name dst tz)))))
      (declare (simple-string time-string date-string timezone-name))
      (when print-weekday
	(push (case style
		((:short :long) (svref long-weekday-table dow))
		((:abbreviated :rfc1123 :government :iso8601)
		 (svref abbrev-weekday-table dow)))
	      date-args)
	(setq date-string
	      (concatenate 'simple-string "~A, " date-string)))
      (when (or print-seconds (eq style :government))
	(push secs time-args)
	(setq time-string
	      (concatenate 'simple-string time-string ":~2,'0D")))
      (when (and print-meridian (not (member style '(:rfc1123 :iso8601))))
	(push (signum (floor hours 12)) time-args)
	(setq time-string
	      (concatenate 'simple-string time-string " ~[am~;pm~]")))
      (apply #'format destination
	     (if (or date-first (eq style :iso8601))
		 (concatenate 'simple-string date-string " " time-string
			      (when print-timezone
				(if (eq style :iso8601)
				    "~A"
				    " ~A")))
		 (concatenate 'simple-string time-string " " date-string
			      (if print-timezone " ~A")))
	     (if (or date-first (eq style :iso8601))
		 (nconc date-args (nreverse time-args)
			(if print-timezone
			    (list timezone-name)))
		 (nconc (nreverse time-args) date-args
			(if print-timezone
			    (list timezone-name))))))))

(defun timezone-name (dst tz)
  (if (and (integerp tz)
	   (or (and (not dst) (= tz 0))
	       (<= 5 tz 8)))
      (svref (if dst daylight-table timezone-table) tz)
      (multiple-value-bind
	  (rest seconds)
	  (truncate (* (if dst (1- tz) tz) 60 60) 60)
	(multiple-value-bind
	    (hours minutes)
	    (truncate rest 60)
	  (format nil "[~C~D~@[~*:~2,'0D~@[~*:~2,'0D~]~]]"
		  (if (minusp tz) #\- #\+)
		  (abs hours)
		  (not (and (zerop minutes) (zerop seconds)))
		  (abs minutes)
		  (not (zerop seconds))
		  (abs seconds))))))

;;; RFC 1123 style timezone: GMT, +1000, -1000.
;;; Timezone is the negative of the CL timezone.
;;;
(defun timezone-rfc1123-name (dst tz)
  (let ((tz (- tz)))
    (if (and (integerp tz)
	     (or (and (not dst) (= tz 0))
		 (<= 5 tz 8)))
	(svref (if dst daylight-table timezone-table) tz)
	(multiple-value-bind
	      (hours minutes)
	    (truncate (if dst (1+ tz) tz))
	  (format nil "~C~2,'0D~2,'0D"
		  (if (minusp tz) #\- #\+)
		  (abs hours)
		  (abs (truncate (* minutes 60))))))))

;;; ISO 8601 style timezone: Z, +1000, -1000.
;;; Timezone is the negative of the CL timezone.
;;;
(defun timezone-iso8601-name (dst tz)
  (let ((tz (- tz)))
    (if (and (not dst) (= tz 0))
	"Z"
	(multiple-value-bind (hours minutes)
	    (truncate (if dst (1+ tz) tz))
	  (format nil "~C~2,'0D:~2,'0D"
		  (if (minusp tz) #\- #\+)
		  (abs hours)
		  (abs (truncate (* minutes 60))))))))


;;; Format-Decoded-Time - External.

(defun format-decoded-time (destination seconds minutes hours
					  day month year
					  &key (timezone nil)
					  (style :short)
					  (date-first t)
					  (print-seconds t)
					  (print-meridian t)
					  (print-timezone t)
					  (print-weekday t))
  "Format-Decoded-Time formats a string containing decoded-time
   expressed in a humanly-readable manner.  The destination is any
   destination which can be accepted by the Format function.  The
   timezone keyword is an integer specifying hours west of Greenwich.
   The style keyword can be :short (numeric date), :long (months and
   weekdays expressed as words), or :abbreviated (like :long but words are
   abbreviated).  The keyword date-first, if nil, will cause the time
   to be printed first instead of the date (the default).  The print-
   keywords, if nil, inhibit the printing of certain semi-obvious
   parts of the string."
  (unless (valid-destination-p destination)
    (error "~A: Not a valid format destination." destination))
  (unless (and (integerp seconds) (<= 0 seconds 59))
    (error "~A: Seconds should be an integer between 0 and 59." seconds))
  (unless (and (integerp minutes) (<= 0 minutes 59))
    (error "~A: Minutes should be an integer between 0 and 59." minutes))
  (unless (and (integerp hours) (<= 0 hours 23))
    (error "~A: Hours should be an integer between 0 and 23." hours))
  (unless (and (integerp day) (<= 1 day 31))
    (error "~A: Day should be an integer between 1 and 31." day))
  (unless (and (integerp month) (<= 1 month 12))
    (error "~A: Month should be an integer between 1 and 12." month))
  (unless (and (integerp year) (plusp year))
    (error "~A: Hours should be an non-negative integer." year))
  (when timezone
    (unless (and (integerp timezone) (<= 0 timezone 32))
      (error "~A: Timezone should be an integer between 0 and 32."
	     timezone)))
  (format-universal-time destination
   (encode-universal-time seconds minutes hours day month year)
   :timezone timezone :style style :date-first date-first
   :print-seconds print-seconds :print-meridian print-meridian
   :print-timezone print-timezone :print-weekday print-weekday))


