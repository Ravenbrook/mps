;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/time.lisp,v 1.28 2005/04/15 01:40:07 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the definitions for the Spice Lisp time functions.
;;; They are mostly fairly straightforwardly implemented as calls to the 
;;; time server.
;;;
;;;    Written by Rob MacLachlan.
;;;
(in-package "LISP")
(export '(internal-time-units-per-second get-internal-real-time
	  get-internal-run-time get-universal-time
	  get-decoded-time encode-universal-time decode-universal-time))

(defconstant internal-time-units-per-second 100
  "The number of internal time units that fit into a second.  See
  Get-Internal-Real-Time and Get-Internal-Run-Time.")

(defconstant micro-seconds-per-internal-time-unit
  (/ 1000000 internal-time-units-per-second))




;;; The base number of seconds for our internal "epoch".  We initialize this to
;;; the time of the first call to G-I-R-T, and then subtract this out of the
;;; result.
;;;
(defvar *internal-real-time-base-seconds* nil)
(declaim (type (or (unsigned-byte 32) null) *internal-real-time-base-seconds*))

;;; Get-Internal-Real-Time  --  Public
;;;
(defun get-internal-real-time ()
  "Return the real time in the internal time format.  This is useful for
  finding elapsed time.  See Internal-Time-Units-Per-Second."
  (locally (declare (optimize (speed 3) (safety 0)))
    (multiple-value-bind (ignore seconds useconds) (unix:unix-gettimeofday)
      (declare (ignore ignore) (type (unsigned-byte 32) seconds useconds))
      (let ((base *internal-real-time-base-seconds*)
	    (uint (truncate useconds
			    micro-seconds-per-internal-time-unit)))
	(declare (type (unsigned-byte 32) uint))
	(cond (base
	       (truly-the (unsigned-byte 32)
		    (+ (the (unsigned-byte 32)
			    (* (the (unsigned-byte 32) (- seconds base))
			       internal-time-units-per-second))
		       uint)))
	      (t
	       (setq *internal-real-time-base-seconds* seconds)
	       uint))))))


;;; Get-Internal-Run-Time  --  Public
;;;
#-(and sparc svr4)
(defun get-internal-run-time ()
  "Return the run time in the internal time format.  This is useful for
  finding CPU usage."
  (declare (values (unsigned-byte 32)))
  (locally (declare (optimize (speed 3) (safety 0)))
    (multiple-value-bind (ignore utime-sec utime-usec stime-sec stime-usec)
			 (unix:unix-fast-getrusage unix:rusage_self)
      (declare (ignore ignore)
	       (type (unsigned-byte 31) utime-sec stime-sec)
	       (type (mod 1000000) utime-usec stime-usec))
      (+ (the (unsigned-byte 32)
	      (* (the (unsigned-byte 32) (+ utime-sec stime-sec))
		 internal-time-units-per-second))
	 (truncate (+ utime-usec stime-usec)
		   micro-seconds-per-internal-time-unit)))))

;;; Get-Internal-Run-Time  --  Public
;;;
#+(and sparc svr4)
(defun get-internal-run-time ()
  "Return the run time in the internal time format.  This is useful for
  finding CPU usage."
  (declare (values (unsigned-byte 32)))
  (locally (declare (optimize (speed 3) (safety 0)))
    (multiple-value-bind (ignore utime stime cutime cstime)
	(unix:unix-times)
      (declare (ignore ignore cutime cstime)
	       (type (unsigned-byte 31) utime stime))
      (the (unsigned-byte 32) (+ utime stime)))))


;;;; Encode and Decode universal times.

;;; CURRENT-TIMEZONE -- internal.
;;;
;;; Returns two values:
;;;  - the minuteswest of GMT.
;;;  - T if daylight savings is in effect, NIL if not.
;;;
(alien:def-alien-routine get-timezone c-call:void
  (when c-call:long :in)
  (minutes-west c-call:int :out)
  (daylight-savings-p alien:boolean :out))


;;; Subtract from the returned Internal_Time to get the universal time.
;;; The offset between our time base and the Perq one is 2145 weeks and
;;; five days.
;;;
(defconstant seconds-in-week (* 60 60 24 7))
(defconstant weeks-offset 2145)
(defconstant seconds-offset 432000)
(defconstant minutes-per-day (* 24 60))
(defconstant quarter-days-per-year (1+ (* 365 4)))
(defconstant quarter-days-per-century 146097)
(defconstant november-17-1858 678882)
(defconstant weekday-november-17-1858 2)
(defconstant unix-to-universal-time 2208988800)


;;; Get-Universal-Time  --  Public
;;;
;;;
(defun get-universal-time ()
  "Returns a single integer for the current time of
   day in universal time format."
  (multiple-value-bind (res secs) (unix:unix-gettimeofday)
    (declare (ignore res))
    (+ secs unix-to-universal-time)))

(defun get-decoded-time ()
  "Returns nine values specifying the current time as follows:
   second, minute, hour, date, month, year, day of week (0 = Monday), T
   (daylight savings times) or NIL (standard time), and timezone."
  (decode-universal-time (get-universal-time)))


(defun decode-universal-time (universal-time &optional time-zone)
  "Converts a universal-time to decoded time format returning the following
   nine values: second, minute, hour, date, month, year, day of week (0 =
   Monday), T (daylight savings time) or NIL (standard time), and timezone.
   Completely ignores daylight-savings-time when time-zone is supplied."
  (multiple-value-bind (daylight timezone)
      (if time-zone
	  (values nil (* time-zone 60 60))
	  (multiple-value-bind
		(ignore minwest dst)
	      (get-timezone (- universal-time unix-to-universal-time))
	    (declare (ignore ignore))
	    (values dst (* minwest 60))))
    (declare (fixnum timezone))
    (multiple-value-bind (weeks secs)
	(truncate (+ (- universal-time timezone) seconds-offset)
		  seconds-in-week)
      (let ((weeks (+ weeks weeks-offset)))
	(multiple-value-bind (t1 second)
	    (truncate secs 60)
	  (let ((tday (truncate t1 minutes-per-day)))
	    (multiple-value-bind (hour minute)
		(truncate (- t1 (* tday minutes-per-day)) 60)
	      (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
		     (tcent (truncate t2 quarter-days-per-century)))
		(setq t2 (mod t2 quarter-days-per-century))
		(setq t2 (+ (- t2 (mod t2 4)) 3))
		(let* ((year (+ (* tcent 100)
				(truncate t2 quarter-days-per-year)))
		       (days-since-mar0
			(1+ (truncate (mod t2 quarter-days-per-year) 4)))
		       (day (mod (+ tday weekday-november-17-1858) 7))
		       (t3 (+ (* days-since-mar0 5) 456)))
		  (cond ((>= t3 1989)
			 (setq t3 (- t3 1836))
			 (setq year (1+ year))))
		  (multiple-value-bind (month t3)
		      (truncate t3 153)
		    (let ((date (1+ (truncate t3 5))))
		      (values second minute hour date month year day
			      daylight
			      (if daylight
				  (1+ (/ timezone 60 60))
				  (/ timezone 60 60))))))))))))))


(defun pick-obvious-year (year)
  (declare (type (mod 100) year))
  (let* ((current-year (nth-value 5 (get-decoded-time)))
	 (guess (+ year (* (truncate (- current-year 50) 100) 100))))
    (declare (type (integer 1900 9999) current-year guess))
    (if (> (- current-year guess) 50)
	(+ guess 100)
	guess)))

(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
	  (truncate years 100))
       (truncate (+ years 300) 400))))

(defvar *days-before-month*
  (collect ((results))
    (results nil)
    (let ((sum 0))
      (dolist (days-per-month '(31 28 31 30 31 30 31 31 30 31 30 31))
	(results sum)
	(incf sum days-per-month)))
    (coerce (results) 'vector)))

;;; Encode-Universal-Time  --  Public
;;;
(defun encode-universal-time (second minute hour date month year
				     &optional time-zone)
  "The time values specified in decoded format are converted to 
   universal time, which is returned."
  (declare (type (mod 60) second)
	   (type (mod 60) minute)
	   (type (mod 24) hour)
	   (type (integer 1 31) date)
	   (type (integer 1 12) month)
	   (type (or (integer 0 99) (integer 1900)) year)
	   (type (or null rational) time-zone))
  (let* ((year (if (< year 100)
		   (pick-obvious-year year)
		   year))
	 (days (+ (1- date)
		  (aref *days-before-month* month)
		  (if (> month 2)
		      (leap-years-before (1+ year))
		      (leap-years-before year))
		  (* (- year 1900) 365)))
	 (hours (+ hour (* days 24))))
    (if time-zone
	(+ second (* (+ minute (* (+ hours time-zone) 60)) 60))
	(let* ((minwest-guess
		(nth-value 1
			   (get-timezone (- (* hours 60 60)
					    unix-to-universal-time))))
	       (guess (+ minute (* hours 60) minwest-guess))
	       (minwest
		(nth-value 1
			   (get-timezone (- (* guess 60)
					    unix-to-universal-time)))))
	  (+ second (* (+ guess (- minwest minwest-guess)) 60))))))


;;;; Time:

(defmacro time (form)
  "Evaluates the Form and prints timing information on *Trace-Output*."
  `(%time #'(lambda () ,form)))

;;; MASSAGE-TIME-FUNCTION  --  Internal
;;;
;;;    Try to compile the closure arg to %TIME if it is interpreted.
;;;
(defun massage-time-function (fun)
  (cond
   ((eval:interpreted-function-p fun)
    (multiple-value-bind (def env-p)
			 (function-lambda-expression fun)
      (declare (ignore def))
      (cond
       (env-p
	(warn "TIME form in a non-null environment, forced to interpret.~@
	       Compiling entire form will produce more accurate times.")
	fun)
       (t
	(compile nil fun)))))
   (t fun)))

;;; TIME-GET-SYS-INFO  --  Internal
;;;
;;;    Return all the values that we want time to report.
;;;
(defun time-get-sys-info ()
  (multiple-value-bind (user sys faults)
		       (system:get-system-info)
    (values user sys faults (get-bytes-consed))))

#+(or pentium sparc-v9)
(defun cycle-count/float ()
  (multiple-value-bind (lo hi)
      (vm::read-cycle-counter)
    (+ (* hi (expt 2.0d0 32)) lo)))
#+ppc
(progn
(alien:def-alien-variable cycles-per-tick c-call:int)
(defun cycle-count/float ()
  (multiple-value-bind (lo hi)
      (vm::read-cycle-counter)
    ;; The cycle counter on PPC isn't really a cycle counter.  It
    ;; counts ticks, so we need to convert ticks to cycles.
    ;; CYCLES-PER-TICK is the scale factor, computed in C.
    (* cycles-per-tick (+ (* hi (expt 2.0d0 32)) lo))))
)

#-(or pentium sparc-v9 ppc)
(defun cycle-count/float () 0.0)

(defvar *time-consing* nil)
(defvar *last-time-consing* nil)
(defvar *in-get-time-consing* nil)

(defun get-time-consing ()
  (when (and (null *time-consing*) (not *in-get-time-consing*))
    (let ((*in-get-time-consing* t))
      (time nil)
      (setq *time-consing* *last-time-consing*))))


;;; %TIME  --  Internal
;;;
;;;    The guts of the TIME macro.  Compute overheads, run the (compiled)
;;; function, report the times.
;;;
(defun %time (fun &optional get-time-p)
  (let ((fun (massage-time-function fun))
	old-run-utime
        new-run-utime
        old-run-stime
        new-run-stime
        old-real-time
        new-real-time
        old-page-faults
        new-page-faults
        real-time-overhead
        run-utime-overhead
        run-stime-overhead
        cycle-count
        page-faults-overhead
        old-bytes-consed
        new-bytes-consed
        cons-overhead)
    (get-time-consing)
    ;; Calculate the overhead...
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    ;; Do it a second time to make sure everything is faulted in.
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    (multiple-value-setq
        (new-run-utime new-run-stime new-page-faults new-bytes-consed)
      (time-get-sys-info))
    (setq run-utime-overhead (- new-run-utime old-run-utime))
    (setq run-stime-overhead (- new-run-stime old-run-stime))
    (setq page-faults-overhead (- new-page-faults old-page-faults))
    (setq old-real-time (get-internal-real-time))
    (setq old-real-time (get-internal-real-time))
    (setq new-real-time (get-internal-real-time))
    (setq real-time-overhead (- new-real-time old-real-time))
    (setq cons-overhead (- new-bytes-consed old-bytes-consed))
    ;; Now get the initial times.
    (setq old-real-time (get-internal-real-time))
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      (time-get-sys-info))
    (let ((start-gc-run-time *gc-run-time*))
    (setq cycle-count (- (cycle-count/float)))
    (multiple-value-prog1
        ;; Execute the form and return its values.
        (funcall fun)
      (incf cycle-count (cycle-count/float))
      (multiple-value-setq
	  (new-run-utime new-run-stime new-page-faults new-bytes-consed)
	(time-get-sys-info))
      (setq new-real-time (- (get-internal-real-time) real-time-overhead))
      (let ((gc-run-time (max (- *gc-run-time* start-gc-run-time) 0))
	    (bytes-consed (- new-bytes-consed old-bytes-consed cons-overhead)))
	(unless *in-get-time-consing*
	  (terpri *trace-output*)
	  (pprint-logical-block (*trace-output* nil :per-line-prefix "; ")
	    (format *trace-output*
		    "Evaluation took:~%  ~
		     ~S second~:P of real time~%  ~
		     ~S second~:P of user run time~%  ~
		     ~S second~:P of system run time~%  ~
                     ~:D ~A cycles~%  ~
		     ~@[[Run times include ~S second~:P GC run time]~%  ~]~
		     ~S page fault~:P and~%  ~
		     ~:D bytes consed.~%"
		    (max (/ (- new-real-time old-real-time)
			    (float internal-time-units-per-second))
			 0.0)
		    (max (/ (- new-run-utime old-run-utime) 1000000.0) 0.0)
		    (max (/ (- new-run-stime old-run-stime) 1000000.0) 0.0)
		    (truncate cycle-count)
		    "CPU"
		    (unless (zerop gc-run-time)
		      (/ (float gc-run-time)
			 (float internal-time-units-per-second)))
		    (max (- new-page-faults old-page-faults) 0)
		    (max (- bytes-consed (or *time-consing* 0)) 0)))
	  (terpri *trace-output*))
	(setq *last-time-consing* bytes-consed))))))
