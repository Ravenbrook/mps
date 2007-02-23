;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/sunos-os.lisp,v 1.10 2002/11/18 13:52:24 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMUCL under SunOS.  From Miles Bader and David
;;; Axmark.
;;;

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))

(pushnew :sunos *features*)
(setq *software-type* "SunOS")

(defvar *software-version* nil "Version string for supporting software")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  (unless *software-version*
    (setf *software-version*
          (let ((version-line
                 (with-output-to-string (stream)
                   (run-program "/usr/bin/uname"
                                '("-r" )
                                :output stream
                                :pty nil
                                :error nil))))
            (subseq version-line 0 (1- (length version-line))))))
  *software-version*)


;;; OS-INIT -- interface.
;;;
;;; Other OS dependent initializations.
;;; 
(defun os-init ()
  ;; Decache version on save, because it might not be the same when we restart.
  (setf *software-version* nil))

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.
;;;
#-(and sparc svr4)
(defun get-system-info ()
  (multiple-value-bind
      (err? utime stime maxrss ixrss idrss isrss minflt majflt)
      (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (cond ((null err?)
	   (error "Unix system call getrusage failed: ~A."
		  (unix:get-unix-error-msg utime)))
	  (T
	   (values utime stime majflt)))))

;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.
;;;
#+(and sparc svr4)
(defun get-system-info ()
  (multiple-value-bind
      (err? utime stime cutime cstime)
      (unix:unix-times)
    (declare (ignore err? cutime cstime))
    ;; Return times in microseconds; page fault statistics not supported.
    (values (* utime 10000) (* stime 10000) 0)))

;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  (multiple-value-bind (val err)
		       (unix:unix-getpagesize)
    (unless val
      (error "Getpagesize failed: ~A" (unix:get-unix-error-msg err)))
    val))
