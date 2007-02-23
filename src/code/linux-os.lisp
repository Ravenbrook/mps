;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/linux-os.lisp,v 1.4 2004/06/13 09:53:09 emarsden Exp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMUCL under Linux.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
;;; Derived from mach-os.lisp by Paul Werkowski

(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))

(pushnew :linux *features*)

(setq *software-type* "Linux")

;;; We use READ-SEQUENCE instead of READ-LINE to work around a bug in
;;; the proc file system on Linux kernel 2.6.x. The select() system
;;; call does not work correctly on certain files; it never reports
;;; that data is available for reading. Since CMUCL's fd-streams use
;;; select(), as a part of the SERVE-EVENT mechanism, normal I/O (for
;;; instance with READ-CHAR or READ-LINE) will fail on these files.
;;; Luckily READ-SEQUENCE does not suffer from this problem.
;;;
;;; We could also call "uname -r" here, but using the filesystem-based
;;; interface seems cleaner.
(defun software-version ()
  "Returns a string describing version of the supporting software."
  (when (probe-file "/proc/version")
    (with-open-file (f "/proc/version")
      (let* ((buf (make-string 1024))
             (count (read-sequence buf f :end 1024)))
        (subseq buf 0 (1- count))))))


;;; OS-Init initializes our operating-system interface.
;;;
(defun os-init () nil)


;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.
;;;
(defun get-system-info ()
  (multiple-value-bind (err? utime stime maxrss ixrss idrss
			     isrss minflt majflt)
		       (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (unless err?
      (error "Unix system call getrusage failed: ~A."
	     (unix:get-unix-error-msg utime)))
    
    (values utime stime majflt)))


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

