;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/mach-os.lisp,v 1.12 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; OS interface functions for CMU CL under Mach.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
(in-package "SYSTEM")
(use-package "EXTENSIONS")
(export '(get-system-info get-page-size os-init))
(export '(*task-self* *task-data* *task-notify*))

(pushnew :mach *features*)
(setq *software-type* "MACH/4.3BSD")

(defun software-version ()
  "Returns a string describing version of the supporting software."
  (string-trim '(#\newline)
	       (with-output-to-string (stream)
		 (run-program "/usr/cs/etc/version" ; Site dependent???
			      nil :output stream))))


;;; OS-Init initializes our operating-system interface.  It sets the values
;;; of the global port variables to what they should be and calls the functions
;;; that set up the argument blocks for the server interfaces.

(defvar *task-self*)

(defun os-init ()
  (setf *task-self* (mach:mach-task_self))
  #+sparc ;; Can't use #x20000000 thru #xDFFFFFFF, but mach tries to let us.
  (system:allocate-system-memory-at (system:int-sap #x20000000) #xc0000000))


;;; GET-SYSTEM-INFO  --  Interface
;;;
;;;    Return system time, user time and number of page faults.  For
;;; page-faults, we add pagein and pageout, since that is a somewhat more
;;; interesting number than the total faults.
;;;
(defun get-system-info ()
  (multiple-value-bind (err? utime stime maxrss ixrss idrss
			     isrss minflt majflt)
		       (unix:unix-getrusage unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt majflt))
    (unless err?
      (error "Unix system call getrusage failed: ~A."
	     (unix:get-unix-error-msg utime)))
    
    (multiple-value-bind (gr ps fc ac ic wc zf ra in ot)
			 (mach:vm_statistics *task-self*)
      (declare (ignore ps fc ac ic wc zf ra))
      (mach:gr-error 'mach:vm_statistics gr)
      
      (values utime stime (+ in ot)))))


;;; GET-PAGE-SIZE  --  Interface
;;;
;;;    Return the system page size.
;;;
(defun get-page-size ()
  (mach:gr-call* mach:vm_statistics *task-self*))

