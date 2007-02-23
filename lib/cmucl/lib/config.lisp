;;; -*- Mode: Lisp; Package: System -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/tools/config.lisp,v 1.8 2005/03/04 17:07:15 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Utility to load subsystems and save a new core.
;;;
(in-package "CL-USER")


(block abort
  (let ((output-file #p"library:lisp.core")
	(load-gray-streams t)
	(load-clm t)
	(load-clx t)
	(load-hemlock t)
	(other ()))
    (loop
      (fresh-line)
      (format t " 1: specify result file (currently ~S)~%"
	      (namestring output-file))
      (format t " 2: toggle loading of the Gray Stream library, currently ~
		 ~:[dis~;en~]abled.~%"
	      load-gray-streams)
      (format t " 3: toggle loading of the CLX X library, currently ~
		 ~:[dis~;en~]abled.~%"
	      load-clx)
      (format t " 4: toggle loading of Motif and the graphical debugger, ~
		 currently ~:[dis~;en~]abled.~
		 ~:[~%    (would force loading of CLX.)~;~]~%"
	      load-clm load-clx)
      (format t " 5: toggle loading the Hemlock editor, currently ~
		 ~:[dis~;en~]abled.~
		 ~:[~%    (would force loading of CLX.)~;~]~%"
	      load-hemlock load-clx)
      (format t " 6: specify some site-specific file to load.~@
		 ~@[    Current files:~%~{      ~S~%~}~]"
	      (mapcar #'namestring other))
      (format t " 7: configure according to current options.~%")
      (format t " 8: abort the configuration process.~%")
      (format t "~%Option number: ")
      (force-output)
      (flet ((file-prompt (prompt)
	       (format t prompt)
	       (force-output)
	       (pathname (string-trim " 	" (read-line)))))
	(let ((res (ignore-errors (read-from-string (read-line)))))
	  (case res
	    (1
	     (setq output-file (file-prompt "Result core file name: ")))
	    (2
	     (setq load-gray-streams (not load-gray-streams)))
	    (3
	     (unless (setq load-clx (not load-clx))
	       (setq load-hemlock nil)))
	    (4
	     (when (setq load-clm (not load-clm))
	       (setq load-clx t)))
	    (5
	     (when (setq load-hemlock (not load-hemlock))
	       (setq load-clx t)))
	    (6
	     (setq other
		   (append other
			   (list (file-prompt "File(s) to load ~
					       (can have wildcards): ")))))
	    (7 (return))
	    (8
	     (format t "~%Aborted.~%")
	     (return-from abort))))))
    
    (gc-off)
    (when load-gray-streams
      (require :gray-streams))
    (when load-clx
      (require :clx))
    (when load-clm
      (require :clm))
    (when load-hemlock
      (require :hemlock))
    (dolist (f other) (load f))
    
    (setq *info-environment*
	  (list* (make-info-environment :name "Working")
		 (compact-info-environment (first *info-environment*)
					   :name "Auxiliary")
		 (rest *info-environment*)))
    
    (when (probe-file output-file)
      (multiple-value-bind
	  (ignore old new)
	  (rename-file output-file
		       (concatenate 'string (namestring output-file)
				    ".BAK"))
	(declare (ignore ignore))
	(format t "~&Saved ~S as ~S.~%" (namestring old) (namestring new))))
    
    ;;
    ;; Enable the garbage collector.  But first fake it into thinking that
    ;; we don't need to garbage collect.  The save-lisp is going to call
    ;; purify so any garbage will be collected then.
    (setf lisp::*need-to-collect-garbage* nil)
    (gc-on)
    ;;
    ;; Save the lisp.
    (save-lisp output-file)))

(quit)
