;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/tools/clmcom.lisp,v 1.21 2005/12/23 14:14:20 pwerkowski Exp $")
;;;
;;; **********************************************************************
;;;
;;; File for compiling the Motif toolkit and related interface
;;; stuff.
;;;
(in-package "CL-USER")


;;; If CLX and PCL have not been loaded, then load them.
;;;
#-pcl(load "target:pcl/pclload")
#-clx
(progn
  (load "target:clx/clx-library")
  #+gencgc (gc :full t)
  #-gencgc (ext:purify))

(pushnew :motif *features*)

;;; Set this pesky bit ON - see note in clxcom.lisp
(when (boundp 'conditions::*make-condition-accessor-methods*)
  (setq conditions::*make-condition-accessor-methods* t))

(defparameter tk-internals-files 
  '("target:motif/lisp/initial"
    "target:motif/lisp/internals"
    "target:motif/lisp/transport"
    "target:motif/lisp/events"
    "target:motif/lisp/conversion"))

(defparameter tk-files 
  '("target:motif/lisp/interface-glue"
    "target:motif/lisp/xt-types"
    "target:motif/lisp/string-base"
    "target:motif/lisp/prototypes"
    "target:motif/lisp/interface-build"
    "target:motif/lisp/callbacks"
    "target:motif/lisp/widgets"
;    "target:motif/lisp/timer-support"
    "target:motif/lisp/main"))

(defparameter interface-files
  '("target:interface/initial"
    "target:interface/interface"
    "target:interface/inspect"
    "target:interface/debug"
    "target:interface/precom"))


;;; Make sure we don't try to debug a possibly broken new version with the
;;; windowing debugger.
;;;
(unless (find-package "INTERFACE")
  (make-package "INTERFACE"))
(eval `(defparameter ,(intern "*INTERFACE-STYLE*" "INTERFACE") :tty))

;;; Load any existing toolkit files:
;;;
#-motif
(dolist (f (append tk-internals-files tk-files))
  (flet ((try (type) (probe-file (make-pathname :defaults f :type type))))
    (let ((pn (or (try (c:backend-fasl-file-type c:*backend*))
		  (try (c:backend-byte-fasl-file-type c:*backend*)))))
      (when pn (load pn)))))

(with-compiler-log-file
    ("target:compile-motif.log")

  (with-compilation-unit
      (:optimize '(optimize (speed 3) (ext:inhibit-warnings 3)
			    #+small (safety 0)
			    #+small (debug .5)))
    
    (dolist (f tk-internals-files)
      (comf f :load t)))
  
  (with-compilation-unit
      (:optimize
       '(optimize (debug #-small 2 #+small .5) 
		  (speed 2) (inhibit-warnings 2)
		  (safety #-small 1 #+small 0))
       :optimize-interface
       '(optimize-interface (debug .5))
       :context-declarations
       '(((:and :external :global)
	  (declare (optimize-interface (safety 2) (debug 1))))
	 ((:and :external :macro)
	  (declare (optimize (safety 2))))
	 (:macro (declare (optimize (speed 0))))))
    
    (dolist (f tk-files)
      (comf f :load t))

    (let ((btki (intern "BUILD-TOOLKIT-INTERFACE" "XT")))
      (unless (fboundp btki)
	(mapc #'load tk-internals-files)
	(mapc #'load tk-files))
    
      (funcall (fdefinition btki)))
    
    (dolist (f interface-files)
      (comf f :load t #+small :byte-compile #+small t))))

(apply #'cat-if-anything-changed
       "target:interface/clm-library"
       (remove "target:motif/lisp/interface-build"
	       (append tk-internals-files tk-files interface-files)
	       :test #'string=))
