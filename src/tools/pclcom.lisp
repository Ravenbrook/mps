;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/tools/pclcom.lisp,v 1.32 2003/09/08 16:07:04 toy Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "CL-USER")
(setf lisp::*enable-package-locked-errors* nil)

(when (find-package "PCL")
  ;; Load the lisp:documentation functions.
  (load "target:code/misc")

  ;;
  ;; Blow away make-instance optimizer so that it doesn't confuse
  ;; bootstrapping.
  (setf (compiler-macro-function 'make-instance) nil)
  ;;
  ;; Blow away other compiler-macros exported from Lisp so that bootstrapping
  ;; doesn't get confused.
  (setf (compiler-macro-function 'slot-value) nil)
  (setf (compiler-macro-function 'slot-boundp) nil)
  ;;
  ;; Undefine all generic functions exported from Lisp so that
  ;; bootstrapping doesn't get confused, but convert condition
  ;; accessor gfs to normal functions beforehand, for the obvious
  ;; reason.
  (when (fboundp 'conditions::make-early-condition-accessors-generic)
    (conditions::make-early-condition-accessors-generic nil))
  (let ((class (kernel::find-class 'generic-function nil)))
    (when class
      (do-external-symbols (sym "LISP")
	(when (and (fboundp sym)
		   (typep (fdefinition sym) class))
	  (fmakunbound sym))
	(let ((ssym `(setf ,sym)))
	  (when (and (fboundp ssym)
		     (typep (fdefinition ssym) class))
	    (fmakunbound ssym))))))

  (let ((sym (find-symbol "%CHECK-GF-REDEFINITION" "PCL")))
    (when sym
      (setq lisp::*setf-fdefinition-hook*
	    (delete (symbol-function sym) lisp::*setf-fdefinition-hook*))))

  (when (boundp 'kernel::*defstruct-hooks*)
    (let ((sym (find-symbol "REINITIALIZE-STRUCTURE-CLASS" "PCL")))
      (when sym
	(setq kernel::*defstruct-hooks*
	      (delete sym kernel::*defstruct-hooks*)))))

  ;; Undefine all PCL classes, and clear CLASS-PCL-CLASS slots.
  (let ((wot (kernel::find-symbol "*FIND-CLASS*" "PCL")))
    (when (and wot (boundp wot))
      (do-hash (name ignore (symbol-value wot))
	(declare (ignore ignore))
	(let ((class (kernel::find-class name nil)))
	  (cond ((not class))
		((typep class 'kernel::std-class)
		 (setf (kernel:class-cell-class
			(kernel:find-class-cell name))
		       nil)
		 (setf (info type kind name) nil))
		(t
		 (setf (kernel:%class-pcl-class class) nil)))))))

  ;; Rename the PCL package to OLD-PCL, then restoring pcl::class and
  ;; pcl::..slot-unbound.. back to the PCL package as they need be
  ;; consistent with the symbols recognised by the compiler.
  (let ((slot-unbound 'pcl::..slot-unbound..))
    (rename-package "PCL" "OLD-PCL")
    (make-package "PCL")
    (import slot-unbound "PCL")
    (kernel:%set-symbol-package slot-unbound (find-package "PCL"))))

(when (find-package "CLOS-MOP")
  (rename-package "CLOS-MOP" "OLD-CLOS-MOP"))

;;; Inhibit ANSI :print-function and :print-object defstruct options.
(setq kernel::*ansi-defstruct-options-p* nil)

(setf c:*suppress-values-declaration* t)
(setf *features* (adjoin :setf *features*))

(setf (search-list "pcl:") '("target:pcl/"))

(let ((obj (make-pathname :defaults "pcl:defsys"
			  :type (c:backend-byte-fasl-file-type c:*backend*))))
  (when (< (or (file-write-date obj) 0)
	   (file-write-date "pcl:defsys.lisp"))
    (compile-file "pcl:defsys" :byte-compile t)))

(load "pcl:defsys" :verbose t)

(import 'kernel:funcallable-instance-p (find-package "PCL"))
(setq *gc-verbose* nil)

(with-compiler-log-file
    ("target:compile-pcl.log"
     :optimize '(optimize (debug #+small .5 #-small 2)
			  (speed 2) (safety #+small 0 #-small 2)
			  (inhibit-warnings 2))
     :optimize-interface '(optimize-interface #+small (safety 1))
     :context-declarations
     '((:external (declare (optimize-interface (safety 2) (debug 1))))
       ((:or :macro (:match "$EARLY-") (:match "$BOOT-"))
	(declare (optimize (speed 0))))))
 (pcl::compile-pcl))


(cat-if-anything-changed
 "pcl:gray-streams-library"
 "pcl:gray-streams-class"
 "pcl:gray-streams")

(cat-if-anything-changed
 "pcl:simple-streams-library"
 "pcl:simple-streams/herald"
 "pcl:simple-streams/classes"
 "pcl:simple-streams/internal"
 "pcl:simple-streams/strategy"
 "pcl:simple-streams/impl"
 "pcl:simple-streams/null"
 "pcl:simple-streams/direct"
 "pcl:simple-streams/file"
 "pcl:simple-streams/string"
 "pcl:simple-streams/terminal"
 "pcl:simple-streams/socket")

(cat-if-anything-changed
 "pcl:iodefs-library"
 "pcl:simple-streams/iodefs")

(cat-if-anything-changed
 "pcl:gray-compat-library"
 "pcl:simple-streams/gray-compat")
