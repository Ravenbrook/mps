;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/defsys.lisp,v 1.36 2004/04/14 03:32:47 rtoy Exp $")
;;;
;;; Some support stuff for compiling and loading PCL.  It would be nice if
;;; there was some portable make-system we could all agree to share for a
;;; while.  At least until people really get databases and stuff.
;;;
;;; ***                                                               ***
;;; ***        DIRECTIONS FOR INSTALLING PCL AT YOUR SITE             ***
;;; ***                                                               ***
;;;
;;; To get PCL working at your site you should:
;;; 
;;;  - Get all the PCL source files from Xerox.  The complete list of source
;;;    file names can be found in the defsystem for PCL which appears towards
;;;    the end of this file.
;;; 
;;;  - Edit the variable *pcl-directory* below to specify the directory at
;;;    your site where the pcl sources and binaries will be.  This variable
;;;    can be found by searching from this point for the string "***" in
;;;    this file.
;;; 
;;;  - Use the function (pcl::compile-pcl) to compile PCL for your site.
;;; 
;;;  - Once PCL has been compiled it can be loaded with (pcl::load-pcl).
;;;    Note that PCL cannot be loaded on top of itself, nor can it be
;;;    loaded into the same world it was compiled in.
;;;

(in-package :cl-user)

(defpackage "WALKER"
  (:use "COMMON-LISP" "EXT")
  (:export "DEFINE-WALKER-TEMPLATE"
	   "WALK-FORM"
	   "WALK-FORM-EXPAND-MACROS-P"
	   "NESTED-WALK-FORM"
	   "VARIABLE-LEXICAL-P"
	   "VARIABLE-SPECIAL-P"
	   "VARIABLE-GLOBALLY-SPECIAL-P"
	   "*VARIABLE-DECLARATIONS*"
	   "VARIABLE-DECLARATION"
	   "MACROEXPAND-ALL"))
(defpackage "PCL" (:use :walker :common-lisp :ext))
(in-package "PCL")

;;;
;;; Sure, its weird for this to be here, but in order to follow the rules
;;; about order of export and all that stuff, we can't put it in PKG before
;;; we want to use it.
;;; 
(defvar *the-pcl-package* (find-package :pcl))

(defvar *pcl-system-date* "$Date: 2004/04/14 03:32:47 $")

(setf (getf ext:*herald-items* :pcl)
      `("    CLOS based on Gerd's PCL " ,(subseq *pcl-system-date* 7 26)))


;;; Yet Another Sort Of General System Facility and friends.
;;;
;;; The entry points are defsystem and operate-on-system.  defsystem is used
;;; to define a new system and the files with their load/compile constraints.
;;; Operate-on-system is used to operate on a system defined that has been
;;; defined by defsystem.  For example:
#||

(defsystem my-very-own-system
	   "/usr/myname/lisp/"
  ((classes   (precom)           ()                ())
   (methods   (precom classes)   (classes)         ())
   (precom    ()                 (classes methods) (classes methods))))

This defsystem should be read as follows:

* Define a system named MY-VERY-OWN-SYSTEM, the sources and binaries
  should be in the directory "/usr/me/lisp/".  There are three files
  in the system, there are named classes, methods and precom.  (The
  extension the filenames have depends on the lisp you are running in.)
  
* For the first file, classes, the (precom) in the line means that
  the file precom should be loaded before this file is loaded.  The
  first () means that no other files need to be loaded before this
  file is compiled.  The second () means that changes in other files
  don't force this file to be recompiled.

* For the second file, methods, the (precom classes) means that both
  of the files precom and classes must be loaded before this file
  can be loaded.  The (classes) means that the file classes must be
  loaded before this file can be compiled.  The () means that changes
  in other files don't force this file to be recompiled.

* For the third file, precom, the first () means that no other files
  need to be loaded before this file is loaded.  The first use of
  (classes methods)  means that both classes and methods must be
  loaded before this file can be compiled.  The second use of (classes
  methods) mean that whenever either classes or methods changes precom
  must be recompiled.

Then you can compile your system with:

 (operate-on-system 'my-very-own-system :compile)

and load your system with:

 (operate-on-system 'my-very-own-system :load)

||#

;;; 
(defvar *system-directory*)

;;;
;;; *port* is a list of symbols (in the PCL package) which represent the
;;; Common Lisp in which we are now running.  Many of the facilities in
;;; defsys use the value of *port* rather than #+ and #- to conditionalize
;;; the way they work.
;;; 
(defvar *port* '(CMU))

;;;
;;; When you get a copy of PCL (by tape or by FTP), the sources files will
;;; have extensions of ".lisp" in particular, this file will be defsys.lisp.
;;; The preferred way to install pcl is to rename these files to have the
;;; extension which your lisp likes to use for its files.  Alternately, it
;;; is possible not to rename the files.  If the files are not renamed to
;;; the proper convention, the second line of the following defvar should
;;; be changed to:
;;;     (let ((files-renamed-p nil)
;;;
;;; Note: Something people installing PCL on a machine running Unix
;;;       might find useful.  If you want to change the extensions
;;;       of the source files from ".lisp" to ".lsp", *all* you have
;;;       to do is the following:
;;;
;;;       % foreach i (*.lisp)
;;;       ? mv $i $i:r.lsp
;;;       ? end
;;;       %
;;;
;;;       I am sure that a lot of people already know that, and some
;;;       Unix hackers may say, "jeez who doesn't know that".  Those
;;;       same Unix hackers are invited to fix mv so that I can type
;;;       "mv *.lisp *.lsp".
;;;
(defvar *default-pathname-extensions*
  (car '(("lisp" . #.(c:backend-fasl-file-type c:*backend*)))))

(defvar *pathname-extensions*
  (let ((proper-extensions *default-pathname-extensions*))
    (cond ((null proper-extensions) '("l" . "lbin"))
          (t proper-extensions))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun get-system (name)
  (get name 'system-definition))

(defun set-system (name new-value)
  (setf (get name 'system-definition) new-value))

(defmacro defsystem (name directory files)
  `(set-system ',name (list (lambda () ,directory)
			    (make-modules ',files)
			    ',(mapcar #'car files))))

)


;;;
;;; The internal datastructure used when operating on a system.
;;; 
(defstruct (module (:constructor make-module (name))
                   (:print-function
                     (lambda (m s d)
                       (declare (ignore d))
                       (format s "#<Module ~A>" (module-name m)))))
  name
  load-env
  comp-env
  recomp-reasons)

(defun make-modules (system-description)
  (let ((modules ()))
    (labels ((get-module (name)
               (or (find name modules :key #'module-name)
                   (progn (setq modules (cons (make-module name) modules))
                          (car modules))))
             (parse-spec (spec)
               (if (eq spec t)
                   (reverse (cdr modules))
		   (case (car spec)
		     (+ (append (reverse (cdr modules))
				(mapcar #'get-module (cdr spec))))
		     (- (let ((rem (mapcar #'get-module (cdr spec))))
			  (remove-if (lambda (m) (member m rem))
				     (reverse (cdr modules)))))
		     (otherwise (mapcar #'get-module spec))))))
      (dolist (file system-description)
        (let* ((name (car file))
               (port (car (cddddr file)))
               (module nil))
          (when (or (null port)
                    (member port *port*))
            (setq module (get-module name))
            (setf (module-load-env module) (parse-spec (cadr file))
                  (module-comp-env module) (parse-spec (caddr file))
                  (module-recomp-reasons module) (parse-spec (cadddr file))))))
      (let ((filenames (mapcar #'car system-description)))
	(sort modules (lambda (name1 name2)
			(member name2 (member name1 filenames)))
	      :key #'module-name)))))


(defun make-transformations (modules filter make-transform)
  (declare (type function filter make-transform))
  (let ((transforms (list nil)))
    (dolist (m modules)
      (when (funcall filter m transforms) (funcall make-transform m transforms)))
    (reverse (cdr transforms))))

(defun make-compile-transformation (module transforms)
  (unless (dolist (trans transforms)
	    (and (eq (car trans) :compile)
		 (eq (cadr trans) module)
		 (return t)))
    (dolist (c (module-comp-env module)) (make-load-transformation c transforms))
    (setf (cdr transforms)
	  (remove-if (lambda (trans) (and (eq (car trans) :load)
					  (eq (cadr trans) module)))
		     (cdr transforms)))
    (push `(:compile ,module) (cdr transforms))))

(defvar *being-loaded* ())

(defun make-load-transformation (module transforms)
  (if (assoc module *being-loaded*)
      (throw module (setf (cdr transforms) (cdr (assoc module *being-loaded*))))
      (let ((*being-loaded* (cons (cons module (cdr transforms)) *being-loaded*)))
	(catch module
	  (unless (dolist (trans transforms)
		    (when (and (eq (car trans) :load)
			       (eq (cadr trans) module))
		      (return t)))
	    (dolist (l (module-load-env module))
	      (make-load-transformation l transforms))
	    (push `(:load ,module) (cdr transforms)))))))

(defun make-load-without-dependencies-transformation (module transforms)
  (unless (dolist (trans transforms)
            (and (eq (car trans) :load)
                 (eq (cadr trans) module)
                 (return trans)))
    (push `(:load ,module) (cdr transforms))))

(defun compile-filter (module transforms)
  (or (dolist (r (module-recomp-reasons module))
        (when (dolist (transform transforms)
                (when (and (eq (car transform) :compile)
                           (eq (cadr transform) r))
                  (return t)))
          (return t)))
      (null (probe-file (make-binary-pathname (module-name module))))
      (> (file-write-date (make-source-pathname (module-name module)))
         (file-write-date (make-binary-pathname (module-name module))))))

(defun operation-transformations (name mode &optional arg)
  (let ((system (get-system name)))
    (unless system (error "Can't find system with name ~S." name))
    (let ((*system-directory* (funcall (the function (car system))))
	  (modules (cadr system)))
      (ecase mode
	(:compile
	  ;; Compile any files that have changed and any other files
	  ;; that require recompilation when another file has been
	  ;; recompiled.
	  (make-transformations
	   modules
	   #'compile-filter
	   #'make-compile-transformation))
	(:recompile
	  ;; Force recompilation of all files.
	  (make-transformations
	   modules
	   #'true
	   #'make-compile-transformation))
	(:recompile-some
	  ;; Force recompilation of some files.  Also compile the
	  ;; files that require recompilation when another file has
	  ;; been recompiled.
	  (make-transformations
	   modules
	   (lambda (m transforms)
	     (or (member (module-name m) arg)
		 (compile-filter m transforms)))
	   #'make-compile-transformation))
	(:query-compile
	  ;; Ask the user which files to compile.  Compile those
	  ;; and any other files which must be recompiled when
	  ;; another file has been recompiled.
	  (make-transformations
	   modules
	   (lambda (m transforms)
	     (or (compile-filter m transforms)
		 (y-or-n-p "Compile ~A?"
			   (module-name m))))
	   #'make-compile-transformation))
	(:confirm-compile
	  ;; Offer the user a chance to prevent a file from being
	  ;; recompiled.
	  (make-transformations
	   modules
	   (lambda (m transforms)
	     (and (compile-filter m transforms)
		  (y-or-n-p "Go ahead and compile ~A?"
			    (module-name m))))
	   #'make-compile-transformation))
	(:load
	  ;; Load the whole system.
	  (make-transformations
	   modules
	   #'true
	   #'make-load-transformation))
	(:query-load
	  ;; Load only those files the user says to load.
	  (make-transformations
	   modules
	   (lambda (m transforms)
	     (declare (ignore transforms))
	     (y-or-n-p "Load ~A?" (module-name m)))
	   #'make-load-without-dependencies-transformation))))))

(defun true (&rest ignore)
  (declare (ignore ignore))
  t)

(defparameter *byte-files* '(defclass defcombin env))

(defun operate-on-system (name mode &optional arg print-only)
  (let ((system (get-system name)))
    (unless system (error "Can't find system with name ~S." name))
    (let* ((*system-directory* (funcall (the function (car system))))
	   (transformations (operation-transformations name mode arg)))
      (labels ((load-binary (name pathname)
		 (format t "~&Loading binary of ~A...~%" name)
		 (or print-only (load pathname)))	       
	       (load-module (m)
		 (let* ((name (module-name m))
			(*load-verbose* nil)
			(binary (make-binary-pathname name)))
		   (load-binary name binary)))
	       (compile-module (m)
		 (format t "~&Compiling ~A...~%" (module-name m))
		 (unless print-only
		   (let  ((name (module-name m)))
		     (compile-file (make-source-pathname name)
				   :output-file
				   (make-pathname :defaults
						  (make-binary-pathname name)
						  :version :newest)
				   :byte-compile nil)))))
	(with-compilation-unit ()
	  (loop (when (null transformations) (return t))
	    (let ((transform (pop transformations)))
	      (ecase (car transform)
		(:compile (compile-module (cadr transform)))
		(:load (load-module (cadr transform)))))))))))


(defun make-source-pathname (name) (make-pathname-internal name :source))
(defun make-binary-pathname (name) (make-pathname-internal name :binary))

(defun make-pathname-internal (name type)
  (let* ((extension (ecase type
                      (:source (car *pathname-extensions*))
                      (:binary (cdr *pathname-extensions*))))
         (directory (pathname
		      (etypecase *system-directory*
			(string *system-directory*)
			(pathname *system-directory*)
			(cons (ecase type
				(:source (car *system-directory*))
				(:binary (cdr *system-directory*)))))))
         (pathname
           (make-pathname
             :name (string-downcase (string name))
             :type extension
             :defaults directory)))

    pathname))

(defun system-source-files (name)
  (let ((system (get-system name)))
    (unless system (error "Can't find system with name ~S." name))
    (let ((*system-directory* (funcall (the function (car system))))
	  (modules (cadr system)))
      (mapcar (lambda (module)
		(make-source-pathname (module-name module)))
	      modules))))

(defun system-binary-files (name)
  (let ((system (get-system name)))
    (unless system (error "Can't find system with name ~S." name))
    (let ((*system-directory* (funcall (the function (car system))))
	  (modules (cadr system)))
      (mapcar (lambda (module)
		(make-binary-pathname (module-name module)))
	      modules))))

;;; ***                SITE SPECIFIC PCL DIRECTORY                        ***
;;;
;;; *pcl-directory* is a variable which specifies the directory pcl is stored
;;; in at your site.  If the value of the variable is a single pathname, the
;;; sources and binaries should be stored in that directory.  If the value of
;;; that directory is a cons, the CAR should be the source directory and the
;;; CDR should be the binary directory.
;;;
;;; By default, the value of *pcl-directory* is set to the directory that
;;; this file is loaded from.  This makes it simple to keep multiple copies
;;; of PCL in different places, just load defsys from the same directory as
;;; the copy of PCL you want to use.
;;;
;;; Note that the value of *PCL-DIRECTORY* is set using a DEFVAR.  This is
;;; done to make it possible for users to set it in their init file and then
;;; load this file.  The value set in the init file will override the value
;;; here.
;;;
;;; ***                                                                   ***

(defvar *pcl-directory* (pathname "target:pcl/"))

(defsystem pcl	   
           *pcl-directory*
  ;;
  ;; file         load           compile      files which       port
  ;;              environment    environment  force the
  ;;                                          recompilation
  ;;                                          of this file
  ;;                                          
  (
   (pkg             t            t            ())
   ;;(walk            (pkg)        (pkg)        ())
   (macros          t            t            ())
   (low             (pkg macros) t            (macros))
   ;;(info            (pkg walk)   t            (macros))
   (info            (pkg)   t            (macros))
   
   (fin         t                                   t (low))
   (defclass    t                                   t (low info))
   (defs        t                                   t (defclass macros))
   (fngen       t                                   t (low))
   (cache       t                                   t (low defs))
   (dlisp       t                                   t (defs low fin cache))
   (dlisp2      t                                   t (low fin cache dlisp))
   (boot        t                                   t (defs fin))
   (vector t t (boot defs cache fin))
   (method-slot-access-optimization t t (vector))
   (gf-call-optimization t t (vector))
   (slots-boot  t                                   t (vector boot defs cache fin))
   (combin      t                                   t (boot defs))
   (dfun        t                                   t (boot low cache))
   (ctor t t (boot low))
   (braid       (+ precom2)		            t (boot defs low fin cache))
   (dlisp3      t                                   t (dlisp2 boot braid))
   (generic-functions t                             t (boot))
   (slots       t                                   t (vector boot defs low cache fin))
   (init t t (vector boot defs low ctor))
   (seal        t                                   t (info generic-functions slots))
   (std-class   t                                   t (vector boot defs low cache fin slots seal))
   (cpl         t                                   t (vector boot defs low cache fin slots))
   (fsc         t                                   t (defclass boot defs low fin cache))
   (methods     t                                   t (defclass boot defs low fin cache))
   (fixup       t                                   t (boot defs low fin))
   (defcombin   t                                   t (defclass boot defs low fin))
   (ctypes      t                                   t (defclass defcombin))
   (env         t                                   t (defclass boot defs low fin))
   (cmucl-documentation t			    t ())
   (precom2     (dlisp)         t (defs low cache fin dfun cmucl-documentation))
   ))

(defsystem simple-streams
           (merge-pathnames "simple-streams/" *pcl-directory*)
  ;;
  ;; file         load           compile      files which       port
  ;;              environment    environment  force the
  ;;                                          recompilation
  ;;                                          of this file
  ;;                                          
  ((herald        t           ()               ()               CMU)
   (iodefs        t           ()               ()               CMU)
   (classes       t           (iodefs)         ()               CMU)
   (internal      t           (classes)        ()               CMU)
   (strategy      t           (internal)       ()               CMU)
   (impl          t           (strategy)       ()               CMU)
   (null          t           (classes)        (classes)        CMU)
   (direct        t           (classes)        (classes)        CMU)
   (file          t           (classes direct) (classes direct) CMU)
   (string        t           (classes)        (classes)        CMU)
   (terminal      t           (classes)        (classes)        CMU)
   (socket        t           (classes)        (classes)        CMU)

   (gray-compat   t           ()               ()               CMU)))

(defsystem gray-streams
           *pcl-directory*
  ;;
  ;; file         load           compile      files which       port
  ;;              environment    environment  force the
  ;;                                          recompilation
  ;;                                          of this file
  ;;                                          
  (
   (gray-streams-class  t            t            ()                   CMU)
   (gray-streams        t            t            (gray-streams-class) CMU)
   ))

(defun compile-pcl (&optional m)
  (cond ((null m)
	 (operate-on-system 'pcl :compile)
	 (operate-on-system 'simple-streams :compile)
	 (operate-on-system 'gray-streams :compile))
	((eq m :print)
	 (operate-on-system 'pcl :compile () t))
	((eq m :query)
	 (operate-on-system 'pcl :query-compile))
	((eq m :confirm)
	 (operate-on-system 'pcl :confirm-compile))
	((eq m t)
	 (operate-on-system 'pcl :recompile)
	 (operate-on-system 'simple-streams :recompile)
	 (operate-on-system 'gray-streams :recompile))
	((listp m)
	 (operate-on-system 'pcl :compile-from m))
	((symbolp m)
	 (operate-on-system 'pcl :recompile-some `(,m)))))

(defun load-pcl (&optional m)
  (cond ((null m)
	 (operate-on-system 'pcl :load))
	((eq m :query)
	 (operate-on-system 'pcl :query-load))))

(defun bug-report-info (&optional (stream *standard-output*))
  (format stream "~&PCL system date: ~A~
                  ~&Lisp Implementation type: ~A~
                  ~&Lisp Implementation version: ~A~
                  ~&*features*: ~S"
	  *pcl-system-date*
	  (lisp-implementation-type)
	  (lisp-implementation-version)
	  *features*))
