;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/tools/setup.lisp,v 1.40 2004/07/25 18:25:16 pmai Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Set up package environment and search lists for compiler.  Also some
;;; compilation utilities.
;;;

(when (boundp 'conditions::*make-condition-accessor-methods*)
  (setq conditions::*make-condition-accessor-methods* nil))

;;; Ensure pre-ANSI defstruct processing occurs during system builds.
(in-package "KERNEL")
(defparameter *ansi-defstruct-options-p* nil)


;; Disable package locks while rebuilding CMUCL. This variable is
;; enabled upon startup from the function PACKAGE-LOCKS-INIT.
(in-package "LISP")
(defparameter *enable-package-locked-errors* nil)
(ext:unlock-all-packages)

(define-condition genesis-c-header-file-changed (warning)
  ((name :initarg :name :reader genesis-c-header-file-name))
  (:report
   (lambda (c s)
     (format s "The C header file ~S has changed.~%~
                Be sure to re-compile the startup code."
             (genesis-c-header-file-name c)))))

(in-package "CL-USER")


;; these forward declarations are only intended to avoid compiler
;; warnings about undefined functions when building CMUCL.
(proclaim '(ftype (function * *)
            c::assemble-file
            pcl::class-direct-subclasses
            pcl::class-direct-superclasses
            pcl::specializer-direct-methods
            pcl::class-slots
            pcl::slot-boundp-using-class
            pcl::slot-definition-name
            pcl::slot-value-using-class
            ;; Simple-stream functions, that are provided when building PCL
            stream:device-close
            stream::%charpos
            stream::%clear-input
            stream::%clear-output
            stream::%file-length
            stream::%file-name
            stream::%file-position
            stream::%file-rename
            stream::%file-string-length
            stream::%finish-output
            stream::%force-output
            stream::%fresh-line
            stream::%input-stream-p
            stream::%interactive-stream-n
            stream::%interactive-stream-p
            stream::%interactive-stream-y
            stream::%line-length
            stream::%listen
            stream::%open-stream-p
            stream::%output-stream-p
            stream::%peek-char
            stream::%read-byte
            stream::%read-char
            stream::%read-line
            stream::%read-sequence
            stream::%read-vector
            stream::%stream-external-format
            stream::%unread-char
            stream::%write-char
            stream::%write-byte
            stream::%write-sequence
            stream::%write-string
            debug::all-method-functions-in-package
            profile::reinitialize-method-function
            lisp::make-instance
            lisp::class-of
            lisp::sxhash-instance
            hemlock::ts-stream-p
            hemlock::ts-stream-wire
            ext::call-display-event-handler
            ext::disable-clx-event-handling
            ext::flush-display-events
            xlib::display-input-stream
            xlib::event-listen))



;;; DUMP-PACKAGE-STATE  --  Public
;;;
(defun dump-package-state (packages file)
  (declare (type (or list package symbol string) packages)
	   (type (or pathname symbol string) file))
  (let* ((packages (lisp::package-listify packages)))
    (collect ((forms))
      (dolist (pkg packages)
	(let ((nicks (package-nicknames pkg))
	      (name (package-name pkg))
	      (shad (package-shadowing-symbols pkg)))
	  (forms `(if (find-package ,name)
		      (rename-package ,name ,name ',nicks)
		      (make-package ,name :nicknames ',nicks :use nil)))
	  (when shad
	    (forms `(shadow ',(mapcar #'string shad) ,name)))))

      (dolist (pkg packages)
	(forms `(use-package ',(mapcar #'package-name
				       (package-use-list pkg))
			     ,(package-name pkg))))

      (dolist (old packages)
	(collect ((exports))
	  (let ((imports (make-hash-table :test #'eq)))
	    (do-symbols (sym old)
	      (let ((pkg (symbol-package sym))
		    (name (symbol-name sym)))
		(multiple-value-bind (found how)
				     (find-symbol name old)
		  (assert (and (eq found sym) how))
		  (cond
		   ((not pkg)
		    (warn "Not dumping uninterned symbol ~S." sym))
		   ((eq how :inherited))
		   (t
		    (unless (eq pkg old)
		      (pushnew name (gethash pkg imports) :test #'string=))
		    (when (eq how :external)
		      (exports name)))))))
	    (collect ((import-froms))
	      (maphash #'(lambda (pkg raw-names)
			   (let ((names (sort (delete-duplicates raw-names
								 :test
								 #'string=)
					      #'string<))
				 (pkg-name (package-name pkg)))
			     (when names
			       (import-froms `(:import-from ,pkg-name ,@names))
			       (dolist (name names)
				 (forms `(intern ,name ,pkg-name))))))
		       imports)
	      (forms `(defpackage ,(package-name old)
			,@(import-froms)
			,@(when (exports)
			    `((:export
			       ,@(sort (delete-duplicates (exports)
							  :test #'string=)
				       #'string<))))))))))

      (with-open-file (s file :direction :output :if-exists :rename-and-delete)
	(dolist (form (forms))
	  (write form :stream s :pretty t)
	  (terpri s)))))

  (values))
  

;;; COPY-PACKAGES  --  Public
;;;
(defun copy-packages (packages)
  "Rename all the of the Named packages to OLD-Name, and then create new
  packages for each name that have the same names, nicknames, imports, shadows
  and exports.  If any of the OLD-Name packages already exist, then we quietly
  do nothing."
  (let* ((packages (lisp::package-listify packages))
	 (names (mapcar #'package-name packages))
	 (new-names (mapcar #'(lambda (x)
				(concatenate 'string "OLD-" x))
			    names)))
    (unless (some #'find-package new-names)
      (collect ((new-packages))
	(flet ((trans-pkg (x)
		 (or (cdr (assoc x (new-packages))) x)))
	  (loop for pkg in packages and new in new-names do
	    (let ((nicks (package-nicknames pkg))
		  (name (package-name pkg)))
	      (rename-package pkg new)
	      (let ((new-pkg (make-package name :nicknames nicks :use nil))
		    (shad (package-shadowing-symbols pkg)))
		(when shad
		  (shadow shad new-pkg))
		(new-packages (cons pkg new-pkg)))))
	  
	  (loop for (old . new) in (new-packages) do
	    (dolist (use (package-use-list old))
	      (use-package (trans-pkg use) new)))
	  
	  (loop for (old . new) in (new-packages) do
	    (do-symbols (sym old)
	      (let ((pkg (symbol-package sym))
		    (name (symbol-name sym)))
		(multiple-value-bind (found how)
				     (find-symbol name old)
		  (assert (and (eq found sym) how))
		  (cond
		   ((not pkg)
		    (warn "Not copying uninterned symbol ~S." sym))
		   ((or (eq how :inherited)
			(and (eq how :internal) (eq pkg old))))
		   (t
		    (let* ((npkg (trans-pkg pkg))
			   (nsym (intern name npkg)))
		      (multiple-value-bind (ignore new-how)
					   (find-symbol name new)
			(declare (ignore ignore))
			(unless new-how (import nsym new)))
		      (when (eq how :external)
			(export nsym new)))))))))))))
  (values))


;;;; Compile utility:

;;; Switches:
;;;
(defvar *interactive* t) ; Batch compilation mode?

(defvar *log-file* nil)
(defvar *last-file-position*)

(defmacro with-compiler-log-file ((name &rest wcu-keys) &body forms)
  `(if *interactive*
       (with-compilation-unit (,@wcu-keys)
	 ,@forms)
       (let ((*log-file* (open ,name :direction :output
			       :if-exists :append
			       :if-does-not-exist :create)))
	 (unwind-protect
	     (let ((*error-output* *log-file*)
		   (*last-file-position* (file-position *log-file*)))
	       (with-compilation-unit (,@wcu-keys)
		 ,@forms))
	   (close *log-file*)))))


(defun comf (name &rest keys &key proceed assem &allow-other-keys)
  (when (and *log-file*
	     (> (- (file-position *log-file*) *last-file-position*) 10000))
    (setq *last-file-position* (file-position *log-file*))
    (force-output *log-file*))

  (let* ((src (merge-pathnames name (make-pathname :type "lisp")))
	 (obj (if assem
		  (make-pathname :defaults src :type "assem")
		  (apply #'compile-file-pathname src keys))))

    (unless (and (probe-file obj)
		 (>= (file-write-date obj) (file-write-date src)))
      (write-line (namestring name))
      (format *error-output* "~2&Start time: ~A, compiling ~A.~%"
	      (ext:format-universal-time nil (get-universal-time))
	      name)
      (catch 'blow-this-file
	(with-simple-restart
	    (continue "Blow this file")
	  (cond
	   (*interactive*
	    (if assem
		(c::assemble-file src :output-file obj)
		(apply #'compile-file src :allow-other-keys t keys)))
	   (t
	    (handler-bind
		((error #'(lambda (condition)
			    (unless (typep condition 'c::compiler-error)
			      (format *error-output* "~2&~A~2&"
				      condition)
			      (when proceed
				(format *error-output* "Proceeding...~%")
				(continue))
			      (format *error-output* "Aborting...~%")
			      (handler-case
				  (let ((*debug-io* *error-output*))
				    (debug:backtrace))
				(error (condition)
				       (declare (ignore condition))
				       (format t "Error in backtrace!~%")))
			      (format t "Error abort.~%")
			      (return-from comf)))))
	      (if assem
		  (c::assemble-file src :output-file obj)
		  (apply #'compile-file src :allow-other-keys t keys))))))))))


;;; VMDIR  --  Interface
;;;
(defun vmdir (f)
  (merge-pathnames
   (make-pathname :directory nil :defaults f)
   (merge-pathnames
    (cond ((c:target-featurep :pmax) "mips/")
	  ((c:target-featurep :rt) "rt/")
	  ((c:target-featurep :hppa) "hppa/")
	  ((c:target-featurep :sparc) "sparc/")
	  ((c:target-featurep :x86) "x86/")
	  ((c:target-featurep :alpha) "alpha/")
	  ((c:target-featurep :sgi) "mips/")
	  ((c:target-featurep :ppc) "ppc/")
	  ((c:target-featurep :amd64) "amd64/")
	  (t
	   (error "What machine is this?")))
    (make-pathname :directory (pathname-directory f)))))


;;; CAT-IF-ANYTHING-CHANGED

(defun cat-if-anything-changed (output-file &rest input-files)
  (flet ((add-correct-type (pathname)
	   (or (probe-file
		(make-pathname :type (c:backend-byte-fasl-file-type
				      c:*target-backend*)
			       :defaults pathname))
	       (make-pathname :type (c:backend-fasl-file-type
				     c:*target-backend*)
			      :defaults pathname))))
    (let* ((output-file (add-correct-type output-file))
	   (write-date (file-write-date output-file))
	   (input-namestrings
	    (mapcar #'(lambda (file)
			(let ((file (add-correct-type file)))
			  (let ((src-write-date (file-write-date file)))
			    (unless src-write-date
			      (error "Missing file: ~S" file))
			    (when (and write-date
				       (> src-write-date write-date))
			      (setf write-date nil)))
			  (unix-namestring file)))
		    input-files)))
      (cond ((null write-date)
	     (format t "~S out of date.~%" (namestring output-file))
	     (run-program "/bin/cat" input-namestrings
			  :output output-file
			  :if-output-exists :supersede
			  :error t))
	    (t
	     (format t "~S up to date.~%" (namestring output-file)))))))

