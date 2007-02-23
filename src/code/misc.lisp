;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/misc.lisp,v 1.35 2005/07/13 12:43:58 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Environment query functions, documentation and dribble.
;;;
;;; Written and maintained mostly by Skef Wholey and Rob MacLachlan.
;;; Scott Fahlman, Dan Aronson, and Steve Handerson did stuff here, too.
;;;
(in-package "LISP")
(export '(documentation *features* variable room
	  lisp-implementation-type lisp-implementation-version machine-type
	  machine-version machine-instance software-type software-version
	  short-site-name long-site-name dribble compiler-macro))

(in-package "SYSTEM")
(export '(*software-type* *short-site-name* *long-site-name*))

(in-package "EXT")
(export 'featurep)

(in-package "LISP")

;;; cobbled from stuff in describe.lisp.
(defun function-doc (x)
  (let ((name
	 (case (kernel:get-type x)
	   (#.vm:closure-header-type
	    (kernel:%function-name (%closure-function x)))
	   ((#.vm:function-header-type #.vm:closure-function-header-type)
	    (kernel:%function-name x))
	   (#.vm:funcallable-instance-header-type
	    (typecase x
	      (kernel:byte-function
	       (c::byte-function-name x))
	      (kernel:byte-closure
	       (c::byte-function-name (byte-closure-function x)))
	      (eval:interpreted-function
	       (multiple-value-bind 
		     (exp closure-p dname)
		   (eval:interpreted-function-lambda-expression x)
		 (declare (ignore exp closure-p))
		 dname))
	      (t ;; funcallable-instance
	       (kernel:%function-name
		(kernel:funcallable-instance-function x))))))))
    (when (and name (typep name '(or symbol cons)))
      (values (info function documentation name)))))

(defun documentation (x doc-type)
  "Returns the documentation string of Doc-Type for X, or NIL if
  none exists.  System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  SETF, and T."
  (flet (;; CMUCL random-documentation.
	 (try-cmucl-random-doc (x doc-type)
	   (declare (symbol doc-type))
	   (cdr (assoc doc-type
		       (values (info random-documentation stuff x))))))
    (case doc-type
      (variable 
       (typecase x
	 (symbol (values (info variable documentation x)))))
      (function
       (typecase x
	 (symbol (values (info function documentation x)))
	 (function (function-doc x))
	 (list ;; Must be '(setf symbol)
	  (values (info function documentation (cadr x))))))
      (structure
       (typecase x
	 (symbol (when (eq (info type kind x) :instance)
		   (values (info type documentation x))))))
      (type
       (typecase x
	 (kernel::structure-class (values (info type documentation (%class-name x))))
	 (t (and (typep x 'symbol) (values (info type documentation x))))))
      (setf (info setf documentation x))
      ((t)
       (typecase x
	 (function (function-doc x))
	 (package (package-doc-string x))
	 (kernel::structure-class (values (info type documentation (%class-name x))))
	 (symbol (try-cmucl-random-doc x doc-type))))
      (t
       (typecase x
	 (symbol (try-cmucl-random-doc x doc-type)))))))

(defun (setf documentation) (string name doc-type)
  #-no-docstrings
  (case doc-type
    (variable (setf (info variable documentation name) string))
    (function (setf (info function documentation name) string))
    (structure
     (unless (eq (info type kind name) :instance)
       (error "~S is not the name of a structure type." name))
     (setf (info type documentation name) string))
    (type (setf (info type documentation name) string))
    (setf (setf (info setf documentation name) string))
    (t
     (let ((pair (assoc doc-type (info random-documentation stuff name))))
       (if pair
	   (setf (cdr pair) string)
	   (push (cons doc-type string)
		 (info random-documentation stuff name))))))
  string)


#+nil
(defvar *features* '(:common :common-lisp :ansi-cl :ieee-floating-point :cmu)
  "Holds a list of symbols that describe features provided by the
   implementation.")

;;; Register various Lisp features
#+i486
(sys:register-lisp-runtime-feature :i486)

#+pentium
(sys:register-lisp-runtime-feature :pentium)

#+sparc-v7
(sys:register-lisp-runtime-feature :sparc-v7)

#+sparc-v8
(sys:register-lisp-runtime-feature :sparc-v8)

#+sparc-v9
(sys:register-lisp-runtime-feature :sparc-v9)

#+complex-fp-vops
(sys:register-lisp-feature :complex-fp-vops)

(defun featurep (x)
  "If X is an atom, see if it is present in *FEATURES*.  Also
  handle arbitrary combinations of atoms using NOT, AND, OR."
  (if (consp x)
      (case (car x)
	((:not not) (not (featurep (cadr x))))
	((:and and) (every #'featurep (cdr x)))
	((:or or) (some #'featurep (cdr x)))
	(t
	 (error "Unknown operator in feature expression: ~S." x)))
      (not (null (memq x *features*)))))


;;; Other Environment Inquiries.

(defun lisp-implementation-type ()
  "Returns a string describing the implementation type."
  "CMU Common Lisp")

(defun lisp-implementation-version ()
  "Returns a string describing the implementation version."
  (format nil "~A (~X)" *lisp-implementation-version* c:byte-fasl-file-version))

(defun machine-instance ()
  "Returns a string giving the name of the local machine."
  (unix:unix-gethostname))

(defvar *software-type* "Unix"
  "The value of SOFTWARE-TYPE.  Set in FOO-os.lisp.")

(defun software-type ()
  "Returns a string describing the supporting software."
  *software-type*)

(defvar *short-site-name* "Unknown"
  "The value of SHORT-SITE-NAME.  Set in library:site-init.lisp.")

(defun short-site-name ()
  "Returns a string with the abbreviated site name."
  *short-site-name*)

(defvar *long-site-name* "Site name not initialized"
  "The value of LONG-SITE-NAME.  Set in library:site-init.lisp.")

(defun long-site-name ()
  "Returns a string with the long form of the site name."
  *long-site-name*)


;;;; Dribble stuff:

;;; Each time we start dribbling to a new stream, we put it in
;;; *dribble-stream*, and push a list of *dribble-stream*, *standard-input*,
;;; *standard-output* and *error-output* in *previous-streams*.
;;; *standard-output* and *error-output* is changed to a broadcast stream that
;;; broadcasts to *dribble-stream* and to the old values of the variables.
;;; *standard-input* is changed to an echo stream that echos input from the old
;;; value of standard input to *dribble-stream*.
;;;
;;; When dribble is called with no arguments, *dribble-stream* is closed,
;;; and the values of *dribble-stream*, *standard-input*, and
;;; *standard-output* are poped from *previous-streams*.

(defvar *previous-streams* nil)
(defvar *dribble-stream* nil)

(defun dribble (&optional pathname &key (if-exists :append))
  "With a file name as an argument, dribble opens the file and
   sends a record of further I/O to that file.  Without an
   argument, it closes the dribble file, and quits logging."
  (cond (pathname
	 (let* ((new-dribble-stream
		 (open pathname :direction :output :if-exists if-exists
		       :if-does-not-exist :create))
		(new-standard-output
		 (make-broadcast-stream *standard-output* new-dribble-stream))
		(new-error-output
		 (make-broadcast-stream *error-output* new-dribble-stream))
		(new-standard-input
		 (make-echo-stream *standard-input* new-dribble-stream)))
	   (push (list *dribble-stream* *standard-input* *standard-output*
		       *error-output*)
		 *previous-streams*)
	   (setf *dribble-stream* new-dribble-stream)
	   (setf *standard-input* new-standard-input)
	   (setf *standard-output* new-standard-output)
	   (setf *error-output* new-error-output)))
	((null *dribble-stream*)
	 (error "Not currently dribbling."))
	(t
	 (let ((old-streams (pop *previous-streams*)))
	   (close *dribble-stream*)
	   (setf *dribble-stream* (first old-streams))
	   (setf *standard-input* (second old-streams))
	   (setf *standard-output* (third old-streams))
	   (setf *error-output* (fourth old-streams)))))
  (values))

(defun ed (&optional x)
  "Default implementation of ed.  This does nothing.  If hemlock is
  loaded, ed can be used to edit a file"
  (values))
