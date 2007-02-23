;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/load.lisp,v 1.91 2006/06/30 18:41:22 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Loader for CMUCL.
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package "LISP")
(export '(load *load-verbose* *load-print* *load-truename* *load-pathname*))

(in-package "EXTENSIONS")
(export '(*load-if-source-newer* *load-source-types* *load-object-types*
	  invalid-fasl))

(in-package "SYSTEM")
(export '(foreign-symbol-address alternate-get-global-address))

(in-package "LISP")


;;;; Variables:

;;; Public:

(defvar *load-if-source-newer* :load-object
  "The default for the :IF-SOURCE-NEWER argument to load.")

(declaim (type (member :load-object :load-source :query :compile)
	       *load-if-source-newer*))

(defvar *load-source-types* '("lisp" "l" "cl" "lsp")
  "The source file types which LOAD recognizes.")

(defvar *load-object-types*
  '(#.(c:backend-fasl-file-type c:*backend*)
    #.(c:backend-byte-fasl-file-type c:*backend*)
    "fasl")
  "A list of the object file types recognized by LOAD.")

(defvar *load-lp-object-types*
  '(#.(string-upcase (c:backend-fasl-file-type c:*backend*))
    #.(string-upcase (c:backend-byte-fasl-file-type c:*backend*))
    "FASL")
  "A list of the object file types recognized by LOAD for logical pathnames.")

(declaim (list *load-source-types* *load-object-types* *load-lp-object-types*))

(defvar *load-verbose* t
  "The default for the :VERBOSE argument to Load.")

(defvar *load-print* ()
  "The default for the :PRINT argument to Load.")

(defvar *load-truename* nil
  "The TRUENAME of the file that LOAD is currently loading.")

(defvar *load-pathname* nil
  "The defaulted pathname that LOAD is currently loading.")

(declaim (type (or pathname null) *load-truename* *load-pathname*)) 


;;; Internal state variables:

(defvar *load-depth* 0
  "Count of the number of recursive loads.")
(declaim (type index *load-depth*))
(defvar *fasl-file*)
(declaim (type lisp-stream *fasl-file*))

;; If non-nil, don't check that FASL files loaded into core have the
;; same version number as the current backend. This is used when
;; rebuilding CMUCL after having incremented the FASL-FILE-VERSION
;; parameter.
(defvar *skip-fasl-file-version-check* nil)

(define-condition invalid-fasl (error)
  ((filename :reader invalid-fasl-pathname :initarg :file)
   (fasl-version :reader invalid-fasl-version :initarg :fasl-version)
   (expected-version :reader invalid-fasl-expected-version :initarg :expected-version))
  (:report
   (lambda (condition stream)
     (format stream "~A was compiled for fasl-file version ~X, ~
                     but this is version ~X"
	     (invalid-fasl-pathname condition)
	     (invalid-fasl-version condition)
	     (invalid-fasl-expected-version condition)))))

;;; LOAD-FRESH-LINE -- internal.
;;;
;;; Output the corrent number of semicolons after a fresh-line.
;;; 
(defconstant semicolons ";;;;;;;;;;;;;;;;")
;;;
(defun load-fresh-line ()
  (fresh-line)
  (do ((count *load-depth* (- count (length semicolons))))
      ((< count (length semicolons))
       (unless (zerop count)
	 (write-string semicolons *standard-output* :end count)))
    (declare (fixnum count))
    (write-string semicolons))
  (write-char #\space))


;;;; The Fop-Table:
;;;
;;;    The table is implemented as a simple-vector indexed by the table
;;; offset.  We may need to have several, since load can be called recursively.

(defvar *free-fop-tables* (list (make-array 1000))
  "List of free fop tables for the fasloader.")

;;; The current fop table.
(defvar *current-fop-table*)
(declaim (simple-vector *current-fop-table*))

;;; The length of the current fop table.
(defvar *current-fop-table-size*)
(declaim (type index *current-fop-table-size*))

;;; Index in the fop-table of the next entry to be used.
(defvar *current-fop-table-index*)
(declaim (type index *current-fop-table-index*))

(defun grow-fop-table ()
  (let* ((new-size (* *current-fop-table-size* 2))
	 (new-table (make-array new-size)))
    (declare (fixnum new-size) (simple-vector new-table))
    (replace new-table (the simple-vector *current-fop-table*))
    (setq *current-fop-table* new-table)
    (setq *current-fop-table-size* new-size)))

(defmacro push-table (thing)
  (let ((n-index (gensym)))
    `(let ((,n-index *current-fop-table-index*))
       (declare (fixnum ,n-index))
       (when (= ,n-index (the fixnum *current-fop-table-size*))
	 (grow-fop-table))
       (setq *current-fop-table-index* (1+ ,n-index))
       (setf (svref *current-fop-table* ,n-index) ,thing))))

;;;; The Fop-Stack:
;;;
;;;  The is also in a simple-vector, but it grows down, since it is somewhat 
;;; cheaper to test for overflow that way.
;;;
(defvar *fop-stack* (make-array 100)
  "The fop stack (we only need one!).")
(declaim (simple-vector *fop-stack*))

;;; The index of the most recently pushed item on the fop-stack.
(defvar *fop-stack-pointer* 100)

;;; The current index into the fop stack when we last recursively entered
;;; LOAD.
(defvar *fop-stack-pointer-on-entry*)
(declaim (type index *fop-stack-pointer* *fop-stack-pointer-on-entry*))

(defun grow-fop-stack ()
  (let* ((size (length (the simple-vector *fop-stack*)))
	 (new-size (* size 2))
	 (new-stack (make-array new-size)))
    (declare (fixnum size new-size) (simple-vector new-stack))
    (replace new-stack (the simple-vector *fop-stack*) :start1 size)
    (incf *fop-stack-pointer-on-entry* size)
    (setq *fop-stack-pointer* size)
    (setq *fop-stack* new-stack)))

;;; With-Fop-Stack  --  Internal
;;;
;;;    Cache information about the fop-stack in local variables.  Define
;;; a local macro to pop from the stack.  Push the result of evaluation if
;;; specified.
;;;
(defmacro with-fop-stack (pushp &body forms)
  (let ((n-stack (gensym))
	(n-index (gensym))
	(n-res (gensym)))
    `(let ((,n-stack *fop-stack*)
	   (,n-index *fop-stack-pointer*))
       (declare (simple-vector ,n-stack) (type index ,n-index))
       (macrolet ((pop-stack ()
		    `(prog1
		      (svref ,',n-stack ,',n-index)
		      (setq ,',n-index (1+ ,',n-index))))
		  (call-with-popped-things (fun n)
		    (let ((n-start (gensym)))
		      `(let ((,n-start (+ ,',n-index ,n)))
			 (declare (type index ,n-start))
			 (setq ,',n-index ,n-start)
			 (,fun ,@(make-list n :initial-element
					    `(svref ,',n-stack
						    (decf ,n-start))))))))
	 ,(if pushp
	      `(let ((,n-res (progn ,@forms)))
		 (when (zerop ,n-index)
		   (grow-fop-stack)
		   (setq ,n-index *fop-stack-pointer*
			 ,n-stack *fop-stack*))
		 (decf ,n-index)
		 (setq *fop-stack-pointer* ,n-index)
		 (setf (svref ,n-stack ,n-index) ,n-res))
	      `(prog1
		(progn ,@forms)
		(setq *fop-stack-pointer* ,n-index)))))))

;;; FOP database:

(defvar fop-codes (make-array 256)
  "Vector indexed by a FaslOP that yields the FOP's name.")

(defvar fop-functions
  (make-array 256 :initial-element #'(lambda () (error "Losing FOP!")))
  "Vector indexed by a FaslOP that yields a function of 0 arguments which
  will perform the operation.")

(declaim (simple-vector fop-codes fop-functions))

;;; Define-FOP  --  Internal
;;;
;;;    Defines Name as a fasl operation, with op-code op.  If pushp is :nope,
;;; the body neither pushes or pops the fop stack.  If it is nil, then
;;; the body may pop, but the result is ignored.  If it is true, the result
;;; is pushed on the stack.
;;;
(defmacro define-fop ((name op &optional (pushp t)) &rest forms)
  `(progn
    (defun ,name ()
      ,(if (eq pushp :nope)
	   `(progn ,@forms)
	   `(with-fop-stack ,pushp ,@forms)))
    (setf (svref fop-codes ,op) ',name)
    (setf (get ',name 'fop-code) ,op)
    (setf (svref fop-functions ,op) #',name)))

;;; Clone-Fop  --  Internal
;;;
;;;    Defines a pair of fops which are identical except in that one reads
;;; a four byte argument and the other reads a one byte argument.  The
;;; argument can be accessed by using the Clone-Arg macro.
;;;
(defmacro clone-fop ((name op &optional (pushp t))
		     (small-name small-op) &rest forms)
  `(progn
     (macrolet ((clone-arg () '(read-arg 4)))
       (define-fop (,name ,op ,pushp) ,@forms))
     (macrolet ((clone-arg () '(read-arg 1)))
       (define-fop (,small-name ,small-op ,pushp) ,@forms))))

;;;; Utilities for reading from the fasl file.

(declaim (inline read-byte))

;;; Fast-Read-U-Integer  --  Internal
;;;
;;;    Expands into code to read an N-byte unsigned integer using
;;; fast-read-byte.
;;;
(defmacro fast-read-u-integer (n)
  (declare (optimize (speed 0)))
  (do ((res '(fast-read-byte)
	    `(logior (fast-read-byte)
		     (ash ,res 8)))
       (cnt 1 (1+ cnt)))
      ((>= cnt n) res)))

;;; Fast-Read-Variable-U-Integer  --  Internal
;;;
;;;    Like Fast-Read-U-Integer, but the size may be determined at run time.
;;;
(defmacro fast-read-variable-u-integer (n)
  (let ((n-pos (gensym))
	(n-res (gensym))
	(n-cnt (gensym)))
    `(do ((,n-pos 8 (+ ,n-pos 8))
	  (,n-cnt (1- ,n) (1- ,n-cnt))
	  (,n-res
	   (fast-read-byte)
	   (dpb (fast-read-byte) (byte 8 ,n-pos) ,n-res)))
	 ((zerop ,n-cnt) ,n-res)
       (declare (type index ,n-pos ,n-cnt)))))

;;; Fast-Read-S-Integer  --  Internal
;;;
;;;    Read a signed integer.
;;;
(defmacro fast-read-s-integer (n)
  (declare (optimize (speed 0)))
  (let ((n-last (gensym)))
    (do ((res `(let ((,n-last (fast-read-byte)))
		 (if (zerop (logand ,n-last #x80))
		     ,n-last
		     (logior ,n-last #x-100)))
	      `(logior (fast-read-byte)
		       (ash (the (signed-byte ,(* cnt 8)) ,res) 8)))
	 (cnt 1 (1+ cnt)))
	((>= cnt n) res))))

;;; Read-Arg  --  Internal
;;;
;;;    Read an N-byte unsigned integer from the *fasl-file*
;;;
(defmacro read-arg (n)
  (declare (optimize (speed 0)))
  (if (= n 1)
      `(the (unsigned-byte 8) (read-byte *fasl-file*))
      `(prepare-for-fast-read-byte *fasl-file*
	 (prog1
	  (fast-read-u-integer ,n)
	  (done-with-fast-read-byte)))))

;;; Fasload:

(defun do-load-verbose (stream)
  (when *load-verbose*
    (load-fresh-line)
    (let ((name (file-name stream)))
      (if name
	  (format t "Loading ~S.~%" name)
	  (format t "Loading stuff from ~S.~%" stream)))))

(defun fasload (stream)
  (when (zerop (file-length stream))
    (error "Attempt to load an empty FASL FILE:~%  ~S" (namestring stream)))
  (do-load-verbose stream)
  (let* ((*fasl-file* stream)
	 (*current-fop-table* (or (pop *free-fop-tables*) (make-array 1000)))
	 (*current-fop-table-size* (length *current-fop-table*))
	 (*fop-stack-pointer-on-entry* *fop-stack-pointer*))
    (unwind-protect 
      (do ((loaded-group (load-group stream) (load-group stream)))
	  ((not loaded-group)))
      (setq *fop-stack-pointer* *fop-stack-pointer-on-entry*)
      (push *current-fop-table* *free-fop-tables*)
      ;;
      ;; Nil out the stack and table, so we don't hold onto garbage.
      (dotimes (i *fop-stack-pointer-on-entry*)
	(declare (fixnum i))
	(setf (svref *fop-stack* i) nil))
      (let ((tab *current-fop-table*))
	(dotimes (i (length tab))
	  (declare (fixnum i))
	  (setf (svref tab i) nil)))))
  t)

#|

(defvar *fop-counts* (make-array 256 :initial-element 0))
(defvar *fop-times* (make-array 256 :initial-element 0))
(defvar *print-fops* nil)

(defun clear-counts ()
  (fill (the simple-vector *fop-counts*) 0)
  (fill (the simple-vector *fop-times*) 0)
  t)

(defun analyze-counts ()
  (let ((counts ())
	(total-count 0)
	(times ())
	(total-time 0))
    (macrolet ((breakdown (lvar tvar vec)
		 `(progn
		   (dotimes (i 255)
		     (declare (fixnum i))
		     (let ((n (svref ,vec i)))
		       (push (cons (svref fop-codes i) n) ,lvar)
		       (incf ,tvar n)))
		   (setq ,lvar (subseq (sort ,lvar #'(lambda (x y)
						       (> (cdr x) (cdr y))))
				       0 10)))))
		 
      (breakdown counts total-count *fop-counts*)
      (breakdown times total-time *fop-times*)
      (format t "Total fop count is ~D~%" total-count)
      (dolist (c counts)
	(format t "~30S: ~4D~%" (car c) (cdr c)))
      (format t "~%Total fop time is ~D~%" (/ (float total-time) 60.0))
      (dolist (m times)
	(format t "~30S: ~6,2F~%" (car m) (/ (float (cdr m)) 60.0))))))
|#

;;; Load-Group  --  Internal
;;;
;;; Load-Group returns t if it successfully loads a group from the file,
;;; or () if EOF was encountered while trying to read from the file.
;;; Dispatch to the right function for each fop.  Special-case fop-byte-push
;;; since it is real common.
;;;
(defun load-group (file)
  (when (check-header file)
    (catch 'group-end
      (let ((*current-fop-table-index* 0))
	(loop
	  (let ((byte (read-byte file)))
	    #+debug ; Add :debug to *features* to get a trace of the fops.
	    (let ((ptr *fop-stack-pointer*)
		  (stack *fop-stack*))
	      (fresh-line *trace-output*)
	      (unless (= ptr (length stack))
		(write-char #\space *trace-output*)
		(prin1 (svref stack ptr) *trace-output*)
		(terpri *trace-output*))
	      (prin1 (svref fop-codes byte) *trace-output*)
	      (terpri *trace-output*))
	    (if (eql byte 3)
		(let ((index *fop-stack-pointer*))
		  (declare (type index index))
		  (when (zerop index)
		    (grow-fop-stack)
		    (setq index *fop-stack-pointer*))
		  (decf index)
		  (setq *fop-stack-pointer* index)
		  (setf (svref *fop-stack* index)
			(svref *current-fop-table* (read-byte file))))
		(funcall (the function (svref fop-functions byte))))))))))


;;; Check-Header returns t if it succesfully read a header from the file,
;;; or () if EOF was hit before anything was read.  An error is signaled
;;; if garbage is encountered.
;;;
;;; We check that the stream starts with the magic bytes "FASL FILE".
;;; The end of the header is marked by the octet #xFF. 
(defun check-header (file)
  (let ((byte (read-byte file NIL '*eof*)))
    (cond ((eq byte '*eof*) ())
	  ((eq byte (char-code #\F))
	   (do ((byte (read-byte file) (read-byte file))
		(count 1 (1+ count)))
	       ((= byte 255) t)
	     (declare (fixnum byte count))
	     (if (and (< count 9)
		      (not (eql byte (char-code (schar "FASL FILE" count)))))
		 (error "Bad FASL file format."))))
	  (t (error "Bad FASL file format.")))))


;;; Load-S-Integer loads a signed integer Length bytes long from the File.

(defun load-s-integer (length)  
  (declare (fixnum length) (optimize (inhibit-warnings 2)))
  (do* ((index length (1- index))
	(byte 0 (read-byte *fasl-file*))
	(result 0 (+ result (ash byte bits)))
	(bits 0 (+ bits 8)))
       ((= index 0)
	(if (logbitp 7 byte)	; look at sign bit
	    (- result (ash 1 bits))
	    result))
    (declare (fixnum index byte bits))))


;;; Sloload:

;;; Something not EQ to anything read from a file:

(defconstant load-eof-value '(()))

;;; Sloload loads a text file into the given Load-Package.

(defun sloload (stream)
  (do-load-verbose stream)
  (do ((sexpr (read stream nil load-eof-value)
	      (read stream nil load-eof-value)))
      ((eq sexpr load-eof-value))
    (if *load-print*
	(let ((results (multiple-value-list (eval sexpr))))
	  (load-fresh-line)
	  (format t "~{~S~^, ~}~%" results))
	(eval sexpr)))
  t)


;;; LOAD  --  Public
;;;
;;;    This function mainly sets up special bindings and then calls
;;; sub-functions.  We conditionally bind the switches with PROGV so that
;;; people can set them in their init files and have the values take effect.
;;; If the compiler is loaded, we make the compiler-policy local to LOAD by
;;; binding it to itself.
;;;
(defun load (filename &key (verbose nil verbose-p) (print nil print-p)
		      (if-source-newer nil if-source-newer-p)
		      (if-does-not-exist :error) contents
		      (external-format :default))
  "Loads the file named by Filename into the Lisp environment.  The file type
   (a.k.a extension) is defaulted if missing.  These options are defined:

   :IF-SOURCE-NEWER <keyword>
	If the file type is not specified, and both source and object files
        exist, then this argument controls which is loaded:
	    :LOAD-OBJECT - load object file (default),
	    :LOAD-SOURCE - load the source file,
	    :COMPILE - compile the source and then load the object file, or
	    :QUERY - ask the user which to load.

   :IF-DOES-NOT-EXIST {:ERROR | NIL}
       If :ERROR (the default), signal an error if the file can't be located.
       If NIL, simply return NIL (LOAD normally returns T.)

   :VERBOSE {T | NIL}
       If true (the default), print a line describing each file loaded.

   :PRINT {T | NIL}
       If true, print information about loaded values.  When loading the
       source, the result of evaluating each top-level form is printed.

   :CONTENTS {NIL | :SOURCE | :BINARY}
       Forces the input to be interpreted as a source or object file, instead
       of guessing based on the file type.  This also inhibits file type
       defaulting.  Probably only necessary if you have source files with a
       \"fasl\" type. 

   The variables *LOAD-VERBOSE*, *LOAD-PRINT* and EXT:*LOAD-IF-SOURCE-NEWER*
   determine the defaults for the corresponding keyword arguments.  These
   variables are also bound to the specified argument values, so specifying a
   keyword affects nested loads.  The variables EXT:*LOAD-SOURCE-TYPES*,
   EXT:*LOAD-OBJECT-TYPES*, and EXT:*LOAD-LP-OBJECT-TYPES* determine the file
   types that we use for defaulting when none is specified."
  (declare (type (or null (member :source :binary)) contents)
	   (ignore external-format))
  (collect ((vars)
	    (vals))
    (macrolet ((frob (wot)
		 `(when ,(concat-pnames wot '-p)
		    (vars ',(intern (format nil "*LOAD-~A*" wot)))
		    (vals ,wot))))
      (frob if-source-newer)
      (frob verbose)
      (frob print))

    (when (boundp 'c::*default-cookie*)
      (vars 'c::*default-cookie* 'c::*default-interface-cookie*)
      (vals c::*default-cookie* c::*default-interface-cookie*))

    (progv (vars) (vals)
      (let ((*package* *package*)
	    (*readtable* *readtable*)
            (*enable-package-locked-errors* *enable-package-locked-errors*)
	    (*load-depth* (1+ *load-depth*)))
	(values 
	 (with-simple-restart (continue "Return NIL from load of ~S." filename)
	   (if (streamp filename)
	       (if (or (eq contents :binary)
		       (and (null contents)
			    (equal (stream-element-type filename)
				   '(unsigned-byte 8))))
		   (fasload filename)
		   (sloload filename))
	       (let ((pn (merge-pathnames (pathname filename)
					  *default-pathname-defaults* nil)))
		 (if (wild-pathname-p pn)
		     (dolist (file (directory pn) t)
		       (internal-load pn file if-does-not-exist contents))
		     (let ((tn (probe-file pn)))
		       (if (or tn (pathname-type pn) contents)
			   (internal-load pn tn if-does-not-exist contents)
			   (internal-load-default-type
			    pn if-does-not-exist))))))))))))


;;; INTERNAL-LOAD  --  Internal
;;;
;;;    Load the stuff in a file when we have got the name.
;;;
(defun internal-load (pathname truename if-does-not-exist contents)
  (unless truename
    (return-from
     internal-load
     (ecase if-does-not-exist
       (:error
	(restart-case (error 'simple-file-error
			     :pathname pathname
			     :format-control "~S does not exist."
			     :format-arguments (list (namestring pathname)))
	  (check-again () :report "See if it exists now."
	    (load pathname))
	  (use-value () :report "Prompt for a new name."
	    (write-string "New name: " *query-io*)
	    (force-output *query-io*)
	    (load (read-line *query-io*)))))
       ((nil) nil))))

  (let ((*load-truename* truename)
	(*load-pathname* pathname))
    (case contents
      (:source
       (with-open-file (file truename
			     :direction :input
			     :if-does-not-exist if-does-not-exist)
	 (sloload file)))
      (:binary
       (with-open-file (file truename
			     :direction :input
			     :if-does-not-exist if-does-not-exist
			     :element-type '(unsigned-byte 8))
	 (fasload file)))
      (t
       (let ((first-line (with-open-file (file truename :direction :input)
			   (read-line file nil))))
	 (cond
	  ((and first-line
		(>= (length (the simple-string first-line)) 9)
		(string= first-line "FASL FILE" :end1 9))
	   (internal-load pathname truename if-does-not-exist :binary))
	  (t
	   (when (member (pathname-type truename) *load-object-types*
			 :test #'string=)
	     (cerror
	      "Load it as a source file."
	      "File has a fasl file type, but no fasl file header:~%  ~S"
	      (namestring truename)))
	   (internal-load pathname truename if-does-not-exist :source))))))))


;;; TRY-DEFAULT-TYPES  --  Internal
;;;
(defun try-default-types (pathname types lp-types)
  (flet ((frob (pathname type)
	   (let* ((pn (make-pathname :type type :defaults pathname))
		  (tn (probe-file pn)))
	     (values pn tn))))
    (dolist (type (if (logical-pathname-p pathname)
		      lp-types types)
	     (values nil nil))
      (multiple-value-bind (pn tn)
	  (frob pathname type)
	(when tn
	  (return (values pn tn)))))))

;;; INTERNAL-LOAD-DEFAULT-TYPE  --  Internal
;;;
;;;    Handle the case of INTERNAL-LOAD where the file does not exist.
;;;
(defun internal-load-default-type (pathname if-does-not-exist)
  (multiple-value-bind
      (src-pn src-tn)
      (try-default-types pathname *load-source-types* '("LISP"))
    (multiple-value-bind
	(obj-pn obj-tn)
	(try-default-types pathname *load-object-types* *load-lp-object-types*)
      (cond
       ((and obj-tn src-tn
	     (> (file-write-date src-tn) (file-write-date obj-tn)))
	(ecase *load-if-source-newer*
	  (:load-object
	   (warn "Loading object file ~A,~@
		  which is older than the presumed source:~%  ~A."
		 (namestring obj-tn) (namestring src-tn))
	   (internal-load obj-pn obj-tn if-does-not-exist :binary))
	  (:load-source
	   (warn "Loading source file ~A,~@
		  which is newer than the presumed object file:~%  ~A."
		 (namestring src-tn) (namestring obj-tn))
	   (internal-load src-pn src-tn if-does-not-exist :source))
	  (:compile
	   (let ((obj-tn (compile-file src-pn)))
	     (unless obj-tn
	       (error "Compile of source failed, cannot load object."))
	     (internal-load src-pn obj-tn :error :binary)))
	  (:query
	   (restart-case
	       (error "Object file ~A is~@
		       older than the presumed source:~%  ~A."
		      (namestring obj-tn) (namestring src-tn))
	     (continue () :report "load source file"
	       (internal-load src-pn src-tn if-does-not-exist :source))
	     (load-object () :report "load object file"
	       (internal-load src-pn obj-tn if-does-not-exist :binary))))))
       (obj-tn
	(internal-load obj-pn obj-tn if-does-not-exist :binary))
       (src-pn
	(internal-load src-pn src-tn if-does-not-exist :source))
       (t
	(internal-load pathname nil if-does-not-exist nil))))))


;;;; Actual FOP definitions:

(define-fop (fop-nop 0 :nope))
(define-fop (fop-pop 1 nil) (push-table (pop-stack)))
(define-fop (fop-pop-for-effect 65 nil) (pop-stack))
(define-fop (fop-push 2) (svref *current-fop-table* (read-arg 4)))
(define-fop (fop-byte-push 3) (svref *current-fop-table* (read-arg 1)))

(define-fop (fop-empty-list 4) ())
(define-fop (fop-truth 5) t)
(define-fop (fop-misc-trap 66)
	    (%primitive make-other-immediate-type 0 vm:unbound-marker-type))

(define-fop (fop-character 68)
  (code-char (read-arg 3)))
(define-fop (fop-short-character 69)
  (code-char (read-arg 1)))

(clone-fop (fop-struct 48)
	   (fop-small-struct 49)
  (let* ((size (clone-arg))
	 (res (%make-instance size)))
    (declare (type index size))
    (do ((n (1- size) (1- n)))
	((minusp n))
      (declare (type (integer -1 #.most-positive-fixnum) n))
      (setf (%instance-ref res n) (pop-stack)))
    res))

(define-fop (fop-layout 45)
  (let ((length (pop-stack))
	(depth (pop-stack))
	(inherits (pop-stack))
	(name (pop-stack)))
    (find-layout name length inherits depth)))

(define-fop (fop-end-group 64 :nope) (throw 'group-end t))
(define-fop (fop-end-header 255)
  (error "Fop-End-Header was executed???"))

;;; In the normal loader, we just ignore these.  Genesis overwrites
;;; fop-maybe-cold-load with something that knows when to revert to
;;; cold-loading or not.
;;; 
(define-fop (fop-normal-load 81 :nope))
(define-fop (fop-maybe-cold-load 82 :nope))

(define-fop (fop-verify-table-size 62 :nope)
  (if (/= *current-fop-table-index* (read-arg 4))
      (error "Fasl table of improper size.  Bug!")))
(define-fop (fop-verify-empty-stack 63 :nope)
  (if (/= *fop-stack-pointer* *fop-stack-pointer-on-entry*)
      (error "Fasl stack not empty.  Bug!")))

;;;; Loading symbols:

(defvar *load-symbol-buffer* (make-string 100))
(declaim (simple-string *load-symbol-buffer*))
(defvar *load-symbol-buffer-size* 100)
(declaim (type index *load-symbol-buffer-size*))
   
(macrolet ((frob (name code name-size package)
	     (let ((n-package (gensym))
		   (n-size (gensym))
		   (n-buffer (gensym)))
	       `(define-fop (,name ,code)
		  (prepare-for-fast-read-byte *fasl-file*
		    (let ((,n-package ,package)
			  (,n-size (fast-read-u-integer ,name-size)))
		      (when (> ,n-size *load-symbol-buffer-size*)
			(setq *load-symbol-buffer*
			      (make-string (setq *load-symbol-buffer-size*
						 (* ,n-size 2)))))
		      (done-with-fast-read-byte)
		      (let ((,n-buffer *load-symbol-buffer*))
			(read-n-bytes *fasl-file* ,n-buffer 0 ,n-size)
			(push-table (intern* ,n-buffer ,n-size ,n-package)))))))))
  (frob fop-symbol-save 6 4 *package*)
  (frob fop-small-symbol-save 7 1 *package*)
  (frob fop-lisp-symbol-save 75 4 *lisp-package*)
  (frob fop-lisp-small-symbol-save 76 1 *lisp-package*)
  (frob fop-keyword-symbol-save 77 4 *keyword-package*)
  (frob fop-keyword-small-symbol-save 78 1 *keyword-package*)

  (frob fop-symbol-in-package-save 8 4
    (svref *current-fop-table* (fast-read-u-integer 4)))
  (frob fop-small-symbol-in-package-save 9 1
    (svref *current-fop-table* (fast-read-u-integer 4)))
  (frob fop-symbol-in-byte-package-save 10 4
    (svref *current-fop-table* (fast-read-u-integer 1)))
  (frob fop-small-symbol-in-byte-package-save 11 1
    (svref *current-fop-table* (fast-read-u-integer 1))))

(clone-fop (fop-uninterned-symbol-save 12)
	   (fop-uninterned-small-symbol-save 13)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (read-n-bytes *fasl-file* res 0 arg)
    (push-table (make-symbol res))))

(define-fop (fop-package 14)
  (let ((name (pop-stack)))
    (or (find-package name)
	(error "The package ~S does not exist." name))))

;;;; Loading numbers:

(clone-fop (fop-integer 33)
	   (fop-small-integer 34)
  (load-s-integer (clone-arg)))

(define-fop (fop-word-integer 35)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
     (fast-read-s-integer 4)
     (done-with-fast-read-byte))))

(define-fop (fop-byte-integer 36)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
     (fast-read-s-integer 1)
     (done-with-fast-read-byte))))

(define-fop (fop-ratio 70)
  (let ((den (pop-stack)))
    (%make-ratio (pop-stack) den)))

(define-fop (fop-complex 71)
  (let ((im (pop-stack)))
    (%make-complex (pop-stack) im)))

(define-fop (fop-complex-single-float 72)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
	(complex (make-single-float (fast-read-s-integer 4))
		 (make-single-float (fast-read-s-integer 4)))
      (done-with-fast-read-byte))))

(define-fop (fop-complex-double-float 73)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
	(let* ((re-lo (fast-read-u-integer 4))
	       (re-hi (fast-read-u-integer 4))
	       (re (make-double-float re-hi re-lo))
	       (im-lo (fast-read-u-integer 4))
	       (im-hi (fast-read-u-integer 4))
	       (im (make-double-float im-hi im-lo)))
	  (complex re im))
      (done-with-fast-read-byte))))

#+long-float
(define-fop (fop-complex-long-float 67)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
	(let* ((re-lo (fast-read-u-integer 4))
	       #+sparc (re-mid (fast-read-u-integer 4))
	       (re-hi (fast-read-u-integer 4))
	       (re-exp (fast-read-s-integer #+x86 2 #+sparc 4))
	       (re (make-long-float re-exp re-hi #+sparc re-mid re-lo))
	       (im-lo (fast-read-u-integer 4))
	       #+sparc (im-mid (fast-read-u-integer 4))
	       (im-hi (fast-read-u-integer 4))
	       (im-exp (fast-read-s-integer #+x86 2 #+sparc 4))
	       (im (make-long-float im-exp im-hi #+sparc im-mid im-lo)))
	  (complex re im))
      (done-with-fast-read-byte))))

(define-fop (fop-single-float 46)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1 (make-single-float (fast-read-s-integer 4))
      (done-with-fast-read-byte))))

(define-fop (fop-double-float 47)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
	(let ((lo (fast-read-u-integer 4)))
	  (make-double-float (fast-read-s-integer 4) lo))
      (done-with-fast-read-byte))))

#+long-float
(define-fop (fop-long-float 52)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
	(let ((lo (fast-read-u-integer 4))
	      #+sparc (mid (fast-read-u-integer 4))
	      (hi (fast-read-u-integer 4))
	      (exp (fast-read-s-integer #+x86 2 #+sparc 4)))
	  (make-long-float exp hi #+sparc mid lo))
      (done-with-fast-read-byte))))

#+double-double
(define-fop (fop-double-double-float 67)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
	(let ((hi-lo (fast-read-u-integer 4))
	      (hi-hi (fast-read-s-integer 4))
	      (lo-lo (fast-read-u-integer 4))
	      (lo-hi (fast-read-s-integer 4)))
	  (kernel::make-double-double-float
	   (make-double-float hi-hi hi-lo)
	   (make-double-float lo-hi lo-lo)))
      (done-with-fast-read-byte))))

#+double-double
(define-fop (fop-double-double-float-vector 88)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type 'double-double-float)))
    (read-n-bytes *fasl-file* result 0 (* length vm:word-bytes 4))
    result))

#+double-double
(define-fop (fop-complex-double-double-float 89)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
	(let* ((real-hi-lo (fast-read-u-integer 4))
	       (real-hi-hi (fast-read-s-integer 4))
	       (real-lo-lo (fast-read-u-integer 4))
	       (real-lo-hi (fast-read-s-integer 4))
	       (re (kernel::make-double-double-float
		    (make-double-float real-hi-hi real-hi-lo)
		    (make-double-float real-lo-hi real-lo-lo)))
	       (imag-hi-lo (fast-read-u-integer 4))
	       (imag-hi-hi (fast-read-s-integer 4))
	       (imag-lo-lo (fast-read-u-integer 4))
	       (imag-lo-hi (fast-read-s-integer 4))
	       (im (kernel::make-double-double-float
		    (make-double-float imag-hi-hi imag-hi-lo)
		    (make-double-float imag-lo-hi imag-lo-lo))))
	  (complex re im)) 
      (done-with-fast-read-byte))))

#+double-double
(define-fop (fop-complex-double-double-float-vector 90)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type '(complex double-double-float))))
    (read-n-bytes *fasl-file* result 0 (* length vm:word-bytes 8))
    result))

;;;; Loading lists:

(define-fop (fop-list 15)
  (do ((res () (cons (pop-stack) res))
       (n (read-arg 1) (1- n)))
      ((zerop n) res)
    (declare (type index n))))

(define-fop (fop-list* 16)
  (do ((res (pop-stack) (cons (pop-stack) res))
       (n (read-arg 1) (1- n)))
      ((zerop n) res)
    (declare (type index n))))

(macrolet ((frob (name op fun n)
	     `(define-fop (,name ,op)
		(call-with-popped-things ,fun ,n))))

  (frob fop-list-1 17 list 1)
  (frob fop-list-2 18 list 2)
  (frob fop-list-3 19 list 3)
  (frob fop-list-4 20 list 4)
  (frob fop-list-5 21 list 5)
  (frob fop-list-6 22 list 6)
  (frob fop-list-7 23 list 7)
  (frob fop-list-8 24 list 8)

  (frob fop-list*-1 25 list* 2)
  (frob fop-list*-2 26 list* 3)
  (frob fop-list*-3 27 list* 4)
  (frob fop-list*-4 28 list* 5)
  (frob fop-list*-5 29 list* 6)
  (frob fop-list*-6 30 list* 7)
  (frob fop-list*-7 31 list* 8)
  (frob fop-list*-8 32 list* 9))


;;;; Loading arrays:
;;;

(clone-fop (fop-string 37)
	   (fop-small-string 38)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (read-n-bytes *fasl-file* res 0 arg)
    res))

(clone-fop (fop-vector 39)
	   (fop-small-vector 40)
  (let* ((size (clone-arg))
	 (res (make-array size)))
    (declare (fixnum size))
    (do ((n (1- size) (1- n)))
	((minusp n))
      (setf (svref res n) (pop-stack)))
    res))

(clone-fop (fop-uniform-vector 41)
	   (fop-small-uniform-vector 42)
  (make-array (clone-arg) :initial-element (pop-stack)))

(define-fop (fop-array 83)
  (let* ((rank (read-arg 4))
	 (vec (pop-stack))
	 (length (length vec))
	 (res (make-array-header vm:simple-array-type rank)))
    (declare (simple-array vec)
	     (type (unsigned-byte 24) rank))
    (set-array-header res vec length nil 0
		      (do ((i rank (1- i))
			   (dimensions () (cons (pop-stack) dimensions)))
			  ((zerop i) dimensions)
			(declare (type index i)))
		      nil)
    res))

(define-fop (fop-single-float-vector 84)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type 'single-float)))
    (read-n-bytes *fasl-file* result 0 (* length vm:word-bytes))
    result))

(define-fop (fop-double-float-vector 85)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type 'double-float)))
    (read-n-bytes *fasl-file* result 0 (* length vm:word-bytes 2))
    result))

#+long-float
(define-fop (fop-long-float-vector 88)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type 'long-float)))
    (read-n-bytes *fasl-file* result 0
		  (* length vm:word-bytes #+x86 3 #+sparc 4))
    result))

(define-fop (fop-complex-single-float-vector 86)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type '(complex single-float))))
    (read-n-bytes *fasl-file* result 0 (* length vm:word-bytes 2))
    result))

(define-fop (fop-complex-double-float-vector 87)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type '(complex double-float))))
    (read-n-bytes *fasl-file* result 0 (* length vm:word-bytes 2 2))
    result))

#+long-float
(define-fop (fop-complex-long-float-vector 89)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type '(complex long-float))))
    (read-n-bytes *fasl-file* result 0
		  (* length vm:word-bytes #+x86 3 #+sparc 4 2))
    result))

;;; FOP-INT-VECTOR  --  Internal
;;;
;;; *** NOT *** the FOP-INT-VECTOR as currently documented in rtguts.  Size
;;; must be a directly supported I-vector element size, with no extra bits.
;;; This must be packed according to the local byte-ordering, allowing us to
;;; directly read the bits.
;;;
(define-fop (fop-int-vector 43)
  (prepare-for-fast-read-byte *fasl-file*
    (let* ((len (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (res (case size
		  (1 (make-array len :element-type 'bit))
		  (2 (make-array len :element-type '(unsigned-byte 2)))
		  (4 (make-array len :element-type '(unsigned-byte 4)))
		  (8 (make-array len :element-type '(unsigned-byte 8)))
		  (16 (make-array len :element-type '(unsigned-byte 16)))
		  (32 (make-array len :element-type '(unsigned-byte 32)))
		  (t (error "Losing i-vector element size: ~S" size)))))
      (declare (type index len))
      (done-with-fast-read-byte)
      (read-n-bytes *fasl-file* res 0
		    (ceiling (the index (* size len)) vm:byte-bits))
      res)))


(define-fop (fop-uniform-int-vector 44)
  (prepare-for-fast-read-byte *fasl-file*
    (let* ((n (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (value (fast-read-variable-u-integer (ceiling size 8))))
      (done-with-fast-read-byte)
      (make-array n :element-type `(unsigned-byte ,size)
		  :initial-element value))))

;;; FOP-SIGNED-INT-VECTOR
;;;
;;; Same as FOP-INT-VECTOR, except this is for signed simple-arrays.
;;; It appears that entry 50 and 51 are clear.
(define-fop (fop-signed-int-vector 50)
  (prepare-for-fast-read-byte *fasl-file*
    (let* ((len (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (res (case size
 		  (8 (make-array len :element-type '(signed-byte 8)))
 		  (16 (make-array len :element-type '(signed-byte 16)))
 		  (30 (make-array len :element-type '(signed-byte 30)))
 		  (32 (make-array len :element-type '(signed-byte 32)))
 		  (t (error "Losing i-vector element size: ~S" size)))))
      (declare (type index len))
      (done-with-fast-read-byte)
      (read-n-bytes *fasl-file* res 0
 		    (ceiling (the index (* (if (= size 30)
					       32 ; Adjust for (signed-byte 30)
					       size) len)) vm:byte-bits))
      res)))

;;; Same as fop-uniform-int-vector, but for signed integers
(define-fop (fop-uniform-signed-int-vector 51)
   (prepare-for-fast-read-byte *fasl-file*
     (let* ((n (fast-read-u-integer 4))
	    (size (fast-read-byte))
	    (value (fast-read-variable-u-integer (ceiling size 8))))
       (done-with-fast-read-byte)
       (make-array n :element-type `(signed-byte ,size)
		   :initial-element value))))
 
(define-fop (fop-eval 53)
  (let ((result (eval (pop-stack))))
    (when *load-print*
      (load-fresh-line)
      (prin1 result)
      (terpri))
    result))

(define-fop (fop-eval-for-effect 54 nil)
  (let ((result (eval (pop-stack))))
    (when *load-print*
      (load-fresh-line)
      (prin1 result)
      (terpri))))

(define-fop (fop-funcall 55)
  (let ((arg (read-arg 1)))
    (if (zerop arg)
	(funcall (pop-stack))
	(do ((args () (cons (pop-stack) args))
	     (n arg (1- n)))
	    ((zerop n) (apply (pop-stack) args))
	  (declare (type index n))))))

(define-fop (fop-funcall-for-effect 56 nil)
  (let ((arg (read-arg 1)))
    (if (zerop arg)
	(funcall (pop-stack))
	(do ((args () (cons (pop-stack) args))
	     (n arg (1- n)))
	    ((zerop n) (apply (pop-stack) args))
	  (declare (type index n))))))

;;;; Fixing up circularities.
(define-fop (fop-rplaca 200 nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4))
	(val (pop-stack)))
    (setf (car (nthcdr idx obj)) val)))


(define-fop (fop-rplacd 201 nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4))
	(val (pop-stack)))
    (setf (cdr (nthcdr idx obj)) val)))

(define-fop (fop-svset 202 nil)
  (let* ((obi (read-arg 4))
	 (obj (svref *current-fop-table* obi))
	 (idx (read-arg 4))
	 (val (pop-stack)))
    (if (%instancep obj)
	(setf (%instance-ref obj idx) val)
	(setf (svref obj idx) val))))

(define-fop (fop-structset 204 nil)
  (setf (%instance-ref (svref *current-fop-table* (read-arg 4))
		       (read-arg 4))
	(pop-stack)))

(define-fop (fop-nthcdr 203 t)
  (nthcdr (read-arg 4) (pop-stack)))

;;;; Loading functions:

;;; must be compatible with the function OPEN-FASL-FILE in compiler/dump.lisp
;;; The old fop-code-format FOP is legacy support for FASL formats prior to
;;; #x18d, which used a single octet for the version number.
(clone-fop (fop-long-code-format 157 :nope)
	   (fop-code-format 57)
  (let ((implementation (read-arg 1))
	(version (clone-arg)))
    (flet ((check-version (imp vers)
	     (when (eql imp implementation)
	       (unless (eql version vers)
		 (cerror "Load ~A anyway"
                         'invalid-fasl :file *fasl-file*
			 :fasl-version version :expected-version vers))
	       t))
	   (imp-name (imp)
             (if (< -1 imp (length '#.c:fasl-file-implementations))
                 (nth imp '#.c:fasl-file-implementations)
		 "unknown machine")))
    (unless (or *skip-fasl-file-version-check*
                (check-version #.(c:backend-fasl-file-implementation
				  c:*backend*)
			       #.(c:backend-fasl-file-version
				  c:*backend*))
		(check-version #.(c:backend-byte-fasl-file-implementation
				  c:*backend*)
			       c:byte-fasl-file-version))
      (cerror "Load ~A anyway"
              "~A was compiled for a ~A, but this is a ~A"
              *Fasl-file*
              (imp-name implementation)
              (imp-name
	       #.(c:backend-fasl-file-implementation c:*backend*)))))))

;;; Load-Code loads a code object.  NItems objects are popped off the stack for
;;; the boxed storage section, then Size bytes of code are read in.
;;;
#-(or x86 amd64)
(defun load-code (box-num code-length)
  (declare (fixnum box-num code-length))
  (with-fop-stack t
    (let ((code (%primitive allocate-code-object box-num code-length))
	  (index (+ #-gengc vm:code-trace-table-offset-slot
		    #+gengc vm:code-debug-info-slot
		    box-num)))
      (declare (type index index))
      #-gengc (setf (%code-debug-info code) (pop-stack))
      (dotimes (i box-num)
	(declare (fixnum i))
	(setf (code-header-ref code (decf index)) (pop-stack)))
      (system:without-gcing
	(read-n-bytes *fasl-file* (code-instructions code) 0
		      #-gengc code-length
		      #+gengc (* code-length vm:word-bytes)))
      code)))


;;; Moving native code during a GC or purify is not trivial on the x86
;;; port, so there are a few options for code placement.
;;;
;;; Byte-compiled code objects can always be moved so can be place in
;;; the dynamics heap.  This is enabled with
;;; *load-byte-compiled-code-to-dynamic-space*.
;;;
;;; Native code top level forms only have a short life so can be
;;; safely loaded into the dynamic heap (without fixups) so long as
;;; the GC is not active. This could be handy during a world load to
;;; save core space without the need to enable the support for moving
;;; x86 native code. Enable with *load-x86-tlf-to-dynamic-space*.
;;;
;;; One strategy for allowing the loading of x86 native code into the
;;; dynamic heap requires that the addresses of fixups be saved for
;;; all these code objects.  After a purify these fixups can be
;;; dropped. This is enabled with *enable-dynamic-space-code*.
;;;
;;; A little analysis of the header information is used to determine
;;; if a code object is byte compiled, or native code.
;;;

(defvar *load-byte-compiled-code-to-dynamic-space* t)
(defvar *load-x86-tlf-to-dynamic-space* nil)  ; potentially dangerous with CGC.
(defvar *load-code-verbose* nil)
(defvar *enable-dynamic-space-code* #-gencgc nil #+gencgc t)

#+(or x86 amd64)
(defun load-code (box-num code-length)
  (declare (fixnum box-num code-length))
  (with-fop-stack t
    (let ((stuff (list (pop-stack))))
      (dotimes (i box-num)
	(declare (fixnum i))
	(push (pop-stack) stuff))
      (let* ((dbi (car (last stuff)))	; debug-info
	     (tto (first stuff))	; trace-table-offset
	     (load-to-dynamic-space
	      (or *enable-dynamic-space-code*
	       ;; Definitely Byte compiled code?
	       (and *load-byte-compiled-code-to-dynamic-space*
		    (c::debug-info-p dbi)
		    (not (c::compiled-debug-info-p dbi)))
	       ;; Or a x86 top level form.
	       (and *load-x86-tlf-to-dynamic-space*
		    (c::compiled-debug-info-p dbi)
		    (string= (c::compiled-debug-info-name dbi)
			     "Top-Level Form")))) )

	(setq stuff (nreverse stuff))

	;; Check that tto is always a list for byte-compiled
	;; code. Could be used an alternate check.
	(when (and (typep tto 'list)
		   (not (and (c::debug-info-p dbi)
			     (not (c::compiled-debug-info-p dbi)))))
	      (format t "* tto list on non-bc code: ~s~% ~s ~s~%" 
		      stuff dbi tto))
	
	(when *load-code-verbose*
	      (format t "stuff: ~s~%" stuff)
	      (format t "   : ~s ~s ~s ~s~%" 
		      (c::compiled-debug-info-p dbi)
		      (c::debug-info-p dbi)
		      (c::compiled-debug-info-name dbi)
		      tto)
	      (if load-to-dynamic-space
		  (format t "   Loading to the dynamic space~%")
		(format t "   Loading to the static space~%")))
      
	(let ((code
	       (if load-to-dynamic-space
		   (%primitive
		    allocate-dynamic-code-object box-num code-length)
		   (%primitive allocate-code-object box-num code-length)))
	      (index (+ vm:code-trace-table-offset-slot box-num))) 
	  (declare (type index index)) 
	  (when *load-code-verbose*
		(format t "  obj addr=~x~%"
			(kernel::get-lisp-obj-address code)))
	  (setf (%code-debug-info code) (pop stuff))
	  (dotimes (i box-num)
	    (declare (fixnum i))
	    (setf (code-header-ref code (decf index)) (pop stuff)))
	  (system:without-gcing
	   (read-n-bytes *fasl-file* (code-instructions code) 0 code-length))
	  code)))))

(define-fop (fop-code 58 :nope)
  (load-code (read-arg 4) (read-arg 4)))

(define-fop (fop-small-code 59 :nope)
  (load-code (read-arg 1) (read-arg 2)))

(define-fop (fop-fdefinition 60)
  (fdefinition-object (pop-stack) t))

(define-fop (fop-sanctify-for-execution 61)
  (let ((component (pop-stack)))
    (vm:sanctify-for-execution component)
    component))

;;; Now a NOOP except in cold load... 
(define-fop (fop-fset 74 nil)
  (pop-stack)
  (pop-stack))


;;; Modify a slot in a Constants object.
;;;
(clone-fop (fop-alter-code 140 nil) (fop-byte-alter-code 141)
  (let ((value (pop-stack))
	(code (pop-stack)))
    (setf (code-header-ref code (clone-arg)) value)
    (undefined-value)))

(define-fop (fop-function-entry 142)
  (let ((type (pop-stack))
	(arglist (pop-stack))
	(name (pop-stack))
	(code-object (pop-stack))
	(offset (read-arg 4)))
    (declare (type index offset))
    (unless (zerop (logand offset vm:lowtag-mask))
      (error "Unaligned function object, offset = #x~X." offset))
    (let ((fun (%primitive compute-function code-object offset)))
      (setf (%function-self fun) fun)
      (setf (%function-next fun) (%code-entry-points code-object))
      (setf (%code-entry-points code-object) fun)
      (setf (%function-name fun) name)
      (setf (%function-arglist fun) arglist)
      (setf (%function-type fun) type)
      (when *load-print*
	(load-fresh-line)
	(format t "~S defined~%" fun))
      fun)))

(define-fop (fop-make-byte-compiled-function 143)
  (let* ((size (read-arg 1))
	 (layout (pop-stack))
	 (res (%make-funcallable-instance size layout)))
    (declare (type index size))
    (do ((n (1- size) (1- n)))
	((minusp n))
      (declare (type (integer -1 #.most-positive-fixnum) n))
      (setf (%funcallable-instance-info res n) (pop-stack)))
    (initialize-byte-compiled-function res)
    (when *load-print*
      (load-fresh-line)
      (format t "~S defined~%" res))
    res))




;;;; Linkage fixups.

;;; These two variables are initially filled in by Genesis.

(defvar *initial-assembler-routines*)
(defvar *initial-foreign-symbols*)

(defvar *assembler-routines* (make-hash-table :test #'eq))
(defvar *foreign-symbols* (make-hash-table :test #'equal))
(declaim (type hash-table *assembler-routines* *foreign-symbols*))

(defun loader-init ()
  (dolist (routine *initial-assembler-routines*)
    (setf (gethash (car routine) *assembler-routines*) (cdr routine)))
  (dolist (symbol *initial-foreign-symbols*)
    (setf (gethash (car symbol) *foreign-symbols*) (cdr symbol)))
  (makunbound '*initial-assembler-routines*)
  (makunbound '*initial-foreign-symbols*)
  #+linkage-table
  (foreign-linkage-init))

#-linkage-table
(defun foreign-symbol-address-aux (symbol flavor)
  (declare (ignore flavor))
  (multiple-value-bind
      (value found)
      (gethash symbol *foreign-symbols* 0)
    ;; can't make irix linker give values in the symbol table to global vars
    ;;from dsos, so we have to resolve at runtime (and handle symbols being
    ;;defined with null values)
    (if #-irix found #+irix (and found (not (zerop value)))
	value
	(let ((value (system:alternate-get-global-address symbol)))
	  (when (zerop value)
	    (error "Unknown foreign symbol: ~S" symbol))
	  value))))

(defun foreign-symbol-address (symbol &key (flavor :code))
  (let ((maybe-link-table-addr
	 (foreign-symbol-address-aux (vm:extern-alien-name symbol) flavor)))
    (if (or #-linkage-table t (eq flavor :code))
	(int-sap maybe-link-table-addr)
	;;; Get address out of linkage table
	(int-sap (sap-ref-32 (int-sap maybe-link-table-addr) 0)))))

(define-fop (fop-foreign-fixup 147)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (read-n-bytes *fasl-file* sym 0 len)
    (vm:fixup-code-object code-object (read-arg 4)
			  (foreign-symbol-address-aux sym :code)
			  kind)
    code-object))

(define-fop (fop-foreign-data-fixup 150)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (read-n-bytes *fasl-file* sym 0 len)
    (vm:fixup-code-object code-object (read-arg 4)
			  (foreign-symbol-address-aux sym :data)
			  kind)
    code-object))

(define-fop (fop-assembler-code 144)
  (error "Cannot load assembler code."))

(define-fop (fop-assembler-routine 145)
  (error "Cannot load assembler code."))

(define-fop (fop-assembler-fixup 148)
  (let ((routine (pop-stack))
	(kind (pop-stack))
	(code-object (pop-stack)))
    (multiple-value-bind
	(value found)
	(gethash routine *assembler-routines*)
      (unless found
	(error "Undefined assembler routine: ~S" routine))
      (vm:fixup-code-object code-object (read-arg 4) value kind))
    code-object))

(define-fop (fop-code-object-fixup 149)
  (let ((kind (pop-stack))
	(code-object (pop-stack)))
    ;; Note: We don't have to worry about GC moving the code-object after
    ;; the GET-LISP-OBJ-ADDRESS and before that value is deposited, because
    ;; we can only use code-object fixups when code-objects don't move.
    (vm:fixup-code-object code-object (read-arg 4)
			  (get-lisp-obj-address code-object) kind)
    code-object))


(declaim (maybe-inline read-byte))
