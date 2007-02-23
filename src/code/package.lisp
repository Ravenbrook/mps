;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/package.lisp,v 1.75 2006/05/11 17:07:22 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;     Package stuff and stuff like that.
;;;
;;; Re-Written by Rob MacLachlan.  Earlier version written by
;;; Lee Schumacher.  Apropos & iteration macros courtesy of Skef Wholey.
;;; Defpackage by Dan Zigmond.  With-Package-Iterator by Blaine Burks. 
;;; Defpackage and do-mumble-symbols macros re-written by William Lott.
;;;
(in-package "LISP")
(export '(package packagep *package* make-package in-package find-package
	  package-name package-nicknames rename-package delete-package
	  package-use-list package-used-by-list package-shadowing-symbols
	  list-all-packages intern find-symbol unintern export
	  unexport import shadowing-import shadow use-package
	  unuse-package find-all-symbols do-symbols with-package-iterator
	  do-external-symbols do-all-symbols apropos apropos-list defpackage))

(in-package "EXTENSIONS")
(export '(*keyword-package* *lisp-package* *default-package-use-list*
	  map-apropos package-children package-parent
          package-lock package-definition-lock without-package-locks
          unlock-all-packages))

(in-package "KERNEL")
(export '(%in-package old-in-package %defpackage))

(in-package "LISP")

#+relative-package-names
(sys:register-lisp-feature :relative-package-names)

(defvar *default-package-use-list* '("COMMON-LISP")
  "The list of packages to use by default of no :USE argument is supplied
   to MAKE-PACKAGE or other package creation forms.")

;;; INTERNAL conditions
(define-condition simple-package-error (simple-condition package-error) ())

(defstruct (package
	    (:constructor internal-make-package)
	    (:predicate packagep)
	    (:print-function %print-package)
	    (:make-load-form-fun
	     (lambda (package)
	       (values `(package-or-lose ',(package-name package))
		       nil))))
  "Standard structure for the description of a package.  Consists of 
   a list of all hash tables, the name of the package, the nicknames of
   the package, the use-list for the package, the used-by- list, hash-
   tables for the internal and external symbols, and a list of the
   shadowing symbols."
  (tables (list nil))	; A list of all the hashtables for inherited symbols.
  ;;
  ;; The string name of the package.
  (%name nil :type (or simple-string null))
  ;;
  ;; List of nickname strings.
  (%nicknames () :type list)
  ;;
  ;; List of packages we use.
  (%use-list () :type list)
  ;;
  ;; List of packages that use this package.
  (%used-by-list () :type list)
  ;;
  ;; Hashtables of internal & external symbols.
  (internal-symbols (required-argument) :type package-hashtable)
  (external-symbols (required-argument) :type package-hashtable)
  ;;
  ;; List of shadowing symbols.
  (%shadowing-symbols () :type list)
  ;;
  ;; Locks for this package. The PACKAGE-LOCK is a lock on the
  ;; structure of the package, and controls modifications to its list
  ;; of symbols and its export list. The PACKAGE-DEFINITION-LOCK
  ;; protects all symbols in the package from being redefined. These
  ;; are initially disabled, and are enabled by the function
  ;; PACKAGE-LOCKS-INIT during after-save-initializations. 
  (lock nil :type boolean)
  (definition-lock nil :type boolean)

  ;; Documentation string for this package
  (doc-string nil :type (or simple-string null)))


(defun %print-package (s stream d)
  (declare (ignore d) (stream stream))
  (if (package-%name s)
      (cond (*print-escape*
             (multiple-value-bind (iu it) (internal-symbol-count s)
               (multiple-value-bind (eu et) (external-symbol-count s)
                 (print-unreadable-object (s stream)
                   (format stream "The ~A package, ~D/~D internal, ~D/~D external"
                           (package-%name s) iu it eu et)))))
            (t
             (print-unreadable-object (s stream)
               (format stream "The ~A package" (package-%name s)))))
      (print-unreadable-object (s stream :identity t)
	(format stream "deleted package"))))

;;; Can get the name (NIL) of a deleted package.
;;;
(defun package-name (x)
  (package-%name (if (packagep x) x (package-or-lose x))))

(macrolet ((frob (ext real)
	     `(defun ,ext (x) (,real (package-or-lose x)))))
  (frob package-nicknames package-%nicknames)
  (frob package-use-list package-%use-list)
  (frob package-used-by-list package-%used-by-list)
  (frob package-shadowing-symbols package-%shadowing-symbols))

(defvar *package* () "The current package.")

;;; An equal hashtable from package names to packages.
;;;
(defvar *package-names* (make-hash-table :test #'equal))


;;; Lots of people want the keyword package and Lisp package without a lot
;;; of fuss, so we give them their own variables.
;;;
(defvar *lisp-package*)
(defvar *keyword-package*)


(defvar *enable-package-locked-errors* nil)


(define-condition package-locked-error (simple-package-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~&~@<Attempt to modify the locked package ~A, by ~3i~:_~?~:>"
                     (package-name (package-error-package condition))
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))))

(defun package-locks-init ()
  (let ((package-names '("COMMON-LISP" "LISP" "PCL" "CLOS-MOP" "EVAL"
                         "NEW-ASSEM" "DISASSEM" "LOOP" "ANSI-LOOP" "INSPECT"
                         "C" "PROFILE" "WIRE" "BIGNUM" "VM"
                         "FORMAT" "DFIXNUM" "PRETTY-PRINT" "C-CALL" "ALIEN"
                         "ALIEN-INTERNALS" "UNIX"
                         "CONDITIONS" "DEBUG" "DEBUG-INTERNALS" "SYSTEM"
                         "KERNEL" "EXTENSIONS" #+mp "MULTIPROCESSING"
                         "WALKER" "XREF" "STREAM")))
    (dolist (p package-names)
      (let ((p (find-package p)))
        (when p
          (setf (package-definition-lock p) t)
          (setf (package-lock p) t))))
    (setf *enable-package-locked-errors* t)
    (push 'redefining-function ext:*setf-fdefinition-hook*))
  (values))

(pushnew 'package-locks-init ext:*after-save-initializations*)


(defun unlock-all-packages ()
  (dolist (p (list-all-packages))
    (setf (package-definition-lock p) nil)
    (setf (package-lock p) nil)))


(defmacro without-package-locks (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((*enable-package-locked-errors* nil))
       ;; Tell the compiler about disabled locks too.  This is a
       ;; workaround for the case of defmacro of a symbol in a locked
       ;; package.
       (ext:compiler-let ((*enable-package-locked-errors* nil))
	 ,@body))))


;; trap attempts to redefine a function in a locked package, and
;; signal a continuable error.
(defun redefining-function (function replacement)
  (declare (ignore replacement))
  (when *enable-package-locked-errors*
    (multiple-value-bind (valid block-name)
        (ext:valid-function-name-p function)
      (declare (ignore valid))
      (let ((package (symbol-package block-name)))
        (when package
          (when (package-definition-lock package)
            (when (and (consp function)
                       (member (first function)
                               '(pcl::slot-accessor
                                 pcl::method
                                 pcl::fast-method
                                 pcl::effective-method
                                 pcl::ctor)))
              (return-from redefining-function nil))
            (restart-case
                (error 'package-locked-error
                       :package package
                       :format-control "redefining function ~A"
                       :format-arguments (list function))
              (continue ()
                :report "Ignore the lock and continue")
              (unlock-package ()
                :report "Disable package's definition-lock, then continue"
                (setf (ext:package-definition-lock package) nil))
              (unlock-all ()
                :report "Disable all package locks, then continue"
                (unlock-all-packages)))))))))


;;; This magical variable is T during initialization so Use-Package's of packages
;;; that don't yet exist quietly win.  Such packages are thrown onto the list
;;; *Deferred-Use-Packages* so that this can be fixed up later.

(defvar *in-package-init* nil)
(defvar *deferred-use-packages* nil)

(defun stringify-name (name kind)
  (typecase name
    (simple-string name)
    (string (coerce name 'simple-string))
    (symbol (symbol-name name))
    (base-char
     (let ((res (make-string 1)))
       (setf (schar res 0) name)
       res))
    (t
     (error "Bogus ~A name: ~S" kind name))))

(defun stringify-names (names kind)
  (mapcar #'(lambda (name)
	      (stringify-name name kind))
	  names))

;;; package-namify  --  Internal
;;;
;;;    Make a package name into a simple-string.
;;;
(defun package-namify (n)
  (stringify-name n "package"))

;;; package-namestring  --  Internal
;;;
;;;    Take a package-or-string-or-symbol and return a package name.
;;;
(defun package-namestring (thing)
  (if (packagep thing)
      (let ((name (package-%name thing)))
	(or name
	    (error "Can't do anything to a deleted package: ~S" thing)))
      (package-namify thing)))

;;; package-name-to-package  --  Internal
;;;
;;; Given a package name, a simple-string, do a package name lookup.
;;;
(defun package-name-to-package (name)
  (declare (simple-string name))
  (values (gethash name *package-names*)))

;;; package-parent  --  Internal.
;;;
;;; Because this function is called via the reader, we want it to be as
;;; fast as possible.
;;;
#+relative-package-names
(defun package-parent (package-specifier)
  "Given PACKAGE-SPECIFIER, a package, symbol or string, return the
  parent package.  If there is not a parent, signal an error."
  (declare (optimize (speed 3)))
  (flet ((find-last-dot (name)
           (do* ((len (1- (length name)))
		 (i len (1- i)))
		((= i -1) nil)
             (declare (fixnum len i))
             (when (char= #\. (schar name i)) (return i)))))
    (let* ((child (package-namestring package-specifier))
           (dot-position (find-last-dot child)))
      (cond (dot-position
             (let ((parent (subseq child 0 dot-position)))
               (or (package-name-to-package parent)
		   (error 'simple-package-error
                          :name child
                          :format-control "The parent of ~a does not exist."
                          :format-arguments (list child)))))
            (t
	     (error 'simple-package-error
                    :name child
                    :format-control "There is no parent of ~a."
                    :format-arguments (list child)))))))


;;; package-children  --  Internal.
;;;
;;; While this function is not called via the reader, we do want it to be
;;; fast.
;;;
#+relative-package-names
(defun package-children (package-specifier &key (recurse t))
  "Given PACKAGE-SPECIFIER, a package, symbol or string, return all the
  packages which are in the hierarchy 'under' the given package.  If
  :recurse is nil, then only return the immediate children of the package."
  (declare (optimize (speed 3)))
  (let ((res ())
        (parent (package-namestring package-specifier)))
    (labels
        ((string-prefix-p (prefix string)
	   (declare (simple-string prefix string))
           ;; Return length of `prefix' if `string' starts with `prefix'.
           ;; We don't use `search' because it does much more than we need
           ;; and this version is about 10x faster than calling `search'.
           (let ((prefix-len (length prefix))
                 (seq-len (length string)))
             (declare (type index prefix-len seq-len))
             (when (>= prefix-len seq-len)
               (return-from string-prefix-p nil))
             (do ((i 0 (1+ i)))
                 ((= i prefix-len) prefix-len)
               (declare (type index i))
               (unless (char= (schar prefix i) (schar string i))
                 (return nil)))))
         (test-package (package-name package)
	   (declare (simple-string package-name)
		    (type package package))
           (let ((prefix
                  (string-prefix-p (concatenate 'simple-string parent ".")
                                   package-name)))
             (cond (recurse
		    (when prefix
		      (pushnew package res)))
                   (t
		    (when (and prefix
			       (not (find #\. package-name :start prefix)))
		      (pushnew package res)))))))
      (maphash #'test-package *package-names*)
      res)))

;;; relative-package-name-to-package  --  Internal
;;;
;;; Given a package name, a simple-string, do a relative package name lookup.
;;; It is intended that this function will be called from find-package.
;;;
#+relative-package-names
(defun relative-package-name-to-package (name)
  (declare (simple-string name)
	   (optimize (speed 3)))
  (flet ((relative-to (package name)
	   (declare (type package package)
		    (simple-string name))
           (if (string= "" name)
	       package
	       (let ((parent-name (package-%name package)))
		 (unless parent-name
		   (error "Can't do anything to a deleted package: ~S"
			  package))
		 (package-name-to-package
		  (concatenate 'simple-string parent-name "." name)))))
         (find-non-dot (name)
           (do* ((len (length name))
                 (i 0 (1+ i)))
		((= i len) nil)
             (declare (type index len i))
             (when (char/= #\. (schar name i)) (return i)))))
    (when (and (plusp (length name))
	       (char= #\. (schar name 0)))
      (let* ((last-dot-position (or (find-non-dot name) (length name)))
             (n-dots last-dot-position)
             (name (subseq name last-dot-position)))
        (cond ((= 1 n-dots)
               ;; relative to current package
               (relative-to *package* name))
              (t
               ;; relative to our (- n-dots 1)'th parent
               (let ((package *package*)
                     (tmp nil))
                 (dotimes (i (1- n-dots))
		   (declare (fixnum i))
		   (setq tmp (package-parent package))
                   (unless tmp
		     (error 'simple-package-error
                            :name (string package)
                            :format-control "The parent of ~a does not exist."
                            :format-arguments (list package)))
                   (setq package tmp))
                 (relative-to package name))))))))

;;; find-package  --  Public
;;;
;;;
(defun find-package (name)
  "Find the package having the specified name."
  (if (packagep name)
      name
      (let ((name (package-namify name)))
	(or (package-name-to-package name)
	    #+relative-package-names
	    (relative-package-name-to-package name)))))

;;; package-or-lose  --  Internal
;;;
;;;    Take a package-or-string-or-symbol and return a package.
;;;
(defun package-or-lose (thing)
  (cond ((packagep thing)
	 (unless (package-%name thing)
	   (error "Can't do anything to a deleted package: ~S" thing))
	 thing)
	(t
	 (let ((thing (package-namify thing)))
	   (cond ((package-name-to-package thing))
		 (t
		  ;; ANSI spec's type-error where this is called. But,
		  ;; but the resulting message is somewhat unclear.
		  ;; May need a new condition type?
		  (with-simple-restart
		      (continue "Make this package.")
		    (error 'type-error
			   :datum thing
			   :expected-type 'package))
		  (make-package thing)))))))

;;; package-listify  --  Internal
;;;
;;;    Return a list of packages given a package-or-string-or-symbol or
;;; list thereof, or die trying.
;;;
(defun package-listify (thing)
  (let ((res ()))
    (dolist (thing (if (listp thing) thing (list thing)) res)
      (push (package-or-lose thing) res))))


;;;; Package-Hashtables
;;;
;;;    Packages are implemented using a special kind of hashtable.  It is
;;; an open hashtable with a parallel 8-bit I-vector of hash-codes.  The
;;; primary purpose of the hash for each entry is to reduce paging by
;;; allowing collisions and misses to be detected without paging in the
;;; symbol and pname for an entry.  If the hash for an entry doesn't
;;; match that for the symbol that we are looking for, then we can
;;; go on without touching the symbol, pname, or even hastable vector.
;;;    It turns out that, contrary to my expectations, paging is a very
;;; important consideration the design of the package representation.
;;; Using a similar scheme without the entry hash, the fasloader was
;;; spending more than half its time paging in INTERN.
;;;    The hash code also indicates the status of an entry.  If it zero,
;;; the entry is unused.  If it is one, then it is deleted.
;;; Double-hashing is used for collision resolution.

(deftype hash-vector () '(simple-array (unsigned-byte 8) (*)))

(defstruct (package-hashtable
	    (:constructor internal-make-package-hashtable ())
	    (:copier nil)
	    (:print-function
	     (lambda (table stream d)
	       (declare (ignore d) (stream stream))
	       (format stream
		       "#<Package-Hashtable: Size = ~D, Free = ~D, Deleted = ~D>"
		       (package-hashtable-size table)
		       (package-hashtable-free table)
		       (package-hashtable-deleted table)))))
  ;;
  ;; The g-vector of symbols.
  (table nil :type (or simple-vector null))
  ;;
  ;; The i-vector of pname hash values.
  (hash nil :type (or hash-vector null))
  ;;
  ;; The maximum number of entries allowed.
  (size 0 :type index)
  ;;
  ;; The entries that can be made before we have to rehash.
  (free 0 :type index)
  ;;
  ;; The number of deleted entries.
  (deleted 0 :type index))


;;; The maximum density we allow in a package hashtable.
;;;
(defparameter package-rehash-threshold 3/4)

;;; Entry-Hash  --  Internal
;;;
;;;    Compute a number from the sxhash of the pname and the length which
;;; must be between 2 and 255.
;;;
(defmacro entry-hash (length sxhash)
  `(the fixnum
	(+ (the fixnum
		(rem (the fixnum
			  (logxor ,length
				  ,sxhash
				  (the fixnum (ash ,sxhash -8))
				  (the fixnum (ash ,sxhash -16))
				  (the fixnum (ash ,sxhash -19))))
		     254))
	   2)))

;;; Make-Package-Hashtable  --  Internal
;;;
;;;    Make a package hashtable having a prime number of entries at least
;;; as great as (/ size package-rehash-threshold).  If Res is supplied,
;;; then it is destructively modified to produce the result.  This is
;;; useful when changing the size, since there are many pointers to
;;; the hashtable.
;;;
(defun make-package-hashtable (size &optional
				    (res (internal-make-package-hashtable)))
  (do ((n (logior (truncate size package-rehash-threshold) 1)
	  (+ n 2)))
      ((primep n)
       (setf (package-hashtable-table res)
	     (make-array n))
       (setf (package-hashtable-hash res)
	     (make-array n :element-type '(unsigned-byte 8)
			 :initial-element 0))
       (let ((size (truncate (* n package-rehash-threshold))))
	 (setf (package-hashtable-size res) size)
	 (setf (package-hashtable-free res) size))
       (setf (package-hashtable-deleted res) 0)
       res)
    (declare (fixnum n))))


;;; Internal-Symbol-Count, External-Symbols-Count  --  Internal
;;;
;;;    Return internal and external symbols.  Used by Genesis and stuff.
;;;
(flet ((stuff (table)
	 (let ((size (the fixnum
			  (- (the fixnum (package-hashtable-size table))
			     (the fixnum
				  (package-hashtable-deleted table))))))
	   (declare (fixnum size))
	   (values (the fixnum
			(- size
			   (the fixnum
				(package-hashtable-free table))))
		   size))))

  (defun internal-symbol-count (package)
    (stuff (package-internal-symbols package)))

  (defun external-symbol-count (package)
    (stuff (package-external-symbols package))))


;;; Add-Symbol  --  Internal
;;;
;;;    Add a symbol to a package hashtable.  The symbol is assumed
;;; not to be present.
;;;
(defun add-symbol (table symbol)
  (let* ((vec (package-hashtable-table table))
	 (hash (package-hashtable-hash table))
	 (len (length vec))
	 (sxhash (%sxhash-simple-string (symbol-name symbol)))
	 (h2 (the fixnum (1+ (the fixnum (rem sxhash
					      (the fixnum (- len 2))))))))
    (declare (simple-vector vec)
	     (type (simple-array (unsigned-byte 8)) hash)
	     (fixnum len sxhash h2))
    (cond ((zerop (the fixnum (package-hashtable-free table)))
	   (make-package-hashtable (the fixnum
					(* (the fixnum
						(package-hashtable-size table))
					   2))
				   table)
	   (add-symbol table symbol)
	   (dotimes (i len)
	     (declare (fixnum i))
	     (when (> (the fixnum (aref hash i)) 1)
	       (add-symbol table (svref vec i)))))
	  (t
	   (do ((i (rem sxhash len) (rem (+ i h2) len)))
	       ((< (the fixnum (aref hash i)) 2)
		(if (zerop (the fixnum (aref hash i)))
		    (decf (the fixnum (package-hashtable-free table)))
		    (decf (the fixnum (package-hashtable-deleted table))))
		(setf (svref vec i) symbol)
		(setf (aref hash i)
		      (entry-hash (length (the simple-string
					       (symbol-name symbol)))
				  sxhash)))
	     (declare (fixnum i)))))))

;;; With-Symbol  --  Internal
;;;
;;;    Find where the symbol named String is stored in Table.  Index-Var
;;; is bound to the index, or NIL if it is not present.  Symbol-Var
;;; is bound to the symbol.  Length and Hash are the length and sxhash
;;; of String.  Entry-Hash is the entry-hash of the string and length.
;;;
(defmacro with-symbol ((index-var symbol-var table string length sxhash
				  entry-hash)
		       &body forms)
  (let ((vec (gensym)) (hash (gensym)) (len (gensym)) (h2 (gensym))
	(name (gensym)) (name-len (gensym)) (ehash (gensym)))
    `(let* ((,vec (package-hashtable-table ,table))
	    (,hash (package-hashtable-hash ,table))
	    (,len (length ,vec))
	    (,h2 (1+ (the index (rem (the index ,sxhash)
				     (the index (- ,len 2)))))))
       (declare (type (simple-array (unsigned-byte 8) (*)) ,hash)
		(simple-vector ,vec)
		(type index ,len ,h2))
       (prog ((,index-var (rem (the index ,sxhash) ,len))
	      ,symbol-var ,ehash)
	 (declare (type (or index null) ,index-var))
	 LOOP
	 (setq ,ehash (aref ,hash ,index-var))
	 (cond ((eql ,ehash ,entry-hash)
		(setq ,symbol-var (svref ,vec ,index-var))
		(let* ((,name (symbol-name ,symbol-var))
		       (,name-len (length ,name)))
		  (declare (simple-string ,name)
			   (type index ,name-len))
		  (when (and (= ,name-len ,length)
			     (string= ,string ,name  :end1 ,length
				      :end2 ,name-len))
		    (go DOIT))))
	       ((zerop ,ehash)
		(setq ,index-var nil)
		(go DOIT)))
	 (setq ,index-var (+ ,index-var ,h2))
	 (when (>= ,index-var ,len)
	   (setq ,index-var (- ,index-var ,len)))
	 (go LOOP)
	 DOIT
	 (return (progn ,@forms))))))

;;; Nuke-Symbol  --  Internal
;;;
;;;    Delete the entry for String in Table.  The entry must exist.
;;;
(defun nuke-symbol (table string)
  (declare (simple-string string))
  (let* ((length (length string))
	 (hash (%sxhash-simple-string string))
	 (ehash (entry-hash length hash)))
    (declare (type index length hash))
    (with-symbol (index symbol table string length hash ehash)
      (setf (aref (package-hashtable-hash table) index) 1)
      (setf (aref (package-hashtable-table table) index) nil)
      (incf (package-hashtable-deleted table)))))

;;;; Iteration macros.

(defmacro do-symbols ((var &optional (package '*package*) result-form)
		      &parse-body (body decls))
  "DO-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs at least once for each symbol accessible in the given
   PACKAGE with VAR bound to the current symbol."
  (let ((flet-name (gensym "DO-SYMBOLS-")))
    `(block nil
       (flet ((,flet-name (,var)
		,@decls
		(tagbody ,@body)))
	 (let* ((package (package-or-lose ,package))
		(shadows (package-%shadowing-symbols package)))
	   (flet ((iterate-over-hash-table (table ignore)
		    (let ((hash-vec (package-hashtable-hash table))
			  (sym-vec (package-hashtable-table table)))
		      (declare (type (simple-array (unsigned-byte 8) (*))
				     hash-vec)
			       (type simple-vector sym-vec))
		      (dotimes (i (length sym-vec))
			(when (>= (aref hash-vec i) 2)
			  (let ((sym (aref sym-vec i)))
			    (declare (inline member))
			    (unless (member sym ignore :test #'string=)
			      (,flet-name sym))))))))
	     (iterate-over-hash-table (package-internal-symbols package) nil)
	     (iterate-over-hash-table (package-external-symbols package) nil)
	     (dolist (use (package-%use-list package))
	       (iterate-over-hash-table (package-external-symbols use)
					shadows)))))
       (let ((,var nil))
	 (declare (ignorable ,var))
	 ,@decls
	 ,result-form))))

(defmacro do-external-symbols ((var &optional (package '*package*) result-form)
			       &parse-body (body decls))
  "DO-EXTERNAL-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECL}* {TAG | FORM}*
   Executes the FORMs once for each external symbol in the given PACKAGE with
   VAR bound to the current symbol."
  (let ((flet-name (gensym "DO-SYMBOLS-")))
    `(block nil
       (flet ((,flet-name (,var)
		,@decls
		(tagbody ,@body)))
	 (let* ((package (package-or-lose ,package))
		(table (package-external-symbols package))
		(hash-vec (package-hashtable-hash table))
		(sym-vec (package-hashtable-table table)))
	   (declare (type (simple-array (unsigned-byte 8) (*))
			  hash-vec)
		    (type simple-vector sym-vec))
	   (dotimes (i (length sym-vec))
	     (when (>= (aref hash-vec i) 2)
	       (,flet-name (aref sym-vec i))))))
       (let ((,var nil))
	 (declare (ignorable ,var))
	 ,@decls
	 ,result-form))))

(defmacro do-all-symbols ((var &optional result-form) &parse-body (body decls))
  "DO-ALL-SYMBOLS (VAR [RESULT-FORM]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs once for each symbol in every package with VAR bound
   to the current symbol."
  (let ((flet-name (gensym "DO-SYMBOLS-")))
    `(block nil
       (flet ((,flet-name (,var)
		,@decls
		(tagbody ,@body)))
	 (dolist (package (list-all-packages))
	   (flet ((iterate-over-hash-table (table)
		    (let ((hash-vec (package-hashtable-hash table))
			  (sym-vec (package-hashtable-table table)))
		      (declare (type (simple-array (unsigned-byte 8) (*))
				     hash-vec)
			       (type simple-vector sym-vec))
		      (dotimes (i (length sym-vec))
			(when (>= (aref hash-vec i) 2)
			  (,flet-name (aref sym-vec i)))))))
	     (iterate-over-hash-table (package-internal-symbols package))
	     (iterate-over-hash-table (package-external-symbols package)))))
       (let ((,var nil))
	 (declare (ignorable ,var))
	 ,@decls
	 ,result-form))))


;;;; WITH-PACKAGE-ITERATOR

(defmacro with-package-iterator ((mname package-list &rest symbol-types)
				 &body body)
  "Within the lexical scope of the body forms, MNAME is defined via macrolet
   such that successive invocations of (mname) will return the symbols,
   one by one, from the packages in PACKAGE-LIST. SYMBOL-TYPES may be
   any of :inherited :external :internal."
  (let* ((packages (gensym))
	 (these-packages (gensym))
	 (ordered-types (let ((res nil))
			  (dolist (kind '(:inherited :external :internal)
					res)
			    (when (member kind symbol-types)
			      (push kind res)))))  ; Order symbol-types.
	 (counter (gensym))
	 (kind (gensym))
	 (hash-vector (gensym))
	 (vector (gensym))
	 (package-use-list (gensym))
	 (init-macro (gensym))
	 (end-test-macro (gensym))
	 (real-symbol-p (gensym))
	 (inherited-symbol-p (gensym))
	 (BLOCK (gensym)))
    `(let* ((,these-packages ,package-list)
	    (,packages `,(mapcar #'(lambda (package)
				     (if (packagep package)
					 package
					 (or (find-package package)
					     (error 'simple-package-error
						    :name (string package)
						    :format-control "~@<~S does not name a package ~:>"
						    :format-arguments (list package)))))
				 (if (consp ,these-packages)
				     ,these-packages
				     (list ,these-packages))))
	    (,counter nil)
	    (,kind (car ,packages))
	    (,hash-vector nil)
	    (,vector nil)
	    (,package-use-list nil))
       ,(if (member :inherited ordered-types)
	    `(setf ,package-use-list (package-%use-list (car ,packages)))
	    `(declare (ignore ,package-use-list)))
       (macrolet ((,init-macro (next-kind)
 	 (let ((symbols (gensym)))
	   `(progn
	      (setf ,',kind ,next-kind)
	      (setf ,',counter nil)
	      ,(case next-kind
		 (:internal
		  `(let ((,symbols (package-internal-symbols
				    (car ,',packages))))
		     (when ,symbols
		       (setf ,',vector (package-hashtable-table ,symbols))
		       (setf ,',hash-vector (package-hashtable-hash ,symbols)))))
		 (:external
		  `(let ((,symbols (package-external-symbols
				    (car ,',packages))))
		     (when ,symbols
		       (setf ,',vector (package-hashtable-table ,symbols))
		       (setf ,',hash-vector
			     (package-hashtable-hash ,symbols)))))
		 (:inherited
		  `(let ((,symbols (and ,',package-use-list
					(package-external-symbols
					 (car ,',package-use-list)))))
		     (when ,symbols
		       (setf ,',vector (package-hashtable-table ,symbols))
		       (setf ,',hash-vector
			     (package-hashtable-hash ,symbols)))))))))
		  (,end-test-macro (this-kind)
		     `,(let ((next-kind (cadr (member this-kind
						      ',ordered-types))))
			 (if next-kind
			     `(,',init-macro ,next-kind)
			     `(if (endp (setf ,',packages (cdr ,',packages)))
				  (return-from ,',BLOCK)
				  (,',init-macro ,(car ',ordered-types)))))))
	 (when ,packages
	   ,(when (null symbol-types)
	      (simple-program-error "Must supply at least one of :internal, ~
	                             :external, or :inherited."))
	   ,(dolist (symbol symbol-types)
	      (unless (member symbol '(:internal :external :inherited))
		(simple-program-error "~S is not one of :internal, :external, ~
		                       or :inherited."
			              symbol)))
	   (,init-macro ,(car ordered-types))
	   (flet ((,real-symbol-p (number)
		    (> number 1)))
	     (macrolet ((,mname ()
	      `(block ,',BLOCK
		 (loop
		   (case ,',kind
		     ,@(when (member :internal ',ordered-types)
			 `((:internal
			    (setf ,',counter
				  (position-if #',',real-symbol-p ,',hash-vector
					       :start (if ,',counter
							  (1+ ,',counter)
							  0)))
			    (if ,',counter
				(return-from ,',BLOCK
				 (values t (svref ,',vector ,',counter)
					 ,',kind (car ,',packages)))
				(,',end-test-macro :internal)))))
		     ,@(when (member :external ',ordered-types)
			 `((:external
			    (setf ,',counter
				  (position-if #',',real-symbol-p ,',hash-vector
					       :start (if ,',counter
							  (1+ ,',counter)
							  0)))
			    (if ,',counter
				(return-from ,',BLOCK
				 (values t (svref ,',vector ,',counter)
					 ,',kind (car ,',packages)))
				(,',end-test-macro :external)))))
		     ,@(when (member :inherited ',ordered-types)
			 `((:inherited
			    (flet ((,',inherited-symbol-p (number)
				     (when (,',real-symbol-p number)
				       (let* ((p (position
						  number ,',hash-vector
						  :start (if ,',counter
							     (1+ ,',counter)
							     0)))
					      (s (svref ,',vector p)))
					 (eql (nth-value
					       1 (find-symbol
						  (symbol-name s)
						  (car ,',packages)))
					      :inherited)))))
			      (setf ,',counter
				    (position-if #',',inherited-symbol-p
						 ,',hash-vector
						 :start (if ,',counter
							    (1+ ,',counter)
							    0))))
			    (cond (,',counter
				   (return-from
				    ,',BLOCK
				    (values t (svref ,',vector ,',counter)
					    ,',kind (car ,',packages))
				    ))
				  (t
				   (setf ,',package-use-list
					 (cdr ,',package-use-list))
				   (cond ((endp ,',package-use-list)
					  (setf ,',packages (cdr ,',packages))
					  (when (endp ,',packages)
					    (return-from ,',BLOCK))
					  (setf ,',package-use-list
						(package-%use-list
						 (car ,',packages)))
					  (,',init-macro ,(car ',ordered-types)))
					 (t (,',init-macro :inherited)
					    (setf ,',counter nil)))))))))))))
	       ,@body)))))))

;;;; DEFPACKAGE:

(defmacro defpackage (package &rest options)
  "Defines a new package called PACKAGE.  Each of OPTIONS should be one of the
   following:
     (:NICKNAMES {package-name}*)
     (:SIZE <integer>)
     (:SHADOW {symbol-name}*)
     (:SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
     (:USE {package-name}*)
     (:IMPORT-FROM <package-name> {symbol-name}*)
     (:INTERN {symbol-name}*)
     (:EXPORT {symbol-name}*)
     (:DOCUMENTATION doc-string)
   All options except :SIZE and :DOCUMENTATION can be used multiple times."
  (let ((nicknames nil)
	(size nil)
	(shadows nil)
	(shadowing-imports nil)
	(use nil)
	(use-p nil)
	(imports nil)
	(interns nil)
	(exports nil)
	(doc nil))
    (dolist (option options)
      (unless (consp option)
	(simple-program-error "Bogus DEFPACKAGE option: ~S" option))
      (case (car option)
	(:nicknames
	 (setf nicknames (stringify-names (cdr option) "package")))
	(:size
	 (cond (size
		(simple-program-error "Can't specify :SIZE twice."))
	       ((and (consp (cdr option))
		     (typep (second option) 'unsigned-byte))
		(setf size (second option)))
	       (t
		(simple-program-error
		 "Bogus :SIZE, must be a positive integer: ~S"
		 (second option)))))
	(:shadow
	 (let ((new (stringify-names (cdr option) "symbol")))
	   (setf shadows (append shadows new))))
	(:shadowing-import-from
	 (let ((package-name (stringify-name (second option) "package"))
	       (names (stringify-names (cddr option) "symbol")))
	   (let ((assoc (assoc package-name shadowing-imports
			       :test #'string=)))
	     (if assoc
		 (setf (cdr assoc) (append (cdr assoc) names))
		 (setf shadowing-imports
		       (acons package-name names shadowing-imports))))))
	(:use
	 (let ((new (stringify-names (cdr option) "package")))
	   (setf use (delete-duplicates (nconc use new) :test #'string=))
	   (setf use-p t)))
	(:import-from
	 (let ((package-name (stringify-name (second option) "package"))
	       (names (stringify-names (cddr option) "symbol")))
	   (let ((assoc (assoc package-name imports
			       :test #'string=)))
	     (if assoc
		 (setf (cdr assoc) (append (cdr assoc) names))
		 (setf imports (acons package-name names imports))))))
	(:intern
	 (let ((new (stringify-names (cdr option) "symbol")))
	   (setf interns (append interns new))))
	(:export
	 (let ((new (stringify-names (cdr option) "symbol")))
	   (setf exports (append exports new))))
	(:documentation
	 (when doc
	   (simple-program-error "Can't specify :DOCUMENTATION twice."))
	 (setf doc (coerce (second option) 'simple-string)))
	(t
	 (simple-program-error "Bogus DEFPACKAGE option: ~S" option))))
    (check-disjoint `(:intern ,@interns) `(:export  ,@exports))
    (check-disjoint `(:intern ,@interns)
		    `(:import-from
		      ,@(apply #'append (mapcar #'rest imports)))
		    `(:shadow ,@shadows)
		    `(:shadowing-import-from
		      ,@(apply #'append (mapcar #'rest shadowing-imports))))
    `(eval-when (compile load eval)
       (%defpackage ,(stringify-name package "package") ',nicknames ',size
		    ',shadows ',shadowing-imports ',(if use-p use :default)
		    ',imports ',interns ',exports ',doc))))

(defun check-disjoint (&rest args)
  ;; Check whether all given arguments specify disjoint sets of symbols.
  ;; Each argument is of the form (:key . set).
  (loop for (current-arg . rest-args) on args
        do
	(loop with (key1 . set1) = current-arg
	      for (key2 . set2) in rest-args
	      for common = (delete-duplicates
	                    (intersection set1 set2 :test #'string=))
	      unless (null common)
	      do
	      (simple-program-error "Parameters ~S and ~S must be disjoint ~
	                             but have common elements ~%   ~S"
				    key1 key2 common))))

(defun %defpackage (name nicknames size shadows shadowing-imports
			 use imports interns exports doc-string)
  (declare (type simple-base-string name)
	   (type list nicknames shadows shadowing-imports
		 imports interns exports)
	   (type (or list (member :default)) use)
	   (type (or simple-base-string null) doc-string))
  (let ((package (or (find-package name)
		     (progn
		       (when (eq use :default)
			 (setf use *default-package-use-list*))
		       (make-package name
				     :use nil
				     :internal-symbols (or size 10)
				     :external-symbols (length exports))))))
    (unless (string= (the string (package-name package)) name)
      (error 'simple-package-error
	     :package name
	     :format-control "~A is a nick-name for the package ~A"
	     :format-arguments (list name (package-name name))))
    (enter-new-nicknames package nicknames)
    ;; Shadows and Shadowing-imports.
    (let ((old-shadows (package-%shadowing-symbols package)))
      (shadow shadows package)
      (dolist (sym-name shadows)
	(setf old-shadows (remove (find-symbol sym-name package) old-shadows)))
      (dolist (simports-from shadowing-imports)
	(let ((other-package (package-or-lose (car simports-from))))
	  (dolist (sym-name (cdr simports-from))
	    (let ((sym (find-or-make-symbol sym-name other-package)))
	      (shadowing-import sym package)
	      (setf old-shadows (remove sym old-shadows))))))
      (when old-shadows
	(warn "~A also shadows the following symbols:~%  ~S"
	      name old-shadows)))
    ;; Use
    (unless (eq use :default)
      (let ((old-use-list (package-use-list package))
	    (new-use-list (mapcar #'package-or-lose use)))
	(use-package (set-difference new-use-list old-use-list) package)
	(let ((laterize (set-difference old-use-list new-use-list)))
	  (when laterize
	    (unuse-package laterize package)
	    (warn "~A previously used the following packages:~%  ~S"
		  name
		  laterize)))))
    ;; Import and Intern.
    (dolist (sym-name interns)
      (intern sym-name package))
    (dolist (imports-from imports)
      (let ((other-package (package-or-lose (car imports-from))))
	(dolist (sym-name (cdr imports-from))
	  (import (list (find-or-make-symbol sym-name other-package))
		  package))))
    ;; Exports.
    (let ((old-exports nil)
	  (exports (mapcar #'(lambda (sym-name) (intern sym-name package))
			   exports)))
      (do-external-symbols (sym package)
	(push sym old-exports))
      (export exports package)
      (let ((diff (set-difference old-exports exports)))
	(when diff
	  (warn "~A also exports the following symbols:~%  ~S"
		name diff))))
    ;; Documentation
    (setf (package-doc-string package) doc-string)
    package))

(defun find-or-make-symbol (name package)
  (multiple-value-bind
      (symbol how)
      (find-symbol name package)
    (cond (how
	   symbol)
	  (t
	   (with-simple-restart (continue "INTERN it.")
	     (error 'simple-package-error
		    :package package
		    :format-control "~A does not contain a symbol ~A"
		    :format-arguments (list (package-name package) name)))
	   (intern name package)))))


;;; Enter-New-Nicknames  --  Internal
;;;
;;;    Enter any new Nicknames for Package into *package-names*.
;;; If there is a conflict then give the user a chance to do
;;; something about it.
;;;
(defun enter-new-nicknames (package nicknames)
  (check-type nicknames list)
  (dolist (n nicknames)
    (let* ((n (package-namify n))
	   (found (package-name-to-package n)))
      (cond ((not found)
	     (setf (gethash n *package-names*) package)
	     (push n (package-%nicknames package)))
	    ((eq found package))
	    ((string= (the string (package-%name found)) n)
	     (with-simple-restart (continue "Ignore this nickname.")
	       (error 'simple-package-error
		      :package package
		      :format-control
		      "~S is a package name, so it cannot be a nickname for ~S."
		      :format-arguments (list n (package-%name package)))))
	    (t
	     (with-simple-restart (continue  "Redefine this nickname.")
	       (error 'simple-package-error
		      :package package
		      :format-control "~S is already a nickname for ~S."
		      :format-arguments (list n (package-%name found))))
	     (setf (gethash n *package-names*) package)
	     (push n (package-%nicknames package)))))))


;;; Make-Package  --  Public
;;;
;;;    Check for package name conflicts in name and nicknames, then
;;; make the package.  Do a use-package for each thing in the use list
;;; so that checking for conflicting exports among used packages is done.
;;;
(defun make-package (name &key (use *default-package-use-list*) nicknames
			  (internal-symbols 10) (external-symbols 10))
  "Makes a new package having the specified Name and Nicknames.  The
  package will inherit all external symbols from each package in
  the use list.  :Internal-Symbols and :External-Symbols are
  estimates for the number of internal and external symbols which
  will ultimately be present in the package."
  (when (find-package name)
    (cerror "Leave existing package alone."
	    "A package named ~S already exists" name))
  (let* ((name (package-namify name))
	 (package (internal-make-package
		   :%name name
		   :internal-symbols (make-package-hashtable internal-symbols)
		   :external-symbols (make-package-hashtable external-symbols))))
    (if *in-package-init*
	(push (list use package) *deferred-use-packages*)
	(use-package use package))
    (enter-new-nicknames package nicknames)
    (setf (gethash name *package-names*) package)))

;;; Old-In-Package  --  Sorta Public.
;;;
;;;    Like Make-Package, only different.  Should go away someday.
;;;
(defun old-in-package (name &rest keys &key nicknames use)
  "Sets *PACKAGE* to package with given NAME, creating the package if
   it does not exist.  If the package already exists then it is modified
   to agree with the :USE and :NICKNAMES arguments.  Any new nicknames
   are added without removing any old ones not specified.  If any package
   in the :Use list is not currently used, then it is added to the use
   list."
  (let ((package (find-package name)))
    (cond
     (package
      (if *in-package-init*
	  (push (list use package) *deferred-use-packages*)
	  (use-package use package))
      (enter-new-nicknames package nicknames)
      (setq *package* package))
     (t
      (setq *package* (apply #'make-package name keys))))))

;;; IN-PACKAGE -- public.
;;; 
(defmacro in-package (package &rest noise)
  (cond ((or noise
	     (not (or (stringp package) (symbolp package))))
	 (warn "Old-style IN-PACKAGE.")
	 `(old-in-package ,package ,@noise))
	(t
	 `(%in-package ',(stringify-name package "package")))))
;;;
(defun %in-package (name)
  (let ((package (find-package name)))
    (unless package
      (with-simple-restart (continue "Make this package.")
	(error 'simple-package-error
	       :package name
	       :format-control "The package named ~S doesn't exist."
	       :format-arguments (list name)))
      (setq package (make-package name)))
    (setf *package* package)))

;;; Rename-Package  --  Public
;;;
;;;    Change the name if we can, blast any old nicknames and then
;;; add in any new ones.
;;;
(defun rename-package (package name &optional (nicknames ()))
  "Changes the name and nicknames for a package."
  (let* ((package (package-or-lose package))
	 (name (string name))
	 (found (find-package name)))
    (unless (or (not found) (eq found package))
      (error 'simple-package-error
             :package name
             :format-control "A package named ~S already exists."
             :format-arguments (list name)))
    (remhash (package-%name package) *package-names*)
    (dolist (n (package-%nicknames package))
      (remhash n *package-names*))
     (setf (package-%name package) name)
    (setf (gethash name *package-names*) package)
    (setf (package-%nicknames package) ())
    (enter-new-nicknames package nicknames)
    package))

;;; Delete-Package -- Public
;;;
(defun delete-package (package-or-name)
  "Delete the PACKAGE-OR-NAME from the package system data structures."
  (let ((package (if (packagep package-or-name)
		     package-or-name
		     (find-package package-or-name))))
    (cond ((not package)
	   (with-simple-restart (continue "Return NIL")
	     (error 'simple-package-error
		    :package package-or-name
		    :format-control "No package of name ~S."
		    :format-arguments (list package-or-name)))
	   nil)
	  ((not (package-name package)) nil)
	  (t
	   (let ((use-list (package-used-by-list package)))
	     (when use-list
	       (with-simple-restart
		   (continue "Remove dependency in other packages.")
		 (error 'simple-package-error
			:package package
			:format-control
			"Package ~S is used by package(s):~%  ~S"
			:format-arguments
			(list (package-name package)
			      (mapcar #'package-name use-list))))
	       (dolist (p use-list)
		 (unuse-package package p))))
	   (dolist (used (package-use-list package))
	     (unuse-package used package))
	   (do-symbols (sym package)
	     (unintern sym package))
	   (remhash (package-name package) *package-names*)
	   (dolist (nick (package-nicknames package))
	     (remhash nick *package-names*))
	   (setf (package-%name package) nil)
	   t))))

;;; List-All-Packages  --  Public
;;;
;;;
(defun list-all-packages ()
  "Returns a list of all existing packages."
  (let ((res ()))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (pushnew v res))
	     *package-names*)
    res))

;;; Intern  --  Public
;;;
;;;    Simple-stringify the name and call intern*.
;;;
(defun intern (name &optional package)
  "Returns a symbol having the specified name, creating it if necessary."
  (let ((name (if (simple-string-p name) name (coerce name 'simple-string)))
        (package (if package (package-or-lose package) *package*)))
    (declare (type simple-string name))
    (intern* name (length name) package)))

;;; Find-Symbol  --  Public
;;;
;;;    Ditto.
;;;
(defun find-symbol (name &optional package)
  "Returns the symbol NAME in PACKAGE.  If such a symbol is found
  then the second value is :internal, :external or :inherited to indicate
  how the symbol is accessible.  If no symbol is found then both values
  are NIL."
  (let ((name (if (simple-string-p name) name (coerce name 'simple-string))))
    (declare (simple-string name))
    (find-symbol* name (length name)
		  (if package (package-or-lose package) *package*))))

;;; Intern*  --  Internal
;;;
;;;    If the symbol doesn't exist then create it, special-casing
;;; the keyword package.
;;;
(defun intern* (name length package)
  (declare (simple-string name))
  (multiple-value-bind (symbol where) (find-symbol* name length package)
    (if where
	(values symbol where)
        (progn
          #+(or)
          (when *enable-package-locked-errors*
            (when (ext:package-lock package)
              (restart-case
                  (error 'package-locked-error
                         :package package
                         :format-control "interning symbol ~A"
                         :format-arguments (list (subseq name 0 length)))
                (continue ()
                  :report "Ignore the lock and continue")
                (unlock-package ()
                  :report "Unlock package, then continue"
                  (setf (ext:package-lock package) nil))
                (unlock-all ()
                  :report "Unlock all packages, then continue"
                  (unlock-all-packages)))))
          (let ((symbol (make-symbol (subseq name 0 length))))
            (%set-symbol-package symbol package)
            (cond ((eq package *keyword-package*)
                   (add-symbol (package-external-symbols package) symbol)
                   (%set-symbol-value symbol symbol))
                  (t
                   (add-symbol (package-internal-symbols package) symbol)))
            (values symbol nil))))))

;;; find-symbol*  --  Internal
;;;
;;;    Check internal and external symbols, then scan down the list
;;; of hashtables for inherited symbols.  When an inherited symbol
;;; is found pull that table to the beginning of the list.
;;;
(defun find-symbol* (string length package)
  (declare (simple-string string)
	   (type index length))
  (let* ((hash (%sxhash-simple-substring string length))
	 (ehash (entry-hash length hash)))
    (declare (type index hash ehash))
    (with-symbol (found symbol (package-internal-symbols package)
			string length hash ehash)
      (when found
	(return-from find-symbol* (values symbol :internal))))
    (with-symbol (found symbol (package-external-symbols package)
			string length hash ehash)
      (when found
	(return-from find-symbol* (values symbol :external))))
    (let ((head (package-tables package)))
      (do ((prev head table)
	   (table (cdr head) (cdr table)))
	  ((null table) (values nil nil))
	(with-symbol (found symbol (car table) string length hash ehash)
	  (when found
	    (unless (eq prev head)
	      (shiftf (cdr prev) (cdr table) (cdr head) table))
	    (return-from find-symbol* (values symbol :inherited))))))))

;;; find-external-symbol  --  Internal
;;;
;;;    Similar to find-symbol, but only looks for an external symbol.
;;; This is used for fast name-conflict checking in this file and symbol
;;; printing in the printer.
;;;
(defun find-external-symbol (string package)
  (declare (simple-string string))
  (let* ((length (length string))
	 (hash (%sxhash-simple-string string))
	 (ehash (entry-hash length hash)))
    (declare (type index length hash))
    (with-symbol (found symbol (package-external-symbols package)
			string length hash ehash)
      (values symbol found))))

;;; unintern  --  Public
;;;
;;;    If we are uninterning a shadowing symbol, then a name conflict can
;;; result, otherwise just nuke the symbol.
;;;
(defun unintern (symbol &optional (package *package*))
  "Makes SYMBOL no longer present in PACKAGE.  If SYMBOL was present
  then T is returned, otherwise NIL.  If PACKAGE is SYMBOL's home
  package, then it is made uninterned."
  (let* ((package (package-or-lose package))
	 (name (symbol-name symbol))
	 (shadowing-symbols (package-%shadowing-symbols package)))
    (declare (list shadowing-symbols) (simple-string name))
    (when *enable-package-locked-errors*
      (when (ext:package-lock package)
        (restart-case
            (error 'package-locked-error
                   :package package
                   :format-control "uninterning symbol ~A"
                   :format-arguments (list name))
          (continue ()
            :report "Ignore the lock and continue")
          (unlock-package ()
            :report "Disable package's lock then continue"
            (setf (ext:package-lock package) nil))
          (unlock-all ()
            :report "Unlock all packages, then continue"
            (unlock-all-packages)))))
    ;;
    ;; If a name conflict is revealed, give use a chance to shadowing-import
    ;; one of the accessible symbols.
    (when (member symbol shadowing-symbols)
      (let ((cset ()))
	(dolist (p (package-%use-list package))
	  (multiple-value-bind (s w) (find-external-symbol name p)
	    (when w (pushnew s cset))))
	(when (cdr cset)
	  (loop
	   (cerror
	    "prompt for a symbol to shadowing-import."
	    'simple-package-error
	    :package package
	    :format-control
	    "Uninterning symbol ~S causes name conflict among these symbols:~%~S"
	    :format-arguments (list symbol cset))
	   (write-string "Symbol to shadowing-import: " *query-io*)
	   (let ((sym (read *query-io*)))
	     (cond
	      ((not (symbolp sym))
	       (format *query-io* "~S is not a symbol." sym))
	      ((not (member sym cset))
	       (format *query-io* "~S is not one of the conflicting symbols."
		       sym))
	      (t
	       (shadowing-import sym package)
	       (return-from unintern t)))))))
      (setf (package-%shadowing-symbols package)
	    (remove symbol shadowing-symbols)))

    (multiple-value-bind (s w) (find-symbol name package)
      (declare (ignore s))
      (cond ((or (eq w :internal) (eq w :external))
	     (nuke-symbol (if (eq w :internal)
			      (package-internal-symbols package)
			      (package-external-symbols package))
			  name)
	     (if (eq (symbol-package symbol) package)
		 (%set-symbol-package symbol nil))
	     t)
	    (t nil)))))

;;; Symbol-Listify  --  Internal
;;;
;;;    Take a symbol-or-list-of-symbols and return a list, checking types.
;;;
(defun symbol-listify (thing)
  (cond ((listp thing)
	 (dolist (s thing)
	   (unless (symbolp s) (error "~S is not a symbol." s)))
	 thing)
	((symbolp thing) (list thing))
	(t
	 (error "~S is neither a symbol nor a list of symbols." thing))))

;;; Moby-Unintern  --  Internal
;;;
;;;    Like Unintern, but if symbol is inherited chases down the
;;; package it is inherited from and uninterns it there.  Used
;;; for name-conflict resolution.  Shadowing symbols are not
;;; uninterned since they do not cause conflicts.
;;;
(defun moby-unintern (symbol package)
  (unless (member symbol (package-%shadowing-symbols package))
    (or (unintern symbol package)
	(let ((name (symbol-name symbol)))
	  (multiple-value-bind (s w) (find-symbol name package)
	    (declare (ignore s))
	    (when (eq w :inherited)
	      (dolist (q (package-%use-list package))
		(multiple-value-bind (u x) (find-external-symbol name q)
		  (declare (ignore u))
		  (when x
		    (unintern symbol q)
		    (return t))))))))))

;;; Export  --  Public
;;;
;;;    Do more stuff.
;;;
(defun export (symbols &optional (package *package*))
  "Exports SYMBOLS from PACKAGE, checking that no name conflicts result."
  (let ((package (package-or-lose package))
	(syms ()))
    ;;
    ;; Punt any symbols that are already external.
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w)
			   (find-external-symbol (symbol-name sym) package)
	(declare (ignore s))
	(unless (or w (member sym syms)) (push sym syms))))
    ;;
    ;; Find symbols and packages with conflicts.
    (let ((used-by (package-%used-by-list package))
	  (cpackages ())
	  (cset ()))
      (dolist (sym syms)
	(let ((name (symbol-name sym)))
	  (dolist (p used-by)
	    (multiple-value-bind (s w) (find-symbol name p)
	      (when (and w (not (eq s sym))
			 (not (member s (package-%shadowing-symbols p))))
		(pushnew sym cset)
		(pushnew p cpackages))))))
      (when cset
	(restart-case
	    (error
	     'simple-package-error
	     :package package
	     :format-control
	     "Exporting these symbols from the ~A package:~%~S~%~
	      results in name conflicts with these packages:~%~{~A ~}"
	     :format-arguments
	     (list (package-%name package) cset
		   (mapcar #'package-%name cpackages)))
	  (unintern-conflicting-symbols ()
	   :report "Unintern conflicting symbols."
	   (dolist (p cpackages)
	     (dolist (sym cset)
	       (moby-unintern sym p))))
	  (skip-exporting-these-symbols ()
	   :report "Skip exporting conflicting symbols."
	   (setq syms (nset-difference syms cset))))))
    ;;
    ;; Check that all symbols are accessible.  If not, ask to import them.
    (let ((missing ())
	  (imports ()))
      (dolist (sym syms)
	(multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	  (cond ((not (and w (eq s sym))) (push sym missing))
		((eq w :inherited) (push sym imports)))))
      (when missing
	(with-simple-restart
	    (continue "Import these symbols into the ~A package."
	      (package-%name package))
	  (error 'simple-package-error
		 :package package
		 :format-control
		 "These symbols are not accessible in the ~A package:~%~S"
		 :format-arguments
		 (list (package-%name package) missing)))
	(import missing package))
      (import imports package))
    ;;
    ;; And now, three pages later, we export the suckers.
    (let ((internal (package-internal-symbols package))
	  (external (package-external-symbols package)))
      (dolist (sym syms)
	(nuke-symbol internal (symbol-name sym))
	(add-symbol external sym)))
    t))

;;; Unexport  --  Public
;;;
;;;    Check that all symbols are accessible, then move from external to
;;; internal.
;;;
(defun unexport (symbols &optional (package *package*))
  "Makes SYMBOLS no longer exported from PACKAGE."
  (let ((package (package-or-lose package))
	(syms ()))
    (when *enable-package-locked-errors*
      (when (ext:package-lock package)
        (restart-case
            (error 'package-locked-error
                   :package package
                   :format-control "unexporting symbols ~A"
                   :format-arguments (list symbols))
          (continue ()
            :report "Ignore the lock and continue")
          (unlock-package ()
            :report "Disable package's lock then continue"
            (setf (ext:package-lock package) nil))
          (unlock-all ()
            :report "Unlock all packages, then continue"
            (unlock-all-packages)))))
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(cond ((or (not w) (not (eq s sym)))
	       (error 'simple-package-error
		      :package package
		      :format-control "~S is not accessible in the ~A package."
		      :format-arguments (list sym (package-%name package))))
	      ((eq w :external) (pushnew sym syms)))))

    (let ((internal (package-internal-symbols package))
	  (external (package-external-symbols package)))
      (dolist (sym syms)
	(add-symbol internal sym)
	(nuke-symbol external (symbol-name sym))))
    t))

;;; Import  --  Public
;;;
;;;    Check for name conflic caused by the import and let the user 
;;; shadowing-import if there is.
;;;
(defun import (symbols &optional (package *package*))
  "Make SYMBOLS accessible as internal symbols in PACKAGE.  If a symbol
  is already accessible then it has no effect.  If a name conflict
  would result from the importation, then a correctable error is signalled."
  (let ((package (package-or-lose package))
	(symbols (symbol-listify symbols))
	(syms ())
	(cset ()))
    (dolist (sym symbols)
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(cond ((not w)
	       (let ((found (member sym syms :test #'string=)))
		 (if found
		     (when (not (eq (car found) sym))
		       (push sym cset))
		     (push sym syms))))
	      ((not (eq s sym)) (push sym cset))
	      ((eq w :inherited) (push sym syms)))))
    (when cset
      (with-simple-restart
	  (continue "Import these symbols with Shadowing-Import.")
	(error 'simple-package-error
	       :package package
	       :format-control
	       "Importing these symbols into the ~A package ~
		causes a name conflict:~%~S"
	       :format-arguments (list (package-%name package) cset))))
    ;;
    ;; Add the new symbols to the internal hashtable.
    (let ((internal (package-internal-symbols package)))
      (dolist (sym syms)
	(add-symbol internal sym)))
    ;;
    ;; If any of the symbols are uninterned, make them be owned by Package.
    (dolist (sym symbols)
      (unless (symbol-package sym) (%set-symbol-package sym package)))
    (shadowing-import cset package)))

;;; Shadowing-Import  --  Public
;;;
;;;    If a conflicting symbol is present, unintern it, otherwise just
;;; stick the symbol in.
;;;
(defun shadowing-import (symbols &optional (package *package*))
  "Import SYMBOLS into PACKAGE, disregarding any name conflict.  If
  a symbol of the same name is present, then it is uninterned.
  The symbols are added to the Package-Shadowing-Symbols."
  (let* ((package (package-or-lose package))
	 (internal (package-internal-symbols package)))
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(unless (and w (not (eq w :inherited)) (eq s sym))
	  (when (or (eq w :internal) (eq w :external))
	    ;;
	    ;; If it was shadowed, we don't want Unintern to flame out...
	    (setf (package-%shadowing-symbols package)
		  (remove s (the list (package-%shadowing-symbols package))))
	    (unintern s package))
	  (add-symbol internal sym))
	(pushnew sym (package-%shadowing-symbols package)))))
  t)


;;; Shadow  --  Public
;;;
;;;
(defun shadow (symbols &optional (package *package*))
  "Make an internal symbol in PACKAGE with the same name as each of the
  specified SYMBOLS, adding the new symbols to the Package-Shadowing-Symbols.
  If a symbol with the given name is already present in PACKAGE, then
  the existing symbol is placed in the shadowing symbols list if it is
  not already present."
  (let* ((package (package-or-lose package))
	 (internal (package-internal-symbols package)))
    (dolist (name (mapcar #'string
			  (if (listp symbols) symbols (list symbols))))
      (multiple-value-bind (s w) (find-symbol name package)
	(when (or (not w) (eq w :inherited))
	  (setq s (make-symbol name))
	  (%set-symbol-package s package)
	  (add-symbol internal s))
	(pushnew s (package-%shadowing-symbols package)))))
  t)



;;; Use-Package  --  Public
;;;
;;;    Do stuff to use a package, with all kinds of fun name-conflict
;;; checking.
;;;
(defun use-package (packages-to-use &optional (package *package*))
  "Add all the PACKAGES-TO-USE to the use list for PACKAGE so that
  the external symbols of the used packages are accessible as internal
  symbols in PACKAGE."
  (let ((packages (package-listify packages-to-use))
	(package (package-or-lose package)))
    ;;
    ;; Loop over each package, use'ing one at a time...
    (dolist (pkg packages)
      (unless (member pkg (package-%use-list package))
	(let ((cset ())
	      (shadowing-symbols (package-%shadowing-symbols package))
	      (use-list (package-%use-list package)))
	  ;;
	  ;;   If the number of symbols already accessible is less than the
	  ;; number to be inherited then it is faster to run the test the
	  ;; other way.  This is particularly valuable in the case of
	  ;; a new package use'ing Lisp.
	  (cond
	   ((< (+ (internal-symbol-count package)
		  (external-symbol-count package)
		  (let ((res 0))
		    (dolist (p use-list res)
		      (incf res (external-symbol-count p)))))
	       (external-symbol-count pkg))
	    (do-symbols (sym package)
	      (multiple-value-bind (s w)
		  (find-external-symbol (symbol-name sym) pkg)
		(when (and w (not (eq s sym))
			   (not (member sym shadowing-symbols)))
		  (push sym cset))))
	    (dolist (p use-list)
	      (do-external-symbols (sym p)
		(multiple-value-bind (s w)
		    (find-external-symbol (symbol-name sym) pkg)
		  (when (and w (not (eq s sym))
			     (not (member (find-symbol (symbol-name sym)
						       package)
					  shadowing-symbols)))
		    (push sym cset))))))
	   (t
	    (do-external-symbols (sym pkg)
	      (multiple-value-bind (s w)
		  (find-symbol (symbol-name sym) package)
		(when (and w (not (eq s sym))
			   (not (member s shadowing-symbols)))
		  (push s cset))))))
	  
	  (when cset
	    (cerror
	     "Unintern the conflicting symbols in the ~2*~A package."
	     "Use'ing package ~A results in name conflicts for these symbols:~%~S"
	     (package-%name pkg) cset (package-%name package))
	    (dolist (s cset) (moby-unintern s package))))

	(push pkg (package-%use-list package))
	(push (package-external-symbols pkg) (cdr (package-tables package)))
	(push package (package-%used-by-list pkg)))))
  t)

;;; Unuse-Package  --  Public
;;;
;;;
(defun unuse-package (packages-to-unuse &optional (package *package*))
  "Remove PACKAGES-TO-UNUSE from the use list for PACKAGE."
  (let ((package (package-or-lose package)))
    (dolist (p (package-listify packages-to-unuse))
      (setf (package-%use-list package)
	    (remove p (the list (package-%use-list package))))
      (setf (package-tables package)
	    (delete (package-external-symbols p)
		    (the list (package-tables package))))
      (setf (package-%used-by-list p)
	    (remove package (the list (package-%used-by-list p)))))
    t))

;;; Find-All-Symbols --  Public
;;;
;;;
(defun find-all-symbols (string-or-symbol)
  "Return a list of all symbols in the system having the specified name."
  (let ((string (string string-or-symbol))
	(res ()))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (multiple-value-bind (s w) (find-symbol string v)
		   (when w (pushnew s res))))
	     *package-names*)
    res))


;;; Apropos and Apropos-List.

(defun briefly-describe-symbol (symbol)
  (let ((prefix-length nil))
    (flet ((print-symbol (&optional kind)
             (fresh-line)
	     (if prefix-length
	         (dotimes (i prefix-length)
		   (write-char #\Space))
		 (let ((symbol-string (prin1-to-string symbol)))
		   (write-string symbol-string)
		   (setq prefix-length (length symbol-string))))
	     (when kind
	       (write-string " [")
	       (write-string kind)
	       (write-string "] "))))

      ;; Variable namespace
      (multiple-value-bind (kind recorded-p) (info variable kind symbol)
        (when (or (boundp symbol) recorded-p)
	  (print-symbol (ecase kind
                          (:special  "special variable")
                          (:constant "constant")
                          (:global   "undefined variable")
                          (:macro    "symbol macro")
                          (:alien    "alien variable")))
          (when (boundp symbol)
	    (write-string "value: ")
	    (let ((*print-length*
	             (or ext:*describe-print-length* *print-length*))
	          (*print-level*
		     (or ext:*describe-print-level* *print-level*)))
              (prin1 (symbol-value symbol))))))

      ;; Function namespace
      (when (fboundp symbol)
        (cond
          ((macro-function symbol)
           (print-symbol "macro")
           (let ((arglist (kernel:%function-arglist (macro-function symbol))))
             (when (stringp arglist) (write-string arglist))))
          ((special-operator-p symbol)
           (print-symbol "special operator")
           (let ((arglist (kernel:%function-arglist (symbol-function symbol))))
             (when (stringp arglist) (write-string arglist))))
          (t
           (print-symbol "function")
           ;; could do better than this with (kernel:type-specifier
           ;; (info function type symbol)) when it's a byte-compiled function
           (let ((arglist (kernel:%function-arglist (symbol-function symbol))))
             (when (stringp arglist) (write-string arglist))))))

      ;; Class and Type Namespace(s)
      (cond
        ((kernel::find-class symbol nil)
         (print-symbol "class"))
        ((info type kind symbol)
         (print-symbol "type")))

      ;; Make sure we at least print the symbol itself if we don't know
      ;; anything else about it:
      (when (null prefix-length)
        (print-symbol)))))

(defun apropos-search (symbol string)
  (declare (simple-string string))
  (do* ((index 0 (1+ index))
	(name (symbol-name symbol))
	(length (length string))
	(terminus (- (length name) length)))
       ((> index terminus)
	nil)
    (declare (simple-string name)
	     (type index index length)
	     (fixnum terminus))
    (if (do ((jndex 0 (1+ jndex))
	     (kndex index (1+ kndex)))
	    ((= jndex length)
	     t)
	  (declare (fixnum jndex kndex))
	  (let ((char (schar name kndex)))
	    (unless (char= (schar string jndex) (char-upcase char))
	      (return nil))))
	(return t))))

;;; MAP-APROPOS -- public (extension).
;;;
(defun map-apropos (fun string &optional package external-only)
  "Call FUN with each symbol that contains STRING.
  If PACKAGE is supplied then only use symbols present in
  that package.  If EXTERNAL-ONLY is true then only use
  symbols exported from the specified package."
  (let ((string (string-upcase string)))
    (declare (simple-string string))
    (flet ((apropos-in-package (package)
             (if external-only
                 (do-external-symbols (symbol package)
                   (if (and (eq (symbol-package symbol) package)
                            (apropos-search symbol string))
                       (funcall fun symbol)))
                 (do-symbols (symbol package)
                   (if (and (eq (symbol-package symbol) package)
                            (apropos-search symbol string))
                       (funcall fun symbol))))))
      (if (null package)
          (mapc #'apropos-in-package (list-all-packages))
          (apropos-in-package (package-or-lose package))))
    nil))

;;; APROPOS -- public.
;;; 
(defun apropos (string &optional package)
  "Briefly describe all symbols which contain the specified STRING.
  If PACKAGE is supplied then only describe symbols present in
  that package.  If EXTERNAL-ONLY is non-NIL then only describe
  external symbols in the specified package."
  (map-apropos #'briefly-describe-symbol string package)
  (values))

;;; APROPOS-LIST -- public.
;;; 
(defun apropos-list (string &optional package)
  "Identical to APROPOS, except that it returns a list of the symbols
  found instead of describing them."
  (collect ((result))
    (map-apropos #'(lambda (symbol)
		     (result symbol))
		 string package)
    (result)))



;;; Initialization.

;;; The cold loader (Genesis) makes the data structure in *initial-symbols*.
;;; We grovel over it, making the specified packages and interning the
;;; symbols.  For a description of the format of *initial-symbols* see
;;; the Genesis source.

(defvar *initial-symbols*)

(defun package-init ()
  (let ((*in-package-init* t))
    (dolist (spec *initial-symbols*)
      (let* ((pkg (apply #'make-package (first spec)))
	     (internal (package-internal-symbols pkg))
	     (external (package-external-symbols pkg)))
	;;
	;; Put internal symbols in the internal hashtable and set package.
	(dolist (symbol (second spec))
	  (add-symbol internal symbol)
	  (%set-symbol-package symbol pkg))
	;;
	;; External symbols same, only go in external table.
	(dolist (symbol (third spec))
	  (add-symbol external symbol)
	  (%set-symbol-package symbol pkg))
	;;
	;; Don't set package for Imported symbols.
	(dolist (symbol (fourth spec))
	  (add-symbol internal symbol))
	(dolist (symbol (fifth spec))
	  (add-symbol external symbol))
	;;
	;; Put shadowing symbols in the shadowing symbols list.
	(setf (package-%shadowing-symbols pkg) (sixth spec))))

    (makunbound '*initial-symbols*) ; So it gets GC'ed.
    
    ;; Make some other packages that should be around in the cold load:
    (make-package "COMMON-LISP-USER" :nicknames '("CL-USER"))

    ;; Now do the *deferred-use-packages*:
    (dolist (args *deferred-use-packages*)
      (apply #'use-package args))
    (makunbound '*deferred-use-packages*)

    (setq *lisp-package* (find-package "LISP"))
    (setq *keyword-package* (find-package "KEYWORD"))

    ;; For the kernel core image wizards, set the package to *Lisp-Package*.
    (setq *package* *lisp-package*)))
