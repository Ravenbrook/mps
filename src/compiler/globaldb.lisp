;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/globaldb.lisp,v 1.52 2005/11/10 17:43:15 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file provides a functional interface to global information about
;;; named things in the system.  Information is considered to be global if it
;;; must persist between invocations of the compiler.  The use of a functional
;;; interface eliminates the need for the compiler to worry about the actual
;;; representation.   This is important, since the information may well have
;;; several representations.   This code also deals with the need for multiple
;;; "global" environments, so that changing something in the compiler doesn't
;;; trash the running Lisp environment.
;;;
;;;    The database contains arbitrary Lisp values, addressed by a combination
;;; of Name, Class and Type.  The Name is a EQUAL-thing which is the name of
;;; the thing we are recording information about.  Class is the kind of object
;;; involved.  Typical classes are Function, Variable, Type, ...  A Type names
;;; a particular piece of information within a given class.  Class and Type are
;;; symbols, but are compared with STRING=.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")
(use-package "EXTENSIONS")
(use-package "SYSTEM")

(in-package "EXTENSIONS")
(export '(info clear-info define-info-class define-info-type
	       make-info-environment do-info *info-environment*
	       compact-info-environment))

(in-package "C")

;;; The defvar for this appears later.
(declaim (special *universal-type*))
(declaim (type list type-specifier-symbols))


;;; PRIMIFY  --  Internal
;;;
;;;    Given any non-negative integer, return a prime number >= to it.
;;;
(defun primify (x)
  (declare (type unsigned-byte x) (optimize (inhibit-warnings 2)))
  (do ((n (logior x 1) (+ n 2)))
      ((system:primep n) n)))


;;;; Defining info types:

(eval-when (compile load eval)

(defstruct (class-info
	    (:constructor make-class-info (name))
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<Class-Info ~S>" (class-info-name s)))))
  ;;
  ;; String name of this class.
  (name nil :type simple-string)
  ;;
  ;; List of Type-Info structures for each type in this class.
  (types () :type list))


;;; At run-time, we represent the type of info that we want by a small
;;; non-negative integer.
;;;
(defconstant type-number-bits 6)
(deftype type-number () `(unsigned-byte ,type-number-bits))
;;;
;;; Also initialized in GLOBALDB-INIT...
(defvar *type-numbers*
  (make-array (ash 1 type-number-bits)  :initial-element nil))


(defstruct (type-info
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<Type-Info ~S ~S, Number = ~D>"
		       (class-info-name (type-info-class s))
		       (type-info-name s)
		       (type-info-number s)))))
			     
  ;;
  ;; String name of this type.
  (name (required-argument) :type simple-string)
  ;;
  ;; This type's class.
  (class (required-argument) :type class-info)
  ;;
  ;; A number that uniquely identifies this type (and implicitly its class.)
  (number (required-argument) :type type-number)
  ;;
  ;; Type specifier which info of this type must satisfy.
  (type nil :type t)
  ;;
  ;; Function called when there is no information of this type.
  (default #'(lambda () (error "Type not defined yet.")) :type function))


;;; A hashtable from class names to Class-Info structures.  This data structure
;;; exists at compile time as well as run time.  Also initialized in
;;; GLOBALDB-INIT...
;;;
(defvar *info-classes* (make-hash-table :test #'equal))
(declaim (hash-table *info-classes*))


;;; FIND-TYPE-INFO  --  Internal
;;;
;;;    If Name is the name of a type in Class, then return the TYPE-INFO,
;;; otherwise NIL.
;;;
(defun find-type-info (name class)
  (declare (simple-string name) (type class-info class))
  (dolist (type (class-info-types class) nil)
    (when (string= (type-info-name type) name)
      (return type))))


;;; Class-Info-Or-Lose, Type-Info-Or-Lose --  Internal
;;;
;;;    Return the info structure for an info class or type, or die trying.
;;;
(defun class-info-or-lose (class)
  (declare (string class) (values class-info))
  (or (gethash class *info-classes*)
      (error "~S is not a defined info class." class)))
;;;
(defun type-info-or-lose (class type)
  (declare (string class type) (values type-info))
  (or (find-type-info type (class-info-or-lose class))
      (error "~S is not a defined info type." type)))


;;; Define-Info-Class  --  Public
;;;
;;;    Set up the data structures to support an info class.  We make sure that
;;; the class exists at compile time so that macros can use it, but don't
;;; actually store the init function until load time so that we don't break the
;;; running compiler.
;;;
(defmacro define-info-class (class)
  "Define-Info-Class Class
  Define a new class of global information."
  `(progn
     (eval-when (compile load eval)
       (%define-info-class ',(symbol-name class)))
     ',class))


;;; %Define-Info-Class  --  Internal
;;;
;;;    If there is no info for the class, then create it, otherwise do nothing.
;;;
(defun %define-info-class (class)
  (declare (string class))
  (unless (gethash class *info-classes*)
    (setf (gethash class *info-classes*) (make-class-info class))))


;;; FIND-UNUSED-TYPE-NUMBER  --  Internal
;;;
;;;    Find a type number not already in use by looking for a null entry in
;;; *TYPE-NUMBERS*.
;;;
(defun find-unused-type-number ()
  (or (position nil *type-numbers*)
      (error "Out of INFO type numbers!")))


;;; Define-Info-Type  --  Public
;;;
;;;    The main thing we do is determine the type's number.  We need to do this
;;; at macroexpansion time, since both the COMPILE and LOAD time calls to
;;; %DEFINE-INFO-TYPE must use the same type number.
;;;
(defmacro define-info-type (class type type-spec &optional default)
  "Define-Info-Type Class Type default Type-Spec
  Define a new type of global information for Class.  Type is the symbol name
  of the type, Default is the value for that type when it hasn't been set, and
  Type-Spec is a type-specifier which values of the type must satisfy.  The
  default expression is evaluated each time the information is needed, with
  Name bound to the name for which the information is being looked up.  If the
  default evaluates to something with the second value true, then the second
  value of Info will also be true."
  (let* ((class (symbol-name class))
	 (type (symbol-name type))
	 (old (find-type-info type (class-info-or-lose class))))
    `(progn
       (eval-when (compile load eval)
	 (%define-info-type ',class ',type ',type-spec
			    ,(if old
				 (type-info-number old)
				 (find-unused-type-number))))
       (eval-when (load eval)
	 (setf (type-info-default (type-info-or-lose ',class ',type))
	       #'(lambda (name) name ,default)))
       ',type)))


;;; %Define-Info-Type  --  Internal
;;;
;;;    If there is no such type, create it.  In any case, set the type
;;; specifier for the value.  The class must exist.
;;;
(defun %define-info-type (class type type-spec number)
  (declare (simple-string class type) (type type-number number))
  (let* ((class-info (class-info-or-lose class))
	 (old (find-type-info type class-info))
	 (res (or old
		  (make-type-info :name type
				  :class class-info
				  :number number
				  :type type-spec)))
	 (num-old (svref *type-numbers* number)))
    (cond (old
	   (setf (type-info-type res) type-spec)
	   (unless (= (type-info-number res) number)
	     (cerror "Redefine it." "Changing type number for ~A ~A."
		     class type)
	     (setf (type-info-number res) number)))
	  (t
	   (push res (class-info-types class-info))))

    (unless (eq num-old res)
      (when num-old
	(cerror "Go for it." "Reusing type number for ~A ~A."
		(class-info-name (type-info-class num-old))
		(type-info-name num-old)))
      (setf (svref *type-numbers* number) res)))

  (undefined-value))

); eval-when (compile load eval)


;;;; Info environments:
;;;
;;;    We do info access relative to the current *info-environment*.  This is a
;;; list of INFO-ENVIRONMENT structures we search.  The variable is actually
;;; initialized in GLOBALDB-INIT.

(defvar *info-environment*)
(declaim (type list *info-environment*))


(defun %print-info-environment (s stream d)
  (declare (ignore d) (stream stream)) 
  (format stream "#<~S ~S>" (type-of s) (info-env-name s)))


;;; Note: the CACHE-NAME slot is deliberately not shared for bootstrapping
;;; reasons.  If we access with accessors for the exact type, then the inline
;;; type check will win.  If the inline check didn't win, we would try to use
;;; the type system before it was properly initialized.
;;;
(defstruct (info-env (:print-function %print-info-environment))
  ;;
  ;; Some string describing what is in this environment, for printing purposes
  ;; only.
  (name (required-argument) :type string))


;;; INFO-HASH  --  Internal
;;;
;;;    Sorta semantically equivalent to SXHASH, but optimized for legal
;;; function names.  Note: semantically equivalent does *not* mean that it
;;; always returns the same value as SXHASH, just that it satisfies the formal
;;; definition of SXHASH.  The ``sorta'' is because symbol-hash will not
;;; necessarily return the same value in different lisps.
;;;
;;;    All we do for now is pick off the cases of a symbol and a list of two
;;; symbols [e.g. (SETF FOO)].  The symbol case is the same as what SXHASH
;;; does, but we get there more expeditiously.  With the two-list, we LOGXOR a
;;; random constant with the hash of the second symbol.
;;;
(declaim (inline info-hash))
(defun info-hash (x)
  (cond
   ((symbolp x)
    #-(or gengc x86 amd64 sparc ppc) (%sxhash-simple-string (symbol-name x))
    #+(or gengc x86 amd64 sparc ppc) (sxhash x))
   ((and (listp x)
	 (eq (car x) 'setf)
	 (let ((next (cdr x)))
	   (when (listp next)
	     (let ((name (car next)))
	       (when (and (symbolp name) (null (cdr next)))
		 (let ((sym name))
		   (declare (symbol sym))
		   (logxor #-(or gengc x86 amd64 sparc ppc) (%sxhash-simple-string (symbol-name sym))
			   #+(or gengc x86 amd64 sparc ppc) (sxhash sym)
			   110680597))))))))
   (t
    (sxhash x))))


;;;; Generic interfaces:

;;; Info  --  Public
;;;
;;;    This is a macro so that we can resolve the Class and Type to a type
;;; number at compile time.  When we check the new-value's type directly in the
;;; SETF expansion, since the check can be done much more efficiently when the
;;; type is constant.
;;;
(defmacro info (class type name &optional env-list)
  "Return the information of the specified Type and Class for Name.
   The second value is true if there is any such information recorded.  If
   there is no information, the first value is the default and the second value
   is NIL."
  ;;
  ;; ### Should be a values type, but interpreter can't hack that now.
  (let* ((class (symbol-name class))
	 (type (symbol-name type))
	 (info (type-info-or-lose class type)))
    `(truly-the (values ,(type-info-type info) t)
		(get-info-value ,name ,(type-info-number info)
				,@(when env-list `(,env-list))))))
;;;
(define-setf-expander info (class type name &optional env-list)
  "Set the global information for Name."
  (let* ((n-name (gensym))
	 (n-env-list (if env-list (gensym)))
	 (n-value (gensym))
	 (class-str (symbol-name class))
	 (type-str (symbol-name type))
	 (info (type-info-or-lose class-str type-str)))
    (values
     `(,n-name ,@(when env-list `(,n-env-list)))
     `(,name ,@(when env-list `(,env-list)))
     `(,n-value)
     `(progn
	(check-type ,n-value ,(type-info-type info))
	(set-info-value ,n-name ,(type-info-number info) ,n-value
			,@(when env-list
			    `((get-write-info-env ,n-env-list)))))
     `(info ,class ,type ,n-name ,@(when env-list `(,n-env-list))))))


;;; DO-INFO  --  Public
;;;
(defmacro do-info ((env &key (name (gensym)) (class (gensym)) (type (gensym))
			(type-number (gensym)) (value (gensym)) known-volatile)
		   &body body)
  "DO-INFO (Env &Key Name Class Type Value) Form*
  Iterate over all the values stored in the Info-Env Env.  Name is bound to
  the entry's name, Class and Type are bound to the class and type
  (represented as strings), and Value is bound to the entry's value."
  (once-only ((n-env env))
    (if known-volatile
	(do-volatile-info name class type type-number value n-env body)
	`(if (typep ,n-env 'volatile-info-env)
	     ,(do-volatile-info name class type type-number value n-env body)
	     ,(do-compact-info name class type type-number value
			       n-env body)))))


(eval-when (compile load eval)

;;; DO-COMPACT-INFO  --  Internal
;;;
;;;    Return code to iterate over a compact info environment.
;;;
(defun do-compact-info (name-var class-var type-var type-number-var value-var
				 n-env body)
  (let ((n-index (gensym)) (n-type (gensym)) (punt (gensym)))
    (once-only ((n-table `(compact-info-env-table ,n-env))
		(n-entries-index `(compact-info-env-index ,n-env))
		(n-entries `(compact-info-env-entries ,n-env))
		(n-entries-info `(compact-info-env-entries-info ,n-env))
		(n-type-numbers '*type-numbers*))
      `(dotimes (,n-index (length ,n-table))
	 (declare (type index ,n-index))
	 (block ,PUNT
	   (let ((,name-var (svref ,n-table ,n-index)))
	     (unless (eql ,name-var 0)
	       (do-anonymous ((,n-type (aref ,n-entries-index ,n-index)
				       (1+ ,n-type)))
			     (nil)
		 (declare (type index ,n-type))
		 ,(once-only ((n-info `(aref ,n-entries-info ,n-type)))
		    `(let ((,type-number-var
			    (logand ,n-info compact-info-entry-type-mask)))
		       ,(once-only ((n-type-info
				     `(svref ,n-type-numbers
					     ,type-number-var)))
			  `(let ((,type-var (type-info-name ,n-type-info))
				 (,class-var (class-info-name
					      (type-info-class ,n-type-info)))
				 (,value-var (svref ,n-entries ,n-type)))
			     (declare (ignorable ,type-var ,class-var
						 ,value-var))
			     ,@body
			     (unless (zerop (logand ,n-info compact-info-entry-last))
			       (return-from ,PUNT))))))))))))))

;;; DO-VOLATILE-INFO  --  Internal
;;;
;;;    Return code to iterate over a volatile info environment.
;;;
(defun do-volatile-info (name-var class-var type-var type-number-var value-var
				  n-env body)
  (let ((n-index (gensym)) (n-names (gensym)) (n-types (gensym)))
    (once-only ((n-table `(volatile-info-env-table ,n-env))
		(n-type-numbers '*type-numbers*))
      `(dotimes (,n-index (length ,n-table))
	 (declare (type index ,n-index))
	 (do-anonymous ((,n-names (svref ,n-table ,n-index)
				  (cdr ,n-names)))
		       ((null ,n-names))
	   (let ((,name-var (caar ,n-names)))
	     (declare (ignorable ,name-var))
	     (do-anonymous ((,n-types (cdar ,n-names) (cdr ,n-types)))
			   ((null ,n-types))
	       (let ((,type-number-var (caar ,n-types)))
		 ,(once-only ((n-type `(svref ,n-type-numbers
					      ,type-number-var)))
		    `(let ((,type-var (type-info-name ,n-type))
			   (,class-var (class-info-name
					(type-info-class ,n-type)))
			   (,value-var (cdar ,n-types)))
		       (declare (ignorable ,type-var ,class-var ,value-var))
		       ,@body))))))))))


); Eval-When (Compile Load Eval)


;;;; INFO cache:
;;;
;;;    We use a hash cache to cache name X type => value for the current value
;;; of *INFO-ENVIRONMENT*.  This is in addition to the per-environment caching
;;; of name => types.
;;;

;;; The value of *INFO-ENVIRONMENT* that has cached values.  *INFO-ENVIRONMENT*
;;; should nevern be destructively modified, so it is EQ to this, then the
;;; cache is valid.
;;;
(defvar *cached-info-environment*)


;;; INFO-CACHE-HASH  --  Internal
;;;
;;;    Hash function used for INFO cache.
;;;
(defmacro info-cache-hash (name type)
  `(the fixnum
	(logand
	 (the fixnum
	      (logxor (the fixnum
			   #+(or gengc x86) (info-hash ,name)
			   #-(or gengc x86) (cache-hash-eq ,name))
		      (the fixnum (ash (the fixnum ,type) 7))))
	 #x3FF)))


(define-hash-cache info ((name eq) (type eq))
  :values 2
  :hash-function info-cache-hash
  :hash-bits 10
  :default (values nil :empty))


;;; INFO-CACHE-INIT  --  Internal
;;;
;;;    Set up the info cache.  The top-level code of DEFINE-HASH-CACHE can't
;;; initialize the cache, since it must be initialized before we run any
;;; top-level forms. This is called in GLOBALDB-INIT.
;;;
(defun info-cache-init ()
  (setq *cached-info-environment* nil)
  (setq *info-cache-vector* (make-array (* 4 (ash 1 10))))
  (info-cache-clear)
  (undefined-value))


;;; Whenever we GC, we must blow away the INFO cache, otherwise values might
;;; become unreachable (and hence not be updated), and then could become
;;; reachable again in a future GC.
;;;
;;; Not needed with the gengc system, because we use an address independent
;;; hashing.
;;; 
#-(or gengc x86 amd64)
(defun info-cache-gc-hook ()
  (setq *cached-info-environment* nil))
;;;
#-(or gengc x86 amd64)
(pushnew 'info-cache-gc-hook *after-gc-hooks*)


;;; CLEAR-INVALID-INFO-CACHE  --  Internal
;;;
;;;    If the info cache is invalid, then clear it.
;;;
(declaim (inline clear-invalid-info-cache))
(defun clear-invalid-info-cache ()
  (unless (eq *info-environment* *cached-info-environment*)
    (without-interrupts
      (info-cache-clear)
      (setq *cached-info-environment* *info-environment*))))


;;;; Compact environments:

;;; The upper limit on the size of the ENTRIES vector in a COMPACT-INFO-ENV.
;;;
(defconstant compact-info-env-entries-bits 16)
(deftype compact-info-entries-index () `(unsigned-byte ,compact-info-env-entries-bits))


;;; Type of the values in COMPACT-INFO-ENTRIES-INFO.
;;;
(deftype compact-info-entry () `(unsigned-byte ,(1+ type-number-bits)))


;;; This is an open hashtable with rehashing.  Since modification is not
;;; allowed, we don't have to worry about deleted entries.  We indirect through
;;; a parallel vector to find the index in the ENTRIES at which the entries for
;;; a given name starts.
;;;
(defstruct (compact-info-env
	    (:include info-env)
	    (:pure :substructure)
	    (:print-function %print-info-environment))
  ;;
  ;; If this value is EQ to the name we want to look up, then the cache hit
  ;; function can be called instead of the lookup function.
  (cache-name 0)
  ;;
  ;; The index in ENTRIES for the CACHE-NAME, or NIL if that name has no
  ;; entries.
  (cache-index nil :type (or compact-info-entries-index null))
  ;;
  ;; Hashtable of the names in this environment.  If a bucket is unused, it is
  ;; 0.
  (table (required-argument) :type simple-vector)
  ;;
  ;; Indirection vector parallel to TABLE, translating indices in TABLE to the
  ;; start of the ENTRIES for that name.  Unused entries are undefined.
  (index (required-argument)
	 :type (simple-array compact-info-entries-index (*)))
  ;;
  ;; Vector contining in contiguous ranges the values of for all the types of
  ;; info for each name.
  (entries (required-argument) :type simple-vector)
  ;;
  ;; Vector parallel to ENTRIES, indicating the type number for the value
  ;; stored in that location and whether this location is the last type of info
  ;; stored for this name.  The type number is in the low TYPE-NUMBER-BITS
  ;; bits, and the next bit is set if this is the last entry.
  (entries-info (required-argument)
		:type (simple-array compact-info-entry (*))))


(defconstant compact-info-entry-type-mask (ldb (byte type-number-bits 0) -1))
(defconstant compact-info-entry-last (ash 1 type-number-bits))


;;; COMPACT-INFO-CACHE-HIT  --  Internal
;;;
;;;    Return the value of the type corresponding to Number for the currently
;;; cached name in Env.
;;;
(declaim (inline compact-info-cache-hit))
(defun compact-info-cache-hit (env number)
  (declare (type compact-info-env env) (type type-number number))
  (let ((entries-info (compact-info-env-entries-info env))
	(index (compact-info-env-cache-index env)))
    (if index
	(do ((index index (1+ index)))
	    (nil)
	  (declare (type index index))
	  (let ((info (aref entries-info index)))
	    (when (= (logand info compact-info-entry-type-mask) number)
	      (return (values (svref (compact-info-env-entries env) index)
			      t)))
	    (unless (zerop (logand compact-info-entry-last info))
	      (return (values nil nil)))))
	(values nil nil))))


;;; COMPACT-INFO-LOOKUP  --  Internal
;;;
;;;    Encache Name in the compact environment Env.  Hash is the INFO-HASH of
;;; Name.
;;;
(defun compact-info-lookup (env name hash)
  (declare (type compact-info-env env)
	   (type (integer 0 #.most-positive-fixnum) hash))
  (let* ((table (compact-info-env-table env))
	 (len (length table))
	 (len-2 (- len 2))
	 (hash2 (- len-2 (rem hash len-2))))
    (declare (type index len-2 hash2))
    (macrolet ((lookup (test)
		 `(do ((probe (rem hash len)
			      (let ((new (+ probe hash2)))
				(declare (type index new))
				;;
				;; same as (mod new len), but faster.
				(if (>= new len)
				    (the index (- new len))
				    new))))
		      (nil)
		    (let ((entry (svref table probe)))
		      (when (eql entry 0)
			(return nil))
		      (when (,test entry name)
			(return (aref (compact-info-env-index env)
				      probe)))))))
      (setf (compact-info-env-cache-index env)
	    (if (symbolp name)
		(lookup eq)
		(lookup equal)))
      (setf (compact-info-env-cache-name env) name)))

  (undefined-value))


;;; Exact density (modulo rounding) of the hashtable in a compact info
;;; environment in names/bucket.
;;;
(defconstant compact-info-environment-density 65)


;;; COMPACT-INFO-ENVIRONMENT  --  Public
;;;
;;;    Iterate over the environment once to find out how many names and entries
;;; it has, then build the result.  This code assumes that all the entries for
;;; a name well be iterated over contiguously, which holds true for the
;;; implementation of iteration over both kinds of environments.
;;;
;;;    When building the table, we sort the entries by POINTER< in an attempt
;;; to preserve any VM locality present in the original load order, rather than
;;; randomizing with the original hash function.
;;; 
(defun compact-info-environment (env &key (name (info-env-name env)))
  "Return a new compact info environment that holds the same information as
  Env."
  (let ((name-count 0)
	(prev-name 0)
	(entry-count 0))
    (collect ((names))
      (let ((types ()))
	(do-info (env :name name :type-number num :value value)
	  (unless (eq name prev-name)
	    (incf name-count)
	    (unless (eql prev-name 0)
	      (names (cons prev-name types)))
	    (setq prev-name name)
	    (setq types ()))
	  (incf entry-count)
	  (push (cons num value) types))
	(unless (eql prev-name 0)
	  (names (cons prev-name types))))
      
      (let* ((table-size
	      (primify
	       (+ (truncate (* name-count 100)
			    compact-info-environment-density)
		  3)))
	     (table (make-array table-size :initial-element 0))
	     (index (make-array table-size
				:element-type 'compact-info-entries-index))
	     (entries (make-array entry-count))
	     (entries-info (make-array entry-count
				       :element-type 'compact-info-entry))
	     (sorted (sort (names) #'(lambda (x y)
				       (< (%primitive make-fixnum x)
					  (%primitive make-fixnum y))))))
	(let ((entries-idx 0))
	  (dolist (types sorted)
	    (let* ((name (first types))
		   (hash (info-hash name))
		   (len-2 (- table-size 2))
		   (hash2 (- len-2 (rem hash len-2))))
	      (do ((probe (rem hash table-size)
			  (rem (+ probe hash2) table-size)))
		  (nil)
		(let ((entry (svref table probe)))
		  (when (eql entry 0)
		    (setf (svref table probe) name)
		    (setf (aref index probe) entries-idx)
		    (return))
		  (assert (not (equal entry name))))))

	    (unless (zerop entries-idx)
	      (setf (aref entries-info (1- entries-idx)) 
		    (logior (aref entries-info (1- entries-idx))
			    compact-info-entry-last)))

	    (loop for (num . value) in (rest types) do
	      (setf (aref entries-info entries-idx) num)
	      (setf (aref entries entries-idx) value)
	      (incf entries-idx)))
	  
	  (unless (zerop entry-count)
	    (setf (aref entries-info (1- entry-count)) 
		  (logior (aref entries-info (1- entry-count))
			  compact-info-entry-last)))
	  
	  (make-compact-info-env :name name
				 :table table
				 :index index
				 :entries entries
				 :entries-info entries-info))))))
      
      

;;;; Volatile environments:

;;; This is a closed hashtable, with the bucket being computed by taking the
;;; INFO-HASH of the Name mod the table size.
;;;
(defstruct (volatile-info-env
	    (:include info-env)
	    (:print-function %print-info-environment))

  ;;
  ;; If this value is EQ to the name we want to look up, then the cache hit
  ;; function can be called instead of the lookup function.
  (cache-name 0)
  ;;
  ;; The alist translating type numbers to values for the currently cached
  ;; name.
  (cache-types nil :type list)
  ;;
  ;; Vector of alists of alists of the form:
  ;;    ((Name . ((Type-Number . Value) ...) ...)
  ;;
  (table (required-argument) :type simple-vector)
  ;;
  ;; The number of distinct names currently in this table (each name may have
  ;; multiple entries, since there can be many types of info.
  (count 0 :type index)
  ;;
  ;; The number of names at which we should grow the table and rehash.
  (threshold 0 :type index))


;;; VOLATILE-INFO-CACHE-HIT  --  Internal
;;;
;;;    Just like COMPACT-INFO-CACHE-HIT, only do it on a volatile environment.
;;;
(declaim (inline volatile-info-cache-hit))
(defun volatile-info-cache-hit (env number)
  (declare (type volatile-info-env env) (type type-number number))
  (dolist (type (volatile-info-env-cache-types env) (values nil nil))
    (when (eql (car type) number)
      (return (values (cdr type) t)))))  


;;; VOLATILE-INFO-LOOKUP  --  Internal
;;;
;;;    Just like COMPACT-INFO-LOOKUP, only do it on a volatile environment.
;;;
(defun volatile-info-lookup (env name hash)
  (declare (type volatile-info-env env)
	   (type (integer 0 #.most-positive-fixnum) hash))
  (let ((table (volatile-info-env-table env)))
    (macrolet ((lookup (test)
		 `(dolist (entry (svref table (mod hash (length table))) ())
		    (when (,test (car entry) name)
		      (return (cdr entry))))))
      (setf (volatile-info-env-cache-types env)
	    (if (symbolp name)
		(lookup eq)
		(lookup equal)))
      (setf (volatile-info-env-cache-name env) name)))

  (undefined-value))


;;; WITH-INFO-BUCKET  --  Internal
;;;
;;;    Given a volatile environment Env, bind Table-Var the environment's table
;;; and Index-Var to the index of Name's bucket in the table.  We also flush
;;; the cache so that things will be consistent if body modifies something.
;;;
(eval-when (compile eval)
  (defmacro with-info-bucket ((table-var index-var name env) &body body)
    (once-only ((n-name name)
		(n-env env))
      `(progn
	 (setf (volatile-info-env-cache-name ,n-env) 0)
	 (let* ((,table-var (volatile-info-env-table ,n-env))
		(,index-var (mod (info-hash ,n-name) (length ,table-var))))
	   ,@body)))))


;;; GET-WRITE-INFO-ENV  --  Internal
;;;
;;;    Get the info environment that we use for write/modification operations.
;;; This is always the first environment in the list, and must be a
;;; VOLATILE-INFO-ENV.
;;;
(declaim (inline get-write-info-env))
(defun get-write-info-env (&optional (env-list *info-environment*))
  (let ((env (car env-list)))
    (unless env
      (error "No info environment?"))
    (unless (typep env 'volatile-info-env)
      (error "Cannot modify this environment: ~S." env))
    (the volatile-info-env env)))


;;; SET-INFO-VALUE  --  Internal
;;;
;;;    If Name is already present in the table, then just create or modify the
;;; specified type.  Otherwise, add the new name and type, checking for
;;; rehashing.
;;;
;;;    We rehash by making a new larger environment, copying all of the entries
;;; into it, then clobbering the old environment with the new environment's
;;; table.  We clear the old table to prevent it from holding onto garbage if
;;; it is statically allocated.
;;;
(defun set-info-value (name type new-value &optional
			    (env (get-write-info-env)))
  (declare (type type-number type) (type volatile-info-env env)
	   (inline assoc))
  (when (eql name 0)
    (error "0 is not a legal INFO name."))
  ;; We don't enter the value in the cache because we don't know that this
  ;; info-environment is part of *cached-info-environment*.
  (info-cache-enter name type nil :empty)
  (with-info-bucket (table index name env)
    (let ((types (if (symbolp name)
		     (assoc name (svref table index) :test #'eq)
		     (assoc name (svref table index) :test #'equal))))
      (cond
       (types
	(let ((value (assoc type (cdr types))))
	  (if value
	      (setf (cdr value) new-value)
	      (push (cons type new-value) (cdr types)))))
       (t
	(push (cons name (list (cons type new-value)))
	      (svref table index))

	(let ((count (incf (volatile-info-env-count env))))
	  (when (>= count (volatile-info-env-threshold env))
	    (let ((new (make-info-environment :size (* count 2))))
	      (do-info (env :name entry-name :type-number entry-num
			    :value entry-val :known-volatile t)
		(set-info-value entry-name entry-num entry-val new))
	      (fill (volatile-info-env-table env) nil)
	      (setf (volatile-info-env-table env)
		    (volatile-info-env-table new))
	      (setf (volatile-info-env-threshold env)
		    (volatile-info-env-threshold new)))))))))
	  
  new-value)


;;; The maximum density of the hashtable in a volatile env (in names/bucket).
;;;
(defconstant volatile-info-environment-density 50)


;;; MAKE-INFO-ENVIRONMENT  --  Public
;;;
;;;    Make a new volatile environment of the specified size.
;;;
(defun make-info-environment (&key (size 42) (name "Unknown"))
  (declare (type (integer 1) size))
  (let ((table-size
	 (primify (truncate (* size 100) volatile-info-environment-density))))
    (make-volatile-info-env
     :name name
     :table (make-array table-size :initial-element nil)
     :threshold size)))


;;; CLEAR-INFO  --  Public
;;;
(defmacro clear-info (class type name)
  "Clear the information of the specified Type and Class for Name in the
  current environment, allowing any inherited info to become visible.  We
  return true if there was any info."
  (let* ((class (symbol-name class))
	 (type (symbol-name type))
	 (info (type-info-or-lose class type)))
    `(clear-info-value ,name ,(type-info-number info))))
;;;
(defun clear-info-value (name type)
  (declare (type type-number type) (inline assoc))
  (clear-invalid-info-cache)
  (info-cache-enter name type nil :empty)
  (with-info-bucket (table index name (get-write-info-env))
    (let ((types (assoc name (svref table index) :test #'equal)))
      (when (and types
		 (assoc type (cdr types)))
	(setf (cdr types)
	      (delete type (cdr types) :key #'car))
	t))))


;;;; GET-INFO-VALUE:

;;; GET-INFO-VALUE  --  Internal
;;;
;;;    Check if the name and type is in our cache, if so return it.  Otherwise,
;;; search for the value and encache it.
;;;
;;;    Return the value from the first environment which has it defined, or
;;; return the default if none does.  We have a cache for the last name looked
;;; up in each environment.  We don't compute the hash until the first time the
;;; cache misses.  When the cache does miss, we invalidate it before calling
;;; the lookup routine to eliminate the possiblity of the cache being
;;; partially updated if the lookup is interrupted.
;;;
(defun get-info-value (name type &optional (env-list nil env-list-p))
  (declare (type type-number type))
  (sys:without-interrupts
   (flet ((lookup-ignoring-global-cache (env-list)
	    (let ((hash nil))
	      (dolist (env env-list
		       (multiple-value-bind
			     (val winp)
			   (funcall (type-info-default
				     (svref *type-numbers* type))
				    name)
			 (values val winp)))
		(macrolet ((frob (lookup cache slot)
			     `(progn
				(unless (eq name (,slot env))
				  (unless hash
				    (setq hash (info-hash name)))
				  (setf (,slot env) 0)
				  (,lookup env name hash))
				(multiple-value-bind
				      (value winp)
				    (,cache env type)
				  (when winp (return (values value t)))))))
		  (if (typep env 'volatile-info-env)
		      (frob volatile-info-lookup volatile-info-cache-hit
			    volatile-info-env-cache-name)
		      (frob compact-info-lookup compact-info-cache-hit
			    compact-info-env-cache-name)))))))
     (cond (env-list-p
	    (lookup-ignoring-global-cache env-list))
	   (t
	    (clear-invalid-info-cache)
	    (multiple-value-bind (val winp)
		(info-cache-lookup name type)
	      (if (eq winp :empty)
		  (multiple-value-bind (val winp)
		      (lookup-ignoring-global-cache *info-environment*)
		    (info-cache-enter name type val winp)
		    (values val winp))
		  (values val winp))))))))


;;;; Initialization:

;;; GLOBALDB-INIT  --  Interface
;;;
;;;    Since the global enviornment database is used by top-level forms in this
;;; file, we must initialize the database before processing any top-level
;;; forms.  This requires a special initialization function that is called from
;;; %INITIAL-FUNCTION.  We replicate the init forms of the variables that
;;; maintain the class/type namespace.  We also initialize the info cache.
;;;
(defun globaldb-init ()
  (unless (boundp '*info-environment*)
    (setq *info-environment*
	  (list (make-info-environment :name "Initial Global"))))
  (unless (boundp '*info-classes*)
    (setq *info-classes* (make-hash-table :test #'equal))
    (setq *type-numbers*
	  (make-array (ash 1 type-number-bits)  :initial-element nil)))

  (info-cache-init)
  (function-info-init)
  (other-info-init))


;;;; Definitions for function information.

(defun function-info-init ()

(define-info-class function)

;;; The kind of functional object being described.  If null, Name isn't a known
;;; functional object.
(define-info-type function kind (member nil :function :macro :special-form)
  (if (fboundp name) :function nil))

;;; The type specifier for this function.
(define-info-type function type ctype
  (if (fboundp name)
      (extract-function-type (fdefinition name))
      (specifier-type 'function)))

;;; The Assumed-Type for this function, if we have to infer the type due to not
;;; having a declaration or definition.
(define-info-type function assumed-type (or approximate-function-type null))

;;; Where this information came from:
;;;  :declared, from a declaration.
;;;  :assumed, from uses of the object.
;;;  :defined, from examination of the definition.
(define-info-type function where-from (member :declared :assumed :defined)
  (if (fboundp name) :defined :assumed))

;;; Lambda used for inline expansion of this function.
(define-info-type function inline-expansion list)

;;; Specifies whether this function may be expanded inline.  If null, we
;;; don't care.
(define-info-type function inlinep inlinep nil)

;;; A macro-like function which transforms a call to this function into some
;;; other Lisp form.  This expansion is inhibited if inline expansion is
;;; inhibited.
(define-info-type function source-transform (or function null))

;;; The macroexpansion function for this macro.
(define-info-type function macro-function (or function null)
  nil)

;;; The compiler-macroexpansion function for this macro.
(define-info-type function compiler-macro-function (or function null)
  nil)

;;; A function which converts this special form into IR1.
(define-info-type function ir1-convert (or function null))

;;; A function which gets a chance to do stuff to the IR1 for any call to this
;;; function.
(define-info-type function ir1-transform (or function null))

;;; If a function is a slot accessor or setter, then this is the class that it
;;; accesses slots of.
;;;
(define-info-type function accessor-for (or class null) nil)

;;; If a function is "known" to the compiler, then this is FUNCTION-INFO
;;; structure containing the info used to special-case compilation.
(define-info-type function info (or function-info null) nil)

(define-info-type function documentation (or string null) nil)

(define-info-type function definition t nil)


); defun function-info-init

#|
  Other:
    Documentation?
|#

;;;; Definitions for other random information.

(defun other-info-init ()

(define-info-class variable)

;;; The kind of variable-like thing described.
(define-info-type variable kind (member :special :constant :global :alien
					:macro)
  (if (or (eq (symbol-package name) (symbol-package :end))
	  (member name '(t nil)))
      :constant
      :global))

;;; The declared type for this variable.
(define-info-type variable type ctype *universal-type*)

;;; Where this type and kind information came from.
(define-info-type variable where-from (member :declared :assumed :defined)
  :assumed)

;;; The lisp object which is the value of this constant, if known.
(define-info-type variable constant-value t
  (if (boundp name)
      (values (symbol-value name) t)
      (values nil nil)))

(define-info-type variable alien-info (or heap-alien-info null) nil)

(define-info-type variable macro-expansion t nil)

(define-info-type variable documentation (or string null) nil)

(define-info-class type)

;;; The kind of type described.  We return :Instance for standard types that
;;; are implemented as structures.
;;;
(define-info-type type kind (member :primitive :defined :instance nil)
  nil)

;;; Expander function for a defined type.
(define-info-type type expander (or function null) nil)

(define-info-type type documentation (or string null))

;;; Function that parses type specifiers into CTYPE structures.
;;;
(define-info-type type translator (or function null) nil)

;;; If true, then the type coresponding to this name.  Note that if this is a
;;; built-in class with a translation, then this is the translation, not the
;;; class object.  This info type keeps track of random atomic types (NIL etc.)
;;; and also serves as a cache to ensure that common standard types (atomic and
;;; otherwise) are only consed once.
;;;
(define-info-type type builtin (or ctype null) nil)

;;; If this is a class name, then the value is a cons (Name . Class), where
;;; Class may be null if the class hasn't been defined yet.  Note that for
;;; built-in classes, the kind may be :PRIMITIVE and not :INSTANCE.  The
;;; the name is in the cons so that we can signal a meaningful error if we only
;;; have the cons.
;;;
(define-info-type type class (or kernel::class-cell null) nil)

;;; Layout for this type being used by the compiler.
;;;
(define-info-type type compiler-layout (or layout null)
  (let ((class (kernel::find-class name nil)))
    (when class (%class-layout class))))

(define-info-class typed-structure)
(define-info-type typed-structure info t nil)
(define-info-type typed-structure documentation (or string null))

(define-info-class declaration)
(define-info-type declaration recognized boolean)

(define-info-class alien-type)
(define-info-type alien-type kind (member :primitive :defined :unknown)
  :unknown)
(define-info-type alien-type translator (or function null) nil)
(define-info-type alien-type definition (or alien-type null) nil)
(define-info-type alien-type struct (or alien-type null) nil)
(define-info-type alien-type union (or alien-type null) nil)
(define-info-type alien-type enum (or alien-type null) nil)

(define-info-class setf)

(define-info-type setf inverse (or symbol null) nil)

(define-info-type setf documentation (or string null) nil)

(define-info-type setf expander (or function null) nil)

;;; Used for storing random documentation types.  The stuff is an alist
;;; translating documentation kinds to values.
;;;
(define-info-class random-documentation)
(define-info-type random-documentation stuff list ())

;;; Used to record the source-location of definitions.
;;;
(define-info-class source-location)
(define-info-type source-location defvar (or form-numbers null) nil)
		   
); defun other-info-init

(declaim (freeze-type info-env))

