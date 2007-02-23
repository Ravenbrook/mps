;;; -*- Package: Lisp; Log: C.Log -*-

;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Core image builder for Spice Lisp.
;;; Written by Skef Wholey.  Package hackery courtesy of Rob MacLachlan.
;;;
;;; We gobble up FASL files, building a virtual memory image in a collection
;;; of blocks in system table space.  When we're done and wish to write out the
;;; file, we allocate a block large enough to hold the contents of the memory
;;; image, and twiddle the page map to map the parts of the image consecutively
;;; into this large block, which we then write out as a file.
;;;

(in-package "LISP")

;;; Hack version that accepts NFASL file type...

#-new-compiler
(defun load (filename &key ((:verbose *load-verbose*) *load-verbose*)
		      ((:print *load-print-stuff*) *load-print-stuff*)
		      (if-does-not-exist :error))
  "Loads the file named by Filename into the Lisp environment.  See manual
   for details."
  (let ((*package* *package*))
    (if (streamp filename)
	(if (equal (stream-element-type filename) '(unsigned-byte 8))
	    (fasload filename)
	    (sloload filename))
	(let* ((pn (merge-pathnames (pathname filename)
				    *default-pathname-defaults*))
	       (tn (probe-file pn)))
	  (cond
	   (tn
	    (if (or (string-equal (pathname-type tn) "fasl")
		    (string-equal (pathname-type tn) "nfasl"))
		(with-open-file (file tn
				      :direction :input
				      :element-type '(unsigned-byte 8))
		  (fasload file))
		(with-open-file (file tn :direction :input)
		  (sloload file)))
	    t)
	   ((pathname-type pn)
	    (let ((stream (open pn :direction :input
				:if-does-not-exist if-does-not-exist)))
	      (when stream
		(sloload stream)
		(close stream)
		t)))
	   (t
	    (let* ((srcn (make-pathname :type "lisp" :defaults pn))
		   (src (probe-file srcn))
		   (objn (make-pathname :type "fasl" :defaults pn))
		   (obj (probe-file objn)))
	      (cond
	       (obj
		(cond ((and src (> (file-write-date src)
				   (file-write-date obj)))
		       (case *load-if-source-newer*
			 (:load-object
			  (warn "Loading object file ~A, which is~%  ~
			         older than the presumed source, ~A."
				(namestring obj)
				(namestring src))
			  (load obj))
			 (:load-source
			  (warn "Loading source file ~A, which is~%  ~
			         newer than the presumed object file, ~A."
				(namestring src)
				(namestring obj))
			  (load src))
			 (:compile
			  (compile-file (namestring src))
			  (load obj))
			 (:query
			  (if (y-or-n-p "Load source file ~A which is newer~%  ~
					 than presumed object file ~A? "
					(namestring src)
					(namestring obj))
			      (load src)
			      (load obj)))
			 (T (error "*Load-if-source-newer* contains ~A which is not one of:~%  ~
			            :load-object, :load-source, :compile, or :query."
				   *load-if-source-newer*))))
		      (T (load obj))))
	       (t
		(load srcn :if-does-not-exist if-does-not-exist))))))))))


(eval-when (compile load eval)
  (defconstant type-shift 11)
  (defconstant type-space-shift 9)
  (defconstant type-space-byte (byte 7 9))
  (defconstant high-data-byte (byte 11 16))
  (defconstant high-data-mask 511)
  (defconstant space-right-shift -9)
)

(defvar *external-references* ()
  "List of reference frobs made by assembler routines that have been cold loaded.")

(defvar machine-code-buffer
  (make-array 2048 :element-type '(unsigned-byte 8))
  "Buffer for machine code for genesis of the Romp")

(defconstant cold-i-vector-header-size 8
  "Size of an i-vector.")

(defvar *cold-map-file* nil)
(defvar *defined-functions* nil)

;;; Head of list of functions to be called when the Lisp starts up.
;;;
(defvar current-init-functions-cons)


;;; True if we are in the cold loader.  Used by FOP-MAYBE-COLD-LOAD in
;;; the normal loader.
;;;
(proclaim '(special *in-cold-load*))


;;; Memory accessing.

;;; The virtual address space is divided into 128 pieces.

(defparameter number-of-spaces 512 "Number of pointer-type spaces.")

;;; The initial size of spaces will be one megabyte.  If we need more than this,
;;; the following constant will have to be changed.

(defparameter space-size (ash 1 21) "Number of bytes in each space.")

;;; Each space is represented as a structure containing the address of its
;;; data in system space and the number of bytes used so far in it.

(defstruct (space (:print-function print-a-space))
  address
  real-address
  free-pointer)

(defun print-a-space (space stream depth)
  depth
  (write-string "#<SPACE, address = " stream)
  (prin1 (sap-int (space-address space)) stream)
  (write-char #\> stream))

;;; The type codes:

(eval-when (compile load eval)
  (defparameter +-fixnum-ltype 0)
  (defparameter gc-forward-ltype 1) 
  (defparameter trap-ltype 4)
  (defparameter bignum-ltype 5)
  (defparameter ratio-ltype 6)
  (defparameter complex-ltype 7)
  (defparameter short-float-low-ltype 8)
  (defparameter short-float-high-ltype 9)
  (defparameter short-float-4bit-ltype 4)
  (defparameter long-float-ltype 10)
  (defparameter string-ltype 11)
  (defparameter bit-vector-ltype 12)
  (defparameter integer-vector-ltype 13)
  (defparameter code-ltype 14)
  (defparameter general-vector-ltype 15)
  (defparameter array-ltype 16)
  (defparameter function-ltype 17)
  (defparameter symbol-ltype 18)
  (defparameter list-ltype 19)
  (defparameter control-stack-ltype 20)
  (defparameter binding-stack-ltype 21)
  (defparameter assembly-code-ltype 0)

  (defparameter string-char-ltype 26)
  (defparameter bitsy-char-ltype 27)
  (defparameter values-marker-ltype 28)
  (defparameter catch-all-ltype 29)
  (defparameter --fixnum-ltype 31)
  (defparameter smallest-ltype +-fixnum-ltype)
  (defparameter largest-ltype binding-stack-ltype)
  (defparameter first-pointer-ltype 0))

(defmacro space-to-highbits (space)
  `(ash ,space 9))

;;; The subspace codes:

(defparameter dynamic-space 0)
(defparameter static-space 2)
(defparameter read-only-space 3)

;;; Lispobj-Shift is the number of places we shift left to convert an index
;;; into an offset we can do address calculation with.

(defconstant lispobj-shift 2)

;;; Lispobj-Size is the number of glomps it takes to represent a Lisp object.

(defconstant lispobj-size 4)

;;; Macros to construct space numbers from a type and subspace.

(defmacro dynamic (type)
  `(logior (ash ,type 2) dynamic-space))

(defmacro static (type)
  `(logior (ash ,type 2) static-space))

(defmacro read-only (type)
  `(logior (ash ,type 2) read-only-space))

;;; Used-Spaces is a list of the spaces we allocate stuff for.

(defparameter used-spaces
  (list (dynamic code-ltype)
	(dynamic bit-vector-ltype)
	(dynamic integer-vector-ltype)
	(dynamic string-ltype)
	(dynamic bignum-ltype)
	(static bignum-ltype)
	(dynamic long-float-ltype)
	(dynamic complex-ltype)
	(dynamic ratio-ltype)
	(dynamic general-vector-ltype)
	(dynamic function-ltype)
	(dynamic array-ltype)
	(dynamic symbol-ltype)
	(static symbol-ltype)
	(dynamic list-ltype)
	(static list-ltype)
	(dynamic trap-ltype)
	(dynamic assembly-code-ltype)))

;;; Memory is a vector of these spaces.  If a space is non-existent or is not
;;; used in cold load, NIL will be stored in the corresponding location instead
;;; of a space structure.

(defvar memory (make-array number-of-spaces))

(defparameter code-space (dynamic assembly-code-ltype))

(defvar *genesis-memory-initialized* nil)

;;; In order to allow access of symbols that weren't dumped in normal load, we
;;; maintain a back-translation from handles to symbols.  This is a vector of
;;; vectors, indexed first by space code, and then by the offset (in units of
;;; symbol size).
;;;
(defvar *space-to-symbol-array*)


(defun initialize-memory ()
  (unless *genesis-memory-initialized*
    (setq *genesis-memory-initialized* t)
    (setq *space-to-symbol-array* (make-array 4))
    (setf (svref *space-to-symbol-array* 0) (make-array 10000))
    (setf (svref *space-to-symbol-array* 1) nil)
    (setf (svref *space-to-symbol-array* 2) (make-array 5000))
    (setf (svref *space-to-symbol-array* 3) (make-array 500))
    (do* ((spaces used-spaces (cdr spaces))
	  (space (car spaces) (car spaces)))
	 ((null spaces))
      (declare (list spaces))
      (multiple-value-bind (hunk addr)
			   (get-valid-hunk
			    (if (= space (dynamic code-ltype))
				(ash space-size 3)
				space-size))
	(setf (svref memory space)
	      (make-space :address (int-sap hunk)
			  :real-address (int-sap addr)
			  :free-pointer (cons (if (eq space code-space)
						  clc::romp-code-base
						  (space-to-highbits space))
					      0)))))))


(defun get-valid-hunk (size)
  (let ((addr (do-validate 0 size -1)))
    (values addr addr)))

(defun initialize-spaces ()
  (do* ((spaces used-spaces (cdr spaces))
	(space (car spaces) (car spaces)))
      ((null spaces))
    (declare (list spaces))
    (setf (space-free-pointer (svref memory space))
	  (cons (if (eq space code-space)
		    clc::romp-code-base
		    (space-to-highbits space)) 0))))

;;; Since Spice Lisp objects are 32-bits wide, and Spice Lisp integers are only
;;; 28-bits wide, we use the following scheme to represent the 32-bit objects
;;; in the image being built.  In the core image builder, we can have a handle
;;; on a 32-bit object, which is a cons of two 16-bit numbers, the high order
;;; word in the CAR, and the low order word in the CDR.  We provide some macros
;;; to manipulate these handles.

(defmacro handle-on (high low)
  `(cons ,high ,low))

(defun handle-beyond (handle offset)
  (let ((new (+ (cdr handle) offset)))
    (if (> new 65535)
	(cons (+ (car handle) (ash new -16)) (logand new 65535))
	(cons (car handle) new))))

(defun bump-handle (handle offset)
  (let ((new (+ (cdr handle) offset)))
    (if (> new 65535)
	(setf (car handle) (+ (car handle) (ash new -16))
	      (cdr handle) (logand new 65535))
	(setf (cdr handle) new))))

(defun copy-handle (handle)
  (cons (car handle) (cdr handle)))

(defun handle< (han1 han2)
  (or (< (car han1) (car han2))
      (and (= (car han1) (car han2))
	   (< (cdr han1) (cdr han2)))))

;;; Handle-Offset is used to map a virtual address into an offset into the table
;;; of 16-bit words we're building of objects of that type.  Blah blah.

;;; The LDM is a byte-addressed machine, so we shift the offset bits right 1.

(defun handle-offset (handle)
  (logior (ash (logand (car handle) high-data-mask) 15) (ash (cdr handle) -1)))

(defun handle-type (handle)
  (ash (car handle) (- type-shift)))

(defmacro build-object (type bits)
  `(let ((bits ,bits))
     (handle-on (logior (ash ,type type-shift) (ldb high-data-byte bits))
		(logand bits 65535))))

(defmacro build-sub-object (type subtype bits)
  `(let ((bits ,bits))
     (handle-on (logior (ash ,type type-shift)
			(logior (ash ,subtype 8) (ldb (byte 8 16) bits)))
		(logand bits 65535))))

(defparameter trap-handle
  (handle-on (space-to-highbits (dynamic trap-ltype)) 0000)
  "Handle on the trap object.")

(defparameter nil-handle (handle-on (space-to-highbits (static list-ltype)) 0000)
  "Handle on Nil.")

(defparameter zero-handle (handle-on 0 0))

#|
(defmacro byte-swap-16 (object)
  `(let ((obj ,object))
     (logior (ash (logand obj #xFF) 8)
	     (logand (ash obj -8) #xFF))))
|#

(defmacro byte-swap-16 (object) object)

;;; Write-Memory writes the Object (identified by a handle) into the given
;;; Address (also a handle).

(defun write-memory (address object)
  (let* ((high (car address))
	 (low (cdr address))
	 (space-number (ldb type-space-byte high))
	 (offset (handle-offset address))
	 (space (svref memory space-number)))
    (when (eq space-number code-space)
      (decf offset (ash clc::romp-code-base 15)))
    (if space
	(if (handle< address (space-free-pointer space))
	    (let ((saddress (space-address space)))
	      (%primitive 16bit-system-set saddress offset
			  (byte-swap-16 (car object)))
	      (%primitive 16bit-system-set saddress (1+ offset)
			  (byte-swap-16 (cdr object))))
	    (error "No object at ~S,,~S has been allocated." high low))
	(error "The space for address ~S,,~S does not exist." high low))))

;;; Read-Memory returns a handle to an object read from the given Address.

(defun read-memory (address)
  (let* ((high (car address))
	 (low (cdr address))
	 (space-number (ldb type-space-byte high))
	 (offset (handle-offset address))
	 (space (svref memory space-number)))
    (when (eq space-number code-space)
      (decf offset (ash clc::romp-code-base 15)))
    (if space
	(if (handle< address (space-free-pointer space))
	    (let ((saddress (space-address space)))
	      (handle-on (byte-swap-16 (%primitive 16bit-system-ref
						   saddress offset))
			 (byte-swap-16 (%primitive 16bit-system-ref
						   saddress (1+ offset)))))
	    (error "No object at ~S,,~S has been allocated." high low))
	(error "The space for address ~S,,~S does not exist." high low))))

;;; Read-Indexed is used to read from g-vector-like things.

(defun read-indexed (address index)
  (read-memory (handle-beyond address
			      (+ (ash index lispobj-shift) lispobj-size))))

;;; Write-Indexed is used to write into g-vector-like things.

(defun write-indexed (address index value)
  (write-memory (handle-beyond address (+ (ash index lispobj-shift) lispobj-size))
		value))

;;; Allocating primitive objects.

;;; Allocate-Boxed-Object returns a handle to an object allocated in the
;;; space of the given Space-Number with the given Length.  No header words are
;;; initialized, and the Length should include the length of the header.  The
;;; free pointer for the Space is incremented and (on the Perq) quadword alligned.

(defun allocate-boxed-object (space-number length)
  (let ((space (svref memory space-number)))
    (if space
	(let* ((start (space-free-pointer space))
	       (result (copy-handle start)))
	  (bump-handle start (ash length lispobj-shift))
	  result)
	(error "Space ~S does not exist." space-number))))

;;; Allocate-Unboxed-Object returns a handle to an object allocated in the
;;; space of the given Space-Number with the given Byte-Size and Length in
;;; bytes of that size.  The 2 unboxed object header words are initialized
;;; with the optional Subtype code.

(defun allocate-unboxed-object (space-number byte-size size subtype)
  (let ((space (svref memory space-number)))
    (if space
	(let* ((start (space-free-pointer space))
	       (result (copy-handle start))
	       (length (+ 2 (ceiling size (/ 32 byte-size)))))
	  (bump-handle start (ash length lispobj-shift))
	  (write-memory result
			(build-sub-object +-fixnum-ltype subtype length))
	  (write-memory (handle-beyond result lispobj-size)
			(handle-on (logior (ash (access-type byte-size) 12)
					   (logand (ash size -16) #xFFF))
				   (logand size #xFFFF)))
	  result)
	(error "Space ~S does not exist." space-number))))

;;; Access-Type returns the I-Vector access type for a given Byte-Size.

(defun access-type (byte-size)
  (let ((access-type (cdr (assoc byte-size '((1 . 0) (2 . 1) (4 . 2)
					     (8 . 3) (16 . 4) (32 . 5)
					     (64 . 6) (128 . 7))))))
    (if access-type access-type
	(error "Invalid I-Vector byte size, ~S." byte-size))))

;;; I-Vector-To-Core copies the contents of the given unboxed thing into
;;; the virtual memory image in the space with the give Space-Number, returning
;;; a handle to the new object.

(defun i-vector-to-core (space-number byte-size size subtype thing)
  (let* ((dest (allocate-unboxed-object space-number byte-size size subtype))
	 (byte-count (ash (ceiling size (/ 16 byte-size)) 1))
	 (offset (handle-offset dest))
	 (dest-byte-addr (+ offset offset 8)))
    (%primitive byte-blt
		thing 0
		(space-address (svref memory space-number))
		dest-byte-addr (+ dest-byte-addr byte-count))
    dest))

;;; Dump a dynamic string.

(defun string-to-core (string)
  (i-vector-to-core (dynamic string-ltype) 8 (length string) 0 string))

;;; Dump a bignum.
(defun bignum-to-core (n)
  (let* ((words (1+ (ceiling (1+ (integer-length n)) 32)))
	 (handle (allocate-boxed-object (dynamic bignum-ltype) words)))
    (declare (fixnum words))
    (write-memory handle (build-sub-object (if (< n 0) --fixnum-ltype
					       +-fixnum-ltype)
					   0 words))
    (do ((i 1 (1+ i))
	 (half 2 (+ half 2)))
	((= i words)
	 handle)
      (declare (fixnum i half))
      (write-memory (handle-beyond handle (* lispobj-size i))
		    (handle-on (%primitive 16bit-system-ref n half)
			       (%primitive 16bit-system-ref n (1+ half)))))))
			       
;;; Number-To-Core copies the given number to the virtual memory image,
;;; returning a handle to it.

(defun number-to-core (thing)
  (typecase thing
    (fixnum (handle-on (logand (ash thing -16) 65535)
		       (logand thing 65535)))
    (bignum (bignum-to-core thing))
    (short-float
     (let ((fthing (%primitive make-fixnum thing)))
       (handle-on (logior (ash short-float-4bit-ltype 12)
			  (%primitive logldb 12 16 fthing)
			  (if (< thing 0) #x800 0))
		  (logand fthing #xFFFF))))
    (long-float
     (let* ((handle (allocate-boxed-object (dynamic long-float-ltype) 3)))
       (write-memory handle zero-handle)
       (write-memory (handle-beyond handle lispobj-size)
		     (handle-on (%primitive 16bit-system-ref thing 2)
				(%primitive 16bit-system-ref thing 3)))
       (write-memory (handle-beyond handle (+ lispobj-size lispobj-size))
		     (handle-on (%primitive 16bit-system-ref thing 4)
				(%primitive 16bit-system-ref thing 5)))
       handle))
    (t (error "~S isn't a cold-loadable number at all!" thing))))

;;; Allocate-G-Vector allocates a G-Vector of the given Length and writes
;;; the header word.

(defun allocate-g-vector (space-number length &optional (subtype 0))
  (let* ((length (+ length 1))
	 (dest (allocate-boxed-object space-number length)))
    (write-memory dest (build-sub-object +-fixnum-ltype subtype length))
    dest))

;;; Cold-Set-Vector-Subtype frobs the subtype field of an already allocated
;;; G-Vector.

(defun cold-set-vector-subtype (vector subtype)
  (let ((handle (read-memory vector)))
    (write-memory vector (build-sub-object +-fixnum-ltype subtype
					   (logior (logand high-data-mask
							   (car handle))
						   (cdr handle))))
    vector))

;;; Allocate-Cons allocates a cons and fills it with the given stuff.

(defun allocate-cons (space-number car cdr)
  (let ((dest (allocate-boxed-object space-number 2)))
    (write-memory dest car)
    (write-memory (handle-beyond dest lispobj-size) cdr)
    dest))

;;; Cold-Put  --  Internal
;;;
;;;    Add a property to a symbol in the core.  Assumes it doesn't exist.
;;;
(defun cold-put (symbol indicator value)
  (let ((plist-handle (handle-beyond symbol (* 2 lispobj-size))))
    (write-memory plist-handle
		  (allocate-cons
		   (dynamic list-ltype) indicator
		   (allocate-cons
		    (dynamic list-ltype) value
		    (read-memory plist-handle))))))

;;; Allocate-Symbol allocates a symbol and fills its print name cell and
;;; property list cell.

(defparameter *cold-symbol-allocation-space* (dynamic symbol-ltype))

(defun allocate-symbol (name &optional (symbol nil defined))
  (declare (simple-string name))
  (let ((dest (allocate-boxed-object *cold-symbol-allocation-space* 5)))
    (write-memory dest trap-handle)
    (write-memory (handle-beyond dest lispobj-size) trap-handle)
    (write-memory (handle-beyond dest (* 2 lispobj-size)) nil-handle)
    (write-memory (handle-beyond dest (* 3 lispobj-size))
      (i-vector-to-core (dynamic string-ltype) 8 (length name) 0 name))
    (if defined
	(add-symbol-to-handle-map dest symbol)
	(write-memory (handle-beyond dest (* 4 lispobj-size)) nil-handle))
    dest))


(defmacro cold-push (thing list)
  "Generates code to push the Thing onto the given cold load List."
  `(setq ,list (allocate-cons (dynamic list-ltype) ,thing ,list)))


;;;; Interning.

;;;    In order to avoid having to know about the package format, we
;;; build a data structure which we stick in *cold-symbols* that
;;; holds all interned symbols along with info about their packages.
;;; The data structure is a list of lists in the following format:
;;;   (<make-package-arglist>
;;;    <internal-symbols>
;;;    <external-symbols>
;;;    <imported-internal-symbols>
;;;    <imported-external-symbols>
;;;    <shadowing-symbols>)
;;;
;;;    Package manipulation forms are dumped magically by the compiler
;;; so that we can eval them at Genesis time.  An eval-for-effect fop
;;; is used, surrounded by fops that switch the fop table to the hot
;;; fop table and back.
;;;

;;; An alist from packages to the list of symbols in that package to be
;;; dumped.
;;;
(defvar *cold-packages*)


;;; Symbols known to the microcode that must be allocated before all others.
;;; DON'T CHANGE the order of these UNLESS you know what you are doing.
(defparameter initial-symbols
  '(t %sp-internal-apply %sp-internal-error %sp-software-interrupt-handler
      %sp-internal-throw-tag %initial-function %link-table-header
      current-allocation-space %sp-bignum/fixnum %sp-bignum/bignum
      %sp-fixnum/bignum %sp-abs-ratio %sp-abs-complex %sp-negate-ratio
      %sp-negate-complex %sp-integer+ratio %sp-ratio+ratio %sp-complex+number
      %sp-number+complex %sp-complex+complex %sp-1+ratio %sp-1+complex
      %sp-integer-ratio %sp-ratio-integer %sp-ratio-ratio %sp-complex-number 
      %sp-number-complex %sp-complex-complex %sp-1-ratio %sp-1-complex
      %sp-integer*ratio %sp-ratio*ratio %sp-number*complex %sp-complex*number
      %sp-complex*complex %sp-integer/ratio %sp-ratio/integer %sp-ratio/ratio 
      %sp-number/complex %sp-complex/number %sp-complex/complex
      %sp-integer-truncate-ratio %sp-ratio-truncate-integer
      %sp-ratio-truncate-ratio %sp-number-truncate-complex
      %sp-complex-truncate-number %sp-complex-truncate-complex maybe-gc
      lisp-environment-list call-lisp-from-c lisp-command-line-list
      *nameserverport* *ignore-floating-point-underflow*
      %sp-sin-rational %sp-sin-short %sp-sin-long %sp-sin-complex
      %sp-cos-rational %sp-cos-short %sp-cos-long %sp-cos-complex
      %sp-tan-rational %sp-tan-short %sp-tan-long %sp-tan-complex
      %sp-atan-rational %sp-atan-short %sp-atan-long %sp-atan-complex
      %sp-exp-rational %sp-exp-short %sp-exp-long %sp-exp-complex
      %sp-log-rational %sp-log-short %sp-log-long %sp-log-complex
      lisp::%sp-sqrt-rational lisp::%sp-sqrt-short
      lisp::%sp-sqrt-long lisp::%sp-sqrt-complex
      eval::*eval-stack-top*
      ))


;;; Cold-Intern  --  Internal
;;;
;;;    Return a handle on an interned symbol.  If necessary allocate
;;; the symbol and record which package the symbol was referenced in.
;;; When we allocate the symbol, make sure we record a reference to
;;; the symbol in the home package so that the package gets set.
;;;
(defun cold-intern (symbol package)
  (let ((cold-info (get symbol 'cold-info)))
    (unless cold-info
      (cond ((eq (symbol-package symbol) package)
	     (let ((handle (allocate-symbol (symbol-name symbol) symbol)))
	       (when (eq package *keyword-package*) (write-memory handle handle))
	       (setq cold-info (setf (get symbol 'cold-info) (cons handle nil)))))
	    (t
	     (cold-intern symbol (symbol-package symbol))
	     (setq cold-info (get symbol 'cold-info)))))
    (unless (memq package (cdr cold-info))
      (push package (cdr cold-info))
      (push symbol (cdr (or (assq package *cold-packages*)
			    (car (push (list package) *cold-packages*))))))
    (car cold-info)))


(defun add-symbol-to-handle-map (handle symbol)
  (let* ((space (logand (ash (car handle) space-right-shift) #x3))
	 (index (truncate (logior (ash (logand (car handle) #x007F) 16)
				  (logand (cdr handle) #xFFFF))
			  20))
	 (vector (svref *space-to-symbol-array* space)))
    (setf (svref vector index) symbol)))


(defun find-cold-symbol (handle)
  (let* ((space (logand (ash (car handle) space-right-shift) #x3))
	 (index (truncate (logior (ash (logand (car handle) #x007F) 16)
				  (logand (cdr handle) #xFFFF))
			  20))
	 (vector (svref *space-to-symbol-array* space)))
    (svref vector index)))


;;; Initialize-Symbols  --  Internal
;;;
;;;    Since the initial symbols must be allocated before we can intern
;;; anything else, we intern those here.  We also set the values of T and Nil.
;;;
(defun initialize-symbols ()
  "Initilizes the cold load symbol-hacking data structures."
  ;; Special case NIL.
  (let ((*cold-symbol-allocation-space* (static list-ltype))
	(nil-fcn-handle (handle-beyond nil-handle lispobj-size)))
    (cold-intern nil *lisp-package*)
    (write-memory nil-fcn-handle nil-handle))
  (let ((*cold-symbol-allocation-space* (static symbol-ltype)))
    (dolist (symbol initial-symbols)
      (cold-intern symbol *lisp-package*)))
  (write-memory nil-handle nil-handle)
  (write-memory (cold-intern t *lisp-package*) (cold-intern t *lisp-package*)))


;;; Finish-Symbols  --  Internal
;;;
;;;    Scan over all the symbols referenced in each package in *cold-packages*
;;; making the apropriate entry in the *initial-symbols* data structure to
;;; intern the thing.
;;;
(defun finish-symbols ()
  (let ((res nil-handle))
    (dolist (cpkg *cold-packages*)
      (let* ((pkg (car cpkg))
	     (shadows (package-shadowing-symbols pkg)))
	(let ((internal nil-handle)
	      (external nil-handle)
	      (imported-internal nil-handle)
	      (imported-external nil-handle)
	      (shadowing nil-handle))
	  (dolist (sym (cdr cpkg))
	    (let ((handle (car (get sym 'cold-info))))
	      (multiple-value-bind (found where)
				   (find-symbol (symbol-name sym) pkg)
		(unless (and where (eq found sym))
		  (error "Symbol ~S is not available in ~S." sym pkg))
		(when (memq sym shadows)
		  (cold-push handle shadowing))
		(ecase where
		  (:internal
		   (if (eq (symbol-package sym) pkg)
		       (cold-push handle internal)
		       (cold-push handle imported-internal)))
		  (:external
		   (if (eq (symbol-package sym) pkg)
		       (cold-push handle external)
		       (cold-push handle imported-external)))
		  (:inherited)))))
	  (let ((r nil-handle))
	    (cold-push shadowing r)
	    (cold-push imported-external r)
	    (cold-push imported-internal r)
	    (cold-push external r)
	    (cold-push internal r)
	    (cold-push (make-make-package-args pkg) r)
	    (cold-push r res)))))
    
    (write-memory (cold-intern '*initial-symbols* *lisp-package*) res)))


;;; Make-Make-Package-Args  --  Internal
;;;
;;;    Make a cold list that can be used as the arglist to make-package to
;;; make a similar package.
;;;
(defun make-make-package-args (package)
  (let* ((use nil-handle)
	 (nicknames nil-handle)
	 (res nil-handle))
    (dolist (u (package-use-list package))
      (when (assoc u *cold-packages*)
	(cold-push (string-to-core (package-name u)) use)))
    (dolist (n (package-nicknames package))
      (cold-push (string-to-core n) nicknames))
    (cold-push (number-to-core (truncate (internal-symbol-count package) 0.8)) res)
    (cold-push (cold-intern :internal-symbols *keyword-package*) res)
    (cold-push (number-to-core (truncate (external-symbol-count package) 0.8)) res)
    (cold-push (cold-intern :external-symbols *keyword-package*) res)

    (cold-push nicknames res)
    (cold-push (cold-intern :nicknames *keyword-package*) res)

    (cold-push use res)
    (cold-push (cold-intern :use *keyword-package*) res)
    
    (cold-push (string-to-core (package-name package)) res)
    res))


;;;; Static bignum initialization:

;;; Initialize-bignums sets up the least-positive bignum and least-negative
;;; bignum in static bignum space.

(defun initialize-bignums ()
  (let ((lpb-handle (allocate-boxed-object (static bignum-ltype) 2))
	(lnb-handle (allocate-boxed-object (static bignum-ltype) 2))
	(least-positive-bignum (1+ most-positive-fixnum))
	(least-negative-bignum (1- most-negative-fixnum)))
    (write-memory lpb-handle (build-sub-object +-fixnum-ltype 0 2))
    (write-memory (handle-beyond lpb-handle 4)
		  (handle-on (ash least-positive-bignum -16)
			     (logand least-positive-bignum #xFFFF)))
    (write-memory lnb-handle (build-sub-object --fixnum-ltype 0 2))
    (write-memory (handle-beyond lnb-handle 4)
		  (handle-on (ash least-negative-bignum -16)
			     (logand least-negative-bignum #xFFFF)))))


;;; Initialize-Trap-Object  --  Internal
;;;
;;;    Allocate the trap object and initialize it so that calls to it jump to
;;; the TRAP-ERROR-HANDLER routine.
;;;
(defun initialize-trap-object ()
  (let ((obj (allocate-g-vector (dynamic trap-ltype) 3))
	(addr (miscop-address 'clc::trap-error-handler)))
    (write-indexed obj %function-code-slot
		   (handle-on (ldb (byte 16 16) addr)
			      (ldb (byte 16 0) addr)))
    (write-indexed obj %function-offset-slot (number-to-core 0))))


;;; Reading FASL files.

(defvar cold-fop-functions (replace (make-array 256) fop-functions)
  "FOP functions for cold loading.")

(defvar normal-fop-functions)

;;; Define-Cold-FOP  --  Internal
;;;
;;;    Like Define-FOP in load, but looks up the code, and stores into
;;; the cold-fop-functions vector.
;;;
(defmacro define-cold-fop ((name &optional (pushp t)) &rest forms)
  (let ((fname (concat-pnames 'cold- name))
	(code (get name 'fop-code)))
    (unless code (error "~S is not a defined fop." name))
    `(progn
      (defun ,fname ()
	,(if (eq pushp :nope)
	     `(progn ,@forms)
	     `(with-fop-stack ,pushp ,@forms)))
      (setf (svref cold-fop-functions ,code) #',fname))))

;;; Clone-Cold-FOP  --  Internal
;;;
;;;    Clone a couple of cold fops.
;;;
(defmacro clone-cold-fop ((name &optional (pushp t)) (small-name) &rest forms)
  `(progn
    (macrolet ((clone-arg () '(read-arg 4)))
      (define-cold-fop (,name ,pushp) ,@forms))
    (macrolet ((clone-arg () '(read-arg 1)))
      (define-cold-fop (,small-name ,pushp) ,@forms))))

;;; Not-Cold-Fop  --  Internal
;;;
;;;    Define a fop to be undefined in cold load.
;;;
(defmacro not-cold-fop (name)
  `(define-cold-fop (,name)
     (error "~S is not supported in cold load." ',name)))

;;;; Random cold fops...

(define-cold-fop (fop-misc-trap) trap-handle)

(define-cold-fop (fop-character)
  (build-object bitsy-char-ltype (read-arg 3)))
(define-cold-fop (fop-short-character)
  (build-object string-char-ltype (read-arg 1)))

(define-cold-fop (fop-empty-list) nil-handle)
(define-cold-fop (fop-truth) (cold-intern 't *lisp-package*))

(define-cold-fop (fop-normal-load :nope)
  (setq fop-functions normal-fop-functions))

(define-cold-fop (fop-maybe-cold-load :nope))

(define-cold-fop (fop-structure)
  (cold-set-vector-subtype (pop-stack) 1))


;;;; Loading symbols...

;;; Cold-Load-Symbol loads a symbol N characters long from the File and interns
;;; that symbol in the given Package.
;;;
(defun cold-load-symbol (size package)
  (let ((string (make-string size)))
    (read-n-bytes *fasl-file* string 0 size)
    (cold-intern (intern string package) package)))

(clone-cold-fop (fop-symbol-save)
		(fop-small-symbol-save)
  (push-table (cold-load-symbol (clone-arg) *package*)))

(macrolet ((frob (name pname-len package-len)
	     `(define-cold-fop (,name)
		(let ((index (read-arg ,package-len)))
		  (push-table
		   (cold-load-symbol (read-arg ,pname-len)
				     (svref *current-fop-table* index)))))))
  (frob fop-symbol-in-package-save 4 4)
  (frob fop-small-symbol-in-package-save 1 4)
  (frob fop-symbol-in-byte-package-save 4 1)
  (frob fop-small-symbol-in-byte-package-save 1 1))

(clone-cold-fop (fop-lisp-symbol-save)
		(fop-lisp-small-symbol-save)
  (push-table (cold-load-symbol (clone-arg) *lisp-package*)))

(clone-cold-fop (fop-keyword-symbol-save)
		(fop-keyword-small-symbol-save)
  (push-table (cold-load-symbol (clone-arg) *keyword-package*)))

(clone-cold-fop (fop-uninterned-symbol-save)
		(fop-uninterned-small-symbol-save)
  (let* ((size (clone-arg))
	 (name (make-string size)))
    (read-n-bytes *fasl-file* name 0 size)
    (let ((symbol (allocate-symbol name)))
      (push-table symbol))))


;;; Loading lists...

;;; Cold-Stack-List makes a list of the top Length things on the Fop-Stack.
;;; The last cdr of the list is set to Last.

(defmacro cold-stack-list (length last)
  `(do* ((index ,length (1- index))
	 (result ,last (allocate-cons (dynamic list-ltype) (pop-stack) result)))
	((= index 0) result)
     (declare (fixnum index))))

(define-cold-fop (fop-list)
  (cold-stack-list (read-arg 1) nil-handle))
(define-cold-fop (fop-list*)
  (cold-stack-list (read-arg 1) (pop-stack)))
(define-cold-fop (fop-list-1)
  (cold-stack-list 1 nil-handle))
(define-cold-fop (fop-list-2)
  (cold-stack-list 2 nil-handle))
(define-cold-fop (fop-list-3)
  (cold-stack-list 3 nil-handle))
(define-cold-fop (fop-list-4)
  (cold-stack-list 4 nil-handle))
(define-cold-fop (fop-list-5)
  (cold-stack-list 5 nil-handle))
(define-cold-fop (fop-list-6)
  (cold-stack-list 6 nil-handle))
(define-cold-fop (fop-list-7)
  (cold-stack-list 7 nil-handle))
(define-cold-fop (fop-list-8)
  (cold-stack-list 8 nil-handle))
(define-cold-fop (fop-list*-1)
  (cold-stack-list 1 (pop-stack)))
(define-cold-fop (fop-list*-2)
  (cold-stack-list 2 (pop-stack)))
(define-cold-fop (fop-list*-3)
  (cold-stack-list 3 (pop-stack)))
(define-cold-fop (fop-list*-4)
  (cold-stack-list 4 (pop-stack)))
(define-cold-fop (fop-list*-5)
  (cold-stack-list 5 (pop-stack)))
(define-cold-fop (fop-list*-6)
  (cold-stack-list 6 (pop-stack)))
(define-cold-fop (fop-list*-7)
  (cold-stack-list 7 (pop-stack)))
(define-cold-fop (fop-list*-8)
  (cold-stack-list 8 (pop-stack)))


;;;; Loading vectors...

(clone-cold-fop (fop-string)
		(fop-small-string)
  (let* ((len (clone-arg))
	 (string (make-string len)))
    (read-n-bytes *fasl-file* string 0 len)
    (i-vector-to-core (dynamic string-ltype) 8 len 0 string)))

(clone-cold-fop (fop-vector)
		(fop-small-vector)
  (let ((size (clone-arg)))
    (do ((index (1- size) (1- index))
	 (result (allocate-g-vector (dynamic general-vector-ltype) size)))
	((< index 0) result)
      (declare (fixnum index))
      (write-indexed result index (pop-stack)))))


(define-cold-fop (fop-int-vector)
  (prepare-for-fast-read-byte *fasl-file*
    (let* ((len (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (ac (1- (integer-length size)))
	   (res (%primitive alloc-i-vector len ac)))
      (done-with-fast-read-byte)
      (unless (and (<= ac 5) (= size (ash 1 ac)))
	(error "Losing element size ~S." size))
      (read-n-bytes *fasl-file* res 0 (ash (+ (ash len ac) 7) -3))
      (i-vector-to-core (if (typep res 'simple-bit-vector)
			    (dynamic bit-vector-ltype)
			    (dynamic integer-vector-ltype))
			size len 0 res))))
			

(not-cold-fop fop-uniform-vector)
(not-cold-fop fop-small-uniform-vector)
(not-cold-fop fop-uniform-int-vector)
(not-cold-fop fop-array)


;;;; Fixing up circularities.

(define-cold-fop (fop-rplaca nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-memory (cold-nthcdr idx obj) (pop-stack))))

(define-cold-fop (fop-rplacd nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-memory (handle-beyond (cold-nthcdr idx obj) lispobj-size)
		  (pop-stack))))

(define-cold-fop (fop-svset nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-indexed obj idx (pop-stack))))

(define-cold-fop (fop-nthcdr t)
  (cold-nthcdr (read-arg 4) (pop-stack)))


(defun cold-nthcdr (index obj)
  (do ((i 0 (1+ i))
       (r obj))
      ((>= i index) r)
    (setq r (read-memory (handle-beyond r lispobj-size)))))


;;;; Calling (or not calling).

(not-cold-fop fop-alter)
(not-cold-fop fop-eval)
(not-cold-fop fop-eval-for-effect)
(not-cold-fop fop-funcall)

(define-cold-fop (fop-funcall-for-effect nil)
  (if (= (read-arg 1) 0)
      (cold-push (pop-stack) current-init-functions-cons)
      (error "Can't FOP-FUNCALL random stuff in cold load.")))



;;; Loading assembler code:


(defun maybe-grow-machine-code-buffer (length)
  (if (> length (length machine-code-buffer))
      (setq machine-code-buffer
	    (make-array (logand (+ length 2047) (lognot 2047))
			:element-type '(unsigned-byte 8)))))


;;; Define-Cold-Labels  --  Internal
;;;
;;;    Stick the addresses of some labels on the plists.  We do this
;;; in the core being built as well so that the normal loader can
;;; load assembler code.  %Loaded-Address is a byte offset.
;;;
(defun define-cold-labels (start external-labels)
  (let ((prop (cold-intern '%loaded-address *lisp-package*)))
    (dolist (lab external-labels)
      (let ((addr (+ start (cdr lab)))
	    (name (car lab)))
	(setf (get name '%address) addr)
	(cold-put (cold-intern name (symbol-package name))
		  prop (number-to-core (ash addr 1)))))))


(defun miscop-address (name)
  (ash (or (get name '%address)
	   (error "Miscop ~A doesn't seem to be defined." name))
       1))

(define-cold-fop (fop-assembler-routine t)
  (let* ((code-start (copy-handle
		      (space-free-pointer
		       (svref memory code-space))))
	 (start (- (ash (handle-offset code-start) 1)
		   (ash clc::romp-code-base 16)))
	 (code-length (read-arg 4)))
    (declare (fixnum code-length))
    ;; Zap the code into the core image:
    (maybe-grow-machine-code-buffer code-length)
    (read-n-bytes *fasl-file* machine-code-buffer 0 code-length)
    (%primitive byte-blt machine-code-buffer 0
		(space-real-address (svref memory code-space))
		start (+ start code-length))
    (bump-handle (space-free-pointer (svref memory code-space)) code-length)
    code-start))


;;; Fixup-Assembler-Code  --  Internal
;;;
;;;    Since we now always access miscops through their name, we don't need to
;;; discriminate between assembler routines and miscops.  So we have one
;;; function that does both kinds of fixups.
;;;
(defun fixup-assembler-code ()
  (with-fop-stack nil
    (let* ((external-references (pop-stack))
	   (external-labels (pop-stack))
	   (name (pop-stack))
	   (code-start (pop-stack))
	   (start (handle-offset code-start)))
      (define-cold-labels start external-labels)
      (push (cons name external-references) *external-references*))))

(define-cold-fop (fop-fixup-miscop-routine :nope)
  (fixup-assembler-code))

(define-cold-fop (fop-fixup-assembler-routine :nope)
  (fixup-assembler-code))

(not-cold-fop fop-fixup-user-miscop-routine)


;;; Loading functions...


;;; Cold-Load-Function loads a code object (constant pool and code vector).
;;; Box-Num objects are popped off the stack for the boxed storage section,
;;; then Code-Length bytes are read in and made into the code vector.

(defun cold-load-function (box-num code-length)
  (declare (fixnum box-num code-length))
  (with-fop-stack t
    (let ((function (allocate-g-vector (dynamic function-ltype) box-num))
	  (code (%primitive alloc-i-vector code-length 3)))
      ;;
      ;; Pop boxed stuff, storing it in the allocated function object.
      (do ((index (1- box-num) (1- index)))
	  ((minusp index))
	(write-indexed function index (pop-stack)))
      ;;
      ;; Set Code object subtype.
      (cold-set-vector-subtype function %function-constants-subtype)
      ;;
      ;; Read in code, dump it, and store code pointer.
      (read-n-bytes *fasl-file* code 0 code-length)
      (let ((code-handle (i-vector-to-core (dynamic code-ltype)
					   8 code-length 1 code)))
	(write-indexed function %function-code-slot code-handle))
      function)))

(define-cold-fop (fop-code :nope)
  (if (eql *current-code-format* c::target-fasl-code-format)
      (cold-load-function (read-arg 4) (read-arg 4))
      (error "~S: Bad code format for this implementation"
	     *current-code-format*)))

(define-cold-fop (fop-small-code :nope)
  (if (eql *current-code-format* c::target-fasl-code-format)
      (cold-load-function (read-arg 1) (read-arg 2))
      (error "~S: Bad code format for this implementation"
	     *current-code-format*)))


;;; Kind of like Cold-Load-Function, except that we set the Code and Constants
;;; slots from the Constants object that is our first stack argument.  The
;;; subtype is set to the second stack argument.
;;;
;;; We make an entry in the *Defined-Functions* for benefit of the map file.
;;; If the entry's Name is a symbol (meaning the function is a DEFUN), then
;;; dump that name, otherwise we say UNKNOWN-FUNCTION.
;;;
(define-cold-fop (fop-function-entry)
  (let* ((box-num (read-arg 1))
	 (function (allocate-g-vector (dynamic function-ltype) box-num)))
    ;;
    ;; Pop boxed things, storing them in the allocated function object.
    (do ((index (1- box-num) (1- index)))
	((minusp index))
      (write-indexed function index (pop-stack)))
    ;;
    ;; Set the subtype of the function object.  The subtype should be a small
    ;; fixnum, so its value is the cdr of the handle...
    (cold-set-vector-subtype function (cdr (pop-stack)))

    (let* ((constants-handle (pop-stack))
	   (code-handle (read-indexed constants-handle %function-code-slot))
	   (offset-handle (read-indexed function %function-offset-slot))
	   (offset (logior (ash (car offset-handle) 16) (cdr offset-handle))))
      (write-indexed function %function-code-slot code-handle)
      (write-indexed function %function-entry-constants-slot constants-handle)

      (let ((name-handle (read-indexed function %function-name-slot)))
	(push (list (if (= (handle-type name-handle) symbol-ltype)
			(find-cold-symbol name-handle)
			'unknown-function)
		    function
		    (handle-beyond code-handle offset))
	      *defined-functions*)))

    function))


(define-cold-fop (fop-fset nil)
  (let ((function (pop-stack)))
    (write-memory (handle-beyond (pop-stack) lispobj-size) function)))


(define-cold-fop (fop-user-miscop-fixup)
  (let* ((name (find-cold-symbol (pop-stack)))
	 (function-object (pop-stack))
	 (offset (read-arg 4))
	 (miscop-address (miscop-address name))
	 (addr-high (ldb (byte 16 16) miscop-address))
	 (addr-low (ldb (byte 16 0) miscop-address))
	 (code-handle (read-indexed function-object %function-code-slot)))
    
    (bump-handle code-handle (+ cold-i-vector-header-size offset))
    (let ((inst (read-memory code-handle)))
      (write-memory code-handle
		    (handle-on (logior (logand (car inst) #xFF00)
				       (logand addr-high #xFF))
			       addr-low)))
    function-object))


;;; Modify a slot in a Constants object.
;;;
(clone-cold-fop (fop-alter-code nil) (fop-byte-alter-code)
  (let ((value (pop-stack))
	(code (pop-stack))
	(index (clone-arg)))
    (write-indexed code index value)))


;;; Loading numbers...

(define-cold-fop (fop-float :nope)
  (fop-float)
  (with-fop-stack t (number-to-core (pop-stack))))

(clone-cold-fop (fop-integer)
		(fop-small-integer)
  (number-to-core (load-s-integer (clone-arg))))

(define-cold-fop (fop-word-integer)
  (number-to-core (load-s-integer 4)))

(define-cold-fop (fop-byte-integer)
  (let ((byte (load-s-integer 1)))
    (build-object (if (< byte 0) --fixnum-ltype +-fixnum-ltype) byte)))

(define-cold-fop (fop-ratio)
  (let ((dest (allocate-boxed-object (dynamic ratio-ltype) 2))
	(den (pop-stack)))
    (write-memory dest (pop-stack))
    (write-memory (handle-beyond dest lispobj-size) den)
    dest))

(define-cold-fop (fop-complex)
  (let ((dest (allocate-boxed-object (dynamic complex-ltype) 2))
	(im (pop-stack)))
    (write-memory dest (pop-stack))
    (write-memory (handle-beyond dest lispobj-size) im)
    dest))



;;; Resolving all the assembler routines' references.

(defmacro create-assembler-handle (address)
  `(handle-on (logior (ash ,assembly-code-ltype type-shift)
		      (logand (ash ,address -15) high-data-mask))
	      (logand (ash ,address 1) #xFFFF)))

;;; Recall that the format of a reference is (How Label Location),
;;; where How is one of JI, BI, BA, or L, Label is the label's name, and
;;; Location is the location of the reference.  These things are stored on
;;; the list *external-references* as (Name . References), where Name is
;;; the name of the referencing routine, and References is a list of references
;;; in the above format.

(defun resolve-references ()
  (dolist (reflist *external-references*)
    (let ((address (get (car reflist) '%address)))
      (dolist (refs (cdr reflist))
	(let ((how (car refs))
	      (label (get (cadr refs) '%address))
	      (location (caddr refs)))
	  (if (null label)
	      (format t "~A references ~A, which has not been defined.~%"
		      (car reflist) (cadr refs))
	      (ecase how
		(clc::ji
		 (resolve-ji-reference (car reflist) address (cadr refs)
				       label location))
		(clc::bi
		 (resolve-bi-reference (car reflist) address (cadr refs)
				       label location))
		(clc::ba
		 (resolve-ba-reference (car reflist) address (cadr refs)
					  label location)))))))))


(defun resolve-ji-reference (routine-name address label-name label location)
  (let* ((handle (handle-beyond (create-assembler-handle address)
			       (ash location 1)))
	 (offset (- label (handle-offset handle)))
	 (inst (read-memory handle)))
    (if (or (< offset #x-80) (> offset #x7F))
	(format t "Offset #X~X out of JI range for ~A to reference ~A.~%"
		offset routine-name label-name))
    (write-memory handle
		  (handle-on (logior (logand (car inst) #xFF00)
				     (logand offset #xFF))
			     (cdr inst)))))

(defun resolve-bi-reference (routine-name address label-name label location)
  (let* ((handle (handle-beyond (create-assembler-handle address)
				(ash location 1)))
	 (offset (- label (handle-offset handle)))
	 (inst (read-memory handle)))
    (if (or (< offset #x-80000) (> offset #x7FFFF))
	(format t "Offset #X~X out of BI range for ~A to reference ~A.~%"
		offset routine-name label-name))
    (write-memory handle (handle-on (logior (logand (car inst) #xFFF0)
					    (logand (ash offset -16) #xF))
				    (logand offset #xFFFF)))))

(defun resolve-ba-reference (routine-name address label-name label location)
  (let* ((handle (handle-beyond (create-assembler-handle address)
				(ash location 1)))
	 (l-handle (create-assembler-handle label))
	 (l-addr (ash (logior (ash (car l-handle) 15) (cdr l-handle)) -1))
	 (inst (read-memory handle)))
    (if (> l-addr #xFFFFFF)
	(format t "Address #X~X is out of BA range for ~A to reference ~A.~%"
		l-addr routine-name label-name))
    (write-memory handle
		  (handle-on (logior (logand (car inst) #xFF00)
				     (logand (ash l-addr -16) #xFF))
			     (logand l-addr #xFFFF)))))

;;; Writing the core file.

;;; We assume here that the directory will fit on one page, make the
;;; alloc table be the first data page, and make the escape routine table
;;; the second data page.  So the length of the file is the length of all
;;; of the stuff in the used spaces plus 3 pages.

(eval-when (compile load eval)
  (defconstant offset-to-page-shift -13)
  (defconstant page-to-offset-shift 13)
  (defconstant process-page-shift -4)
  (defconstant extra-pages 1)
  (defconstant alloc-table-data-start 4096)
  (defconstant alloc-table-page (ash (ash clc::romp-data-base 16) offset-to-page-shift))
  (defconstant interrupt-routine-offset (+ 2048 132))
)

(defun write-initial-core-file (name)
  (format t "[Building Initial Core File in file ~S: ~%" name)
  (force-output t)
  (let ((data-pages (1+ extra-pages))
	(nonempty-spaces 0)
	(byte-length 0))
    (do ((spaces used-spaces (cdr spaces)))
	((null spaces) (setq byte-length (ash data-pages page-to-offset-shift)))
      (let ((free (handle-offset
		   (space-free-pointer (svref memory (car spaces))))))
	(when (eq (car spaces) code-space)
	  (decf free (ash clc::romp-code-base 15)))
	(when (> free 0)
	  (incf nonempty-spaces)
	  (incf data-pages (1+ (ash free (1+ offset-to-page-shift)))))))
    (multiple-value-bind (hunk addr) (get-valid-hunk byte-length)
      (setq hunk (int-sap hunk))
      (setq addr (int-sap addr))
      ;; Write the CORE file password.
      (%primitive 16bit-system-set hunk 0 (byte-swap-16 #x+434F))
      (%primitive 16bit-system-set hunk 1 (byte-swap-16 #x+5245))
      ;; Write the directory header entry.
      (%primitive 16bit-system-set hunk 2 0)
      (%primitive 16bit-system-set hunk 3 (byte-swap-16 3841))
      (%primitive 16bit-system-set hunk 4 0)
      (%primitive 16bit-system-set hunk 5
		  (byte-swap-16 (+ 2 (* 3 (+ nonempty-spaces extra-pages)))))
      ;; First, an entry for the alloc table.
      (%primitive 16bit-system-set hunk 6 0)	; Data page
      (%primitive 16bit-system-set hunk 7 0)
      (%primitive 16bit-system-set hunk 8
		  (byte-swap-16 (logand (ash alloc-table-page -16) #xFFFF)))
      (%primitive 16bit-system-set hunk 9
		  (byte-swap-16 (logand alloc-table-page #xFFFF)))
      (%primitive 16bit-system-set hunk 10 0)
      (%primitive 16bit-system-set hunk 11 (byte-swap-16 1))	; Page count
      ;; Construct the alloc table, with both free and clean pointers.
      (do ((space (ash first-pointer-ltype 2) (1+ space)))
	  ((> space (+ (ash largest-ltype 2) 3)))
	(let ((alloc-index (+ alloc-table-data-start (ash space 2))))
	  (cond ((svref memory space)
		 (let ((free (space-free-pointer (svref memory space))))
		   (when (eq space code-space)
		     (handle-on (- (car free) clc::romp-code-base)
				(cdr free)))
		   (%primitive 16bit-system-set hunk alloc-index
			       (byte-swap-16 (car free)))
		   (%primitive 16bit-system-set hunk (+ alloc-index 1)
			       (byte-swap-16 (cdr free)))
		   (%primitive 16bit-system-set hunk (+ alloc-index 2)
			       (byte-swap-16 (ash space type-space-shift)))
		   (%primitive 16bit-system-set hunk (+ alloc-index 3) 0)))
		(t
		 (%primitive 16bit-system-set hunk alloc-index
			     (byte-swap-16 (ash space type-space-shift)))
		 (%primitive 16bit-system-set hunk (+ alloc-index 1) 0)
		 (%primitive 16bit-system-set hunk (+ alloc-index 2)
			     (byte-swap-16 (ash space type-space-shift)))
		 (%primitive 16bit-system-set hunk (+ alloc-index 3) 0)))))

      (let ((index (+ alloc-table-data-start
		      (ash interrupt-routine-offset -1)))
	    (addr (miscop-address 'clc::interrupt-routine)))
	(%primitive 16bit-system-set hunk index
		    (byte-swap-16 (ldb (byte 16 16) addr)))
	(%primitive 16bit-system-set hunk (1+ index)
		    (byte-swap-16 (ldb (byte 16 0) addr))))

      ;; Then, write entries for each space.
      (do ((spaces used-spaces (cdr spaces))
	   (index 12)
	   (data-page extra-pages))
	  ((null spaces)
	   ;; Finally, the end of header code.
	   (%primitive 16bit-system-set hunk index 0)
	   (%primitive 16bit-system-set hunk (+ index 1) (byte-swap-16 3840))
	   (%primitive 16bit-system-set hunk (+ index 2) 0)
	   (%primitive 16bit-system-set hunk (+ index 3) (byte-swap-16 2)))
	(let* ((space (car spaces))
	       (free (handle-offset
		      (space-free-pointer (svref memory space)))))
	  (if (> free 0)
	      (let ((page-count (1+ (ash free (1+ offset-to-page-shift))))
		    (process-page (ash space process-page-shift))
		    (process-page-low (ash (logand space (1- (ash 1 (- process-page-shift))))
					   (+ 16 process-page-shift))))
		(when (eq space code-space)
		  (let ((ppage (ash (ash clc::romp-code-base 16) offset-to-page-shift)))
		    (setq process-page (ash ppage -16))
		    (setq process-page-low (logand ppage #xFFFF)))
		  (decf page-count (ash (ash clc::romp-code-base 16) offset-to-page-shift)))
		;; Make the directory entry.
		(%primitive 16bit-system-set hunk index 0)
		(%primitive 16bit-system-set hunk (+ index 1)
			    (byte-swap-16 data-page))
		(%primitive 16bit-system-set hunk (+ index 2)
			    (byte-swap-16 process-page))
		(%primitive 16bit-system-set hunk (+ index 3)
			    (byte-swap-16 process-page-low))
		(%primitive 16bit-system-set hunk (+ index 4) 0)
		(%primitive 16bit-system-set hunk (+ index 5)
			    (byte-swap-16 page-count))
		(format t "	~S page~:P, page ~S, from space #X~X.~%"
			page-count (1+ data-page) space)
		;; Move the words into the file.
		(%primitive byte-blt (space-real-address (svref memory space))
			    0 addr
			    (ash (1+ data-page) page-to-offset-shift)
			    (+ (ash (1+ data-page) page-to-offset-shift)
			       (ash page-count page-to-offset-shift)))
		(incf data-page page-count)
		(incf index 6)))))
      (let ((name (predict-name name nil)))
	(format t "Writing ~A.~%" name)
	(multiple-value-bind (fd err) (mach:unix-creat name #o644)
	  (when (null fd)
	    (error "Open on ~A failed, unix error ~S."
		   name (mach:get-unix-error-msg err)))
	  (multiple-value-bind (res err) (mach:unix-write fd addr 0 byte-length)
	    (when (null res)
	      (error "Write of core file ~S failed, unix error ~S."
		     name (mach:get-unix-error-msg err))))
	  (unless (eq (mach:unix-close fd) T)
	    (error "Close of core file ~S failed, unix error ~S."
		   name (mach:get-unix-error-msg err)))))
      (write-line "Done!]")
      t)))


;;; Top level.


(defun clean-up-genesis ()
  (when (boundp '*cold-packages*)
    (dolist (p *cold-packages*)
      (dolist (symbol (cdr p))
	(remprop symbol 'cold-info))))
  (setq *defined-functions* nil)
  (setq *external-references* nil)
  (setq *cold-packages* ())
  (setq *cold-map-file* nil)
  (setq current-init-functions-cons nil-handle))


(defun genesis (file-list core-name &optional map-file)
  "Builds a kernel Lisp image from the .FASL files specified in the given
  File-List and writes it to a file named by Core-Name."
  (fresh-line)
  (clean-up-genesis)
  (when (eq map-file t)
    (setq map-file (make-pathname :defaults core-name :type "map")))

  (when map-file
    (setq *cold-map-file* (open map-file :direction :output
				:if-exists :supersede))
    (format *cold-map-file* "Map file for ~A:" core-name))
  (initialize-memory)
  (initialize-spaces)
  (initialize-symbols)
  (initialize-bignums)
  (dolist (file file-list)
    (write-line file)
    (let* ((normal-fop-functions fop-functions)
	   (fop-functions cold-fop-functions)
	   (*in-cold-load* t))
      (load file)))
  (initialize-trap-object)
  (finish-symbols)
  (resolve-references)
  (write-memory (cold-intern '*lisp-initialization-functions* *lisp-package*)
		current-init-functions-cons)
  (write-map-file)
  (write-initial-core-file core-name))


;;;; Writing the map file:


(defun write-map-file ()
  (when *cold-map-file*
    (let* ((file *cold-map-file*)
	   (reflist *external-references*)
	   (*print-pretty* nil)
	   (*print-case* :upcase))
      (declare (stream file))
      (format file "~%~|Functions defined in core image:~%~%")
      (dolist (info (nreverse *defined-functions*))
	(let ((name (first info))
	      (funobj (second info))
	      (code (third info)))
	  (format file "~X,,~4,'0X	~X,,~4,'0X	~S~%"
		  (car funobj) (cdr funobj)
		  (car code) (cdr code)
		  name)))
      
      (format file "~%~|Assembler routine locations:~%~%")
      (let ((refs NIL))
	(dolist (ref reflist) (push (car ref) refs))
	(setq refs
	      (sort refs #'(lambda (x y)
			     (< (get x '%address) (get y '%address)))))
	(dolist (ref refs)
	  (format file "~X  ~50S~%" (ash (get ref '%address) 1) ref))))
    (close *cold-map-file*)))
