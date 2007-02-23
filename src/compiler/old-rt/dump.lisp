;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;
;;;    This file contains stuff that knows about dumping code both to files to
;;; the running Lisp.
;;;
(in-package 'c)

(proclaim '(special compiler-version))

(import '(system:%primitive system:%array-data-slot
			    system:%array-displacement-slot
			    system:%g-vector-structure-subtype
			    system:%function-constants-constants-offset))



;;;; Fasl dumper state:

;;; The Fasl-File structure represents everything we need to know about dumping
;;; to a fasl file.  We need to objectify the state, since the fasdumper must
;;; be reentrant.
;;;
(defstruct (fasl-file
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<Fasl-File ~S>"
		       (namestring (fasl-file-stream s))))))
  ;;
  ;; The stream we dump to.
  (stream nil :type stream)
  ;;
  ;; Hashtables we use to keep track of dumped constants so that we can get
  ;; them from the table rather than dumping them again.  The EQUAL-TABLE is
  ;; used for lists and strings, and the EQ-TABLE is used for everything else.
  ;; We use a separate EQ table to avoid performance patholigies with objects
  ;; for which EQUAL degnerates to EQL.  Everything entered in the EQUAL table
  ;; is also entered in the EQ table.
  (equal-table (make-hash-table :test #'equal) :type hash-table)
  (eq-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; The table's current free pointer: the next offset to be used.
  (table-free 0 :type unsigned-byte)
  ;;
  ;; Alist (Package . Offset) of the table offsets for each package we have
  ;; currently located.
  (packages () :type list)
  ;;
  ;; Table mapping from the Entry-Info structures for dumped XEPs to the table
  ;; offsets of the corresponding code pointers.
  (entry-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; Table holding back-patching info for forward references to XEPs.  The key
  ;; is the Entry-Info structure for the XEP, and the value is a list of conses
  ;; (<code-handle> . <offset>), where <code-handle> is the offset in the table
  ;; of the code object needing to be patched, and <offset> is the offset that
  ;; must be patched.
  (patch-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; A list of the table handles for all of the DEBUG-INFO structures dumped in
  ;; this file.  These structures must be back-patched with source location
  ;; information when the compilation is complete.
  (debug-info () :type list)
  ;;
  ;; Used to keep track of objects that we are in the process of dumping so that
  ;; circularities can be preserved.  The key is the object that we have
  ;; previously seen, and the value is the object that we reference in the table
  ;; to find this previously seen object.  (The value is never NIL.)
  ;;
  ;; Except with list objects, the key and the value are always the same.  In a
  ;; list, the key will be some tail of the value.
  (circularity-table (make-hash-table :test #'eq) :type hash-table))


;;; This structure holds information about a circularity.
;;;
(defstruct circularity
  ;;
  ;; Kind of modification to make to create circularity.
  (type nil :type (member :rplaca :rplacd :svset))
  ;;
  ;; Object containing circularity.
  object
  ;;
  ;; Index in object for circularity.
  (index nil :type unsigned-byte)
  ;;
  ;; The object to be stored at Index in Object.  This is that the key that we
  ;; were using when we discovered the circularity.
  value
  ;;
  ;; The value that was associated with Value in the CIRCULARITY-TABLE.  This
  ;; is the object that we look up in the EQ-TABLE to locate Value.
  enclosing-object)


;;; A list of the Circularity structures for all of the circularities detected
;;; in the the current top-level call to Dump-Object.  Setting this lobotomizes
;;; circularity detection as well, since circular dumping uses the table.
;;;
(defvar *circularities-detected*)


;;; Used to inhibit table access when dumping forms to be read by the cold
;;; loader.
;;;
(defvar *cold-load-dump* nil)


;;;; Utilities:

;;; Dump-Byte  --  Internal
;;;
;;;    Write the byte B to the specified fasl-file stream.
;;;
(proclaim '(inline dump-byte))
(defun dump-byte (b file)
  (declare (type (unsigned-byte 8) b) (type fasl-file file))
  (write-byte b (fasl-file-stream file))
  (undefined-value))


;;; Dump-FOP  --  Internal
;;;
;;;    Dump the FOP code for the named FOP to the specified fasl-file.
;;;
(defun dump-fop (fs file)
  (declare (symbol fs) (type fasl-file file))
  (let ((val (get fs 'lisp::fop-code)))
    (assert val () "Compiler bug: ~S not a legal fasload operator." fs)
    (dump-byte val file))
  (undefined-value))


;;; Dump-FOP*  --  Internal
;;;
;;;    Dump a FOP-Code along with an integer argument, choosing the FOP based
;;; on whether the argument will fit in a single byte.
;;;
(defmacro dump-fop* (n byte-fop word-fop file)
  (once-only ((n-n n)
	      (n-file file))
    `(cond ((< ,n-n 256)
	    (dump-fop ',byte-fop ,n-file)
	    (dump-byte ,n-n ,n-file))
	   (t
	    (dump-fop ',word-fop ,n-file)
	    (quick-dump-number ,n-n 4 ,n-file)))))


;;; Quick-Dump-Number  --  Internal
;;;
;;;    Dump Num to the fasl stream, represented by the specified number of
;;; bytes.
;;;
(defun quick-dump-number (num bytes file)
  (declare (integer num) (type unsigned-byte bytes) (type fasl-file file))
  (let ((stream (fasl-file-stream file)))
    (do ((n num (ash n -8))
	 (i bytes (1- i)))
	((= i 0))
      (write-byte (logand n #xFF) stream)))
  (undefined-value))


;;; Dump-Push  --  Internal
;;;
;;;    Push the object at table offset Handle on the fasl stack.
;;;
(defun dump-push (handle file)
  (declare (type unsigned-byte handle) (type fasl-file file))
  (dump-fop* handle lisp::fop-byte-push lisp::fop-push file)
  (undefined-value))


;;; Dump-Pop  --  Internal
;;;
;;;    Pop the object currently on the fasl stack top into the table, and
;;; return the table index, incrementing the free pointer.
;;;
(defun dump-pop (file)
  (prog1 (fasl-file-table-free file)
    (dump-fop 'lisp::fop-pop file)
    (incf (fasl-file-table-free file))))


;;; EQUAL-CHECK-TABLE  --  Internal
;;;
;;;    If X is in File's EQUAL-TABLE, then push the object and return T,
;;; otherwise NIL.  If *COLD-LOAD-DUMP* is true, then do nothing and return
;;; NIL.
;;;
(defun equal-check-table (x file)
  (declare (type fasl-file file))
  (unless *cold-load-dump*
    (let ((handle (gethash x (fasl-file-equal-table file))))
      (cond (handle
	     (dump-push handle file)
	     t)
	    (t
	     nil)))))


;;; EQ-SAVE-OBJECT, EQUAL-SAVE-OBJECT  --  Internal
;;;
;;;    These functions are called after dumping an object to save the object in
;;; the table.  The object (also passed in as X) must already be on the top of
;;; the FOP stack.  If *COLD-LOAD-DUMP* is true, then we don't do anything.
;;;
(defun eq-save-object (x file)
  (declare (type fasl-file file))
  (unless *cold-load-dump*
    (let ((handle (dump-pop file)))
      (setf (gethash x (fasl-file-eq-table file)) handle)
      (dump-push handle file)))
  (undefined-value))
;;;
(defun equal-save-object (x file)
  (declare (type fasl-file file))
  (unless *cold-load-dump*
    (let ((handle (dump-pop file)))
      (setf (gethash x (fasl-file-equal-table file)) handle)
      (setf (gethash x (fasl-file-eq-table file)) handle)
      (dump-push handle file)))
  (undefined-value))


;;; NOTE-POTENTIAL-CIRCULARITY  --  Internal
;;;
;;;    Record X in File's CIRCULARITY-TABLE unless *COLD-LOAD-DUMP* is true.
;;; This is called on objects that we are about to dump might have a circular
;;; path through them.
;;;
;;; The object must not currently be in this table, since the dumper should
;;; never be recursively called on a circular reference.  Instead, the dumping
;;; function must detect the circularity and arrange for the dumped object to
;;; be patched.
;;;
(defun note-potential-circularity (x file)
  (unless *cold-load-dump*
    (let ((circ (fasl-file-circularity-table file)))
      (assert (not (gethash x circ)))
      (setf (gethash x circ) x)))
  (undefined-value))


;;; Fasl-Dump-Cold-Load-Form  --  Interface
;;;
;;;    Dump Form to a fasl file so that it evaluated at load time in normal
;;; load and at cold-load time in cold load.  This is used to dump package
;;; frobbing forms.
;;;
(defun fasl-dump-cold-load-form (form file)
  (declare (type fasl-file file))
  (dump-fop 'lisp::fop-normal-load file)
  (let ((*cold-load-dump* t))
    (dump-object form file))
  (dump-fop 'lisp::fop-eval-for-effect file)
  (dump-fop 'lisp::fop-maybe-cold-load file)
  (undefined-value))


;;;; Opening and closing:

;;; Open-Fasl-File  --  Interface
;;;
;;;    Return a Fasl-File object for dumping to the named file.  Some
;;; information about the source is specified by the string Where.
;;;
(defun open-fasl-file (name where)
  (declare (type pathname name))
  (let* ((stream (open name :direction :output
		       :if-exists :new-version
		       :element-type '(unsigned-byte 8)))
	 (res (make-fasl-file :stream stream)))
    (format stream
	    "FASL FILE output from ~A.~@
	    Compiled ~A on ~A~@
	    Compiler ~A, Lisp ~A~@
	    Targeted for ~A, FASL code format ~D~%"
	    where
	    (ext:format-universal-time nil (get-universal-time))
	    (machine-instance) compiler-version
	    (lisp-implementation-version) vm-version target-fasl-code-format)
    ;;
    ;; Terminate header.
    (dump-byte 255 res)
    ;;
    ;; Specify code format.
    (dump-fop 'lisp::fop-code-format res)
    (dump-byte target-fasl-code-format res)

    res))


;;; Close-Fasl-File  --  Interface
;;;
;;;    Close the specified Fasl-File, aborting the write if Abort-P is true.
;;; We do various sanity checks, then end the group.
;;;
(defun close-fasl-file (file abort-p)
  (declare (type fasl-file file))
  (dump-fop 'lisp::fop-verify-empty-stack file)
  (dump-fop 'lisp::fop-verify-table-size file)
  (quick-dump-number (fasl-file-table-free file) 4 file)
  (dump-fop 'lisp::fop-end-group file)
  (close (fasl-file-stream file) :abort abort-p)
  (undefined-value))


;;;; Component (function) dumping:

;;; Dump-Code-Object  --  Internal
;;;
;;;    Dump out the constant pool and code-vector for component, push the
;;; result in the table and return the offset.
;;;
;;;    The only tricky thing is handling constant-pool references to functions.
;;; If we have already dumped the function, then we just push the code pointer.
;;; Otherwise, we must create back-patching information so that the constant
;;; will be set when the function is eventually dumped.  This is a bit awkward,
;;; since we don't have the handle for the code object being dumped while we
;;; are dumping its constants.
;;;
;;;    We dump a trap object as a placeholder for the code vector, which is
;;; actually filled in by the loader.
;;;
(defun dump-code-object (component code-vector code-length node-vector
				   nodes-length file)
  (declare (type component component) (type fasl-file file)
	   (simple-vector node-vector)
	   (type unsigned-byte code-length nodes-length))
  (let* ((2comp (component-info component))
	 (constants (ir2-component-constants 2comp))
	 (num-consts (length constants)))
    (collect ((patches))
      (dump-object (component-name component) file)
      (dump-fop 'lisp::fop-misc-trap file)

      (let ((info (debug-info-for-component component node-vector
					    nodes-length)))
	(dump-object info file)
	(let ((info-handle (dump-pop file)))
	  (dump-push info-handle file)
	  (push info-handle (fasl-file-debug-info file))))

      (do ((i %function-constants-constants-offset (1+ i)))
	  ((= i num-consts))
	(let ((entry (aref constants i)))
	  (etypecase entry
	    (constant
	     (dump-object (constant-value entry) file))
	    (cons
	     (ecase (car entry)
	       (:entry
		(let* ((info (leaf-info (cdr entry)))
		       (handle (gethash info (fasl-file-entry-table file))))
		  (cond
		   (handle
		    (dump-push handle file))
		   (t
		    (patches (cons info i))
		    (dump-fop 'lisp::fop-misc-trap file)))))
	       (:label
		(dump-object (+ (label-location (cdr entry))
				clc::i-vector-header-size)
			     file))))
	    (null
	     (dump-fop 'lisp::fop-misc-trap file)))))

      (cond ((and (< num-consts #x100) (< code-length #x10000))
	     (dump-fop 'lisp::fop-small-code file)
	     (dump-byte num-consts file)
	     (quick-dump-number code-length 2 file))
	    (t
	     (dump-fop 'lisp::fop-code file)
	     (quick-dump-number num-consts 4 file)
	     (quick-dump-number code-length 4 file)))
      
      (write-string code-vector (fasl-file-stream file) :end code-length)
      
      (let ((handle (dump-pop file)))
	(dolist (patch (patches))
	  (push (cons handle (cdr patch))
		(gethash (car patch) (fasl-file-patch-table file))))
	handle))))


;;; Dump-Fixups  --  Internal
;;;
;;;    Dump all the fixups.  Currently there are only miscop fixups, and we
;;; always access them by name rather than number.  There is no reason for
;;; using miscop numbers other than a minor load-time efficiency win.
;;;
(defun dump-fixups (code-handle fixups file)
  (declare (type unsigned-byte code-handle) (list fixups)
	   (type fasl-file file))
  (dump-push code-handle file)
  (dolist (fixup fixups)
    (let ((offset (second fixup))
	  (value (third fixup)))
      (ecase (first fixup)
	(:miscop
	 (assert (symbolp value))
	 (dump-object value file)
	 (dump-fop 'lisp::fop-user-miscop-fixup file)
	 (quick-dump-number offset 4 file)))))

  (dump-fop 'lisp::fop-pop-for-effect file)
  (undefined-value))


;;; Dump-One-Entry  --  Internal
;;;
;;;    Dump a function-entry data structure corresponding to Entry to File.
;;; Code-Handle is the table offset of the code object for the component.
;;;
;;; If the entry is a DEFUN, then we also dump a FOP-FSET so that the cold
;;; loader can instantiate the definition at cold-load time, allowing forward
;;; references to functions in top-level forms.
;;;
(defun dump-one-entry (entry code-handle file)
  (declare (type entry-info entry) (type unsigned-byte code-handle)
	   (type fasl-file file))
  (let ((name (entry-info-name entry)))
    (dump-push code-handle file)
    (dump-object (if (entry-info-closure-p entry)
		     system:%function-closure-entry-subtype
		     system:%function-entry-subtype)
		 file)

    (dump-object name file)
    (dump-fop 'lisp::fop-misc-trap file)
    (dump-object (+ (label-location (entry-info-offset entry))
		    clc::i-vector-header-size)
		 file)
    (dump-fop 'lisp::fop-misc-trap file)
    (dump-object (entry-info-arguments entry) file)
    (dump-object (entry-info-type entry) file)
    (dump-fop 'lisp::fop-function-entry file)
    (dump-byte 6 file)

    (let ((handle (dump-pop file)))
      (when (and name (symbolp name))
	(dump-object name file)
	(dump-push handle file)
	(dump-fop 'lisp::fop-fset file))
      handle)))


;;; Alter-Code-Object  --  Internal
;;;
;;;    Alter the code object referenced by Code-Handle at the specified Offset,
;;; storing the object referenced by Entry-Handle.
;;;
(defun alter-code-object (code-handle offset entry-handle file)
  (dump-push code-handle file)
  (dump-push entry-handle file)
  (dump-fop* offset lisp::fop-byte-alter-code lisp::fop-alter-code file)
  (undefined-value))


;;; Fasl-Dump-Component  --  Interface
;;;
;;;    Dump the code, constants, etc. for component.  We pass in the assembler
;;; fixups, code vector and node info.
;;;
(defun fasl-dump-component (component code-vector code-length
				      node-vector nodes-length
				      fixups file)
  (declare (type component component) (type unsigned-byte length)
	   (list fixups) (type fasl-file file))

  (dump-fop 'lisp::fop-verify-empty-stack file)
  (dump-fop 'lisp::fop-verify-table-size file)
  (quick-dump-number (fasl-file-table-free file) 4 file)

  (let ((code-handle (dump-code-object component code-vector code-length
				       node-vector nodes-length file))
	(2comp (component-info component)))
    (dump-fixups code-handle fixups file)
    (dump-fop 'lisp::fop-verify-empty-stack file)

    (dolist (entry (ir2-component-entries 2comp))
      (let ((entry-handle (dump-one-entry entry code-handle file)))
	(setf (gethash entry (fasl-file-entry-table file)) entry-handle)

	(let ((old (gethash entry (fasl-file-patch-table file))))
	  (when old
	    (dolist (patch old)
	      (alter-code-object (car patch) (cdr patch) entry-handle file))
	    (remhash entry (fasl-file-patch-table file)))))))

  (assert (zerop (hash-table-count (fasl-file-patch-table file))))

  (undefined-value))


;;; FASL-DUMP-TOP-LEVEL-LAMBDA-CALL  --  Interface
;;;
;;;    Dump a FOP-FUNCALL to call an already dumped top-level lambda at load
;;; time.  
;;;
(defun fasl-dump-top-level-lambda-call (fun file)
  (declare (type clambda fun) (type fasl-file file))
  (let ((handle (gethash (leaf-info fun) (fasl-file-entry-table file))))
    (assert handle)
    (dump-push handle file)
    (dump-fop 'lisp::fop-funcall-for-effect file)
    (dump-byte 0 file))
  (undefined-value))


;;; FASL-DUMP-SOURCE-INFO  --  Interface
;;;
;;;    Compute the correct list of DEBUG-SOURCE structures and backpatch all of
;;; the dumped DEBUG-INFO structures.  We clear the FASL-FILE-DEBUG-INFO,
;;; so that subsequent components with different source info may be dumped.
;;;
(defun fasl-dump-source-info (info file)
  (declare (type source-info info) (type fasl-file file))
  (let ((res (debug-source-for-info info)))
    (dump-object res file)
    (let ((res-handle (dump-pop file)))
      (dolist (info-handle (fasl-file-debug-info file))
	(dump-push res-handle file)
	(dump-fop 'lisp::fop-svset file)
	(quick-dump-number info-handle 4 file)
	(quick-dump-number 2 4 file))))

  (setf (fasl-file-debug-info file) ())
  (undefined-value))


;;;; Main entries to object dumping:

;;; Dump-Non-Immediate-Object  --  Internal
;;;
;;;    This function deals with dumping objects that are complex enough so that
;;; we want to cache them in the table, rather than repeatedly dumping them.
;;; If the object is in the EQ-TABLE, then we push it, otherwise, we do a type
;;; dispatch to a type specific dumping function.  The type specific branches
;;; do any appropriate EQUAL-TABLE check and table entry.
;;;
;;;    When we go to dump the object, we enter it in the CIRCULARITY-TABLE.
;;;
(defun dump-non-immediate-object (x file)
  (let ((index (gethash x (fasl-file-eq-table file))))
    (cond ((and index (not *cold-load-dump*))
	   (dump-push index file))
	  (t
	   (typecase x
	     (symbol (dump-symbol x file))
	     (list
	      (unless (equal-check-table x file)
		(dump-list x file)
		(equal-save-object x file)))
	     (vector
	      (cond ((stringp x)
		     (unless (equal-check-table x file)
		       (dump-string x file)
		       (equal-save-object x file)))
		    ((subtypep (array-element-type x)
			       '(unsigned-byte 32))
		     (dump-i-vector x file)
		     (eq-save-object x file))
		    (t
		     (dump-vector x file)
		     (eq-save-object x file))))
	     (array
	      (dump-array x file)
	      (eq-save-object x file))
	     (number
	      (unless (equal-check-table x file)
		(etypecase x
		  (ratio (dump-ratio x file))
		  (complex (dump-complex x file))
		  (long-float (dump-long-float x file))
		  (integer (dump-integer x file)))
		(equal-save-object x file)))
#|
	     (compiled-function
	      (dump-function x file)
	      (eq-save-object x file))
|#
	     (t
	      (compiler-error
	       "This object cannot be dumped into a fasl file:~% ~S"
	       x))))))

  (undefined-value))


;;; Sub-Dump-Object  --  Internal
;;;
;;;    Dump an object of any type by dispatching to the correct type-specific
;;; dumping function.  We pick off immediate objects, symbols and and magic
;;; lists here.  Other objects are handled by Dump-Non-Immediate-Object.
;;;
;;; This is the function used for recursive calls to the fasl dumper.  We don't
;;; worry about creating circularities here, since it is assumed that there is
;;; a top-level call to Dump-Object.
;;;
(defun sub-dump-object (x file)
  (cond ((listp x)
	 (cond ((null x) (dump-fop 'lisp::fop-empty-list file))
	       #|
	       ((eq (car x) '%eval-at-load-time) (load-time-eval x))
	       |#
	       (t
		(dump-non-immediate-object x file))))
	((symbolp x)
	 (if (eq x t)
	     (dump-fop 'lisp::fop-truth file)
	     (dump-non-immediate-object x file)))
	((fixnump x) (dump-integer x file))
	((characterp x) (dump-character x file))
	((typep x 'short-float) (dump-short-float x file))
#| Probably a bug to ever dump a trap object...
	((lisp::trap-object-p x)
	 (dump-fop 'lisp::fop-misc-trap file))
|#
	(t
	 (dump-non-immediate-object x file))))


;;; Dump-Circularities  --  Internal
;;;
;;;    Dump stuff to backpatch already dumped objects.  Infos is the list of
;;; Circularity structures describing what to do.  The patching FOPs take the
;;; value to store on the stack.  We compute this value by fetching the
;;; enclosing object from the table, and then CDR'ing it if necessary.
;;;
(defun dump-circularities (infos file)
  (let ((table (fasl-file-eq-table file)))
    (dolist (info infos)
      (let* ((value (circularity-value info))
	     (enclosing (circularity-enclosing-object info)))
	(dump-push (gethash enclosing table) file)
	(unless (eq enclosing value)
	  (do ((current enclosing (cdr current))
	       (i 0 (1+ i)))
	      ((eq current value)
	       (dump-fop 'lisp::fop-nthcdr file)
	       (quick-dump-number i 4 file)))))
      
      (dump-fop (case (circularity-type info)
		  (:rplaca 'lisp::fop-rplaca)
		  (:rplacd 'lisp::fop-rplacd)
		  (:svset 'lisp::fop-svset))
		file)
      (quick-dump-number (gethash (circularity-object info) table) 4 file)
      (quick-dump-number (circularity-index info) 4 file))))


;;; Dump-Object  -- Interface
;;;
;;;    Set up stuff for circularity detection, then dump an object.  All shared
;;; and circular structure will be exactly preserved within a single call to
;;; Dump-Object.  Sharing between objects dumped by separate calls is only
;;; preserved when convenient.
;;;
;;;    We peek at the objec type so that we only pay the circular detection
;;; overhead on types of objects that might be circular.
;;;
(defun dump-object (x file)
  (if (or (arrayp x) (consp x))
      (let ((*circularities-detected* ())
	    (circ (fasl-file-circularity-table file)))
	(clrhash circ)
	(sub-dump-object x file)
	(when *circularities-detected*
	  (dump-circularities *circularities-detected* file)
	  (clrhash circ)))
      (sub-dump-object x file)))


#|
;;; Load-Time-Eval  --  Internal
;;;
;;;    This guy deals with the magical %Eval-At-Load-Time marker that
;;; #, turns into when the *compiler-is-reading* and a fasl file is being
;;; written.
;;;
(defun load-time-eval (x file)
  (when *compile-to-lisp*
    (compiler-error "#,~S in a bad place." (third x)))
  (assemble-one-lambda (cadr x))
  (dump-fop 'lisp::fop-funcall file)
  (dump-byte 0 file))
|#

;;;; Number Dumping:

;;; Dump a ratio

(defun dump-ratio (x file)
  (sub-dump-object (numerator x) file)
  (sub-dump-object (denominator x) file)
  (dump-fop 'lisp::fop-ratio file))

;;; Or a complex...

(defun dump-complex (x file)
  (sub-dump-object (realpart x) file)
  (sub-dump-object (imagpart x) file)
  (dump-fop 'lisp::fop-complex file))

;;; Dump an integer.

(defun dump-integer (n file)
  (let* ((bytes (compute-bytes n)))
    (cond ((= bytes 1)
	   (dump-fop 'lisp::fop-byte-integer file)
	   (dump-byte (logand #xFF n) file))
	  ((< bytes 5)
	   (dump-fop 'lisp::fop-word-integer file)
	   (quick-dump-number n 4 file))
	  ((< bytes 256)
	   (dump-fop 'lisp::fop-small-integer file)
	   (dump-byte bytes file)
	   (quick-dump-number n bytes file))
	  (t (dump-fop 'lisp::fop-integer file)
	     (quick-dump-number bytes 4 file)
	     (quick-dump-number n bytes file)))))

;;; Compute how many bytes it will take to represent signed integer N.

(defun compute-bytes (n)
  (truncate (+ (integer-length n) 8) 8))

;;;
;;; These two are almost exactly alike, and could easily be the same function.

(defun dump-short-float (x file)
  (multiple-value-bind (f exponent sign) (decode-float x)
    (let ((mantissa (truncate (scale-float (* f sign) (float-precision f)))))
      (dump-fop 'lisp::fop-float file)
      (dump-byte (1+ (integer-length exponent)) file)
      (quick-dump-number exponent (compute-bytes exponent) file)
      (dump-byte (1+ (integer-length mantissa)) file)
      (quick-dump-number mantissa (compute-bytes mantissa) file))))

#|
(defun dump-single-float (x file)
  (multiple-value-bind (f exponent sign) (decode-float x)
    (let ((mantissa (truncate (scale-float (* f sign) (float-precision f)))))
      (dump-fop 'lisp::fop-float file)
      (dump-byte (1+ (integer-length exponent)) file)
      (dump-byte exponent file)
      (dump-byte (1+ (integer-length mantissa)) file)
      (quick-dump-number mantissa (compute-bytes mantissa) file))))
|#
;;; For long-floats we're careful that the dumped mantissa actually
;;; has 63 significant bits, so the fasloader can recognize it as such.

(defun dump-long-float (x file)
  (multiple-value-bind (f exponent sign) (decode-float x)
    (let ((mantissa (truncate (scale-float (* f sign) (float-precision f)))))
      (dump-fop 'lisp::fop-float file)
      (dump-byte (1+ (integer-length exponent)) file)
      (quick-dump-number exponent (compute-bytes exponent) file)
      (dump-byte (1+ (integer-length mantissa)) file)
      (quick-dump-number mantissa (compute-bytes mantissa) file))))


;;;; Symbol Dumping:

;;; Dump-Package  --  Internal
;;;
;;;    Return the table index of Pkg, adding the package to the table if
;;; necessary.  During cold load, we read the string as a normal string so that
;;; we can do the package lookup at cold load time.
;;;
(defun dump-package (pkg file)
  (cond ((cdr (assoc pkg (fasl-file-packages file))))
	(t
	 (unless *cold-load-dump*
	   (dump-fop 'lisp::fop-normal-load file))
	 (dump-string (package-name pkg) file)
	 (dump-fop 'lisp::fop-package file)
	 (unless *cold-load-dump*
	   (dump-fop 'lisp::fop-maybe-cold-load file))
	 (let ((entry (dump-pop file)))
	   (push (cons pkg entry) (fasl-file-packages file))
	   entry))))


;;; Dump-Symbol  --  Internal
;;;
;;;    If we get here, it is assumed that the symbol isn't in the table, but we
;;; are responsible for putting it there when appropriate.  To avoid too much
;;; special-casing, we always push the symbol in the table, but don't record
;;; that we have done so if *Cold-Load-Dump* is true.
;;;
(defun dump-symbol (s file)
  (let* ((pname (symbol-name s))
	 (pname-length (length pname))
	 (pkg (symbol-package s)))

    (cond ((null pkg)
	   (dump-fop* pname-length lisp::fop-uninterned-small-symbol-save
		      lisp::fop-uninterned-symbol-save file))
	  ((eq pkg *package*)
	   (dump-fop* pname-length lisp::fop-small-symbol-save
		      lisp::fop-symbol-save file))
	  ((eq pkg ext:*lisp-package*)
	   (dump-fop* pname-length lisp::fop-lisp-small-symbol-save
		      lisp::fop-lisp-symbol-save file))
	  ((eq pkg ext:*keyword-package*)
	   (dump-fop* pname-length lisp::fop-keyword-small-symbol-save
		      lisp::fop-keyword-symbol-save file))
	  ((< pname-length 256)
	   (dump-fop* (dump-package pkg file)
		      lisp::fop-small-symbol-in-byte-package-save
		      lisp::fop-small-symbol-in-package-save file)
	   (dump-byte pname-length file))
	  (t
	   (dump-fop* (dump-package pkg file)
		      lisp::fop-symbol-in-byte-package-save
		      lisp::fop-symbol-in-package-save file)
	   (quick-dump-number pname-length 4 file)))

    (write-string pname (fasl-file-stream file))

    (unless *cold-load-dump*
      (setf (gethash s (fasl-file-eq-table file)) (fasl-file-table-free file)))

    (incf (fasl-file-table-free file)))

  (undefined-value))


;;; Dumper for lists.

;;; Dump-List  --  Internal
;;;
;;;    Dump a list, setting up patching information when there are
;;; circularities.  We scan down the list, checking for CDR and CAR
;;; circularities.
;;;
;;; If there is a CDR circularity, we terminate the list with NIL and make a
;;; Circularity notation for the CDR of the previous cons.
;;;
;;; If there is no CDR circularity, then we mark the current cons and check for
;;; a CAR circularity.  When there is a CAR circularity, we make the CAR NIL
;;; initially, arranging for the current cons to be patched later.
;;;
;;; Otherwise, we recursively call the dumper to dump the current element.
;;;
;;; Marking of the conses is inhibited when *cold-load-dump* is true.  This
;;; inhibits all circularity detection.
;;;
(defun dump-list (list file)
  (assert (and list
	       (not (gethash list (fasl-file-circularity-table file)))))
  (do* ((l list (cdr l))
	(n 0 (1+ n))
	(circ (fasl-file-circularity-table file)))
       ((atom l)
	(cond ((null l)
	       (terminate-undotted-list n file))
	      (t
	       (sub-dump-object l file)
	       (terminate-dotted-list n file))))

    (let ((ref (gethash l circ)))
      (when ref
	(push (make-circularity :type :rplacd  :object list  :index (1- n)
				:value l  :enclosing-object ref)
	      *circularities-detected*)
	(terminate-undotted-list n file)
	(return)))

    (unless *cold-load-dump*
      (setf (gethash l circ) list))

    (let* ((obj (car l))
	   (ref (gethash obj circ)))
      (cond (ref
	     (push (make-circularity :type :rplaca  :object list  :index n
				     :value obj  :enclosing-object ref)
		   *circularities-detected*)
	     (sub-dump-object nil file))
	    (t
	     (sub-dump-object obj file))))))


(defun terminate-dotted-list (n file)
  (case n
    (1 (dump-fop 'lisp::fop-list*-1 file))
    (2 (dump-fop 'lisp::fop-list*-2 file))
    (3 (dump-fop 'lisp::fop-list*-3 file))
    (4 (dump-fop 'lisp::fop-list*-4 file))
    (5 (dump-fop 'lisp::fop-list*-5 file))
    (6 (dump-fop 'lisp::fop-list*-6 file))
    (7 (dump-fop 'lisp::fop-list*-7 file))
    (8 (dump-fop 'lisp::fop-list*-8 file))
    (T (do ((nn n (- nn 255)))
	   ((< nn 256)
	    (dump-fop 'lisp::fop-list* file)
	    (dump-byte nn file))
	 (dump-fop 'lisp::fop-list* file)
	 (dump-byte 255 file)))))

;;; If N > 255, must build list with one list operator, then list* operators.

(defun terminate-undotted-list (n file)
    (case n
      (1 (dump-fop 'lisp::fop-list-1 file))
      (2 (dump-fop 'lisp::fop-list-2 file))
      (3 (dump-fop 'lisp::fop-list-3 file))
      (4 (dump-fop 'lisp::fop-list-4 file))
      (5 (dump-fop 'lisp::fop-list-5 file))
      (6 (dump-fop 'lisp::fop-list-6 file))
      (7 (dump-fop 'lisp::fop-list-7 file))
      (8 (dump-fop 'lisp::fop-list-8 file))
      (T (cond ((< n 256)
		(dump-fop 'lisp::fop-list file)
		(dump-byte n file))
	       (t (dump-fop 'lisp::fop-list file)
		  (dump-byte 255 file)
		  (do ((nn (- n 255) (- nn 255)))
		      ((< nn 256)
		       (dump-fop 'lisp::fop-list* file)
		       (dump-byte nn file))
		    (dump-fop 'lisp::fop-list* file)
		    (dump-byte 255 file)))))))

;;;; Array dumping:

;;; Named G-vectors get their subtype field set at load time.

(defun dump-vector (obj file)
  (cond ((and (simple-vector-p obj)
	      (= (%primitive get-vector-subtype obj)
		 %g-vector-structure-subtype))
	 (normal-dump-vector obj file)
	 (dump-fop 'lisp::fop-structure file))
	(t
	 (normal-dump-vector obj file))))

(defun normal-dump-vector (v file)
  (note-potential-circularity v file)
  (do ((index 0 (1+ index))
       (length (length v))
       (circ (fasl-file-circularity-table file)))
      ((= index length)
       (dump-fop* length lisp::fop-small-vector lisp::fop-vector file))
    (let* ((obj (aref v index))
	   (ref (gethash obj circ)))
      (cond (ref
	     (push (make-circularity :type :svset  :object v  :index index
				     :value obj  :enclosing-object ref)
		   *circularities-detected*)
	     (sub-dump-object nil file))
	    (t
	     (sub-dump-object obj file))))))

;;; Dump a string.

(defun dump-string (s file)
  (let ((length (length s)))
    (dump-fop* length lisp::fop-small-string lisp::fop-string file)
    (dotimes (i length)
      (dump-byte (char-code (char s i)) file))))

;;; Dump-Array  --  Internal
;;;
;;;    Dump a multi-dimensional array.  Someday when we figure out what
;;; a displaced array looks like, we can fix this.
;;;
(defun dump-array (array file)
  (unless (zerop (%primitive header-ref array %array-displacement-slot))
    (compiler-error
     "Attempt to dump an array with a displacement, you lose big."))
  (let ((rank (array-rank array)))
    (dotimes (i rank)
      (dump-integer (array-dimension array i) file))
    (sub-dump-object (%primitive header-ref array %array-data-slot) file)
    (dump-fop 'lisp::fop-array file)
    (quick-dump-number rank 4 file)))


;;; DUMP-I-VECTOR  --  Internal
;;;
;;; *** NOT *** the FOP-INT-VECTOR as currently documented in rtguts.  Size
;;; must be a directly supported I-vector element size, with no extra bits.
;;;
;;; If a byte vector, or if the native and target byte orderings are the same,
;;; then just write the bits.  Otherwise, dispatch off of the target byte order
;;; and write the vector one element at a time.
;;;
(defun dump-i-vector (vec file)
  (let* ((vec (if #+new-compiler (array-header-p vec)
		  #-new-compiler (%primitive complex-array-p vec)
		  (coerce vec 'simple-array)
		  vec))
	 (ac (%primitive get-vector-access-code vec))
	 (len (length vec))
	 (size (ash 1 ac))
	 (bytes (ash (+ (ash len ac) 7) -3)))
		    
    (dump-fop 'lisp::fop-int-vector file)
    (quick-dump-number len 4 file)
    (dump-byte size file)
    (cond ((or (eq target-byte-order native-byte-order)
	       (= size 8))
	   (dotimes (i bytes)
	     (dump-byte (%primitive typed-vref 3 vec i) file)))
	  ((> size 8)
	   (ecase target-byte-order
	     (:little-endian
	      (dotimes (i len)
		(let ((int (aref vec i)))
		  (quick-dump-number int (ash size -3) file))))
	     (:big-endian
	      (dotimes (i len)
		(let ((int (aref vec i)))
		  (do ((shift (- 8 size) (+ shift 8)))
		      ((plusp shift))
		    (dump-byte (logand (ash int shift) #xFF) file)))))))
	  (t
	   (macrolet ((frob (initial step done)
			`(let ((shift ,initial)
			       (byte 0))
			   (dotimes (i len)
			     (let ((int (aref vec i)))
			       (setq byte (logior byte (ash int shift)))
			       (,step shift size))
			     (when ,done
			       (dump-byte byte file)
			       (setq shift ,initial  byte 0)))
			   (unless (= shift ,initial) (dump-byte byte file)))))
	   (ecase target-byte-order
	     (:little-endian
	      (frob 0 incf (= shift 8)))
	     (:big-endian
	      (let ((initial-shift (- 8 size)))
		(frob initial-shift decf (minusp shift))))))))))


;;; Dump a character.

(defun dump-character (ch file)
  (cond
   ((string-char-p ch)
    (dump-fop 'lisp::fop-short-character file)
    (dump-byte (char-code ch) file))
   (t
    (dump-fop 'lisp::fop-character file)
    (dump-byte (char-code ch) file)
    (dump-byte (char-bits ch) file)
    (dump-byte (char-font ch) file))))
