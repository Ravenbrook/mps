;;; -*- Log: code.log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/debug-info.lisp,v 1.27 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains structures used for recording debugger information.
;;;
(in-package "C")

(export '(make-sc-offset sc-offset-scn sc-offset-offset
	  read-var-integer write-var-integer
	  read-var-string write-var-string
	  read-packed-bit-vector write-packed-bit-vector))


;;;; SC-Offsets:
;;;
;;;    We represent the place where some value is stored with a SC-OFFSET,
;;; which is the SC number and offset encoded as an integer.

(defconstant sc-offset-scn-byte (byte 5 0))
(defconstant sc-offset-offset-byte (byte 22 5))
(deftype sc-offset () '(unsigned-byte 27))

(defmacro make-sc-offset (scn offset)
  `(dpb ,scn sc-offset-scn-byte
	(dpb ,offset sc-offset-offset-byte 0)))

(defmacro sc-offset-scn (sco) `(ldb sc-offset-scn-byte ,sco))
(defmacro sc-offset-offset (sco) `(ldb sc-offset-offset-byte ,sco))


;;;; Variable length integers:
;;;
;;;    The debug info representation makes extensive use of integers encoded in
;;; a byte vector using a variable number of bytes:
;;;    0..253 => the integer
;;;    254 => read next two bytes for integer
;;;    255 => read next four bytes for integer

;;; READ-VAR-INTEGER  --  Interface
;;;
;;;    Given a byte vector Vec and an index variable Index, read a variable
;;; length integer and advance index.
;;;
(defmacro read-var-integer (vec index)
  (once-only ((val `(aref ,vec ,index)))
    `(cond ((<= ,val 253)
	    (incf ,index)
	    ,val)
	   ((= ,val 254)
	    (prog1
		(logior (aref ,vec (+ ,index 1))
			(ash (aref ,vec (+ ,index 2)) 8))
	      (incf ,index 3)))
	   (t
	    (prog1
		(logior (aref ,vec (+ ,index 1))
			(ash (aref ,vec (+ ,index 2)) 8)
	      		(ash (aref ,vec (+ ,index 3)) 16)
	      		(ash (aref ,vec (+ ,index 4)) 24))
	      (incf ,index 5))))))


;;; WRITE-VAR-INTEGER  --  Interface
;;;
;;;    Takes an adjustable vector Vec with a fill pointer and pushes the
;;; variable length representation of Int on the end.
;;;
(defun write-var-integer (int vec)
  (declare (type (unsigned-byte 32) int))
  (cond ((<= int 253)
	 (vector-push-extend int vec))
	(t
	 (let ((32-p (> int #xFFFF)))
	   (vector-push-extend (if 32-p 255 254) vec)
	   (vector-push-extend (ldb (byte 8 0) int) vec)
	   (vector-push-extend (ldb (byte 8 8) int) vec)
	   (when 32-p
	     (vector-push-extend (ldb (byte 8 16) int) vec)
	     (vector-push-extend (ldb (byte 8 24) int) vec)))))
  (undefined-value))



;;;; Packed strings:
;;;
;;;    A packed string is a variable length integer length followed by the
;;; character codes.


;;; READ-VAR-STRING  --  Interface
;;;
;;;    Read a packed string from Vec starting at Index, leaving advancing
;;; Index.
;;;
(defmacro read-var-string (vec index)
  (once-only ((len `(read-var-integer ,vec ,index)))
    (once-only ((res `(make-string ,len)))
      `(progn
	 (%primitive byte-blt ,vec ,index ,res 0 ,len)
	 (incf ,index ,len)
	 ,res))))


;;; WRITE-VAR-STRING  --  Interface
;;;
;;;    Write String into Vec (adjustable, fill-pointer) represented as the
;;; length (in a var-length integer) followed by the codes of the characters.
;;;
(defun write-var-string (string vec)
  (declare (simple-string string))
  (let ((len (length string)))
    (write-var-integer len vec)
    (dotimes (i len)
      (vector-push-extend (char-code (schar string i)) vec)))
  (undefined-value))


;;;; Packed bit vectors:
;;;

;;; READ-PACKED-BIT-VECTOR  --  Interface
;;;
;;;    Read the specified number of Bytes out of Vec at Index and convert them
;;; to a bit-vector.  Index is incremented.
;;;
(defmacro read-packed-bit-vector (bytes vec index)
  (once-only ((n-bytes bytes))
    (once-only ((n-res `(make-array (* ,n-bytes 8) :element-type 'bit)))
      `(progn
	 (%primitive byte-blt ,vec ,index ,n-res 0 ,n-bytes)
	 (incf ,index ,n-bytes)
	 ,n-res))))


;;; WRITE-PACKED-BIT-VECTOR  --  Interface
;;;
;;;    Write Bits out to Vec.  Bits must be an eight-bit multiple.
;;;
(defun write-packed-bit-vector (bits vec)
  (declare (type simple-bit-vector bits))
  (let ((len (length bits))
	(start (fill-pointer vec)))
    (cond ((eq (backend-byte-order *backend*)
	       (backend-byte-order *native-backend*))
	   (let ((bytes (ash len -3)))
	     (dotimes (i bytes)
	       (vector-push-extend 0 vec))
	     (lisp::with-array-data ((data vec) (ig1) (ig2))
	       (declare (ignore ig1 ig2))
	       (%primitive byte-blt bits 0 data start (+ start bytes)))))
	  (t
	   (macrolet ((frob (initial step done)
			`(let ((shift ,initial)
			       (byte 0))
			   (dotimes (i len)
			     (let ((int (aref bits i)))
			       (setq byte (logior byte (ash int shift)))
			       (,step shift))
			     (when ,done
			       (vector-push-extend byte vec)
			       (setq shift ,initial  byte 0)))
			   (unless (= shift ,initial)
			     (vector-push-extend byte vec)))))
	     (ecase (backend-byte-order *backend*)
	       (:little-endian
		(frob 0 incf (= shift 8)))
	       (:big-endian
		(frob 7 decf (minusp shift))))))))
  
  (undefined-value))


;;;; Compiled debug variables:
;;;
;;;    Compiled debug variables are in a packed binary representation in the
;;; DEBUG-FUNCTION-VARIABLES:
;;;    single byte of boolean flags:
;;;        uninterned name
;;;	   packaged name
;;;        environment-live
;;;        has distinct save location
;;;        has ID (name not unique in this fun)
;;;        minimal debug-info argument (name generated as ARG-0, ...)
;;;        deleted: placeholder for unused minimal argument
;;;    [name length in bytes (as var-length integer), if not minimal]
;;;    [...name bytes..., if not minimal]
;;;    [if packaged, var-length integer that is package name length]
;;;     ...package name bytes...]
;;;    [If has ID, ID as var-length integer]
;;;    SC-Offset of primary location (as var-length integer)
;;;    [If has save SC, SC-Offset of save location (as var-length integer)]

(defconstant compiled-debug-variable-uninterned		#b00000001)
(defconstant compiled-debug-variable-packaged		#b00000010)
(defconstant compiled-debug-variable-environment-live	#b00000100)
(defconstant compiled-debug-variable-save-loc-p		#b00001000)
(defconstant compiled-debug-variable-id-p		#b00010000)
(defconstant compiled-debug-variable-minimal-p		#b00100000)
(defconstant compiled-debug-variable-deleted-p		#b01000000)


;;;; Compiled debug blocks:
;;;
;;;    Compiled debug blocks are in a packed binary representation in the
;;; DEBUG-FUNCTION-BLOCKS:
;;;    number of successors + bit flags (single byte)
;;;        elsewhere-p
;;;    ...ordinal number of each successor in the function's blocks vector...
;;;    number of locations in this block
;;;    kind of first location (single byte)
;;;    delta from previous PC (or from 0 if first location in function.)
;;;    [offset of first top-level form, if no function TLF-NUMBER]
;;;    form number of first source form
;;;    first live mask (length in bytes determined by number of VARIABLES)
;;;    ...more <kind, delta, top-level form offset, form-number, live-set>
;;;       tuples...


(defconstant compiled-debug-block-nsucc-byte (byte 2 0))
(defconstant compiled-debug-block-elsewhere-p #b00000100)

(defconstant compiled-code-location-kind-byte (byte 3 0))
(defconstant compiled-code-location-kinds
  '#(:unknown-return :known-return :internal-error :non-local-exit
     :block-start :call-site :single-value-return :non-local-entry))



;;;; Debug function:

(defstruct debug-function)

(defstruct (compiled-debug-function
	    (:include debug-function)
	    (:pure t))
  ;;
  ;; The name of this function.  If from a DEFUN, etc., then this is the
  ;; function name, otherwise it is a descriptive string.
  (name (required-argument) :type (or simple-string cons symbol))
  ;;
  ;; The kind of function (same as FUNCTIONAL-KIND):
  (kind nil :type (member nil :optional :external :top-level :cleanup))
  ;;
  ;; A vector of the packed binary representation of variable locations in this
  ;; function.  These are in alphabetical order by name.  This ordering is used
  ;; in lifetime info to refer to variables: the first entry is 0, the second
  ;; entry is 1, etc.  Variable numbers are *not* the byte index at which the
  ;; representation of the location starts.  This slot may be NIL to save
  ;; space.
  (variables nil :type (or (simple-array (unsigned-byte 8) (*)) null))
  ;;
  ;; A vector of the packed binary representation of the COMPILED-DEBUG-BLOCKS
  ;; in this function, in the order that the blocks were emitted.  The first
  ;; block is the start of the function.  This slot may be NIL to save space.
  (blocks nil :type (or (simple-array (unsigned-byte 8) (*)) null))
  ;;
  ;; If all code locations in this function are in the same top-level form,
  ;; then this is the number of that form, otherwise NIL.  If NIL, then each
  ;; code location represented in the BLOCKS specifies the TLF number.
  (tlf-number nil :type (or index null))
  ;;
  ;; A vector describing the variables that the argument values are stored in
  ;; within this function.  The locations are represented by the ordinal number
  ;; of the entry in the VARIABLES.  The locations are in the order that the
  ;; arguments are actually passed in, but special marker symbols can be
  ;; interspersed to indicate the orignal call syntax:
  ;;
  ;; DELETED
  ;;    There was an argument to the function in this position, but it was
  ;;    deleted due to lack of references.  The value cannot be recovered.
  ;;
  ;; SUPPLIED-P
  ;;    The following location is the supplied-p value for the preceding
  ;;    keyword or optional.
  ;;
  ;; OPTIONAL-ARGS
  ;;    Indicates that following unqualified args are optionals, not required.
  ;;
  ;; REST-ARG
  ;;    The following location holds the list of rest args.
  ;;
  ;; MORE-ARG
  ;;    The following two locations are the more arg context and count.
  ;;
  ;; <any other symbol>
  ;;    The following location is the value of the keyword argument with the
  ;;    specified name.
  ;;
  ;; This may be NIL to save space.  If no symbols are present, then this will
  ;; be represented with an I-vector with sufficiently large element type.  If
  ;; this is :MINIMAL, then this means that the VARIABLES are all required
  ;; arguments, and are in the order they appear in the VARIABLES vector.  In
  ;; other words, :MINIMAL stands in for a vector where every element holds its
  ;; index.
  (arguments nil :type (or (simple-array * (*)) (member :minimal nil)))
  ;;
  ;; There are three alternatives for this slot:
  ;; 
  ;; A vector
  ;;    A vector of SC-OFFSETS describing the return locations.  The
  ;;    vector element type is chosen to hold the largest element.
  ;;
  ;; :Standard 
  ;;    The function returns using the standard unknown-values convention.
  ;;
  ;; :Fixed
  ;;    The function returns using the a fixed-values convention, but we
  ;;    elected not to store a vector to save space.
  (returns :fixed :type (or (simple-array * (*)) (member :standard :fixed)))
  ;;
  ;; SC-Offsets describing where the return PC and return FP are kept.
  (return-pc (required-argument) :type sc-offset)
  (old-fp (required-argument) :type sc-offset)
  ;;
  ;; SC-Offset for the number stack FP in this function, or NIL if no NFP
  ;; allocated.
  (nfp nil :type (or sc-offset null))
  ;;
  ;; The earliest PC in this function at which the environment is properly
  ;; initialized (arguments moved from passing locations, etc.)
  (start-pc (required-argument) :type index)
  ;;
  ;; The start of elsewhere code for this function (if any.)
  (elsewhere-pc (required-argument) :type index))


;;;; Minimal debug function:

;;; The minimal debug info format compactly represents debug-info for some
;;; cases where the other debug info (variables, blocks) is small enough so
;;; that the per-function overhead becomes relatively large.  The minimal
;;; debug-info format can represent any function at level 0, and any fixed-arg
;;; function at level 1.
;;;
;;; In the minimal format, the debug functions and function map are packed into
;;; a single byte-vector which is placed in the
;;; COMPILED-DEBUG-INFO-FUNCTION-MAP.  Because of this, all functions in a
;;; component must be representable in minimal format for any function to
;;; actually be dumped in minimal format.  The vector is a sequence of records
;;; in this format:
;;;    name representation + kind + return convention (single byte)
;;;    bit flags (single byte)
;;;        setf, nfp, variables
;;;    [package name length (as var-length int), if name is packaged]
;;;    [...package name bytes, if name is packaged]
;;;    [name length (as var-length int), if there is a name]
;;;    [...name bytes, if there is a name]
;;;    [variables length (as var-length int), if variables flag]
;;;    [...bytes holding variable descriptions]
;;;        If variables are dumped (level 1), then the variables are all
;;;        arguments (in order) with the minimal-arg bit set.
;;;    [If returns is specified, then the number of return values]
;;;    [...sequence of var-length ints holding sc-offsets of the return
;;;        value locations, if fixed return values are specified.]
;;;    return-pc location sc-offset (as var-length int)
;;;    old-fp location sc-offset (as var-length int)
;;;    [nfp location sc-offset (as var-length int), if nfp flag]
;;;    code-start-pc (as a var-length int)
;;;        This field implicitly encodes start of this function's code in the
;;;        function map, as a delta from the previous function's code start.
;;;        If the first function in the component, then this is the delta from
;;;        0 (i.e. the absolute offset.)
;;;    start-pc (as a var-length int)
;;;        This encodes the environment start PC as an offset from the
;;;        code-start PC.
;;;    elsewhere-pc
;;;        This encodes the elsewhere code start for this function, as a delta
;;;        from the previous function's elsewhere code start. (i.e. the
;;;        encoding is the same as for code-start-pc.)
;;;    
;;;    

#|
### For functions with XEPs, name could be represented more simply and
compactly as some sort of info about with how to find the function-entry that
this is a function for.  Actually, you really hardly need any info.  You can
just chain through the functions in the component until you find the right one.
Well, I guess you need to at least know which function is an XEP for the real
function (which would be useful info anyway).
|#

;;; Following are definitions of bit-fields in the first byte of the minimal
;;; debug function:
;;;
(defconstant minimal-debug-function-name-symbol 0)
(defconstant minimal-debug-function-name-packaged 1)
(defconstant minimal-debug-function-name-uninterned 2)
(defconstant minimal-debug-function-name-component 3)
(defconstant minimal-debug-function-name-style-byte (byte 2 0))
(defconstant minimal-debug-function-kind-byte (byte 3 2))
(defconstant minimal-debug-function-kinds
  '#(nil :optional :external :top-level :cleanup))
(defconstant minimal-debug-function-returns-standard 0)
(defconstant minimal-debug-function-returns-specified 1)
(defconstant minimal-debug-function-returns-fixed 2)
(defconstant minimal-debug-function-returns-byte (byte 2 5))

;;; The following are bit-flags in the second byte of the minimal debug
;;; function:

;;; If true, wrap (SETF ...) around the name.
(defconstant minimal-debug-function-setf-bit (ash 1 0))

;;; If true, there is a NFP.
(defconstant minimal-debug-function-nfp-bit (ash 1 1))

;;; If true, variables (hence arguments) have been dumped.
(defconstant minimal-debug-function-variables-bit (ash 1 2))


;;;; Debug source:

(defstruct (debug-source (:pure t))
  ;;
  ;; This slot indicates where the definition came from:
  ;;    :File - from a file (Compile-File)
  ;;    :Lisp - from Lisp (Compile)
  ;;  :Stream - from a non-file stream (Compile-From-Stream)
  (from (required-argument) :type (member :file :stream :lisp))
  ;;
  ;; If :File, the file name, if :Lisp or :Stream, then a vector of the
  ;; top-level forms.  When from COMPILE, form 0 is #'(LAMBDA ...).
  (name nil)
  ;;
  ;; File comment for this file, if any.
  (comment nil :type (or simple-string null))
  ;;
  ;; The universal time that the source was written, or NIL if unavailable.
  (created nil :type (or unsigned-byte null))
  ;;
  ;; The universal time that the source was compiled.
  (compiled (required-argument) :type unsigned-byte)
  ;;
  ;; The source path root number of the first form read from this source (i.e.
  ;; the total number of forms converted previously in this compilation.)
  (source-root 0 :type index)
  ;;
  ;; The file-positions of each truly top-level form read from this file (if
  ;; applicable).  The vector element type will be chosen to hold the largest
  ;; element.  May be null to save space.
  (start-positions nil :type (or (simple-array * (*)) null))
  ;;
  ;; If from :LISP, this is the function whose source is form 0.
  ;; If from :STREAM, this is whatever was the :SOURCE-INFO argument to
  ;; COMPILE-FROM-STREAM.
  (info nil))


;;;; The DEBUG-INFO structure:

(defstruct debug-info
  ;;
  ;; Some string describing something about the code in this component.
  (name (required-argument) :type simple-string)
  ;;
  ;; A list of DEBUG-SOURCE structures describing where the code for this
  ;; component came from, in the order that they were read.
  ;;
  ;; *** NOTE: the offset of this slot is wired into the fasl dumper so that it
  ;; *** can backpatch the source info when compilation is complete.
  (source nil :type list))


(defstruct (compiled-debug-info
	    (:include debug-info)
	    (:pure t))
  ;;
  ;; The name of the package that DEBUG-FUNCTION-VARIABLES were dumped relative
  ;; to.  Locations that aren't packaged are in this package.
  (package (required-argument) :type simple-string)
  ;;
  ;; Either a simple-vector or a byte-vector holding the debug functions for
  ;; this component.  This is used to map PCs to functions, so that we can
  ;; figure out what function we were running in.  If a byte-vector, then it is
  ;; a sequence of minimal debug functions in a packed binary representation.
  ;;
  ;; If a simple-vector, then it alternates Debug-Function structures and
  ;; fixnum PCs.  The function is valid between the PC before it (inclusive)
  ;; and the PC after it (exclusive).  The PCs are in sorted order, so we can
  ;; binary-search.  We omit the first and last PC, since their values are 0
  ;; and the length of the code vector.  Null only temporarily.
  (function-map nil :type (or simple-vector
			      (simple-array (unsigned-byte 8) (*))
			      null)))
