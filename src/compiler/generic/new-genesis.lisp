;;; -*- Package: Lisp -*-
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/new-genesis.lisp,v 1.79 2006/06/30 18:41:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Core image builder for CMU Common Lisp.
;;;
;;; Written by Skef Wholey.  Package hackery courtesy of Rob MacLachlan.
;;;
;;; Completely Rewritten by William Lott for MIPS port.
;;;

(in-package "LISP")


;;;; Representation of descriptors and spaces in the core.

(defvar *dynamic* nil)
(defparameter dynamic-space-id 1)

(defvar *static* nil)
(defparameter static-space-id 2)

(defvar *read-only* nil)
(defparameter read-only-space-id 3)

(defvar *foreign-linkage* nil)
(defparameter foreign-linkage-space-id 4)

(defmacro round-up (num size)
  "Rounds number up to be an integral multiple of size."
  (let ((size-var (gensym)))
    `(let ((,size-var ,size))
       (* ,size-var (ceiling ,num ,size-var)))))


(defstruct (space
	    (:constructor %make-space (name identifier address sap
					    words-allocated))
	    (:print-function %print-space))
  name			      ; Name of this space.
  identifier		      ; Space Identifier
  address		      ; Word address it will be at when loaded.
  sap			      ; System area pointer for this space.
  words-allocated	      ; Number of words currently allocated.
  (free-pointer 0))	      ; Word offset of next free word.

(defun %print-space (space stream depth)
  (declare (ignore depth))
  (format stream "#<~S space (#x~X), ~S bytes used>"
	  (space-name space)
	  (ash (space-address space) 2)
	  (ash (space-free-pointer space) 2)))

(eval-when (compile eval load)

(defvar *nil-descriptor* nil
  "Handle on Nil.")

(defconstant descriptor-low-bits 16
  "Number of bits in the low half of the descriptor")

(defconstant space-alignment (ash 1 descriptor-low-bits)
  "Alignment requirement for spaces in the target.
  Must be at least (ash 1 descriptor-low-bits")

); eval-when

(defstruct (descriptor
	    (:constructor make-descriptor (high low &optional space offset))
	    (:print-function %print-descriptor))
  space			      ; The space this descriptor is allocated in.
  offset		      ; The offset (in words) from the start of
			      ;  that space.
  high			      ; The high part of the descriptor.
  low			      ; The low part of the descriptor.
  )

(defun %print-descriptor (des stream depth)
  (declare (ignore depth))
  (let ((lowtag (descriptor-lowtag des)))
    (cond ((or (= lowtag vm:even-fixnum-type) (= lowtag vm:odd-fixnum-type))
	   (let ((unsigned
		  (logior (ash (descriptor-high des)
			       (1+ (- descriptor-low-bits vm:lowtag-bits)))
			  (ash (descriptor-low des) (- 1 vm:lowtag-bits)))))
	     (if (> unsigned (1- (ash 1 (- vm:word-bits 3))))
		 ;; negative fixnum
		 (format stream "#<fixnum: ~D>"
			 (- unsigned (ash 1 (- vm:word-bits 2))))
		 ;; positive fixnum
		 (format stream "#<fixnum: ~D>" unsigned))))
	  ((or (= lowtag vm:other-immediate-0-type)
	       (= lowtag vm:other-immediate-1-type))
	   (format stream "#<other immediate: #x~X, type #b~8,'0B>"
		   (logior (ash (descriptor-high des)
				(- descriptor-low-bits vm:type-bits))
			   (ash (descriptor-low des)
				(- vm:type-bits)))
		   (logand (descriptor-low des) vm:type-mask)))
	  ((and (= lowtag vm:list-pointer-type)
		(= (descriptor-bits des) (descriptor-bits *nil-descriptor*)))
	   (format stream "NIL"))
	  (t
	   (format stream
		   (ecase lowtag
		     (#.vm:function-pointer-type "#<function-pointer")
		     (#.vm:list-pointer-type "#<list-pointer")
		     (#.vm:other-pointer-type "#<other-pointer")
		     (#.vm:instance-pointer-type "#<instance-pointer")))
	   (format stream ": #x~X, ~A space>"
		   (logior (ash (descriptor-high des)
				descriptor-low-bits)
			   (logandc2 (descriptor-low des) vm:lowtag-mask))
		   (let ((space (descriptor-space des)))
		     (if space
			 (space-name space)
			 "unknown")))))))


(defun make-space (name identifier address
			&optional (initial-size space-alignment))
  (multiple-value-bind
      (ignore remainder)
      (truncate address space-alignment)
    (declare (ignore ignore))
    (unless (zerop remainder)
      (error "The address #x~X is not aligned on a #x~X boundary."
	     address space-alignment)))
  (let* ((actual-size (round-up initial-size
			       (c:backend-page-size c:*backend*)))
	 (addr (allocate-system-memory actual-size)))
    (%make-space name identifier
		 (ash address (- vm:word-shift)) addr
		 (ash actual-size (- vm:word-shift)))))

(defun deallocate-space (space)
  (deallocate-system-memory (space-sap space)
			    (* (space-words-allocated space) vm:word-bytes)))

(defun allocate-descriptor (space length lowtag)
  "Return a descriptor for a block of LENGTH bytes out of SPACE.  The free
  pointer is boosted as necessary.  If any additional memory is needed, we
  vm_allocate it.  The descriptor returned is a pointer of type LOWTAG."
  (let* ((bytes (round-up length #+amd64 16 #-amd64 (ash 1 vm:lowtag-bits)))
	 (offset (space-free-pointer space))
	 (new-free-ptr (+ offset (ash bytes (- vm:word-shift)))))
    (when (> new-free-ptr (space-words-allocated space))
      (do ((size (space-words-allocated space) (* 2 size)))
	  ((>= size new-free-ptr)
	   (setf (space-sap space)
		 (reallocate-system-memory (space-sap space)
					   (ash (space-words-allocated space)
						vm:word-shift)
					   (ash size vm:word-shift)))
	   (setf (space-words-allocated space) size))))
    (setf (space-free-pointer space) new-free-ptr)
    (let ((ptr (+ (space-address space) offset)))
      (make-descriptor (ash ptr (- vm:word-shift descriptor-low-bits))
		       (logior (ash (logand ptr
					    (1- (ash 1
						     (- descriptor-low-bits
							vm:word-shift))))
				    vm:word-shift)
			       lowtag)
		       space
		       offset))))

(defun descriptor-lowtag (des)
  "Return the lowtag bits for DES."
  (logand (descriptor-low des) vm:lowtag-mask))

(declaim (inline descriptor-bits))
(defun descriptor-bits (des)
  "Return the bits of DES."
  (logior (ash (descriptor-high des) descriptor-low-bits)
	  (descriptor-low des)))

#+nil; unused
(defun descriptor-eq (x y)
  (and (eql (descriptor-high x) (descriptor-high y))
       (eql (descriptor-low x) (descriptor-low y))))

(defun descriptor-fixnum (x)
  (let ((bits (descriptor-bits x)))
    (if (logbitp (1- vm:word-bits) bits)
	(logior (ash bits -2) (ash -1 (1- vm:word-bits)))
	(ash bits -2))))

(defun descriptor-sap (des)
  "Return a SAP pointing to the piece of memory DES refers to.  The lowtag
  bits of DES are ignored."
  (let ((space (descriptor-space des)))
    (when (null space)
      (let ((lowtag (descriptor-lowtag des))
	    (high (descriptor-high des))
	    (low (descriptor-low des)))
	(when (or (eql lowtag vm:function-pointer-type)
		  (eql lowtag vm:instance-pointer-type)
		  (eql lowtag vm:list-pointer-type)
		  (eql lowtag vm:other-pointer-type))
	  (dolist (space (list *dynamic* *static* *read-only*)
			 (error "Could not find a space for ~S" des))
	    ;; This code relies on the fact that spaces are aligned such that
	    ;; the descriptor-low-bits low bits are zero.
	    (when (and (>= high (ash (space-address space)
				     (- vm:word-shift descriptor-low-bits)))
		       (<= high (ash (+ (space-address space)
					(space-free-pointer space))
				     (- vm:word-shift descriptor-low-bits))))
	      (setf (descriptor-space des) space)
	      (setf (descriptor-offset des)
		    (+ (ash (- high (ash (space-address space)
					 (- vm:word-shift descriptor-low-bits)))
			    (- descriptor-low-bits vm:word-shift))
		       (ash (logandc2 low vm:lowtag-mask) (- vm:word-shift))))
	      (return)))))
      (setf space (descriptor-space des)))
    (unless space
      (error "~S has no space?" des))
    (int-sap (+ (sap-int (space-sap space))
		(ash (descriptor-offset des) vm:word-shift)))))


(defun make-random-descriptor (value)
  (make-descriptor (logand (ash value (- descriptor-low-bits))
			   (1- (ash 1 (- vm:word-bits descriptor-low-bits))))
		   (logand value (1- (ash 1 descriptor-low-bits)))))

(defun make-fixnum-descriptor (num)
  (when (>= (integer-length num)
	    (1+ (- vm:word-bits vm:lowtag-bits)))
    (error "~D is too big for a fixnum." num))
  (make-random-descriptor (ash num (1- vm:lowtag-bits))))

(defun make-other-immediate-descriptor (data type)
  (make-descriptor (ash data (- vm:type-bits descriptor-low-bits))
		   (logior (logand (ash data (- descriptor-low-bits
						vm:type-bits))
				   (1- (ash 1 descriptor-low-bits)))
			   type)))

(defun make-character-descriptor (data)
  (make-other-immediate-descriptor data vm:base-char-type))

(defun descriptor-beyond (des offset type)
  (let* ((low (logior (+ (logandc2 (descriptor-low des) vm:lowtag-mask)
			 offset)
		      type))
	 (high (+ (descriptor-high des)
		  (ash low (- descriptor-low-bits)))))
    (make-descriptor high (logand low (1- (ash 1 descriptor-low-bits))))))


(defun initialize-spaces ()
  (macrolet ((frob (sym name identifier addr)
	       `(if ,sym
		    (setf (space-free-pointer ,sym) 0)
		    (setf ,sym
			  (make-space ,name ,identifier ,addr)))))
    (frob *read-only* :read-only read-only-space-id
      vm:target-read-only-space-start)
    (frob *static* :static static-space-id
      vm:target-static-space-start)
    (frob *dynamic* :dynamic dynamic-space-id
      vm:target-dynamic-space-start)))


;;;; Random variables and other noise.

(defparameter unbound-marker
  (make-other-immediate-descriptor 0 vm:unbound-marker-type)
  "Handle on the trap object.")

(defvar *current-init-functions-cons* nil
  "Head of list of functions to be called when the Lisp starts up.")

(defvar *in-cold-load* nil
  "Used by normal loader.")



;;;; Stuff to read and write the core memory.

;; TODO: make this work for 64-bit
(defun maybe-byte-swap (word)
  (if (eq (c:backend-byte-order c:*native-backend*)
	  (c:backend-byte-order c:*backend*))
      word
      (locally (declare (type (unsigned-byte 32) word))
	(assert (= vm:word-bits 32))
	(assert (= vm:byte-bits 8))
	(logior (ash (ldb (byte 8 0) word) 24)
		(ash (ldb (byte 8 8) word) 16)
		(ash (ldb (byte 8 16) word) 8)
		(ldb (byte 8 24) word)))))

(defun maybe-byte-swap-short (short)
  (if (eq (c:backend-byte-order c:*native-backend*)
	  (c:backend-byte-order c:*backend*))
      short
      (locally (declare (type (unsigned-byte 16) short))
	(assert (= vm:word-bits 32))
	(assert (= vm:byte-bits 8))
	(logior (ash (ldb (byte 8 0) short) 8)
		(ldb (byte 8 8) short)))))
  

(defun write-indexed (address index value)
  "Write VALUE (a descriptor) INDEX words from ADDRESS (also a descriptor)."
  (if (and (null (descriptor-space value))
	   (not (null (descriptor-offset value))))
      (note-load-time-value-reference
       (int-sap (+ (logandc2 (descriptor-bits address) vm:lowtag-mask)
		   (ash index vm:word-shift)))
       value)
      (let ((sap (descriptor-sap address))
	    (high (descriptor-high value))
	    (low (descriptor-low value)))
	(ecase vm:word-bits
	  (32
	   (setf (sap-ref-32 sap (ash index vm:word-shift))
		 (maybe-byte-swap (logior (ash high descriptor-low-bits) low))))
	  (64
	   (setf (sap-ref-64 sap (ash index vm:word-shift))
		 (maybe-byte-swap (logior (ash high descriptor-low-bits) low))))))))

(defun write-memory (address value)
  "Write VALUE (a descriptor) at ADDRESS (also a descriptor)."
  (write-indexed address 0 value))


(defun read-indexed (address index)
  "Return the value (as a descriptor) INDEX words from ADDRESS (a descriptor)."
  (let* ((sap (descriptor-sap address))
	 (value (maybe-byte-swap
		 (ecase vm:word-bits
		   (32 (sap-ref-32 sap (ash index vm:word-shift)))
		   (64 (sap-ref-64 sap (ash index vm:word-shift)))))))
    (make-random-descriptor value)))

(defun read-memory (address)
  "Return the value at ADDRESS (a descriptor)."
  (read-indexed address 0))


;;;; Allocating primitive objects.

;;; There are three kinds of blocks of memory in the new type system:
;;;
;;;   Boxed objects (cons cells, structures, etc):
;;; These objects have no header as all slots are descriptors.
;;;
;;;   Unboxed objects (bignums):
;;; A single header words that contains the length.
;;;
;;;   Vector objects:
;;; A header word with the type, a word for the length, plus the data.
;;;

(defun allocate-boxed-object (space length lowtag)
  "Allocate LENGTH words in SPACE and return a new descriptor of type LOWTAG
  pointing to them."
  (allocate-descriptor space (ash length vm:word-shift) lowtag))

(defun allocate-unboxed-object (space element-size length type)
  "Allocate LENGTH units of ELEMENT-SIZE bits plus a header word in SPACE and
  return an ``other-pointer'' descriptor to them.  Initialize the header word
  with the resultant length and TYPE."
  (let* ((bytes (/ (* element-size length) vm:byte-bits))
	 (des (allocate-descriptor space
				   (+ bytes vm:word-bytes)
				   vm:other-pointer-type)))
    (write-memory des
		  (make-other-immediate-descriptor (ash bytes (- vm:word-shift))
						   type))
    des))

(defun allocate-vector-object (space element-size length type)
  "Allocate LENGTH units of ELEMENT-SIZE plus a header plus a length slot in
  SPACE and return an ``other-pointer'' descriptor to them.  Initialize the
  header word with TYPE and the length slot with LENGTH."
  (let* ((bytes (/ (* element-size length) vm:byte-bits))
	 (des (allocate-descriptor space (+ bytes (* 2 vm:word-bytes))
				   vm:other-pointer-type)))
    (write-memory des (make-other-immediate-descriptor 0 type))
    (write-indexed des vm:vector-length-slot (make-fixnum-descriptor length))
    des))



;;;; Routines to move simple objects into the core.

(defun string-to-core (string &optional (space *dynamic*))
  "Copy string into the CORE and return a descriptor to it."
  ;; Note: We allocate an extra byte and tweek the length back to make sure
  ;; there will be a null at the end of the string to aid in call-out to
  ;; C.
  (let* ((len (length string))
	 (des (allocate-vector-object space vm:byte-bits (1+ len)
				      vm:simple-string-type)))
    (write-indexed des vm:vector-length-slot (make-fixnum-descriptor len))
    (copy-to-system-area string (* vm:vector-data-offset
				   ;; the word size of the native backend which
				   ;; may be different from the target backend
				   (if (= (c:backend-fasl-file-implementation
					   c::*native-backend*)
					  #.c:amd64-fasl-file-implementation)
				       64
				       32))
			 (descriptor-sap des)
			 (* vm:vector-data-offset vm:word-bits)
			 (* (1+ len) vm:byte-bits))
    des))

(defun bignum-to-core (n)
  "Copy the bignum to the core."
  (let* ((words (ceiling (1+ (integer-length n)) vm:word-bits))
	 (handle (allocate-unboxed-object *dynamic* vm:word-bits
					  words vm:bignum-type)))
    (declare (fixnum words))
    (do ((index 1 (1+ index))
	 (remainder n (ash remainder (- vm:word-bits))))
	((> index words)
	 (unless (zerop (integer-length remainder))
	   (warn "Wrote ~D words of ~D, but ~D was left over"
		 words n remainder)))
      (let ((word (ldb (byte vm:word-bits 0) remainder)))
	(write-indexed handle index
		       (make-descriptor (ash word (- descriptor-low-bits))
					(ldb (byte descriptor-low-bits 0)
					     word)))))
    handle))

(defun number-pair-to-core (first second type)
  "Makes a number pair of TYPE (ratio or complex) and fills it in."
  (let ((des (allocate-unboxed-object *dynamic* vm:word-bits 2 type)))
    (write-indexed des 1 first)
    (write-indexed des 2 second)
    des))

(defun float-to-core (num)
  (etypecase num
    (single-float
     (let ((des (allocate-unboxed-object *dynamic* vm:word-bits
					 (1- vm:single-float-size)
					 vm:single-float-type)))
       (write-indexed des vm:single-float-value-slot
		      (make-random-descriptor (single-float-bits num)))
       des))
    (double-float
     (let ((des (allocate-unboxed-object *dynamic* vm:word-bits
					 (1- vm:double-float-size)
					 vm:double-float-type))
	   (high-bits (make-random-descriptor (double-float-high-bits num)))
	   (low-bits (make-random-descriptor (double-float-low-bits num))))
       (ecase (c:backend-byte-order c:*backend*)
	 (:little-endian
	  (write-indexed des vm:double-float-value-slot low-bits)
	  (write-indexed des (1+ vm:double-float-value-slot) high-bits))
	 (:big-endian
	  (write-indexed des vm:double-float-value-slot high-bits)
	  (write-indexed des (1+ vm:double-float-value-slot) low-bits)))
       des))
    #+(and long-float x86)
    (long-float
     (let ((des (allocate-unboxed-object *dynamic* vm:word-bits
					 (1- vm:long-float-size)
					 vm:long-float-type))
	   (exp-bits (make-random-descriptor (long-float-exp-bits num)))
	   (high-bits (make-random-descriptor (long-float-high-bits num)))
	   (low-bits (make-random-descriptor (long-float-low-bits num))))
       (ecase (c:backend-byte-order c:*backend*)
	 (:little-endian
	  (write-indexed des vm:long-float-value-slot low-bits)
	  (write-indexed des (1+ vm:long-float-value-slot) high-bits)
	  (write-indexed des (+ 2 vm:long-float-value-slot) exp-bits))
	 (:big-endian
	  (error "Long-Float not supported")))
       des))
    #+double-double
    (double-double-float
     (let* ((des (allocate-unboxed-object *dynamic* vm:word-bits
					  (1- vm:double-double-float-size)
					  vm:double-double-float-type))
	    (hi (kernel:double-double-hi num))
	    (lo (kernel:double-double-lo num))
	    (hi-high-bits (make-random-descriptor (double-float-high-bits hi)))
	    (hi-low-bits (make-random-descriptor (double-float-low-bits hi)))
	    (lo-high-bits  (make-random-descriptor (double-float-high-bits lo)))
	    (lo-low-bits (make-random-descriptor (double-float-low-bits lo))))
       (ecase (c:backend-byte-order c:*backend*)
	 (:little-endian
	  (write-indexed des vm:double-double-float-lo-slot lo-low-bits)
	  (write-indexed des (+ 1 vm:double-double-float-lo-slot) lo-high-bits)
	  (write-indexed des vm:double-double-float-hi-slot hi-low-bits)
	  (write-indexed des (+ 1 vm:double-double-float-hi-slot) hi-high-bits))
	 (:big-endian
	  (write-indexed des vm:double-double-float-hi-slot hi-high-bits)
	  (write-indexed des (+ 1 vm:double-double-float-hi-slot) hi-low-bits)
	  (write-indexed des vm:double-double-float-lo-slot lo-high-bits)
	  (write-indexed des (1+ vm:double-double-float-lo-slot) lo-low-bits)))
       des))))

(defun complex-single-float-to-core (num)
  (declare (type (complex single-float) num))
  (let ((des (allocate-unboxed-object *dynamic* vm:word-bits
				      (1- vm:complex-single-float-size)
				      vm:complex-single-float-type)))
    (write-indexed des vm:complex-single-float-real-slot
		   (make-random-descriptor (single-float-bits (realpart num))))
    (write-indexed des vm:complex-single-float-imag-slot
		   (make-random-descriptor (single-float-bits (imagpart num))))
    des))

(defun complex-double-float-to-core (num)
  (declare (type (complex double-float) num))
  (let ((des (allocate-unboxed-object *dynamic* vm:word-bits
				      (1- vm:complex-double-float-size)
				      vm:complex-double-float-type)))
    (let* ((real (realpart num))
	   (high-bits (make-random-descriptor (double-float-high-bits real)))
	   (low-bits (make-random-descriptor (double-float-low-bits real))))
      (ecase (c:backend-byte-order c:*backend*)
	(:little-endian
	 (write-indexed des vm:complex-double-float-real-slot low-bits)
	 (write-indexed des (1+ vm:complex-double-float-real-slot) high-bits))
	(:big-endian
	 (write-indexed des vm:complex-double-float-real-slot high-bits)
	 (write-indexed des (1+ vm:complex-double-float-real-slot) low-bits))))
    (let* ((imag (imagpart num))
	   (high-bits (make-random-descriptor (double-float-high-bits imag)))
	   (low-bits (make-random-descriptor (double-float-low-bits imag))))
      (ecase (c:backend-byte-order c:*backend*)
	(:little-endian
	 (write-indexed des vm:complex-double-float-imag-slot low-bits)
	 (write-indexed des (1+ vm:complex-double-float-imag-slot) high-bits))
	(:big-endian
	 (write-indexed des vm:complex-double-float-imag-slot high-bits)
	 (write-indexed des (1+ vm:complex-double-float-imag-slot) low-bits))))
    des))

#+double-double
(defun complex-double-double-float-to-core (num)
  (declare (type (complex double-double-float) num))
  (let ((des (allocate-unboxed-object *dynamic* vm:word-bits
				      (1- vm::complex-double-double-float-size)
				      vm::complex-double-double-float-type)))
    (let* ((real (kernel:double-double-hi (realpart num)))
	   (high-bits (make-random-descriptor (double-float-high-bits real)))
	   (low-bits (make-random-descriptor (double-float-low-bits real))))
      (ecase (c:backend-byte-order c:*backend*)
	(:little-endian
	 (write-indexed des vm:complex-double-double-float-real-hi-slot low-bits)
	 (write-indexed des (1+ vm:complex-double-double-float-real-hi-slot) high-bits))
	(:big-endian
	 (write-indexed des vm:complex-double-double-float-real-hi-slot high-bits)
	 (write-indexed des (1+ vm:complex-double-double-float-real-hi-slot) low-bits))))
    (let* ((real (kernel:double-double-lo (realpart num)))
	   (high-bits (make-random-descriptor (double-float-high-bits real)))
	   (low-bits (make-random-descriptor (double-float-low-bits real))))
      (ecase (c:backend-byte-order c:*backend*)
	(:little-endian
	 (write-indexed des vm:complex-double-double-float-real-lo-slot low-bits)
	 (write-indexed des (1+ vm:complex-double-double-float-real-lo-slot) high-bits))
	(:big-endian
	 (write-indexed des vm:complex-double-double-float-real-lo-slot high-bits)
	 (write-indexed des (1+ vm:complex-double-double-float-real-lo-slot) low-bits))))
    (let* ((imag (kernel:double-double-hi (imagpart num)))
	   (high-bits (make-random-descriptor (double-float-high-bits imag)))
	   (low-bits (make-random-descriptor (double-float-low-bits imag))))
      (ecase (c:backend-byte-order c:*backend*)
	(:little-endian
	 (write-indexed des vm:complex-double-double-float-imag-hi-slot low-bits)
	 (write-indexed des (1+ vm:complex-double-double-float-imag-hi-slot) high-bits))
	(:big-endian
	 (write-indexed des vm:complex-double-double-float-imag-hi-slot high-bits)
	 (write-indexed des (1+ vm:complex-double-double-float-imag-hi-slot) low-bits))))
    (let* ((imag (kernel:double-double-lo (imagpart num)))
	   (high-bits (make-random-descriptor (double-float-high-bits imag)))
	   (low-bits (make-random-descriptor (double-float-low-bits imag))))
      (ecase (c:backend-byte-order c:*backend*)
	(:little-endian
	 (write-indexed des vm:complex-double-double-float-imag-lo-slot low-bits)
	 (write-indexed des (1+ vm:complex-double-double-float-imag-lo-slot) high-bits))
	(:big-endian
	 (write-indexed des vm:complex-double-double-float-imag-lo-slot high-bits)
	 (write-indexed des (1+ vm:complex-double-double-float-imag-lo-slot) low-bits))))
    des))
  

(defun number-to-core (number)
  "Copy the given number to the core, or flame out if we can't deal with it."
  (typecase number
    (integer (if (< (integer-length number) 30)
		 (make-fixnum-descriptor number)
		 (bignum-to-core number)))
    (ratio (number-pair-to-core (number-to-core (numerator number))
				(number-to-core (denominator number))
				vm:ratio-type))
    ((complex single-float) (complex-single-float-to-core number))
    ((complex double-float) (complex-double-float-to-core number))
    #+long-float
    ((complex long-float)
     (error "~S isn't a cold-loadable number at all!" number))
    #+double-double
    ((complex double-double-float) (complex-double-double-float-to-core number))
    (complex (number-pair-to-core (number-to-core (realpart number))
				  (number-to-core (imagpart number))
				  vm:complex-type))
    (float (float-to-core number))
    (t (error "~S isn't a cold-loadable number at all!" number))))

(defun sap-to-core (sap)
  (let ((des (allocate-unboxed-object *dynamic* vm:word-bits
				      (1- vm:sap-size) vm:sap-type)))
    (write-indexed des vm:sap-pointer-slot
		   (make-random-descriptor (sap-int sap)))
    des))

(defun allocate-cons (space car cdr)
  "Allocate a cons cell in SPACE and fill it in with CAR and CDR."
  (let ((dest (allocate-boxed-object space 2 vm:list-pointer-type)))
    (write-memory dest car)
    (write-indexed dest 1 cdr)
    dest))

(defmacro cold-push (thing list)
  "Generates code to push the THING onto the given cold load LIST."
  `(setq ,list (allocate-cons *dynamic* ,thing ,list)))

;;; COLD-VECTOR  --  Internal
;;;
;;;    Make a simple-vector that holds the specified Objects and return its
;;; descriptor.
;;;
(defun cold-vector (&rest objects)
  (let* ((size (length objects))
	 (result (allocate-vector-object *dynamic* vm:word-bits size
					 vm:simple-vector-type)))
    (dotimes (index size)
      (write-indexed result (+ index vm:vector-data-offset) (pop objects)))
    result))


;;;; Symbol magic.

;;; Allocate-Symbol allocates a symbol and fills its print name cell and
;;; property list cell.

(defvar *cold-symbol-allocation-space* nil)

(defun allocate-symbol (name)
  (declare (simple-string name))
  (let ((symbol (allocate-unboxed-object
		 (or *cold-symbol-allocation-space* *dynamic*)
		 vm:word-bits (1- vm:symbol-size) vm:symbol-header-type)))
    (write-indexed symbol vm:symbol-value-slot unbound-marker)
    (when (or (c:backend-featurep :x86)
	      (c:backend-featurep :amd64)
	      (c:backend-featurep :sparc)
	      (c:backend-featurep :ppc))
      (let ((offset #+(or x86 amd64 sparc ppc) vm:symbol-hash-slot
		    #-(or x86 amd64 sparc ppc) vm:symbol-unused-slot)
	    (value (sxhash name)))
	
      (write-indexed symbol offset (make-fixnum-descriptor value))))

    (write-indexed symbol vm:symbol-plist-slot *nil-descriptor*)
    (write-indexed symbol vm:symbol-name-slot (string-to-core name *dynamic*))
    (write-indexed symbol vm:symbol-package-slot *nil-descriptor*)
    symbol))

(defun cold-setq (symbol value)
  (write-indexed symbol vm:symbol-value-slot value))

;;; Cold-Put  --  Internal
;;;
;;;    Add a property to a symbol in the core.  Assumes it doesn't exist.
;;;
(defun cold-put (symbol indicator value)
  (write-indexed symbol
		 vm:symbol-plist-slot
		 (allocate-cons *dynamic*
			indicator
			(allocate-cons *dynamic*
			       value
			       (read-indexed symbol
					     vm:symbol-plist-slot)))))

;;;; Interning.

;;;    In order to avoid having to know about the package format, we
;;; build a data structure which we stick in *cold-packages* that
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

(defvar *cold-packages* nil)

;;; This holds a hash table mapping from descriptors to symbols, so we can
;;; back up.  The key is the *address* in the target core.
;;; 
(defvar *cold-symbols* (make-hash-table :test #'equal))

;;; Cold-Intern  --  Internal
;;;
;;;    Return a handle on an interned symbol.  If necessary allocate
;;; the symbol and record which package the symbol was referenced in.
;;; When we allocate the symbol, make sure we record a reference to
;;; the symbol in the home package so that the package gets set.
;;;
(defun cold-intern (symbol &optional (package (symbol-package symbol)))
  (let ((cold-info (get symbol 'cold-info)))
    (unless cold-info
      (cond ((eq (symbol-package symbol) package)
	     (let ((handle (allocate-symbol (symbol-name symbol))))
	       (setf (gethash (descriptor-bits handle) *cold-symbols*) symbol)
	       (when (eq package *keyword-package*)
		 (cold-setq handle handle))
	       (setq cold-info
		     (setf (get symbol 'cold-info) (cons handle nil)))))
	    (t
	     (cold-intern symbol)
	     (setq cold-info (get symbol 'cold-info)))))
    (unless (or (null package)
		(member package (cdr cold-info) :test #'eq))
      (push package (cdr cold-info))
      (push symbol (cdr (or (assoc package *cold-packages* :test #'eq)
			    (car (push (list package) *cold-packages*))))))
    (car cold-info)))

;;; Initialize-Symbols  --  Internal
;;;
;;;    Since the initial symbols must be allocated before we can intern
;;; anything else, we intern those here.  We also set the values of T and Nil.
;;;
(defun initialize-symbols ()
  "Initializes the cold load symbol-hacking data structures."
  (do-all-symbols (sym)
    (remprop sym 'cold-info))
  (setq *cold-packages* nil)
  (let ((*cold-symbol-allocation-space* *static*))
    ;; Special case NIL.
    (let ((des (allocate-unboxed-object *static* vm:word-bits
					#+amd64 (1+ vm:symbol-size) #-amd64 vm:symbol-size
					0)))
      (setf *nil-descriptor*
	    (make-descriptor (descriptor-high des)
			     (+ (descriptor-low des) (* 2 vm:word-bytes)
			        (- vm:list-pointer-type
				   vm:other-pointer-type))))
      (write-indexed des 1
		     (make-other-immediate-descriptor 0 vm:symbol-header-type))
      (write-indexed des (1+ vm:symbol-value-slot) *nil-descriptor*)
      (write-indexed des (+ vm:symbol-value-slot 2) *nil-descriptor*)
      (write-indexed des (1+ vm:symbol-plist-slot) *nil-descriptor*)
      (write-indexed des (1+ vm:symbol-name-slot)
		     (string-to-core "NIL" *dynamic*))
      (write-indexed des (1+ vm:symbol-package-slot) *nil-descriptor*)
      (setf (get nil 'cold-info) (cons *nil-descriptor* nil))
      (cold-intern nil))

    ;; Intern the others.
    (dolist (symbol vm:static-symbols)
      (let ((des (cold-intern symbol)))
	(unless (= (- (descriptor-low des) (descriptor-low *nil-descriptor*))
		   (vm:static-symbol-offset symbol))
	  (warn "Offset from ~S to ~S is ~D, not ~D"
		symbol
		nil
		(- (descriptor-low des) (descriptor-low *nil-descriptor*))
		(vm:static-symbol-offset symbol)))))

    ;; Establish the value of T.
    (let ((t-symbol (cold-intern t)))
      (cold-setq t-symbol t-symbol))))

;;; Finish-Symbols  --  Internal
;;;
;;;    Establish initial values for magic symbols.
;;; 
;;;    Scan over all the symbols referenced in each package in *cold-packages*
;;; making the apropriate entry in the *initial-symbols* data structure to
;;; intern the thing.
;;;
(defun finish-symbols ()
  (macrolet ((frob (symbol)
	       `(cold-setq (cold-intern ',symbol)
			   (cold-fdefinition-object (cold-intern ',symbol)))))
    (frob lisp::%initial-function)
    (frob lisp::maybe-gc)
    (frob kernel::internal-error)
    #+stack-checking
    (progn
      (frob kernel::yellow-zone-hit)
      (frob kernel::red-zone-hit))
    #+heap-overflow-check
    (progn
      (frob kernel::dynamic-space-overflow-error-hit)
      (frob kernel::dynamic-space-overflow-warning-hit))
    (frob di::handle-breakpoint)
    (frob di::handle-function-end-breakpoint)
    (frob lisp::fdefinition-object))
  (macrolet ((frob (symbol value)
	       `(cold-setq (cold-intern ',symbol) ,value)))
    (frob *current-catch-block* (make-fixnum-descriptor 0))
    (frob *current-unwind-protect-block* (make-fixnum-descriptor 0))
    (frob *eval-stack-top* (make-fixnum-descriptor 0))

    (frob *free-interrupt-context-index* (make-fixnum-descriptor 0))

    (frob *batch-mode* (cold-intern nil))

    (frob *initial-layouts* (list-all-layouts))
    #+linkage-table
    (frob system::*global-table* (cold-intern nil))
    (let ((res *nil-descriptor*))
      (dolist (cpkg *cold-packages*)
	(let* ((pkg (car cpkg))
	       (shadows (package-shadowing-symbols pkg)))
	  (let ((internal *nil-descriptor*)
		(external *nil-descriptor*)
		(imported-internal *nil-descriptor*)
		(imported-external *nil-descriptor*)
		(shadowing *nil-descriptor*))
	    (dolist (sym (cdr cpkg))
	      (let ((handle (car (get sym 'cold-info))))
		(multiple-value-bind (found where)
				     (find-symbol (symbol-name sym) pkg)
		  (unless (and where (eq found sym))
		    (error "Symbol ~S is not available in ~S." sym pkg))
		  (when (memq sym shadows)
		    (cold-push handle shadowing))
		  (case where
		    (:internal
		     (if (eq (symbol-package sym) pkg)
			 (cold-push handle internal)
			 (cold-push handle imported-internal)))
		    (:external
		     (if (eq (symbol-package sym) pkg)
			 (cold-push handle external)
			 (cold-push handle imported-external)))))))
	    (let ((r *nil-descriptor*))
	      (cold-push shadowing r)
	      (cold-push imported-external r)
	      (cold-push imported-internal r)
	      (cold-push external r)
	      (cold-push internal r)
	      (cold-push (make-make-package-args pkg) r)
	      (cold-push r res)))))
      
      (frob *initial-symbols* res))

    (frob *initial-fdefn-objects* (list-all-fdefn-objects))

    (frob *lisp-initialization-functions* *current-init-functions-cons*)

    (when (c:backend-featurep :x86)
      (macrolet ((frob (name pkg value)
		   `(cold-setq (cold-intern (intern ,name ,pkg)) ,value)))
	(frob "*FP-CONSTANT-0D0*" "X86" (number-to-core 0d0))
	(frob "*FP-CONSTANT-1D0*" "X86" (number-to-core 1d0))
	(frob "*FP-CONSTANT-0S0*" "X86" (number-to-core 0s0))
	(frob "*FP-CONSTANT-1S0*" "X86" (number-to-core 1s0))
	#+long-float
	(when (c:backend-featurep :long-float)
	  (frob "*FP-CONSTANT-0L0*" "X86" (number-to-core 0l0))
	  (frob "*FP-CONSTANT-1L0*" "X86" (number-to-core 1l0))
	  (frob "*FP-CONSTANT-PI*" "X86" (number-to-core pi))
	  (frob "*FP-CONSTANT-L2T*" "X86" (number-to-core (log 10l0 2l0)))
	  (frob "*FP-CONSTANT-L2E*" "X86"
		(number-to-core
		 (log 2.718281828459045235360287471352662L0 2l0)))
	  (frob "*FP-CONSTANT-LG2*" "X86" (number-to-core (log 2l0 10l0)))
	  (frob "*FP-CONSTANT-LN2*" "X86"
		(number-to-core
		 (log 2l0 2.718281828459045235360287471352662L0))))
	(when (c:backend-featurep :gencgc)
	  (frob "*SCAVENGE-READ-ONLY-SPACE*" "X86" (cold-intern nil)))))

    ;; Nothing should be allocated after this.
    ;;
    (frob *read-only-space-free-pointer*
      (allocate-descriptor *read-only* 0 vm:even-fixnum-type))
    (frob *static-space-free-pointer*
      (allocate-descriptor *static* 0 vm:even-fixnum-type))
    (frob *initial-dynamic-space-free-pointer*
      (allocate-descriptor *dynamic* 0 vm:even-fixnum-type))))

;;; Make-Make-Package-Args  --  Internal
;;;
;;;    Make a cold list that can be used as the arglist to make-package to
;;; make a similar package.
;;;
(defun make-make-package-args (package)
  (let* ((use *nil-descriptor*)
	 (nicknames *nil-descriptor*)
	 (res *nil-descriptor*))
    (dolist (u (package-use-list package))
      (when (assoc u *cold-packages*)
	(cold-push (string-to-core (package-name u)) use)))
    (dolist (n (package-nicknames package))
      (cold-push (string-to-core n) nicknames))
    (cold-push (number-to-core (truncate (internal-symbol-count package) 0.8))
	       res)
    (cold-push (cold-intern :internal-symbols) res)
    (cold-push (number-to-core (truncate (external-symbol-count package) 0.8))
	       res)
    (cold-push (cold-intern :external-symbols) res)

    (cold-push nicknames res)
    (cold-push (cold-intern :nicknames) res)

    (cold-push use res)
    (cold-push (cold-intern :use) res)
    
    (cold-push (string-to-core (package-name package)) res)
    res))



;;;; Fdefinition objects.

(defvar *fdefn-objects* (make-hash-table :test #'equal))

(defvar *fdefn-space* nil)

(defun extract-fdefn-name (des)
  (ecase (descriptor-lowtag des)
    (#.vm:list-pointer-type
     (if (= (descriptor-bits des) (descriptor-bits *nil-descriptor*))
	 nil
	 (cons (extract-fdefn-name (read-indexed des vm:cons-car-slot))
	       (extract-fdefn-name (read-indexed des vm:cons-cdr-slot)))))
    (#.vm:other-pointer-type
     (or (gethash (descriptor-bits des) *cold-symbols*)
	 (descriptor-bits des)))))

(defun cold-fdefinition-object (name &optional leave-fn-raw)
  (let ((hash (extract-fdefn-name name)))
    (or (gethash hash *fdefn-objects*)
	(let ((fdefn (allocate-boxed-object (or *fdefn-space* *dynamic*)
					    vm:fdefn-size
					    vm:other-pointer-type)))
	  (setf (gethash hash *fdefn-objects*) fdefn)
	  (write-memory fdefn (make-other-immediate-descriptor
			       (1- vm:fdefn-size) vm:fdefn-type))
	  (write-indexed fdefn vm:fdefn-name-slot name)
	  (unless leave-fn-raw
	    (write-indexed fdefn vm:fdefn-function-slot *nil-descriptor*)
	    (write-indexed fdefn vm:fdefn-raw-addr-slot
			   (make-random-descriptor
			    (lookup-foreign-symbol
			     (vm::extern-alien-name "undefined_tramp")
			     #+(or sparc ppc) :data))))
	  fdefn))))
  
(defun cold-fset (name defn)
  (let ((fdefn (cold-fdefinition-object name t))
	(type (logand (descriptor-low (read-memory defn)) vm:type-mask)))
    (write-indexed fdefn vm:fdefn-function-slot defn)
    (write-indexed fdefn vm:fdefn-raw-addr-slot
		   (ecase type
		     (#.vm:function-header-type
		      (if (or (c:backend-featurep :sparc)
			      (c:backend-featurep :ppc))
			  defn
			  (make-random-descriptor
			   (+ (logandc2 (descriptor-bits defn) vm:lowtag-mask)
			      (ash vm:function-code-offset vm:word-shift)))))
		     (#.vm:closure-header-type
		      (make-random-descriptor
		       (lookup-foreign-symbol
		        (vm::extern-alien-name "closure_tramp")
			#+(or sparc ppc) :data)))))
    fdefn))

(defun initialize-static-fns ()
  (let ((*fdefn-space* *static*))
    (dolist (sym vm:static-functions)
      (let* ((fdefn (cold-fdefinition-object (cold-intern sym)))
	     (offset (- (+ (- (descriptor-low fdefn)
			      vm:other-pointer-type)
			   (* vm:fdefn-raw-addr-slot vm:word-bytes))
			(descriptor-low *nil-descriptor*)))
	     (desired (vm:static-function-offset sym)))
	(unless (= offset desired)
	  (warn "Offset from FDEFN ~S to ~S is ~D, not ~D"
		sym nil offset desired))))))

(defun list-all-fdefn-objects ()
  (let ((result *nil-descriptor*))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (cold-push value result))
	     *fdefn-objects*)
    result))


;;;; Layouts & type system pre-initialization:
;;;
;;; Since we want to be able to dump structure constants and predicates with
;;; reference layouts, we need to create layouts at cold-load time.  We use the
;;; name to intern layouts by, and dump a list of all cold layouts in
;;; *INITIAL-LAYOUTS* so that type-system initialization can find them.  The
;;; only thing that's tricky is initializing layout's layout, which must point
;;; to itself.

;;; Table mapping from class names to lists of:
;;;    (descriptor name length inherits depth)
;;;
(defvar *cold-layouts* (make-hash-table :test #'equal))

;;; Table mapping DESCRIPTOR-BITS of cold layouts to the name, for inverting
;;; mapping.
;;;
(defvar *cold-layout-names* (make-hash-table :test #'eql)) 

;;; The descriptor for layout's layout, which we need when making layouts.
;;;
(defvar *layout-layout*)

(defparameter target-layout-length 16)

(defun make-cold-layout (name length inherits depth)
  (let ((result (allocate-boxed-object *dynamic* (1+ target-layout-length)
				       vm:instance-pointer-type)))
    (write-memory result (make-other-immediate-descriptor
			  target-layout-length vm:instance-header-type))
    (write-indexed result vm:instance-slots-offset *layout-layout*)
    (let ((base (+ vm:instance-slots-offset
		   kernel:layout-hash-length
		   1)))
      ;0 class uninitialized
      (write-indexed result (+ base 1) *nil-descriptor*); invalid
      (write-indexed result (+ base 2) inherits)
      (write-indexed result (+ base 3) depth)
      (write-indexed result (+ base 4) length)
      (write-indexed result (+ base 5) *nil-descriptor*); info
      (write-indexed result (+ base 6) *nil-descriptor*)); pure
    (setf (gethash name *cold-layouts*)
	  (list result name (descriptor-fixnum length)
		(listify-cold-inherits inherits)
		(descriptor-fixnum depth)))
    (setf (gethash (descriptor-bits result) *cold-layout-names*) name)
    result))

(defun listify-cold-inherits (x)
  (let ((len (descriptor-fixnum (read-indexed x vm:vector-length-slot))))
    (collect ((res))
      (dotimes (i len)
	(let* ((des (read-indexed x (+ vm:vector-data-offset i)))
	       (found (gethash (descriptor-bits des) *cold-layout-names*)))
	  (if found
	      (res found)
	      (res (format nil "Unknown descriptor, bits = ~8,'0X"
			   (descriptor-bits des))))))
      (res))))
  
(defun cold-layout-redefined (old inherits depth length)
  (destructuring-bind (descriptor name olength oinherits-l odepth)
		      old
    (declare (ignore descriptor))
    (let ((result nil))
      (unless (eql (descriptor-fixnum length) olength)
	(warn "Changing ~S length from ~D to ~D."
	      name olength (descriptor-fixnum length))
	(setq result t))
      (unless (eql (descriptor-fixnum depth) odepth)
	(warn "Changing ~S inheritance-depth from ~D to ~D."
	      name odepth (descriptor-fixnum depth))
	(setq result t))
      (let ((inherits-l (listify-cold-inherits inherits)))
	(unless (equal inherits-l oinherits-l)
	  (warn "Changing ~S inherits from:~%  ~S~%To:~%  ~S"
		name oinherits-l inherits-l)
	  (setq result t)))
      result)))


;;; INITIALIZE-LAYOUTS  --  Internal
;;;
;;;    Clear the layout table and set *layout-layout*.  We initially create
;;; LAYOUT with NIL as the LAYOUT and INHERITS, then back-patch with itself and
;;; the correct INHERITS vector (which includes T, INSTANCE and
;;; STRUCTURE-OBJECT).
;;;
(defun initialize-layouts ()
  (clrhash *cold-layouts*)
  (setq *layout-layout* *nil-descriptor*)
  (setq *layout-layout*
	(make-cold-layout 'layout (number-to-core target-layout-length)
			  (cold-vector) (number-to-core 3)))
  (write-indexed *layout-layout* vm:instance-slots-offset *layout-layout*)
  (let* ((t-layout
	  (make-cold-layout 't (number-to-core 0)
			    (cold-vector) (number-to-core 0)))
	 (inst-layout
	  (make-cold-layout 'instance (number-to-core 0)
			    (cold-vector t-layout) (number-to-core 1)))
	 (so-layout
	  (make-cold-layout 'structure-object (number-to-core 1)
			    (cold-vector t-layout inst-layout)
			    (number-to-core 2))))
    (let ((layout-inh (cold-vector t-layout inst-layout so-layout)))
      (setf (fourth (gethash 'layout *cold-layouts*))
	    (listify-cold-inherits layout-inh))
      (write-indexed *layout-layout*
		     (+ vm:instance-slots-offset layout-hash-length 1 2)
		     layout-inh))))


;;; LIST-ALL-LAYOUTS  --  Internal
;;;
;;;    Return a cold alist that we're going to store in *INITIAL-LAYOUTS*.
;;;
(defun list-all-layouts ()
  (let ((result *nil-descriptor*))
    (maphash #'(lambda (key stuff)
		 (cold-push (allocate-cons *dynamic* (cold-intern key)
					   (first stuff))
			    result))
	     *cold-layouts*)
    result))


;;;; Reading FASL files.

(defvar *cold-fop-functions* (replace (make-array 256) fop-functions)
  "FOP functions for cold loading.")

(defvar *normal-fop-functions*)

;;; Define-Cold-FOP  --  Internal
;;;
;;;    Like Define-FOP in load, but looks up the code, and stores into
;;; the *cold-fop-functions* vector.
;;;
(defmacro define-cold-fop ((name &optional (pushp t)) &rest forms)
  (let ((fname (concat-pnames 'cold- name))
	(code (get name 'fop-code)))
    `(progn
       (defun ,fname ()
	 ,@(if (eq pushp :nope)
	       forms
	       `((with-fop-stack ,pushp ,@forms))))
       ,@(if code
	     `((setf (svref *cold-fop-functions* ,code) #',fname))
	     (warn "~S is not a defined FOP." name)))))

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

(define-cold-fop (fop-misc-trap) unbound-marker)

(define-cold-fop (fop-character)
  (make-character-descriptor (read-arg 3)))
(define-cold-fop (fop-short-character)
  (make-character-descriptor (read-arg 1)))

(define-cold-fop (fop-empty-list) *nil-descriptor*)
(define-cold-fop (fop-truth) (cold-intern t))

(define-cold-fop (fop-normal-load :nope)
  (setq fop-functions *normal-fop-functions*))

(define-fop (fop-maybe-cold-load 82 :nope)
  (when *in-cold-load*
    (setq fop-functions *cold-fop-functions*)))

(define-cold-fop (fop-maybe-cold-load :nope))

(clone-cold-fop (fop-struct)
		(fop-small-struct)
  (let* ((size (clone-arg))
	 (result (allocate-boxed-object *dynamic* (1+ size)
					vm:instance-pointer-type)))
    (write-memory result (make-other-immediate-descriptor
			  size vm:instance-header-type))
    (do ((index (1- size) (1- index)))
	((minusp index))
      (declare (fixnum index))
      (write-indexed result (+ index vm:instance-slots-offset) (pop-stack)))
    result))

;;; Check for layout redefinition, and possibly clobber old layout w/ new info.
;;;
(define-cold-fop (fop-layout)
  (let* ((length (pop-stack))
	 (depth (pop-stack))
	 (inherits (pop-stack))
	 (name (pop-stack))
	 (old (gethash name *cold-layouts*)))
    (cond
     (old
      (when (cold-layout-redefined old inherits depth length)
	(restart-case
	    (error "Loading a reference to class ~S when the compile~
		    ~%  time definition was incompatible with the current ~
		    one."
		   name)
	  (use-current ()
	    :report "Ignore the incompatibility, leave class alone."
	    (warn "Assuming the current definition of ~S is correct, and~@
		   that the loaded code doesn't care about the ~
		   incompatibility."
		  name))
	  (clobber-it ()
	    :report "Smash current layout, preserving old code."
	    (warn "Any old ~S instances will be in a bad way.~@
		   I hope you know what you're doing..."
		  name)
	    
	    (let ((base (+ vm:instance-slots-offset
			   kernel:layout-hash-length
			   1))
		  (desc (first old)))
	      (write-indexed desc (+ base 2) inherits)
	      (write-indexed desc (+ base 3) depth)
	      (write-indexed desc (+ base 4) length)
	      (setf (gethash name *cold-layouts*)
		    (list desc name (descriptor-fixnum length)
			  (listify-cold-inherits inherits)
			  (descriptor-fixnum depth)))))))
      (first old))
     (t
      (make-cold-layout name length inherits depth)))))


;;; Loading symbols...

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
	 (result ,last (allocate-cons *dynamic* (pop-stack) result)))
	((= index 0) result)
     (declare (fixnum index))))

(define-cold-fop (fop-list)
  (cold-stack-list (read-arg 1) *nil-descriptor*))
(define-cold-fop (fop-list*)
  (cold-stack-list (read-arg 1) (pop-stack)))
(define-cold-fop (fop-list-1)
  (cold-stack-list 1 *nil-descriptor*))
(define-cold-fop (fop-list-2)
  (cold-stack-list 2 *nil-descriptor*))
(define-cold-fop (fop-list-3)
  (cold-stack-list 3 *nil-descriptor*))
(define-cold-fop (fop-list-4)
  (cold-stack-list 4 *nil-descriptor*))
(define-cold-fop (fop-list-5)
  (cold-stack-list 5 *nil-descriptor*))
(define-cold-fop (fop-list-6)
  (cold-stack-list 6 *nil-descriptor*))
(define-cold-fop (fop-list-7)
  (cold-stack-list 7 *nil-descriptor*))
(define-cold-fop (fop-list-8)
  (cold-stack-list 8 *nil-descriptor*))
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

;;; Loading vectors...

(clone-cold-fop (fop-string)
		(fop-small-string)
  (let* ((len (clone-arg))
	 (string (make-string len)))
    (read-n-bytes *fasl-file* string 0 len)
    (string-to-core string)))

(clone-cold-fop (fop-vector)
		(fop-small-vector)
  (let* ((size (clone-arg))
	 (result (allocate-vector-object *dynamic* vm:word-bits size
					 vm:simple-vector-type)))
    (do ((index (1- size) (1- index)))
	((minusp index))
      (declare (fixnum index))
      (write-indexed result (+ index vm:vector-data-offset) (pop-stack)))
    result))

(clone-cold-fop (fop-uniform-vector)
		(fop-small-uniform-vector)
  (let* ((size (clone-arg))
	 (datum (pop-stack))
	 (result (allocate-vector-object *dynamic* vm:word-bits size
					 vm:simple-vector-type)))
    (do ((index (1- size) (1- index)))
	((minusp index))
      (declare (fixnum index))
      (write-indexed result (+ index vm:vector-data-offset) datum))
    result))

(define-cold-fop (fop-uniform-int-vector)
  (let* ((len (read-arg 4))
	 (size (read-arg 1))
	 (type (case size
		 (1 vm:simple-bit-vector-type)
		 (2 vm:simple-array-unsigned-byte-2-type)
		 (4 vm:simple-array-unsigned-byte-4-type)
		 (8 vm:simple-array-unsigned-byte-8-type)
		 (16 vm:simple-array-unsigned-byte-16-type)
		 (32 vm:simple-array-unsigned-byte-32-type)
		 (t (error "Losing element size: ~D." size))))
	 (value (case size
		  ((1 2 4 8)
		   (read-arg 1))
		  (16
		   (read-arg 2))
		  (32
		   (read-arg 4))))
	 (result (allocate-vector-object *dynamic* size len type)))
    (do ((bits size (* bits 2))
	 (word value (logior word (ash word bits))))
	((= size vm:word-bits)
	 (let ((datum (make-random-descriptor word)))
	   (dotimes (index (ceiling (* len size) vm:word-bits))
	     (write-indexed result (+ index vm:vector-data-offset) datum)))))
    result))

(define-cold-fop (fop-int-vector)
  (let* ((len (read-arg 4))
	 (size (read-arg 1))
	 (type (case size
		 (1 vm:simple-bit-vector-type)
		 (2 vm:simple-array-unsigned-byte-2-type)
		 (4 vm:simple-array-unsigned-byte-4-type)
		 (8 vm:simple-array-unsigned-byte-8-type)
		 (16 vm:simple-array-unsigned-byte-16-type)
		 (32 vm:simple-array-unsigned-byte-32-type)
		 (t (error "Losing element size: ~D." size))))
	 (result (allocate-vector-object *dynamic* size len type)))
    (unless (zerop len)
      (read-n-bytes *fasl-file*
		    (descriptor-sap result)
		    (ash vm:vector-data-offset vm:word-shift)
		    (ceiling (* len size) vm:byte-bits)))
    result))

(define-cold-fop (fop-single-float-vector)
  (let* ((len (read-arg 4))
	 (result (allocate-vector-object *dynamic* vm:word-bits len
					 vm:simple-array-single-float-type)))
    (unless (zerop len)
      (read-n-bytes *fasl-file*
		    (descriptor-sap result)
		    (ash vm:vector-data-offset vm:word-shift)
		    (* len vm:word-bytes)))
    result))

(define-cold-fop (fop-double-float-vector)
  (let* ((len (read-arg 4))
	 (result (allocate-vector-object *dynamic* (* vm:word-bits 2) len
					 vm:simple-array-double-float-type)))
    (unless (zerop len)
      (read-n-bytes *fasl-file*
		    (descriptor-sap result)
		    (ash vm:vector-data-offset vm:word-shift)
		    (* len vm:word-bytes 2)))
    result))

#+long-float (not-cold-fop fop-long-float-vector)
(not-cold-fop fop-complex-single-float-vector)
(not-cold-fop fop-complex-double-float-vector)
#+long-float (not-cold-fop fop-complex-long-float-vector)
;; Why is this not-cold-fop?  I'm just cargo-culting this.
#+double-double (not-cold-fop fop-double-double-float-vector)
#+double-double (not-cold-fop fop-complex-double-double-float-vector)

(define-cold-fop (fop-array)
  (let* ((rank (read-arg 4))
	 (data-vector (pop-stack))
	 (result (allocate-boxed-object *dynamic*
					(+ vm:array-dimensions-offset rank)
					vm:other-pointer-type)))
    (write-memory result
		  (make-other-immediate-descriptor rank vm:simple-array-type))
    (write-indexed result vm:array-fill-pointer-slot *nil-descriptor*)
    (write-indexed result vm:array-data-slot data-vector)
    (write-indexed result vm:array-displacement-slot *nil-descriptor*)
    (write-indexed result vm:array-displaced-p-slot *nil-descriptor*)
    (let ((total-elements 1))
      (dotimes (axis rank)
	(let ((dim (pop-stack)))
	  (unless (or (= (descriptor-lowtag dim) vm:even-fixnum-type)
		      (= (descriptor-lowtag dim) vm:odd-fixnum-type))
	    (error "Non-fixnum dimension? (~S)" dim))
	  (setf total-elements
		(* total-elements
		   (logior (ash (descriptor-high dim)
				(- descriptor-low-bits (1- vm:lowtag-bits)))
			   (ash (descriptor-low dim)
				(- 1 vm:lowtag-bits)))))
	  (write-indexed result (+ vm:array-dimensions-offset axis) dim)))
      (write-indexed result vm:array-elements-slot
		     (make-fixnum-descriptor total-elements)))
    result))

(defun make-cold-linkage-vector (vec)
  (let* ((result (allocate-boxed-object *dynamic*
					(+ vm:array-dimensions-offset 1)
					vm:other-pointer-type))
	 (vec-length (length vec))
	 (data-vec (allocate-vector-object *dynamic*
					   vm:word-bits
					   vec-length
					   vm:simple-vector-type)))
    (loop for i from 0 below vec-length
	  for vec-elem = (aref vec i)
	  do (write-indexed data-vec (+ i vm:vector-data-offset)
			    (etypecase vec-elem
			      (string
			       (string-to-core vec-elem))
			      (number
			       (number-to-core vec-elem))
			      (null
			       *nil-descriptor*))))
    (write-memory result
		  (make-other-immediate-descriptor vm:array-dimensions-offset
						   vm:complex-vector-type))
    (write-indexed result vm:array-elements-slot (number-to-core vec-length))
    (write-indexed result vm:array-displacement-slot (number-to-core 0))
    (write-indexed result vm:array-displaced-p-slot *nil-descriptor*)
    (write-indexed result vm:array-data-slot data-vec)
    (write-indexed result
		   vm:array-fill-pointer-slot
		   (number-to-core vec-length))
    (write-indexed result vm:array-fill-pointer-p-slot (cold-intern t))
    (write-indexed result
		   vm:array-dimensions-offset
		   (number-to-core vec-length))
    result))


;;; Loading numbers.

(defmacro cold-number (fop)
  `(define-cold-fop (,fop :nope)
     (,fop)
     (with-fop-stack t
       (number-to-core (pop-stack)))))

(cold-number fop-single-float)
(cold-number fop-double-float)
#+double-double
(cold-number fop-double-double-float)
(cold-number fop-integer)
(cold-number fop-small-integer)
(cold-number fop-word-integer)
(cold-number fop-byte-integer)
(cold-number fop-complex-single-float)
(cold-number fop-complex-double-float)
#+double-double
(cold-number fop-complex-double-double-float)

#+long-float
(define-cold-fop (fop-long-float)
  (ecase (c:backend-fasl-file-implementation c:*backend*)
    (#.c:x86-fasl-file-implementation		; 80 bit long-float format.
     (prepare-for-fast-read-byte *fasl-file*
       (let* ((des (allocate-unboxed-object *dynamic* vm:word-bits
					    (1- vm:long-float-size)
					    vm:long-float-type))
	      (low-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (high-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (exp-bits (make-random-descriptor (fast-read-s-integer 2))))
	 (done-with-fast-read-byte)
	 (write-indexed des vm:long-float-value-slot low-bits)
	 (write-indexed des (1+ vm:long-float-value-slot) high-bits)
	 (write-indexed des (+ 2 vm:long-float-value-slot) exp-bits)
	 des)))
    (#.c:sparc-fasl-file-implementation		; 128 bit long-float format.
     (prepare-for-fast-read-byte *fasl-file*
       (let* ((des (allocate-unboxed-object *dynamic* vm:word-bits
					    (1- vm:long-float-size)
					    vm:long-float-type))
	      (low-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (mid-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (high-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (exp-bits (make-random-descriptor (fast-read-s-integer 4))))
	 (done-with-fast-read-byte)
	 (write-indexed des vm:long-float-value-slot exp-bits)
	 (write-indexed des (1+ vm:long-float-value-slot) high-bits)
	 (write-indexed des (+ 2 vm:long-float-value-slot) mid-bits)
	 (write-indexed des (+ 3 vm:long-float-value-slot) low-bits)
	 des)))))

#+long-float
(define-cold-fop (fop-complex-long-float)
  (ecase (c:backend-fasl-file-implementation c:*backend*)
    (#.c:x86-fasl-file-implementation		; 80 bit long-float format.
     (prepare-for-fast-read-byte *fasl-file*
       (let* ((des (allocate-unboxed-object *dynamic* vm:word-bits
					    (1- vm:complex-long-float-size)
					    vm:complex-long-float-type))
	      (real-low-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (real-high-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (real-exp-bits (make-random-descriptor (fast-read-s-integer 2)))
	      (imag-low-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (imag-high-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (imag-exp-bits (make-random-descriptor (fast-read-s-integer 2))))
	 (done-with-fast-read-byte)
	 (write-indexed des vm:complex-long-float-real-slot real-low-bits)
	 (write-indexed des (1+ vm:complex-long-float-real-slot)
			real-high-bits)
	 (write-indexed des (+ 2 vm:complex-long-float-real-slot)
			real-exp-bits)
	 (write-indexed des vm:complex-long-float-imag-slot imag-low-bits)
	 (write-indexed des (1+ vm:complex-long-float-imag-slot)
			imag-high-bits)
	 (write-indexed des (+ 2 vm:complex-long-float-imag-slot)
			imag-exp-bits)
	 des)))
    (#.c:sparc-fasl-file-implementation		; 128 bit long-float format.
     (prepare-for-fast-read-byte *fasl-file*
       (let* ((des (allocate-unboxed-object *dynamic* vm:word-bits
					    (1- vm:complex-long-float-size)
					    vm:complex-long-float-type))
	      (real-low-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (real-mid-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (real-high-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (real-exp-bits (make-random-descriptor (fast-read-s-integer 4)))
	      (imag-low-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (imag-mid-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (imag-high-bits (make-random-descriptor (fast-read-u-integer 4)))
	      (imag-exp-bits (make-random-descriptor (fast-read-s-integer 4))))
	 (done-with-fast-read-byte)
	 (write-indexed des vm:complex-long-float-real-slot real-exp-bits)
	 (write-indexed des (1+ vm:complex-long-float-real-slot)
			real-high-bits)
	 (write-indexed des (+ 2 vm:complex-long-float-real-slot)
			real-mid-bits)
	 (write-indexed des (+ 3 vm:complex-long-float-real-slot)
			real-low-bits)
	 (write-indexed des vm:complex-long-float-real-slot imag-exp-bits)
	 (write-indexed des (1+ vm:complex-long-float-real-slot)
			imag-high-bits)
	 (write-indexed des (+ 2 vm:complex-long-float-real-slot)
			imag-mid-bits)
	 (write-indexed des (+ 3 vm:complex-long-float-real-slot)
			imag-low-bits)
	 des)))))

#+(and nil double-double)
(define-cold-fop (fop-double-double-float)
    ;; Double-double format
    (prepare-for-fast-read-byte *fasl-file*
      (let* ((des (allocate-unboxed-object *dynamic* vm:word-bits
					   (1- vm:double-double-float-size)
					   vm:double-double-float-type))
	     (hi-high-bits (make-random-descriptor (fast-read-s-integer 4)))
	     (hi-low-bits (make-random-descriptor (fast-read-u-integer 4)))
	     (lo-high-bits (make-random-descriptor (fast-read-s-integer 4)))
	     (lo-low-bits (make-random-descriptor (fast-read-s-integer 4))))
	(done-with-fast-read-byte)
	(write-indexed des vm:double-double-float-hi-slot hi-high-bits)
	(write-indexed des (1+ vm:double-double-float-hi-slot) hi-low-bits)
	(write-indexed des vm:double-double-float-lo-slot lo-high-bits)
	(write-indexed des (1+ vm:double-double-float-lo-slot) lo-low-bits)
	des)))

(define-cold-fop (fop-ratio)
  (let ((den (pop-stack)))
    (number-pair-to-core (pop-stack) den vm:ratio-type)))

(define-cold-fop (fop-complex)
  (let ((im (pop-stack)))
    (number-pair-to-core (pop-stack) im vm:complex-type)))


;;; Calling (or not calling).

(not-cold-fop fop-eval)
(not-cold-fop fop-eval-for-effect)


(defvar *load-time-value-counter*)

(define-cold-fop (fop-funcall)
  (unless (= (read-arg 1) 0)
    (error "Can't FOP-FUNCALL random stuff in cold load."))
  (let ((counter *load-time-value-counter*))
    (cold-push (allocate-cons
		*dynamic*
		(cold-intern :load-time-value)
		(allocate-cons
		 *dynamic*
		 (pop-stack)
		 (allocate-cons
		  *dynamic*
		  (number-to-core counter)
		  *nil-descriptor*)))
	       *current-init-functions-cons*)
    (setf *load-time-value-counter* (1+ counter))
    (make-descriptor 0 0 nil counter)))

(defun note-load-time-value-reference (address marker)
  (cold-push (allocate-cons
	      *dynamic*
	      (cold-intern :load-time-value-fixup)
	      (allocate-cons
	       *dynamic*
	       (sap-to-core address)
	       (allocate-cons
		*dynamic*
		(number-to-core (descriptor-offset marker))
		*nil-descriptor*)))
	     *current-init-functions-cons*))

(defun finalize-load-time-value-noise ()
  (cold-setq (cold-intern 'lisp::*load-time-values*)
	     (allocate-vector-object *dynamic* vm:word-bits
				     *load-time-value-counter*
				     vm:simple-vector-type)))

(define-cold-fop (fop-funcall-for-effect nil)
  (if (= (read-arg 1) 0)
      (cold-push (pop-stack) *current-init-functions-cons*)
      (error "Can't FOP-FUNCALL random stuff in cold load.")))


;;;; Fixing up circularities.

(define-cold-fop (fop-rplaca nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-memory (cold-nthcdr idx obj) (pop-stack))))

(define-cold-fop (fop-rplacd nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-indexed (cold-nthcdr idx obj) 1 (pop-stack))))

(define-cold-fop (fop-svset nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-indexed obj
		   (+ idx
		      (ecase (descriptor-lowtag obj)
			(#.vm:instance-pointer-type 1)
			(#.vm:other-pointer-type 2)))
		   (pop-stack))))

(define-cold-fop (fop-structset nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-indexed obj (1+ idx) (pop-stack))))

(define-cold-fop (fop-nthcdr t)
  (cold-nthcdr (read-arg 4) (pop-stack)))


(defun cold-nthcdr (index obj)
  (dotimes (i index)
    (setq obj (read-indexed obj 1)))
  obj)


;;; Loading code objects and functions.

(define-cold-fop (fop-fset nil)
  (let ((fn (pop-stack))
	(name (pop-stack)))
    (cold-fset name fn)))

(define-cold-fop (fop-fdefinition)
  (cold-fdefinition-object (pop-stack)))

(define-cold-fop (fop-sanctify-for-execution)
  (pop-stack))

;;; must be compatible with the function OPEN-FASL-FILE in compiler/dump.lisp
(clone-cold-fop (fop-long-code-format :nope) (fop-code-format)
  (let ((implementation (read-arg 1))
	(version (clone-arg)))
    (unless (= implementation (c:backend-fasl-file-implementation c:*backend*))
      (cerror
       "Load ~A anyway"
       "~A was compiled for a ~A, but we are trying to build a core for a ~A"
       *Fasl-file*
       (if (< -1 implementation (length c:fasl-file-implementations))
           (elt c:fasl-file-implementations implementation)
	   "unknown machine")
       (if (< -1 implementation (length c:fasl-file-implementations))
           (elt c:fasl-file-implementations
		(c:backend-fasl-file-implementation c:*backend*))
	   "unknown machine")))
    (unless (or *skip-fasl-file-version-check*
                (eql version (c:backend-fasl-file-version c:*backend*)))
      (cerror
       "Load ~A anyway"
       "~A was compiled for a fasl-file version ~X, but we need version ~X"
       *Fasl-file* version (c:backend-fasl-file-version c:*backend*)))))

(not-cold-fop fop-make-byte-compiled-function)

(defmacro define-cold-code-fop (name nconst size)
  `(define-cold-fop (,name)
     (let* ((nconst ,nconst)
	    (size ,size)
	    (raw-header-size (+ vm:code-trace-table-offset-slot nconst))
	    (header-size
	     ;; Note: we round the number of constants up to assure that
	     ;; the code vector will be properly aligned.
	     (round-up raw-header-size 2))
	    (des (allocate-descriptor
		  (if (and (c:backend-featurep :x86) (c:backend-featurep :cgc))
		      *static*
		      *dynamic*)
		  (+ (ash header-size vm:word-shift) size)
		  vm:other-pointer-type)))
       (write-memory des
		     (make-other-immediate-descriptor header-size
						      vm:code-header-type))
       (write-indexed des vm:code-code-size-slot
		      (make-fixnum-descriptor
		       (ash (+ size (1- (ash 1 vm:word-shift)))
			    (- vm:word-shift))))
       (write-indexed des vm:code-entry-points-slot *nil-descriptor*)
       (write-indexed des vm:code-debug-info-slot (pop-stack))
       (when (oddp raw-header-size)
	 (write-indexed des raw-header-size
			(make-random-descriptor 0)))
       (do ((index (1- raw-header-size) (1- index)))
	   ((< index vm:code-trace-table-offset-slot))
	 (write-indexed des index (pop-stack)))
       (read-n-bytes *fasl-file*
		     (descriptor-sap des)
		     (ash header-size vm:word-shift)
		     size)
       des)))  

(define-cold-code-fop fop-code (read-arg 4) (read-arg 4))

(define-cold-code-fop fop-small-code (read-arg 1) (read-arg 2))


(clone-cold-fop (fop-alter-code nil)
		(fop-byte-alter-code)
  (let ((slot (clone-arg))
	(value (pop-stack))
	(code (pop-stack)))
    (write-indexed code slot value)))

(defun calc-offset (code-object after-header)
  (let ((header (read-memory code-object)))
    (+ after-header
       (ash (logior (ash (descriptor-high header)
			 (- descriptor-low-bits vm:type-bits))
		    (ash (descriptor-low header)
			 (- vm:type-bits)))
	    vm:word-shift))))

(define-cold-fop (fop-function-entry)
  (let* ((type (pop-stack))
	 (arglist (pop-stack))
	 (name (pop-stack))
	 (code-object (pop-stack))
	 (offset (calc-offset code-object (read-arg 4)))
	 (fn (descriptor-beyond code-object offset vm:function-pointer-type))
	 (next (read-indexed code-object vm:code-entry-points-slot)))
    (unless (zerop (logand offset vm:lowtag-mask))
      (warn "Unaligned function entry: ~S at #x~X" name offset))
    (write-indexed code-object vm:code-entry-points-slot fn)
    (write-memory fn (make-other-immediate-descriptor (ash offset
							   (- vm:word-shift))
						      vm:function-header-type))
    (write-indexed fn vm:function-self-slot
		   (if (or (= (c:backend-fasl-file-implementation c:*backend*)
			      #.c:x86-fasl-file-implementation)
			   (= (c:backend-fasl-file-implementation c:*backend*)
			      #.c:amd64-fasl-file-implementation))
		       ;; point to raw code instead of the code function object itself
		       (make-random-descriptor
			(+ (descriptor-bits fn)
			   (- (ash vm:function-code-offset
				   vm:word-shift)
			      vm:function-pointer-type)))
		       fn))
    (write-indexed fn vm:function-next-slot next)
    (write-indexed fn vm:function-name-slot name)
    (write-indexed fn vm:function-arglist-slot arglist)
    (write-indexed fn vm:function-type-slot type)
    fn))

(define-cold-fop (fop-foreign-fixup)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (read-n-bytes *fasl-file* sym 0 len)
    (let ((offset (read-arg 4))
	  (value #+linkage-table (cold-register-foreign-linkage sym :code)
		 #-linkage-table (lookup-foreign-symbol sym)))
      (do-cold-fixup code-object offset value kind))
    code-object))

;;; Should we do something for this in the non linkage table case, or should we
;;; count on not emitting them at all?

(define-cold-fop (fop-foreign-data-fixup)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (read-n-bytes *fasl-file* sym 0 len)
    (let ((offset (read-arg 4))
	  (value (cold-register-foreign-linkage sym :data)))
      (do-cold-fixup code-object offset value kind))
    code-object))

(define-cold-fop (fop-assembler-code)
  (let* ((length (read-arg 4))
	 (header-size
	  ;; Note: we round the number of constants up to assure that
	  ;; the code vector will be properly aligned.
	  (round-up vm:code-constants-offset 2))
	 (des (allocate-descriptor *read-only*
				   (+ (ash header-size vm:word-shift) length)
				   vm:other-pointer-type)))
    (write-memory des
		  (make-other-immediate-descriptor header-size
						   vm:code-header-type))
    (write-indexed des vm:code-code-size-slot
		   (make-fixnum-descriptor
		    (ash (+ length (1- (ash 1 vm:word-shift)))
			 (- vm:word-shift))))
    (write-indexed des vm:code-entry-points-slot *nil-descriptor*)
    (write-indexed des vm:code-debug-info-slot *nil-descriptor*)

    (read-n-bytes *fasl-file*
		  (descriptor-sap des)
		  (ash header-size vm:word-shift)
		  length)
    des))

(define-cold-fop (fop-assembler-routine)
  (let* ((routine (pop-stack))
	 (des (pop-stack))
	 (offset (calc-offset des (read-arg 4))))
    (record-cold-assembler-routine
     routine
     (+ (logandc2 (descriptor-bits des) vm:lowtag-mask) offset))
    des))

(define-cold-fop (fop-assembler-fixup)
  (let* ((routine (pop-stack))
	 (kind (pop-stack))
	 (code-object (pop-stack))
	 (offset (read-arg 4)))
    (record-cold-assembler-fixup routine code-object offset kind)
    code-object))

(define-cold-fop (fop-code-object-fixup)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (offset (read-arg 4))
	 (value (descriptor-bits code-object)))
    (do-cold-fixup code-object offset value kind)
    code-object))


;;; Cold-Load loads stuff into the core image being built by rebinding
;;; the Fop-Functions table to a table of cold loading functions.

(defun cold-load (filename)
  "Loads the file named by FileName into the cold load image being built."
  (let* ((*normal-fop-functions* fop-functions)
	 (fop-functions *cold-fop-functions*)
	 (*in-cold-load* t))
    (with-open-file (file (merge-pathnames
			   filename
			   (make-pathname
			    :type (c:backend-fasl-file-type c:*backend*)))
			  :element-type '(unsigned-byte 8))
      (load file :verbose nil))))



;;;; Fixups and related stuff.

(defvar *cold-foreign-symbol-table*
  (make-hash-table :test 'equal))

(defun init-foreign-symbol-table ()
  (clrhash *cold-foreign-symbol-table*))

(defun load-foreign-symbol-table (filename)
  (with-open-file (file filename)
    (let* ((version-line (read-line file))
	   (last-space (position #\Space version-line :from-end t))
	   (version (parse-integer version-line :start (1+ last-space))))
      (loop
	(let ((line (read-line file nil nil)))
	  (unless line
	    (return))
	  (when (or (whitespacep (aref line 0))
		    (whitespacep (aref line (1- (length line)))))
	    (setq line (string-trim '(#\Space #\Tab) line)))
	  (let ((p1 (position-if #'whitespacep line :from-end nil))
		(p2 (position-if #'whitespacep line :from-end t)))
	    (if (not (and p1 p2 (< p1 p2)))
		(warn "Ignoring malformed line ~s in ~a" line filename)
		(multiple-value-bind
		    (value name)
		    (if (string= "0x" line :end2 2)
			(values (parse-integer line :start 2 :end p1 :radix 16)
				(subseq line (1+ p2)))
			(values (parse-integer line :end p1 :radix 16)
				(subseq line (1+ p2))))
		  (multiple-value-bind
		      (old-value found)
		      (gethash name *cold-foreign-symbol-table*)
		    (when found
		      (warn "Redefining ~S from #x~X to #x~X"
			    name old-value value)))
		  (setf (gethash name *cold-foreign-symbol-table*) value))))))
      version)))

(defun lookup-foreign-symbol (name &optional (link-type :code))
  #-linkage-table
  (declare (ignore link-type))
  (flet ((lookup-sym (name)
	   #-linkage-table
	   (gethash name *cold-foreign-symbol-table* nil)
	   #+linkage-table
	   (cold-register-foreign-linkage name link-type)))
    (let ((linux-p (and (or (eq (c:backend-fasl-file-implementation c:*backend*)
				#.c:x86-fasl-file-implementation)
			    (eq (c:backend-fasl-file-implementation c:*backend*)
				#.c:alpha-fasl-file-implementation))
			(c:backend-featurep :linux)))
	  (freebsd4-p (and (eq (c:backend-fasl-file-implementation c:*backend*)
			       #.c:x86-fasl-file-implementation)
			   (c:backend-featurep :freebsd4))))
      (cond
	((and #+linkage-table nil
	      (or linux-p freebsd4-p)
	      (lookup-sym (concatenate 'string "PVE_stub_" name))))
	((and #+linkage-table nil
	      (c:backend-featurep :darwin)
	      (lookup-sym (concatenate 'string "ldso_stub__" name))))
	;; Non-linux case
	(#-irix
	 (lookup-sym name)
	 #+irix
	 (let ((value (lookup-sym name)))
	   (when (and (numberp value) (zerop value))
	     (warn "Not-really-defined foreign symbol: ~S" name))
	   value)
	 #+(or sparc ppc)
	 (let ((address (lookup-sym name)))
	   ;; If the link-type is :data, need to lookup and return the
	   ;; value, not the address of NAME in the linkage table.
	   ;;
	   ;; Perhaps we should consider changing how sparc handles
	   ;; undefined_tramp and closure_tramp?  We could add special
	   ;; entries for them so that instead of doing
	   ;; resolve_linkage_tramp stuff, we jump directly to the
	   ;; routines.
	   (if (eq link-type :code)
	       address
	     (sys:sap-ref-32 (sys:int-sap address) 0))))
	;; Are those still necessary?
	((and linux-p (lookup-sym (concatenate 'string "__libc_" name))))
	((and linux-p (lookup-sym (concatenate 'string "__" name))))
	((and linux-p (lookup-sym (concatenate 'string "_" name))))
	(t
	 (warn "Undefined foreign symbol: ~S" name)
	 0)))))

(defvar *cold-linkage-table* (make-array 8192 :adjustable t :fill-pointer 0))
(defvar *cold-foreign-hash* (make-hash-table :test #'equal))

#+linkage-table
(defun cold-register-foreign-linkage (sym type)
  (let ((entry-num (register-foreign-linkage sym
					     type
					     *cold-linkage-table*
					     *cold-foreign-hash*)))
    (+ vm:target-foreign-linkage-space-start
       (* entry-num vm:target-foreign-linkage-entry-size))))

(defun init-foreign-linkage ()
  (setf (fill-pointer *cold-linkage-table*) 0)
  (clrhash *cold-foreign-hash*)
  ;; This has gotta be the first entry.  This has to match what
  ;; os_foreign_linkage_init does!
  #+(or x86 amd64)
  (cold-register-foreign-linkage "resolve_linkage_tramp" :code)
  #+(or sparc ppc)
  (progn
    (cold-register-foreign-linkage (vm::extern-alien-name "call_into_c") :code)
    (cold-register-foreign-linkage (vm::extern-alien-name "undefined_tramp") :data)
    (cold-register-foreign-linkage (vm::extern-alien-name "closure_tramp") :data)
    ))

(defvar *cold-assembler-routines* nil)

(defvar *cold-assembler-fixups* nil)

(defun record-cold-assembler-routine (name address)
  (push (cons name address)
	*cold-assembler-routines*))

(defun record-cold-assembler-fixup
       (routine code-object offset &optional (kind :both))
  (push (list routine code-object offset kind)
	*cold-assembler-fixups*))

(defun lookup-assembler-reference (symbol)
  (let ((value (cdr (assoc symbol *cold-assembler-routines*))))
    (unless value (warn "Assembler routine ~S not defined." symbol))
    value))

(defun resolve-assembler-fixups ()
  (dolist (fixup *cold-assembler-fixups*)
    (let* ((routine (car fixup))
	   (value (lookup-assembler-reference routine)))
      (when value
	(do-cold-fixup (second fixup) (third fixup) value (fourth fixup))))))

;;; The x86 port needs to store code fixups along with code objects if
;;; they are to be moved, so fixups for code objects in the dynamic
;;; heap need to be noted.
;;;
(defvar *load-time-code-fixups* nil)

(defun note-load-time-code-fixup (code-object offset value kind)
  (assert (or (c:backend-featurep :x86) (c:backend-featurep :amd64)))
  (when (= (space-identifier (descriptor-space code-object)) dynamic-space-id)
    (push (list code-object offset value kind) *load-time-code-fixups*)))

(defun output-load-time-code-fixups ()
  (dolist (fixups *load-time-code-fixups*)
    (let ((code-object (first fixups))
	  (offset (second fixups))
	  (value (third fixups))
	  (kind (fourth fixups)))
      (cold-push (allocate-cons
		  *dynamic*
		  (cold-intern :load-time-code-fixup)
		  (allocate-cons
		   *dynamic*
		   code-object
		   (allocate-cons
		    *dynamic*
		    (number-to-core offset)
		    (allocate-cons
		     *dynamic*
		     (number-to-core value)
		     (allocate-cons
		      *dynamic*
		      (cold-intern kind)
		      *nil-descriptor*)))))
		 *current-init-functions-cons*))))

(defun do-cold-fixup (code-object after-header value kind)
  (let* ((offset (calc-offset code-object after-header))
	 (sap (sap+ (descriptor-sap code-object) offset)))
    (ecase (c:backend-fasl-file-implementation c:*backend*)
      (#.c:pmax-fasl-file-implementation
       (ecase kind
	 (:jump
	  (assert (zerop (ash value -28)))
	  (let ((inst (maybe-byte-swap (sap-ref-32 sap 0))))
	    (setf (ldb (byte 26 0) inst) (ash value -2))
	    (setf (sap-ref-32 sap 0) (maybe-byte-swap inst))))
	 (:lui
	  (setf (sap-ref-16 sap 0)
		(maybe-byte-swap-short
		 (+ (ash value -16)
		    (if (logbitp 15 value) 1 0)))))
	 (:addi
	  (setf (sap-ref-16 sap 0)
		(maybe-byte-swap-short
		 (ldb (byte 16 0) value))))))
      (#.c:sparc-fasl-file-implementation
       (let ((inst (maybe-byte-swap (sap-ref-32 sap 0))))
	 (ecase kind
	   (:call
	    (error "Can't deal with call fixups yet."))
	   (:sethi
	    (setf inst
		  (dpb (ldb (byte 22 10) value)
		       (byte 22 0)
		       inst)))
	   (:add
	    (setf inst
		  (dpb (ldb (byte 10 0) value)
		       (byte 10 0)
		       inst))))
	 (setf (sap-ref-32 sap 0)
	       (maybe-byte-swap inst))))
      ((#.c:rt-fasl-file-implementation 
	#.c:rt-afpa-fasl-file-implementation)
       (ecase kind
	 (:cal
	  (setf (sap-ref-16 sap 2)
		(maybe-byte-swap-short
		 (ldb (byte 16 0) value))))
	 (:cau
	  (let ((high (ldb (byte 16 16) value)))
	    (setf (sap-ref-16 sap 2)
		  (maybe-byte-swap-short
		   (if (logbitp 15 value) (1+ high) high)))))
	 (:ba
	  (unless (zerop (ash value -24))
	    (warn "#x~8,'0X out of range for branch-absolute." value))
	  (let ((inst (maybe-byte-swap-short (sap-ref-16 sap 0))))
	    (setf (sap-ref-16 sap 0)
		  (maybe-byte-swap-short
		   (dpb (ldb (byte 8 16) value)
			(byte 8 0)
			inst))))
	  (setf (sap-ref-16 sap 2)
		(maybe-byte-swap-short (ldb (byte 16 0) value))))))
      ((#.c:x86-fasl-file-implementation
	#.c:amd64-fasl-file-implementation) ;; I don't know if this right
       (let* ((disp (logior (sap-ref-8 sap 0)
			    (ash (sap-ref-8 sap 1) 8)
			    (ash (sap-ref-8 sap 2) 16)
			    (ash (sap-ref-8 sap 3) 24)))
	      (obj-start-addr (logandc2 (descriptor-bits code-object)
					vm:lowtag-mask)))
	 (ecase kind
	   (:absolute
	    ;; fix this for 64 bit addresses
	    (let ((new-value (+ value disp)))
	      (setf (sap-ref-8 sap 0) (ldb (byte 8 0) new-value))
	      (setf (sap-ref-8 sap 1) (ldb (byte 8 8) new-value))
	      (setf (sap-ref-8 sap 2) (ldb (byte 8 16) new-value))
	      (setf (sap-ref-8 sap 3) (ldb (byte 8 24) new-value))
	      ;; Note absolute fixups that point within the object.
	      (unless (< new-value obj-start-addr)
		(note-load-time-code-fixup code-object after-header value
					   kind))))
	   (:relative
	    (let ((new-value (- (+ value disp)
				obj-start-addr offset 4)))
	      (setf (sap-ref-8 sap 0) (ldb (byte 8 0) new-value))
	      (setf (sap-ref-8 sap 1) (ldb (byte 8 8) new-value))
	      (setf (sap-ref-8 sap 2) (ldb (byte 8 16) new-value))
	      (setf (sap-ref-8 sap 3) (ldb (byte 8 24) new-value))
	      ;; Note relative fixups that point outside the code object.
	      (note-load-time-code-fixup code-object after-header value
					 kind))))))
      (#.c:hppa-fasl-file-implementation
       (let ((inst (maybe-byte-swap (sap-ref-32 sap 0))))
	 (setf (sap-ref-32 sap 0)
	       (maybe-byte-swap
		(ecase kind
		  (:load
		   (logior (ash (ldb (byte 11 0) value) 1)
			   (logand inst #xffffc000)))
		  (:load-short
		   (let ((low-bits (ldb (byte 11 0) value)))
		     (assert (<= 0 low-bits (1- (ash 1 4))))
		     (logior (ash low-bits 17)
			     (logand inst #xffe0ffff))))
		  (:hi
		   (logior (ash (ldb (byte 5 13) value) 16)
			   (ash (ldb (byte 2 18) value) 14)
			   (ash (ldb (byte 2 11) value) 12)
			   (ash (ldb (byte 11 20) value) 1)
			   (ldb (byte 1 31) value)
			   (logand inst #xffe00000)))
		  (:branch
		   (let ((bits (ldb (byte 9 2) value)))
		     (assert (zerop (ldb (byte 2 0) value)))
		     (logior (ash bits 3)
			     (logand inst #xffe0e002)))))))))
      (#.c:alpha-fasl-file-implementation
       (ecase kind
	 (:jmp-hint
	  (assert (zerop (ldb (byte 2 0) value)))
	  #+nil
	  (setf (sap-ref-16 sap 0)
		(logior (sap-ref-16 sap 0) (ldb (byte 14 0) (ash value -2)))))
	 (:bits-63-48
	  (let* ((value (if (logbitp 15 value) (+ value (ash 1 16)) value))
		 (value (if (logbitp 31 value) (+ value (ash 1 32)) value))
		 (value (if (logbitp 47 value) (+ value (ash 1 48)) value)))
	    (setf (sap-ref-8 sap 0) (ldb (byte 8 48) value))
	    (setf (sap-ref-8 sap 1) (ldb (byte 8 56) value))))
	 (:bits-47-32
	  (let* ((value (if (logbitp 15 value) (+ value (ash 1 16)) value))
		 (value (if (logbitp 31 value) (+ value (ash 1 32)) value)))
	    (setf (sap-ref-8 sap 0) (ldb (byte 8 32) value))
	    (setf (sap-ref-8 sap 1) (ldb (byte 8 40) value))))
	 (:ldah
	  (let ((value (if (logbitp 15 value) (+ value (ash 1 16)) value)))
	    (setf (sap-ref-8 sap 0) (ldb (byte 8 16) value))
	    (setf (sap-ref-8 sap 1) (ldb (byte 8 24) value))))
	 (:lda
	  (setf (sap-ref-8 sap 0) (ldb (byte 8 0) value))
	  (setf (sap-ref-8 sap 1) (ldb (byte 8 8) value)))))
      (#.c:sgi-fasl-file-implementation
       (ecase kind
	 (:jump
	  ;; emarsden2002-03-05 (assert (zerop (ash value -28)))
	  (let ((inst (maybe-byte-swap (sap-ref-32 sap 0))))
	    (setf (ldb (byte 26 0) inst) (ash value -2))
	    (setf (sap-ref-32 sap 0) (maybe-byte-swap inst))))
	 (:lui
	  (setf (sap-ref-16 sap 2)
		(maybe-byte-swap-short
		 (+ (ash value -16)
		    (if (logbitp 15 value) 1 0)))))
	 (:addi
	  (setf (sap-ref-16 sap 2)
		(maybe-byte-swap-short
		 (ldb (byte 16 0) value))))))
      (#.c:ppc-fasl-file-implementation
       (ecase kind
	 (:ba
 	  (setf (sap-ref-32 sap 0)
 		(maybe-byte-swap
		 (dpb (ash value -2) (byte 24 2)
		      (maybe-byte-swap (sap-ref-32 sap 0))))))
 	 (:ha
 	  (let* ((h (ldb (byte 16 16) value))
 		 (l (ldb (byte 16 0) value)))
 	    (setf (sap-ref-16 sap 2)
 		  (maybe-byte-swap-short
		   ;; This takes care of the fact that the low part is
		   ;; signed extended by addi when we add in the low
		   ;; part to the high part.
		   (if (logbitp 15 l) (ldb (byte 16 0) (1+ h)) h)))))
 	 (:l
  	  (setf (sap-ref-16 sap 2)
	        (maybe-byte-swap-short
  		 (ldb (byte 16 0) value))))))))
  (undefined-value))

(defun linkage-info-to-core ()
  (let ((result *nil-descriptor*))
    (maphash #'(lambda (symbol value)
		 (cold-push (allocate-cons *dynamic*
					   (string-to-core symbol)
					   (number-to-core value))
			    result))
	     *cold-foreign-symbol-table*)
    (cold-setq (cold-intern '*initial-foreign-symbols*) result))
  (let ((result *nil-descriptor*))
    (dolist (rtn *cold-assembler-routines*)
      (cold-push (allocate-cons *dynamic*
				(cold-intern (car rtn))
				(number-to-core (cdr rtn)))
		 result))
    (cold-setq (cold-intern '*initial-assembler-routines*) result))
  (cold-setq (cold-intern '*linkage-table-data*)
	     (make-cold-linkage-vector *cold-linkage-table*)))



;;;; Emit C Header.

(defun tail-comp (string tail)
  (and (>= (length string) (length tail))
       (string= string tail :start1 (- (length string) (length tail)))))

(defun head-comp (string head)
  (and (>= (length string) (length head))
       (string= string head :end1 (length head))))

(defun emit-c-header-aux ()
  (format t "/*~% * Machine generated header file.  Do not edit.~% */~2%")
  (format t "#ifndef _INTERNALS_H_~%#define _INTERNALS_H_~2%")
  ;; Write out various constants
  (let ((constants nil))
    (do-external-symbols (symbol (find-package "VM"))
      (when (constantp symbol)
	(let ((name (symbol-name symbol)))
	  (labels
	      ((record (prefix string priority)
		 (push (list (concatenate
			      'simple-string
			      prefix
			      (delete #\- (string-capitalize string)))
			     priority
			     (symbol-value symbol)
			     (documentation symbol 'variable))
		       constants))
	       (test-tail (tail prefix priority)
		 (when (tail-comp name tail)
		   (record prefix
			   (let* ((len (- (length name)
					  (length tail)))
				  (result (make-string len)))
			     ;; (subseq name 0 len) but subseq is broken somehow.
			     (do ((i 0 (1+ i)))
				 ((= i len) result)
			       (setf (aref result i) (aref name i))))
			   priority)))
	       (test-head (head prefix priority)
		 (when (head-comp name head)
		   (record prefix
			   ;; (subseq name (length head)) but subseq is broken
			   (let* ((head-len (length head))
				  (len (- (length name) head-len))
				  (result (make-string len)))
			     (do ((i 0 (1+ i)))
				 ((= i len) result)
			       (setf (aref result i) (aref name (+ i head-len)))))
			   priority))))
	    (test-tail "-TYPE" "type_" 0)
	    (test-tail "-FLAG" "flag_" 1)
	    (test-tail "-TRAP" "trap_" 2)
	    (test-tail "-SUBTYPE" "subtype_" 3)
	    (test-head "TRACE-TABLE-" "tracetab_" 4)
	    (test-tail "-SC-NUMBER" "sc_" 5)
	    (test-head "TARGET-FOREIGN" "" 6)
	    (test-head "PSEUDO-ATOMIC-" "pseudo_atomic_" 7)
	    (test-head "LOWTAG-" "lowtag_" 8)
	    (test-head "TYPE-" "type_" 9)
	    (test-tail "-SPACE-START" "SpaceStart_" 10)))))
    (setf constants
	  (sort constants
		#'(lambda (const1 const2)
		    (if (= (second const1) (second const2))
			(< (third const1) (third const2))
			(< (second const1) (second const2))))))
    (let ((prev-priority (second (car constants))))
      (dolist (const constants)
	(unless (= prev-priority (second const))
	  (terpri)
	  (setf prev-priority (second const)))
	(if (search "SpaceStart" (first const))
	    (format t "#define ~A 0x~XU~@[  /* ~A */~]~%"
		    (first const) (third const) (fourth const))
	    (format t "#define ~A ~D~@[  /* ~A */~]~%"
		    (first const) (third const) (fourth const))))))

  ;; Write out internal error codes and error descriptions
  (terpri)
  (loop for (error-name . rest) across (c:backend-internal-errors c:*backend*)
        for error-code from 0
	when error-name
        do
        (format t "#define ~A ~D~%"
	        (substitute #\_ #\- (symbol-name error-name))
		error-code))
  (terpri)
  (format t "#define ERRORS { \\~%")
  (loop for info across (c:backend-internal-errors c:*backend*)
        do (format t "    ~S, \\~%" (cdr info)))
  (format t "    NULL \\~%}~%")

  ;; Write out primitive object layouts
  (terpri)
  (let ((structs (sort (copy-list vm:*primitive-objects*) #'string<
		       :key #'(lambda (obj)
				(symbol-name
				 (vm:primitive-object-name obj))))))
    (format t "#ifndef LANGUAGE_ASSEMBLY~2%")
    (format t "#define LISPOBJ(x) ((lispobj)x)~2%")
    (dolist (obj structs)
      (format t "struct ~A {~%"
	      (nsubstitute #\_ #\-
			   (string-downcase
			    (string (vm:primitive-object-name obj)))))
      (when (vm:primitive-object-header obj)
	(format t "    lispobj header;~%"))
      (dolist (slot (vm:primitive-object-slots obj))
	(format t "    ~A ~A~@[[1]~];~%"
		(getf (vm:slot-options slot) :c-type "lispobj")
		(nsubstitute #\_ #\-
			     (string-downcase (string (vm:slot-name slot))))
		(vm:slot-rest-p slot)))
      (format t "};~2%"))
    (format t "#else /* LANGUAGE_ASSEMBLY */~2%")
    (format t "#define LISPOBJ(thing) thing~2%")
    (dolist (obj structs)
      (let ((name (vm:primitive-object-name obj))
	    (lowtag (eval (vm:primitive-object-lowtag obj))))
	(when lowtag
	  (dolist (slot (vm:primitive-object-slots obj))
	    (format t "#define ~A_~A_OFFSET ~D~%"
		    (substitute #\_ #\- (string name))
		    (substitute #\_ #\- (string (vm:slot-name slot)))
		    (- (* (vm:slot-offset slot) vm:word-bytes) lowtag)))
	  (terpri))))
    (format t "#endif /* LANGUAGE_ASSEMBLY */~2%"))
  ;;
  ;; Write out static symbols
  (dolist (symbol (cons nil vm:static-symbols))
    (format t "#define ~A LISPOBJ(0x~X)~%"
	    (nsubstitute #\_ #\-
			 (remove-if #'(lambda (char)
					(member char '(#\% #\* #\.)))
				    (symbol-name symbol)))
	    (if *static*
		;; We actually ran Genesis, use the real value.
		(descriptor-bits (cold-intern symbol))
		;; We didn't run Genesis, so guess at the address.
		(+ vm:target-static-space-start
		   vm:word-bytes
		   vm:other-pointer-type
		   (if symbol (vm:static-symbol-offset symbol) 0)))))
  ;;
  ;; Write out features.
  (format t "~%/* Runtime features when built. */~2%")
  (dolist (feature sys:*runtime-features*)
    (format t "#define FEATURE_~a 1~%"
	    (nsubstitute #\_ #\-
			 (remove-if #'(lambda (char)
					(member char '(#\% #\* #\.)))
				    (symbol-name feature)))))
  ;;
  (format t "~%#endif~%"))

;;; FILES-DIFFER --- internal
;;;
;;; Return T iff the two files differ.
;;;
(defun files-differ (name1 name2)
  (with-open-file (file1 name1)
    (with-open-file (file2 name2)
      (or (not (= (file-length file1)
		  (file-length file2)))
	  (do ((line1 "foo" (read-line file1 nil nil))
	       (line2 "foo" (read-line file2 nil nil)))
	      ((and (null line1) (null line2)) nil)
	    (when (or (null line1)
		      (null line2)
		      (string/= line1 line2))
	      (return t)))))))

(defun emit-c-header (name)
  (let* ((new-name (concatenate 'string (namestring name) ".NEW"))
	 (unix-newname (unix-namestring new-name nil)))
    (with-open-file
	(*standard-output* new-name
			   :direction :output
			   :if-exists :supersede)
      (emit-c-header-aux))
    (cond ((not (probe-file name))
	   (unix:unix-chmod unix-newname #o444)
	   (rename-file new-name name)
	   (warn 'genesis-c-header-file-changed :name name))
	  ((files-differ name new-name)
	   (rename-file name
			(concatenate 'simple-string
				     (namestring name)
				     ".OLD"))
	   (unix:unix-chmod unix-newname #o444)
	   (rename-file new-name name)
	   (warn 'genesis-c-header-file-changed :name name))
	  (t
	   (unix:unix-unlink unix-newname))))
  (undefined-value))

(defun emit-makefile-header (name)
  (with-open-file (*standard-output* name :direction :output
				     :if-exists :supersede)
    ;;
    ;; Write out features.
    (dolist (feature (c:backend-features c:*backend*))
      (format t "FEATURE_~a = 1~%"
	      (substitute #\_ #\- (symbol-name feature))))))


;;;; The actual genesis function.

(defvar *genesis-core-name* "lisp.core")
(defvar *genesis-map-name* nil)
(defvar *genesis-c-header-name* t)
(defvar *genesis-symbol-table* nil)

#+linkage-table
(defun symbol-table-version (filename)
  "Return the version number from the symbol table file Filename.
  Value is 0 if Filename is null or the file doesn't exist."
  (if (and filename (probe-file filename))
      (with-open-file (file filename)
	(let* ((version-line (read-line file))
	       (last-space (position #\Space version-line :from-end t)))
	  (parse-integer version-line :start (1+ last-space))))
      0))

(defun genesis (file-list &optional
			  (symbol-table *genesis-symbol-table*)
			  (core-name *genesis-core-name*)
			  (map-name *genesis-map-name*)
			  (header-name *genesis-c-header-name*))
  "Builds a kernel Lisp image from the .FASL files specified in the given
  File-List and writes it to a file named by Core-Name."
  (unless (or #+linkage-table t symbol-table)
    (error "Can't genesis without a symbol-table."))
  (format t "~&Building ~S for the ~A~%"
	  core-name (c:backend-version c:*backend*))
  (setq *current-init-functions-cons* *nil-descriptor*)
  (let ((*load-time-value-counter* 0)
	*static* *dynamic* *read-only* *cold-assembler-routines*
	*cold-assembler-fixups* *load-time-code-fixups*)
    (unwind-protect
	(progn
	  (clrhash *fdefn-objects*)
	  (clrhash *cold-symbols*)
	  #-linkage-table (init-foreign-symbol-table)
	  #+linkage-table (init-foreign-linkage)
	  (let ((version #-linkage-table (load-foreign-symbol-table
					  symbol-table)
			 #+linkage-table (symbol-table-version
					  symbol-table)))
	    (initialize-spaces)
	    (initialize-symbols)
	    (initialize-layouts)
	    (setf *current-init-functions-cons* *nil-descriptor*)
	    (initialize-static-fns)
	    (dolist (file (if (listp file-list)
			      file-list
			      (list file-list)))
	      (let ((file (truename
			   (merge-pathnames file
					    (make-pathname
					     :type
					     (c:backend-fasl-file-type
					      c:*backend*))))))
		(write-line (namestring file))
		(cold-load file))
	      (maybe-gc))
	    (resolve-assembler-fixups)
	    (when (or (c:backend-featurep :x86) (c:backend-featurep :amd64))
	      (output-load-time-code-fixups))
	    (linkage-info-to-core)
	    (finish-symbols)
	    (finalize-load-time-value-noise)
	    (when map-name
	      (with-open-file
		  (*standard-output* (if (eq map-name t)
					 (make-pathname
					  :type "map"
					  :defaults core-name)
					 (merge-pathnames
					  map-name
					  (make-pathname :type "map"
							 :defaults core-name)))
				     :direction :output
				     :if-exists :supersede)
		(write-map-file)))
	    (when header-name
	      (emit-c-header
	       (merge-pathnames (if (eq header-name t)
				    "internals.h"
				    (merge-pathnames
				     header-name
				     (make-pathname :type "h")))
				core-name))
	      (emit-makefile-header
	       (merge-pathnames (if (eq header-name t)
				    "internals.inc"
				    (merge-pathnames
				     (make-pathname :type "inc")
				     header-name))
				core-name)))
	    (write-initial-core-file
	     core-name version
	     (let ((fdefn (cold-fdefinition-object
			   (cold-intern '%initial-function))))
	       (read-indexed fdefn vm:fdefn-function-slot)))))
      (dolist (space (list *static* *dynamic* *read-only*))
	(when space
	  (deallocate-space space))))))


(defun write-map-file ()
  (let ((*print-pretty* nil)
	(*print-case* :upcase))
    (format t "Assembler routines defined in core image:~2%")
    (dolist (routine (sort (copy-list *cold-assembler-routines*) #'<
			   :key #'cdr))
      (format t "#x~8,'0X: ~S~%" (cdr routine) (car routine)))
    (let ((funs nil) (undefs nil))
      (maphash #'(lambda (name fdefn)
		   (let ((fun (read-indexed fdefn vm:fdefn-function-slot)))
		     (if (= (descriptor-bits fun)
			    (descriptor-bits *nil-descriptor*))
			 (push name undefs)
			 (let ((addr (read-indexed fdefn
						   vm:fdefn-raw-addr-slot)))
			   (push (cons name (descriptor-bits addr))
				 funs)))))
	       *fdefn-objects*)
      (format t "~%~|~%Initially defined functions:~2%")
      (dolist (info (sort funs #'< :key #'cdr))
	(format t "0x~8,'0X: ~S   #x~8,'0X~%" (cdr info) (car info) 
		(- (cdr info) #x17)))
      (format t "~%~|~%Undefined function references:~2%")
      (labels ((key (name)
		 (etypecase name
		   (symbol (symbol-name name))
		   (integer (prin1-to-string name))
		   (list (key (second name))))))
	(dolist (fun (sort undefs #'string< :key #'key))
	  (if (integerp fun)
	      (format t "~8,'0X~%" fun)
	      (format t "~S~%" fun))))))
  
  (format t "~%~|~%Layout names:~2%")
  (collect ((stuff))
    (maphash #'(lambda (name gorp)
		 (declare (ignore name))
		 (stuff (cons (descriptor-bits (car gorp))
			      (cdr gorp))))
	     *cold-layouts*)
    (dolist (x (sort (stuff) #'< :key #'car))
      (apply #'format t "~8,'0X: ~S[~D]~%~10T~S~%" x)))

  (undefined-value))


;;;; Core file writing magic.

(defvar *core-file* nil)
(defvar *data-page* 0)

(defparameter version-entry-type-code 3860)
(defparameter validate-entry-type-code 3845)
(defparameter directory-entry-type-code 3841)
(defparameter new-directory-entry-type-code 3861)
(defparameter initial-function-entry-type-code 3863)
(defparameter end-entry-type-code 3840)

(defun write-long (num)
  (ecase (c:backend-byte-order c:*backend*)
    (:little-endian
     (dotimes (i 4)
       (write-byte (ldb (byte 8 (* i 8)) num) *core-file*)))
    (:big-endian
     (dotimes (i 4)
       (write-byte (ldb (byte 8 (* (- 3 i) 8)) num) *core-file*)))))


(defun advance-to-page ()
  (force-output *core-file*)
  (file-position *core-file*
		 (round-up (file-position *core-file*)
			   (c:backend-page-size c:*backend*))))

(defun output-space (space)
  (force-output *core-file*)
  (let* ((posn (file-position *core-file*))
	 (bytes (* (space-free-pointer space) vm:word-bytes))
	 (pages (ceiling bytes (c:backend-page-size c:*backend*)))
	 (total-bytes (* pages (c:backend-page-size c:*backend*))))
    ;; 
    (file-position *core-file* (* (c:backend-page-size c:*backend*)
				  (1+ *data-page*)))
    (format t "Writing ~S byte~:P [~S page~:P] from ~S space~%"
	    total-bytes pages (space-name space))
    (force-output)
    ;;
    ;; Note: It is assumed that the space allocation routines always
    ;; allocate whole pages (of size *target-page-size*) and that any empty
    ;; space between the free pointer and the end of page will be
    ;; zero-filled.  This will always be true under Mach on machines
    ;; where the page size is equal.  (RT is 4K, PMAX is 4K, Sun 3 is 8K).
    ;; 
    (system:output-raw-bytes *core-file* (space-sap space) 0 total-bytes)
    (force-output *core-file*)
    (file-position *core-file* posn)
    ;; 
    ;; Write part of a (new) directory entry which looks like this:
    ;;
    ;; SPACE IDENTIFIER
    ;; WORD COUNT
    ;; DATA PAGE
    ;; ADDRESS
    ;; PAGE COUNT
    ;; 
    (write-long (space-identifier space))
    (write-long (space-free-pointer space))
    (write-long *data-page*)
    (write-long (/ (ash (space-address space) vm:word-shift)
		   (c:backend-page-size c:*backend*)))
    (write-long pages)
    (incf *data-page* pages)))

(defun write-initial-core-file (name version initial-function)
  (format t "[Building Initial Core File (version ~D) in file ~S: ~%"
	  version (namestring name))
  (force-output)
  (let ((*data-page* 0))
    (with-open-file (*core-file* name
				 :direction :output
				 :element-type '(unsigned-byte 8)
				 :if-exists :rename-and-delete)
      ;; Write the magic number
      ;; 
      (write-long (logior (ash (char-code #\C) 24)
			  (ash (char-code #\O) 16)
			  (ash (char-code #\R) 8)
			  (char-code #\E)))
      
      ;; Write the Version entry.
      ;; 
      (write-long version-entry-type-code)
      (write-long 3)
      (write-long version)

      ;; Write the New Directory entry header.
      ;; 
      (write-long new-directory-entry-type-code)
      (write-long 17) ; length = 5 words / space * 3 spaces + 2 for header.
      
      (output-space *read-only*)
      (output-space *static*)
      (output-space *dynamic*)
      
      (write-long initial-function-entry-type-code)
      (write-long 3)
      (write-long (descriptor-bits initial-function))

      ;; Write the End entry.
      ;; 
      (write-long end-entry-type-code)
      (write-long 2)))
  (format t "done]~%")
  (force-output))
