;;; -*- Package: Lisp -*-
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/gengc-genesis.lisp,v 1.15 1994/10/31 04:38:06 ram Exp $")
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

(defmacro round-up (num size)
  "Rounds number up to be an integral multiple of size."
  (let ((size-var (gensym)))
    `(let ((,size-var ,size))
       (* ,size-var (ceiling ,num ,size-var)))))


;;;; Addresses

(deftype address ()
  `(unsigned-byte ,vm:word-bits))

(defconstant null-address 0)



;;;; Block allocator.

(defconstant block-shift 16)
(defconstant block-bytes (ash 1 block-shift))
(defconstant block-words (truncate block-bytes vm:word-bytes))

(defparameter *heap-start* #x01000000)
(defvar *heap-free-pointer*)
(defvar *allocations*)

(defun allocate-blocks (n)
  (declare (type index n)
	   (values address system-area-pointer))
  (let* ((free-ptr *heap-free-pointer*)
	 (bytes (* n block-bytes))
	 (sap (system:allocate-system-memory bytes)))
    (setf *heap-free-pointer* (+ free-ptr bytes))
    (push (cons sap bytes) *allocations*)
    (values free-ptr sap)))

(defun free-allocations ()
  (dolist (allocation *allocations*)
    (system:deallocate-system-memory (car allocation) (cdr allocation))))

(defmacro with-allocation-pool (&body body)
  `(let ((*heap-free-pointer* *heap-start*)
	 (*allocations* nil))
     (unwind-protect
	 (progn
	   ,@body)
       (free-allocations))))

(defvar *block-owners*)

(defun set-address-owner (address blocks owner)
  (let* ((start (truncate (- address *heap-start*) block-bytes))
	 (end (+ start blocks))
	 (owners *block-owners*)
	 (length (length owners)))
    (when (< length end)
      (do ((new-length length (ash new-length 2)))
	  ((<= end new-length)
	   (setf owners
		 (adjust-array owners new-length :initial-element nil))
	   (setf *block-owners* owners))))
    (do ((index start (1+ index)))
	((= index end))
      (assert (null (aref owners index)))
      (setf (aref owners index) owner))))

(defun address-owner (address)
  (declare (type address address))
  (let ((index (truncate (- address *heap-start*) block-bytes))
	(owners *block-owners*))
    (if (and (<= 0 index) (< index (length owners)))
	(aref owners index)
	nil)))


;;;; Generation representation.

(defconstant gen-default-policy 0)
(defconstant gen-tenure-policy 1)

(defstruct (generation
	    (:constructor make-generation (num policy))
	    (:print-function %print-generation))
  ;;
  ;; The generation number for this generation.  0 is the youngest.
  (num 0 :type index)
  ;;
  ;; The policy number for this generation.
  (policy 0 :type index)
  ;;
  ;; List of all the steps allocated for this generation.
  (steps nil :type list)
  ;;
  ;; Hash table holding all addresses that (might) hold pointers to younger
  ;; generations.
  (remset (make-hash-table :test #'eql)
	  :type hash-table))

(defun %print-generation (gen stream depth)
  (declare (ignore depth))
  (print-unreadable-object (gen stream :type t)
    (format stream "~D" (generation-num gen))))

(defun add-remset-entry (generation address)
  (declare (type generation generation) (type address address))
  (setf (gethash address (generation-remset generation)) t))



;;;; Step representation.

(defstruct (block
	    (:print-function %print-block))
  ;;
  ;; The step this block belongs to.
  (step (required-argument) :type step)
  ;;
  ;; The address this block starts at.
  (base 0 :type address)
  ;;
  ;; Pointer to the memory actually backing this block.
  (sap (required-argument) :type system-area-pointer)
  ;;
  ;; Offset (in bytes) into the block were the next allocation goes.
  (free 0 :type index)
  ;;
  ;; Number of words remaining in this block.
  (remaining block-words :type index)
  ;;
  ;; The next block allocated to this step.
  (next nil :type (or null block)))

(defun %print-block (block stream depth)
  (declare (ignore depth))
  (print-unreadable-object (block stream :type t)
    (format stream "at #x~8,'0X" (block-base block))))

(defconstant step-default-policy 0)
(defconstant step-tenure-policy 1)

(defstruct (step
	    (:constructor %make-step (num gen prom-step max-blocks policy))
	    (:print-function %print-step))
  ;;
  ;; The number for this step.
  (num 0 :type index)
  ;;
  ;; The generation this step is part of.
  (gen (required-argument) :type generation)
  ;;
  ;; The step (number) to promote into.  Just passed on through to the initial
  ;; core file.
  (prom-step 0 :type index)
  ;;
  ;; The max number of blocks (just a hint).  Also just passed on through.
  (max-blocks 0 :type index)
  ;;
  ;; The policy number for this step.
  (policy 0 :type index)
  ;;
  ;; Chain of blocks allocated to this step.
  (first-block nil :type (or null block))
  (block nil :type (or null block))
  ;;
  ;; Address of the first and last large object in this step.
  (first-LO null-address :type address)
  (last-LO null-address :type address)
  ;;
  ;; SAP pointing to the region_header for the last large object in this step.
  (last-LO-sap nil :type (or null system-area-pointer)))

(defun %print-step (step stream depth)
  (declare (ignore depth))
  (print-unreadable-object (step stream :type t)
    (format stream "~D" (step-num step))))

(defun make-step (num gen prom-step max-blocks policy)
  (let ((step (%make-step num gen prom-step max-blocks policy)))
    (push step (generation-steps gen))
    step))

(defun alloc-small-object (words step)
  (let ((padded (logandc2 (1+ words) 1))
	(block (step-block step)))
    (assert (<= padded block-words))
    (when (or (null block) (< (block-remaining block) padded))
      (multiple-value-bind
	  (addr sap)
	  (allocate-blocks 1)
	(let ((new (make-block :step step :base addr :sap sap)))
	  (set-address-owner addr 1 new)
	  (setf (step-block step) new)
	  (if block
	      (setf (block-next block) new)
	      (setf (step-first-block step) new))
	  (setf block new))))
    (let ((offset (block-free block)))
      (setf (block-free block) (+ offset (* padded vm:word-bytes)))
      (decf (block-remaining block) padded)
      (values (+ (block-base block) offset)
	      (sap+ (block-sap block) offset)))))


;;;; Generation/Step setup.

(defvar *initial-generation-setup*
  '((:policy gen-default-policy
     :steps ((:step 0 :max-blocks 1 :policy step-default-policy :prom-step 1)))
    (:policy gen-default-policy
     :steps ((:step 1 :is :dynamic :max-blocks 64
	      :policy step-default-policy :prom-step 1)))
    (:policy gen-tenure-policy
     :steps ((:step 2 :is :static :policy step-tenure-policy :prom-step 2)))))

(defvar *static*)
(defvar *dynamic*)


(defun make-generations ()
  (loop
    for gen-num from 0
    for info in *initial-generation-setup*
    collect
    (destructuring-bind (&key policy steps) info
      (let ((gen (make-generation gen-num (eval policy))))
	(dolist (step-info steps)
	  (destructuring-bind
	      (&key step is (max-blocks 0) policy prom-step)
	      step-info
	    (let ((step
		   (make-step step gen prom-step max-blocks (eval policy))))
	      (case is
		(:static (setf *static* step))
		(:dynamic (setf *dynamic* step))))))
	gen))))


;;;; Large object stuff

(defstruct (cluster
	    (:print-function %print-cluster))
  ;;
  ;; Number of blocks in this cluster.
  (blocks 0 :type index)
  ;;
  ;; Address of the start of this cluster.
  (base (required-argument) :type address)
  ;;
  ;; Pointer to the memory backing this cluster.
  (sap (required-argument) :type system-area-pointer)
  ;;
  ;; Address of the first header in this cluster.
  (first (required-argument) :type address)
  ;;
  ;; Offset (in bytes) into the cluster were the next allocation goes.
  (free 0 :type index)
  ;;
  ;; Number of words remaining in this cluster.
  (remaining 0 :type index)
  ;;
  ;; The next cluster in the chain of clusters.
  (next nil :type (or null cluster)))

(defun %print-cluster (cluster stream depth)
  (declare (ignore depth))
  (print-unreadable-object (cluster stream :type t)
    (format stream "of ~D blocks at #x~8,'0X"
	    (cluster-blocks cluster)
	    (cluster-base cluster))))

(defvar *current-los-cluster*)

;;; LOS-OBJECT-WORD-OVERHEAD -- number of words extra we have to pre-pend
;;; to each large object.
;;; 
(defconstant los-object-word-overhead 6)

;;; LOS-HEADER-mumble-OFFSET -- byte offset of the various slots in the
;;; large-object header.
;;; 
(defconstant los-header-longs-offset 0)
(defconstant los-header-step-offset 4)
(defconstant los-header-prev-offset 8)
(defconstant los-header-next-offset 12)

;;; LOS-OBJECT-MINIMUM-SIZE -- minimum number of words that a large object
;;; can be.  Objects smaller then this must be padded out to this.  Includes
;;; the los-object-word-overhead.
;;; 
(defconstant los-object-minimum-size 128)

;;; LOS-REGION-ALIGNMENT -- word alignment for regions (object + overhead) in
;;; the large object store.
;;;
(defconstant los-region-alignment 2)

;;; MIN-CLUSTER-BLOCKS -- minimum number of blocks to allocate per cluster.
;;; 
(defconstant min-cluster-blocks 4)

;;; LOS-CLUSTER-BYTE-OVERHEAD -- number of bytes needs for the cluster
;;; header.
;;;
(defconstant los-cluster-byte-overhead (* vm:word-bytes 3))

;;; LOS-CHUNK-BYTES -- number of bytes per los chunk.
;;;
(defconstant los-chunk-bytes (ash 1 9))

;;; LOS-CHUNK-BYTE-OVERHEAD -- number of bytes overhead needed in the
;;; cluster per chuck.
;;;
(defconstant los-chunk-byte-overhead 1)

;;; CLUSTER-START-ALIGNMENT -- Alignment restriction for the first object
;;; in the cluster.  Must be at least the largest page size for any system.
;;;
(defconstant cluster-start-alignment (ash 1 13))

(defun alloc-large-object (words step)
  (declare (type index words)
	   (values address system-area-pointer))
  (let ((padded (max (round-up (+ los-object-word-overhead words)
			       los-region-alignment)
		     los-object-minimum-size))
	(cluster *current-los-cluster*))
    (when (or (null cluster) (> padded (cluster-remaining cluster)))
      ;; Figure out how many blocks to actually allocate.
      (iterate repeat
	       ((blocks (max (ceiling padded block-words) min-cluster-blocks)))
	(let* ((bytes (* blocks block-bytes))
	       (chunks (ceiling bytes los-chunk-bytes))
	       (overhead (round-up (+ los-cluster-byte-overhead
				      (* chunks los-chunk-byte-overhead))
				   cluster-start-alignment)))
	  (if (> (- bytes overhead) (* padded vm:word-bytes))
	      (multiple-value-bind
		  (addr sap)
		  (allocate-blocks blocks)
		(setf cluster
		      (make-cluster :blocks blocks :base addr :sap sap
				    :first (+ addr overhead)
				    :free overhead
				    :remaining
				    (truncate (- bytes overhead) vm:word-bytes)
				    :next cluster))
		(set-address-owner addr blocks cluster)
		(setf *current-los-cluster* cluster))
	      (repeat (1+ blocks))))))
    (let* ((offset (cluster-free cluster))
	   (addr (+ (cluster-base cluster) offset))
	   (sap (sap+ (cluster-sap cluster) offset)))
      (setf (cluster-free cluster) (+ offset (* padded vm:word-bytes)))
      (decf (cluster-remaining cluster) padded)
      ;; Fill in the length.
      (setf (sap-ref-32 sap los-header-longs-offset)
	    (maybe-byte-swap padded))
      ;; Fill in the step with some random non-zero value.
      (setf (sap-ref-32 sap los-header-step-offset)
	    (maybe-byte-swap 1))
      ;; Grab the previous large object in this step.
      (let ((last (step-last-LO-sap step)))
	(cond (last
	       ;; Set our prev link to point to him.
	       (setf (sap-ref-32 sap los-header-prev-offset)
		     (maybe-byte-swap (step-last-LO step)))
	       ;; And his next link to point to us.
	       (setf (sap-ref-32 last los-header-next-offset)
		     (maybe-byte-swap addr)))
	      (t
	       ;; Set out prev link to NULL.
	       (setf (sap-ref-32 sap los-header-prev-offset)
		     (maybe-byte-swap null-address))
	       ;; And record the fact that we are the first.
	       (setf (step-first-LO step) addr))))
      ;; Set our next link to NULL.
      (setf (sap-ref-32 sap los-header-next-offset)
	    (maybe-byte-swap null-address))
      ;; Record us as the last LO in this step.
      (setf (step-last-LO step) addr)
      (setf (step-last-LO-sap step) sap)
      ;; Return pointers just past the header.
      (values (+ addr (* los-object-word-overhead vm:word-bytes))
	      (sap+ sap (* los-object-word-overhead vm:word-bytes))))))


;;;; Descriptors.

(defstruct (descriptor
	    (:constructor make-random-descriptor (bits))
	    (:constructor make-pointer-descriptor (bits %sap step))
	    (:print-function %print-descriptor))
  ;;
  ;; The actual bits of this descriptor.
  (bits (required-argument) :type address)
  ;;
  ;; A sap pointing to the memory backing this object if it is heap allocated,
  ;; or :LOAD-TIME-COOKIE if references to this descriptor need to be fixed
  ;; up a load time.  NIL until known.
  (%sap nil :type (or null system-area-pointer (member :load-time-cookie)))
  ;;
  ;; The step this object is part of, NIL if unknown or immediate.
  (step nil :type (or null step)))

(defun %print-descriptor (des stream depth)
  (declare (ignore depth))
  (let ((bits (descriptor-bits des))
	(sap (descriptor-%sap des)))
    (if (eq sap :load-time-cookie)
	(print-unreadable-object (des stream)
	  (format stream "load-time-cookie ~D" bits))
	(if (logbitp 0 bits)
	    (print-unreadable-object (des stream)
	      (format stream "pointer: #x~8,'0X" bits))
	    (if (logbitp 1 bits)
		(print-unreadable-object (des stream)
		  (format stream "other immediate: #x~X, type #b~8,'0B"
			  (ash bits (- vm:type-bits))
			  (logand bits vm:type-mask)))
		(print-unreadable-object (des stream)
		  (format stream "fixnum: ~D"
			  (let ((unsigned (ash bits -2)))
			    (if (logbitp 29 unsigned)
				(logior (ash -1 30) unsigned)
				unsigned)))))))))

(defun descriptor-sap (des)
  (declare (type descriptor des))
  (let ((sap (descriptor-%sap des)))
    (case sap
      ((nil)
       (let* ((addr (logandc2 (descriptor-bits des) vm:lowtag-mask))
	      (owner (address-owner addr)))
	 (multiple-value-bind
	     (base base-sap step)
	     (etypecase owner
	       (block
		(values (block-base owner)
			(block-sap owner)
			(block-step owner)))
	       (cluster
		(values (cluster-base owner)
			(cluster-sap owner)
			nil)))
	   (setf (descriptor-step des) step)
	   (setf (descriptor-%sap des)
		 (sap+ base-sap (- addr base))))))
      (:load-time-cookie
       (error "Values computed at load time cannot be used as addresses: ~S"
	      des))
      (t
       sap))))

(defun allocate-descriptor (step words lowtag force-large)
  (declare (type step step)
	   (type index words)
	   (type (unsigned-byte #.vm:lowtag-bits) lowtag)
	   (type (member t nil) force-large))
  (multiple-value-bind
      (addr sap)
      (if (or force-large (>= words vm:large-object-cutoff))
	  (alloc-large-object words step)
	  (alloc-small-object words step))
    (make-pointer-descriptor (logior addr lowtag) sap step)))

(declaim (inline descriptor-lowtag))
(defun descriptor-lowtag (des)
  "Return the lowtag bits for DES."
  (logand (descriptor-bits des) vm:lowtag-mask))

(defun make-fixnum-descriptor (num)
  (when (>= (integer-length num)
	    (1+ (- vm:word-bits vm:lowtag-bits)))
    (error "~D is too big for a fixnum." num))
  (make-random-descriptor (ldb (byte vm:word-bits 0) (ash num 2))))

(defun make-other-immediate-descriptor (data type)
  (make-random-descriptor (logior (ash data vm:type-bits) type)))

(defun make-character-descriptor (data)
  (make-other-immediate-descriptor data vm:base-char-type))

(defun descriptor-beyond (des offset type)
  (make-pointer-descriptor
   (logior (+ (logandc2 (descriptor-bits des) vm:lowtag-mask) offset) type)
   (sap+ (descriptor-sap des) offset)
   (descriptor-step des)))


;;;; Random variables and other noise.

(defparameter unbound-marker
  (make-other-immediate-descriptor 0 vm:unbound-marker-type)
  "Handle on the trap object.")

(defvar *nil-descriptor* nil
  "Handle on Nil.")

(defvar *current-init-functions-cons* nil
  "Head of list of functions to be called when the Lisp starts up.")

(defvar *in-cold-load* nil
  "Used by normal loader.")



;;;; Stuff to read and write the core memory.

(defun maybe-byte-swap (word)
  (declare (type (unsigned-byte 32) word))
  (assert (= vm:word-bits 32))
  (assert (= vm:byte-bits 8))
  (if (eq (c:backend-byte-order c:*native-backend*)
	  (c:backend-byte-order c:*backend*))
      word
      (logior (ash (ldb (byte 8 0) word) 24)
	      (ash (ldb (byte 8 8) word) 16)
	      (ash (ldb (byte 8 16) word) 8)
	      (ldb (byte 8 24) word))))

(defun maybe-byte-swap-short (short)
  (declare (type (unsigned-byte 16) short))
  (assert (= vm:word-bits 32))
  (assert (= vm:byte-bits 8))
  (if (eq (c:backend-byte-order c:*native-backend*)
	  (c:backend-byte-order c:*backend*))
      short
      (logior (ash (ldb (byte 8 0) short) 8)
	      (ldb (byte 8 8) short))))
  

(defun write-bits (address index bits)
  (declare (type descriptor address)
	   (type index index)
	   (type (unsigned-byte #.vm:word-bits) bits))
  (setf (sap-ref-32 (descriptor-sap address) (ash index vm:word-shift))
	(maybe-byte-swap bits)))

(defun write-descriptor (address index value &key (remember :slot))
  "Write VALUE (a descriptor) INDEX words from ADDRESS (also a descriptor)."
  (declare (type descriptor address value)
	   (type index index)
	   (type (member :nothing :slot :object) remember))
  (if (eq (descriptor-%sap value) :load-time-cookie)
      (note-load-time-value-reference address index value)
      (let ((bits (descriptor-bits value)))
	(write-bits address index bits)
	(unless (eq remember :nothing)
	  (when (logbitp 0 bits)
	    (let ((from-step (descriptor-step address))
		  (to-step (descriptor-step value)))
	      (unless from-step
		(error "Don't know the step for ~S" address))
	      (unless to-step
		(error "Don't know the step for ~S" value))
	      (let* ((from-gen (step-gen from-step))
		     (from-gen-num (generation-num from-gen))
		     (to-gen-num (generation-num (step-gen to-step))))
		(when (> from-gen-num to-gen-num)
		  (add-remset-entry from-gen 
				    (ecase remember
				      (:slot
				       (+ (logandc2 (descriptor-bits address)
						    vm:lowtag-mask)
					  (ash index vm:word-shift)))
				      (:object
				       (descriptor-bits address))))))))))))

(defun read-bits (address index)
  (declare (type descriptor address)
	   (type index index)
	   (values (unsigned-byte #.vm:word-bits)))
  (maybe-byte-swap (sap-ref-32 (descriptor-sap address)
			       (ash index vm:word-shift))))

(defun read-descriptor (address index)
  "Return the value (as a descriptor) INDEX words from ADDRESS (a descriptor)."
  (declare (type descriptor address)
	   (type index index)
	   (values descriptor))
  (make-random-descriptor (read-bits address index)))



;;;; Allocating primitive objects.

;;; There are two common kinds of objects that we have special allocators
;;; for:
;;;
;;;   Simple objects:
;;; Maybe a header word with the type and the length, plus the data.
;;;
;;;   Vector objects:
;;; A header word with the type, a word for the length, plus the data.
;;;

(defun allocate-simple-object (step words lowtag header-type)
  (declare (type step step)
	   (type index words)
	   (type (unsigned-byte #.vm:lowtag-bits) lowtag)
	   (type (or (unsigned-byte #.vm:type-bits) null) header-type)
	   (values descriptor))
  (let ((des (allocate-descriptor step words lowtag nil)))
    (when header-type
      (write-descriptor des 0
			(make-other-immediate-descriptor
			 (1- words) header-type)))
    des))

(defun allocate-vector-object (step element-size length header-type)
  (declare (type step step)
	   (type index element-size length)
	   (type (unsigned-byte #.vm:type-bits) header-type)
	   (values descriptor))
  (let* ((words (+ (ceiling (* length element-size) vm:word-bits) 2))
	 (des (allocate-descriptor step words vm:other-pointer-type nil)))
    (write-descriptor des 0 (make-other-immediate-descriptor 0 header-type))
    (write-descriptor des vm:vector-length-slot
		      (make-fixnum-descriptor length))
    des))


;;;; Routines to move simple objects into the core.

(defun string-to-core (string)
  (declare (type string string)
	   (values descriptor))
  (let* ((len (length string))
	 (des (allocate-vector-object *dynamic* vm:byte-bits (1+ len)
				      vm:simple-string-type)))
    ;; Note: We allocate an extra byte and tweek the length back to make sure
    ;; there will be a null at the end of the string to aid in call-out to
    ;; C.
    (write-descriptor des vm:vector-length-slot (make-fixnum-descriptor len))
    (copy-to-system-area string (* vm:vector-data-offset vm:word-bits)
			 (descriptor-sap des)
			 (* vm:vector-data-offset vm:word-bits)
			 (* (1+ len) vm:byte-bits))
    des))

(defun bignum-to-core (n)
  (declare (type integer n)
	   (values descriptor))
  (let* ((words (ceiling (1+ (integer-length n)) vm:word-bits))
	 (handle (allocate-simple-object *dynamic* (1+ words)
					 vm:other-pointer-type
					 vm:bignum-type)))
    (declare (fixnum words))
    (do ((index 1 (1+ index))
	 (remainder n (ash remainder (- vm:word-bits))))
	((> index words)
	 (unless (zerop (integer-length remainder))
	   (warn "Wrote ~D words of ~D, but ~D was left over"
		 words n remainder)))
      (write-bits handle index (ldb (byte vm:word-bits 0) remainder)))
    handle))

(defun number-pair-to-core (first second type)
  (declare (type descriptor first second)
	   (type (unsigned-byte #.vm:type-bits) type)
	   (values descriptor))
  (let ((des (allocate-simple-object *dynamic* 3 vm:other-pointer-type type)))
    (write-descriptor des 1 first)
    (write-descriptor des 2 second)
    des))

(defun float-to-core (num)
  (declare (type float num)
	   (values descriptor))
  (etypecase num
    (single-float
     (let ((des (allocate-simple-object *dynamic* vm:single-float-size
					vm:other-pointer-type
					vm:single-float-type)))
       (write-bits des vm:single-float-value-slot
		   (ldb (byte vm:word-bits 0)
			(single-float-bits num)))
       des))
    (double-float
     (let ((des (allocate-simple-object *dynamic* vm:double-float-size
					vm:other-pointer-type
					vm:double-float-type))
	   (high-bits (ldb (byte vm:word-bits 0) (double-float-high-bits num)))
	   (low-bits (double-float-low-bits num)))
       (ecase (c:backend-byte-order c:*backend*)
	 (:little-endian
	  (write-bits des vm:double-float-value-slot low-bits)
	  (write-bits des (1+ vm:double-float-value-slot) high-bits))
	 (:big-endian
	  (write-bits des vm:double-float-value-slot high-bits)
	  (write-bits des (1+ vm:double-float-value-slot) low-bits)))
       des))))

(defun number-to-core (number)
  (declare (type number number)
	   (values descriptor))
  (etypecase number
    (integer
     (if (< (integer-length number) 30)
	 (make-fixnum-descriptor number)
	 (bignum-to-core number)))
    (ratio
     (number-pair-to-core (number-to-core (numerator number))
			  (number-to-core (denominator number))
			  vm:ratio-type))
    (complex
     (number-pair-to-core (number-to-core (realpart number))
			  (number-to-core (imagpart number))
			  vm:complex-type))
    (float
     (float-to-core number))))

(defun sap-to-core (sap)
  (declare (type system-area-pointer sap)
	   (values descriptor))
  (let ((des (allocate-simple-object *dynamic* vm:sap-size
				     vm:other-pointer-type vm:sap-type)))
    (write-bits des vm:sap-pointer-slot (sap-int sap))
    des))

(defun cold-cons (car cdr)
  (declare (type descriptor car cdr)
	   (values descriptor))
  (let ((dest (allocate-simple-object *dynamic* 2 vm:list-pointer-type nil)))
    (write-descriptor dest 0 car)
    (write-descriptor dest 1 cdr)
    dest))

(defmacro cold-push (thing list)
  "Generates code to push the THING onto the given cold load LIST."
  `(setq ,list (cold-cons ,thing ,list)))

;;; COLD-VECTOR  --  Internal
;;;
;;;    Make a simple-vector that holds the specified Objects and return its
;;; decriptor.
;;;
(defun cold-vector (&rest objects)
  (let* ((size (length objects))
	 (result (allocate-vector-object *dynamic* vm:word-bits size
					 vm:simple-vector-type)))
    (dotimes (index size)
      (write-descriptor result (+ index vm:vector-data-offset) (pop objects)))
    result))



;;;; Symbol magic.

;;; Cold-Make-Symbol allocates a symbol and fills its print name cell and
;;; property list cell.

(defvar *cold-symbol-allocation-step* nil)

(defun cold-make-symbol (name)
  (declare (simple-string name)
	   (values descriptor))
  (let ((symbol (allocate-simple-object
		 (or *cold-symbol-allocation-step* *dynamic*) vm:symbol-size
		 vm:other-pointer-type vm:symbol-header-type)))
    (write-descriptor symbol vm:symbol-value-slot unbound-marker)
    (write-descriptor symbol vm:symbol-hash-slot
		      (make-fixnum-descriptor
		       (random vm:target-most-positive-fixnum)))
    (write-descriptor symbol vm:symbol-plist-slot *nil-descriptor*)
    (write-descriptor symbol vm:symbol-name-slot (string-to-core name))
    (write-descriptor symbol vm:symbol-package-slot *nil-descriptor*)
    symbol))

(defun cold-setq (symbol value)
  (declare (type descriptor symbol value))
  (write-descriptor symbol vm:symbol-value-slot value))


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

(defvar *cold-packages*)

;;; This holds a hash table mapping from descriptors to symbols, so we can
;;; back up.  The key is the *address* in the target core.
;;; 
(defvar *cold-symbols*)

;;; Cold-Intern  --  Internal
;;;
;;;    Return a handle on an interned symbol.  If necessary allocate
;;; the symbol and record which package the symbol was referenced in.
;;; When we allocatethe symbol, make sure we record a reference to
;;; the symbol in the home package so that the package gets set.
;;;
(defun cold-intern (symbol &optional (package (symbol-package symbol)))
  (declare (type symbol symbol)
	   (type (or package null) package)
	   (values descriptor))
  (let ((cold-info (get symbol 'cold-info)))
    (unless cold-info
      (cond ((eq (symbol-package symbol) package)
	     (let ((handle (cold-make-symbol (symbol-name symbol))))
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
  "Initilizes the cold load symbol-hacking data structures."
  (do-all-symbols (sym)
    (remprop sym 'cold-info))
  (setq *cold-packages* nil)
  (let ((*cold-symbol-allocation-step* *static*))
    ;; Special case NIL.
    (let* ((des (allocate-simple-object *static* (+ vm:symbol-size 2) 0 nil))
	   (nil-des
	    (make-pointer-descriptor (+ (descriptor-bits des)
					vm:word-bytes
					vm:other-pointer-type)
				     nil
				     *static*)))
      (setf *nil-descriptor* nil-des)
      (write-descriptor des 1
			(make-other-immediate-descriptor
			 0 vm:symbol-header-type))
      (write-descriptor nil-des vm:cons-car-slot nil-des)
      (write-descriptor nil-des vm:cons-cdr-slot nil-des)
      (write-descriptor des (1+ vm:symbol-plist-slot) nil-des)
      (write-descriptor des (1+ vm:symbol-name-slot) (string-to-core "NIL"))
      (write-descriptor des (1+ vm:symbol-package-slot) nil-des)
      (setf (get nil 'cold-info) (cons nil-des nil))
      (cold-intern nil))

    ;; Intern the others.
    (dolist (symbol vm:static-symbols)
      (let ((des (cold-intern symbol)))
	(unless (= (- (descriptor-bits des) (descriptor-bits *nil-descriptor*))
		   (vm:static-symbol-offset symbol))
	  (warn "Offset from ~S to ~S is ~D, not ~D"
		symbol
		nil
		(- (descriptor-bits des) (descriptor-bits *nil-descriptor*))
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
    (frob kernel::internal-error)
    (frob mach::handle-exception)
    (frob di::handle-breakpoint)
    (frob di::handle-function-end-breakpoint)
    (frob lisp::do-before-gc-stuff)
    (frob lisp::do-after-gc-stuff)
    (frob lisp::fdefinition-object)
    (frob apply))
  (macrolet ((frob (symbol value)
	       `(cold-setq (cold-intern ',symbol) ,value)))
    (frob *initial-layouts* (list-all-layouts))
    (frob *initial-symbols* (list-initial-symbols))
    (frob *initial-fdefn-objects* (list-all-fdefn-objects))
    (frob *lisp-initialization-functions* *current-init-functions-cons*)
    (frob kernel::*saved-state-chain* *nil-descriptor*))
  nil)

(defun list-initial-symbols ()
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
    res))

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

(defvar *fdefn-objects*)

(defvar *fdefn-space* nil)

(defun extract-fdefn-name (des)
  (ecase (descriptor-lowtag des)
    (#.vm:list-pointer-type
     (if (= (descriptor-bits des) (descriptor-bits *nil-descriptor*))
	 nil
	 (cons (extract-fdefn-name (read-descriptor des vm:cons-car-slot))
	       (extract-fdefn-name (read-descriptor des vm:cons-cdr-slot)))))
    (#.vm:other-pointer-type
     (or (gethash (descriptor-bits des) *cold-symbols*)
	 (descriptor-bits des)))))

(defun cold-fdefinition-object (name &optional leave-fn-raw)
  (let ((hash (extract-fdefn-name name)))
    (or (gethash hash *fdefn-objects*)
	(let ((fdefn (allocate-simple-object
		      (or *fdefn-space* *dynamic*)
		      vm:fdefn-size vm:other-pointer-type vm:fdefn-type)))
	  (setf (gethash hash *fdefn-objects*) fdefn)
	  (write-descriptor fdefn vm:fdefn-name-slot name)
	  (unless leave-fn-raw
	    (write-descriptor fdefn vm:fdefn-function-slot *nil-descriptor*)
	    (write-descriptor
	     fdefn vm:fdefn-raw-addr-slot
	     (make-random-descriptor
	      (ecase (c:backend-fasl-file-implementation
		      c:*backend*)
		((#.c:pmax-fasl-file-implementation
		  #.c:rt-fasl-file-implementation
		  #.c:rt-afpa-fasl-file-implementation
		  #.c:x86-fasl-file-implementation
		  #.c:hppa-fasl-file-implementation)
		 (lookup-foreign-symbol "undefined_tramp"))
		(#.c:sparc-fasl-file-implementation
		 (lookup-foreign-symbol "_undefined_tramp"))))))
	  fdefn))))
  
(defun cold-fset (name defn)
  (assert (= (logand (read-bits defn 0) vm:type-mask) vm:function-header-type))
  (let ((fdefn (cold-fdefinition-object name t)))
    (write-descriptor fdefn vm:fdefn-function-slot defn
		      :remember :object)
    (write-bits fdefn vm:fdefn-raw-addr-slot
		(read-bits defn vm:function-entry-point-slot))
    fdefn))

(defun initialize-static-fns ()
  (let ((*fdefn-space* *static*))
    (dolist (sym vm:static-functions)
      (let* ((fdefn (cold-fdefinition-object (cold-intern sym)))
	     (offset (- (+ (- (descriptor-bits fdefn) vm:other-pointer-type)
			   (* vm:fdefn-raw-addr-slot vm:word-bytes))
			(descriptor-bits *nil-descriptor*)))
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

;;; Table mapping from class names to descriptors for their layouts.
;;;
(defvar *cold-layouts* (make-hash-table :test #'equal))

;;; The descriptor for layout's layout, which we need when making layouts.
;;;
(defvar *layout-layout*)

(defparameter target-layout-length 16)

(defun make-cold-layout (name length inherits depth)
  (let ((result (allocate-simple-object *dynamic* (1+ target-layout-length)
					vm:instance-pointer-type
					vm:instance-header-type)))
    (write-descriptor result vm:instance-slots-offset *layout-layout*)
    (let ((base (+ vm:instance-slots-offset
		   kernel:layout-hash-length
		   1)))
      ;0 class uninitialized
      (write-descriptor result (+ base 1) *nil-descriptor*); invalid
      (write-descriptor result (+ base 2) inherits)
      (write-descriptor result (+ base 3) depth)
      (write-descriptor result (+ base 4) length)
      (write-descriptor result (+ base 5) *nil-descriptor*); info
      (write-descriptor result (+ base 6) *nil-descriptor*)); pure
    (setf (gethash name *cold-layouts*) result)
    result))


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
			  *nil-descriptor* (number-to-core 3)))
  (write-descriptor *layout-layout* vm:instance-slots-offset *layout-layout*)
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
    (write-descriptor *layout-layout*
		      (+ vm:instance-slots-offset layout-hash-length 1 2)
		      (cold-vector t-layout inst-layout so-layout))))


;;; LIST-ALL-LAYOUTS  --  Internal
;;;
;;;    Return a cold alist that we're going to store in *INITIAL-LAYOUTS*.
;;;
(defun list-all-layouts ()
  (let ((result *nil-descriptor*))
    (maphash #'(lambda (key value)
		 (cold-push (cold-cons (cold-intern key) value) result))
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
	 (result (allocate-simple-object *dynamic* (1+ size)
					 vm:instance-pointer-type
					 vm:instance-header-type)))
    (do ((index (1- size) (1- index)))
	((minusp index))
      (declare (fixnum index))
      (write-descriptor result (+ index vm:instance-slots-offset)
			(pop-stack)))
    result))

(define-cold-fop (fop-layout)
  (let ((length (pop-stack))
	(depth (pop-stack))
	(inherits (pop-stack))
	(name (pop-stack)))
    (or (gethash name *cold-layouts*)
	(make-cold-layout name length inherits depth))))

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
    (let ((symbol (cold-make-symbol name)))
      (push-table symbol))))

;;; Loading lists...

;;; Cold-Stack-List makes a list of the top Length things on the Fop-Stack.
;;; The last cdr of the list is set to Last.

(defmacro cold-stack-list (length last)
  `(do* ((index ,length (1- index))
	 (result ,last (cold-cons (pop-stack) result)))
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
      (write-descriptor result (+ index vm:vector-data-offset) (pop-stack)))
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
      (write-descriptor result (+ index vm:vector-data-offset) datum))
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
	 (dotimes (index (ceiling (* len size) vm:word-bits))
	   (write-bits result (+ index vm:vector-data-offset) word))))
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

(define-cold-fop (fop-array)
  (let* ((rank (read-arg 4))
	 (data-vector (pop-stack))
	 (result (allocate-simple-object *dynamic*
					 (+ vm:array-dimensions-offset rank)
					 vm:other-pointer-type
					 vm:simple-array-type)))
    (write-descriptor result vm:array-fill-pointer-slot *nil-descriptor*)
    (write-descriptor result vm:array-data-slot data-vector)
    (write-descriptor result vm:array-displacement-slot *nil-descriptor*)
    (write-descriptor result vm:array-displaced-p-slot *nil-descriptor*)
    (let ((total-elements 1))
      (dotimes (axis rank)
	(let* ((dim (pop-stack))
	       (bits (descriptor-bits dim)))
	  (unless (zerop (ldb (byte 2 0) bits))
	    (error "Non-fixnum dimension? (~S)" dim))
	  (setf total-elements (* total-elements (ash bits -2)))
	  (write-descriptor result (+ vm:array-dimensions-offset axis) dim)))
      (write-descriptor result vm:array-elements-slot
			(make-fixnum-descriptor total-elements)))
    result))

;;; Loading numbers.

(defmacro cold-number (fop)
  `(define-cold-fop (,fop :nope)
     (,fop)
     (with-fop-stack t
       (number-to-core (pop-stack)))))

(cold-number fop-single-float)
(cold-number fop-double-float)
(cold-number fop-integer)
(cold-number fop-small-integer)
(cold-number fop-word-integer)
(cold-number fop-byte-integer)

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
    (cold-push (cold-cons
		(cold-intern :load-time-value)
		(cold-cons
		 (pop-stack)
		 (cold-cons
		  (number-to-core counter)
		  *nil-descriptor*)))
	       *current-init-functions-cons*)
    (setf *load-time-value-counter* (1+ counter))
    (make-pointer-descriptor counter :load-time-cookie nil)))

(defun note-load-time-value-reference (address index marker)
  (cold-push (cold-cons
	      (cold-intern :load-time-value-fixup)
	      (cold-cons
	       address
	       (cold-cons
		(make-fixnum-descriptor index)
		(cold-cons
		 (number-to-core (descriptor-bits marker))
		 *nil-descriptor*))))
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
    (write-descriptor (cold-nthcdr idx obj) 0 (pop-stack))))

(define-cold-fop (fop-rplacd nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-descriptor (cold-nthcdr idx obj) 1 (pop-stack))))

(define-cold-fop (fop-svset nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-descriptor obj (+ idx vm:vector-data-offset) (pop-stack))))

(define-cold-fop (fop-structset nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4)))
    (write-descriptor obj (+ idx vm:instance-slots-offset) (pop-stack))))

(define-cold-fop (fop-nthcdr t)
  (cold-nthcdr (read-arg 4) (pop-stack)))


(defun cold-nthcdr (index obj)
  (iterate repeat ((result obj) (index index))
    (cond ((plusp index)
	   (repeat (read-descriptor result 1) (1- index)))
	  (t
	   (setf (descriptor-step result) (descriptor-step obj))
	   result))))

;;; Loading code objects and functions.

(define-cold-fop (fop-fset nil)
  (let ((fn (pop-stack))
	(name (pop-stack)))
    (cold-fset name fn)))

(define-cold-fop (fop-fdefinition)
  (cold-fdefinition-object (pop-stack)))

(define-cold-fop (fop-sanctify-for-execution)
  (pop-stack))

(define-cold-fop (fop-code-format :nope)
  (let ((implementation (read-arg 1))
	(version (read-arg 1)))
    (unless (= implementation (c:backend-fasl-file-implementation c:*backend*))
      (error
       "~A was compiled for a ~A, but we are trying to build a core for a ~A"
       *Fasl-file*
       (or (elt c:fasl-file-implementations implementation)
	   "unknown machine")
       (or (elt c:fasl-file-implementations
		(c:backend-fasl-file-implementation c:*backend*))
	   "unknown machine")))
    (unless (= version (c:backend-fasl-file-version c:*backend*))
      (error
       "~A was compiled for a fasl-file version ~A, but we need version ~A"
       *Fasl-file* version (c:backend-fasl-file-version c:*backend*)))))

(defmacro define-cold-code-fop (name nconst size)
  `(define-cold-fop (,name)
     (let* ((nconst ,nconst)
	    (inst-words ,size)
	    (raw-header-size (+ vm:code-debug-info-slot nconst))
	    (header-size
	     ;; Note: we round the number of constants up to assure that
	     ;; the code vector will be properly aligned.
	     (round-up raw-header-size 2))
	    (des (allocate-descriptor *dynamic*
				      (+ header-size inst-words)
				      vm:other-pointer-type
				      t)))
       (write-descriptor des 0
			 (make-other-immediate-descriptor header-size
							  vm:code-header-type))
       (write-descriptor des vm:code-code-size-slot
			 (make-fixnum-descriptor inst-words))
       (write-descriptor des vm:code-entry-points-slot *nil-descriptor*)
       (when (oddp raw-header-size)
	 (write-bits des raw-header-size 0))
       (do ((index (1- raw-header-size) (1- index)))
	   ((< index vm:code-debug-info-slot))
	 (write-descriptor des index (pop-stack)))
       (read-n-bytes *fasl-file*
		     (descriptor-sap des)
		     (ash header-size vm:word-shift)
		     (ash inst-words vm:word-shift))
       des)))

(define-cold-code-fop fop-code (read-arg 4) (read-arg 4))

(define-cold-code-fop fop-small-code (read-arg 1) (read-arg 2))


(clone-cold-fop (fop-alter-code nil)
		(fop-byte-alter-code)
  (let ((slot (clone-arg))
	(value (pop-stack))
	(code (pop-stack)))
    (write-descriptor code slot value)))

(defun calc-offset (code-object after-header)
  (+ after-header
     (ash (ash (read-bits code-object 0) (- vm:type-bits)) vm:word-shift)))

(define-cold-fop (fop-function-entry)
  (let* ((type (pop-stack))
	 (arglist (pop-stack))
	 (name (pop-stack))
	 (code-object (pop-stack))
	 (offset (calc-offset code-object (read-arg 4)))
	 (fn (descriptor-beyond code-object offset vm:function-pointer-type))
	 (next (read-descriptor code-object vm:code-entry-points-slot)))
    (unless (zerop (logand offset vm:lowtag-mask))
      (warn "Unaligned function entry: ~S at #x~X" name offset))
    (write-descriptor code-object vm:code-entry-points-slot fn)
    (write-descriptor fn 0
		      (make-other-immediate-descriptor
		       (ash offset (- vm:word-shift))
		       vm:function-header-type))
    (write-bits fn vm:function-entry-point-slot
		(if (= (c:backend-fasl-file-implementation c:*backend*)
		       c:sparc-fasl-file-implementation)
		    (descriptor-bits fn)
		    (+ (logandc2 (descriptor-bits fn) vm:lowtag-mask)
		       (ash vm:function-code-offset vm:word-shift))))
    (write-descriptor fn vm:function-next-slot next :remember :nothing)
    (write-descriptor fn vm:function-name-slot name)
    (write-descriptor fn vm:function-arglist-slot arglist)
    (write-descriptor fn vm:function-type-slot type)
    fn))

(define-cold-fop (fop-foreign-fixup)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (read-n-bytes *fasl-file* sym 0 len)
    (let ((offset (calc-offset code-object (read-arg 4))))
      (do-cold-fixup code-object offset (lookup-foreign-symbol sym) kind))
    code-object))

(define-cold-fop (fop-assembler-code)
  (let* ((length (read-arg 4))
	 (header-size
	  ;; Note: we round the number of constants up to assure that
	  ;; the code vector will be properly aligned.
	  (round-up vm:code-constants-offset 2))
	 (des (allocate-descriptor *static*
				   (+ header-size length)
				   vm:other-pointer-type
				   t)))
    (write-descriptor des 0
		      (make-other-immediate-descriptor header-size
						       vm:code-header-type))
    (write-descriptor des vm:code-code-size-slot
		      (make-fixnum-descriptor length))
    (write-descriptor des vm:code-entry-points-slot *nil-descriptor*)
    (write-descriptor des vm:code-debug-info-slot
		      (cold-intern :assembler-routine))

    (read-n-bytes *fasl-file*
		  (descriptor-sap des)
		  (ash header-size vm:word-shift)
		  (ash length vm:word-shift))
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
	 (offset (calc-offset code-object (read-arg 4))))
    (record-cold-assembler-fixup routine code-object offset kind)
    code-object))

(define-cold-fop (fop-code-object-fixup)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (offset (calc-offset code-object (read-arg 4))))
    (do-cold-fixup code-object offset
		   (descriptor-bits code-object)
		   kind)
    code-object))

(not-cold-fop fop-make-byte-compiled-function)


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

(defvar *cold-foreign-symbol-table*)

(defun load-foreign-symbol-table (filename)
  (with-open-file (file filename)
    (let* ((version-line (read-line file))
	   (last-space (position #\Space version-line :from-end t))
	   (version (parse-integer version-line :start (1+ last-space))))
      (loop
	(let ((line (read-line file nil nil)))
	  (unless line
	    (return))
	  (multiple-value-bind
	      (value name)
	      (if (string= "0x" line :end2 2)
		  (values (parse-integer line :start 2 :end 10 :radix 16)
			  (subseq line 13))
		  (values (parse-integer line :end 8 :radix 16)
			  (subseq line 11)))
	    (multiple-value-bind
		(old-value found)
		(gethash name *cold-foreign-symbol-table*)
	      (when found
		(warn "Redefining ~S from #x~X to #x~X" name old-value value)))
	    (setf (gethash name *cold-foreign-symbol-table*) value))))
      version)))

(defun lookup-foreign-symbol (name)
  (multiple-value-bind
      (value found)
      (gethash name *cold-foreign-symbol-table* 0)
    (unless found
      (warn "Undefined foreign symbol: ~S" name))
    value))


(defvar *cold-assembler-routines*)

(defvar *cold-assembler-fixups*)

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

(defun do-cold-fixup (code-object offset value kind)
  (let ((sap (sap+ (descriptor-sap code-object) offset)))
    (ecase (c:backend-fasl-file-implementation c:*backend*)
      (#.c:pmax-fasl-file-implementation
       (let ((inst (maybe-byte-swap (sap-ref-32 sap 0))))
	 (ecase kind
	   (:jump
	    (assert (zerop (ash value -26)))
	    (setf (ldb (byte 26 0) inst) (ash value -2)))
	   (:lui
	    (setf (ldb (byte 16 0) inst)
		  (+ (ash value -16)
		     (if (logbitp 15 value) 1 0))))
	   (:addi
	    (setf (ldb (byte 16 0) inst)
		  (ldb (byte 16 0) value))))
	 (setf (sap-ref-32 sap 0) (maybe-byte-swap inst))))
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
      (#.c:x86-fasl-file-implementation
       (let* ((disp (logior (sap-ref-8 sap 0)
			    (ash (sap-ref-8 sap 1) 8)
			    (ash (sap-ref-8 sap 2) 16)
			    (ash (sap-ref-8 sap 3) 24)))
	      (bits (descriptor-bits code-object))
	      (value (ecase kind
		       (:absolute (+ value disp))
		       (:relative (- (+ value disp vm:other-pointer-type)
				     bits offset 4)))))
	 (setf (sap-ref-8 sap 0) (ldb (byte 8 0) value))
	 (setf (sap-ref-8 sap 1) (ldb (byte 8 8) value))
	 (setf (sap-ref-8 sap 2) (ldb (byte 8 16) value))
	 (setf (sap-ref-8 sap 3) (ldb (byte 8 24) value))))
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
			     (logand inst #xffe0e002)))))))))))
  (undefined-value))

(defun linkage-info-to-core ()
  (let ((result *nil-descriptor*))
    (maphash #'(lambda (symbol value)
		 (cold-push (cold-cons (string-to-core symbol)
				       (number-to-core value))
			    result))
	     *cold-foreign-symbol-table*)
    (cold-setq (cold-intern '*initial-foreign-symbols*) result))
  (let ((result *nil-descriptor*))
    (dolist (rtn *cold-assembler-routines*)
      (cold-push (cold-cons (cold-intern (car rtn))
			    (number-to-core (cdr rtn)))
		 result))
    (cold-setq (cold-intern '*initial-assembler-routines*) result)))



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
			   (subseq name 0
				   (- (length name)
				      (length tail)))
			   priority)))
	       (test-head (head prefix priority)
		 (when (head-comp name head)
		   (record prefix
			   (subseq name (length head))
			   priority))))
	    (test-tail "-TYPE" "type_" 0)
	    (test-tail "-TRAP" "trap_" 2)
	    (test-head "TRACE-TABLE-" "tracetab_" 4)
	    (test-tail "-SC-NUMBER" "sc_" 5)))))
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
	(format t "#define ~A ~D~@[  /* ~A */~]~%"
		(first const) (third const) (fourth const))))
    (terpri)
    (format t "#define ERRORS { \\~%")
    (loop
      for info across (c:backend-internal-errors c:*backend*)
      do (format t "    ~S, \\~%" (cdr info)))
    (format t "    NULL \\~%}~%")
    (terpri))
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
	(format t "    ~A ~A~@[[0]~];~%"
		(getf (vm:slot-options slot) :c-type "lispobj")
		(nsubstitute #\_ #\-
			     (string-downcase (string (vm:slot-name slot))))
		(vm:slot-rest-p slot)))
      (format t "};~2%"))
    (format t "#else /* LANGUAGE_ASSEMBLY */~2%")
    (format t "#define LISPOBJ(thing) thing~2%")
    (dolist (obj structs)
      (let ((name (vm:primitive-object-name obj))
	    (lowtag (or (eval (vm:primitive-object-lowtag obj)) 0)))
	(dolist (slot (vm:primitive-object-slots obj))
	  (format t "#define ~A_~A_OFFSET ~D~%"
		  (substitute #\_ #\- (string name))
		  (substitute #\_ #\- (string (vm:slot-name slot)))
		  (- (* (vm:slot-offset slot) vm:word-bytes) lowtag)))
	(terpri)))
    (format t "#endif /* LANGUAGE_ASSEMBLY */~2%"))
  (dolist (symbol (cons nil vm:static-symbols))
    (format t "#define ~A LISPOBJ(0x~X)~%"
	    (nsubstitute #\_ #\-
			 (remove-if #'(lambda (char)
					(member char '(#\% #\*)))
				    (symbol-name symbol)))
	    (if (boundp '*static*)
		;; We actually ran Genesis, use the real value.
		(descriptor-bits (cold-intern symbol))
		;; We didn't run Genesis, so guess at the address.
		(+ *heap-start*
		   vm:word-bytes
		   vm:other-pointer-type
		   (if symbol (vm:static-symbol-offset symbol) 0)))))
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
	   (rename-file new-name name))
	  ((files-differ name new-name)
	   (rename-file name
			(concatenate 'simple-string
				     (namestring name)
				     ".OLD"))
	   (unix:unix-chmod unix-newname #o444)
	   (rename-file new-name name)
	   (warn "The C header file has changed.~%Be sure to ~
		  re-compile the startup code."))
	  (t
	   (unix:unix-unlink unix-newname))))
  (undefined-value))


;;;; The actual genesis function.

(defvar *genesis-core-name* "lisp.core")
(defvar *genesis-map-name* nil)
(defvar *genesis-c-header-name* t)
(defvar *genesis-symbol-table* nil)

(defun genesis (file-list &key
			  (symbol-table *genesis-symbol-table*)
			  (core-name *genesis-core-name*)
			  (map-name *genesis-map-name*)
			  (header-name *genesis-c-header-name*))
  "Builds a kernel Lisp image from the .FASL files specified in the given
  File-List and writes it to a file named by Core-Name."
  (unless symbol-table
    (error "Can't genesis without a symbol-table."))
  (format t "~&Building ~S for the ~A~%"
	  core-name (c:backend-version c:*backend*))
  (with-allocation-pool
    (let* ((*block-owners* (make-array 16 :initial-element nil))
	   (*static* nil)
	   (*dynamic* nil)
	   (generations (make-generations))
	   (*current-los-cluster* nil)
	   (*current-init-functions-cons* nil)
	   (*cold-packages* nil)
	   (*cold-symbols* (make-hash-table :test #'equal))
	   (*fdefn-objects* (make-hash-table :test #'equal))
	   (*load-time-value-counter* 0)
	   (*cold-foreign-symbol-table* (make-hash-table :test #'equal))
	   (*cold-assembler-routines* nil)
	   (*cold-assembler-fixups* nil)
	   (version (load-foreign-symbol-table symbol-table))
	   (initial-function nil))
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
     (let ((initial-fdefn
	    (cold-fdefinition-object (cold-intern 'lisp::%initial-function))))
       (setf initial-function
	     (read-bits initial-fdefn vm:fdefn-function-slot)))
     (resolve-assembler-fixups)
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
			 core-name)))
     (write-initial-core-file core-name version generations
			      initial-function))))


(defun write-map-file ()
  (let ((*print-pretty* nil)
	(*print-case* :upcase))
    (format t "Assembler routines defined in core image:~2%")
    (dolist (routine (sort (copy-list *cold-assembler-routines*) #'<
			   :key #'cdr))
      (format t "#x~8,'0X: ~S~%" (cdr routine) (car routine)))
    (let ((funs nil) (undefs nil))
      (maphash #'(lambda (name fdefn)
		   (let ((fun-bits (read-bits fdefn vm:fdefn-function-slot)))
		     (if (= fun-bits (descriptor-bits *nil-descriptor*))
			 (push name undefs)
			 (push (list* name
				      fun-bits
				      (read-bits fdefn vm:fdefn-raw-addr-slot))
			       funs))))
	       *fdefn-objects*)
      (format t "~%~|~%Initially defined functions:~2%")
      (dolist (info (sort funs #'< :key #'cddr))
	(format t "#x~8,'0X: ~S  #x~8,'0X~%"
		(cddr info) (car info) (cadr info)))
      (format t "~%~|~%Undefined function references:~2%")
      (labels ((key (name)
		 (etypecase name
		   (symbol (symbol-name name))
		   (integer (prin1-to-string name))
		   (list (key (second name))))))
	(dolist (fun (sort undefs #'string< :key #'key))
	  (format t "~S~%" fun)))
      (format t "~%~|~%Layout names:~2%")
      (collect ((stuff))
	(maphash #'(lambda (name desc)
		     (stuff (cons (descriptor-bits desc) name)))
		 *cold-layouts*)
	(dolist (x (sort (stuff) #'< :key #'car))
	  (format t "#x~8,'0X: ~S~%" (car x) (cdr x))))))
  (undefined-value))


;;;; Core file writing magic.

(defstruct (range
	    (:print-function %print-range))
  ;;
  ;; The core address this range corresponds to.
  (address (required-argument) :type address)
  ;;
  ;; The number of bytes at that address to dump.
  (bytes 0 :type (unsigned-byte #.vm:word-bits))
  ;;
  ;; List of (sap . bytes) indicating where to get the bytes to dump.
  (pieces nil :type list)
  ;;
  ;; The next range in the chain, sorted by by address.
  (next nil :type (or null range)))

(defun %print-range (range stream depth)
  (declare (ignore depth))
  (print-unreadable-object (range stream :type t)
    (format stream "#x~8,'0X...#x~8,'0X"
	    (range-address range)
	    (+ (range-address range) (range-bytes range)))))

(defconstant max-gap (ash 1 14))

(defun compute-ranges (generations)
  (let ((ranges nil)
	(page-size (c:backend-page-size c:*backend*)))
    (flet
	((write-range (sap addr bytes)
	   (let* ((start-offset (rem addr page-size))
		  (addr (- addr start-offset))
		  (sap (sap+ sap (- start-offset)))
		  (pages (ceiling (+ bytes start-offset) page-size))
		  (bytes (* pages page-size)))
	     (iterate repeat ((prev nil) (range ranges))
	       (flet ((insert (next)
			(if prev
			    (setf (range-next prev) next)
			    (setf ranges next))))
		 (if range
		     (if (< addr (range-address range))
			 (insert (make-range :address addr :bytes bytes
					     :pieces (list (cons sap bytes))
					     :next range))
			 (repeat range (range-next range)))
		     (insert (make-range :address addr :bytes bytes
					 :pieces
					 (list (cons sap bytes))))))))))
      (dolist (gen generations)
	(dolist (step (generation-steps gen))
	  (do ((block (step-first-block step) (block-next block)))
	      ((null block))
	    (write-range (block-sap block)
			 (block-base block)
			 (block-free block)))))
      (do ((cluster *current-los-cluster* (cluster-next cluster)))
	  ((null cluster))
	(let* ((first (cluster-first cluster))
	       (base (cluster-base cluster))
	       (offset (- first base)))
	  (write-range (sap+ (cluster-sap cluster) offset)
		       first
		       (- (cluster-free cluster) offset)))))
    (when ranges
      (iterate repeat ((prev ranges) (next (range-next ranges)))
	(when next
	  (let* ((prev-length (range-bytes prev))
		 (prev-end (+ (range-address prev) prev-length))
		 (next-start (range-address next))
		 (gap (- next-start prev-end)))
	    (if (< gap max-gap)
		(let ((next-next (range-next next)))
		  (setf (range-pieces prev)
			(append (range-pieces prev)
				(cons (cons (int-sap 0) gap)
				      (range-pieces next))))
		  (setf (range-next prev) next-next)
		  (setf (range-bytes prev)
			(+ prev-length gap (range-bytes next)))
		  (repeat prev next-next))
		(repeat next (range-next next)))))))
    ranges))


(defconstant max-header-size (ash 1 12))

(defvar *header-buffer*)
(defvar *header-fill-pointer*)

(defun write-long (num)
  (let ((fill-pointer *header-fill-pointer*))
    (setf (sap-ref-32 *header-buffer* fill-pointer) (maybe-byte-swap num))
    (setf *header-fill-pointer* (+ fill-pointer 4))))


(defun write-magic-number (version)
  (write-long (logior (ash (char-code #\C) 24)
		      (ash (char-code #\O) 16)
		      (ash (char-code #\R) 8)
		      (char-code #\E)))
  (write-long version)
  (write-long 1) ; kernel_core = TRUE
  (undefined-value))

(defun write-chunks (chunks)
  (do ((range chunks (range-next range)))
      ((null range))
    (write-long (range-address range))
    (write-long (range-bytes range))
    (write-long 0))
  (write-long 0))

(defun really-do-writing (chunk-info-start chunks file)
  (let ((header-length (round-up *header-fill-pointer*
				 (c:backend-page-size c:*backend*))))
    (do ((range chunks (range-next range))
	 (index (+ chunk-info-start 8) (+ index 12))
	 (file-offset header-length (+ file-offset (range-bytes range))))
	((null range))
      (setf (sap-ref-32 *header-buffer* index) (maybe-byte-swap file-offset))
      (format t "Writing ~:D bytes from #x~8,'0X...~%"
	      (range-bytes range)
	      (range-address range))
      (do ((file-offset file-offset (+ file-offset (cdar pieces)))
	   (pieces (range-pieces range) (cdr pieces)))
	  ((null pieces))
	(let* ((piece (car pieces))
	       (sap (car piece)))
	  (unless (zerop (sap-int sap))
	    (file-position file file-offset)
	    (system:output-raw-bytes file sap 0 (cdr piece))))))
    (file-position file 0)
    (system:output-raw-bytes file *header-buffer* 0 header-length))
  nil)

(defun write-clusters ()
  (do ((cluster *current-los-cluster* (cluster-next cluster)))
      ((null cluster))
    (let ((base (cluster-base cluster)))
      (write-long base)
      (write-long (cluster-blocks cluster))
      (write-long (cluster-first cluster))
      (write-long (+ base (cluster-free cluster)))))
  (write-long 0))

(defun write-generations (generations)
  (write-long (length generations))
  (dolist (gen generations)
    (write-long (generation-policy gen))
    (let ((steps (generation-steps gen)))
      (write-long (length steps))
      (dolist (step steps)
	(write-long (step-num step))
	(write-long (step-prom-step step))
	(write-long (step-max-blocks step))
	(write-long (step-policy step))
	(do ((block (step-first-block step) (block-next block)))
	    ((null block))
	  (let ((base (block-base block)))
	    (write-long base)
	    (write-long (+ base (block-free block)))))
	(write-long 0)
	(let ((block (step-block step)))
	  (if block
	      (write-long (+ (block-base block) block-bytes))
	      (write-long 0)))
	(write-long (step-first-LO step))
	(write-long (step-last-LO step))))
    (maphash #'(lambda (address ignore)
		 (declare (ignore ignore))
		 (write-long address))
	     (generation-remset gen))
    (write-long 0)))

(defun write-initial-core-file (name version generations initial-function)
  (format t "[Building Initial Core File (version ~D) in file ~S: ~%"
	  version (namestring name))
  (force-output)
  (let ((*header-buffer* (nth-value 1 (allocate-blocks 1)))
	(*header-fill-pointer* 0))
    (with-open-file (file name
			  :direction :output
			  :element-type '(unsigned-byte 8)
			  :if-exists :rename-and-delete)
      (write-magic-number version)
      (write-long *heap-start*)
      (write-long (ash (- *heap-free-pointer* *heap-start*) (- block-shift)))
      (write-long 0)
      (let* ((chunks-start *header-fill-pointer*)
	     (chunks (compute-ranges generations)))
	(write-chunks chunks)
	(write-clusters)
	(write-generations generations)
	(write-long initial-function)
	(really-do-writing chunks-start chunks file))))
  (format t "done]~%")
  (force-output))

