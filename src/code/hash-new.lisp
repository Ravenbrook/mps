;;; -*- Package: CL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/hash-new.lisp,v 1.43 2006/08/18 13:19:05 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Hashing and hash table functions for Spice Lisp.
;;; Originally written by Skef Wholey.
;;; Everything except SXHASH rewritten by William Lott.
;;; Hash table functions rewritten by Douglas Crosher, 1997.
;;; Equalp hashing by William Newman, Cadabra Inc, and Douglas Crosher, 2000.
;;;
(in-package :lisp)

(export '(hash-table hash-table-p make-hash-table
	  gethash remhash maphash clrhash
	  hash-table-count with-hash-table-iterator
	  hash-table-rehash-size hash-table-rehash-threshold
	  hash-table-size hash-table-test sxhash))

(in-package :ext)
(export '(define-hash-table-test))

(in-package :lisp)

(register-lisp-runtime-feature :hash-new)


;;;; The hash-table structures.

;;; HASH-TABLE -- defstruct.
;;;
(defstruct (hash-table
	    (:constructor %make-hash-table)
	    (:print-function %print-hash-table)
	    (:make-load-form-fun make-hash-table-load-form))
  "Structure used to implement hash tables."
  ;;
  ;; The type of hash table this is.  Only used for printing and as part of
  ;; the exported interface.
  (test (required-argument) :type symbol :read-only t)
  ;;
  ;; The function used to compare two keys.  Returns T if they are the same
  ;; and NIL if not.
  (test-fun (required-argument) :type function :read-only t)
  ;;
  ;; The function used to compute the hashing of a key.  Returns two values:
  ;; the index hashing and T if that might change with the next GC.
  (hash-fun (required-argument) :type function :read-only t)
  ;;
  ;; How much to grow the hash table by when it fills up.  If an index, then
  ;; add that amount.  If a floating point number, then multiple it by that.
  (rehash-size (required-argument) :type (or index (single-float (1.0)))
	       :read-only t)
  ;;
  ;; How full the hash table has to get before we rehash.
  (rehash-threshold (required-argument) :type (single-float (0.0) 1.0)
		    :read-only t)
  ;;
  ;; The number of entries before a rehash, just the one less than the
  ;; size of the next-vector, hash-vector, and half the size of the
  ;; kv-vector.
  (rehash-trigger (required-argument) :type index)
  ;;
  ;; The current number of entries in the table.
  (number-entries 0 :type index)
  ;;
  ;; The Key-Value pair vector.
  (table (required-argument) :type simple-vector)
  ;;
  ;; Non-nil if this is a weak hash table.  Four separate types of
  ;; weakness are supported: :key, :value, :key-and-value,
  ;; :key-or-value.  The entry in the table is retained if the key,
  ;; (respectively, value, key and value, key or value) is alive
  ;; because it is referenced elsewhere.  If the condition does not
  ;; hold, the entry is removed.  For tables with a weak key, EQL or
  ;; EQ must be used as the test.
  (weak-p nil :type (member nil
			    :key
			    :value
			    :key-and-value
			    :key-or-value))
  ;;
  ;; Index into the next-vector, chaining together buckets that need
  ;; to be rehashed because their hashing is EQ based and the key has
  ;; been moved by the garbage collector.
  (needing-rehash 0 :type index)
  ;;
  ;; Index into the Next vector chaining together free slots in the KV
  ;; vector.
  (next-free-kv 0 :type index)
  ;;
  ;; The index vector. This may be larger than the hash size to help
  ;; reduce collisions.
  (index-vector (required-argument)
		:type (simple-array (unsigned-byte 32) (*)))
  ;;
  ;; This table parallels the KV vector, and is used to chain together
  ;; the hash buckets, the free list, and the values needing rehash, a
  ;; slot will only ever be in one of these lists.
  (next-vector (required-argument) :type (simple-array (unsigned-byte 32) (*)))
  ;;
  ;; This table parallels the KV table, and can be used to store the
  ;; hash associated with the key, saving recalculation. Could be
  ;; useful for EQL, and EQUAL hash tables. This table is not needed
  ;; for EQ hash tables (an is NIL in that case), and when present the
  ;; value of #x8000000 represents EQ-based hashing on the respective
  ;; Key.
  (hash-vector nil :type (or null (simple-array (unsigned-byte 32) (*))))
  ;;
  ;; This is used by GC to chain the list of weak hash tables
  ;; together.  It should otherwise always be NIL.
  (next-weak-table nil :type (or null hash-table)))

;;;
(defun %print-hash-table (ht stream depth)
  (declare (ignore depth) (stream stream))
  (print-unreadable-object (ht stream :type t :identity t)
    (format stream "~S ~S ~S ~S ~S ~D"
	    :test
	    (hash-table-test ht)
	    :weak-p
	    (hash-table-weak-p ht)
	    :count
	    (hash-table-number-entries ht))))

(defconstant max-hash most-positive-fixnum)

(deftype hash ()
  `(integer 0 ,max-hash))

;; This value in the hash-vector indicates the the key uses EQ-based
;; hashing (i.e., either EQ or EQL).
(defconstant +eq-based-hash-value+ #x80000000)


;;;; Utility functions.

(declaim (inline pointer-hash))
(defun pointer-hash (key)
  (declare (values hash))
  (truly-the hash (%primitive make-fixnum key)))

(declaim (inline eq-hash))
(defun eq-hash (key)
  (declare (values hash (member t nil)))
  (values (pointer-hash key)
	  (oddp (get-lisp-obj-address key))))

(declaim (inline eql-hash))
(defun eql-hash (key)
  (declare (values hash (member t nil)))
  (if (numberp key)
      (equal-hash key)
      (eq-hash key)))

(declaim (inline equal-hash))
(defun equal-hash (key)
  (declare (values hash (member t nil)))
  (values (sxhash key) nil))

(defun equalp-hash (key)
  (declare (values hash (member t nil)))
  (values (internal-equalp-hash key 0) nil))


(defun almost-primify (num)
  (declare (type index num))
  "Almost-Primify returns an almost prime number greater than or equal
   to NUM."
  (if (= (rem num 2) 0)
      (setq num (+ 1 num)))
  (if (= (rem num 3) 0)
      (setq num (+ 2 num)))
  (if (= (rem num 7) 0)
      (setq num (+ 4 num)))
  num)



;;;; User defined hash table tests.

;;; *HASH-TABLE-TESTS* -- Internal.
;;; 
(defvar *hash-table-tests* nil)

;;; DEFINE-HASH-TABLE-TEST -- Public.
;;;
(defun define-hash-table-test (name test-fun hash-fun)
  "Define a new kind of hash table test."
  (declare (type symbol name)
	   (type function test-fun hash-fun))
  (setf *hash-table-tests*
	(cons (list name test-fun hash-fun)
	      (remove name *hash-table-tests* :test #'eq :key #'car)))
  name)


;;;; Construction and simple accessors.

;;; MAKE-HASH-TABLE -- public.
;;; 
(defun make-hash-table (&key (test 'eql) (size 65) (rehash-size 1.5)
			     (rehash-threshold 1.0) (weak-p nil))
  "Creates and returns a new hash table.  The keywords are as follows:
     :TEST -- Indicates what kind of test to use.  Only EQ, EQL, EQUAL,
       and EQUALP are currently supported.
     :SIZE -- A hint as to how many elements will be put in this hash
       table.
     :REHASH-SIZE -- Indicates how to expand the table when it fills up.
       If an integer, add space for that many elements.  If a floating
       point number (which must be greater than 1.0), multiple the size
       by that amount.
     :REHASH-THRESHOLD -- Indicates how dense the table can become before
       forcing a rehash.  Can be any positive number <= to 1, with density
       approaching zero as the threshold approaches 0.  Density 1 means an
       average of one entry per bucket.
   CMUCL Extension:
     :WEAK-P -- Weak hash table.  An entry in the table is remains if the
                condition holds:
                
                :KEY            -- key is referenced elsewhere
                :VALUE          -- value is referenced elsewhere
                :KEY-AND-VALUE  -- key and value are referenced elsewhere
                :KEY-OR-VALUE   -- key or value is referenced elsewhere

                If the condition does not hold, the entry is removed.  For
                backward compatibility, a value of T is the same as :KEY."
  (declare (type (or function symbol) test)
	   (type index size)
	   (type (member t nil :key :value :key-and-value :key-or-value) weak-p))
  (let ((rehash-size (if (integerp rehash-size)
			 rehash-size
			 (float rehash-size 1.0))))
    (when (eq weak-p t)
      (setf weak-p :key))
    (multiple-value-bind
	(test test-fun hash-fun)
	(cond ((or (eq test #'eq) (eq test 'eq))
	       (values 'eq #'eq #'eq-hash))
	      ((or (eq test #'eql) (eq test 'eql))
	       (values 'eql #'eql #'eql-hash))
	      ((or (eq test #'equal) (eq test 'equal))
	       (values 'equal #'equal #'equal-hash))
	      ((or (eq test #'equalp) (eq test 'equalp))
	       (values 'equalp #'equalp #'equalp-hash))
	      (t
	       (dolist (info *hash-table-tests*
			     (error 'simple-program-error
                                    :format-control "Unknown :TEST for MAKE-HASH-TABLE: ~S"
				    :format-arguments (list test)))
		 (destructuring-bind
		  (test-name test-fun hash-fun)
		  info
		  (when (or (eq test test-name) (eq test test-fun))
		    (return (values test-name test-fun hash-fun)))))))
      (let* ((size (max 36 size)) ; Needs to be at least 1, say 36.
	     (size+1 (1+ size))   ; The first element is not usable.
	     ;; Don't let rehash-threshold get too small to cause
	     ;; overflows or division by zero.  It's a hint, not a
	     ;; requirement, anyway.
	     (rehash-threshold (float (max 0.1 rehash-threshold) 1.0))
	     (scaled-size (round (/ (float size+1) rehash-threshold)))
	     (length (if (<= scaled-size 37) 37 (almost-primify scaled-size))))
	(declare (type index size+1 scaled-size length))
	#-gencgc
	(when weak-p
	  (format *debug-io* ";; Creating unsupported weak-p hash table~%"))
	#+gencgc
	(when (and (member weak-p '(t :key :key-and-value :key-or-value))
		   (not (member test '(eq eql))))
	  ;; I (rtoy) think the current GENCGC code really expects the
	  ;; test to be EQ, but doesn't enforce it in any way.  Let's
	  ;; warn about it for now.
	  ;;
	  ;; XXX: Either fix GC to work with other tests, or change
	  ;; this warning into an error.
	  (error "Cannot make a weak ~A hashtable with test: ~S" weak-p test))
	(let* ((index-vector
		(make-array length :element-type '(unsigned-byte 32)
			    :initial-element 0))
	       ;; Needs to be the same length as the KV vector
	       (next-vector 
		(make-array size+1 :element-type '(unsigned-byte 32)))
	       (kv-vector (make-array (* 2 size+1) :initial-element 'empty-hash-entry))
	       (table
		(%make-hash-table
		 :test test
		 :test-fun test-fun
		 :hash-fun hash-fun
		 :rehash-size rehash-size
		 :rehash-threshold rehash-threshold
		 :rehash-trigger size
		 :table kv-vector
		 :weak-p weak-p
		 :index-vector index-vector
		 :next-vector next-vector
		 :hash-vector (unless (eq test 'eq)
				(make-array size+1
					    :element-type '(unsigned-byte 32)
					    :initial-element +eq-based-hash-value+)))))
	  ;; Setup the free list, all free. These lists are 0
	  ;; terminated.
	  (do ((i 1 (1+ i)))
	      ((>= i size))
	    (setf (aref next-vector i) (1+ i)))
	  (setf (aref next-vector size) 0)
	  (setf (hash-table-next-free-kv table) 1)
	  (setf (hash-table-needing-rehash table) 0)
	  (setf (aref kv-vector 0) table)
	  table)))))


(declaim (inline hash-table-count))
(defun hash-table-count (hash-table)
  "Returns the number of entries in the given HASH-TABLE."
  (declare (type hash-table hash-table)
	   (values index))
  (hash-table-number-entries hash-table))

(setf (documentation 'hash-table-rehash-size 'function)
      "Return the rehash-size HASH-TABLE was created with.")

(setf (documentation 'hash-table-rehash-threshold 'function)
      "Return the rehash-threshold HASH-TABLE was created with.")

(declaim (inline hash-table-size))
(defun hash-table-size (hash-table)
  "Return a size that can be used with MAKE-HASH-TABLE to create a hash
   table that can hold however many entries HASH-TABLE can hold without
   having to be grown."
  (hash-table-rehash-trigger hash-table))

(setf (documentation 'hash-table-test 'function)
      "Return the test HASH-TABLE was created with.")

(setf (documentation 'hash-table-weak-p 'function)
      "Return T if HASH-TABLE will not keep entries for keys that would
   otherwise be garbage, and NIL if it will.")


;;;; Accessing functions.

;;; REHASH -- internal.
;;;
;;; Make new vectors for the table, extending the table based on the
;;; rehash-size.
;;;
(defun rehash (table)
  (declare (type hash-table table))
  (let* ((old-kv-vector (hash-table-table table))
	 (old-next-vector (hash-table-next-vector table))
	 (old-hash-vector (hash-table-hash-vector table))
	 (old-size (length old-next-vector))
	 (new-size
	  (let ((rehash-size (hash-table-rehash-size table)))
	    (etypecase rehash-size
	      (fixnum
	       (+ rehash-size old-size))
	      (float
	       (the (values index t) (round (* rehash-size old-size)))))))
	 (new-kv-vector (make-array (* 2 new-size) :initial-element 'empty-hash-entry))
	 (new-next-vector (make-array new-size
				      :element-type '(unsigned-byte 32)
				      :initial-element 0))
	 (new-hash-vector (when old-hash-vector
			    (make-array new-size
					:element-type '(unsigned-byte 32)
					:initial-element +eq-based-hash-value+)))
	 (old-index-vector (hash-table-index-vector table))
	 (new-length (almost-primify
		      (round (/ (float new-size)
				(hash-table-rehash-threshold table)))))
	 (new-index-vector (make-array new-length
				       :element-type '(unsigned-byte 32)
				       :initial-element 0)))
    (declare (type index new-size new-length old-size))

    ;; Disable GC tricks on the old-kv-vector.
    (set-header-data old-kv-vector vm:vector-normal-subtype)

    ;; Copy over the kv-vector, the element positions should not move
    ;; in case there are active scans.
    (dotimes (i (* old-size 2))
      (declare (type index i))
      (setf (aref new-kv-vector i) (aref old-kv-vector i)))

    ;; Copy over the hash-vector.
    (when old-hash-vector
      (dotimes (i old-size)
	(setf (aref new-hash-vector i) (aref old-hash-vector i))))
    
    (setf (hash-table-next-free-kv table) 0)
    (setf (hash-table-needing-rehash table) 0)
    ;; Rehash all the entries; last to first so that after the pushes
    ;; the chains are first to last.
    (do ((i (1- new-size) (1- i))
	 (empty (aref new-kv-vector 1)))
	((zerop i))
      (let ((key (aref new-kv-vector (* 2 i)))
	    (value (aref new-kv-vector (1+ (* 2 i)))))
	;; A slot is empty if both the key and the value are "empty",
	;; which is indicated by the value of kv_vector[1].
	(cond ((and (eq key empty) (eq value empty))
	       ;; Push this slot onto the free list.
	       (setf (aref new-next-vector i)
		     (hash-table-next-free-kv table))
	       (setf (hash-table-next-free-kv table) i))
	      ((and new-hash-vector
		    (not (= (aref new-hash-vector i) +eq-based-hash-value+)))
	       ;; Can use the existing hash value (not EQ based)
	       (let* ((hashing (aref new-hash-vector i))
		      (index (rem hashing new-length))
		      (next (aref new-index-vector index)))
		 (declare (type index index)
			  (type hash hashing))
		 ;; Push this slot into the next chain.
		 (setf (aref new-next-vector i) next)
		 (setf (aref new-index-vector index) i)))
	      (t
	       ;; EQ base hash.
	       ;; Enable GC tricks.
	       (set-header-data new-kv-vector vm:vector-valid-hashing-subtype)
	       (let* ((hashing (pointer-hash key))
		      (index (rem hashing new-length))
		      (next (aref new-index-vector index)))
		 (declare (type index index)
			  (type hash hashing))
		 ;; Push this slot onto the next chain.
		 (setf (aref new-next-vector i) next)
		 (setf (aref new-index-vector index) i))))))
    (setf (hash-table-table table) new-kv-vector)
    (setf (hash-table-index-vector table) new-index-vector)
    (setf (hash-table-next-vector table) new-next-vector)
    (setf (hash-table-hash-vector table) new-hash-vector)
    ;; Shrink the old vectors to 0 size to help the conservative GC.
    (shrink-vector old-kv-vector 0)
    (shrink-vector old-index-vector 0)
    (shrink-vector old-next-vector 0)
    (when old-hash-vector
      (shrink-vector old-hash-vector 0))
    (setf (hash-table-rehash-trigger table) new-size))
  (undefined-value))

;;; REHASH-WITHOUT-GROWING -- internal.
;;;
;;; Use the same size as before, re-using the vectors.
;;;
(defun rehash-without-growing (table)
  (declare (type hash-table table))
  (let* ((kv-vector (hash-table-table table))
	 (next-vector (hash-table-next-vector table))
	 (hash-vector (hash-table-hash-vector table))
	 (size (length next-vector))
	 (index-vector (hash-table-index-vector table))
	 (length (length index-vector)))
    (declare (type index size length)
	     (type (simple-array (unsigned-byte 32) (*))))
    
    ;; Disable GC tricks, they will be re-enabled during the re-hash
    ;; if necesary.
    (set-header-data kv-vector vm:vector-normal-subtype)

    ;; Rehash all the entries.
    (setf (hash-table-next-free-kv table) 0)
    (setf (hash-table-needing-rehash table) 0)
    (dotimes (i size)
      (setf (aref next-vector i) 0))
    (dotimes (i length)
      (setf (aref index-vector i) 0))
    (do ((i (1- size) (1- i))
	 (empty (aref kv-vector 1)))
	((zerop i))
      (let ((key (aref kv-vector (* 2 i)))
	    (value (aref kv-vector (1+ (* 2 i)))))
	;; A slot is empty if both the key and the value are "empty",
	;; which is indicated by the value of kv_vector[1].
	(cond ((and (eq key empty) (eq value empty))
	       ;; Push this slot onto the free list.
	       (setf (aref next-vector i) (hash-table-next-free-kv table))
	       (setf (hash-table-next-free-kv table) i))
	      ((and hash-vector (not (= (aref hash-vector i) +eq-based-hash-value+)))
	       ;; Can use the existing hash value (not EQ based)
	       (let* ((hashing (aref hash-vector i))
		      (index (rem hashing length))
		      (next (aref index-vector index)))
		 (declare (type index index))
		 ;; Push this slot into the next chain.
		 (setf (aref next-vector i) next)
		 (setf (aref index-vector index) i)))
	      (t
	       ;; EQ base hash.
	       ;; Enable GC tricks.
	       (set-header-data kv-vector vm:vector-valid-hashing-subtype)
	       (let* ((hashing (pointer-hash key))
		      (index (rem hashing length))
		      (next (aref index-vector index)))
		 (declare (type index index)
			  (type hash hashing))
		 ;; Push this slot into the next chain.
		 (setf (aref next-vector i) next)
		 (setf (aref index-vector index) i)))))))
  (undefined-value))

(defun flush-needing-rehash (table)
  (let* ((kv-vector (hash-table-table table))
	 (index-vector (hash-table-index-vector table))
	 (next-vector (hash-table-next-vector table))
	 (length (length index-vector)))
    (do ((next (hash-table-needing-rehash table)))
	((zerop next))
      (declare (type index next))
      (let* ((key (aref kv-vector (* 2 next)))
	     (hashing (pointer-hash key))
	     (index (rem hashing length))
	     (temp (aref next-vector next)))
	(setf (aref next-vector next) (aref index-vector index))
	(setf (aref index-vector index) next)
	(setf next temp))))
  (setf (hash-table-needing-rehash table) 0)
  (undefined-value))

;;; GETHASH -- Public.
;;; 
(defun gethash (key hash-table &optional default)
  "Finds the entry in HASH-TABLE whose key is KEY and returns the associated
   value and T as multiple values, or returns DEFAULT and NIL if there is no
   such entry.  Entries can be added using SETF."
  (declare (type hash-table hash-table)
	   (values t (member t nil)))
  (without-gcing
   (cond ((= (get-header-data (hash-table-table hash-table))
	     vm:vector-must-rehash-subtype)
	  (rehash-without-growing hash-table))
	 ((not (zerop (hash-table-needing-rehash hash-table)))
	  (flush-needing-rehash hash-table)))
   ;; Search for key in the hash table.
   (multiple-value-bind
	 (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (declare (type hash hashing))
     (let* ((index-vector (hash-table-index-vector hash-table))
	    (length (length index-vector))
	    (index (rem hashing length))
	    (next (aref index-vector index))
	    (table (hash-table-table hash-table))
	    (next-vector (hash-table-next-vector hash-table))
	    (hash-vector (hash-table-hash-vector hash-table))
	    (test-fun (hash-table-test-fun hash-table)))
       (declare (type index index))
       ;; Search next-vector chain for a matching key.
       (if (or eq-based (not hash-vector))
	   (do ((next next (aref next-vector next)))
	       ((zerop next) (values default nil))
	     (declare (type index next))
	     (when (eq key (aref table (* 2 next)))
	       (return (values (aref table (1+ (* 2 next))) t))))
	   (do ((next next (aref next-vector next)))
	       ((zerop next) (values default nil))
	     (declare (type index next))
	     (when (and (= hashing (aref hash-vector next))
			(funcall test-fun key (aref table (* 2 next))))
	       ;; Found.
	       (return (values (aref table (1+ (* 2 next))) t)))))))))

;;; So people can call #'(setf gethash).
;;;
(defun (setf gethash) (new-value key table &optional default)
  (declare (ignore default))
  (%puthash key table new-value))

;;; %PUTHASH -- public setf method.
;;; 
(defun %puthash (key hash-table value)
  (declare (type hash-table hash-table))
  (assert (hash-table-index-vector hash-table))
  (without-gcing
   ;; Need to rehash here so that a current key can be found if it
   ;; exists. Check that there is room for one more entry. May not be
   ;; needed if the key is already present.
   (cond ((zerop (hash-table-next-free-kv hash-table))
	  (rehash hash-table))
	 ((= (get-header-data (hash-table-table hash-table))
	     vm:vector-must-rehash-subtype)
	  (rehash-without-growing hash-table))
	 ((not (zerop (hash-table-needing-rehash hash-table)))
	  (flush-needing-rehash hash-table)))

   ;; Search for key in the hash table.
   (multiple-value-bind
	 (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (declare (type hash hashing))
     (let* ((index-vector (hash-table-index-vector hash-table))
	    (length (length index-vector))
	    (index (rem hashing length))
	    (next (aref index-vector index))
	    (kv-vector (hash-table-table hash-table))
	    (next-vector (hash-table-next-vector hash-table))
	    (hash-vector (hash-table-hash-vector hash-table))
	    (test-fun (hash-table-test-fun hash-table)))
       (declare (type index index))
       
       (cond ((or eq-based (not hash-vector))
	      (when eq-based
		(set-header-data kv-vector vm:vector-valid-hashing-subtype))

	      ;; Search next-vector chain for a matching key.
	      (do ((next next (aref next-vector next)))
		  ((zerop next))
		(declare (type index next))
		(when (eq key (aref kv-vector (* 2 next)))
		  ;; Found, just replace the value.
		  (setf (aref kv-vector (1+ (* 2 next))) value)
		  (return-from %puthash value))))
	     (t
	      ;; Search next-vector chain for a matching key.
	      (do ((next next (aref next-vector next)))
		  ((zerop next))
		(declare (type index next))
		(when (and (= hashing (aref hash-vector next))
			   (funcall test-fun key 
				    (aref kv-vector (* 2 next))))
		  ;; Found, just replace the value.
		  (setf (aref kv-vector (1+ (* 2 next))) value)
		  (return-from %puthash value)))))
	     
       ;; Pop a KV slot off the free list
       (let ((free-kv-slot (hash-table-next-free-kv hash-table)))
	 ;; Double-check for overflow.
	 (assert (not (zerop free-kv-slot)))
	 (setf (hash-table-next-free-kv hash-table)
	       (aref next-vector free-kv-slot))
	 (incf (hash-table-number-entries hash-table))
	 
	 (setf (aref kv-vector (* 2 free-kv-slot)) key)
	 (setf (aref kv-vector (1+ (* 2 free-kv-slot))) value)

	 ;; Setup the hash-vector if necessary.
	 (when hash-vector
	   (if (not eq-based)
	       (setf (aref hash-vector free-kv-slot) hashing)
	       (assert (= (aref hash-vector free-kv-slot) +eq-based-hash-value+))))

	 ;; Push this slot into the next chain.
	 (setf (aref next-vector free-kv-slot) next)
	 (setf (aref index-vector index) free-kv-slot)))))
  value)

;;; REMHASH -- public.
;;; 
(defun remhash (key hash-table)
  "Remove the entry in HASH-TABLE associated with KEY.  Returns T if there
   was such an entry, and NIL if not."
  (declare (type hash-table hash-table)
	   (values (member t nil)))
  (without-gcing
   ;; Need to rehash here so that a current key can be found if it
   ;; exists.
   (cond ((= (get-header-data (hash-table-table hash-table))
	     vm:vector-must-rehash-subtype)
	  (rehash-without-growing hash-table))
	 ((not (zerop (hash-table-needing-rehash hash-table)))
	  (flush-needing-rehash hash-table)))

   ;; Search for key in the hash table.
   (multiple-value-bind (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (declare (type hash hashing))
     (let* ((index-vector (hash-table-index-vector hash-table))
	    (length (length index-vector))
	    (index (rem hashing length))
	    (next (aref index-vector index))
	    (table (hash-table-table hash-table))
	    (next-vector (hash-table-next-vector hash-table))
	    (hash-vector (hash-table-hash-vector hash-table))
	    (test-fun (hash-table-test-fun hash-table))
	    (empty (aref table 1)))
       (declare (type index index next))
       (cond ((zerop next)
	      nil)
	     ((if (or eq-based (not hash-vector))
		  (eq key (aref table (* 2 next)))
		  (and (= hashing (aref hash-vector next))
		       (funcall test-fun key (aref table (* 2 next)))))
	      ;; Empty out Key and Value, by using the empty value in
	      ;; kv-vector[1].
	      (setf (aref table (* 2 next)) empty)
	      (setf (aref table (1+ (* 2 next))) empty)
	      ;; Update the index-vector pointer.
	      (setf (aref index-vector index) (aref next-vector next))
	      ;; Push KV slot onto free chain.
	      (setf (aref next-vector next)
		    (hash-table-next-free-kv hash-table))
	      (setf (hash-table-next-free-kv hash-table) next)
	      (when hash-vector
		(setf (aref hash-vector next) +eq-based-hash-value+))
	      (decf (hash-table-number-entries hash-table))
	      t)
	     ;; Search next-vector chain for a matching key.
	     ((or eq-based (not hash-vector))
	      ;; EQ based.
	      (do ((prior next next)
		   (next (aref next-vector next) (aref next-vector next)))
		  ((zerop next) nil)
		(declare (type index next))
		(when (eq key (aref table (* 2 next)))
		  ;; Empty out Key and Value by using the empty value
		  ;; in kv_vector[1].
		  (setf (aref table (* 2 next)) empty)
		  (setf (aref table (1+ (* 2 next))) empty)
		  ;; Update the prior pointer in the chain to skip this.
		  (setf (aref next-vector prior) (aref next-vector next))
		  ;; Push KV slot onto free chain.
		  (setf (aref next-vector next)
			(hash-table-next-free-kv hash-table))
		  (setf (hash-table-next-free-kv hash-table) next)
		  (when hash-vector
		    (setf (aref hash-vector next) +eq-based-hash-value+))
		  (decf (hash-table-number-entries hash-table))
		  (return t))))
	     (t
	      ;; Not EQ based
	      (do ((prior next next)
		   (next (aref next-vector next) (aref next-vector next)))
		  ((zerop next) nil)
		(declare (type index next))
		(when (and (= hashing (aref hash-vector next))
			   (funcall test-fun key (aref table (* 2 next))))
		  ;; Empty out Key and Value by using the empty value
		  ;; in kv_vector[1].
		  (setf (aref table (* 2 next)) empty)
		  (setf (aref table (1+ (* 2 next))) empty)
		  ;; Update the prior pointer in the chain to skip this.
		  (setf (aref next-vector prior) (aref next-vector next))
		  ;; Push KV slot onto free chain.
		  (setf (aref next-vector next)
			(hash-table-next-free-kv hash-table))
		  (setf (hash-table-next-free-kv hash-table) next)
		  (when hash-vector
		    (setf (aref hash-vector next) +eq-based-hash-value+))
		  (decf (hash-table-number-entries hash-table))
		  (return t)))))))))

;;; CLRHASH -- public.
;;; 
(defun clrhash (hash-table)
  "This removes all the entries from HASH-TABLE and returns the hash table
   itself."
  (let* ((kv-vector (hash-table-table hash-table))
	 (kv-length (length kv-vector))
	 (next-vector (hash-table-next-vector hash-table))
	 (hash-vector (hash-table-hash-vector hash-table))
	 (size (length next-vector))
	 (index-vector (hash-table-index-vector hash-table))
	 (length (length index-vector)))
    ;; Disable GC tricks.
    (set-header-data kv-vector vm:vector-normal-subtype)
    ;; Empty out the Keys and Values by using the empty value in
    ;; kv_vector[1].
    (do ((i 2 (1+ i))
	 (empty (aref kv-vector 1)))
	((>= i kv-length))
      (setf (aref kv-vector i) empty))
    (assert (eq (aref kv-vector 0) hash-table))
    ;; Setup the free list, all free.
    (do ((i 1 (1+ i)))
	((>= i (1- size)))
      (setf (aref next-vector i) (1+ i)))
    (setf (aref next-vector (1- size)) 0)
    (setf (hash-table-next-free-kv hash-table) 1)
    (setf (hash-table-needing-rehash hash-table) 0)
    ;; Clear the index-vector
    (dotimes (i length)
      (setf (aref index-vector i) 0))
    ;; Clear the hash-vector
    (when hash-vector
      (dotimes (i size)
	(setf (aref hash-vector i) +eq-based-hash-value+))))
  (setf (hash-table-number-entries hash-table) 0)
  hash-table)

;;; CLOBBER-HASH -- public.
;;; 
(defun clobber-hash (hash-table)
  "This removes all the entries from HASH-TABLE and returns the hash table
   itself, shrinking the size to free memory."
  (let* ((old-kv-vector (hash-table-table hash-table))
	 (old-index-vector (hash-table-index-vector hash-table))
	 (old-next-vector (hash-table-next-vector hash-table))
	 (old-hash-vector (hash-table-hash-vector hash-table))
	 (new-size 37)
	 (new-kv-vector (make-array (* 2 new-size) :initial-element 'empty-hash-entry))
	 (new-next-vector (make-array new-size
				      :element-type '(unsigned-byte 32)
				      :initial-element 0))
	 (new-hash-vector
	  (when old-hash-vector
	    (make-array new-size :element-type '(unsigned-byte 32)
			:initial-element +eq-based-hash-value+)))
	 (new-length 37)
	 (new-index-vector (make-array new-length
				       :element-type '(unsigned-byte 32)
				       :initial-element 0)))
    (declare (type index new-size new-length))
    ;; Disable GC tricks.
    (set-header-data old-kv-vector vm:vector-normal-subtype)
    ;; Setup the free list, all free.
    (do ((i 1 (1+ i)))
	((>= i (1- new-size)))
      (setf (aref new-next-vector i) (1+ i)))
    (setf (aref new-next-vector (1- new-size)) 0)
    (setf (hash-table-rehash-trigger hash-table) new-size)
    (setf (hash-table-next-free-kv hash-table) 1)
    (setf (hash-table-needing-rehash hash-table) 0)
    (setf (hash-table-number-entries hash-table) 0)
    (setf (hash-table-table hash-table) new-kv-vector)
    (setf (hash-table-index-vector hash-table) new-index-vector)
    (setf (hash-table-next-vector hash-table) new-next-vector)
    (setf (hash-table-hash-vector hash-table) new-hash-vector)
    ;; Shrink the old vectors to 0 size to help the conservative GC.
    (shrink-vector old-kv-vector 0)
    (shrink-vector old-index-vector 0)
    (shrink-vector old-next-vector 0)
    (when old-hash-vector
      (shrink-vector old-hash-vector 0)))
  hash-table)


;;;; MAPHASH and WITH-HASH-TABLE-ITERATOR

(declaim (maybe-inline maphash))
(defun maphash (map-function hash-table)
  "For each entry in HASH-TABLE, calls MAP-FUNCTION on the key and value
   of the entry; returns NIL."
  (declare (type (or function symbol) map-function)
	   (type hash-table hash-table))
  (let ((fun (etypecase map-function
	       (function
		map-function)
	       (symbol
		(symbol-function map-function))))
	(size (length (hash-table-next-vector hash-table)))
	(kv-vector (hash-table-table hash-table)))
    (declare (type function fun))
    (do ((i 1 (1+ i))
	 (empty (aref kv-vector 1)))
	((>= i size))
      (declare (type index i))
      (let* ((key (aref kv-vector (* 2 i)))
	     (value (aref kv-vector (1+ (* 2 i)))))
	;; X hack
	(unless (and (eq key empty) (eq value empty))
	  (funcall fun key value))))))

(defmacro with-hash-table-iterator ((function hash-table) &body body)
  "WITH-HASH-TABLE-ITERATOR ((function hash-table) &body body)
   provides a method of manually looping over the elements of a hash-table.
   FUNCTION is bound to a generator-macro that, withing the scope of the
   invocation, returns one or three values. The first value tells whether
   any objects remain in the hash table. When the first value is non-NIL, 
   the second and third values are the key and the value of the next object."
  (let ((n-function (gensym "WITH-HASH-TABLE-ITERRATOR-")))
    `(let ((,n-function
	    (let* ((table ,hash-table)
		   (length (length (hash-table-next-vector table)))
		   (index 1))
              (declare (type (integer 0 #.(1- (floor most-positive-fixnum 2))) index))
	      (labels
		  ((,function ()
		     ;; Grab the table again on each iteration just
		     ;; in case it was rehashed by a PUTHASH.
		     (let ((kv-vector (hash-table-table table)))
		       (do ((empty (aref kv-vector 1)))
			   ((>= index length) (values nil))
			 (let ((key (aref kv-vector (* 2 index)))
			       (value (aref kv-vector (1+ (* 2 index)))))
			   (incf index)
			   (unless (and (eq key empty) (eq value empty))
			     (return (values t key value))))))))
		#',function))))
      (macrolet ((,function () '(funcall ,n-function)))
	,@body))))



;;;; SXHASH and support functions

;;; The maximum length and depth to which we hash lists.
(defconstant sxhash-max-len 7)
(defconstant sxhash-max-depth 3)

(eval-when (compile eval)

(defconstant sxhash-bits-byte (byte 29 0))
(defconstant sxmash-total-bits 29)
(defconstant sxmash-rotate-bits 9)

(defmacro sxmash (place with)
  `(setf ,place
	 (logxor (truly-the hash
			    (ash ,place
				 ,(- sxmash-rotate-bits sxmash-total-bits)))
		 (truly-the hash
			    (ash (logand
				  ,place
				  ,(1- (ash 1
					    (- sxmash-total-bits
					       sxmash-rotate-bits))))
				 ,sxmash-rotate-bits))
		 (truly-the hash ,with))))

(defmacro sxhash-simple-string (sequence)
  `(%sxhash-simple-string ,sequence))

(defmacro sxhash-string (sequence)
  (let ((data (gensym))
	(start (gensym))
	(end (gensym))
	(fill-end (gensym)))
    (once-only ((n-sequence sequence))
      `(let ((,fill-end (if (array-has-fill-pointer-p ,n-sequence)
			    (fill-pointer ,n-sequence)
			    nil)))
	 (with-array-data ((,data (the (values string &rest t) ,n-sequence))
			   (,start)
			   (,end ,fill-end))
	   (if (zerop ,start)
	       (%sxhash-simple-substring ,data ,end)
	       (sxhash-simple-string (coerce (the (values string &rest t)
					       ,n-sequence)
					     'simple-string))))))))


(defmacro sxhash-list (sequence depth &key (equalp nil))
  `(if (= ,depth sxhash-max-depth)
       0
       (do ((sequence ,sequence (cdr (the list sequence)))
	    (index 0 (1+ index))
	    (hash 2)
	    (,depth (1+ ,depth)))
	   ((or (atom sequence) (= index sxhash-max-len)) hash)
	 (declare (fixnum hash index))
	 (sxmash hash (,(if equalp 'internal-equalp-hash 'internal-sxhash)
			(car sequence) ,depth)))))

(defmacro sxhash-bit-vector (vector)
  `(let* ((length (length ,vector))
	  (hash length))
     (declare (type index length) (type hash hash))
     (dotimes (index (min length sxhash-max-len) hash)
       (declare (type index index))
       (sxmash hash (bit ,vector index)))))

); eval-when (compile eval)


(defun internal-sxhash (s-expr depth)
  (declare (type index depth) (values hash))
  (typecase s-expr
    ;; The pointers and immediate types.
    (cons (sxhash-list s-expr depth))
    (fixnum (ldb sxhash-bits-byte s-expr))
    (character (char-code (char-upcase s-expr)))
    (pathname
     ;; Pathnames are EQUAL if all the components are EQUAL, so we
     ;; hash all of the components of a pathname together.
     (let ((hash (internal-sxhash (%pathname-host s-expr) depth)))
       (sxmash hash (internal-sxhash (%pathname-device s-expr) depth))
       (sxmash hash (internal-sxhash (%pathname-directory s-expr) depth))
       (sxmash hash (internal-sxhash (%pathname-name s-expr) depth))
       (sxmash hash (internal-sxhash (%pathname-type s-expr) depth))
       ;; Hash :newest the same as NIL because EQUAL for pathnames
       ;; assumes that :newest and nil are equal.
       (let ((version (%pathname-version s-expr)))
	 (sxmash hash (internal-sxhash (if (eql version :newest) nil version) depth)))))
    (instance
     (if (or (typep s-expr 'structure-object)
	     (typep s-expr 'condition))
	 (internal-sxhash (%class-name (layout-class (%instance-layout s-expr)))
			  depth)
	 (sxhash-instance s-expr)))
    (funcallable-instance
     (if (eval:interpreted-function-p s-expr)
	 (internal-sxhash (eval:interpreted-function-lambda-expression s-expr)
			  depth)
	 (sxhash-instance s-expr)))
    ;; Other-pointer types.
    (simple-string (sxhash-simple-string s-expr))
    (symbol #-(or sparc x86 ppc) (sxhash-simple-string (symbol-name s-expr))
	    #+(or sparc x86 ppc) (sxhash s-expr))
    (number
     (etypecase s-expr
       (integer (ldb sxhash-bits-byte s-expr))
       (single-float
	;; CLHS says sxhash must return the same thing for +0.0 and
	;; -0.0.  We get the desired result by adding +0.0, which
	;; converts -0.0 to 0.0.
	(let* ((x (+ s-expr 0f0))
	       (bits (single-float-bits x)))
	  (ldb sxhash-bits-byte
	       (logxor (ash bits (- sxmash-rotate-bits)) bits))))
       (double-float
	(let* ((x (+ s-expr 0d0))
	       (lo (double-float-low-bits x))
	       (hi (double-float-high-bits x)))
	  (ldb sxhash-bits-byte
	       (logxor (ash lo (- sxmash-rotate-bits)) lo
		       (ldb sxhash-bits-byte
			    (logxor (ash hi (- sxmash-rotate-bits)) hi))))))
       #+long-float
       (long-float
	(let ((lo (long-float-low-bits s-expr))
	      #+sparc (mid (long-float-mid-bits s-expr))
	      (hi (long-float-high-bits s-expr))
	      (exp (long-float-exp-bits s-expr)))
	  (ldb sxhash-bits-byte
	       (logxor (ash lo (- sxmash-rotate-bits)) lo
		       #+sparc (ash mid (- sxmash-rotate-bits)) #+sparc mid
		       (ash hi (- sxmash-rotate-bits)) hi
		       (ldb sxhash-bits-byte
			    (logxor (ash exp (- sxmash-rotate-bits)) exp))))))
       #+double-double
       (double-double-float
	;; Is this good enough?
	(logxor (internal-sxhash (kernel:double-double-hi s-expr) depth)
		(internal-sxhash (kernel:double-double-lo s-expr) depth)))
       (ratio (logxor (internal-sxhash (numerator s-expr) 0)
		      (internal-sxhash (denominator s-expr) 0)))
       (complex (logxor (internal-sxhash (realpart s-expr) 0)
			(internal-sxhash (imagpart s-expr) 0)))))
    (array
     (typecase s-expr
       (string (sxhash-string s-expr))
       (simple-bit-vector (sxhash-bit-vector
			   (truly-the simple-bit-vector s-expr)))
       (bit-vector (sxhash-bit-vector (truly-the bit-vector s-expr)))
       (t (array-rank s-expr))))
    ;; Everything else.
    (t 42)))

(defun sxhash (s-expr)
  "Computes a hash code for S-EXPR and returns it as an integer."
  (internal-sxhash s-expr 0))


;;;; Equalp hash.

(eval-when (compile eval)

(defmacro hash-table-equalp-hash (table)
  `(let ((hash (hash-table-count ,table)))
     (declare (type hash hash))
     (sxmash hash (sxhash (hash-table-test ,table)))
     hash))

(defmacro structure-equalp-hash (structure depth)
  `(if (= ,depth sxhash-max-depth)
       0
       (let* ((layout (%instance-layout ,structure))
	      (length (min (1- (layout-length layout)) sxhash-max-len))
	      (hash (internal-sxhash (%class-name (layout-class layout))
				     depth))
	      (,depth (+ ,depth 1)))
	 (declare (type index length) (type hash hash))
	 (do ((index 1 (1+ index)))
	     ((= index length) hash)
	   (declare (type index index))
	   (sxmash hash (internal-equalp-hash
			 (%instance-ref ,structure index) ,depth))))))

(defmacro vector-equalp-hash (vector depth)
  `(if (= ,depth sxhash-max-depth)
       0
       (let* ((length (length ,vector))
	      (hash length)
	      (,depth (+ ,depth 1)))
	 (declare (type index length) (type hash hash))
	 (dotimes (index (min length sxhash-max-len) hash)
	   (declare (type index index))
	   (sxmash hash (internal-equalp-hash (aref ,vector index) ,depth))))))

(defmacro array-equalp-hash (array depth)
  `(if (= ,depth sxhash-max-depth)
       0
       (let* ((size (array-total-size ,array))
	      (hash size)
	      (,depth (+ ,depth 1)))
	 (declare (type hash hash))
	 (dotimes (index (min sxhash-max-len size) hash)
	   (sxmash hash (internal-equalp-hash
			 (row-major-aref ,array index) ,depth))))))

); eval-when (compile eval)


(defun internal-equalp-hash (s-expr depth)
  (declare (type index depth) (values hash))
  (typecase s-expr
    ;; The pointers and immediate types.
    (cons (sxhash-list s-expr depth :equalp t))
    (fixnum (ldb sxhash-bits-byte s-expr))
    (character (char-code (char-upcase s-expr)))
    (instance
     (typecase s-expr
       (hash-table (hash-table-equalp-hash s-expr))
       (structure-object (structure-equalp-hash s-expr depth))
       (t 42)))
    ;; Other-pointer types.
    (simple-string (vector-equalp-hash (truly-the simple-string s-expr) depth))
    (symbol (sxhash-simple-string (symbol-name s-expr)))
    (number
     (etypecase s-expr
       (integer (sxhash s-expr))
       (float
	(macrolet ((frob (val type)
		     (let ((lo (coerce most-negative-fixnum type))
			   (hi (coerce most-positive-fixnum type)))
		       `(if (<= ,lo ,val ,hi)
			    (multiple-value-bind (q r)
				(truncate ,val)
			      (if (zerop r)
				  (sxhash q)
				  (sxhash (coerce ,val 'long-float))))
			    (multiple-value-bind (q r)
				(truncate ,val)
			      (if (zerop r)
				  (sxhash q)
				  (sxhash (coerce ,val 'long-float))))))))
	  (etypecase s-expr
	    (single-float (frob s-expr single-float))
	    (double-float (frob s-expr double-float))
	    #+long-float (long-float (frob s-expr long-float)))))
       (ratio
	(let ((float (coerce s-expr 'long-float)))
	  (if (= float s-expr)
	      (sxhash float)
	      (sxhash s-expr))))
       (complex (if (zerop (imagpart s-expr))
		    (internal-equalp-hash (realpart s-expr) 0)
		    (logxor (internal-equalp-hash (realpart s-expr) 0)
			    (internal-equalp-hash (realpart s-expr) 0))))))
    (array
     (typecase s-expr
       (simple-vector (vector-equalp-hash (truly-the simple-vector s-expr) depth))
       (vector (vector-equalp-hash s-expr depth))
       (t (array-equalp-hash s-expr depth))))
    ;; Everything else.
    (t 42)))


;;;; Dumping one as a constant.

(defun make-hash-table-load-form (table)
  (values
   `(make-hash-table
     :test ',(hash-table-test table) :size ',(hash-table-size table)
     :rehash-size ',(hash-table-rehash-size table)
     :rehash-threshold ',(hash-table-rehash-threshold table)
     :weak-p (hash-table-weak-p table))
   (let ((values nil))
     (declare (inline maphash))
     (maphash #'(lambda (key value)
		  (push (cons key value) values))
	      table)
     (if values
	 `(stuff-hash-table ,table ',values)
	 nil))))

(defun stuff-hash-table (table alist)
  (dolist (x alist)
    (setf (gethash (car x) table) (cdr x))))
