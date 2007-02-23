;;; -*- Package: CL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/hash.lisp,v 1.43 2003/06/18 09:23:11 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Hashing and hash table functions for Spice Lisp.
;;; Originally written by Skef Wholey.
;;; Everything except SXHASH rewritten by William Lott.
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
  ;; (* rehash-threshold (length table)), saved here so we don't have to keep
  ;; recomputing it.
  (rehash-trigger (required-argument) :type index)
  ;;
  ;; The current number of entries in the table.
  (number-entries 0 :type index)
  ;;
  ;; Vector of ht-buckets.
  (table (required-argument) :type simple-vector)
  ;;
  ;; True if this is a weak hash table, meaning that key->value mappings will
  ;; disappear if there are no other references to the key.  Note: this only
  ;; matters if the hash function indicates that the hashing is EQ based.
  (weak-p nil :type (member t nil))
  ;;
  #+gengc
  ;; Chain of buckets that need to be rehashed because their hashing is EQ
  ;; based and the key has been moved by the garbage collector.
  (needing-rehash nil :type (or null hash-table-bucket)))
;;;
(defun %print-hash-table (ht stream depth)
  (declare (ignore depth) (stream stream))
  (print-unreadable-object (ht stream :identity t)
    (format stream "~A hash table, ~D entr~@:P"
	    (symbol-name (hash-table-test ht))
	    (hash-table-number-entries ht))))

(defconstant max-hash most-positive-fixnum)

(deftype hash ()
  `(integer 0 ,max-hash))


(defstruct (hash-table-bucket
	    (:print-function %print-hash-table-bucket))
  ;;
  ;; The hashing associated with key, kept around so we don't have to recompute
  ;; it each time.  In the non-gengc system, if this is NIL it means that the
  ;; hashing is EQ based, so use the address of the value.  If the gengc
  ;; system, we use the presence of the scavhook to tell that.
  #-gengc (hash nil :type (or hash null))
  #+gengc (hash 0 :type hash)
  ;;
  ;; The key and value, originally supplied by the user.  If the hash table
  ;; is weak, and this is eq based, then the key is really a weak pointer to
  ;; the key.
  (key nil :type t)
  (value nil :type t)
  ;;
  ;; The next bucket, or NIL if there are no more.
  (next nil :type (or hash-table-bucket null)))
;;;
(defun %print-hash-table-bucket (bucket stream depth)
  (declare (ignore depth))
  (print-unreadable-object (bucket stream :type t)
    (format stream "for ~S->~S~@[ ~D~]"
	    (hash-table-bucket-key bucket)
	    (hash-table-bucket-value bucket)
	    (hash-table-bucket-hash bucket))))

#+gengc
(defstruct (hash-table-eq-bucket
	    (:include hash-table-bucket))
  ;;
  ;; The scavenger-hook object used to detect when the EQ hashing of key will
  ;; change.  Only NIL during creation.
  (scavhook nil :type (or null scavenger-hook))
  ;;
  ;; True iff this bucket is still linked into the corresponding hash table's
  ;; vector.
  (linked nil :type (member t nil)))

#|

;;; SCAN-STATE -- defstruct.
;;;
;;; Holds the state of a MAPHASH or WITH-HASH-TABLE-ITERATOR.
;;;
(defstruct (scan-state)
  ;;
  ;; The index into the hash-table-table.
  (index 0 :type index)
  ;;
  ;; The current bucket in that chain.
  (bucket nil :type (or null hash-table-bucket))
  ;;
  )

;;; Non-gengc:
;;;
;;; %puthash: if there are any active scans, then make sure the current bucket
;;; for each scan holds the key we are trying to puthash, and flame out of it
;;; isn't.  Given that we have our hands directly on the correct bucket, just 
;;; go for it.
;;; 
;;; remhash: make the same check as with %puthash.  If it checks out, then
;;; just scan down the correct bucket chain and yank it.
;;;
;;; rehash: because of the above two tests, rehash will only be called by
;;; gethash.  And we need to do the rehash in order to look anything up.  So
;;; make a list of all the remaining buckets, and stick them in the scan-state.
;;;
;;; Gengc:
;;;
;;; %puthash & remhash: same as above.
;;;
;;; rehash: is only ever called by puthash, so doesn't need anything special to
;;; account for active scans.
;;;
;;; flush-needing-rehash: will only be called by gethash for the same reason
;;; rehash is only called by gethash in the non-gengc system.  And basically
;;; needs to do the same thing rehash does in the non-gengc system.
;;;
;;; hash-table-scavenger-hook: needs to check to see if the bucket being
;;; unlinked is after the current bucket in any of the active scans.  If so,
;;; it needs to add it to a list of buckets that will be processed after all
;;; the buckets visable in the hash-table-table have been delt with.

|#


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
			     (rehash-threshold 1) (weak-p nil))
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
     :WEAK-P -- If T, don't keep entries if the key would otherwise be
       garbage."
  (declare (type (or function symbol) test)
	   (type index size) (type (member t nil) weak-p))
  (let ((rehash-size (if (integerp rehash-size)
			 rehash-size
			 (float rehash-size 1.0)))
	(rehash-threshold (float rehash-threshold 1.0)))
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
			     (error "Unknown :TEST for MAKE-HASH-TABLE: ~S"
				    test))
		 (destructuring-bind
		  (test-name test-fun hash-fun)
		  info
		  (when (or (eq test test-name) (eq test test-fun))
		    (return (values test-name test-fun hash-fun)))))))
      (let* ((scaled-size (round (/ (float size) rehash-threshold)))
	     (length (if (<= scaled-size 37) 37 (almost-primify scaled-size)))
	     (vector (make-array length :initial-element nil)))
	(declare (type index scaled-size length)
		 (type simple-vector vector))
	(%make-hash-table
	 :test test
	 :test-fun test-fun
	 :hash-fun hash-fun
	 :rehash-size rehash-size
	 :rehash-threshold rehash-threshold
	 :rehash-trigger (round (* (float length) rehash-threshold))
	 :table vector
	 :weak-p weak-p)))))

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
;;; Make a new vector for TABLE.  If GROW is NIL, use the same size as before,
;;; otherwise extend the table based on the rehash-size.
;;;
(defun rehash (table grow)
  (declare (type hash-table table))
  (let* ((old-vector (hash-table-table table))
	 (old-length (length old-vector))
	 (new-length
	  (if grow
	      (let ((rehash-size (hash-table-rehash-size table)))
		(etypecase rehash-size
		  (fixnum
		   (+ rehash-size old-length))
		  (float
		   (the (values index t) (round (* rehash-size old-length))))))
	      old-length))
	 (new-vector (make-array new-length :initial-element nil))
	 #-gengc (weak-p (hash-table-weak-p table)))
    (declare (type index new-length))
    (dotimes (i old-length)
      (declare (type index i))
      (do ((bucket (svref old-vector i) next)
	   (next nil))
	  ((null bucket))
	(setf next (hash-table-bucket-next bucket))
	(block deal-with-one-bucket
	  (let* ((hashing
		  #-gengc
		  (or (hash-table-bucket-hash bucket)
		      (let ((key (hash-table-bucket-key bucket)))
			(set-header-data new-vector
					 vm:vector-valid-hashing-subtype)
			(if weak-p
			    (multiple-value-bind
				(real-key valid)
				(weak-pointer-value key)
			      (cond (valid
				     (pointer-hash real-key))
				    (t
				     (decf (hash-table-number-entries table))
				     (return-from deal-with-one-bucket nil))))
			    (pointer-hash key))))
		  #+gengc (hash-table-bucket-hash bucket))
		 (index (rem hashing new-length)))
	    (declare (type index hashing index))
	    (setf (hash-table-bucket-next bucket) (svref new-vector index))
	    (setf (svref new-vector index) bucket))))
      ;; We clobber the old vector contents so that if it is living in
      ;; static space it won't keep ahold of pointers into dynamic space.
      (setf (svref old-vector i) nil))
    (setf (hash-table-table table) new-vector)
    (unless (= new-length old-length)
      (setf (hash-table-rehash-trigger table)
	    (round (* (hash-table-rehash-threshold table)
		      (float new-length))))))
  (undefined-value))

#+gengc
(defun flush-needing-rehash (table)
  (let* ((weak-p (hash-table-weak-p table))
	 (vector (hash-table-table table))
	 (length (length vector)))
    (do ((bucket (hash-table-needing-rehash table) next)
	 (next nil))
	((null bucket))
      (setf next (hash-table-bucket-next bucket))
      (flet ((relink-bucket (key)
	       (let* ((hashing (pointer-hash key))
		      (index (rem hashing length)))
		 (setf (hash-table-bucket-hash bucket) hashing)
		 (setf (hash-table-bucket-next bucket) (svref vector index))
		 (setf (svref vector index) bucket)
		 (setf (hash-table-eq-bucket-linked bucket) t))))
	(let ((key (hash-table-bucket-key bucket)))
	  (if weak-p
	      (multiple-value-bind
		  (real-key valid)
		  (weak-pointer-value key)
		(if valid
		    (relink-bucket real-key)
		    (decf (hash-table-number-entries table))))
	      (relink-bucket key))))))
  (setf (hash-table-needing-rehash table) nil)
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
   #-gengc
   (when (= (get-header-data (hash-table-table hash-table))
	    vm:vector-must-rehash-subtype)
     (rehash hash-table nil))
   #+gengc
   (when (hash-table-needing-rehash hash-table)
     (flush-needing-rehash hash-table))
   (multiple-value-bind
       (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (let* ((vector (hash-table-table hash-table))
	    (length (length vector))
	    (index (rem hashing length)))
       (declare (type index hashing))
       (if eq-based
	   (if (hash-table-weak-p hash-table)
	       (do ((bucket (svref vector index)
			    (hash-table-bucket-next bucket)))
		   ((null bucket) (values default nil))
		 (when #+gengc (hash-table-eq-bucket-p bucket)
		       #-gengc (null (hash-table-bucket-hash bucket))
		   (multiple-value-bind
		       (bucket-key valid)
		       (weak-pointer-value (hash-table-bucket-key bucket))
		     (assert valid)
		     (when (eq key bucket-key)
		       (return (values (hash-table-bucket-value bucket) t))))))
	       (do ((bucket (svref vector index)
			    (hash-table-bucket-next bucket)))
		   ((null bucket) (values default nil))
		 (when (eq key (hash-table-bucket-key bucket))
		   (return (values (hash-table-bucket-value bucket) t)))))
	   (do ((test-fun (hash-table-test-fun hash-table))
		(bucket (svref vector index) (hash-table-bucket-next bucket)))
	       ((null bucket) (values default nil))
	     (let ((bucket-hashing (hash-table-bucket-hash bucket)))
	       (when (and #-gengc bucket-hashing
			  (= bucket-hashing hashing)
			  #+gengc (not (hash-table-eq-bucket-p bucket))
			  (funcall test-fun key
				   (hash-table-bucket-key bucket)))
		 (return (values (hash-table-bucket-value bucket) t))))))))))


#+gengc
(defun get-hash-table-scavenger-hook (hash-table bucket)
  (declare (type hash-table hash-table)
	   (type hash-table-eq-bucket bucket))
  (flet ((hash-table-scavenger-hook ()
	   (when (hash-table-eq-bucket-linked bucket)
	     (let* ((vector (hash-table-table hash-table))
		    (length (length vector))
		    (index (rem (hash-table-eq-bucket-hash bucket) length)))
	       (declare (type index index))
	       (do ((prev nil next)
		    (next (svref vector index) (hash-table-bucket-next next)))
		   ((null next)
		    (warn "Couldn't find where ~S was linked inside ~S"
			  bucket hash-table))
		 (when (eq next bucket)
		   (if prev
		       (setf (hash-table-bucket-next prev)
			     (hash-table-bucket-next bucket))
		       (setf (svref vector index)
			     (hash-table-bucket-next bucket)))
		   (setf (hash-table-eq-bucket-linked bucket) nil)
		   (return)))
	       (if (and (hash-table-weak-p hash-table)
			(not (nth-value 1
					(weak-pointer-value
					 (hash-table-bucket-key bucket)))))
		   (decf (hash-table-number-entries hash-table))
		   (setf (hash-table-bucket-next bucket)
			   (hash-table-needing-rehash hash-table)
			 (hash-table-needing-rehash hash-table)
			   bucket))))))
    #'hash-table-scavenger-hook))

;;; So people can call #'(setf gethash).
;;;
(defun (setf gethash) (new-value key table &optional default)
  (declare (ignore default))
  (%puthash key table new-value))

;;; %PUTHASH -- public setf method.
;;; 
(defun %puthash (key hash-table value)
  (declare (type hash-table hash-table))
  (without-gcing
   (let ((entries (1+ (hash-table-number-entries hash-table))))
     (setf (hash-table-number-entries hash-table) entries)
     (cond ((> entries (hash-table-rehash-trigger hash-table))
	    (rehash hash-table t))
	   #-gengc
	   ((= (get-header-data (hash-table-table hash-table))
	       vm:vector-must-rehash-subtype)
	    (rehash hash-table nil))))
   #+gengc
   (when (hash-table-needing-rehash hash-table)
     (flush-needing-rehash hash-table))
   (multiple-value-bind
       (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (declare (type hash hashing))
     (let* ((vector (hash-table-table hash-table))
	    (length (length vector))
	    (index (rem hashing length))
	    (first-bucket (svref vector index)))
       (declare (type index index))
       (block scan
	 (if eq-based
	     (if (hash-table-weak-p hash-table)
		 (do ((bucket first-bucket (hash-table-bucket-next bucket)))
		     ((null bucket))
		   (when #+gengc (hash-table-eq-bucket-p bucket)
		         #-gengc (null (hash-table-bucket-hash bucket))
		     (multiple-value-bind
			 (bucket-key valid)
			 (weak-pointer-value (hash-table-bucket-key bucket))
		       (assert valid)
		       (when (eq key bucket-key)
			 (setf (hash-table-bucket-value bucket) value)
			 (decf (hash-table-number-entries hash-table))
			 (return-from scan nil)))))
		 (do ((bucket first-bucket (hash-table-bucket-next bucket)))
		     ((null bucket))
		   (when (eq key (hash-table-bucket-key bucket))
		     (setf (hash-table-bucket-value bucket) value)
		     (decf (hash-table-number-entries hash-table))
		     (return-from scan nil))))
	     (do ((test-fun (hash-table-test-fun hash-table))
		  (bucket first-bucket (hash-table-bucket-next bucket)))
		 ((null bucket))
	       (let ((bucket-hashing (hash-table-bucket-hash bucket)))
		 (when (and #-gengc bucket-hashing
			    (= bucket-hashing hashing)
			    #+gengc (not (hash-table-eq-bucket-p bucket))
			    (funcall test-fun
				     key
				     (hash-table-bucket-key bucket)))
		   (setf (hash-table-bucket-value bucket) value)
		   (decf (hash-table-number-entries hash-table))
		   (return-from scan nil)))))
	 #-gengc
	 (when eq-based
	   (set-header-data vector vm:vector-valid-hashing-subtype))
	 (setf (svref vector index)
	       #-gengc
	       (if eq-based
		   (make-hash-table-bucket
		    :hash nil
		    :key (if (hash-table-weak-p hash-table)
			     (make-weak-pointer key)
			     key)
		    :value value
		    :next first-bucket)
		   (make-hash-table-bucket
		    :hash hashing
		    :key key
		    :value value
		    :next first-bucket))
	       #+gengc
	       (if eq-based
		   (let ((bucket (make-hash-table-eq-bucket
				  :hash hashing
				  :key (if (hash-table-weak-p hash-table)
					   (make-weak-pointer key)
					   key)
				  :value value
				  :next first-bucket
				  :linked t)))
		     (setf (hash-table-eq-bucket-scavhook bucket)
			   (make-scavenger-hook
			    :value key
			    :function (get-hash-table-scavenger-hook
				       hash-table bucket)))
		     bucket)
		   (make-hash-table-bucket
		    :hash hashing
		    :key key
		    :value value
		    :next first-bucket)))))))
  value)

;;; REMHASH -- public.
;;; 
(defun remhash (key hash-table)
  "Remove the entry in HASH-TABLE associated with KEY.  Returns T if there
   was such an entry, and NIL if not."
  (declare (type hash-table hash-table)
	   (values (member t nil)))
  (without-gcing
   #-gengc
   (when (= (get-header-data (hash-table-table hash-table))
	    vm:vector-must-rehash-subtype)
     (rehash hash-table nil))
   #+gengc
   (when (hash-table-needing-rehash hash-table)
     (flush-needing-rehash hash-table))
   (multiple-value-bind
       (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (let* ((vector (hash-table-table hash-table))
	    (length (length vector))
	    (index (rem hashing length)))
       (declare (type index hashing index))
       (if eq-based
	   (if (hash-table-weak-p hash-table)
	       (do ((prev nil bucket)
		    (bucket (svref vector index)
			    (hash-table-bucket-next bucket)))
		   ((null bucket) nil)
		 (when #+gengc (hash-table-eq-bucket-p bucket)
		       #-gengc (null (hash-table-bucket-hash bucket))
		   (multiple-value-bind
		       (bucket-key valid)
		       (weak-pointer-value (hash-table-bucket-key bucket))
		     (assert valid)
		     (when (eq key bucket-key)
		       #+gengc
		       (setf (hash-table-eq-bucket-linked bucket) nil)
		       (if prev
			   (setf (hash-table-bucket-next prev)
				 (hash-table-bucket-next bucket))
			   (setf (svref vector index)
				 (hash-table-bucket-next bucket)))
		       (decf (hash-table-number-entries hash-table))
		       (return t)))))
	       (do ((prev nil bucket)
		    (bucket (svref vector index)
			    (hash-table-bucket-next bucket)))
		   ((null bucket) nil)
		 (when (eq key (hash-table-bucket-key bucket))
		   #+gengc
		   (setf (hash-table-eq-bucket-linked bucket) nil)
		   (if prev
		       (setf (hash-table-bucket-next prev)
			     (hash-table-bucket-next bucket))
		       (setf (svref vector index)
			     (hash-table-bucket-next bucket)))
		   (decf (hash-table-number-entries hash-table))
		   (return t))))
	   (do ((test-fun (hash-table-test-fun hash-table))
		(prev nil bucket)
		(bucket (svref vector index)
			(hash-table-bucket-next bucket)))
	       ((null bucket) nil)
	     (let ((bucket-hashing (hash-table-bucket-hash bucket)))
	       (when (and #-gengc bucket-hashing
			  (= bucket-hashing hashing)
			  #+gengc (not (hash-table-eq-bucket-p bucket))
			  (funcall test-fun key
				   (hash-table-bucket-key bucket)))
		 (if prev
		     (setf (hash-table-bucket-next prev)
			   (hash-table-bucket-next bucket))
		     (setf (svref vector index)
			   (hash-table-bucket-next bucket)))
		 (decf (hash-table-number-entries hash-table))
		 (return t)))))))))


;;; CLRHASH -- public.
;;; 
(defun clrhash (hash-table)
  "This removes all the entries from HASH-TABLE and returns the hash table
   itself."
  (let ((vector (hash-table-table hash-table)))
    (dotimes (i (length vector))
      #+gengc
      (do ((bucket (aref vector i) (hash-table-bucket-next bucket)))
	  ((null bucket))
	(when (hash-table-eq-bucket-p bucket)
	  (setf (hash-table-eq-bucket-linked bucket) nil)))
      (setf (aref vector i) nil))
    (setf (hash-table-number-entries hash-table) 0)
    #-gengc
    (set-header-data vector vm:vector-normal-subtype))
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
	(vector (hash-table-table hash-table)))
    (declare (type function fun))
    (if (hash-table-weak-p hash-table)
	(dotimes (i (length vector))
	  (declare (type index i))
	  (do ((bucket (svref vector i) (hash-table-bucket-next bucket)))
	      ((null bucket))
	    (if #-gengc (null (hash-table-bucket-hash bucket))
		#+gengc (hash-table-eq-bucket-p bucket)
		(let ((weak-pointer (hash-table-bucket-key bucket)))
		  (multiple-value-bind
		      (key valid)
		      (weak-pointer-value weak-pointer)
		    (when valid
		      (funcall fun key (hash-table-bucket-value bucket)))))
		(funcall fun
			 (hash-table-bucket-key bucket)
			 (hash-table-bucket-value bucket)))))
	(dotimes (i (length vector))
	  (declare (type index i))
	  (do ((bucket (svref vector i) (hash-table-bucket-next bucket)))
	      ((null bucket))
	    (funcall fun
		     (hash-table-bucket-key bucket)
		     (hash-table-bucket-value bucket)))))))


(defmacro with-hash-table-iterator ((function hash-table) &body body)
  "WITH-HASH-TABLE-ITERATOR ((function hash-table) &body body)
   provides a method of manually looping over the elements of a hash-table.
   function is bound to a generator-macro that, withing the scope of the
   invocation, returns three values.  First, whether there are any more objects
   in the hash-table, second, the key, and third, the value."
  (let ((n-function (gensym "WITH-HASH-TABLE-ITERRATOR-")))
    `(let ((,n-function
	    (let* ((table ,hash-table)
		   (weak-p (hash-table-weak-p ,hash-table))
		   (vector (hash-table-table table))
		   (length (length vector))
		   (index 0)
		   (bucket (svref vector 0)))
	      (labels
		  ((,function ()
		     (cond
		      (bucket
		       (let ((orig bucket))
			 (setf bucket (hash-table-bucket-next orig))
			 (if (and weak-p
				  #-gengc (null (hash-table-bucket-hash orig))
				  #+gengc (hash-table-eq-bucket-p orig))
			     (multiple-value-bind
				 (key valid)
				 (weak-pointer-value
				  (hash-table-bucket-key orig))
			       (if valid
				   (values t
					   key
					   (hash-table-bucket-value orig))
				   (,function)))
			     (values t
				     (hash-table-bucket-key orig)
				     (hash-table-bucket-value orig)))))
		      ((= (incf index) length)
		       (values nil))
		      (t
		       (setf bucket (svref vector index))
		       (,function)))))
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
	(end (gensym)))
    `(with-array-data ((,data (the (values string &rest t) ,sequence))
		       (,start)
		       (,end))
       (if (zerop ,start)
	   (%sxhash-simple-substring ,data ,end)
	   (sxhash-simple-string (coerce (the (values string &rest t)
					      ,sequence)
					 'simple-string))))))

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
    (list (sxhash-list s-expr depth))
    (fixnum (ldb sxhash-bits-byte s-expr))
    (character (char-code (char-upcase s-expr)))
    (instance
     (if (typep s-expr 'structure-object)
	 (internal-sxhash (%class-name (layout-class (%instance-layout s-expr)))
			  depth)
	 (sxhash-instance s-expr)))
    (funcallable-instance (sxhash-instance s-expr))
    ;; Other-pointer types.
    (simple-string (sxhash-simple-string s-expr))
    (symbol (sxhash-simple-string (symbol-name s-expr)))
    (number
     (etypecase s-expr
       (integer (ldb sxhash-bits-byte s-expr))
       (single-float
	(let ((bits (single-float-bits s-expr)))
	  (ldb sxhash-bits-byte
	       (logxor (ash bits (- sxmash-rotate-bits)) bits))))
       (double-float
	(let ((lo (double-float-low-bits s-expr))
	      (hi (double-float-high-bits s-expr)))
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
    (list (sxhash-list s-expr depth :equalp t))
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
     :rehash-threshold ',(hash-table-rehash-threshold table))
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
