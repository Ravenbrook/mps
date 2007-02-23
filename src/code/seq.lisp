;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/seq.lisp,v 1.53 2006/06/30 18:41:22 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions to implement generic sequences for Spice Lisp.
;;; Written by Skef Wholey.
;;; Fixed up by Jim Muller on Friday the 13th, January, 1984.
;;; Gone over again by Bill Chiles.  Next?
;;;
;;; Be careful when modifying code.  A lot of the structure of the code is
;;; affected by the fact that compiler transforms use the lower level support
;;; functions.  If transforms are written for some sequence operation, note
;;; how the end argument is handled in other operations with transforms.

(in-package "LISP")
(export '(elt subseq copy-seq coerce
	  length reverse nreverse make-sequence concatenate map some every
	  notany notevery reduce fill replace remove remove-if remove-if-not
	  delete delete-if delete-if-not remove-duplicates delete-duplicates
	  substitute substitute-if substitute-if-not nsubstitute nsubstitute-if
	  nsubstitute-if-not find find-if find-if-not position position-if
	  position-if-not count count-if count-if-not mismatch search
	  map-into
          identity)) ; Yep, thet's whar it is.

	  
;;; Spice-Lisp specific stuff and utilities:
	  
(eval-when (:compile-toplevel)

;;; Seq-Dispatch does an efficient type-dispatch on the given Sequence.

(defmacro seq-dispatch (sequence list-form array-form)
  `(if (listp ,sequence)
       ,list-form
       ,array-form))

(defmacro elt-slice (sequences n)
  "Returns a list of the Nth element of each of the sequences.  Used by MAP
   and friends."
  `(mapcar #'(lambda (seq) (elt seq ,n)) ,sequences))

(defmacro make-sequence-like (sequence length)
  "Returns a sequence of the same type as SEQUENCE and the given LENGTH."
  `(make-sequence-of-type (type-of ,sequence) ,length))

(defmacro type-specifier-atom (type)
  "Returns the broad class of which TYPE is a specific subclass."
  `(if (atom ,type) ,type (car ,type)))

) ; eval-when




;;; RESULT-TYPE-OR-LOSE  --  Internal
;;;
;;;    Given an arbitrary type specifier, return a sane sequence type specifier
;;; that we can directly match.
;;;
(defun result-type-or-lose (seq-type &optional nil-ok)
  (let ((type (specifier-type seq-type)))
    (cond
      ((eq type *empty-type*)
       (if nil-ok
	   nil
	   (error 'simple-type-error
		  :datum type
		  :expected-type '(or vector cons)
		  :format-control
		  "NIL output type invalid for this sequence function."
		  :format-arguments ())))
      ((or (union-type-p type)
	   (and (member-type-p type)
		(not (eq type kernel:*null-type*))))
       (error 'simple-type-error
	      :datum type
	      :exptected-type 'sequence
	      :format-control "~S is too hairy for sequence functions."
	      :format-arguments (list seq-type)))
      ((dolist (seq-type '(list string simple-vector bit-vector))
	 (when (csubtypep type (specifier-type seq-type))
	   (return seq-type))))
      ((csubtypep type (specifier-type 'vector))
       (type-specifier type))
      (t
       (error 'simple-type-error
	      :datum type
	      :expected-type 'sequence
	      :format-control
	      "~S is a bad type specifier for sequence functions."
	      :format-arguments (list seq-type))))))

(define-condition index-too-large-error (type-error)
  ()
  (:report
   (lambda(condition stream)
     (format stream "Error in ~S: ~S: Index too large."
	     (condition-function-name condition)
	     (type-error-datum condition)))))

(defun signal-index-too-large-error (sequence index)
  (let* ((length (length sequence))
	 (max-index (and (plusp length)(1- length))))
    (error 'index-too-large-error
	   :datum index
	   :expected-type (if max-index
			      `(integer 0 ,max-index)
			      ;; This seems silly, is there something better?
			      '(integer (0) (0))))))

(defun make-sequence-of-type (type length)
  "Returns a sequence of the given TYPE and LENGTH."
  (declare (fixnum length))
  (case (type-specifier-atom type)
    (list (make-list length))
    ((bit-vector simple-bit-vector) (make-array length :element-type '(mod 2)))
    ((string simple-string base-string simple-base-string)
     (make-string length))
    (simple-vector (make-array length))
    ((array simple-array vector)
     (if (listp type)
	 (make-array length :element-type (cadr type))
	 (make-array length)))
    (t
     (make-sequence-of-type (result-type-or-lose type) length))))
  
(defun elt (sequence index)
  "Returns the element of SEQUENCE specified by INDEX."
  (etypecase sequence
    (list
     (do ((count index (1- count))
	  (list sequence (cdr list)))
	 ((= count 0)
	  (if (endp list)
	      (signal-index-too-large-error sequence index)
	      (car list)))
       (declare (type (integer 0) count))))
    (vector
     (when (>= index (length sequence))
       (signal-index-too-large-error sequence index))
     (aref sequence index))))

(defun %setelt (sequence index newval)
  "Store NEWVAL as the component of SEQUENCE specified by INDEX."
  (etypecase sequence
    (list
     (do ((count index (1- count))
	  (seq sequence))
 	 ((= count 0) (rplaca seq newval) newval)
       (declare (fixnum count))
       (if (atom (cdr seq))
	   (signal-index-too-large-error sequence index)
	   (setq seq (cdr seq)))))
    (vector
     (when (>= index (length sequence))
       (signal-index-too-large-error sequence index))
     (setf (aref sequence index) newval))))


(defun length (sequence)
  "Returns an integer that is the length of SEQUENCE."
  (etypecase sequence
    (vector (length (truly-the vector sequence)))
    (list (length (truly-the list sequence)))))

;; Check that the specifier-type SPEC-TYPE has a length compatible
;; with the given length.  Return T if so.  SPEC-TYPE must be some
;; kind of vector.  If the type system isn't initialized, we return T.
(defun valid-sequence-and-length-p (spec-type length)
  (if *type-system-initialized*
      (let ((type-len (first (array-type-dimensions spec-type))))
	(or (eq type-len '*)
	    (= type-len length)))
      ;; Type system not ready, so assume it's ok.
      t))

(defun bad-sequence-type-error (type-spec)
  (error 'simple-type-error
	 :datum type-spec
	 ;; This expected-type is wrong; it should be something like
	 ;; (SATISFIES IS-A-VALID-SEQUENCE-TYPE-SPECIFIER-P), but we
	 ;; aren't really using it.
	 :expected-type 'sequence
	 :format-control "~S is a bad type specifier for sequences"
	 :format-arguments (list type-spec)))

(defun sequence-length-error (type-spec length)
  (error 'simple-type-error
	 :datum length
	 :expected-type (cond ((array-type-p type-spec)
			       `(eql ,(car (array-type-dimensions type-spec))))
			      ((type= type-spec (specifier-type 'null))
			       '(eql 0))
			      ((cons-type-p type-spec)
			       `(integer 1))
			      (t
			       (error "Shouldn't happen!  Weird type")))
	 :format-control "The length of ~S does not match the specified ~
                          length of ~S."
	 :format-arguments (list (type-specifier type-spec) length)))
	 
(defun make-sequence (type length &key (initial-element NIL iep))
  "Returns a sequence of the given Type and Length, with elements initialized
  to :Initial-Element."
  (declare (fixnum length))
  (flet ((check-seq-len (spec-type length)
	     (unless (valid-sequence-and-length-p spec-type length)
	       (sequence-length-error spec-type length))))

    (let ((type (specifier-type type)))
      (cond
	((csubtypep type (specifier-type 'null))
	 (unless (zerop length)
	   (sequence-length-error type length))
	 nil)
	((type= type (specifier-type 'cons))
	 (unless (plusp length)
	   (sequence-length-error type length))
	 (make-list length :initial-element initial-element))
	;; Should we also check for types like '(cons (cons ...))?
	((csubtypep type (specifier-type 'list))
	 (make-list length :initial-element initial-element))
	((csubtypep type (specifier-type 'string))
	 (check-seq-len type length)
	 (if iep
	     (make-string length :initial-element initial-element)
	     (make-string length)))
	((csubtypep type (specifier-type 'simple-vector))
	 (check-seq-len type length)
	 (make-array length :initial-element initial-element))
	((csubtypep type (specifier-type 'bit-vector))
	 (check-seq-len type length)
	 (if iep
	     (make-array length :element-type '(mod 2)
			 :initial-element initial-element)
	     (make-array length :element-type '(mod 2))))
	((csubtypep type (specifier-type 'vector))
	 (if (typep type 'array-type)
	     (let ((etype (type-specifier
			   (array-type-specialized-element-type type)))
		   (vlen (car (array-type-dimensions type))))
	       (if (and (numberp vlen) (/= vlen length))
		   (error 'simple-type-error
			  ;; these two are under-specified by ANSI
			  :datum (type-specifier type)
			  :expected-type (type-specifier type)
			  :format-control
			  "The length of ~S does not match the specified ~
                           length  of ~S."
			  :format-arguments
			  (list (type-specifier type) length)))
	       (if iep
		   (make-array length :element-type etype
			       :initial-element initial-element)
		   (make-array length :element-type etype)))
	     (make-array length :initial-element initial-element)))
	(t (error 'simple-type-error
		  :datum type
		  :expected-type 'sequence
		  :format-control "~S is a bad type specifier for sequences."
		  :format-arguments (list (type-specifier type))))))))



;;; Subseq:
;;;
;;; The support routines for SUBSEQ are used by compiler transforms, so we
;;; worry about dealing with end being supplied as or defaulting to nil
;;; at this level.

(defun vector-subseq* (sequence start &optional end)
  (declare (vector sequence) (fixnum start))
  (when (null end) (setf end (length sequence)))
  (do ((old-index start (1+ old-index))
       (new-index 0 (1+ new-index))
       (copy (make-sequence-like sequence (- end start))))
      ((= old-index end) copy)
    (declare (fixnum old-index new-index))
    (setf (aref copy new-index) (aref sequence old-index))))

(defun list-subseq* (sequence start &optional end)
  (declare (list sequence) (fixnum start))
  (if (and end (>= start (the fixnum end)))
      ()
      (let* ((groveled (nthcdr start sequence))
	     (result (list (car groveled))))
	(if groveled
	    (do ((list (cdr groveled) (cdr list))
		 (splice result (cdr (rplacd splice (list (car list)))))
		 (index (1+ start) (1+ index)))
		((or (atom list) (and end (= index (the fixnum end))))
		 result)
	      (declare (fixnum index)))
	    ()))))

;;; SUBSEQ cannot default end to the length of sequence since it is not
;;; an error to supply nil for its value.  We must test for end being nil
;;; in the body of the function, and this is actually done in the support
;;; routines for other reasons (see above).
(defun subseq (sequence start &optional end)
  "Returns a copy of a subsequence of SEQUENCE starting with element number 
   START and continuing to the end of SEQUENCE or the optional END."
  (seq-dispatch sequence
		(list-subseq* sequence start end)
		(vector-subseq* sequence start end)))


;;; Copy-seq:

(eval-when (compile eval)

(defmacro vector-copy-seq (sequence type)
  `(let ((length (length (the vector ,sequence))))
     (declare (fixnum length))
     (do ((index 0 (1+ index))
	  (copy (make-sequence-of-type ,type length)))
	 ((= index length) copy)
       (declare (fixnum index))
       (setf (aref copy index) (aref ,sequence index)))))

(defmacro list-copy-seq (list)
  `(if (atom ,list) '()
       (let ((result (cons (car ,list) '()) ))
	 (do ((x (cdr ,list) (cdr x))
	      (splice result
		      (cdr (rplacd splice (cons (car x) '() ))) ))
	     ((atom x) (unless (null x)
			       (rplacd splice x))
		       result)))))

)

(defun copy-seq (sequence)
  "Returns a copy of SEQUENCE which is EQUAL to SEQUENCE but not EQ."
  (seq-dispatch sequence
		(list-copy-seq* sequence)
		(vector-copy-seq* sequence)))

;;; Internal Frobs:

(defun list-copy-seq* (sequence)
  (list-copy-seq sequence))

(defun vector-copy-seq* (sequence)
  (vector-copy-seq sequence (type-of sequence)))


;;; Fill:

(eval-when (compile eval)

(defmacro vector-fill (sequence item start end)
  `(do ((index ,start (1+ index)))
       ((= index (the fixnum ,end)) ,sequence)
     (declare (fixnum index))
     (setf (aref ,sequence index) ,item)))

(defmacro list-fill (sequence item start end)
  `(do ((current (nthcdr ,start ,sequence) (cdr current))
	(index ,start (1+ index)))
       ((or (atom current) (and end (= index (the fixnum ,end))))
	sequence)
     (declare (fixnum index))
     (rplaca current ,item)))

)

;;; The support routines for FILL are used by compiler transforms, so we
;;; worry about dealing with end being supplied as or defaulting to nil
;;; at this level.

(defun list-fill* (sequence item start end)
  (declare (list sequence))
  (list-fill sequence item start end))

(defun vector-fill* (sequence item start end)
  (declare (vector sequence))
  (when (null end) (setq end (length sequence)))
  (vector-fill sequence item start end))

;;; FILL cannot default end to the length of sequence since it is not
;;; an error to supply nil for its value.  We must test for end being nil
;;; in the body of the function, and this is actually done in the support
;;; routines for other reasons (see above).
(defun fill (sequence item &key (start 0) end)
  "Replace the specified elements of SEQUENCE with ITEM."
  (seq-dispatch sequence
		(list-fill* sequence item start end)
		(vector-fill* sequence item start end)))



;;; Replace:

(eval-when (:compile-toplevel :execute)

;;; If we are copying around in the same vector, be careful not to copy the
;;; same elements over repeatedly.  We do this by copying backwards.
(defmacro mumble-replace-from-mumble ()
  `(if (and (eq target-sequence source-sequence) (> target-start source-start))
       (let ((nelts (min (- target-end target-start) (- source-end source-start))))
	 (do ((target-index (+ (the fixnum target-start) (the fixnum nelts) -1)
			    (1- target-index))
	      (source-index (+ (the fixnum source-start) (the fixnum nelts) -1)
			    (1- source-index)))
	     ((= target-index (the fixnum (1- target-start))) target-sequence)
	   (declare (fixnum target-index source-index))
	   (setf (aref target-sequence target-index)
		 (aref source-sequence source-index))))
       (do ((target-index target-start (1+ target-index))
	    (source-index source-start (1+ source-index)))
	   ((or (= target-index (the fixnum target-end))
		(= source-index (the fixnum source-end)))
	    target-sequence)
	 (declare (fixnum target-index source-index))
	 (setf (aref target-sequence target-index)
	       (aref source-sequence source-index)))))

(defmacro list-replace-from-list ()
  `(if (and (eq target-sequence source-sequence) (> target-start source-start))
       (let ((new-elts (subseq source-sequence source-start
			       (+ (the fixnum source-start)
				  (the fixnum
				       (min (- (the fixnum target-end)
					       (the fixnum target-start))
					    (- (the fixnum source-end)
					       (the fixnum source-start))))))))
	 (do ((n new-elts (cdr n))
	      (o (nthcdr target-start target-sequence) (cdr o)))
	     ((null n) target-sequence)
	   (rplaca o (car n))))
       (do ((target-index target-start (1+ target-index))
	    (source-index source-start (1+ source-index))
	    (target-sequence-ref (nthcdr target-start target-sequence)
				 (cdr target-sequence-ref))
	    (source-sequence-ref (nthcdr source-start source-sequence)
				 (cdr source-sequence-ref)))
	   ((or (= target-index (the fixnum target-end))
		(= source-index (the fixnum source-end))
		(null target-sequence-ref) (null source-sequence-ref))
	    target-sequence)
	 (declare (fixnum target-index source-index))
	 (rplaca target-sequence-ref (car source-sequence-ref)))))

(defmacro list-replace-from-mumble ()
  `(do ((target-index target-start (1+ target-index))
	(source-index source-start (1+ source-index))
	(target-sequence-ref (nthcdr target-start target-sequence)
			     (cdr target-sequence-ref)))
       ((or (= target-index (the fixnum target-end))
	    (= source-index (the fixnum source-end))
	    (null target-sequence-ref))
	target-sequence)
     (declare (fixnum source-index target-index))
     (rplaca target-sequence-ref (aref source-sequence source-index))))

(defmacro mumble-replace-from-list ()
  `(do ((target-index target-start (1+ target-index))
	(source-index source-start (1+ source-index))
	(source-sequence (nthcdr source-start source-sequence)
			 (cdr source-sequence)))
       ((or (= target-index (the fixnum target-end))
	    (= source-index (the fixnum source-end))
	    (null source-sequence))
	target-sequence)
     (declare (fixnum target-index source-index))
     (setf (aref target-sequence target-index) (car source-sequence))))

) ; eval-when

;;; The support routines for REPLACE are used by compiler transforms, so we
;;; worry about dealing with end being supplied as or defaulting to nil
;;; at this level.

(defun list-replace-from-list* (target-sequence source-sequence target-start
				target-end source-start source-end)
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (list-replace-from-list))

(defun list-replace-from-vector* (target-sequence source-sequence target-start
				  target-end source-start source-end)
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (list-replace-from-mumble))

(defun vector-replace-from-list* (target-sequence source-sequence target-start
				  target-end source-start source-end)
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (mumble-replace-from-list))

(defun vector-replace-from-vector* (target-sequence source-sequence
				    target-start target-end source-start
				    source-end)
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (mumble-replace-from-mumble))

;;; REPLACE cannot default end arguments to the length of sequence since it
;;; is not an error to supply nil for their values.  We must test for ends
;;; being nil in the body of the function.
(defun replace (target-sequence source-sequence &key
	        ((:start1 target-start) 0)
		((:end1 target-end))
		((:start2 source-start) 0)
		((:end2 source-end)))
  "The target sequence is destructively modified by copying successive
   elements into it from the source sequence."
  (let ((target-end (or target-end (length target-sequence)))
	(source-end (or source-end (length source-sequence))))
    (seq-dispatch target-sequence
		  (seq-dispatch source-sequence
				(list-replace-from-list)
				(list-replace-from-mumble))
		  (seq-dispatch source-sequence
				(mumble-replace-from-list)
				(mumble-replace-from-mumble)))))


;;; Reverse:

(eval-when (compile eval)

(defmacro vector-reverse (sequence type)
  `(let ((length (length ,sequence)))
     (declare (fixnum length))
     (do ((forward-index 0 (1+ forward-index))
	  (backward-index (1- length) (1- backward-index))
	  (new-sequence (make-sequence-of-type ,type length)))
	 ((= forward-index length) new-sequence)
       (declare (fixnum forward-index backward-index))
       (setf (aref new-sequence forward-index)
	     (aref ,sequence backward-index)))))

(defmacro list-reverse-macro (sequence)
  `(do ((new-list ()))
       ((atom ,sequence) new-list)
     (push (pop ,sequence) new-list)))

)

(defun reverse (sequence)
  "Returns a new sequence containing the same elements but in reverse order."
  (seq-dispatch sequence
		(list-reverse* sequence)
		(vector-reverse* sequence)))

;;; Internal Frobs:

(defun list-reverse* (sequence)
  (list-reverse-macro sequence))

(defun vector-reverse* (sequence)
  (vector-reverse sequence (type-of sequence)))


;;; Nreverse:

(eval-when (compile eval)

(defmacro vector-nreverse (sequence)
  `(let ((length (length (the vector ,sequence))))
     (declare (fixnum length))
     (do ((left-index 0 (1+ left-index))
	  (right-index (1- length) (1- right-index))
	  (half-length (truncate length 2)))
	 ((= left-index half-length) ,sequence)
       (declare (fixnum left-index right-index half-length))
       (rotatef (aref ,sequence left-index)
		(aref ,sequence right-index)))))

(defmacro list-nreverse-macro (list)
  `(do ((1st (cdr ,list) (if (atom 1st) 1st (cdr 1st)))
	(2nd ,list 1st)
	(3rd '() 2nd))
       ((atom 2nd) 3rd)
     (rplacd 2nd 3rd)))

)


(defun list-nreverse* (sequence)
  (list-nreverse-macro sequence))

(defun vector-nreverse* (sequence)
  (vector-nreverse sequence))

(defun nreverse (sequence)
  "Returns a sequence of the same elements in reverse order; the argument
   is destroyed."
  (seq-dispatch sequence
		(list-nreverse* sequence)
		(vector-nreverse* sequence)))


;;; Concatenate:

(eval-when (compile eval)

(defmacro concatenate-to-list (sequences)
  `(let ((result (list nil)))
     (do ((sequences ,sequences (cdr sequences))
	  (splice result))
	 ((null sequences) (cdr result))
       (let ((sequence (car sequences)))
	 (seq-dispatch sequence
		       (do ((sequence sequence (cdr sequence)))
			   ((atom sequence))
			 (setq splice
			       (cdr (rplacd splice (list (car sequence))))))
		       (do ((index 0 (1+ index))
			    (length (length sequence)))
			   ((= index length))
			 (declare (fixnum index length))
			 (setq splice
			       (cdr (rplacd splice
					    (list (aref sequence index)))))))))))

(defmacro concatenate-to-mumble (output-type-spec sequences)
  `(do ((seqs ,sequences (cdr seqs))
	(total-length 0)
	(lengths ()))
       ((null seqs)
	(do ((sequences ,sequences (cdr sequences))
	     (lengths lengths (cdr lengths))
	     (index 0)
	     (result (make-sequence ,output-type-spec total-length)))
	    ((= index total-length) result)
	  (declare (fixnum index))
	  (let ((sequence (car sequences)))
	    (seq-dispatch sequence
			  (do ((sequence sequence (cdr sequence)))
			      ((atom sequence))
			    (setf (aref result index) (car sequence))
			    (setq index (1+ index)))
			  (do ((jndex 0 (1+ jndex))
			       (this-length (car lengths)))
			      ((= jndex this-length))
			    (declare (fixnum jndex this-length))
			    (setf (aref result index)
				  (aref sequence jndex))
			    (setq index (1+ index)))))))
     (let ((length (length (car seqs))))
       (declare (fixnum length))
       (setq lengths (nconc lengths (list length)))
       (setq total-length (+ total-length length)))))

)

(defun concatenate (output-type-spec &rest sequences)
  "Returns a new sequence of all the argument sequences concatenated together
  which shares no structure with the original argument sequences of the
  specified OUTPUT-TYPE-SPEC."
  (case (type-specifier-atom output-type-spec)
    ((simple-vector simple-string vector string
		    bit-vector simple-bit-vector base-string
		    simple-base-string)
     (apply #'concat-to-simple* output-type-spec sequences))
    (list (apply #'concat-to-list* sequences))
    ((array simple-array)
     ;; Make sure these are actually vectors!
     (apply #'concat-to-simple* (result-type-or-lose output-type-spec) sequences))
    (t
     (apply #'concatenate (result-type-or-lose output-type-spec) sequences))))

;;; Internal Frobs:

(defun concat-to-list* (&rest sequences)
  (concatenate-to-list sequences))

(defun concat-to-simple* (type &rest sequences)
  (concatenate-to-mumble type sequences))


;;; Map:

(declaim (start-block map))

(defun map-for-effect (function sequences)
  (declare (list sequences))
  (let ((min-length (1- most-positive-fixnum))
	(args (make-list (length sequences))))
    (declare (type index min-length))
    (dolist (seq sequences)
      (when (vectorp seq)
	(let ((length (length seq)))
	  (when (< length min-length)
	    (setf min-length length)))))
    (do ((index 0 (1+ index)))
	((>= index min-length))
      (declare (type index index))
      (do-anonymous ((rem-slice sequences (rest rem-slice))
		     (rem-args args (rest rem-args)))
		    ((endp rem-slice))
	(let ((seq (first rem-slice)))
	  (etypecase seq
	    (null (return))
	    (list
	     (setf (first rem-args) (first seq))
	     (setf (first rem-slice) (rest seq)))
	    (vector
	     (setf (first rem-args) (aref seq index))))))
      (apply function args))))

(defun map-to-list (function sequences)
  (declare (list sequences))
  (let ((min-length (1- most-positive-fixnum))
	(args (make-list (length sequences))))
    (declare (type index min-length))
    (dolist (seq sequences)
      (when (vectorp seq)
	(let ((length (length seq)))
	  (when (< length min-length)
	    (setf min-length length)))))
    (collect ((result))
      (do ((index 0 (1+ index)))
	  ((>= index min-length))
	(declare (type index index))
	(do-anonymous ((rem-slice sequences (rest rem-slice))
		       (rem-args args (rest rem-args)))
		      ((endp rem-slice))
	  (let ((seq (first rem-slice)))
	    (etypecase seq
	      (null (return))
	      (list
	       (setf (first rem-args) (first seq))
	       (setf (first rem-slice) (rest seq)))
	      (vector
	       (setf (first rem-args) (aref seq index))))))
	(result (apply function args)))
      (result))))

(defun map-to-simple (output-type-spec function sequences)
  (declare (list sequences))
  (flet ((seqlen (sequences)
	   (declare (list sequences))
	   (let ((min-length (1- most-positive-fixnum))
		 (lists nil))
	     (declare (type index min-length))
	     (dolist (seq sequences)
	       (etypecase seq
		 (list
		  (push seq lists))
		 (vector
		  (let ((length (length seq)))
		    (when (< length min-length)
		      (setf min-length length))))))
	     (cond (lists
		    (do ((index 0 (1+ index)))
			((>= index min-length) min-length)
		      (declare (type index index))
		      (do-anonymous ((rem lists (rest rem)))
				    ((endp rem))
			(let ((next (first rem)))
			  (when (endp next)
			    (return index))
			  (setf (first rem) (rest next))))))
		   (t
		    min-length)))))
    (let* ((args (make-list (length sequences)))
	   (min-length (seqlen sequences))
	   (result (if *type-system-initialized*
		       (make-sequence output-type-spec min-length)
		       (make-sequence-of-type output-type-spec min-length))))
      (declare (type index min-length))
      (do ((index 0 (1+ index)))
	  ((>= index min-length))
	(declare (type index index))
	(do-anonymous ((rem-slice sequences (rest rem-slice))
		       (rem-args args (rest rem-args)))
		      ((endp rem-slice))
	  (let ((seq (first rem-slice)))
	    (etypecase seq
	      (null (return))
	      (list
	       (setf (first rem-args) (first seq))
	       (setf (first rem-slice) (rest seq)))
	      (vector
	       (setf (first rem-args) (aref seq index))))))
	(setf (aref result index) (apply function args)))
      result)))

(defun map (output-type-spec function first-sequence &rest more-sequences)
  "FUNCTION must take as many arguments as there are sequences provided.  The 
   result is a sequence such that element i is the result of applying FUNCTION
   to element i of each of the argument sequences."
  (let ((sequences (cons first-sequence more-sequences)))
    (case (type-specifier-atom output-type-spec)
      ((nil) (map-for-effect function sequences))
      (list (map-to-list function sequences))
      ((simple-vector simple-string vector string
		   bit-vector simple-bit-vector base-string simple-base-string)
       (map-to-simple output-type-spec function sequences))
      ((simple-array array)
       ;; Make sure these are really sequences (vectors)
       (map-to-simple (result-type-or-lose output-type-spec) function sequences))
      (t
       (apply #'map (result-type-or-lose output-type-spec t)
	      function sequences)))))

(declaim (end-block))


(defun map-into (result-sequence function &rest sequences)
  (let* ((fp-result
	  (and (arrayp result-sequence)
	       (array-has-fill-pointer-p result-sequence)))
	 (len (apply #'min
		     (if fp-result
			 (array-dimension result-sequence 0)
			 (length result-sequence))
		     (mapcar #'length sequences))))

    (when fp-result
      (setf (fill-pointer result-sequence) len))

    (dotimes (index len)
      (setf (elt result-sequence index)
	    (apply function
		   (mapcar #'(lambda (seq) (elt seq index))
			   sequences)))))
  result-sequence)
  

;;; Quantifiers:

(eval-when (compile eval)
(defmacro defquantifier (name doc-string every-result abort-sense abort-value)
  `(defun ,name (predicate first-sequence &rest more-sequences)
     ,doc-string
     (let* ((sequences (cons first-sequence more-sequences))
	    (min-length (1- most-positive-fixnum))
	    (args (make-list (length sequences))))
       (declare (type index min-length))
       (dolist (seq sequences)
	 (check-type seq sequence)
	 (when (vectorp seq)
	   (let ((length (length seq)))
	     (when (< length min-length)
	       (setf min-length length)))))
       (do ((index 0 (1+ index)))
	   ((>= index min-length) ,every-result)
	 (declare (type index index))
	 (do-anonymous ((rem-slice sequences (rest rem-slice))
			(rem-args args (rest rem-args)))
		       ((endp rem-slice))
	   (let ((seq (first rem-slice)))
	     (etypecase seq
	       (null (return ,every-result))
	       (list
		(setf (first rem-args) (first seq))
		(setf (first rem-slice) (rest seq)))
	       (vector
		(setf (first rem-args) (aref seq index))))))
	 (let ((result (apply predicate args)))
	   (if ,(if abort-sense 'result '(not result))
	       (return ,abort-value)))))))
) ; eval-when

(defquantifier some
  "PREDICATE is applied to the elements with index 0 of the sequences, then 
   possibly to those with index 1, and so on.  SOME returns the first 
   non-() value encountered, or () if the end of a sequence is reached."
  nil t result)

(defquantifier every
  "PREDICATE is applied to the elements with index 0 of the sequences, then
   possibly to those with index 1, and so on.  EVERY returns () as soon
   as any invocation of PREDICATE returns (), or T if every invocation
   is non-()."
  t nil nil)

(defquantifier notany
  "PREDICATE is applied to the elements with index 0 of the sequences, then 
   possibly to those with index 1, and so on.  NOTANY returns () as soon
   as any invocation of PREDICATE returns a non-() value, or T if the end
   of a sequence is reached."
  t t nil)

(defquantifier notevery
  "PREDICATE is applied to the elements with index 0 of the sequences, then
   possibly to those with index 1, and so on.  NOTEVERY returns T as soon
   as any invocation of PREDICATE returns (), or () if every invocation
   is non-()."
  nil nil t)



;;; Reduce:

(eval-when (compile eval)

(defmacro mumble-reduce (function sequence key start end initial-value ref)
  `(do ((index ,start (1+ index))
	(value ,initial-value))
       ((= index (the fixnum ,end)) value)
     (declare (fixnum index))
     (setq value (funcall ,function value
			  (apply-key ,key (,ref ,sequence index))))))

(defmacro mumble-reduce-from-end (function sequence key start end initial-value ref)
  `(do ((index (1- ,end) (1- index))
	(value ,initial-value)
	(terminus (1- ,start)))
       ((= index terminus) value)
     (declare (fixnum index terminus))
     (setq value (funcall ,function
			  (apply-key ,key (,ref ,sequence index))
			  value))))

(defmacro list-reduce (function sequence key start end initial-value ivp)
  `(let ((sequence (nthcdr ,start ,sequence)))
     (do ((count (if ,ivp ,start (1+ (the fixnum ,start)))
		 (1+ count))
	  (sequence (if ,ivp sequence (cdr sequence))
		    (cdr sequence))
	  (value (if ,ivp ,initial-value (apply-key ,key (car sequence)))
		 (funcall ,function value (apply-key ,key (car sequence)))))
	 ((= count (the fixnum ,end)) value)
       (declare (fixnum count)))))

(defmacro list-reduce-from-end (function sequence key start end initial-value ivp)
  `(let ((sequence (nthcdr (- (the fixnum (length ,sequence)) (the fixnum ,end))
			   (reverse ,sequence))))
     (do ((count (if ,ivp ,start (1+ (the fixnum ,start)))
		 (1+ count))
	  (sequence (if ,ivp sequence (cdr sequence))
		    (cdr sequence))
	  (value (if ,ivp ,initial-value (apply-key ,key (car sequence)))
		 (funcall ,function (apply-key ,key (car sequence)) value)))
	 ((= count (the fixnum ,end)) value)
       (declare (fixnum count)))))

)

(defun reduce (function sequence &key key from-end (start 0)
			end (initial-value nil ivp))
  "The specified Sequence is ``reduced'' using the given Function.
  See manual for details."
  (declare (type index start))
  (let ((start start)
	(end (or end (length sequence))))
    (declare (type index start end))
    (cond ((= end start)
	   (if ivp initial-value (funcall function)))
	  ((listp sequence)
	   (if from-end
	       (list-reduce-from-end function sequence key start end
				     initial-value ivp)
	       (list-reduce function sequence key start end
			    initial-value ivp)))
	  (from-end
	   (when (not ivp)
	     (setq end (1- (the fixnum end)))
	     (setq initial-value (apply-key key (aref sequence end))))
	   (mumble-reduce-from-end function sequence key start end
				   initial-value aref))
	  (t
	   (when (not ivp)
	     (setq initial-value (apply-key key (aref sequence start)))
	     (setq start (1+ start)))
	   (mumble-reduce function sequence key start end
			  initial-value aref)))))


;;; Coerce:

(defun coerce (object output-type-spec)
  "Coerces the Object to an object of type Output-Type-Spec."
  (labels ((coerce-error ()
	     (error 'simple-type-error
		    :expected-type output-type-spec
		    :datum object
		    :format-control "~S can't be converted to type ~S."
		    :format-arguments (list object output-type-spec)))
	   (check-seq-len (type length)
	     (unless (valid-sequence-and-length-p type length)
	       (coerce-error)))
	   (output-type-or-lose (val)
	     ;; Verify that the value really is of type
	     ;; OUTPUT-TYPE-SPEC.  If so, return the value.  Intended
	     ;; to catch things like
	     ;;
	     ;; (coerce 2 '(single-float 5.0 6.0))
	     (unless (typep val output-type-spec)
	       (coerce-error))
	     val))
    (let ((type (specifier-type output-type-spec)))
      (cond
	((%typep object output-type-spec)
	 object)
	((eq type *empty-type*)
	 (coerce-error))
	((csubtypep type (specifier-type 'character))
	 (character object))
	((csubtypep type (specifier-type 'function))
	 (eval `#',object))
	((numberp object)
	 (let ((res
		(cond
		  ((csubtypep type (specifier-type 'single-float))
		   (output-type-or-lose (%single-float object)))
		  ((csubtypep type (specifier-type 'double-float))
		   (output-type-or-lose (%double-float object)))
		  #+long-float
		  ((csubtypep type (specifier-type 'long-float))
		   (output-type-or-lose (%long-float object)))
		  #+double-double
		  ((csubtypep type (specifier-type 'double-double-float))
		   (output-type-or-lose (%double-double-float object)))
		  ((csubtypep type (specifier-type 'float))
		   (output-type-or-lose (%single-float object)))
		  ((csubtypep type (specifier-type '(complex single-float)))
		   (complex (%single-float (realpart object))
			    (%single-float (imagpart object))))
		  ((csubtypep type (specifier-type '(complex double-float)))
		   (complex (%double-float (realpart object))
			    (%double-float (imagpart object))))
		  #+long-float
		  ((csubtypep type (specifier-type '(complex long-float)))
		   (complex (%long-float (realpart object))
			    (%long-float (imagpart object))))
		  #+double-double
		  ((csubtypep type (specifier-type '(complex double-double-float)))
		   (complex (%double-double-float (realpart object))
			    (%double-double-float (imagpart object))))
		  ((csubtypep type (specifier-type '(complex float)))
		   (complex (%single-float (realpart object))
			    (%single-float (imagpart object))))
		  ((and (typep object 'rational)
			(csubtypep type (specifier-type '(complex float))))
		   ;; The case where object is complex, is handled way
		   ;; above; where object is a float is handled below.
		   ;; We only need to check for rationals here.
		   (complex (%single-float object)))
		  ((csubtypep type (specifier-type 'complex))
		   (complex object))
		  (t
		   (coerce-error)))))
	   ;; If RES has the wrong type, that means that rule of canonical
	   ;; representation for complex rationals was invoked.  According to
	   ;; the Hyperspec, (coerce 7/2 'complex) returns 7/2.  Thus, if the
	   ;; object was a rational, there is no error here.
	   (unless (or (typep res output-type-spec) (rationalp object))
	     (coerce-error))
	   res))
	((csubtypep type (specifier-type 'list))
	 (if (vectorp object)
	     (vector-to-list* object)
	     (coerce-error)))
	((csubtypep type (specifier-type 'string))
	 (check-seq-len type (length object))
	 (typecase object
	   (list (list-to-string* object))
	   (string (string-to-simple-string* object))
	   (vector (vector-to-string* object))
	   (t
	    (coerce-error))))
	((csubtypep type (specifier-type 'bit-vector))
	 (check-seq-len type (length object))
	 (typecase object
	   (list (list-to-bit-vector* object))
	   (vector (vector-to-bit-vector* object))
	   (t
	    (coerce-error))))
	((csubtypep type (specifier-type 'vector))
	 (check-seq-len type (length object))
	 (typecase object
	   (list (list-to-vector* object output-type-spec))
	   (vector (vector-to-vector* object output-type-spec))
	   (t
	    (coerce-error))))
	(t
	 (coerce-error))))))


;;; Internal Frobs:

(macrolet ((frob (name result access src-type &optional typep)
		 `(defun ,name (object ,@(if typep '(type) ()))
		    (do* ((index 0 (1+ index))
			  (length (length (the ,(case src-type
						  (:list 'list)
						  (:vector 'vector))
					       object)))
			  (result ,result))
			 ((= index length) result)
		      (declare (fixnum length index))
		      (setf (,access result index)
			    ,(case src-type
			       (:list '(pop object))
			       (:vector '(aref object index))))))))

  (frob list-to-string* (make-string length) schar :list)

  (frob list-to-bit-vector* (make-array length :element-type '(mod 2))
	sbit :list)

  (frob list-to-vector* (make-sequence-of-type type length)
	aref :list t)

  (frob vector-to-vector* (make-sequence-of-type type length)
	aref :vector t)

  (frob vector-to-string* (make-string length) schar :vector)

  (frob vector-to-bit-vector* (make-array length :element-type '(mod 2))
	sbit :vector))

(defun vector-to-list* (object)
  (let ((result (list nil))
	(length (length object)))
    (declare (fixnum length))
    (do ((index 0 (1+ index))
	 (splice result (cdr splice)))
	((= index length) (cdr result))
      (declare (fixnum index))
      (rplacd splice (list (aref object index))))))

(defun string-to-simple-string* (object)
  (if (simple-string-p object)
      object
      (with-array-data ((data object)
			(start)
			(end (length object)))
	(declare (simple-string data))
	(subseq data start end))))

(defun bit-vector-to-simple-bit-vector* (object)
  (if (simple-bit-vector-p object)
      object
      (with-array-data ((data object)
			(start)
			(end (length object)))
	(declare (simple-bit-vector data))
	(subseq data start end))))


;;; Delete:

(eval-when (compile eval)

(defmacro mumble-delete (pred)
  `(do ((index start (1+ index))
	(jndex start)
	(number-zapped 0))
       ((or (= index (the fixnum end)) (= number-zapped (the fixnum count)))
	(do ((index index (1+ index))		; copy the rest of the vector
	     (jndex jndex (1+ jndex)))
	    ((= index (the fixnum length))
	     (shrink-vector sequence jndex))
	  (declare (fixnum index jndex))
	  (setf (aref sequence jndex) (aref sequence index))))
     (declare (fixnum index jndex number-zapped))
     (setf (aref sequence jndex) (aref sequence index))
     (if ,pred
	 (setq number-zapped (1+ number-zapped))
	 (setq jndex (1+ jndex)))))

(defmacro mumble-delete-from-end (pred)
  `(do ((index (1- (the fixnum end)) (1- index)) ; find the losers
	(number-zapped 0)
	(losers ())
        this-element
	(terminus (1- start)))
       ((or (= index terminus) (= number-zapped (the fixnum count)))
	(do ((losers losers)			 ; delete the losers
	     (index start (1+ index))
	     (jndex start))
	    ((or (null losers) (= index (the fixnum end)))
	     (do ((index index (1+ index))	 ; copy the rest of the vector
		  (jndex jndex (1+ jndex)))
		 ((= index (the fixnum length))
		  (shrink-vector sequence jndex))
	       (declare (fixnum index jndex))
	       (setf (aref sequence jndex) (aref sequence index))))
	  (declare (fixnum index jndex))
	  (setf (aref sequence jndex) (aref sequence index))
	  (if (= index (the fixnum (car losers)))
	      (pop losers)
	      (setq jndex (1+ jndex)))))
     (declare (fixnum index number-zapped terminus))
     (setq this-element (aref sequence index))
     (when ,pred
       (setq number-zapped (1+ number-zapped))
       (push index losers))))

(defmacro normal-mumble-delete ()
  `(mumble-delete
    (if test-not
	(not (funcall test-not item (apply-key key (aref sequence index))))
	(funcall test item (apply-key key (aref sequence index))))))

(defmacro normal-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (if test-not
	(not (funcall test-not item (apply-key key this-element)))
	(funcall test item (apply-key key this-element)))))

(defmacro list-delete (pred)
  `(let ((handle (cons nil sequence)))
     (do ((current (nthcdr start sequence) (cdr current))
	  (previous (nthcdr start handle))
	  (index start (1+ index))
	  (number-zapped 0))
	 ((or (= index (the fixnum end)) (= number-zapped (the fixnum count)))
	  (cdr handle))
       (declare (fixnum index number-zapped))
       (cond (,pred
	      (rplacd previous (cdr current))
	      (setq number-zapped (1+ number-zapped)))
	     (t
	      (setq previous (cdr previous)))))))

(defmacro list-delete-from-end (pred)
  `(let* ((reverse (nreverse (the list sequence)))
	  (handle (cons nil reverse)))
     (do ((current (nthcdr (- (the fixnum length) (the fixnum end)) reverse)
		   (cdr current))
	  (previous (nthcdr (- (the fixnum length) (the fixnum end)) handle))
	  (index start (1+ index))
	  (number-zapped 0))
	 ((or (= index (the fixnum end)) (= number-zapped (the fixnum count)))
	  (nreverse (cdr handle)))
       (declare (fixnum index number-zapped))
       (cond (,pred
	      (rplacd previous (cdr current))
	      (setq number-zapped (1+ number-zapped)))
	     (t
	      (setq previous (cdr previous)))))))

(defmacro normal-list-delete ()
  '(list-delete
    (if test-not
	(not (funcall test-not item (apply-key key (car current))))
	(funcall test item (apply-key key (car current))))))

(defmacro normal-list-delete-from-end ()
  '(list-delete-from-end
    (if test-not
	(not (funcall test-not item (apply-key key (car current))))
	(funcall test item (apply-key key (car current))))))

(defmacro real-count (count)
  `(cond ((null ,count) most-positive-fixnum)
	 ((fixnump ,count) (if (minusp ,count) 0 ,count))
	 ((integerp ,count) (if (minusp ,count) 0 most-positive-fixnum))
	 (t ,count)))

)




(defun delete (item sequence &key from-end (test #'eql) test-not (start 0)
		end count key)
  "Returns a sequence formed by destructively removing the specified Item from
  the given Sequence."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (declare (type index length end)
	     (fixnum count))
    (seq-dispatch sequence
		  (if from-end
		      (normal-list-delete-from-end)
		      (normal-list-delete))
		  (if from-end
		      (normal-mumble-delete-from-end)
		      (normal-mumble-delete)))))

(eval-when (compile eval)

(defmacro if-mumble-delete ()
  `(mumble-delete
    (funcall predicate (apply-key key (aref sequence index)))))

(defmacro if-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (funcall predicate (apply-key key this-element))))

(defmacro if-list-delete ()
  '(list-delete
    (funcall predicate (apply-key key (car current)))))

(defmacro if-list-delete-from-end ()
  '(list-delete-from-end
    (funcall predicate (apply-key key (car current)))))

)

(defun delete-if (predicate sequence &key from-end (start 0) key end count)
  "Returns a sequence formed by destructively removing the elements satisfying
  the specified Predicate from the given Sequence."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (declare (type index length end)
	     (fixnum count))
    (seq-dispatch sequence
		  (if from-end
		      (if-list-delete-from-end)
		      (if-list-delete))
		  (if from-end
		      (if-mumble-delete-from-end)
		      (if-mumble-delete)))))

(eval-when (compile eval)

(defmacro if-not-mumble-delete ()
  `(mumble-delete
    (not (funcall predicate (apply-key key (aref sequence index))))))

(defmacro if-not-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (not (funcall predicate (apply-key key this-element)))))

(defmacro if-not-list-delete ()
  '(list-delete
    (not (funcall predicate (apply-key key (car current))))))

(defmacro if-not-list-delete-from-end ()
  '(list-delete-from-end
    (not (funcall predicate (apply-key key (car current))))))

)

(defun delete-if-not (predicate sequence &key from-end (start 0) end key count)
  "Returns a sequence formed by destructively removing the elements not
  satisfying the specified Predicate from the given Sequence."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (declare (type index length end)
	     (fixnum count))
    (seq-dispatch sequence
		  (if from-end
		      (if-not-list-delete-from-end)
		      (if-not-list-delete))
		  (if from-end
		      (if-not-mumble-delete-from-end)
		      (if-not-mumble-delete)))))


;;; Remove:

(eval-when (compile eval)

;;; MUMBLE-REMOVE-MACRO does not include (removes) each element that
;;; satisfies the predicate.
(defmacro mumble-remove-macro (bump left begin finish right pred)
  `(do ((index ,begin (,bump index))
	(result
	 (do ((index ,left (,bump index))
	      (result (make-sequence-like sequence length)))
	     ((= index (the fixnum ,begin)) result)
	   (declare (fixnum index))
	   (setf (aref result index) (aref sequence index))))
	(new-index ,begin)
	(number-zapped 0)
	(this-element))
       ((or (= index (the fixnum ,finish)) (= number-zapped (the fixnum count)))
	(do ((index index (,bump index))
	     (new-index new-index (,bump new-index)))
	    ((= index (the fixnum ,right)) (shrink-vector result new-index))
	  (declare (fixnum index new-index))
	  (setf (aref result new-index) (aref sequence index))))
     (declare (fixnum index new-index number-zapped))
     (setq this-element (aref sequence index))
     (cond (,pred (setq number-zapped (1+ number-zapped)))
	   (t (setf (aref result new-index) this-element)
	      (setq new-index (,bump new-index))))))

(defmacro mumble-remove (pred)
  `(mumble-remove-macro 1+ 0 start end length ,pred))

(defmacro mumble-remove-from-end (pred)
  `(let ((sequence (copy-seq sequence)))
     (mumble-delete-from-end ,pred)))

(defmacro normal-mumble-remove ()
  `(mumble-remove 
    (if test-not
	(not (funcall test-not item (apply-key key this-element)))
	(funcall test item (apply-key key this-element)))))

(defmacro normal-mumble-remove-from-end ()
  `(mumble-remove-from-end 
    (if test-not
	(not (funcall test-not item (apply-key key this-element)))
	(funcall test item (apply-key key this-element)))))

(defmacro if-mumble-remove ()
  `(mumble-remove (funcall predicate (apply-key key this-element))))

(defmacro if-mumble-remove-from-end ()
  `(mumble-remove-from-end (funcall predicate (apply-key key this-element))))

(defmacro if-not-mumble-remove ()
  `(mumble-remove (not (funcall predicate (apply-key key this-element)))))

(defmacro if-not-mumble-remove-from-end ()
  `(mumble-remove-from-end
    (not (funcall predicate (apply-key key this-element)))))

;;; LIST-REMOVE-MACRO does not include (removes) each element that satisfies
;;; the predicate.
(defmacro list-remove-macro (pred reverse-p)
  `(let* ((sequence ,(if reverse-p
			 '(reverse (the list sequence))
			 'sequence))
	  (%start ,(if reverse-p '(- length end) 'start))
	  (%end ,(if reverse-p '(- length start) 'end))
	  (splice (list nil))
	  (results (do ((index 0 (1+ index))
			(before-start splice))
		       ((= index (the fixnum %start)) before-start)
		     (declare (fixnum index))
		     (setq splice
			   (cdr (rplacd splice (list (pop sequence))))))))
     (do ((index %start (1+ index))
	  (this-element)
	  (number-zapped 0))
	 ((or (= index (the fixnum %end)) (= number-zapped (the fixnum count)))
	  (do ((index index (1+ index)))
	      ((null sequence)
	       ,(if reverse-p
		    '(nreverse (the list (cdr results)))
		    '(cdr results)))
	    (declare (fixnum index))
	    (setq splice (cdr (rplacd splice (list (pop sequence)))))))
       (declare (fixnum index number-zapped))
       (setq this-element (pop sequence))
       (if ,pred
	   (setq number-zapped (1+ number-zapped))
	   (setq splice (cdr (rplacd splice (list this-element))))))))


(defmacro list-remove (pred)
  `(list-remove-macro ,pred nil))

(defmacro list-remove-from-end (pred)
  `(list-remove-macro ,pred t))

(defmacro normal-list-remove ()
  `(list-remove
    (if test-not
	(not (funcall test-not item (apply-key key this-element)))
	(funcall test item (apply-key key this-element)))))

(defmacro normal-list-remove-from-end ()
  `(list-remove-from-end
    (if test-not
	(not (funcall test-not item (apply-key key this-element)))
	(funcall test item (apply-key key this-element)))))

(defmacro if-list-remove ()
  `(list-remove
    (funcall predicate (apply-key key this-element))))

(defmacro if-list-remove-from-end ()
  `(list-remove-from-end
    (funcall predicate (apply-key key this-element))))

(defmacro if-not-list-remove ()
  `(list-remove
    (not (funcall predicate (apply-key key this-element)))))

(defmacro if-not-list-remove-from-end ()
  `(list-remove-from-end
    (not (funcall predicate (apply-key key this-element)))))

)

(defun remove (item sequence &key from-end (test #'eql) test-not (start 0)
		end count key)
  "Returns a copy of SEQUENCE with elements satisfying the test (default is
   EQL) with ITEM removed."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (declare (type index length end)
	     (fixnum count))
    (seq-dispatch sequence
		  (if from-end
		      (normal-list-remove-from-end)
		      (normal-list-remove))
		  (if from-end
		      (normal-mumble-remove-from-end)
		      (normal-mumble-remove)))))

(defun remove-if (predicate sequence &key from-end (start 0) end count key)
  "Returns a copy of sequence with elements such that predicate(element)
   is non-null are removed"
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (declare (type index length end)
	     (fixnum count))
    (seq-dispatch sequence
		  (if from-end
		      (if-list-remove-from-end)
		      (if-list-remove))
		  (if from-end
		      (if-mumble-remove-from-end)
		      (if-mumble-remove)))))

(defun remove-if-not (predicate sequence &key from-end (start 0) end count key)
  "Returns a copy of sequence with elements such that predicate(element)
   is null are removed"
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (declare (type index length end)
	     (fixnum count))
    (seq-dispatch sequence
		  (if from-end
		      (if-not-list-remove-from-end)
		      (if-not-list-remove))
		  (if from-end
		      (if-not-mumble-remove-from-end)
		      (if-not-mumble-remove)))))


;;; Remove-Duplicates:
     
;;; Remove duplicates from a list. If from-end, remove the later duplicates,
;;; not the earlier ones. Thus if we check from-end we don't copy an item
;;; if we look into the already copied structure (from after :start) and see
;;; the item. If we check from beginning we check into the rest of the 
;;; original list up to the :end marker (this we have to do by running a
;;; do loop down the list that far and using our test.
(defun list-remove-duplicates* (list test test-not start end key from-end)
  (declare (fixnum start))
  (let* ((result (list ())) ; Put a marker on the beginning to splice with.
	 (splice result)
	 (current list))
    (do ((index 0 (1+ index)))
	((= index start))
      (declare (fixnum index))
      (setq splice (cdr (rplacd splice (list (car current)))))
      (setq current (cdr current)))
    (do ((index start (1+ index)))
	((or (and end (= index (the fixnum end)))
	     (atom current)))
      (declare (fixnum index))
      (if (or (and from-end 
		   (not (member (apply-key key (car current))
				(nthcdr (1+ start) result)
				:test test
				:test-not test-not
				:key key)))
	      (and (not from-end)
		   (not (do ((it (apply-key key (car current)))
			     (l (cdr current) (cdr l))
			     (i (1+ index) (1+ i)))
			    ((or (atom l) (and end (= i (the fixnum end))))
			     ())
			  (declare (fixnum i))
			  (if (if test-not
				  (not (funcall test-not it (apply-key key (car l))))
				  (funcall test it (apply-key key (car l))))
			      (return t))))))
	  (setq splice (cdr (rplacd splice (list (car current))))))
      (setq current (cdr current)))
    (do ()
	((atom current))
      (setq splice (cdr (rplacd splice (list (car current)))))
      (setq current (cdr current)))
    (cdr result)))



(defun vector-remove-duplicates* (vector test test-not start end key from-end
					 &optional (length (length vector)))
  (declare (vector vector) (fixnum start length))
  (when (null end) (setf end (length vector)))
  (let ((result (make-sequence-like vector length))
	(index 0)
	(jndex start))
    (declare (fixnum index jndex))
    (do ()
	((= index start))
      (setf (aref result index) (aref vector index))
      (setq index (1+ index)))
    (do ((elt))
	((= index end))
      (setq elt (aref vector index))
      (unless (or (and from-end
		        (position (apply-key key elt) result :start start
			   :end jndex :test test :test-not test-not :key key))
		  (and (not from-end)
		        (position (apply-key key elt) vector :start (1+ index)
			   :end end :test test :test-not test-not :key key)))
	(setf (aref result jndex) elt)
	(setq jndex (1+ jndex)))
      (setq index (1+ index)))
    (do ()
	((= index length))
      (setf (aref result jndex) (aref vector index))
      (setq index (1+ index))
      (setq jndex (1+ jndex)))
    (shrink-vector result jndex)))


(defun remove-duplicates (sequence &key (test #'eql) test-not (start 0) from-end
				   end key)
  "The elements of Sequence are compared pairwise, and if any two match,
   the one occuring earlier is discarded, unless FROM-END is true, in
   which case the one later in the sequence is discarded.  The resulting
   sequence is returned.

   The :TEST-NOT argument is depreciated."
  (declare (fixnum start))
  (seq-dispatch sequence
		(if sequence
		    (list-remove-duplicates* sequence test test-not
					      start end key from-end))
		(vector-remove-duplicates* sequence test test-not
					    start end key from-end)))



;;; Delete-Duplicates:


(defun list-delete-duplicates* (list test test-not key from-end start end)
  (declare (fixnum start))
  (let ((handle (cons nil list)))
    (do ((current (nthcdr start list) (cdr current))
	 (previous (nthcdr start handle))
	 (index start (1+ index)))
	((or (and end (= index (the fixnum end))) (null current))
	 (cdr handle))
      (declare (fixnum index))
      (if (do ((x (if from-end 
		      (nthcdr (1+ start) handle)
		      (cdr current))
		  (cdr x))
	       (i (1+ index) (1+ i)))
	      ((or (null x)
		   (and (not from-end) end (= i (the fixnum end)))
		   (eq x current))
	       nil)
	    (declare (fixnum i))
	    (if (if test-not
		    (not (funcall test-not 
				  (apply-key key (car current))
				  (apply-key key (car x))))
		    (funcall test 
			     (apply-key key (car current)) 
			     (apply-key key (car x))))
		(return t)))
	  (rplacd previous (cdr current))
	  (setq previous (cdr previous))))))


(defun vector-delete-duplicates* (vector test test-not key from-end start end 
					 &optional (length (length vector)))
  (declare (vector vector) (fixnum start length))
  (when (null end) (setf end (length vector)))
  (do ((index start (1+ index))
       (jndex start))
      ((= index end)
       (do ((index index (1+ index))		; copy the rest of the vector
	    (jndex jndex (1+ jndex)))
	   ((= index length)
	    (shrink-vector vector jndex)
	    vector)
	 (setf (aref vector jndex) (aref vector index))))
    (declare (fixnum index jndex))
    (setf (aref vector jndex) (aref vector index))
    (unless (position (apply-key key (aref vector index)) vector :key key
		      :start (if from-end start (1+ index)) :test test
		      :end (if from-end jndex end) :test-not test-not)
      (setq jndex (1+ jndex)))))


(defun delete-duplicates (sequence &key (test #'eql) test-not (start 0) from-end
			    end key)
  "The elements of Sequence are examined, and if any two match, one is
   discarded.  The resulting sequence, which may be formed by destroying the
   given sequence, is returned.

   The :TEST-NOT argument is depreciated."
  (seq-dispatch sequence
    (if sequence
	(list-delete-duplicates* sequence test test-not key from-end start end))
  (vector-delete-duplicates* sequence test test-not key from-end start end)))

(defun list-substitute* (pred new list start end count key test test-not old)
  (declare (fixnum start end count))
  (let* ((result (list nil))
	 elt
	 (splice result)
	 (list list))           ; Get a local list for a stepper.
    (do ((index 0 (1+ index)))
	((= index start))
      (declare (fixnum index))
      (setq splice (cdr (rplacd splice (list (car list)))))
      (setq list (cdr list)))
    (do ((index start (1+ index)))
	((or (= index end) (null list) (= count 0)))
      (declare (fixnum index))
      (setq elt (car list))
      (setq splice
	    (cdr (rplacd splice
			 (list
			  (cond
			   ((case pred
				   (normal
				    (if test-not
					(not 
					 (funcall test-not old (apply-key key elt)))
					(funcall test old (apply-key key elt))))
				   (if (funcall test (apply-key key elt)))
				   (if-not (not (funcall test (apply-key key elt)))))
			    (setq count (1- count))
			    new)
				(t elt))))))
      (setq list (cdr list)))
    (do ()
	((null list))
      (setq splice (cdr (rplacd splice (list (car list)))))
      (setq list (cdr list)))
    (cdr result)))

;;; Replace old with new in sequence moving from left to right by incrementer
;;; on each pass through the loop. Called by all three substitute functions.
(defun vector-substitute* (pred new sequence incrementer left right length
			   start end count key test test-not old)
  (declare (fixnum start count end incrementer right))
  (let ((result (make-sequence-like sequence length))
	(index left))
    (declare (fixnum index))
    (do ()
	((= index start))
      (setf (aref result index) (aref sequence index))
      (setq index (+ index incrementer)))
    (do ((elt))
	((or (= index end) (= count 0)))
      (setq elt (aref sequence index))
      (setf (aref result index) 
	    (cond ((case pred
			  (normal
			    (if test-not
				(not (funcall test-not old (apply-key key elt)))
				(funcall test old (apply-key key elt))))
			  (if (funcall test (apply-key key elt)))
			  (if-not (not (funcall test (apply-key key elt)))))
		   (setq count (1- count))
		   new)
		  (t elt)))
      (setq index (+ index incrementer)))
    (do ()
	((= index right))
      (setf (aref result index) (aref sequence index))
      (setq index (+ index incrementer)))
    result))

(eval-when (compile eval)


(defmacro subst-dispatch (pred)
 `(if (listp sequence)
      (if from-end
	  (nreverse (list-substitute* ,pred new (reverse sequence)
				      (- (the fixnum length) (the fixnum end))
				      (- (the fixnum length) (the fixnum start))
				      count key test test-not old))
	  (list-substitute* ,pred new sequence start end count key test test-not
			    old))
      (if from-end
	  (vector-substitute* ,pred new sequence -1 (1- (the fixnum length))
			      -1 length (1- (the fixnum end))
			      (1- (the fixnum start)) count key test test-not old)
	  (vector-substitute* ,pred new sequence 1 0 length length
	   start end count key test test-not old))))

)


;;; Substitute:

(defun substitute (new old sequence &key from-end (test #'eql) test-not
		   (start 0) count end key)
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements equal to Old are replaced with New.  See manual
  for details."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (declare (type index length end)
	     (fixnum count))
    (subst-dispatch 'normal)))


;;; Substitute-If:

(defun substitute-if (new test sequence &key from-end (start 0) end count key)
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements satisfying the Test are replaced with New.  See
  manual for details."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count))
	 test-not
	 old)
    (declare (type index length end)
	     (fixnum count))
    (subst-dispatch 'if)))
  

;;; Substitute-If-Not:

(defun substitute-if-not (new test sequence &key from-end (start 0)
			   end count key)
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements not satisfying the Test are replaced with New.
  See manual for details."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count))
	 test-not
	 old)
    (declare (type index length end)
	     (fixnum count))
    (subst-dispatch 'if-not)))



;;; NSubstitute:

(defun nsubstitute (new old sequence &key from-end (test #'eql) test-not 
		     end count key (start 0))
  "Returns a sequence of the same kind as Sequence with the same elements
  except that all elements equal to Old are replaced with New.  The Sequence
  may be destroyed.  See manual for details."
  (declare (fixnum start))
  (let ((end (or end (length sequence)))
	(count (real-count count)))
    (declare (fixnum count))
    (if (listp sequence)
	(if from-end
	    (let ((length (length sequence)))
	      (nreverse (nlist-substitute*
			 new old (nreverse (the list sequence))
			 test test-not (- length end) (- length start) count key)))
	    (nlist-substitute* new old sequence
			       test test-not start end count key))
	(if from-end
	    (nvector-substitute* new old sequence -1
				 test test-not (1- end) (1- start) count key)
	    (nvector-substitute* new old sequence 1
				 test test-not start end count key)))))

(defun nlist-substitute* (new old sequence test test-not start end count key)
  (declare (fixnum start count end))
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (declare (fixnum index))
    (when (if test-not
	      (not (funcall test-not old (apply-key key (car list))))
	      (funcall test old (apply-key key (car list))))
      (rplaca list new)
      (setq count (1- count)))))

(defun nvector-substitute* (new old sequence incrementer
			    test test-not start end count key)
  (declare (fixnum start incrementer count end))
  (do ((index start (+ index incrementer)))
      ((or (= index end) (= count 0)) sequence)
    (declare (fixnum index))
    (when (if test-not
	      (not (funcall test-not old (apply-key key (aref sequence index))))
	      (funcall test old (apply-key key (aref sequence index))))
      (setf (aref sequence index) new)
      (setq count (1- count)))))


;;; NSubstitute-If:

(defun nsubstitute-if (new test sequence &key from-end (start 0) end count key)
  "Returns a sequence of the same kind as Sequence with the same elements
   except that all elements satisfying the Test are replaced with New.  The
   Sequence may be destroyed.  See manual for details."
  (declare (fixnum start))
  (let ((end (or end (length sequence)))
	(count (real-count count)))
    (declare (fixnum end count))
    (if (listp sequence)
	(if from-end
	    (let ((length (length sequence)))
	      (nreverse (nlist-substitute-if*
			 new test (nreverse (the list sequence))
			 (- length end) (- length start) count key)))
	    (nlist-substitute-if* new test sequence
				  start end count key))
	(if from-end
	    (nvector-substitute-if* new test sequence -1
				    (1- end) (1- start) count key)
	    (nvector-substitute-if* new test sequence 1
				    start end count key)))))

(defun nlist-substitute-if* (new test sequence start end count key)
  (declare (fixnum end))
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (when (funcall test (apply-key key (car list)))
      (rplaca list new)
      (setq count (1- count)))))

(defun nvector-substitute-if* (new test sequence incrementer
			       start end count key)
  (do ((index start (+ index incrementer)))
      ((or (= index end) (= count 0)) sequence)
    (when (funcall test (apply-key key (aref sequence index)))
      (setf (aref sequence index) new)
      (setq count (1- count)))))


;;; NSubstitute-If-Not:

(defun nsubstitute-if-not (new test sequence &key from-end (start 0)
			       end count key)
  "Returns a sequence of the same kind as Sequence with the same elements
   except that all elements not satisfying the Test are replaced with New.
   The Sequence may be destroyed.  See manual for details."
  (declare (fixnum start))
  (let ((end (or end (length sequence)))
	(count (real-count count)))
    (declare (fixnum end count))
    (if (listp sequence)
	(if from-end
	    (let ((length (length sequence)))
	      (nreverse (nlist-substitute-if-not*
			 new test (nreverse (the list sequence))
			 (- length end) (- length start) count key)))
	    (nlist-substitute-if-not* new test sequence
				      start end count key))
	(if from-end
	    (nvector-substitute-if-not* new test sequence -1
					(1- end) (1- start) count key)
	    (nvector-substitute-if-not* new test sequence 1
					start end count key)))))

(defun nlist-substitute-if-not* (new test sequence start end count key)
  (declare (fixnum end))
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (when (not (funcall test (apply-key key (car list))))
      (rplaca list new)
      (setq count (1- count)))))

(defun nvector-substitute-if-not* (new test sequence incrementer
				   start end count key)
  (do ((index start (+ index incrementer)))
      ((or (= index end) (= count 0)) sequence)
    (when (not (funcall test (apply-key key (aref sequence index))))
      (setf (aref sequence index) new)
      (setq count (1- count)))))


;;; Locater macros used by FIND and POSITION.

(eval-when (compile eval)

(defmacro vector-locater-macro (sequence body-form return-type)
  `(let ((incrementer (if from-end -1 1))
	 (start (if from-end (1- (the fixnum end)) start))
	 (end (if from-end (1- (the fixnum start)) end)))
     (declare (fixnum start end incrementer))
     (do ((index start (+ index incrementer))
	  ,@(case return-type (:position nil) (:element '(current))))
	 ((= index end) ())
       (declare (fixnum index))
       ,@(case return-type
	   (:position nil)
	   (:element `((setf current (aref ,sequence index)))))
       ,body-form)))

(defmacro locater-test-not (item sequence seq-type return-type)
  (let ((seq-ref (case return-type
		   (:position
		    (case seq-type
		      (:vector `(aref ,sequence index))
		      (:list `(pop ,sequence))))
		   (:element 'current)))
	(return (case return-type
		  (:position 'index)
		  (:element 'current))))
    `(if test-not
	 (if (not (funcall test-not ,item (apply-key key ,seq-ref)))
	     (return ,return))
	 (if (funcall test ,item (apply-key key ,seq-ref))
	     (return ,return)))))

(defmacro vector-locater (item sequence return-type)
  `(vector-locater-macro ,sequence
			 (locater-test-not ,item ,sequence :vector ,return-type)
			 ,return-type))

(defmacro locater-if-test (test sequence seq-type return-type sense)
  (let ((seq-ref (case return-type
		   (:position
		    (case seq-type
		      (:vector `(aref ,sequence index))
		      (:list `(pop ,sequence))))
		   (:element 'current)))
	(return (case return-type
		  (:position 'index)
		  (:element 'current))))
    (if sense
	`(if (funcall ,test (apply-key key ,seq-ref))
	     (return ,return))
	`(if (not (funcall ,test (apply-key key ,seq-ref)))
	     (return ,return)))))

(defmacro vector-locater-if-macro (test sequence return-type sense)
  `(vector-locater-macro ,sequence
			 (locater-if-test ,test ,sequence :vector ,return-type ,sense)
			 ,return-type))

(defmacro vector-locater-if (test sequence return-type)
  `(vector-locater-if-macro ,test ,sequence ,return-type t))

(defmacro vector-locater-if-not (test sequence return-type)
  `(vector-locater-if-macro ,test ,sequence ,return-type nil))


(defmacro list-locater-macro (sequence body-form return-type)
  `(if from-end
       (do ((sequence (nthcdr (- (the fixnum (length sequence))
				 (the fixnum end))
			      (reverse (the list ,sequence))))
	    (index (1- (the fixnum end)) (1- index))
	    (terminus (1- (the fixnum start)))
	    ,@(case return-type (:position nil) (:element '(current))))
	   ((or (= index terminus) (null sequence)) ())
	 (declare (fixnum index terminus))
	 ,@(case return-type
	     (:position nil)
	     (:element `((setf current (pop ,sequence)))))
	 ,body-form)
       (do ((sequence (nthcdr start ,sequence))
	    (index start (1+ index))
	    ,@(case return-type (:position nil) (:element '(current))))
	   ((or (= index (the fixnum end)) (null sequence)) ())
	 (declare (fixnum index))
	 ,@(case return-type
	     (:position nil)
	     (:element `((setf current (pop ,sequence)))))
	 ,body-form)))

(defmacro list-locater (item sequence return-type)
  `(list-locater-macro ,sequence
		       (locater-test-not ,item ,sequence :list ,return-type)
		       ,return-type))

(defmacro list-locater-if-macro (test sequence return-type sense)
  `(list-locater-macro ,sequence
		       (locater-if-test ,test ,sequence :list ,return-type ,sense)
		       ,return-type))

(defmacro list-locater-if (test sequence return-type)
  `(list-locater-if-macro ,test ,sequence ,return-type t))

(defmacro list-locater-if-not (test sequence return-type)
  `(list-locater-if-macro ,test ,sequence ,return-type nil))

) ; eval-when


;;; Position:

(eval-when (compile eval)

(defmacro vector-position (item sequence)
  `(vector-locater ,item ,sequence :position))

(defmacro list-position (item sequence)
  `(list-locater ,item ,sequence :position))

) ; eval-when


;;; POSITION cannot default end to the length of sequence since it is not
;;; an error to supply nil for its value.  We must test for end being nil
;;; in the body of the function, and this is actually done in the support
;;; routines for other reasons (see below).
(defun position (item sequence &key from-end (test #'eql) test-not (start 0)
		  end key)
  "Returns the zero-origin index of the first element in SEQUENCE
   satisfying the test (default is EQL) with the given ITEM"
  (seq-dispatch sequence
    (list-position* item sequence from-end test test-not start end key)
    (vector-position* item sequence from-end test test-not start end key)))


;;; The support routines for SUBSEQ are used by compiler transforms, so we
;;; worry about dealing with end being supplied as or defaulting to nil
;;; at this level.

(defun list-position* (item sequence from-end test test-not start end key)
  (declare (fixnum start))
  (when (null end) (setf end (length sequence)))
  (list-position item sequence))

(defun vector-position* (item sequence from-end test test-not start end key)
  (declare (fixnum start))
  (when (null end) (setf end (length sequence)))
  (vector-position item sequence))


;;; Position-if:

(eval-when (compile eval)

(defmacro vector-position-if (test sequence)
  `(vector-locater-if ,test ,sequence :position))


(defmacro list-position-if (test sequence)
  `(list-locater-if ,test ,sequence :position))

)

(defun position-if (test sequence &key from-end (start 0) key end)
  "Returns the zero-origin index of the first element satisfying test(el)"
  (declare (fixnum start))
  (let ((end (or end (length sequence))))
    (declare (type index end))
    (seq-dispatch sequence
		  (list-position-if test sequence)
		  (vector-position-if test sequence))))


;;; Position-if-not:

(eval-when (compile eval)

(defmacro vector-position-if-not (test sequence)
  `(vector-locater-if-not ,test ,sequence :position))

(defmacro list-position-if-not (test sequence)
  `(list-locater-if-not ,test ,sequence :position))

)

(defun position-if-not (test sequence &key from-end (start 0) key end)
  "Returns the zero-origin index of the first element not satisfying test(el)"
  (declare (fixnum start))
  (let ((end (or end (length sequence))))
    (declare (type index end))
    (seq-dispatch sequence
		  (list-position-if-not test sequence)
		  (vector-position-if-not test sequence))))


;;; Find:

(eval-when (compile eval)

(defmacro vector-find (item sequence)
  `(vector-locater ,item ,sequence :element))

(defmacro list-find (item sequence)
  `(list-locater ,item ,sequence :element))

)

;;; FIND cannot default end to the length of sequence since it is not
;;; an error to supply nil for its value.  We must test for end being nil
;;; in the body of the function, and this is actually done in the support
;;; routines for other reasons (see above).
(defun find (item sequence &key from-end (test #'eql) test-not (start 0)
	       end key)
  "Returns the first element in SEQUENCE satisfying the test (default
   is EQL) with the given ITEM"
  (declare (fixnum start))
  (seq-dispatch sequence
    (list-find* item sequence from-end test test-not start end key)
    (vector-find* item sequence from-end test test-not start end key)))


;;; The support routines for FIND are used by compiler transforms, so we
;;; worry about dealing with end being supplied as or defaulting to nil
;;; at this level.

(defun list-find* (item sequence from-end test test-not start end key)
  (when (null end) (setf end (length sequence)))
  (list-find item sequence))

(defun vector-find* (item sequence from-end test test-not start end key)
  (when (null end) (setf end (length sequence)))
  (vector-find item sequence))


;;; Find-if:

(eval-when (compile eval)

(defmacro vector-find-if (test sequence)
  `(vector-locater-if ,test ,sequence :element))

(defmacro list-find-if (test sequence)
  `(list-locater-if ,test ,sequence :element))

)

(defun find-if (test sequence &key from-end (start 0) end key)
  "Returns the first element in SEQUENCE satisfying the test."
  (declare (fixnum start))
  (let ((end (or end (length sequence))))
    (declare (type index end))
    (seq-dispatch sequence
		  (list-find-if test sequence)
		  (vector-find-if test sequence))))


;;; Find-if-not:

(eval-when (compile eval)

(defmacro vector-find-if-not (test sequence)
  `(vector-locater-if-not ,test ,sequence :element))

(defmacro list-find-if-not (test sequence)
  `(list-locater-if-not ,test ,sequence :element))

)

(defun find-if-not (test sequence &key from-end (start 0) end key)
  "Returns the first element in SEQUENCE not satisfying the test."
  (declare (fixnum start))
  (let ((end (or end (length sequence))))
    (declare (type index end))
    (seq-dispatch sequence
		  (list-find-if-not test sequence)
		  (vector-find-if-not test sequence))))


;;; Count:

(eval-when (compile eval)

(defmacro vector-count-if (not-p from-end-p predicate sequence)
  (let ((next-index (if from-end-p '(1- index) '(1+ index)))
	(pred `(funcall ,predicate (apply-key key (aref ,sequence index)))))
    `(let ((%start ,(if from-end-p '(1- end) 'start))
	   (%end ,(if from-end-p '(1- start) 'end)))
       (do ((index %start ,next-index)
	    (count 0))
	   ((= index (the fixnum %end)) count)
	 (declare (fixnum index count))
	 (,(if not-p 'unless 'when) ,pred
	   (setq count (1+ count)))))))

(defmacro list-count-if (not-p from-end-p predicate sequence)
  (let ((pred `(funcall ,predicate (apply-key key (pop sequence)))))
    `(let ((%start ,(if from-end-p '(- length end) 'start))
	   (%end ,(if from-end-p '(- length start) 'end))
	   (sequence ,(if from-end-p '(reverse sequence) 'sequence)))
       (do ((sequence (nthcdr %start ,sequence))
	    (index %start (1+ index))
	    (count 0))
	   ((or (= index (the fixnum %end)) (null sequence)) count)
	 (declare (fixnum index count))
	 (,(if not-p 'unless 'when) ,pred
	   (setq count (1+ count)))))))

)

(defun count (item sequence &key from-end (test #'eql test-p) (test-not nil test-not-p)
		   (start 0) end key)
  "Returns the number of elements in SEQUENCE satisfying a test with ITEM,
   which defaults to EQL."
  (declare (fixnum start))
  (when (and test-p test-not-p)
    ;; ANSI Common Lisp has left the behavior in this situation unspecified.
    ;; (CLHS 17.2.1)
    (error ":TEST and :TEST-NOT are both present."))
  (let* ((length (length sequence))
	 (end (or end length)))
    (declare (type index end))
    (let ((%test (if test-not-p
		     (lambda (x)
		       (not (funcall test-not item x)))
		     (lambda (x)
		       (funcall test item x)))))
      (seq-dispatch sequence
		    (if from-end
			(list-count-if nil t %test sequence)
			(list-count-if nil nil %test sequence))
		    (if from-end
			(vector-count-if nil t %test sequence)
			(vector-count-if nil nil %test sequence))))))


;;; Count-if:

(defun count-if (test sequence &key from-end (start 0) end key)
  "Returns the number of elements in SEQUENCE satisfying TEST(el)."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length)))
    (declare (type index end))
    (seq-dispatch sequence
		  (if from-end
		      (list-count-if nil t test sequence)
		      (list-count-if nil nil test sequence))
		  (if from-end
		      (vector-count-if nil t test sequence)
		      (vector-count-if nil nil test sequence)))))

(defun count-if-not (test sequence &key from-end (start 0) end key)
  "Returns the number of elements in SEQUENCE satisfying TEST(el)."
  (declare (fixnum start))
  (let* ((length (length sequence))
	 (end (or end length)))
    (declare (type index end))
    (seq-dispatch sequence
		  (if from-end
		      (list-count-if t t test sequence)
		      (list-count-if t nil test sequence))
		  (if from-end
		      (vector-count-if t t test sequence)
		      (vector-count-if t nil test sequence)))))


;;; Mismatch utilities:

(eval-when (compile eval)


(defmacro match-vars (&rest body)
  `(let ((inc (if from-end -1 1))
	 (start1 (if from-end (1- (the fixnum end1)) start1))
	 (start2 (if from-end (1- (the fixnum end2)) start2))
	 (end1 (if from-end (1- (the fixnum start1)) end1))
	 (end2 (if from-end (1- (the fixnum start2)) end2)))
     (declare (fixnum inc start1 start2 end1 end2))
     ,@body))

(defmacro matchify-list ((sequence start length end) &body body)
  (declare (ignore end)) ;; ### Should END be used below?
  `(let ((,sequence (if from-end
			(nthcdr (- (the fixnum ,length) (the fixnum ,start) 1)
				(reverse (the list ,sequence)))
			(nthcdr ,start ,sequence))))
     (declare (type list ,sequence))
     ,@body))

)

;;; Mismatch:

(eval-when (compile eval)

(defmacro if-mismatch (elt1 elt2)
  `(cond ((= (the fixnum index1) (the fixnum end1))
	  (return (if (= (the fixnum index2) (the fixnum end2))
		      nil
		      (if from-end
			  (1+ (the fixnum index1))
			  (the fixnum index1)))))
	 ((= (the fixnum index2) (the fixnum end2))
	  (return (if from-end (1+ (the fixnum index1)) index1)))
	 (test-not
	  (if (funcall test-not (apply-key key ,elt1) (apply-key key ,elt2))
	      (return (if from-end (1+ (the fixnum index1)) index1))))
	 (t (if (not (funcall test (apply-key key ,elt1)
			      (apply-key key ,elt2)))
		(return (if from-end (1+ (the fixnum index1)) index1))))))

(defmacro mumble-mumble-mismatch ()
  `(do ((index1 start1 (+ index1 (the fixnum inc)))
	(index2 start2 (+ index2 (the fixnum inc))))
       (())
     (declare (fixnum index1 index2))
     (if-mismatch (aref sequence1 index1) (aref sequence2 index2))))

(defmacro mumble-list-mismatch ()
  `(do ((index1 start1 (+ index1 (the fixnum inc)))
	(index2 start2 (+ index2 (the fixnum inc))))
       (())
     (declare (fixnum index1 index2))
     (if-mismatch (aref sequence1 index1) (pop sequence2))))

(defmacro list-mumble-mismatch ()
  `(do ((index1 start1 (+ index1 (the fixnum inc)))
	(index2 start2 (+ index2 (the fixnum inc))))
       (())
     (declare (fixnum index1 index2))
     (if-mismatch (pop sequence1) (aref sequence2 index2))))

(defmacro list-list-mismatch ()
  `(do ((sequence1 sequence1)
	(sequence2 sequence2)
	(index1 start1 (+ index1 (the fixnum inc)))
	(index2 start2 (+ index2 (the fixnum inc))))
       (())
     (declare (fixnum index1 index2))
     (if-mismatch (pop sequence1) (pop sequence2))))

)

(defun mismatch (sequence1 sequence2 &key from-end (test #'eql) test-not 
			   (start1 0) end1 (start2 0) end2 key)
  "The specified subsequences of Sequence1 and Sequence2 are compared
   element-wise.  If they are of equal length and match in every element, the
   result is NIL.  Otherwise, the result is a non-negative integer, the index
   within Sequence1 of the leftmost position at which they fail to match; or,
   if one is shorter than and a matching prefix of the other, the index within
   Sequence1 beyond the last position tested is returned.  If a non-Nil
   :From-End keyword argument is given, then one plus the index of the
   rightmost position in which the sequences differ is returned."
  (declare (fixnum start1 start2))
  (let* ((length1 (length sequence1))
	 (end1 (or end1 length1))
	 (length2 (length sequence2))
	 (end2 (or end2 length2)))
    (declare (type index length1 end1 length2 end2))
    (match-vars
     (seq-dispatch sequence1
       (matchify-list (sequence1 start1 length1 end1)
	 (seq-dispatch sequence2
	   (matchify-list (sequence2 start2 length2 end2)
	     (list-list-mismatch))
	   (list-mumble-mismatch)))
       (seq-dispatch sequence2
	 (matchify-list (sequence2 start2 length2 end2)
	   (mumble-list-mismatch))
	 (mumble-mumble-mismatch))))))


;;; Search comparison functions:

(eval-when (compile eval)

;;; Compare two elements and return if they don't match:

(defmacro compare-elements (elt1 elt2)
  `(if test-not
       (if (funcall test-not (apply-key key ,elt1) (apply-key key ,elt2))
	   (return nil)
	   t)
       (if (not (funcall test (apply-key key ,elt1) (apply-key key ,elt2)))
	   (return nil)
	   t)))

(defmacro search-compare-list-list (main sub)
  `(do ((main ,main (cdr main))
	(jndex start1 (1+ jndex))
	(sub (nthcdr start1 ,sub) (cdr sub)))
       ((or (null main) (null sub) (= (the fixnum end1) jndex))
	t)
     (declare (fixnum jndex))
     (compare-elements (car sub) (car main))))

(defmacro search-compare-list-vector (main sub)
  `(do ((main ,main (cdr main))
	(index start1 (1+ index)))
       ((or (null main) (= index (the fixnum end1))) t)
     (declare (fixnum index))
     (compare-elements (aref ,sub index) (car main))))

(defmacro search-compare-vector-list (main sub index)
  `(do ((sub (nthcdr start1 ,sub) (cdr sub))
	(jndex start1 (1+ jndex))
	(index ,index (1+ index)))
       ((or (= (the fixnum end1) jndex) (null sub)) t)
     (declare (fixnum jndex index))
     (compare-elements (car sub) (aref ,main index))))

(defmacro search-compare-vector-vector (main sub index)
  `(do ((index ,index (1+ index))
	(sub-index start1 (1+ sub-index)))
       ((= sub-index (the fixnum end1)) t)
     (declare (fixnum sub-index index))
     (compare-elements (aref ,sub sub-index) (aref ,main index))))

(defmacro search-compare (main-type main sub index)
  (if (eq main-type 'list)
      `(seq-dispatch ,sub
		     (search-compare-list-list ,main ,sub)
		     (search-compare-list-vector ,main ,sub))
      `(seq-dispatch ,sub
		     (search-compare-vector-list ,main ,sub ,index)
		     (search-compare-vector-vector ,main ,sub ,index))))

)

(eval-when (compile eval)
 
(defmacro list-search (main sub)
  `(do ((main (nthcdr start2 ,main) (cdr main))
	(index2 start2 (1+ index2))
	(terminus (- (the fixnum end2)
		     (the fixnum (- (the fixnum end1)
				    (the fixnum start1)))))
	(last-match ()))
       ((> index2 terminus) last-match)
     (declare (fixnum index2 terminus))
     (if (search-compare list main ,sub index2)
	 (if from-end
	     (setq last-match index2)
	     (return index2)))))


(defmacro vector-search (main sub)
  `(do ((index2 start2 (1+ index2))
	(terminus (- (the fixnum end2)
		     (the fixnum (- (the fixnum end1)
				    (the fixnum start1)))))
	(last-match ()))
       ((> index2 terminus) last-match)
     (declare (fixnum index2 terminus))
     (if (search-compare vector ,main ,sub index2)
	 (if from-end
	     (setq last-match index2)
	     (return index2)))))

)


(defun search (sequence1 sequence2 &key from-end (test #'eql) test-not 
		(start1 0) end1 (start2 0) end2 key)
  "A search is conducted using EQL for the first subsequence of sequence2 
   which element-wise matches sequence1.  If there is such a subsequence in 
   sequence2, the index of the its leftmost element is returned; 
   otherwise () is returned."
  (declare (fixnum start1 start2))
  (let ((end1 (or end1 (length sequence1)))
	(end2 (or end2 (length sequence2))))
    (seq-dispatch sequence2
		  (list-search sequence2 sequence1)
		  (vector-search sequence2 sequence1))))
