;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/vm-tran.lisp,v 1.58 2005/01/29 22:39:03 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains impelemtentation-dependent transforms.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

;;; We need to define these predicates, since the TYPEP source transform picks
;;; whichever predicate was defined last when there are multiple predicates for
;;; equivalent types.
;;;
(def-source-transform short-float-p (x) `(single-float-p ,x))
#-long-float
(def-source-transform long-float-p (x) `(double-float-p ,x))

(def-source-transform compiled-function-p (x)
  `(functionp ,x))

(def-source-transform char-int (x)
  `(char-code ,x))

(deftransform abs ((x) (rational))
  '(if (< x 0) (- x) x))

;;; For now, the layout is stored in slot 0.
;;;
(def-source-transform %instance-layout (x)
  `(truly-the layout (%instance-ref ,x 0)))
;;;
(def-source-transform %set-instance-layout (x val)
  `(%instance-set ,x 0 (the layout ,val)))


;;;; Character support.

;;; There are really only base-chars.
;;;
(def-source-transform characterp (obj)
  `(base-char-p ,obj))


;;;; Transforms for data-vector-ref for strange array types.

(deftransform data-vector-ref ((array index)
			       (simple-array t))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (when (and (consp dims) (= (length dims) 1))
	(give-up))
      (let* ((el-type (array-type-specialized-element-type array-type))
	     (declared-type (array-type-element-type array-type))
	     (total-size (if (or (atom dims) (member '* dims))
			     '*
			     (reduce #'* dims)))
	     (vector-type `(simple-array ,(type-specifier el-type)
					 (,total-size))))
	(if (atom dims)
	    `(the ,(type-specifier declared-type)
	       (data-vector-ref (truly-the ,vector-type
					   (if (array-header-p array)
					       (%array-data-vector array)
					       array))
				index))
	    `(the ,(type-specifier declared-type)
	       (data-vector-ref (truly-the ,vector-type
					   (%array-data-vector array))
				index)))))))

(deftransform data-vector-ref ((array index) (array t) *
			       :node node  :policy (> speed space))
  (let ((array-type (continuation-type array)))
    (unless (and (array-type-p array-type) (array-type-complexp array-type)
		 (not (eq (array-type-specialized-element-type array-type)
			  *wild-type*)))
      (give-up))
    (delay-transform node :optimize)
    (let* ((dims (array-type-dimensions array-type))
	   (el-type (array-type-specialized-element-type array-type))
	   (declared-type (array-type-element-type array-type))
	   (total-size (if (or (atom dims) (member '* dims))
			   '*
			   (reduce #'* dims)))
	   (vector-type `(simple-array ,(type-specifier el-type)
				       (,total-size))))
      (if (and (consp dims) (> (length dims) 1))
	  `(multiple-value-bind (vector index)
	       (%with-array-data array index nil)
	     (the ,(type-specifier declared-type)
	       (data-vector-ref (truly-the ,vector-type vector) index)))
	  `(multiple-value-bind (vector index)
	       (if (array-header-p array)
		   (%with-array-data array index nil)
		   (values array index))
	     (the ,(type-specifier declared-type)
	       (data-vector-ref (truly-the ,vector-type vector) index)))))))

(deftransform data-vector-set ((array index new-value)
			       (simple-array t t))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (when (and (consp dims) (= (length dims) 1))
	(give-up))
      (let* ((el-type (array-type-element-type array-type))
	     (declared-type (array-type-element-type array-type))
	     (total-size (if (or (atom dims) (member '* dims))
			     '*
			     (reduce #'* dims)))
	     (vector-type `(simple-array ,(type-specifier el-type)
					 (,total-size))))
	(if (atom dims)
	    `(data-vector-set (truly-the ,vector-type
					 (if (array-header-p array)
					     (%array-data-vector array)
					     array))
			      index
			      (the ,(type-specifier declared-type) new-value))
	    `(data-vector-set (truly-the ,vector-type
					 (%array-data-vector array))
			      index
			      (the ,(type-specifier declared-type) new-value)))))))

(deftransform data-vector-set ((array index new-value) (array t t) *
			       :node node  :policy (> speed space))
  (let ((array-type (continuation-type array)))
    (unless (and (array-type-p array-type) (array-type-complexp array-type)
		 (not (eq (array-type-specialized-element-type array-type)
			  *wild-type*)))
      (give-up))
    (delay-transform node :optimize)
    (let* ((dims (array-type-dimensions array-type))
	   (el-type (array-type-element-type array-type))
	   (declared-type (array-type-element-type array-type))
	   (total-size (if (or (atom dims) (member '* dims))
			   '*
			   (reduce #'* dims)))
	   (vector-type `(simple-array ,(type-specifier el-type)
				       (,total-size))))
      (if (and (consp dims) (> (length dims) 1))
	  `(multiple-value-bind (vector index)
	       (%with-array-data array index nil)
	     (data-vector-set (truly-the ,vector-type vector)
			      index
			      (the ,(type-specifier declared-type) new-value)))
	  `(multiple-value-bind (vector index)
	       (if (array-header-p array)
		   (%with-array-data array index nil)
		   (values array index))
	     (data-vector-set (truly-the ,vector-type vector)
			      index
			      (the ,(type-specifier declared-type) new-value)))))))


;;; Transforms for getting at arrays of unsigned-byte n when n < 8.

#+nil
(macrolet
    ((frob (type bits)
       (let ((elements-per-word (truncate vm:word-bits bits)))
	 `(progn
	    (deftransform data-vector-ref ((vector index)
					   (,type *))
	      `(multiple-value-bind (word bit)
				    (floor index ,',elements-per-word)
		 (ldb ,(ecase vm:target-byte-order
			 (:little-endian '(byte ,bits (* bit ,bits)))
			 (:big-endian '(byte ,bits (- vm:word-bits
						      (* (1+ bit) ,bits)))))
		      (%raw-bits vector (+ word vm:vector-data-offset)))))
	    (deftransform data-vector-set ((vector index new-value)
					   (,type * *))
	      `(multiple-value-bind (word bit)
				    (floor index ,',elements-per-word)
		 (setf (ldb ,(ecase vm:target-byte-order
			       (:little-endian '(byte ,bits (* bit ,bits)))
			       (:big-endian
				'(byte ,bits (- vm:word-bits
						(* (1+ bit) ,bits)))))
			    (%raw-bits vector (+ word vm:vector-data-offset)))
		       new-value)))))))
  (frob simple-bit-vector 1)
  (frob (simple-array (unsigned-byte 2) (*)) 2)
  (frob (simple-array (unsigned-byte 4) (*)) 4))


;;;; Simple string transforms:

(defconstant vector-data-bit-offset (* vm:vector-data-offset vm:word-bits))

(deftransform subseq ((string start &optional (end nil))
		      (simple-string t &optional t))
  '(let* ((len (length string))
	  (end (if end (min end len) len))
	  (start (min start end))
	  (size (- end start))
	  (result (make-string size)))
     (declare (optimize (safety 0)))
     (bit-bash-copy string
		    (the index
			 (+ (the index (* start vm:byte-bits))
			    vector-data-bit-offset))
		    result
		    vector-data-bit-offset
		    (the index (* size vm:byte-bits)))
     result))


(deftransform copy-seq ((seq) (simple-string))
  '(let* ((len (length seq))
	  (res (make-string len)))
     (declare (optimize (safety 0)))
     (bit-bash-copy seq
		    vector-data-bit-offset
		    res
		    vector-data-bit-offset
		    (the index (* len vm:byte-bits)))
     res))


(deftransform replace ((string1 string2 &key (start1 0) (start2 0)
				end1 end2)
		       (simple-string simple-string &rest t))
  '(locally (declare (optimize (safety 0)))
     (bit-bash-copy string2
		    (the index
			 (+ (the index (* start2 vm:byte-bits))
			    vector-data-bit-offset))
		    string1
		    (the index
			 (+ (the index (* start1 vm:byte-bits))
			    vector-data-bit-offset))
		    (the index
			 (* (min (the index (- (or end1 (length string1))
					       start1))
				 (the index (- (or end2 (length string2))
					       start2)))
			    vm:byte-bits)))
     string1))

;; The original version of this deftransform seemed to cause the
;; compiler to spend huge amounts of time deriving the type of the
;; START variable.  The following version uses nested lets to prevent
;; the compiler from doing this analysis.  This only hides the
;; symptom.

(deftransform concatenate ((rtype &rest sequences)
			   (t &rest simple-string)
			   simple-string
			   :policy (< safety 3))
  (collect ((lets)
	    (forms)
	    (all-lengths)
	    (args))
    (dolist (seq sequences)
      (declare (ignore seq))
      (let ((n-seq (gensym))
	    (n-length (gensym)))
	(args n-seq)
	(lets `(,n-length (the index (* (length ,n-seq) vm:byte-bits))))
	(all-lengths n-length)
	(forms `((bit-bash-copy ,n-seq vector-data-bit-offset
		  res start
		  ,n-length)
		 (start (+ start ,n-length))))))
    (flet ((nestify (lists)
	     (let* ((lists (reverse lists))
		    (result `(,(caar lists))))
	       (dolist (item (rest lists))
		 (destructuring-bind (bit-bash init)
		     item
		   (setf result `(,bit-bash
				  (let (,init)
				    ,@result)))))
	       result)))
      (let ((result 
	     `(lambda (rtype ,@(args))
	       (declare (ignore rtype))
	       (let* (,@(lets)
			(res (make-string (truncate (the index (+ ,@(all-lengths)))
						    vm:byte-bits))))
		 (declare (type index ,@(all-lengths)))
		 (let ((start vector-data-bit-offset))
		   ,@(nestify (forms)))
		 res))))
	result))))



;;;; Bit vector hackery:


;;; SIMPLE-BIT-VECTOR bit-array operations are transformed to a word loop that
;;; does 32 bits at a time.
;;;
(dolist (x '((bit-and 32bit-logical-and)
	     (bit-ior 32bit-logical-or)
	     (bit-xor 32bit-logical-xor)
	     (bit-eqv 32bit-logical-eqv)
	     (bit-nand 32bit-logical-nand)
	     (bit-nor 32bit-logical-nor)
	     (bit-andc1 32bit-logical-andc1)
	     (bit-andc2 32bit-logical-andc2)
	     (bit-orc1 32bit-logical-orc1)
	     (bit-orc2 32bit-logical-orc2)))
  (destructuring-bind (bitfun wordfun) x
    (deftransform bitfun
		  ((bit-array-1 bit-array-2 result-bit-array)
		   '(simple-bit-vector simple-bit-vector simple-bit-vector) '*
		   :eval-name t  :node node  :policy (>= speed space))
      `(progn
	 ,@(unless (policy node (zerop safety))
	     '((unless (= (length bit-array-1) (length bit-array-2)
			  (length result-bit-array))
		 (error "Argument and/or result bit arrays not the same length:~
			 ~%  ~S~%  ~S  ~%  ~S"
			bit-array-1 bit-array-2 result-bit-array))))
	 (let ((length (length result-bit-array)))
	   (if (= length 0)
	       ;; We avoid doing anything to 0-length
	       ;; bit-vectors, or rather, the memory that
	       ;; follows them. Other divisible-by-32 cases
	       ;; are handled by the (1- length), below.
	       ;; CSR, 2002-04-24
	       result-bit-array
	       (do ((index vm:vector-data-offset (1+ index))
		    (end-1 (+ vm:vector-data-offset
			      ;; bit-vectors of length 1-32
			      ;; need precisely one (SETF
			      ;; %RAW-BITS), done in the
			      ;; epilogue. - CSR, 2002-04-24
			      (truncate (truly-the index (1- length))
					vm:word-bits))))
		   ((= index end-1)
		    (setf (%raw-bits result-bit-array index)
			  (,wordfun (%raw-bits bit-array-1 index)
				    (%raw-bits bit-array-2 index)))
		    result-bit-array)
		 (declare (optimize (speed 3) (safety 0))
			  (type index index end-1))
		 (setf (%raw-bits result-bit-array index)
		       (,wordfun (%raw-bits bit-array-1 index)
				 (%raw-bits bit-array-2 index))))))))))

(deftransform bit-not
	      ((bit-array result-bit-array)
	       (simple-bit-vector simple-bit-vector) *
	       :node node  :policy (>= speed space))
  `(progn
     ,@(unless (policy node (zerop safety))
	 '((unless (= (length bit-array)
		      (length result-bit-array))
	     (error "Argument and result bit arrays not the same length:~
	     	     ~%  ~S~%  ~S"
		    bit-array result-bit-array))))
    (let ((length (length result-bit-array)))
      (if (= length 0)
	  ;; We avoid doing anything to 0-length bit-vectors, or
	  ;; rather, the memory that follows them. Other
	  ;; divisible-by-32 cases are handled by the (1- length),
	  ;; below.  CSR, 2002-04-24
	  result-bit-array
	  (do ((index vm:vector-data-offset (1+ index))
	       (end-1 (+ vm:vector-data-offset
			 ;; bit-vectors of length 1-32 need precisely
			 ;; one (SETF %RAW-BITS), done in the epilogue.
			 (truncate (truly-the index (1- length))
				   vm:word-bits))))
	      ((= index end-1)
	       (setf (%raw-bits result-bit-array index)
		     (32bit-logical-not (%raw-bits bit-array index)))
	       result-bit-array)
	    (declare (optimize (speed 3) (safety 0))
		     (type index index end-1))
	    (setf (%raw-bits result-bit-array index)
		  (32bit-logical-not (%raw-bits bit-array index))))))))

(deftransform count ((item sequence) (bit simple-bit-vector) *
                     :policy (>= speed space))
  `(let ((length (length sequence)))
     (if (zerop length)
	 0
	 (multiple-value-bind (nwords extra)
	     (truncate (truly-the index length) vm:word-bits)
	   ;; This loop counts the number of 1 bits in whole words
	   (do ((index vm:vector-data-offset (1+ index))
		(count 0)
		(end (+ vm:vector-data-offset nwords)))
	       ((= index end)
		(let ((ones
		       ;; Count the number of one bits in the last
		       ;; word, if any, masking off any junk bits.
		       (if (zerop extra)
			   count
			   (let* ((bits (ldb (byte extra
						   ,(ecase (c:backend-byte-order c:*target-backend*)
							   (:little-endian 0)
							   (:big-endian
							    '(- vm:word-bits extra))))
					     (%raw-bits sequence index))))
			     (incf count (logcount bits))))))
		  ;; If we're counting ones, the we're done.  If we're
		  ;; counting zeroes, we need to subtract the number
		  ;; of ones from the length, obviously.
		  ,(if (constant-continuation-p item)
		       (if (zerop (continuation-value item))
			   '(- length ones)
			   'ones)
		       '(if (zerop item)
			  (- length ones)
			  ones))))
	     (declare (type index index count end)
		      (optimize (speed 3) (safety 0)))
	     (incf count (logcount (%raw-bits sequence index))))))))

;;;; Primitive translator for byte-blt


(def-primitive-translator byte-blt (src src-start dst dst-start dst-end)
  `(let ((src ,src)
	 (src-start (* ,src-start vm:byte-bits))
	 (dst ,dst)
	 (dst-start (* ,dst-start vm:byte-bits))
	 (dst-end (* ,dst-end vm:byte-bits)))
     (let ((length (- dst-end dst-start)))
       (etypecase src
	 (system-area-pointer
	  (etypecase dst
	    (system-area-pointer
	     (system-area-copy src src-start dst dst-start length))
	    ((simple-unboxed-array (*))
	     (copy-from-system-area src src-start
				    dst (+ dst-start vector-data-bit-offset)
				    length))))
	 ((simple-unboxed-array (*))
	  (etypecase dst
	    (system-area-pointer
	     (copy-to-system-area src (+ src-start vector-data-bit-offset)
				  dst dst-start
				  length))
	    ((simple-unboxed-array (*))
	     (bit-bash-copy src (+ src-start vector-data-bit-offset)
			    dst (+ dst-start vector-data-bit-offset)
			    length))))))))

;;;; SXHASH:

;;; Should be in VM:

(defconstant sxhash-bits-byte (byte 29 0))
(defconstant sxmash-total-bits 29)
(defconstant sxmash-rotate-bits 9)

(deftransform sxhash ((s-expr) (integer))
  '(ldb sxhash-bits-byte s-expr))

(deftransform sxhash ((s-expr) (simple-string))
  '(%sxhash-simple-string s-expr))

#-(or sparc x86 amd64 ppc)
(deftransform sxhash ((s-expr) (symbol))
  '(%sxhash-simple-string (symbol-name s-expr)))

#+(or sparc x86 amd64 ppc)
(deftransform sxhash ((s-expr) (symbol))
  ;; Pick off the constant case first. (Important!)
  (if (constant-continuation-p s-expr)
      (sxhash (continuation-value s-expr))
      '(let ((result (symbol-hash s-expr)))
	;; A 0 in the symbol-hash slot means uninitialized.  We should
	;; probably use -1 instead, once we make sure that NIL doesn't
	;; have a negative hash value.
	(if (<= result 0)
	    (let ((sxhash (%sxhash-simple-string (symbol-name s-expr))))
	      ;; If 0 is the uninitialized indicator, should we make
	      ;; sure we never store 0 into this slot?  It would be
	      ;; pretty bad if it that happens.
	      (%set-symbol-hash s-expr sxhash)
	      sxhash)
	    result))))



(deftransform sxhash ((s-expr) (single-float))
  '(let ((bits (single-float-bits s-expr)))
     (ldb sxhash-bits-byte
	  (logxor (ash bits (- sxmash-rotate-bits)) bits))))

(deftransform sxhash ((s-expr) (double-float))
  '(let* ((lo (double-float-low-bits s-expr))
	  (hi (double-float-high-bits s-expr)))
     (ldb sxhash-bits-byte
	  (logxor (ash lo (- sxmash-rotate-bits)) lo
		  (ldb sxhash-bits-byte
		       (logxor (ash hi (- sxmash-rotate-bits)) hi))))))

#+long-float
(deftransform sxhash ((s-expr) (long-float))
  '(let* ((lo (long-float-low-bits s-expr))
	  #+sparc (mid (long-float-mid-bits s-expr))
	  (hi (long-float-high-bits s-expr))
	  (exp (long-float-exp-bits s-expr)))
     (ldb sxhash-bits-byte
	  (logxor (ash lo (- sxmash-rotate-bits)) lo
		  #+sparc (ash mid (- sxmash-rotate-bits)) #+sparc mid
		  (ash hi (- sxmash-rotate-bits)) hi
		  (ldb sxhash-bits-byte
		       (logxor (ash exp (- sxmash-rotate-bits)) exp))))))


;;;; Float EQL transforms.

(deftransform eql ((x y) (single-float single-float))
  '(= (single-float-bits x) (single-float-bits y)))

(deftransform eql ((x y) (double-float double-float))
  '(and (= (double-float-low-bits x) (double-float-low-bits y))
	(= (double-float-high-bits x) (double-float-high-bits y))))

;;; There are two different ways the multiplier can be recoded. The
;;; more obvious is to shift X by the correct amount for each bit set
;;; in Y and to sum the results. But if there is a string of bits that
;;; are all set, you can add X shifted by one more then the bit
;;; position of the first set bit and subtract X shifted by the bit
;;; position of the last set bit. We can't use this second method when
;;; the high order bit is bit 31 because shifting by 32 doesn't work
;;; too well.
(defun strength-reduce-constant-multiply (arg num)
  (declare (type (unsigned-byte 32) num))
  (let ((adds 0) (shifts 0)
	(result nil) first-one)
    (labels ((add (next-factor)
	       (setf result
		     (if result
                         (progn (incf adds) `(+ ,result ,next-factor))
                         next-factor))))
      (declare (inline add))
      (dotimes (bitpos 32)
	(if first-one
	    (when (not (logbitp bitpos num))
	      (add (if (= (1+ first-one) bitpos)
		       ;; There is only a single bit in the string.
		       (progn (incf shifts) `(ash ,arg ,first-one))
		       ;; There are at least two.
		       (progn
			 (incf adds)
			 (incf shifts 2)
			 `(- (ash ,arg ,bitpos)
			     (ash ,arg ,first-one)))))
	      (setf first-one nil))
	    (when (logbitp bitpos num)
	      (setf first-one bitpos))))
      (when first-one
	(cond ((= first-one 31))
	      ((= first-one 30)
	       (incf shifts)
	       (add `(ash ,arg 30)))
	      (t
	       (incf shifts 2)
	       (incf adds)
	       (add `(- (ash ,arg 31)
			(ash ,arg ,first-one)))))
	(incf shifts)
	(add `(ash ,arg 31))))
    (values (if (plusp adds)
                `(logand ,result #.(1- (ash 1 32))) ; using modular arithmetic
                result)
            adds
            shifts)))


;;; Support routines for division by multiplication.

;; Truncating division by multiplication
;;
;; This is taken from Hacker's Delight, by Henry S. Warren.  This
;; book describes how to do a truncating division by doing a
;; multiplication instead.
;;
;; We refer the reader to that book for a full description and proof
;; of the algorithm.  We summarize the basic ideas here.
;;
;; Let W be the word size in bits and d be the known divisor, 2 <= d
;; < 2^(W-1).  We wish to find the least integer m and integer p such
;; that
;;
;;   floor(m*n/2^p) = floor(n/d) for 0 <= n < 2^(W-1)
;;
;; and
;;
;;   floor(m*n/2^p) + 1 = floor(n/d) for -2^(W-1) <= n <= -1
;;
;; for 0 <= m < 2^W and p >= W.
;;
;; This is found by the following algorithm.
;;
;; Compute
;;
;;   nc = floor(2^(W-1)/d)*d - 1 = 2^(W-1) - rem(2^(W-1), d) - 1.
;;
;; Then find p >= W such that
;;
;;   2^p > nc * (d - rem(2^p, d)
;;
;; This gives
;;
;;   m = (2^p - d - rem(2^p, d))/d
;;
;; From m we compute the desired multiplier, M, because in some cases
;; m will not fit in a signed W-bit word.  Hence,
;;
;;       { m,        if 0 <= m < 2^(W-1)
;;   M = {
;;       { m - 2^W,  if 2^(W-1) <= m < 2^W
;;
;; Then the basic algorithm is to compute
;;
;;   floor(m*n/2^p) = floor((m*n/2^W)/2^(p-W))
;;
;; When m is too large, we compute
;;
;;   (m*n/2^W) = ((m - 2^W + 2^W)*n/2^W)
;;             = (m - 2^W)*n/2^W + n
;;             = M*n/2^W + n
;; and (m - 2^W) fits in a W-bit word.
;;
;; And we're done.
;;
;; For example:
;;
;;   d      M          s = p - W
;; ------------------------------------
;;   3   #x55555556       0
;;   5   #x66666667       1
;;   7   #x-6DB6DB6D      2
;; 100   #x51EB851F       5
;;
;; Finally, we note that if n is negative, we can easily add 1 to
;; floor(m*n/p) without branches.  Let t = n >> (- (W - 1)).  That
;; is, t = -1 if n is negative and t = 0 if n is positive.
;;
;; Then subtract t from floor(m*n/p).  (Note that the example machine
;; code in Hacker's delight is wrong.  It adds t instead of
;; subtracting t.


;; Find the magic number for the divisor DIVISOR assuming a word size
;; of WORD-WIDTH bits.  We return the (signed) magic number M and s =
;; p - W.
(defun find-signed-reciprocal (divisor &optional (word-width vm:word-bits))
  (let ((nc (1- (* divisor (floor (ash 1 (1- word-width)) divisor)))))
    ;; Find p
    (do ((p word-width (1+ p)))
        ((> (ash 1 p)
            (* nc (- divisor (rem (ash 1 p) divisor))))
         (let ((m (/ (- (+ (ash 1 p) divisor)
                               (rem (ash 1 p) divisor))
                            divisor)))
           (values (if (< m (ash 1 (1- word-width)))
                       m
                       (- m (ash 1 word-width)))
                   (- p word-width))))
      )))

;; Unsigned division is a bit more complicated.  We can't just use
;; the above results to get the correct unsigned division.
;;
;; Using the same notation, we want to find m and p such that
;;
;;   floor(m*n/2^p) = floor(n/d) for 0 <= n < 2^W.
;;
;; with 0 <= m < 2^(W+1) and p >= W.
;;
;; First compute
;;
;;   nc = floor(2^W/d)*d - 1 = 2^W - rem(2^W,d) - 1
;;
;; Then find p such that
;;
;;   2^p > nc * (d - 1 - rem(2^p - 1, d))
;;
;; and m is
;;
;;   m = (2^p + d - 1 - rem(2^p - 1, d))/d
;;
;; If m fits in a word, the multiplier M = m.  However, if m cannot
;; fit in a word, we set the multiplier M to be m - 2^W, which needs
;; to be adjusted.
;;
;; So,
;;   floor(m*n/2^W/2^s) = floor((m - 2^W + 2^W)*n/2^W/2^s)
;;                      = floor([(m - 2^W)*n/2^W + n]/2^s)
;;                      = floor([M*n/2^W + n]/2^s)
;;
;; We would be done, except the sum can overflow.  If the architecture
;; has an instruction that can shift the carry bit into the MSB during
;; a right shift, then we are done.  If the architecture does not, we
;; can use the following approach.
;;
;;   floor((q+n)/2^p) = floor(z/2^(p-1)), p >= 1
;;
;; where
;;
;;   z = floor((n-q)/2) + q
;;
;; This requires that p >= 1, but it can be shown that if d > 1, and
;; if m >= 2^W, then p >= 1.
;;
;; Some examples
;;
;;   d      M          s = p - W   overflow
;; ------------------------------------
;;   3   #xaaaaaaab       1         NIL
;;   5   #xcccccccd       2         NIL
;;   7   #x24924925       3          T
;; 100   #x51eb851f       5         NIL
;;
;; where the overflow column indicates if we m is too large to fit in
;; a word

;; Compute M, s, and overflow as indicated above.      
(defun find-unsigned-reciprocal (divisor &optional (word-width vm:word-bits))
  (let ((nc (1- (* divisor (floor (ash 1 word-width) divisor)))))
    ;; Find p
    (do ((p word-width (1+ p)))
        ((> (ash 1 p)
            (* nc (- divisor 1 (rem (1- (ash 1 p)) divisor))))
         (let* ((m (/ (- (+ (ash 1 p) divisor)
                         (rem (ash 1 p) divisor))
                      divisor))
                (overflowp (>= m (ash 1 word-width))))
           (values (if overflowp
                       (- m (ash 1 word-width))
                       m)
                   (- p word-width)
                   overflowp))))))

