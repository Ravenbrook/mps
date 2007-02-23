;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/array.lisp,v 1.33 2006/06/30 18:41:32 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the SPARC definitions for array operations.
;;;
;;; Written by William Lott
;;; Signed-array support by Douglas Crosher 1997.
;;; Complex-float and long-float support by Douglas Crosher 1998.
;;;
(in-package "SPARC")


;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg))
	 (rank :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) gc-temp)	; gencgc
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (pseudo-atomic ()
      (inst add ndescr rank (* (1+ array-dimensions-offset) vm:word-bytes))
      (inst andn ndescr 4)
      (allocation header ndescr other-pointer-type :temp-tn gc-temp)
      (inst add ndescr rank (fixnumize (1- vm:array-dimensions-offset)))
      (inst slln ndescr ndescr vm:type-bits)
      (inst or ndescr ndescr type)
      ;; Remove the extraneous fixnum tag bits because TYPE and RANK
      ;; were fixnums
      (inst srln ndescr ndescr fixnum-tag-bits)
      (storew ndescr header 0 vm:other-pointer-type))
    (move result header)))


;;;; Additional accessors and setters for the array header.

(defknown lisp::%array-dimension (t fixnum) fixnum
  (flushable))
(defknown lisp::%set-array-dimension (t fixnum fixnum) fixnum
  ())

(define-vop (%array-dimension word-index-ref)
  (:translate lisp::%array-dimension)
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))

(define-vop (%set-array-dimension word-index-set)
  (:translate lisp::%set-array-dimension)
  (:policy :fast-safe)
  (:variant vm:array-dimensions-offset vm:other-pointer-type))



(defknown lisp::%array-rank (t) fixnum (flushable))

(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 vm:other-pointer-type)
    (inst sra temp vm:type-bits)
    (inst sub temp (1- vm:array-dimensions-offset))
    (inst slln res temp fixnum-tag-bits)))



;;;; Bounds checking routine.


(define-vop (check-bound)
  (:translate %check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
	 (bound :scs (any-reg descriptor-reg))
	 (index :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (any-reg descriptor-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 5
    (let ((error (generate-error-code vop invalid-array-index-error
				      array bound index)))
      (inst cmp index bound)
      (inst b :geu error)
      (inst nop)
      (move result index))))



;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whose
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.

(defmacro def-data-vector-frobs (type variant element-type &rest scs)
  `(progn
     (define-vop (,(intern (concatenate 'simple-string
					"DATA-VECTOR-REF/"
					(string type)))
		  ,(intern (concatenate 'simple-string
					(string variant)
					"-REF")))
       (:note "inline array access")
       (:variant vm:vector-data-offset vm:other-pointer-type)
       (:translate data-vector-ref)
       (:arg-types ,type positive-fixnum)
       (:results (value :scs ,scs))
       (:result-types ,element-type))
     (define-vop (,(intern (concatenate 'simple-string
					"DATA-VECTOR-SET/"
					(string type)))
		  ,(intern (concatenate 'simple-string
					(string variant)
					"-SET")))
       (:note "inline array store")
       (:variant vm:vector-data-offset vm:other-pointer-type)
       (:translate data-vector-set)
       (:arg-types ,type positive-fixnum ,element-type)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg zero immediate))
	      (value :scs ,scs))
       (:results (result :scs ,scs))
       (:result-types ,element-type))))

(def-data-vector-frobs simple-string byte-index
  base-char base-char-reg)
(def-data-vector-frobs simple-vector word-index
  * descriptor-reg any-reg)

(def-data-vector-frobs simple-array-unsigned-byte-8 byte-index
  positive-fixnum unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-16 halfword-index
  positive-fixnum unsigned-reg)
(def-data-vector-frobs simple-array-unsigned-byte-32 word-index
  unsigned-num unsigned-reg)

(def-data-vector-frobs simple-array-signed-byte-30 word-index
  tagged-num any-reg)
(def-data-vector-frobs simple-array-signed-byte-32 word-index
  signed-num signed-reg)

;;; Integer vectors whos elements are smaller than a byte.  I.e. bit, 2-bit,
;;; and 4-bit vectors.
;;;
;;; The elements are stored in the word with the first element in the
;;; most-significant parts of the word.
;;; 

(eval-when (compile eval)

(defmacro def-small-data-vector-frobs (type bits)
  (let* ((elements-per-word (floor vm:word-bits bits))
	 (bit-shift (1- (integer-length elements-per-word))))
    `(progn
       (define-vop (,(symbolicate 'data-vector-ref/ type))
	 (:note "inline array access")
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (value :scs (any-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
	 (:generator 20
	   ;; temp = floor(index bit-shift), to get address of word
	   ;; containing our bits.
	   (inst srln temp index ,bit-shift)
	   (inst slln temp fixnum-tag-bits)
	   (inst add temp (- (* vm:vector-data-offset vm:word-bytes)
			     vm:other-pointer-type))
	   (inst ldn result object temp)
	   ;; temp = mod(index, bit-shift) to figure out what part of
	   ;; word we want.  (XOR is a quick way of computing
	   ;; elements-per-word minus temp.)
	   (inst and temp index ,(1- elements-per-word))
	   (inst xor temp ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst slln temp ,(1- (integer-length bits)))))
	   (inst srln result temp)
	   (inst and result ,(1- (ash 1 bits)))
	   (inst slln value result vm:fixnum-tag-bits)))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type (:constant index))
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp)
	 (:generator 15
	   (multiple-value-bind (word extra)
	       (floor index ,elements-per-word)
	     ;; Compute how many "units" we need to right shift to get
	     ;; the bits we want.
	     (setf extra (- ,(1- elements-per-word) extra))
	     (let ((offset (- (* (+ word vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type)))
	       (cond ((typep offset '(signed-byte 13))
		      (inst ldn result object offset))
		     (t
		      (inst li temp offset)
		      (inst ldn result object temp))))
	     (unless (zerop extra)
	       (inst srln result (* ,bits extra)))
	     ;; Always need the mask unless the bits we wanted were the
	     ;; most significant bits of the word.
	     (unless (= extra ,(1- elements-per-word))
	       (inst and result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note "inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg) :target shift)
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp old offset)
	 (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) shift)
	 (:generator 25
	   (inst srln offset index ,bit-shift)
	   (inst slln offset fixnum-tag-bits)
	   (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			       vm:other-pointer-type))
	   (inst ldn old object offset)
	   (inst and shift index ,(1- elements-per-word))
	   (inst xor shift ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst slln shift ,(1- (integer-length bits)))))
	   (unless (and (sc-is value immediate)
			(= (tn-value value) ,(1- (ash 1 bits))))
	     (inst li temp ,(1- (ash 1 bits)))
	     (inst slln temp shift)
	     (inst not temp)
	     (inst and old temp))
	   (unless (sc-is value zero)
	     (sc-case value
	       (immediate
		(inst li temp (logand (tn-value value) ,(1- (ash 1 bits)))))
	       (unsigned-reg
		(inst and temp value ,(1- (ash 1 bits)))))
	     (inst slln temp shift)
	     (inst or old temp))
	   (inst stn old object offset)
	   (sc-case value
	     (immediate
	      (inst li result (tn-value value)))
	     (t
	      (move result value)))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg zero immediate) :target result))
	 (:arg-types ,type
		     (:constant index)
		     positive-fixnum)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) offset-reg temp old)
	 (:generator 20
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (let ((offset (- (* (+ word vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type)))
	       (cond ((typep offset '(signed-byte 13))
		      (inst ldn old object offset))
		     (t
		      (inst li offset-reg offset)
		      (inst ldn old object offset-reg)))
	       (unless (and (sc-is value immediate)
			    (= (tn-value value) ,(1- (ash 1 bits))))
		 (cond ((zerop extra)
			(inst slln old ,bits)
			(inst srln old ,bits))
		       (t
			(inst li temp
			      (lognot (ash ,(1- (ash 1 bits))
					   (* (logxor extra
						      ,(1- elements-per-word))
					      ,bits))))
			(inst and old temp))))
	       (sc-case value
		 (zero)
		 (immediate
		  (let ((value (ash (logand (tn-value value)
					    ,(1- (ash 1 bits)))
				    (* (logxor extra
					       ,(1- elements-per-word))
				       ,bits))))
		    (cond ((typep value '(signed-byte 13))
			   (inst or old value))
			  (t
			   (inst li temp value)
			   (inst or old temp)))))
		 (unsigned-reg
		  (inst slln temp value
			(* (logxor extra ,(1- elements-per-word)) ,bits))
		  (inst or old temp)))
	       (if (typep offset '(signed-byte 13))
		   (inst stn old object offset)
		   (inst stn old object offset-reg)))
	     (sc-case value
	       (immediate
		(inst li result (tn-value value)))
	       (t
		(move result value)))))))))

); eval-when (compile eval)

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)


;;; And the float variants.
;;; 

(define-vop (data-vector-ref/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:result-types single-float)
  (:generator 5
    (inst add offset index (- (* vm:vector-data-offset vm:word-bytes)
			      vm:other-pointer-type))
    (inst ldf value object offset)))

(define-vop (data-vector-ref-c/simple-array-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-array-single-float (:constant index))
  (:info index)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:result-types single-float)
  (:generator 3
    (let ((offset (+ (fixnumize index)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      (if (typep offset '(signed-byte 13))
	  (inst ldf value object offset)
	  (progn
	    (inst li temp offset)
	    (inst ldf value object temp))))))


(define-vop (data-vector-set/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst add offset index
	  (- (* vm:vector-data-offset vm:word-bytes)
	     vm:other-pointer-type))
    (inst stf value object offset)
    (unless (location= result value)
      (inst fmovs result value))))

(define-vop (data-vector-set-c/simple-array-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (single-reg) :target result))
  (:arg-types simple-array-single-float
	      (:constant index)
	      single-float)
  (:info index)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (let ((offset (+ (fixnumize index)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      (if (typep offset '(signed-byte 13))
	  (inst stf value object offset)
	  (progn
	    (inst li temp offset)
	    (inst stf value object temp)))
      (unless (location= result value)
	(inst fmovs result value)))))

(define-vop (data-vector-ref/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 7
    (inst slln offset index 1)
    (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))
    (inst lddf value object offset)))

(define-vop (data-vector-ref-c/simple-array-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types simple-array-double-float (:constant index))
  (:info index)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 3
    (let ((offset (+ (* index 8)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      (if (typep offset '(signed-byte 13))
	  (inst lddf value object offset)
	  (progn
	    (inst li temp offset)
	    (inst lddf value object temp))))))

(define-vop (data-vector-set/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 20
    (inst slln offset index 1)
    (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))
    (inst stdf value object offset)
    (unless (location= result value)
      (move-double-reg result value))))

(define-vop (data-vector-set-c/simple-array-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (value :scs (double-reg) :target result))
  (:arg-types simple-array-double-float
	      (:constant index)
	      double-float)
  (:info index)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 10
    (let ((offset (+ (* index 8)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))))
      (if (typep offset '(signed-byte 13))
	  (inst stdf value object offset)
	  (progn
	    (inst li temp offset)
	    (inst stdf value object temp)))
      (unless (location= result value)
	(move-double-reg result value)))))

#+long-float
(define-vop (data-vector-ref/simple-array-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-long-float positive-fixnum)
  (:results (value :scs (long-reg)))
  (:result-types long-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 7
    (inst slln offset index 2)
    (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))
    (load-long-reg value object offset nil)))

#+long-float
(define-vop (data-vector-set/simple-array-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (long-reg) :target result))
  (:arg-types simple-array-long-float positive-fixnum long-float)
  (:results (result :scs (long-reg)))
  (:result-types long-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 20
    (inst slln offset index 2)
    (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type))
    (store-long-reg value object offset nil)
    (unless (location= result value)
      (move-long-reg result value))))


;;;; Misc. Array VOPs.


#+nil
(define-vop (vector-word-length)
  (:args (vec :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw res vec clc::g-vector-header-words)
    (inst niuo res res clc::g-vector-words-mask-16)))

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))


;;;
(define-vop (data-vector-ref/simple-array-signed-byte-8 signed-byte-index-ref)
  (:note "inline array access")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-8 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-8 byte-index-set)
  (:note "inline array store")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-8 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg zero immediate))
	 (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))


(define-vop (data-vector-ref/simple-array-signed-byte-16
	     signed-halfword-index-ref)
  (:note "inline array access")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-ref)
  (:arg-types simple-array-signed-byte-16 positive-fixnum)
  (:results (value :scs (signed-reg)))
  (:result-types tagged-num))

(define-vop (data-vector-set/simple-array-signed-byte-16 halfword-index-set)
  (:note "inline array store")
  (:variant vm:vector-data-offset vm:other-pointer-type)
  (:translate data-vector-set)
  (:arg-types simple-array-signed-byte-16 positive-fixnum tagged-num)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg zero immediate))
	 (value :scs (signed-reg)))
  (:results (result :scs (signed-reg)))
  (:result-types tagged-num))


;;; Complex float arrays.

(define-vop (data-vector-ref/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-single-float positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (inst slln offset index 1)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (inst ldf real-tn object offset))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (inst add offset vm:word-bytes)
      (inst ldf imag-tn object offset))))

(define-vop (data-vector-ref-c/simple-array-complex-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-single-float
	      (:constant index))
  (:info index)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:result-types complex-single-float)
  (:generator 3
    (let ((offset (+ (* index 8)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type)))
	  (real-tn (complex-single-reg-real-tn value))
	  (imag-tn (complex-single-reg-imag-tn value)))
      (cond ((typep (+ offset 4) '(signed-byte 13))
	     (inst ldf real-tn object offset)
	     (inst ldf imag-tn object (+ offset 4)))
	    (t
	     (inst li temp offset)
	     (inst ldf real-tn object temp)
	     (inst add temp 4)
	     (inst ldf imag-tn object temp))))))

(define-vop (data-vector-set/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float positive-fixnum
	      complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result)))
      (inst slln offset index 1)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (inst stf value-real object offset)
      (unless (location= result-real value-real)
	(inst fmovs result-real value-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (inst add offset vm:word-bytes)
      (inst stf value-imag object offset)
      (unless (location= result-imag value-imag)
	(inst fmovs result-imag value-imag)))))

(define-vop (data-vector-set-c/simple-array-complex-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (complex-single-reg) :target result))
  (:arg-types simple-array-complex-single-float
	      (:constant index)
	      complex-single-float)
  (:info index)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 3
    (let ((offset (+ (* index 8)
		     (- (* vm:vector-data-offset vm:word-bytes)
			vm:other-pointer-type)))
	  (value-real (complex-single-reg-real-tn value))
	  (result-real (complex-single-reg-real-tn result))
	  (value-imag (complex-single-reg-imag-tn value))
	  (result-imag (complex-single-reg-imag-tn result)))
      (cond ((typep (+ offset 4) '(signed-byte 13))
	     (inst stf value-real object offset)
	     (inst stf value-imag object (+ offset 4)))
	    (t
	     (inst li temp offset)
	     (inst stf value-real object temp)
	     (inst add temp 4)
	     (inst stf value-imag object temp)))
      (unless (location= result-real value-real)
	(inst fmovs result-real value-real))
      (unless (location= result-imag value-imag)
	(inst fmovs result-imag value-imag)))))

(define-vop (data-vector-ref/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-float positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (inst slln offset index 2)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (inst lddf real-tn object offset))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (inst add offset (* 2 vm:word-bytes))
      (inst lddf imag-tn object offset))))

(define-vop (data-vector-ref-c/simple-array-complex-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-double-float (:constant index))
  (:info index)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 5
    (let ((offset (+ (* index 16)
		     (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type)))
	  (real-tn (complex-double-reg-real-tn value))
	  (imag-tn (complex-double-reg-imag-tn value)))
      (cond ((typep (+ offset 8) '(signed-byte 13))
	     (inst lddf real-tn object offset)
	     (inst lddf imag-tn object (+ offset 8)))
	    (t
	     (inst li temp offset)
	     (inst lddf real-tn object temp)
	     (inst add temp (* 2 vm:word-bytes))
	     (inst lddf imag-tn object temp))))))

(define-vop (data-vector-set/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float positive-fixnum
	      complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result)))
      (inst slln offset index 2)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (inst stdf value-real object offset)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result)))
      (inst add offset (* 2 vm:word-bytes))
      (inst stdf value-imag object offset)
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))))

(define-vop (data-vector-set-c/simple-array-complex-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (complex-double-reg) :target result))
  (:arg-types simple-array-complex-double-float
	      (:constant index)
	      complex-double-float)
  (:info index)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 15
    (let ((value-real (complex-double-reg-real-tn value))
	  (result-real (complex-double-reg-real-tn result))
	  (value-imag (complex-double-reg-imag-tn value))
	  (result-imag (complex-double-reg-imag-tn result))
	  (offset (+ (* index 16)
		     (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))))
      ;; There's a possible optimization here if the offset for the
      ;; real part fits in a signed-byte 13 but the imag part doesn't.
      ;; We don't do this because it can't happen with the current
      ;; values of vm:other-pointer-type and vm:vector-data-offset.
      (cond ((typep (+ offset 8) '(signed-byte 13))
	     (inst stdf value-real object offset)
	     (inst stdf value-imag object (+ offset 8)))
	    (t
	     (inst li temp offset)
	     (inst stdf value-real object temp)
	     (inst add temp 8)
	     (inst stdf value-imag object temp)))
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real))
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))))

#+long-float
(define-vop (data-vector-ref/simple-array-complex-long-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-long-float positive-fixnum)
  (:results (value :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (let ((real-tn (complex-long-reg-real-tn value)))
      (inst slln offset index 3)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (load-long-reg real-tn object offset nil))
    (let ((imag-tn (complex-long-reg-imag-tn value)))
      (inst add offset (* 4 vm:word-bytes))
      (load-long-reg imag-tn object offset nil))))

#+long-float
(define-vop (data-vector-set/simple-array-complex-long-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-long-reg) :target result))
  (:arg-types simple-array-complex-long-float positive-fixnum
	      complex-long-float)
  (:results (result :scs (complex-long-reg)))
  (:result-types complex-long-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (let ((value-real (complex-long-reg-real-tn value))
	  (result-real (complex-long-reg-real-tn result)))
      (inst slln offset index 3)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (store-long-reg value-real object offset nil)
      (unless (location= result-real value-real)
	(move-long-reg result-real value-real)))
    (let ((value-imag (complex-long-reg-imag-tn value))
	  (result-imag (complex-long-reg-imag-tn result)))
      (inst add offset (* 4 vm:word-bytes))
      (store-long-reg value-imag object offset nil)
      (unless (location= result-imag value-imag)
	(move-long-reg result-imag value-imag)))))


;;; These VOPs are used for implementing float slots in structures (whose raw
;;; data is an unsigned-32 vector.
;;;
(define-vop (raw-ref-single data-vector-ref/simple-array-single-float)
  (:translate %raw-ref-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-single data-vector-set/simple-array-single-float)
  (:translate %raw-set-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum single-float))
;;;
(define-vop (raw-ref-double data-vector-ref/simple-array-double-float)
  (:translate %raw-ref-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-double data-vector-set/simple-array-double-float)
  (:translate %raw-set-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum double-float))
;;;
#+long-float
(define-vop (raw-ref-long data-vector-ref/simple-array-long-float)
  (:translate %raw-ref-long)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
#+long-float
(define-vop (raw-set-double data-vector-set/simple-array-long-float)
  (:translate %raw-set-long)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum long-float))

(define-vop (raw-ref-complex-single
	     data-vector-ref/simple-array-complex-single-float)
  (:translate %raw-ref-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-complex-single
	     data-vector-set/simple-array-complex-single-float)
  (:translate %raw-set-complex-single)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-single-float))
;;;
(define-vop (raw-ref-complex-double
	     data-vector-ref/simple-array-complex-double-float)
  (:translate %raw-ref-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
(define-vop (raw-set-complex-double
	     data-vector-set/simple-array-complex-double-float)
  (:translate %raw-set-complex-double)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-double-float))
;;;
#+long-float
(define-vop (raw-ref-complex-long
	     data-vector-ref/simple-array-complex-long-float)
  (:translate %raw-ref-complex-long)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum))
;;;
#+long-float
(define-vop (raw-set-complex-long
	     data-vector-set/simple-array-complex-long-float)
  (:translate %raw-set-complex-long)
  (:arg-types simple-array-unsigned-byte-32 positive-fixnum
	      complex-long-float))


;;; These vops are useful for accessing the bits of a vector irrespective of
;;; what type of vector it is.
;;; 

(define-vop (raw-bits word-index-ref)
  (:note "raw-bits VOP")
  (:translate %raw-bits)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 0 vm:other-pointer-type))

(define-vop (set-raw-bits word-index-set)
  (:note "setf raw-bits VOP")
  (:translate %set-raw-bits)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg zero immediate))
	 (value :scs (unsigned-reg)))
  (:arg-types * tagged-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 0 vm:other-pointer-type))


#+double-double
(progn
(define-vop (data-vector-ref/simple-array-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-double-float positive-fixnum)
  (:results (value :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (let ((hi-tn (double-double-reg-hi-tn value)))
      (inst slln offset index 2)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (inst lddf hi-tn object offset))
    (let ((lo-tn (double-double-reg-lo-tn value)))
      (inst add offset (* 2 vm:word-bytes))
      (inst lddf lo-tn object offset))))

(define-vop (data-vector-ref-c/simple-array-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 5
    (let ((offset (+ (* index 16)
		     (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type)))
	  (hi-tn (double-double-reg-hi-tn value))
	  (lo-tn (double-double-reg-lo-tn value)))
      (cond ((typep (+ offset 8) '(signed-byte 13))
	     (inst lddf hi-tn object offset)
	     (inst lddf lo-tn object (+ offset 8)))
	    (t
	     (inst li temp offset)
	     (inst lddf hi-tn object temp)
	     (inst add temp (* 2 vm:word-bytes))
	     (inst lddf lo-tn object temp))))))

(define-vop (data-vector-set/simple-array-double-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (double-double-reg) :target result))
  (:arg-types simple-array-double-double-float positive-fixnum
	      double-double-float)
  (:results (result :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (let ((value-hi (double-double-reg-hi-tn value))
	  (result-hi (double-double-reg-hi-tn result)))
      (inst slln offset index 2)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (inst stdf value-hi object offset)
      (unless (location= result-hi value-hi)
	(move-double-reg result-hi value-hi)))
    (let ((value-lo (double-double-reg-lo-tn value))
	  (result-lo (double-double-reg-lo-tn result)))
      (inst add offset (* 2 vm:word-bytes))
      (inst stdf value-lo object offset)
      (unless (location= result-lo value-lo)
	(move-double-reg result-lo value-lo)))))

(define-vop (data-vector-set-c/simple-array-double-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (double-double-reg) :target result))
  (:arg-types simple-array-double-double-float
	      (:constant index)
	      double-double-float)
  (:info index)
  (:results (result :scs (double-double-reg)))
  (:result-types double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 15
    (let ((value-hi (double-double-reg-hi-tn value))
	  (result-hi (double-double-reg-hi-tn result))
	  (value-lo (double-double-reg-lo-tn value))
	  (result-lo (double-double-reg-lo-tn result))
	  (offset (+ (* index 16)
		     (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))))
      ;; There's a possible optimization here if the offset for the
      ;; real part fits in a signed-byte 13 but the imag part doesn't.
      ;; We don't do this because it can't happen with the current
      ;; values of vm:other-pointer-type and vm:vector-data-offset.
      (cond ((typep (+ offset 8) '(signed-byte 13))
	     (inst stdf value-hi object offset)
	     (inst stdf value-lo object (+ offset 8)))
	    (t
	     (inst li temp offset)
	     (inst stdf value-hi object temp)
	     (inst add temp 8)
	     (inst stdf value-lo object temp)))
      (unless (location= result-hi value-hi)
	(move-double-reg result-hi value-hi))
      (unless (location= result-lo value-lo)
	(move-double-reg result-lo value-lo)))))

(define-vop (data-vector-ref/simple-array-complex-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg)))
  (:arg-types simple-array-complex-double-double-float positive-fixnum)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 7
    (let ((real-tn (complex-double-double-reg-real-hi-tn value)))
      (inst slln offset index 3)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (inst lddf real-tn object offset))
    (let ((real-tn (complex-double-double-reg-real-lo-tn value)))
      (inst add offset (* 2 vm:word-bytes))
      (inst lddf real-tn object offset))
    (let ((imag-tn (complex-double-double-reg-imag-hi-tn value)))
      (inst add offset (* 2 vm:word-bytes))
      (inst lddf imag-tn object offset))
    (let ((imag-tn (complex-double-double-reg-imag-lo-tn value)))
      (inst add offset (* 2 vm:word-bytes))
      (inst lddf imag-tn object offset))))

(define-vop (data-vector-ref-c/simple-array-complex-double-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-complex-double-double-float (:constant index))
  (:info index)
  (:results (value :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 5
    (let ((offset (+ (* index 16)
		     (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type)))
	  (real-hi-tn (complex-double-double-reg-real-hi-tn value))
	  (imag-hi-tn (complex-double-double-reg-imag-hi-tn value))
	  (real-lo-tn (complex-double-double-reg-real-lo-tn value))
	  (imag-lo-tn (complex-double-double-reg-imag-lo-tn value)))
      (cond ((typep (+ offset (* 6 vm:word-bytes)) '(signed-byte 13))
	     (inst lddf real-hi-tn object offset)
	     (inst lddf real-lo-tn object (+ offset (* 2 vm:word-bytes)))
	     (inst lddf imag-hi-tn object (+ offset (* 4 vm:word-bytes)))
	     (inst lddf imag-lo-tn object (+ offset (* 6 vm:word-bytes))))
	    (t
	     (inst li temp offset)
	     (inst lddf real-hi-tn object temp)
	     (inst add temp (* 2 vm:word-bytes))
	     (inst lddf real-lo-tn object temp)
	     (inst add temp (* 2 vm:word-bytes))
	     (inst lddf imag-hi-tn object temp)
	     (inst add temp (* 2 vm:word-bytes))
	     (inst lddf imag-lo-tn object temp))))))

(define-vop (data-vector-set/simple-array-complex-double-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg))
	 (value :scs (complex-double-double-reg) :target result))
  (:arg-types simple-array-complex-double-double-float positive-fixnum
	      complex-double-double-float)
  (:results (result :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) offset)
  (:generator 20
    (let ((value-real (complex-double-double-reg-real-hi-tn value))
	  (result-real (complex-double-double-reg-real-hi-tn result)))
      (inst slln offset index 3)
      (inst add offset (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))
      (inst stdf value-real object offset)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real)))
    (let ((value-real (complex-double-double-reg-real-lo-tn value))
	  (result-real (complex-double-double-reg-real-lo-tn result)))
      (inst add offset (* 2 vm:word-bytes))
      (inst stdf value-real object offset)
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real)))
    (let ((value-imag (complex-double-double-reg-imag-hi-tn value))
	  (result-imag (complex-double-double-reg-imag-hi-tn result)))
      (inst add offset (* 2 vm:word-bytes))
      (inst stdf value-imag object offset)
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))
    (let ((value-imag (complex-double-double-reg-imag-lo-tn value))
	  (result-imag (complex-double-double-reg-imag-lo-tn result)))
      (inst add offset (* 2 vm:word-bytes))
      (inst stdf value-imag object offset)
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))))

(define-vop (data-vector-set-c/simple-array-complex-double-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (complex-double-double-reg) :target result))
  (:arg-types simple-array-complex-double-double-float
	      (:constant index)
	      complex-double-double-float)
  (:info index)
  (:results (result :scs (complex-double-double-reg)))
  (:result-types complex-double-double-float)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) temp)
  (:generator 15
    (let ((value-real (complex-double-double-reg-real-hi-tn value))
	  (result-real (complex-double-double-reg-real-hi-tn result))
	  (value-imag (complex-double-double-reg-imag-hi-tn value))
	  (result-imag (complex-double-double-reg-imag-hi-tn result))
	  (offset (+ (* index 16)
		     (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))))
      ;; There's a possible optimization here if the offset for the
      ;; real part fits in a signed-byte 13 but the imag part doesn't.
      ;; We don't do this because it can't happen with the current
      ;; values of vm:other-pointer-type and vm:vector-data-offset.
      (cond ((typep (+ offset 8) '(signed-byte 13))
	     (inst stdf value-real object offset)
	     (inst stdf value-imag object (+ offset 8)))
	    (t
	     (inst li temp offset)
	     (inst stdf value-real object temp)
	     (inst add temp 8)
	     (inst stdf value-imag object temp)))
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real))
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))
    (let ((value-real (complex-double-double-reg-real-lo-tn value))
	  (result-real (complex-double-double-reg-real-lo-tn result))
	  (value-imag (complex-double-double-reg-imag-lo-tn value))
	  (result-imag (complex-double-double-reg-imag-lo-tn result))
	  (offset (+ (* index 16)
		     (- (* vm:vector-data-offset vm:word-bytes)
			  vm:other-pointer-type))))
      ;; There's a possible optimization here if the offset for the
      ;; real part fits in a signed-byte 13 but the imag part doesn't.
      ;; We don't do this because it can't happen with the current
      ;; values of vm:other-pointer-type and vm:vector-data-offset.
      (cond ((typep (+ offset (* 6 vm:word-bytes)) '(signed-byte 13))
	     (inst stdf value-real object (+ offset (* 4 vm:word-bytes)))
	     (inst stdf value-imag object (+ offset (* 6 vm:word-bytes))))
	    (t
	     (inst add temp 8)
	     (inst stdf value-real object temp)
	     (inst add temp 8)
	     (inst stdf value-imag object temp)))
      (unless (location= result-real value-real)
	(move-double-reg result-real value-real))
      (unless (location= result-imag value-imag)
	(move-double-reg result-imag value-imag)))))


)
