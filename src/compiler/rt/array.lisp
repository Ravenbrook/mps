;;; -*- Package: RT; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/array.lisp,v 1.12 2003/08/03 11:27:47 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/array.lisp,v 1.12 2003/08/03 11:27:47 gerd Exp $
;;;
;;; This file contains the IBM RT definitions for array operations.
;;;
;;; Written by William Lott and Bill Chiles.
;;;

(in-package "RT")



;;;; Allocator for the array header.

(define-vop (make-array-header)
  (:translate make-array-header)
  (:policy :fast-safe)
  (:args (type :scs (any-reg descriptor-reg))
	 (rank :scs (any-reg descriptor-reg)))
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result) header)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:temporary (:sc word-pointer-reg) alloc)
  (:results (result :scs (descriptor-reg)))
  (:generator 0
    (pseudo-atomic (ndescr)
      ;; Take free pointer and make descriptor pointer for the result.  Just
      ;; add in the three low-tag bits since alloc ptr is dual-word aligned,
      ;; and its three low bits are zero.
      (load-symbol-value alloc *allocation-pointer*)
      (inst cal header alloc vm:other-pointer-type)
      (inst cal alloc alloc
	    (+ (* vm:array-dimensions-offset vm:word-bytes)
	       vm:lowtag-mask))
      ;; Rank is the dimensions as a fixnum which happens to be the number of
      ;; bytes (by the machine's interpretation) we need to add to the alloc
      ;; ptr.
      (inst cas alloc rank alloc)
      (inst nilo alloc (logand #xFFFF (lognot vm:lowtag-mask)))
      (store-symbol-value alloc *allocation-pointer*)
      (inst cal ndescr rank (fixnumize (1- vm:array-dimensions-offset)))
      ;; Shift the fixnum representation of the length, then OR in the fixnum
      ;; rep of the type code, and then shift off the two extra fixnum zeros.
      (inst sl ndescr vm:type-bits)
      (inst o ndescr type)
      (inst sr ndescr 2)
      (storew ndescr header 0 vm:other-pointer-type))
    (load-symbol-value ndescr *internal-gc-trigger*)
    (inst tlt ndescr alloc)
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

;;; ARRAY-RANK-VOP -- VOP.
;;;
;;; The length of the array header manifests the rank of the array.  We fetch
;;; the header word, extract the length, and subtract off the start of the
;;; dimension slots.
;;;
(define-vop (array-rank-vop)
  (:translate lisp::%array-rank)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random :target res) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw temp x 0 vm:other-pointer-type)
    (inst sr temp vm:type-bits)
    (inst s temp (1- vm:array-dimensions-offset))
    (inst sl temp 2)
    (move res temp)))



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
      (inst c index bound)
      (inst bnc :lt error)
      (move result index))))



;;;; 32-bit, 16-bit, and 8-bit vectors.

;;; We build these variants on top of VOP's defined in memory.lisp.  These
;;; vectors' elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.

(eval-when (compile eval)
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
	      (index :scs (any-reg immediate))
	      (value :scs ,scs))
       (:results (result :scs ,scs))
       (:result-types ,element-type))))
) ;EVAL-WHEN

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



;;;; Integer vectors with 1, 2, and 4 bit elements.

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
	 (:temporary (:scs (interior-reg)) lip)
	 (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp result)
	 (:generator 20
	   (move temp index)
	   (inst sr temp ,bit-shift)
	   (inst sl temp 2)
	   (move lip object)
	   (inst a lip temp)
	   (loadw result lip vm:vector-data-offset vm:other-pointer-type)
	   (inst nilz temp index ,(1- elements-per-word))
	   (inst xil temp ,(1- elements-per-word))
	   ,@(unless (= bits 1)
	       `((inst sl temp ,(1- (integer-length bits)))))
	   (inst sr result temp)
	   (inst nilz result ,(1- (ash 1 bits)))
	   (move value result)
	   (inst sl value 2)))
       (define-vop (,(symbolicate 'data-vector-ref-c/ type))
	 (:translate data-vector-ref)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:arg-types ,type (:constant index))
	 (:temporary (:scs (interior-reg)) lip)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:generator 15
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (setf extra (logxor extra (1- ,elements-per-word)))
	     (let ((offset (- (* (+ word vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type)))
	       (cond ((typep offset '(signed-byte 16))
		      ;; Use the load with immediate offset if possible.
		      (inst l result offset))
		     (t
		      ;; Load the upper half of the offset in one instruction
		      ;; and add the lower half as an immediate offset in
		      ;; the load.  NOTE: if bit 15 is on, the load will sign-
		      ;; extend it, so we have to add 1 to the upper half now
		      ;; to counter the effect of the -1 from the sign-extension.
		      (inst cau lip object
			    (if (logbitp 15 offset)
				(1+ (ash offset -16))
				(ash offset -16)))
		      (inst l result lip (logand #xFFFF offset)))))
	     (unless (zerop extra)
	       (inst sr result (* extra ,bits)))
	     (unless (= extra ,(1- elements-per-word))
	       (inst n result ,(1- (ash 1 bits)))))))
       (define-vop (,(symbolicate 'data-vector-set/ type))
	 (:note "inline array store")
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:vop-var vop)
	 (:args (object :scs (descriptor-reg) :to (:eval 0))
		;; Normally, we would take the next arg in an unsigned-reg too.
		;; This caused a discrimination problem in the compiler for
		;; choosing a move function.  Since this VOP uses so many
		;; non-descriptor registers anyway which has been a hassle, we
		;; just removed unsigned-reg.
		(index :scs (any-reg) :to (:eval 1))
		(value :scs (unsigned-reg immediate)
		       :load-if (not (sc-is value unsigned-stack))
		       :target value-save))
	 (:arg-types ,type positive-fixnum positive-fixnum)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (unsigned-stack) :from (:argument 2)) value-save)
	 (:temporary (:scs (interior-reg)) lip)
	 (:temporary (:scs (non-descriptor-reg) :from (:argument 2)
			   :to (:result 0)) temp)
	 (:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:result 0))
		     old)
	 (:temporary (:scs (non-descriptor-reg) :from (:eval 1) :to (:result 0))
		     shift)
	 (:generator 25
	   (when (sc-is value unsigned-reg)
	     (storew value (current-nfp-tn vop) (tn-offset value-save)))
	   (move temp index)
	   (inst sr temp
		 (sc-case index
		   ;; In addition to dividing the index to map it to
		   ;; a number of words to skip, shift off the fixnum tag
		   ;; bits too.
		   (any-reg ,(+ bit-shift 2))
		   ;; No extraneous bits in this case.
		   (unsigned-reg ,bit-shift)))
	   (inst sl temp 2)
	   (move lip object)
	   (inst a lip temp)
	   (loadw old lip vm:vector-data-offset vm:other-pointer-type)
	   (sc-case index
	     (unsigned-reg
	      (inst nilz shift index ,(1- elements-per-word))
	      ,@(unless (= bits 1)
		  `((inst sl shift ,(1- (integer-length bits))))))
	     (any-reg
	      (inst nilz shift index ,(ash (1- elements-per-word) 2))
	      ,@(unless (= bits 4)
		  `((inst sr shift ,(- 3 (integer-length bits)))))))
	   (inst xil shift ,(1- elements-per-word))
	      
	   ;; Unless our immediate is all 1's, zero the destination bits.
	   (unless (and (sc-is value immediate)
			(= (tn-value value) ,(1- (ash 1 bits))))
	     (inst li temp ,(1- (ash 1 bits)))
	     (inst sl temp shift)
	     (inst not temp)
	     (inst n old temp))
	   ;; Unless the value is zero, really deposit it.
	   (unless (and (sc-is value immediate)
			(zerop (tn-value value)))
	     (sc-case value
	       (immediate
		(inst li temp (logand (tn-value value) ,(1- (ash 1 bits)))))
	       ((unsigned-reg unsigned-stack)
		(loadw temp (current-nfp-tn vop)
		       (tn-offset (if (sc-is value unsigned-stack)
				      value
				      value-save)))
		(inst nilz temp ,(1- (ash 1 bits)))))
	     (inst sl temp shift)
	     (inst o old temp))
	   (storew old lip vm:vector-data-offset vm:other-pointer-type)
	   (sc-case value
	     (immediate
	      (inst li result (tn-value value)))
	     ((unsigned-reg unsigned-stack)
	      (loadw result (current-nfp-tn vop)
		     (tn-offset (if (sc-is value unsigned-stack)
				    value
				    value-save)))))))
       (define-vop (,(symbolicate 'data-vector-set-c/ type))
	 (:translate data-vector-set)
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(value :scs (unsigned-reg immediate) :target result))
	 (:arg-types ,type (:constant index) positive-fixnum)
	 (:temporary (:scs (interior-reg)) lip)
	 (:info index)
	 (:results (result :scs (unsigned-reg)))
	 (:result-types positive-fixnum)
	 (:temporary (:scs (non-descriptor-reg)) temp old)
	 (:generator 20
	   (multiple-value-bind (word extra) (floor index ,elements-per-word)
	     (setf extra (logxor extra (1- ,elements-per-word)))
	     (let ((offset (- (* (+ word vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type)))
	       (cond ((typep offset '(signed-byte 16))
		      ;; Use the load with immediate offset if possible.
		      (inst l old offset))
		     (t
		      ;; Load the upper half of the offset in one instruction
		      ;; and add the lower half as an immediate offset in
		      ;; the load.  NOTE: if bit 15 is on, the load will sign-
		      ;; extend it, so we have to add 1 to the upper half now
		      ;; to counter the effect of the -1 from the sign-extension.
		      (inst cau lip object
			    (if (logbitp 15 offset)
				(1+ (ash offset -16))
				(ash offset -16)))
		      (inst l old lip (logand #xFFFF offset)))))
	     ;; Unless our immediate is all 1's, zero the destination bits.
	     (unless (and (sc-is value immediate)
			  (= (tn-value value) ,(1- (ash 1 bits))))
	       (inst li temp
		     (lognot (ash ,(1- (ash 1 bits)) (* extra ,bits))))
	       (inst n old temp))
	     ;; Unless the value is zero, really deposit it.
	     (unless (and (sc-is value immediate)
			  (zerop (tn-value value)))
	       (sc-case value
		 (immediate
		  (let ((value (ash (logand (tn-value value) ,(1- (ash 1 bits)))
				    (* extra ,bits))))
		    ;; Isn't this test silly, or the otherwise branch doesn't
		    ;; work anyway????
		    (cond ((< value #x10000)
			   (inst oil old (logand #xFF value))
			   (inst oiu old (logand #xFF00 value)))
			  (t
			   (inst li temp value)
			   (inst o old temp)))))
		 (unsigned-reg
		  (move temp value)
		  ;; Shouldn't we do this to check the size of the value?
		  (inst nilz temp ,(1- (ash 1 bits)))
		  (inst sl temp (* extra ,bits))
		  (inst o old temp))))
	     (storew old object vm:vector-data-offset vm:other-pointer-type)
	     (sc-case value
	       (immediate
		(inst li result (tn-value value)))
	       (unsigned-reg
		(move result value)))))))))

) ;EVAL-WHEN

(def-small-data-vector-frobs simple-bit-vector 1)
(def-small-data-vector-frobs simple-array-unsigned-byte-2 2)
(def-small-data-vector-frobs simple-array-unsigned-byte-4 4)



;;;; Float vectors.

#-afpa(progn
(define-vop (data-vector-ref/simple-array-mc68881-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (mc68881-single-reg)))
  (:result-types mc68881-single-float)
  (:temporary (:sc sap-reg :from :eval) scratch)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst cas lip object index)
    (inst inc lip (- (* vm:vector-data-offset vm:word-bytes)
		     vm:other-pointer-type))
    (inst mc68881-load value lip :single scratch)))

(define-vop (data-vector-set/simple-array-mc68881-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (mc68881-single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum mc68881-single-float)
  (:results (result :scs (mc68881-single-reg)))
  (:result-types mc68881-single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc sap-reg :from :eval) scratch)
  (:generator 20
    (inst cas lip object index)
    (inst inc lip (- (* vm:vector-data-offset vm:word-bytes)
		     vm:other-pointer-type))
    (inst mc68881-store value lip :single scratch)
    (unless (location= result value)
      (inst mc68881-move result value scratch))))

(define-vop (data-vector-ref/simple-array-mc68881-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (mc68881-double-reg)))
  (:result-types mc68881-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc sap-reg :from :eval) scratch)
  (:generator 20
    (inst cas lip object index)
    (inst cas lip lip index)
    (inst inc lip (- (* vm:vector-data-offset vm:word-bytes)
		     vm:other-pointer-type))
    (inst mc68881-load value lip :double scratch)))

(define-vop (data-vector-set/simple-array-mc68881-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (mc68881-double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum mc68881-double-float)
  (:results (result :scs (mc68881-double-reg)))
  (:result-types mc68881-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc sap-reg :from :eval) scratch)
  (:generator 20
    (inst cas lip object index)
    (inst cas lip lip index)
    (inst inc lip (- (* vm:vector-data-offset vm:word-bytes)
		     vm:other-pointer-type))
    (inst mc68881-store value lip :double scratch)
    (unless (location= result value)
      (inst mc68881-move result value scratch))))
)

#+afpa(progn
(define-vop (data-vector-ref/simple-array-afpa-single-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-single-float positive-fixnum)
  (:results (value :scs (afpa-single-reg)))
  (:result-types afpa-single-float)
  (:temporary (:sc sap-reg :from :eval) scratch)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 20
    (inst cas lip object index)
    (inst inc lip (- (* vm:vector-data-offset vm:word-bytes)
		     vm:other-pointer-type))
    (inst afpa-load value lip :single scratch)
    (inst afpa-noop scratch)))

(define-vop (data-vector-set/simple-array-afpa-single-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (afpa-single-reg) :target result))
  (:arg-types simple-array-single-float positive-fixnum afpa-single-float)
  (:results (result :scs (afpa-single-reg)))
  (:result-types afpa-single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc sap-reg :from :eval) scratch)
  (:generator 20
    (inst cas lip object index)
    (inst inc lip (- (* vm:vector-data-offset vm:word-bytes)
		     vm:other-pointer-type))
    (inst afpa-store value lip :single scratch)
    (inst afpa-noop scratch)
    (unless (location= result value)
      (inst afpa-move result value :single scratch))))

(define-vop (data-vector-ref/simple-array-afpa-double-float)
  (:note "inline array access")
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg)))
  (:arg-types simple-array-double-float positive-fixnum)
  (:results (value :scs (afpa-double-reg)))
  (:result-types afpa-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc sap-reg :from :eval) scratch)
  (:generator 20
    (inst cas lip object index)
    (inst cas lip lip index)
    (inst inc lip (- (* vm:vector-data-offset vm:word-bytes)
		     vm:other-pointer-type))
    (inst afpa-load value lip :double scratch)
    (inst afpa-noop scratch)))

(define-vop (data-vector-set/simple-array-afpa-double-float)
  (:note "inline array store")
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg))
	 (value :scs (afpa-double-reg) :target result))
  (:arg-types simple-array-double-float positive-fixnum afpa-double-float)
  (:results (result :scs (afpa-double-reg)))
  (:result-types afpa-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc sap-reg :from :eval) scratch)
  (:generator 20
    (inst cas lip object index)
    (inst cas lip lip index)
    (inst inc lip (- (* vm:vector-data-offset vm:word-bytes)
		     vm:other-pointer-type))
    (inst afpa-store value lip :double scratch)
    (inst afpa-noop scratch)
    (unless (location= result value)
      (inst afpa-move result value :double scratch))))
)


;;;; Raw bits without regard to vector type.

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
	 (index :scs (any-reg immediate))
	 (value :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 0 vm:other-pointer-type))



;;;; Vector subtype frobs.

(define-vop (get-vector-subtype get-header-data))
(define-vop (set-vector-subtype set-header-data))
