;;; -*- Package: RT; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/arith.lisp,v 1.12 2003/08/03 11:27:47 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/arith.lisp,v 1.12 2003/08/03 11:27:47 gerd Exp $
;;;
;;; This file contains the VM definition arithmetic VOPs for the IBM RT.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by Bill Chiles.
;;;

(in-package "RT")



;;;; Unary operations.

(define-vop (fixnum-unop)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num)
  (:policy :fast-safe))

(define-vop (signed-unop)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num)
  (:policy :fast-safe))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst neg res x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0) :target res)
	      temp)
  (:translate lognot)
  (:generator 2
    (inst li temp (fixnumize -1))
    (inst x temp x)
    (move res temp)))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (inst not res x)))



;;;; Binary fixnum operations (+, -, LOGIOR, LOGAND, LOGXOR).

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop)
  (:args (x :target r :scs (any-reg))
	 (y :scs (any-reg immediate) :to :save))
  (:arg-types tagged-num tagged-num)
  (:temporary (:scs (any-reg)) temp)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-unsigned-binop)
  (:args (x :target r :scs (unsigned-reg))
	 (y :scs (unsigned-reg immediate) :to :save))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:scs (any-reg)) temp)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-signed-binop)
  (:args (x :target r :scs (signed-reg))
	 (y :scs (signed-reg immediate) :to :save))
  (:arg-types signed-num signed-num)
  (:temporary (:scs (any-reg)) temp)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:effects)
  (:affected)
  (:policy :fast-safe))

(eval-when (compile eval)

(defmacro define-binop (translate cost op &body imm-body)
  `(progn
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-"
					(string translate)
					"/FIXNUM=>FIXNUM"))
		  fast-fixnum-binop)
       (:translate ,translate)
       (:generator ,cost
	 (sc-case y
	   (any-reg
	    (move r x)
	    (inst ,op r y))
	   (immediate
	    (let ((value (fixnumize (tn-value y))))
	      ,@imm-body)))))
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-"
					(string translate)
					"/SIGNED=>SIGNED"))
		  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ cost)
	 (sc-case y
	   (signed-reg
	    (move r x)
	    (inst ,op r y))
	   (immediate
	    (let ((value (tn-value y)))
	      ,@imm-body)))))
     (define-vop (,(intern (concatenate 'simple-string
					"FAST-"
					(string translate)
					"/UNSIGNED=>UNSIGNED"))
		  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ cost)
	 (sc-case y
	   (unsigned-reg
	    (move r x)
	    (inst ,op r y))
	   (immediate
	    (let ((value (tn-value y)))
	      ,@imm-body)))))))

) ;EVAL-WHEN


(define-binop + 3 a
  (cond ((typep value '(signed-byte 16))
	 (inst a r x value))
	(t
	 (move r x)
	 (inst li temp value)
	 (inst a r temp))))

(define-binop - 3 s
  (cond ((typep value '(signed-byte 16))
	 (inst s r x value))
	(t
	 (move r x)
	 (inst li temp value)
	 (inst s r temp))))

(define-binop logior 2 o
  (let ((low (ldb (byte 16 0) value))
	(high (ldb (byte 16 16) value)))
    (cond ((zerop value)
	   (move r x))
	  ((zerop low)
	   (inst oiu r x high))
	  ((zerop high)
	   (inst oil r x low))
	  (t
	   (inst oil r x low)
	   (inst oiu r r high)))))

(define-binop logxor 2 x
  (let ((low (ldb (byte 16 0) value))
	(high (ldb (byte 16 16) value)))
    (cond ((zerop value)
	   (move r x))
	  ((zerop low)
	   (inst xiu r x high))
	  ((zerop high)
	   (inst xil r x low))
	  (t
	   (inst xil r x low)
	   (inst xiu r r high)))))

(define-binop logand 1 n
  (let ((low (ldb (byte 16 0) value))
	(high (ldb (byte 16 16) value)))
    (cond ((= low high #xFFFF)
	   (move r x))
	  ((zerop low)
	   (inst niuz r x high))
	  ((= low #xFFFF)
	   (inst niuo r x high))
	  ((zerop high)
	   (inst nilz r x low))
	  ((= high #xFFFF)
	   (inst nilo r x low))
	  (t
	   (inst nilo r x low)
	   (inst niuo r r high)))))



;;;; Binary fixnum operations.

(define-vop (fast-ash)
  (:note "inline ASH")
  (:args (number :scs (signed-reg unsigned-reg) :to (:result 0) :target result)
	 (amount-arg :scs (signed-reg immediate) :target amount))
  (:arg-types (:or signed-num unsigned-num) signed-num)
  (:results (result :scs (signed-reg unsigned-reg)))
  (:result-types (:or signed-num unsigned-num))
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg :from (:argument 1)) amount)
  (:generator 12
    (sc-case amount-arg
      (signed-reg
       (let ((positive (gen-label))
	     (shift-right (gen-label))
	     (done (gen-label)))
	 ;; Copy the amount and check to see if it's positive.
	 (inst oil amount amount-arg 0)
	 (inst bnc :lt positive)

	 ;; We want to shift to the right, so make the amount positive.
	 (inst neg amount)
	 (inst c amount 32)
	 ;; If less than 32, do the shifting.
	 (inst bc :lt shift-right)
	 ;; 32 or greater, we just shift by 31.
	 (inst li amount 31)
	 (emit-label shift-right)
	 (move result number)
	 (inst bx done)
	 (sc-case number
	   (signed-reg
	    (inst sar result amount))
	   (unsigned-reg
	    (inst sr result amount)))

	 (emit-label positive)
	 ;; The result-type assures us that this shift will not overflow.
	 (move result number)
	 (inst sl result amount)

	 (emit-label done)))

      (immediate
       (let ((amount (tn-value amount-arg)))
	 (cond ((minusp amount)
		(sc-case number
		  (unsigned-reg
		   (move result number)
		   (inst sr result (min (- amount) 31)))
		  (t
		   (move result number)
		   (inst sar result (min (- amount) 31)))))
	       (t
		(move result number)
		(inst sl result (min amount 31)))))))))


;;; SIGNED-BYTE-32-LEN -- VOP.
;;;
;;; Common Lisp INTEGER-LENGTH.  Due to the definition of this operation,
;;;    (integer-length x) == (integer-length (lognot x)).
;;; We determine the result by using the RT's count-leading-zeros which only
;;; works on the zeros in the lower half of a word, so we have to use it on
;;; the upperhalf and lowerhalf of number separately and do a little addition
;;; (actually, subtraction from the right values) to get the length.
;;;
(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) number)
  (:temporary (:scs (non-descriptor-reg) :from (:eval 0)) temp)
  (:generator 16
    (let ((upper-not-zero (gen-label))
	  (count (gen-label)))
      ;; Move arg and tell us if number is positive.
      (inst oil number arg 0)
      (inst bnc :lt count)
      ;; Number is negative, so logically negate it to compute length.
      (inst not number)
      (emit-label count)
      ;; Shift the upperhalf down and see if there are any 1's in it.
      (move temp number)
      (inst sr temp 16)
      (inst bncx :eq upper-not-zero)
      (inst li res 32)
      ;; Upper half is all 0's, so get ready to subtract leading 0's in
      ;; the lowerhalf from 16 to determine integer length.
      (inst li res 16)
      (move temp number)
      (emit-label upper-not-zero)
      ;; Here temp contains the upperhalf if not all 0's or the lowerhalf.
      ;; Res contains the appropriate value (32 or 16) from which to subtract
      ;; the number of leading 0's in temp to determine integer length.
      (inst clz temp)
      (inst s res temp))))

;;; UNSIGNED-BYTE-32-COUNT -- VOP.
;;;
;;; To count the 1's in number, count how many times you can do the following:
;;;    AND number and the result of subtracting 1 from number.
;;;    Set number to the result of the AND operation.
;;; This subtract and AND clears the lowest 1 in number, so when number becomes
;;; zero, you're done.
;;;
(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)) number temp)
  (:generator 14
    (let ((loop (gen-label))
	  (done (gen-label)))
      ;; Move arg and tell us if number is zero, in which case res is 0.
      (inst oil number arg 0)
      (inst bcx :eq done)
      (inst li res 0)
      ;; Count 1's in number.
      (emit-label loop)
      (move temp number)
      (inst s temp 1)
      (inst n number temp)
      (inst bncx :eq loop)
      (inst a res (fixnumize 1))
      ;; We're done and res already has result.
      (emit-label done))))      



;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  ;; Leave room in the signed field for
  (:arg-types tagged-num (:constant (signed-byte 14)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (signed-byte 16)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (unsigned-byte 15)))
  (:info target not-p y))


(defmacro define-conditional-vop (translate &rest generator)
  `(progn
     ,@(mapcar #'(lambda (suffix cost signed)
		   (unless (and (member suffix '(/fixnum -c/fixnum))
				(eq translate 'eql))
		     `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
						    translate suffix))
				   ,(intern
				     (format nil "~:@(FAST-CONDITIONAL~A~)"
					     suffix)))
			(:translate ,translate)
			(:generator ,cost
			  (let* ((signed ,signed)
				 (-c/fixnum ,(eq suffix '-c/fixnum))
				 (y (if -c/fixnum (fixnumize y) y)))
			    ,@generator)))))
	       '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
	       ;; All these really take six cycles, but we decrease the -c/...
	       ;; cost to prefer these VOPs since we avoid doing
	       ;; load-immediates.  Then the first two are decreased by by one
	       ;; more to prefer comparing the fixnum representation directly.
	       '(5 4 6 5 6 5)
	       '(t t t t nil nil))))

(define-conditional-vop <
  (if signed
      (inst c x y)
      (inst cl x y))
  (if not-p
      (inst bnc :lt target)
      (inst bc :lt target)))

(define-conditional-vop >
  (if signed
      (inst c x y)
      (inst cl x y))
  (if not-p
      (inst bnc :gt target)
      (inst bc :gt target)))

;;; This only handles EQL of restricted integers, so it's simple.
;;;
(define-conditional-vop eql
  (declare (ignore signed))
  (inst c x y)
  (if not-p
      (inst bnc :eq target)
      (inst bc :eq target)))


;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.
;;;

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:ignore temp)
  (:generator 4
    (inst c x y)
    (if not-p
	(inst bnc :eq target)
	(inst bc :eq target))))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (signed-byte 14)))
  (:info target not-p y)
  (:translate eql)
  (:generator 3
    (inst c x (fixnumize y))
    (if not-p
	(inst bnc :eq target)
	(inst bc :eq target))))



;;;; 32-bit logical operations

(define-vop (32bit-logical)
  (:args (x :scs (unsigned-reg) :target r)
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:policy :fast-safe))

(define-vop (32bit-logical-not 32bit-logical)
  (:translate 32bit-logical-not)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:generator 1
    (inst not r x)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (move r x)
    (inst n r y)))

(deftransform 32bit-logical-nand ((x y) (* *))
  '(32bit-logical-not (32bit-logical-and x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (move r x)
    (inst o r y)))

(deftransform 32bit-logical-nor ((x y) (* *))
  '(32bit-logical-not (32bit-logical-or x y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (move r x)
    (inst x r y)))

(deftransform 32bit-logical-eqv ((x y) (* *))
  '(32bit-logical-not (32bit-logical-xor x y)))

(deftransform 32bit-logical-andc1 ((x y) (* *))
  '(32bit-logical-and (32bit-logical-not x) y))

(deftransform 32bit-logical-andc2 ((x y) (* *))
  '(32bit-logical-and x (32bit-logical-not y)))

(deftransform 32bit-logical-orc1 ((x y) (* *))
  '(32bit-logical-or (32bit-logical-not x) y))

(deftransform 32bit-logical-orc2 ((x y) (* *))
  '(32bit-logical-or x (32bit-logical-not y)))

(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg) :target r :to (:eval 0))
	 (amount :scs (signed-reg) :target temp))
  (:temporary (:scs (signed-reg) :from (:argument 1)) temp)
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "SHIFT-TOWARDS-START")
  (:generator 1
    (move temp amount)
    (inst nilz temp #x1f)
    (move r num)
    (inst sl r temp)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "SHIFT-TOWARDS-END")
  (:generator 1
    (move temp amount)
    (inst nilz temp #x1f)
    (move r num)
    (inst sr r temp)))




;;;; Bignum stuff.

(define-vop (bignum-length get-header-data)
  (:translate bignum::%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate bignum::%bignum-set-length)
  (:policy :fast-safe))

(define-vop (bignum-ref word-index-ref)
  (:variant vm:bignum-digits-offset vm:other-pointer-type)
  (:translate bignum::%bignum-ref)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (bignum-set word-index-set)
  (:variant vm:bignum-digits-offset vm:other-pointer-type)
  (:translate bignum::%bignum-set)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg immediate))
	 (value :scs (unsigned-reg)))
  (:arg-types t positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (digit-0-or-plus)
  (:translate bignum::%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 7
    (let ((done (gen-label)))
      (inst c digit 0)
      (inst bcx :lt done)
      (move result null-tn)
      (load-symbol result 't)
      (emit-label done))))

(define-vop (add-w/carry)
  (:translate bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :target result)
	 (b :scs (unsigned-reg))
	 (carry-in :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from (:argument 0))
	    (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :to (:argument 2)) temp)
  (:generator 5
    ;; Set the carry condition bit.
    (inst a temp carry-in -1)
    ;; Add A & B.
    (move result a)
    (inst ae result b)
    ;; Set carry to the condition bit.
    ;; Add zero to zero and see if we get a one.
    (inst li carry 0)
    (inst ae carry carry)))

(define-vop (sub-w/borrow)
  (:translate bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :target result)
	 (b :scs (unsigned-reg))
	 (borrow-in :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from (:argument 0))
	    (borrow :scs (unsigned-reg) :from :eval))
  (:result-types unsigned-num positive-fixnum)
  (:temporary (:scs (non-descriptor-reg)
		    :from (:argument 0) :to (:argument 2)) temp)
  (:generator 5
    (move result a)
    ;; Set the carry condition bit.
    ;; Borrow-in is zero if there was a borrow, one if there was no borrow.
    ;; The RT has the same sense with its C0 condition bit.
    (inst a temp borrow-in -1)
    (inst se result b)
    ;; Set borrow to 0 if the carry condition bit is zero, or to 1 otherwise.
    (inst li borrow 0)
    (inst ae borrow borrow)))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:vop-var vop)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (carry-in :scs (unsigned-reg unsigned-stack) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:eval 0)) temp)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target high-res) high)
  (:temporary (:scs (unsigned-reg) :from (:eval 0) :to (:result 1)
		    :target low-res) low)
  (:results (high-res :scs (unsigned-reg))
	    (low-res :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 76
    ;; Do the multiply.
    (unsigned-multiply x y high low)

    ;; Add the carry-in.
    (sc-case carry-in
      (unsigned-stack
       (load-stack-tn temp carry-in vop)
       (inst a low temp))
      (unsigned-reg
       (inst a low carry-in)))
    (inst ae high 0)
    (move high-res high)
    (move low-res low)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:vop-var vop)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg))
	 (prev :scs (unsigned-reg unsigned-stack) :to (:eval 1))
	 (carry-in :scs (unsigned-reg unsigned-stack) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :from (:eval 0)) temp)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target high-res) high)
  (:temporary (:scs (unsigned-reg) :from (:eval 0) :to (:result 1)
		    :target low-res) low)
  (:results (high-res :scs (unsigned-reg))
	    (low-res :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 81
    ;; Do the multiply.
    (unsigned-multiply x y high low)

    ;; Add the carry-in.
    (sc-case carry-in
      (unsigned-stack
       (load-stack-tn temp carry-in vop)
       (inst a low temp))
      (unsigned-reg
       (inst a low carry-in)))
    (inst ae high 0)
    
    ;; Add in digit from accumulating result.
    (sc-case carry-in
      (unsigned-stack
       (load-stack-tn temp prev vop)
       (inst a low temp))
      (unsigned-reg
       (inst a low prev)))
    (inst ae high 0)
    (move high-res high)
    (move low-res low)))

(define-vop (bignum-mult)
  (:translate bignum::%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (high :scs (unsigned-reg) :from :load)
	    (low :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 74
    ;; Do the multiply.
    (unsigned-multiply x y high low)))

;;; UNSIGNED-MULTIPLY -- Internal.
;;;
;;; The RT has a signed multiply.  To unsigned-multiply bignum digits, we use
;;; the following identity:
;;;    signed-interpretation
;;;       =  unsigned-interpretation  -  (sign-bit)(2^32)
;;;    ==>
;;;    ui = si + (sb)(2^32)
;;; This gives us the following equation:
;;;    (ui1) (ui2)  =  [si1 + (sb1)(2^32)] [si2 + (sb2)(2^32)]
;;;    (ui1) (ui2)  =  (si1) (si2)
;;;                  + [(sb1)(si2) + (sb2)(si1)] (2^32)
;;; 		     + (sb1)(sb2)(2^32)
;;; Inspection and the fact that the result must fit into 64 bits reveals that
;;; the third time can always be ignored.
;;;
(defun unsigned-multiply (x y high low)
  ;; Setup system register for multiply.
  (inst mtmqscr x)
  
  ;; Do the multiply.
  ;; Subtract high from high to set it to zero and to set the C0 condition
  ;; bit appropriately for the m instruction.
  (inst s high high)
  (dotimes (i 16)
    (inst m high y))
  ;; Adjust high word of result for unsigned multiply.
  ;; If x is negative, add in y.  If y is negative, add in x.
  (let ((x-pos (gen-label))
	(y-pos (gen-label)))
    (inst c x 0)
    (inst bnc :lt x-pos)
    (inst a high y)
    (emit-label x-pos)
    (inst c y 0)
    (inst bnc :lt y-pos)
    (inst a high x)
    (emit-label y-pos))
  
  ;; Get the low 32 bits of the product.
  (inst mfmqscr low))


(define-vop (bignum-lognot)
  (:translate bignum::%lognot)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst not r x)))

(define-vop (fixnum-to-digit)
  (:translate bignum::%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg) :target digit))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move digit fixnum)
    (inst sar digit 2)))

;;; BIGNUM-FLOOR -- VOP.
;;;
;;; The way the bignum code uses this allows us to ignore dividing by 0, 1, or
;;; -1.  Furthermore, we always divided a positive high:low value by a positive
;;; divisor value.  This means we don't have to worry about using the RT's
;;; signed divide instruction to get an unsigned division result.
;;;
;;; We are going to copy the GENERIC-TRUNCATE assembly routine to implement
;;; this since FLOOR and TRUNCATE return the same values for positive
;;; arguments.  However, we do modify it slightly to make use of the positive
;;; nature of the arguments.
;;;
(define-vop (bignum-floor)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (dividend-high :scs (unsigned-reg) :target rem)
	 (dividend-low :scs (unsigned-reg))
	 (divisor :scs (unsigned-reg) :to (:eval 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg) :from (:eval 0)))
  (:result-types unsigned-num unsigned-num)
  (:generator 44
    (inst mtmqscr dividend-low)
    (move rem dividend-high)
    (dotimes (i 32)
      (inst d rem divisor))
    ;; Check preliminary remainder by considering signs of rem and divisor.
    ;; This is an extra step to account for the non-restoring divide-step instr.
    ;; We don't actually have to check this since the d instruction set :c0
    ;; when the signs of rem and divisor are the same.
    (let ((rem-okay (gen-label)))
      (inst bc :c0 rem-okay)
      (inst a rem divisor)
      (emit-label rem-okay))

    (let ((move-results (gen-label)))
      ;; The RT gives us some random division concept, but we're writing
      ;; TRUNCATE.  The fixup involves adding one to the quotient and
      ;; subtracting the divisor from the remainder under certain
      ;; circumstances (for this VOP, as opposed to the assembly routine,
      ;; this boils down to one test):
      ;;
      ;; IF the remainder is equal to the divisor, we can obviously take
      ;; one more divisor out of the dividend, so do it.
      (inst c rem divisor)
      (inst bnc :eq move-results) ;Just fall through to do it.

      ;; Add 1 to quotient and subtract divisor from remainder.
      (inst mfmqscr quo)
      (inst inc quo 1)
      (inst s rem divisor)
      
      (emit-label move-results))

    (inst mfmqscr quo)))


;;; SIGNIFY-DIGIT -- VOP.
;;;
;;; This is supposed to make digit into a fixnum which is signed.  We just
;;; move it here, letting the compiler's operand motion stuff make this
;;; result into a fixnum.
;;;
(define-vop (signify-digit)
  (:translate bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (move res digit)))

(define-vop (digit-ashr)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target result)
	 (count :scs (unsigned-reg immediate)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:generator 2
    (move result digit)
    (inst sar result
	  (sc-case count
	    (unsigned-reg count)
	    (immediate (tn-value count))))))

(define-vop (digit-lshr digit-ashr)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 2
    (move result digit)
    (inst sr result
	  (sc-case count
	    (unsigned-reg count)
	    (immediate (tn-value count))))))

(define-vop (digit-ashl digit-ashr)
  (:translate bignum::%ashl)
  (:generator 2
    (move result digit)
    (inst sl result
	  (sc-case count
	    (unsigned-reg count)
	    (immediate (tn-value count))))))



;;;; Static functions.

(define-static-function two-arg-gcd (x y) :translate gcd)
(define-static-function two-arg-lcm (x y) :translate lcm)

(define-static-function two-arg-/ (x y) :translate /)

(define-static-function %negate (x) :translate %negate)

(define-static-function two-arg-and (x y) :translate logand)
(define-static-function two-arg-ior (x y) :translate logior)
(define-static-function two-arg-xor (x y) :translate logxor)
