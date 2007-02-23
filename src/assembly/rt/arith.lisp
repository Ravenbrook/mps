;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/rt/arith.lisp,v 1.8 2003/08/03 11:27:50 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/assembly/rt/arith.lisp,v 1.8 2003/08/03 11:27:50 gerd Exp $
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;
;;; Written by William Lott.
;;; Converted to IBM RT by Bill Chiles.

(in-package "RT")


(define-assembly-routine (generic-+
			  (:cost 25)
			  (:return-style :full-call)
			  (:translate +)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (move temp x)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)
  (move temp y)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)
  ;; Use ocfp as a temporary result.
  (move ocfp x)
  (inst a ocfp y)
  (inst bnc :ov fixnum-result)
  ;; If we overflowed, shift the operands down to lose the fixnum lowtag bits
  ;; and do the add again.
  (move ocfp x)
  (inst sar ocfp 2)
  (move temp y)
  (inst sar temp 2)
  (inst a ocfp temp)
  ;; Allocate a bignum for sum.
  (with-fixed-allocation (res temp cname bignum-type
			      (1+ bignum-digits-offset))
    (storew ocfp res bignum-digits-offset other-pointer-type))
  ;; Now get out of here.
  (lisp-return lra lip :offset 1)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg-+)
  (inst li nargs (fixnumize 2))
  (loadw lip cname symbol-raw-function-addr-slot other-pointer-type)
  (move ocfp cfp-tn)
  ;; Tail call TWO-ARG-+.
  (inst bx lip)
  (move cfp-tn csp-tn)

  FIXNUM-RESULT
  ;; Ocfp was used for the temporary result.
  (move res ocfp))

(define-assembly-routine (generic--
			  (:cost 25)
			  (:return-style :full-call)
			  (:translate -)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (move temp x)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)
  (move temp y)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)
  ;; Use ocfp as a temporary result.
  (move ocfp x)
  (inst s ocfp y)
  (inst bnc :ov fixnum-result)
  ;; If we overflowed, shift the operands down to lose the fixnum lowtag bits
  ;; and do the subtract again.
  (move ocfp x)
  (inst sar ocfp 2)
  (move temp y)
  (inst sar temp 2)
  (inst s ocfp temp)
  ;; Allocate a bignum for sum.
  (with-fixed-allocation (res temp cname bignum-type
			      (1+ bignum-digits-offset))
    (storew ocfp res bignum-digits-offset other-pointer-type))
  ;; Now get out of here.
  (lisp-return lra lip :offset 1)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg--)
  (inst li nargs (fixnumize 2))
  (loadw lip cname symbol-raw-function-addr-slot other-pointer-type)
  (move ocfp cfp-tn)
  ;; Tail call TWO-ARG-+.
  (inst bx lip)
  (move cfp-tn csp-tn)

  FIXNUM-RESULT
  ;; Ocfp was used for the temporary result.
  (move res ocfp))

(define-assembly-routine (generic-*
			  (:cost 70)
			  (:return-style :full-call)
			  (:translate *)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp alloc word-pointer-reg a2-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs/high non-descriptor-reg nargs-offset)
			  (:temp ocfp/low non-descriptor-reg ocfp-offset)
			  (:temp cname descriptor-reg cname-offset))
  ;; If either arg is not a fixnum, call the static function.
  (move temp x)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)
  (move temp y)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)

  ;; Remove the tag from one arg so that the result will have the correct
  ;; fixnum lowtag.  Without this, the result if off by a factor of 16, but we
  ;; want it off by a factor of 4 (the fixnum lowtag).
  (move temp x)
  (inst sar temp 2)
  ;; Setup system register for multiply.
  (inst mtmqscr temp)

  ;; Do the multiply.
  ;; Subtract high from high to set it to zero and to set the C0 condition
  ;; bit appropriately for the m instruction.
  (inst s nargs/high nargs/high)
  (dotimes (i 16)
    (inst m nargs/high y))
  (inst mfmqscr res)

  ;; Check to see if the result will fit in a fixnum.  (I.e. the high word is
  ;; just 32 copies of the sign bit of the low word).
  (move temp res)
  (inst sar temp 31)
  (inst c temp nargs/high)
  (inst bc :eq DONE)
  ;; Build two bignum-digits by shifting the double word high:res down two bits
  ;; into high:low to get rid of the fixnum lowtag.
  (move ocfp/low res)
  (inst sr ocfp/low 2)
  (move temp nargs/high)
  (inst sl temp 30)
  (inst o ocfp/low temp)
  (inst sar nargs/high 2)
  ;; Allocate a bignum for the result.
  (pseudo-atomic (temp)
    (let ((one-word (gen-label)))
      (load-symbol-value alloc *allocation-pointer*)
      (inst cal res alloc other-pointer-type)
      ;; Assume we need one word.
      (inst cal alloc alloc (pad-data-block (1+ bignum-digits-offset)))
      ;; Is that correct?
      (move temp ocfp/low)
      (inst sar temp 31)
      (inst c temp nargs/high)
      (inst bcx :eq one-word)
      (inst li temp (logior (ash 1 type-bits) bignum-type))
      ;; Nope, we need two, so allocate the addition space.
      (inst cal alloc alloc (- (pad-data-block (+ 2 bignum-digits-offset))
			       (pad-data-block (1+ bignum-digits-offset))))
      (inst li temp (logior (ash 2 type-bits) bignum-type))
      (storew nargs/high res (1+ bignum-digits-offset) other-pointer-type)
      (emit-label one-word)
      (store-symbol-value alloc *allocation-pointer*)
      (storew temp res 0 other-pointer-type)
      (storew ocfp/low res bignum-digits-offset other-pointer-type)))
  (load-symbol-value temp *internal-gc-trigger*)
  (inst tlt temp alloc)
  ;; Out of here
  (lisp-return lra lip :offset 1)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg-*)
  (inst li nargs/high (fixnumize 2))
  (loadw lip cname symbol-raw-function-addr-slot other-pointer-type)
  (move ocfp/low cfp-tn)
  (inst bx lip)
  (move cfp-tn csp-tn)

  DONE)

(define-assembly-routine (generic-truncate
			  (:cost 95)
			  (:return-style :full-call)
			  (:translate truncate)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res quo (descriptor-reg any-reg) a0-offset)
			  (:res rem (descriptor-reg any-reg) a1-offset)

			  (:temp temp1 non-descriptor-reg nl0-offset)
			  (:temp alloc word-pointer-reg a2-offset)
			  (:temp lip interior-reg lip-offset)

			  ;; High holds the sign of x and becomes the remainder.
			  (:temp nargs/high non-descriptor-reg nargs-offset)
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp/temp2 non-descriptor-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (move temp1 x)
  (inst nilz temp1 3)
  (inst bnc :eq DO-STATIC-FUN)
  (move temp1 y)
  (inst nilz temp1 3)
  (inst bnc :eq DO-STATIC-FUN)

  ;; Make sure y is not 0, 1, or -1.
  (inst c y 0)
  (inst bcx :eq zero-divisor)
  (inst c y (fixnumize 1))
  (inst bcx :eq one-divisor)
  (inst c y (fixnumize -1))
  (inst bc :eq neg-one-divisor)

  ;; Do the division.
  (move nargs/high x)
  (inst sar nargs/high 31)
  (inst mtmqscr x)
  (dotimes (i 32)
    (inst d nargs/high y))
  ;; Check preliminary remainder by considering signs of high and y.
  ;; This is an extra step to account for the non-restoring divide-step instr.
  ;; We don't actually have to check this since the d instruction set :c0
  ;; when the signs of high and y are the same.
  (inst bc :c0 rem-okay)
  (inst a nargs/high y)
  REM-OKAY

  ;; The RT gives us some random division concept, but we're writing TRUNCATE.
  ;; The fixup involves adding one to the quotient and subtracting the divisor
  ;; from the remainder under certain circumstances:
  ;; IF the remainder is zero, we're done.
  (inst c nargs/high 0)
  (inst bc :eq move-results)
  ;; ELSE-IF the remainder is equal to the divisor, we can obviously take
  ;; one more divisor out of the dividend, so do it.
  (inst c nargs/high y)
  (inst bc :eq do-it)
  ;; ELSE-IF the divisor and dividend had different signs, adjust the results.
  (move temp1 x)
  (inst x temp1 y)
  (inst bnc :lt move-results)

  DO-IT
  ;; Add 1 to quotient and subtract divisor from remainder.
  (inst mfmqscr temp1)
  (inst sl temp1 2)
  (inst a quo temp1 (fixnumize 1))
  ;; Since rem and y are the same register, this subtracts the divisor from
  ;; the remainder, putting the result in our rem result register.
  (inst sf y nargs/high)

  (inst b done)

  MOVE-RESULTS
  (inst mfmqscr temp1)
  (inst sl temp1 2)
  (move quo temp1)
  ;; The remainder is always on the same order of magnitude as the dividend,
  ;; so it already has fixnum lowtag bits.
  (move rem nargs/high)
  (inst b done)

  DO-STATIC-FUN
  (load-symbol cname 'truncate)
  (inst li nargs/high (fixnumize 2))
  (loadw lip cname symbol-raw-function-addr-slot other-pointer-type)
  (move ocfp/temp2 cfp-tn)
  ;; Get out of here, doing a tail call.
  (inst bx lip)
  (move cfp-tn csp-tn)


  ZERO-DIVISOR
  ;; signal an error somehow.

  NEG-ONE-DIVISOR
  (inst li rem 0)
  (inst neg quo x)
  ;; If quo still fits in a fixnum, go to done.
  (inst bnc :ov done)
  ;; Allocate a bignum for quo.
  (with-fixed-allocation (temp1 ocfp/temp2 alloc bignum-type
				(+ 2 bignum-digits-offset))
    ;; If we overflowed quo, then we must have made a positive value from a
    ;; negative one.  The overflow would be one bit and some zero's for sign.
    (inst li ocfp/temp2 0)
    (storew quo temp1 bignum-digits-offset other-pointer-type)
    (storew ocfp/temp2 temp1 (1+ bignum-digits-offset) other-pointer-type))
  (move quo temp1)

  (inst b done)

  ONE-DIVISOR
  (inst li rem 0)
  (move quo x)

  DONE)



;;;; Conditionals

(define-assembly-routine (generic-<
			  (:cost 25)
			  (:return-style :full-call)
			  (:translate <)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (move temp x)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)
  (move temp y)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)

  ;; They are both fixnums, we can compare them.
  (inst c x y)
  (inst bcx :lt DONE)
  (load-symbol res t)

  (inst bx DONE)
  (move res null-tn)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg-<)
  (inst li nargs (fixnumize 2))
  (loadw lip cname symbol-raw-function-addr-slot other-pointer-type)
  (move ocfp cfp-tn)
  ;; Tail call TWO-ARG->.
  (inst bx lip)
  (move cfp-tn csp-tn)

  DONE)

(define-assembly-routine (generic->
			  (:cost 25)
			  (:return-style :full-call)
			  (:translate >)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If either arg is not a fixnum, call the static function.
  (move temp x)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)
  (move temp y)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)

  ;; They are both fixnums, we can compare them.
  (inst c x y)
  (inst bcx :gt DONE)
  (load-symbol res t)

  (inst bx DONE)
  (move res null-tn)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg->)
  (inst li nargs (fixnumize 2))
  (loadw lip cname symbol-raw-function-addr-slot other-pointer-type)
  (move ocfp cfp-tn)
  ;; Tail call TWO-ARG->.
  (inst bx lip)
  (move cfp-tn csp-tn)

  DONE)


(define-assembly-routine (generic-=
			  (:cost 25)
			  (:return-style :full-call)
			  (:translate =)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg) a0-offset)

			  (:temp temp non-descriptor-reg nl0-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp cname descriptor-reg cname-offset)
			  (:temp ocfp any-reg ocfp-offset))
  ;; If they are eq, they must be =
  (inst c x y)
  (inst bc :eq TRUE)

  ;; If both args are fixnums, we know the result is NIL.  Otherwise, we
  ;; have to hit the static fun.
  (move temp x)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)
  (move temp y)
  (inst nilz temp 3)
  (inst bnc :eq DO-STATIC-FUN)

  (inst bx DONE)
  (move res null-tn)

  DO-STATIC-FUN
  (load-symbol cname 'two-arg-=)
  (inst li nargs (fixnumize 2))
  (loadw lip cname symbol-raw-function-addr-slot other-pointer-type)
  (move ocfp cfp-tn)
  ;; Tail call TWO-ARG-=.
  (inst bx lip)
  (move cfp-tn csp-tn)

  TRUE
  (load-symbol res t)
  DONE)
