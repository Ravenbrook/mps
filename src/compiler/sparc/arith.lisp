;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/arith.lisp,v 1.44 2004/08/22 15:32:48 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition arithmetic VOPs for the SPARC.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;;
;;; Enhancements/debugging by Raymond Toy 1999, 2000

(in-package "SPARC")



;;;; Unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))


(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (inst neg res x)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (inst neg res x)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
    (inst xor res x (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (inst not res x)))


;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero))
	 (y :target r :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero))
	 (y :target r :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero))
	 (y :target r :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
	      (:constant (and (signed-byte #.(- 13 vm:fixnum-tag-bits)) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero)))
  (:info y)
  (:arg-types unsigned-num
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg zero)))
  (:info y)
  (:arg-types signed-num
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(eval-when (compile load eval)

(defmacro define-binop (translate untagged-penalty op
			&optional arg-swap restore-fixnum-mask)
  `(progn
     (define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
		   fast-fixnum-binop)
       (:translate ,translate)
       ,@(when restore-fixnum-mask
	       `((:temporary (:sc non-descriptor-reg) temp)))
       (:generator 2
	 ,(if arg-swap
	      `(inst ,op ,(if restore-fixnum-mask 'temp 'r) y x)
	      `(inst ,op ,(if restore-fixnum-mask 'temp 'r) x y))
	 ,@(when restore-fixnum-mask
		 ``(inst andn r temp fixnum-tag-mask))))
     ,@(unless arg-swap
	       `((define-vop (,(symbolicate "FAST-" translate "-C/FIXNUM=>FIXNUM")
			       fast-fixnum-binop-c)
		   (:translate ,translate)
		   ,@(when restore-fixnum-mask
			   `((:temporary (:sc non-descriptor-reg) temp)))
		   (:generator 1
		     (inst ,op ,(if restore-fixnum-mask 'temp 'r) x (fixnumize y))
		     ,@(when restore-fixnum-mask
			     `((inst andn r temp fixnum-tag-mask)))))))
     (define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
		  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 ,(if arg-swap
	      `(inst ,op r y x)
	      `(inst ,op r x y))))
     ,@(unless arg-swap
	       `((define-vop (,(symbolicate "FAST-" translate "-C/SIGNED=>SIGNED")
			       fast-signed-binop-c)
		   (:translate ,translate)
		   (:generator ,untagged-penalty
		     (inst ,op r x y)))))
     (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED=>UNSIGNED")
		  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
	 ,(if arg-swap
	      `(inst ,op r y x)
	      `(inst ,op r x y))))
     ,@(unless arg-swap
	       `((define-vop (,(symbolicate "FAST-" translate "-C/UNSIGNED=>UNSIGNED")
			       fast-unsigned-binop-c)
		   (:translate ,translate)
		   (:generator ,untagged-penalty
		     (inst ,op r x y)))))))

); eval-when

(define-binop + 4 add)
(define-binop - 4 sub)
(define-binop logand 2 and)
(define-binop logandc1 2 andn t)
(define-binop logandc2 2 andn)
(define-binop logior 2 or)
(define-binop logorc1 2 orn t t)
(define-binop logorc2 2 orn nil t)
(define-binop logxor 2 xor)
(define-binop logeqv 2 xnor nil t)

;;; Special logand cases: (logand signed unsigned) => unsigned

(define-vop (fast-logand/signed-unsigned=>unsigned
	     fast-logand/unsigned=>unsigned)
    (:args (x :scs (signed-reg))
	   (y :target r :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num))

(define-vop (fast-logand/unsigned-signed=>unsigned
	     fast-logand/unsigned=>unsigned)
    (:args (x :target r :scs (unsigned-reg))
	   (y :scs (signed-reg)))
  (:arg-types unsigned-num signed-num))

;; This vop is intended to handle the case (logand x #xffffffff) when
;; x is a (signed-byte 32).  We can just do a register move instead of
;; and'ing with #xffffffff.
(define-vop (fast-logand-c/signed-unsigned=>unsigned fast-unsigned-binop-c)
  (:args (x :scs (signed-reg)))
  (:translate logand)
  (:arg-types signed-num
	      (:constant (or (and (unsigned-byte 12) (not (integer 0 0)))
			     (integer #xfffff000 #xffffffff))))
  (:generator 1				; Needs to be low to give this vop a chance.
    (cond ((= y #xffffffff)
	   (move r x))
	  ((typep y '(unsigned-byte 13))
	   (inst and r x y))
	  (t
	   ;; The constant is really a (signed-byte 13), so convert it
	   ;; to a negative number so the immediate operand gets
	   ;; signed extended to the right bits.
	   (inst and r x (- y #x100000000))
	   ))))

(define-vop (fast-abs/signed fast-safe-arith-op)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate abs)
  (:note "inline 32-bit abs")
  (:temporary (:scs (signed-reg)) y)
  (:generator 1
    ;; From Hacker's Delight
    ;;
    ;; abs(x) = (x ^ y) - y, where y = x >> 31 (signed shift)
    (inst sran y x (1- vm:word-bits))
    (inst xor r y x)
    (inst sub r y)))

;;; Special case fixnum + and - that trap on overflow.  Useful when we
;;; don't know that the output type is a fixnum.

;;; I (toy@rtp.ericsson.se) took these out.  They don't seem to be
;;; used anywhere at all.
#+nil
(progn
(define-vop (+/fixnum fast-+/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 4
    (inst taddcctv r x y)))

(define-vop (+-c/fixnum fast-+-c/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 3
    (inst taddcctv r x (fixnumize y))))

(define-vop (-/fixnum fast--/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 4
    (inst tsubcctv r x y)))

(define-vop (--c/fixnum fast---c/fixnum=>fixnum)
  (:policy :safe)
  (:results (r :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "safe inline fixnum arithmetic")
  (:generator 3
    (inst tsubcctv r x (fixnumize y))))

)

;;; Truncate

;; This doesn't work for some reason.
#+nil
(define-vop (fast-v8-truncate/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (quo :scs (any-reg))
	    (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note "inline fixnum arithmetic")
  (:temporary (:scs (any-reg) :target quo) q)
  (:temporary (:scs (any-reg)) r)
  (:temporary (:scs (signed-reg)) y-int)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 12
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero)
      ;; Extend the sign of X into the Y register
        (inst sra r x 31)
      (inst wry r)
      ;; Remove tag bits so Q and R will be tagged correctly.
      (inst sra y-int y fixnum-tag-bits)
      (inst nop)
      (inst nop)

      (inst sdiv q x y-int)		; Q is tagged.
      ;; We have the quotient so we need to compute the remainder
      (inst smul r q y-int)		; R is tagged
      (inst sub rem x r)
      (unless (location= quo q)
	(move quo q)))))

(define-vop (fast-v8-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (quo :scs (signed-reg))
	    (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:temporary (:scs (signed-reg) :target quo) q)
  (:temporary (:scs (signed-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 12
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero #+sparc-v9 :pn)
      ;; Extend the sign of X into the Y register
        (inst sra r x 31)
      (inst wry r)
      (inst nop)
      (inst nop)
      (inst nop)

      (inst sdiv q x y)
      ;; We have the quotient so we need to compue the remainder
      (inst smul r q y)		; rem
      (inst sub rem x r)
      (unless (location= quo q)
	(move quo q)))))

(define-vop (fast-v8-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:temporary (:scs (unsigned-reg) :target quo) q)
  (:temporary (:scs (unsigned-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 8
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero #+sparc-v9 :pn)
        (inst wry zero-tn)		; Clear out high part
      (inst nop)
      (inst nop)
      (inst nop)
      
      (inst udiv q x y)
      ;; Compute remainder
      (inst umul r q y)
      (inst sub rem x r)
      (unless (location= quo q)
	(inst move quo q)))))

(define-vop (fast-v9-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg))
	 (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (quo :scs (signed-reg))
	    (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:temporary (:scs (signed-reg) :target quo) q)
  (:temporary (:scs (signed-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (backend-featurep :sparc-64))
  (:generator 8
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero :pn)
      ;; Sign extend the numbers, just in case.
        (inst signx x)
      (inst signx y)
      (inst sdivx q x y)
      ;; Compute remainder
      (inst mulx r q y)
      (inst sub rem x r)
      (unless (location= quo q)
	(inst move quo q)))))

#+nil
(define-vop (fast-v9-truncate/signed64=>signed64 fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed64-reg))
	 (y :scs (signed64-reg)))
  (:arg-types signed64-num signed64-num)
  (:results (quo :scs (signed64-reg))
	    (rem :scs (signed64-reg)))
  (:result-types signed64-num signed64-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:temporary (:scs (signed64-reg) :target quo) q)
  (:temporary (:scs (signed64-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (backend-featurep :sparc-v9))
  (:generator 8
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero :pn :xcc)
      (inst nop)

      (inst sdivx q x y)
      ;; Compute remainder
      (inst mulx r q y)
      (inst sub rem x r)
      (unless (location= quo q)
	(inst move quo q)))))

(define-vop (fast-v9-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg))
	 (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:temporary (:scs (unsigned-reg) :target quo) q)
  (:temporary (:scs (unsigned-reg)) r)
  (:vop-var vop)
  (:save-p :compute-only)
  (:guard (backend-featurep :sparc-64))
  (:generator 8
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (inst cmp y zero-tn)
      (inst b :eq zero :pn)
      ;; Zap the higher 32 bits, just in case
        (inst clruw x)
      (inst clruw y)
      (inst udivx q x y)
      ;; Compute remainder
      (inst mulx r q y)
      (inst sub rem x r)
      (unless (location= quo q)
	(inst move quo q)))))

;;; Shifting

(define-vop (fast-ash/signed=>signed)
  (:note "inline (signed-byte 32) ASH")
  (:args (number :scs (signed-reg) :to :save)
	 (amount :scs (signed-reg immediate) :to :save))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 5
    (sc-case amount
      (signed-reg
       (cond ((backend-featurep :sparc-v9)
	      (let ((done (gen-label)))
		(inst cmp amount)
		(inst b :ge done)
		;; The result-type assures us that this shift will not
		;; overflow.
		(inst slln result number amount)
		(inst neg ndesc amount)
		;; ndesc = max(-amount, 31)
		(inst cmp ndesc 31)
		(inst cmove :ge ndesc 31)
		(inst sran result number ndesc)
		(emit-label done)))
	     (t
	      (let ((done (gen-label)))
		(inst cmp amount)
		(inst b :ge done)
		;; The result-type assures us that this shift will not overflow.
		(inst slln result number amount)

		;; Handle right shifts here.
		(inst neg ndesc amount)
		(inst cmp ndesc 31)
		(inst b :le done)
		(inst sran result number ndesc)
		;; Right shift of greater than 31 bits is the same as a shift
		;; of 31 bits for signed 32-bit numbers.
		(inst sran result number 31)

		(emit-label done)))))

	(immediate
	 (let ((amount (tn-value amount)))
	   (cond ((< amount -31)
		  (inst li result -1))
		 ((< amount 0)
		  (inst sran result number (- amount)))
		 ((> amount 0)
		  (inst slln result number amount))
		 (t
		  ;; amount = 0.  Shouldn't happen because of a
		  ;; deftransform, but it's easy.
		  (move result number))))))))

(define-vop (fast-ash/unsigned=>unsigned)
  (:note "inline (unsigned-byte 32) ASH")
  (:args (number :scs (unsigned-reg) :to :save)
	 (amount :scs (signed-reg immediate) :to :save))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc non-descriptor-reg) ndesc)
  (:generator 5
    (sc-case amount
      (signed-reg
       (cond ((backend-featurep :sparc-v9)
	      (let ((done (gen-label)))
		(inst cmp amount)
		(inst b :ge done)
		;; The result-type assures us that this shift will not
		;; overflow.
		(inst slln result number amount)
		(inst neg ndesc amount)
		;; A right shift of 32 or more results in zero.
		(inst cmp ndesc 32)
		(inst srln result number ndesc)
		(inst cmove :ge result zero-tn)
		(emit-label done)))
	     (t
	      (let ((done (gen-label)))
		(inst cmp amount)
		(inst b :ge done)
		;; The result-type assures us that this shift will not
		;; overflow.
		(inst slln result number amount)
		(inst neg ndesc amount)
		(inst cmp ndesc 32)
		(inst b :lt done)
		(inst srln result number ndesc)
		;; Right shift of 32 or more is the same as zero for unsigned
		;; 32-bit numbers.
		(move result zero-tn)

		(emit-label done)))))

      (immediate
       (let ((amount (tn-value amount)))
	 (cond ((< amount -31)
		(move result zero-tn))
	       ((< amount 0)
		(inst srln result number (- amount)))
	       ((> amount 0)
		(inst slln result number amount))
	       (t
		;; amount = 0.  Shouldn't happen because of a
		;; deftransform, but it's easy.
		(move result number))))))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:note "inline constant ASH")
  (:args (number :scs (unsigned-reg)))
  (:info count)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate ash)
  (:policy :fast-safe)
  (:generator 4
    (cond
      ((< count -31) (move result zero-tn))
      ((< count 0) (inst srl result number (min (- count) 31)))
      ((> count 0) (inst sll result number (min count 31)))
      (t (error "identity ASH not transformed away")))))

;; Some special cases where we know we want a left shift.  Just do the
;; shift, instead of checking for the sign of the shift.
(macrolet
    ((frob (name sc-type type result-type cost)
       `(define-vop (,name)
	 (:note "inline ASH")
	 (:translate ash)
	 (:args (number :scs (,sc-type))
	        (amount :scs (signed-reg unsigned-reg immediate)))
	 (:arg-types ,type positive-fixnum)
	 (:results (result :scs (,result-type)))
	 (:result-types ,type)
	 (:policy :fast-safe)
	 (:generator ,cost
	  ;; The result-type assures us that this shift will not
	  ;; overflow. And for fixnum's, the zero bits that get
	  ;; shifted in are just fine for the fixnum tag.
	  (sc-case amount
	   ((signed-reg unsigned-reg)
	    (inst slln result number amount))
	   (immediate
	    (let ((amount (tn-value amount)))
	      (assert (>= amount 0))
	      (inst slln result number amount))))))))
  (frob fast-ash-left/signed=>signed signed-reg signed-num signed-reg 3)
  (frob fast-ash-left/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (frob fast-ash-left/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

;; Constant left shift
(macrolet
    ((frob (name sc-type type result-type cost)
       `(define-vop (,name)
	  (:note "inline ASH")
	  (:translate ash)
	  (:args (number :scs (,sc-type)))
	  (:info amount)
	  (:arg-types ,type
		      (:constant (integer 0 31)))
	  (:results (result :scs (,result-type)))
	  (:result-types ,type)
	  (:policy :fast-safe)
	  (:generator ,cost
	    ;; The result-type assures us that this shift will not
	    ;; overflow. And for fixnum's, the zero bits that get
	    ;; shifted in are just fine for the fixnum tag.
	    (if (zerop amount)
		(move result number)
		(inst slln result number amount))))))
  (frob fast-ash-left-c/signed=>signed signed-reg signed-num signed-reg 3)
  (frob fast-ash-left-c/fixnum=>fixnum any-reg tagged-num any-reg 2)
  (frob fast-ash-left-c/unsigned=>unsigned unsigned-reg unsigned-num unsigned-reg 3))

;;#+sparc-v9
#+nil
(define-vop (fast-ash-left/signed64=>signed64)
    (:note "inline ASH")
  (:translate ash)
  (:args (number :scs (signed64-reg))
	 (amount :scs (signed-reg unsigned-reg immediate)))
  (:arg-types signed64-num positive-fixnum)
  (:results (result :scs (signed64-reg)))
  (:result-types ,type)
  (:policy :fast-safe)
  (:generator ,cost
    ;; The result-type assures us that this shift will not
    ;; overflow. And for fixnum's, the zero bits that get shifted in
    ;; are just fine for the fixnum tag.
    (sc-case amount
      ((signed-reg unsigned-reg)
       (inst sllx result number amount))
      (immediate
       (let ((amount (tn-value amount)))
	 (assert (>= amount 0))
	 (inst sllx result number amount))))))
    
#-(and sparc-v9 sparc-v8plus)
(defknown ash-right-signed ((signed-byte #.vm:word-bits)
			    (and fixnum unsigned-byte))
  (signed-byte #.vm:word-bits)
  (movable foldable flushable))

#-(and sparc-v9 sparc-v8plus)
(defknown ash-right-unsigned ((unsigned-byte #.vm:word-bits)
			      (and fixnum unsigned-byte))
  (unsigned-byte #.vm:word-bits)
  (movable foldable flushable))

#+(and sparc-v9 sparc-v8plus)
(defknown ash-right-signed ((signed-byte #.(* 2 vm:word-bits))
			    (and fixnum unsigned-byte))
  (signed-byte #.(* 2 vm:word-bits))
  (movable foldable flushable))

#+(and sparc-v9 sparc-v8plus)
(defknown ash-right-unsigned ((unsigned-byte #.(* 2 vm:word-bits))
			      (and fixnum unsigned-byte))
  (unsigned-byte #.(* 2 vm:word-bits))
  (movable foldable flushable))



;; Some special cases where we want a right shift.  Just do the shift.
;; (Needs appropriate deftransforms to call these, though.)

(macrolet
    ((frob (trans name sc-type type shift-inst cost)
       `(define-vop (,name)
	 (:note "inline right ASH")
	 (:translate ,trans)
	 (:args (number :scs (,sc-type))
	        (amount :scs (signed-reg unsigned-reg immediate)))
	 (:arg-types ,type positive-fixnum)
	 (:results (result :scs (,sc-type)))
	 (:result-types ,type)
	 (:policy :fast-safe)
	 (:generator ,cost
	    (sc-case amount
	     ((signed-reg unsigned-reg)
		(inst ,shift-inst result number amount))
	     (immediate
	      (let ((amt (tn-value amount)))
		(inst ,shift-inst result number amt))))))))
  (frob ash-right-signed fast-ash-right/signed=>signed
	signed-reg signed-num sra 3)
  (frob ash-right-unsigned fast-ash-right/unsigned=>unsigned
	unsigned-reg unsigned-num srl 3)
  #+(and sparc-v9 sparc-v8plus)
  (frob ash-right-signed fast-ash-right/signed64=>signed64
	signed64-reg signed64-num srax 3)
  #+(and sparc-v9 sparc-v8plus)
  (frob ash-right-unsigned fast-ash-right/unsigned64=>unsigned64
	unsigned64-reg unsigned64-num srlx 3)
  )

;; Constant right shift.
(macrolet
    ((frob (trans name sc-type type shift-inst cost max-shift)
       `(define-vop (,name)
	 (:note "inline right ASH")
	 (:translate ,trans)
	 (:args (number :target result :scs (,sc-type)))
	 (:info amount)
	 (:arg-types ,type
		     (:constant (integer 0 ,max-shift)))
	 (:results (result :scs (,sc-type)))
	 (:result-types ,type)
	 (:policy :fast-safe)
	 (:generator ,cost
	   (if (zerop amount)
	       (move result number)
	       (inst ,shift-inst result number amount))))))
  (frob ash-right-signed fast-ash-right-c/signed=>signed
	signed-reg signed-num sra 1 31)
  (frob ash-right-unsigned fast-ash-right-c/unsigned=>unsigned
	unsigned-reg unsigned-num srl 1 31)
  #+(and sparc-v9 sparc-v8plus)
  (frob ash-right-signed fast-ash-right-c/signed64=>signed64
	signed64-reg signed64-num srax 1 63)
  #+(and sparc-v9 sparc-v8plus)
  (frob ash-right-unsigned fast-ash-right-c/unsigned64=>unsigned64
	unsigned64-reg unsigned64-num srlx 1 63)
  )

#+nil
(define-vop (fash-ash-right-c/signed=>signed fast-signed-binop-c)
  (:args (x :target r :scs (signed-reg zero)))
  (:arg-types signed-num
	      (:constant (integer 0 31)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:translate ash-right-signed)
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 1
    (if (zerop y)
	(move r x)
	(inst srln r x y))))

  
(define-vop (fast-ash-right/fixnum=>fixnum)
    (:note "inline right ASH")
  (:translate ash-right-signed)
  (:args (number :scs (any-reg))
	 (amount :scs (signed-reg unsigned-reg immediate)))
  (:arg-types tagged-num positive-fixnum)
  (:results (result :scs (any-reg)))
  (:result-types tagged-num)
  (:temporary (:sc non-descriptor-reg :target result) temp)
  (:policy :fast-safe)
  (:generator 2
    ;; Shift the fixnum right by the desired amount.  Then zap out the
    ;; 2 LSBs to make it a fixnum again.  (Those bits are junk.)
    (sc-case amount
      ((signed-reg unsigned-reg)
       (inst sran temp number amount))
      (immediate
       (inst sran temp number (tn-value amount))))
    (inst andn result temp fixnum-tag-mask)))
    



(define-vop (signed-byte-32-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target shift))
  (:arg-types signed-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 30
    (let ((loop (gen-label))
	  (test (gen-label)))
      (inst addcc shift zero-tn arg)
      (inst b :ge test)
      (move res zero-tn)
      (inst b test)
      (inst not shift)

      (emit-label loop)
      (inst add res (fixnumize 1))
      
      (emit-label test)
      (inst cmp shift)
      (inst b :ne loop)
      (inst srln shift 1))))

(define-vop (unsigned-byte-32-len)
  (:translate integer-length)
  (:note "inline (unsigned-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target shift))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) shift)
  (:generator 30
    (let ((loop (gen-label))
	  (test (gen-label)))
      (move shift arg)
      (inst b test)
      (move res zero-tn)

      (emit-label loop)
      (inst add res (fixnumize 1))
      
      (emit-label test)
      (inst cmp shift)
      (inst b :ne loop)
      (inst srln shift 1))))


(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) mask temp)
  (:generator 35
      (move res arg)

      (dolist (stuff '((1 #x55555555) (2 #x33333333) (4 #x0f0f0f0f)
		       (8 #x00ff00ff) (16 #x0000ffff)))
	(destructuring-bind (shift bit-mask)
	    stuff
	  ;; Set mask
	  (inst li mask bit-mask)

	  (inst and temp res mask)
	  (inst srln res shift)
	  (inst and res mask)
	  (inst add res temp)))))


;;; Multiply and Divide.

(define-vop (fast-v8-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 2
    ;; The cost here should be less than the cost for
    ;; */signed=>signed.  Why?  A fixnum product using signed=>signed
    ;; has to convert both args to signed-nums.  But using this, we
    ;; don't have to and that saves an instruction.
    (inst sran temp y fixnum-tag-bits)
    (inst smul r x temp)))

;; Multiplication by a constant.
(define-vop (fast-v8-*-c/unsigned=>unsigned fast-unsigned-binop-c)
  (:translate *)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 2
    (inst umul r x y)))

(define-vop (fast-v8-*-c/signed=>signed fast-signed-binop-c)
  (:translate *)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 2
    (inst smul r x y)))

(define-vop (fast-v8-*-c/fixnum=>fixnum fast-safe-arith-op)
  (:args (x :target r :scs (any-reg zero)))
  (:info y)
  (:arg-types tagged-num
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:translate *)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 1
    (inst smul r x y)))


(define-vop (fast-v8-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 3
    (inst smul r x y)))

(define-vop (fast-v8-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 3
    (inst umul r x y)))

;; The smul and umul instructions are deprecated on the Sparc V9.  Use
;; mulx instead.
(define-vop (fast-v9-*/fixnum=>fixnum fast-fixnum-binop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate *)
  (:guard (backend-featurep :sparc-64))
  (:generator 4
    (inst sran temp y fixnum-tag-bits)
    (inst mulx r x temp)))

(define-vop (fast-v9-*/signed=>signed fast-signed-binop)
  (:translate *)
  (:guard (backend-featurep :sparc-64))
  (:generator 3
    (inst mulx r x y)))

(define-vop (fast-v9-*/unsigned=>unsigned fast-unsigned-binop)
  (:translate *)
  (:guard (backend-featurep :sparc-64))
  (:generator 3
    (inst mulx r x y)))


;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(deftype integer-with-a-bite-out (s bite)
  (cond ((eq s '*) 'integer)
	((and (integerp s) (> s 1))
	 (let ((bound (ash 1 (1- s))))
	   `(integer ,(- bound) ,(- bound bite 1))))
	(t
	 (error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg zero))
	 (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg zero)))
  (:arg-types tagged-num (:constant (signed-byte #.(- 13 vm:fixnum-tag-bits))))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg zero))
	 (y :scs (signed-reg zero)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg zero)))
  (:arg-types signed-num (:constant (signed-byte 13)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg zero))
	 (y :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg zero)))
  (:arg-types unsigned-num (:constant (unsigned-byte 12)))
  (:info target not-p y))


(defmacro define-conditional-vop (tran cond unsigned not-cond not-unsigned)
  `(progn
     ,@(mapcar #'(lambda (suffix cost signed)
		   (unless (and (member suffix '(/fixnum -c/fixnum))
				(eq tran 'eql))
		     `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
						    tran suffix))
				   ,(intern
				     (format nil "~:@(FAST-CONDITIONAL~A~)"
					     suffix)))
			(:translate ,tran)
			(:generator ,cost
			  (inst cmp x
				,(if (eq suffix '-c/fixnum) '(fixnumize y) 'y))
			  (inst b (if not-p
				      ,(if signed not-cond not-unsigned)
				      ,(if signed cond unsigned))
				target)
			  (inst nop)))))
	       '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
	       '(4 3 6 5 6 5)
	       '(t t t t nil nil))))

(define-conditional-vop < :lt :ltu :ge :geu)

(define-conditional-vop > :gt :gtu :le :leu)

(define-conditional-vop eql :eq :eq :ne :ne)

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

;;; These versions specify a fixnum restriction on their first arg.  We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost.  The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.
;;;

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg descriptor-reg zero))
	 (y :scs (any-reg zero)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmp x y)
    (inst b (if not-p :ne :eq) target)
    (inst nop)))
;;;
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg descriptor-reg zero)))
  ;; This is a signed-byte 11 because after fixnum shifting, we get a
  ;; 13-bit number, and that's the largest immediate allowed.
  (:arg-types tagged-num (:constant (signed-byte #.(- 13 vm:fixnum-tag-bits))))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst cmp x (fixnumize y))
    (inst b (if not-p :ne :eq) target)
    (inst nop)))
;;;
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  ;; This is a signed-byte 11 because after fixnum shifting, we get a
  ;; 13-bit number, and that's the largest immediate allowed.
  (:arg-types * (:constant (signed-byte #.(- 13 vm:fixnum-tag-bits))))
  (:variant-cost 6))


;;;; 32-bit logical operations

(define-vop (merge-bits)
  (:translate merge-bits)
  (:args (shift :scs (signed-reg unsigned-reg))
	 (prev :scs (unsigned-reg))
	 (next :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:temporary (:scs (unsigned-reg) :to (:result 0) :target result) res)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 4
    (let ((done (gen-label)))
      (inst cmp shift)
      (inst b :eq done)
      (inst srln res next shift)
      (inst sub temp zero-tn shift)
      (inst slln temp prev temp)
      (inst or res temp)
      (emit-label done)
      (move result res))))


(define-vop (32bit-logical)
  (:args (x :scs (unsigned-reg zero))
	 (y :scs (unsigned-reg zero)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe))

(define-vop (32bit-logical-not 32bit-logical)
  (:translate 32bit-logical-not)
  (:args (x :scs (unsigned-reg zero)))
  (:arg-types unsigned-num)
  (:generator 1
    (inst not r x)))

(define-vop (32bit-logical-and 32bit-logical)
  (:translate 32bit-logical-and)
  (:generator 1
    (inst and r x y)))

(deftransform 32bit-logical-nand ((x y) (* *))
  '(32bit-logical-not (32bit-logical-and x y)))

(define-vop (32bit-logical-or 32bit-logical)
  (:translate 32bit-logical-or)
  (:generator 1
    (inst or r x y)))

(deftransform 32bit-logical-nor ((x y) (* *))
  '(32bit-logical-not (32bit-logical-or x y)))

(define-vop (32bit-logical-xor 32bit-logical)
  (:translate 32bit-logical-xor)
  (:generator 1
    (inst xor r x y)))

(define-vop (32bit-logical-eqv 32bit-logical)
  (:translate 32bit-logical-eqv)
  (:generator 1
    (inst xnor r x y)))

(define-vop (32bit-logical-orc2 32bit-logical)
  (:translate 32bit-logical-orc2)
  (:generator 1
    (inst orn r x y)))

(deftransform 32bit-logical-orc1 ((x y) (* *))
  '(32bit-logical-orc2 y x))

(define-vop (32bit-logical-andc2 32bit-logical)
  (:translate 32bit-logical-andc2)
  (:generator 1
    (inst andn r x y)))

(deftransform 32bit-logical-andc1 ((x y) (* *))
  '(32bit-logical-andc2 y x))


(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg))
	 (amount :scs (signed-reg)))
  (:arg-types unsigned-num tagged-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "shift-towards-start")
  (:generator 1
    (inst slln r num amount)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "shift-towards-end")
  (:generator 1
    (inst srln r num amount)))




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
	 (index :scs (any-reg immediate zero))
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
  (:guard (not (backend-featurep :sparc-v9)))
  (:generator 3
    (let ((done (gen-label)))
      (inst cmp digit)
      (inst b :lt done)
      (move result null-tn)
      (load-symbol result t)
      (emit-label done))))

(define-vop (v9-digit-0-or-plus-cmove)
  (:translate bignum::%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:guard (backend-featurep :sparc-v9))
  (:generator 3
    (inst cmp digit)
    (load-symbol result t)
    (inst cmove :lt result null-tn)))

;; This doesn't work?
#+nil
(define-vop (v9-digit-0-or-plus-movr)
  (:translate bignum::%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:guard (backend-featurep :sparc-v9))
  (:generator 2
    (load-symbol temp t)
    (inst movr result null-tn digit :lz)
    (inst movr result temp digit :gez)))


(define-vop (add-w/carry)
  (:translate bignum::%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
	    (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 3
    (inst addcc zero-tn c -1)
    (inst addxcc result a b)
    (inst addx carry zero-tn zero-tn)))

(define-vop (sub-w/borrow)
  (:translate bignum::%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg))
	 (b :scs (unsigned-reg))
	 (c :scs (any-reg)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg))
	    (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (inst subcc zero-tn c 1)
    (inst subxcc result a b)
    (inst addx borrow zero-tn zero-tn)
    (inst xor borrow 1)))

;;; EMIT-MULTIPLY -- This is used both for bignum stuff and in assembly
;;; routines.
;;; 
(defun emit-multiply (multiplier multiplicand result-high result-low)
  "Emit code to multiply MULTIPLIER with MULTIPLICAND, putting the result
  in RESULT-HIGH and RESULT-LOW.  KIND is either :signed or :unsigned.
  Note: the lifetimes of MULTIPLICAND and RESULT-HIGH overlap."
  (declare (type tn multiplier result-high result-low)
	   (type (or tn (signed-byte 13)) multiplicand))
  ;; It seems that emit-multiply is only used to do an unsigned
  ;; multiply, so the code only does an unsigned multiply.
  (cond ((backend-featurep :sparc-64)
	 ;; Take advantage of V9's 64-bit multiplier.
	 ;;
	 ;; Make sure the multiplier and multiplicand are really
	 ;; unsigned 64-bit numbers.
	 (inst clruw multiplier)
	 (inst clruw multiplicand)
	 
	 ;; Multiply the two numbers and put the result in
	 ;; result-high.  Copy the low 32-bits to result-low.  Then
	 ;; shift result-high so the high 32-bits end up in the low
	 ;; 32-bits.
	 (inst mulx result-high multiplier multiplicand)
	 (inst move result-low result-high)
	 (inst srax result-high 32))
	((or (backend-featurep :sparc-v8)
	     (backend-featurep :sparc-v9))
	 ;; V8 has a multiply instruction.  This should also work for
	 ;; the V9, but umul and the Y register is deprecated on the
	 ;; V9.
	 (inst umul result-low multiplier multiplicand)
	 (inst rdy result-high))
	(t
	 (let ((label (gen-label)))
	   (inst wry multiplier)
	   (inst andcc result-high zero-tn)
	   ;; Note: we can't use the Y register until three insts
	   ;; after it's written.
	   (inst nop)
	   (inst nop)
	   (dotimes (i 32)
	     (inst mulscc result-high multiplicand))
	   (inst mulscc result-high zero-tn)
	   (inst cmp multiplicand)
	   (inst b :ge label)
	   (inst nop)
	   (inst add result-high multiplier)
	   (emit-label label)
	   (inst rdy result-low)))))

(define-vop (bignum-mult-and-add-3-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
	 (y :scs (unsigned-reg) :to (:eval 1))
	 (carry-in :scs (unsigned-reg) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
	    (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)
    (inst addcc lo carry-in)
    (inst addx hi zero-tn)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate bignum::%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:eval 1))
	 (y :scs (unsigned-reg) :to (:eval 1))
	 (prev :scs (unsigned-reg) :to (:eval 2))
	 (carry-in :scs (unsigned-reg) :to (:eval 2)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg) :from (:eval 0))
	    (lo :scs (unsigned-reg) :from (:eval 1)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)
    (inst addcc lo carry-in)
    (inst addx hi zero-tn)
    (inst addcc lo prev)
    (inst addx hi zero-tn)))

(define-vop (bignum-mult)
  (:translate bignum::%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :to (:result 1))
	 (y :scs (unsigned-reg) :to (:result 1)))
  (:arg-types unsigned-num unsigned-num)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 40
    (emit-multiply x y hi lo)))

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
  (:args (fixnum :scs (any-reg)))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst sran digit fixnum fixnum-tag-bits)))

(define-vop (bignum-floor)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target rem)
	 (div-low :scs (unsigned-reg) :target quo)
	 (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from (:argument 1))
	    (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:guard (not (or (backend-featurep :sparc-v8)
		   (backend-featurep :sparc-v9))))
  (:generator 300
    (move rem div-high)
    (move quo div-low)
    (dotimes (i 33)
      (let ((label (gen-label)))
	(inst cmp rem divisor)
	(inst b :ltu label)
	(inst addxcc quo quo)
	(inst sub rem divisor)
	(emit-label label)
	(unless (= i 32)
	  (inst addx rem rem))))
    (inst not quo)))

(define-vop (bignum-floor-v8)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target rem)
	 (div-low :scs (unsigned-reg) :target quo)
	 (divisor :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:results (quo :scs (unsigned-reg) :from (:argument 1))
	    (rem :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num unsigned-num)
  (:temporary (:scs (unsigned-reg) :target quo) q)
  ;; This vop is for a v8 or v9, provided we're also not using
  ;; sparc-64, for which there a special sparc-64 vop.
  (:guard (or (backend-featurep :sparc-v8)
	      (and (backend-featurep :sparc-v9)
		   (not (backend-featurep :sparc-64)))))
  (:generator 15
    (inst wry div-high)
    (inst nop)
    (inst nop)
    (inst nop)
    ;; Compute the quotient [Y, div-low] / divisor
    (inst udiv q div-low divisor)
    ;; Compute the remainder.  The high part of the result is in the Y
    ;; register.
    (inst umul rem q divisor)
    (inst sub rem div-low rem)
    (unless (location= quo q)
      (move quo q))))

(define-vop (bignum-floor-v9)
  (:translate bignum::%floor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg))
	 (div-low :scs (unsigned-reg))
	 (divisor :scs (unsigned-reg) :to (:result 1)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :from (:argument 0)) dividend)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:guard (backend-featurep :sparc-64))
  (:generator 5
    ;; Set dividend to be div-high and div-low	      
    (inst sllx dividend div-high 32)
    (inst add dividend div-low)
    ;; Compute quotient
    (inst udivx quo dividend divisor)
    ;; Compute the remainder
    (inst mulx rem quo divisor)
    (inst sub rem dividend rem)))

(define-vop (signify-digit)
  (:translate bignum::%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (sc-case res
      (any-reg
       (inst slln res digit fixnum-tag-bits))
      (signed-reg
       (move res digit)))))


(define-vop (digit-ashr)
  (:translate bignum::%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg))
	 (count :scs (signed-reg unsigned-reg immediate)))
  (:arg-types unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (sc-case count
      ((signed-reg unsigned-reg)
       (inst sran result digit count))
      (immediate
       (inst sran result digit (tn-value count))))))

(define-vop (digit-lshr digit-ashr)
  (:translate bignum::%digit-logical-shift-right)
  (:generator 1
    (sc-case count
      ((signed-reg unsigned-reg)
       (inst srln result digit count))
      (immediate
       (inst srln result digit (tn-value count))))))

(define-vop (digit-ashl digit-ashr)
  (:translate bignum::%ashl)
  (:generator 1
    (sc-case count
      ((signed-reg unsigned-reg)
       (inst slln result digit count))
      (immediate
       (inst slln result digit (tn-value count))))))


;;;; Static functions.

(define-static-function two-arg-gcd (x y) :translate gcd)
(define-static-function two-arg-lcm (x y) :translate lcm)

(define-static-function two-arg-+ (x y) :translate +)
(define-static-function two-arg-- (x y) :translate -)
(define-static-function two-arg-* (x y) :translate *)
(define-static-function two-arg-/ (x y) :translate /)

(define-static-function two-arg-< (x y) :translate <)
(define-static-function two-arg-<= (x y) :translate <=)
(define-static-function two-arg-> (x y) :translate >)
(define-static-function two-arg->= (x y) :translate >=)
(define-static-function two-arg-= (x y) :translate =)
(define-static-function two-arg-/= (x y) :translate /=)

(define-static-function %negate (x) :translate %negate)

(define-static-function two-arg-and (x y) :translate logand)
(define-static-function two-arg-ior (x y) :translate logior)
(define-static-function two-arg-xor (x y) :translate logxor)


;; Truncation by a constant can be done by multiplication with the
;; appropriate constant.
;;
;; See generic/vm-tran.lisp for the algorithm.

(define-vop (signed-truncate-by-mult fast-signed-binop)
  (:translate truncate)
  (:args (x :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num (:constant (integer 2 #.(1- (ash 1 vm:word-bits)))))
  (:results (quo :scs (signed-reg))
            (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:guard (or (backend-featurep :sparc-v8)
              (and (backend-featurep :sparc-v9)
                   (not (backend-featurep :sparc-64)))))
  (:temporary (:scs (signed-reg)) q)
  (:temporary (:scs (signed-reg)) r)
  (:temporary (:scs (signed-reg)) temp)
  (:generator 6
    (multiple-value-bind (recip shift)
        (c::find-signed-reciprocal y vm:word-bits)
      ;; Compute q = floor(M*n/2^32).  That is, the high half of the
      ;; product.
      (inst li temp recip)
      (inst smul r x temp)
      (inst rdy q)
      ;; Adjust if the M is negative.
      (when (minusp recip)
        (inst add q x))
      ;; Shift quotient as needed.
      (unless (zerop shift)
        (inst sran q shift))
      ;; Add one to quotient if X is negative.  This is done by right
      ;; shifting X to give either -1 or 0.  Then subtract this from
      ;; the quotient.  (NOTE: in the book, the sample code has this
      ;; wrong and ADDS instead of SUBTRACTS.)
      (inst sran temp x 31)
      (inst sub q q temp)
      ;; Now compute remainder via r = x - q*d
      (cond ((typep y '(signed-byte 13))
             (inst smul r q y))
            (t
             (inst li temp y)
             (inst smul r q temp)))
      (inst sub rem x r)
      (unless (location= quo q)
        (move quo q)))))

(define-vop (unsigned-truncate-by-mult fast-signed-binop)
  (:translate truncate)
  (:args (x :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num (:constant (integer 2 #.(1- (ash 1 vm:word-bits)))))
  (:results (quo :scs (unsigned-reg))
            (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:guard (or (backend-featurep :sparc-v8)
              (and (backend-featurep :sparc-v9)
                   (not (backend-featurep :sparc-64)))))
  (:temporary (:scs (unsigned-reg)) q)
  (:temporary (:scs (unsigned-reg)) r)
  (:temporary (:scs (unsigned-reg)) temp)
  (:generator 6
    (multiple-value-bind (recip shift overflowp)
        (c::find-unsigned-reciprocal y vm:word-bits)
      ;; q = floor(M*x/2^32)
      (inst li temp recip)
      (inst umul r x temp)
      (inst rdy q)
      (cond (overflowp
             ;; The complicated case.  Sparc does not have a
             ;; shift-with-carry instruction, so we use idea above.
             ;; (But we could also do this by putting the carry flag
             ;; into a register, shifting it left some amount,
             ;; shifting the quotient right, and adding the two
             ;; results together.)
             (inst sub temp x q)
             (inst srln temp 1)
             (inst add temp q)
             (inst srln q temp (1- shift)))
            (t
             ;; The easy case
             (unless (zerop shift)
               (inst srln q shift))))
      ;; Compute the remainder
      (cond ((typep y '(signed-byte 13))
             (inst umul r q y))
            (t
             (inst li temp y)
             (inst umul r q temp)))
      (inst sub rem x r)
      (unless (location= quo q)
        (move quo q)))))

;; Need these so constant folding works with the deftransform.

#-(and sparc-v9 sparc-v8plus)
(progn
(defun ash-right-signed (num shift)
  (declare (type (signed-byte #.vm:word-bits) num)
	   (type (integer 0 #.(1- vm:word-bits)) shift))
  (ash num (- shift)))

(defun ash-right-unsigned (num shift)
  (declare (type (unsigned-byte #.vm:word-bits) num)
	   (type (integer 0 #.(1- vm:word-bits)) shift))
  (ash num (- shift)))
)

#+(and sparc-v9 sparc-v8plus)
(progn
(defun ash-right-signed (num shift)
  (declare (type (signed-byte #.(* 2 vm:word-bits)) num)
	   (type (integer 0 #.(1- (* 2 vm:word-bits))) shift))
  (ash num (- shift)))

(defun ash-right-unsigned (num shift)
  (declare (type (unsigned-byte #.(* 2 vm:word-bits)) num)
	   (type (integer 0 #.(1- (* 2 vm:word-bits))) shift))
  (ash num (- shift)))
)

;; If we can prove that we have a right shift, just do the right shift
;; instead of calling the inline ASH which has to check for the
;; direction of the shift at run-time.
(in-package "C")

#-(and sparc-v9 sparc-v8plus)
(deftransform ash ((num shift) (integer integer))
  (let ((num-type (continuation-type num))
	(shift-type (continuation-type shift)))
    ;; Can only handle right shifts
    (unless (csubtypep shift-type (specifier-type '(integer * 0)))
      (give-up))

    ;; If we can prove the shift is so large that all bits are shifted
    ;; out, return the appropriate constant.  If the shift is small
    ;; enough, call the VOP.  Otherwise, check for the shift size and
    ;; do the appropriate thing.  (Hmm, could we just leave the IF
    ;; s-expr and depend on other parts of the compiler to delete the
    ;; unreachable parts, if any?)
    (cond ((csubtypep num-type (specifier-type '(signed-byte #.vm:word-bits)))
	   ;; A right shift by 31 is the same as a right shift by
	   ;; larger amount.  We get just the sign.
	   (if (csubtypep shift-type (specifier-type '(integer #.(- 1 vm:word-bits) 0)))
	       `(sparc::ash-right-signed num (- shift))
	       `(sparc::ash-right-signed num (min (- shift) #.(1- vm:word-bits)))))
	  ((csubtypep num-type (specifier-type '(unsigned-byte #.vm:word-bits)))
	   (if (csubtypep shift-type (specifier-type '(integer #.(- 1 vm:word-bits) 0)))
	       `(sparc::ash-right-unsigned num (- shift))
	       `(if (<= shift #.(- vm:word-bits))
		 0
		 (sparc::ash-right-unsigned num (- shift)))))
	  (t
	   (give-up)))))

#+(and later sparc-v9 sparc-v8plus)
(deftransform ash ((num shift) (integer integer))
  (let ((num-type (continuation-type num))
	(shift-type (continuation-type shift)))
    ;; Can only handle right shifts
    (unless (csubtypep shift-type (specifier-type '(integer * 0)))
      (give-up))

    ;; If we can prove the shift is so large that all bits are shifted
    ;; out, return the appropriate constant.  If the shift is small
    ;; enough, call the VOP.  Otherwise, check for the shift size and
    ;; do the appropriate thing.  (Hmm, could we just leave the IF
    ;; s-expr and depend on other parts of the compiler to delete the
    ;; unreachable parts, if any?)
    (cond ((csubtypep num-type (specifier-type '(signed-byte #.(* 2 vm:word-bits))))
	   ;; A right shift by 31 is the same as a right shift by
	   ;; larger amount.  We get just the sign.
	   (if (csubtypep shift-type (specifier-type '(integer #.(- 1 (* 2 vm:word-bits)) 0)))
	       `(sparc::ash-right-signed num (- shift))
	       `(sparc::ash-right-signed num (min (- shift) #.(1- (* 2 vm:word-bits))))))
	  ((csubtypep num-type (specifier-type '(unsigned-byte #.(* 2 vm:word-bits))))
	   (if (csubtypep shift-type (specifier-type '(integer #.(- 1 (* 2 vm:word-bits)) 0)))
	       `(sparc::ash-right-unsigned num (- shift))
	       `(if (<= shift #.(- vm:word-bits))
		 0
		 (sparc::ash-right-unsigned num (- shift)))))
	  (t
	   (give-up)))))


;;; 64-bit integer operations
(in-package "SPARC")

;; Unary operations

#+(and sparc-v9 sparc-v8plus)
(progn

;;; The vops for the 64-bit operations are written this way because I
;;; (RLT) can't figure out how to get representation selection to
;;; convert signed-num and unsigned-num to signed64-num when
;;; needed. (Actually, I don't even know how representation selection
;;; works.)
;;;
;;; Thus, any arg that is a 64-bit register defines the SCS to include
;;; the 64-bit reg and well as the 32-bit regs.  Then the generators
;;; will have to dispatch on the sc type that is actually used.  This
;;; probably generates lots of redundant moves, but we still win big
;;; by not having to do bignum arithmetic.
;;;
;;; I think the dispatch could be simplified if we just sign or zero
;;; extended the 32-bit registers in place.  However, we can't do that
;;; in general because not all of the 32-bit regs preserve all 64-bits
;;; on task switches.  If we constrain the 32-bit regs to be the same
;;; 6 regs used for the 64-bit regs, we run out of regs when do
;;; foreign function calls because we need 6 regs for that and we
;;; don't have any more non-descriptor regs for temps.


(define-vop (signed64-unop fast-safe-arith-op)
  (:args (x :target res :scs (signed64-reg signed-reg unsigned-reg zero)))
  (:results (res :scs (signed64-reg)))
  (:arg-types (:or signed64-num signed-num unsigned-num))
  (:result-types signed64-num)
  (:temporary (:scs (signed64-reg)) x64)
  (:note "inline (signed-byte 64) arithmetic"))

(define-vop (fast-signed64-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed64-reg unsigned-reg signed-reg zero))
	 (y :target r :scs (signed64-reg unsigned-reg signed-reg zero)))
  (:arg-types (:or signed64-num signed-num unsigned-num)
	      (:or signed64-num signed-num unsigned-num))
  (:results (r :scs (signed64-reg)))
  (:result-types signed64-num)
  (:note "inline (signed-byte 64) arithmetic"))

(define-vop (fast-signed64-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed64-reg unsigned-reg signed-reg zero)))
  (:info y)
  (:arg-types (:or signed64-num signed-num unsigned-num)
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (signed64-reg)))
  (:result-types signed64-num)
  (:note "inline (signed-byte 64) arithmetic"))

(define-vop (fast-unsigned64-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned64-reg unsigned-reg zero))
	 (y :target r :scs (unsigned64-reg unsigned-reg zero)))
  (:arg-types (:or unsigned64-num unsigned-num)
	      (:or unsigned64-num unsigned-num))
  (:results (r :scs (unsigned64-reg)))
  (:result-types unsigned64-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

(define-vop (fast-unsigned64-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned64-reg unsigned-reg zero)))
  (:info y)
  (:arg-types (:or unsigned64-num unsigned-num)
	      (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (unsigned64-reg)))
  (:result-types unsigned64-num)
  (:note "inline (unsigned-byte 64) arithmetic"))

;; Extend the sign of Y appropriately and perform the desired
;; operation. R is where the result should go, X is the first arg, Y
;; is the second, and Y64 is a 64-bit temp reg for us to use.
(defmacro sc-case-64 (r x y y64 op)
  `(if (tn-p ,y)
     (sc-case ,y
       ((signed64-reg unsigned64-reg)
	(inst ,op ,r ,x ,y))
       (signed-reg
	;; Sign-extend Y
	(inst signx ,y64 ,y)
	(inst ,op ,r ,x ,y64))
       (unsigned-reg
	;; Zero-extend Y
	(inst clruw ,y64 ,y)
	(inst ,op ,r ,x ,y64))
       (immediate
	(inst ,op ,r ,x ,y)))
      (inst ,op ,r ,x ,y)))

;; Same as above, but we only have one arg.
(defmacro sc-case-64-one-arg (r x x64 op)
  `(sc-case ,x
     ((signed64-reg unsigned64-reg)
      (inst ,op ,r ,x))
     (signed-reg
      ;; Sign-extend X
      (inst signx ,x64 ,x)
      (inst ,op ,r ,x64))
     (unsigned-reg
      ;; Zero-extend X
      (inst clruw ,x64 ,x)
      (inst ,op ,r ,x64))))
  
;; Try all possible combinations of signed64-reg, signed-reg, and
;; unsigned-reg and appropriately extend the sign of the shorter
;; operand and perform the desired operation on the 64-bit operands
;; with a 64-bit result.
;;
;; FIXME: This should be more like numeric-dispatch so it can be used
;; in other places.
(defmacro sc-dispatch (r x y x64 y64 op)
  `(sc-case x
     ((signed64-reg unsigned64-reg)
      ;; X is 64-bits
      (sc-case-64 ,r ,x ,y ,y64 ,op))
     (signed-reg
      ;; Sign-extend X
      (inst signx ,x64 ,x)
      (sc-case-64 ,r ,x64 ,y ,y64 ,op))
     (unsigned-reg
      ;; Zero-extend X
      (inst clruw ,x64 ,x)
      (sc-case-64 ,r ,x64 ,y ,y64 ,op))))

;; Operations that result in 64-bit results.

(defmacro define-binop-64 (translate op)
  `(progn
    (define-vop (,(symbolicate "FAST-" translate "/SIGNED64")
		 fast-signed64-binop)
      (:translate ,translate)
      (:temporary (:scs (signed64-reg)) x64 y64)
      (:generator 2
       (sc-dispatch r x y x64 y64 ,op)))
    (define-vop (,(symbolicate "FAST-" translate "/UNSIGNED64")
		 fast-unsigned64-binop)
      (:translate ,translate)
      (:temporary (:scs (signed64-reg)) x64 y64)
      (:generator 2
       (sc-dispatch r x y x64 y64 ,op)))
    (define-vop (,(symbolicate "FAST-" translate "-C/SIGNED64")
		 fast-signed64-binop-c)
      (:translate ,translate)
      (:temporary (:scs (signed64-reg)) x64 y64)
      (:generator 2
       (sc-dispatch r x y x64 y64 ,op)))
    (define-vop (,(symbolicate "FAST-" translate "-C/UNSIGNED64")
		 fast-unsigned64-binop-c)
      (:translate ,translate)
      (:temporary (:scs (signed64-reg)) x64 y64)
      (:generator 2
       (sc-dispatch r x y x64 y64 ,op)))
    ))

(define-binop-64 + add)
(define-binop-64 - sub)
(define-binop-64 logand and)
(define-binop-64 logandc2 andn)
(define-binop-64 logior or)
(define-binop-64 logorc2 orn)
(define-binop-64 logxor xor)
(define-binop-64 logeqv xnor)

;; Some special cases for logand.
(define-vop (fast-logand/signed64 fast-safe-arith-op)
  (:args (x :target r :scs (signed64-reg signed-reg zero))
	 (y :target r :scs (signed64-reg signed-reg zero)))
  (:arg-types (:or signed64-num signed-num)
	      (:or signed64-num signed-num))
  (:results (r :scs (signed64-reg)))
  (:result-types signed64-num)
  (:translate logand)
  (:temporary (:scs (signed64-reg)) x64 y64)
  (:generator 2
    (sc-dispatch r x y x64 y64 and)))

(define-vop (fast-logand/u32-64=>signed fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero))
	 (y :target r :scs (signed64-reg unsigned64-reg signed-reg  zero)))
  (:arg-types (:or unsigned-num)
	      (:or signed64-num unsigned64-num signed-num))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate logand)
  (:generator 2
    (sc-case y
      ((signed64-reg unsigned64-reg)
       (inst clruw r x)			; Zero-extend, just in case
       (inst and r y)
       (inst clruw r)			; Zero-extend, just in case
       )
      (signed-reg
       (inst and r x y)
       ))))


(define-vop (fast-logand/64-u32=>signed fast-safe-arith-op)
  (:args (x :target r :scs (signed64-reg unsigned64-reg signed-reg zero))
	 (y :target r :scs (unsigned-reg zero)))
  (:arg-types (:or signed64-num unsigned64-num signed-num)
	      (:or unsigned-num))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate logand)
  (:generator 2
    (sc-case x
      ((signed64-reg unsigned64-reg)
       (inst clruw r y)			; Zero-extend, just in case
       (inst and r x y)
       (inst clruw r)			; Zero-extend, just in case
       )
      (signed-reg
       (inst and r x y)
       ))))

(define-vop (fast-lognot/signed64 signed64-unop)
  (:translate lognot)
  (:generator 2
    (sc-case-64-one-arg res x x64 not)))

(define-vop (fast-negate/signed64 signed64-unop)
  (:args (x :target res :scs (signed64-reg zero)))
  (:arg-types signed64-num)
  (:results (res :scs (signed64-reg)))
  (:result-types signed64-num)
  (:translate %negate)
  (:generator 1
    (inst neg res x)))

(define-vop (fast-negate/unsigned=>signed64 fast-safe-arith-op)
  (:args (x :target res :scs (unsigned-reg zero)))
  (:arg-types unsigned-num)
  (:results (res :scs (signed64-reg)))
  (:result-types signed64-num)
  (:translate %negate)
  (:generator 1
    ;; Zero-extend	      
    (inst clruw res x)
    (inst neg res)))

(define-vop (fast-negate/signed=>signed64 fast-safe-arith-op)
  (:args (x :target res :scs (signed-reg zero)))
  (:arg-types signed-num)
  (:results (res :scs (signed64-reg)))
  (:result-types signed64-num)
  (:translate %negate)
  (:generator 1
    ;; Sign-extend	      
    (inst signx res x)
    (inst neg res)))

;; Signed 64x64->64 bit multiply
(define-vop (fast-*/signed64 fast-safe-arith-op)
  (:args (x :target r :scs (signed64-reg unsigned64-reg unsigned-reg signed-reg zero))
	 (y :target r :scs (signed64-reg unsigned64-reg unsigned-reg signed-reg zero)))
  (:arg-types (:or signed64-num unsigned64-num signed-num unsigned-num)
	      (:or signed64-num unsigned64-num signed-num unsigned-num))
  (:results (r :scs (signed64-reg unsigned64-reg)))
  (:result-types signed64-num)
  (:temporary (:scs (signed64-reg)) x64 y64)
  (:translate *)
  (:note "inline (signed-byte 64) arithmetic")
  (:guard (backend-featurep :sparc-v9))
  (:generator 3
    (sc-dispatch r x y x64 y64 mulx)))

(define-vop (fast-*/unsigned64 fast-safe-arith-op)
  (:args (x :target r :scs (signed64-reg unsigned64-reg unsigned-reg signed-reg zero))
	 (y :target r :scs (signed64-reg unsigned64-reg unsigned-reg signed-reg zero)))
  (:arg-types (:or signed64-num unsigned64-num signed-num unsigned-num)
	      (:or signed64-num unsigned64-num signed-num unsigned-num))
  (:results (r :scs (signed64-reg unsigned64-reg)))
  (:result-types unsigned64-num)
  (:temporary (:scs (signed64-reg)) x64 y64)
  (:translate *)
  (:note "inline (signed-byte 64) arithmetic")
  (:guard (backend-featurep :sparc-v9))
  (:generator 3
    (sc-dispatch r x y x64 y64 mulx)))


;; Signed 32x32->64 multiply 
(define-vop (fast-*/signed=>signed64 fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg unsigned-reg zero))
	 (y :target r :scs (signed-reg unsigned-reg zero)))
  (:arg-types (:or signed-num unsigned-num)
	      (:or signed-num unsigned-num))
  (:results (r :scs (signed64-reg unsigned64-reg)))
  (:result-types signed64-num)
  (:temporary (:scs (signed64-reg)) x64 y64)
  (:translate *)
  (:note "inline (signed-byte 64) arithmetic")
  (:guard (backend-featurep :sparc-v9))
  (:generator 3
    (sc-dispatch r x y x64 y64 mulx)))

;; Unsigned 32x32->64 multiply
(define-vop (fast-*/u32=>unsigned64 fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg zero))
	 (y :target r :scs (unsigned-reg zero)))
  (:arg-types (:or unsigned-num)
	      (:or unsigned-num))
  (:results (r :scs (signed64-reg unsigned64-reg)))
  (:result-types unsigned64-num)
  (:temporary (:scs (unsigned64-reg)) x64 y64)
  (:translate *)
  (:note "inline (unsigned-byte 64) arithmetic")
  (:guard (backend-featurep :sparc-v9))
  (:generator 3
    (sc-dispatch r x y x64 y64 mulx)))

(define-vop (fast-ash/signed64=>signed64)
  (:note "inline (signed-byte 64) ASH")
  (:args (number :scs (signed64-reg unsigned-reg signed-reg) :to :save)
	 (amount :scs (signed64-reg signed-reg unsigned-reg) :to :save))
  (:arg-types (:or signed64-num signed-num unsigned-num)
	      (:or signed64-num signed-num unsigned-num))
  (:results (result :scs (signed64-reg)))
  (:result-types signed64-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:sc signed64-reg) ndesc)
  (:temporary (:scs (signed64-reg)) num64)
  (:guard (and (backend-featurep :sparc-v9)
	       (backend-featurep :sparc-v8plus)
	       (not (backend-featurep :sparc-64))))
  (:generator 5
    (sc-case number
      (signed64-reg
       (move num64 number))
      (signed-reg
       (inst signx num64 number))
      (unsigned-reg
       (inst clruw num64 number)))
    (sc-case amount
      (signed64-reg
       (let ((done (gen-label)))
	 (inst cmp amount)
	 (inst b :ge done :pt :xcc)
	 ;; The result-type assures us that this shift will not
	 ;; overflow.
	 (inst sllx result num64 amount)
	 (inst neg ndesc amount)
	 ;; ndesc = max(-amount, 31)
	 (inst cmp ndesc 31)
	 (inst cmove :ge ndesc 31 :xcc)
	 (inst srax result num64 ndesc)
	 (emit-label done)))
      (signed-reg
       (let ((done (gen-label)))
	 (inst cmp amount)
	 (inst b :ge done :pt)
	 ;; The result-type assures us that this shift will not
	 ;; overflow.
	 (inst sllx result num64 amount)
	 (inst neg ndesc amount)
	 ;; ndesc = max(-amount, 31)
	 (inst cmp ndesc 31)
	 (inst cmove :ge ndesc 31)
	 (inst srax result num64 ndesc)
	 (emit-label done)))
      (unsigned-reg
       (inst sllx result num64 amount))

      (immediate
       (let ((amount (tn-value amount)))
	 (cond ((< amount -63)
		(inst li result -1))
	       ((< amount 0)
		(inst srax result num64 (- amount)))
	       ((> amount 0)
		(inst sllx result num64 amount))
	       (t
		;; amount = 0.  Shouldn't happen because of a
		;; deftransform, but it's easy.
		(move result num64))))))))

(define-vop (fast-ash/unsigned64=>unsigned64)
  (:note "inline (signed-byte 64) ASH")
  (:args (number :scs (unsigned64-reg unsigned-reg) :to :save)
	 (amount :scs (signed64-reg signed-reg unsigned-reg immediate) :to :save))
  (:arg-types (:or unsigned64-num unsigned-num)
	      (:or signed64-num signed-num unsigned-num))
  (:results (result :scs (unsigned64-reg)))
  (:result-types unsigned64-num)
  (:translate ash)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned64-reg)) ndesc num64)
  (:guard (and (backend-featurep :sparc-v9)
	       (backend-featurep :sparc-v8plus)
	       (not (backend-featurep :sparc-64))))
  (:generator 5
    (sc-case number
      (unsigned64-reg
       (move num64 number))
      (unsigned-reg
       (inst clruw num64 number)))
    (sc-case amount
      (signed64-reg
       (let ((done (gen-label)))
	 (inst cmp amount)
	 (inst b :ge done :pt :xcc)
	 ;; The result-type assures us that this shift will not
	 ;; overflow.
	 (inst sllx result num64 amount)
	 (inst neg ndesc amount)
	 ;; ndesc = max(-amount, 31)
	 (inst cmp ndesc 31)
	 (inst cmove :ge ndesc 31 :xcc)
	 (inst srlx result num64 ndesc)
	 (emit-label done)))
      (signed-reg
       (let ((done (gen-label)))
	 (inst cmp amount)
	 (inst b :ge done :pt)
	 ;; The result-type assures us that this shift will not
	 ;; overflow.
	 (inst sllx result num64 amount)
	 (inst neg ndesc amount)
	 ;; ndesc = max(-amount, 31)
	 (inst cmp ndesc 31)
	 (inst cmove :ge ndesc 31)
	 (inst srlx result num64 ndesc)
	 (emit-label done)))
      (unsigned-reg
       (inst sllx result num64 amount))

      (immediate
       (let ((amount (tn-value amount)))
	 (cond ((< amount -63)
		(inst li result -1))
	       ((< amount 0)
		(inst srlx result num64 (- amount)))
	       ((> amount 0)
		(inst sllx result num64 amount))
	       (t
		;; amount = 0.  Shouldn't happen because of a
		;; deftransform, but it's easy.
		(move result num64))))))))



;;; Conditional operations on 64-bit numbers

(define-vop (fast-conditional-c/64-fixnum fast-conditional)
  (:args (x :scs (signed64-reg unsigned-reg signed-reg zero)))
  (:arg-types (:or signed64-num unsigned-num signed-num)
	      (:constant (signed-byte #.(- 13 vm:fixnum-tag-bits))))
  (:info target not-p y))
	      
(define-vop (fast-conditional/signed64 fast-conditional)
  (:args (x :scs (signed64-reg unsigned-reg signed-reg zero))
	 (y :scs (signed64-reg unsigned-reg signed-reg zero)))
  (:arg-types (:or signed64-num unsigned-num signed-num)
	      (:or signed64-num unsigned-num signed-num))
  (:note "inline (signed-byte 64) comparison"))

(define-vop (fast-conditional-c/signed64 fast-conditional)
  (:args (x :scs (signed64-reg unsigned-reg signed-reg zero)))
  (:arg-types (:or signed64-num unsigned-num signed-num)
	      (:constant (signed-byte 13)))
  (:info target not-p y)
  (:note "inline (signed-byte 64) comparison"))

(define-vop (fast-conditional/unsigned64 fast-conditional)
  (:args (x :scs (unsigned64-reg zero))
	 (y :scs (unsigned64-reg zero)))
  (:arg-types (:or unsigned64-num)
	      (:or unsigned64-num))
  (:note "inline (unsigned-byte 64) comparison"))

(define-vop (fast-conditional-c/unsigned64 fast-conditional)
  (:args (x :scs (unsigned64-reg zero)))
  (:arg-types (:or unsigned64-num)
	      (:constant (unsigned-byte 12)))
  (:info target not-p y)
  (:note "inline (signed-byte 64) comparison"))

;; If I were smarter, This would be a macro like it is for the 32-bit
;; versions.  It's easier this way to see what's happening, though.
;;
(define-vop (fast-if-</signed64 fast-conditional/signed64)
  (:translate <)
  (:temporary (:scs (signed64-reg)) x64 y64)
  (:generator 6
    (sc-case x
      (signed-reg
       (inst signx x64 x))
      (unsigned-reg
       (inst clruw x64 x))
      (t
       (move x64 x)))
    (sc-case y
      (signed-reg
       (inst signx y64 y))
      (unsigned-reg
       (inst clruw y64 y))
      ((signed64-reg unsigned64-reg zero)
       (move y64 y)))

    (inst cmp x64 y64)
    (inst b (if not-p :ge :lt)
	  target :pt :xcc)
    (inst nop)))

(define-vop (fast-if->/signed64 fast-conditional/signed64)
  (:translate >)
  (:temporary (:scs (signed64-reg)) x64 y64)
  (:generator 6
    (sc-case x
      (signed-reg
       (inst signx x64 x))
      (unsigned-reg
       (inst clruw x64 x))
      (t
       (move x64 x)))
    (sc-case y
      (signed-reg
       (inst signx y64 y))
      (unsigned-reg
       (inst clruw y64 y))
      ((signed64-reg unsigned64-reg zero)
       (move y64 y)))

    (inst cmp x64 y64)
    (inst b (if not-p :le :gt)
	  target :pt :xcc)
    (inst nop)))

(define-vop (fast-if-eql/signed64 fast-conditional/signed64)
  (:translate eql)
  (:temporary (:scs (signed64-reg)) x64 y64)
  (:generator 6
    (sc-case x
      (signed-reg
       (inst signx x64 x))
      (unsigned-reg
       (inst clruw x64 x))
      (t
       (move x64 x)))
    (sc-case y
      (signed-reg
       (inst signx y64 y))
      (unsigned-reg
       (inst clruw y64 y))
      ((signed64-reg unsigned64-reg zero)
       (move y64 y)))

    (inst cmp x64 y64)
    (inst b (if not-p :ne :eq)
	  target :pt :xcc)
    (inst nop)))

(define-vop (fast-if-eql-c/signed64 fast-conditional-c/signed64)
  (:translate eql)
  (:temporary (:scs (signed64-reg)) x64)
  (:generator 6
    (sc-case x
      (signed-reg
       (inst signx x64 x))
      (unsigned-reg
       (inst clruw x64 x))
      (t
       (move x64 x)))
    (inst cmp x64 y)
    (inst b (if not-p :ne :eq)
	  target :pt :xcc)
    (inst nop)))

(define-vop (fast-if-</unsigned64 fast-conditional/unsigned64)
  (:translate <)
  (:temporary (:scs (unsigned64-reg)) x64 y64)
  (:generator 6
    (sc-case x
      (unsigned-reg
       (inst clruw x64 x))
      (t
       (move x64 x)))
    (sc-case y
      (unsigned-reg
       (inst clruw y64 y))
      ((signed64-reg unsigned64-reg zero)
       (move y64 y)))

    (inst cmp x64 y64)
    (inst b (if not-p :geu :ltu)
	  target :pt :xcc)
    (inst nop)))

(define-vop (fast-if->/unsigned64 fast-conditional/unsigned64)
  (:translate >)
  (:temporary (:scs (unsigned64-reg)) x64 y64)
  (:generator 6
    (sc-case x
      (unsigned-reg
       (inst clruw x64 x))
      (t
       (move x64 x)))
    (sc-case y
      (unsigned-reg
       (inst clruw y64 y))
      ((signed64-reg unsigned64-reg zero)
       (move y64 y)))

    (inst cmp x64 y64)
    (inst b (if not-p :leu :gtu)
	  target :pt :xcc)
    (inst nop)))

(define-vop (fast-if-eql/unsigned64 fast-conditional/unsigned64)
  (:translate eql)
  (:temporary (:scs (unsigned64-reg)) x64 y64)
  (:generator 6
    (sc-case x
      (unsigned-reg
       (inst clruw x64 x))
      (t
       (move x64 x)))
    (sc-case y
      (unsigned-reg
       (inst clruw y64 y))
      ((signed64-reg unsigned64-reg zero)
       (move y64 y)))

    (inst cmp x64 y64)
    (inst b (if not-p :ne :eq)
	  target :pt :xcc)
    (inst nop)))

(define-vop (fast-if-eql-c/unsigned64 fast-conditional-c/unsigned64)
  (:translate eql)
  (:temporary (:scs (unsigned64-reg)) x64)
  (:generator 6
    (sc-case x
      (unsigned-reg
       (inst clruw x64 x))
      (t
       (move x64 x)))
    (inst cmp x64 y)
    (inst b (if not-p :ne :eq)
	  target :pt :xcc)
    (inst nop)))
  
)

;;; Sparc implementation of modular arithmetic.
#+modular-arith
(progn
(c::define-modular-fun lognot-mod32 (x) lognot 32)

(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (inst not res x)))

;; Handle (ldb (byte 32 0) (- x)).  The (- x) gets converted to
;; (%negate x), so we build modular functions for %negate.

(c::define-modular-fun %negate-mod32 (x) kernel:%negate 32)

(define-vop (%negate-mod32/unsigned=>unsigned fast-safe-arith-op)
  (:translate %negate-mod32)
  (:args (x :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst neg res x)))

(define-vop (%negate-mod32/signed=>unsigned fast-safe-arith-op)
  (:translate %negate-mod32)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst neg res x)))

(defmacro define-modular-backend (fun &optional constantp derived)
  (let ((mfun-name (symbolicate fun '-mod32))
	(modvop (symbolicate 'fast- fun '-mod32/unsigned=>unsigned))
	(modcvop (symbolicate 'fast- fun '-mod32-c/unsigned=>unsigned))
	(vop (symbolicate 'fast- (or derived fun) '/unsigned=>unsigned))
	(cvop (symbolicate 'fast- (or derived fun) '-c/unsigned=>unsigned)))
    `(progn
       (c::define-modular-fun ,mfun-name (x y) ,fun 32)
       (define-vop (,modvop ,vop)
	 (:translate ,mfun-name))
       ,@(when constantp
	       `((define-vop (,modcvop ,cvop)
		   (:translate ,mfun-name)))))))

(define-modular-backend + t)
(define-modular-backend - t)
(define-modular-backend logxor t)
(define-modular-backend logeqv t)
(define-modular-backend logandc1)
(define-modular-backend logandc2 t)
(define-modular-backend logorc1)
(define-modular-backend logorc2 t)
(define-modular-backend * t v8-*)

(def-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))
(def-source-transform lognor (x y)
  `(lognot (logior ,x ,y)))

(defknown vm::ash-left-mod32 (integer (integer 0))
  (unsigned-byte 32)
  (foldable flushable movable))

(defknown vm::ash-mod32 (integer integer)
  (unsigned-byte 32)
  (foldable flushable movable))

(define-vop (fast-ash-left-mod32-c/unsigned=>unsigned
	     digit-ashl)
  (:translate ash-left-mod32))

(define-vop (fast-ash-mod32/unsigned=>unsigned
	     fast-ash/unsigned=>unsigned)
    (:translate ash-mod32))

)

(in-package :c)

#+modular-arith
(define-modular-fun-optimizer ash ((integer count) :width width)
  ;; The count needs to be (mod 32) because the Sparc shift
  ;; instruction takes the count modulo 32.  (NOTE: Should we make
  ;; this work on Ultrasparcs?  We could then use the sllx instruction
  ;; which takes the count mod 64.  Then a left shift of 32 or more
  ;; will produce 0 in the lower 32 bits of the register, which is
  ;; what we want.)
  (when (<= width 32)
    ;; We can do a modular shift.  If the shift is known to be a left
    ;; shift, we can use the left shift vop to get a left shift
    ;; instruction.  Otherwise, we can use the regular fast-ash vop to
    ;; get the shift.
    (let ((count-type (continuation-type count)))
      (cond ((csubtypep count-type (specifier-type '(unsigned-byte 5)))
	     (cut-to-width integer width)
	     'vm::ash-left-mod32)
	    ((csubtypep count-type (specifier-type '(integer -31 31)))
	     (cut-to-width integer width)
	     'vm::ash-mod32)
	    (t
	     ;; Do nothing
	     nil)))))

;;; If both arguments and the result are (unsigned-byte 32), try to come up
;;; with a ``better'' multiplication using multiplier recoding.  There are two
;;; different ways the multiplier can be recoded.  The more obvious is to shift
;;; X by the correct amount for each bit set in Y and to sum the results.  But
;;; if there is a string of bits that are all set, you can add X shifted by
;;; one more then the bit position of the first set bit and subtract X shifted
;;; by the bit position of the last set bit.  We can't use this second method
;;; when the high order bit is bit 31 because shifting by 32 doesn't work
;;; too well.
;;;


(defun *-transformer (y)
  (let ((y (continuation-value y)))
    (multiple-value-bind (result adds shifts)
	(strength-reduce-constant-multiply 'x y)
      (cond
        ((c::backend-featurep '(or :sparc-v9 :sparc-v8))
	 ;; This is an approximate break-even point.  It's pretty
	 ;; rough.
	 (when (> (+ adds shifts) 9)
	   (give-up)))
	(t
	 (give-up)))
      (or result 0))))  

#+modular-arith
(deftransform * ((x y)
		 ((unsigned-byte 32) (constant-argument (unsigned-byte 32)))
		 (unsigned-byte 32))
  "recode as shifts and adds"
  (*-transformer y))

#+modular-arith
(deftransform vm::*-mod32 ((x y)
		 ((unsigned-byte 32) (constant-argument (unsigned-byte 32)))
		 (unsigned-byte 32))
  "recode as shifts and adds"
  (*-transformer y))
