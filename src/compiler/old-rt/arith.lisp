;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the VM definition arithmetic VOPs for the RT.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Assume that any constant operand is the second arg...

(defmacro define-fixnum-binop (name inherits translate op &key
				    unsigned immed-op commute-op short-op
				    function)
  `(define-vop (,name ,inherits)
     (:translate ,translate)
     (:generator 1
       (sc-case y
	 ((any-reg)
	  (cond ((location= x r)
		 (inst ,op x y))
		,@(when commute-op
		    `(((location= y r)
		       (inst ,commute-op y x))))
		(t
		 (inst lr r x)
		 (inst ,op r y))))
	 ,@(when (or immed-op short-op)
	     `(((short-immediate unsigned-immediate
				 ,@(unless unsigned '(immediate)))
		(cond
		 ,@(when short-op
		     `(((and (sc-is y short-immediate)
			     (location= x r))
			(inst ,short-op r (tn-value y)))))
		 (t
		  (inst ,immed-op r x
			,(if function
			     `(,function (tn-value y))
			     '(tn-value y))))))))))))


;;;; Arithmetic:

(define-vop (fast-binop/fixnum)
  (:args (x :target r
	    :scs (any-reg))
	 (y :target r
	    :scs (any-reg short-immediate unsigned-immediate immediate)))
  (:results (r :scs (any-reg)))
  (:arg-types fixnum fixnum)
  (:result-types fixnum)
  (:effects)
  (:affected)
  (:note "inline fixnum arithmetic")
  (:policy :fast-safe))

(define-fixnum-binop fast-+/fixnum fast-binop/fixnum + a
  :short-op ais :immed-op ai :commute-op a)

(define-fixnum-binop fast--/fixnum fast-binop/fixnum - s
  :short-op sis :immed-op ai :function - :commute-op sf)


(define-vop (fixnum-unop)
  (:args (x :scs (any-reg)))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types fixnum)
  (:result-types fixnum)
  (:policy :fast-safe))

(macrolet ((frob (name inst trans)
	     `(define-vop (,name fixnum-unop)
		(:translate ,trans)
		(:generator 1
		  (inst ,inst res x)))))
  (frob fast-negate/fixnum twoc %negate)
  (frob fast-lognot/fixnum onec lognot)
  (frob fast-abs/fixnum abs abs))

(define-miscop-variants effectless-unaffected-two-arg-miscop + - / *)
(define-miscop negate (x) :translate %negate)
(define-miscop truncate (x y) :results (q r) :translate truncate)


;;;; Logic operations:

;;; Like fast-binop/fixnum, except the immediate operand is unsigned, and
;;; a fixnum result assertion isn't needed.
;;;
(define-vop (fast-logic-binop/fixnum fast-binop/fixnum)
  (:args (x :target r
	    :scs (any-reg))
	 (y :target r
	    :scs (any-reg short-immediate unsigned-immediate))))

(define-fixnum-binop fast-logior/fixnum fast-logic-binop/fixnum logior o
  :immed-op oil :unsigned t :commute-op o)

(define-fixnum-binop fast-logand/fixnum fast-logic-binop/fixnum logand n
  :immed-op nilz :unsigned t :commute-op n)

(define-fixnum-binop fast-logxor/fixnum fast-logic-binop/fixnum logxor x
  :immed-op xil :unsigned t :commute-op x)


(define-vop (fast-ash/fixnum fast-binop/fixnum)
  (:args (i :scs (any-reg)
	    :target r))
  (:arg-types fixnum (:constant (integer -31 31)))
  (:info n)
  (:translate ash)
  (:generator 1
    (unless (location= i r)
      (inst lr r i))

    (cond ((plusp n)
	   (if (> n 15)
	       (inst sli16 r (- n 16))
	       (inst sli r n)))
	  ((minusp n)
	   (let ((n (- n)))
	     (if (> n 15)
		 (inst sari16 r (- n 16))
		 (inst sari r n)))))))


(define-miscop-variants effectless-unaffected-two-arg-miscop
			logand logior logxor)
(define-miscop-variants effectless-unaffected-one-arg-miscop lognot)
(define-miscop ldb (size pos int) :translate %ldb)
(define-miscop mask-field (size pos int) :translate %mask-field)
(define-miscop dpb (new size pos int) :translate %dpb)
(define-miscop deposit-field (new size pos int) :translate %deposit-field)


;;;; Binary conditional VOPs:

(define-vop (fast-conditional/fixnum)
  (:args (x :scs (any-reg))
	 (y :scs (any-reg short-immediate unsigned-immediate immediate)))
  (:arg-types fixnum fixnum)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe)
  (:note "inline fixnum comparison")
  (:variant-vars condition)
  (:generator 1
    (sc-case y
      (short-immediate
       (inst cis x (tn-value y)))
      ((immediate unsigned-immediate)
       (inst ci x (tn-value y)))
      (any-reg
       (inst c x y)))

    (if not-p
	(inst bnb condition target)
	(inst bb condition target))))


(define-vop (fast-if-</fixnum fast-conditional/fixnum)
  (:translate <)
  (:variant :lt))

(define-vop (fast-if->/fixnum fast-conditional/fixnum)
  (:translate >)
  (:variant :gt))

(define-vop (fast-if-=/fixnum fast-conditional/fixnum)
  (:translate =)
  (:variant :eq))



(define-vop (if-< two-arg-conditional-miscop)
  (:variant 'clc::compare :lt)
  (:translate <))

(define-vop (if-> two-arg-conditional-miscop)
  (:variant 'clc::compare :gt)
  (:translate >))

(define-vop (if-= two-arg-conditional-miscop)
  (:variant 'clc::compare :eq)
  (:translate =))
