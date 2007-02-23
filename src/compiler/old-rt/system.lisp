;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    RT VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(define-vop (pointer+)
  (:args (ptr :scs (descriptor-reg))
	 (offset :scs (any-reg descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:generator 1
    (inst cas res offset ptr)))

(define-vop (sap+ pointer+)
  (:translate sap+))

(define-vop (pointer-)
  (:args (ptr1 :scs (descriptor-reg) :target temp)
	 (ptr2 :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:sc any-reg
	       :from (:argument 0)
	       :to (:result 0)
	       :target res)
	      temp)
  (:generator 1
    (unless (location= ptr1 temp)
      (inst lr temp ptr1))
    (inst s temp ptr2)
    (unless (location= temp res)
      (inst lr res temp))))

(define-vop (vector-word-length)
  (:args (vec :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw res vec clc::g-vector-header-words)
    (inst niuo res res clc::g-vector-words-mask-16)))

(define-vop (int-sap)
  (:args (x :scs (any-reg descriptor-reg) :target res))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 6
    (unless (location= res x)
      (inst lr res x))
    (let ((fixp (gen-label)))
      (test-simple-type res temp fixp t system:%bignum-type)
      (loadw res x (/ clc::bignum-header-size 4))
      (emit-label fixp))))


(macrolet ((frob (name cond)
	     `(progn
		(def-primitive-translator ,name (x y) `(,',name ,x ,y))
		(defknown ,name (t t) boolean (movable foldable flushable))
		(define-vop (,name pointer-compare)
		  (:translate ,name)
		  (:variant ,cond)))))
  (frob pointer< :lt)
  (frob pointer> :gt))

(define-vop (check-op)
  (:args (x :scs (any-reg descriptor-reg))
	 (y :scs (any-reg descriptor-reg)))
  (:variant-vars condition not-p error)
  (:vop-var vop)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 3
    (inst c x y)
    (let ((target (generate-error-code vop error x y)))
      (if not-p
	  (inst bb condition target)
	  (inst bnb condition target)))))

(define-vop (check<= check-op)
  (:variant :gt t clc::error-not-<=)
  (:translate check<=))

(define-vop (check= check-op)
  (:variant :eq nil clc::error-not-=)
  (:translate check=))

(def-primitive-translator make-fixnum (x)
  `(%primitive make-immediate-type ,x system:%+-fixnum-type))

(define-vop (make-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg short-immediate
			     unsigned-immediate)
	       :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      ((short-immediate unsigned-immediate)
       (inst niuo res val clc::type-not-mask-16)
       (let ((code (tn-value type)))
	 (check-type code (unsigned-byte 5))
	 (unless (zerop code)
	   (inst oiu res res (ash code clc::type-shift-16)))))
      (t
       (unless (location= type temp)
	 (inst lr temp type))
       (inst niuo res val clc::type-not-mask-16)
       (inst sli16 temp clc::type-shift-16)
       (inst o res temp)))))


(define-vop (16bit-system-ref halfword-index-ref)
  (:translate sap-ref-16)
  (:variant 0))

(define-vop (signed-16bit-system-ref signed-halfword-index-ref)
  (:variant 0))

(define-vop (16bit-system-set halfword-index-set)
  (:translate (setf sap-ref-16))
  (:variant 0))

(define-vop (8bit-system-ref byte-index-ref)
  (:translate sap-ref-8)
  (:variant 0))

(define-vop (8bit-system-set byte-index-set)
  (:translate (setf sap-ref-8))
  (:variant 0))


(define-vop (current-sp)
  (:results (val :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst lr val sp-tn)))

;;; This guy makes sure that there aren't any random garbage pointers lying
;;; around in registers by clearing all of the boxed registers.  Our allocating
;;; all of the boxed registers as temporaries will prevent any TNs from being
;;; packed in those registers at the time this VOP is invoked.
;;;
(define-vop (clear-registers)
  (:temporary (:sc any-reg :offset 1) a0)
  (:temporary (:sc any-reg :offset 3) a1)
  (:temporary (:sc any-reg :offset 5) a2)
  (:temporary (:sc any-reg :offset 4) t0)
  (:temporary (:sc any-reg :offset 7) l0)
  (:temporary (:sc any-reg :offset 8) l1)
  (:temporary (:sc any-reg :offset 9) l2)
  (:temporary (:sc any-reg :offset 10) l3)
  (:temporary (:sc any-reg :offset 11) l4)
  (:generator 10
    (inst lis a0 0)
    (inst lis a1 0)
    (inst lis a2 0)
    (inst lis t0 0)
    (inst lis l0 0)
    (inst lis l1 0)
    (inst lis l2 0)
    (inst lis l3 0)
    (inst lis l4 0)))
