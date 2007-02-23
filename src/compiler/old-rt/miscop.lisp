;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    We have a few general-purpose VOPs that are used to implement all the
;;; miscop VOPs using the variant mechanism.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

    
(define-vop (miscop)
  (:temporary (:sc any-reg  :offset 0
	       :from (:eval 0)  :to (:eval 1))
	      nl0)
  (:temporary (:sc any-reg  :offset 1
	       :from (:eval 0)  :to (:eval 1))
	      a0)
  (:temporary (:sc any-reg  :offset 2
	       :from (:eval 0)  :to (:eval 1))
	      nl1)
  (:temporary (:sc any-reg  :offset 3
	       :from (:eval 0)  :to (:eval 1))
	      a1)
  (:temporary (:sc any-reg  :offset 4
	       :from (:eval 0)  :to (:eval 1))
	      a3)
  (:temporary (:sc any-reg  :offset 5
	       :from (:eval 0)  :to (:eval 1))
	      a2)
  (:temporary (:sc any-reg  :offset 15
	       :from (:eval 0)  :to (:eval 1))
	      misc-pc)
  (:note "miscop call")
  (:variant-vars miscop-name)
  (:vop-var vop)
  (:save-p :compute-only)
  (:policy :safe))

(eval-when (compile load eval)
  (defconstant reg-arg-count 4)
  (defconstant arg-names '(x y z arg3 arg4 arg5 arg6 arg7 arg8))
  (defconstant temp-names '(a0 a1 a2 a3 s3 s4 s5 s6 s7))
  (defconstant temp-offsets '(1 3 5 4 3 4 5 6 7))
  (defconstant result-names '(r r1 r2 r3 r4 r5 r6 r7 r8))
); eval-when (compile load eval)


;;; Make-Miscop  --  Internal
;;;
;;;    Make a miscop with the specified numbers of arguments and results,
;;; conditional flag, etc.
;;;
(defmacro make-miscop (nargs nresults &key conditional)
  (collect ((args)
	    (results)
	    (temps)
	    (arg-moves)
	    (result-moves))
    (let ((max-ops (max nargs nresults)))
      (dotimes (i nargs)
	(let ((arg (elt arg-names i))
	      (temp (elt temp-names i)))
	  (args
	   `(,arg :target ,temp :scs (any-reg descriptor-reg)))
	  (arg-moves
	   (if (>= i reg-arg-count)
	       `(store-stack-tn ,temp ,arg)
	       `(unless (location= ,arg ,temp)
		  (inst lr ,temp ,arg))))))
      
      (dotimes (i nresults)
	(let ((result (elt result-names i))
	      (temp (elt temp-names i)))
	  (results
	   `(,result :scs (any-reg descriptor-reg)))
	  (result-moves
	   (if (>= i reg-arg-count)
	       `(load-stack-tn ,result ,temp)
	       `(unless (location= ,result ,temp)
		  (inst lr ,result ,temp))))))
      
      (dotimes (i (max nargs nresults))
	(temps
	 `(:temporary (:sc ,(if (>= i reg-arg-count) 'stack 'any-reg)
			   :offset ,(elt temp-offsets i)
			   :from ,(if (>= i nargs) '(:eval 0) `(:argument ,i))
			   :to ,(if (>= i nresults) '(:eval 1) `(:result ,i))
			   ,@(unless (>= i nresults)
			       `(:target ,(elt result-names i))))
		      ,(elt temp-names i))))
      
      `(define-vop (,(miscop-name nargs nresults conditional) miscop)
	 (:args ,@(args))
	 ,@(unless conditional `((:results ,@(results))))
	 ,@(temps)
	 ,@(when conditional
	     '((:conditional t)
	       (:variant-vars miscop-name condition)
	       (:info target not-p)))
	 (:ignore nl0 nl1 misc-pc
		  ,@(if (>= max-ops reg-arg-count)
			()
			(subseq temp-names max-ops reg-arg-count)))
	 (:generator 20
	   ,@(arg-moves)
	   (inst miscop miscop-name)
	   (note-this-location vop :known-return)
	   ,@(result-moves)
	   ,@(when conditional
	       '((if not-p
		     (inst bnb condition target)
		     (inst bb condition target)))))))))

(make-miscop 1 0 :conditional t)
(make-miscop 2 0 :conditional t)

(make-miscop 0 0)
(make-miscop 1 0)
(make-miscop 2 0)
(make-miscop 3 0)
(make-miscop 4 0)

(make-miscop 0 1)
(make-miscop 1 1)
(make-miscop 2 1)
(make-miscop 3 1)
(make-miscop 4 1)
(make-miscop 5 1)

(make-miscop 0 2)
(make-miscop 1 2)
(make-miscop 2 2)
(make-miscop 3 2)
(make-miscop 4 2)
(make-miscop 5 2)

(make-miscop 1 3)

(define-vop (effectless-unaffected-one-arg-miscop one-arg-miscop)
  (:effects)
  (:affected))

(define-vop (effectless-unaffected-two-arg-miscop two-arg-miscop)
  (:effects)
  (:affected))

(define-vop (n-arg-miscop zero-arg-miscop)
  (:args (passed-args :more t))
  (:ignore passed-args a1 a2 a3 nl1 misc-pc)
  (:info nargs)
  (:generator 40
    (inst miscopx miscop-name)
    (inst cal nl0 zero-tn nargs)
    (note-this-location vop :known-return)
    (unless (location= r a0)
      (inst lr r a0))))

(define-vop (n-arg-two-value-miscop zero-arg-two-value-miscop)
  (:args (passed-args :more t))
  (:ignore passed-args a2 a3 nl1 misc-pc)
  (:info nargs)
  (:generator 40
    (inst miscopx miscop-name)
    (inst cal nl0 zero-tn nargs)
    (note-this-location vop :known-return)
    (unless (location= r a0)
      (inst lr r a0))
    (unless (location= r1 a1)
      (inst lr r1 a1))))


;;;; ILLEGAL-MOVE

;;; This VOP is emitted when we attempt to do a move between incompatible
;;; primitive types.  We signal an error, and ignore the result (which is
;;; specified only to terminate its lifetime.)
;;;
(define-vop (illegal-move three-arg-miscop)
  (:args (x :scs (any-reg descriptor-reg) :target a1)
	 (y-type :scs (any-reg descriptor-reg) :target a2))
  (:variant-vars)
  (:ignore r a3 nl0 nl1 misc-pc)
  (:generator 666
    (loadi a0 clc::error-object-not-type)
    (unless (location= x a1)
      (inst lr a1 x))
    (unless (location= y-type a2)
      (inst lr a2 y-type))
    (inst miscop 'clc::error2)
    (note-this-location vop :internal-error)))
