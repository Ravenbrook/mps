;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of type testing and checking VOPs
;;; for the RT.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

;;;; Simple type checking and testing:
;;;
;;;    These types are represented by a single type code, so are easily
;;; open-coded as non-shifting type test.

(define-vop (check-simple-type)
  (:args
   (value :target result
	  :scs (any-reg descriptor-reg)))
  (:results
   (result :scs (any-reg descriptor-reg)))
  (:variant-vars type-code error-code)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 4
    (let ((err-lab (generate-error-code vop error-code value)))
      (test-simple-type value temp err-lab t type-code)
      (unless (location= value result)
	(inst lr result value)))))

(define-vop (simple-type-predicate)
  (:args
   (value :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:variant-vars type-code)
  (:policy :fast-safe)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 4
    (test-simple-type value temp target not-p type-code)))


(macrolet ((frob (pred-name check-name ptype type-code error-code)
	     `(progn
		(define-vop (,pred-name simple-type-predicate)
		  (:variant ,type-code)
		  (:translate ,pred-name))

		,@(when check-name
		    `((define-vop (,check-name check-simple-type)
			(:variant ,type-code ,error-code))
		      (primitive-type-vop ,check-name (:check) ,ptype))))))

  (frob array-header-p nil nil system:%array-type nil)

  (frob simple-string-p check-simple-string simple-string
    system:%string-type clc::error-object-not-simple-string)

  (frob simple-vector-p check-simple-vector simple-vector
    system:%general-vector-type clc::error-object-not-simple-vector)

  (frob simple-bit-vector-p check-simple-bit-vector simple-bit-vector
    system:%bit-vector-type clc::error-object-not-simple-bit-vector)

  (frob functionp check-function function
    system:%function-type clc::error-illegal-function)

  (frob listp check-list list system:%list-type clc::error-not-list)

  (frob long-float-p check-long-float long-float
    system:%long-float-type clc::error-object-not-long-float)

  (frob %string-char-p check-string-char string-char
    system:%string-char-type clc::error-object-not-string-char)

  (frob bignump nil bignum system:%bignum-type nil)
  (frob ratiop nil ratio system:%ratio-type nil)
  (frob complexp nil complex system:%complex-type nil))

#|
simple-integer-vector-p?
|#



;;;; Type checking and testing via miscops:
;;;
;;;    These operations seem too complex to be worth open-coding.

(macrolet ((frob (name)
	     `(define-vop (,name one-arg-conditional-miscop)
		(:translate ,name)
		(:variant ',name :eq))))
  (frob vectorp)
  (frob stringp)
  (frob bit-vector-p))


;;;; Hairy type tests:
;;;
;;;    These types are represented by a union of type codes.  
;;;

(define-vop (hairy-type-predicate)
  (:args
   (obj :scs (any-reg descriptor-reg)
	:target temp))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (any-reg)
		    :type fixnum
		    :from (:argument 0))
	      temp))

(define-vop (check-hairy-type)
  (:args
   (obj :scs (any-reg descriptor-reg)
	:target res))
  (:results
   (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:vop-var vop)
  (:save-p :compute-only))

  
(macrolet ((frob (pred-name check-name error-code &rest types)
	     (let ((cost (* (+ (length types)
			       (count-if #'consp types))
			    4)))
	       `(progn
		  ,@(when pred-name
		      `((define-vop (,pred-name hairy-type-predicate)
			  (:translate ,pred-name)
			  (:generator ,cost
			    (test-hairy-type obj temp target not-p ,@types)))))
			
		  ,@(when check-name
		      `((define-vop (,check-name check-hairy-type)
			  (:generator ,cost
			    (let ((err-lab (generate-error-code
					    vop ,error-code obj)))
			      (test-hairy-type obj temp err-lab t ,@types))
			    (unless (location= obj res)
			      (inst lr res obj))))))))))

  (frob nil check-function-or-symbol clc::error-object-not-function-or-symbol
    system:%function-type system:%symbol-type)

  (frob arrayp nil nil (system:%string-type system:%array-type))

  (frob numberp nil nil
    system:%+-fixnum-type system:%--fixnum-type
    (system:%bignum-type system:%long-float-type))

  (frob rationalp nil nil
    system:%+-fixnum-type system:%--fixnum-type
    system:%ratio-type system:%bignum-type)

  (frob floatp nil nil (system:%short-+-float-type system:%long-float-type))

  (frob integerp nil nil
    system:%+-fixnum-type system:%--fixnum-type system:%bignum-type)

  (frob characterp nil nil system:%bitsy-char-type system:%string-char-type)
  
  (frob fixnump check-fixnum clc::error-object-not-fixnum
    system:%+-fixnum-type system:%--fixnum-type)

  (frob short-float-p check-short-float clc::error-object-not-short-float
    system:%short---float-type system:%short-+-float-type))

(primitive-type-vop check-fixnum (:check) fixnum)
(primitive-type-vop check-short-float (:check) short-float)


;;;; List/symbol types:


;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (list-symbol-predicate)
  (:args
   (obj :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)))

(define-vop (check-list-symbol check-hairy-type)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp))


(macrolet ((frob (pred-name check-name error-code &rest body)
	     `(progn
		(define-vop (,pred-name list-symbol-predicate)
		  (:translate ,pred-name)
		  (:generator 12
		    ,@body))
		(define-vop (,check-name check-list-symbol)
		  (:generator 12
		    (let ((target (generate-error-code vop ,error-code obj))
			  (not-p t))
		      ,@body
		      (unless (location= obj res)
			(inst lr res obj))))))))

  (frob symbolp check-symbol clc::error-not-symbol
    (let* ((drop-thru (gen-label))
	   (in-lab (if not-p drop-thru target)))
      (test-simple-type obj temp in-lab nil system:%symbol-type)
      (test-special-value obj temp nil target not-p)
      (emit-label drop-thru)))

  (frob consp check-cons clc::error-object-not-cons
    (let* ((drop-thru (gen-label))
	   (out-lab (if not-p target drop-thru)))
      (test-simple-type obj temp out-lab t system:%list-type)
      (test-special-value obj temp nil target (not not-p))
      (emit-label drop-thru))))


;;;; Function coercion:

;;; If not a function, get the symbol value and test for that being a function.
;;; Since we test for a function rather than the unbound marker, this works on
;;; NIL.
;;;
(define-vop (fast-safe-coerce-to-function)
  (:args (thing :scs (descriptor-reg)
		:target res))
  (:results (res :scs (descriptor-reg)))
  (:node-var node)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) thing-temp)
  (:generator 0
    (let ((not-fun-lab (gen-label))
	  (done-lab (gen-label)))
      (test-simple-type thing temp not-fun-lab t system:%function-type)
      (unless (location= thing res)
	(inst lr res thing))
      (emit-label done-lab)

      (unassemble
	(assemble-elsewhere node
	  (emit-label not-fun-lab)
	  (inst lr thing-temp thing)
	  (loadw res thing (/ clc::symbol-definition 4))
	  (test-simple-type res temp done-lab nil system:%function-type)
	  (error-call clc::error-symbol-undefined thing-temp))))))


(define-vop (coerce-to-function)
  (:args (thing :scs (descriptor-reg)
		:target res))
  (:results (res :scs (descriptor-reg)))
  (:node-var node)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) thing-temp)
  (:generator 0
    (let ((not-fun-lab (gen-label))
	  (done-lab (gen-label))
	  (not-sym-lab (gen-label)))
      (test-simple-type thing temp not-fun-lab t system:%function-type)
      (unless (location= thing res)
	(inst lr res thing))
      (emit-label done-lab)

      (unassemble
	(assemble-elsewhere node
	  (emit-label not-fun-lab)
	  (test-simple-type thing temp not-sym-lab t system:%symbol-type)
	  (inst lr thing-temp thing)
	  (loadw res thing (/ clc::symbol-definition 4))
	  (test-simple-type res temp done-lab nil system:%function-type)
	  (error-call clc::error-symbol-undefined thing-temp)

	  (emit-label not-sym-lab)
	  (error-call clc::error-object-not-function-or-symbol thing))))))
