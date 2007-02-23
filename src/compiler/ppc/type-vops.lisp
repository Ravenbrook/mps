;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/type-vops.lisp,v 1.3 2006/06/30 18:41:24 rtoy Exp $")
;;;
;;; **********************************************************************
;;; 
;;; This file contains the VM definition of type testing and checking VOPs
;;; for the PPC
;;;
;;; Written by William Lott.
;;;
(in-package "PPC")


;;;; Simple type checking and testing:
;;;
;;;    These types are represented by a single type code, so are easily
;;; open-coded as a mask and compare.

(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (non-descriptor-reg)) temp))

(eval-when (:compile-toplevel :execute)

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) vm:lowtag-limit) 7 2)))

(defmacro def-type-vops (pred-name check-name ptype error-code
				   &rest type-codes)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
	   `((define-vop (,pred-name type-predicate)
	       (:translate ,pred-name)
	       (:generator ,cost
		 (test-type value temp target not-p ,@type-codes)))))
       ,@(when check-name
	   `((define-vop (,check-name check-type)
	       (:generator ,cost
		 (let ((err-lab
			(generate-error-code vop ,error-code value)))
		   (test-type value temp err-lab t ,@type-codes)
		   (move result value))))))
       ,@(when ptype
	   `((primitive-type-vop ,check-name (:check) ,ptype))))))

); eval-when (:compile-toplevel :execute)

(def-type-vops fixnump nil nil object-not-fixnum-error
  vm:even-fixnum-type vm:odd-fixnum-type)
(define-vop (check-fixnum check-type)
  (:generator 3
    (inst andi. temp value 3)
    (inst twi 0 value (error-number-or-lose 'object-not-fixnum-error))
    (inst twi :ne temp 0)
    (move result value)))
(primitive-type-vop check-fixnum (:check) fixnum)

(def-type-vops functionp nil nil
  object-not-function-error vm:function-pointer-type)
(define-vop (check-function check-type)
  (:generator 3
    (inst andi. temp value 7)
    (inst twi 0 value (error-number-or-lose 'object-not-function-error))
    (inst twi :ne temp vm:function-pointer-type)
    (move result value)))
(primitive-type-vop check-function (:check) function)

(def-type-vops listp nil nil
  object-not-list-error vm:list-pointer-type)
(define-vop (check-list check-type)
  (:generator 3
    (inst andi. temp value 7)
    (inst twi 0 value (error-number-or-lose 'object-not-list-error))
    (inst twi :ne temp vm:list-pointer-type)
    (move result value)))
(primitive-type-vop check-list (:check) list)

(def-type-vops %instancep nil nil
  object-not-instance-error vm:instance-pointer-type)
(define-vop (check-instance check-type)
  (:generator 3
    (inst andi. temp value 7)
    (inst twi 0 value (error-number-or-lose 'object-not-instance-error))
    (inst twi :ne temp vm:instance-pointer-type)
    (move result value)))
(primitive-type-vop check-instance (:check) instance)


(def-type-vops bignump check-bignum bignum
  object-not-bignum-error vm:bignum-type)

(def-type-vops ratiop check-ratio ratio
  object-not-ratio-error vm:ratio-type)

(def-type-vops complexp check-complex complex
  object-not-complex-error vm:complex-type
  complex-single-float-type complex-double-float-type
  #+double-double vm::complex-double-double-float-type)

(def-type-vops complex-rational-p check-complex-rational nil
  object-not-complex-rational-error complex-type)

(def-type-vops complex-float-p check-complex-float nil
  object-not-complex-float-error
  complex-single-float-type complex-double-float-type
  #+double-double vm::complex-double-double-float-type)

(def-type-vops complex-single-float-p check-complex-single-float
  complex-single-float object-not-complex-single-float-error
  complex-single-float-type)

(def-type-vops complex-double-float-p check-complex-double-float
  complex-double-float object-not-complex-double-float-error
  complex-double-float-type)

#+double-double
(def-type-vops complex-double-double-float-p check-complex-double-double-float
  complex-double-double-float object-not-complex-double-double-float-error
  vm::complex-double-double-float-type)


(def-type-vops single-float-p check-single-float single-float
  object-not-single-float-error vm:single-float-type)

(def-type-vops double-float-p check-double-float double-float
  object-not-double-float-error vm:double-float-type)

#+double-double
(def-type-vops double-double-float-p check-double-double-float
  double-double-float object-not-double-double-float-error
  vm:double-double-float-type)

(def-type-vops simple-string-p check-simple-string simple-string
  object-not-simple-string-error vm:simple-string-type)

(def-type-vops simple-bit-vector-p check-simple-bit-vector simple-bit-vector
  object-not-simple-bit-vector-error vm:simple-bit-vector-type)

(def-type-vops simple-vector-p check-simple-vector simple-vector
  object-not-simple-vector-error vm:simple-vector-type)

(def-type-vops simple-array-unsigned-byte-2-p
  check-simple-array-unsigned-byte-2
  simple-array-unsigned-byte-2
  object-not-simple-array-unsigned-byte-2-error
  vm:simple-array-unsigned-byte-2-type)

(def-type-vops simple-array-unsigned-byte-4-p
  check-simple-array-unsigned-byte-4
  simple-array-unsigned-byte-4
  object-not-simple-array-unsigned-byte-4-error
  vm:simple-array-unsigned-byte-4-type)

(def-type-vops simple-array-unsigned-byte-8-p
  check-simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8
  object-not-simple-array-unsigned-byte-8-error
  vm:simple-array-unsigned-byte-8-type)

(def-type-vops simple-array-unsigned-byte-16-p
  check-simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16
  object-not-simple-array-unsigned-byte-16-error
  vm:simple-array-unsigned-byte-16-type)

(def-type-vops simple-array-unsigned-byte-32-p
  check-simple-array-unsigned-byte-32
  simple-array-unsigned-byte-32
  object-not-simple-array-unsigned-byte-32-error
  vm:simple-array-unsigned-byte-32-type)

(def-type-vops simple-array-signed-byte-8-p
  check-simple-array-signed-byte-8
  simple-array-signed-byte-8
  object-not-simple-array-signed-byte-8-error
  simple-array-signed-byte-8-type)

(def-type-vops simple-array-signed-byte-16-p
  check-simple-array-signed-byte-16
  simple-array-signed-byte-16
  object-not-simple-array-signed-byte-16-error
  simple-array-signed-byte-16-type)

(def-type-vops simple-array-signed-byte-30-p
  check-simple-array-signed-byte-30
  simple-array-signed-byte-30
  object-not-simple-array-signed-byte-30-error
  simple-array-signed-byte-30-type)

(def-type-vops simple-array-signed-byte-32-p
  check-simple-array-signed-byte-32
  simple-array-signed-byte-32
  object-not-simple-array-signed-byte-32-error
  simple-array-signed-byte-32-type)

(def-type-vops simple-array-single-float-p check-simple-array-single-float
  simple-array-single-float object-not-simple-array-single-float-error
  vm:simple-array-single-float-type)

(def-type-vops simple-array-double-float-p check-simple-array-double-float
  simple-array-double-float object-not-simple-array-double-float-error
  vm:simple-array-double-float-type)

#+double-double
(def-type-vops simple-array-double-double-float-p check-simple-array-double-double-float
  simple-array-double-double-float object-not-simple-array-double-double-float-error
  vm::simple-array-double-double-float-type)

(def-type-vops simple-array-complex-single-float-p
  check-simple-array-complex-single-float
  simple-array-complex-single-float
  object-not-simple-array-complex-single-float-error
  simple-array-complex-single-float-type)

(def-type-vops simple-array-complex-double-float-p
  check-simple-array-complex-double-float
  simple-array-complex-double-float
  object-not-simple-array-complex-double-float-error
  simple-array-complex-double-float-type)

#+double-double
(def-type-vops simple-array-complex-double-double-float-p
  check-simple-array-complex-double-double-float
  simple-array-complex-double-double-float
  object-not-simple-array-complex-double-double-float-error
  vm::simple-array-complex-double-double-float-type)

(def-type-vops base-char-p check-base-char base-char
  object-not-base-char-error vm:base-char-type)

(def-type-vops system-area-pointer-p check-system-area-pointer
  system-area-pointer object-not-sap-error vm:sap-type)

(def-type-vops weak-pointer-p check-weak-pointer weak-pointer
  object-not-weak-pointer-error vm:weak-pointer-type)

(def-type-vops scavenger-hook-p nil nil nil
  0)

(def-type-vops code-component-p nil nil nil
  vm:code-header-type)

(def-type-vops lra-p nil nil nil
  vm:return-pc-header-type)

(def-type-vops fdefn-p nil nil nil
  vm:fdefn-type)

(def-type-vops funcallable-instance-p nil nil nil
  vm:funcallable-instance-header-type)

(def-type-vops array-header-p nil nil nil
  vm:simple-array-type vm:complex-string-type vm:complex-bit-vector-type
  vm:complex-vector-type vm:complex-array-type)

(def-type-vops nil check-function-or-symbol nil object-not-function-or-symbol-error
  vm:function-pointer-type vm:symbol-header-type)

(def-type-vops stringp check-string nil object-not-string-error
  vm:simple-string-type vm:complex-string-type)

(def-type-vops bit-vector-p check-bit-vector nil object-not-bit-vector-error
  vm:simple-bit-vector-type vm:complex-bit-vector-type)

(def-type-vops vectorp check-vector nil object-not-vector-error
  simple-string-type simple-bit-vector-type simple-vector-type
  simple-array-unsigned-byte-2-type simple-array-unsigned-byte-4-type
  simple-array-unsigned-byte-8-type simple-array-unsigned-byte-16-type
  simple-array-unsigned-byte-32-type
  simple-array-signed-byte-8-type simple-array-signed-byte-16-type
  simple-array-signed-byte-30-type simple-array-signed-byte-32-type
  simple-array-single-float-type simple-array-double-float-type
  #+double-double vm::simple-array-double-double-float-type
  simple-array-complex-single-float-type
  simple-array-complex-double-float-type
  #+double-double vm::simple-array-complex-double-double-float-type
  complex-string-type complex-bit-vector-type complex-vector-type)

(def-type-vops simple-array-p check-simple-array nil object-not-simple-array-error
  simple-array-type simple-string-type simple-bit-vector-type
  simple-vector-type simple-array-unsigned-byte-2-type
  simple-array-unsigned-byte-4-type simple-array-unsigned-byte-8-type
  simple-array-unsigned-byte-16-type simple-array-unsigned-byte-32-type
  simple-array-signed-byte-8-type simple-array-signed-byte-16-type
  simple-array-signed-byte-30-type simple-array-signed-byte-32-type
  simple-array-single-float-type simple-array-double-float-type
  #+double-double vm::simple-array-double-double-float-type
  simple-array-complex-single-float-type
  simple-array-complex-double-float-type
  #+double-double vm::simple-array-complex-double-double-float-type)

(def-type-vops arrayp check-array nil object-not-array-error
  simple-array-type simple-string-type simple-bit-vector-type
  simple-vector-type simple-array-unsigned-byte-2-type
  simple-array-unsigned-byte-4-type simple-array-unsigned-byte-8-type
  simple-array-unsigned-byte-16-type simple-array-unsigned-byte-32-type
  simple-array-signed-byte-8-type simple-array-signed-byte-16-type
  simple-array-signed-byte-30-type simple-array-signed-byte-32-type
  simple-array-single-float-type simple-array-double-float-type
  #+double-double vm::simple-array-double-double-float-type
  simple-array-complex-single-float-type
  simple-array-complex-double-float-type
  #+double-double vm::simple-array-complex-double-double-float-type
  complex-string-type complex-bit-vector-type complex-vector-type
  complex-array-type)

(def-type-vops numberp check-number nil object-not-number-error
  even-fixnum-type odd-fixnum-type bignum-type ratio-type
  single-float-type double-float-type
  #+double-double vm:double-double-float-type
  complex-type
  complex-single-float-type complex-double-float-type
  #+double-double vm::complex-double-double-float-type)

(def-type-vops rationalp check-rational nil object-not-rational-error
  vm:even-fixnum-type vm:odd-fixnum-type vm:ratio-type vm:bignum-type)

(def-type-vops integerp check-integer nil object-not-integer-error
  vm:even-fixnum-type vm:odd-fixnum-type vm:bignum-type)

(def-type-vops floatp check-float nil object-not-float-error
  vm:single-float-type vm:double-float-type
  #+double-double vm:double-double-float-type)

(def-type-vops realp check-real nil object-not-real-error
  vm:even-fixnum-type vm:odd-fixnum-type vm:ratio-type vm:bignum-type
  vm:single-float-type vm:double-float-type
  #+double-double vm:double-double-float-type)


;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label)))
      (multiple-value-bind
	  (yep nope)
	  (if not-p
	      (values not-target target)
	      (values target not-target))
	(inst andi. temp value #x3)
	(inst beq yep)
	(test-type value temp nope t vm:other-pointer-type)
	(loadw temp value 0 vm:other-pointer-type)
	(inst cmpwi temp (+ (ash 1 vm:type-bits)
			  vm:bignum-type))
	(inst b? (if not-p :ne :eq) target)
	(emit-label not-target)))))

(define-vop (check-signed-byte-32 check-type)
  (:generator 45
    (let ((nope (generate-error-code vop object-not-signed-byte-32-error value))
	  (yep (gen-label)))
      (inst andi. temp value #x3)
      (inst beq yep)
      (test-type value temp nope t vm:other-pointer-type)
      (loadw temp value 0 vm:other-pointer-type)
      (inst cmpwi temp (+ (ash 1 vm:type-bits) vm:bignum-type))
      (inst bne nope)
      (emit-label yep)
      (move result value))))


;;; An (unsigned-byte 32) can be represented with either a positive fixnum, a
;;; bignum with exactly one positive digit, or a bignum with exactly two digits
;;; and the second digit all zeros.

(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label))
	  (single-word (gen-label))
 	  (fixnum (gen-label)))
      (multiple-value-bind
	  (yep nope)
	  (if not-p
	      (values not-target target)
	      (values target not-target))
	;; Is it a fixnum?
	(inst andi. temp value #x3)
        (inst cmpwi :cr1 value 0)
        (inst beq fixnum)

	;; If not, is it an other pointer?
	(test-type value temp nope t vm:other-pointer-type)
	;; Get the header.
	(loadw temp value 0 vm:other-pointer-type)
	;; Is it one?
	(inst cmpwi temp (+ (ash 1 vm:type-bits) vm:bignum-type))
	(inst beq single-word)
	;; If it's other than two, we can't be an (unsigned-byte 32)
	(inst cmpwi temp (+ (ash 2 vm:type-bits) vm:bignum-type))
	(inst bne nope)
	;; Get the second digit.
	(loadw temp value (1+ vm:bignum-digits-offset) vm:other-pointer-type)
	;; All zeros, its an (unsigned-byte 32).
	(inst cmpwi temp 0)
	(inst beq yep)
	;; Otherwise, it isn't.
	(inst b nope)
	
	(emit-label single-word)
	;; Get the single digit.
	(loadw temp value vm:bignum-digits-offset vm:other-pointer-type)
	(inst cmpwi :cr1 temp 0)

	;; positive implies (unsigned-byte 32).
	(emit-label fixnum)
	(inst b?  :cr1 (if not-p :lt :ge) target)

	(emit-label not-target)))))	  

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
    (let ((nope
	   (generate-error-code vop object-not-unsigned-byte-32-error value))
	  (yep (gen-label))
	  (fixnum (gen-label))
	  (single-word (gen-label)))
      ;; Is it a fixnum?
      (inst andi. temp value #x3)
      (inst cmpwi :cr1 value 0)
      (inst beq fixnum)

      ;; If not, is it an other pointer?
      (test-type value temp nope t vm:other-pointer-type)
      ;; Get the number of digits.
      (loadw temp value 0 vm:other-pointer-type)
      ;; Is it one?
      (inst cmpwi temp (+ (ash 1 vm:type-bits) vm:bignum-type))
      (inst beq single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst cmpwi temp (+ (ash 2 vm:type-bits) vm:bignum-type))
      (inst bne nope)
      ;; Get the second digit.
      (loadw temp value (1+ vm:bignum-digits-offset) vm:other-pointer-type)
      ;; All zeros, its an (unsigned-byte 32).
      (inst cmpwi temp 0)
      (inst beq yep)
      ;; Otherwise, it isn't.
      (inst b nope)
      
      (emit-label single-word)
      ;; Get the single digit.
      (loadw temp value vm:bignum-digits-offset vm:other-pointer-type)
      ;; positive implies (unsigned-byte 32).
      (inst cmpwi :cr1 temp 0)
      
      (emit-label fixnum)
      (inst blt :cr1 nope)
      
      (emit-label yep)
      (move result value))))




;;;; List/symbol types:
;;; 
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (let* ((drop-thru (gen-label))
	   (is-symbol-label (if not-p drop-thru target)))
      (inst cmpw value null-tn)
      (inst beq is-symbol-label)
      (test-type value temp target not-p vm:symbol-header-type)
      (emit-label drop-thru))))

(define-vop (check-symbol check-type)
  (:generator 12
    (let ((drop-thru (gen-label))
	  (error (generate-error-code vop object-not-symbol-error value)))
      (inst cmpw value null-tn)
      (inst beq drop-thru)
      (test-type value temp error t vm:symbol-header-type)
      (emit-label drop-thru)
      (move result value))))
  
(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let* ((drop-thru (gen-label))
	   (is-not-cons-label (if not-p target drop-thru)))
      (inst cmpw value null-tn)
      (inst beq is-not-cons-label)
      (test-type value temp target not-p vm:list-pointer-type)
      (emit-label drop-thru))))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop object-not-cons-error value)))
      (inst cmpw value null-tn)
      (inst beq error)
      (test-type value temp error t vm:list-pointer-type)
      (move result value))))

