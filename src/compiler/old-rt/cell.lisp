;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for the RT.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Symbol hacking VOPs:

;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (unless (location= obj-temp object)
      (inst lr obj-temp object))
    (loadw value obj-temp (/ clc::symbol-value 4))
    (let ((err-lab (generate-error-code vop clc::error-symbol-unbound
					obj-temp)))
      (test-special-value value temp '%trap-object err-lab nil))))

;;; With Symbol-Function, we check that the result is a function, so NIL is
;;; always un-fbound.
;;;
(define-vop (symbol-function checked-cell-ref)
  (:translate symbol-function)
  (:generator 10
    (unless (location= obj-temp object)
      (inst lr obj-temp object))
    (loadw value obj-temp (/ clc::symbol-definition 4))
    (let ((err-lab (generate-error-code vop clc::error-symbol-undefined
					obj-temp)))
      (test-simple-type value temp err-lab t system:%function-type))))


;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp))

(define-vop (boundp boundp-frob)
  (:translate boundp)
  (:generator 9
    (loadw value object (/ clc::symbol-value 4))
    (test-special-value value temp '%trap-object target (not not-p))))


;;; SYMBOL isn't a primitive type, so we can't use it for the arg restriction
;;; on the symbol case of fboundp.  Instead, we transform to a funny function.

(defknown fboundp/symbol (t) boolean (flushable))
;;;
(deftransform fboundp ((x) (symbol))
  '(fboundp/symbol x))
;;;
(define-vop (fboundp/symbol boundp-frob)
  (:translate fboundp/symbol)
  (:generator 10
    (loadw value object (/ clc::symbol-definition 4))
    (test-simple-type value temp target not-p system:%function-type)))

(def-source-transform makunbound (x)
  `(set ,x (%primitive make-immediate-type 0 system:%trap-type)))


(define-vop (fast-symbol-value cell-ref)
  (:variant (/ clc::symbol-value 4))
  (:policy :fast)
  (:translate symbol-value))

(define-vop (fast-symbol-function cell-ref)
  (:variant (/ clc::symbol-definition 4))
  (:policy :fast)
  (:translate symbol-function))

(define-cell-accessors (/ clc::symbol-value 4) nil nil set set)
(define-cell-accessors (/ clc::symbol-definition 4)
  nil nil set-symbol-function %sp-set-definition)
(define-cell-accessors (/ clc::symbol-property-list 4)
  symbol-plist symbol-plist set-symbol-plist %sp-set-plist)
(define-cell-accessors (/ clc::symbol-print-name 4)
  symbol-name symbol-name nil nil)
(define-cell-accessors (/ clc::symbol-package 4)
  symbol-package symbol-package set-package nil)


(define-miscop bind (val symbol) :results ())
(define-miscop unbind (num) :results ())


;;;; List hackery:

(define-cell-accessors (/ clc::list-car 4)
  car car set-car %rplaca)
(define-cell-accessors (/ clc::list-cdr 4)
  cdr cdr set-cdr %rplacd)

(define-miscop cons (x y) :translate cons)


;;;; Value cell and closure hackery:

(define-miscop make-value-cell (val))
(define-miscop make-closure (nvars entry))

(define-vop (value-cell-ref cell-ref)
  (:variant (+ clc::g-vector-header-size-in-words
	       system:%function-value-cell-value-slot)))

(define-vop (value-cell-set cell-set)
  (:variant (+ clc::g-vector-header-size-in-words
	       system:%function-value-cell-value-slot)))

(define-vop (closure-init slot-set))
(define-vop (closure-ref slot-ref))


;;;; Structure hackery:

(define-vop (structure-ref slot-ref))
(define-vop (structure-set slot-set))


;;;; Number hackery:

(define-vop (realpart cell-ref)
  (:policy :fast-safe)
  (:variant (/ clc::complex-realpart 4)))

(define-vop (imagpart cell-ref)
  (:policy :fast-safe)
  (:variant (/ clc::complex-imagpart 4)))

(define-vop (numerator cell-ref)
  (:policy :fast-safe)
  (:variant (/ clc::ratio-numerator 4)))

(define-vop (denominator cell-ref)
  (:policy :fast-safe)
  (:variant (/ clc::ratio-denominator 4)))
