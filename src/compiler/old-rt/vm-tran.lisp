;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains impelemtentation-dependent transforms, IR2 convert
;;; methods, etc.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

;;; We need to define these predicates, since the TYPEP source transform picks
;;; whichever predicate was defined last when there are multiple predicates for
;;; equivalent types.
;;;
(def-source-transform single-float-p (x) `(short-float-p ,x))
(def-source-transform double-float-p (x) `(long-float-p ,x))


;;; Some hacks to let us implement structures as simple-vectors without
;;; confusing type inference too much.
;;;
(def-builtin-type 'structure-vector
  (make-named-type :name 'structure-vector
		   :supertypes '(structure-vector t)
		   :subclasses '(structure)))

(defknown structure-vector-p (t) boolean)

(define-type-predicate structure-vector-p structure-vector)

(def-source-transform structurep (x)
  (once-only ((n-x x))
    `(and (structure-vector-p ,n-x)
	  (eql (%primitive get-vector-subtype ,n-x)
	       system:%g-vector-structure-subtype))))

(define-vop (structure-vector-p simple-vector-p)
  (:translate structure-vector-p))

#-new-compiler
(set 'lisp::type-pred-alist
     (adjoin (cons 'structure-vector 'simple-vector-p)
	     (symbol-value 'lisp::type-pred-alist)
	     :key #'car))

(def-source-transform compiled-function-p (x)
  `(functionp ,x))

(def-source-transform char-int (x)
  `(truly-the char-int (%primitive make-fixnum ,x)))


;;;; Funny function implementations:

;;; Convert these funny functions into a %Primitive use, letting %Primitive
;;; deal with evaluating the "Fixed" codegen-info arguments.
;;;
(def-source-transform %more-arg-context (&rest foo)
  `(%primitive more-arg-context ,@foo))
;;;
(def-source-transform %verify-argument-count (&rest foo)
  `(%primitive verify-argument-count ,@foo))


;;; Error funny functions:
;;;
;;; Mark these as as not returning by using TRULY-THE NIL.

(def-source-transform %type-check-error (obj type)
  `(truly-the nil (%primitive error2 clc::error-object-not-type ,obj ,type)))

(def-source-transform %odd-keyword-arguments-error ()
  '(truly-the nil (%primitive error0 clc::error-odd-keyword-arguments)))

(def-source-transform %unknown-keyword-argument-error (key)
  `(truly-the nil (%primitive error1 clc::error-unknown-keyword-argument ,key)))

(def-source-transform %argument-count-error (&rest foo)
  `(truly-the nil (%primitive argument-count-error ,@foo)))


;;;; Syscall:

(def-primitive-translator syscall (&rest args) `(%syscall ,@args))
(defknown %syscall (&rest t) *)

(defoptimizer (%syscall ir2-convert) ((&rest args) node block)
  (let* ((refs (move-tail-full-call-args node block))
	 (cont (node-cont node))
	 (res (continuation-result-tns
	       cont
	       (list *any-primitive-type* *any-primitive-type*))))
    (vop* syscall node block
	  (refs)
	  ((first res) (second res) nil)
	  (length args))
    (move-continuation-result node block res cont)))  
