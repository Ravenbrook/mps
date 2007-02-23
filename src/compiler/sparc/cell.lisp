;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/cell.lisp,v 1.26 2004/05/18 01:14:05 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for the SPARC.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "SPARC")


;;;; Data object ref/set stuff.

(define-vop (slot)
  (:args (object :scs (descriptor-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (loadw result object offset lowtag)))

(define-vop (set-slot)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
    (storew value object offset lowtag)))



;;;; Symbol hacking VOPs:

;;; The compiler likes to be able to directly SET symbols.
;;;
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-type))

;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp vm:symbol-value-slot vm:other-pointer-type)
    (let ((err-lab (generate-error-code vop unbound-symbol-error obj-temp)))
      (inst cmp value vm:unbound-marker-type)
      (inst b :eq err-lab)
      (inst nop))))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value))

(define-vop (boundp boundp-frob)
  (:translate boundp)
  (:generator 9
    (loadw value object vm:symbol-value-slot vm:other-pointer-type)
    (inst cmp value vm:unbound-marker-type)
    (inst b (if not-p :eq :ne) target)
    (inst nop)))

(define-vop (fast-symbol-value cell-ref)
  (:variant vm:symbol-value-slot vm:other-pointer-type)
  (:policy :fast)
  (:translate symbol-value))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg null)))
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:generator 2
    ;; the symbol-hash slot of NIL holds NIL because it is also the cdr
    ;; slot, so we have to strip off the two low bits to make sure it is
    ;; a fixnum.
    (loadw res symbol symbol-hash-slot other-pointer-type)
    (inst andn res vm:fixnum-tag-mask)))

(define-vop (%set-symbol-hash cell-set)
  (:translate %set-symbol-hash)
  (:variant symbol-hash-slot other-pointer-type))


;;;; Fdefinition (fdefn) objects.

(define-vop (fdefn-function cell-ref)
  (:variant fdefn-function-slot other-pointer-type))

(define-vop (safe-fdefn-function)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp)
  (:generator 10
    (move obj-temp object)
    (loadw value obj-temp fdefn-function-slot other-pointer-type)
    (inst cmp value null-tn)
    (let ((err-lab (generate-error-code vop undefined-symbol-error obj-temp)))
      (inst b :eq err-lab))
    (inst nop)))

(define-vop (set-fdefn-function)
  (:policy :fast-safe)
  (:translate (setf fdefn-function))
  (:args (function :scs (descriptor-reg) :target result)
	 (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (let ((normal-fn (gen-label)))
      (load-type type function (- function-pointer-type))
      (inst cmp type function-header-type)
      (inst b :eq normal-fn)
      (inst move lip function)
      ;; For the linkage-table stuff, we need to look up the address
      ;; from the linkage table instead of using the address directly.
      (inst li lip (make-fixup (extern-alien-name "closure_tramp")
			       #-linkage-table :foreign
			       #+linkage-table :foreign-data))
      #+linkage-table
      (loadw lip lip)
      (emit-label normal-fn)
      (storew function fdefn fdefn-function-slot other-pointer-type)
      (storew lip fdefn fdefn-raw-addr-slot other-pointer-type)
      (move result function))))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew null-tn fdefn fdefn-function-slot other-pointer-type)
    ;; For the linkage-table stuff, we need to look up the address
    ;; from the linkage table instead of using the address directly.
    (inst li temp (make-fixup (extern-alien-name "undefined_tramp")
			      #-linkage-table :foreign
			      #+linkage-table :foreign-data))
    #+linkage-table
    (loadw temp temp)
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-type)
    (move result fdefn)))



;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
	 (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol vm:symbol-value-slot vm:other-pointer-type)
    (inst add bsp-tn bsp-tn (* 2 vm:word-bytes))
    (storew temp bsp-tn (- vm:binding-value-slot vm:binding-size))
    (storew symbol bsp-tn (- vm:binding-symbol-slot vm:binding-size))
    (storew val symbol vm:symbol-value-slot vm:other-pointer-type)))


(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (loadw symbol bsp-tn (- vm:binding-symbol-slot vm:binding-size))
    (loadw value bsp-tn (- vm:binding-value-slot vm:binding-size))
    (storew value symbol vm:symbol-value-slot vm:other-pointer-type)
    (storew zero-tn bsp-tn (- vm:binding-symbol-slot vm:binding-size))
    (inst sub bsp-tn bsp-tn (* 2 vm:word-bytes))))


(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (let ((loop (gen-label))
	  (skip (gen-label))
	  (done (gen-label)))
      (move where arg)
      (inst cmp where bsp-tn)
      (inst b :eq done)
      (inst nop)

      (emit-label loop)
      (loadw symbol bsp-tn (- vm:binding-symbol-slot vm:binding-size))
      (inst cmp symbol)
      (inst b :eq skip)
      (loadw value bsp-tn (- vm:binding-value-slot vm:binding-size))
      (storew value symbol vm:symbol-value-slot vm:other-pointer-type)
      (storew zero-tn bsp-tn (- vm:binding-symbol-slot vm:binding-size))

      (emit-label skip)
      (inst sub bsp-tn bsp-tn (* 2 vm:word-bytes))
      (inst cmp where bsp-tn)
      (inst b :ne loop)
      (inst nop)

      (emit-label done))))



;;;; Closure indexing.

(define-vop (closure-index-ref word-index-ref)
  (:variant vm:closure-info-offset vm:function-pointer-type)
  (:translate %closure-index-ref))

(define-vop (funcallable-instance-info word-index-ref)
  (:variant funcallable-instance-info-offset vm:function-pointer-type)
  (:translate %funcallable-instance-info))

(define-vop (set-funcallable-instance-info word-index-set)
  (:variant funcallable-instance-info-offset function-pointer-type)
  (:translate %set-funcallable-instance-info))

(define-vop (funcallable-instance-lexenv cell-ref)
  (:variant funcallable-instance-lexenv-slot function-pointer-type))


(define-vop (closure-ref slot-ref)
  (:variant closure-info-offset function-pointer-type))

(define-vop (closure-init slot-set)
  (:variant closure-info-offset function-pointer-type))


;;;; Value Cell hackery.

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-type))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-type))



;;;; Instance hackery:

(define-vop (instance-length)
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw temp struct 0 instance-pointer-type)
    (inst srln res temp vm:type-bits)))

(define-vop (instance-ref slot-ref)
  (:variant instance-slots-offset instance-pointer-type)
  (:policy :fast-safe)
  (:translate %instance-ref)
  (:arg-types instance (:constant index)))

(define-vop (instance-set slot-set)
  (:policy :fast-safe)
  ;; This is an invalid translation because %instance-set needs a
  ;; return value, and this VOP doesn't return anything.  I (RLT)
  ;; don't know how to fix this so that this VOP returns a value and
  ;; still works correctly when it is called directly by the compiler.
  ;; However, disabling the translation works around the bug.
  
  ;;(:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance (:constant index) *))

(define-vop (instance-index-ref word-index-ref)
  (:policy :fast-safe) 
  (:translate %instance-ref)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance positive-fixnum))

(define-vop (instance-index-set word-index-set)
  (:policy :fast-safe) 
  (:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance positive-fixnum *))



;;;; Code object frobbing.

(define-vop (code-header-ref word-index-ref)
  (:translate code-header-ref)
  (:policy :fast-safe)
  (:variant 0 other-pointer-type))

(define-vop (code-header-set word-index-set)
  (:translate code-header-set)
  (:policy :fast-safe)
  (:variant 0 other-pointer-type))

