;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/cell.lisp,v 1.77 1994/10/31 04:44:16 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "MIPS")


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
	 (value :scs (descriptor-reg any-reg null zero)))
  (:info name offset lowtag #+gengc remember)
  (:ignore name)
  (:results)
  (:generator 1
    #+gengc
    (if remember
	(storew-and-remember-slot value object offset lowtag)
	(storew value object offset lowtag))
    #-gengc
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
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp symbol-value-slot other-pointer-type)
    (let ((err-lab (generate-error-code vop unbound-symbol-error obj-temp)))
      (inst xor temp value unbound-marker-type)
      (inst beq temp zero-tn err-lab)
      (inst nop))))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  (:temporary (:scs (non-descriptor-reg)) temp))

(define-vop (boundp boundp-frob)
  (:translate boundp)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-type)
    (inst xor temp value unbound-marker-type)
    (if not-p
	(inst beq temp zero-tn target)
	(inst bne temp zero-tn target))
    (inst nop)))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-type)
  (:policy :fast)
  (:translate symbol-value))

#+gengc
(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg null)))
  (:temporary (:scs (unsigned-reg) :from (:argument 0)) temp)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    ;; the symbol-hash slot of NIL holds NIL because it is also the cdr
    ;; slot, so we have to strip off the two low bits to make sure it is
    ;; a fixnum.
    (loadw res symbol symbol-hash-slot other-pointer-type)
    (inst li temp (lognot #b11))
    (inst and res res temp)))


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
    (let ((err-lab (generate-error-code vop undefined-symbol-error obj-temp)))
      (inst beq value null-tn err-lab))
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
      (inst nop)
      (inst xor type function-header-type)
      (inst beq type zero-tn normal-fn)
      (inst addu lip function
	    (- (ash function-code-offset word-shift)
	       function-pointer-type))
      (inst li lip (make-fixup "closure_tramp" :foreign))
      (emit-label normal-fn)
      (storew lip fdefn fdefn-raw-addr-slot other-pointer-type)
      (#+gengc storew-and-remember-object #-gengc storew
	       function fdefn fdefn-function-slot
	       other-pointer-type)
      (move result function))))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew null-tn fdefn fdefn-function-slot other-pointer-type)
    (inst li temp (make-fixup "undefined_tramp" :foreign))
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
    (loadw temp symbol symbol-value-slot other-pointer-type)
    (inst addu bsp-tn bsp-tn (* 2 word-bytes))
    (storew temp bsp-tn (- binding-value-slot binding-size))
    (storew symbol bsp-tn (- binding-symbol-slot binding-size))
    (#+gengc storew-and-remember-slot #-gengc storew
	     val symbol symbol-value-slot other-pointer-type)))


(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (#+gengc storew-and-remember-slot #-gengc storew
	     value symbol symbol-value-slot other-pointer-type)
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
    (inst addu bsp-tn bsp-tn (* -2 word-bytes))))


(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (let ((loop (gen-label))
	  (skip (gen-label))
	  (done (gen-label)))
      (move where arg)
      (inst beq where bsp-tn done)
      (inst nop)

      (emit-label loop)
      (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
      (inst beq symbol zero-tn skip)
      (loadw value bsp-tn (- binding-value-slot binding-size))
      (#+gengc storew-and-remember-slot #-gengc storew
	       value symbol symbol-value-slot other-pointer-type)
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

      (emit-label skip)
      (inst addu bsp-tn bsp-tn (* -2 word-bytes))
      (inst bne where bsp-tn loop)
      (inst nop)

      (emit-label done))))



;;;; Closure indexing.

(define-full-reffer closure-index-ref *
  closure-info-offset function-pointer-type
  (descriptor-reg any-reg) * %closure-index-ref)

(define-full-setter set-funcallable-instance-info *
  funcallable-instance-info-offset function-pointer-type
  (descriptor-reg any-reg null zero) * %set-funcallable-instance-info)

(define-full-reffer funcallable-instance-info *
  funcallable-instance-info-offset function-pointer-type
  (descriptor-reg any-reg) * %funcallable-instance-info)

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
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 instance-pointer-type)
    (inst srl res type-bits)))

(define-vop (instance-ref slot-ref)
  (:variant instance-slots-offset instance-pointer-type)
  (:policy :fast-safe)
  (:translate %instance-ref)
  (:arg-types instance (:constant index)))

(define-vop (instance-set slot-set)
  (:policy :fast-safe)
  (:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance (:constant index) *))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-type (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-type (descriptor-reg any-reg null zero) * %instance-set)



;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-type
  (descriptor-reg any-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-type
  (descriptor-reg any-reg null zero) * code-header-set)



;;;; Mutator accessing.

#+gengc (progn

(define-vop (mutator-ub32-ref)
  (:policy :fast-safe)
  (:args)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant-vars slot)
  (:generator 2
    (loadw res mutator-tn slot)))

(define-vop (mutator-descriptor-ref mutator-ub32-ref)
  (:results (res :scs (any-reg descriptor-reg)))
  (:result-types *))

(define-vop (mutator-sap-ref mutator-ub32-ref)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer))


(define-vop (mutator-ub32-set)
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant-vars slot)
  (:generator 2
    (storew arg mutator-tn slot)
    (move res arg)))

(define-vop (mutator-descriptor-set mutator-ub32-set)
  (:args (arg :scs (any-reg descriptor-reg null zero) :target res))
  (:arg-types *)
  (:results (res :scs (any-reg descriptor-reg)))
  (:result-types *))

(define-vop (mutator-sap-set mutator-ub32-set)
  (:args (arg :scs (sap-reg) :target res))
  (:arg-types system-area-pointer)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer))


(eval-when (compile eval)
  (defmacro define-mutator-accessors (slot type writable)
    (let ((ref (symbolicate "MUTATOR-" slot "-REF"))
	  (set (and writable (symbolicate "MUTATOR-" slot "-SET")))
	  (offset (symbolicate "MUTATOR-" slot "-SLOT"))
	  (fn
	   (let ((*package* (find-package :kernel)))
	     (symbolicate "MUTATOR-" slot))))
      (multiple-value-bind
	  (lisp-type ref-vop set-vop)
	  (ecase type
	    (:des
	     (values t 'mutator-descriptor-ref 'mutator-descriptor-set))
	    (:ub32
	     (values '(unsigned-byte 32) 'mutator-ub32-ref 'mutator-ub32-set))
	    (:sap
	     (values 'system-area-pointer 'mutator-sap-ref 'mutator-sap-set)))
      `(progn
	 (export ',fn :kernel)
	 (defknown ,fn () ,lisp-type (flushable))
	 (define-vop (,ref ,ref-vop)
	   (:translate ,fn)
	   (:variant ,offset))
	 ,@(when writable
	     `((defknown ((setf ,fn)) (,lisp-type) ,lisp-type (unsafe))
	       (define-vop (,set ,set-vop)
		 (:translate (setf ,fn))
		 (:variant ,offset)))))))))

(define-mutator-accessors thread :des t)
(define-mutator-accessors interrupts-disabled-count :ub32 t)
(define-mutator-accessors interrupt-pending :ub32 t)
(define-mutator-accessors control-stack-base :sap nil)
(define-mutator-accessors control-stack-end :sap nil)
(define-mutator-accessors current-unwind-protect :sap nil)
(define-mutator-accessors current-catch-block :sap nil)
(define-mutator-accessors binding-stack-base :sap nil)
(define-mutator-accessors binding-stack-end :sap nil)
(define-mutator-accessors number-stack-base :sap nil)
(define-mutator-accessors number-stack-end :sap nil)
(define-mutator-accessors eval-stack :des t)
(define-mutator-accessors eval-stack-top :ub32 t)
(define-mutator-accessors nursery-start :sap nil)
(define-mutator-accessors nursery-end :sap nil)
(define-mutator-accessors storebuf-start :sap nil)
(define-mutator-accessors storebuf-end :sap nil)
(define-mutator-accessors words-consed :ub32 nil)

); #+gengc progn
