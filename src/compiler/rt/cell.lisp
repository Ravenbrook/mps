;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/cell.lisp,v 1.8 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/cell.lisp,v 1.8 1994/10/31 04:45:41 ram Exp $
;;;
;;; This file contains the VM definition of various primitive memory access
;;; VOPs for the IBM RT.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by Bill Chiles.
;;;

(in-package "RT")



;;;; Data object definition macros.

(vm:define-for-each-primitive-object (obj)
  (collect ((forms))
    (let ((lowtag (vm:primitive-object-lowtag obj)))
      (dolist (slot (vm:primitive-object-slots obj))
	(let* ((name (vm:slot-name slot))
	       (offset (vm:slot-offset slot))
	       (rest-p (vm:slot-rest-p slot))
	       (slot-opts (vm:slot-options slot))
	       (ref-trans (getf slot-opts :ref-trans))
	       (ref-vop (getf slot-opts :ref-vop ref-trans))
	       (set-trans (getf slot-opts :set-trans))
	       (setf-function-p (and (listp set-trans)
				     (= (length set-trans) 2)
				     (eq (car set-trans) 'setf)))
	       (setf-vop (getf slot-opts :setf-vop
			       (when setf-function-p
				 (intern (concatenate
					  'simple-string
					  "SET-"
					  (string (cadr set-trans)))))))
	       (set-vop (getf slot-opts :set-vop
			      (if setf-vop nil set-trans))))
	  (when ref-vop
	    (forms `(define-vop (,ref-vop ,(if rest-p 'slot-ref 'cell-ref))
				(:variant ,offset ,lowtag)
		      ,@(when ref-trans
			  `((:translate ,ref-trans))))))
	  (when (or set-vop setf-vop)
	    (forms `(define-vop ,(cond ((and rest-p setf-vop)
					(error "Can't automatically generate ~
					a setf VOP for :rest-p ~
					slots: ~S in ~S"
					       name
					       (vm:primitive-object-name obj)))
				       (rest-p `(,set-vop slot-set))
				       ((and set-vop setf-function-p)
					(error "Setf functions (list ~S) must ~
					use :setf-vops."
					       set-trans))
				       (set-vop `(,set-vop cell-set))
				       (setf-function-p
					`(,setf-vop cell-setf-function))
				       (t
					`(,setf-vop cell-setf)))
		      (:variant ,offset ,lowtag)
		      ,@(when set-trans
			  `((:translate ,set-trans)))))))))
    (when (forms)
      `(progn
	 ,@(forms)))))



;;;; Symbol hacking VOPs:

;;; CHECKED-CELL-REF -- VOP.
;;;
;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (word-pointer-reg descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; SYMBOL-VALUE -- VOP.
;;;
;;; Check that the value isn't the trap object.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp vm:symbol-value-slot vm:other-pointer-type)
    (let ((err-lab (generate-error-code vop unbound-symbol-error obj-temp)))
      (inst c value vm:unbound-marker-type)
      (inst bc :eq err-lab))))

;;; SYMBOL-FUNCTION -- VOP.
;;;
;;; Check that the result is a function, so NIL is always un-fbound.
;;;
(define-vop (symbol-function checked-cell-ref)
  (:translate symbol-function)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 10
    (move obj-temp object)
    (loadw value obj-temp vm:symbol-function-slot vm:other-pointer-type)
    (let ((err-lab (generate-error-code vop undefined-symbol-error obj-temp)))
      (test-type value temp err-lab t vm:function-pointer-type))))


;;; BOUNDP-FROB -- VOP.
;;;
;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
;;;
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
    (loadw value object vm:symbol-value-slot vm:other-pointer-type)
    (inst c value vm:unbound-marker-type)
    (if not-p
	;; If they're the same, symbol is unbound, so if not-p (the symbol is
	;; bound), then go to target.
	(inst bc :eq target)
	(inst bnc :eq target))))


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
    (loadw value object vm:symbol-function-slot vm:other-pointer-type)
    (test-type value temp target not-p vm:function-pointer-type)))

(define-vop (fast-symbol-value cell-ref)
  (:variant vm:symbol-value-slot vm:other-pointer-type)
  (:policy :fast)
  (:translate symbol-value))

(define-vop (fast-symbol-function cell-ref)
  (:variant vm:symbol-function-slot vm:other-pointer-type)
  (:policy :fast)
  (:translate symbol-function))

(define-vop (set-symbol-function)
  (:translate %set-symbol-function)
  (:policy :fast-safe)
  (:args (symbol :scs (descriptor-reg))
	 (function :scs (descriptor-reg) :target result))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) type)
  (:temporary (:scs (any-reg)) temp)
  (:save-p :compute-only)
  (:vop-var vop)
  (:generator 30
    (let ((closure (gen-label))
	  (normal-fn (gen-label)))
      (load-type type function vm:function-pointer-type)
      (inst c type vm:closure-header-type)
      (inst bc :eq closure)
      (inst c type funcallable-instance-header-type)
      (inst bc :eq closure)
      (inst c type vm:function-header-type)
      (inst bcx :eq normal-fn)
      (inst a temp function
	    (- (ash vm:function-header-code-offset vm:word-shift)
	       vm:function-pointer-type))
      (error-call vop kernel:object-not-function-error function)
      (emit-label closure)
      (inst cai temp (make-fixup "closure_tramp" :foreign))
      (emit-label normal-fn)
      (storew function symbol vm:symbol-function-slot vm:other-pointer-type)
      (storew temp symbol vm:symbol-raw-function-addr-slot vm:other-pointer-type)
      (move result function))))


(defknown fmakunbound/symbol (symbol) symbol (unsafe))
;;;
(deftransform fmakunbound ((symbol) (symbol))
  '(when symbol
     (fmakunbound/symbol symbol)))
;;;
(define-vop (fmakunbound/symbol)
  (:translate fmakunbound/symbol)
  (:policy :fast-safe)
  (:args (symbol :scs (descriptor-reg) :target result))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (sap-reg)) temp)
  (:generator 5
    (inst li temp vm:unbound-marker-type)
    (storew temp symbol vm:symbol-function-slot vm:other-pointer-type)
    (inst cai temp (make-fixup "undefined_tramp" :foreign))
    (storew temp symbol vm:symbol-raw-function-addr-slot vm:other-pointer-type)
    (move result symbol)))


;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
	 (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (word-pointer-reg)) bsp)
  (:generator 7
    (loadw temp symbol vm:symbol-value-slot vm:other-pointer-type)
    (load-symbol-value bsp *binding-stack-pointer*)
    (inst inc bsp (* vm:binding-size vm:word-bytes))
    (store-symbol-value bsp *binding-stack-pointer*)
    (storew temp bsp (- vm:binding-value-slot vm:binding-size))
    (storew symbol bsp (- vm:binding-symbol-slot vm:binding-size))
    (storew val symbol vm:symbol-value-slot vm:other-pointer-type)))

(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (word-pointer-reg)) bsp)
  (:generator 7
    (load-symbol-value bsp *binding-stack-pointer*)
    (loadw symbol bsp (- vm:binding-symbol-slot vm:binding-size))
    (loadw value bsp (- vm:binding-value-slot vm:binding-size))
    (storew value symbol vm:symbol-value-slot vm:other-pointer-type)
    (inst li value 0)
    (storew value bsp (- vm:binding-symbol-slot vm:binding-size))
    (inst dec bsp (* vm:binding-size vm:word-bytes))
    (store-symbol-value bsp *binding-stack-pointer*)))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (word-pointer-reg)) bsp)
  (:generator 0
    (let ((loop (gen-label))
	  (skip (gen-label))
	  (done (gen-label)))
      (move where arg)
      (load-symbol-value bsp *binding-stack-pointer*)
      (inst c where bsp)
      (inst bc :eq done)

      (emit-label loop)
      (loadw symbol bsp (- vm:binding-symbol-slot vm:binding-size))
      (inst c symbol 0)
      (inst bc :eq skip)
      (loadw value bsp (- vm:binding-value-slot vm:binding-size))
      (storew value symbol vm:symbol-value-slot vm:other-pointer-type)
      (inst li value 0)
      (storew value bsp (- vm:binding-symbol-slot vm:binding-size))

      (emit-label skip)
      (inst dec bsp (* vm:binding-size vm:word-bytes))
      (store-symbol-value bsp *binding-stack-pointer*)
      (inst c where bsp)
      (inst bnc :eq loop)

      (emit-label done))))



;;;; Closure indexing.

(define-vop (closure-index-ref word-index-ref)
  (:variant vm:closure-info-offset vm:function-pointer-type)
  (:translate %closure-index-ref))

(define-vop (set-funcallable-instance-info word-index-set)
  (:variant funcallable-instance-info-offset function-pointer-type)
  (:translate %set-funcallable-instance-info))


;;;; Structure hackery:

(define-vop (structure-length)
  (:policy :fast-safe)
  (:translate structure-length)
  (:args (struct :scs (descriptor-reg)))
  ;;(:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 structure-pointer-type)
    (inst sr res vm:type-bits)))

(define-vop (structure-ref slot-ref)
  (:variant structure-slots-offset structure-pointer-type)
  (:policy :fast-safe)
  (:translate structure-ref)
  (:arg-types structure (:constant index)))

(define-vop (structure-set slot-set)
  (:policy :fast-safe)
  (:translate structure-set)
  (:variant structure-slots-offset structure-pointer-type)
  (:arg-types structure (:constant index) *))

(define-vop (structure-index-ref word-index-ref)
  (:policy :fast-safe) 
  (:translate structure-ref)
  (:variant structure-slots-offset structure-pointer-type)
  (:arg-types structure positive-fixnum))

(define-vop (structure-index-set word-index-set)
  (:policy :fast-safe) 
  (:translate structure-set)
  (:variant structure-slots-offset structure-pointer-type)
  (:arg-types structure positive-fixnum *))



;;;; Extra random indexers.

(define-vop (code-header-ref word-index-ref)
  (:translate code-header-ref)
  (:policy :fast-safe)
  (:variant 0 other-pointer-type))

(define-vop (code-header-set word-index-set)
  (:translate code-header-set)
  (:policy :fast-safe)
  (:variant 0 other-pointer-type))


