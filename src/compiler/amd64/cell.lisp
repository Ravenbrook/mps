;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/amd64/cell.lisp,v 1.4 2004/07/29 20:14:53 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for the amd64.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997,1999.
;;; 

(in-package :amd64)



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
	 (value :scs (descriptor-reg any-reg immediate)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
     (if (sc-is value immediate)
        (let ((val (tn-value value)))
           (etypecase val
              (integer
	       (inst mov
		     (make-ea :qword :base object
			      :disp (- (* offset word-bytes) lowtag))
		     (fixnumize val)))
	      (symbol
	       (inst mov
		     (make-ea :qword :base object
			      :disp (- (* offset word-bytes) lowtag))
		     (+ nil-value (static-symbol-offset val))))
	      (character
	       (inst mov
		     (make-ea :qword :base object
			      :disp (- (* offset word-bytes) lowtag))
		     (logior (ash (char-code val) type-bits)
			     base-char-type)))))
       ;; Else, value not immediate.
       (storew value object offset lowtag))))



;;;; Symbol hacking VOPs:

;;; these next two cf the sparc version, by jrd.

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
  (:temporary (:sc descriptor-reg :from (:argument 0)) obj-temp))



;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
;;; This is a slow version. We don't need to check for nil if we move to 4-bit
;;; low-tag.
(define-vop (symbol-value)
  (:translate symbol-value)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 9
    (let ((err-lab (generate-error-code vop unbound-symbol-error object))
	  (itsnil (gen-label))
	  (done (gen-label)))

      (inst mov temp nil-value)
      (inst cmp object temp)
      (inst jmp :e itsnil)

      ;; not nil
      (loadw value object symbol-value-slot other-pointer-type)
      (inst cmp value unbound-marker-type)
      (inst jmp :e err-lab)
      (inst jmp done)
      
      (emit-label itsnil)
      (inst mov value nil-value)

      (emit-label done))))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-type)
  (:policy :fast)
  (:translate symbol-value))

(export 'kernel::set-symbol-value-conditional "KERNEL")
(defknown kernel::set-symbol-value-conditional (symbol t t) t (unsafe))

(define-vop (set-symbol-value-conditional cell-set-conditional)
  (:translate kernel::set-symbol-value-conditional)
  (:variant symbol-value-slot other-pointer-type)
  (:policy :fast-safe))

(defknown fast-symbol-value-xadd (symbol fixnum) fixnum ())
(define-vop (fast-symbol-value-xadd cell-xadd)
  (:variant symbol-value-slot other-pointer-type)
  (:policy :fast)
  (:translate fast-symbol-value-xadd)
  (:arg-types * tagged-num))

(define-vop (boundp)
  (:translate boundp)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:temporary (:sc descriptor-reg :from (:argument 0)) value)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-type)
    (inst cmp value unbound-marker-type)
    (inst jmp (if not-p :e :ne) target)))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types tagged-num)
  (:generator 2
    ;; the symbol-hash slot of NIL holds NIL because it is also the cdr
    ;; slot, so we have to strip off the two low bits to make sure it is
    ;; a fixnum.
    (loadw res symbol symbol-hash-slot other-pointer-type)
    (inst and res (lognot #b11))))

(define-vop (%set-symbol-hash cell-set)
  (:translate %set-symbol-hash)
  (:variant symbol-hash-slot other-pointer-type))


;;;; Fdefinition (fdefn) objects.

(define-vop (fdefn-function cell-ref)	; /pfw - alpha
  (:variant fdefn-function-slot other-pointer-type))

(define-vop (safe-fdefn-function)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 10
    (loadw value object fdefn-function-slot other-pointer-type)
    (inst cmp value nil-value)
    (let ((err-lab (generate-error-code vop undefined-symbol-error object)))
      (inst jmp :e err-lab))))

(define-vop (set-fdefn-function)
  (:policy :fast-safe)
  (:translate (setf fdefn-function))
  (:args (function :scs (descriptor-reg) :target result)
	 (fdefn :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) raw)
  (:temporary (:sc byte-reg) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (load-type type function (- function-pointer-type))
    (inst lea raw
	  (make-ea :byte :base function
		   :disp (- (* function-code-offset word-bytes)
			    function-pointer-type)))
    (inst cmp type function-header-type)
    (inst jmp :e normal-fn)
    (inst mov-imm raw (make-fixup (extern-alien-name "closure_tramp") :foreign))
    NORMAL-FN
    (storew function fdefn fdefn-function-slot other-pointer-type)
    (storew raw fdefn fdefn-raw-addr-slot other-pointer-type)
    (move result function)))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc any-reg) temp)
  (:generator 38
    (storew nil-value fdefn fdefn-function-slot other-pointer-type)
    (inst mov-imm temp
	  (make-fixup (extern-alien-name "undefined_tramp") :foreign))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-type)
    (move result fdefn)))



;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
	 (symbol :scs (descriptor-reg)))
  (:temporary (:sc descriptor-reg) temp)
  (:temporary (:sc any-reg) bsp)
  (:temporary (:sc any-reg) temp2)
  (:generator 5
    (load-symbol-value bsp *binding-stack-pointer*)
    (loadw temp symbol symbol-value-slot other-pointer-type)
    (inst add bsp (* binding-size word-bytes))
    (store-symbol-value bsp *binding-stack-pointer* temp2)
    (storew temp bsp (- binding-value-slot binding-size))
    (storew symbol bsp (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-type)))

(define-vop (unbind)
  (:temporary (:sc descriptor-reg) symbol value)
  (:temporary (:sc any-reg) bsp)
  (:generator 0
    (load-symbol-value bsp *binding-stack-pointer*)
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (loadw value bsp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-type)
    (storew 0 bsp (- binding-symbol-slot binding-size))
    (inst sub bsp (* binding-size word-bytes))
    (store-symbol-value bsp *binding-stack-pointer* symbol)))


(define-vop (unbind-to-here)
  (:args (where :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg) symbol value)
  (:temporary (:sc any-reg) bsp)
  (:generator 0
    (load-symbol-value bsp *binding-stack-pointer*)
    (inst cmp where bsp)
    (inst jmp :e done)

    LOOP
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (inst or symbol symbol)
    (inst jmp :z skip)
    (loadw value bsp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-type)
    (storew 0 bsp (- binding-symbol-slot binding-size))

    SKIP
    (inst sub bsp (* binding-size word-bytes))
    (inst cmp where bsp)
    (inst jmp :ne loop)
    (store-symbol-value bsp *binding-stack-pointer* symbol)

    DONE))



;;;; Closure indexing.

(define-full-reffer closure-index-ref *
  closure-info-offset function-pointer-type
  (any-reg descriptor-reg) * %closure-index-ref)

(define-full-setter set-funcallable-instance-info *
  funcallable-instance-info-offset function-pointer-type
  (any-reg descriptor-reg) * %set-funcallable-instance-info)

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


;;;; Structure hackery:

(define-vop (instance-length)
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 instance-pointer-type)
    (inst shr res type-bits)))

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
  instance-pointer-type (any-reg descriptor-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-type (any-reg descriptor-reg) * %instance-set)

(export 'kernel::%instance-set-conditional "KERNEL")
(defknown kernel::%instance-set-conditional (instance index t t) t
  (unsafe))

(define-vop (instance-set-conditional-c slot-set-conditional)
  (:policy :fast-safe)
  (:translate kernel::%instance-set-conditional)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance (:constant index) * *))

(define-vop (instance-set-conditional)
  (:translate kernel::%instance-set-conditional)
  (:args (object :scs (descriptor-reg) :to :eval)
	 (slot :scs (any-reg) :to :result)
	 (old-value :scs (descriptor-reg any-reg) :target rax)
	 (new-value :scs (descriptor-reg any-reg)))
  (:arg-types instance positive-fixnum * *)
  (:temporary (:sc descriptor-reg :offset rax-offset
		   :from (:argument 2) :to :result :target result)  rax)
  (:results (result :scs (descriptor-reg any-reg)))
  (:guard (backend-featurep :i486))
  (:policy :fast-safe)
  (:generator 5
    (move rax old-value)
    (inst cmpxchg (make-ea :qword :base object :index slot :scale 1
			   :disp (- (* instance-slots-offset word-bytes)
				    instance-pointer-type))
	  new-value)
    (move result rax)))

(defknown %instance-xadd (instance index fixnum) fixnum ())
(define-vop (instance-xadd-c slot-xadd)
  (:policy :fast-safe)
  (:translate %instance-xadd)
  (:variant instance-slots-offset instance-pointer-type)
  (:arg-types instance (:constant index) tagged-num))


;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-type
  (any-reg descriptor-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-type
  (any-reg descriptor-reg) * code-header-set)


;;;; Cons conditional setters.

(export 'kernel::rplaca-conditional "KERNEL")
(defknown kernel::rplaca-conditional (cons t t) t
  (unsafe))

(define-vop (rplaca-conditional cell-set-conditional)
  (:policy :fast-safe)
  (:translate kernel::rplaca-conditional)
  (:variant cons-car-slot list-pointer-type)
  (:arg-types list * *))

(export 'kernel::rplacd-conditional "KERNEL")
(defknown kernel::rplacd-conditional (cons t t) t
  (unsafe))

(define-vop (rplacd-conditional cell-set-conditional)
  (:policy :fast-safe)
  (:translate kernel::rplacd-conditional)
  (:variant cons-cdr-slot list-pointer-type)
  (:arg-types list * *))
