;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/mips/alloc.lisp,v 1.9 2003/08/03 11:27:51 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle allocating simple objects.
;;;
;;; Written by William Lott.
;;;

(in-package "MIPS")

#+gengc
(progn

#+assembler
(defparameter large-alloc-entry (gen-label))

#+assembler
(define-assembly-routine
    var-alloc
    ((:arg lowtag unsigned-reg nl0-offset)
     (:arg header unsigned-reg nl1-offset)
     (:arg words any-reg nargs-offset)

     (:temp ndescr non-descriptor-reg nl2-offset)

     (:res result descriptor-reg a0-offset))

  ;; Is it a large object?
  (inst slt ndescr words (1+ (fixnumize large-object-cutoff)))
  (inst beq ndescr large-alloc-entry)
  (inst nop)

  ;; Round words up to a dual-word.  This is kinda sleezy, changing words like
  ;; this.  But we can because the vop thinks it is temporary.
  (inst addu words (+ (1- (ash 1 lowtag-bits))
		      (* vector-data-offset word-bytes)))
  (inst li ndescr (lognot lowtag-mask))
  (inst and words ndescr)

  ;; And allocate it in the nursery.
  (without-scheduling ()
    (inst or result alloc-tn lowtag)
    (inst sw header alloc-tn)
    (inst addu alloc-tn alloc-tn words)))

(define-assembly-routine
    (allocate-vector
     (:policy :fast-safe)
     (:translate allocate-vector)
     (:arg-types positive-fixnum positive-fixnum positive-fixnum))
    ((:arg type unsigned-reg nl1-offset)
     (:arg length any-reg nl2-offset)
     (:arg words any-reg nargs-offset)

     (:temp ndescr non-descriptor-reg nl0-offset)
     (:temp cfunc unsigned-reg cfunc-offset)
     (:temp nl4 unsigned-reg nl4-offset)

     (:res result descriptor-reg a0-offset))

  (progn cfunc nl4) ; ignore em

  ;; Add in space for the header word and length.
  (inst addu words (* vector-data-offset word-bytes))

  ;; Is it a large object?
  (inst slt ndescr words (1+ (fixnumize large-object-cutoff)))
  (inst beq ndescr large-alloc-entry)
  ;; Load ndescr with other-pointer-type because if we take the branch, we
  ;; need nl0 loaded with the desired lowtag.
  (inst li ndescr other-pointer-type)

  ;; Round words up to a dual-word.  This is kinda sleezy, changing words like
  ;; this.  But we can because the vop thinks it is temporary.
  (inst addu words (1- (ash 1 lowtag-bits)))
  (inst li ndescr (lognot lowtag-mask))
  (inst and words ndescr)

  ;; And allocate it.
  (without-scheduling ()
    (inst or result alloc-tn other-pointer-type)
    (storew type alloc-tn)
    (storew length alloc-tn vector-length-slot)
    (inst addu alloc-tn words)))

#+assembler
(defmacro with-regs-saved ((&rest regs) &body body)
  (let ((stack-space-needed (* (logandc2 (1+ (length regs)) 1) word-bytes))
	(sc-var (gensym "DESCRIPTOR-REG-SC-")))
    `(let ((,sc-var (sc-or-lose 'descriptor-reg)))
       (inst addu csp-tn ,stack-space-needed)
       ,(iterate repeat ((remaining regs) (offset (- word-bytes)))
	  (if remaining
	      (let* ((reg (car remaining))
		     (tn-var (gensym (concatenate 'string (symbol-name reg)
						  "-TN-")))
		     (reg-offset (symbolicate reg "-OFFSET")))
		`(let ((,tn-var
			(make-random-tn :kind :normal :sc ,sc-var
					:offset ,reg-offset)))
		   (inst sw ,tn-var csp-tn ,offset)
		   ,(repeat (cdr remaining) (- offset word-bytes))
		   (inst lw ,tn-var csp-tn ,offset)))
	      `(progn ,@body)))
       (inst subu csp-tn ,stack-space-needed))))

(define-assembly-routine
    (large-alloc
     (:info name)
     (:ignore name nl4 temp))
    ((:arg words any-reg nargs-offset)
     (:arg lowtag unsigned-reg nl0-offset)
     (:arg header any-reg nl1-offset)
     (:arg next-word any-reg nl2-offset)

     (:temp temp unsigned-reg cfunc-offset)
     (:temp nl4 unsigned-reg nl4-offset)

     (:res result descriptor-reg a0-offset))

  (progn words lowtag header next-word nl4)

  ;; var-alloc and allocate-vector jump to here if the result is going to
  ;; be large.
  (emit-label large-alloc-entry)

  ;; Disable interrupts.
  (loadw temp mutator-tn mutator-interrupts-disabled-count-slot)
  (inst addu temp temp 1)
  (storew temp mutator-tn mutator-interrupts-disabled-count-slot)

  (with-regs-saved (a1 a2 a3 a4 a5 fdefn lexenv ra)
    (inst li temp (make-fixup "allocate_large_object" :foreign))
    (inst jal (make-fixup "call_into_c" :foreign))
    (inst nop))

  (inst move result temp)

  ;; Re-enable interrupts.
  (loadw temp mutator-tn mutator-interrupts-disabled-count-slot)
  (inst subu temp temp 1)
  (inst bne temp zero-tn done)
  (storew temp mutator-tn mutator-interrupts-disabled-count-slot)
  ;; Check to see if any are pending.
  (loadw temp mutator-tn mutator-interrupt-pending-slot)
  (inst beq temp zero-tn done)
  (inst nop)

  (inst break pending-interrupt-trap)
  DONE)

); #+gengc progn
