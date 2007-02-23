;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the RT definitions of some general purpose memory
;;; reference VOPs inherited by basic memory reference operations.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.  Cell-Setf is similar to
;;; Cell-Set, but delivers the new value as the result.
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset)))
;;;
(define-vop (cell-setf)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)
		:target result))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset)
    (unless (location= value result)
      (inst lr result value))))

;;; Define-Cell-Accessors  --  Interface
;;;
;;;    Define accessor VOPs for some cells in an object.  If the operation name
;;; is NIL, then that operation isn't defined.  If the translate function is
;;; null, then we don't define a translation.
;;;
(defmacro define-cell-accessors (offset ref-op ref-trans set-op set-trans)
  `(progn
     ,@(when ref-op
	 `((define-vop (,ref-op cell-ref)
	     (:variant ,offset)
	     ,@(when ref-trans
		 `((:translate ,ref-trans))))))
     ,@(when set-op
	 `((define-vop (,set-op cell-setf)
	     (:variant ,offset)
	     ,@(when set-trans
		 `((:translate ,set-trans))))))))


;;; Slot-Ref and Slot-Set are used to define VOPs like Closure-Ref, where the
;;; offset is constant at compile time, but varies for different uses.  We add
;;; in the stardard g-vector overhead.
;;;
(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:info offset)
  (:generator 4
    (load-slot value object offset)))
;;;
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:info offset)
  (:generator 4
    (store-slot value object offset)))



;;;; Indexed references:


;;; Define-Indexer  --  Internal
;;;
;;;    Define some VOPs for indexed memory reference.  Unless the index is
;;; constant, we must compute an intermediate result in a boxed temporary,
;;; since the RT doesn't have any indexed addressing modes.  This means that GC
;;; has to adjust the "raw" pointer in Index-Temp by observing that Index-Temp
;;; points within Object-Temp.  After we are done, we clear Index-Temp so that
;;; we don't raw pointers lying around.
;;;
(defmacro define-indexer (name write-p op shift)
  `(define-vop (,name)
     (:args (object :scs (descriptor-reg) :target object-temp)
	    (index :scs (any-reg descriptor-reg short-immediate
				 unsigned-immediate)
		   :target index-temp)
	    ,@(when write-p
		'((value :scs (any-reg descriptor-reg) :target result))))
     (:results (,(if write-p 'result 'value)
		:scs (any-reg descriptor-reg)))
     (:variant-vars offset)
     (:temporary (:scs (descriptor-reg)
		       :from (:argument 0))
		 object-temp)
     (:temporary (:scs (descriptor-reg)
		       :from (:argument 1))
		 index-temp)
     (:policy :fast-safe)
     (:generator 5
       (sc-case index
	 ((short-immediate unsigned-immediate)
	  (,op value object (+ (tn-value index) offset)))
	 (t
	  (unless (location= object object-temp)
	    (inst lr object-temp object))
	  (unless (location= index index-temp)
	    (inst lr index-temp index))
	  
	  ,@(unless (zerop shift)
	      `((inst sli index-temp ,shift)))
	  
	  (inst a index-temp object-temp)
	  (,op value index-temp offset)
	  (loadi index-temp 0)))
       
       ,@(when write-p
	   `((unless (location= value result)
	       (inst lr result value)))))))

(define-indexer word-index-ref nil loadw 2)
(define-indexer word-index-set t storew 2)
(define-indexer halfword-index-ref nil loadh 1)
(define-indexer signed-halfword-index-ref nil loadha 1)
(define-indexer halfword-index-set t storeha 1)
(define-indexer byte-index-ref nil loadc 0)
(define-indexer byte-index-set t storec 0)
