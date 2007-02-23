;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/sap.lisp,v 1.17 1997/04/26 20:04:59 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/sap.lisp,v 1.17 1997/04/26 20:04:59 dtc Exp $
;;;
;;; This file contains the IBM RT VM definition of SAP operations.
;;;
;;; Written by William Lott.
;;;

(in-package "RT")



;;;; Moves and coercions:

;;; MOVE-TO-SAP -- VOP.
;;;
;;; Move a tagged SAP to an untagged representation.
;;;
(define-vop (move-to-sap)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:generator 1
    (loadw y x vm:sap-pointer-slot vm:other-pointer-type)))
;;;
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))


;;; MOVE-FROM-SAP -- VOP.
;;;
;;; Move an untagged SAP to a tagged representation.
;;;
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :save))
  (:temporary (:sc any-reg) header)
  (:temporary (:sc word-pointer-reg) alloc)
  (:results (y :scs (descriptor-reg)))
  (:generator 1
    (with-fixed-allocation (y header alloc sap-type sap-size)
      (storew sap y sap-pointer-slot other-pointer-type))))
;;;
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))


;;; SAP-MOVE -- VOP.
;;;
;;; Move untagged sap values.
;;;
(define-vop (sap-move)
  (:args (x :target y
	    :scs (sap-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
;;;
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))


;;; MOVE-SAP-ARGUMENT -- VOP.
;;;
;;; Move untagged sap arguments/return-values.
;;;
(define-vop (move-sap-argument)
  (:args (x :target y
	    :scs (sap-reg))
	 (fp :scs (word-pointer-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (sap-reg
       (move y x))
      (sap-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-sap-argument :move-argument
  (descriptor-reg sap-reg) (sap-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged sap to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (sap-reg) (descriptor-reg))



;;;; SAP-INT and INT-SAP

(define-vop (sap-int)
  (:args (sap :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sap-int)
  (:policy :fast-safe) 
 (:generator 1
    (move int sap)))

(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (move sap int)))



;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg) :target res)
	 (offset :scs (signed-reg) :to :save))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:policy :fast-safe)
  (:generator 2
    ;; Can't use CAS since offset and ptr may be register 0, so we have to move.
    (move res ptr)
    (inst a res offset)))
;;;
(define-vop (pointer+-c pointer+)
  (:args (ptr :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 16)))
  (:generator 1
    (inst a res ptr offset)))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg) :target res)
	 (ptr2 :scs (sap-reg) :to :save))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 1
    (move res ptr1)
    (inst s res ptr2)))



;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(eval-when (compile eval)

;;; DEFINE-SYSTEM-REF -- Internal Interface.
;;;
;;; Name is the name of a computed system-ref offset, from which we generate a
;;; <name>-c VOP for immediate constant offsets.  Shift is the multiples of two
;;; for which we must adjust the offset to make it an index in terms of bytes
;;; (the machines address units).  Translate and signed-translate are the Lisp
;;; function calls for which these VOP's are in-line expansions.
;;;
(defmacro define-system-ref (name size translate result-sc result-type
				  &optional
				  signed-translate signed-result-sc
				  signed-result-type)
  (let ((access-form 
	 (ecase size
	   (:byte
	    '((inst lc result base offset)
	      (when signed
		(inst sl result 24)
		(inst sar result 24))))
	   (:halfword
	    '((if signed
		  (inst lha result base offset)
		  (inst lh result base offset))))
	   (:word
	    '(signed ; suppress silly warnings.
	      (inst l result base offset)))))
	(name-c (symbolicate name "-C")))
    `(progn
       (define-vop (,name-c)
	 (:policy :fast-safe)
	 (:translate ,translate)
	 (:args (base :scs (sap-reg)))
	 (:results (result :scs (,result-sc)))
	 (:result-types ,result-type)
	 (:arg-types system-area-pointer (:constant (unsigned-byte 15)))
	 (:info offset)
	 (:variant-vars signed)
	 (:variant nil)
	 (:generator 5
	   ,@access-form))
       
       (define-vop (,name)
	 (:policy :fast-safe)
	 (:translate ,translate)
	 (:args (object :scs (sap-reg))
		(offset :scs (unsigned-reg) :target base))
	 (:results (result :scs (,result-sc)))
	 (:arg-types system-area-pointer positive-fixnum)
	 (:result-types ,result-type)
	 (:temporary (:scs (sap-reg) :from (:argument 1) :to :eval) base)
	 (:variant-vars signed)
	 (:variant nil)
	 (:generator 7
	   (inst cas base offset object)
	   (let ((offset 0))
	     ,@access-form)))
       
       ,@(when signed-translate
	   `((define-vop (,(symbolicate "SIGNED-" name-c) ,name-c)
	       (:translate ,signed-translate)
	       (:results (result :scs (,signed-result-sc)))
	       (:result-types ,signed-result-type)
	       (:variant t))
	     
	     (define-vop (,(symbolicate "SIGNED-" name) ,name)
	       (:translate ,signed-translate)
	       (:results (result :scs (,signed-result-sc)))
	       (:result-types ,signed-result-type)
	       (:variant t)))))))

) ;eval-when

(define-system-ref 8bit-system-ref :byte
  sap-ref-8 unsigned-reg positive-fixnum
  signed-sap-ref-8 signed-reg tagged-num)

(define-system-ref 16bit-system-ref :halfword
  sap-ref-16 unsigned-reg positive-fixnum
  signed-sap-ref-16 signed-reg tagged-num)

(define-system-ref 32bit-system-ref :word
  sap-ref-32 unsigned-reg unsigned-num
  signed-sap-ref-32 signed-reg signed-num)

(define-system-ref sap-system-ref :word
  sap-ref-sap sap-reg system-area-pointer)


(eval-when (compile eval)

;;; DEFINE-SYSTEM-SET -- Internal.
;;;
;;; Name is the name of a computed system-ref offset, from which we generate a
;;; <name>-c VOP for immediate constant offsets.  Shift is the multiples of two
;;; for which we must adjust the offset to make it an index in terms of bytes
;;; (the machines address units).  Translate and signed-translate are the Lisp
;;; function calls for which these VOP's are in-line expansions.
;;;
(defmacro define-system-set (name size translate data-sc data-type)
  (let ((set-form 
	 (ecase size
	   (:byte '(inst stc data base offset))
	   (:halfword '(inst sth data base offset))
	   (:word '(inst st data base offset))))
	(name-c (symbolicate name "-C")))
    `(progn
       (define-vop (,name-c)
	 (:policy :fast-safe)
	 (:translate ,translate)
	 (:args (base :scs (sap-reg))
		(data :scs (,data-sc) :target result :to (:result 0)))
	 (:arg-types system-area-pointer
		     (:constant (unsigned-byte 15))
		     ,data-type)
	 (:results (result :scs (,data-sc)))
	 (:result-types ,data-type)
	 (:info offset)
	 (:generator 5
	   ,set-form
	   (move result data)))
       
       (define-vop (,name)
	 (:policy :fast-safe)
	 (:translate ,translate)
	 (:args (object :scs (sap-reg))
		(offset :scs (unsigned-reg))
		(data :scs (,data-sc) :target result))
	 (:arg-types system-area-pointer positive-fixnum ,data-type)
	 (:temporary (:scs (sap-reg) :from (:argument 1)) base)
	 (:results (result :scs (,data-sc)))
	 (:result-types ,data-type)
	 (:generator 7
	   (inst cas base offset object)
	   (let ((offset 0))
	     ,set-form)
	   (move result data))))))

) ;EVAL-WHEN

(define-system-set 8bit-system-set :byte %set-sap-ref-8
  unsigned-reg positive-fixnum)

(define-system-set 16bit-system-set :halfword %set-sap-ref-16
  unsigned-reg positive-fixnum)

(define-system-set 32bit-system-set :word %set-sap-ref-32
  unsigned-reg unsigned-num)

(define-system-set signed-8bit-system-set :byte %set-signed-sap-ref-8
  signed-reg tagged-num)

(define-system-set signed-16bit-system-set :halfword %set-signed-sap-ref-16
  signed-reg tagged-num)

(define-system-set signed-32bit-system-set :word %set-signed-sap-ref-32
  signed-reg signed-num)


(define-system-set sap-system-set :word %set-sap-ref-sap
  sap-reg system-area-pointer)

#| 

Maybe we can get away with using define-system-set now that offsets don't 
need to be shifted.

;;; Ugly, because there are only 2 free sap-regs.  We stash the data value in
;;; NARGS to free up a sap-reg for BASE.
;;;
(define-vop (sap-system-set)
  (:policy :fast-safe)
  (:translate %set-sap-ref-sap)
  (:args (object :scs (sap-reg))
	 (offset :scs (unsigned-reg) :target base)
	 (data :scs (sap-reg sap-stack)))
  (:arg-types system-area-pointer positive-fixnum system-area-pointer)
  (:temporary (:scs (sap-reg) :from (:eval 0) :to (:eval 1)) base)
  (:temporary (:scs (non-descriptor-reg) :offset nargs-offset
	       :from (:eval 0) :to (:eval 1))
	      save)
  (:vop-var vop)
  (:results (result :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 7
    (sc-case data
      (sap-reg (move save data))
      (sap-stack
       (loadw save (current-nfp-tn vop) (* (tn-offset data) vm:word-bytes))))
       
    (inst cas base offset object)
    (inst st save base)
    (move result save)))

(define-vop (sap-system-set-c)
  (:policy :fast-safe)
  (:translate %set-sap-ref-sap)
  (:args (object :scs (sap-reg))
	 (data :scs (sap-reg) :target result))
  (:info offset)
  (:arg-types system-area-pointer
	      (:constant (unsigned-byte 16))
	      system-area-pointer)
  (:results (result :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 4
    (inst st data object offset)
    (move result data)))

|#

;;; fake the presence of sap-ref-single and sap-ref-double.

(def-source-transform sap-ref-single (sap offset)
  `(make-single-float (signed-sap-ref-32 ,sap ,offset)))

(def-source-transform %set-sap-ref-single (sap offset value)
  (once-only ((sap sap) (offset offset) (value value))
    `(progn
       (setf (signed-sap-ref-32 ,sap ,offset)
	     (single-float-bits ,value))
       ,value)))

(def-source-transform sap-ref-double (sap offset)
  (once-only ((sap sap) (offset offset))
    `(make-double-float (signed-sap-ref-32 ,sap ,offset)
			(sap-ref-32 ,sap (+ ,offset word-bytes)))))

(def-source-transform %set-sap-ref-double (sap offset value)
  (once-only ((sap sap) (offset offset) (value value))
    `(progn
       (setf (signed-sap-ref-32 ,sap ,offset)
	     (double-float-high-bits ,value))
       (setf (sap-ref-32 ,sap (+ ,offset word-bytes))
	     (double-float-low-bits ,value))
       ,value)))


;;;; Noise to convert normal lisp data objects into SAPs.

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst cal sap vector
	  (- (* vm:vector-data-offset vm:word-bytes) vm:other-pointer-type))))
