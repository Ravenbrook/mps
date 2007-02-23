;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/mc68881.lisp,v 1.12 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; The following code is to support the MC68881 floating point chip on the APC
;;; card.  Adapted by Rob MacLachlan from the Sparc support, written by Rob
;;; MacLachlan and William Lott, with some stuff from Dave McDonald's original
;;; RT miscops.
;;;
(in-package "RT")

(eval-when (compile eval load)

;;; The actual positions of the info in the mc68881 FPCR and FPSR.
;;;
(defconstant mc68881-fpcr-rounding-mode-byte (byte 2 4))
(defconstant mc68881-fpcr-rounding-precision-byte (byte 2 6))
(defconstant mc68881-fpcr-traps-byte (byte 8 8))
(defconstant mc68881-fpsr-accrued-exceptions-byte (byte 5 3))
(defconstant mc68881-fpsr-current-exceptions-byte (byte 8 8))
(defconstant mc68881-fpsr-condition-code-byte (byte 4 24))

;;; Amount to shift by the get the condition code, - 16.
;;;
(defconstant mc68881-fpsr-condition-code-shift-16 8)

;;; The condition code bits.
;;;
(defconstant mc68881-nan-condition (ash 1 0))
(defconstant mc68881-infinity-condition (ash 1 1))
(defconstant mc68881-zero-condition (ash 1 2))
(defconstant mc68881-negative-condition (ash 1 3))

;;; Masks that map the extended set of exceptions implemented by the 68881 to
;;; the IEEE exceptions.  This extended format is used for the enabled traps
;;; and the current exceptions.
;;;
(defconstant mc68881-invalid-exception (ash #b111 5))
(defconstant mc68881-overflow-exception (ash 1 4))
(defconstant mc68881-underflow-exception (ash 1 3))
(defconstant mc68881-divide-zero-exception (ash 1 2))
(defconstant mc68881-inexact-exception (ash #b11 0))

;;; Encoding of float exceptions in the FLOATING-POINT-MODES result.  This is
;;; also the encoding used in the mc68881 accrued exceptions.
;;;
(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-divide-by-zero-trap-bit (ash 1 1))
(defconstant float-underflow-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-negative 2)
(defconstant float-round-to-positive 3)

;;; Positions of bits in the FLOATING-POINT-MODES result.
;;;
(defconstant float-rounding-mode (byte 2 0))
(defconstant float-sticky-bits (byte 5 2))
(defconstant float-traps-byte (byte 5 7))
(defconstant float-exceptions-byte (byte 5 12))
(defconstant float-fast-bit 0)

); eval-when

;;; When compared to the 68881 documentation, the RT only uses the low 16 bits
;;; of the instruction.  Memory access is controlled in an RT specific way.
;;; See the AFPA coprocessor hardware assist operation (page B34 in volume 1.)


;;;; Move functions:
;;;
;;; See With-FP-Temp comment...

(define-move-function (load-single 7) (vop x y)
  ((single-stack) (mc68881-single-reg))
  (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset x) vm:word-bytes))
  (with-fp-temp (temp)
    (inst mc68881-load y lip-tn :single temp)))

(define-move-function (store-single 8) (vop x y)
  ((mc68881-single-reg) (single-stack))
  (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes))
  (with-fp-temp (temp)
    (inst mc68881-store x lip-tn :single temp)))

(define-move-function (load-double 7) (vop x y)
  ((double-stack) (mc68881-double-reg))
  (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset x) vm:word-bytes))
  (with-fp-temp (temp)
    (inst mc68881-load y lip-tn :double temp)))

(define-move-function (store-double 8) (vop x y)
  ((mc68881-double-reg) (double-stack))
  (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes))
  (with-fp-temp (temp)
    (inst mc68881-store x lip-tn :double temp)))


;;;; Move VOPs:

(define-vop (mc68881-move)
  (:args (x :scs (mc68881-single-reg mc68881-double-reg)
	    :target y
	    :load-if (not (location= x y))))
  (:results (y :scs (mc68881-single-reg mc68881-double-reg)
	       :load-if (not (location= x y))))
  (:temporary (:sc sap-reg) temp)
  (:generator 0
    (unless (location= y x)
      (inst mc68881-move y x temp))))

(define-move-vop mc68881-move :move
  (mc68881-single-reg) (mc68881-single-reg)
  (mc68881-double-reg) (mc68881-double-reg))


(define-vop (move-to-mc68881)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (mc68881-single-reg mc68881-double-reg)))
  (:temporary (:sc sap-reg) temp)
  (:variant-vars format data)
  (:generator 7
    (inst cal lip-tn x (- (* data vm:word-bytes) vm:other-pointer-type))
    (inst mc68881-load y lip-tn format temp)))

(macrolet ((frob (name format data sc)
	     `(progn
		(define-vop (,name move-to-mc68881)
		  (:variant ,format ,data))
		(define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-mc68881-single :single vm:single-float-value-slot
    mc68881-single-reg)
  (frob move-to-mc68881-double :double vm:double-float-value-slot
    mc68881-double-reg))


(define-vop (move-from-mc68881)
  (:args (x :scs (mc68881-single-reg mc68881-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (sap-reg)) ndescr)
  (:temporary (:scs (word-pointer-reg)) alloc)
  (:variant-vars format size type data)
  (:generator 20
    (with-fixed-allocation (y ndescr alloc type size)
      (inst cal lip-tn y (- (* data vm:word-bytes) vm:other-pointer-type))
      (inst mc68881-store x lip-tn format ndescr))))

(macrolet ((frob (name sc &rest args)
	     `(progn
		(define-vop (,name move-from-mc68881)
		  (:variant ,@args))
		(define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-mc68881-single mc68881-single-reg
    :single vm:single-float-size vm:single-float-type
    vm:single-float-value-slot)
  (frob move-from-mc68881-double mc68881-double-reg
    :double vm:double-float-size vm:double-float-type
    vm:double-float-value-slot))

(define-vop (move-to-mc68881-argument)
  (:args (x :scs (mc68881-single-reg mc68881-double-reg) :target y)
	 (nfp :scs (word-pointer-reg)
	      :load-if (not (sc-is y mc68881-single-reg mc68881-double-reg))))
  (:results (y))
  (:temporary (:sc sap-reg) temp)
  (:variant-vars format)
  (:vop-var vop)
  (:generator 7
    (sc-case y
      ((mc68881-single-reg mc68881-double-reg)
       (unless (location= y x)
	 (inst mc68881-move y x temp)))
      ((single-stack double-stack)
       (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes))
       (inst mc68881-store y lip-tn format temp)))))

(macrolet ((frob (name format sc)
	     `(progn
		(define-vop (,name move-to-mc68881-argument)
		  (:variant ,format))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-mc68881-single-float-argument :single mc68881-single-reg)
  (frob move-mc68881-double-float-argument :double mc68881-double-reg))

(define-move-vop move-argument :move-argument
  (mc68881-single-reg mc68881-double-reg) (descriptor-reg))


;;;; Arithmetic VOPs:

(define-vop (mc68881-op)
  (:args (x) (y))
  (:results (r))
  (:temporary (:sc sap-reg) temp)
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name mc68881-op)
		(:args (x :scs (,sc) :target r)
		       (y :scs (,sc)))
		(:results (r :scs (,sc) :from (:argument 0)))
		(:arg-types ,ptype ,ptype)
		(:result-types ,ptype)
		(:variant-vars op)
		(:generator 20
		  (unless (location= x r)
		    (inst mc68881-move r x temp))
		  (note-this-location vop :internal-error)
		  (inst mc68881-binop r y op temp)))))
  (frob mc68881-single-float-op mc68881-single-reg mc68881-single-float)
  (frob mc68881-double-float-op mc68881-double-reg mc68881-double-float))

(macrolet ((frob (op sinst sname dinst dname)
	     `(progn
		(define-vop (,sname mc68881-single-float-op)
		  (:translate ,op)
		  (:variant ,sinst))
		(define-vop (,dname mc68881-double-float-op)
		  (:translate ,op)
		  (:variant ,dinst)))))
  (frob + :add +/single-float :add +/double-float)
  (frob - :sub -/single-float :sub -/double-float)
  (frob * :sglmul */single-float :mul */double-float)
  (frob / :sgldiv //single-float :div //double-float))

(define-vop (mc68881-unop mc68881-op)
  (:args (x)))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name mc68881-unop)
		(:args (x :scs (,sc)))
		(:results (r :scs (,sc)))
		(:arg-types ,ptype)
		(:result-types ,ptype)
		(:variant-vars op)
		(:generator 20
		  (inst mc68881-unop r x op temp)))))
  (frob mc68881-single-float-unop mc68881-single-reg mc68881-single-float)
  (frob mc68881-double-float-unop mc68881-double-reg mc68881-double-float))


(macrolet ((frob (op sinst sname dinst dname)
	     `(progn
		(define-vop (,sname mc68881-single-float-unop)
		  (:translate ,op)
		  (:variant ,sinst))
		(define-vop (,dname mc68881-double-float-unop)
		  (:translate ,op)
		  (:variant ,dinst)))))
  (frob abs :abs abs/single-float :abs abs/double-float)
  (frob %negate :neg %negate/single-float :neg %negate/double-float))


;;;; Comparison:

(define-vop (mc68881-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:sc sap-reg) temp)
  (:temporary (:sc descriptor-reg) loc)
  (:temporary (:sc unsigned-stack) loc-tn)
  (:variant-vars condition)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 20
    (let ((drop-thru (gen-label)))
      (note-this-location vop :internal-error)
      (if (eq condition '<)
	  (inst mc68881-compare y x :cmp temp)
	  (inst mc68881-compare x y :cmp temp))
      (let ((nfp (current-nfp-tn vop)))
	(inst cal loc nfp (* (tn-offset loc-tn) word-bytes)))
      (inst mc68881-store-status :fpsr loc temp)
      (loadw temp loc)
      (ecase condition
	((< >)
	 (inst niuz temp temp
	       (ash (logior mc68881-zero-condition
			    mc68881-negative-condition
			    mc68881-nan-condition)
		    mc68881-fpsr-condition-code-shift-16)))
	(eql
	 (inst niuz temp temp
	       (ash mc68881-zero-condition
		    mc68881-fpsr-condition-code-shift-16))
	 (setq not-p (not not-p))))
      
      (if not-p
	  (inst bnc :eq target)
	  (inst bc :eq target))
      (emit-label drop-thru))))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name mc68881-compare)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:arg-types ,ptype ,ptype))))
  (frob mc68881-single-float-compare mc68881-single-reg mc68881-single-float)
  (frob mc68881-double-float-compare mc68881-double-reg mc68881-double-float))

(macrolet ((frob (translate sname dname)
	     `(progn
		(define-vop (,sname mc68881-single-float-compare)
		  (:translate ,translate)
		  (:variant ',translate))
		(define-vop (,dname mc68881-double-float-compare)
		  (:translate ,translate)
		  (:variant ',translate)))))
  (frob < mc68881-</single-float mc68881-</double-float)
  (frob > mc68881->/single-float mc68881->/double-float)
  (frob eql mc68881-eql/single-float mc68881-eql/double-float))


;;;; Conversion:

(macrolet ((frob (name translate to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (signed-reg) :target temp
			  :load-if (not (sc-is x signed-stack))))
		(:temporary (:scs (single-stack)) temp)
		(:temporary (:sc word-pointer-reg) addr)
		(:temporary (:sc sap-reg) scratch)
		(:results (y :scs (,to-sc)))
		(:arg-types signed-num)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 20
		  (let ((stack-tn
			 (sc-case x
			   (signed-reg
			    (storew x
				    (current-nfp-tn vop)
				    (* (tn-offset temp) vm:word-bytes))
			    temp)
			   (signed-stack
			    x))))
		    (inst cal addr (current-nfp-tn vop) 
			 (* (tn-offset stack-tn) vm:word-bytes))
		    (note-this-location vop :internal-error)
		    (inst mc68881-load y addr :integer scratch))))))
  (frob mc68881-%single-float/signed %single-float
    mc68881-single-reg mc68881-single-float)
  (frob mc68881-%double-float/signed %double-float
    mc68881-double-reg mc68881-double-float))

;;; Everything is represented as extended precision, so these operations don't
;;; really do anything.  (Or, rather, whatever semantics there is, is
;;; automatically handled by the load functions.)
;;;
(macrolet ((frob (name translate from-sc from-type to-sc to-type)
	     `(define-vop (,name mc68881-move)
		(:args (x :scs (,from-sc) :target y))
		(:results (y :scs (,to-sc)))
		(:arg-types ,from-type)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate))))
  (frob mc68881-%single-float/double-float %single-float
    mc68881-double-reg mc68881-double-float
    mc68881-single-reg mc68881-single-float)
  (frob mc68881-%double-float/single-float %double-float
    mc68881-single-reg mc68881-single-float
    mc68881-double-reg mc68881-double-float))

(macrolet ((frob (trans from-sc from-type inst)
	     `(define-vop (,(symbolicate trans "/" from-type))
		(:args (x :scs (,from-sc) :target temp))
		(:temporary (:from (:argument 0) :sc mc68881-single-reg) temp)
		(:temporary (:sc sap-reg) scratch)
		(:temporary (:scs (signed-stack)) stack-temp)
		(:temporary (:sc word-pointer-reg) addr)
		(:results (y :scs (signed-reg)
			     :load-if (not (sc-is y signed-stack))))
		(:arg-types ,from-type)
		(:result-types signed-num)
		(:translate ,trans)
		(:policy :fast-safe)
		(:note "inline float truncate")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (note-this-location vop :internal-error)
		  (inst mc68881-unop temp x ,inst scratch)
		  (sc-case y
		    (signed-stack
		     (inst cal addr (current-nfp-tn vop)
			  (* (tn-offset y) vm:word-bytes))
		     (inst mc68881-store temp addr :integer scratch))
		    (signed-reg
		     (inst cal addr (current-nfp-tn vop)
			  (* (tn-offset stack-temp) vm:word-bytes))
		     (inst mc68881-store temp addr :integer scratch)
		     (loadw y (current-nfp-tn vop)
			    (tn-offset stack-temp))))))))
  (frob %unary-truncate mc68881-single-reg mc68881-single-float :intrz)
  (frob %unary-truncate mc68881-double-reg mc68881-double-float :intrz)
  (frob %unary-round mc68881-single-reg mc68881-single-float :int)
  (frob %unary-round mc68881-double-reg mc68881-double-float :int))


(define-vop (make-mc68881-single-float)
  (:args (bits :scs (signed-reg) :target res
	       :load-if (not (sc-is bits signed-stack))))
  (:results (res :scs (mc68881-single-reg)
		 :load-if (not (sc-is res single-stack))))
  (:temporary (:scs (signed-reg) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (signed-stack)) stack-temp)
  (:temporary (:sc sap-reg) scratch)
  (:temporary (:sc word-pointer-reg) addr)
  (:arg-types signed-num)
  (:result-types mc68881-single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 20
    (sc-case bits
      (signed-reg
       (sc-case res
	 (mc68881-single-reg
	  (storew bits (current-nfp-tn vop) (tn-offset stack-temp))
	  (inst cal addr (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst mc68881-load res addr :single scratch))
	 (single-stack
	  (storew bits (current-nfp-tn vop) (tn-offset res)))))
      (signed-stack
       (sc-case res
	 (mc68881-single-reg
	  (inst cal addr (current-nfp-tn vop)
		(* (tn-offset bits) vm:word-bytes))
	  (inst mc68881-load res addr :single scratch))
	 (single-stack
	  (unless (location= bits res)
	    (loadw temp (current-nfp-tn vop) (tn-offset bits))
	    (storew temp (current-nfp-tn vop) (tn-offset res)))))))))

(define-vop (make-mc68881-double-float)
  (:args (hi-bits :scs (signed-reg))
	 (lo-bits :scs (unsigned-reg)))
  (:arg-types signed-num unsigned-num)
  (:results (res :scs (mc68881-double-reg)
		 :load-if (not (sc-is res double-stack))))
  (:result-types mc68881-double-float)
  (:temporary (:scs (double-stack)) temp)
  (:temporary (:sc sap-reg :from (:eval 0)) scratch)
  (:temporary (:sc word-pointer-reg :from (:eval 0)) addr)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 25
    (let ((stack-tn (sc-case res
		      (double-stack res)
		      (mc68881-double-reg temp))))
      (storew hi-bits (current-nfp-tn vop) (tn-offset stack-tn))
      (storew lo-bits (current-nfp-tn vop) (1+ (tn-offset stack-tn))))
    (when (sc-is res mc68881-double-reg)
      (inst cal addr (current-nfp-tn vop) (* (tn-offset temp) vm:word-bytes))
      (inst mc68881-load res addr :double scratch))))

(define-vop (mc68881-single-float-bits)
  (:args (float :scs (mc68881-single-reg)))
  (:results (bits :scs (signed-reg)))
  (:temporary (:scs (signed-stack)) stack-temp)
  (:temporary (:sc sap-reg) scratch)
  (:temporary (:sc word-pointer-reg) addr)
  (:arg-types mc68881-single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 20
    (inst cal addr (current-nfp-tn vop)
	  (* (tn-offset stack-temp) vm:word-bytes))
    (inst mc68881-store float addr :single scratch)
    (loadw bits (current-nfp-tn vop) (tn-offset stack-temp))))

(define-vop (mc68881-double-float-high-bits)
  (:args (float :scs (mc68881-double-reg)))
  (:results (bits :scs (signed-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:temporary (:sc sap-reg) scratch)
  (:temporary (:sc word-pointer-reg) addr)
  (:arg-types mc68881-double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:variant-vars offset)
  (:variant 0)
  (:generator 20
    (inst cal addr (current-nfp-tn vop)
	  (* (tn-offset stack-temp) vm:word-bytes))
    (inst mc68881-store float addr :double scratch)
    (loadw bits (current-nfp-tn vop) (+ (tn-offset stack-temp) offset))))

(define-vop (mc68881-double-float-low-bits mc68881-double-float-high-bits)
  (:results (bits :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant 1)
  (:translate double-float-low-bits))


;;;; Float mode hackery:

(deftype float-modes () '(unsigned-byte 32))
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes)
  float-modes)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  #+nil (:vop-var vop)
  #+nil (:temporary (:sc unsigned-stack) temp)
  (:generator 3
    #+nil
    (let ((nfp (current-nfp-tn vop)))
      (inst stfsr nfp (* word-bytes (tn-offset temp)))
      (loadw res nfp (tn-offset temp))
      (inst nop))
    (inst li res 0)))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  #+nil (:temporary (:sc unsigned-stack) temp)
  #+nil (:vop-var vop)
  (:generator 3
    #+nil
    (let ((nfp (current-nfp-tn vop)))
      (storew new nfp (tn-offset temp))
      (inst ldfsr nfp (* word-bytes (tn-offset temp)))
      (move res new))
    (move res new)))
