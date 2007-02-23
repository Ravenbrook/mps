;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/afpa.lisp,v 1.5 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; The following code is to support the AFPA on the EAPC card and the
;;; accessory AFPA for the APC card.  There isn't heavy use of AFPA features;
;;; other than the size of the register set, and the DMA use, this would work
;;; on the FPA (supposing anyone still has one of those.)  This is adapted from
;;; the 68881 support, and from the MIPS support.
;;;
;;; See section 4 in Vol 1 of the RT technical manual.  In particular DMA is
;;; described on 4-96 to 4-98.
;;;
(in-package "RT")

;;;; Status register formats.

(defconstant afpa-rounding-mode-byte (byte 2 (- 31 24)))
(defconstant afpa-compare-result-byte (byte 2 (- 31 22)))

;;; The condition code bits.
;;;
(defconstant afpa-compare-gtr #b00)
(defconstant afpa-compare-eql #b01)
(defconstant afpa-compare-lss #b10)
(defconstant afpa-compare-unordered #b11)

;;; ### Note: the following status register constants are totally bogus (are
;;; actually for the mc68881, and exist only to make some code compile without
;;; errors.
;;;
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


;;;; Move functions:
;;;
;;;    Since moving between memory and a FP register reqires *two* temporaries,
;;; we need a special temporary to form the magic address we store to do a
;;; floating point operation.  We get this temp by always spilling NL0 on the
;;; number stack.  See WITH-FP-TEMP in the 68881 support.
;;;
;;; We also use LIP to form the address of the data location that we are
;;; reading or writing.


(define-move-function (load-afpa-single 7) (vop x y)
  ((single-stack) (afpa-single-reg))
  (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset x) vm:word-bytes))
  (with-fp-temp (temp)
    (inst afpa-load y lip-tn :single temp)
    (inst afpa-noop temp)))

(define-move-function (store-single 8) (vop x y)
  ((afpa-single-reg) (single-stack))
  (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes))
  (with-fp-temp (temp)
    (inst afpa-store x lip-tn :single temp)
    (inst afpa-noop temp)))

(define-move-function (load-double 7) (vop x y)
  ((double-stack) (afpa-double-reg))
  (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset x) vm:word-bytes))
  (with-fp-temp (temp)
    (inst afpa-load y lip-tn :double temp)
    (inst afpa-noop temp)))

(define-move-function (store-double 8) (vop x y)
  ((afpa-double-reg) (double-stack))
  (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes))
  (with-fp-temp (temp)
    (inst afpa-store x lip-tn :double temp)
    (inst afpa-noop temp)))


;;;; Move VOPs:


(macrolet ((frob (vop sc inst)
	     `(progn
		(define-vop (,vop)
		  (:args (x :scs (,sc)
			    :target y
			    :load-if (not (location= x y))))
		  (:results (y :scs (,sc)
			       :load-if (not (location= x y))))
		  (:note "float move")
		  (:temporary (:sc sap-reg) temp)
		  (:generator 0
		    (unless (location= y x)
		      (inst afpa-unop y x ,inst temp))))
		(define-move-vop ,vop :move (,sc) (,sc)))))
  (frob afpa-single-move afpa-single-reg :cops)
  (frob afpa-double-move afpa-double-reg :copl))



(macrolet ((frob (name format data sc)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (descriptor-reg)))
		  (:results (y :scs (,sc)))
		  (:temporary (:sc sap-reg) temp)
		  (:generator 7
		    (inst cal lip-tn x (- (* ,data vm:word-bytes)
					  vm:other-pointer-type))
		    (inst afpa-load y lip-tn ,format temp)
		    (inst afpa-noop temp)))
		(define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-afpa-single :single vm:single-float-value-slot
    afpa-single-reg)
  (frob move-to-afpa-double :double vm:double-float-value-slot
    afpa-double-reg))



(macrolet ((frob (name sc format size type data)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (afpa-single-reg afpa-double-reg) :to :save))
		  (:results (y :scs (descriptor-reg)))
		  (:temporary (:scs (sap-reg)) ndescr)
		  (:temporary (:scs (word-pointer-reg)) alloc)
		  (:generator 20
		    (with-fixed-allocation (y ndescr alloc ,type ,size)
		      (inst cal lip-tn y (- (* ,data vm:word-bytes)
					    vm:other-pointer-type))
		      (inst afpa-store x lip-tn ,format ndescr)
		      (inst afpa-noop ndescr))))
		(define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-afpa-single afpa-single-reg
    :single vm:single-float-size vm:single-float-type
    vm:single-float-value-slot)
  (frob move-from-afpa-double afpa-double-reg
    :double vm:double-float-size vm:double-float-type
    vm:double-float-value-slot))

(define-vop (move-to-afpa-argument)
  (:args (x :scs (afpa-single-reg afpa-double-reg))
	 (nfp :scs (word-pointer-reg)
	      :load-if (not (sc-is y afpa-single-reg afpa-double-reg))))
  (:results (y))
  (:temporary (:sc sap-reg) temp)
  (:variant-vars format)
  (:vop-var vop)
  (:generator 7
    (sc-case y
      ((afpa-single-reg afpa-double-reg)
       (unless (location= y x)
	 (inst afpa-move y x format temp)))
      ((single-stack double-stack)
       (inst cal lip-tn (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes))
       (inst afpa-store y lip-tn format temp)
       (inst afpa-noop temp)))))

(macrolet ((frob (name format sc)
	     `(progn
		(define-vop (,name move-to-afpa-argument)
		  (:variant ,format))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-afpa-single-float-argument :single afpa-single-reg)
  (frob move-afpa-double-float-argument :double afpa-double-reg))

(define-move-vop move-argument :move-argument
  (afpa-single-reg afpa-double-reg) (descriptor-reg))


;;;; Arithmetic VOPs:

(define-vop (afpa-op)
  (:args (x) (y))
  (:results (r))
  (:temporary (:sc sap-reg) temp)
  (:policy :fast-safe)
  (:guard (eq *target-float-hardware* :afpa))
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype format)
	     `(define-vop (,name afpa-op)
		(:args (x :scs (,sc) :target r)
		       (y :scs (,sc)))
		(:results (r :scs (,sc) :from (:argument 0)))
		(:arg-types ,ptype ,ptype)
		(:result-types ,ptype)
		(:variant-vars op)
		(:generator 20
		  (unless (location= x r)
		    (inst afpa-move r x ,format temp))
		  (note-this-location vop :internal-error)
		  (inst afpa-binop r y op temp)))))
  (frob afpa-single-float-op afpa-single-reg afpa-single-float :single)
  (frob afpa-double-float-op afpa-double-reg afpa-double-float :double))

(macrolet ((frob (op sinst sname dinst dname)
	     `(progn
		(define-vop (,sname afpa-single-float-op)
		  (:translate ,op)
		  (:variant ,sinst))
		(define-vop (,dname afpa-double-float-op)
		  (:translate ,op)
		  (:variant ,dinst)))))
  (frob + :adds +/single-float :addl +/double-float)
  (frob - :subs -/single-float :subl -/double-float)
  (frob * :muls */single-float :mull */double-float)
  (frob / :divs //single-float :divl //double-float))

(define-vop (afpa-unop afpa-op)
  (:args (x)))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name afpa-unop)
		(:args (x :scs (,sc)))
		(:results (r :scs (,sc)))
		(:arg-types ,ptype)
		(:result-types ,ptype)
		(:variant-vars op)
		(:generator 20
		  (inst afpa-unop r x op temp)))))
  (frob afpa-single-float-unop afpa-single-reg afpa-single-float)
  (frob afpa-double-float-unop afpa-double-reg afpa-double-float))


(macrolet ((frob (op sinst sname dinst dname)
	     `(progn
		(define-vop (,sname afpa-single-float-unop)
		  (:translate ,op)
		  (:variant ,sinst))
		(define-vop (,dname afpa-double-float-unop)
		  (:translate ,op)
		  (:variant ,dinst)))))
  (frob abs :abss abs/single-float :absl abs/double-float)
  (frob %negate :negs %negate/single-float :negl %negate/double-float))


;;;; Comparison:

(define-vop (afpa-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:guard (eq *target-float-hardware* :afpa))
  (:temporary (:sc sap-reg) temp)
  (:variant-vars condition format)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 15
    (note-this-location vop :internal-error)
    (inst afpa-compare x y
	  (ecase format
	    (:single :comts)
	    (:double :comtl))
	  temp)
    (inst afpa-get-status temp temp)
    (inst nilz temp temp (mask-field afpa-compare-result-byte -1))
    (inst c temp (dpb condition afpa-compare-result-byte 0))
    (if not-p
	(inst bnc :eq target)
	(inst bc :eq target))))


(macrolet ((frob (name sc ptype)
	     `(define-vop (,name afpa-compare)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:arg-types ,ptype ,ptype))))
  (frob afpa-single-float-compare afpa-single-reg afpa-single-float)
  (frob afpa-double-float-compare afpa-double-reg afpa-double-float))

(macrolet ((frob (translate sname dname condition)
	     `(progn
		(define-vop (,sname afpa-single-float-compare)
		  (:translate ,translate)
		  (:variant ,condition :single))
		(define-vop (,dname afpa-double-float-compare)
		  (:translate ,translate)
		  (:variant ,condition :double)))))
  (frob < afpa-</single-float afpa-</double-float afpa-compare-lss)
  (frob > afpa->/single-float afpa->/double-float afpa-compare-gtr)
  (frob eql afpa-eql/single-float afpa-eql/double-float afpa-compare-eql))


;;;; Conversion:

(macrolet ((frob (name translate
		       from-sc from-type
		       to-sc to-type
		       word-p op)
	     `(define-vop (,name)
		(:args (x :scs (,from-sc)))
		(:results (y :scs (,to-sc)))
		(:arg-types ,from-type)
		(:result-types ,to-type)
		(:temporary (:sc sap-reg) temp)
		(:policy :fast-safe)
		(:guard (eq *target-float-hardware* :afpa))
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 10
		  (note-this-location vop :internal-error)
		  ,@(if word-p
			`((inst afpa-put-float-inst y x temp
				(do-afpa-inst ,op temp :fr2 y :data x
					      :ds :fr1-immediate)))
			`((inst afpa-unop y x ,op temp)))))))
  (frob %afpa-single-float/signed %single-float
    signed-reg signed-num
    afpa-single-reg afpa-single-float
    t :cws)
  (frob %afpa-double-float/signed %double-float
    signed-reg signed-num
    afpa-double-reg afpa-double-float
    t :cwl)
  (frob %afpa-single-float/double-float %single-float
    afpa-double-reg afpa-double-float
    afpa-single-reg afpa-single-float
    nil :cls)
  (frob %afpa-double-float/single-float %double-float
    afpa-single-reg afpa-single-float
    afpa-double-reg afpa-double-float
    nil :csl))

(macrolet ((frob (translate name from-sc from-type op)
	     `(define-vop (,name)
		(:args (x :scs (,from-sc)))
		(:results (y :scs (signed-reg)))
		(:temporary (:from (:argument 0) :sc ,from-sc) fp-temp)
		(:temporary (:sc sap-reg) temp)
		(:arg-types ,from-type)
		(:result-types signed-num)
		(:translate ,translate)
		(:policy :fast-safe)
		(:guard (eq *target-float-hardware* :afpa))
		(:note "inline float round/truncate")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 10
		  (note-this-location vop :internal-error)
		  (inst afpa-unop fp-temp x ,op temp)
		  (inst afpa-get-float y fp-temp temp)))))

  (frob %unary-round %unary-round/afpa-single-float
    afpa-single-reg afpa-single-float :rsw)
  (frob %unary-round %unary-round/afpa-double-float
    afpa-double-reg afpa-double-float :rlw)
  (frob %unary-truncate %unary-truncate/afpa-single-float
    afpa-single-reg afpa-single-float :tsw)
  (frob %unary-truncate %unary-truncate/afpa-double-float
    afpa-double-reg afpa-double-float :tlw))


(define-vop (make-afpa-single-float)
  (:args (bits :scs (signed-reg)))
  (:results (res :scs (afpa-single-reg)))
  (:arg-types signed-num)
  (:temporary (:sc sap-reg) temp)
  (:guard (eq *target-float-hardware* :afpa))
  (:result-types afpa-single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:generator 5
    (inst afpa-put-float res bits temp)))

(define-vop (make-afpa-double-float)
  (:args (hi-bits :scs (signed-reg))
	 (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (afpa-double-reg)))
  (:temporary (:sc sap-reg) temp)
  (:guard (eq *target-float-hardware* :afpa))
  (:arg-types signed-num unsigned-num)
  (:result-types afpa-double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:generator 10
    (inst afpa-put-float-odd res lo-bits temp)
    (inst afpa-put-float res hi-bits temp)))

(define-vop (afpa-single-float-bits)
  (:args (float :scs (afpa-single-reg)))
  (:results (bits :scs (signed-reg)))
  (:temporary (:sc sap-reg) temp)
  (:guard (eq *target-float-hardware* :afpa))
  (:arg-types afpa-single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:generator 5
    (inst afpa-get-float bits float temp)))

(define-vop (afpa-double-float-high-bits)
  (:args (float :scs (afpa-double-reg)))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:sc sap-reg) temp)
  (:guard (eq *target-float-hardware* :afpa))
  (:arg-types afpa-double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:generator 5
    (inst afpa-get-float hi-bits float temp)))

(define-vop (afpa-double-float-low-bits)
  (:args (float :scs (afpa-double-reg)))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:sc sap-reg) temp)
  (:guard (eq *target-float-hardware* :afpa))
  (:arg-types afpa-double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:generator 5
    (inst afpa-get-float-odd lo-bits float temp)))


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
