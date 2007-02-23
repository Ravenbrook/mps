;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/insts.lisp,v 1.14 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the IBM RT instruction set.
;;;
;;; Written by William Lott and Bill Chiles.
;;;

(in-package "RT")

(use-package "ASSEM")
(use-package "EXT")


;;;; Resources:

(disassem:set-disassem-params :instruction-alignment 16)
(define-resources memory float-status mq cc)


;;;; Formats.

(define-format (j1 16 :use (cc))
  (op (byte 4 12))
  (sub-op (byte 1 11))
  (n (byte 3 8))
  (j1 (byte 8 0)))

(define-format (x 16 :clobber (cc))
  (op (byte 4 12))
  (r1 (byte 4 8) :write t)
  (r2 (byte 4 4) :read t)
  (r3 (byte 4 0) :read t))


(define-format (r 16 :clobber (cc))
  (op (byte 8 8))
  (r2 (byte 4 4) :read t :write t)
  (r3 (byte 4 0) :read t))

(define-format (r-immed 16 :clobber (cc))
  (op (byte 8 8))
  (r2 (byte 4 4) :read t :write t)
  (r3 (byte 4 0)))


(define-format (bi 32 :use (cc))
  (op (byte 8 24))
  (r2 (byte 4 20))
  (bi (byte 20 0)))

(define-format (ba 32 :attributes (assembly-call))
  (op (byte 8 24))
  (ba (byte 24 0)))

(define-format (d 32 :clobber (cc))
  (op (byte 8 24))
  (r2 (byte 4 20) :write t)
  (r3 (byte 4 16) :read t)
  (i (byte 16 0)))

(define-format (d-short 16 :clobber (cc))
  (op (byte 4 12))
  (i (byte 4 8))
  (r2 (byte 4 4) :write t)
  (r3 (byte 4 0) :read t))


;;;; Special argument types and fixups.

(define-argument-type register
  :type '(satisfies (lambda (object)
		      (and (tn-p object)
			   (or (eq (sc-name (tn-sc object)) 'null)
			       (eq (sb-name (sc-sb (tn-sc object)))
				   'registers)))))
  :function (lambda (tn)
	      (case (sc-name (tn-sc tn))
		(null null-offset)
		(t (tn-offset tn)))))

(defun address-register-p (object)
  (and (tn-p object)
       (not (zerop (tn-offset object)))
       (eq (sb-name (sc-sb (tn-sc object))) 'registers)))

(define-argument-type address-register
  :type '(satisfies address-register-p)
  :function tn-offset)


;;; LABEL-OFFSET -- Internal.
;;;
;;; This uses assem:*current-position* and the label's position to compute
;;; the bits that an instructions immediate field wants.
;;;
(defun label-offset (label)
  (ash (- (label-position label) *current-position*) -1))

(define-argument-type relative-label
  :type 'label
  :function label-offset)


(defun jump-condition-value (cond)
  (ecase cond
    (:pz #b000)
    (:lt #b001)
    (:eq #b010)
    (:gt #b011)
    (:c0 #b100)
    (:ov #b110)
    (:tb #b111)))

(define-argument-type jump-condition
  :type '(member :pz :lt :eq :gt :c0 :ov :tb)
  :function jump-condition-value)

(defun branch-condition-value (cond)
  (ecase cond
    (:pz #b1000)
    (:lt #b1001)
    (:eq #b1010)
    (:gt #b1011)
    (:c0 #b1100)
    (:ov #b1110)
    (:tb #b1111)))

(define-argument-type branch-condition
  :type '(member :pz :lt :eq :gt :c0 :ov :tb)
  :function branch-condition-value)


(define-fixup-type :ba) ;branch-absolute.
(define-fixup-type :cau)
(define-fixup-type :cal)



;;;; Loading and storing.

;;; load-character.
;;;
(define-instruction (lc)
  (d-short (op :constant 4)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :argument (unsigned-byte 4)))
  (d-short (op :constant 4)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :constant 0))
  (d (op :constant #xCE)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))

;;; store-character.
;;;
(define-instruction (stc)
  (d-short (op :constant 1)
	   (r2 :argument register :read t :write nil)
	   (r3 :argument address-register)
	   (i :argument (unsigned-byte 4)))
  (d-short (op :constant 1)
	   (r2 :argument register :read t :write nil)
	   (r3 :argument address-register)
	   (i :constant 0))
  (d (op :constant #xDE)
     (r2 :argument register :read t :write nil)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))

;;; load-half.
;;;
(define-instruction (lh)
  (d (op :constant #xDA)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16)))
  (r (op :constant #xEB)
     (r2 :argument register :read nil)
     (r3 :argument register)))

;;; load-half-algebraic.
;;;
(define-instruction (lha)
  (d (op :constant #xCA)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16)))
  (d-short (op :constant 5)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   ;; We want the instruction to take byte indexes, but we plug the
	   ;; index into the instruction as a half-word index.
	   (i :argument (member 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
	      :function (lambda (x) (ash x -1)))))

;;; store-half.
;;;
(define-instruction (sth)
  (d-short (op :constant 2)
	   (r2 :argument register :read t :write nil)
	   (r3 :argument address-register)
	   ;; We want the instruction to take byte indexes, but we plug the
	   ;; index into the instruction as a half-word index.
	   (i :argument (member 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
	      :function (lambda (x) (ash x -1))))
  (d (op :constant #xDC)
     (r2 :argument register :read t :write nil)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))

;;; load-word.
;;;
(define-instruction (l)
  (d-short (op :constant 7)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   ;; We want the instruction to take byte indexes, but we plug the
	   ;; index into the instruction as a word index.
	   (i :argument (member 0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
	      :function (lambda (x) (ash x -2))))
  (d-short (op :constant 7)
	   (r2 :argument register)
	   (r3 :argument address-register)
	   (i :constant 0))
  (d (op :constant #xCD)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))

;;; store-word.
;;;
(define-instruction (st)
  (d-short (op :constant 3)
	   (r2 :argument register :read t :write nil)
	   (r3 :argument address-register)
	   ;; We want the instruction to take byte indexes, but we plug the
	   ;; index into the instruction as a word index.
	   (i :argument (member 0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
	      :function (lambda (x) (ash x -2))))
  (d-short (op :constant 3)
	   (r2 :argument register :read t :write nil)
	   (r3 :argument address-register)
	   (i :constant 0))
  (d (op :constant #xDD)
     (r2 :argument register :read t :write nil)
     (r3 :argument address-register)
     (i :argument (signed-byte 16))))


;;;; Address computation.

;;; compute-address-lower-half.
;;;
;;; The second format can be used for load-immediate (signed-byte 16).
;;;
(define-instruction (cal)
  (d (op :constant #xC8)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (signed-byte 16)))
  (d (op :constant #xC8)
     (r2 :argument register)
     (r3 :same-as r2)
     (i :argument (signed-byte 16)))
  (d (op :constant #xC8)
     (r2 :argument register)
     (r3 :argument (integer 0 0) :read nil)
     (i :argument (signed-byte 16)))
  (d (op :constant #xC8)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument cal-fixup)))

;;; compute-address-lower-half-16bit.
;;;
;;; The second format can be used for load-immediate (unsigned-byte 16).
;;;
(define-instruction (cal16)
  (d (op :constant #xC2)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (unsigned-byte 16)))
  (d (op :constant #xC2)
     (r2 :argument register)
     (r3 :constant 0)
     (i :argument (unsigned-byte 16))))

;;; compute-address-upper-half.
;;;
(define-instruction (cau)
  (d (op :constant #xD8)
     (r2 :argument register)
     (r3 :argument address-register)
     (i :argument (unsigned-byte 16)))
  (d (op :constant #xD8)
     (r2 :argument register)
     (r3 :argument (integer 0 0) :read nil)
     (i :argument (unsigned-byte 16)))
  (d (op :constant #xD8)
     (r2 :argument register)
     (r3 :constant 0)
     (i :argument (unsigned-byte 16)))
  (d (op :constant #xD8)
     (r2 :argument register)
     (r3 :constant 0)
     (i :argument cau-fixup)))

;;; compute-address-short.
;;;
;;; Use the second flavor to copy registers.
;;;
(define-instruction (cas)
  (x (op :constant 6)
     (r1 :argument register)
     (r2 :argument register)
     (r3 :argument address-register))
  (x (op :constant 6)
     (r1 :argument register)
     (r2 :argument register)
     (r3 :constant 0)))

;;; increment.
;;;
(define-instruction (inc)
  (r-immed
   (op :constant #x91)
   (r2 :argument register)
   (r3 :argument (unsigned-byte 4))))

;;; decrement
;;;
(define-instruction (dec)
  (r-immed
   (op :constant #x93)
   (r2 :argument register)
   (r3 :argument (unsigned-byte 4))))


;;; load-immediate-short.
;;;
(define-instruction (lis)
  (r-immed
   (op :constant #xA4)
   (r2 :argument register :read nil)
   (r3 :argument (unsigned-byte 4))))



;;;; Arithmetics.

(macrolet ((define-math-inst (name &key two-regs unary short-immediate
				   immediate function (signed t)
				   (use nil use-p) 
				   (clobber nil clobber-p))
	     `(define-instruction (,name
				   ,@(when use-p `(:use ,use))
				   ,@(when clobber-p `(:clobber ,clobber)))
		,@(when two-regs
		    `((r (op :constant ,two-regs)
			 (r2 :argument register)
			 (r3 :argument register))
		      (r (op :constant ,two-regs)
			 (r2 :argument register)
			 (r3 :same-as r2))))
		,@(when unary
		    `((r (op :constant ,unary)
			 (r2 :argument register :read nil)
			 (r3 :argument register))
		      (r (op :constant ,unary)
			 (r2 :argument register :read nil)
			 (r3 :same-as r2))))
		,@(when short-immediate
		    `((r-immed
		       (op :constant ,short-immediate)
		       (r2 :argument register)
		       (r3 :argument (unsigned-byte 4)))))
		,@(when immediate
		    `((d (op :constant ,immediate)
			 (r2 :argument register)
			 (r3 :argument register)
			 (i :argument
			    (,(if signed 'signed-byte 'unsigned-byte) 16)
			    ,@(if function `(:function ,function))))
		      (d (op :constant ,immediate)
			 (r2 :argument register)
			 (r3 :same-as r2)
			 (i :argument
			    (,(if signed 'signed-byte 'unsigned-byte) 16)
			    ,@(if function `(:function ,function)))))))))

  (define-math-inst a :two-regs #xE1 :short-immediate #x90 :immediate #xC1)
  (define-math-inst ae :two-regs #xF1 :immediate #xD1 :use (cc))
  (define-math-inst abs :unary #xE0)
  (define-math-inst neg :unary #xE4) ;Arithmetic negation (two's complement).
  (define-math-inst s :two-regs #xE2 :short-immediate #x92
    :immediate #xC1 :function -)
  (define-math-inst sf :two-regs #xB2 :immediate #xD2)
  (define-math-inst se :two-regs #xF2 :use (cc))
  (define-math-inst d :two-regs #xB6 :use (cc mq) :clobber (cc mq))
  (define-math-inst m :two-regs #xE6 :use (cc mq) :clobber (cc mq))
  
  (define-math-inst exts :unary #xB1)
  
  (define-math-inst clrbl :short-immediate #x99)
  (define-math-inst clrbu :short-immediate #x98)
  (define-math-inst setbl :short-immediate #x9B)
  (define-math-inst setbu :short-immediate #x9A)
  
  (define-math-inst not :unary #xF4) ;Logical not.
  (define-math-inst n :two-regs #xE5)
  (define-math-inst nilz :immediate #xC5 :signed nil)
  (define-math-inst nilo :immediate #xC6 :signed nil)
  (define-math-inst niuz :immediate #xD5 :signed nil)
  (define-math-inst niuo :immediate #xD6 :signed nil)
  (define-math-inst o :two-regs #xE3)
  (define-math-inst oil :immediate #xC4 :signed nil)
  (define-math-inst oiu :immediate #xC3 :signed nil)
  (define-math-inst x :two-regs #xE7)
  (define-math-inst xil :immediate #xC7 :signed nil)
  (define-math-inst xiu :immediate #xD7 :signed nil)

  (define-math-inst clz :two-regs #xF5)

) ;macrolet


;;; compare.
;;; compare-immediate-short.
;;; compare-immediate.
;;;
(define-instruction (c)
  (r (op :constant #xB4)
     (r2 :argument register :write nil)
     (r3 :argument register))
  (r-immed
   (op :constant #x94)
   (r2 :argument register :write nil)
   (r3 :argument (unsigned-byte 4)))
  (d (op :constant #xD4)
     (r2 :constant 0)
     (r3 :argument register)
     (i :argument (signed-byte 16))))


;;; compare-logical.
;;; compare-logical-immediate.
;;;
(define-instruction (cl)
  (r (op :constant #xB3)
     (r2 :argument register :write nil)
     (r3 :argument register))
  (d (op :constant #xD3)
     (r2 :constant 0)
     (r3 :argument register)
     (i :argument (signed-byte 16))))



;;;; Shifting.

(define-instruction (sr)
  (r (op :constant #xB8)
     (r2 :argument register)
     (r3 :argument register))
  (r-immed
   (op :constant #xA8)
   (r2 :argument register :write nil)
   (r3 :argument (unsigned-byte 4)))
  (r-immed
   (op :constant #xA9)
   (r2 :argument register :write nil)
   (r3 :argument (integer 16 31)
       :function (lambda (x) (- x 16)))))

(define-instruction (sl)
  (r (op :constant #xBA)
     (r2 :argument register)
     (r3 :argument register))
  (r-immed
   (op :constant #xAA)
   (r2 :argument register)
   (r3 :argument (unsigned-byte 4)))
  (r-immed
   (op :constant #xAB)
   (r2 :argument register)
   (r3 :argument (integer 16 31)
       :function (lambda (x) (- x 16)))))

(define-instruction (sar)
  (r (op :constant #xB0)
     (r2 :argument register)
     (r3 :argument register))
  (r-immed
   (op :constant #xA0)
   (r2 :argument register)
   (r3 :argument (unsigned-byte 4)))
  (r-immed
   (op :constant #xA1)
   (r2 :argument register)
   (r3 :argument (integer 16 31)
       :function (lambda (x) (- x 16)))))


;;;; Branch instructions.

;;; There are some pseudo-instructions defined after this page that use these
;;; definitions.
;;;

(define-instruction (jb)
  (j1 (op :constant 0)
      (sub-op :constant 1)
      (n :argument jump-condition)
      (j1 :argument relative-label)))

(define-instruction (jnb)
  (j1 (op :constant 0)
      (sub-op :constant 0)
      (n :argument jump-condition)
      (j1 :argument relative-label)))

(macrolet ((define-branch-inst (name immediate-op register-op)
	     `(define-instruction (,name)
		(bi (op :constant ,immediate-op)
		    (r2 :argument branch-condition)
		    (bi :argument relative-label))
		(r (op :constant ,register-op)
		   (r2 :argument branch-condition :read nil :write nil)
		   (r3 :argument register)))))

  ;; branch-on-condition-bit-immediate.
  ;; branch-on-condition-bit-register.
  ;;
  (define-branch-inst bb #x8E #xEE)
  
  ;; branch-on-condition-bit-immediate-with-execute.
  ;; branch-on-condition-bit-register-with-execute.
  ;;
  (define-branch-inst bbx #x8F #xEF)

  ;; branch-on-not-condition-bit-immediate.
  ;; branch-on-not-condition-bit-register.
  ;;
  (define-branch-inst bnb #x88 #xE8)

  ;; branch-on-not-condition-bit-immediate-with-execute.
  ;; branch-on-not-condition-bit-register-with-execute.
  ;;
  (define-branch-inst bnbx #x89 #xE9)

) ;MACROLET

(define-instruction (bala)
  (ba (op :constant #x8A)
      (ba :argument ba-fixup)))

(define-instruction (balax)
  (ba (op :constant #x8B)
      (ba :argument ba-fixup)))



;;;; Pseudo-instructions

;;; move.
;;;
;;; This body is the second format of compute-address-short.
;;;
(define-instruction (move)
  (x (op :constant 6)
     (r1 :argument register)
     (r2 :argument register)
     (r3 :constant 0)))

;;;
;;; A couple load-immediate pseudo-instructions.
;;;

;;; load-immediate.
;;;
;;; This might affect the condition codes, but it allows for loading 32-bit
;;; quantities into R0.
;;;
(define-pseudo-instruction li 64 (reg value)
  (etypecase value
    ((unsigned-byte 4)
     (inst lis reg value))
    ((signed-byte 16)
     (inst cal reg 0 value))
    ((unsigned-byte 16)
     (inst cal16 reg value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst cau reg (ldb (byte 16 16) value))
     (let ((low (ldb (byte 16 0) value)))
       (unless (zerop low)
	 (inst oil reg low))))))

;;; compute-address-immediate.
;;;
;;; This basically exists to load 32-bit constants into address-registers, and
;;; it does not affect condition codes.  Since fixups are always addresses, we
;;; have the fixup branch here instead of in load-immediate.  We may use the
;;; other branches since it already exists.
;;;
(define-pseudo-instruction cai 64 (reg value)
  (etypecase value
    ((unsigned-byte 4)
     (inst lis reg value))
    ((signed-byte 16)
     (inst cal reg 0 value))
    ((unsigned-byte 16)
     (inst cal16 reg value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst cau reg (ldb (byte 16 16) value))
     (let ((low (ldb (byte 16 0) value)))
       (unless (zerop low)
	 (inst cal16 reg reg low))))
    (fixup
     (inst cau reg value)
     (inst cal reg reg value))))


;;; branch-unconditional.
;;;
(define-pseudo-instruction b 32 (target)
  (if (and (assem::label-p target)
	   (<= -128 (label-offset target) 127))
      (inst jnb :pz target)
      (inst bnb :pz target)))

;;; branch-unconditional-with-execute.
;;;
(define-pseudo-instruction bx 32 (target)
  (inst bnbx :pz target))

;;; branch-condition.
;;;
(define-pseudo-instruction bc 32 (condition target)
  (if (and (assem::label-p target)
	   (<= -128 (label-offset target) 127))
      (inst jb condition target)
      (inst bb condition target)))

;;; branch-not-condition.
;;;
(define-pseudo-instruction bnc 32 (condition target)
  (if (and (assem::label-p target)
	   (<= -128 (label-offset target) 127))
      (inst jnb condition target)
      (inst bnb condition target)))

;;; branch-condition-with-execute.
;;; branch-not-condition-with-execute.
;;;
;;; We define these, so VOP readers see a consistent naming scheme in branch
;;; instructions.
;;;
(define-pseudo-instruction bcx 32 (condition target)
  (inst bbx condition target))
;;;
(define-pseudo-instruction bncx 32 (condition target)
  (inst bnbx condition target))

;;; no-op.
;;;
;;; This is compute-address-short, adding zero to R0 putting the result in R0.
;;;
(define-instruction (no-op)
  (x (op :constant 6)
     (r1 :constant 0)
     (r2 :constant 0)
     (r3 :constant 0)))

(define-format (word-format 32)
  (data (byte 32 0)))
(define-instruction (word)
  (word-format (data :argument (or (unsigned-byte 32) (signed-byte 32)))))

(define-format (short-format 16)
  (data (byte 16 0)))
(define-instruction (short)
  (short-format (data :argument (or (unsigned-byte 16) (signed-byte 16)))))

(define-format (byte-format 8)
  (data (byte 8 0)))
(define-instruction (byte)
  (byte-format (data :argument (or (unsigned-byte 8) (signed-byte 8)))))



;;;; Breaking.

(define-instruction (tge)
  (r (op :constant #xBD)
     (r2 :argument register)
     (r3 :argument register)))

(define-instruction (tlt)
  (r (op :constant #xBE)
     (r2 :argument register)
     (r3 :argument register)))

;;; break.
;;;
;;; This is trap-on-condition-immediate.  We use the immediate field to
;;; stick in a constant value indicating why we're breaking.
;;;
(define-instruction (break)
  (d (op :constant #xCC)
     (r2 :constant #b0111)
     (r3 :constant 0)
     (i :argument (signed-byte 16))))



;;;; System control.

;;; move-to-multiplier-quotient-system-control-register
;;;
(define-instruction (mtmqscr)
  (r (op :constant #xB5)
     (r2 :constant 10)
     (r3 :argument register)))

;;; move-from-multiplier-quotient-system-control-register
;;;
(define-instruction (mfmqscr)
  (r (op :constant #x96)
     (r2 :constant 10)
     (r3 :argument register :read nil :write t)))



;;;; Function and LRA Headers emitters and calculation stuff.

(defun header-data (ignore)
  (declare (ignore ignore))
  (ash (+ *current-position* (component-header-length)) (- vm:word-shift)))

(define-format (header-object 32)
  (type (byte 8 0))
  (data (byte 24 8) :default 0 :function header-data))

(define-instruction (function-header-word)
  (header-object (type :constant vm:function-header-type)))

(define-instruction (lra-header-word)
  (header-object (type :constant vm:return-pc-header-type)))


;;; DEFINE-COMPUTE-INSTRUCTION -- Internal.
;;;
;;; This defines a pseudo-instruction, name, which requires the other specific
;;; instructions.  When the pseudo-instruction expands, it looks at the current
;;; value of the calculation to choose between two instruction sequences.
;;; Later, when the assembler emits the instructions, the :function's run to
;;; really compute the calculation's value.  This is guaranteed to be lesser in
;;; magnitude than the original test in the pseudo-instruction since all these
;;; calculation are relative to the component start, so we can only remove
;;; instructions in that range, not add them.
;;;
(defmacro define-compute-instruction (name calculation)
  (let ((cal (symbolicate name "-CAL"))
	(cau (symbolicate name "-CAU")))
    `(progn
       ;; This is a special compute-address-lower that takes a label and
       ;; asserts the type of the immediate value.
       (defun ,name (label)
	 (let* ((whole ,calculation)
		(low (logand whole #xffff))
		(high (ash whole -16)))
	   (values (if (logbitp 15 low)
		       (1+ high)
		       high)
		   low)))
       (define-instruction (,cal)
	 (d (op :constant #xC8)
	    (r2 :argument register)
	    (r3 :argument address-register)
	    (i :argument label
	       :function (lambda (label)
			   (multiple-value-bind (high low) (,name label)
			     (declare (ignore high))
			     low)))))
       (define-instruction (,cau)
	 (d (op :constant #xD8)
	    (r2 :argument register)
	    (r3 :argument address-register)
	    (i :argument label
	       :function (lambda (label)
			   (multiple-value-bind (high low) (,name label)
			     (declare (ignore low))
			     high)))))
       (define-pseudo-instruction ,name 64 (dst src label)
	 (multiple-value-bind (high low) (,name label)
	   (declare (ignore low))
	   (cond ((zerop high)
		  (inst ,cal dst src label))
		 (t
		  (inst ,cal dst src label)
		  (inst ,cau dst dst label))))))))


;; code = fn - header - label-offset + other-pointer-tag
(define-compute-instruction compute-code-from-fn
			    (- vm:other-pointer-type
			       (label-position label)
			       (component-header-length)))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
(define-compute-instruction compute-code-from-lra
			    (- (+ (label-position label)
				  (component-header-length))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-compute-instruction compute-lra-from-code
			    (+ (label-position label)
			       (component-header-length)))

;;;; 68881 instruction set details:

;;; The low 7 bits of the 68881 instructions.
;;; 
(defconstant mc68881-opcodes
  '((:move . #x00)
    (:int . #x01)
    (:sinh . #x02)
    (:intrz . #x03)
    (:sqrt . #x04)
    (:lognp1 . #x06)
    (:etoxm1 . #x08)
    (:tanh . #x09)
    (:atan . #x0A)
    (:asin . #x0C)
    (:atanh . #x0D)
    (:sin . #x0E)
    (:tan . #x0F)
    (:etox . #x10)
    (:twotox . #x11)
    (:tentox . #x12)
    (:logn . #x14)
    (:log10 . #x15)
    (:log2 . #x16)
    (:abs . #x18)
    (:cosh . #x19)
    (:neg . #x1A)
    (:acos . #x1C)
    (:cos . #x1D)
    (:getexp . #x1E)
    (:getman . #x1F)
    (:div . #x20)
    (:mod . #x21)
    (:add . #x22)
    (:mul . #x23)
    (:sgldiv . #x24)
    (:rem . #x25)
    (:scale . #x26)
    (:sglmul . #x27)
    (:sub . #x28)
    (:sincos . #x30)
    (:cmp . #x38)
    (:tst . #x3A)))


;;; Names of interesting 68881 control registers.
;;;
(defconstant mc68881-control-regs
  '((:fpsr . 2)
    (:fpcr . 4)
    (:fpiar . 1)))


;;; The encoding of operand format in memory (bits 10..12 in the 68881
;;; instruction.)
;;;
(defconstant mc68881-operand-types
  '((:integer . 0)	      ; 32 bit integer.
    (:single . 1)	      ; 32 bit float.
    (:double . 5)))	      ; 64 bit float.


;;; Bits 13..15 in the 68881 instruction.  Controls where operands are found.
;;;
(defconstant mc68881-instruction-classes
  '((:freg-to-freg . 0)
    (:mem-to-freg . 2)
    (:freg-to-mem . 3)
    (:mem-to-scr . 4)
    (:scr-to-mem . 5)))


;;; Length in words of memory data to transfer.  This is part of the RT
;;; hardware assist protocol, and is placed in the low two bits of the address
;;; to the store instruction.
;;;
(defconstant mc68881-operand-lengths
  '((nil . 0) ; No transfer...
    (:integer . 1)
    (:single . 1)
    (:double . 2)))

;;; RT hardware assist protocol for whether to read or write memory, here
;;; represented as a function of the mc68881 instruction class.
;;; 
(defconstant mc68881-transfer-control-field-alist
  '((:freg-to-freg . 0)
    (:mem-to-freg . 0)
    (:freg-to-mem . #x3c0000)
    (:mem-to-scr . 0)
    (:scr-to-mem . #x3c0000)))


(defun mc68881-fp-reg-p (object)
  (and (tn-p object)
       (eq (sb-name (sc-sb (tn-sc object)))
	   'mc68881-float-registers)))

(define-argument-type mc68881-fp-reg
  :type '(satisfies mc68881-fp-reg-p)
  :function tn-offset)

;;; This is really a ST (store word) instruction, but we have magic extra
;;; arguments to represent the reading and writing of the FP registers.  R3
;;; will already have been set up with the high bits of the FP instruction by a
;;; preceding CAL.
;;;
(define-format (mc68881-inst 32)
  (fpn (byte 0 0) :read t :write t)
  (fpm (byte 0 0) :read t)
  (op (byte 8 24) :default #xDD)
  (r2 (byte 4 20) :read t)
  (r3 (byte 4 16) :read t)
  (i (byte 16 0)))


;;; DO-68881-INST  --  Internal
;;;
;;;    Utility used to emit a 68881 operation.  We emit the CAU that sets up
;;; the high bits of the operation in Temp, and we return the low bits that
;;; should be passed as the I field of the actual FP operation instruction.
;;;
(defun do-68881-inst (op temp &key fpn fpm (class :freg-to-freg)
			 data optype creg)
  (assert (if fpm
	      (and (mc68881-fp-reg-p fpm)
		   (not (or optype data))
		   (eql class :freg-to-freg))
	      (and (tn-p data)
		   (eq (sb-name (sc-sb (tn-sc data))) 'registers)
 		   (or optype creg)
		   (not (eql class :freg-to-freg)))))
  (assert (if fpn
	      (mc68881-fp-reg-p fpn)
	      (and creg (member class '(:mem-to-scr :scr-to-mem)))))
		   
  (let* ((opcode
	  (logior #xFC000000
		  (or (cdr (assoc class mc68881-transfer-control-field-alist))
		      (error "Unknown instruction class ~S." class))
		  (ash (cdr (assoc class mc68881-instruction-classes))
		       15)
		  (ash (ecase class
			 (:freg-to-freg (tn-offset fpm))
			 ((:mem-to-freg :freg-to-mem)
			  (or (cdr (assoc optype mc68881-operand-types))
			      (error "Unknown operation type ~S." optype)))
			 ((:mem-to-scr :scr-to-mem)
			  (or (cdr (assoc creg mc68881-control-regs))
			      (error "Unknown control register ~S." creg))))
		       12)
		  (ash (if fpn
			   (tn-offset fpn)
			   0)
		       9)
		  (ash (or (cdr (assoc op mc68881-opcodes))
			   (error "Unknown opcode ~S." op))
		       2)
		  (if creg
		      1
		      (or (cdr (assoc optype mc68881-operand-lengths))
			  (error "Unknown operation type ~S." optype)))))
	 (low (logand opcode #xFFFF))
	 (high (+ (logand (ash opcode -16) #xFFFF)
		  (if (eql (logand low #x8000) 0) 0 1))))
    (inst cau temp 0 high)
    low))


(define-instruction (mc68881-binop-inst)
  (mc68881-inst
   (fpn :argument mc68881-fp-reg)
   (fpm :argument mc68881-fp-reg)
   (r2 :constant null-offset)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

;;; This pseudo-instruction emits a floating-point binop on the 68881.  FPN is
;;; the destination float register (and second arg) FPM is the source float
;;; register.  Op is the 68881 opcode.  Temp is a sap-reg (i.e. non-zero,
;;; non-descriptor) register that we form the FP instruction in.
;;;
(define-pseudo-instruction mc68881-binop 64 (fpn fpm op temp)
  (inst mc68881-binop-inst fpn fpm temp
	(do-68881-inst op temp :fpm fpm :fpn fpn)))


;;; Unop is like binop, but we don't read FPN before we write it.
;;;
(define-instruction (mc68881-unop-inst)
  (mc68881-inst
   (fpn :argument mc68881-fp-reg :read nil)
   (fpm :argument mc68881-fp-reg)
   (r2 :constant null-offset)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction mc68881-unop 64 (fpn fpm op temp)
  (inst mc68881-unop-inst fpn fpm temp
	(do-68881-inst op temp :fpm fpm :fpn fpn)))

;;; Compare is like binop, but we don't write FPN.
;;;
(define-instruction (mc68881-compare-inst)
  (mc68881-inst
   (fpn :argument mc68881-fp-reg :write nil)
   (fpm :argument mc68881-fp-reg)
   (r2 :constant null-offset)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction mc68881-compare 64 (fpn fpm op temp)
  (inst mc68881-compare-inst fpn fpm temp
	(do-68881-inst op temp :fpm fpm :fpn fpn)))

;;; Move FPM to FPN.
;;;
(define-pseudo-instruction mc68881-move 64 (fpn fpm temp)
  (inst mc68881-unop fpn fpm :move temp))



(define-format (s 16)
  (op (byte 12 4))
  (n (byte 4 0)))
			   
;;; This is a "setcb 8", which is used to wait for a result from the FPA to
;;; appear in memory (or something...)
;;;
(define-instruction (mc68881-wait)
  (s
   (op :constant #x97F)
   (n :constant 8)))


(define-instruction (mc68881-load-inst :use (memory))
  (mc68881-inst
   (fpn :argument mc68881-fp-reg :read nil)
   (fpm :constant 0)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction mc68881-load 80 (fpn data optype temp)
  (inst mc68881-load-inst fpn data temp
	(do-68881-inst :move temp :fpn fpn :data data :optype optype
		       :class :mem-to-freg))
  (inst mc68881-wait))

(define-instruction (mc68881-store-inst :clobber (memory))
  (mc68881-inst
   (fpn :argument mc68881-fp-reg :write nil)
   (fpm :constant 0)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction mc68881-store 80 (fpn data optype temp)
  (inst mc68881-store-inst fpn data temp
	(do-68881-inst :move temp :fpn fpn :data data :optype optype
		       :class :freg-to-mem))
  (inst mc68881-wait))


(define-instruction (mc68881-load-status-inst :use (memory)
					      :clobber (float-status))
  (mc68881-inst
   (fpn :constant 0)
   (fpm :constant 0)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction mc68881-load-status 80 (creg data temp)
  (inst mc68881-load-status-inst data temp
	(do-68881-inst :move temp :creg creg :data data
		       :class :mem-to-scr))
  (inst mc68881-wait))

(define-instruction (mc68881-store-status-inst :clobber (memory)
					       :use (float-status))
  (mc68881-inst
   (fpn :constant 0)
   (fpm :constant 0)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction mc68881-store-status 80 (creg data temp)
  (inst mc68881-store-status-inst data temp
	(do-68881-inst :move temp :creg creg :data data
		       :class :scr-to-mem))
  (inst mc68881-wait))


;;; AFPA instruction set details:

;;; Support for the AFPA on IBM RT PC (APC and EAPC models).

(defconstant afpa-opcodes
  '((:absl . #x74)
    (:abss . #x75)
    (:addl . #x40)
    (:adds . #x41)
    (:coml . #x48)
    (:coms . #x49)
    (:comtl . #x4A)
    (:comts . #x4B)
    (:csl  . #x1B)
    (:cls  . #x16)
    (:cwl  . #x03)
    (:cws  . #x07)
    (:copl . #x44)
    (:cops . #x45)
    (:divl . #x60)
    (:divs . #x61)
    (:flw  . #x3B)
    (:fsw  . #x3F)
    (:mull . #x70)
    (:muls . #x71)
    (:negl . #x54)
    (:negs . #x55)
    (:noop . #x9F)
    (:rdfr . #xBC) ; type = ld
    (:rlw  . #x23)
    (:rsw  . #x27)
    (:subl . #x50)
    (:subs . #x51)
    (:tlw  . #x2B)
    (:tsw  . #x2F)
    (:wtfr . #x94)))
  
(defconstant afpa-special-opcodes
  '((:wtstr . #.(ash #x10FEE 2)) ; DS = #b01, OP1, OP2 = #xE.
    (:rdstr . #.(ash #x137EE 2)) ; DS = #b01, OP1, OP2 = #xE, type = ld
    (:rddma . #.(ash #xF9F00 2)))) ; DS = #b11, OP2 = #x30

(defconstant afpa-ds-codes
  '((:register . #b00)
    (:fr1-immediate . #b10)
    (:fr2-immediate . #b01)))

(defconstant afpa-ts-codes
  '((:pio . #b00)
    (:word . #b01)
    (:single . #b01)
    (:double . #b10)
    (:multiple . #b11)))

#|
(defconstant afpa-atanl #x0D4)
(defconstant afpa-cosl #x0C2)
(defconstant afpa-expl #x0D8)
(defconstant afpa-log10l #x0DE)
(defconstant afpa-logl #x0DC)
(defconstant afpa-sinl #x0C0)
(defconstant afpa-sqrl #x064)
(defconstant afpa-sqrs #x065)
(defconstant afpa-tanl #x0C4)
|#

(defun afpa-fp-reg-p (object)
  (and (tn-p object)
       (eq (sb-name (sc-sb (tn-sc object)))
	   'afpa-float-registers)))

(define-argument-type afpa-fp-reg
  :type '(satisfies afpa-fp-reg-p)
  :function tn-offset)

;;; These are really L and ST (load and store word) instructions, but we have
;;; magic extra arguments to represent the reading and writing of the FP
;;; registers.  R3 will already have been set up with the high bits of the FP
;;; instruction by a preceding CAL.
;;;
(define-format (afpa-l-inst 32)
  (fr2 (byte 0 0) :default 0)
  (fr1 (byte 0 0) :read t)
  (op (byte 8 24) :default #xCD)
  (r2 (byte 4 20) :write t)
  (r3 (byte 4 16) :read t)
  (i (byte 16 0)))
;;;
(define-format (afpa-st-inst 32)
  (fr2 (byte 0 0) :read t :write t)
  (fr1 (byte 0 0) :read t)
  (op (byte 8 24) :default #xDD)
  (r2 (byte 4 20) :read t)
  (r3 (byte 4 16) :read t)
  (i (byte 16 0)))


;;; DO-AFPA-INST  --  Internal
;;;
;;;    Utility used to emit a afpa operation.  We emit the CAU that sets up
;;; the high bits of the operation in Temp, and we return the low bits that
;;; should be passed as the I field of the actual FP operation instruction.
;;;
;;; Note: FR2 is the modified register (if any), and the *first* operand to
;;; binops (I didn't make this up.)
;;;
(defun do-afpa-inst (op temp &key fr1 fr2 (ds :register) (ts :pio)
			data odd)
  (when fr1
    (assert (afpa-fp-reg-p fr1)))
  (when fr2
    (assert (afpa-fp-reg-p fr2)))
  (when data
    (assert (and (tn-p data)
		 (eq (sb-name (sc-sb (tn-sc data))) 'registers)
		 (not (and fr1 fr2)))))
  (when odd (assert data))
  (let* ((inc (if odd 1 0))
	 (fr1-offset (if fr1 (+ (tn-offset fr1) inc) 0))
	 (fr2-offset (if fr2 (+ (tn-offset fr2) inc) 0))
	 (opcode
	  (logior (ash (if (eq ts :pio) #xFF #xFE) 24)
		  (ash (ldb (byte 2 4) fr1-offset) 22)
		  (ash (ldb (byte 2 4) fr2-offset) 20)
		  (ash (or (cdr (assoc ds afpa-ds-codes))
			   (error "Unknown DS code: ~S." ds))
		       18)
		  (let ((res (cdr (assoc op afpa-opcodes))))
		    (if res
			(ash res 10)
			(or (cdr (assoc op afpa-special-opcodes))
			    (error "Unknown opcode: ~S." op))))
		  (ash (ldb (byte 4 0) fr1-offset) 6)
		  (ash (ldb (byte 4 0) fr2-offset) 2)
		  (or (cdr (assoc ts afpa-ts-codes))
		      (error "Unknown TS code: ~S." ts))))
	 (low (logand opcode #xFFFF))
	 (high (+ (logand (ash opcode -16) #xFFFF)
		  (if (eql (logand low #x8000) 0) 0 1))))
    (inst cau temp 0 high)
    low))


;;; The AFPA-BINOP pseudo-instruction emits a floating-point binop on the afpa.
;;; FR2 is the destination float register (and first arg).  FR1 is the source
;;; float register.  Op is the afpa opcode.  Temp is a sap-reg (i.e. non-zero,
;;; non-descriptor) register that we form the FP instruction in.
;;;
(define-instruction (afpa-binop-inst)
  (afpa-st-inst
   (fr2 :argument afpa-fp-reg)
   (fr1 :argument afpa-fp-reg)
   (r2 :constant null-offset)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))
;;;
(define-pseudo-instruction afpa-binop 64 (fr2 fr1 op temp)
  (inst afpa-binop-inst fr2 fr1 temp
	(do-afpa-inst op temp :fr1 fr1 :fr2 fr2)))


;;; Unop is like binop, but we don't read FR2 before we write it.
;;;
(define-instruction (afpa-unop-inst)
  (afpa-st-inst
   (fr2 :argument afpa-fp-reg :read nil)
   (fr1 :argument afpa-fp-reg)
   (r2 :constant null-offset)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-unop 64 (fr2 fr1 op temp)
  (inst afpa-unop-inst fr2 fr1 temp
	(do-afpa-inst op temp :fr1 fr1 :fr2 fr2)))

;;; Sugar up the move a bit...
(define-pseudo-instruction afpa-move 64 (fr2 fr1 format temp)
  (inst afpa-unop fr2 fr1
	(ecase format
	  (:single :cops)
	  (:double :copl))
	temp))

;;; Compare is like binop, but we don't write FR2.
;;;
(define-instruction (afpa-compare-inst)
  (afpa-st-inst
   (fr2 :argument afpa-fp-reg :write nil)
   (fr1 :argument afpa-fp-reg)
   (r2 :constant null-offset)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-compare 64 (fr2 fr1 op temp)
  (inst afpa-compare-inst fr2 fr1 temp
	(do-afpa-inst op temp :fr1 fr1 :fr2 fr2)))


;;; Noop is used to wait for DMA operations (load and store) to complete.
;;;
(define-instruction (afpa-noop-inst :pinned t)
  (afpa-st-inst
   (fr1 :constant 0)
   (fr2 :constant 0)
   (r2 :constant null-offset)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-noop 64 (temp)
  (inst afpa-noop-inst temp
	(do-afpa-inst :noop temp)))


;;; Load = WTFR (write float reg) + DMA.
;;;
(define-instruction (afpa-load-inst :use (memory))
  (afpa-st-inst
   (fr2 :argument afpa-fp-reg :read nil)
   (fr1 :constant 0)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-load 64 (fr2 data ts temp)
  (inst afpa-load-inst fr2 data temp
	(do-afpa-inst :wtfr temp :fr2 fr2 :data data :ts ts)))


;;; Store = RDDMA
;;;
(define-instruction (afpa-store-inst :clobber (memory))
  (afpa-st-inst
   (fr2 :constant 0)
   (fr1 :argument afpa-fp-reg)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-store 64 (fr1 data ts temp)
  (inst afpa-store-inst fr1 data temp
	(do-afpa-inst :rddma temp :fr1 fr1 :data data :ts ts)))


;;; Get float = RDFR
;;;
(define-instruction (afpa-get-float-inst :clobber (float-status))
  (afpa-l-inst
   (fr2 :constant 0)
   (fr1 :argument afpa-fp-reg)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-get-float 64 (data fr1 temp)
  (inst afpa-get-float-inst fr1 data temp
	(do-afpa-inst :rdfr temp :fr1 fr1 :data data)))

(define-pseudo-instruction afpa-get-float-odd 64 (data fr1 temp)
  (inst afpa-get-float-inst fr1 data temp
	(do-afpa-inst :rdfr temp :fr1 fr1 :data data :odd t)))


;;; Put float = WTFR
;;;
(define-instruction (afpa-put-float-inst)
  (afpa-st-inst
   (fr2 :argument afpa-fp-reg)
   (fr1 :constant 0)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-put-float 64 (fr2 data temp)
  (inst afpa-put-float-inst fr2 data temp
	(do-afpa-inst :wtfr temp :fr2 fr2 :data data)))

(define-pseudo-instruction afpa-put-float-odd 64 (fr2 data temp)
  (inst afpa-put-float-inst fr2 data temp
	(do-afpa-inst :wtfr temp :fr2 fr2 :data data :odd t)))


;;; Get status = RDSTR
;;;
(define-instruction (afpa-get-status-inst :clobber (float-status))
  (afpa-l-inst
   (fr2 :constant 0)
   (fr1 :constant 0)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-get-status 64 (data temp)
  (inst afpa-get-status-inst data temp
	(do-afpa-inst :rdstr temp :data data)))


;;; Put status = WTSTR
;;;
(define-instruction (afpa-put-status-inst :use (float-status))
  (afpa-st-inst
   (fr2 :constant 0)
   (fr1 :constant 0)
   (r2 :argument register)
   (r3 :argument address-register)
   (i :argument (unsigned-byte 16))))

(define-pseudo-instruction afpa-put-status 64 (data temp)
  (inst afpa-put-status-inst data temp
	(do-afpa-inst :wtstr temp :data data)))
