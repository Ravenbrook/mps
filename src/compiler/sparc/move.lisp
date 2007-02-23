;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/move.lisp,v 1.15 2004/05/13 14:37:06 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/sparc/move.lisp,v 1.15 2004/05/13 14:37:06 rtoy Exp $
;;;
;;;    This file contains the SPARC VM definition of operand loading/saving and
;;; the Move VOP.
;;;
;;; Written by Rob MacLachlan.
;;; SPARC conversion by William Lott.
;;;
(in-package "SPARC")


(define-move-function (load-immediate 1) (vop x y)
  ((null immediate zero)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst li y (fixnumize val)))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val))
      (character
       (inst li y (logior (ash (char-code val) type-bits)
			  base-char-type))))))

#-(and sparc-v9 sparc-v8plus)
(define-move-function (load-number 1) (vop x y)
  ((immediate zero)
   (signed-reg unsigned-reg))
  (inst li y (tn-value x)))

#+(and sparc-v9 sparc-v8plus)
(define-move-function (load-number 1) (vop x y)
  ((immediate zero)
   (signed-reg unsigned-reg signed64-reg unsigned64-reg))
  (inst li64 y (tn-value x)))

(define-move-function (load-base-char 1) (vop x y)
  ((immediate) (base-char-reg))
  (inst li y (char-code (tn-value x))))

(define-move-function (load-system-area-pointer 1) (vop x y)
  ((immediate) (sap-reg))
  (inst li y (sap-int (tn-value x))))

(define-move-function (load-constant 5) (vop x y)
  ((constant) (descriptor-reg))
  (loadw y code-tn (tn-offset x) other-pointer-type gtemp-tn))

(define-move-function (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-function (load-number-stack 5) (vop x y)
  ((base-char-stack) (base-char-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (let ((nfp (current-nfp-tn vop)))
    (loadw y nfp (tn-offset x))))

(define-move-function (store-stack 5) (vop x y)
  ((any-reg descriptor-reg) (control-stack))
  (store-stack-tn y x))

(define-move-function (store-number-stack 5) (vop x y)
  ((base-char-reg) (base-char-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (let ((nfp (current-nfp-tn vop)))
    (storew x nfp (tn-offset y))))



;;;; The Move VOP:
;;;
(define-vop (move)
  (:args (x :target y
	    :scs (any-reg descriptor-reg zero null)
	    :load-if (not (location= x y))))
  (:results (y :scs (any-reg descriptor-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))

(define-move-vop move :move
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))

;;; Make Move the check VOP for T so that type check generation doesn't think
;;; it is a hairy type.  This also allows checking of a few of the values in a
;;; continuation to fall out.
;;;
(primitive-type-vop move (:check) t)

;;;    The Move-Argument VOP is used for moving descriptor values into another
;;; frame for argument or known value passing.
;;;
(define-vop (move-argument)
  (:args (x :target y
	    :scs (any-reg descriptor-reg zero null))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y any-reg descriptor-reg))))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (y))
  (:generator 0
    (sc-case y
      ((any-reg descriptor-reg)
       (move y x))
      (control-stack
       (storew x fp (tn-offset y) 0 temp)))))
;;;
(define-move-vop move-argument :move-argument
  (any-reg descriptor-reg)
  (any-reg descriptor-reg))



;;;; ILLEGAL-MOVE

;;; This VOP exists just to begin the lifetime of a TN that couldn't be written
;;; legally due to a type error.  An error is signalled before this VOP is
;;; so we don't need to do anything (not that there would be anything sensible
;;; to do anyway.)
;;;
(define-vop (illegal-move)
  (:args (x) (type))
  (:results (y))
  (:ignore y)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 666
    (error-call vop object-not-type-error x type)))



;;;; Moves and coercions:

;;; These MOVE-TO-WORD VOPs move a tagged integer to a raw full-word
;;; representation.  Similarly, the MOVE-FROM-WORD VOPs converts a raw integer
;;; to a tagged bignum or fixnum.

;;; Arg is a fixnum, so just shift it.  We need a type restriction because some
;;; possible arg SCs (control-stack) overlap with possible bignum arg SCs.
;;;
(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 1
    (inst sran y x fixnum-tag-bits)))

;;;
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))

;;; Arg is a non-immediate constant, load it.
(define-vop (move-to-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "constant load")
  (:generator 1
    (inst li y (tn-value x))))

;;;
(define-move-vop move-to-word-c :move
  (constant) (signed-reg unsigned-reg))

;;; Arg is a fixnum or bignum, figure out which and load if necessary.
#-(and sparc-v9 sparc-v8plus)
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (let ((done (gen-label)))
      (inst andcc temp x fixnum-tag-mask)
      (inst b :eq done)
      (inst sran y x fixnum-tag-bits)
      
      (loadw y x bignum-digits-offset other-pointer-type)
      
      (emit-label done))))

;; Same as above, but the number is sign-extended to a full 64-bit
;; length.  Not really needed, I suppose.
#+(and sparc-v9 sparc-v8plus)
(define-vop (move-to-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (let ((done (gen-label)))
      (inst andcc temp x fixnum-tag-mask)
      (inst signx temp x)		; sign-extend x to temp
      (inst b :eq done)
      (inst srax y temp fixnum-tag-bits)
      
      (loadsw y x bignum-digits-offset other-pointer-type)
      
      (emit-label done))))

;;;
(define-move-vop move-to-word/integer :move
  (descriptor-reg) (signed-reg unsigned-reg))


    
;;; Result is a fixnum, so we can just shift.  We need the result type
;;; restriction because of the control-stack ambiguity noted above.
;;;
(define-vop (move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:note "fixnum tagging")
  (:generator 1
    (inst slln y x fixnum-tag-bits)))
;;;
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))


;;; Result may be a bignum, so we have to check.  Use a worst-case cost to make
;;; sure people know they may be number consing.
;;;
(define-vop (move-from-signed)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:note "signed word to integer coercion")
  (:generator 20
    (move x arg)
    (let ((done (gen-label)))
      ;; Need to figure out if we have a fixnum or not, so look at the
      ;; top 3 bits of the 32-bit int.  If these 3 bits are 0 or 7,
      ;; then we have a fixnum.  Otherwise we need to allocate a
      ;; bignum.
      ;;
      ;; A simple way to tell if those 3 bits are 0 or 7 was given by
      ;; Frode Vatvedt Fjeld: (zerop (logand #b110 (1+ temp)))
      (inst srln temp x positive-fixnum-bits)
      (inst add temp 1)
      (inst and temp #b110)
      (inst cmp temp)
      (inst b :eq done)
      (inst slln y x fixnum-tag-bits)
      
      (with-fixed-allocation
	(y temp bignum-type (1+ bignum-digits-offset))
	(storew x y bignum-digits-offset other-pointer-type))
      (emit-label done))))
;;;
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))

;;; Result may be a bignum, so we have to check.  Use a worst-case cost to make
;;; sure people know they may be number consing.
;;;
    
;;; Check for fixnum, and possibly allocate one or two word bignum result.  Use
;;; a worst-case cost to make sure people know they may be number consing.
;;;
#-sparc-v9
(define-vop (move-from-unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (move x arg)
    (let ((done (gen-label))
	  (one-word (gen-label)))
      (inst sran temp x positive-fixnum-bits)
      (inst cmp temp)
      (inst b :eq done)
      (inst slln y x fixnum-tag-bits)

      ;; We always allocate 2 words even if we don't need it.  (The
      ;; copying GC will take care of freeing the unused extra word.)
      (with-fixed-allocation
	  (y temp bignum-type (+ 2 bignum-digits-offset))
	(inst cmp x)
	(inst b :ge one-word)
	(inst li temp (logior (ash 1 type-bits) bignum-type))
	(inst li temp (logior (ash 2 type-bits) bignum-type))
	(emit-label one-word)
	;; Set the header word, then the actual digit.  The extra
	;; digit, if any, is automatically set to zero, so we don't
	;; have to.
	(storew temp y 0 other-pointer-type)
	(storew x y bignum-digits-offset other-pointer-type))
      (emit-label done))))

#+sparc-v9
(define-vop (move-from-unsigned)
  (:args (arg :scs (signed-reg unsigned-reg) :target x))
  (:results (y :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) x temp)
  (:note "unsigned word to integer coercion")
  (:generator 20
    (move x arg)
    (let ((done (gen-label)))
      (inst sran temp x positive-fixnum-bits)
      (inst cmp temp)
      (inst b :eq done)
      (inst slln y x fixnum-tag-bits)

      ;; We always allocate 2 words even if we don't need it.  (The
      ;; copying GC will take care of freeing the unused extra word.)
      (with-fixed-allocation (y temp nil (+ 2 bignum-digits-offset))
	(inst cmp x)
	(inst li temp (logior (ash 2 type-bits) bignum-type))
	(inst cmove :ge temp (logior (ash 1 type-bits) bignum-type))
	;; Set the header word, then the actual digit.  The extra
	;; digit, if any, is automatically set to zero, so we don't
	;; have to.
	(storew temp y 0 other-pointer-type)
	(storew x y bignum-digits-offset other-pointer-type))
      (emit-label done))))
;;;
(define-move-vop move-from-unsigned :move
  (unsigned-reg) (descriptor-reg))


;;; Move untagged numbers.
;;;
(define-vop (word-move)
  (:args (x :target y
	    :scs (signed-reg unsigned-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (signed-reg unsigned-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:note "word integer move")
  (:generator 0
    (move y x)))
;;;
(define-move-vop word-move :move
  (signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Move untagged number arguments/return-values.
;;;
(define-vop (move-word-argument)
  (:args (x :target y
	    :scs (signed-reg unsigned-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:note "word integer argument move")
  (:generator 0
    (sc-case y
      ((signed-reg unsigned-reg)
       (move y x))
      ((signed-stack unsigned-stack)
       (storew x fp (tn-offset y) 0 temp)))))
;;;
(define-move-vop move-word-argument :move-argument
  (descriptor-reg any-reg signed-reg unsigned-reg) (signed-reg unsigned-reg))

;;; Use standard MOVE-ARGUMENT + coercion to move an untagged number to a
;;; descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

;; 64-bit stuff
#+(and sparc-v9 sparc-v8plus)
(progn

;; Move a signed-reg to a signed64-reg by sign-extending.  (Is this
;; needed?)
(define-move-function (load-signed64-signed 1) (vop x y)
  ((signed-reg) (signed64-reg unsigned64-reg))
  (inst signx y x))

;; Move a signed64-reg to signed-reg by setting the high 32 bits to be
;; the sign.  (Is this needed and will this do the right thing when
;; that signed64-reg actually has more than 32 significant bits?)
#+nil
(define-move-function (load-signed-signed64 1) (vop x y)
  ((signed64-reg) (signed-reg))
  (inst signx y x))

;; Load a 64-bit number from the stack
(define-move-function (load-number-stack-64 5) (vop x y)
  ((signed64-stack) (signed64-reg)
   (unsigned64-stack) (unsigned64-reg))
  (let ((nfp (current-nfp-tn vop)))
    (load64 y nfp (tn-offset x))))

;; Save a 64-bit number to the stack
(define-move-function (store-number-stack-64 5) (vop x y)
  ((signed64-reg) (signed64-stack)
   (unsigned64-reg) (unsigned64-stack))
  (let ((nfp (current-nfp-tn vop)))
    (store64 x nfp (tn-offset y))))

;; Move a tagged integer to a raw double-word representation.
(define-vop (move-to-64bit-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed64-reg unsigned64-reg)))
  (:arg-types tagged-num)
  (:note "fixnum untagging")
  (:generator 0
    ;; Sign-extend the fixnum and then remove the tag.  (Can't just
    ;; remove the tag because we don't know for sure if X has been
    ;; sign-extended to 64-bits.  Let's be safe.)
    (inst signx y x)	      
    (inst srax y y fixnum-tag-bits)))

(define-move-vop move-to-64bit-word/fixnum :move
  (any-reg descriptor-reg) (signed64-reg unsigned64-reg))

;; Arg is a non-immediate constant, load it.
(define-vop (move-to-64bit-word-c)
  (:args (x :scs (constant)))
  (:results (y :scs (signed64-reg unsigned64-reg)))
  (:note "constant load")
  (:generator 1
    (inst li64 y (tn-value x))))

(define-move-vop move-to-64bit-word-c :move
  (constant) (signed64-reg unsigned64-reg))

;; Arg is a fixnum or bignum.  Figure out which and load if necessary
(define-vop (move-to-64bit-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (signed64-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (signed64-reg)) temp)
  (:generator 4
    (let ((done (gen-label)))
      (inst andcc temp x fixnum-tag-mask)
      (inst signx temp x)		; sign-extend X to TEMP
      (inst b :eq done :pt :xcc)
      (inst sran y temp fixnum-tag-bits)	; Zap the tag bits

      ;; We have a bignum.  We need to check the length.  If the
      ;; length is 1, just get the one word.  If it's 2, we need to
      ;; get both words.

      (loadw temp x 0 other-pointer-type)
      (inst srln temp 8)
      (inst cmp temp 1)
      (inst b :eq done)
      ;; Get the low word and sign-extend it
      (loadsw y x bignum-digits-offset other-pointer-type)

      
      ;; Get the high word and then the low word.  Merge them
      ;; together. (If we knew that bignum digits started on an 8-byte
      ;; boundary, we could do an 8-byte load and them manipulate the
      ;; pieces to get the order we want.  I think this would require
      ;; adding a filler word to the bignum type in objdef.lisp.  But
      ;; then every bignum has a wasted word.  Is that ok?)
      (loadw temp x (1+ bignum-digits-offset) other-pointer-type)
      (inst sllx temp temp 32)
      (loadw y x bignum-digits-offset other-pointer-type)
      (inst or y temp)

      (emit-label done)

      )))

(define-move-vop move-to-64bit-word/integer :move
  (descriptor-reg) (signed64-reg))

;; Move a signed-byte 32 to a signed-byte 64.  (Is this ever called?
;; I don't think so.)
(define-vop (move-to-64bit-word/signed)
  (:args (x :scs (signed-reg)))
  (:results (y :scs (signed64-reg)))
  (:arg-types signed-num)
  (:generator 0
    ;; Sign-extend the 32-bit number
    (inst signx y x)))

(define-move-vop move-to-64bit-word/signed :move
  (signed-reg) (signed64-reg unsigned64-reg))

;; Move an unsigned-byte 32 to signed-byte 64.  (I don't think this
;; ever gets called.)
(define-vop (move-to-64bit-word/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:results (y :scs (signed64-reg)))
  (:arg-types unsigned-num)
  (:generator 1
    ;; Zero-extend the 32-bit number	      
    (inst clruw y x)))

(define-move-vop move-to-64bit-word/unsigned :move
  (unsigned-reg) (signed64-reg unsigned64-reg))

;; Save a 64-bit int to a bignum.
(define-vop (move-from-signed64)
  (:args (arg :scs (signed64-reg) :target x))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (signed64-reg) :from (:argument 0)) x temp)
  (:note "signed 64-bit word to integer coercion")
  (:generator 20
    (move x arg)
    (let ((fixnum (gen-label))
	  (done (gen-label)))
      ;; See if the result will fit in a fixnum.
      (inst srax temp x positive-fixnum-bits)
      (inst cmp temp)
      ;; If result is all zeroes, we have a positive fixnum.
      (inst b :eq fixnum :pt :xcc)
      (inst orncc temp zero-tn temp)
      ;; If result is all zeroes, we have a negative fixnum.
      (inst b :eq done :pt :xcc)
      (inst slln y x fixnum-tag-bits)

      ;; A 64-bit signed integer takes exactly 2 bignum digits
      (with-fixed-allocation
	(y temp bignum-type (+ 2 bignum-digits-offset))
	;; Store the low word at the low address, the high word at the
	;; higher address.  (Like move-to-64bit-word/integer, if we knew
	;; the first bignum digit was on a 8-byte boundary, we could
	;; just do a single 8-byte store instead of 2 stores here.)
	(storew x y bignum-digits-offset other-pointer-type)
	(inst srax x x 32)
	(storew x y (1+ bignum-digits-offset) other-pointer-type))
      (inst b done)
      (inst nop)
      
      (emit-label fixnum)
      (inst slln y x fixnum-tag-bits)
      (emit-label done))))

(define-move-vop move-from-signed64 :move
  (signed64-reg) (descriptor-reg))

;; Save an unsigned 64-bit int to a bignum.
(define-vop (move-from-unsigned64)
  (:args (arg :scs (unsigned64-reg) :target x))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (unsigned64-reg) :from (:argument 0)) x temp)
  (:note "unsigned 64-bit word to integer coercion")
  (:generator 20
    (move x arg)
    (let ((two-words (gen-label))
	  (done (gen-label)))
      ;; See if the result will fit in a fixnum.
      (inst srax temp x positive-fixnum-bits)
      (inst cmp temp)
      ;; If result is all zeroes, we have a positive fixnum.
      (inst b :eq done :pt :xcc)
      (inst slln y x fixnum-tag-bits)

      ;; A unsigned 64-bit signed integer takes exactly 2 or 3 bignum
      ;; digits.  We always allocate 3.  (The copying GC will take
      ;; care of freeing the unused extra word, if any.)
      (with-fixed-allocation
	(y temp bignum-type (+ 3 bignum-digits-offset))
	(inst cmp x)
	(inst b :ge two-words :pn :xcc)
	(inst li temp (logior (ash 2 type-bits) bignum-type))
	(inst li temp (logior (ash 3 type-bits) bignum-type))
	(emit-label two-words)
	;; Set the header word with the correct bignum length.
	(storew temp y 0 other-pointer-type)
	;; Store the low word at the low address, the high word at the
	;; higher address.  (Like move-to-64bit-word/integer, if we knew
	;; the first bignum digit was on a 8-byte boundary, we could
	;; just do a single 8-byte store instead of 2 stores here.)
	(storew x y bignum-digits-offset other-pointer-type)
	(inst srax x x 32)
	(storew x y (1+ bignum-digits-offset) other-pointer-type))
      (emit-label done))))

(define-move-vop move-from-unsigned64 :move
  (unsigned64-reg) (descriptor-reg))

(define-vop (move-to-unsigned-64bit-word/integer)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (unsigned64-reg)))
  (:note "integer to untagged word coercion")
  (:temporary (:scs (unsigned64-reg)) temp)
  (:generator 4
    (let ((done (gen-label)))
      (inst andcc temp x fixnum-tag-mask)
      (inst signx temp x)		; sign-extend X to TEMP
      (inst b :eq done :pt :xcc)
      (inst sran y temp fixnum-tag-bits)	; Zap the tag bits

      ;; We have a bignum.  We need to check the length.  If the
      ;; length is 1, just get the one word.  If it's 2, we need to
      ;; get both words.

      (loadw temp x 0 other-pointer-type)
      (inst srln temp 8)
      (inst cmp temp 1)
      (inst b :eq done)
      ;; Get the low word and zero-extend it and we're done.
      (loadw y x bignum-digits-offset other-pointer-type)

      
      ;; Get the high word and then the low word.  Merge them
      ;; together. (If we knew that bignum digits started on an 8-byte
      ;; boundary, we could do an 8-byte load and them manipulate the
      ;; pieces to get the order we want.  I think this would require
      ;; adding a filler word to the bignum type in objdef.lisp.  But
      ;; then every bignum has a wasted word.  Is that ok?)
      (loadw temp x (1+ bignum-digits-offset) other-pointer-type)
      (inst sllx temp temp 32)
      (loadw y x bignum-digits-offset other-pointer-type)
      (inst or y temp)

      (emit-label done)

      )))

(define-move-vop move-to-unsigned-64bit-word/integer :move
  (descriptor-reg) (unsigned64-reg))

(define-vop (64bit-word-move)
  (:args (x :target y
	    :scs (signed64-reg unsigned64-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (signed64-reg unsigned64-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:note "word integer move")
  (:generator 0
    (move y x)))

(define-move-vop 64bit-word-move :move
  (signed64-reg unsigned64-reg) (signed64-reg unsigned64-reg))

;; Move untagged number arguments/return-values.
(define-vop (move-64bit-word-argument)
  (:args (x :target y
	    :scs (signed-reg signed64-reg unsigned64-reg immediate))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note "word integer argument move")
  (:generator 0
    (sc-case y
      ((signed64-reg unsigned64-reg)
       (sc-case x
	 ((signed64-reg unsigned64-reg)
	  (move y x))
	 (signed-reg
	  (inst signx y x))
	 (immediate
	  (inst li64 y (tn-value x)))))
      ((signed64-stack unsigned64-stack)
       (store64 x fp (tn-offset y))))))

(define-move-vop move-64bit-word-argument :move-argument
  (descriptor-reg signed64-reg unsigned64-reg) (signed64-reg unsigned64-reg))

(define-move-vop move-argument :move-argument
  (signed64-reg unsigned64-reg) (descriptor-reg))

)
