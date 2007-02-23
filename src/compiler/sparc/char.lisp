;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/char.lisp,v 1.12 2003/10/20 01:25:01 toy Exp $")
;;;
;;; **********************************************************************
;;; 
;;; This file contains the SPARC VM definition of character operations.
;;;
;;; Written by Rob MacLachlan
;;; Converted for the MIPS R2000 by Christopher Hoover.
;;; And then to the SPARC by William Lott.
;;;
(in-package "SPARC")



;;;; Moves and coercions:

;;; Move a tagged char to an untagged representation.
;;;
(define-vop (move-to-base-char)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (base-char-reg)))
  (:note "character untagging")
  (:generator 1
    (inst srln y x vm:type-bits)))
;;;
(define-move-vop move-to-base-char :move
  (any-reg descriptor-reg) (base-char-reg))


;;; Move an untagged char to a tagged representation.
;;;
(define-vop (move-from-base-char)
  (:args (x :scs (base-char-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note "character tagging")
  (:generator 1
    (inst slln y x vm:type-bits)
    (inst or y vm:base-char-type)))
;;;
(define-move-vop move-from-base-char :move
  (base-char-reg) (any-reg descriptor-reg))

;;; Move untagged base-char values.
;;;
(define-vop (base-char-move)
  (:args (x :target y
	    :scs (base-char-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (base-char-reg)
	       :load-if (not (location= x y))))
  (:note "character move")
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
;;;
(define-move-vop base-char-move :move
  (base-char-reg) (base-char-reg))


;;; Move untagged base-char arguments/return-values.
;;;
(define-vop (move-base-char-argument)
  (:args (x :target y
	    :scs (base-char-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y base-char-reg))))
  (:results (y))
  (:temporary (:sc non-descriptor-reg) temp)
  (:note "character arg move")
  (:generator 0
    (sc-case y
      (base-char-reg
       (move y x))
      (base-char-stack
       (storew x fp (tn-offset y) 0 temp)))))
;;;
(define-move-vop move-base-char-argument :move-argument
  (any-reg base-char-reg) (base-char-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged base-char
;;; to a descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (base-char-reg) (any-reg descriptor-reg))



;;;; Other operations:

(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (base-char-reg) :target res))
  (:arg-types base-char)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst slln res ch fixnum-tag-bits)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (any-reg) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 1
    (inst srln res code fixnum-tag-bits)))


;;; Comparison of base-chars.
;;;
(define-vop (base-char-compare)
  (:args (x :scs (base-char-reg))
	 (y :scs (base-char-reg)))
  (:arg-types base-char base-char)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition not-condition)
  (:generator 3
    (inst cmp x y)
    (inst b (if not-p not-condition condition) target)
    (inst nop)))

(define-vop (fast-char=/base-char base-char-compare)
  (:translate char=)
  (:variant :eq :ne))

(define-vop (fast-char</base-char base-char-compare)
  (:translate char<)
  (:variant :ltu :geu))

(define-vop (fast-char>/base-char base-char-compare)
  (:translate char>)
  (:variant :gtu :leu))

(define-vop (base-char-compare-c)
  (:args (x :scs (base-char-reg)))
  (:arg-types base-char (:constant base-char))
  (:conditional)
  (:info target not-p y)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition not-condition)
  (:generator 2
    (inst cmp x (char-code y))
    (inst b (if not-p not-condition condition) target)
    (inst nop)))

(define-vop (fast-char=-c/base-char base-char-compare-c)
  (:translate char=)
  (:variant :eq :ne))

(define-vop (fast-char<-c/base-char base-char-compare-c)
  (:translate char<)
  (:variant :ltu :geu))

(define-vop (fast-char>-c/base-char base-char-compare-c)
  (:translate char>)
  (:variant :gtu :leu))

