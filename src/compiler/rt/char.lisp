;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/char.lisp,v 1.4 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/char.lisp,v 1.4 1994/10/31 04:45:41 ram Exp $
;;; 
;;; This file contains the RT VM definition of character operations.
;;;
;;; Written by Rob MacLachlan and Bill Chiles.
;;;

(in-package "RT")



;;;; Moves and coercions:

;;; MOVE-TO-BASE-CHAR -- VOP.
;;;
;;; Move a tagged char to an untagged representation.
;;;
(define-vop (move-to-base-char)
  (:args (x :scs (any-reg descriptor-reg) :target y))
  (:arg-types base-char)
  (:results (y :scs (base-char-reg)))
  (:generator 1
    (move y x)
    (inst sr y vm:type-bits)))
;;;
(define-move-vop move-to-base-char :move
  (any-reg descriptor-reg) (base-char-reg))


;;; MOVE-FROM-BASE-CHAR -- VOP.
;;;
;;; Move an untagged char to a tagged representation.
;;;
(define-vop (move-from-base-char)
  (:args (x :scs (base-char-reg) :target temp))
  (:temporary (:scs (base-char-reg) :from (:argument 0)) temp)
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types base-char)
  (:generator 1
    (move temp x)
    (inst sl temp vm:type-bits)
    (inst oil y temp vm:base-char-type)))
;;;
(define-move-vop move-from-base-char :move
  (base-char-reg) (any-reg descriptor-reg))

;;; BASE-CHAR-MOVE -- VOP.
;;;
;;; Move untagged base-char values.
;;;
(define-vop (base-char-move)
  (:args (x :target y
	    :scs (base-char-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (base-char-reg)
	       :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
;;;
(define-move-vop base-char-move :move
  (base-char-reg) (base-char-reg))


;;; MOVE-BASE-CHAR-ARGUMENT -- VOP.
;;;
;;; Move untagged base-char arguments/return-values.
;;;
(define-vop (move-base-char-argument)
  (:args (x :target y
	    :scs (base-char-reg))
	 (fp :scs (word-pointer-reg)
	     :load-if (not (sc-is y base-char-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (base-char-reg
       (move y x))
      (base-char-stack
       (storew x fp (tn-offset y))))))
;;;
(define-move-vop move-base-char-argument :move-argument
  (any-reg base-char-reg) (base-char-reg))


;;; Use standard MOVE-ARGUMENT + coercion to move an untagged base-char
;;; to a descriptor passing location.
;;;
(define-move-vop move-argument :move-argument
  (base-char-reg) (any-reg descriptor-reg))



;;;; Other operations:

;;; CHAR-CODE -- VOP.
;;;
;;; This assumes it is best to keep characters in their raw representation.
;;;
(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (base-char-reg) :target temp))
  (:arg-types base-char)
  (:temporary (:scs (base-char-reg)
		    :from (:argument 0) :to (:result 0)
		    :target res) temp)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (move temp ch)
    (inst sl temp 2)
    (move res temp)))

;;; CODE-CHAR -- VOP.
;;;
(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (any-reg) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 1
    (move res code)
    (inst sr res 2)))



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
  (:variant-vars condition)
  (:generator 6
    (inst cl x y)
    (if not-p
	(inst bnc condition target)
	(inst bc condition target))))

(define-vop (fast-char=/base-char base-char-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char</base-char base-char-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char>/base-char base-char-compare)
  (:translate char>)
  (:variant :gt))
