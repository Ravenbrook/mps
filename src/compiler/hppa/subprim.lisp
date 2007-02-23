;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/hppa/subprim.lisp,v 1.3 2003/08/03 11:27:48 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Linkage information for standard static functions, and random vops.
;;;
;;; Written by William Lott.
;;; 
(in-package "HPPA")



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg) :target ptr))
  (:arg-types list)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) ptr)
  (:temporary (:scs (non-descriptor-reg) :type random) temp)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0) :target result)
	      count)
  (:results (result :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (move object ptr)
    (inst comb := ptr null-tn done)
    (inst li 0 count)

    (inst extru ptr 31 3 temp)
    (inst comib :<> list-pointer-type temp loose :nullify t)
    (loadw ptr ptr vm:cons-cdr-slot vm:list-pointer-type)

    LOOP
    (inst addi (fixnumize 1) count count)
    (inst comb := ptr null-tn done :nullify t)
    (inst extru ptr 31 3 temp)
    (inst comib := list-pointer-type temp loop :nullify t)
    (loadw ptr ptr vm:cons-cdr-slot vm:list-pointer-type)

    LOOSE
    (cerror-call vop done object-not-list-error ptr)

    DONE
    (move count result)))

(define-static-function length (object) :translate length)
