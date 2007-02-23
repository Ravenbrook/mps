;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/subprim.lisp,v 1.2 2003/08/03 11:27:47 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Linkage information for standard static functions, and random vops.
;;;
;;; Written by William Lott.
;;; 
(in-package "PPC")



;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg) :target ptr))
  (:arg-types list)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) ptr)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0) :target result)
	      count)
  (:results (result :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (let ((done (gen-label))
	  (loop (gen-label))
	  (not-list (generate-cerror-code vop object-not-list-error object)))
      (move ptr object)
      (move count zero-tn)

      (emit-label loop)

      (inst cmpw ptr null-tn)
      (inst beq done)

      (test-type ptr temp not-list t vm:list-pointer-type)

      (loadw ptr ptr vm:cons-cdr-slot vm:list-pointer-type)
      (inst addi count count (fixnumize 1))
      (test-type ptr temp loop nil vm:list-pointer-type)

      (cerror-call vop done object-not-list-error ptr)

      (emit-label done)
      (move result count))))
       

(define-static-function length (object) :translate length)

