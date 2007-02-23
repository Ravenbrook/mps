;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/float.lisp,v 1.7 2006/07/01 13:52:48 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating point support for the PPC.
;;;
;;; Written by Rob MacLachlan
;;; Sparc conversion by William Lott.
;;;
(in-package "PPC")


;;;; Move functions:

(define-move-function (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst lfs y (current-nfp-tn vop) (* (tn-offset x) vm:word-bytes)))

(define-move-function (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst stfs x (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes)))


(define-move-function (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (inst lfd y nfp offset)))

(define-move-function (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (inst stfd x nfp offset)))



;;;; Move VOPs:

(macrolet ((frob (vop sc)
	     `(progn
		(define-vop (,vop)
		  (:args (x :scs (,sc)
			    :target y
			    :load-if (not (location= x y))))
		  (:results (y :scs (,sc)
			       :load-if (not (location= x y))))
		  (:note "float move")
		  (:generator 0
		    (unless (location= y x)
                      (inst fmr y x))))
		(define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg)
  (frob double-move double-reg))


(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:variant-vars double-p size type data)
  (:generator 13
    (with-fixed-allocation (y pa-flag ndescr type size))
    (if double-p
	(inst stfd x y (- (* data vm:word-bytes) vm:other-pointer-type))
	(inst stfs x y (- (* data vm:word-bytes) vm:other-pointer-type)))))

(macrolet ((frob (name sc &rest args)
	     `(progn
		(define-vop (,name move-from-float)
		  (:args (x :scs (,sc) :to :save))
		  (:results (y :scs (descriptor-reg)))
		  (:variant ,@args))
		(define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg
    nil vm:single-float-size vm:single-float-type vm:single-float-value-slot)
  (frob move-from-double double-reg
    t vm:double-float-size vm:double-float-type vm:double-float-value-slot))

(macrolet ((frob (name sc double-p value)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (descriptor-reg)))
		  (:results (y :scs (,sc)))
		  (:note "pointer to float coercion")
		  (:generator 2
		    (inst ,(if double-p 'lfd 'lfs) y x
			  (- (* ,value vm:word-bytes) vm:other-pointer-type))))
		(define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg nil vm:single-float-value-slot)
  (frob move-to-double double-reg t vm:double-float-value-slot))


(macrolet ((frob (name sc stack-sc double-p)
	     `(progn
		(define-vop (,name)
		  (:args (x :scs (,sc) :target y)
			 (nfp :scs (any-reg)
			      :load-if (not (sc-is y ,sc))))
		  (:results (y))
		  (:note "float argument move")
		  (:generator ,(if double-p 2 1)
		    (sc-case y
		      (,sc
		       (unless (location= x y)
			 (inst fmr y x)))
		      (,stack-sc
		       (let ((offset (* (tn-offset y) vm:word-bytes)))
			 (inst ,(if double-p 'stfd 'stfs) x nfp offset))))))
		(define-move-vop ,name :move-argument
		  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-argument single-reg single-stack nil)
  (frob move-double-float-argument double-reg double-stack t))



;;;; Complex float move functions

(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg *backend*)
		  :offset (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (1+ (tn-offset x))))

#+double-double
(progn
(defun complex-double-double-reg-real-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))
(defun complex-double-double-reg-real-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 1 (tn-offset x))))
(defun complex-double-double-reg-imag-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 2 (tn-offset x))))
(defun complex-double-double-reg-imag-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (+ 3 (tn-offset x))))
)

(define-move-function (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst lfs real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst lfs imag-tn nfp (+ offset vm:word-bytes)))))

(define-move-function (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst stfs real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst stfs imag-tn nfp (+ offset vm:word-bytes)))))


(define-move-function (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst lfd real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst lfd imag-tn nfp (+ offset (* 2 vm:word-bytes))))))

(define-move-function (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst stfd real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst stfd imag-tn nfp (+ offset (* 2 vm:word-bytes))))))


#+double-double
(progn
(define-move-function (load-complex-double-double 4) (vop x y)
  ((complex-double-double-stack) (complex-double-double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (let ((value-tn (complex-double-double-reg-real-hi-tn y)))
      (inst lfd value-tn nfp offset))
    (let ((value-tn (complex-double-double-reg-real-lo-tn y)))
      (inst lfd value-tn nfp (+ offset (* 2 vm:word-bytes))))
    (let ((value-tn (complex-double-double-reg-imag-hi-tn y)))
      (inst lfd value-tn nfp (+ offset (* 4 vm:word-bytes))))
    (let ((value-tn (complex-double-double-reg-imag-lo-tn y)))
      (inst lfd value-tn nfp (+ offset (* 6 vm:word-bytes))))))

(define-move-function (store-complex-double-double 4) (vop x y)
  ((complex-double-double-reg) (complex-double-double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (let ((value-tn (complex-double-double-reg-real-hi-tn x)))
      (inst stfd value-tn nfp offset))
    (let ((value-tn (complex-double-double-reg-real-lo-tn x)))
      (inst stfd value-tn nfp (+ offset (* 2 vm:word-bytes))))
    (let ((value-tn (complex-double-double-reg-imag-hi-tn x)))
      (inst stfd value-tn nfp (+ offset (* 4 vm:word-bytes))))
    (let ((value-tn (complex-double-double-reg-imag-lo-tn x)))
      (inst stfd value-tn nfp (+ offset (* 6 vm:word-bytes))))))

)
;;;
;;; Complex float register to register moves.
;;;
(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
	    :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:note "complex single float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-single-reg-real-tn x))
	     (y-real (complex-single-reg-real-tn y)))
	 (inst fmr y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
	     (y-imag (complex-single-reg-imag-tn y)))
	 (inst fmr y-imag x-imag)))))
;;;
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-double-reg-real-tn x))
	     (y-real (complex-double-reg-real-tn y)))
	 (inst fmr y-real x-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
	     (y-imag (complex-double-reg-imag-tn y)))
	 (inst fmr y-imag x-imag)))))
;;;
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))

#+double-double
(define-vop (complex-double-double-move)
  (:args (x :scs (complex-double-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-double-double-reg-real-hi-tn x))
	     (y-real (complex-double-double-reg-real-hi-tn y)))
	 (inst fmr y-real x-real))
       (let ((x-real (complex-double-double-reg-real-lo-tn x))
	     (y-real (complex-double-double-reg-real-lo-tn y)))
	 (inst fmr y-real x-real))
       (let ((x-real (complex-double-double-reg-imag-hi-tn x))
	     (y-real (complex-double-double-reg-imag-hi-tn y)))
	 (inst fmr y-real x-real))
       (let ((x-imag (complex-double-double-reg-imag-lo-tn x))
	     (y-imag (complex-double-double-reg-imag-lo-tn y)))
	 (inst fmr y-imag x-imag)))))
;;;
#+double-double
(define-move-vop complex-double-double-move :move
  (complex-double-double-reg) (complex-double-double-reg))

;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag ndescr vm:complex-single-float-type
			       vm:complex-single-float-size))
     (let ((real-tn (complex-single-reg-real-tn x)))
       (inst stfs real-tn y (- (* vm:complex-single-float-real-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))
     (let ((imag-tn (complex-single-reg-imag-tn x)))
       (inst stfs imag-tn y (- (* vm:complex-single-float-imag-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))))
;;;
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag ndescr vm:complex-double-float-type
			       vm:complex-double-float-size))
     (let ((real-tn (complex-double-reg-real-tn x)))
       (inst stfd real-tn y (- (* vm:complex-double-float-real-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))
     (let ((imag-tn (complex-double-reg-imag-tn x)))
       (inst stfd imag-tn y (- (* vm:complex-double-float-imag-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))))
;;;
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))

#+double-double
(define-vop (move-from-complex-double-double)
  (:args (x :scs (complex-double-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag ndescr vm::complex-double-double-float-type
			       vm::complex-double-double-float-size))
     (let ((real-tn (complex-double-double-reg-real-hi-tn x)))
       (inst stfd real-tn y (- (* vm::complex-double-double-float-real-hi-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))
     (let ((real-tn (complex-double-double-reg-real-lo-tn x)))
       (inst stfd real-tn y (- (* vm::complex-double-double-float-real-lo-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))
     (let ((imag-tn (complex-double-double-reg-imag-hi-tn x)))
       (inst stfd imag-tn y (- (* vm::complex-double-double-float-imag-hi-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))
     (let ((imag-tn (complex-double-double-reg-imag-lo-tn x)))
       (inst stfd imag-tn y (- (* vm::complex-double-double-float-imag-lo-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))))
;;;
#+double-double
(define-move-vop move-from-complex-double-double :move
  (complex-double-double-reg) (descriptor-reg))


;;;
;;; Move from a descriptor to a complex float register
;;;
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst lfs real-tn x (- (* complex-single-float-real-slot word-bytes)
			     other-pointer-type)))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst lfs imag-tn x (- (* complex-single-float-imag-slot word-bytes)
			     other-pointer-type)))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst lfd real-tn x (- (* complex-double-float-real-slot word-bytes)
			     other-pointer-type)))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst lfd imag-tn x (- (* complex-double-float-imag-slot word-bytes)
			     other-pointer-type)))))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))


#+double-double
(define-vop (move-to-complex-double-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-double-reg-real-hi-tn y)))
      (inst lfd real-tn x (- (* complex-double-double-float-real-hi-slot word-bytes)
			      other-pointer-type)))
    (let ((real-tn (complex-double-double-reg-real-lo-tn y)))
      (inst lfd real-tn x (- (* complex-double-double-float-real-lo-slot word-bytes)
			      other-pointer-type)))
    (let ((imag-tn (complex-double-double-reg-imag-hi-tn y)))
      (inst lfd imag-tn x (- (* complex-double-double-float-imag-hi-slot word-bytes)
			      other-pointer-type)))
    (let ((imag-tn (complex-double-double-reg-imag-lo-tn y)))
      (inst lfd imag-tn x (- (* complex-double-double-float-imag-lo-slot word-bytes)
			      other-pointer-type)))))
#+double-double
(define-move-vop move-to-complex-double-double :move
  (descriptor-reg) (complex-double-double-reg))
;;;
;;; Complex float move-argument vop
;;;
(define-vop (move-complex-single-float-argument)
  (:args (x :scs (complex-single-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note "complex single-float argument move")
  (:generator 1
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
	 (let ((x-real (complex-single-reg-real-tn x))
	       (y-real (complex-single-reg-real-tn y)))
	   (inst fmr y-real x-real))
	 (let ((x-imag (complex-single-reg-imag-tn x))
	       (y-imag (complex-single-reg-imag-tn y)))
	   (inst fmr y-imag x-imag))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((real-tn (complex-single-reg-real-tn x)))
	   (inst stfs real-tn nfp offset))
	 (let ((imag-tn (complex-single-reg-imag-tn x)))
	   (inst stfs imag-tn nfp (+ offset word-bytes))))))))
(define-move-vop move-complex-single-float-argument :move-argument
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-argument)
  (:args (x :scs (complex-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "complex double-float argument move")
  (:generator 2
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
	 (let ((x-real (complex-double-reg-real-tn x))
	       (y-real (complex-double-reg-real-tn y)))
	   (inst fmr y-real x-real))
	 (let ((x-imag (complex-double-reg-imag-tn x))
	       (y-imag (complex-double-reg-imag-tn y)))
	   (inst fmr y-imag x-imag))))
      (complex-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((real-tn (complex-double-reg-real-tn x)))
	   (inst stfd real-tn nfp offset))
	 (let ((imag-tn (complex-double-reg-imag-tn x)))
	   (inst stfd imag-tn nfp (+ offset (* 2 word-bytes)))))))))
(define-move-vop move-complex-double-float-argument :move-argument
  (complex-double-reg descriptor-reg) (complex-double-reg))

#+double-double
(define-vop (move-complex-double-double-float-argument)
  (:args (x :scs (complex-double-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-double-reg))))
  (:results (y))
  (:note "complex long-float argument move")
  (:generator 2
    (sc-case y
      (complex-double-double-reg
       (unless (location= x y)
	 (let ((x-real (complex-double-double-reg-real-hi-tn x))
	       (y-real (complex-double-double-reg-real-hi-tn y)))
	   (inst fmr y-real x-real))
	 (let ((x-real (complex-double-double-reg-real-lo-tn x))
	       (y-real (complex-double-double-reg-real-lo-tn y)))
	   (inst fmr y-real x-real))
	 (let ((x-imag (complex-double-double-reg-imag-hi-tn x))
	       (y-imag (complex-double-double-reg-imag-hi-tn y)))
	   (inst fmr y-imag x-imag))
	 (let ((x-imag (complex-double-double-reg-imag-lo-tn x))
	       (y-imag (complex-double-double-reg-imag-lo-tn y)))
	   (inst fmr y-imag x-imag))))
      (complex-double-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((real-tn (complex-double-double-reg-real-hi-tn x)))
	   (inst stfd real-tn nfp offset))
	 (let ((real-tn (complex-double-double-reg-real-lo-tn x)))
	   (inst stfd real-tn nfp (+ offset (* 2 word-bytes))))
	 (let ((imag-tn (complex-double-double-reg-imag-hi-tn x)))
	   (inst stfd imag-tn nfp (+ offset (* 4 word-bytes))))
	 (let ((imag-tn (complex-double-double-reg-imag-lo-tn x)))
	   (inst stfd imag-tn nfp (+ offset (* 6 word-bytes)))))))))

#+double-double
(define-move-vop move-complex-double-double-float-argument :move-argument
  (complex-double-double-reg descriptor-reg) (complex-double-double-reg))

#+long-float
(define-move-vop move-complex-long-float-argument :move-argument
  (complex-long-reg descriptor-reg) (complex-long-reg))

(define-move-vop move-argument :move-argument
  (single-reg double-reg complex-single-reg complex-double-reg
	      #+double-double double-double-reg
	      #+double-double complex-double-double-reg)
  (descriptor-reg))


;;;; Arithmetic VOPs:

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name float-op)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:results (r :scs (,sc)))
		(:arg-types ,ptype ,ptype)
		(:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float))

(macrolet ((frob (op sinst sname scost dinst dname dcost)
	     `(progn
		(define-vop (,sname single-float-op)
		  (:translate ,op)
		  (:generator ,scost
		    (inst ,sinst r x y)))
		(define-vop (,dname double-float-op)
		  (:translate ,op)
		  (:generator ,dcost
		    (inst ,dinst r x y))))))
  (frob + fadds +/single-float 2 fadd +/double-float 2)
  (frob - fsubs -/single-float 2 fsub -/double-float 2)
  (frob * fmuls */single-float 4 fmul */double-float 5)
  (frob / fdivs //single-float 12 fdiv //double-float 19))

(macrolet ((frob (name inst translate sc type)
	     `(define-vop (,name)
		(:args (x :scs (,sc)))
		(:results (y :scs (,sc)))
		(:translate ,translate)
		(:policy :fast-safe)
		(:arg-types ,type)
		(:result-types ,type)
		(:note "inline float arithmetic")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1
		  (note-this-location vop :internal-error)
		  (inst ,inst y x)))))
  (frob abs/single-float fabs abs single-reg single-float)
  (frob abs/double-float fabs abs double-reg double-float)
  (frob %negate/single-float fneg %negate single-reg single-float)
  (frob %negate/double-float fneg %negate double-reg double-float))


;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format yep nope)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (ecase format
      ((:single :double)
       (inst fcmpo :cr1 x y)))
    (inst b?  :cr1 (if not-p nope yep) target)))

(macrolet ((frob (name sc ptype)
	     `(define-vop (,name float-compare)
		(:args (x :scs (,sc))
		       (y :scs (,sc)))
		(:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate yep nope sname dname)
	     `(progn
		(define-vop (,sname single-float-compare)
		  (:translate ,translate)
		  (:variant :single ,yep ,nope))
		(define-vop (,dname double-float-compare)
		  (:translate ,translate)
		  (:variant :double ,yep ,nope)))))
  (frob < :lt :ge </single-float </double-float)
  (frob > :gt :le >/single-float >/double-float)
  (frob = :eq :ne eql/single-float eql/double-float))


;;;; Conversion:

(macrolet ((frob (name translate inst to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (signed-reg)))
		(:temporary (:scs (double-stack)) temp)
                (:temporary (:scs (double-reg)) fmagic)
                (:temporary (:scs (signed-reg)) rtemp)
		(:results (y :scs (,to-sc)))
		(:arg-types signed-num)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (let* ((stack-offset (* (tn-offset temp) vm:word-bytes))
                         (nfp-tn (current-nfp-tn vop))
                         (temp-offset-high (* stack-offset vm:word-bytes))
                         (temp-offset-low (* (1+ stack-offset) vm:word-bytes)))
                    (inst lis rtemp #x4330)   ; High word of magic constant
                    (inst stw rtemp nfp-tn temp-offset-high)
                    (inst lis rtemp #x8000)
                    (inst stw rtemp nfp-tn temp-offset-low)
                    (inst lfd fmagic nfp-tn temp-offset-high)
                    (inst xor rtemp rtemp x)          ; invert sign bit of x : rtemp had #x80000000
                    (inst stw rtemp nfp-tn temp-offset-low)
                    (inst lfd y nfp-tn temp-offset-high) 		    
		    (note-this-location vop :internal-error)
		    (inst ,inst y y fmagic))))))
  (frob %single-float/signed %single-float fsubs single-reg single-float)
  (frob %double-float/signed %double-float fsub double-reg double-float))

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
	     `(define-vop (,name)
		(:args (x :scs (,from-sc)))
		(:results (y :scs (,to-sc)))
		(:arg-types ,from-type)
		(:result-types ,to-type)
		(:policy :fast-safe)
		(:note "inline float coercion")
		(:translate ,translate)
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 2
		  (note-this-location vop :internal-error)
		  (inst ,inst y x)))))
  (frob %single-float/double-float %single-float frsp
    double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float fmr
    single-reg single-float double-reg double-float))

(macrolet ((frob (trans from-sc from-type inst)
	     `(define-vop (,(symbolicate trans "/" from-type))
		(:args (x :scs (,from-sc) :target temp))
		(:temporary (:from (:argument 0) :sc single-reg) temp)
		(:temporary (:scs (double-stack)) stack-temp)
		(:results (y :scs (signed-reg)))
		(:arg-types ,from-type)
		(:result-types signed-num)
		(:translate ,trans)
		(:policy :fast-safe)
		(:note "inline float truncate")
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 5
		  (note-this-location vop :internal-error)
		  (inst ,inst temp x)
		  (inst stfd temp (current-nfp-tn vop)
			(* (tn-offset stack-temp) vm:word-bytes))
		  (inst lwz y (current-nfp-tn vop)
			(+ 4 (* (tn-offset stack-temp) vm:word-bytes)))))))
  (frob %unary-truncate single-reg single-float fctiwz)
  (frob %unary-truncate double-reg double-float fctiwz)
  (frob %unary-round single-reg single-float fctiw)
  (frob %unary-round double-reg double-float fctiw))



(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
	       :load-if (not (sc-is bits signed-stack))))
  (:results (res :scs (single-reg)
		 :load-if (not (sc-is res single-stack))))
  (:temporary (:scs (signed-reg) :from (:argument 0) :to (:result 0)) temp)
  (:temporary (:scs (signed-stack)) stack-temp)
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case res
	 (single-reg
	  (inst stw bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst lfs res (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (single-stack
	  (inst stw bits (current-nfp-tn vop)
		(* (tn-offset res) vm:word-bytes)))))
      (signed-stack
       (sc-case res
	 (single-reg
	  (inst lfs res (current-nfp-tn vop)
		(* (tn-offset bits) vm:word-bytes)))
	 (single-stack
	  (unless (location= bits res)
	    (inst lwz temp (current-nfp-tn vop)
		  (* (tn-offset bits) vm:word-bytes))
	    (inst stw temp (current-nfp-tn vop)
		  (* (tn-offset res) vm:word-bytes)))))))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
	 (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
		 :load-if (not (sc-is res double-stack))))
  (:temporary (:scs (double-stack)) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let ((stack-tn (sc-case res
		      (double-stack res)
		      (double-reg temp))))
      (inst stw hi-bits (current-nfp-tn vop)
	    (* (tn-offset stack-tn) vm:word-bytes))
      (inst stw lo-bits (current-nfp-tn vop)
	    (* (1+ (tn-offset stack-tn)) vm:word-bytes)))
    (when (sc-is res double-reg)
      (inst lfd res (current-nfp-tn vop)
	    (* (tn-offset temp) vm:word-bytes)))))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
		:load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)
		  :load-if (or (sc-is float descriptor-reg single-stack)
			       (not (sc-is bits signed-stack)))))
  (:temporary (:scs (signed-stack)) stack-temp)
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case float
	 (single-reg
	  (inst stfs float (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst lwz bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (single-stack
	  (inst lwz bits (current-nfp-tn vop)
		(* (tn-offset float) vm:word-bytes)))
	 (descriptor-reg
	  (loadw bits float vm:single-float-value-slot vm:other-pointer-type))))
      (signed-stack
       (sc-case float
	 (single-reg
	  (inst stfs float (current-nfp-tn vop)
		(* (tn-offset bits) vm:word-bytes))))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stfd float (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes))
       (inst lwz hi-bits (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes)))
      (double-stack
       (inst lwz hi-bits (current-nfp-tn vop)
	     (* (tn-offset float) vm:word-bytes)))
      (descriptor-reg
       (loadw hi-bits float vm:double-float-value-slot
	      vm:other-pointer-type)))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stfd float (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes))
       (inst lwz lo-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset stack-temp)) vm:word-bytes)))
      (double-stack
       (inst lwz lo-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset float)) vm:word-bytes)))
      (descriptor-reg
       (loadw lo-bits float (1+ vm:double-float-value-slot)
	      vm:other-pointer-type)))))

(define-vop (double-float-bits)
  (:args (float :scs (double-reg descriptor-reg)
		:load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg))
	    (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types signed-num unsigned-num)
  (:translate kernel::double-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stfd float (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes))
       (inst lwz hi-bits (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes))
       (inst lwz lo-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset stack-temp)) vm:word-bytes)))
      (double-stack
       (inst lwz hi-bits (current-nfp-tn vop)
	     (* (tn-offset float) vm:word-bytes))
       (inst lwz lo-bits (current-nfp-tn vop)
	     (* (1+ (tn-offset float)) vm:word-bytes)))
      (descriptor-reg
       (loadw hi-bits float vm:double-float-value-slot
	      vm:other-pointer-type)
       (loadw lo-bits float (1+ vm:double-float-value-slot)
	      vm:other-pointer-type)))))

;; This vop and the next are intended to be used only for moving a
;; float to an integer arg location (register or stack) for C callout.
;; See %alien-funcall ir2convert in aliencomp.lisp.

#+darwin
(define-vop (move-double-to-int-arg)
  (:args (float :scs (double-reg)))
  (:results (hi-bits :scs (signed-reg signed-stack))
	    (lo-bits :scs (unsigned-reg unsigned-stack)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:temporary (:scs (signed-reg)) temp)
  (:arg-types double-float)
  (:result-types signed-num unsigned-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst stfd float (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes))
       (sc-case hi-bits
         (signed-reg		
	  (inst lwz hi-bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (signed-stack
	  (inst lwz temp (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst stw temp nsp-tn
		(* (tn-offset hi-bits) vm:word-bytes))))
       (sc-case lo-bits
         (unsigned-reg		
	  (inst lwz lo-bits (current-nfp-tn vop)
		(* (1+ (tn-offset stack-temp)) vm:word-bytes)))
	 (unsigned-stack
	  (inst lwz temp (current-nfp-tn vop)
		(* (1+ (tn-offset stack-temp)) vm:word-bytes))
	  (inst stw temp nsp-tn
		(* (tn-offset lo-bits) vm:word-bytes))))))))

#+darwin
(define-vop (move-single-to-int-arg)
  (:args (float :scs (single-reg)))
  (:results (bits :scs (signed-reg signed-stack)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:temporary (:scs (signed-reg)) temp)
  (:arg-types single-float)
  (:result-types signed-num)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (single-reg
       (inst stfs float (current-nfp-tn vop)
	     (* (tn-offset stack-temp) vm:word-bytes))
       (sc-case bits
         (signed-reg		
	  (inst lwz bits (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes)))
	 (signed-stack
	  (inst lwz temp (current-nfp-tn vop)
		(* (tn-offset stack-temp) vm:word-bytes))
	  (inst stw temp nsp-tn
		(* (tn-offset bits) vm:word-bytes))))))))


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
  (:vop-var vop)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc single-reg) fp-temp)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (inst mffs fp-temp)
      (inst stfd fp-temp nfp (* word-bytes (tn-offset temp)))
      (loadw res nfp (1+ (tn-offset temp))))))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:temporary (:sc double-stack) temp)
  (:temporary (:sc single-reg) fp-temp)
  (:vop-var vop)
  (:generator 3
    (let ((nfp (current-nfp-tn vop)))
      (storew new nfp (1+ (tn-offset temp)))
      (inst lfd fp-temp nfp (* word-bytes (tn-offset temp)))
      (inst mtfsf 255 fp-temp)
      (move res new))))


;;;; Complex float VOPs

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r
	       :load-if (not (location= real r)))
	 (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note "inline complex single-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-single-reg
       (let ((r-real (complex-single-reg-real-tn r)))
	 (unless (location= real r-real)
	   (inst fmr r-real real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst fmr r-imag imag))))
      (complex-single-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) vm:word-bytes)))
	 (unless (location= real r)
	   (inst stfs real nfp offset))
	 (inst stfs imag nfp (+ offset vm:word-bytes)))))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r
	       :load-if (not (location= real r)))
	 (imag :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-double-stack))))
  (:result-types complex-double-float)
  (:note "inline complex double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-double-reg
       (let ((r-real (complex-double-reg-real-tn r)))
	 (unless (location= real r-real)
	   (inst fmr r-real real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
	 (unless (location= imag r-imag)
	   (inst fmr r-imag imag))))
      (complex-double-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) vm:word-bytes)))
	 (unless (location= real r)
	   (inst stfd real nfp offset))
	 (inst stfd imag nfp (+ offset (* 2 vm:word-bytes))))))))


(define-vop (complex-single-float-value)
  (:args (x :scs (complex-single-reg) :target r
	    :load-if (not (sc-is x complex-single-stack))))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-single-reg
       (let ((value-tn (ecase slot
			 (:real (complex-single-reg-real-tn x))
			 (:imag (complex-single-reg-imag-tn x)))))
	 (unless (location= value-tn r)
	   (inst fmr r value-tn))))
      (complex-single-stack
       (inst lfs r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 1))
					      (tn-offset x))
					   vm:word-bytes))))))

(define-vop (realpart/complex-single-float complex-single-float-value)
  (:translate realpart)
  (:note "complex single float realpart")
  (:variant :real))

(define-vop (imagpart/complex-single-float complex-single-float-value)
  (:translate imagpart)
  (:note "complex single float imagpart")
  (:variant :imag))

(define-vop (complex-double-float-value)
  (:args (x :scs (complex-double-reg) :target r
	    :load-if (not (sc-is x complex-double-stack))))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-double-reg
       (let ((value-tn (ecase slot
			 (:real (complex-double-reg-real-tn x))
			 (:imag (complex-double-reg-imag-tn x)))))
	 (unless (location= value-tn r)
	   (inst fmr r value-tn))))
      (complex-double-stack
       (inst lfd r (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 2))
					      (tn-offset x))
					   vm:word-bytes))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))


;;; Support for double-double floats

#+double-double
(progn
(deftransform eql ((x y) (double-double-float double-double-float))
  '(and (eql (double-double-hi x) (double-double-hi y))
     	(eql (double-double-lo x) (double-double-lo y))))

(defun double-double-reg-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (tn-offset x)))

(defun double-double-reg-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
		  :offset (1+ (tn-offset x))))

(define-move-function (load-double-double 4) (vop x y)
  ((double-double-stack) (double-double-reg))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset x) vm:word-bytes)))
    (let ((hi-tn (double-double-reg-hi-tn y)))
      (inst lfd hi-tn nfp offset))
    (let ((lo-tn (double-double-reg-lo-tn y)))
      (inst lfd lo-tn nfp (+ offset (* 2 vm:word-bytes))))))

(define-move-function (store-double-double 4) (vop x y)
  ((double-double-reg) (double-double-stack))
  (let ((nfp (current-nfp-tn vop))
	(offset (* (tn-offset y) vm:word-bytes)))
    (let ((hi-tn (double-double-reg-hi-tn x)))
      (inst stfd hi-tn nfp offset))
    (let ((lo-tn (double-double-reg-lo-tn x)))
      (inst stfd lo-tn nfp (+ offset (* 2 vm:word-bytes))))))

;;; Double-double float register to register moves

(define-vop (double-double-move)
  (:args (x :scs (double-double-reg)
	    :target y :load-if (not (location= x y))))
  (:results (y :scs (double-double-reg) :load-if (not (location= x y))))
  (:note "double-double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the double-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-hi (double-double-reg-hi-tn x))
	     (y-hi (double-double-reg-hi-tn y)))
	 (inst fmr y-hi x-hi))
       (let ((x-lo (double-double-reg-lo-tn x))
	     (y-lo (double-double-reg-lo-tn y)))
	 (inst fmr y-lo x-lo)))))
;;;
(define-move-vop double-double-move :move
  (double-double-reg) (double-double-reg))

;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.

(define-vop (move-from-double-double)
  (:args (x :scs (double-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:note "double double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y pa-flag ndescr vm::double-double-float-type
			       vm::double-double-float-size))
     (let ((hi-tn (double-double-reg-hi-tn x)))
       (inst stfd hi-tn y (- (* vm::double-double-float-hi-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))
     (let ((lo-tn (double-double-reg-lo-tn x)))
       (inst stfd lo-tn y (- (* vm::double-double-float-lo-slot
				  vm:word-bytes)
			       vm:other-pointer-type)))))
;;;
(define-move-vop move-from-double-double :move
  (double-double-reg) (descriptor-reg))

;;; Move from a descriptor to a double-double float register

(define-vop (move-to-double-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-double-reg)))
  (:note "pointer to double float coercion")
  (:generator 2
    (let ((hi-tn (double-double-reg-hi-tn y)))
      (inst lfd hi-tn x (- (* double-double-float-hi-slot word-bytes)
			     other-pointer-type)))
    (let ((lo-tn (double-double-reg-lo-tn y)))
      (inst lfd lo-tn x (- (* double-double-float-lo-slot word-bytes)
			   other-pointer-type)))))

(define-move-vop move-to-double-double :move
  (descriptor-reg) (double-double-reg))

;;; double-double float move-argument vop

(define-vop (move-double-double-float-argument)
  (:args (x :scs (double-double-reg) :target y)
	 (nfp :scs (any-reg) :load-if (not (sc-is y double-double-reg))))
  (:results (y))
  (:note "double double-float argument move")
  (:generator 2
    (sc-case y
      (double-double-reg
       (unless (location= x y)
	 (let ((x-hi (double-double-reg-hi-tn x))
	       (y-hi (double-double-reg-hi-tn y)))
	   (inst fmr y-hi x-hi))
	 (let ((x-lo (double-double-reg-lo-tn x))
	       (y-lo (double-double-reg-lo-tn y)))
	   (inst fmr y-lo x-lo))))
      (double-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
	 (let ((hi-tn (double-double-reg-hi-tn x)))
	   (inst stfd hi-tn nfp offset))
	 (let ((lo-tn (double-double-reg-lo-tn x)))
	   (inst stfd lo-tn nfp (+ offset (* 2 word-bytes)))))))))

(define-move-vop move-double-double-float-argument :move-argument
  (double-double-reg descriptor-reg) (double-double-reg))


(define-vop (make/double-double-float)
  (:args (hi :scs (double-reg) :target r
	     :load-if (not (location= hi r)))
	 (lo :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (double-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r double-double-stack))))
  (:result-types double-double-float)
  (:translate kernel::%make-double-double-float)
  (:note "inline double-double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (sc-case r
      (double-double-reg
       (let ((r-hi (double-double-reg-hi-tn r)))
	 (unless (location= hi r-hi)
	   (inst fmr r-hi hi)))
       (let ((r-lo (double-double-reg-lo-tn r)))
	 (unless (location= lo r-lo)
	   (inst fmr r-lo lo))))
      (double-double-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) vm:word-bytes)))
	 (unless (location= hi r)
	   (inst stfd hi nfp offset))
	 (inst stfd lo nfp (+ offset (* 2 vm:word-bytes))))))))

(define-vop (double-double-float-value)
  (:args (x :scs (double-double-reg) :target r
	    :load-if (not (sc-is x double-double-stack))))
  (:arg-types double-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (double-double-reg
       (let ((value-tn (ecase slot
			 (:hi (double-double-reg-hi-tn x))
			 (:lo (double-double-reg-lo-tn x)))))
	 (unless (location= value-tn r)
	   (inst fmr r value-tn))))
      (double-double-stack
       (inst lfd r (current-nfp-tn vop) (* (+ (ecase slot
						(:hi 0)
						(:lo 2))
					      (tn-offset x))
					   vm:word-bytes))))))

(define-vop (hi/double-double-float double-double-float-value)
  (:translate kernel::double-double-hi)
  (:note "double-double float high part")
  (:variant :hi))

(define-vop (lo/double-double-float double-double-float-value)
  (:translate kernel::double-double-lo)
  (:note "double-double float low part")
  (:variant :lo))

(define-vop (make-complex-double-double-float)
  (:translate complex)
  (:args (real :scs (double-double-reg) :target r
	       :load-if (not (location= real r)))
	 (imag :scs (double-double-reg) :to :save))
  (:arg-types double-double-float double-double-float)
  (:results (r :scs (complex-double-double-reg) :from (:argument 0)
	       :load-if (not (sc-is r complex-double-double-stack))))
  (:result-types complex-double-double-float)
  (:note "inline complex double-double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case r
      (complex-double-double-reg
       (let ((r-real (complex-double-double-reg-real-hi-tn r))
	     (real-hi (double-double-reg-hi-tn real)))
	 (inst fmr r-real real-hi))
       (let ((r-real (complex-double-double-reg-real-lo-tn r))
	     (real-lo (double-double-reg-lo-tn real)))
	 (inst fmr r-real real-lo))
       (let ((r-imag (complex-double-double-reg-imag-hi-tn r))
	     (imag-hi (double-double-reg-hi-tn imag)))
	 (inst fmr r-imag imag-hi))
       (let ((r-imag (complex-double-double-reg-imag-lo-tn r))
	     (imag-lo (double-double-reg-lo-tn imag)))
	 (inst fmr r-imag imag-lo)))
      (complex-double-double-stack
       (let ((nfp (current-nfp-tn vop))
	     (offset (* (tn-offset r) vm:word-bytes)))
	 (let ((r-real (complex-double-double-reg-real-hi-tn r)))
	   (inst stfd r-real nfp offset))
	 (let ((r-real (complex-double-double-reg-real-lo-tn r)))
	   (inst stfd r-real nfp (+ offset (* 2 vm:word-bytes))))
	 (let ((r-imag (complex-double-double-reg-imag-hi-tn r)))
	   (inst stfd r-imag nfp (+ offset (* 4 vm:word-bytes))))
	 (let ((r-imag (complex-double-double-reg-imag-lo-tn r)))
	   (inst stfd r-imag nfp (+ offset (* 6 vm:word-bytes)))))))))

(define-vop (complex-double-double-float-value)
  (:args (x :scs (complex-double-double-reg) :target r
	    :load-if (not (sc-is x complex-double-double-stack))))
  (:arg-types complex-double-double-float)
  (:results (r :scs (double-double-reg)))
  (:result-types double-double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (sc-case x
      (complex-double-double-reg
       (let ((value-tn (ecase slot
			 (:real (complex-double-double-reg-real-hi-tn x))
			 (:imag (complex-double-double-reg-imag-hi-tn x))))
	     (r-hi (double-double-reg-hi-tn r)))
	 (unless (location= value-tn r-hi)
	   (inst fmr r-hi value-tn)))
       (let ((value-tn (ecase slot
			 (:real (complex-double-double-reg-real-lo-tn x))
			 (:imag (complex-double-double-reg-imag-lo-tn x))))
	     (r-lo (double-double-reg-lo-tn r)))
	 (unless (location= value-tn r-lo)
	   (inst fmr r-lo value-tn))))
      (complex-double-double-stack
       (let ((r-hi (double-double-reg-hi-tn r)))
	 (inst lfd r-hi (current-nfp-tn vop) (* (+ (ecase slot (:real 0) (:imag 4))
						   (tn-offset x))
						vm:word-bytes)))
       (let ((r-lo (double-double-reg-lo-tn r)))
	 (inst lfd r-lo (current-nfp-tn vop) (* (+ (ecase slot (:real 2) (:imag 6))
						   (tn-offset x))
						vm:word-bytes)))))))

(define-vop (realpart/complex-double-double-float complex-double-double-float-value)
  (:translate realpart)
  (:note "complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-double-float complex-double-double-float-value)
  (:translate imagpart)
  (:note "complex double float imagpart")
  (:variant :imag))

); progn
