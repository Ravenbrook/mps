;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/hppa/c-call.lisp,v 1.2 1994/10/31 04:42:45 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "HPPA")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (args 0))

(defstruct (arg-info
	    (:constructor make-arg-info (offset prim-type reg-sc stack-sc)))
  offset
  prim-type
  reg-sc
  stack-sc)

(def-alien-type-method (integer :arg-tn) (type state)
  (let ((args (arg-state-args state)))
    (setf (arg-state-args state) (1+ args))
    (if (alien-integer-type-signed type)
	(make-arg-info args 'signed-byte-32 'signed-reg 'signed-stack)
	(make-arg-info args 'unsigned-byte-32 'unsigned-reg 'unsigned-stack))))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((args (arg-state-args state)))
    (setf (arg-state-args state) (1+ args))
    (make-arg-info args 'system-area-pointer 'sap-reg 'sap-stack)))

(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((args (arg-state-args state)))
    (setf (arg-state-args state) (1+ args))
    (make-arg-info args 'single-float 'single-reg 'single-stack)))

(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((args (logior (1+ (arg-state-args state)) 1)))
    (setf (arg-state-args state) (1+ args))
    (make-arg-info args 'double-float 'double-reg 'double-stack)))

(def-alien-type-method (integer :result-tn) (type)
  (if (alien-integer-type-signed type)
      (my-make-wired-tn 'signed-byte-32 'signed-reg nl4-offset)
      (my-make-wired-tn 'unsigned-byte-32 'unsigned-reg nl4-offset)))
  
(def-alien-type-method (system-area-pointer :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'system-area-pointer 'sap-reg nl4-offset))

(def-alien-type-method (single-float :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'single-float 'single-reg 4))

(def-alien-type-method (double-float :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'double-float 'double-reg 4))

(def-alien-type-method (values :result-tn) (type)
  (let ((values (alien-values-type-values type)))
    (when values
      (assert (null (cdr values)))
      (invoke-alien-type-method :result-tn (car values)))))

(defun make-argument-tns (type)
  (let* ((state (make-arg-state))
	 (args (mapcar #'(lambda (arg-type)
			   (invoke-alien-type-method :arg-tn arg-type state))
		       (alien-function-type-arg-types type)))
	 ;; We need 8 words of cruft, and we need to round up to a multiple
	 ;; of 16 words.
	 (frame-size (logandc2 (+ (arg-state-args state) 8 15) 15)))
    (values
     (mapcar #'(lambda (arg)
		 (declare (type arg-info arg))
		 (let ((offset (arg-info-offset arg))
		       (prim-type (arg-info-prim-type arg)))
		   (cond ((>= offset 4)
			  (my-make-wired-tn prim-type (arg-info-stack-sc arg)
					    (- frame-size offset 8 1)))
			 ((or (eq prim-type 'single-float)
			      (eq prim-type 'double-float))
			  (my-make-wired-tn prim-type (arg-info-reg-sc arg)
					    (+ offset 4)))
			 (t
			  (my-make-wired-tn prim-type (arg-info-reg-sc arg)
					    (- nl0-offset offset))))))
	     args)
     (* frame-size word-bytes))))

(def-vm-support-routine make-call-out-tns (type)
  (declare (type alien-function-type type))
  (multiple-value-bind
      (arg-tns stack-size)
      (make-argument-tns type)
    (values (make-normal-tn *fixnum-primitive-type*)
	    stack-size
	    arg-tns
	    (invoke-alien-type-method
	     :result-tn
	     (alien-function-type-result-type type)))))


(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst li (make-fixup foreign-symbol :foreign) res)))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
		   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:scs (any-reg) :to (:result 0)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (move function cfunc)
      (let ((fixup (make-fixup "call_into_c" :foreign)))
	(inst ldil fixup temp)
	(inst ble fixup c-text-space temp :nullify t))
      (inst nop)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (move nsp-tn result)
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 63) 63)))
	(cond ((< delta (ash 1 10))
	       (inst addi delta nsp-tn nsp-tn))
	      (t
	       (inst li delta temp)
	       (inst add temp nsp-tn nsp-tn)))))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (- (logandc2 (+ amount 63) 63))))
	(cond ((<= (- (ash 1 10)) delta)
	       (inst addi delta nsp-tn nsp-tn))
	      (t
	       (inst li delta temp)
	       (inst add temp nsp-tn nsp-tn)))))))
