;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/c-call.lisp,v 1.11 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/c-call.lisp,v 1.11 1994/10/31 04:45:41 ram Exp $
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;; Converted by Bill Chiles.
;;;

(in-package "RT")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")



;;;; Make-call-out-tns and support stuff.

(defun c-call-wired-tn (primitive-type sc-name offset)
  (make-wired-tn (primitive-type-or-lose primitive-type)
		 (sc-number-or-lose sc-name)
		 offset))


(defstruct arg-state
  (offset 0))

(defun int-arg (state prim-type stack-sc)
  ;; C expects 4 register args and the 5th arg at the top of the stack.
  ;; We can't put args in registers, because we need those registers
  ;; for something else.  So we put them just beyond the end of the
  ;; stack and the trampoline code will move them into place.
  (let ((frame-size (arg-state-offset state)))
    (setf (arg-state-offset state) (1+ frame-size))
    (c-call-wired-tn prim-type stack-sc frame-size)))

(defun int-result (state prim-type reg-sc)
  (let ((reg-num (arg-state-offset state)))
    (setf (arg-state-offset state) (1+ reg-num))
    (c-call-wired-tn prim-type reg-sc reg-num)))

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 'signed-stack)
      (int-arg state 'unsigned-byte-32 'unsigned-stack)))

(def-alien-type-method (integer :result-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-result state 'signed-byte-32 'signed-reg)
      (int-result state 'unsigned-byte-32 'unsigned-reg)))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-stack))

(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (int-result state 'system-area-pointer 'sap-reg))

(def-alien-type-method (values :result-tn) (type state)
  (mapcar #'(lambda (type)
	      (invoke-alien-type-method :result-tn type state))
	  (alien-values-type-values type)))

(def-vm-support-routine make-call-out-tns (type)
  (declare (type alien-function-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (c-call-wired-tn 'positive-fixnum 'word-pointer-reg nsp-offset)
	      (* (arg-state-offset arg-state) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method
	       :result-tn
	       (alien-function-type-result-type type)
	       (make-arg-state :offset nl0-offset))))))


;;;; Deftransforms to convert uses of %alien-funcall into cononical form.

(deftransform %alien-funcall ((function type &rest args)
			      (t * &rest t))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    (if (some #'alien-double-float-type-p arg-types)
	(collect ((new-args) (lambda-vars) (new-arg-types))
	  (dolist (type arg-types)
	    (let ((arg (gensym)))
	      (lambda-vars arg)
	      (typecase type
		(alien-double-float-type
		 (new-args `(double-float-high-bits ,arg))
		 (new-args `(double-float-low-bits ,arg))
		 (new-arg-types (parse-alien-type '(signed 32)))
		 (new-arg-types (parse-alien-type '(unsigned 32))))
		(t
		 (new-args arg)
		 (new-arg-types type)))))
	  `(lambda (function type ,@(lambda-vars))
	     (declare (ignore type))
	     (%alien-funcall function
			     ',(make-alien-function-type
				:arg-types (new-arg-types)
				:result-type result-type)
			     ,@(new-args))))
	(c::give-up))))

(deftransform %alien-funcall ((function type &rest args)
			      (* t &rest t))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    (flet ((make-arg-name-list ()
	     (mapcar #'(lambda (x) (declare (ignore x)) (gensym))
		     arg-types)))
      (typecase result-type
	(alien-double-float-type
	 (let ((arg-names (make-arg-name-list)))
	   `(lambda (function type ,@arg-names)
	      (declare (ignore type))
	      (multiple-value-bind
		  (hi lo)
		  (%alien-funcall function
				  ',(make-alien-function-type
				     :arg-types arg-types
				     :result-type
				     (let ((*values-type-okay* t))
				       (parse-alien-type
					'(values (signed 32)
						 (unsigned 32)))))
				  ,@arg-names)
	      (make-double-float hi lo)))))
	(alien-single-float-type
	 (let ((arg-names (make-arg-name-list)))
	   `(lambda (function type ,@arg-names)
	      (declare (ignore type))
	      (make-single-float
	       (%alien-funcall function
			       ',(parse-alien-type
				  `(function (signed 32) ,@arg-types))
			       ,@arg-names)))))
	(t
	 (c::give-up))))))


;;;; Vops.


(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst cai res (make-fixup (concatenate 'simple-string "_" foreign-symbol)
			      :foreign))))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target nl0)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset
		   :from (:argument 0) :to (:result 0)) nl0)
  (:temporary (:sc any-reg :offset lra-offset) lra)
  (:temporary (:sc any-reg :offset code-offset) code)
  (:temporary (:scs (sap-reg) :to (:result 0)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((lra-label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (move nl0 function)
      (when cur-nfp
	(store-stack-tn cur-nfp nfp-save))
      (inst compute-lra-from-code lra code lra-label)
      (inst cai temp (make-fixup "call_into_c" :foreign))
      (inst b temp)

      (align vm:lowtag-bits)
      (emit-label lra-label)
      (inst lra-header-word)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (unless (zerop amount)
      (inst cal nsp-tn nsp-tn (- (logandc2 (+ amount 7) 7))))
    (move result nsp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (inst cal nsp-tn  nsp-tn (logandc2 (+ amount 7) 7)))))
