;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/amd64/c-call.lisp,v 1.3 2004/07/20 22:39:21 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Debugging and Enhancements by Douglas Crosher 1996,1997,1998,1999.
;;;

(in-package :amd64)
(use-package :alien)
(use-package :alien-internals)

;; The move-argument vop is going to store args on the stack for
;; call-out. These tn's will be used for that. move-arg is normally
;; used for things going down the stack but C wants to have args
;; indexed in the positive direction.

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (register-args 0)
  (stack-frame-size 0))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-register-args state)))
    (cond ((< reg-args 6)
	   (setf (arg-state-register-args state) (1+ reg-args))
	   (my-make-wired-tn prim-type reg-sc (ecase reg-args
						(0 #.rdi-offset)
						(1 #.rsi-offset)
						(2 #.rdx-offset)
						(3 #.rcx-offset)
						(4 #.r8-offset)
						(5 #.r9-offset))))
	  (t
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc frame-size))))))

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-64 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-64 'unsigned-reg 'unsigned-stack)))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

#+long-float
(def-alien-type-method (long-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 3))
    (my-make-wired-tn 'long-float 'long-stack stack-frame-size)))

(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 2))
    (my-make-wired-tn 'double-float 'double-stack stack-frame-size)))

(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (my-make-wired-tn 'single-float 'single-stack stack-frame-size)))

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 rax-offset)
    (1 rdx-offset)))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind
	(ptype reg-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-64 'signed-reg)
	    (values 'unsigned-byte-64 'unsigned-reg))
      (my-make-wired-tn ptype reg-sc (result-reg-offset num-results)))))

(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'system-area-pointer 'sap-reg
		      (result-reg-offset num-results))))

#+long-float
(def-alien-type-method (long-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'long-float 'long-reg (* num-results 2))))

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'double-float 'double-reg (* num-results 2))))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'single-float 'single-reg (* num-results 2))))

(def-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar #'(lambda (type)
		(invoke-alien-type-method :result-tn type state))
	    (alien-values-type-values type))))

(def-vm-support-routine make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg rsp-offset)
	      (* (arg-state-stack-frame-size arg-state) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method :result-tn
					(alien-function-type-result-type type)
					(make-result-state))))))

(deftransform %alien-funcall ((function type &rest args))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    (if (or (some #'(lambda (type)
		      (and (alien-integer-type-p type)
			   (> (alien::alien-integer-type-bits type) 32)))
		  arg-types)
	    (and (alien-integer-type-p result-type)
		 (> (alien::alien-integer-type-bits result-type) 32)))
	(collect ((new-args) (lambda-vars) (new-arg-types))
	  (dolist (type arg-types)
	    (let ((arg (gensym)))
	      (lambda-vars arg)
	      (cond ((and (alien-integer-type-p type)
			  (> (alien::alien-integer-type-bits type) 32))
		     (new-args `(logand ,arg #xffffffff))
		     (new-args `(ash ,arg -32))
		     (new-arg-types (parse-alien-type '(unsigned 32)))
		     (if (alien-integer-type-signed type)
			 (new-arg-types (parse-alien-type '(signed 32)))
			 (new-arg-types (parse-alien-type '(unsigned 32)))))
		    (t
		     (new-args arg)
		     (new-arg-types type)))))
	  (cond ((and (alien-integer-type-p result-type)
		      (> (alien::alien-integer-type-bits result-type) 32))
		 (let ((new-result-type
			(let ((alien::*values-type-okay* t))
			  (parse-alien-type
			   (if (alien-integer-type-signed result-type)
			       '(values (unsigned 32) (signed 32))
			       '(values (unsigned 32) (unsigned 32)))))))
		   `(lambda (function type ,@(lambda-vars))
		      (declare (ignore type))
		      (multiple-value-bind (low high)
			  (%alien-funcall function
					  ',(make-alien-function-type
					     :arg-types (new-arg-types)
					     :result-type new-result-type)
					  ,@(new-args))
			(logior low (ash high 32))))))
		(t
		 `(lambda (function type ,@(lambda-vars))
		    (declare (ignore type))
		    (%alien-funcall function
				    ',(make-alien-function-type
				       :arg-types (new-arg-types)
				       :result-type result-type)
				    ,@(new-args))))))
	(c::give-up))))

(define-vop (foreign-symbol-code-address)
  (:translate #+linkage-table foreign-symbol-code-address
	      #-linkage-table foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
   (inst mov-imm res (make-fixup (extern-alien-name foreign-symbol)
			     :foreign))))

(define-vop (foreign-symbol-data-address)
  (:translate foreign-symbol-data-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
   (inst mov-imm res (make-fixup (extern-alien-name foreign-symbol)
				 :foreign-data))
   (inst mov res (make-ea :qword :base res))))

(define-vop (call-out)
  (:args (function :scs (sap-reg))
	 (args :more t))
  (:results (results :more t))
  ;; c temporary register
  (:temporary (:sc unsigned-reg :offset rax-offset
		   :from :eval :to :result) rax)
  (:temporary (:sc unsigned-reg :offset rbx-offset
		   :from :eval :to :result) rbx)
  ;; c temporary register
  (:temporary (:sc unsigned-reg :offset r10-offset
		   :from :eval :to :result) r10)
  ;; c temporary register
  (:temporary (:sc unsigned-reg :offset r11-offset
		   :from :eval :to :result) r11)
  (:node-var node)
  (:vop-var vop)
  (:save-p t)
  (:ignore args r10 r11)
  (:generator 0 
    (cond ((policy node (> space speed))
	   (move rax function)
	   (inst call (make-fixup (extern-alien-name "call_into_c") :foreign)))
	  (t
	   (move rbx function)
	   ;; With variable arguments, rax passes information about the number of
	   ;; SSE register used. This is unnecessary for normal functions, but
	   ;; I don't know how to distinguish them.
	   (inst xor rax rax)
	   
	   ;; Setup the NPX for C; all the FP registers need to be
	   ;; empty; pop them all.
	   (dotimes (i 8)
	     (fp-pop))

	   ;; C stack must be 16 byte aligned
	   (inst mov r11 #xfffffffffffffff0)
	   (inst and rsp-tn r11)
	   (inst call rbx)
	   ;; To give the debugger a clue. XX not really internal-error?
	   (note-this-location vop :internal-error)

	   ;; Restore the NPX for lisp; insure no regs are empty.
	   (dotimes (i 7)
	     (inst fldz))

	   (if (and results
		    (location= (tn-ref-tn results) fr0-tn))
	       ;; The return result is in fr0.
	       (inst fxch fr7-tn) ; move the result back to fr0
	       (inst fldz)) ; insure no regs are empty
	   ))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (assert (location= result rsp-tn))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst sub rsp-tn delta)))
    (move result rsp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst add rsp-tn delta)))))

(define-vop (alloc-alien-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (assert (not (location= result rsp-tn)))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst mov result (+ nil-value
			    (static-symbol-offset '*alien-stack*)
			    (ash symbol-value-slot word-shift)
			    (- other-pointer-type)))
	(inst sub (make-ea :qword :base result)
	      delta)))
    (load-symbol-value result *alien-stack*)))

(define-vop (dealloc-alien-stack-space)
  (:temporary (:sc any-reg) temp-tn) ; from/to?
  (:info amount)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst mov temp-tn (+ nil-value
			     (static-symbol-offset '*alien-stack*)
			     (ash symbol-value-slot word-shift)
			     (- other-pointer-type)))
	(inst add (make-ea :qword :base temp-tn)
	      delta)))))

;;; Support for callbacks to Lisp.
(export '(make-callback-trampoline callback-accessor-form))

(defun callback-accessor-form (type sp offset)
  `(alien:deref (sap-alien 
		 (sys:sap+ ,sp ,offset)
		 (* ,type))))

(defun make-callback-trampoline (index return-type)
  "Cons up a piece of code which calls call-callback with INDEX and a
pointer to the arguments."
  (let* ((segment (make-segment))
	 (rax amd64::rax-tn)
	 (rdx amd64::rdx-tn)
	 (rbp amd64::rbp-tn)
	 (rsp amd64::rsp-tn)
	 ([rbp-8] (amd64::make-ea :qword :base rbp :disp -8))
	 ([rbp-4] (amd64::make-ea :qword :base rbp :disp -4)))
    (assemble (segment)
	      (inst push rbp)			    ; save old frame pointer
	      (inst mov  rbp rsp)		    ; establish new frame
	      (inst mov  rax rsp)		    ; 
	      (inst sub  rax 8)		            ; place for result 
	      (inst push rax)			    ; arg2
	      (inst add  rax 16)		    ; arguments  
	      (inst push rax)			    ; arg1
	      (inst push (ash index 2))		    ; arg0
	      (inst push (alien::address-of-call-callback))     ; function
	      (inst mov  rax (alien::address-of-funcall3))
	      (inst call rax)
	      ;; now put the result into the right register
	      (etypecase return-type
		(alien::integer-64$
		 (inst mov rax [rbp-8])
		 (inst mov rdx [rbp-4]))
		((or alien::integer$ alien::pointer$ alien::sap$)
		 (inst mov rax [rbp-8]))
		(alien::single$
		 (inst fld  [rbp-8]))
		(alien::double$
		 (inst fldd [rbp-8]))
		(alien::void$ ))
	      (inst mov rsp rbp)		   ; discard frame
	      (inst pop rbp)			   ; restore frame pointer
	      (inst ret))
    (let* ((length (finalize-segment segment)))
      (prog1 (alien::segment-to-trampoline segment length)
	(release-segment segment)))))


