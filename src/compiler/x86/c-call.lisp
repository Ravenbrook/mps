;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/x86/c-call.lisp,v 1.16 2004/10/24 16:58:55 rtoy Exp $")
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

(in-package :x86)
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
  (stack-frame-size 0))

(def-alien-type-method (integer :arg-tn) (type state)
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (multiple-value-bind
	(ptype stack-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-stack)
	    (values 'unsigned-byte-32 'unsigned-stack))
      (my-make-wired-tn ptype stack-sc stack-frame-size))))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (my-make-wired-tn 'system-area-pointer
		      'sap-stack
		      stack-frame-size)))

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
    (0 eax-offset)
    (1 edx-offset)))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind
	(ptype reg-sc)
	(if (alien-integer-type-signed type)
	    (values 'signed-byte-32 'signed-reg)
	    (values 'unsigned-byte-32 'unsigned-reg))
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
      (values (my-make-wired-tn 'positive-fixnum 'any-reg esp-offset)
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
   (inst lea res (make-fixup (extern-alien-name foreign-symbol)
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
   (inst mov res (make-fixup (extern-alien-name foreign-symbol)
			     :foreign-data))))

(define-vop (call-out)
  (:args (function :scs (sap-reg))
	 (args :more t))
  (:results (results :more t))
  (:temporary (:sc unsigned-reg :offset eax-offset
		   :from :eval :to :result) eax)
  (:temporary (:sc unsigned-reg :offset ecx-offset
		   :from :eval :to :result) ecx)
  (:temporary (:sc unsigned-reg :offset edx-offset
		   :from :eval :to :result) edx)
  (:node-var node)
  (:vop-var vop)
  (:save-p t)
  (:ignore args ecx edx)
  (:generator 0 
    (cond ((policy node (> space speed))
	   (move eax function)
	   (inst call (make-fixup (extern-alien-name "call_into_c") :foreign)))
	  (t
	   ;; Setup the NPX for C; all the FP registers need to be
	   ;; empty; pop them all.
	   (dotimes (i 8)
	     (fp-pop))

	   (inst call function)
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
  (:node-var node)
  (:generator 0
    (assert (location= result esp-tn))
    ;; Set the rounding precision to 64-bits when calling out to C.
    (when (policy node (= float-accuracy 3))
      (inst sub esp-tn 4)
      (inst fnstcw (make-ea :word :base esp-tn))
      (inst wait)
      (inst or (make-ea :word :base esp-tn) #x300)
      (inst fldcw (make-ea :word :base esp-tn))
      (inst wait))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst sub esp-tn delta)))
    (move result esp-tn)))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:node-var node)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst add esp-tn delta)))
    ;; Reset the rounding precision to 53-bits
    (when (policy node (= float-accuracy 3))
      (inst fnstcw (make-ea :word :base esp-tn))
      (inst wait)
      (inst and (make-ea :word :base esp-tn) #xfeff)
      (inst fldcw (make-ea :word :base esp-tn))
      (inst wait)
      (inst add esp-tn 4))))

(define-vop (alloc-alien-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (assert (not (location= result esp-tn)))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst sub (make-ea :dword
			   :disp (+ nil-value
				    (static-symbol-offset '*alien-stack*)
				    (ash symbol-value-slot word-shift)
				    (- other-pointer-type)))
	      delta)))
    (load-symbol-value result *alien-stack*)))

(define-vop (dealloc-alien-stack-space)
  (:info amount)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
	(inst add (make-ea :dword
			   :disp (+ nil-value
				    (static-symbol-offset '*alien-stack*)
				    (ash symbol-value-slot word-shift)
				    (- other-pointer-type)))
	      delta)))))

;;; Support for callbacks to Lisp.
(export '(make-callback-trampoline callback-accessor-form
	  compatible-function-types-p))

(defun callback-accessor-form (type sp offset)
  `(alien:deref (sap-alien 
		 (sys:sap+ ,sp ,offset)
		 (* ,type))))

(defun compatible-function-types-p (type1 type2)
  (flet ((machine-rep (type)
	   (etypecase type
	     (alien::integer-64$ :dword)
	     ((or alien::integer$ alien::pointer$ alien::sap$) :word)
	     (alien::single$ :single)
	     (alien::double$ :double)
	     (alien::void$ :void))))
    (let ((type1 (alien-function-type-result-type type1))
	  (type2 (alien-function-type-result-type type2)))
      (eq (machine-rep type1) (machine-rep type2)))))

(defun make-callback-trampoline (index fn-type)
  "Cons up a piece of code which calls call-callback with INDEX and a
pointer to the arguments."
  (let* ((return-type (alien-function-type-result-type fn-type))
	 (segment (make-segment))
	 (eax x86::eax-tn)
	 (edx x86::edx-tn)
	 (ebp x86::ebp-tn)
	 (esp x86::esp-tn)
	 ([ebp-8] (x86::make-ea :dword :base ebp :disp -8))
	 ([ebp-4] (x86::make-ea :dword :base ebp :disp -4)))
    (assemble (segment)
	      (inst push ebp)			    ; save old frame pointer
	      (inst mov  ebp esp)		    ; establish new frame
	      (inst mov  eax esp)		    ; 
	      (inst sub  eax 8)		            ; place for result 
	      (inst push eax)			    ; arg2
	      (inst add  eax 16)		    ; arguments  
	      (inst push eax)			    ; arg1
	      (inst push (ash index 2))		    ; arg0
	      (inst push (alien::address-of-call-callback))     ; function
	      (inst mov  eax (alien::address-of-funcall3))
	      (inst call eax)
	      ;; now put the result into the right register
	      (etypecase return-type
		(alien::integer-64$
		 (inst mov eax [ebp-8])
		 (inst mov edx [ebp-4]))
		((or alien::integer$ alien::pointer$ alien::sap$)
		 (inst mov eax [ebp-8]))
		(alien::single$
		 (inst fld  [ebp-8]))
		(alien::double$
		 (inst fldd [ebp-8]))
		(alien::void$ ))
	      (inst mov esp ebp)		   ; discard frame
	      (inst pop ebp)			   ; restore frame pointer
	      (inst ret))
    (let* ((length (finalize-segment segment)))
      (prog1 (alien::segment-to-trampoline segment length)
	(release-segment segment)))))


