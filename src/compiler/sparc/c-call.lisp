;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/c-call.lisp,v 1.27 2005/11/29 17:02:53 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "SPARC")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (register-args 0)
  ;; No matter what we have to allocate at least 7 stack frame slots.  One
  ;; because the C call convention requries it, and 6 because whoever we call
  ;; is going to expect to be able to save his 6 register arguments there.
  (stack-frame-size 7))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-register-args state)))
    (cond ((< reg-args 6)
	   (setf (arg-state-register-args state) (1+ reg-args))
	   (my-make-wired-tn prim-type reg-sc (+ reg-args nl0-offset)))
	  (t
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc (+ frame-size 16)))))))

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack)))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

(defstruct result-state
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
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

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'double-float 'double-reg 0))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg 0))

#+long-float
(def-alien-type-method (long-float :result-tn) (type)
  (declare (ignore type))
  (my-make-wired-tn 'long-float 'long-reg 0))

(def-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar #'(lambda (type)
		(invoke-alien-type-method :result-tn type state))
	    values)))

(def-vm-support-routine make-call-out-tns (type)
  (declare (type alien-function-type type))
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-function-type-arg-types type))
	(arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg nsp-offset)
	      (* (arg-state-stack-frame-size arg-state) word-bytes)
	      (arg-tns)
	      (invoke-alien-type-method
	       :result-tn
	       (alien-function-type-result-type type)
	       (make-result-state))))))

(deftransform %alien-funcall ((function type &rest args))
  (assert (c::constant-continuation-p type))
  (let* ((type (c::continuation-value type))
	 (arg-types (alien-function-type-arg-types type))
	 (result-type (alien-function-type-result-type type)))
    (assert (= (length arg-types) (length args)))
    ;; We need to do something special for the following argument
    ;; types: single-float, double-float, and 64-bit integers.  For
    ;; results, we need something special for 64-bit integer results.
    (if (or (some #'alien-single-float-type-p arg-types)
	    (some #'alien-double-float-type-p arg-types)
	    (some #'(lambda (type)
		      (and (alien-integer-type-p type)
			   (> (alien::alien-integer-type-bits type) 32)))
		  arg-types)
	    #+long-float (some #'alien-long-float-type-p arg-types)
	    (and (alien-integer-type-p result-type)
		 (> (alien::alien-integer-type-bits result-type) 32)))
	(collect ((new-args) (lambda-vars) (new-arg-types) (mv-vars) (mv-form))
	  (dolist (type arg-types)
	    (let ((arg (gensym)))
	      (lambda-vars arg)
	      (cond ((and (alien-integer-type-p type)
			  (> (alien::alien-integer-type-bits type) 32))
		     ;; 64-bit long long types are stored in
		     ;; consecutive locations, most significant word
		     ;; first (big-endian).
		     (new-args `(ash ,arg -32))
		     (new-args `(logand ,arg #xffffffff))
		     (if (alien-integer-type-signed type)
			 (new-arg-types (parse-alien-type '(signed 32)))
			 (new-arg-types (parse-alien-type '(unsigned 32))))
		     (new-arg-types (parse-alien-type '(unsigned 32))))
		    ((alien-single-float-type-p type)
		     (new-args `(single-float-bits ,arg))
		     (new-arg-types (parse-alien-type '(signed 32))))
		    ((alien-double-float-type-p type)
		     ;; Use double-float-bits instead of
		     ;; double-float-hi/lo-bits to get the bits
		     ;; out.  This gives a some improvement
		     ;; because there's only store of the FP value.
		     ;;
		     ;; Sparc calling conventions say floats must be
		     ;; passed in the integer registers.
		     (let ((mvarg1 (gensym))
			   (mvarg2 (gensym)))
		       (mv-vars `(,mvarg1 ,mvarg2))
		       (mv-form `(double-float-bits ,arg))
		       (new-args mvarg1)
		       (new-args mvarg2)
		       (new-arg-types (parse-alien-type '(signed 32)))
		       (new-arg-types (parse-alien-type '(unsigned 32)))))
		    #+long-float
		    ((alien-long-float-type-p type)
		     (new-args `(long-float-exp-bits ,arg))
		     (new-args `(long-float-high-bits ,arg))
		     (new-args `(long-float-mid-bits ,arg))
		     (new-args `(long-float-low-bits ,arg))
		     (new-arg-types (parse-alien-type '(signed 32)))
		     (new-arg-types (parse-alien-type '(unsigned 32)))
		     (new-arg-types (parse-alien-type '(unsigned 32)))
		     (new-arg-types (parse-alien-type '(unsigned 32))))
		    (t
		     (new-args arg)
		     (new-arg-types type)))))
	  (flet ((mv (vars forms body)
		   ;; Create the set of nested mv-binds
		   (let ((res body))
		     (do ((v (reverse vars) (cdr v))
			  (f (reverse forms) (cdr f)))
			 ((null v))
		       (setf res `(multiple-value-bind ,(car v)
				   ,(car f)
				   ,res)))
		     res)))
	    (cond ((and (alien-integer-type-p result-type)
			(> (alien::alien-integer-type-bits result-type) 32))
		   (let* ((new-result-type
			   (let ((alien::*values-type-okay* t))
			     (parse-alien-type
			      (if (alien-integer-type-signed result-type)
				  '(values (signed 32) (unsigned 32))
				  '(values (unsigned 32) (unsigned 32))))))
			  (body (mv (mv-vars)
				    (mv-form)
				    `(multiple-value-bind (high low)
				      (%alien-funcall function
				       ',(make-alien-function-type
					  :arg-types (new-arg-types)
					  :result-type new-result-type)
				       ,@(new-args))
				      (logior low (ash high 32))))))
		     `(lambda (function type ,@(lambda-vars))
		       (declare (ignore type))
		       ,body)))
		  (t
		   (let ((body (mv (mv-vars)
				   (mv-form)
				   `(%alien-funcall function
				     ',(make-alien-function-type
					:arg-types (new-arg-types)
					:result-type result-type)
				     ,@(new-args)))))
		     `(lambda (function type ,@(lambda-vars))
		       (declare (ignore type))
		       ,body))))))
	(c::give-up))))


#-linkage-table
(define-vop (foreign-symbol-address)
  (:translate foreign-symbol-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (inst li res (make-fixup (extern-alien-name foreign-symbol)
			     :foreign))))

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
    (inst li res (make-fixup (extern-alien-name foreign-symbol)
			     :foreign))))

(define-vop (foreign-symbol-data-address)
  (:translate foreign-symbol-data-address)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:temporary (:scs (non-descriptor-reg)) addr)
  (:generator 2
    (inst li addr (make-fixup (extern-alien-name foreign-symbol)
			      :foreign-data))
    (loadw res addr)))

(define-vop (call-out)
  (:args (function :scs (sap-reg) :target cfunc)
	 (args :more t))
  (:results (results :more t))
  (:ignore args results)
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset
		   :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:temporary (:scs (any-reg) :to (:result 0)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (move cfunc function)
      (inst li temp (make-fixup (extern-alien-name "call_into_c") :foreign))
      (inst jal lip temp)
      (inst nop)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
	(cond ((< delta (ash 1 12))
	       (inst sub nsp-tn delta))
	      (t
	       (inst li temp delta)
	       (inst sub nsp-tn temp)))))
    (unless (location= result nsp-tn)
      ;; They are only location= when the result tn was allocated by
      ;; make-call-out-tns above, which takes the number-stack-displacement
      ;; into account itself.
      (inst add result nsp-tn number-stack-displacement))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 7) 7)))
	(cond ((< delta (ash 1 12))
	       (inst add nsp-tn delta))
	      (t
	       (inst li temp delta)
	       (inst add nsp-tn temp)))))))

;;; Support for callbacks to Lisp.
(export '(make-callback-trampoline callback-accessor-form
	  compatible-function-types-p))

(defun callback-accessor-form (type sp offset)
  (let ((parsed-type (alien::parse-alien-type type)))
    (typecase parsed-type
      (alien::double$
       ;; Due to sparc calling conventions, a double arg doesn't have to
       ;; be aligned on a double word boundary.  We have to get the two
       ;; words separately and create the double from them.  Doubles are
       ;; stored in big-endian order, naturally.
       `(kernel:make-double-float
	 (alien:deref (sap-alien (sys:sap+ ,sp ,offset) (* c-call:int)))
	 (alien:deref (sap-alien (sys:sap+ ,sp (+ ,offset vm:word-bytes))
				 (* c-call:unsigned-int)))))
      (alien::integer-64$
       ;; Same as for double, above
       `(let ((hi (alien:deref (sap-alien (sys:sap+ ,sp ,offset)
					  ,(if (alien-integer-type-signed parsed-type)
					       '(* c-call:int)
					       '(* c-call:unsigned-int)))))
	      (lo (alien:deref (sap-alien (sys:sap+ ,sp
						    (+ ,offset vm:word-bytes))
					  (* c-call:unsigned-int)))))
	  (+ (ash hi vm:word-bits) lo)))
      (alien::integer$
       ;; All other objects can be accessed directly.  But we need to
       ;; get the offset right, since the offset we're given is the
       ;; start of the object, and we're a big-endian machine.
       (let ((byte-offset
	      (- vm:word-bytes
		 (ceiling (alien::alien-integer-type-bits parsed-type)
			  vm:byte-bits))))
	 `(deref (sap-alien (sys:sap+ ,sp ,(+ byte-offset offset))
			    (* ,type)))))
      (t
       `(deref (sap-alien (sys:sap+ ,sp ,offset)
			  (* ,type)))))))

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
  (let ((return-type (alien-function-type-result-type fn-type)))
    (flet ((def-reg-tn (offset)
	     (c:make-random-tn :kind :normal
			       :sc (c:sc-or-lose 'vm::unsigned-reg)
			       :offset offset)))
      (let* ((segment (make-segment))
	     ;; Window save area (16 registers)
	     (window-save-size (* 16 vm:word-bytes))
	     ;; Structure return pointer area (1 register)
	     (struct-return-size vm:word-bytes)
	     ;; Register save area (6 registers)
	     (reg-save-area-size (* 6 vm:word-bytes))
	     ;; Local var.  Should be large enough to hold a double-float or long
	     (return-value-size (* 2 vm:word-bytes))
	     ;; Frame size: the register window, the arg save area, the
	     ;; structure return area, and return-value-area, all
	     ;; rounded to a multiple of eight.
	     (framesize (* 8 (ceiling (+ window-save-size struct-return-size
					 reg-save-area-size
					 return-value-size)
				      8)))
	     ;; Offset from FP where the first arg is located.
	     (arg0-save-offset (+ window-save-size struct-return-size))
	     ;; Establish the registers we need
	     (%g0 (def-reg-tn vm::zero-offset))
	     (%o0 (def-reg-tn vm::nl0-offset))
	     (%o1 (def-reg-tn vm::nl1-offset))
	     (%o2 (def-reg-tn vm::nl2-offset))
	     (%o3 (def-reg-tn vm::nl3-offset))
	     (%o7 (def-reg-tn vm::nargs-offset))
	     (%sp (def-reg-tn vm::nsp-offset)) ; aka %o6
	     (%l0 (def-reg-tn vm::a0-offset))
	     (%i0 (def-reg-tn vm::cname-offset))
	     (%i1 (def-reg-tn vm::lexenv-offset))
	     (%i2 (def-reg-tn 26))
	     (%i3 (def-reg-tn vm::nfp-offset))
	     (%i4 (def-reg-tn vm::cfunc-offset))
	     (%i5 (def-reg-tn vm::code-offset))
	     (%fp (def-reg-tn 30))
	     (%i7 (def-reg-tn vm::lip-offset))
	     (f0-s (c:make-random-tn :kind :normal
				     :sc (c:sc-or-lose 'vm::single-reg)
				     :offset 0))
	     (f0-d (c:make-random-tn :kind :normal
				     :sc (c:sc-or-lose 'vm::double-reg)
				     :offset 0))
	     )
	;; The generated assembly roughly corresponds to this C code:
	;;
	;;        tramp(int a0, int a1, int a2, int a3, int a4, int a5, ...)
	;;	{
	;;	  double result;
	;;	  funcall3(call-callback, <index>, &a0, &result);
	;;	  return <cast> result;
	;;	}
	;;
	;; Except, of course, the result is the appropriate result type
	;; for the trampoline.
	;;	       
	(assemble (segment)
	  ;; Save old %fp, etc. establish our call frame with local vars
	  ;; %i contains the input args
	  (inst save %sp %sp (- framesize))
	  ;; The stack frame now looks like
	  ;;
	  ;; TOP (high memory)
	  ;;
	  ;; argn
	  ;; ...
	  ;; arg6
	  ;; arg5
	  ;; arg4
	  ;; arg3
	  ;; arg2
	  ;; arg1
	  ;; arg0
	  ;; struct_return
	  ;; window-save-area	<- %fp + 64
	  ;;			<- %fp
	  ;; local-vars-extra-args	(8-bytes)
	  ;; arg5-save
	  ;; arg4-save
	  ;; arg3-save
	  ;; arg2-save
	  ;; arg1-save
	  ;; arg0-save
	  ;; struct_return		
	  ;; window-save-area
	  ;;			<- %sp

	  ;; Save all %i arg register values on the stack.  (We
	  ;; might not always need to save all, but this is safe
	  ;; and easy.)
	  (inst st %i0 %fp (+ arg0-save-offset (* 0 vm:word-bytes)))
	  (inst st %i1 %fp (+ arg0-save-offset (* 1 vm:word-bytes)))
	  (inst st %i2 %fp (+ arg0-save-offset (* 2 vm:word-bytes)))
	  (inst st %i3 %fp (+ arg0-save-offset (* 3 vm:word-bytes)))
	  (inst st %i4 %fp (+ arg0-save-offset (* 4 vm:word-bytes)))
	  (inst st %i5 %fp (+ arg0-save-offset (* 5 vm:word-bytes)))

	  ;; Set up our args to call funcall3
	  ;;
	  ;; %o0 = address of call-callback
	  ;; %o1 = index
	  ;; %o2 = pointer to the arguments of the caller (address
	  ;;       of arg0 above)
	  ;; %o3 = pointer to return area

	  (inst li %o0 (alien::address-of-call-callback))
	  (inst li %o1 (ash index vm:fixnum-tag-bits))
	  (inst add %o2 %fp arg0-save-offset)
	  (inst add %o3 %fp (- return-value-size))

	  ;; And away we go to funcall3!
	  (let ((addr (alien::address-of-funcall3)))
	    (inst sethi %l0 (ldb (byte 22 10) addr))
	    (inst jal %o7 %l0 (ldb (byte 10 0) addr))
	    (inst nop))

	  ;; Ok, we're back.  The value returned is actually
	  ;; stored in the return area.  Need to get that into
	  ;; the right registers for return.
	  (etypecase return-type
	    (alien::integer-64$
	     ;; A 64-bit bignum, stored big-endian
	     (inst ld %i0 %fp (- return-value-size))
	     (inst ld %i1 %fp (- (- return-value-size vm:word-bytes))))
	    ((or alien::integer$ alien::pointer$ alien::sap$)
	     (inst ld %i0 %fp (- return-value-size)))
	    (alien::single$
	     ;; Get the FP value into F0
	     (inst ldf f0-s %fp (- return-value-size))
	     )
	    (alien::double$
	     (inst lddf f0-d %fp (- return-value-size)))
	    (alien::void$
	     ))

	  (inst jal %g0 %i7 8)
	  (inst restore %g0 %g0 %g0)
	  )
	(let ((length (finalize-segment segment)))
	  (prog1 (alien::segment-to-trampoline segment length)
	    (release-segment segment)))))))


