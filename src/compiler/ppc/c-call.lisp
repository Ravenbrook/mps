;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/c-call.lisp,v 1.20 2006/01/20 04:38:20 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "PPC")

;;; Return the number of bytes needed for the current non-descriptor
;;; stack frame.  Non-descriptor stack frames must be multiples of 16
;;; bytes under the PPC SVr4 ABI (though the EABI may be less
;;; restrictive).  On linux, two words are reserved for the stack
;;; backlink and saved LR (see SB!VM::NUMBER-STACK-DISPLACEMENT).

(defconstant +stack-alignment-bytes+
  ;; Duh.  PPC Linux (and VxWorks) adhere to the EABI.
  #-darwin 7
  ;; But Darwin doesn't
  #+darwin 15)

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name *backend*)
		 (sc-number-or-lose sc-name *backend*)
		 offset))

(defstruct arg-state
  (gpr-args 0)
  (fpr-args 0)
  ;; SVR4 [a]abi wants two words on stack (callee saved lr, backpointer).
  #-darwin
  (stack-frame-size 2)
  ;; PowerOpen ABI wants 8 words on the stack corresponding to GPR3-10
  ;; in addition to the 6 words of link area (see number-stack-displacement)
  #+darwin
  (stack-frame-size (+ 8 6)))

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (arg-state-gpr-args state)))
    (cond ((< reg-args 8)
	   (setf (arg-state-gpr-args state) (1+ reg-args))
	   (my-make-wired-tn prim-type reg-sc (+ reg-args nl0-offset)))
	  (t
	   (let ((frame-size (arg-state-stack-frame-size state)))
	     (setf (arg-state-stack-frame-size state) (1+ frame-size))
	     (my-make-wired-tn prim-type stack-sc frame-size))))))

(def-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
      (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack)))

(def-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer 'sap-reg 'sap-stack))

;;; If a single-float arg has to go on the stack, it's promoted to
;;; double.  That way, C programs can get subtle rounding errors
;;; when unrelated arguments are introduced.

#-darwin
(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
	   (incf (arg-state-fpr-args state))
	   ; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	  (t
	   (let* ((stack-offset (arg-state-stack-frame-size state)))
	     (if (oddp stack-offset)
	       (incf stack-offset))
	     (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))

#+darwin
(def-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state))
	 (gprs (arg-state-gpr-args state)))
    (cond ((< gprs 8) ; and by implication also (< fprs 13)
	   ;; Corresponding GPR is kept empty for functions with fixed args
	   #+nil
	   (incf (arg-state-gpr-args state))
	   
	   (incf (arg-state-fpr-args state))
	   ;; Assign outgoing FPRs starting at FP1.  See comments
	   ;; below for double-float.
	   (list (my-make-wired-tn 'single-float 'single-reg (1+ fprs))
		 (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)))
	  ((< fprs 13)
	   ;; See comments below for double-float.
	   (incf (arg-state-fpr-args state))
	   (progn
	     (incf (arg-state-stack-frame-size state))
	     (my-make-wired-tn 'single-float 'single-reg (1+ fprs)))
	   #+nil
	   (list (my-make-wired-tn 'single-float 'single-reg (1+ fprs))
		 (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)))
	  (t
	   ;; Pass on stack only
	   (let ((stack-offset (arg-state-stack-frame-size state)))
	     (incf (arg-state-stack-frame-size state))
	     (my-make-wired-tn 'single-float 'single-stack stack-offset))))))

#-darwin
(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let* ((fprs (arg-state-fpr-args state)))
    (cond ((< fprs 8)
	   (incf (arg-state-fpr-args state))
	   ; Assign outgoing FPRs starting at FP1
	   (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	  (t
	   (let* ((stack-offset (arg-state-stack-frame-size state)))
	     (if (oddp stack-offset)
	       (incf stack-offset))
	     (setf (arg-state-stack-frame-size state) (+ stack-offset 2))
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))
	   
#+darwin
(def-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((fprs (arg-state-fpr-args state))
	(gprs (arg-state-gpr-args state)))
    (cond ((< gprs 8) ; and by implication also (< fprs 13)
	   #+nil
	   (progn
	     (incf (arg-state-gpr-args state) 2)
	     (when (> (arg-state-gpr-args state) 8)
	       ;; Spill one word to stack
	       (decf (arg-state-gpr-args state))
	       (incf (arg-state-stack-frame-size state))))
	   (incf (arg-state-fpr-args state))
	   ;; Assign outgoing FPRs starting at FP1.
	   ;;
	   ;; The PowerOpen ABI says float values are stored in float
	   ;; regs.  But if we're calling a varargs function, we also
	   ;; need to put the float into some gprs.  We indicate this
	   ;; to %alien-funcall ir2-convert by making a list of the
	   ;; TNs for the float reg and for the int regs.
	   ;;
	   ;; We really only need this for vararg functions, but we
	   ;; currently don't know that, so we do it always.
	   (list (my-make-wired-tn 'double-float 'double-reg (1+ fprs))
		 (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
		 (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack)))
	  ((< fprs 13)
	   ;; As above, we also need to put the float on the stack.
	   #+nil
	   (progn
	     (incf (arg-state-stack-frame-size state) 2)
	     (incf (arg-state-fpr-args state))
	     (my-make-wired-tn 'double-float 'double-reg (1+ fprs)))
	   (progn
	     (incf (arg-state-fpr-args state))
	     (list (my-make-wired-tn 'double-float 'double-reg (1+ fprs))
		   (int-arg state 'signed-byte-32 'signed-reg 'signed-stack)
		   (int-arg state 'unsigned-byte-32 'unsigned-reg 'unsigned-stack))))
	  (t
	   ;; Pass on stack only
	   (let ((stack-offset (arg-state-stack-frame-size state)))
	     (incf (arg-state-stack-frame-size state) 2)
	     (my-make-wired-tn 'double-float 'double-stack stack-offset))))))

;;; Result state handling

(defstruct result-state
  (num-results 0))

(defun generate-result-reg-offset (state)
  (let ((slot (result-state-num-results state)))
    (incf (result-state-num-results state))
    (ecase slot
      (0 nl0-offset)
      (1 nl1-offset))))

(def-alien-type-method (integer :result-tn) (type state)
  (let ((offset (generate-result-reg-offset state)))
    (if (alien-integer-type-signed type)
        (my-make-wired-tn 'signed-byte-32 'signed-reg offset)
        (my-make-wired-tn 'unsigned-byte-32 'unsigned-reg offset))))


(def-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (my-make-wired-tn 'system-area-pointer 'sap-reg
                    (generate-result-reg-offset state)))

(def-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'single-float 'single-reg 1))

(def-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (my-make-wired-tn 'double-float 'double-reg 1))

(def-alien-type-method (values :result-tn) (type state)
  (when (> (length (alien-values-type-values type)) 2)
    (error "Too many result values from c-call."))
  (mapcar #'(lambda (type)
	      (invoke-alien-type-method :result-tn type state))
	  (alien-values-type-values type)))


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
		     (new-args `(ash ,arg -32))
		     (new-args `(logand ,arg #xffffffff))
		     (if (alien-integer-type-signed type)
			 (new-arg-types (parse-alien-type '(signed 32)))
			 (new-arg-types (parse-alien-type '(unsigned 32))))
		     (new-arg-types (parse-alien-type '(unsigned 32))))
		    (t
		     (new-args arg)
		     (new-arg-types type)))))
	  (cond ((and (alien-integer-type-p result-type)
		      (> (alien::alien-integer-type-bits result-type) 32))
		 (let ((new-result-type
			(let ((alien::*values-type-okay* t))
			  (parse-alien-type
			   (if (alien-integer-type-signed result-type)
			       '(values (signed 32) (unsigned 32))
			       '(values (unsigned 32) (unsigned 32)))))))
		   `(lambda (function type ,@(lambda-vars))
		      (declare (ignore type))
		      (multiple-value-bind (high low)
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
    (inst lr res  (make-fixup (extern-alien-name foreign-symbol) :foreign))))

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
    (inst lr res (make-fixup (extern-alien-name foreign-symbol)
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
    (inst lr addr (make-fixup (extern-alien-name foreign-symbol)
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
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (inst lr temp (make-fixup (extern-alien-name "call_into_c") :foreign))
      (inst mtctr temp)
      (move cfunc function)
      (inst bctrl)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:temporary (:scs (unsigned-reg) :to (:result 0)) temp)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (- (logandc2 (+ amount number-stack-displacement
                                   +stack-alignment-bytes+)
				+stack-alignment-bytes+))))
	(cond ((>= delta (ash -1 16))
	       (inst stwu nsp-tn nsp-tn delta))
	      (t
	       (inst lr temp delta)
	       (inst stwux  nsp-tn nsp-tn temp)))))
    (unless (location= result nsp-tn)
      ;; They are only location= when the result tn was allocated by
      ;; make-call-out-tns above, which takes the number-stack-displacement
      ;; into account itself.
      (inst addi result nsp-tn number-stack-displacement))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount number-stack-displacement
                                +stack-alignment-bytes+)
			     +stack-alignment-bytes+)))
	(cond ((< delta (ash 1 16))
	       (inst addi nsp-tn nsp-tn delta))
	      (t
	       (inst lwz nsp-tn nsp-tn 0)))))))


(export '(make-callback-trampoline callback-accessor-form
	  compatible-function-types-p))

(defun callback-accessor-form (type sp offset)
  (let ((parsed-type (alien::parse-alien-type type)))
    (typecase parsed-type
      (alien::integer-64$
       ;; Get both words of a 64-bit integer and combine together, in
       ;; a big-endian fashion.
       `(let ((hi (alien:deref (sap-alien (sys:sap+ ,sp ,offset)
					  ,(if (alien-integer-type-signed parsed-type)
					       '(* c-call:int)
					       '(* c-call:unsigned-int)))))
	      (lo (alien:deref (sap-alien (sys:sap+ ,sp
						    (+ ,offset vm:word-bytes))
					  (* c-call:unsigned-int)))))
	     (+ (ash hi vm:word-bits) lo)))
      (alien::integer$
       ;; We can access machine integers directly, but we need to get
       ;; the offset right, since the offset we're given is the start
       ;; of the object, and we're a big-endian machine.
       (let ((byte-offset
	      (- vm:word-bytes
		 (ceiling (alien::alien-integer-type-bits parsed-type)
			  vm:byte-bits))))
	 `(deref (sap-alien (sys:sap+ ,sp ,(+ byte-offset offset))
			    (* ,type)))))
      (t
       ;; This should work for everything else.
       `(deref (sap-alien (sys:sap+ ,sp ,offset)
			  (* ,type)))))))

(defun compatible-function-types-p (fun-type1 fun-type2)
  (labels ((type-words (type)
             (ceiling (alien-type-bits type) vm:word-bits))
           (compatible-type-p (type1 type2)
             (let ((float1 (alien-float-type-p type1))
                   (float2 (alien-float-type-p type2)))
               (and (if float1
			float2
			(not float2))
                    (= (type-words type1) (type-words type2))))))
    (let ((args1 (alien-function-type-arg-types fun-type1))
          (args2 (alien-function-type-arg-types fun-type2))
          (ret1 (alien-function-type-result-type fun-type1))
          (ret2 (alien-function-type-result-type fun-type2)))
      (and (= (length args1) (length args2))
           (every #'compatible-type-p args1 args2)
           (compatible-type-p ret1 ret2)))))

(defun make-callback-trampoline (index fn-type)
  (let ((return-type (alien-function-type-result-type fn-type))
	(arg-types (alien::alien-function-type-arg-types fn-type)))
    (make-callback-trampoline-segment index arg-types return-type)))

(defun make-callback-trampoline-segment (index argument-types return-type)
  "Return an sb-assem:segment which calls call-callback with INDEX and
a pointer to the arguments."
  (declare (type (unsigned-byte 16) index)
	   (optimize (debug 3)))
  (flet ((make-gpr (n)
	   (make-random-tn :kind :normal :sc (sc-or-lose 'any-reg) :offset n))
	 (make-fpr (n)
	   (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
			   :offset n))
	 (round-up-16 (n)
	   ;; Round up to a multiple of 16.  Darwin wants that for the
	   ;; stack pointer.
	   (* 16 (ceiling n 16))))

    ;; The "Mach-O Runtime Conventions" document for OS X almost specifies
    ;; the calling convention (it neglects to mention that the linkage area
    ;; is 24 bytes).
    (let* ((segment (make-segment))
	   (save-gprs (mapcar #'make-gpr '(13 24)))
	   
	   (argument-words
	    (mapcar (lambda (arg) (ceiling (alien-type-bits arg) vm:word-bits))
		    argument-types))
	   (linkage-area-size 24))
      (assemble (segment)

	(let ((sp (make-gpr 1)))
	  
	  ;; To save our arguments, we follow the algorithm sketched in the
	  ;; "PowerPC Calling Conventions" section of that document.
	  (let ((words-processed 0)
		(gprs (mapcar #'make-gpr '(3 4 5 6 7 8 9 10)))
		(fprs (mapcar #'make-fpr '(1 2 3 4 5 6 7 8 9 10 11 12 13))))
	    (flet ((handle-arg (type words)
		     (let ((integerp (not (alien-float-type-p type)))
			   (offset (+ (* words-processed vm:word-bytes)
				      linkage-area-size)))
		       (cond
			 (integerp
			  (dotimes (k words)
			    (let ((gpr (pop gprs)))
			      (when gpr
				     (inst stw gpr sp offset)
				     (incf words-processed)
				     (incf offset vm:word-bytes)))))
			 ;; The handling of floats is a little ugly because we
			 ;; hard-code the number of words for single- and
			 ;; double-floats.
			 ((alien-single-float-type-p type)
			  (pop gprs)
			  (let ((fpr (pop fprs)))
			    (inst stfs fpr sp offset))
			  (incf words-processed))
			 ((alien-double-float-type-p type)
			  (setf gprs (cddr gprs))
			  (let ((fpr (pop fprs)))
			    (inst stfd fpr sp offset))
			  (incf words-processed 2))))))
	      (mapc #'handle-arg argument-types argument-words)))

	  ;; The args have been saved to memory.
	  ;;
	  ;; The stack frame is something like this:
	  ;;
	  ;; stack arg n
	  ;; ...
	  ;; stack arg 1
	  ;; stack arg 0
	  ;; save arg 7
	  ;; save arg 6
	  ;; save arg 5
	  ;; save arg 4
	  ;; save arg 3
	  ;; save arg 2
	  ;; save arg 1
	  ;; save arg 0
	  ;; 24 bytes for linkage area
	  ;; -> sp points to the bottom of the linkage area
	  ;;
	  ;; Set aside space for our stack frame.  We need enough room
	  ;; for the callback return area, some space to save the
	  ;; non-volatile (callee-saved) registers, space to save the
	  ;; args for the function we're calling, and the linkage
	  ;; area.  The space is rounded up to a multiple of 16 bytes
	  ;; because the stack should be aligned to a multiple of 16
	  ;; bytes.
	  ;;
	  ;; Our stack frame will look something like this now:
	  ;;
	  ;; Offset    Value
	  ;; 64        Caller's frame (see above)
	  ;; 56/60     return area (1 or 2 words) 
	  ;; 48        filler (unused)
	  ;; 44        save r24
	  ;; 40        save r13
	  ;; 36        save arg 3
	  ;; 32        save arg 2
	  ;; 28        save arg 1
	  ;; 24        save arg 0
	  ;; 0         linkage area (24 bytes)
	  ;;
	  ;;
	  ;; The return area is allocated at the top of the frame.
	  ;; When we call funcall3, the linkage table entry is used,
	  ;; which unconditionally uses r13 and r24.  (See
	  ;; lisp/ppc-arch.c.)  So these need to be saved.  funcall3,
	  ;; which calls call_into_lisp, will take care of saving all
	  ;; the remaining registers that could be used.

	  ;; INDEX is fixnumized, ARGS and RETURN-AREA don't need to
	  ;; be because they're word-aligned.  Kinda gross, but
	  ;; hey....
	  
	  (let* ((return-area-words (ceiling (or (alien-type-bits return-type)
						0)
					    vm:word-bits))
		 (save-words (length save-gprs))
		 (args-size (* 4 vm:word-bytes))
		 (frame-size
		  (round-up-16 (+ linkage-area-size
				  (* return-area-words vm:word-bytes)
				  (* save-words vm:word-bytes)
				  args-size))))
	    (destructuring-bind (r0 arg1 arg2 arg3 arg4)
		(mapcar #'make-gpr '(0 3 4 5 6))
	      ;; Setup the args for the call.  We call
	      ;; funcall3(call-callback, index, arg-pointer,
	      ;; return-area-address)
	      (inst lr arg1 (alien::address-of-call-callback))
	      (inst li arg2 (fixnumize index))
	      (inst addi arg3 sp linkage-area-size)
	      (inst addi arg4 sp (- (* return-area-words vm:word-bytes)))

	      ;; Save sp, setup the frame
	      (inst mflr r0)
	      (inst stw r0 sp (* 2 vm:word-bytes))
	      (inst stwu sp sp (- frame-size))

	      ;; Save the caller-saved registers that the linkage
	      ;; table trampoline clobbers.
	      (let ((save-offset (+ linkage-area-size args-size)))
		(dolist (r save-gprs)
		  (inst stw r sp save-offset)
		  (incf save-offset vm:word-bytes)))
	      
	      ;; Make the call
	      (inst lr r0 (alien::address-of-funcall3))
	      (inst mtlr r0)
	      (inst blrl)

	      (let ((return-offset (- frame-size
				      (* return-area-words vm:word-bytes))))
		(etypecase return-type
		  ((or alien::integer$ alien::pointer$ alien::sap$
		       alien::integer-64$)
		   (loop repeat return-area-words
		         with gprs = (mapcar #'make-gpr '(3 4))
		         for gpr = (pop gprs)
		         for offset from return-offset by vm:word-bytes
			 do (inst lwz gpr sp offset)))
		  (alien::single$
		   ;; Get the FP value into F1
		   (let ((f1 (make-fpr 1)))
		     (inst lfs f1 sp return-offset)))
		  (alien::double$
		   ;; Get the FP value into F1
		   (let ((f1 (make-fpr 1)))
		     (inst lfd f1 sp return-offset)))
		  (alien::void$
		   ;; Nothing to do
		   )))
	      
	      ;; Restore the GPRS we saved.
	      (let ((save-offset (+ linkage-area-size args-size)))
		(dolist (r save-gprs)
		  (inst lwz r sp save-offset)
		  (incf save-offset vm:word-bytes)))

	      ;; All done.  Restore sp and lr and return.
	      (inst lwz r0 sp (+ frame-size (* 2 vm:word-bytes)))
	      (inst mtlr r0)
	      (inst addic sp sp frame-size)
	      
	      ;; And back we go!
	      (inst blr)))))

      (let ((length (finalize-segment segment)))
	(prog1 (alien::segment-to-trampoline segment length)
	  (release-segment segment))))))
