;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/amd64/call.lisp,v 1.5 2004/07/27 23:30:41 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition of function call for the AMD64.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997.
;;;
(in-package :amd64)

;;;


;;;; Interfaces to IR2 conversion:

;;; Standard-Argument-Location  --  Interface
;;;
;;;    Return a wired TN describing the N'th full call argument passing
;;; location.
;;;
(def-vm-support-routine standard-argument-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *any-primitive-type* descriptor-reg-sc-number
		     (nth n register-arg-offsets))
      (make-wired-tn *any-primitive-type* control-stack-sc-number n)))


;;; Make-Return-PC-Passing-Location  --  Interface
;;;
;;; Make a passing location TN for a local call return PC.
;;;
;;; Always wire the return PC location to the stack in its standard
;;; location.
;;;
;;; No problems.
;#+nil
(def-vm-support-routine make-return-pc-passing-location (standard)
  (declare (ignore standard))
  (make-wired-tn (primitive-type-or-lose 'system-area-pointer *backend*)
		 sap-stack-sc-number return-pc-save-offset))
;;;
;;; If standard is true, then use the standard (full call) location,
;;; otherwise use any legal location.
;;;
;;; No problems.
#+nil
(def-vm-support-routine make-return-pc-passing-location (standard)
  (let ((ptype (primitive-type-or-lose 'system-area-pointer *backend*)))
    (if standard
	(make-wired-tn ptype sap-stack-sc-number return-pc-save-offset)
	(make-normal-tn ptype))))

;;; Make-Old-FP-Passing-Location  --  Interface
;;;
;;; Similar to Make-Return-PC-Passing-Location, but makes a location to pass
;;; Old-FP in.
;;;
;;; This is wired in both the standard and the local-call
;;; conventions, because we want to be able to assume it's always there.
;;; Besides, the x86 doesn't have enough registers to really make it
;;; profitable to pass it in a register.
;;;
;;; No problems
;#+nil
(def-vm-support-routine make-old-fp-passing-location (standard)
  (declare (ignore standard))
  (make-wired-tn *fixnum-primitive-type* control-stack-sc-number
		 ocfp-save-offset))
;;;
;;; If standard is true, then use the standard (full call) location,
;;; otherwise use any legal location.
;;;
;;; No problems.
#+nil
(def-vm-support-routine make-old-fp-passing-location (standard)
  (if standard
      (make-wired-tn *fixnum-primitive-type* control-stack-sc-number
		     ocfp-save-offset)
      (make-normal-tn *fixnum-primitive-type*)))

;;; Make-Old-FP-Save-Location, Make-Return-PC-Save-Location  --  Interface
;;;
;;; Make the TNs used to hold Old-FP and Return-PC within the current
;;; function.  We treat these specially so that the debugger can find them at a
;;; known location.
;;;
;;; Without using a save-tn - which does not make much sense if it is
;;; wire to the stack? No problems.
(def-vm-support-routine make-old-fp-save-location (env)
  (environment-debug-live-tn (make-wired-tn *fixnum-primitive-type*
					    control-stack-sc-number
					    ocfp-save-offset)
			     env))
;;;
;;; Using a save-tn. No problems.
#+nil
(def-vm-support-routine make-old-fp-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type* control-stack-sc-number
		  ocfp-save-offset)))

;;;
;;; Without using a save-tn - which does not make much sense if it is
;;; wire to the stack? No problems.
(def-vm-support-routine make-return-pc-save-location (env)
  (environment-debug-live-tn
   (make-wired-tn (primitive-type-or-lose 'system-area-pointer *backend*)
		  sap-stack-sc-number return-pc-save-offset)
   env))
;;;
;;; Using a save-tn. No problems.
#+nil
(def-vm-support-routine make-return-pc-save-location (env)
  (let ((ptype (primitive-type-or-lose 'system-area-pointer *backend*)))
    (specify-save-tn
     (environment-debug-live-tn (make-normal-tn ptype) env)
     (make-wired-tn ptype sap-stack-sc-number return-pc-save-offset))))

;;; Make-Argument-Count-Location  --  Interface
;;;
;;;    Make a TN for the standard argument count passing location.  We only
;;; need to make the standard location, since a count is never passed when we
;;; are using non-standard conventions.
;;;
(def-vm-support-routine make-argument-count-location ()
  (make-wired-tn *fixnum-primitive-type* any-reg-sc-number rcx-offset))


;;; MAKE-NFP-TN  --  Interface
;;;
;;;    Make a TN to hold the number-stack frame pointer.  This is allocated
;;; once per component, and is component-live.
;;;
(def-vm-support-routine make-nfp-tn ()
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number))

;;; MAKE-STACK-POINTER-TN ()
;;; 
(def-vm-support-routine make-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

;;; MAKE-NUMBER-STACK-POINTER-TN ()
;;; 
(def-vm-support-routine make-number-stack-pointer-tn ()
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number))

;;; Make-Unknown-Values-Locations  --  Interface
;;;
;;;    Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
;;;
(def-vm-support-routine make-unknown-values-locations ()
  (list (make-stack-pointer-tn)
	(make-normal-tn *fixnum-primitive-type*)))


;;; Select-Component-Format  --  Interface
;;;
;;;    This function is called by the Entry-Analyze phase, allowing
;;; VM-dependent initialization of the IR2-Component structure.  We push
;;; placeholder entries in the Constants to leave room for additional
;;; noise in the code object header.
;;;
;;; For the x86 the first constant is a pointer to a list of fixups,
;;; or nil if the code object has none.
;;;
(def-vm-support-routine select-component-format (component)
  (declare (type component component))
  (dotimes (i (1+ code-constants-offset))
    (vector-push-extend nil
			(ir2-component-constants (component-info component))))
  (undefined-value))


;;;; Frame hackery:

;;; Used for setting up the Old-FP in local call.
;;;
(define-vop (current-fp)
  (:results (val :scs (any-reg control-stack)))
  (:generator 1
    (move val rbp-tn)))

;;; We don't have a seperate NFP, so we don't need to do anything here.
;;;
(define-vop (compute-old-nfp)
  (:results (val))
  (:ignore val)
  (:generator 1
    nil))


(define-vop (xep-allocate-frame)
  (:info start-lab copy-more-arg-follows)
  (:vop-var vop)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (align lowtag-bits)
    (trace-table-entry trace-table-function-prologue)
    (emit-label start-lab)
    ;; Skip space for the function header.
    (inst function-header-word)
    (dotimes (i (1- vm:function-code-offset))
      (inst qword 0))

    ;; The start of the actual code.
    ;; Save the return-pc.
    (popw rbp-tn (- (1+ return-pc-save-offset)))

    ;; If copy-more-arg follows it will allocate the correct stack
    ;; size. The stack is not allocated first here as this may expose
    ;; args on the stack if they take up more space than the frame!
    (unless copy-more-arg-follows
      ;; The args fit within the frame so just allocate the frame.
      (inst lea rsp-tn
	    (make-ea :qword :base rbp-tn
		     :disp (- (* vm:word-bytes
				 (max 3 (sb-allocated-size 'stack)))))))

    (trace-table-entry trace-table-normal)))

;;; This is emitted directly before either a known-call-local, call-local,
;;; or a multiple-call-local.  All it does is allocate stack space for the
;;; callee (who has the same size stack as us).
;;; 
(define-vop (allocate-frame)
  (:results (res :scs (any-reg control-stack))
	    (nfp))
  (:info callee)
  (:ignore nfp callee)
  (:generator 2
    (move res rsp-tn)
    (inst sub rsp-tn (* vm:word-bytes (sb-allocated-size 'stack)))))

;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  We allocate at least 3 slots, because
;;; the XEP noise is going to want to use them before it can extend the stack.
;;;
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg control-stack)))
  (:generator 2
    (move res rsp-tn)
    (inst sub rsp-tn (* (max nargs 3) vm:word-bytes))))



;;; Default-Unknown-Values  --  Internal
;;;
;;;    Emit code needed at the return-point from an unknown-values call for a
;;; fixed number of values.  Values is the head of the TN-Ref list for the
;;; locations that the values are to be received into.  Nvals is the number of
;;; values that are to be received (should equal the length of Values).
;;;
;;;    Move-Temp is a Descriptor-Reg TN used as a temporary.
;;;
;;;    This code exploits the fact that in the unknown-values convention, a
;;; single value return returns at the return PC + 3, whereas a return of other
;;; than one value returns directly at the return PC.
;;;
;;;    If 0 or 1 values are expected, then we just emit an instruction to reset
;;; the SP (which will only be executed when other than 1 value is returned.)
;;;
;;; In the general case we have to do three things:
;;;  -- Default unsupplied register values.  This need only be done when a
;;;     single value is returned, since register values are defaulted by the
;;;     called in the non-single case.
;;;  -- Default unsupplied stack values.  This needs to be done whenever there
;;;     are stack values.
;;;  -- Reset SP.  This must be done whenever other than 1 value is returned,
;;;     regardless of the number of values desired.
;;;
;;;
(defun default-unknown-values (vop values nvals)
  (declare (type (or tn-ref null) values)
	   (type unsigned-byte nvals))
  (cond
   ((<= nvals 1)
    (note-this-location vop :single-value-return)
    (inst mov rsp-tn rbx-tn))
   ((<= nvals register-arg-count)
    (let ((regs-defaulted (gen-label)))
      (note-this-location vop :unknown-return)
      (inst jmp-short regs-defaulted)
      ;; Default the unsupplied registers.
      (let* ((2nd-tn-ref (tn-ref-across values))
	     (2nd-tn (tn-ref-tn 2nd-tn-ref)))
	(inst mov 2nd-tn nil-value)
	(when (> nvals 2)
	  (loop
	    for tn-ref = (tn-ref-across 2nd-tn-ref)
	    then (tn-ref-across tn-ref)
	    for count from 2 below register-arg-count
	    do (inst mov (tn-ref-tn tn-ref) 2nd-tn))))
      (inst mov rbx-tn rsp-tn)
      (emit-label regs-defaulted)
      (inst mov rsp-tn rbx-tn)))
   ((<= nvals 7)
    ;; Number of bytes depends on the relative jump instructions. Best
    ;; case is 31+(n-3)*14, worst case is 35+(n-3)*18.  For nvals=6
    ;; that is 73/89 bytes, and for nvals=7 that is 87/107 bytes which
    ;; is likely better than using the blt below.
    (let ((regs-defaulted (gen-label))
	  (defaulting-done (gen-label))
	  (default-stack-slots (gen-label)))
      (note-this-location vop :unknown-return)
      ;; Branch off to the MV case.
      (inst jmp-short regs-defaulted)
      ;; Do the single value case.
      ;; Default the register args
      (inst mov rax-tn nil-value)
      (do ((i 1 (1+ i))
	   (val (tn-ref-across values) (tn-ref-across val)))
	  ((= i (min nvals register-arg-count)))
	(inst mov (tn-ref-tn val) rax-tn))

      ;; Fake other registers so it looks like we returned with all the
      ;; registers filled in.
      (move rbx-tn rsp-tn)
      (inst push rdx-tn)
      (inst jmp default-stack-slots)
      
      (emit-label regs-defaulted)

      (inst mov rax-tn nil-value)
      (storew rdx-tn rbx-tn -1)
      (collect ((defaults))
	(do ((i register-arg-count (1+ i))
	     (val (do ((i 0 (1+ i))
		       (val values (tn-ref-across val)))
		      ((= i register-arg-count) val))
		  (tn-ref-across val)))
	    ((null val))
	  (let ((default-lab (gen-label))
		(tn (tn-ref-tn val)))
	    (defaults (cons default-lab tn))

	    (inst cmp rcx-tn (fixnumize i))
	    (inst jmp :be default-lab)
	    (loadw rdx-tn rbx-tn (- (1+ i)))
	    (inst mov tn rdx-tn)))
	    
	(emit-label defaulting-done)
	(loadw rdx-tn rbx-tn -1)
	(move rsp-tn rbx-tn)
	    
	(let ((defaults (defaults)))
	  (when defaults
	    (assemble (*elsewhere*)
	      (trace-table-entry trace-table-function-prologue)
	      (emit-label default-stack-slots)
	      (dolist (default defaults)
		(emit-label (car default))
		(inst mov (cdr default) rax-tn))
	      (inst jmp defaulting-done)
	      (trace-table-entry trace-table-normal)))))))
   (t
    ;; 91 bytes for this branch.
    (let ((regs-defaulted (gen-label))
	  (restore-rdi (gen-label))
	  (no-stack-args (gen-label))
	  (default-stack-vals (gen-label))
	  (count-okay (gen-label)))
      (note-this-location vop :unknown-return)
      ;; Branch off to the MV case.
      (inst jmp-short regs-defaulted)

      ;; Default the register args, and set up the stack as if we entered
      ;; the MV return point.
      (inst mov rbx-tn rsp-tn)
      (inst push rdx-tn)
      (inst mov rdi-tn nil-value)
      (inst push rdi-tn)
      (inst mov rsi-tn rdi-tn)
      ;; Compute a pointer to where to put the [defaulted] stack values.
      (emit-label no-stack-args)
      (inst lea rdi-tn
	    (make-ea :qword :base rbp-tn
		     :disp (* (- (1+ register-arg-count)) word-bytes)))
      ;; Load RAX with NIL so we can quickly store it, and set up stuff
      ;; for the loop.
      (inst mov rax-tn nil-value)
      (inst std)
      (inst mov rcx-tn (- nvals register-arg-count))
      ;; Jump into the default loop.
      (inst jmp default-stack-vals)

      ;; The regs are defaulted.  We need to copy any stack arguments,
      ;; and then default the remaining stack arguments.
      (emit-label regs-defaulted)
      ;; Save RDI.
      (storew rdi-tn rbx-tn (- (1+ 1)))
      ;; Compute the number of stack arguments, and if it's zero or less,
      ;; don't copy any stack arguments.
      (inst sub rcx-tn (fixnumize register-arg-count))
      (inst jmp :le no-stack-args)

      ;; Throw away any unwanted args.
      (inst cmp rcx-tn (fixnumize (- nvals register-arg-count)))
      (inst jmp :be count-okay)
      (inst mov rcx-tn (fixnumize (- nvals register-arg-count)))
      (emit-label count-okay)
      ;; Save the number of stack values.
      (inst mov rax-tn rcx-tn)
      ;; Compute a pointer to where the stack args go.
      (inst lea rdi-tn
	    (make-ea :qword :base rbp-tn
		     :disp (* (- (1+ register-arg-count)) word-bytes)))
      ;; Save RSI, and compute a pointer to where the args come from.
      (storew rsi-tn rbx-tn (- (1+ 2)))
      (inst lea rsi-tn
	    (make-ea :qword :base rbx-tn
		     :disp (* (- (1+ register-arg-count)) word-bytes)))
      ;; Do the copy.
      (inst shr rcx-tn 2)		; make word count
      (inst std)
      (inst rep)
      (inst movs :qword)
      ;; Restore RSI.
      (loadw rsi-tn rbx-tn (- (1+ 2)))
      ;; Now we have to default the remaining args.  Find out how many.
      (inst sub rax-tn (fixnumize (- nvals register-arg-count)))
      (inst neg rax-tn)
      ;; If none, then just blow out of here.
      (inst jmp :le restore-rdi)
      (inst mov rcx-tn rax-tn)
      (inst shr rcx-tn 2)	; word count
      ;; Load RAX with NIL for fast storing.
      (inst mov rax-tn nil-value)
      ;; Do the store.
      (emit-label default-stack-vals)
      (inst rep)
      (inst stos rax-tn)
      ;; Restore RDI, and reset the stack.
      (emit-label restore-rdi)
      (loadw rdi-tn rbx-tn (- (1+ 1)))
      (inst mov rsp-tn rbx-tn))))
  (undefined-value))



;;;; Unknown values receiving:

;;; Receive-Unknown-Values  --  Internal
;;;
;;;    Emit code needed at the return point for an unknown-values call for an
;;; arbitrary number of values.
;;;
;;;    We do the single and non-single cases with no shared code: there doesn't
;;; seem to be any potential overlap, and receiving a single value is more
;;; important efficiency-wise.
;;;
;;;    When there is a single value, we just push it on the stack, returning
;;; the old SP and 1.
;;;
;;;    When there is a variable number of values, we move all of the argument
;;; registers onto the stack, and return Args and Nargs.
;;;
;;;    Args and Nargs are TNs wired to the named locations.  We must
;;; explicitly allocate these TNs, since their lifetimes overlap with the
;;; results Start and Count (also, it's nice to be able to target them).
;;;
(defun receive-unknown-values (args nargs start count)
  (declare (type tn args nargs start count))
  (let ((variable-values (gen-label))
	(done (gen-label)))
    (inst jmp-short variable-values)
    (inst nop) ; pad one more byte to make it 3
    
    (inst mov start rsp-tn)
    (inst push (first register-arg-tns))
    (inst mov count (fixnumize 1))
    (inst jmp done)

    (emit-label variable-values)
    ;; dtc: this writes the registers onto the stack even if they are
    ;; not needed, only the number specified in rcx are used and have
    ;; stack allocated to them. No harm is done.
    (loop
      for arg in register-arg-tns
      for i downfrom -1
      do (storew arg args i))
    (move start args)
    (move count nargs)

    (emit-label done))
  (undefined-value))


;;; VOP that can be inherited by unknown values receivers.  The main thing this
;;; handles is allocation of the result temporaries.
;;;
(define-vop (unknown-values-receiver)
  (:temporary (:sc descriptor-reg :offset rbx-offset
		   :from :eval :to (:result 0))
	      values-start)
  (:temporary (:sc any-reg :offset rcx-offset
	       :from :eval :to (:result 1))
	      nvals)
  (:results (start :scs (any-reg control-stack))
	    (count :scs (any-reg control-stack))))


;;;; Local call with unknown values convention return:

;;; Non-TR local call for a fixed number of values passed according to the
;;; unknown values convention.
;;;
;;; FP is the frame pointer in install before doing the call.
;;;
;;; NFP would be the number-stack frame pointer if we had a seperate number
;;; stack.
;;; 
;;; Args are the argument passing locations, which are specified only to
;;; terminate their lifetimes in the caller.
;;;
;;; Values are the return value locations (wired to the standard passing
;;; locations).
;;; Nvals is the number of values received.
;;;
;;; Save is the save info, which we can ignore since saving has been done.
;;;
;;; Target is a continuation pointing to the start of the called function.
;;;
;;;
(define-vop (call-local)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:temporary (:sc any-reg) temp-tn) ; from/to?
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:vop-var vop)
  (:ignore nfp arg-locs args #+nil callee)
  (:generator 5
    (trace-table-entry trace-table-call-site)
    (move rbp-tn fp)

    (let ((ret-tn (callee-return-pc-tn callee)))
      #+nil
      (format t "*call-local ~s; tn-kind ~s; tn-save-tn ~s; its tn-kind ~s~%"
	      ret-tn (c::tn-kind ret-tn) (c::tn-save-tn ret-tn)
	      (c::tn-kind (c::tn-save-tn ret-tn)))

      ;; Is the return-pc on the stack or in a register?
      (sc-case ret-tn
	((sap-stack)
	 #+nil (format t "*call-local: ret-tn on stack; offset=~s~%"
		       (tn-offset ret-tn))
	 ;; Stack
	 (inst mov-imm temp-tn (make-fixup nil :code-object return))
	 (storew temp-tn rbp-tn (- (1+ (tn-offset ret-tn)))))
	((sap-reg)
	 ;; Register
	 (inst lea ret-tn (make-fixup nil :code-object return)))))

    (note-this-location vop :call-site)
    (inst jmp target)
    RETURN
    (default-unknown-values vop values nvals)
    (trace-table-entry trace-table-normal)))

;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:temporary (:sc any-reg) temp-tn) ; from/to?
  (:save-p t)
  (:move-args :local-call)
  (:info save callee target)
  (:ignore args save nfp #+nil callee)
  (:vop-var vop)
  (:generator 20
    (trace-table-entry trace-table-call-site)
    (move rbp-tn fp)

    (let ((ret-tn (callee-return-pc-tn callee)))
      #+nil
      (format t "*multiple-call-local ~s; tn-kind ~s; tn-save-tn ~s; its tn-kind ~s~%"
	      ret-tn (c::tn-kind ret-tn) (c::tn-save-tn ret-tn)
	      (c::tn-kind (c::tn-save-tn ret-tn)))

      ;; Is the return-pc on the stack or in a register?
      (sc-case ret-tn
	((sap-stack)
	 #+nil (format t "*multiple-call-local: ret-tn on stack; offset=~s~%"
		       (tn-offset ret-tn))
	 ;; Stack
	 (inst mov-imm temp-tn (make-fixup nil :code-object return))
	 (storew temp-tn rbp-tn (- (1+ (tn-offset ret-tn)))))
	((sap-reg)
	 ;; Register
	 (inst lea ret-tn (make-fixup nil :code-object return)))))

    (note-this-location vop :call-site)
    (inst jmp target)
    RETURN
    (note-this-location vop :unknown-return)
    (receive-unknown-values values-start nvals start count)
    (trace-table-entry trace-table-normal)))


;;;; Local call with known values return:

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (known-call-local)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:temporary (:sc any-reg) temp-tn) ; from/to?
  (:results (res :more t))
  (:move-args :local-call)
  (:save-p t)
  (:info save callee target)
  (:ignore args res save nfp #+nil callee)
  (:vop-var vop)
  (:generator 5
    (trace-table-entry trace-table-call-site)
    (move rbp-tn fp)

    (let ((ret-tn (callee-return-pc-tn callee)))

      #+nil
      (format t "*known-call-local ~s; tn-kind ~s; tn-save-tn ~s; its tn-kind ~s~%"
	      ret-tn (c::tn-kind ret-tn) (c::tn-save-tn ret-tn)
	      (c::tn-kind (c::tn-save-tn ret-tn)))
      
      ;; Is the return-pc on the stack or in a register?
      (sc-case ret-tn
	((sap-stack)
	 #+nil (format t "*known-call-local: ret-tn on stack; offset=~s~%"
		       (tn-offset ret-tn))
	 ;; Stack
	 ;; need a temp register since we need 64-bit address here
	 (inst mov-imm temp-tn (make-fixup nil :code-object return))
	 (storew temp-tn rbp-tn (- (1+ (tn-offset ret-tn)))))
	((sap-reg)
	 ;; Register
	 (inst lea ret-tn (make-fixup nil :code-object return)))))

    (note-this-location vop :call-site)
    (inst jmp target)
    RETURN
    (note-this-location vop :known-return)
    (trace-table-entry trace-table-normal)))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore FP and CSP and jump to the Return-PC.
;;;
;;; We can assume we know exactly where old-fp and return-pc are because
;;; make-old-fp-save-location and make-return-pc-save-location always
;;; return the same place.
;;;
#+nil
(define-vop (known-return)
  (:args (old-fp)
	 (return-pc :scs (any-reg immediate-stack) :target rpc)
	 (vals :more t))
  (:move-args :known-return)
  (:info val-locs)
  (:temporary (:sc unsigned-reg :from (:argument 1)) rpc)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    ;; Save the return-pc in a register 'cause the frame-pointer is going away.
    ;; Note this not in the usual stack location so we can't use RET
    (move rpc return-pc)
    ;; Restore the stack.
    (move rsp-tn rbp-tn)
    ;; Restore the old fp.  We know OLD-FP is going to be in it's stack
    ;; save slot, which is a different frame that than this one,
    ;; so we don't have to worry about having just cleared
    ;; most of the stack.
    (move rbp-tn old-fp)
    (inst jmp rpc)
    (trace-table-entry trace-table-normal)))

;;; From Douglas Crosher
;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore FP and CSP and jump to the Return-PC.
;;;
;;; The old-fp may be either in a register or on the stack in its
;;; standard save locations - slot 0.
;;;
;;; The return-pc may be in a register or on the stack in any slot.
;;;
(define-vop (known-return)
  (:args (old-fp)
	 (return-pc)
	 (vals :more t))
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)

    #+nil (format t "*known-return: old-fp ~s, tn-kind ~s; ~s ~s~%"
		  old-fp (c::tn-kind old-fp) (c::tn-save-tn old-fp)
		  (c::tn-kind (c::tn-save-tn old-fp)))

    #+nil (format t "*known-return: return-pc ~s, tn-kind ~s; ~s ~s~%"
		  return-pc (c::tn-kind return-pc) (c::tn-save-tn return-pc)
		  (c::tn-kind (c::tn-save-tn return-pc)))
    
    ;; return-pc may be either in a register or on the stack.
    (sc-case return-pc
      ((sap-reg)
       (sc-case old-fp
         ((control-stack)

	  #+nil (format t "*known-return: old-fp ~s on stack; offset=~s~%"
			old-fp (tn-offset old-fp))
	  
	  (cond ((zerop (tn-offset old-fp))
		 ;; Zot all of the stack except for the old-fp.
		 (inst lea rsp-tn (make-ea :qword :base rbp-tn
					   :disp (- (* (1+ ocfp-save-offset)
						       word-bytes))))
		 ;; Restore the old fp from its save location on the stack,
		 ;; and zot the stack.
		 (inst pop rbp-tn))

		(t 
		 (cerror "Continue any-way"
			 "VOP return-local doesn't work if old-fp (in slot %s) is not in slot 0"
			 (tn-offset old-fp)))))
	  
	 ((any-reg descriptor-reg)
	  ;; Zot all the stack.
	  (move rsp-tn rbp-tn)
	  ;; Restore the old-fp.
	  (move rbp-tn old-fp)))

       ;; Return; return-pc is in a register.
       (inst jmp return-pc))

      ((sap-stack)

       #+nil (format t "*known-return: return-pc ~s on stack; offset=~s~%"
		     return-pc (tn-offset return-pc))
       
       ;; Zot all of the stack except for the old-fp and return-pc.
       (inst lea rsp-tn
	     (make-ea :qword :base rbp-tn
		      :disp (- (* (1+ (tn-offset return-pc)) word-bytes))))
       ;; Restore the old fp.  old-fp may be either on the stack in its
       ;; save location or in a register, in either case this restores it.
       (move rbp-tn old-fp)
       ;; The return pops the return address (8 bytes), then we need
       ;; to pop all the slots before the return-pc which includes the
       ;; 8 bytes for the old-fp.
       (inst ret (* (tn-offset return-pc) word-bytes))))

    (trace-table-entry trace-table-normal)))


;;;; Full call:
;;;
;;;    There is something of a cross-product effect with full calls.  Different
;;; versions are used depending on whether we know the number of arguments or
;;; the name of the called function, and whether we want fixed values, unknown
;;; values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on the
;;; stack top and storing stack arguments into that frame.  On entry to the
;;; callee, this partial frame is pointed to by FP.

;;; Define-Full-Call  --  Internal
;;;
;;;    This macro helps in the definition of full call VOPs by avoiding code
;;; replication in defining the cross-product VOPs.
;;;
;;; Name is the name of the VOP to define.
;;; 
;;; Named is true if the first argument is an fdefinition object whose
;;; definition is to be called.
;;;
;;; Return is either :Fixed, :Unknown or :Tail:
;;; -- If :Fixed, then the call is for a fixed number of values, returned in
;;;    the standard passing locations (passed as result operands).
;;; -- If :Unknown, then the result values are pushed on the stack, and the
;;;    result values are specified by the Start and Count as in the
;;;    unknown-values continuation representation.
;;; -- If :Tail, then do a tail-recursive call.  No values are returned.
;;;    The Old-Fp and Return-PC are passed as the second and third arguments.
;;;
;;; In non-tail calls, the pointer to the stack arguments is passed as the last
;;; fixed argument.  If Variable is false, then the passing locations are
;;; passed as a more arg.  Variable is true if there are a variable number of
;;; arguments passed on the stack.  Variable cannot be specified with :Tail
;;; return.  TR variable argument call is implemented separately.
;;;
;;; In tail call with fixed arguments, the passing locations are passed as a
;;; more arg, but there is no new-FP, since the arguments have been set up in
;;; the current frame.
;;;
(defmacro define-full-call (name named return variable)
  (assert (not (and variable (eq return :tail))))
  `(define-vop (,name
		,@(when (eq return :unknown)
		    '(unknown-values-receiver)))
    (:args
     ,@(unless (eq return :tail)
	       '((new-fp :scs (any-reg) :to (:argument 1))))
     
     (fun :scs (descriptor-reg control-stack)
          :target rax :to (:argument 0))
     
     ,@(when (eq return :tail)
	     '((old-fp)
	       (return-pc)))
      
     ,@(unless variable '((args :more t :scs (descriptor-reg)))))

    ,@(when (eq return :fixed)
	    '((:results (values :more t))))
   
    (:save-p ,(if (eq return :tail) :compute-only t))

    ,@(unless (or (eq return :tail) variable)
	      '((:move-args :full-call)))

    (:vop-var vop)
    (:info
     ,@(unless (or variable (eq return :tail)) '(arg-locs))
     ,@(unless variable '(nargs))
     ,@(when (eq return :fixed) '(nvals)))

    (:ignore
     ,@(unless (or variable (eq return :tail)) '(arg-locs))
     ,@(unless variable '(args)))
    
    ;; We pass either the fdefn object (for named call) or the actual
    ;; function object (for unnamed call) in RAX.  With named call,
    ;; closure-tramp will replace it with the real function and invoke
    ;; the real function for closures.  Non-closures do not need this
    ;; value, so don't care what shows up in it.
    (:temporary
     (:sc descriptor-reg :offset rax-offset :from (:argument 0) :to :eval)
     rax)

    ;; linkage table will trash r11 when calling closure-tramp
    (:temporary
     (:sc descriptor-reg :offset r11-offset :from (:argument 0) :to :eval)
     r11)

    ;; We pass the number of arguments in RCX.
    (:temporary (:sc unsigned-reg :offset rcx-offset :to :eval) rcx)

    ;; With variable call, we have to load the register-args out
    ;; of the (new) stack frame before doing the call.  Therefore,
    ;; we have to tell the lifetime stuff that we need to use them.
    ,@(when variable
	    (mapcar #'(lambda (name offset)
			`(:temporary (:sc descriptor-reg
				      :offset ,offset
				      :from (:argument 0)
				      :to :eval)
			  ,name))
		    register-arg-names register-arg-offsets))

    ,@(when (eq return :tail)
	    '((:temporary (:sc unsigned-reg
			   :from (:argument 1) :to (:argument 2)) old-fp-tmp)))

    (:generator ,(+ (if named 5 0)
		    (if variable 19 1)
		    (if (eq return :tail) 0 10)
		    15
		    (if (eq return :unknown) 25 0))
     (trace-table-entry trace-table-call-site)

     ;; This has to be done before the frame pointer is changed!
     ;; rax stores the 'lexical environment' needed for closures
     (move rax fun)
     

     ,@(if variable
	   ;; For variable call, compute the number of arguments and
	   ;; move some of the arguments to registers.
	   (collect ((noise))
		    ;; Compute the number of arguments.
		    (noise '(inst mov rcx new-fp))
		    (noise '(inst sub rcx rsp-tn))
		    (noise '(inst shr rcx 1))
		    ;; Move the necessary args to registers, this
		    ;; moves them all even if they are not all needed.
		    (loop
		     for name in register-arg-names
		     for index downfrom -1
		     do (noise `(loadw ,name new-fp ,index)))
		    (noise))
	   '((if (zerop nargs)
		 (inst xor rcx rcx)
	       (inst mov rcx (fixnumize nargs)))))
     ,@(cond ((eq return :tail)
	      '(;; Python has figured out what frame we should return
		;; to so might as well use that clue. This seems
		;; really important to the implementation of things
		;; like (without-interrupts ...)

		;; dtc; Could be doing a tail call from a
		;; known-local-call etc in which the old-fp or ret-pc
		;; are in regs or in non-standard places. If the
		;; passing location were wired to the stack in
		;; standard locations then these moves will be
		;; un-necessary; this is probably best for the x86.
		(sc-case old-fp
		  ((control-stack)
		   (unless (= ocfp-save-offset (tn-offset old-fp))
		     (format t "** tail-call old-fp not S0~%")
		     (move old-fp-tmp old-fp)
		     (storew old-fp-tmp rbp-tn (- (1+ ocfp-save-offset)))))
		  ((any-reg descriptor-reg)
		     (format t "** tail-call old-fp in reg not S0~%")
		   (storew old-fp rbp-tn (- (1+ ocfp-save-offset)))))

		;; For tail call, we have to push the return-pc so
		;; that it looks like we CALLed despite the fact that
		;; we are going to JMP.
		(inst push return-pc)
		))
	     (t
	      ;; For non-tail call, we have to save our frame pointer
	      ;; and install the new frame pointer.  We can't load stack
	      ;; tns after this point.
	      `(;; Python doesn't seem to allocate a frame here which
		;; doesn't leave room for the ofp/ret stuff.
		
		;; The variable args are on the stack and become the
		;; frame, but there may be <3 args and 3 stack slots
		;; are assumed allocate on the call. So need to ensure
		;; there are at least 3 slots. This hack just adds 3
		;; more.
		,(if variable
		     '(inst sub rsp-tn (* 3 word-bytes)))

		;; Save the fp
		(storew rbp-tn new-fp (- (1+ ocfp-save-offset)))

		(move rbp-tn new-fp)	; NB - now on new stack frame.
		)))
  
     (note-this-location vop :call-site)

     (inst ,(if (eq return :tail) 'jmp 'call)
      (make-ea :qword :base rax
	       :disp ,(if named
			  '(- (* fdefn-raw-addr-slot word-bytes)
			    other-pointer-type)
			  '(- (* closure-function-slot word-bytes)
			    function-pointer-type))))
     ,@(ecase return
	      (:fixed
		 '((default-unknown-values vop values nvals)))
	      (:unknown
	       '((note-this-location vop :unknown-return)
		 (receive-unknown-values values-start nvals start count)))
	      (:tail))
     (trace-table-entry trace-table-normal))))


(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)
(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)

(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)


;;; Defined separately, since needs special code that BLT's the arguments
;;; down.  All the real work is done in the assembly routine.  We just
;;; set things up so that it can find what it needs.
;;;
(define-vop (tail-call-variable)
  (:args (args :scs (any-reg control-stack) :target rsi)
	 (function :scs (descriptor-reg control-stack) :target rax)
	 (old-fp)
	 (ret-addr))
  (:temporary (:sc unsigned-reg :offset rsi-offset :from (:argument 0)) rsi)
  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 1)) rax)
;  (:ignore ret-addr old-fp)
  (:generator 75
    ;; Move these into the passing locations if they are not already there.
    (move rsi args)
    (move rax function)

    ;; The following assumes that the return-pc and old-fp are on the
    ;; stack in their standard save locations - Check this.
    (unless (and (sc-is old-fp control-stack)
		 (= (tn-offset old-fp) ocfp-save-offset))
	    (error "tail-call-variable: ocfp not on stack in standard save location?"))
    (unless (and (sc-is ret-addr sap-stack)
		 (= (tn-offset ret-addr) return-pc-save-offset))
	    (error "tail-call-variable: ret-addr not on stack in standard save location?"))
    

    ;; And jump to the assembly routine.
    (inst jmp (make-fixup 'tail-call-variable :assembly-routine))))


;;;; Unknown values return:

;;; Return a single-value using the Unknown-Values convention.  Specifically,
;;; we jump to clear the stack and jump to return-pc+3. 3 is the number of
;;; bytes for the next instruction (MOV).
;;;
;;; We require old-fp to be in a register, because we want to reset rSP before
;;; restoring rBP.  If old-fp were still on the stack, it could get clobbered
;;; by a signal.
;;; 
;;; pfw--get wired-tn conflicts sometimes if register sc specd for args
;;; having problems targeting args to regs -- using temps instead.
(define-vop (return-single)
  (:args (old-fp)
	 (return-pc)
	 (value))
  (:temporary (:sc unsigned-reg) ofp)
  (:temporary (:sc unsigned-reg) ret)
  (:ignore value)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    (move ret return-pc)
    ;; Clear the control stack
    (move ofp old-fp)
    ;; Adjust the return address for the single value return.
    (inst add ret 3)
    ;; Restore the frame pointer.
    (move rsp-tn rbp-tn)
    (move rbp-tn ofp)
    ;; Out of here.
    (inst jmp ret)))

;;; Do unknown-values return of a fixed (other than 1) number of values.  The
;;; Values are required to be set up in the standard passing locations.  Nvals
;;; is the number of values returned.
;;;
;;; Basically, we just load rCX with the number of values returned and rBX
;;; with a pointer to the values, set rSP to point to the end of the values,
;;; and jump directly to return-pc.
;;;
(define-vop (return)
  (:args (old-fp)
	 (return-pc :to (:eval 1))
	 (values :more t))
  (:ignore values)
  (:info nvals)

  ;; In the case of other than one value, we need these registers to tell
  ;; the caller where they are and how many there are.
  (:temporary (:sc unsigned-reg :offset rbx-offset) rbx)
  (:temporary (:sc unsigned-reg :offset rcx-offset) rcx)

  ;; We need to stretch the lifetime of return-pc past the argument
  ;; registers so that we can default the argument registers without
  ;; trashing return-pc.
  (:temporary (:sc unsigned-reg :offset (first register-arg-offsets)
		   :from :eval) a0)
  (:temporary (:sc unsigned-reg :offset (second register-arg-offsets)
		   :from :eval) a1)
  (:temporary (:sc unsigned-reg :offset (third register-arg-offsets)
		   :from :eval) a2)

  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    ;; Establish the values pointer and values count.
    (move rbx rbp-tn)
    (if (zerop nvals)
	(inst xor rcx rcx) ; smaller
      (inst mov rcx (fixnumize nvals)))
    ;; restore the frame pointer.
    (move rbp-tn old-fp)
    ;; clear as much of the stack as possible, but not past the return
    ;; address.
    (inst lea rsp-tn (make-ea :qword :base rbx
			      :disp (- (* (max nvals 2) word-bytes))))
    ;; pre-default any argument register that need it.
    (when (< nvals register-arg-count)
      (let* ((arg-tns (nthcdr nvals (list a0 a1 a2)))
	     (first (first arg-tns)))
	(inst mov first nil-value)
	(dolist (tn (cdr arg-tns))
	  (inst mov tn first))))
    ;; And away we go.  Except that return-pc is still on the
    ;; stack and we've changed the stack pointer.  So we have to
    ;; tell it to index off of RBX instead of RBP.
    (cond ((zerop nvals)
	   ;; Return popping the return address and the OCFP.
	   (inst ret word-bytes))
	  ((= nvals 1)
	   ;; Return popping the return, leaving 1 slot. Can this
	   ;; happen, or is a single value return handled elsewhere?
	   (inst ret))
	  (t
	   (inst jmp (make-ea :qword :base rbx
			      :disp (- (* (1+ (tn-offset return-pc))
					  word-bytes))))))
    
    (trace-table-entry trace-table-normal)))

;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention.  Otherwise, we
;;; branch off to code that calls an assembly-routine.
;;;
;;; The assembly routine takes the following args:
;;;  rAX -- the return-pc to finally jump to.
;;;  rBX -- pointer to where to put the values.
;;;  rCX -- number of values to find there.
;;;  rSI -- pointer to where to find the values.
;;;
(define-vop (return-multiple)
  (:args (old-fp :to (:eval 1) :target old-fp-temp)
	 (return-pc :target rax)
	 (vals :scs (any-reg) :target rsi)
	 (nvals :scs (any-reg) :target rcx))

  (:temporary (:sc unsigned-reg :offset rax-offset :from (:argument 1)) rax)
  (:temporary (:sc unsigned-reg :offset rsi-offset :from (:argument 2)) rsi)
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 3)) rcx)
  (:temporary (:sc unsigned-reg :offset rbx-offset :from (:eval 0)) rbx)
  (:temporary (:sc descriptor-reg :offset (first register-arg-offsets)
		   :from (:eval 0)) a0)
  (:temporary (:sc unsigned-reg :from (:eval 1)) old-fp-temp)
  (:node-var node)

  (:generator 13
    (trace-table-entry trace-table-function-epilogue)
    ;; Load the return-pc.
    (move rax return-pc)
    (unless (policy node (> space speed))
      ;; Check for the single case.
      (let ((not-single (gen-label)))
	(inst cmp nvals (fixnumize 1))
	(inst jmp :ne not-single)
	
	;; Return with one value.
	(loadw a0 vals -1)
	;; Clear the stack.  We load old-fp into a register before clearing
	;; the stack.
	(move old-fp-temp old-fp)
	(move rsp-tn rbp-tn)
	(move rbp-tn old-fp-temp)
	;; Fix the return-pc to point at the single-value entry point.
	(inst add rax 3)
	;; Out of here.
	(inst jmp rax)
	
	;; Nope, not the single case.  Jump to the assembly routine.
	(emit-label not-single)))
    (move rsi vals)
    (move rcx nvals)
    (move rbx rbp-tn)
    (move rbp-tn old-fp)
    (inst jmp (make-fixup 'return-multiple :assembly-routine))
    (trace-table-entry trace-table-normal)))


;;;; XEP hackery:

;;; We don't need to do anything special for regular functions.
;;;
(define-vop (setup-environment)
  (:info label)
  (:ignore label)
  (:generator 0
    ;; Don't bother doing anything.
    nil))

;;; Get the lexical environment from it's passing location.
;;;
(define-vop (setup-closure-environment)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6
    ;; Get result.
    (move closure rax-tn)))

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments.
;;;
;;; The tricky part is doing this without trashing any of the calling
;;; convention registers that are still needed.  This vop is emitted directly
;;; after the xep-allocate frame.  That means the registers are in use as
;;; follows:
;;;
;;;  RAX -- The lexenv.
;;;  RBX -- Available.
;;;  RCX -- The total number of arguments.
;;;  RDX -- The first arg.
;;;  RDI -- The second arg.
;;;  RSI -- The third arg.
;;;
;;; So basically, we have one register available for our use: RBX.
;;;
;;; What we can do is push the other regs onto the stack, and then restore
;;; their values by looking directly below where we put the more-args.
;;; 
(define-vop (copy-more-arg)
  (:info fixed)
  (:generator 20
    ;; Avoid the copy if there are no more args.
    (cond ((zerop fixed)
	   (inst jrcxz just-alloc-frame))
	  (t
	   (inst cmp rcx-tn (fixnumize fixed))
	   (inst jmp :be just-alloc-frame)))
    
    ;; Allocate the space on the stack.
    ;; stack = rbp - (max 3 frame-size) - (nargs - fixed)
    (inst lea rbx-tn
	  (make-ea :qword :base rbp-tn
		   :disp (* word-bytes
			    (- fixed (max 3 (sb-allocated-size 'stack))))))
    (inst sub rbx-tn rcx-tn)  ; need to do this twice since rcx-tn has only 2 tag bits
    (inst sub rbx-tn rcx-tn)  ; Got the new stack in rbx
    (inst mov rsp-tn rbx-tn)
    
    ;; Now: nargs>=1 && nargs>fixed
    
    ;; Save the original count of args.
    (inst mov rbx-tn rcx-tn)
    
    (cond ((< fixed register-arg-count)
	   ;; We must stop when we run out of stack args, not when we
	   ;; run out of more args.
	   ;; Number to copy = nargs-3
	   (inst sub rcx-tn (fixnumize register-arg-count))
	   ;; Everything of interest in registers.
	   (inst jmp :be do-regs))
	  (t
	   ;; Number to copy = nargs-fixed
	   (inst sub rcx-tn (fixnumize fixed))))
    
    ;; Save rdi and rsi register args.
    (inst push rdi-tn)
    (inst push rsi-tn)
    ;; Okay, we have pushed the register args.  We can trash them
    ;; now.
    
    ;; Initialize dst to be end of stack; skiping the values pushed
    ;; above.
    (inst lea rdi-tn (make-ea :qword :base rsp-tn :disp (* 2 word-bytes)))
    
    ;; Initialize src to be end of args.
    (inst mov rsi-tn rbp-tn)
    (inst sub rsi-tn rbx-tn)
    (inst sub rsi-tn rbx-tn)  ; need to do this twice since rbx-tn has only 2 tag bits
    
    (inst shr rcx-tn 2)	; make word count
    ;; And copy the args.
    (inst cld)				; auto-inc RSI and RDI.
    (inst rep)
    (inst movs :qword)
    
    ;; So now we need to restore RDI and RSI.
    (inst pop rsi-tn)
    (inst pop rdi-tn)
    
    DO-REGS
    
    ;; Restore RCX
    (inst mov rcx-tn rbx-tn)
    
    ;; Here: nargs>=1 && nargs>fixed
    (when (< fixed register-arg-count)
	  ;; Now we have to deposit any more args that showed up in
	  ;; registers.
	  (do ((i fixed))
	      ( nil )
	      ;; Store it relative to rbp
	      (inst mov (make-ea :qword :base rbp-tn
				 :disp (- (* word-bytes
					     (+ 1 (- i fixed)
						(max 3 (sb-allocated-size 'stack))))))
		    (nth i register-arg-tns))
	      
	      (incf i)
	      (when (>= i register-arg-count)
		    (return))
	      
	      ;; Don't deposit any more than there are.
	      (if (zerop i)
		  (inst test rcx-tn rcx-tn)
		(inst cmp rcx-tn (fixnumize i)))
	      (inst jmp :eq done)))

    (inst jmp done)
    
    JUST-ALLOC-FRAME
    (inst lea rsp-tn
	  (make-ea :qword :base rbp-tn
		   :disp (- (* vm:word-bytes
			       (max 3 (sb-allocated-size 'stack))))))
    
    DONE))

;;; More args are stored contiguously on the stack, starting
;;; immediately at the context pointer.  The context pointer is not
;;; typed, so the lowtag is 0.
;;;
(define-vop (more-arg)
  (:translate %more-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :result)
	 (index :scs (any-reg) :target temp))
  (:arg-types * tagged-num)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to :result) temp)
  (:results (value :scs (any-reg descriptor-reg)))
  (:result-types *)
  (:generator 5
    (move temp index)
    (inst neg temp)
    ;; temp is tagged by 2 bits, we need another factor of 2
    (inst mov value (make-ea :qword :base object :index temp :scale 2))))

(define-vop (more-arg-c)
  (:translate %more-arg)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:info index)
  (:arg-types * (:constant (signed-byte 30)))
  (:results (value :scs (any-reg descriptor-reg)))
  (:result-types *)
  (:generator 4
   (inst mov value
	 (make-ea :qword :base object :disp (- (* index word-bytes))))))


;;; Turn more arg (context, count) into a list.
;;;
(define-vop (listify-rest-args)
  (:translate %listify-rest-args)
  (:policy :safe)
  (:args (context :scs (descriptor-reg) :target src)
	 (count :scs (any-reg) :target rcx))
  (:info dynamic-extent)
  (:arg-types * tagged-num (:constant t))
  (:temporary (:sc unsigned-reg :offset rsi-offset :from (:argument 0)) src)
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 1)) rcx)
  (:temporary (:sc unsigned-reg :offset rax-offset) rax)
  (:temporary (:sc unsigned-reg :offset r11-offset) r11)
  (:temporary (:sc unsigned-reg) dst)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 20
    (let ((enter (gen-label))
	  (loop (gen-label))
	  (done (gen-label)))
      (move src context)
      (move rcx count)
      ;; Check to see if there are no arguments, and just return NIL if so.
      (inst mov result nil-value)
      (inst jrcxz done)
      (inst lea dst (make-ea :qword :index rcx :scale 4)) ; 2 words for each arg
      (let ((*enable-pseudo-atomic* (unless dynamic-extent
				      *enable-pseudo-atomic*)))
	(pseudo-atomic
	 (allocation dst dst r11 node dynamic-extent)
	 (inst lea dst (make-ea :byte :base dst :disp list-pointer-type))
	 ;; Convert the count into a raw value, so we can use the LOOP inst.
	 (inst shr rcx 2)
	 ;; Set decrement mode (successive args at lower addresses)
	 (inst std)
	 ;; Set up the result.
	 (move result dst)
	 ;; Jump into the middle of the loop, 'cause that's were we want
	 ;; to start.
	 (inst jmp enter)
	 (emit-label loop)
	 ;; Compute a pointer to the next cons.
	 (inst add dst (* cons-size word-bytes))
	 ;; Store a pointer to this cons in the CDR of the previous cons.
	 (storew dst dst -1 list-pointer-type)
	 (emit-label enter)
	 ;; Grab one value and stash it in the car of this cons.
	 (inst lods rax)
	 (storew rax dst 0 list-pointer-type)
	 ;; Go back for more.
	 (inst loop loop)
	 ;; NIL out the last cons.
	 (storew nil-value dst 1 vm:list-pointer-type)))
      (emit-label done))))

;;; Return the location and size of the more arg glob created by Copy-More-Arg.
;;; Supplied is the total number of arguments supplied (originally passed in
;;; rCX.)  Fixed is the number of non-rest arguments.
;;;
;;; We must duplicate some of the work done by Copy-More-Arg, since at that
;;; time the environment is in a pretty brain-damaged state, preventing this
;;; info from being returned as values.  What we do is compute
;;; supplied - fixed, and return a pointer that many words below the current
;;; stack top.
;;;
(define-vop (more-arg-context)
  (:policy :fast-safe)
  (:translate c::%more-arg-context)
  (:args (supplied :scs (any-reg) :target count))
  (:arg-types positive-fixnum (:constant fixnum))
  (:info fixed)
  (:results (context :scs (descriptor-reg))
	    (count :scs (any-reg)))
  (:result-types t tagged-num)
  (:note "more-arg-context")
  (:generator 5
    (move count supplied)
    ;; SP at this point points at the last arg pushed.
    ;; Point to the first more-arg, not above it.
    (inst lea context (make-ea :qword :base rsp-tn
			       :index count :scale 2 ; count has 2 tag bits
			       :disp (- (* (1+ fixed) vm:word-bytes))))
    (unless (zerop fixed)
      (inst sub count (fixnumize fixed)))))

;;; Signal wrong argument count error if Nargs isn't = to Count.
;;;
(define-vop (verify-argument-count)
  (:policy :fast-safe)
  (:translate c::%verify-argument-count)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t))
  (:info count)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
	   (generate-error-code vop invalid-argument-count-error nargs)))
      (if (zerop count)
	  (inst test nargs nargs)  ; smaller instruction
	(inst cmp nargs (fixnumize count)))
      (inst jmp :ne err-lab))))

;;; Various other error signallers.
;;;
(macrolet ((frob (name error translate &rest args)
	     `(define-vop (,name)
		,@(when translate
		    `((:policy :fast-safe)
		      (:translate ,translate)))
		(:args ,@(mapcar #'(lambda (arg)
				     `(,arg :scs (any-reg descriptor-reg)))
				 args))
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1000
		  (error-call vop ,error ,@args)))))
  (frob argument-count-error invalid-argument-count-error
    c::%argument-count-error nargs)
  (frob type-check-error object-not-type-error c::%type-check-error
    object type)
  (frob layout-invalid-error layout-invalid-error c::%layout-invalid-error
    object layout)
  (frob odd-keyword-arguments-error odd-keyword-arguments-error
    c::%odd-keyword-arguments-error)
  (frob unknown-keyword-argument-error unknown-keyword-argument-error
    c::%unknown-keyword-argument-error key)
  (frob nil-function-returned-error nil-function-returned-error nil fun))
