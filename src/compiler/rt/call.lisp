;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/call.lisp,v 1.13 2003/08/03 11:27:47 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition of function call for the IBM RT.
;;;
;;; Written by Rob MacLachlan, William Lott, and Bill Chiles.
;;;

(in-package "RT")



;;;; Interfaces to IR2 conversion:

;;; STANDARD-ARGUMENT-LOCATION -- Interface.
;;;
;;; Return a wired TN describing the N'th full call argument passing location.
;;;
(def-vm-support-routine standard-argument-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *any-primitive-type*
		     register-arg-scn
		     (elt register-arg-offsets n))
      (make-wired-tn *any-primitive-type*
		     control-stack-arg-scn n)))

;;; MAKE-RETURN-PC-PASSING-LOCATION -- Interface.
;;;
;;; Make a passing location TN for a local call return-PC.  If standard is
;;; true, then use the standard (full call) location, otherwise use any legal
;;; location.  Even in the non-standard case, this may be restricted by a
;;; desire to use a subroutine call instruction.
;;;
;;; These are *any-primitive-type* since LRA's are descriptor objects.
;;;
(def-vm-support-routine make-return-pc-passing-location (standard)
  (if standard
      (make-wired-tn *any-primitive-type* register-arg-scn lra-offset)
      (make-restricted-tn *any-primitive-type* register-arg-scn)))

;;; MAKE-OLD-FP-PASSING-LOCATION -- Interface.
;;;
;;; Similar to MAKE-RETURN-PC-PASSING-LOCATION, but makes a location to supply
;;; the old control FP.  This is (obviously) wired in the standard convention,
;;; but is totally unrestricted in non-standard conventions, since we can
;;; always fetch it off of the stack using the arg pointer.
;;;
;;; These are *word-pointer-type* since FP's are word aligned pointers,
;;; so the low tow bits are zero.
;;;
(def-vm-support-routine make-old-fp-passing-location (standard)
  (if standard
      (make-wired-tn *word-pointer-type* immediate-arg-scn ocfp-offset)
      (make-normal-tn *word-pointer-type*)))

;;; MAKE-OLD-FP-SAVE-LOCATION, MAKE-RETURN-PC-SAVE-LOCATION -- Interface.
;;;
;;; Make the TN's used to hold the old control FP and return-PC within the
;;; current function.  We treat these specially so that the debugger can find
;;; them at a known location.
;;;
;;; ### Fix to use specified save TNs...
;;;
;;; See comments for MAKE-RETURN-PC-PASSING-LOCATION and M-O-FP-P-L.
;;;
(def-vm-support-routine make-old-fp-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *word-pointer-type*) env)
   (make-wired-tn *word-pointer-type*
		  control-stack-arg-scn
		  ocfp-save-offset)))
;;;
(def-vm-support-routine make-return-pc-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *any-primitive-type*) env)
   (make-wired-tn *any-primitive-type*
		  control-stack-arg-scn
		  lra-save-offset)))

;;; MAKE-ARGUMENT-COUNT-LOCATION -- Interface.
;;;
;;; Make a TN for the standard argument count passing location.  We only need
;;; to make the standard location, since a count is never passed when we are
;;; using non-standard conventions.
;;;
(def-vm-support-routine make-argument-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))


;;; MAKE-NFP-TN -- Interface.
;;;
;;; Make a TN to hold the number-stack frame pointer.  This is allocated once
;;; per component, and is component-live.
;;;
(def-vm-support-routine make-nfp-tn ()
  (component-live-tn
   (make-wired-tn *word-pointer-type* immediate-arg-scn nfp-offset)))

;;; MAKE-STACK-POINTER-TN -- Interface.
;;;
;;; This is of fixnum type because stack pointers are word aligned on our
;;; byte-addressable machine, so they have fixnum tag bits inherently.  Also,
;;; GC doesn't need to fix these since the stack doesn't move.
;;;
(def-vm-support-routine make-stack-pointer-tn ()
  (make-normal-tn *word-pointer-type*))

;;; MAKE-NUMBER-STACK-POINTER-TN -- Interface.
;;; 
(def-vm-support-routine make-number-stack-pointer-tn ()
  (make-normal-tn *word-pointer-type*))

;;; MAKE-UNKNOWN-VALUES-LOCATIONS -- Interface.
;;;
;;; Return a list of TN's that can be used to represent an unknown-values
;;; continuation within a function.  The first one is a stack pointer TN, and
;;; the second is an argument count TN.
;;;
(def-vm-support-routine make-unknown-values-locations ()
  (list (make-stack-pointer-tn)
	(make-normal-tn *fixnum-primitive-type*)))


;;; SELECT-COMPONENT-FORMAT -- Interface.
;;;
;;; This function is called by the Entry-Analyze phase, allowing VM-dependent
;;; initialization of the IR2-Component structure.  We push placeholder entries
;;; in the Constants to leave room for additional noise in the code object
;;; header.
;;;
(def-vm-support-routine select-component-format (component)
  (declare (type component component))
  (dotimes (i code-constants-offset)
    (vector-push-extend nil
			(ir2-component-constants (component-info component))))
  (undefined-value))



;;;; Frame hackery:

;;; CURRENT-FP -- VOP.
;;;
;;; Used for setting up the OCFP in local call.
;;;
(define-vop (current-fp)
  ;; Stack pointers look like fixnums, and GC doesn't need to fix them.
  (:results (val :scs (word-pointer-reg)))
  (:generator 1
    (move val cfp-tn)))

;;; COMPUTE-OLD-NFP  --  VOP.
;;;
;;; A returner uses this for computing the returnee's NFP for use in
;;; known-values return.  Only works assuming there is no variable size stuff
;;; on the nstack.
;;;
(define-vop (compute-old-nfp)
  (:results (val :scs (word-pointer-reg)
		 :load-if (current-nfp-tn vop)))
  (:vop-var vop)
  (:generator 1
    (let ((nfp (current-nfp-tn vop)))
      ;; We know nfp is in a register.
      (when nfp
	;; The number stack grows down in memory, so the old-nfp is greater
	;; than the current one -- add a positive number.
	(inst cal val nfp
	      (component-non-descriptor-stack-usage))))))

;;; XEP-ALLOCATE-FRAME -- VOP.
;;;
;;; This is the first VOP invoked by the compiler at every entry point for a
;;; component.  It sets up to be dual-word aligned (because of our low-tag
;;; bits), emits the header-word for the function data-block, and emits any
;;; extra words for data about the function (such as debug-info, its name,
;;; etc.).  Then it emits instructions to allocate whatever room the function
;;; will need on the control stack.
;;;
(define-vop (xep-allocate-frame)
  (:info start-lab)
  (:vop-var vop)
  (:generator 1
    ;; Make sure the label is aligned.
    (align vm:lowtag-bits)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst function-header-word)
    (dotimes (i (1- vm:function-header-code-offset))
      (inst word 0))
    ;; The start of the actual code.  A pointer to here is stored in symbols
    ;; for named call.  Make CODE point to the component from this pointer.
    (let ((entry-point (gen-label)))
      (emit-label entry-point)
      (inst compute-code-from-fn code-tn lip-tn entry-point))
    ;; Caller has set FP to the beginning of the callee's frame.
    (inst cal csp-tn cfp-tn
	  (* vm:word-bytes (sb-allocated-size 'control-stack)))
    (let ((nfp-tn (current-nfp-tn vop)))
      (when nfp-tn
	(inst cal nsp-tn nsp-tn
	      (- (component-non-descriptor-stack-usage)))
	(move nfp-tn nsp-tn)))))

;;; ALLOCATE-FRAME -- VOP.
;;;
;;; The compiler invokes this in local call for the caller.
;;;
(define-vop (allocate-frame)
  (:results (res :scs (word-pointer-reg))
	    (nfp :scs (word-pointer-reg)
		 :load-if (ir2-environment-number-stack-p callee)))
  (:info callee)
  (:generator 2
    (move res csp-tn)
    (inst cal csp-tn csp-tn
	  (* vm:word-bytes (sb-allocated-size 'control-stack)))
    (when (ir2-environment-number-stack-p callee)
      (inst cal nsp-tn nsp-tn
	    (- (component-non-descriptor-stack-usage)))
      (move nfp nsp-tn))))

;;; ALLOCATE-FULL-CALL-FRAME -- VOP.
;;;
;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  If no stack arguments are passed, then
;;; we don't have to do anything.  The compiler only invokes this for the
;;; caller.
;;;
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (word-pointer-reg)
		 :load-if (> nargs register-arg-count)))
  (:generator 2
    (when (> nargs register-arg-count)
      (move res csp-tn)
      (inst cal csp-tn csp-tn (* nargs vm:word-bytes)))))




;;; DEFAULT-UNKNOWN-VALUES  --  Internal.
;;;
;;; Emit code needed at the return-point from an unknown-values call to get or
;;; default a desired, fixed number of values.  Values is the head of the
;;; TN-Ref list for the locations that the values are to be received into.
;;; Nvals is the number of values that are to be received (should equal the
;;; length of Values).
;;;
;;; Move-Temp is a Descriptor-Reg TNs used as a temporary.
;;;
;;; This code exploits the fact that in the unknown-values convention, a single
;;; value return returns at the return PC + 4, whereas a return of other than
;;; one value returns directly at the return PC.
;;;
;;; If 0 or 1 values are expected, then we just emit an instruction to reset
;;; the SP (which will only be executed when other than 1 value is returned.)
;;;
;;; In the general case, we have to do three things:
;;;  -- Default unsupplied register values.  This need only be done when a
;;;     single value is returned, since register values are defaulted by the
;;;     called in the non-single case.
;;;  -- Default unsupplied stack values.  This needs to be done whenever there
;;;     are stack values.
;;;  -- Reset SP.  This must be done whenever other than 1 value is returned,
;;;     regardless of the number of values desired.
;;;
;;; The general-case code looks like this:
#|
	br regs-defaulted		; Skip if MVs
	cau a1 0 nil-16			; Default register values
	...
	loadi nargs 1			; Force defaulting of stack values
	lr args sp			; Set up args for SP resetting

regs-defaulted
	cau nil-temp 0 nil-16		; Cache nil

	cmpi nargs 3			; If 4'th value unsupplied...
	blex default-value-4		;    jump to default code
	loadw move-temp ocfp-tn 3	; Move value to correct location.
	store-stack-tn val4-tn move-temp

	cmpi nargs 4			; Check 5'th value, etc.
	blex default-value-5
	loadw move-temp ocfp-tn 4
	store-stack-tn val5-tn move-temp

	...

defaulting-done
	lr sp args			; Reset SP.
<end of code>

<elsewhere>
default-value-4 
	store-stack-tn val4-tn nil-temp ; Nil out 4'th value.

default-value-5
	store-stack-tn val5-tn nil-temp ; Nil out 5'th value.

	...

	br defaulting-done
|#
;;;
(defun default-unknown-values (values nvals move-temp lra-label)
  (declare (type (or tn-ref null) values)
	   (type unsigned-byte nvals) (type tn move-temp))
  (cond
   ((<= nvals 1)
    ;; Don't use MOVE.  Use a known 32-bit long instruction, so the returner
    ;; can know how many bytes we used here in the multiple-value return case.
    ;; The returner wants to add a known quantity to LRA indicating how many
    ;; values it returned.
    (inst cal csp-tn ocfp-tn 0)
    (inst compute-code-from-lra code-tn code-tn lra-label))
   (t
    (let ((regs-defaulted (gen-label))
	  (defaulting-done (gen-label))
	  (default-stack-vars (gen-label)))
      ;; Branch off to the MV case.
      ;; The returner has setup value registers and NARGS, and OCFP points to
      ;; any stack values.
      ;; Use a known 32-bit long instruction, so the returner can know how many
      ;; bytes we used here in the multiple-value return case.  The returner
      ;; wants to add a known quantity to LRA indicating how many values it
      ;; returned.
      (inst bnb :pz regs-defaulted)
      ;;
      ;; Do the single value case.
      ;; Fill in some n-1 registers with nil to get to a consistent state with
      ;; having gotten multiple values, so the code after regs-defaulted can
      ;; be the same for both cases.
      (do ((i 1 (1+ i))
	   (val (tn-ref-across values) (tn-ref-across val)))
	  ((= i (min nvals register-arg-count)))
	(move (tn-ref-tn val) null-tn))
      ;; Set OCFP to CSP and (maybe) jump to the code that defaults (i.e.
      ;; NILs out) all the values that would come from the stack.  We have
      ;; to set OCFP because the stack defaulting stuff is going to set CSP
      ;; to OCFP when it gets done to clear any values off the stack, and we
      ;; don't want that to trash CSP.
      (when (> nvals register-arg-count)
	(inst bx default-stack-vars))
      (move ocfp-tn csp-tn)
      
      (emit-label regs-defaulted)
      
      (when (> nvals register-arg-count)
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
	      (inst c nargs-tn (fixnumize i))
	      (inst bnbx :gt default-lab)
	      (loadw move-temp ocfp-tn i)
	      (store-stack-tn move-temp tn)))

	  (emit-label defaulting-done)

	  (assemble (*elsewhere*)
	    (emit-label default-stack-vars)
	    (dolist (def (defaults))
	      (emit-label (car def))
	      (store-stack-tn null-tn (cdr def)))
	    (inst b defaulting-done))))

      (inst compute-code-from-lra code-tn code-tn lra-label)
      (move csp-tn ocfp-tn))))
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
(defun receive-unknown-values (args nargs start count lra-label)
  (declare (type tn args nargs start count))
  (let ((variable-values (gen-label))
	(done (gen-label)))
    ;; Use a known 32-bit long instruction, so the returner can know how many
    ;; bytes we used here in the multiple-value return case.  The returner
    ;; wants to add a known quantity to LRA indicating how many values it
    ;; returned.
    (inst bnb :pz variable-values)
    ;; Here's the return point for the single-value return.
    (inst compute-code-from-lra code-tn code-tn lra-label)
    (inst inc csp-tn vm:word-bytes)
    (storew (first register-arg-tns) csp-tn -1)
    (inst cal start csp-tn -4)
    (inst li count (fixnumize 1))
    
    (emit-label done)
    
    (assemble (*elsewhere*)
      (emit-label variable-values)
      (inst compute-code-from-lra code-tn code-tn lra-label)
      (do ((arg register-arg-tns (rest arg))
	   (i 0 (1+ i)))
	  ((null arg))
	(storew (first arg) args i))
      (move start args)
      (move count nargs)
      (inst b done)))
  (undefined-value))


;;; UNKNOWN-VALUES-RECEIVER -- VOP.
;;;
;;; This is inherited by unknown values receivers.  The main thing this handles
;;; is allocation of the result temporaries.  This has to be included for in
;;; any VOP using RECEIVE-UNKNOWN-VALUES.
;;;
(define-vop (unknown-values-receiver)
  (:results
   (start :scs (word-pointer-reg))
   (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :offset ocfp-offset
		   :from :eval :to (:result 0))
	      values-start)
  (:temporary (:sc any-reg :offset nargs-offset
	       :from :eval :to (:result 1))
	      nvals))



;;;; Local call with unknown values convention return:

;;; Non-TR local call for a fixed number of values passed according to the
;;; unknown values convention.
;;;
;;; Args are the argument passing locations, which are specified only to
;;; terminate their lifetimes in the caller.
;;;
;;; Values are the return value locations (wired to the standard passing
;;; locations).
;;;
;;; Save is the save info, which we can ignore since saving has been done.
;;; Return-PC is the TN that the return PC should be passed in.
;;; Target is a continuation pointing to the start of the called function.
;;; Nvals is the number of values received.
;;;
(define-vop (call-local)
  (:args (fp :scs (word-pointer-reg control-stack))
	 (nfp :scs (word-pointer-reg control-stack))
	 (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:ignore arg-locs args nfp)
  (:vop-var vop)
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:generator 5
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn cur-nfp nfp-save))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label)
      (inst b target)
      (emit-return-pc label)
      (note-this-location vop :unknown-return)
      (default-unknown-values values nvals move-temp label)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (fp :scs (word-pointer-reg control-stack))
	 (nfp :scs (word-pointer-reg control-stack))
	 (args :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info save callee target)
  (:ignore args save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:generator 20
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn cur-nfp nfp-save))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label)
      (inst b target)
      (emit-return-pc label)
      (note-this-location vop :unknown-return)
      (receive-unknown-values values-start nvals start count label)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


;;;; Local call with known values return:

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
(define-vop (known-call-local)
  (:args (fp :scs (word-pointer-reg control-stack))
	 (nfp :scs (word-pointer-reg control-stack))
	 (args :more t))
  (:results (res :more t))
  (:move-args :local-call)
  (:save-p t)
  (:info save callee target)
  (:ignore args res save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:generator 5
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn cur-nfp nfp-save))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (maybe-load-stack-tn callee-nfp nfp)))
      (maybe-load-stack-tn cfp-tn fp)
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label)
      (inst b target)
      (emit-return-pc label)
      (note-this-location vop :known-return)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore FP and CSP and jump to the Return-PC.
;;;
(define-vop (known-return)
  (:args (old-fp :scs (word-pointer-reg control-stack))
	 (return-pc-arg :scs (descriptor-reg control-stack)
			:target return-pc)
	 (vals :more t))
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:temporary (:sc descriptor-reg :from (:argument 1)) return-pc)
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (move csp-tn cfp-tn)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst cal nsp-tn cur-nfp
	      (component-non-descriptor-stack-usage))))
    (maybe-load-stack-tn return-pc return-pc-arg)
    ;; Skip over a word, the LRA header, and subtract out low-tag bits.
    (inst cal lip return-pc (- vm:word-bytes vm:other-pointer-type))
    (inst bx lip)
    (maybe-load-stack-tn cfp-tn old-fp)))



;;;; Full call:
;;;
;;;    There is something of a cross-product effect with full calls.  Different
;;; versions are used depending on whether we know the number of arguments or
;;; the name of the called function, and whether we want fixed values, unknown
;;; values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on the
;;; stack top and storing stack arguments into that frame.  On entry to the
;;; callee, this partial frame is pointed to by FP.  If there are no stack
;;; arguments, we don't bother allocating a partial frame, and instead set FP
;;; to SP just before the call.

;;; Define-Full-Call  --  Internal.
;;;
;;;    This macro helps in the definition of full call VOPs by avoiding code
;;; replication in defining the cross-product VOPs.
;;;
;;; Name is the name of the VOP to define.
;;; 
;;; Named is true if the first argument is a symbol whose global function
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
;;; When variable is false, the compiler actually has already invoked VOP's to
;;; explicitly move arguments into their passing locations.  Hence the VOP
;;; argument args (which is :more t) is actually ignored.  We pass it into the
;;; call VOP's for lifetime analysis, and the TN references it represents stand
;;; in at the call site for the references to those TN's where the callee
;;; actually reads them.
;;;
;;; In tail call with fixed arguments, the passing locations are passed as a
;;; more arg, but there is no new-FP, since the arguments have been set up in
;;; the current frame.
;;;
(eval-when (compile eval)
(defmacro define-full-call (name named return variable)
  (assert (not (and variable (eq return :tail))))
  `(define-vop (,name
		,@(when (eq return :unknown)
		    '(unknown-values-receiver)))
     (:args
      ,@(unless (eq return :tail)
	  '((new-fp :scs (word-pointer-reg) :to :eval)))

      ,(if named
	   '(name :scs (descriptor-reg) :target name-pass)
	   '(arg-fun :scs (descriptor-reg) :target lexenv))
      
      ,@(when (eq return :tail)
	  '((old-fp :scs (word-pointer-reg) :target old-fp-pass)
	    (return-pc :scs (descriptor-reg control-stack)
		       :target return-pc-pass)))
      
      ,@(unless variable '((args :more t :scs (descriptor-reg)))))

     ,@(when (eq return :fixed)
	 '((:results (values :more t))))
   
     ,@(unless (eq return :tail)
	 `((:save-p t)
	   ,@(unless variable
	       '((:move-args :full-call)))))

     (:vop-var vop)
     (:info ,@(unless (or variable (eq return :tail)) '(arg-locs))
	    ,@(unless variable '(nargs))
	    ,@(when (eq return :fixed) '(nvals)))

     (:ignore
      ,@(unless (or variable (eq return :tail)) '(arg-locs))
      ,@(unless variable '(args)))

     (:temporary (:sc descriptor-reg
		  :offset ocfp-offset
		  :from (:argument 1)
		  :to :eval)
		 old-fp-pass)

     (:temporary (:sc descriptor-reg
		  :offset lra-offset
		  :from (:argument ,(if (eq return :tail) 2 1))
		  :to :eval)
		 return-pc-pass)

     ,@(if named
	 `((:temporary (:sc descriptor-reg :offset cname-offset
			:from (:argument ,(if (eq return :tail) 0 1))
			:to :eval)
		       name-pass))

	 `((:temporary (:sc descriptor-reg :offset lexenv-offset
			:from (:argument ,(if (eq return :tail) 0 1))
			:to :eval)
		       lexenv)
	   (:temporary (:scs (descriptor-reg) :from (:argument 0) :to :eval)
		       function)))


     (:temporary (:sc descriptor-reg :offset nargs-offset :to :eval)
		 nargs-pass)

     ,@(when variable
	 (mapcar #'(lambda (name offset)
		     `(:temporary (:sc descriptor-reg
				   :offset ,offset
				   :to :eval)
			 ,name))
		 register-arg-names register-arg-offsets))
     ,@(when (eq return :fixed)
	 '((:temporary (:scs (descriptor-reg) :from :eval) move-temp)))

     ,@(unless (eq return :tail)
	 '((:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)))

     (:temporary (:scs (interior-reg) :type interior) lip)

     (:generator ,(+ (if named 5 0)
		     (if variable 19 1)
		     (if (eq return :tail) 0 10)
		     15
		     (if (eq return :unknown) 25 0))
       
       (let ((cur-nfp (current-nfp-tn vop))
	     ,@(unless (eq return :tail)
		 '((lra-label (gen-label)))))

	 ,@(if variable
	       ;; Compute the number of arguments and move some of the arguments
	       ;; from the stack to the argument registers.
	       `((move nargs-pass csp-tn)
		 ;; This computes a byte difference, but due to the number of
		 ;; bytes per word and out fixnum tags, this leaves a fixnum
		 ;; word count for the callee.
		 (inst s nargs-pass new-fp)
		 ,@(let ((index -1))
		     (mapcar #'(lambda (name)
				 `(loadw ,name new-fp ,(incf index)))
			     register-arg-names)))
	       `((inst li nargs-pass (fixnumize nargs))))
 

	 ,@(if named
	       `((move name-pass name)
		 (loadw lip name-pass vm:symbol-raw-function-addr-slot
			vm:other-pointer-type))
	       `((move lexenv arg-fun)
		 (loadw function lexenv vm:closure-function-slot
			vm:function-pointer-type)
		 (inst cal lip function (- (ash vm:function-header-code-offset
						vm:word-shift)
					   vm:function-pointer-type))))

	 ,@(if (eq return :tail)
	       '((move old-fp-pass old-fp)
		 (maybe-load-stack-tn return-pc-pass return-pc)
		 (when cur-nfp
		   (inst cal nsp-tn cur-nfp
			 (component-non-descriptor-stack-usage)))
		 (inst b lip))
	       `((inst compute-lra-from-code
		       return-pc-pass code-tn lra-label)
		 (move old-fp-pass cfp-tn)
		 (when cur-nfp
		   (store-stack-tn cur-nfp nfp-save))
		 ,(if variable
		      '(move cfp-tn new-fp)
		      '(if (> nargs register-arg-count)
			   (move cfp-tn new-fp)
			   (move cfp-tn csp-tn)))
		 (inst b lip)
		 (emit-return-pc lra-label)))

	 ,@(ecase return
	     (:fixed
	      '((note-this-location vop :unknown-return)
		(default-unknown-values values nvals move-temp lra-label)
		(when cur-nfp
		  (load-stack-tn cur-nfp nfp-save))))
	     (:unknown
	      '((note-this-location vop :unknown-return)
		(receive-unknown-values values-start nvals start count
					lra-label)
		(when cur-nfp
		  (load-stack-tn cur-nfp nfp-save))))
	     (:tail))))))
) ;EVAL-WHEN

(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)
(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)

(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)


;;; Defined separately, since needs special code that BLT's the arguments
;;; down.
;;;
(define-vop (tail-call-variable)
  (:args (args-arg :scs (word-pointer-reg) :target args)
	 (function-arg :scs (descriptor-reg) :target lexenv)
	 (old-fp-arg :scs (word-pointer-reg) :target old-fp)
	 (lra-arg :scs (descriptor-reg) :target lra))
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) args)
  (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 1)) lexenv)
  (:temporary (:sc any-reg :offset ocfp-offset :from (:argument 2)) old-fp)
  (:temporary (:sc any-reg :offset lra-offset :from (:argument 3)) lra)
  (:temporary (:scs (any-reg) :from :eval) temp)
  (:vop-var vop)
  (:generator 75
    ;; Move these into the passing locations if they are not already there.
    (move args args-arg)
    (move lexenv function-arg)
    (move old-fp old-fp-arg)
    (move lra lra-arg)

    ;; Clear the number stack if anything is there.
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst cal nsp-tn cur-nfp
	      (component-non-descriptor-stack-usage))))

    ;; And jump to the assembly-routine that moves the arguments for us.
    (inst bala (make-fixup 'really-tail-call-variable :assembly-routine))))



;;;; Unknown values return:


;;; Do unknown-values return of a fixed number of values.  The Values are
;;; required to be set up in the standard passing locations.  Nvals is the
;;; number of values returned.
;;;
;;; If returning a single value, then deallocate the current frame, restore
;;; FP and jump to the single-value entry at Return-PC + 4.
;;;
;;; If returning other than one value, then load the number of values returned,
;;; NIL out unsupplied values registers, restore FP and return at Return-PC.
;;; When there are stack values, we must initialize the argument pointer to
;;; point to the beginning of the values block (which is the beginning of the
;;; current frame.)
;;;
(define-vop (return)
  (:args
   (old-fp :scs (word-pointer-reg))
   (return-pc :scs (descriptor-reg) :to (:eval 1))
   (values :more t))
  (:ignore values)
  (:info nvals)
  (:temporary (:sc any-reg :offset a0-offset :from (:eval 0) :to (:eval 1))
	      a0)
  (:temporary (:sc any-reg :offset a1-offset :from (:eval 0) :to (:eval 1))
	      a1)
  (:temporary (:sc any-reg :offset a2-offset :from (:eval 0) :to (:eval 1))
	      a2)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) val-ptr)
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:vop-var vop)
  (:generator 6
    (cond ((= nvals 1)
	   ;; Clear the stacks.
	   (let ((cur-nfp (current-nfp-tn vop)))
	     (when cur-nfp
	       (inst cal nsp-tn cur-nfp
		     (component-non-descriptor-stack-usage))))
	   (move csp-tn cfp-tn)
	   ;; Reset the frame pointer.
	   (move cfp-tn old-fp)
	   ;; Out of here.
	   ;; The return-pc (LRA) has other-pointer lowtag bits.  Also, in the
	   ;; instruction sequence in which the LRA points, there is a header
	   ;; word with immediate data indicating the offset back to the
	   ;; beginning of the component.  The calling convention says to
	   ;; return one word past the return point when the returner knows
	   ;; it has only one value, so we skip that word here and the header
	   ;; for the LRA.
	   (inst cal lip return-pc (- (* 2 word-bytes) other-pointer-type))
	   (inst bx lip)
	   (move code-tn return-pc))
	  (t
	   (inst li nargs (fixnumize nvals))
	   ;; Clear the number stack.
	   (let ((cur-nfp (current-nfp-tn vop)))
	     (when cur-nfp
	       (inst cal nsp-tn cur-nfp
		     (component-non-descriptor-stack-usage))))
	   (move val-ptr cfp-tn)
	   ;; Reset the frame pointer.
	   (move cfp-tn old-fp)
	   
	   (let ((immediate (* nvals word-bytes)))
	     (assert (typep immediate '(signed-byte 16)))
	     (inst cal csp-tn val-ptr immediate))
	   
	   (when (< nvals 1) (move a0 null-tn))
	   (when (< nvals 2) (move a1 null-tn))
	   (when (< nvals 3) (move a2 null-tn))
	   
	   (lisp-return return-pc lip)))))

;;; RETURN-MULTIPLE -- VOP.
;;;
;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention.  Otherwise, we
;;; branch off to an assembler routine.
;;;
;;; The Return-Multiple miscop uses a non-standard calling convention.  For one
;;; thing, it doesn't return.  We only use BALA because there isn't a BA
;;; instruction.   Also, we don't use A0..A2 for argument passing, since the
;;; miscop will want to load these with values off of the stack.  Instead, we
;;; pass Old-Fp, Start and Count in the normal locations for these values.
;;; Return-PC is passed in A3 since PC is trashed by the BALA. 
;;;
(define-vop (return-multiple)
  (:args
   (ocfp-arg :scs (word-pointer-reg) :target ocfp)
   (lra-arg :scs (descriptor-reg) :target lra)
   (vals-arg :scs (word-pointer-reg) :target vals)
   (nvals-arg :scs (any-reg) :target nvals))
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) ocfp)
  (:temporary (:sc descriptor-reg :offset lra-offset :from (:argument 1)) lra)
  (:temporary (:sc any-reg :offset cname-offset :from (:argument 2)) vals)
  (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nvals)
  (:temporary (:sc descriptor-reg :offset a0-offset) a0)
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:vop-var vop)
  (:generator 13
    (let ((not-single (gen-label)))
      ;; Clear the number stack.
      (let ((cur-nfp (current-nfp-tn vop)))
	(when cur-nfp
	  (inst cal nsp-tn cur-nfp
		(component-non-descriptor-stack-usage))))
      
      ;; Single case?
      (inst c nvals-arg (fixnumize 1))
      (inst bnc :eq not-single)
      
      ;; Return with one value.
      (loadw a0 vals-arg)
      (move csp-tn cfp-tn)
      (move cfp-tn ocfp-arg)
      (lisp-return lra-arg lip :offset 1)
      
      ;; Nope, not the single case.
      (emit-label not-single)
      
      ;; Load the register args, bailing out when we are done.
      (move ocfp ocfp-arg)
      (move lra lra-arg)
      (move vals vals-arg)
      (move nvals nvals-arg)
      (inst bala (make-fixup 'really-return-multiple :assembly-routine)))))

;;; COMPONENT-NON-DESCRIPTOR-STACK-USAGE -- Internal.
;;;
;;; This returns the non-descriptor stack usage in bytes for the component on
;;; which the compiler is currently working.
;;;
;;; On the MIPS, the stack must be dual-word aligned since it is also the C
;;; call stack.
;;;
;;; We're going to make the same assumption on the RT, but we don't even know
;;; why this is true on the MIPS.
;;;
(defun component-non-descriptor-stack-usage ()
  (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
     vm:word-bytes))



;;;; XEP hackery:


;;; We don't need to do any special setup for regular functions.
;;;
(define-vop (setup-environment)
  (:info label)
  (:generator 5))

;;; Extract the closure from the passing location (LEXENV).
;;;
(define-vop (setup-closure-environment)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :target closure
	       :to (:result 0))
	      lexenv)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:generator 6
    ;; Get result.
    (move closure lexenv)))

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments. 
;;;
;;; We wire the temporaries to make sure they do not interfere with any of
;;; special registers used during full-call, because we do not have acurate
;;; lifetime info about them at the time this vop is used.
;;; 
(define-vop (copy-more-arg)
  (:temporary (:sc descriptor-reg :offset cname-offset) temp)
  (:info fixed)
  (:generator 20
    (let ((do-more-args (gen-label))
	  (done-more-args (gen-label)))
      (inst c nargs-tn (fixnumize fixed))
      (inst bc :gt do-more-args)
      (assemble (*elsewhere*)
	(emit-label do-more-args)
	;; Jump to assembler routine passing fixed at cname-offset.
	(inst li temp (fixnumize fixed))
	(inst bala (make-fixup 'really-copy-more-args :assembly-routine))
	(inst b done-more-args))
      (emit-label done-more-args))))


;;; More args are stored consequtively on the stack, starting immediately at
;;; the context pointer.  The context pointer is not typed, so the lowtag is 0.
;;;
(define-vop (more-arg word-index-ref)
  (:variant 0 0)
  (:translate %more-arg))


;;; Turn more arg (context, count) into a list.
;;;
(define-vop (listify-rest-args)
  (:args (context-arg :target context :scs (descriptor-reg))
	 (count-arg :target count :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:scs (word-pointer-reg) :from (:argument 0)) context)
  (:temporary (:scs (non-descriptor-reg) :from :eval) ndescr dst)
  (:temporary (:scs (any-reg) :from (:argument 1)) count)
  (:temporary (:scs (word-pointer-reg) :from :eval) alloc)
  (:temporary (:scs (descriptor-reg) :from :eval) temp)
  (:results (result :scs (descriptor-reg)))
  (:translate %listify-rest-args)
  (:policy :safe)
  (:generator 20
    (let ((enter (gen-label))
	  (loop (gen-label))
	  (done (gen-label)))
      (move context context-arg)
      (move count count-arg)
      ;; Check to see if there are any arguments.
      (inst c count 0)
      (inst bcx :eq done)
      (move result null-tn)

      ;; We need to do this atomically.
      (pseudo-atomic (ndescr)
	(load-symbol-value alloc *allocation-pointer*)
	;; Allocate a cons (2 words) for each item.
	(inst cal result alloc vm:list-pointer-type)
	(move dst result)
	(inst cas alloc count alloc)
	(inst cas alloc count alloc)
	(inst bx enter)
	(store-symbol-value alloc *allocation-pointer*)

	;; Store the current cons in the cdr of the previous cons.
	(emit-label loop)
	(storew dst dst -1 vm:list-pointer-type)

	;; Grab one value and stash it in the car of this cons.
	(emit-label enter)
	(loadw temp context)
	(inst inc context vm:word-bytes)
	(storew temp dst 0 vm:list-pointer-type)

	;; Dec count, and if != zero, go back for more.
	(inst s count (fixnumize 1))
	(inst bncx :eq loop)
	(inst inc dst (* 2 vm:word-bytes))

	;; NIL out the last cons.
	(storew null-tn dst -1 vm:list-pointer-type))
      (load-symbol-value ndescr *internal-gc-trigger*)
      (inst tlt ndescr alloc)
      (emit-label done))))



;;; Return the location and size of the more arg glob created by Copy-More-Arg.
;;; Supplied is the total number of arguments supplied (originally passed in
;;; NARGS.)  Fixed is the number of non-rest arguments.
;;;
;;; We must duplicate some of the work done by Copy-More-Arg, since at that
;;; time the environment is in a pretty brain-damaged state, preventing this
;;; info from being returned as values.  What we do is compute
;;; supplied - fixed, and return a pointer that many words below the current
;;; stack top.
;;;
(define-vop (more-arg-context)
  (:args (supplied :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info fixed)
  (:results
   (context :scs (descriptor-reg))
   (count :scs (any-reg)))
  (:generator 5
    (inst s count supplied (fixnumize fixed))
    (move context csp-tn)
    (inst s context count)))


;;; Signal wrong argument count error if Nargs isn't = to Count.
;;;
(define-vop (verify-argument-count)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info count)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (let ((err-lab
	   (generate-error-code vop invalid-argument-count-error nargs)))
      (inst c nargs (fixnumize count))
      (inst bnc :eq err-lab))))

;;; Signal an argument count error.
;;;
(macrolet ((frob (name error &rest args)
	     `(define-vop (,name)
		(:args ,@(mapcar #'(lambda (arg)
				     `(,arg :scs (any-reg descriptor-reg)))
				 args))
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1000
		  (error-call vop ,error ,@args)))))
  (frob argument-count-error invalid-argument-count-error nargs)
  (frob type-check-error object-not-type-error object type)
  (frob odd-keyword-arguments-error odd-keyword-arguments-error)
  (frob unknown-keyword-argument-error unknown-keyword-argument-error key)
  (frob nil-function-returned-error nil-function-returned-error fun))
