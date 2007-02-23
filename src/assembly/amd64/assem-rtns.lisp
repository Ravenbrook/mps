;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/assembly/amd64/assem-rtns.lisp,v 1.3 2004/07/27 23:28:41 cwang Exp $")
;;;
;;; **********************************************************************
;;; 
;;; This file contains the machine specific support routines needed by
;;; the file assembler.
;;;
;;; Written by William Lott
;;;
;;; Debugged by Paul F. Werkowski -- Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1997.
;;;
(in-package :amd64)


;;;; Return-multiple

;;; For return-multiple, we have to move the results from the end of the
;;; frame for the function that is returning to the end of the frame for
;;; the function being returned to.

#+assembler ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple (:return-style :none))
    (;; These four are really arguments.
     (:temp rax unsigned-reg rax-offset) ; the return-pc to finally jump to.
     (:temp rbx unsigned-reg rbx-offset) ; pointer to where to put the values.
     (:temp rcx unsigned-reg rcx-offset) ; number of values to find there.
     (:temp rsi unsigned-reg rsi-offset) ; pointer to where to find the values.

     ;; These we need as temporaries.
     (:temp rdx unsigned-reg rdx-offset)
     (:temp rdi unsigned-reg rdi-offset))
     
  ;; Pick off the cases where everything fits in register args.
  (inst jrcxz zero-values)
  (inst cmp rcx (fixnumize 1))
  (inst jmp :e one-value)
  (inst cmp rcx (fixnumize 2))
  (inst jmp :e two-values)
  (inst cmp rcx (fixnumize 3))
  (inst jmp :e three-values)

  ;; Save the count, because the loop is going to destroy it.
  (inst mov rdx rcx)

  ;; Blit the values down the stack.  Note: there might be overlap, so we have
  ;; to be careful not to clobber values before we've read them.  Because the
  ;; stack builds down, we are copying to a larger address.  Therefore, we need
  ;; to iterate from larger addresses to smaller addresses.
  ;; pfw-this says copy rcx words from rsi to rdi counting down.
  (inst shr rcx 2)			; fixnum to raw word count
  (inst std)				; count down
  (inst sub rsi 8)			; ?
  (inst lea rdi (make-ea :qword :base rbx :disp (- word-bytes)))
  (inst rep)
  (inst movs :qword)

  ;; Restore the count.
  (inst mov rcx rdx)

  ;; Set the stack top to the last result.
  (inst lea rsp-tn (make-ea :qword :base rdi :disp word-bytes))

  ;; Load the register args.
  (loadw rdx rbx -1)
  (loadw rdi rbx -2)
  (loadw rsi rbx -3)

  ;; And back we go.
  (inst jmp rax)

  ;; Handle the register arg cases.
  ZERO-VALUES
  (move rsp-tn rbx)
  (inst mov rdx nil-value)
  (inst mov rdi rdx)
  (inst mov rsi rdx)
  (inst jmp rax)

  ONE-VALUE ; Note: we can get this, because the return-multiple vop
	    ; doesn't check for this case when size > speed.
  (loadw rdx rsi -1)
  (inst mov rsp-tn rbx)
  (inst add rax 3)
  (inst jmp rax)

  TWO-VALUES
  (loadw rdx rsi -1)
  (loadw rdi rsi -2)
  (inst mov rsi nil-value)
  (inst lea rsp-tn (make-ea :qword :base rbx :disp (* -2 word-bytes)))
  (inst jmp rax)

  THREE-VALUES
  (loadw rdx rsi -1)
  (loadw rdi rsi -2)
  (loadw rsi rsi -3)
  (inst lea rsp-tn (make-ea :qword :base rbx :disp (* -3 word-bytes)))
  (inst jmp rax))



;;;; tail-call-variable.

;;; For tail-call-variable, we have to copy the arguments from the end of our
;;; stack frame (were args are produced) to the start of our stack frame
;;; (were args are expected).
;;;
;;; We take the function to call in rAX and a pointer to the arguments in
;;; rSI.  rBP says the same over the jump, and the old frame pointer is
;;; still saved in the first stack slot.  The return-pc is saved in
;;; the second stack slot, so we have to push it to make it look like
;;; we actually called.  We also have to compute rCX from the difference
;;; between rSI and the stack top.
;;; 
#+assembler ;; no vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ((:temp rax unsigned-reg rax-offset)
     (:temp rbx unsigned-reg rbx-offset)
     (:temp rcx unsigned-reg rcx-offset)
     (:temp rdx unsigned-reg rdx-offset)
     (:temp rdi unsigned-reg rdi-offset)
     (:temp rsi unsigned-reg rsi-offset))

  ;; Calculate NARGS (as a fixnum)
  (move rcx rsi)
  (inst sub rcx rsp-tn)
  (inst shr rcx 1)
  
  ;; Check for all the args fitting the the registers.
  (inst cmp rcx (fixnumize 3))
  (inst jmp :le REGISTER-ARGS)

  ;; Save the OLD-FP and RETURN-PC because the blit it going to trash
  ;; those stack locations.  Save the rCX, because the loop is going
  ;; to trash it.
  (pushw rbp-tn -1)
  (loadw rbx rbp-tn -2)
  (inst push rcx)

  ;; Do the blit.  Because we are coping from smaller addresses to larger
  ;; addresses, we have to start at the largest pair and work our way down.
  (inst shr rcx 2)			; fixnum to raw words
  (inst std)				; count down
  (inst lea rdi (make-ea :qword :base rbp-tn :disp (- word-bytes)))
  (inst sub rsi word-bytes)
  (inst rep)
  (inst movs :qword)

  ;; Load the register arguments carefully.
  (loadw rdx rbp-tn -1)

  ;; Restore OLD-FP and rCX.
  (inst pop rcx)
  (popw rbp-tn -1)			; overwrites a0

  ;; Blow off the stack above the arguments.
  (inst lea rsp-tn (make-ea :qword :base rdi :disp word-bytes))

  ;; remaining register args
  (loadw rdi rbp-tn -2)
  (loadw rsi rbp-tn -3)

  ;; Push the (saved) return-pc so it looks like we just called.
  (inst push rbx)

  ;; And jump into the function.
    (inst jmp 
	  (make-ea :byte :base rax
		   :disp (- (* closure-function-slot word-bytes)
			    function-pointer-type)))

  ;; All the arguments fit in registers, so load them.
  REGISTER-ARGS
  (loadw rdx rsi -1)
  (loadw rdi rsi -2)
  (loadw rsi rsi -3)

  ;; Clear most of the stack.
  (inst lea rsp-tn
	(make-ea :qword :base rbp-tn :disp (* -3 word-bytes)))

  ;; Push the return-pc so it looks like we just called.
  (pushw rbp-tn -2)

  ;; And away we go.
  (inst jmp (make-ea :byte :base rax
		     :disp (- (* closure-function-slot word-bytes)
			      function-pointer-type))))



(define-assembly-routine (throw
			  (:return-style :none))
			 ((:arg target (descriptor-reg any-reg) rdx-offset)
			  (:arg start any-reg rbx-offset)
			  (:arg count any-reg rcx-offset)
			  (:temp catch any-reg rax-offset))
  
  (declare (ignore start count))

  (load-symbol-value catch lisp::*current-catch-block*)
  
  LOOP
  
  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
    (inst or catch catch)		; check for NULL pointer
    (inst jmp :z error))
  
  (inst cmp target (make-ea-for-object-slot catch catch-block-tag-slot 0))
  (inst jmp :e exit)
  
  (loadw catch catch catch-block-previous-catch-slot)
  (inst jmp loop)
  
  EXIT
  
  ;; Here rAX points to catch block containing symbol pointed to by rDX.
  (inst jmp (make-fixup 'unwind :assembly-routine)))

;;;; Non-local exit noise.

(define-assembly-routine (unwind
			  (:return-style :none)
			  (:translate %continue-unwind)
			  (:policy :fast-safe))
			 ((:arg block (any-reg descriptor-reg) rax-offset)
			  (:arg start (any-reg descriptor-reg) rbx-offset)
			  (:arg count (any-reg descriptor-reg) rcx-offset)
			  (:temp uwp unsigned-reg rsi-offset)
			  (:temp temp unsigned-reg r11-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil invalid-unwind-error)))
    (inst or block block)		; check for NULL pointer
    (inst jmp :z error))
  
  (load-symbol-value uwp lisp::*current-unwind-protect-block*)

  ;; Does *cuwpb* match value stored in argument cuwp slot?
  (inst cmp uwp
	(make-ea-for-object-slot block unwind-block-current-uwp-slot 0))
  ;; If a match, return to context in arg block.
  (inst jmp :e do-exit)

  ;; Not a match - return to *current-unwind-protect-block* context.
  ;; Important! Must save (and return) the arg 'block' for later use!!
  (move rdx-tn block)
  (move block uwp)
  ;; Set next unwind protect context.
  (loadw uwp uwp unwind-block-current-uwp-slot)
  (store-symbol-value uwp lisp::*current-unwind-protect-block* temp)
  
  DO-EXIT
  
  (loadw rbp-tn block unwind-block-current-cont-slot)
  
  ;; Uwp-entry expects some things in known locations so that they can
  ;; be saved on the stack: the block in rdx-tn; start in rbx-tn; and
  ;; count in rcx-tn

  (inst jmp (make-ea :byte :base block
		     :disp (* unwind-block-entry-pc-slot word-bytes))))
