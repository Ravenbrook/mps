;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/assembly/amd64/arith.lisp,v 1.2 2004/07/14 20:51:49 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle simple cases for generic arithmetic.
;;;
;;; Written by William Lott.
;;; Debugged by Paul Werkowski -- Spring/Summer 1995.
;;;
(in-package :amd64)

(eval-when (compile eval)


;;;; Addition, Subtraction, and Multiplication

(defmacro define-generic-arith-routine ((fun cost) &body body)
  `(define-assembly-routine (,(symbolicate "GENERIC-" fun)
			     (:cost ,cost)
			     (:return-style :full-call)
			     (:translate ,fun)
			     (:policy :safe)
			     (:save-p t))
			    ((:arg x (descriptor-reg any-reg) rdx-offset)
			     (:arg y (descriptor-reg any-reg)
			      ;; this seems wrong esi-offset
			      rdi-offset)

			     (:res res (descriptor-reg any-reg) rdx-offset)

			     (:temp rax unsigned-reg rax-offset)
			     (:temp rbx unsigned-reg rbx-offset)
			     (:temp rcx unsigned-reg rcx-offset)
			     (:temp r11 unsigned-reg r11-offset))

    (inst test x 3)			; fixnum?
    (inst jmp :nz DO-STATIC-FUN)	; no - do generic
    (inst test y 3)			; fixnum?
    (inst jmp :z DO-BODY)		; yes - doit here

    DO-STATIC-FUN
    (inst pop rax)
    (inst push rbp-tn)
    (inst lea rbp-tn (make-ea :qword :base rsp-tn :disp word-bytes))
    (inst sub rsp-tn (* 2 word-bytes))
    (inst push rax)			; callers return addr
    (inst mov rcx (fixnumize 2))	; arg count
    (inst mov rbx (+ nil-value (static-function-offset
				',(symbolicate "TWO-ARG-" fun))))
    (inst jmp (make-ea :qword :base rbx))
    
    DO-BODY
    ,@body))

); eval-when


(define-generic-arith-routine (+ 10)
  (move res x)
  (inst add res y)
  (inst jmp :no OKAY)
  (inst rcr res 1)			; carry has correct sign
  (inst sar res 1)			; remove type bits

  (move rcx res)

  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset) r11)
    (storew rcx res bignum-digits-offset other-pointer-type))
  
  OKAY)


(define-generic-arith-routine (- 10)
    ;;; I can't figure out the flags on subtract. Overflow never gets
    ;;; set and carry always does. (- 0 most-negative-fixnum) can't be
    ;;; easily detected so just let the upper level stuff do it.
  (inst jmp DO-STATIC-FUN)

  ;; this is junk code. clean this up!!!
  (move res x)
  (inst sub res y)
  (inst jmp :no OKAY)
  (inst rcr res 1)
  (inst sar res 1)			; remove type bits
  
  (move rcx res)
  
  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset) r11)
    (storew rcx res bignum-digits-offset other-pointer-type))
  OKAY)

(define-generic-arith-routine (* 30)
  (move rax x)				; must use rax for 64-bit result
  (inst sar rax 2)			; remove *4 fixnum bias
  (inst imul y)				; result in rdx:eax
  (inst jmp :no okay)			; still fixnum

  ;; zzz jrd changed rdx to rbx in here, as rdx isn't listed as a temp, above
  ;;     pfw says that loses big -- rdx is target for arg x and result res
  ;;     note that 'rdx' is not defined -- using x
  (inst shrd rax x 2)			; high bits from rdx
  (inst sar x 2)			; now shift rdx too

  (move rcx x)				; save high bits from cdq
  (inst cdq)				; rdx:eax <- sign-extend of rax
  (inst cmp x rcx)
  (inst jmp :e SINGLE-WORD-BIGNUM)

  (with-fixed-allocation (res bignum-type (+ bignum-digits-offset 2) r11)
    (storew rax res bignum-digits-offset other-pointer-type)
    (storew rcx res (1+ bignum-digits-offset) other-pointer-type))
  (inst jmp DONE)

  SINGLE-WORD-BIGNUM
  
  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset) r11)
    (storew rax res bignum-digits-offset other-pointer-type))
  (inst jmp DONE)

  OKAY
  (move res rax)
  DONE)

(define-assembly-routine (generic-negate
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate %negate)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) rdx-offset)
			  (:res res (descriptor-reg any-reg) rdx-offset)

			  (:temp rax unsigned-reg rax-offset)
			  (:temp rcx unsigned-reg rcx-offset)
			  (:temp r11 unsigned-reg r11-offset))
  (inst test x 3)
  (inst jmp :z FIXNUM)

  (inst pop rax)
  (inst push rbp-tn)
  (inst lea rbp-tn (make-ea :qword :base rsp-tn :disp word-bytes))
  (inst sub rsp-tn (fixnumize 2))
  (inst push rax)
  (inst mov rcx (fixnumize 1))		; arg count
  (inst mov rax (+ nil-value (static-function-offset '%negate)))
  (inst jmp (make-ea :qword :base rax))
  
  FIXNUM
  (move res x)
  (inst neg res)			; (- most-negative-fixnum) is BIGNUM
  (inst jmp :no OKAY)
  (inst shr res 2)			; sign bit is data - remove type bits
  (move rcx res)

  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset) r11)
    (storew rcx res bignum-digits-offset other-pointer-type))
  
  OKAY)



;;;; Comparison routines.

(eval-when (compile eval)

(defmacro define-cond-assem-rtn (name translate static-fn test)
  `(define-assembly-routine (,name
			     (:cost 10)
			     (:return-style :full-call)
			     (:policy :safe)
			     (:translate ,translate)
			     (:save-p t))
			    ((:arg x (descriptor-reg any-reg) rdx-offset)
			     (:arg y (descriptor-reg any-reg) rdi-offset)
			     
			     (:res res descriptor-reg rdx-offset)

			     (:temp rax unsigned-reg rax-offset)
			     (:temp rcx unsigned-reg rcx-offset))
    (inst test x 3)
    (inst jmp :nz DO-STATIC-FN)
    (inst test y 3)
    (inst jmp :z DO-COMPARE)
    
    DO-STATIC-FN
    (inst pop rax)
    (inst push rbp-tn)
    (inst lea rbp-tn (make-ea :qword :base rsp-tn :disp word-bytes))
    (inst sub rsp-tn (* 2 word-bytes))
    (inst push rax)
    (inst mov rcx (fixnumize 2))
    (inst mov rax (+ nil-value
		   (static-function-offset ',static-fn)))
    (inst jmp (make-ea :qword :base rax))
    
    DO-COMPARE
    (inst cmp x y)
    (inst jmp ,test TRUE)
    (inst mov res nil-value)
    (inst pop rax)
    (inst add rax 3) ; single value return
    (inst jmp rax)
    
    TRUE
    (load-symbol res t)))

); eval-when

(define-cond-assem-rtn generic-< < two-arg-< :l)
(define-cond-assem-rtn generic-> > two-arg-> :g)

(define-assembly-routine (generic-eql
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate eql)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) rdx-offset)
			  (:arg y (descriptor-reg any-reg) rdi-offset)
			  
			  (:res res descriptor-reg rdx-offset)
			  
			  (:temp rax unsigned-reg rax-offset)
			  (:temp rcx unsigned-reg rcx-offset))
  (inst cmp x y)
  (inst jmp :e RETURN-T)
  (inst test x 3)
  (inst jmp :z RETURN-NIL)
  (inst test y 3)
  (inst jmp :nz DO-STATIC-FN)

  RETURN-NIL
  (inst mov res nil-value)
  (inst pop rax)
  (inst add rax 3) ; single value return
  (inst jmp rax)

  DO-STATIC-FN
  (inst pop rax)
  (inst push rbp-tn)
  (inst lea rbp-tn (make-ea :qword :base rsp-tn :disp word-bytes))
  (inst sub rsp-tn (fixnumize 2))
  (inst push rax) ; return address
  (inst mov rcx (fixnumize 2)) ; number of arguments
  (inst mov rax (+ nil-value (static-function-offset 'eql)))
  (inst jmp (make-ea :qword :base rax))
  
  RETURN-T
  (load-symbol res t))

(define-assembly-routine (generic-=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate =)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) rdx-offset)
			  (:arg y (descriptor-reg any-reg) rdi-offset)
			  
			  (:res res descriptor-reg rdx-offset)
			  
			  (:temp rax unsigned-reg rax-offset)
			  (:temp rcx unsigned-reg rcx-offset)
			  )
  (inst test x 3)			; descriptor?
  (inst jmp :nz DO-STATIC-FN)		; yes do it here
  (inst test y 3)			; descriptor?
  (inst jmp :nz DO-STATIC-FN)
  (inst cmp x y)
  (inst jmp :e RETURN-T)		; ok

  (inst mov res nil-value)
  (inst pop rax)
  (inst add rax 3)
  (inst jmp rax)

  DO-STATIC-FN
  (inst pop rax)
  (inst push rbp-tn)
  (inst lea rbp-tn (make-ea :qword :base rsp-tn :disp word-bytes))
  (inst sub rsp-tn (* 2 word-bytes))
  (inst push rax)
  (inst mov rcx (fixnumize 2))
  (inst mov rax (+ nil-value (static-function-offset 'two-arg-=)))
  (inst jmp (make-ea :qword :base rax))
  
  RETURN-T
  (load-symbol res t))


;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.
;;;
;;; State:
;;;  0-1:   Constant matrix A. [0, #x9908b0df] (not used here)
;;;  2:     Index; init. to 1.
;;;  3-626: State.
;;;

;;; This assembly routine is called from the inline VOP and updates
;;; the state vector with new random numbers. The state vector is
;;; passed in the rAX register.
;;;
#+assembler ; we don't want a vop for this one.
(define-assembly-routine
    (random-mt19937-update)
    ((:temp state unsigned-reg rax-offset)
     (:temp k unsigned-reg rbx-offset)
     (:temp y unsigned-reg rcx-offset)
     (:temp tmp unsigned-reg rdx-offset))

  ;; Save the temporary registers.
  (inst push k)
  (inst push y)
  (inst push tmp)

  ;; Generate a new set of results.
  (inst xor k k)
  LOOP1
  (inst mov y (make-ea :qword :base state :index k :scale 4
		       :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov tmp (make-ea :qword :base state :index k :scale 4
			 :disp (- (* (+ 1 3 vm:vector-data-offset)
				     vm:word-bytes)
				  vm:other-pointer-type)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip1)
  (inst xor y #x9908b0df)
  SKIP1
  (inst xor y (make-ea :qword :base state :index k :scale 4
		       :disp (- (* (+ 397 3 vm:vector-data-offset)
				   vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov (make-ea :qword :base state :index k :scale 4
		     :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type))
	y)
  (inst inc k)
  (inst cmp k (- 624 397))
  (inst jmp :b loop1)
  LOOP2
  (inst mov y (make-ea :qword :base state :index k :scale 4
		       :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov tmp (make-ea :qword :base state :index k :scale 4
			 :disp (- (* (+ 1 3 vm:vector-data-offset)
				     vm:word-bytes)
				  vm:other-pointer-type)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip2)
  (inst xor y #x9908b0df)
  SKIP2
  (inst xor y (make-ea :qword :base state :index k :scale 4
		       :disp (- (* (+ (- 397 624) 3 vm:vector-data-offset)
				   vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov (make-ea :qword :base state :index k :scale 4
		     :disp (- (* (+ 3 vm:vector-data-offset) vm:word-bytes)
			      vm:other-pointer-type))
	y)
  (inst inc k)
  (inst cmp k (- 624 1))
  (inst jmp :b loop2)
  
  (inst mov y (make-ea :qword :base state
		       :disp (- (* (+ (- 624 1) 3 vm:vector-data-offset)
				   vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov tmp (make-ea :qword :base state
			 :disp (- (* (+ 0 3 vm:vector-data-offset)
				     vm:word-bytes)
				  vm:other-pointer-type)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip3)
  (inst xor y #x9908b0df)
  SKIP3
  (inst xor y (make-ea :qword :base state
		       :disp (- (* (+ (- 397 1) 3 vm:vector-data-offset)
				   vm:word-bytes)
				vm:other-pointer-type)))
  (inst mov (make-ea :qword :base state
		     :disp (- (* (+ (- 624 1) 3 vm:vector-data-offset)
				 vm:word-bytes)
			      vm:other-pointer-type))
	y)

  ;; Restore the temporary registers and return.
  (inst pop tmp)
  (inst pop y)
  (inst pop k)
  (inst ret))
