;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/amd64/macros.lisp,v 1.6 2004/10/19 19:16:20 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the a bunch of handy macros for the amd64.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997,1998,1999.
;;;
(in-package :amd64)


;;; We can load/store into fp registers through the top of
;;; stack %st(0) (fr0 here). Loads imply a push to an empty register
;;; which then changes all the reg numbers. These macros help manage that.

;;; Use this when don't have to load anything. It preserves old tos value
;;; But probably destroys tn with operation.
(defmacro with-tn@fp-top((tn) &body body)
  `(progn
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))
    ,@body
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))))

;;; Use this to prepare for load of new value from memory. This
;;; changes the register numbering so the next instruction had better
;;; be a FP load from memory; a register load from another register
;;; will probably be loading the wrong register!
(defmacro with-empty-tn@fp-top((tn) &body body)
  `(progn
    (inst fstp ,tn)
    ,@body
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))))		; save into new dest and restore st(0)


;;; Instruction-like macros.
(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

(defmacro make-ea-for-object-slot (ptr slot lowtag)
  `(make-ea :qword :base ,ptr :disp (- (* ,slot word-bytes) ,lowtag)))

(defmacro loadw (value ptr &optional (slot 0) (lowtag 0))
  `(inst mov ,value (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro storew (value ptr &optional (slot 0) (lowtag 0))
  (once-only ((value value))
    `(inst mov (make-ea-for-object-slot ,ptr ,slot ,lowtag) ,value)))

(defmacro pushw (ptr &optional (slot 0) (lowtag 0))
  `(inst push (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro popw (ptr &optional (slot 0) (lowtag 0))
  `(inst pop (make-ea-for-object-slot ,ptr ,slot ,lowtag)))


;;;; Macros to generate useful values.

(defmacro load-symbol (reg symbol)
  `(inst mov ,reg (+ nil-value (static-symbol-offset ,symbol))))

(defmacro load-symbol-value (reg symbol)
  `(if (= (tn-offset ,reg) rax-offset)
    ;; only rax can do this in one instruction
    (inst mov ,reg
     (make-ea :qword
      :disp (+ nil-value
	       (static-symbol-offset ',symbol)
	       (ash symbol-value-slot word-shift)
	       (- other-pointer-type))))
    (progn
      ;; other registers need 2 instructions
      (inst mov ,reg
	    (+ nil-value
	       (static-symbol-offset ',symbol)
	       (ash symbol-value-slot word-shift)
	       (- other-pointer-type)))
      (inst mov ,reg (make-ea :qword :base ,reg)))))
      

(defmacro store-symbol-value (reg symbol &optional temp) ; temp is unnecessary for rax
  `(if (or (= (tn-offset ,reg) rax-offset) (not ,temp))
    ;; only rax can do this in one instruction
    (inst mov
     (make-ea :qword
      :disp (+ nil-value
	       (static-symbol-offset ',symbol)
	       (ash symbol-value-slot word-shift)
	       (- other-pointer-type)))
     ,reg)
    (progn
      ;; other registers need 2 instructions
      (inst mov ,temp
	    (+ nil-value
	       (static-symbol-offset ',symbol)
	       (ash symbol-value-slot word-shift)
	       (- other-pointer-type)))
      (inst mov (make-ea :qword :base ,temp) ,reg))))
      

(defun symbol-value-address (symbol)
  (+ nil-value
     (static-symbol-offset symbol)
     (ash symbol-value-slot word-shift)
     (- other-pointer-type)))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase (backend-byte-order *target-backend*)
      (:little-endian
       `(inst mov ,n-target
	      (make-ea :byte :base ,n-source :disp ,n-offset)))
      (:big-endian
       `(inst mov ,n-target
	      (make-ea :byte :base ,n-source :disp (+ ,n-offset 3)))))))

(defmacro load-foreign-data-symbol (reg name )
  #+linkage-table `(inst mov ,reg (make-fixup (extern-alien-name ,name)
					      :foreign-data))
  #-linkage-table `(inst lea ,reg (make-fixup (extern-alien-name ,name)
					      :foreign)))

;; the right one to use
(defmacro load-foreign-data-symbol2 (reg name temp)
  #+linkage-table `(progn
		    (inst mov-imm ,temp (make-fixup (extern-alien-name ,name)
					 :foreign-data))
		    (inst mov ,reg (make-ea :qword :base ,temp)))
  #-linkage-table `(inst lea ,reg (make-fixup (extern-alien-name ,name)
					      :foreign))
  )

;;;; Allocation helpers

;;; Two allocation approaches are implemented. A call into C can be
;;; used where special care can be taken to disable
;;; interrupts. Alternatively with gencgc inline allocation is possible
;;; although it isn't interrupt safe.

;;; For GENCGC it is possible to inline object allocation, to permit
;;; this set the following variable to True.
(defparameter *maybe-use-inline-allocation* t)

(defun load-size (alloc-tn dst-tn size)
  (unless (and (tn-p size) (location= alloc-tn size))
    (inst mov dst-tn size)))

(defun inline-allocation (alloc-tn size temp-tn)
  ;; Since linkage table will trash r11, the easiest solution is to make sure temp-tn is r11
  (assert (= (tn-offset temp-tn) #.r11-offset))
  (let ((ok (gen-label)))
    ;;
    ;; Load the size first so that the size can be in the same
    ;; register as alloc-tn.
    (load-size alloc-tn alloc-tn size)
    ;;
    (inst mov temp-tn (symbol-value-address '*current-region-free-pointer*))
    (inst add alloc-tn (make-ea :qword :base temp-tn))
    (inst mov temp-tn (symbol-value-address '*current-region-end-addr*))
    (inst cmp alloc-tn (make-ea :qword :base temp-tn))
    (inst jmp :be OK)
    ;;
    ;; Dispatch to the appropriate overflow routine. There is a
    ;; routine for each destination.
    (inst mov-imm temp-tn
	  (make-fixup (extern-alien-name
		       (ecase (tn-offset alloc-tn)
			 (#.rax-offset "alloc_overflow_rax")
			 (#.rcx-offset "alloc_overflow_rcx")
			 (#.rdx-offset "alloc_overflow_rdx")
			 (#.rbx-offset "alloc_overflow_rbx")
			 (#.rsi-offset "alloc_overflow_rsi")
			 (#.rdi-offset "alloc_overflow_rdi")
			 (#.r8-offset "alloc_overflow_r8")
			 (#.r9-offset "alloc_overflow_r9")
			 (#.r10-offset "alloc_overflow_r10")
			 ;; no r11
			 (#.r12-offset "alloc_overflow_r12")
			 (#.r13-offset "alloc_overflow_r13")
			 (#.r14-offset "alloc_overflow_r14")
			 (#.r15-offset "alloc_overflow_r15")))
		      :foreign))
    (inst call temp-tn)
    (emit-label ok)
    (inst mov temp-tn (symbol-value-address '*current-region-free-pointer*))
    (inst xchg (make-ea :qword :base temp-tn) alloc-tn))
  (values))

(defun not-inline-allocation (alloc-tn size temp-tn)
  ;; C call to allocate via dispatch routines. Each destination has a
  ;; special entry point. The size may be a register or a constant.
  ;; This is used only if space is more important than speed.
  ;; Since linkage table will trash r11, the easiest solution is to make sure temp-tn is r11
  (assert (= (tn-offset temp-tn) #.r11-offset))
  (ecase (tn-offset alloc-tn)
    (#.rax-offset
     (load-size alloc-tn rax-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_rax")
				       :foreign)))
    (#.rcx-offset
     (load-size alloc-tn rcx-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_rcx")
				       :foreign)))
    (#.rdx-offset
     (load-size alloc-tn rdx-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_rdx")
				       :foreign)))
    (#.rbx-offset
     (load-size alloc-tn rbx-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_rbx")
				       :foreign)))
    (#.rsi-offset
     (load-size alloc-tn rsi-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_rsi")
				       :foreign)))
    (#.rdi-offset
     (load-size alloc-tn rdi-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_rdi")
				       :foreign)))
    (#.r8-offset
     (load-size alloc-tn r8-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_r8")
				       :foreign)))
    (#.r9-offset
     (load-size alloc-tn r9-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_r9")
				       :foreign)))
    (#.r10-offset
     (load-size alloc-tn r10-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_r10")
				       :foreign)))
    ;; no r11
    (#.r12-offset
     (load-size alloc-tn r12-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_r12")
				       :foreign)))
    (#.r13-offset
     (load-size alloc-tn r13-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_r13")
				       :foreign)))
    (#.r14-offset
     (load-size alloc-tn r14-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_r14")
				       :foreign)))
    (#.r15-offset
     (load-size alloc-tn r15-tn size)
     (inst mov-imm temp-tn (make-fixup (extern-alien-name "alloc_to_r15")
				       :foreign))))
  (inst call temp-tn)
  (values))

;;;
;;; Allocate SIZE bytes from the stack, storing a pointer to the
;;; allocated memory in ALLOC-TN.
;;;
(defun dynamic-extent-allocation (alloc-tn nbytes temp-tn)
  (inst sub rsp-tn nbytes)
  (inst mov temp-tn #xfffffffffffffff0)
  (inst and rsp-tn temp-tn) ; alignment, not necessary for 3 bit low-tag
					; but necessary if we go to 4 bit
  (inst mov alloc-tn rsp-tn)
  (values))

(defun allocation (alloc-tn size temp-tn &optional inline dynamic-extent)
  "Allocate an object with a size in bytes given by Size.
   The size may be an integer or a TN.
   If Inline is a VOP node-var then it is used to make an appropriate
   speed vs size decision.  If Dynamic-Extent is true, and otherwise
   appropriate, allocate from the stack."
  (cond (dynamic-extent
	 (dynamic-extent-allocation alloc-tn size temp-tn))
	((and *maybe-use-inline-allocation*
	      (or (null inline)
		  (policy inline (>= speed space)))
	      (backend-featurep :gencgc))
	 (inline-allocation alloc-tn size temp-tn))
	(t
	 (not-inline-allocation alloc-tn size temp-tn)))
  (values))

(defmacro with-fixed-allocation ((result-tn type-code size temp-tn &optional inline)
				 &rest forms)
  "Allocate an other-pointer object of fixed Size with a single
   word header having the specified Type-Code.  The result is placed in
   Result-TN."
  `(pseudo-atomic
    (allocation ,result-tn (pad-data-block ,size) ,temp-tn ,inline)
    (storew (logior (ash (1- ,size) vm:type-bits) ,type-code) ,result-tn)
    (inst lea ,result-tn
     (make-ea :byte :base ,result-tn :disp other-pointer-type))
    ,@forms))


;;;; Error Code

(eval-when (compile load eval)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym))
	  (length (gensym)))
      `((inst int 3)				; i386 breakpoint instruction
	;; The return PC points here; note the location for the debugger.
	(let ((vop ,vop))
  	  (when vop
		(note-this-location vop :internal-error)))
	(inst byte ,kind)			; eg trap_Xyyy
	(let ((,vector (make-array 8 :element-type '(unsigned-byte 8)
				   :fill-pointer 0 :adjustable t)))
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar #'(lambda (tn)
			`(let ((tn ,tn))
			   (write-var-integer
			    (make-sc-offset (sc-number (tn-sc tn))
			     ;; tn-offset is zero for constant tns.
			     (or (tn-offset tn) 0))
			    ,vector)))
		    values)
	  (let ((,length (length ,vector)))
	    (inst byte ,length)
	    (dotimes (i ,length)
	      (inst byte (aref ,vector i)))))))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     ,@(emit-error-break vop cerror-trap error-code values)
     (inst jmp ,label)))

(defmacro generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,vop ,error-code ,@values)
       start-lab)))

(defmacro generate-cerror-code (vop error-code &rest values)
  "Generate-CError-Code Error-code Value*
  Emit code for a continuable error with the specified Error-Code and
  context Values.  If the error is continued, execution resumes after
  the GENERATE-CERROR-CODE form."
  (let ((continue (gensym "CONTINUE-LABEL-"))
	(error (gensym "ERROR-LABEL-")))
    `(let ((,continue (gen-label))
	   (,error (gen-label)))
       (emit-label ,continue)
       (assemble (*elsewhere*)
	 (emit-label ,error)
	 (cerror-call ,vop ,continue ,error-code ,@values))
       ,error)))



;;;; PSEUDO-ATOMIC.

(defparameter *enable-pseudo-atomic* nil) ; remember to turn this back on

;;; PSEUDO-ATOMIC -- Internal Interface.
;;;
(defmacro pseudo-atomic (&rest forms)
  (let ((label (gensym "LABEL-")))
    `(let ((,label (gen-label)))
      (when *enable-pseudo-atomic*
	(inst mov (make-ea :byte :disp (+ nil-value
					  (static-symbol-offset
					   'lisp::*pseudo-atomic-interrupted*)
					  (ash symbol-value-slot word-shift)
					  (- other-pointer-type)))
	      0)
	(inst mov (make-ea :byte :disp (+ nil-value
					  (static-symbol-offset
					   'lisp::*pseudo-atomic-atomic*)
					  (ash symbol-value-slot word-shift)
					  (- other-pointer-type)))
	      (fixnumize 1)))
      ,@forms
      (when *enable-pseudo-atomic*
	(inst mov (make-ea :byte :disp (+ nil-value
					  (static-symbol-offset
					   'lisp::*pseudo-atomic-atomic*)
					  (ash symbol-value-slot word-shift)
					  (- other-pointer-type)))
	      0)
	(inst cmp (make-ea :byte
			   :disp (+ nil-value
				    (static-symbol-offset
				     'lisp::*pseudo-atomic-interrupted*)
				    (ash symbol-value-slot word-shift)
				    (- other-pointer-type)))
	      0)
	(inst jmp :eq ,label)
	(inst break pending-interrupt-trap)
	(emit-label ,label)))))


;;;; Indexed references:

(defmacro define-full-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3			; pw was 5
	 (inst mov value (make-ea :qword :base object :index index :scale 2
				  :disp (- (* ,offset word-bytes) ,lowtag)))))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 2			; pw was 5
	 (inst mov value (make-ea :qword :base object
				  :disp (- (* (+ ,offset index) word-bytes)
					   ,lowtag)))))))

(defmacro define-full-setter (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg))
	      (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4			; was 5
	 (inst mov (make-ea :qword :base object :index index :scale 2
			    :disp (- (* ,offset word-bytes) ,lowtag))
	       value)
	 (move result value)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (value :scs ,scs :target result))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)) ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 3			; was 5
	 (inst mov (make-ea :qword :base object
			    :disp (- (* (+ ,offset index) word-bytes) ,lowtag))
	       value)
	 (move result value)))))

(defmacro define-half-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3			; pw was 5
	;; high 32 bits of doubleword operands are zero
	;; extended to 64 bits
	 (inst mov (64-bit-to-32-bit-tn value)
	       (make-ea :dword :base object :index index
				  :disp (- (* ,offset word-bytes) ,lowtag)))))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 2			; pw was 5
	 (inst mov (64-bit-to-32-bit-tn value)
	       (make-ea :dword :base object
				  :disp (- (+ (* ,offset word-bytes) (* index 4))
					   ,lowtag)))))))

(defmacro define-half-setter (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg))
	      (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4			; was 5
	 (inst mov (make-ea :dword :base object :index index
			    :disp (- (* ,offset word-bytes) ,lowtag))
	       (64-bit-to-32-bit-tn value))
	 (move result value)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (value :scs ,scs :target result))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)) ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 3			; was 5
	 (inst mov (make-ea :dword :base object
			    :disp (- (+ (* index 4) (* ,offset word-bytes)) ,lowtag))
	       (64-bit-to-32-bit-tn value))
	 (move result value)))))

(defmacro define-full-conditional-setter (name type offset lowtag scs el-type
					  &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg) :to :result)
	      (index :scs (any-reg) :to :result)
	      (old-value :scs ,scs :target rax)
	      (new-value :scs ,scs))
       (:arg-types ,type tagged-num ,el-type ,el-type)
       (:temporary (:sc ,(first scs) :offset rax-offset
		    :from (:argument 2) :to :result :target result) rax)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:guard (backend-featurep :i486))
       (:generator 5
	 (move rax old-value)
	 (inst cmpxchg (make-ea :qword :base object :index index :scale 1
				:disp (- (* ,offset word-bytes) ,lowtag))
	       new-value)
	 (move result rax)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg) :to :result)
	      (old-value :scs ,scs :target rax)
	      (new-value :scs ,scs))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)) ,el-type ,el-type)
       (:temporary (:sc ,(first scs) :offset rax-offset
		    :from (:argument 1) :to :result :target result)  rax)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:guard (backend-featurep :i486))
       (:generator 4
	 (move rax old-value)
	 (inst cmpxchg (make-ea :qword :base object
				:disp (- (* (+ ,offset index) word-bytes)
					 ,lowtag))
	       new-value)
	 (move result rax)))))
