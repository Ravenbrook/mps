;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/macros.lisp,v 1.34 2006/06/30 18:41:32 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/sparc/macros.lisp,v 1.34 2006/06/30 18:41:32 rtoy Exp $
;;;
;;; This file contains various useful macros for generating SPARC code.
;;;
;;; Written by William Lott.
;;; 

(in-package "SPARC")


;;; Instruction-like macros.

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst move ,n-dst ,n-src))))

;; (loadw object base &optional (offset 0) (lowtag 0) temp)
;;
;; Load a word at a given address into the register OBJECT. The
;; address of the word is in register BASE, plus an offset given by
;; OFFSET, which is in words.  LOWTAG is an adjustment to OFFSET to
;; account for any tag bits used in the BASE descriptor register.
;;
;; In some situations, the offset may be so large that it cannot fit
;; into the offset field of the LD instruction (a 13-bit signed
;; quantity).  In this situation, the TEMP non-descriptor register, if
;; supplied, is used to compute the correct offset.  If TEMP is not
;; given, the offset is assumed to fit.  (TEMP must be a
;; non-descriptor because we store random values into it.  If OBJECT
;; were always a non-descriptor, we wouldn't need the TEMP register.)
;;
;; Samething for storew, except we store OBJECT at the given address.
(macrolet
    ((frob (op inst shift)
     `(defmacro ,op (object base &optional (offset 0) (lowtag 0) temp)
       (if temp
	   (let ((offs (gensym)))
	     `(let ((,offs (- (ash ,offset ,',shift) ,lowtag)))
	       (if (typep ,offs '(signed-byte 13))
		   (inst ,',inst ,object ,base ,offs)
		   (progn
		     (inst li ,temp ,offs)
		     (inst ,',inst ,object ,base ,temp)))))
	   `(inst ,',inst ,object ,base (- (ash ,offset ,',shift) ,lowtag))))))
  (frob loadw ldn word-shift)
  #+(and sparc-v9 sparc-v8plus)
  (frob loadsw ldsw word-shift)
  (frob storew stn word-shift))

#+(and sparc-v9 sparc-v8plus)
(macrolet
    ((frob (op inst shift)
     `(defmacro ,op (object base &optional (offset 0) (lowtag 0) temp)
       (if temp
	   (let ((offs (gensym)))
	     `(let ((,offs (- (ash ,offset ,',shift) ,lowtag)))
	       (if (typep ,offs '(signed-byte 13))
		   (inst ,',inst ,object ,base ,offs)
		   (progn
		     (inst li ,temp ,offs)
		     (inst ,',inst ,object ,base ,temp)))))
	   `(inst ,',inst ,object ,base (- (ash ,offset ,',shift) ,lowtag))))))
  (frob load64 ldx (* 2 word-shift))
  (frob store64 stx (* 2 word-shift)))

(defmacro load-symbol (reg symbol)
  `(inst add ,reg null-tn (static-symbol-offset ,symbol)))

(macrolet
    ((frob (slot)
       (let ((loader (intern (concatenate 'simple-string
					  "LOAD-SYMBOL-"
					  (string slot))))
	     (storer (intern (concatenate 'simple-string
					  "STORE-SYMBOL-"
					  (string slot))))
	     (offset (intern (concatenate 'simple-string
					  "SYMBOL-"
					  (string slot)
					  "-SLOT")
			     (find-package "VM"))))
	 `(progn
	    (defmacro ,loader (reg symbol)
	      `(inst ldn ,reg null-tn
		     (+ (static-symbol-offset ',symbol)
			(ash ,',offset word-shift)
			(- other-pointer-type))))
	    (defmacro ,storer (reg symbol)
	      `(inst stn ,reg null-tn
		     (+ (static-symbol-offset ',symbol)
			(ash ,',offset word-shift)
			(- other-pointer-type))))))))
  (frob value)
  (frob function))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase (backend-byte-order *target-backend*)
      (:little-endian
       `(inst ldub ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst ldub ,n-target ,n-source (+ ,n-offset 3))))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

(defmacro lisp-jump (function)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst j ,function
	   (- (ash function-code-offset word-shift) vm:function-pointer-type))
     (move code-tn ,function)))

(defmacro lisp-return (return-pc &key (offset 0) (frob-code t))
  "Return to RETURN-PC."
  `(progn
     (inst j ,return-pc
	   (- (* (1+ ,offset) word-bytes) other-pointer-type))
     ,(if frob-code
	  `(move code-tn ,return-pc)
	  '(inst nop))))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (align lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; Stack TN's

;;; Load-Stack-TN, Store-Stack-TN  --  Interface
;;;
;;;    Move a stack TN to a register and vice-versa.
;;;
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
	 (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (loadw reg cfp-tn offset 0 gtemp-tn))))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
	 (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (storew reg cfp-tn offset 0 gtemp-tn))))))


;;; MAYBE-LOAD-STACK-TN  --  Interface
;;;
(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
	      (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
	(sc-case ,n-stack
	  ((any-reg descriptor-reg)
	   (move ,n-reg ,n-stack))
	  ((control-stack)
	   (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:

;; Allocation macro
;;
;; This macro does the appropriate stuff to allocate space.
;;
;; The allocated space is stored in RESULT-TN with the lowtag LOWTAG
;; applied.  The amount of space to be allocated is SIZE bytes (which
;; must be a multiple of the lisp object size).
;;
;; If STACK-P is given, then allocation occurs on the control stack
;; (for dynamic-extent).  In this case, you MUST also specify NODE, so
;; that the appropriate compiler policy can be used, and TEMP-TN,
;; which is needed for work-space.  TEMP-TN MUST be a non-descriptor
;; reg.
;;
;; If generational GC is enabled, you MUST supply a value for TEMP-TN
;; because a temp register is needed to do inline allocation.
;; TEMP-TN, in this case, can be any register, since it holds a
;; double-word aligned address (essentially a fixnum).
(defmacro allocation (result-tn size lowtag &key stack-p temp-tn)
  ;; We assume we're in a pseudo-atomic so the pseudo-atomic bit is
  ;; set.
  `(cond (,stack-p
	  ;; Stack allocation
	  ;;
	  ;; The control stack grows up, so round up CSP to a
	  ;; multiple of 8 (lispobj size).  Use that as the
	  ;; allocation pointer.  Then add SIZE bytes to the
	  ;; allocation and set CSP to that, so we have the desired
	  ;; space.

	  ;; Make sure the temp-tn is a non-descriptor register!
	  (assert (and ,temp-tn (sc-is ,temp-tn non-descriptor-reg)))

	  ;; temp-tn is csp-tn rounded up to a multiple of 8 (lispobj size)
	  (inst add ,temp-tn csp-tn vm:lowtag-mask)
	  (inst andn ,temp-tn vm:lowtag-mask)
	  ;; Set the result to temp-tn, with appropriate lowtag
	  (inst or ,result-tn ,temp-tn ,lowtag)

	  ;; Allocate the desired space on the stack.
	  ;;
	  ;; FIXME: Can't allocate on stack if SIZE is too large.
	  ;; Need to rearrange this code.
	  (inst add csp-tn ,temp-tn ,size)
	  )
	 #-gencgc
	 (t
	  ;; Normal allocation to the heap.
	  (if (logbitp (1- lowtag-bits) ,lowtag)
	      (progn
		(inst or ,result-tn alloc-tn ,lowtag)
		(inst add alloc-tn ,size))
	      (progn
		(inst andn ,result-tn alloc-tn lowtag-mask)
		(inst or ,result-tn ,lowtag)
		(inst add alloc-tn ,size))))
	 #+gencgc
	 (t
	  ;; See if we can do an inline allocation.  The updated
	  ;; free pointer should not point past the end of the
	  ;; current region.  If it does, a full alloc needs to be
	  ;; done.
	  (load-symbol-value ,result-tn *current-region-end-addr*)

	  ;; Sometimes the size is an known constant, but won't fit in
	  ;; the immediate field of an instruction.  Hence we have to
	  ;; do this to get it.
	  (cond ((and (tn-p ,temp-tn)
		      (numberp ,size)
		      (not (typep ,size '(signed-byte 13))))
		 (inst li ,temp-tn ,size)
		 (inst add alloc-tn ,temp-tn))
		(t
		 (inst add alloc-tn ,size)))

	  (inst andn ,temp-tn alloc-tn lowtag-mask) ; Zap PA bits
	      
	  ;; temp-tn points to the new end of region.  Did we go
	  ;; past the actual end of the region?  If so, we need a
	  ;; full alloc.
	  (inst cmp ,temp-tn ,result-tn)
	  (without-scheduling ()
	    ;; NOTE: alloc-tn has been updated to point to the new
	    ;; end.  But the allocation routines expect alloc-tn
	    ;; points to original free region.  Thus, the allocation
	    ;; trap handler MUST subtract SIZE from alloc-tn before
	    ;; calling the alloc routine.  This allows for
	    ;; (slightly) faster code for inline allocation.

	    ;; As above, SIZE might not fit in the immediate field of
	    ;; an instruction.  Need to do this complicated thing.
			      
            (cond ((and (tn-p ,temp-tn)
			(numberp ,size)
			(not (typep ,size '(signed-byte 13))))
		   (inst li ,result-tn ,size)
		   (inst sub ,result-tn ,temp-tn ,result-tn))
		  (t
		   (inst sub ,result-tn ,temp-tn ,size)))
	    (inst t :gt allocation-trap))
	  ;; Set lowtag appropriately
	  (inst or ,result-tn ,lowtag))))

(defmacro with-fixed-allocation ((result-tn temp-tn type-code size
					    &key (lowtag other-pointer-type)
					    stack-p)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (temp-tn temp-tn)
	      (type-code type-code) (size size)
	      (lowtag lowtag))
    `(pseudo-atomic ()
       (allocation ,result-tn (pad-data-block ,size) ,lowtag
	           :temp-tn ,temp-tn
	           :stack-p ,stack-p)
      (when ,type-code
	(inst li ,temp-tn (logior (ash (1- ,size) type-bits) ,type-code))
	(storew ,temp-tn ,result-tn 0 ,lowtag))
       ,@body)))


;;;; Type testing noise.

;;; GEN-RANGE-TEST -- internal
;;;
;;; Generate code that branches to TARGET iff REG contains one of VALUES.
;;; If NOT-P is true, invert the test.  Jumping to NOT-TARGET is the same
;;; as falling out the bottom.
;;; 
(defun gen-range-test (reg target not-target not-p min seperation max values)
  (let ((tests nil)
	(start nil)
	(end nil)
	(insts nil))
    (multiple-value-bind (equal less-or-equal greater-or-equal label)
			 (if not-p
			     (values :ne :gt :lt not-target)
			     (values :eq :le :ge target))
      (flet ((emit-test ()
	       (if (= start end)
		   (push start tests)
		   (push (cons start end) tests))))
	(dolist (value values)
	  (cond ((< value min)
		 (error "~S is less than the specified minimum of ~S"
			value min))
		((> value max)
		 (error "~S is greater than the specified maximum of ~S"
			value max))
		((not (zerop (rem (- value min) seperation)))
		 (error "~S isn't an even multiple of ~S from ~S"
			value seperation min))
		((null start)
		 (setf start value))
		((> value (+ end seperation))
		 (emit-test)
		 (setf start value)))
	  (setf end value))
	(emit-test))
      (macrolet ((inst (name &rest args)
		       `(push (list 'inst ',name ,@args) insts)))
	(do ((remaining (nreverse tests) (cdr remaining)))
	    ((null remaining))
	  (let ((test (car remaining))
		(last (null (cdr remaining))))
	    (if (atom test)
		(progn
		  (inst cmp reg test)
		  (if last
		      (inst b equal target)
		      (inst b :eq label)))
		(let ((start (car test))
		      (end (cdr test)))
		  (cond ((and (= start min) (= end max))
			 (warn "The values ~S cover the entire range from ~
			 ~S to ~S [step ~S]."
			       values min max seperation)
			 (push `(unless ,not-p (inst b ,target)) insts))
			((= start min)
			 (inst cmp reg end)
			 (if last
			     (inst b less-or-equal target)
			     (inst b :le label)))
			((= end max)
			 (inst cmp reg start)
			 (if last
			     (inst b greater-or-equal target)
			     (inst b :ge label)))
			(t
			 (inst cmp reg start)
			 (inst b :lt (if not-p target not-target))
			 (inst cmp reg end)
			 (if last
			     (inst b less-or-equal target)
			     (inst b :le label))))))))))
    (nreverse insts)))

(defun gen-other-immediate-test (reg target not-target not-p values)
  (gen-range-test reg target not-target not-p
		  (+ other-immediate-0-type lowtag-limit)
		  (- other-immediate-1-type other-immediate-0-type)
		  (ash 1 type-bits)
		  values))


(defun test-type-aux (reg temp target not-target not-p lowtags immed hdrs
			  function-p)
  (let* ((fixnump (and (member even-fixnum-type lowtags :test #'eql)
		       (member odd-fixnum-type lowtags :test #'eql)))
	 (lowtags (sort (if fixnump
			    (delete even-fixnum-type
				    (remove odd-fixnum-type lowtags
					    :test #'eql)
				    :test #'eql)
			    (copy-list lowtags))
			#'<))
	 (lowtag (if function-p
		     vm:function-pointer-type
		     vm:other-pointer-type))
	 (hdrs (sort (copy-list hdrs) #'<))
	 (immed (sort (copy-list immed) #'<)))
    (append
     (when immed
       `((inst and ,temp ,reg type-mask)
	 ,@(if (or fixnump lowtags hdrs)
	       (let ((fall-through (gensym)))
		 `((let (,fall-through (gen-label))
		     ,@(gen-other-immediate-test
			temp (if not-p not-target target)
			fall-through nil immed)
		     (emit-label ,fall-through))))
	       (gen-other-immediate-test temp target not-target not-p immed))))
     (when fixnump
       `((inst andcc zero-tn ,reg fixnum-tag-mask)
	 ,(if (or lowtags hdrs)
	      `(inst b :eq ,(if not-p not-target target)
		     #+sparc-v9 ,(if not-p :pn :pt))
	      `(inst b ,(if not-p :ne :eq) ,target
		       #+sparc-v9 ,(if not-p :pn :pt)))))
     (when (or lowtags hdrs)
       `((inst and ,temp ,reg lowtag-mask)))
     (when lowtags
       (if hdrs
	   (let ((fall-through (gensym)))
	     `((let ((,fall-through (gen-label)))
		 ,@(gen-range-test temp (if not-p not-target target)
				   fall-through nil
				   0 1 (1- lowtag-limit) lowtags)
		 (emit-label ,fall-through))))
	   (gen-range-test temp target not-target not-p 0 1
			   (1- lowtag-limit) lowtags)))
     (when hdrs
       `((inst cmp ,temp ,lowtag)
	 (inst b :ne ,(if not-p target not-target)
	         #+sparc-v9 ,(if not-p :pn :pt))
	 (inst nop)
	 (load-type ,temp ,reg (- ,lowtag))
	 ,@(gen-other-immediate-test temp target not-target not-p hdrs))))))

(defconstant immediate-types
  (list base-char-type unbound-marker-type))

(defconstant function-subtypes
  (list funcallable-instance-header-type
	#-double-double dylan-function-header-type
	function-header-type closure-function-header-type
	closure-header-type))

(defmacro test-type (register temp target not-p &rest type-codes)
  (let* ((type-codes (mapcar #'eval type-codes))
	 (lowtags (remove lowtag-limit type-codes :test #'<))
	 (extended (remove lowtag-limit type-codes :test #'>))
	 (immediates (intersection extended immediate-types :test #'eql))
	 (headers (set-difference extended immediate-types :test #'eql))
	 (function-p nil))
    (unless type-codes
      (error "Must supply at least on type for test-type."))
    (when (and headers (member other-pointer-type lowtags))
      (warn "OTHER-POINTER-TYPE supersedes the use of ~S" headers)
      (setf headers nil))
    (when (and immediates
	       (or (member other-immediate-0-type lowtags)
		   (member other-immediate-1-type lowtags)))
      (warn "OTHER-IMMEDIATE-n-TYPE supersedes the use of ~S" immediates)
      (setf immediates nil))
    (when (intersection headers function-subtypes)
      (unless (subsetp headers function-subtypes)
	(error "Can't test for mix of function subtypes and normal ~
		header types."))
      (setq function-p t))
      
    (let ((n-reg (gensym))
	  (n-temp (gensym))
	  (n-target (gensym))
	  (not-target (gensym)))
      `(let ((,n-reg ,register)
	     (,n-temp ,temp)
	     (,n-target ,target)
	     (,not-target (gen-label)))
	 (declare (ignorable ,n-temp))
	 ,@(if (constantp not-p)
	       (test-type-aux n-reg n-temp n-target not-target
			      (eval not-p) lowtags immediates headers
			      function-p)
	       `((cond (,not-p
			,@(test-type-aux n-reg n-temp n-target not-target t
					 lowtags immediates headers
					 function-p))
		       (t
			,@(test-type-aux n-reg n-temp n-target not-target nil
					 lowtags immediates headers
					 function-p)))))
	 (inst nop)
	 (emit-label ,not-target)))))


;;;; Error Code

(defvar *adjustable-vectors* nil)

(defmacro with-adjustable-vector ((var) &rest body)
  `(let ((,var (or (pop *adjustable-vectors*)
		   (make-array 16
			       :element-type '(unsigned-byte 8)
			       :fill-pointer 0
			       :adjustable t))))
     (setf (fill-pointer ,var) 0)
     (unwind-protect
	 (progn
	   ,@body)
       (push ,var *adjustable-vectors*))))

(eval-when (compile load eval)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym)))
      `((let ((vop ,vop))
	  (when vop
	    (note-this-location vop :internal-error)))
	(inst unimp ,kind)
	(with-adjustable-vector (,vector)
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar #'(lambda (tn)
			`(let ((tn ,tn))
			   (write-var-integer (make-sc-offset (sc-number
							       (tn-sc tn))
							      (tn-offset tn))
					      ,vector)))
		    values)
	  (inst byte (length ,vector))
	  (dotimes (i (length ,vector))
	    (inst byte (aref ,vector i))))
	(align word-shift)))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     (inst b ,label)
     ,@(emit-error-break vop cerror-trap error-code values)))

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
    `(let ((,continue (gen-label)))
       (emit-label ,continue)
       (assemble (*elsewhere*)
	 (let ((,error (gen-label)))
	   (emit-label ,error)
	   (cerror-call ,vop ,continue ,error-code ,@values)
	   ,error)))))



;;; PSEUDO-ATOMIC -- Handy macro for making sequences look atomic.
;;;
(defmacro pseudo-atomic ((&key (extra 0)) &rest forms)
  (declare (ignore extra))
  `(progn
     ;; Set the pseudo-atomic flag
     (without-scheduling ()
       (inst or alloc-tn pseudo-atomic-value))
     ,@forms
     ;; Reset the pseudo-atomic flag
     (without-scheduling ()
       ;; Remove the pseudo-atomic flag.  (Could do subtraction here,
       ;; but the disassembler prints some notes based on the add
       ;; instruction.)
       (inst andn alloc-tn pseudo-atomic-value)
       ;; Check to see if pseudo-atomic interrupted flag is set (bit 0 = 1)
       (inst andcc zero-tn alloc-tn pseudo-atomic-interrupted-value)
       ;; The C code needs to process this correctly and fixup alloc-tn.
       (inst t :ne pseudo-atomic-trap)
       )))
