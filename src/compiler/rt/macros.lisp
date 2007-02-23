;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/macros.lisp,v 1.11 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains various useful macros for generating code for the IBM
;;; RT.
;;;
;;; Written by William Lott, Christopher Hoover, and Rob Maclachlin, and Bill
;;; Chiles.
;;;

(in-package "RT")



;;; Instruction-like macros.

;;; MOVE -- Interface.
;;;
(defmacro move (dst src)
  "Move src into dst unless they are LOCATION=."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst move ,n-dst ,n-src))))

;;; LOADW and STOREW -- Interface.
;;;
;;; Load and store words.
;;;
(macrolet ((def-mem-op (op inst)
	    `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
	       `(inst ,',inst ,object ,base
		      (- (ash ,offset word-shift) ,lowtag)))))
  (def-mem-op loadw l)
  (def-mem-op storew st))

;;; LOAD-SYMBOL -- Interface.
;;;
(defmacro load-symbol (reg symbol)
  "Load a pointer to the static symbol into reg."
  `(inst a ,reg null-tn (static-symbol-offset ,symbol)))

;;; LOAD-SYMBOL-FUNCTION, STORE-SYMBOL-FUNCTION,
;;; LOAD-SYMBOL-VALUE, STORE-SYMBOL-VALUE		-- interface.
;;;
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
	      `(inst l ,reg null-tn
		     (+ (static-symbol-offset ',symbol)
			(ash ,',offset word-shift)
			(- other-pointer-type))))
	    (defmacro ,storer (reg symbol)
	      `(inst st ,reg null-tn
		     (+ (static-symbol-offset ',symbol)
			(ash ,',offset word-shift)
			(- other-pointer-type))))))))
  (frob value)
  (frob function))

;;; LOAD-TYPE -- Interface.
;;;
;;; Subtract the low-tag from the pointer and add one less than the address
;;; units per word to get us to the low byte since the RT is big-endian.
;;;
(defmacro load-type (target source &optional (low-tag 0))
  "Loads the type bits from the source pointer into target, where pointer has
   the specified low-tag."
  `(inst lc ,target ,source (- word-bytes 1 ,low-tag)))



;;;; Call and Return.

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions due to low-tag bits on pointers.
;;;

;;; LISP-JUMP -- Interface.
;;;
(defmacro lisp-jump (function lip)
  "Jump to the lisp function.  Lip is the lisp interior pointer register
   used as a temporary."
  `(progn
     (inst cal ,lip ,function (- (ash function-header-code-offset
				      word-shift)
				 function-pointer-type))
     (inst bx ,lip)
     (move code-tn ,function)))

;;; LISP-RETURN -- Interface.
;;;
(defmacro lisp-return (return-pc lip &key (offset 0) (frob-code t))
  "Return to return-pc, an LRA.  Lip is the lisp interior pointer register
   used as a temporary.  Offset is the number of words to skip at the target,
   and it has to be small enough for whatever instruction used in here."
  `(progn
     (inst cal ,lip ,return-pc
	   (- (ash (1+ ,offset) word-shift) other-pointer-type))
     ,@(if frob-code
	  `((inst bx ,lip)
	    (move code-tn ,return-pc))
	  `((inst b ,lip)))))

;;; EMIT-RETURN-PC -- Interface.
;;;
(defmacro emit-return-pc (label)
  "Emit a return-pc header word, making sure it is aligned properly for our
   low-tag bits since there are other-pointers referring to the LRA's.  label
   is the label to use for this return-pc."
  `(progn
     (align lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))


;;;; Stack TN's.

;;; LOAD-STACK-TN, STORE-STACK-TN  --  Interface.
;;;
;;; Move a stack TN to a register and vice-versa.  If the VOP is supplied,
;;; we can load from (or store onto) the number stack also.
;;;
(defmacro load-stack-tn (reg stack &optional vop)
  (once-only ((n-reg reg)
	      (n-stack stack))
    `(sc-case ,n-stack
       (control-stack
	(loadw ,n-reg cfp-tn (tn-offset ,n-stack)))
       ,@(when vop
	   `(((unsigned-stack signed-stack base-char-stack sap-stack)
	      (loadw ,n-reg (current-nfp-tn ,vop) (tn-offset ,n-stack))))))))
;;;
(defmacro store-stack-tn (reg stack &optional vop)
  (once-only ((n-reg reg)
	      (n-stack stack))
    `(sc-case ,n-stack
       (control-stack
	(storew ,n-reg cfp-tn (tn-offset ,n-stack)))
       ,@(when vop
	   `(((unsigned-stack signed-stack base-char-stack sap-stack)
	      (storew ,n-reg (current-nfp-tn ,vop) (tn-offset ,n-stack))))))))


;;; MAYBE-LOAD-STACK-TN  --  Interface.
;;;
(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
	      (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg non-descriptor-reg word-pointer-reg)
	(sc-case ,n-stack
	  ((any-reg descriptor-reg non-descriptor-reg word-pointer-reg)
	   (move ,n-reg ,n-stack))
	  ((control-stack)
	   (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))



;;;; Storage allocation:

;;; WITH-FIXED-ALLOCATION -- Internal Interface.
;;;
(defmacro with-fixed-allocation ((result-tn header-tn alloc-tn type-code size)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
   word header having the specified Type-Code.  The result is placed in
   Result-TN.  Header-Tn is any register temp which this uses to hold the
   header of the allocated object before storing it into the header slot.
   Alloc-tn is a register to hold the heap pointer, and it can be any register
   since raw heap pointers look like fixnums (dual-word aligned objects).  The
   body is placed inside PSEUDO-ATOMIC, and presumably initializes the object."
  `(progn
     (pseudo-atomic (,header-tn)
       (load-symbol-value ,alloc-tn *allocation-pointer*)
       ;; Take free pointer and make descriptor pointer.
       ;; Just add in three low-tag bits since alloc ptr is dual-word aligned.
       (inst cal ,result-tn ,alloc-tn other-pointer-type)
       (inst cal ,alloc-tn ,alloc-tn (pad-data-block ,size))
       (store-symbol-value ,alloc-tn *allocation-pointer*)
       (inst li ,header-tn (logior (ash (1- ,size) type-bits) ,type-code))
       (storew ,header-tn ,result-tn 0 other-pointer-type)
       ,@body)
     (load-symbol-value ,header-tn *internal-gc-trigger*)
     (inst tlt ,header-tn ,alloc-tn)))



;;;; Type testing noise.

;;; GEN-RANGE-TEST -- Internal.
;;;
;;; Generate code that branches to TARGET iff REG contains one of VALUES.  If
;;; NOT-P is true, invert the test.  Jumping to DROP-THROUGH is the same as
;;; falling out the bottom.
;;;
(defun gen-range-test (reg target drop-through not-p values
		       &optional (separation 1) (min 0))
  (let ((tests nil)
	(start nil)
	(end nil)
	(insts nil))
    ;; Block off list of values into ranges, so we can test these as intervals
    ;; instead of individually.
    (flet ((emit-test ()
		      (if (= start end)
			  (push start tests)
			  (push (cons start end) tests))))
      (dolist (value values)
	(cond ((< value min)
	       (error "~S is less than the specified minimum of ~S"
		      value min))
	      ((null start)
	       (setf start value))
	      ((> value (+ end separation))
	       (emit-test)
	       (setf start value)))
	(setf end value))
      (emit-test))
    ;; Output tests, dealing with not-p.
    (macrolet ((inst (name &rest args)
		     `(push (list 'inst ',name ,@args) insts)))
      (do ((remaining (nreverse tests) (cdr remaining)))
	  ((null remaining))
	(let ((test (car remaining))
	      (last (null (cdr remaining))))
	  (cond
	   ((atom test)
	    (inst c reg test)
	    (if last
		(if not-p
		    ;; We compared for the thing.  If not-p, since it is the
		    ;; last thing to look for, and we don't want it, then goto
		    ;; target.
		    (inst bnc :eq target)
		    ;; If we do want the thing, and we got it, then goto
		    ;; target.
		    (inst bc :eq target))
		;; If it is not the last thing, and it is not not-p, and we
		;; have it, then go to target.  If not-p, and we have the thing
		;; we don't want, then drop through.
		(inst bc :eq (if not-p drop-through target))))
	   (t
	    (let ((start (car test))
		  (end (cdr test)))
	      ;; We don't need this code if start is the smallest value we
	      ;; could possibly have.  We know reg can't be less than start.
	      (unless (= start min)
		(inst c reg start)
		;; If I want the range, and I'm less than the start, then
		;; drop through.  If I don't want the range, goto target
		;; because I'm not in the range.
		(inst bc :lt (if not-p target drop-through)))
	      ;; We know reg is greater than or equal to start, so see if it
	      ;; is less than or equal to end.
	      (inst c reg end)
	      (if last
		  (if not-p
		      ;; If this range is the last test, and I don't
		      ;; want it, and I'm not in it, then goto target.
		      (inst bc :gt target)
		      ;; If this range is the last test, and I want the
		      ;; range, and I'm in it, then goto target.
		      (inst bnc :gt target))
		  (inst bnc :gt (if not-p drop-through target)))))))))
    (nreverse insts)))

(defconstant type-separation 4)
(defconstant min-type (+ other-immediate-0-type lowtag-limit))

;;; TEST-TYPE-AUX -- Internal.
;;;
(defun test-type-aux (reg temp target drop-through not-p lowtags immed hdrs
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
       `((inst nilz ,temp ,reg type-mask)
	 ,@(if (or fixnump lowtags hdrs)
	       ;; If we're going to output any tests below, fall through to
	       ;; those tests from these.
	       (let ((fall-through (gensym)))
		 `((let (,fall-through (gen-label))
		     ,@(gen-range-test
			temp (if not-p drop-through target)
			fall-through nil immed type-separation)
		     (emit-label ,fall-through))))
	       ;; If there are no other tests, drop through to the end of all
	       ;; these tests.
	       (gen-range-test temp target drop-through not-p
			       immed type-separation))))
     (when fixnump
       `((inst nilz ,temp ,reg 3)
	 ,(if (or lowtags hdrs)
	      ;; If more tests follow this one, fall through to them.
	      `(inst bc :eq ,(if not-p drop-through target))
	      ;; If no more tests follow, drop totally out of here.
	      `(inst ,(if not-p 'bnc 'bc) :eq ,target))))
     (when (or lowtags hdrs)
       `((inst nilz ,temp ,reg lowtag-mask)))
     (when lowtags
       (if hdrs
	   ;; If we're going to output any tests below, fall through to
	   ;; those tests from these.
	   (let ((fall-through (gensym)))
	     `((let ((,fall-through (gen-label)))
		 ,@(gen-range-test temp (if not-p drop-through target)
				   fall-through nil lowtags)
		 (emit-label ,fall-through))))
	   (gen-range-test temp target drop-through not-p lowtags)))
     (when hdrs
       `((inst c ,temp ,lowtag)
	 (inst bnc :eq ,(if not-p target drop-through))
	 (load-type ,temp ,reg ,lowtag)
	 ,@(gen-range-test temp target drop-through not-p
			   hdrs type-separation min-type))))))

(defconstant immediate-types
  (list base-char-type unbound-marker-type))

(defconstant function-subtypes
  (list funcallable-instance-header-type closure-header-type
	function-header-type closure-function-header-type))

;;; TEST-TYPE -- Interface.
;;;
;;; This is used in type-vops.lisp.
;;;
(defmacro test-type (register temp target not-p &rest type-codes)
  "Register holds a descriptor object that might have one of the types in
   type-codes.  If it does, goto target; otherwise, fall through.  If not-p is
   set, goto target when register does not hold one of the types.  Type-codes
   must be compile-time constants that this evaluates to get numeric codes.
   Temp is a non-descriptor temporary needed by the code returned by the
   macro."
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
	  (drop-through (gensym)))
      `(let ((,n-reg ,register)
	     (,n-temp ,temp)
	     (,n-target ,target)
	     (,drop-through (gen-label)))
	 (declare (ignorable ,n-temp))
	 ,@(if (constantp not-p)
	       (test-type-aux n-reg n-temp n-target drop-through
			      (eval not-p) lowtags immediates headers
			      function-p)
	       `((cond (,not-p
			,@(test-type-aux n-reg n-temp n-target drop-through t
					 lowtags immediates headers
					 function-p))
		       (t
			,@(test-type-aux n-reg n-temp n-target drop-through nil
					 lowtags immediates headers
					 function-p)))))
	 (emit-label ,drop-through)))))



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
	(inst break ,kind)
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


;;; CERROR-CALL -- Internal Interface.
;;;
;;; Output the error break stuff followed by a branch to the continuable code.
;;; The break handler can skip the error break data, setting the pc in the
;;; signal context to our following branch, and if we continue, we're setup.
;;;
(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     ,@(emit-error-break vop cerror-trap error-code values)
     (inst b ,label)))

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



;;;; PSEUDO-ATOMIC.

;;; PSEUDO-ATOMIC -- Internal Interface.
;;;
(defmacro pseudo-atomic ((ndescr-temp) &rest forms)
  (let ((label (gensym "LABEL-")))
    `(let ((,label (gen-label)))
       (inst li ,ndescr-temp 0)
       (store-symbol-value ,ndescr-temp lisp::*pseudo-atomic-interrupted*)
       ;; Note: we just use cfp as some not-zero value.
       (store-symbol-value cfp-tn lisp::*pseudo-atomic-atomic*)
       ,@forms
       (inst li ,ndescr-temp 0)
       (store-symbol-value ,ndescr-temp lisp::*pseudo-atomic-atomic*)
       (load-symbol-value ,ndescr-temp lisp::*pseudo-atomic-interrupted*)
       (inst c ,ndescr-temp 0)
       (inst bc :eq ,label)
       (inst break pending-interrupt-trap)
       (emit-label ,label))))

;;;; Float stuff:
;;;
;;;    Since moving between memory and a FP register reqires *two* temporaries,
;;; we need a special temporary to form the magic address we store to do a
;;; floating point operation.  We get this temp by always spilling NL0 on the
;;; number stack.  This appears rather grody, but actually the 68881 is so slow
;;; compared to the ROMP that this overhead is not very great.
;;;
;;; Note: The RT interrupt handler preserves 64 bytes beyond the current stack
;;; pointer, so we don't need to dink the stack pointer.  We can just use the
;;; space beyond it.
;;;
;;; We also use LIP to form the address of the data location that we are
;;; reading or writing.

(defvar *in-with-fp-temp* nil)

(defmacro with-fp-temp ((var) &body body)
  `(if *in-with-fp-temp*
       (error "Can only have one FP temp.")
       (let ((,var nl0-tn)
	     (*in-with-fp-temp* t))
	 (storew ,var nsp-tn -1)
	 (multiple-value-prog1 (progn ,@body)
	   (loadw ,var nsp-tn -1)))))
