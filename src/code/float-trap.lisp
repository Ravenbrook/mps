;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/float-trap.lisp,v 1.26 2005/11/09 18:26:25 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains stuff for controlling floating point traps.  It is
;;; IEEE float specific, but should work for pretty much any FPU where the
;;; state fits in one word and exceptions are represented by bits being set.
;;;
;;; Author: Rob MacLachlan
;;; 
(in-package "VM")
(export '(current-float-trap floating-point-modes sigfpe-handler))
(in-package "EXTENSIONS")
(export '(set-floating-point-modes get-floating-point-modes
	  with-float-traps-masked))
(in-package "VM")

(eval-when (compile load eval)

(defconstant float-trap-alist
  (list (cons :underflow float-underflow-trap-bit)
	(cons :overflow float-overflow-trap-bit)
	(cons :inexact float-inexact-trap-bit)
	(cons :invalid float-invalid-trap-bit)
	(cons :divide-by-zero float-divide-by-zero-trap-bit)
	#+x86 (cons :denormalized-operand float-denormal-trap-bit)))

;;; FLOAT-TRAP-MASK  --  Internal
;;;
;;;    Return a mask with all the specified float trap bits set.
;;;
(defun float-trap-mask (names)
  (reduce #'logior
	  (mapcar #'(lambda (x)
		      (or (cdr (assoc x float-trap-alist))
			  (error "Unknown float trap kind: ~S." x)))
		  names)))

(defconstant rounding-mode-alist
  (list (cons :nearest float-round-to-nearest)
	(cons :zero float-round-to-zero)
	(cons :positive-infinity float-round-to-positive)
	(cons :negative-infinity float-round-to-negative)))

#+x86
(defconstant precision-control-alist
  (list (cons :24-bit float-precision-24-bit)
	(cons :53-bit float-precision-53-bit)
	(cons :64-bit float-precision-64-bit)))
  
); Eval-When (Compile Load Eval)


;;; Interpreter stubs.
;;;
(defun floating-point-modes () (floating-point-modes))
(defun (setf floating-point-modes) (new) (setf (floating-point-modes) new))


;;; SET-FLOATING-POINT-MODES  --  Public
;;;
(defun set-floating-point-modes (&key (traps nil traps-p)
				      (rounding-mode nil round-p)
				      (current-exceptions nil current-x-p)
				      (accrued-exceptions nil accrued-x-p)
				      (fast-mode nil fast-mode-p)
				      (precision-control nil precision-control-p))
  "This function sets options controlling the floating-point hardware.  If a
  keyword is not supplied, then the current value is preserved.  Possible
  keywords:

   :TRAPS
       A list of the exception conditions that should cause traps.  Possible
       exceptions are :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID,
       :DIVIDE-BY-ZERO, and on the X86 :DENORMALIZED-OPERAND. Initially
       all traps except :INEXACT are enabled.

   :ROUNDING-MODE
       The rounding mode to use when the result is not exact.  Possible values
       are :NEAREST, :POSITIVE-INFINITY, :NEGATIVE-INFINITY and :ZERO.
       Initially, the rounding mode is :NEAREST.

   :CURRENT-EXCEPTIONS
   :ACCRUED-EXCEPTIONS
       These arguments allow setting of the exception flags.  The main use is
       setting the accrued exceptions to NIL to clear them.

   :FAST-MODE
       Set the hardware's \"fast mode\" flag, if any.  When set, IEEE
       conformance or debuggability may be impaired.  Some machines may not
       have this feature, in which case the value is always NIL.

   :PRECISION-CONTROL
       On the x86 architecture, you can set the precision of the arithmetic
       to :24-BIT, :53-BIT, or :64-BIT mode, corresponding to IEEE single
       precision, double precision, and extended double precision.

   GET-FLOATING-POINT-MODES may be used to find the floating point modes
   currently in effect."
  #-x86
  (declare (ignore precision-control))
  (let ((modes (floating-point-modes)))
    (when traps-p
      (setf (ldb float-traps-byte modes) (float-trap-mask traps)))
    (when round-p
      (setf (ldb float-rounding-mode modes)
	    (or (cdr (assoc rounding-mode rounding-mode-alist))
		(error "Unknown rounding mode: ~S." rounding-mode))))
    (when current-x-p
      (setf (ldb float-exceptions-byte modes)
	    (float-trap-mask current-exceptions)))
    (when accrued-x-p
      (setf (ldb float-sticky-bits modes)
	    (float-trap-mask accrued-exceptions)))
    (when fast-mode-p
      (if fast-mode
	  (setq modes (logior float-fast-bit modes))
	  (setq modes (logand (lognot float-fast-bit) modes))))
    #+x86
    (when precision-control-p
      (setf (ldb float-precision-control modes)
	    (or (cdr (assoc precision-control precision-control-alist))
		(error "Unknown precision mode: ~S." precision-control))))
    #-x86
    (when precision-control-p
      (warn "Precision control only available for x86"))
    
    ;; Temporarily disabled on darwin due to strange breakage
    #-darwin
    (setf (floating-point-modes) modes))
    
  (values))


;;; GET-FLOATING-POINT-MODES  --  Public
;;;
(defun get-floating-point-modes ()
  "This function returns a list representing the state of the floating point
  modes.  The list is in the same format as the keyword arguments to
  SET-FLOATING-POINT-MODES, i.e. 
      (apply #'set-floating-point-modes (get-floating-point-modes))

  sets the floating point modes to their current values (and thus is a no-op)."
  (flet ((exc-keys (bits)
	   (macrolet ((frob ()
			`(collect ((res))
			   ,@(mapcar #'(lambda (x)
					 `(when (logtest bits ,(cdr x))
					    (res ',(car x))))
				     float-trap-alist)
			   (res))))
	     (frob))))
    (let ((modes (floating-point-modes))) 
      `(:traps ,(exc-keys (ldb float-traps-byte modes))
	:rounding-mode ,(car (rassoc (ldb float-rounding-mode modes)
				     rounding-mode-alist))
	:current-exceptions ,(exc-keys (ldb float-exceptions-byte modes))
	:accrued-exceptions ,(exc-keys (ldb float-sticky-bits modes))
	:fast-mode ,(logtest float-fast-bit modes)
	#+x86
	:precision-control
	#+x86
	,(car (rassoc (ldb float-precision-control modes)
		      precision-control-alist))
	))))

  
;;; CURRENT-FLOAT-TRAP  --  Interface
;;;
(defmacro current-float-trap (&rest traps)
  "Current-Float-Trap Trap-Name*
  Return true if any of the named traps are currently trapped, false
  otherwise."
  `(not (zerop (logand ,(dpb (float-trap-mask traps) float-traps-byte 0)
		       (floating-point-modes)))))


;;; SIGFPE-HANDLER  --  Interface
;;;
;;;    Signal the appropriate condition when we get a floating-point error.
;;;

#+(and bsd (not freebsd))
(define-condition floating-point-exception (arithmetic-error)
  ((flags :initarg :traps
	  :reader floating-point-exception-traps))
  (:report (lambda (condition stream)
	     (format stream "Arithmetic error ~S signalled.~%"
		     (type-of condition))
	     (let ((traps (floating-point-exception-traps condition)))
	       (if traps
		   (format stream
			   "Trapping conditions are: ~%~{ ~s~^~}~%"
			   traps)
		   (write-line
		    "No traps are enabled? How can this be?"
		    stream))))))

#+freebsd
(progn
(defconstant FPE_INTOVF	1)	; integer overflow
(defconstant FPE_INTDIV	2)	; integer divide by zero
(defconstant FPE_FLTDIV	3)	; floating point divide by zero
(defconstant FPE_FLTOVF	4)	; floating point overflow
(defconstant FPE_FLTUND	5)	; floating point underflow
(defconstant FPE_FLTRES	6)	; floating point inexact result
(defconstant FPE_FLTINV	7)	; invalid floating point operation
(defconstant FPE_FLTSUB 8)	; subscript out of range
)

#+freebsd
(defun sigfpe-handler (signal code scp)
  (declare (ignore signal))
  (let ((condition
	 (ecase code
	   ((#.FPE_INTDIV #.FPE_FLTDIV) 'division-by-zero)
	   ((#.FPE_FLTINV #.FPE_FLTSUB) 'floating-point-invalid-operation)
	   ((#.FPE_FLTOVF #.FPE_INTOVF) 'floating-point-overflow)
	   (#.FPE_FLTUND 'floating-point-underflow)
	   (#.FPE_FLTRES 'floating-point-inexact)))
	fop operands
	(sym (find-symbol "GET-FP-OPERANDS" "VM")))
    (when (and sym (fboundp sym))
      (multiple-value-setq (fop operands)
	(funcall sym (alien:sap-alien scp (* unix:sigcontext)))))
    (error condition :operation fop :operands operands)))

#-freebsd
(defun sigfpe-handler (signal code scp)
  (declare (ignore signal code)
	   (type system-area-pointer scp))
  (let* ((modes (sigcontext-floating-point-modes
		 (alien:sap-alien scp (* unix:sigcontext))))
	 (traps (logand (ldb float-exceptions-byte modes)
			(ldb float-traps-byte modes))))
    (multiple-value-bind (fop operands)
	(let ((sym (find-symbol "GET-FP-OPERANDS" "VM")))
	  (if (fboundp sym)
	      (funcall sym (alien:sap-alien scp (* unix:sigcontext)))
	      (values nil nil)))
      (cond ((not (zerop (logand float-divide-by-zero-trap-bit traps)))
	     (error 'division-by-zero
		    :operation fop
		    :operands operands))
	    ((not (zerop (logand float-invalid-trap-bit traps)))
	     (error 'floating-point-invalid-operation
		    :operation fop
		    :operands operands))
	    ((not (zerop (logand float-overflow-trap-bit traps)))
	     (error 'floating-point-overflow
		    :operation fop
		    :operands operands))
	    ((not (zerop (logand float-underflow-trap-bit traps)))
	     (error 'floating-point-underflow
		    :operation fop
		    :operands operands))
	    ((not (zerop (logand float-inexact-trap-bit traps)))
	     (error 'floating-point-inexact
		    :operation fop
		    :operands operands))
	    #+BSD
	    ((zerop (ldb float-exceptions-byte modes))
	     ;; I can't tell what caused the exception!!
	     (error 'floating-point-exception
		    :traps (getf (get-floating-point-modes) :traps)))
	    (t
	     (error "SIGFPE with no exceptions currently enabled?"))))))


;;; WITH-FLOAT-TRAPS-MASKED  --  Public
;;;
(defmacro with-float-traps-masked (traps &body body)
  "Execute BODY with the floating point exceptions listed in TRAPS
  masked (disabled).  TRAPS should be a list of possible exceptions
  which includes :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID and
  :DIVIDE-BY-ZERO and on the X86 :DENORMALIZED-OPERAND. The respective
  accrued exceptions are cleared at the start of the body to support
  their testing within, and restored on exit."
  (let ((traps (dpb (float-trap-mask traps) float-traps-byte 0))
	(exceptions (dpb (float-trap-mask traps) float-sticky-bits 0))
	(trap-mask (dpb (lognot (float-trap-mask traps))
			float-traps-byte #xffffffff))
	(exception-mask (dpb (lognot (vm::float-trap-mask traps))
			     float-sticky-bits #xffffffff))
	(orig-modes (gensym)))
    `(let ((,orig-modes (floating-point-modes)))
      (unwind-protect
	   (progn
	     (setf (floating-point-modes)
		   (logand ,orig-modes ,(logand trap-mask exception-mask)))
	     ,@body)
	;; Restore the original traps and exceptions.
	(setf (floating-point-modes)
	      (logior (logand ,orig-modes ,(logior traps exceptions))
		      (logand (floating-point-modes)
			      ,(logand trap-mask exception-mask)
		       #+mips ,(dpb 0 float-exceptions-byte #xffffffff))))))))
