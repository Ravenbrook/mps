
(in-package "EXTENSIONS")
(export '(with-float-traps-masked))
(in-package "VM")

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
			   ,(dpb 0 float-exceptions-byte #xffffffff))))
	 ))))


(in-package "C")

;;; Apply the function F to a bound X.  If X is an open bound, then the result
;;; will be open.  IF X is NIL, the result is NIL.
;;;
(defun bound-func (f x)
  (and x
       (with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero)
	 ;; With these traps masked, we might get things like infinity or
	 ;; negative infinity returned.  Check for this and return NIL to
	 ;; indicate unbounded.
	 (let ((y (funcall f (bound-value x))))
	   (if (and (floatp y)
		    (float-infinity-p y))
	       nil
	       (set-bound (funcall f (bound-value x)) (consp x)))))))

;;; Apply a binary operator OP to two bounds X and Y.  The result is NIL if
;;; either is NIL.  Otherwise bound is computed and the result is open if
;;; either X or Y is open.
;;;
(defmacro bound-binop (op x y)
  `(and ,x ,y
       (with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero)
	 (set-bound (,op (bound-value ,x)
			 (bound-value ,y))
	            (or (consp ,x) (consp ,y))))))

;;; ONE-ARG-DERIVE-TYPE
;;;
;;; This is used in defoptimizers for computing the resulting type of a
;;; function.
;;;
;;; Given the continuation ARG, derive the resulting type using the
;;; DERIVE-FCN.  DERIVE-FCN takes exactly one argument which is some "atomic"
;;; continuation type like numeric-type or member-type (containing just one
;;; element).  It should return the resulting type, which can be a list of
;;; types.
;;;
;;; For the case of member types, if a member-fcn is given it is called to
;;; compute the result otherwise the member type is first converted to a
;;; numeric type and the derive-fcn is call.
;;;
(defun one-arg-derive-type (arg derive-fcn member-fcn
				&optional (convert-type t))
  (declare (type function derive-fcn)
	   (type (or null function) member-fcn)
	   #+negative-zero-is-not-zero (ignore convert-type))
  (let ((arg-list (prepare-arg-for-derive-type (continuation-type arg))))
    (when arg-list
      (flet ((deriver (x)
	       (typecase x
		 (member-type
		  (if member-fcn
		      (with-float-traps-masked
			  (:underflow :overflow :divide-by-zero)
			(make-member-type
			 :members (list
				   (funcall member-fcn
					    (first (member-type-members x))))))
		      ;; Otherwise convert to a numeric type.
		      (let ((result-type-list
			     (funcall derive-fcn (convert-member-type x))))
			#-negative-zero-is-not-zero
			(if convert-type
			    (convert-back-numeric-type-list result-type-list)
			    result-type-list)
			#+negative-zero-is-not-zero
			result-type-list)))
		 (numeric-type
		  #-negative-zero-is-not-zero
		  (if convert-type
		      (convert-back-numeric-type-list
		       (funcall derive-fcn (convert-numeric-type x)))
		      (funcall derive-fcn x))
		  #+negative-zero-is-not-zero
		  (funcall derive-fcn x))
		 (t
		  *universal-type*))))
	;; Run down the list of args and derive the type of each one, saving
	;; all of the results in a list.
	(let ((results nil))
	  (dolist (arg arg-list)
	    (let ((result (deriver arg)))
	      (if (listp result)
		  (setf results (append results result))
		  (push result results))))
	  (if (rest results)
	      (make-canonical-union-type results)
	      (first results)))))))

;;; TWO-ARG-DERIVE-TYPE
;;;
;;; Same as ONE-ARG-DERIVE-TYPE, except we assume the function takes two
;;; arguments.  DERIVE-FCN takes 3 args in this case: the two original args
;;; and a third which is T to indicate if the two args really represent the
;;; same continuation.  This is useful for deriving the type of things like
;;; (* x x), which should always be positive.  If we didn't do this, we
;;; wouldn't be able to tell.
;;;
;;; Without the negative-zero-is-not-zero feature, numeric types are first
;;; converted to the negative-zero-is-not-zero conventions as expected by the
;;; deriver function.
;;;
;;; For the case of two member types, the result may be derived by calling the
;;; given function FCN but if a NaN is generated then an unbounded type is
;;; returned. Alternatively a tighter, less conservative, type can often be
;;; returned by converting to numeric types and calling the deriver function,
;;; which is the default behavior without the conservative-float-type feature.
;;;
(defun two-arg-derive-type (arg1 arg2 derive-fcn fcn
				 &optional (convert-type t))
  #+negative-zero-is-not-zero
  (declare (ignore convert-type))
  #-conservative-float-type
  (declare (ignore fcn))
  (labels ((maybe-convert-numeric-type (type)
	     #-negative-zero-is-not-zero
	     (if convert-type (convert-numeric-type type) type)
	     #+negative-zero-is-not-zero
	     type)
	   (maybe-convert-back-type-list (type)
	     #-negative-zero-is-not-zero
	     (if convert-type (convert-back-numeric-type-list type) type)
	     #+negative-zero-is-not-zero
	     type)
	   (deriver (x y same-arg)
	     (cond #+conservative-float-type
		   ((and (member-type-p x) (member-type-p y))
		    (let* ((x (first (member-type-members x)))
			   (y (first (member-type-members y)))
			   (result (with-float-traps-masked
				       (:underflow :overflow :divide-by-zero
					:invalid)
				     (funcall fcn x y))))
		      (cond ((null result))
			    ((and (floatp result) (float-nan-p result))
			     (make-numeric-type :class 'float
						:format (type-of result)
						:complexp :real))
			    (t
			     (make-member-type :members (list result))))))
		   #-conservative-float-type
		   ((and (member-type-p x) (member-type-p y))
		    (let* ((x (convert-member-type x))
			   (y (convert-member-type y))
			   (result (funcall derive-fcn x y same-arg)))
		      (maybe-convert-back-type-list result)))
		   ((and (member-type-p x) (numeric-type-p y))
		    (let* ((x (convert-member-type x))
			   (y (maybe-convert-numeric-type y))
			   (result (funcall derive-fcn x y same-arg)))
		      (maybe-convert-back-type-list result)))
		   ((and (numeric-type-p x) (member-type-p y))
		    (let* ((x (maybe-convert-numeric-type x))
			   (y (convert-member-type y))
			   (result (funcall derive-fcn x y same-arg)))
		      (maybe-convert-back-type-list result)))
		   ((and (numeric-type-p x) (numeric-type-p y))
		    (let* ((x (maybe-convert-numeric-type x))
			   (y (maybe-convert-numeric-type y))
			   (result (funcall derive-fcn x y same-arg)))
		      (maybe-convert-back-type-list result)))
		   (t
		    *universal-type*))))
    (let ((same-arg (same-leaf-ref-p arg1 arg2))
	  (a1 (prepare-arg-for-derive-type (continuation-type arg1)))
	  (a2 (prepare-arg-for-derive-type (continuation-type arg2))))
      (when (and a1 a2)
	(let ((results nil))
	  (if same-arg
	      ;; Since the args are the same continuation, just run
	      ;; down the lists.
	      (dolist (x a1)
		(let ((result (deriver x x same-arg)))
		  (if (listp result)
		      (setf results (append results result))
		      (push result results))))
	      ;; Try all pairwise combinations.
	      (dolist (x a1)
		(dolist (y a2)
		  (let ((result (or (deriver x y same-arg)
				    (numeric-contagion x y))))
		    (if (listp result)
			(setf results (append results result))
			(push result results))))))
	  (if (rest results)
	      (make-canonical-union-type results)
	      (first results)))))))

