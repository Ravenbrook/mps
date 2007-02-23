;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/irrat.lisp,v 1.50 2006/07/19 14:58:52 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains all the irrational functions.  Actually, most of the
;;; work is done by calling out to C...
;;;
;;; Author: William Lott.
;;; 

(in-package "KERNEL")


;;;; Random constants, utility functions, and macros.

(defconstant pi 3.14159265358979323846264338327950288419716939937511L0)
;(defconstant e 2.71828182845904523536028747135266249775724709369996L0)

;;; Make these INLINE, since the call to C is at least as compact as a Lisp
;;; call, and saves number consing to boot.
;;;
(defmacro def-math-rtn (name num-args)
  (let ((function (intern (concatenate 'simple-string
				       "%"
				       (string-upcase name)))))
    `(progn
       (declaim (inline ,function))
       (export ',function)
       (alien:def-alien-routine (,name ,function) double-float
	 ,@(let ((results nil))
	     (dotimes (i num-args (nreverse results))
	       (push (list (intern (format nil "ARG-~D" i))
			   'double-float)
		     results)))))))

(eval-when (compile load eval)

(defun handle-reals (function var)
  `((((foreach fixnum single-float bignum ratio))
     (coerce (,function (coerce ,var 'double-float)) 'single-float))
    ((double-float)
     (,function ,var))
    #+double-double
    ((double-double-float)
     (,(symbolicate "DD-" function) ,var))))

); eval-when (compile load eval)


;;;; Stubs for the Unix math library.

;;; Please refer to the Unix man pages for details about these routines.

;;; Trigonometric.
#-x86 (def-math-rtn "sin" 1)
#-x86 (def-math-rtn "cos" 1)
#-x86 (def-math-rtn "tan" 1)
(def-math-rtn "asin" 1)
(def-math-rtn "acos" 1)
#-x86 (def-math-rtn "atan" 1)
#-x86 (def-math-rtn "atan2" 2)
(def-math-rtn "sinh" 1)
(def-math-rtn "cosh" 1)
(def-math-rtn "tanh" 1)
(def-math-rtn "asinh" 1)
(def-math-rtn "acosh" 1)
(def-math-rtn "atanh" 1)

;;; Exponential and Logarithmic.
#-x86 (def-math-rtn "exp" 1)
#-x86 (def-math-rtn "log" 1)
#-x86 (def-math-rtn "log10" 1)
(def-math-rtn "pow" 2)
#-(or x86 sparc-v7 sparc-v8 sparc-v9) (def-math-rtn "sqrt" 1)
(def-math-rtn "hypot" 2)
#-(or hpux x86) (def-math-rtn "log1p" 1)

#+x86 ;; These are needed for use by byte-compiled files.
(progn
  #+nil
  (defun %sin (x)
    (declare (double-float x)
	     (values double-float))
    (%sin x))
  (defun %sin-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%sin-quick x))
  #+nil
  (defun %cos (x)
    (declare (double-float x)
	     (values double-float))
    (%cos x))
  (defun %cos-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%cos-quick x))
  #+nil
  (defun %tan (x)
    (declare (double-float x)
	     (values double-float))
    (%tan x))
  (defun %tan-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%tan-quick x))
  (defun %atan (x)
    (declare (double-float x)
	     (values double-float))
    (%atan x))
  (defun %atan2 (x y)
    (declare (double-float x y)
	     (values double-float))
    (%atan2 x y))
  (defun %exp (x)
    (declare (double-float x)
	     (values double-float))
    (%exp x))
  (defun %log (x)
    (declare (double-float x)
	     (values double-float))
    (%log x))
  (defun %log10 (x)
    (declare (double-float x)
	     (values double-float))
    (%log10 x))
  #+nil ;; notyet
  (defun %pow (x y)
    (declare (type (double-float 0d0) x)
	     (double-float y)
	     (values (double-float 0d0)))
    (%pow x y))
  (defun %sqrt (x)
    (declare (double-float x)
	     (values double-float))
    (%sqrt x))
  (defun %scalbn (f ex)
    (declare (double-float f)
	     (type (signed-byte 32) ex)
	     (values double-float))
    (%scalbn f ex))
  (defun %scalb (f ex)
    (declare (double-float f ex)
	     (values double-float))
    (%scalb f ex))
  (defun %logb (x)
    (declare (double-float x)
	     (values double-float))
    (%logb x))
  (defun %log1p (x)
    (declare (double-float x)
	     (values double-float))
    (%log1p x))
  ) ; progn


;; As above for x86.  It also seems to be needed to handle
;; constant-folding in the compiler.
#+sparc
(progn
  (defun %sqrt (x)
    (declare (double-float x)
	     (values double-float))
    (%sqrt x))
  )

;;; The standard libm routines for sin, cos, and tan on x86 (Linux)
;;; and ppc are not very accurate for large arguments when compared to
;;; sparc (and maxima).  This is basically caused by the fact that
;;; those libraries do not do an accurate argument reduction.  The
;;; following functions use some routines Sun's free fdlibm library to
;;; do accurate reduction.  Then we call the standard C functions (or
;;; vops for x86) on the reduced argument.  This produces much more
;;; accurate values.

#+(or ppc x86)
(progn
(declaim (inline %ieee754-rem-pi/2))
;; Basic argument reduction routine.  It returns two values: n and y
;; such that (n + 8*k)*pi/2+y = x where |y|<pi/4 and n indicates in
;; which octant the arg lies.  Y is actually computed in two parts,
;; y[0] and y[1] such that the sum is y, for accuracy.

(alien:def-alien-routine ("__ieee754_rem_pio2" %ieee754-rem-pi/2) c-call:int
  (x double-float)
  (y (* double-float)))
)

#+ppc
(progn
(declaim (inline %%sin %%cos %%tan))
(macrolet ((frob (alien-name lisp-name)
	     `(alien:def-alien-routine (,alien-name ,lisp-name) double-float
		(x double-float))))
  (frob "sin" %%sin)
  (frob "cos" %%cos)
  (frob "tan" %%tan))
)

#+(or ppc x86)
(macrolet
    ((frob (sin cos tan)
       `(let ((y (make-array 2 :element-type 'double-float)))
	  ;; The array y holds the result for %ieee754-rem-pi/2
	  ;;
	  ;; In all of the routines below, we just compute the sum of
	  ;; y[0] and y[1] and use that as the (reduced) argument for
	  ;; the trig functions.  This is slightly less accurate than
	  ;; what fdlibm does, which calls special functions using
	  ;; y[0] and y[1] separately, for greater accuracy.  This
	  ;; isn't implemented, and some spot checks indicate that
	  ;; what we have here is accurate.
	  (defun %sin (x)
	    (declare (double-float x))
	    (if (< (abs x) (/ pi 4))
		(,sin x)
		;; Argument reduction needed
		(let* ((n (%ieee754-rem-pi/2 x (vector-sap y)))
		       (reduced (+ (aref y 0) (aref y 1))))
		  (case (logand n 3)
		    (0 (,sin reduced))
		    (1 (,cos reduced))
		    (2 (- (,sin reduced)))
		    (3 (- (,cos reduced)))))))
	  (defun %cos (x)
	    (declare (double-float x))
	    (if (< (abs x) (/ pi 4))
		(,cos x)
		;; Argument reduction needed
		(let* ((n (%ieee754-rem-pi/2 x (vector-sap y)))
		       (reduced (+ (aref y 0) (aref y 1))))
		  (case (logand n 3)
		    (0 (,cos reduced))
		    (1 (- (,sin reduced)))
		    (2 (- (,cos reduced)))
		    (3 (,sin reduced))))))
	  (defun %tan (x)
	    (declare (double-float x))
	    (if (< (abs x) (/ pi 4))
		(,tan x)
		;; Argument reduction needed
		(let* ((n (%ieee754-rem-pi/2 x (vector-sap y)))
		       (reduced (+ (aref y 0) (aref y 1))))
		  (if (evenp n)
		      (,tan reduced)
		      (- (/ (,tan reduced))))))))))
  #+x86
  (frob %sin-quick %cos-quick %tan-quick)
  #+ppc
  (frob %%sin %%cos %%tan))



;;;; Power functions.

(defun exp (number)
  "Return e raised to the power NUMBER."
  (number-dispatch ((number number))
    (handle-reals %exp number)
    ((complex)
     (* (exp (realpart number))
	(cis (imagpart number))))))

;;; INTEXP -- Handle the rational base, integer power case.

(defparameter *intexp-maximum-exponent* 10000)

;;; This function precisely calculates base raised to an integral power.  It
;;; separates the cases by the sign of power, for efficiency reasons, as powers
;;; can be calculated more efficiently if power is a positive integer.  Values
;;; of power are calculated as positive integers, and inverted if negative.
;;;
(defun intexp (base power)
  ;; Handle the special case of 1^power.  Maxima sometimes does this,
  ;; and there's no need to cause a continuable error in this case.
  ;; Should we also handle (-1)^power?
  (when (eql base 1)
    (return-from intexp base))
  
  (when (> (abs power) *intexp-maximum-exponent*)
    (cerror "Continue with calculation."
	    "The absolute value of ~S exceeds ~S."
	    power '*intexp-maximum-exponent* base power))
  (cond ((minusp power)
	 (/ (intexp base (- power))))
	((eql base 2)
	 (ash 1 power))
	(t
	 (do ((nextn (ash power -1) (ash power -1))
	      (total (if (oddp power) base 1)
		     (if (oddp power) (* base total) total)))
	     ((zerop nextn) total)
	   (setq base (* base base))
	   (setq power nextn)))))


;;; EXPT  --  Public
;;;
;;;    If an integer power of a rational, use INTEXP above.  Otherwise, do
;;; floating point stuff.  If both args are real, we try %POW right off,
;;; assuming it will return 0 if the result may be complex.  If so, we call
;;; COMPLEX-POW which directly computes the complex result.  We also separate
;;; the complex-real and real-complex cases from the general complex case.
;;;
(defun expt (base power)
  "Returns BASE raised to the POWER."
  (if (zerop power)
      ;; CLHS says that if the power is 0, the result is 1, subject to
      ;; numeric contagion.  But what happens if base is infinity or
      ;; NaN?  Do we silently return 1?  For now, I think we should
      ;; signal an error if the FP modes say so.
      (let ((result (1+ (* base power))))
	;; If we get an NaN here, that means base*power above didn't
	;; produce 0 and FP traps were disabled, so we handle that
	;; here.  Should this be a continuable restart?
	(if (and (floatp result) (float-nan-p result))
	    (float 1 result)
	    result))
    (labels (;; determine if the double float is an integer.
	     ;;  0 - not an integer
	     ;;  1 - an odd int
	     ;;  2 - an even int
	     (isint (ihi lo)
	       (declare (type (unsigned-byte 31) ihi)
			(type (unsigned-byte 32) lo)
			(optimize (speed 3) (safety 0)))
	       (let ((isint 0))
		 (declare (type fixnum isint))
		 (cond ((>= ihi #x43400000)	; exponent >= 53
			(setq isint 2))
		       ((>= ihi #x3ff00000)
			(let ((k (- (ash ihi -20) #x3ff)))	; exponent
			  (declare (type (mod 53) k))
			  (cond ((> k 20)
				 (let* ((shift (- 52 k))
					(j (logand (ash lo (- shift))))
					(j2 (ash j shift)))
				   (declare (type (mod 32) shift)
					    (type (unsigned-byte 32) j j2))
				   (when (= j2 lo)
				     (setq isint (- 2 (logand j 1))))))
				((= lo 0)
				 (let* ((shift (- 20 k))
					(j (ash ihi (- shift)))
					(j2 (ash j shift)))
				   (declare (type (mod 32) shift)
					    (type (unsigned-byte 31) j j2))
				   (when (= j2 ihi)
				     (setq isint (- 2 (logand j 1))))))))))
		 isint))
	     (real-expt (x y rtype)
	       (let ((x (coerce x 'double-float))
		     (y (coerce y 'double-float)))
		 (declare (double-float x y))
		 (let* ((x-hi (kernel:double-float-high-bits x))
			(x-lo (kernel:double-float-low-bits x))
			(x-ihi (logand x-hi #x7fffffff))
			(y-hi (kernel:double-float-high-bits y))
			(y-lo (kernel:double-float-low-bits y))
			(y-ihi (logand y-hi #x7fffffff)))
		   (declare (type (signed-byte 32) x-hi y-hi)
			    (type (unsigned-byte 31) x-ihi y-ihi)
			    (type (unsigned-byte 32) x-lo y-lo))
		   ;; y==zero: x**0 = 1
		   (when (zerop (logior y-ihi y-lo))
		     (return-from real-expt (coerce 1d0 rtype)))
		   ;; +-NaN return x+y
		   (when (or (> x-ihi #x7ff00000)
			     (and (= x-ihi #x7ff00000) (/= x-lo 0))
			     (> y-ihi #x7ff00000)
			     (and (= y-ihi #x7ff00000) (/= y-lo 0)))
		     (return-from real-expt (coerce (+ x y) rtype)))
		   (let ((yisint (if (< x-hi 0) (isint y-ihi y-lo) 0)))
		     (declare (type fixnum yisint))
		     ;; special value of y
		     (when (and (zerop y-lo) (= y-ihi #x7ff00000))
		       ;; y is +-inf
		       (return-from real-expt
			 (cond ((and (= x-ihi #x3ff00000) (zerop x-lo))
				;; +-1**inf is NaN
				(coerce (- y y) rtype))
			       ((>= x-ihi #x3ff00000)
				;; (|x|>1)**+-inf = inf,0
				(if (>= y-hi 0)
				    (coerce y rtype)
				    (coerce 0 rtype)))
			       (t
				;; (|x|<1)**-,+inf = inf,0
				(if (< y-hi 0)
				    (coerce (- y) rtype)
				    (coerce 0 rtype))))))

		     (let ((abs-x (abs x)))
		       (declare (double-float abs-x))
		       ;; special value of x
		       (when (and (zerop x-lo)
				  (or (= x-ihi #x7ff00000) (zerop x-ihi)
				      (= x-ihi #x3ff00000)))
			 ;; x is +-0,+-inf,+-1
			 (let ((z (if (< y-hi 0)
				      (/ 1 abs-x)	; z = (1/|x|)
				      abs-x)))
			   (declare (double-float z))
			   (when (< x-hi 0)
			     (cond ((and (= x-ihi #x3ff00000) (zerop yisint))
				    ;; (-1)**non-int
				    (let ((y*pi (* y pi)))
				      (declare (double-float y*pi))
				      (return-from real-expt
				        (complex
					 (coerce (%cos y*pi) rtype)
					 (coerce (%sin y*pi) rtype)))))
				   ((= yisint 1)
				    ;; (x<0)**odd = -(|x|**odd)
				    (setq z (- z)))))
			   (return-from real-expt (coerce z rtype))))
		       
		       (if (>= x-hi 0)
			   ;; x>0
			   (coerce (kernel::%pow x y) rtype)
			   ;; x<0
			   (let ((pow (kernel::%pow abs-x y)))
			     (declare (double-float pow))
			     (case yisint
			       (1 ; Odd
				(coerce (* -1d0 pow) rtype))
			       (2 ; Even
				(coerce pow rtype))
			       (t ; Non-integer
				(let ((y*pi (* y pi)))
				  (declare (double-float y*pi))
				  (complex
				   (coerce (* pow (%cos y*pi)) rtype)
				   (coerce (* pow (%sin y*pi)) rtype)))))))))))))
      (declare (inline real-expt))
      (number-dispatch ((base number) (power number))
        (((foreach fixnum (or bignum ratio) (complex rational)) integer)
	 (intexp base power))
	(((foreach single-float double-float) rational)
	 (real-expt base power '(dispatch-type base)))
	(((foreach fixnum (or bignum ratio) single-float)
	  (foreach ratio single-float))
	 (real-expt base power 'single-float))
	(((foreach fixnum (or bignum ratio) single-float double-float)
	  double-float)
	 (real-expt base power 'double-float))
	((double-float single-float)
	 (real-expt base power 'double-float))
	#+double-double
	(((foreach fixnum (or bignum ratio) single-float double-float double-double-float)
	  double-double-float)
	 (dd-%pow (coerce base 'double-double-float) power))
	#+double-double
	((double-double-float
	  (foreach fixnum (or bignum ratio) single-float double-float))
	 (dd-%pow base (coerce power 'double-double-float)))
	(((foreach (complex rational) (complex float)) rational)
	 (* (expt (abs base) power)
	    (cis (* power (phase base)))))
	(((foreach fixnum (or bignum ratio) single-float double-float
		   #+double-double double-double-float)
	  complex)
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (log base)))))
	(((foreach (complex float) (complex rational))
	  (foreach complex double-float single-float #+double-double double-double-float))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (log base)))))))))

;; Compute the base 2 log of an integer
(defun log2 (x)
  ;; Write x = 2^n*f where 1/2 < f <= 1.  Then log2(x) = n + log2(f).
  ;;
  ;; So we grab the top few bits of x and scale that appropriately,
  ;; take the log of it and add it to n.
  (let ((n (integer-length x)))
    (if (< n vm:double-float-digits)
	(log (coerce x 'double-float) 2d0)
	(let ((exp (min vm:double-float-digits n))
	      (f (ldb (byte vm:double-float-digits
			    (max 0 (- n vm:double-float-digits)))
		      x)))
	  (+ n (log (scale-float (float f 1d0) (- exp))
		    2d0))))))

(defun log (number &optional (base nil base-p))
  "Return the logarithm of NUMBER in the base BASE, which defaults to e."
  (if base-p
      (cond ((zerop base)
	     ;; ANSI spec
	     base)
	    ((and (integerp number) (integerp base)
		  (plusp number) (plusp base))
	     ;; Let's try to do something nice when both the number
	     ;; and the base are positive integers.  Use the rule that
	     ;; log_b(x) = log_2(x)/log_2(b)
	     (coerce (/ (log2 number) (log2 base)) 'single-float))
	    ((and (realp number) (realp base))
	     ;; CLHS 12.1.4.1 says
	     ;;
	     ;;   When rationals and floats are combined by a
	     ;;   numerical function, the rational is first converted
	     ;;   to a float of the same format.
	     ;;
	     ;; So assume this applies to floats as well convert all
	     ;; numbers to the largest float format before computing
	     ;; the log.
	     ;;
	     ;; This makes (log 17 10.0) = (log 17.0 10) and so on.
	     (number-dispatch ((number real) (base real))
	       ((double-float
		 (foreach double-float single-float fixnum bignum ratio))
		(/ (log number) (log (coerce base 'double-float))))
	       (((foreach single-float fixnum bignum ratio)
		 double-float)
		(/ (log (coerce number 'double-float)) (log base)))
	       #+double-double
	       ((double-double-float
		 (foreach double-double-float double-float single-float fixnum bignum ratio))
		(/ (log number) (log (coerce base 'double-double-float))))
	       #+double-double
	       (((foreach double-float single-float fixnum bignum ratio)
		 double-double-float)
		(/ (log (coerce number 'double-double-float)) (log base)))
	       (((foreach single-float fixnum bignum ratio)
		 (foreach single-float fixnum bignum ratio))
		;; Converting everything to double-float helps the
		;; cases like (log 17 10) = (/ (log 17) (log 10)).
		;; This is usually handled above, but if we compute (/
		;; (log 17) (log 10)), we get a slightly different
		;; answer due to roundoff.  This makes it a bit more
		;; consistent.
		;;
		;; FIXME: This probably needs more work.
		(let ((result (/ (log (float number 1d0))
				 (log (float base 1d0)))))
		  (if (realp result)
		      (coerce result 'single-float)
		      (coerce result '(complex single-float)))))))
	    (t
	     ;; FIXME:  This probably needs some work as well.
	     (/ (log number) (log base))))
      (number-dispatch ((number number))
	(((foreach fixnum bignum))
	 (if (minusp number)
	     (complex (coerce (log (- number)) 'single-float)
		      (coerce pi 'single-float))
	     (coerce (/ (log2 number) #.(log (exp 1d0) 2d0)) 'single-float)))
	((ratio)
	 (if (minusp number)
	     (complex (coerce (log (- number)) 'single-float)
		      (coerce pi 'single-float))
	     ;; What happens when the ratio is close to 1?  We need to
	     ;; be careful to preserve accuracy.
	     (let ((top (numerator number))
		   (bot (denominator number)))
	       ;; If the number of bits in the numerator and
	       ;; denominator are different, just use the fact
	       ;; log(x/y) = log(x) - log(y).  But to preserve
	       ;; accuracy, we actually do
	       ;; (log2(x)-log2(y))/log2(e)).
	       ;;
	       ;; However, if the numerator and denominator have the
	       ;; same number of bits, implying the quotient is near
	       ;; one, we use log1p(x) = log(1+x). Since the number is
	       ;; rational, we don't lose precision subtracting 1 from
	       ;; it, and converting it to double-float is accurate.
	       (if (= (integer-length top)
		      (integer-length bot))
		   (coerce (%log1p (coerce (- number 1) 'double-float))
			   'single-float)
		   (coerce (/ (- (log2 top) (log2 bot))
			      #.(log (exp 1d0) 2d0))
			   'single-float)))))
	(((foreach single-float double-float))
	 ;; Is (log -0) -infinity (libm.a) or -infinity + i*pi (Kahan)?
	 ;; Since this doesn't seem to be an implementation issue
	 ;; I (pw) take the Kahan result.
	 (if (< (float-sign number)
		(coerce 0 '(dispatch-type number)))
	     (complex (log (- number)) (coerce pi '(dispatch-type number)))
	     (coerce (%log (coerce number 'double-float))
		     '(dispatch-type number))))
	#+double-double
	((double-double-float)
	 (let ((hi (kernel:double-double-hi number)))
	   (if (< (float-sign hi) 0d0)
	       (complex (dd-%log (- number)) dd-pi)
	       (dd-%log number))))
	((complex)
	 (complex-log number)))))

(defun sqrt (number)
  "Return the square root of NUMBER."
  (number-dispatch ((number number))
    (((foreach fixnum bignum ratio))
     (if (minusp number)
	 (complex-sqrt number)
	 (coerce (%sqrt (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (minusp number)
	 (complex-sqrt number)
	 (coerce (%sqrt (coerce number 'double-float))
		 '(dispatch-type number))))
    #+double-double
    ((double-double-float)
     (if (minusp number)
	 (dd-complex-sqrt number)
	 (multiple-value-bind (hi lo)
	     (c::sqrt-dd (kernel:double-double-hi number) (kernel:double-double-lo number))
	   (kernel:%make-double-double-float hi lo))))
    ((complex)
     (complex-sqrt number))))


;;;; Trigonometic and Related Functions

(defun abs (number)
  "Returns the absolute value of the number."
  (number-dispatch ((number number))
    (((foreach single-float double-float fixnum rational
	       #+double-double double-double-float))
     (abs number))
    ((complex)
     (let ((rx (realpart number))
	   (ix (imagpart number)))
       (etypecase rx
	 (rational
	  (sqrt (+ (* rx rx) (* ix ix))))
	 (single-float
	  (coerce (%hypot (coerce rx 'double-float)
			  (coerce ix 'double-float))
		  'single-float))
	 (double-float
	  (%hypot rx ix))
	 #+double-double
	 (double-double-float
	  (multiple-value-bind (abs^2 scale)
	      (dd-cssqs number)
	    (scale-float (sqrt abs^2) scale))))))))

(defun phase (number)
  "Returns the angle part of the polar representation of a complex number.
  For complex numbers, this is (atan (imagpart number) (realpart number)).
  For non-complex positive numbers, this is 0.  For non-complex negative
  numbers this is PI."
  (etypecase number
    (rational
     (if (minusp number)
	 (coerce pi 'single-float)
	 0.0f0))
    (single-float
     (if (minusp (float-sign number))
	 (coerce pi 'single-float)
	 0.0f0))
    (double-float
     (if (minusp (float-sign number))
	 (coerce pi 'double-float)
	 0.0d0))
    #+double-double
    (double-double-float
     (if (minusp (float-sign number))
	 dd-pi
	 0w0))
    (complex
     (atan (imagpart number) (realpart number)))))


(defun sin (number)  
  "Return the sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %sin number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (sin x) (cosh y))
		(* (cos x) (sinh y)))))))

(defun cos (number)
  "Return the cosine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %cos number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (cos x) (cosh y))
		(- (* (sin x) (sinh y))))))))

(defun tan (number)
  "Return the tangent of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %tan number)
    ((complex)
     (complex-tan number))))

(defun cis (theta)
  "Return cos(Theta) + i sin(Theta), AKA exp(i Theta)."
  (if (complexp theta)
      (error "Argument to CIS is complex: ~S" theta)
      (complex (cos theta) (sin theta))))

(defun asin (number)
  "Return the arc sine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
	 (complex-asin number)
	 (coerce (%asin (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (float-nan-p number)
	     (and (<= number (coerce 1 '(dispatch-type number)))
		  (>= number (coerce -1 '(dispatch-type number)))))
	 (coerce (%asin (coerce number 'double-float))
		 '(dispatch-type number))
	 (complex-asin number)))
    #+double-double
    ((double-double-float)
     (if (or (float-nan-p number)
	     (and (<= number 1w0)
		  (>= number -1w0)))
	 (dd-%asin number)
	 (dd-complex-asin number)))
    ((complex)
     (complex-asin number))))

(defun acos (number)
  "Return the arc cosine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
	 (complex-acos number)
	 (coerce (%acos (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (float-nan-p number)
	     (and (<= number (coerce 1 '(dispatch-type number)))
		  (>= number (coerce -1 '(dispatch-type number)))))
	 (coerce (%acos (coerce number 'double-float))
		 '(dispatch-type number))
	 (complex-acos number)))
    #+double-double
    ((double-double-float)
     (if (or (float-nan-p number)
	     (and (<= number 1w0)
		  (>= number -1w0)))
	 (dd-%acos number)
	 (complex-acos number)))
    ((complex)
     (complex-acos number))))


(defun atan (y &optional (x nil xp))
  "Return the arc tangent of Y if X is omitted or Y/X if X is supplied."
  (if xp
      (flet ((atan2 (y x)
	       (declare (type double-float y x)
			(values double-float))
	       (if (zerop x)
		   (if (zerop y)
		       (if (plusp (float-sign x))
			   y
			   (float-sign y pi))
		       (float-sign y (/ pi 2)))
		   (%atan2 y x))))
	;; If X is given, both X and Y must be real numbers.
	(number-dispatch ((y real) (x real))
	  ((double-float
	    (foreach double-float single-float fixnum bignum ratio))
	   (atan2 y (coerce x 'double-float)))
	  (((foreach single-float fixnum bignum ratio)
	    double-float)
	   (atan2 (coerce y 'double-float) x))
	  (((foreach single-float fixnum bignum ratio)
	    (foreach single-float fixnum bignum ratio))
	   (coerce (atan2 (coerce y 'double-float) (coerce x 'double-float))
		   'single-float))
	  #+double-double
	  ((double-double-float
	    (foreach double-double-float double-float single-float fixnum bignum ratio))
	   (dd-%atan2 y (coerce x 'double-double-float)))
	  #+double-double
	  (((foreach double-float single-float fixnum bignum ratio)
	    double-double-float)
	   (dd-%atan2 (coerce y 'double-double-float) x))))
      (number-dispatch ((y number))
	(handle-reals %atan y)
	((complex)
	 (complex-atan y)))))

(defun sinh (number)
  "Return the hyperbolic sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %sinh number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (sinh x) (cos y))
		(* (cosh x) (sin y)))))))

(defun cosh (number)
  "Return the hyperbolic cosine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %cosh number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (cosh x) (cos y))
		(* (sinh x) (sin y)))))))

(defun tanh (number)
  "Return the hyperbolic tangent of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %tanh number)
    ((complex)
     (complex-tanh number))))

(defun asinh (number)
  "Return the hyperbolic arc sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %asinh number)
    ((complex)
     (complex-asinh number))))

(defun acosh (number)
  "Return the hyperbolic arc cosine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     ;; acosh is complex if number < 1
     (if (< number 1)
	 (complex-acosh number)
	 (coerce (%acosh (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (< number (coerce 1 '(dispatch-type number)))
	 (complex-acosh number)
	 (coerce (%acosh (coerce number 'double-float))
		 '(dispatch-type number))))
    #+double-double
    ((double-double-float)
     (if (< number 1w0)
	 (complex-acosh number)
	 (dd-%acosh number)))
    ((complex)
     (complex-acosh number))))

(defun atanh (number)
  "Return the hyperbolic arc tangent of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     ;; atanh is complex if |number| > 1
     (if (or (> number 1) (< number -1))
	 (complex-atanh number)
	 (coerce (%atanh (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
	     (< number (coerce -1 '(dispatch-type number))))
	 (complex-atanh number)
	 (coerce (%atanh (coerce number 'double-float))
		 '(dispatch-type number))))
    #+double-double
    ((double-double-float)
     (if (or (> number 1w0)
	     (< number -1w0))
	 (complex-atanh number)
	 (dd-%atanh (coerce number 'double-double-float))))
    ((complex)
     (complex-atanh number))))

;;; HP-UX does not supply a C version of log1p, so use the definition.
;;; We really need to fix this.  The definition really loses big-time
;;; in roundoff as x gets small.

#+hpux
(declaim (inline %log1p))
#+hpux
(defun %log1p (number)
  (declare (double-float number)
	   (optimize (speed 3) (safety 0)))
  (the double-float (log (the (double-float 0d0) (+ number 1d0)))))


;;;;
;;;; This is a set of routines that implement many elementary
;;;; transcendental functions as specified by ANSI Common Lisp.  The
;;;; implementation is based on Kahan's paper.
;;;;
;;;; I believe I have accurately implemented the routines and are
;;;; correct, but you may want to check for your self.
;;;;
;;;; These functions are written for CMU Lisp and take advantage of
;;;; some of the features available there.  It may be possible,
;;;; however, to port this to other Lisps.
;;;;
;;;; Some functions are significantly more accurate than the original
;;;; definitions in CMU Lisp.  In fact, some functions in CMU Lisp
;;;; give the wrong answer like (acos #c(-2.0 0.0)), where the true
;;;; answer is pi + i*log(2-sqrt(3)).
;;;;
;;;; All of the implemented functions will take any number for an
;;;; input, but the result will always be a either a complex
;;;; single-float or a complex double-float.
;;;;
;;;; General functions
;;;;   complex-sqrt
;;;;   complex-log
;;;;   complex-atanh
;;;;   complex-tanh
;;;;   complex-acos
;;;;   complex-acosh
;;;;   complex-asin
;;;;   complex-asinh
;;;;   complex-atan
;;;;   complex-tan
;;;;
;;;; Utility functions:
;;;;   scalb logb
;;;;
;;;; Internal functions:
;;;;    square coerce-to-complex-type cssqs complex-log-scaled
;;;;
;;;;
;;;; Please send any bug reports, comments, or improvements to Raymond
;;;; Toy at toy@rtp.ericsson.se.
;;;;
;;;; References
;;;;
;;;; Kahan, W. "Branch Cuts for Complex Elementary Functions, or Much
;;;; Ado About Nothing's Sign Bit" in Iserles and Powell (eds.) "The
;;;; State of the Art in Numerical Analysis", pp. 165-211, Clarendon
;;;; Press, 1987
;;;;

(declaim (inline square))
(defun square (x)
  (declare (float x))
  (* x x))

;; If you have these functions in libm, perhaps they should be used
;; instead of these Lisp versions.  These versions are probably good
;; enough, especially since they are portable.

(declaim (inline scalb))
(defun scalb (x n)
  "Compute 2^N * X without compute 2^N first (use properties of the
underlying floating-point format"
  (declare (type float x)
	   (type double-float-exponent n))
  (scale-float x n))

(declaim (inline logb-finite))
(defun logb-finite (x)
  "Same as logb but X is not infinity and non-zero and not a NaN, so
that we can always return an integer"
  (declare (type float x))
  (multiple-value-bind (signif expon sign)
      (decode-float x)
    (declare (ignore signif sign))
    ;; decode-float is almost right, except that the exponent
    ;; is off by one
    (1- expon)))
      
(defun logb (x)
  "Compute an integer N such that 1 <= |2^(-N) * x| < 2.
For the special cases, the following values are used:

    x             logb
   NaN            NaN
   +/- infinity   +infinity
   0              -infinity
"
  (declare (type float x))
  (cond ((float-nan-p x)
	 x)
	((float-infinity-p x)
	 #.ext:double-float-positive-infinity)
	((zerop x)
	 ;; The answer is negative infinity, but we are supposed to
	 ;; signal divide-by-zero, so do the actual division
	 (/ -1 x)
	 )
	(t
	 (logb-finite x))))

  

;; This function is used to create a complex number of the appropriate
;; type.

(declaim (inline coerce-to-complex-type))
(defun coerce-to-complex-type (x y z)
  "Create complex number with real part X and imaginary part Y such that
it has the same type as Z.  If Z has type (complex rational), the X
and Y are coerced to single-float."
  (declare (double-float x y)
	   (number z)
	   (optimize (extensions:inhibit-warnings 3)))
  (if (typep (realpart z) 'double-float)
      (complex x y)
      ;; Convert anything that's not a double-float to a single-float.
      (complex (float x 1f0)
	       (float y 1f0))))

(defun cssqs (z)
  ;; Compute |(x+i*y)/2^k|^2 scaled to avoid over/underflow. The
  ;; result is r + i*k, where k is an integer.
  
  ;; Save all FP flags
  (let ((x (float (realpart z) 1d0))
	(y (float (imagpart z) 1d0)))
    ;; Would this be better handled using an exception handler to
    ;; catch the overflow or underflow signal?  For now, we turn all
    ;; traps off and look at the accrued exceptions to see if any
    ;; signal would have been raised.
    (with-float-traps-masked (:underflow :overflow)
      (let ((rho (+ (square x) (square y))))
	(declare (optimize (speed 3) (space 0)))
	(cond ((and (or (float-nan-p rho)
			(float-infinity-p rho))
		    (or (float-infinity-p (abs x))
			(float-infinity-p (abs y))))
	       (values ext:double-float-positive-infinity 0))
	      ((let ((threshold #.(/ least-positive-double-float
				     double-float-epsilon))
		     (traps (ldb vm::float-sticky-bits
				 (vm:floating-point-modes))))
		 ;; Overflow raised or (underflow raised and rho <
		 ;; lambda/eps)
		 (or (not (zerop (logand vm:float-overflow-trap-bit traps)))
		     (and (not (zerop (logand vm:float-underflow-trap-bit traps)))
			  (< rho threshold))))
	       ;; If we're here, neither x nor y are infinity and at
	       ;; least one is non-zero.. Thus logb returns a nice
	       ;; integer.
	       (let ((k (- (logb-finite (max (abs x) (abs y))))))
		 (values (+ (square (scalb x k))
			    (square (scalb y k)))
			 (- k))))
	      (t
	       (values rho 0)))))))

(defun complex-sqrt (z)
  "Principle square root of Z

Z may be any number, but the result is always a complex."
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-sqrt (dd-complex-sqrt z)))
  (multiple-value-bind (rho k)
      (cssqs z)
    (declare (type (or (member 0d0) (double-float 0d0)) rho)
	     (type fixnum k))
    (let ((x (float (realpart z) 1.0d0))
	  (y (float (imagpart z) 1.0d0))
	  (eta 0d0)
	  (nu 0d0))
      (declare (double-float x y eta nu))

      (locally
	  ;; space 0 to get maybe-inline functions inlined.
	  (declare (optimize (speed 3) (space 0)))

	(if (not (float-nan-p x))
	    (setf rho (+ (scalb (abs x) (- k)) (sqrt rho))))

	(cond ((oddp k)
	       (setf k (ash k -1)))
	      (t
	       (setf k (1- (ash k -1)))
	       (setf rho (+ rho rho))))

	(setf rho (scalb (sqrt rho) k))

	(setf eta rho)
	(setf nu y)

	(when (/= rho 0d0)
	  (when (not (float-infinity-p (abs nu)))
	    (setf nu (/ (/ nu rho) 2d0)))
	  (when (< x 0d0)
	    (setf eta (abs nu))
	    (setf nu (float-sign y rho))))
	(coerce-to-complex-type eta nu z)))))

(defun complex-log-scaled (z j)
  "Compute log(2^j*z).

This is for use with J /= 0 only when |z| is huge."
  (declare (number z)
	   (fixnum j))
  ;; The constants t0, t1, t2 should be evaluated to machine
  ;; precision.  In addition, Kahan says the accuracy of log1p
  ;; influences the choices of these constants but doesn't say how to
  ;; choose them.  We'll just assume his choices matches our
  ;; implementation of log1p.
  (let ((t0 #.(/ 1 (sqrt 2.0d0)))
	(t1 1.2d0)
	(t2 3d0)
	(ln2 #.(log 2d0))
	(x (float (realpart z) 1.0d0))
	(y (float (imagpart z) 1.0d0)))
    (multiple-value-bind (rho k)
	(cssqs z)
      (declare (optimize (speed 3)))
      (let ((beta (max (abs x) (abs y)))
	    (theta (min (abs x) (abs y))))
	(coerce-to-complex-type (if (and (zerop k)
					 (< t0 beta)
					 (or (<= beta t1)
					     (< rho t2)))
				    (/ (%log1p (+ (* (- beta 1.0d0)
						     (+ beta 1.0d0))
						  (* theta theta)))
				       2d0)
				    (+ (/ (log rho) 2d0)
				       (* (+ k j) ln2)))
				(atan y x)
				z)))))

(defun complex-log (z)
  "Log of Z = log |Z| + i * arg Z

Z may be any number, but the result is always a complex."
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-log (dd-complex-log-scaled z 0)))
  (complex-log-scaled z 0))
	       
;; Let us note the following "strange" behavior.  atanh 1.0d0 is
;; +infinity, but the following code returns approx 176 + i*pi/4. The
;; reason for the imaginary part is caused by the fact that arg i*y is
;; never 0 since we have positive and negative zeroes.

(defun complex-atanh (z)
  "Compute atanh z = (log(1+z) - log(1-z))/2"
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-atanh (dd-complex-atanh z)))
  
  (if (and (realp z) (< z -1))
      ;; atanh is continuous in quadrant III in this case.
      (complex-atanh (complex z -0f0))
      (let* ( ;; Constants
	     (theta (/ (sqrt most-positive-double-float) 4.0d0))
	     (rho (/ 4.0d0 (sqrt most-positive-double-float)))
	     (half-pi (/ pi 2.0d0))
	     (rp (float (realpart z) 1.0d0))
	     (beta (float-sign rp 1.0d0))
	     (x (* beta rp))
	     (y (* beta (- (float (imagpart z) 1.0d0))))
	     (eta 0.0d0)
	     (nu 0.0d0))
	;; Shouldn't need this declare.
	(declare (double-float x y))
	(locally
	    (declare (optimize (speed 3)))
	  (cond ((or (> x theta)
		     (> (abs y) theta))
		 ;; To avoid overflow...
		 (setf nu (float-sign y half-pi))
		 ;; eta is real part of 1/(x + iy).  This is x/(x^2+y^2),
		 ;; which can cause overflow.  Arrange this computation so
		 ;; that it won't overflow.
		 (setf eta (let* ((x-bigger (> x (abs y)))
				  (r (if x-bigger (/ y x) (/ x y)))
				  (d (+ 1.0d0 (* r r))))
			     (if x-bigger
				 (/ (/ x) d)
				 (/ (/ r y) d)))))
		((= x 1.0d0)
		 ;; Should this be changed so that if y is zero, eta is set
		 ;; to +infinity instead of approx 176?  In any case
		 ;; tanh(176) is 1.0d0 within working precision.
		 (let ((t1 (+ 4d0 (square y)))
		       (t2 (+ (abs y) rho)))
		   (setf eta (log (/ (sqrt (sqrt t1))
				     (sqrt t2))))
		   (setf nu (* 0.5d0
			       (float-sign y
					   (+ half-pi (atan (* 0.5d0 t2))))))))
		(t
		 (let ((t1 (+ (abs y) rho)))
		   ;; Normal case using log1p(x) = log(1 + x)
		   (setf eta (* 0.25d0
				(%log1p (/ (* 4.0d0 x)
					   (+ (square (- 1.0d0 x))
					      (square t1))))))
		   (setf nu (* 0.5d0
			       (atan (* 2.0d0 y)
				     (- (* (- 1.0d0 x)
					   (+ 1.0d0 x))
					(square t1))))))))
	  (coerce-to-complex-type (* beta eta)
				  (- (* beta nu))
				  z)))))

(defun complex-tanh (z)
  "Compute tanh z = sinh z / cosh z"
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-tanh (dd-complex-tanh z)))
  
  (let ((x (float (realpart z) 1.0d0))
	(y (float (imagpart z) 1.0d0)))
    (locally
	;; space 0 to get maybe-inline functions inlined
	(declare (optimize (speed 3) (space 0)))
      (cond ((> (abs x)
		#-(or linux hpux) #.(/ (%asinh most-positive-double-float) 4d0)
		;; This is more accurate under linux.
		#+(or linux hpux) #.(/ (+ (%log 2.0d0)
					  (%log most-positive-double-float)) 4d0))
	     (coerce-to-complex-type (float-sign x)
				     (float-sign y) z))
	    (t
	     (let* ((tv (%tan y))
		    (beta (+ 1.0d0 (* tv tv)))
		    (s (sinh x))
		    (rho (sqrt (+ 1.0d0 (* s s)))))
	       (if (float-infinity-p (abs tv))
		   (coerce-to-complex-type (/ rho s)
					   (/ tv)
					   z)
		   (let ((den (+ 1.0d0 (* beta s s))))
		     (coerce-to-complex-type (/ (* beta rho s)
						den)
					     (/ tv den)
					     z)))))))))

;; Kahan says we should only compute the parts needed.  Thus, the
;; realpart's below should only compute the real part, not the whole
;; complex expression.  Doing this can be important because we may get
;; spurious signals that occur in the part that we are not using.
;;
;; However, we take a pragmatic approach and just use the whole
;; expression.

;; NOTE: The formula given by Kahan is somewhat ambiguous in whether
;; it's the conjugate of the square root or the square root of the
;; conjugate.  This needs to be checked.

;; I checked.  It doesn't matter because (conjugate (sqrt z)) is the
;; same as (sqrt (conjugate z)) for all z.  This follows because
;;
;; (conjugate (sqrt z)) = exp(0.5*log |z|)*exp(-0.5*j*arg z).
;;
;; (sqrt (conjugate z)) = exp(0.5*log|z|)*exp(0.5*j*arg conj z)
;;
;; and these two expressions are equal if and only if arg conj z =
;; -arg z, which is clearly true for all z.

;; NOTE: The rules of Common Lisp says that if you mix a real with a
;; complex, the real is converted to a complex before performing the
;; operation.  However, Kahan says in this paper (pg 176):
;;
;; (iii) Careless handling can turn infinity or the sign of zero into
;;       misinformation that subsequently disappears leaving behind
;;       only a plausible but incorrect result.  That is why compilers
;;       must not transform z-1 into z-(1+i*0), as we have seen above,
;;       nor -(-x-x^2) into (x+x^2), as we shall see below, lest a
;;       subsequent logarithm or square root produce a non-zero
;;       imaginary part whose sign is opposite to what was intended.
;;
;; The interesting examples are too long and complicated to reproduce
;; here.  We refer the reader to his paper.
;;
;; The functions below are intended to handle the cases where a real
;; is mixed with a complex and we don't want CL complex contagion to
;; occur..

(declaim (inline 1+z 1-z z-1 z+1))
(defun 1+z (z)
  (complex (+ 1 (realpart z)) (imagpart z)))
(defun 1-z (z)
  (complex (- 1 (realpart z)) (- (imagpart z))))
(defun z-1 (z)
  (complex (- (realpart z) 1) (imagpart z)))
(defun z+1 (z)
  (complex (+ (realpart z) 1) (imagpart z)))

(defun complex-acos (z)
  "Compute acos z = pi/2 - asin z

Z may be any number, but the result is always a complex."
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-acos (dd-complex-acos z)))
  (if (and (realp z) (> z 1))
      ;; acos is continuous in quadrant IV in this case.
      (complex-acos (complex z -0f0))
      (let ((sqrt-1+z (complex-sqrt (1+z z)))
	    (sqrt-1-z (complex-sqrt (1-z z))))
	(with-float-traps-masked (:divide-by-zero)
	  (complex (* 2 (atan (/ (realpart sqrt-1-z)
				 (realpart sqrt-1+z))))
		   (asinh (imagpart (* (conjugate sqrt-1+z)
				       sqrt-1-z))))))))

(defun complex-acosh (z)
  "Compute acosh z = 2 * log(sqrt((z+1)/2) + sqrt((z-1)/2))

Z may be any number, but the result is always a complex."
  (declare (number z))
  (let ((sqrt-z-1 (complex-sqrt (z-1 z)))
	(sqrt-z+1 (complex-sqrt (z+1 z))))
    (with-float-traps-masked (:divide-by-zero)
      (complex (asinh (realpart (* (conjugate sqrt-z-1)
				   sqrt-z+1)))
	       (* 2 (atan (/ (imagpart sqrt-z-1)
			     (realpart sqrt-z+1))))))))


(defun complex-asin (z)
  "Compute asin z = asinh(i*z)/i

Z may be any number, but the result is always a complex."
  (declare (number z))
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-asin (dd-complex-asin z)))
  (if (and (realp z) (> z 1))
      ;; asin is continuous in quadrant IV in this case.
      (complex-asin (complex z -0f0))
      (let ((sqrt-1-z (complex-sqrt (1-z z)))
	    (sqrt-1+z (complex-sqrt (1+z z))))
	(with-float-traps-masked (:divide-by-zero)
	  (complex (atan (/ (realpart z)
			    (realpart (* sqrt-1-z sqrt-1+z))))
		   (asinh (imagpart (* (conjugate sqrt-1-z)
				       sqrt-1+z))))))))

(defun complex-asinh (z)
  "Compute asinh z = log(z + sqrt(1 + z*z))

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; asinh z = -i * asin (i*z)
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-asinh (dd-complex-asinh z)))
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-asin iz)))
    (complex (imagpart result)
	     (- (realpart result)))))
	 
(defun complex-atan (z)
  "Compute atan z = atanh (i*z) / i

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; atan z = -i * atanh (i*z)
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-atan (dd-complex-atan z)))
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-atanh iz)))
    (complex (imagpart result)
	     (- (realpart result)))))

(defun complex-tan (z)
  "Compute tan z = -i * tanh(i * z)

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; tan z = -i * tanh(i*z)
  #+double-double
  (when (typep z '(or double-double-float (complex double-double-float)))
    (return-from complex-tan (dd-complex-tan z)))
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-tanh iz)))
    (complex (imagpart result)
	     (- (realpart result)))))

