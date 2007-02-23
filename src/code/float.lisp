;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/float.lisp,v 1.33 2006/08/21 16:39:53 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the definitions of float specific number support
;;; (other than irrational stuff, which is in irrat.)  There is code in here
;;; that assumes there are only two float formats: IEEE single and double.
;;;
;;; Author: Rob MacLachlan
;;; Long-float support by Douglas Crosher, 1998.
;;; 
(in-package "KERNEL")
(export '(%unary-truncate %unary-round %unary-ftruncate
	  %unary-ftruncate/single-float %unary-ftruncate/double-float))

(in-package "LISP")
(export '(least-positive-normalized-short-float
	  least-positive-normalized-single-float
	  least-positive-normalized-double-float
	  least-positive-normalized-long-float
	  least-negative-normalized-short-float
	  least-negative-normalized-single-float
	  least-negative-normalized-double-float
	  least-negative-normalized-long-float
	  least-positive-single-float
	  least-positive-short-float
	  least-negative-single-float
	  least-negative-short-float
	  least-positive-double-float
	  least-positive-long-float
	  least-negative-double-float
	  least-negative-long-float
	  most-positive-single-float
	  most-positive-short-float
	  most-negative-single-float
	  most-negative-short-float
	  most-positive-double-float
	  most-positive-long-float
	  most-negative-double-float
	  most-negative-long-float))

(in-package "EXTENSIONS")
(export '(single-float-positive-infinity short-float-positive-infinity
	  double-float-positive-infinity long-float-positive-infinity
	  single-float-negative-infinity short-float-negative-infinity
	  double-float-negative-infinity long-float-negative-infinity
	  set-floating-point-modes float-denormalized-p float-nan-p
	  float-trapping-nan-p float-infinity-p))

(in-package "KERNEL")


;;;; Utilities:

;;; SINGLE-FROM-BITS, DOUBLE-FROM-BITS  --  Internal
;;;
;;;    These functions let us create floats from bits with the significand
;;; uniformly represented as an integer.  This is less efficient for double
;;; floats, but is more convenient when making special values, etc.
;;;
(defun single-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 24) sig)
	   (type (unsigned-byte 8) exp))
  (make-single-float
   (dpb exp vm:single-float-exponent-byte
	(dpb sig vm:single-float-significand-byte
	     (if (zerop sign) 0 -1)))))
;;;
(defun double-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 53) sig)
	   (type (unsigned-byte 11) exp))
  (make-double-float (dpb exp vm:double-float-exponent-byte
			  (dpb (ash sig -32) vm:double-float-significand-byte
			       (if (zerop sign) 0 -1)))
		     (ldb (byte 32 0) sig)))

#+double-double
(defun double-double-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte #.vm:double-double-float-digits) sig)
	   (type (unsigned-byte 11) exp))
  (let ((lo (ldb (byte vm:double-float-digits 0) sig))
	(hi (ldb (byte vm:double-float-digits vm:double-float-digits) sig)))
    ;; We can't return a double-double because exp might be very
    ;; small, and the low part of the double-double would get scaled
    ;; to 0.  So we return both parts and expect the caller to figure
    ;; out what to do.
    (values (scale-float (float (* hi (- 1 (* 2 sign))) 1d0) #.(- vm:double-float-digits))
	    (scale-float (float lo 1d0) #.(- vm:double-float-digits)))))
;;;
#+(and long-float x86)
(defun long-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 64) sig)
	   (type (unsigned-byte 15) exp))
  (make-long-float (logior (ash sign 15) exp)
		   (ldb (byte 32 32) sig)
		   (ldb (byte 32 0) sig)))
					

;;;; Float parameters:

(defconstant least-positive-single-float (single-from-bits 0 0 1))
(defconstant least-positive-short-float least-positive-single-float)
(defconstant least-negative-single-float (single-from-bits 1 0 1))
(defconstant least-negative-short-float least-negative-single-float)
(defconstant least-positive-double-float (double-from-bits 0 0 1))
#-long-float
(defconstant least-positive-long-float least-positive-double-float)
#+(and long-float x86)
(defconstant least-positive-long-float (long-from-bits 0 0 1))
(defconstant least-negative-double-float (double-from-bits 1 0 1))
#-long-float
(defconstant least-negative-long-float least-negative-double-float)
#+(and long-float x86)
(defconstant least-negative-long-float (long-from-bits 1 0 1))

(defconstant least-positive-normalized-single-float
  (single-from-bits 0 vm:single-float-normal-exponent-min 0))
(defconstant least-positive-normalized-short-float
  least-positive-normalized-single-float)
(defconstant least-negative-normalized-single-float
  (single-from-bits 1 vm:single-float-normal-exponent-min 0))
(defconstant least-negative-normalized-short-float
  least-negative-normalized-single-float)
(defconstant least-positive-normalized-double-float
  (double-from-bits 0 vm:double-float-normal-exponent-min 0))
#-long-float
(defconstant least-positive-normalized-long-float
  least-positive-normalized-double-float)
#+(and long-float x86)
(defconstant least-positive-normalized-long-float
  (long-from-bits 0 vm:long-float-normal-exponent-min
		  (ash vm:long-float-hidden-bit 32)))
(defconstant least-negative-normalized-double-float
  (double-from-bits 1 vm:double-float-normal-exponent-min 0))
#-long-float
(defconstant least-negative-normalized-long-float
  least-negative-normalized-double-float)
#+(and long-float x86)
(defconstant least-negative-normalized-long-float
  (long-from-bits 1 vm:long-float-normal-exponent-min
		  (ash vm:long-float-hidden-bit 32)))

(defconstant most-positive-single-float
  (single-from-bits 0 vm:single-float-normal-exponent-max
		    (ldb vm:single-float-significand-byte -1)))
(defconstant most-positive-short-float most-positive-single-float)
(defconstant most-negative-single-float
  (single-from-bits 1 vm:single-float-normal-exponent-max
		    (ldb vm:single-float-significand-byte -1)))
(defconstant most-negative-short-float most-negative-single-float)
(defconstant most-positive-double-float
  (double-from-bits 0 vm:double-float-normal-exponent-max
		    (ldb (byte vm:double-float-digits 0) -1)))
#-long-float
(defconstant most-positive-long-float most-positive-double-float)
#+(and long-float x86)
(defconstant most-positive-long-float
  (long-from-bits 0 vm:long-float-normal-exponent-max
		  (ldb (byte vm:long-float-digits 0) -1)))
(defconstant most-negative-double-float
  (double-from-bits 1 vm:double-float-normal-exponent-max
		    (ldb (byte vm:double-float-digits 0) -1)))
#-long-float
(defconstant most-negative-long-float most-negative-double-float)
#+(and long-float x86)
(defconstant most-negative-long-float
  (long-from-bits 1 vm:long-float-normal-exponent-max
		  (ldb (byte vm:long-float-digits 0) -1)))

(defconstant single-float-positive-infinity
  (single-from-bits 0 (1+ vm:single-float-normal-exponent-max) 0))
(defconstant short-float-positive-infinity single-float-positive-infinity)
(defconstant single-float-negative-infinity
  (single-from-bits 1 (1+ vm:single-float-normal-exponent-max) 0))
(defconstant short-float-negative-infinity single-float-negative-infinity)
(defconstant double-float-positive-infinity
  (double-from-bits 0 (1+ vm:double-float-normal-exponent-max) 0))
#-long-float
(defconstant long-float-positive-infinity double-float-positive-infinity)
#+(and long-float x86)
(defconstant long-float-positive-infinity
  (long-from-bits 0 (1+ vm:long-float-normal-exponent-max)
		  (ash vm:long-float-hidden-bit 32)))
(defconstant double-float-negative-infinity
  (double-from-bits 1 (1+ vm:double-float-normal-exponent-max) 0))
#-long-float
(defconstant long-float-negative-infinity double-float-negative-infinity)
#+(and long-float x86)
(defconstant long-float-negative-infinity
  (long-from-bits 1 (1+ vm:long-float-normal-exponent-max)
		  (ash vm:long-float-hidden-bit 32)))

(defconstant single-float-epsilon
  (single-from-bits 0 (- vm:single-float-bias (1- vm:single-float-digits)) 1))
(defconstant short-float-epsilon single-float-epsilon)
(defconstant single-float-negative-epsilon
  (single-from-bits 0 (- vm:single-float-bias vm:single-float-digits) 1))
(defconstant short-float-negative-epsilon single-float-negative-epsilon)
#-(and long-float x86)
(defconstant double-float-epsilon
  (double-from-bits 0 (- vm:double-float-bias (1- vm:double-float-digits)) 1))
#+(and long-float x86)
(defconstant double-float-epsilon
  (double-from-bits 0 (- vm:double-float-bias (1- vm:double-float-digits))
		    (expt 2 42)))
#-long-float
(defconstant long-float-epsilon double-float-epsilon)
#+(and long-float x86)
(defconstant long-float-epsilon
  (long-from-bits 0 (- vm:long-float-bias (1- vm:long-float-digits))
		  (+ 1 (ash vm:long-float-hidden-bit 32))))
#-(and long-float x86)
(defconstant double-float-negative-epsilon
  (double-from-bits 0 (- vm:double-float-bias vm:double-float-digits) 1))
#+(and long-float x86)
(defconstant double-float-negative-epsilon
  (double-from-bits 0 (- vm:double-float-bias vm:double-float-digits)
		    (expt 2 42)))
#-long-float
(defconstant long-float-negative-epsilon double-float-negative-epsilon)
#+(and long-float x86)
(defconstant long-float-negative-epsilon
  (long-from-bits 0 (- vm:long-float-bias vm:long-float-digits)
		  (+ 1 (ash vm:long-float-hidden-bit 32))))


;;;; Float predicates and environment query:

(declaim (maybe-inline float-denormalized-p float-infinity-p float-nan-p
			 float-trapping-nan-p))

;;; FLOAT-DENORMALIZED-P  --  Public
;;;
(defun float-denormalized-p (x)
  "Return true if the float X is denormalized."
  (number-dispatch ((x float))
    ((single-float)
     (and (zerop (ldb vm:single-float-exponent-byte (single-float-bits x)))
	  (not (zerop x))))
    ((double-float)
     (and (zerop (ldb vm:double-float-exponent-byte
		      (double-float-high-bits x)))
	  (not (zerop x))))
    #+(and long-float x86)
    ((long-float)
     (and (zerop (ldb vm:long-float-exponent-byte (long-float-exp-bits x)))
	  (not (zerop x))))))

(macrolet ((frob (name doc single double #+(and long-float x86) long
		       #+double-double double-double)
	     `(defun ,name (x)
		,doc
		(number-dispatch ((x float))
		  ((single-float)
		   (let ((bits (single-float-bits x)))
		     (and (> (ldb vm:single-float-exponent-byte bits)
			     vm:single-float-normal-exponent-max)
			  ,single)))
		  ((double-float)
		   (let ((hi (double-float-high-bits x))
			 (lo (double-float-low-bits x)))
		     (declare (ignorable lo))
		     (and (> (ldb vm:double-float-exponent-byte hi)
			     vm:double-float-normal-exponent-max)
			  ,double)))
		  #+(and long-float x86)
		  ((long-float)
		   (let ((exp (long-float-exp-bits x))
			 (hi (long-float-high-bits x))
			 (lo (long-float-low-bits x)))
		     (declare (ignorable lo))
		     (and (> (ldb vm:long-float-exponent-byte exp)
			     vm:long-float-normal-exponent-max)
			  ,long)))
		  #+double-double
		  ((double-double-float)
		   ,double-double)))))

  (frob float-infinity-p "Return true if the float X is an infinity (+ or -)."
    (zerop (ldb vm:single-float-significand-byte bits))
    (and (zerop (ldb vm:double-float-significand-byte hi))
	 (zerop lo))
    #+(and long-float x86)
    (and (zerop (ldb vm:long-float-significand-byte hi))
	 (zerop lo))
    #+double-double
    (float-infinity-p (double-double-hi x)))

  (frob float-nan-p "Return true if the float X is a NaN (Not a Number)."
    (not (zerop (ldb vm:single-float-significand-byte bits)))
    (or (not (zerop (ldb vm:double-float-significand-byte hi)))
	(not (zerop lo)))
    #+(and long-float x86)
    (or (not (zerop (ldb vm:long-float-significand-byte hi)))
	(not (zerop lo)))
    #+double-double
    (float-nan-p (double-double-hi x)))

  (frob float-trapping-nan-p
    "Return true if the float X is a trapping NaN (Not a Number)."
    (zerop (logand (ldb vm:single-float-significand-byte bits)
		   vm:single-float-trapping-nan-bit))
    (zerop (logand (ldb vm:double-float-significand-byte hi)
		   vm:double-float-trapping-nan-bit))
    #+(and long-float x86)
    (zerop (logand (ldb vm:long-float-significand-byte hi)
		   vm:long-float-trapping-nan-bit))
    #+double-double
    (float-trapping-nan-p (double-double-hi x))))


;;; FLOAT-PRECISION  --  Public
;;;
;;;    If denormalized, use a subfunction from INTEGER-DECODE-FLOAT to find the
;;; actual exponent (and hence how denormalized it is), otherwise we just
;;; return the number of digits or 0.
;;;
(declaim (maybe-inline float-precision))
(defun float-precision (f)
  "Returns a non-negative number of significant digits in it's float argument.
  Will be less than FLOAT-DIGITS if denormalized or zero."
  (macrolet ((frob (digits bias decode)
	       `(cond ((zerop f) 0)
		      ((float-denormalized-p f)
		       (multiple-value-bind (ignore exp)
					    (,decode f)
			 (declare (ignore ignore))
			 (truly-the fixnum
				    (+ ,digits (1- ,digits) ,bias exp))))
		      (t
		       ,digits))))
    (number-dispatch ((f float))
      ((single-float)
       (frob vm:single-float-digits vm:single-float-bias
	 integer-decode-single-denorm))
      ((double-float)
       (frob vm:double-float-digits vm:double-float-bias
	 integer-decode-double-denorm))
      #+long-float
      ((long-float)
       (frob vm:long-float-digits vm:long-float-bias
	 integer-decode-long-denorm)))))


#+nil
(defun float-sign (float1 &optional (float2 (float 1 float1)))
  "Returns a floating-point number that has the same sign as
   float1 and, if float2 is given, has the same absolute value
   as float2."
  (declare (float float1 float2))
  (* (if (etypecase float1
	   (single-float (minusp (single-float-bits float1)))
	   (double-float (minusp (double-float-high-bits float1)))
	   #+long-float
	   (long-float (minusp (long-float-exp-bits float1)))
	   #+double-double
	   (double-double-float (minusp (double-double-hi float1))))
	 (float -1 float1)
	 (float 1 float1))
     (abs float2)))

(defun float-sign (float1 &optional float2)
  "Returns a floating-point number that has the same sign as
   float1 and, if float2 is given, has the same absolute value
   as float2."
  (declare (float float1)
	   (type (or null float) float2))
  (let ((f1-sign (if (etypecase float1
		       (single-float (minusp (single-float-bits float1)))
		       (double-float (minusp (double-float-high-bits float1)))
		       #+long-float
		       (long-float (minusp (long-float-exp-bits float1)))
		       #+double-double
		       (double-double-float (minusp (float-sign (double-double-hi float1)))))
		     (float -1 float1)
		     (float 1 float1))))
    ;; Multiplication of double-double-float's doesn't preserve the
    ;; sign of signed-zeroes, so we split the case of float2 this way
    ;; so we can get the right sign.
    (if float2
	(if (minusp f1-sign)
	    (- (abs float2))
	    (abs float2))
	f1-sign)))

(defun float-format-digits (format)
  (ecase format
    ((short-float single-float) vm:single-float-digits)
    ((double-float #-long-float long-float) vm:double-float-digits)
    #+long-float
    (long-float vm:long-float-digits)
    #+double-double
    (double-double-float vm:double-double-float-digits)))

(declaim (inline float-digits float-radix))

(defun float-digits (f)
  "Returns a non-negative number of radix-b digits used in the
   representation of it's argument.  See Common Lisp: The Language
   by Guy Steele for more details."
  (number-dispatch ((f float))
    ((single-float) vm:single-float-digits)
    ((double-float) vm:double-float-digits)
    #+long-float
    ((long-float) vm:long-float-digits)
    #+double-double
    ((double-double-float) vm:double-double-float-digits)))

(defun float-radix (f)
  "Returns (as an integer) the radix b of its floating-point
   argument."
  (number-dispatch ((f float))
    ((float) 2)))



;;;; INTEGER-DECODE-FLOAT and DECODE-FLOAT:

(declaim (maybe-inline integer-decode-single-float
		       integer-decode-double-float))

;;; INTEGER-DECODE-SINGLE-DENORM  --  Internal
;;;
;;;    Handle the denormalized case of INTEGER-DECODE-FLOAT for SINGLE-FLOAT.
;;;
(defun integer-decode-single-denorm (x)
  (declare (type single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (sig (ash (ldb vm:single-float-significand-byte bits) 1))
	 (extra-bias 0))
    (declare (type (unsigned-byte 24) sig)
	     (type (integer 0 23) extra-bias))
    (loop
      (unless (zerop (logand sig vm:single-float-hidden-bit))
	(return))
      (setq sig (ash sig 1))
      (incf extra-bias))
    (values sig
	    (- (- vm:single-float-bias) vm:single-float-digits extra-bias)
	    (if (minusp (float-sign x)) -1 1))))


;;; INTEGER-DECODE-SINGLE-FLOAT  --  Internal
;;;
;;;    Handle the single-float case of INTEGER-DECODE-FLOAT.  If an infinity or
;;; NAN, error.  If a denorm, call i-d-s-DENORM to handle it.
;;;
(defun integer-decode-single-float (x)
  (declare (single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (exp (ldb vm:single-float-exponent-byte bits))
	 (sig (ldb vm:single-float-significand-byte bits))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- exp vm:single-float-bias vm:single-float-digits)))
    (declare (fixnum biased))
    (unless (<= exp vm:single-float-normal-exponent-max)
      (error "Can't decode NAN or infinity: ~S." x))
    (cond ((and (zerop exp) (zerop sig))
	   (values 0 biased sign))
	  ((< exp vm:single-float-normal-exponent-min)
	   (integer-decode-single-denorm x))
	  (t
	   (values (logior sig vm:single-float-hidden-bit) biased sign)))))


;;; INTEGER-DECODE-DOUBLE-DENORM  --  Internal
;;;
;;;    Like INTEGER-DECODE-SINGLE-DENORM, only doubly so.
;;;
(defun integer-decode-double-denorm (x)
  (declare (type double-float x))
  (let* ((high-bits (double-float-high-bits (abs x)))
	 (sig-high (ldb vm:double-float-significand-byte high-bits))
	 (low-bits (double-float-low-bits x))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- (- vm:double-float-bias) vm:double-float-digits)))
    (if (zerop sig-high)
	(let ((sig low-bits)
	      (extra-bias (- vm:double-float-digits 33))
	      (bit (ash 1 31)))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig bit)) (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (ash sig (- vm:double-float-digits 32))
		  (truly-the fixnum (- biased extra-bias))
		  sign))
	(let ((sig (ash sig-high 1))
	      (extra-bias 0))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig vm:double-float-hidden-bit))
	      (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (logior (ash sig 32) (ash low-bits (1- extra-bias)))
		  (truly-the fixnum (- biased extra-bias))
		  sign)))))


;;; INTEGER-DECODE-DOUBLE-FLOAT  --  Internal
;;;
;;;    Like INTEGER-DECODE-SINGLE-FLOAT, only doubly so.
;;;
(defun integer-decode-double-float (x)
  (declare (double-float x))
  (let* ((abs (abs x))
	 (hi (double-float-high-bits abs))
	 (lo (double-float-low-bits abs))
	 (exp (ldb vm:double-float-exponent-byte hi))
	 (sig (ldb vm:double-float-significand-byte hi))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- exp vm:double-float-bias vm:double-float-digits)))
    (declare (fixnum biased))
    (unless (<= exp vm:double-float-normal-exponent-max)
      (error "Can't decode NAN or infinity: ~S." x))
    (cond ((and (zerop exp) (zerop sig) (zerop lo))
	   (values 0 biased sign))
	  ((< exp vm:double-float-normal-exponent-min)
	   (integer-decode-double-denorm x))
	  (t
	   (values
	    (logior (ash (logior (ldb vm:double-float-significand-byte hi)
				 vm:double-float-hidden-bit)
			 32)
		    lo)
	    biased sign)))))


;;; INTEGER-DECODE-LONG-DENORM  --  Internal
;;;
#+(and long-float x86)
(defun integer-decode-long-denorm (x)
  (declare (type long-float x))
  (let* ((high-bits (long-float-high-bits (abs x)))
	 (sig-high (ldb vm:long-float-significand-byte high-bits))
	 (low-bits (long-float-low-bits x))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- (- vm:long-float-bias) vm:long-float-digits)))
    (if (zerop sig-high)
	(let ((sig low-bits)
	      (extra-bias (- vm:long-float-digits 33))
	      (bit (ash 1 31)))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig bit)) (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (ash sig (- vm:long-float-digits 32))
		  (truly-the fixnum (- biased extra-bias))
		  sign))
	(let ((sig (ash sig-high 1))
	      (extra-bias 0))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig vm:long-float-hidden-bit))
	      (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (logior (ash sig 32) (ash low-bits (1- extra-bias)))
		  (truly-the fixnum (- biased extra-bias))
		  sign)))))

#+double-double
(defun integer-decode-double-double-float (x)
  (declare (type double-double-float x))
  (multiple-value-bind (hi-int hi-exp sign)
      (integer-decode-float (double-double-hi x))
    (if (zerop (double-double-lo x))
	(values (ash hi-int 53) (- hi-exp 53) sign)
	(multiple-value-bind (lo-int lo-exp lo-sign)
	    (integer-decode-float (double-double-lo x))
	  ;; We have x = 2^e1*i1 + 2^e2*i2
	  ;;           = 2^e2*(2^(e1-e2)*i1 + i2)
	  (values (+ (* lo-sign lo-int)
		     (ash hi-int (- hi-exp lo-exp)))
		  lo-exp
		  sign)))))

;;; INTEGER-DECODE-LONG-FLOAT  --  Internal
;;;
#+(and long-float x86)
(defun integer-decode-long-float (x)
  (declare (long-float x))
  (let* ((hi (long-float-high-bits x))
	 (lo (long-float-low-bits x))
	 (exp-bits (long-float-exp-bits x))
	 (exp (ldb vm:long-float-exponent-byte exp-bits))
	 (sign (if (minusp exp-bits) -1 1))
	 (biased (- exp vm:long-float-bias vm:long-float-digits)))
    (declare (fixnum biased))
    (unless (<= exp vm:long-float-normal-exponent-max)
      (error "Can't decode NAN or infinity: ~S." x))
    (cond ((and (zerop exp) (zerop hi) (zerop lo))
	   (values 0 biased sign))
	  ((< exp vm:long-float-normal-exponent-min)
	   (integer-decode-long-denorm x))
	  (t
	   (values (logior (ash hi 32) lo) biased sign)))))


;;; INTEGER-DECODE-FLOAT  --  Public
;;;
;;;    Dispatch to the correct type-specific i-d-f function.
;;;
(defun integer-decode-float (x)
  "Returns three values:
   1) an integer representation of the significand.
   2) the exponent for the power of 2 that the significand must be multiplied
      by to get the actual value.  This differs from the DECODE-FLOAT exponent
      by FLOAT-DIGITS, since the significand has been scaled to have all its
      digits before the radix point.
   3) -1 or 1 (i.e. the sign of the argument.)"
  (number-dispatch ((x float))
    ((single-float)
     (integer-decode-single-float x))
    ((double-float)
     (integer-decode-double-float x))
    #+long-float
    ((long-float)
     (integer-decode-long-float x))
    #+double-double
    ((double-double-float)
     (integer-decode-double-double-float x))))


(declaim (maybe-inline decode-single-float decode-double-float))

;;; DECODE-SINGLE-DENORM  --  Internal
;;;
;;;    Handle the denormalized case of DECODE-SINGLE-FLOAT.  We call
;;; INTEGER-DECODE-SINGLE-DENORM and then make the result into a float.
;;;
(defun decode-single-denorm (x)
  (declare (type single-float x))
  (multiple-value-bind (sig exp sign)
		       (integer-decode-single-denorm x)
    (values (make-single-float
	     (dpb sig vm:single-float-significand-byte
		  (dpb vm:single-float-bias vm:single-float-exponent-byte 0)))
	    (truly-the fixnum (+ exp vm:single-float-digits))
	    (float sign x))))


;;; DECODE-SINGLE-FLOAT  --  Internal
;;;
;;;    Handle the single-float case of DECODE-FLOAT.  If an infinity or NAN,
;;; error.  If a denorm, call d-s-DENORM to handle it.
;;;
(defun decode-single-float (x)
  (declare (single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (exp (ldb vm:single-float-exponent-byte bits))
	 (sign (float-sign x))
	 (biased (truly-the single-float-exponent
			    (- exp vm:single-float-bias))))
    (unless (<= exp vm:single-float-normal-exponent-max) 
      (error "Can't decode NAN or infinity: ~S." x))
    (cond ((zerop x)
	   (values 0.0f0 biased sign))
	  ((< exp vm:single-float-normal-exponent-min)
	   (decode-single-denorm x))
	  (t
	   (values (make-single-float
		    (dpb vm:single-float-bias
			 vm:single-float-exponent-byte
			 bits))
		   biased sign)))))


;;; DECODE-DOUBLE-DENORM  --  Internal
;;;
;;;    Like DECODE-SINGLE-DENORM, only doubly so.
;;; 
(defun decode-double-denorm (x)
  (declare (double-float x))
  (multiple-value-bind (sig exp sign)
		       (integer-decode-double-denorm x)
    (values (make-double-float
	     (dpb (logand (ash sig -32) (lognot vm:double-float-hidden-bit))
		  vm:double-float-significand-byte
		  (dpb vm:double-float-bias vm:double-float-exponent-byte 0))
	     (ldb (byte 32 0) sig))
	    (truly-the fixnum (+ exp vm:double-float-digits))
	    (float sign x))))


;;; DECODE-DOUBLE-FLOAT  --  Public
;;;
;;;    Like DECODE-SINGLE-FLOAT, only doubly so.
;;;
(defun decode-double-float (x)
  (declare (double-float x))
  (let* ((abs (abs x))
	 (hi (double-float-high-bits abs))
	 (lo (double-float-low-bits abs))
	 (exp (ldb vm:double-float-exponent-byte hi))
	 (sign (float-sign x))
	 (biased (truly-the double-float-exponent
			    (- exp vm:double-float-bias))))
    (unless (<= exp vm:double-float-normal-exponent-max)
      (error "Can't decode NAN or infinity: ~S." x))
    (cond ((zerop x)
	   (values 0.0d0 biased sign))
	  ((< exp vm:double-float-normal-exponent-min)
	   (decode-double-denorm x))
	  (t
	   (values (make-double-float
		    (dpb vm:double-float-bias vm:double-float-exponent-byte hi)
		    lo)
		   biased sign)))))


;;; DECODE-LONG-DENORM  --  Internal
;;;
#+(and long-float x86)
(defun decode-long-denorm (x)
  (declare (long-float x))
  (multiple-value-bind (sig exp sign)
		       (integer-decode-long-denorm x)
    (values (make-long-float vm:long-float-bias (ash sig -32)
			     (ldb (byte 32 0) sig))
	    (truly-the fixnum (+ exp vm:long-float-digits))
	    (float sign x))))


;;; DECODE-LONG-FLOAT  --  Public
;;;
#+(and long-float x86)
(defun decode-long-float (x)
  (declare (long-float x))
  (let* ((hi (long-float-high-bits x))
	 (lo (long-float-low-bits x))
	 (exp-bits (long-float-exp-bits x))
	 (exp (ldb vm:long-float-exponent-byte exp-bits))
	 (sign (if (minusp exp-bits) -1l0 1l0))
	 (biased (truly-the long-float-exponent (- exp vm:long-float-bias))))
    (unless (<= exp vm:long-float-normal-exponent-max)
      (error "Can't decode NAN or infinity: ~S." x))
    (cond ((zerop x)
	   (values 0.0l0 biased sign))
	  ((< exp vm:long-float-normal-exponent-min)
	   (decode-long-denorm x))
	  (t
	   (values (make-long-float
		    (dpb vm:long-float-bias vm:long-float-exponent-byte
			 exp-bits)
		    hi
		    lo)
		   biased sign)))))

;;; DECODE-DOUBLE-DOUBLE-FLOAT -- Public
#+double-double
(defun decode-double-double-float (x)
  (declare (type double-double-float x))
  (multiple-value-bind (hi-frac hi-exp sign)
      (decode-float (double-double-hi x))
    (values (make-double-double-float hi-frac
				      (scale-float (double-double-lo x) (- hi-exp)))
	    hi-exp
	    (coerce sign 'double-double-float))))

;;; DECODE-FLOAT  --  Public
;;;
;;;    Dispatch to the appropriate type-specific function.
;;;
(defun decode-float (f)
  "Returns three values:
   1) a floating-point number representing the significand.  This is always
      between 0.5 (inclusive) and 1.0 (exclusive).
   2) an integer representing the exponent.
   3) -1.0 or 1.0 (i.e. the sign of the argument.)"
  (number-dispatch ((f float))
    ((single-float)
     (decode-single-float f))
    ((double-float)
     (decode-double-float f))
    #+long-float
    ((long-float)
     (decode-long-float f))
    #+double-double
    ((double-double-float)
     (decode-double-double-float f))))


;;;; SCALE-FLOAT:

(declaim (maybe-inline scale-single-float scale-double-float))

;;; SCALE-FLOAT-MAYBE-UNDERFLOW  --  Internal
;;;
;;;    Handle float scaling where the X is denormalized or the result is
;;; denormalized or underflows to 0.
;;;
(defun scale-float-maybe-underflow (x exp)
  (multiple-value-bind (sig old-exp)
		       (integer-decode-float x)
    (let* ((digits (float-digits x))
	   (new-exp (+ exp old-exp digits
		       (etypecase x
			 (single-float vm:single-float-bias)
			 (double-float vm:double-float-bias))))
	   (sign (if (minusp (float-sign x)) 1 0)))
      (cond
       ((< new-exp
	   (etypecase x
	     (single-float vm:single-float-normal-exponent-min)
	     (double-float vm:double-float-normal-exponent-min)))
	(when (vm:current-float-trap :inexact)
	  (error 'floating-point-inexact :operation 'scale-float
		 :operands (list x exp)))
	(when (vm:current-float-trap :underflow)
	  (error 'floating-point-underflow :operation 'scale-float
		 :operands (list x exp)))
	(let ((shift (1- new-exp)))
	  (if (< shift (- (1- digits)))
	      (float-sign x 0.0)
	      (etypecase x
		(single-float (single-from-bits sign 0 (ash sig shift)))
		(double-float (double-from-bits sign 0 (ash sig shift)))))))
       (t
	(etypecase x
	  (single-float (single-from-bits sign new-exp sig))
	  (double-float (double-from-bits sign new-exp sig))))))))


;;; SCALE-FLOAT-MAYBE-OVERFLOW  --  Internal
;;;
;;;    Called when scaling a float overflows, or the oringinal float was a NaN
;;; or infinity.  If overflow errors are trapped, then error, otherwise return
;;; the appropriate infinity.  If a NaN, signal or not as appropriate.
;;;
(defun scale-float-maybe-overflow (x exp)
  (cond
   ((float-infinity-p x)
    ;; Infinity is infinity, no matter how small...
    x)
   ((float-nan-p x)
    (when (and (float-trapping-nan-p x)
	       (vm:current-float-trap :invalid))
      (error 'floating-point-invalid-operation :operation 'scale-float
	     :operands (list x exp)))
    x)
   (t
    (when (vm:current-float-trap :overflow)
      (error 'floating-point-overflow :operation 'scale-float
	     :operands (list x exp)))
    (when (vm:current-float-trap :inexact)
      (error 'floating-point-inexact :operation 'scale-float
	     :operands (list x exp)))
    (* (float-sign x)
       (etypecase x
	 (single-float single-float-positive-infinity)
	 (double-float double-float-positive-infinity))))))


;;; SCALE-SINGLE-FLOAT, SCALE-DOUBLE-FLOAT  --  Internal
;;;
;;;    Scale a single or double float, calling the correct over/underflow
;;; functions.
;;;
(defun scale-single-float (x exp)
  (declare (single-float x) (fixnum exp))
  (let* ((bits (single-float-bits x))
	 (old-exp (ldb vm:single-float-exponent-byte bits))
	 (new-exp (+ old-exp exp)))
    (cond
     ((zerop x) x)
     ((or (< old-exp vm:single-float-normal-exponent-min)
	  (< new-exp vm:single-float-normal-exponent-min))
      (scale-float-maybe-underflow x exp))
     ((or (> old-exp vm:single-float-normal-exponent-max)
	  (> new-exp vm:single-float-normal-exponent-max))
      (scale-float-maybe-overflow x exp))
     (t
      (make-single-float (dpb new-exp vm:single-float-exponent-byte bits))))))
;;;
(defun scale-double-float (x exp)
  (declare (double-float x) (fixnum exp))
  (let* ((hi (double-float-high-bits x))
	 (lo (double-float-low-bits x))
	 (old-exp (ldb vm:double-float-exponent-byte hi))
	 (new-exp (+ old-exp exp)))
    (cond
     ((zerop x) x)
     ((or (< old-exp vm:double-float-normal-exponent-min)
	  (< new-exp vm:double-float-normal-exponent-min))
      (scale-float-maybe-underflow x exp))
     ((or (> old-exp vm:double-float-normal-exponent-max)
	  (> new-exp vm:double-float-normal-exponent-max))
      (scale-float-maybe-overflow x exp))
     (t
      (make-double-float (dpb new-exp vm:double-float-exponent-byte hi)
			 lo)))))

#+(and x86 long-float)
(defun scale-long-float (x exp)
  (declare (long-float x) (fixnum exp))
  (scale-float x exp))

#+double-double
(defun scale-double-double-float (x exp)
  (declare (type double-double-float x) (fixnum exp))
  (let ((hi (double-double-hi x))
	(lo (double-double-lo x)))
    (make-double-double-float (scale-double-float hi exp)
			      (scale-double-float lo exp))))

;;; SCALE-FLOAT  --  Public
;;;
;;;    Dispatch to the correct type-specific scale-float function.
;;;
(defun scale-float (f ex)
  "Returns the value (* f (expt (float 2 f) ex)), but with no unnecessary loss
  of precision or overflow."
  (number-dispatch ((f float))
    ((single-float)
     (scale-single-float f ex))
    ((double-float)
     (scale-double-float f ex))
    #+long-float
    ((long-float)
     (scale-long-float f ex))
    #+double-double
    ((double-double-float)
     (scale-double-double-float f ex))))


;;;; Converting to/from floats:

(defun float (number &optional (other () otherp))
  "Converts any REAL to a float.  If OTHER is not provided, it returns a
  SINGLE-FLOAT if NUMBER is not already a FLOAT.  If OTHER is provided, the
  result is the same float format as OTHER."
  (if otherp
      (number-dispatch ((number real) (other float))
	(((foreach rational single-float double-float #+long-float long-float
		   #+double-double double-double-float)
	  (foreach single-float double-float #+long-float long-float
		   #+double-double double-double-float))
	 (coerce number '(dispatch-type other))))
      (if (floatp number)
	  number
	  (coerce number 'single-float))))


(macrolet ((frob (name type)
	     `(defun ,name (x)
		(number-dispatch ((x real))
		  (((foreach single-float double-float
			     #+long-float long-float
			     #+double-double double-double-float
			     fixnum))
		   (coerce x ',type))
		  ((bignum)
		   (bignum-to-float x ',type))
		  ((ratio)
		   (float-ratio x ',type))))))
  (frob %single-float single-float)
  (frob %double-float double-float)
  #+long-float
  (frob %long-float long-float)
  #+(and nil double-double)
  (frob %double-double-float double-double-float))


;;; FLOAT-RATIO  --  Internal
;;;
;;;    Convert a ratio to a float.  We avoid any rounding error by doing an
;;; integer division.  Accuracy is important to preserve read/print
;;; consistency, since this is ultimately how the reader reads a float.  We
;;; scale the numerator by a power of two until the division results in the
;;; desired number of fraction bits, then do round-to-nearest.
;;;
(defun float-ratio (x format)
  (let* ((signed-num (numerator x))
	 (plusp (plusp signed-num))
	 (num (if plusp signed-num (- signed-num)))
	 (den (denominator x))
	 (digits (float-format-digits format))
	 (scale 0))
    (declare (fixnum digits scale))
    ;;
    ;; Strip any trailing zeros from the denominator and move it into the scale
    ;; factor (to minimize the size of the operands.)
    (let ((den-twos (1- (integer-length (logxor den (1- den))))))
      (declare (fixnum den-twos))
      (decf scale den-twos)
      (setq den (ash den (- den-twos))))
    ;;
    ;; Guess how much we need to scale by from the magnitudes of the numerator
    ;; and denominator.  We want one extra bit for a guard bit.
    (let* ((num-len (integer-length num))
	   (den-len (integer-length den))
	   (delta (- den-len num-len))
	   (shift (1+ (the fixnum (+ delta digits))))
	   (shifted-num (ash num shift)))
      (declare (fixnum delta shift))
      (decf scale delta)
      (labels ((float-and-scale (bits)
		 (let* ((bits (ash bits -1))
			(len (integer-length bits)))
		   (cond ((> len digits)
			  (assert (= len (the fixnum (1+ digits))))
			  (multiple-value-bind (f0 f1)
			      (floatit (ash bits -1))
			    #||
			    (format t "1: f0, f1 = ~A ~A~%" f0 f1)
			    (format t "   scale = ~A~%" (1+ scale))
			    ||#
			    (if f1
				(%make-double-double-float
				 (scale-float f0 (1+ scale))
				 (scale-float f1 (+ 1 scale #.(- vm:double-float-digits))))
				(scale-float f0 (1+ scale)))))
			 (t
			  (multiple-value-bind (f0 f1)
			      (floatit bits)
			    #||
			    (format t "2: f0, f1 = ~A ~A~%" f0 f1)
			    (format t "   scale = ~A~%" scale)
			    (format t "scale-float f0 = ~A~%" (scale-float f0 scale))
			    (when f1
			      (format t "scale-float f1 = ~A~%" (scale-float f1 (- scale 53))))
			    ||#
			    (if f1
				(make-double-double-float
				 (scale-float f0 scale)
				 (scale-float f1 (- scale #.vm:double-float-digits)))
				(scale-float f0 scale)))))))
	       (floatit (bits)
		 (let ((sign (if plusp 0 1)))
		   (case format
		     (single-float
		      (single-from-bits sign vm:single-float-bias bits))
		     (double-float
		      (double-from-bits sign vm:double-float-bias bits))
		     #+long-float
		     (long-float
		      (long-from-bits sign vm:long-float-bias bits))
		     #+double-double
		     (double-double-float
		      (double-double-from-bits sign vm:double-float-bias bits))))))
	(loop
	  (multiple-value-bind (fraction-and-guard rem)
			       (truncate shifted-num den)
	    (let ((extra (- (integer-length fraction-and-guard) digits)))
	      (declare (fixnum extra))
	      (cond ((/= extra 1)
		     (assert (> extra 1)))
		    ((oddp fraction-and-guard)
		     (return
		      (if (zerop rem)
			  (float-and-scale
			   (if (zerop (logand fraction-and-guard 2))
			       fraction-and-guard
			       (1+ fraction-and-guard)))
			  (float-and-scale (1+ fraction-and-guard)))))
		    (t
		     (return (float-and-scale fraction-and-guard)))))
	    (setq shifted-num (ash shifted-num -1))
	    (incf scale)))))))

#|
These might be useful if we ever have a machine w/o float/integer conversion
hardware.  For now, we'll use special ops that uninterruptibly frob the
rounding modes & do ieee round-to-integer.

;;; %UNARY-TRUNCATE-SINGLE-FLOAT/FIXNUM  --  Interface
;;;
;;;    The compiler compiles a call to this when we are doing %UNARY-TRUNCATE
;;; and the result is known to be a fixnum.  We can avoid some generic
;;; arithmetic in this case.
;;;
(defun %unary-truncate-single-float/fixnum (x)
  (declare (single-float x) (values fixnum))
  (locally (declare (optimize (speed 3) (safety 0)))
    (let* ((bits (single-float-bits x))
	   (exp (ldb vm:single-float-exponent-byte bits))
	   (frac (logior (ldb vm:single-float-significand-byte bits)
			 vm:single-float-hidden-bit))
	   (shift (- exp vm:single-float-digits vm:single-float-bias)))
      (when (> exp vm:single-float-normal-exponent-max)
	(error 'floating-point-invalid-operation :operator 'truncate
	       :operands (list x)))
      (if (<= shift (- vm:single-float-digits))
	  0
	  (let ((res (ash frac shift)))
	    (declare (type (unsigned-byte 31) res)) 
	    (if (minusp bits)
		(- res)
		res))))))


;;; %UNARY-TRUNCATE-DOUBLE-FLOAT/FIXNUM  --  Interface
;;;
;;;    Double-float version of this operation (see above single op).
;;;
(defun %unary-truncate-double-float/fixnum (x)
  (declare (double-float x) (values fixnum))
  (locally (declare (optimize (speed 3) (safety 0)))
    (let* ((hi-bits (double-float-high-bits x))
	   (exp (ldb vm:double-float-exponent-byte hi-bits))
	   (frac (logior (ldb vm:double-float-significand-byte hi-bits)
			 vm:double-float-hidden-bit))
	   (shift (- exp (- vm:double-float-digits vm:word-bits)
		     vm:double-float-bias)))
      (when (> exp vm:double-float-normal-exponent-max)
	(error 'floating-point-invalid-operation :operator 'truncate
	       :operands (list x)))
      (if (<= shift (- vm:word-bits vm:double-float-digits))
	  0
	  (let* ((res-hi (ash frac shift))
		 (res (if (plusp shift)
			  (logior res-hi
				  (the fixnum
				       (ash (double-float-low-bits x)
					    (- shift vm:word-bits))))
			  res-hi)))
	    (declare (type (unsigned-byte 31) res-hi res))
	    (if (minusp hi-bits)
		(- res)
		res))))))
|#

  
;;; %UNARY-TRUNCATE  --  Interface
;;;
;;;    This function is called when we are doing a truncate without any funky
;;; divisor, i.e. converting a float or ratio to an integer.  Note that we do
;;; *not* return the second value of truncate, so it must be computed by the
;;; caller if needed.
;;;
;;;    In the float case, we pick off small arguments so that compiler can use
;;; special-case operations.  We use an exclusive test, since (due to round-off
;;; error), (float most-positive-fixnum) may be greater than
;;; most-positive-fixnum.
;;;
(defun %unary-truncate (number)
  (number-dispatch ((number real))
    ((integer) number)
    ((ratio) (values (truncate (numerator number) (denominator number))))
    (((foreach single-float double-float #+long-float long-float))
     (if (< (float most-negative-fixnum number)
	    number
	    (float most-positive-fixnum number))
	 (truly-the fixnum (%unary-truncate number))
	 (multiple-value-bind (bits exp)
			      (integer-decode-float number)
	   (let ((res (ash bits exp)))
	     (if (minusp number)
		 (- res)
		 res)))))
    #+double-double
    ((double-double-float)
     (multiple-value-bind (bits exp)
	 (integer-decode-double-double-float number)
       (let ((res (ash bits exp)))
	 (if (minusp number)
	     (- res)
	     res))))))


;;; %UNARY-ROUND  --  Interface
;;;
;;;    Similar to %UNARY-TRUNCATE, but rounds to the nearest integer.  If we
;;; can't use the round primitive, then we do our own round-to-nearest on the
;;; result of i-d-f.  [Note that this rounding will really only happen with
;;; double floats, since the whole single-float fraction will fit in a fixnum,
;;; so all single-floats larger than most-positive-fixnum can be precisely
;;; represented by an integer.]
;;;
(defun %unary-round (number)
  (number-dispatch ((number real))
    ((integer) number)
    ((ratio) (values (round (numerator number) (denominator number))))
    (((foreach single-float double-float #+long-float long-float))
     (if (< (float most-negative-fixnum number)
	    number
	    (float most-positive-fixnum number))
	 (truly-the fixnum (%unary-round number))
	 (multiple-value-bind (bits exp)
	     (integer-decode-float number)
	   (let* ((shifted (ash bits exp))
		  (rounded (if (and (minusp exp)
				    (not (zerop (logand bits
							(ash 1 (- -1 exp))))))
			       (1+ shifted)
			       shifted)))
	     (if (minusp number)
		 (- rounded)
		 rounded)))))
    #+double-double
    ((double-double-float)
     (multiple-value-bind (bits exp)
	     (integer-decode-float number)
	   (let* ((shifted (ash bits exp))
		  (rounded (if (and (minusp exp)
				    (not (zerop (logand bits
							(ash 1 (- -1 exp))))))
			       (1+ shifted)
			       shifted)))
	     (if (minusp number)
		 (- rounded)
		 rounded))))))

(declaim (maybe-inline %unary-ftruncate/single-float
		       %unary-ftruncate/double-float))
;; %UNARY-FTRUNCATE/SINGLE-FLOAT
;;
;; Basically the same as ftruncate, but specialized to handle only
;; single-floats and to only return the first value.  We diddle the
;; bits directly to truncate the number to reduce consing.
(defun %unary-ftruncate/single-float (x)
  (declare (single-float x)
	   (optimize (speed 3) (safety 0)))
  (let* ((bits (kernel:single-float-bits x))
	 (exp (ldb vm:single-float-exponent-byte bits))
	 (biased (truly-the kernel:single-float-exponent
			    (- exp vm:single-float-bias))))
    (declare (type (signed-byte 32) bits))
    ;; At this point, we have the number represented as
    ;;
    ;; x = sign * 2^exp * frac
    ;;
    ;; where 0.5 <= frac < 1.0
    ;;
    ;; So if the exp <= 0, |x| < 1 and we know the result is 0.
    ;;
    ;; If exp >= (float-digits x), we know that all of the bits
    ;; in the fraction are integer bits, so the result is x.
    ;;
    ;; For everything else, some of the bits in frac are
    ;; fractional.  Figure which ones they are and set them to
    ;; zero.
    ;;
    (cond ((> exp vm:single-float-normal-exponent-max)
	   ;; Infinity or NaN.  Convert NaN to quiet NaN.
	   (let ((signif (ldb vm:single-float-significand-byte bits)))
	     (if (zerop signif)
		 ;; Infinity
		 x
		 ;; NaN.  Convert to quiet Nan
		 (make-single-float (logior bits vm:single-float-trapping-nan-bit)))))
	  ((<= biased 0)
	   ;; Number is less than 1.  IEEE 754 says it should have the
	   ;; same sign.  Make it so.
	   (* x 0f0))
	  ((>= biased (float-digits x))
	   ;; Number is an integer
	   x)
	  (t
	   ;; Somewhere in between.  Zap the bits that
	   ;; represent the fraction part.
	   (let ((frac-bits (- (float-digits x) biased)))
	     (setf bits (logandc2 bits (- (ash 1 frac-bits) 1)))
	     (kernel:make-single-float bits))))))

;; %UNARY-FTRUNCATE/DOUBLE-FLOAT
;;
;; Like %UNARY-FTRUNCATE/SINGLE-FLOAT, except for double-float.

(defun %unary-ftruncate/double-float (x)
  (declare (double-float x)
	   (optimize (speed 3) (safety 0)))
  (let* ((hi (kernel:double-float-high-bits x))
	 (lo (kernel:double-float-low-bits x))
	 (exp (ldb vm:double-float-exponent-byte hi))
	 (biased (truly-the kernel:double-float-exponent
			    (- exp vm:double-float-bias))))
    (declare (type (signed-byte 32) hi)
	     (type (unsigned-byte 32) lo))
    (cond ((> exp vm:double-float-normal-exponent-max)
	   ;; Infinity or NaN.  Convert NaN to quiet NaN.
	   (if (and (zerop (ldb vm:double-float-significand-byte hi))
		    (zerop lo))
	       ;; Infinity
	       x
	       ;; NaN.  Convert to quiet NaN
	       (make-double-float (logior hi vm:double-float-trapping-nan-bit)
				  lo)))
	  ((<= biased 0)
	   ;; Number is less than 1.  IEEE 754 says it should have the
	   ;; same sign.  Make it so.
	   (* x 0d0))
	  ((>= biased (float-digits x))
	   ;; Number is an integer
	   x)
	  (t
	   ;; Somewhere in between.  Zap the bits that
	   ;; represent the fraction part.
	   (let ((frac-bits (- (float-digits x) biased)))
	     (cond ((< frac-bits 32)
		    (setf lo (logandc2 lo (- (ash 1 frac-bits) 1))))
		   (t
		    (setf lo 0)
		    (setf hi (logandc2 hi (- (ash 1 (- frac-bits 32)) 1)))))
	     (kernel:make-double-float hi lo))))))

#+double-double
(defun %unary-ftruncate/double-double-float (x)
  ;; We should do something like what we do for double-double-float,
  ;; but I'm lazy right now.
  (coerce (truncate x) 'double-double-float))

;;; %UNARY-FTRUNCATE  --  Interface
;;;
;;; This function is called when we are doing an ftruncate without any
;;; funky divisor.  Note that we do *not* return the second value of
;;; truncate, so it must be computed by the caller if needed.
;;;
;;; In the float case, we pick off small arguments so that compiler
;;; can use special-case operations.  We use an exclusive test, since
;;; (due to round-off error), (float most-positive-signed-byte-32) may
;;; be greater than most-positive-signed-byte-32.  We chose this
;;; because %unary-truncate can be optimized when the result is known
;;; to be a (signed-byte 32).
;;;
(defun %unary-ftruncate (number)
  (number-dispatch ((number real))
    ((integer)
     (float number))
    ((ratio)
     (float (truncate (numerator number) (denominator number))))
    ((single-float)
     (%unary-ftruncate/single-float number))
    ((double-float)
     (%unary-ftruncate/double-float number))
    #+double-double
    ((double-double-float)
     (%unary-ftruncate/double-double-float number))))
	     


(defun rational (x)
  "RATIONAL produces a rational number for any real numeric argument.  This is
  more efficient than RATIONALIZE, but it assumes that floating-point is
  completely accurate, giving a result that isn't as pretty."
  (number-dispatch ((x real))
    (((foreach single-float double-float #+long-float long-float
	       #+double-double double-double-float))
     (multiple-value-bind (bits exp)
			  (integer-decode-float x)
       (if (eql bits 0)
	   0
	   (let* ((int (if (minusp x) (- bits) bits))
		  (digits (float-digits x))
		  (ex (+ exp digits)))
	     (if (minusp ex)
		 (integer-/-integer int (ash 1 (+ digits (- ex))))
		 (integer-/-integer (ash int ex) (ash 1 digits)))))))
    ((rational) x)))


#+nil
(defun rationalize (x)
  "Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
  representation exploiting the assumption that floats are only accurate to
  their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
      (= x (float (rationalize x) x))"
  (number-dispatch ((x real))
    (((foreach single-float double-float #+long-float long-float))
     ;; Thanks to Kim Fateman, who stole this function rationalize-float
     ;; from macsyma's rational. Macsyma'a rationalize was written
     ;; by the legendary Gosper (rwg). Gosper is now working for Symbolics.
     ;; Guy Steele said about Gosper, "He has been called the
     ;; only living 17th century mathematician and is also the best
     ;; pdp-10 hacker I know." So, if you can understand or debug this
     ;; code you win big.
     (cond ((minusp x) (- (rationalize (- x))))
	   ((zerop x) 0)
	   (t
	    (let ((eps (etypecase x
			   (single-float single-float-epsilon)
			   (double-float double-float-epsilon)
			   #+long-float
			   (long-float long-float-epsilon)))
		  (y ())
		  (a ()))
	      (do ((xx x (setq y (/ (float 1.0 x) (- xx (float a x)))))
		   (num (setq a (truncate x))
			(+ (* (setq a (truncate y)) num) onum))
		   (den 1 (+ (* a den) oden))
		   (onum 1 num)
		   (oden 0 den))
		  ((or (= xx (float a x))
		       (and (not (zerop den))
			    (not (> (abs (/ (- x (/ (float num x)
						    (float den x)))
					    x))
                                    eps))))
		   (integer-/-integer num den))
		(declare ((dispatch-type x) xx)))))))
    ((rational) x)))

;;; RATIONALIZE  --  Public
;;;
;;; The algorithm here is the method described in CLISP.  Bruno Haible has
;;; graciously given permission to use this algorithm.  He says, "You can use
;;; it, if you present the following explanation of the algorithm."
;;;
;;; Algorithm (recursively presented):
;;;   If x is a rational number, return x.
;;;   If x = 0.0, return 0.
;;;   If x < 0.0, return (- (rationalize (- x))).
;;;   If x > 0.0:
;;;     Call (integer-decode-float x). It returns a m,e,s=1 (mantissa,
;;;     exponent, sign).
;;;     If m = 0 or e >= 0: return x = m*2^e.
;;;     Search a rational number between a = (m-1/2)*2^e and b = (m+1/2)*2^e
;;;     with smallest possible numerator and denominator.
;;;     Note 1: If m is a power of 2, we ought to take a = (m-1/4)*2^e.
;;;       But in this case the result will be x itself anyway, regardless of
;;;       the choice of a. Therefore we can simply ignore this case.
;;;     Note 2: At first, we need to consider the closed interval [a,b].
;;;       but since a and b have the denominator 2^(|e|+1) whereas x itself
;;;       has a denominator <= 2^|e|, we can restrict the seach to the open
;;;       interval (a,b).
;;;     So, for given a and b (0 < a < b) we are searching a rational number
;;;     y with a <= y <= b.
;;;     Recursive algorithm fraction_between(a,b):
;;;       c := (ceiling a)
;;;       if c < b
;;;         then return c       ; because a <= c < b, c integer
;;;         else
;;;           ; a is not integer (otherwise we would have had c = a < b)
;;;           k := c-1          ; k = floor(a), k < a < b <= k+1
;;;           return y = k + 1/fraction_between(1/(b-k), 1/(a-k))
;;;                             ; note 1 <= 1/(b-k) < 1/(a-k)
;;;
;;; You can see that we are actually computing a continued fraction expansion.
;;;
;;; Algorithm (iterative):
;;;   If x is rational, return x.
;;;   Call (integer-decode-float x). It returns a m,e,s (mantissa,
;;;     exponent, sign).
;;;   If m = 0 or e >= 0, return m*2^e*s. (This includes the case x = 0.0.)
;;;   Create rational numbers a := (2*m-1)*2^(e-1) and b := (2*m+1)*2^(e-1)
;;;   (positive and already in lowest terms because the denominator is a
;;;   power of two and the numerator is odd).
;;;   Start a continued fraction expansion
;;;     p[-1] := 0, p[0] := 1, q[-1] := 1, q[0] := 0, i := 0.
;;;   Loop
;;;     c := (ceiling a)
;;;     if c >= b
;;;       then k := c-1, partial_quotient(k), (a,b) := (1/(b-k),1/(a-k)),
;;;            goto Loop
;;;   finally partial_quotient(c).
;;;   Here partial_quotient(c) denotes the iteration
;;;     i := i+1, p[i] := c*p[i-1]+p[i-2], q[i] := c*q[i-1]+q[i-2].
;;;   At the end, return s * (p[i]/q[i]).
;;;   This rational number is already in lowest terms because
;;;   p[i]*q[i-1]-p[i-1]*q[i] = (-1)^i.
;;;
(defun rationalize (x)
  "Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
  representation exploiting the assumption that floats are only accurate to
  their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
      (= x (float (rationalize x) x))"
  (number-dispatch ((x real))
    (((foreach single-float double-float #+long-float long-float))
     ;; This is a fairly straigtforward implementation of the iterative
     ;; algorithm above.
     (multiple-value-bind (frac expo sign)
	 (integer-decode-float x)
       (cond ((or (zerop frac) (>= expo 0))
	      (if (minusp sign)
		  (- (ash frac expo))
		  (ash frac expo)))
	     (t
	      ;; expo < 0 and (2*m-1) and (2*m+1) are coprime to 2^(1-e),
	      ;; so build the fraction up immediately, without having to do
	      ;; a gcd.
	      (let ((a (build-ratio (- (* 2 frac) 1) (ash 1 (- 1 expo))))
		    (b (build-ratio (+ (* 2 frac) 1) (ash 1 (- 1 expo))))
		    (p0 0)
		    (q0 1)
		    (p1 1)
		    (q1 0))
		(do ((c (ceiling a) (ceiling a)))
		    ((< c b)
		     (let ((top (+ (* c p1) p0))
			   (bot (+ (* c q1) q0)))
		       (build-ratio (if (minusp sign)
					(- top)
					top)
				    bot)))
		  (let* ((k (- c 1))
			 (p2 (+ (* k p1) p0))
			 (q2 (+ (* k q1) q0)))
		    (psetf a (/ (- b k))
			   b (/ (- a k)))
		    (setf p0 p1
			  q0 q1
			  p1 p2
			  q1 q2))))))))
    ((rational) x)))

