;;; -*- Mode: Lisp; Package: Kernel -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/rand.lisp,v 1.11 2002/07/10 16:15:59 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions to random number functions for Spice Lisp
;;;
;;; Originally written by David Adam.  Python tuning, better large integer
;;; randomness and efficient IEEE float support by Rob MacLachlan.
;;;
#-new-random
(in-package "LISP")
#-new-random
(export '(random-state random-state-p random *random-state*
	  make-random-state))

#-new-random
(in-package "KERNEL")
#-new-random
(export '(%random-single-float %random-double-float random-chunk
			       random-fixnum-max))


;;;; Random state hackery:

#-new-random
(progn
(defconstant random-const-a 8373)
(defconstant random-const-c 101010101)
(defconstant random-max 54)

;;; Inclusive upper bound on the size of fixnum kept in the state (and returned
;;; by random-chunk.)  Must be even.
;;;
(defconstant random-upper-bound (- most-positive-fixnum 3))
(defconstant random-chunk-length (integer-length random-upper-bound))
(deftype random-chunk () `(integer 0 ,random-upper-bound))

(defvar rand-seed 0)
(defstruct (random-state
	    (:constructor make-random-object)
	    (:make-load-form-fun :just-dump-it-normally))
  (j 24 :type index)
  (k 0 :type index)
  (seed (make-array (1+ random-max) :initial-contents
		    (do ((list-rands () (cons (rand1) list-rands))
			 (i 0 (1+ i)))
			((> i random-max) list-rands)
		      (declare (fixnum i))))
	:type simple-vector))


;;; Generates a random number from rand-seed.
(defun rand1 ()
  (declare (optimize (inhibit-warnings 3)))
  (setq rand-seed
	(mod (+ (* rand-seed random-const-a) random-const-c)
	     (1+ random-upper-bound))))


(defvar *random-state* (make-random-object))


(defun copy-state (cur-state)
  (let ((state (make-random-object
		:seed (make-array 55)
		:j (random-state-j cur-state)
		:k (random-state-k cur-state))))
    (do ((i 0 (1+ i)))
	((= i 55) state)
      (declare (fixnum i))
      (setf (aref (random-state-seed  state) i)
	    (aref (random-state-seed cur-state) i)))))

(defun make-random-state (&optional state)
  "Make a random state object.  If State is not supplied, return a copy
  of the default random state.  If State is a random state, then return a
  copy of it.  If state is T then return a random state generated from
  the universal time."
  (cond ((not state) (copy-state *random-state*))
	((random-state-p state) (copy-state state))
	((eq state t) (setq rand-seed (get-universal-time))
		      (make-random-object))
	(t (error 'simple-type-error
                  :expected-type '(or null t (satisfies random-state-p))
                  :datum state
                  :format-control "Argument is not a RANDOM-STATE, T or NIL: ~S"
                  :format-arguments (list state)))))


;;;; Random entries:

(declaim (start-block random %random-single-float %random-double-float
		      random-chunk))

;;; random-chunk  --  Internal
;;;
;;; This function generates fixnums between 0 and random-upper-bound, 
;;; inclusive.  For the algorithm to work random-upper-bound must be an 
;;; even positive fixnum.  State is the random state to use.
;;;
(declaim (ftype (function (random-state) random-chunk) random-chunk))
(defun random-chunk (state)
  (let* ((seed (random-state-seed state))
	 (j (random-state-j state))
	 (k (random-state-k state))
	 (a (- (- random-upper-bound
		  (the random-chunk
		       (svref seed
			      (setf (random-state-j state)
				    (if (= j 0) random-max (1- j))))))
	       (the random-chunk
		    (svref seed
			   (setf (random-state-k state)
				 (if (= k 0) random-max (1- k))))))))
    (declare (fixnum a))
    (setf (svref seed k)
	  (the random-chunk (if (minusp a) (- a) (- random-upper-bound a))))))


;;; %RANDOM-SINGLE-FLOAT, %RANDOM-DOUBLE-FLOAT  --  Interface
;;;
;;;    Handle the single or double float case of RANDOM.  We generate a float
;;; between 0.0 and 1.0 by clobbering the significand of 1.0 with random bits,
;;; then subtracting 1.0.  This hides the fact that we have a hidden bit.
;;;
(declaim (inline %random-single-float %random-double-float))
(defun %random-single-float (arg state)
  (declare (type (single-float (0f0)) arg)
	   (type (or random-state null) state))
  (* arg
     (- (make-single-float
	 (dpb (ash (random-chunk (or state *random-state*))
		   (- vm:single-float-digits random-chunk-length))
	      vm:single-float-significand-byte
	      (single-float-bits 1.0)))
	1.0)))
;;;
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type (or random-state null) state))
  (let ((state (or state *random-state*)))
    (* arg
       (- (make-double-float
	   (dpb (ash (random-chunk state)
		     (- vm:double-float-digits random-chunk-length
			vm:word-bits))
		vm:double-float-significand-byte
		(double-float-high-bits 1d0))
	   (logxor (ash (random-chunk state)
			(- vm:word-bits random-chunk-length))
		   (ash (random-chunk state)
			(- random-chunk-length vm:word-bits))))
	  1d0))))


;;;; Random integers:

;;; Amount we overlap chunks by when building a large integer to make up for
;;; the loss of randomness in the low bits.
;;;
(defconstant random-integer-overlap 3)

;;; Extra bits of randomness that we generate before taking the value MOD the
;;; limit, to avoid loss of randomness near the limit.
;;;
(defconstant random-integer-extra-bits 10)

;;; Largest fixnum we can compute from one chunk of bits.
;;;
(defconstant random-fixnum-max
  (1- (ash 1 (- random-chunk-length random-integer-extra-bits))))


;;; %RANDOM-INTEGER  --  Internal
;;;
(defun %random-integer (arg state)
  (declare (type (integer 1) arg) (type random-state state))
  (let ((shift (- random-chunk-length random-integer-overlap)))
    (do ((bits (random-chunk state)
	       (logxor (ash bits shift) (random-chunk state)))
	 (count (+ (integer-length arg)
		   (- random-integer-extra-bits shift))
		(- count shift)))
	((minusp count)
	 (rem bits arg))
      (declare (fixnum count)))))

(defun random (arg &optional (state *random-state*))
  "Generate a uniformly distributed pseudo-random number between zero
  and Arg.  State, if supplied, is the random state to use."
  (declare (inline %random-single-float %random-double-float))
  (cond
   ((and (fixnump arg) (<= arg random-fixnum-max))
    (rem (random-chunk state) arg))
   ((typep arg 'single-float)
    (%random-single-float arg state))
   ((typep arg 'double-float)
    (%random-double-float arg state))
   ((integerp arg)
    (%random-integer arg state))
   (t
    (error 'simple-type-error :expected-type '(real (0)) :datum arg
	   :format-control "Argument is not a positive real number: ~S"
	   :format-arguments (list arg)))))

)	; end progn

;;;
;;; **********************************************************************
;;;
;;; Functions for generating random numbers for CMU Common Lisp.
;;;
;;; Written by Raymond Toy.  This is a replacement of the original
;;; rand.lisp in CMUCL.  According to tests done by Peter VanEynde,
;;; the original generator fails some tests of randomness in
;;; Marsaglia's diehard test suite.  This generator passes those
;;; tests.
;;;
;;; Also, the original generator lacks documentation.  This remedies
;;; that, and includes references for the algorithms used. Hopefully
;;; better algorithms have been chosen that are at least as good and
;;; at least as fast as the original.  In fact, the heart of the
;;; generator is geared towards generating floating point numbers
;;; which should be faster than the original.  This should also give
;;; faster integer numbers since longer floating-point random numbers
;;; are generated.  Tests have shown that this new generator is always
;;; a 10-20% faster and upto 3 times faster for double-floats.
;;;

#+new-random
(in-package "LISP")
#+new-random
(export '(random-state random-state-p random *random-state*
	  make-random-state))

#+new-random
(in-package "KERNEL")
#+new-random
(export '(%random-single-float %random-double-float))


#+new-random
(progn
;;;; Random state hackery used to initialize the random state
(defvar rand-seed 54217137)
(declaim (type (integer 0 900000000) rand-seed))

;;; This is the function RMARIN, in James' paper.  The routine is
;;; slightly extended to handle the longer floats that we are using
;;; here.

(defun init-random-state ()
  (flet ((init-aux (y0 y1 y2 z0)
	   (declare (type (mod 179) y0 y1 y2)
		    (type (mod 169) z0))
	   (let ((r (make-array 32 :element-type 'double-float)))
	     (dotimes (n 32
		       r)
	       (declare (fixnum n))
	       (let ((s 0d0)
		     (tt 0.5d0))
		 (declare (double-float s tt))
		 (dotimes (m #.(float-digits 1d0))
		   (declare (fixnum m))
		   (let ((new-y (mod (* y0 y1 y2) 179))
			 (new-z (mod (+ 1 (* 53 z0)) 169)))
		     (when (> (mod (* new-y new-z) 63) 32)
		       (incf s tt))
		     (setf tt (* tt 0.5d0))
		     (setf y0 y1)
		     (setf y1 y2)
		     (setf y2 new-y)
		     (setf z0 new-z)))
		 (setf (aref r n) s))))))
	   
    (multiple-value-bind (ij kl)
	(floor rand-seed 30082)
      (declare (type (integer 0 29918) ij)
	       (type (integer 0 30081) kl))
      (multiple-value-bind (i j)
	  (floor ij 177)
	(declare (type (mod 179) i j))
	(incf i 2)
	(incf j 2)
	(multiple-value-bind (k L)
	    (floor kl 169)
	  (declare (fixnum k L))
	  (incf k 1)
	  (init-aux i j k l))))))
  
(defstruct (random-state
	     (:constructor make-random-object)
	     (:make-load-form-fun :just-dump-it-normally))
  (index 0
	 :type (mod 32))
  (weyl 0.0d0
	:type double-float)
  (cache (make-array 32
		     :element-type 'double-float
		     :initial-contents (init-random-state))
	 :type (simple-array double-float (32)))
  (borrow 0.0d0
	  :type double-float))

(defvar *random-state* (make-random-object))

(defun copy-state (cur-state)
  (let ((state (make-random-object
		:index (random-state-index cur-state)
		:weyl (random-state-weyl cur-state)
		:borrow (random-state-borrow cur-state)
		:cache (make-array 32 :element-type 'double-float))))
    (dotimes (k 32
	      state)
      (declare (fixnum k))
      (setf (aref (random-state-cache state) k)
	    (aref (random-state-cache cur-state) k)))))

(defun make-random-state (&optional state)
  "Make a random state object.  If State is not supplied, return a copy
  of the default random state.  If State is a random state, then return a
  copy of it.  If state is T then return a random state generated from
  the universal time."
  (cond ((not state)
	 (copy-state *random-state*))
	((random-state-p state)
	 (copy-state state))
	((eq state t)
	 (setq rand-seed (mod (get-universal-time) 900000000))
	 (make-random-object))
	(t (error 'simple-type-error
                  :expected-type '(or null t (satisfies random-state-p))
                  :datum state
                  :format-control "Argument is not a RANDOM-STATE, T or NIL: ~S"
                  :format-arguments (list state)))))


;;;; Random entries

#+nil ; Not yet as block compiling tickles compiler trouble.
(declaim (start-block random %random-single-float %random-double-float
		      new-random-float))

;;; random-float -- Internal
;;;
;;; Generates a double-float random number between 0 and 1.
;;;
;;; The technique used is a subtract-with-borrow generator combined
;;; with a Weyl generator.
;;;
;;; The subtract-with-borrow generator is described in the Matlab News
;;; and Notes, Fall 1995, based on suggestions from George
;;; Marsaglia[1]. The generator works as follows.
;;;
;;; The i'th random number is given by
;;;
;;;     z[i] = z[i + 20] - z[i + 5] - b
;;;
;;; The indices, i, i + 20, and i + 5 are all interpreted mod 32,
;;; i.e., use just the 5 least significant bits.  (Note that we really
;;; only need the last 20 values of z, not the last 32.  However,
;;; masking out 5 bits should be faster than comparing against 20.)
;;;
;;; The quantity b is carried over from the previous iteration and is
;;; either 0 or a small quantity.  If z[i] is positive, b is set to
;;; zero.  If z[i] is negative, we make it positive by adding 1 and we
;;; set b to be 2^-53, or double-float-epsilon for double precision
;;; IEEE floats.
;;;
;;; This generator is claimed to have a period of almost 2^1430
;;; values.  However, it has a defect: all numbers are integer
;;; multiples of 2^-53.  Consequently many of the floating-point
;;; numbers are not represented.
;;;
;;; The article describes one method for overcoming this difficulty,
;;; but does not give the full details.  Therefore, we use the
;;; technique of combining two generators.  The basic idea is to
;;; combine the output from the subtract-with-borrow generator above
;;; with the Weyl generator: x[i] = x[i-1] - y mod 1, where y is any
;;; (fixed) irrational number.  This technique is used in RANMAR [2]
;;; and ACARRYPC [3].
;;;
;;; In RANMAR, y = 7654321 / 2^24.  In ACARRYPC, y = 632436069 / 2^32
;;; is used.  We use y = 7097293079245107 / 2^53, where y was randomly
;;; selected.
;;;
;;; Thus, the random number returned is (z[i] - x[i]) mod 1.
;;;
;;; While we could have used a Lisp version of RANMAR, we choose not
;;; to because it only produces single-float results.  The generator
;;; given in Matlab News and Notes generates double-float results and
;;; only requires about 32 double-floats of state compared to 97
;;; single-floats for RANMAR.  James also gives a subtract-with-borrow
;;; generator, but it is also only for single-floats.
;;;
;;; Marsaglia's ACARRYPC (below), generates 32-bit integers.
;;;
;;; This generator uses Matlab's generator combined with the technique
;;; given by Marsaglia for combining a Weyl generator with the
;;; subtract-with-borrow generator.  This idea also seems to be used
;;; in RANMAR.
;;;
;;; References:
;;;
;;; [1] Cleve Moler, Matlab News and Notes, Fall 1995.
;;;
;;; [2] F. James, "A Review of Pseudorandom Number Generators",
;;; Computer Physics Communications 60 (1990).
;;;
;;; [3] George Marsaglia, B. Narasimhan, and Arif Zaman, "A Random
;;; Number Generator for PC's", Computer Physics Communications 60
;;; (1990) 345-349.

;;; According to Marsaglia, a suitable constant is any integer
;;; relatively prime to 2^53.  We randomly selected 7097293079245107.
;;;
(defconstant weyl-constant #.(scale-float (float 7097293079245107 1d0) -53))

(declaim (ftype (function (random-state)
			  (double-float 0.0d0 (1.0d0)))
		new-random-float))

(declaim (inline new-random-float))

(defun new-random-float (state)
  (declare (type random-state state))
  (let* ((z (random-state-cache state))
	 (index (random-state-index state))
	 (new-z (- (aref z (logand (+ index 20) #x1f))
		   (aref z (logand (+ index 5) #x1f))
		   (random-state-borrow state))))
    (declare (double-float new-z)
	     (type (simple-array double-float (32)) z)
	     (type (mod 32) index))
    ;; Do the subtract-with-borrow generator
    (setf (random-state-borrow state) 
	  (cond ((minusp new-z)
		 (incf new-z 1.0d0)
		 double-float-epsilon)
		(t
		 0d0)))
    (setf (aref z index) new-z)
    (setf (random-state-index state) (logand (1+ index) #x1f))

    ;; Weyl generator
    (let ((w (- (random-state-weyl state) weyl-constant)))
      (declare (double-float w))
      (let ((w1 (if (minusp w) (1+ w) w)))
	(declare (double-float w1))
	(setf (random-state-weyl state) w1)
	
	;; Mix them together
	(decf new-z w1)
	(when (minusp new-z)
	  (incf new-z 1d0))
	new-z))))


;;; %RANDOM-SINGLE-FLOAT, %RANDOM-DOUBLE-FLOAT  --  Interface
;;;
;;;    Handle the single or double float case of RANDOM.  These
;;;    functions are required because the compiler will optimize calls
;;;    to random to one of the following functions if possible.
;;;

(declaim (inline %random-single-float %random-double-float))
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (* arg (new-random-float state)))

(defun %random-single-float (arg state)
  (declare (type (single-float (0f0)) arg)
	   (type random-state state))
  (coerce (%random-double-float (coerce arg 'double-float) state)
	  'single-float))


;;;; Random integers:

;;; Random integers are created by concatenating a bunch of chunks
;;; together. Chunks are generated without consing by truncating a
;;; random float to an integer. With the latest propagate-float-type
;;; code the compiler can inline truncate (signed-byte 32) allowing 31
;;; bits, and (unsigned-byte 32) 32 bits on the x86. When not using the
;;; propagate-float-type feature the best size that can be inlined is
;;; 29 bits.  Use of sizes greater than 29 bits causes consing in
;;; argument passing to generic functions, so 29 bits is a good
;;; default.
;;;
(defconstant random-chunk-size 29)

;;; %RANDOM-INTEGER -- Internal
;;;
;;; Make a random integer.  We extract the most significant bits from
;;; the generator and concatenate enough of them together to make up
;;; the desired integer.
(defun %random-integer (arg state)
  (declare (type (integer 1) arg)
	   (type random-state state))
  (flet ((random-chunk (state)
	   (values (truncate (* (float (expt 2 random-chunk-size) 1d0)
				(new-random-float state))))))
    (let ((shift random-chunk-size))
      (do ((bits (random-chunk state)
		 (logxor (ash bits shift) (random-chunk state)))
	   (count (+ (integer-length arg)
		     (- 0 shift))
		  (- count shift)))
	  ((minusp count)
	   (rem bits arg))
	(declare (fixnum count))))))

(defun random (arg &optional (state *random-state*))
  "Generate a uniformly distributed pseudo-random number between zero
  and Arg.  State, if supplied, is the random state to use."
  (declare (inline %random-single-float %random-double-float))
  (cond
    ((<= arg 0)
     (error 'simple-type-error
	    :expected-type '(or (integer 1) (float (0)))
	    :datum arg
	    :format-control "Argument is not a positive integer or a positive float: ~S"
	    :format-arguments (list arg)))
    ((typep arg 'fixnum)
     (values (truncate (%random-double-float (coerce arg 'double-float)
					     state))))
    ((typep arg 'single-float)
     (%random-single-float arg state))
    ((typep arg 'double-float)
     (%random-double-float arg state))
    ((integerp arg)
     (%random-integer arg state))
    (t
     (error 'simple-type-error
	    :expected-type '(or (integer 1) (float (0)))
	    :datum arg
	    :format-control "Argument is not a positive integer or a positive float: ~S"
	    :format-arguments (list arg)))))

)	; end progn
