;;; -*- Mode: Lisp; Package: Kernel -*-
;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and Raymond Toy based
;;; on public domain code from Carnegie Mellon University and has been
;;; placed in the Public domain, and is provided 'as is'.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/rand-mt19937.lisp,v 1.14 2006/07/01 18:12:41 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura. This implementation has been
;;; placed in the public domain with permission from M. Matsumoto.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.

(in-package "LISP")
(export '(random-state random-state-p random *random-state*
	  make-random-state))

(in-package "KERNEL")
(export '(%random-single-float %random-double-float random-chunk init-random-state))

(sys:register-lisp-feature :random-mt19937)


;;;; Random state hackery:

;;; The state is stored in a (simple-array (unsigned-byte 32) (627))
;;; wrapped in a random-state structure:
;;;
;;;  0-1:   Constant matrix A. [0, #x9908b0df]
;;;  2:     Index k.
;;;  3-626: State.

;; GENERATE-SEED
;;
;; Generate a random seed that can be used for seeding the generator.
;; If /dev/urandom is available, it is used to generate random data as
;; the seed.  Otherwise, the current time is used as the seed.
;;
;; The /dev/urandom device exists on Linux, FreeBSD, Solaris 8 and
;; later. (You need to have patch 112438-01 for Solaris 8 to get that
;; device, though). It returns pseudorandom data with entropy that the
;; kernel collects from the environment. Unlike the related
;; /dev/random, this device does not block when the entropy pool has
;; been depleted.
(defun generate-seed (&optional (nwords 1))
  ;; On some systems (as reported by Ole Rohne on cmucl-imp),
  ;; /dev/urandom isn't what we think it is, so if it doesn't work,
  ;; silently generate the seed from the current time.
  (or (ignore-errors
	(let ((words (make-array nwords :element-type '(unsigned-byte 32))))
	  (with-open-file (rand "/dev/urandom"
				:direction :input
				:element-type '(unsigned-byte 32))
	    (read-sequence words rand))
	  (if (= nwords 1)
	      (aref words 0)
	      words)))
      (logand (get-universal-time) #xffffffff)))

;; New initializer proposed by Takuji Nishimura and Makota Matsumoto.
;; (See http://www.math.keio.ac.jp/~matumoto/MT2002/emt19937ar.html)
;;
;; This corrects a deficiency in the original initializer wherein the
;; MSB of the seed was not well represented in the state.
;;
;; The initialization routine is described below.  Let s be the seed,
;; mt[] be the state vector.  Then the algorithm is
;;
;; mt[0] = s & 0xffffffffUL
;;
;; for (k = 1; k < N; k++) {
;;   mt[k] = 1812433253 * (mt[k-1] ^ (mt[k-1] >> 30)) + k
;;   mt[k] &= 0xffffffffUL
;; }
;;
;; The multiplier is from Knuth TAOCP Vol2, 3rd Ed., p. 106.
;;

(defun int-init-random-state (&optional (seed 5489) state)
  (declare (type (integer 1 #xffffffff) seed))
  (let ((state (or state (make-array 627 :element-type '(unsigned-byte 32)))))
    (declare (type (simple-array (unsigned-byte 32) (627)) state))
    (setf (aref state 0) 0)
    (setf (aref state 1) #x9908b0df)
    (setf (aref state 2) mt19937-n)
    (setf (aref state 3) seed)
    (do ((k 1 (1+ k)))
	((>= k 624))
      (declare (type (mod 625) k))
      (let ((prev (aref state (+ 3 (1- k)))))
	(setf (aref state (+ 3 k))
	      (logand (+ (* 1812433253 (logxor prev (ash prev -30)))
			 k)
		      #xffffffff))))
    state))

;; Initialize from an array.
;;
;; Here is the algorithm, in C.  init_genrand is the initalizer above,
;; init_key is the seed vector of length key_length.
;;
;;     init_genrand(19650218UL);
;;     i=1; j=0;
;;     k = (N>key_length ? N : key_length);
;;     for (; k; k--) {
;;         mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
;;           + init_key[j] + j; /* non linear */
;;         mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
;;         i++; j++;
;;         if (i>=N) {
;;           mt[0] = mt[N-1]; i=1;
;;         }
;;         if (j>=key_length) {
;;           j=0;
;;         }
;;     }
;;     for (k=N-1; k; k--) {
;;         mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
;;           - i; /* non linear */
;;         mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
;;         i++;
;;         if (i>=N) { mt[0] = mt[N-1]; i=1; }
;;     }
;;
;;     mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
;;

(defun vec-init-random-state (key &optional state)
  (declare (type (array (unsigned-byte 32) (*)) key))
  (let ((key-len (length key))
	(state (init-random-state 19650218 state))
	(i 1)
	(j 0))
    (loop for k from (max key-len mt19937-n) above 0 do
	  (let ((prev (aref state (+ 3 (1- i)))))
	    (setf (aref state (+ 3 i))
		  (ldb (byte 32 0)
		       (+ (aref key j) j
			  (logxor (aref state (+ 3 i))
				  (ldb (byte 32 0)
				       (* 1664525
					  (logxor prev (ash prev -30))))))))
	    (incf i)
	    (incf j)
	    (when (>= i mt19937-n)
	      (setf (aref state 3)
		    (aref state (+ 3 (- mt19937-n 1))))
	      (setf i 1))
	    (when (>= j key-len)
	      (setf j 0))))

    (loop for k from (1- mt19937-n) above 0 do
	  (let ((prev (aref state (+ 3 (1- i)))))
	    (setf (aref state (+ 3 i))
		  (ldb (byte 32 0)
		       (- (logxor (aref state (+ 3 i))
				  (* 1566083941
				     (logxor prev (ash prev -30))))
			  i)))
	    (incf i)
	    (when (>= i mt19937-n)
	      (setf (aref state 3)
		    (aref state (+ 3 (- mt19937-n 1))))
	      (setf i 1))))
    (setf (aref state 3) #x80000000)
    state))

;; 
(defun init-random-state (&optional (seed 5489) state)
  "Generate an random state vector from the given SEED.  The seed can be
  either an integer or a vector of (unsigned-byte 32)"
  (declare (type (or null integer
		     (array (unsigned-byte 32) (*)))
		 seed))
  (etypecase seed
    (integer
     (int-init-random-state (ldb (byte 32 0) seed) state))
    ((array (unsigned-byte 32) (*))
     (vec-init-random-state seed state))))

(defstruct (random-state
	     (:constructor make-random-object)
	     (:make-load-form-fun :just-dump-it-normally))
  (state (init-random-state) :type (simple-array (unsigned-byte 32) (627))))

(defvar *random-state* (make-random-object))

(defun make-random-state (&optional state)
  "Make a random state object.  If STATE is not supplied, return a copy
  of the default random state.  If STATE is a random state, then return a
  copy of it.  If STATE is T then return a random state generated from
  the universal time or /dev/urandom if available."
  (flet ((copy-random-state (state)
	   (let ((state (random-state-state state))
		 (new-state
		  (make-array 627 :element-type '(unsigned-byte 32))))
	     (dotimes (i 627)
	       (setf (aref new-state i) (aref state i)))
	     (make-random-object :state new-state))))
    (cond ((not state) (copy-random-state *random-state*))
	  ((random-state-p state) (copy-random-state state))
	  ((eq state t)
	   (make-random-object :state (init-random-state (generate-seed))))
	  (t (error "Argument is not a RANDOM-STATE, T or NIL: ~S" state)))))

(defun rand-mt19937-initializer ()
  (init-random-state (generate-seed)
                     (random-state-state *random-state*)))

(pushnew 'rand-mt19937-initializer ext:*after-save-initializations*)


;;;; Random entries:

;;; Size of the chunks returned by random-chunk.
;;;
(defconstant random-chunk-length 32)

;;; random-chunk -- Internal
;;;
;;; This function generaters a 32bit integer between 0 and #xffffffff
;;; inclusive.
;;;
(declaim (inline random-chunk))
;;;
;;; Portable implementation.
(defconstant mt19937-n 624)
(defconstant mt19937-m 397)
(defconstant mt19937-upper-mask #x80000000)
(defconstant mt19937-lower-mask #x7fffffff)
(defconstant mt19937-b #x9D2C5680)
(defconstant mt19937-c #xEFC60000)
;;;
#-x86
(defun random-mt19937-update (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state)
	   (optimize (speed 3) (safety 0)))
  (let ((y 0))
    (declare (type (unsigned-byte 32) y))
    (do ((kk 3 (1+ kk)))
	((>= kk (+ 3 (- mt19937-n mt19937-m))))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) mt19937-upper-mask)
		      (logand (aref state (1+ kk)) mt19937-lower-mask)))
      (setf (aref state kk) (logxor (aref state (+ kk mt19937-m))
				    (ash y -1) (aref state (logand y 1)))))
    (do ((kk (+ (- mt19937-n mt19937-m) 3) (1+ kk)))
	((>= kk (+ (1- mt19937-n) 3)))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) mt19937-upper-mask)
		      (logand (aref state (1+ kk)) mt19937-lower-mask)))
      (setf (aref state kk) (logxor (aref state (+ kk (- mt19937-m mt19937-n)))
				    (ash y -1) (aref state (logand y 1)))))
    (setf y (logior (logand (aref state (+ 3 (1- mt19937-n)))
			    mt19937-upper-mask)
		    (logand (aref state 3) mt19937-lower-mask)))
    (setf (aref state (+ 3 (1- mt19937-n)))
	  (logxor (aref state (+ 3 (1- mt19937-m)))
		  (ash y -1) (aref state (logand y 1)))))
  (values))
;;;
#-x86
(defun random-chunk (state)
  (declare (type random-state state)
	   (optimize (speed 3) (safety 0)))
  (let* ((state (random-state-state state))
	 (k (aref state 2)))
    (declare (type (mod 628) k))
    (when (= k mt19937-n)
      (random-mt19937-update state)
      (setf k 0))
    (setf (aref state 2) (1+ k))
    (let ((y (aref state (+ 3 k))))
      (declare (type (unsigned-byte 32) y))
      (setf y (logxor y (ash y -11)))
      (setf y (logxor y (ash (logand y (ash mt19937-b -7)) 7)))
      (setf y (logxor y (ash (logand y (ash mt19937-c -15)) 15)))
      (setf y (logxor y (ash y -18)))
      y)))

;;; Using inline VOP support, only available on the x86 so far.
#+x86
(defun random-chunk (state)
  (declare (type random-state state))
  (vm::random-mt19937 (random-state-state state)))



;;; %RANDOM-SINGLE-FLOAT, %RANDOM-DOUBLE-FLOAT  --  Interface
;;;
;;;    Handle the single or double float case of RANDOM.  We generate a float
;;; between 0.0 and 1.0 by clobbering the significand of 1.0 with random bits,
;;; then subtracting 1.0.  This hides the fact that we have a hidden bit.
;;;
(declaim (inline %random-single-float %random-double-float))
(declaim (ftype (function ((single-float (0f0)) random-state)
			  (single-float 0f0))
		%random-single-float))
;;;
(defun %random-single-float (arg state)
  (declare (type (single-float (0f0)) arg)
	   (type random-state state))
  (* arg
     (- (make-single-float
	 (dpb (ash (random-chunk state)
		   (- vm:single-float-digits random-chunk-length))
	      vm:single-float-significand-byte
	      (single-float-bits 1.0)))
	1.0)))
;;;
(declaim (ftype (function ((double-float (0d0)) random-state)
			  (double-float 0d0))
		%random-double-float))
;;;
;;; 32bit version
;;;
#+nil
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (* (float (random-chunk state) 1d0) (/ 1d0 (expt 2 32))))
;;;
;;; 53bit version.
;;;
#-x86
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (* arg
     (- (lisp::make-double-float
	 (dpb (ash (random-chunk state)
		   (- vm:double-float-digits random-chunk-length
		      vm:word-bits))
	      vm:double-float-significand-byte
	      (lisp::double-float-high-bits 1d0))
	 (random-chunk state))
	1d0)))

;;; Using a faster inline VOP.
#+x86
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (let ((state-vector (random-state-state state)))
    (* arg
       (- (lisp::make-double-float
	   (dpb (ash (vm::random-mt19937 state-vector)
		     (- vm:double-float-digits random-chunk-length
			vm:word-bits))
		vm:double-float-significand-byte
		(lisp::double-float-high-bits 1d0))
	   (vm::random-mt19937 state-vector))
	  1d0))))

#+long-float
(declaim (inline %random-long-float))
#+long-float
(declaim (ftype (function ((long-float (0l0)) random-state) (long-float 0l0))
		%random-long-float))

;;; Using a faster inline VOP.
#+(and long-float x86)
(defun %random-long-float (arg state)
  (declare (type (long-float (0l0)) arg)
	   (type random-state state))
  (let ((state-vector (random-state-state state)))
    (* arg
       (- (lisp::make-long-float
	   (lisp::long-float-exp-bits 1l0)
	   (logior (vm::random-mt19937 state-vector) vm:long-float-hidden-bit)
	   (vm::random-mt19937 state-vector))
	  1l0))))

#+(and long-float sparc)
(defun %random-long-float (arg state)
  (declare (type (long-float (0l0)) arg)
	   (type random-state state))
  (* arg
     (- (lisp::make-long-float
	 (lisp::long-float-exp-bits 1l0)	; X needs more work
	 (random-chunk state) (random-chunk state) (random-chunk state))
	1l0)))
#+double-double
(defun %random-double-double-float (arg state)
  (declare (type (double-double-float (0w0)) arg)
	   (type random-state state))
  ;; Generate a 31-bit integer, scale it and sum them up
  (let* ((r 0w0)
	 (scale (scale-float 1d0 -31))
	 (mult scale))
    (declare (double-float mult)
	     (type double-double-float r)
	     (optimize (speed 3)))
    (dotimes (k 4)
      (setf r (+ r (* mult (ldb (byte 31 0) (random-chunk state)))))
      (setf mult (* mult scale)))
    (* arg r)))


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
  (declare (inline %random-single-float %random-double-float
		   #+long-float %long-float))
  (cond
    ((and (fixnump arg) (<= arg random-fixnum-max) (> arg 0))
     (rem (random-chunk state) arg))
    ((and (typep arg 'single-float) (> arg 0.0F0))
     (%random-single-float arg state))
    ((and (typep arg 'double-float) (> arg 0.0D0))
     (%random-double-float arg state))
    #+long-float
    ((and (typep arg 'long-float) (> arg 0.0L0))
     (%random-long-float arg state))
    #+double-double
    ((and (typep arg 'double-double-float) (> arg 0.0w0))
     (%random-double-double-float arg state))
    ((and (integerp arg) (> arg 0))
     (%random-integer arg state))
    (t
     (error 'simple-type-error
	    :expected-type '(or (integer 1) (float (0))) :datum arg
	    :format-control "Argument is not a positive integer or a positive float: ~S"
	    :format-arguments (list arg)))))
