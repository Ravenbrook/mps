;;; Copyright (C) 2002, 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

;;; This is a hacked version of bench.lisp originally found in PCL,
;;; which contained the following comment:

;;;Here are a few homebrew benchmarks for testing out Lisp performance.
;;; BENCH-THIS-LISP: benchmarks for common lisp.
;;; BENCH-THIS-CLOS: benchmarks for CLOS.
;;; BENCH-FLAVORS:    ditto for Symbolics flavors.
;;; BE SURE TO CHANGE THE PACKAGE DEFINITION TO GET THE CLOS + LISP YOU WANT TO TEST.
;;;
;;;Each benchmark is reported as operations per second.  Without-interrupts is used,
;;;  so the scheduler isn't supposed to get in the way.  Accuracy is generally
;;;  between one and five percent.
;;;
;;;Elapsed time is measured using get-internal-run-time.  Because the accuracy of
;;;  this number is fairly crude, it is important to use a large number of 
;;;  iterations to get an accurate benchmark.  The function median-time may
;;;  complain to you if you didn't pick enough iterations.
;;;
;;;July 1992.  Watch out!  In some cases the instruction being timed will be
;;;  optimized away by a clever compiler.  Beware of benchmarks that are
;;;  nearly as fast as *speed-of-empty-loop*.
;;;
;;;Thanks to Ken Anderson for much of this code.
;;;
;;; jeff morrill
;;; jmorrill@bbn.com

#+cmu
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/clos-bench.lisp,v 1.3 2003/05/22 15:50:06 gerd Exp $")
 
#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(pcl:allocate-instance pcl::allocate-standard-instance)))

#+nil
(declaim (optimize (speed 3) (safety 1) (space 0)
		   (compilation-speed 0)))

(declaim (single-float *min-time*))

(defvar *min-time* (/ 10 (float internal-time-units-per-second))
  "At least 2 orders of magnitude larger than our time resolution.")

#-cmu
(defmacro elapsed-time (form)
  "Returns (1) the result of form and (2) the time (seconds) it takes to evaluate form."
  ;; Note that this function is completely portable.
  (let ((start-time (gensym)) (end-time (gensym)))
    `(let ((,start-time (get-internal-run-time)))
	 (declare (type (unsigned-byte 31) ,start-time))
	 (values ,form
		 (let ((,end-time (get-internal-run-time)))
		   (declare (type (unsigned-byte 31) ,end-time))
		   (/ (abs (- ,end-time ,start-time))
		      ,(float internal-time-units-per-second)))))))

#+cmu
(defmacro elapsed-time (form)
  "Returns (1) the result of form and (2) the time (seconds) it takes to evaluate form."
  ;; Note that this function is completely portable.
  (let ((start-time (gensym)) (end-time (gensym))
	(gc-start-time (gensym))
	(gc-end-time (gensym)))
    `(let ((,start-time (get-internal-run-time))
	   (,gc-start-time lisp::*gc-run-time*))
	 (declare (type (unsigned-byte 31) ,start-time))
	 (values ,form
		 (let ((,end-time (get-internal-run-time))
		       (,gc-end-time lisp::*gc-run-time*))
		   (declare (type (unsigned-byte 31) ,end-time))
		   (/ (- (abs (- ,end-time ,start-time))
			 (max 0 (- ,gc-end-time ,gc-start-time)))
		      ,(float internal-time-units-per-second)))))))

(defmacro without-interruption (&body forms)
  #+genera `(scl:without-interrupts ,@forms)
  #+lucid `(lcl::with-scheduling-inhibited ,@forms)
  #+allegro `(excl:without-interrupts ,@forms)
  #+lispworks `(without-interrupts ,@forms)
  #+(and (not genera) (not lucid) (not allegro)) `(progn ,@forms))

(defmacro median-time (form &optional (I 5))
  "Return the median time it takes to evaluate form."
  ;; I: number of samples to take.
  `(without-interruption
     (let ((results nil))
       (dotimes (ignore ,I)
	 (declare (fixnum ignore))
	 (multiple-value-bind (ignore time) (elapsed-time ,form)
	   (declare (ignore ignore)
		    (single-float time))
	   (if (< time *min-time*)
	       (format t "~% Warning.  Evaluating ~S took only ~S seconds.~
                          ~% You should probably use more iterations." ',form time))
	   (push time results)))
       (nth ,(truncate I 2) (sort results #'<)))))

#+debug
(defun test () (median-time (sleep 1.0)))

;;;*********************************************************************

;;;OPERATIONS-PER-SECOND actually does the work of computing a benchmark.  The amount
;;;  of time it takes to execute the form N times is recorded, minus the time it
;;;  takes to execute the empty loop.  OP/S = N/time.  This quantity is recomputed
;;;  five times and the median value is returned.  Variance in the numbers increases
;;;  when memory is being allocated (cons, make-instance, etc).

(defmacro repeat (form N)
  ;; Minimal loop
  (let ((count (gensym)) (result (gensym)))
    `(let ((,count ,N) ,result)
       (declare (fixnum ,count))
       (loop 
	 ;; If you don't use the setq, the compiler may decide that since the
	 ;; result is ignored, FORM can be "compiled out" of the loop.
	 (setq ,result ,form)
	 (if (zerop (decf ,count)) (return ,result))))))

(defun nempty (N)
  (declare (fixnum N))
  "The empty loop."
  (repeat nil N))

(defun empty-speed (N) (median-time (nempty N)))

(defun compute-empty-iterations (&optional (default 10000000))
  (format t "~%Computing speed of empty loop...")
  (let ((time 0.0))
    (declare (single-float time))
    (loop
      (setq time (empty-speed default))
      (if (< time *min-time*) (setq default (* default 10)) (return)))
    (format t "done.")
    default))

(declaim (fixnum *empty-iterations*))
(defvar *empty-iterations*)
(declaim (single-float *speed-of-empty-loop*))
(defvar *speed-of-empty-loop*)

(eval-when (:load-toplevel :execute)
  (setq *empty-iterations* (compute-empty-iterations))
  (setq *speed-of-empty-loop* (/ (empty-speed *empty-iterations*)
				 (float *empty-iterations*))))

(defmacro operations-per-second (form N &optional (I 5))
  "Return the number of times FORM can evaluate in one second."
  `(let ((time (progn (gc) (median-time (repeat ,form ,N) ,I))))
     (declare (single-float time) (fixnum ,N))
     (/ (float ,N) (- time (* *speed-of-empty-loop* ,N)))))

(defmacro bench (pretty-name name N &optional (stream t))
  `(progn
     (format ,stream "~&~A:~40T" ,pretty-name)
     (force-output ,stream)
     (format ,stream "~10,2,1E~%" (,name ,N))))

;;;****************************************************************************
;;;BENCH-THIS-LISP

(defun Nmult (N)
  (let ((a 2.1))
    (operations-per-second (* a a) N)))

(defun Nadd (N)
  (let ((a 2.1))
    (operations-per-second (+ a a) N))) 

(defun square (x) (* x x))

(defun funcall-1 (N)
  (let ((x 2.1))
    (operations-per-second (funcall #'(lambda (a) (* a a)) x) N)))

(defun f1 (n) n)

(defun funcall-2 (N)
  (let ((f #'f1) 
	(x 2.1))
    (operations-per-second (funcall f x) N)))

(defun funcall-3 (N)
  (let ((x 2.1))
    (operations-per-second (f1 x) N)))

(defun funcall-4 (N)
  (let ((x 2.1))
    (operations-per-second (funcall #'square x) N)))

(defun funcall-5 (N)
  (let ((x 2.1)
	(f #'square))
    (let ((g #'(lambda (x) 
		 (operations-per-second (funcall f x) N))))
      (funcall g x))))

(defun Nsetf (N)
  (let ((array (make-array 15)))
    (operations-per-second (setf (aref array 5) t) N)))

(defun Nsymeval (N) (operations-per-second (eval T) N))

(defun Repeatuations (N) (operations-per-second (eval '(* 2.1 2.1)) N))

(defun n-cons (N) (let ((a 1)) (operations-per-second (cons a a) N)))

(defvar *object* t)
(Defun nspecial (N) (operations-per-second (null *object*) N))

(defun nlexical (N) 
  (let ((o t))
    (operations-per-second (null o) N)))

(defun nfree (N) 
  (let ((o t))
    (let ((g #'(lambda ()
		 #+genera (declare (sys:downward-function))
		 (operations-per-second (null o) N))))
      (funcall g))))

(defun nfree2 (N) 
  (let ((o t))
    (let ((g #'(lambda ()
		 (let ((f #'(lambda ()
			      #+genera (declare (sys:downward-function))
			      (operations-per-second (null o) N))))
		   (funcall f)))))
      (funcall g))))

(defun ncompilations (N)
  (let ((lambda-expression
	  '(lambda (bar) (let ((baz t)) (if baz (cons bar nil))))))
    (operations-per-second (compile 'bob lambda-expression) N)))

(defun bench-this-lisp ()
  (let ((N (/ *empty-iterations* 1)))
    (bench "(* 2.1 2.1)" nmult N)
    (bench "(+ 2.1 2.1)" nadd N)
    (bench "funcall & (* 2.1 2.1)" funcall-3 N)
    (bench "special reference" nspecial *empty-iterations*)
    (bench "lexical reference" nlexical *empty-iterations*)
    ;;  (bench "ivar reference" n-ivar-ref N)
    (bench "(setf (aref array 5) t)" nsetf N)
    (bench "(funcall lexical-f x)" funcall-2 N)
    (bench "(f x)" funcall-3 N) 
    ;;  (Bench "(eval t)" nsymeval 10000)
    ;;  (bench "(eval '(* 2.1 2.1))" repeatuations 10000)
    ;;  (bench "(cons 1 2)" n-cons 100000)
    ;;  (bench "compile simple function" ncompilations 50)
    ))

;(bench-this-lisp)

;;;**************************************************************
;;;BENCH-THIS-CLOS
;;; (evolved from Ken Anderson's tests of Symbolics CLOS)

(defmethod strange ((x t)) t)			; default method
(defmethod area ((x number)) 'green)		; builtin class

(defun strange-defun (x) (declare (ignore x)) t)

(defclass point ()
  ((x :initform 0 :accessor x :initarg :x)
   (y :initform 0 :accessor y :initarg :y)))

(defclass large-class ()
  ((a0 :initform 0 :accessor a0 :initarg :a0)
   (a1 :initform 0 :accessor a1 :initarg :a1)
   (a2 :initform 0 :accessor a2 :initarg :a2)
   (a3 :initform 0 :accessor a3 :initarg :a3)
   (a4 :initform 0 :accessor a4 :initarg :a4)
   (a5 :initform 0 :accessor a5 :initarg :a5)
   (a6 :initform 0 :accessor a6 :initarg :a6)
   (a7 :initform 0 :accessor a7 :initarg :a7)
   (a8 :initform 0 :accessor a8 :initarg :a8)
   (a9 :initform 0 :accessor a9 :initarg :a9)))

#+gerds-pcl
(declaim (ext:slots (inline inline-point inline-large-class)))

(defclass inline-point ()
  ((x :initform 0 :accessor x :initarg :x)
   (y :initform 0 :accessor y :initarg :y)))

(defclass inline-large-class ()
  ((a0 :initform 0 :accessor a0 :initarg :a0)
   (a1 :initform 0 :accessor a1 :initarg :a1)
   (a2 :initform 0 :accessor a2 :initarg :a2)
   (a3 :initform 0 :accessor a3 :initarg :a3)
   (a4 :initform 0 :accessor a4 :initarg :a4)
   (a5 :initform 0 :accessor a5 :initarg :a5)
   (a6 :initform 0 :accessor a6 :initarg :a6)
   (a7 :initform 0 :accessor a7 :initarg :a7)
   (a8 :initform 0 :accessor a8 :initarg :a8)
   (a9 :initform 0 :accessor a9 :initarg :a9)))
  
(defstruct large-struct
  (a0 0)
  (a1 0)
  (a2 0)
  (a3 0)
  (a4 0)
  (a5 0)
  (a6 0)
  (a7 0)
  (a8 0)
  (a9 0))

(defmethod color ((thing point)) 'red)
(defmethod address ((thing point)) 'boston)
(defmethod area ((thing point)) 0)
(defmethod move-to ((p1 point) (p2 point)) 0)

(defmethod large-class/with-slots ((thing large-class))
  (dotimes (i 1000)
    (with-slots (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) thing
      (+ a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))))

(defmethod large-class/slot-value ((thing large-class))
  (dotimes (i 1000)
    (+ (slot-value thing 'a0)
       (slot-value thing 'a1)
       (slot-value thing 'a2)
       (slot-value thing 'a3)
       (slot-value thing 'a4)
       (slot-value thing 'a5)
       (slot-value thing 'a6)
       (slot-value thing 'a7)
       (slot-value thing 'a8)
       (slot-value thing 'a9))))

(defmethod x-offset ((thing point))
  (with-slots (x y) thing x))

(defmethod x-offset/slot-value ((thing point))
  (slot-value thing 'x))

(defmethod x-offset/reader ((thing point))
  (x thing))

(defmethod x-offset/reader/inline ((thing inline-point))
  #+gerds-pcl
  (declare (ext:slots (slot-boundp inline-point)
		      (inline inline-point)))
  (x thing))

(defmethod set-x-offset ((thing point) new-x)
  (with-slots (x y) thing (setq x new-x)))

(defmethod set-x-offset/slot-value ((thing point) new-x)
  (setf (slot-value thing 'x) new-x))

(defmethod set-x-offset/writer ((thing point) new-x)
  (setf (x thing) new-x))

(defmethod set-x-offset/writer/inline ((thing inline-point) new-x)
  #+gerds-pcl
  (declare (ext:slots (inline inline-point)))
  (setf (x thing) new-x))

(defclass box (point)
  ((width :initform 10 :accessor width :initarg :width)
   (height :initform 10 :accessor height :initarg :height)))

(defmethod area ((thing box)) 0)
(defmethod move-to ((box box) (point point)) 0)
(defmethod address :around ((thing box)) (call-next-method))
(defmethod area2 ((x box) (y box)) 0)

(defvar p (make-instance 'point))
(defvar b (make-instance 'box))

(defvar inline-point (make-instance 'inline-point))

(defmethod x-offset-inline ((thing inline-point))
  #+gerds-pcl
  (declare (ext:slots (slot-boundp inline-point)
		      (inline inline-point)))
  (with-slots (x y) thing x))

(defmethod large-class/with-slots/inline ((thing inline-large-class))
  #+gerds-pcl
  (declare (ext:slots (slot-boundp inline-large-class)
		      (inline inline-large-class)))
  (dotimes (i 1000)
    (with-slots (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) thing
      (+ a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))))

(defmethod large-class/with-slots/struct ((thing large-struct))
  (dotimes (i 1000)
    (+ (large-struct-a0 thing)
       (large-struct-a1 thing)
       (large-struct-a2 thing)
       (large-struct-a3 thing)
       (large-struct-a4 thing)
       (large-struct-a5 thing)
       (large-struct-a6 thing)
       (large-struct-a7 thing)
       (large-struct-a8 thing)
       (large-struct-a9 thing))))

(defmethod gf-call ((x box))
  (area x))

(defmethod gf-call-2 ((x box) (y box))
  (area2 x y))

(defun n-gf-call (n)
  (let ((instance b))
    (operations-per-second (gf-call instance) n)))

(defun n-gf-call-2 (n)
  (let ((instance b))
    (operations-per-second (gf-call-2 instance instance) n)))

(defun n-strange (N) (operations-per-second (strange 5) N))
(defun n-strange-defun (N) (operations-per-second (strange-defun 5) N))
(defun n-accesses (N)
  (let ((instance p))
    (operations-per-second (x instance) N)))
(defun n-color (N)
  (let ((instance p))
    (operations-per-second (color instance) n)))
(defun n-call-next-method (N)
  (let ((instance b))
    (operations-per-second (address instance) n)))
(defun n-area-1 (N)
  (let ((instance p))
    (operations-per-second (area instance) n)))
(defun n-area-2 (N)
  (operations-per-second (area 5) n))
(defun n-move-1 (N)
  (let ((instance p))
    (operations-per-second (move-to instance instance) n)))
(defun n-move-2 (N)
  (let ((x p) (y b))
    (operations-per-second (move-to x y) n)))
(defun n-off (N)
  (let ((instance p))
    (operations-per-second (x-offset instance) n)))
(defun n-off-inline (N)
  (let ((instance inline-point))
    (operations-per-second (x-offset-inline instance) n)))
(defun n-off/reader (N)
  (let ((instance p))
    (operations-per-second (x-offset/reader instance) n)))
(defun n-off/reader/inline (N)
  (let ((instance inline-point))
    (operations-per-second (x-offset/reader/inline instance) n)))
(defun n-off/slot-value (N)
  (let ((instance p))
    (operations-per-second (x-offset/slot-value instance) n)))
(defun n-setoff (N)
  (let ((instance p))
    (operations-per-second (set-x-offset instance 500) n)))

(defun n-setoff/writer (N)
  (let ((instance p))
    (operations-per-second (set-x-offset/writer instance 700) n)))

(defun n-setoff/writer/inline (N)
  (let ((instance inline-point))
    (operations-per-second (set-x-offset/writer/inline instance 700) n)))

(defun n-setoff/slot-value (N)
  (let ((instance p))
    (operations-per-second (set-x-offset/slot-value instance 700) n)))

(defun n-large-class/with-slots (n)
  (let ((instance (make-instance 'large-class)))
    (operations-per-second (large-class/with-slots instance) n)))

(defun n-large-class/with-slots/inline (n)
  (let ((instance (make-instance 'inline-large-class)))
    (operations-per-second (large-class/with-slots/inline instance) n)))

(defun n-large-class/with-slots/struct (n)
  (let ((instance (make-large-struct)))
    (operations-per-second (large-class/with-slots/struct instance) n)))

(defun n-large-class/slot-value (n)
  (let ((instance (make-instance 'large-class)))
    (operations-per-second (large-class/slot-value instance) n)))

(defun n-slot-value (N)
  (let ((instance p))
    (operations-per-second (slot-value instance 'x) n)))

(defun n-slot-boundp (N)
  (let ((instance p))
    (operations-per-second (slot-boundp instance 'x) n)))

(defun n-class-of-1 (N)
  (let ((instance p))
    (operations-per-second (class-of instance) n)))
(defun n-class-of-2 (N)
  (operations-per-second (class-of 5) n))

(defun n-alloc (N)
  (let ((c (#+pcl pcl:find-class
		  #+sbcl sb-pcl:find-class
		  #-pcl find-class 'point)))
    (operations-per-second (allocate-instance c) n)))

#+pcl
(defun n-alloc-standard (N)
  (let* ((c (pcl:find-class 'point))
	 (wrapper (pcl::class-wrapper c)))
    (operations-per-second (allocate-standard-instance wrapper)
			   n)))

#+pcl
(defun n-alloc-instance (n)
  (operations-per-second (pcl::%%allocate-instance--class) n))

#+pcl
(defun n-%make-instance (n)
  (operations-per-second (kernel::%make-instance 2) n))

(defun n-alloc-vector (n)
  (operations-per-second (make-array 2) n))

(defun n-make (N)
  (operations-per-second (make-instance 'point) n))

(defun n-make-non-constant (N)
  (let ((class (find-class 'point)))
    (operations-per-second (make-instance class) n)))

(defun n-make-initargs (N)
  (operations-per-second (make-instance 'point :x 0 :y 5) n))

(defun n-make-variable-initargs (N)
  (let ((x 0)
	(y 5))
    (operations-per-second (make-instance 'point :x x :y y) n)))

(defmethod many-specializers ((p1 point) (p2 point) (b1 box) (b2 box)
			      (x integer) (y integer))
  p1)

(defun n-many-specializers (n)
  (let ((p (make-instance 'point))
	(b (make-instance 'box)))
    (operations-per-second (many-specializers p p b b 1 2) n)))

(defmethod two-specializers ((p1 point) (p2 point))
  t)

(defun n-2-specializers (n)
  (let ((instance p))
    (operations-per-second (two-specializers instance instance) n)))

(defmethod three-specializers ((p1 point) (p2 point) (p3 point))
  t)

(defun n-3-specializers (n)
  (let ((instance p))
    (operations-per-second (three-specializers instance instance instance) n)))

(defmethod four-specializers ((p1 point) (p2 point) (p3 point) (p4 point))
  t)

(defun n-4-specializers (n)
  (let ((instance p))
    (operations-per-second (four-specializers instance instance instance
					      instance) n)))

(defmethod five-specializers ((p1 point) (p2 point) (p3 point) (p4 point)
			      (p5 point))
  t)

(defun n-5-specializers (n)
  (let ((instance p))
    (operations-per-second (five-specializers instance instance instance
					      instance
					      instance) n)))

(defmethod 2/4-specializers ((p1 point) (p2 point) a b1 c d)
  (declare (ignore a b1 c d))
  t)

(defmethod 2/3-specializers ((p1 point) (p2 point) a b1 c)
  (declare (ignore a b1 c))
  t)

(defmethod 2/2-specializers ((p1 point) (p2 point) a b1)
  (declare (ignore a b1))
  t)

(defmethod 2/1-specializers ((p1 point) (p2 point) a)
  (declare (ignore a))
  t)

(defun n-2/1-specializers (n)
  (let ((instance p))
    (operations-per-second (2/1-specializers instance instance t)
			   n)))

(defun n-2/2-specializers (n)
  (let ((instance p))
    (operations-per-second (2/2-specializers instance instance t t)
			   n)))

(defun n-2/3-specializers (n)
  (let ((instance p))
    (operations-per-second (2/3-specializers instance instance t t t)
			   n)))

(defun n-2/4-specializers (n)
  (let ((instance p))
    (operations-per-second (2/4-specializers instance instance t t t t)
			   n)))

#+pcl
(pcl::precompile-random-code-segments)

(defun bench-this-clos ()
  (bench-gf-call)
  (bench-slot-accessors)
  #+nil (bench-allocations)
  (bench-make-instance)
  (bench-dispatch))

(defun bench-dispatch (&optional n)
  (when (null n) (setq n 10000000))
  (bench "1 default method" n-strange n)
  (bench "equivalent defun" n-strange-defun n)
  (bench "1 dispatch, 1 method" n-color n)
  (bench "1 dispatch, :around + primary" n-call-next-method n)
  (bench "1 dispatch, 3 methods, instance" n-area-1 n)
  (bench "1 dispatch, 3 methods, noninstance" n-area-2 n)
  (bench "2 dispatch, 2 methods" n-move-1 n)
  (bench "2 specializers" n-2-specializers n)
  (bench "2/1 specializers" n-2/2-specializers n)
  (bench "2/2 specializers" n-2/2-specializers n)
  (bench "2/3 specializers" n-2/3-specializers n)
  (bench "2/4 specializers" n-2/4-specializers n)
  (bench "3 specializers" n-3-specializers n)
  (bench "4 specializers" n-4-specializers n)
  (bench "5 specializers" n-5-specializers n)
  (bench "6 specializers" n-many-specializers n))
	    
(defun bench-make-instance (&optional n)
  (when (null n) (setq n 5000000))
  #+cmu (gc :full t)
  ;; The first test, whatever it is, gets a penalty from something I
  ;; don't understand.
  (bench "make-instance (2 slots)" n-make n)
  (bench "make-instance (2 slots)" n-make n)
  (bench "make-instance (2 slots, non-constant class)" n-make-non-constant n)
  (bench "make-instance (2 constant initargs)" n-make-initargs n)
  (bench "make-instance (2 variable initargs)" n-make-variable-initargs n))

(defun bench-allocations (&optional n)
  (when (null n) (setq n 50000000))
  ;(bench "class-of instance" n-class-of-1 n)
  ;(bench "class-of noninstance" n-class-of-2 n)
  (bench "allocate-instance (2 slots)" n-alloc n)
  (bench "allocate-standard-instance (2 slots)" n-alloc-standard n)
  (bench "%%allocate-instance--class" n-alloc-instance n)
  (bench "%make-instance" n-%make-instance n)
  (bench "make-array" n-alloc-vector n))

;;;
;;; Note that this depends in some way on compile-time knowledge of
;;; class or what; I haven't investigated.  Fact is that compiling
;;; this with known classes gives faster results.
;;;
(defun bench-slot-accessors (&optional n)
  (when (null n) (setq n 100000000))
  (bench "slot reader method" n-accesses n)
  (bench "naked slot-value" n-slot-value n)
  (bench "naked slot-boundp" n-slot-boundp n)
  (bench "large class with-slots" n-large-class/with-slots (/ n 1000))
  (bench "large class with-slots, inline" n-large-class/with-slots/inline
	 (/ n 100))
  (bench "large class with-slots, struct" n-large-class/with-slots/struct
	 (/ n 100))
  (bench "large class slot-value" n-large-class/slot-value (/ n 1000))
  (bench "with-slots, 1 read" n-off n)
  (bench "same with slot-value" n-off/slot-value n)
  (bench "same with reader method" n-off/reader n)
  (bench "with-slots, 1 read, inline" n-off-inline n)
  (bench "same with reader method, inline" n-off/reader/inline n)
  (bench "with-slots, 1 modify" n-setoff n)
  (bench "same with (setf slot-value)" n-setoff/slot-value n)
  (bench "same with writer method" n-setoff/writer n)
  (bench "same with writer method, inline" n-setoff/writer/inline n))

(defun bench-gf-call (&optional n)
  (when (null n) (setq n 100000000))
  (bench "optimizable gf call (1)" n-gf-call n)
  (bench "optimizable gf call (2)" n-gf-call-2 n))

#+pcl
(pcl::precompile-random-code-segments)



