;;; -*- Package: dfixnum -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project
;;; and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/dfixnum.lisp,v 1.3 2003/02/12 18:35:29 cracauer Exp $")
;;;
;;; **********************************************************************
;;;
;;;
;;; Description: A skeleton of a package to do consing-free arithmetic on
;;;   integers using two fixnums.  One bit in each fixnum is used for internal
;;;   calculations, so a 32-bit Lisp implementation with two-bit tags will
;;;   have 56 bit range in this package (54 bits unsigned).
;;;
;;; NOTE: this package is extremly raw and only supports what is needed for
;;;   the profiler.  It should be considered an interface specification with
;;;   a partial sketchy implentation.
;;;
;;; Author: Martin Cracauer
;;;
;;; Compatibility: Runs in any valid Common Lisp.

(defpackage "DFIXNUM"
  (:export

   ;; types
   dfixnum dfparttype

   ;; constructing
   make-dfixnum
   dfixnum-make-from-number
   
   ;; arthmetic with our datatypes
   dfixnum-inc-df dfixnum-inc-hf
   dfixnum-set-df dfixnum-dec-df dfixnum-dec-hf


   ;; operations with normal datatypes
   dfixnum-set-from-number dfixnum-inc-integer
   dfixnum-set-single-float dfixnum-inc-single-float
   dfixnum-integer dfixnum-single-float
   dfixnum-single-float dfixnum-single-float-inline

   ;; operations on pairs instead of the dfixnum struct
   dfixnum-set-pair dfixnum-inc-pair dfixnum-pair-integer
   dfixnum-dec-pair dfixnum-copy-pair))

(in-package "DFIXNUM")

(defconstant dfbits #.(- (integer-length most-positive-fixnum) 1))
(defconstant dfmax #.(expt 2 dfbits))
(deftype dfparttype () `(integer 0 ,#.(expt 2 dfbits)))

(defstruct dfixnum
  (h 0 :type dfparttype)
  (l 0 :type dfparttype))

(defun dfixnum-inc-df (v i)
  "increments dfixnum v by dfixnum i"
  (declare (type dfixnum v) (type dfixnum i))
  (let ((low (+ (dfixnum-l v) (dfixnum-l i))))
    (if (> low dfmax)
	(progn
	  (setf (dfixnum-l v) (- low dfmax))
	  (incf (dfixnum-h v)))
      (setf (dfixnum-l v) low)))
  (let ((high (+ (dfixnum-h v) (dfixnum-h i))))
    (when (> high dfmax)
      (error "dfixnum became too big ~a + ~a" v i))
    (setf (dfixnum-h v) high))
  v)

(defun dfixnum-set-df (v i)
  (declare (type dfixnum v) (type dfixnum i))
  (setf (dfixnum-h v) (dfixnum-h i))
  (setf (dfixnum-l v) (dfixnum-l i)))

(defun dfixnum-inc-hf (v i)
  "increments dfixnum v by i (max half fixnum)"
  (declare (type dfixnum v) (type fixnum i))
  (when (> i dfmax)
      (error "not a half-fixnum: ~a" i))
  (let ((low (+ (dfixnum-l v) i)))
    (if (> low dfmax)
	(progn
	  (setf (dfixnum-l v) (- low dfmax))
	  (incf (dfixnum-h v)))
      (setf (dfixnum-l v) (the dfparttype low))))
  (when (> (+ (dfixnum-h v) i) dfmax)
    (error "dfixnum became too big ~a + ~a" v i))
  v)

(defun dfixnum-dec-df (v i)
  "decrement dfixnum v by dfixnum i"
  (declare (type dfixnum v) (type dfixnum i))
  (let ((low (- (dfixnum-l v) (dfixnum-l i)))
	(high (- (dfixnum-h v) (dfixnum-h i))))
    (declare (type fixnum low high))
    (when (< low 0)
	(decf high)
	(setf low (+ low dfmax)))
    (when (< high 0)
      (error "dfixnum became negative ~a - ~a (~a/~a)" v i low high))
    (setf (dfixnum-h v) high)
    (setf (dfixnum-l v) low))
  v)

(defun dfixnum-dec-hf (v i)
  "decrement dfixnum v by half-fixnum i"
  (declare (type dfixnum v) (type (integer 0 #.dfmax) i))
  (let ((low (- (dfixnum-l v) i))
	(high (dfixnum-h v)))
    (declare (type fixnum low high))
    (when (< low 0)
	(decf high)
	(setf low (+ low dfmax)))
    (when (< high 0)
      (error "dfixnum became negative ~a - ~a (~a/~a)" v i low high))
    (setf (dfixnum-h v) high)
    (setf (dfixnum-l v) low))
  v)

(defun dfixnum-inc-integer (df i)
  "increments dfixnum by an interger which may be bigger than fixnum.
   May cons"
  (declare (type dfixnum df) (integer i) (optimize (ext:inhibit-warnings 3)))
  (let ((carry (+ (dfixnum-l df) (mod i dfmax))))
    (setf (dfixnum-l df) (mod carry dfmax))
    (if (> carry dfmax)
	(setf carry 1)
	(setf carry 0))
    (setf (dfixnum-h df)
	  (+ (dfixnum-h df)
	     (ash i (- dfbits))
	     carry))))

(defun dfixnum-set-from-number (df i)
  (declare (type dfixnum df) (optimize (ext:inhibit-warnings 3)))
  (setf (dfixnum-h df) (ash i (- dfbits)))
  (setf (dfixnum-l df) (mod i dfmax)))

(defun dfixnum-make-from-number (i)
  "returns a new dfixnum from number i"
  (declare (type number i) (optimize (ext:inhibit-warnings 3)))
  (let ((df (make-dfixnum)))
    (declare (type dfixnum df))
    (dfixnum-set-from-number df i)
    df))

(defun dfixnum-integer (df)
  (declare (optimize (ext:inhibit-warnings 3)))
  (+ (* (dfixnum-h df) dfmax)
     (dfixnum-l df)))

(defun dfixnum-single-float (df)
  (declare (optimize (ext:inhibit-warnings 3)))
  (+ (* (coerce (dfixnum-h df) 'single-float) #.(coerce dfmax 'single-float))
     (coerce (dfixnum-l df) 'single-float)))

(defun dfixnum-single-float-inline (df)
  (declare (optimize (ext:inhibit-warnings 3)))
  (+ (* (coerce (dfixnum-h df) 'single-float) #.(coerce dfmax 'single-float))
     (coerce (dfixnum-l df) 'single-float)))
(declaim (inline dfixnum-single-float-inline))

(defmacro dfixnum-set-single-float (float df)
  `(progn
     (setf
      ,float
      (+ (* (coerce (dfixnum-h ,df) 'single-float)
	    ,#.(coerce dfmax 'single-float))
	 (coerce (dfixnum-l ,df) 'single-float)))))

(defmacro dfixnum-inc-single-float (float df)
  `(progn
     (setf
      ,float
      (+ ,float (* (coerce (dfixnum-h ,df) 'single-float)
		   ,#.(coerce dfmax 'single-float))
	 (coerce (dfixnum-l ,df) 'single-float)))))

(defmacro dfixnum-set-pair (h l dfnum)
  `(progn
     (setf ,h (dfixnum-h ,dfnum))
     (setf ,l (dfixnum-l ,dfnum))))

(defmacro dfixnum-inc-pair (vh vl ih il)
  "increments a pair of halffixnums by another pair"
  `(progn
     (let ((low (+ ,vl ,il)))
       (if (> low dfmax)
	   (progn
	     (setf ,vl (- low dfmax))
	     (incf ,vh))
	 (setf ,vl low)))
     (let ((high (+ ,vh ,ih)))
       (when (> high dfmax)
	 (error "dfixnum became too big ~a/~a + ~a/~a" ,vh ,vl ,ih ,il))
       (setf ,vh high))))

(defun dfixnum-pair-integer (h l)
  (+ (* h dfmax) l))

(defmacro dfixnum-dec-pair (vh vl ih il)
  "decrement dfixnum pair by another pair"
  `(let ((low (- ,vl ,il))
	 (high (- ,vh ,ih)))
     (declare (type fixnum low high))
     (when (< low 0)
       (decf high)
       (setf low (+ low dfmax)))
     (when (< high 0)
       (error "dfixnum became negative ~a/~a - ~a/~a(~a/~a)"
	      ,vh ,vl ,ih ,il low high))
     (setf ,vh high)
     (setf ,vl low)))

(defmacro dfixnum-copy-pair (vh vl ih il)
  `(progn
     (setf ,vh ,ih)
     (setf ,vl ,il)))
