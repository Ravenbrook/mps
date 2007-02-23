;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/srctran.lisp,v 1.162 2006/06/30 18:41:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains macro-like source transformations which convert
;;; uses of certain functions into the canonical form desired within the
;;; compiler.  ### and other IR1 transforms and stuff.  Some code adapted from
;;; CLC, written by Wholey and Fahlman.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Propagate-float-type extension by Raymond Toy.
;;;
(in-package "C")

#+conservative-float-type
(sys:register-lisp-feature :conservative-float-type)

;;; Source transform for Not, Null  --  Internal
;;;
;;;    Convert into an IF so that IF optimizations will eliminate redundant
;;; negations.
;;;
(def-source-transform not (x) `(if ,x nil t))
(def-source-transform null (x) `(if ,x nil t))

;;; Source transform for Endp  --  Internal
;;;
;;;    Endp is just NULL with a List assertion.
;;;
(def-source-transform endp (x)
  `(null (the (values &optional list &rest t) ,x)))

;;; We turn Identity into Prog1 so that it is obvious that it just returns the
;;; first value of its argument.  Ditto for Values with one arg.
(def-source-transform identity (x) `(prog1 ,x))
(def-source-transform values (x) `(prog1 ,x))

;;; CONSTANTLY source transform  --  Internal
;;;
;;;    Bind the values and make a closure that returns them.
;;;
(def-source-transform constantly (value &rest values)
  (let ((temps (loop repeat (1+ (length values))
		     collect (gensym)))
	(dum (gensym)))
    `(let ,(loop for temp in temps and
	         value in (list* value values)
	         collect `(,temp ,value))
       #'(lambda (&rest ,dum)
	   (declare (ignore ,dum))
	   (values ,@temps)))))


;;; COMPLEMENT IR1 transform  --  Internal
;;;
;;;    If the function has a known number of arguments, then return a lambda
;;; with the appropriate fixed number of args.  If the destination is a
;;; FUNCALL, then do the &REST APPLY thing, and let MV optimization figure
;;; things out.
;;;
(deftransform complement ((fun) * * :node node :when :both)
  "open code"
  (multiple-value-bind (min max)
		       (function-type-nargs (continuation-type fun))
    (cond
     ((and min (eql min max))
      (let ((dums (loop repeat min collect (gensym))))
	`#'(lambda ,dums (not (funcall fun ,@dums)))))
     ((let* ((cont (node-cont node))
	     (dest (continuation-dest cont)))
	(and (combination-p dest)
	     (eq (combination-fun dest) cont)))
      '#'(lambda (&rest args)
	   (not (apply fun args))))
     (t
      (give-up "Function doesn't have fixed argument count.")))))


;;;; List hackery:

;;;
;;; Translate CxxR into car/cdr combos.

(defun source-transform-cxr (form)
  (if (or (byte-compiling) (/= (length form) 2))
      (values nil t)
      (let ((name (symbol-name (car form))))
	(do ((i (- (length name) 2) (1- i))
	     (res (cadr form)
		  `(,(ecase (char name i)
		       (#\A 'car)
		       (#\D 'cdr))
		    ,res)))
	    ((zerop i) res)))))

(do ((i 2 (1+ i))
     (b '(1 0) (cons i b)))
    ((= i 5))
  (dotimes (j (ash 1 i))
    (setf (info function source-transform
		(intern (format nil "C~{~:[A~;D~]~}R"
				(mapcar #'(lambda (x) (logbitp x j)) b))))
	  #'source-transform-cxr)))

;;;
;;; Turn First..Fourth and Rest into the obvious synonym, assuming whatever is
;;; right for them is right for us.  Fifth..Tenth turn into Nth, which can be
;;; expanded into a car/cdr later on if policy favors it.
(def-source-transform first (x) `(car ,x))
(def-source-transform rest (x) `(cdr ,x))
(def-source-transform second (x) `(cadr ,x))
(def-source-transform third (x) `(caddr ,x))
(def-source-transform fourth (x) `(cadddr ,x))
(def-source-transform fifth (x) `(nth 4 ,x))
(def-source-transform sixth (x) `(nth 5 ,x))
(def-source-transform seventh (x) `(nth 6 ,x))
(def-source-transform eighth (x) `(nth 7 ,x))
(def-source-transform ninth (x) `(nth 8 ,x))
(def-source-transform tenth (x) `(nth 9 ,x))


;;;
;;; Translate RPLACx to LET and SETF.
(def-source-transform rplaca (x y)
  (once-only ((n-x x))
    `(progn
       (setf (car ,n-x) ,y)
       ,n-x)))
;;;
(def-source-transform rplacd (x y)
  (once-only ((n-x x))
    `(progn
       (setf (cdr ,n-x) ,y)
       ,n-x)))


(def-source-transform nth (n l) `(car (nthcdr ,n ,l)))
  
(defvar *default-nthcdr-open-code-limit* 6)
(defvar *extreme-nthcdr-open-code-limit* 20)

(deftransform nthcdr ((n l) (unsigned-byte t) * :node node)
  "convert NTHCDR to CAxxR"
  (unless (constant-continuation-p n) (give-up))
  (let ((n (continuation-value n)))
    (when (> n
	     (if (policy node (= speed 3) (= space 0))
		 *extreme-nthcdr-open-code-limit*
		 *default-nthcdr-open-code-limit*))
      (give-up))

    (labels ((frob (n)
	       (if (zerop n)
		   'l
		   `(cdr ,(frob (1- n))))))
      (frob n))))


;;;; ARITHMETIC and NUMEROLOGY.

(def-source-transform plusp (x) `(> ,x 0))
(def-source-transform minusp (x) `(< ,x 0))
(def-source-transform zerop (x) `(= ,x 0))

(def-source-transform 1+ (x) `(+ ,x 1))
(def-source-transform 1- (x) `(- ,x 1))

(def-source-transform oddp (x) `(not (zerop (logand ,x 1))))
(def-source-transform evenp (x) `(zerop (logand ,x 1)))

;;; Note that all the integer division functions are available for inline
;;; expansion.

(macrolet ((frob (fun)
	     `(def-source-transform ,fun (x &optional (y nil y-p))
		(declare (ignore y))
		(if y-p
		    (values nil t)
		    `(,',fun ,x 1)))))
  (frob truncate)
  (frob round)
  (frob floor)
  (frob ceiling))

;; Some of these source transforms are not needed when modular
;; arithmetic is available.  When modular arithmetic is available, the
;; various backends need to define them there.
#-modular-arith
(progn
(def-source-transform lognand (x y) `(lognot (logand ,x ,y)))
(def-source-transform lognor (x y) `(lognot (logior ,x ,y)))
(def-source-transform logandc1 (x y) `(logand (lognot ,x) ,y))
(def-source-transform logandc2 (x y) `(logand ,x (lognot ,y)))
(def-source-transform logorc1 (x y) `(logior (lognot ,x) ,y))
(def-source-transform logorc2 (x y) `(logior ,x (lognot ,y)))
)

(def-source-transform logtest (x y) `(not (zerop (logand ,x ,y))))
(def-source-transform byte (size position) `(cons ,size ,position))
(def-source-transform byte-size (spec) `(car ,spec))
(def-source-transform byte-position (spec) `(cdr ,spec))
(def-source-transform ldb-test (bytespec integer)
  `(not (zerop (mask-field ,bytespec ,integer))))


;;; With the ratio and complex accessors, we pick off the "identity" case, and
;;; use a primitive to handle the cell access case.
;;;
(def-source-transform numerator (num)
  (once-only ((n-num `(the (values rational &rest t) ,num)))
    `(if (ratiop ,n-num)
	 (%numerator ,n-num)
	 ,n-num)))
;;;
(def-source-transform denominator (num)
  (once-only ((n-num `(the (values rational &rest t) ,num)))
    `(if (ratiop ,n-num)
	 (%denominator ,n-num)
	 1)))

(deftransform logbitp ((index integer)
		       (integer (or (signed-byte #.vm:word-bits)
				    (unsigned-byte #.vm:word-bits)))
		       (member nil t))
  `(if (>= index #.vm:word-bits)
       (minusp integer)
       (not (zerop (logand integer (ash 1 index))))))

;;;; Interval arithmetic for computing bounds
;;;; (toy@rtp.ericsson.se)
;;;;
;;;; This is a set of routines for operating on intervals.  It implements a
;;;; simple interval arithmetic package.  Although CMUCL has an interval type
;;;; in numeric-type, we choose to use our own for two reasons:
;;;;
;;;;   1.  This package is simpler than numeric-type
;;;;
;;;;   2.  It makes debugging much easier because you can just strip out these
;;;;   routines and test them independently of CMUCL.  (A big win!)
;;;;
;;;; One disadvantage is a probable increase in consing because we have to
;;;; create these new interval structures even though numeric-type has
;;;; everything we want to know.  Reason 2 wins for now.


;;; The basic interval type.  It can handle open and closed intervals.  A
;;; bound is open if it is a list containing a number, just like Lisp says.
;;; NIL means unbounded.
;;;
(defstruct (interval
	     (:constructor %make-interval))
  low high)

(defun make-interval (&key low high)
  (labels ((normalize-bound (val)
	     (cond ((and (floatp val)
			 (float-infinity-p val))
		    ;; Handle infinities
		    nil)
		   ((or (numberp val)
			(eq val nil))
		    ;; Handle any closed bounds
		    val)
		   ((listp val)
		    ;; We have an open bound.  Normalize the numeric bound.
		    ;; If the normalized bound is still a number (not nil),
		    ;; keep the bound open.  Otherwise, the bound is really
		    ;; unbounded, so drop the openness.
		    (let ((new-val (normalize-bound (first val))))
		      (when new-val
			;; Bound exists, so keep it open still
			(list new-val))))
		   (t
		    (error "Unknown bound type in make-interval!")))))
    (%make-interval :low (normalize-bound low)
		    :high (normalize-bound high))))

(declaim (inline bound-value set-bound))

;;; Extract the numeric value of a bound.  Return NIL, if X is NIL.
;;;
(defun bound-value (x)
  (if (consp x) (car x) x))

;;; Given a number X, create a form suitable as a bound for an interval.
;;; Make the bound open if OPEN-P is T.  NIL remains NIL.
;;;
(defun set-bound (x open-p)
  (if (and x open-p) (list x) x))

;;; Apply the function F to a bound X.  If X is an open bound, then the result
;;; will be open.  IF X is NIL, the result is NIL.
;;;
(defun bound-func (f x)
  (and x
       (with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero)
	 ;; With these traps masked, we might get things like infinity or
	 ;; negative infinity returned.  Check for this and return NIL to
	 ;; indicate unbounded.
	 ;;
	 ;; We also ignore any errors that funcall might cause and
	 ;; return NIL instead to indicate infinity.
	 (let ((y (ignore-errors (funcall f (bound-value x)))))
	   (if (and (floatp y)
		    (float-infinity-p y))
	       nil
	       (set-bound y (consp x)))))))

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

;;; NUMERIC-TYPE->INTERVAL
;;;
;;; Convert a numeric-type object to an interval object.
;;;
(defun numeric-type->interval (x)
  (declare (type numeric-type x))
  (make-interval :low (numeric-type-low x)
		 :high (numeric-type-high x)))

(defun copy-interval-limit (limit)
  (if (numberp limit)
      limit
      (copy-list limit)))

(defun copy-interval (x)
  (declare (type interval x))
  (make-interval :low (copy-interval-limit (interval-low x))
		 :high (copy-interval-limit (interval-high x))))

;;; INTERVAL-SPLIT
;;;
;;; Given a point P contained in the interval X, split X into two interval at
;;; the point P.  If CLOSE-LOWER is T, then the left interval contains P.  If
;;; CLOSE-UPPER is T, the right interval contains P. You can specify both to
;;; be T or NIL.
;;;
(defun interval-split (p x &optional close-lower close-upper)
  (declare (type number p)
	   (type interval x))
  ;; Need to be careful if the lower limit is -0.0 and the split point is 0.
  (let ((low (interval-low x)))
    (cond ((and (zerop p)
		(floatp (bound-value low))
		(member (bound-value low) '(-0f0 -0d0)))
	   (list (make-interval :low (copy-interval-limit low)
				:high (float -0d0 (bound-value low)))
		 (make-interval :low (if close-upper (list p) p)
				:high (copy-interval-limit (interval-high x)))))
	  (t
	   (list (make-interval :low (copy-interval-limit (interval-low x))
				:high (if close-lower p (list p)))
		 (make-interval :low (if close-upper (list p) p)
				:high (copy-interval-limit (interval-high x))))))))

;;; INTERVAL-CLOSURE
;;;
;;; Return the closure of the interval.  That is, convert open bounds to
;;; closed bounds.
;;;
(defun interval-closure (x)
  (declare (type interval x))
  (make-interval :low (bound-value (interval-low x))
		 :high (bound-value (interval-high x))))

;;; INTERVAL-RANGE-INFO
;;;
;;; For an interval X, if X >= POINT, return '+.  If X <= POINT, return
;;; '-. Otherwise return NIL.
;;;
(defun interval-range-info (x &optional (point 0))
  (declare (type interval x))
  (labels ((signed->= (x y)
	     ;; If one of the args is a float, we need to do a float
	     ;; comparison to get the correct value when testing for a
	     ;; signed-zero.  That is, we want (>= -0.0 0) to be false.
	     (if (and (zerop x) (zerop y)
		      (or (floatp x) (floatp y)))
		 (>= (float-sign (float x)) (float-sign (float y)))
		 (>= x y))))
    (let ((lo (interval-low x))
	  (hi (interval-high x)))
      ;; FIXME!  We get confused if X is the interval -0d0 to 0d0.
      ;; Special case that.  What else could we be missing?
      (cond ((and (zerop point)
		  (numberp (bound-value lo))
		  (numberp (bound-value hi))
		  (floatp (bound-value lo))
		  (zerop (bound-value lo))
		  (= (bound-value lo) (bound-value hi)))
	     ;; At this point lo = hi = +/- 0.0.
	     (cond ((or (eql (bound-value lo) (bound-value hi))
			(integerp (bound-value hi)))
		    ;; Both bounds are the same kind of signed 0.  Or
		    ;; the high bound is an exact 0.  The sign of the
		    ;; zero tells us the sign of the interval.
		    (if (= (float-sign (bound-value lo)) -1)
			'-
			'+))
		   (t
		    ;; They have different signs
		    nil)))
	    ((and lo (signed->= (bound-value lo) point))
	     '+)
	    ((and hi (signed->= point (bound-value hi)))
	     '-)
	    (t
	     nil)))))

;;; INTERVAL-BOUNDED-P
;;;
;;; Test to see if the interval X is bounded.  HOW determines the test, and
;;; should be either ABOVE, BELOW, or BOTH.
;;;
(defun interval-bounded-p (x how)
  (declare (type interval x))
  (ecase how
    ('above
     (interval-high x))
    ('below
     (interval-low x))
    ('both
     (and (interval-low x) (interval-high x)))))

;;; Return the sign of the number, taking into account the sign of
;;; signed-zeros.  An integer 0 has a positive sign here.
(declaim (inline number-sign))
(defun number-sign (x)
  (declare (real x))
  (if (floatp x)
      (float-sign x)
      (if (minusp x) -1.0 1.0)))

;;; Signed zero comparison functions.  Use these functions if we need
;;; to distinguish between signed zeroes.  Thus -0.0 < 0.0, which not
;;; true with normal Lisp comparison functions.

(defun signed-zero-= (x y)
  (declare (real x y))
  (and (= x y)
       (= (number-sign x)
	  (number-sign y))))


(macrolet ((frob (name op1 op2)
	     `(defun ,name (x y)
		(declare (real x y))
		;; Comparison (op1) is true, or the numbers are EQUAL
		;; so we need to compare the signs of the numbers
		;; appropriately.
		(or (,op1 x y)
		    (and (= x y)
			 ;; Convert the numbers to long-floats so we
			 ;; don't get problems converting to
			 ;; shorter floats from longer. 
			 (,op2 (number-sign x)
			       (number-sign y)))))))
  (frob signed-zero-< < <)
  (frob signed-zero-> > >)
  (frob signed-zero-<= < <=)
  (frob signed-zero->= > >=))

;;; INTERVAL-CONTAINS-P
;;;
;;; See if the interval X contains the number P, taking into account that the
;;; interval might not be closed.
;;;
(defun interval-contains-p (p x)
  (declare (type number p)
	   (type interval x))
  ;; Does the interval X contain the number P?  This would be a lot easier if
  ;; all intervals were closed!
  (let ((lo (interval-low x))
	(hi (interval-high x)))
    (cond ((and lo hi)
	   ;; The interval is bounded
	   (if (and (signed-zero-<= (bound-value lo) p)
		    (signed-zero-<= p (bound-value hi)))
	       ;; P is definitely in the closure of the interval.
	       ;; We just need to check the end points now.
	       (cond ((signed-zero-= p (bound-value lo))
		      (numberp lo))
		     ((signed-zero-= p (bound-value hi))
		      (numberp hi))
		     (t t))
	       nil))
	  (hi
	   ;; Interval with upper bound
	   (if (signed-zero-< p (bound-value hi))
	       t
	       (and (numberp hi) (signed-zero-= p hi))))
	  (lo
	   ;; Interval with lower bound
	   (if (signed-zero-> p (bound-value lo))
	       t
	       (and (numberp lo) (signed-zero-= p lo))))
	  (t
	   ;; Interval with no bounds
	   t))))

;;; INTERVAL-INTERSECT-P
;;;
;;; Determine if two intervals X and Y intersect.  Return T if so.  If
;;; CLOSED-INTERVALS-P is T, the treat the intervals as if they were closed.
;;; Otherwise the intervals are treated as they are.
;;;
;;; Thus if X = [0, 1) and Y = (1, 2), then they do not intersect because no
;;; element in X is in Y.  However, if CLOSED-INTERVALS-P is T, then they do
;;; intersect because we use the closure of X = [0, 1] and Y = [1, 2] to
;;; determine intersection.
;;;
(defun interval-intersect-p (x y &optional closed-intervals-p)
  (declare (type interval x y))
  (multiple-value-bind (intersect diff)
      (interval-intersection/difference (if closed-intervals-p
					    (interval-closure x)
					    x)
					(if closed-intervals-p
					    (interval-closure y)
					    y))
    (declare (ignore diff))
    intersect))

;;; Are the two intervals adjacent?  That is, is there a number between the
;;; two intervals that is not an element of either interval?  If so, they are
;;; not adjacent.  For example [0, 1) and [1, 2] are adjacent but [0, 1) and
;;; (1, 2] are not because 1 lies between both intervals.
;;;
(defun interval-adjacent-p (x y)
  (declare (type interval x y))
  (flet ((adjacent (lo hi)
	   ;; Check to see if lo and hi are adjacent.  If either is
	   ;; nil, they can't be adjacent.
	   (when (and lo hi (= (bound-value lo) (bound-value hi)))
	     ;; The bounds are equal.  They are adjacent if one of them is
	     ;; closed (a number).  If both are open (consp), then there is a
	     ;; number that lies between them.
	     (or (numberp lo) (numberp hi)))))
    (or (adjacent (interval-low y) (interval-high x))
	(adjacent (interval-low x) (interval-high y)))))

;;; INTERVAL-INTERSECTION/DIFFERENCE
;;;
;;; Compute the intersection and difference between two intervals.
;;; Two values are returned: the intersection and the difference.
;;;
;;; Let the two intervals be X and Y, and let I and D be the two values
;;; returned by this function.  Then I = X intersect Y.  If I is NIL (the
;;; empty set), then D is X union Y, represented as the list of X and Y.  If I
;;; is not the empty set, then D is (X union Y) - I, which is a list of two
;;; intervals.
;;;
;;; For example, let X = [1,5] and Y = [-1,3).  Then I = [1,3) and D = [-1,1)
;;; union [3,5], which is returned as a list of two intervals.
;;;
(defun interval-intersection/difference (x y)
  (declare (type interval x y))
  (let ((x-lo (interval-low x))
	(x-hi (interval-high x))
	(y-lo (interval-low y))
	(y-hi (interval-high y)))
    (labels
	((test-lower-bound (p int)
	   ;; Test if the low bound P is in the interval INT.
	   (if p
	       (if (interval-contains-p (bound-value p)
					(interval-closure int))
		   (let ((lo (interval-low int))
			 (hi (interval-high int)))
		     ;; Check for endpoints
		     (cond ((and lo (= (bound-value p) (bound-value lo)))
			    (not (and (numberp p) (consp lo))))
			   ((and hi (= (bound-value p) (bound-value hi)))
			    (and (numberp p) (numberp hi)))
			   (t t))))
	       (not (interval-bounded-p int 'below))))
	 (test-upper-bound (p int)
	   ;; Test if the upper bound P is in the interval INT.
	   (if p
	       (if (interval-contains-p (bound-value p)
					(interval-closure int))
		   (let ((lo (interval-low int))
			 (hi (interval-high int)))
		     ;; Check for endpoints
		     (cond ((and lo (= (bound-value p) (bound-value lo)))
			    (and (numberp p) (numberp lo)))
			   ((and hi (= (bound-value p) (bound-value hi)))
			    (not (and (numberp p) (consp hi))))
			   (t t))))
	       (not (interval-bounded-p int 'above))))
	 (opposite-bound (p)
	   ;; If P is an open bound, make it closed.  If P is a closed bound,
	   ;; make it open.
	   (if (listp p)
	       (first p)
	       (list p))))
      (let ((x-lo-in-y (test-lower-bound x-lo y))
	    (x-hi-in-y (test-upper-bound x-hi y))
	    (y-lo-in-x (test-lower-bound y-lo x))
	    (y-hi-in-x (test-upper-bound y-hi x)))
	(cond ((or x-lo-in-y x-hi-in-y y-lo-in-x y-hi-in-x)
	       ;; Intervals intersect.  Let's compute the intersection and the
	       ;; difference.
	       (multiple-value-bind (lo left-lo left-hi)
		   (cond (x-lo-in-y
			  (values x-lo y-lo (opposite-bound x-lo)))
			 (y-lo-in-x
			  (values y-lo x-lo (opposite-bound y-lo))))
		 (multiple-value-bind (hi right-lo right-hi)
		     (cond (x-hi-in-y
			    (values x-hi (opposite-bound x-hi) y-hi))
			   (y-hi-in-x
			    (values y-hi (opposite-bound y-hi) x-hi)))
		   (values (make-interval :low lo :high hi)
			   (list (make-interval :low left-lo :high left-hi)
				 (make-interval :low right-lo :high right-hi))))))
	      (t
	       (values nil (list x y))))))))

;;; INTERVAL-MERGE-PAIR
;;;
;;; If intervals X and Y intersect, return a new interval that is the union of
;;; the two.  If they do not intersect, return NIL.
;;;
(defun interval-merge-pair (x y)
  (declare (type interval x y))
  ;; If x and y intersect or are adjacent, create the union.
  ;; Otherwise return nil
  (when (or (interval-intersect-p x y)
	    (interval-adjacent-p x y))
    (flet ((select-bound (x1 x2 min-op max-op)
	     (let ((x1-val (bound-value x1))
		   (x2-val (bound-value x2)))
	       (cond ((and x1 x2)
		      ;; Both bounds are finite.  Select the right one.
		      (cond ((funcall min-op x1-val x2-val)
			     ;; x1 definitely better
			     x1)
			    ((funcall max-op x1-val x2-val)
			     ;; x2 definitely better
			     x2)
			    (t
			     ;; Bounds are equal.  Select either value and
			     ;; make it open only if both were open.
			     (set-bound x1-val (and (consp x1) (consp x2))))))
		     (t
		      ;; At least one bound is not finite.  The non-finite
		      ;; bound always wins.
		      nil)))))
      (let* ((x-lo (copy-interval-limit (interval-low x)))
	     (x-hi (copy-interval-limit (interval-high x)))
	     (y-lo (copy-interval-limit (interval-low y)))
	     (y-hi (copy-interval-limit (interval-high y))))
	(make-interval :low (select-bound x-lo y-lo
					  #'signed-zero-< #'signed-zero->)
		       :high (select-bound x-hi y-hi
					   #'signed-zero-> #'signed-zero-<))))))

;; Wrap a handler-case around BODY so that any errors in the body
;; will return a doubly-infinite interval.
;;
;; This is intended to catch things like (* 0f0 n) where n is an
;; integer that won't fit in a single-float.
;;
;; This is a bit heavy-handed since ANY error gets converted to an
;; unbounded interval.  Perhaps some more fine-grained control would
;; be appropriate?

(defmacro with-unbounded-interval-on-error (() &body body)
  `(handler-case
       (progn ,@body)
     (error ()
       (make-interval :low nil :high nil))))
     
;;; Basic arithmetic operations on intervals.  We probably should do true
;;; interval arithmetic here, but it's complicated because we have float and
;;; integer types and bounds can be open or closed.

;;; INTERVAL-NEG
;;;
;;; The negative of an interval
;;;
(defun interval-neg (x)
  (declare (type interval x))
  (make-interval :low (bound-func #'- (interval-high x))
		   :high (bound-func #'- (interval-low x))))
		       
;;; INTERVAL-ADD
;;;
;;; Add two intervals
;;;
(defun interval-add (x y)
  (declare (type interval x y))
  (with-unbounded-interval-on-error ()
    (make-interval :low (bound-binop + (interval-low x) (interval-low y))
		   :high (bound-binop + (interval-high x) (interval-high y)))))

;;; INTERVAL-SUB
;;;
;;; Subtract two intervals
;;;
(defun interval-sub (x y)
  (declare (type interval x y))
  (with-unbounded-interval-on-error ()
    (make-interval :low (bound-binop - (interval-low x) (interval-high y))
		   :high (bound-binop - (interval-high x) (interval-low y)))))

;;; INTERVAL-MUL
;;;
;;; Multiply two intervals
;;;
(defun interval-mul (x y)
  (declare (type interval x y))
  (flet ((bound-mul (x y)
	   (cond ((or (null x) (null y))
		  ;; Multiply by infinity is infinity
		  nil)
		 ((or (and (numberp x) (zerop x))
		      (and (numberp y) (zerop y)))
		  ;; Multiply by closed zero is special.  The result is always
		  ;; a closed bound.  But don't replace this with zero; we
		  ;; want the multiplication to produce the correct signed
		  ;; zero, if needed.
		  (* (bound-value x) (bound-value y)))
		 ((or (and (floatp x) (float-infinity-p x))
		      (and (floatp y) (float-infinity-p y)))
		  ;; Infinity times anything is infinity
		  nil)
		 (t
		  ;; General multiply.  The result is open if either is open.
		  (bound-binop * x y)))))
    (let ((x-range (interval-range-info x))
	  (y-range (interval-range-info y)))
      (cond ((null x-range)
	     ;; Split x into two and multiply each separately
	     (destructuring-bind (x- x+)
		 (interval-split 0 x t t)
	       (interval-merge-pair (interval-mul x- y)
				    (interval-mul x+ y))))
	    ((null y-range)
	     ;; Split y into two and multiply each separately
	     (destructuring-bind (y- y+)
		 (interval-split 0 y t t)
	       (interval-merge-pair (interval-mul x y-)
				    (interval-mul x y+))))
	    ((eq x-range '-)
	     (interval-neg (interval-mul (interval-neg x) y)))
	    ((eq y-range '-)
	     (interval-neg (interval-mul x (interval-neg y))))
	    ((and (eq x-range '+) (eq y-range '+))
	     ;; If we are here, X and Y are both positive
	     (with-unbounded-interval-on-error ()
	       (make-interval :low (bound-mul (interval-low x) (interval-low y))
			      :high (bound-mul (interval-high x)
					       (interval-high y)))))
	    (t
	     (error "This shouldn't happen!"))))))

;;; INTERVAL-DIV
;;;
;;; Divide two intervals.
;;;
(defun interval-div (top bot)
  (declare (type interval top bot))
  (flet ((bound-div (x y y-low-p)
	   ;; Compute x/y
	   (cond ((null y)
		  ;; Divide by infinity means result is 0.  However, we need
		  ;; to watch out for the sign of the result, to correctly
		  ;; handle signed zeros.  We also need to watch out for
		  ;; positive or negative infinity.
		  (if (floatp (bound-value x))
		      (if y-low-p
			  (- (float-sign (bound-value x) 0.0))
			  (float-sign (bound-value x) 0.0))
		      0))
		 ((zerop (bound-value y))
		  ;; Divide by zero means result is infinity
		  nil)
		 ((and (numberp x) (zerop x))
		  ;; Zero divided by anything is zero.
		  x)
		 (t
		  (bound-binop / x y)))))
    (let ((top-range (interval-range-info top))
	  (bot-range (interval-range-info bot)))
      (cond ((null bot-range)
	     ;; The denominator contains zero, so anything goes!
	     (make-interval :low nil :high nil))
	    ((eq bot-range '-)
	     ;; Denominator is negative so flip the sign, compute the result,
	     ;; and flip it back.
	     (interval-neg (interval-div top (interval-neg bot))))
	    ((null top-range)
	     ;; Split top into two positive and negative parts, and divide
	     ;; each separately
	     (destructuring-bind (top- top+)
		 (interval-split 0 top t t)
	       (interval-merge-pair (interval-div top- bot)
				    (interval-div top+ bot))))
	    ((eq top-range '-)
	     ;; Top is negative so flip the sign, divide, and flip the sign of
	     ;; the result.
	     (interval-neg (interval-div (interval-neg top) bot)))
	    ((and (eq top-range '+) (eq bot-range '+))
	     ;; The easy case, sort of.  Both are positive, so we know that
	     ;; the lower bound must be >= +0.  If bound-div returns NIL, we
	     ;; were dividing by zero, so replace that result with 0 or '(0),
	     ;; depending on whether the numerator contains 0.  This isn't
	     ;; quite right, but until we make the interval and numeric-type
	     ;; routines understand the concept of infinity better, this will
	     ;; have to do for now. (RLT)
	     (with-unbounded-interval-on-error ()
	       (make-interval :low (or (bound-div (interval-low top)
						  (interval-high bot) nil)
				       (if (interval-contains-p 0 top)
					   0
					   '(0)))
			      :high (bound-div (interval-high top)
					       (interval-low bot) t))))
	    (t
	     (error "This shouldn't happen!"))))))


;;; INTERVAL-FUNC
;;;
;;; Apply the function F to the interval X.  If X = [a, b], then the result is
;;; [f(a), f(b)].  It is up to the user to make sure the result makes sense.
;;; It will if F is monotonic increasing (or non-decreasing).
;;;
(defun interval-func (f x)
  (declare (type interval x))
  (let ((lo (bound-func f (interval-low x)))
	(hi (bound-func f (interval-high x))))
    (make-interval :low lo :high hi)))

;;; INTERVAL-<
;;;
;;; Return T if X < Y.  That is every number in the interval X is always less
;;; than any number in the interval Y.
;;;
(defun interval-< (x y)
  (declare (type interval x y))
  ;; X < Y only if X is bounded above, Y is bounded below, and they don't
  ;; overlap.
  (when (and (interval-bounded-p x 'above)
	     (interval-bounded-p y 'below))
    ;; Intervals are bounded in the appropriate way.  Make sure they don't
    ;; overlap.
    (let ((left (interval-high x))
	  (right (interval-low y))) 
      (cond ((> (bound-value left)
		(bound-value right))
	     ;; Definitely overlap so result is NIL
	     nil)
	    ((< (bound-value left)
		(bound-value right))
	     ;; Definitely don't touch, so result is T
	     t)
	    (t
	     ;; Limits are equal.  Check for open or closed bounds.
	     ;; Don't overlap if one or the other are open.
	     (or (consp left) (consp right)))))))

;;; INVTERVAL->=
;;;
;;; Return T if X >= Y.  That is, every number in the interval X is always
;;; greater than any number in the interval Y.
;;;
(defun interval->= (x y)
  (declare (type interval x y))
  ;; X >= Y if lower bound of X >= upper bound of Y
  (when (and (interval-bounded-p x 'below)
	     (interval-bounded-p y 'above))
    (>= (bound-value (interval-low x)) (bound-value (interval-high y)))))

;;; INTERVAL-ABS
;;;
;;; Return an interval that is the absolute value of X.  Thus, if X = [-1 10],
;;; the result is [0, 10].
;;;
(defun interval-abs (x)
  (declare (type interval x))
  (case (interval-range-info x)
    ('+
     (copy-interval x))
    ('-
     (interval-neg x))
    (t
     (destructuring-bind (x- x+)
	 (interval-split 0 x t t)
       (interval-merge-pair (interval-neg x-) x+)))))

;;; INTERVAL-SQR
;;;
;;; Compute the square of an interval.
;;;
(defun interval-sqr (x)
  (declare (type interval x))
  (interval-func #'(lambda (x) (* x x))
		 (interval-abs x)))




;;;; Numeric Derive-Type methods:

;;; Derive-Integer-Type  --  Internal
;;;
;;;    Utility for defining derive-type methods of integer operations.  If the
;;; types of both X and Y are integer types, then we compute a new integer type
;;; with bounds determined Fun when applied to X and Y.  Otherwise, we use
;;; Numeric-Contagion.
;;;
(defun derive-integer-type (x y fun)
  (declare (type continuation x y) (type function fun))
  (let ((x (continuation-type x))
	(y (continuation-type y)))
    (if (and (numeric-type-p x) (numeric-type-p y)
	     (eq (numeric-type-class x) 'integer)
	     (eq (numeric-type-class y) 'integer)
	     (eq (numeric-type-complexp x) :real)
	     (eq (numeric-type-complexp y) :real))
	(multiple-value-bind (low high)
			     (funcall fun x y)
	  (make-numeric-type :class 'integer  :complexp :real
			     :low low  :high high))
	(numeric-contagion x y))))

;; Simple utility to flatten a list
(defun flatten-list (x)
  (labels ((flatten-helper (x r);; 'r' is the stuff to the 'right'.
	     (cond ((null x) r)
		   ((atom x)
		    (cons x r))
		   (t (flatten-helper (car x)
				      (flatten-helper (cdr x) r))))))
    (flatten-helper x nil)))

;;; Take some type of continuation and massage it so that we get a list of the
;;; constituent types.  If ARG is *EMPTY-TYPE*, return NIL to indicate
;;; failure.
;;;
(defun prepare-arg-for-derive-type (arg)
  (flet ((listify (arg)
	   (typecase arg
	     (numeric-type
	      (list arg))
	     (union-type
	      (union-type-types arg))
	     (t
	      (list arg)))))
    (unless (eq arg *empty-type*)
      ;; Make sure all args are some type of numeric-type.  For member types,
      ;; convert the list of members into a union of equivalent single-element
      ;; member-type's.
      (let ((new-args nil))
	(dolist (arg (listify arg))
	  (if (member-type-p arg)
	      ;; Run down the list of members and convert to a list of
	      ;; member types.
	      (dolist (member (member-type-members arg))
		(push (if (numberp member)
			  (specifier-type `(eql ,member))
			  *empty-type*)
		      new-args))
	      (push arg new-args)))
	(unless (member *empty-type* new-args)
	  new-args)))))

;;; Convert from the standard type convention for which -0.0 and 0.0 and equal
;;; to an intermediate convention for which they are considered different
;;; which is more natural for some of the optimisers.
;;;
(defun convert-numeric-type (type)
  (declare (type numeric-type type))
  ;; Only convert real float interval delimiters types.
  (if (eq (numeric-type-complexp type) :real)
      (let* ((lo (numeric-type-low type))
	     (lo-val (bound-value lo))
	     (lo-float-zero-p (and lo (floatp lo-val) (= lo-val 0.0)))
	     (hi (numeric-type-high type))
	     (hi-val (bound-value hi))
	     (hi-float-zero-p (and hi (floatp hi-val) (= hi-val 0.0))))
	(if (or lo-float-zero-p hi-float-zero-p)
	    (make-numeric-type
	     :class (numeric-type-class type)
	     :format (numeric-type-format type)
	     :complexp :real
	     :low (if lo-float-zero-p
		      (if (consp lo)
			  (list (float 0.0 lo-val))
			  (float -0.0 lo-val))
		      lo)
	     :high (if hi-float-zero-p
		       (if (consp hi)
			   (list (float -0.0 hi-val))
			   (float 0.0 hi-val))
		       hi))
	    type))
      ;; Not real float.
      type))

;;; Convert back from the intermediate convention for which -0.0 and 0.0 are
;;; considered different to the standard type convention for which and equal.
;;;
(defun convert-back-numeric-type (type)
  (declare (type numeric-type type))
  ;;; Only convert real float interval delimiters types.
  (if (eq (numeric-type-complexp type) :real)
      (let* ((lo (numeric-type-low type))
	     (lo-val (bound-value lo))
	     (lo-float-zero-p
	      (and lo (floatp lo-val) (= lo-val 0.0)
		   (float-sign lo-val)))
	     (hi (numeric-type-high type))
	     (hi-val (bound-value hi))
	     (hi-float-zero-p
	      (and hi (floatp hi-val) (= hi-val 0.0)
		   (float-sign hi-val))))
	(cond
	  ;; (float +0.0 +0.0) => (member 0.0)
	  ;; (float -0.0 -0.0) => (member -0.0)
	  ((and lo-float-zero-p hi-float-zero-p)
	   ;; Shouldn't have exclusive bounds here.
	   (assert (and (not (consp lo)) (not (consp hi))))
	   (if (= lo-float-zero-p hi-float-zero-p)
	       ;; (float +0.0 +0.0) => (member 0.0)
	       ;; (float -0.0 -0.0) => (member -0.0)
	       (specifier-type `(member ,lo-val))
	       ;; (float -0.0 +0.0) => (float 0.0 0.0)
	       ;; (float +0.0 -0.0) => (float 0.0 0.0)
	       (make-numeric-type :class (numeric-type-class type)
				  :format (numeric-type-format type)
				  :complexp :real
				  :low hi-val
				  :high hi-val)))
	  (lo-float-zero-p
	   (cond
	     ;; (float -0.0 x) => (float 0.0 x)
	     ((and (not (consp lo)) (minusp lo-float-zero-p))
	      (make-numeric-type :class (numeric-type-class type)
				 :format (numeric-type-format type)
				 :complexp :real
				 :low (float 0.0 lo-val)
				 :high hi))
	     ;; (float (+0.0) x) => (float (0.0) x)
	     ((and (consp lo) (plusp lo-float-zero-p))
	      (make-numeric-type :class (numeric-type-class type)
				 :format (numeric-type-format type)
				 :complexp :real
				 :low (list (float 0.0 lo-val))
				 :high hi))
	     (t
	      ;; (float +0.0 x) => (or (member 0.0) (float (0.0) x))
	      ;; (float (-0.0) x) => (or (member 0.0) (float (0.0) x))
	      (list (make-member-type :members (list (float 0.0 lo-val)))
		    (make-numeric-type :class (numeric-type-class type)
				       :format (numeric-type-format type)
				       :complexp :real
				       :low (list (float 0.0 lo-val))
				       :high hi)))))
	  (hi-float-zero-p
	   (cond
	     ;; (float x +0.0) => (float x 0.0)
	     ((and (not (consp hi)) (plusp hi-float-zero-p))
	      (make-numeric-type :class (numeric-type-class type)
				 :format (numeric-type-format type)
				 :complexp :real
				 :low lo
				 :high (float 0.0 hi-val)))
	     ;; (float x (-0.0)) => (float x (0.0))
	     ((and (consp hi) (minusp hi-float-zero-p))
	      (make-numeric-type :class (numeric-type-class type)
				 :format (numeric-type-format type)
				 :complexp :real
				 :low lo
				 :high (list (float 0.0 hi-val))))
	     (t
	      ;; (float x (+0.0)) => (or (member -0.0) (float x (0.0)))
	      ;; (float x -0.0) => (or (member -0.0) (float x (0.0)))
	      (list (make-member-type :members (list (float -0.0 hi-val)))
		    (make-numeric-type :class (numeric-type-class type)
				       :format (numeric-type-format type)
				       :complexp :real
				       :low lo
				       :high (list (float 0.0 hi-val)))))))
	  (t
	   type)))
      ;; Not real float.
      type))

;;; Convert back a possible list of numeric types.
;;;
(defun convert-back-numeric-type-list (type-list)
  (typecase type-list
    (list
     (let ((results '()))
       (dolist (type type-list)
	 (if (numeric-type-p type)
	     (let ((result (convert-back-numeric-type type)))
	       (if (listp result)
		   (setf results (append results result))
		   (push result results)))
	     (push type results)))
       results))
    (numeric-type
     (convert-back-numeric-type type-list))
    (union-type
     (convert-back-numeric-type-list (union-type-types type-list)))
    (t
     type-list)))

;;; Make-Canonical-Union-Type
;;;
;;; Take a list of types and return a canonical type specifier, combining any
;;; members types together. If both positive and negative members types are
;;; present they are converted to a float type.
;;;
(defun make-canonical-union-type (type-list)
  (let ((members '())
	(misc-types '()))
    (dolist (type type-list)
      (if (member-type-p type)
	  (setf members (union members (member-type-members type)))
	  (push type misc-types)))
    #+long-float
    (when (null (set-difference '(-0l0 0l0) members))
      (push (specifier-type '(long-float 0l0 0l0)) misc-types)
      (setf members (set-difference members '(-0l0 0l0))))
    #+double-double-2
    (let ((pzero (kernel:make-double-double-float 0d0 0d0))
	  (nzero (kernel:make-double-double-float -0d0 0d0)))
      (when (null (set-difference (list pzero nzero) members))
	(push (specifier-type (list 'kernel:double-double-float pzero pzero)) misc-types)
	(setf members (set-difference members (list nzero pzero)))))
    (when (null (set-difference '(-0d0 0d0) members))
      (push (specifier-type '(double-float 0d0 0d0)) misc-types)
      (setf members (set-difference members '(-0d0 0d0))))
    (when (null (set-difference '(-0f0 0f0) members))
      (push (specifier-type '(single-float 0f0 0f0)) misc-types)
      (setf members (set-difference members '(-0f0 0f0))))
    (if members
	(apply #'type-union (make-member-type :members members) misc-types)
	(apply #'type-union misc-types))))

;;; Convert-Member-Type
;;;
;;; Convert a member type with a single member to a numeric type.
;;;
(defun convert-member-type (arg)
  (let* ((members (member-type-members arg))
	 (member (first members))
	 (member-type (type-of member)))
    (assert (not (rest members)))
    (cond ((subtypep member-type 'integer)
	   (specifier-type `(integer ,member ,member)))
	  ((subtypep member-type 'complex)
	   (specifier-type member-type))
	  (t
	   (specifier-type `(,member-type ,member ,member))))))

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
	   )
  (let ((arg-list (prepare-arg-for-derive-type (continuation-type arg))))
    (when arg-list
      (flet ((deriver (x)
	       (typecase x
		 (member-type
		  (if member-fcn
		      (with-float-traps-masked
			  (:underflow :overflow :divide-by-zero)
			(specifier-type `(eql ,(funcall member-fcn
					    (first (member-type-members x))))))
		      ;; Otherwise convert to a numeric type.
		      (let ((result-type-list
			     (funcall derive-fcn (convert-member-type x))))
			(if convert-type
			    (convert-back-numeric-type-list result-type-list)
			    result-type-list))))
		 (numeric-type
		  (if convert-type
		      (convert-back-numeric-type-list
		       (funcall derive-fcn (convert-numeric-type x)))
		      (funcall derive-fcn x)))
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
  #-conservative-float-type
  (declare (ignore fcn))
  (labels ((maybe-convert-numeric-type (type)
	     (if convert-type (convert-numeric-type type) type))
	   (maybe-convert-back-type-list (type)
	     (if convert-type (convert-back-numeric-type-list type) type))
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
			     (specifier-type `(eql ,result))))))
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
		    *universal-type*)))
	   (non-const-same-leaf-ref-p (x y)
	     ;; Just like same-leaf-ref-p, but we don't care if the
	     ;; value of the leaf is constant or not.
	     (declare (type continuation x y))
	     (let ((x-use (continuation-use x))
		   (y-use (continuation-use y)))
	       (and (ref-p x-use)
		    (ref-p y-use)
		    (eq (ref-leaf x-use) (ref-leaf y-use))))))

    (let ((same-arg (non-const-same-leaf-ref-p arg1 arg2))
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



(defun +-derive-type-aux (x y same-arg)
  (if (and (numeric-type-real-p x)
	   (numeric-type-real-p y))
      (let ((result
	     (if same-arg
		 (let ((x-int (numeric-type->interval x)))
		   (interval-add x-int x-int))
		 (interval-add (numeric-type->interval x)
			       (numeric-type->interval y))))
	    (result-type (numeric-contagion x y)))
	;; If the result type is a float, we need to be sure to coerce
	;; the bounds into the correct type.
	(when (eq (numeric-type-class result-type) 'float)
	  (setf result (interval-func
			#'(lambda (x)
			    (coerce x (or (numeric-type-format result-type)
					  'float)))
			result)))
	(make-numeric-type
	 :class (if (and (eq (numeric-type-class x) 'integer)
			 (eq (numeric-type-class y) 'integer))
		    ;; The sum of integers is always an integer
		    'integer
		    (numeric-type-class result-type))
	 :format (numeric-type-format result-type)
	 :low (interval-low result)
	 :high (interval-high result)))
      ;; General contagion
      (numeric-contagion x y)))


(defoptimizer (+ derive-type) ((x y))
  (two-arg-derive-type x y #'+-derive-type-aux #'+))

(defun --derive-type-aux (x y same-arg)
  (if (and (numeric-type-real-p x)
	   (numeric-type-real-p y))
      (let ((result 
	     ;; (- x x) is always 0.
	     (if same-arg
		 (make-interval :low 0 :high 0)
		 (interval-sub (numeric-type->interval x)
			       (numeric-type->interval y))))
	    (result-type (numeric-contagion x y)))
	;; If the result type is a float, we need to be sure to coerce
	;; the bounds into the correct type.
	(when (eq (numeric-type-class result-type) 'float)
	  (setf result (interval-func
			#'(lambda (x)
			    (coerce x (or (numeric-type-format result-type)
					  'float)))
			result)))
	(make-numeric-type
	 :class (if (and (eq (numeric-type-class x) 'integer)
			 (eq (numeric-type-class y) 'integer))
		    ;; The difference of integers is always an integer
		    'integer
		    (numeric-type-class result-type))
	 :format (numeric-type-format result-type)
	 :low (interval-low result)
	 :high (interval-high result)))
      ;; General contagion
      (numeric-contagion x y)))

(defoptimizer (- derive-type) ((x y))
  (two-arg-derive-type x y #'--derive-type-aux #'-))

(defun *-derive-type-aux (x y same-arg)
  (if (and (numeric-type-real-p x)
	   (numeric-type-real-p y))
      (let ((result
	     ;; (* x x) is always positive, so take care to do it
	     ;; right.
	     (if same-arg
		 (interval-sqr (numeric-type->interval x))
		 (interval-mul (numeric-type->interval x)
			       (numeric-type->interval y))))
	    (result-type (numeric-contagion x y)))
	;; If the result type is a float, we need to be sure to coerce
	;; the bounds into the correct type.
	(when (eq (numeric-type-class result-type) 'float)
	  (setf result (interval-func
			#'(lambda (x)
			    (coerce x (or (numeric-type-format result-type)
					  'float)))
			result)))
	(make-numeric-type
	 :class (if (and (eq (numeric-type-class x) 'integer)
			 (eq (numeric-type-class y) 'integer))
		    ;; The product of integers is always an integer
		    'integer
		    (numeric-type-class result-type))
	 :format (numeric-type-format result-type)
	 :low (interval-low result)
	 :high (interval-high result)))
      (numeric-contagion x y)))

(defoptimizer (* derive-type) ((x y))
  (two-arg-derive-type x y #'*-derive-type-aux #'*))

(defun /-derive-type-aux (x y same-arg)
  (if (and (numeric-type-real-p x)
	   (numeric-type-real-p y))
      (let ((result
	     ;; (/ x x) is always 1, except if x can contain 0.  In
	     ;; that case, we shouldn't optimize the division away
	     ;; because we want 0/0 to signal an error.
	     (if (and same-arg
		      (not (interval-contains-p
			    0 (interval-closure (numeric-type->interval y)))))
		 (make-interval :low 1 :high 1)
		 (interval-div (numeric-type->interval x)
			       (numeric-type->interval y))))
	    (result-type (numeric-contagion x y)))
	;; If the result type is a float, we need to be sure to coerce
	;; the bounds into the correct type.
	(when (eq (numeric-type-class result-type) 'float)
	  (setf result (interval-func
			#'(lambda (x)
			    (coerce x (or (numeric-type-format result-type)
					  'float)))
			result)))
	(make-numeric-type :class (numeric-type-class result-type)
			   :format (numeric-type-format result-type)
			   :low (interval-low result)
			   :high (interval-high result)))
      (numeric-contagion x y)))


(defoptimizer (/ derive-type) ((x y))
  (two-arg-derive-type x y #'/-derive-type-aux #'/))




;;; 'ash derive type optimizer.
;;;
;;; Large resulting bounds are easy to generate but are not particularly
;;; useful, so an open outer bound is returned for a shift greater than 64 -
;;; the largest word size of any of the ports. Large negative shifts are also
;;; problematic as the 'ash implementation only accepts shifts greater than
;;; the most-negative-fixnum. These issues are handled by two local functions:
;;;
;;; ash-outer: performs the shift when within an acceptable range, otherwise
;;; returns an open bound.
;;;
;;; ash-inner: performs the shift when within range, limited to a maximum of
;;; 64, otherwise returns the inner limit.
;;;

(defun ash-derive-type-aux (n-type shift same-arg)
  (declare (ignore same-arg))
  (flet ((ash-outer (n s)
	   (when (and (fixnump s)
		      (<= s 64)
		      (> s most-negative-fixnum))
	     (ash n s)))
	 (ash-inner (n s)
	   (if (and (fixnump s)
		    (> s most-negative-fixnum))
	       (ash n (min s 64))
	       (if (minusp n) -1 0))))
    (or (and (csubtypep n-type (specifier-type 'integer))
	     (csubtypep shift (specifier-type 'integer))
	     (let ((n-low (numeric-type-low n-type))
		   (n-high (numeric-type-high n-type))
		   (s-low (numeric-type-low shift))
		   (s-high (numeric-type-high shift)))
	       (make-numeric-type :class 'integer  :complexp :real
				  :low (when n-low
					 (if (minusp n-low)
					     (ash-outer n-low s-high)
					     (ash-inner n-low s-low)))
				  :high (when n-high
					  (if (minusp n-high)
					      (ash-inner n-high s-low)
					      (ash-outer n-high s-high))))))
	*universal-type*)))
;;;
(defoptimizer (ash derive-type) ((n shift))
  (two-arg-derive-type n shift #'ash-derive-type-aux #'ash))


(defoptimizer (lognot derive-type) ((int))
  (derive-integer-type int int
		       #'(lambda (type type2)
			   (declare (ignore type2))
			   (let ((lo (numeric-type-low type))
				 (hi (numeric-type-high type)))
			     (values (if hi (lognot hi) nil)
				     (if lo (lognot lo) nil)
				     (numeric-type-class type)
				     (numeric-type-format type))))))

(defoptimizer (%negate derive-type) ((num))
  (flet ((negate-bound (b)
	   (set-bound (- (bound-value b)) (consp b))))
    (one-arg-derive-type num
			 #'(lambda (type)
			     (let ((lo (numeric-type-low type))
				   (hi (numeric-type-high type))
				   (result (copy-numeric-type type)))
			       (setf (numeric-type-low result)
				      (if hi (negate-bound hi) nil))
			       (setf (numeric-type-high result)
				     (if lo (negate-bound lo) nil))
			       result))
			 #'-)))

(defun abs-derive-type-aux (type)
  (cond ((eq (numeric-type-complexp type) :complex)
	 ;; The absolute value of a complex number is always a
	 ;; non-negative float.
	 (let* ((format (case (numeric-type-class type)
			  ((integer rational) 'single-float)
			  (t (numeric-type-format type))))
		(bound-format (or format 'float)))
	   (make-numeric-type :class 'float
			      :format format
			      :complexp :real
			      :low (coerce 0 bound-format)
			      :high nil)))
	(t
	 ;; The absolute value of a real number is a non-negative real
	 ;; of the same type.
	 (let* ((abs-bnd (interval-abs (numeric-type->interval type)))
		(class (numeric-type-class type))
		(format (numeric-type-format type))
		(bound-type (or format class 'real)))
	   (make-numeric-type
	    :class class
	    :format format
	    :complexp :real
	    :low (coerce-numeric-bound (interval-low abs-bnd) bound-type)
	    :high (coerce-numeric-bound
		   (interval-high abs-bnd) bound-type))))))

(defoptimizer (abs derive-type) ((num))
  (one-arg-derive-type num #'abs-derive-type-aux #'abs))

(defun rem-result-type (number-type divisor-type)
  ;; Figure out what the remainder type is.  The remainder is an
  ;; integer if both args are integers; a rational if both args are
  ;; rational; and a float otherwise.
  (cond ((and (csubtypep number-type (specifier-type 'integer))
	      (csubtypep divisor-type (specifier-type 'integer)))
	 'integer)
	((and (csubtypep number-type (specifier-type 'rational))
	      (csubtypep divisor-type (specifier-type 'rational)))
	 'rational)
	((and (csubtypep number-type (specifier-type 'float))
	      (csubtypep divisor-type (specifier-type 'float)))
	 ;; Both are floats so the result is also a float, of
	 ;; the largest type.
	 (or (float-format-max (numeric-type-format number-type)
			       (numeric-type-format divisor-type))
	     'float))
	((and (csubtypep number-type (specifier-type 'float))
	      (csubtypep divisor-type (specifier-type 'rational)))
	 ;; One of the arguments is a float and the other is a
	 ;; rational.  The remainder is a float of the same
	 ;; type.
	 (or (numeric-type-format number-type) 'float))
	((and (csubtypep divisor-type (specifier-type 'float))
	      (csubtypep number-type (specifier-type 'rational)))
	 ;; One of the arguments is a float and the other is a
	 ;; rational.  The remainder is a float of the same
	 ;; type.
	 (or (numeric-type-format divisor-type) 'float))
	(t
	 ;; Some unhandled combination.  This usually means both args
	 ;; are REAL so the result is a REAL.
	 'real)))


(defun truncate-derive-type-quot (number-type divisor-type)
  (let* ((rem-type (rem-result-type number-type divisor-type))
	 (number-interval (numeric-type->interval number-type))
	 (divisor-interval (numeric-type->interval divisor-type)))
    ;;(declare (type (member '(integer rational float)) rem-type))
    ;; We have real numbers now.
    (cond ((eq rem-type 'integer)
	   ;; Since the remainder type is INTEGER, both args are
	   ;; INTEGERs.
	   (let* ((res (integer-truncate-derive-type
			(interval-low number-interval)
			(interval-high number-interval)
			(interval-low divisor-interval)
			(interval-high divisor-interval))))
	     (specifier-type (if (listp res) res 'integer))))
	  (t
	   (let ((quot (truncate-quotient-bound
			(interval-div number-interval
				      divisor-interval))))
	     (specifier-type `(integer ,(or (interval-low quot) '*)
			               ,(or (interval-high quot) '*))))))))

(defun truncate-derive-type-rem (number-type divisor-type)
  (let* ((rem-type (rem-result-type number-type divisor-type))
	 (number-interval (numeric-type->interval number-type))
	 (divisor-interval (numeric-type->interval divisor-type))
	 (rem (truncate-rem-bound number-interval divisor-interval)))
    ;;(declare (type (member '(integer rational float)) rem-type))
    ;; We have real numbers now.
    (cond ((eq rem-type 'integer)
	   ;; Since the remainder type is INTEGER, both args are
	   ;; INTEGERs.
	   (specifier-type `(,rem-type ,(or (interval-low rem) '*)
			               ,(or (interval-high rem) '*))))
	  (t
	   (multiple-value-bind (class format)
	       (ecase rem-type
		 (integer
		  (values 'integer nil))
		 (rational
		  (values 'rational nil))
		 ((or single-float double-float #+long-float long-float
		      #+double-double double-double-float)
		  (values 'float rem-type))
		 (float
		  (values 'float nil))
		 (real
		  (values nil nil)))
	     (when (member rem-type '(float single-float double-float
				      #+long-float long-float
				      #+double-double double-double-float))
	       (setf rem (interval-func #'(lambda (x)
					    (coerce x rem-type))
					rem)))
	     (make-numeric-type :class class
				:format format
				:low (interval-low rem)
				:high (interval-high rem)))))))

(defun truncate-derive-type-quot-aux (num div same-arg)
  (declare (ignore same-arg))
  (if (and (numeric-type-real-p num)
	   (numeric-type-real-p div))
      (truncate-derive-type-quot num div)
      *empty-type*))

(defun truncate-derive-type-rem-aux (num div same-arg)
  (declare (ignore same-arg))
  (if (and (numeric-type-real-p num)
	   (numeric-type-real-p div))
      (truncate-derive-type-rem num div)
      *empty-type*))

(defoptimizer (truncate derive-type) ((number divisor))
  (let ((quot (two-arg-derive-type number divisor
				   #'truncate-derive-type-quot-aux #'truncate))
	(rem (two-arg-derive-type number divisor
				  #'truncate-derive-type-rem-aux #'rem)))
    (when (and quot rem)
      (make-values-type :required (list quot rem)))))


(defun ftruncate-derive-type-quot (number-type divisor-type)
  ;; The bounds are the same as for truncate.  However, the first
  ;; result is a float of some type.  We need to determine what that
  ;; type is.  Basically it's the more contagious of the two types.
  ;;
  ;; FIXME: Now that ftruncate returns signed zeroes, we should do
  ;; something better than calling truncate-derive-type-quot to figure
  ;; out the result and massaging that to get the correct float
  ;; result.
  (let* ((q-type (truncate-derive-type-quot number-type divisor-type))
	 (res-type (numeric-contagion number-type divisor-type))
	 (res-format (numeric-type-format res-type)))
    (flet ((floatify-bound (x)
	     ;; Don't have to deal with list-type bounds because the
	     ;; truncate defoptimizer doesn't return list-type bounds.
	     ;;
	     ;; Also, if RES-FORMAT is NIL, that means we want a FLOAT
	     ;; type.
	     (if (numberp x)
		 (coerce x (or res-format 'float))
		 x)))
      (let ((q-lo (floatify-bound (numeric-type-low q-type)))
	    (q-hi (floatify-bound (numeric-type-high q-type))))
	;; We need to be careful if q-type contains zero.  Why?
	;; Because ftruncate returns signed zeroes.  See GCL
	;; ansi-tests misc.558 for an example:
	;;
	;; (defun misc-558 (p1)
	;;   (declare (optimize (speed 1) (safety 2) (debug 2)
	;;                      (space 3))
	;; 	   (type (eql -39466.56) p1))
	;;   (ffloor p1 305598613))
	;; 
	(when (csubtypep (specifier-type '(integer 0 0)) q-type)
	  ;; The quotient contains 0.  We really only have a problem
	  ;; if one of the end points is zero.  
	  (cond ((and q-lo (zerop q-lo))
		 ;; We should only include -0.0 if the result really
		 ;; could be -0.0, but we're lazy right now and just
		 ;; force it.  The interval is very slightly larger
		 ;; than the true interval.
		 (setq q-lo (float -0d0 q-lo)))
		((and q-hi (zerop q-hi))
		 ;; This probably isn't needed because 0 is converted
		 ;; to +0.0, which is the upper bound we want.
		 (setq q-hi (float 0d0 q-hi)))))
	(make-numeric-type :class 'float
			   :format res-format
			   :low q-lo
			   :high q-hi)))))

(defun ftruncate-derive-type-quot-aux (n d same-arg)
  (declare (ignore same-arg))
  (if (and (numeric-type-real-p n)
	   (numeric-type-real-p d))
      (ftruncate-derive-type-quot n d)
      *empty-type*))

;; I (RLT) don't understand why I need to make divisor optional.  This
;; should look just like the optimizer for TRUNCATE.  Until I figure
;; out why, this seems to work.
(defoptimizer (ftruncate derive-type) ((number &optional divisor))
  (if divisor
      (let ((quot
	     (two-arg-derive-type number divisor
				  #'ftruncate-derive-type-quot-aux #'ftruncate))
	    (rem (two-arg-derive-type number divisor
				      #'truncate-derive-type-rem-aux #'rem)))
	(when (and quot rem)
	  (make-values-type :required (list quot rem))))
      (let* ((div (specifier-type '(integer 1 1)))
	     (quot
	      (one-arg-derive-type number
				   #'(lambda (n)
				       (ftruncate-derive-type-quot-aux n div nil))
				   #'ftruncate))
	     (rem (one-arg-derive-type number
				       #'(lambda (n)
					   (truncate-derive-type-rem-aux n div nil))
				       #'(lambda (x)
					   (rem x 1)))))
	(when (and quot rem)
	  (make-values-type :required (list quot rem))))))


(defun %unary-truncate-derive-type-aux (number)
  (truncate-derive-type-quot number (specifier-type '(integer 1 1))))

(defoptimizer (%unary-truncate derive-type) ((number))
  (one-arg-derive-type number
		       #'%unary-truncate-derive-type-aux
		       #'%unary-truncate))

(defoptimizer (%unary-ftruncate derive-type) ((number))
  (let ((divisor (specifier-type '(integer 1 1))))
    (one-arg-derive-type number
			 #'(lambda (n)
			     (ftruncate-derive-type-quot-aux n divisor nil))
			 #'%unary-ftruncate)))

;;; Define optimizers for floor and ceiling
(macrolet
    ((frob-opt (name q-name r-name)
       (let ((q-aux (symbolicate q-name "-AUX"))
	     (r-aux (symbolicate r-name "-AUX")))
	 `(progn
	   ;; Compute type of quotient (first) result
	   (defun ,q-aux (number-type divisor-type)
	     (let* ((number-interval
		     (numeric-type->interval number-type))
		    (divisor-interval
		     (numeric-type->interval divisor-type))
		    (quot (,q-name (interval-div number-interval
						 divisor-interval))))
	       (specifier-type `(integer ,(or (interval-low quot) '*)
				         ,(or (interval-high quot) '*)))))

	   ;; Compute type of remainder
	   (defun ,r-aux (number-type divisor-type)
	     (let* ((divisor-interval
		     (numeric-type->interval divisor-type))
		    (rem (,r-name divisor-interval))
		    (result-type (rem-result-type number-type divisor-type)))
	       (when (member result-type '(float single-float double-float
					   #+long-float long-float
					   #+double-double double-double-float))
		 ;; Make sure the limits on the interval have the right type.
		 (setf rem (interval-func #'(lambda (x)
					      (coerce x result-type))
					  rem)))
	       (specifier-type `(,result-type ,(or (interval-low rem) '*)
					      ,(or (interval-high rem) '*)))))

	   ;; The optimizer itself
	   (defoptimizer (,name derive-type) ((number divisor))
	     (flet ((derive-q (n d same-arg)
		      (declare (ignore same-arg))
		      (if (and (numeric-type-real-p n)
			       (numeric-type-real-p d))
			  (,q-aux n d)
			  *empty-type*))
		    (derive-r (n d same-arg)
		      (declare (ignore same-arg))
		      (if (and (numeric-type-real-p n)
			       (numeric-type-real-p d))
			  (,r-aux n d)
			  *empty-type*)))
	       (let ((quot (two-arg-derive-type
			    number divisor #'derive-q #',name))
		     (rem (two-arg-derive-type
			   number divisor #'derive-r #'mod)))
		 (when (and quot rem)
		   (make-values-type :required (list quot rem))))))
	   ))))

  (frob-opt floor floor-quotient-bound floor-rem-bound)
  (frob-opt ceiling ceiling-quotient-bound ceiling-rem-bound))

;;; Define optimizers for ffloor and fceiling
(macrolet
    ((frob-opt (name q-name r-name)
       (let ((q-aux (symbolicate "F" q-name "-AUX"))
	     (r-aux (symbolicate r-name "-AUX")))
	 `(progn
	   ;; Compute type of quotient (first) result
	   (defun ,q-aux (number-type divisor-type)
	     (let* ((number-interval
		     (numeric-type->interval number-type))
		    (divisor-interval
		     (numeric-type->interval divisor-type))
		    (quot (,q-name (interval-div number-interval
						 divisor-interval)))
		    (res-type (numeric-contagion number-type divisor-type))
		    (res-format (numeric-type-format res-type)))
	       (flet ((floatify (x)
			(if (numberp x)
			    (coerce x (or res-format 'float))
			    x)))
		 (make-numeric-type
		  :class 'float
		  :format res-format
		  :low (floatify (interval-low quot))
		  :high (floatify (interval-high quot))))))
	   
	   (defoptimizer (,name derive-type) ((number divisor))
	     (flet ((derive-q (n d same-arg)
		      (declare (ignore same-arg))
		      (if (and (numeric-type-real-p n)
			       (numeric-type-real-p d))
			  (,q-aux n d)
			  *empty-type*))
		    (derive-r (n d same-arg)
		      (declare (ignore same-arg))
		      (if (and (numeric-type-real-p n)
			       (numeric-type-real-p d))
			  (,r-aux n d)
			  *empty-type*)))
	       (let ((quot (two-arg-derive-type
			    number divisor #'derive-q #',name))
		     (rem (two-arg-derive-type
			   number divisor #'derive-r #'mod)))
		 (when (and quot rem)
		   (make-values-type :required (list quot rem))))))))))

  (frob-opt ffloor floor-quotient-bound floor-rem-bound)
  (frob-opt fceiling ceiling-quotient-bound ceiling-rem-bound))

;;; Functions to compute the bounds on the quotient and remainder for
;;; the FLOOR function.

(defun floor-quotient-bound (quot)
  ;; Take the floor of the quotient and then massage it into what we
  ;; need.
  (let ((lo (interval-low quot))
	(hi (interval-high quot)))
    ;; Take the floor of the lower bound.  The result is always a
    ;; closed lower bound.
    (setf lo (if lo
		 (floor (bound-value lo))
		 nil))
    ;; For the upper bound, we need to be careful
    (setf hi
	  (cond ((consp hi)
		 ;; An open bound.  We need to be careful here because
		 ;; the floor of '(10.0) is 9, but the floor of
		 ;; 10.0 is 10.
		 (multiple-value-bind (q r)
		     (floor (first hi))
		   (if (zerop r)
		       (1- q)
		       q)))
		(hi
		 ;; A closed bound, so the answer is obvious.
		 (floor hi))
		(t
		 hi)))
    (make-interval :low lo :high hi)))

(defun floor-rem-bound (div)
  ;; The remainder depends only on the divisor.  Try to get the
  ;; correct sign for the remainder if we can.
  
  (case (interval-range-info div)
    (+
     ;; Divisor is always positive.  
     (let ((rem (interval-abs div)))
       (setf (interval-low rem) 0)
       (when (and (numberp (interval-high rem))
		  (not (zerop (interval-high rem))))
	 ;; The remainder never contains the upper bound.  However,
	 ;; watch out for the case where the high limit is zero!
	 (setf (interval-high rem) (list (interval-high rem))))
       rem))
    (-
     ;; Divisor is always negative
     (let ((rem (interval-neg (interval-abs div))))
       (setf (interval-high rem) 0)
       (when (numberp (interval-low rem))
	 ;; The remainder never contains the lower bound.
	 (setf (interval-low rem) (list (interval-low rem))))
       rem))
    (otherwise
     ;; The divisor can be positive or negative.  All bets off.
     ;; The magnitude of remainder is the maximum value of the
     ;; divisor.
     (let ((limit (bound-value (interval-high (interval-abs div)))))
       ;; The bound never reaches the limit, so make the interval open
       (make-interval :low (if limit
			       (list (- limit))
			       limit)
		      :high (list limit))))))
#| Test cases
(floor-quotient-bound (make-interval :low 0.3 :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low 0.3 :high '(10.3)))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low 0.3 :high 10))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low 0.3 :high '(10)))
=> #S(INTERVAL :LOW 0 :HIGH 9)
(floor-quotient-bound (make-interval :low '(0.3) :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low '(0.0) :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low '(-1.3) :high 10.3))
=> #S(INTERVAL :LOW -2 :HIGH 10)
(floor-quotient-bound (make-interval :low '(-1.0) :high 10.3))
=> #S(INTERVAL :LOW -1 :HIGH 10)
(floor-quotient-bound (make-interval :low -1.0 :high 10.3))
=> #S(INTERVAL :LOW -1 :HIGH 10)


(floor-rem-bound (make-interval :low 0.3 :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH '(10.3))
(floor-rem-bound (make-interval :low 0.3 :high '(10.3)))
=> #S(INTERVAL :LOW 0 :HIGH '(10.3))
(floor-rem-bound (make-interval :low -10 :high -2.3))
#S(INTERVAL :LOW (-10) :HIGH 0)
(floor-rem-bound (make-interval :low 0.3 :high 10))
=> #S(INTERVAL :LOW 0 :HIGH '(10))
(floor-rem-bound (make-interval :low '(-1.3) :high 10.3))
=> #S(INTERVAL :LOW '(-10.3) :HIGH '(10.3))
(floor-rem-bound (make-interval :low '(-20.3) :high 10.3))
=> #S(INTERVAL :LOW (-20.3) :HIGH (20.3))
|#


;;; Same functions for CEILING
(defun ceiling-quotient-bound (quot)
  ;; Take the ceiling of the quotient and then massage it into what we
  ;; need.
  (let ((lo (interval-low quot))
	(hi (interval-high quot)))
    ;; Take the ceiling of the upper bound.  The result is always a
    ;; closed upper bound.
    (setf hi (if hi
		 (ceiling (bound-value hi))
		 nil))
    ;; For the lower bound, we need to be careful
    (setf lo
	  (cond ((consp lo)
		 ;; An open bound.  We need to be careful here because
		 ;; the ceiling of '(10.0) is 11, but the ceiling of
		 ;; 10.0 is 10.
		 (multiple-value-bind (q r)
		     (ceiling (first lo))
		   (if (zerop r)
		       (1+ q)
		       q)))
		(lo
		 ;; A closed bound, so the answer is obvious.
		 (ceiling lo))
		(t
		 lo)))
    (make-interval :low lo :high hi)))


(defun ceiling-rem-bound (div)
  ;; The remainder depends only on the divisor.  Try to get the
  ;; correct sign for the remainder if we can.
  
  (case (interval-range-info div)
    (+
     ;; Divisor is always positive.  The remainder is negative.
     (let ((rem (interval-neg (interval-abs div))))
       (setf (interval-high rem) 0)
       (when (and (numberp (interval-low rem))
		  (not (zerop (interval-low rem))))
	 ;; The remainder never contains the upper bound.  However,
	 ;; watch out for the case when the upper bound is zero!
	 (setf (interval-low rem) (list (interval-low rem))))
       rem))
    (-
     ;; Divisor is always negative.  The remainder is positive
     (let ((rem (interval-abs div)))
       (setf (interval-low rem) 0)
       (when (numberp (interval-high rem))
	 ;; The remainder never contains the lower bound.
	 (setf (interval-high rem) (list (interval-high rem))))
       rem))
    (otherwise
     ;; The divisor can be positive or negative.  All bets off.
     ;; The magnitude of remainder is the maximum value of the
     ;; divisor.
     (let ((limit (bound-value (interval-high (interval-abs div)))))
       ;; The bound never reaches the limit, so make the interval open
       (make-interval :low (if limit
			       (list (- limit))
			       limit)
		      :high (list limit))))))

#| Test cases
(ceiling-quotient-bound (make-interval :low 0.3 :high 10.3))
=> #S(INTERVAL :LOW 1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low 0.3 :high '(10.3)))
=> #S(INTERVAL :LOW 1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low 0.3 :high 10))
=> #S(INTERVAL :LOW 1 :HIGH 10)
(ceiling-quotient-bound (make-interval :low 0.3 :high '(10)))
=> #S(INTERVAL :LOW 1 :HIGH 10)
(ceiling-quotient-bound (make-interval :low '(0.3) :high 10.3))
=> #S(INTERVAL :LOW 1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low '(0.0) :high 10.3))
=> #S(INTERVAL :LOW 1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low '(-1.3) :high 10.3))
=> #S(INTERVAL :LOW -1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low '(-1.0) :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH 11)
(ceiling-quotient-bound (make-interval :low -1.0 :high 10.3))
=> #S(INTERVAL :LOW -1 :HIGH 11)


(ceiling-rem-bound (make-interval :low 0.3 :high 10.3))
=> #S(INTERVAL :LOW (-10.3) :HIGH 0)
(ceiling-rem-bound (make-interval :low 0.3 :high '(10.3)))
=> #S(INTERVAL :LOW 0 :HIGH '(10.3))
(ceiling-rem-bound (make-interval :low -10 :high -2.3))
=> #S(INTERVAL :LOW 0 :HIGH (10))
(ceiling-rem-bound (make-interval :low 0.3 :high 10))
=> #S(INTERVAL :LOW (-10) :HIGH 0)
(ceiling-rem-bound (make-interval :low '(-1.3) :high 10.3))
=> #S(INTERVAL :LOW (-10.3) :HIGH (10.3))
(ceiling-rem-bound (make-interval :low '(-20.3) :high 10.3))
=> #S(INTERVAL :LOW (-20.3) :HIGH (20.3))
|#





(defun truncate-quotient-bound (quot)
  ;; For positive quotients, truncate is exactly like floor.  For
  ;; negative quotients, truncate is exactly like ceiling.  Otherwise,
  ;; it's the union of the two pieces.
  (case (interval-range-info quot)
    (+
     ;; Just like floor
     (floor-quotient-bound quot))
    (-
     ;; Just like ceiling
     (ceiling-quotient-bound quot))
    (otherwise
     ;; Split the interval into positive and negative pieces, compute
     ;; the result for each piece and put them back together.
     (destructuring-bind (neg pos)
	 (interval-split 0 quot t t)
       (interval-merge-pair (ceiling-quotient-bound neg)
			    (floor-quotient-bound pos))))))


(defun truncate-rem-bound (num div)
  ;; This is significantly more complicated than floor or ceiling.  We
  ;; need both the number and the divisor to determine the range.  The
  ;; basic idea is to split the ranges of num and den into positive
  ;; and negative pieces and deal with each of the four possibilities
  ;; in turn.
  (case (interval-range-info num)
    (+
     (case (interval-range-info div)
       (+
	(floor-rem-bound div))
       (-
	(ceiling-rem-bound div))
       (otherwise
	(destructuring-bind (neg pos)
	    (interval-split 0 div t t)
	  (interval-merge-pair (truncate-rem-bound num neg)
			       (truncate-rem-bound num pos))))))
    (-
     (case (interval-range-info div)
       (+
	(ceiling-rem-bound div))
       (-
	(floor-rem-bound div))
       (otherwise
	(destructuring-bind (neg pos)
	    (interval-split 0 div t t)
	  (interval-merge-pair (truncate-rem-bound num neg)
			       (truncate-rem-bound num pos))))))
    (otherwise
     (destructuring-bind (neg pos)
	 (interval-split 0 num t t)
       (interval-merge-pair (truncate-rem-bound neg div)
			    (truncate-rem-bound pos div))))))




;;; NUMERIC-RANGE-INFO  --  internal.
;;;
;;; Derive useful information about the range.  Returns three values:
;;; - '+ if its positive, '- negative, or nil if it overlaps 0.
;;; - The abs of the minimal value (i.e. closest to 0) in the range.
;;; - The abs of the maximal value if there is one, or nil if it is
;;;   unbounded.
;;;
(defun numeric-range-info (low high)
  (cond ((and low (not (minusp low)))
	 (values '+ low high))
	((and high (not (plusp high)))
	 (values '- (- high) (if low (- low) nil)))
	(t
	 (values nil 0 (and low high (max (- low) high))))))

;;; INTEGER-TRUNCATE-DERIVE-TYPE -- internal
;;; 
(defun integer-truncate-derive-type
       (number-low number-high divisor-low divisor-high)
  ;; The result cannot be larger in magnitude than the number, but the sign
  ;; might change.  If we can determine the sign of either the number or
  ;; the divisor, we can eliminate some of the cases.
  (multiple-value-bind
      (number-sign number-min number-max)
      (numeric-range-info number-low number-high)
    (multiple-value-bind
	(divisor-sign divisor-min divisor-max)
	(numeric-range-info divisor-low divisor-high)
      (when (and divisor-max (zerop divisor-max))
	;; We've got a problem: guarenteed division by zero.
	(return-from integer-truncate-derive-type t))
      (when (zerop divisor-min)
	;; We'll assume that they aren't going to divide by zero.
	(incf divisor-min))
      (cond ((and number-sign divisor-sign)
	     ;; We know the sign of both.
	     (if (eq number-sign divisor-sign)
		 ;; Same sign, so the result will be positive.
		 `(integer ,(if divisor-max
				(truncate number-min divisor-max)
				0)
			   ,(if number-max
				(truncate number-max divisor-min)
				'*))
		 ;; Different signs, the result will be negative.
		 `(integer ,(if number-max
				(- (truncate number-max divisor-min))
				'*)
			   ,(if divisor-max
				(- (truncate number-min divisor-max))
				0))))
	    ((eq divisor-sign '+)
	     ;; The divisor is positive.  Therefore, the number will just
	     ;; become closer to zero.
	     `(integer ,(if number-low
			    (truncate number-low divisor-min)
			    '*)
		       ,(if number-high
			    (truncate number-high divisor-min)
			    '*)))
	    ((eq divisor-sign '-)
	     ;; The divisor is negative.  Therefore, the absolute value of
	     ;; the number will become closer to zero, but the sign will also
	     ;; change.
	     `(integer ,(if number-high
			    (- (truncate number-high divisor-min))
			    '*)
		       ,(if number-low
			    (- (truncate number-low divisor-min))
			    '*)))
	    ;; The divisor could be either positive or negative.
	    (number-max
	     ;; The number we are dividing has a bound.  Divide that by the
	     ;; smallest posible divisor.
	     (let ((bound (truncate number-max divisor-min)))
	       `(integer ,(- bound) ,bound)))
	    (t
	     ;; The number we are dividing is unbounded, so we can't tell
	     ;; anything about the result.
	     `integer)))))


(defun random-derive-type-aux (type)
  (let ((class (numeric-type-class type))
	(high (numeric-type-high type))
	(format (numeric-type-format type)))
    (make-numeric-type
	 :class class
	 :format format
	 :low (coerce 0 (or format class 'real))
	 :high (cond ((not high) nil)
		     ((eq class 'integer) (max (1- high) 0))
		     ((or (consp high) (zerop high)) high)
		     (t `(,high))))))

(defoptimizer (random derive-type) ((bound &optional state))
  (one-arg-derive-type bound #'random-derive-type-aux nil))


;;;; Logical derive-type methods:


;;; Integer-Type-Length -- Internal
;;;
;;; Return the maximum number of bits an integer of the supplied type can take
;;; up, or NIL if it is unbounded.  The second (third) value is T if the
;;; integer can be positive (negative) and NIL if not.  Zero counts as
;;; positive.
;;;
(defun integer-type-length (type)
  (if (numeric-type-p type)
      (let ((min (numeric-type-low type))
	    (max (numeric-type-high type)))
	(values (and min max (max (integer-length min) (integer-length max)))
		(or (null max) (not (minusp max)))
		(or (null min) (minusp min))))
      (values nil t t)))

;;; From Hacker's Delight, by Henry S. Warren, Jr.

;;; Let a <= x <= b and c <= y <= d, with X and Y both unsigned 32-bit
;;; numbers.  (Mostly because that's what the routines support, but
;;; they could be extended to any positive integer.)  MIN-AND and
;;; MAX-AND compute reasonably tight bounds on x&y.  MIN-OR and MAX-OR
;;; compute the bounds on x|y.

(defun min-and (a b c d)
  ;; Note that the body of the loop doesn't really do anything unless
  ;; ~a&~c&m is non-zero.  So, rather than start m at #x80000000, we
  ;; can start at the most significant bit where ~a&~c is non-zero.
  (let ((m (ash 1 (integer-length (logandc2 (lognot a) c)))))
    (loop while (not (zerop m))
       do
       (when (/= (logand m (lognot a) (lognot c)) 0)
	 (let ((temp (logandc2 (logior a m) (- m 1))))
	   (when (<= temp b)
	     (setf a temp)
	     (return)))
	 (let ((temp (logandc2 (logior c m) (- m 1))))
	   (when (<= temp d)
	     (setf c temp)
	     (return))))
       (setf m (ash m -1)))
    (logand a c)))

(defun max-and (a b c d)
  ;; Note that the body of the loop doesn't really do anything unless
  ;; b&~d&m is non-zero or ~b&d&m is non-zero.  So, rather than start
  ;; m at #x80000000, we can start at the most significant bit where
  ;; b&~d or ~b&d is non-zero.  That is, b^d is non-zero
  (let ((m (ash 1 (integer-length (logxor b d)))))
    (loop while (not (zerop m))
       do
       (cond ((/= (logand b (lognot d) m) 0)
	      (let ((temp (logior (logandc2 b m) (- m 1))))
		(when (>= temp a)
		  (setf b temp)
		  (return))))
	     ((/= (logand (lognot b) d m) 0)
	      (let ((temp (logior (logandc2 d m) (- m 1))))
		(when (>= temp c)
		  (setf d temp)
		  (return)))))
       (setf m (ash m -1))))
  (logand b d))

(defun min-or (a b c d)
  ;; Note that the body of the loop doesn't do anything unless ~a&c&m
  ;; is non-zero or if ~c&a&m is non-zero.  So rather than start m at
  ;; #x80000000, we can start at the most significant bit where ~a&c
  ;; or ~c&a is non-zero, i.e., where MSB of a^c.
  (let ((m (ash 1 (integer-length (logxor a c)))))
    (loop while (not (zerop m))
       do
       (cond ((/= (logandc2 (logand c m) a) 0)
	      (let ((temp (logand (logior a m)
				  (1+ (lognot m)))))
		(when (<= temp b)
		  (setf a temp)
		  (return))))
	     ((/= (logandc1 c (logand a m)) 0)
	      (let ((temp (logand (logior c m)
				  (1+ (lognot m)))))
		(when (<= temp d)
		  (setf c temp)
		  (return)))))
       (setf m (ash m -1))))
  (logior a c))

(defun max-or (a b c d)
  ;; Note that the body of the loop doesn't do anything unless b&d&m
  ;; is non-zero.  That is, when the MSB of b&d is non-zero.
  (let ((m (ash 1 (integer-length (logand b d)))))
    (loop while (not (zerop m))
       do
       (when (/= (logand m b d) 0)
	 (let ((temp (logior (- b m)
			     (- m 1))))
	   (when (>= temp a)
	     (setf b temp)
	     (return)))
	 (let ((temp (logior (- d m)
			     (- m 1))))
	   (when (>= temp c)
	     (setf d temp)
	     (return))))
	 (setf m (ash m -1)))
    (logior b d)))
  
(defun min-xor (a b c d)
  ;; Note that the body of the loop doesn't do anything unless ~a&c&m
  ;; is non-zero or if ~c&a&m is non-zero.  So rather than start m at
  ;; #x80000000, we can start at the most significant bit where ~a&c
  ;; or ~c&a is non-zero, i.e., where MSB of a^c is 1.
  (let ((m (ash 1 (1- (integer-length (logxor a c))))))
    (loop while (not (zerop m))
       do
       (cond ((/= (logandc2 (logand c m) a) 0)
	      (let ((temp (logand (logior a m)
				  (1+ (lognot m)))))
		(when (<= temp b)
		  (setf a temp))))
	     ((/= (logandc1 c (logand a m)) 0)
	      (let ((temp (logand (logior c m)
				  (1+ (lognot m)))))
		(when (<= temp d)
		  (setf c temp)))))
       (setf m (ash m -1))))
  (logxor a c))

(defun max-xor (a b c d)
  ;; Note that the body of the loop doesn't do anything unless b&d&m
  ;; is non-zero.  So rather than start m at #x80000000, we can start
  ;; at the most significant bit where b&d is non-zero.
  (let ((m (ash 1 (1- (integer-length (logand b d))))))
    (loop while (not (zerop m))
       do
       (when (/= (logand m b d) 0)
	 (let ((temp (logior (- b m)
			     (- m 1))))
	   (if (>= temp a)
	       (setf b temp)
	       (let ((temp (logior (- d m)
				   (- m 1))))
		 (when (>= temp c)
		   (setf d temp))))))
       (setf m (ash m -1)))
    (logxor b d)))
  

(defun logand-derive-type-aux (x y &optional same-leaf)
  (declare (ignore same-leaf))
  (multiple-value-bind
	(x-len x-pos x-neg)
      (integer-type-length x)
    (declare (ignore x-pos))
    (multiple-value-bind
	  (y-len y-pos y-neg)
	(integer-type-length  y)
      (declare (ignore y-pos))
      (if (not x-neg)
	  ;; X must be positive.
	  (if (not y-neg)
	      ;; The must both be positive.
	      (cond ((or (null x-len) (null y-len))
		     (specifier-type 'unsigned-byte))
		    ((or (zerop x-len) (zerop y-len))
		     (specifier-type '(integer 0 0)))
		    ((and (<= x-len 32) (<= y-len 32))
		     ;; If both args are unsigned 32-bit numbers, we
		     ;; can compute better bounds, so we do.  But if
		     ;; one arg is a constant and is a single bit, we
		     ;; can do even better.
		     ;;
		     ;; What about the case where one arg is constant
		     ;; and has several bits set?  We could compute
		     ;; exact values by turning off each individual
		     ;; bit and all combinations thereof.  Should we?
		     (let ((xlo (numeric-type-low x))
			   (xhi (numeric-type-high x))
			   (ylo (numeric-type-low y))
			   (yhi (numeric-type-high y)))
		       (cond ((and (= xlo xhi) (= 1 (logcount xlo)))
			      (specifier-type `(member 0 ,xlo)))
			     ((and (= ylo yhi) (= 1 (logcount ylo)))
			      (specifier-type `(member 0 ,ylo)))
			     (t
			      (specifier-type `(integer ,(min-and xlo xhi ylo yhi)
							,(max-and xlo xhi ylo yhi)))))))
		    (t
		     (specifier-type `(unsigned-byte ,(min x-len y-len)))))
	      ;; X is positive, but Y might be negative.
	      (cond ((null x-len)
		     (specifier-type 'unsigned-byte))
		    ((zerop x-len)
		     (specifier-type '(integer 0 0)))
		    (t
		     (specifier-type `(unsigned-byte ,x-len)))))
	  ;; X might be negative.
	  (if (not y-neg)
	      ;; Y must be positive.
	      (cond ((null y-len)
		     (specifier-type 'unsigned-byte))
		    ((zerop y-len)
		     (specifier-type '(integer 0 0)))
		    (t
		     (specifier-type
		      `(unsigned-byte ,y-len))))
	      ;; Either might be negative.
	      (if (and x-len y-len)
		  ;; The result is bounded.
		  (specifier-type `(signed-byte ,(1+ (max x-len y-len))))
		  ;; We can't tell squat about the result.
		  (specifier-type 'integer)))))))

(defun logior-derive-type-aux (x y &optional same-leaf)
  (declare (ignore same-leaf))
  (multiple-value-bind
	(x-len x-pos x-neg)
      (integer-type-length x)
    (multiple-value-bind
	  (y-len y-pos y-neg)
	(integer-type-length y)
      (cond ((and (not x-neg) (not y-neg))
	     ;; Both are positive.
	     (cond ((or (null x-len) (null y-len))
		    (specifier-type 'unsigned-byte))
		   ((and (zerop x-len) (zerop y-len))
		    (specifier-type '(integer 0 0)))
		   ((and (<= x-len 32) (<= y-len 32))
		    (let ((xlo (numeric-type-low x))
			  (xhi (numeric-type-high x))
			  (ylo (numeric-type-low y))
			  (yhi (numeric-type-high y)))
		      (specifier-type `(integer ,(min-or xlo xhi ylo yhi)
						,(max-or xlo xhi ylo yhi)))))
		   (t
		    (specifier-type `(unsigned-byte ,(max x-len y-len))))))
	    ((not x-pos)
	     ;; X must be negative.
	     (if (not y-pos)
		 ;; Both are negative.  The result is going to be negative and be
		 ;; the same length or shorter than the smaller.
		 (if (and x-len y-len)
		     ;; It's bounded.
		     (specifier-type `(integer ,(ash -1 (min x-len y-len)) -1))
		     ;; It's unbounded.
		     (specifier-type '(integer * -1)))
		 ;; X is negative, but we don't know about Y.  The result will be
		 ;; negative, but no more negative than X.
		 (specifier-type
		  `(integer ,(or (numeric-type-low x) '*)
			    -1))))
	    (t
	     ;; X might be either positive or negative.
	     (if (not y-pos)
		 ;; But Y is negative.  The result will be negative.
		 (specifier-type
		  `(integer ,(or (numeric-type-low y) '*)
			    -1))
		 ;; We don't know squat about either.  It won't get any bigger.
		 (if (and x-len y-len)
		     ;; Bounded.
		     (specifier-type `(signed-byte ,(1+ (max x-len y-len))))
		     ;; Unbounded.
		     (specifier-type 'integer))))))))

(defun logxor-derive-type-aux (x y &optional same-leaf)
  (declare (ignore same-leaf))
  (multiple-value-bind
	(x-len x-pos x-neg)
      (integer-type-length x)
    (multiple-value-bind
	  (y-len y-pos y-neg)
	(integer-type-length y)
      (cond
	((and (not x-neg) (not y-neg))
	 ;; Both are positive
	 (cond ((or (null x-len) (null y-len))
		(specifier-type 'unsigned-byte))
	       ((and (zerop x-len) (zerop y-len))
		(specifier-type '(integer 0 0)))
	       ((and (<= x-len 32) (<= y-len 32))
		(let ((xlo (numeric-type-low x))
		      (xhi (numeric-type-high x))
		      (ylo (numeric-type-low y))
		      (yhi (numeric-type-high y)))
		  (specifier-type `(integer ,(min-xor xlo xhi ylo yhi)
					    ,(max-xor xlo xhi ylo yhi)))))
	       (t
		(specifier-type `(unsigned-byte ,(max x-len y-len))))))
	((and (not x-pos) (not y-pos))
	 ;; Both are negative.  The result will be positive, and as
	 ;; long as the longer.
	 (if (and x-len y-len (zerop x-len) (zerop y-len))
	     (specifier-type '(integer 0 0))
	     (specifier-type `(unsigned-byte ,(if (and x-len y-len)
						  (max x-len y-len)
						  '*)))))	 
       ((or (and (not x-pos) (not y-neg))
	    (and (not y-neg) (not y-pos)))
	;; Either X is negative and Y is positive of vice-verca.  The result
	;; will be negative.
	(specifier-type `(integer ,(if (and x-len y-len)
				       (ash -1 (max x-len y-len))
				       '*)
				  -1)))
       ;; We can't tell what the sign of the result is going to be.  All we
       ;; know is that we don't create new bits.
       ((and x-len y-len)
	(specifier-type `(signed-byte ,(1+ (max x-len y-len)))))
       (t
	(specifier-type 'integer))))))

(defun derive-integer-type-aux (x y fun)
  (declare (type function fun))
  (if (and (numeric-type-p x) (numeric-type-p y)
           (eq (numeric-type-class x) 'integer)
           (eq (numeric-type-class y) 'integer)
           (eq (numeric-type-complexp x) :real)
           (eq (numeric-type-complexp y) :real))
      (multiple-value-bind (low high) (funcall fun x y)
        (make-numeric-type :class 'integer
                           :complexp :real
                           :low low
                           :high high))
      (numeric-contagion x y)))

(defun lognot-derive-type-aux (int)
  (derive-integer-type-aux int int
                           (lambda (type type2)
                             (declare (ignore type2))
                             (let ((lo (numeric-type-low type))
                                   (hi (numeric-type-high type)))
                               (values (if hi (lognot hi) nil)
                                       (if lo (lognot lo) nil)
                                       (numeric-type-class type)
                                       (numeric-type-format type))))))

(defoptimizer (lognot derive-type) ((int))
  (lognot-derive-type-aux (continuation-type int)))


(macrolet ((frob (logfcn)
	     (let ((fcn-aux (symbolicate logfcn "-DERIVE-TYPE-AUX")))
	     `(defoptimizer (,logfcn derive-type) ((x y))
	        (two-arg-derive-type x y #',fcn-aux #',logfcn)))))
  (frob logand)
  (frob logior)
  (frob logxor))

;;; FIXME: could actually do stuff with SAME-LEAF
(defoptimizer (logeqv derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux 
                              (logxor-derive-type-aux x y same-leaf))) 
                       #'logeqv))
(defoptimizer (lognand derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux
                              (logand-derive-type-aux x y same-leaf)))
                       #'lognand))
(defoptimizer (lognor derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux
                              (logior-derive-type-aux x y same-leaf)))
                       #'lognor))

;; We should do something better for these.  (logandc1 x x) is 0, for
;; example.
(defoptimizer (logandc1 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
			     (declare (ignore same-leaf))
                             (logand-derive-type-aux
                              (lognot-derive-type-aux x) y nil))
                       #'logandc1))
(defoptimizer (logandc2 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
			     (declare (ignore same-leaf))
                             (logand-derive-type-aux
                              x (lognot-derive-type-aux y) nil))
                       #'logandc2))
(defoptimizer (logorc1 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
			     (declare (ignore same-leaf))
                             (logior-derive-type-aux
                              (lognot-derive-type-aux x) y nil))
                       #'logorc1))
(defoptimizer (logorc2 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
			     (declare (ignore same-leaf))
                             (logior-derive-type-aux
                              x (lognot-derive-type-aux y) nil))
                       #'logorc2))

(defoptimizer (integer-length derive-type) ((num))
  (one-arg-derive-type
   num
   #'(lambda (type)
       (when (and (numeric-type-p type)
		  (eq (numeric-type-class type) 'integer))
	 (let ((low (numeric-type-low type))
	       (high (numeric-type-high type)))
	   (cond ((and low (>= low 0))
		  (make-numeric-type :class 'integer :complexp :real
				     :low (integer-length low)
				     :high (and high (integer-length high))))
		 ((and high (<= high 0))
		  (make-numeric-type :class 'integer :complexp :real
				     :low (integer-length high)
				     :high (and low (integer-length low))))
		 ((and low (<= low 0) high (>= high 0))
		  (make-numeric-type :class 'integer :complexp :real
				     :low 0 :high (max (integer-length low)
						       (integer-length high))))
		 (t
		  (make-numeric-type :class 'integer :complexp :real
				     :low 0 :high nil))))))
   #'integer-length))




;;;; Miscellaneous derive-type methods:


(defoptimizer (code-char derive-type) ((code))
  (specifier-type 'base-char))


(defoptimizer (values derive-type) ((&rest values))
  (values-specifier-type
   `(values ,@(mapcar #'(lambda (x)
			  (type-specifier (continuation-type x)))
		      values))))


(defun signum-derive-type-aux (type)
  ;; The signum of a complex number is a complex number of the same
  ;; type, except complex rationals become complex single-floats. The
  ;; signum of a real number is 0, 1, or -1, of the same type.
  (if (eq (numeric-type-complexp type) :complex)
      (if (eq (numeric-type-class type) 'rational)
	  (specifier-type '(complex single-float))
	  type)
      (let* ((type-interval (numeric-type->interval type))
	     (range-info (interval-range-info type-interval))
	     (contains-0-p (interval-contains-p 0 type-interval))
	     (num-plus1 (coerce 1 (or (numeric-type-format type) 'real)))
	     (num-minus1 (coerce -1 (or (numeric-type-format type) 'real)))
	     (plus (make-numeric-type :class (numeric-type-class type)
				      :format (numeric-type-format type)
				      :low num-plus1 :high num-plus1))
	     (minus (make-numeric-type :class (numeric-type-class type)
				       :format (numeric-type-format type)
				       :low num-minus1 :high num-minus1))
	     ;; We need to handle signed zeroes because (signum -0.0)
	     ;; is -0.0.
	     (zero (if (eq (numeric-type-class type) 'float)
		       (case (numeric-type-format type)
			 (single-float
			  (specifier-type '(single-float -0.0 0.0)))
			 (double-float
			  (specifier-type '(double-float -0d0 0d0)))
			 ((nil)
			  (specifier-type '(float -0d0 0d0))))
		       (specifier-type '(eql 0)))))
	(case range-info
	  (+ (if contains-0-p
	       (type-union zero plus)
	       plus))
	  (-
	   (if contains-0-p
	       (type-union zero minus)
	       minus))
	  (t
	   (type-union zero plus minus))))))

	
(defoptimizer (signum derive-type) ((num))
  (one-arg-derive-type num #'signum-derive-type-aux nil))


;;;; Byte operations:
;;;
;;;    We try to turn byte operations into simple logical operations.  First,
;;; we convert byte specifiers into separate size and position arguments passed
;;; to internal %FOO functions.  We then attempt to transform the %FOO
;;; functions into boolean operations when the size and position are constant
;;; and the operands are fixnums.


;;; With-Byte-Specifier  --  Internal
;;;
;;;    Evaluate body with Size-Var and Pos-Var bound to expressions that
;;; evaluate to the Size and Position of the byte-specifier form Spec.  We may
;;; wrap a let around the result of the body to bind some variables.
;;;
;;;    If the spec is a Byte form, then bind the vars to the subforms.
;;; otherwise, evaluate Spec and use the Byte-Size and Byte-Position.  The goal
;;; of this transformation is to avoid consing up byte specifiers and then
;;; immediately throwing them away.
;;;
(defmacro with-byte-specifier ((size-var pos-var spec) &body body)
  (once-only ((spec `(macroexpand ,spec))
	      (temp '(gensym)))
    `(if (and (consp ,spec)
	      (eq (car ,spec) 'byte)
	      (= (length ,spec) 3))
	 (let ((,size-var (second ,spec))
	       (,pos-var (third ,spec)))
	   ,@body)
	 (let ((,size-var `(byte-size ,,temp))
	       (,pos-var `(byte-position ,,temp)))
	   `(let ((,,temp ,,spec))
	      ,,@body)))))

(def-source-transform ldb (spec int)
  (with-byte-specifier (size pos spec)
    `(%ldb ,size ,pos ,int)))

(def-source-transform dpb (newbyte spec int)
  (with-byte-specifier (size pos spec)
    `(%dpb ,newbyte ,size ,pos ,int)))

(def-source-transform mask-field (spec int)
  (with-byte-specifier (size pos spec)
    `(%mask-field ,size ,pos ,int)))

(def-source-transform deposit-field (newbyte spec int)
  (with-byte-specifier (size pos spec)
    `(%deposit-field ,newbyte ,size ,pos ,int)))


(defoptimizer (%ldb derive-type) ((size posn num))
  (let ((size (continuation-type size)))
    (if (and (numeric-type-p size)
	     (csubtypep size (specifier-type 'integer)))
	(let ((size-high (numeric-type-high size)))
	  (if (and size-high (<= 1 size-high vm:word-bits))
	      (specifier-type `(unsigned-byte ,size-high))
	      (specifier-type 'unsigned-byte)))
	*universal-type*)))

(defoptimizer (%mask-field derive-type) ((size posn num))
  (let ((size (continuation-type size))
	(posn (continuation-type posn)))
    (if (and (numeric-type-p size)
	     (csubtypep size (specifier-type 'integer))
	     (numeric-type-p posn)
	     (csubtypep posn (specifier-type 'integer)))
	(let ((size-high (numeric-type-high size))
	      (posn-high (numeric-type-high posn)))
	  (if (and size-high posn-high
		   (<= 1 (+ size-high posn-high) vm:word-bits))
	      (specifier-type `(unsigned-byte ,(+ size-high posn-high)))
	      (specifier-type 'unsigned-byte)))
	*universal-type*)))

(defun %dpb-derive-type-aux (size posn int)
  (let ((size (continuation-type size))
	(posn (continuation-type posn))
	(int (continuation-type int)))
    (when (and (numeric-type-p size)
	       (numeric-type-p posn)
	       (numeric-type-p int))
      (let ((size-high (numeric-type-high size))
	    (posn-high (numeric-type-high posn))
	    (high (numeric-type-high int))
	    (low (numeric-type-low int)))
	(when (and size-high posn-high high low
		   (<= (+ size-high posn-high) vm:word-bits))
	  (let ((raw-bit-count (max (integer-length high)
				    (integer-length low)
				    (+ size-high posn-high))))
	    (specifier-type
	     (if (minusp low)
		 `(signed-byte ,(1+ raw-bit-count))
		 `(unsigned-byte ,raw-bit-count)))))))))

(defoptimizer (%dpb derive-type) ((newbyte size posn int))
  (%dpb-derive-type-aux size posn int))
  
(defoptimizer (%deposit-field derive-type) ((newbyte size posn int))
  (%dpb-derive-type-aux size posn int))
  
(deftransform %ldb ((size posn int)
		    (fixnum fixnum integer)
		    (unsigned-byte #.vm:word-bits))
  "convert to inline logical ops"
  `(logand (ash int (- posn))
	   (ash ,(1- (ash 1 vm:word-bits))
		(- size ,vm:word-bits))))

(deftransform %mask-field ((size posn int)
			   (fixnum fixnum integer)
			   (unsigned-byte #.vm:word-bits))
  "convert to inline logical ops"
  `(logand int
	   (ash (ash ,(1- (ash 1 vm:word-bits))
		     (- size ,vm:word-bits))
		posn)))

;;; Note: for %dpb and %deposit-field, we can't use (or (signed-byte n)
;;; (unsigned-byte n)) as the result type, as that would allow result types
;;; that cover the range -2^(n-1) .. 1-2^n, instead of allowing result types
;;; of (unsigned-byte n) and result types of (signed-byte n).

(deftransform %dpb ((new size posn int)
		    *
		    (unsigned-byte #.vm:word-bits))
  "convert to inline logical ops"
  `(let ((mask (ldb (byte size 0) -1)))
     (logior (ash (logand new mask) posn)
	     (logand int (lognot (ash mask posn))))))

(deftransform %dpb ((new size posn int)
		    *
		    (signed-byte #.vm:word-bits))
  "convert to inline logical ops"
  `(let ((mask (ldb (byte size 0) -1)))
     (logior (ash (logand new mask) posn)
	     (logand int (lognot (ash mask posn))))))

(deftransform %deposit-field ((new size posn int)
			      *
			      (unsigned-byte #.vm:word-bits))
  "convert to inline logical ops"
  `(let ((mask (ash (ldb (byte size 0) -1) posn)))
     (logior (logand new mask)
	     (logand int (lognot mask)))))

(deftransform %deposit-field ((new size posn int)
			      *
			      (signed-byte #.vm:word-bits))
  "convert to inline logical ops"
  `(let ((mask (ash (ldb (byte size 0) -1) posn)))
     (logior (logand new mask)
	     (logand int (lognot mask)))))


;;; Miscellanous numeric transforms:


;;; COMMUTATIVE-ARG-SWAP  --  Internal
;;;
;;;    If a constant appears as the first arg, swap the args.
;;;
(deftransform commutative-arg-swap ((x y) * * :defun-only t :node node)
  (if (and (constant-continuation-p x)
	   (not (constant-continuation-p y)))
      `(,(continuation-function-name (basic-combination-fun node))
	y
	,(continuation-value x))
      (give-up)))

(dolist (x '(= char= + * logior logand logxor))
  (%deftransform x '(function * *) #'commutative-arg-swap
		 "place constant arg last."))

;;; Handle the case of a constant boole-code.
;;;
(deftransform boole ((op x y) * * :when :both)
  "convert to inline logical ops"
  (unless (constant-continuation-p op)
    (give-up "BOOLE code is not a constant."))
  (let ((control (continuation-value op)))
    (case control
      (#.boole-clr 0)
      (#.boole-set -1)
      (#.boole-1 'x)
      (#.boole-2 'y)
      (#.boole-c1 '(lognot x))
      (#.boole-c2 '(lognot y))
      (#.boole-and '(logand x y))
      (#.boole-ior '(logior x y))
      (#.boole-xor '(logxor x y))
      (#.boole-eqv '(logeqv x y))
      (#.boole-nand '(lognand x y))
      (#.boole-nor '(lognor x y))
      (#.boole-andc1 '(logandc1 x y))
      (#.boole-andc2 '(logandc2 x y))
      (#.boole-orc1 '(logorc1 x y))
      (#.boole-orc2 '(logorc2 x y))
      (t
       (abort-transform "~S illegal control arg to BOOLE." control)))))


;;;; Convert multiply/divide to shifts.

;;; If arg is a constant power of two, turn * into a shift.
;;;
(deftransform * ((x y) (integer integer) * :when :both)
  "convert x*2^k to shift"
  (unless (constant-continuation-p y) (give-up))
  (let* ((y (continuation-value y))
	 (y-abs (abs y))
	 (len (1- (integer-length y-abs))))
    (unless (and (plusp y-abs)
		 (= y-abs (ash 1 len)))
      (give-up))
    (if (minusp y)
	`(- (ash x ,len))
	`(ash x ,len))))

;;; If both arguments and the result are (unsigned-byte 32), try to come up
;;; with a ``better'' multiplication using multiplier recoding.  There are two
;;; different ways the multiplier can be recoded.  The more obvious is to shift
;;; X by the correct amount for each bit set in Y and to sum the results.  But
;;; if there is a string of bits that are all set, you can add X shifted by
;;; one more then the bit position of the first set bit and subtract X shifted
;;; by the bit position of the last set bit.  We can't use this second method
;;; when the high order bit is bit 31 because shifting by 32 doesn't work
;;; too well.
;;;

;;; This is commented out because its uses of TRULY-THE for vop
;;; selection lie to the compiler, leading to internal
;;; inconsistencies, which in turn lead to incorrect code being
;;; generated.  Example:
;;;
;;; (funcall (compile nil
;;; 	'(lambda () (flet ((%f2 () 288213285))
;;; 		      (+ (%f2) (* 13 (%f2)))))))
;;;  => segmentation violation
;;;
;;;
;;; Another useful test is
;;;      (defun zot ()
;;;	   (let ((v9 (labels ((%f13 () nil)) nil)))
;;;	     (let ((v3 (logandc2 97 3)))
;;;	       (* v3 (- 37391897 (logand v3 -66)))))
;;;
;;; The right fix for this is probably to port SBCL's modular
;;; functions implementation.

#+nil
(deftransform * ((x y)
		 ((unsigned-byte 32) (unsigned-byte 32))
		 (unsigned-byte 32))
  "recode as shift and add"
  (unless (constant-continuation-p y)
    (give-up))
  (let ((y (continuation-value y))
	(result nil)
	(first-one nil)
	(add-count 0)
	(shift-count 0))
    (labels ((tub32 (x) `(truly-the (unsigned-byte 32) ,x))
	     (add (next-factor)
	       (setf result
		     (tub32
		      (if result
			  (progn
			    (incf add-count)
			    `(+ ,result ,(tub32 next-factor)))
			  next-factor)))))
      (declare (inline add))
      (dotimes (bitpos 32)
	(if first-one
	    (when (not (logbitp bitpos y))
	      (add (cond ((= (1+ first-one) bitpos)
			  ;; There is only a single bit in the string.
			  (incf shift-count)
			  `(ash x ,first-one))
			 (t
			  ;; There are at least two.
			  (incf add-count)
			  (incf shift-count 2)
			  `(- ,(tub32 `(ash x ,bitpos))
			    ,(tub32 `(ash x ,first-one))))))
	      (setf first-one nil))
	    (when (logbitp bitpos y)
	      (setf first-one bitpos))))
      (when first-one
	(cond ((= first-one 31))
	      ((= first-one 30)
	       (incf shift-count)
	       (add '(ash x 30)))
	      (t
	       (incf shift-count 2)
	       (add `(- ,(tub32 '(ash x 31)) ,(tub32 `(ash x ,first-one))))))
	(add '(ash x 31)))
      ;; See how many shifts and adds we had to do.  If there are too
      ;; many, it's probably better to use the multiply instruction
      ;; (for those architectures that have multiply instructions).
      ;; Sparc-v7 doesn't have a mutiply instruction.
      ;;
      ;; Some simple tests on Solaris v9 indicates the about 9
      ;; shift-adds is comparable to a multiply.  Use a threshold of
      ;; 9.  Should this be architeucture specific?
      #-sparc-v7
      (when (> (+ add-count shift-count) 9)
	(give-up))
    
      (or result 0))))


;;; If arg is a constant power of two, turn floor into a shift and
;;; mask. If ceiling, add in (1- (abs y)) and do floor, and correct
;;; the remainder.
;;;
(flet ((frob (y ceil-p)
	 (unless (constant-continuation-p y) (give-up))
	 (let* ((y (continuation-value y))
		(y-abs (abs y))
		(len (1- (integer-length y-abs))))
	   (unless (and (plusp y-abs)
			(= y-abs (ash 1 len)))
	     (give-up))
	   (let ((shift (- len))
		 (mask (1- y-abs))
                 (delta (if ceil-p (* (signum y) (1- y-abs)) 0)))
	     `(let ((x (+ x ,delta)))
		,(if (minusp y)
		     `(values (ash (- x) ,shift)
			      (- (- (logand (- x) ,mask)) ,delta))
		     `(values (ash x ,shift)
			      (- (logand x ,mask) ,delta))))))))
  (deftransform floor ((x y) (integer integer) *)
    "convert division by 2^k to shift"
    (frob y nil))
  (deftransform ceiling ((x y) (integer integer) *)
    "convert division by 2^k to shift"
    (frob y t)))


;;; Do the same for mod.
;;;
(deftransform mod ((x y) (integer integer) * :when :both)
  "convert remainder mod 2^k to LOGAND"
  (unless (constant-continuation-p y) (give-up))
  (let* ((y (continuation-value y))
	 (y-abs (abs y))
	 (len (1- (integer-length y-abs))))
    (unless (and (plusp y-abs)
		 (= y-abs (ash 1 len)))
      (give-up))
    (let ((mask (1- y-abs)))
      (if (minusp y)
	  `(- (logand (- x) ,mask))
	  `(logand x ,mask)))))


;;; If arg is a constant power of two, turn truncate into a shift and mask.
;;;
(deftransform truncate ((x y) (integer integer))
  "convert division by 2^k to shift"
  (unless (constant-continuation-p y) (give-up))
  (let* ((y (continuation-value y))
	 (y-abs (abs y))
	 (len (1- (integer-length y-abs))))
    (unless (and (plusp y-abs)
		 (= y-abs (ash 1 len)))
      (give-up))
    (let* ((shift (- len))
	   (mask (1- y-abs)))
      `(if (minusp x)
	   (values ,(if (minusp y)
			`(ash (- x) ,shift)
			`(- (ash (- x) ,shift)))
		   (- (logand (- x) ,mask)))
	   (values ,(if (minusp y)
			`(ash (- ,mask x) ,shift)
			`(ash x ,shift))
		   (logand x ,mask))))))

;;; And the same for rem.
;;;
(deftransform rem ((x y) (integer integer) * :when :both)
  "convert remainder mod 2^k to LOGAND"
  (unless (constant-continuation-p y) (give-up))
  (let* ((y (continuation-value y))
	 (y-abs (abs y))
	 (len (1- (integer-length y-abs))))
    (unless (and (plusp y-abs)
		 (= y-abs (ash 1 len)))
      (give-up))
    (let ((mask (1- y-abs)))
      `(if (minusp x)
	   (- (logand (- x) ,mask))
	   (logand x ,mask)))))


;;;; Arithmetic and logical identity operation elimination:
;;;
;;; Flush calls to random arith functions that convert to the identity
;;; function or a constant.


(dolist (stuff '((ash 0 x)
		 (logand -1 x)
		 (logand 0 0)
		 (logior 0 x)
		 (logior -1 -1)
		 (logxor -1 (lognot x))
		 (logxor 0 x)))
  (destructuring-bind (name identity result) stuff
    (deftransform name ((x y) `(* (constant-argument (member ,identity))) '*
			:eval-name t :when :both)
      "fold identity operations"
      result)))

(deftransform logand ((x y) (* (constant-argument t)) *)
  "fold identity operation"
  (let ((y (continuation-value y)))
    (unless (and (plusp y)
                 (= y (1- (ash 1 (integer-length y)))))
      (give-up))
    (unless (csubtypep (continuation-type x)
                       (specifier-type `(integer 0 ,y)))
      (give-up))
    'x))



;;; Restricted to rationals, because (- 0 0.0) is 0.0, not -0.0.
;;;
(deftransform - ((x y) ((constant-argument (member 0)) rational) *
		 :when :both)
  "convert (- 0 x) to negate"
  '(%negate y))

;;; Restricted to rationals, because (* 0 -4.0) is -0.0.
;;;
(deftransform * ((x y) (rational (constant-argument (member 0))) *
		 :when :both)
  "convert (* x 0) to 0."
  0)

;;; Fold (+ x 0).
;;;
;;; Restricted to rationals, because (+ -0.0 0) is 0.0, not -0.0.
;;;
(deftransform + ((x y) (rational (constant-argument (member 0))) *
		 :when :both)
  "fold zero arg"
  'x)


;;; Not-More-Contagious  --  Interface
;;;
;;;    Return T if in an arithmetic OP including continuations X and
;;; Y, the result type is not affected by the type of X. The main
;;; checks performed here are that the type of X does not cause a
;;; change in the float format of the result, or change the result to
;;; a complex float. It is assumed that the caller considers the
;;; affect of X on Value of the result. Thus with rational
;;; canonicalisation, X is permitted to be a rational or complex
;;; rational even if Y is only an integer or complex integer assuming
;;; that the result will be canonacilisted to the correct type.
;;;
(defun not-more-contagious (x y)
  (declare (type continuation x y))
  (let ((type1 (continuation-type x))
	(type2 (continuation-type y)))
    (if (and (numeric-type-p type1) (numeric-type-p type2))
	(let ((class1 (numeric-type-class type1))
	      (class2 (numeric-type-class type2))
	      (format1 (numeric-type-format type1))
	      (format2 (numeric-type-format type2))
	      (complexp1 (numeric-type-complexp type1))
	      (complexp2 (numeric-type-complexp type2)))
	  (cond ((or (null complexp1) (null class1)) Nil)
		((member class1 '(integer rational)) 'T)
		((and (eq class1 'float) (null complexp2)) Nil)
		((and (eq class1 'float) (null class2)) Nil)
		((and (eq class1 'float) (eq class2 'float))
		 (and (ecase complexp2
			(:real (eq complexp1 :real))
			(:complex 'T))
		      (ecase format2
			((nil short-float single-float)
			 (member format1 '(short-float single-float)))
			#-long-float
			((double-float long-float) 'T)
			#+(or long-float double-double)
			(double-float
			 (member format1 '(short-float single-float
					   double-float)))
			#+long-float
			(long-float 'T)
			#+double-double
			(double-double-float 't))))
		((and (eq class1 'float) (member class2 '(integer rational)))
		 Nil)
		(t
		 (error "Unexpected types: ~s ~s~%" type1 type2)))))))

;;; Fold (- x 0).
;;;
;;;    If y is not constant, not zerop, or is contagious, or a negative
;;; float -0.0 then give up because (- -0.0 -0.0) is 0.0, not -0.0.
;;;
(deftransform - ((x y) (t (constant-argument number)) * :when :both)
  "fold zero arg"
  (let ((val (continuation-value y)))
    (unless (and (zerop val)
		 (not (and (floatp val) (minusp (float-sign val))))
		 (not-more-contagious y x))
      (give-up)))
  'x)

;;; Fold (OP x +/-1)
;;;
(dolist (stuff '((* x (%negate x))
		 (/ x (%negate x))
		 (expt x (/ 1 x))))
  (destructuring-bind (name result minus-result) stuff
    (deftransform name ((x y) '(t (constant-argument real)) '* :eval-name t
			:when :both)
      "fold identity operations"
      (let ((val (continuation-value y)))
	(unless (and (= (abs val) 1)
		     (not-more-contagious y x))
	  (give-up))
	(if (minusp val) minus-result result)))))

;;; Fold (expt x n) into multiplications for small integral values of
;;; N; convert (expt x 1/2) to sqrt.
;;;
(deftransform expt ((x y) (t (constant-argument real)) *)
  "recode as multiplication or sqrt"
  (let ((val (continuation-value y)))
    ;; If Y would cause the result to be promoted to the same type as
    ;; Y, we give up.  If not, then the result will be the same type
    ;; as X, so we can replace the exponentiation with simple
    ;; multiplication and division for small integral powers.
    (unless (not-more-contagious y x)
      (give-up))
    (cond ((zerop val)
	   ;; Watch out.  We need to return a 1 of the same type as
	   ;; X. So floats should return a floating point 1, complex
	   ;; numbers with float components should return a complex 1
	   ;; with the desired floating-point type.  For all other
	   ;; cases, a simple 1 is the right answer.
	   '(cond ((floatp x)
		   (float 1 x))
	          ((and (complexp x) (floatp (realpart x)))
		   (complex (float 1 (realpart x))))
	          (t
		   1)))
	  ((= val 2) '(* x x))
	  ((= val -2) '(/ (* x x)))
	  ((= val 3) '(* x x x))
	  ((= val -3) '(/ (* x x x)))
	  ((= val 1/2) '(sqrt x))
	  ((= val -1/2) '(/ (sqrt x)))
	  (t (give-up)))))

(dolist (name '(ash /))
  (deftransform name ((x y) '((constant-argument (integer 0 0)) integer) '*
		      :eval-name t :when :both)
    "fold zero arg"
    0))

(dolist (name '(truncate round floor ceiling))
  (deftransform name ((x y) '((constant-argument (integer 0 0)) integer) '*
		      :eval-name t :when :both)
    "fold zero arg"
    '(values 0 0)))

    

;;;; Character operations:

(deftransform char-equal ((a b) (base-char base-char))
  "open code"
  '(let* ((ac (char-code a))
	  (bc (char-code b))
	  (sum (logxor ac bc)))
     (or (zerop sum)
	 (when (eql sum #x20)
	   (let ((sum (+ ac bc)))
	     (and (> sum 161) (< sum 213)))))))

(deftransform char-upcase ((x) (base-char))
  "open code"
  '(let ((n-code (char-code x)))
     (if (and (> n-code #o140)	; Octal 141 is #\a.
	      (< n-code #o173))	; Octal 172 is #\z.
	 (code-char (logxor #x20 n-code))
	 x)))

(deftransform char-downcase ((x) (base-char))
  "open code"
  '(let ((n-code (char-code x)))
     (if (and (> n-code 64)	; 65 is #\A.
	      (< n-code 91))	; 90 is #\Z.
	 (code-char (logxor #x20 n-code))
	 x)))


;;;; Equality predicate transforms:


;;; SAME-LEAF-REF-P  --  Internal
;;;
;;;    Return true if X and Y are continuations whose only use is a reference
;;; to the same leaf, and the value of the leaf cannot change.
;;;
(defun same-leaf-ref-p (x y)
  (declare (type continuation x y))
  (let ((x-use (continuation-use x))
	(y-use (continuation-use y)))
    (and (ref-p x-use)
	 (ref-p y-use)
	 (eq (ref-leaf x-use) (ref-leaf y-use))
	 (constant-reference-p x-use))))

;;; SIMPLE-EQUALITY-TRANSFORM  --  Internal
;;;
;;;    If X and Y are the same leaf, then the result is true.  Otherwise, if
;;; there is no intersection between the types of the arguments, then the
;;; result is definitely false.
;;;
(deftransform simple-equality-transform ((x y) * * :defun-only t
					 :when :both)
  (cond ((same-leaf-ref-p x y)
	 't)
	((not (types-intersect (continuation-type x) (continuation-type y)))
	 'nil)
	(t
	 (give-up))))

(dolist (x '(eq char= equal))
  (%deftransform x '(function * *) #'simple-equality-transform))


;;; EQL IR1 Transform  --  Internal
;;;
;;;    Similar to SIMPLE-EQUALITY-PREDICATE, except that we also try to convert
;;; to a type-specific predicate or EQ:
;;; -- If both args are characters, convert to CHAR=.  This is better than just
;;;    converting to EQ, since CHAR= may have special compilation strategies
;;;    for non-standard representations, etc.
;;; -- If either arg is definitely not a number, then we can compare with EQ.
;;; -- Otherwise, we try to put the arg we know more about second.  If X is
;;;    constant then we put it second.  If X is a subtype of Y, we put it
;;;    second.  These rules make it easier for the back end to match these
;;;    interesting cases.
;;; -- If Y is a fixnum, then we quietly pass because the back end can handle
;;;    that case, otherwise give an efficency note.
;;;
(deftransform eql ((x y) * * :when :both)
  "convert to simpler equality predicate"
  (let ((x-type (continuation-type x))
	(y-type (continuation-type y))
	(char-type (specifier-type 'character))
	(number-type (specifier-type 'number)))
    (cond ((same-leaf-ref-p x y)
	   't)
	  ((not (types-intersect x-type y-type))
	   'nil)
	  ((and (csubtypep x-type char-type)
		(csubtypep y-type char-type))
	   '(char= x y))
	  ((or (not (types-intersect x-type number-type))
	       (not (types-intersect y-type number-type)))
	   '(eq x y))
	  ((and (not (constant-continuation-p y))
		(or (constant-continuation-p x)
		    (and (csubtypep x-type y-type)
			 (not (csubtypep y-type x-type)))))
	   '(eql y x))
	  (t
	   (give-up)))))


;;; = IR1 Transform  --  Internal
;;;
;;;    Convert to EQL if both args are rational and complexp is specified
;;; and the same for both.
;;; 
(deftransform = ((x y) * * :when :both)
  "open code"
  (let ((x-type (continuation-type x))
	(y-type (continuation-type y)))
    (if (and (csubtypep x-type (specifier-type 'number))
	     (csubtypep y-type (specifier-type 'number)))
	(cond ((or (and (csubtypep x-type (specifier-type 'float))
			(csubtypep y-type (specifier-type 'float)))
		   (and (csubtypep x-type (specifier-type '(complex float)))
			(csubtypep y-type (specifier-type '(complex float)))))
	       ;; They are both floats.  Leave as = so that -0.0 is
	       ;; handled correctly.
	       (give-up))
	      ((or (and (csubtypep x-type (specifier-type 'rational))
			(csubtypep y-type (specifier-type 'rational)))
		   (and (csubtypep x-type (specifier-type '(complex rational)))
			(csubtypep y-type (specifier-type '(complex rational)))))
	       ;; They are both rationals and complexp is the same.  Convert
	       ;; to EQL.
	       '(eql x y))
	      (t
	       (give-up "Operands might not be the same type.")))
	(give-up "Operands might not be the same type."))))


;;; Numeric-Type-Or-Lose  --  Interface
;;;
;;;    If Cont's type is a numeric type, then return the type, otherwise
;;; GIVE-UP.
;;;
(defun numeric-type-or-lose (cont)
  (declare (type continuation cont))
  (let ((res (continuation-type cont)))
    (unless (numeric-type-p res) (give-up))
    res))


;;; Ir1-transform-<-helper  --  Internal
;;;
;;; Derive the result type of the comparision X < Y returning two values: the
;;; first true if X < Y, and the second true if X >= Y. Union types are
;;; handled by comparing all types of X with all types of Y.  If all types of
;;; X are less than all types of Y, then X < Y. Similarly, if all types of X
;;; are >= all types of Y, then X >= Y.
;;;
(defun ir1-transform-<-helper (x y)
  (flet ((maybe-convert (type)
	   (numeric-type->interval
	    (cond ((numeric-type-p type) type)
		  ((member-type-p type) (convert-member-type type))
		  (t (give-up))))))
    (let ((xi (mapcar #'maybe-convert
		      (prepare-arg-for-derive-type (continuation-type x))))
	  (yi (mapcar #'maybe-convert
		      (prepare-arg-for-derive-type (continuation-type y))))
	  (definitely-true t)
	  (definitely-false t))
      (dolist (x-arg xi)
	(dolist (y-arg yi)
	  (setf definitely-true (and definitely-true
				     (interval-< x-arg y-arg)))
	  (setf definitely-false (and definitely-false
				      (interval->= x-arg y-arg)))))
      (values definitely-true definitely-false))))

;;; IR1-TRANSFORM-<  --  Internal
;;;
;;;    See if we can statically determine (< X Y) using type information.  If
;;; X's high bound is < Y's low, then X < Y.  Similarly, if X's low is >= to
;;; Y's high, then X >= Y (so return NIL).  If not, at least make sure any
;;; constant arg is second.
;;;
;;; Note that "inverse" isn't really the inverse---it's the function
;;; that would return the same answer if the args were reversed.  So 2
;;; < 3 gives the same answer as 3 > 2.
(defun ir1-transform-< (x y first second inverse)
  (if (same-leaf-ref-p x y)
      'nil
      (multiple-value-bind (definitely-true definitely-false)
	  (ir1-transform-<-helper x y)
	(cond (definitely-true
	       t)
	      (definitely-false
	       nil)
              ((and (constant-continuation-p first)
                    (not (constant-continuation-p second)))
               `(,inverse y x))
              (t
               (give-up))))))

(deftransform < ((x y) (real real) * :when :both)
  (ir1-transform-< x y x y '>))

(deftransform > ((x y) (real real) * :when :both)
  (ir1-transform-< y x x y '<))

;; Like IR1-TRANSFORM-< but for CHAR<.  This is needed so that the
;; vops for base-char comparison with a constant gets used when the
;; first arg is the constant.
(defun ir1-transform-char< (x y first second inverse)
  (if (same-leaf-ref-p x y)
      'nil
      (cond ((and (constant-continuation-p first)
		  (not (constant-continuation-p second)))
	     `(,inverse y x))
	    (t
	     (give-up)))))

(deftransform char< ((x y) (base-char base-char) * :when :both)
  (ir1-transform-char< x y x y 'char>))

(deftransform char> ((x y) (base-char base-char) * :when :both)
  (ir1-transform-char< y x x y 'char<))


;;;; Converting N-arg comparisons:
;;;
;;;    We convert calls to N-arg comparison functions such as < into two-arg
;;; calls.  This transformation is enabled for all such comparisons in this
;;; file.  If any of these predicates are not open-coded, then the
;;; transformation should be removed at some point to avoid pessimization.

;;; Multi-Compare  --  Internal
;;;
;;;    This function is used for source transformation of N-arg comparison
;;; functions other than inequality.  We deal both with converting to two-arg
;;; calls and inverting the sense of the test, if necessary.  If the call has
;;; two args, then we pass or return a negated test as appropriate.  If it is a
;;; degenerate one-arg call, then we transform to code that returns true.
;;; Otherwise, we bind all the arguments and expand into a bunch of IFs.
;;;
(defun multi-compare (predicate args not-p)
  (declare (symbol predicate) (list args) (type boolean not-p))
  (let ((nargs (length args)))
    (cond ((< nargs 1) (values nil t))
	  ((= nargs 1) `(progn ,@args t))
	  ((= nargs 2)
	   (if not-p
	       `(if (,predicate ,(first args) ,(second args)) nil t)
	       (values nil t)))
	  (t
	   (do* ((i (1- nargs) (1- i))
		 (last nil current)
		 (current (gensym) (gensym))
		 (vars (list current) (cons current vars))
		 (result 't (if not-p
				`(if (,predicate ,current ,last)
				     nil ,result)
				`(if (,predicate ,current ,last)
				     ,result nil))))
	       ((zerop i)
		`((lambda ,vars ,result) . ,args)))))))


(def-source-transform = (&rest args) (multi-compare '= args nil))
(def-source-transform < (&rest args) (multi-compare '< args nil))
(def-source-transform > (&rest args) (multi-compare '> args nil))
(def-source-transform <= (&rest args) (multi-compare '> args t))
(def-source-transform >= (&rest args) (multi-compare '< args t))

(def-source-transform char= (&rest args) (multi-compare 'char= args nil))
(def-source-transform char< (&rest args) (multi-compare 'char< args nil))
(def-source-transform char> (&rest args) (multi-compare 'char> args nil))
(def-source-transform char<= (&rest args) (multi-compare 'char> args t))
(def-source-transform char>= (&rest args) (multi-compare 'char< args t))

(def-source-transform char-equal (&rest args) (multi-compare 'char-equal args nil))
(def-source-transform char-lessp (&rest args) (multi-compare 'char-lessp args nil))
(def-source-transform char-greaterp (&rest args) (multi-compare 'char-greaterp args nil))
(def-source-transform char-not-greaterp (&rest args) (multi-compare 'char-greaterp args t))
(def-source-transform char-not-lessp (&rest args) (multi-compare 'char-lessp args t))


;;; Multi-Not-Equal  --  Internal
;;;
;;;    This function does source transformation of N-arg inequality functions
;;; such as /=.  This is similar to Multi-Compare in the <3 arg cases.  If
;;; there are more than two args, then we expand into the appropriate n^2
;;; comparisons only when speed is important.
;;;
(defun multi-not-equal (predicate args)
  (declare (symbol predicate) (list args))
  (let ((nargs (length args)))
    (cond ((< nargs 1) (values nil t))
	  ((= nargs 1) `(progn ,@args t))
	  ((= nargs 2)
	   `(if (,predicate ,(first args) ,(second args)) nil t))
	  ((not (policy nil (>= speed space) (>= speed cspeed)))
	   (values nil t))
	  (t
	   (collect ((vars))
	     (dotimes (i nargs) (vars (gensym)))
	     (do ((var (vars) next)
		  (next (cdr (vars)) (cdr next))
		  (result 't))
		 ((null next)
		  `((lambda ,(vars) ,result) . ,args))
	       (let ((v1 (first var)))
		 (dolist (v2 next)
		   (setq result `(if (,predicate ,v1 ,v2) nil ,result))))))))))

(def-source-transform /= (&rest args) (multi-not-equal '= args))
(def-source-transform char/= (&rest args) (multi-not-equal 'char= args))
(def-source-transform char-not-equal (&rest args) (multi-not-equal 'char-equal args))



#-sparc-v9
(progn
;;; Expand Max and Min into the obvious comparisons.
(def-source-transform max (arg &rest more-args)
  (if (null more-args)
      `(values (the real ,arg))
      (once-only ((arg1 arg)
		  (arg2 `(max ,@more-args)))
	`(if (>= ,arg1 ,arg2)
	     ,arg1 ,arg2))))
;;;
(def-source-transform min (arg &rest more-args)
  (if (null more-args)
      `(values (the real ,arg))
      (once-only ((arg1 arg)
		  (arg2 `(min ,@more-args)))
	`(if (<= ,arg1 ,arg2)
	     ,arg1 ,arg2))))

)


;;;; Converting N-arg arithmetic functions:
;;;
;;;    N-arg arithmetic and logic functions are associated into two-arg
;;; versions, and degenerate cases are flushed.

;;; Associate-Arguments  --  Internal
;;;
;;;    Left-associate First-Arg and More-Args using Function.
;;;
(defun associate-arguments (function first-arg more-args)
  (declare (symbol function) (list more-args) (values list))
  (let ((next (rest more-args))
	(arg (first more-args)))
    (if (null next)
	`(,function ,first-arg ,arg)
	(associate-arguments function `(,function ,first-arg ,arg) next))))

;;; Source-Transform-Transitive  --  Internal
;;;
;;;    Do source transformations for transitive functions such as +.  One-arg
;;; cases are replaced with the arg and zero arg cases with the identity.
;;;
;;; However for the one-arg case, we assert the type of the arg to
;;; catch type errors instead of silently returning the single arg.
;;; Result-type tells us the expected type.
;;;
(defun source-transform-transitive (fun args identity &optional (result-type 'integer))
  (declare (symbol fun) (list args))
  (case (length args)
    (0 identity)
    (1 `(values (the ,result-type ,(first args))))
    (2 (values nil t))
    (t
     (associate-arguments fun (first args) (rest args)))))

(def-source-transform + (&rest args) (source-transform-transitive '+ args 0 'number))
(def-source-transform * (&rest args) (source-transform-transitive '* args 1 'number))
(def-source-transform logior (&rest args) (source-transform-transitive 'logior args 0))
(def-source-transform logxor (&rest args) (source-transform-transitive 'logxor args 0))
(def-source-transform logand (&rest args) (source-transform-transitive 'logand args -1))

(def-source-transform logeqv (&rest args)
  (if (evenp (length args))
      `(lognot (logxor ,@args))
      `(logxor ,@args)))

;;; Note: we can't use source-transform-transitive for GCD and LCM because when
;;; they are given one argument, they return it's absolute value.

(def-source-transform gcd (&rest args)
  (case (length args)
    (0 0)
    (1 `(abs (the (values integer &rest t) ,(first args))))
    (2 (values nil t))
    (t (associate-arguments 'gcd (first args) (rest args)))))

(def-source-transform lcm (&rest args)
  (case (length args)
    (0 1)
    (1 `(abs (the (values integer &rest t) ,(first args))))
    (2 (values nil t))
    (t (associate-arguments 'lcm (first args) (rest args)))))


;;; Source-Transform-Intransitive  --  Internal
;;;
;;;    Do source transformations for intransitive n-arg functions such as /.
;;; With one arg, we form the inverse.  With two args we pass.  Otherwise we
;;; associate into two-arg calls.
;;;
(defun source-transform-intransitive (function args inverse)
  (declare (symbol function) (list args) (values list &optional t))
  (case (length args)
    ((0 2) (values nil t))
    (1 `(,@inverse ,(first args)))
    (t
     (associate-arguments function (first args) (rest args)))))

(def-source-transform - (&rest args)
  (source-transform-intransitive '- args '(%negate)))
(def-source-transform / (&rest args)
  (source-transform-intransitive '/ args '(/ 1)))


;;;; Apply:
;;;
;;;    We convert Apply into Multiple-Value-Call so that the compiler only
;;; needs to understand one kind of variable-argument call.  It is more
;;; efficient to convert Apply to MV-Call than MV-Call to Apply.

(def-source-transform apply (fun arg &rest more-args)
  (let ((args (cons arg more-args)))
    `(multiple-value-call ,fun
       ,@(mapcar #'(lambda (x)
		     `(values ,x))
		 (butlast args))
       (values-list ,(car (last args))))))


;;;; FORMAT transform:
;;;
;;; If the control string is a compile-time constant, then replace it with
;;; a use of the FORMATTER macro so that the control string is ``compiled.''
;;; Furthermore, if the destination is either a stream or T and the control
;;; string is a function (i.e. formatter), then convert the call to format to
;;; just a funcall of that function.
;;;

(defun check-format-args-2 (min-args max-args args context)
  (when min-args
    (let ((nargs (length args)))
      (cond ((stringp min-args)
	     (compiler-warning "~a" min-args))
	    ((< nargs min-args)
	     (compiler-warning "~s: too few args (~d), need at least ~d"
			       context nargs min-args))
	    ((> nargs max-args)
	     (compiler-note "~s: too many args (~d), wants at most ~d"
			    context nargs max-args))))))

(defun check-format-args-1 (string args context)
  (multiple-value-bind (min-args max-args)
      (format::min/max-format-arguments-count string)
    (check-format-args-2 min-args max-args args context)))

(defun check-format-args (string-cont args context)
  (when (constant-continuation-p string-cont)
    (check-format-args-1 (continuation-value string-cont) args context)))

;;;
;;; Note that two DEFTRANSFORMs differing in their :POLICY only cannot
;;; be defined; one replaces the other.
;;;
(deftransform format ((dest control &rest args) (t simple-string &rest t) *)
  (cond ((policy nil (> speed space))
	 (unless (constant-continuation-p control)
	   (give-up "Control string is not a constant."))
	 (let ((string (continuation-value control)))
	   (check-format-args-1 string args 'format)
	   (let ((arg-names (loop repeat (length args) collect (gensym))))
	     `(lambda (dest control ,@arg-names)
		(declare (ignore control))
		(format dest (formatter ,string) ,@arg-names)))))
	(t
	 (check-format-args control args 'format)
	 (give-up))))

(deftransform format ((stream control &rest args) (stream function &rest t) *
		      :policy (> speed space))
  (let ((arg-names (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) args)))
    `(lambda (stream control ,@arg-names)
       (funcall control stream ,@arg-names)
       nil)))

(deftransform format ((tee control &rest args) ((member t) function &rest t) *
		      :policy (> speed space))
  (let ((arg-names (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) args)))
    `(lambda (tee control ,@arg-names)
       (declare (ignore tee))
       (funcall control *standard-output* ,@arg-names)
       nil)))

(macrolet ((define-format-checker (name)
	     `(deftransform ,name ((format &rest args)
				   (simple-string &rest t) *)
		(check-format-args format args ',name)
		(give-up))))
  (define-format-checker error)
  (define-format-checker warn)
  (define-format-checker compiler-error)
  (define-format-checker compiler-warning)
  (define-format-checker compiler-note)
  (define-format-checker compiler-mumble))

;;; Modular functions

;;; (ldb (byte s 0) (foo                 x  y ...)) =
;;; (ldb (byte s 0) (foo (ldb (byte s 0) x) y ...))
;;;
;;; and similar for other arguments.

;;; Try to recursively cut all uses of LVAR to WIDTH bits.
;;;
;;; For good functions, we just recursively cut arguments; their
;;; "goodness" means that the result will not increase (in the
;;; (unsigned-byte +infinity) sense). An ordinary modular function is
;;; replaced with the version, cutting its result to WIDTH or more
;;; bits. For most functions (e.g. for +) we cut all arguments; for
;;; others (e.g. for ASH) we have "optimizers", cutting only necessary
;;; arguments (maybe to a different width) and returning the name of a
;;; modular version, if it exists, or NIL. If we have changed
;;; anything, we need to flush old derived types, because they have
;;; nothing in common with the new code.

(defun cut-to-width (cont width)
  (declare (type continuation cont) (type (integer 0) width))
  (labels ((reoptimize-node (node name)
             (setf (node-derived-type node)
                   (function-type-returns (info function type name)))
             (setf (continuation-%derived-type (node-cont node)) nil)
             (setf (node-reoptimize node) t)
             (setf (block-reoptimize (node-block node)) t)
             (setf (component-reoptimize (block-component (node-block node)))
		   t))
           (cut-node (node &aux did-something)
             (when (and (not (block-delete-p (node-block node)))
                        (combination-p node)
                        (function-info-p (basic-combination-kind node)))
               (let* ((fun-ref (continuation-use (combination-fun node)))
		      (fun-name (leaf-name (ref-leaf fun-ref)))
                      (modular-fun (find-modular-version fun-name width))
                      #+nil
		      (name (and (modular-fun-info-p modular-fun)
                                 (modular-fun-info-name modular-fun))))
                 (when (and modular-fun
                            (not (and (eq fun-name 'logand)
                                      (csubtypep
                                       (single-value-type (node-derived-type node))
				       ;; This is (unsigned-byte
				       ;; width), but if width is 0,
				       ;; (unsigned-byte 0) isn't
				       ;; valid.
                                       (specifier-type `(integer 0 ,(1- (ash 1 width))))))))
		   (let ((name (etypecase modular-fun
				 ((eql :good) fun-name)
				 (modular-fun-info
				  (modular-fun-info-name modular-fun))
				 (function
				  (funcall modular-fun node width)))))
		     #+debug-mod32
		     (progn
		       (format t "leaf-name   = ~S~%" (leaf-name (ref-leaf fun-ref)))
		       (format t "fun-name    = ~S~%" fun-name)
		       (format t "modular-fun = ~A~%" modular-fun)
		       (format t " type = ~A~%" (single-value-type (node-derived-type node)))
		       (format t "name        = ~A~%" name))
		     (when name
		       (unless (eq modular-fun :good)
			 (setq did-something t)
			 #+debug-mod32
			 (format t "  changing leaf ~S ~%  to ~S~%" fun-ref
				 (find-free-function name "in a strange place"))
			 (change-ref-leaf
			  fun-ref
			  (find-free-function name "in a strange place"))
			 (setf (combination-kind node) :full))
		       (unless (functionp modular-fun)
			 (dolist (arg (basic-combination-args node))
			   (when (cut-cont arg)
			     (setq did-something t))))
		       (when did-something
			 (reoptimize-node node fun-name))
		       did-something))))))
           (cut-cont (cont &aux did-something)
             (do-uses (node cont)
               (when (cut-node node)
                 (setq did-something t)))
             did-something))
    (cut-cont cont)))

#+nil
(defun cut-to-width (lvar width)
  (declare (type lvar lvar) (type (integer 0) width))
  (labels ((reoptimize-node (node name)
             (setf (node-derived-type node)
                   (fun-type-returns
                    (info :function :type name)))
             (setf (lvar-%derived-type (node-lvar node)) nil)
             (setf (node-reoptimize node) t)
             (setf (block-reoptimize (node-block node)) t)
             (setf (component-reoptimize (node-component node)) t))
           (cut-node (node &aux did-something)
             (when (and (not (block-delete-p (node-block node)))
                        (combination-p node)
                        (fun-info-p (basic-combination-kind node)))
               (let* ((fun-ref (lvar-use (combination-fun node)))
                      (fun-name (leaf-source-name (ref-leaf fun-ref)))
                      (modular-fun (find-modular-version fun-name width)))
                 (when (and modular-fun
                            (not (and (eq fun-name 'logand)
                                      (csubtypep
                                       (single-value-type (node-derived-type node))
                                       (specifier-type `(unsigned-byte* ,width))))))
                   (binding* ((name (etypecase modular-fun
                                      ((eql :good) fun-name)
                                      (modular-fun-info
                                       (modular-fun-info-name modular-fun))
                                      (function
                                       (funcall modular-fun node width)))
                                :exit-if-null))
                     (unless (eql modular-fun :good)
                       (setq did-something t)
                       (change-ref-leaf
                        fun-ref
                        (find-free-fun name "in a strange place"))
                       (setf (combination-kind node) :full))
                     (unless (functionp modular-fun)
                       (dolist (arg (basic-combination-args node))
                         (when (cut-lvar arg)
                           (setq did-something t))))
                     (when did-something
                       (reoptimize-node node name))
                     did-something)))))
           (cut-lvar (lvar &aux did-something)
             (do-uses (node lvar)
               (when (cut-node node)
                 (setq did-something t)))
             did-something))
    (cut-lvar lvar)))


(defun logand-defopt-helper (x y node)
  (let ((result-type (single-value-type (node-derived-type node))))
    (when (numeric-type-p result-type)
      (let ((low (numeric-type-low result-type))
            (high (numeric-type-high result-type)))
        (when (and (numberp low)
                   (numberp high)
                   (>= low 0))
          (let ((width (integer-length high)))
            (when (some (lambda (x) (<= width x))
                        *modular-funs-widths*)
              ;; FIXME: This should be (CUT-TO-WIDTH NODE WIDTH).
              (cut-to-width x width)
              (cut-to-width y width)
              nil		 ; After fixing above, replace with T.
              )))))))

(defvar *enable-modular-arithmetic* t
  "When non-NIL, the compiler will generate code utilizing modular
  arithmetic.  Set to NIL to disable this, if you don't want modular
  arithmetic in some cases.")

#+modular-arith
(defoptimizer (logand optimizer) ((x y) node)
  (when *enable-modular-arithmetic*
    (logand-defopt-helper x y node)))
