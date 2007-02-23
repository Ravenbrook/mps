;;; -*- Mode:LISP; Package:PCL; Base:10; Syntax:Common-Lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/dfun.lisp,v 1.38 2005/08/18 16:55:00 rtoy Exp $")

(in-package :pcl)

#|

This implementation of method lookup was redone in early August of 89.

It has the following properties:

 - Its modularity makes it easy to modify the actual caching algorithm.
   The caching algorithm is almost completely separated into the files
   cache.lisp and dlap.lisp.  This file just contains the various uses
   of it. There will be more tuning as we get more results from Luis'
   measurements of caching behavior.

 - The metacircularity issues have been dealt with properly.  All of
   PCL now grounds out properly.  Moreover, it is now possible to have
   metaobject classes which are themselves not instances of standard
   metaobject classes.

** Modularity of the code **

The actual caching algorithm is isolated in a modest number of functions.
The code which generates cache lookup code is all found in cache.lisp and
dlap.lisp.  Certain non-wrapper-caching special cases are in this file.


** Handling the metacircularity **

In CLOS, method lookup is the potential source of infinite metacircular
regress.  The metaobject protocol specification gives us wide flexibility
in how to address this problem.  PCL uses a technique which handles the
problem not only for the metacircular language described in Chapter 3, but
also for the PCL protocol which includes additional generic functions
which control more aspects of the CLOS implementation.

The source of the metacircular regress can be seen in a number of ways.
One is that the specified method lookup protocol must, as part of doing
the method lookup (or at least the cache miss case), itself call generic
functions.  It is easy to see that if the method lookup for a generic
function ends up calling that same generic function there can be trouble.

Fortunately, there is an easy solution at hand.  The solution is based on 
the restriction that portable code cannot change the class of a specified
metaobject.  This restriction implies that for specified generic functions,
the method lookup protocol they follow is fixed.  

More precisely, for such specified generic functions, most generic functions
that are called during their own method lookup will not run portable methods. 
This allows the implementation to usurp the actual generic function call in
this case.  In short, method lookup of a standard generic function, in the
case where the only applicable methods are themselves standard doesn't
have to do any method lookup to implement itself.

And so, we are saved.

|#


;;;
;;; An alist in which each entry is of the form (<generator>
;;; . (<subentry> ...))  Each subentry is of the form: (<args>
;;; <constructor> <system>)
;;;
(defvar *dfun-constructors* ())			

;;;
;;; If this is NIL, then the whole mechanism for caching dfun
;;; constructors is turned off.  The only time that makes sense is
;;; when debugging LAP code.
;;;
(defvar *enable-dfun-constructor-caching* t)

(defun get-dfun-constructor (generator &rest args)
  (when (member generator '(emit-checking emit-caching
			    emit-in-checking-cache-p
			    emit-constant-value))
    (loop for type in (car args)
	  if (eq type t)
	    collect type into types
	  else
	    collect 'class into types
	  finally
	    (setf (car args) types)))
  (let* ((generator-entry (assq generator *dfun-constructors*))
	 (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (cond ((null *enable-dfun-constructor-caching*)
	   (apply (symbol-function generator) args))
	  ((cadr args-entry)
	   (cadr args-entry))
	  (t
	   (multiple-value-bind (new not-best-p)
	       (apply (symbol-function generator) args)
	     (let ((entry (list (copy-list args)
				 new
				 (unless not-best-p 'pcl)
				 not-best-p)))
	       (if generator-entry
		   (push entry (cdr generator-entry))
		   (push (list generator entry) *dfun-constructors*)))
	     (values new not-best-p))))))

(defun load-precompiled-dfun-constructor (generator args system constructor)
  (let* ((generator-entry (assq generator *dfun-constructors*))
	 (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (if args-entry
	(when (fourth args-entry)
	  (let* ((dfun-type (case generator
			      (emit-checking       'checking)
			      (emit-caching        'caching)
			      (emit-constant-value 'constant-value)
			      (emit-default-only   'default-method-only)))
		 (metatypes (car args))
		 (gfs (when dfun-type (gfs-of-type dfun-type))))
	    (dolist (gf gfs)
	      (when (and (equal metatypes
				(arg-info-metatypes (gf-arg-info gf)))
			 (let ((gf-name (generic-function-name gf)))
			   (and (not (eq gf-name 'slot-value-using-class))
				(not (equal gf-name
					    '(setf slot-value-using-class)))
				(not (eq gf-name 'slot-boundp-using-class)))))
		(update-dfun gf)))
	    (setf (second args-entry) constructor)
	    (setf (third args-entry)  system)
	    (setf (fourth args-entry) nil)))
	(let ((entry (list args constructor system nil)))
	  (if generator-entry
	      (push entry (cdr generator-entry))
	      (push (list generator entry) *dfun-constructors*))))))

(defmacro precompile-dfun-constructors (&optional system)
  (let ((*precompiling-lap* t))
    `(progn
       ,@(let ((collected ()))
	   (dolist (generator-entry *dfun-constructors*
		    (nreverse collected))
	     (dolist (args-entry (cdr generator-entry))
	       (when (or (null (caddr args-entry))
			 (eq (caddr args-entry) system))
		 (when system
		   (setf (caddr args-entry) system))
		 (push
		   (make-top-level-form `(precompile-dfun-constructor 
					  ,(car generator-entry))
					'(:load-toplevel)
		     `(load-precompiled-dfun-constructor
		       ',(car generator-entry)
		       ',(car args-entry)
		       ',system
		       ,(apply (symbol-function (car generator-entry))
			       (car args-entry))))
		   collected))))))))


;;; **********************************
;;; Standard Class Slot Access *******
;;; **********************************
;;;
;;; When trying to break vicious metacircles, we need a way to get at
;;; the values of slots of some standard classes without going through
;;; the whole meta machinery, because that would likely enter the
;;; vicious circle again.  That's what the following is for.
;;;

(defvar *standard-classes*
  '(standard-method standard-generic-function standard-class
    standard-effective-slot-definition))

(defvar *standard-slot-locations* (make-hash-table :test 'equal))

(defun compute-standard-slot-locations ()
  (clrhash *standard-slot-locations*)
  (dolist (class-name *standard-classes*)
    (let ((class (find-class class-name)))
      (dolist (slot (class-slots class))
	(setf (gethash (cons class (slot-definition-name slot))
		       *standard-slot-locations*)
	      (slot-definition-location slot))))))
	      
(defun maybe-update-standard-class-locations (class)
  (when (and (eq *boot-state* 'complete)
	     (memq (class-name class) *standard-classes*))
    (compute-standard-slot-locations)))

(defun standard-slot-value (object slot-name class)
  (let ((location (gethash (cons class slot-name) *standard-slot-locations*)))
    (if location
	(let ((value (if (funcallable-instance-p object)
			 (funcallable-standard-instance-access object location)
			 (standard-instance-access object location))))
	  (when (eq +slot-unbound+ value)
	    (error "~@<Slot ~s of class ~s is unbound in object ~s~@:>"
		   slot-name class object))
	  value)
	(error "~@<Cannot get standard value of slot ~s of class ~s ~
                in object ~s~@:>"
	       slot-name class object))))

(defun standard-slot-value/gf (gf slot-name)
  (standard-slot-value gf slot-name *the-class-standard-generic-function*))

(defun standard-slot-value/method (method slot-name)
  (standard-slot-value method slot-name *the-class-standard-method*))

(defun standard-slot-value/eslotd (slotd slot-name)
  (standard-slot-value slotd slot-name
		       *the-class-standard-effective-slot-definition*))

(defun standard-slot-value/class (class slot-name)
  (standard-slot-value class slot-name *the-class-standard-class*))


;;;
;;; When all the methods of a generic function are automatically generated
;;; reader or writer methods a number of special optimizations are possible.
;;; These are important because of the large number of generic functions of
;;; this type.
;;;
;;; There are a number of cases:
;;;
;;;   ONE-CLASS-ACCESSOR
;;;     In this case, the accessor generic function has only been called
;;;     with one class of argument.  There is no cache vector, the wrapper
;;;     of the one class, and the slot index are stored directly as closure
;;;     variables of the discriminating function.  This case can convert to
;;;     either of the next kind.
;;;
;;;   TWO-CLASS-ACCESSOR
;;;     Like above, but two classes.  This is common enough to do specially.
;;;     There is no cache vector.  The two classes are stored a separate
;;;     closure variables.
;;;
;;;   ONE-INDEX-ACCESSOR
;;;     In this case, the accessor generic function has seen more than one
;;;     class of argument, but the index of the slot is the same for all
;;;     the classes that have been seen.  A cache vector is used to store
;;;     the wrappers that have been seen, the slot index is stored directly
;;;     as a closure variable of the discriminating function.  This case
;;;     can convert to the next kind.
;;;
;;;   N-N-ACCESSOR
;;;     This is the most general case.  In this case, the accessor generic
;;;     function has seen more than one class of argument and more than one
;;;     slot index.  A cache vector stores the wrappers and corresponding
;;;     slot indexes.  Because each cache line is more than one element
;;;     long, a cache lock count is used.
;;;
(defstruct (dfun-info
	     (:constructor nil)
	     (:print-function print-dfun-info))
  (cache nil))

(defun print-dfun-info (dfun-info stream depth)
  (declare (ignore depth) (stream stream))
  (print-unreadable-object (dfun-info stream :identity t)
    (format stream "~A" (type-of dfun-info))))

(defstruct (no-methods
	     (:constructor make-no-methods-dfun-info ())
	     (:include dfun-info)))

(defstruct (initial
	     (:constructor make-initial-dfun-info ())
	     (:include dfun-info)))

(defstruct (initial-dispatch
	     (:constructor make-initial-dispatch-dfun-info ())
	     (:include dfun-info)))

(defstruct (dispatch
	     (:constructor make-dispatch-dfun-info ())
	     (:include dfun-info)))

(defstruct (default-method-only
	     (:constructor make-default-method-only-dfun-info ())
	     (:include dfun-info)))

;without caching:
;  dispatch one-class two-class default-method-only

;with caching:
;  one-index n-n checking caching

;accessor:
;  one-class two-class one-index n-n

(defstruct (accessor-dfun-info
	     (:constructor nil)
	     (:conc-name dfun-info-)
	     (:include dfun-info))
  accessor-type) ; (member reader writer)

(defstruct (one-index-dfun-info
	     (:constructor nil)
	     (:conc-name dfun-info-)
	     (:include accessor-dfun-info))
  index)

(defstruct (n-n
	     (:constructor make-n-n-dfun-info (accessor-type cache))
	     (:include accessor-dfun-info)))

(defstruct (one-class
	     (:constructor make-one-class-dfun-info
			   (accessor-type index wrapper0))
	     (:conc-name dfun-info-)
	     (:include one-index-dfun-info))
  wrapper0)

(defstruct (two-class
	     (:constructor make-two-class-dfun-info
			   (accessor-type index wrapper0 wrapper1))
	     (:conc-name dfun-info-)
	     (:include one-class))
  wrapper1)

(defstruct (one-index
	     (:constructor make-one-index-dfun-info
			   (accessor-type index cache))
	     (:include one-index-dfun-info)))	     

(defstruct (checking
	     (:constructor make-checking-dfun-info (function cache))
	     (:conc-name dfun-info-)
	     (:include dfun-info))
  function)

(defstruct (caching
	     (:constructor make-caching-dfun-info (cache))
	     (:include dfun-info)))

(defstruct (constant-value
	     (:constructor make-constant-value-dfun-info (cache))
	     (:include dfun-info)))

(defmacro dfun-update (generic-function function &rest args)
  `(multiple-value-bind (dfun cache info)
       (funcall ,function ,generic-function ,@args)
     (update-dfun ,generic-function dfun cache info)))

(defun accessor-miss-function (gf dfun-info)
  (ecase (dfun-info-accessor-type dfun-info)
    ((reader boundp)
     (lambda (arg)
       (accessor-miss gf nil arg dfun-info)))
    (writer
     (lambda (new arg)
       (accessor-miss gf new arg dfun-info)))))

(declaim (freeze-type dfun-info))


;;;
;;; ONE-CLASS-ACCESSOR
;;;
(defun make-one-class-accessor-dfun (gf type wrapper index)
  (let ((emit (ecase type
		(reader 'emit-one-class-reader)
		(boundp 'emit-one-class-boundp)
		(writer 'emit-one-class-writer)))
	(dfun-info (make-one-class-dfun-info type index wrapper)))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      wrapper index
	      (accessor-miss-function gf dfun-info))
     nil
     dfun-info)))

;;;
;;; TWO-CLASS-ACCESSOR
;;;
(defun make-two-class-accessor-dfun (gf type w0 w1 index)
  (let ((emit (ecase type
		(reader 'emit-two-class-reader)
		(boundp 'emit-two-class-boundp)
		(writer 'emit-two-class-writer)))
	(dfun-info (make-two-class-dfun-info type index w0 w1)))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      w0 w1 index
	      (accessor-miss-function gf dfun-info))
     nil
     dfun-info)))

;;;
;;; std accessors same index dfun
;;;
(defun make-one-index-accessor-dfun (gf type index &optional cache)
  (let* ((emit (ecase type
		 (reader 'emit-one-index-readers)
		 (boundp 'emit-one-index-boundps)
		 (writer 'emit-one-index-writers)))
	 (cache (or cache (get-cache 1 nil 4)))
	 (dfun-info (make-one-index-dfun-info type index cache)))
    (declare (type cache cache))
    (values
     (funcall (get-dfun-constructor emit (consp index))
	      cache
	      index
	      (accessor-miss-function gf dfun-info))
     cache
     dfun-info)))

(defun make-final-one-index-accessor-dfun (gf type index table)
  (let ((cache (fill-dfun-cache table nil 1)))
    (make-one-index-accessor-dfun gf type index cache)))

(defun make-n-n-accessor-dfun (gf type &optional cache)
  (let* ((emit (ecase type
		 (reader 'emit-n-n-readers)
		 (boundp 'emit-n-n-boundps)
		 (writer 'emit-n-n-writers)))
	 (cache (or cache (get-cache 1 t 2)))
	 (dfun-info (make-n-n-dfun-info type cache)))
    (declare (type cache cache))
    (values
     (funcall (get-dfun-constructor emit)
	      cache
	      (accessor-miss-function gf dfun-info))
     cache
     dfun-info)))

(defun make-final-n-n-accessor-dfun (gf type table)
  (let ((cache (fill-dfun-cache table t 1)))
    (make-n-n-accessor-dfun gf type cache)))

(defun make-checking-dfun (generic-function function &optional cache)
  (unless cache
    (when (use-caching-dfun-p generic-function)
      (return-from make-checking-dfun (make-caching-dfun generic-function)))
    (when (use-dispatch-dfun-p generic-function)
      (return-from make-checking-dfun (make-dispatch-dfun generic-function))))
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info generic-function)
    (declare (ignore nreq))
    (if (every (lambda (mt) (eq mt t)) metatypes)
	(let ((dfun-info (make-default-method-only-dfun-info)))
	  (values 
	   (funcall (get-dfun-constructor 'emit-default-only metatypes applyp)
		    function)
	   nil
	   dfun-info))
	(let* ((cache (or cache (get-cache nkeys nil 2)))
	       (dfun-info (make-checking-dfun-info function cache)))
	  (values
	   (funcall (get-dfun-constructor 'emit-checking metatypes applyp)
		    cache
		    function 
		    (lambda (&rest args)
		      (checking-miss generic-function args dfun-info)))
	   cache
	   dfun-info)))))

(defun make-final-checking-dfun (generic-function function
						  classes-list new-class)
  (let ((metatypes (arg-info-metatypes (gf-arg-info generic-function))))
    (if (every (lambda (mt) (eq mt t)) metatypes)
	(values (lambda (&rest args)
		  (invoke-emf function args))
		nil
		(make-default-method-only-dfun-info))
	(let ((cache (make-final-ordinary-dfun-internal 
		      generic-function nil
		      classes-list new-class)))
	  (make-checking-dfun generic-function function cache)))))

(defun use-caching-dfun-p (gf)
  (some (lambda (method)
	  (let ((fmf (if (listp method)
			 (third method)
			 (method-fast-function method))))
	    (method-function-get fmf :slot-name-lists)))
	(generic-function-methods* gf)))


;;;
;;;
;;;
(defun make-caching-dfun (generic-function &optional cache)
  (unless cache
    (when (use-constant-value-dfun-p generic-function)
      (return-from make-caching-dfun (make-constant-value-dfun generic-function)))
    (when (use-dispatch-dfun-p generic-function)
      (return-from make-caching-dfun (make-dispatch-dfun generic-function))))
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info generic-function)
    (declare (ignore nreq))
    (let* ((cache (or cache (get-cache nkeys t 2)))
	   (dfun-info (make-caching-dfun-info cache)))
      (values
       (funcall (get-dfun-constructor 'emit-caching metatypes applyp)
		cache
		(lambda (&rest args)
		  (caching-miss generic-function args dfun-info)))
       cache
       dfun-info))))

(defun make-final-caching-dfun (generic-function classes-list new-class)
  (let ((cache (make-final-ordinary-dfun-internal 
		generic-function t classes-list new-class)))
    (make-caching-dfun generic-function cache)))

(defun insure-caching-dfun (gf)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info gf)
    (declare (ignore nreq nkeys))
    (when (and metatypes
	       (not (null (car metatypes)))
	       (dolist (mt metatypes nil)
		 (unless (eq mt t) (return t))))
      (get-dfun-constructor 'emit-caching metatypes applyp))))

(defun use-constant-value-dfun-p (gf &optional boolean-values-p)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info gf)
    (declare (ignore nreq metatypes nkeys))
    (let ((methods (generic-function-methods* gf)))
      (and (null applyp)
	   (or (not (eq *boot-state* 'complete))
	       ;;
	       ;; If COMPUTE-APPLICABLE-METHODS is specialized, we
	       ;; can't use this, of course, because we can't tell
	       ;; which methods will be considered applicable.
	       ;;
	       ;; Also, don't use this method if the generic function
	       ;; has a non-standard method combination, because if it
	       ;; has, it's not sure that method functions are used
	       ;; directly as effective methods, which
	       ;; CONSTANT-VALUE-MISS depends on.  The pre-defined
	       ;; method combinations like LIST are examples of that.
	       (and (compute-applicable-methods-emf-std-p gf)
		    (eq (generic-function-method-combination gf)
			*standard-method-combination*)))
	   ;;
	   ;; Check that no method is eql-specialized, and that all
	   ;; methods return a constant value.  If BOOLEAN-VALUES-P,
	   ;; check that all return T or NIL.  Also, check that no
	   ;; method has specializers, to make sure that emfs are
	   ;; really method functions; see above.
	   (dolist (method methods t)
	     (when (eq *boot-state* 'complete)
	       (when (or (some #'eql-specializer-p
			       (method-specializers method))
			 (method-qualifiers method))
		 (return nil)))
	     (let* ((mf (if (consp method)
			    (or (third method) (second method))
			    (or (method-fast-function method)
				(method-function method))))
		    (constant (method-function-get mf :constant-value mf)))
	       (when (or (eq constant mf)
			 (and boolean-values-p
			      (not (member constant '(t nil)))))
		 (return nil))))))))

(defun make-constant-value-dfun (generic-function &optional cache)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-function-info generic-function)
    (declare (ignore nreq applyp))
    (let* ((cache (or cache (get-cache nkeys t 2)))
	   (dfun-info (make-constant-value-dfun-info cache)))
      (values
       (funcall (get-dfun-constructor 'emit-constant-value metatypes)
		cache
		(lambda (&rest args)
		  (constant-value-miss generic-function args dfun-info)))
       cache
       dfun-info))))

(defun make-final-constant-value-dfun (generic-function classes-list new-class)
  (let ((cache (make-final-ordinary-dfun-internal 
		generic-function :constant-value
		classes-list new-class)))
    (make-constant-value-dfun generic-function cache)))

(defun use-dispatch-dfun-p (gf &optional (caching-p (use-caching-dfun-p gf)))
  (when (eq *boot-state* 'complete)
    (unless (or caching-p (emfs-must-check-applicable-keywords-p gf))
      ;; This should return T when almost all dispatching is by
      ;; eql specializers or built-in classes.  In other words,
      ;; return NIL if we might ever need to do more than
      ;; one (non built-in) typep.
      ;; Otherwise, it is probably at least as fast to use
      ;; a caching dfun first, possibly followed by secondary dispatching.

      #||;;; Original found in cmu 17f -- S L O W 
      (< (dispatch-dfun-cost gf) (caching-dfun-cost gf))
      ||#
      ;; This uses improved dispatch-dfun-cost below
      (let ((cdc  (caching-dfun-cost gf))) ; fast
	(> cdc (dispatch-dfun-cost gf cdc))))))

;; Try this on print-object, find-method-combination, and documentation.
;; Look at pcl/generic-functions.lisp for other potential test cases.
(defun show-dfun-costs (gf)
  (when (or (symbolp gf) (consp gf))
    (setq gf (gdefinition gf)))
  (format t "~&Name ~S  caching cost ~D  dispatch cost ~D~%"
	  (generic-function-name gf)
	  (caching-dfun-cost gf)
	  (dispatch-dfun-cost gf)))

(defparameter *non-built-in-typep-cost* 1)
(defparameter *structure-typep-cost* 1)
(defparameter *built-in-typep-cost* 0)

;; This version is from the pcl found in the gcl-2.1 distribution.
;; Someone added a cost limit so as to keep the execution time controlled
(defun dispatch-dfun-cost (gf &optional limit)
  (generate-discrimination-net-internal 
   gf (generic-function-methods gf) nil
   (lambda (methods known-types)
     (declare (ignore methods known-types))
     0)
   (lambda (position type true-value false-value)
     (declare (ignore position))
     (let* ((type-test-cost
	     (if (eq 'class (car type))
		 (let* ((metaclass (class-of (cadr type)))
			(mcpl (class-precedence-list metaclass)))
		   (cond ((memq *the-class-built-in-class* mcpl)
			  *built-in-typep-cost*)
			 ((memq *the-class-structure-class* mcpl)
			  *structure-typep-cost*)
			 (t
			  *non-built-in-typep-cost*)))
		 0))
	    (max-cost-so-far
	     (+ (max true-value false-value) type-test-cost)))
       (when (and limit (<= limit max-cost-so-far))
	 (return-from dispatch-dfun-cost max-cost-so-far))
       max-cost-so-far))
   #'identity))


(defparameter *cache-lookup-cost* 1)
(defparameter *wrapper-of-cost* 0)
(defparameter *secondary-dfun-call-cost* 1)

(defun caching-dfun-cost (gf)
  (let* ((arg-info (gf-arg-info gf))
         (nreq (length (arg-info-metatypes arg-info))))
    (+ *cache-lookup-cost*
       (* *wrapper-of-cost* nreq)
       (if (methods-contain-eql-specializer-p 
	    (generic-function-methods gf))
	   *secondary-dfun-call-cost*
	   0))))

(progn
  (setq *non-built-in-typep-cost* 100)
  (setq *structure-typep-cost* 15)
  (setq *built-in-typep-cost* 5)
  (setq *cache-lookup-cost* 30)
  (setq *wrapper-of-cost* 15)
  (setq *secondary-dfun-call-cost* 30))
  

(defun make-dispatch-dfun (gf)
  (values (get-dispatch-function gf) nil (make-dispatch-dfun-info)))

(defun get-dispatch-function (gf)
  (let* ((methods (generic-function-methods gf))
	 (generator (get-secondary-dispatch-function1
		     gf methods nil nil nil nil nil t)))
    (make-callable gf methods generator nil nil)))

(defun make-final-dispatch-dfun (gf)
  (make-dispatch-dfun gf))

(defun update-dispatch-dfuns ()
  (dolist (gf (gfs-of-type '(dispatch initial-dispatch)))
    (dfun-update gf #'make-dispatch-dfun)))

(defun fill-dfun-cache (table valuep nkeys &optional cache)
  (let ((cache (or cache (get-cache nkeys valuep
				    (+ (hash-table-count table) 3)))))
    (maphash (lambda (classes value)
	       (setq cache (fill-cache cache
				       (class-wrapper classes)
				       value)))
	     table)
    cache))

(defun make-final-ordinary-dfun-internal (generic-function valuep
					  classes-list new-class)
  (let* ((arg-info (gf-arg-info generic-function))
	 (nkeys (arg-info-nkeys arg-info))
	 (new-class (and new-class
			 (equal (type-of (gf-dfun-info generic-function))
				(cond ((eq valuep t)
				       'caching)
				      ((eq valuep :constant-value)
				       'constant-value)
				      ((null valuep)
				       'checking)))
			 new-class))
	 (cache (if new-class
		    (copy-cache (gf-dfun-cache generic-function))
		    (get-cache nkeys (not (null valuep)) 4))))
      (make-emf-cache generic-function valuep cache classes-list new-class)))

(defvar *dfun-miss-gfs-on-stack* ())

(defmacro dfun-miss ((gf args wrappers invalidp nemf
		      &optional type index caching-p applicable)
		     &body body)
  (unless applicable
    (setq applicable (gensym)))
  `(multiple-value-bind (,nemf ,applicable ,wrappers ,invalidp 
			 ,@(when type `(,type ,index)))
       (cache-miss-values ,gf ,args ',(cond (caching-p 'caching)
					    (type 'accessor)
					    (t 'checking)))
     (when (and ,applicable (not (memq ,gf *dfun-miss-gfs-on-stack*)))
       (let ((*dfun-miss-gfs-on-stack* (cons ,gf *dfun-miss-gfs-on-stack*)))
	 ,@body))
     ;;
     ;; Create a FAST-INSTANCE-BOUNDP structure instance for a cached
     ;; SLOT-BOUNDP so that INVOKE-EMF does the right thing, that is,
     ;; does not signal a SLOT-UNBOUND error for a boundp test.
     ,@(if type
	   `((if (and (eq ,type 'boundp) (integerp ,nemf))
		 (invoke-emf (make-fast-instance-boundp :index ,nemf) ,args)
		 (invoke-emf ,nemf ,args)))
	   `((invoke-emf ,nemf ,args)))))

;;;
;;; The dynamically adaptive method lookup algorithm is implemented is
;;; implemented as a kind of state machine.  The kinds of discriminating
;;; function is the state, the various kinds of reasons for a cache miss
;;; are the state transitions.
;;;
;;; The code which implements the transitions is all in the miss handlers
;;; for each kind of dfun.  Those appear here.
;;;
;;; Note that within the states that cache, there are dfun updates which
;;; simply select a new cache or cache field.  Those are not considered
;;; as state transitions.
;;; 
(defvar *early-p* nil)

(defvar *max-emf-precomputation-methods* 100
  "Precompute effective methods at method load time if the generic
   function has less than this number of methods.  If zero,
   no effective methods are precomputed at method load time.")

;;;
;;; Try to finalize all unfinalized class specializers of all methods
;;; of generic function GF.  Value is true if successful.
;;;
(defun finalize-specializers (gf)
  (let ((methods (generic-function-methods gf)))
    (when (< (length methods) *max-emf-precomputation-methods*)
      (let ((all-finalized t))
	(dolist (method (generic-function-methods gf) all-finalized)
	  (dolist (specializer (method-specializers method))
	    (when (and (classp specializer)
		       (not (class-finalized-p specializer)))
	      (if (class-has-a-forward-referenced-superclass-p specializer)
		  (setq all-finalized nil)
		  (finalize-inheritance specializer)))))))))
      
(defun make-initial-dfun (gf)
  (let ((initial-dfun 
	 #'(kernel:instance-lambda (&rest args)
	     (initial-dfun gf args))))
    (multiple-value-bind (dfun cache info)
	(cond ((and (eq *boot-state* 'complete)
		    (not (finalize-specializers gf)))
	       (values initial-dfun nil (make-initial-dfun-info)))

	      ((and (eq *boot-state* 'complete)
		    (compute-applicable-methods-emf-std-p gf))
	       (let* ((caching-p (use-caching-dfun-p gf)))
		 ;;
		 ;; The call to PRECOMPUTE-EFFECTIVE-METHODS should
		 ;; not be removed, because effective method
		 ;; computation will detect invalid methods (invalid
		 ;; qualifiers) early, which is desirable.
		 (precompute-effective-methods gf caching-p)
		 (cond ((use-dispatch-dfun-p gf caching-p)
			(values initial-dfun nil (make-initial-dispatch-dfun-info)))
		       (caching-p
			(insure-caching-dfun gf)
			(values initial-dfun nil (make-initial-dfun-info)))
		       (t
			(values initial-dfun nil (make-initial-dfun-info))))))
	      (t
	       (let ((arg-info (gf-arg-info* gf))
		     (type nil))
		 (if (and (gf-precompute-dfun-and-emf-p arg-info)
			  (setq type (final-accessor-dfun-type gf)))
		     (if *early-p*
			 (values (make-early-accessor gf type) nil nil)
			 (make-final-accessor-dfun gf type))
		     (values initial-dfun nil (make-initial-dfun-info))))))
      (set-dfun gf dfun cache info))))

(defun make-early-accessor (gf type)
  (let* ((methods (early-gf-methods gf))
	 (slot-name (early-method-standard-accessor-slot-name (car methods))))
    (ecase type
      (reader #'(kernel:instance-lambda (instance)
		  (let* ((class (class-of instance))
			 (class-name (bootstrap-get-slot 'class class 'name)))
		    (bootstrap-get-slot class-name instance slot-name))))
      (boundp #'(kernel:instance-lambda (instance)
		  (let* ((class (class-of instance))
			 (class-name (bootstrap-get-slot 'class class 'name)))
		    (not (eq +slot-unbound+
			     (bootstrap-get-slot class-name instance
						 slot-name))))))
      (writer #'(kernel:instance-lambda (new-value instance)
		  (let* ((class (class-of instance))
			 (class-name (bootstrap-get-slot 'class class 'name)))
		    (bootstrap-set-slot class-name instance slot-name new-value)))))))

(defun initial-dfun (gf args)
  (dfun-miss (gf args wrappers invalidp nemf ntype nindex)
    (cond (invalidp)
	  ((and ntype nindex)
	   (dfun-update gf #'make-one-class-accessor-dfun
			ntype wrappers nindex))
	  ((use-caching-dfun-p gf)
	   (dfun-update gf #'make-caching-dfun))
	  (t
	   (dfun-update gf #'make-checking-dfun
			(cache-miss-values gf args 'checking))))))

(defun make-final-dfun (gf &optional classes-list)
  (multiple-value-bind (dfun cache info)
      (make-final-dfun-internal gf classes-list)
    (set-dfun gf dfun cache info)))

(defun update-dfun (gf &optional dfun cache info)
  (let* ((early-p (early-gf-p gf))
	 (gf-name (generic-function-name* gf)))
    (set-dfun gf dfun cache info)
    (let ((dfun (if early-p
		    (or dfun (make-initial-dfun gf))
		    (compute-discriminating-function gf))))
      (set-funcallable-instance-function gf dfun)
      (set-function-name gf gf-name)
      (update-pv-calls-for-gf gf)
      dfun)))

(defun gfs-of-type (type)
  (unless (consp type) (setq type (list type)))
  (let ((gf-list nil))
    (map-all-generic-functions (lambda (gf)
				 (when (memq (type-of (gf-dfun-info gf)) type)
				   (push gf gf-list))))
    gf-list))

(defvar *new-class* nil)

(defvar *free-hash-tables* (mapcar #'list '(eq equal eql)))

(defmacro with-hash-table ((table test) &body forms)
  `(let* ((.free. (assoc ',test *free-hash-tables*))
	  (,table (if (cdr .free.)
		      (pop (cdr .free.))
		      (make-hash-table :test ',test))))
     (multiple-value-prog1
	 (progn ,@forms)
       (clrhash ,table)
       (push ,table (cdr .free.)))))

(defmacro with-eq-hash-table ((table) &body forms)
  `(with-hash-table (,table eq) ,@forms))

(defun final-accessor-dfun-type (gf)
  (let ((methods (generic-function-methods* gf)))
    (cond ((every (lambda (method) 
		    (if (consp method)
			(eq *the-class-standard-reader-method*
			    (early-method-class method))
			(standard-reader-method-p method)))
		  methods)
	   'reader)
	  ((every (lambda (method)
		    (if (consp method)
			(eq *the-class-standard-boundp-method*
			    (early-method-class method))
			(standard-boundp-method-p method)))
		  methods)
	   'boundp)
	  ((every (lambda (method) 
		    (if (consp method)
			(eq *the-class-standard-writer-method*
			    (early-method-class method))
			(standard-writer-method-p method)))
		  methods)
	   'writer))))

(defun make-final-accessor-dfun (gf type &optional classes-list new-class)
  (with-eq-hash-table (table)
    (multiple-value-bind (table all-index first second size no-class-slots-p)
	(make-accessor-table gf type table)
      (if table
	  (cond ((= size 1)
		 (let ((w (class-wrapper first)))
		   (make-one-class-accessor-dfun gf type w all-index)))
		((and (= size 2) (or (integerp all-index) (consp all-index)))
		 (let ((w0 (class-wrapper first))
		       (w1 (class-wrapper second)))
		   (make-two-class-accessor-dfun gf type w0 w1 all-index)))
		((or (integerp all-index) (consp all-index))
		 (make-final-one-index-accessor-dfun 
		  gf type all-index table))
		(no-class-slots-p
		 (make-final-n-n-accessor-dfun gf type table))
		(t
		 (make-final-caching-dfun gf classes-list new-class)))
	  (make-final-caching-dfun gf classes-list new-class)))))

(defun make-final-dfun-internal (gf &optional classes-list)
  (let ((methods (generic-function-methods gf)) type
	(new-class *new-class*) (*new-class* nil)
	specls all-same-p)
    (cond ((null methods)
	   (values
	    #'(kernel:instance-lambda (&rest args)
		(apply #'no-applicable-method gf args))
	    nil
	    (make-no-methods-dfun-info)))
	  ((setq type (final-accessor-dfun-type gf))
	   (make-final-accessor-dfun gf type classes-list new-class))
	  ((and (not (and (every (lambda (specl) (eq specl *the-class-t*))
				 (setq specls (method-specializers (car methods))))
			  (setq all-same-p
				(every (lambda (method)
					 (and (equal specls
						     (method-specializers method))))
				       methods))))
		(use-constant-value-dfun-p gf))
	   (make-final-constant-value-dfun gf classes-list new-class))
	  ((use-dispatch-dfun-p gf)
	   (make-final-dispatch-dfun gf))
	  ((and all-same-p (not (use-caching-dfun-p gf)))
	   (let ((emf (get-secondary-dispatch-function gf methods nil)))
	     (make-final-checking-dfun gf emf classes-list new-class)))
	  (t
	   (make-final-caching-dfun gf classes-list new-class)))))

(defun accessor-miss (gf new object dfun-info)
  (let* ((ostate (type-of dfun-info))
	 (otype (dfun-info-accessor-type dfun-info))
	 oindex ow0 ow1 cache
	 (args (ecase otype
		((reader boundp) (list object))
		(writer (list new object)))))
    (dfun-miss (gf args wrappers invalidp nemf ntype nindex)
      ;;
      ;; The following lexical functions change the state of the
      ;; dfun to that which is their name.  They accept arguments
      ;; which are the parameters of the new state, and get other
      ;; information from the lexical variables bound above.
      ;; 
      (flet ((two-class (index w0 w1)
	       (when (zerop (random 2)) (psetf w0 w1 w1 w0))
	       (dfun-update gf #'make-two-class-accessor-dfun
			    ntype w0 w1 index))
	     (one-index (index &optional cache)
	       (dfun-update gf #'make-one-index-accessor-dfun
			    ntype index cache))
	     (n-n (&optional cache)
	       (if (consp nindex)
		   (dfun-update gf #'make-checking-dfun nemf)
		   (dfun-update gf #'make-n-n-accessor-dfun ntype cache)))
	     (caching () ; because cached accessor emfs are much faster for accessors
	       (dfun-update gf #'make-caching-dfun))
	     ;;
	     (do-fill (update-fn)
	       (let ((ncache (fill-cache cache wrappers nindex)))
		 (unless (eq ncache cache)
		   (funcall update-fn ncache)))))
	(cond ((null ntype)
	       (caching))
	      ((or invalidp
		   (null nindex)))
	      ((not (pcl-instance-p object))
	       (caching))
	      ((or (neq ntype otype) (listp wrappers))
	       (caching))
	      (t
	       (ecase ostate
		 (one-class
		  (setq oindex (dfun-info-index dfun-info))
		  (setq ow0 (dfun-info-wrapper0 dfun-info))
		  (unless (eq ow0 wrappers)
		    (if (eql nindex oindex)
			(two-class nindex ow0 wrappers)
			(n-n))))
		 (two-class
		  (setq oindex (dfun-info-index dfun-info))
		  (setq ow0 (dfun-info-wrapper0 dfun-info))
		  (setq ow1 (dfun-info-wrapper1 dfun-info))
		  (unless (or (eq ow0 wrappers) (eq ow1 wrappers))
		    (if (eql nindex oindex)
			(one-index nindex)
			(n-n))))
		 (one-index
		  (setq oindex (dfun-info-index dfun-info))
		  (setq cache (dfun-info-cache dfun-info))
		  (if (eql nindex oindex)
		      (do-fill (lambda (ncache)
				 (one-index nindex ncache)))
		      (n-n)))
		 (n-n
		  (setq cache (dfun-info-cache dfun-info))
		  (if (consp nindex)
		      (caching)
		      (do-fill #'n-n))))))))))

(defun checking-miss (generic-function args dfun-info)
  (let ((oemf (dfun-info-function dfun-info))
	(cache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp nemf)
      (cond (invalidp)
	    ((eq oemf nemf)
	     (let ((ncache (fill-cache cache wrappers nil)))
	       (unless (eq ncache cache)
		 (dfun-update generic-function #'make-checking-dfun 
			      nemf ncache))))
	    (t
	     (dfun-update generic-function #'make-caching-dfun))))))

(defun caching-miss (generic-function args dfun-info)
  (let ((ocache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp emf nil nil t)
      (cond (invalidp)
	    (t
	     (let ((ncache (fill-cache ocache wrappers emf)))
	       (unless (eq ncache ocache)
		 (dfun-update generic-function 
			      #'make-caching-dfun ncache))))))))

(defun constant-value-miss (gf args dfun-info)
  (let ((ocache (dfun-info-cache dfun-info)))
    (dfun-miss (gf args wrappers invalidp emf nil nil t)
      (unless invalidp
	(let* ((mf (typecase emf
		     (fast-method-call (fast-method-call-function emf))
		     (method-call (method-call-function emf))))
	       (value (let ((val (method-function-get mf :constant-value
						      '.not-found.)))
			;;
			;; This way of retrieving the constant value
			;; assumes that the emf is actually a method
			;; function that's used directly as effective
			;; method function.  Let's check that is
			;; indeed the case, because it sometimes
			;; wasn't.
			(assert (neq val '.not-found.))
			val))
	       (ncache (fill-cache ocache wrappers value)))
	  (unless (eq ncache ocache)
	    (dfun-update gf #'make-constant-value-dfun ncache)))))))

;;; Given a generic function and a set of arguments to that generic function,
;;; returns a mess of values.
;;;
;;;  <function>   The compiled effective method function for this set of
;;;               arguments. 
;;;
;;;  <applicable> Sorted list of applicable methods. 
;;;
;;;  <wrappers>   Is a single wrapper if the generic function has only
;;;               one key, that is arg-info-nkeys of the arg-info is 1.
;;;               Otherwise a list of the wrappers of the specialized
;;;               arguments to the generic function.
;;;
;;;               Note that all these wrappers are valid.  This function
;;;               does invalid wrapper traps when it finds an invalid
;;;               wrapper and then returns the new, valid wrapper.
;;;
;;;  <invalidp>   True if any of the specialized arguments had an invalid
;;;               wrapper, false otherwise.
;;;
;;;  <type>       READER or WRITER when the only method that would be run
;;;               is a standard reader or writer method.  To be specific,
;;;               the value is READER when the method combination is eq to
;;;               *standard-method-combination*; there are no applicable
;;;               :before, :after or :around methods; and the most specific
;;;               primary method is a standard reader method.
;;;
;;;  <index>      If <type> is READER or WRITER, and the slot accessed is
;;;               an :instance slot, this is the index number of that slot
;;;               in the object argument.
;;;
;;; Only used in this file.
;;;
(defun cache-miss-values (gf args state)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-function-info gf)
    (declare (ignore nreq applyp nkeys))
    (with-dfun-wrappers (args metatypes)
      (dfun-wrappers invalid-wrapper-p wrappers classes types)
      #+nil
      (error "~@<The function ~S requires at least ~D arguments.~@:>"
	     gf (length metatypes))
      (error 'kernel:simple-program-error
	     :name gf
	     :format-control "~<The function ~S requires at least ~D arguments.~@:>"
	     :format-arguments (list gf (length metatypes)))
      (multiple-value-bind (emf methods accessor-type index)
	  (cache-miss-values-internal gf arg-info wrappers classes types state)
	(values emf methods
		dfun-wrappers
		invalid-wrapper-p
		accessor-type index)))))

;;;
;;; Called from vector.lisp
;;;
(defvar *cmv-stack* ())

(defun cache-miss-values-internal (gf arg-info wrappers classes types state)
  (if (and (eq state 'accessor)
	   wrappers
	   (equal wrappers (cdr (assq gf *cmv-stack*))))
      (break-vicious-metacircle gf classes arg-info)
      (let ((*cmv-stack* (cons (cons gf wrappers) *cmv-stack*))
	    (cam-std-p (or (null arg-info)
			   (gf-info-c-a-m-emf-std-p arg-info))))
	(multiple-value-bind (methods all-applicable-and-sorted-p)
	    (if cam-std-p
		(compute-applicable-methods-using-types gf types)
		(compute-applicable-methods-using-classes gf classes))
	  (let* ((for-accessor-p (eq state 'accessor))
		 (for-cache-p (memq state '(caching accessor)))
		 (emf (if (or cam-std-p all-applicable-and-sorted-p)
			  (let ((generator (get-secondary-dispatch-function1
					    gf methods types nil
					    (and for-cache-p wrappers)
					    all-applicable-and-sorted-p)))
			    ;;
			    ;; MAKE-CALLABLE can call GET-METHOD-FUNCTION,
			    ;; which can do a PV-TABLE-LOOKUP, which
			    ;; can call CACHE-MISS-VALUES-INTERNAL again
			    ;; for the same emf we are computing.
			    (make-callable gf methods generator nil
					   (and for-cache-p wrappers)))
			  (default-secondary-dispatch-function gf))))
	    (multiple-value-bind (index accessor-type)
		(and for-accessor-p all-applicable-and-sorted-p methods
		     (accessor-values gf arg-info classes methods))
	      (values (if (integerp index) index emf)
		      methods accessor-type index)))))))

;;;
;;; Try to break a vicious circle while computing a cache miss.
;;; GF is the generic function, CLASSES are the classes of actual
;;; arguments, and ARG-INFO is the generic functions' arg-info.
;;;
;;; A vicious circle can be entered when the computation of the cache
;;; miss values itself depends on the values being computed.  For
;;; instance, adding a method which is an instance of a subclass of
;;; STANDARD-METHOD leads to cache misses for slot accessors of
;;; STANDARD-METHOD like METHOD-SPECIALIZERS, and METHOD-SPECIALIZERS
;;; is itself used while we compute cache miss values.
;;;
(defun break-vicious-metacircle (gf classes arg-info)
  (when (typep gf 'standard-generic-function)
    (multiple-value-bind (class slotd accessor-type)
	(accesses-standard-class-slot-p gf)
      (when class
	(let ((method (find-standard-class-accessor-method
		       gf class accessor-type))
	      (index (standard-slot-value/eslotd slotd 'location))
	      (type (gf-info-simple-accessor-type arg-info)))
	  (when (and method
		     (subtypep (if (eq accessor-type 'reader)
				   (car classes)
				   (cadr classes))
			       class))
	    (return-from break-vicious-metacircle
	      (values index (list method) type index)))))))
  (kernel:infinite-error-protect
   (error "~@<Vicious metacircle:  The computation of an ~
	   effective method of ~s for arguments of types ~s uses ~
	   the effective method being computed.~@:>"
	  gf classes)))

;;;
;;; Return (CLASS SLOTD ACCESSOR-TYPE) if some method of generic
;;; function GF accesses a slot of some class in *STANDARD-CLASSES*.
;;; CLASS is the class accessed, SLOTD is the effective slot definition
;;; object of the slot accessed, and ACCESSOR-TYPE is one of the symbols
;;; READER or WRITER describing the slot access.
;;;
(defun accesses-standard-class-slot-p (gf)
  (flet ((standard-class-slot-access (gf class)
	   (loop with gf-name = (standard-slot-value/gf gf 'name)
		 for slotd in (standard-slot-value/class class 'slots)
		 as readers = (standard-slot-value/eslotd slotd 'readers)
		 as writers = (standard-slot-value/eslotd slotd 'writers)
		 if (member gf-name readers :test #'equal)
		   return (values slotd 'reader)
		 else if (member gf-name writers :test #'equal)
		   return (values slotd 'writer))))
    (dolist (class-name *standard-classes*)
      (let ((class (find-class class-name)))
	(multiple-value-bind (slotd accessor-type)
	    (standard-class-slot-access gf class)
	  (when slotd
	    (return (values class slotd accessor-type))))))))

;;;
;;; Find a slot reader/writer method among the methods of generic
;;; function GF which reads/writes instances of class CLASS.
;;; TYPE is one of the symbols READER or WRITER.
;;;
(defun find-standard-class-accessor-method (gf class type)
  (dolist (method (standard-slot-value/gf gf 'methods))
    (let ((specializers (standard-slot-value/method method 'specializers))
	  (qualifiers (plist-value method 'qualifiers)))
      (when (and (null qualifiers)
		 (eq (ecase type
		       (reader (car specializers))
		       (writer (cadr specializers)))
		     class))
	(return method)))))

;;;
;;; Only used in this file.
;;;
(defun accessor-values (gf arg-info classes methods)
  (declare (ignore gf))
  (let* ((accessor-type (gf-info-simple-accessor-type arg-info))
	 (accessor-class (case accessor-type
			   ((reader boundp) (car classes))
			   (writer (cadr classes)))))
    (accessor-values-internal accessor-type accessor-class methods)))

;;;
;;; Called from vector.lisp
;;;
(defun accessor-values1 (gf accessor-type accessor-class)
  (let* ((type `(class-eq ,accessor-class))
	 (types (ecase accessor-type
		  (writer `(t ,type))
		  ((reader boundp) `(,type))))
	 (methods (compute-applicable-methods-using-types gf types)))
    (accessor-values-internal accessor-type accessor-class methods)))

;;;
;;; Only used in this file.
;;;
(declaim (inline cpl-maybe-early slot-location-maybe-early))

(defun cpl-maybe-early (class)
  (if (eq *boot-state* 'complete)
      (class-precedence-list class)
      (early-class-precedence-list class)))

;;;
;;; Return the class precedence list of CLASS or null if we can't
;;; tell, which is the case when the class isn't finalized yet.
;;;
(defun cpl-or-nil (class)
  (if (eq *boot-state* 'complete)
      (when (class-finalized-p class)
	(class-precedence-list class))
      (early-class-precedence-list class)))

(defun slot-location-maybe-early (slotd)
  (if (eq *boot-state* 'complete)
      (slot-definition-location slotd) 
      (early-slot-definition-location slotd)))

;;;
;;; Only used in this file.
;;;
(defun accessor-values-internal (accessor-type accessor-class methods)
  (when (some (lambda (method)
		(if (consp method) 
		    (early-method-qualifiers method)
		    (method-qualifiers method)))
	      methods)
    (return-from accessor-values-internal (values nil nil)))
  (let* ((meth (car methods))
	 (early-p (not (eq *boot-state* 'complete)))
	 (slot-name (when accessor-class
		      (if (consp meth)
			  (and (early-method-standard-accessor-p meth)
			       (early-method-standard-accessor-slot-name meth))
			  (and (member *the-class-standard-object*
				       (cpl-maybe-early accessor-class))
			       (if early-p
				   (not (eq *the-class-standard-method*
					    (early-method-class meth)))
				   (standard-accessor-method-p meth))
			       (if early-p
				   (early-accessor-method-slot-name meth)
				   (accessor-method-slot-name meth))))))
	 (slotd (and accessor-class
		     (if early-p
			 (dolist (slot (early-class-slotds accessor-class) nil)
			   (when (eql slot-name (early-slot-definition-name slot))
			     (return slot)))
			 (find-slot-definition accessor-class slot-name)))))
    (when (and slotd
	       (or early-p
		   (slot-accessor-std-p slotd accessor-type)))
      (values (slot-location-maybe-early slotd)
	      accessor-type))))

;;;
;;; Only used in this file.
;;;
(defun make-accessor-table (gf type table)
  (let ((table (or table (make-hash-table :test 'eq)))
	(methods (generic-function-methods* gf))
	(all-index nil)
	(no-class-slots-p t)
	(early-p (not (eq *boot-state* 'complete)))
	first second (size 0))
    (declare (fixnum size))
    (flet ((precedence (class)
	     (cpl-maybe-early class))
	   (slot-name (method)
	     (if (consp method)
		 (and (early-method-standard-accessor-p method)
		      (early-method-standard-accessor-slot-name method))
		 (accessor-method-slot-name method)))
	   (slot-location (slotd)
	     (slot-location-maybe-early slotd)))
      ;; class -> {(specl slotd)}
      (dolist (method methods)
	(let* ((specializers (if (consp method)
				 (early-method-specializers method t)
				 (method-specializers method)))
	       (specl (ecase type
			((reader boundp) (car specializers))
			(writer (cadr specializers))))
	       (specl-cpl (precedence specl))
	       (so-p (member *the-class-standard-object* specl-cpl))
	       (slot-name (slot-name method)))
	  (when (or (null specl-cpl)
		    (member *the-class-structure-object* specl-cpl))
	    (return-from make-accessor-table nil))
	  (maphash (lambda (class slotd)
		     (let ((cpl (precedence class)))
		       (when (memq specl cpl)
			 (unless (and (or so-p
					  (member *the-class-standard-object*
						  cpl))
				      (or early-p
					  (slot-accessor-std-p slotd type)))
			   (return-from make-accessor-table nil))
			 (push (cons specl slotd) (gethash class table)))))
		   (slot-name->class-table slot-name))))
      (maphash (lambda (class specl+slotd-list)
		 (dolist (sclass (precedence class)
			  (internal-error "This can't happen."))
		   (let ((a (assq sclass specl+slotd-list)))
		     (when a
		       (let* ((slotd (cdr a))
			      (index (slot-location slotd)))
			 (unless index
			   (return-from make-accessor-table nil))
			 (setf (gethash class table) index)
			 (when (consp index)
			   (setq no-class-slots-p nil))
			 (setq all-index (if (or (null all-index)
						 (eql all-index index))
					     index t))
			 (incf size)
			 (cond ((= size 1) (setq first class))
			       ((= size 2) (setq second class)))
			 (return nil))))))
	       table)
      (values table all-index first second size no-class-slots-p))))


;;;; ***********************************************
;;;; The guts of COMPUTE-APPLICABLE-METHODS  *******
;;;; ***********************************************

;;;
;;; First value is a sorted list of possibly applicable methods of
;;; generic function GF when applied to arguments of types TYPES.
;;; Second value is true if all methods are known to be applicable.
;;;
(defun compute-applicable-methods-using-types (gf types)
  (let ((definite-p t)
	(possibly-applicable-methods ()))
    (dolist (method (generic-function-methods* gf))
      (let ((specls (if (consp method)
			(early-method-specializers method t)
			(method-specializers method)))
	    (types types)
	    (possibly-applicable-p t)
	    (applicable-p t))
	(dolist (specl specls)
	  (multiple-value-bind (specl-applicable-p specl-possibly-applicable-p)
	      (specializer-applicable-using-type-p specl (pop types))
	    (unless specl-applicable-p
	      (setq applicable-p nil))
	    (unless specl-possibly-applicable-p
	      (setq possibly-applicable-p nil)
	      (return))))
	(when possibly-applicable-p
	  (unless applicable-p
	    (setq definite-p nil))
	  (push method possibly-applicable-methods))))
    (let ((precedence (arg-info-precedence (gf-arg-info* gf))))
      (values (sort-applicable-methods (nreverse possibly-applicable-methods)
				       types precedence)
	      definite-p))))

;;;
;;; Sort the list of methods METHODS which are applicable to TYPES
;;; destructively according to PRECEDENCE.  PRECEDENCE is a list of
;;; indices for the argument precedence order.
;;;
;;; Only used in this file.
;;;
(defun sort-applicable-methods (methods types precedence)
  (sort-methods methods
		precedence
		(lambda (class1 class2 index)
		  (let* ((class (type-class (nth index types)))
			 (cpl (cpl-maybe-early class)))
		    (if (memq class2 (memq class1 cpl))
			class1
			class2)))))

;;;
;;; Sort the list of methods METHODS using argument precedence order
;;; PRECEDENCE.  COMPARE-CLASSES is a function used to compare method
;;; specializers.
;;;
;;; Only used in this file.
;;;
(defun sort-methods (methods precedence compare-classes)
  (flet ((sorter (method1 method2)
	   (dolist (index precedence)
	     (flet ((specializers (method)
		      (if (listp method)
			  (early-method-specializers method t)
			  (method-specializers method))))
	       (let* ((specl1 (nth index (specializers method1)))
		      (specl2 (nth index (specializers method2)))
		      (more-specific (more-specific-specializer
				      specl1 specl2 index compare-classes)))
		 (when more-specific
		   (return-from sorter (eq more-specific specl1))))))))
    (stable-sort methods #'sorter)))

;;;
;;; Return the more specific specializer of SPEC1 and SPEC2, or null
;;; if equally specific.  INDEX is the index of SPEC1 and SPEC2 in the
;;; method specializers.  COMPARE-CLASSES is a function to call for
;;; comparing class specializers.
;;;
;;; Only used in this file.
;;;
(defun more-specific-specializer (specl1 specl2 index compare-classes)
  (flet ((spec-type (spec)
	   (if (eq *boot-state* 'complete)
	       (specializer-type spec)
	       (bootstrap-get-slot 'specializer spec 'type))))
    (let ((type1 (spec-type specl1))
	  (type2 (spec-type specl2)))
      (cond ((eq specl1 specl2)
	     nil)
	    ((atom type1)
	     specl2)
	    ((atom type2)
	     specl1)
	    (t
	     (case (car type1)
	       (class
		(case (car type2)
		  (class (funcall compare-classes specl1 specl2 index))
		  (t specl2)))
	       (prototype
		(case (car type2)
		  (class (funcall compare-classes specl1 specl2 index))
		  (t specl2)))
	       (class-eq
		(case (car type2)	; FIXME: ECASE?
		  (eql specl2)
		  (class-eq nil)
		  (class type1)))
	       (eql
		(case (car type2)
		  (eql nil)
		  (t specl1)))))))))


;;;; ***********************
;;;; MAP-ALL-ORDERS  *******
;;;; ***********************

(defun map-all-orders (methods precedence function)
  (let ((choices nil))
    (flet ((compare-classes-function (class1 class2 index)
	     (declare (ignore index))
	     (let ((choice nil))
	       (dolist (c choices nil)
		 (when (or (and (eq (first c) class1)
				(eq (second c) class2))
			   (and (eq (first c) class2)
				(eq (second c) class1)))
		   (return (setq choice c))))
	       (unless choice
		 (setq choice
		       (if (class-might-precede-p class1 class2)
			   (if (class-might-precede-p class2 class1)
			       (list class1 class2 nil t)
			       (list class1 class2 t))
			   (if (class-might-precede-p class2 class1)
			       (list class2 class1 t)
			       (let ((name1 (class-name class1))
				     (name2 (class-name class2)))
				 (if (and name1 name2 (symbolp name1) (symbolp name2)
					  (string< (symbol-name name1)
						   (symbol-name name2)))
				     (list class1 class2 t)
				     (list class2 class1 t))))))
		 (push choice choices))
	       (car choice))))
      (loop (funcall function
		     (sort-methods methods precedence #'compare-classes-function))
	    (unless (dolist (c choices nil)
		      (unless (third c)
			(rotatef (car c) (cadr c))
			(return (setf (third c) t))))
	      (return nil))))))

(defvar *in-precompute-effective-methods-p* nil)

;used only in map-all-orders
(defun class-might-precede-p (class1 class2)
  (if (not *in-precompute-effective-methods-p*)
      (not (member class1 (cdr (class-precedence-list class2))))
      (class-can-precede-p class1 class2)))


;;;; ********************************************
;;;; SPECIALIZER-APPLICABLE-USING-TYPE-P  *******
;;;; ********************************************

;;;
;;; First value is true if specializer SPECL is applicable to TYPE.
;;; Second value is true if its possibly applicable to TYPE.  This is
;;; used by c-a-m-u-t and generate-discrimination-net-internal, and
;;; has only what they need.
;;;
(defun specializer-applicable-using-type-p (specl type)
  (let ((specl (type-from-specializer specl)))
    (cond ((eq specl t)
	   (values t t))
	  ((or (atom type) (eq (car type) t))
	   (values nil t))
	  (t
	   (case (car type)
	     (class (saut-class specl type))
	     (and (saut-and specl type))
	     (not (saut-not specl type))
	     (prototype (saut-prototype specl type))
	     (class-eq (saut-class-eq specl type))
	     (eql (saut-eql specl type))
	     (t (internal-error
		 "~@<~s cannot handle the second argument ~s.~@:>"
		 'specializer-applicable-using-type-p type)))))))

(defun saut-and (specl type)
  (let ((applicable nil)
	(possibly-applicable t))
    (dolist (type (cdr type))
      (multiple-value-bind (appl poss-appl)
	  (specializer-applicable-using-type-p specl type)
	(when appl
	  (return (setq applicable t)))
	(unless poss-appl
	  (return (setq possibly-applicable nil)))))
    (values applicable possibly-applicable)))

(defun saut-not (specl type)
  (let ((ntype (cadr type)))
    (values
     nil
     (case (car ntype)
       (class
	(let* ((class (type-class specl))
	       (cpl (cpl-or-nil class)))
	  (not (memq (cadr ntype) cpl))))
       (class-eq
	(let ((class (case (car specl)
		       (eql (class-of (cadr specl)))
		       (class-eq (cadr specl)))))
	  (not (eq class (cadr ntype)))))
       (prototype
	(let* ((class (case (car specl)
			(eql (class-of (cadr specl)))
			(class-eq (cadr specl))
			(prototype (cadr specl))
			(class (cadr specl))))
	       (cpl (cpl-or-nil class)))
	  (not (memq (cadr ntype) cpl))))
       (eql
	(case (car specl)
	  (eql (not (eql (cadr specl) (cadr ntype))))
	  (t t)))
       (t
	(internal-error "~@<~s cannot handle the second argument ~s.~@:>"
			'specializer-applicable-using-type-p type))))))

(defun saut-class (specl type)
  (if (eq 'class (car specl))
      (let* ((specl (cadr specl))
	     (type (cadr type))
	     (cpl (cpl-or-nil type))
	     (pred (memq specl cpl)))
	(values pred
		(or pred
		    (if (not *in-precompute-effective-methods-p*)
			;; classes might get common subclass
			(superclasses-compatible-p specl type)
			;; worry only about existing classes
			(classes-have-common-subclass-p specl type)))))
      (values nil
	      (let* ((class (type-class specl))
		     (cpl (cpl-or-nil class)))
		(memq (cadr type) cpl)))))

(defun classes-have-common-subclass-p (class1 class2)
  (or (eq class1 class2)
      (let ((class1-subs (class-direct-subclasses class1)))
	(or (memq class2 class1-subs)
	    (dolist (class1-sub class1-subs nil)
	      (when (classes-have-common-subclass-p class1-sub class2)
		(return t)))))))

(defun saut-class-eq (specl type)
  (if (eq (car specl) 'eql)
      (values nil (eq (class-of (cadr specl)) (cadr type)))
      (let ((pred (case (car specl)
		    (class-eq   
		     (eq (cadr specl) (cadr type)))
		    (class      
		     (or (eq (cadr specl) (cadr type))
			 (memq (cadr specl)
			       (cpl-or-nil (cadr type))))))))
	(values pred pred))))

(defun saut-prototype (specl type)
  (declare (ignore specl type))
  (values nil nil)) ; fix this someday

(defun saut-eql (specl type) 
  (let ((pred (case (car specl)
		(eql
		 (eql (cadr specl) (cadr type)))
		(class-eq
		 (eq (cadr specl) (class-of (cadr type))))
		(class
		 (memq (cadr specl)
		       (cpl-or-nil (class-of (cadr type))))))))
    (values pred pred)))


(defvar *effective-method-cache* (make-hash-table :test 'eq))

(defun flush-effective-method-cache (gf)
  (dolist (method (generic-function-methods gf))
    (remhash method *effective-method-cache*)))

(defun get-secondary-dispatch-function (gf methods types &optional 
					method-alist wrappers)
  (let ((generator (get-secondary-dispatch-function1 
		    gf methods types
		    (not (null method-alist))
		    (not (null wrappers))
		    (not (methods-contain-eql-specializer-p methods)))))
    (make-callable gf methods generator method-alist wrappers)))

(defun get-secondary-dispatch-function1 (gf methods types method-alist-p wrappers-p
					 &optional all-applicable-p
					 (all-sorted-p t) function-p)
  (if (null methods)
      (if function-p
          (lambda (method-alist wrappers)
	    (declare (ignore method-alist wrappers))
	    #'(kernel:instance-lambda (&rest args)
                (apply #'no-applicable-method gf args)))
	  (lambda (method-alist wrappers)
	    (declare (ignore method-alist wrappers))
	    (lambda (&rest args)
	      (apply #'no-applicable-method gf args))))
      (let* ((key (car methods))
	     (ht-value (or (gethash key *effective-method-cache*)
			   (setf (gethash key *effective-method-cache*)
				 (cons nil nil)))))
	(if (and (null (cdr methods)) all-applicable-p ; the most common case
		 (null method-alist-p) wrappers-p (not function-p))
	    (or (car ht-value)
		(setf (car ht-value)
		      (get-secondary-dispatch-function2 
		       gf methods types method-alist-p wrappers-p
		       all-applicable-p all-sorted-p function-p)))
	    (let ((akey (list methods
			      (if all-applicable-p 'all-applicable types)
			      method-alist-p wrappers-p function-p)))
	      (or (cdr (assoc akey (cdr ht-value) :test #'equal))
		  (let ((value (get-secondary-dispatch-function2 
				gf methods types method-alist-p wrappers-p
				all-applicable-p all-sorted-p function-p)))
		    (push (cons akey value) (cdr ht-value))
		    value)))))))

(defvar *computing-standard-effective-method-p* nil)

(defun get-secondary-dispatch-function2 (gf methods types method-alist-p
					 wrappers-p all-applicable-p
					 all-sorted-p function-p)
  (if (and all-applicable-p all-sorted-p (not function-p))
      (if (and (eq *boot-state* 'complete)
	       ;; Because of loadable-pcl.  See combin.lisp.
	       (not (eq #'compute-effective-method gf))
	       (not *computing-standard-effective-method-p*))
	  (let* ((combin (generic-function-method-combination gf))
		 (effective (compute-effective-method gf combin methods)))
	    (callable-generator gf effective method-alist-p wrappers-p))
	  (let ((*computing-standard-effective-method-p* t)
		(effective (standard-compute-effective-method gf nil methods)))
	    (callable-generator gf effective method-alist-p wrappers-p)))
      (let ((net (generate-discrimination-net gf methods types all-sorted-p)))
	(compute-secondary-dispatch-function1 gf net function-p))))

(defun get-effective-method-function (gf methods
				      &optional method-alist wrappers)
  (let ((generator (get-secondary-dispatch-function1
		    gf methods nil (not (null method-alist))
		    (not (null wrappers)))))
    (make-callable gf methods generator method-alist wrappers)))

(defun get-effective-method-function1 (gf methods &optional (sorted-p t))
  (get-secondary-dispatch-function1 gf methods nil nil nil t sorted-p))

(defun methods-contain-eql-specializer-p (methods)
  (and (eq *boot-state* 'complete)
       (some (lambda (method)
	       (some #'eql-specializer-p (method-specializers method)))
	     methods)))


;;;; ************************
;;;; Debugging Stuff  *******
;;;; ************************

#+pcl-debug
(progn
  
(defvar dfun-count nil)
(defvar dfun-list nil)
(defvar *minimum-cache-size-to-list*)

(defun list-dfun (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (a (assq sym dfun-list)))
    (unless a
      (push (setq a (list sym)) dfun-list))
    (push (generic-function-name gf) (cdr a))))

(defun list-all-dfuns ()
  (setq dfun-list nil)
  (map-all-generic-functions #'list-dfun)
  dfun-list)

(defun list-large-cache (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (cache (gf-dfun-cache gf)))
    (when cache
      (let ((size (cache-size cache)))
	(when (>= size *minimum-cache-size-to-list*)
	  (let ((a (assoc size dfun-list)))
	    (unless a
	      (push (setq a (list size)) dfun-list))
	    (push (let ((name (generic-function-name gf)))
		    (if (eq sym 'caching) name (list name sym)))
		  (cdr a))))))))

(defun list-large-caches (&optional (*minimum-cache-size-to-list* 130))
  (setq dfun-list nil)
  (map-all-generic-functions #'list-large-cache)
  (setq dfun-list (sort dfun-list #'< :key #'car))
  (mapc #'print dfun-list)
  (values))

(defun count-dfun (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
	 (cache (gf-dfun-cache gf))
	 (a (assq sym dfun-count)))
    (unless a
      (push (setq a (list sym 0 nil)) dfun-count))
    (incf (cadr a))
    (when cache
      (let* ((size (cache-size cache))
	     (b (assoc size (third a))))
	(unless b 
	  (push (setq b (cons size 0)) (third a)))
	(incf (cdr b))))))

(defun count-all-dfuns ()
  (setq dfun-count (mapcar (lambda (type) (list type 0 nil))
			   '(ONE-CLASS TWO-CLASS DEFAULT-METHOD-ONLY
			     ONE-INDEX N-N CHECKING CACHING 
			     DISPATCH)))
  (map-all-generic-functions #'count-dfun)
  (mapc (lambda (type+count+sizes)
	  (setf (third type+count+sizes)
		(sort (third type+count+sizes) #'< :key #'car)))
	dfun-count)
  (mapc (lambda (type+count+sizes)
	  (format t "~&There are ~4d dfuns of type ~s"
		  (cadr type+count+sizes) (car type+count+sizes))
	  (format t "~%   ~S~%" (caddr type+count+sizes)))
	dfun-count)
  (values))


(defun show-dfun-constructors ()
  (format t "~&DFUN constructor caching is ~A." 
	  (if *enable-dfun-constructor-caching*
	      "enabled" "disabled"))
  (dolist (generator-entry *dfun-constructors*)
    (dolist (args-entry (cdr generator-entry))
      (format t "~&~S ~S"
	      (cons (car generator-entry) (caar args-entry))
	      (caddr args-entry)))))

) ; #+pcl-debug
