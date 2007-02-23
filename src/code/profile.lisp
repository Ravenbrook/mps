;;; -*- Package: Profile -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/profile.lisp,v 1.41 2005/05/26 19:09:15 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Description: Simple profiling facility.
;;;
;;; Author: Skef Wholey, Rob MacLachlan
;;;
;;; Compatibility: Runs in any valid Common Lisp.  Three small implementation-
;;;   dependent changes can be made to improve performance and prettiness.
;;;
;;; Dependencies: The macro Quickly-Get-Time and the function
;;;   Required-Arguments should probably be tailored to the implementation for
;;;   the best results.  They will default to working, albeit inefficent, forms
;;;   in non-CMU implementations.  The Total-Consing macro is used to profile
;;;   consing: in unknown implementations 0 will be used.
;;;   See the "Implementation Parameters" section.
;;;
;;; Note: a timing overhead factor is computed when REPORT-TIME is first
;;; called.  This will be incorrect if profiling code is run in a different
;;; environment than the first call to REPORT-TIME.  For example, saving a core
;;; image on a high performance machine and running it on a low performance one
;;; will result in use of an erroneously small timing overhead factor.  In CMU
;;; CL, this cache is invalidated when a core is saved.
;;;

(defpackage "PROFILE"
  (:use :common-lisp :ext :fwrappers)
  (:export *timed-functions* profile profile-all unprofile reset-time 
	   report-time report-time-custom *default-report-time-printfunction*
	   with-spacereport print-spacereports reset-spacereports
	   delete-spacereports *insert-spacereports*
	   *no-calls* *no-calls-limit*))

(in-package "PROFILE")


;;;; Implementation dependent interfaces:

(defconstant quick-time-units-per-second internal-time-units-per-second)
  
(defmacro quickly-get-time ()
  `(the time-type (get-internal-run-time)))

;;;
;;; The type of the result from quickly-get-time.
;;;
(deftype time-type () '(unsigned-byte 29))

;;; 
;;; Return two values: the first is the number of required arguments,
;;; and the second is T iff NAME has any non-required arguments
;;; (e.g. &OPTIONAL, &REST, &KEY).
;;;
(defun required-arguments (name)
  (let ((type (ext:info function type name)))
    (cond ((not (kernel:function-type-p type))
	   (values 0 t))
	  (t
	   (values (length (kernel:function-type-required type))
		   (if (or (kernel:function-type-optional type)
			   (kernel:function-type-keyp type)
			   (kernel:function-type-rest type))
		       t nil))))))

;;;
;;; TOTAL-CONSING is called to find the total number of bytes consed
;;; since the beginning of time.
;;;
(declaim (inline total-consing))
(defun total-consing () (ext:get-bytes-consed-dfixnum))

;;;
;;; The type of the result of TOTAL-CONSING.
;;;
(deftype consing-type () '(and fixnum unsigned-byte))

;;;
;;; On the CMUCL x86 port the return address is represented as a SAP
;;; and to save the costly calculation of the SAPs code object the
;;; profiler maintains callers as SAPs. These SAPs will become invalid
;;; if a caller code object moves, so this should be prevented by the
;;; use of purify or by moving code objects into an older generation
;;; when using GENCGC.
;;;
(defmacro get-caller-info ()
  `(nth-value 1 (kernel:%caller-frame-and-pc)))

#-x86
(defun print-caller-info (info stream)
  (prin1 (kernel:lra-code-header info) stream))

#+x86
(defun print-caller-info (info stream)
  (prin1 (nth-value 1 (di::compute-lra-data-from-pc info)) stream))


;;;; Global data structures:

(defvar *timed-functions* ()
  "List of functions that are currently being timed.")

(defvar *no-calls* nil
  "A list of profiled functions which weren't called.")

(defvar *no-calls-limit* 20
  "If the number of profiled functions that were not called is less than
this, the functions are listed.  If NIL, then always list the functions.")

;;;
;;; This is stored as user-data of profile fwrappers.
;;;
(defstruct (profile-info
	     (:conc-name pi-)
	     (:constructor make-profile-info (function-name callers-p)))
  ;;
  ;; The name of the function being profiled.
  function-name
  ;;
  ;; True if :CALLERS arg was given to PROFILE.
  (callers-p nil :type boolean)
  ;;
  ;; Various counters for profiling.
  (count 0 :type fixnum)
  (time 0 :type time-type)
  (consed-h 0 :type dfixnum:dfparttype)
  (consed-l 0 :type dfixnum:dfparttype)
  (consed-w/c-h 0 :type dfixnum:dfparttype)
  (consed-w/c-l 0 :type dfixnum:dfparttype)
  (profile 0 :type integer)
  (callers () :type list))

;;;
;;; Reset counters of the given PROFILE-INFO
;;;
(defun reset-profile-info (info)
  (setf (pi-count info) 0
	(pi-time info) 0
	(pi-consed-h info) 0
	(pi-consed-l info) 0
	(pi-consed-w/c-h info) 0
	(pi-consed-w/c-l info) 0
	(pi-profile info) 0
	(pi-callers info) ()))

;;;
;;; Return various profiling information from INFO as multiple values.
;;;
(defun profile-info-profiling-values (info)
  (values (pi-count info)
	  (pi-time info)
	  (dfixnum:dfixnum-pair-integer (pi-consed-h info)
					(pi-consed-l info))
	  (dfixnum:dfixnum-pair-integer (pi-consed-w/c-h info)
					(pi-consed-w/c-l info))
	  (pi-profile info)
	  (pi-callers info)))

;;;
;;; These variables are used to subtract out the time and consing for
;;; recursive and other dynamically nested profiled calls.  The total
;;; resource consumed for each nested call is added into the
;;; appropriate variable.  When the outer function returns, these
;;; amounts are subtracted from the total.
;;;
;;; *ENCLOSED-CONSING-H* and *ENCLOSED-CONSING-L* represent the total
;;; consing as a pair of fixnum-sized integers to reduce consing and
;;; allow for about 2^58 bytes of total consing.  (Assumes positive
;;; fixnums are 29 bits long).
;;;
(defvar *enclosed-time* 0)
(defvar *enclosed-consing-h* 0)
(defvar *enclosed-consing-l* 0)
(defvar *enclosed-profilings* 0)
(declaim (type time-type *enclosed-time*))
(declaim (type dfixnum:dfparttype *enclosed-consing-h*))
(declaim (type dfixnum:dfparttype *enclosed-consing-l*))
(declaim (fixnum *enclosed-profilings*))


;;;
;;; The number of seconds a bare function call takes.  Factored into
;;; the other overheads, but not used for itself.
;;;
(defvar *call-overhead*)

;;;
;;; The number of seconds that will be charged to a profiled function
;;; due to the profiling code.
;;;
(defvar *internal-profile-overhead*)

;;;
;;; The number of seconds of overhead for profiling that a single
;;; profiled call adds to the total runtime for the program.
;;;
(defvar *total-profile-overhead*)

(declaim (single-float *call-overhead* *internal-profile-overhead*
		       *total-profile-overhead*))


;;;; Profile encapsulations:

(eval-when (:compile-toplevel :load-toplevel :execute)

   ;;;
   ;;; Names of fwrappers look like (PROFILE <nreq> <optionals-p>).
   ;;; <Nreq> is the number of required parameters, <optionals-p> is
   ;;; true if the fwrapper is for functions with optional arguments.
   ;;;
  (define-function-name-syntax profile (name)
    (when (integerp (cadr name))
      (values t 'profile)))

  ;;;
  ;;; Return the profile fwrapper name for profiling a function
  ;;; with NREQ required arguments and optional arguments according
  ;;; to OPTIONALS-P.
  ;;;
  (defun make-profile-fwrapper-name (nreq optionals-p)
    `(profile ,nreq ,optionals-p))

  ;;;
  ;;; Return a DEFINE-FWRAPPER form for profiling a function with
  ;;; arguments according to NREQ and OPTIONALS-P.
  ;;;
  (defun make-profile-fwrapper (nreq optionals-p)
    (let ((req (loop repeat nreq collect (gensym)))
	  (name (make-profile-fwrapper-name nreq optionals-p)))
      `(define-fwrapper ,name (,@req ,@(if optionals-p `(&rest .rest.)))
	 (let* ((info (fwrapper-user-data fwrapper))
		(fn-name (pi-function-name info))
		(fdefn (lisp::fdefinition-object fn-name nil)))
	   ;;
	   ;; "Deactivate" the profile fwrapper for the time it is
	   ;; running to ease profiling of functions used in the
	   ;; implementation of PROFILE itself.
	   (letf (((lisp::fdefn-function fdefn) (fwrapper-next fwrapper)))
	     (incf (pi-count info))
	     ;;
	     ;; If :CALLERS was specified for profiling, record caller
	     ;; information.
	     (when (pi-callers-p info)
	       (let ((caller (get-caller-info)))
		 (do ((prev nil current)
		      (current (pi-callers info) (cdr current)))
		     ((null current)
		      (push (cons caller 1) (pi-callers info)))
		   (let ((old-caller-info (car current)))
		     (when (progn #-x86 (eq caller (car old-caller-info))
				  #+x86 (sys:sap= caller (car old-caller-info)))
		       (if prev
			   (setf (cdr prev) (cdr current))
			   (setf (pi-callers info) (cdr current)))
		       (setf (cdr old-caller-info)
			     (the fixnum (+ (cdr old-caller-info) 1)))
		       (setf (cdr current) (pi-callers info))
		       (setf (pi-callers info) current)
		       (return))))))
			       
	     (let ((time-inc 0)
		   (cons-inc-h 0)
		   (cons-inc-l 0)
		   (profile-inc 0))
	       (declare (type time-type time-inc)
			(type dfixnum:dfparttype cons-inc-h cons-inc-l)
			(fixnum profile-inc))
	       (multiple-value-prog1
		   (let ((start-time (quickly-get-time))
			 (start-consed-h 0)
			 (start-consed-l 0)
			 (end-consed-h 0)
			 (end-consed-l 0)
			 (*enclosed-time* 0)
			 (*enclosed-consing-h* 0)
			 (*enclosed-consing-l* 0)
			 (*enclosed-profilings* 0))
		     (dfixnum:dfixnum-set-pair start-consed-h
					       start-consed-l
					       (total-consing))
		     (multiple-value-prog1
			 (call-next-function)
		       (setq time-inc
			     #-BSD (- (quickly-get-time) start-time)
			     #+BSD (max (- (quickly-get-time) start-time) 0))
		       ;; How much did we cons so far?
		       (dfixnum:dfixnum-set-pair end-consed-h
						 end-consed-l
						 (total-consing))
		       (dfixnum:dfixnum-copy-pair cons-inc-h cons-inc-l
						  end-consed-h
						  end-consed-l)
		       (dfixnum:dfixnum-dec-pair cons-inc-h cons-inc-l
						 start-consed-h
						 start-consed-l)
		       ;; (incf consed (- cons-inc *enclosed-consing*))
		       (dfixnum:dfixnum-inc-pair (pi-consed-h info)
						 (pi-consed-l info)
						 cons-inc-h cons-inc-l)
		       (dfixnum:dfixnum-inc-pair (pi-consed-w/c-h info)
						 (pi-consed-w/c-l info)
						 cons-inc-h cons-inc-l)
		       (setq profile-inc *enclosed-profilings*)
		       (incf (pi-time info)
			     (the time-type
			       #-BSD
			       (- time-inc *enclosed-time*)
			       #+BSD
			       (max (- time-inc *enclosed-time*) 0)))
		       (dfixnum:dfixnum-dec-pair (pi-consed-h info)
						 (pi-consed-l info)
						 *enclosed-consing-h*
						 *enclosed-consing-l*)
		       (incf (pi-profile info) profile-inc)))
		 (incf *enclosed-time* time-inc)
		 ;; *enclosed-consing* = *enclosed-consing + cons-inc
		 (dfixnum:dfixnum-inc-pair *enclosed-consing-h*
					   *enclosed-consing-l*
					   cons-inc-h
					   cons-inc-l)))))))))

;;;
;;; Pre-define some profile fwrappers.
;;;
(macrolet ((def-profile-fwrapper (nreq)
	     `(progn
	       ,(make-profile-fwrapper nreq t)
	       ,(make-profile-fwrapper nreq nil))))
  (def-profile-fwrapper 0)
  (def-profile-fwrapper 1)
  (def-profile-fwrapper 2)
  (def-profile-fwrapper 3))

#+(or)
(defun ensure-profile-fwrapper (nreq optionals-p)
  "Ensure that a profile fwrapper for functions with NREQ required
   arguments and optional arguments according to OPTIONALS-P exists.
   Return the name of that fwrapper."
  (let ((name (make-profile-fwrapper-name nreq optionals-p)))
    (unless (fboundp name)
      (without-package-locks
       (eval (make-profile-fwrapper nreq optionals-p))
       (compile name)))
    name))

(defun ensure-profile-fwrapper (nreq optionals-p)
  "Ensure that a profile fwrapper for functions with NREQ required
   arguments and optional arguments according to OPTIONALS-P exists.
   Return the name of that fwrapper."
  (let ((name (make-profile-fwrapper-name nreq optionals-p)))
    (unless (fboundp name)
      (without-package-locks
	;; I (rtoy) do not know why the above version does not work,
	;; but this seems to work better.  
	(destructuring-bind (def name args &body body)
	    (macroexpand-1 (make-profile-fwrapper nreq optionals-p))
	  (declare (ignore def))
	  (compile name `(lambda ,args ,@body)))))
    name))

(defun find-profile-fwrapper (name)
  "Return the profile FWRAPPER object on function NAME, if any."
  (find-fwrapper name :type 'profile))

(defun pi-or-lose (name)
  "Return the PROFILE-INFO for function NAME.
   Signal an error if NAME is not profiled."
  (let ((f (find-profile-fwrapper name)))
    (if f
	(fwrapper-user-data f)
	(error "No profile info for ~s" name))))


;;; Interfaces:

;;; PROFILE-1-FUNCTION  --  Internal
;;;
;;;    Profile the function Name.  If already profiled, unprofile first.
;;;
(defun profile-1-function (name callers-p)
  (if (fboundp name)
      (multiple-value-bind (nreq optionals-p)
	  (required-arguments name)
	(when (find-profile-fwrapper name)
	  (warn "~s already profiled, unprofiling it first" name)
	  (unprofile-1-function name))
	(let ((ctor (ensure-profile-fwrapper nreq optionals-p)))
	  (fwrap name (fdefinition ctor) :type 'profile
		 :user-data (make-profile-info name callers-p))
	  (push name *timed-functions*)))
      (warn "Ignoring undefined function ~s" name)))


;;; PROFILE  --  Public
;;;
(defmacro profile (&rest names)
  "PROFILE Name*
   Wraps profiling code around the named functions.  As in TRACE, the names are
   not evaluated.  If a function is already profiled, then unprofile and
   reprofile (useful to notice function redefinition.)  If a name is undefined,
   then we give a warning and ignore it.

   CLOS methods can be profiled by specifying names of the form
   (METHOD <name> <qualifier>* (<specializer>*)), like in TRACE.

   :METHODS Function-Form is a way of specifying that all methods of a
   generic functions should be profiled.  The Function-Form is
   evaluated immediately, and the methods of the resulting generic
   function are profiled.

   If :CALLERS T appears, subsequent names have counts of the most
   common calling functions recorded.

   See also UNPROFILE, REPORT-TIME and RESET-TIME."
  (collect ((binds) (forms))
     (let ((names names)
	   (callers nil))
       (loop
	  (unless names (return))
	  (let ((name (pop names)))
	    (cond ((eq name :callers)
		   (setq callers (not (null (pop names)))))
		  ;;
		  ;; Method functions.
		  #+pcl
		  ((and (consp name) (eq 'method (car name)))
		   (let ((fast-name `(pcl::fast-method ,@(cdr name))))
		     (forms `(when (fboundp ',name)
			       (profile-1-function ',name ,callers)
			       (reinitialize-method-function ',name)))
		     (forms `(when (fboundp ',fast-name)
			       (profile-1-function ',fast-name ,callers)
			       (reinitialize-method-function ',fast-name)))))
		  ;;
		  ;; All method of a generic function.
		  #+pcl
		  ((eq :methods name)
		   (let ((tem (gensym)))
		     (binds `(,tem ,(pop names)))
		     (forms `(dolist (name
				       (debug::all-method-function-names ,tem))
			       (when (fboundp name)
				 (profile-1-function name ,callers)
				 (reinitialize-method-function name))))))
		  (t
		   (forms `(profile-1-function ',name ,callers))))))
       (if (binds)
	   `(let ,(binds) ,@(forms) (values))
	   `(progn ,@(forms) (values))))))

;;; PROFILE-ALL -- Public
;;;
;;; Add profiling to all symbols in the given package.
;;;
(defun profile-all (&key (package *package*) (callers-p nil)
		    (methods nil))
  "PROFILE-ALL

 Wraps profiling code around all functions in PACKAGE, which defaults
 to *PACKAGE*. If a function is already profiled, then unprofile and
 reprofile (useful to notice function redefinition.)  If a name is
 undefined, then we give a warning and ignore it.  If CALLERS-P is T
 names have counts of the most common calling functions recorded.

 When called with arguments :METHODS T, profile all methods of all
 generic function having names in the given package.  Generic functions
 themselves, that is, their dispatch functions, are left alone.

 See also UNPROFILE, REPORT-TIME and RESET-TIME. "
  (let ((package (if (packagep package)
		     package
		     (find-package package))))
    (do-symbols (symbol package (values))
      (when (and (eq (symbol-package symbol) package)
		 (fboundp symbol)
		 (not (special-operator-p symbol))
		 (or (not methods)
		     (not (typep (fdefinition symbol) 'generic-function))))
	(profile-1-function symbol callers-p)))
    ;;
    ;; Profile all method functions whose generic function name
    ;; is in the package.
    (when methods
      (dolist (name (debug::all-method-functions-in-package package))
	(when (fboundp name)
	  (profile-1-function name callers-p)
	  (reinitialize-method-function name))))))

;;; UNPROFILE  --  Public
;;;
(defmacro unprofile (&rest names)
  "Unwraps the profiling code around the named functions.  Names defaults to
  the list of all currently profiled functions."
  (collect ((binds) (forms))
    (let ((names (or names *timed-functions*)))
      (loop
	 (unless names (return))
	 (let ((name (pop names)))
	   (cond #+pcl
		 ((and (consp name)
		       (member (car name) '(method pcl::fast-method)))
		  (let ((name `(method ,@(cdr name)))
			(fast-name `(pcl::fast-method ,@(cdr name))))
		    (forms `(when (fboundp ',name)
			      (unprofile-1-function ',name)
			      (reinitialize-method-function ',name)))
		    (forms `(when (fboundp ',fast-name)
			      (unprofile-1-function ',fast-name)
			      (reinitialize-method-function ',fast-name)))))
		 #+pcl
		 ((eq :methods name)
		  (let ((tem (gensym)))
		    (binds `(,tem ,(pop names)))
		    (forms `(dolist (name (debug::all-method-function-names ,tem))
			      (when (fboundp name)
				(unprofile-1-function name)
				(reinitialize-method-function name))))))
		 (t
		  (forms `(unprofile-1-function ',name))))))
      (if (binds)
	  `(let ,(binds) ,@(forms) (values))
	  `(progn ,@(forms) (values))))))


;;; UNPROFILE-1-FUNCTION  --  Internal
;;;
(defun unprofile-1-function (name)
  (funwrap name :type 'profile)
  (setq *timed-functions* (delete name *timed-functions* :test #'equal)))


(defun re-profile-redefined-function (name new-value)
  (declare (ignore new-value))
  (let (f)
    (when (and (fboundp name)
	       (setq f (find-profile-fwrapper name)))
      (profile-1-function name (pi-callers-p (fwrapper-user-data f))))))

(push #'re-profile-redefined-function ext:*setf-fdefinition-hook*)



;;; COMPENSATE-TIME  --  Internal
;;;
;;;    Return our best guess for the run time in a function, subtracting out
;;; factors for profiling overhead.  We subtract out the internal overhead for
;;; each call to this function, since the internal overhead is the part of the
;;; profiling overhead for a function that is charged to that function.
;;;
;;;    We also subtract out a factor for each call to a profiled function
;;; within this profiled function.  This factor is the total profiling overhead
;;; *minus the internal overhead*.  We don't subtract out the internal
;;; overhead, since it was already subtracted when the nested profiled
;;; functions subtracted their running time from the time for the enclosing
;;; function.
;;;
(defun compensate-time (calls time profile)
  (let ((compensated
	 (- (/ (float time) (float quick-time-units-per-second))
	    (* *internal-profile-overhead* (float calls))
	    (* (- *total-profile-overhead* *internal-profile-overhead*)
	       (float profile)))))
    (if (minusp compensated) 0.0 compensated)))


(defstruct width-info
  cons
  calls
  time
  time/call
  cons/call)
	   
;; Compute and return the width of the field needed to hold the total
;; time, total cons, total-calls, and the max time/call.
(defun compute-widths (info)
  (let ((total-time 0)
	(total-cons 0)
	(total-calls 0)
	(max-time/call 0)
	(max-cons/call 0))
    ;; Find the total time, total consing, total calls, and the max
    ;; time/call
    (dolist (item info)
      (let ((time (time-info-time item)))
	(incf total-time time)
	(incf total-cons (time-info-consing item))
	(incf total-calls (time-info-calls item))
	(setf max-time/call (max max-time/call
				 (/ time (float (time-info-calls item)))))
	(setf max-cons/call (max max-cons/call
				 (/ (time-info-consing item)
				    (float (time-info-calls item)))))))

    ;; Figure out the width needed for total-time, total-cons,
    ;; total-calls and the max-time/call.  The total-cons is more
    ;; complicated because we print the consing with comma
    ;; separators. For total-time, we assume a default of "~10,3F";
    ;; for total-calls, "~7D"; for time/call, "~10,5F".  This is where
    ;; the constants come from.
    (flet ((safe-log10 (x)
	     ;; log base 10 of x, but any non-positive value of x, 0
	     ;; is ok for what we want.
	     (if (zerop x)
		 0.0
		 (log x 10))))
      (let ((cons-length (ceiling (safe-log10 total-cons)))
	    (calls-length (ceiling (safe-log10 total-calls)))
	    (cons/call-len (ceiling (safe-log10 max-cons/call))))
	;; Adjust these to include the number of commas that will be
	;; printed.
	(incf cons-length (floor (safe-log10 total-cons) 3))
	(incf calls-length (floor (safe-log10 total-calls) 3))
	(incf cons/call-len (floor (safe-log10 max-cons/call) 3))
	(make-width-info :cons (max 9 cons-length)
			 :calls (max 7 calls-length)
			 :time (+ 4 (ceiling (safe-log10 total-time)))
			 :time/call (+ 6 (max 2 (ceiling (safe-log10 max-time/call))))
			 :cons/call (max 8 cons/call-len))))))

(defstruct (time-info
	    (:constructor make-time-info
			  (name calls time consing consing-w/c callers)))
  name
  calls
  time
  consing
  consing-w/c
  callers)

(defstruct (time-totals)
  (time 0.0)
  (consed 0)
  (calls 0))

(defun report-times-time (time action &optional field-widths)
  (multiple-value-bind (time-width cons-width calls-width time/call-width cons/call-width)
      (if field-widths
	  (values (width-info-time field-widths)
		  (width-info-cons field-widths)
		  (width-info-calls field-widths)
		  (width-info-time/call field-widths)
		  (width-info-cons/call field-widths))
	  (values 9 9 7 10 10))
    (case action
      (:head
       (format *trace-output*
	       "~&~V@A | ~V@A | ~V@A | ~V@A | ~V@A | Name:~@
	       -----------------------------------------------------------------------~%"
	       cons-width "Consed"
	       calls-width "Calls"
	       time-width "Secs"
	       time/call-width "Sec/Call"
	       cons/call-width "Bytes/C."
	       )
       (return-from report-times-time))

      (:tail
       (format *trace-output*
	       "-------------------------------------------------------------------~@
	      ~V:D | ~V:D | ~V,3F | ~V:A | ~V:A | Total~%"
	       cons-width (time-totals-consed time)
	       calls-width (time-totals-calls time)
	       time-width (time-totals-time time)
	       time/call-width ""
	       cons/call-width ""))
      (:sort (sort time #'>= :key #'time-info-time))
      (:one-function
       (format *trace-output*
	       "~V:D | ~V:D | ~V,3F | ~V,5F | ~V:D | ~S~%"
	       cons-width (floor (time-info-consing time))
	       calls-width (time-info-calls time)
	       time-width (time-info-time time)
	       time/call-width (/ (time-info-time time) (float (time-info-calls time)))
	       cons/call-width
	       (round
		(/ (time-info-consing time) (float (time-info-calls time))))
	       (time-info-name time)))
      (t
       (error "Unknown action for profiler report: ~s" action)))))

(defun report-times-space (time action &optional field-widths)
  (case action
    (:head
     (format *trace-output*
	     "~& Consed w/c |  Consed    |   Calls   | Sec/Call  | Bytes/C.  | Name:~@
	       -----------------------------------------------------------------------~%")
     (return-from report-times-space))
    
    (:tail
     (format *trace-output*
	     "-------------------------------------------------------------------~@
	      :-)         |~11:D |~10:D |           |           | Total~%"
	     (time-totals-consed time) (time-totals-calls time)))
    (:sort (sort time #'>= :key #'time-info-consing))
    (:one-function
     (format *trace-output*
	     "~11:D |~11:D |~10:D |~10,5F |~10:D | ~S~%"
	     (floor (time-info-consing-w/c time))
	     (floor (time-info-consing time))
	     (time-info-calls time)
	     (/ (time-info-time time) (float (time-info-calls time)))
	     (round
	      (/ (time-info-consing time) (float (time-info-calls time))))
	     (time-info-name time)))
    (t
     (error "Unknown action for profiler report"))))

(defparameter *default-report-time-printfunction* #'report-times-time)

(defun %report-times (names
		      &key (printfunction *default-report-time-printfunction*))
  (declare (optimize (speed 0)))
  (unless (boundp '*call-overhead*)
    (compute-time-overhead))
  (let ((info ())
	(no-call ())
	(widths ()))
    (dolist (name names)
      (let ((pinfo (pi-or-lose name)))
	(multiple-value-bind (calls time consing consing-w/c profile callers)
	    (profile-info-profiling-values pinfo)
	  (if (zerop calls)
	      (push name no-call)
	      (push (make-time-info name calls
				    (compensate-time calls time profile)
				    consing
				    consing-w/c
				    (sort (copy-seq callers)
					  #'>= :key #'cdr))
		    info)))))
    
    (setq info (funcall printfunction info :sort))

    (setf widths (compute-widths info))

    (funcall printfunction nil :head widths)

    (let ((totals (make-time-totals)))
      (dolist (time info)
	(incf (time-totals-time totals) (time-info-time time))
	(incf (time-totals-calls totals) (time-info-calls time))
	(incf (time-totals-consed totals) (time-info-consing time))

	(funcall printfunction time :one-function widths)

	(let ((callers (time-info-callers time))
	      (*print-readably* nil))
	  (when callers
	    (dolist (x (subseq callers 0 (min (length callers) 5)))
	      (format *trace-output* "~13T~10:D: " (cdr x))
	      (print-caller-info (car x) *trace-output*)
	      (terpri *trace-output*))
	    (terpri *trace-output*))))
      (funcall printfunction totals :tail widths))
    
    (when no-call
      (setf *no-calls* no-call)
      (if (and (realp *no-calls-limit*)
	       (>= (length no-call) *no-calls-limit*))
	  (format *trace-output*
		  "~%~D functions were not called.  ~
                  See profile::*no-calls* for a list~%"
		  (length no-call))
	  (format *trace-output*
		  "~%These functions were not called:~%~{~<~%~:; ~S~>~}~%"
		  (sort no-call #'string<
			:key (lambda (n)
			       (if (symbolp n)
				   (symbol-name n)
				   (multiple-value-bind (valid block-name)
				       (ext:valid-function-name-p n)
				     (declare (ignore valid))
				     (if block-name
					 block-name
					 (princ-to-string n)))))))))
    (values)))


(defmacro reset-time (&rest names)
  "Resets the time counter for the named functions.  Names defaults to the list
  of all currently profiled functions."
  `(%reset-time ,(if names `',names '*timed-functions*)))

(defun %reset-time (names)
  (dolist (name names)
    (reset-profile-info (pi-or-lose name)))
  (values))


(defmacro report-time (&rest names)
  "Reports the time spent in the named functions.  Names defaults to the list
  of all currently profiled functions."
  `(%report-times ,(if names `',names '*timed-functions*)))

(defun report-time-custom (&key names printfunction)
  "Reports the time spent in the named functions.  Names defaults to the list
  of all currently profiled functions.  Uses printfunction."
  (%report-times (or names *timed-functions*)
		 :printfunction
		 (or (typecase printfunction
		       (null *default-report-time-printfunction*)
		       (function printfunction)
		       (symbol
		        (case printfunction
			  (:space #'report-times-space)
			  (:time #'report-times-time))))
		     (error "Cannot handle printfunction ~s" printfunction))))


;;;; Overhead computation.

;;; We average the timing overhead over this many iterations.
;;;
(defconstant timer-overhead-iterations 5000)


;;; COMPUTE-TIME-OVERHEAD-AUX  --  Internal
;;;
;;;    Dummy function we profile to find profiling overhead.  Declare
;;; debug-info to make sure we have arglist info.
;;;
(declaim (notinline compute-time-overhead-aux))
(defun compute-time-overhead-aux (x)
  (declare (ext:optimize-interface (debug 2)))
  (declare (ignore x)))


;;; COMPUTE-TIME-OVERHEAD  --  Internal
;;;
;;;    Initialize the profiling overhead variables.
;;;
(defun compute-time-overhead ()
  (macrolet ((frob (var)
	       `(let ((start (quickly-get-time))
		      (fun (symbol-function 'compute-time-overhead-aux)))
		  (dotimes (i timer-overhead-iterations)
		    (funcall fun fun))
		  (setq ,var
			(/ (float (- (quickly-get-time) start))
			   (float quick-time-units-per-second)
			   (float timer-overhead-iterations))))))
    (frob *call-overhead*)

    (unwind-protect
	 (progn
	   (profile compute-time-overhead-aux)
	   (frob *total-profile-overhead*)
	   (decf *total-profile-overhead* *call-overhead*)
	   (let ((pinfo (pi-or-lose 'compute-time-overhead-aux)))
	     (multiple-value-bind (calls time)
		 (profile-info-profiling-values pinfo)
	       (declare (ignore calls))
	       (setq *internal-profile-overhead*
		     (/ (float time)
			(float quick-time-units-per-second)
			(float timer-overhead-iterations))))))
      (unprofile compute-time-overhead-aux))))

(pushnew (lambda ()
	   (makunbound '*call-overhead*))
	 ext:*before-save-initializations*)


;;;
;;; (with-spacereport <tag> <body> ...) and friends
;;;

;;; TODO:
;;; - if counting place haven't been allocated at compile time, try to do it
;;;   at load time
;;; - Introduce a mechanism that detects whether *all* calls were the same
;;;   amount of bytes (single variable).
;;; - record the source file and place this report appears in
;;; - detect whether this is a nested spacereport and if so, record
;;;   the outer reports

;; This struct is used for whatever counting the checkpoints do
;; AND
;; stores information we find at compile time
(defstruct spacereport-info
  (n 0 :type fixnum)
  (consed-h 0 :type dfixnum:dfparttype)
  (consed-l 0 :type dfixnum:dfparttype)
  (codesize -1 :type fixnum))

;; In the usual case, the hashtable with entries will be allocated at
;; compile or load time
(eval-when (load eval)
  (defvar *spacereports* (make-hash-table)))

;;
;; Helper functions
;;
(defun format-quotient (p1 p2 width komma)
  (let (format)
    (cond ((= 0 p2)
	   (make-string width :initial-element #\ ))
	  ((and (integerp p1)
		(integerp p2)
		(zerop (rem p1 p2)))
	   (setf format (format nil "~~~d:D!~a"
				(- width komma 1)
				(make-string komma :initial-element #\ )))
	   (format nil format (/ p1 p2)))
	  (t
	   (setf format (format nil "~~~d,~df" width komma))
	   (format nil format (/ (float p1) (float p2)))))))

(defun deep-list-length (list)
  (let ((length 0))
    (dolist (e list)
      (when (listp e)
	(incf length (deep-list-length e)))
      (incf length))
    length))

;; bunch for tests for above
#+nil
(defun test-format-quotient ()
  (print (format-quotient 10 5 10 2))
  (print (format-quotient 10 3 10 2))
  (print (format-quotient 10 5 10 0))
  (print (format-quotient 10 3 10 0))
  (print (format-quotient 10 0 10 0)))

(defvar *insert-spacereports* t)

;; Main wrapper macro for user - exported
(defmacro with-spacereport (name-or-args &body body)
  (if (not *insert-spacereports*)
      `(progn ,@body)
      (let ((name
	     (typecase name-or-args
	       (symbol name-or-args)
	       (cons (first name-or-args))
	       (t (error "Spacereport args neither symbol nor cons") nil)))
	    (options (if (consp name-or-args)
			 (rest name-or-args)
			 nil)))
	(when (gethash name *spacereports*)
	  (unless (find :mok options)
	    (warn "spacereport for ~a was requested before, resetting it"
		  name)))
	(setf (gethash name *spacereports*) (make-spacereport-info))
	(setf (spacereport-info-codesize (gethash name *spacereports*))
	      (deep-list-length body))

	`(let* ((counterplace nil)
		(place (gethash ,name *spacereports*))
		(start-h 0)
		(start-l 0))
	  (declare (type dfixnum:dfparttype start-h start-l))
	  (declare (type (or dfixnum:dfixnum null) counterplace))
	  (declare (type (or spacereport-info null) place))

	  ;; Make sure counter is there
	  (unless place
	    ;; Ups, it isn't, so create it...
	    (setf place (make-spacereport-info))
	    (setf (gethash ,name *spacereports*) place)
	    (print
	     "with-spaceprofile had to create place, leaked bytes to outer
              spacereports in nested calls"))

	  ;; Remember bytes already consed at start
	  (setf counterplace (total-consing))
	  (dfixnum:dfixnum-set-pair start-h start-l counterplace)

	  (prog1
	      (progn ,@body)

	    (incf (spacereport-info-n place))
	    ;; Add bytes newly consed.
	    ;; first update counterplace.
	    (total-consing)
	    (dfixnum:dfixnum-inc-pair (spacereport-info-consed-h place)
				      (spacereport-info-consed-l place)
				      (dfixnum::dfixnum-h counterplace)
				      (dfixnum::dfixnum-l counterplace))
	    (dfixnum:dfixnum-dec-pair (spacereport-info-consed-h place)
				      (spacereport-info-consed-l place)
				      start-h
				      start-l))))))

(defun print-spacereports (&optional (stream *trace-output*))
  (maphash (lambda (key value)
	     (format
	      stream
	      "~&~10:D bytes ~9:D calls ~a b/call: ~a (sz ~d)~%"
	      (dfixnum:dfixnum-pair-integer
	       (spacereport-info-consed-h value)
	       (spacereport-info-consed-l value))
	      (spacereport-info-n value)
	      (format-quotient (dfixnum:dfixnum-pair-integer
				(spacereport-info-consed-h value)
				(spacereport-info-consed-l value))
			       (spacereport-info-n value)
			       10 2)
	      key
	      (spacereport-info-codesize value)))
	   *spacereports*))

(defun reset-spacereports ()
  (maphash (lambda (key value)
	     (declare (ignore key))
	     (setf (spacereport-info-consed-h value) 0)
	     (setf (spacereport-info-consed-l value) 0)
	     (setf (spacereport-info-n value) 0))
	   *spacereports*))

(defun delete-spacereports ()
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (remhash key *spacereports*))
	   *spacereports*))
