;;; -*- Mode: Lisp; Package: Profile; Log: profile.log -*-
;;;
;;; This code has been placed in the public domain by the author.
;;; It is distributed without warranty of any kind.
;;;
;;; Description: Simple profiling facility.
;;;
;;; Author: Skef Wholey, Rob MacLachlan
;;;
;;; Current maintainer:	Rob MacLachlan
;;;
;;; Address: Carnegie-Mellon University
;;;          Computer Science Department
;;;	     Pittsburgh, PA 15213
;;;
;;; Net address: ram@cs.cmu.edu
;;;
;;; Copyright status: Public domain.
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
;;; Note: a timing overhead factor is computed at load time.  This will be
;;;   incorrect if profiling code is run in a different environment than this
;;;   file was loaded in.  For example, saving a core image on a high
;;;   performance machine and running it on a low performance one will result
;;;   in use of an erroneously small timing overhead factor.
;;;
(in-package "OPROFILE")

(export '(*timed-functions* profile unprofile report-time reset-time))



(progn
  #-:cmu
  (eval-when (compile eval)
    (warn
     "You may want to supply an implementation-specific ~
     Quickly-Get-Time function."))

  (defconstant quick-time-units-per-second internal-time-units-per-second)
  
  (defmacro quickly-get-time ()
    `(get-internal-run-time)))


;;; To avoid unnecessary consing in the "encapsulation" code, we find out the
;;; number of required arguments, and use &rest to capture only non-required
;;; arguments.  The function Required-Arguments returns two values: the first
;;; is the number of required arguments, and the second is T iff there are any
;;; non-required arguments (e.g. &optional, &rest, &key).

#+nil
(defun required-arguments (name)
  (let ((function (symbol-function name)))
    (if (eql (system:%primitive get-type function) system:%function-type)
	(let ((min (ldb system:%function-min-args-byte
			(system:%primitive header-ref function
					   system:%function-min-args-slot)))
	      (max (ldb system:%function-max-args-byte
			(system:%primitive header-ref function
					   system:%function-max-args-slot)))
	      (rest (ldb system:%function-rest-arg-byte
			 (system:%primitive header-ref function
					    system:%function-rest-arg-slot)))
	      (key (ldb system:%function-keyword-arg-byte
			(system:%primitive header-ref function
					   system:%function-keyword-arg-slot))))
	  (values min (or (/= min max) (/= rest 0) (/= key 0))))
	(values 0 t))))
	   

#-:cmu
(progn
 (eval-when (compile eval)
   (warn
    "You may want to add an implementation-specific Required-Arguments function."))
 (eval-when (load eval)
   (defun required-arguments (name)
     (declare (ignore name))
     (values 0 t))))



;;; The Total-Consing macro is called to find the total number of bytes consed
;;; since the beginning of time.

#+cmu
(defmacro total-consing () '(ext:get-bytes-consed))

#-:cmu
(progn
  (eval-when (compile eval)
    (warn "No consing will be reported unless a Total-Consing function is ~
           defined."))

  (defmacro total-consing () '0))
    

(defvar *timed-functions* ()
  "List of functions that are currently being timed.")


(defmacro profile (&rest names)
  "Wraps profiling code around the named functions.  The Names are not evaluated,
  as in Trace."
  (do ((names names (cdr names))
       (stuff ()))
      ((null names)
       ;; Keep the compiler quiet by sending standard output to bit bucket.
       `(compiler-let (#|(*standard-output* (make-broadcast-stream))|#)
	  ,@stuff
	  (values)))
    (push (profile-1-function (car names)) stuff)))


;;; A function is profiled by replacing its definition with a closure created by
;;; the following function.  The closure records the starting time, calls the
;;; original function, and records finishing time.  Other closures are used to
;;; perform various operations on the encapsulated function.

(defun profile-1-function (name)
  (multiple-value-bind (min-args optionals-p)
		       (required-arguments name)
    (let ((required-args ()))
      (dotimes (i min-args)
	(push (gensym) required-args))
      `(funcall
	(compile
	 nil
	 ;; Use ' instead of #' below so guaranteed null lexical environment.
	 '(lambda ()
	    (let* ((time 0)
		   (count 0)
		   (consed 0)
		   (old-definition (symbol-function ',name))
		   (new-definition
		    #'(lambda (,@required-args
			       ,@(if optionals-p
				     `(&rest optional-args)))
			(incf count)
			(let ((old-time time)
			      (start-time (quickly-get-time))
			      (old-consed consed)
			      (start-consed (total-consing)))
			  (multiple-value-prog1
			    ,(if optionals-p
				 `(apply old-definition
					 ,@required-args optional-args)
				 `(funcall old-definition ,@required-args))
			    (setq time
				  (+ old-time (- (quickly-get-time)
						 start-time)))
			    (setq consed
				  (+ old-consed (- (total-consing)
						   start-consed))))))))
	      (pushnew ',name *timed-functions*)
	      (setf (get ',name 'read-time)
		    #'(lambda ()
			(values count time consed))
		    (get ',name 'reset-time)
		    #'(lambda ()
			(setq count 0)
			(setq time 0)
			(setq consed 0)
			t)
		    (symbol-function ',name)
		    new-definition
		    (get ',name 'reset-definition)
		    #'(lambda ()
			(remprop ',name 'read-time)
			(remprop ',name 'reset-time)
			(if (eq (symbol-function ',name) new-definition)
			    (setf (symbol-function ',name) old-definition)
			    (warn "The function ~S was redefined without ~
				  unprofiling and reprofiling.~%~
				  Timing figures have not been updated ~
				  since that redefinition."
				  ',name))
			(remprop ',name 'reset-definition)
			(setq *timed-functions*
			      (delete ',name *timed-functions*))
			nil)))))))))


(defmacro unprofile (&rest names)
  "Unwraps the profiling code around the named functions.  Names defaults to the
  list of all currently profiled functions."
  `(dolist (name ,(if names `',names '*timed-functions*) (values))
     (unprofile-1-function name)))

(defun unprofile-1-function (name)
  (if (get name 'reset-definition)
      (funcall (get name 'reset-definition))
      (error "~S is not a function being profiled." name)))


(defmacro report-time (&rest names)
  "Reports the time spent in the named functions.  Names defaults to the list of
  all currently profiled functions."
  `(%report-times ,(if names `',names '*timed-functions*)))


;;; We average the timing overhead over this many iterations.
;;;
(defconstant timer-overhead-iterations 5000)

;;; Compute-Time-Overhead  --  Internal
;;;
;;;    Return as a float the total number of seconds it takes to call both
;;; Quickly-Get-Time and Total-Consing together, plus a funcall thrown in
;;; to represent some of the other overhead.   We also return a something
;;; computed from the results in order to frustrate clever compilers.
;;;
(defun compute-time-overhead-aux (x)
  x)
(proclaim '(notinline compute-time-overhead-aux))
;;;
(defun compute-time-overhead ()
  (let ((foo 0)
	(fun (symbol-function 'compute-time-overhead-aux))
	(start (quickly-get-time)))
    (funcall fun nil)
    (dotimes (i timer-overhead-iterations)
      (setq foo (logxor (funcall fun (quickly-get-time))
			(total-consing)
			foo)))
    (let ((now (quickly-get-time)))
      (values
       (/ (float (- now start))
	  (float timer-overhead-iterations)
	  (float quick-time-units-per-second))
       foo))))


(defvar *time-overhead* (compute-time-overhead))

(defstruct (time-info
	    (:constructor make-time-info (name calls time consing)))
  name
  calls
  time
  consing)


(defun %report-times (names)
  (let ((info ())
	(no-call ()))
    (dolist (name names)
      (multiple-value-bind
	  (calls time consing)
	  (funcall (or (get name 'read-time)
		       (error "~S is not profiled.")))
	(if (zerop calls)
	    (push name no-call)
	    (let ((compensated
		   (- (/ (float time) (float quick-time-units-per-second))
		      (* *time-overhead* (float calls)))))
	      (push (make-time-info name calls
				    (if (minusp compensated) 0.0 compensated)
				    consing)
		    info)))))
    
    (setq info (sort info #'>= :key #'time-info-time))

    (format *trace-output*
	    "~&  Seconds  |  Consed   |  Calls  |  Sec/Call  |  Name:~@
	       ------------------------------------------------------~%")

    (let ((total-time 0.0)
	  (total-consed 0)
	  (total-calls 0))
      (dolist (time info)
	(incf total-time (time-info-time time))
	(incf total-calls (time-info-calls time))
	(incf total-consed (time-info-consing time))
	(format *trace-output*
		"~10,3F | ~9:D | ~7:D | ~10,5F | ~S~%"
		(time-info-time time)
		(time-info-consing time)
		(time-info-calls time)
		(/ (time-info-time time) (float (time-info-calls time)))
		(time-info-name time)))
      (format *trace-output*
	      "------------------------------------------------------~@
	      ~10,3F | ~9:D | ~7:D |            | Total~%"
	      total-time total-consed total-calls)

      (format *trace-output*
	      "~%Estimated total profiling overhead: ~4,2F seconds~%"
	      (* *time-overhead* (float total-calls) 2.0)))

    (when no-call
      (format *trace-output*
	      "~%These functions were not called:~%~{~<~%~:; ~S~>~}~%"
	      (sort no-call #'string< :key #'symbol-name)))
    (values)))


(defmacro reset-time (&rest names)
  "Resets the time counter for the named functions.  Names defaults to the list
  of all currently profiled functions."
  `(dolist (name ,(if names `',names '*timed-functions*) (values))
     (reset-1-time name)))

(defun reset-1-time (name)
  (if (get name 'reset-time)
      (funcall (get name 'reset-time))
      (error "~S is not a function being profiled.")))
