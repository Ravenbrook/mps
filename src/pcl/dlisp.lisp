;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
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
  "$Header: /project/cmucl/cvsroot/src/pcl/dlisp.lisp,v 1.12 2003/05/04 13:11:21 gerd Exp $")
;;;

(in-package :pcl)

;;; This file is (almost) functionally equivalent to dlap.lisp,
;;; but easier to read.

;;; Might generate faster code, too, depending on the compiler and 
;;; whether an implementation-specific lap assembler was used.

(defun emit-one-class-reader (class-slot-p)
  (emit-reader/writer :reader 1 class-slot-p))

(defun emit-one-class-boundp (class-slot-p)
  (emit-reader/writer :boundp 1 class-slot-p))

(defun emit-one-class-writer (class-slot-p)
  (emit-reader/writer :writer 1 class-slot-p))

(defun emit-two-class-reader (class-slot-p)
  (emit-reader/writer :reader 2 class-slot-p))

(defun emit-two-class-boundp (class-slot-p)
  (emit-reader/writer :boundp 2 class-slot-p))

(defun emit-two-class-writer (class-slot-p)
  (emit-reader/writer :writer 2 class-slot-p))

;;; --------------------------------

(defun emit-one-index-readers (class-slot-p)
  (emit-one-or-n-index-reader/writer :reader nil class-slot-p))

(defun emit-one-index-boundps (class-slot-p)
  (emit-one-or-n-index-reader/writer :boundp nil class-slot-p))

(defun emit-one-index-writers (class-slot-p)
  (emit-one-or-n-index-reader/writer :writer nil class-slot-p))

(defun emit-n-n-readers ()
  (emit-one-or-n-index-reader/writer :reader t nil))

(defun emit-n-n-boundps ()
  (emit-one-or-n-index-reader/writer :boundp t nil))

(defun emit-n-n-writers ()
  (emit-one-or-n-index-reader/writer :writer t nil))

;;; --------------------------------

(defun emit-checking (metatypes applyp)
  (emit-checking-or-caching nil nil metatypes applyp))

(defun emit-caching (metatypes applyp)
  (emit-checking-or-caching t nil metatypes applyp))

(defun emit-in-checking-cache-p (metatypes)
  (emit-checking-or-caching nil t metatypes nil))

(defun emit-constant-value (metatypes)
  (emit-checking-or-caching t t metatypes nil))

;;; --------------------------------

(defvar *precompiling-lap* nil)
(defvar *emit-function-p* t)

(defvar *optimize-cache-functions-p* t)

(defun emit-default-only (metatypes applyp)
  (unless *optimize-cache-functions-p*
    (when (and (null *precompiling-lap*) *emit-function-p*)
      (return-from emit-default-only
	(emit-default-only-function metatypes applyp))))
  (let* ((dlap-lambda-list (make-dlap-lambda-list metatypes applyp))
	 (args (remove '&rest dlap-lambda-list))
	 (restl (when applyp '(.lap-rest-arg.))))
    (generating-lisp '(emf)
		     dlap-lambda-list
      `(invoke-effective-method-function emf ,applyp ,@args ,@restl))))
      
;;; --------------------------------

(defun generating-lisp (closure-variables args form)
  (let* ((rest (memq '&rest args))
	 (ldiff (and rest (ldiff args rest)))
	 (args (if rest (append ldiff '(&rest .lap-rest-arg.)) args))
	 (lambda `(lambda ,closure-variables
		    ,@(when (member 'miss-fn closure-variables)
			`((declare (type function miss-fn))))
		    #'(kernel:instance-lambda ,args
			;;
			;; Don't ask me why LOCALLY is necessary here.
			;; Fact is that without it the resulting code is
			;; up ta 25% slower.  --gerd 2002-10-26.
			(locally (declare #.*optimize-speed*)
			  ,form)))))
    (values (if *precompiling-lap*
		`#',lambda
		(compile-lambda lambda))
	    nil)))

;;;
;;; cmu17 note: since std-instance-p is weakened, that branch may run
;;; on non-pcl instances (structures).  The result will be the
;;; non-wrapper layout for the structure, which will cause a miss.
;;; The "slots" will be whatever the first slot is, but will be
;;; ignored.  Similarly, fsc-instance-p returns true on funcallable
;;; structures as well as PCL fins.
;;;
(defun emit-reader/writer (reader/writer 1-or-2-class class-slot-p)
  (unless *optimize-cache-functions-p*
    (when (and (null *precompiling-lap*) *emit-function-p*)
      (return-from emit-reader/writer
	(emit-reader/writer-function reader/writer 1-or-2-class class-slot-p))))
  (let* ((instance
	  (ecase reader/writer
	    ((:reader :boundp) (dfun-arg-symbol 0))
	    (:writer (dfun-arg-symbol 1))))
	 (arglist
	  (ecase reader/writer
	    ((:reader :boundp) (list instance))
	    (:writer (list (dfun-arg-symbol 0) instance))))
	 (closure-variables
	  (ecase 1-or-2-class
	    (1 '(wrapper-0 index miss-fn))
	    (2 '(wrapper-0 wrapper-1 index miss-fn))))
	 (read-form
	  (emit-slot-read-form class-slot-p 'index 'slots)))
    (generating-lisp closure-variables arglist
       `(let* (,@(unless class-slot-p `((slots nil)))
	       (wrapper (cond ((std-instance-p ,instance)
			       ,@(unless class-slot-p
				   `((setq slots (std-instance-slots ,instance))))
			       (std-instance-wrapper ,instance))
			      ((fsc-instance-p ,instance)
			       ,@(unless class-slot-p
				   `((setq slots (fsc-instance-slots ,instance))))
			       (fsc-instance-wrapper ,instance)))))
	  (block access
	    (when (and wrapper
		       (not (zerop (kernel:layout-hash wrapper 0)))
		       ,@(if (eql 1 1-or-2-class)
			     `((eq wrapper wrapper-0))
			     `((or (eq wrapper wrapper-0)
				   (eq wrapper wrapper-1)))))
	      ,@(ecase reader/writer
		 (:reader
		  `((let ((value ,read-form))
		      (unless (eq value +slot-unbound+)
			(return-from access value)))))
		 (:boundp
		  `((let ((value ,read-form))
		      (return-from access (not (eq value +slot-unbound+))))))
		 (:writer
		  `((return-from access (setf ,read-form ,(car arglist)))))))
	    (funcall miss-fn ,@arglist))))))

(defun emit-slot-read-form (class-slot-p index slots)
  (if class-slot-p
      `(cdr ,index)
      `(%slot-ref ,slots ,index)))

(defun emit-boundp-check (value-form miss-fn arglist)
  `(let ((value ,value-form))
     (if (eq value +slot-unbound+)
	 (funcall ,miss-fn ,@arglist)
	 value)))

(defun emit-slot-access (reader/writer class-slot-p slots index miss-fn arglist)
  (let ((read-form (emit-slot-read-form class-slot-p index slots)))
    (ecase reader/writer
      (:reader (emit-boundp-check read-form miss-fn arglist))
      (:boundp `(not (eq +slot-unbound+ ,read-form)))
      (:writer `(setf ,read-form ,(car arglist))))))

(defmacro emit-reader/writer-macro (reader/writer 1-or-2-class class-slot-p)
  (let ((*emit-function-p* nil)
	(*precompiling-lap* t))
    (values 
     (emit-reader/writer reader/writer 1-or-2-class class-slot-p))))

(defun emit-one-or-n-index-reader/writer (reader/writer cached-index-p class-slot-p)
  (unless *optimize-cache-functions-p*
    (when (and (null *precompiling-lap*) *emit-function-p*)
      (return-from emit-one-or-n-index-reader/writer
	(emit-one-or-n-index-reader/writer-function
	 reader/writer cached-index-p class-slot-p))))
  (multiple-value-bind (arglist metatypes)
      (ecase reader/writer
	((:reader :boundp)
	 (values (list (dfun-arg-symbol 0))
		 '(standard-instance)))
	(:writer
	 (values (list (dfun-arg-symbol 0) (dfun-arg-symbol 1))
		 '(t standard-instance))))
    (generating-lisp `(cache ,@(unless cached-index-p '(index)) miss-fn)
		     arglist
      `(let (,@(unless class-slot-p '(slots))
	     ,@(when cached-index-p '(index)))
         ,(emit-dlap arglist metatypes
		     (emit-slot-access reader/writer class-slot-p
				       'slots 'index 'miss-fn arglist)
		     `(funcall miss-fn ,@arglist)
		     (when cached-index-p 'index)
		     (unless class-slot-p '(slots)))))))

(defmacro emit-one-or-n-index-reader/writer-macro
    (reader/writer cached-index-p class-slot-p)
  (let ((*emit-function-p* nil)
	(*precompiling-lap* t))
    (values
     (emit-one-or-n-index-reader/writer reader/writer cached-index-p
					class-slot-p))))

(defun emit-miss (miss-fn args &optional applyp)
  (let ((restl (when applyp '(.lap-rest-arg.))))
    (if restl
	`(apply ,miss-fn ,@args ,@restl)
	`(funcall ,miss-fn ,@args ,@restl))))

(defun emit-checking-or-caching (cached-emf-p return-value-p metatypes applyp)
  (unless *optimize-cache-functions-p*
    (when (and (null *precompiling-lap*) *emit-function-p*)
      (return-from emit-checking-or-caching
	(emit-checking-or-caching-function
	 cached-emf-p return-value-p metatypes applyp))))
  (let* ((dlap-lambda-list (make-dlap-lambda-list metatypes applyp))
	 (args (remove '&rest dlap-lambda-list))
	 (restl (when applyp '(.lap-rest-arg.))))
    (generating-lisp `(cache ,@(unless cached-emf-p '(emf)) miss-fn)
		     dlap-lambda-list
      `(let (,@(when cached-emf-p '(emf)))
         ,(emit-dlap args
	             metatypes
	             (if return-value-p
			 (if cached-emf-p 'emf t)
			 `(invoke-effective-method-function emf ,applyp
			   ,@args ,@restl))
	             (emit-miss 'miss-fn args applyp)
		     (when cached-emf-p 'emf))))))

(defun emit-dlap (args metatypes hit miss value-reg &optional slot-regs)
  (let* ((index -1)
	 (wrapper-bindings (mapcan (lambda (arg mt)
				     (unless (eq mt t)
				       (incf index)
				       `((,(intern (format nil "WRAPPER-~D" index)
						   *the-pcl-package*)
					  ,(emit-fetch-wrapper mt arg 'miss
							       (pop slot-regs))))))
				   args metatypes))
	 (wrappers (mapcar #'car wrapper-bindings)))
    (declare (fixnum index))
    (assert (not (null wrappers)) () "Every metatype is T.")
    `(block dfun
       (tagbody
	  (let ((field (cache-field cache))
		(cache-vector (cache-vector cache))
		(mask (cache-mask cache))
		(size (cache-size cache))
		(overflow (cache-overflow cache))
		,@wrapper-bindings)
	    (declare (fixnum size field mask))
	    ,(cond ((cdr wrappers)
		    (emit-greater-than-1-dlap wrappers 'miss value-reg))
		   (value-reg
		    (emit-1-t-dlap (car wrappers) 'miss value-reg))
		   (t
		    (emit-1-nil-dlap (car wrappers) 'miss)))
	    (return-from dfun ,hit))
	miss
	  (return-from dfun ,miss)))))

(defun emit-1-nil-dlap (wrapper miss-label)
  `(let* ((primary ,(emit-1-wrapper-compute-primary-cache-location wrapper miss-label))
	  (location primary))
     (declare (fixnum primary location))
     (block search
       (loop (when (eq ,wrapper (cache-vector-ref cache-vector location))
	       (return-from search nil))
	     (setq location (the fixnum (+ location 1)))
	     (when (= location size)
	       (setq location 0))
	     (when (= location primary)
	       (dolist (entry overflow)
		 (when (eq (car entry) ,wrapper)
		   (return-from search nil)))
	       (go ,miss-label))))))

(defmacro get-cache-vector-lock-count (cache-vector)
  `(let ((lock-count (cache-vector-lock-count ,cache-vector)))
     (the fixnum lock-count)))

(defun emit-1-t-dlap (wrapper miss-label value)
  `(let ((primary ,(emit-1-wrapper-compute-primary-cache-location wrapper miss-label))
	 (initial-lock-count (get-cache-vector-lock-count cache-vector)))
     (declare (fixnum primary initial-lock-count))
     (let ((location primary))
       (declare (fixnum location))
       (block search
	 (loop (when (eq ,wrapper (cache-vector-ref cache-vector location))
		 (setq ,value (cache-vector-ref cache-vector (1+ location)))
		 (return-from search nil))
	       (setq location (the fixnum (+ location 2)))
	       (when (= location size)
		 (setq location 0))
	       (when (= location primary)
		 (dolist (entry overflow)
		   (when (eq (car entry) ,wrapper)
		     (setq ,value (cdr entry))
		     (return-from search nil)))
		 (go ,miss-label))))
       (unless (= initial-lock-count
		  (get-cache-vector-lock-count cache-vector))
	 (go ,miss-label)))))

(defun emit-greater-than-1-dlap (wrappers miss-label value)
  (declare (type list wrappers))
  (let ((cache-line-size (compute-line-size (+ (length wrappers) (if value 1 0)))))
    `(let ((primary 0) (size-1 (the fixnum (- size 1))))
       (declare (fixnum primary size-1))
       ,(emit-n-wrapper-compute-primary-cache-location wrappers miss-label)
       (let ((initial-lock-count (get-cache-vector-lock-count cache-vector)))
	 (declare (fixnum initial-lock-count))
	 (let ((location primary))
	   (declare (fixnum location))
	   (block search
	     (loop
		(let ((next-location (the fixnum (+ location ,cache-line-size))))
		  (declare #.*optimize-speed*)
		  (declare (fixnum next-location))
		  (when (and ,@(mapcar
				(lambda (wrapper)
				  `(eq ,wrapper 
				       (%svref cache-vector
					       (setq location
						     (the fixnum (1+ location))))))
				wrappers))
		     ,@(when value
			 `((setq location (the fixnum (1+ location)))
			   (setq ,value (%svref cache-vector location))))
		     (return-from search nil))
		  (setq location next-location)
		  (when (= location size-1)
		    (setq location 0))
		  (when (= location primary)
		    (loop for (ws . v) in overflow
			  when (and ,@(mapcar (lambda (w)
						`(eq ,w (pop ws)))
					      wrappers)) do
			    ,@(when value `((setq ,value v)))
			    (return-from search nil))
		    (go ,miss-label)))))
	   (unless (= initial-lock-count
		      (get-cache-vector-lock-count cache-vector))
	     (go ,miss-label)))))))

(defun emit-1-wrapper-compute-primary-cache-location (wrapper miss-label)
  `(let ((wrapper-cache-no (kernel:layout-hash ,wrapper field)))
     (declare (fixnum wrapper-cache-no))
     (when (zerop wrapper-cache-no)
       (go ,miss-label))
     ,(let ((form `(logand mask wrapper-cache-no)))
	`(the fixnum ,form))))

#-pcl-xorhash
(defun emit-n-wrapper-compute-primary-cache-location (wrappers miss-label)
  (declare (type list wrappers))
  ;; this returns 1 less that the actual location
  `(progn
     ,@(let ((adds 0) (len (length wrappers)))
	 (declare (fixnum adds len))
	 (mapcar (lambda (wrapper)
		   `(let ((wrapper-cache-no (kernel:layout-hash ,wrapper field)))
		     (declare (fixnum wrapper-cache-no))
		     (when (zerop wrapper-cache-no) (go ,miss-label))
		     (setq primary (the fixnum (+ primary wrapper-cache-no)))
		     ,@(progn
			(incf adds)
			(when (or (zerop (mod adds +max-hash-code-additions+))
				  (eql adds len))
			  `((setq primary
			     ,(let ((form `(logand primary mask)))
				   `(the fixnum ,form))))))))
		 wrappers))))

#+pcl-xorhash
(defun emit-n-wrapper-compute-primary-cache-location (wrappers miss-label)
  (declare (type list wrappers))
  ;; this returns 1 less that the actual location
  `(progn
     ,@(let ((adds 0) (len (length wrappers)))
	 (declare (fixnum adds len))
	 (mapcar (lambda (wrapper)
		   `(let ((hash (kernel:layout-hash ,wrapper field)))
		     (declare (fixnum hash))
		     (when (zerop hash) (go ,miss-label))
		     (setq primary (logxor primary hash))
		     ,@(progn
			(incf adds)
			(when (eql adds len)
			  `((setq primary
			     ,(let ((form `(logand primary mask)))
				   `(the fixnum ,form))))))))
		 wrappers))))

;;;
;;; cmu17 note: since std-instance-p is weakened, that branch may run
;;; on non-pcl instances (structures).  The result will be the 
;;; non-wrapper layout for the structure, which will cause a miss.  The "slots"
;;; will be whatever the first slot is, but will be ignored.  Similarly,
;;; fsc-instance-p returns true on funcallable structures as well as PCL fins.
;;;
(defun emit-fetch-wrapper (metatype argument miss-label &optional slot)
  (ecase metatype
    ((standard-instance)
     `(cond ((std-instance-p ,argument)
	     ,@(when slot `((setq ,slot (std-instance-slots ,argument))))
	     (std-instance-wrapper ,argument))
	    ((fsc-instance-p ,argument)
	     ,@(when slot `((setq ,slot (fsc-instance-slots ,argument))))
	     (fsc-instance-wrapper ,argument))
	    (t
	     (go ,miss-label))))
    (class
     (assert (null slot) () "Can't do a slot reg for this metatype.")
     `(wrapper-of-macro ,argument))
    ((built-in-instance structure-instance)
     (assert (null slot) () "Can't do a slot reg for this metatype.")
     `(built-in-or-structure-wrapper ,argument))))

