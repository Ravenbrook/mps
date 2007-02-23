;;; -*- Mode: Lisp; Package: LISP; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/gc.lisp,v 1.42 2005/01/31 18:02:58 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Garbage collection and allocation related code.
;;;
;;; Written by Christopher Hoover, Rob MacLachlan, Dave McDonald, et al.
;;; New code for MIPS port by Christopher Hoover.
;;; 

(in-package "EXTENSIONS")
(export '(*before-gc-hooks* *after-gc-hooks* gc gc-on gc-off
	  *bytes-consed-between-gcs* *gc-verbose* *gc-inhibit-hook*
	  *gc-notify-before* *gc-notify-after* get-bytes-consed
	  *gc-run-time* bytes-consed-between-gcs
	  get-bytes-consed-dfixnum))

(in-package "LISP")
(export '(room))

#+gencgc
(sys:register-lisp-runtime-feature :gencgc)


;;;; DYNAMIC-USAGE and friends.

(declaim (special *read-only-space-free-pointer*
		  *static-space-free-pointer*))

(eval-when (compile eval)
  (defmacro c-var-frob (lisp-fun c-var-name)
    `(progn
       (declaim (inline ,lisp-fun))
       (defun ,lisp-fun ()
	 (alien:extern-alien ,c-var-name (alien:unsigned 32))))))

(c-var-frob read-only-space-start "read_only_space")
(c-var-frob static-space-start "static_space")
(c-var-frob dynamic-0-space-start "dynamic_0_space")
(c-var-frob dynamic-1-space-start "dynamic_1_space")
(c-var-frob control-stack-start "control_stack")
#+x86 (c-var-frob control-stack-end "control_stack_end")
(c-var-frob binding-stack-start "binding_stack")
(c-var-frob current-dynamic-space-start "current_dynamic_space")

(c-var-frob read-only-space-size "read_only_space_size")
(c-var-frob binding-stack-size "binding_stack_size")
(c-var-frob static-space-size "static_space_size")
(c-var-frob control-stack-size "control_stack_size")
(c-var-frob dynamic-space-size "dynamic_space_size")
(declaim (inline dynamic-usage))

#-(or cgc gencgc)
(defun dynamic-usage ()
  (the (unsigned-byte 32)
       (- (system:sap-int (c::dynamic-space-free-pointer))
	  (current-dynamic-space-start))))

;; #+(or cgc gencgc)
;; (c-var-frob dynamic-usage "bytes_allocated")

#+gencgc
(progn
  (alien:def-alien-routine get_bytes_allocated_lower c-call:int)
  (alien:def-alien-routine get_bytes_allocated_upper c-call:int)

  (defun dynamic-usage ()
    (dfixnum:dfixnum-pair-integer
     (get_bytes_allocated_upper) (get_bytes_allocated_lower))))

#+cgc
(c-var-frob dynamic-usage "bytes_allocated")

(defun static-space-usage ()
  (- (* lisp::*static-space-free-pointer* #-amd64 vm:word-bytes
	#+amd64 4) ; won't be necessary when amd64 uses 4-bit lowtag
     (static-space-start)))

(defun read-only-space-usage ()
  (- (* lisp::*read-only-space-free-pointer* #-amd64 vm:word-bytes
	#+amd64 4) ; won't be necessary when amd64 uses 4-bit lowtag
     (read-only-space-start)))

(defun control-stack-usage ()
#-(or x86 amd64) (- (system:sap-int (c::control-stack-pointer-sap)) (control-stack-start))
#+(or x86 amd64) (- (control-stack-end) (system:sap-int (c::control-stack-pointer-sap))) )

(defun binding-stack-usage ()
  (- (system:sap-int (c::binding-stack-pointer-sap)) (binding-stack-start)))


(defun current-dynamic-space ()
  (let ((start (current-dynamic-space-start)))
    (cond ((= start (dynamic-0-space-start))
	   0)
	  ((= start (dynamic-1-space-start))
	   1)
	  (t
	   (error "Oh no.  The current dynamic space is missing!")))))


;;;; Room.

(defun room-minimal-info ()
  (flet ((megabytes (bytes)
	   ;; Convert bytes to nearest megabyte
	   (ceiling bytes (* 1024 1024))))
    (format t "Dynamic Space Usage:    ~13:D bytes (out of ~4:D MB).~%"
	    (dynamic-usage) (megabytes (dynamic-space-size)))
    (format t "Read-Only Space Usage:  ~13:D bytes (out of ~4:D MB).~%"
	    (read-only-space-usage) (megabytes (read-only-space-size)))
    (format t "Static Space Usage:     ~13:D bytes (out of ~4:D MB).~%"
	    (static-space-usage) (megabytes (static-space-size)))
    (format t "Control Stack Usage:    ~13:D bytes (out of ~4:D MB).~%"
	    (control-stack-usage) (megabytes (control-stack-size)))
    (format t "Binding Stack Usage:    ~13:D bytes (out of ~4:D MB).~%"
	    (binding-stack-usage) (megabytes (binding-stack-size)))
    (format t "The current dynamic space is ~D.~%" (current-dynamic-space))
    (format t "Garbage collection is currently ~:[enabled~;DISABLED~].~%"
	    *gc-inhibit*)))

(defun room-intermediate-info ()
  (room-minimal-info)
  (vm:memory-usage :count-spaces '(:dynamic)
		   :print-spaces t
		   :cutoff 0.05s0
		   :print-summary nil))

(defun room-maximal-info ()
  (room-minimal-info)
  (vm:memory-usage :count-spaces '(:static :dynamic))
  (vm:instance-usage :dynamic :top-n 10)
  (vm:instance-usage :static :top-n 10))


(defun room (&optional (verbosity :default))
  "Prints to *STANDARD-OUTPUT* information about the state of internal
  storage and its management.  The optional argument controls the
  verbosity of ROOM.  If it is T, ROOM prints out a maximal amount of
  information.  If it is NIL, ROOM prints out a minimal amount of
  information.  If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information.  See also VM:MEMORY-USAGE and
  VM:INSTANCE-USAGE for finer report control."
  (fresh-line)
  (if (fboundp 'vm:memory-usage)
      (case verbosity
	((t)
	 (room-maximal-info))
	((nil)
	 (room-minimal-info))
	(:default
	 (room-intermediate-info))
	(t
	 (error "No way man!  The optional argument to ROOM must be T, NIL, ~
		 or :DEFAULT.~%What do you think you are doing?")))
      (room-minimal-info))
  (values))


;;;; GET-BYTES-CONSED.

;;;
;;; Internal State
;;; 
(defvar *last-bytes-in-use* nil)
(defvar *total-bytes-consed* (dfixnum:make-dfixnum))

(declaim (type (or (unsigned-byte 32) null) *last-bytes-in-use*))
(declaim (type dfixnum:dfixnum *total-bytes-consed*))

;;; GET-BYTES-CONSED -- Exported
;;; 
#+(or cgc gencgc)
(defun get-bytes-consed-dfixnum ()
  ;(declare (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  (cond ((null *last-bytes-in-use*)
	 (pushnew
	  #'(lambda ()
	      (print "resetting GC counters")
	      (force-output)
	      (setf *last-bytes-in-use* nil)
	      (setf *total-bytes-consed* (dfixnum:make-dfixnum)))
	  ext:*before-save-initializations*)
	 (setf *last-bytes-in-use* (dynamic-usage))
	 (dfixnum:dfixnum-set-from-number *total-bytes-consed* 0))
	(t
	 (let* ((bytes (dynamic-usage))
		(incbytes (- bytes *last-bytes-in-use*)))
	   (if (< incbytes dfixnum::dfmax)
	       (dfixnum:dfixnum-inc-hf *total-bytes-consed* incbytes)
	     (dfixnum:dfixnum-inc-df
	      *total-bytes-consed*
	      ;; Kinda fixme - we cons, but it doesn't matter if we consed
	      ;; more than 250 Megabyte *within* this measuring period anyway.
	      (let ((df (dfixnum:make-dfixnum)))
		(dfixnum:dfixnum-set-from-number df incbytes)
		df)))
	   (setq *last-bytes-in-use* bytes))))
  *total-bytes-consed*)

#-(or cgc gencgc)
(defun get-bytes-consed-dfixnum ()
  "Returns the number of bytes consed since the first time this function
  was called.  The first time it is called, it returns zero."
  (declare (optimize (speed 3) (safety 0)(inhibit-warnings 3)))
  (cond ((null *last-bytes-in-use*)
         (setq *last-bytes-in-use* (dynamic-usage))
         (setq *total-bytes-consed* (dfixnum:make-dfixnum)))
        (t
         (let ((bytes (dynamic-usage)))
           (dfixnum:dfixnum-inc-hf *total-bytes-consed*
			    (the index (- bytes *last-bytes-in-use*)))
           (setq *last-bytes-in-use* bytes))))
  *total-bytes-consed*)

(defun get-bytes-consed ()
  "Returns the number of bytes consed since the first time this function
  was called.  The first time it is called, it returns zero."
  (dfixnum:dfixnum-integer (get-bytes-consed-dfixnum)))
    

;;;; Variables and Constants.

;;; The default value of *BYTES-CONSED-BETWEEN-GCS* and *GC-TRIGGER*.
;;; 
(defconstant default-bytes-consed-between-gcs 12000000)

;;; This variable is the user-settable variable that specifices the
;;; minimum amount of dynamic space which must be consed before a GC
;;; will be triggered.
;;; 
(defparameter *bytes-consed-between-gcs* default-bytes-consed-between-gcs
  "This number specifies the minimum number of bytes of dynamic space
   that must be consed before the next gc will occur.")
;;;
(declaim (type index *bytes-consed-between-gcs*))

;;; Public
(defvar *gc-run-time* 0
  "The total CPU time spend doing garbage collection (as reported by
   GET-INTERNAL-RUN-TIME.)")

(declaim (type index *gc-run-time*))

;;; Internal trigger.  When the dynamic usage increases beyond this
;;; amount, the system notes that a garbage collection needs to occur by
;;; setting *NEED-TO-COLLECT-GARBAGE* to T.  It starts out as NIL meaning
;;; nobody has figured out what it should be yet.
;;; 
(defvar *gc-trigger* nil)

(declaim (type (or (unsigned-byte 32) null) *gc-trigger*))

;;; On the RT, we store the GC trigger in a ``static'' symbol instead of
;;; letting magic C code handle it.  It gets initialized by the startup
;;; code. The X86 port defines this here because it uses the `ibmrt'
;;; feature in the C code for allocation and binding stack access and
;;; a lot of stuff wants this INTERNAL_GC_TRIGGER available as well.
#+(or ibmrt x86)
(defvar vm::*internal-gc-trigger*)

;;;
;;; The following specials are used to control when garbage collection
;;; occurs.
;;; 

;;; 
;;; *GC-INHIBIT*
;;;
;;; When non-NIL, inhibits garbage collection.
;;; 
(defvar *gc-inhibit* nil)

;;;
;;; *ALREADY-MAYBE-GCING*
;;;
;;; This flag is used to prevent recursive entry into the garbage
;;; collector.
;;; 
(defvar *already-maybe-gcing* nil)

;;; When T, indicates that the dynamic usage has exceeded the value
;;; *GC-TRIGGER*.
;;; 
(defvar *need-to-collect-garbage* nil)


;;;; GC Hooks.

;;;
;;; *BEFORE-GC-HOOKS*
;;; *AFTER-GC-HOOKS*
;;;
;;; These variables are a list of functions which are run before and
;;; after garbage collection occurs.
;;;
(defvar *before-gc-hooks* nil
  "A list of functions that are called before garbage collection occurs.
  The functions should take no arguments.")
;;; 
(defvar *after-gc-hooks* nil
  "A list of functions that are called after garbage collection occurs.
  The functions should take no arguments.")

;;;
;;; *GC-INHIBIT-HOOK*
;;; 
;;; This hook is invoked whenever SUB-GC intends to GC (unless the GC
;;; was explicitly forced by calling EXT:GC).  If the hook function
;;; returns NIL then the GC procedes; otherwise, the GC is inhibited and
;;; *GC-INHIBIT* and *NEED-TO-COLLECT-GARBAGE* are left bound to T.
;;; Presumably someone will call GC-ON later to collect the garbage.
;;;
(defvar *gc-inhibit-hook* nil
  "Should be bound to a function or NIL.  If it is a function, this
  function should take one argument, the current amount of dynamic
  usage.  The function should return NIL if garbage collection should
  continue and non-NIL if it should be inhibited.  Use with caution.")



;;;
;;; *GC-VERBOSE*
;;;
(defvar *gc-verbose* t
  "When non-NIL, causes the functions bound to *GC-NOTIFY-BEFORE* and
  *GC-NOTIFY-AFTER* to be called before and after a garbage collection
  occurs respectively.  If :BEEP, causes the default notify functions to beep
  annoyingly.")


(defun default-gc-notify-before (bytes-in-use)
  (when (eq *gc-verbose* :beep)
    (system:beep *standard-output*))
  (format t "~&; [GC threshold exceeded with ~:D bytes in use.  ~
             Commencing GC.]~%" bytes-in-use)
  (finish-output))
;;;
(defparameter *gc-notify-before* #'default-gc-notify-before
  "This function bound to this variable is invoked before GC'ing (unless
  *GC-VERBOSE* is NIL) with the current amount of dynamic usage (in
  bytes).  It should notify the user that the system is going to GC.")

(defun default-gc-notify-after (bytes-retained bytes-freed new-trigger)
  (format t "~&; [GC completed with ~:D bytes retained and ~:D bytes freed.]~%"
	  bytes-retained bytes-freed)
  (format t "~&; [GC will next occur when at least ~:D bytes are in use.]~%"
	  new-trigger)
  (when (eq *gc-verbose* :beep)
    (system:beep *standard-output*))
  (finish-output))
;;;
(defparameter *gc-notify-after* #'default-gc-notify-after
  "The function bound to this variable is invoked after GC'ing (unless
  *GC-VERBOSE* is NIL) with the amount of dynamic usage (in bytes) now
  free, the number of bytes freed by the GC, and the new GC trigger
  threshold.  The function should notify the user that the system has
  finished GC'ing.")


;;;; Internal GC

(alien:def-alien-routine collect-garbage c-call:int
  #+gencgc (last-gen c-call:int))

#-ibmrt
(alien:def-alien-routine set-auto-gc-trigger c-call:void
  (dynamic-usage c-call:unsigned-long))

#+ibmrt
(defun set-auto-gc-trigger (bytes)
  (let ((words (ash (+ (current-dynamic-space-start) bytes) -2)))
    (unless (and (fixnump words) (plusp words))
      (clear-auto-gc-trigger)
      (warn "Attempt to set GC trigger to something bogus: ~S" bytes))
    (setf rt::*internal-gc-trigger* words)))

#-ibmrt
(alien:def-alien-routine clear-auto-gc-trigger c-call:void)

#+ibmrt
(defun clear-auto-gc-trigger ()
  (setf rt::*internal-gc-trigger* -1))

;;;
;;; *INTERNAL-GC*
;;;
;;; This variables contains the function that does the real GC.  This is
;;; for low-level GC experimentation.  Do not touch it if you do not
;;; know what you are doing.
;;; 
(defvar *internal-gc* #'collect-garbage)


;;;; SUB-GC

;;;
;;; CAREFULLY-FUNCALL -- Internal
;;;
;;; Used to carefully invoke hooks.
;;; 
(defmacro carefully-funcall (function &rest args)
  `(handler-case (funcall ,function ,@args)
     (error (cond)
       (warn "(FUNCALL ~S~{ ~S~}) lost:~%~A" ',function ',args cond)
       nil)))

;;;
;;; SUB-GC -- Internal
;;;
;;; SUB-GC decides when and if to do a garbage collection.  The
;;; VERBOSE-P flag controls whether or not the notify functions are
;;; called.  The FORCE-P flags controls if a GC should occur even if the
;;; dynamic usage is not greater than *GC-TRIGGER*.
;;; 
;;; For GENCGC all generations < GEN will be GC'ed.
;;;
(defun sub-gc (&key (verbose-p *gc-verbose*) force-p #+gencgc (gen 0))
  (unless *already-maybe-gcing*
    (let* ((*already-maybe-gcing* t)
	   (start-time (get-internal-run-time))
	   (pre-gc-dyn-usage (dynamic-usage)))
      (unless (integerp (symbol-value '*bytes-consed-between-gcs*))
	;; The noise w/ symbol-value above is to keep the compiler from
	;; optimizing the test away because of the type declaim for
	;; *bytes-consed-between-gcs*.
	(warn "The value of *BYTES-CONSED-BETWEEN-GCS*, ~S, is not an ~
	       integer.  Resetting it to ~D." *bytes-consed-between-gcs*
	       default-bytes-consed-between-gcs)
	(setf *bytes-consed-between-gcs* default-bytes-consed-between-gcs))
      (when (and *gc-trigger* (>= pre-gc-dyn-usage *gc-trigger*))
	(setf *need-to-collect-garbage* t))
      (when (or force-p
		(and *need-to-collect-garbage* (not *gc-inhibit*)))
	(when (and (not force-p)
		   *gc-inhibit-hook*
		   (carefully-funcall *gc-inhibit-hook* pre-gc-dyn-usage))
	  (setf *gc-inhibit* t)
	  (return-from sub-gc nil))
	(without-interrupts
	  (let ((*standard-output* *terminal-io*))
	    (when verbose-p
	      (carefully-funcall *gc-notify-before* pre-gc-dyn-usage))
	    (dolist (hook *before-gc-hooks*)
	      (carefully-funcall hook))
	    (when *gc-trigger*
	      (clear-auto-gc-trigger))
	    #-gencgc (funcall *internal-gc*)
	    #+gencgc (if (eq *internal-gc* #'collect-garbage)
			 (funcall *internal-gc* gen)
                         (funcall *internal-gc*))
	    (let* ((post-gc-dyn-usage (dynamic-usage))
		   (bytes-freed (- pre-gc-dyn-usage post-gc-dyn-usage)))
	      (when *last-bytes-in-use*
		#+nil
		(when verbose-p
		  (format
		   t "~&Adjusting *last-bytes-in-use* from ~:D to ~:D, gen ~d, pre ~:D ~%"
		   *last-bytes-in-use*
		   post-gc-dyn-usage
		   gen
		   pre-gc-dyn-usage)
		  (force-output))
		(let ((correction (- pre-gc-dyn-usage *last-bytes-in-use*)))
		  (if (<= correction dfixnum::dfmax)
		      (dfixnum:dfixnum-inc-hf *total-bytes-consed* correction)
		      ;; give up on not consing
		      (dfixnum:dfixnum-inc-integer *total-bytes-consed*
						   correction)))
		(setq *last-bytes-in-use* post-gc-dyn-usage))
	      (setf *need-to-collect-garbage* nil)
	      (setf *gc-trigger*
		    (+ post-gc-dyn-usage *bytes-consed-between-gcs*))
	      (set-auto-gc-trigger *gc-trigger*)
	      (dolist (hook *after-gc-hooks*)
		(carefully-funcall hook))
	      (when verbose-p
		(carefully-funcall *gc-notify-after*
				   post-gc-dyn-usage bytes-freed
				   *gc-trigger*))))
	  (scrub-control-stack)))
      (incf *gc-run-time* (- (get-internal-run-time) start-time))))
  nil)

;;;
;;; MAYBE-GC -- Internal
;;; 
;;; This routine is called by the allocation miscops to decide if a GC
;;; should occur.  The argument, object, is the newly allocated object
;;; which must be returned to the caller.
;;; 
(defun maybe-gc (&optional object)
  (sub-gc)
  object)

;;;
;;; GC -- Exported
;;;
;;; This is the user advertised garbage collection function.
;;; 
#-gencgc
(defun gc (&optional (verbose-p *gc-verbose*))
  "Initiates a garbage collection.  The optional argument, VERBOSE-P,
  which defaults to the value of the variable *GC-VERBOSE* controls
  whether or not GC statistics are printed."
  (sub-gc :verbose-p verbose-p :force-p t))
;;;
#+gencgc
(defun gc (&key (verbose *gc-verbose*) (gen 0) (full nil))
  "Initiates a garbage collection.  The keyword :VERBOSE, which
   defaults to the value of the variable *GC-VERBOSE* controls whether or
   not GC statistics are printed. The keyword :GEN defaults to 0, and
   controls the number of generations to garbage collect."
  (sub-gc :verbose-p verbose :force-p t :gen (if full 6 gen)))


;;;; Auxiliary Functions.

(defun bytes-consed-between-gcs ()
  "Return the amount of memory that will be allocated before the next garbage
   collection is initiated.  This can be set with SETF."
  *bytes-consed-between-gcs*)
;;;
(defun %set-bytes-consed-between-gcs (val)
  (declare (type index val))
  (let ((old *bytes-consed-between-gcs*))
    (setf *bytes-consed-between-gcs* val)
    (when *gc-trigger*
      (setf *gc-trigger* (+ *gc-trigger* (- val old)))
      (cond ((<= (dynamic-usage) *gc-trigger*)
	     (clear-auto-gc-trigger)
	     (set-auto-gc-trigger *gc-trigger*))
	    (t
	     (system:scrub-control-stack)
	     (sub-gc)))))
  val)
;;;
(defsetf bytes-consed-between-gcs %set-bytes-consed-between-gcs)


(defun gc-on ()
  "Enables the garbage collector."
  (setq *gc-inhibit* nil)
  (when *need-to-collect-garbage*
    (sub-gc))
  nil)

(defun gc-off ()
  "Disables the garbage collector."
  (setq *gc-inhibit* t)
  nil)



;;;; Initialization stuff.

(defun gc-init ()
  (when *gc-trigger*
    (if (< *gc-trigger* (dynamic-usage))
	(sub-gc)
	(set-auto-gc-trigger *gc-trigger*))))

;;; setters and accessors for gencgc parameters

#+gencgc
(eval-when (load eval)
(alien:def-alien-type nil
  (alien:struct generation-stats
    (bytes-allocated c-call:int)
    (gc-trigger c-call:int)
    (bytes-consed-between-gc c-call:int)
    (num-gc c-call:int)
    (trigger-age c-call:int)
    (cum-sum-bytes-allocated c-call:int)
    (min-av-mem-age c-call:double)))

(defun gencgc-stats (generation)
  "Return some GC statistics for the specified GENERATION.  The
  statistics are the number of bytes allocated in this generation; the
  gc-trigger; the number of bytes consed between GCs; the number of
  GCs that have occurred; the trigger age; the cumulative number of
  bytes allocated in this generation; and the average age of this
  generation.  See the gencgc source code for more info."
  (alien:with-alien ((stats (alien:struct generation-stats)))
    (alien:alien-funcall (alien:extern-alien "get_generation_stats"
					     (function c-call:void
						       c-call:int
						       (* (alien:struct
							   generation-stats))))
			 generation
			 (alien:addr stats))
    (values (alien:slot stats 'bytes-allocated)
	    (alien:slot stats 'gc-trigger)
	    (alien:slot stats 'bytes-consed-between-gc)
	    (alien:slot stats 'num-gc)
	    (alien:slot stats 'trigger-age)
	    (alien:slot stats 'cum-sum-bytes-allocated)
	    (alien:slot stats 'min-av-mem-age))))

(alien:def-alien-routine set-gc-trigger c-call:void
			 (gen c-call:int) (trigger c-call:int))
(alien:def-alien-routine set-trigger-age c-call:void
			 (gen c-call:int) (trigger-age c-call:int))
(alien:def-alien-routine set-min-mem-age c-call:void
			 (gen c-call:int) (min-mem-age c-call:double))
)
