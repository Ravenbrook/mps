;;; -*- Mode: Lisp; Package: Multiprocessing -*-
;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/multi-proc.lisp,v 1.43 2005/07/05 13:12:50 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stack-group and multi-process support for CMUCL x86.
;;;

(in-package "MULTIPROCESSING")

(sys:register-lisp-runtime-feature :mp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Handle the binding stack.

;;; Undo all the bindings in the bind stack, restoring the global
;;; values.
(defun unbind-binding-stack ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (kernel:binding-stack-pointer-sap))
	 (binding-stack 
	  (sys:int-sap (alien:extern-alien "binding_stack" alien:unsigned)))
	 (size (sys:sap- binding-stack-pointer binding-stack)))
    (declare (type (unsigned-byte 29) size))
    (do ((binding size))
	((zerop binding))
      (declare (type (unsigned-byte 29) binding))
      (decf binding 8)
      (let* ((value 
	      (kernel:make-lisp-obj
	       (sys:sap-int (sys:sap-ref-sap binding-stack binding))))
	     (symbol
	      (kernel:make-lisp-obj
	       (sys:sap-int (sys:sap-ref-sap binding-stack (+ binding 4))))))
	(cond ((symbolp symbol)
	       (let ((symbol-value (c::%primitive c:fast-symbol-value symbol)))
		 #+nil
		 (format t "Undoing: ~s ~s <-> ~s~%" symbol value symbol-value)
		 (kernel:%set-symbol-value symbol value)
		 (setf (sys:sap-ref-sap binding-stack binding)
		       (sys:int-sap (kernel:get-lisp-obj-address
				     symbol-value)))))
	      (t
	       #+nil
	       (format t "Ignoring undoing: ~s ~s~%" symbol value)))))))

;;; Re-apply the bindings in a binding stack after an
;;; unbind-binding-stack.
(defun rebind-binding-stack ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (kernel:binding-stack-pointer-sap))
	 (binding-stack 
	  (sys:int-sap (alien:extern-alien "binding_stack" alien:unsigned)))
	 (size (sys:sap- binding-stack-pointer binding-stack)))
    (declare (type (unsigned-byte 29) size))
    (do ((binding 0 (+ 8 binding)))
	((= binding size))
      (declare (type (unsigned-byte 29) binding))
      (let* ((value 
	      (kernel:make-lisp-obj
	       (sys:sap-int (sys:sap-ref-sap binding-stack binding))))
	     (symbol
	      (kernel:make-lisp-obj
	       (sys:sap-int (sys:sap-ref-sap binding-stack (+ binding 4))))))
	(cond ((symbolp symbol)
	       (let ((symbol-value (c::%primitive c:fast-symbol-value symbol)))
		 #+nil
		 (format t "Rebinding: ~s ~s <-> ~s~%"
			 symbol value symbol-value)
		 (kernel:%set-symbol-value symbol value)
		 (setf (sys:sap-ref-sap binding-stack binding)
		       (sys:int-sap (kernel:get-lisp-obj-address
				     symbol-value)))))
	      (t
	       #+nil
	       (format t "Ignoring rebinding: ~s ~s~%" symbol value)))))))

(defun save-binding-stack (binding-save-stack)
  (declare (type (simple-array t (*)) binding-save-stack)
	   (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (kernel:binding-stack-pointer-sap))
	 (binding-stack 
	  (sys:int-sap (alien:extern-alien "binding_stack" alien:unsigned)))
	 (size (sys:sap- binding-stack-pointer binding-stack))
	 (vector-size (truncate size 4)))
    (declare (type (unsigned-byte 29) size))
    ;; Grow binding-save-stack if necessary.
    (when (< (length binding-save-stack) vector-size)
      (setq binding-save-stack
	    (adjust-array binding-save-stack vector-size :element-type t)))
    ;; Save the stack.
    (do ((binding 0 (+ 4 binding))
	 (index 0 (1+ index)))
	((= binding size))
      (declare (type (unsigned-byte 29) binding index))
      (setf (aref binding-save-stack index)
	    (kernel:make-lisp-obj
	     (sys:sap-int (sys:sap-ref-sap binding-stack binding)))))
    (values binding-save-stack vector-size)))

(defun restore-binding-stack (new-binding-stack size)
  (declare (type (simple-array t (*)) new-binding-stack)
	   (type (unsigned-byte 29) size)
	   (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-size (* size 4))
	 (binding-stack (alien:extern-alien "binding_stack" alien:unsigned)))
    (declare (type (unsigned-byte 32) binding-stack-size binding-stack))
    (setf (kernel:binding-stack-pointer-sap)
	  (sys:int-sap (+ binding-stack binding-stack-size)))
    (do ((binding 0 (+ 4 binding))
	 (index 0 (1+ index)))
	((= binding binding-stack-size))
      (declare (type (unsigned-byte 29) binding index))
      (setf (sys:sap-ref-sap (sys:int-sap binding-stack) binding)
	    (sys:int-sap (kernel:get-lisp-obj-address
			  (aref new-binding-stack index))))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Alien-stack

;;; The Top of the Alien-stack.
(declaim (type (unsigned-byte 32) *alien-stack-top*))
(defvar *alien-stack-top* 0)

;;; Save the alien-stack.
(defun save-alien-stack (save-stack)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-stack)
	   (optimize (speed 3) (safety 0)))
  (let* ((alien-stack (kernel:get-lisp-obj-address x86::*alien-stack*))
	 (size (- *alien-stack-top* alien-stack))
	 (vector-size (ceiling size 4)))
    (declare (type (unsigned-byte 32) alien-stack)
	     (type (unsigned-byte 29) size))
    #+nil
    (format t "alien-stack ~x; size ~x~%" alien-stack size)
    ;; Grow save-stack if necessary.
    (when (< (length save-stack) vector-size)
      (setq save-stack
	    (adjust-array save-stack vector-size
			  :element-type '(unsigned-byte 32))))
    ;; Save the stack.
    (do ((index 0 (1+ index)))
	((>= index vector-size))
      (declare (type (unsigned-byte 29) index))
      (setf (aref save-stack index)
	    (sys:sap-ref-32 (sys:int-sap *alien-stack-top*)
			    (* 4 (- (1+ index))))))
    (values save-stack vector-size alien-stack)))

(defun restore-alien-stack (save-stack size alien-stack)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-stack)
	   (type (unsigned-byte 29) size)
	   (type (unsigned-byte 32) alien-stack)
	   (optimize (speed 3) (safety 0)))
  (setf x86::*alien-stack* (kernel:make-lisp-obj alien-stack))
  (do ((index 0 (1+ index)))
      ((>= index size))
    (declare (type (unsigned-byte 29) index))
    (setf (sys:sap-ref-32 (sys:int-sap *alien-stack-top*) (* 4 (- (1+ index))))
	  (aref save-stack index)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Interrupt contexts.

;;; Save the interrupt contexts.
(defun save-interrupt-contexts (save-vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-vector)
	   (optimize (speed 3) (safety 0)))
  (let* ((size lisp::*free-interrupt-context-index*))
    (declare (type (unsigned-byte 29) size))
    ;; Grow save-stack if necessary.
    (when (< (length save-vector) size)
      (setq save-vector
	    (adjust-array save-vector size :element-type '(unsigned-byte 32))))
    (alien:with-alien
	((lisp-interrupt-contexts (array alien:unsigned nil) :extern))
      (dotimes (index size)
	(setf (aref save-vector index)
	      (alien:deref lisp-interrupt-contexts index))))
    save-vector))

;;; Restore the interrupt contexts.
(defun restore-interrupt-contexts (save-vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-vector)
	   (optimize (speed 3) (safety 0)))
  (let* ((size lisp::*free-interrupt-context-index*))
    (declare (type (unsigned-byte 29) size))
    (alien:with-alien
	((lisp-interrupt-contexts (array alien:unsigned nil) :extern))
      (dotimes (index size)
	(setf (alien:deref lisp-interrupt-contexts index)
	      (aref save-vector index)))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

;;; The control stacks need special handling on the x86 as they
;;; contain conservative roots. When placed in the *control-stacks*
;;; vector they will be scavenged for conservative roots by the
;;; garbage collector.
(declaim (type (simple-array (or null (simple-array (unsigned-byte 32) (*)))
			     (*)) x86::*control-stacks*))
(defvar x86::*control-stacks*
  (make-array 0 :element-type '(or null (unsigned-byte 32))
	      :initial-element nil))

;;; Stack-group structure.
(defstruct (stack-group
	     (:constructor %make-stack-group)
	     (:print-function
	      (lambda (stack-group stream depth)
		(declare (type stack-group stack-group)
			 (stream stream)
			 (ignore depth))
		(print-unreadable-object (stack-group stream :identity t)
		 (format stream "Stack-group ~a, ~a"
			 (stack-group-name stack-group)
			 (stack-group-state stack-group))))))
  ;; Must have a name.
  (name "Anonymous" :type simple-base-string)
  ;; State: :active or :inactive.
  (state :inactive :type (member :active :inactive))
  ;; The control stack; an index into *control-stacks*.
  (control-stack-id nil :type (or kernel:index null))
  ;; Binding stack.
  (binding-stack nil :type (or (simple-array t (*)) null))
  ;; Twice the number of bindings.
  (binding-stack-size 0 :type (unsigned-byte 29))
  ;; Current catch block, on the control stack.
  (current-catch-block 0 :type fixnum)
  ;; Unwind protect block, on the control stack.
  (current-unwind-protect-block 0 :type fixnum)
  ;; Alien stack
  (alien-stack nil :type (or (simple-array (unsigned-byte 32) (*)) null))
  (alien-stack-size 0 :type (unsigned-byte 29))
  (alien-stack-pointer 0 :type (unsigned-byte 32))
  ;; Eval-stack
  (eval-stack nil :type (or (simple-array t (*)) null))
  (eval-stack-top 0 :type fixnum)
  ;;
  ;; Interrupt contexts
  (interrupt-contexts nil :type (or (simple-array (unsigned-byte 32) (*))
				    null))
  ;; Resumer
  (resumer nil :type (or stack-group null)))

;;; The current stack group.
(declaim (type (or stack-group null) *current-stack-group*))
(defvar *current-stack-group* nil)

(declaim (type (or stack-group null) *initial-stack-group*))
(defvar *initial-stack-group* nil)

;;; Process defstruct is up here because stack group functions refer
;;; to process slots in assertions, but are also compiled at high
;;; optimization... so if the process structure changes, all hell
;;; could break loose.

(defstruct (process
	     (:constructor %make-process)
	     (:predicate processp)
	     (:print-function
	      (lambda (process stream depth)
		(declare (type process process) (stream stream) (ignore depth))
		(print-unreadable-object (process stream :identity t)
		 (format stream "Process ~a" (process-name process))))))
  (name "Anonymous" :type simple-base-string)
  (state :killed :type (member :killed :active :inactive))
  (%whostate nil :type (or null simple-base-string))
  (initial-function nil :type (or null function))
  (initial-args nil :type list)
  (wait-function nil :type (or null function))
  (wait-function-args nil :type list)
  (%run-reasons nil :type list)
  (%arrest-reasons nil :type list)
  ;; The real time after which the wait will timeout.
  (wait-timeout nil :type (or null double-float))
  (wait-return-value nil :type t)
  (interrupts '() :type list)
  (stack-group nil :type (or null stack-group))
  ;;
  ;; The real and run times when the current process was last
  ;; scheduled or yielded.
  (scheduled-real-time (get-real-time) :type double-float)
  (scheduled-run-time (get-run-time) :type double-float)
  ;;
  ;; Accrued real and run times in seconds.
  (%real-time 0d0 :type double-float)
  (%run-time 0d0 :type double-float)
  (property-list nil :type list)
  (initial-bindings nil :type list))


;;; Init-Stack-Groups -- Interface
;;;
;;; Setup the initial stack group.
;;;
(defun init-stack-groups ()
  ;; Grab the top of the alien-stack; it's currently stored at the top
  ;; of the control stack.
  (setf *alien-stack-top*
	(sys:sap-ref-32
	 (sys:int-sap (alien:extern-alien "control_stack_end" alien:unsigned))
	 -4))
  ;; Initialise the *control-stacks* vector.
  (setq x86::*control-stacks*
	(make-array 10 :element-type '(or null (unsigned-byte 32))
		    :initial-element nil))
  ;; Setup a control-stack for the initial stack-group.
  (setf (aref x86::*control-stacks* 0)
	(make-array 0 :element-type '(unsigned-byte 32)
		    :initial-element 0))
  ;; Make and return the initial stack group.
  (setf *current-stack-group*
	(%make-stack-group
	 :name "Initial"
	 :state :active
	 :control-stack-id 0
	 :binding-stack #()
	 :alien-stack (make-array 0 :element-type '(unsigned-byte 32))
	 :interrupt-contexts (make-array 0 :element-type '(unsigned-byte 32))
	 :eval-stack #()))
  (setf *initial-stack-group* *current-stack-group*))

;;; Inactivate-Stack-Group -- Internal
;;;
;;; Inactivate the stack group, cleaning its slot and freeing the
;;; control stack.
;;;
(defun inactivate-stack-group (stack-group)
  (declare (type stack-group stack-group))
  (setf (stack-group-state stack-group) :inactive)
  (let ((cs-id (stack-group-control-stack-id stack-group)))
    (when (and cs-id (aref x86::*control-stacks* cs-id))
      (setf (aref x86::*control-stacks* cs-id) nil)))
  (setf (stack-group-control-stack-id stack-group) nil)
  (setf (stack-group-binding-stack stack-group) nil)
  (setf (stack-group-binding-stack-size stack-group) 0)
  (setf (stack-group-current-catch-block stack-group) 0)
  (setf (stack-group-current-unwind-protect-block stack-group) 0)
  (setf (stack-group-alien-stack stack-group) nil)
  (setf (stack-group-alien-stack-size stack-group) 0)
  (setf (stack-group-alien-stack-pointer stack-group) 0)
  (setf (stack-group-eval-stack stack-group) nil)
  (setf (stack-group-eval-stack-top stack-group) 0)
  (setf (stack-group-resumer stack-group) nil))

;;; Scrub-Stack-Group-Stacks -- Internal
;;;
;;; Scrub the binding and eval stack of the give stack-group.
;;;
(defun scrub-stack-group-stacks (stack-group)
  (declare (type stack-group stack-group)
	   (optimize (speed 3) (safety 0)))
  ;; Binding stack.
  (let ((binding-save-stack (stack-group-binding-stack stack-group)))
    (when binding-save-stack
      (let ((size
	     ;; The stored binding stack for the current stack group
	     ;; can be completely scrubbed.
	     (if (eq stack-group *current-stack-group*)
		 0
		 (stack-group-binding-stack-size stack-group)))
	    (len (length binding-save-stack)))
	;; Scrub the remainder of the binding stack.
	(do ((index size (+ index 1)))
	    ((>= index len))
	  (declare (type (unsigned-byte 29) index))
	  (setf (aref binding-save-stack index) 0)))))
  ;; If this is the current stack group then update the stored
  ;; eval-stack and eval-stack-top before scrubbing.
  (when (eq stack-group *current-stack-group*)
    ;; Updare the stored vector, flushing an old vector if a new one
    ;; has been allocated.
    (setf (stack-group-eval-stack stack-group) lisp::*eval-stack*)
    ;; Ensure that the stack-top is valid.
    (setf (stack-group-eval-stack-top stack-group) lisp::*eval-stack-top*))
  ;; Scrub the eval stack.
  (let ((eval-stack (stack-group-eval-stack stack-group)))
    (when eval-stack
      (let ((eval-stack-top (stack-group-eval-stack-top stack-group))
	    (len (length eval-stack)))
	(do ((i eval-stack-top (1+ i)))
	    ((= i len))
	  (declare (type kernel:index i))
	  (setf (svref eval-stack i) nil))))))

;;; Initial-binding-stack  --  Internal
;;;
;;; Generate the initial bindings for a newly created stack-group.
;;; This function may be redefined to return a vector with other bindings
;;; but *interrupts-enabled* and *gc-inhibit* must be the last two.
;;;
(defun initial-binding-stack ()
  (vector
   (find-package "COMMON-LISP-USER") '*package*
   ;; Other bindings may be added here.
   nil 'unix::*interrupts-enabled*
   t 'lisp::*gc-inhibit*))

;;; Make-Stack-Group -- Interface
;;;
;;; Fork a new stack-group from the *current-stack-group*. Execution
;;; continues with the *current-stack-group* returning the new stack
;;; group. Control may be transfer to the child by stack-group-resume
;;; and it executes the initial-function.
;;;
(defun make-stack-group (name initial-function &optional
			      (resumer *current-stack-group*)
			      (inherit t))
  (declare (type simple-base-string name)
	   (type function initial-function)
	   (type stack-group resumer))
  (flet ((allocate-control-stack ()
	   (let* (;; Allocate a new control-stack ID.
		  (control-stack-id (position nil x86::*control-stacks*))
		  ;; Find the required stack size.
		  (control-stack-end
		   (alien:extern-alien "control_stack_end" alien:unsigned))
		  (control-stack-pointer (kernel:control-stack-pointer-sap))
		  (control-stack-size
		   (- control-stack-end (sys:sap-int control-stack-pointer)))
		  ;; Saved control stack needs three extra words. The
		  ;; stack pointer will be stored in the first
		  ;; element, and the frame pointer and return address
		  ;; push onto the bottom of the stack.
		  (control-stack
		   (make-array (+ (ceiling control-stack-size 4) 3)
			       :element-type '(unsigned-byte 32)
			       :initial-element 0)))
	     (declare (type (unsigned-byte 29) control-stack-size))
	     (unless control-stack-id
	       ;; Need to extend the *control-stacks* vector.
	       (setq control-stack-id (length x86::*control-stacks*))
	       (setq x86::*control-stacks*
		     (adjust-array x86::*control-stacks*
				   (* 2 (length x86::*control-stacks*))
				   :element-type '(or null (unsigned-byte 32))
				   :initial-element nil)))
	     (setf (aref x86::*control-stacks* control-stack-id) control-stack)
	     (values control-stack control-stack-id)))
	 ;; Allocate a stack group inheriting stacks and bindings from
	 ;; the current stack group.
	 (allocate-child-stack-group (control-stack-id)
	   ;; Save the interrupt-contexts while the size is still
	   ;; bound.
	   (let ((interrupt-contexts
		  (save-interrupt-contexts
		   (make-array 0 :element-type '(unsigned-byte 32)))))
	     ;; Save the binding stack.  Note that
	     ;; *interrutps-enabled* could be briefly set during the
	     ;; unbinding and re-binding process so signals are
	     ;; blocked.
	     (let ((old-sigs (unix:unix-sigblock
			      (unix:sigmask :sigint :sigalrm))))
	       (declare (type (unsigned-byte 32) old-sigs))
	       (unbind-binding-stack)
	       (multiple-value-bind (binding-stack binding-stack-size)
		   (save-binding-stack #())
		 (rebind-binding-stack)
		 (unix:unix-sigsetmask old-sigs)
		 ;; Save the Alien stack
		 (multiple-value-bind (alien-stack alien-stack-size
						   alien-stack-pointer)
		     (save-alien-stack
		      (make-array 0 :element-type '(unsigned-byte 32)))
		   ;; Allocate a stack-group structure.
		   (%make-stack-group
		    :name name
		    :state :active
		    :control-stack-id control-stack-id
		    ;; Save the Eval stack.
		    :eval-stack (copy-seq (the simple-vector
					       kernel:*eval-stack*))
		    :eval-stack-top kernel:*eval-stack-top*
		    ;; Misc stacks.
		    :current-catch-block lisp::*current-catch-block*
		    :current-unwind-protect-block
		    lisp::*current-unwind-protect-block*
		    ;; Alien stack.
		    :alien-stack alien-stack
		    :alien-stack-size alien-stack-size
		    :alien-stack-pointer alien-stack-pointer
		    ;; Interrupt contexts
		    :interrupt-contexts interrupt-contexts
		    ;; Binding stack.
		    :binding-stack binding-stack
		    :binding-stack-size binding-stack-size
		    ;; Resumer
		    :resumer resumer))))))
	 ;; Allocate a new stack group with fresh stacks and bindings.
	 (allocate-new-stack-group (control-stack-id)
	   (let ((binding-stack (initial-binding-stack)))
	     ;; Allocate a stack-group structure.
	     (%make-stack-group
	      :name name
	      :state :active
	      :control-stack-id control-stack-id
	      ;; Eval stack. Needs at least one element be because
	      ;; push doubles the size when full.
	      :eval-stack (make-array 32)
	      :eval-stack-top 0
	      ;; Misc stacks.
	      :current-catch-block 0
	      :current-unwind-protect-block 0
	      ;; Alien stack.
	      :alien-stack (make-array 0 :element-type '(unsigned-byte 32))
	      :alien-stack-size 0
	      :alien-stack-pointer *alien-stack-top*
	      ;; Interrupt contexts
	      :interrupt-contexts (make-array 0 :element-type
					      '(unsigned-byte 32))
	      ;; Binding stack - some initial bindings.
	      :binding-stack binding-stack
	      :binding-stack-size (length binding-stack)
	      ;; Resumer
	      :resumer resumer))))
    (let ((child-stack-group nil))
      (let ((unix::*interrupts-enabled* nil)
	    (lisp::*gc-inhibit* t))
	(multiple-value-bind (control-stack control-stack-id)
	    (allocate-control-stack)
	  (setq child-stack-group
		(if inherit
		    (allocate-child-stack-group control-stack-id)
		    (allocate-new-stack-group control-stack-id)))
	  ;; Fork the control-stack
	  (if (x86:control-stack-fork control-stack inherit)
	      ;; Current-stack-group returns the child-stack-group.
	      child-stack-group
	      ;; Child starts.
	      (unwind-protect
		   (progn
		     (setq *current-stack-group* child-stack-group)
		     (assert (eq *current-stack-group*
				 (process-stack-group *current-process*)))
		     ;; Enable interrupts and GC.
		     (setf unix::*interrupts-enabled* t)
		     (setf lisp::*gc-inhibit* nil)
		     (when unix::*interrupt-pending*
		       (unix::do-pending-interrupt))
		     (when lisp::*need-to-collect-garbage*
		       (lisp::maybe-gc))
		     (funcall initial-function))
		(let ((resumer (stack-group-resumer child-stack-group)))
		  ;; Disable interrupts and GC.
		  (setf unix::*interrupts-enabled* nil)
		  (setf lisp::*gc-inhibit* t)
		  (inactivate-stack-group child-stack-group)
		  ;; Verify the resumer.
		  (unless (and resumer
			       (eq (stack-group-state resumer) :active))
		    (format t "*Resuming stack-group ~s instead of ~s~%"
			    *initial-stack-group* resumer)
		    (setq resumer *initial-stack-group*))
		  ;; Restore the resumer state.
		  (setq *current-stack-group* resumer)
		  ;; Eval-stack
		  (setf kernel:*eval-stack* (stack-group-eval-stack resumer))
		  (setf kernel:*eval-stack-top*
			(stack-group-eval-stack-top resumer))
		  ;; The binding stack.  Note that
		  ;; *interrutps-enabled* could be briefly set during
		  ;; the unbinding and re-binding process so signals
		  ;; are blocked.
		  (let ((old-sigs (unix:unix-sigblock
				   (unix:sigmask :sigint :sigalrm))))
		    (declare (type (unsigned-byte 32) old-sigs))
		    (unbind-binding-stack)
		    (restore-binding-stack
		     (stack-group-binding-stack resumer)
		     (stack-group-binding-stack-size resumer))
		    (rebind-binding-stack)
		    (unix:unix-sigsetmask old-sigs))
		  ;; Misc stacks.
		  (setf lisp::*current-catch-block*
			(stack-group-current-catch-block resumer))
		  (setf lisp::*current-unwind-protect-block*
			(stack-group-current-unwind-protect-block resumer))
		  ;; The Alien stack
		  (restore-alien-stack
		   (stack-group-alien-stack resumer)
		   (stack-group-alien-stack-size resumer)
		   (stack-group-alien-stack-pointer resumer))
		  ;; Interrupt-contexts.
		  (restore-interrupt-contexts
		   (stack-group-interrupt-contexts resumer))
		  ;; 
		  (let ((new-control-stack
			 (aref x86::*control-stacks*
			       (stack-group-control-stack-id resumer))))
		    (declare (type (simple-array (unsigned-byte 32) (*))
				   new-control-stack))
		    (x86:control-stack-return new-control-stack)))))))
      (when (and unix::*interrupts-enabled* unix::*interrupt-pending*)
	(unix::do-pending-interrupt))
      (when (and lisp::*need-to-collect-garbage* (not lisp::*gc-inhibit*))
	(lisp::maybe-gc))
      child-stack-group)))


;;; Stack-Group-Resume -- Interface
;;;
;;; Transfer control to the given stack-group, resuming its execution,
;;; and saving the *current-stack-group*.
;;;
(defun stack-group-resume (new-stack-group)
  (declare (type stack-group new-stack-group)
	   (optimize (speed 3)))
  (assert (and (eq (stack-group-state new-stack-group) :active)
	       (not (eq new-stack-group *current-stack-group*))))
  (assert (eq new-stack-group (process-stack-group *current-process*)))
  (let ((unix::*interrupts-enabled* nil)
	(lisp::*gc-inhibit* t))
    (let* (;; Save the current stack-group on its stack.
	   (stack-group *current-stack-group*)
	   ;; Find the required stack size.
	   (control-stack-end
	    (alien:extern-alien "control_stack_end" alien:unsigned))
	   (control-stack-pointer (kernel:control-stack-pointer-sap))
	   (control-stack-size (- control-stack-end
				  (sys:sap-int control-stack-pointer)))
	   ;; Stack-save array needs three extra elements. The stack
	   ;; pointer will be stored in the first, and the frame
	   ;; pointer and return address push onto the bottom of the
	   ;; stack.
	   (save-stack-size (+ (ceiling control-stack-size 4) 3))
	   ;; The save-stack vector.
	   (control-stack (aref x86::*control-stacks*
				(stack-group-control-stack-id stack-group))))
      (declare (type (unsigned-byte 29) control-stack-size save-stack-size)
	       (type (simple-array (unsigned-byte 32) (*)) control-stack))
      ;; Increase the save-stack size if necessary.
      (when (> save-stack-size (length control-stack))
	(setf control-stack (adjust-array control-stack save-stack-size
					  :element-type '(unsigned-byte 32)
					  :initial-element 0))
	(setf (aref x86::*control-stacks*
		    (stack-group-control-stack-id stack-group))
	      control-stack))

      ;; Eval-stack
      (setf (stack-group-eval-stack stack-group) kernel:*eval-stack*)
      (setf (stack-group-eval-stack-top stack-group) kernel:*eval-stack-top*)
      (setf kernel:*eval-stack* (stack-group-eval-stack new-stack-group))
      (setf kernel:*eval-stack-top*
	    (stack-group-eval-stack-top new-stack-group))
      
      ;; Misc stacks.
      (setf (stack-group-current-catch-block stack-group)
	    lisp::*current-catch-block*)
      (setf (stack-group-current-unwind-protect-block stack-group)
	    lisp::*current-unwind-protect-block*)
      (setf lisp::*current-catch-block*
	    (stack-group-current-catch-block new-stack-group))
      (setf lisp::*current-unwind-protect-block*
	    (stack-group-current-unwind-protect-block new-stack-group))
      
      ;; Save the interrupt-contexts.
      (setf (stack-group-interrupt-contexts stack-group)
	    (save-interrupt-contexts
	     (stack-group-interrupt-contexts stack-group)))

      ;; The binding stack.  Note that *interrutps-enabled* could be
      ;; briefly set during the unbinding and re-binding process so
      ;; signals are blocked.
      (let ((old-sigs (unix:unix-sigblock (unix:sigmask :sigint :sigalrm))))
	(declare (type (unsigned-byte 32) old-sigs))
	(unbind-binding-stack)
	(multiple-value-bind (stack size)
	    (save-binding-stack (stack-group-binding-stack stack-group))
	  (setf (stack-group-binding-stack stack-group) stack)
	  (setf (stack-group-binding-stack-size stack-group) size))
	(restore-binding-stack (stack-group-binding-stack new-stack-group)
			       (stack-group-binding-stack-size
				new-stack-group))
	(rebind-binding-stack)
	(unix:unix-sigsetmask old-sigs))
      
      ;; Restore the interrupt-contexts.
      (restore-interrupt-contexts
       (stack-group-interrupt-contexts new-stack-group))

      ;; The Alien stack
      (multiple-value-bind (save-stack size alien-stack)
	  (save-alien-stack (stack-group-alien-stack stack-group))
	(setf (stack-group-alien-stack stack-group) save-stack)
	(setf (stack-group-alien-stack-size stack-group) size)
	(setf (stack-group-alien-stack-pointer stack-group) alien-stack))
      (restore-alien-stack (stack-group-alien-stack new-stack-group)
			   (stack-group-alien-stack-size new-stack-group)
			   (stack-group-alien-stack-pointer new-stack-group))
      ;; 
      (let ((new-control-stack
	     (aref x86::*control-stacks*
		   (stack-group-control-stack-id new-stack-group))))
	(declare (type (simple-array (unsigned-byte 32) (*))
		       new-control-stack))
	(x86:control-stack-resume control-stack new-control-stack))
      ;; Thread returns.
      (setq *current-stack-group* stack-group)))
  (assert (eq *current-stack-group* (process-stack-group *current-process*)))
  (when (and unix::*interrupts-enabled* unix::*interrupt-pending*)
    (unix::do-pending-interrupt))
  (when (and lisp::*need-to-collect-garbage* (not lisp::*gc-inhibit*))
    (lisp::maybe-gc))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Double-float timing functions for use by the scheduler.

;;; These timer functions use double-floats for accuracy. In most
;;; cases consing is avoided.

;;; Get-Real-Time
;;;
(declaim (inline get-real-time))
;;;
(defun get-real-time ()
  "Return the real time in seconds."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ignore seconds useconds)
      (unix:unix-gettimeofday)
    (declare (ignore ignore)
	     (type (unsigned-byte 32) seconds useconds))
    (+ (coerce seconds 'double-float)
       (* (coerce useconds 'double-float) 1d-6))))

;;; Get-Run-Time
;;;
(declaim (inline get-run-time))
;;;
(defun get-run-time ()
  "Return the run time in seconds"
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ignore utime-sec utime-usec stime-sec stime-usec)
      (unix:unix-fast-getrusage unix:rusage_self)
    (declare (ignore ignore)
	     (type (unsigned-byte 31) utime-sec stime-sec)
	     (type (mod 1000000) utime-usec stime-usec))
    (+ (coerce utime-sec 'double-float) (coerce stime-sec 'double-float)
       (* (+ (coerce utime-usec 'double-float)
	     (coerce stime-usec 'double-float))
	  1d-6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Multi-process support. The interface is based roughly on the
;;;; CLIM-SYS spec. and support needed for cl-http.

(defvar *multi-processing* t)

;;; Process-Whostate  --  Public
;;;
(defun process-whostate (process)
  "Return the process state which is either Run, Killed, or a wait reason."
  (cond ((eq (process-state process) :killed)
	 "Killed")
	((process-wait-function process)
	 (or (process-%whostate process) "Run"))
	(t
	 "Run")))

;;; Process-Active-P  --  Public
;;;
(declaim (inline process-active-p))
(defun process-active-p (process)
  (and (eq (process-state process) :active)
       (process-%run-reasons process)
       (not (process-%arrest-reasons process))))

;;; Process-Alive-P  --  Public
;;;
(declaim (inline process-alive-p))
(defun process-alive-p (process)
  (let ((state (process-state process)))
    (or (eq state :active) (eq state :inactive))))

;;; A dummy initial process is defined so that locks will work before
;;; multi-processing has been started.
(declaim (type process *current-process*))
(defvar *current-process*
  (%make-process :name "Startup" :state :inactive :stack-group nil))

;;; Current-Process  --  Public
;;;
(declaim (inline current-process))
(defun current-process ()
  "Returns the current process."
  *current-process*)

(declaim (list *all-processes*))
(defvar *all-processes* nil
  "A list of all alive processes.")

;;; All-Processes  --  Public
;;;
(declaim (inline all-processes))
(defun all-processes ()
  "Return a list of all the live processes."
  *all-processes*)

(declaim (type (or null process) *initial-process*))
(defvar *initial-process* nil)

;;; Without-scheduling  --  Public
;;;
;;; Disable scheduling while the body is executed. Scheduling is
;;; typically inhibited when process state is being modified.
;;;
(defvar *inhibit-scheduling* t)
;;;
(defmacro without-scheduling (&body body)
  "Execute the body the scheduling disabled."
  `(let ((inhibit *inhibit-scheduling*))
    (unwind-protect
	 (progn
	   (setf *inhibit-scheduling* t)
	   ,@body)
      (setf *inhibit-scheduling* inhibit))))

(defmacro atomic-incf (reference &optional (delta 1))
  "Increaments the reference by delta in a single atomic operation"
  `(without-scheduling
    (incf ,reference ,delta)))

(defmacro atomic-decf (reference &optional (delta 1))
  "Decrements the reference by delta in a single atomic operation"
  `(without-scheduling
    (decf ,reference ,delta)))

(defmacro atomic-push (obj place)
  "Atomically push object onto place."
  `(without-scheduling
    (push ,obj ,place)))

(defmacro atomic-pop (place)
  "Atomically pop place."
  `(without-scheduling
    (pop ,place)))

;;; If a process other than the initial process throws to the
;;; %end-of-the-world then *quitting-lisp* is set to the exit value,
;;; after which further process creation blocks. If the initial
;;; process is running the idle loop then it will perform the exit
;;; when it runs.
;;;
(defvar *quitting-lisp* nil)

;;; Update-Process-Timers -- Internal
;;;
;;; Update the processes times for the current and new process before
;;; a process switch.
;;;
(defun update-process-timers (current-process new-process)
  (declare (type process current-process new-process)
	   (optimize (speed 3) (safety 0)))
  (let ((real-time (get-real-time)))
    (incf (process-%real-time current-process)
	  (- real-time (process-scheduled-real-time current-process)))
    (setf (process-scheduled-real-time current-process) real-time)
    (setf (process-scheduled-real-time new-process) real-time))
  (let ((run-time (get-run-time)))
    (incf (process-%run-time current-process)
	  (- run-time (process-scheduled-run-time current-process)))
    (setf (process-scheduled-run-time current-process) run-time)
    (setf (process-scheduled-run-time new-process) run-time))
  (values))

(defun apply-with-bindings (function args bindings)
  (if bindings
      (progv
	  (mapcar #'car bindings)
	  (mapcar #'(lambda (binding)
		      (eval (cdr binding)))
		  bindings)
	(apply function args))
      (apply function args)))


;;; Make-Process -- Public
;;;
(defun make-process (function &key
		     (name "Anonymous")
		     (run-reasons (list :enable))
		     (arrest-reasons nil)
		     (initial-bindings nil))
  "Make a process which will run FUNCTION when it starts up.  By
  default the process is created in a runnable (active) state.
  If FUNCTION is NIL, the process is started in a killed state; it may
  be restarted later with process-preset.

  :NAME
	A name for the process displayed in process listings.

  :RUN-REASONS
	Initial value for process-run-reasons; defaults to (:ENABLE).  A
	process needs a at least one run reason to be runnable.  Together with
	arrest reasons, run reasons provide an alternative to process-wait for
	controling whether or not a process is runnable.  To get the default
	behavior of MAKE-PROCESS in Allegro Common Lisp, which is to create a
	process which is active but not runnable, initialize RUN-REASONS to
	NIL.

  :ARREST-REASONS
	Initial value for process-arrest-reasons; defaults to NIL.  A
	process must have no arrest reasons in order to be runnable.

  :INITIAL-BINDINGS
	An alist of initial special bindings for the process.  At
	startup the new process has a fresh set of special bindings
	with a default binding of *package* setup to the CL-USER
	package.  INITIAL-BINDINGS specifies additional bindings for
	the process.  The cdr of each alist element is evaluated in
	the fresh dynamic environment and then bound to the car of the
	element."
  (declare (type (or null function) function))
  (cond (*quitting-lisp*
	 ;; No more processes if about to quit lisp.
	 (process-wait "Quitting Lisp" #'(lambda () nil)))
	((null function)
	 ;; If function is nil then create a dead process; can be
	 ;; restarted with process-preset.
	 (%make-process :initial-function nil :name name :state	:killed
			:%run-reasons run-reasons
			:%arrest-reasons arrest-reasons
			:initial-bindings initial-bindings))
	(t
	 ;; Create a stack-group.
	 (let ((process
		(%make-process
		 :name name
		 :state :active 
		 :initial-function function
		 :%run-reasons run-reasons
		 :%arrest-reasons arrest-reasons
		 :initial-bindings initial-bindings
		 :stack-group
		 (make-stack-group
		  name 
		  #'(lambda ()
		      (unwind-protect
			   (catch '%end-of-the-process
			     ;; Catch throws to the %end-of-the-world.
			     (setf *quitting-lisp*
				   (catch 'lisp::%end-of-the-world
				     (with-simple-restart
					 (destroy "Destroy the process")
				       (setf *inhibit-scheduling* nil)
				       (apply-with-bindings function
							    nil
							    initial-bindings))
				     ;; Normal exit.
				     (throw '%end-of-the-process nil))))
			(setf *inhibit-scheduling* t)
			;; About to return to the resumer's
			;; stack-group, which in this case is the
			;; initial process's stack-group.
			(setf (process-state *current-process*) :killed)
			(setf *all-processes*
			      (delete *current-process* *all-processes*))
			(setf (process-%whostate *current-process*) nil)
			(setf (process-%run-reasons *current-process*) nil)
			(setf (process-%arrest-reasons *current-process*) nil)
			(setf (process-wait-function-args *current-process*)
			      nil)
			(setf (process-wait-function *current-process*) nil)
			(setf (process-wait-timeout *current-process*) nil)
			(setf (process-wait-return-value *current-process*)
			      nil)
			(setf (process-interrupts *current-process*) nil)
			(update-process-timers *current-process*
					       *initial-process*)
			(setf *current-process* *initial-process*)))
		  *initial-stack-group* nil))))
	   (atomic-push process *all-processes*)
	   process))))

(defun process-run-reasons (process)
  (process-%run-reasons process))

(defun process-add-run-reason (process object)
  (atomic-push object (process-%run-reasons process)))

(defun process-revoke-run-reason (process object)
  (let ((run-reasons (without-scheduling
		      (setf (process-%run-reasons process)
			    (delete object (process-%run-reasons process))))))
      (when (and (null run-reasons) (eq process mp::*current-process*))
	(process-yield))))


(defun process-arrest-reasons (process)
  (process-%arrest-reasons process))

(defun process-add-arrest-reason (process object)
  (atomic-push object (process-%arrest-reasons process))
  (when (eq process mp::*current-process*)
    (process-yield)))

(defun process-revoke-arrest-reason (process object)
  (without-scheduling
    (setf (process-%arrest-reasons process)
	  (delete object (process-%arrest-reasons process)))))

;;; Process-Interrupt  --  Public
;;;
(defun process-interrupt (process function)
  "Interrupt process and cause it to evaluate function."
  ;; Place the interrupt function at the end of process's interrupts
  ;; queue, to be called the next time the process is scheduled.
  (without-scheduling
   (setf (process-interrupts process)
	 (append (list function) (process-interrupts process))))
  (process-yield))


;;; Destroy-Process  --  Public
;;;
(defun destroy-process (process)
  "Destroy a process. The process is sent a interrupt which throws to
  the end of the process allowing it to unwind gracefully."
  (declare (type process process))
  (assert (not (eq process *current-process*)))
  (without-scheduling
   (unless (eq (process-state process) :killed)
     ;; Place a throw to end-of-the-world at the start of process's
     ;; interrupts queue, to be called the next time the process is
     ;; scheduled.
     (push #'(lambda ()
	       (throw '%end-of-the-process nil))
	   (process-interrupts process))
     ;; Ensure that the process is active so that it can accept this
     ;; interrupt.
     (setf (process-state process) :active)))
  ;; Should we wait until it's dead?
  (process-yield))

(defun restart-process (process)
  "Restart process by unwinding it to its initial state and calling its
  initial function."
  (destroy-process process)
  (if *inhibit-scheduling*		;Called inside without-scheduling?
      (assert (eq (process-state process) :killed))
      (process-wait "Waiting for process to die" 
		#'(lambda ()
		    (eq (process-state process) :killed))))
  ;; No more processes if about to quit lisp.
  (when *quitting-lisp*
    (process-wait "Quitting Lisp" #'(lambda () nil)))
  ;; Create a new stack-group.
  (without-scheduling
   (setf (process-stack-group process)
	 (make-stack-group
	  (process-name process)
	  #'(lambda ()
	      (unwind-protect 
		   (catch '%end-of-the-process
		     ;; Catch throws to the %end-of-the-world.
		     (setf *quitting-lisp*
			   (catch 'lisp::%end-of-the-world
			     (with-simple-restart
				 (destroy "Destroy the process")
			       (setf *inhibit-scheduling* nil)
			       (apply-with-bindings
				(process-initial-function process)
				(process-initial-args process)
				(process-initial-bindings process)))
			     ;; Normal exit.
			     (throw '%end-of-the-process nil))))
		(setf *inhibit-scheduling* t)
		;; About to return to the resumer's stack-group, which
		;; in this case is the initial process's stack-group.
		(setf (process-state *current-process*) :killed)
		(setf *all-processes*
		      (delete *current-process* *all-processes*))
		(setf (process-%whostate *current-process*) nil)
		(setf (process-%run-reasons *current-process*) nil)
		(setf (process-%arrest-reasons *current-process*) nil)
		(setf (process-wait-function-args *current-process*)
			      nil)
		(setf (process-wait-function *current-process*) nil)
		(setf (process-wait-timeout *current-process*) nil)
		(setf (process-wait-return-value *current-process*) nil)
		(setf (process-interrupts *current-process*) nil)
		(update-process-timers *current-process* *initial-process*)
		(setf *current-process* *initial-process*)))
	  *initial-stack-group* nil))
   (setf (process-%whostate process) nil)
   (setf (process-wait-function-args process) nil)
   (setf (process-wait-function process) nil)
   (setf (process-wait-timeout process) nil)
   (setf (process-wait-return-value process) nil)
   (setf (process-interrupts process) nil)
   (setf (process-scheduled-real-time process) (get-real-time))
   (setf (process-scheduled-run-time process) (get-run-time))
   (setf (process-%real-time process) 0d0)
   (setf (process-%run-time process) 0d0)
   (setf (process-state process) :active)
   (push process *all-processes*))
  process)


;;; Process-Preset
(defun process-preset (process function &rest args)
  "Restart process, unwinding it to its initial state and calls
  function with args."
  (setf (process-initial-function process) function)
  (setf (process-initial-args process) args)
  (restart-process process))


;;; Disable-Process  --  Public
;;;
(defun disable-process (process)
  "Disable process from being runnable until enabled."
  (without-scheduling
   (assert (not (eq (process-state process) :killed)))
   (setf (process-state process) :inactive)))

;;; Enable-Process  --  Public
;;;
(defun enable-process (process)
  "Allow process to become runnable again after it has been disabled."
  (without-scheduling
   (assert (not (eq (process-state process) :killed)))
   (setf (process-state process) :active)))

;;; Process-Wait  --  Public.
;;;
(defun process-wait (whostate predicate &rest args)
  "Causes the process to wait until predicate returns True. Processes
  can only call process-wait when scheduling is enabled, and the predicate
  can not call process-wait. Since the predicate may be evaluated may
  times by the scheduler it should be relative fast native compiled code.
  The single True predicate value is returned."
  (assert (not *inhibit-scheduling*))
  (assert (not (process-wait-function *current-process*)))
  ;; Don't need the disable scheduling here because the scheduler
  ;; doesn't mess with the whostate or timeout until the function is
  ;; setup, unless the process is interrupted in which case the
  ;; scheduler restores the state when execution resumers here.
  (setf (process-%whostate *current-process*) whostate)
  (setf (process-wait-timeout *current-process*) nil)
  (setf (process-wait-function-args *current-process*) args)
  (setf (process-wait-function *current-process*) predicate)
  (process-yield)
  (process-wait-return-value *current-process*))

;;; Process-Wait-With-Timeout  --  Public
;;;
(defun process-wait-with-timeout (whostate timeout predicate &rest args)
  (declare (type (or fixnum float) timeout))
  "Causes the process to wait until predicate returns True, or the
  number of seconds specified by timeout has elapsed. The timeout may
  be a fixnum or a float in seconds.  The single True predicate value is
  returned, or NIL if the timeout was reached."
  (assert (not *inhibit-scheduling*))
  (assert (not (process-wait-function *current-process*)))
  ;; Don't need the disable scheduling here because the scheduler
  ;; doesn't mess with the whostate or timeout until the function is
  ;; setup, unless the process is interrupted in which case the
  ;; scheduler restores the state when execution resumers here.
  (setf (process-%whostate *current-process*) whostate)
  (let ((timeout (etypecase timeout
		   (fixnum
		    (coerce timeout 'double-float))
		   (single-float
		    (coerce timeout 'double-float))
		   (double-float
		    (coerce timeout 'double-float)))))
    (declare (double-float timeout))
    (setf (process-wait-timeout *current-process*)
	  (+ timeout (get-real-time)))
    (setf (process-wait-function-args *current-process*) args)
    (setf (process-wait-function *current-process*) predicate))
  (process-yield)
  (process-wait-return-value *current-process*))

;;; The remaining processes in the scheduling queue for this cycle,
;;; the remainder of *all-processes*. The *current-process* is the
;;; first element of this list.
(defvar *remaining-processes* nil)

;;; The idle process will only run when there are no other runnable
;;; processes.
(defvar *idle-process* nil)

;;; Run-Idle-Process-P  --  Internal.
;;;
;;; Decide when to allow the idle process to run.
;;;
(defun run-idle-process-p ()
  ;; Check if there are any other runnable processes.
  (dolist (process *all-processes* t)
    (when (and (not (eq process *idle-process*))
	       (process-active-p process)
	       (not (process-wait-function process)))
      (return nil))))

;;; Shutdown-multi-processing  --  Internal.
;;;
(defun shutdown-multi-processing ()
  "Try to gracefully destroy all the processes giving them some
  chance to unwinding, before shutting down multi-processing. This is
  currently necessary before a purify and is performed before a save-lisp.
  Multi-processing can be restarted by calling init-multi-processing."
  (when *initial-process*
    (assert (eq *current-process* *initial-process*) ()
	    "Only the *initial-process* can shutdown multi-processing")

    (let ((destroyed-processes nil))
      (do ((cnt 0 (1+ cnt)))
	  ((> cnt 10))
	(declare (type kernel:index cnt))
	(dolist (process *all-processes*)
	  (when (and (not (eq process *current-process*))
		     (process-active-p process)
		     (not (member process destroyed-processes)))
	    (destroy-process process)
	    (push process destroyed-processes)))
	(unless (rest *all-processes*)
	  (return))
	(format t "Destroyed ~d process~:P; remaining ~d~%"
		(length destroyed-processes) (length *all-processes*))
	(process-yield)))

    (start-sigalrm-yield 0 0)	; Off with the interrupts.
    ;; Reset the multi-processing state.
    (setf *inhibit-scheduling* t)
    (setf *initial-process* nil)
    (setf *idle-process* nil)
    (setf *current-process*
	  (%make-process :name "Startup" :state :inactive :stack-group nil))
    (setf *all-processes* nil)
    (setf *remaining-processes* nil)
    ;; Cleanup the stack groups.
    (setf x86::*control-stacks*
	  (make-array 0 :element-type '(or null (unsigned-byte 32))
		      :initial-element nil))
    (setf *current-stack-group* nil)
    (setf *initial-stack-group* nil)))

;;; Idle-Process-Loop  --  Internal
;;;
;;; A useful idle process loop, waiting on events using the select
;;; based event server, which is assumed to be setup to call
;;; process-yielding periodically.
;;;
(declaim (double-float *idle-loop-timeout*))
(defvar *idle-loop-timeout* 0.1d0)
;;;
(defun idle-process-loop ()
  "An idle loop to be run by the initial process. The select based event
  server is called with a timeout calculated from the minimum of the
  *idle-loop-timeout* and the time to the next process wait timeout.
  To avoid this delay when there are runnable processes the *idle-process*
  should be setup to the *initial-process*. If one of the processes quits
  by throwing to %end-of-the-world then *quitting-lisp* will have been
  set to the exit value which is noted by the idle loop which tries to
  exit gracefully destroying all the processes and giving them a chance
  to unwind."
  (declare (optimize (speed 3)))
  (assert (eq *current-process* *initial-process*) ()
	  "Only the *initial-process* is intended to run the idle loop")
  ;; Ensure the *idle-process* is setup.
  (unless *idle-process*
    (setf *idle-process* *current-process*))
  ;; Adjust the process name.
  (setf (process-name *current-process*) "Idle Loop")
  (do ()
      (*quitting-lisp*)
    ;; Calculate the wait period.
    (let ((real-time (get-real-time))
	  (timeout *idle-loop-timeout*))
      (declare (double-float timeout))
      (dolist (process *all-processes*)
	(when (process-active-p process)
	  (let ((wait-timeout (process-wait-timeout process)))
	    (when wait-timeout
	      (let ((delta (- wait-timeout real-time)))
		(when (< delta timeout)
		  (x86::double-float-reg-bias timeout)
		  (setf timeout delta)))))))
      (when (> timeout 1d-5)
	(sys:serve-all-events timeout))
      (process-yield)))
  (shutdown-multi-processing)
  (throw 'lisp::%end-of-the-world *quitting-lisp*))

;;; Process-Yield  --  Public
;;;
;;; The Scheduler.
;;;
(defun process-yield ()
  (declare (optimize (speed 3)))
  "Allow other processes to run."
  (unless *inhibit-scheduling*
    ;; Catch any FP exceptions before entering the scheduler.
    (kernel:float-wait)
    ;; Inhibit recursive entry of the scheduler.
    (setf *inhibit-scheduling* t)
    (assert (eq (first *remaining-processes*) *current-process*))
    (assert (eq *current-stack-group* (process-stack-group *current-process*)))
    (loop
     ;; Rotate the queue.
     (setf *remaining-processes*
	   (or (rest *remaining-processes*) *all-processes*))
     
     (let ((next (first *remaining-processes*)))
       ;; Shouldn't see any :killed porcesses here.
       (assert (process-alive-p next))
       
       (cond
	 ;; New process at the head of the queue?
	 ((eq next *current-process*))
	 ;; Ignore inactive processes.
	 ((not (process-active-p next)))
	 ;; If the next process has pending interrupts then return to
	 ;; it to execute these.
	 ((process-interrupts next)
	  (update-process-timers *current-process* next)
	  (setf *current-process* next)
	  (stack-group-resume (process-stack-group next)))
	 (t
	  ;; If not waiting then return.
	  (let ((wait-fn (process-wait-function next))
		(wait-fn-args (process-wait-function-args next)))
	    (cond
	      ((null wait-fn)
	       ;; Skip the idle process if there are other runnable
	       ;; processes.
	       (when (or (not (eq next *idle-process*))
			 (run-idle-process-p))
		 (update-process-timers *current-process* next)
		 (setf *current-process* next)
		 (stack-group-resume (process-stack-group next))))
	      (t
	       ;; Check the wait function in the current context
	       ;; saving a stack-group switch; although
	       ;; *current-process* is setup.
	       (let ((current-process *current-process*))
		 (setf *current-process* next)
		 ;; Predicate true?
		 (let ((wait-return-value (apply wait-fn wait-fn-args)))
		   (cond (wait-return-value
			  ;; Flush the wait.
			  (setf (process-wait-return-value next) 
				wait-return-value)
			  (setf (process-wait-timeout next) nil)
			  (setf (process-wait-function next) nil)
			  (setf (process-%whostate next) nil)
			  (update-process-timers current-process next)
			  (stack-group-resume (process-stack-group next)))
			 (t
			  ;; Timeout?
			  (let ((timeout (process-wait-timeout next)))
			    (when (and timeout (> (get-real-time) timeout))
			      ;; Flush the wait.
			      (setf (process-wait-return-value next) nil)
			      (setf (process-wait-timeout next) nil)
			      (setf (process-wait-function next) nil)
			      (setf (process-%whostate next) nil)
			      (update-process-timers current-process next)
			      (stack-group-resume
			       (process-stack-group next)))))))
		 ;; Restore the *current-process*.
		 (setf *current-process* current-process))))))))
    
     ;; May have just returned, or have cycled the queue.
     (let ((next (first *remaining-processes*)))
       ;; Tolerate :killed processes on the *remaining-processes* list
       ;; saving their deletion from this list when killed; will be
       ;; corrected when it cycles back to *all-processes*.
       (when (and (process-active-p next)
		  ;; Current process at the head of the queue?
		  (eq next *current-process*))
	 ;; Run any pending interrupts.
	 (let ((interrupt (pop (process-interrupts next))))
	   (declare (type (or null function) interrupt))
	   (cond (interrupt 
		  ;; Save and reset any wait reasons so that the
		  ;; interrupt can wait. The return-value is also
		  ;; saved and restored in case a process is
		  ;; interrupted before it is read.
		  (let ((wait-function (process-wait-function next))
			(wait-timeout (process-wait-timeout next))
			(whostate (process-%whostate next))
			(wait-return-value (process-wait-return-value next)))
		    (setf (process-wait-function next) nil)
		    (setf (process-wait-timeout next) nil)
		    (setf (process-%whostate next) nil)
		    (setf (process-wait-return-value next) nil)
		    ;; Allow recursive scheduling during the interrupt
		    ;; processing. Only one interrupt is processed on
		    ;; each scheduler queue cycle. The process doesn't
		    ;; return until there are no interrupts.
		    (setf *inhibit-scheduling* nil)
		    (funcall interrupt)
		    (setf *inhibit-scheduling* t)
		    ;; Restore any wait reasons.
		    (setf (process-wait-function next) wait-function)
		    (setf (process-wait-timeout next) wait-timeout)
		    (setf (process-%whostate next) whostate)
		    (setf (process-wait-return-value next) wait-return-value)))
		 (t
		  ;; Check the wait function.
		  (let ((wait-fn (process-wait-function next)))
		    (cond
		      ((null wait-fn)
		       (when (or (not (eq next *idle-process*))
				 (run-idle-process-p))
			 (return)))
		      (t
		       ;; Predicate true?
		       (let ((return-value (funcall wait-fn)))
			 (when return-value
			   ;; Flush the wait.
			   (setf (process-wait-return-value next) return-value)
			   (setf (process-wait-timeout next) nil)
			   (setf (process-wait-function next) nil)
			   (setf (process-%whostate next) nil)
			   (return)))
		       ;; Timeout?
		       (let ((timeout (process-wait-timeout next)))
			 (when (and timeout (> (get-real-time) timeout))
			   ;; Flush the wait.
			   (setf (process-wait-return-value next) nil)
			   (setf (process-wait-timeout next) nil)
			   (setf (process-wait-function next) nil)
			   (setf (process-%whostate next) nil)
			   (return))))))))))))
    (setf *inhibit-scheduling* nil)))

;;; Process-Real-Time
;;;
;;; The real time in seconds accrued while the process was scheduled.
;;;
(defun process-real-time (process)
  "Return the accrued real time elapsed while the given process was
  scheduled. The returned time is a double-float in seconds."
  (declare (type process process))
  (if (eq process *current-process*)
      (without-scheduling
       (let ((real-time (get-real-time)))
	 (+ (process-%real-time process)
	    (- real-time (process-scheduled-real-time process)))))
      (process-%real-time process)))

;;; Process-Run-Time  --  Public
;;;
;;; The run time in seconds accrued while the process was scheduled.
;;;
(defun process-run-time (process)
  "Return the accrued run time elapsed for the given process. The returned
  time is a double-float in seconds."
  (declare (type process process))
  (if (eq process *current-process*)
      (without-scheduling
       (let ((run-time (get-run-time)))
	 (+ (process-%run-time process)
	    (- run-time (process-scheduled-run-time process)))))
      (process-%run-time process)))

;;; Process-Idle-Time  --  Public
;;;
;;; The real time in seconds elapsed since the process was last
;;; de-scheduled.
;;;
(defun process-idle-time (process)
  "Return the real time elapsed since the given process was last
  descheduled. The returned time is a double-float in seconds."
  (declare (type process process))
  (if (eq process *current-process*)
      0
      (without-scheduling
       (let ((real-time (get-real-time)))
	 (- real-time (process-scheduled-real-time process))))))

;;; Start-Sigalrm-Yield  --  Internal
;;;
;;; Start a regular interrupt to switch processes. This may not be a
;;; good idea yet as the CMUCL code is not too interrupt safe.
;;;
(defun start-sigalrm-yield (&optional (sec 0) (usec 500000))
  "Start a regular SIGALRM interrupt which calls process-yield. An optional
  time in seconds and micro seconds may be provided. Note that CMUCL code
  base is not too interrupt safe so this may cause problems."
  (declare (fixnum sec usec))
  ;; Disable the gencgc pointer filter to improve interrupt safety.
  #+(and gencgc nil)
  (setf (alien:extern-alien "enable_pointer_filter" alien:unsigned) 0)
  (flet ((sigalrm-handler (signal code scp)
	   (declare (ignore signal code scp))
	   (cond ((<= lisp::*free-interrupt-context-index* 1)
		  #+nil (format t ".~%")
		  (process-yield))
		 (t
		  #+nil (format t "-~%")))))
    (sys:enable-interrupt :sigalrm #'sigalrm-handler))
  (unix:unix-setitimer :real sec usec 0 1)
  (values))

;;; Init-Multi-Processing  --  Internal.
;;;
;;; Startup multi-processing, initialising the initial process. This
;;; must be called before use of the other multi-process functions.
;;;
(defun init-multi-processing ()
  (unless *initial-process*
    (init-stack-groups)
    (setf *initial-process*
	  (%make-process
	   :name "Initial"
	   :state :active
	   :%run-reasons (list :enable)
	   :stack-group *initial-stack-group*))
    (setf *current-process* *initial-process*)
    (setf *all-processes* (list *initial-process*))
    (setf *remaining-processes* *all-processes*)
    ;;
    #+nil (start-sigalrm-yield)
    (setf *inhibit-scheduling* nil)))

(pushnew 'init-multi-processing ext:*after-save-initializations*)

;;; Scrub-all-processes-stacks  --  Internal
;;;
;;; Scrub the stored stacks of all the processes.
;;;
(defun scrub-all-processes-stacks ()
  (sys:without-interrupts
   (dolist (process *all-processes*)
     (let ((stack-group (process-stack-group process)))
       (when stack-group
	 (scrub-stack-group-stacks stack-group))))))
;;;
(pushnew 'scrub-all-processes-stacks ext:*before-gc-hooks*)


;;; Process-Wait-Until-FD-Usable  --  Public.
;;;
;;; Wait until FD is usable for DIRECTION.
;;;
(defun process-wait-until-fd-usable (fd direction &optional timeout)
  "Wait until FD is usable for DIRECTION and return True. DIRECTION should be
  either :INPUT or :OUTPUT. TIMEOUT, if supplied, is the number of seconds to
  wait before giving up and returing NIL."
  (declare (type kernel:index fd)
	   (type (or real null) timeout)
	   (optimize (speed 3)))
  (if (or (eq *current-process* *initial-process*)
	  ;; Can't call process-wait if the scheduling is inhibited.
	  *inhibit-scheduling*)
      ;; The initial-process calls the event server to block.
      (sys:wait-until-fd-usable fd direction timeout)
      ;; Other processes use process-wait.
      (flet ((fd-usable-for-input ()
	       (declare (optimize (speed 3) (safety 1)))
	       (alien:with-alien ((read-fds (alien:struct unix:fd-set)))
		 (unix:fd-zero read-fds)
		 (unix:fd-set fd read-fds)
		 (multiple-value-bind (value err)
		     (unix:unix-fast-select
		      (1+ fd) (alien:addr read-fds) nil nil 0 0)
		   ;; Return true when input is available or there is
		   ;; an error other than an interrupt.
		   (and (not (eql value 0))
			(or value (not (eql err unix:eintr)))))))
	     (fd-usable-for-output ()
	       (declare (optimize (speed 3) (safety 1)))
	       (alien:with-alien ((write-fds (alien:struct unix:fd-set)))
		 (unix:fd-zero write-fds)
		 (unix:fd-set fd write-fds)
		 (multiple-value-bind (value err)
		     (unix:unix-fast-select
		      (1+ fd) nil (alien:addr write-fds) nil 0 0)
		   ;; Return true when ready for output or there is an
		   ;; error other than an interrupt.
		   (and (not (eql value 0))
			(or value (not (eql err unix:eintr))))))))

	(ecase direction
	  (:input
	   (or (fd-usable-for-input)
	       ;; Wait until input possible.
	       (sys:with-fd-handler (fd :input
					#'(lambda (fd)
					    (declare (ignore fd)
						     (optimize (speed 3)
							       (safety 0)))
					    (mp:process-yield)))
		 (if timeout
		     (mp:process-wait-with-timeout "Input Wait" timeout
						   #'fd-usable-for-input)
		     (mp:process-wait "Input Wait" #'fd-usable-for-input)))))
	  (:output
	   (or (fd-usable-for-output)
	       ;; Wait until output possible.
	       (sys:with-fd-handler (fd :output
					#'(lambda (fd)
					    (declare (ignore fd)
						     (optimize (speed 3)
							       (safety 0)))
					    (mp:process-yield)))
		 (if timeout
		     (mp:process-wait-with-timeout "Output Wait" timeout
						   #'fd-usable-for-output)
		     (mp:process-wait "Output Wait"
				      #'fd-usable-for-output)))))))))


;;; Sleep  --  Public
;;;
;;; Redefine the sleep function to call process-wait-with-timeout,
;;; rather than blocking.
;;;
(defun sleep (n)
  "This function causes execution to be suspended for N seconds.  N may
  be any non-negative, non-complex number."
  (when (or (not (realp n))
	    (minusp n))
    (error "Invalid argument to SLEEP: ~S.~%~
            Must be a non-negative, non-complex number."
	   n))
  (cond ((or (eq *current-process* *initial-process*)
	     ;; Can't call process-wait if the scheduling is inhibited.
	     *inhibit-scheduling*)
	 ;; The initial-process may block.
	 (multiple-value-bind (sec usec)
	     (if (integerp n)
		 (values n 0)
		 (multiple-value-bind (sec frac)(truncate n)
		   (values sec (truncate frac 1e-6))))
	   (unix:unix-select 0 0 0 0 sec usec))
	 nil)
	(t
	 (process-wait-with-timeout "Sleep" n (constantly nil)))))



;;; With-Timeout-Internal  --  Internal
;;;
(defun with-timeout-internal (timeout function timeout-function)
  (catch 'timer-interrupt
    (let* ((current-process mp:*current-process*)
	   (timer-process (mp:make-process
			   #'(lambda ()
			       (sleep timeout)
			       (mp:process-interrupt
				current-process
				#'(lambda () (throw 'timer-interrupt nil))))
			   :name "Timeout timer")))
      (unwind-protect
	   (return-from with-timeout-internal (funcall function))
	(mp:destroy-process timer-process))))
   (funcall timeout-function))

;;; With-Timeout  --  Public
;;;
(defmacro with-timeout ((timeout &body timeout-forms) &body body)
  "Executes body and returns the values of the last form in body. However, if
  the execution takes longer than timeout seconds, abort it and evaluate
  timeout-forms, returning the values of last form."
  `(flet ((fn () . ,body)
	  (tf () . ,timeout-forms))
    (with-timeout-internal ,timeout #'fn #'tf)))


;;; Show-Processes  --  Public
;;;
(defun show-processes (&optional verbose)
  "Show the all the processes, their whostate, and state. If the optional
  verbose argument is true then the run, real, and idle times are also
  shown."
  (fresh-line)
  (dolist (process *all-processes*)
    (when (eq process *current-process*)
      (format t "-> "))
    (format t "~s ~s ~a~%" process (process-whostate process) 
	    (process-state process))
    (when verbose
      (format t "~4TRun time: ~,3f; Real time: ~,3f; Idle time: ~,3f~%"
	      (process-run-time process)
	      (process-real-time process)
	      (process-idle-time process)))))


;;; Top-Level  --  Internal
;;;
(defun top-level ()
  "Top-level READ-EVAL-PRINT loop for processes."
  (let ((* nil) (** nil) (*** nil)
	(- nil) (+ nil) (++ nil) (+++ nil)
	(/// nil) (// nil) (/ nil)
	(magic-eof-cookie (cons :eof nil)))
    (loop
      (with-simple-restart (abort "Return to Top-Level.")
	(catch 'lisp::top-level-catcher
	  (unix:unix-sigsetmask 0)
	  (let ((lisp::*in-top-level-catcher* t))
	    (loop
	      (sys:scrub-control-stack)
	      (fresh-line)
	      (princ (if (functionp ext:*prompt*)
			 (funcall ext:*prompt*)
		       ext:*prompt*))
	      (force-output)
	      (let ((form (read *standard-input* nil magic-eof-cookie)))
		(cond ((not (eq form magic-eof-cookie))
		       (let ((results
			      (multiple-value-list
				  (ext:interactive-eval form))))
			 (dolist (result results)
			   (fresh-line)
			   (prin1 result))))
		      (t
		       (throw '%end-of-the-process nil)))))))))))

;;; Startup-Idle-and-Top-Level-Loops -- Internal
;;;
(defun startup-idle-and-top-level-loops ()
  "Enter the idle loop, starting a new process to run the top level loop.
  The awaking of sleeping processes is timed better with the idle loop process
  running, and starting a new process for the top level loop supports a
  simultaneous interactive session. Such an initialisation will likely be the
  default when there is better MP debug support etc."
  (assert (eq *current-process* *initial-process*) ()
	  "Only the *initial-process* is intended to run the idle loop")
  (init-multi-processing)	; Initialise in case MP had been shutdown.
  ;; Start a new Top Level loop.
  (make-process #'top-level :name "Top Level Loop") 
  ;; Enter the idle loop.
  (idle-process-loop))

;;; Start-Lisp-Connection-Listener
;;;
;;; Create a process to listen for connections on a TCP port and start
;;; a new top-level process for each connection.
;;;
(defun start-lisp-connection-listener (&key (port 1025)
					    (password (random (expt 2 24))))
  (declare (type (unsigned-byte 16) port))
  "Create a Lisp connection listener, listening on a TCP port for new
  connections and starting a new top-level loop for each. If a password
  is not given then one will be generated and reported.  A search is
  performed for the first free port starting at the given port which
  defaults to 1025."
  (labels (;; The session top level read eval. loop.
	   (start-top-level (fd)
	     (let ((stream (sys:make-fd-stream fd :input t :output t)))
	       (unwind-protect
		    (let* ((*terminal-io* stream)
			   (*standard-input*
			    (make-synonym-stream '*terminal-io*))
			   (*standard-output* *standard-input*)
			   (*error-output* *standard-input*)
			   (*debug-io* *standard-input*)
			   (*query-io* *standard-input*)
			   (*trace-output* *standard-input*))
		      ;;
		      (format t "Enter password: ")
		      (finish-output)
		      (let* ((*read-eval* nil)
			     (read-password
			      (handler-case 
			       (read)
			       (error () (return-from start-top-level)))))
			(unless (equal read-password password)
			  (return-from start-top-level)))
		      (ext:print-herald)
		      ;;
		      (top-level))
		 (handler-case 
		  (close stream)
		  (error ())))))
	   ;;
	   ;; Turn internet address into string format
	   (ip-address-string (address)
	     (format nil "~D.~D.~D.~D"
		     (ldb (byte 8 24) address)
		     (ldb (byte 8 16) address)
		     (ldb (byte 8 8)  address)
		     (ldb (byte 8 0)  address)))
	   ;;
	   ;; The body of the connection listener.
	   (listener ()
	     (declare (optimize (speed 3)))
	     (let ((fd nil))
	       (unwind-protect
		    (progn
		      ;; Start the listener.
		      (do ()
			  (fd)
			(handler-case
			 (setf fd (ext:create-inet-listener port))
			 (error () (incf port))))

		      (setf (process-name *current-process*)
			    (format nil "Lisp connection listener on port ~d"
				    port))

		      (format t "~&;;; Started lisp connection listener on ~
 				  port ~d with password ~d~%"
			      port password)

		      (loop
		       ;; Wait for new connections.
		       (process-wait-until-fd-usable fd :input)
		       (multiple-value-bind (new-fd remote-host)
			   (ext:accept-tcp-connection fd)
			 (let ((host-entry (ext:lookup-host-entry
					    remote-host)))
			   (make-process
			    #'(lambda ()
				(start-top-level new-fd))
			    :name (format nil "Lisp session from ~A"
					  (if host-entry
					      (ext:host-entry-name host-entry)
					      (ip-address-string
					       remote-host))))))))
		 ;; Close the listener stream.
		 (when fd
		   (unix:unix-close fd))))))

    ;; Make the listening thread.
    (make-process #'listener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Simple Locking.

;;;
(defstruct (lock
	     (:constructor nil)
	     (:print-function %print-lock))
  (name nil :type (or null simple-base-string))
  (process nil :type (or null process)))

(defstruct (recursive-lock
	     (:include lock)
	     (:constructor make-recursive-lock (&optional name))))

(defstruct (error-check-lock
	     (:include lock)
	     (:constructor make-error-check-lock (&optional name))))

(defun make-lock (&optional name &key (kind :recursive))
  (ecase kind
    (:recursive (make-recursive-lock name))
    (:error-check (make-error-check-lock name))))

(defun %print-lock (lock stream depth)
  (declare (type lock lock) (stream stream) (ignore depth))
  (print-unreadable-object (lock stream :identity t)
    (write-string (etypecase lock
		    (recursive-lock "Recursive-lock")
		    (error-check-lock "Error-Check-lock"))
		  stream)
    (let ((name (lock-name lock)))
      (when name
	(format stream " ~a" name)))
    (let ((process (lock-process lock)))
      (cond (process
	     (format stream ", held by ~s" process))
	    (t
	     (write-string ", free" stream))))))

;;; Lock-Wait  --  Internal
;;;
;;; Wait for the lock to be free and acquire it for the
;;; *current-process*.
;;;
(defun lock-wait (lock whostate)
  (declare (type lock lock))
  (process-wait whostate
		#'(lambda ()
		    (declare (optimize (speed 3)))
		    #-i486
		    (unless (lock-process lock)
		      (setf (lock-process lock) *current-process*))
		    #+i486
		    (null (kernel:%instance-set-conditional
			   lock 2 nil *current-process*)))))

;;; Lock-Wait-With-Timeout  --  Internal
;;;
;;; Wait with a timeout for the lock to be free and acquire it for the
;;; *current-process*.
;;;
(defun lock-wait-with-timeout (lock whostate timeout)
  (declare (type lock lock))
  (process-wait-with-timeout
   whostate timeout
   #'(lambda ()
       (declare (optimize (speed 3)))
       #-i486
       (unless (lock-process lock)
	 (setf (lock-process lock) *current-process*))
       #+i486
       (null (kernel:%instance-set-conditional
	      lock 2 nil *current-process*)))))

;;; Seize-lock  --  Internal
;;;
;;; Atomically seize a lock if it's free.
;;;
#-i486
(defun seize-lock (lock)
  (declare (type lock lock)
	   (optimize (speed 3)))
  (sys:without-interrupts
   (unless (lock-process lock)
     (setf (lock-process lock) *current-process*))))

;;; With-Lock-Held  --  Public
;;;
(defmacro with-lock-held ((lock &optional (whostate "Lock Wait")
				&key (wait t) timeout)
			  &body body)
  "Execute the body with the lock held. If the lock is held by another
  process then the current process waits until the lock is released or
  an optional timeout is reached. The optional wait timeout is a time in
  seconds acceptable to process-wait-with-timeout.  The results of the
  body are return upon success and NIL is return if the timeout is
  reached. When the wait key is NIL and the lock is held by another
  process then NIL is return immediately without processing the body."
  (let ((have-lock (gensym)))
    `(let ((,have-lock (eq (lock-process ,lock) *current-process*)))
      (unwind-protect
	   ,(cond ((and timeout wait)
		   `(progn
		      (when (and (error-check-lock-p ,lock) ,have-lock)
			(error "Dead lock"))
		      (when (or ,have-lock
				 #+i486 (null (kernel:%instance-set-conditional
					       ,lock 2 nil *current-process*))
				 #-i486 (seize-lock ,lock)
				 (if ,timeout
				     (lock-wait-with-timeout
				      ,lock ,whostate ,timeout)
				     (lock-wait ,lock ,whostate)))
			,@body)))
		  (wait
		   `(progn
		      (when (and (error-check-lock-p ,lock) ,have-lock)
		        (error "Dead lock"))
		      (unless (or ,have-lock
				 #+i486 (null (kernel:%instance-set-conditional
					       ,lock 2 nil *current-process*))
				 #-i486 (seize-lock ,lock))
			(lock-wait ,lock ,whostate))
		      ,@body))
		  (t
		   `(when (or (and (recursive-lock-p ,lock) ,have-lock)
			      #+i486 (null (kernel:%instance-set-conditional
					    ,lock 2 nil *current-process*))
			      #-i486 (seize-lock ,lock))
		      ,@body)))
	(unless ,have-lock
	  #+i486 (kernel:%instance-set-conditional
		  ,lock 2 *current-process* nil)
	  #-i486 (when (eq (lock-process ,lock) *current-process*)
		   (setf (lock-process ,lock) nil)))))))
