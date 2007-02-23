;;; -*- Mode: Lisp; Package: Debug; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/debug.lisp,v 1.64 2004/08/30 14:55:38 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; CMU Common Lisp Debugger.  This includes a basic command-line oriented
;;; debugger interface as well as support for Hemlock to deliver debugger
;;; commands to a slave Lisp.
;;;
;;; Written by Bill Chiles.
;;;

(in-package "DEBUG")

(export '(internal-debug *in-the-debugger* backtrace *flush-debug-errors*
	  *debug-print-level* *debug-print-length* *debug-prompt*
	  *default-print-frame-call-verbosity*
	  *debug-readtable* *help-line-scroll-count* *stack-top-hint*

	  *auto-eval-in-frame* var arg
	  *only-block-start-locations* *print-location-kind*

	  do-debug-command))

(in-package "LISP")
(export '(invoke-debugger *debugger-hook* step))

(in-package "DEBUG")

;;;
;;; Used to communicate to debug-loop that we are at a step breakpoint.
;;;
(define-condition step-condition (simple-condition) ())


;;;; Variables, parameters, and constants.

(defparameter *debug-print-level* 3
  "*PRINT-LEVEL* is bound to this value when debug prints a function call.  If
  null, use *PRINT-LEVEL*")

(defparameter *debug-print-length* 5
  "*PRINT-LENGTH* is bound to this value when debug prints a function call.  If
  null, use *PRINT-LENGTH*.")

(defparameter *default-print-frame-call-verbosity* 1
  "default value for the verbose argument to print-frame-call.  If set to >= 2, source will be printed for all frames")

(defvar *in-the-debugger* nil
  "This is T while in the debugger.")

(defvar *debug-command-level* 0
  "Pushes and pops/exits inside the debugger change this.")

(defvar *stack-top-hint* nil
  "If this is bound before the debugger is invoked, it is used as the stack
   top by the debugger.")
(defvar *stack-top* nil)
(defvar *real-stack-top* nil)

(defvar *current-frame* nil)

;;; DEBUG-PROMPT -- Internal.
;;;
;;; This is the default for *debug-prompt*.
;;;
(defun debug-prompt ()
  (let ((*standard-output* *debug-io*))
    (terpri)
    (prin1 (di:frame-number *current-frame*))
    (dotimes (i *debug-command-level*) (princ "]"))
    (princ " ")
    (force-output)))

(defparameter *debug-prompt* #'debug-prompt
  "This is a function of no arguments that prints the debugger prompt
   on *debug-io*.")

(defconstant debug-help-string
"
The prompt is right square brackets, the number indicating how many
  recursive command loops you are in.
Debug commands do not affect * and friends, but evaluation in the debug loop
  do affect these variables.
Any command may be uniquely abbreviated.

Getting in and out of DEBUG:
  Q        throws to top level.
  GO       calls CONTINUE which tries to proceed with the restart 'continue.
  RESTART  invokes restart numbered as shown (prompt if not given).
  ERROR    prints the error condition and restart cases.
  FLUSH    toggles *flush-debug-errors*, which is initially t.
 
  The name of any restart, or its number, is a valid command, and is the same
    as using RESTART to invoke that restart.

Changing frames:
  U  up frame        D  down frame       T  top frame       B  bottom frame

  F n   goes to frame n.

Inspecting frames:
  BACKTRACE [n]  shows n frames going down the stack.
  L              lists locals in current function.
  P, PP          displays current function call.
  SOURCE [n]     displays frame's source form with n levels of enclosing forms.
  VSOURCE [n]    displays frame's source form without any ellipsis.

Breakpoints and steps:
  LIST-LOCATIONS [{function | :c}]  list the locations for breakpoints.
    Specify :c for the current frame.  Abbreviation: LL
  LIST-BREAKPOINTS                  list the active breakpoints.
    Abbreviations: LB, LBP
  DELETE-BREAKPOINT [n]             remove breakpoint n or all breakpoints.
    Abbreviations: DEL, DBP    
  BREAKPOINT {n | :end | :start} [:break form] [:function function]
    [{:print form}*] [:condition form]    set a breakpoint.
    Abbreviations: BR, BP
  STEP [n]                          step to the next location or step n times.

Actions on frames:
  DEBUG-RETURN expression
    returns expression's values from the current frame, exiting the debugger.
    Abbreviations: R

See the CMU Common Lisp User's Manual for more information.
")


;;;; Breakpoint state:

(defvar *only-block-start-locations* nil
  "When true, the LIST-LOCATIONS command only displays block start locations.
   Otherwise, all locations are displayed.")

(defvar *print-location-kind* nil
  "If true, list the code location type in the LIST-LOCATIONS command.")

;;; A list of the types of code-locations that should not be stepped to and
;;; should not be listed when listing breakpoints.
;;;
(defvar *bad-code-location-types* '(:call-site :internal-error))
(declaim (type list *bad-code-location-types*))

;;; Code locations of the possible breakpoints
;;;
(defvar *possible-breakpoints*)
(declaim (type list *possible-breakpoints*))

;;; A list of the made and active breakpoints, each is a breakpoint-info
;;; structure.
;;;
(defvar *breakpoints* nil)
(declaim (type list *breakpoints*))

;;; A list of breakpoint-info structures of the made and active step
;;; breakpoints.
;;;
(defvar *step-breakpoints* nil)  
(declaim (type list *step-breakpoints*))

;;; Number of times left to step.
;;;
(defvar *number-of-steps* 1)
(declaim (type integer *number-of-steps*))

;;; Used when listing and setting breakpoints.
;;;
(defvar *default-breakpoint-debug-function* nil)
(declaim (type (or list di:debug-function) *default-breakpoint-debug-function*))


;;;; Code location utilities:

;;; FIRST-CODE-LOCATION -- Internal.
;;;
;;; Returns the first code-location in the passed debug block
;;;
(defun first-code-location (debug-block)
  (let ((found nil)
	(first-code-location nil))
    (di:do-debug-block-locations (code-location debug-block)
      (unless found 
	(setf first-code-location code-location)
	(setf found t)))
    first-code-location))

;;; NEXT-CODE-LOCATIONS -- Internal.
;;;
;;; Returns a list of the next code-locations following the one passed.  One of
;;; the *bad-code-location-types* will not be returned.
;;;
(defun next-code-locations (code-location)
  (let ((debug-block (di:code-location-debug-block code-location))
	(block-code-locations nil))
    (di:do-debug-block-locations (block-code-location debug-block)
      (unless (member (di:code-location-kind block-code-location)
		      *bad-code-location-types*)
	(push block-code-location block-code-locations)))
    (setf block-code-locations (nreverse block-code-locations))
    (let* ((code-loc-list (rest (member code-location block-code-locations
					:test #'di:code-location=)))
	   (next-list (cond (code-loc-list
			     (list (first code-loc-list)))
			    ((map 'list #'first-code-location
				  (di:debug-block-successors debug-block)))
			    (t nil))))
      (when (and (= (length next-list) 1)
		 (di:code-location= (first next-list) code-location))
	(setf next-list (next-code-locations (first next-list))))
      next-list)))

;;; POSSIBLE-BREAKPOINTS -- Internal.
;;;  
;;; Returns a list of code-locations of the possible breakpoints of the 
;;; debug-function passed.
;;;
(defun possible-breakpoints (debug-function)
  (let ((possible-breakpoints nil))
    (di:do-debug-function-blocks (debug-block debug-function)
      (unless (di:debug-block-elsewhere-p debug-block)
	(if *only-block-start-locations*
	    (push (first-code-location debug-block) possible-breakpoints)
	    (di:do-debug-block-locations (code-location debug-block)
	      (when (not (member (di:code-location-kind code-location)
				 *bad-code-location-types*))
		(push code-location possible-breakpoints))))))
    (nreverse possible-breakpoints)))

;;; LOCATION-IN-LIST -- Internal.
;;;
;;; Searches the info-list for the item passed (code-location, debug-function,
;;; or breakpoint-info).  If the item passed is a debug function then kind will
;;; be compared if it was specified.  The kind if also compared if a
;;; breakpoint-info is passed since it's in the breakpoint.  The info structure
;;; is returned if found.
;;;
(defun location-in-list (place info-list &optional (kind nil)) 
  (when (breakpoint-info-p place)
    (setf kind (di:breakpoint-kind (breakpoint-info-breakpoint place)))
    (setf place (breakpoint-info-place place)))
  (cond ((di:code-location-p place)
	 (find place info-list
	       :key #'breakpoint-info-place
	       :test #'(lambda (x y) (and (di:code-location-p y)
					  (di:code-location= x y)))))
	(t
	 (find place info-list
	       :test #'(lambda (x-debug-function y-info)
			 (let ((y-place (breakpoint-info-place y-info))
			       (y-breakpoint (breakpoint-info-breakpoint
					      y-info)))
			   (and (di:debug-function-p y-place)
				(eq x-debug-function y-place)
				(or (not kind)
				    (eq kind (di:breakpoint-kind
					      y-breakpoint))))))))))


;;; MAYBE-BLOCK-START-LOCATION  --  Internal.
;;;
;;; If Loc is an unknown location, then try to find the block start location.
;;; Used by source printing to some information instead of none for the user.
;;;
(defun maybe-block-start-location (loc)
  (if (di:code-location-unknown-p loc)
      (let* ((block (di:code-location-debug-block loc))
	     (start (di:do-debug-block-locations (loc block)
		      (return loc))))
	(cond ((and (not (di:debug-block-elsewhere-p block))
		    start)
	       (format t "~%Unknown location: using block start.~%")
	       start)
	      (t
	       loc)))
      loc))


;;;; The BREAKPOINT-INFO structure:

;;; Hold info about made breakpoints
;;;
(defstruct breakpoint-info
  ;;
  ;; Where we are going to stop.
  (place (required-argument) :type (or di:code-location di:debug-function)) 
  ;;
  ;; The breakpoint returned by di:make-breakpoint.
  (breakpoint (required-argument) :type di:breakpoint)
  ;;
  ;; Function returned from di:preprocess-for-eval.  If result is non-nil,
  ;; drop into the debugger.
  (break #'identity :type function)
  ;; 
  ;; Function returned from di:preprocess-for-eval.  If result is non-nil,
  ;; eval (each) print and print results.
  (condition #'identity :type function)
  ;;
  ;; List of functions from di:preprocess-for-eval to evaluate, results are
  ;; conditionally printed.  Car of each element is the function, cdr is the
  ;; form it goes with.
  (print nil :type list)
  ;;
  ;; The number used when listing the possible breakpoints within a function.
  ;; Could also be a symbol such as start or end.
  (code-location-number (required-argument) :type (or symbol integer))
  ;;
  ;; The number used when listing the breakpoints active and to delete
  ;; breakpoints. 
  (breakpoint-number (required-argument) :type integer))


;;; CREATE-BREAKPOINT-INFO -- Internal.
;;;
;;; Returns a new breakpoint-info structure with the info passed.
;;;
(defun create-breakpoint-info (place breakpoint code-location-number
				     &key (break #'identity)
				     (condition #'identity) (print nil))
  (setf *breakpoints*
	(sort *breakpoints* #'< :key #'breakpoint-info-breakpoint-number))
  (let ((breakpoint-number
	 (do ((i 1 (incf i)) (breakpoints *breakpoints* (rest breakpoints)))
	     ((or (> i (length *breakpoints*))
		  (not (= i (breakpoint-info-breakpoint-number
			     (first breakpoints)))))

	      i))))
    (make-breakpoint-info :place place :breakpoint breakpoint
			  :code-location-number code-location-number
			  :breakpoint-number breakpoint-number
			  :break break :condition condition :print print)))

;;; PRINT-BREAKPOINT-INFO -- Internal.
;;;
;;; Prints the breakpoint info for the breakpoint-info structure passed.
;;;
(defun print-breakpoint-info (breakpoint-info)
  (let ((place (breakpoint-info-place breakpoint-info))
	(bp-number (breakpoint-info-breakpoint-number breakpoint-info))
	(loc-number (breakpoint-info-code-location-number breakpoint-info)))
    (case (di:breakpoint-kind (breakpoint-info-breakpoint breakpoint-info))
      (:code-location 
       (print-code-location-source-form place 0)
       (format t "~&~S: ~S in ~S"
	       bp-number loc-number (di:debug-function-name
				      (di:code-location-debug-function place))))
      (:function-start
       (format t "~&~S: FUNCTION-START in ~S" bp-number
	       (di:debug-function-name place)))
      (:function-end
       (format t "~&~S: FUNCTION-END in ~S" bp-number
	       (di:debug-function-name place))))))



;;;; Main-hook-function for steps and breakpoints

;;; MAIN-HOOK-FUNCTION -- Internal.
;;;
;;; Must be passed as the hook function.  Keeps track of where step 
;;; breakpoints are.
;;;
(defun main-hook-function (current-frame breakpoint &optional return-vals
					 function-end-cookie)
  (setf *default-breakpoint-debug-function*
	(di:frame-debug-function current-frame))
  (dolist (step-info *step-breakpoints*)
    (di:delete-breakpoint (breakpoint-info-breakpoint step-info))
    (let ((bp-info (location-in-list step-info *breakpoints*)))
      (when bp-info
	(di:activate-breakpoint (breakpoint-info-breakpoint bp-info)))))
  (let ((*stack-top-hint* current-frame)
	(step-hit-info
	 (location-in-list (di:breakpoint-what breakpoint)
			   *step-breakpoints* (di:breakpoint-kind breakpoint)))
	(bp-hit-info
	 (location-in-list (di:breakpoint-what breakpoint)
			   *breakpoints* (di:breakpoint-kind breakpoint)))
	(break)
	(condition)
	(string ""))
    (setf *step-breakpoints* nil)
    (labels ((build-string (str)
	       (setf string (concatenate 'string string str)))
	     (print-common-info ()
	       (build-string 
		(with-output-to-string (*standard-output*)
		  (when function-end-cookie 
		    (format t "~%Return values: ~S" return-vals))
		  (when condition
		    (when (breakpoint-info-print bp-hit-info)
		      (format t "~%")
		      (print-frame-call current-frame))
		    (dolist (print (breakpoint-info-print bp-hit-info))
		      (format t "~& ~S = ~S" (rest print)
			      (funcall (first print) current-frame))))))))
      (when bp-hit-info
	(setf break (funcall (breakpoint-info-break bp-hit-info)
			     current-frame))
	(setf condition (funcall (breakpoint-info-condition bp-hit-info)
				 current-frame)))
      (cond ((and bp-hit-info step-hit-info (= 1 *number-of-steps*))
	     (build-string (format nil "~&*Step (to a breakpoint)*"))
	     (print-common-info)
	     (break string))
	    ((and bp-hit-info step-hit-info break)
	     (build-string (format nil "~&*Step (to a breakpoint)*"))
	     (print-common-info)
	     (break string))
	    ((and bp-hit-info step-hit-info)
	     (print-common-info)
	     (format t "~A" string)
	     (decf *number-of-steps*)
	     (set-step-breakpoint current-frame))
	    ((and step-hit-info (= 1 *number-of-steps*))
	     (build-string "*Step*")
	     (break (make-condition 'step-condition :format-control string)))
	    (step-hit-info
	     (decf *number-of-steps*)
	     (set-step-breakpoint current-frame))
	    (bp-hit-info
	     (when break
	       (build-string (format nil "~&*Breakpoint hit*")))
	     (print-common-info)
	     (if break
		 (break string)
		 (format t "~A" string)))
	    (t
	     (break "Error in main-hook-function: unknown breakpoint"))))))



;;; SET-STEP-BREAKPOINT -- Internal.
;;;
;;; Sets breakpoints at the next possible code-locations.  After calling
;;; this either (continue) if in the debugger or just let program flow
;;; return if in a hook function.
(defun set-step-breakpoint (frame)
  (cond
   ((di:debug-block-elsewhere-p (di:code-location-debug-block
				 (di:frame-code-location frame)))
    (format t "Cannot step, in elsewhere code~%"))
   (t
    (let* ((code-location (di:frame-code-location frame))
	   (next-code-locations (next-code-locations code-location)))
      (cond
       (next-code-locations
	(dolist (code-location next-code-locations)
	  (let ((bp-info (location-in-list code-location *breakpoints*)))
	    (when bp-info
	      (di:deactivate-breakpoint (breakpoint-info-breakpoint bp-info))))
	  (let ((bp (di:make-breakpoint #'main-hook-function code-location
					:kind :code-location)))
	    (di:activate-breakpoint bp)
	    (push (create-breakpoint-info code-location bp 0)
		  *step-breakpoints*))))
       (t
	(let* ((debug-function (di:frame-debug-function *current-frame*))
	       (bp (di:make-breakpoint #'main-hook-function debug-function
				       :kind :function-end)))
	  (di:activate-breakpoint bp)
	  (push (create-breakpoint-info debug-function bp 0)
		*step-breakpoints*))))))))

;;; STEP-INTERNAL -- Internal.
;;;
(defun step-internal (function form)
  (when (eval:interpreted-function-p function)
    ;; The stepper currently only supports compiled functions So we
    ;; try to compile the passed-in function, bailing out if it fails.
    (handler-case
	(setq function (compile nil function))
      (error (c)
	(error "Currently only compiled code can be stepped.~%~
                Trying to compile the passed form resulted in ~
                the following error:~%  ~A" c))))
  (let ((*print-length* *debug-print-length*)
	(*print-level* *debug-print-level*))
    (format *debug-io* "~2&Stepping the form~%  ~S~%" form)
    (format *debug-io* "~&using the debugger.  Type HELP for help.~2%"))
  (let* ((debug-function (di:function-debug-function function))
	 (bp (di:make-breakpoint #'main-hook-function debug-function
				 :kind :function-start)))
    (di:activate-breakpoint bp)
    (push (create-breakpoint-info debug-function bp 0)
	  *step-breakpoints*))
  (funcall function))

;;; STEP -- Public.
;;;
(defmacro step (form)
  "STEP implements a debugging paradigm wherein the programmer is allowed
   to step through the evaluation of a form.  We use the debugger's stepping
   facility to step through an anonymous function containing only form.

   Currently the stepping facility only supports stepping compiled code,
   so step will try to compile the resultant anonymous function.  If this
   fails, e.g. because it closes over a non-null lexical environment, an
   error is signalled."
  `(step-internal #'(lambda () ,form) ',form))


;;;; Backtrace:

;;; BACKTRACE -- Public.
;;;
(defun backtrace (&optional (count most-positive-fixnum)
			    (*standard-output* *debug-io*))
  "Show a listing of the call stack going down from the current frame.  In the
   debugger, the current frame is indicated by the prompt.  Count is how many
   frames to show."
  (let ((*print-length* (or *debug-print-length* *print-length*))
	(*print-level* (or *debug-print-level* *print-level*)))
    (fresh-line *standard-output*)
    (do ((frame (if *in-the-debugger* *current-frame* (di:top-frame))
		(di:frame-down frame))
	 (count count (1- count)))
	((or (null frame) (zerop count)))
      (print-frame-call frame :number t))
    (fresh-line *standard-output*)
    (values)))


;;;; Frame printing:

(eval-when (compile eval)

;;; LAMBDA-LIST-ELEMENT-DISPATCH -- Internal.
;;;
;;; This is a convenient way to express what to do for each type of lambda-list
;;; element.
;;;
(defmacro lambda-list-element-dispatch (element &key required optional rest
						keyword deleted)
  `(etypecase ,element
     (di:debug-variable
      ,@required)
     (cons
      (ecase (car ,element)
	(:optional ,@optional)
	(:rest ,@rest)
	(:keyword ,@keyword)))
     (symbol
      (assert (eq ,element :deleted))
      ,@deleted)))

(defmacro lambda-var-dispatch (variable location deleted valid other)
  (let ((var (gensym)))
    `(let ((,var ,variable))
       (cond ((eq ,var :deleted) ,deleted)
	     ((eq (di:debug-variable-validity ,var ,location) :valid) ,valid)
	     (t ,other)))))

) ;EVAL-WHEN


;;; This is used in constructing arg lists for debugger printing when the arg
;;; list is unavailable, some arg is unavailable or unused, etc.
;;;
(defstruct (unprintable-object
	    (:constructor make-unprintable-object (string))
	    (:print-function (lambda (x s d)
			       (declare (ignore d))
			       (format s "#<~A>"
				       (unprintable-object-string x)))))
  string)


;;; PRINT-FRAME-CALL-1 -- Internal.
;;;
;;; This prints frame with verbosity level 1.  If we hit a rest-arg, 
;;; then print as many of the values as possible,
;;; punting the loop over lambda-list variables since any other arguments
;;; will be in the rest-arg's list of values.
;;;
(defun print-frame-call-1 (frame)
  (let* ((d-fun (di:frame-debug-function frame))
	 (loc (di:frame-code-location frame))
	 (results (list (di:debug-function-name d-fun))))
    (handler-case
	(dolist (ele (di:debug-function-lambda-list d-fun))
	  (lambda-list-element-dispatch ele
	    :required ((push (frame-call-arg ele loc frame) results))
	    :optional ((push (frame-call-arg (second ele) loc frame) results))
	    :keyword ((push (second ele) results)
		      (push (frame-call-arg (third ele) loc frame) results))
	    :deleted ((push (frame-call-arg ele loc frame) results))
	    :rest ((lambda-var-dispatch (second ele) loc
		     nil
		     (progn
		       (setf results
			     (append (reverse (di:debug-variable-value
					       (second ele) frame))
				     results))
		       (return))
		     (push (make-unprintable-object "unavaliable-rest-arg")
			   results)))))
      (di:lambda-list-unavailable
       ()
       (push (make-unprintable-object "lambda-list-unavailable") results)))
    (prin1 (mapcar #'ensure-printable-object (nreverse results)))
    (when (di:debug-function-kind d-fun)
      (write-char #\[)
      (prin1 (di:debug-function-kind d-fun))
      (write-char #\]))))

(defun ensure-printable-object (object)
  (handler-case
      (with-open-stream (out (make-broadcast-stream))
	(prin1 object out)
	object)
    (error (cond)
      (declare (ignore cond))
      (make-unprintable-object "error printing object"))))

(defun frame-call-arg (var location frame)
  (lambda-var-dispatch var location
    (make-unprintable-object "unused-arg")
    (di:debug-variable-value var frame)
    (make-unprintable-object "unavailable-arg")))


;;; PRINT-FRAME-CALL -- Interface
;;;
;;; This prints a representation of the function call causing frame to exist.
;;; Verbosity indicates the level of information to output; zero indicates just
;;; printing the debug-function's name, and one indicates displaying call-like,
;;; one-liner format with argument values.
;;;
(defun print-frame-call (frame &key
			       ((:print-length *print-length*)
				(or *debug-print-length* *print-length*))
			       ((:print-level *print-level*)
				(or *debug-print-level* *print-level*))
			       (verbosity *default-print-frame-call-verbosity*)
			       (number nil))
  (cond
   ((zerop verbosity)
    (when number
      (format t "~&~S: " (di:frame-number frame)))
    (format t "~S" frame))
   (t
    (when number
      (format t "~&~S: " (di:frame-number frame)))
    (print-frame-call-1 frame)))
  (when (>= verbosity 2)
    (let ((loc (di:frame-code-location frame)))
      (handler-case
	  (progn
	    (di:code-location-debug-block loc)
	    (format t "~%Source: ")
	    (print-code-location-source-form loc 0))
	(di:debug-condition (ignore) ignore)
	(error (cond) (format t "Error finding source: ~A" cond))))))

;;; SAFE-CONDITION-MESSAGE  --  Internal
;;;
;;;    Safely print condition to a string, handling any errors during
;;;    printing.
;;;
(defun safe-condition-message (condition)
  (handler-case
      (princ-to-string condition)
    (error (cond)
      ;; Beware of recursive errors in printing, so only use the condition
      ;; if it is printable itself:
      (format nil "Unable to display error condition~@[: ~A~]"
	      (ignore-errors (princ-to-string cond))))))


;;;; Invoke-debugger.

(defvar *debugger-hook* nil
  "This is either nil or a function of two arguments, a condition and the value
   of *debugger-hook*.  This function can either handle the condition or return
   which causes the standard debugger to execute.  The system passes the value
   of this variable to the function because it binds *debugger-hook* to nil
   around the invocation.")

;;; These are bound on each invocation of INVOKE-DEBUGGER.
;;;
(defvar *debug-restarts*)
(defvar *debug-condition*)

;;; INVOKE-TTY-DEBUGGER  --  Internal
;;;
;;;    Print condition and invoke the TTY debugger.
;;;
(defun invoke-tty-debugger (condition)
  (format *error-output* "~2&~A~%   [Condition of type ~S]~2&"
	  (safe-condition-message *debug-condition*)
          (type-of *debug-condition*))
  (unless (typep condition 'step-condition)
    (show-restarts *debug-restarts* *error-output*))
  (internal-debug))

;;; REAL-INVOKE-DEBUGGER  --  Internal
;;;
;;;    This function really invokes the current standard debugger.
;;;    This is overwritten by e.g. the Motif Interface code.  It is a
;;;    function and not a special variable hook, because users are
;;;    supposed to use *debugger-hook*, and this is supposed to be the
;;;    safe fall-back, which should be fairly secure against
;;;    accidental mishaps.
;;;
(defun real-invoke-debugger (condition)
  (invoke-tty-debugger condition))

;;; INVOKE-DEBUGGER -- Public.
;;;
(defun invoke-debugger (condition)
  "The CMU Common Lisp debugger.  Type h for help."
  (when *debugger-hook*
    (let ((hook *debugger-hook*)
	  (*debugger-hook* nil))
      (funcall hook condition hook)))
  (unix:unix-sigsetmask 0)
  (let* ((*debug-condition* condition)
	 (*debug-restarts* (compute-restarts condition))
	 (*standard-input* *debug-io*)		;in case of setq
	 (*standard-output* *debug-io*)		;''  ''  ''  ''
	 (*error-output* *debug-io*)
	 ;; Rebind some printer control variables.
	 (kernel:*current-level* 0)
	 (*print-readably* nil)
	 (*read-eval* t))
    (real-invoke-debugger condition)))

;;; SHOW-RESTARTS -- Internal.
;;;
(defun show-restarts (restarts &optional (s *error-output*))
  (when restarts
    (format s "~&Restarts:~%")
    (let ((count 0)
	  (names-used '(nil))
	  (max-name-len 0))
      (dolist (restart restarts)
	(let ((name (restart-name restart)))
	  (when name
	    (let ((len (length (princ-to-string name))))
	      (when (> len max-name-len)
		(setf max-name-len len))))))
      (unless (zerop max-name-len)
	(incf max-name-len 3))
      (dolist (restart restarts)
	(let ((name (restart-name restart)))
	  (cond ((member name names-used)
		 (format s "~& ~2D: ~@VT~A~%" count max-name-len restart))
		(t
		 (format s "~& ~2D: [~VA] ~A~%"
			 count (- max-name-len 3) name restart)
		 (push name names-used))))
	(incf count)))))

;;; INTERNAL-DEBUG -- Internal Interface.
;;;
;;; This calls DEBUG-LOOP, performing some simple initializations before doing
;;; so.  INVOKE-DEBUGGER calls this to actually get into the debugger.
;;; CONDITIONS::ERROR-ERROR calls this in emergencies to get into a debug
;;; prompt as quickly as possible with as little risk as possible for stepping
;;; on whatever is causing recursive errors.
;;;
(defun internal-debug ()
  (let ((*in-the-debugger* t)
	(*read-suppress* nil))
    (unless (typep *debug-condition* 'step-condition)
      (clear-input *debug-io*)
      (format *debug-io* "~2&Debug  (type H for help)~2%"))
    #-mp (debug-loop)
    #+mp (mp:without-scheduling (debug-loop))))



;;;; Debug-loop.

(defvar *flush-debug-errors* t
  "When set, avoid calling INVOKE-DEBUGGER recursively when errors occur while
   executing in the debugger.  The 'flush' command toggles this.")

(defvar *debug-readtable* nil
  "When non-NIL, becomes the system *READTABLE* in the debugger
   read-eval-print loop")

(defvar *debug-print-current-frame* t
  "When non-NIL, print the current frame when entering the debugger.")

(defun maybe-handle-dead-input-stream (condition)
  ;; Scenario: "xon <remote-box> cmucl -edit"
  ;; Then close the display with the window manager or shutdown the
  ;; local computer. The remote lisp goes into infinite error loop.
  (labels ((real-stream (stream)
	     (etypecase stream
	       (system:fd-stream
		(values stream (system:fd-stream-fd stream)))
	       (synonym-stream
		(real-stream (symbol-value (synonym-stream-symbol stream))))
	       (two-way-stream
		(real-stream (two-way-stream-input-stream stream))))))

    (when (typep condition 'stream-error)
      (let* ((stream-with-error (stream-error-stream condition))
	     (real-stream-with-error (real-stream stream-with-error))
	     (real-debug-io  (real-stream *debug-io*)))
	(when (and (eq real-stream-with-error real-debug-io)
		   (not (unix:unix-isatty (system:fd-stream-fd real-debug-io))))
	  ;; Probably running on a remote processor and lost the connection.
	  (ext:quit))))))

(defun debug-loop ()
  (let* ((*debug-command-level* (1+ *debug-command-level*))
	 (*real-stack-top* (di:top-frame))
	 (*stack-top* (or *stack-top-hint* *real-stack-top*))
	 (*stack-top-hint* nil)
	 (*current-frame* *stack-top*)
	 (*readtable* (or *debug-readtable* *readtable*)))
    (handler-bind ((di:debug-condition #'(lambda (condition)
					   (princ condition *debug-io*)
					   (throw 'debug-loop-catcher nil))))
      (when *debug-print-current-frame*
        (fresh-line)
        (print-frame-call *current-frame* :verbosity 2))
      (loop
	(catch 'debug-loop-catcher
	  (handler-bind ((error #'(lambda (condition)
				    (maybe-handle-dead-input-stream condition)
				    (when *flush-debug-errors*
				      (clear-input *debug-io*)
				      (princ condition)
				      (format t "~&Error flushed ...")
				      (throw 'debug-loop-catcher nil)))))
	    ;; Must bind level for restart function created by
	    ;; WITH-SIMPLE-RESTART.
	    (let ((level *debug-command-level*)
		  (restart-commands (make-restart-commands)))
	      (with-simple-restart (abort "Return to debug level ~D." level)
		(funcall *debug-prompt*)
		(let ((input (ext:get-stream-command *debug-io*)))
		  (cond (input
			 (let ((cmd-fun (debug-command-p
					 (ext:stream-command-name input)
					 restart-commands)))
			   (cond
			    ((not cmd-fun)
			     (error "Unknown stream-command -- ~S." input))
			    ((consp cmd-fun)
			     (error "Ambiguous debugger command: ~S." cmd-fun))
			    (t
			     (apply cmd-fun (ext:stream-command-args input))))))
			(t
			 (let* ((exp (read))
				(cmd-fun (debug-command-p exp restart-commands)))
			   (cond ((not cmd-fun)
				  (debug-eval-print exp))
				 ((consp cmd-fun)
				  (format t "~&Your command, ~S, is ambiguous:~%"
					  exp)
				  (dolist (ele cmd-fun)
				    (format t "   ~A~%" ele)))
				 (t
				  (funcall cmd-fun)))))))))))))))

(defvar *auto-eval-in-frame* t
  "When set (the default), evaluations in the debugger's command loop occur
   relative to the current frame's environment without the need of debugger
   forms that explicitly control this kind of evaluation.")

(defun debug-eval-print (exp)
  (when (and (fboundp 'lisp::commandp) (funcall 'lisp::commandp exp))
    (return-from debug-eval-print
      (funcall 'lisp::invoke-command-interactive exp)))
  (setq +++ ++ ++ + + - - exp)
  (let* ((values (multiple-value-list
		     (if (and (fboundp 'compile) *auto-eval-in-frame*)
			 (di:eval-in-frame *current-frame* -)
			 (eval -))))
	 (*standard-output* *debug-io*))
    (fresh-line)
    (if values (prin1 (car values)))
    (dolist (x (cdr values))
      (fresh-line)
      (prin1 x))
    (setq /// // // / / values)
    (setq *** ** ** * * (car values))
    ;; Make sure nobody passes back an unbound marker.
    (unless (boundp '*)
      (setq * nil)
      (fresh-line)
      (princ "Setting * to NIL -- was unbound marker."))))



;;;; Debug loop functions.

;;; These commands are function, not really commands, so users can get their
;;; hands on the values returned.
;;;

(eval-when (eval compile)

(defmacro define-var-operation (ref-or-set &optional value-var)
  `(let* ((temp (etypecase name
		  (symbol (di:debug-function-symbol-variables
			   (di:frame-debug-function *current-frame*)
			   name))
		  (simple-string (di:ambiguous-debug-variables
				  (di:frame-debug-function *current-frame*)
				  name))))
	  (location (di:frame-code-location *current-frame*))
	  ;; Let's only deal with valid variables.
	  (vars (remove-if-not #'(lambda (v)
				   (eq (di:debug-variable-validity v location)
				       :valid))
			       temp)))
     (declare (list vars))
     (cond ((null vars)
	    (error "No known valid variables match ~S." name))
	   ((= (length vars) 1)
	    ,(ecase ref-or-set
	       (:ref
		'(di:debug-variable-value (car vars) *current-frame*))
	       (:set
		`(setf (di:debug-variable-value (car vars) *current-frame*)
		       ,value-var))))
	   (t
	    ;; Since we have more than one, first see if we have any
	    ;; variables that exactly match the specification.
	    (let* ((name (etypecase name
			   (symbol (symbol-name name))
			   (simple-string name)))
		   (exact (remove-if-not #'(lambda (v)
					     (string= (di:debug-variable-name v)
						      name))
					 vars))
		   (vars (or exact vars)))
	      (declare (simple-string name)
		       (list exact vars))
	      (cond
	       ;; Check now for only having one variable.
	       ((= (length vars) 1)
		,(ecase ref-or-set
		   (:ref
		    '(di:debug-variable-value (car vars) *current-frame*))
		   (:set
		    `(setf (di:debug-variable-value (car vars) *current-frame*)
			   ,value-var))))
	       ;; If there weren't any exact matches, flame about ambiguity
	       ;; unless all the variables have the same name.
	       ((and (not exact)
		     (find-if-not
		      #'(lambda (v)
			  (string= (di:debug-variable-name v)
				   (di:debug-variable-name (car vars))))
		      (cdr vars)))
		(error "Specification ambiguous:~%~{   ~A~%~}"
		       (mapcar #'di:debug-variable-name
			       (delete-duplicates
				vars :test #'string=
				:key #'di:debug-variable-name))))
	       ;; All names are the same, so see if the user ID'ed one of them.
	       (id-supplied
		(let ((v (find id vars :key #'di:debug-variable-id)))
		  (unless v
		    (error "Invalid variable ID, ~D, should have been one of ~S."
			   id (mapcar #'di:debug-variable-id vars)))
		  ,(ecase ref-or-set
		     (:ref
		      '(di:debug-variable-value v *current-frame*))
		     (:set
		      `(setf (di:debug-variable-value v *current-frame*)
			     ,value-var)))))
	       (t
		(error "Specify variable ID to disambiguate ~S.  Use one of ~S."
		       name (mapcar #'di:debug-variable-id vars)))))))))

) ;EVAL-WHEN

;;; VAR -- Public.
;;;
(defun var (name &optional (id 0 id-supplied))
  "Returns a variable's value if possible.  Name is a simple-string or symbol.
   If it is a simple-string, it is an initial substring of the variable's name.
   If name is a symbol, it has the same name and package as the variable whose
   value this function returns.  If the symbol is uninterned, then the variable
   has the same name as the symbol, but it has no package.

   If name is the initial substring of variables with different names, then
   this return no values after displaying the ambiguous names.  If name
   determines multiple variables with the same name, then you must use the
   optional id argument to specify which one you want.  If you left id
   unspecified, then this returns no values after displaying the distinguishing
   id values.

   The result of this function is limited to the availability of variable
   information.  This is SETF'able."
  (define-var-operation :ref))
;;;
(defun (setf var) (value name &optional (id 0 id-supplied))
  (define-var-operation :set value))



;;; ARG -- Public.
;;;
(defun arg (n)
  "Returns the n'th argument's value if possible.  Argument zero is the first
   argument in a frame's default printed representation.  Count keyword/value
   pairs as separate arguments."
  (multiple-value-bind
      (var lambda-var-p)
      (nth-arg n (handler-case (di:debug-function-lambda-list
				(di:frame-debug-function *current-frame*))
		   (di:lambda-list-unavailable ()
		     (error "No argument values are available."))))
    (if lambda-var-p
	(lambda-var-dispatch var (di:frame-code-location *current-frame*)
	  (error "Unused arguments have no values.")
	  (di:debug-variable-value var *current-frame*)
	  (error "Invalid argument value."))
	var)))

;;; NTH-ARG -- Internal.
;;;
;;; This returns the n'th arg as the user sees it from args, the result of
;;; DI:DEBUG-FUNCTION-LAMBDA-LIST.  If this returns a potential debug-variable
;;; from the lambda-list, then the second value is t.  If this returns a
;;; keyword symbol or a value from a rest arg, then the second value is nil.
;;;
(defun nth-arg (count args)
  (let ((n count))
    (dolist (ele args (error "Argument specification out of range -- ~S." n))
      (lambda-list-element-dispatch ele
	:required ((if (zerop n) (return (values ele t))))
	:optional ((if (zerop n) (return (values (second ele) t))))
	:keyword ((cond ((zerop n)
			 (return (values (second ele) nil)))
			((zerop (decf n))
			 (return (values (third ele) t)))))
	:deleted ((if (zerop n) (return (values ele t))))
	:rest ((let ((var (second ele)))
		 (lambda-var-dispatch var
				      (di:frame-code-location *current-frame*)
		   (error "Unused rest-arg before n'th argument.")
		   (dolist (value
			    (di:debug-variable-value var *current-frame*)
			    (error "Argument specification out of range -- ~S."
				   n))
		     (if (zerop n)
			 (return-from nth-arg (values value nil))
			 (decf n)))
		   (error "Invalid rest-arg before n'th argument.")))))
      (decf n))))



;;;; Debug loop command definition:

(defvar *debug-commands* nil)

;;; DEF-DEBUG-COMMAND -- Internal.
;;;
;;; Interface to *debug-commands*.  No required arguments in args are
;;; permitted.
;;;
(defmacro def-debug-command (name args &rest body)
  (let ((fun-name (intern (concatenate 'simple-string name "-DEBUG-COMMAND"))))
    `(progn
       (when (assoc ,name *debug-commands* :test #'string=)
	 (setf *debug-commands*
	       (remove ,name *debug-commands* :key #'car :test #'string=)))
       (defun ,fun-name ,args
	 (unless *in-the-debugger*
	   (error "Invoking debugger command while outside the debugger."))
	 ,@body)
       (push (cons ,name #',fun-name) *debug-commands*)
       ',fun-name)))

;;; DEF-DEBUG-COMMAND-ALIAS -- Internal.
;;;
(defun def-debug-command-alias (new-name existing-name)
  (let ((pair (assoc existing-name *debug-commands* :test #'string=)))
    (unless pair (error "Unknown debug command name -- ~S" existing-name))
    (push (cons new-name (cdr pair)) *debug-commands*))
  new-name)

;;; DEBUG-COMMAND-P -- Internal.
;;;
;;; This takes a symbol and uses its name to find a debugger command, using
;;; initial substring matching.  It returns the command function if form
;;; identifies only one command, but if form is ambiguous, this returns a list
;;; of the command names.  If there are no matches, this returns nil.  Whenever
;;; the loop that looks for a set of possibilities encounters an exact name
;;; match, we return that command function immediately.
;;;
(defun debug-command-p (form &optional other-commands)
  (if (or (symbolp form) (integerp form))
      (let* ((name
	      (if (symbolp form)
		  (symbol-name form)
		  (format nil "~d" form)))
	     (len (length name))
	     (res nil))
	(declare (simple-string name)
		 (fixnum len)
		 (list res))
	;;
	;; Find matching commands, punting if exact match.
	(flet ((match-command (ele)
	         (let* ((str (car ele))
			(str-len (length str)))
		   (declare (simple-string str)
			    (fixnum str-len))
		   (cond ((< str-len len))
			 ((= str-len len)
			  (when (string= name str :end1 len :end2 len)
			    (return-from debug-command-p (cdr ele))))
			 ((string= name str :end1 len :end2 len)
			  (push ele res))))))
	  (mapc #'match-command *debug-commands*)
	  (mapc #'match-command other-commands))
	;;
	;; Return the right value.
	(cond ((not res) nil)
	      ((= (length res) 1)
	       (cdar res))
	      (t ;Just return the names.
	       (do ((cmds res (cdr cmds)))
		   ((not cmds) res)
		 (setf (car cmds) (caar cmds))))))))


;;;
;;; Returns a list of debug commands (in the same format as *debug-commands*)
;;; that invoke each active restart.
;;;
;;; Two commands are made for each restart: one for the number, and one for
;;; the restart name (unless it's been shadowed by an earlier restart of the
;;; same name, or it is nil).
;;;
(defun make-restart-commands (&optional (restarts *debug-restarts*))
  (let ((commands)
	(num 0))			; better be the same as show-restarts!
    (dolist (restart restarts)
      (let ((name (string (restart-name restart))))
	;;
	;; Use %Invoke-Restart-Interactively because the dynamic
	;; environment when the debugger invokes the restart can be
	;; different from the dynamic environment when the debugger
	;; computes active restarts.  If this is the case,
	;; Invoke-Restart-Interactively might find that the restart
	;; being invoked is not currently active and signal a
	;; Control-Error.
        (let ((restart-fun
	       (lambda ()
		 (conditions::%invoke-restart-interactively restart))))
	  (push (cons (format nil "~d" num) restart-fun) commands)
	  (unless (or (null (restart-name restart)) 
	              (find name commands :key #'car :test #'string=))
	    (push (cons name restart-fun) commands))))
      (incf num))
    commands))


;;;
;;; Frame changing commands.
;;;

(def-debug-command "UP" ()
  (let ((next (di:frame-up *current-frame*)))
    (cond (next
	   (setf *current-frame* next)
	   (print-frame-call next))
	  (t
	   (format t "~&Top of stack.")))))
  
(def-debug-command "DOWN" ()
  (let ((next (di:frame-down *current-frame*)))
    (cond (next
	   (setf *current-frame* next)
	   (print-frame-call next))
	  (t
	   (format t "~&Bottom of stack.")))))

(def-debug-command-alias "D" "DOWN")

(def-debug-command "TOP" ()
  (do ((prev *current-frame* lead)
       (lead (di:frame-up *current-frame*) (di:frame-up lead)))
      ((null lead)
       (setf *current-frame* prev)
       (print-frame-call prev))))

(def-debug-command "BOTTOM" ()
  (do ((prev *current-frame* lead)
       (lead (di:frame-down *current-frame*) (di:frame-down lead)))
      ((null lead)
       (setf *current-frame* prev)
       (print-frame-call prev))))


(def-debug-command-alias "B" "BOTTOM")

(def-debug-command "FRAME" (&optional
			    (n (read-prompting-maybe "Frame number: ")))
  (let ((current (di:frame-number *current-frame*)))
    (cond ((= n current)
	   (princ "You are here."))
	  ((> n current)
	   (print-frame-call
	    (setf *current-frame*
		  (do ((prev *current-frame* lead)
		       (lead (di:frame-down *current-frame*)
			     (di:frame-down lead)))
		      ((null lead)
		       (princ "Bottom of stack encountered.")
		       prev)
		    (when (= n (di:frame-number prev))
		      (return prev))))))
	  (t
	   (print-frame-call
	    (setf *current-frame*
		  (do ((prev *current-frame* lead)
		       (lead (di:frame-up *current-frame*)
			     (di:frame-up lead)))
		      ((null lead)
		       (princ "Top of stack encountered.")
		       prev)
		    (when (= n (di:frame-number prev))
		      (return prev)))))))))

(def-debug-command-alias "F" "FRAME")

;; debug-return, equivalent to return-from-frame in some other lisps,
;; allows us to return an arbitrary value from any frame
(def-debug-command "DEBUG-RETURN" (&optional
				   (return (read-prompting-maybe
					    "debug-return: ")))
  (unless (di:return-from-frame *current-frame* return)
    ;; the "unless" here is for aesthetical purposes only. If all goes
    ;; well with return-from-frame, the code after it will never get
    ;; reached anyway.
    (format t "~@<can't find a tag for this frame ~
                   ~2I~_(hint: try increasing the DEBUG optimization quality ~
                   and recompiling)~:@>")))

(def-debug-command-alias "R" "DEBUG-RETURN")


;;;
;;; In and Out commands.
;;;

(def-debug-command "QUIT" ()
  (throw 'lisp::top-level-catcher nil))

(def-debug-command "GO" ()
  (continue *debug-condition*)
  (error "No restart named continue."))

(def-debug-command "RESTART" ()
  (let ((num (read-if-available :prompt)))
    (when (eq num :prompt)
      (show-restarts *debug-restarts*)
      (write-string "Restart: ")
      (force-output)
      (setf num (read *standard-input*)))
    (let ((restart (typecase num
		     (unsigned-byte
		      (nth num *debug-restarts*))
		     (symbol
		      (find num *debug-restarts* :key #'restart-name
			    :test #'(lambda (sym1 sym2)
				      (string= (symbol-name sym1)
					       (symbol-name sym2)))))
		     (t
		      (format t "~S is invalid as a restart name.~%" num)
		      (return-from restart-debug-command nil)))))
      (if restart
	  (invoke-restart-interactively restart)
	  (princ "No such restart.")))))


;;;
;;; Information commands.
;;;
 
(defvar *help-line-scroll-count* 20
  "This controls how many lines the debugger's help command prints before
   printing a prompting line to continue with output.")

(def-debug-command "HELP" ()
  (let* ((end -1)
	 (len (length debug-help-string))
	 (len-1 (1- len)))
    (loop
      (let ((start (1+ end))
	    (count *help-line-scroll-count*))
	(loop
	  (setf end (position #\newline debug-help-string :start (1+ end)))
	  (cond ((or (not end) (= end len-1))
		 (setf end len)
		 (return))
		((or (zerop (decf count)) (= end len))
		 (return))))
	(write-string debug-help-string *standard-output*
		      :start start :end end))
      (when (= end len) (return))
      (format t "~%[RETURN FOR MORE, Q TO QUIT HELP TEXT]: ")
      (force-output)
      (let ((res (read-line)))
	(when (or (string= res "q") (string= res "Q"))
	  (return))))))

(def-debug-command-alias "?" "HELP")

(def-debug-command "ERROR" ()
  (format t "~A~%" (safe-condition-message *debug-condition*))
  (show-restarts *debug-restarts*))

(def-debug-command "BACKTRACE" ()
  (backtrace (read-if-available most-positive-fixnum)))

(def-debug-command "PRINT" ()
  (print-frame-call *current-frame*))

(def-debug-command-alias "P" "PRINT")

(def-debug-command "VPRINT" ()
  (print-frame-call *current-frame* :print-level nil :print-length nil
		    :verbosity (read-if-available 2)))

(def-debug-command-alias "PP" "VPRINT")

(def-debug-command "LIST-LOCALS" ()
  (let ((d-fun (di:frame-debug-function *current-frame*)))
    (if (di:debug-variable-info-available d-fun)
	(let ((*print-level* (or *debug-print-level* *print-level*))
	      (*print-length* (or *debug-print-length* *print-length*))
	      (*standard-output* *debug-io*)
	      (location (di:frame-code-location *current-frame*))
	      (prefix (read-if-available nil))
	      (any-p nil)
	      (any-valid-p nil))
	  (dolist (v (di:ambiguous-debug-variables
			d-fun
			(if prefix (string prefix) "")))
	    (setf any-p t)
	    (when (eq (di:debug-variable-validity v location) :valid)
	      (setf any-valid-p t)
	      (format t "~S~:[#~D~;~*~]  =  ~S~%"
		      (di:debug-variable-symbol v)
		      (zerop (di:debug-variable-id v))
		      (di:debug-variable-id v)
		      (di:debug-variable-value v *current-frame*))))

	  (cond
	   ((not any-p)
	    (format t "No local variables ~@[starting with ~A ~]~
	               in function."
		    prefix))
	   ((not any-valid-p)
	    (format t "All variables ~@[starting with ~A ~]currently ~
	               have invalid values."
		    prefix))))
	(write-line "No variable information available."))))

(def-debug-command-alias "L" "LIST-LOCALS")

(def-debug-command "SOURCE" ()
  (fresh-line)
  (print-code-location-source-form (di:frame-code-location *current-frame*)
				   (read-if-available 0)))

(def-debug-command "VSOURCE" ()
  (fresh-line)
  (print-code-location-source-form (di:frame-code-location *current-frame*)
				   (read-if-available 0)
				   t))


;;;; Source location printing:

;;; We cache a stream to the last valid file debug source so that we won't have
;;; to repeatedly open the file.
;;;
(defvar *cached-debug-source* nil)
(declaim (type (or di:debug-source null) *cached-debug-source*))
(defvar *cached-source-stream* nil)
(declaim (type (or stream null) *cached-source-stream*))

;;; To suppress the read-time evaluation #. macro during source read
;;; the *readtable* is modified. The *readtable* is cached to avoid
;;; copying it each time, and invalidated when the
;;; *cached-debug-source* has changed.
(defvar *cached-readtable* nil)
(declaim (type (or readtable null) *cached-readtable*))

(pushnew #'(lambda ()
	     (setq *cached-debug-source* nil *cached-source-stream* nil
		   *cached-readtable* nil))
	 ext:*before-save-initializations*)


;;; We also cache the last top-level form that we printed a source for so that
;;; we don't have to do repeated reads and calls to FORM-NUMBER-TRANSLATIONS.
;;;
(defvar *cached-top-level-form-offset* nil)
(declaim (type (or kernel:index null) *cached-top-level-form-offset*))
(defvar *cached-top-level-form*)
(defvar *cached-form-number-translations*)


;;; GET-TOP-LEVEL-FORM  --  Internal
;;;
;;;    Given a code location, return the associated form-number translations
;;; and the actual top-level form.  We check our cache --- if there is a miss,
;;; we dispatch on the kind of the debug source.
;;;
(defun get-top-level-form (location)
  (let ((d-source (di:code-location-debug-source location)))
    (if (and (eq d-source *cached-debug-source*)
	     (eql (di:code-location-top-level-form-offset location)
		  *cached-top-level-form-offset*))
	(values *cached-form-number-translations* *cached-top-level-form*)
	(let* ((offset (di:code-location-top-level-form-offset location))
	       (res
		(ecase (di:debug-source-from d-source)
		  (:file (get-file-top-level-form location))
		  ((:lisp :stream)
		   (svref (di:debug-source-name d-source) offset)))))
	  (setq *cached-top-level-form-offset* offset)
	  (values (setq *cached-form-number-translations*
			(di:form-number-translations res offset))
		  (setq *cached-top-level-form* res))))))


;;; GET-FILE-TOP-LEVEL-FORM -- Internal.
;;;
;;; Locates the source file (if it still exists) and grabs the top-level form.
;;; If the file is modified, we use the top-level-form offset instead of the
;;; recorded character offset.
;;;
(defun get-file-top-level-form (location)
  (let* ((d-source (di:code-location-debug-source location))
	 (tlf-offset (di:code-location-top-level-form-offset location))
	 (local-tlf-offset (- tlf-offset
			      (di:debug-source-root-number d-source)))
	 (char-offset
	  (aref (or (di:debug-source-start-positions d-source)
		    (error "No start positions map."))
		local-tlf-offset))
	 (name (di:debug-source-name d-source)))
    (unless (eq d-source *cached-debug-source*)
      (unless (and *cached-source-stream*
		   (equal (pathname *cached-source-stream*)
			  (pathname name)))
	(setq *cached-readtable* nil)
	(when *cached-source-stream* (close *cached-source-stream*))
	(setq *cached-source-stream* (open name :if-does-not-exist nil))
	(unless *cached-source-stream*
	  (error "Source file no longer exists:~%  ~A." (namestring name)))
	(format t "~%; File: ~A~%" (namestring name)))

	(setq *cached-debug-source*
	      (if (= (di:debug-source-created d-source) (file-write-date name))
		  d-source nil)))

    (cond
     ((eq *cached-debug-source* d-source)
      (file-position *cached-source-stream* char-offset))
     (t
      (format t "~%; File has been modified since compilation:~%;   ~A~@
		 ; Using form offset instead of character position.~%"
	      (namestring name))
      (file-position *cached-source-stream* 0)
      (let ((*read-suppress* t))
	(dotimes (i local-tlf-offset)
	  (read *cached-source-stream*)))))
    (unless *cached-readtable*
      (setq *cached-readtable* (copy-readtable))
      (set-dispatch-macro-character
       #\# #\.
       #'(lambda (stream sub-char &rest rest)
	   (declare (ignore rest sub-char))
	   (let ((token (read stream t nil t)))
	     (format nil "#.~s" token)))
       *cached-readtable*))
    (let ((*readtable* *cached-readtable*))
      (read *cached-source-stream*))))


;;; PRINT-CODE-LOCATION-SOURCE-FORM -- Internal.
;;;
(defun print-code-location-source-form (location context &optional verbose)
  (let* ((location (maybe-block-start-location location))
	 (*print-level* (if verbose
			    nil
			    (or *debug-print-level* *print-level*)))
	 (*print-length* (if verbose
			     nil
			     (or *debug-print-length* *print-length*)))
	 (form-num (di:code-location-form-number location)))
    (multiple-value-bind (translations form)
			 (get-top-level-form location)
      (unless (< form-num (length translations))
	(error "Source path no longer exists."))
      (prin1 (di:source-path-context form
				     (svref translations form-num)
				     context)))))


;;;
;;; Breakpoint and step commands.
;;;

;;; Steps to the next code-location
(def-debug-command "STEP" ()
  (setf *number-of-steps* (read-if-available 1))
  (set-step-breakpoint *current-frame*)
  (continue *debug-condition*)
  (error "Couldn't continue."))
  
;;; Lists possible breakpoint locations, which are active, and where go will
;;; continue.  Sets *possible-breakpoints* to the code-locations which can then
;;; be used by sbreakpoint.  Takes a function as an optional argument.
(def-debug-command "LIST-LOCATIONS" ()
  (let ((df (read-if-available *default-breakpoint-debug-function*)))
    (cond ((consp df)
	   (setf df (di:function-debug-function (eval df)))
	   (setf *default-breakpoint-debug-function* df))	  
	  ((or (eq ':c df)
	       (not *default-breakpoint-debug-function*))
	   (setf df (di:frame-debug-function *current-frame*))
	   (setf *default-breakpoint-debug-function* df)))
    (setf *possible-breakpoints* (possible-breakpoints df)))
  (let ((continue-at (di:frame-code-location *current-frame*)))
    (let ((active (location-in-list *default-breakpoint-debug-function*
				    *breakpoints* :function-start))
	  (here (di:code-location=
		 (di:debug-function-start-location
		  *default-breakpoint-debug-function*) continue-at)))
      (when (or active here)
	(format t "::FUNCTION-START ")
	(when active (format t " *Active*"))
	(when here (format t " *Continue here*"))))
    
    (let ((prev-location nil)
	  (prev-num 0)
	  (this-num 0))
      (flet ((flush ()
	       (when prev-location
		 (let ((this-num (1- this-num)))
		   (if (= prev-num this-num)
		       (format t "~&~D: " prev-num)
		       (format t "~&~D-~D: " prev-num this-num)))
		 (print-code-location-source-form prev-location 0)
		 (when *print-location-kind*
		   (format t "~S " (di:code-location-kind prev-location)))
		 (when (location-in-list prev-location *breakpoints*)
		   (format t " *Active*"))
		 (when (di:code-location= prev-location continue-at)
		   (format t " *Continue here*")))))
	
	(dolist (code-location *possible-breakpoints*)
	  (when (or *print-location-kind*
		    (location-in-list code-location *breakpoints*)
		    (di:code-location= code-location continue-at)
		    (not prev-location)
		    (not (eq (di:code-location-debug-source code-location)
			     (di:code-location-debug-source prev-location)))
		    (not (eq (di:code-location-top-level-form-offset
			      code-location)
			     (di:code-location-top-level-form-offset
			      prev-location)))
		    (not (eq (di:code-location-form-number code-location)
			     (di:code-location-form-number prev-location))))
	    (flush)
	    (setq prev-location code-location  prev-num this-num))
	  
	  (incf this-num))))

    (when (location-in-list *default-breakpoint-debug-function* *breakpoints*
			    :function-end)
      (format t "~&::FUNCTION-END *Active* "))))

(def-debug-command-alias "LL" "LIST-LOCATIONS")
    
;;; set breakpoint at # given
(def-debug-command "BREAKPOINT" ()
  (let ((index (read-prompting-maybe "Location number, :start, or :end: "))
	(break t)
	(condition t)
	(print nil)
	(print-functions nil)
	(function nil)
	(bp)
	(place *default-breakpoint-debug-function*))
    (flet ((get-command-line ()
	     (let ((command-line nil)
		   (unique '(nil)))
	       (loop
		 (let ((next-input (read-if-available unique)))
		   (when (eq next-input unique) (return))
		   (push next-input command-line)))
	       (nreverse command-line)))
	   (set-vars-from-command-line (command-line)
	     (do ((arg (pop command-line) (pop command-line)))
		 ((not arg))
	       (ecase arg
		 (:condition (setf condition (pop command-line)))
		 (:print (push (pop command-line) print))
		 (:break (setf break (pop command-line)))
		 (:function
		  (setf function (eval (pop command-line)))
		  (setf *default-breakpoint-debug-function*
			(di:function-debug-function function))
                  (setf place *default-breakpoint-debug-function*)
		  (setf *possible-breakpoints*
			(possible-breakpoints
			 *default-breakpoint-debug-function*))))))
	   (setup-function-start ()
	     (let ((code-loc (di:debug-function-start-location place)))
	       (setf bp (di:make-breakpoint #'main-hook-function place
					    :kind :function-start))
	       (setf break (di:preprocess-for-eval break code-loc))
	       (setf condition (di:preprocess-for-eval condition code-loc))
	       (dolist (form print)
		 (push (cons (di:preprocess-for-eval form code-loc) form)
		       print-functions))))
	   (setup-function-end ()
	     (setf bp
		   (di:make-breakpoint #'main-hook-function place
					  :kind :function-end))
	     (setf break
		   (coerce `(lambda (dummy)
				    (declare (ignore dummy)) ,break)
				 'function))
	     (setf condition (coerce `(lambda (dummy)
					(declare (ignore dummy)) ,condition)
				     'function))
	     (dolist (form print)
	       (push (cons
		      (coerce `(lambda (dummy)
				 (declare (ignore dummy)) ,form) 'function)
		      form)
		     print-functions)))
	   (setup-code-location ()
	     (setf place (nth index *possible-breakpoints*))
	     (setf bp (di:make-breakpoint #'main-hook-function place
					  :kind :code-location))
	     (dolist (form print)
	       (push (cons
		      (di:preprocess-for-eval form place)
		      form)
		     print-functions))
	     (setf break (di:preprocess-for-eval break place))
	     (setf condition (di:preprocess-for-eval condition place))))
      (set-vars-from-command-line (get-command-line))
      (cond
       ((or (eq index :start) (eq index :s))
	(setup-function-start))
       ((or (eq index :end) (eq index :e))
	(setup-function-end))
       (t
	(setup-code-location)))
      (di:activate-breakpoint bp)
      (let* ((new-bp-info (create-breakpoint-info place bp index
						  :break break
						  :print print-functions
						  :condition condition))
	     (old-bp-info (location-in-list new-bp-info *breakpoints*)))
	(when old-bp-info
	  (di:deactivate-breakpoint (breakpoint-info-breakpoint old-bp-info))
	  (setf *breakpoints* (remove old-bp-info *breakpoints*))
	  (format t "Note: previous breakpoint removed.~%"))
	(push new-bp-info *breakpoints*))
      (print-breakpoint-info (first *breakpoints*))
      (format t "~&Added."))))

(def-debug-command-alias "BP" "BREAKPOINT")

;;; list all breakpoints set
(def-debug-command "LIST-BREAKPOINTS" ()
  (setf *breakpoints*
	(sort *breakpoints* #'< :key #'breakpoint-info-breakpoint-number))
  (dolist (info *breakpoints*)
    (print-breakpoint-info info)))

(def-debug-command-alias "LB" "LIST-BREAKPOINTS")
(def-debug-command-alias "LBP" "LIST-BREAKPOINTS")

;;; remove breakpoint n or all if none given
(def-debug-command "DELETE-BREAKPOINT" ()
  (let* ((index (read-if-available nil))
	 (bp-info
	  (find index *breakpoints* :key #'breakpoint-info-breakpoint-number)))
    (cond (bp-info
	   (di:delete-breakpoint (breakpoint-info-breakpoint bp-info))
	   (setf *breakpoints* (remove bp-info *breakpoints*))
	   (format t "Breakpoint ~S removed.~%" index))
	  (index (format t "Breakpoint doesn't exist."))
	  (t
	   (dolist (ele *breakpoints*)
	     (di:delete-breakpoint (breakpoint-info-breakpoint ele)))
	   (setf *breakpoints* nil)
	   (format t "All breakpoints deleted.~%")))))

(def-debug-command-alias "DBP" "DELETE-BREAKPOINT")


;;;
;;; Miscellaneous commands.
;;;

(def-debug-command "FLUSH-ERRORS" ()
  (if (setf *flush-debug-errors* (not *flush-debug-errors*))
      (write-line "Errors now flushed.")
      (write-line "Errors now create nested debug levels.")))


(def-debug-command "DESCRIBE" ()
  (let* ((curloc (di:frame-code-location *current-frame*))
	 (debug-fun (di:code-location-debug-function curloc))
	 (function (di:debug-function-function debug-fun)))
    (if function
	(describe function)
	(format t "Can't figure out the function for this frame."))))


;;;
;;; Editor commands.
;;;

(def-debug-command "EDIT-SOURCE" ()
  (unless (ed::ts-stream-p *terminal-io*)
    (error "The debugger's EDIT-SOURCE command only works in slave Lisps ~
	    connected to a Hemlock editor."))
  (let* ((wire (ed::ts-stream-wire *terminal-io*))
	 (location (maybe-block-start-location
		    (di:frame-code-location *current-frame*)))
	 (d-source (di:code-location-debug-source location))
	 (name (di:debug-source-name d-source)))
    (ecase (di:debug-source-from d-source)
      (:file
       (let* ((tlf-offset (di:code-location-top-level-form-offset location))
	      (local-tlf-offset (- tlf-offset
				   (di:debug-source-root-number d-source)))
	      (char-offset (aref (or (di:debug-source-start-positions d-source)
				     (error "No start positions map."))
				 local-tlf-offset)))
	 (wire:remote wire
	   (ed::edit-source-location (namestring name)
				     (di:debug-source-created d-source)
				     tlf-offset local-tlf-offset char-offset
				     (di:code-location-form-number location)))
	 (wire:wire-force-output wire)))
      ((:lisp :stream)
       (wire:remote wire
	 (ed::cannot-edit-source-location))
       (wire:wire-force-output wire)))))



;;;; Debug loop command utilities.

(defun read-prompting-maybe (prompt &optional (in *standard-input*)
				    (out *standard-output*))
  (unless (ext:listen-skip-whitespace in)
    (princ prompt out)
    (force-output out))
  (read in))

(defun read-if-available (default &optional (stream *standard-input*))
  (if (ext:listen-skip-whitespace stream)
      (read stream)
      default))
