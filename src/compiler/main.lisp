;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/main.lisp,v 1.146 2005/07/01 13:00:26 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the top-level interfaces to the compiler.
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package "C")
(in-package "EXTENSIONS")
(export '(*compile-progress* compile-from-stream *block-compile-default*
			     start-block end-block
			     *byte-compile-default*
			     *byte-compile-top-level*))
(in-package "LISP")
(export '(*compile-verbose* *compile-print* *compile-file-pathname*
			    *compile-file-truename*
			    compile-file-pathname))
(in-package "C")

(declaim (special *constants* *free-variables* *compile-component*
		  *code-vector* *next-location* *result-fixups*
		  *free-functions* *source-paths*
		  *continuation-number* *continuation-numbers*
		  *number-continuations* *tn-id* *tn-ids* *id-tns*
		  *label-ids* *label-id* *id-labels*
		  *undefined-warnings* *compiler-error-count*
		  *compiler-warning-count* *compiler-note-count*
		  *compiler-error-output* *compiler-error-bailout*
		  *compiler-trace-output*
		  *last-source-context* *last-original-source*
		  *last-source-form* *last-format-string* *last-format-args*
		  *last-message-count* *lexical-environment*
		  *coalesce-constants*))

;;; Exported:
(defvar *block-compile-default* :specified
  "The default value for the :Block-Compile argument to COMPILE-FILE.")
(declaim (type (member t nil :specified) *block-compile-default*))

;;; Exported:
(defvar *byte-compile-default* :maybe
  "The default value for the :Byte-Compile argument to COMPILE-FILE.")

;;; Exported:
(defvar *byte-compile-top-level* t
  "Similar to *BYTE-COMPILE-DEFAULT*, but controls the compilation of top-level
   forms (evaluated at load-time) when the :BYTE-COMPILE argument is :MAYBE
   (the default.)  When true, we decide to byte-compile.")

;;; Exported:
(defvar *loop-analyze* nil
  "Whether loop analysis should be done or not.")

;;; Value of the :byte-compile argument to the compiler.
(defvar *byte-compile* :maybe)

;;; Bound by COMPILE-COMPONENT to T when byte-compiling, and NIL when
;;; native compiling.  During IR1 conversion this can also be :MAYBE, in which
;;; case we must look at the policy, see (byte-compiling).
;;;
(defvar *byte-compiling* :maybe)
(declaim (type (member t nil :maybe) *byte-compile* *byte-compiling*
	       *byte-compile-default*))

(defvar compiler-version "1.1")
(pushnew :python *features*)
(setf (getf ext:*herald-items* :python)
      `("    Python " ,compiler-version ", target "
	,#'(lambda (stream)
	     (write-string (backend-version *backend*) stream))))

(defvar *check-consistency* nil)

(defvar *record-xref-info* nil
  "Whether the compiler should record cross-reference information.")

(defvar *all-components*)

;;; The current block compilation state.  These are initialized to the 
;;; :Block-Compile and :Entry-Points arguments that COMPILE-FILE was called
;;; with.  Subsequent START-BLOCK or END-BLOCK declarations alter the values.
;;;
;;; *Block-Compile-Argument* holds the original value of the :block-compile
;;; argument, which overrides any internal declarations.
;;;
(defvar *block-compile*)
(defvar *block-compile-argument*)
(declaim (type (member nil t :specified)
	       *block-compile* *block-compile-argument*))
(defvar *entry-points*)
(declaim (list *entry-points*))

;;;

;;; When block compiling, used by PROCESS-FORM to accumulate top-level lambdas
;;; resulting from compiling subforms.  (In reverse order.)
;;;
(defvar *top-level-lambdas*)
(declaim (list *top-level-lambdas*))

(defvar *compile-verbose* t
  "The default for the :VERBOSE argument to COMPILE-FILE.")
(defvar *compile-print* t
  "The default for the :PRINT argument to COMPILE-FILE.")
(defvar *compile-progress* nil
  "The default for the :PROGRESS argument to COMPILE-FILE.")

(defvar *compile-file-pathname* nil
  "The defaulted pathname of the file currently being compiled, or NIL if not
  compiling.")
(defvar *compile-file-truename* nil
  "The TRUENAME of the file currently being compiled, or NIL if not
  compiling.")

(declaim (type (or pathname null) *compile-file-pathname*
	       *compile-file-truename*))

;;; The values of *Package* and policy when compilation started.
;;;
(defvar *initial-package*)
(defvar *initial-cookie*)
(defvar *initial-interface-cookie*)

;;; The source-info structure for the current compilation.  This is null
;;; globally to indicate that we aren't currently in any identifiable
;;; compilation.
;;;
(defvar *source-info* nil)

(defvar *user-source-info* nil
  "The user supplied source-info for the current compilation.  
This is the :source-info argument to COMPILE-FROM-STREAM and will be
stored in the INFO slot of the DEBUG-SOURCE in code components and 
in the user USER-INFO slot of STREAM-SOURCE-LOCATIONs.")

;;; Maybe-Mumble  --  Internal
;;;
;;;    Mumble conditional on *compile-progress*.
;;;
(defun maybe-mumble (&rest foo)
  (when *compile-progress*
    (apply #'compiler-mumble foo)))


(deftype object () '(or fasl-file core-object null))

(defvar *compile-object* nil)
(declaim (type object *compile-object*))



;;;; Component compilation:

(defparameter max-optimize-iterations 6
  "The upper limit on the number of times that we will consecutively do IR1
  optimization that doesn't introduce any new code.  A finite limit is
  necessary, since type inference may take arbitrarily long to converge.")

(defevent ir1-optimize-until-done "IR1-OPTIMIZE-UNTIL-DONE called.")
(defevent ir1-optimize-maxed-out "Hit MAX-OPTIMIZE-ITERATIONS limit.")

;;; IR1-Optimize-Until-Done  --  Internal
;;;
;;;    Repeatedly optimize Component until no further optimizations can be
;;; found or we hit our iteration limit.  When we hit the limit, we clear the
;;; component and block REOPTIMIZE flags to discourage the following
;;; optimization attempt from pounding on the same code.
;;;
(defun ir1-optimize-until-done (component)
  (declare (type component component))
  (maybe-mumble "Opt")
  (event ir1-optimize-until-done)
  (let ((count 0)
	(cleared-reanalyze nil))
    (loop
      (when (component-reanalyze component)
	(setf count 0)
	(setf cleared-reanalyze t)
	(setf (component-reanalyze component) nil))
      (setf (component-reoptimize component) nil)
      (ir1-optimize component)
      (cond ((component-reoptimize component)
	     (incf count)
	     (when (= count max-optimize-iterations)
	       (maybe-mumble "*")
	       (cond ((retry-delayed-transforms :optimize)
		      (maybe-mumble "+")
		      (setf count 0))
		     (t
		      (event ir1-optimize-maxed-out)
		      (setf (component-reoptimize component) nil)
		      (do-blocks (block component)
			(setf (block-reoptimize block) nil))
		      (return)))))
	    ((retry-delayed-transforms :optimize)
	     (setf count 0)
	     (maybe-mumble "+"))
	    (t
	     (return)))
      (maybe-mumble "."))
    (when cleared-reanalyze
      (setf (component-reanalyze component) t))
    (maybe-mumble " "))
  (undefined-value))

(defparameter *constraint-propagate* t)
(defparameter *reoptimize-after-type-check-max* 10)

(defevent reoptimize-maxed-out
  "*REOPTIMIZE-AFTER-TYPE-CHECK-MAX* exceeded.")


;;; DFO-AS-NEEDED  --  Internal
;;;
;;;    Iterate doing FIND-DFO until no new dead code is discovered.
;;;
(defun dfo-as-needed (component)
  (declare (type component component))
  (when (component-reanalyze component)
    (maybe-mumble "DFO")
    (loop
      (find-dfo component)
      (unless (component-reanalyze component)
	(maybe-mumble " ")
	(return))
      (maybe-mumble ".")))
  (undefined-value))


;;; IR1-Phases  --  Internal
;;;
;;;    Do all the IR1 phases for a non-top-level component.
;;;
(defun ir1-phases (component)
  (declare (type component component))
  (let ((*constraint-number* 0)
	(loop-count 1)
	(*delayed-transforms* nil))
    (declare (special *constraint-number* *delayed-transforms*))
    (loop
     (ir1-optimize-until-done component)
     (when (or (component-new-functions component)
	       (component-reanalyze-functions component))
       (maybe-mumble "Locall ")
       (local-call-analyze component))
     (dfo-as-needed component)
     (when *constraint-propagate*
       (maybe-mumble "Constraint ")
       (constraint-propagate component))
     (when (retry-delayed-transforms :constraint)
       (maybe-mumble "Rtran "))
     ;; Delay the generation of type checks until the type constraints have
     ;; had time to propagate, else the compiler can confuse itself.
     (unless (and (or (component-reoptimize component)
		      (component-reanalyze component)
		      (component-new-functions component)
		      (component-reanalyze-functions component))
		  (< loop-count (- *reoptimize-after-type-check-max* 4)))
       (maybe-mumble "Type ")
       (generate-type-checks component)
       (unless (or (component-reoptimize component)
		   (component-reanalyze component)
		   (component-new-functions component)
		   (component-reanalyze-functions component))
	 (return)))
     (when (>= loop-count *reoptimize-after-type-check-max*)
       (maybe-mumble "[Reoptimize Limit]")
       (event reoptimize-maxed-out)
       (return))
     (incf loop-count)))

  (ir1-finalize component)
  (undefined-value))


;;; Native-Compile-Component  --  Internal
;;;
(defun native-compile-component (component)
  (let ((*code-segment* nil)
	(*elsewhere* nil)
	(*elsewhere-label* nil))
    (maybe-mumble "GTN ")
    (gtn-analyze component)
    (maybe-mumble "LTN ")
    (ltn-analyze component)
    (dfo-as-needed component)
    (maybe-mumble "Control ")
    (control-analyze component #'make-ir2-block)

    (when (ir2-component-values-receivers (component-info component))
      (maybe-mumble "Stack ")
      (stack-analyze component)
      ;;
      ;; Assign BLOCK-NUMBER for any cleanup blocks introduced by stack
      ;; analysis.  There shouldn't be any unreachable code after control, so
      ;; this won't delete anything.
      (dfo-as-needed component))

    (unwind-protect
	(progn
	  (maybe-mumble "IR2Tran ")
	  (init-assembler)
	  (entry-analyze component)
	  (ir2-convert component)
	  
	  (when (policy nil (>= speed cspeed))
	    (maybe-mumble "Copy ")
	    (copy-propagate component))
	  
	  (select-representations component)
	  
	  (when *check-consistency*
	    (maybe-mumble "Check2 ")
	    (check-ir2-consistency component))
	  
	  (delete-unreferenced-tns component)
	  
	  (maybe-mumble "Life ")
	  (lifetime-analyze component)
	  
	  (when *compile-progress*
	    (compiler-mumble "") ; Sync before doing random output.
	    (pre-pack-tn-stats component *compiler-error-output*))
	  
	  (when *check-consistency*
	    (maybe-mumble "CheckL ")
	    (check-life-consistency component))

	  (maybe-mumble "Pack ")
	  (pack component)
	  
	  (when *check-consistency*
	    (maybe-mumble "CheckP ")
	    (check-pack-consistency component))
	  
	  (when *compiler-trace-output*
	    (describe-component component *compiler-trace-output*)
	    (describe-ir2-component component *compiler-trace-output*))
	  
	  (maybe-mumble "Code ")
	  (multiple-value-bind
	      (length trace-table fixups)
	      (generate-code component)

	    (when (and *compiler-trace-output*
		       (backend-disassem-params *backend*))
	      (format *compiler-trace-output*
		      "~|~%Disassembly of code for ~S~2%" component)
	      (disassem:disassemble-assem-segment *code-segment*
						  *compiler-trace-output*
						  *backend*))

	    (etypecase *compile-object*
	      (fasl-file
	       (maybe-mumble "FASL")
	       (fasl-dump-component component *code-segment*
				    length trace-table fixups
				    *compile-object*))
	      (core-object
	       (maybe-mumble "Core")
	       (make-core-component component *code-segment*
				    length trace-table fixups
				    *compile-object*))
	      (null))))

      (when *code-segment*
	(new-assem:release-segment *code-segment*))
      (when *elsewhere*
	(new-assem:release-segment *elsewhere*))))

  ;; We are done, so don't bother keeping anything around.
  (nuke-ir2-component component)
  (setf (component-info component) nil)
  
  (undefined-value))


;;; BYTE-COMPILING  --  Interface
;;;
;;;    Return our best guess for whether we will byte compile code currently
;;; being IR1 converted.  Only a guess because the decision is made on a
;;; per-component basis.
;;;
(defun byte-compiling ()
  (if (eq *byte-compiling* :maybe)
      (or (eq *byte-compile* t)
	  (policy nil (zerop speed) (<= debug 1)))
      (and *byte-compile* *byte-compiling*)))


;;; DELETE-IF-NO-ENTRIES  --  Internal
;;;
;;;    Delete components with no external entry points before we try to
;;; generate code.  Unreachable closures can cause IR2 conversion to puke on
;;; itself, since it is the reference to the closure which normally causes the
;;; components to be combined.  This doesn't really cover all cases...
;;;
(defun delete-if-no-entries (component)
  (dolist (fun (component-lambdas component)
	       (delete-component component))
    (case (functional-kind fun)
      (:top-level (return))
      (:external
       (unless (every #'(lambda (ref)
			  (eq (block-component (node-block ref))
			      component))
		      (leaf-refs fun))
	 (return))))))

  
;;; COMPILE-COMPONENT -- internal.
;;;
(defun compile-component (component)
  (let* ((*compile-component* component)
	 (*byte-compiling*
	  (ecase *byte-compile*
	    ((t) t)
	    ((nil) nil)
	    (:maybe
	     (dolist (fun (component-lambdas component) t)
	       (unless (policy (lambda-bind fun)
			       (zerop speed) (<= debug 1))
		 (return nil)))))))

    (when *compile-print*
      (compiler-mumble "~&")
      (pprint-logical-block (*compiler-error-output* nil :per-line-prefix "; ")
	(compiler-mumble "~:[~;Byte ~]Compiling ~A: "
		       *byte-compiling*
		       (component-name component))))

    (ir1-phases component)

    (when *loop-analyze*
      (dfo-as-needed component)
      (maybe-mumble "Dom ")
      (find-dominators component)
      (maybe-mumble "Loop ")
      (loop-analyze component))


    (maybe-mumble "Env ")
    (environment-analyze component)
    (dfo-as-needed component)

    (delete-if-no-entries component)

    (when *record-xref-info*
      (maybe-mumble "[record-xref-info]~%")
      (record-component-xrefs component))

    (unless (eq (block-next (component-head component))
		(component-tail component))
      (if *byte-compiling*
	  (byte-compile-component component)
	  (native-compile-component component))))

  (clear-constant-info)

  (when *compile-print*
    (compiler-mumble "~&"))

  (undefined-value))


;;;; Clearing global data structures:

;;; CLEAR-CONSTANT-INFO  --  Internal
;;;
;;;    Clear the INFO in constants in the *FREE-VARIABLES*, etc.  In addition
;;; to allowing stuff to be reclaimed, this is required for correct assignment
;;; of constant offsets, since we need to assign a new offset for each
;;; component.  We don't clear the FUNCTIONAL-INFO slots, since they are used
;;; to keep track of functions across component boundaries.
;;;
(defun clear-constant-info ()
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (setf (leaf-info v) nil))
	   *constants*)

  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (when (constant-p v)
		 (setf (leaf-info v) nil)))
	   *free-variables*)

  (undefined-value))


;;; CLEAR-IR1-INFO  --  Internal
;;;
;;;    Blow away the REFS for all global variables, and recycle the IR1 for
;;; Component.
;;;
(defun clear-ir1-info (component)
  (declare (type component component))
  (labels ((blast (x)
	     (maphash #'(lambda (k v)
			  (declare (ignore k))
			  (when (leaf-p v)
			    (setf (leaf-refs v)
				  (delete-if #'here-p (leaf-refs v)))
			    (when (basic-var-p v)
			      (setf (basic-var-sets v)
				    (delete-if #'here-p (basic-var-sets v))))))
		      x))
	   (here-p (x)
	     (eq (block-component (node-block x)) component)))
    (blast *free-variables*)
    (blast *free-functions*)
    (blast *constants*))
  (macerate-ir1-component component)
  (undefined-value))


;;; CLEAR-STUFF  --  Interface
;;;
;;;    Clear all the global variables used by the compiler.
;;;
(defun clear-stuff (&optional (debug-too t))
  ;;
  ;; Clear global tables.
  (when (boundp '*free-functions*)
    (clrhash *free-functions*)
    (clrhash *free-variables*)
    (clrhash *constants*))

  (when debug-too
    (clrhash *continuation-numbers*)
    (clrhash *number-continuations*)
    (setq *continuation-number* 0)
    (clrhash *tn-ids*)
    (clrhash *id-tns*)
    (setq *tn-id* 0)
    (clrhash *label-ids*)
    (clrhash *id-labels*)
    (setq *label-id* 0)
    ;;
    ;; Clear some Pack data structures (for GC purposes only.)
    (assert (not *in-pack*))
    (dolist (sb (backend-sb-list *backend*))
      (when (finite-sb-p sb)
	(fill (finite-sb-live-tns sb) nil))))
  ;;
  ;; Reset Gensym.
  (setq lisp:*gensym-counter* 0)

  (values))


;;; PRINT-SUMMARY  --  Interface
;;;
;;;    This function is called by WITH-COMPILATION-UNIT at the end of a
;;; compilation unit.  It prints out any residual unknown function warnings and
;;; the total error counts.  ABORT-P should be true when the compilation unit
;;; was aborted by throwing out.  ABORT-COUNT is the number of dynamically
;;; enclosed nested compilation units that were aborted.
;;;
(defun print-summary (abort-p abort-count)
  (unless abort-p
    (handler-bind ((warning #'compiler-warning-handler))
      (let ((undefs (sort *undefined-warnings* #'string<
			  :key #'(lambda (x)
				   (let ((x (undefined-warning-name x)))
				     (if (symbolp x)
					 (symbol-name x)
					 (prin1-to-string x)))))))
	(unless *converting-for-interpreter*
	  (dolist (undef undefs)
	    (let ((name (undefined-warning-name undef))
		  (kind (undefined-warning-kind undef))
                  (context (undefined-warning-context undef))
		  (warnings (undefined-warning-warnings undef))
		  (count (undefined-warning-count undef)))
	      (dolist (*compiler-error-context* warnings)
		(compiler-warning "Undefined ~(~A~) ~S~@[ ~A~]" kind name context))
	      
	      (let ((warn-count (length warnings)))
		(when (and warnings (> count warn-count))
		  (let ((more (- count warn-count)))
		    (compiler-warning "~D more use~:P of undefined ~(~A~) ~S."
				      more kind name)))))))
	
	(dolist (kind '(:variable :function :type))
	  (let ((summary (mapcar #'undefined-warning-name
				 (remove kind undefs :test-not #'eq
					 :key #'undefined-warning-kind))))
	    (when summary
	      (compiler-warning
	       "~:[This ~(~A~) is~;These ~(~A~)s are~] undefined:~
		~%  ~{~<~%  ~1:;~S~>~^ ~}"
	       (cdr summary) kind summary)))))))
  
  (unless (or *converting-for-interpreter*
	      (and (not abort-p) (zerop abort-count)
		   (zerop *compiler-error-count*)
		   (zerop *compiler-warning-count*)
		   (zerop *compiler-note-count*)))
    (compiler-mumble
     "~2&; Compilation unit ~:[finished~;aborted~].~
      ~[~:;~:*~&;   ~D fatal error~:P~]~
      ~[~:;~:*~&;   ~D error~:P~]~
      ~[~:;~:*~&;   ~D warning~:P~]~
      ~[~:;~:*~&;   ~D note~:P~]~2%"
     abort-p
     abort-count
     *compiler-error-count*
     *compiler-warning-count*
     *compiler-note-count*)))

   
;;;; Trace output:

;;; Describe-Component  --  Internal
;;;
;;;    Print out some useful info about Component to Stream.
;;;
(defun describe-component (component *standard-output*)
  (declare (type component component))
  (format t "~|~%;;;; Component: ~S~2%" (component-name component))
  (print-blocks component)  
  (undefined-value))


(defun describe-ir2-component (component *standard-output*)
  (format t "~%~|~%;;;; IR2 component: ~S~2%" (component-name component))
  
  (format t "Entries:~%")
  (dolist (entry (ir2-component-entries (component-info component)))
    (format t "~4TL~D: ~S~:[~; [Closure]~]~%"
	    (label-id (entry-info-offset entry))
	    (entry-info-name entry)
	    (entry-info-closure-p entry)))
  
  (terpri)
  (pre-pack-tn-stats component *standard-output*)
  (terpri)
  (print-ir2-blocks component)
  (terpri)
  (undefined-value))


;;;; File reading:
;;;
;;;    When reading from a file, we have to keep track of some source
;;; information.  We also exploit our ability to back up for printing the error
;;; context and for recovering from errors.
;;;
;;; The interface we provide to this stuff is the stream-oid Source-Info
;;; structure.  The bookkeeping is done as a side-effect of getting the next
;;; source form.


;;; The File-Info structure holds all the source information for a given file.
;;;
(defstruct file-info
  ;;
  ;; If a file, the truename of the corresponding source file.  If from a Lisp
  ;; form, :LISP, if from a stream, :STREAM.
  (name (required-argument) :type (or pathname (member :lisp :stream)))
  ;;
  ;; The defaulted, but not necessarily absolute file name (i.e. prior to
  ;; TRUENAME call.)  Null if not a file.  This is used to set
  ;; *COMPILE-FILE-PATHNAME*, and if absolute, is dumped in the debug-info.
  (untruename nil :type (or pathname null))
  ;;
  ;; The file's write date (if relevant.)
  (write-date nil :type (or unsigned-byte null))
  ;;
  ;; This file's FILE-COMMENT, or NIL if none.
  (comment nil :type (or simple-string null))
  ;;
  ;; The source path root number of the first form in this file (i.e. the
  ;; total number of forms converted previously in this compilation.)
  (source-root 0 :type unsigned-byte)
  ;;
  ;; Parallel vectors containing the forms read out of the file and the file
  ;; positions that reading of each form started at (i.e. the end of the
  ;; previous form.)
  (forms (make-array 10 :fill-pointer 0 :adjustable t) :type (vector t))
  (positions (make-array 10 :fill-pointer 0 :adjustable t) :type (vector t))
  ;;
  ;; Language to use.  Normally Lisp, but sometimes Dylan.
  (language :lisp :type (member :lisp #+nil :dylan)))

;;; The Source-Info structure provides a handle on all the source information
;;; for an entire compilation.
;;;
(defstruct (source-info
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (format stream "#<Source-Info>"))))
  ;;
  ;; The UT that compilation started at.
  (start-time (get-universal-time) :type unsigned-byte)
  ;;
  ;; A list of the file-info structures for this compilation.
  (files nil :type list)
  ;;
  ;; The tail of the Files for the file we are currently reading.
  (current-file nil :type list)
  ;;
  ;; The stream that we are using to read the Current-File.  Null if no stream
  ;; has been opened yet.
  (stream nil :type (or stream null)))


;;; Make-File-Source-Info  --  Internal
;;;
;;;    Given a list of pathnames, return a Source-Info structure.
;;;
(defun make-file-source-info (files)
  (declare (list files))
  (let ((file-info
	 (mapcar #'(lambda (x)
		     (make-file-info :name (truename x)
				     :untruename x
				     :write-date (file-write-date x)
				     :language :lisp))
		 files)))

    (make-source-info :files file-info
		      :current-file file-info)))


;;; MAKE-LISP-SOURCE-INFO  --  Interface
;;;
;;;    Return a SOURCE-INFO to describe the incremental compilation of Form.
;;; Also used by EVAL:INTERNAL-EVAL.
;;;
(defun make-lisp-source-info (form)
  (make-source-info
   :start-time (get-universal-time)
   :files (list (make-file-info :name :lisp
				:forms (vector form)
				:positions '#(0)))))


;;; MAKE-STREAM-SOURCE-INFO  --  Internal
;;;
;;;    Return a SOURCE-INFO which will read from Stream.
;;;
(defun make-stream-source-info (stream language)
  (declare (type (member :lisp) language))
  (let ((files (list (make-file-info :name :stream :language language))))
    (make-source-info
     :files files
     :current-file files
     :stream stream)))


;;; Normal-Read-Error  --  Internal
;;;
;;;    Print an error message for a non-EOF error on Stream.  Old-Pos is a
;;; preceding file position that hopefully comes before the beginning of the
;;; line.  Of course, this only works on streams that support the file-position
;;; operation.
;;;
(defun normal-read-error (stream old-pos condition)
  (declare (type stream stream) (type unsigned-byte old-pos))
  (let ((pos (file-position stream)))
    (file-position stream old-pos)
    (let ((start old-pos))
      (loop
	(let ((line (read-line stream nil))
	      (end (file-position stream)))
	  (when (>= end pos)
	    (compiler-read-error 
	     pos "Read error at ~D:~% \"~A/\\~A\"~%~A"
	     pos (string-left-trim '(#\space #\tab)
				   (subseq line 0 (- pos start)))
	     (subseq line (- pos start))
	     condition)
	    (return))
	  (setq start end)))))
  (undefined-value))


;;; Ignore-Error-Form  --  Internal
;;;
;;;    Back Stream up to the position Pos, then read a form with
;;; *Read-Suppress* on, discarding the result.  If an error happens during this
;;; read, then bail out using Compiler-Error (fatal in this context).
;;;
(defun ignore-error-form (stream pos)
  (declare (type stream stream) (type unsigned-byte pos))
  (file-position stream pos)
  (handler-case (let ((*read-suppress* t)
		      (*features* (backend-features *target-backend*)))
		  (read stream))
    (error (condition)
      (declare (ignore condition))
      (compiler-error "Unable to recover from read error."))))


;;; Unexpected-EOF-Error  --  Internal
;;;
;;;    Print an error message giving some context for an EOF error.  We print
;;; the first line after POS that contains #\" or #\(, or lacking that, the
;;; first non-empty line.
;;;
(defun unexpected-eof-error (stream pos condition)
  (declare (type stream stream) (type unsigned-byte pos))
  (let ((eof-pos (file-position stream))
        (res nil))
    (file-position stream pos)
    (loop
       (let ((line (read-line stream nil nil))) 
	 (unless line (return))
	 (when (or (find #\" line) (find #\( line))
	   (setq res line)
	   (return))
	 (unless (or res (zerop (length line)))
	   (setq res line))))

    (compiler-read-error 
     pos "Read error in form starting at ~D:~%~@[ \"~A\"~%~]~A"
     pos res condition)

    (file-position stream eof-pos)
    (undefined-value)))


;;; Careful-Read  --  Internal
;;;
;;;    Read a form from STREAM, returning EOF at EOF.  If a read error happens,
;;; then attempt to recover if possible, returing a proxy error form.
;;;
(defun careful-read (stream eof pos)
  (handler-case (let ((*features* (backend-features *target-backend*)))
		  (read stream nil eof))
    (error (condition)
      (if (null (peek-char nil stream nil))
          (unexpected-eof-error stream pos condition)
          (progn
            (normal-read-error stream pos condition)
            (ignore-error-form stream pos)))
      '(cerror "Skip this form."
	       "Attempt to load a file having a compile-time read error."))))


;;; Get-Source-Stream  --  Internal
;;;
;;;    If Stream is present, return it, otherwise open a stream to the current
;;; file.  There must be a current file.  When we open a new file, we also
;;; reset *Package* and policy.  This gives the effect of rebinding
;;; around each file.
;;;
(defun get-source-stream (info)
  (declare (type source-info info))
  (cond ((source-info-stream info))
	(t
	 (setq *package* *initial-package*)
	 (setq *default-cookie* (copy-cookie *initial-cookie*))
	 (setq *default-interface-cookie*
	       (copy-cookie *initial-interface-cookie*))
	 (let* ((finfo (first (source-info-current-file info)))
		(name (file-info-name finfo)))
	   (setq *compile-file-truename* name)
	   (setq *compile-file-pathname* (file-info-untruename finfo))
	   (setf (source-info-stream info)
		 (open name :direction :input))))))

;;; CLOSE-SOURCE-INFO  --  Internal
;;;
;;;    Close the stream in Info if it is open.
;;;
(defun close-source-info (info)
  (declare (type source-info info))
  (let ((stream (source-info-stream info)))
    (when stream (close stream)))
  (setf (source-info-stream info) nil)
  (undefined-value))


;;; Advance-Source-File  --  Internal
;;;
;;;    Advance Info to the next source file.  If none, return NIL, otherwise T.
;;;
(defun advance-source-file (info)
  (declare (type source-info info))
  (close-source-info info)
  (let ((prev (pop (source-info-current-file info))))
    (if (source-info-current-file info)
	(let ((current (first (source-info-current-file info))))
	  (setf (file-info-source-root current)
		(+ (file-info-source-root prev)
		   (length (file-info-forms prev))))
	  t)
	nil)))

;;; PROCESS-SOURCES -- internal.
;;;
;;; Read the sources from the source files and process them.
;;; 
(defun process-sources (info)
  (let* ((file (first (source-info-current-file info)))
	 (language (file-info-language file))
	 (stream (get-source-stream info)))
    (ecase language
      (:lisp
       (flet
	   ((process-one (stream)
	      (loop
		 (let* ((pos (file-position stream))
			(eof '(*eof*))
			(form (careful-read stream eof pos)))
		   (if (eq form eof)
		       (return)
		       (let* ((forms (file-info-forms file))
			      (current-idx (+ (fill-pointer forms)
					      (file-info-source-root file))))
			 (vector-push-extend form forms)
			 (vector-push-extend pos (file-info-positions file))
			 (clrhash *source-paths*)
			 (find-source-paths form current-idx)
			 (process-form form
				       `(original-source-start 0 ,current-idx)))))))
	    (process-xref-info (pathname)
	      ;; When we have xref enabled, we save the xref data to the
	      ;; file by faking it.  What we do is append a bunch of forms
	      ;; to the file as if the file actually contained them.  These
	      ;; forms clear out the entries from the xref databases
	      ;; pertaining to this file, and then registers new entries
	      ;; based on what we've found out so far from compiling this
	      ;; file.
	      ;;
	      ;; Is this what we really want to do?  A new FOP might be good.
	      ;;
	      ;; Also, when compiling a file should we have a file-local
	      ;; version of the databases?  This makes it easy to figure
	      ;; out what we need to save to the fasl.  Then we can update
	      ;; the global tables with the new info when we're done.  Or
	      ;; we can just wait until the user loads the fasl.  This
	      ;; latter option, however, changes how xref currently behaves.

	      ;; Set *compile-print* to nil so we don't see any
	      ;; spurious output from our fake source forms.  (Do we
	      ;; need more?)
	      (let ((*compile-print* nil))
		;; Clear the xref database of all references to this file
		(when (or (pathnamep pathname)
			  (stringp pathname))
		  (process-form `(xref::invalidate-xrefs-for-namestring
				  ,(namestring pathname))
				;; What should we use here?
				`(original-source-start 0 0))
		  ;; Now dump all the xref info pertaining to this file
		  (dolist (db-type '(:calls :called :references :binds :sets
				     :macroexpands))
		    (dolist (xrefs (xref::find-xrefs-for-pathname db-type pathname))
		      (destructuring-bind (target contexts)
			  xrefs
			(dolist (c contexts)
			  (process-form
			   `(xref:register-xref
			     ,db-type ',target
			     (xref:make-xref-context :name ',(xref:xref-context-name c)
						     :file ,(xref:xref-context-file c)
						     :source-path ',(xref:xref-context-source-path c)))

			   `(original-source-start 0 0))))))))))
	 ;; Compile the real file.
	 (process-one stream)

	 (when *record-xref-info*
	   (process-xref-info (file-info-name file))))
      
       (when (advance-source-file info)
	 (process-sources info))))))

;;; FIND-FILE-INFO  --  Interface
;;;
;;;    Return the File-Info describing the Index'th form.
;;;
(defun find-file-info (index info)
  (declare (type index index) (type source-info info))
  (dolist (file (source-info-files info))
    (when (> (+ (length (file-info-forms file))
		(file-info-source-root file))
	     index)
      (return file))))


;;; FIND-SOURCE-ROOT  --  Interface
;;;
;;;    Return the Index'th source form read from Info and the position that it
;;; was read at.
;;;
(defun find-source-root (index info)
  (declare (type source-info info) (type index index))
  (let* ((file (find-file-info index info))
	 (idx (- index (file-info-source-root file))))
    (values (aref (file-info-forms file) idx)
	    (aref (file-info-positions file) idx))))

;;;; Top-level form processing:

;;; CONVERT-AND-MAYBE-COMPILE  --  Internal
;;;
;;;    Called by top-level form processing when we are ready to actually
;;; compile something.  If *BLOCK-COMPILE* is T, then we still convert the
;;; form, but delay compilation, pushing the result on *TOP-LEVEL-LAMBDAS*
;;; instead.
;;;
;;;   The cookies at this time becomes the default policy for compiling the
;;; form.  Any enclosed PROCLAIMs will affect only subsequent forms.
;;;
(defun convert-and-maybe-compile (form path)
  (declare (list path))
  (let* ((*lexical-environment*
	  (make-lexenv :cookie *default-cookie*
		       :interface-cookie *default-interface-cookie*))
	 (tll (ir1-top-level form path nil)))
    (if (eq *block-compile* t)
	(push tll *top-level-lambdas*)
	(compile-top-level (list tll) nil))))

;;; PROCESS-PROGN  --  Internal
;;;
;;;    Process a PROGN-like portion of a top-level form.  Forms is a list of
;;; the forms, and Path is source path of the form they came out of.
;;;
(defun process-progn (forms path)
  (declare (list forms) (list path))
  (dolist (form forms)
    (process-form form path)))


;;; PREPROCESSOR-MACROEXPAND  --  Internal
;;;
;;;    Macroexpand form in the current environment with an error handler.  We
;;; only expand one level, so that we retain all the intervening forms in the
;;; source path.
;;;
(defun preprocessor-macroexpand (form)
  (handler-case (macroexpand-1 form *lexical-environment*)
    (error (condition)
       (compiler-error "(during macroexpansion)~%~A" condition))))


;;; PROCESS-LOCALLY  --  Internal
;;;
;;;    Process a top-level use of LOCALLY.  We parse declarations and then
;;; recursively process the body.
;;;
;;;    Binding *DEFAULT-xxx-COOKIE* is pretty much of a hack, since it causes
;;; LOCALLY to "capture" enclosed proclamations.  It is necessary because
;;; CONVERT-AND-MAYBE-COMPILE uses the value of *DEFAULT-COOKIE* as the policy.
;;; The need for this hack is due to the quirk that there is no way to
;;; represent in a cookie that an optimize quality came from the default.
;;;
(defun process-locally (form path)
  (declare (list path))
  (multiple-value-bind
      (body decls)
      (system:parse-body (cdr form) *lexical-environment* nil)
    (let* ((*lexical-environment*
	    (process-declarations decls nil nil (make-continuation)))
	   (*default-cookie* (lexenv-cookie *lexical-environment*))
	   (*default-interface-cookie*
	    (lexenv-interface-cookie *lexical-environment*)))
      (process-progn body path))))


;;; PROCESS-FILE-COMMENT  --  Internal
;;;
;;;    Stash file comment in the file-info structure.
;;;
(defun process-file-comment (form)
  (unless (and (= (length form) 2) (stringp (second form)))
    (compiler-error "Bad FILE-COMMENT form: ~S." form))
  (let ((file (first (source-info-current-file *source-info*))))
    (cond ((file-info-comment file)
	   (compiler-warning "Ignoring extra file comment:~%  ~S." form))
	  (t
	   (let ((comment (coerce (second form) 'simple-string)))
	     (setf (file-info-comment file) comment)
	     (when *compile-verbose*
	       (compiler-mumble "~&; Comment: ~A~2&" comment)))))))


;;; PROCESS-COLD-LOAD-FORM  --  Internal
;;;
;;;    Force any pending top-level forms to be compiled and dumped so that they
;;; will be evaluated in the correct package environment.  Eval the form if
;;; Eval is true, then dump the form to evaled at (cold) load time.
;;;
(defun process-cold-load-form (form path eval)
  (let ((object *compile-object*))
    (etypecase object
      (fasl-file
       (compile-top-level-lambdas () t)
       (fasl-dump-cold-load-form form object))
      ((or null core-object)
       (convert-and-maybe-compile form path)))
    (when eval (eval form))))


;;; PROCESS-PROCLAIM  --  Internal
;;;
;;;    If a special block compilation delimiter, then start or end the block as
;;; appropriate.  Otherwise, just convert-and-maybe-compile the form.  If
;;; :block-compile was T or NIL, then we ignore any start/end block
;;; declarations.
;;;
(defun process-proclaim (form path)
  (if (and (eql (length form) 2) (constantp (cadr form)))
      (let ((spec (eval (cadr form))))
	(if (and (consp spec) (eq *block-compile-argument* :specified))
	    (case (first spec)
	      (start-block
	       (finish-block-compilation)
	       (setq *block-compile* t)
	       (setq *entry-points* (rest spec)))
	      (end-block
	       (finish-block-compilation))
	      (t
	       (convert-and-maybe-compile form path)))
	    (convert-and-maybe-compile form path)))
      (convert-and-maybe-compile form path)))


(declaim (special *compiler-error-bailout*))

;;; PROCESS-FORM  --  Internal
;;;
;;;    Process a top-level Form with the specified source Path and output to
;;; Object.
;;; -- If this is a magic top-level form, then do stuff.
;;; -- If it is a macro expand it.
;;; -- Otherwise, just compile it.
;;;
(defun process-form (form path)
  (declare (list path))
  (catch 'process-form-error-abort
    (let* ((path (or (gethash form *source-paths*) (cons form path)))
	   (*compiler-error-bailout*
	    #'(lambda ()
		(convert-and-maybe-compile
		 `(error 'simple-program-error
		    :format-control "Execution of a form compiled with errors:~% ~S"
		    :format-arguments (list ',form))
		 path)
		(throw 'process-form-error-abort nil))))
      (if (atom form)
	  (convert-and-maybe-compile form path)
	  (case (car form)
	    ((make-package shadow shadowing-import export
	      unexport use-package unuse-package import
	      old-in-package %in-package %defpackage)
	     (process-cold-load-form form path t))
	    ((error cerror break signal)
	     (process-cold-load-form form path nil))
	    (kernel:%compiler-defstruct
	     (convert-and-maybe-compile form path)
	     (compile-top-level-lambdas () t))
	    ((eval-when)
	     (unless (>= (length form) 2)
	       (compiler-error "EVAL-WHEN form is too short: ~S." form))
	     (do-eval-when-stuff
	      (cadr form) (cddr form)
	      #'(lambda (forms)
		  (process-progn forms path))
	      t))
	    ((macrolet)
	     (unless (>= (length form) 2)
	       (compiler-error "MACROLET form is too short: ~S." form))
	     ;; Macrolets can have declarations.
	     (multiple-value-bind (body decls)
		 (system:parse-body (cddr form) nil nil)
	       (do-macrolet-stuff
		 (cadr form)
		 #'(lambda ()
		     (process-progn body path))
		 decls)))
	    (locally (process-locally form path))
	    (progn (process-progn (cdr form) path))
	    (file-comment (process-file-comment form))
	    (proclaim (process-proclaim form path))
	    (t
	     (let ((exp (preprocessor-macroexpand form)))
	       (if (eq exp form)
		   (convert-and-maybe-compile form path)
		   (process-form exp path))))))))
      
  (undefined-value))


;;;; Load time value support.

;;; See EMIT-MAKE-LOAD-FORM.

;;; PRODUCING-FASL-FILE  --  interface.
;;;
;;; Returns T iff we are currently producing a fasl-file and hence constants
;;; need to be dumped carefully.
;;; 
(defun producing-fasl-file ()
  (unless *converting-for-interpreter*
    (fasl-file-p *compile-object*)))

;;; COMPILE-LOAD-TIME-VALUE  --  interface.
;;;
;;; Compile FORM and arrange for it to be called at load-time.  Returns the
;;; dumper handle and our best guess at the type of the object.
;;; 
(defun compile-load-time-value
       (form &optional
	     (name (let ((*print-level* 2) (*print-length* 3))
		     (format nil "Load Time Value of ~S"
			     (if (and (listp form)
				      (eq (car form) 'make-value-cell))
				 (second form)
				 form)))))
  (let ((lambda (compile-load-time-stuff form name t)))
    (values
     (fasl-dump-load-time-value-lambda lambda *compile-object*)
     (let ((type (leaf-type lambda)))
       (if (function-type-p type)
	   (single-value-type (function-type-returns type))
	   *wild-type*)))))

;;; COMPILE-MAKE-LOAD-FORM-INIT-FORMS  --  internal.
;;;
;;; Compile the FORMS and arrange for them to be called (for effect, not value)
;;; at load-time.
;;; 
(defun compile-make-load-form-init-forms (forms name)
  (let ((lambda (compile-load-time-stuff `(progn ,@forms) name nil)))
    (fasl-dump-top-level-lambda-call lambda *compile-object*)))

;;; COMPILE-LOAD-TIME-STUFF  --  internal.
;;;
;;; Does the actual work of COMPILE-LOAD-TIME-VALUE or COMPILE-MAKE-LOAD-FORM-
;;; INIT-FORMS.
;;; 
(defun compile-load-time-stuff (form name for-value)
  (with-ir1-namespace
   (let* ((*lexical-environment* (make-null-environment))
	  (lambda (ir1-top-level form *current-path* for-value)))
     (setf (leaf-name lambda) name)
     (ext:without-package-locks
      (compile-top-level (list lambda) t))
     lambda)))

;;; COMPILE-LOAD-TIME-VALUE-LAMBDA  --  internal.
;;;
;;; Called by COMPILE-TOP-LEVEL when it was pased T for LOAD-TIME-VALUE-P
;;; (which happens in COMPILE-LOAD-TIME-STUFF).  We don't try to combine
;;; this component with anything else and frob the name.  If not in a
;;; :TOP-LEVEL component, then don't bother compiling, because it was merged
;;; with a run-time component.
;;; 
(defun compile-load-time-value-lambda (lambdas)
  (assert (null (cdr lambdas)))
  (let* ((lambda (car lambdas))
	 (component (block-component (node-block (lambda-bind lambda)))))
    (when (eq (component-kind component) :top-level)
      (setf (component-name component) (leaf-name lambda))
      (compile-component component)
      (clear-ir1-info component))))


;;; EMIT-MAKE-LOAD-FORM  --  interface.
;;;
;;; The entry point for MAKE-LOAD-FORM support.  When IR1 conversion finds a
;;; constant structure, it invokes this to arrange for proper dumping.  If it
;;; turns out that the constant has already been dumped, then we don't need
;;; to do anything.
;;;
;;; If the constant hasn't been dumped, then we check to see if we are in the
;;; process of creating it.  We detect this by maintaining the special
;;; *constants-being-created* as a list of all the constants we are in the
;;; process of creating.  Actually, each entry is a list of the constant and
;;; any init forms that need to be processed on behalf of that constant.
;;;
;;; It's not necessarily an error for this to happen.  If we are processing the
;;; init form for some object that showed up *after* the original reference
;;; to this constant, then we just need to defer the processing of that init
;;; form.  To detect this, we maintain *constants-created-since-last-init* as
;;; a list of the constants created since the last time we started processing
;;; an init form.  If the constant passed to emit-make-load-form shows up in
;;; this list, then there is a circular chain through creation forms, which is
;;; an error.
;;;
;;; If there is some intervening init form, then we blow out of processing it
;;; by throwing to the tag PENDING-INIT.  The value we throw is the entry from
;;; *constants-being-created*.  This is so the offending init form can be
;;; tacked onto the init forms for the circular object.
;;;
;;; If the constant doesn't show up in *constants-being-created*, then we have
;;; to create it.  We call MAKE-LOAD-FORM and check to see if the creation
;;; form is the magic value :just-dump-it-normally.  If it is, then we don't
;;; do anything.  The dumper will eventually get it's hands on the object
;;; and use the normal structure dumping noise on it.
;;;
;;; Otherwise, we bind *constants-being-created* and *constants-created-since-
;;; last-init* and compile the creation form a la load-time-value.  When this
;;; finishes, we tell the dumper to use that result instead whenever it sees
;;; this constant.
;;;
;;; Now we try to compile the init form.  We bind *constants-created-since-
;;; last-init* to NIL and compile the init form (and any init forms that were
;;; added because of circularity detection).  If this works, great.  If not,
;;; we add the init forms to the init forms for the object that caused the
;;; problems and let it deal with it.
;;; 
(defvar *constants-being-created* nil)
(defvar *constants-created-since-last-init* nil)
;;; 
(defun emit-make-load-form (constant)
  (assert (fasl-file-p *compile-object*))
  (unless (fasl-constant-already-dumped constant *compile-object*)
    (let ((circular-ref (assoc constant *constants-being-created* :test #'eq)))
      (when circular-ref
	(when (find constant *constants-created-since-last-init* :test #'eq)
	  (throw constant t))
	(throw 'pending-init circular-ref)))
    (multiple-value-bind
	(creation-form init-form)
	(handler-case
	    (if (fboundp 'lisp::make-load-form)
                (locally
                    (declare (optimize (inhibit-warnings 3)))
                  (ext:without-package-locks
                   (lisp::make-load-form constant (make-null-environment))))
                (ext:without-package-locks
                 (make-structure-load-form constant)))
	  (error (condition)
            (compiler-error "(while making load form for ~S)~%~A"
                            constant condition)))
      (case creation-form
	(:just-dump-it-normally
	 (fasl-validate-structure constant *compile-object*)
	 t)
	(:ignore-it
	 nil)
	(t
	 (compile-top-level-lambdas () t)
	 (when (fasl-constant-already-dumped constant *compile-object*)
	   (return-from emit-make-load-form nil))
	 (let* ((name (let ((*print-level* 1) (*print-length* 2))
			(with-output-to-string (stream)
			  (write constant :stream stream))))
		(info (if init-form
			  (list constant name init-form)
			  (list constant))))
	   (let ((*constants-being-created*
		  (cons info *constants-being-created*))
		 (*constants-created-since-last-init*
		  (cons constant *constants-created-since-last-init*)))
	     (when
		 (catch constant
		   (fasl-note-handle-for-constant
		    constant
		    (compile-load-time-value
		     creation-form
		     (format nil "Creation Form for ~A" name))
		    *compile-object*)
		   nil)
	       (compiler-error "Circular references in creation form for ~S"
			       constant)))
	   (when (cdr info)
	     (let* ((*constants-created-since-last-init* nil)
		    (circular-ref
		     (catch 'pending-init
		       (loop for (name form) on (cdr info) by #'cddr
			 collect name into names
			 collect form into forms
			 finally
			 (compile-make-load-form-init-forms
			  forms
			  (format nil "Init Form~:[~;s~] for ~{~A~^, ~}"
				  (cdr forms) names)))
		       nil)))
	       (when circular-ref
		 (setf (cdr circular-ref)
		       (append (cdr circular-ref) (cdr info))))))))))))



;;;; COMPILE-FILE and COMPILE-FROM-STREAM: 

;;; We build a list of top-level lambdas, and then periodically smash them
;;; together into a single component and compile it.
;;;
(defvar *pending-top-level-lambdas*)

;;; The maximum number of top-level lambdas we put in a single top-level
;;; component.
;;;
(defparameter top-level-lambda-max 10)


;;; OBJECT-CALL-TOP-LEVEL-LAMBDA  --  Internal
;;;
(defun object-call-top-level-lambda (tll)
  (declare (type functional tll))
  (let ((object *compile-object*))
    (etypecase object
      (fasl-file
       (fasl-dump-top-level-lambda-call tll object))
      (core-object
       (core-call-top-level-lambda tll object))
      (null))))


;;; SUB-COMPILE-TOP-LEVEL-LAMBDAS  --  Internal
;;;
;;;    Add Lambdas to the pending lambdas.  If this leaves more than
;;; TOP-LEVEL-LAMBDA-MAX lambdas in the list, or if Force-P is true, then smash
;;; the lambdas into a single component, compile it, and call the resulting
;;; function.
;;;
(defun sub-compile-top-level-lambdas (lambdas force-p)
  (declare (list lambdas))
  (setq *pending-top-level-lambdas*
	(append *pending-top-level-lambdas* lambdas))
  (let ((pending *pending-top-level-lambdas*))
    (when (and pending
	       (or (> (length pending) top-level-lambda-max)
		   force-p))
      (multiple-value-bind (component tll)
			   (merge-top-level-lambdas pending)
	(setq *pending-top-level-lambdas* ())
	(let ((*byte-compile* (if (eq *byte-compile* :maybe)
				  *byte-compile-top-level*
				  *byte-compile*)))
	  (compile-component component))
	(clear-ir1-info component)
	(object-call-top-level-lambda tll))))
  (undefined-value))


;;; COMPILE-TOP-LEVEL-LAMBDAS  --  Internal
;;;
;;;    Compile top-level code and call the Top-Level lambdas.  We pick off
;;; top-level lambdas in non-top-level components here, calling SUB-c-t-l-l on
;;; each subsequence of normal top-level lambdas.
;;;
(defun compile-top-level-lambdas (lambdas force-p)
  (declare (list lambdas))
  (let ((len (length lambdas)))
    (flet ((loser (start)
	     (or (position-if #'(lambda (x)
				  (not (eq (component-kind
					    (block-component
					     (node-block
					      (lambda-bind x))))
					   :top-level)))
			      lambdas
			      :start start)
		 len)))
      (do* ((start 0 (1+ loser))
	    (loser (loser start) (loser start)))
	   ((>= start len)
	    (when force-p
	      (sub-compile-top-level-lambdas nil t)))
	(sub-compile-top-level-lambdas (subseq lambdas start loser)
				       (or force-p (/= loser len)))
	(unless (= loser len)
	  (object-call-top-level-lambda (elt lambdas loser))))))
  (undefined-value))


;;; Compile-Top-Level  --  Internal
;;;
;;;    Compile Lambdas (a list of the lambdas for top-level forms) into the
;;; Object file.  We loop doing local call analysis until it converges, since a
;;; single pass might miss something due to components being joined by let
;;; conversion.
;;;
(defun compile-top-level (lambdas load-time-value-p)
  (declare (list lambdas))
  (maybe-mumble "Locall ")
  (loop
    (let ((did-something nil))
      (dolist (lambda lambdas)
	(let* ((component (block-component (node-block (lambda-bind lambda))))
	       (*all-components* (list component)))
	  (when (component-new-functions component)
	    (setq did-something t)
	    (local-call-analyze component))))
      (unless did-something (return))))
  
  (maybe-mumble "IDFO ")
  (multiple-value-bind (components top-components hairy-top)
		       (find-initial-dfo lambdas)
    (let ((*all-components* (append components top-components))
	  (top-level-closure nil))
      (when *check-consistency*
	(maybe-mumble "[Check]~%")
	(check-ir1-consistency *all-components*))
      
      (dolist (component (append hairy-top top-components))
	(when (pre-environment-analyze-top-level component)
	  (setq top-level-closure t)))

      (let ((*byte-compile*
	     (if (and top-level-closure (eq *byte-compile* :maybe))
		 nil
		 *byte-compile*)))
	(dolist (component components)
	  (compile-component component)
	  (when (replace-top-level-xeps component)
	    (setq top-level-closure t)))
	
	(when *check-consistency*
	  (maybe-mumble "[Check]~%")
	  (check-ir1-consistency *all-components*))
	
	(if load-time-value-p
	    (compile-load-time-value-lambda lambdas)
	    (compile-top-level-lambdas lambdas top-level-closure)))

      (dolist (component components)
	(clear-ir1-info component))
      (clear-stuff)))
  (undefined-value))


;;; FINISH-BLOCK-COMPILATION  --  Internal
;;;
;;;    Actually compile any stuff that has been queued up for block
;;; compilation.
;;;
(defun finish-block-compilation ()
  (when *block-compile*
    (when *top-level-lambdas*
      (compile-top-level (nreverse *top-level-lambdas*) nil)
      (setq *top-level-lambdas* ()))
    (setq *block-compile* :specified)
    (setq *entry-points* nil)))


;;; Sub-Compile-File  --  Internal
;;;
;;;    Read all forms from Info and compile them, with output to Object.  We
;;; return :ERROR, :WARNING, :NOTE or NIL to indicate the most severe kind of
;;; compiler diagnostic emitted.
;;;
(defun sub-compile-file (info &optional d-s-info)
  (declare (type source-info info))
  (with-ir1-namespace
    (let* ((*block-compile* *block-compile-argument*)
	   (start-errors *compiler-error-count*)
	   (start-warnings *compiler-warning-count*)
	   (start-notes *compiler-note-count*)
	   (*package* *package*)
	   (*initial-package* *package*)
	   (*initial-cookie* *default-cookie*)
	   (*initial-interface-cookie* *default-interface-cookie*)
	   (*default-cookie* (copy-cookie *initial-cookie*))
	   (*default-interface-cookie*
	    (copy-cookie *initial-interface-cookie*))
	   (*lexical-environment* (make-null-environment))
	   (*converting-for-interpreter* nil)
	   (*source-info* info)
	   (*user-source-info* d-s-info)
	   (*compile-file-pathname* nil)
	   (*compile-file-truename* nil)
	   (*top-level-lambdas* ())
	   (*pending-top-level-lambdas* ())
	   (*compiler-error-bailout*
	    #'(lambda ()
		(compiler-mumble
		 "~2&Fatal error, aborting compilation...~%")
		(return-from sub-compile-file :error)))
	   (*current-path* nil)
	   (*last-source-context* nil)
	   (*last-original-source* nil)
	   (*last-source-form* nil)
	   (*last-format-string* nil)
	   (*last-format-args* nil)
	   (*last-message-count* 0)
	   (*info-environment*
	    (or (backend-info-environment *backend*)
		*info-environment*))
	   (*gensym-counter* 0))
      (with-debug-counters
	(clear-stuff)
	(with-compilation-unit ()
	  (process-sources info)

	  (finish-block-compilation)
	  (compile-top-level-lambdas () t)
	  (let ((object *compile-object*))
	    (etypecase object
	      (fasl-file (fasl-dump-source-info info object))
	      (core-object (fix-core-source-info info object d-s-info))
	      (null)))

	  (cond ((> *compiler-error-count* start-errors) :error)
		((> *compiler-warning-count* start-warnings) :warning)
		((> *compiler-note-count* start-notes) :note)
		(t nil)))))))


;;; Verify-Source-Files  --  Internal
;;;
;;;    Return a list of pathnames for the named files.  All the files must
;;; exist.
;;;
(defun verify-source-files (stuff)
  (let* ((stuff (if (listp stuff) stuff (list stuff)))
	 (default-host (make-pathname
			:host (pathname-host (pathname (first stuff))))))
    (flet ((try-with-type (path type error-p)
	     (let ((new (merge-pathnames 
			 path (make-pathname :type type 
					     :defaults default-host))))
	       (if (probe-file new)
		   new
		   (and error-p (truename new))))))
      (unless stuff
	(error "Can't compile with no source files."))
      (mapcar #'(lambda (x)
		  (let ((x (pathname (merge-pathnames x))))
		    (cond ((typep x 'logical-pathname)
			   (try-with-type x "LISP" t))
			  ((probe-file x) x)
			  ((try-with-type x "lisp"  nil))
			  ((try-with-type x "lisp"  t)))))
	      stuff))))

;;; COMPILE-FROM-STREAM  --  Public
;;;
;;;    Just call SUB-COMPILE-FILE on the on a stream source info for the
;;; stream, sending output to core.
;;;
(defun compile-from-stream
       (stream &key
	       ((:error-stream *compiler-error-output*) *error-output*)
	       ((:trace-stream *compiler-trace-output*) nil)
	       ((:verbose *compile-verbose*) *compile-verbose*)
	       ((:print *compile-print*) *compile-print*)
	       ((:progress *compile-progress*) *compile-progress*)
	       ((:block-compile *block-compile-argument*)
		*block-compile-default*)
	       ((:entry-points *entry-points*) nil)
	       ((:byte-compile *byte-compile*) *byte-compile-default*)
	       source-info
	       (language :lisp))
  "Similar to COMPILE-FILE, but compiles text from Stream into the current lisp
  environment.  Stream is closed when compilation is complete.  These keywords
  are supported:

  :Error-Stream
      The stream to write compiler error output to (default *ERROR-OUTPUT*.)
  :Trace-Stream
      The stream that we write compiler trace output to, or NIL (the default)
      to inhibit trace output.
  :Block-Compile {T, NIL, :SPECIFIED}
        If true, then function names may be resolved at compile time.
  :Source-Info
        Some object to be placed in the DEBUG-SOURCE-INFO.
  :Byte-Compile {T, NIL, :MAYBE}
        If true, then may compile to interpreted byte code."
  (declare (type (member :lisp) language))
  (let ((info (make-stream-source-info stream language))
	(*backend* *native-backend*))
    (unwind-protect
	(let* ((*compile-object* (make-core-object))
	       (won (sub-compile-file info source-info)))
	  (values (not (null won))
		  (if (member won '(:error :warning)) t nil)))
      (close-source-info info))))


(defun elapsed-time-to-string (tsec)
  (multiple-value-bind (tmin sec)
		       (truncate tsec 60)
    (multiple-value-bind (thr min)
			 (truncate tmin 60)
      (format nil "~D:~2,'0D:~2,'0D" thr min sec))))


;;; START-ERROR-OUTPUT, FINISH-ERROR-OUTPUT  --  Internal
;;;
;;;    Print some junk at the beginning and end of compilation.
;;;
(defun start-error-output (source-info)
  (declare (type source-info source-info))
  (compiler-mumble "~2&; Python version ~A, VM version ~A on ~A.~%"
		   compiler-version (backend-version *backend*)
		   (ext:format-universal-time nil (get-universal-time)
					      :style :government
					      :print-weekday nil
					      :print-timezone nil))
  (dolist (x (source-info-files source-info))
    (compiler-mumble "; Compiling: ~A ~A~%"
		     (namestring (file-info-name x))
		     (ext:format-universal-time nil (file-info-write-date x)
						:style :government
						:print-weekday nil
						:print-timezone nil)))
  (compiler-mumble "~%")
  (undefined-value))
;;;
(defun finish-error-output (source-info won)
  (declare (type source-info source-info))
  (compiler-mumble "~&; Compilation ~:[aborted after~;finished in~] ~A.~&"
		   won
		   (elapsed-time-to-string
		    (- (get-universal-time)
		       (source-info-start-time source-info))))
  (undefined-value))


;;; COMPILE-FILE  --  Public.
;;;
;;; Open some files and call SUB-COMPILE-FILE.  If something unwinds out of the
;;; compile, then abort the writing of the output file, so we don't overwrite
;;; it with known garbage.
;;;
(defun compile-file (source &key
			    (output-file t)
			    (error-file nil)
			    (trace-file nil) 
			    (error-output t)
			    (load nil)
			    (external-format :default)
			    ((:verbose *compile-verbose*) *compile-verbose*)
			    ((:print *compile-print*) *compile-print*)
			    ((:progress *compile-progress*) *compile-progress*)
			    ((:block-compile *block-compile-argument*)
			     *block-compile-default*)
			    ((:entry-points *entry-points*) nil)
			    ((:byte-compile *byte-compile*)
			     *byte-compile-default*)
		            ((:xref *record-xref-info*)
			     *record-xref-info*))
  "Compiles Source, producing a corresponding .FASL file.  Source may be a list
   of files, in which case the files are compiled as a unit, producing a single
   .FASL file.  The output file names are defaulted from the first (or only)
   input file name.  Other options available via keywords:
   :Output-File
      The name of the fasl to output, NIL for none, T for the default.
   :Error-File
      The name of the error listing file, NIL for none (the default), T for
      .err.
   :Trace-File
      If specified, internal data structures are dumped to this file.  T for
      the .trace default.
   :Error-Output
      If a stream, then error output is sent there as well as to the listing
      file.  NIL suppresses this additional error output.  The default is T,
      which means use *ERROR-OUTPUT*.
   :Block-Compile {NIL | :SPECIFIED | T}
      Determines whether multiple functions are compiled together as a unit,
      resolving function references at compile time.  NIL means that global
      function names are never resolved at compilation time.  :SPECIFIED means
      that names are resolved at compile-time when convenient (as in a
      self-recursive call), but the compiler doesn't combine top-level DEFUNs.
      With :SPECIFIED, an explicit START-BLOCK declaration will enable block
      compilation.  A value of T indicates that all forms in the file(s) should
      be compiled as a unit.  The default is the value of
      EXT:*BLOCK-COMPILE-DEFAULT*, which is initially :SPECIFIED.
   :Entry-Points
      This specifies a list of function names for functions in the file(s) that
      must be given global definitions.  This only applies to block
      compilation, and is useful mainly when :BLOCK-COMPILE T is specified on a
      file that lacks START-BLOCK declarations.  If the value is NIL (the
      default) then all functions will be globally defined.
   :Byte-Compile {T | NIL | :MAYBE}
      Determines whether to compile into interpreted byte code instead of
      machine instructions.  Byte code is several times smaller, but much
      slower.  If :MAYBE, then only byte-compile when SPEED is 0 and
      DEBUG <= 1.  The default is the value of EXT:*BYTE-COMPILE-DEFAULT*,
      which is initially :MAYBE.
   :Xref
      If non-NIL, enable recording of cross-reference information.  The default
      is the value of C:*RECORD-XREF-INFO*"
  (declare (ignore external-format))
  (let* ((fasl-file nil)
	 (error-file-stream nil)
	 (output-file-pathname nil)
	 (*compiler-error-output* *compiler-error-output*)
	 (*compiler-trace-output* nil)
	 (compile-won nil)
	 (error-severity nil)
	 (source (verify-source-files source))
	 (source-info (make-file-source-info source))
	 (default (pathname (first source))))
    (unwind-protect
	(progn
	  (flet ((frob (file type)
		   (if (eq file t)
		       (make-pathname
			:type type
			:defaults (translate-logical-pathname default))
		       (pathname file))))

	    (when output-file
	      (setq output-file-pathname
		    (translate-logical-pathname
		     (if (eq output-file t)
			 (compile-file-pathname (first source)
						:byte-compile *byte-compile*)
			 (compile-file-pathname (first source)
						:output-file output-file
						:byte-compile *byte-compile*))))
	      (setq fasl-file (open-fasl-file output-file-pathname
					      (namestring (first source))
					      (eq *byte-compile* t))))
	    
	    (when trace-file
	      (setq *compiler-trace-output*
		    (open (frob trace-file "trace")
			  :if-exists :supersede
			  :direction :output)))
	    
	    (when error-file
	      (setq error-file-stream
		    (open (frob error-file "err")
			  :if-exists :supersede
			  :direction :output))))
	  
	  (setq *compiler-error-output*
		(apply #'make-broadcast-stream
		       (remove nil
			       (list (if (eq error-output t)
					 *error-output*
					 error-output)
				     error-file-stream))))

	  (when *compile-verbose*
	    (start-error-output source-info))
	  (setq error-severity
		(let ((*compile-object* fasl-file))
		  (sub-compile-file source-info)))
	  (setq compile-won t))

      (close-source-info source-info)

      (when fasl-file
	(close-fasl-file fasl-file (not compile-won))
	(setq output-file-pathname (pathname (fasl-file-stream fasl-file)))
	(when (and compile-won *compile-verbose*)
	  (compiler-mumble "~2&; ~A written.~%"
			   (namestring output-file-pathname))))

      (when *compile-verbose*
	(finish-error-output source-info compile-won))

      (when error-file-stream
	(let ((name (pathname error-file-stream)))
	  ;;
	  ;; Leave this var pointing to something reasonable in case someone
	  ;; tries to use it before the LET ends, e.g. during the LOAD.
	  (setq *compiler-error-output* *error-output*)
	  (close error-file-stream)
	  (when (and compile-won (not error-severity))
	    (delete-file name))))

      (when *compiler-trace-output*
	(close *compiler-trace-output*)))

    (when load
      (unless output-file
	(error "Can't :LOAD with no output file."))
      (load output-file-pathname :verbose *compile-verbose*))

    (values (if output-file
		;; Hack around filesystem race condition...
		(or (probe-file output-file-pathname) output-file-pathname)
		nil)
            (if (member error-severity '(:warning :error)) t nil)
            ;; FIXME in the following we should not return t for a STYLE-WARNING
	    (if (member error-severity '(:warning :error)) t nil))))

;;;; COMPILE and UNCOMPILE:

;;; GET-LAMBDA-TO-COMPILE  --  Internal
;;;
(defun get-lambda-to-compile (definition)
  (if (consp definition)
      definition
      (multiple-value-bind (def env-p)
			   (function-lambda-expression definition)
	(when env-p
	  (error "~S was defined in a non-null environment." definition))
	(unless def
	  (error "Can't find a definition for ~S." definition))
	def)))


;;; COMPILE-FIX-FUNCTION-NAME  --  Internal
;;;
;;;    Find the function that is being compiled by COMPILE and bash its name to
;;; NAME.  We also substitute for any references to name so that recursive
;;; calls will be compiled direct.  LAMBDA is the top-level lambda for the
;;; compilation.  A REF for the real function is the only thing in the
;;; top-level lambda other than the bind and return, so it isn't too hard to
;;; find.
;;;
(defun compile-fix-function-name (lambda name)
  (declare (type clambda lambda) (type (or symbol cons) name))
  (when name
    (let ((fun (ref-leaf
		(continuation-next
		 (node-cont (lambda-bind lambda))))))
      (setf (leaf-name fun) name)
      (let ((old (gethash name *free-functions*)))
	(when old (substitute-leaf fun old)))
      name)))


;;; COMPILE  --  Public
;;;
(defun compile (name &optional (definition (fdefinition name)))
  "Compiles the function whose name is NAME.  If DEFINITION is supplied,
  it should be a lambda expression that is compiled and then placed in the
  function cell of NAME.  If NAME is Nil, the compiled code object is
  returned."
  (with-compilation-unit ()
    (with-ir1-namespace
      (let* ((*backend* *native-backend*)
	     (*info-environment*
	      (or (backend-info-environment *backend*)
		  *info-environment*))
	     (start-errors *compiler-error-count*)
	     (start-warnings *compiler-warning-count*)
	     (start-notes *compiler-note-count*)
	     (*lexical-environment* (make-null-environment))
	     (form `#',(get-lambda-to-compile definition))
	     (*source-info* (make-lisp-source-info form))
	     (*top-level-lambdas* ())
	     (*converting-for-interpreter* nil)
	     (*coalesce-constants* nil)
	     (*block-compile* nil)
	     (*compiler-error-bailout*
	      #'(lambda ()
		  (compiler-mumble
		   "~2&Fatal error, aborting compilation...~%")
		  (return-from compile (values nil t nil))))
	     (*compiler-error-output* *error-output*)
	     (*compiler-trace-output* nil)
	     (*current-path* nil)
	     (*last-source-context* nil)
	     (*last-original-source* nil)
	     (*last-source-form* nil)
	     (*last-format-string* nil)
	     (*last-format-args* nil)
	     (*last-message-count* 0)
	     (*compile-object* (make-core-object))
	     (*gensym-counter* 0)
	     (*current-function-names* (list name)))
	(with-debug-counters
	  (clear-stuff)
	  (find-source-paths form 0)
	  (let ((lambda (ir1-top-level form '(original-source-start 0 0) t)))

	    (compile-fix-function-name lambda name)
	    (let* ((component
		    (block-component (node-block (lambda-bind lambda))))
		   (*all-components* (list component)))
	      (local-call-analyze component))

	    (multiple-value-bind (components top-components)
		(find-initial-dfo (list lambda))
	      (let ((*all-components* (append components top-components)))
		(dolist (component *all-components*)
		  (compile-component component))))

	    (let* ((res (core-call-top-level-lambda lambda *compile-object*))
		   (return (or name res)))
	      (fix-core-source-info *source-info* *compile-object* res)
	      (when name
		(setf (fdefinition name) res))

	      (cond ((or (> *compiler-error-count* start-errors)
			 (> *compiler-warning-count* start-warnings))
		     (values return t t))
		    ((> *compiler-note-count* start-notes)
		     (values return t nil))
		    (t
		     (values return nil nil))))))))))

;;; UNCOMPILE  --  Public
;;;
(defun uncompile (name)
  "Attempt to replace Name's definition with an interpreted version of that
  definition.  If no interpreted definition is to be found, then signal an
  error."
  (let ((def (fdefinition name)))
    (if (eval:interpreted-function-p def)
	(warn "~S is already interpreted." name)
	(setf (fdefinition name)
	      (coerce (get-lambda-to-compile def) 'function))))
  name)

(defun compile-output-type-from-input (input-file byte-compile)
  (declare (type (or string stream pathname) input-file))
  (let ((type (if (typep (pathname input-file) 'logical-pathname)
		  (if (not (eq byte-compile t)) ; This is the old logic. But why?
		      "FASL"
		      (string-upcase (c:backend-byte-fasl-file-type c:*backend*)))
		  (if (eq byte-compile t)
		      (c:backend-byte-fasl-file-type c:*backend*)
		      (c:backend-fasl-file-type c:*backend*)))))
    (make-pathname :host nil
		   :device nil
		   :directory nil
		   :type type
		   :version nil)))

;;; COMPILE-FILE-PATHNAME -- Public
;;;
;;; The output-file, if given, is merged with the input-file.
(defun compile-file-pathname (input-file
			      &key
			      (byte-compile *byte-compile-default*)
			      (output-file t output-file-supplied-p)
			      &allow-other-keys)
  "Return a pathname describing what file COMPILE-FILE would write to given
   these arguments."
  (declare (type (or string pathname stream) input-file)
	   (type (or string pathname stream (member t)) output-file)
	   (values (or null pathname)))

  (flet ((check-file-argument (file name)
	   (when (and (streamp file)
		      (not (typep file 'file-stream)))
	     (error 'simple-type-error
		    :datum input-file
		    :expected-type 'file-stream
		    :format-control "The ~A parameter is a ~S, which is an invalid value ~@
            to COMPILE-FILE-PATHNAME."
		    :format-arguments (list name (type-of input-file))))
	   ;; Maybe this is too much.  CLHS says "might".
	   (when (wild-pathname-p input-file)
	     (error 'file-error :pathname input-file))))
    ;; Check INPUT-FILE
    (check-file-argument input-file "INPUT-FILE")
	 
    (let ((output-file-type (compile-output-type-from-input input-file
							    byte-compile)))
      (when (eq output-file t)
	(setf output-file output-file-type))
      (check-file-argument output-file "OUTPUT-FILE")
    
      (let* ((merged-input-file
	      (merge-pathnames input-file *default-pathname-defaults*))
	     (output-pathname-type (or (pathname-type output-file)
				       (pathname-type output-file-type))))
	(cond ((and (not output-file-supplied-p)
		    (typep merged-input-file 'logical-pathname))
	       (make-pathname :type output-pathname-type
			      :defaults merged-input-file
			      :case :common)) ; Not sure about this last one.
	      ((typep merged-input-file 'logical-pathname)
	       (make-pathname :type output-pathname-type
			      :defaults (merge-pathnames output-file
							 (translate-logical-pathname merged-input-file))))
	      (t
	       (make-pathname :type output-pathname-type
			      :defaults (merge-pathnames output-file
							 merged-input-file))))))))
