;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/codegen.lisp,v 1.23 1994/10/31 04:27:28 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    The implementation-independent parts of the code generator.  We use
;;; functions and information provided by the VM definition to convert IR2 into
;;; assembly code.  After emitting code, we finish the assembly and then do the
;;; post-assembly phase.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package :c)

(in-package :new-assem)
(import '(label gen-label emit-label label-position) :c)

(in-package :c)
(export '(component-header-length sb-allocated-size current-nfp-tn
	  callee-nfp-tn callee-return-pc-tn *code-segment* *elsewhere*
	  trace-table-entry pack-trace-table note-fixup *assembly-optimize*
	  fixup fixup-p make-fixup fixup-name fixup-flavor fixup-offset))


;;;; Utilities used during code generation.

;;; Component-Header-Length   --  Interface
;;; 
(defun component-header-length (&optional (component *compile-component*))
  "Returns the number of bytes used by the code object header."
  (let* ((2comp (component-info component))
	 (constants (ir2-component-constants 2comp))
	 (num-consts (length constants)))
    (ash (logandc2 (1+ num-consts) 1) vm:word-shift)))

;;; SB-Allocated-Size  --  Interface
;;;
(defun sb-allocated-size (name)
  "The size of the Name'd SB in the currently compiled component.  Useful
  mainly for finding the size for allocating stack frames."
  (finite-sb-current-size (sb-or-lose name *backend*)))


;;; Current-NFP-TN  --  Interface
;;;
(defun current-nfp-tn (vop)
  "Return the TN that is used to hold the number stack frame-pointer in VOP's
  function.  Returns NIL if no number stack frame was allocated."
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (let ((block (ir2-block-block (vop-block vop))))
    (when (ir2-environment-number-stack-p
	   (environment-info
	    (block-environment block)))
      (ir2-component-nfp (component-info (block-component block)))))))

;;; CALLEE-NFP-TN  --  Interface
;;;
(defun callee-nfp-tn (2env)
  "Return the TN that is used to hold the number stack frame-pointer in the
  function designated by 2env.  Returns NIL if no number stack frame was
  allocated."
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (when (ir2-environment-number-stack-p 2env)
      (ir2-component-nfp (component-info *compile-component*)))))


;;; CALLEE-RETURN-PC-TN  --  Interface
;;;
(defun callee-return-pc-tn (2env)
  "Return the TN used for passing the return PC in a local call to the function
  designated by 2env."
  (ir2-environment-return-pc-pass 2env))



;;;; Fixups

;;; FIXUP -- A fixup of some kind.
;;;
(defstruct (fixup
	    (:constructor make-fixup (name flavor &optional offset)))
  ;; The name and flavor of the fixup.  The assembler makes no assumptions
  ;; about the contents of these fields; their semantics are imposed by the
  ;; dumper.
  name
  flavor
  ;; An optional offset from whatever external label this fixup refers to.
  offset)

(defun %print-fixup (fixup stream depth)
  (declare (ignore depth))
  (format stream "#<~S fixup ~S~@[ offset=~S~]>"
	  (fixup-flavor fixup)
	  (fixup-name fixup)
	  (fixup-offset fixup)))

(defvar *fixups*)

;;; NOTE-FIXUP -- interface.
;;;
;;; This function is called by the (new-assembler) instruction emitters that
;;; find themselves trying to deal with a fixup.
;;; 
(defun note-fixup (segment kind fixup)
  (new-assem:emit-back-patch
   segment 0
   #'(lambda (segment posn)
       (declare (ignore segment))
       (push (list kind fixup posn) *fixups*)))
  (undefined-value))



;;;; Specials used during code generation.

(defvar *trace-table-info*)
(defvar *code-segment* nil)
(defvar *elsewhere* nil)
(defvar *elsewhere-label* nil)

(defvar *assembly-optimize* t
  "Set to NIL to inhibit assembly-level optimization.  For compiler debugging,
  rather than policy control.")


;;;; Noise to emit an instruction trace.

(defvar *prev-segment*)
(defvar *prev-vop*)

(defun trace-instruction (segment vop inst args)
  (let ((*standard-output* *compiler-trace-output*))
    (unless (eq *prev-segment* segment)
      (format t "In the ~A segment:~%" (new-assem:segment-name segment))
      (setf *prev-segment* segment))
    (unless (eq *prev-vop* vop)
      (when vop
	(format t "~%VOP ")
	(if (vop-p vop)
	    (print-vop vop)
	    (format *compiler-trace-output* "~S~%" vop)))
      (terpri)
      (setf *prev-vop* vop))
    (case inst
      (:label
       (format t "~A:~%" args))
      (:align
       (format t "~0,8T.align~0,8T~A~%" args))
      (t
       (format t "~0,8T~A~@[~0,8T~{~A~^, ~}~]~%" inst args))))
  (undefined-value))



;;;; Generate-code and support routines.

(defun make-segment (&optional name)
  (new-assem:make-segment
   :name name
   :run-scheduler
   (and *assembly-optimize*
	(policy (lambda-bind
		 (block-home-lambda
		  (block-next (component-head *compile-component*))))
		(or (> speed cspeed) (> space cspeed))))
   :inst-hook (if *compiler-trace-output* #'trace-instruction)))

;;; Init-Assembler  --  Interface
;;; 
(defun init-assembler ()
  (setf *code-segment* (make-segment "Regular"))
  (setf (new-assem:segment-collect-dynamic-statistics *code-segment*)
	*collect-dynamic-statistics*)
  (setf *elsewhere* (make-segment "Elsewhere"))
  (undefined-value))

;;; Generate-Code  --  Interface
;;;
(defun generate-code (component)
  (when *compiler-trace-output*
    (format *compiler-trace-output*
	    "~|~%Assembly code for ~S~2%"
	    component))
  (let ((prev-env nil)
	(*trace-table-info* nil)
	(*prev-segment* nil)
	(*prev-vop* nil)
	(*fixups* nil))
    (let ((label (new-assem:gen-label)))
      (setf *elsewhere-label* label)
      (new-assem:assemble (*elsewhere*)
	(new-assem:emit-label label)))
    (do-ir2-blocks (block component)
      (let ((1block (ir2-block-block block)))
	(when (and (eq (block-info 1block) block)
		   (block-start 1block))
	  (new-assem:assemble (*code-segment*)
	    (new-assem:emit-label (block-label 1block)))
	  (let ((env (block-environment 1block)))
	    (unless (eq env prev-env)
	      (let ((lab (gen-label)))
		(setf (ir2-environment-elsewhere-start (environment-info env))
		      lab)
		(emit-label-elsewhere lab))
	      (setq prev-env env)))))

      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
	  ((null vop))
	(let ((gen (vop-info-generator-function (vop-info vop))))
	  (if gen 
	      (funcall gen vop)
	      (format t "Missing generator for ~S.~%"
		      (template-name (vop-info vop)))))))
    
    (new-assem:append-segment *code-segment* *elsewhere*)
    (setf *elsewhere* nil)
    (values (new-assem:finalize-segment *code-segment*)
	    (nreverse *trace-table-info*)
	    *fixups*)))

(defun emit-label-elsewhere (label)
  (new-assem:assemble (*elsewhere*)
    (new-assem:emit-label label)))

(defun label-elsewhere-p (label-or-posn)
  (<= (label-position *elsewhere-label*)
      (etypecase label-or-posn
	(label
	 (label-position label-or-posn))
	(index
	 label-or-posn))))

(defun trace-table-entry (state)
  (let ((label (gen-label)))
    (emit-label label)
    (push (cons label state) *trace-table-info*))
  (undefined-value))

;;; COMPUTE-TRACE-TABLE -- interface.
;;;
;;; Convert the list of (label . state) entries into an ivector.
;;; 
(eval-when (compile load eval)
  (defconstant tt-bits-per-state 3)
  (defconstant tt-bits-per-entry 16)
  (defconstant tt-bits-per-offset (- tt-bits-per-entry tt-bits-per-state))
  (defconstant tt-max-offset (1- (ash 1 tt-bits-per-offset))))
;;;
(deftype tt-state ()
  `(unsigned-byte ,tt-bits-per-state))
(deftype tt-entry ()
  `(unsigned-byte ,tt-bits-per-entry))
(deftype tt-offset ()
  `(unsigned-byte ,tt-bits-per-offset))
;;;
(defun pack-trace-table (entries)
  (declare (list entries))
  (if (backend-featurep :gengc)
      (let ((result (make-array (logandc2 (1+ (length entries)) 1)
				:element-type 'tt-entry))
	    (index 0)
	    (last-posn 0)
	    (last-state 0))
	(declare (type index index last-posn)
		 (type tt-state last-state))
	(flet ((push-entry (offset state)
		 (declare (type tt-offset offset)
			  (type tt-state state))
		 (when (>= index (length result))
		   (setf result
			 (replace (make-array
				   (truncate (* (length result) 5) 4)
				   :element-type
				   'tt-entry)
				  result)))
		 (setf (aref result index)
		       (logior (ash offset tt-bits-per-state) state))
		 (incf index)))
	  (dolist (entry entries)
	    (let* ((posn (label-position (car entry)))
		   (state (cdr entry)))
	      (declare (type index posn) (type tt-state state))
	      (assert (<= last-posn posn))
	      (do ((offset (- posn last-posn) (- offset tt-max-offset)))
		  ((< offset tt-max-offset)
		   (push-entry offset state))
		(push-entry tt-max-offset last-state))
	      (setf last-posn posn)
	      (setf last-state state)))
	  (when (oddp index)
	    (push-entry 0 last-state)))
	(if (eql (length result) index)
	    result
	    (subseq result 0 index)))
      (make-array 0 :element-type 'tt-entry)))
