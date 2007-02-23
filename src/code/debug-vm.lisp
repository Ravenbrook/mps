;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/debug-vm.lisp,v 1.2 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This is some very low-level support for the debugger :function-end
;;; breakpoints.
;;;
;;; Written by William Lott.
;;;

(in-package "VM")

(export '(make-bogus-lra))

(defconstant bogus-lra-constants 2)
(defconstant real-lra-slot (+ code-constants-offset 0))
(defconstant known-return-p-slot (+ code-constants-offset 1))

;;; MAKE-BOGUS-LRA -- Interface.
;;;
(defun make-bogus-lra (real-lra &optional known-return-p)
  "Make a bogus LRA object that signals a breakpoint trap when returned to.  If
   the breakpoint trap handler returns to the fake component, the fake code
   template returns to real-lra.  This returns three values: the bogus LRA
   object, the code component it points to, and the pc-offset for the trap
   instruction."
  (system:without-gcing
   (let* ((src-start (truly-the system-area-pointer
				(%primitive foreign-symbol-address
					    "function_end_breakpoint_guts")))
	  (src-end (truly-the system-area-pointer
			      (%primitive foreign-symbol-address
					  "function_end_breakpoint_end")))
	  (trap-loc (truly-the system-area-pointer
			       (%primitive foreign-symbol-address
					   "function_end_breakpoint_trap")))
	  (length (sap- src-end src-start))
	  (code-object (%primitive allocate-code-object
				   (1+ bogus-lra-constants)
				   length))
	  (dst-start (code-instructions code-object)))
     (declare (type system-area-pointer src-start src-end dst-start trap-loc)
	      (type index length))
     (setf (code-header-ref code-object code-debug-info-slot) nil)
     (setf (code-header-ref code-object code-trace-table-offset-slot) length)
     (setf (code-header-ref code-object real-lra-slot) real-lra)
     (setf (code-header-ref code-object known-return-p-slot) known-return-p)
     (system-area-copy src-start 0 dst-start 0 (* length byte-bits))
     (let ((new-lra
	    (make-lisp-obj (+ (sap-int dst-start) other-pointer-type))))
       (kernel:set-header-data new-lra
			       (logandc2 (+ code-constants-offset
					    bogus-lra-constants
					    1)
					 1))
       (values new-lra
	       code-object
	       (sap- trap-loc src-start))))))
