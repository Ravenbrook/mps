;;; 3/13/01 Got rid of some symbols that didn't belong in CL package.
;;; Moved get-setf-method to extensions package since it is still used.
;;; Should be easy enough to flush later as it just calls
;;; get-setf-expansion checking for a single store value.
;;;
;;; Load this into compiler before attempting to worldcom/comcom.

(unexport '(GET-SETF-METHOD-MULTIPLE-VALUE
	    LOGICAL-PATHNAME-P
	    DEFINE-SETF-METHOD
	    HASH-TABLE-WEAK-P
	    GET-SETF-METHOD)
	  :lisp)

;;; Needed before load of exports.lisp to bootstrap ANSI defstruct fix
(intern "*ANSI-DEFSTRUCT-OPTIONS-P*" :kernel)

(load "target:code/exports.lisp")
