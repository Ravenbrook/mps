;;; -*- Package: MACH -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/mach.lisp,v 1.5 2003/04/19 20:52:43 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the low-level support for MACH features not found
;;; in UNIX.
;;;

(in-package "MACH")
(use-package "ALIEN")
(use-package "C-CALL")
(use-package "SYSTEM")

(export '(port mach-task_self mach-task_data mach-task_notify
	  kern-success get-mach-error-msg
	  gr-error gr-call gr-call* gr-bind
	  vm_allocate vm_copy vm_deallocate vm_statistics))


;;;; Standard ports.

(def-alien-type port int)

(def-alien-routine ("task_self" mach-task_self) port)
(def-alien-routine ("thread_reply" mach-task_data) port)
(def-alien-routine ("task_notify" mach-task_notify) port)



;;;; Return codes.

(def-alien-type kern-return int)

(defconstant kern-success 0)
(defconstant kern-invalid-address 1)
(defconstant kern-protection-failure 2)
(defconstant kern-no-space 3)
(defconstant kern-invalid-argument 4)
(defconstant kern-failure 5)
(defconstant kern-resource-shortage 6)
(defconstant kern-not-receiver 7)
(defconstant kern-no-access 8)
(defconstant kern-memory-failure 9)
(defconstant kern-memory-error 10)
(defconstant kern-already-in-set 11)
(defconstant kern-not-in-set 12)
(defconstant kern-name-exists 13)
(defconstant kern-aborted 14)
(defconstant kern-memory-present 23)

(def-alien-routine ("mach_error_string" get-mach-error-msg) c-string
  (errno kern-return))

;;; GR-Error  --  Public
;;;
(defun gr-error (function gr &optional context)
  "Signal an error indicating that Function returned code GR.  If the code
  is success, then do nothing."
  (unless (eql gr kern-success)
    (error "~S~@[ ~A~], ~(~A~)." function context (get-mach-error-msg gr))))

;;; GR-Call  --  Public
;;;
(defmacro gr-call (fun &rest args)
  "GR-Call Function {Arg}*
  Call the function with the specified Args and signal an error if the
  first value returned is not mach:kern-success.  Nil is returned."
  (let ((n-gr (gensym)))
    `(let ((,n-gr (,fun ,@args)))
       (unless (eql ,n-gr kern-success) (gr-error ',fun ,n-gr)))))

;;; GR-Call*  --  Public
;;;
(defmacro gr-call* (fun &rest args)
  "GR-Call* Function {Arg}*
  Call the function with the specified Args and signal an error if the
  first value returned is not mach:kern-success.  The second value is
  returned."
  (let ((n-gr (gensym))
	(n-res (gensym)))
    `(multiple-value-bind (,n-gr ,n-res) (,fun ,@args)
       (unless (eql ,n-gr kern-success) (gr-error ',fun ,n-gr))
       ,n-res)))

;;; GR-Bind  --  Public
;;;
(defmacro gr-bind (vars (fun . args) &parse-body (body decls))
  "GR-Bind ({Var}*) (Function {Arg}*) {Form}*
  Call the function with the specified Args and signal an error if the
  first value returned is not mach:Kern-Success.  If the call succeeds,
  the Forms are evaluated with remaining return values bound to the
  Vars."
  (let ((n-gr (gensym)))
    `(multiple-value-bind (,n-gr ,@vars) (,fun ,@args)
       ,@decls
       (unless (eql ,n-gr kern-success) (gr-error ',fun ,n-gr))
       ,@body)))



;;;; VM routines.

(export '(vm_allocate vm_copy vm_deallocate vm_statistics))

(def-alien-routine ("vm_allocate" vm_allocate) int
  (task port)
  (address system-area-pointer :in-out)
  (size unsigned-long)
  (anywhere boolean))

(def-alien-routine ("vm_copy" vm_copy) int
  (task port)
  (source system-area-pointer)
  (count unsigned-long)
  (dest system-area-pointer))

(def-alien-routine ("vm_deallocate" vm_deallocate) int
  (task port)
  (address system-area-pointer)
  (size unsigned-long))


(def-alien-type nil
  (struct vm_statistics
    (pagesize long)
    (free_count long)
    (active_count long)
    (inactive_count long)
    (wire_count long)
    (zero_fill_count long)
    (reactivations long)
    (pageins long)
    (pageouts long)
    (faults long)
    (cow_faults long)
    (lookups long)
    (hits long)))

(defun vm_statistics (task)
  (with-alien ((vm_stats (struct vm_statistics)))
    (values
     (alien-funcall (extern-alien "vm_statistics"
				  (function int
					    port
					    (* (struct vm_statistics))))
		    task (alien-sap vm_stats))
     (slot vm_stats 'pagesize)
     (slot vm_stats 'free_count)
     (slot vm_stats 'active_count)
     (slot vm_stats 'inactive_count)
     (slot vm_stats 'wire_count)
     (slot vm_stats 'zero_fill_count)
     (slot vm_stats 'reactivations)
     (slot vm_stats 'pageins)
     (slot vm_stats 'pageouts)
     (slot vm_stats 'faults)
     (slot vm_stats 'cow_faults)
     (slot vm_stats 'lookups)
     (slot vm_stats 'hits))))
