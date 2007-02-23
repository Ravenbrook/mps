;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;
;;;    This file contains stuff that knows how to load compiled code directly
;;; into core, e.g. incremental compilation.
;;;
(in-package 'c)


;;; The CORE-OBJECT structure holds the state needed to resolve cross-component
;;; references during in-core compilation.
;;;
(defstruct (core-object
	    (:constructor make-core-object ())
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore s d))
	       (format stream "#<Core-Object>"))))
  ;;
  ;; A hashtable translating ENTRY-INFO structures to the corresponding actual
  ;; FUNCTIONs for functions in this compilation.
  (entry-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; A list of all the DEBUG-INFO objects created, kept so that we can
  ;; backpatch with the source info.
  (debug-info () :type list))


;;; MAKE-FUNCTION-ENTRY  --  Internal
;;;
;;;    Make a function entry, filling in slots from the ENTRY-INFO.
;;;
(defun make-function-entry (entry code-obj code-vec object)
  (declare (type entry-info entry) (type core-object object))
  (let ((res (%primitive alloc-function (1+ %function-entry-type-slot))))
    (%primitive set-vector-subtype res
		(if (entry-info-closure-p entry)
		    %function-closure-entry-subtype
		    %function-entry-subtype))
    (%primitive header-set res %function-name-slot (entry-info-name entry))
    (%primitive header-set res %function-code-slot code-vec)
    (%primitive header-set res %function-offset-slot
		(+ (label-location (entry-info-offset entry))
		   clc::i-vector-header-size))
    (%primitive header-set res %function-entry-constants-slot code-obj)
    (%primitive header-set res %function-entry-arglist-slot
		(entry-info-arguments entry))
    (%primitive header-set res %function-entry-type-slot
		(entry-info-type entry))

    (setf (gethash entry (core-object-entry-table object)) res))
  (undefined-value))
    

;;; CORE-FUNCTION-OR-LOSE  --  Internal
;;;
;;;    Get the function for a function entry that has been dumped to core.
;;;
(defun core-function-or-lose (fun object)
  (declare (type clambda fun) (type core-object object))
  (let ((res (gethash (leaf-info fun) (core-object-entry-table object))))
    (assert res () "Unresolved forward function reference?")
    res))


;;; DO-CORE-FIXUPS  --  Internal
;;;
;;;    Do "load-time" fixups on the code vector.  Currently there are only
;;; :MISCOP fixups.
;;;
(defun do-core-fixups (code fixups)
  (declare (list fixups))
  (dolist (fixup fixups)
    (let ((offset (second fixup))
	  (value (third fixup)))
      (ecase (first fixup)
	(:miscop
	 (let ((loaded-addr (get value 'lisp::%loaded-address)))
	   (unless loaded-addr
	     (error "Miscop ~A is undefined." value))
    
	   (let ((hi-addr (logior (ash clc::type-assembler-code
				       clc::type-shift-16)
				  (logand (ash loaded-addr -16) #xFFFF))))
	     (setf (aref code (+ offset 1)) (logand hi-addr #xFF))
	     (setf (aref code (+ offset 2))
		   (logand (ash loaded-addr -8) #xFF))
	     (setf (aref code (+ offset 3))
		   (logand loaded-addr #xFF)))))))))


;;; MAKE-CORE-COMPONENT  --  Interface
;;;
;;;    Dump a component to core.  We pass in the assembler fixups, code vector
;;; and node info.
;;;
(defun make-core-component (component code-vector code-length
				      node-vector nodes-length
				      fixups object)
  (declare (type component component) (type index nodes-length code-length)
	   (list fixups) (type core-object object))
  (without-gcing
    (let* ((2comp (component-info component))
	   (constants (ir2-component-constants 2comp))
	   (box-num (length constants))
	   (code-obj (%primitive alloc-function box-num))
	   (code-vec (%primitive alloc-code code-length)))
      (%primitive byte-blt code-vector 0 code-vec 0 code-length)
      (%primitive header-set code-obj %function-code-slot code-vec)
      (do-core-fixups code-vec fixups)
      
      (dolist (entry (ir2-component-entries 2comp))
	(make-function-entry entry code-obj code-vec object))
      
      (%primitive set-vector-subtype code-obj %function-constants-subtype)
      
      (%primitive header-set code-obj %function-name-slot
		  (component-name component))
      (let ((info (debug-info-for-component component node-vector
					    nodes-length)))
	(push info (core-object-debug-info object))
	(%primitive header-set code-obj %function-constants-debug-info-slot
		    info))
      
      (dotimes (i box-num)
	(let ((const (aref constants i)))
	  (etypecase const
	    (null)
	    (constant
	     (%primitive header-set code-obj i (constant-value const)))
	    (list
	     (ecase (car const)
	       (:entry
		(%primitive header-set code-obj i
			    (core-function-or-lose (cdr const) object)))
	       (:label
		(%primitive header-set code-obj i
			    (+ (label-location (cdr const))
				clc::i-vector-header-size))))))))))
  (undefined-value))


;;; CORE-CALL-TOP-LEVEL-LAMBDA  --  Interface
;;;
;;;    Call the top-level lambda function dumped for Entry, returning the
;;; values.
;;;
(defun core-call-top-level-lambda (entry object)
  (declare (type clambda entry) (type core-object object))
  (funcall (core-function-or-lose entry object)))


;;; FIX-CORE-SOURCE-INFO  --  Interface
;;;
;;;    Backpatch all the DEBUG-INFOs dumped so far with the specified
;;; SOURCE-INFO list.
;;;
(defun fix-core-source-info (info object)
  (declare (type source-info info) (type core-object object))
  (let ((res (debug-source-for-info info)))
    (dolist (info (core-object-debug-info object))
      (setf (compiled-debug-info-source info) res))
    (setf (core-object-debug-info object) ()))
  (undefined-value))
