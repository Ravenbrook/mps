;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/core.lisp,v 1.40 2002/08/27 22:18:27 moore Exp $")
;;;
;;; **********************************************************************
;;;
;;;
;;;    This file contains stuff that knows how to load compiled code directly
;;; into core, e.g. incremental compilation.
;;;
(in-package "C")


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
  ;; A hashtable translating ENTRY-INFO structures to a list of pairs
  ;; (<code object> . <offset>) describing the places that need to be
  ;; backpatched to point to the function for ENTRY-INFO.
  (patch-table (make-hash-table :test #'eq) :type hash-table)
  ;;
  ;; A list of all the DEBUG-INFO objects created, kept so that we can
  ;; backpatch with the source info.
  (debug-info () :type list))


;;; NOTE-FUNCTION -- Internal.
;;;
;;; Note the existance of FUNCTION.
;;; 
(defun note-function (info function object)
  (declare (type function function)
	   (type core-object object))
  (let ((patch-table (core-object-patch-table object)))
    (dolist (patch (gethash info patch-table))
      (setf (code-header-ref (car patch) (the index (cdr patch))) function))
    (remhash info patch-table))
  (setf (gethash info (core-object-entry-table object)) function)
  (undefined-value))


;;; MAKE-FUNCTION-ENTRY  --  Internal
;;;
;;;    Make a function entry, filling in slots from the ENTRY-INFO.
;;;
(defun make-function-entry (entry code-obj object)
  (declare (type entry-info entry) (type core-object object))
  (let ((offset (label-position (entry-info-offset entry))))
    (declare (type index offset))
    (unless (zerop (logand offset vm:lowtag-mask))
      (error "Unaligned function object, offset = #x~X." offset))
    (let ((res (%primitive compute-function code-obj offset)))
      (setf (%function-self res) res)
      (setf (%function-next res) (%code-entry-points code-obj))
      (setf (%code-entry-points code-obj) res)
      (setf (%function-name res) (entry-info-name entry))
      (setf (%function-arglist res) (entry-info-arguments entry))
      (setf (%function-type res) (entry-info-type entry))

      (note-function entry res object))))

;;; DO-CORE-FIXUPS  --  Internal
;;;
;;;    Do "load-time" fixups on the code vector.
;;;
(defun do-core-fixups (code fixups)
  (declare (list fixups))
  (dolist (info fixups)
    (let* ((kind (first info))
	   (fixup (second info))
	   (name (fixup-name fixup))
	   (flavor (fixup-flavor fixup))
	   (offset (third info)))
      (multiple-value-bind
	  (value found)
	  (ecase flavor
	    (:assembly-routine
	     (assert (symbolp name))
	     (gethash name lisp::*assembler-routines*))
	    ((:foreign :foreign-data)
	     (assert (stringp name))
	     (let ((val (lisp::foreign-symbol-address-aux name
							  (if (eq flavor
								  :foreign)
							      :code
							      :data))))
	       ;; Foreign-symbol-address-aux always signals exactly
	       ;; the same error we would if the symbol isn't found
	       (values val t)))
	    #+x86
	    (:code-object
	     (values (get-lisp-obj-address code) t)))
	(unless found
	  (error (ecase flavor
		   (:assembly-routine "Undefined assembler routine: ~S")
		   (:foreign "Unknown foreign symbol: ~S"))
		 name))
	(vm:fixup-code-object code offset value kind)))))




;;; REFERENCE-CORE-FUNCTION  --  Internal
;;;
;;;    Stick a reference to the function Fun in Code-Object at index I.  If the
;;; function hasn't been compiled yet, make a note in the Patch-Table.
;;;
(defun reference-core-function (code-obj i fun object)
  (declare (type core-object object) (type functional fun)
	   (type index i))
  (let* ((info (leaf-info fun))
	 (found (gethash info (core-object-entry-table object))))
    (if found
	(setf (code-header-ref code-obj i) found)
	(push (cons code-obj i)
	      (gethash info (core-object-patch-table object)))))
  (undefined-value))


;;; MAKE-CORE-COMPONENT  --  Interface
;;;
;;;    Dump a component to core.  We pass in the assembler fixups, code vector
;;; and node info.
;;;
(defun make-core-component (component segment length trace-table fixups object)
  (declare (type component component)
	   (type new-assem:segment segment)
	   (type index length)
	   (list trace-table fixups)
	   (type core-object object))
  (without-gcing
    (let* ((2comp (component-info component))
	   (constants (ir2-component-constants 2comp))
	   (trace-table (pack-trace-table trace-table))
	   (trace-table-len (length trace-table))
	   (trace-table-bits (* trace-table-len tt-bits-per-entry))
	   (total-length (+ length (ceiling trace-table-bits vm:byte-bits)))
	   (box-num (- (length constants) vm:code-trace-table-offset-slot))
	   #+x86
	   (code-obj
	    (if (and (boundp lisp::*enable-dynamic-space-code*)
		     lisp::*enable-dynamic-space-code*)
		(%primitive allocate-dynamic-code-object box-num total-length)
	      (%primitive allocate-code-object box-num total-length)))
	   #-x86
	   (code-obj
	    (%primitive allocate-code-object box-num total-length))
	   (fill-ptr (code-instructions code-obj)))
      (declare (type index box-num total-length))

      (new-assem:segment-map-output
       segment
       #'(lambda (sap amount)
	   (declare (type system-area-pointer sap)
		    (type index amount))
	   (system-area-copy sap 0 fill-ptr 0
			     (* amount vm:byte-bits))
	   (setf fill-ptr (sap+ fill-ptr amount))))
      (do-core-fixups code-obj fixups)
      
      (dolist (entry (ir2-component-entries 2comp))
	(make-function-entry entry code-obj object))
      
      (vm:sanctify-for-execution code-obj)

      (let ((info (debug-info-for-component component)))
	(push info (core-object-debug-info object))
	(setf (%code-debug-info code-obj) info))
      
      (setf (code-header-ref code-obj vm:code-trace-table-offset-slot) length)
      (copy-to-system-area trace-table (* vm:vector-data-offset vm:word-bits)
			   fill-ptr 0 trace-table-bits)

      (do ((index vm:code-constants-offset (1+ index)))
	  ((>= index (length constants)))
	(let ((const (aref constants index)))
	  (etypecase const
	    (null)
	    (constant
	     (setf (code-header-ref code-obj index)
		   (constant-value const)))
	    (list
	     (ecase (car const)
	       (:entry
		(reference-core-function code-obj index
					 (cdr const) object))
	       (:fdefinition
		(setf (code-header-ref code-obj index)
		      (lisp::fdefinition-object (cdr const) t))))))))))
  (undefined-value))


;;; MAKE-CORE-BYTE-COMPONENT -- Interface.
;;;
(defun make-core-byte-component (segment length constants xeps object)
  (declare (type new-assem:segment segment)
	   (type index length)
	   (type vector constants)
	   (type list xeps)
	   (type core-object object))
  (without-gcing
    (let* ((num-constants (length constants))
	   (code-obj (%primitive allocate-code-object
				 (the index (1+ num-constants))
				 length))
	   (fill-ptr (code-instructions code-obj)))
      (declare (type index length)
	       (type system-area-pointer fill-ptr))

      (new-assem:segment-map-output
       segment
       #'(lambda (sap amount)
	   (declare (type system-area-pointer sap)
		    (type index amount))
	   (system-area-copy sap 0 fill-ptr 0
			     (* amount vm:byte-bits))
	   (setf fill-ptr (sap+ fill-ptr amount))))

      (setf (code-header-ref code-obj vm:code-trace-table-offset-slot)
	    nil)
      (dolist (noise xeps)
	(let ((xep (cdr noise)))
	  (setf (byte-function-component xep) code-obj)
	  (initialize-byte-compiled-function xep)
	  (note-function (lambda-info (car noise)) xep object)))

      (dotimes (index num-constants)
	(let ((const (aref constants index))
	      (code-obj-index (+ index vm:code-constants-offset)))
	  (etypecase const
	    (null)
	    (constant
	     (setf (code-header-ref code-obj code-obj-index)
		   (constant-value const)))
	    (list
	     (ecase (car const)
	       (:entry
		(reference-core-function code-obj code-obj-index (cdr const)
					 object))
	       (:fdefinition
		(setf (code-header-ref code-obj code-obj-index)
		      (lisp::fdefinition-object (cdr const) t)))
	       (:type-predicate
		(let ((*unparse-function-type-simplify* t))
		  (setf (code-header-ref code-obj code-obj-index)
			(load-type-predicate (type-specifier (cdr const))))))
	       (:xep
		(let ((xep (cdr (assoc (cdr const) xeps :test #'eq))))
		  (assert xep)
		  (setf (code-header-ref code-obj code-obj-index) xep))))))))))

  (undefined-value))


;;; CORE-CALL-TOP-LEVEL-LAMBDA  --  Interface
;;;
;;;    Call the top-level lambda function dumped for Entry, returning the
;;; values.  Entry may be a :TOP-LEVEL-XEP functional.
;;;
(defun core-call-top-level-lambda (entry object)
  (declare (type functional entry) (type core-object object))
  (funcall (or (gethash (leaf-info entry)
			(core-object-entry-table object))
	       (error "Unresolved forward reference."))))


;;; FIX-CORE-SOURCE-INFO  --  Interface
;;;
;;;    Backpatch all the DEBUG-INFOs dumped so far with the specified
;;; SOURCE-INFO list.  We also check that there are no outstanding forward
;;; references to functions.
;;;
(defun fix-core-source-info (info object source-info)
  (declare (type source-info info) (type core-object object))
  (assert (zerop (hash-table-count (core-object-patch-table object))))
  (let ((res (debug-source-for-info info)))
    (dolist (sinfo res)
      (setf (debug-source-info sinfo) source-info))
    (dolist (info (core-object-debug-info object))
      (setf (compiled-debug-info-source info) res))
    (setf (core-object-debug-info object) ()))
  (undefined-value))


;;;; Code-instruction-streams

(defstruct (code-instruction-stream
	    (:print-function %print-code-inst-stream)
	    (:include lisp-stream
		      (lisp::sout #'code-inst-stream-sout)
		      (lisp::bout #'code-inst-stream-bout)
		      (lisp::misc #'code-inst-stream-misc))
	    (:constructor make-code-instruction-stream
			  (code-object
			   &aux
			   (current (code-instructions code-object))
			   (end (sap+ current
				      (* (%code-code-size code-object)
					 vm:word-bytes))))))
  code-object
  current
  end)

(defun %print-code-inst-stream (code-inst-stream stream depth)
  (declare (ignore depth))
  (format stream "#<Code Instruction Stream for ~S>"
	  (code-instruction-stream-code-object code-inst-stream)))

(defun code-inst-stream-sout (stream string start end)
  (let* ((start (or start 0))
	 (end (or end (length string)))
	 (length (- end start))
	 (current (code-instruction-stream-current stream))
	 (new (sap+ current length)))
    (when (sap> new (code-instruction-stream-end stream))
      (error "Writing ~D bytes to ~S would cause it to overflow."
	     length stream))
    (copy-to-system-area string (+ (* start vm:byte-bits)
				   (* vm:vector-data-offset vm:word-bits))
			 current 0
			 (* length vm:byte-bits))
    (setf (code-instruction-stream-current stream) new)))

(defun code-inst-stream-bout (stream byte)
  (declare (type code-instruction-stream stream)
	   (type (unsigned-byte 8) byte))
  (let* ((current (code-instruction-stream-current stream))
	 (new (sap+ current 1)))
    (when (sap> new (code-instruction-stream-end stream))
      (error "Writing another byte to ~S would cause it to overflow."
	     stream))
    (setf (sap-ref-8 current 0) byte)
    (setf (code-instruction-stream-current stream) new)))

(defun code-inst-stream-misc (stream method &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case method
    (:close
     (lisp::set-closed-flame stream))
    (t
     nil)))
