;;; -*- Package: HEAPANAL -*-

(in-package "HEAPANAL")
(use-package "SYSTEM")
(use-package "EXT")
(use-package "MACH")
(use-package "ALIEN")
(use-package "C-CALL")


;;;; MACH primitives we need.

(def-alien-routine ("vm_read" vm_read) int
  (task port)
  (address unsigned-long)
  (size unsigned-long)
  (data system-area-pointer :out)
  (data-size unsigned-long :out))

(def-alien-routine ("task_by_unix_pid" task_by_unix_pid) int
  (task port)
  (pid unsigned-long)
  (result port :out))



;;;; Data structures for holding the info.

(defstruct (heap-info
	    (:constructor make-heap-info (pid))
	    (:print-function %print-heap-info))
  ;;
  ;; The UNIX pid for the task we are interested in.
  (pid (required-argument))
  ;;
  ;; The MACH port for the task.
  (task (gr-call* task_by_unix_pid *task-self* pid))
  ;;
  ;; Cache used by read-heap.
  (prev-addr nil :type (or null (unsigned-byte 32)))
  (prev-sap nil :type (or null system-area-pointer))
  ;;
  ;; Symbol table used by the task, or NIL if it's the same as ours.
  (symbol-table nil :type (or null hash-table))
  ;;
  ;; The range of dynamic space, so we can tell dynamic pointers.
  (dynamic-start nil :type (or null (unsigned-byte 32)))
  (dynamic-end nil :type (or null (unsigned-byte 32)))
  ;;
  ;; Hash table mapping addresses to objects.
  (objects (make-hash-table :test #'equal :size 1000 :rehash-size 1000))
  ;;
  ;; Queue of ungroveled objects.  Objects are added at the head, and poped
  ;; from the tail.
  (queue-head nil :type list)
  (queue-tail nil :type list))

(defun %print-heap-info (info stream depth)
  (declare (ignore depth))
  (print-unreadable-object (info stream :type t :identity t)
    (format stream "for pid ~D" (heap-info-pid info))))

(defstruct (object
	    (:print-function %print-object)
	    (:constructor make-object (address)))
  ;;
  ;; The address this object lives at.
  (address (required-argument) :type (unsigned-byte 32))
  ;;
  ;; List of references to this object.  Each entry is either an object,
  ;; or an ub-32 stack address.
  (references nil :type list))

(defun %print-object (object stream depth)
  (declare (ignore depth))
  (print-unreadable-object (object stream :type t)
    (format stream "#x~8,'0X" (object-address object))))



;;;; Interface to importing stuff.

(defparameter page-size (get-page-size))

(defun read-heap (info address)
  (declare (type heap-info info))
  (multiple-value-bind (page offset) (truncate address page-size)
    (let ((address (* page page-size)))
      (cond
       ((and (heap-info-prev-addr info)
	     (= (heap-info-prev-addr info) address))
	(values (sap+ (heap-info-prev-sap info) offset)
		(- (* page-size 2) offset)))
       (t
	(when (heap-info-prev-sap info)
	  (gr-call vm_deallocate *task-self* (heap-info-prev-sap info)
		   (* page-size 2))
	  (setf (heap-info-prev-addr info) nil)
	  (setf (heap-info-prev-sap info) nil))
	(gr-bind (base size)
		 (vm_read (heap-info-task info) address (* page-size 2))
	  (declare (ignore size))
	  (setf (heap-info-prev-addr info) address)
	  (setf (heap-info-prev-sap info) base)
	  (values (sap+ base offset)
		  (- (* page-size 2) offset))))))))


;;;; Symbol table interface.

(defun load-symbol-table (info pathname)
  (with-open-file (stream pathname)
    (setf (heap-info-symbol-table info) (make-hash-table :test #'equal))
    ))

(defun extract-foreign-symbol-value (info name)
  (let ((sym-tab (heap-info-symbol-table info)))
    (sap-ref-32 (read-heap info
			   (if sym-tab
			       (or (gethash name sym-tab)
				   (error "Unknown foreign symbol: ~S" name))
			       (sap-int (foreign-symbol-address name))))
		0)))



;;;; Utilities.

(defun find-object (info addr &optional create)
  (let ((object (gethash addr (heap-info-objects info))))
    (when (and (null object) create)
      (setf object (make-object addr))
      (let ((new (list object)))
	(if (heap-info-queue-head info)
	    (setf (cdr (heap-info-queue-head info)) new
		  (heap-info-queue-head info) new)
	    (setf (heap-info-queue-head info) new
		  (heap-info-queue-tail info) new)))
      (setf (gethash addr (heap-info-objects info)) object))
    object))

(defun next-object (info)
  (let ((tail (heap-info-queue-tail info)))
    (when tail
      (let ((result (pop tail)))
	(setf (heap-info-queue-tail info) tail)
	(unless tail
	  (setf (heap-info-queue-head info) nil))
	result))))

(declaim (inline dynamic-pointer-p))
(defun dynamic-pointer-p (info ptr)
  (and (logbitp 0 ptr)
       (<= (heap-info-dynamic-start info) ptr)
       (< ptr (heap-info-dynamic-end info))))


;;;; 

(defun grovel-stack (info start end)
  (let ((addr start))
    (loop
      (when (>= addr end)
	(return))
      (multiple-value-bind (sap bytes) (read-heap info addr)
	(let ((count (min bytes (- end addr))))
	  (dotimes (index (floor count 4))
	    (let ((value (sap-ref-32 sap (* index 4))))
	      (when (dynamic-pointer-p info value)
		(push (+ addr (* index 4))
		      (object-references (find-object info value t))))))
	  (incf addr count))))))

(defun grovel-control-stack (info)
  (format t "Groveling control stack...~%")
  (grovel-stack
   info
   (extract-foreign-symbol-value info "control_stack")
   (extract-foreign-symbol-value info "current_control_stack_pointer")))

(defun grovel-binding-stack (info)
  (format t "Groveling binding stack...~%")
  (grovel-stack
   info
   (extract-foreign-symbol-value info "binding_stack")
   (extract-foreign-symbol-value info "current_binding_stack_pointer")))

(defun grovel-object (info object)
  (let* ((orig-addr (object-address object))
	 (lowtag (logand orig-addr vm:lowtag-mask))
	 (address (- orig-addr lowtag)))
    (multiple-value-bind
	(sap available)
	(read-heap info address)
      (multiple-value-bind
	  (words words-to-grovel)
	  (ecase lowtag
	    (#.vm:list-pointer-type
	     (values 2 2))
	    (#.vm:instance-pointer-type
	     (let* ((header (sap-ref-32 sap 0))
		    (length (1+ (ash header (- vm:type-bits)))))
	       (values length length)))
	    (#.vm:other-pointer-type
	     (let* ((header (sap-ref-32 sap 0))
		    (type (logand header vm:type-mask)))
	       (flet ((vector-len (bits-per-element &optional (extra 0))
			(+ (ceiling (* (+ (ash (sap-ref-32
						sap
						(* vm:vector-length-slot 4))
					       -2)
					  extra)
				       bits-per-element)
				    vm:word-bits)
			   vm:vector-data-offset)))
		 (declare (inline vector-len))
		 (ecase type
		   ((#.vm:bignum-type
		     #.vm:single-float-type
		     #.vm:double-float-type
		     #.vm:sap-type)
		    (values (1+ (ash header (- vm:type-bits))) 0))
		   ((#.vm:ratio-type
		     #.vm:complex-type
		     #.vm:simple-array-type
		     #.vm:complex-string-type
		     #.vm:complex-bit-vector-type
		     #.vm:complex-vector-type
		     #.vm:complex-array-type
		     #.vm:value-cell-header-type
		     #.vm:symbol-header-type
		     #.vm:weak-pointer-type
		     #.vm:fdefn-type)
		    (let ((length (1+ (ash header (- vm:type-bits)))))
		      (values length length)))
		   (#.vm:simple-string-type
		    (values (vector-len 8 1) 0))
		   (#.vm:simple-bit-vector-type
		    (values (vector-len 1) 0))
		   (#.vm:simple-array-unsigned-byte-2-type
		    (values (vector-len 2) 0))
		   (#.vm:simple-array-unsigned-byte-4-type
		    (values (vector-len 4) 0))
		   (#.vm:simple-array-unsigned-byte-8-type
		    (values (vector-len 8) 0))
		   (#.vm:simple-array-unsigned-byte-16-type
		    (values (vector-len 16) 0))
		   ((#.vm:simple-array-unsigned-byte-32-type
		     #.vm:simple-array-single-float-type)
		    (values (vector-len 32) 0))
		   (#.vm:simple-array-double-float-type
		    (values (vector-len 64) 0))
		   (#.vm:simple-vector-type
		    (let ((length (vector-len 32)))
		      (values length length)))
		   (#.vm:code-header-type
		    (values
		     (ash (sap-ref-32 sap (* vm:code-code-size-slot 8)) -2)
		     (ash header (- vm:type-bits))))
		   (#.vm:return-pc-header-type
		    (push object
			  (object-references
			   (find-object info
					(- orig-addr
					   (* (ash header (- vm:type-bits)) 4))
					t)))
		    (values 2 0))))))
	    (#.vm:function-pointer-type
	     (let* ((header (sap-ref-32 sap 0))
		    (type (logand header vm:type-mask)))
	       (ecase type
		 ((#.vm:closure-header-type
		   #.vm:funcallable-instance-header-type)
		  (let ((length (1+ (ash header (- vm:type-bits)))))
		    (values length length)))
		 ((#.vm:function-header-type
		   #.vm:closure-function-header-type)
		  (push object
			(object-references
			 (find-object
			  info
			  (+ (- address
				(* (ash header (- vm:type-bits)) 4))
			     vm:other-pointer-type)
			  t)))
		  (values vm:function-code-offset
			  vm:function-code-offset))))))
	(unless (zerop words-to-grovel)
	  (loop
	    (let ((count (min (floor available 4) words-to-grovel)))
	      (dotimes (i count)
		(let ((value (sap-ref-32 sap (* i 4))))
		  (when (dynamic-pointer-p info value)
		    (push object
			  (object-references (find-object info value t))))))
	      (decf words-to-grovel count)
	      (when (<= words-to-grovel 0)
		(return))
	      (incf address (* count 4))
	      (multiple-value-setq (sap available) (read-heap info address)))))
	words))))

(defun grovel-static-space (info)
  (format t "Groveling static space")
  (force-output)
  (let ((addr
	 (extract-foreign-symbol-value info "static_space"))
	(end
	 (sap-ref-32 (read-heap info
				(logandc2 (kernel:get-lisp-obj-address
					   'lisp::*static-space-free-pointer*)
					  vm:lowtag-mask))
		     (* vm:symbol-value-slot 4)))
	(count 0))
    (loop
      (when (>= addr end)
	(return))
      (incf count)
      (when (>= count 1000)
	(write-char #\.)
	(force-output)
	(setf count 0))
      (let* ((header (sap-ref-32 (read-heap info addr) 0))
	     (object
	      (if (= (logand header 3) 2)
		  ;; There is an other-immedate there, maybe it's a header.
		  (let ((type (logand header vm:type-mask)))
		    (cond ((= type vm:instance-header-type)
			   (logior addr vm:instance-pointer-type))
			  ((<= type vm:code-header-type)
			   (logior addr vm:other-pointer-type))
			  ((<= type vm:closure-function-header-type)
			   (logior addr vm:function-pointer-type))
			  ((or (= type vm:base-char-type)
			       (= type vm:unbound-marker-type))
			   ;; Assume we've got a cons pointing to it.
			   (logior addr vm:list-pointer-type))
			  (t
			   (logior addr vm:other-pointer-type))))
		  ;; It doesn't point to a header, so assume it's a cons cell.
		  (logior addr vm:list-pointer-type)))
	     (words (grovel-object info (make-object object))))
	(incf addr (ash (ash (1+ words) -1) 3))))))

(defun grovel-objects (info)
  (format t "~&Groveling dynamic space")
  (let ((count 0))
    (loop
      (incf count)
      (when (>= count 1000)
	(write-char #\.)
	(force-output)
	(setf count 0))
      (let ((object (next-object info)))
	(if object
	    (grovel-object info object)
	    (return))))))

(defun grovel-heap (pid &key symbol-table)
  (let ((info (make-heap-info pid)))
    (when symbol-table
      (load-symbol-table info symbol-table))
    (setf (heap-info-dynamic-start info)
	  (extract-foreign-symbol-value info "current_dynamic_space"))
    (setf (heap-info-dynamic-end info)
	  (extract-foreign-symbol-value info
					"current_dynamic_space_free_pointer"))
    (grovel-control-stack info)
    (grovel-binding-stack info)
    (grovel-static-space info)
    (grovel-objects info)
    info))

(defun find-instances-of-type (info type-address)
  (let ((results nil))
    (maphash #'(lambda (key object)
		 (declare (ignore key))
		 (let ((address (object-address object)))
		   (when (and (= (logand address vm:lowtag-mask)
				 vm:instance-pointer-type)
			      (= (sap-ref-32
				  (read-heap info
					     (- address 
						vm:instance-pointer-type))
				  4)
				 type-address))
		     (push object results))))
	     (heap-info-objects info))
    results))

(defun find-roots-for (info address)
  (let ((object (find-object info address)))
    (unless object
      (error "#x~8,'0X does not appear to be reachable in ~S" address info))
    (let ((queue (list (list object)))
	  (depth 0))
      (loop
	(let ((list queue)
	      (any-results nil))
	  (setf queue nil)
	  (format t "Searching at depth ~D~%" depth)
	  (dolist (entry list)
	    (let ((object (car entry)))
	      (cond ((and (object-p object)
			  (dynamic-pointer-p info (object-address object)))
		     (dolist (reference (object-references object))
		       (unless (member reference entry)
			 (push (cons reference entry) queue))))
		    (t
		     (unless any-results
		       (terpri))
		     (setf any-results t)
		     (describe-path info entry)))))
	  (unless queue
	    (return))
	  (when any-results
	    (return))
	  (incf depth)))))
  nil)

(defun describe-path (info path)
  (dolist (object path)
    (describe-object info object))
  (terpri))

(defun extract-symbol-name (info address)
  (let* ((pname-addr (sap-ref-32 (read-heap info
					    (- address
					       vm:other-pointer-type))
				 (* vm:symbol-name-slot 4)))
	 (sap (read-heap info (- pname-addr vm:other-pointer-type)))
	 (len (ash (sap-ref-32 sap 1) -2))
	 (result (make-string len)))
    (dotimes (i len)
      (setf (schar result i)
	    (code-char (sap-ref-8 sap
				  (+ i
				     (* vm:vector-data-offset
					vm:word-bytes))))))
    result))

(defun describe-object (info object)
  (if (object-p object)
      (let* ((orig-addr (object-address object))
	     (lowtag (logand orig-addr vm:lowtag-mask))
	     (addr (- orig-addr lowtag)))
	(ecase lowtag
	  (#.vm:list-pointer-type
	   (format t "Cons cell #x~8,'0X~%" orig-addr))
	  (#.vm:instance-pointer-type
	   (format t "Instance ~A at #x~8,'0X~%"
		   (extract-symbol-name info
					(sap-ref-32 (read-heap info addr) 4))
		   orig-addr))
	  ((#.vm:other-pointer-type
	    #.vm:function-pointer-type)
	   (let ((header (sap-ref-32 (read-heap info addr) 0)))
	     (format t "~A at #x~8,'0X~%"
		     (ecase (logand header vm:type-mask)
		       (#.vm:symbol-header-type
			(format nil "Symbol ~A"
				(extract-symbol-name info orig-addr)))
		       (#.vm:bignum-type "bignum")
		       (#.vm:ratio-type "ratio")
		       (#.vm:single-float-type "single-float")
		       (#.vm:double-float-type "double-float")
		       (#.vm:complex-type "complex")
		       (#.vm:simple-array-type "simple-array")
		       (#.vm:simple-string-type "simple-string")
		       (#.vm:simple-bit-vector-type "simple-bit-vector")
		       (#.vm:simple-vector-type "simple-vector")
		       (#.vm:simple-array-unsigned-byte-2-type
			"simple-array-unsigned-byte-2")
		       (#.vm:simple-array-unsigned-byte-4-type
			"simple-array-unsigned-byte-4")
		       (#.vm:simple-array-unsigned-byte-8-type
			"simple-array-unsigned-byte-8")
		       (#.vm:simple-array-unsigned-byte-16-type
			"simple-array-unsigned-byte-16")
		       (#.vm:simple-array-unsigned-byte-32-type
			"simple-array-unsigned-byte-32")
		       (#.vm:simple-array-single-float-type
			"simple-array-single-float")
		       (#.vm:simple-array-double-float-type
			"simple-array-double-float")
		       (#.vm:complex-string-type "complex-string")
		       (#.vm:complex-bit-vector-type "complex-bit-vector")
		       (#.vm:complex-vector-type "complex-vector")
		       (#.vm:complex-array-type "complex-array")
		       (#.vm:code-header-type "code-header")
		       (#.vm:function-header-type "function")
		       (#.vm:closure-header-type "closure-header")
		       (#.vm:funcallable-instance-header-type
			"funcallable-instance-header")
		       (#.vm:closure-function-header-type
			"closure-function")
		       (#.vm:return-pc-header-type "return-pc-header")
		       (#.vm:value-cell-header-type "value-cell-header")
		       (#.vm:symbol-header-type "symbol-header")
		       (#.vm:sap-type "sap")
		       (#.vm:weak-pointer-type "weak-pointer")
		       (#.vm:fdefn-type "fdefn"))
		     orig-addr)))))
      (format t "Stack reference at #x~8,'0X~%" object)))
