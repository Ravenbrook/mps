;;; -*- Mode: Lisp; Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/room.lisp,v 1.36 2006/07/20 16:19:35 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Heap grovelling memory usage stuff.
;;; 
(in-package "VM")
(use-package "SYSTEM")
(export '(memory-usage count-no-ops descriptor-vs-non-descriptor-storage
		       instance-usage find-holes print-allocated-objects
		       code-breakdown uninterned-symbol-count
		       list-allocated-objects))
(in-package "LISP")
(import '(
	  dynamic-0-space-start dynamic-1-space-start read-only-space-start
	  static-space-start current-dynamic-space-start
	  *static-space-free-pointer* *read-only-space-free-pointer*)
	"VM")
(in-package "VM")


;;;; Type format database.

(eval-when (compile load eval)
  (defstruct (room-info (:make-load-form-fun :just-dump-it-normally))
    ;;
    ;; The name of this type.
    (name nil :type symbol)
    ;;
    ;; Kind of type (how we determine length).
    (kind (required-argument)
	  :type (member :lowtag :fixed :header :vector
			:string :code :closure :instance))
    ;;
    ;; Length if fixed-length, shift amount for element size if :vector.
    (length nil :type (or fixnum null))))

(eval-when (compile eval)

(defvar *meta-room-info* (make-array 256 :initial-element nil))

(dolist (obj *primitive-objects*)
  (let ((header (primitive-object-header obj))
	(lowtag (primitive-object-lowtag obj))
	(name (primitive-object-name obj))
	(variable (primitive-object-variable-length obj))
	(size (primitive-object-size obj)))
    (cond
     ((not lowtag))
     ((not header)
      (let ((info (make-room-info :name name  :kind :lowtag))
	    (lowtag (symbol-value lowtag)))
	(declare (fixnum lowtag))
	(dotimes (i 32)
	  (setf (svref *meta-room-info* (logior lowtag (ash i 3))) info))))
     (variable)
     (t
      (setf (svref *meta-room-info* (symbol-value header))
	    (make-room-info :name name  :kind :fixed  :length size))))))

(dolist (code (list complex-string-type simple-array-type
		    complex-bit-vector-type complex-vector-type 
		    complex-array-type))
  (setf (svref *meta-room-info* code)
	(make-room-info :name 'array-header  :kind :header)))

(setf (svref *meta-room-info* bignum-type)
      (make-room-info :name 'bignum  :kind :header))

(setf (svref *meta-room-info* closure-header-type)
      (make-room-info :name 'closure  :kind :closure))

(dolist (stuff '((simple-bit-vector-type . -3)
		 (simple-vector-type . 2)
		 (simple-array-unsigned-byte-2-type . -2)
		 (simple-array-unsigned-byte-4-type . -1)
		 (simple-array-unsigned-byte-8-type . 0)
		 (simple-array-unsigned-byte-16-type . 1)
		 (simple-array-unsigned-byte-32-type . 2)
		 (simple-array-signed-byte-8-type . 0)
		 (simple-array-signed-byte-16-type . 1)
		 (simple-array-signed-byte-30-type . 2)
		 (simple-array-signed-byte-32-type . 2)
		 (simple-array-single-float-type . 2)
		 (simple-array-double-float-type . 3)
		 (simple-array-complex-single-float-type . 3)
		 (simple-array-complex-double-float-type . 4)
		 #+double-double
		 (simple-array-double-double-float-type . 4)
		 #+double-double
		 (simple-array-complex-double-double-float-type . 5)
		 ))
  (let ((name (car stuff))
	(size (cdr stuff)))
    (setf (svref *meta-room-info* (symbol-value name))
	  (make-room-info :name name  :kind :vector  :length size))))

(setf (svref *meta-room-info* simple-string-type)
      (make-room-info :name 'simple-string-type :kind :string :length 0))

(setf (svref *meta-room-info* code-header-type)
      (make-room-info :name 'code  :kind :code))

(setf (svref *meta-room-info* instance-header-type)
      (make-room-info :name 'instance :kind :instance))

); eval-when (compile eval)

(defparameter *room-info* '#.*meta-room-info*)
(deftype spaces () '(member :static :dynamic :read-only))
;; A type denoting the virtual address available to us.
(deftype memory-size () `(unsigned-byte #.vm:word-bits))

;;;; MAP-ALLOCATED-OBJECTS:

(declaim (type fixnum *static-space-free-pointer*
	       *read-only-space-free-pointer* ))

#+gencgc
(eval-when (compile load eval)
  ;; This had better match the value in gencgc.h!!!!
  (defconstant gencgc-page-size
    #+sparc (* 4 8192)
    #+ppc (* 4 4096)
    #-(or sparc ppc) 4096))

#+gencgc
(def-alien-variable last-free-page c-call:unsigned-int)
    
(defun space-bounds (space)
  (declare (type spaces space))
  (ecase space
    (:static
     (values (int-sap (static-space-start))
	     (int-sap (* *static-space-free-pointer* word-bytes))))
    (:read-only
     (values (int-sap (read-only-space-start))
	     (int-sap (* *read-only-space-free-pointer* word-bytes))))
    (:dynamic
     ;; DYNAMIC-SPACE-FREE-POINTER isn't quite right here for sparc
     ;; and ppc with gencgc.  We really want the last free page, which
     ;; is stored in *allocation-pointer* on x86, but sparc and ppc
     ;; don't have *allocation-pointer*, so grab the value directly
     ;; from last-free-page.
     (values (int-sap (current-dynamic-space-start))
	     #+(and gencgc (or sparc ppc))
	     (int-sap (truly-the (unsigned-byte 32)
				 (+ (current-dynamic-space-start)
				    (the (unsigned-byte 32) (* gencgc-page-size last-free-page)))))
	     #-(and gencgc (or sparc ppc))
	     (dynamic-space-free-pointer)))))

;;; SPACE-BYTES  --  Internal
;;;
;;;    Return the total number of bytes used in Space.
;;;
(defun space-bytes (space)
  (multiple-value-bind (start end)
		       (space-bounds space)
    (- (sap-int end) (sap-int start))))

;;; ROUND-TO-DUALWORD  --  Internal
;;;
;;;    Round Size (in bytes) up to the next dualword (eight/16 byte) boundry.
;;;
(declaim (inline round-to-dualword))
(defun round-to-dualword (size)
  (declare (type memory-size size))
  #-amd64
  (logandc2 (the memory-size (+ size lowtag-mask)) lowtag-mask)
  ;; when we use 4-bit lowtag for amd64 we can get rid of this
  #+amd64
  (logandc2 (the memory-size (+ size 15)) 15))


;;; VECTOR-TOTAL-SIZE  --  Internal
;;;
;;;    Return the total size of a vector in bytes, including any pad.
;;;
(declaim (inline vector-total-size))
(defun vector-total-size (obj info)
  (let ((shift (room-info-length info))
	(len (+ (length (the (simple-array * (*)) obj))
		(ecase (room-info-kind info)
		  (:vector 0)
		  (:string 1)))))
    (declare (type (integer -3 3) shift))
    (round-to-dualword
     (+ (* vector-data-offset word-bytes)
	(the memory-size
	     (if (minusp shift)
		 (ash (the memory-size
			   (+ len (the memory-size
				       (1- (the memory-size (ash 1 (- shift)))))))
		      shift)
		 (ash len shift)))))))

;;; Access to the GENCGC page table for better precision in
;;; MAP-ALLOCATED-OBJECTS.
#+gencgc
(progn
  (declaim (inline find-page-index get-page-table-info))
  (def-alien-routine "find_page_index" c-call:int
    (addr c-call:long))
  (def-alien-routine get-page-table-info c-call:void
    (page c-call:int)
    (flags c-call:int :out)
    (bytes c-call:int :out))
  )

;;; MAP-ALLOCATED-OBJECTS  --  Interface
;;;
;;;    Iterate over all the objects allocated in Space, calling Fun with the
;;; object, the object's type code, and the objects total size in bytes,
;;; including any header and padding.
;;;
(declaim (maybe-inline map-allocated-objects))
#+nil
(defun map-allocated-objects (fun space)
  (declare (type function fun) (type spaces space))
  (without-gcing
    (multiple-value-bind (start end)
			 (space-bounds space)
      (declare (type system-area-pointer start end))
      (declare (optimize (speed 3) (safety 0)))
      (iterate step ((current start))
	(flet ((next (size)
		 (let ((c (etypecase size
			    (fixnum (sap+ current size))
			    (memory-size (sap+ current size)))))
		   (cond ((sap< c end)
			  (step c))
			 (t
			  (assert (sap= c end)))))))
	  (let* ((header (sap-ref-32 current 0))
		 (header-type (logand header #xFF))
		 (info (svref *room-info* header-type)))
	    (cond
	     ((or (not info)
		  (eq (room-info-kind info) :lowtag))
	      (let ((size (* cons-size word-bytes)))
		(funcall fun
			 (make-lisp-obj (logior (sap-int current)
						list-pointer-type))
			 list-pointer-type
			 size)
		(next size)))
	     ((eql header-type closure-header-type)
	      (let* ((obj (make-lisp-obj (logior (sap-int current)
						 function-pointer-type)))
		     (size (round-to-dualword
			    (* (the fixnum (1+ (get-closure-length obj)))
			       word-bytes))))
		(funcall fun obj header-type size)
		(next size)))
	     ((eq (room-info-kind info) :instance)
	      (let* ((obj (make-lisp-obj
			   (logior (sap-int current) instance-pointer-type)))
		     (size (round-to-dualword
			    (* (+ (%instance-length obj) 1) word-bytes))))
		(declare (type memory-size size))
		(funcall fun obj header-type size)
		(assert (zerop (logand size lowtag-mask)))
		#+nil
		(when (> size 200000) (break "Implausible size, prev ~S" prev))
		#+nil
		(setq prev current)
		(next size)))
	     (t
	      (let* ((obj (make-lisp-obj
			   (logior (sap-int current) other-pointer-type)))
		     (size (ecase (room-info-kind info)
			     (:fixed
			      (assert (or (eql (room-info-length info)
					       (1+ (get-header-data obj)))
					  (floatp obj)))
			      (round-to-dualword
			       (* (room-info-length info) word-bytes)))
			     ((:vector :string)
			      (vector-total-size obj info))
			     (:header
			      (round-to-dualword
			       (* (1+ (get-header-data obj)) word-bytes)))
			     (:code
			      (+ (the fixnum
				      (* (get-header-data obj) word-bytes))
				 (round-to-dualword
				  (* (the fixnum (%code-code-size obj))
				     word-bytes)))))))
		(declare (type memory-size size))
		(funcall fun obj header-type size)
		(assert (zerop (logand size lowtag-mask)))
		#+nil
		(when (> size 200000)
		  (break "Implausible size, prev ~S" prev))
		#+nil
		(setq prev current)
		(next size))))))

	#+nil
	prev))))

(defun map-allocated-objects (fun space)
  (declare (type function fun) (type spaces space))
  (without-gcing
   (multiple-value-bind (start end)
       (space-bounds space)
     (declare (type system-area-pointer start end))
     (declare (optimize (speed 3) (safety 0)))
     (let ((skip-tests-until-addr 0)
	   (current start))
       (declare (type (unsigned-byte 31) skip-tests-until-addr))
       (labels
	   ((maybe-finish-mapping ()
	      (unless (sap< current end)
		(return-from map-allocated-objects)))
	    ;; GENCGC doesn't allocate linearly, which means that the
	    ;; dynamic space can contain large blocks of zeros that
	    ;; get accounted as conses in ROOM (and slow down other
	    ;; applications of MAP-ALLOCATED-OBJECTS). To fix this
	    ;; check the GC page structure for the current address.
	    ;; If the page is free or the address is beyond the page-
	    ;; internal allocation offset (bytes-used) skip to the
	    ;; next page immediately.
	    (maybe-skip-page ()
	      #+gencgc
	      (when (eq space :dynamic)
		(let ((tested (>= (sap-int current) skip-tests-until-addr)))
		  (loop with page-mask = (1- gencgc-page-size)
		     for addr of-type (unsigned-byte 32) = (sap-int current)
		     while (>= addr skip-tests-until-addr)
		     do
		     (multiple-value-bind (ret flags bytes-used)
			 (get-page-table-info (find-page-index addr))
		       (declare (ignore ret))
		       (let ((alloc-flag (logand flags #x40)))
			 ;; If the page is not free and the current
			 ;; pointer is still below the allocation
			 ;; offset of the page
			 (when (and (not (zerop alloc-flag))
				    (<= (logand page-mask addr)
					bytes-used))
			   ;; Don't bother testing again until we get
			   ;; past that allocation offset
			   (setf skip-tests-until-addr
				 (+ (logandc2 addr page-mask)
				    (the fixnum bytes-used)))
			   ;; And then continue with the scheduled mapping
			   (return-from maybe-skip-page))
			 ;; Move CURRENT to start of next page
			 (setf current (int-sap (+ (logandc2 addr page-mask)
						   gencgc-page-size)))
			 (maybe-finish-mapping)))))))
	    (next (size)
	      (let ((c (etypecase size
			 (fixnum (sap+ current size))
			 (memory-size (sap+ current size)))))
		(setf current c))))
	 (declare (inline next))
	 (loop
	    (maybe-finish-mapping)
	    (maybe-skip-page)
	    (let* ((header (sap-ref-32 current 0))
		   (header-type (logand header #xFF))
		   (info (svref *room-info* header-type)))
	      (cond
		((or (not info)
		     (eq (room-info-kind info) :lowtag))
		 (let ((size (* cons-size word-bytes)))
		   (funcall fun
			    (make-lisp-obj (logior (sap-int current)
						   list-pointer-type))
			    list-pointer-type
			    size)
		   (next size)))
		((eql header-type closure-header-type)
		 (let* ((obj (make-lisp-obj (logior (sap-int current)
						    function-pointer-type)))
			(size (round-to-dualword
			       (* (the fixnum (1+ (get-closure-length obj)))
				  word-bytes))))
		   (funcall fun obj header-type size)
		   (next size)))
		((eq (room-info-kind info) :instance)
		 (let* ((obj (make-lisp-obj
			      (logior (sap-int current) instance-pointer-type)))
			(size (round-to-dualword
			       (* (+ (%instance-length obj) 1) word-bytes))))
		   (declare (type memory-size size))
		   (funcall fun obj header-type size)
		   (assert (zerop (logand size lowtag-mask)))
		   (next size)))
		(t
		 (let* ((obj (make-lisp-obj
			      (logior (sap-int current) other-pointer-type)))
			(size (ecase (room-info-kind info)
				(:fixed
				 (assert (or (eql (room-info-length info)
						  (1+ (get-header-data obj)))
					     (floatp obj)))
				 (round-to-dualword
				  (* (room-info-length info) word-bytes)))
				((:vector :string)
				 (vector-total-size obj info))
				(:header
				 (round-to-dualword
				  (* (1+ (get-header-data obj)) word-bytes)))
				(:code
				 (+ (the fixnum
				      (* (get-header-data obj) word-bytes))
				    (round-to-dualword
				     (* (the fixnum (%code-code-size obj))
					word-bytes)))))))
		   (declare (type memory-size size))
		   (funcall fun obj header-type size)
		   (assert (zerop (logand size lowtag-mask)))
		   (next size)))))))))))


;;;; MEMORY-USAGE:

;;; TYPE-BREAKDOWN  --  Interface
;;;
;;;    Return a list of 3-lists (bytes object type-name) for the objects
;;; allocated in Space.
;;;
(defun type-breakdown (space)
  (let ((sizes (make-array 256 :initial-element 0 :element-type '(unsigned-byte 32)))
	(counts (make-array 256 :initial-element 0 :element-type '(unsigned-byte 32))))
    (map-allocated-objects
     #'(lambda (obj type size)
	 (declare (type memory-size size) (optimize (speed 3) (safety 0)) (ignore obj))
	 (incf (aref sizes type) size)
	 (incf (aref counts type)))
     space)

    (let ((totals (make-hash-table :test #'eq)))
      (dotimes (i 256)
	(let ((total-count (aref counts i)))
	  (unless (zerop total-count)
	    (let* ((total-size (aref sizes i))
		   (name (room-info-name (aref *room-info* i)))
		   (found (gethash name totals)))
	      (cond (found
		     (incf (first found) total-size)
		     (incf (second found) total-count))
		    (t
		     (setf (gethash name totals)
			   (list total-size total-count name))))))))

      (collect ((totals-list))
	(maphash #'(lambda (k v)
		     (declare (ignore k))
		     (totals-list v))
		 totals)
	(sort (totals-list) #'> :key #'first)))))


;;; PRINT-SUMMARY  --  Internal
;;;
;;;    Handle the summary printing for MEMORY-USAGE.  Totals is a list of lists
;;; (space-name . totals-for-space), where totals-for-space is the list
;;; returned by TYPE-BREAKDOWN.
;;;
(defun print-summary (spaces totals)
  (let ((summary (make-hash-table :test #'eq)))
    (dolist (space-total totals)
      (dolist (total (cdr space-total))
	(push (cons (car space-total) total)
	      (gethash (third total) summary))))

    (collect ((summary-totals))
      (maphash #'(lambda (k v)
		   (declare (ignore k))
		   (let ((sum 0))
		     (declare (type memory-size sum))
		     (dolist (space-total v)
		       (incf sum (first (cdr space-total))))
		     (summary-totals (cons sum v))))
	       summary)
      
      (format t "~2&Summary of spaces: ~(~{~A ~}~)~%" spaces)
      (let ((summary-total-bytes 0)
	    (summary-total-objects 0))
	(declare (type memory-size summary-total-bytes summary-total-objects))
	(dolist (space-totals
		 (mapcar #'cdr (sort (summary-totals) #'> :key #'car)))
	  (let ((total-objects 0)
		(total-bytes 0)
		name)
	    (declare (fixnum total-objects)
		     (type memory-size total-bytes))
	    (collect ((spaces))
	      (dolist (space-total space-totals)
		(let ((total (cdr space-total)))
		  (setq name (third total))
		  (incf total-bytes (first total))
		  (incf total-objects (second total))
		  (spaces (cons (car space-total) (first total)))))
	      (format t "~%~A:~%    ~:D bytes, ~:D object~:P"
		      name total-bytes total-objects)
	      (dolist (space (spaces))
		(format t ", ~D% ~(~A~)"
			(round (* (cdr space) 100) total-bytes)
			(car space)))
	      (format t ".~%")
	      (incf summary-total-bytes total-bytes)
	      (incf summary-total-objects total-objects))))
	(format t "~%Summary total:~%    ~:D bytes, ~:D objects.~%"
		summary-total-bytes summary-total-objects)))))


;;; REPORT-SPACE-TOTAL  --  Internal
;;;
;;;    Report object usage for a single space.
;;;
(defun report-space-total (space-total cutoff)
  (declare (list space-total) (type (or single-float null) cutoff))
  (format t "~2&Breakdown for ~(~A~) space:~%" (car space-total))
  (let* ((types (cdr space-total))
	 (total-bytes (reduce #'+ (mapcar #'first types)))
	 (total-objects (reduce #'+ (mapcar #'second types)))
	 (cutoff-point (if cutoff
			   (truncate (* (float total-bytes) cutoff))
			   0))
	 (reported-bytes 0)
	 (reported-objects 0))
    (declare (fixnum total-objects cutoff-point reported-objects)
	     (type memory-size total-bytes reported-bytes))
    (loop for (bytes objects name) in types do
      (when (<= bytes cutoff-point)
	(format t "  ~13:D bytes for ~9:D other object~2:*~P.~%"
		(- total-bytes reported-bytes)
		(- total-objects reported-objects))
	(return))
      (incf reported-bytes bytes)
      (incf reported-objects objects)
      (format t "  ~13:D bytes for ~9:D ~(~A~) object~2:*~P.~%"
	      bytes objects name))
    (format t "  ~13:D bytes for ~9:D ~(~A~) object~2:*~P (space total.)~%"
	    total-bytes total-objects (car space-total))))


;;; MEMORY-USAGE  --  Public
;;;
(defun memory-usage (&key print-spaces (count-spaces '(:dynamic))
			  (print-summary t) cutoff)
  "Print out information about the heap memory in use.  :Print-Spaces is a list
  of the spaces to print detailed information for.  :Count-Spaces is a list of
  the spaces to scan.  For either one, T means all spaces (:Static, :Dyanmic
  and :Read-Only.)  If :Print-Summary is true, then summary information will be
  printed.  The defaults print only summary information for dynamic space.
  If true, Cutoff is a fraction of the usage in a report below which types will
  be combined as OTHER."
  (declare (type (or single-float null) cutoff))
  (let* ((spaces (if (eq count-spaces t)
		     '(:static :dynamic :read-only)
		     count-spaces))
	 (totals (mapcar #'(lambda (space)
			     (cons space (type-breakdown space)))
			 spaces)))

    (dolist (space-total totals)
      (when (or (eq print-spaces t)
		(member (car space-total) print-spaces))
	(report-space-total space-total cutoff)))

    (when print-summary (print-summary spaces totals)))

  (values))


;;; COUNT-NO-OPS  --  Public
;;;
(defun count-no-ops (space)
  "Print info about how much code and no-ops there are in Space."
  (declare (type spaces space))
  (let ((code-words 0)
	(no-ops 0)
	(total-bytes 0))
    (declare (fixnum code-words no-ops)
	     (type unsigned-byte total-bytes))
    (map-allocated-objects
     #'(lambda (obj type size)
 	 (declare (fixnum size) (optimize (safety 0)))
	 (when (eql type code-header-type)
	   (incf total-bytes size)
	   (let ((words (truly-the fixnum (%code-code-size obj)))
		 (sap (truly-the system-area-pointer
				 (%primitive code-instructions obj))))
	     (incf code-words words)
	     (dotimes (i words)
	       (when (zerop (sap-ref-32 sap (* i vm:word-bytes)))
		 (incf no-ops))))))
     space)
    
    (format t
	    "~:D code-object bytes, ~:D code words, with ~:D no-ops (~D%).~%"
	    total-bytes code-words no-ops
	    (round (* no-ops 100) code-words)))
  
  (values))


;;; DESCRIPTOR-VS-NON-DESCRIPTOR-STORAGE  --  Public
;;;
(defun descriptor-vs-non-descriptor-storage (&rest spaces)
  (let ((descriptor-words 0)
	(non-descriptor-headers 0)
	(non-descriptor-bytes 0))
    (declare (type unsigned-byte descriptor-words non-descriptor-headers
		   non-descriptor-bytes))
    (dolist (space (or spaces '(:read-only :static :dynamic)))
      (declare (inline map-allocated-objects))
      (map-allocated-objects
       #'(lambda (obj type size)
	   (declare (fixnum size) (optimize (safety 0)))
	   (case type
	     (#.code-header-type
	      (let ((inst-words (truly-the fixnum (%code-code-size obj))))
		(declare (type fixnum inst-words))
		(incf non-descriptor-bytes (* inst-words word-bytes))
		(incf descriptor-words
		      (- (truncate size word-bytes) inst-words))))
	     ((#.bignum-type
	       #.single-float-type
	       #.double-float-type
	       #+double-double
	       #.double-double-float-type
	       #.complex-single-float-type
	       #.complex-double-float-type
	       #+double-double
	       #.complex-double-double-float-type
	       #.simple-string-type
	       #.simple-bit-vector-type
	       #.simple-array-unsigned-byte-2-type
	       #.simple-array-unsigned-byte-4-type
	       #.simple-array-unsigned-byte-8-type
	       #.simple-array-unsigned-byte-16-type
	       #.simple-array-unsigned-byte-32-type
	       #.simple-array-signed-byte-8-type
	       #.simple-array-signed-byte-16-type
	       #.simple-array-signed-byte-30-type
	       #.simple-array-signed-byte-32-type
	       #.simple-array-single-float-type
	       #.simple-array-double-float-type
	       #+double-double
	       #.simple-array-double-double-float-type
	       #.simple-array-complex-single-float-type
	       #.simple-array-complex-double-float-type
	       #+double-double
	       #.simple-array-complex-double-double-float-type)
	      (incf non-descriptor-headers)
	      (incf non-descriptor-bytes (- size word-bytes)))
	     ((#.list-pointer-type
	       #.instance-pointer-type
	       #.ratio-type
	       #.complex-type
	       #.simple-array-type
	       #.simple-vector-type
	       #.complex-string-type
	       #.complex-bit-vector-type
	       #.complex-vector-type
	       #.complex-array-type
	       #.closure-header-type
	       #.funcallable-instance-header-type
	       #.value-cell-header-type
	       #.symbol-header-type
	       #.sap-type
	       #.weak-pointer-type
	       #.instance-header-type
	       #.fdefn-type
	       #+gencgc
	       #.scavenger-hook-type)
	      (incf descriptor-words (truncate size word-bytes)))
	     (t
	      (error "Bogus type: ~D" type))))
       space))
    (format t "~:D words allocated for descriptor objects.~%"
	    descriptor-words)
    (format t "~:D bytes data/~:D words header for non-descriptor objects.~%"
	    non-descriptor-bytes non-descriptor-headers)
    (values)))


;;; INSTANCE-USAGE  --  Public
;;;
(defun instance-usage (space &key (top-n 15))
  (declare (type spaces space) (type (or fixnum null) top-n))
  "Print a breakdown by instance type of all the instances allocated in
  Space.  If TOP-N is true, print only information for the the TOP-N types with
  largest usage."
  (format t "~2&~@[Top ~D ~]~(~A~) instance types:~%" top-n space)
  (let ((totals (make-hash-table :test #'eq))
	(total-objects 0)
	(total-bytes 0))
    (declare (fixnum total-objects)
	     (type memory-size total-bytes))
    (map-allocated-objects
     #'(lambda (obj type size)
	 (declare (type memory-size size) (optimize (speed 3) (safety 0)))
	 (when (eql type instance-header-type)
	   (incf total-objects)
	   (incf total-bytes size)
	   (let* ((class (layout-class (%instance-ref obj 0)))
		  (found (gethash class totals)))
	     (cond (found
		    (incf (the fixnum (car found)))
		    (incf (the fixnum (cdr found)) size))
		   (t
		    (setf (gethash class totals) (cons 1 size)))))))
     space)

    (collect ((totals-list))
      (maphash #'(lambda (class what)
		   (totals-list (cons (prin1-to-string
				       (class-proper-name class))
				      what)))
	       totals)
      (let ((sorted (sort (totals-list) #'> :key #'cddr))
	    (printed-bytes 0)
	    (printed-objects 0))
	(declare (type memory-size printed-bytes printed-objects))
	(dolist (what (if top-n
			  (subseq sorted 0 (min (length sorted) top-n))
			  sorted))
	  (let ((bytes (cddr what))
		(objects (cadr what)))
	    (incf printed-bytes bytes)
	    (incf printed-objects objects)
	    (format t "  ~32A: ~7:D bytes, ~5D object~:P.~%" (car what)
		    bytes objects)))

	(let ((residual-objects (- total-objects printed-objects))
	      (residual-bytes (- total-bytes printed-bytes)))
	  (unless (zerop residual-objects)
	    (format t "  Other types: ~:D bytes, ~D: object~:P.~%"
		    residual-bytes residual-objects))))

      (format t "  ~:(~A~) instance total: ~:D bytes, ~:D object~:P.~%"
	      space total-bytes total-objects)))

  (values))


;;; FIND-HOLES -- Public
;;; 
(defun find-holes (&rest spaces)
  (dolist (space (or spaces '(:read-only :static :dynamic)))
    (format t "In ~A space:~%" space)
    (let ((start-addr nil)
	  (total-bytes 0))
      (declare (type (or null (unsigned-byte 32)) start-addr)
	       (type (unsigned-byte 32) total-bytes))
      (map-allocated-objects
       #'(lambda (object typecode bytes)
	   (declare (ignore typecode)
		    (type (unsigned-byte 32) bytes))
	   (if (and (consp object)
		    (eql (car object) 0)
		    (eql (cdr object) 0))
	       (if start-addr
		   (incf total-bytes bytes)
		   (setf start-addr (di::get-lisp-obj-address object)
			 total-bytes bytes))
	       (when start-addr
		 (format t "~D bytes at #x~X~%" total-bytes start-addr)
		 (setf start-addr nil))))
       space)
      (when start-addr
	(format t "~D bytes at #x~X~%" total-bytes start-addr))))
  (values))


;;; Print allocated objects:

(defun print-allocated-objects (space &key (percent 0) (pages 5)
				      type larger smaller count
				      (stream *standard-output*))
  (declare (type (integer 0 99) percent) (type c::index pages)
	   (type stream stream) (type spaces space)
	   (type (or c::index null) type larger smaller count))
  (multiple-value-bind (start-sap end-sap)
		       (space-bounds space)
    (let* ((space-start (sap-int start-sap))
	   (space-end (sap-int end-sap))
	   (space-size (- space-end space-start))
	   (pagesize (system:get-page-size))
	   (start (+ space-start (round (* space-size percent) 100)))
	   (printed-conses (make-hash-table :test #'eq))
	   (pages-so-far 0)
	   (count-so-far 0)
	   (last-page 0))
      (declare (type (unsigned-byte 32) last-page start)
	       (fixnum pages-so-far count-so-far pagesize))
      (labels ((note-conses (x)
		 (unless (or (atom x) (gethash x printed-conses))
		   (setf (gethash x printed-conses) t)
		   (note-conses (car x))
		   (note-conses (cdr x)))))
	(map-allocated-objects
	 #'(lambda (obj obj-type size)
	     (declare (optimize (safety 0)))
	     (let ((addr (get-lisp-obj-address obj)))
	       (when (>= addr start)
		 (when (if count
			   (> count-so-far count)
			   (> pages-so-far pages))
		   (return-from print-allocated-objects (values)))
		 
		 (unless count
		   (let ((this-page (* (the (values (unsigned-byte 32) t)
					    (truncate addr pagesize))
				       pagesize)))
		     (declare (type (unsigned-byte 32) this-page))
		     (when (/= this-page last-page)
		       (when (< pages-so-far pages)
			 (format stream "~2&**** Page ~D, address ~X:~%"
				 pages-so-far addr))
		       (setq last-page this-page)
		       (incf pages-so-far))))
		 
		 (when (and (or (not type) (eql obj-type type))
			    (or (not smaller) (<= size smaller))
			    (or (not larger) (>= size larger)))
		   (incf count-so-far)
		   (case type
		     (#.code-header-type
		      (let ((dinfo (%code-debug-info obj)))
			(format stream "~&Code object: ~S~%"
				(if dinfo
				    (c::compiled-debug-info-name dinfo)
				    "No debug info."))))
		     (#.symbol-header-type
		      (format stream "~&~S~%" obj))
		     (#.list-pointer-type
		      (unless (gethash obj printed-conses)
			(note-conses obj)
			(let ((*print-circle* t)
			      (*print-level* 5)
			      (*print-length* 10))
			  (format stream "~&~S~%" obj))))
		     (t
		      (fresh-line stream)
		      (let ((str (write-to-string obj :level 5 :length 10
						  :pretty nil)))
			(unless (eql type instance-header-type)
			  (format stream "~S: " (type-of obj)))
			(format stream "~A~%"
				(subseq str 0 (min (length str) 60))))))))))
	 space))))
  (values))


;;;; LIST-ALLOCATED-OBJECTS, LIST-REFERENCING-OBJECTS

(defvar *ignore-after* nil)

(defun maybe-cons (space x stuff)
  (if (or (not (eq space :dynamic))
	  (< (get-lisp-obj-address x) (get-lisp-obj-address *ignore-after*)))
      (cons x stuff)
      stuff))

(defun list-allocated-objects (space &key type larger smaller count
				     test)
  (declare (type spaces space)
	   (type (or c::index null) larger smaller type count)
	   (type (or function null) test)
	   (inline map-allocated-objects))
  (unless *ignore-after* (setq *ignore-after* (cons 1 2)))
  (collect ((counted 0 1+))
    (let ((res ()))
      (map-allocated-objects
       #'(lambda (obj obj-type size)
	   (declare (optimize (safety 0)))
	   (when (and (or (not type) (eql obj-type type))
		      (or (not smaller) (<= size smaller))
		      (or (not larger) (>= size larger))
		      (or (not test) (funcall test obj)))
	     (setq res (maybe-cons space obj res))
	     (when (and count (>= (counted) count))
	       (return-from list-allocated-objects res))))
       space)
      res)))

(defun list-referencing-objects (space object)
  (declare (type spaces space) (inline map-allocated-objects))
  (unless *ignore-after* (setq *ignore-after* (cons 1 2)))
  (let ((res ()))
    (flet ((res (x)
	     (setq res (maybe-cons space x res))))
      (map-allocated-objects
       #'(lambda (obj obj-type size)
	   (declare (optimize (safety 0)) (ignore obj-type size))
	   (typecase obj
	     (cons
	      (when (or (eq (car obj) object) (eq (cdr obj) object))
		(res obj)))
	     (instance
	      (dotimes (i (%instance-length obj))
		(when (eq (%instance-ref obj i) object)
		  (res obj)
		  (return))))
	     (simple-vector
	      (dotimes (i (length obj))
		(when (eq (svref obj i) object)
		  (res obj)
		  (return))))
	     (symbol
	      (when (or (eq (symbol-name obj) object)
			(eq (symbol-package obj) object)
			(eq (symbol-plist obj) object)
			(eq (symbol-value obj) object))
		(res obj)))))
       space))
    res))


;;;; Misc:

(defun uninterned-symbol-count (space)
  (declare (type spaces space))
  (let ((total 0)
	(uninterned 0))
    (map-allocated-objects
     #'(lambda (obj type size)
	 (declare (ignore type size))
	 (when (symbolp obj)
	   (incf total)
	   (unless (symbol-package obj)
	     (incf uninterned))))
     space)
    (values uninterned (float (/ uninterned total)))))


(defun code-breakdown (space &key (how :package))
  (declare (type spaces space) (type (member :file :package) how))
  (let ((packages (make-hash-table :test #'equal)))
    (map-allocated-objects
     #'(lambda (obj type size)
	 (when (eql type code-header-type)
	   (let* ((dinfo (let ((x (%code-debug-info obj)))
			   (when (typep x 'c::debug-info) x)))
		  (package (if (typep dinfo 'c::compiled-debug-info)
			       (c::compiled-debug-info-package dinfo)
			       "UNKNOWN"))
		  (pkg-info (or (gethash package packages)
				(setf (gethash package packages)
				      (make-hash-table :test #'equal))))
		  (file
		   (if dinfo
		       (let ((src (c::debug-info-source dinfo)))
			 (cond (src
				(let ((source
				       (first
					(c::debug-info-source
					 dinfo))))
				  (if (eq (c::debug-source-from source)
					  :file)
				      (c::debug-source-name source)
				      "FROM LISP")))
			       (t
				(warn "No source for ~S" obj)
				"NO SOURCE")))
		       "UNKNOWN"))
		  (file-info (or (gethash file pkg-info)
				 (setf (gethash file pkg-info)
				       (cons 0 0)))))
	     (incf (car file-info))
	     (incf (cdr file-info) size))))
     space)

    (let ((res ()))
      (do-hash (pkg pkg-info packages)
	(let ((pkg-res ())
	      (pkg-count 0)
	      (pkg-size 0))
	  (do-hash (file file-info pkg-info)
	    (incf pkg-count (car file-info))
	    (incf pkg-size (cdr file-info))
	    (push (list file file-info) pkg-res))
	  (push (cons pkg-count pkg-size) pkg-res)
	  (push pkg pkg-res)
	  (push pkg-res res)))
	    
      (loop for (pkg (pkg-count . pkg-size) . files) in
	    (sort res #'> :key #'(lambda (x) (cdr (second x)))) do
	(format t "~%Package ~A: ~32T~9:D bytes, ~9:D object~:P.~%"
		pkg pkg-size pkg-count)
	(when (eq how :file)
	  (loop for (file (file-count . file-size)) in
	        (sort files #'> :key #'(lambda (x) (cdr (second x)))) do
	    (format t "~30@A: ~9:D bytes, ~9:D object~:P.~%"
		    (file-namestring file) file-size file-count))))))

  (values))


;;;; Histogram interface.  Uses Scott's Hist package.
#+nil
(defun memory-histogram (space &key (low 4) (high 20)
			       (bucket-size 1)
			       (function
				#'(lambda (obj type size)
				    (declare (ignore obj type) (fixnum size))
				    (integer-length (1- size))))
			       (type nil))
  (let ((function (if (eval:interpreted-function-p function)
		      (compile nil function)
		      function)))
    (hist:hist (low high bucket-size)
      (map-allocated-objects
       #'(lambda (obj this-type size)
	   (when (or (not type) (eql this-type type))
	     (hist:hist-record (funcall function obj type size))))
       space)))
  (values))

;;; Return the number of fbound constants in a code object.
;;;
(defun code-object-calls (obj)
  (loop for i from code-constants-offset below (get-header-data obj)
    count (find-code-object (code-header-ref obj i))))

;;; Return the number of calls in Obj to functions with <= N calls.  Calls is
;;; an eq hashtable translating code objects to the number of references.
;;;
(defun code-object-leaf-calls (obj n calls)
  (loop for i from code-constants-offset below (get-header-data obj)
    count (let ((code (find-code-object (code-header-ref obj i))))
	    (and code (<= (gethash code calls 0) n)))))

#+nil
(defun report-histogram (table &key (low 1) (high 20) (bucket-size 1)
			       (function #'identity))
  "Given a hashtable, print a histogram of the contents.  Function should give
  the value to plot when applied to the hashtable values."
  (let ((function (if (eval:interpreted-function-p function)
		      (compile nil function)
		      function)))
    (hist:hist (low high bucket-size)
      (loop for count being each hash-value in table do
	(hist:hist-record (funcall function count))))))

(defun report-top-n (table &key (top-n 20) (function #'identity))
  "Report the Top-N entries in the hashtable Table, when sorted by Function
  applied to the hash value.  If Top-N is NIL, report all entries."
  (let ((function (if (eval:interpreted-function-p function)
		      (compile nil function)
		      function)))
    (collect ((totals-list)
	      (total-val 0 +))
      (maphash #'(lambda (name what)
		   (let ((val (funcall function what)))
		     (totals-list (cons name val))
		     (total-val val)))
	       table)
      (let ((sorted (sort (totals-list) #'> :key #'cdr))
	    (printed 0))
	(declare (fixnum printed))
	(dolist (what (if top-n
			  (subseq sorted 0 (min (length sorted) top-n))
			  sorted))
	  (let ((val (cdr what)))
	    (incf printed val)
	    (format t "~8:D: ~S~%" val (car what))))

	(let ((residual (- (total-val) printed)))
	  (unless (zerop residual)
	    (format t "~8:D: Other~%" residual))))

      (format t "~8:D: Total~%" (total-val))))
  (values))


;;; Given any Lisp object, return the associated code object, or NIL.
;;;
(defun find-code-object (const)
  (flet ((frob (def)
	   (function-code-header
	    (ecase (get-type def)
	      ((#.closure-header-type
		#.funcallable-instance-header-type)
	       (%closure-function def))
	      (#.function-header-type
	       def)))))
    (typecase const
      (function (frob const))
      (symbol
       (if (fboundp const)
	   (frob (symbol-function const))
	   nil))
      (t nil))))
	

(defun find-caller-counts (space)
  "Return a hashtable mapping each function in for which a call appears in
  Space to the number of times such a call appears."
  (let ((counts (make-hash-table :test #'eq)))
    (map-allocated-objects
     #'(lambda (obj type size)
	 (declare (ignore size))
	 (when (eql type code-header-type)
	   (loop for i from code-constants-offset below (get-header-data obj)
	     do (let ((code (find-code-object (code-header-ref obj i))))
		  (when code
		    (incf (gethash code counts 0)))))))
       space)
    counts))

(defun find-high-callers (space &key (above 10) table (threshold 2))
  "Return a hashtable translating code objects to function constant counts for
  all code objects in Space with more than Above function constants."
  (let ((counts (make-hash-table :test #'eq)))
    (map-allocated-objects
     #'(lambda (obj type size)
	 (declare (ignore size))
	 (when (eql type code-header-type)
	   (let ((count (if table
			    (code-object-leaf-calls obj threshold table)
			    (code-object-calls obj))))
	     (when (> count above)
	       (setf (gethash obj counts) count)))))
     space)
    counts))
