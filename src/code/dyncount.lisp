;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/dyncount.lisp,v 1.6 2003/06/10 16:52:36 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Runtime support for dynamic VOP statistics collection.
;;; 
(in-package "C")

#|
Put *count-adjustments* back into VOP costs, and verify them.
Make sure multi-cycle instruction costs are plausible.
VOP classification.
  Make tables of %cost for benchmark X class.
  Could be represented as a sort of bar chart.
|#

(eval-when (compile)
  (when *collect-dynamic-statistics*
    (error "Compiling this file with dynamic stat collection turn on would ~
    be a very bad idea.")))

;;;; Hash utilities:

(defun make-hash-table-like (table)
  "Make a hash-table with the same test as table."
  (declare (type hash-table table))
  (make-hash-table :test (lisp::hash-table-kind table)))

(defun hash-difference (table1 table2)
  "Return a hash-table containing only the entries in Table1 whose key is not
   also a key in Table2." (declare (type hash-table table1 table2))
  (let ((res (make-hash-table-like table1)))
    (do-hash (k v table1)
      (unless (nth-value 1 (gethash k table2))
	(setf (gethash k res) v)))
    res))

(defun hash-list (table)
  "Return a list of the values in Table."
  (declare (type hash-table table))
  (collect ((res))
    (do-hash (k v table)
      (declare (ignore k))
      (res v))
    (res)))

;;; READ-HASH-TABLE, WRITE-HASH-TABLE  --  Public
;;;
;;;    Read (or write) a hashtable from (or to) a file.
;;;
(defun read-hash-table (file)
  (with-open-file (s file :direction :input)
    (dotimes (i 3)
      (format t "~%; ~A" (read-line s)))
    (let* ((eof '(nil))
	   (test (read s))
	   (reader (read s))
	   (res (make-hash-table :test test)))
      (read s); Discard writer...
      (loop
	(let ((key (read s nil eof)))
	  (when (eq key eof) (return))
	  (setf (gethash key res)
		(funcall reader s key))))
      res)))
;;;
(defun write-hash-table (table file &key
			       (comment (format nil "Contents of ~S" table))
			       (reader 'read) (writer 'prin1) (test 'equal))
  (with-open-file (s file :direction :output :if-exists :rename-and-delete)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
	(format s "~A~%Version ~A on ~A~%"
		comment (lisp-implementation-version)
		(machine-instance))
	(format-universal-time s (get-universal-time))
	(terpri s)
	(format s "~S ~S ~S~%" test reader writer)
	(do-hash (k v table)
	  (prin1 k s)
	  (write-char #\space s)
	  (funcall writer v s)
	  (terpri s)))))
  table)


;;;; Info accumulation:

;;; Used to accumulate info about the usage of a single VOP.  Cost and count
;;; are kept as double-floats, which lets us get more bits and avoid annoying
;;; overflows.
;;;
(deftype count-vector () '(simple-array double-float (2)))
;;;
(defstruct (vop-stats
	    (:constructor %make-vop-stats (name))
	    (:constructor make-vop-stats-key))
  (name (required-argument) :type simple-string)
  (data (make-array 2 :element-type 'double-float) :type count-vector))

(defmacro vop-stats-count (x) `(aref (vop-stats-data ,x) 0))
(defmacro vop-stats-cost (x) `(aref (vop-stats-data ,x) 1))

(defun make-vop-stats (&key name count cost)
  (let ((res (%make-vop-stats name)))
    (setf (vop-stats-count res) count)
    (setf (vop-stats-cost res) cost)
    res))
    
(declaim (freeze-type dyncount-info vop-stats))


;;; NOTE-DYNCOUNT-INFO  --  Internal
;;;
;;;    Add the Info into the cumulative result on the VOP name plist.  We use
;;; plists so that we will touch minimal system code outside of this file
;;; (which may be compiled with profiling on.)
;;;
(defun note-dyncount-info (info)
  (declare (type dyncount-info info) (inline get %put)
	   (optimize (speed 2)))
  (let ((counts (dyncount-info-counts info))
	(vops (dyncount-info-vops info)))
    (dotimes (index (length counts))
      (declare (type index index))
      (let ((count (coerce (the (unsigned-byte 31)
				(aref counts index))
			   'double-float)))
	(when (minusp count)
	  (warn "Oops: overflow.")
	  (return-from note-dyncount-info nil))
	(unless (zerop count)
	  (let* ((vop-info (svref vops index))
		 (length (length vop-info)))
	    (declare (simple-vector vop-info))
	    (do ((i 0 (+ i 4)))
		((>= i length))
	      (declare (type index i))
	      (let* ((name (svref vop-info i))
		     (entry (or (get name 'vop-stats)
				(setf (get name 'vop-stats)
				      (%make-vop-stats (symbol-name name))))))
		(incf (vop-stats-count entry)
		      (* (coerce (the index (svref vop-info (1+ i)))
				 'double-float)
			 count))
		(incf (vop-stats-cost entry)
		      (* (coerce (the index (svref vop-info (+ i 2)))
				 'double-float)
			 count))))))))))

(defun clear-dyncount-info (info)
  (declare (type dyncount-info info))
  (declare (optimize (speed 3) (safety 0)))
  (let ((counts (dyncount-info-counts info)))
    (dotimes (i (length counts))
      (setf (aref counts i) 0))))


;;; CLEAR-VOP-COUNTS  --  Public
;;;
;;;    Clear any VOP-COUNTS properties and the counts vectors for all code
;;; objects.  The latter loop must not call any random functions.
;;;
(defun clear-vop-counts (&optional (spaces '(:dynamic)))
  "Clear all dynamic VOP counts for code objects in the specified spaces."
  (do-hash (k v (backend-template-names *backend*))
    (declare (ignore v))
    (remprop k 'vop-stats))
  
  (locally
      (declare (optimize (speed 3) (safety 0))
	       (inline vm::map-allocated-objects))
    (without-gcing
      (dolist (space spaces)
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (dyncount-info-p object)
	       (clear-dyncount-info object)))
	 space)))))


;;; GET-VOP-COUNTS  --  Public
;;;
;;;    Call NOTE-DYNCOUNT-INFO on all DYNCOUNT-INFO structure allocated in the
;;; specified spaces.  Return a hashtable describing the counts.  The initial
;;; loop must avoid calling any functions outside this file to prevent adding
;;; noise to the data, since other files may be compiled with profiling.
;;;
(defun get-vop-counts (&optional (spaces '(:dynamic)) &key (clear nil))
  "Return a hash-table mapping string VOP names to VOP-STATS structures
   describing the VOPs executed.  If clear is true, then reset all counts to
   zero as a side-effect."
  (locally
      (declare (optimize (speed 3) (safety 0))
	       (inline vm::map-allocated-objects))
    (without-gcing
      (dolist (space spaces)
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (dyncount-info-p object)
	       (note-dyncount-info object)
	       (when clear
		 (clear-dyncount-info object))))
	 space))))
  
  (let ((counts (make-hash-table :test #'equal)))
    (do-hash (k v (backend-template-names *backend*))
      (declare (ignore v))
      (let ((stats (get k 'vop-stats)))
	(when stats
	  (setf (gethash (symbol-name k) counts) stats)
	  (when clear
	    (remprop k 'vop-stats)))))
    counts))


;;; FIND-INFO-FOR  --  Interface
;;;
;;;    Return the DYNCOUNT-INFO for FUNCTION.
;;;
(defun find-info-for (function)
  (declare (type function function))
  (let* ((function (%primitive closure-function function))
	 (component (di::function-code-header function)))
    (do ((end (get-header-data component))
	 (i vm:code-constants-offset (1+ i)))
	((= end i))
      (let ((constant (code-header-ref component i)))
	(when (dyncount-info-p constant)
	  (return constant))))))


(defun vop-counts-apply (function args &key (spaces '(:dynamic)) by-space)
  "Apply Function to Args, collecting dynamic statistics on the running.
   Spaces are the spaces to scan for counts.  If By-Space is true, we return a
   list of result tables, instead of a single table.  In this case, specify
   :READ-ONLY first."
  (clear-vop-counts spaces)
  (apply function args)
  (if by-space
      (mapcar #'(lambda (space)
		  (get-vop-counts (list space) :clear t))
	      spaces)
      (get-vop-counts spaces)))

;;;; Adjustments:

#|
(defparameter *count-adjustments*
  '((return-multiple 152)
    (tail-call-variable 88)
    (unwind 92)
    (throw 116)
    (allocate-vector 72)
    (sxhash-simple-string 248)
    (sxhash-simple-substring 264)
    (copy-to-system-area 1200)
    (copy-from-system-area 1200)
    (system-area-copy 1204)
    (bit-bash-copy 1412)
    (vm::generic-+ 72)
    (vm::generic-- 72)
    (vm::generic-* 184)
    (vm::generic-< 68)
    (vm::generic-> 68)
    (vm::generic-eql 80)
    (vm::generic-= 80)
    (vm::generic-/= 104)
    (%make-weak-pointer 60)
    (make-value-cell 56)
    (vm::make-funcallable-instance 76)
    (make-closure 76)
    (make-complex 60)
    (make-ratio 60)
    (%allocate-bignum 72)
    (make-structure 72)
    (cons 50)))
|#

;;; GET-VOP-COSTS  --  Public
;;;
(defun get-vop-costs ()
  "Return a hash-table mapping string VOP names to the cost recorded in the
   generator for all VOPs which are also the names of assembly routines."
  (let ((res (make-hash-table :test #'equal)))
     (do-hash (name v lisp::*assembler-routines*)
       (declare (ignore v))
       (let ((vop (gethash name (backend-template-names *backend*))))
	 (when vop
	   (setf (gethash (symbol-name name) res)
		 (template-cost (template-or-lose name))))))
    res))

(defvar *native-costs* (get-vop-costs)
  "Costs of assember routines on this machine.")


;;;; Classification:

(defparameter *basic-classes*
  '(("Integer multiplication"
     "*/FIXNUM" "*/SIGNED" "*/UNSIGNED" "SIGNED-*" "FIXNUM-*" "GENERIC-*")
    ("Integer division" "TRUNCATE")
    ("Generic arithmetic" "GENERIC" "TWO-ARG")
    ("Inline EQL" "EQL")
    ("Inline compare less/greater" "</" ">/" "<-C/" ">-C/")
    ("Inline arith" "*/" "//" "+/" "-/" "NEGATE" "ABS" "+-C" "--C")
    ("Inline logic" "-ASH" "$ASH" "LOG")
    ("CAR/CDR" "CAR" "CDR")
    ("Array type test" "ARRAYP" "VECTORP" "ARRAY-HEADER-P")
    ("Simple type predicate" "STRUCTUREP" "LISTP" "FIXNUMP")
    ("Simple type check" "CHECK-LIST" "CHECK-FIXNUM" "CHECK-STRUCTURE")
    ("Array bounds check" "CHECK-BOUND")
    ("Complex type check" "$CHECK-" "COERCE-TO-FUNCTION")
    ("Special read" "SYMBOL-VALUE")
    ("Special bind" "BIND$")
    ("Tagging" "MOVE-FROM")
    ("Untagging" "MOVE-TO" "MAKE-FIXNUM")
    ("Move" "MOVE")
    ("Non-local exit" "CATCH" "THROW" "DYNAMIC-STATE" "NLX" "UNWIND")
    ("Array write" "DATA-VECTOR-SET" "$SET-RAW-BITS$")
    ("Array read" "DATA-VECTOR-REF" "$RAW-BITS$" "VECTOR-LENGTH"
     "LENGTH/SIMPLE" "ARRAY-HEADER")
    ("List/string utility" "LENGTH/LIST" "SXHASH" "BIT-BASH" "$LENGTH$")
    ("Alien operations" "SAP" "ALLOC-NUMBER-STACK" "$CALL-OUT$")
    ("Function call/return" "CALL" "RETURN" "ALLOCATE-FRAME"
     "COPY-MORE-ARG" "LISTIFY-REST-ARG" "VERIFY-ARGUMENT-COUNT")
    ("Allocation" "MAKE-" "ALLOC" "$CONS$" "$LIST$" "$LIST*$")
    ("Float conversion" "%SINGLE-FLOAT" "%DOUBLE-FLOAT" "-BITS$")
    ("Complex type predicate" "P$")))


;;; MATCHES-PATTERN  --  Internal
;;;
;;;    Return true if Name patches a specified pattern.  Pattern is a string
;;; (or symbol) or a list of strings (or symbols).  If any specified string
;;; appears as a substring of name, the pattern is matched.  #\$'s are wapped
;;; around name, allowing the use of $ to force a match at the beginning or
;;; end.
;;;
(defun matches-pattern (name pattern)
  (declare (simple-string name))
  (let ((name (concatenate 'string "$" name "$")))
    (dolist (pat (if (listp pattern) pattern (list pattern)) nil)
      (when (search (the simple-string (string pat))
		    name :test #'char=)
	(return t)))))


;;; FIND-MATCHES, WHAT-CLASS  --  Interface
;;;
;;;    Utilities for debugging classification rules.  FIND-MATCHES returns a
;;; list of all the VOP names in Table that match Pattern.   WHAT-CLASS returns
;;; the class that NAME would be placed in.
;;;
(defun find-matches (table pattern)
  (collect ((res))
    (do-hash (key value table)
      (declare (ignore value))
      (when (matches-pattern key pattern) (res key)))
    (res)))
;;;
(defun what-class (name classes)
  (dolist (class classes nil)
    (when (matches-pattern name (rest class)) (return (first class)))))

    
;;; CLASSIFY-COSTS  --  Interface
;;;
;;;    Given a VOP-STATS hash-table, return a new one with VOPs in the same
;;; class merged into a single entry for that class.  The classes are
;;; represented as a list of lists: (class-name pattern*).  Each pattern is a
;;; string (or symbol) that can appear as a subsequence of the VOP name.  A VOP
;;; is placed in the first class that it matches, or is left alone if it
;;; matches no class.
;;;
(defun classify-costs (table classes)
  (let ((res (make-hash-table-like table)))
    (do-hash (key value table)
      (let ((class (dolist (class classes nil)
		     (when (matches-pattern key (rest class))
		       (return (first class))))))
	(if class
	    (let ((found (or (gethash class res)
			     (setf (gethash class res)
				   (%make-vop-stats class)))))
	      (incf (vop-stats-count found) (vop-stats-count value))
	      (incf (vop-stats-cost found) (vop-stats-cost value)))
	    (setf (gethash key res) value))))
    res))


;;;; Analysis:

;;; COST-SUMMARY  --  Internal
;;;
;;;    Sum the count and costs.
;;;
(defun cost-summary (table)
  (let ((total-count 0d0)
	(total-cost 0d0))
    (do-hash (k v table)
      (declare (ignore k))
      (incf total-count (vop-stats-count v))
      (incf total-cost (vop-stats-cost v)))
    (values total-count total-cost)))

  
;;; COMPENSATE-COSTS  --  Internal
;;;
;;;    Return a hashtable of DYNCOUNT-INFO structures, with cost adjustments
;;; according to the Costs table.  Any VOPs in the list IGNORE are ignored.
;;;
(defun compensate-costs (table costs &optional ignore)
  (let ((res (make-hash-table-like table)))
    (do-hash (key value table)
      (unless (or (string= key "COUNT-ME")
		  (member key ignore :test #'string=))
	(let ((cost (gethash key costs)))
	  (if cost
	      (let* ((count (vop-stats-count value))
		     (sum (+ (* cost count)
			     (vop-stats-cost value))))
		(setf (gethash key res)
		      (make-vop-stats :name key :count count :cost sum)))
	      (setf (gethash key res) value)))))
    res))


;;; COMPARE-STATS  --  Internal
;;;
;;;    Take two tables of vop-stats and return a table of entries where the
;;; entries have been compared.  The counts are normalized to Compared.  The
;;; costs are the difference of the costs adjusted by the difference in counts:
;;; the cost for Original is modified to correspond to the count in Compared.
;;;
(defun compare-stats (original compared)
  (declare (type hash-table original compared))
  (let ((res (make-hash-table-like original)))
    (do-hash (k cv compared)
      (let ((ov (gethash k original)))
	(when ov
	  (let ((norm-cnt (/ (vop-stats-count ov) (vop-stats-count cv))))
	    (setf (gethash k res)
		  (make-vop-stats
		   :name k
		   :count norm-cnt
		   :cost (- (/ (vop-stats-cost ov) norm-cnt)
			    (vop-stats-cost cv))))))))
    res))


;;; COMBINE-STATS  --  Public
;;;
(defun combine-stats (&rest tables)
  "Sum the VOP stats for the specified tables, returning a new table with the
   combined results."
  (let ((res (make-hash-table-like (first tables))))
    (dolist (table tables)
      (do-hash (k v table)
	(let ((found (or (gethash k res)
			 (setf (gethash k res) (%make-vop-stats k)))))
	  (incf (vop-stats-count found) (vop-stats-count v))
	  (incf (vop-stats-cost found) (vop-stats-cost v)))))
    res))


;;;; Report generation:

;;; SORT-RESULT  --  Internal
;;;
(defun sort-result (table by)
  (sort (hash-list table) #'>
	:key #'(lambda (x)
		 (abs (ecase by
			(:count (vop-stats-count x))
			(:cost (vop-stats-cost x)))))))


;;; ENTRY-REPORT  --  Internal
;;;
;;;    Report about VOPs in the list of stats structures.
;;;
(defun entry-report (entries cut-off compensated compare total-cost)
  (let ((counter (if (and cut-off (> (length entries) cut-off))
		     cut-off
		     most-positive-fixnum)))
  (dolist (entry entries)
    (let* ((cost (vop-stats-cost entry))
	   (name (vop-stats-name entry))
	   (entry-count (vop-stats-count entry))
	   (comp-entry (if compare (gethash name compare) entry))
	   (count (vop-stats-count comp-entry)))
      (format t "~30<~A~>: ~:[~13:D~;~13,2F~] ~9,2F  ~5,2,2F%~%"
	      (vop-stats-name entry)
	      compare
	      (if compare entry-count (round entry-count))
	      (/ cost count)
	      (/ (if compare
		     (- (vop-stats-cost (gethash name compensated))
			(vop-stats-cost comp-entry))
		     cost)
		 total-cost))
      (when (zerop (decf counter))
	(format t "[End of top ~D]~%" cut-off))))))

;;; FIND-CUT-OFF  --  Internal
;;;
;;;    Divide Sorted into two lists, the first cut-off elements long.  Any VOP
;;; names that match one of the report strings are moved into the report list
;;; even if they would otherwise fall below the cut-off.
;;;
(defun find-cut-off (sorted cut-off report)
  (if (or (not cut-off) (<= (length sorted) cut-off))
      (values sorted ())
      (let ((not-cut (subseq sorted 0 cut-off)))
	(collect ((select)
		  (reject))
	  (dolist (el (nthcdr cut-off sorted))
	    (let ((name (vop-stats-name el)))
	      (if (matches-pattern name report)
		  (select el)
		  (reject el))))
	  (values (append not-cut (select)) (reject))))))
		  

;;; CUT-OFF-REPORT  --  Internal
;;;
;;;    Display information about entries that were not displayed due to the
;;; cut-off.  Note: if compare, we find the total cost delta and the geometric
;; mean of the normalized counts.
;;
(defun cut-off-report (other compare total-cost)
  (let ((rest-cost 0d0)
	(rest-count 0d0)
	(rest-entry-count (if compare 1d0 0d0)))
    (dolist (entry other)
      (incf rest-cost (vop-stats-cost entry))
      (incf rest-count
	    (vop-stats-count
	     (if compare
		 (gethash (vop-stats-name entry) compare)
		 entry)))
      (if compare
	  (setq rest-entry-count
		(* rest-entry-count (vop-stats-count entry)))
	  (incf rest-entry-count (vop-stats-count entry))))
    
    (let ((count (if compare
		     (expt rest-entry-count
			   (/ (coerce (length other) 'double-float)))
		     (round rest-entry-count))))
      (format t "~30<Other~>: ~:[~13:D~;~13,2F~] ~9,2F  ~@[~5,2,2F%~]~%"
	      compare count
	      (/ rest-cost rest-count)
	      (unless compare
		(/ rest-cost total-cost))))))


;;; COMPARE-REPORT  --  Internal
;;;
;;;    Report summary information about the difference between the comparison
;;; and base data sets.
;;;
(defun compare-report (total-count total-cost compare-total-count
				   compare-total-cost compensated compare)
  (format t "~30<Relative total~>: ~13,2F ~9,2F~%"
	  (/ total-count compare-total-count)
	  (/ total-cost compare-total-cost))
  (flet ((frob (a b sign wot)
	   (multiple-value-bind
	       (cost count) (cost-summary (hash-difference a b))
	     (unless (zerop count)
	       (format t "~30<~A~>: ~13:D ~9,2F  ~5,2,2F%~%"
		       wot (* sign (round count))
		       (* sign (/ cost count))
		       (* sign (/ cost compare-total-cost)))))))
    (frob compensated compare 1 "Not in comparison")
    (frob compare compensated -1 "Only in comparison"))
  (format t "~30<Comparison total~>: ~13,2E ~9,2E~%"
	  compare-total-count compare-total-cost))


;;; The fraction of system time that we guess happened during GC.
;;;
(defparameter *gc-system-fraction* 2/3)

;;; FIND-CPI  --  Interface
;;;
;;;    Estimate CPI from CPU time and cycles accounted in profiling
;;; information.
;;;
(defun find-cpi (total-cost user system gc clock)
  (let ((adj-time (if (zerop gc)
		      user
		      (- user (- gc (* system *gc-system-fraction*))))))
    (/ (* adj-time clock) total-cost)))

  
;;; GENERATE-REPORT  --  Public
;;;
;;; Generate a report from the specified table.
;;;
(defun generate-report (table &key (cut-off 15) (sort-by :cost)
			      (costs *native-costs*)
			      ((:compare uncomp-compare))
			      (compare-costs costs)
			      ignore report
			      (classes *basic-classes*)
			      user (system 0d0) (gc 0d0)
			      (clock 25d6))
  (let* ((compensated
	  (classify-costs
	   (if costs
	       (compensate-costs table costs ignore)
	       table)
	   classes))
	 (compare
	  (when uncomp-compare
	    (classify-costs
	     (if compare-costs
		 (compensate-costs uncomp-compare compare-costs ignore)
		 uncomp-compare)
	     classes)))
	 (compared (if compare
		       (compare-stats compensated compare)
		       compensated))
	 (*gc-verbose* nil))
    (multiple-value-bind (total-count total-cost)
			 (cost-summary compensated)
      (multiple-value-bind (compare-total-count compare-total-cost)
			   (when compare (cost-summary compare))
	(format t "~2&~30<Vop~>  ~13<Count~> ~9<Cost~>  ~6:@<Percent~>~%")
	(let ((sorted (sort-result compared sort-by))
	      (base-total (if compare compare-total-cost total-cost)))
	  (multiple-value-bind
	      (report other)
	      (find-cut-off sorted cut-off report)
	    (entry-report report cut-off compensated compare base-total)
	    (when other
	      (cut-off-report other compare base-total))))

	(when compare
	  (compare-report total-count total-cost compare-total-count
			  compare-total-cost compensated compare))

	(format t "~30<Total~>: ~13,2E ~9,2E~%" total-count total-cost)
	(when user
	  (format t "~%Cycles per instruction = ~,2F~%"
		  (find-cpi total-cost user system gc clock))))))
  (values))


;;; STATS-{READER,WRITER}  --  Public
;;;
;;;    Read & write VOP stats using hash IO utility.
;;;
(defun stats-reader (stream key)
  (make-vop-stats :name key :count (read stream) :cost (read stream)))
;;;
(defun stats-writer (object stream)
  (format stream "~S ~S" (vop-stats-count object) (vop-stats-cost object)))
