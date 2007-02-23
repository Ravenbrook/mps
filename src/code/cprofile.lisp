;;; -*- Package: CPROFILE -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/cprofile.lisp,v 1.2 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains run-time support for collecting dynamic profiling
;;; information from code instrumented by the compiler.
;;; 
(defpackage "CPROFILE"
  (:use "C" "DI" "KERNEL" "EXTENSIONS" "LISP" "SYSTEM"))
(in-package "C")
(export '(dyncount-info dyncount-info-p) "C")
(in-package "CPROFILE")

(eval-when (compile)
  (when *collect-dynamic-statistics*
    (error "You don't want to compile this file with profiling.")))


;;; Represents a single high-cost code object we've pulled out of memory.
;;;
(defstruct (selection-elt
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (print-unreadable-object (s stream :type t :identity t)))))
  ;;
  ;; The code-object we're collecting info for.
  (code nil)
  ;;
  ;; The Dyncount-Info for this object (the first constant.)
  (info nil :type (or dyncount-info null))
  ;;
  ;; The total cost associated with this code object.
  (cost 0d0 :type double-float)

  ;;
  ;; Remaining slots only filled in when we have definitely selected this
  ;; object...
  ;;
  ;; A copy of the counts vector (so the results don't get modified.)
  (counts nil :type (or (simple-array (unsigned-byte 32) (*)) null))
  ;;
  ;; A list of lists ((debug-function cost first-index) for all the functions
  ;; in this object, where cost is the total cost for this function and
  ;; first-index is the index in costs/counts of the first block in this
  ;; function.
  (functions nil :type list))


;;; Represents a collection of the N most expensive code objects.
;;;
(defstruct (selection
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (print-unreadable-object (s stream :type t :identity t)
		 (format stream "~A, top ~D, cost ~S" (selection-name s)
			 (length (selection-elements s))
			 (selection-total-cost s))))))
  ;;
  ;; Some kind of descriptive name.
  (name (required-argument) :type string)
  ;;
  ;; Total cost of all code objects scanned.
  (total-cost 0d0 :type double-float)
  ;;
  ;; List of elements selected.
  (elements (list (make-selection-elt :cost -1d0)) :type list))


(defconstant count-me-cost-fudge 3d0
  "Our guess is to how many cycles in each block are really due to
   profiling.")

;;; SAVE-SELECT-RESULT  --  Internal
;;;
;;;    First copy all of the counts vectors, then find the debug-functions and
;;; costs.  This split avoids considering spurious costs related to parsing
;;; debug-info, etc.
;;;
(defun save-select-result (res)
  (dolist (elt res)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((info (selection-elt-info elt))
	   (counts (dyncount-info-counts info))
	   (len (length counts))
	   (new (make-array len :element-type '(unsigned-byte 32))))
      (dotimes (i len)
	(setf (aref new i) (aref counts i)))
      (setf (selection-elt-counts elt) new)))

  (dolist (elt res)
    (let* ((object (selection-elt-code elt))
	   (debug-info (code-header-ref object vm:code-debug-info-slot))
	   (map (di::get-debug-info-function-map debug-info))
	   (costs (dyncount-info-costs (selection-elt-info elt)))
	   (counts (selection-elt-counts elt))
	   (j 0)
	   (last-j 0))
      (declare (simple-vector map))
      (dotimes (i (length map))
	(declare (fixnum i))
	(let ((e (svref map i)))
	  (when (c::compiled-debug-function-p e)
	    (let ((fn (di::make-compiled-debug-function e object))
		  (sum 0d0))
	      (declare (double-float sum) (type index j))
	      (do-debug-function-blocks (blk fn)
		(unless (debug-block-elsewhere-p blk)
		  (incf sum
			(* (- (coerce (the (unsigned-byte 31)
					   (aref costs j))
				      'double-float)
			      count-me-cost-fudge)
			   (coerce (the (unsigned-byte 31)
					(aref counts j))
				   'double-float)))
		  (incf j)))
	      (push (list fn sum last-j)
		    (selection-elt-functions elt))
	      (setq last-j j)))))))
  (undefined-value))

;;; CLEAR-PROFILE-INFO  --  Public
;;;
(defun clear-profile-info (&optional (spaces '(:dynamic)))
  "Clear all profiling counts.  Call this before running your test."
  (declare (inline vm::map-allocated-objects)
	   (optimize (speed 3) (safety 0)))
  (without-gcing
    (dolist (space spaces)
      (vm::map-allocated-objects
       #'(lambda (object type-code size)
	   (declare (ignore type-code size))
	   (when (code-component-p object)
	     (let ((info (code-header-ref object vm:code-constants-offset)))
	       (when (dyncount-info-p info)
		 (clear-dyncount-info info)))))
       space)))
  (values))


;;; SELECT-PROFILE-RESULT  --  Public
;;;
(defun select-profile-result (&key (name "unknown") (n 30)
				   (spaces '(:dynamic)))
  (declare (string name) (list spaces) (type index n))
  (let ((res (loop repeat n collect (make-selection-elt :cost -1d0)))
	(min-cost -1d0)
	(selection (make-selection :name name)))
    (declare (type double-float min-cost)
	     (inline vm::map-allocated-objects)
	     (optimize (speed 3) (safety 0)))
    (without-gcing
      (dolist (space spaces)
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (code-component-p object)
	       (let ((info (code-header-ref object vm:code-constants-offset)))
		 (when (dyncount-info-p info)
		   (let ((costs (dyncount-info-costs info))
			 (counts (dyncount-info-counts info))
			 (this-cost 0d0))
		     (declare (double-float this-cost))
		     (dotimes (i (length counts))
		       (incf this-cost
			     (* (coerce (the (unsigned-byte 31)
					     (aref counts i))
					'double-float)
				(- (coerce (the (unsigned-byte 31)
						(aref costs i))
					   'double-float)
				   count-me-cost-fudge))))
		     (incf (selection-total-cost selection) this-cost)
		     (when (> this-cost min-cost)
		       (do ((prev res current)
			    (current (cdr res) (cdr current)))
			   ((or (null current)
				(> (selection-elt-cost (car current))
				   this-cost))
			    (setf (cdr prev)
				  (cons (make-selection-elt
					 :cost this-cost
					 :code object
					 :info info)
					current))))
		       (let ((old (pop res)))
			 (setq min-cost (selection-elt-cost old)))))))))
	 space)))

    (loop
      (unless res
	(error "No profilable code objects found."))
      (unless (minusp (selection-elt-cost (first res))) (return))
      (pop res))

    (save-select-result res)
    (setf (selection-elements selection) res)
    selection))

(defun function-report (selection &key (top-n 20))
  (let ((selected-funs ()))
    (dolist (elt (selection-elements selection))
      (let ((comp-funs ()))
	(dolist (fun (selection-elt-functions elt))
	  (let* ((name (debug-function-name (first fun)))
		 (found (assoc name comp-funs :test #'equal)))
	    (if found
		(incf (cdr found) (second fun))
		(push (cons name (second fun)) comp-funs))))
	(setq selected-funs (append comp-funs selected-funs))))

    (let ((sorted (sort selected-funs #'> :key #'cdr))
	  (total 0d0))
      (declare (double-float total))
      (loop for (name . cost) in sorted
	    repeat top-n do
	(format t "~,2E: ~S~%" cost name)
	(incf total cost))
      (format t "~,2E: Total~%" total)
      (format t "~,2E: Other~%" (- (selection-total-cost selection) total))))

  (values))


;;; FIND-INFO-FOR  --  Interface
;;;
;;;    Return the DYNCOUNT-INFO for FUNCTION.
;;;
(defun find-info-for (function)
  (declare (type function function))
  (let* ((function (%closure-function function))
	 (component (di::function-code-header function)))
    (do ((end (get-header-data component))
	 (i vm:code-constants-offset (1+ i)))
	((= end i))
      (let ((constant (code-header-ref component i)))
	(when (dyncount-info-p constant)
	  (return constant))))))

;;
(defun clear-dyncount-info (info)
  (declare (type dyncount-info info))
  (declare (optimize (speed 3) (safety 0)))
  (let ((counts (dyncount-info-counts info)))
    (dotimes (i (length counts))
      (setf (aref counts i) 0))))

;;
(defun dyncount-total-cost (info)
  (let ((costs (dyncount-info-costs info))
	(counts (dyncount-info-counts info))
	(sum 0))
    (dotimes (i (length costs))
      (incf sum (* (aref costs i) (aref counts i))))
    sum))


;;; SAME-SOURCE-P  --  Internal
;;;
;;;    Return true if two code locations have the same source form.
;;;
(defun same-source-p (x y)
  (and (= (code-location-form-number x)
	  (code-location-form-number y))
       (= (di:code-location-top-level-form-offset x)
	  (di:code-location-top-level-form-offset y))))


;;; FUNCTION-CYCLES -- External.
;;;
(defun function-cycles (name selection &key (top-n 15) (combine t))
  (declare (type selection selection))
  "Print detailed information about the costs associated with a particular
   function in a SELECTION of dynamic statistics.  All functions with names
   EQUAL to the specified name are reported.  If Combine is true, then
   all blocks with the same source location are combined into a single entry in
   the report."
  ;;
  ;; DFs = list of (selection-elt . (debug-fun cost first-num))
  ;; Locs = list of (code-location . (loc-cost max-count))
  (collect ((dfs)
	    (locs))
    (dolist (sel (selection-elements selection))
      (dolist (fun (selection-elt-functions sel))
	(when (equal (debug-function-name (first fun)) name)
	  (dfs (cons sel fun)))))
    (dolist (df (dfs))
      (let* ((num (third (cdr df)))
	     (sel (car df))
	     (counts (selection-elt-counts sel))
	     (costs (dyncount-info-costs (selection-elt-info sel))))
	(declare (type index num))
	(do-debug-function-blocks (db (first (cdr df)))
	  (do-debug-block-locations (loc db)
	    (let* ((cnt (coerce (aref counts num) 'double-float))
		   (cost (* (- (coerce (aref costs num) 'double-float)
			       count-me-cost-fudge)
			    cnt)))
	      (unless (debug-block-elsewhere-p db)
		(assert (member (code-location-kind loc)
				'(:block-start :non-local-entry)))
		(let ((found (and combine
				  (assoc loc (locs) :test #'same-source-p))))
		  (cond (found
			 (incf (second found) cost)
			 (setf (third found)
			       (max (the double-float (third found)) cnt)))
			(t
			 (locs (list loc cost cnt)))))
		(incf num))
	      (return))))))

    (let ((locs (stable-sort (locs) #'>= :key #'second)))
      (dolist (loc (subseq locs 0 (min (length locs) top-n)))
	(format t "~%~,2E cycles, ~[not run, ~;~:; ~:*~D repeats, ~]~
		   ~S:~%    "
		(second loc)
		(truncate (third loc))
		(debug-function-name (code-location-debug-function (car loc))))
	(debug::print-code-location-source-form (car loc) 0)
	(fresh-line))))
  (values))

#+nil
;;; GET-SPACE-LIST -- Internal.
;;;
;;;
(defun get-space-list (spaces clear)
  (locally
   (declare (optimize (speed 3) (safety 0))
	    (inline vm::map-allocated-objects))
   (without-gcing
    (let ((list nil))
      (dolist (space spaces)
	(vm::map-allocated-objects
	 #'(lambda (object type-code size)
	     (declare (ignore type-code size))
	     (when (kernel:code-component-p object)
	       (let ((info (kernel:code-header-ref object 5))
		     (j 0)
		     (sum 0)
		     (alist))
		 (declare (fixnum j))
		 (when (dyncount-info-p info)
		   (let* ((debug-info (kernel:code-header-ref object 3))
			  (map (di::get-debug-info-function-map debug-info)))
		     (declare (vector map))
		     (dotimes (i (length map))
		       (declare (fixnum i))
		       (let ((e (svref map i)))
			 (when (c::compiled-debug-function-p e)
			   (let ((fn (di::make-compiled-debug-function
				      e object)))
			     (di:do-debug-function-blocks (blk fn)
			       (unless (di:debug-block-elsewhere-p blk)
				 (incf sum
				       (* (aref (dyncount-info-costs info) j)
					  (aref (dyncount-info-counts info) j)))
				 (incf j)))
			     (let ((a (find (di:debug-function-name fn)
					    alist :key #'car)))
			       (cond (a (incf (third a) sum))
				     (t
				      (push (list (di:debug-function-name fn)
						  (c::compiled-debug-info-package
						   debug-info)
						   sum)
					    alist)))
			       (setf sum 0)))))))
		   (when clear
		     (clear-dyncount-info info)))
		 (dolist (e alist)
		   (push e list)))))
	 space))
      list))))

#+nil
;;; GET-STATS -- External.
;;;
;;; Returns the cycles of all the functions in the spaces.
;;;
(defun get-stats (&key (spaces '(:dynamic)) (clear nil) (top-n 10) (cost 0))
  (let ((list (stable-sort (sort (get-space-list spaces clear)
				 #'> :key #'third) #'string< :key #'second))
	(package-name "")
	(i 0)
	(other 0))
    (dolist (e list)
      (unless (string= package-name (second e))
	(setf package-name (second e))
	(when (> other cost)
	  (format t " ~10:D: Other~&" other))
	(setf i 0)
	(setf other 0)
	(when (> (third e) cost)
	  (format t "Package: ~A~&" package-name)))
      (cond ((< i top-n)
	     (when (> (third e) cost)
	       (format t " ~10:D: ~S~&" (third e) (first e))))
	    (t
	     (incf other (third e))))
      (incf i))
    (when (> other cost)
      (format t " ~10:D: Other~&" other))))
