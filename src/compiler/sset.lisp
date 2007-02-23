;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sset.lisp,v 1.7 2000/07/07 09:33:05 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;;    A sparse set abstraction, implemented as a sorted linked list.  We don't
;;; use bit-vectors to represent sets in flow analysis, since the universe may
;;; be quite large but the average number of elements is small.  We keep the
;;; list sorted so that we can do union and intersection in linear time.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

;;;
;;; Each structure that may be placed in a SSet must include the SSet-Element
;;; structure.  We allow an initial value of NIL to mean that no ordering has
;;; been assigned yet (although an ordering must be assigned before doing set
;;; operations.)
;;;
(defstruct sset-element
  (number nil :type (or index null)))


(defstruct (sset (:constructor make-sset ())
		 (:copier nil)
		 (:print-function %print-sset))
  (elements (list nil) :type list))


(defprinter sset
  (elements :prin1 (cdr elements)))


;;; Do-Elements  --  Interface
;;;
;;;    Iterate over the elements in Set, binding Var to each element in turn.
;;;
(defmacro do-elements ((var set &optional result) &body body)
  `(dolist (,var (cdr (sset-elements ,set)) ,result) ,@body))


;;; SSet-Adjoin  --  Interface
;;;
;;;    Destructively add Element to Set.  If Element was not in the set, then
;;; we return true, otherwise we return false.
;;;
(defun sset-adjoin (element set)
  (declare (type sset-element element) (type sset set) (values boolean))
  (let ((number (sset-element-number element))
	(elements (sset-elements set)))
    (do ((prev elements current)
	 (current (cdr elements) (cdr current)))
	((null current)
	 (setf (cdr prev) (list element))
	 t)
      (let ((el (car current)))
	(when (>= (sset-element-number el) number)
	  (when (eq el element)
	    (return nil))
	  (setf (cdr prev) (cons element current))
	  (return t))))))


;;; SSet-Delete  --  Interface
;;;
;;;    Destructively remove Element from Set.  If element was in the set,
;;; then return true, otherwise return false.
;;;
(defun sset-delete (element set)
  (declare (type sset-element element) (type sset set) (values boolean))
  (let ((elements (sset-elements set)))
    (do ((prev elements current)
	 (current (cdr elements) (cdr current)))
	((null current) nil)
      (when (eq (car current) element)
	(setf (cdr prev) (cdr current))
	(return t)))))


;;; SSet-Member  --  Interface
;;;
;;;    Return true if Element is in Set, false otherwise.
;;;
(defun sset-member (element set)
  (declare (type sset-element element) (type sset set) (values boolean)
	   (inline member))
  (not (null (member element (cdr (sset-elements set)) :test #'eq))))


;;; SSet-Empty  --  Interface
;;;
;;;    Return true if Set contains no elements, false otherwise.
;;;
(defun sset-empty (set)
  (declare (type sset set) (values boolean))
  (null (cdr (sset-elements set))))


;;; SSet-Singleton  --  Interface
;;;
;;;    If Set contains exactly one element, then return it, otherwise return
;;; NIL.
;;;
(defun sset-singleton (set)
  (declare (type sset set) (values (or sset-element null)))
  (let ((elements (cdr (sset-elements set))))
    (if (and elements (not (cdr elements)))
	(car elements)
	nil)))


;;; SSet-Subsetp  --  Interface
;;;
;;;    If Set1 is a (not necessarily proper) subset of Set2, then return true,
;;; otherwise return false.
;;;
(defun sset-subsetp (set1 set2)
  (declare (type sset set1 set2) (values boolean))
  (let ((el2 (cdr (sset-elements set2))))
    (do ((el1 (cdr (sset-elements set1)) (cdr el1)))
	((null el1) t)
      (let ((num1 (sset-element-number (car el1))))
	(loop
	  (when (null el2) (return-from sset-subsetp nil))
	  (let ((num2 (sset-element-number (pop el2))))
	    (when (>= num2 num1)
	      (when (> num2 num1) (return-from sset-subsetp nil))
	      (return))))))))


;;; SSet-Equal  --  Interface
;;;
;;;    Return true if Set1 and Set2 contain the same elements, false otherwise.
;;;
(defun sset-equal (set1 set2)
  (declare (type sset set1 set2) (values boolean))
  (do ((el1 (cdr (sset-elements set1)) (cdr el1))
       (el2 (cdr (sset-elements set2)) (cdr el2)))
      (())
    (when (null el1) (return (null el2)))
    (when (null el2) (return nil))
    (unless (eq (car el1) (car el2)) (return nil))))


;;; Copy-SSet  --  Interface
;;;
;;;    Return a new copy of Set.
;;;
(defun copy-sset (set)
  (declare (type sset set) (values sset))
  (let ((res (make-sset)))
    (setf (sset-elements res) (copy-list (sset-elements set)))
    res))


;;; SSet-Union, SSet-Intersection, SSet-Difference  --  Interface
;;;
;;; Perform the appropriate set operation on Set1 and Set2 by destructively
;;; modifying Set1.  We return true if Set1 was modified, false otherwise.
;;;
(defun sset-union (set1 set2)
  (declare (type sset set1 set2) (values boolean))
  (let* ((prev-el1 (sset-elements set1))
	 (el1 (cdr prev-el1))
	 (changed nil))
    (do ((el2 (cdr (sset-elements set2)) (cdr el2)))
	((null el2) changed)
      (let* ((e (car el2))
	     (num2 (sset-element-number e)))
	(loop
	  (when (null el1)
	    (setf (cdr prev-el1) (copy-list el2))
	    (return-from sset-union t))
	  (let ((num1 (sset-element-number (car el1))))
	    (when (>= num1 num2)
	      (if (> num1 num2)
		  (let ((new (cons e el1)))
		    (setf (cdr prev-el1) new)
		    (setq prev-el1 new  changed t))
		  (shiftf prev-el1 el1 (cdr el1)))
	      (return))
	    (shiftf prev-el1 el1 (cdr el1))))))))
;;;
(defun sset-intersection (set1 set2)
  (declare (type sset set1 set2) (values boolean))
  (let* ((prev-el1 (sset-elements set1))
	 (el1 (cdr prev-el1))
	 (changed nil))
    (do ((el2 (cdr (sset-elements set2)) (cdr el2)))
	((null el2)
	 (cond (el1
		(setf (cdr prev-el1) nil)
		t)
	       (t changed)))
      (let ((num2 (sset-element-number (car el2))))
	(loop
	  (when (null el1)
	    (return-from sset-intersection changed))
	  (let ((num1 (sset-element-number (car el1))))
	    (when (>= num1 num2)
	      (when (= num1 num2)
		(shiftf prev-el1 el1 (cdr el1)))
	      (return))
	    (pop el1)
	    (setf (cdr prev-el1) el1)
	    (setq changed t)))))))
;;;
(defun sset-difference (set1 set2)
  (declare (type sset set1 set2) (values boolean))
  (let* ((prev-el1 (sset-elements set1))
	 (el1 (cdr prev-el1))
	 (changed nil))
    (do ((el2 (cdr (sset-elements set2)) (cdr el2)))
	((null el2) changed)
      (let ((num2 (sset-element-number (car el2))))
	(loop
	  (when (null el1)
	    (return-from sset-difference changed))
	  (let ((num1 (sset-element-number (car el1))))
	    (when (>= num1 num2)
	      (when (= num1 num2)
		(pop el1)
		(setf (cdr prev-el1) el1)
		(setq changed t))
	      (return))
	    (shiftf prev-el1 el1 (cdr el1))))))))


;;; SSet-Union-Of-Difference  --  Interface
;;;
;;;    Destructively modify Set1 to include its union with the difference of
;;; Set2 and Set3.  We return true if Set1 was modified, false otherwise.
;;;
(defun sset-union-of-difference (set1 set2 set3)
  (declare (type sset set1 set2 set3) (values boolean))
  (let* ((prev-el1 (sset-elements set1))
	 (el1 (cdr prev-el1))
	 (el3 (cdr (sset-elements set3)))
	 (changed nil))
    (do ((el2 (cdr (sset-elements set2)) (cdr el2)))
	((null el2) changed)
      (let* ((e (car el2))
	     (num2 (sset-element-number e)))
	(loop
	  (when (null el3)
	    (loop
	      (when (null el1)
		(setf (cdr prev-el1) (copy-list el2))
		(return-from sset-union-of-difference t))
	      (let ((num1 (sset-element-number (car el1))))
		(when (>= num1 num2)
		  (if (> num1 num2)
		      (let ((new (cons e el1)))
			(setf (cdr prev-el1) new)
			(setq prev-el1 new  changed t))
		      (shiftf prev-el1 el1 (cdr el1)))
		  (return))
		(shiftf prev-el1 el1 (cdr el1))))
	    (return))
	  (let ((num3 (sset-element-number (car el3))))
	    (when (<= num2 num3)
	      (unless (= num2 num3)
		(loop
		  (when (null el1)
		    (do ((el2 el2 (cdr el2)))
			((null el2)
			 (return-from sset-union-of-difference changed))
		      (let* ((e (car el2))
			     (num2 (sset-element-number e)))
			(loop
			  (when (null el3)
			    (setf (cdr prev-el1) (copy-list el2))
			    (return-from sset-union-of-difference t))
			  (setq num3 (sset-element-number (car el3)))
			  (when (<= num2 num3)
			    (unless (= num2 num3)
			      (let ((new (cons e el1)))
				(setf (cdr prev-el1) new)
				(setq prev-el1 new  changed t)))
			    (return))
			  (pop el3)))))
		  (let ((num1 (sset-element-number (car el1))))
		    (when (>= num1 num2)
		      (if (> num1 num2)
			  (let ((new (cons e el1)))
			    (setf (cdr prev-el1) new)
			    (setq prev-el1 new  changed t))
			  (shiftf prev-el1 el1 (cdr el1)))
		      (return))
		    (shiftf prev-el1 el1 (cdr el1)))))
	      (return)))
	  (pop el3))))))
