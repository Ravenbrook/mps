;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/statcount.lisp,v 1.6 1994/10/31 04:27:28 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/statcount.lisp,v 1.6 1994/10/31 04:27:28 ram Exp $
;;;
;;; Functions and utilities for collecting statistics on static vop usages.
;;;
;;; Written by William Lott
;;;
(in-package "C")

(export '(*count-vop-usages*))



;;;; Vop counting utilities

;;; T if we should count the number of times we use each vop and the number
;;; of instructions that come from each.
;;; 
(defvar *count-vop-usages* nil)

;;; Hash table containing all the current counts.  The key is the name of the
;;; vop, and the value is a 3 element simple-vector.  The elements are:
;;;   0 - the name of the vop.
;;;   1 - the number of times this vop was emitted.
;;;   2 - the number of normal instructions emitted due to this vop.
;;;   3 - the number of elsewhere instructions emitted due to this vop.
;;; 
(defvar *vop-counts* (make-hash-table :test #'eq))

;;; COUNT-VOPS  --  internal interface.
;;;
;;; COUNT-VOPS is called by COMPILE-COMPONENT to count the vop usages in
;;; component.
;;; 
(defun count-vops (component)
  (declare (ignore component))
  (error "Vop counting not implemented for the new assembler.")
  #+nil
  (flet ((vop-entry (vop)
	   (let* ((name (vop-info-name (vop-info vop)))
		  (entry (gethash name *vop-counts*)))
	     (the (simple-vector 4)
		  (or entry
		      (let ((new (make-array 4 :initial-element 0)))
			(setf (svref new 0) name)
			(setf (gethash name *vop-counts*) new)
			new))))))
    (do-ir2-blocks (block component)
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
	  ((null vop))
	(incf (svref (vop-entry vop) 1))))
    (assem:count-instructions
     #'(lambda (vop count elsewherep)
	 (incf (svref (vop-entry vop)
		      (if elsewherep 3 2))
	       count))
     *code-segment*
     *elsewhere*
     :size))
  (undefined-value))


;;;; Stuff for using the statistics.

;;; Clear-Vop-Counts -- interface
;;; 
(defun clear-vop-counts ()
  (clrhash *vop-counts*)
  nil)

;;; Report-Vop-Counts -- interface
;;;
(defun report-vop-counts (&key (cut-off 15) (sort-by :size))
  (declare (type (or null unsigned-byte) cut-off)
	   (type (member :size :count :name) sort-by))
  (let ((results nil)
	(total-count 0)
	(total-size 0)
	(w/o-elsewhere-size 0))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push value results)
		 (incf total-count (svref value 1))
		 (incf total-size (+ (svref value 2) (svref value 3)))
		 (incf w/o-elsewhere-size (svref value 2)))
	     *vop-counts*)
    (format t "~18<Vop ~> ~17:@<Count~> ~17:@<Bytes~> ~
              ~17:@<W/o elsewhere~> Ave Sz~%")
    (let ((total-count (coerce total-count 'double-float))
	  (total-size (coerce total-size 'double-float))
	  (w/o-elsewhere-size (coerce w/o-elsewhere-size 'double-float)))
      (dolist (info (sort results
			  (ecase sort-by
			    (:name #'(lambda (name-1 name-2)
				       (string< (symbol-name name-1)
						(symbol-name name-2))))
			    ((:count :size) #'>))
			  :key (ecase sort-by
				 (:name #'(lambda (x) (svref x 0)))
				 (:count #'(lambda (x) (svref x 1)))
				 (:size #'(lambda (x)
					    (+ (svref x 2)
					       (svref x 3)))))))
	(when cut-off
	  (if (zerop cut-off)
	      (return)
	      (decf cut-off)))
	(let* ((name (symbol-name (svref info 0)))
	       (name-len (length name))
	       (count (svref info 1))
	       (count-len (truncate (truncate (rational (log count 10))) 3/4))
	       (w/o-elsewhere (svref info 2))
	       (size (+ w/o-elsewhere (svref info 3))))
	  (if (> (+ name-len count-len) 24)
	      (format t "~A:~%~18T~9:D" name count)
	      (format t "~VT~A:~VT~:D"
		      (max (- 18 name-len) 0)
		      name
		      (- 26 count-len)
		      count))
	  (format t " (~4,1,2F%) ~9:D (~4,1,2F%) ~9:D (~4,1,2F%) ~3D ~3D~%"
		  (/ (coerce count 'double-float) total-count)
		  size
		  (/ (coerce size 'double-float) total-size)
		  w/o-elsewhere
		  (/ (coerce w/o-elsewhere 'double-float) w/o-elsewhere-size)
		  (round size count)
		  (round w/o-elsewhere count))))))
  (values))

(defun save-vop-counts (filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (prin1 value stream)
		 (terpri stream))
	     *vop-counts*)))

(defun augment-vop-counts (filename)
  (with-open-file (stream filename)
    (loop
      (let ((stuff (read stream nil :eof)))
	(when (eq stuff :eof)
	  (return))
	(multiple-value-bind
	    (entry found)
	    (gethash (svref stuff 0) *vop-counts*)
	  (cond (found
		 (incf (svref entry 1) (svref stuff 1))
		 (incf (svref entry 2) (svref stuff 3))
		 (incf (svref entry 3) (svref stuff 2)))
		(t
		 (setf (gethash (svref stuff 0) *vop-counts*) stuff))))))))

