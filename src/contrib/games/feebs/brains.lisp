;;; -*- Mode: Lisp; Package: Sample-Brains; Log: feebs.log -*-
;;;
;;; Some Feeb brains.
;;; Written by Skef Wholey and Scott Fahlman.
;;;
;;; The file Feebs.Lisp contains some feebs that are built-in for noise
;;; purposes while running: RANDOM-BRAIN, WANDERING-BRAIN, and
;;; CONSERVATIVE-BRAIN.  This file contains some other simply examples of
;;; feebs.
;;;

(in-package "SAMPLE-BRAINS" :use '("LISP" "FEEBS"))


;;; Experimental brain.  Like the built-in conservative brain, but without the
;;; range limitation.
;;;
(defun cautious-brain (status proximity vision vision-left vision-right)
  (declare (ignore vision-left vision-right))
  (let ((stuff (my-square proximity)))
    (cond ((and (consp stuff) (member :mushroom stuff :test #'eq))
	   :eat-mushroom)
	  ((and (consp stuff) (member :carcass stuff :test #'eq))
	   :eat-carcass)
	  ((and (ready-to-fire status)
		(> (energy-reserve status) 30)
		(dotimes (index (line-of-sight status))
		  (let ((stuff (aref vision index)))
		    (if (listp stuff)
			(if (dolist (thing stuff)
			      (if (feeb-image-p thing)
				  (return t)))
			    (return t))
			(if (feeb-image-p stuff)
			    (return t))))))
	   :flame)
	  ((and (not (eq (left-square proximity) :rock))
		(> 0.2 (random 1.0)))
	   :turn-left)
	  ((and (not (eq (right-square proximity) :rock))
		(> 0.2 (random 1.0)))
	   :turn-right)
	  ((not (ready-to-fire status))
	   :wait)
	  ((> (line-of-sight status) 0)
	   :move-forward)
	  ((not (eq (left-square proximity) :rock))
	   :turn-left)
	  ((not (eq (right-square proximity) :rock))
	   :turn-right)
	  (t
	   :turn-around))))

(define-feeb "Cau1" 'cautious-brain)
(define-feeb "Cau2" 'cautious-brain)
(define-feeb "Cau3" 'cautious-brain)


