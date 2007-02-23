;;; -*- Log: code.log; Package: inspect -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/tty-inspect.lisp,v 1.24 2005/12/06 15:06:28 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Tty interface for INSPECT.
;;;
;;; Written by Blaine Burks

;;;
(in-package "INSPECT")

;;; The Tty inspector views LISP objects as being composed of parts.  A list,
;;; for example, would be divided into it's members, and a instance into its
;;; slots.  These parts are stored in a list.  The first two elements of this
;;; list are for bookkeeping.  The first element is a preamble string that will
;;; be displayed before the object.  The second element is a boolean value that
;;; indicates whether a label will be printed in front of a value, or just the
;;; value.  Symbols and instances need to display both a slot name and a
;;; value, while lists, vectors, and atoms need only display a value.  If the
;;; second member of a parts list is t, then the third and successive members
;;; must be an association list of slot names and values.  When the second slot
;;; is nil, the third and successive slots must be the parts of an object.
;;;

;;; *tty-object-stack* is an assoc list of objects to their parts.  
;;;
(defvar *tty-object-stack* ())

;;; ### Copied from inspect.lisp.  Remove after it is up.
(defparameter inspect-length 10)
(defparameter inspect-level 1)

(declaim (inline numbered-parts-p))
(defun numbered-parts-p (parts)
  (second parts))

(defconstant parts-offset 2)

(defun nth-parts (parts n)
  (if (numbered-parts-p parts)
      (cdr (nth (+ n parts-offset) parts))
      (nth (+ n parts-offset) parts)))

;;; Dummy definition for when we have no CLX...
(defun inspect (object)
  (tty-inspect object))

(defun tty-inspect (object)
  (unwind-protect
      (input-loop object (describe-parts object) *standard-output*)
    (setf *tty-object-stack* nil)))

;;; When %illegal-object% occurs in a parts list, it indicates that that slot
;;; is unbound.
(defvar %illegal-object% (cons nil nil))

(defun input-loop (object parts s)
  (tty-display-object parts s)
  (loop
    (format s "~&> ")
    (force-output)
    (let ((command (read))
	  ;; Use 2 less than length because first 2 elements are bookkeeping.
	  (parts-len-2 (- (length parts) 2)))
      (typecase command
	(integer
	 (cond ((< -1 command parts-len-2)
		(cond ((eq (nth-parts parts command) %illegal-object%)
		       (format s "~%That slot is unbound.~%"))
		      (t
		       (push (cons object parts) *tty-object-stack*)
		       (setf object (nth-parts parts command))
		       (setf parts (describe-parts object))
		       (tty-display-object parts s))))
	       (t
		(if (= parts-len-2 0)
		    (format s "~%This object contains nothing to inspect.~%~%")
		    (format s "~%Enter a VALID number (~:[0-~D~;0~]).~%~%"
			    (= parts-len-2 1) (1- parts-len-2))))))
	(symbol
	 (case (find-symbol (symbol-name command) (find-package "KEYWORD"))
	   ((:q :e)
	    (return object))
	   (:u
	    (cond (*tty-object-stack*
		   (setf object (caar *tty-object-stack*))
		   (setf parts (cdar *tty-object-stack*))
		   (pop *tty-object-stack*)
		   (tty-display-object parts s))
		  (t (format s "~%Bottom of Stack.~%"))))
	   (:r
	    (setf parts (describe-parts object))
	    (tty-display-object parts s))
	   (:d
	    (tty-display-object parts s))
	   ((:h :? :help)
	    (show-help s))
	   (t
	    (do-tty-inspect-eval command s))))
	(t
	 (do-tty-inspect-eval command s))))))

(defun do-tty-inspect-eval (command stream)
  (let ((result-list (restart-case (multiple-value-list (eval command))
		       (nil () :report "Return to the TTY-INSPECTOR"
			  (format stream "~%Returning to INSPECTOR.~%")
			  (return-from do-tty-inspect-eval nil)))))
    (setf /// // // / / result-list)
    (setf +++ ++ ++ + + - - command)
    (setf *** ** ** * * (car /))
    (format stream "~&~{~S~%~}" /)))

(defun show-help (s)
  (terpri)
  (write-line "TTY-Inspector Help:" s)
  (write-line "  R           -  recompute current object." s)
  (write-line "  D           -  redisplay current object." s)
  (write-line "  U           -  Move upward through the object stack." s)
  (write-line "  <number>    -  Inspect this slot." s)
  (write-line "  Q, E        -  Quit TTY-INSPECTOR." s)
  (write-line "  ?, H, Help  -  Show this help." s))

(defun tty-display-object (parts stream)
  (format stream "~%~a" (car parts))
  (let ((numbered-parts-p (numbered-parts-p parts))
	(parts (cddr parts)))
    (do ((part parts (cdr part))
	 (i 0 (1+ i)))
	((endp part) nil)
      (if numbered-parts-p
	  (format stream "~d. ~a: ~a~%" i (caar part)
		  (if (eq (cdar part) %illegal-object%)
		      "Unbound"
		      (cdar part)))
	  (format stream "~d. ~a~%" i (car part))))))



;;;; DESCRIBE-PARTS

(defun describe-parts (object)
  (cond ((typep object 'symbol)
	 (describe-symbol-parts object))
	((and (kernel::find-class 'standard-object nil)
	      (typep object 'standard-object))
	 (describe-standard-object-parts object))
	((typep object 'instance)
	 (describe-instance-parts object :structure))
	((typep object 'function)
	 (if (kernel:funcallable-instance-p object)
	     (describe-instance-parts object :funcallable-instance)
	     (describe-function-parts object)))
	((typep object 'vector)
	 (describe-vector-parts object))
	((typep object 'array)
	 (describe-array-parts object))
	((typep object 'cons)
	 (describe-cons-parts object))
	(t
	 (describe-atomic-parts object))))

(defun describe-symbol-parts (object)
  (list (format nil "~s is a symbol.~%" object) t
	(cons "Value" (if (boundp object)
			  (symbol-value object)
			  %illegal-object%))
	(cons "Function" (if (fboundp object)
			     (symbol-function object)
			     %illegal-object%))
	(cons "Plist" (symbol-plist object))
	(cons "Package" (symbol-package object))))

(defun describe-standard-object-parts (object)
  (collect ((parts))
    (let ((class (class-of object)))
      (parts (format nil "~s is an instance of ~s.~%" object class))
      (parts t)
      (dolist (slot (pcl::class-slots class) (parts))
	(parts (cons (pcl::slot-definition-name slot)
		     (if (pcl::slot-boundp-using-class class object slot)
			 (pcl::slot-value-using-class class object slot)
			 "- (slot is unbound)")))))))

(defun describe-instance-parts (object kind)
  (let ((info (layout-info (kernel:layout-of object)))
	(parts-list ()))
    (push (format nil "~s is a ~(~A~).~%" object kind) parts-list)
    (push t parts-list)
    (when (kernel::defstruct-description-p info)
      (dolist (dd-slot (dd-slots info) (nreverse parts-list))
	(push (cons (dsd-%name dd-slot)
		    (funcall (dsd-accessor dd-slot) object))
	      parts-list)))))

(defun describe-function-parts (object)
  (let* ((type (kernel:get-type object))
	 (object (if (= type vm:closure-header-type)
		     (kernel:%closure-function object)
		     object)))
    (list (format nil "Function ~s.~@[~%Argument List: ~a~]." object
		  (kernel:%function-arglist object)
		  ;; Defined from stuff used to be here.  Someone took it out.
		  )
	  t)))

(defun describe-vector-parts (object)
  (list* (format nil "Object is a ~:[~;displaced ~]vector of length ~d.~%"
		 (and (lisp::array-header-p object)
		      (lisp::%array-displaced-p object))
		 (length object))
	 nil
	 (coerce object 'list)))

(defun describe-cons-parts (object)
  (if (listp (cdr object))
      (list* (format nil "Object is a LIST of length ~d.~%" (length object))
	     nil
	     object)
      (list (format nil "Object is a CONS.~%") t
	    (cons "Car" (car object))
	    (cons "Cdr" (cdr object)))))

;;; ### Copied from inspect.lisp.  Remove when it is up.
;;; 
(defun index-string (index rev-dimensions)
  (if (null rev-dimensions)
      "[]"
      (let ((list nil))
	(dolist (dim rev-dimensions)
	  (multiple-value-bind (q r)
			       (floor index dim)
	    (setq index q)
	    (push r list)))
	(format nil "[~D~{,~D~}]" (car list) (cdr list)))))

(defun describe-array-parts (object)
  (let* ((length (min (array-total-size object) inspect-length))
	 (reference-array (make-array length
				      :displaced-to object
				      :element-type
				      (array-element-type object)))
	 (dimensions (array-dimensions object))
	 (parts ()))
    (push (format nil "Object is ~:[a displaced~;an~] array of ~a.~%~
                       Its dimensions are ~s.~%"
		  (array-element-type object)
		  (and (lisp::array-header-p object)
		       (lisp::%array-displaced-p object))
		  dimensions)
	  parts)
    (push t parts)
    (dotimes (i length (nreverse parts))
      (push (cons (format nil "~a " (index-string i (reverse dimensions)))
		  (aref reference-array i))
	    parts))))

(defun describe-atomic-parts (object)
  (list (format nil "Object is an atom.~%") nil object))
