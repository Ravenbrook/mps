;;;
;;; Bootstrap.lisp for changing slot %NAME of DEFSTRUCT-SLOT-DESCRIPTION
;;; to NAME, and making NAME hold the slot name symbol instead
;;; of a string.  This is necessary for a conforming SLOT-EXISTS-P
;;; and MAKE-LOAD-FORM-SAVING-SLOTS.
;;;
;;; The build is unusual in that it requires two full builds with the
;;; same bootstrap file in a row, that is:
;;;
;;; 1. Copy this file to target:bootstrap.lisp and do a full build.
;;;    Choose the CLOBBER-IT restart when asked.  For an unknown
;;;    reason, it doesn't work to do this programatically.
;;;
;;; 2. Leave the bootstrap file where it is and do a full build with
;;;    the Lisp produced by step 1.
;;;
;;; 3. Remove the bootstrap file, and build again.
;;;

(in-package :kernel)

(setq *ANSI-defstruct-options-p* nil)

(defun define-class-methods (defstruct)
  (let* ((name (dd-name defstruct)))
    `(,@(let ((pf (dd-print-function defstruct)))
	  (when pf
	    `((setf (basic-structure-class-print-function (find-class ',name))
		    ,(if (symbolp pf)
			 `',pf
			 `#',pf)))))
      ,@(let ((mlff (dd-make-load-form-fun defstruct)))
	  (when mlff
	    `((setf (structure-class-make-load-form-fun (find-class ',name))
		    ,(if (symbolp mlff)
			 `',mlff
			 `#',mlff)))))
      ,@(let ((pure (dd-pure defstruct)))
	  (cond ((eq pure 't)
		 `((setf (layout-pure (%class-layout (find-class ',name)))
		    t)))
		((eq pure :substructure)
		 `((setf (layout-pure (%class-layout (find-class ',name)))
		    0)))))
      ,@(let ((def-con (dd-default-constructor defstruct)))
	  (when (and def-con (not (dd-alternate-metaclass defstruct)))
	    `((setf (structure-class-constructor (find-class ',name))
		    #',def-con)))))))

(defstruct (defstruct-slot-description
             (:conc-name dsd-)
             (:print-function print-defstruct-slot-description)
	     (:pure t)
	     (:make-load-form-fun :just-dump-it-normally))
  name
  (index (required-argument) :type fixnum)
  (accessor nil)
  default
  (type t)
  (raw-type t :type (member t single-float double-float #+long-float long-float
			    complex-single-float complex-double-float
			    #+long-float complex-long-float
			    unsigned-byte))
  (read-only nil :type (member t nil)))

(setf (info type compiler-layout 'defstruct-slot-description)
      (%class-layout (find-class 'defstruct-slot-description)))

(let ((*setf-fdefinition-hook* nil))
  (setf (fdefinition 'dsd-name)
	(lambda (dsd)
	  (let ((name (%instance-ref dsd 1)))
	    (if (stringp name)
		(intern name (symbol-package (dsd-accessor dsd)))
		name)))))

(defun dsd-%name (dsd)
  (symbol-name (dsd-name dsd)))

(defun parse-1-dsd (defstruct spec &optional
		     (islot (make-defstruct-slot-description
			     :name nil :index 0 :type t)))
  (multiple-value-bind (name default default-p type type-p read-only ro-p)
      (cond ((listp spec)
	     (destructuring-bind (name &optional (default nil default-p)
				       &key (type nil type-p)
				       (read-only nil ro-p))
		 spec
	       (values name default default-p type type-p read-only ro-p)))
	    (t
	     (when (keywordp spec)
	       (warn "Keyword slot name indicates probable syntax ~
		      error in DEFSTRUCT -- ~S."
		     spec))
	     spec))
    (when (find name (dd-slots defstruct) :test #'string= :key #'dsd-%name)
      (error 'simple-program-error
	     :format-control "Duplicate slot name ~S."
	     :format-arguments (list name)))
    (setf (dsd-name islot) name)
    (setf (dd-slots defstruct) (nconc (dd-slots defstruct) (list islot)))
    (setf (dsd-accessor islot) (concat-pnames (dd-conc-name defstruct) name))
    (when default-p
      (setf (dsd-default islot) default))
    (when type-p
      (setf (dsd-type islot)
	    (if (eq (dsd-type islot) 't)
		type
		`(and ,(dsd-type islot) ,type))))
    (when ro-p
      (if read-only
	  (setf (dsd-read-only islot) t)
	  (when (dsd-read-only islot)
	    (error "Slot ~S must be read-only in subtype ~S." name
		   (dsd-name islot)))))
    islot))

(defun compare-slots (old new)
  (declare (ignore old new))
  (values nil nil nil))

;;; end of file
