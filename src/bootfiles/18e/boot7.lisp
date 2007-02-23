;;;
;;; Bootfile for adding a documentation slot to CONDITION-SLOT.
;;; Use this file as bootstrap.lisp using Pierre Mai's build scripts,
;;;

(in-package :conditions)

;;;
;;; Like DEFSTRUCT, but silently clobber old definitions.
;;;
(defmacro defstruct! (name &rest stuff)
  `(handler-bind ((error (lambda (c)
			   (declare (ignore c))
			   (invoke-restart 'kernel::clobber-it))))
     (defstruct ,name ,@stuff)))

;;;
;;; Add the documentation slot.
;;; 
(defstruct! condition-slot
  (name (required-argument) :type symbol)
  (initargs (required-argument) :type list)
  (readers (required-argument) :type list)
  (writers (required-argument) :type list)
  (initform-p (required-argument) :type (member t nil))
  (initform (required-argument) :type t)
  (allocation nil :type (member :instance :class nil))
  (cell nil :type (or cons null))
  (documentation nil :type (or null string)))

;;;
;;; Parse the :DOCUMENTATION slot option.
;;;
(defmacro define-condition (name (&rest parent-types) (&rest slot-specs)
				 &body options)
  (let* ((parent-types (or parent-types '(condition)))
	 (layout (find-condition-layout name parent-types))
	 (documentation nil)
	 (report nil)
	 (slot-name/accessors ())
	 (default-initargs ()))
    (collect ((slots)
	      (all-readers nil append)
	      (all-writers nil append))
      (dolist (spec slot-specs)
	(when (keywordp spec)
	  (warn "Keyword slot name indicates probable syntax error:~%  ~S"
		spec))
	(let* ((spec (if (consp spec) spec (list spec)))
	       (slot-name (first spec))
	       (allocation :instance)
	       (documentation nil)
	       (initform-p nil)
	       initform)
	  (collect ((initargs)
		    (readers)
		    (writers))
	    (do ((options (rest spec) (cddr options)))
		((null options))
	      (unless (and (consp options) (consp (cdr options)))
		(simple-program-error "Malformed condition slot spec:~%  ~S."
                                      spec))
	      (let ((arg (second options)))
		(case (first options)
		  (:reader (readers arg))
		  (:writer (writers arg))
		  (:accessor
		   (readers arg)
		   (writers `(setf ,arg)))
		  (:initform
		   (when initform-p
		     (simple-program-error "More than one :INITFORM in:~%  ~S"
                                           spec))
		   (setq initform-p t)
		   (setq initform arg))
		  (:initarg (initargs arg))
		  (:allocation
		   (setq allocation arg))
		  (:documentation
		   (when documentation
		     (simple-program-error
		      "More than one slot :DOCUMENTATION in~%  ~s" spec))
		   (unless (stringp arg)
		     (simple-program-error
		      "Slot :DOCUMENTATION is not a string in~%  ~s" spec))
		   (setq documentation arg))
		  (:type)
		  (t
		   (simple-program-error "Unknown slot option:~%  ~S"
                                         (first options))))))

	    (push (list slot-name (readers) (writers)) slot-name/accessors)
	    (all-readers (readers))
	    (all-writers (writers))
	    (slots `(make-condition-slot
		     :name ',slot-name
		     :initargs ',(initargs)
		     :readers ',(readers)
		     :writers ',(writers)
		     :initform-p ',initform-p
		     :documentation ',documentation
		     :initform
		     ,(if (constantp initform)
			  `',(eval initform)
			  `#'(lambda () ,initform)))))))
      
      (dolist (option options)
	(unless (consp option)
	  (simple-program-error "Bad option:~%  ~S" option))
	(case (first option)
	  (:documentation (setq documentation (second option)))
	  (:report
	   (let ((arg (second option)))
	     (setq report
		   (if (stringp arg)
		       `#'(lambda (condition stream)
			    (declare (ignore condition))
			    (write-string ,arg stream))
		       `#'(lambda (condition stream)
			    (funcall #',arg condition stream))))))
	  (:default-initargs
	   (do ((initargs (rest option) (cddr initargs)))
	       ((endp initargs))
	     (let ((val (second initargs)))
	       (setq default-initargs
		     (list* `',(first initargs)
			    (if (constantp val)
				`',(eval val)
				`#'(lambda () ,val))
			    default-initargs)))))
	  (t
	   (simple-program-error "Unknown option: ~S" (first option)))))

      `(progn
	 (eval-when (compile load eval)
	   (%compiler-define-condition ',name ',parent-types ',layout))

	 (declaim (ftype (function (t) t) ,@(all-readers)))
	 (declaim (ftype (function (t t) t) ,@(all-writers)))

	 ,@(when *make-condition-accessor-methods*
	     (collect ((methods))
	       (dolist (elt slot-name/accessors)
		 (destructuring-bind (slot-name readers writers) elt
		   (dolist (reader readers)
		     (methods (make-condition-accessor
			       reader name slot-name 'reader t)))
		   (dolist (writer writers)
		     (methods (make-condition-accessor
			       writer name slot-name 'writer t)))))
	       (methods)))

	 (%define-condition ',name
			    (list ,@(slots))
			    ,documentation
			    ,report
			    (list ,@default-initargs))))))

;;; end of file
