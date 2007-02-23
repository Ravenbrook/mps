;; For bootstrapping the conc-name inheritance fix, part 2.
;;
;; This file should be loaded up from a lisp.core that was built using
;; boot6-a.lisp.  Load ONLY boot6-b (and NOT boot6-a) when building
;; with this.
(setf *features* (remove :bootstrap-conc-name *features*))

(in-package "KERNEL")

(defun accessor-inherited-data (name defstruct)
  (assoc name (dd-inherited-accessor-alist defstruct) :test #'eq))

(defun do-inclusion-stuff (defstruct)
  (destructuring-bind (included-name &rest modified-slots)
		      (dd-include defstruct)
    (let* ((type (dd-type defstruct))
	   (included-structure
	    (if (class-structure-p defstruct)
		(layout-info (compiler-layout-or-lose included-name))
		(typed-structure-info-or-lose included-name))))
      (unless (and (eq type (dd-type included-structure))
		   (type= (specifier-type (dd-element-type included-structure))
			  (specifier-type (dd-element-type defstruct))))
	(error ":TYPE option mismatch between structures ~S and ~S."
	       (dd-name defstruct) included-name))
      
      (incf (dd-length defstruct) (dd-length included-structure))
      (when (class-structure-p defstruct)
	(unless (dd-print-function defstruct)
	  (setf (dd-print-function defstruct)
		(dd-print-function included-structure)))
	(unless (dd-make-load-form-fun defstruct)
	  (setf (dd-make-load-form-fun defstruct)
		(dd-make-load-form-fun included-structure)))
	(let ((mc (rest (dd-alternate-metaclass included-structure))))
	  (when (and mc (not (dd-alternate-metaclass defstruct)))
	    (setf (dd-alternate-metaclass defstruct)
		  (cons included-name mc))))
	(when (eq (dd-pure defstruct) :unspecified)
	  (setf (dd-pure defstruct) (dd-pure included-structure)))
	(setf (dd-raw-index defstruct) (dd-raw-index included-structure))
	(setf (dd-raw-length defstruct) (dd-raw-length included-structure)))
      
      (setf (dd-inherited-accessor-alist defstruct)
	    (dd-inherited-accessor-alist included-structure))
      
      (dolist (islot (dd-slots included-structure))
	(let* ((iname (dsd-name islot))
	       (modified (or (find iname modified-slots
				   :key #'(lambda (x) (if (atom x) x (car x)))
				   :test #'string=)
			     `(,iname))))
	  ;;
	  ;; We stash away an alist of accessors to parents' slots
	  ;; that have already been created to avoid conflicts later
	  ;; so that structures with :INCLUDE and :CONC-NAME (and
	  ;; other edge cases) can work as specified.
	  (when (dsd-accessor islot)
	    ;; the "oldest" (i.e. highest up the tree of inheritance)
	    ;; will prevail, so don't push new ones on if they
	    ;; conflict.
	    (pushnew (cons (dsd-accessor islot) (dsd-index islot))
		     (dd-inherited-accessor-alist defstruct)
		     :test #'eq :key #'car))
	  (parse-1-dsd defstruct modified
		       (copy-defstruct-slot-description islot)))))))

(defun define-accessors (defstruct)
  (collect ((stuff))
    (let ((ltype (dd-lisp-type defstruct)))
      (dolist (slot (dd-slots defstruct))
	(let* ((aname (dsd-accessor slot))
	       (index (dsd-index slot))
	       (slot-type `(and ,(dsd-type slot)
				,(dd-element-type defstruct)))
	       (inherited (accessor-inherited-data aname defstruct)))
	  (cond ((not inherited)
		 (stuff `(declaim (inline ,aname (setf ,aname))))
		 (stuff `(defun ,aname (structure)
			   (declare (type ,ltype structure))
			   (the ,slot-type (elt structure ,index))))
		 (unless (dsd-read-only slot)
		   (stuff
		    `(defun (setf ,aname) (new-value structure)
		       (declare (type ,ltype structure) (type ,slot-type new-value))
		       (setf (elt structure ,index) new-value)))))
		((not (= (cdr inherited) index))
		 (warn 'simple-style-warning
		       :format-control
		       "~@<Non-overwritten accessor ~S does not access ~
                        slot with name ~S (accessing an inherited slot ~
                        instead).~:@>"
		       :format-arguments (list aname (dsd-%name slot))))))))
    (stuff)))

(defun undefine-structure (class)
  (let ((info (layout-info (class-layout class))))
    (when (defstruct-description-p info)
      (let ((type (dd-name info)))
	(setf (info type compiler-layout type) nil)
	(undefine-function-name (dd-copier info))
	(undefine-function-name (dd-predicate info))
	(dolist (slot (dd-slots info))
	  (unless (dsd-inherited-p info slot)
	    (let ((aname (dsd-accessor slot)))
	      (unless (accessor-inherited-data aname info)
		(undefine-function-name aname)
		(unless (dsd-read-only slot)
		  (undefine-function-name `(setf ,aname))))))))
      ;;
      ;; Clear out the SPECIFIER-TYPE cache so that subsequent references are
      ;; unknown types.
      (values-specifier-type-cache-clear)))
  (undefined-value))

(defun %%compiler-defstruct (info)
  (declare (type defstruct-description info))
  (let* ((name (dd-name info))
	 (class (find-class name)))
    (let ((copier (dd-copier info)))
      (when copier
	(proclaim `(ftype (function (,name) ,name) ,copier))))
    
    (let ((pred (dd-predicate info)))
      (when pred
	(define-defstruct-name pred)
	(setf (info function inlinep pred) :inline)
	(setf (info function inline-expansion pred)
	      `(lambda (x) (typep x ',name)))))

    (dolist (slot (dd-slots info))
      (let* ((aname (dsd-accessor slot))
	     (setf-fun `(setf ,aname))
	     (inherited (and aname (accessor-inherited-data aname info))))
	(cond (inherited
	       (unless (= (cdr inherited) (dsd-index slot))
		 (warn 'simple-style-warning
		       :format-control
		       "~@<Non-overwritten accessor ~S does not access ~
                        slot with name ~S (accessing an inherited slot ~
                        instead).~:@>"
		       :format-arguments (list aname (dsd-%name slot)))))
	      (t
	       (unless (or (dsd-inherited-p info slot)
			   (not (eq (dsd-raw-type slot) 't)))
		 (define-defstruct-name aname)
		 (setf (info function accessor-for aname) class)
		 (unless (dsd-read-only slot)
		   (define-defstruct-name setf-fun)
		   (setf (info function accessor-for setf-fun) class))))))))
  
  (undefined-value))

(defun %defstruct (info inherits)
  (declare (type defstruct-description info))
  (multiple-value-bind (class layout old-layout)
      (ensure-structure-class info inherits "current" "new")
    (cond ((not old-layout)
	   (unless (eq (class-layout class) layout)
	     (register-layout layout)))
	  (t
	   (let ((old-info (layout-info old-layout)))    
	     (when (defstruct-description-p old-info)
	       (dolist (slot (dd-slots old-info))
		 (unless (dsd-inherited-p old-info slot)
		   (let ((aname (dsd-accessor slot)))
		     (fmakunbound aname)
		     (unless (dsd-read-only slot)
		       (fmakunbound `(setf ,aname))))))))
	   (%redefine-defstruct class old-layout layout)
	   (setq layout (class-layout class))))

    (setf (find-class (dd-name info)) class)

    (unless (eq (dd-type info) 'funcallable-structure)
      (dolist (slot (dd-slots info))
	(unless (or (dsd-inherited-p info slot)
		    (not (eq (dsd-raw-type slot) 't)))
	  (let* ((aname (dsd-accessor slot))
		 (inherited (accessor-inherited-data aname info)))
	    (unless inherited
	      (setf (symbol-function aname)
		    (structure-slot-accessor layout slot))
	      (unless (dsd-read-only slot)
		(setf (fdefinition `(setf ,aname))
		      (structure-slot-setter layout slot)))))))

      (when (dd-predicate info)
	(setf (symbol-function (dd-predicate info))
	      #'(lambda (object)
		  (declare (optimize (speed 3) (safety 0)))
		  (typep-to-layout object layout))))

      (when (dd-copier info)
	(setf (symbol-function (dd-copier info))
	      #'(lambda (structure)
		  (declare (optimize (speed 3) (safety 0)))
		  (unless (typep-to-layout structure layout)
		    (error 'simple-type-error
			   :datum structure
			   :expected-type class
			   :format-control "Structure for copier is not a ~S:~% ~S"
			   :format-arguments (list class structure)))
		  (copy-structure structure))))))

  (when (dd-doc info)
    (setf (documentation (dd-name info) 'type) (dd-doc info)))

  (undefined-value))


