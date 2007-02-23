;;;
;;; Bootstrap file 1 of 2 for moving LISP:CLASS and assorted functions
;;; and types to KERNEL, so that they are freed for use by PCL.
;;;
;;; Use this file as bootstrap.lisp, using Pierre Mai's build scripts,
;;; to cross-compile, resulting in a Lisp that is almost what we
;;; need to get to final result, except that it has still CLASS-NAME
;;; defined as a function, which needs to be corrected in a second
;;; build with the second bootstrap file of this series.
;;;

(in-package "LISP")

;;;
;;; Move the symbol whose name is NAME from LISP to KERNEL, and
;;; fix packages using that symbol.
;;;
(defun lisp-symbol->kernel (name)
  (let ((sym (find-symbol name :lisp)))
    (ext:collect ((pkgs))
      (dolist (p (list-all-packages))
	(when (eq (find-symbol name p) sym)
	  (pkgs p)))
      (unexport sym :lisp)
      (unintern sym :lisp)
      (import sym :kernel)
      (dolist (p (pkgs))
	(unless (eq (find-symbol name p) sym)
	  (import sym p))))))

;;;
;;; Move symbols we want to end up in KERNEL from LISP to KERNEL.
;;;
(defvar *symbols-to-move*
  '("CLASS"
    "BUILT-IN-CLASS"
    "STANDARD-CLASS"
    "STRUCTURE-CLASS"
    "FIND-CLASS"
    "CLASS-OF"))

(mapc #'lisp-symbol->kernel *symbols-to-move*)

;;;
;;; Redefine MAKE-PACKAGE because cross-compilation and build scripts
;;; rename some packages and make new ones.  We want to make sure that
;;; the new packages contain the symbols we imported from KERNEL
;;; in LISP-SYMBOL->KERNEL into the old package.
;;; 
(defun make-package (name &key (use *default-package-use-list*) nicknames
			  (internal-symbols 10) (external-symbols 10))
  "Makes a new package having the specified Name and Nicknames.  The
  package will inherit all external symbols from each package in
  the use list.  :Internal-Symbols and :External-Symbols are
  estimates for the number of internal and external symbols which
  will ultimately be present in the package."
  (when (find-package name)
    (cerror "Leave existing package alone."
	    "A package named ~S already exists" name))
  (let* ((name (package-namify name))
	 (package (internal-make-package
		   :%name name
		   :internal-symbols (make-package-hashtable internal-symbols)
		   :external-symbols (make-package-hashtable external-symbols))))
    (if *in-package-init*
	(push (list use package) *deferred-use-packages*)
	(use-package use package))
    (enter-new-nicknames package nicknames)
    (setf (gethash name *package-names*) package)
    (let ((old-pkg (find-package (concatenate 'string "OLD-" name))))
      (when (and old-pkg
		 (find-symbol (car *symbols-to-move*) old-pkg))
	(dolist (sym *symbols-to-move*)
	  (import (find-symbol sym "KERNEL") package))))
    package))

(in-package :kernel)

(export '(%class-layout %class-name %class-state
	  %class-direct-superclasses %class-subclasses %class-pcl-class))

;;;
;;; Like DEFSTRUCT, but silently clobber old definitions.
;;;
(defmacro defstruct! (name &rest stuff)
  `(handler-bind ((error (lambda (c)
			   (declare (ignore c))
			   (invoke-restart 'kernel::clobber-it))))
     (defstruct ,name ,@stuff)))

;;;
;;; Redefine CLASS in the compiling Lisp.
;;;
(defstruct! (class
	     (:conc-name %class-)
	     (:make-load-form-fun class-make-load-form-fun)
	     (:print-function %print-class)
	     (:include ctype
		       (:class-info (type-class-or-lose 'class)))
	     (:pure nil))
  (name nil)
  (layout nil :type (or layout null))
  (state nil :type (member nil :read-only :sealed))
  (direct-superclasses () :type list)
  (subclasses nil :type (or hash-table null))
  (pcl-class nil))

;;;
;;; Provide old accessors so that the compiling Lisp continues
;;; to work.
;;;
(macrolet ((def (o n)
	     `(progn
		(defun ,o (x) (,n x))
		(defun (setf ,o) (v x) (setf (,n x) v)))))
  (def class-layout %class-layout)
  (def class-name %class-name)
  (def class-state %class-state)
  (def class-direct-subclasses %class-direct-superclasses)
  (def class-subclasses %class-subclasses))

;;;
;;; This build feature is checked in code:class.lisp, and makes it
;;; define the same accessors as above.
;;;
(pushnew :bootstrap-lisp-class=pcl-class *features*)

;;;
;;; Redefine other affected structs.
;;;
(defstruct! (undefined-class
	     (:include class)
	     (:constructor make-undefined-class (name))))

(defstruct! (built-in-class (:include class))
  (translation nil :type (or ctype (member nil :initializing))))

(defstruct! (slot-class (:include class))
  (print-function nil :type (or function symbol null)))

(defstruct! (basic-structure-class (:include slot-class)))

(defstruct! (structure-class (:include basic-structure-class))
  (make-load-form-fun nil :type (or function symbol
				    (member :just-dump-it-normally
					    :ignore-it
					    nil)))
  (constructor nil :type (or function null)))

(defstruct! (funcallable-structure-class (:include basic-structure-class)))
(defstruct! (std-class (:include class)))
(defstruct! (standard-class (:include std-class)))
(defstruct! (random-pcl-class (:include std-class)))

(in-package "CONDITIONS")

(kernel::defstruct! (condition-class (:include slot-class))
  (slots nil :type list)
  (class-slots nil :type list)
  (report nil :type (or function null))
  (default-initargs () :type list)
  (cpl () :type list)
  (hairy-slots nil :type list))

;;; end of file
