;;;
;;; Boot file for moving PCL's declaration identifiers SLOTS,
;;; AUTO-COMPILE, NOT-AUTO-COMPILE to EXT.  Use this file
;;; as target:bootstrap.lisp using Pierre Mai's build scripts.
;;;

(in-package :user)

(setq lisp::*enable-package-locked-errors* nil)

(defun unintern-in-all-packages (name)
  (dolist (pkg (list-all-packages))
    (multiple-value-bind (symbol status)
	(find-symbol name pkg)
      (when symbol
	(unintern symbol pkg)))))

(unintern-in-all-packages "SLOTS")
(unintern-in-all-packages "AUTO-COMPILE")
(unintern-in-all-packages "NOT-AUTO-COMPILE")

(in-package :ext)

(export '(slots auto-compile not-auto-compile))

(in-package :kernel)

(defun compare-slots (old new)
  (values nil nil nil))

(in-package :conditions)

(defstruct (condition-class (:include slot-class))
  (slots nil :type list)
  (class-slots nil :type list)
  (report nil :type (or function null))
  (default-initargs () :type list)
  (cpl () :type list)
  (hairy-slots nil :type list))

;;; end of file
