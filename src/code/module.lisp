;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/module.lisp,v 1.10 2005/03/04 17:09:06 rtoy Exp $")
;;;
;;; **********************************************************************

;;; Code written by Jim Muller.
;;; Rewritten by Bill Chiles.
;;;
;;; Note that this module file is based on the old system, and is being
;;; spliced into the current sources to reflect the last minute deprecated
;;; addition of modules to the X3J13 ANSI standard.
;;;
(in-package "LISP")

(export '(*modules* provide require))


(in-package "EXTENSIONS")
(export '(*require-verbose* *module-provider-functions* defmodule))
(in-package "LISP")



;;;; Exported specials.

(defvar *modules* ()
  "This is a list of module names that have been loaded into Lisp so far.
   It is used by PROVIDE and REQUIRE.")

(defvar *require-verbose* t
  "*load-verbose* is bound to this before loading files.")

(defvar *module-provider-functions*
    '(module-provide-cmucl-defmodule module-provide-cmucl-library)
  "See function documentation for REQUIRE")

;;;; Defmodule.

(defvar *module-file-translations* (make-hash-table :test #'equal))
(defmacro defmodule (name &rest files)
  "Defines a module by registering the files that need to be loaded when
   the module is required.  If name is a symbol, its print name is used
   after downcasing it."
  `(%define-module ,name ',files))

(defun %define-module (name files)
  (setf (gethash (module-name-string name) *module-file-translations*)
        files))

(defun module-files (name)
  (gethash name *module-file-translations*))



;;;; Provide and Require.

(defun provide (module-name)
  "Adds a new module name to *modules* indicating that it has been loaded.
   Module-name may be any valid string designator.  All comparisons are
   done using string=, i.e. module names are case-sensitive."
  (pushnew (module-name-string module-name) *modules* :test #'string=)
  t)

(defun require (module-name &optional pathname)
  "Loads a module when it has not been already.  Pathname, if supplied,
   is a single pathname or list of pathnames to be loaded if the module
   needs to be.  If pathname is not supplied, then functions from the list
   *MODULE-PROVIDER-FUNCTIONS* are called in order with the stringified
   MODULE-NAME as the argument, until one of them returns non-NIL.  By
   default the functions MODULE-PROVIDE-CMUCL-DEFMODULE and MODULE-PROVIDE-
   CMUCL-LIBRARY are on this list of functions, in that order.  The first
   of those looks for a list of files that was registered by a EXT:DEFMODULE
   form.  If the module has not been defined, then the second function
   causes a file to be loaded whose name is formed by merging \"modules:\"
   and the concatenation of module-name with the suffix \"-LIBRARY\".
   Note that both the module-name and the suffix are each, separately,
   converted from :case :common to :case :local.  This merged name will be
   probed with both a .lisp and .fasl extensions, calling LOAD if it exists.

   Note that in all cases covered above, user code is responsible for
   calling PROVIDE to indicate a successful load of the module.

   While loading any files, *load-verbose* is bound to *require-verbose*
   which defaults to t."
  (let ((saved-modules (copy-list *modules*))
        (module-name (module-name-string module-name)))
    (unless (member module-name *modules* :test #'string=)
      (let ((*load-verbose* *require-verbose*))
        (if pathname
            (dolist (file (if (consp pathname) pathname (list pathname)) t)
	      (load file))
            (unless (some (lambda (p) (funcall p module-name))
                          *module-provider-functions*)
              (error "Don't know how to load ~A" module-name)))))
    (set-difference *modules* saved-modules)))

;;;; Default module providers
(defun module-provide-cmucl-defmodule (module-name)
  (when (module-files module-name)
    (dolist (file (module-files module-name) t)
      (load file))))

(defun module-provide-cmucl-library (module-name)
  (ext:without-package-locks
    (load (module-default-pathname module-name) :if-does-not-exist nil)))


;;;; Misc.

(defun module-name-string (name)
  "Coerce a string designator to a module name."
  (string name))

(defun module-default-pathname (module-name)
  "Derive a default pathname to try to load for an undefined module
named module-name.  The default pathname is constructed from the
module-name by appending the suffix \"-LIBRARY\" to it, and merging
with \"modules:\".  Note that both the module-name and the suffix are
each, separately, converted from :case :common to :case :local."
  (let* ((module-pathname (make-pathname :name module-name :case :common))
         (library-pathname (make-pathname :name "-LIBRARY" :case :common)))
    (merge-pathnames
     "modules:"
     (make-pathname :name
		    (concatenate 'string
				 (pathname-name module-pathname :case :local)
				 (pathname-name library-pathname :case :local))
                    :case :local))))
