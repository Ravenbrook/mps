;;;
;;; Bootstrap file 2 of 2 for moving LISP:CLASS and assorted functions
;;; and types to KERNEL, so that they are freed for use by PCL.
;;;
;;; Use this file as bootstrap.lisp, using Pierre Mai's build scripts,
;;; to compile the final Lisp with the cross-compiled Lisp that resulted
;;; from using the first bootstrap file in this series.
;;;

(in-package "USER")

;;;
;;; Otherwise PCL barfs when it is built, because it defines generic
;;; functions with the same names.  The functions are still there
;;; in the compiling Lisp for bootstrapping reasons.
;;;
(fmakunbound 'class-name)
(fmakunbound '(setf class-name))

;;; end of file
