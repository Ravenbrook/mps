;;; -*- Mode: Lisp; Package: library -*-
;;;
;;; This file contains stupid code to simply compile all the library entries.
;;;
;;; Written by Blaine Burks.
;;;

(in-package "LIBRARY" :nicknames '("LIB"))

(export '(compile-entries))

(defun compile-entries ()
  (let ((lib-directory (directory ed::*lisp-library-directory*)))
    (dolist (lib-spec lib-directory)
      (let* ((path-parts (pathname-directory lib-spec))
	     (last (svref path-parts (1- (length path-parts))))
	     (raw-pathname (merge-pathnames last lib-spec)))
	(when (and (ed::directoryp lib-spec)
		   (probe-file (merge-pathnames ".catalog" raw-pathname)))
	  (compile-file (merge-pathnames raw-pathname ".lisp") :error-file nil)
	  (write-string last))))))
