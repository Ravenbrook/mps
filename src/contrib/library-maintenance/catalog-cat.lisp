;;; -*- Mode: Lisp; Package: library -*-
;;; 
;;; This contains somewhat specific code for gathering all the pertinent
;;; .catalog files from the CMU Common Lisp Library subdirectories to
;;; construct one catalog.txt file from them.
;;; 
;;; This code uses at least one implementation internal function, hacks
;;; pathnames as strings, accepts CMU Common Lisp logical names, tries
;;; to determine what a directory name looks like, uses CMU Common Lisp
;;; extensions, and generally tries not to be portable in any way.
;;; 
;;; Written by Bill Chiles
;;;

(in-package "LIBRARY" :nicknames '("LIB"))

(export 'build-catalog)


(defun build-catalog (dir)
  "Gather all the .catalog files from the subdirectories of dir, bashing them
   into a file named \"new-catalog.txt\" in dir.  The argument must be a
   logical name or end with a slash (/)"
  (with-open-file (stream (merge-pathnames dir "new-catalog.txt")
			  :direction :output :if-exists :new-version)
    (let* ((files (catalog-files dir))
	   (last (car (last files))))
      (dolist (f files)
	(with-open-file (catalog f)
	  (do ((line (read-line catalog nil :eof)
		     (read-line catalog nil :eof)))
	      ((eq line :eof))
	    (write-line line stream))
	  (unless (equal f last)
	    (format stream "~%~|~%")))))))


;;; CATALOG-FILES does a DIRECTORY (CMU Common Lisp) of dir returning a
;;; list of file names containing the string ".catalog".  The directory
;;; listing may include subdirectories, but these subdirs contain only
;;; files (one of which is a catalog file).  The subdirectory catalog
;;; files are included in the result list in order of the subdirectory's
;;; appearance.
;;; 
(defun catalog-files (dir)
  (let ((listing (directory dir))
	result)
    (dolist (f listing (nreverse result))
      (let ((name (namestring f)))
	(declare (simple-string name))
	(if (directoryp f)
	    (dolist (f (directory name))
	      (let ((name (namestring f)))
		(declare (simple-string name))
		(when (search ".catalog" name :test #'char=)
		  (push name result)
		  (return))))
	    (if (search ".catalog" name :test #'char=)
		(push name result)))))))

(defun directoryp (p)
  (let ((p (pathname p)))
    (string= (the simple-string (directory-namestring p))
	     (the simple-string (namestring p)))))
