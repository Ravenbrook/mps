;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/search-list.lisp,v 1.4 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Logical name (search list) hackery for Lisp'ers.
;;;
;;; Written by Bill Chiles.

(in-package 'lisp)
(in-package "EXTENSIONS")
(export 'search-list)
(in-package 'lisp)


(defvar *search-list-table* (make-hash-table :test #'equal))
(defvar *rsl-circularity-check* (make-hash-table :test #'equal))


(defun search-list (name)
  "Returns a list of strings that are the of name.
   This is setf'able.  If any provided string in a setting value
   does end with a colon or slash, a slash is added.  Also, the
   list is copied."
  (let ((dev (pathname-device name)))
    (unless dev (error "No device in ~S." name))
    (copy-list (gethash dev *search-list-table*))))

(defun %set-search-list (name new-value)
  (unless (listp new-value)
    (error "New value for search-list ~S not a list -- ~S."
	   name new-value))
  (let ((dev (pathname-device name)))
    (unless dev (error "No device in ~S." name))
    (nstring-downcase dev)
    (setf (gethash dev *search-list-table*)
	  (mapcar #'(lambda (x)
		      (let ((x (if (pathnamep x) (namestring x) x)))
			(declare (simple-string x))
			(let* ((len (length x))
			       (char (schar x (1- len))))
			  (if (or (char= char #\:) (char= char #\/))
			      x
			      (concatenate 'simple-string x "/")))))
		  new-value)))
  new-value)


(defun resolve-search-list (name first-only-p)
  "This takes a Sesame search-list name (\"default\") instead of the form
   taken by SEARCH-LIST (\"default:\").  If first-only-p is non-nil, then
   only the first complete expansion of name is returned.  If, during the
   expansion of name, an undefined search list is encountered, an error
   is signaled."
  (setf name (string-downcase name))
  (setf (gethash name *rsl-circularity-check*) t)
  (unwind-protect
    (resolve-search-list-aux name first-only-p)
    (clrhash *rsl-circularity-check*)))


;;; RESOLVE-SEARCH-LIST-BODY is used in RESOLVE-SEARCH-LIST-AUX and
;;; RSL-FIRST.  This means the former is recursive, and the former and
;;; latter are mutually recursive.  This form first looks at an element of
;;; a list of expansions for a search list for a colon which means that the
;;; element needs to be further resolved.  If there is no colon, execute
;;; the already-form.  If there is a colon, grab the new element to resolve
;;; recursively.  If this new element has been seen already, we have an
;;; infinite recursion brewing.  Recursively expand this new element.  If
;;; there are no expansions, signal an error with the offending search list;
;;; otherwise, execute the expanded-form if the argument element was only a
;;; search list, or the concat-form if the argument element was a search
;;; list followed by a directory sequence.  The locals pos, len, and res
;;; are meant to be referenced at the call sites.
;;; 
(eval-when (compile eval)
(defmacro resolve-search-list-body (first-only-p element expanded-form
						 concat-form already-form)
  `(let ((pos (position #\: ,element :test #'char=))
	 (len (length ,element)))
     (declare (fixnum len))
     (if pos
	 (let ((dev (nstring-downcase (subseq ,element 0 pos))))
	   (if (gethash dev *rsl-circularity-check*)
	       (error "Circularity in search list -- ~S." dev)
	       (setf (gethash dev *rsl-circularity-check*) t))
	   (let ((res (resolve-search-list-aux dev ,first-only-p)))
	     (remhash dev *rsl-circularity-check*)
	     (if res
		 (if (= (the fixnum pos) (the fixnum (1- len)))
		     ,expanded-form
		     ,concat-form)
		 (error "Undefined search list -- ~S"
			(subseq ,element 0 (1+ pos))))))
	 ,already-form)))
) ; eval-when

;;; RESOLVE-SEARCH-LIST-AUX takes a device/search-list string (that is,
;;; without the colon) and whether it should return the first expansion
;;; found.  If dev is not defined, signal an error with the offending
;;; search list.  If dev is defined, and first-only-p is non-nil, then just
;;; resolve the first possible expansion.  Otherwise, we loop over all of
;;; the possible expansions resolving each one completely, appending the
;;; results in order as they appear in entry.  If entry is just another
;;; search list, then append the result (res) of its expansion onto result.
;;; If entry is a search list followed by a directory spec, then
;;; concatenate each of the expansions of the search list with the
;;; directory, appending this to result.  If entry is just a directory
;;; spec, then append the list of entry to result.
;;; 
(defun resolve-search-list-aux (dev first-only-p)
  (let ((entry (gethash dev *search-list-table*)))
    (if entry
	(if first-only-p
	    (rsl-first (car entry))
	    (do ((entries entry (cdr entries))
		 (result (cons nil nil)))
		((null entries) (cdr result))
	      (let ((entry (car entries)))
		(declare (simple-string entry))
		(resolve-search-list-body
		 nil entry (nconc result res)
		 (nconc result (rsl-concat res (subseq entry (1+ pos) len)))
		 (nconc result (list entry))))))
	(error "Undefined search list -- ~S" 
	       (concatenate 'simple-string dev ":")))))

;;; RSL-FIRST takes a possible expansion and resolves it if necessary.
;;; If first is just another search list, then return the expansions
;;; of this search list.  If first is another search list followed by
;;; directory spec, then concatenate each of the expansions of the
;;; search list with the directory, returning this list.  If first is
;;; just a directory spec, then return the list of it.
;;; 
(defun rsl-first (first)
  (declare (simple-string first))
  (resolve-search-list-body t first res
			    (rsl-concat res (subseq first (1+ pos) len))
			    (list first)))

;;; RSL-CONCAT takes a list of expansions (prefixes) for a search list
;;; that was concatenated with a directory spec (suffix).  Each prefix
;;; is concatenated with the suffix and stored back where the prefix
;;; was.  The destructively modified prefixes is returned.
;;; 
(defun rsl-concat (prefixes suffix)
  (declare (simple-string suffix))
  (do ((ptr prefixes (cdr ptr)))
      ((null ptr) prefixes)
    (setf (car ptr)
	  (concatenate 'simple-string (the simple-string (car ptr)) suffix))))
