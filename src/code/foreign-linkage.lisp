(in-package "LISP")

(sys:register-lisp-runtime-feature :linkage-table)

;;; This gets created by genesis and lives in the static area.
(defvar *linkage-table-data*)

;;; Key is symbol name, value is index into *linkage-table-data*
(defvar *foreign-linkage-symbols* (make-hash-table :test #'equal))

;;; Very unlispy layout of the *linkage-table-data*:
;;; symbol name
;;; type - a fixnum, 1 = code, 2 = data
;;; library list - the library list at the time the symbol is registered.

(defconstant +linkage-data-entry-size+ 3)

(defun add-linkage-data (symbol-name type table loaded-libs)
  (let ((sym-type (ecase type
		    (:code 1)
		    (:data 2)))
	(entry-num (/ (fill-pointer table) +linkage-data-entry-size+)))
    (vector-push-extend symbol-name table)
    (vector-push-extend sym-type table)
    (vector-push-extend loaded-libs table)
    entry-num))

(defun foreign-linkage-entry (entry-num)
  (let ((index (* entry-num +linkage-data-entry-size+)))
    (values (aref *linkage-table-data* index)
	    (ecase (aref *linkage-table-data* (1+ index))
	      (1 :code)
	      (2 :data))
	    (aref *linkage-table-data* (+ index 2)))))

(defun foreign-linkage-symbols ()
  (/ (length *linkage-table-data*) +linkage-data-entry-size+))

(defun add-foreign-linkage (symbol-name type table linkage-hash
			    make-linkage-stub-p)
  (let ((entry-num (add-linkage-data symbol-name
				     type
				     table
				     (if make-linkage-stub-p
					 system::*global-table*
					 nil))))
    (when make-linkage-stub-p
      #-building-cross-compiler
      (let ((result (alien:alien-funcall (alien:extern-alien
					  "os_link_one_symbol"
					  (alien:function c-call:int
							  c-call:long))
					 entry-num)))
	(when (zerop result)
	  (error "~A is not defined as a foreign symbol"
		 symbol-name))))
    (setf (gethash symbol-name linkage-hash) entry-num)
    entry-num))

;;; Add a foreign linkage entry if none exists.
;;;
;;; If a linkage table is passed in it's assumed this is genesis and
;;; we don't want to make an actual stub or entry in the table space.
(defun register-foreign-linkage (symbol-name type 
				 &optional
				 (table *linkage-table-data* tablep)
				 (linkage-hash *foreign-linkage-symbols*))
  (let ((entry-num (gethash symbol-name linkage-hash)))
    (if entry-num
	entry-num
	(add-foreign-linkage symbol-name
			     type table
			     linkage-hash
			     (not tablep)))))

;;; At world build time, build the linkage hash table

(defun foreign-linkage-init ()
  ;; Sigh, would like to use LENGTH instead of FILL-POINTER but it's
  ;; not generic enough during the init process!
  ;; XXX I'm not sure that comment is true...
  (loop for i from 0 below (/ (fill-pointer *linkage-table-data*)
			      +linkage-data-entry-size+)
	do (setf (gethash (aref *linkage-table-data*
				(* i +linkage-data-entry-size+))
			  *foreign-linkage-symbols*)
		 i)))
