;;;; -*- Mode: Lisp ; Package: Toolkit -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/interface-build.lisp,v 1.4 1997/08/22 20:49:27 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; Interface Builder
;;;
;;; This defines functions to crunch through the information generated
;;; about the various request operations and automatically generate the
;;; interface files for the C server.

(in-package "TOOLKIT")



;;;; Files where interface information will be stored

(defconstant *string-table-file* "target:motif/server/StringTable.h")
(defconstant *class-file* "target:motif/server/ClassTable.h")
(defconstant *interface-file* "target:motif/server/Interface.h")
(defconstant *type-file* "target:motif/server/TypeTable.h")



;;;; Functions for building the C interface files

(defun build-class-file (out)
  (declare (stream out))
  (write-line "WidgetClass *class_table[] = {" out)
  (dotimes (index (length *class-table*))
    (let ((entry (aref *class-table* index)))
      (format out "  (WidgetClass *)(&~a),~%" (cdr entry))))
  (format out "  NULL~%};~%~%#define CLASS_TABLE_SIZE ~a~%"
	  (length *class-table*)))

(defun build-type-file (out)
  (declare (stream out))
  (write-line "type_entry type_table[] = {" out)
  (dotimes (index next-type-tag)
    (let* ((stuff (svref *type-table* index))
	   (name (car stuff))
	   (kind (cdr stuff))
           sizetype)
      (when (and (eq kind name) (gethash kind *enum-table*))
        (setf kind :enum))
      (setf sizetype (case kind
                       (:enum "XtEnum")
                       (:short "short")
                       (:long "long")
                       (:float "float")
                       (:int "int")
                       (t "long")))
      (setf kind (symbol-atom kind))
      (format out "  {\"~a\",message_write_~(~a~),message_read_~(~a~),sizeof(~a)},~%"
              (symbol-class name) kind kind sizetype)))
  (format out "  {NULL,NULL,NULL,0}~%};~%~%#define TYPE_TABLE_SIZE ~a~%"
	  next-type-tag))

(defun build-interface-file (out)
  (declare (stream out))
  (write-line "request_f request_table[] = {" out)
  (dotimes (index (length *request-table*))
    (format out "  ~a,~%" (aref *request-table* index)))
  (format out "  NULL~%};~%"))

(defun build-string-table (out)
  (declare (stream out))
  (let ((table xti::*toolkit-string-table*))
    (declare (simple-vector table))
    (write-line "String string_table[] = {" out)
    (dotimes (index (length table))
      (format out "  \"~a\",~%" (svref table index)))
    (format out "  NULL~%};~%~%")
    (format out "#define STRING_TABLE_SIZE ~a~%" (length table))))

(defun build-toolkit-interface ()
  (with-open-file (out *class-file*
		       :direction :output :if-exists :supersede
		       :if-does-not-exist :create)
    (build-class-file out))
  (with-open-file (out *type-file*
		       :direction :output :if-exists :supersede
		       :if-does-not-exist :create)
    (build-type-file out))
  (with-open-file (out *interface-file*
		       :direction :output :if-exists :supersede
		       :if-does-not-exist :create)
    (build-interface-file out))
  (with-open-file (out *string-table-file*
		       :direction :output :if-exists :supersede
		       :if-does-not-exist :create)
    (build-string-table out)))
