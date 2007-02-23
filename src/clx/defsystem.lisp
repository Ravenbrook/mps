;;; -*- Mode: Lisp; Package: Xlib; Log: clx.log -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Portions Copyright (C) 1987 Texas Instruments Incorporated.
;;; Portions Copyright (C) 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Franz Incorporated provides this software "as is" without express or
;;; implied warranty.
#+cmu
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/clx/defsystem.lisp,v 1.7 2003/07/20 15:55:23 emarsden Exp $")

;;; #+ features used in this file
;;;   CMU

(cl:in-package "XLIB")
(export 'load-clx)

(common-lisp:in-package :common-lisp-user)




;;;; Compile CLX

;;; COMPILE-CLX compiles the lisp source files and loads the binaries.
;;; It goes to some trouble to let the source files be in one directory
;;; and the binary files in another.  Thus the same set of sources can
;;; be used for different machines and/or lisp systems.  It also allows
;;; you to supply explicit extensions, so source files do not have to
;;; be renamed to fit into the naming conventions of an implementation.

;;; For example,
;;;     (compile-clx "*.lisp" "machine/")
;;; compiles source files from the connected directory and puts them
;;; into the "machine" subdirectory.  You can then load CLX out of the
;;; machine directory.

;;; The code has no knowledge of the source file types (eg, ".l" or
;;; ".lisp") or of the binary file types (eg, ".b" or ".sbin").  Calling
;;; compile-file and load with a file type of NIL usually sorts things
;;; out correctly, but you may have to explicitly give the source and
;;; binary file types.

(defun compile-clx (&optional
		    (source-pathname-defaults "")
		    (binary-pathname-defaults "")
		    &key
		    (compile-c t))

  ;; The pathname-defaults above might only be strings, so coerce them
  ;; to pathnames.  Build a default binary path with every component
  ;; of the source except the file type.  This should prevent
  ;; (compile-clx "*.lisp") from destroying source files.
  (let* ((source-path (pathname source-pathname-defaults))
	 (path        (make-pathname
			:host      (pathname-host      source-path)
			:device    (pathname-device    source-path)
			:directory (pathname-directory source-path)
			:name      (pathname-name      source-path)
			:type      nil
			:version   (pathname-version   source-path)))
	 (binary-path (merge-pathnames binary-pathname-defaults
				       path))
	 (*load-verbose* t))
				       
    ;; Make sure source-path and binary-path file types are distinct so
    ;; we don't accidently overwrite the source files.  NIL should be an
    ;; ok type, but anything else spells trouble.
    (if (and (equal (pathname-type source-path)
		    (pathname-type binary-path))
	     (not (null (pathname-type binary-path))))
	(error "Source and binary pathname defaults have same type ~s ~s"
	       source-path binary-path))

    (format t "~&;;; Default paths: ~s ~s~%" source-path binary-path)

    (labels ((compile-lisp (filename)
	       (let ((source (merge-pathnames filename source-path))
		     (binary (merge-pathnames filename binary-path)))
		 ;; If the source and binary pathnames are the same,
		 ;; then don't supply an output file just to be sure
		 ;; compile-file defaults correctly.
		 (if (equal source binary)
		     (compile-file source)
		     (compile-file source :output-file binary))
		 binary))
	     (compile-and-load (filename)
	       (load (compile-lisp filename))))

      ;; Now compile and load all the files.
      ;; Defer compiler warnings until everything's compiled, if possible.
      (with-compilation-unit
       ()
       
       (compile-and-load "package")
       (compile-and-load "depdefs")
       (compile-and-load "clx")
       (compile-and-load "dependent")
       (compile-and-load "macros")		; these are just macros
       (compile-and-load "bufmac")		; these are just macros
       (compile-and-load "buffer")
       (compile-and-load "display")
       (compile-and-load "gcontext")
       (compile-and-load "input")
       (compile-and-load "requests")
       (compile-and-load "fonts")
       (compile-and-load "graphics")
       (compile-and-load "text")
       (compile-and-load "attributes")
       (compile-and-load "translate")
       (compile-and-load "keysyms")
       (compile-and-load "manager")
       (compile-and-load "image")
       (compile-and-load "resource")
       ))))


;;;; Load CLX

;;; This procedure loads the binaries for CLX.  All of the binaries
;;; should be in the same directory, so setting the default pathname
;;; should point load to the right place.

;;; You should have a module definition somewhere so the require/provide
;;; mechanism can avoid reloading CLX.  In an ideal world, somebody would
;;; just put
;;;		(REQUIRE 'CLX)
;;; in their file (some implementations don't have a central registry for
;;; modules, so a pathname needs to be supplied).

;;; The REQUIRE should find a file that does
;;;		(IN-PACKAGE 'XLIB :USE '(LISP))
;;;		(PROVIDE 'CLX)
;;;		(LOAD <clx-defsystem-file>)
;;;		(LOAD-CLX <binary-specific-clx-directory>)

(defun load-clx (&optional (binary-pathname-defaults "")
		 &key (macrosp nil))

  (let* ((source-path (pathname ""))
	 (path        (make-pathname
			:host      (pathname-host      source-path)
			:device    (pathname-device    source-path)
			:directory (pathname-directory source-path)
			:name      (pathname-name      source-path)
			:type      nil
			:version   (pathname-version   source-path)))
	 (binary-path (merge-pathnames binary-pathname-defaults
				       path))
	 (*load-verbose* t))

    (flet ((load-binary (filename)
	     (let ((binary (merge-pathnames filename binary-path)))
	       (load binary))))

      (load-binary "package")
      (load-binary "depdefs")
      (load-binary "clx")
      (load-binary "dependent")
      (when macrosp
	(load-binary "macros")
	(load-binary "bufmac"))
      (load-binary "buffer")
      (load-binary "display")
      (load-binary "gcontext")
      (load-binary "input")
      (load-binary "requests")
      (load-binary "fonts")
      (load-binary "graphics")
      (load-binary "text")
      (load-binary "attributes")
      (load-binary "translate")
      (load-binary "keysyms")
      (load-binary "manager")
      (load-binary "image")
      (load-binary "resource")
      )))
