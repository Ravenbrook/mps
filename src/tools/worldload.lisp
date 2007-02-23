;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
;;; $Header: /project/cmucl/cvsroot/src/tools/worldload.lisp,v 1.105 2005/02/21 17:14:29 rtoy Exp $
;;;
;;; **********************************************************************
;;;
;;; This file loads the parts of the system that aren't cold loaded and saves
;;; the resulting core image.  It writes "lisp.core" in the DEFAULT-DIRECTORY.

;;; Make sure the core will start up in the user package.
(lisp::assert-user-package)

(in-package "LISP")

;;; Since it is unlikely that native code top-level forms are moved
;;; before being executed during worldload it is probably safe to load
;;; these into the dynamic space under CGC even without enabling
;;; dynamic space code below.
;(setf *load-x86-tlf-to-dynamic-space* t)

;;; Purify and GENCGC can move native code so all code can be loading
;;; into the dynamic space during worldload; overrides the above when
;;; enabled. Enable by default for GENCGC.  May also be safe with CGC
;;; but untested.
;(setf lisp::*enable-dynamic-space-code* t)


;;; Get some data on this core.
;;;
(write-string "What is the current lisp-implementation-version? ")
(force-output)
(set '*lisp-implementation-version* (read-line))

;;; Load the rest of the reader (maybe byte-compiled.)
(maybe-byte-load "target:code/sharpm")
(maybe-byte-load "target:code/backq")
(maybe-byte-load "target:code/pprint-loop")
(setq std-lisp-readtable (copy-readtable *readtable*))

;;; The pretty printer is part of the kernel core, but we don't turn it in
;;; until now.
;;;
(pp::pprint-init)


(maybe-byte-load "target:code/extensions")
(maybe-byte-load "target:code/defmacro")
(maybe-byte-load "target:code/sysmacs")

;;; Define a bunch of search lists relative to target:
;;;
(setf (ext:search-list "code:") '("target:code/"))
(setf (ext:search-list "c:") '("target:compiler/"))
(setf (ext:search-list "vm:")
      '(#+(or pmax sgi) "c:mips/"
        #+sparc "c:sparc/"
	#+rt "c:rt/"
	#+hppa "c:hppa/"
	#+x86 "c:x86/"
	#+amd64 "c:amd64/"
	#+alpha "c:alpha/"
	#+ppc "c:ppc/"
	"c:generic/"))
(setf (ext:search-list "assem:")
      '(#+(or pmax sgi) "target:assembly/mips/"
	#+sparc "target:assembly/sparc/"
	#+rt "target:assembly/rt/"
	#+hppa "target:assembly/hppa/"
	#+x86 "target:assembly/x86/"
	#+amd64 "target:assembly/amd64/"
	#+alpha "target:assembly/alpha/"
	#+ppc "target:assembly/ppc/"
	"target:assembly/"))
(setf (ext:search-list "hem:") '("target:hemlock/"))
(setf (ext:search-list "clx:") '("target:clx/"))
(setf (ext:search-list "pcl:") '("target:pcl/"))
(setf (ext:search-list "tools:") '("target:tools/"))

;;; Make sure the package structure is correct.
;;;
(maybe-byte-load "code:exports")

;;; Temporarily add OLD-XX nicknames needed for certain cross-compiles
#+pmax
(rename-package "PMAX" "PMAX" '("VM" "OLD-PMAX"))
#+sparc
(rename-package "SPARC" "SPARC" '("VM" "OLD-SPARC"))
#+ibmrt
(rename-package "RT" "RT" '("VM" "OLD-RT"))
#+x86
(rename-package "X86" "X86" '("VM" "OLD-X86"))
#+amd64
(rename-package "AMD64" "AMD64" '("VM" "OLD-AMD64"))
#+hppa
(rename-package "HPPA" "HPPA" '("VM" "OLD-HPPA"))
#+alpha
(rename-package "ALPHA" "ALPHA" '("VM" "OLD-ALPHA"))
#+sgi
(rename-package "SGI" "SGI" '("VM" "MIPS" "OLD-SGI"))
#+ppc
(rename-package "PPC" "PPC" '("VM" "OLD-PPC"))

;;; Load random code sources.

(maybe-byte-load "code:format-time")
(maybe-byte-load "code:parse-time")
#-gengc (maybe-byte-load "code:purify")
(maybe-byte-load "code:commandline")
(maybe-byte-load "code:sort")
(maybe-byte-load "code:time")
(maybe-byte-load "code:tty-inspect")
(maybe-byte-load "code:describe")
#+random-mt19937 (maybe-byte-load "code:rand-mt19937")
#-random-mt19937 (maybe-byte-load "code:rand")
(maybe-byte-load "target:pcl/walk")
(maybe-byte-load "code:fwrappers")
(maybe-byte-load "code:ntrace")
#-runtime (maybe-byte-load "code:profile")
(maybe-byte-load "code:weak")
(maybe-byte-load "code:final")
(maybe-byte-load "code:sysmacs")
#-gengc (maybe-byte-load "code:run-program")
(maybe-byte-load "code:query")
#-runtime (maybe-byte-load "code:internet")
#-runtime (maybe-byte-load "code:wire")
#-runtime (maybe-byte-load "code:remote")
(maybe-byte-load "code:foreign")
(maybe-byte-load "code:setf-funs")
(maybe-byte-load "code:module")
(maybe-byte-load "code:loop")
(maybe-byte-load "code:dfixnum")
#-(or gengc runtime) (maybe-byte-load "code:room")
(maybe-byte-load "code:stream-vector-io")

;;; Overwrite some cold-loaded stuff with byte-compiled versions, if any.
#-(or gengc cgc)	; x86/cgc has stuff in static space.
(progn
  (byte-load-over "target:code/debug")
  (byte-load-over "target:code/error")
  (maybe-byte-load "target:code/pprint" nil)
  (maybe-byte-load "target:code/pprint-loop" nil)
  (maybe-byte-load "target:code/format" nil)
  (maybe-byte-load "target:code/reader" nil)
  (maybe-byte-load "target:code/pathname" nil)
  (maybe-byte-load "target:code/filesys" nil)
  (maybe-byte-load "target:code/macros" nil))

(purify :root-structures
	`(lisp::%top-level extensions:save-lisp ,lisp::fop-codes)
	:environment-name "Kernel")

;;; Load the compiler.
#-(or no-compiler runtime)
(progn
  (maybe-byte-load "c:loadcom.lisp")
  (purify :root-structures '(compile-file)
	  :environment-name "Compiler")

  (maybe-byte-load "c:loadbackend.lisp")
  ;;
  ;; If we want a small core, blow away the meta-compile time VOP info.
  ;; Redundant clrhash to work around gc leakage.
  #+small
  (progn
    (clrhash (c::backend-parsed-vops c:*backend*))
    (setf (c::backend-parsed-vops c:*backend*)
	  (make-hash-table :test #'eq)))

  (purify :root-structures (list c:*backend*)
	  :environment-name (concatenate 'string (c:backend-name c:*backend*)
					 " backend")))

;;; PCL.
;;;
#-(or no-pcl runtime) (maybe-byte-load "pcl:pclload")

;;; CLX.
;;;
#-(or no-clx runtime)
(maybe-byte-load "clx:clx-library")

;;; Hemlock.
;;;
#-(or no-hemlock runtime)
(maybe-byte-load "target:hemlock/hemlock-library")

;;; CLM.
;;;
#-(or no-clm runtime)
(maybe-byte-load "target:interface/clm-library")

#+(or no-compiler runtime) (proclaim '(special *target-sl*))
#-(or no-compiler runtime) (defvar *target-sl*)
(setq *target-sl* (search-list "target:"))

#+(or no-compiler runtime) (proclaim '(special *target-core-name*))
#-(or no-compiler runtime) (defvar *target-core-name*)
(setq *target-core-name* (unix-namestring "target:lisp/lisp.core" nil))

;;; Don't include the search lists used for loading in the resultant core.
;;;
(lisp::clear-all-search-lists)

;;; Set up a default for modules and target:
;;; 
(setf (search-list "modules:") '("./"))
(setf (search-list "target:") *target-sl*)

;;; Remove temporarily added OLD-XX nicknames
#+pmax
(rename-package "PMAX" "PMAX" '("VM"))
#+sparc
(rename-package "SPARC" "SPARC" '("VM"))
#+ibmrt
(rename-package "RT" "RT" '("VM"))
#+x86
(rename-package "X86" "X86" '("VM"))
#+hppa
(rename-package "HPPA" "HPPA" '("VM"))
#+alpha
(rename-package "ALPHA" "ALPHA" '("VM"))
#+sgi
(rename-package "SGI" "SGI" '("VM"))
#+ppc
(rename-package "PPC" "PPC" '("VM"))

;;; Okay, build the thing!
;;;
(progn
  ;; We want to be in the USER package when the command line switches run.
  (in-package "CL-USER")
  ;; Clean random top-level specials.
  (setq - nil)
  (setq + nil)
  (setq * nil)
  (setq / nil)
  (setq ++ nil)
  (setq ** nil)
  (setq // nil)
  (setq +++ nil)
  (setq *** nil)
  (setq /// nil)
  ;; 
  ;; Enable the garbage collector.  But first fake it into thinking that
  ;; we don't need to garbage collect.  The save-lisp is going to call purify
  ;; so any garbage will be collected then.
  #-gengc (setf *need-to-collect-garbage* nil)
  #-gengc (gc-on)
  (setf *gc-run-time* 0)

  ;; Disable the loading of native code top level forms into the
  ;; dynamic space under CGC as it is potentially dangerous if a
  ;; native code top level form is executed after being moved without
  ;; fixups.
  #+x86 (setf *load-x86-tlf-to-dynamic-space* nil)

  ;;; GENCGC can move native code so all code can be loaded into the
  ;;; dynamic space; overrides the above when enabled.
  #+gencgc (setf lisp::*enable-dynamic-space-code* t)
  ;;; Reset the counter of the number of native code fixups.
  #+x86 (setf x86::*num-fixups* 0)

  ;; Maybe enable ANSI defstruct :print-function/:print-object processing
  #-NO-PCL
  (setq ext:*ansi-defstruct-options-p* t)
  ;;
  ;; Save the lisp.  If RUNTIME, there is nothing new to purify, so don't.
  ;; the following features are only used to control the build
  ;; process, so we remove them from the generated image
  (setq *features*
	(nreverse
	 (set-difference
	  *features* 
	  '(:runtime :no-compiler :no-pcl :no-clx :no-clm :no-hemlock))))
  (save-lisp *target-core-name*
             :root-structures
             #-(or runtime no-hemlock) `(ed ,hi::*global-command-table*)
             #+(or runtime no-hemlock) ()
             :purify #+runtime nil #-runtime t))
