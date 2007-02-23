;;;;
;;;; Boot file for control stack checking, except that there is no
;;;; boot file necessary, only a normal cross-compilation.
;;;;
;;;; Stack checking is implemented and known to work on FreeBSD 4/x86
;;;; and Debian 2.2.20/86 only, and it is optional, that is, one
;;;; should be able to build with or without it.
;;;;
;;;; Using Pierre Mai's build scripts, assuming your source directory
;;;; is called src-head, and your fasl directories are called
;;;; fasl-head and cross-fasl-head:
;;;;
;;;; -- Add :STACK-CHECKING to fasl-head/setenv.lisp.
;;;;
;;;; -- Add :STACK-CHECKING to your cross-compilation script,
;;;; where it has 
;;;;
;;;; (c::new-backend "X86"
;;;;   '(... :stack-checking ...)
;;;;   '(...))
;;;;
;;;; -- Change occurrences of KERNEL:CLASS-LAYOUT in your
;;;; cross-compilation script to KERNEL:%CLASS-LAYOUT.
;;;;
;;;; -- Cross-compile to fasl-head via cross-fasl-head.  This will fail
;;;; and tell you that the C header file has changed.  Build a new
;;;; runtime, and start over.
;;;;	
;;;; NB: If your source directory is not called "src", make sure that
;;;; fasl-head/lisp/Config refers to the directory you are actually
;;;; using, src-head in this example.
;;;;	
;;;; -- Do a full normal compile with the result of the
;;;; cross-compilation.
;;;;
