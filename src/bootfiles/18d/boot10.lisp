;;;;
;;;; Boot file for the new/changed static symbols LISP::*CMUCL-LIB*
;;;; and LISP::*CMUCL-CORE-PATH* (CMUCL_LIB and CMUCL_CORE_PATH in
;;;; the C runtime).
;;;;

;;;
;;; This is the same function as in tools/setup.lisp.  It is here
;;; because setup.lisp is loaded after bootstrap.lisp in pmai's
;;; build scripts.
;;;
(defun vmdir (f)
  (merge-pathnames
   (make-pathname :directory nil :defaults f)
   (merge-pathnames
    (cond ((c:target-featurep :pmax) "mips/")
	  ((c:target-featurep :rt) "rt/")
	  ((c:target-featurep :hppa) "hppa/")
	  ((c:target-featurep :sparc) "sparc/")
	  ((c:target-featurep :x86) "x86/")
	  ((c:target-featurep :alpha) "alpha/")
	  ((c:target-featurep :sgi) "mips/")
	  ((c:target-featurep :ppc) "ppc/")
	  (t
	   (error "What machine is this?")))
    (make-pathname :directory (pathname-directory f)))))

;;;
;;; Load parms.lisp into the compiling Lisp so that it writes the new
;;; symbols to internals.h during genesis.
;;;
(load (vmdir "target:compiler/parms"))

;;;; end of boot10.lisp
