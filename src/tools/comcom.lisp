;;; -*- Package: CL-USER -*-
;;;
;;; **********************************************************************
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/tools/comcom.lisp,v 1.57 2004/11/05 22:02:38 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Loading this file causes the CMUCL compiler to be compiled.
;;;
(in-package "CL-USER")

#+bootstrap
(copy-packages (cons (c::backend-name c::*target-backend*) '("NEW-ASSEM" "C")))

(defvar *load-stuff*
  #+bootstrap t
  #-bootstrap (eq c:*backend* c:*native-backend*))

(defvar *byte-compile* #+small t #-small :maybe)

(unless (find-package "OLD-C")
  (rename-package "C" "C" '("OLD-C")))

;;; Import so that these types which appear in the globaldb are the same...
#+bootstrap
(import '(old-c::approximate-function-type
	  old-c::function-info old-c::defstruct-description
	  old-c::defstruct-slot-description)
	"C")


(with-compiler-log-file
    ("target:compile-compiler.log"
     :optimize
     '(optimize (speed 2) (space 2) (inhibit-warnings 2)
		(safety #+small 0 #-small 1)
		(debug #+small .5 #-small 2))
     :optimize-interface
     '(optimize-interface (safety #+small 1 #-small 2)
			  (debug #+small .5 #-small 2))
     :context-declarations
     '(#+small
       ((:or :macro
	     (:match "$SOURCE-TRANSFORM-" "$IR1-CONVERT-"
		     "$PRIMITIVE-TRANSLATE-" "$PARSE-"))
	(declare (optimize (safety 1))))
       ((:or :macro (:match "$%PRINT-"))
	(declare (optimize (speed 0))))
       (:external (declare (optimize-interface (safety 2) (debug 1))))))


(setf lisp::*enable-package-locked-errors* nil)

(comf "target:compiler/macros"
      :byte-compile #+bootstrap t #-bootstrap *byte-compile*)
(when *load-stuff*
  (load "target:compiler/macros"))

(comf "target:compiler/generic/vm-macs" :proceed t
	:byte-compile #+bootstrap t #-bootstrap nil)
(when *load-stuff*
  (load "target:compiler/generic/vm-macs"))

(comf "target:compiler/backend" :proceed t
	:byte-compile #+bootstrap t #-bootstrap nil)
(when *load-stuff*
  (load "target:compiler/backend"))

(defvar c::*target-backend* (c::make-backend))

(comf (vmdir "target:compiler/parms") :proceed t)
;(when *load-stuff*
;  (load (vmdir "target:compiler/parms")))

(comf "target:compiler/generic/objdef" :proceed t)
(comf "target:compiler/generic/interr")

(comf "target:code/struct") ; For defstruct description structures.
(comf "target:compiler/proclaim") ; For COOKIE structure.
(comf "target:compiler/globals")

(comf "target:compiler/sset")
(comf "target:compiler/bit-util")
(comf "target:compiler/node")
(comf "target:compiler/ctype")
(comf "target:compiler/vop" :proceed t)
(comf "target:compiler/vmdef")

#-bootstrap
(comf "target:compiler/meta-vmdef" :proceed t)
#+bootstrap ;; pw adds
(comf "target:compiler/meta-vmdef" :byte-compile t)
(when *load-stuff*
  (load "target:compiler/meta-vmdef"))
(comf "target:compiler/disassem" :byte-compile *byte-compile*)
(comf "target:compiler/new-assem")
(when *load-stuff*
  (load "target:compiler/new-assem"))
(comf "target:compiler/alloc")
(comf "target:compiler/knownfun")
(comf "target:compiler/fndb")
(comf "target:compiler/generic/vm-fndb")
(comf "target:compiler/main")

(with-compilation-unit
    (:optimize '(optimize (safety 1)))
  (comf "target:compiler/ir1tran"))

(with-compilation-unit
    (:optimize '(optimize (debug 2)))
  (comf "target:compiler/ir1util")
  (comf "target:compiler/ir1opt"))

(comf "target:compiler/loop")
(comf "target:compiler/ir1final")
;;try(comf "target:compiler/srctran")
(comf "target:compiler/array-tran" :byte-compile *byte-compile*)
(comf "target:compiler/seqtran" :byte-compile *byte-compile*)
(comf "target:compiler/typetran" :byte-compile *byte-compile*)
(comf "target:compiler/generic/vm-typetran" :byte-compile *byte-compile*)
(comf "target:compiler/float-tran" :byte-compile *byte-compile*)
(comf "target:compiler/saptran" :byte-compile *byte-compile*)
(comf "target:compiler/srctran") ;; try
(comf "target:compiler/locall")
(comf "target:compiler/dfo")
(comf "target:compiler/checkgen")
(comf "target:compiler/constraint")
(comf "target:compiler/envanal")


(comf "target:compiler/tn")
(comf "target:compiler/life")

(comf "target:code/debug-info")

(comf "target:compiler/debug-dump")
(comf "target:compiler/generic/utils")
#-bootstrap
(comf "target:assembly/assemfile")
#+bootstrap
(comf "target:assembly/assemfile" :byte-compile t)
(when *load-stuff* (load "target:assembly/assemfile"))


(with-compilation-unit
    (:optimize '(optimize (safety #+small 0 #-small 1) #+small (debug 0)))

  #+original
  (progn				; this is distributed order
    (comf (vmdir "target:compiler/insts"))
    (comf (vmdir "target:compiler/macros") :load *load-stuff*)
    (comf (vmdir "target:compiler/vm")))
  #+original
  (progn				; this works for x86
    (comf (vmdir "target:compiler/vm"))
    (comf (vmdir "target:compiler/macros") :load *load-stuff*)
    (comf (vmdir "target:compiler/insts")))
  #-tryit
  (progn				; this also works - better??
    (comf (vmdir "target:compiler/vm"))
    (comf (vmdir "target:compiler/insts"))
    (comf (vmdir "target:compiler/macros")
	   :byte-compile #+bootstrap t #-bootstrap nil)
    (when *load-stuff*
      (load (vmdir "target:compiler/macros")))
    )
  
(comf "target:compiler/generic/primtype")
(comf (vmdir "target:assembly/support")
       :byte-compile #+bootstrap t #-bootstrap nil) ; pw
(when *load-stuff*
  (load (vmdir "target:assembly/support")))
(comf (vmdir "target:compiler/move"))
(comf (vmdir "target:compiler/float") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/sap") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/system") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/char") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/memory"))
(comf (vmdir "target:compiler/static-fn"))
(comf (vmdir "target:compiler/arith"))
(comf (vmdir "target:compiler/subprim") :byte-compile *byte-compile*)

(comf (vmdir "target:compiler/debug") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/c-call") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/cell"))
(comf (vmdir "target:compiler/values") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/alloc"))
(comf (vmdir "target:compiler/call"))
(comf (vmdir "target:compiler/nlx") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/print") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/array") :byte-compile *byte-compile*)
(comf (vmdir "target:compiler/pred"))
(comf (vmdir "target:compiler/type-vops") :byte-compile *byte-compile*)

(comf (vmdir "target:assembly/assem-rtns") :byte-compile *byte-compile*)
(comf (vmdir "target:assembly/array") :byte-compile *byte-compile*)
(comf (vmdir "target:assembly/arith"))
(comf (vmdir "target:assembly/alloc"))

(comf "target:compiler/pseudo-vops")

); with-compilation-unit for back end.

(comf "target:compiler/aliencomp" :byte-compile *byte-compile*)

(comf "target:compiler/ltv")
(comf "target:compiler/gtn")
(with-compilation-unit
    (:optimize '(optimize (debug 2)))
  (comf "target:compiler/ltn"))
(comf "target:compiler/stack")
(comf "target:compiler/control")
(comf "target:compiler/entry")
(with-compilation-unit
    (:optimize '(optimize (debug 2)))
  (comf "target:compiler/ir2tran")
  (comf "target:compiler/generic/vm-ir2tran"))
(comf "target:compiler/copyprop")
(with-compilation-unit
    (:optimize '(optimize (debug 2)))
  (comf "target:compiler/represent"))
(comf "target:compiler/generic/vm-tran")
(with-compilation-unit
    (:optimize '(optimize (debug 2)))
  (comf "target:compiler/pack"))
(comf "target:compiler/codegen")
(with-compilation-unit
    (:optimize '(optimize (debug 2) (safety 2)))
  (comf "target:compiler/debug" :byte-compile *byte-compile*)
  (comf "target:compiler/xref" :byte-compile *byte-compile*))
#+nil
(comf "target:compiler/statcount")
(comf "target:compiler/dyncount")

(comf "target:compiler/dump")

(comf "target:compiler/generic/core")
(comf "target:compiler/generic/new-genesis")

(comf "target:compiler/eval-comp")
(comf "target:compiler/eval")
(comf "target:compiler/byte-comp")

); with-compiler-error-log
