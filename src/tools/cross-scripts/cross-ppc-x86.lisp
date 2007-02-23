;; Cross-platform cross-compile.  ppc/darwin to x86/linux.  I think a
;; few minor changes to extern-alien-name will allow this to work with
;; other x86 platforms.

(in-package :cl-user)

;;; Rename the PPC package and backend so that new-backend does the
;;; right thing.
(rename-package "PPC" "OLD-PPC")
(setf (c:backend-name c:*native-backend*) "OLD-PPC")

(c::new-backend "X86"
   ;; Features to add here
   '(:x86 :i486 :pentium
     :stack-checking
     :heap-overflow-check
     :relative-package-names
     :mp
     :gencgc
     :linkage-table
     :conservative-float-type
     :hash-new :random-mt19937
     :linux :glibc2
     :cmu :cmu19 :cmu19c
     )
   ;; Features to remove from current *features* here
   '(::alpha :osf1 :mips :x86 :i486 :pentium :ppro
     :propagate-fun-type :propagate-float-type :constrain-float-type
     :openbsd :freebsd :elf :darwin :bsd
     :ppc
     :cgc :long-float :new-random :small))

;;; Extern-alien-name for the new backend.
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  #+nil
  (concatenate 'string "_" name)
  name)
(export 'extern-alien-name)
(export 'fixup-code-object)
(export 'sanctify-for-execution)

;;; Compile the new backend.
(pushnew :bootstrap *features*)
(pushnew :building-cross-compiler *features*)
(load "target:tools/comcom")

;;; Load the new backend.
(setf (search-list "c:")
      '("target:compiler/"))
(setf (search-list "vm:")
      '("c:x86/" "c:generic/"))
(setf (search-list "assem:")
      '("target:assembly/" "target:assembly/x86/"))

;; Load the backend of the compiler.

(in-package "C")

(load "vm:vm-macs")
(load "vm:parms")
(load "vm:objdef")
(load "vm:interr")
(load "assem:support")

(load "target:compiler/srctran")
(load "vm:vm-typetran")
(load "target:compiler/float-tran")
(load "target:compiler/saptran")

(load "vm:macros")
(load "vm:utils")

(load "vm:vm")
(load "vm:insts")
(load "vm:primtype")
(load "vm:move")
(load "vm:sap")
(load "vm:system")
(load "vm:char")
(load "vm:float")

(load "vm:memory")
(load "vm:static-fn")
(load "vm:arith")
(load "vm:cell")
(load "vm:subprim")
(load "vm:debug")
(load "vm:c-call")
(load "vm:print")
(load "vm:alloc")
(load "vm:call")
(load "vm:nlx")
(load "vm:values")
(load "vm:array")
(load "vm:pred")
(load "vm:type-vops")

(load "assem:assem-rtns")

(load "assem:array")
(load "assem:arith")
(load "assem:alloc")

(load "c:pseudo-vops")

(check-move-function-consistency)

(load "vm:new-genesis")

;;; OK, the cross compiler backend is loaded.

(setf *features* (remove :building-cross-compiler *features*))

;;; Info environment hacks.
(macrolet ((frob (&rest syms)
	     `(progn ,@(mapcar #'(lambda (sym)
				   `(defconstant ,sym
				      (symbol-value
				       (find-symbol ,(symbol-name sym)
						    :vm))))
			       syms))))
  (frob OLD-PPC:BYTE-BITS OLD-PPC:WORD-BITS
	#+long-float OLD-PPC:SIMPLE-ARRAY-LONG-FLOAT-TYPE 
	OLD-PPC:SIMPLE-ARRAY-DOUBLE-FLOAT-TYPE 
	OLD-PPC:SIMPLE-ARRAY-SINGLE-FLOAT-TYPE
	#+long-float OLD-PPC:SIMPLE-ARRAY-COMPLEX-LONG-FLOAT-TYPE 
	OLD-PPC:SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-TYPE 
	OLD-PPC:SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-TYPE
	OLD-PPC:SIMPLE-ARRAY-UNSIGNED-BYTE-2-TYPE 
	OLD-PPC:SIMPLE-ARRAY-UNSIGNED-BYTE-4-TYPE
	OLD-PPC:SIMPLE-ARRAY-UNSIGNED-BYTE-8-TYPE 
	OLD-PPC:SIMPLE-ARRAY-UNSIGNED-BYTE-16-TYPE 
	OLD-PPC:SIMPLE-ARRAY-UNSIGNED-BYTE-32-TYPE 
	OLD-PPC:SIMPLE-ARRAY-SIGNED-BYTE-8-TYPE 
	OLD-PPC:SIMPLE-ARRAY-SIGNED-BYTE-16-TYPE
	OLD-PPC:SIMPLE-ARRAY-SIGNED-BYTE-30-TYPE 
	OLD-PPC:SIMPLE-ARRAY-SIGNED-BYTE-32-TYPE
	OLD-PPC:SIMPLE-BIT-VECTOR-TYPE
	OLD-PPC:SIMPLE-STRING-TYPE OLD-PPC:SIMPLE-VECTOR-TYPE 
	OLD-PPC:SIMPLE-ARRAY-TYPE OLD-PPC:VECTOR-DATA-OFFSET
	))

(let ((function (symbol-function 'kernel:error-number-or-lose)))
  (let ((*info-environment* (c:backend-info-environment c:*target-backend*)))
    (setf (symbol-function 'kernel:error-number-or-lose) function)
    (setf (info function kind 'kernel:error-number-or-lose) :function)
    (setf (info function where-from 'kernel:error-number-or-lose) :defined)))

(defun fix-class (name)
  (let* ((new-value (find-class name))
	 (new-layout (kernel::%class-layout new-value))
	 (new-cell (kernel::find-class-cell name))
	 (*info-environment* (c:backend-info-environment c:*target-backend*)))
    (remhash name kernel::*forward-referenced-layouts*)
    (kernel::%note-type-defined name)
    (setf (info type kind name) :instance)
    (setf (info type class name) new-cell)
    (setf (info type compiler-layout name) new-layout)
    new-value))
(fix-class 'c::vop-parse)
(fix-class 'c::operand-parse)

#+random-mt19937
(declaim (notinline kernel:random-chunk))

(setf c:*backend* c:*target-backend*)

;;; Extern-alien-name for the new backend.
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  #+nil
  (concatenate 'string "_" name)
  name)
(export 'extern-alien-name)
(export 'fixup-code-object)
(export 'sanctify-for-execution)

(in-package :cl-user)

;;; Don't load compiler parts from the target compilation

(defparameter *load-stuff* nil)

;; hack, hack, hack: Make old-x86::any-reg the same as
;; x86::any-reg as an SC.  Do this by adding old-x86::any-reg
;; to the hash table with the same value as x86::any-reg.
     
(let ((ht (c::backend-sc-names c::*target-backend*)))
  (setf (gethash 'old-ppc::any-reg ht)
	(gethash 'x86::any-reg ht)))

;; A hack for ppc.  Make sure the vop, move-double-to-int-arg, is
;; available in both the OLD-PPC and new PPC package.  (I don't know
;; why this is needed, but it seems to be.)
#+nil
(let ((ht (c::backend-template-names c::*target-backend*)))
  (dolist (syms '((old-ppc::move-double-to-int-arg
		   ppc::move-double-to-int-arg)
		  (old-ppc::move-single-to-int-arg
		   ppc::move-single-to-int-arg)))
    (destructuring-bind (old new)
	syms
      (setf (gethash old
		     ht)
	    (gethash new ht)))))
