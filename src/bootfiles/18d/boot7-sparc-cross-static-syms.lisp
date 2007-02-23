;;; Cross-compile script for adding some new static symbols for the
;;; core search routines.
;;;
;;; Intended only for the sparc port.  Works with Pierre Mai's build
;;; scripts.
;;;
;;; You may get an error on loading target:compiler/new-assem.  Use
;;; the CONTINUE restart (return NIL from load).  The build should
;;; continue successfully.  You should take the resulting build and
;;; recompile everything once again to be sure.
;;;
;;; I (RLT) don't know why this message is there and why it's not a
;;; real problem, but the resulting cross-compiled result should work,
;;; and does work for me.
;;;

(in-package "USER")

;;; Rename the X86 package and backend so that new-backend does the
;;; right thing.
(rename-package "SPARC" "OLD-SPARC")
(setf (c:backend-name c:*native-backend*) "OLD-SPARC")

;;; Sparc to Sparc current cross compile.
;#+nil
(c::new-backend "SPARC"
   '(:new-assembler :complex-fp-vops :sparc-v9 :hash-new
     :random-mt19937
     :cmu18 :cmu18d :sparc
     :relative-package-names
     :linkage-table)
   '(:gencgc :x86 :x86-bootstrap :alpha :osf1 :mips :pentium :cgc
     :mp :i486 :long-float :new-random)
   )

#|
(load "target:tools/setup" :if-source-newer :load-source)
(comf "target:tools/setup" :load t)
|#

(setf *interactive* nil)
(setf *gc-verbose* nil)

;;; Extern-alien-name for the new backend.
(in-package "VM")
(defun extern-alien-name (name)
  (declare (type simple-string name))
  name)
(export 'extern-alien-name)
(export 'fixup-code-object)
(export 'sanctify-for-execution)

(in-package "USER")

;;; So compilation of make-fixup will work.
(setf (get 'lisp::fop-foreign-data-fixup 'lisp::fop-code) 150)

;;; Compile the new backend.
(pushnew :bootstrap *features*)

(load "target:tools/comcom")
;;(comf "target:code/foreign-linkage")

;;; Load the new backend.
(setf (search-list "c:")
      '("target:compiler/"))
(setf (search-list "vm:")
      '("c:sparc/" "c:generic/"))
(setf (search-list "assem:")
      '("target:assembly/" "target:assembly/sparc/"))
;;; Note: may need to add extra files to load to loadbackend, perhaps
;;; float-tran and srctran; try to follow the order in comcom as this
;;; is often important.

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

;;(load "target:code/foreign-linkage")
;;(load "target:compiler/dump")

(load "vm:new-genesis")

;;; OK, the cross compiler backend is loaded.

;;(setf *features* (remove :building-cross-compiler *features*))

;;; Info environment hacks.
(macrolet ((frob (&rest syms)
	     `(progn ,@(mapcar #'(lambda (sym)
				   `(defconstant ,sym
				      (symbol-value
				       (find-symbol ,(symbol-name sym)
						    :vm))))
			       syms))))
  (frob OLD-SPARC:BYTE-BITS OLD-SPARC:WORD-BITS
	#+long-float OLD-SPARC:SIMPLE-ARRAY-LONG-FLOAT-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-DOUBLE-FLOAT-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-SINGLE-FLOAT-TYPE
	#+long-float OLD-SPARC:SIMPLE-ARRAY-COMPLEX-LONG-FLOAT-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-TYPE
	OLD-SPARC:SIMPLE-ARRAY-UNSIGNED-BYTE-2-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-UNSIGNED-BYTE-4-TYPE
	OLD-SPARC:SIMPLE-ARRAY-UNSIGNED-BYTE-8-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-UNSIGNED-BYTE-16-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-UNSIGNED-BYTE-32-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-SIGNED-BYTE-8-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-SIGNED-BYTE-16-TYPE
	OLD-SPARC:SIMPLE-ARRAY-SIGNED-BYTE-30-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-SIGNED-BYTE-32-TYPE
	OLD-SPARC:SIMPLE-BIT-VECTOR-TYPE
	OLD-SPARC:SIMPLE-STRING-TYPE OLD-SPARC:SIMPLE-VECTOR-TYPE 
	OLD-SPARC:SIMPLE-ARRAY-TYPE OLD-SPARC:VECTOR-DATA-OFFSET
	OLD-SPARC:CERROR-TRAP OLD-SPARC:ERROR-TRAP
	OLD-SPARC::LOWTAG-BITS
	OLD-SPARC:CATCH-BLOCK-SIZE
	))

(let ((function (symbol-function 'kernel:error-number-or-lose)))
  (let ((*info-environment* (c:backend-info-environment c:*target-backend*)))
    (setf (symbol-function 'kernel:error-number-or-lose) function)
    (setf (info function kind 'kernel:error-number-or-lose) :function)
    (setf (info function where-from 'kernel:error-number-or-lose) :defined)))

(defun fix-class (name)
  (let* ((new-value (find-class name))
	 (new-layout (kernel::class-layout new-value))
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

;;; Ready to build.
(pushnew :bootstrap *features*)

#+nil
(let* ((old-package :old-sparc)
       (sym (find-symbol "ANY-REG" old-package)))
  (unintern sym (symbol-package sym))
  ;;(import sym :sparc)
  (import sym old-package)
  )

;; hack, hack, hack: Make old-sparc::any-reg the same as
;; sparc::any-reg as an SC.  Do this by adding old-sparc::any-reg to
;; the hash table with the same value as sparc::any-reg.
(let ((ht (c::backend-sc-names c::*target-backend*)))
  (setf (gethash 'old-sparc::any-reg ht)
	(gethash 'sparc::any-reg ht)))

#|
(load "target:tools/worldcom")
(load "target:tools/comcom")

;;; If worldbuild produces a warning that "The C header file has
;;; changed." then it will be necessary to recompile the C code, and
;;; run worldload again.
(load "target:tools/worldbuild")
(quit)
|#
