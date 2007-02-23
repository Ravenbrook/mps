;;;
;;; Cross-compile script to add (simple-array double-double-float (*)).
;;;
;;; Use the standard cross-build-world.sh with this file to
;;; cross-compile.  For this to work, you need a binary that already
;;; has double-double-float support, including reader support.  (A
;;; build from the double-double-reader-branch will do nicely.)  Then
;;; do a normal full build using the cross-compiled build to get a
;;; binary with support for 1D simple arrays of double-double-float.

(in-package :cl-user)

;;; Rename the PPC package and backend so that new-backend does the
;;; right thing.
(rename-package "PPC" "OLD-PPC")
(setf (c:backend-name c:*native-backend*) "OLD-PPC")

(c::new-backend "PPC"
   ;; Features to add here
   '(:ppc
     :new-assembler
     :conservative-float-type
     :hash-new
     :random-mt19937
     :darwin :bsd
     :cmu :cmu19 :cmu19c
     :relative-package-names		; Relative package names from Allegro
     :linkage-table
     :modular-arith
     :double-double			; Double-double float support
     :gencgc				; Generational gc
     )
   ;; Features to remove from current *features* here
   '(:x86-bootstrap :alpha :osf1 :mips :x86 :i486 :pentium :ppro
     :propagate-fun-type :propagate-float-type :constrain-float-type
     :openbsd :freebsd :glibc2 :linux :pentium :elf :mp
     :stack-checking :heap-overflow-check
     :cgc :long-float :new-random :small))

;;; Extern-alien-name for the new backend.
(in-package :vm)
(defun extern-alien-name (name)
  (declare (type simple-string name))
  (concatenate 'string "_" name))
(export 'extern-alien-name)
(export 'fixup-code-object)
(export 'sanctify-for-execution)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Things needed to cross-compile double-double changes.
(in-package "C")

(eval-when (compile load eval)
(defknown simple-array-double-double-float-p (t)
  boolean
  (movable foldable flushable))
(defknown complex-double-double-float-p (t)
  boolean
  (movable foldable flushable))
(defknown simple-array-complex-double-double-float-p (t)
  boolean
  (movable foldable flushable))
)

(in-package "LISP")
(define-fop (fop-double-double-float-vector 88)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type 'double-double-float)))
    (read-n-bytes *fasl-file* result 0 (* length 4 4))
    result))

(define-fop (fop-complex-double-double-float 89)
  (prepare-for-fast-read-byte *fasl-file*
    (prog1
	(let* ((real-hi-lo (fast-read-u-integer 4))
	       (real-hi-hi (fast-read-s-integer 4))
	       (real-lo-lo (fast-read-u-integer 4))
	       (real-lo-hi (fast-read-s-integer 4))
	       (re (kernel::make-double-double-float
		    (make-double-float real-hi-hi real-hi-lo)
		    (make-double-float real-lo-hi real-lo-lo)))
	       (imag-hi-lo (fast-read-u-integer 4))
	       (imag-hi-hi (fast-read-s-integer 4))
	       (imag-lo-lo (fast-read-u-integer 4))
	       (imag-lo-hi (fast-read-s-integer 4))
	       (im (kernel::make-double-double-float
		    (make-double-float imag-hi-hi imag-hi-lo)
		    (make-double-float imag-lo-hi imag-lo-lo))))
	  (complex re im)) 
      (done-with-fast-read-byte))))

(define-fop (fop-complex-double-double-float-vector 90)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type '(complex double-double-float))))
    (read-n-bytes *fasl-file* result 0 (* length 4 8))
    result))


;; End changes for double-double
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "CL-USER")

;;; Compile the new backend.
(pushnew :bootstrap *features*)
(pushnew :building-cross-compiler *features*)
(load "target:tools/comcom")

;;; Load the new backend.
(setf (search-list "c:")
      '("target:compiler/"))
(setf (search-list "vm:")
      '("c:ppc/" "c:generic/"))
(setf (search-list "assem:")
      '("target:assembly/" "target:assembly/ppc/"))

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
				   `(handler-bind ((error #'(lambda (c)
							      (declare (ignore c))
							      (invoke-restart 'kernel::continue))))
				     (defconstant ,sym
				       (symbol-value
					(find-symbol ,(symbol-name sym)
						     :vm)))))
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
	OLD-PPC:DOUBLE-FLOAT-EXPONENT-BYTE
	OLD-PPC:DOUBLE-FLOAT-NORMAL-EXPONENT-MAX 
	OLD-PPC:DOUBLE-FLOAT-SIGNIFICAND-BYTE
	OLD-PPC:SINGLE-FLOAT-EXPONENT-BYTE
	OLD-PPC:SINGLE-FLOAT-NORMAL-EXPONENT-MAX
	OLD-PPC:SINGLE-FLOAT-SIGNIFICAND-BYTE
	))

;; Modular arith hacks
(setf (fdefinition 'vm::ash-left-mod32) #'old-ppc::ash-left-mod32)
(setf (fdefinition 'vm::lognot-mod32) #'old-ppc::lognot-mod32)

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
  (concatenate 'string "_" name))
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
	(gethash 'ppc::any-reg ht)))

;; A hack for ppc.  Make sure the vop, move-double-to-int-arg, is
;; available in both the OLD-PPC and new PPC package.  (I don't know
;; why this is needed, but it seems to be.)
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
