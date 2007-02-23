;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/parms.lisp,v 1.117 2003/09/26 15:37:11 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains some parameterizations of various VM
;;; attributes for the MIPS.  This file is separate from other stuff so 
;;; that it can be compiled and loaded earlier. 
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted to MIPS by William Lott.
;;;

(in-package "MIPS")
(use-package "C")


;;;; Compiler constants.

(eval-when (compile eval load)

(setf (backend-name *target-backend*) #+pmax "PMAX" #+sgi "SGI")
#+pmax
(setf (backend-version *target-backend*)
      #-gengc "DECstation 3100/Mach 1.0"
      #+gengc "DECstation 3100/Mach 1.0 (gengc)")
#+sgi
(setf (backend-version *target-backend*) "SGI")

(setf (backend-fasl-file-type *target-backend*) #+pmax "pmaxf" #+sgi "sgif")
(setf (backend-fasl-file-implementation *target-backend*)
      #+pmax pmax-fasl-file-implementation #+sgi sgi-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) byte-fasl-file-version)
(setf (backend-register-save-penalty *target-backend*) 3)
(setf (backend-byte-order *target-backend*) #+pmax :little-endian
      #+sgi :big-endian)
(setf (backend-page-size *target-backend*) 16384)

); eval-when

(pushnew :new-assembler *features*)


;;;; Machine Architecture parameters:

(export '(word-bits byte-bits word-shift word-bytes float-sign-shift

	  single-float-bias single-float-exponent-byte
	  single-float-significand-byte single-float-normal-exponent-min
	  single-float-normal-exponent-max single-float-hidden-bit
	  single-float-trapping-nan-bit single-float-digits

	  double-float-bias double-float-exponent-byte
	  double-float-significand-byte double-float-normal-exponent-min
	  double-float-normal-exponent-max double-float-hidden-bit
	  double-float-trapping-nan-bit double-float-digits

	  float-underflow-trap-bit float-overflow-trap-bit
	  float-imprecise-trap-bit float-invalid-trap-bit
	  float-divide-by-zero-trap-bit))


(eval-when (compile load eval)

(defconstant word-bits 32
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant word-shift (1- (integer-length (/ word-bits byte-bits)))
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant word-bytes (/ word-bits byte-bits)
  "Number of bytes in a word.")


(defconstant float-sign-shift 31)

(defconstant single-float-bias 126)
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))
(defconstant single-float-trapping-nan-bit (ash 1 22))

(defconstant double-float-bias 1022)
(defconstant double-float-exponent-byte (byte 11 20))
(defconstant double-float-significand-byte (byte 20 0))
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))
(defconstant double-float-trapping-nan-bit (ash 1 19))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) word-bits 1))

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-underflow-trap-bit (ash 1 1))
(defconstant float-overflow-trap-bit (ash 1 2))
(defconstant float-divide-by-zero-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

(defconstant float-rounding-mode (byte 2 0))
(defconstant float-sticky-bits (byte 5 2))
(defconstant float-traps-byte (byte 5 7))
(defconstant float-exceptions-byte (byte 5 12))
(defconstant float-condition-bit (ash 1 23))
(defconstant float-fast-bit 0)			  ; No fast mode on PMAX.

); eval-when


;;;; Description of the target address space.

(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start
	  target-foreign-linkage-space-start
	  target-foreign-linkage-entry-size))

;;; Where to put the different spaces.
;;; 
(defparameter target-read-only-space-start       #x01000000)
(defparameter target-static-space-start          #x05000000)
(defparameter target-dynamic-space-start         #x07000000)
(defparameter target-foreign-linkage-space-start #x0fc00000)
(defconstant  target-foreign-linkage-entry-size 8)

;;; LARGE-OBJECT-CUTOFF -- Size in words where we start allocating in the
;;; large object space instead.  Must be less then a page.
;;;
#+gengc
(export 'large-object-cutoff)
#+gengc
(defparameter large-object-cutoff 1024)



;;;; Other non-type constants.

(export '(atomic-flag interrupted-flag halt-trap pending-interrupt-trap
	  error-trap cerror-trap breakpoint-trap function-end-breakpoint-trap
	  after-breakpoint-trap trace-table-normal trace-table-call-site
	  trace-table-function-prologue trace-table-function-epilogue))

(defenum (:suffix -flag)
  atomic
  interrupted)

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  function-end-breakpoint
  after-breakpoint)

(defenum (:prefix trace-table-)
  normal
  call-site
  function-prologue
  function-epilogue)



;;;; Static symbols.

(export '(static-symbols static-functions))

;;; Static symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
#-gengc
(defparameter static-symbols
  '(t

    ;; Random stuff needed for initialization.
    lisp::lisp-environment-list
    lisp::lisp-command-line-list
    ext::*batch-mode*
    lisp::*initial-fdefn-objects*

    ;; Functions that the C code needs to call
    lisp::%initial-function
    lisp::maybe-gc
    kernel::internal-error
    di::handle-breakpoint
    di::handle-function-end-breakpoint
    lisp::fdefinition-object

    ;; Free Pointers
    lisp::*read-only-space-free-pointer*
    lisp::*static-space-free-pointer*
    lisp::*initial-dynamic-space-free-pointer*

    ;; Things needed for non-local-exit.
    lisp::*current-catch-block*
    lisp::*current-unwind-protect-block*
    *eval-stack-top*

    ;; Interrupt Handling
    lisp::*free-interrupt-context-index*
    unix::*interrupts-enabled*
    unix::*interrupt-pending*

    ;; Make the ..slot-unbound.. symbol static to optimise the
    ;; common slot unbound check.
    pcl::..slot-unbound..

    ;; These are filled in the C run-time.
    lisp::*cmucl-lib*
    lisp::*cmucl-core-path*
    ))

#+gengc
(defparameter static-symbols
  '(t

    ;; Random stuff needed for initialization.
    lisp::lisp-environment-list
    lisp::lisp-command-line-list
    ext::*batch-mode*

    ;; Functions that the C code needs to call
    kernel::internal-error
    mach::handle-exception
    di::handle-breakpoint
    di::handle-function-end-breakpoint
    lisp::do-before-gc-stuff
    lisp::do-after-gc-stuff
    lisp::fdefinition-object
    apply

    ;; Holds a pointer to the sigcontext chain.
    kernel::*saved-state-chain*
    ))

(defparameter static-functions
  '(two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    two-arg-<= two-arg->= two-arg-/= eql %negate
    two-arg-and two-arg-ior two-arg-xor
    length two-arg-gcd two-arg-lcm))
