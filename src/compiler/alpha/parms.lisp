;;; -*- Package: alpha -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/alpha/parms.lisp,v 1.10 2003/02/16 19:18:41 emarsden Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains some parameterizations of various VM
;;; attributes for the Alpha.  This file is separate from other stuff so 
;;; that it can be compiled and loaded earlier. 
;;;
;;; Written by William Lott.
;;; Alpha conversion by Sean Hallgren.
;;;

(in-package :alpha)
(use-package :c)


;;;; Compiler constants.

(eval-when (compile eval load)

(setf (backend-name *target-backend*) "ALPHA")
(setf (backend-version *target-backend*) "Alpha")
(setf (backend-fasl-file-type *target-backend*) "axpf")
(setf (backend-fasl-file-implementation *target-backend*)
      alpha-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) #x18e)
(setf (backend-register-save-penalty *target-backend*) 3)
(setf (backend-byte-order *target-backend*) :little-endian)
(setf (backend-page-size *target-backend*) 8192)

); eval-when


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

;; Values in 17f code seem to be same as HPPA. These values are from
;; DEC Assembly Language Programmers guide. The active bits are
;; actually in (byte 12 52) of the fpcr. (byte 6 52) contain the
;; exception flags. Bit 63 is the bitwise logor of all exceptions.
;; The enable and exception bytes are in a software control word
;; manipulated via OS functions and the bits in the SCP match those
;; defs. This mapping follows <machine/fpu.h>
(defconstant float-inexact-trap-bit        (ash 1 4)) ; rw
(defconstant float-underflow-trap-bit      (ash 1 3)) ; rw
(defconstant float-overflow-trap-bit       (ash 1 2)) ; ro
(defconstant float-divide-by-zero-trap-bit (ash 1 1)) ; ro
(defconstant float-invalid-trap-bit        (ash 1 0)) ; ro

(defconstant float-round-to-zero     0)
(defconstant float-round-to-negative 1)
(defconstant float-round-to-nearest  2)
(defconstant float-round-to-positive 3)

;; These aren't quite correct yet. Work in progress.
(defconstant float-rounding-mode   (byte 2 58))	; hardware fpcr
(defconstant float-exceptions-byte (byte 6 52))	; hardware fpcr
(defconstant float-sticky-bits     (byte 6 17))	; software (clear only)
(defconstant float-traps-byte      (byte 6  1))	; software fp control word
(defconstant float-condition-bit   (ash  1 63))	; summary - not used??
(defconstant float-fast-bit 0)

); eval-when



;;;; Description of the target address space.

(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start
          target-foreign-linkage-space-start
	  target-foreign-linkage-entry-size))

;;; Where to put the different spaces.
;;; 
#-linux (defparameter target-read-only-space-start #x20000000)
#-linux (defparameter target-static-space-start    #x28000000)
#-linux (defparameter target-dynamic-space-start   #x30000000)
#-linux (defparameter target-foreign-linkage-space-start #x0fc00000)
#-linux (defconstant target-foreign-linkage-entry-size 8) ;In bytes.  Duh.

#+linux (defparameter target-read-only-space-start #x10000000)
#+linux (defparameter target-static-space-start    #x28000000)
#+linux (defparameter target-dynamic-space-start   #x30000000)
#+linux (defparameter target-foreign-linkage-space-start #x0fc00000)
#+linux (defconstant target-foreign-linkage-entry-size 8) ;In bytes.  Duh.

;; The space-register holding the lisp heap.
(defconstant lisp-heap-space 4)

;; The space-register holding the C text segment.
(defconstant c-text-space 4)


;;;; Other random constants.

(export '(halt-trap pending-interrupt-trap error-trap cerror-trap
	  breakpoint-trap function-end-breakpoint-trap single-step-breakpoint
	  trace-table-normal trace-table-call-site
	  trace-table-function-prologue trace-table-function-epilogue))

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  function-end-breakpoint
  single-step-breakpoint)

(defenum (:prefix trace-table-)
  normal
  call-site
  function-prologue
  function-epilogue)



;;;; Static symbols.

(export '(static-symbols static-functions))

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defparameter static-symbols
  '(t

    ;; The C startup code must fill these in.
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

    ;; Free Pointers.
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

(defparameter static-functions
  '(length
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    ;; Probably need the following as they are defined in arith.lisp
    ;; two-arg-<= two-arg->= two-arg-/= 
    eql %negate two-arg-and two-arg-ior two-arg-xor two-arg-gcd two-arg-lcm
    ))
