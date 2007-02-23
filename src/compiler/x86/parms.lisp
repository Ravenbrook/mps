;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: python-x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/x86/parms.lisp,v 1.31 2006/08/18 02:29:28 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains some parameterizations of various VM
;;; attributes for the x86.  This file is separate from other stuff so 
;;; that it can be compiled and loaded earlier. 
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997.
;;;

(in-package :x86)
(use-package :c)

;;; ### Note: we simultaneously use ``word'' to mean a 32 bit quantity and
;;; a 16 bit quantity depending on context.  This is because Intel insists
;;; on calling 16 bit things words and 32 bit things double-words (or dwords).
;;; Therefore, in the instruction defintion and register specs, we use the
;;; Intel convention.  But whenever we are talking about stuff the rest of
;;; the lisp system might be interested in, we use ``word'' to mean the size
;;; of a descriptor object, which is 32 bits.


;;;; Compiler constants.

(eval-when (compile eval load)

(setf (backend-name *target-backend*) "X86")
(setf (backend-version *target-backend*) "Intel x86")
(setf (backend-fasl-file-type *target-backend*) "x86f")
(setf (backend-fasl-file-implementation *target-backend*)
      x86-fasl-file-implementation)
(setf (backend-fasl-file-version *target-backend*) byte-fasl-file-version)
(setf (backend-register-save-penalty *target-backend*) 3)
(setf (backend-byte-order *target-backend*) :little-endian)

;;;#+cross-compiler
;;;(setf (backend-package *target-backend*) :python-x86)

#|
;;;
;;; windows NT uses a memory system granularity of 64K, which means everything
;;; that gets mapped must be a multiple of that.  The real page size is 512, but 
;;; that doesn't do us a whole lot of good.  Effectively, the page size is 64K
;;;
(setf (backend-page-size *target-backend*) 65536)
|#

(setf (backend-page-size *target-backend*) 4096)
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

	  long-float-bias long-float-exponent-byte
	  long-float-significand-byte long-float-normal-exponent-min
	  long-float-normal-exponent-max long-float-hidden-bit
	  long-float-trapping-nan-bit long-float-digits

	  float-underflow-trap-bit float-overflow-trap-bit
	  float-imprecise-trap-bit float-invalid-trap-bit
	  float-divide-by-zero-trap-bit))

#+double-double
(export '(double-double-float-digits))
	  

(eval-when (compile load eval)

(defconstant word-bits 32
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant word-shift (1- (integer-length (/ word-bits byte-bits)))
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant word-bytes (/ word-bits byte-bits)
  "Number of bytes in a word.")
) ; eval-when

(eval-when (compile load eval)
(defconstant float-sign-shift 31)

;; These values were taken from the alpha code. The values for
;; bias and exponent min/max are not the same as shown in the 486 book.
;; They may be correct for how Python uses them.
(defconstant single-float-bias 126)	; Intel says 127
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
;; The 486 book shows the exponent range -126 to +127. The Lisp
;; code that uses these values seems to want already biased numbers.
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

(defconstant long-float-bias 16382)
(defconstant long-float-exponent-byte (byte 15 0))
(defconstant long-float-significand-byte (byte 31 0))
(defconstant long-float-normal-exponent-min 1)
(defconstant long-float-normal-exponent-max #x7FFE)
(defconstant long-float-hidden-bit (ash 1 31))		; Actually not hidden
(defconstant long-float-trapping-nan-bit (ash 1 30))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) word-bits 1))

(defconstant long-float-digits
  (+ (byte-size long-float-significand-byte) word-bits 1))

#+double-double
(defconstant double-double-float-digits
  (* 2 double-float-digits))

;;; pfw -- from i486 microprocessor programmers reference manual
(defconstant float-invalid-trap-bit        (ash 1 0))
(defconstant float-denormal-trap-bit       (ash 1 1))
(defconstant float-divide-by-zero-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit       (ash 1 3))
(defconstant float-underflow-trap-bit      (ash 1 4))
(defconstant float-inexact-trap-bit        (ash 1 5))

(defconstant float-round-to-nearest  0)
(defconstant float-round-to-negative 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-zero     3)

(defconstant float-precision-24-bit  0)
(defconstant float-precision-53-bit  2)
(defconstant float-precision-64-bit  3)

(defconstant float-rounding-mode   (byte 2 10))
(defconstant float-sticky-bits     (byte 6 16))
(defconstant float-traps-byte      (byte 6  0))
(defconstant float-exceptions-byte (byte 6 16))
(defconstant float-precision-control (byte 2 8))
(defconstant float-fast-bit 0) ; No fast mode on x86
); eval-when


;;;; Description of the target address space.

(export '(target-read-only-space-start
	  target-static-space-start
	  target-dynamic-space-start
	  target-foreign-linkage-space-start
	  target-foreign-linkage-entry-size))

;;; Where to put the different spaces.
;;; 
(defconstant target-read-only-space-start #x10000000)
(defconstant target-static-space-start
  #+FreeBSD #x28F00000
  #-FreeBSD #x28000000)
(defconstant target-dynamic-space-start
  #+linux #x58100000
  #+(or FreeBSD OpenBSD) #x48000000
  #+NetBSD #x48800000)
(defconstant target-foreign-linkage-space-start
  #+linux #x58000000
  #-linux #xB0000000)
(defconstant target-foreign-linkage-entry-size 8) ;In bytes.  Duh.

;;; Given that NIL is the first thing allocated in static space, we
;;; know its value at compile time:
;;; 
(defparameter nil-value (+ target-static-space-start #xB))


;;;; Other random constants.

(export '(halt-trap pending-interrupt-trap error-trap cerror-trap
	  breakpoint-trap function-end-breakpoint-trap
	  single-step-breakpoint-trap
	  dynamic-space-overflow-error-trap
	  dynamic-space-overflow-warning-trap
          object-not-list-trap object-not-instance-trap
	  trace-table-normal trace-table-call-site
	  trace-table-function-prologue trace-table-function-epilogue))

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  function-end-breakpoint
  single-step-breakpoint
  dynamic-space-overflow-warning
  dynamic-space-overflow-error
  )

(defenum (:prefix object-not- :suffix -trap :start 16)
  list
  instance)

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
;;; pfw X86 doesn't have enough registers to keep these things there.
;;;     Note these spaces grow from low to high addresses.
(defvar *allocation-pointer*)
(defvar *binding-stack-pointer*)
(defvar *x86-cgc-active-p*)
(defvar *static-blue-bag* nil)

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
      #+stack-checking kernel::yellow-zone-hit
      #+stack-checking kernel::red-zone-hit
      #+heap-overflow-check kernel::dynamic-space-overflow-warning-hit
      #+heap-overflow-check kernel::dynamic-space-overflow-error-hit
      di::handle-breakpoint
      lisp::fdefinition-object

      ;; Free Pointers.
      lisp::*read-only-space-free-pointer*
      lisp::*static-space-free-pointer*
      lisp::*initial-dynamic-space-free-pointer*

      ;; Things needed for non-local-exit.
      lisp::*current-catch-block*
      lisp::*current-unwind-protect-block*
      *eval-stack-top*
      *alien-stack*

      ;; Interrupt Handling
      lisp::*pseudo-atomic-atomic*
      lisp::*pseudo-atomic-interrupted*
      unix::*interrupts-enabled*
      unix::*interrupt-pending*
      lisp::*free-interrupt-context-index*

      *allocation-pointer*
      *binding-stack-pointer*
      *internal-gc-trigger*   ; Not used.

      ;; The FP constants
      *fp-constant-0d0*
      *fp-constant-1d0*
      *fp-constant-0s0*
      *fp-constant-1s0*
      ;; Following are all long-floats.
      *fp-constant-0l0*
      *fp-constant-1l0*
      *fp-constant-pi*
      *fp-constant-l2t*
      *fp-constant-l2e*
      *fp-constant-lg2*
      *fp-constant-ln2*

      ;; Used by gencgc.
      *scavenge-read-only-space*

      ;; Multi-process support.
      *control-stacks*

      ;; Make the ..slot-unbound.. symbol static to optimise the
      ;; common slot unbound check.
      pcl::..slot-unbound..

      ;; These are filled in the C run-time.
      lisp::*cmucl-lib*
      lisp::*cmucl-core-path*

      ;; Weak hash table support
      :key
      :value
      :key-and-value
      :key-or-value
      
      ;; Spare symbols.  Rename these when you need to add some static
      ;; symbols and don't want to do a cross-compile.
      spare-9
      spare-8
      spare-7
      spare-6
      spare-5
      spare-4
      spare-3
      spare-2
      spare-1
      
      ;; Used by CGC.
      *x86-cgc-active-p*
      ;; Foreign linkage stuff
      lisp::*linkage-table-data*
      system::*global-table*
      *current-region-free-pointer*
      *current-region-end-addr*
      *static-blue-bag*		; Must be last or change C code

      
      ))

(defparameter static-functions
  '(length
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-= eql
    %negate two-arg-and two-arg-ior two-arg-xor two-arg-gcd two-arg-lcm
    ))

;;;
;;; Stuff added by jrd ----
;;;


;;; cf the sparc PARMS.LISP
(defparameter *assembly-unit-length* 8)
