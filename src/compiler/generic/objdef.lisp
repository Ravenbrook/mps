;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/objdef.lisp,v 1.60 2006/06/30 18:41:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the machine independent aspects of the object
;;; representation.
;;;
;;; Written by William Lott.
;;;
(in-package "VM")

(export '(lowtag-bits lowtag-mask lowtag-limit type-bits type-mask
	  target-most-positive-fixnum target-most-negative-fixnum
	  even-fixnum-type function-pointer-type other-immediate-0-type
	  list-pointer-type odd-fixnum-type instance-pointer-type
	  other-immediate-1-type other-pointer-type bignum-type ratio-type
	  single-float-type double-float-type long-float-type complex-type
	  simple-array-type simple-string-type simple-bit-vector-type
	  simple-vector-type simple-array-unsigned-byte-2-type
	  simple-array-unsigned-byte-4-type simple-array-unsigned-byte-8-type
	  simple-array-unsigned-byte-16-type simple-array-unsigned-byte-32-type
	  simple-array-signed-byte-8-type simple-array-signed-byte-16-type
	  simple-array-signed-byte-30-type simple-array-signed-byte-32-type
	  simple-array-single-float-type simple-array-double-float-type
	  simple-array-long-float-type complex-string-type
	  complex-bit-vector-type complex-vector-type complex-array-type
	  code-header-type function-header-type closure-header-type
	  closure-function-header-type return-pc-header-type
	  byte-code-function-type byte-code-closure-type
	  value-cell-header-type symbol-header-type base-char-type
	  sap-type unbound-marker-type weak-pointer-type
	  instance-header-type funcallable-instance-header-type
	  fdefn-type vector-normal-subtype
	  vector-valid-hashing-subtype vector-must-rehash-subtype
	  forwarding-pointer-type scavenger-hook-type
	  complex-single-float-type complex-double-float-type
	  complex-long-float-type
	  simple-array-complex-single-float-type
	  simple-array-complex-double-float-type
	  simple-array-complex-long-float-type))

#+double-double
(export '(double-double-float double-double-float-type))

(in-package "KERNEL")
(export '(%make-funcallable-instance
	  #+gengc funcallable-instance-entry-point
	  #-gengc %funcallable-instance-function
	  %funcallable-instance-lexenv %funcallable-instance-layout))

(export '(%numerator %denominator %realpart %imagpart
	  %code-code-size %code-entry-points %code-debug-info
	  %function-self %function-next %function-name %function-arglist
	  %function-type %make-symbol symbol-hash))

(in-package "VM")


;;;; Type based constants:

(eval-when (compile eval load)

(defconstant lowtag-bits 3
  "Number of bits at the low end of a pointer used for type information.")

(defconstant lowtag-mask (1- (ash 1 lowtag-bits))
  "Mask to extract the low tag bits from a pointer.")
  
(defconstant lowtag-limit (ash 1 lowtag-bits)
  "Exclusive upper bound on the value of the low tag bits from a
  pointer.")
  
(defconstant type-bits 8
  "Number of bits used in the header word of a data block for typeing.")

(defconstant type-mask (1- (ash 1 type-bits))
  "Mask to extract the type from a header word.")

); eval-when


(defparameter target-most-positive-fixnum (1- (ash 1 #-amd64 29 #+amd64 61))
  "most-positive-fixnum in the target architecture.")

(defparameter target-most-negative-fixnum (ash -1 #-amd64 29 #+amd64 61)
  "most-negative-fixnum in the target architecture.")


;;; The main types.  These types are represented by the low three bits of the
;;; pointer or immeditate object.
;;; 
(defenum (:suffix -type)
  even-fixnum
  function-pointer
  other-immediate-0
  list-pointer
  odd-fixnum
  instance-pointer
  other-immediate-1
  other-pointer)

;;; The heap types.  Each of these types is in the header of objects in
;;; the heap.
;;; 
(defenum (:suffix -type
	  :start (+ (ash 1 lowtag-bits) other-immediate-0-type)
	  :step (ash 1 (1- lowtag-bits)))
  bignum
  ratio
  single-float
  double-float
  #+long-float long-float
  #+double-double double-double-float
  complex
  complex-single-float
  complex-double-float
  #+long-float complex-long-float
  #+double-double complex-double-double-float
  
  simple-array
  simple-string
  simple-bit-vector
  simple-vector
  simple-array-unsigned-byte-2
  simple-array-unsigned-byte-4
  simple-array-unsigned-byte-8
  simple-array-unsigned-byte-16
  simple-array-unsigned-byte-32
  simple-array-signed-byte-8
  simple-array-signed-byte-16
  simple-array-signed-byte-30
  simple-array-signed-byte-32
  simple-array-single-float
  simple-array-double-float
  #+long-float simple-array-long-float
  #+double-double simple-array-double-double-float
  simple-array-complex-single-float
  simple-array-complex-double-float
  #+long-float simple-array-complex-long-float
  #+double-double simple-array-complex-double-double-float
  complex-string
  complex-bit-vector
  complex-vector
  complex-array
  
  code-header
  function-header
  closure-header
  funcallable-instance-header
  byte-code-function
  byte-code-closure
  #-double-double dylan-function-header
  closure-function-header
  #-gengc return-pc-header
  #+gengc forwarding-pointer
  value-cell-header
  symbol-header
  base-char
  sap
  unbound-marker
  weak-pointer
  instance-header
  fdefn
  #+(or gengc gencgc) scavenger-hook
  )


;;; The different vector subtypes.
;;; 
(defenum (:prefix vector- :suffix -subtype)
  normal
  unused
  valid-hashing
  must-rehash)


;;;; The primitive objects themselves.

(define-primitive-object (cons :lowtag list-pointer-type
			       :alloc-trans cons)
  (car :ref-trans car :set-trans c::%rplaca :init :arg)
  (cdr :ref-trans cdr :set-trans c::%rplacd :init :arg))

(define-primitive-object (instance :lowtag instance-pointer-type
				    :header instance-header-type
				    :alloc-trans %make-instance)
  (slots :rest-p t))

(define-primitive-object (bignum :lowtag other-pointer-type
				 :header bignum-type
				 :alloc-trans bignum::%allocate-bignum)
  (digits :rest-p t :c-type #-alpha "long" #+alpha "u32"))

(define-primitive-object (ratio :type ratio
				:lowtag other-pointer-type
				:header ratio-type
				:alloc-trans %make-ratio)
  (numerator :type integer
	     :ref-known (flushable movable)
	     :ref-trans %numerator
	     :init :arg)
  (denominator :type integer
	       :ref-known (flushable movable)
	       :ref-trans %denominator
	       :init :arg))

(define-primitive-object (single-float :lowtag other-pointer-type
				       :header single-float-type)
  (value :c-type "float"))

(define-primitive-object (double-float :lowtag other-pointer-type
				       :header double-float-type)
  (filler)
  (value :c-type "double" :length 2))

#+long-float
(define-primitive-object (long-float :lowtag other-pointer-type
				     :header long-float-type)
  #+sparc (filler)
  (value :c-type "long double" :length #+x86 3 #+sparc 4))

(define-primitive-object (complex :type complex
				  :lowtag other-pointer-type
				  :header complex-type
				  :alloc-trans %make-complex)
  (real :type real
	:ref-known (flushable movable)
	:ref-trans %realpart
	:init :arg)
  (imag :type real
	:ref-known (flushable movable)
	:ref-trans %imagpart
	:init :arg))

(define-primitive-object (array :lowtag other-pointer-type
				:header t)
  (fill-pointer :type index
		:ref-trans %array-fill-pointer
		:ref-known (flushable foldable)
		:set-trans (setf %array-fill-pointer)
		:set-known (unsafe))
  (fill-pointer-p :type (member t nil)
		  :ref-trans %array-fill-pointer-p
		  :ref-known (flushable foldable)
		  :set-trans (setf %array-fill-pointer-p)
		  :set-known (unsafe))
  (elements :type index
	    :ref-trans %array-available-elements
	    :ref-known (flushable foldable)
	    :set-trans (setf %array-available-elements)
	    :set-known (unsafe))
  (data :type array
	:ref-trans %array-data-vector
	:ref-known (flushable foldable)
	:set-trans (setf %array-data-vector)
	:set-known (unsafe))
  (displacement :type (or index null)
		:ref-trans %array-displacement
		:ref-known (flushable foldable)
		:set-trans (setf %array-displacement)
		:set-known (unsafe))
  (displaced-p :type (member t nil)
	       :ref-trans %array-displaced-p
	       :ref-known (flushable foldable)
	       :set-trans (setf %array-displaced-p)
	       :set-known (unsafe))
  (dimensions :rest-p t))

(define-primitive-object (vector :type vector
				 :lowtag other-pointer-type
				 :header t)
  (length :ref-trans c::vector-length
	  :type index)
  (data :rest-p t :c-type #-alpha "unsigned long" #+alpha "u32"))

(define-primitive-object (code :type code-component
			       :lowtag other-pointer-type
			       :header t)
  (code-size :type index
	     :ref-known (flushable movable)
	     :ref-trans %code-code-size)
  (entry-points :type (or function null)
		:ref-known (flushable)
		:ref-trans %code-entry-points
		:set-known (unsafe)
		:set-trans (setf %code-entry-points))
  (debug-info :type t
	      :ref-known (flushable)
	      :ref-trans %code-debug-info
	      :set-known (unsafe)
	      :set-trans (setf %code-debug-info))
  (trace-table-offset)
  (constants :rest-p t))

(define-primitive-object (fdefn :type fdefn
				:lowtag other-pointer-type
				:header fdefn-type)
  (name :ref-trans fdefn-name)
  (function :type (or function null) :ref-trans fdefn-function)
  (raw-addr :c-type #-alpha "char *" #+alpha "u32"))

(define-primitive-object (function :type function
				   :lowtag function-pointer-type
				   :header function-header-type)
  #-gengc (self :ref-trans %function-self :set-trans (setf %function-self))
  #+gengc (entry-point :c-type "char *")
  (next :type (or function null)
	:ref-known (flushable)
	:ref-trans %function-next
	:set-known (unsafe)
	:set-trans (setf %function-next))
  (name :ref-known (flushable)
	:ref-trans %function-name
	:set-known (unsafe)
	:set-trans (setf %function-name))
  (arglist :ref-known (flushable)
	   :ref-trans %function-arglist
	   :set-known (unsafe)
	   :set-trans (setf %function-arglist))
  (type :ref-known (flushable)
	:ref-trans %function-type
	:set-known (unsafe)
	:set-trans (setf %function-type))
  (code :rest-p t :c-type "unsigned char"))

#-gengc
(define-primitive-object (return-pc :lowtag other-pointer-type :header t)
  (return-point :c-type "unsigned char" :rest-p t))

(define-primitive-object (closure :lowtag function-pointer-type
				  :header closure-header-type)
  #-gengc (function :init :arg :ref-trans %closure-function)
  #+gengc (entry-point :c-type "char *")
  (info :rest-p t))

(define-primitive-object (funcallable-instance
			  :lowtag function-pointer-type
			  :header funcallable-instance-header-type
			  :alloc-trans %make-funcallable-instance)
  #-gengc
  (function
   :ref-known (flushable) :ref-trans %funcallable-instance-function
   :set-known (unsafe) :set-trans (setf %funcallable-instance-function))
  #+gengc (entry-point :c-type "char *")
  (lexenv :ref-known (flushable) :ref-trans %funcallable-instance-lexenv
	  :set-known (unsafe) :set-trans (setf %funcallable-instance-lexenv))
  (layout :init :arg
	  :ref-known (flushable) :ref-trans %funcallable-instance-layout
	  :set-known (unsafe) :set-trans (setf %funcallable-instance-layout))
  (info :rest-p t))

(define-primitive-object (value-cell :lowtag other-pointer-type
				     :header value-cell-header-type
				     :alloc-trans make-value-cell)
  (value :set-trans value-cell-set
	 :set-known (unsafe)
	 :ref-trans value-cell-ref
	 :ref-known (flushable)
	 :init :arg))

#+alpha
(define-primitive-object (sap :lowtag other-pointer-type
			      :header sap-type)
  (padding)
  (pointer :c-type "char *" :length 2))

#-alpha
(define-primitive-object (sap :lowtag other-pointer-type
			      :header sap-type)
  (pointer :c-type "char *"))


(define-primitive-object (weak-pointer :type weak-pointer
				       :lowtag other-pointer-type
				       :header weak-pointer-type
				       :alloc-trans make-weak-pointer)
  (value :ref-trans c::%weak-pointer-value :ref-known (flushable)
	 :set-trans c::%set-weak-pointer-value :set-known (unsafe)
	 :init :arg)
  (broken :type (member t nil)
	  :ref-trans c::%weak-pointer-broken :ref-known (flushable)
	  :set-trans c::%set-weak-pointer-broken :set-known (unsafe)
	  :init :null)
  ;; This is used in gencgc.c to note if we've visited this weak
  ;; pointer before, so that the scavenging weak-pointers isn't an
  ;; O(n^2) process.
  #+gencgc
  (mark-bit :type (member t nil)
	    :init :null)
  (next :c-type #-alpha "struct weak_pointer *" #+alpha "u32"))

#+(or gengc gencgc)
(define-primitive-object (scavenger-hook :type scavenger-hook
					 :lowtag other-pointer-type
					 :header scavenger-hook-type
					 :alloc-trans c::%make-scavenger-hook)
  (value :ref-trans scavenger-hook-value :ref-known (flushable)
	 :init :arg)
  (function :ref-trans scavenger-hook-function :ref-known (flushable)
	    :set-trans (setf scavenger-hook-function) :set-known (unsafe)
	    :init :arg)
  (next :c-type "struct scavenger_hook *"))

;;; Other non-heap data blocks.

(define-primitive-object (binding)
  value
  symbol)

(define-primitive-object (unwind-block)
  (current-uwp :c-type #-alpha "struct unwind_block *" #+alpha "u32")
  (current-cont :c-type #-alpha "lispobj *" #+alpha "u32")
  #-x86 current-code
  entry-pc)

(define-primitive-object (catch-block)
  (current-uwp :c-type #-alpha "struct unwind_block *" #+alpha "u32")
  (current-cont :c-type #-alpha "lispobj *" #+alpha "u32")
  #-x86 current-code
  entry-pc
  tag
  (previous-catch :c-type #-alpha "struct catch_block *" #+alpha "u32")
  size)

#+gengc
(define-primitive-object (mutator)
  ;; Holds the lisp thread structure, if any.
  (thread)
  ;; Signal control magic.
  (foreign-fn-call-active :c-type "boolean")
  (interrupts-disabled-count :c-type "int")
  (interrupt-pending :c-type "boolean")
  (pending-signal :c-type "int")
  (pending-code :c-type "int")
  (pending-mask :c-type "int")
  (gc-pending :c-type "boolean")
  ;; Stacks.
  (control-stack-base :c-type "lispobj *")
  (control-stack-pointer :c-type "lispobj *")
  (control-stack-end :c-type "lispobj *")
  (control-frame-pointer :c-type "lispobj *")
  (current-unwind-protect :c-type "struct unwind_block *")
  (current-catch-block :c-type "struct catch_block *")
  (binding-stack-base :c-type "struct binding *")
  (binding-stack-pointer :c-type "struct binding *")
  (binding-stack-end :c-type "struct binding *")
  (number-stack-base :c-type "char *")
  (number-stack-pointer :c-type "char *")
  (number-stack-end :c-type "char *")
  (eval-stack)
  (eval-stack-top)
  ;; Allocation stuff.
  (nursery-start :c-type "lispobj *")
  (nursery-fill-pointer :c-type "lispobj *")
  (nursery-end :c-type "lispobj *")
  (storebuf-start :c-type "lispobj **")
  (storebuf-fill-pointer :c-type "lispobj **")
  (storebuf-end :c-type "lispobj **")
  (words-consed :c-type "unsigned long"))



;;;; Symbols

#+(or gengc sparc x86 amd64 ppc)
(defknown %make-symbol (fixnum simple-string) symbol
  (flushable movable))

#+(or gengc sparc x86 amd64 ppc)
(defknown symbol-hash (symbol) fixnum
  (flushable movable))

#+(or gencgc sparc x86 amd64 ppc)
(defknown %set-symbol-hash  (symbol index)
  t (unsafe))

#+(and nil x86)
(defknown symbol-hash (symbol) lisp::hash
  (flushable movable))

#+amd64
(defknown %symbol-name (symbol) simple-string (movable foldable flushable))

(define-primitive-object (symbol :lowtag other-pointer-type
				 :header symbol-header-type
				 :alloc-trans
				 #-(or gengc x86 amd64 sparc ppc) make-symbol
				 #+(or gengc x86 amd64 sparc ppc) %make-symbol)
  (value :set-trans %set-symbol-value
	 :init :unbound)
  #-(or gengc x86 amd64 sparc ppc) unused
  #+(or gengc x86 amd64 sparc ppc)
  (hash :init :arg)
  (plist :ref-trans symbol-plist
	 :set-trans %set-symbol-plist
	 :init :null)
  (name :ref-trans #-amd64 symbol-name #+amd64 %symbol-name :init :arg)
  (package :ref-trans symbol-package
	   :set-trans %set-symbol-package
	   :init :null))

;; We couldn't use the nil/symbol trick in 32-bit, because the difference
;; between the low tags of nil and symbol is 4 but the word-size is 8. This is
;; a slow workaround.
#+amd64
(deftransform symbol-name ((symbol) (*))
	      '(if (eq symbol nil)
		"NIL"
		(%symbol-name symbol)))

(define-primitive-object (complex-single-float
			  :lowtag other-pointer-type
			  :header complex-single-float-type)
  (real :c-type "float")
  (imag :c-type "float"))

(define-primitive-object (complex-double-float
			  :lowtag other-pointer-type
			  :header complex-double-float-type)
  (filler)
  (real :c-type "double" :length 2)
  (imag :c-type "double" :length 2))

#+long-float
(define-primitive-object (complex-long-float
			  :lowtag other-pointer-type
			  :header complex-long-float-type)
  #+sparc (filler)
  (real :c-type "long double" :length #+x86 3 #+sparc 4)
  (imag :c-type "long double" :length #+x86 3 #+sparc 4))

#+double-double
(define-primitive-object (double-double-float
			  :lowtag other-pointer-type
			  :header double-double-float-type)
  (filler)
  (hi :c-type "double" :length 2)
  (lo :c-type "double" :length 2))

#+double-double
(define-primitive-object (complex-double-double-float
			  :lowtag other-pointer-type
			  :header complex-double-double-float-type)
  (filler)
  (real-hi :c-type "double" :length 2)
  (real-lo :c-type "double" :length 2)
  (imag-hi :c-type "double" :length 2)
  (imag-lo :c-type "double" :length 2))
