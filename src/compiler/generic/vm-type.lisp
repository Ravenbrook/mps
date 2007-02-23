;;; -*- Package: KERNEL; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/vm-type.lisp,v 1.44 2006/06/30 18:41:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains implementation-dependent parts of the type support
;;; code.  This is stuff which deals with the mapping from types defined in
;;; Common Lisp to types actually supported by an implementation.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "KERNEL")


;;;; Implementation dependent deftypes:

;;; Make double-float a synonym for long-float, single-float for Short-Float.
;;; This is be expanded before the translator gets a chance, so we will get
;;; precedence.
;;;
#-long-float
(setf (info type kind 'long-float) :defined)
#-long-float
(deftype long-float (&optional low high)
  `(double-float ,low ,high))
;;;
(setf (info type kind 'short-float) :defined)
(deftype short-float (&optional low high)
  `(single-float ,low ,high))

;;;
;;; An index into an integer.
(deftype bit-index () `(integer 0 ,most-positive-fixnum))
;;;
;;; Offset argument to Ash (a signed bit index).
(deftype ash-index () 'integer)
;;;
;;; A lexical environment for macroexpansion.
(deftype lexical-environment () 'c::lexenv)
;;;
;;; Worst case values for float attributes.
;;;
(deftype float-exponent ()
  #-long-float 'double-float-exponent
  #+long-float 'long-float-exponent)
(deftype float-digits ()
  #-long-float `(integer 0 ,vm:double-float-digits)
  #+long-float `(integer 0 ,vm:long-float-digits))
(deftype float-radix () '(integer 2 2))
;;;
;;; A code for Boole.
(deftype boole-code () '(unsigned-byte 4))
;;;
;;; A byte-specifier.
(deftype byte-specifier () 'cons)
;;;
;;; Result of Char-Int...
(deftype char-int () 'char-code)
;;;
;;; Pathname pieces, as returned by the PATHNAME-xxx functions.
(deftype pathname-host () '(or lisp::host null))
(deftype pathname-device () '(or simple-string (member nil :unspecific)))
(deftype pathname-directory () 'list)
(deftype pathname-name ()
  '(or simple-string lisp::pattern (member nil :unspecific :wild)))
(deftype pathname-type ()
  '(or simple-string lisp::pattern (member nil :unspecific :wild)))
(deftype pathname-version ()
  '(or integer (member nil :newest :wild :unspecific)))
;;;
;;; Internal time format.  Not a fixnum (blag...)
(deftype internal-time () 'unsigned-byte)

(deftype bignum-element-type () `(unsigned-byte ,vm:word-bits))
(deftype bignum-type () 'bignum)
(deftype bignum-index () 'index)

#+ns-boot
(deftype structure-index () `(unsigned-byte ,(- vm:word-bits vm:type-bits)))

(deftype instance-index () `(unsigned-byte ,(- vm:word-bits vm:type-bits)))


;;;; Hooks into type system:

;;; The kinds of specialised array that actually exist in this implementation.
;;;
(defparameter specialized-array-element-types
  '(bit (unsigned-byte 2) (unsigned-byte 4) (unsigned-byte 8)
    (unsigned-byte 16) (unsigned-byte 32)
    (signed-byte 8) (signed-byte 16) (signed-byte 30) (signed-byte 32)
    (complex single-float) (complex double-float)
    #+long-float (complex long-float)
    #+double-double (complex double-double-float)
    base-char single-float double-float
    #+long-float long-float
    #+double-double double-double-float))

(deftype unboxed-array (&optional dims)
  (collect ((types (list 'or)))
    (dolist (type specialized-array-element-types)
      (when (subtypep type '(or integer character float (complex float)))
	(types `(array ,type ,dims))))
    (types)))

(deftype simple-unboxed-array (&optional dims)
  (collect ((types (list 'or)))
    (dolist (type specialized-array-element-types)
      (when (subtypep type '(or integer character float (complex float)))
	(types `(simple-array ,type ,dims))))
    (types)))


;;; Float-Format-Name  --  Internal
;;;
;;;    Return the symbol that describes the format of Float.
;;;
(defun float-format-name (x)
  (declare #+nil(float x) (values symbol))
  (etypecase x
    (single-float 'single-float)
    (double-float 'double-float)
    #+long-float (long-float 'long-float)
    #+double-double (double-double-float 'double-double-float)))

;;; Specialize-Array-Type  --  Internal
;;;
;;;    This function is called when the type code wants to find out how an
;;; array will actually be implemented.  We set the Specialized-Element-Type to
;;; correspond to the actual specialization used in this implementation.
;;;
(defun specialize-array-type (type)
  (declare (type array-type type) (values array-type))
  (let ((eltype (array-type-element-type type)))

    (setf (array-type-specialized-element-type type)
	  (if (eq eltype *wild-type*)
	      *wild-type*
	      (dolist (stype-name specialized-array-element-types
				  (specifier-type 't))
		(let ((stype (specifier-type stype-name)))
		  (when (csubtypep eltype stype)
		    (return stype))))))

    type))


;;; Contaning-Integer-Type  --  Interface
;;;
;;; Return the most specific integer type that can be quickly checked that
;;; includes the given type.
;;; 
(defun containing-integer-type (subtype)
  (dolist (type '(fixnum
		  (signed-byte 32)
		  (unsigned-byte 32)
		  bignum
		  integer)
		(error "~S isn't an integer type?" subtype))
    (when (csubtypep subtype (specifier-type type))
      (return type))))


;;; Hairy-Type-Check-Template  --  Interface
;;;
;;;    If Type has a CHECK-xxx template, but doesn't have a corresponding
;;; primitive-type, then return the template's name.  Otherwise, return NIL.
;;;
(defun hairy-type-check-template (type)
  (declare (type ctype type))
  (typecase type
    (numeric-type
     (cond ((type= type (specifier-type 'fixnum))
	    'c:check-fixnum)
	   ((type= type (specifier-type '(signed-byte 32)))
	    'c:check-signed-byte-32)
	   ((type= type (specifier-type '(unsigned-byte 32)))
	    'c:check-unsigned-byte-32)
	   (t nil)))
    (union-type
     (if (type= type (specifier-type 'bignum))
	 'c:check-bignum
	 nil))
    (cons-type
     (if (type= type (specifier-type 'cons))
	  'c:check-cons
	   nil))
    (kernel::built-in-class
     (case (%class-name type)
       (symbol 'c:check-symbol)))
    (function-type
     'c:check-function)
    (t
     nil)))
