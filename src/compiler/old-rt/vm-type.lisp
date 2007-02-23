;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains implementation-dependent parts of the type support
;;; code.  This is stuff which deals with the mapping from types defined in
;;; Common Lisp to types actually supported by an implementation.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

;;;; Implementation dependent deftypes:

;;; Make double-float a synonym for long-float, single-float for Short-Float.
;;; This is be expanded before the translator gets a chance, so we will get
;;; precedence.
;;;
(deftype double-float (&optional low high)
  `(long-float ,low ,high))
;;;
(deftype single-float (&optional low high)
  `(short-float ,low ,high))

;;; Compiled-function is the same as function in this implementation.
;;;
(deftype compiled-function () 'function)

;;;
;;; An index into an integer.
(deftype bit-index () `(integer 0 ,most-positive-fixnum))
;;;
;;; Offset argument to Ash (a signed bit index).
(deftype ash-index () 'fixnum)
;;;
;;; A lexical environment for macroexpansion.
(deftype lexical-environment () '(or list lisp::lexical-environment))
;;;
;;; A full lexical environment.
(deftype full-lexical-environment () 'lisp::lexical-environment)
;;;
;;; Worst case values for float attributes.
;;; ### long-float exponent range seems to be this, but I don't know why.
;;; Perhaps IEEE double uses some of the negative exponents for NAN, etc?
;;;
(deftype float-exponent () '(integer -1021 1024))
(deftype float-digits () '(unsigned-byte 6))
(deftype float-radix () '(integer 2 2))
;;;
;;; A code for Boole.
(deftype boole-code () '(unsigned-byte 4))
;;;
;;; A byte-specifier.
(deftype byte-specifier () 'cons)
;;;
;;; Result of Char-Int...
(deftype char-int () '(unsigned-byte 16))
;;;
;;; Legal character bit names:
(deftype bit-names () '(member :control :meta :super :hyper))
;;;
;;; Pathname pieces, as returned by the PATHNAME-xxx functions.
(deftype pathname-host () '(or simple-string null)); Host not really supported...
(deftype pathname-device () '(or simple-string (member :absolute nil)))
(deftype pathname-directory () '(or simple-vector null))
(deftype pathname-name () '(or simple-string null))
(deftype pathname-type () '(or simple-string null))
(deftype pathname-version () '(or simple-string (member nil :newest)))
;;;
;;; Internal time format.  Not a fixnum (blag...)
(deftype internal-time () 'unsigned-byte)


;;;; Hooks into type system:

;;; The kinds of specialised array that actually exist in this implementation.
;;;
(defparameter specialized-array-element-types
  '(bit (unsigned-byte 2) (unsigned-byte 4) (unsigned-byte 8) (unsigned-byte 16)
	(unsigned-byte 32) string-char))

;;; Float-Format-Name  --  Internal
;;;
;;;    Return the symbol that describes the format of Float.
;;;
(proclaim '(function float-format-name (float) symbol))
(defun float-format-name (x)
  (etypecase x
    (short-float 'short-float)
    (long-float 'long-float)))

;;; Specialize-Array-Type  --  Internal
;;;
;;;    This function is called when the type code wants to find out how an
;;; array will actually be implemented.  We set the Specialized-Element-Type to
;;; correspond to the actual specialization used in this implementation.
;;;
(proclaim '(function specialize-array-type (array-type) array-type))
(defun specialize-array-type (type)
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


;;; Hairy-Type-Check-Template  --  Interface
;;;
;;;    If Type has a CHECK-xxx template, but doesn't have a corresponding
;;; primitive-type, then return the template's name.  Otherwise, return NIL.
;;;
(defun hairy-type-check-template (type)
  (declare (type ctype type))
  (typecase type
    (named-type
     (case (named-type-name type)
       (cons 'check-cons)
       (symbol 'check-symbol)
       (t nil)))
    (union-type
     (if (type= type (specifier-type '(or function symbol)))
	 'check-function-or-symbol
	 nil))
    (t
     nil)))
