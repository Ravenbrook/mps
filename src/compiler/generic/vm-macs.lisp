;;; -*- Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/generic/vm-macs.lisp,v 1.20 2004/05/24 23:22:51 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains some macros and constants that are object-format
;;; specific or are used for defining the object format.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; 
(in-package "VM")



;;;; Other random stuff.

;;; PAD-DATA-BLOCK -- Internal Interface.
;;;
;;; This returns a form that returns a dual-word aligned number of bytes when
;;; given a number of words.
;;;
(defmacro pad-data-block (words)
  `(logandc2 (+ (ash ,words word-shift) #+amd64 15 #-amd64 lowtag-mask)
    #+amd64 15 #-amd64 lowtag-mask))

;;; DEFENUM -- Internal Interface.
;;;
(defmacro defenum ((&key (prefix "") (suffix "") (start 0) (step 1))
		   &rest identifiers)
  (let ((results nil)
	(index 0)
	(start (eval start))
	(step (eval step)))
    (dolist (id identifiers)
      (when id
	(multiple-value-bind
	    (root docs)
	    (if (consp id)
		(values (car id) (cdr id))
		(values id nil))
	  (push `(defconstant ,(intern (concatenate 'simple-string
						    (string prefix)
						    (string root)
						    (string suffix)))
		   ,(+ start (* step index))
		   ,@docs)
		results)))
      (incf index))
    `(eval-when (compile load eval)
       ,@(nreverse results))))


;;;; Primitive object definition stuff.

(export '(primitive-object primitive-object-p
	  primitive-object-name primitive-object-header
	  primitive-object-lowtag primitive-object-options
	  primitive-object-slots primitive-object-size
	  primitive-object-variable-length slot-name slot-docs slot-rest-p
	  slot-offset slot-length slot-options *primitive-objects*))


(defun remove-keywords (options keywords)
  (cond ((null options) nil)
	((member (car options) keywords)
	 (remove-keywords (cddr options) keywords))
	(t
	 (list* (car options) (cadr options)
		(remove-keywords (cddr options) keywords)))))

(defstruct (prim-object-slot
	    (:conc-name slot-)
	    (:constructor make-slot (name docs rest-p offset length options))
	    (:make-load-form-fun :just-dump-it-normally))
  (name nil :type symbol)
  (docs nil :type (or null simple-string))
  (rest-p nil :type (member t nil))
  (offset 0 :type fixnum)
  (length 1 :type fixnum)
  (options nil :type list))

(defstruct (primitive-object
	    (:make-load-form-fun :just-dump-it-normally))
  (name nil :type symbol)
  (header nil :type symbol)
  (lowtag nil :type symbol)
  (options nil :type list)
  (slots nil :type list)
  (size 0 :type fixnum)
  (variable-length nil :type (member t nil)))


(defvar *primitive-objects* nil)

(defun %define-primitive-object (primobj)
  (let ((name (primitive-object-name primobj)))
    (setf *primitive-objects*
	  (cons primobj
		(remove name *primitive-objects*
			:key #'primitive-object-name :test #'eq)))
    name))


(defmacro define-primitive-object
	  ((name &key header lowtag alloc-trans (type t))
	   &rest slot-specs)
  (collect ((slots) (exports) (constants) (forms) (inits))
    (let ((offset (if header 1 0))
	  (variable-length nil))
      (dolist (spec slot-specs)
	(when variable-length
	  (error "No more slots can follow a :rest-p slott."))
	(destructuring-bind
	    (slot-name &rest options
		       &key docs rest-p (length (if rest-p 0 1))
		       ((:type slot-type) t) init
		       (ref-known nil ref-known-p) ref-trans
		       (set-known nil set-known-p) set-trans
		       &allow-other-keys)
	    (if (atom spec) (list spec) spec)
	  (slots (make-slot slot-name docs rest-p offset length
			    (remove-keywords options
					     '(:docs :rest-p :length))))
	  (let ((offset-sym (symbolicate name "-" slot-name
					 (if rest-p "-OFFSET" "-SLOT"))))
	    (constants `(defconstant ,offset-sym ,offset
			  ,@(when docs (list docs))))
	    (exports offset-sym))
	  (when ref-trans
	    (when ref-known-p
	      (forms `(defknown ,ref-trans (,type) ,slot-type ,ref-known)))
	    (forms `(def-reffer ,ref-trans ,offset ,lowtag)))
	  (when set-trans
	    (when set-known-p
	      (forms `(defknown ,set-trans
				,(if (listp set-trans)
				     (list slot-type type)
				     (list type slot-type))
				,slot-type
			,set-known)))
	    (forms `(def-setter ,set-trans ,offset ,lowtag)))
	  (when init
	    (inits (cons init offset)))
	  (when rest-p
	    (setf variable-length t))
	  (incf offset length)))
      (unless variable-length
	(let ((size (symbolicate name "-SIZE")))
	  (constants `(defconstant ,size ,offset
			,(format nil
				 "Number of slots used by each ~S~
				  ~@[~* including the header~]."
				 name header)))
	  (exports size)))
      (when alloc-trans
	(forms `(def-alloc ,alloc-trans ,offset ,variable-length ,header
			   ,lowtag ',(inits))))
      `(progn
	 (export ',(exports))
	 (eval-when (compile load eval)
	   (%define-primitive-object
	    ',(make-primitive-object :name name
				     :header header
				     :lowtag lowtag
				     :slots (slots)
				     :size offset
				     :variable-length variable-length))
	   ,@(constants))
	 ,@(forms)))))



;;;; reffer and setter definition stuff.

(in-package :c)

(export '(def-reffer def-setter def-alloc))

(defun %def-reffer (name offset lowtag)
  (let ((info (function-info-or-lose name)))
    (setf (function-info-ir2-convert info)
	  #'(lambda (node block)
	      (ir2-convert-reffer node block name offset lowtag))))
  name)

(defmacro def-reffer (name offset lowtag)
  `(%def-reffer ',name ,offset ,lowtag))

(defun %def-setter (name offset lowtag)
  (let ((info (function-info-or-lose name)))
    (setf (function-info-ir2-convert info)
	  (if (listp name)
	      #'(lambda (node block)
		  (ir2-convert-setfer node block name offset lowtag))
	      #'(lambda (node block)
		  (ir2-convert-setter node block name offset lowtag)))))
  name)

(defmacro def-setter (name offset lowtag)
  `(%def-setter ',name ,offset ,lowtag))

(defun %def-alloc (name words variable-length header lowtag inits)
  (let ((info (function-info-or-lose name)))
    (setf (function-info-ir2-convert info)
	  (if variable-length
	      #'(lambda (node block)
		  (ir2-convert-variable-allocation node block name words header
						   lowtag inits))
	      #'(lambda (node block)
		  (ir2-convert-fixed-allocation node block name words header
						lowtag inits)))))
  name)

(defmacro def-alloc (name words variable-length header lowtag inits)
  `(%def-alloc ',name ,words ,variable-length ,header ,lowtag ,inits))


;;;; Some general constant definitions:

(in-package "C")

(export '(fasl-file-implementations
	  pmax-fasl-file-implementation
	  sparc-fasl-file-implementation
	  rt-fasl-file-implementation
	  rt-afpa-fasl-file-implementation
	  x86-fasl-file-implementation
	  hppa-fasl-file-implementation
	  big-endian-fasl-file-implementation
	  little-endian-fasl-file-implementation
	  alpha-fasl-file-implementation
	  sgi-fasl-file-implementation
	  ppc-fasl-file-implementation
	  amd64-fasl-file-implementation))

;;; Constants for the different implementations.  These are all defined in
;;; one place to make sure they are all unique.

(defparameter fasl-file-implementations
  '(nil "Pmax" "Sparc" "RT" "RT/AFPA" "x86" "HPPA"
	"Big-endian byte-code" "Little-endian byte-code" "Alpha" "SGI" "PPC" "AMD64"))
(defconstant pmax-fasl-file-implementation 1)
(defconstant sparc-fasl-file-implementation 2)
(defconstant rt-fasl-file-implementation 3)
(defconstant rt-afpa-fasl-file-implementation 4)
(defconstant x86-fasl-file-implementation 5)
(defconstant hppa-fasl-file-implementation 6)
(defconstant big-endian-fasl-file-implementation 7)
(defconstant little-endian-fasl-file-implementation 8)
(defconstant alpha-fasl-file-implementation 9)
(defconstant sgi-fasl-file-implementation 10)
(defconstant ppc-fasl-file-implementation 11)
(defconstant amd64-fasl-file-implementation 12)

;;; The maximum number of SCs in any implementation.
(defconstant sc-number-limit 32)
