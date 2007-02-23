;;; -*- Package: DISASSEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/disassem.lisp,v 1.53 2006/05/18 17:04:54 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Machine independent disassembler for CMU Common Lisp
;;;
;;; Written by Miles Bader <miles@cogsci.ed.ac.uk>
;;;

(in-package :disassem)

(use-package :extensions)

(export '(;; for defining the instruction set
	  set-disassem-params
	  define-argument-type
	  define-instruction-format
	  install-inst-flavors

	  ;; macroexpanders
	  gen-preamble-form gen-clear-info-form
	  gen-arg-type-def-form gen-format-def-form
	  gen-printer-def-forms-def-form

	  ;; main user entry-points
	  disassemble
	  disassemble-memory
	  disassemble-function
	  disassemble-code-component
	  disassemble-assem-segment

	  ;; some variables to set
	  *opcode-column-width*
	  *note-column*

	  ;; slightly lower level entry-points
	  make-dstate
	  get-function-segments get-code-segments
	  label-segments disassemble-segments disassemble-segment
	  map-segment-instructions
	  segment-overflow
	  set-location-printing-range
	  add-offs-hook add-offs-note-hook add-offs-comment-hook
	  *default-dstate-hooks*
	  make-segment make-code-segment make-vector-segment make-memory-segment
	  make-offs-hook

	  ;; segment type
	  segment seg-sap-maker seg-length seg-virtual-location

	  ;; decoding a bit-pattern
	  sap-ref-dchunk
	  get-inst-space
	  find-inst

	  ;; getting at the dstate (usually from mach-dep code)
	  disassem-state dstate-cur-offs dstate-next-offs
	  dstate-segment dstate-segment-sap
	  dstate-get-prop
	  dstate-cur-addr dstate-next-addr

	  ;; random types
	  dchunk params instruction

	  ;; 
	  read-suffix read-signed-suffix
	  sign-extend

	  ;; useful for printers
	  princ16

	  ;; making handy margin notes
	  note
	  note-code-constant
	  maybe-note-nil-indexed-symbol-slot-ref
	  maybe-note-nil-indexed-object
	  maybe-note-assembler-routine
	  maybe-note-static-function
	  maybe-note-single-storage-ref
	  maybe-note-associated-storage-ref
	  handle-break-args

	  ;; taking over and printing...
	  print-notes-and-newline
	  print-current-address
	  print-bytes print-words
	  prin1-short
	  prin1-quoted-short
	  ))

;;; ----------------------------------------------------------------

(defvar *opcode-column-width* nil
  "The width of the column in which instruction-names are printed.
  NIL means use the default.  A value of zero gives the effect of not
  aligning the arguments at all.")
(defvar *note-column* 45
  "The column in which end-of-line comments for notes are started.")

(defconstant default-opcode-column-width 6)
(defconstant default-location-column-width 8)
(defconstant label-column-width 7)

(deftype address () '(unsigned-byte 32))
(deftype alignment () '(integer 0 64))
(deftype offset () '(signed-byte 24))
(deftype length () '(unsigned-byte 24))

(deftype column () '(integer 0 1000))
(deftype text-width () '(integer 0 1000))

(defconstant max-filtered-value-index 32)
(deftype filtered-value-index ()
  `(integer 0 ,max-filtered-value-index))
(deftype filtered-value-vector ()
  `(simple-array t (,max-filtered-value-index)))

;;; ----------------------------------------------------------------

(defmacro set-disassem-params (&rest args)
  "Specify global disassembler params for C:*TARGET-BACKEND*.
  Keyword arguments include:
      
  :INSTRUCTION-ALIGNMENT number
      Minimum alignment of instructions, in bits.
      
  :ADDRESS-SIZE number
      Size of a machine address, in bits.
      
  :OPCODE-COLUMN-WIDTH
      Width of the column used for printing the opcode portion of the
      instruction, or NIL to use the default."
  (gen-preamble-form args))

(defmacro define-argument-type (name &rest args)
  "DEFINE-ARGUMENT-TYPE Name {Key Value}*
  Define a disassembler argument type NAME (which can then be referenced in
  another argument definition using the :TYPE keyword argument).  Keyword
  arguments are:

  :SIGN-EXTEND boolean
      If non-NIL, the raw value of this argument is sign-extended.

  :TYPE arg-type-name
      Inherit any properties of given argument-type.

  :PREFILTER function
      A function which is called (along with all other prefilters, in the
      order that their arguments appear in the instruction- format) before
      any printing is done, to filter the raw value.  Any uses of READ-SUFFIX
      must be done inside a prefilter.
      
  :PRINTER function-string-or-vector
      A function, string, or vector which is used to print an argument of
      this type.
      
  :USE-LABEL 
      If non-NIL, the value of an argument of this type is used as an
      address, and if that address occurs inside the disassembled code, it is
      replaced by a label.  If this is a function, it is called to filter the
      value."
  (gen-arg-type-def-form name args))

(defmacro define-instruction-format (header &rest fields)
  "DEFINE-INSTRUCTION-FORMAT (Name Length {Format-Key Value}*) Arg-Def*
  Define an instruction format NAME for the disassembler's use.  LENGTH is
  the length of the format in bits.
  Possible FORMAT-KEYs:

  :INCLUDE other-format-name
      Inherit all arguments and properties of the given format.  Any
      arguments defined in the current format definition will either modify
      the copy of an existing argument (keeping in the same order with
      respect to when pre-filter's are called), if it has the same name as
      one, or be added to the end.
  :DEFAULT-PRINTER printer-list
      Use the given PRINTER-LIST as a format to print any instructions of
      this format when they don't specify something else.

  Each ARG-DEF defines one argument in the format, and is of the form
    (Arg-Name {Arg-Key Value}*)

  Possible ARG-KEYs (the values are evaulated unless otherwise specified):
  
  :FIELDS byte-spec-list
      The argument takes values from these fields in the instruction.  If
      the list is of length one, then the corresponding value is supplied by
      itself; otherwise it is a list of the values.  The list may be NIL.
  :FIELD byte-spec
      The same as :FIELDS (list byte-spec).

  :VALUE value
      If the argument only has one field, this is the value it should have,
      otherwise it's a list of the values of the individual fields.  This can
      be overridden in an instruction-definition or a format definition
      including this one by specifying another, or NIL to indicate that it's
      variable.

  :SIGN-EXTEND boolean
      If non-NIL, the raw value of this argument is sign-extended,
      immediately after being extracted from the instruction (before any
      prefilters are run, for instance).  If the argument has multiple
      fields, they are all sign-extended.

  :TYPE arg-type-name
      Inherit any properties of the given argument-type.

  :PREFILTER function
      A function which is called (along with all other prefilters, in the
      order that their arguments appear in the instruction-format) before
      any printing is done, to filter the raw value.  Any uses of READ-SUFFIX
      must be done inside a prefilter.

  :PRINTER function-string-or-vector
      A function, string, or vector which is used to print this argument.
      
  :USE-LABEL 
      If non-NIL, the value of this argument is used as an address, and if
      that address occurs inside the disassembled code, it is replaced by a
      label.  If this is a function, it is called to filter the value."
  (gen-format-def-form header fields))

;;; ----------------------------------------------------------------

(declaim (inline bytes-to-bits)
	 (maybe-inline sign-extend aligned-p align tab tab0))

(defun bytes-to-bits (bytes)
  (declare (type length bytes))
  (* bytes vm:byte-bits))

(defun bits-to-bytes (bits)
  (declare (type length bits))
  (multiple-value-bind (bytes rbits)
      (truncate bits vm:byte-bits)
    (when (not (zerop rbits))
      (error "~d bits is not a byte-multiple" bits))
    bytes))

(defun sign-extend (int size)
  (declare (type integer int)
	   (type (integer 0 128) size))
  (if (logbitp (1- size) int)
      (dpb int (byte size 0) -1)
      int))

(defun aligned-p (address size)
  "Returns non-NIL if ADDRESS is aligned on a SIZE byte boundary."
  (declare (type address address)
	   (type alignment size))
  (zerop (logand (1- size) address)))

(defun align (address size)
  "Return ADDRESS aligned *upward* to a SIZE byte boundary."
  (declare (type address address)
	   (type alignment size))
  (logandc1 (1- size) (+ (1- size) address)))

(defun tab (column stream)
  (funcall (formatter "~v,1t") stream column)
  nil)
(defun tab0 (column stream)
  (funcall (formatter "~v,0t") stream column)
  nil)

(defun princ16 (value stream)
  (write value :stream stream :radix t :base 16 :escape nil))

;;; ----------------------------------------------------------------

(defun self-evaluating-p (x)
  (typecase x
    (null t)
    (keyword t)
    (symbol (eq x t))
    (cons nil)
    (t t)))

;;; ----------------------------------------------------------------
;;; Some simple functions that help avoid consing when we're just
;;; recursively filtering things that usually don't change.

(defun sharing-cons (old-cons car cdr)
  "If CAR is eq to the car of OLD-CONS and CDR is eq to the CDR, return
  OLD-CONS, otherwise return (cons CAR CDR)."
  (if (and (eq car (car old-cons)) (eq cdr (cdr old-cons)))
      old-cons
      (cons car cdr)))

(defun sharing-mapcar (fun list)
  "A simple (one list arg) mapcar that avoids consing up a new list
  as long as the results of calling FUN on the elements of LIST are
  eq to the original."
  (and list
       (sharing-cons list
		     (funcall fun (car list))
		     (sharing-mapcar fun (cdr list)))))

;;; ----------------------------------------------------------------
;;; A Dchunk contains the bits we look at to decode an
;;; instruction.
;;; I tried to keep this abstract so that if using integers > the machine
;;; word size conses too much, it can be changed to use bit-vectors or
;;; something.

(declaim (inline dchunk-or dchunk-and dchunk-clear dchunk-not
		 dchunk-make-mask dchunk-make-field
		 sap-ref-dchunk
		 dchunk-extract
		 dchunk=
		 dchunk-count-bits))

(defconstant dchunk-bits 32)

(deftype dchunk ()
  `(unsigned-byte ,dchunk-bits))
(deftype dchunk-index ()
  `(integer 0 ,dchunk-bits))

(defconstant dchunk-zero 0)
(defconstant dchunk-one #xFFFFFFFF)

(defmacro dchunk-copy (x)
  `(the dchunk ,x))

(defun dchunk-or (to from)
  (declare (type dchunk to from))
  (the dchunk (logior to from)))
(defun dchunk-and (to from)
  (declare (type dchunk to from))
  (the dchunk (logand to from)))
(defun dchunk-clear (to from)
  (declare (type dchunk to from))
  (the dchunk (logandc2 to from)))
(defun dchunk-not (from)
  (declare (type dchunk from))
  (the dchunk (logand dchunk-one (lognot from))))

(defmacro dchunk-andf (to from)
  `(setf ,to (dchunk-and ,to ,from)))
(defmacro dchunk-orf (to from)
  `(setf ,to (dchunk-or ,to ,from)))
(defmacro dchunk-clearf (to from)
  `(setf ,to (dchunk-clear ,to ,from)))

(defun dchunk-make-mask (pos)
  (the dchunk (mask-field pos -1)))
(defun dchunk-make-field (pos value)
  (the dchunk (dpb value pos 0)))

(defmacro make-dchunk (value)
  `(the dchunk ,value))

(defun sap-ref-dchunk (sap byte-offset byte-order)
  (declare (type system:system-area-pointer sap)
	   (type offset byte-offset)
	   (optimize (speed 3) (safety 0)))
  (the dchunk
       (if (eq byte-order :big-endian)
	   (+ (ash (system:sap-ref-8 sap byte-offset) 24)
	      (ash (system:sap-ref-8 sap (+ 1 byte-offset)) 16)
	      (ash (system:sap-ref-8 sap (+ 2 byte-offset)) 8)
	      (system:sap-ref-8 sap (+ 3 byte-offset)))
	   (+ (system:sap-ref-8 sap byte-offset)
	      (ash (system:sap-ref-8 sap (+ 1 byte-offset)) 8)
	      (ash (system:sap-ref-8 sap (+ 2 byte-offset)) 16)
	      (ash (system:sap-ref-8 sap (+ 3 byte-offset)) 24)))))

(defun correct-dchunk-bytespec-for-endianness (bs unit-bits byte-order)
  (if (eq byte-order :big-endian)
      (byte (byte-size bs) (+ (byte-position bs) (- dchunk-bits unit-bits)))
      bs))

(defun dchunk-extract (from pos)
  (declare (type dchunk from))
  (the dchunk (ldb pos (the dchunk from))))

(defun dchunk-corrected-extract (from pos unit-bits byte-order)
  (declare (type dchunk from))
  (if (eq byte-order :big-endian)
      (ldb (byte (byte-size pos)
		 (+ (byte-position pos) (- dchunk-bits unit-bits)))
	   (the dchunk from))
      (ldb pos (the dchunk from))))

(defmacro dchunk-insertf (place pos value)
  `(setf ,place (the dchunk (dpb ,value ,pos (the dchunk,place)))))

(defun dchunk= (x y)
  (declare (type dchunk x y))
  (= x y))
(defmacro dchunk-zerop (x)
  `(dchunk= ,x dchunk-zero))

(defun dchunk-strict-superset-p (sup sub)
  (and (zerop (logandc2 sub sup))
       (not (zerop (logandc2 sup sub)))))

(defun dchunk-count-bits (x)
  (declare (type dchunk x))
  (logcount x))

;;; ----------------------------------------------------------------

(defstruct (params (:print-function %print-params))
  (instructions (make-hash-table :test #'eq) :type hash-table)
  (inst-space nil :type (or null inst-space))
  (instruction-alignment vm:word-bytes :type alignment)
  (location-column-width default-location-column-width :type text-width)
  (opcode-column-width default-opcode-column-width :type (or null text-width))
  (backend (required-argument) :type c::backend) ; for convenience
  )

(defun %print-params (params stream level)
  (declare (ignore level))
  (print-unreadable-object (params stream :type t)
    (when (params-backend params)
      (prin1 (c:backend-name (params-backend params)) stream))))

;;; ----------------------------------------------------------------
;;; Only used during compilation of the instructions for a backend

(defstruct (argument (:conc-name arg-))
  (name nil :type symbol)
  (fields nil :type list)

  (value nil :type (or list integer))
  (sign-extend-p nil :type (member t nil))

  ;; position in a vector of prefiltered values
  (position 0 :type fixnum)

  ;; functions to use
  (printer nil)
  (prefilter nil)
  (use-label nil)
  )

(defstruct (instruction-format (:conc-name format-))
  (name nil)
  (args nil :type list)

  (length 0 :type length)		; in bytes

  (default-printer nil :type list)
  )

;;; ----------------------------------------------------------------
;;; 

(defstruct (instruction (:conc-name inst-)
			(:print-function %print-instruction)
			(:constructor
			 make-instruction (name
					   format-name
					   print-name
					   length
					   mask id
					   printer
					   labeller prefilter control)))
  (name nil :type (or symbol string))
  (format-name nil :type (or symbol string))

  (mask dchunk-zero :type dchunk)	; bits in the inst that are constant
  (id dchunk-zero :type dchunk)		; value of those constant bits

  (length 0 :type length)		; in bytes

  (print-name nil :type symbol)

  ;; disassembly functions
  (prefilter nil :type (or null function))
  (labeller nil :type (or null function))
  (printer (required-argument) :type (or null function))
  (control nil :type (or null function))

  ;; instructions that are the same as this instruction but with more
  ;; constraints
  (specializers nil :type list)
  )

(defun %print-instruction (inst stream depth)
  (declare (ignore depth))
  (print-unreadable-object (inst stream :type t :identity t)
    (format stream "~a(~a)" (inst-name inst) (inst-format-name inst))))

;;; ----------------------------------------------------------------
;;; provide more meaningful error messages during compilation

(defvar *current-instruction-flavor* nil)

(defun pd-error (fmt &rest args)
  (if *current-instruction-flavor*
      (error "~@<In printer-definition for ~s(~s):  ~3i~:_~?~:>"
	     (car *current-instruction-flavor*)
	     (cdr *current-instruction-flavor*)
	     fmt args)
      (apply #'error fmt args)))

;;; ----------------------------------------------------------------
;;; Since we can't include some values in compiled output as they are
;;; (notably functions), we sometimes use a valsrc structure to keep track of
;;; the source from which they were derived.

(defstruct (valsrc (:constructor %make-valsrc))
  (value nil)
  (source nil))

;;; Returns a version of THING suitable for including in an evaluable
;;; position in some form.
(defun source-form (thing)
  (cond ((valsrc-p thing)
	 (valsrc-source thing))
	((functionp thing)
	 (pd-error
	  "Can't dump functions, so function ref form must be quoted: ~s"
	  thing))
	((self-evaluating-p thing)
	 thing)
	((eq (car thing) 'function)
	 thing)
	(t
	 `',thing)))

;;; Returns anything but a valsrc structure.
(defun value-or-source (thing)
  (if (valsrc-p thing)
      (valsrc-value thing)
      thing))

(defun make-valsrc (value source)
  (cond ((equal value source)
	 source)
	((and (listp value) (eq (car value) 'function))
	 value)
	(t
	 (%make-valsrc :value value :source source))))

;;; ----------------------------------------------------------------
;;; A funstate holds the state of any arguments used in a disassembly
;;; function.

(defstruct (funstate (:conc-name funstate-) (:constructor %make-funstate))
  (args nil :type list)
  (arg-temps nil :type list)		; see below
  )

(defun make-funstate (args)
  ;; give the args a position
  (let ((i 0))
    (dolist (arg args)
      (setf (arg-position arg) i)
      (incf i)))
  (%make-funstate :args args))

(defun arg-or-lose (name funstate)
  (let ((arg (find name (funstate-args funstate) :key #'arg-name)))
    (when (null arg)
      (pd-error "Unknown argument ~s" name))
    arg))

(defun get-arg-temp (arg kind funstate)
  (let ((this-arg-temps (assoc arg (funstate-arg-temps funstate))))
    (if this-arg-temps
	(let ((this-kind-temps
	       (assoc (canonicalize-arg-form-kind kind)
		      (cdr this-arg-temps))))
	  (values (cadr this-kind-temps) (cddr this-kind-temps)))
	(values nil nil))))

(defun make-arg-temp-bindings (funstate)
  ;; Everything is in reverse order, so we just use push, which results in
  ;; everything being in the right order at the end.
  (let ((bindings nil))
    (dolist (ats (funstate-arg-temps funstate))
      (dolist (atk (cdr ats))
	(cond ((null (cadr atk)))
	      ((atom (cadr atk))
	       (push `(,(cadr atk) ,(cddr atk)) bindings))
	      (t
	       (mapc #'(lambda (var form)
			 (push `(,var ,form) bindings))
		     (cadr atk)
		     (cddr atk))))))
    bindings))

(defun set-arg-temps (vars forms arg kind funstate)
  (let ((this-arg-temps
	 (or (assoc arg (funstate-arg-temps funstate))
	     (car (push (cons arg nil) (funstate-arg-temps funstate)))))
	(kind (canonicalize-arg-form-kind kind)))
    (let ((this-kind-temps
	   (or (assoc kind (cdr this-arg-temps))
	       (car (push (cons kind nil) (cdr this-arg-temps))))))
      (setf (cdr this-kind-temps) (cons vars forms)))))

(defun gen-arg-forms (arg kind funstate)
  (multiple-value-bind (vars forms)
      (get-arg-temp arg kind funstate)
    (when (null forms)
      (multiple-value-bind (new-forms single-value-p)
	  (funcall (find-arg-form-producer kind) arg funstate)
	(setq forms new-forms)
	(cond ((or single-value-p (atom forms))
	       (unless (symbolp forms)
		 (setq vars (gensym))))
	      ((every #'symbolp forms)
	       ;; just use the same as the forms
	       (setq vars nil))
	      (t
	       (setq vars nil)
	       (dotimes (i (length forms))
		 (push (gensym) vars))))
	(set-arg-temps vars forms arg kind funstate)))
    (or vars forms)))

(defun funstate-compatible-p (funstate args)
  (every #'(lambda (this-arg-temps)
	     (let* ((old-arg (car this-arg-temps))
		    (new-arg (find (arg-name old-arg) args :key #'arg-name)))
	       (and new-arg
		    (every #'(lambda (this-kind-temps)
			       (funcall (find-arg-form-checker
					 (car this-kind-temps))
					new-arg
					old-arg))
			   (cdr this-arg-temps)))))
	 (funstate-arg-temps funstate)))

(defun maybe-listify (forms)
  (cond ((atom forms)
	 forms)
	((/= (length forms) 1)
	 `(list ,@forms))
	(t
	 (car forms))))

(defun arg-value-form (arg funstate
		       &optional
		       (kind :final)
		       (allow-multiple-p (not (eq kind :numeric))))
  (let ((forms (gen-arg-forms arg kind funstate)))
    (when (and (not allow-multiple-p)
	       (listp forms)
	       (/= (length forms) 1))
      (pd-error "~s must not have multiple values" arg))
    (maybe-listify forms)))

;;; ----------------------------------------------------------------
;;; These are the kind of values we can compute for an argument, and
;;; how to compute them.  The :checker functions make sure that a given
;;; argument is compatible with another argument for a given use.

(defvar *arg-form-kinds* nil)

(defstruct arg-form-kind
  (names nil :type list)
  (producer (required-argument) :type function)
  (checker (required-argument) :type function)
  )

(defun arg-form-kind-or-lose (kind)
  (or (getf *arg-form-kinds* kind)
      (pd-error "Unknown arg-form kind ~s" kind)))

(defun find-arg-form-producer (kind)
  (arg-form-kind-producer (arg-form-kind-or-lose kind)))
(defun find-arg-form-checker (kind)
  (arg-form-kind-checker (arg-form-kind-or-lose kind)))

(defun canonicalize-arg-form-kind (kind)
  (car (arg-form-kind-names (arg-form-kind-or-lose kind))))

(defmacro def-arg-form-kind ((&rest names) &rest inits)
  `(let ((kind (make-arg-form-kind :names ',names ,@inits)))
     ,@(mapcar #'(lambda (name)
		   `(setf (getf *arg-form-kinds* ',name) kind))
	       names)))

(def-arg-form-kind (:raw)
  :producer #'(lambda (arg funstate)
		(declare (ignore funstate))
		(mapcar #'(lambda (bytespec)
			    `(the (unsigned-byte ,(byte-size bytespec))
				  (local-extract ',bytespec)))
			(arg-fields arg)))
  :checker #'(lambda (new-arg old-arg)
	       (equal (arg-fields new-arg)
		      (arg-fields old-arg))))

(def-arg-form-kind (:sign-extended :unfiltered)
  :producer #'(lambda (arg funstate)
		(let ((raw-forms (gen-arg-forms arg :raw funstate)))
		  (if (and (arg-sign-extend-p arg) (listp raw-forms))
		      (mapcar #'(lambda (form field)
				  `(the (signed-byte ,(byte-size field))
					(sign-extend ,form
						     ,(byte-size field))))
			      raw-forms
			      (arg-fields arg))
		      raw-forms)))
  :checker #'(lambda (new-arg old-arg)
	       (equal (arg-sign-extend-p new-arg)
		      (arg-sign-extend-p old-arg))))

(defun valsrc-equal (f1 f2)
  (if (null f1)
      (null f2)
      (equal (value-or-source f1)
	     (value-or-source f2))))

(def-arg-form-kind (:filtering)
  :producer #'(lambda (arg funstate)
		(let ((sign-extended-forms
		       (gen-arg-forms arg :sign-extended funstate))
		      (pf (arg-prefilter arg)))
		  (if pf
		      (values
		       `(local-filter ,(maybe-listify sign-extended-forms)
				      ,(source-form pf))
		       t)
		      (values sign-extended-forms nil))))
  :checker #'(lambda (new-arg old-arg)
	       (valsrc-equal (arg-prefilter new-arg) (arg-prefilter old-arg))))

(def-arg-form-kind (:filtered :unadjusted)
  :producer #'(lambda (arg funstate)
		(let ((pf (arg-prefilter arg)))
		  (if pf
		      (values `(local-filtered-value ,(arg-position arg)) t)
		      (gen-arg-forms arg :sign-extended funstate))))
  :checker #'(lambda (new-arg old-arg)
	       (let ((pf1 (arg-prefilter new-arg))
		     (pf2 (arg-prefilter old-arg)))
		 (if (null pf1)
		     (null pf2)
		     (= (arg-position new-arg)
			(arg-position old-arg))))))

(def-arg-form-kind (:adjusted :numeric :unlabelled)
  :producer #'(lambda (arg funstate)
		(let ((filtered-forms (gen-arg-forms arg :filtered funstate))
		      (use-label (arg-use-label arg)))
		  (if (and use-label (not (eq use-label t)))
		      (list
		       `(adjust-label ,(maybe-listify filtered-forms)
				      ,(source-form use-label)))
		      filtered-forms)))
  :checker #'(lambda (new-arg old-arg)
	       (valsrc-equal (arg-use-label new-arg) (arg-use-label old-arg))))

(def-arg-form-kind (:labelled :final)
  :producer #'(lambda (arg funstate)
		(let ((adjusted-forms
		       (gen-arg-forms arg :adjusted funstate))
		      (use-label (arg-use-label arg)))
		  (if use-label
		      (let ((form (maybe-listify adjusted-forms)))
			(if (and (not (eq use-label t))
				 (not (atom adjusted-forms))
				 (/= (Length adjusted-forms) 1))
			    (pd-error
			     "Cannot label a multiple-field argument ~
			      unless using a function: ~s" arg)
			    `((lookup-label ,form))))
		      adjusted-forms)))
  :checker #'(lambda (new-arg old-arg)
	       (let ((lf1 (arg-use-label new-arg))
		     (lf2 (arg-use-label old-arg)))
		 (if (null lf1) (null lf2) t))))

;;; This is a bogus kind that's just used to ensure that printers are
;;; compatible...
(def-arg-form-kind (:printed)
  :producer #'(lambda (&rest noise)
		(declare (ignore noise))
		(pd-error "Bogus!  Can't use the :printed value of an arg!"))
  :checker #'(lambda (new-arg old-arg)
	       (valsrc-equal (arg-printer new-arg) (arg-printer old-arg))))

(defun remember-printer-use (arg funstate)
  (set-arg-temps nil nil arg :printed funstate))

;;; ----------------------------------------------------------------

(defun compare-fields-form (val-form-1 val-form-2)
  (flet ((listify-fields (fields)
	   (cond ((symbolp fields) fields)
		 ((every #'constantp fields) `',fields)
		 (t `(list ,@fields)))))
    (cond ((or (symbolp val-form-1) (symbolp val-form-2))
	   `(equal ,(listify-fields val-form-1)
		   ,(listify-fields val-form-2)))
	  (t
	   `(and ,@(mapcar #'(lambda (v1 v2) `(= ,v1 ,v2))
			   val-form-1 val-form-2))))))

(defun compile-test (subj test funstate)
  (when (and (consp test) (symbolp (car test)) (not (keywordp (car test))))
    (setf subj (car test)
	  test (cdr test)))
  (let ((key (if (consp test) (car test) test))
	(body (if (consp test) (cdr test) nil)))
    (cond ((null key)
	   nil)
	  ((eq key t)
	   t)
	  ((eq key :constant)
	   (let* ((arg (arg-or-lose subj funstate))
		  (fields (arg-fields arg))
		  (consts body))
	     (when (not (= (length fields) (length consts)))
	       (pd-error "number of constants doesn't match number of fields ~
			  in: (~s :constant~{ ~s~})"
			 subj body))
	     (compare-fields-form (gen-arg-forms arg :numeric funstate)
				  consts)))
	  ((eq key :positive)
	   `(> ,(arg-value-form (arg-or-lose subj funstate) funstate :numeric)
	       0))
	  ((eq key :negative)
	   `(< ,(arg-value-form (arg-or-lose subj funstate) funstate :numeric)
	       0))
	  ((eq key :same-as)
	   (let ((arg1 (arg-or-lose subj funstate))
		 (arg2 (arg-or-lose (car body) funstate)))
	     (unless (and (= (length (arg-fields arg1))
			     (length (arg-fields arg2)))
			  (every #'(lambda (bs1 bs2)
				     (= (byte-size bs1) (byte-size bs2)))
				 (arg-fields arg1)
				 (arg-fields arg2)))
	       (pd-error "Can't compare differently sized fields: ~
		          (~s :same-as ~s)" subj (car body)))
	     (compare-fields-form (gen-arg-forms arg1 :numeric funstate)
				  (gen-arg-forms arg2 :numeric funstate))))
	  ((eq key :or)
	   `(or ,@(mapcar #'(lambda (sub) (compile-test subj sub funstate))
			  body)))
	  ((eq key :and)
	   `(and ,@(mapcar #'(lambda (sub) (compile-test subj sub funstate))
			   body)))
	  ((eq key :not)
	   `(not ,(compile-test subj (car body) funstate)))
	  ((and (consp key) (null body))
	   (compile-test subj key funstate))
	  (t
	   (pd-error "Bogus test-form: ~s" test)))))

;;; ----------------------------------------------------------------

(defun find-first-field-name (tree)
  "Returns the first non-keyword symbol in a depth-first search of TREE."
  (cond ((null tree)
	 nil)
	((and (symbolp tree) (not (keywordp tree)))
	 tree)
	((atom tree)
	 nil)
	((eq (car tree) 'quote)
	 nil)
	(t
	 (or (find-first-field-name (car tree))
	     (find-first-field-name (cdr tree))))))

(defun string-or-qsym-p (thing)
  (or (stringp thing)
      (and (consp thing)
	   (eq (car thing) 'quote)
	   (or (stringp (cadr thing))
	       (symbolp (cadr thing))))))

(defun strip-quote (thing)
  (if (and (consp thing) (eq (car thing) 'quote))
      (cadr thing)
      thing))

(defun compile-printer-list (sources funstate)
  (unless (null sources)
    ;; Coalesce adjacent symbols/strings, and convert to strings if possible,
    ;; since they require less consing to write.
    (do ((el (car sources) (car sources))
	 (names nil (cons (strip-quote el) names)))
	((not (string-or-qsym-p el))
	 (when names
	   ;; concatenate adjacent strings and symbols
	   (let ((string
		  (apply #'concatenate
			 'string
			 (mapcar #'string (nreverse names)))))
	     (push (if (some #'alpha-char-p string)
		       `',(make-symbol string) ; preserve casifying output
		       string)
		   sources))))
      (pop sources))
    (cons (compile-printer-body (car sources) funstate)
	  (compile-printer-list (cdr sources) funstate))))

(defun compile-print (arg-name funstate &optional printer)
  (let* ((arg (arg-or-lose arg-name funstate))
	 (printer (or printer (arg-printer arg)))
	 (printer-val (value-or-source printer))
	 (printer-src (source-form printer)))
    (remember-printer-use arg funstate)
    (cond ((stringp printer-val)
	   `(local-format-arg ,(arg-value-form arg funstate) ,printer-val))
	  ((vectorp printer-val)
	   `(local-princ
	     (aref ,printer-src
		   ,(arg-value-form arg funstate :numeric))))
	  ((or (functionp printer-val)
	       (and (consp printer-val) (eq (car printer-val) 'function)))
	   `(local-call-arg-printer ,(arg-value-form arg funstate)
				    ,printer-src))
	  ((or (null printer-val) (eq printer-val t))
	   `(,(if (arg-use-label arg) 'local-princ16 'local-princ)
	     ,(arg-value-form arg funstate)))
	  (t
	   (pd-error "Illegal printer: ~s" printer-src)))))

(defun compile-printer-body (source funstate)
  (cond ((null source)
	 nil)
	((eq source :name)
	 `(local-print-name))
	((eq source :tab)
	 `(local-tab-to-arg-column))
	((keywordp source)
	 (pd-error "Unknown printer element: ~s" source))
	((symbolp source)
	 (compile-print source funstate))
	((atom source)
	 `(local-princ ',source))
	((eq (car source) :using)
	 (unless (or (stringp (cadr source))
		     (and (listp (cadr source))
			  (eq (caadr source) 'function)))
	   (pd-error "First arg to :USING must be a string or #'function"))
	 (compile-print (caddr source) funstate
			(cons (eval (cadr source)) (cadr source))))
	((eq (car source) :plus-integer)
	 ;; prints the given field proceed with a + or a -
	 (let ((form
		(arg-value-form (arg-or-lose (cadr source) funstate)
				funstate
				:numeric)))
	   `(progn
	      (when (>= ,form 0)
		(local-write-char #\+))
	      (local-princ ,form))))
	((eq (car source) 'quote)
	 `(local-princ ,source))
	((eq (car source) 'function)
	 `(local-call-global-printer ,source))
	((eq (car source) :cond)
	 `(cond ,@(mapcar #'(lambda (clause)
			      `(,(compile-test (find-first-field-name
						(cdr clause))
					       (car clause)
					       funstate)
				,@(compile-printer-list (cdr clause)
							funstate)))
			  (cdr source))))
	;; :if, :unless, and :when are replaced by :cond during preprocessing
	(t
	 `(progn ,@(compile-printer-list source funstate)))))

;;; ----------------------------------------------------------------
;;; Note that these things are compiled byte compiled to save space.

(defun make-printer-defun (source funstate function-name)
  (let ((printer-form (compile-printer-list source funstate))
	(bindings (make-arg-temp-bindings funstate)))
    `(defun ,function-name (chunk inst stream dstate)
       (declare (type dchunk chunk)
		(type instruction inst)
		(type stream stream)
		(type disassem-state dstate)
		#+small (optimize (speed 0) (safety 0) (debug 0)))

       (macrolet ((local-format-arg (arg fmt)
		    `(funcall (formatter ,fmt) stream ,arg)))
	 (flet ((local-tab-to-arg-column ()
		  (tab (dstate-argument-column dstate) stream))
		(local-print-name ()
		  (princ (inst-print-name inst) stream))
		(local-write-char (ch)
		  (write-char ch stream))
		(local-princ (thing)
		  (princ thing stream))
		(local-princ16 (thing)
		  (princ16 thing stream))
		(local-call-arg-printer (arg printer)
		  (funcall printer arg stream dstate))
		(local-call-global-printer (fun)
		  (funcall fun chunk inst stream dstate))
		(local-filtered-value (offset)
		  (declare (type filtered-value-index offset))
		  (aref (dstate-filtered-values dstate) offset))
		(local-extract (bytespec)
		  (dchunk-extract chunk bytespec))
		(lookup-label (lab)
		  (or (gethash lab (dstate-label-hash dstate))
		      lab))
		(adjust-label (val adjust-fun)
		  (funcall adjust-fun val dstate)))
	   (declare (ignorable #'local-tab-to-arg-column
			       #'local-print-name
			       #'local-princ #'local-princ16
			       #'local-write-char
			       #'local-call-arg-printer
			       #'local-call-global-printer
			       #'local-extract
			       #'local-filtered-value
			       #'lookup-label #'adjust-label)
		    (inline local-tab-to-arg-column
			    local-princ local-princ16
			    local-call-arg-printer local-call-global-printer
			    local-filtered-value local-extract
			    lookup-label adjust-label))
	   (let* ,bindings
	     ,@printer-form))))))

;;; ----------------------------------------------------------------

(defun all-arg-refs-relevent-p (printer args)
  (cond ((or (null printer) (keywordp printer) (eq printer t))
	 t)
	((symbolp printer)
	 (find printer args :key #'arg-name))
	((listp printer)
	 (every #'(lambda (x) (all-arg-refs-relevent-p x args))
		printer))
	(t t)))

(defun pick-printer-choice (choices args)
  (dolist (choice choices
	   (pd-error "No suitable choice found in ~s" choices))
    (when (all-arg-refs-relevent-p choice args)
      (return choice))))

(defun preprocess-chooses (printer args)
  (cond ((atom printer)
	 printer)
	((eq (car printer) :choose)
	 (pick-printer-choice (cdr printer) args))
	(t
	 (sharing-mapcar #'(lambda (sub) (preprocess-chooses sub args))
			 printer))))

;;; ----------------------------------------------------------------

(defun preprocess-test (subj form args)
  (multiple-value-bind (subj test)
      (if (and (consp form) (symbolp (car form)) (not (keywordp (car form))))
	  (values (car form) (cdr form))
	  (values subj form))
    (let ((key (if (consp test) (car test) test))
	  (body (if (consp test) (cdr test) nil)))
      (case key
	(:constant
	 (if (null body)
	     ;; if no supplied constant values, just any constant is ok, just
	     ;; see if there's some constant value in the arg.
	     (not
	      (null
	       (arg-value
		(or (find subj args :key #'arg-name)
		    (pd-error "Unknown argument ~s" subj)))))
	     ;; otherwise, defer to run-time
	     form))
	((:or :and :not)
	 (sharing-cons
	  form
	  subj
	  (sharing-cons
	   test
	   key
	   (sharing-mapcar
	    #'(lambda (sub-test)
		(preprocess-test subj sub-test args))
	    body))))
	(t form)))))

(defun preprocess-conditionals (printer args)
  (if (atom printer)
      printer
      (case (car printer)
	(:unless
	 (preprocess-conditionals
	  `(:cond ((:not ,(nth 1 printer)) ,@(nthcdr 2 printer)))
	  args))
	(:when
	 (preprocess-conditionals `(:cond (,(cdr printer))) args))
	(:if
	 (preprocess-conditionals
	  `(:cond (,(nth 1 printer) ,(nth 2 printer))
		  (t ,(nth 3 printer)))
	  args))
	(:cond
	 (sharing-cons
	  printer
	  :cond
	  (sharing-mapcar
	   #'(lambda (clause)
	       (let ((filtered-body
		      (sharing-mapcar
		       #'(lambda (sub-printer)
			   (preprocess-conditionals sub-printer args))
		       (cdr clause))))
		 (sharing-cons
		  clause
		  (preprocess-test (find-first-field-name filtered-body)
				   (car clause)
				   args)
		  filtered-body)))
	   (cdr printer))))
	(quote printer)
	(t
	 (sharing-mapcar
	  #'(lambda (sub-printer)
	      (preprocess-conditionals sub-printer args))
	  printer)))))

(defun preprocess-printer (printer args)
  "Returns a version of the disassembly-template PRINTER with compile-time
  tests (e.g. :constant without a value), and any :CHOOSE operators resolved
  properly for the args ARGS.  (:CHOOSE Sub*) simply returns the first Sub in
  which every field reference refers to a valid arg."
  (preprocess-conditionals (preprocess-chooses printer args) args))

;;; ----------------------------------------------------------------

(defstruct (cached-function (:conc-name cached-fun-))
  (funstate nil :type (or null funstate))
  (constraint nil :type list)
  (name nil :type (or null symbol)))

(defun find-cached-function (cached-funs args constraint)
  (dolist (cached-fun cached-funs nil)
    (let ((funstate (cached-fun-funstate cached-fun)))
      (when (and (equal constraint (cached-fun-constraint cached-fun))
		 (or (null funstate)
		     (funstate-compatible-p funstate args)))
	(return cached-fun)))))

(defmacro with-cached-function ((name-var funstate-var cache cache-slot
					  args &key constraint prefix)
				&body defun-maker-forms)
  (let ((cache-var (gensym))
	(constraint-var (gensym)))
    `(let* ((,constraint-var ,constraint)
	    (,cache-var (find-cached-function (,cache-slot ,cache)
					      ,args ,constraint-var)))
       (cond (,cache-var
	      #+nil
	      (Format t "~&; Using cached function ~s~%"
		      (cached-fun-name ,cache-var))
	      (values (cached-fun-name ,cache-var) nil))
	     (t
	      (let* ((,name-var (gensym ,prefix))
		     (,funstate-var (make-funstate ,args))
		     (,cache-var
		      (make-cached-function :name ,name-var
					    :funstate ,funstate-var
					    :constraint ,constraint-var)))
		#+nil
		(format t "~&; Making new function ~s~%"
			(cached-fun-name ,cache-var))
		(values ,name-var
			`(progn
			   ,(progn ,@defun-maker-forms)
			   (eval-when (compile eval)
			     (push ,,cache-var
				   (,',cache-slot ',,cache)))))))))))

;;; ----------------------------------------------------------------

(defstruct function-cache
  (printers nil :type list)
  (labellers nil :type list)
  (prefilters nil :type list))

(defun find-printer-fun (printer-source args cache)
  (if (null printer-source)
      (values nil nil)
      (let ((printer-source (preprocess-printer printer-source args)))
	(with-cached-function
	    (name funstate cache function-cache-printers args
		  :constraint printer-source
		  :prefix "PRINTER")
	  (make-printer-defun printer-source funstate name)))))

(defun find-labeller-fun (args cache)
  (let ((labelled-fields
	 (mapcar #'arg-name (remove-if-not #'arg-use-label args))))
    (if (null labelled-fields)
	(values nil nil)
	(with-cached-function
	    (name funstate cache function-cache-labellers args
	     :prefix "LABELLER"
	     :constraint labelled-fields)
	  (let ((labels-form 'labels))
	    (dolist (arg args)
	      (when (arg-use-label arg)
		(setf labels-form
		      `(let ((labels ,labels-form)
			     (addr
			      ,(arg-value-form arg funstate :adjusted nil)))
			 (if (assoc addr labels :test #'eq)
			     labels
			     (cons (cons addr nil) labels))))))
	    `(defun ,name (chunk labels dstate)
	       (declare (type list labels)
			(type dchunk chunk)
			(type disassem-state dstate)
			#+small
			(optimize (speed 0) (safety 0) (debug 0)))
	       (flet ((local-filtered-value (offset)
			(declare (type filtered-value-index offset))
			(aref (dstate-filtered-values dstate) offset))
		      (local-extract (bytespec)
			(dchunk-extract chunk bytespec))
		      (adjust-label (val adjust-fun)
			(funcall adjust-fun val dstate)))
		 (declare (ignorable #'local-filtered-value #'local-extract
				     #'adjust-label)
			  (inline local-filtered-value local-extract
				  adjust-label))
		 (let* ,(make-arg-temp-bindings funstate)
		   ,labels-form))))))))

(defun find-prefilter-fun (args cache)
  (let ((filtered-args
	 (mapcar #'arg-name (remove-if-not #'arg-prefilter args))))
    (if (null filtered-args)
	(values nil nil)
	(with-cached-function
	    (name funstate cache function-cache-prefilters args
	     :prefix "PREFILTER"
	     :constraint filtered-args)
	  (collect ((forms))
	    (dolist (arg args)
	      (let ((pf (arg-prefilter arg)))
		(when pf
		  (forms
		   `(setf (local-filtered-value ,(arg-position arg))
			  ,(maybe-listify
			    (gen-arg-forms arg :filtering funstate)))))
		))
	    `(defun ,name (chunk dstate)
	       (declare (type dchunk chunk)
			(type disassem-state dstate)
			#+small
			(optimize (speed 0) (safety 0) (debug 0)))
		 (flet (((setf local-filtered-value) (value offset)
			  (declare (type filtered-value-index offset))
			  (setf (aref (dstate-filtered-values dstate) offset)
				value))
			(local-filter (value filter)
			  (funcall filter value dstate))
			(local-extract (bytespec)
			  (dchunk-extract chunk bytespec)))
		   (declare (ignorable #'local-filter #'local-extract)
			    (inline (setf local-filtered-value)
				    local-filter local-extract))
		   ;; use them for side-effects only
		   (let* ,(make-arg-temp-bindings funstate)
		     ,@(forms)))))))))

;;; ----------------------------------------------------------------

(defun set-arg-from-type (arg type-name table)
  (let ((type-arg (find type-name table :key #'arg-name)))
    (when (null type-arg)
      (pd-error "Unknown argument type: ~s" type-name))
    (setf (arg-printer arg) (arg-printer type-arg))
    (setf (arg-prefilter arg) (arg-prefilter type-arg))
    (setf (arg-sign-extend-p arg) (arg-sign-extend-p type-arg))
    (setf (arg-use-label arg) (arg-use-label type-arg))))

(defun modify-or-add-arg (arg-name
			  args
			  type-table
			  &key
			  (value nil value-p)
			  (type nil type-p)
			  (prefilter nil prefilter-p)
			  (printer nil printer-p)
			  (sign-extend nil sign-extend-p)
			  (use-label nil use-label-p)
			  (field nil field-p)
			  (fields nil fields-p)
			  format-length)
  (let* ((arg-pos (position arg-name args :key #'arg-name))
	 (arg
	  (if (null arg-pos)
	      (let ((arg (make-argument :name arg-name)))
		(if (null args)
		    (setf args (list arg))
		    (push arg (cdr (last args))))
		arg)
	      (setf (nth arg-pos args) (copy-argument (nth arg-pos args))))))
    (when (and field-p (not fields-p))
      (setf fields (list field))
      (setf fields-p t))
    (when type-p
      (set-arg-from-type arg type type-table))
    (when value-p
      (setf (arg-value arg) value))
    (when prefilter-p
      (setf (arg-prefilter arg) prefilter))
    (when sign-extend-p
      (setf (arg-sign-extend-p arg) sign-extend))
    (when printer-p
      (setf (arg-printer arg) printer))
    (when use-label-p
      (setf (arg-use-label arg) use-label))
    (when fields-p
      (when (null format-length)
	(error
	 "~@<In arg ~s:  ~3i~:_~
          Can't specify fields except using DEFINE-INSTRUCTION-FORMAT.~:>"
	 arg-name))
      (setf (arg-fields arg)
	    (mapcar #'(lambda (bytespec)
			(when (> (+ (byte-position bytespec)
				    (byte-size bytespec))
				 format-length)
			  (error "~@<In arg ~s:  ~3i~:_~
				     Field ~s doesn't fit in an ~
				     instruction-format ~d bits wide.~:>"
				 arg-name
				 bytespec
				 format-length))
			(correct-dchunk-bytespec-for-endianness
			 bytespec
			 format-length
			 (c:backend-byte-order c:*target-backend*)))
		    fields))
      )
    args))

;;; ----------------------------------------------------------------
;;; Compile time info that we stash in the package of the machine backend

(defun format-table-name ()
  (intern "*DISASSEMBLER-INSTRUCTION-FORMATS*"))
(defun arg-type-table-name ()
  (intern "*DISASSEMBLER-ARG-TYPES*"))
(defun function-cache-name ()
  (intern "*DISASSEMBLER-CACHED-FUNCTIONS*"))

;;; ----------------------------------------------------------------

(defparameter *arg-function-params*
  '((:printer . (value stream dstate))
    (:use-label . (value dstate))
    (:prefilter . (value dstate))))

;;; detect things that obviously don't need wrapping, like variable-refs &
;;; #'function
(defun doesnt-need-wrapping-p (form)
  (or (symbolp form)
      (and (listp form)
	   (eq (car form) 'function)
	   (symbolp (cadr form)))))

(defun make-wrapper (form arg-name funargs prefix)
  (if (and (listp form)
	   (eq (car form) 'function))
      ;; a function def
      (let ((wrapper-name (symbolicate prefix "-" arg-name "-WRAPPER"))
	    (wrapper-args nil))
	(dotimes (i (length funargs))
	  (push (gensym) wrapper-args))
	(values `#',wrapper-name
		`(defun ,wrapper-name ,wrapper-args
		   (funcall ,form ,@wrapper-args))))
      ;; something else
      (let ((wrapper-name (symbolicate "*" prefix "-" arg-name "-WRAPPER*")))
	(values wrapper-name `(defparameter ,wrapper-name ,form)))))

(defun munge-fun-refs (params evalp &optional wrap-defs-p (prefix ""))
  (let ((params (copy-list params)))
    (do ((tail params (cdr tail))
	 (wrapper-defs nil))
	((null tail)
	 (values params (nreverse wrapper-defs)))
      (let ((fun-arg (assoc (car tail) *arg-function-params*)))
	(when fun-arg
	  (let* ((fun-form (cadr tail))
		 (quoted-fun-form `',fun-form))
	    (when (and wrap-defs-p (not (doesnt-need-wrapping-p fun-form)))
	      (multiple-value-bind (access-form wrapper-def-form)
		  (make-wrapper fun-form (car fun-arg) (cdr fun-arg) prefix)
		(setf quoted-fun-form `',access-form)
		(push wrapper-def-form wrapper-defs)))
	    (if evalp
		(setf (cadr tail)
		      `(make-valsrc ,fun-form ,quoted-fun-form))
		(setf (cadr tail)
		      fun-form))))))))

;;; ----------------------------------------------------------------

(defun gen-preamble-form (args)
  "Generate a form to specify global disassembler params.  See the
  documentation for SET-DISASSEM-PARAMS for more info."
  (destructuring-bind
	(&key instruction-alignment
	      address-size
	      (opcode-column-width nil opcode-column-width-p))
      args
    `(progn
       (eval-when (compile eval)
	 ;; these are not in the params because they only exist at compile time
	 (defparameter ,(format-table-name) (make-hash-table))
	 (defparameter ,(arg-type-table-name) nil)
	 (defparameter ,(function-cache-name) (make-function-cache)))
       (let ((params
	      (or (c:backend-disassem-params c:*target-backend*)
		  (setf (c:backend-disassem-params c:*target-backend*)
			(make-params :backend c::*target-backend*)))))
	 (declare (ignorable params))
	 ,(when instruction-alignment
	    `(setf (params-instruction-alignment params)
		   (bits-to-bytes ,instruction-alignment)))
	 ,(when address-size
	    `(setf (params-location-column-width params)
		   (* 2 ,address-size)))
	 ,(when opcode-column-width-p
	    `(setf (params-opcode-column-width params) ,opcode-column-width))
	 'disassem-params))))

(defun gen-clear-info-form ()
  `(eval-when (compile eval)
     (setf ,(format-table-name) nil)
     (setf ,(arg-type-table-name) nil)))

(defun update-args-form (var name-form descrip-forms evalp
			     &optional format-length-form)
  `(setf ,var
	 ,(if evalp
	      `(modify-or-add-arg ,name-form
				  ,var
				  ,(arg-type-table-name)
				  ,@(and format-length-form
					 `(:format-length
					    ,format-length-form))
				  ,@descrip-forms)
	      `(apply #'modify-or-add-arg
		      ,name-form
		      ,var
		      ,(arg-type-table-name);
		      ,@(and format-length-form
			     `(:format-length ,format-length-form))
		      ',descrip-forms))))

(defun gen-arg-type-def-form (name args &optional (evalp t))
  "Generate a form to define a disassembler argument type.  See
  DEFINE-ARGUMENT-TYPE for more info."
  (multiple-value-bind (args wrapper-defs)
      (munge-fun-refs args evalp t name)
    `(progn
       ,@wrapper-defs
       (eval-when (compile eval)
	 ,(update-args-form (arg-type-table-name) `',name args evalp))
       ',name)))

(defun maybe-quote (evalp form)
  (if (or evalp (self-evaluating-p form)) form `',form))

(defun maybe-quote-list (evalp list)
  (if evalp
      list
      (mapcar #'(lambda (el) (maybe-quote nil el)) list)))

(defun gen-arg-access-macro-def-form (arg args format-name)
  (let* ((funstate (make-funstate args))
	 (arg-val-form (arg-value-form arg funstate :adjusted))
	 (bindings (make-arg-temp-bindings funstate)))
    `(defmacro ,(symbolicate format-name "-" (arg-name arg)) (chunk dstate)
       `(let ((chunk ,chunk) (dstate ,dstate))
	  (declare (ignorable chunk dstate))
	  (flet ((local-filtered-value (offset)
		   (declare (type filtered-value-index offset))
		   (aref (dstate-filtered-values dstate) offset))
		 (local-extract (bytespec)
		   (dchunk-extract chunk bytespec)))
	    (declare (ignorable #'local-filtered-value #'local-extract)
		     (inline local-filtered-value local-extract))
	    (let* ,',bindings
	      ,',arg-val-form))))))

(defun gen-format-def-form (header descrips &optional (evalp t))
  "Generate a form to define an instruction format.  See
  DEFINE-INSTRUCTION-FORMAT for more info."
  (when (atom header)
    (setf header (list header)))
  (destructuring-bind (name length &key default-printer include)
      header
    (let ((args-var (gensym))
	  (length-var (gensym))
	  (all-wrapper-defs nil)
	  (arg-count 0))
      (collect ((arg-def-forms))
	(dolist (descrip descrips)
	  (let ((name (pop descrip))) 
	    (multiple-value-bind (descrip wrapper-defs)
		(munge-fun-refs
		 descrip evalp t (format nil "~:@(~a~)-~d" name arg-count))
	      (arg-def-forms
	       (update-args-form args-var `',name descrip evalp length-var))
	      (setf all-wrapper-defs
		    (nconc wrapper-defs all-wrapper-defs)))
	    (incf arg-count)))
	`(progn
	   ,@all-wrapper-defs
	   (eval-when (compile eval)
	     (let ((,length-var ,length)
		   (,args-var
		    ,(and include
			  `(copy-list
			    (format-args
			     (format-or-lose ,include
					     ,(format-table-name)))))))
	       ,@(arg-def-forms)
	       (setf (gethash ',name ,(format-table-name))
		     (make-instruction-format
		      :name ',name
		      :length (bits-to-bytes ,length-var)
		      :default-printer ,(maybe-quote evalp default-printer)
		      :args ,args-var))
	       (eval
		`(progn
		   ,@(mapcar #'(lambda (arg)
				 (when (arg-fields arg)
				   (gen-arg-access-macro-def-form
				    arg ,args-var ',name)))
			     ,args-var)))
	       )))))))

;;; ----------------------------------------------------------------

(defun compute-mask-id (args)
  (let ((mask dchunk-zero)
	(id dchunk-zero))
    (dolist (arg args (values mask id))
      (let ((av (arg-value arg)))
	(when av
	  (do ((fields (arg-fields arg) (cdr fields))
	       (values (if (atom av) (list av) av) (cdr values)))
	      ((null fields))
	    (let ((field-mask (dchunk-make-mask (car fields))))
	      (when (/= (dchunk-and mask field-mask) dchunk-zero)
		(pd-error "Field ~s in arg ~s overlaps some other field"
			  (car fields)
			  (arg-name arg)))
	      (dchunk-insertf id (car fields) (car values))
	      (dchunk-orf mask field-mask))))))))

(defun install-inst-flavors (name flavors)
  (setf (gethash name
		 (params-instructions
		  (c:backend-disassem-params c:*target-backend*)))
	flavors))
			  
(defun format-or-lose (name table)
  (or (gethash name table)
      (pd-error "Unknown instruction format ~s" name)))

(defun filter-overrides (overrides evalp)
  (mapcar #'(lambda (override)
	      (list* (car override) (cadr override)
		     (munge-fun-refs (cddr override) evalp)))
	  overrides))

(defun gen-args-def-form (overrides format-form &optional (evalp t))
  (let ((args-var (gensym)))
    `(let ((,args-var (copy-list (format-args ,format-form))))
       ,@(mapcar #'(lambda (override)
		     (update-args-form args-var
				       `',(car override)
				       (and (cdr override)
					    (cons :value (cdr override)))
				       evalp))
		 overrides)
       ,args-var)))

(defun gen-printer-def-forms-def-form (name def &optional (evalp t))
  (destructuring-bind (format-name (&rest field-defs)
				   &optional (printer-form :default)
				   &key
				   ((:print-name print-name-form) `',name)
				   control)
      def
    (let ((format-var (gensym))
	  (field-defs (filter-overrides field-defs evalp)))
      `(let* ((*current-instruction-flavor* ',(cons name format-name))
	      (,format-var (format-or-lose ',format-name ,(format-table-name)))
	      (args ,(gen-args-def-form field-defs format-var evalp))
	      (funcache ,(function-cache-name)))
	 #+small (declare (optimize (speed 0) (safety 0) (debug 0)))
	 (multiple-value-bind (printer-fun printer-defun)
	     (find-printer-fun ,(if (eq printer-form :default)
				     `(format-default-printer ,format-var)
				     (maybe-quote evalp printer-form))
			       args funcache)
	   (multiple-value-bind (labeller-fun labeller-defun)
	       (find-labeller-fun args funcache)
	     (multiple-value-bind (prefilter-fun prefilter-defun)
		 (find-prefilter-fun args funcache)
	       (multiple-value-bind (mask id)
		   (compute-mask-id args)
		 (values
		  `(make-instruction ',',name
				     ',',format-name
				     ,',print-name-form
				     ,(format-length ,format-var)
				     ,mask
				     ,id
				     ,(and printer-fun `#',printer-fun)
				     ,(and labeller-fun `#',labeller-fun)
				     ,(and prefilter-fun `#',prefilter-fun)
				     ,',control)
		  `(progn
		     ,@(and printer-defun (list printer-defun))
		     ,@(and labeller-defun (list labeller-defun))
		     ,@(and prefilter-defun (list prefilter-defun))))
		 ))))))))


;;; ----------------------------------------------------------------
;;; combining instructions where one specializes another

(defun inst-specializes-p (special general)
  "Returns non-NIL if the instruction SPECIAL is a more specific version of
  GENERAL (i.e., the same instruction, but with more constraints)."
  (declare (type instruction special general))
  (let ((smask (inst-mask special))
	(gmask (inst-mask general)))
    (and (dchunk= (inst-id general)
		  (dchunk-and (inst-id special) gmask))
	 (dchunk-strict-superset-p smask gmask))))

;;; a bit arbitrary, but should work ok...
(defun specializer-rank (inst)
  "Returns an integer corresponding to the specifivity of the instruction INST."
  (declare (type instruction inst))
  (* (dchunk-count-bits (inst-mask inst)) 4))

(defun order-specializers (insts)
  "Order the list of instructions INSTS with more specific (more constant
  bits, or same-as argument constains) ones first.  Returns the ordered list."
  (declare (type list insts))
  (sort insts
	#'(lambda (i1 i2)
	    (> (specializer-rank i1) (specializer-rank i2)))))

(defun specialization-error (insts)
  (error "Instructions either aren't related or conflict in some way:~% ~s" insts))

(defun try-specializing (insts)
  "Given a list of instructions INSTS, Sees if one of these instructions is a
  more general form of all the others, in which case they are put into its
  specializers list, and it is returned.  Otherwise an error is signaled."
  (declare (type list insts))
  (let ((masters (copy-list insts)))
    (dolist (possible-master insts)
      (dolist (possible-specializer insts)
	(unless (or (eq possible-specializer possible-master)
		    (inst-specializes-p possible-specializer possible-master))
	  (setf masters (delete possible-master masters))
	  (return)			; exit the inner loop
	  )))
    (cond ((null masters)
	   (specialization-error insts))
	  ((cdr masters)
	   (error "Multiple specializing masters: ~s" masters))
	  (t
	   (let ((master (car masters)))
	     (setf (inst-specializers master)
		   (order-specializers (remove master insts)))
	     master)))))

;;; ----------------------------------------------------------------
;;; choosing an instruction

(declaim (inline inst-matches-p choose-inst-specialization))

(defun inst-matches-p (inst chunk)
  "Returns non-NIL if all constant-bits in INST match CHUNK."
  (declare (type instruction inst)
	   (type dchunk chunk))
  (dchunk= (dchunk-and (inst-mask inst) chunk) (inst-id inst)))

(defun choose-inst-specialization (inst chunk)
  "Given an instruction object, INST, and a bit-pattern, CHUNK, picks the
  most specific instruction on INST's specializer list who's constraints are
  met by CHUNK.  If none do, then INST is returned."
  (declare (type instruction inst)
	   (type dchunk chunk))
  (or (dolist (spec (inst-specializers inst) nil)
	(declare (type instruction spec))
	(when (inst-matches-p spec chunk)
	  (return spec)))
      inst))

;;; ----------------------------------------------------------------
;;; an instruction space holds all known machine instructions in a form that
;;; can be easily searched

(defstruct (inst-space (:conc-name ispace-) (:print-function %print-ispace))
  (valid-mask dchunk-zero :type dchunk)	; applies to *children*
  (choices nil :type list)
  )

(defun %print-ispace (ispace stream level)
  (declare (ignore level))
  (print-unreadable-object (ispace stream :type t :identity t)))

(defstruct (inst-space-choice (:conc-name ischoice-))
  (common-id dchunk-zero :type dchunk)	; applies to *parent's* mask
  (subspace (required-argument) :type (or inst-space instruction))
  )

;;; ----------------------------------------------------------------
;;; searching for an instruction in instruction space

(defun find-inst (chunk inst-space)
  "Returns the instruction object within INST-SPACE corresponding to the
  bit-pattern CHUNK, or NIL if there isn't one."
  (declare (type dchunk chunk)
	   (type (or null inst-space instruction) inst-space))
  (etypecase inst-space
    (null nil)
    (instruction
     (if (inst-matches-p inst-space chunk)
	 (choose-inst-specialization inst-space chunk)
	 nil))
    (inst-space
     (let* ((mask (ispace-valid-mask inst-space))
	    (id (dchunk-and mask chunk)))
       (declare (type dchunk id mask))
       (dolist (choice (ispace-choices inst-space))
	 (declare (type inst-space-choice choice))
	 (when (dchunk= id (ischoice-common-id choice))
	   (return (find-inst chunk (ischoice-subspace choice)))))))))

;;; ----------------------------------------------------------------
;;; building the instruction space

(defun build-inst-space (insts &optional (initial-mask dchunk-one))
  "Returns an instruction-space object corresponding to the list of
  instructions INSTS.  If the optional parameter INITIAL-MASK is supplied, only
  bits it has set are used."
  ;; This is done by finding any set of bits that's common to
  ;; all instructions, building an instruction-space node that selects on those
  ;; bits, and recursively handle sets of instructions with a common value for
  ;; these bits (which, since there should be fewer instructions than in INSTS,
  ;; should have some additional set of bits to select on, etc).  If there
  ;; are no common bits, or all instructions have the same value within those
  ;; bits, TRY-SPECIALIZING is called, which handles the cases of many
  ;; variations on a single instruction.
  (declare (type list insts)
	   (type dchunk initial-mask))
  (cond ((null insts)
	 nil)
	((null (cdr insts))
	 (car insts))
	(t
	 (let ((vmask (dchunk-copy initial-mask)))
	   (dolist (inst insts)
	     (dchunk-andf vmask (inst-mask inst)))
	   (if (dchunk-zerop vmask)
	       (try-specializing insts)
	       (let ((buckets nil))
		 (dolist (inst insts)
		   (let* ((common-id (dchunk-and (inst-id inst) vmask))
			  (bucket (assoc common-id buckets :test #'dchunk=)))
		     (cond ((null bucket)
			    (push (list common-id inst) buckets))
			   (t
			    (push inst (cdr bucket))))))
		 (let ((submask (dchunk-clear initial-mask vmask)))
		   (if (= (length buckets) 1)
		       (try-specializing insts)
		       (make-inst-space
			:valid-mask vmask
			:choices (mapcar #'(lambda (bucket)
					     (make-inst-space-choice
					      :subspace (build-inst-space
							 (cdr bucket)
							 submask)
					      :common-id (car bucket)))
					 buckets))))))))))

;;; ----------------------------------------------------------------
;;; an inst-space printer for debugging purposes

(defun print-masked-binary (num mask word-size &optional (show word-size))
  (do ((bit (1- word-size) (1- bit)))
      ((< bit 0))
    (write-char (cond ((logbitp bit mask)
		       (if (logbitp bit num) #\1 #\0))
		      ((< bit show) #\x)
		      (t #\space)))))

(defun print-inst-bits (inst)
  (print-masked-binary (inst-id inst)
		       (inst-mask inst)
		       dchunk-bits
		       (bytes-to-bits (inst-length inst))))

(defun print-inst-space (inst-space &optional (indent 0))
  "Prints a nicely formatted version of INST-SPACE."
  (etypecase inst-space
    (null)
    (instruction
     (format t "~vt[~a(~a)~45t" indent
	     (inst-name inst-space)
	     (inst-format-name inst-space))
     (print-inst-bits inst-space)
     (dolist (inst (inst-specializers inst-space))
       (format t "~%~vt:~a~45t" indent (inst-name inst))
       (print-inst-bits inst))
     (write-char #\])
     (terpri))
    (inst-space
     (format t "~vt---- ~8,'0x ----~%"
	     indent
	     (ispace-valid-mask inst-space))
     (map nil
	  #'(lambda (choice)
	      (format t "~vt~8,'0x ==>~%"
		      (+ 2 indent)
		      (ischoice-common-id choice))
	      (print-inst-space (ischoice-subspace choice)
				(+ 4 indent)))
	  (ispace-choices inst-space)))))

(defun print-backend-inst-space (&optional (backend c:*target-backend*))
  "Print the inst space for the specified backend"
  (let ((ext:*gc-verbose* nil))
    (print-inst-space (get-inst-space (c:backend-disassem-params backend)))))

;;;; ----------------------------------------------------------------
;;;; the actual disassembly part
;;;; ----------------------------------------------------------------

;;; Code object layout:
;;;	header-word
;;;	code-size (starting from first inst, in words)
;;;	entry-points (points to first function header)
;;;	debug-info
;;;	trace-table-offset (starting from first inst, in bytes)
;;;	constant1
;;;	constant2
;;;	...
;;;	<padding to dual-word boundry>
;;;	start of instructions
;;;	...
;;;	function-headers and lra's buried in here randomly
;;;	...
;;;	start of trace-table
;;;	<padding to dual-word boundry>
;;;
;;; Function header layout (dual word aligned):
;;;	header-word
;;;	self pointer
;;;	next pointer (next function header)
;;;	name
;;;	arglist
;;;	type
;;;
;;; LRA layout (dual word aligned):
;;;	header-word

(declaim (inline words-to-bytes bytes-to-words))

(eval-when (eval load compile)		; used in a defconstant
  (defun words-to-bytes (num)
    "Converts a word-offset NUM to a byte-offset."
    (declare (type offset num))
    (ash num vm:word-shift))
  )

(defun bytes-to-words (num)
  "Converts a byte-offset NUM to a word-offset."
  (declare (type offset num))
  (ash num (- vm:word-shift)))

(defconstant lra-size (words-to-bytes 1))

;;; ----------------------------------------------------------------
;;;

(defstruct offs-hook
  (offset 0 :type offset)
  (function (required-argument) :type function)
  (before-address nil :type (member t nil)))

(defstruct (segment (:conc-name seg-)
		    (:print-function %print-segment)
		    (:constructor %make-segment))
  (sap-maker (required-argument) :type (function () system:system-area-pointer))
  (length 0 :type length)
  (virtual-location 0 :type address)
  (storage-info nil :type (or null storage-info))
  (code nil :type (or null kernel:code-component))
  (hooks nil :type list)
  )

(defun %print-segment (seg stream level)
  (declare (ignore level))
  (print-unreadable-object (seg stream :type t)
    (let ((addr (system:sap-int (funcall (seg-sap-maker seg)))))
      (format stream "#x~x[~d]~:[ (#x~x)~;~*~]~@[ in ~s~]"
	      addr
	      (seg-length seg)
	      (= (seg-virtual-location seg) addr)
	      (seg-virtual-location seg)
	      (seg-code seg)))))

;;; ----------------------------------------------------------------

;;; All state during disassembly.  We store some seemingly redundant
;;; information so that we can allow garbage collect during disassembly and
;;; not get tripped up by a code block being moved...
(defstruct (disassem-state (:conc-name dstate-)
			   (:print-function %print-dstate)
			   (:constructor %make-dstate))
  (cur-offs 0 :type offset)		; offset of current pos in segment
  (next-offs 0 :type offset)		; offset of next position

  (segment-sap (required-argument) :type system:system-area-pointer)
					; a sap pointing to our segment
  (segment nil :type (or null segment))	; the current segment

  (alignment vm:word-bytes :type alignment) ; what to align to in most cases
  (byte-order :little-endian
	      :type (member :big-endian :little-endian))

  (properties nil :type list)		; for user code to hang stuff off of
  (filtered-values (make-array max-filtered-value-index)
		   :type filtered-value-vector)

  (addr-print-len nil :type		; used for prettifying printing
		  (or null (integer 0 20)))
  (argument-column 0 :type column)
  (output-state :beginning		; to make output look nicer
		  :type (member :beginning
				:block-boundary
				nil))

  (labels nil :type list)		; alist of (address . label-number)
  (label-hash (make-hash-table)		; same thing in a different form
	      :type hash-table)

  (fun-hooks nil :type list) 		; list of function

  ;; these next two are popped as they are used
  (cur-labels nil :type list)		; alist of (address . label-number)
  (cur-offs-hooks nil :type list) 	; list of offs-hook

  (notes nil :type list)		; for the current location

  (current-valid-locations nil		; currently active source variables
			   :type (or null (vector bit)))

  (params (required-argument) :type params) ; a handy pointer ...
  )

(defun %print-dstate (dstate stream level)
  (declare (ignore level))
  (print-unreadable-object (dstate stream :type t)
    (format stream "+~d~@[ in ~s~]" (dstate-cur-offs dstate) (dstate-segment dstate))))

(defmacro dstate-get-prop (dstate name)
  "Get the value of the property called NAME in DSTATE.  Also setf'able."
  `(getf (dstate-properties ,dstate) ,name))

(defun dstate-cur-addr (dstate)
  "Returns the absolute address of the current instruction in DSTATE."
  (the address (+ (seg-virtual-location (dstate-segment dstate))
		  (dstate-cur-offs dstate))))

(defun dstate-next-addr (dstate)
  "Returns the absolute address of the next instruction in DSTATE."
  (the address (+ (seg-virtual-location (dstate-segment dstate))
		  (dstate-next-offs dstate))))

;;; ----------------------------------------------------------------
;;; function ops

(defun fun-self (fun)
  (declare (type compiled-function fun))
  (kernel:%function-self fun))

(defun fun-code (fun)
  (declare (type compiled-function fun))
  (kernel:function-code-header (fun-self fun)))

(defun fun-next (fun)
  (declare (type compiled-function fun))
  (kernel:%function-next fun))

(defun fun-address (function)
  (declare (type compiled-function function))
  (ecase (kernel:get-type function)
    (#.vm:function-header-type
     (- (kernel:get-lisp-obj-address function) vm:function-pointer-type))
    (#.vm:closure-header-type
     (fun-address (kernel:%closure-function function)))
    (#.vm:funcallable-instance-header-type
     (fun-address (kernel:funcallable-instance-function function)))))

(defun fun-insts-offset (function)
  "Offset of FUNCTION from the start of its code-component's instruction area."
  (declare (type compiled-function function))
  (- (fun-address function)
     (system:sap-int (kernel:code-instructions (fun-code function)))))

(defun fun-offset (function)
  "Offset of FUNCTION from the start of its code-component."
  (declare (type compiled-function function))
  (words-to-bytes (kernel:get-closure-length function)))

;;; ----------------------------------------------------------------
;;; Operations on code-components (which hold the instructions for
;;; one or more functions).

(defun code-inst-area-length (code-component)
  "Returns the length of the instruction area in CODE-COMPONENT."
  (declare (type kernel:code-component code-component))
  (kernel:code-header-ref code-component vm:code-trace-table-offset-slot))

(defun code-inst-area-address (code-component)
  "Returns the address of the instruction area in CODE-COMPONENT."
  (declare (type kernel:code-component code-component))
  (system:sap-int (kernel:code-instructions code-component)))

(defun code-first-function (code-component)
  "Returns the first function in CODE-COMPONENT."
  (declare (type kernel:code-component code-component))
  (kernel:code-header-ref code-component vm:code-trace-table-offset-slot))

(defun segment-offs-to-code-offs (offset segment)
  (system:without-gcing
   (let* ((seg-base-addr (system:sap-int (funcall (seg-sap-maker segment))))
	  (code-addr
	   (logandc1 vm:lowtag-mask
		     (kernel:get-lisp-obj-address (seg-code segment))))
	  (addr (+ offset seg-base-addr)))
     (declare (type address seg-base-addr code-addr addr))
     (- addr code-addr))))

(defun code-offs-to-segment-offs (offset segment)
  (system:without-gcing
   (let* ((seg-base-addr (system:sap-int (funcall (seg-sap-maker segment))))
	  (code-addr
	   (logandc1 vm:lowtag-mask
		     (kernel:get-lisp-obj-address (seg-code segment))))
	  (addr (+ offset code-addr)))
     (declare (type address seg-base-addr code-addr addr))
     (- addr seg-base-addr))))

(defun code-insts-offs-to-segment-offs (offset segment)
  (system:without-gcing
   (let* ((seg-base-addr (system:sap-int (funcall (seg-sap-maker segment))))
	  (code-insts-addr
	   (system:sap-int (kernel:code-instructions (seg-code segment))))
	  (addr (+ offset code-insts-addr)))
     (declare (type address seg-base-addr code-insts-addr addr))
     (- addr seg-base-addr))))

;;; ----------------------------------------------------------------

(defun lra-hook (chunk stream dstate)
  (declare (ignore stream))
  (declare (type dchunk chunk)
	   (ignore chunk)
	   (type disassem-state dstate))
  (when (and (aligned-p (+ (seg-virtual-location (dstate-segment dstate))
			   (dstate-cur-offs dstate))
			(* 2 vm:word-bytes))
	     ;; check type
	     (= (system:sap-ref-8 (dstate-segment-sap dstate)
				  (if (eq (dstate-byte-order dstate)
					  :little-endian)
				      (dstate-cur-offs dstate)
				      (+ (dstate-cur-offs dstate)
					 (1- lra-size))))
		vm:return-pc-header-type))
    (note (format nil "Possible ~A header word" '.lra) dstate))
  nil)

(defun fun-header-hook (stream dstate)
  "Print the function-header (entry-point) pseudo-instruction at the current
  location in DSTATE to STREAM."
  (declare (type (or null stream) stream)
	   (type disassem-state dstate))
  (unless (null stream)
    (let* ((seg (dstate-segment dstate))
	   (code (seg-code seg))
	   (woffs
	    (bytes-to-words
	     (segment-offs-to-code-offs (dstate-cur-offs dstate) seg)))
	   (name
	    (kernel:code-header-ref code (+ woffs vm:function-name-slot)))
	   (args
	    (kernel:code-header-ref code (+ woffs vm:function-arglist-slot)))
	   (type
	    (kernel:code-header-ref code (+ woffs vm:function-type-slot))))
      (format stream ".~a ~s~:a" 'entry name args)
      (note #'(lambda (stream)
		(format stream "~:s" type)) ; use format to print NIL as ()
	    dstate)))
  (incf (dstate-next-offs dstate)
	(words-to-bytes vm:function-code-offset)))

;;; ----------------------------------------------------------------

(defun alignment-hook (chunk stream dstate)
  (declare (type dchunk chunk)
	   (ignore chunk)
	   (type (or null stream) stream)
	   (type disassem-state dstate))
  (let ((location
	 (+ (seg-virtual-location (dstate-segment dstate))
	    (dstate-cur-offs dstate)))
	(alignment (dstate-alignment dstate)))
    (unless (aligned-p location alignment)
      (when stream
	(format stream "~a~vt~d~%" '.align
		(dstate-argument-column dstate)
		alignment))
      (incf(dstate-next-offs dstate)
	   (- (align location alignment) location)))
    nil))

(defun rewind-current-segment (dstate segment)
  (declare (type disassem-state dstate)
	   (type segment segment))
  (setf (dstate-segment dstate) segment)
  (setf (dstate-cur-offs-hooks dstate)
	(stable-sort (nreverse (copy-list (seg-hooks segment)))
		     #'(lambda (oh1 oh2)
			 (or (< (offs-hook-offset oh1) (offs-hook-offset oh2))
			     (and (= (offs-hook-offset oh1)
				     (offs-hook-offset oh2))
				  (offs-hook-before-address oh1)
				  (not (offs-hook-before-address oh2)))))))
  (setf (dstate-cur-offs dstate) 0)
  (setf (dstate-cur-labels dstate) (dstate-labels dstate)))

(defun do-offs-hooks (before-address stream dstate)
  (declare (type (or null stream) stream)
	   (type disassem-state dstate))
  (let ((cur-offs (dstate-cur-offs dstate)))
    (setf (dstate-next-offs dstate) cur-offs)
    (loop
      (let ((next-hook (car (dstate-cur-offs-hooks dstate))))
	(when (null next-hook)
	  (return))
	(let ((hook-offs (offs-hook-offset next-hook)))
	  (when (or (> hook-offs cur-offs)
		    (and (= hook-offs cur-offs)
			 before-address
			 (not (offs-hook-before-address next-hook))))
	    (return))
	  (unless (< hook-offs cur-offs)
	    (funcall (offs-hook-function next-hook) stream dstate))
	  (pop (dstate-cur-offs-hooks dstate))
	  (unless (= (dstate-next-offs dstate) cur-offs)
	    (return)))))))

(defun do-fun-hooks (chunk stream dstate)
  (let ((hooks (dstate-fun-hooks dstate))
	(cur-offs (dstate-cur-offs dstate)))
    (setf (dstate-next-offs dstate) cur-offs)
    (dolist (hook hooks nil)
      (let ((prefix-p (funcall hook chunk stream dstate)))
	(unless (= (dstate-next-offs dstate) cur-offs)
	  (return prefix-p))))))

(defun handle-bogus-instruction (stream dstate)
  (let ((alignment (dstate-alignment dstate)))
    (unless (null stream)
      (multiple-value-bind (words bytes)
	  (truncate alignment vm:word-bytes)
	(when (> words 0)
	  (print-words words stream dstate))
	(when (> bytes 0)
	  (print-bytes bytes stream dstate))))
    (incf (dstate-next-offs dstate) alignment)))

(defun map-segment-instructions (function segment dstate &optional stream)
  "Iterate through the instructions in SEGMENT, calling FUNCTION
  for each instruction, with arguments of CHUNK, STREAM, and DSTATE."
  (declare (type function function)
	   (type segment segment)
	   (type disassem-state dstate)
	   (type (or null stream) stream))
  
  (let ((ispace (get-inst-space (dstate-params dstate)))
	(prefix-p nil))	; just processed a prefix inst

    (rewind-current-segment dstate segment)

    (loop
      (when (>= (dstate-cur-offs dstate)
		(seg-length (dstate-segment dstate)))
	;; done!
	(return))

      (setf (dstate-next-offs dstate) (dstate-cur-offs dstate))

      (do-offs-hooks t stream dstate)
      (unless (or prefix-p (null stream))
	(print-current-address stream dstate))
      (do-offs-hooks nil stream dstate)

      (unless (> (dstate-next-offs dstate) (dstate-cur-offs dstate))
	(system:without-gcing
	 (setf (dstate-segment-sap dstate) (funcall (seg-sap-maker segment)))

	 (let ((chunk
		(sap-ref-dchunk (dstate-segment-sap dstate)
				(dstate-cur-offs dstate)
				(dstate-byte-order dstate))))
	   (let ((fun-prefix-p (do-fun-hooks chunk stream dstate)))
	     (if (> (dstate-next-offs dstate) (dstate-cur-offs dstate))
		 (setf prefix-p fun-prefix-p)
		 (let ((inst (find-inst chunk ispace)))
		   (cond ((null inst)
			  (handle-bogus-instruction stream dstate))
			 (t
			  (setf (dstate-next-offs dstate)
				(+ (dstate-cur-offs dstate)
				   (inst-length inst)))

			  (let ((prefilter (inst-prefilter inst))
				(control (inst-control inst)))
			    (when prefilter
			      (funcall prefilter chunk dstate))

			    (funcall function chunk inst)

			    (setf prefix-p (null (inst-printer inst)))

			    (when control
			      (funcall control chunk inst stream dstate))))))
		 )))))

      (setf (dstate-cur-offs dstate) (dstate-next-offs dstate))

      (unless (null stream)
	(unless prefix-p
	  (print-notes-and-newline stream dstate))
	(setf (dstate-output-state dstate) nil)))))

;;; ----------------------------------------------------------------

(defun add-segment-labels (segment dstate)
  "Make an initial non-printing disassembly pass through DSTATE, noting any
  addresses that are referenced by instructions in this segment."
  ;; add labels at the beginning with a label-number of nil; we'll notice
  ;; later and fill them in (and sort them)
  (declare (type disassem-state dstate))
  (let ((labels (dstate-labels dstate)))
    (map-segment-instructions
     #'(lambda (chunk inst)
	 (declare (type dchunk chunk) (type instruction inst))
	 (let ((labeller (inst-labeller inst)))
	   (when labeller
	     (setf labels (funcall labeller chunk labels dstate)))))
     segment
     dstate)
    (setf (dstate-labels dstate) labels)
    ;; erase any notes that got there by accident
    (setf (dstate-notes dstate) nil)))

(defun number-labels (dstate)
  "If any labels in DSTATE have been added since the last call to this
  function, give them label-numbers, enter them in the hash-table, and make
  sure the label list is in sorted order."
  (let ((labels (dstate-labels dstate)))
    (when (and labels (null (cdar labels)))
      ;; at least one label left un-numbered
      (setf labels (sort labels #'< :key #'car))
      (let ((max -1)
	    (label-hash (dstate-label-hash dstate)))
	(dolist (label labels)
	  (when (not (null (cdr label)))
	    (setf max (max max (cdr label)))))
	(dolist (label labels)
	  (when (null (cdr label))
	    (incf max)
	    (setf (cdr label) max)
	    (setf (gethash (car label) label-hash)
		  (format nil "L~d" max)))))
      (setf (dstate-labels dstate) labels))))

;;; ----------------------------------------------------------------

(defun get-inst-space (params)
  "Get the instruction-space from PARAMS, creating it if necessary."
  (declare (type params params))
  (let ((ispace (params-inst-space params)))
    (when (null ispace)
      (let ((insts nil))
	(maphash #'(lambda (name inst-flavs)
		     (declare (ignore name))
		     (dolist (flav inst-flavs)
		       (push flav insts)))
		 (params-instructions params))
	(setf ispace (build-inst-space insts)))
      (setf (params-inst-space params) ispace))
    ispace))

;;; ----------------------------------------------------------------
;;; add global hooks

(defun add-offs-hook (segment addr hook)
  (let ((entry (cons addr hook)))
    (if (null (seg-hooks segment))
	(setf (seg-hooks segment) (list entry))
	(push entry (cdr (last (seg-hooks segment)))))))

(defun add-offs-note-hook (segment addr note)
  (add-offs-hook segment
		 addr
		 #'(lambda (stream dstate)
		     (declare (type (or null stream) stream)
			      (type disassem-state dstate))
		     (when stream
		       (note note dstate)))))

(defun add-offs-comment-hook (segment addr comment)
  (add-offs-hook segment
		 addr
		 #'(lambda (stream dstate)
		     (declare (type (or null stream) stream)
			      (ignore dstate))
		     (when stream
		       (write-string ";;; " stream)
		       (etypecase comment
			 (string
			  (write-string comment stream))
			 (function
			  (funcall comment stream)))
		       (terpri stream)))))

(defun add-fun-hook (dstate function)
  (push function (dstate-fun-hooks dstate)))

;;; ----------------------------------------------------------------

(defun set-location-printing-range (dstate from length)
  (setf (dstate-addr-print-len dstate)
	;; 4 bits per hex digit
	(ceiling (integer-length (logxor from (+ from length))) 4)))

(defun print-current-address (stream dstate)
  "Print the current address in DSTATE to STREAM, plus any labels that
  correspond to it, and leave the cursor in the instruction column."
  (declare (type stream stream)
	   (type disassem-state dstate))
  (let* ((location
	  (+ (seg-virtual-location (dstate-segment dstate))
	     (dstate-cur-offs dstate)))
	 (location-column-width
	  (params-location-column-width (dstate-params dstate)))
	 (plen (dstate-addr-print-len dstate)))

    (when (null plen)
      (setf plen location-column-width)
      (set-location-printing-range dstate
				  (seg-virtual-location (dstate-segment dstate))
				  (seg-length (dstate-segment dstate))))
    (when (eq (dstate-output-state dstate) :beginning)
      (setf plen location-column-width))

    (fresh-line stream)

    ;; print the location
    ;; [this is equivalent to (format stream "~v,'0x:" plen printed-value), but
    ;;  usually avoids any consing]
    (tab0 (- location-column-width plen) stream)
    (let* ((printed-bits (* 4 plen))
	   (printed-value (ldb (byte printed-bits 0) location))
	   (leading-zeros
	    (truncate (- printed-bits (integer-length printed-value)) 4)))
      (dotimes (i leading-zeros)
	(write-char #\0 stream))
      (unless (zerop printed-value)
	(write printed-value :stream stream :base 16 :radix nil))
      (write-char #\: stream))

    ;; print any labels
    (loop
      (let* ((next-label (car (dstate-cur-labels dstate)))
	     (label-location (car next-label)))
	(when (or (null label-location) (> label-location location))
	  (return))
	(unless (< label-location location)
	  (format stream " L~d:" (cdr next-label)))
	(pop (dstate-cur-labels dstate))))

    ;; move to the instruction column
    (tab0 (+ location-column-width 1 label-column-width) stream)
    ))

;;; ----------------------------------------------------------------

(defmacro with-print-restrictions (&rest body)
  `(let ((*print-pretty* t)
	 (*print-lines* 2)
	 (*print-length* 5)
	 (*print-level* 5))
     ,@body))

(defun print-notes-and-newline (stream dstate)
  "Print a newline to STREAM, inserting any pending notes in DSTATE as
  end-of-line comments.  If there is more than one note, a separate line
  will be used for each one."
  (declare (type stream stream)
	   (type disassem-state dstate))
  (with-print-restrictions
    (dolist (note (dstate-notes dstate))
      (format stream "~vt" *note-column*)
      (pprint-logical-block (stream nil :per-line-prefix "; ")
	(etypecase note
	  (string
	   (write-string note stream))
	  (function
	   (funcall note stream))))
      (terpri stream))
    (fresh-line stream)
    (setf (dstate-notes dstate) nil)))

(defun print-bytes (num stream dstate)
  "Disassemble NUM bytes to STREAM as simple `BYTE' instructions"
  (declare (type offset num)
	   (type stream stream)
	   (type disassem-state dstate))
  (format stream "~a~vt" 'BYTE (dstate-argument-column dstate))
  (let ((sap (dstate-segment-sap dstate))
	(start-offs (dstate-cur-offs dstate)))
    (dotimes (offs num)
      (unless (zerop offs)
	(write-string ", " stream))
      (format stream "#x~2,'0x" (system:sap-ref-8 sap (+ offs start-offs))))))

(defun print-words (num stream dstate)
  "Disassemble NUM machine-words to STREAM as simple `WORD' instructions"
  (declare (type offset num)
	   (type stream stream)
	   (type disassem-state dstate))
  (format stream "~a~vt" 'WORD (dstate-argument-column dstate))
  (let ((sap (dstate-segment-sap dstate))
	(start-offs (dstate-cur-offs dstate))
	(byte-order (dstate-byte-order dstate)))
    (dotimes (word-offs num)
      (unless (zerop word-offs)
	(write-string ", " stream))
      (let ((word 0) (bit-shift 0))
	(dotimes (byte-offs vm:word-bytes)
	  (let ((byte
		 (system:sap-ref-8
			sap
			(+ start-offs (* word-offs vm:word-bytes) byte-offs))))
	    (setf word
		  (if (eq byte-order :big-endian)
		      (+ (ash word vm:byte-bits) byte)
		      (+ word (ash byte bit-shift))))
	    (incf bit-shift vm:byte-bits)))
	(format stream "#x~v,'0x" (ash vm:word-bits -2) word)))))

;;; ----------------------------------------------------------------

(defvar *default-dstate-hooks* (list #'lra-hook))

(defun make-dstate (params &optional (fun-hooks *default-dstate-hooks*))
  "Make a disassembler-state object."
  (declare (type params params))
  (let ((sap
	 ;; a random address
	 (system:vector-sap (coerce #() '(vector (unsigned-byte 8)))))
	(alignment
	 (params-instruction-alignment params))
	(arg-column
	 (+ (or *opcode-column-width* (params-opcode-column-width params) 0)
	    (params-location-column-width params)
	    1
	    label-column-width)))

    (when (> alignment 1)
      (push #'alignment-hook fun-hooks))

    (%make-dstate :segment-sap sap
		  :params params
		  :fun-hooks fun-hooks
		  :argument-column arg-column
		  :alignment alignment 
		  :byte-order (c:backend-byte-order (params-backend params)))))

(defun add-fun-header-hooks (segment)
  (declare (type segment segment))
  (do ((fun (kernel:code-header-ref (seg-code segment)
				    vm:code-entry-points-slot)
	    (fun-next fun))
       (length (seg-length segment)))
      ((null fun)) 
    (let ((offset (code-offs-to-segment-offs (fun-offset fun) segment)))
      (when (<= 0 offset length)
	(push (make-offs-hook :offset offset :function #'fun-header-hook)
	      (seg-hooks segment))))))

;;; ----------------------------------------------------------------
;;; A sap-maker is a no-argument function that returns a sap.

(declaim (inline sap-maker))

(defun sap-maker (function input offset)
  (declare (optimize (speed 3))
	   (type (function (t) system:system-area-pointer) function)
	   (type offset offset))
  (let ((old-sap (system:sap+ (funcall function input) offset)))
    (declare (type system:system-area-pointer old-sap))
    #'(lambda ()
	(let ((new-addr
	       (+ (system:sap-int (funcall function input)) offset)))
	  ;; Saving the sap like this avoids consing except when the sap
	  ;; changes (because the sap-int, arith, etc., get inlined).
	  (declare (type address new-addr))
	  (if (= (system:sap-int old-sap) new-addr)
	      old-sap
	      (setf old-sap (system:int-sap new-addr)))))))

(defun vector-sap-maker (vector offset)
  (declare (optimize (speed 3))
	   (type offset offset))
  (sap-maker #'system:vector-sap vector offset))

(defun code-sap-maker (code offset)
  (declare (optimize (speed 3))
	   (type kernel:code-component code)
	   (type offset offset))
  (sap-maker #'kernel:code-instructions code offset))

(defun memory-sap-maker (address)
  (declare (optimize (speed 3))
	   (type address address))
  (let ((sap (system:int-sap address)))
    #'(lambda () sap)))

;;; ----------------------------------------------------------------

(defun make-segment (sap-maker length
		     &key
		     code virtual-location
		     debug-function source-form-cache
		     hooks)
  "Return a memory segment located at the system-area-pointer returned by
  SAP-MAKER and LENGTH bytes long in the disassem-state object DSTATE.
  Optional keyword arguments include :VIRTUAL-LOCATION (by default the same as
  the address), :DEBUG-FUNCTION, :SOURCE-FORM-CACHE (a source-form-cache
  object), and :HOOKS (a list of offs-hook objects)."
  (declare (type (function () system:system-area-pointer) sap-maker)
	   (type length length)
	   (type (or null address) virtual-location)
	   (type (or null di:debug-function) debug-function)
	   (type (or null source-form-cache) source-form-cache))
  (let* ((segment
	  (%make-segment
	   :sap-maker sap-maker
	   :length length
	   :virtual-location (or virtual-location
				 (system:sap-int (funcall sap-maker)))
	   :hooks hooks
	   :code code)))
    (add-debugging-hooks segment debug-function source-form-cache)
    (add-fun-header-hooks segment)
    segment))

(defun make-vector-segment (vector offset &rest args)
  (declare (type vector vector)
	   (type offset offset)
	   (inline make-segment))
  (apply #'make-segment (vector-sap-maker vector offset) args))

(defun make-code-segment (code offset length &rest args)
  (declare (type kernel:code-component code)
	   (type offset offset)
	   (inline make-segment))
  (apply #'make-segment (code-sap-maker code offset) length :code code args))

(defun make-memory-segment (address &rest args)
  (declare (type address address)
	   (inline make-segment))
  (apply #'make-segment (memory-sap-maker address) args))

;;; ----------------------------------------------------------------

;;; just for fun
(defun print-fun-headers (function)
  (declare (type compiled-function function))
  (let* ((self (fun-self function))
	 (code (kernel:function-code-header self)))
    (format t "Code-header ~s: size: ~s, trace-table-offset: ~s~%"
	    code
	    (kernel:code-header-ref code vm:code-code-size-slot)
	    (kernel:code-header-ref code vm:code-trace-table-offset-slot))
    (do ((fun (kernel:code-header-ref code vm:code-entry-points-slot)
	      (fun-next fun)))
	((null fun))
      (let ((fun-offset (kernel:get-closure-length fun)))
	;; There is function header fun-offset words from the
	;; code header.
	(format t "Fun-header ~s at offset ~d (words): ~s~a => ~s~%"
		fun
		fun-offset
		(kernel:code-header-ref
		 code (+ fun-offset vm:function-name-slot))
		(kernel:code-header-ref
		 code (+ fun-offset vm:function-arglist-slot))
		(kernel:code-header-ref
		 code (+ fun-offset vm:function-type-slot)))))))

;;; ----------------------------------------------------------------
;;; getting at the source code...

(defstruct (source-form-cache (:conc-name sfcache-))
  (debug-source nil :type (or null di:debug-source))
  (top-level-form-index -1 :type fixnum)
  (top-level-form nil :type (or list string))
  (form-number-mapping-table nil :type (or null (vector list)))
  (last-location-retrieved nil :type (or null di:code-location))
  (last-form-retrieved -1 :type fixnum))

(defun get-top-level-form (debug-source tlf-index)
  (let ((name (di:debug-source-name debug-source)))
    (ecase (di:debug-source-from debug-source)
      (:file
       (cond ((not (probe-file name))
	      (warn "The source file ~s no longer seems to exist" name)
	      nil)
	     (t
	      (let ((start-positions
		     (di:debug-source-start-positions debug-source)))
		(cond ((null start-positions)
		       (warn "No start positions map")
		       nil)
		      (t
		       (let* ((local-tlf-index
			       (- tlf-index
				  (di:debug-source-root-number debug-source)))
			      (char-offset
			       (aref start-positions local-tlf-index)))
			 (with-open-file (f name)
			   (cond ((= (di:debug-source-created debug-source)
				     (file-write-date name))
				  (file-position f char-offset))
				 (t
				  (warn "Source file ~s has been modified; ~@
					 Using form offset instead of file index"
					name)
				  (let ((*read-suppress* t))
				    (dotimes (i local-tlf-index) (read f)))))
			   (let ((*readtable* (copy-readtable)))
			     (set-dispatch-macro-character
			      #\# #\.
			      #'(lambda (stream sub-char &rest rest)
				  (declare (ignore rest sub-char))
				  (let ((token (read stream t nil t)))
				    (format nil "#.~s" token))))
			     (read f))
			   ))))))))
      ((:lisp :stream)
       (aref name tlf-index)))))

(defun cache-valid (loc cache)
  (and cache
       (and (eq (di:code-location-debug-source loc)
		(sfcache-debug-source cache))
	    (eq (di:code-location-top-level-form-offset loc)
		(sfcache-top-level-form-index cache)))))

(defun get-source-form (loc context &optional cache)
  (let* ((cache-valid (cache-valid loc cache))
	 (tlf-index (di:code-location-top-level-form-offset loc))
	 (form-number (di:code-location-form-number loc))
	 (top-level-form
	  (if cache-valid
	      (sfcache-top-level-form cache)
	      (get-top-level-form (di:code-location-debug-source loc)
				  tlf-index)))
	 (mapping-table
	  (if cache-valid
	      (sfcache-form-number-mapping-table cache)
	      (di:form-number-translations top-level-form tlf-index))))
    (when (and (not cache-valid) cache)
      (setf (sfcache-debug-source cache) (di:code-location-debug-source loc)
	    (sfcache-top-level-form-index cache) tlf-index
	    (sfcache-top-level-form cache) top-level-form
	    (sfcache-form-number-mapping-table cache) mapping-table))
    (cond ((null top-level-form)
	   nil)
	  ((>= form-number (length mapping-table))
	   (warn "Bogus form-number in form!  The source file has probably ~@
		  been changed too much to cope with")
	   (when cache
	     ;; disable future warnings
	     (setf (sfcache-top-level-form cache) nil))
	   nil)
	  (t
	   (when cache
	     (setf (sfcache-last-location-retrieved cache) loc)
	     (setf (sfcache-last-form-retrieved cache) form-number))
	   (di:source-path-context top-level-form
				   (aref mapping-table form-number)
				   context)))))

(defun get-different-source-form (loc context &optional cache)
  (if (and (cache-valid loc cache)
	   (or (= (di:code-location-form-number loc)
		  (sfcache-last-form-retrieved cache))
	       (and (sfcache-last-location-retrieved cache)
		    (di:code-location=
		     loc
		     (sfcache-last-location-retrieved cache)))))
      (values nil nil)
      (values (get-source-form loc context cache) t)))

;;; ----------------------------------------------------------------
;;; stuff to use debugging-info to augment the disassembly

(defun code-function-map (code)
  (declare (type kernel:code-component code))
  (di::get-debug-info-function-map (kernel:%code-debug-info code)))

(defstruct location-group
  (locations #() :type (vector (or list fixnum)))
  )

(defstruct storage-info
  (groups nil :type list)		; alist of (name . location-group)
  (debug-variables #() :type vector)
  )

(defun dstate-debug-variables (dstate)
  "Return the vector of debug-variables currently associated with DSTATE."
  (declare (type disassem-state dstate))
  (storage-info-debug-variables (seg-storage-info (dstate-segment dstate))))

(defun find-valid-storage-location (offset lg-name dstate)
  "Given the OFFSET of a location within the location-group called LG-NAME,
  see if there's a current mapping to a source variable in DSTATE, and if so,
  return the offset of that variable in the current debug-variable vector."
  (declare (type offset offset)
	   (type symbol lg-name)
	   (type disassem-state dstate))
  (let* ((storage-info
	  (seg-storage-info (dstate-segment dstate)))
	 (location-group
	  (and storage-info
	       (cdr (assoc lg-name (storage-info-groups storage-info)))))
	 (currently-valid
	  (dstate-current-valid-locations dstate)))
    (and location-group
	 (not (null currently-valid))
	 (let ((locations (location-group-locations location-group)))
	   (and (< offset (length locations))
		(let ((used-by (aref locations offset)))
		  (and used-by
		       (let ((debug-var-num
			      (typecase used-by
				(fixnum
				 (and (< used-by (length currently-valid))
				      (not
				       (zerop (bit currently-valid used-by)))
				      used-by))
				(list
				 (some
				  #'(lambda (num)
				      (and (< num (length currently-valid))
					   (not
					    (zerop (bit currently-valid num)))
					   num))
				  used-by)))))
			 (and debug-var-num
			      (progn
				;; Found a valid storage reference!
				;; can't use it again until it's revalidated...
				(setf (bit (dstate-current-valid-locations
					    dstate)
					   debug-var-num)
				      0)
				debug-var-num))
			 ))))))))

(defun grow-vector (vec new-len &optional initial-element)
  "Return a new vector which has the same contents as the old one VEC, plus
  new cells (for a total size of NEW-LEN).  The additional elements are
  initailized to INITIAL-ELEMENT."
  (declare (type vector vec)
	   (type fixnum new-len)) 
  (let ((new
	 (make-sequence `(vector ,(array-element-type vec) ,new-len)
			new-len
			:initial-element initial-element)))
    (dotimes (i (length vec))
      (setf (aref new i) (aref vec i)))
    new))

(defun storage-info-for-debug-function (debug-function)
  "Returns a STORAGE-INFO struction describing the object-to-source
  variable mappings from DEBUG-FUNCTION."
  (declare (type di:debug-function debug-function))
  (let ((sc-vec (c::backend-sc-numbers c:*native-backend*))
	(groups nil)
	(debug-variables (di::debug-function-debug-variables debug-function)))
    (and debug-variables
	 (dotimes (debug-var-offset
		   (length debug-variables)
		   (make-storage-info :groups groups
				      :debug-variables debug-variables))
	   (let ((debug-var (aref debug-variables debug-var-offset)))
	     #+nil
	     (format t ";;; At offset ~d: ~s~%" debug-var-offset debug-var)
	     (let* ((sc-offset
		     (di::compiled-debug-variable-sc-offset debug-var))
		    (sb-name
		     (c:sb-name
		      (c:sc-sb (aref sc-vec (c:sc-offset-scn sc-offset))))))
	       #+nil
	       (format t ";;; SET: ~s[~d]~%"
		       sb-name (c:sc-offset-offset sc-offset))
	       (unless (null sb-name)
		 (let ((group (cdr (assoc sb-name groups))))
		   (when (null group)
		     (setf group (make-location-group))
		     (push `(,sb-name . ,group) groups))
		   (let* ((locations (location-group-locations group))
			  (length (length locations))
			  (offset (c:sc-offset-offset sc-offset)))
		     (when (>= offset length)
		       (setf locations
			     (grow-vector locations
					  (max (* 2 length)
					       (1+ offset))
					  nil)
			     (location-group-locations group)
			     locations))
		     (let ((already-there (aref locations offset)))
		       (cond ((null already-there)
			      (setf (aref locations offset) debug-var-offset))
			     ((eql already-there debug-var-offset))
			     (t
			      (if (listp already-there)
				  (pushnew debug-var-offset
					   (aref locations offset))
				  (setf (aref locations offset)
					(list debug-var-offset
					      already-there)))))
		       )))))))
	 )))

(defun source-available-p (debug-function)
  (handler-case
      (di:do-debug-function-blocks (block debug-function)
	(declare (ignore block))
	(return t))
    (di:no-debug-blocks () nil)))

(defun print-block-boundary (stream dstate)
  (let ((os (dstate-output-state dstate)))
    (when (not (eq os :beginning))
      (when (not (eq os :block-boundary))
	(terpri stream))
      (setf (dstate-output-state dstate)
	    :block-boundary))))

(defun add-source-tracking-hooks (segment debug-function &optional sfcache)
  "Add hooks to track to track the source code in SEGMENT during
  disassembly.  SFCACHE can be either NIL or it can be a SOURCE-FORM-CACHE
  structure, in which case it is used to cache forms from files."
  (declare (type segment segment)
	   (type (or null di:debug-function) debug-function)
	   (type (or null source-form-cache) sfcache))
  (let ((last-block-pc -1)
	#+nil
	(segment-base 
	 (- (seg-virtual-location segment)
	    (sys:sap-int
	     (kernel:code-instructions 
	      (di::compiled-debug-function-component debug-function))))))
    (flet ((add-hook (pc fun &optional before-address)
	     (push (make-offs-hook
		    :offset (code-insts-offs-to-segment-offs pc segment)
		    :function fun
		    :before-address before-address)
		   (seg-hooks segment))))
      (handler-case
	  (di:do-debug-function-blocks (block debug-function)
	    (let ((first-location-in-block-p t))
	      (di:do-debug-block-locations (loc block)
		(let ((pc (di::compiled-code-location-pc loc)))

		  ;; Print the code-location-kind
		  (let ((kind (di::compiled-code-location-kind loc)))
		    (add-hook pc 
			      (lambda (stream dstate)
				(declare (ignore stream))
				(note (format nil "[~S]" kind) dstate))))
		  
		  ;; Put blank lines in at block boundaries
		  (when (and first-location-in-block-p
			     (/= pc last-block-pc))
		    (setf first-location-in-block-p nil)
		    (add-hook pc
			      #'(lambda (stream dstate)
				  (print-block-boundary stream dstate))
			      t)
		    (setf last-block-pc pc))

		  ;; Print out corresponding source; this information is not
		  ;; all that accurate, but it's better than nothing
		  (unless (zerop (di:code-location-form-number loc))
		    (multiple-value-bind (form new)
			(get-different-source-form loc 0 sfcache)
		      (when new
			 (let ((at-block-begin (= pc last-block-pc)))
			   (add-hook
			    pc
			    #'(lambda (stream dstate)
				(declare (ignore dstate))
				(when stream
				  (unless at-block-begin
				    (terpri stream))
				  (pprint-logical-block (stream nil :per-line-prefix ";;; ")
				    (format stream "[~D] "
					    (di:code-location-form-number loc))
				    (prin1-short form stream))
				  (terpri stream)
				  (terpri stream)))
			    t)))))

		  ;; Keep track of variable live-ness as best we can
		  (let ((live-set
			 (copy-seq (di::compiled-code-location-live-set loc))))
		    (add-hook
		     pc
		     #'(lambda (stream dstate)
			 (declare (ignore stream))
			 (setf (dstate-current-valid-locations dstate)
			       live-set)
			 #+nil
			 (note #'(lambda (stream)
				   (let ((*print-length* nil))
				     (format stream "Live set: ~s"
					     live-set)))
			       dstate))))
		  ))))
	(di:no-debug-blocks () nil)))))

(defun add-debugging-hooks (segment debug-function &optional sfcache)
  (when debug-function
    (setf (seg-storage-info segment)
	  (storage-info-for-debug-function debug-function))
    (add-source-tracking-hooks segment debug-function sfcache)
    (let ((kind (di:debug-function-kind debug-function)))
      (flet ((anh (n)
	       (push (make-offs-hook
		      :offset 0
		      :function #'(lambda (stream dstate)
				    (declare (ignore stream))
				    (note n dstate)))
		     (seg-hooks segment))))
	(case kind
	  (:external)
	  ((nil)
	   (anh "No-arg-parsing entry point"))
	  (t
	   (anh #'(lambda (stream)
		    (format stream "~s entry point" kind)))))))))

;;; ----------------------------------------------------------------

(defun fun-header-pc (function)
  "Return the PC of FUNCTION's header."
  (declare (type compiled-function function))
  (let ((code (fun-code function)))
    (* (- (kernel:function-word-offset function)
	  (kernel:get-header-data code)) ; i.e code header length
       vm:word-bytes)))

(defvar *disassemble-flets* t
  "If non-NIL, disassemble flets/labels too")

(defun get-function-segments (function)
  "Returns a list of the segments of memory containing machine code
  instructions for FUNCTION."
  (declare (type compiled-function function))
  (let* ((code (fun-code function))
	 (header-pc (fun-header-pc function))
	 (function-map (code-function-map code))
	 (sfcache (make-source-form-cache)))
    (let ((first-block-seen-p nil)
	  (nil-block-seen-p nil)
	  (last-offset 0)
	  (last-debug-function nil)
	  (segments nil))
      (flet ((add-seg (offs len df)
	       (when (> len 0)
		 (push (make-code-segment code offs len
					  :debug-function df
					  :source-form-cache sfcache)
		       segments))))
	(dotimes (fmap-index (length function-map))
	  (let ((fmap-entry (aref function-map fmap-index)))
	    (etypecase fmap-entry
	      (integer
	       (when first-block-seen-p
		 (add-seg last-offset
			  (- fmap-entry last-offset)
			  last-debug-function)
		 (setf last-debug-function nil))
	       (setf last-offset fmap-entry))
	      (c::compiled-debug-function
	       (let ((start-pc 
		      (c::compiled-debug-function-start-pc fmap-entry))
		     (kind (c::compiled-debug-function-kind fmap-entry)))
		 #+nil
		 (format t ";;; SAW ~s ~s ~s,~s ~d,~d [~d]~%"
			 (c::compiled-debug-function-name fmap-entry)
			 kind first-block-seen-p nil-block-seen-p
			 last-offset start-pc header-pc)
		 (cond ((and (<= last-offset header-pc start-pc)
			     (not first-block-seen-p))
			(setf first-block-seen-p t))
		       ((eq kind :external)
			(let* ((name (c::compiled-debug-function-name fmap-entry))
			       (flet-p (and (listp name)
					    (member (car name) '(flet labels)))))
			  ;; Don't return from loop if this external
			  ;; entry is an flet or labels entry.  We
			  ;; want to continue the disassembly, if
			  ;; enabled.
			  (when (and first-block-seen-p
				     (if *disassemble-flets*
					 (not flet-p)
					 t))
			    (return))))
		       ((eq kind nil)
			;; FIXME: Why do we return when we have a nil
			;; block and have already seen a nil block?
			;; At the very least this prevents the
			;; disassembler from diassembling labels and
			;; flets in a function.
			(when (and nil-block-seen-p (not *disassemble-flets*))
			  (return))
			(when first-block-seen-p
			  (setf nil-block-seen-p t))))
		 (setf last-debug-function
		       (di::make-compiled-debug-function fmap-entry code)))))))
	(let ((max-offset (code-inst-area-length code)))
	  (when (and first-block-seen-p last-debug-function)
	    (add-seg last-offset
		     (- max-offset last-offset)
		     last-debug-function))
	  (if (null segments)
	      (let ((offs (fun-insts-offset function)))
		(make-code-segment code offs (- max-offset offs)))
	      (nreverse segments)))))))

(defun get-code-segments (code
			  &optional
			  (start-offs 0)
			  (length (code-inst-area-length code)))
  "Returns a list of the segments of memory containing machine code
  instructions for the code-component CODE.  If START-OFFS and/or LENGTH is
  supplied, only that part of the code-segment is used (but these are
  constrained to lie within the code-segment)."
  (declare (type kernel:code-component code)
	   (type offset start-offs)
	   (type length length))
  (let ((segments nil))
    (when code
      (let ((function-map (code-function-map code))
	    (sfcache (make-source-form-cache)))
	(let ((last-offset 0)
	      (last-debug-function nil))
	  (flet ((add-seg (offs len df)
		   (let* ((restricted-offs
			   (min (max start-offs offs) (+ start-offs length)))
			  (restricted-len
			   (- (min (max start-offs (+ offs len))
				   (+ start-offs length))
			      restricted-offs)))
		     (when (> restricted-len 0)
		       (push (make-code-segment code
						restricted-offs restricted-len
						:debug-function df
						:source-form-cache sfcache)
			     segments)))))
	    (dotimes (fmap-index (length function-map))
	      (let ((fmap-entry (aref function-map fmap-index)))
		(etypecase fmap-entry
		  (integer
		   (add-seg last-offset (- fmap-entry last-offset)
			    last-debug-function)
		   (setf last-debug-function nil)
		   (setf last-offset fmap-entry))
		  (c::compiled-debug-function
		   (setf last-debug-function
			 (di::make-compiled-debug-function fmap-entry code)))
		  )))
	    (when last-debug-function
	      (add-seg last-offset
		       (- (code-inst-area-length code) last-offset)
		       last-debug-function))))))
    (if (null segments)
	(make-code-segment code start-offs length)
	(nreverse segments))))

;;; ----------------------------------------------------------------

#+nil
(defun find-function-segment (fun)
  "Return the address of the instructions for function and its length.
  The length is computed using a heuristic, and so may not be accurate."
  (declare (type compiled-function fun))
  (let* ((code
	  (fun-code fun))
	 (fun-addr
	  (- (kernel:get-lisp-obj-address fun) vm:function-pointer-type))
	 (max-length
	  (code-inst-area-length code))
	 (upper-bound
	  (+ (code-inst-area-address code) max-length)))
    (do ((some-fun (code-first-function code)
		   (fun-next some-fun)))
	((null some-fun)
	 (values fun-addr (- upper-bound fun-addr)))
      (let ((some-addr (fun-address some-fun)))
	(when (and (> some-addr fun-addr)
		   (< some-addr upper-bound))
	  (setf upper-bound some-addr))))))

;;; ----------------------------------------------------------------

(defun segment-overflow (segment dstate)
  "Returns two values:  the amount by which the last instruction in the
  segment goes past the end of the segment, and the offset of the end of the
  segment from the beginning of that instruction.  If all instructions fit
  perfectly, this will return 0 and 0."
  (declare (type segment segment)
	   (type disassem-state dstate))
  (let ((seglen (seg-length segment))
	(last-start 0))
    (map-segment-instructions #'(lambda (chunk inst)
				  (declare (ignore chunk inst))
				  (setf last-start (dstate-cur-offs dstate)))
			      segment
			      dstate)
    (values (- (dstate-cur-offs dstate) seglen)
	    (- seglen last-start))))
  
(defun label-segments (seglist dstate)
  "Computes labels for all the memory segments in SEGLIST and adds them to
  DSTATE.  It's important to call this function with all the segments you're
  interested in, so it can find references from one to another."
  (declare (type list seglist)
	   (type disassem-state dstate))
  (dolist (seg seglist)
    (add-segment-labels seg dstate))
  ;; now remove any labels that don't point anywhere in the segments we have
  (setf (dstate-labels dstate)
	(remove-if #'(lambda (lab)
		       (not
			(some #'(lambda (seg)
				  (let ((start (seg-virtual-location seg)))
				    (<= start
					(car lab)
					(+ start (seg-length seg)))))
			      seglist)))
		   (dstate-labels dstate))))

(defun disassemble-segment (segment stream dstate)
  "Disassemble the machine code instructions in SEGMENT to STREAM."
  (declare (type segment segment)
	   (type stream stream)
	   (type disassem-state dstate))
  (let ((*print-pretty* nil)) ; otherwise the pp conses hugely
    (number-labels dstate)
    (map-segment-instructions
     #'(lambda (chunk inst)
	 (declare (type dchunk chunk) (type instruction inst))
	 (let ((printer (inst-printer inst)))
	   (when printer
	     (funcall printer chunk inst stream dstate))))
     segment
     dstate
     stream)))

(defun disassemble-segments (segments stream dstate)
  "Disassemble the machine code instructions in each memory segment in
  SEGMENTS in turn to STREAM."
  (declare (type list segments)
	   (type stream stream)
	   (type disassem-state dstate))
  (unless (null segments)
    (let ((first (car segments))
	  (last (car (last segments))))
      (set-location-printing-range dstate
				  (seg-virtual-location first)
				  (- (+ (seg-virtual-location last)
					(seg-length last))
				     (seg-virtual-location first)))
      (setf (dstate-output-state dstate) :beginning)
      #+sparc
      (progn
	;; Initialize these to a sane value, just in case.
	(setf vm::*note-sethi-inst* nil)
	(setf vm::*pseudo-atomic-set* nil))
      #+ppc
      (progn
	;; Initialize these to a sane value, just in case.
	(setf vm::*note-addis-inst* nil)
	(setf vm::*pseudo-atomic-set* nil))
      (dolist (seg segments)
	(disassemble-segment seg stream dstate)))))

;;; ----------------------------------------------------------------
;;; top-level functions

(defun disassemble-function (function &key (stream *standard-output*)
				      (use-labels t)
				      (backend c:*native-backend*))
  "Disassemble the machine code instructions for FUNCTION."
  (declare (type compiled-function function)
	   (type stream stream)
	   (type (member t nil) use-labels)
	   (type c::backend backend))
  (let* ((dstate (make-dstate (c:backend-disassem-params backend)))
	 (segments (get-function-segments function)))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

(defun compile-function-lambda-expr (function)
  (declare (type function function))
  (multiple-value-bind
      (lambda closurep name)
      (function-lambda-expression function)
    (declare (ignore name))
    (when closurep
      (error "Cannot compile a lexical closure"))
    (compile nil lambda)))

(defun compiled-function-or-lose (thing &optional (name thing))
  (cond ((valid-function-name-p thing)
	 (compiled-function-or-lose (fdefinition thing) thing))
	((eval:interpreted-function-p thing)
	 (compile-function-lambda-expr thing))
	((functionp thing)
	 thing)
	((and (listp thing)
	      (eq (car thing) 'lisp::lambda))
	 (compile nil thing))
	(t
	 (error 'simple-type-error
		:datum name
		:expected-type '(satisfies valid-function-name-p)
		:format-control "Can't make a compiled function from ~S"
		:format-arguments (list name)))))

(defun disassemble (object &key (stream *standard-output*)
			   (use-labels t)
			   (backend c:*native-backend*))
  "Disassemble the machine code associated with OBJECT, which can be a
  function, a lambda expression, or a symbol with a function definition.  If
  it is not already compiled, the compiler is called to produce something to
  disassemble."
  (declare (type (or function symbol cons) object)
	   (type (or (member t) stream) stream)
	   (type (member t nil) use-labels)
	   (type c::backend backend))
  (let ((fun (compiled-function-or-lose object)))
    (if (typep fun 'kernel:byte-function)
	(c:disassem-byte-fun fun)
	;; we can't detect closures, so be careful
	(disassemble-function (fun-self fun)
			      :stream stream
			      :use-labels use-labels
			      :backend backend)))
  (values))

(defun disassemble-memory (address
			   length
			   &key
			   (stream *standard-output*)
			   code-component
			   (use-labels t)
			   (backend c:*backend*))
  "Disassembles the given area of memory starting at ADDRESS and LENGTH long.
  Note that if CODE-COMPONENT is NIL and this memory could move during a GC,
  you'd better disable it around the call to this function."
  (declare (type (or address system:system-area-pointer) address)
	   (type length length)
	   (type stream stream)
	   (type (or null kernel:code-component) code-component)
	   (type (member t nil) use-labels)
	   (type c::backend backend))
  (let*	((address
	  (if (system:system-area-pointer-p address)
	      (system:sap-int address)
	      address))
	 (dstate (make-dstate (c:backend-disassem-params backend)))
	 (segments
	  (if code-component
	      (let ((code-offs
		     (- address
			(system:sap-int
			 (kernel:code-instructions code-component)))))
		(when (or (< code-offs 0)
			  (> code-offs (code-inst-area-length code-component)))
		  (error "Address ~x not in the code component ~s."
			 address code-component))
		(get-code-segments code-component code-offs length))
	      (list (make-memory-segment address length)))))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

(defun disassemble-code-component (code-component &key
						  (stream *standard-output*)
						  (use-labels t)
						  (backend c:*native-backend*))
  "Disassemble the machine code instructions associated with
  CODE-COMPONENT (this may include multiple entry points)."
  (declare (type (or null kernel:code-component compiled-function)
		 code-component)
	   (type stream stream)
	   (type (member t nil) use-labels)
	   (type c::backend backend))
  (let*	((code-component
	  (if (functionp code-component)
	      (fun-code code-component)
	      code-component))
	 (dstate (make-dstate (c:backend-disassem-params backend)))
	 (segments (get-code-segments code-component)))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

;;; ----------------------------------------------------------------
;;; Code for making useful segments from arbitrary lists of code-blocks

;;; The maximum size of an instruction -- this includes pseudo-instructions
;;; like error traps with their associated operands, so it should be big enough
;;; to include them (i.e. it's not just 4 on a risc machine!).
(defconstant max-instruction-size 16)

(defun sap-to-vector (sap start end)
    (let* ((length (- end start))
	   (result (make-array length :element-type '(unsigned-byte 8)))
	   (sap (system:sap+ sap start)))
      (dotimes (i length)
	(setf (aref result i) (system:sap-ref-8 sap i)))
      result))

(defun add-block-segments (sap amount seglist location connecting-vec dstate)
  (declare (type list seglist)
	   (type integer location)
	   (type (or null (vector (unsigned-byte 8))) connecting-vec)
	   (type disassem-state dstate))
  (flet ((addit (seg overflow)
	   (let ((length (+ (seg-length seg) overflow)))
	     (when (> length 0)
	       (setf (seg-length seg) length)
	       (incf location length)
	       (push seg seglist)))))
    (let ((connecting-overflow 0))
      (when connecting-vec
	;; tack on some of the new block to the old overflow vector
	(let* ((beginning-of-block-amount
		(if sap (min max-instruction-size amount) 0))
	       (connecting-vec
		(if sap
		    (concatenate
		     '(vector (unsigned-byte 8))
		     connecting-vec
		     (sap-to-vector sap 0 beginning-of-block-amount))
		    connecting-vec)))
	  (when (and (< (length connecting-vec) max-instruction-size)
		     (not (null sap)))
	    (return-from add-block-segments
	      ;; We want connecting vectors to be large enough to hold
	      ;; any instruction, and since the current sap wasn't large
	      ;; enough to do this (and is now entirely on the end of the
	      ;; overflow-vector), just save it for next time.
	      (values seglist location connecting-vec)))
	  (when (> (length connecting-vec) 0)
	    (let ((seg
		   (make-vector-segment connecting-vec
					0
					(- (length connecting-vec)
					   beginning-of-block-amount)
					:virtual-location location)))
	      (setf connecting-overflow (segment-overflow seg dstate))
	      (addit seg connecting-overflow)))))
      (cond ((null sap)
	     ;; Nothing more to add.
	     (values seglist location nil))
	    ((< (- amount connecting-overflow) max-instruction-size)
	     ;; We can't create a segment with the minimum size
	     ;; required for an instruction, so just keep on accumulating
	     ;; in the overflow vector for the time-being.
	     (values seglist
		     location
		     (sap-to-vector sap connecting-overflow amount)))
	    (t
	     ;; Put as much as we can into a new segment, and the rest
	     ;; into the overflow-vector.
	     (let* ((initial-length
		     (- amount connecting-overflow max-instruction-size))
		    (seg
		     (make-segment #'(lambda ()
				       (system:sap+ sap connecting-overflow))
				   initial-length
				   :virtual-location location))
		    (overflow
		     (segment-overflow seg dstate)))
	       (addit seg overflow)
	       (values seglist
		       location
		       (sap-to-vector sap
				      (+ connecting-overflow (seg-length seg))
				      amount))))))))

;;; ----------------------------------------------------------------
;;; Code to disassemble assembler segments.

(defun assem-segment-to-disassem-segments (assem-segment dstate)
  (declare (type new-assem:segment assem-segment)
	   (type disassem-state dstate))
  (let ((location 0)
	(disassem-segments nil)
	(connecting-vec nil))
    (new-assem:segment-map-output
     assem-segment
     #'(lambda (sap amount)
	 (multiple-value-setq (disassem-segments location connecting-vec)
	   (add-block-segments sap amount
			       disassem-segments location
			       connecting-vec
			       dstate))))
    (when connecting-vec
      (setf disassem-segments
	    (add-block-segments nil nil
				disassem-segments location
				connecting-vec
				dstate)))
    (sort disassem-segments #'< :key #'seg-virtual-location))) 

(defun disassemble-assem-segment (assem-segment stream backend)
  "Disassemble the machine code instructions associated with
  ASSEM-SEGMENT (of type new-assem:segment)."
  (declare (type new-assem:segment assem-segment)
	   (type stream stream)
	   (type c::backend backend))
  (let* ((dstate (make-dstate (c:backend-disassem-params backend)))
	 (disassem-segments
	  (assem-segment-to-disassem-segments assem-segment dstate)))
    (label-segments disassem-segments dstate)
    (disassemble-segments disassem-segments stream dstate)))

;;; ----------------------------------------------------------------
;;; Routines to find things in the lisp environment.  Obviously highly
;;; implementation specific!

(defconstant groked-symbol-slots
  (sort `((,vm:symbol-value-slot . symbol-value)
	  (,vm:symbol-plist-slot . symbol-plist)
	  (,vm:symbol-name-slot . symbol-name)
	  (,vm:symbol-package-slot . symbol-package))
	#'<
	:key #'car)
  "An alist of (SYMBOL-SLOT-OFFSET . ACCESS-FUNCTION-NAME) for slots in a
symbol object that we know about.")

(defun grok-symbol-slot-ref (address)
  "Given ADDRESS, try and figure out if which slot of which symbol is being
  refered to.  Of course we can just give up, so it's not a big deal...
  Returns two values, the symbol and the name of the access function of the
  slot."
  (declare (type address address))
  (if (not (aligned-p address vm:word-bytes))
      (values nil nil)
      (do ((slots-tail groked-symbol-slots (cdr slots-tail)))
	  ((null slots-tail)
	   (values nil nil))
	(let* ((field (car slots-tail))
	       (slot-offset (words-to-bytes (car field)))
	       (maybe-symbol-addr (- address slot-offset))
	       (maybe-symbol
		(kernel:make-lisp-obj
		 (+ maybe-symbol-addr vm:other-pointer-type))))
	  (when (symbolp maybe-symbol)
	    (return (values maybe-symbol (cdr field))))))))

(defconstant nil-addr (kernel:get-lisp-obj-address nil))

(defun grok-nil-indexed-symbol-slot-ref (byte-offset)
  "Given a BYTE-OFFSET from NIL, try and figure out if which slot of which
  symbol is being refered to.  Of course we can just give up, so it's not a big
  deal...  Returns two values, the symbol and the access function."
  (declare (type offset byte-offset))
  (grok-symbol-slot-ref (+ nil-addr byte-offset)))

(defun get-nil-indexed-object (byte-offset)
  "Returns the lisp object located BYTE-OFFSET from NIL."
  (declare (type offset byte-offset))
  (kernel:make-lisp-obj (+ nil-addr byte-offset)))

(defun get-code-constant (byte-offset dstate)
  "Returns two values; the lisp-object located at BYTE-OFFSET in the constant
  area of the code-object in the current segment and T, or NIL and NIL if
  there is no code-object in the current segment."
  (declare (type offset byte-offset)
	   (type disassem-state dstate))
  (let ((code (seg-code (dstate-segment dstate))))
    (if code
	(values
	 (kernel:code-header-ref code
				 (ash (+ byte-offset vm:other-pointer-type)
				      (- vm:word-shift)))
	 t)
	(values nil nil))))

(defun get-code-constant-absolute (addr dstate)
  (declare (type address addr))
  (declare (type disassem-state dstate))
  (let ((code (seg-code (dstate-segment dstate))))
    (if (null code)
	(return-from get-code-constant-absolute (values nil nil)))
    (let ((code-size (ash (kernel:get-header-data code) vm:word-shift)))
      (system:without-gcing
       (let ((code-addr (- (kernel:get-lisp-obj-address code)
			   vm:other-pointer-type)))
	 (if (or (< addr code-addr) (>= addr (+ code-addr code-size)))
	     (values nil nil)
	     (values (kernel:code-header-ref
			code
			(ash (- addr code-addr) (- vm:word-shift)))
		       t)))))))


(defvar *assembler-routines-by-addr* nil)

(defvar *foreign-symbols-by-addr* nil)

(defun invert-address-hash (htable &optional (addr-hash (make-hash-table)))
  "Build an address-name hash-table from the name-address hash"
  (maphash #'(lambda (name address)
	       (setf (gethash address addr-hash) name))
	     htable)
  addr-hash)

(defun find-assembler-routine (address)
  "Returns the name of the primitive lisp assembler routine or foreign
  symbol located at ADDRESS, or NIL if there isn't one."
  (declare (type address address))
  (when (null *assembler-routines-by-addr*)
    (setf *assembler-routines-by-addr*
	  (invert-address-hash lisp::*assembler-routines*))
    (setf *assembler-routines-by-addr*
	  (invert-address-hash lisp::*foreign-symbols*
			       *assembler-routines-by-addr*)))
  (gethash address *assembler-routines-by-addr*))

;;; ----------------------------------------------------------------
;;; some handy function for machine-dependent code to use...

(declaim (maybe-inline sap-ref-int read-suffix))

(defun sap-ref-int (sap offset length byte-order)
  (declare (type system:system-area-pointer sap)
	   (type (unsigned-byte 16) offset)
	   (type (member 1 2 4) length)
	   (type (member :little-endian :big-endian) byte-order)
	   (optimize (speed 3) (safety 0)))
  (ecase length
    (1 (system:sap-ref-8 sap offset))
    (2 (if (eq byte-order :big-endian)
	   (+ (ash (system:sap-ref-8 sap offset) 8)
	      (system:sap-ref-8 sap (+ offset 1)))
	   (+ (ash (system:sap-ref-8 sap (+ offset 1)) 8)
	      (system:sap-ref-8 sap offset))))
    (4 (if (eq byte-order :big-endian)
	   (+ (ash (system:sap-ref-8 sap offset) 24)
	      (ash (system:sap-ref-8 sap (+ 1 offset)) 16)
	      (ash (system:sap-ref-8 sap (+ 2 offset)) 8)
	      (system:sap-ref-8 sap (+ 3 offset)))
	   (+ (system:sap-ref-8 sap offset)
	      (ash (system:sap-ref-8 sap (+ 1 offset)) 8)
	      (ash (system:sap-ref-8 sap (+ 2 offset)) 16)
	      (ash (system:sap-ref-8 sap (+ 3 offset)) 24))))))

(defun read-suffix (length dstate)
  (declare (type (member 8 16 32) length)
	   (type disassem-state dstate)
	   (optimize (speed 3) (safety 0)))
  (let ((length (ecase length (8 1) (16 2) (32 4))))
    (declare (type (unsigned-byte 3) length))
    (prog1
      (sap-ref-int (dstate-segment-sap dstate)
		   (dstate-next-offs dstate)
		   length
		   (dstate-byte-order dstate))
      (incf (dstate-next-offs dstate) length))))

(defun read-signed-suffix (length dstate)
  (declare (type (member 8 16 32) length)
	   (type disassem-state dstate)
	   (optimize (speed 3) (safety 0)))
  (sign-extend (read-suffix length dstate) length))

;;; ----------------------------------------------------------------
;;; optional routines to make notes about code

(defun note (note dstate)
  "Store NOTE (which can be either a string or a function with a single
  stream argument) to be printed as an end-of-line comment after the current
  instruction is disassembled."
  (declare (type (or string function) note)
	   (type disassem-state dstate))
  (push note (dstate-notes dstate)))

(defun prin1-short (thing stream)
  (with-print-restrictions
    (prin1 thing stream)))

(defun prin1-quoted-short (thing stream)
  (if (self-evaluating-p thing)
      (prin1-short thing stream)
      (prin1-short `',thing stream)))

(defun note-code-constant (byte-offset dstate)
  "Store a note about the lisp constant located BYTE-OFFSET bytes from the
  current code-component, to be printed as an end-of-line comment after the
  current instruction is disassembled."
  (declare (type offset byte-offset)
	   (type disassem-state dstate))
  (multiple-value-bind (const valid)
      (get-code-constant byte-offset dstate)
    (when valid
      (note #'(lambda (stream)
		(prin1-quoted-short const stream))
	    dstate))
    const))

(defun note-code-constant-absolute (addr dstate)
  "Store a note about the lisp constant located at ADDR in the
  current code-component, to be printed as an end-of-line comment after the
  current instruction is disassembled."
  (declare (type address addr)
	   (type disassem-state dstate))
  (multiple-value-bind (const valid)
      (get-code-constant-absolute addr dstate)
    (when valid
      (note #'(lambda (stream)
		(prin1-quoted-short const stream))
	    dstate))
    (values const valid)))

(defun maybe-note-nil-indexed-symbol-slot-ref (nil-byte-offset dstate)
  "If the memory address located NIL-BYTE-OFFSET bytes from the constant NIL
  is a valid slot in a symbol, store a note describing which symbol and slot,
  to be printed as an end-of-line comment after the current instruction is
  disassembled.  Returns non-NIL iff a note was recorded."
  (declare (type offset nil-byte-offset)
	   (type disassem-state dstate))
  (multiple-value-bind (symbol access-fun)
      (grok-nil-indexed-symbol-slot-ref nil-byte-offset)
    (when access-fun
      (note #'(lambda (stream)
		(prin1 (if (eq access-fun 'symbol-value)
			   symbol
			   `(,access-fun ',symbol))
		       stream))
	    dstate))
    access-fun))

(defun maybe-note-nil-indexed-object (nil-byte-offset dstate)
  "If the memory address located NIL-BYTE-OFFSET bytes from the constant NIL
  is a valid lisp object, store a note describing which symbol and slot, to
  be printed as an end-of-line comment after the current instruction is
  disassembled.  Returns non-NIL iff a note was recorded."
  (declare (type offset nil-byte-offset)
	   (type disassem-state dstate))
  (let ((obj (get-nil-indexed-object nil-byte-offset)))
    (note #'(lambda (stream)
	      (prin1-quoted-short obj stream))
	  dstate)
    t))

(defun maybe-note-assembler-routine (address note-address-p dstate)
  "If ADDRESS is the address of a primitive assembler routine or
  foreign symbol, store a note describing which one, to be printed as
  an end-of-line comment after the current instruction is disassembled.
  Returns non-NIL iff a note was recorded.  If NOTE-ADDRESS-P is non-NIL, a
  note of the address is also made." 
  (declare (type disassem-state dstate))
  (unless (typep address 'address)
    (return-from maybe-note-assembler-routine nil))
  (let ((name (or
	       #+linkage-table (lisp::find-foreign-symbol address)
	       (find-assembler-routine address))))
    (unless (null name)
      (note #'(lambda (stream)
		(if NOTE-ADDRESS-P
		    (format stream "#x~8,'0x: ~a" address name)
		    (princ name stream)))
	    dstate))
    name))

(defun maybe-note-static-function (nil-byte-offset dstate)
  "If NIL-BYTE-OFFSET is the offset of static function, store a note
  describing which one, to be printed as an end-of-line comment after
  the current instruction is disassembled.  Returns non-NIL iff a note
  was recorded."
  (declare (type offset nil-byte-offset)
	   (type disassem-state dstate))
  (let ((sym (ignore-errors (vm::offset-static-function nil-byte-offset))))
    (when sym
      (note #'(lambda (stream)
		  (princ sym stream))
	    dstate))
    sym))

(defun maybe-note-single-storage-ref (offset sc-name dstate)
  "If there's a valid mapping from OFFSET in the storage class SC-NAME to a
  source variable, make a note of the source-variable name, to be printed as
  an end-of-line comment after the current instruction is disassembled.
  Returns non-NIL iff a note was recorded."
  (declare (type offset offset)
	   (type symbol sc-name)
	   (type disassem-state dstate))
  (let ((storage-location
	 (find-valid-storage-location offset sc-name dstate)))
    (when storage-location
      (note #'(lambda (stream)
		(princ (di:debug-variable-symbol
			(aref (storage-info-debug-variables
			       (seg-storage-info (dstate-segment dstate)))
			      storage-location))
		       stream))
	    dstate)
      t)))

(defun maybe-note-associated-storage-ref (offset sb-name assoc-with dstate)
  "If there's a valid mapping from OFFSET in the storage-base called SB-NAME
  to a source variable, make a note equating ASSOC-WITH with the
  source-variable name, to be printed as an end-of-line comment after the
  current instruction is disassembled.  Returns non-NIL iff a note was
  recorded."
  (declare (type offset offset)
	   (type symbol sb-name)
	   (type (or symbol string) assoc-with)
	   (type disassem-state dstate))
  (let ((storage-location
	 (find-valid-storage-location offset sb-name dstate)))
    (when storage-location
      (note #'(lambda (stream)
		(format stream "~a = ~s"
			assoc-with
			(di:debug-variable-symbol
			 (aref (dstate-debug-variables dstate)
			       storage-location))))
	    dstate)
      t)))

;;; ----------------------------------------------------------------
;;; these should be somewhere else...

(defun get-error-name (errnum backend)
  (car (svref (c:backend-internal-errors backend) errnum)))

(defun get-sc-name (sc-offs backend)
  (c::location-print-name
   (c::make-random-tn :kind :normal
		      :sc (svref (c::backend-sc-numbers backend)
				 (c:sc-offset-scn sc-offs))
		      :offset (c:sc-offset-offset sc-offs))))

;;; ----------------------------------------------------------------

(defun handle-break-args (error-parse-fun stream dstate)
  "When called from an error break instruction's :DISASSEM-CONTROL (or
  :DISASSEM-PRINTER) function, will correctly deal with printing the
  arguments to the break.

  ERROR-PARSE-FUN should be a function that accepts:
    1) a SYSTEM-AREA-POINTER
    2) a BYTE-OFFSET from the SAP to begin at
    3) optionally, LENGTH-ONLY, which if non-NIL, means to only return
       the byte length of the arguments (to avoid unnecessary consing)
  It should read information from the SAP starting at BYTE-OFFSET, and return
  four values:
    1) the error number
    2) the total length, in bytes, of the information
    3) a list of SC-OFFSETs of the locations of the error parameters
    4) a list of the length (as read from the SAP), in bytes, of each of the
       return-values." 
  (declare (type function error-parse-fun)
	   (type (or null stream) stream)
	   (type disassem-state dstate))
  (multiple-value-bind (errnum adjust sc-offsets lengths)
      (funcall error-parse-fun
	       (dstate-segment-sap dstate)
	       (dstate-next-offs dstate)
	       (null stream))
    (when stream
      (setf (dstate-cur-offs dstate)
	    (dstate-next-offs dstate))
      (flet ((emit-err-arg (note)
	       (let ((num (pop lengths)))
		 (print-notes-and-newline stream dstate)
		 (print-current-address stream dstate)
		 (print-bytes num stream dstate)
		 (incf (dstate-cur-offs dstate) num)
		 (when note
		   (note note dstate)))))
	(let ((backend (params-backend (dstate-params dstate))))
	  (emit-err-arg nil)
	  (emit-err-arg (symbol-name (get-error-name errnum backend)))
	  (dolist (sc-offs sc-offsets)
	    (emit-err-arg (get-sc-name sc-offs backend)))))
      )
    (incf (dstate-next-offs dstate) adjust)
    ))
