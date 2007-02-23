;;; -*- Package: ALIEN -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/alieneval.lisp,v 1.64 2005/11/11 22:30:38 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains any the part of the Alien implementation that
;;; is not part of the compiler.
;;;
(in-package "ALIEN")
(use-package "EXT")
(use-package "SYSTEM")

(export '(alien * array struct union enum function integer signed unsigned
	  boolean values single-float double-float long-float
	  system-area-pointer def-alien-type def-alien-variable sap-alien
	  extern-alien with-alien slot deref addr cast alien-sap alien-size
	  alien-funcall def-alien-routine make-alien free-alien
	  null-alien
	  def-callback callback
	  callback-funcall))

(in-package "ALIEN-INTERNALS")
(in-package "ALIEN")

(import '(alien alien-value alien-value-type parse-alien-type
	  unparse-alien-type alien-type-= alien-subtype-p alien-typep

	  def-alien-type-class def-alien-type-translator def-alien-type-method
	  invoke-alien-type-method

	  alien-type alien-type-p alien-type-bits alien-type-alignment
	  alien-integer-type alien-integer-type-p alien-integer-type-signed
	  alien-boolean-type alien-boolean-type-p
	  alien-enum-type alien-enum-type-p
	  alien-float-type alien-float-type-p
	  alien-single-float-type alien-single-float-type-p
	  alien-double-float-type alien-double-float-type-p
	  alien-long-float-type alien-long-float-type-p
	  alien-pointer-type alien-pointer-type-p alien-pointer-type-to
	  make-alien-pointer-type
	  alien-array-type alien-array-type-p alien-array-type-element-type
	  alien-array-type-dimensions	  
	  alien-record-type alien-record-type-p alien-record-type-fields
	  alien-record-field alien-record-field-p alien-record-field-name
	  alien-record-field-type alien-record-field-offset
	  alien-function-type alien-function-type-p make-alien-function-type
	  alien-function-type-result-type alien-function-type-arg-types
	  alien-values-type alien-values-type-p alien-values-type-values
	  *values-type-okay*

	  %set-slot %slot-addr %set-deref %deref-addr

	  %heap-alien %set-heap-alien %heap-alien-addr
	  heap-alien-info heap-alien-info-p heap-alien-info-type
	  heap-alien-info-sap-form

	  local-alien %set-local-alien %local-alien-addr
	  local-alien-info local-alien-info-p local-alien-info-type
	  local-alien-info-force-to-memory-p
	  %local-alien-forced-to-memory-p
	  make-local-alien dispose-local-alien note-local-alien-type

	  %cast %sap-alien align-offset

	  extract-alien-value deposit-alien-value naturalize deport
	  compute-lisp-rep-type compute-alien-rep-type
	  compute-extract-lambda compute-deposit-lambda
	  compute-naturalize-lambda compute-deport-lambda)
	"ALIEN-INTERNALS")

(export '(alien alien-value alien-value-type parse-alien-type
	  unparse-alien-type alien-type-= alien-subtype-p alien-typep

	  def-alien-type-class def-alien-type-translator def-alien-type-method
	  invoke-alien-type-method

	  alien-type alien-type-p alien-type-bits alien-type-alignment
	  alien-integer-type alien-integer-type-p alien-integer-type-signed
	  alien-boolean-type alien-boolean-type-p
	  alien-enum-type alien-enum-type-p
	  alien-float-type alien-float-type-p
	  alien-single-float-type alien-single-float-type-p
	  alien-double-float-type alien-double-float-type-p
	  alien-long-float-type alien-long-float-type-p
	  alien-pointer-type alien-pointer-type-p alien-pointer-type-to
	  make-alien-pointer-type
	  alien-array-type alien-array-type-p alien-array-type-element-type
	  alien-array-type-dimensions	  
	  alien-record-type alien-record-type-p alien-record-type-fields
	  alien-record-field alien-record-field-p alien-record-field-name
	  alien-record-field-type alien-record-field-offset
	  alien-function-type alien-function-type-p make-alien-function-type
	  alien-function-type-result-type alien-function-type-arg-types
	  alien-values-type alien-values-type-p alien-values-type-values
	  *values-type-okay*

	  %set-slot %slot-addr %set-deref %deref-addr

	  %heap-alien %set-heap-alien %heap-alien-addr
	  heap-alien-info heap-alien-info-p heap-alien-info-type
	  heap-alien-info-sap-form

	  local-alien %set-local-alien %local-alien-addr
	  local-alien-info local-alien-info-p local-alien-info-type
	  local-alien-info-force-to-memory-p
	  %local-alien-forced-to-memory-p
	  make-local-alien dispose-local-alien note-local-alien-type

	  %cast %sap-alien align-offset

	  extract-alien-value deposit-alien-value naturalize deport
	  compute-lisp-rep-type compute-alien-rep-type
	  compute-extract-lambda compute-deposit-lambda
	  compute-naturalize-lambda compute-deport-lambda)
	"ALIEN-INTERNALS")



;;;; Utility functions.

(defun align-offset (offset alignment)
  (let ((extra (rem offset alignment)))
    (if (zerop extra) offset (+ offset (- alignment extra)))))

;; This guesses the alignment of some object, using the natural
;; alignment.
(defun guess-alignment (bits)
  (cond ((null bits) nil)
	#-x86 ((> bits 32) 64)
	((> bits 16) 32)
	((> bits 8) 16)
	((> bits 1) 8)
	(t 1)))

;; This is mostly for ppc.  The first object in a structure gets the
;; natural alignment, but all subsequent objects get an alignment of
;; at most 4 bytes (32 bits).
;;
;; For more details, see the Power alignment mode in
;; http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/index.html
;; for details.
;; 
(defun embedded-alignment (bits &optional firstp)
  (let ((align (guess-alignment bits)))
    (if firstp
	align
	(min align #+(and ppc darwin) 32
	           #-(and ppc darwin) 64))))


;;;; Alien-type-info stuff.

(eval-when (compile eval load)

(defstruct (alien-type-class
	    (:print-function %print-alien-type-class))
  (name nil :type symbol)
  (include nil :type (or null alien-type-class))
  (unparse nil :type (or null function))
  (type= nil :type (or null function))
  (lisp-rep nil :type (or null function))
  (alien-rep nil :type (or null function))
  (extract-gen nil :type (or null function))
  (deposit-gen nil :type (or null function))
  (naturalize-gen nil :type (or null function))
  (deport-gen nil :type (or null function))
  ;; Cast?
  (arg-tn nil :type (or null function))
  (result-tn nil :type (or null function))
  (subtypep nil :type (or null function)))

(defun %print-alien-type-class (type-class stream depth)
  (declare (ignore depth))
  (print-unreadable-object (type-class stream :type t)
    (prin1 (alien-type-class-name type-class) stream)))

(defvar *alien-type-classes* (make-hash-table :test #'eq))

(defun alien-type-class-or-lose (name)
  (or (gethash name *alien-type-classes*)
      (error "No alien type class ~S" name)))

(defun create-alien-type-class-if-necessary (name include)
  (let ((old (gethash name *alien-type-classes*))
	(include (and include (alien-type-class-or-lose include))))
    (if old
	(setf (alien-type-class-include old) include)
	(setf (gethash name *alien-type-classes*)
	      (make-alien-type-class :name name :include include)))))

(defconstant method-slot-alist
  '((:unparse . alien-type-class-unparse)
    (:type= . alien-type-class-type=)
    (:subtypep . alien-type-class-subtypep)
    (:lisp-rep . alien-type-class-lisp-rep)
    (:alien-rep . alien-type-class-alien-rep)
    (:extract-gen . alien-type-class-extract-gen)
    (:deposit-gen . alien-type-class-deposit-gen)
    (:naturalize-gen . alien-type-class-naturalize-gen)
    (:deport-gen . alien-type-class-deport-gen)
    ;; Cast?
    (:arg-tn . alien-type-class-arg-tn)
    (:result-tn . alien-type-class-result-tn)))

(defun method-slot (method)
  (cdr (or (assoc method method-slot-alist)
	   (error "No method ~S" method))))

); eval-when


;;; We define a keyword "BOA" constructor so that we can reference the slots
;;; names in init forms.
;;;
(defmacro def-alien-type-class ((name &key include include-args) &rest slots)
  (let ((defstruct-name
	 (intern (concatenate 'string "ALIEN-" (symbol-name name) "-TYPE"))))
    (multiple-value-bind
	(include include-defstruct overrides)
	(etypecase include
	  (null
	   (values nil 'alien-type nil))
	  (symbol
	   (values
	    include
	    (intern (concatenate 'string
				 "ALIEN-" (symbol-name include) "-TYPE"))
	    nil))
	  (list
	   (values
	    (car include)
	    (intern (concatenate 'string
				 "ALIEN-" (symbol-name (car include)) "-TYPE"))
	    (cdr include))))
      `(progn
	 (eval-when (compile load eval)
	   (create-alien-type-class-if-necessary ',name ',(or include 'root)))
	 (defstruct (,defstruct-name
			(:include ,include-defstruct
				  (:class ',name)
				  ,@overrides)
			(:constructor
			 ,(intern (concatenate 'string "MAKE-"
					       (string defstruct-name)))
			 (&key class bits alignment
			       ,@(mapcar #'(lambda (x)
					     (if (atom x) x (car x)))
					 slots)
			       ,@include-args)))
	   ,@slots)))))

(defmacro def-alien-type-method ((class method) lambda-list &rest body)
  (let ((defun-name (intern (concatenate 'string
					 (symbol-name class)
					 "-"
					 (symbol-name method)
					 "-METHOD"))))
    `(progn
       (defun ,defun-name ,lambda-list
	 ,@body)
       (setf (,(method-slot method) (alien-type-class-or-lose ',class))
	     #',defun-name))))

(defmacro invoke-alien-type-method (method type &rest args)
  (let ((slot (method-slot method)))
    (once-only ((type type))
      `(funcall (do ((class (alien-type-class-or-lose (alien-type-class ,type))
			    (alien-type-class-include class)))
		    ((null class)
		     (error "Method ~S not defined for ~S"
			    ',method (alien-type-class ,type)))
		  (let ((fn (,slot class)))
		    (when fn
		      (return fn))))
		,type ,@args))))



;;;; Alien-type defstruct.

(eval-when (compile load eval)
  (create-alien-type-class-if-necessary 'root nil))

(defstruct (alien-type
	    (:print-function %print-alien-type)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:constructor make-alien-type (&key class bits alignment)))
  (class 'root :type symbol)
  (bits nil :type (or null unsigned-byte))
  (alignment (guess-alignment bits) :type (or null unsigned-byte)))

(defun %print-alien-type (type stream depth)
  (declare (ignore depth))
  (print-unreadable-object (type stream :type t)
    (prin1 (unparse-alien-type type) stream)))


;;;; Type parsing and unparsing.

(defvar *auxiliary-type-definitions* nil)
(defvar *new-auxiliary-types*)

;;; WITH-AUXILIARY-ALIEN-TYPES -- internal.
;;;
;;; Process stuff in a new scope.
;;;
(defmacro with-auxiliary-alien-types (&body body)
  `(let ((*auxiliary-type-definitions*
	  (if (boundp '*new-auxiliary-types*)
	      (append *new-auxiliary-types* *auxiliary-type-definitions*)
	      *auxiliary-type-definitions*))
	 (*new-auxiliary-types* nil))
     ,@body))

;;; PARSE-ALIEN-TYPE -- public
;;;
(defun parse-alien-type (type)
  "Parse the list structure TYPE as an alien type specifier and return
   the resultant alien-type structure."
  (if (boundp '*new-auxiliary-types*)
      (%parse-alien-type type)
      (let ((*new-auxiliary-types* nil))
	(%parse-alien-type type))))

(defun %parse-alien-type (type)
  (if (consp type)
      (let ((translator (info alien-type translator (car type))))
	(unless translator
	  (error "Unknown alien type: ~S" type))
	(funcall translator type))
      (case (info alien-type kind type)
	(:primitive
	 (let ((translator (info alien-type translator type)))
	   (unless translator
	     (error "No translator for primitive alien type ~S?" type))
	   (funcall translator (list type))))
	(:defined
	 (or (info alien-type definition type)
	     (error "Definition missing for alien type ~S?" type)))
	(:unknown
	 (error "Unknown alien type: ~S" type)))))

(defun auxiliary-alien-type (kind name)
  (flet ((aux-defn-matches (x)
	   (and (eq (first x) kind) (eq (second x) name))))
    (let ((in-auxiliaries
	   (or (find-if #'aux-defn-matches *new-auxiliary-types*)
	       (find-if #'aux-defn-matches *auxiliary-type-definitions*))))
      (if in-auxiliaries
	  (values (third in-auxiliaries) t)
	  (ecase kind
	    (:struct
	     (info alien-type struct name))
	    (:union
	     (info alien-type union name))
	    (:enum
	     (info alien-type enum name)))))))

(defun %set-auxiliary-alien-type (kind name defn)
  (flet ((aux-defn-matches (x)
	   (and (eq (first x) kind) (eq (second x) name))))
    (when (find-if #'aux-defn-matches *new-auxiliary-types*)
      (error "Attempt to multiple define ~A ~S." kind name))
    (when (find-if #'aux-defn-matches *auxiliary-type-definitions*)
      (error "Attempt to shadow definition of ~A ~S." kind name)))
  (push (list kind name defn) *new-auxiliary-types*)
  defn)

(defsetf auxiliary-alien-type %set-auxiliary-alien-type)

(defun verify-local-auxiliaries-okay ()
  (dolist (info *new-auxiliary-types*)
    (destructuring-bind (kind name defn) info
      (declare (ignore defn))
      (when (ecase kind
	      (:struct
	       (info alien-type struct name))
	      (:union
	       (info alien-type union name))
	      (:enum
	       (info alien-type enum name)))
	(error "Attempt to shadow definition of ~A ~S." kind name)))))

;;; *record-type-already-unparsed* -- internal
;;;
;;; Holds the list of record types that have already been unparsed.  This is
;;; used to keep from outputing the slots again if the same structure shows
;;; up twice.
;;; 
(defvar *record-types-already-unparsed*)

;;; UNPARSE-ALIEN-TYPE -- public.
;;; 
(defun unparse-alien-type (type)
  "Convert the alien-type structure TYPE back into a list specification of
   the type."
  (declare (type alien-type type))
  (let ((*record-types-already-unparsed* nil))
    (%unparse-alien-type type)))

;;; %UNPARSE-ALIEN-TYPE -- internal.
;;;
;;; Does all the work of UNPARSE-ALIEN-TYPE.  It's seperate because we need
;;; to recurse inside the binding of *record-types-already-unparsed*.
;;; 
(defun %unparse-alien-type (type)
  (invoke-alien-type-method :unparse type))




;;;; Alien type defining stuff.

(defmacro def-alien-type-translator (name lambda-list &body body)
  (let ((whole (gensym))
	(defun-name (intern (concatenate 'string
					 "ALIEN-"
					 (symbol-name name)
					 "-TYPE-TRANSLATOR"))))
    (multiple-value-bind
	(body decls docs)
	(lisp::parse-defmacro lambda-list whole body name
			      'def-alien-type-translator)
      `(progn
	 (defun ,defun-name (,whole)
	   ,decls
	   (block ,name
	     ,body))
	 (%def-alien-type-translator ',name #',defun-name ,docs)))))

(defun %def-alien-type-translator (name translator docs)
  (declare (ignore docs))
  (setf (info alien-type kind name) :primitive)
  (setf (info alien-type translator name) translator)
  (clear-info alien-type definition name)
  #+nil
  (setf (documentation name 'alien-type) docs)
  name)


(defmacro def-alien-type (name type)
  "Define the alien type NAME to be equivalent to TYPE.  Name may be NIL for
   STRUCT and UNION types, in which case the name is taken from the type
   specifier."
  (with-auxiliary-alien-types
    (let ((alien-type (parse-alien-type type)))
      `(eval-when (compile load eval)
	 ,@(when *new-auxiliary-types*
	     `((%def-auxiliary-alien-types ',*new-auxiliary-types*)))
	 ,@(when name
	     `((%def-alien-type ',name ',alien-type)))))))

(defun %def-auxiliary-alien-types (types)
  (dolist (info types)
    (destructuring-bind (kind name defn) info
      (macrolet ((frob (kind)
		   `(let ((old (info alien-type ,kind name)))
		      (unless (or (null old) (alien-type-= old defn))
			(warn "Redefining ~A ~S to be:~%  ~S,~%was:~%  ~S"
			      kind name defn old))
		      (setf (info alien-type ,kind name) defn))))
	(ecase kind
	  (:struct (frob struct))
	  (:union (frob union))
	  (:enum (frob enum)))))))

(defun %def-alien-type (name new)
  (ecase (info alien-type kind name)
    (:primitive
     (error "~S is a built-in alien type." name))
    (:defined
     (let ((old (info alien-type definition name)))
       (unless (or (null old) (alien-type-= new old))
	 (warn "Redefining ~S to be:~%  ~S,~%was~%  ~S" name
	       (unparse-alien-type new) (unparse-alien-type old)))))
    (:unknown))
  (setf (info alien-type definition name) new)
  (setf (info alien-type kind name) :defined)
  name)



;;;; Interfaces to the different methods

(defun alien-type-= (type1 type2)
  "Return T iff TYPE1 and TYPE2 describe equivalent alien types."
  (or (eq type1 type2)
      (and (eq (alien-type-class type1)
	       (alien-type-class type2))
	   (invoke-alien-type-method :type= type1 type2))))

(defun alien-subtype-p (type1 type2)
  "Return T iff the alien type TYPE1 is a subtype of TYPE2.  Currently, the
   only supported subtype relationships are is that any pointer type is a
   subtype of (* t), and any array type first dimension will match 
   (array <eltype> nil ...).  Otherwise, the two types have to be
   ALIEN-TYPE-=."
  (or (eq type1 type2)
      (invoke-alien-type-method :subtypep type1 type2)))

(defun alien-typep (object type)
  "Return T iff OBJECT is an alien of type TYPE."
  (let ((lisp-rep-type (compute-lisp-rep-type type)))
    (if lisp-rep-type
	(typep object lisp-rep-type)
	(and (alien-value-p object)
	     (alien-subtype-p (alien-value-type object) type)))))


(defun compute-naturalize-lambda (type)
  `(lambda (alien ignore)
     (declare (ignore ignore))
     ,(invoke-alien-type-method :naturalize-gen type 'alien)))

(defun compute-deport-lambda (type)
  (declare (type alien-type type))
  (multiple-value-bind
      (form value-type)
      (invoke-alien-type-method :deport-gen type 'value)
    `(lambda (value ignore)
       (declare (type ,(or value-type
			   (compute-lisp-rep-type type)
			   `(alien ,type))
		      value)
		(ignore ignore))
       ,form)))

(defun compute-extract-lambda (type)
  `(lambda (sap offset ignore)
     (declare (type system-area-pointer sap)
	      (type unsigned-byte offset)
	      (ignore ignore))
     (naturalize ,(invoke-alien-type-method :extract-gen type 'sap 'offset)
		 ',type)))

(defun compute-deposit-lambda (type)
  (declare (type alien-type type))
  `(lambda (sap offset ignore value)
     (declare (type system-area-pointer sap)
	      (type unsigned-byte offset)
	      (ignore ignore))
     (let ((value (deport value ',type)))
       ,(invoke-alien-type-method :deposit-gen type 'sap 'offset 'value)
       ;; Note: the reason we don't just return the pre-deported value
       ;; is because that would inhibit any (deport (naturalize ...))
       ;; optimizations that might have otherwise happen.  Re-naturalizing
       ;; the value might cause extra consing, but is flushable, so probably
       ;; results in better code.
       (naturalize value ',type))))

(defun compute-lisp-rep-type (type)
  (invoke-alien-type-method :lisp-rep type))

(defun compute-alien-rep-type (type)
  (invoke-alien-type-method :alien-rep type))





;;;; Default methods.

(def-alien-type-method (root :unparse) (type)
  `(!!unknown-alien-type!! ,(type-of type)))

(def-alien-type-method (root :type=) (type1 type2)
  (declare (ignore type1 type2))
  t)

(def-alien-type-method (root :subtypep) (type1 type2)
  (alien-type-= type1 type2))

(def-alien-type-method (root :lisp-rep) (type)
  (declare (ignore type))
  nil)

(def-alien-type-method (root :alien-rep) (type)
  (declare (ignore type))
  '*)

(def-alien-type-method (root :naturalize-gen) (type alien)
  (declare (ignore alien))
  (error "Cannot represent ~S typed aliens." type))

(def-alien-type-method (root :deport-gen) (type object)
  (declare (ignore object))
  (error "Cannot represent ~S typed aliens." type))

(def-alien-type-method (root :extract-gen) (type sap offset)
  (declare (ignore sap offset))
  (error "Cannot represent ~S typed aliens." type))

(def-alien-type-method (root :deposit-gen) (type sap offset value)
  `(setf ,(invoke-alien-type-method :extract-gen type sap offset) ,value))

(def-alien-type-method (root :arg-tn) (type state)
  (declare (ignore state))
  (error "Cannot pass aliens of type ~S as arguments to call-out"
	 (unparse-alien-type type)))

(def-alien-type-method (root :result-tn) (type state)
  (declare (ignore state))
  (error "Cannot return aliens of type ~S from call-out"
	 (unparse-alien-type type)))


;;;; The INTEGER type.

(def-alien-type-class (integer)
  (signed t :type (member t nil)))

(def-alien-type-translator signed (&optional (bits vm:word-bits))
  (make-alien-integer-type :bits bits))

(def-alien-type-translator integer (&optional (bits vm:word-bits))
  (make-alien-integer-type :bits bits))

(def-alien-type-translator unsigned (&optional (bits vm:word-bits))
  (make-alien-integer-type :bits bits :signed nil))

(def-alien-type-method (integer :unparse) (type)
  (list (if (alien-integer-type-signed type) 'signed 'unsigned)
	(alien-integer-type-bits type)))

(def-alien-type-method (integer :type=) (type1 type2)
  (and (eq (alien-integer-type-signed type1)
	   (alien-integer-type-signed type2))
       (= (alien-integer-type-bits type1)
	  (alien-integer-type-bits type2))))

(def-alien-type-method (integer :lisp-rep) (type)
  (list (if (alien-integer-type-signed type) 'signed-byte 'unsigned-byte)
	(alien-integer-type-bits type)))

(def-alien-type-method (integer :alien-rep) (type)
  (list (if (alien-integer-type-signed type) 'signed-byte 'unsigned-byte)
	(alien-integer-type-bits type)))

#-amd64
(def-alien-type-method (integer :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

;; signed numbers <= 32 bits need to be sign extended.
;; I really should use the movsxd instruction, but I don't
;; know how.
#+amd64
(defun sign-extend-32-bit (num)
  (if (> num #x7fffffff)
      (- num #x100000000)
      num))

#+amd64
(def-alien-type-method (integer :naturalize-gen) (type alien)
  (if (and (alien-integer-type-signed type)
	   (< (alien-integer-type-bits type) 64))
      `(sign-extend-32-bit ,alien)
      alien))

(def-alien-type-method (integer :deport-gen) (type value)
  (declare (ignore type))
  value)

(def-alien-type-method (integer :extract-gen) (type sap offset)
  (declare (type alien-integer-type type))
  (let ((ref-fun
	 (if (alien-integer-type-signed type)
	  (case (alien-integer-type-bits type)
	    (8 'signed-sap-ref-8)
	    (16 'signed-sap-ref-16)
	    (32 'signed-sap-ref-32)
	    (64 'signed-sap-ref-64))
	  (case (alien-integer-type-bits type)
	    (8 'sap-ref-8)
	    (16 'sap-ref-16)
	    (32 'sap-ref-32)
	    (64 'sap-ref-64)))))
    (if ref-fun
	`(,ref-fun ,sap (/ ,offset vm:byte-bits))
	(error "Cannot extract ~D bit integers."
	       (alien-integer-type-bits type)))))



;;;; The BOOLEAN type.

(def-alien-type-class (boolean :include integer :include-args (signed)))

(def-alien-type-translator boolean (&optional (bits vm:word-bits))
  (make-alien-boolean-type :bits bits :signed nil))

(def-alien-type-method (boolean :unparse) (type)
  `(boolean ,(alien-boolean-type-bits type)))

(def-alien-type-method (boolean :lisp-rep) (type)
  (declare (ignore type))
  `(member t nil))

(def-alien-type-method (boolean :naturalize-gen) (type alien)
  (declare (ignore type))
  `(not (zerop ,alien)))

(def-alien-type-method (boolean :deport-gen) (type value)
  (declare (ignore type))
  `(if ,value 1 0))


;;;; The ENUM type.

(def-alien-type-class (enum :include (integer (:bits 32))
			    :include-args (signed))
  name		; name of this enum (if any)
  from		; alist from keywords to integers.
  to		; alist or vector from integers to keywords.
  kind		; Kind of from mapping, :vector or :alist.
  offset)	; Offset to add to value for :vector from mapping.

(def-alien-type-translator enum (&whole type name &rest mappings)
  (cond (mappings
	 (let ((result (parse-enum name mappings)))
	   (when name
	     (multiple-value-bind (old old-p)
		 (auxiliary-alien-type :enum name)
	       (when old-p
		 (unless (alien-type-= result old)
		   (warn "Redefining alien enum ~S" name)))
	       ;; I (rtoy) am not 100% sure about this.  But compare
	       ;; what this does with what PARSE-ALIEN-RECORD-TYPE
	       ;; does.  So, if we've seen this type before and it's
	       ;; the same type, we don't need to setf
	       ;; auxiliary-alien-type.  This gets around the problem
	       ;; noted by Nicolas Neuss on cmucl-help, 2004/11/09
	       ;; where he had something like
	       ;;
	       ;; (def-alien-type yes-no-t (enum yes-no-t :no :yes))
	       ;; (def-alien-type nil
	       ;;    (struct foo
	       ;;      (arg1 yes-no-t)
	       ;;      (arg2 yes-no-t)))
	       (when (or (not old-p)
			 (not (alien-type-= result old)))
		 (setf (auxiliary-alien-type :enum name) result))))
	   result))
	(name
	 (multiple-value-bind
	     (result found)
	     (auxiliary-alien-type :enum name)
	   (unless found
	     (error "Unknown enum type: ~S" name))
	   result))
	(t
	 (error "Empty enum type: ~S" type))))

(defun parse-enum (name elements)
  (when (null elements)
    (error "An anumeration must contain at least one element."))
  (let ((min nil)
	(max nil)
	(from-alist ())
	(prev -1))
    (declare (list from-alist))
    (dolist (el elements)
      (multiple-value-bind
	  (sym val)
	  (if (listp el)
	      (values (first el) (second el))
	      (values el (1+ prev)))
	(setf prev val)
	(unless (keywordp sym)
	  (error "Enumeration element ~S is not a keyword." sym))
	(unless (integerp val)
	  (error "Element value ~S is not an integer." val))
	(unless (and max (> max val)) (setq max val))
	(unless (and min (< min val)) (setq min val))
	(when (rassoc val from-alist)
	  (error "Element value ~S used more than once." val))
	(when (assoc sym from-alist :test #'eq)
	  (error "Enumeration element ~S used more than once." sym))
	(push (cons sym val) from-alist)))
    (let* ((signed (minusp min))
	   (min-bits (if signed
			 (1+ (max (integer-length min)
				  (integer-length max)))
			 (integer-length max))))
      (when (> min-bits 32)
	(error "Can't represent enums needing more than 32 bits."))
      (setf from-alist (sort from-alist #'< :key #'cdr))
      (cond
       ;;
       ;; If range is at least 20% dense, use vector mapping.  Crossover
       ;; point solely on basis of space would be 25%.  Vector mapping
       ;; is always faster, so give the benefit of the doubt.
       ((< 0.2 (/ (float (length from-alist)) (float (- max min))))
	;;
	;; If offset is small and ignorable, ignore it to save time.
	(when (< 0 min 10) (setq min 0))
	(let ((to (make-array (1+ (- max min)))))
	  (dolist (el from-alist)
	    (setf (svref to (- (cdr el) min)) (car el)))
	  (make-alien-enum-type :name name :signed signed
				:from from-alist :to to :kind
				:vector :offset (- min))))
       (t
	(make-alien-enum-type :name name :signed signed
			      :from from-alist
			      :to (mapcar #'(lambda (x) (cons (cdr x) (car x)))
					  from-alist)
			      :kind :alist))))))

(def-alien-type-method (enum :unparse) (type)
  `(enum ,(alien-enum-type-name type)
	 ,@(let ((prev -1))
	     (mapcar #'(lambda (mapping)
			 (let ((sym (car mapping))
			       (value (cdr mapping)))
			   (prog1
			       (if (= (1+ prev) value)
				   sym
				   `(,sym ,value))
			     (setf prev value))))
		     (alien-enum-type-from type)))))

(def-alien-type-method (enum :type=) (type1 type2)
  (and (eq (alien-enum-type-name type1)
	   (alien-enum-type-name type2))
       (equal (alien-enum-type-from type1)
	      (alien-enum-type-from type2))))

(def-alien-type-method (enum :lisp-rep) (type)
  `(member ,@(mapcar #'car (alien-enum-type-from type))))

(def-alien-type-method (enum :naturalize-gen) (type alien)
  (ecase (alien-enum-type-kind type)
    (:vector
     `(svref ',(alien-enum-type-to type)
	     (+ ,alien ,(alien-enum-type-offset type))))
    (:alist
     `(ecase ,alien
	,@(mapcar #'(lambda (mapping)
		      `(,(car mapping) ,(cdr mapping)))
		  (alien-enum-type-to type))))))

(def-alien-type-method (enum :deport-gen) (type value)
  `(ecase ,value
     ,@(mapcar #'(lambda (mapping)
		   `(,(car mapping) ,(cdr mapping)))
	       (alien-enum-type-from type))))



;;;; the FLOAT types.

(def-alien-type-class (float)
  (type (required-argument) :type symbol))

(def-alien-type-method (float :unparse) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :lisp-rep) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :alien-rep) (type)
  (alien-float-type-type type))

(def-alien-type-method (float :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

(def-alien-type-method (float :deport-gen) (type value)
  (declare (ignore type))
  value)


(def-alien-type-class (single-float :include (float (:bits 32))
				    :include-args (type)))

(def-alien-type-translator single-float ()
  (make-alien-single-float-type :type 'single-float))

(def-alien-type-method (single-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-single ,sap (/ ,offset vm:byte-bits)))


(def-alien-type-class (double-float :include (float (:bits 64))
				    :include-args (type)))

(def-alien-type-translator double-float ()
  (make-alien-double-float-type :type 'double-float))

(def-alien-type-method (double-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-double ,sap (/ ,offset vm:byte-bits)))


#+long-float
(def-alien-type-class (long-float :include (float (:bits #+x86 96 #+sparc 128))
				  :include-args (type)))

#+long-float
(def-alien-type-translator long-float ()
  (make-alien-long-float-type :type 'long-float))

#+long-float
(def-alien-type-method (long-float :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-long ,sap (/ ,offset vm:byte-bits)))


;;;; The SAP type

(def-alien-type-class (system-area-pointer))

(def-alien-type-translator system-area-pointer ()
  (make-alien-system-area-pointer-type :bits #-alpha vm:word-bits #+alpha 64))

(def-alien-type-method (system-area-pointer :unparse) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :lisp-rep) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :alien-rep) (type)
  (declare (ignore type))
  'system-area-pointer)

(def-alien-type-method (system-area-pointer :naturalize-gen) (type alien)
  (declare (ignore type))
  alien)

(def-alien-type-method (system-area-pointer :deport-gen) (type object)
  (declare (ignore type))
  object)

(def-alien-type-method (system-area-pointer :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap-ref-sap ,sap (/ ,offset vm:byte-bits)))


;;;; the ALIEN-VALUE type.

(def-alien-type-class (alien-value :include system-area-pointer))

(def-alien-type-method (alien-value :lisp-rep) (type)
  (declare (ignore type))
  nil)

(def-alien-type-method (alien-value :naturalize-gen) (type alien)
  `(%sap-alien ,alien ',type))

(def-alien-type-method (alien-value :deport-gen) (type value)
  (declare (ignore type))
  `(alien-sap ,value))



;;;; The POINTER type.

(def-alien-type-class (pointer :include (alien-value (:bits
						      #-alpha vm:word-bits
						      #+alpha 64)))
  (to nil :type (or alien-type null)))

(def-alien-type-translator * (to)
  (make-alien-pointer-type :to (if (eq to t) nil (parse-alien-type to))))

(def-alien-type-method (pointer :unparse) (type)
  (let ((to (alien-pointer-type-to type)))
    `(* ,(if to
	     (%unparse-alien-type to)
	     t))))

(def-alien-type-method (pointer :type=) (type1 type2)
  (let ((to1 (alien-pointer-type-to type1))
	(to2 (alien-pointer-type-to type2)))
    (if to1
	(if to2
	    (alien-type-= to1 to2)
	    nil)
	(null to2))))

(def-alien-type-method (pointer :subtypep) (type1 type2)
  (and (alien-pointer-type-p type2)
       (let ((to1 (alien-pointer-type-to type1))
	     (to2 (alien-pointer-type-to type2)))
	 (if to1
	     (if to2
		 (alien-subtype-p to1 to2)
		 t)
	     (null to2)))))

(def-alien-type-method (pointer :deport-gen) (type value)
  (values
   `(etypecase ,value
      (null
       (int-sap 0))
      (system-area-pointer
       ,value)
      ((alien ,type)
       (alien-sap ,value)))
   `(or null system-area-pointer (alien ,type))))


;;;; The MEM-BLOCK type.

(def-alien-type-class (mem-block :include alien-value))

(def-alien-type-method (mem-block :extract-gen) (type sap offset)
  (declare (ignore type))
  `(sap+ ,sap (/ ,offset vm:byte-bits)))

(def-alien-type-method (mem-block :deposit-gen) (type sap offset value)
  (let ((bits (alien-mem-block-type-bits type)))
    (unless bits
      (error "Cannot deposit aliens of type ~S (unknown size)." type))
    `(kernel:system-area-copy ,value 0 ,sap ,offset ',bits)))


;;;; The ARRAY type.

(def-alien-type-class (array :include mem-block)
  (element-type (required-argument) :type alien-type)
  (dimensions (required-argument) :type list))

(def-alien-type-translator array (ele-type &rest dims)
  (when dims
    (unless (typep (first dims) '(or kernel:index null))
      (error "First dimension is not a non-negative fixnum or NIL: ~S"
	     (first dims)))
    (let ((loser (find-if-not #'(lambda (x) (typep x 'kernel:index))
			      (rest dims))))
      (when loser
	(error "Dimension is not a non-negative fixnum: ~S" loser))))
	
  (let ((type (parse-alien-type ele-type)))
    (make-alien-array-type
     :element-type type
     :dimensions dims
     :alignment (alien-type-alignment type)
     :bits (if (and (alien-type-bits type)
		    (every #'integerp dims))
	       (* (align-offset (alien-type-bits type)
				(alien-type-alignment type))
		  (reduce #'* dims))))))

(def-alien-type-method (array :unparse) (type)
  `(array ,(%unparse-alien-type (alien-array-type-element-type type))
	  ,@(alien-array-type-dimensions type)))

(def-alien-type-method (array :type=) (type1 type2)
  (and (equal (alien-array-type-dimensions type1)
	      (alien-array-type-dimensions type2))
       (alien-type-= (alien-array-type-element-type type1)
		     (alien-array-type-element-type type2))))

(def-alien-type-method (array :subtypep) (type1 type2)
  (and (alien-array-type-p type2)
       (let ((dim1 (alien-array-type-dimensions type1))
	     (dim2 (alien-array-type-dimensions type2)))
	 (and (= (length dim1) (length dim2))
	      (or (and dim2
		       (null (car dim2))
		       (equal (cdr dim1) (cdr dim2)))
		  (equal dim1 dim2))
	      (alien-subtype-p (alien-array-type-element-type type1)
			       (alien-array-type-element-type type2))))))


;;;; The RECORD type.

(defstruct (alien-record-field
	    (:print-function %print-alien-field)
	    (:make-load-form-fun :just-dump-it-normally))
  (name (required-argument) :type symbol)
  (type (required-argument) :type alien-type)
  (bits nil :type (or unsigned-byte null))
  (offset 0 :type unsigned-byte))

(defun %print-alien-field (field stream depth)
  (declare (ignore depth))
  (print-unreadable-object (field stream :type t)
    (funcall (formatter "~S ~S~@[:~D~]")
	     stream
	     (alien-record-field-type field)
	     (alien-record-field-name field)
	     (alien-record-field-bits field))))

(def-alien-type-class (record :include mem-block)
  (kind :struct :type (member :struct :union))
  (name nil :type (or symbol null))
  (fields nil :type list))

(def-alien-type-translator struct (name &rest fields)
  (parse-alien-record-type :struct name fields))

(def-alien-type-translator union (name &rest fields)
  (parse-alien-record-type :union name fields))

(defun parse-alien-record-type (kind name fields)
  (if fields
      (let* ((old (and name (auxiliary-alien-type kind name)))
	     (result (if (or (null old)
			     (alien-record-type-fields old))
			 (make-alien-record-type :name name :kind kind)
			 old)))
	(when (and name (not (eq old result)))
	  (setf (auxiliary-alien-type kind name) result))
	(parse-alien-record-fields result fields)
	result)
      (if name
	  (or (auxiliary-alien-type kind name)
	      (setf (auxiliary-alien-type kind name)
		    (make-alien-record-type :name name :kind kind)))
	  (make-alien-record-type :kind kind))))

;;; PARSE-ALIEN-RECORD-FIELDS -- internal
;;;
;;; Used by parse-alien-type to parse the fields of struct and union
;;; types.  RESULT holds the record type we are paring the fields of,
;;; and FIELDS is the list of field specifications.
;;; 
(defun parse-alien-record-fields (result fields)
  (declare (type alien-record-type result)
	   (type list fields))
  (let ((total-bits 0)
	(overall-alignment 1)
	(parsed-fields nil)
	(firstp t))
    (dolist (field fields)
      (destructuring-bind (var type &optional bits) field
	(declare (ignore bits))
	(let* ((field-type (parse-alien-type type))
	       (bits (alien-type-bits field-type))
	       (natural-alignment (alien-type-alignment field-type))
	       (alignment (embedded-alignment natural-alignment
					      firstp))
	       (parsed-field
		(make-alien-record-field :type field-type
					 :name var)))
	  (push parsed-field parsed-fields)
	  (when (null bits)
	    (error "Unknown size: ~S"
		   (unparse-alien-type field-type)))
	  (when (null alignment)
	    (error "Unknown alignment: ~S"
		   (unparse-alien-type field-type)))
	  (setf overall-alignment (max overall-alignment alignment))
	  (ecase (alien-record-type-kind result)
	    (:struct
	     (let ((offset (align-offset total-bits alignment)))
	       (setf (alien-record-field-offset parsed-field) offset)
	       (setf total-bits (+ offset bits))))
	    (:union
	     (setf total-bits (max total-bits bits))))))
      (setf firstp nil))
    (let ((new (nreverse parsed-fields)))
      (setf (alien-record-type-fields result) new))
    (setf (alien-record-type-alignment result) overall-alignment)
    (setf (alien-record-type-bits result)
	  (align-offset total-bits overall-alignment))))

(def-alien-type-method (record :unparse) (type)
  `(,(case (alien-record-type-kind type)
       (:struct 'struct)
       (:union 'union)
       (t '???))
    ,(alien-record-type-name type)
    ,@(unless (member type *record-types-already-unparsed* :test #'eq)
	(push type *record-types-already-unparsed*)
	(mapcar #'(lambda (field)
		    `(,(alien-record-field-name field)
		      ,(%unparse-alien-type (alien-record-field-type field))
		      ,@(if (alien-record-field-bits field)
			    (list (alien-record-field-bits field)))))
		(alien-record-type-fields type)))))


;;; Test the record fields. Keep a hashtable table of already compared
;;; types to detect cycles.

(defun record-fields-match-p (field1 field2)
  (and (eq (alien-record-field-name field1)
	   (alien-record-field-name field2))
       (eql (alien-record-field-bits field1)
	    (alien-record-field-bits field2))
       (eql (alien-record-field-offset field1)
	    (alien-record-field-offset field2))
       (alien-type-= (alien-record-field-type field1)
		     (alien-record-field-type field2))))

(defvar *match-history* nil
  "A hashtable used to detect cycles while comparing record types.")

(defun in-match-history-or (type1 type2 alternative)
  "Test if TYPE1 and TYPE2 are in the *MATCH-HISTORY*.
If so return true; otherwise call ALTERNATIVE."
  (cond (*match-history*
	 (let ((list (gethash type1 *match-history*)))
	   (cond ((memq type2 list)
		  t)
		 (t 
		  (setf (gethash type1 *match-history*) (cons type2 list))
		  (funcall alternative)))))
	(t
	 (let ((*match-history* (make-hash-table :test #'eq)))
	   (setf (gethash type1 *match-history*) (list type2))
	   (funcall alternative)))))

(def-alien-type-method (record :type=) (type1 type2)
  (and (eq (alien-record-type-name type1) 
	   (alien-record-type-name type2))
       (eq (alien-record-type-kind type1) 
	   (alien-record-type-kind type2))
       (eql (alien-type-bits type1) 
	    (alien-type-bits type2))
       (eql (alien-type-alignment type1) 
	    (alien-type-alignment type2))
       (= (length (alien-record-type-fields type1))
	  (length (alien-record-type-fields type2)))
       (in-match-history-or 
	type1 type2
	(lambda ()
	  (every #'record-fields-match-p 
		 (alien-record-type-fields type1)
		 (alien-record-type-fields type2))))))


;;;; The FUNCTION and VALUES types.

(defvar *values-type-okay* nil)

(def-alien-type-class (function :include mem-block)
  (result-type (required-argument) :type alien-type)
  (arg-types (required-argument) :type list)
  (stub nil :type (or null function)))

(def-alien-type-translator function (result-type &rest arg-types)
  (make-alien-function-type
   :result-type (let ((*values-type-okay* t))
		  (parse-alien-type result-type))
   :arg-types (mapcar #'parse-alien-type arg-types)))

(def-alien-type-method (function :unparse) (type)
  `(function ,(%unparse-alien-type (alien-function-type-result-type type))
	     ,@(mapcar #'%unparse-alien-type
		       (alien-function-type-arg-types type))))

(def-alien-type-method (function :type=) (type1 type2)
  (and (alien-type-= (alien-function-type-result-type type1)
		     (alien-function-type-result-type type2))
       (= (length (alien-function-type-arg-types type1))
	  (length (alien-function-type-arg-types type2)))
       (every #'alien-type-=
	      (alien-function-type-arg-types type1)
	      (alien-function-type-arg-types type2))))


(def-alien-type-class (values)
  (values (required-argument) :type list))

(def-alien-type-translator values (&rest values)
  (unless *values-type-okay*
    (error "Cannot use values types here."))
  (let ((*values-type-okay* nil))
    (make-alien-values-type
     :values (mapcar #'parse-alien-type values))))

(def-alien-type-method (values :unparse) (type)
  `(values ,@(mapcar #'%unparse-alien-type
		     (alien-values-type-values type))))

(def-alien-type-method (values :type=) (type1 type2)
  (and (= (length (alien-values-type-values type1))
	  (length (alien-values-type-values type2)))
       (every #'alien-type-=
	      (alien-values-type-values type1)
	      (alien-values-type-values type2))))



;;;; Alien variables.

;;; HEAP-ALIEN-INFO -- defstruct.
;;;
;;; Information describing a heap-allocated alien.
;;; 
(defstruct (heap-alien-info
	    (:print-function %print-heap-alien-info)
	    (:make-load-form-fun :just-dump-it-normally))
  ;; The type of this alien.
  (type (required-argument) :type alien-type)
  ;; The form to evaluate to produce the SAP pointing to where in the heap
  ;; it is.
  (sap-form (required-argument)))
;;;
(defun %print-heap-alien-info (info stream depth)
  (declare (ignore depth))
  (print-unreadable-object (info stream :type t)
    (funcall (formatter "~S ~S")
	     stream
	     (heap-alien-info-sap-form info)
	     (unparse-alien-type (heap-alien-info-type info)))))

;;; LOCAL-ALIEN-INFO -- public defstruct.
;;;
;;; Information about local aliens.  The WITH-ALIEN macro builds one of these
;;; structures and local-alien and friends comunicate information about how
;;; that local alien is represented.
;;; 
(defstruct (local-alien-info
	    (:print-function %print-local-alien-info)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:constructor make-local-alien-info (&key type force-to-memory-p)))
  ;; The type of the local alien.
  (type (required-argument) :type alien-type)
  ;; T if this local alien must be forced into memory.  Using the ADDR macro
  ;; on a local alien will set this.
  (force-to-memory-p (or (alien-array-type-p type) (alien-record-type-p type))
		     :type (member t nil)))
;;;
(defun %print-local-alien-info (info stream depth)
  (declare (ignore depth))
  (print-unreadable-object (info stream :type t)
    (funcall (formatter "~:[~;(forced to stack) ~]~S")
	     stream
	     (local-alien-info-force-to-memory-p info)
	     (unparse-alien-type (local-alien-info-type info)))))

;;; GUESS-ALIEN-NAME-FROM-LISP-NAME -- internal.
;;;
;;; Make a string out of the symbol, converting all uppercase letters to
;;; lower case and hyphens into underscores.
;;; 
(defun guess-alien-name-from-lisp-name (lisp-name)
  (declare (type symbol lisp-name))
  (nsubstitute #\_ #\- (string-downcase (symbol-name lisp-name))))

;;; GUESS-LISP-NAME-FROM-ALIEN-NAME -- internal.
;;;
;;; The opposite of GUESS-ALIEN-NAME-FROM-LISP-NAME.  Make a symbol out of the
;;; string, converting all lowercase letters to uppercase and underscores into
;;; hyphens.
;;;
(defun guess-lisp-name-from-alien-name (alien-name)
  (declare (type simple-string alien-name))
  (intern (nsubstitute #\- #\_ (string-upcase alien-name))))

;;; PICK-LISP-AND-ALIEN-NAMES -- internal.
;;;
;;; Extract the lisp and alien names from NAME.  If only one is given, guess
;;; the other.
;;; 
(defun pick-lisp-and-alien-names (name)
  (etypecase name
    (string
     (values (guess-lisp-name-from-alien-name name) name))
    (symbol
     (values name (guess-alien-name-from-lisp-name name)))
    (list
     (unless (= (length name) 2)
       (error "Badly formed alien name."))
     (values (cadr name) (car name)))))

;;; DEF-ALIEN-VARIABLE -- public
;;;
(defmacro def-alien-variable (name type)
  "Define NAME as an external alien variable of type TYPE.  NAME should be
   a list of a string holding the alien name and a symbol to use as the Lisp
   name.  If NAME is just a symbol or string, then the other name is guessed
   from the one supplied."
  (multiple-value-bind
      (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (with-auxiliary-alien-types
      (let ((alien-type (parse-alien-type type)))
	`(eval-when (compile load eval)
	   ,@(when *new-auxiliary-types*
	       `((%def-auxiliary-alien-types ',*new-auxiliary-types*)))
	   (%def-alien-variable ',lisp-name
				',alien-name
				',alien-type))))))

;;; %DEF-ALIEN-VARIABLE -- internal
;;;
;;; Do the actual work of DEF-ALIEN-VARIABLE.
;;; 
(defun %def-alien-variable (lisp-name alien-name type)
  (setf (info variable kind lisp-name) :alien)
  (setf (info variable where-from lisp-name) :defined)
  (clear-info variable constant-value lisp-name)
  (setf (info variable alien-info lisp-name)
	(make-heap-alien-info :type type
			      :sap-form `(foreign-symbol-address
					  ,alien-name :flavor :data))))

;;; EXTERN-ALIEN -- public.
;;; 
(defmacro extern-alien (name type)
  "Access the alien variable named NAME, assuming it is of type TYPE.  This
   is setfable."
  (let* ((alien-name (etypecase name
		       (symbol (guess-alien-name-from-lisp-name name))
		       (string name)))
	 (alien-type (parse-alien-type type))
	 (flavor (if (alien-function-type-p alien-type)
		     :code
		     :data)))
    `(%heap-alien ',(make-heap-alien-info
		     :type alien-type
		     :sap-form `(foreign-symbol-address ',alien-name
				 :flavor ',flavor)))))

;;; WITH-ALIEN -- public.
;;;
(defmacro with-alien (bindings &body body)
  "Establish some local alien variables.  Each BINDING is of the form:
     VAR TYPE [ ALLOCATION ] [ INITIAL-VALUE | EXTERNAL-NAME ]
   ALLOCATION should be one of:
     :LOCAL (the default)
       The alien is allocated on the stack, and has dynamic extent.
     :STATIC
       The alien is allocated on the heap, and has infinate extent.  The alien
       is allocated at load time, so the same piece of memory is used each time
       this form executes.
     :EXTERN
       No alien is allocated, but VAR is established as a local name for
       the external alien given by EXTERNAL-NAME."
  (with-auxiliary-alien-types
    (dolist (binding (reverse bindings))
      (destructuring-bind
	  (symbol type &optional (opt1 nil opt1p) (opt2 nil opt2p))
	  binding
	(let* ((alien-type (parse-alien-type type))
	       (flavor (if (alien-function-type-p alien-type)
			   :code
			   :data)))
	  (multiple-value-bind
	      (allocation initial-value)
	      (if opt2p
		  (values opt1 opt2)
		  (case opt1
		    (:extern
		     (values opt1 (guess-alien-name-from-lisp-name symbol)))
		    (:static
		     (values opt1 nil))
		    (t
		     (values :local opt1))))
	    (setf body
		  (ecase allocation
		    #+nil
		    (:static
		     (let ((sap
			    (make-symbol (concatenate 'string "SAP-FOR-"
						      (symbol-name symbol)))))
		       `((let ((,sap (load-time-value (%make-alien ...))))
			   (declare (type system-area-pointer ,sap))
			   (symbol-macrolet
			    ((,symbol (sap-alien ,sap ,type)))
			    ,@(when initial-value
				`((setq ,symbol ,initial-value)))
			    ,@body)))))
		    (:extern
		     (let ((info (make-heap-alien-info
				  :type alien-type
				  :sap-form `(foreign-symbol-address
					      ',initial-value
					      :flavor ',flavor))))
		       `((symbol-macrolet
			  ((,symbol (%heap-alien ',info)))
			  ,@body))))
		    (:local
		     (let ((var (gensym))
			   (initval (if initial-value (gensym)))
			   (info (make-local-alien-info
				  :type alien-type)))
		       `((let ((,var (make-local-alien ',info))
			       ,@(when initial-value
				   `((,initval ,initial-value))))
			   (note-local-alien-type ',info ,var)
			   (multiple-value-prog1
			       (symbol-macrolet
				((,symbol (local-alien ',info ,var)))
				,@(when initial-value
				    `((setq ,symbol ,initval)))
				,@body)
			       (dispose-local-alien ',info ,var)
			       )))))))))))
    (verify-local-auxiliaries-okay)
    `(compiler-let ((*auxiliary-type-definitions*
		     ',(append *new-auxiliary-types*
			       *auxiliary-type-definitions*)))
       ,@body)))



;;;; Runtime C values that don't correspond directly to Lisp types.

;;; ALIEN-VALUE
;;;
;;; The defstruct for alien-value lives in struct.lisp 'cause it has to be
;;; real early in the cold-load order.
;;;
(declaim (freeze-type alien-value))
;;;
(defun %print-alien-value (value stream depth)
  (declare (ignore depth))
  (print-unreadable-object (value stream)
    (funcall (formatter "Alien ~S at #x~8,'0X")
	     stream 
	     (unparse-alien-type (alien-value-type value))
	     (sap-int (alien-value-sap value)))))

(declaim (inline null-alien))
(defun null-alien (x)
  "Return true if X (which must be an Alien pointer) is null, false otherwise."
  (zerop (sap-int (alien-sap x))))

  
(defmacro sap-alien (sap type)
  "Convert the System-Area-Pointer SAP to an Alien of the specified Type (not
   evaluated.)  Type must be pointer-like."
  (let ((alien-type (parse-alien-type type)))
    (if (eq (compute-alien-rep-type alien-type) 'system-area-pointer)
	`(%sap-alien ,sap ',alien-type)
	(error "Cannot make aliens of type ~S out of SAPs" type))))

(defun %sap-alien (sap type)
  (declare (type system-area-pointer sap)
	   (type alien-type type))
  (make-alien-value :sap sap :type type))

(defun alien-sap (alien)
  "Return a System-Area-Pointer pointing to Alien's data."
  (declare (type alien-value alien))
  (alien-value-sap alien))



;;;; Allocation/Deallocation of heap aliens.

;;; MAKE-ALIEN -- public.
;;; 
(defmacro make-alien (type &optional size)
  "Allocate an alien of type TYPE and return an alien pointer to it.  If SIZE
   is supplied, how it is interpreted depends on TYPE.  If TYPE is an array
   type, SIZE is used as the first dimension for the allocated array.  If TYPE
   is not an array, then SIZE is the number of elements to allocate.  The
   memory is allocated using ``malloc'', so it can be passed to foreign
   functions which use ``free''."
  (let ((alien-type (if (alien-type-p type) type (parse-alien-type type))))
    (multiple-value-bind
	(size-expr element-type)
	(if (alien-array-type-p alien-type)
	    (let ((dims (alien-array-type-dimensions alien-type)))
	      (cond
	       (size
		(unless dims
		  (error
		   "Cannot override the size of zero-dimensional arrays."))
		(when (constantp size)
		  (setf alien-type (copy-alien-array-type alien-type))
		  (setf (alien-array-type-dimensions alien-type)
			(cons (eval size) (cdr dims)))))
	       (dims
		(setf size (car dims)))
	       (t
		(setf size 1)))
	      (values `(* ,size ,@(cdr dims))
		      (alien-array-type-element-type alien-type)))
	    (values (or size 1) alien-type))
      (let ((bits (alien-type-bits element-type))
	    (alignment (alien-type-alignment element-type)))
	(unless bits
	  (error "Size of ~S unknown." (unparse-alien-type element-type)))
	(unless alignment
	  (error "Alignment of ~S unknown." (unparse-alien-type element-type)))
	`(%sap-alien (%make-alien (* ,(align-offset bits alignment)
				     ,size-expr))
		     ',(make-alien-pointer-type :to alien-type))))))

;;; %MAKE-ALIEN -- internal
;;;
;;; Allocate a block of memory at least BITS bits long and return a system
;;; area pointer to it.
;;;
(declaim (inline %make-alien))
(defun %make-alien (bits)
  (declare (type kernel:index bits) (optimize-interface (safety 2)))
  (alien-funcall (extern-alien "malloc" (function system-area-pointer unsigned))
		 (ash (the kernel:index (+ bits 7)) -3)))

;;; FREE-ALIEN -- public
;;;
(declaim (inline free-alien))
(defun free-alien (alien)
  "Dispose of the storage pointed to by ALIEN.  ALIEN must have been allocated
   by MAKE-ALIEN or ``malloc''."
  (alien-funcall (extern-alien "free" (function (values) system-area-pointer))
		 (alien-sap alien))
  nil)


;;;; The SLOT operator

;;; SLOT-OR-LOSE -- internal.
;;;
;;; Find the field named SLOT, or die trying.
;;; 
(defun slot-or-lose (type slot)
  (declare (type alien-record-type type)
	   (type symbol slot))
  (or (find slot (alien-record-type-fields type)
	    :key #'alien-record-field-name)
      (error "No slot named ~S in ~S" slot type)))

;;; SLOT -- public
;;;
;;; Extract the value from the named slot from the record alien.  If the
;;; alien is actually a pointer, then deref it first.
;;; 
(defun slot (alien slot)
  "Extract SLOT from the Alien STRUCT or UNION ALIEN.  May be set with SETF."
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (slot (deref alien) slot))
      (alien-record-type
       (let ((field (slot-or-lose type slot)))
	 (extract-alien-value (alien-value-sap alien)
			      (alien-record-field-offset field)
			      (alien-record-field-type field)))))))

;;; %SET-SLOT -- public setf method
;;;
;;; Deposite the value in the specified slot of the record alien.  If the
;;; alien is really a pointer, deref it first.  The compiler uses this
;;; when it can't figure out anything better.
;;; 
(defun %set-slot (alien slot value)
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%set-slot (deref alien) slot value))
      (alien-record-type
       (let ((field (slot-or-lose type slot)))
	 (deposit-alien-value (alien-value-sap alien)
			      (alien-record-field-offset field)
			      (alien-record-field-type field)
			      value))))))
;;;
(defsetf slot %set-slot)

;;; %SLOT-ADDR -- internal
;;; 
;;; Compute the address of the specified slot and return a pointer to it.
;;; 
(defun %slot-addr (alien slot)
  (declare (type alien-value alien)
	   (type symbol slot)
	   (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%slot-addr (deref alien) slot))
      (alien-record-type
       (let* ((field (slot-or-lose type slot))
	      (offset (alien-record-field-offset field))
	      (field-type (alien-record-field-type field)))
	 (%sap-alien (sap+ (alien-sap alien) (/ offset vm:byte-bits))
		     (make-alien-pointer-type :to field-type)))))))


;;;; The DEREF operator.

;;; DEREF-GUTS -- internal.
;;;
;;; Does most of the work of the different DEREF methods.  Returns two values:
;;; the type and the offset (in bits) of the refered to alien.
;;; 
(defun deref-guts (alien indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (values alien-type integer))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (when (cdr indices)
	 (error "Too many indices when derefing ~S: ~D"
		type
		(length indices)))
       (let ((element-type (alien-pointer-type-to type)))
	 (values element-type
		 (if indices
		     (* (align-offset (alien-type-bits element-type)
				      (alien-type-alignment element-type))
			(car indices))
		     0))))
      (alien-array-type
       (unless (= (length indices) (length (alien-array-type-dimensions type)))
	 (error "Incorrect number of indices when derefing ~S: ~D"
		type (length indices)))
       (labels ((frob (dims indices offset)
		  (if (null dims)
		      offset
		      (frob (cdr dims) (cdr indices)
			(+ (if (zerop offset)
			       0
			       (* offset (car dims)))
			   (car indices))))))
	 (let ((element-type (alien-array-type-element-type type)))
	   (values element-type
		   (* (align-offset (alien-type-bits element-type)
				    (alien-type-alignment element-type))
		      (frob (alien-array-type-dimensions type)
			indices 0)))))))))

;;; DEREF -- public
;;;
;;; Dereference the alien and return the results.
;;; 
(defun deref (alien &rest indices)
  "De-reference an Alien pointer or array.  If an array, the indices are used
   as the indices of the array element to access.  If a pointer, one index can
   optionally be specified, giving the equivalent of C pointer arithmetic."
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind
      (target-type offset)
      (deref-guts alien indices)
    (extract-alien-value (alien-value-sap alien)
			 offset
			 target-type)))

;;; %SET-DEREF -- public setf method
;;; 
(defun %set-deref (alien value &rest indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind
      (target-type offset)
      (deref-guts alien indices)
    (deposit-alien-value (alien-value-sap alien)
			 offset
			 target-type
			 value)))
;;;
(defsetf deref (alien &rest indices) (value)
  `(%set-deref ,alien ,value ,@indices))

;;; %DEREF-ADDR -- public
;;;
(defun %deref-addr (alien &rest indices)
  (declare (type alien-value alien)
	   (type list indices)
	   (optimize (inhibit-warnings 3)))
  (multiple-value-bind
      (target-type offset)
      (deref-guts alien indices)
    (%sap-alien (sap+ (alien-value-sap alien) (/ offset vm:byte-bits))
		(make-alien-pointer-type :to target-type))))


;;;; Accessing heap alien variables.

(defun %heap-alien (info)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (extract-alien-value (eval (heap-alien-info-sap-form info))
		       0
		       (heap-alien-info-type info)))

(defun %set-heap-alien (info value)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (deposit-alien-value (eval (heap-alien-info-sap-form info))
		       0
		       (heap-alien-info-type info)
		       value))
;;;
(defsetf %heap-alien %set-heap-alien)

(defun %heap-alien-addr (info)
  (declare (type heap-alien-info info)
	   (optimize (inhibit-warnings 3)))
  (%sap-alien (eval (heap-alien-info-sap-form info))
	      (make-alien-pointer-type :to (heap-alien-info-type info))))



;;;; Accessing local aliens.

(defun make-local-alien (info)
  (let* ((alien (eval `(make-alien ,(local-alien-info-type info))))
	 (alien-sap (alien-sap alien)))
    (finalize
     alien
     #'(lambda ()
	 (alien-funcall
	  (extern-alien "free" (function (values) system-area-pointer))
	  alien-sap)))
    alien))

(defun note-local-alien-type (info alien)
  (declare (ignore info alien))
  nil)

(defun local-alien (info alien)
  (declare (ignore info))
  (deref alien))

(defun %set-local-alien (info alien value)
  (declare (ignore info))
  (setf (deref alien) value))

(define-setf-expander local-alien (&whole whole info alien)
  (let ((value (gensym))
	(info (if (and (consp info)
		       (eq (car info) 'quote))
		  (second info)
		  (error "Something is wrong; local-alien-info not found: ~S"
			 whole))))
    (values nil
	    nil
	    (list value)
	    (if c:*converting-for-interpreter*
		`(%set-local-alien ',info ,alien ,value)
		`(if (%local-alien-forced-to-memory-p ',info)
		     (%set-local-alien ',info ,alien ,value)
		     (setf ,alien
			   (deport ,value ',(local-alien-info-type info)))))
	    whole)))

(defun %local-alien-forced-to-memory-p (info)
  (local-alien-info-force-to-memory-p info))

(defun %local-alien-addr (info alien)
  (declare (type local-alien-info info))
  (unless (local-alien-info-force-to-memory-p info)
    (error "~S isn't forced to memory.  Something went wrong." alien))
  alien)

(defun dispose-local-alien (info alien)
  (declare (ignore info))
  (cancel-finalization alien)
  (free-alien alien))


;;;; The ADDR macro.

(defmacro addr (expr &environment env)
  "Return an Alien pointer to the data addressed by Expr, which must be a call
   to SLOT or DEREF, or a reference to an Alien variable."
  (let ((form (macroexpand expr env)))
    (or (typecase form
	  (cons
	   (case (car form)
	     (slot
	      (cons '%slot-addr (cdr form)))
	     (deref
	      (cons '%deref-addr (cdr form)))
	     (%heap-alien
	      (cons '%heap-alien-addr (cdr form)))
	     (local-alien
	      (let ((info
		     (let ((info-arg (second form)))
		       (and (consp info-arg)
			    (eq (car info-arg) 'quote)
			    (second info-arg)))))
		(unless (local-alien-info-p info)
		  (error "Something is wrong, local-alien-info not found: ~S"
			 form))
		(setf (local-alien-info-force-to-memory-p info) t))
	      (cons '%local-alien-addr (cdr form)))))
	  (symbol
	   (let ((kind (info variable kind form)))
	     (when (eq kind :alien)
	       `(%heap-alien-addr ',(info variable alien-info form))))))
	(error "~S is not a valid L-value" form))))


;;;; The CAST macro.

(defmacro cast (alien type)
  "Convert ALIEN to an Alien of the specified TYPE (not evaluated.)  Both types
   must be Alien array, pointer or function types."
  `(%cast ,alien ',(parse-alien-type type)))

(defun %cast (alien target-type)
  (declare (type alien-value alien)
	   (type alien-type target-type)
	   (optimize-interface (safety 2))
	   (optimize (inhibit-warnings 3)))
  (if (or (alien-pointer-type-p target-type)
	  (alien-array-type-p target-type)
	  (alien-function-type-p target-type))
      (let ((alien-type (alien-value-type alien)))
	(if (or (alien-pointer-type-p alien-type)
		(alien-array-type-p alien-type)
		(alien-function-type-p alien-type))
	    (naturalize (alien-value-sap alien) target-type)
	    (error "~S cannot be casted." alien)))
      (error "Cannot cast to alien type ~S" (unparse-alien-type target-type))))



;;;; The ALIEN-SIZE macro.

(defmacro alien-size (type &optional (units :bits))
  "Return the size of the alien type TYPE.  UNITS specifies the units to
   use and can be either :BITS, :BYTES, or :WORDS."
  (let* ((alien-type (parse-alien-type type))
	 (bits (alien-type-bits alien-type)))
    (if bits
	(values (ceiling bits
			 (ecase units
			   (:bits 1)
			   (:bytes vm:byte-bits)
			   (:words vm:word-bits))))
	(error "Unknown size for alien type ~S."
	       (unparse-alien-type alien-type)))))



;;;; Naturalize, deport, extract-alien-value, deposit-alien-value

(defun naturalize (alien type)
  (declare (type alien-type type))
  (funcall (coerce (compute-naturalize-lambda type) 'function)
	   alien type))

(defun deport (value type)
  (declare (type alien-type type))
  (funcall (coerce (compute-deport-lambda type) 'function)
	   value type))

(defun extract-alien-value (sap offset type)
  (declare (type system-area-pointer sap)
	   (type unsigned-byte offset)
	   (type alien-type type))
  (funcall (coerce (compute-extract-lambda type) 'function)
	   sap offset type))

(defun deposit-alien-value (sap offset type value)
  (declare (type system-area-pointer sap)
	   (type unsigned-byte offset)
	   (type alien-type type))
  (funcall (coerce (compute-deposit-lambda type) 'function)
	   sap offset type value))



;;;; alien-funcall, def-alien-function

(defun alien-funcall (alien &rest args)
  "Call the foreign function ALIEN with the specified arguments.  ALIEN's
   type specifies the argument and result types."
  (declare (type alien-value alien))
  (let ((type (alien-value-type alien)))
    (typecase type
      (alien-pointer-type
       (apply #'alien-funcall (deref alien) args))
      (alien-function-type
       (unless (= (length (alien-function-type-arg-types type))
		  (length args))
	 (error "Wrong number of arguments for ~S~%Expected ~D, got ~D."
		type
		(length (alien-function-type-arg-types type))
		(length args)))
       (let ((stub (alien-function-type-stub type)))
	 (unless stub
	   (setf stub
		 (let ((fun (gensym))
		       (parms (loop repeat (length args) collect (gensym))))
		   (compile nil
			    `(lambda (,fun ,@parms)
			       (declare (type (alien ,type) ,fun))
			       (alien-funcall ,fun ,@parms)))))
	   (setf (alien-function-type-stub type) stub))
	 (apply stub alien args)))
      (t
       (error "~S is not an alien function." alien)))))

(defmacro def-alien-routine (name result-type &rest args)
  "Def-Alien-Routine Name Result-Type
                    {(Arg-Name Arg-Type [Style])}*

  Define a foreign interface function for the routine with the specified Name,
  which may be either a string, symbol or list of the form (string symbol).
  Return-Type is the Alien type for the function return value.  VOID may be
  used to specify a function with no result.

  The remaining forms specifiy individual arguments that are passed to the
  routine.  Arg-Name is a symbol that names the argument, primarily for
  documentation.  Arg-Type is the C-Type of the argument.  Style specifies the
  say that the argument is passed.

  :IN
        An :In argument is simply passed by value.  The value to be passed is
        obtained from argument(s) to the interface function.  No values are
        returned for :In arguments.  This is the default mode.

  :OUT
        The specified argument type must be a pointer to a fixed sized object.
        A pointer to a preallocated object is passed to the routine, and the
        the object is accessed on return, with the value being returned from
        the interface function.  :OUT and :IN-OUT cannot be used with pointers
        to arrays, records or functions.

  :COPY
        Similar to :IN, except that the argument values are stored in on
        the stack, and a pointer to the object is passed instead of
        the values themselves.

  :IN-OUT
        A combination of :OUT and :COPY.  A pointer to the argument is passed,
        with the object being initialized from the supplied argument and
        the return value being determined by accessing the object on return."
  (multiple-value-bind
      (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (collect ((docs) (lisp-args) (arg-types) (alien-vars)
	      (alien-args) (results))
      (dolist (arg args)
	(if (stringp arg)
	    (docs arg)
	    (destructuring-bind (name type &optional (style :in)) arg
	      (unless (member style '(:in :copy :out :in-out))
		(error "Bogus argument style ~S in ~S." style arg))
	      (unless (eq style :out)
		(lisp-args name))
	      (when (and (member style '(:out :in-out))
			 (typep (parse-alien-type type) 'alien-pointer-type))
		(error "Can't use :out or :in-out on pointer-like type:~%  ~S"
		       type))
	      (cond ((eq style :in)
		     (arg-types type)
		     (alien-args name))
		    (t
		     (arg-types `(* ,type))
		     (if (eq style :out)
			 (alien-vars `(,name ,type))
			 (alien-vars `(,name ,type ,name)))
		     (alien-args `(addr ,name))))
	      (when (or (eq style :out) (eq style :in-out))
		(results name)))))
      `(defun ,lisp-name ,(lisp-args)
	 ,@(docs)
	 (with-alien
	     ((,lisp-name (function ,result-type ,@(arg-types))
			  :extern ,alien-name)
	      ,@(alien-vars))
	     ,(if (alien-values-type-p result-type)
		  (let ((temps (loop
				 repeat (length (alien-values-type-values
						 result-type))
				 collect (gensym))))
		    `(multiple-value-bind
			 ,temps
			 (alien-funcall ,lisp-name ,@(alien-args))
		       (values ,@temps ,@(results))))
		  `(values (alien-funcall ,lisp-name ,@(alien-args))
			   ,@(results))))))))

;;;; Alien callback support
;;;;
;;;; This is basically the implementation posted by Helmut Eller,
;;;; posted to cmucl-imp on 04/13/2003.  It has been modified to live
;;;; in the ALIEN package and to fit the same style as the ALIEN
;;;; package.

;;; This package provides a mechanism for defining callbacks: lisp
;;; functions which can be called from foreign code.  The user
;;; interface consists of the macros DEFCALLBACK and CALLBACK.  (See
;;; the doc-strings for details.)
;;;
;;; Below are two examples.  The first example defines a callback FOO
;;; and calls it with alien-funcall.  The second illustrates the use
;;; of the libc qsort function.
;;;
;;; The implementation generates a piece machine code -- a
;;; "trampoline" -- for each callback function.  A pointer to this
;;; trampoline can then be passed to foreign code.  The trampoline is
;;; allocated with malloc and is not moved by the GC.
;;;
;;; When called, the trampoline passes a pointer to the arguments
;;; (essentially the stack pointer) together with an index to
;;; CALL-CALLACK.  CALL-CALLBACK uses the index to find the
;;; corresponding lisp function and calls this function with the
;;; argument pointer.  The lisp function uses the pointer to copy the
;;; arguments form the stack to local variables.  On return, the lisp
;;; function stores the result into the location given by the argument
;;; pointer, and the trampoline code copies the return value from
;;; there into the right return register.
;;;
;;; The address of CALL-CALLBACK is used in every trampoline and must
;;; not be moved by the gc.  It is therefore necessary to either
;;; include this package into the image (core) or to purify before
;;; creating any trampolines (or to invent some other trick).
;;;
;;; Examples: 

#||
;;; Example 1:

(defcallback foo (int (arg1 int) (arg2 int))
  (format t "~&foo: ~S, ~S~%" arg1 arg2)
  (+ arg1 arg2))

(alien-funcall (sap-alien (callback foo) (function int int int))
	       555 444444)

;;; Example 2:

(def-alien-routine qsort void
  (base (* t))
  (nmemb int)
  (size int)
  (compar (* (function int (* t) (* t)))))

(defcallback my< (int (arg1 (* double))
		      (arg2 (* double)))
  (let ((a1 (deref arg1))
	(a2 (deref arg2)))
    (cond ((= a1 a2)  0)
	  ((< a1 a2) -1)
	  (t         +1))))

(let ((a (make-array 10 :element-type 'double-float
		     :initial-contents '(0.1d0 0.5d0 0.2d0 1.2d0 1.5d0
					 2.5d0 0.0d0 0.1d0 0.2d0 0.3d0))))
  (print a)
  (qsort (sys:vector-sap a)
	 (length a)
	 (alien-size double :bytes)
	 (callback my<))
  (print a))

||#

(defstruct (callback 
	     (:constructor make-callback (trampoline lisp-fn function-type)))
  "A callback consists of a piece assembly code -- the trampoline --
and a lisp function.  We store the function type (including return
type and arg types, so we can detect incompatible redefinitions."
  (trampoline (required-argument) :type system-area-pointer)
  (lisp-fn (required-argument) :type (function (fixnum fixnum) (values)))
  (function-type (required-argument) :type alien::alien-function-type))

(declaim (type (vector callback) *callbacks*))
(defvar *callbacks* (make-array 10 :element-type 'callback
				:fill-pointer 0 :adjustable t)
  "Vector of all callbacks.")

(defun call-callback (index sp-fixnum ret-addr)
  (declare (type fixnum index sp-fixnum ret-addr)
	   (optimize speed))
  (funcall (callback-lisp-fn (aref *callbacks* index))
	   sp-fixnum ret-addr))

(defun create-callback (lisp-fn fn-type)
  (let* ((index (fill-pointer *callbacks*))
	 (tramp (vm:make-callback-trampoline index fn-type))
	 (cb (make-callback tramp lisp-fn fn-type)))
    (vector-push-extend cb *callbacks*)
    cb))

(defun address-of-call-callback ()
  (kernel:get-lisp-obj-address #'call-callback))

(defun address-of-funcall3 ()
  (sys:sap-int (alien-sap (extern-alien "funcall3" (function (* t))))))

;;; Some abbreviations for alien-type classes.  The $ suffix is there
;;; to prevent name clashes.

(deftype void$ () '(satisfies alien-void-type-p))
(deftype integer$ () 'alien-integer-type)
(deftype integer-64$ () '(satisfies alien-integer-64-type-p))
(deftype signed-integer$ () '(satisfies alien-signed-integer-type-p))
(deftype pointer$ () 'alien-pointer-type)
(deftype single$ () 'alien-single-float-type)
(deftype double$ () 'alien-double-float-type)
(deftype sap$ () '(satisfies alien-sap-type=))

(defun alien-sap-type= (type)
  (alien-type-= type (parse-alien-type 'system-area-pointer)))

(defun alien-void-type-p (type)
  (and (alien-values-type-p type)
       (null (alien-values-type-values type))))

(defun alien-integer-64-type-p (type)
  (and (alien-integer-type-p type)
       (= (alien-type-bits type) 64)))

(defun alien-signed-integer-type-p (type)
  (and (alien-integer-type-p type)
       (alien-integer-type-signed type)))

(defun segment-to-trampoline (segment length)
  (let* ((code (alien-funcall 
		(extern-alien "malloc" (function system-area-pointer unsigned))
		length))
	 (fill-pointer code))
    (new-assem:segment-map-output segment
      (lambda (sap length)
	(kernel:system-area-copy sap 0 fill-pointer 0
				 (* length vm:byte-bits))
	(setf fill-pointer (sys:sap+ fill-pointer length))))
    code))

(defun symbol-trampoline (symbol)
  (callback-trampoline (symbol-value symbol)))

(defmacro callback (name)
  "Return the trampoline pointer for the callback NAME."
  `(symbol-trampoline ',name))

;; Convenience macro to make it easy to call callbacks.
(defmacro callback-funcall (name &rest args)
  `(alien-funcall (sap-alien (callback ,name)
			     ,(unparse-alien-type
			       (callback-function-type (symbol-value name))))
		  ,@args))

(defun define-callback-function (name lisp-fn fn-type)
  (declare (type symbol name)
	   (type function lisp-fn))
  (flet ((register-new-callback () 
	   (setf (symbol-value name)
		 (create-callback lisp-fn fn-type))))
    (if (and (boundp name)
	     (callback-p (symbol-value name)))
	;; try do redefine the existing callback
	(let ((callback (find (symbol-trampoline name) *callbacks*
			      :key #'callback-trampoline :test #'sys:sap=)))
	  (cond (callback
		 (let ((old-type (callback-function-type callback)))
		   (cond ((vm::compatible-function-types-p old-type fn-type)
			  ;; (format t "~&; Redefining callback ~A~%" name)
			  (setf (callback-lisp-fn callback) lisp-fn)
			  (setf (callback-function-type callback) fn-type)
			  callback)
			 (t
			  (let ((e (format nil "~
Attempt to redefine callback with incompatible return type.
   Old type was: ~A 
    New type is: ~A" old-type fn-type))
				(c (format nil "~
Create new trampoline (old trampoline calls old lisp function).")))
			    (cerror c e)
			    (register-new-callback))))))
		(t (register-new-callback))))
	(register-new-callback))))

(defun word-aligned-bits (type)
  (align-offset (alien-type-bits type) vm:word-bits))

(defun argument-size (spec)
  (let ((type (parse-alien-type spec)))
    (typecase type
      ((or integer$ single$ double$ pointer$ sap$)
       (ceiling (word-aligned-bits type) vm:byte-bits))
      (t (error "Unsupported argument type: ~A" spec)))))

(defun parse-return-type (spec)
  (let ((*values-type-okay* t))
    (parse-alien-type spec)))

(defun parse-function-type (return-type arg-specs)
  (parse-alien-type
   `(function ,return-type ,@(mapcar #'second arg-specs))))

(defun return-exp (spec sap body)
  (flet ((store (spec) `(setf (deref (sap-alien ,sap (* ,spec))) ,body)))
    (let ((type (parse-return-type spec)))
      (typecase type
	(void$ body)
	(signed-integer$ 
	 (store `(signed ,(word-aligned-bits type))))
	(integer$
	 (store `(unsigned ,(word-aligned-bits type))))
	((or single$ double$ pointer$ sap$)
	 (store spec))
	(t (error "Unsupported return type: ~A" spec))))))

(defmacro def-callback (name (return-type &rest arg-specs) &parse-body (body decls doc))
  "(defcallback NAME (RETURN-TYPE {(ARG-NAME ARG-TYPE)}*)
     {doc-string} {decls}* {FORM}*)

Define a function which can be called by foreign code.  The pointer
returned by (callback NAME), when called by foreign code, invokes the
lisp function.  The lisp function expects alien arguments of the
specified ARG-TYPEs and returns an alien of type RETURN-TYPE.

If (callback NAME) is already a callback function pointer, its value
is not changed (though it's arranged that an updated version of the
lisp callback function will be called).  This feature allows for
incremental redefinition of callback functions."
  (let ((sp-fixnum (gensym (string :sp-fixnum-)))
	(ret-addr (gensym (string :ret-addr-)))
	(sp (gensym (string :sp-)))
	(ret (gensym (string :ret-))))
    `(progn
      (defun ,name (,sp-fixnum ,ret-addr)
	,@(when doc (list doc))
	(declare (type fixnum ,sp-fixnum ,ret-addr))
	,@decls
	;; We assume sp-fixnum is word aligned and pass it untagged to
	;; this function.  The shift compensates this.
 	(let ((,sp (sys:int-sap (bignum:%ashl (ldb (byte vm:word-bits 0) ,sp-fixnum)
 					      2)))
 	      (,ret (sys:int-sap (bignum:%ashl (ldb (byte vm:word-bits 0) ,ret-addr)
 					       2))))
	  (declare (ignorable ,sp ,ret))
	  ;; Copy all arguments to local variables.
	  (with-alien ,(loop for offset = 0 then (+ offset 
						    (argument-size type))
			     for (name type) in arg-specs
			     collect `(,name ,type
				       :local ,(vm:callback-accessor-form type sp offset)))
	    ,(return-exp return-type ret `(progn ,@body))
	    (values))))
      (define-callback-function 
	  ',name #',name ',(parse-function-type return-type arg-specs)))))

;;; dumping support

(defun restore-callbacks ()
  ;; Create new trampolines on reload.
  (loop for cb across *callbacks*
	for i from 0
	do (setf (callback-trampoline cb)
		 (vm:make-callback-trampoline i (callback-function-type cb)))))

;; *after-save-initializations* contains
;; new-assem::forget-output-blocks, and the assembler may not work
;; before forget-output-blocks was called.  We add 'restore-callback at
;; the end of *after-save-initializations* to sidestep this problem.
(setf *after-save-initializations*
      (append *after-save-initializations* (list 'restore-callbacks)))

;;; callback.lisp ends here
