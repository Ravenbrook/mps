;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/backend.lisp,v 1.32 1998/03/04 14:53:22 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file isolates all the backend specific data so that we can compile
;;; and use different backends.
;;; 
;;; Written by William Lott.
;;;
(in-package "C")

(export '(*backend* *target-backend* *native-backend* backend
	  backend-name backend-version backend-fasl-file-type
	  backend-fasl-file-implementation backend-fasl-file-version
	  backend-register-save-penalty backend-byte-order
	  backend-any-primitive-type backend-info-environment
	  backend-instruction-formats backend-instruction-flavors
	  backend-assembler-resources backend-special-arg-types
	  backend-disassem-params backend-internal-errors
	  backend-assembler-params backend-page-size
	  
	  ;; The various backends need to call these support routines
	  def-vm-support-routine make-stack-pointer-tn primitive-type
	  primitive-type-of emit-nop location-number))


;;;; VM support routine stuff.

(eval-when (compile eval)

(defmacro def-vm-support-routines (&rest routines)
  `(progn
     (eval-when (compile load eval)
       (defparameter vm-support-routines ',routines))
     (defstruct (vm-support-routines
		 (:print-function %print-vm-support-routines))
       ,@(mapcar #'(lambda (routine)
		     `(,routine nil :type (or function null)))
		 routines))
     ,@(mapcar
	#'(lambda (name)
	    `(defun ,name (&rest args)
	       (apply (or (,(symbolicate "VM-SUPPORT-ROUTINES-" name)
			   (backend-support-routines *backend*))
			  (error "Machine specific support routine ~S ~
				  undefined for ~S"
				 ',name *backend*))
		      args)))
	routines)))

); eval-when

(def-vm-support-routines
  ;; From VM.LISP
  immediate-constant-sc
  location-print-name
  
  ;; From PRIMTYPE.LISP
  primitive-type-of
  primitive-type
  
  ;; From C-CALL.LISP
  make-call-out-tns
  
  ;; From CALL.LISP
  standard-argument-location
  make-return-pc-passing-location
  make-old-fp-passing-location
  make-old-fp-save-location
  make-return-pc-save-location
  make-argument-count-location
  make-nfp-tn
  make-stack-pointer-tn
  make-number-stack-pointer-tn
  make-unknown-values-locations
  select-component-format
  
  ;; From NLX.LISP
  make-nlx-sp-tn
  make-dynamic-state-tns
  make-nlx-entry-argument-start-location
  
  ;; From SUPPORT.LISP
  generate-call-sequence
  generate-return-sequence

  ;; For use with scheduler.
  emit-nop
  location-number)

(defprinter vm-support-routines)

(defmacro def-vm-support-routine (name ll &body body)
  (unless (member (intern (string name) (find-package "C"))
		  vm-support-routines)
    (warn "Unknown VM support routine: ~A" name))
  (let ((local-name (symbolicate (backend-name *target-backend*) "-" name)))
    `(progn
       (defun ,local-name ,ll ,@body)
       (setf (,(intern (concatenate 'simple-string
				    "VM-SUPPORT-ROUTINES-"
				    (string name))
		       (find-package "C"))
	      (backend-support-routines *target-backend*))
	     #',local-name))))



;;;; The backend structure.

(defstruct (backend
	    (:print-function %print-backend))
  ;; The name of this backend.  Something like ``PMAX''
  (name nil)

  ;; The version string for this backend.
  ;; Something like ``DECstation 3100/Mach 0.0''
  (version nil)

  ;; Information about fasl files for this backend.
  (fasl-file-type nil)
  (fasl-file-implementation nil)
  (fasl-file-version nil)

  ;; The VM support routines.
  (support-routines (make-vm-support-routines) :type vm-support-routines)

  ;; The number of references that a TN must have to offset the overhead of
  ;; saving the TN across a call.
  (register-save-penalty 0)

  ;; The byte order of the target machine.  Should either be :big-endian
  ;; which has the MSB first (RT) or :little-endian which has the MSB last
  ;; (VAX).
  (byte-order nil :type (or null (member :little-endian :big-endian)))

  ;; Translates from SC numbers to SC info structures.  SC numbers are always
  ;; used instead of names at run time, so changing this vector changes all the
  ;; references.
  (sc-numbers (make-array sc-number-limit :initial-element nil)
	      :type sc-vector)

  ;; A list of all the SBs defined, so that we can easily iterate over them.
  (sb-list () :type list)

  ;; Translates from template names to template structures.
  (template-names (make-hash-table :test #'eq) :type hash-table)

  ;; Hashtable from SC and SB names the corresponding structures.  The META
  ;; versions are only used at meta-compile and load times, so the defining
  ;; macros can change these at meta-compile time without breaking the
  ;; compiler.
  (sc-names (make-hash-table :test #'eq) :type hash-table)
  (sb-names (make-hash-table :test #'eq) :type hash-table)
  (meta-sc-names (make-hash-table :test #'eq) :type hash-table)
  (meta-sb-names (make-hash-table :test #'eq) :type hash-table)

  ;; Like *SC-Numbers*, but is updated at meta-compile time.
  (meta-sc-numbers (make-array sc-number-limit :initial-element nil)
		   :type sc-vector)

  ;; Translates from primitive type names to the corresponding primitive-type
  ;; structure.
  (primitive-type-names (make-hash-table :test #'eq) :type hash-table)

  ;; Establishes a convenient handle on primitive type unions, or whatever.
  ;; These names can only be used as the :arg-types or :result-types for VOPs
  ;; and can map to anything else that can be used as :arg-types or
  ;; :result-types (e.g. :or, :constant).
  (primitive-type-aliases (make-hash-table :test #'eq) :type hash-table)

  ;; Meta-compile time translation from names to primitive types.
  (meta-primitive-type-names (make-hash-table :test #'eq) :type hash-table)

  ;; The primitive type T is somewhat magical, in that it is the only
  ;; primitive type that overlaps with other primitive types.  An object
  ;; of primitive-type T is in the canonical descriptor (boxed or pointer)
  ;; representation.
  ;;
  ;; We stick the T primitive-type in a variable so that people who have to
  ;; special-case it can get at it conveniently.  This is done by the machine
  ;; specific VM definition, since the DEF-PRIMITIVE-TYPE for T must specify
  ;; the SCs that boxed objects can be allocated in.
  (any-primitive-type nil :type (or null primitive-type))

  ;; Hashtable translating from VOP names to the corresponding VOP-Parse
  ;; structures.  This information is only used at meta-compile time.
  (parsed-vops (make-hash-table :test #'eq) :type hash-table)

  ;; The backend specific aspects of the info environment.
  (info-environment nil :type list)

  ;; Support for the assembler.
  (instruction-formats (make-hash-table :test #'eq) :type hash-table)
  (instruction-flavors (make-hash-table :test #'equal) :type hash-table)
  (special-arg-types (make-hash-table :test #'eq) :type hash-table)
  (assembler-resources nil :type list)

  ;; The backend specific features list, if any.  During a compilation,
  ;; *features* is bound to *features* - misfeatures + features.
  (%features nil :type list)
  (misfeatures nil :type list)

  ;; Disassembler information.
  (disassem-params nil :type t)

  ;; Mappings between CTYPE structures and the corresponding predicate.
  ;; The type->predicate mapping hash is an alist because there is no
  ;; such thing as a type= hash table.
  (predicate-types (make-hash-table :test #'eq) :type hash-table)
  (type-predicates nil :type list)

  ;; Vector of the internal errors defined for this backend, or NIL if
  ;; they haven't been installed yet.
  (internal-errors nil :type (or simple-vector null))

  ;; Assembler parameters.
  (assembler-params nil :type t)

  ;; The maximum number of bytes per page on this system.  Used by genesis.
  (page-size 0 :type index))


(defprinter backend
  name)


(defvar *native-backend* (make-backend)
  "The backend for the machine we are running on. Do not change this.")
(defvar *target-backend* *native-backend*
  "The backend we are attempting to compile.")
(defvar *backend* *native-backend*
  "The backend we are using to compile with.")



;;;; Other utility functions for fiddling with the backend.

(export '(backend-features target-featurep backend-featurep native-featurep))

(defun backend-features (backend)
  "Compute the *FEATURES* list to use with BACKEND."
  (union (backend-%features backend)
	 (set-difference *features*
			 (backend-misfeatures backend))))

(defun target-featurep (feature)
  "Same as EXT:FEATUREP, except use the features found in *TARGET-BACKEND*."
  (let ((*features* (backend-features *target-backend*)))
    (featurep feature)))

(defun backend-featurep (feature)
  "Same as EXT:FEATUREP, except use the features found in *BACKEND*."
  (let ((*features* (backend-features *backend*)))
    (featurep feature)))

(defun native-featurep (feature)
  "Same as EXT:FEATUREP, except use the features found in *NATIVE-BACKEND*."
  (let ((*features* (backend-features *native-backend*)))
    (featurep feature)))


;;; NEW-BACKEND
;;;
;;; Utility for creating a new backend structure for use with cross
;;; compilers.
;;;
(defun new-backend (name features misfeatures)
  ;; If VM names a different package, rename that package so that VM doesn't
  ;; name it.  
  (let ((pkg (find-package "VM")))
    (when pkg
      (let ((pkg-name (package-name pkg)))
	(unless (string= pkg-name name)
	  (rename-package pkg pkg-name
			  (remove "VM" (package-nicknames pkg)
				  :test #'string=))
	  (unuse-package pkg "C")))))
  ;; Make sure VM names our package, creating it if necessary.
  (let* ((pkg (or (find-package name)
		  (make-package name :nicknames '("VM"))))
	 (nicknames (package-nicknames pkg)))
    (unless (member "VM" nicknames :test #'string=)
      (rename-package pkg name (cons "VM" nicknames)))
    ;; And make sure we are using the necessary packages.
    (use-package '("C-CALL" "ALIEN-INTERNALS" "ALIEN" "BIGNUM" "UNIX"
		   "LISP" "KERNEL" "EXTENSIONS" "SYSTEM" "C" "NEW-ASSEM")
		 pkg))
  ;; Make sure the native info env list is stored in *native-backend*
  (unless (backend-info-environment *native-backend*)
    (setf (backend-info-environment *native-backend*) *info-environment*))
  ;; Cons up a backend structure, filling in the info-env and features slots.
  (let ((backend (make-backend
		  :name name
		  :info-environment
		  (cons (make-info-environment
			 :name
			 (concatenate 'string name " backend"))
			(remove-if #'(lambda (name)
				       (let ((len (length name)))
					 (and (> len 8)
					      (string= name " backend"
						       :start1 (- len 8)))))
				   *info-environment*
				   :key #'info-env-name))
		  :%features features
		  :misfeatures misfeatures)))
    (setf *target-backend* backend)
    (define-standard-type-predicates)
    backend))
