;;; Copyright (C) 2002, 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

;;; The function SPLIT-DECLARATION appeared first in Xerox PCL
;;; vector.lisp, and was moved here for concentrating all declaration
;;; stuff.  See vector.lisp for Xerox' copyright and license.

;;; To Do
;;;
;;; GF is actually non-accessor GF.  Clean this up.
;;; (setf symbol-value) should be handled like (setf fdefinition)

(file-comment "$Header: /project/cmucl/cvsroot/src/pcl/info.lisp,v 1.11 2005/06/14 12:34:59 rtoy Exp $")

(in-package "PCL")

(defstruct class-info
  ;;
  ;; List of SLOT-INFO structures for each slot.
  (slots () :type list)
  ;;
  ;; The class' metaclass.
  (metaclass nil :type symbol)
  ;;
  ;; List of slot reader and writer function names.
  (accessors () :type list)
  ;;
  ;; Slot access optimization info.
  (slot-access () :type list))


(defstruct slot-info
  (name nil :type symbol)
  ;;
  ;; Specified slot allocation.
  (allocation :instance :type (or (member :class :instance) t))
  ;;
  ;; Specified slot type or T.
  (type t :type (or symbol list)))


(defstruct gf-info
  ;;
  ;; Number of required parameters.
  (nreq 0 :type fixnum)
  ;;
  ;; True if this generic function has key, optional, or rest parameters.
  (applyp nil :type boolean)
  ;;
  ;; READER if this generic function has a slot reader method.
  ;; WRITER if this gf has a slot writer method.
  ;; NIL if this is an ordinary generic function.
  (type nil :type (member nil reader writer))
  ;;
  ;; List of class names for slot accessor.  A class C occurring in
  ;; this list means that this is a slot accessor for some slot of C.
  (classes () :type list)
  ;;
  ;; Alist of AUTO-COMPILE declaration infos for methods.  Keys are
  ;; (QUALIFIER* SPECIALIZERS), value is T or NIL for do or don't
  ;; auto-compile.
  (auto-compile () :type list)
  ;;
  ;; The default auto-compilation policy used for methods of this
  ;; generic function which don't have explicit entries in
  ;; AUTO-COMPILE.  T means auto-compile, NIL don't auto-compile,
  ;; MAYBE means use the global default.
  (auto-compile-default 'maybe :type (member t nil maybe)))

(defstruct seal-info
  (seals () :type list))

;;;
;;; Declare some new INFO type, under class PCL, plus nicer accessors.
;;;
(define-info-class pcl)
(define-info-type pcl class (or null class-info) nil)
(define-info-type pcl gf (or null gf-info) nil)
(define-info-type pcl seal (or null seal-info) nil)

(declaim (inline class-info (setf class-info)
		 gf-info (setf gf-info)
		 seal-info (setf seal-info)))

(defun class-info (class-name)
  (info pcl class class-name))

(defun (setf class-info) (new-value class-name)
  (setf (info pcl class class-name) new-value))

(defun class-info-or-make (class-name)
  (let ((info (class-info class-name)))
    (or info
	(setf (class-info class-name) (make-class-info)))))

(defun gf-info (gf-name)
  (info pcl gf gf-name))

(defun (setf gf-info) (new-value gf-name)
  (setf (info pcl gf gf-name) new-value))

(defun gf-info-or-make (gf-name)
  (let ((info (gf-info gf-name)))
    (or info
	(setf (gf-info gf-name) (make-gf-info)))))

(defun seal-info (name)
  (info pcl seal name))

(defun (setf seal-info) (new-value name)
  (setf (info pcl seal name) new-value))

(defun seal-info-or-make (name)
  (let ((info (seal-info name)))
    (or info
	(setf (seal-info name) (make-seal-info)))))


;;; ******************
;;; Classes   ********
;;; ******************

;;;
;;; Set compile-time info for class CLASS-NAME.  Called from
;;; EXPAND-DEFCLASS.
;;;
;;; METACLASS is the metaclass of the declared class.  SLOTS is a
;;; list of slot specs as they appear in DEFCLASS.
;;;
(defun set-class-info (class-name metaclass slots)
  (let ((readers ())
	(writers ())
	(slot-infos ())
	(class-info (class-info class-name)))
    (when class-info
      (loop for a in (class-info-accessors class-info)
	    as info = (gf-info a)
	    when info do
	      ;; INFO can be NIL when the accessor has been redefined
	      ;; as an ordinary function.
	      (setf (gf-info-classes info)
		    (delete class-name (gf-info-classes info)))))
    ;;
    (dolist (slot slots)
      (typecase slot
	(list
	 (loop for (key value) on (cdr slot) by #'cddr do
		 (case key
		   (:reader
		    (push value readers))
		   (:writer
		    (push value writers))
		   (:accessor
		    (push value readers)
		    (push `(setf ,value) writers))))
	 (push (make-slot-info
		:name (car slot)
		:allocation (getf (cdr slot) :allocation :instance)
		:type (getf (cdr slot) :type t))
	       slot-infos))
	(t
	 (push (make-slot-info :name slot) slot-infos))))
    ;;
    (setf (class-info class-name)
	  (make-class-info
	   :slots (nreverse slot-infos)
	   :accessors (append readers writers)
	   :metaclass metaclass
	   :slot-access (and class-info
			     (class-info-slot-access class-info))))
    ;;
    (flet ((add-class (accessors type)
	     (loop for a in accessors as info = (gf-info a)
		   if (null info) do
		     (setf (gf-info a)
			   (make-gf-info :nreq (ecase type
						 (reader 1)
						 (writer 2))
					 :applyp nil
					 :classes (list class-name)
					 :type type))
		   else do
		     (setf (gf-info-type info) type)
		     (push class-name (gf-info-classes info)))))
      (add-class readers 'reader)
      (add-class writers 'writer))))

;;;
;;; Decide what type slot SLOT-NAME in CLASS-OR-NAME has.
;;; CLASS-OR-NAME can be a class or class name.
;;;
(defun decide-slot-type (class-or-name slot-name)
  (or (let* ((class-info (class-info (if (symbolp class-or-name)
					 class-or-name
					 (class-name class-or-name))))
	     (slot-info (when class-info
			  (find slot-name (class-info-slots class-info)
				:key #'slot-info-name :test #'eq))))
	(when slot-info
	  (slot-info-type slot-info)))
      (let* ((class (if (symbolp class-or-name)
			(find-class class-or-name nil)
			class-or-name))
	     (slotd (when (if (eq *boot-state* 'complete)
			      (std-class-p class)
			      class)
		      (find-slot-definition class slot-name))))
	(when slotd
	  (slot-definition-type slotd)))))

;;;
;;; Test if a class names a STANDARD-CLASS or FUNCALLABLE-STANDARD-CLASS.
;;;
(macrolet ((defpred (name metaclass)
	     `(defun ,name (class-name)
		(let ((class (find-class class-name nil)))
		  (if class
		      (typep class ',metaclass)
		      (let* ((info (class-info class-name))
			     (meta (and info (class-info-metaclass info))))
			(eq meta ',metaclass)))))))
  (defpred info-funcallable-standard-class-p funcallable-standard-class)
  (defpred info-standard-class-p standard-class))


;;; **************
;;; Slots  *******
;;; **************

;;;
;;; Return true if NAME should be considerd a slot reader/writer name.
;;;
(defun info-accessor-p (name)
  (or (let ((info (gf-info name)))
	(when info
	  (not (null (gf-info-type info)))))
      (when (and (eq *boot-state* 'complete)
		 (fboundp name))
	(let ((gf (gdefinition name))
	      (accessorp nil))
	  (when (generic-function-p gf)
	    (loop for method in (generic-function-methods gf)
		  when (standard-accessor-method-p method) do
		    (setq accessorp t)
		    (let ((type (if (standard-reader-method-p method)
				    'reader 'writer)))
		      (setf (gf-info name)
			    (make-gf-info :nreq (ecase type
						  (reader 1)
						  (writer 2))
					  :applyp nil
					  :type type
					  :classes ())))
		    (loop-finish)))
	  accessorp))))

;;;
;;; Decide if a call to GF-NAME should be optimized as a slot
;;; reader/writer call of type TYPE.  TYPE must be one of the symbols
;;; READER or WRITER.
;;;
(defun decide-optimize-accessor-p (gf-name type)
  (let ((info (gf-info gf-name)))
    (when info
      (eq type (gf-info-type info)))))

;;;
;;; Decide if access to slot SLOT-NAME in class CLASS-NAME should be
;;; optimized.  TYPE is the type of access, one of READER/WRITER/ALL.
;;;
;;; This function is called from COMPUTE-PV-SLOT and from the code
;;; actually generating the code for optimized slot access.  It's
;;; suboptimal but okay if this function errs: either we'll be doing a
;;; superfluous optimization or prevent a possible optimization.  The
;;; PV code must anyway be able to cope with optimizations that must
;;; be "undone" by setting pv-values to nil.
;;;
;;; It's worthwhile optimizing slot access if we know or suspect at
;;; method compilation time that slots are accessed in the standard
;;; way, which means that there is no non-standard
;;; SLOT-VALUE-USING-CLASS method defined.
;;; 
;;; If we know that there is or will be such a method, it's not worth
;;; optimizing.
;;;
(defun decide-optimize-slot-p (class-name slot-name type)
  (or (let ((class-info (class-info class-name)))
	(and class-info
	     (member slot-name (class-info-slots class-info)
		     :key #'slot-info-name :test #'eq)
	     ;; FUNCALLABLE-STANDARD-CLASS might be ok, too, I haven't
	     ;; checked.  It's not terribly important, though.
	     (eq (class-info-metaclass class-info) 'standard-class)))
      (let* ((class (find-class class-name nil))
	     (slotd (and class (find-slot-definition class slot-name))))
	(when slotd
	  (slot-accessor-std-p slotd type)))))

;;;
;;; Decide if SLOT-NAME should be considered a class slot in class
;;; CLASS-NAME.
;;;
(defun decide-class-slot-p (class-name slot-name)
  (or (let* ((class-info (class-info class-name))
	     (slot-info (and class-info
			     (find slot-name (class-info-slots class-info)
				   :key #'slot-info-name :test #'eq))))
	(when slot-info
	  (eq :class (slot-info-allocation slot-info))))
      (let* ((class (find-class class-name nil))
	     (slotd (and class (find-slot-definition class slot-name))))
	(when slotd
	  (eq :class (slot-definition-allocation slotd))))))


;;; ***************************
;;; Generic Functions   *******
;;; ***************************

;;;
;;; Record compile-time information about the declaration of
;;; generic function GF-NAME with lambda list LAMBDA-LIST.
;;;
(defun set-gf-info (gf-name lambda-list)
  (multiple-value-bind (required optional restp rest keyp keys
				 allow-other-keys-p)
      (parse-generic-function-lambda-list lambda-list)
    (declare (ignore rest keys))
    (setf (gf-info gf-name)
	  (make-gf-info :nreq (length required)
			;; Like in ARG-INFO-APPLYP.
			:applyp (or restp keyp (not (null optional))
				    allow-other-keys-p)))))

;;;
;;; Return true if NAME is the name of a known generic function.
;;;
(defun info-gf-name-p (name)
  (when (valid-function-name-p name)
    (or (gf-info name)
	(when (and (fboundp name)
		   (eq *boot-state* 'complete))
	  (let ((gf (gdefinition name)))
	    (when (standard-generic-function-p gf)
	      (set-gf-info name (generic-function-lambda-list gf))
	      t))))))

;;;
;;; Called to update the info database and pv caches when function
;;; NAME gets redefined to NEWDEF.  If NAME is a generic function,
;;; flush pv caches referring to it.
;;;
(defun %check-gf-redefinition (name newdef)
  (when (eq *boot-state* 'complete)
    (let ((gf (and (fboundp name)
		   (not (memq (car-safe name)
			      '(effective-method method fast-method
				slot-accessor)))
		   (gdefinition name))))
      (when (and gf
		 (generic-function-p gf)
		 (not (generic-function-p newdef)))
	(setf (gf-info name) nil)
	(update-accessor-pvs 'removed-gf gf)
	(update-pv-calls-for-gf gf 'removed-gf)))))

(push #'%check-gf-redefinition lisp::*setf-fdefinition-hook*)


;;; **********************
;;; Declarations  ********
;;; **********************

(defvar *non-variable-declarations*
  '(ftype
    inline
    method-lambda-list
    method-name
    notinline
    optimize
    values))

(defvar *variable-declarations-with-argument*
  '(class
    type))

(defvar *variable-declarations-without-argument*
  '(array
    atom
    base-char
    bignum
    bit
    bit-vector
    character
    compiled-function
    complex
    cons
    double-float
    dynamic-extent
    extended-char
    fixnum
    float function
    hash-table
    ignorable
    ignore
    integer
    keyword
    list
    long-float
    nil
    null
    number
    package
    pathname
    random-state
    ratio
    rational
    readtable
    sequence
    short-float
    signed-byte
    simple-array
    simple-bit-vector
    simple-string
    simple-vector
    single-float
    special
    standard-char
    stream
    string
    symbol
    t
    unsigned-byte
    vector))

(defvar *slots-qualities*
  '(inline
    slot-boundp))

(defun split-declarations (body args calls-next-method-p)
  (declare (ignore calls-next-method-p))
  (let ((inner-decls nil) (outer-decls nil) decl)
    (loop (when (null body) (return nil))
	  (setq decl (car body))
	  (unless (and (consp decl)
		       (eq (car decl) 'declare))
	    (return nil))
	  (dolist (form (cdr decl))
	    (when (consp form)
	      (let ((declaration-name (car form)))
		(if (member declaration-name *non-variable-declarations*)
		    (push `(declare ,form) outer-decls)
		    (let ((arg-p
			   (member declaration-name
				   *variable-declarations-with-argument*))
			  (non-arg-p
			   (member declaration-name
				   *variable-declarations-without-argument*))
			  (dname (list (pop form))))
		      (unless (or arg-p non-arg-p)
			(warn "~@<The declaration ~S is not understood by ~S. ~
                               Please put ~S on one of the lists ~S, ~S, or ~S. ~
                               (Assuming it is a variable declarations without ~
                               argument).~@:>"
			      declaration-name 'split-declarations
			      declaration-name
			      '*non-variable-declarations*
			      '*variable-declarations-with-argument*
			      '*variable-declarations-without-argument*)
			(push declaration-name
			      *variable-declarations-without-argument*))
		      (when arg-p
			(setq dname (append dname (list (pop form)))))
		      ;;
		      ;; The PCL-internal declaration CLASS has the
		      ;; form (DECLARE (CLASS <parameter> <class>),
		      ;; which is incompatible with what the DOLIST
		      ;; below does.
		      (case (car dname)
			(class
			 (push `(declare (,@dname ,@form)) inner-decls))
			(t
			 (let ((inners nil) (outers nil))
			   (dolist (var form)
			     (if (member var args)
				 ;; Quietly remove ignore declarations
				 ;; on args to prevent compiler warns
				 ;; about ignored args being read in
				 ;; CALL-NEXT-METHOD.
				 (unless (eq (car dname) 'ignore)
				   (push var outers))
				 (push var inners)))
			   (when outers
			     (push `(declare (,@dname ,@outers)) outer-decls))
			   (when inners
			     (push `(declare (,@dname ,@inners)) inner-decls))))))))))
	  (setq body (cdr body)))
    (values outer-decls inner-decls body)))


;;;; ********************************
;;;; Adding New Declarations  *******
;;;; ********************************

(eval-when (compile load eval)
  (defvar *declaration-handlers* ())
  
  (defun %define-declaration (decl-name handler-name)
    (let ((entry (assoc decl-name *declaration-handlers*)))
      (if entry
	  (setf (cdr entry) handler-name)
	  (setq *declaration-handlers*
		(acons decl-name handler-name *declaration-handlers*))))))

(defmacro define-declaration (decl-name lambda-list &body body)
  (let ((handler-name (symbolicate 'handle- decl-name '-declaration)))
    `(eval-when (compile load eval)
       (declaim (declaration ,decl-name))
       (defun ,handler-name ,lambda-list ,@body)
       (%define-declaration ',decl-name ',handler-name))))

(defun proclamation-hook (form)
  (when (consp form)
    (let ((handler (cdr (assoc (car form) *declaration-handlers*))))
      (when handler
	(funcall handler form)))))

(pushnew 'proclamation-hook c::*proclamation-hooks*)


;;;; ***************************
;;;; SLOTS declaration  ********
;;;; ***************************

(pushnew 'slots *variable-declarations-with-argument*)
(pushnew 'slots walker:*variable-declarations*)

(define-declaration slots (form)
  (flet ((invalid (&optional subform)
	   (if subform
	       (warn "~@<Invalid slot access specifier ~s in ~s.~@:>"
		     subform form)
	       (warn "~@<Invalid slot access declaration ~s.~@:>"
		     form))))
    (dolist (specifier (cdr form))
      (if (and (consp specifier)
	       (memq (car specifier) *slots-qualities*))
	  (dolist (class-entry (cdr specifier))
	    (let (class slots)
	      (typecase class-entry
		(symbol
		 (setq class class-entry))
		(cons
		 (setq class (car class-entry))
		 (setq slots (cdr class-entry)))
		(t
		 (invalid specifier)))
	      (when class
		(let* ((info (class-info-or-make class))
		       (entry (assoc (car specifier)
				     (class-info-slot-access info))))
		  (if entry
		      (setf (cdr entry) slots)
		      (push (list* (car specifier) slots)
			    (class-info-slot-access info)))))))
	  (invalid)))))

;;;
;;; True if there is a slot optimization declaration DECLARATION in
;;; ENV for CLASS and SLOT-NAME.  Slot optimization declarations look
;;; like
;;;
;;; declare (pcl:slots specifier*)
;;;
;;; specifier ::= (quality class-entry*)
;;; quality ::= SLOT-BOUNDP | INLINE
;;; class-entry ::= class | (class slot-name*)
;;; class ::= the name of a class
;;; slot-name ::= the name of a slot
;;;
;;; Examples:
;;;
;;; (declare (slots (slot-boundp my-class)))
;;; (declare (slots (inline (my-class slot-a))))
;;;
(defun slot-declaration (env quality class &optional slot-name)
  (assert (memq quality *slots-qualities*))
  (or (when env
	(local-slot-declaration env quality class slot-name))
      (global-slot-declaration quality class slot-name)))
	     
(defun local-slot-declaration (env quality class &optional slot-name)
  (let ((class-name (if (symbolp class) class (class-name class))))
    (dolist (decl (walker::env-declarations env))
      (when (and (consp decl)
		 (eq (car decl) 'slots)
		 (slot-access-specifier (cdr decl) quality class-name
					slot-name))
	(return t)))))

(defun slot-access-specifier (specifiers quality class-name
			      &optional slot-name)
  (dolist (specifier specifiers)
    (when (and (consp specifier) (eq quality (car specifier)))
      (dolist (entry (cdr specifier))
	(when (typecase entry
		(symbol
		 (eq entry class-name))
		(cons
		 (and (eq (car entry) class-name)
		      (or (null slot-name)
			  (null (cdr entry))
			  (memq slot-name (cdr entry)))))
		(t (warn "~@<Invalid slot access declaration ~s.~@:>"
			 specifier)))
	  (return-from slot-access-specifier entry))))))

(defun global-slot-declaration (quality class &optional slot-name)
  (let* ((info (class-info (if (symbolp class) class (class-name class))))
	 (spec (when info (assq quality (class-info-slot-access info)))))
    (and spec
	 (or (null slot-name)
	     (null (cdr spec))
	     (memq slot-name (cdr spec))))))
    
(defun class-has-a-forward-referenced-superclass-p (class)
  (or (forward-referenced-class-p class)
      (some #'class-has-a-forward-referenced-superclass-p
	    (class-direct-superclasses class))))	 
      
;;;
;;; Return true if class CLASS-NAME should be defined at compile time.
;;; Called from EXPAND-DEFCLASS.  ENV is the environment in which the
;;; DEFCLASS of CLASS-NAME appears.  SUPERS is the list of superclass
;;; names for CLASS-NAME.
;;;
(defun define-class-at-compile-time-p (env class-name &optional supers)
  (or (slot-declaration env 'inline class-name)
      (some (lambda (super)
	      (let ((class (find-class super nil)))
		(and class
		     (not (class-has-a-forward-referenced-superclass-p class))
		     (some (lambda (c)
			     (slot-declaration env 'inline c))
			   (class-precedence-list class)))))
	    supers)))


;;;; *********************************
;;;; AUTO-COMPILE declaration  *******
;;;; *********************************

(define-declaration auto-compile (form)
  (auto-compile-proclamation form t))

(define-declaration not-auto-compile (form)
  (auto-compile-proclamation form nil))

(defvar *auto-compile-global-default* nil)

(defun auto-compile-proclamation (form compilep)
  (flet ((invalid (&optional subform)
	   (if subform
	       (warn "~@<Invalid auto-compile specifier ~s in ~s.~@:>"
		     subform form)
	       (warn "~@<Invalid auto-compile declaration ~s.~@:>"
		     form)))
	 (gf-name-p (name)
	   (valid-function-name-p name)))
    (if (null (cdr form))
	(setq *auto-compile-global-default* compilep)
	(dolist (specifier (cdr form))
	  (cond ((or (gf-name-p specifier)
		     (and (consp specifier)
			  (gf-name-p (car specifier))
			  (null (cdr specifier))))
		 (let ((info (gf-info-or-make (if (gf-name-p specifier)
						  specifier
						  (car specifier)))))
		   (setf (gf-info-auto-compile-default info) compilep)))
		((and (consp specifier)
		      (gf-name-p (car specifier)))
		 (let* ((info (gf-info-or-make (car specifier)))
			(entry (assoc (cdr specifier)
				      (gf-info-auto-compile info)
				      :test #'equal)))
		   (if entry
		       (setf (cdr entry) compilep)
		       (setf (gf-info-auto-compile info)
			     (acons (cdr specifier) compilep
				    (gf-info-auto-compile info))))))
		(t
		 (invalid specifier)))))))

;;;
;;; Return true if method with the given name, qualifiers, and
;;; specializers should be auto-compiled.
;;;
(defun auto-compile-p (gf-name qualifiers specializers)
  (when (eq *boot-state* 'complete)
    (let* ((info (gf-info gf-name))
	   (key (append qualifiers (list specializers)))
	   (compilep (if info
			 (let ((entry (assoc key (gf-info-auto-compile info)
					     :test #'equal)))
			   (if entry
			       (cdr entry)
			       (gf-info-auto-compile-default info)))
			 'maybe)))
      (if (eq 'maybe compilep)
	  *auto-compile-global-default*
	  compilep))))

;;; end of info.lisp
