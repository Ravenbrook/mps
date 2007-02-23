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

;;; ***************
;;; Overview  *****
;;; ***************
;;;
;;; Code generation for optimized slot access in methods.
;;;
;;; Parts of this code may be derived from code in Xerox-PCL's
;;; vector.lisp, although heavily rewritten.  See vector.lisp for
;;; Xerox' copyright and licensing information.
;;;
;;; ********************
;;; Entry Points  ******
;;; ********************
;;;
;;; OPTIMIZE-SLOT-ACCESS, OPTIMIZE-SLOT-READER, OPTIMIZE-SLOT-WRITER
;;; are called from WALK-METHOD-LAMBDA.
;;;
;;; *******************
;;; To Do/Ideas  ******
;;; *******************
;;;
;;; - Prevent PV lookup if only inline access is done?
;;;

(file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/method-slot-access-optimization.lisp,v 1.7 2005/06/14 12:34:59 rtoy Exp $")
 
(in-package "PCL")

(defvar *optimize-inline-slot-access-p* t
  "Set to true to activate the inline slot access optimization.")

(defvar *use-slot-types-p* t
  "When true, check slot values against specified slot types.")

(defvar *optimize-accessor-calls-p* t
  "When true, optimize slot access through slot reader/writer functions.")


;;; *******************
;;; Conditions  *******
;;; *******************

(define-condition slot-access-warning (simple-warning)
  ((class :reader slot-access-warning-class :initarg :class)
   (title :reader slot-access-warning-title :initarg :title))
  (:report (lambda (condition stream)
	     (format stream
		     (format nil "~~@<~a ~s. ~?~~@:>"
			     (slot-access-warning-title condition)
			     (slot-access-warning-class condition)
			     (simple-condition-format-control condition)
			     (simple-condition-format-arguments condition))))))

(define-condition cant-optimize-warning (slot-access-warning) ())
(define-condition method-recompilation-warning (slot-access-warning) ())

(defun cant-optimize (class format-control &rest format-args)
  (warn 'cant-optimize-warning
	:title "Cannot optimize slot access to"
	:class class
	:format-control format-control
	:format-arguments format-args))


;;; **************************
;;; Support Functions  *******
;;; **************************

;;;
;;; This macro is used to make optimized forms which should not be
;;; further optimized recognizable while walking the method body.
;;;
(defmacro optimized-marker (x) x)
(define-walker-template optimized-marker)

;;;
;;; Return the type of slot SLOT-NAME in class CLASS for slot access
;;; optimizations.  Return T if no type is specified for the slot.
;;;
(defun slot-type (class slot-name)
  (or (and *use-slot-types-p*
	   (decide-slot-type class slot-name))
      t))

;;;
;;; Add an entry for accessing slot SLOT-NAME through parameter PARAM
;;; to PARAM-SLOTS, if it's not already there.  Add PV-OFFSET to the
;;; list of pv offset forms used for accessing SLOT-NAME through PARAM,
;;; if a PV-OFFSET form is specified.
;;;
;;; Value is the name of the variable that will be bound to the slot
;;; vector of PARAM on method entry.
;;;
(defun add-param-slot-entry (param slot-name param-slots
			     &optional (pv-offset nil pv-offset-supplied-p))
  (let* ((param-entry (assoc param param-slots :test #'eq))
	 (slot-entry (assoc slot-name (cdr param-entry)
			    :test (if (symbolp slot-name) #'eq #'equal))))
    (unless slot-entry
      (setq slot-entry (list slot-name))
      (push slot-entry (cdr param-entry)))
    (when pv-offset-supplied-p
      (push pv-offset (cdr slot-entry)))
    (slot-vector-symbol (position param-entry param-slots :test #'eq))))

;;;
;;; Return true if PARAM-ENTRY has anything in it required a pv lookup.
;;;
(defun pv-optimized-param-p (param-entry)
  (loop for (slot-name . pv-offsets) in (cdr param-entry)
	thereis (or (symbolp slot-name)
		    (not (equal '(inline-access) slot-name)))))


;;; ************************************************************************
;;; ********  Optimizing SLOT-VALUE, SLOT-BOUNDP, (SETF SLOT-VALUE)  *******
;;; ************************************************************************

;;;
;;; This function is called from WALK-METHOD-LAMBDA for optimizing
;;; SLOT-VALUE, (SETF SLOT-VALUE), and SLOT-BOUNDP.
;;;
;;; FORM is the form to optimize, and ENV is the environment where
;;; the form occurs.
;;;
;;; REQUIRED-PARAMS is a list of the required parameters of the method
;;; in which FORM occurs.
;;;
;;; PARAM-SLOTS is an alist with an entry for each required parameter.
;;; The CAR of each entry is the name of a required parameter to the
;;; method.  The CDR of of each entry is a list of (SLOT-NAME
;;; PV-OFFSET ...) lists, where SLOT-NAME is either the name of a
;;; slot, or a pseudo slot name like (READER <gf-name>).  Each
;;; PV-OFFSET is a form (PV-OFFSET -1) used in a pv-optimization.  The
;;; -1 gets backpatched to an actual pv-index (index in permutation
;;; vector) by MUTATE-SLOTS.
;;;
;;; The alist is in order, so the position of an entry in the alist
;;; corresponds to the argument's position in the method lambda list.
;;;
(defun optimize-slot-access (form env required-params param-slots)
  (destructuring-bind (fn instance slot-name &optional value) form
    (multiple-value-bind (access fallback-macro)
	(ecase fn
	  (slot-value (values 'slot-value 'accessor-slot-value))
	  (slot-boundp (values 'slot-boundp 'accessor-slot-boundp))
	  (set-slot-value (values 'setf 'accessor-set-slot-value)))
      (assert (constantp slot-name))
      (let ((slot-name (eval slot-name))
	    (fallback-access `(,fallback-macro (optimized-marker ,instance)
					       ,@(cddr form))))
	(multiple-value-bind (param class optimize-p)
	    (get-param/class-to-optimize instance required-params env
					 access slot-name)
	  (cond ((not optimize-p)
		 (when param
		   (check-inline-access-p class slot-name env))
		 fallback-access)
		((and (eq *boot-state* 'complete)
		      (classp class)
		      (member *the-class-structure-object*
			      (class-precedence-list class) :test #'eq))
		 (emit-structure-access access param class slot-name value))
		((check-inline-access-p class slot-name env)
		 (emit-inline-access access param class slot-name value
				     param-slots env))
		(t
		 (emit-pv-access access param class slot-name value
				 param-slots fallback-access env))))))))

;;;
;;; Determine method parameter and its class for slot access
;;; optimization.  INSTANCE is the instance part of the form being
;;; optimized.  REQUIRED-PARAMS is a list of the names of the required
;;; parameters of the method being optimized.  ENV is the environment
;;; in which the slot access occurs.
;;;
;;; SLOT-VALUE-ACCESS being one of the symbols SLOT-VALUE,
;;; SLOT-BOUNDP, or SETF means the slot access is through one of these
;;; operators.  SLOT-NAME must be the name of the slot being accessed
;;; in these cases.  SLOT-VALUE-ACCESS NIL means the access is through
;;; a reader/writer generic function.
;;;
;;; Values are either NIL or PARAM and CLASS.  The NIL case means that
;;; this access shouldn't or can't be optimized.  Otherwise, PARAM is
;;; the name of the method parameter through which the slot is
;;; accessed and CLASS is its class or the name of its class.
;;;
(defun get-param/class-to-optimize (instance required-params env
				    &optional slot-value-access slot-name)
  (flet ((declared-class (param env)
	   (caddr (variable-declaration 'class param env))))
    (let* ((param (method-parameter instance required-params env))
	   (class-name (when param (declared-class param env)))
	   class
	   optimize-p)
      (when (and class-name (not (eq class-name t)))
	(setq class (when (eq *boot-state* 'complete)
		      (let ((class (find-class class-name nil)))
			(when (and class (class-finalized-p class))
			  class))))
	(setq optimize-p
	      ;;
	      ;; When access is through a slot reader/writer, it's
	      ;; not easy to tell which slot that reads, which we
	      ;; need below, so just optimize it and let the PV
	      ;; machinery figure out what to do.
	      (or (null slot-value-access)
		  ;;
		  ;; Problem during bootstrapping PCL: some generic
		  ;; functions are not yet available when this function
		  ;; is called, esp. SLOT-ACCESSOR-STD-P which is used
		  ;; in DECIDE-OPTIMIZE-SLOT-P.  Calling these
		  ;; functions results in an error that the funcallable
		  ;; instance function of these generic functions is
		  ;; not set.
		  (not (eq *boot-state* 'complete))
		  ;;
		  (decide-optimize-slot-p class-name slot-name
					  (ecase slot-value-access
					    (slot-value 'reader)
					    (slot-boundp 'boundp)
					    (setf 'writer))))))
      (values param (or class class-name) optimize-p))))

;;;
;;; Return true if access to slot SLOT-NAME of class CLASS should use
;;; the inline access method.
;;;
;;; Use inline slot access only if the class is known at compile time.
;;; To overcome this restriction, we would need to compute slot
;;; locations in the compile-time environment, that is, we would need
;;; to duplicate COMPUTE-SLOTS or something.  Also, we would have to
;;; prevent methods on COMPUTE-SLOTS and maybe other methods that
;;; influence slot location computation.
;;;
(defun check-inline-access-p (class slot-name env)
  (when (and *optimize-inline-slot-access-p*
	     (slot-declaration env 'inline class slot-name))
    (let ((slotd nil))
      (cond
	;;
	;; For now only for standard classes.  If this
	;; is changed we need to get smart.
	((not (standard-class-p class))
	 (cant-optimize class "The class is not a standard class"))
	;;
	((null (setq slotd (find-slot-definition class slot-name)))
	 (cant-optimize class "The class doesn't contain a slot with name ~s"
			slot-name))
	;;
	;; Class slots not implemented because it's difficult to
	;; back-patch the class slot cons cell into the code.  It's
	;; anyway not important to optimize this.
	((consp (slot-definition-location slotd))
	 (cant-optimize class "Slot ~s is a class slot" slot-name))
	;;
	;; Check for non-standard slot accessors, SLOT-VALUE-USING-CLASS.
	((not (optimize-slot-value-by-class-p class slot-name 'all))
	 (cant-optimize class "There are non-standard accessors for slot ~s"
			slot-name))
	;;
	;; Check if the accessed slot is at the same location in the
	;; class and all its subclasses.
	((not (slot-at-fixed-location-p class slot-name))
	 (cant-optimize class "Slot ~s is not at the same location ~
                               in the class and all of its subclasses"
			slot-name))
	;;
	(t t)))))

(defun optimize-slot-value-by-class-p (class slot-name type)
  (or (not (eq *boot-state* 'complete))
      (let ((slotd (find-slot-definition class slot-name)))
	(and slotd (slot-accessor-std-p slotd type)))))


;;; ***************************
;;; Inline Slot Access  *******
;;; ***************************

;;;
;;; Return a form for reading, setting, or boundp-testing slot
;;; SLOT-NAME of a inline class CLASS.
;;;
;;; ENV is the enviroment in which the slot access occurs.  It is used
;;; for the declarations in it.  ACCESS is one of the symbols
;;; SLOT-VALUE, SETF, or SLOT-BOUNDP specifying the kind of slot
;;; access to generate code for.
;;;
;;; PARAM-SLOTS is the usual alist of slots accessed through
;;; parameters; see OPTIMIZE-SLOT-ACCESS.
;;;
;;; VALUE is the new-value form for the case of SETF SLOT-VALUE.
;;;
(defun emit-inline-access (access param class slot-name value
			   param-slots env)
  (let ((slots-variable (add-param-slot-entry param '(inline-access)
					      param-slots)))
    (pushnew (class-name class) *inline-access*)
    (ecase access
      (slot-value
       (let ((boundp (slot-declaration env 'slot-boundp class slot-name)))
	 `(inline-slot-value ,param ,class ,slot-name ,slots-variable
			     ,(not boundp))))
      (setf
       `(inline-set-slot-value ,class ,slot-name ,slots-variable ,value))
      (slot-boundp
       `(inline-slot-boundp ,class ,slot-name ,slots-variable)))))

;;;
;;; The following three macros implement slot access for the inline
;;; slot access method.
;;;

(defmacro inline-slot-value (param class slot-name slots-variable
			     check-bound-p)
  (multiple-value-bind (read-form slot-type)
      (inline-slot-read-form class slot-name slots-variable)
    (if check-bound-p
	`(the ,slot-type
	   (let ((.slot-value. ,read-form))
	     (if (eq .slot-value. +slot-unbound+)
		 (inline-slot-unbound ,param ',slot-name)
		 .slot-value.)))
	`(the ,slot-type ,read-form))))

(defun inline-slot-unbound (instance slot-name)
  (values (slot-unbound (class-of instance) instance slot-name)))

(defmacro inline-slot-boundp (class slot-name slots-variable)
  (multiple-value-bind (read-form slot-type)
      (inline-slot-read-form class slot-name slots-variable)
    (declare (ignore slot-type))
    `(not (eq ,read-form +slot-unbound+))))

(defmacro inline-set-slot-value (class slot-name slots-variable value-form)
  (multiple-value-bind (read-form slot-type)
      (inline-slot-read-form class slot-name slots-variable)
    `(setf ,read-form (the ,slot-type ,value-form))))

;;;
;;; Return two values READ-FORM and SLOT-TYPE for reading slot
;;; SLOT-NAME of an instance of a inline class CLASS.  READ-FORM is a
;;; form that can be used to read the value of the slot.  SLOT-TYPE is
;;; the type of the slot.
;;;
;;; SLOTS-VARIABLE is the name of a variable bound to the instance's
;;; slot vector.
;;;
(defun inline-slot-read-form (class slot-name slots-variable)
  (let* ((slotd (find-slot-definition class slot-name))
	 (location (slot-definition-location slotd)))
    (values (etypecase location
	      (fixnum
	       `(%svref ,slots-variable ,location)))
	    (slot-type class slot-name))))

;;;
;;; True if slot SLOT-NAME has the same location in CLASS and all of
;;; its subclasses.
;;;
(defun slot-at-fixed-location-p (class slot-name)
  (labels ((location (class slot-name)
	     (let ((slot (find-slot-definition class slot-name)))
	       (slot-definition-location slot)))
	   (fixed-p (class slot-name location)
	     (loop for subclass in (class-direct-subclasses class)
		   as subclass-location = (location subclass slot-name)
		   always (and (eql location subclass-location)
			       (fixed-p subclass slot-name location)))))
    (fixed-p class slot-name (location class slot-name))))


;;; ********************************************************
;;; Inline Slot Access Info on Classes and Methods  ********
;;; ********************************************************

(defun methods-using-inline-slot-access (class)
  (let ((class (if (classp class) class (find-class class))))
    (plist-value class 'inline-access)))

(defun method-info (method)
  (plist-value method 'method-info))

(defun record-inline-access-info (method class-names method-info)
  (when (eq *boot-state* 'complete)
    (setf (plist-value method 'inline-access) class-names)
    (setf (plist-value method 'method-info) method-info)
    (loop for class-name in class-names
	  as class = (find-class class-name) do
	    (pushnew method (plist-value class 'inline-access)))))

(defun remove-inline-access-method (class method)
  (let ((methods (methods-using-inline-slot-access class)))
    (when methods
      (setf (plist-value class 'inline-access)
	    (remove method methods)))))

(defun update-inline-access (class)
  (let ((methods (loop for class in (class-precedence-list class)
		       append (methods-using-inline-slot-access class))))
    (loop for method in methods
	  as defmethod-form = (reconstruct-defmethod-form method)
	  if defmethod-form do
	    (warn "Auto-compiling method ~s." method)
	    (eval defmethod-form)
	  else
	    collect method into remaining-methods
	  finally
	    (setq methods remaining-methods))
    (when methods
      (warn 'method-recompilation-warning
	    :title "Methods may need to be recompiled for the changed ~
                    class layout of"
	    :class class
	    :format-control "~{~%     ~s~}"
	    :format-arguments (list methods)))))

(defun reconstruct-defmethod-form (method)
  (let ((method-info (method-info method)))
    (when method-info
      (destructuring-bind (body lambda-list) method-info
	(let ((method-name (generic-function-name
			    (method-generic-function method)))
	      (qualifiers (method-qualifiers method)))
	  `(defmethod ,method-name ,@qualifiers ,lambda-list
	     ,@body))))))


;;; ************************
;;; PV Slot Access  ********
;;; ************************

;;;
;;; Return a form for accessing slot SLOT-NAME of method parameter PARAM
;;; using the pv indirection.
;;;
;;; ENV is the environment in which the slot access occurs.  It is
;;; used to find declarations in it.  ACCESS is the type of access,
;;; one of SLOT-VALUE, SETF, SLOT-BOUNDP.  PARAM is the name of the
;;; method parameter whose slot is accessed, and CLASS is the
;;; parameter's class.  VALUE is a form for the value part of (SETF
;;; SLOT-VALUE).  FALLBACK-ACCESS is a form for accessing the slot
;;; when its pv-entry says the fast method can't be used.
;;;
(defun emit-pv-access (access param class slot-name value
		       param-slots fallback-access env)
  (let* ((pv-offset (list 'pv-offset -1))
	 (slots-variable (add-param-slot-entry param slot-name
					       param-slots pv-offset)))
    (ecase access
      (slot-value
       (let ((boundp (slot-declaration env 'slot-boundp class slot-name)))
	 `(pv-slot-value ,class ,slot-name ,slots-variable ,pv-offset
			 ,(not boundp) t ,fallback-access)))
      (setf
       `(pv-set-slot-value ,class ,slot-name ,slots-variable
			   ,pv-offset ,value t ,fallback-access))
      (slot-boundp
       `(pv-slot-boundp ,class ,slot-name ,slots-variable
			,pv-offset ,fallback-access)))))

;;;
;;; The following three macros expand to code for SLOT-VALUE, (SETF
;;; SLOT-VALUE), and SLOT-BOUNDP when pv optimization is used.
;;;

(defmacro pv-slot-value (class slot-name slots-variable pv-offset
			 check-bound-p use-type-p fallback-access)
  (multiple-value-bind (read-form predicate slot-type)
      (pv-slot-read-form class slot-name slots-variable)
    (let ((slot-type (if use-type-p slot-type t)))
      (if check-bound-p
	  `(the ,slot-type
	     (locally
	       (declare #.*optimize-speed*)
	       (block nil
		 (let ((.location. (%svref .pv. ,pv-offset)))
		   (tagbody
		      (unless ,predicate
			(go miss))
		      (let ((.slot-value. ,read-form))
			(when (eq +slot-unbound+ .slot-value.)
			  (go miss))
			(return .slot-value.))
		    miss
		      (return ,fallback-access))))))
	  `(the ,slot-type
	     (let ((.location. (%svref .pv. ,pv-offset)))
	       (declare #.*optimize-speed*)
	       (if ,predicate
		   ,read-form
		   ,fallback-access)))))))

(defmacro pv-slot-boundp (class slot-name slots-variable pv-offset
			  fallback-access)
  (multiple-value-bind (read-form predicate slot-type)
      (pv-slot-read-form class slot-name slots-variable)
    (declare (ignore slot-type))
    `(let ((.location. (%svref .pv. ,pv-offset)))
       (declare #.*optimize-speed*)
       (if ,predicate
	   (not (eq +slot-unbound+ ,read-form))
	   ,fallback-access))))

(defmacro pv-set-slot-value (class slot-name slots-variable
			     pv-offset value use-type-p fallback-access)
  (multiple-value-bind (read-form predicate slot-type)
      (pv-slot-read-form class slot-name slots-variable)
    ;;
    ;; Frob the fallback form because we put the value in a let-binding
    ;; for checking its type.
    (let ((fallback-access
	   (let ((copy (copy-seq fallback-access)))
	     (ecase (car fallback-access)
	       ((funcall setf)
		(setf (third copy) '(optimized-marker .value.)))
	       (accessor-set-slot-value
		(setf (fourth copy) '.value.)))
	     copy))
	  (slot-type (if use-type-p slot-type t)))
      `(let ((.value. (the ,slot-type ,value)))
	 (let ((.location. (%svref .pv. ,pv-offset)))
	   (declare #.*optimize-speed*)
	   (if ,predicate
	       (setf ,read-form .value.)
	       ,fallback-access))))))

;;;
;;; Return three values READ-FORM, PREDICATE, SLOT-TYPE for access
;;; to slot SLOT-NAME of class CLASS.  SLOTS-VARIABLE is the name of
;;; the local variable holding the instance's slot vector.
;;;
;;; READ-FORM is a form that can be used to read the value of the
;;; slot.
;;;
;;; The returned PREDICATE is a form that can be used to check if a
;;; permutation vector contains a valid entry for the slot access.
;;; The variable .LOCATION. is assumed to hold the slot location that
;;; was retrieved from the pv vector.
;;;
;;; SLOT-TYPE is the slot's type.
;;;
(defun pv-slot-read-form (class slot-name slots-variable)
  (let ((slot-type (slot-type class slot-name))
	(likely-a-class-slot-p 
	 (and (eq *boot-state* 'complete)
	      (constantp class)
	      (constantp slot-name)
	      (decide-class-slot-p (eval class) (eval slot-name)))))
      (if likely-a-class-slot-p
	  (values '(cdr .location.)
		  '(consp .location.)
		  slot-type)
	  (values `(%svref ,slots-variable .location.)
		  '(fixnump .location.)
		  slot-type))))


;;; ******************************
;;; Structure Slot Access  *******
;;; ******************************

(defun emit-structure-access (access param class slot-name value)
  (let* ((slotd (find-slot-definition class slot-name))
	 (accessor (slot-definition-defstruct-accessor-symbol slotd)))
    (ecase access
      (slot-value `(,accessor ,param))
      (setf `(setf (,accessor ,param) ,value))
      (slot-boundp t))))


;;; ********************************************************
;;; ********  Slot Reader/Writer Call Optimization  ********
;;; ********************************************************

;;;
;;; Optimize slot reader function calls in the same way as SLOT-VALUE.
;;; ??? I think FORM can be (APPLY ...), which we could optimize here
;;; as well, for completeness.
;;;
(defun optimize-slot-reader (form required-parms param-slots env)
  (when (and (eq *boot-state* 'complete)
	     *optimize-accessor-calls-p*
	     (or (not (consp (cadr form)))
		 (not (eq 'optimized-marker (caadr form)))))
    (destructuring-bind (gf-name instance) form
      (when (decide-optimize-accessor-p gf-name 'reader)
	(multiple-value-bind (param class optimize-p)
	    (get-param/class-to-optimize instance required-parms env)
	  (when optimize-p
	    (setq form (optimize-slot-access/call
			param-slots 'slot-value param class
			gf-name nil env)))))))
  form)

;;;
;;; Optimize setf's of slot readers.  Called from WALK-METHOD-LAMBDA.
;;;
;;; FORM is (setf (GF OBJECT) VALUE), where GF is known to be a
;;; generic function.  REQUIRED-PARMS is a list of the required
;;; parameters of the method in whose body FORM appears.  PARAM-SLOTS
;;; is an alist of pairs (PARM . PV-OFFSETS).  PARM is one of the
;;; required parameters.
;;; 
;;; Value is either an optimized form that replaces FORM, or FORM.
;;;
(defun optimize-slot-writer (form required-parms param-slots env)
  (when (and (eq *boot-state* 'complete)
	     *optimize-accessor-calls-p*)
    (destructuring-bind (setf (reader instance) value) form
      (declare (ignore setf))
      (let ((gf-name `(setf ,reader)))
	(when (decide-optimize-accessor-p gf-name 'writer)
	  (multiple-value-bind (param class optimize-p)
	      (get-param/class-to-optimize instance required-parms env)
	    (when optimize-p
	      (setq form (optimize-slot-access/call
			  param-slots 'setf param class
			  gf-name value env))))))))
  form)

;;;
;;; Generate code for reading/writing a slot using a slot accessor
;;; generic function.
;;;
;;; ACCESS is one of SETF or SLOT-VALUE for writing or reading a slot.
;;; PARAM is the method parameter through which the slot is accessed,
;;; and CLASS is its class.  GF-NAME is the name of the generic
;;; reader or writer function called to access the slot.  VALUE is the
;;; value part for the SETF case.  ENV is the environment in which the
;;; access occurs.
;;;
(defun optimize-slot-access/call (param-slots access param class
				  gf-name value env)
  (let ((slot-name (check-inline-accessor-call-p access gf-name class env)))
    (if slot-name
	(emit-inline-access/call param-slots access param class 
				 slot-name value env)
	(emit-pv-access/call param-slots access param class gf-name
			     value env))))

;;;
;;; Check if a slot accessor call can/should be optimized to a inline
;;; slot access.  ACCESS is one of SLOT-VALUE or SETF for read or write
;;; access.  GF-NAME is the name of the generic function being called.
;;; CLASS is the class of the instance being accessed.  ENV is the
;;; environment in which the access occurs.  Value is true if the call
;;; should and can be optimized.
;;; 
(defun check-inline-accessor-call-p (access gf-name class env)
  (when *optimize-inline-slot-access-p*
    ;;
    ;; Check if CLASS is defined.  If not, we can't use inline
    ;; access because we won't be able to determine slot locations.
    (unless (std-class-p class)
      (let ((real-class (find-class class nil)))
	(unless (std-class-p real-class)
	  (when (slot-declaration env 'inline class)
	    (cant-optimize class "The class is not defined at compile time"))
	  (return-from check-inline-accessor-call-p nil))
	(setq class real-class)))
    ;;
    (flet ((declared-inline (slot-name)
	     (slot-declaration env 'inline class slot-name)))
      (multiple-value-bind (slot-names all-standard-accessors-p)
	  (slot-accessor-slot-names access gf-name class)
	(cond ((and all-standard-accessors-p
		    slot-names
		    (null (cdr slot-names))
		    (check-inline-access-p class (car slot-names) env))
	       (car slot-names))
	      ((not (some #'declared-inline slot-names))
	       nil)
	      ((not all-standard-accessors-p)
	       (cant-optimize class "~s has a method that is not a standard ~
                                    slot accessor" gf-name))
	      (t
	       (cant-optimize class "Methods of ~s access different slots"
			      gf-name)))))))

;;;
;;; Return two values SLOT-NAMES, ALL-STANDARD-P.  SLOT-NAMES is a
;;; list of the names of slots accessed by standard accessor methods
;;; of type ACCESS (one of SETF/SLOT-VALUE for writers/readers)
;;; applicable to CLASS.  ALL-STANDARD-P true means all applicable
;;; methods are standard accessor methods.
;;;
(defun slot-accessor-slot-names (access gf-name class)
  (loop with all-standard-p = t
	for method in (compute-applicable-methods
		       (gdefinition gf-name)
		       (let ((proto (class-prototype class)))
			 (ecase access
			   (slot-value (list proto))
			   (setf (list t proto)))))
	if (not (standard-accessor-method-p method)) do
	  (setq all-standard-p nil)
	else
	  collect (slot-definition-name
		   (accessor-method-slot-definition method)) into slot-names
	finally
	  (return (values slot-names all-standard-p))))

;;;
;;; Emit code for a pv slot accessor call.
;;;
;;; This generates entries ((READER <gf-name>) <pv-offet-form> ...) or
;;; ((WRITER <gf-name>) <pv-offet-form> ...) in PARAM-SLOTS.
;;;
(defun emit-pv-access/call (param-slots access param class gf-name value env)
  (let* ((slot-name (ecase access
		      (slot-value `(reader ,gf-name))
		      (setf `(writer ,gf-name))))
	 (pv-offset (list 'pv-offset -1))
	 (slots-variable
	  (add-param-slot-entry param slot-name param-slots pv-offset)))
    (ecase access
      (slot-value
       (let ((boundp (slot-declaration env 'slot-boundp class slot-name)))
	 `(pv-slot-value ,class ,slot-name ,slots-variable ,pv-offset
			 ,(not boundp) nil
			 (,gf-name (optimized-marker ,param)))))
      (setf
       `(pv-set-slot-value ,class ,slot-name ,slots-variable
			   ,pv-offset ,value nil
			   (funcall #',gf-name (optimized-marker ,value)
				    ,param))))))

;;;
;;; Emit code for a inline slot accessor call.  This generates entries
;;; ((INLINE-ACCESS)) in PARAM-SLOTS.
;;;
(defun emit-inline-access/call (param-slots access param class 
				slot-name value env)
  (let ((slots-variable (add-param-slot-entry param '(inline-access)
					      param-slots)))
    (pushnew (class-name class) *inline-access*)
    (ecase access
      (slot-value
       (let ((boundp (slot-declaration env 'slot-boundp class slot-name)))
	 `(inline-slot-value ,param ,class ,slot-name ,slots-variable
			     ,(not boundp))))
      (setf
       `(inline-set-slot-value ,class ,slot-name ,slots-variable ,value)))))

;; end of method-slot-access-optimization.lisp
