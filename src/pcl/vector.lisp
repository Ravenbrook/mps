;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;

(file-comment
  "$Header: /project/cmucl/cvsroot/src/pcl/vector.lisp,v 1.29 2003/05/07 17:14:24 gerd Exp $")

(in-package :pcl)

;;;
;;; Return the index of the slot named SLOT-NAME in the slot vector of
;;; instances whose wrapper is WRAPPER.
;;;
(declaim (inline instance-slot-index))

(defun instance-slot-index (wrapper slot-name)
  (loop for name in (wrapper-instance-slots-layout wrapper)
	for index of-type fixnum from 0
	when (eq slot-name name)
	  return index))


(deftype pv-index () `(integer 0 ,most-positive-fixnum))
(deftype param-index () `(integer 0 ,most-positive-fixnum))

(defstruct pv-table
  ;;
  ;; This cache maps N wrappers, one for each required method
  ;; parameter for which PV optimization has been done, to a pair (PV
  ;; . CALLS).
  ;;
  ;; PV is a "permutation vector" of size PV-SIZE (see below).  Each
  ;; slot name (and (READER <gf-name>) or (WRITER <gf-name>)) used in
  ;; an pv optimization is assigned an index in this vector.  For
  ;; instance slots, the value of PV at that index is a fixnum, the
  ;; index the slot's value an instance's slot value vector.  For
  ;; class slots, the PV slot contains the cons cell of the class
  ;; slot.  If the PV contains neither a fixnum nor a cons, the PV
  ;; optimization code executes a default form.
  ;;
  ;; CALLS is currently not used.
  (cache nil :type (or cache null))
  ;;
  ;; Size of permutation vectors in this table.  This is 1+ the
  ;; highest slot pv index (see below).
  (pv-size 0 :type fixnum)
  ;;
  ;; This list has an entry for each required parameter of the
  ;; pv-table's method.  List element at index K corresponds to the
  ;; Kth parameter P_k.  Entries for parameters for which no
  ;; pv-optimization has been done are NIL.  Other entries have the
  ;; form (NIL SLOT-NAME SLOT-NAME ...)  where each SLOT-NAME is
  ;; either
  ;;
  ;; -- the name of a slot used in a pv-optimization involving P_k
  ;; (that is, say, (SLOT-VALUE P_k SLOT-NAME) has been optimized).
  ;;
  ;; -- a list (READER <gf-name>) or (WRITER <gf-name>), meaning that
  ;; a slot reader/writer call has been pv-optimized, say (GF-NAME P_k).
  ;; GF-NAME is the name of the slot accessor generic function.
  ;;
  ;; A T in the car of an parameter entry means that the corresponding
  ;; method parameter is used in a generic function call optimization.
  ;;
  ;; Each SLOT-NAME has a "pv-index" >= 0 assigned to it, which is the
  ;; index in permutation vectors of this pv-table, under which the
  ;; pv-value (fixnum, cons, or nil) for the slot name is found.
  (slot-name-lists nil :type list)
  ;;
  ;; Contains elements (GF-NAME i0 i1 ...), for each generic function
  ;; call optimized.  GF-NAME is the name of the generic function
  ;; called; iN is the index of the method parameter passed as
  ;; argument N to the generic function.  Only method parameters used
  ;; in pv-optimization are counted, that is, the indices give the 
  ;; index in the wrappers kept in the cache.
  (call-list nil :type list)
  ;;
  ;; This is set to T while computing calls.  The slot is needed to
  ;; prevent GET-METHOD-FUNCTION from using PV-TABLE-LOOKUP to determine
  ;; a pv cell when GET-METHOD-FUNCTION is actually called while computing
  ;; the pv-table cache.
  (computing-cache-p nil :type boolean))

(declaim (freeze-type pv-table))


;;;
;;; Hash-table mapping slot name lists and call lists to canonical
;;; representations.
;;;
(defvar *calls+snls->canonical* (make-hash-table :test 'equal))

;;;
;;; Hash-table mapping a list of calls + slot name lists to a
;;; corresponding PV-TABLE for it.
;;;
(defvar *calls+snls->pv-table* (make-hash-table :test 'equal))

;;;
;;; Values are lists of pv-tables.  Keys are slot names or (READER
;;; <gf-name>), (WRITER <gf-name>), or (CALL <gf-name>).  Each
;;; pv-table is one containing the key in one of its slot name lists,
;;; or, for CALL, having <gf-name> in its call-list.
;;;
(defvar *pv-key->pv-tables* (make-hash-table :test 'equal))

;;;
;;; Execute BODY for all slot-names, (READER <gf-name>) etc.  of
;;; TABLE.  Bind KEY to the key in question, INDEX to its index
;;; in permutation vectors of TABLE, PARAM-INDEX to the method
;;; parameter number of the key.
;;;
(defmacro do-pv-slots ((key param-index index table) &body body)
  (let ((snl (gensym)))
    `(loop with ,index of-type pv-index = 0
	   for ,param-index of-type param-index from 0
	   for ,snl in (pv-table-slot-name-lists ,table) do
	     (loop for ,key in (cdr ,snl) do
		     (locally ,@body)
		     (incf ,index)))))

;;;
;;; Execute BODY for all calls of table, binding INDEX to pv
;;; indices of the calls.
;;;
(defmacro do-pv-calls ((call index table) &body body)
  `(loop for ,index of-type pv-index from 0
	 for ,call in (pv-table-call-list ,table) do
	   (locally ,@body)))

;;;
;;; Return a PV-TABLE for the given SLOT-NAME-LISTS and CALL-LIST.
;;; If such a table already exists, return that.  Otherwise make
;;; a new one.
;;;
(defun intern-pv-table (&key slot-name-lists call-list)
  (let ((new-p nil))
    (flet (;;
	   ;; Map calls or slot name list X to a canonical form.
	   (make-key (x)
	     (or (gethash x *calls+snls->canonical*)
		 (setf (gethash x *calls+snls->canonical*)
		       (copy-list x))))
	   ;;
	   ;; Lookup pv-table for calls + slots in X.  Add a new
	   ;; entry and create a new pv-table if not found.
	   (lookup (x)
	     (or (gethash x *calls+snls->pv-table*)
		 (setf (gethash x *calls+snls->pv-table*)
		       (let ((snl (copy-list (cdr x)))
			     (cl (car x)))
			 (setq new-p t)
			 (make-pv-table :slot-name-lists snl
					:call-list cl))))))
    (let ((table (lookup (mapcar #'make-key
				 (cons call-list slot-name-lists)))))
      ;;
      ;; When a new pv table has been created, record which keys
      ;; (slot-names, (reader <gf-name>), etc.) it uses and which
      ;; permutation vectors indices these keys are associated with.
      (when new-p
	(let ((last-index -1))
	  (do-pv-slots (key param index table)
	    (setq last-index index)
	    (note-pv-table-reference key table))
	  (setf (pv-table-pv-size table) (1+ last-index)))
	(do-pv-calls (call index table)
	  (note-pv-table-reference `(call ,(car call)) table)))
      ;;
      table))))

;;;
;;; Record that REF is referenced from PV-TABLE.  REF can be the name
;;; of a slot, (READER <gf-name>), (WRITER <gf-name>), or (CALL
;;; <gf-name>).
;;;
(defun note-pv-table-reference (ref pv-table)
  (push pv-table (gethash ref *pv-key->pv-tables*)))

;;;
;;; Execute BODY for all pv tables using KEY.  Bind TABLE to the pv
;;; table around BODY.
;;;
(defmacro do-pv-tables ((table key) &body body)
  `(loop for ,table in (gethash ,key *pv-key->pv-tables*) do
	   (locally ,@body)))




;;;
;;; Return the permutation vector value for SLOT-NAME in instances
;;; having WRAPPER layout.  CLASS is the class of the instance,
;;; CLASS-SLOTS is the list of class slots of CLASS.
;;;
;;; SLOT-NAME can be a symbol or a cons.  If it is a symbol, it names
;;; an instance or class slot.  If it is a cons (READER gf) or (WRITER
;;; gf), this signals that a call to the slot accessor generic
;;; function gf has been PV-optimized.
;;;
;;; First value is the permutation vector value for SLOT-NAME;
;;; a fixnum for an optimized instance slot, a cons for an optimized
;;; class slot, or NIL for "not optimized".
;;;
;;; Second value is T for an optimized class slot.
;;;
(defun compute-pv-slot (key wrapper class class-slots)
  (etypecase key
    (symbol
     (when (optimize-slot-value-by-class-p class key 'all)
       (or (instance-slot-index wrapper key)
	   (assq key class-slots))))
    (cons
     (ecase (first key)
       ((reader writer)
	(when (eq *boot-state* 'complete)
	  (let ((gf (gdefinition (second key))))
	    (when (generic-function-p gf)
	      (accessor-values1 gf (first key) class)))))
       (inline-access
	nil)))))

;;;
;;; Return a permutation vector for SLOT-NAME-LISTS given a list of
;;; wrappers for actual method arguments.
;;;
(defun compute-pv (snls wrappers)
  (loop with wrappers = (if (listp wrappers) wrappers (list wrappers))
	for snl in snls
	when snl nconc
	  (loop with wrapper = (pop wrappers)
		with stdp = (typep wrapper 'wrapper)
		with class = (wrapper-class* wrapper)
		with class-slots = (and stdp (wrapper-class-slots wrapper))
		for sn in (cdr snl) collect
		  (when stdp
		    (compute-pv-slot sn wrapper class class-slots)))
	into pv-values
	finally
	  (return (make-permutation-vector pv-values))))

(defun make-permutation-vector (contents)
  (make-array (length contents) :initial-contents contents))

;;;
;;; Look up wrappers PV-WRAPPERS in PV-TABLE.  Value is a pv cell
;;; (PV . CALLS), with PV being the permutation vector associated
;;; with the set of wrappers PV-WRAPPERS.
;;;
;;; If PV-TABLE doen't have a cache, make a new one.  If PV-WRAPPERS
;;; are not in the cache, compute a permutation vector and fill the
;;; cache.
;;;
(defun pv-table-lookup (table wrappers)
  (let* ((cache (pv-table-cache table))
	 (pv-cell (and cache (probe-cache cache wrappers nil))))
    (unless pv-cell
      (let* ((slot-name-lists (pv-table-slot-name-lists table))
	     (call-list (pv-table-call-list table))
	     (cache (or cache
			(setf (pv-table-cache table)
			      (get-cache (- (length slot-name-lists)
					    (count nil slot-name-lists))
					 t 2))))
	     (pv (compute-pv slot-name-lists wrappers))
	     (calls (compute-calls table call-list wrappers))
	     (cell (cons pv calls))
	     (new-cache (fill-cache cache wrappers cell)))
	(setf (pv-table-cache table) new-cache)
	(setq pv-cell cell)))
    pv-cell))


;;; ***************************************
;;; Updating PV-TABLEs for Changes  *******
;;; ***************************************

(defvar *pv-table-cache-update-info* ())

;;;
;;; Record that slot definition SLOTD has been changed.  This pushes
;;; (CLASS . PV-KEY) onto *PV-TABLE-CACHE-UPDATE-INFO*, where CLASS is
;;; the class to which SLOTD belongs, and PV-KEY is either a slot
;;; name, a symbol, or (READER <gf-name>), or (WRITER <gf-name>).
;;; *PV-TABLE-CACHE-UPDATE-INFO* is later used by
;;; UPDATE-PV-TABLE-CACHE-INFO to do the actual updating.
;;;
(defun record-pv-update-info (slotd)
  (let ((class (slot-definition-class slotd)))
    (flet ((push-accessor-info (type accessors)
	     (loop for gf-name in accessors
		   as key = (if (and (consp gf-name) (eq 'setf (car gf-name)))
				(list 'writer (cadr gf-name))
				(list type gf-name))
		   when (gethash key *pv-key->pv-tables*) do
		     (push (cons class key) *pv-table-cache-update-info*))))
      (push (cons class (slot-definition-name slotd))
	    *pv-table-cache-update-info*)
      (push-accessor-info 'reader (slot-definition-readers slotd))
      (push-accessor-info 'writer (slot-definition-writers slotd)))))

;;;
;;; Update PV-TABLE caches for class CLASS.  Called by
;;; SHARED-INITIALIZE :AFTER (STRUCTURE-CLASS T)) and by UPDATE-SLOTS.
;;;
(defun update-pv-table-cache-info (class)
    (loop for entry in *pv-table-cache-update-info*
	  with keys = ()
	  as (entry-class . key) = entry
	  if (eq class entry-class) do
	    (pushnew key keys :test #'equal)
	  else
	    collect entry into remaining-entries
	  finally
	    (when keys
	      (setq *pv-table-cache-update-info* remaining-entries)
	      (update-all-pv-table-caches class keys))))

(defun update-all-pv-table-caches (class slot-names)
  (let* ((cwrapper (class-wrapper class))
	 ;; Note that CLASS can be structure-class here.
	 (class-slots (when (typep cwrapper 'wrapper)
			(wrapper-class-slots cwrapper)))
	 (new-locations
	  (when (typep cwrapper 'wrapper)
	    (loop for sn in slot-names collect
	       (cons sn (compute-pv-slot sn cwrapper class class-slots)))))
	 (pv-tables ()))
    (flet ((new-location (key)
	     (cdr (assoc key new-locations :test #'equal))))
    ;;
    ;; Collect all PV-TABLES referencing a slot name from SLOT-NAMES
    ;; in PV-TABLES.
    (loop for sn in slot-names do
	    (do-pv-tables (table sn)
	      (pushnew table pv-tables)))
    ;;
    ;; Update all these PV-TABLEs.
    (loop for table in pv-tables
	  as cache = (pv-table-cache table)
	  when cache do
	    (let* ((pv-size (pv-table-pv-size table))
		   (pv-map (make-array pv-size :initial-element nil)))
	      (do-pv-slots (key param-index pv-index table)
		(let ((location (new-location key)))
		  (setf (svref pv-map pv-index)
			(and location (cons param-index location)))))
	      (map-cache (lambda (wrappers pv-cell)
			   (update-slots-in-pv wrappers (car pv-cell)
					       cwrapper pv-map))
			 cache))))))

;;;
;;; WRAPPERS - wrappers of a cache line.
;;; CWRAPPER - the wrapper of the class being updated.
;;; PV - the car of a PV cell, that is the permutation vector.
;;; PV-SIZE - the size of PV
;;; PV-MAP - a vector of size PV-SIZE.  NIL slots means nothing to update.
;;; Otherwise (PARAM-INDEX . NEW-VALUE).
;;;
(defun update-slots-in-pv (wrappers pv cwrapper pv-map)
  (if (atom wrappers)
      (when (eq wrappers cwrapper)
	(loop for new across pv-map and i from 0
	      when new do
		(setf (svref pv i) (cdr new))))
      (when (memq cwrapper wrappers)
	(loop for param from 0 and wrapper in wrappers
	      when (eq wrapper cwrapper) do
		(loop for new across pv-map and i from 0
		      when (and new (= (car new) param)) do
		      (setf (svref pv i) (cdr new)))))))

(declaim (inline count-gf-required-parameters))

(defun count-gf-required-parameters (gf)
  (let ((arg-info (gf-arg-info gf)))
    (length (arg-info-metatypes arg-info))))

(defun update-accessor-pvs (reason gf &optional method replaced-method)
  (declare (ignore method replaced-method))
  (let ((nreq (count-gf-required-parameters gf)))
    (when (and (<= 1 nreq 2)
	       (some #'standard-accessor-method-p
		     (generic-function-methods gf)))
      (let* ((gf-name (generic-function-name gf))
	     (key (if (= nreq 1) `(reader ,gf-name) `(writer ,gf-name))))
	(do-pv-tables (table key)
	  (when (pv-table-cache table)
	    (map-cache
	     (lambda (wrappers pv-cell)
	       (setf (car pv-cell)
		     (recompute-pv-for-key key (car pv-cell)
					   (pv-table-slot-name-lists table)
					   wrappers
					   (eq reason 'removed-gf))))
	     (pv-table-cache table))))))))

;;;
;;; Like COMPUTE-PV, but only recomputes entries for KEY.  If no
;;; pv values change, OLD-PV is returned unchanged.
;;;
(defun recompute-pv-for-key (key pv snls wrappers gf-removed-p)
  (loop with pv-index = 0
	with wrappers = (if (listp wrappers) wrappers (list wrappers))
	for snl in snls
	when snl do
	  (loop with wrapper = (pop wrappers)
		with stdp = (typep wrapper 'wrapper)
		and class = (wrapper-class* wrapper)
		with class-slots = (when stdp (wrapper-class-slots wrapper))
		for sn in (cdr snl)
		when (equal sn key) do
		  (setf (svref pv pv-index)
			(and stdp
			     (not gf-removed-p)
			     (compute-pv-slot sn wrapper class class-slots)))
		do (incf pv-index))
	finally
	  (return pv)))



;;; **************************************************
;;; Deciding Whether to Optimize Slot Access  ********
;;; **************************************************


;;; These forms get munged by MUTATE-SLOTS.

(define-walker-template pv-offset)
(defmacro pv-offset (arg) arg)

;;;
;;; Sometimes useful for debugging.
;;;
(defmacro pvref (pv i)
  `(%svref ,pv ,i))

(defmacro instance-read-internal (pv slot-vector pv-offset-form default
				  &optional type)
  (assert (member type '(nil :instance :class)))
  (let* ((index (gensym))
	 (value (gensym)))
    `(locally
	 (declare #.*optimize-speed*)
       (let* ((,index (pvref ,pv ,pv-offset-form))
	      (,value (typecase ,index
			,@(when (or (null type) (eq type :instance))
			    `((fixnum
			       (%slot-ref ,slot-vector ,index))))
			,@(when (or (null type) (eq type :class))
			    `((cons
			       (cdr ,index))))
			(t +slot-unbound+))))
	 (if (eq ,value +slot-unbound+)
	     ,default
	     ,value)))))

(defmacro instance-write-internal (pv slots pv-offset new-value default
				      &optional type)
  (assert (member type '(nil :instance :class)))
  (let* ((index (gensym)))
    `(locally
	 (declare #.*optimize-speed*)
       (let ((,index (pvref ,pv ,pv-offset)))
	 (typecase ,index
	   ,@(when (or (null type) (eq type :instance))
	       `((fixnum (setf (%slot-ref ,slots ,index) ,new-value))))
	   ,@(when (or (null type) (eq type :class))
	       `((cons (setf (cdr ,index) ,new-value))))
	   (t ,default))))))

(defmacro instance-boundp-internal (pv slots pv-offset default
				       &optional type)
  (assert (member type '(nil :instance :class)))
  (let* ((index (gensym)))
    `(locally
	 (declare #.*optimize-speed*)
       (let ((,index (pvref ,pv ,pv-offset)))
	 (typecase ,index
	   ,@(when (or (null type) (eq type :instance))
	       `((fixnum (not (eq (%slot-ref ,slots ,index) +slot-unbound+)))))
	   ,@(when (or (null type) (eq type :class))
	       `((cons (not (eq (cdr ,index) +slot-unbound+)))))
	   (t ,default))))))

;;;
;;; Build ``slot name lists'' from the collected information about
;;; INSTANCE-READ and INSTANCE-WRITE optimizations.
;;;
;;; SLOTS is an alist of lists
;;;
;;; (PARAMETER-NAME ((SLOT-NAME PV-OFFSET-FORMs ...)
;;;                  (SLOT-NAME PV-OFFSET-FORMs ...)
;;;                 ...))
;;;
;;; where each PARAMETER-NAME is the name of a required parameter for
;;; which a slot access via slot-value, set-slot-value, slot-boundp
;;; has been optimized into a PV access.  The SLOT-NAMES are the names
;;; of the slots for which the optimization has been performed.
;;; The PV-OFFSET-FORMs are lists (PV-OFFSET <dummy>) that are contained
;;; in calls to INSTANCE-READ and INSTANCE-WRITE.
;;;
;;; CALLS is currently unused, and always NIL.
;;;
;;; First value is a slot name list of the form
;;;
;;;  ((NIL SLOT-NAME SLOT-NAME ...)
;;;   (NIL SLOT-NAME SLOT-NAME ...)
;;;   ...)
;;;
;;; Second value is nil, since generic function call optimization is
;;; currently not done, and CALLS is always NIL.
;;;
;;; From the code, it seems that CALLS was intended to be a list of
;;; lists of the form
;;;
;;; ((FN ARG1 ARG2 ...) PV-OFFSET-FORMs ...)
;;;
;;; where FN is a symbol or cons for the generic function name, and
;;; each ARG is either an integer specifying the position of a required
;;; parameter, or something else (NIL?).
;;;
;;; The first element of a slot-name list for parameter number P is set
;;; from NIL to T if that parameter appears in an optimized generic
;;; function call.
;;;
(defun slot-name-lists-from-slots (slots calls)
  (multiple-value-bind (slots calls)
      (mutate-slots-and-calls slots calls)
    (let* ((slot-name-lists
	    (mapcar (lambda (parameter-entry)
		      (cons nil (mapcar #'car (cdr parameter-entry))))
		    slots))
	   (call-list
	    (mapcar #'car calls)))
      ;;
      ;; For every parameter used by a gf optimization, mark the
      ;; parameter's slot entry by setting its car to T.  This is
      ;; done to prevent the code below from setting an otherwise
      ;; unused parameter entry in the slot name list to NIL, which
      ;; would mean that that parameter would be considered as not
      ;; participating in pv optimizations.
      (loop for (gf-name . args) in call-list do
	      (loop for arg in args do
		      (setf (car (nth arg slot-name-lists)) t)))
      ;;
      ;; Replace (NIL . NIL) with NIL in the slot name lists.  The NIL
      ;; indicates that a parameter is not used in pv optimizations
      ;; which means we don't need to retrieve its wrapper and slots
      ;; etc. at the start of a method function.
      (setq slot-name-lists
	    (loop for snl in slot-name-lists
		  if (or (car snl) (cdr snl))
		    collect snl
		  else
		    collect nil))
      ;;
      ;; Convert "absolute" parameter numbers in calls to "relative"
      ;; parameter indices, excluding parameters not used for
      ;; pv-optimization (the index of wrappers in the pv table
      ;; cache).
      (setq call-list
	    (loop with index-vector =
		    (coerce (loop with i = -1
				  for snl in slot-name-lists
				  collect (when snl (incf i)))
			    'simple-vector)
		  for (gf-name . args) in call-list collect
		    (cons gf-name
			  (mapcar (lambda (arg) (svref index-vector arg))
				  args))))
      ;;
      (values slot-name-lists call-list))))

;;;
;;; Assign actual PV indices to slots and patch these into
;;; recorded PV-OFFSET forms in SLOTS and CALLS.
;;;
(defun mutate-slots-and-calls (slots calls)
  (let ((sorted-slots (sort-slots slots))
	(sorted-calls (sort-calls (cdr calls)))
	(pv-offset -1))
    ;;
    ;; Patch PV indices into PV-OFFSET forms recorded in SLOTS.
    (dolist (parameter-entry sorted-slots)
      (dolist (slot-entry (cdr parameter-entry))
	(incf pv-offset)	
	(dolist (pv-offset-form (cdr slot-entry))
	  (setf (cadr pv-offset-form) pv-offset))))
    ;;
    ;; Patch PV indices into PV-OFFSET forms recorded in CALLS.
    (loop for i from 0 and call in sorted-calls do
	    (loop for form in (cdr call) do
		    (setf (cadr form) i)))
    ;;
    (values sorted-slots sorted-calls)))

(defun symbol-pkg-name (sym) 
  (let ((pkg (symbol-package sym)))
    (if pkg (package-name pkg) "")))

(defun symbol-lessp (a b)
  (if (eq (symbol-package a) (symbol-package b))
      (string-lessp (symbol-name a) (symbol-name b))
      (string-lessp (symbol-pkg-name a) (symbol-pkg-name b))))

(defun symbol-or-cons-lessp (a b)
  (etypecase a
    (symbol
     (etypecase b
       (symbol (symbol-lessp a b))
       (cons t)))
    (integer
     (< a b))
    (cons
     (etypecase b
       (symbol nil)
       (cons (if (eq (car a) (car b))
		 (symbol-or-cons-lessp (cdr a) (cdr b))
		 (symbol-or-cons-lessp (car a) (car b))))))))

(defun sort-slots (slots)
  (mapcar (lambda (parameter-entry)
	    (cons (car parameter-entry)
		  (sort (cdr parameter-entry) ;slot entries
			#'symbol-or-cons-lessp
			:key #'car)))
	  slots))

(defun sort-calls (calls)
  (sort calls #'symbol-or-cons-lessp :key #'car))


;;;
;;; Giving method lambdas access to PV tables.
;;;

;;; PV-BINDING is wrapped around a method lambda body BODY in
;;; MAKE-METHOD-LAMBDA-INTERNAL when optimizations have been performed
;;; by WALK-METHOD-LAMBDA on the method lambda that require access to
;;; PV tables.
;;;
;;; REQUIRED-PARAMETERS is a list of the names of required parameters
;;; of the method.
;;;
;;; SLOT-NAME-LISTS is a list of the form
;;; 
;;; ((PARAM1 NIL SLOT-NAME SLOT-NAME ...)
;;;  (PARAM2 NIL SLOT-NAME SLOT-NAME ...))
;;;
;;; PV-TABLE-SYMBOL is an uninterned symbol whose symbol-value will be
;;; set to the methods pv-table when the method is invoked.
;;;
;;; The resulting form looks like
;;;
;;; (let* ((.pv-table. PV-TABLE-SYMBOL)
;;;	   (.pv-cell. (pv-table-lookup-pv-args .pv-table. PARAM0 PARAM1 ...))
;;;	   (.pv. (car .pv-cell.))
;;;	   (.calls. (cdr .pv-cell.)))
;;;  (declare (type simple-vector .pv.))
;;;  (declare (type simple-vector .calls.))
;;;  (declare (special PV-TABLE-SYMBOL))
;;;  (declare (ignorable .pv. .calls.))
;;;  (let ((.slots0. (get-slots-or-nil PARAM0))
;;;        (.slots1. (get-slots-or-nil PARAM1))
;;;        ...)
;;;    <the method function body>))
;;;
;;; The .SLOTS*. variables are bound to the instance slot value vectors
;;; of the required parameters appearing in SLOT-NAME-LISTS.
;;;

(defun needs-pv-p (snl)
  (loop for entry in (cdr snl)
	thereis (not (equal '(inline-access) entry))))

(defmacro pv-binding ((required-parameters slot-name-lists pv-table-symbol)
		      &body body)
  (loop for snl in slot-name-lists
	and param in required-parameters and i from 0
	when snl
	  collect param into params
	  and collect (and (cdr snl) (slot-vector-symbol i)) into slot-vars
	finally
	  (return `(pv-binding1 (.pv. .calls. ,pv-table-symbol
				      ,params ,slot-vars)
		     ,@body))))

(defmacro pv-binding1 ((pv calls pv-table-symbol pv-parameters slot-vars) 
		       &body body)
  (multiple-value-bind (bindings vars)
    (loop for v in slot-vars and p in pv-parameters
	  when v
	    collect `(,v (slot-vector-or-nil ,p)) into bindings
	    and collect v into vars
	  finally
	    (return (values bindings vars)))
    `(pv-env (,pv ,calls ,pv-table-symbol ,pv-parameters)
	(let (,@bindings)
	  ,@(when vars `((declare (ignorable ,@vars))))
	  ,@body))))

(defmacro pv-env ((pv calls pv-table-symbol pv-parameters) &rest forms)
  `(let* ((.pv-table. ,pv-table-symbol)
	  (.pv-cell. (pv-table-lookup-pv-args .pv-table. ,@pv-parameters))
	  (,pv (car .pv-cell.))
	  (,calls (cdr .pv-cell.)))
     (declare (type list .pv-cell.))
     ,@(when (symbolp pv-table-symbol)
	 `((declare (special ,pv-table-symbol))))
     (declare (ignorable ,pv ,calls))
     ,@forms))


(defun make-method-initargs-form-internal (method-lambda initargs env)
  (declare (ignore env))
  (let (method-lambda-args lmf lmf-params)
    (if (not (and (= 3 (length method-lambda))
		  (= 2 (length (setq method-lambda-args (cadr method-lambda))))
		  (consp (setq lmf (third method-lambda)))
		  (eq 'simple-lexical-method-functions (car lmf))
		  (eq (car method-lambda-args) (cadr (setq lmf-params (cadr lmf))))
		  (eq (cadr method-lambda-args) (caddr lmf-params))))
	`(list* :function #',method-lambda
	        ',initargs)
	(let* ((lambda-list (car lmf-params))
	       (nreq 0)
	       (restp nil)
	       (args nil))
	  (dolist (arg lambda-list)
	    (when (member arg '(&optional &rest &key))
	      (setq restp t)
	      (return nil))
	    (when (eq arg '&aux)
	      (return nil))
	    (incf nreq)
	    (push arg args))
	  (setq args (nreverse args))
	  (setf (getf (getf initargs :plist) :arg-info) (cons nreq restp))
	  (make-method-initargs-form-internal1
	   initargs (cddr lmf) args lmf-params restp)))))

(defun make-method-initargs-form-internal1 
    (initargs body req-args lmf-params restp)
  (multiple-value-bind (outer-decls inner-decls body)
      (split-declarations
       body req-args (getf (cdr lmf-params) :call-next-method-p))
    (let* ((rest-arg (when restp '.rest-arg.))
	   (args+rest-arg (if restp (append req-args (list rest-arg)) req-args)))
      `(list* :fast-function
	(lambda (.pv-cell. .next-method-call. ,@args+rest-arg)
	  (declare (ignorable .pv-cell. .next-method-call.))
	  ,@outer-decls
	  (macrolet ((pv-env ((pv calls pv-table-symbol pv-parameters)
			      &rest forms)
		       (declare (ignore pv-table-symbol pv-parameters))
		       `(let ((,pv (car .pv-cell.))
			      (,calls (cdr .pv-cell.)))
			  (declare (type list .pv-cell.))
			  (declare (ignorable ,pv ,calls))
			  ,@forms)))
	    (fast-lexical-method-functions 
	     (,(car lmf-params) .next-method-call. ,req-args ,rest-arg
	       ,@(cdddr lmf-params))
	     ,@inner-decls
	     ,@body)))
	',initargs))))

;;;
;;; Use arrays and hash tables and the fngen stuff to make this much
;;; better.  It doesn't really matter, though, because a function
;;; returned by this will get called only when the user explicitly
;;; funcalls a result of method-function.  BUT, this is needed to make
;;; early methods work.
;;;
(defun method-function-from-fast-function (fmf)
  (declare (type function fmf))
  (let* ((method-function nil) (pv-table nil)
	 (arg-info (method-function-get fmf :arg-info))
	 (nreq (car arg-info))
	 (restp (cdr arg-info)))
    (setq method-function
	  (lambda (method-args next-methods)
	    (unless pv-table
	      (setq pv-table (method-function-pv-table fmf)))
	    (let* ((pv-cell (when pv-table
			      (get-method-function-pv-cell 
			       method-function method-args pv-table)))
		   (nm (car next-methods))
		   (nms (cdr next-methods))
		   (nmc (when nm
			  (make-method-call :function (if (std-instance-p nm)
							  (method-function nm)
							  nm)
					    :call-method-args (list nms)))))
	      (if restp
		  (let* ((rest (nthcdr nreq method-args))
			 (args (ldiff method-args rest)))
		    (apply fmf pv-cell nmc (nconc args (list rest))))
		  (apply fmf pv-cell nmc method-args)))))
    (let* ((fname (method-function-get fmf :name))
	   (name (cons 'method (cdr fname))))
      (set-function-name method-function name))      
    (setf (method-function-get method-function :fast-function) fmf)
    method-function))

(defun get-method-function-pv-cell (method-function method-args
				    &optional pv-table)
  (let ((pv-table (or pv-table (method-function-pv-table method-function))))
    (when pv-table
      (let ((pv-wrappers (pv-wrappers-from-all-args pv-table method-args)))
	(when pv-wrappers
	  (pv-table-lookup pv-table pv-wrappers))))))

;;;
;;; Called to bind .PV-CELL. in a method body to the cell
;;; corresponding to the actual arguments the method.  Only those
;;; arguments are considered for which PV optimizations have been
;;; performed.
;;;
;;; PV-TABLE is the PV table of the method.
;;;
;;; ARGS is a list of actual method arguments, only those
;;; pv-optimized.
;;;
(defun pv-table-lookup-pv-args (pv-table &rest args)
  (pv-table-lookup pv-table (pv-wrappers-from-pv-args args)))

(defun pv-wrappers-from-pv-args (&rest args)
  (loop for arg in args
	for wrapper = (wrapper-of arg)
	if (invalid-wrapper-p wrapper)
	  collect (check-wrapper-validity wrapper) into wrappers
	else
	  collect wrapper into wrappers
	finally (return (if (cdr wrappers) wrappers (car wrappers)))))

(defun pv-wrappers-from-all-args (pv-table args)
  (loop for snl in (pv-table-slot-name-lists pv-table) and arg in args
	when snl
	  collect (wrapper-of arg) into wrappers
	finally (return (if (cdr wrappers) wrappers (car wrappers)))))

;;;
;;; Return the subset of WRAPPERS which is used in the cache
;;; of PV-TABLE.
;;;
(defun pv-wrappers-from-all-wrappers (pv-table wrappers)
  (loop for snl in (pv-table-slot-name-lists pv-table) and w in wrappers
	when snl
	  collect w into result
	finally (return (if (cdr result) result (car result)))))

