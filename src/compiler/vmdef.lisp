;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/vmdef.lisp,v 1.49 1994/10/31 04:27:28 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains implementation-independent facilities used for
;;; defining the compiler's interface to the VM in a given implementation.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package :c)

(export '(template-or-lose sc-or-lose sb-or-lose sc-number-or-lose
	  meta-sc-or-lose meta-sb-or-lose meta-sc-number-or-lose
	  primitive-type-or-lose note-this-location note-next-instruction))

;;; Template-Or-Lose  --  Internal
;;;
;;;    Return the template having the specified name, or die trying.
;;;
(defun template-or-lose (x &optional (backend *target-backend*))
  (the template
       (or (gethash x (backend-template-names backend))
	   (error "~S is not a defined template." x))))


;;; SC-Or-Lose, SB-Or-Lose, SC-Number-Or-Lose  --  Internal
;;;
;;;    Return the SC structure, SB structure or SC number corresponding to a
;;; name, or die trying.
;;;
(defun sc-or-lose (x &optional (backend *target-backend*))
  (the sc
       (or (gethash x (backend-sc-names backend))
	   (error "~S is not a defined storage class." x))))
;;;
(defun sb-or-lose (x &optional (backend *target-backend*))
  (the sb
       (or (gethash x (backend-sb-names backend))
	   (error "~S is not a defined storage base." x))))
;;;
(defun sc-number-or-lose (x &optional (backend *target-backend*))
  (the sc-number (sc-number (sc-or-lose x backend))))


;;; META-SC-OR-LOSE, META-SB-OR-LOSE, META-SC-NUMBER-OR-LOSE  --  Internal
;;;
;;;    Like the non-meta versions, but go for the meta-compile-time info.
;;; These should not be used after load time, since compiling the compiler
;;; changes the definitions.
;;;
(defun meta-sc-or-lose (x)
  (the sc
       (or (gethash x (backend-meta-sc-names *target-backend*))
	   (error "~S is not a defined storage class." x))))
;;;
(defun meta-sb-or-lose (x)
  (the sb
       (or (gethash x (backend-meta-sb-names *target-backend*))
	   (error "~S is not a defined storage base." x))))
;;;
(defun meta-sc-number-or-lose (x)
  (the sc-number (sc-number (meta-sc-or-lose x))))


;;;; Side-Effect Classes

(def-boolean-attribute vop
  any)



;;;; Move/coerce definition:

;;; COMPUTE-MOVE-COSTS  --  Internal
;;;
;;; Compute at compiler load time the costs for moving between all SCs that
;;; can be loaded from FROM-SC and to TO-SC given a base move cost Cost.
;;;
(defun compute-move-costs (from-sc to-sc cost)
  (declare (type sc from-sc to-sc) (type index cost))
  (let ((to-scn (sc-number to-sc))
	(from-costs (sc-load-costs from-sc)))
    (dolist (dest-sc (cons to-sc (sc-alternate-scs to-sc)))
      (let ((vec (sc-move-costs dest-sc))
	    (dest-costs (sc-load-costs dest-sc)))
	(setf (svref vec (sc-number from-sc)) cost)
	(dolist (sc (append (sc-alternate-scs from-sc)
			    (sc-constant-scs from-sc)))
	  (let* ((scn (sc-number sc))
		 (total (+ (svref from-costs scn)
			   (svref dest-costs to-scn)
			   cost))
		 (old (svref vec scn)))
	    (unless (and old (< old total))
	      (setf (svref vec scn) total))))))))


;;;; Primitive type definition:

;;; PRIMITIVE-TYPE-OR-LOSE  --  Interface
;;;
;;;    Return the primitive type corresponding to the specified name, or die
;;; trying.
;;;
(defun primitive-type-or-lose (name &optional (backend *target-backend*))
  (the primitive-type
       (or (gethash name (backend-primitive-type-names backend))
	   (error "~S is not a defined primitive type." name))))


;;; SC-ALLOWED-BY-PRIMITIVE-TYPE  --  Interface
;;;
;;;    Return true if SC is either one of Ptype's SC's, or one of those SC's
;;; alternate or constant SCs.
;;;
(defun sc-allowed-by-primitive-type (sc ptype)
  (declare (type sc sc) (type primitive-type ptype))
  (let ((scn (sc-number sc)))
    (dolist (allowed (primitive-type-scs ptype) nil)
      (when (eql allowed scn)
	(return t))
      (let ((allowed-sc (svref (backend-sc-numbers *backend*) allowed)))
	(when (or (member sc (sc-alternate-scs allowed-sc))
		  (member sc (sc-constant-scs allowed-sc)))
	  (return t))))))


;;;; Emit function generation:

(defconstant max-vop-tn-refs 256)

(defvar *vop-tn-refs* (make-array max-vop-tn-refs :initial-element nil))
(defvar *using-vop-tn-refs* nil)

(defun flush-vop-tn-refs ()
  (unless *using-vop-tn-refs*
    (fill *vop-tn-refs* nil)))

(pushnew 'flush-vop-tn-refs *before-gc-hooks*)

(defconstant sc-bits (integer-length (1- sc-number-limit)))

(defun emit-generic-vop (node block template args results &optional info)
  (%emit-generic-vop node block template args results info))

(defun %emit-generic-vop (node block template args results info)
  (let* ((vop (make-vop block node template args results))
	 (num-args (vop-info-num-args template))
	 (last-arg (1- num-args))
	 (num-results (vop-info-num-results template))
	 (num-operands (+ num-args num-results))
	 (last-result (1- num-operands))
	 (ref-ordering (vop-info-ref-ordering template)))
    (declare (type vop vop)
	     (type (integer 0 #.max-vop-tn-refs)
		   num-args num-results num-operands)
	     (type (integer -1 #.(1- max-vop-tn-refs)) last-arg last-result)
	     (type (simple-array (mod #.max-vop-tn-refs) (*)) ref-ordering))
    (setf (vop-codegen-info vop) info)
    (let ((refs *vop-tn-refs*)
	  (*using-vop-tn-refs* t))
      (declare (type (simple-vector #.max-vop-tn-refs) refs))
      (do ((index 0 (1+ index))
	   (ref args (and ref (tn-ref-across ref))))
	  ((= index num-args))
	(setf (svref refs index) ref))
      (do ((index num-args (1+ index))
	   (ref results (and ref (tn-ref-across ref))))
	  ((= index num-operands))
	(setf (svref refs index) ref))
      (let ((temps (vop-info-temps template)))
	(when temps
	  (let ((index num-operands)
		(prev nil))
	    (dotimes (i (length temps))
	      (let* ((temp (aref temps i))
		     (tn (if (logbitp 0 temp)
			     (make-wired-tn nil
					    (ldb (byte sc-bits 1) temp)
					    (ash temp (- (1+ sc-bits))))
			     (make-restricted-tn nil (ash temp -1))))
		     (write-ref (reference-tn tn t)))
		(setf (aref refs index) (reference-tn tn nil))
		(setf (aref refs (1+ index)) write-ref)
		(if prev
		    (setf (tn-ref-across prev) write-ref)
		    (setf (vop-temps vop) write-ref))
		(setf prev write-ref)
		(incf index 2))))))
      (let ((prev nil))
	(flet ((add-ref (ref)
		 (setf (tn-ref-vop ref) vop)
		 (setf (tn-ref-next-ref ref) prev)
		 (setf prev ref)))
	  (declare (inline add-ref))
	  (dotimes (i (length ref-ordering))
	    (let* ((index (aref ref-ordering i))
		   (ref (aref refs index)))
	      (if (or (= index last-arg) (= index last-result))
		  (do ((ref ref (tn-ref-across ref)))
		      ((null ref))
		    (add-ref ref))
		  (add-ref ref)))))
	(setf (vop-refs vop) prev))
      (let ((targets (vop-info-targets template)))
	(when targets
	  (dotimes (i (length targets))
	    (let ((target (aref targets i)))
	      (target-if-desirable (aref refs (ldb (byte 8 8) target))
				   (aref refs (ldb (byte 8 0) target))))))))
    (values vop vop)))


;;;; Function translation stuff:

;;; Adjoin-Template  --  Internal
;;;
;;;    Add Template into List, removing any old template with the same name.
;;; We also maintain the increasing cost ordering.
;;;
(defun adjoin-template (template list)
  (declare (type template template) (list list))
  (sort (cons template
	      (remove (template-name template) list
		      :key #'template-name))
	#'<=
	:key #'template-cost))



;;; Template-Type-Specifier  --  Internal
;;;
;;;    Return a function type specifier describing Template's type computed
;;; from the operand type restrictions.
;;;
(defun template-type-specifier (template)
  (declare (type template template))
  (flet ((convert (types more-types)
	   (flet ((frob (x)
		    (if (eq x '*)
			't
			(ecase (first x)
			  (:or `(or ,@(mapcar #'(lambda (type)
						  (type-specifier
						   (primitive-type-type
						    type)))
					      (rest x))))
			  (:constant `(constant-argument ,(third x)))))))
	     `(,@(mapcar #'frob types)
	       ,@(when more-types
		   `(&rest ,(frob more-types)))))))
    (let* ((args (convert (template-arg-types template)
			  (template-more-args-type template)))
	   (result-restr (template-result-types template))
	   (results (if (eq result-restr :conditional)
			'(boolean)
			(convert result-restr
				 (cond ((template-more-results-type template))
				       ((/= (length result-restr) 1) '*)
				       (t nil))))))
      `(function ,args
		 ,(if (= (length results) 1)
		      (first results)
		      `(values ,@results))))))


;;;; Random utilities.

;;; NOTE-THIS-LOCATION  --  Interface
;;;
(defun note-this-location (vop kind)
  "NOTE-THIS-LOCATION VOP Kind
  Note that the current code location is an interesting (to the debugger)
  location of the specified Kind.  VOP is the VOP responsible for this code.
  This VOP must specify some non-null :SAVE-P value (perhaps :COMPUTE-ONLY) so
  that the live set is computed."
  (let ((lab (gen-label)))
    (emit-label lab)
    (note-debug-location vop lab kind)))

;;; NOTE-NEXT-INSTRUCTION -- interface.
;;; 
(defun note-next-instruction (vop kind)
  "NOTE-NEXT-INSTRUCTION VOP Kind
   Similar to NOTE-THIS-LOCATION, except the use the location of the next
   instruction for the code location, wherever the scheduler decided to put
   it."
  (let ((loc (note-debug-location vop nil kind)))
    (new-assem:emit-postit #'(lambda (segment posn)
			       (declare (ignore segment))
			       (setf (location-info-label loc) posn))))
  (undefined-value))
