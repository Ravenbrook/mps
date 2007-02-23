;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/hppa/static-fn.lisp,v 1.3 2003/08/03 11:27:48 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and macro magic necessary to call static
;;; functions.
;;;
;;; Written by William Lott.
;;;
(in-package "HPPA")


(define-vop (static-function-template)
  (:save-p t)
  (:policy :safe)
  (:variant-vars symbol)
  (:vop-var vop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:sc descriptor-reg :offset lra-offset) lra)
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) old-fp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save))


(eval-when (compile load eval)

(defun static-function-template-name (num-args num-results)
  (intern (format nil "~:@(~R-arg-~R-result-static-function~)"
		  num-args num-results)))


(defun moves (src dst)
  (collect ((moves))
    (do ((src src (cdr src))
	 (dst dst (cdr dst)))
	((or (null src) (null dst)))
      (moves `(move ,(car src) ,(car dst))))
    (moves)))

(defun static-function-template-vop (num-args num-results)
  (assert (and (<= num-args register-arg-count)
	       (<= num-results register-arg-count))
	  (num-args num-results)
	  "Either too many args (~D) or too many results (~D).  Max = ~D"
	  num-args num-results register-arg-count)
  (let ((num-temps (max num-args num-results)))
    (collect ((temp-names) (temps) (arg-names) (args) (result-names) (results))
      (dotimes (i num-results)
	(let ((result-name (intern (format nil "RESULT-~D" i))))
	  (result-names result-name)
	  (results `(,result-name :scs (any-reg descriptor-reg)))))
      (dotimes (i num-temps)
	(let ((temp-name (intern (format nil "TEMP-~D" i))))
	  (temp-names temp-name)
	  (temps `(:temporary (:sc descriptor-reg
			       :offset ,(nth i register-arg-offsets)
			       ,@(when (< i num-args)
				   `(:from (:argument ,i)))
			       ,@(when (< i num-results)
				   `(:to (:result ,i)
				     :target ,(nth i (result-names)))))
			      ,temp-name))))
      (dotimes (i num-args)
	(let ((arg-name (intern (format nil "ARG-~D" i))))
	  (arg-names arg-name)
	  (args `(,arg-name
		  :scs (any-reg descriptor-reg)
		  :target ,(nth i (temp-names))))))
      `(define-vop (,(static-function-template-name num-args num-results)
		    static-function-template)
	 (:args ,@(args))
	 ,@(temps)
	 (:results ,@(results))
	 (:generator ,(+ 50 num-args num-results)
	   (let ((lra-label (gen-label))
		 (cur-nfp (current-nfp-tn vop)))
	     ,@(moves (arg-names) (temp-names))
	     (inst li (fixnumize ,num-args) nargs)
	     (inst ldw (static-function-offset symbol) null-tn lip)
	     (when cur-nfp
	       (store-stack-tn nfp-save cur-nfp))
	     (inst move cfp-tn old-fp)
	     (inst compute-lra-from-code code-tn lra-label temp lra)
	     (note-this-location vop :call-site)
	     (inst bv lip)
	     (inst move csp-tn cfp-tn)
	     (emit-return-pc lra-label)
	     ,(collect ((bindings) (links))
		(do ((temp (temp-names) (cdr temp))
		     (name 'values (gensym))
		     (prev nil name)
		     (i 0 (1+ i)))
		    ((= i num-results))
		  (bindings `(,name
			      (make-tn-ref ,(car temp) nil)))
		  (when prev
		    (links `(setf (tn-ref-across ,prev) ,name))))
		`(let ,(bindings)
		   ,@(links)
		   (default-unknown-values vop
		       ,(if (zerop num-results) nil 'values)
		       ,num-results move-temp temp lra-label)))
	     (when cur-nfp
	       (load-stack-tn cur-nfp nfp-save))
	     ,@(moves (temp-names) (result-names))))))))

) ; eval-when (compile load eval)

(macrolet
    ((foo ()
       (collect ((templates (list 'progn)))
	 (dotimes (i register-arg-count)
	   (templates (static-function-template-vop i 1)))
	 (templates))))
  (foo))

(defmacro define-static-function (name args &key (results '(x)) translate
				       policy cost arg-types result-types)
  `(define-vop (,name
		,(static-function-template-name (length args)
						(length results)))
     (:variant ',name)
     (:note ,(format nil "static-function ~@(~S~)" name))
     ,@(when translate
	 `((:translate ,translate)))
     ,@(when policy
	 `((:policy ,policy)))
     ,@(when cost
	 `((:generator-cost ,cost)))
     ,@(when arg-types
	 `((:arg-types ,@arg-types)))
     ,@(when result-types
	 `((:result-types ,@result-types)))))
