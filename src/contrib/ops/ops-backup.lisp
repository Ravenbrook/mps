;
;************************************************************************
;
;	VPS2 -- Interpreter for OPS5
;
;
;
; This Common Lisp version of OPS5 is in the public domain.  It is based
; in part on based on a Franz Lisp implementation done by Charles L. Forgy
; at Carnegie-Mellon University, which was placed in the public domain by
; the author in accordance with CMU policies.  This version has been
; modified by George Wood, Dario Giuse, Skef Wholey, Michael Parzen,
; and Dan Kuokka.
;
; This code is made available is, and without warranty of any kind by the
; authors or by Carnegie-Mellon University.
;

;;;; Definitions and functions for backing up.

(in-package "OPS")


;;; Internal Global Variables

(defvar *refracts*)
(defvar *record*)
(defvar *record-array*)
(defvar *recording*)
(defvar *max-record-index*)
(defvar *record-index*)



(defun backup-init ()
  (setq *recording* nil)
  (setq *refracts* nil)
  (setq *record-array* (make-array 256 :initial-element ()))  ;jgk
  (initialize-record))


(defun back (k)
  (prog (r)
    loop   (and (< k 1.) (return nil))
    (setq r (getvector *record-array* *record-index*))	; (('))
    (and (null r) (return '|nothing more stored|))
    (putvector *record-array* *record-index* nil)
    (record-index-plus -1.)
    (undo-record r)
    (setq k (1- k))
    (go loop)))


; *max-record-index* holds the maximum legal index for record-array
; so it and the following must be changed at the same time

(defun begin-record (p data)
  (setq *recording* t)
  (setq *record* (list '=>refract p data))) 

(defun end-record nil
  (cond (*recording*
	 (setq *record*
	       (cons *cycle-count* (cons *p-name* *record*)))
	 (record-index-plus 1.)
	 (putvector *record-array* *record-index* *record*)
	 (setq *record* nil)
	 (setq *recording* nil)))) 

(defun record-change (direct time elm)
  (cond (*recording*
	 (setq *record*
	       (cons direct (cons time (cons elm *record*))))))) 

; to maintain refraction information, need keep only one piece of information:
; need to record all unsuccessful attempts to delete things from the conflict
; set.  unsuccessful deletes are caused by attempting to delete refracted
; instantiations.  when backing up, have to avoid putting things back into the
; conflict set if they were not deleted when running forward

(defun record-refract (rule data)
  (and *recording*
       (setq *record* (cons '<=refract (cons rule (cons data *record*))))))

(defun refracted (rule data)
  (prog (z)
    (and (null *refracts*) (return nil))
    (setq z (cons rule data))
    (return (member z *refracts* :test #'equal))))


(defun record-index-plus (k)
  (setq *record-index* (+ k *record-index*))	;"plus" changed to "+" by gdw
  (cond ((< *record-index* 0.)
	 (setq *record-index* *max-record-index*))
	((> *record-index* *max-record-index*)
	 (setq *record-index* 0.)))) 

; the following routine initializes the record.  putting nil in the
; first slot indicates that that the record does not go back further
; than that.  (when the system backs up, it writes nil over the used
; records so that it will recognize which records it has used.  thus
; the system is set up anyway never to back over a nil.)

(defun initialize-record nil
  (setq *record-index* 0.)
  (setq *recording* nil)
  (setq *max-record-index* 31.)
  (putvector *record-array* 0. nil)) 


;; replaced per jcp
;;; Commented out
#|
(defun undo-record (r)
  (prog (save act a b rate)
    ;###	(comment *recording* must be off during back up)
    (setq save *recording*)
    (setq *refracts* nil)
    (setq *recording* nil)
    (and *ptrace* (back-print (list '|undo:| (car r) (cadr r))))
    (setq r (cddr r))
    top  (and (atom r) (go fin))
    (setq act (car r))
    (setq a (cadr r))
    (setq b (caddr r))
    (setq r (cdddr r))
    (and *wtrace* (back-print (list '|undo:| act a)))
    (cond ((eq act '<=wm) (add-to-wm b a))
	  ((eq act '=>wm) (remove-from-wm b))
	  ((eq act '<=refract)
	   (setq *refracts* (cons (cons a b) *refracts*)))
	  ((and (eq act '=>refract) (still-present b))
	   (setq *refracts* (delete (cons a b) *refracts*))
	   (setq rate (rating-part (get a 'topnode)))
	   (removecs a b)
	   (insertcs a b rate))
	  (t (%warn '|back: cannot undo action| (list act a))))
    (go top)
    fin  (setq *recording* save)
    (setq *refracts* nil)
    (return nil)))
;;; End commented out
|#


(defun undo-record (r)
  (prog (save act a b rate)
    ;###	(comment *recording* must be off during back up)
    (setq save *recording*)
    (setq *refracts* nil)
    (setq *recording* nil)
    (and *ptrace* (back-print (list '|undo:| (car r) (cadr r))))
    (setq r (cddr r))
    top  (and (atom r) (go fin))
    (setq act (car r))
    (setq a (cadr r))
    (setq b (caddr r))
    (setq r (cdddr r))
    (and *wtrace* (back-print (list '|undo:| act a)))
    (cond ((eq act '<=wm) (add-to-wm b a))
	  ((eq act '=>wm) (remove-from-wm b))
	  ((eq act '<=refract)
	   (setq *refracts* (cons (cons a b) *refracts*)))
	  ((and (eq act '=>refract) (still-present b))
	   (setq *refracts* (spdelete (cons a b) *refracts*))
	   (setq rate (rating-part (get a 'topnode)))
	   (removecs a b)
	   (insertcs a b rate))
	  (t (%warn '|back: cannot undo action| (list act a))))
    (go top)
    fin  (setq *recording* save)
    (setq *refracts* nil)
    (return nil))) 



; still-present makes sure that the user has not deleted something
; from wm which occurs in the instantiation about to be restored; it
; makes the check by determining whether each wme still has a time tag.

(defun still-present (data)
  (prog nil
    loop
    (cond ((atom data) (return t))
	  ((creation-time (car data))
	   (setq data (cdr data))
	   (go loop))
	  (t (return nil))))) 


(defun back-print (x) 
  (prog (port)
    (setq port (trace-file))
    (terpri port)
    (print x port)))
