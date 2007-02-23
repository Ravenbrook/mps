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

;;;; This file contains utility definitions that are needed by other ops
;;;; modules.  This must be loaded first so commonlisp systems that
;;;; expand macros early have them available.

(unless (find-package "OPS") (make-package "OPS"))

(in-package "OPS")

;;; Assq is included in some Common Lisp implementations (like Spice Lisp and
;;; the Zetalisp CLCP) as an extension.  We'll use ASSOC if it's not there.
;;; DK- turned assq into a function so it can be 'applied'

(eval-when (compile load eval)
  (unless (fboundp 'assq)
    (defmacro assq (i l)
      `(assoc ,i ,l))))

;;; Ditto for DELQ.

(eval-when (compile load eval)
  (unless (fboundp 'delq)
    (defmacro delq (i l)
      `(delete ,i ,l :test #'eq))))

;
; Spdelete "special delete" is a function which deletes every occurence
; of element from list. This function was defined because common lisp's
; delete function only deletes top level elements from a list, not lists
; from lists. 
;
(defun spdelete (element list)
  
  (cond ((null list) nil)
	((equal element (car list)) (spdelete element (cdr list)))
	(t (cons (car list) (spdelete element (cdr list))))))


;;; Functions that were revised so that they would compile efficiently

(eval-when (compile eval load)
	   
;* The function == is machine dependent!
;* This function compares small integers for equality.  It uses EQ
;* so that it will be fast, and it will consequently not work on all
;* Lisps.  It works in Franz Lisp for integers in [-128, 127]
;(system::macro == (z) `(eq ,(cadr z) ,(caddr z)))
;;;
;;; Dario Giuse - made a macro. This is going to be faster than anything else.
;;;
;;; Skef Wholey - The = function in Common Lisp will compile into good code
;;; (in all implementations that I know of) when given the right declarations.
;;; In this case, we know both numbers are fixnums, so we use that information.

(defmacro == (x y)
  `(= (the fixnum ,x) (the fixnum ,y)))

;;; =ALG returns T if A and B are algebraically equal.
;;; This corresponds to equalp - Dario Giuse
;;; But equalp uses eql for comparison if the things are numbers - Skef Wholey
;;;
(defmacro =alg (a b)
  `(eql ,a ,b))

	   
(defmacro fast-symeval (&body z)
  `(symbol-value ,(car z)))

; getvector and putvector are fast routines for using ONE-DIMENSIONAL
; arrays.  these routines do no checking; they assume
;	1. the array is a vector with 0 being the index of the first
;	   element
;	2. the vector holds arbitrary list values

; Example call: (putvector array index value)
;;; Dario Giuse - 6/20/84

(defmacro putvector (array index value)
  `(setf (aref ,array ,index) ,value))

;;; Example call: (getvector name index)
;;;
(defmacro getvector (array index)
  `(aref ,array ,index))


;;; Dario Giuse  6/21/84
(defmacro putprop (atom value property)
  `(setf (get ,atom ,property) ,value))

) ;eval-when


(defun ce-gelm (x k)
  (declare (fixnum k))
  (declare (optimize (speed 3) (safety 0)))
  (prog nil
    loop (and (== k 1.) (return (car x)))
    (setq k (1- k))
    (setq x (cdr x))
    (go loop))) 

(defconstant encode-pair-shift 14)

; The loops in gelm were unwound so that fewer calls on DIFFERENCE
; would be needed

(defun gelm (x k)
  (declare (optimize speed (safety 0)) (fixnum k))
  (prog ((ce (ash k (- encode-pair-shift)))
	 (sub (ldb (byte 14 0) k)))
    (declare (fixnum ce sub))
    celoop (and (eql ce 0.) (go ph2))
    (setq x (cdr x))
    (and (eql ce 1.) (go ph2))
    (setq x (cdr x))
    (and (eql ce 2.) (go ph2))
    (setq x (cdr x))
    (and (eql ce 3.) (go ph2))
    (setq x (cdr x))
    (and (eql ce 4.) (go ph2))
    (setq ce (- ce 4.))
    (go celoop)
    ph2  (setq x (car x))
    subloop (and (eql sub 0.) (go finis))
    (setq x (cdr x))
    (and (eql sub 1.) (go finis))
    (setq x (cdr x))
    (and (eql sub 2.) (go finis))
    (setq x (cdr x))
    (and (eql sub 3.) (go finis))
    (setq x (cdr x))
    (and (eql sub 4.) (go finis))
    (setq x (cdr x))
    (and (eql sub 5.) (go finis))
    (setq x (cdr x))
    (and (eql sub 6.) (go finis))
    (setq x (cdr x))
    (and (eql sub 7.) (go finis))
    (setq x (cdr x))
    (and (eql sub 8.) (go finis))
    (setq sub (- sub 8.))
    (go subloop)
    finis (return (car x))) ) ;  )  	;end prog,< locally >, defun



;;; intersect two lists using eq for the equality test
(defun interq (x y)
  (cond ((atom x) nil)
	((member (car x) y) (cons (car x) (interq (cdr x) y)))
	(t (interq (cdr x) y)))) 


(proclaim '(special *p-name*))

(defun %warn (what where)
  (prog nil
    (terpri)
    (princ '?)
    (and *p-name* (princ *p-name*))
    (princ '|..|)
    (princ where)
    (princ '|..|)
    (princ what)
    (return where))) 

(defun %error (what where)
  (%warn what where)
  (throw '!error! '!error!)) 	;jgk quoted arguments

;@@@(defun round (x) (fix (+ 0.5 x))) 		;"plus" changed to "+" by gdw
;@@@ removed; calls converted to native clisp (round)

(defun top-levels-eq (la lb)
  (prog nil
    lx   (cond ((eq la lb) (return t))
	       ((null la) (return nil))
	       ((null lb) (return nil))
	       ((not (eq (car la) (car lb))) (return nil)))
    (setq la (cdr la))
    (setq lb (cdr lb))
    (go lx))) 


;(defun dtpr  (x) (consp x))	;dtpr\consp gdw


(defun fix (x)(floor x))


(eval-when (compile load eval)
(defmacro ncons (x) `(cons ,x nil))
);eval-when


;@@@ revision suggested by sf/inc. by gdw
(defun variablep (x)
  (and (symbolp x)
       (let ((name (symbol-name x)))
	 (and (>= (length name) 1)
	      (char= (char name 0) #\<)))))



;@@@   this is a mistake: it must either go before = is called for 
;non-numeric args, or such calls replaced with eq, equal, etc.
;(defun = 
;(x y) (equal x y))



#|
Commented out - Dario Giuse.
This is unnecessary in Spice Lisp

; break mechanism:
(proclaim '(special erm *break-character*))

(defun setbreak nil (setq *break-flag* t))
(setq *break-character* #\control-D)
(bind-keyboard-function *break-character* #'setbreak)
(princ "*** use control-d for ops break, or setq *break-character asciival***")

|#
