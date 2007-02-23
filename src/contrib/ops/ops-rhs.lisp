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

;;;; This file contains all the functions necessary for RHS actions
;;;; including $actions.

(in-package "OPS")
(shadow '(remove write))
(export '(remove write make modify crlf))


(proclaim '(special *ptrace* *cycle-count* *halt-flag* *wtrace*))


;;; External global variables

(defvar *size-result-array*)
(defvar *in-rhs*)
(defvar *current-wm*)
(defvar *max-wm*)
(defvar *action-count*)
(defvar *critical*)


;;; Internal global variables

(defvar *wmpart-list*)
(defvar *wm-filter*)
(defvar *wm*)
(defvar *old-wm*)
(defvar *result-array*)
(defvar *variable-memory*)
(defvar *last*)
(defvar *max-index*)
(defvar *next-index*)
(defvar *data-matched*)
(defvar *ce-variable-memory*)
(defvar *rest*)
(defvar *build-trace*)


;;;; Functions for RHS evaluation

(defun rhs-init ()
  ; if the size of result-array changes, change the line in i-g-v which
  ; sets the value of *size-result-array*
  (setq *size-result-array* 255.)                             ;255 /256 set by gdw
  (setq *result-array* (make-array 256 :initial-element nil))  ;jgk
  (setq *in-rhs* nil)
  (setq *build-trace* nil)
  (setq *max-wm* (setq *current-wm* 0.))
  (setq *action-count* 0.)
  (setq *critical* nil)
  (setq *wmpart-list* nil))


(defun eval-rhs (pname data)
  (prog (node port)
    (cond (*ptrace*
	   (setq port (trace-file))
	   (terpri port)
	   (princ *cycle-count* port)
	   (princ '|. | port)
	   (princ pname port)
	   (time-tag-print data port)))
    (setq *data-matched* data)
    (setq *p-name* pname)
    (setq *last* nil)
    (setq node (get pname 'topnode))
    (init-var-mem (var-part node))
    (init-ce-var-mem (ce-var-part node))
    (begin-record pname data)
    (setq *in-rhs* t)
    (eval (rhs-part node))
    (setq *in-rhs* nil)
    (end-record))) 


(defun eval-args (z)
  (prog (r)
    (rhs-tab 1.)
    la   (and (atom z) (return nil))
    (setq r (car z))
    (setq z (cdr z))
    (cond ((EQ R '^)
	   (RHS-tab (car z))
	   (setq r (cadr z))
	   (setq z (cddr z))))
    (cond ((eq r '//) ($value (car z)) (setq z (cdr z)))
	  (t ($change r)))
    (go la))) 



;;;; RHS actions
;;;; Some of these can be called at the top level.

(defmacro make (&body z)
  `(ops-make ',z))

(defmacro remove (&body z)
  `(ops-remove ',z))

(defmacro modify (&body z)
  `(ops-modify ',z))

(defmacro openfile (&body z)
  `(ops-openfile ',z))

(defmacro closefile (&body z)
  `(ops-closefile ',z))

(defmacro default (&body z)
  `(ops-default ',z))

(defmacro write (&body z)
  `(ops-write ',z))

(defmacro crlf (&body z)
  `(ops-crlf ',z))

(defmacro tabto (&body z)
  `(ops-tabto ',z))

(defmacro rjust (&body z)
  `(ops-rjust ',z))

(defmacro call (&body z)
  `(ops-call ',z))

(defmacro bind (&body z)
  `(ops-bind ',z))

(defmacro cbind (&body z)
  `(ops-cbind ',z))

(defmacro build (&body z)
  `(ops-build ',z))

(defmacro substr (&body l)
  `(ops-substr ',l))

(defmacro compute (&body z)
  `(ops-compute ',z))

(defmacro litval (&body z)
  `(ops-litval ',z))

(defmacro accept (&body z)
  `(ops-accept ',z))

(defmacro acceptline (&body z)
  `(ops-acceptline ',z))

(defmacro arith (&body z)
  `(ops-arith ',z))


(defun ops-make (z)
  (prog nil
    ($reset)
    (eval-args z)
    ($assert))) 

(defun ops-remove (z)
  (prog (old)
    (and (not *in-rhs*)(return (top-level-remove z)))
    top  (and (atom z) (return nil))
    (setq old (get-ce-var-bind (car z)))
    (cond ((null old)
	   (%warn '|remove: argument not an element variable| (car z))
	   (return nil)))
    (remove-from-wm old)
    (setq z (cdr z))
    (go top))) 

(defun ops-modify (z)
  (prog (old)
    (cond ((not *in-rhs*)
	   (%warn '|cannot be called at top level| 'modify)
	   (return nil)))
    (setq old (get-ce-var-bind (car z)))
    (cond ((null old)
	   (%warn '|modify: first argument must be an element variable|
		  (car z))
	   (return nil)))
    (remove-from-wm old)
    (setq z (cdr z))
    ($reset)
    copy (and (atom old) (go fin))
    ($change (car old))
    (setq old (cdr old))
    (go copy)
    fin  (eval-args z)
    ($assert))) 

(defun ops-bind (z)
  (prog (val)
    (cond ((not *in-rhs*)
	   (%warn '|cannot be called at top level| 'bind)
	   (return nil)))
    (cond ((< (length z) 1.)
	   (%warn '|bind: wrong number of arguments to| z)
	   (return nil))
	  ((not (symbolp (car z)))
	   (%warn '|bind: illegal argument| (car z))
	   (return nil))
	  ((= (length z) 1.) (setq val (gensym)))
	  (t ($reset)
	     (eval-args (cdr z))
	     (setq val ($parameter 1.))))
    (make-var-bind (car z) val))) 

(defun ops-cbind (z)
  (cond ((not *in-rhs*)
	 (%warn '|cannot be called at top level| 'cbind))
	((not (= (length z) 1.))
	 (%warn '|cbind: wrong number of arguments| z))
	((not (symbolp (car z)))
	 (%warn '|cbind: illegal argument| (car z)))
	((null *last*)
	 (%warn '|cbind: nothing added yet| (car z)))
	(t (make-ce-var-bind (car z) *last*)))) 


(defun ops-call (z)
  (prog (f)
    (setq f (car z))
    ($reset)
    (eval-args (cdr z))
    (funcall f))) 


(defun halt nil 
  (cond ((not *in-rhs*)
	 (%warn '|cannot be called at top level| 'halt))
	(t (setq *halt-flag* t)))) 

(defun ops-build (z)
  (prog (r)
    (cond ((not *in-rhs*)
	   (%warn '|cannot be called at top level| 'build)
	   (return nil)))
    ($reset)
    (build-collect z)
    (setq r (unflat (use-result-array)))
    (and *build-trace* (funcall *build-trace* r))
    (compile-production (car r) (cdr r)))) 

(defun ops-compute (z) ($value (ari z))) 

; arith is the obsolete form of compute
(defun ops-arith (z) ($value (ari z))) 

(defun ari (x)
  (cond ((atom x)
	 (%warn '|bad syntax in arithmetic expression | x)
	 0.)
	((atom (cdr x)) (ari-unit (car x)))
	((eq (cadr x) '+)
	 (+ (ari-unit (car x)) (ari (cddr x))))
	;"plus" changed to "+" by gdw
	((eq (cadr x) '-)
	 (- (ari-unit (car x)) (ari (cddr x))))
	((eq (cadr x) '*)
	 (* (ari-unit (car x)) (ari (cddr x))))
	((eq (cadr x) '//)
	 (floor (ari-unit (car x)) (ari (cddr x))))   ;@@@ quotient? /
	;@@@ kluge only works for integers
	;@@@ changed to floor by jcp (from round)
	((eq (cadr x) '\\)
	 (mod (fix (ari-unit (car x))) (fix (ari (cddr x)))))
	(t (%warn '|bad syntax in arithmetic expression | x) 0.))) 

(defun ari-unit (a)
  (prog (r)
    (cond ((consp  a) (setq r (ari a)))	;dtpr\consp gdw
	  (t (setq r ($varbind a))))
    (cond ((not (numberp r))
	   (%warn '|bad value in arithmetic expression| a)
	   (return 0.))
	  (t (return r))))) 

(defun ops-substr (l)
  (prog (k elm start end)
    (cond ((not (= (length l) 3.))
	   (%warn '|substr: wrong number of arguments| l)
	   (return nil)))
    (setq elm (get-ce-var-bind (car l)))
    (cond ((null elm)
	   (%warn '|first argument to substr must be a ce var|
		  l)
	   (return nil)))
    (setq start ($varbind (cadr l)))
    (setq start ($litbind start))
    (cond ((not (numberp start))
	   (%warn '|second argument to substr must be a number|
		  l)
	   (return nil)))
;###	(comment |if a variable is bound to INF, the following|
;	 |will get the binding and treat it as INF is|
;	 |always treated.  that may not be good|)
    (setq end ($varbind (caddr l)))
    (cond ((eq end 'inf) (setq end (length elm))))
    (setq end ($litbind end))
    (cond ((not (numberp end))
	   (%warn '|third argument to substr must be a number|
		  l)
	   (return nil)))
;###	(comment |this loop does not check for the end of elm|
;         |instead it relies on cdr of nil being nil|
;         |this may not work in all versions of lisp|)
    (setq k 1.)
    la   (cond ((> k end) (return nil))
	       ((not (< k start)) ($value (car elm))))
    (setq elm (cdr elm))
    (setq k (1+ k))
    (go la))) 

(defun genatom nil ($value (gensym))) 

(defun ops-litval (z)
  (prog (r)
    (cond ((not (= (length z) 1.))
	   (%warn '|litval: wrong number of arguments| z)
	   ($value 0) 
	   (return nil))
	  ((numberp (car z)) ($value (car z)) (return nil)))
    (setq r ($litbind ($varbind (car z))))
    (cond ((numberp r) ($value r) (return nil)))
    (%warn '|litval: argument has no literal binding| (car z))
    ($value 0)))



; rhs-tab implements the tab ('^') function in the rhs.  it has
; four responsibilities:
;	- to move the array pointers
;	- to watch for tabbing off the left end of the array
;	  (ie, to watch for pointers less than 1)
;	- to watch for tabbing off the right end of the array
;	- to write nil in all the slots that are skipped
; the last is necessary if the result array is not to be cleared
; after each use; if rhs-tab did not do this, $reset
; would be much slower.

(defun rhs-tab (z) ($tab ($varbind z)))


(defun time-tag-print (data port)
  (cond ((not (null data))
	 (time-tag-print (cdr data) port)
	 (princ '| | port)
	 (princ (creation-time (car data)) port))))

(defun init-var-mem (vlist)
  (prog (v ind r)
    (setq *variable-memory* nil)
    top  (and (atom vlist) (return nil))
    (setq v (car vlist))
    (setq ind (cadr vlist))
    (setq vlist (cddr vlist))
    (setq r (gelm *data-matched* ind))
    (setq *variable-memory* (cons (cons v r) *variable-memory*))
    (go top))) 

(defun init-ce-var-mem (vlist)
  (prog (v ind r)
    (setq *ce-variable-memory* nil)
    top  (and (atom vlist) (return nil))
    (setq v (car vlist))
    (setq ind (cadr vlist))
    (setq vlist (cddr vlist))
    (setq r (ce-gelm *data-matched* ind))
    (setq *ce-variable-memory*
	  (cons (cons v r) *ce-variable-memory*))
    (go top))) 

(defun make-ce-var-bind (var elem)
  (setq *ce-variable-memory*
	(cons (cons var elem) *ce-variable-memory*))) 

(defun make-var-bind (var elem)
  (setq *variable-memory* (cons (cons var elem) *variable-memory*))) 

(defun get-ce-var-bind (x)
  (prog (r)
    (cond ((numberp x) (return (get-num-ce x))))
    (setq r (assq x *ce-variable-memory*))
    (cond (r (return (cdr r)))
	  (t (return nil))))) 

(defun get-num-ce (x)
  (prog (r l d)
    (setq r *data-matched*)
    (setq l (length r))
    (setq d (- l x))
    (and (> 0. d) (return nil))
    la   (cond ((null r) (return nil))
	       ((> 1. d) (return (car r))))
    (setq d (1- d))
    (setq r (cdr r))
    (go la))) 


(defun build-collect (z)
  (prog (r)
    la   (and (atom z) (return nil))
    (setq r (car z))
    (setq z (cdr z))
    (cond ((consp  r)	;dtpr\consp gdw
	   ($value '\()
		   (build-collect r)
		   ($value '\)))
	  ((eq r '\\) ($change (car z)) (setq z (cdr z)))
	  (t ($value r)))
    (go la))) 

(defun unflat (x) (setq *rest* x) (unflat*)) 

(defun unflat* nil
  (prog (c)
    (cond ((atom *rest*) (return nil)))
    (setq c (car *rest*))
    (setq *rest* (cdr *rest*))
    (cond ((eq c '\() (return (cons (unflat*) (unflat*))))
	   ((eq c '\)) (return nil))
	  (t (return (cons c (unflat*))))))) 



;;;; $Functions.
;;;; These functions provide an interface to the result array.
;;;; The result array is used to organize attribute values into their
;;;; correct slot.

(defun $litbind (x)
  (prog (r)
    (cond ((and (symbolp x) (setq r (literal-binding-of x)))
	   (return r))
	  (t (return x))))) 

(defun $varbind (x)
  (prog (r)
    (and (not *in-rhs*) (return x))
    (setq r (assq x *variable-memory*))
    (cond (r (return (cdr r)))
	  (t (return x))))) 

(defun $change (x)
  (prog nil
    (cond ((consp  x) (eval-function x))	;dtpr\consp gdw
	  (t ($value ($varbind x)))))) 

(defun $reset nil
  (setq *max-index* 0.)
  (setq *next-index* 1.)) 

(defun $tab (z)
  (prog (edge next)
    (setq next ($litbind z))
    (and (floatp next) (setq next (fix next)))
    (cond ((or (not (numberp next)) 
	       (> next *size-result-array*)
	       (> 1. next))				; ( '| |)
	   (%warn '|illegal index after ^| next)
	   (return *next-index*)))
    (setq edge (- next 1.))
    (cond ((> *max-index* edge) (go ok)))
    clear (cond ((== *max-index* edge) (go ok)))
    (putvector *result-array* edge nil)
    (setq edge (1- edge))
    (go clear)
    ok   (setq *next-index* next)
    (return next))) 

(defun $value (v)
  (cond ((> *next-index* *size-result-array*)
	 (%warn '|index too large| *next-index*))
	(t
	 (and (> *next-index* *max-index*)
	      (setq *max-index* *next-index*))
	 (putvector *result-array* *next-index* v)
	 (setq *next-index* (1+ *next-index*))))) 

(defun $assert nil
  (setq *last* (use-result-array))
  (add-to-wm *last* nil))

(defun $parametercount nil *max-index*)

(defun $parameter (k)
  (cond ((or (not (numberp k)) (> k *size-result-array*) (< k 1.))
	 (%warn '|illegal parameter number | k)
	 nil)
	((> k *max-index*) nil)
	(t (getvector *result-array* k))))

(defun $ifile (x) 
  (cond ((symbolp x) (get x 'inputfile))
	(t nil)))

(defun $ofile (x) 
  (cond ((symbolp x) (get x 'outputfile))
	(t nil)))


;;; Use-result-array returns the contents of the result array as a list.

(defun use-result-array nil
  (prog (k r)
    (setq k *max-index*)
    (setq r nil)
    top  (and (== k 0.) (return r))
    (setq r (cons (getvector *result-array* k) r))
    (setq k (1- k))
    (go top))) 


(defun eval-function (form)
  (cond ((not *in-rhs*)
	 (%warn '|functions cannot be used at top level| (car form)))
	(t (eval form))))



;;;; WM maintaining functions

;;; The order of operations in the following two functions is critical.
;;; add-to-wm order: (1) change wm (2) record change (3) match 
;;; remove-from-wm order: (1) record change (2) match (3) change wm
;;; (back will not restore state properly unless wm changes are recorded
;;; before the cs changes that they cause)  (match will give errors if 
;;; the thing matched is not in wm at the time)

(defun add-to-wm (wme override)
  (prog (fa z part timetag port)
    (setq *critical* t)
    (setq *current-wm* (1+ *current-wm*))
    (and (> *current-wm* *max-wm*) (setq *max-wm* *current-wm*))
    (setq *action-count* (1+ *action-count*))
    (setq fa (wm-hash wme))
    (or (member fa *wmpart-list*)
	(setq *wmpart-list* (cons fa *wmpart-list*)))
    (setq part (get fa 'wmpart*))
    (cond (override (setq timetag override))
	  (t (setq timetag *action-count*)))
    (setq z (cons wme timetag))
    (putprop fa (cons z part) 'wmpart*)
    (record-change '=>wm *action-count* wme)
    (match 'new wme)
    (setq *critical* nil)
    (cond ((and *in-rhs* *wtrace*)
	   (setq port (trace-file))
	   (terpri port)
	   (princ '|=>wm: | port)
	   (ppelm wme port))))) 

;;; remove-from-wm uses eq, not equal to determine if wme is present

(defun remove-from-wm (wme)
  (prog (fa z part timetag port)
    (setq fa (wm-hash wme))
    (setq part (get fa 'wmpart*))
    (setq z (assq wme part))
    (or z (return nil))
    (setq timetag (cdr z))
    (cond ((and *wtrace* *in-rhs*)
	   (setq port (trace-file))
	   (terpri port)
	   (princ '|<=wm: | port)
	   (ppelm wme port)))
    (setq *action-count* (1+ *action-count*))
    (setq *critical* t)
    (setq *current-wm* (1- *current-wm*))
    (record-change '<=wm timetag wme)
    (match nil wme)
    (putprop fa (delq z part) 'wmpart*)
    (setq *critical* nil))) 

;;; mapwm maps down the elements of wm, applying fn to each element
;;; each element is of form (datum . creation-time)

(defun mapwm (fn)
  (prog (wmpl part)
    (setq wmpl *wmpart-list*)
    lab1 (cond ((atom wmpl) (return nil)))
    (setq part (get (car wmpl) 'wmpart*))
    (setq wmpl (cdr wmpl))
    (mapc fn part)
    (go lab1))) 

(defun ops-wm (a) 
  (mapc (function (lambda (z) (terpri) (ppelm z *standard-output*))) 
	(get-wm a))
  nil) 

(defun creation-time (wme)
  (cdr (assq wme (get (wm-hash wme) 'wmpart*)))) 


(defun get-wm (z)
  (setq *wm-filter* z)
  (setq *wm* nil)
  (mapwm (function get-wm2))
  (prog2 nil *wm* (setq *wm* nil))) 

(defun get-wm2 (elem) 
  ; (cond ((or (null *wm-filter*) (member (cdr elem) *wm-filter*) :test #'equal)))
  (cond ((or (null *wm-filter*) (member (cdr elem) *wm-filter*)) ;test #'equal)
	(setq *wm* (cons (car elem) *wm*)))))

(defun wm-hash (x)
  (cond ((not x) '<default>)
	((not (car x)) (wm-hash (cdr x)))
	((symbolp (car x)) (car x))
	(t (wm-hash (cdr x))))) 

(defun refresh nil
  (prog nil
    (setq *old-wm* nil)
    (mapwm (function refresh-collect))
    (mapc (function refresh-del) *old-wm*)
    (mapc (function refresh-add) *old-wm*)
    (setq *old-wm* nil))) 

(defun refresh-collect (x) (setq *old-wm* (cons x *old-wm*))) 

(defun refresh-del (x) (remove-from-wm (car x))) 

(defun refresh-add (x) (add-to-wm (car x) (cdr x))) 
