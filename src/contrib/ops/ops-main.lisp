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

;;;; This file contains the top-level functions, function to literalize
;;;; and access attributes, and functions to manage the conflict set.


(in-package "OPS")

(export '(literalize p vector-attribute strategy watch))


;;; Global variables also used by other modules.

(defvar *halt-flag*)
(defvar *cycle-count*)
(defvar *p-name*)
(defvar *ptrace*)
(defvar *wtrace*)


;;; Global variables used in this module only.

(defvar *limit-token*)
(defvar *total-wm*)
(defvar *max-token*)
(defvar *total-token*)
(defvar *brkpts*)
(defvar *phase*)
(defvar *break-flag*)
(defvar *remaining-cycles*)
(defvar *conflict-set*)
(defvar *max-cs*)
(defvar *total-cs*)
(defvar *limit-cs*)
(defvar *strategy*)
(defvar *class-list*)
(defvar *buckets*)



(defun main-init ()
  (setq *cycle-count* 0.)
  (setq *p-name* nil)
  (setq *ptrace* t)
  (setq *wtrace* nil)
  (setq *limit-token* 1000000.)
  (setq *limit-cs* 1000000.)
  (setq *total-wm* 0.)
  (setq *total-token* (setq *max-token* 0.))
  (setq *max-cs* (setq *total-cs* 0.))
  (setq *conflict-set* nil)
  (setq *strategy* 'lex)
  (setq *buckets* 127.)		; regular OPS5 allows 64 named slots
  (setq *class-list* nil)
  (setq *brkpts* nil)
  (setq *remaining-cycles* 1000000))



;;;; Top level commands.


(defmacro run (&body z)
  `(ops-run ',z))

(defmacro ppwm (&body avlist)
  `(ops-ppwm ',avlist))

(defmacro wm (&body a) 
  `(ops-wm ',a))

(defmacro pm (&body z)
  `(ops-pm ',z))

(defmacro cs (&body z)
  `(ops-cs ',z))

(defmacro matches (&body rule-list)
  `(ops-matches ',rule-list))

(defmacro strategy (&body z)
  `(ops-strategy ',z))

(defmacro watch (&body z)
  `(ops-watch ',z))

(defmacro pbreak (&body z)
  `(ops-pbreak ',z))

(defmacro excise (&body z)
  `(ops-excise ',z))

(defmacro p (&body z) 
  `(ops-p ',z))

(defmacro external (&body z) 
  `(ops-external ',z))

(defmacro literal (&body z)
  `(ops-literal ',z))

(defmacro literalize (&body z)
  `(ops-literalize ',z))

(defmacro vector-attribute (&body l)
  `(ops-vector-attribute ',l))

(defun top-level-remove (z)
  (cond ((equal z '(*)) (process-changes nil (get-wm nil)))
	(t (process-changes nil (get-wm z))))) 



;;; Functions for run command

(defun ops-run (z)
  (cond ((atom z) (setq *remaining-cycles* 1000000.) (do-continue nil))
	((and (atom (cdr z)) (numberp (car z)) (> (car z) 0.))
	 (setq *remaining-cycles* (car z))
	 (do-continue nil))
	(t 'what?))) 


(defun do-continue (wmi)
  (cond (*critical*
	 (terpri)
	 (princ '|warning: network may be inconsistent|)))
  (process-changes wmi nil)
  (print-times (main))) 


(defun process-changes (adds dels)
  (prog (x)
    process-deletes (and (atom dels) (go process-adds))
    (setq x (car dels))
    (setq dels (cdr dels))
    (remove-from-wm x)
    (go process-deletes)
    process-adds (and (atom adds) (return nil))
    (setq x (car adds))
    (setq adds (cdr adds))
    (add-to-wm x nil)
    (go process-adds))) 


(defun main nil
  (prog (instance r)
    (setq *halt-flag* nil)
    (setq *break-flag* nil)
    (setq instance nil)
    dil  (setq *phase* 'conflict-resolution)
    (cond (*halt-flag*
	   (setq r '|end -- explicit halt|)
	   (go finis))
	  ((zerop *remaining-cycles*)
	   (setq r '***break***)
	   (setq *break-flag* t)
	   (go finis))
	  (*break-flag* (setq r '***break***) (go finis)))
    (setq *remaining-cycles* (1- *remaining-cycles*))
    (setq instance (conflict-resolution))
    (cond ((not instance)
	   (setq r '|end -- no production true|)
	   (go finis)))
    (setq *phase* (car instance))
    (accum-stats)
    (eval-rhs (car instance) (cdr instance))
    (check-limits)
    (and (broken (car instance)) (setq *break-flag* t))
    (go dil)
    finis (setq *p-name* nil)
    (return r))) 


(defun broken (rule) (member rule *brkpts*))


(defun accum-stats nil
  (setq *cycle-count* (1+ *cycle-count*))
  (setq *total-token* (+ *total-token* *current-token*))
  ;"plus" changed to "+" by gdw
  (cond ((> *current-token* *max-token*)
	 (setq *max-token* *current-token*)))
  (setq *total-wm* (+ *total-wm* *current-wm*))	;"plus" changed to "+" by gdw
  (cond ((> *current-wm* *max-wm*) (setq *max-wm* *current-wm*)))) 


(defun check-limits nil
  (cond ((> (length *conflict-set*) *limit-cs*)
	 (format t "~%~%conflict set size exceeded the limit of ~D after ~D~%"
		 *limit-cs* *p-name*)
	 (setq *halt-flag* t)))
  (cond ((> *current-token* *limit-token*)
	 (format t "~%~%token memory size exceeded the limit of ~D after ~D~%"
		 *limit-token* *p-name*)
	 (setq *halt-flag* t))))


(defun print-times (mess)
  (prog (cc)
    (cond (*break-flag* (terpri) (return mess)))
    (setq cc (+ (float *cycle-count*) 1.0e-20))
    (terpri)
    (princ mess)
    (terpri)
    (format t "~3D productions (~D // ~D nodes)~%"
	    *pcount* *real-cnt* *virtual-cnt*)
    (format t "~3D firings (~D rhs actions)~%"
	    *cycle-count* *action-count*)
    (format t "~3D mean working memory size (~D maximum)~%"
	    (round (float *total-wm*) cc) *max-wm*)
    (format t "~3D mean conflict set size (~D maximum)~%"
	    (round (float *total-cs*) cc) *max-cs*)
    (format t "~3D mean token memory size (~D maximum)~%"
	    (round (float *total-token*) cc)
	    *max-token*)))


;;; Functions for strategy command

(defun ops-strategy (z)
  (cond ((atom z) *strategy*)
	((equal z '(lex)) (setq *strategy* 'lex))
	((equal z '(mea)) (setq *strategy* 'mea))
	(t 'what?))) 


;;; Functions for watch command

(defun ops-watch (z)
  (cond ((equal z '(0.))
	 (setq *wtrace* nil)
	 (setq *ptrace* nil)
	 0.)
	((equal z '(1.)) (setq *wtrace* nil) (setq *ptrace* t) 1.)
	((equal z '(2.)) (setq *wtrace* t) (setq *ptrace* t) 2.)
	((equal z '(3.))
	 (setq *wtrace* t)
	 (setq *ptrace* t)
	 '(2. -- conflict set trace not supported))
	((and (atom z) (null *ptrace*)) 0.)
	((and (atom z) (null *wtrace*)) 1.)
	((atom z) 2.)
	(t 'what?))) 


;;; Functions for excise command

(defun ops-excise (z) (mapc (function excise-p) z))

(defun excise-p (name)
  (cond ((and (symbolp name) (get name 'topnode))
	 (format t "~S is excised~%" name)
	 (setq *pcount* (1- *pcount*))
	 (remove-from-conflict-set name)
	 (kill-node (get name 'topnode))
	 (remprop name 'production)
	 (remprop name 'backpointers)
	 (remprop name 'topnode)))) 

(defun kill-node (node)
  (prog nil
    top  (and (atom node) (return nil))
    (rplaca node '&old)
    (setq node (cdr node))
    (go top))) 


;;; Functions for external command

(defun ops-external (z) (catch '!error! (external2 z))) ;jgk inverted args
;& quoted tag
(defun external2 (z) (mapc (function external3) z))

(defun external3 (x) 
  (cond ((symbolp x) (putprop x t 'external-routine))
	(t (%error '|not a legal function name| x))))

;;; Functions for pbreak command

(defun ops-pbreak (z)
  (cond ((atom z) (terpri) *brkpts*)
	(t (mapc (function pbreak2) z) nil)))

(defun pbreak2 (rule)
  (cond ((not (symbolp rule)) (%warn '|illegal name| rule))
	((not (get rule 'topnode)) (%warn '|not a production| rule))
	((member rule *brkpts*) (setq *brkpts* (rematm rule *brkpts*)))
	(t (setq *brkpts* (cons rule *brkpts*)))))

(defun rematm (atm list)
  (cond ((atom list) list)
	((eq atm (car list)) (rematm atm (cdr list)))
	(t (cons (car list) (rematm atm (cdr list))))))


;;; Functions for matches command

(defun ops-matches (rule-list)
  (mapc (function matches2) rule-list)
  (terpri)) 


(defun matches2 (p)
  (cond ((atom p)
	 (terpri)
	 (terpri)
	 (princ p)
	 (matches3 (get p 'backpointers) 2. (ncons 1.))))) 


(defun matches3 (nodes ce part)
  (cond ((not (null nodes))
	 (terpri)
	 (princ '| ** matches for |)
	 (princ part)
	 (princ '| ** |)
	 (mapc (function write-elms) (find-left-mem (car nodes)))
	 (terpri)
	 (princ '| ** matches for |)
	 (princ (ncons ce))
	 (princ '| ** |)
	 (mapc (function write-elms) (find-right-mem (car nodes)))
	 (matches3 (cdr nodes) (1+ ce) (cons ce part))))) 


(defun write-elms (wme-or-count)
  (cond ((consp  wme-or-count)	;dtpr\consp gdw
	 (terpri)
	 (mapc (function write-elms2) wme-or-count)))) 


(defun write-elms2 (x)
  (princ '|  |)
  (princ (creation-time x)))


(defun find-left-mem (node)
  (cond ((eq (car node) '&and) (memory-part (caddr node)))
	(t (car (caddr node))))) 


(defun find-right-mem (node) (memory-part (cadddr node))) 


;;; Function for cs command.

(defun ops-cs (z)
  (cond ((atom z) (conflict-set))
	(t 'what?))) 



;;;; Functions for literalize and related operations.

(defun ops-literal (z)
  (prog (atm val old)
    top  (and (atom z) (return 'bound))
    (or (eq (cadr z) '=) (return (%warn '|wrong format| z)))
    (setq atm (car z))
    (setq val (caddr z))
    (setq z (cdddr z))
    (cond ((not (numberp val))
	   (%warn '|can bind only to numbers| val))
	  ((or (not (symbolp atm)) (variablep atm))
	   (%warn '|can bind only constant atoms| atm))
	  ((and (setq old (literal-binding-of atm)) (not (equal old val)))
	   (%warn '|attempt to rebind attribute| atm))
	  (t (putprop atm val 'ops-bind)))
    (go top))) 


(defun ops-literalize (l)
  (prog (class-name atts)
    (setq class-name (car l))
    (cond ((have-compiled-production)
	   (%warn '|literalize called after p| class-name)
	   (return nil))
	  ((get class-name 'att-list)
	   (%warn '|attempt to redefine class| class-name)
	   (return nil)))
    (setq *class-list* (cons class-name *class-list*))
    (setq atts (remove-duplicates (cdr l)))		; ??? should this
    ; warn of dup atts?
    (test-attribute-names atts)
    (mark-conflicts atts atts)
    (putprop class-name atts 'att-list))) 

(defun ops-vector-attribute (l)
  (cond ((have-compiled-production)
	 (%warn '|vector-attribute called after p| l))
	(t 
	 (test-attribute-names l)
	 (mapc (function vector-attribute2) l)))) 

(defun vector-attribute2 (att) (putprop att t 'vector-attribute))

(defun is-vector-attribute (att) (get att 'vector-attribute))

(defun test-attribute-names (l)
  (mapc (function test-attribute-names2) l)) 

(defun test-attribute-names2 (atm)
  (cond ((or (not (symbolp atm)) (variablep atm))
	 (%warn '|can bind only constant atoms| atm)))) 

(defun finish-literalize nil
  (cond ((not (null *class-list*))
	 (mapc (function note-user-assigns) *class-list*)
	 (mapc (function assign-scalars) *class-list*)
	 (mapc (function assign-vectors) *class-list*)
	 (mapc (function put-ppdat) *class-list*)
	 (mapc (function erase-literal-info) *class-list*)
	 (setq *class-list* nil)
	 (setq *buckets* nil)))) 

(defun have-compiled-production nil (not (zerop *pcount*))) 
	   
(defun put-ppdat (class)
  (prog (al att ppdat)
    (setq ppdat nil)
    (setq al (get class 'att-list))
    top  (cond ((not (atom al))
		(setq att (car al))
		(setq al (cdr al))
		(setq ppdat
		      (cons (cons (literal-binding-of att) att)
			    ppdat))
		(go top)))
    (putprop class ppdat 'ppdat))) 

; note-user-assigns and note-user-vector-assigns are needed only when
; literal and literalize are both used in a program.  They make sure that
; the assignments that are made explicitly with literal do not cause problems
; for the literalized classes.

(defun note-user-assigns (class)
  (mapc (function note-user-assigns2) (get class 'att-list)))

(defun note-user-assigns2 (att)
  (prog (num conf buck clash)
    (setq num (literal-binding-of att))
    (and (null num) (return nil))
    (setq conf (get att 'conflicts))
    (setq buck (store-binding att num))
    (setq clash (find-common-atom buck conf))
    (and clash
	 (%warn '|attributes in a class assigned the same number|
		(cons att clash)))
    (return nil)))

(defun note-user-vector-assigns (att given needed)
  (and (> needed given)
       (%warn '|vector attribute assigned too small a value in literal| att)))

(defun assign-scalars (class)
  (mapc (function assign-scalars2) (get class 'att-list))) 

(defun assign-scalars2 (att)
  (prog (tlist num bucket conf)
    (and (literal-binding-of att) (return nil))
    (and (is-vector-attribute att) (return nil))
    (setq tlist (buckets))
    (setq conf (get att 'conflicts))
    top  (cond ((atom tlist)
		(%warn '|could not generate a binding| att)
		(store-binding att -1.)
		(return nil)))
    (setq num (caar tlist))
    (setq bucket (cdar tlist))
    (setq tlist (cdr tlist))
    (cond ((disjoint bucket conf) (store-binding att num))
	  (t (go top))))) 

(defun assign-vectors (class)
  (mapc (function assign-vectors2) (get class 'att-list))) 

(defun assign-vectors2 (att)
  (prog (big conf new old need)
    (and (not (is-vector-attribute att)) (return nil))
    (setq big 1.)
    (setq conf (get att 'conflicts))
    top  (cond ((not (atom conf))
		(setq new (car conf))
		(setq conf (cdr conf))
		(cond ((is-vector-attribute new)
		       (%warn '|class has two vector attributes|
			      (list att new)))
		      (t (setq big (max (literal-binding-of new) big))))
		(go top)))
    (setq need (1+ big))			;"plus" changed to "+" by gdw
    (setq old (literal-binding-of att))
    (cond (old (note-user-vector-assigns att old need))
	  (t (store-binding att need)))
    (return nil)))

(defun disjoint (la lb) (not (find-common-atom la lb))) 

(defun find-common-atom (la lb)
  (prog nil
    top  (cond ((null la) (return nil))
	       ((member (car la) lb) (return (car la)))
	       (t (setq la (cdr la)) (go top))))) 

(defun mark-conflicts (rem all)
  (cond ((not (null rem))
	 (mark-conflicts2 (car rem) all)
	 (mark-conflicts (cdr rem) all)))) 

(defun mark-conflicts2 (atm lst)
  (prog (l)
    (setq l lst)
    top  (and (atom l) (return nil))
    (conflict atm (car l))
    (setq l (cdr l))
    (go top))) 

(defun conflict (a b)
  (prog (old)
    (setq old (get a 'conflicts))
    (and (not (eq a b))
	 (not (member b old))
	 (putprop a (cons b old) 'conflicts)))) 

;@@@ use intrinsic 
;(defun remove-duplicates  (lst)
   ;  (cond ((atom lst) nil)
	    ;        ((member (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
	    ;        (t (cons (car lst) (remove-duplicates (cdr lst)))))) 

(defun literal-binding-of (name) (get name 'ops-bind)) 

(defun store-binding (name lit)
  (putprop name lit 'ops-bind)
  (add-bucket name lit)) 

(defun add-bucket (name num)
  (prog (buc)
    (setq buc (assoc num (buckets)))
    (and (not (member name buc))
	 (rplacd buc (cons name (cdr buc))))
    (return buc))) 

(defun buckets nil
  (and (atom *buckets*) (setq *buckets* (make-nums *buckets*)))
  *buckets*) 

(defun make-nums (k)
  (prog (nums)
    (setq nums nil)
    l    (and (< k 2.) (return nums))
    (setq nums (cons (ncons k) nums))
    (setq k (1- k))
    (go l))) 

(defun erase-literal-info (class)
  (mapc (function erase-literal-info2) (get class 'att-list))
  (remprop class 'att-list)) 

(defun erase-literal-info2 (att) (remprop att 'conflicts)) 




;;;; Functions for conflict set management and resolution.


;;; Each conflict set element is a list of the following form:
;;; ((p-name . data-part) (sorted wm-recency) special-case-number)

(defun conflict-resolution nil
  (prog (best len)
    (setq len (length *conflict-set*))
    (cond ((> len *max-cs*) (setq *max-cs* len)))
    (setq *total-cs* (+ *total-cs* len))	;"plus" changed to "+" by gdw
    (cond (*conflict-set*
	   (setq best (best-of *conflict-set*))
	   (setq *conflict-set* (delq best *conflict-set*))
	   (return (pname-instantiation best)))
	  (t (return nil))))) 

(defun removecs (name data)
  (prog (cr-data inst cs)
    (setq cr-data (cons name data))
    (setq cs *conflict-set*)
    loop	(cond ((null cs) 
		       (record-refract name data)
		       (return nil)))
    (setq inst (car cs))
    (setq cs (cdr cs))
    (and (not (top-levels-eq (car inst) cr-data)) (go loop))
    (setq *conflict-set* (delq inst *conflict-set*))))

(defun insertcs (name data rating)
  (prog (instan)
    (and (refracted name data) (return nil))
    (setq instan (list (cons name data) (order-tags data) rating))
    (and (atom *conflict-set*) (setq *conflict-set* nil))
    (return (setq *conflict-set* (cons instan *conflict-set*))))) 


(defun remove-from-conflict-set (name)
  (prog (cs entry)
    l1   (setq cs *conflict-set*)
    l2   (cond ((atom cs) (return nil)))
    (setq entry (car cs))
    (setq cs (cdr cs))
    (cond ((eq name (caar entry))
	   (setq *conflict-set* (delq entry *conflict-set*))
	   (go l1))
	  (t (go l2))))) 

(defun order-tags (dat)
  (prog (tags)
    (setq tags nil)
    l1p  (and (atom dat) (go l2p))
    (setq tags (cons (creation-time (car dat)) tags))
    (setq dat (cdr dat))
    (go l1p)
    l2p  (cond ((eq *strategy* 'mea)
		(return (cons (car tags) (dsort (cdr tags)))))
	       (t (return (dsort tags)))))) 

; destructively sort x into descending order

(defun dsort (x)
  (prog (sorted cur next cval nval)
    (and (atom (cdr x)) (return x))
    loop (setq sorted t)
    (setq cur x)
    (setq next (cdr x))
    chek (setq cval (car cur))
    (setq nval (car next))
    (cond ((> nval cval)
	   (setq sorted nil)
	   (rplaca cur nval)
	   (rplaca next cval)))
    (setq cur next)
    (setq next (cdr cur))
    (cond ((not (null next)) (go chek))
	  (sorted (return x))
	  (t (go loop))))) 

(defun best-of (set) (best-of* (car set) (cdr set))) 

(defun best-of* (best rem)
  (cond ((not rem) best)
	((conflict-set-compare best (car rem))
	 (best-of* best (cdr rem)))
	(t (best-of* (car rem) (cdr rem))))) 

(defun pname-instantiation (conflict-elem) (car conflict-elem)) 

(defun order-part (conflict-elem) (cdr conflict-elem)) 

(defun instantiation (conflict-elem)
  (cdr (pname-instantiation conflict-elem))) 


(defun conflict-set-compare (x y)
  (prog (x-order y-order xl yl xv yv)
    (setq x-order (order-part x))
    (setq y-order (order-part y))
    (setq xl (car x-order))
    (setq yl (car y-order))
    data (cond ((and (null xl) (null yl)) (go ps))
	       ((null yl) (return t))
	       ((null xl) (return nil)))
    (setq xv (car xl))
    (setq yv (car yl))
    (cond ((> xv yv) (return t))
	  ((> yv xv) (return nil)))
    (setq xl (cdr xl))
    (setq yl (cdr yl))
    (go data)
    ps   (setq xl (cdr x-order))
    (setq yl (cdr y-order))
    psl  (cond ((null xl) (return t)))
    (setq xv (car xl))
    (setq yv (car yl))
    (cond ((> xv yv) (return t))
	  ((> yv xv) (return nil)))
    (setq xl (cdr xl))
    (setq yl (cdr yl))
    (go psl))) 


(defun conflict-set nil
  (prog (cnts cs p z best)
    (setq cnts nil)
    (setq cs *conflict-set*)
    l1p  (and (atom cs) (go l2p))
    (setq p (caaar cs))
    (setq cs (cdr cs))
    (setq z (assq p cnts))
    (cond ((null z) (setq cnts (cons (cons p 1.) cnts)))
	  (t (rplacd z (1+ (cdr z)))))
    (go l1p)
    l2p  (cond ((atom cnts)
		(setq best (best-of *conflict-set*))
		(terpri)
		(return (list (caar best) 'dominates))))
    (terpri)
    (princ (caar cnts))
    (cond ((> (cdar cnts) 1.)
	   (princ '|	(|)
		  (princ (cdar cnts))
		  (princ '| occurrences)|)))
    (setq cnts (cdr cnts))
    (go l2p))) 
