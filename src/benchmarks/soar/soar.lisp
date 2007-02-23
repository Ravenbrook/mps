;;; -*- Mode: LISP; Package: User -*-

#+declare-unsafe (proclaim '(optimize (speed 3) (safety 0)))
(proclaim
 '(special *matrix* *feature-count* *pcount* *vars* *cur-vars*
	   *curcond* *subnum* *last-node* *last-branch* *first-node*
	   *sendtocall* *flag-part* *alpha-flag-part* *data-part*
	   *alpha-data-part* *ce-vars* *virtual-cnt* *real-cnt*
	   *current-token* *record-array* *result-array* 
	   *max-cs* *total-cs* *limit-cs* *cr-temp* *side*
	   *conflict-set* *halt-flag* *phase* *critical*
	   *cycle-count* *total-token* *max-token* *refracts* 
	   *limit-token* *total-wm* *current-wm* *max-wm*
	   *action-count* *wmpart-list* *wm* *data-matched* *p-name*
	   *variable-memory* *ce-variable-memory* *max-index*
	   *next-index* *size-result-array* *rest* *build-trace* *last*
	   *ptrace* *wtrace* *in-rhs* *recording* *accept-file* *trace-file* 
	   *write-file* *record-index* *max-record-index* *old-wm*
	   *record* *filters* *break-flag* *strategy* *remaining-cycles*
	   *wm-filter* *rhs-bound-vars* *rhs-bound-ce-vars* *ppline* 
	   *ce-count* *brkpts* *class-list* *buckets* *action-type*))

(proclaim '(special *c1* *c2* *c3* *c4* *c5* *c6* *c7* *c8* *c9*
	   *c10* *c11* *c12* *c13* *c14* *c15* *c16* *c17* *c18* *c19*
	   *c20* *c21* *c22* *c23* *c24* *c25* *c26* *c27* *c28* *c29*
	   *c30* *c31* *c32* *c33* *c34* *c35* *c36* *c37* *c38* *c39*
	   *c40* *c41* *c42* *c43* *c44* *c45* *c46* *c47* *c48* *c49*
	   *c50* *c51* *c52* *c53* *c54* *c55* *c56* *c57* *c58* *c59*
	   *c60* *c61* *c62* *c63* *c64* *c65* *c66* *c67* *c68* *c69*
	   *c70* *c71* *c72* *c73* *c74* *c75* *c76* *c77* *c78* *c79*
	   *c80* *c81* *c82* *c83* *c84* *c85* *c86* *c87* *c88* *c89*
	   *c90* *c91* *c92* *c93* *c94* *c95* *c96* *c97* *c98* *c99*
	   *c100* *c101* *c102* *c103* *c104* *c105* *c106* *c107* *c108* 
	   *c109* *c110* *c111* *c112* *c113* *c114* *c115* *c116* *c117* 
	   *c118* *c119* *c120* *c121* *c122* *c123* *c124* *c125* *c126* 
	   *c127* ))

(defmacro soarputprop (n v a)
	`(setf (get ,n ,a) ,v))

(defmacro soarassq (item alist)
	`(assoc ,item ,alist :test #'eq))

; getvector and putvector are fast routines for using ONE-DIMENSIONAL
; arrays.  these routines do no checking; they assume
;	1. the array is a vector with 0 being the index of the first
;	   element
;	2. the vector holds arbitrary list values

(defmacro putvector (array index value)
  `(setf (svref ,array ,index) ,value))

(defmacro getvector (array index)
  `(svref ,array ,index))

(shadow 'remove (find-package 'user))

(defmacro remove (&body z)
  `(ops-remove ',z))

(shadow 'write (find-package 'user))

(defmacro write (&body z)
  `(ops-write ',z))

(defmacro princq (ob &optional st)
  (cond ((eq st nil)
	 `(princ ,ob))
	(t
	 `(princ ,ob ,st))))

(defmacro literal (&body z)
  `(ops-literal ',z))

(defmacro literalize (&body z)
  `(ops-literalize ',z))

(defmacro vector-attribute (&body l)
  `(ops-vector-attribute ',l))

(defmacro p (&body z) 
  `(ops-p ',z))

(defmacro wm (&body a) 
  `(ops-wm ',a))

(defmacro make (&body z)
  `(ops-make ',z))

(defmacro modify (&body z)
  `(ops-modify ',z))

(defmacro bind (&body z)
  `(ops-bind ',z))

(defmacro cbind (&body z)
  `(ops-cbind ',z))

(defmacro call1 (&body z)
  `(ops-call1 ',z))

(defmacro build (&body z)
  `(ops-build ',z))

(defmacro openfile (&body z)
  `(ops-openfile ',z))

(defmacro closefile (&body z)
  `(ops-closefile ',z))

(defmacro default (&body z)
  `(ops-default ',z))

(defmacro accept (&body z)
  `(ops-accept ',z))

(defmacro acceptline (&body z)
  `(ops-acceptline ',z))

(defmacro substr (&body l)
  `(ops-substr ',l))

(defmacro compute (&body z)
  `(ops-compute ',z))

(defmacro arith (&body z)
  `(ops-arith ',z))

(defmacro litval (&body z)
  `(ops-litval ',z))

(defmacro rjust (&body z)
  `(ops-rjust ',z))

(defmacro crlf (&body z)
  `(ops-crlf ',z))

(defmacro tabto (&body z)
  `(ops-tabto ',z))

(defmacro ppwm (&body avlist)
  `(ops-ppwm ',avlist))

(defmacro pm (&body z)
  `(ops-pm ',z))

(defmacro matches (&body rule-list)
  `(ops-matches ',rule-list))

(defmacro excise (&body z)
  `(ops-excise ',z))

(defmacro run (&body z)
  `(ops-run ',z))

(defmacro strategy (&body z)
  `(ops-strategy ',z))

(defmacro cs (&body z)
  `(ops-cs ',z))

(defmacro watch (&body z)
  `(ops-watch ',z))

(defmacro external (&body z) 
  `(ops-external ',z))

(defmacro pbreak (&body z)
  `(ops-pbreak ',z))

(defmacro == (x y)
  `(equal ,x ,y))

(defmacro =alg (a b)
  `(equalp ,a ,b))

(defun ce-gelm (x k)
  (declare (optimize speed))
  (prog nil
    loop (and (== k 1.) (return (car x)))
    (setq k (1- k))
    (setq x (cdr x))
    (go loop))) 

(defun gelm (x k)
  (declare (optimize speed))
  (prog (ce sub)
    (setq ce (truncate  k 10000.))		;use multiple-value-setq???
    (setq sub (- k (* ce 10000.)))		;@@@ ^
    celoop (and (eq ce 0.) (go ph2))
    (setq x (cdr x))
    (and (eq ce 1.) (go ph2))
    (setq x (cdr x))
    (and (eq ce 2.) (go ph2))
    (setq x (cdr x))
    (and (eq ce 3.) (go ph2))
    (setq x (cdr x))
    (and (eq ce 4.) (go ph2))
    (setq ce (- ce 4.))
    (go celoop)
    ph2  (setq x (car x))
    subloop (and (eq sub 0.) (go finis))
    (setq x (cdr x))
    (and (eq sub 1.) (go finis))
    (setq x (cdr x))
    (and (eq sub 2.) (go finis))
    (setq x (cdr x))
    (and (eq sub 3.) (go finis))
    (setq x (cdr x))
    (and (eq sub 4.) (go finis))
    (setq x (cdr x))
    (and (eq sub 5.) (go finis))
    (setq x (cdr x))
    (and (eq sub 6.) (go finis))
    (setq x (cdr x))
    (and (eq sub 7.) (go finis))
    (setq x (cdr x))
    (and (eq sub 8.) (go finis))
    (setq sub (- sub 8.))
    (go subloop)
    finis (return (car x))) )

;;; Utility functions

;;; intersect two lists using eq for the equality test
(defun interq (x y)
  (cond ((atom x) nil)
	((member (car x) y) (cons (car x) (interq (cdr x) y)))
	(t (interq (cdr x) y)))) 


(eval-when (compile load eval)
  (set-macro-character #\{ #'(lambda (s c)
			       (declare (ignore s c))
			       '\{))   ;5/5/83
  (set-macro-character #\} #'(lambda (s c)
			       (declare (ignore s c))
			       '\}))   ;5/5/83
  (set-macro-character #\^ #'(lambda (s c)
			       (declare (ignore s c))
			       '\^))   ;5/5/83
  )

(defun %warn (what where)
  (prog nil
    (terpri)
    (princ '?)
    (and *p-name* (princ *p-name*))
    (princ '"..")
    (princ where)
    (princ '"..")
    (princ what)
    (return where))) 

(defun %error (what where)
  (%warn what where)
  (throw '!error! '!error!))

(defun top-levels-eq (la lb)
  (prog nil
    lx   (cond ((eq la lb) (return t))
	       ((null la) (return nil))
	       ((null lb) (return nil))
	       ((not (eq (car la) (car lb))) (return nil)))
    (setq la (cdr la))
    (setq lb (cdr lb))
    (go lx))) 

;;; LITERAL and LITERALIZE

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
	  (t (soarputprop atm val 'ops-bind))
    (go top))))

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
    (setq atts (remove-duplicates (cdr l)))		
    (test-attribute-names atts)
    (mark-conflicts atts atts)
    (soarputprop class-name atts 'att-list)))

(defun ops-vector-attribute (l)
  (cond ((have-compiled-production)
	 (%warn '|vector-attribute called after p| l))
	(t 
	 (test-attribute-names l)
	 (mapc (function vector-attribute2) l)))) 

(defun vector-attribute2 (att) (soarputprop att t 'vector-attribute))

(defun is-vector-attribute (att) (get att 'vector-attribute))

(defun test-attribute-names (l)
  (mapc (function test-attribute-names2) l)) 

(defun test-attribute-names2 (atm)
  (cond ((or (not (symbolp atm)) (variablep atm))
	 (%warn '|can bind only constant atoms| atm)))) 

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
    (soarputprop class ppdat 'ppdat))) 

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
	 (soarputprop a (cons b old) 'conflicts)))) 

(defun literal-binding-of (name) (get name 'ops-bind)) 

(defun store-binding (name lit)
  (soarputprop name lit 'ops-bind)
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

(defmacro ncons (x) `(cons ,x nil))

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


;;; LHS Compiler

(defun peek-lex nil (car *matrix*)) 

(defun lex nil
  (prog2 nil (car *matrix*) (setq *matrix* (cdr *matrix*)))) 

(defun end-of-p nil (atom *matrix*)) 

(defun rest-of-p nil *matrix*) 

(defun prepare-lex (prod) (setq *matrix* prod)) 

(defun peek-sublex nil (car *curcond*)) 

(defun sublex nil
  (prog2 nil (car *curcond*) (setq *curcond* (cdr *curcond*)))) 

(defun end-of-ce nil (atom *curcond*)) 

(defun rest-of-ce nil *curcond*) 

(defun prepare-sublex (ce) (setq *curcond* ce)) 

(defun make-bottom-node nil (setq *first-node* (list '&bus nil))) 

(defun rating-part (pnode) (cadr pnode)) 

(defun var-part (pnode) (car (cdddr pnode))) 

(defun ce-var-part (pnode) (cadr (cdddr pnode))) 

(defun rhs-part (pnode) (caddr (cdddr pnode))) 

(defun kill-node (node)
  (prog nil
    top  (and (atom node) (return nil))
    (rplaca node '&old)
    (setq node (cdr node))
    (go top))) 

(defun cmp-negce nil (lex) (cmp-ce)) 

(defun cmp-posce nil
  (setq *ce-count* (1+ *ce-count*))
  (cond ((eq (peek-lex) '\{) (cmp-ce+cevar))
	(t (cmp-ce)))) 

(defun cmp-ce+cevar nil
  (prog (z)
    (lex)
    (cond ((atom (peek-lex)) (cmp-cevar) (cmp-ce))
	  (t (cmp-ce) (cmp-cevar)))
    (setq z (lex))
    (or (eq z '\}) (%error '|missing '}'| z)))) 

(defun new-subnum (k)
  (or (numberp k) (%error '|tab must be a number| k))
  (setq *subnum* (fix k))) 

(defun incr-subnum nil (setq *subnum* (1+ *subnum*))) 

(defun cmp-element nil
  (and (eq (peek-sublex) '^) (cmp-tab))
  (cond ((eq (peek-sublex) '\{) (cmp-product))
	(t (cmp-atomic-or-any))))

(defun cmp-atomic-or-any nil
  (cond ((eq (peek-sublex) '<<) (cmp-any))
	(t (cmp-atomic))))

(defun cmp-any nil
  (prog (a z)
    (sublex)
    (setq z nil)
    la   (cond ((end-of-ce) (%error '|missing '>>'| a)))
    (setq a (sublex))
    (cond ((not (eq '>> a)) (setq z (cons a z)) (go la)))
    (link-new-node (list '&any nil (current-field) z)))) 

(defun $litbind (x)
  (prog (r)
    (cond ((and (symbolp x) (setq r (literal-binding-of x)))
	   (return r))
	  (t (return x))))) 

(defun get-bind (x)
  (prog (r)
    (cond ((and (symbolp x) (setq r (literal-binding-of x)))
	   (return r))
	  (t (return nil))))) 

(defun cmp-atomic nil
  (prog (test x)
    (setq x (peek-sublex))
    (cond ((eq x '= ) (setq test 'eq) (sublex))
	  ((eq x '<>) (setq test 'ne) (sublex))
	  ((eq x '<) (setq test 'lt) (sublex))
	  ((eq x '<=) (setq test 'le) (sublex))
	  ((eq x '>) (setq test 'gt) (sublex))
	  ((eq x '>=) (setq test 'ge) (sublex))
	  ((eq x '<=>) (setq test 'xx) (sublex))
	  (t (setq test 'eq)))
    (cmp-symbol test))) 

(defun cmp-product nil
  (prog (save)
    (setq save (rest-of-ce))
    (sublex)
    la   (cond ((end-of-ce)
		(cond ((member '\} save :test #'equal) 
		       (%error '|wrong contex for '}'| save))
		      (t (%error '|missing '}'| save))))
	       ((eq (peek-sublex) '\}) (sublex) (return nil)))
    (cmp-atomic-or-any)
    (go la))) 

(defun symbol-print-name (x)
  (symbol-name x))

(defun cmp-symbol (test)
  (prog (flag)
    (setq flag t)
    (cond ((eq (peek-sublex) '//) (sublex) (setq flag nil)))
    (cond ((and flag (variablep (peek-sublex)))
	   (cmp-var test))
	  ((numberp (peek-sublex)) (cmp-number test))
	  ((symbolp (peek-sublex)) (cmp-constant test))
	  (t (%error '|unrecognized symbol| (sublex)))))) 

(defun cmp-constant (test)
  (or (member test '(eq ne xx))
      (%error '|non-numeric constant after numeric predicate| (sublex)))
  (link-new-node (list (intern (concatenate 'string
					    (symbol-print-name 't)
					    (symbol-print-name  test)
					    (symbol-print-name 'a)))
		       nil
		       (current-field)
		       (sublex)))) 

(defun cmp-number (test)
  (link-new-node 
    (list 
      (intern 
	(concatenate 'string
	  (symbol-print-name 't)
	  (symbol-print-name  test)
          (symbol-print-name 'n)))
      nil				;outs -- nodelist to traverse
      (current-field)			;register 
      (sublex))))			;constant

; %%% things to change to convert set of *cN* to *c* array:
; %%% current-field field-name *subnum*  eval-nodelist ?? wm-hash 
; %%% send-to, &bus

(defun current-field nil (field-name *subnum*)) 

(defun field-name (num)
  (cond ((= num 1.) '*c1*)
	((= num 2.) '*c2*)
	((= num 3.) '*c3*)
	((= num 4.) '*c4*)
	((= num 5.) '*c5*)
	((= num 6.) '*c6*)
	((= num 7.) '*c7*)
	((= num 8.) '*c8*)
	((= num 9.) '*c9*)
	((= num 10.) '*c10*)
	((= num 11.) '*c11*)
	((= num 12.) '*c12*)
	((= num 13.) '*c13*)
	((= num 14.) '*c14*)
	((= num 15.) '*c15*)
	((= num 16.) '*c16*)
	(t (%error '|condition is too long| (rest-of-ce))))) 


;;; Compiling variables
;
;
;
; *cur-vars* are the variables in the condition element currently 
; being compiled.  *vars* are the variables in the earlier condition
; elements.  *ce-vars* are the condition element variables.  note
; that the interpreter will not confuse condition element and regular
; variables even if they have the same name.
;
; *cur-vars* is a list of triples: (name predicate subelement-number)
; eg:		( (<x> eq 3)
		  ;		  (<y> ne 1)
		  ;		  . . . )
;
; *vars* is a list of triples: (name ce-number subelement-number)
; eg:		( (<x> 3 3)
		  ;		  (<y> 1 1)
		  ;		  . . . )
;
; *ce-vars* is a list of pairs: (name ce-number)
; eg:		( (ce1 1)
		  ;		  (<c3> 3)
		  ;		  . . . )


(defun var-dope (var) (soarassq var *vars*))

(defun ce-var-dope (var) (soarassq var *ce-vars*))

(defun cmp-new-var (name test)
  (setq *cur-vars* (cons (list name test *subnum*) *cur-vars*))) 

(defun cmp-old-eq-var (test old)  ; jgk inserted concatenate form
  (link-new-node (list (intern (concatenate 'string
					    (symbol-print-name 't)
					    (symbol-print-name  test)
					    (symbol-print-name 's)))
		       nil
		       (current-field)
		       (field-name (caddr old))))) 

;@@@ added defunb of delq
;(defun delq (i l)
;  (delete i l :test #'eq))

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



(defun cmp-new-eq-var (name old)  ;jgk inserted concatenate form
  (prog (pred next)
    (setq *cur-vars* (delq old *cur-vars*))
    (setq next (soarassq name *cur-vars*))
    (cond (next (cmp-new-eq-var name next))
	  (t (cmp-new-var name 'eq)))
    (setq pred (cadr old))
    (link-new-node (list (intern (concatenate 'string
					      (symbol-print-name 't)
					      (symbol-print-name  pred)
					      (symbol-print-name 's)))
			 nil
			 (field-name (caddr old))
			 (current-field))))) 

(defun cmp-cevar nil
  (prog (name old)
    (setq name (lex))
    (setq old (soarassq name *ce-vars*))
    (and old
	 (%error '|condition element variable used twice| name))
    (setq *ce-vars* (cons (list name 0.) *ce-vars*)))) 

(defun cmp-not nil (cmp-beta '&not)) 

(defun cmp-nobeta nil (cmp-beta nil)) 

(defun cmp-and nil (cmp-beta '&and)) 

(defun cmp-beta (kind)
  (prog (tlist vdope vname #|vpred vpos|# old)
    (setq tlist nil)
    la   (and (atom *cur-vars*) (go lb))
    (setq vdope (car *cur-vars*))
    (setq *cur-vars* (cdr *cur-vars*))
    (setq vname (car vdope))
    ;;  (setq vpred (cadr vdope))    Dario - commented out (unused)
    ;;  (setq vpos (caddr vdope))
    (setq old (soarassq vname *vars*))
    (cond (old (setq tlist (add-test tlist vdope old)))
	  ((not (eq kind '&not)) (promote-var vdope)))
    (go la)
    lb   (and kind (build-beta kind tlist))
    (or (eq kind '&not) (fudge))
    (setq *last-branch* *last-node*))) 

(defun add-test (list new old) ; jgk inserted concatenate form
  (prog (ttype lloc rloc)
    (setq *feature-count* (1+ *feature-count*))
    (setq ttype (intern (concatenate 'string (symbol-print-name 't)
				     (symbol-print-name (cadr new))
				     (symbol-print-name 'b))))
    (setq rloc (encode-singleton (caddr new)))
    (setq lloc (encode-pair (cadr old) (caddr old)))
    (return (cons ttype (cons lloc (cons rloc list)))))) 

; the following two functions encode indices so that gelm can
; decode them as fast as possible

;; %%% change to use clisp hash fns ??
(defun encode-pair (a b) (+ (* 10000. (1- a)) (1- b))) 
;"plus" changed to "+" by gdw

(defun encode-singleton (a) (1- a)) 

(defun fudge nil
  (mapc (function fudge*) *vars*)
  (mapc (function fudge*) *ce-vars*)) 

(defun fudge* (z)
  (prog (a) (setq a (cdr z)) (rplaca a (1+ (car a))))) 

(defun build-beta (type tests)
  (prog (rpred lpred lnode lef)
    (link-new-node (list '&mem nil nil (protomem)))
    (setq rpred *last-node*)
    (cond ((eq type '&and)
	   (setq lnode (list '&mem nil nil (protomem))))
	  (t (setq lnode (list '&two nil nil))))
    (setq lpred (link-to-branch lnode))
    (cond ((eq type '&and) (setq lef lpred))
	  (t (setq lef (protomem))))
    (link-new-beta-node (list type nil lef rpred tests)))) 

(defun protomem nil (list nil)) 

(defun memory-part (mem-node) (car (cadddr mem-node))) 

(defun encode-dope nil
  (prog (r all z k)
    (setq r nil)
    (setq all *vars*)
    la   (and (atom all) (return r))
    (setq z (car all))
    (setq all (cdr all))
    (setq k (encode-pair (cadr z) (caddr z)))
    (setq r (cons (car z) (cons k r)))
    (go la))) 

(defun encode-ce-dope nil
  (prog (r all z k)
    (setq r nil)
    (setq all *ce-vars*)
    la   (and (atom all) (return r))
    (setq z (car all))
    (setq all (cdr all))
    (setq k (cadr z))
    (setq r (cons (car z) (cons k r)))
    (go la))) 



;;; Linking the nodes

(defun link-new-node (r)
  (cond ((not (member (car r) '(&p &mem &two &and &not) :test #'equal))
	 (setq *feature-count* (1+ *feature-count*))))
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-node* (link-left *last-node* r))) 

(defun link-to-branch (r)
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-branch* (link-left *last-branch* r))) 

(defun link-new-beta-node (r)
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-node* (link-both *last-branch* *last-node* r))
  (setq *last-branch* *last-node*)) 

(defun link-left (pred succ)
  (prog (a r)
    (setq a (left-outs pred))
    (setq r (find-equiv-node succ a))
    (and r (return r))
    (setq *real-cnt* (1+ *real-cnt*))
    (attach-left pred succ)
    (return succ))) 

(defun link-both (left right succ)
  (prog (a r)
    (setq a (interq (left-outs left) (right-outs right)))
    (setq r (find-equiv-beta-node succ a))
    (and r (return r))
    (setq *real-cnt* (1+ *real-cnt*))
    (attach-left left succ)
    (attach-right right succ)
    (return succ))) 

(defun attach-right (old new)
  (rplaca (cddr old) (cons new (caddr old)))) 

(defun attach-left (old new)
  (rplaca (cdr old) (cons new (cadr old)))) 

(defun right-outs (node) (caddr node)) 

(defun left-outs (node) (cadr node)) 

(defun find-equiv-node (node list)
  (prog (a)
    (setq a list)
    l1   (cond ((atom a) (return nil))
	       ((equiv node (car a)) (return (car a))))
    (setq a (cdr a))
    (go l1))) 

(defun find-equiv-beta-node (node list)
  (prog (a)
    (setq a list)
    l1   (cond ((atom a) (return nil))
	       ((beta-equiv node (car a)) (return (car a))))
    (setq a (cdr a))
    (go l1))) 

; do not look at the predecessor fields of beta nodes; they have to be
; identical because of the way the candidate nodes were found

(defun equiv (a b)
  (and (eq (car a) (car b))
       (or (eq (car a) '&mem)
	   (eq (car a) '&two)
	   (equal (caddr a) (caddr b)))
       (equal (cdddr a) (cdddr b)))) 

(defun beta-equiv (a b)
  (and (eq (car a) (car b))
       (equal (cddddr a) (cddddr b))
       (or (eq (car a) '&and) (equal (caddr a) (caddr b))))) 

; the equivalence tests are set up to consider the contents of
; node memories, so they are ready for the build action

;;; Network interpreter

(defun match (flag wme)
  (sendto flag (list wme) 'left (list *first-node*)))

; note that eval-nodelist is not set up to handle building
; productions.  would have to add something like ops4's build-flag

(defun eval-nodelist (nl)
  (prog nil
    top  (and (not nl) (return nil))
    (setq *sendtocall* nil)
    (setq *last-node* (car nl))
    (apply (caar nl) (cdar nl))		;; %%% here's the apply cdar nl must 
    					;; be the *cN* item, caar nl is test
    (setq nl (cdr nl))
    (go top))) 

(defun sendto (flag data side nl)
  (prog nil
    top  (and (not nl) (return nil))
    (setq *side* side)
    (setq *flag-part* flag)
    (setq *data-part* data)
    (setq *sendtocall* t)
    (setq *last-node* (car nl))
    (apply (caar nl) (cdar nl))		;; %%% ditto
    (setq nl (cdr nl))
    (go top))) 

(defun &any (outs register const-list)
  (prog (z c)
    (setq z (symbol-value register))
    (cond ((numberp z) (go number)))
    symbol (cond ((null const-list) (return nil))
		 ((eq (car const-list) z) (go ok))
		 (t (setq const-list (cdr const-list)) (go symbol)))
    number (cond ((null const-list) (return nil))
		 ((and (numberp (setq c (car const-list)))
		       (=alg c z))
		  (go ok))
		 (t (setq const-list (cdr const-list)) (go number)))
    ok   (eval-nodelist outs))) 

(defun teqa (outs register constant)
  (and (eq (symbol-value register) constant) (eval-nodelist outs))) 

(defun tnea (outs register constant)
  (and (not (eq (symbol-value register) constant)) (eval-nodelist outs))) 

(defun txxa (outs register constant)
  (declare (ignore constant))
  (and (symbolp (symbol-value register)) (eval-nodelist outs))) 

(defun teqn (outs register constant)
  (prog (z)
    (setq z (symbol-value register))
    (and (numberp z)
	 (=alg z constant)
	 (eval-nodelist outs)))) 

(defun tnen (outs register constant)
  (prog (z)
    (setq z (symbol-value register))
    (and (or (not (numberp z))
	     (not (=alg z constant)))
	 (eval-nodelist outs)))) 

(defun txxn (outs register constant)
  (declare (ignore constant))
  (prog (z)
    (setq z (symbol-value register))
    (and (numberp z) (eval-nodelist outs)))) 

(defun tltn (outs register constant)
  (prog (z)
    (setq z (symbol-value register))
    (and (numberp z)
	 (> constant z)
	 (eval-nodelist outs)))) 

(defun tgtn (outs register constant)
  (prog (z)
    (setq z (symbol-value register))
    (and (numberp z)
	 (> z constant)
	 (eval-nodelist outs)))) 

(defun tgen (outs register constant)
  (prog (z)
    (setq z (symbol-value register))
    (and (numberp z)
	 (not (> constant z))
	 (eval-nodelist outs)))) 

(defun tlen (outs register constant)
  (prog (z)
    (setq z (symbol-value register))
    (and (numberp z)
	 (not (> z constant))
	 (eval-nodelist outs)))) 

(defun teqs (outs vara varb)
  (prog (a b)
    (setq a (symbol-value vara))
    (setq b (symbol-value varb))
    (cond ((eq a b) (eval-nodelist outs))
	  ((and (numberp a)
		(numberp b)
		(=alg a b))
	   (eval-nodelist outs))))) 

(defun tnes (outs vara varb)
  (prog (a b)
    (setq a (symbol-value vara))
    (setq b (symbol-value varb))
    (cond ((eq a b) (return nil))
	  ((and (numberp a)
		(numberp b)
		(=alg a b))
	   (return nil))
	  (t (eval-nodelist outs))))) 

(defun txxs (outs vara varb)
  (prog (a b)
    (setq a (symbol-value vara))
    (setq b (symbol-value varb))
    (cond ((and (numberp a) (numberp b)) (eval-nodelist outs))
	  ((and (not (numberp a)) (not (numberp b)))
	   (eval-nodelist outs))))) 

(defun tlts (outs vara varb)
  (prog (a b)
    (setq a (symbol-value vara))
    (setq b (symbol-value varb))
    (and (numberp a)
	 (numberp b)
	 (> b a)
	 (eval-nodelist outs)))) 

(defun tgts (outs vara varb)
  (prog (a b)
    (setq a (symbol-value vara))
    (setq b (symbol-value varb))
    (and (numberp a)
	 (numberp b)
	 (> a b)
	 (eval-nodelist outs)))) 

(defun tges (outs vara varb)
  (prog (a b)
    (setq a (symbol-value vara))
    (setq b (symbol-value varb))
    (and (numberp a)
	 (numberp b)
	 (not (> b a))
	 (eval-nodelist outs)))) 

(defun tles (outs vara varb)
  (prog (a b)
    (setq a (symbol-value vara))
    (setq b (symbol-value varb))
    (and (numberp a)
	 (numberp b)
	 (not (> a b))
	 (eval-nodelist outs)))) 

(defun &two (left-outs right-outs)
  (prog (fp dp)
    (cond (*sendtocall*
	   (setq fp *flag-part*)
	   (setq dp *data-part*))
	  (t
	   (setq fp *alpha-flag-part*)
	   (setq dp *alpha-data-part*)))
    (sendto fp dp 'left left-outs)
    (sendto fp dp 'right right-outs))) 

(defun &mem (left-outs right-outs memory-list)
  (prog (fp dp)
    (cond (*sendtocall*
	   (setq fp *flag-part*)
	   (setq dp *data-part*))
	  (t
	   (setq fp *alpha-flag-part*)
	   (setq dp *alpha-data-part*)))
    (sendto fp dp 'left left-outs)
    (add-token memory-list fp dp nil)
    (sendto fp dp 'right right-outs))) 

(defun &and (outs lpred rpred tests)
  (prog (mem)
    (cond ((eq *side* 'right) (setq mem (memory-part lpred)))
	  (t (setq mem (memory-part rpred))))
    (cond ((not mem) (return nil))
	  ((eq *side* 'right) (and-right outs mem tests))
	  (t (and-left outs mem tests))))) 

(defun and-left (outs mem tests)
  (prog (fp dp memdp tlist tst lind rind res)
    (setq fp *flag-part*)
    (setq dp *data-part*)
    fail (and (null mem) (return nil))
    (setq memdp (car mem))
    (setq mem (cdr mem))
    (setq tlist tests)
    tloop (and (null tlist) (go succ))
    (setq tst (car tlist))
    (setq tlist (cdr tlist))
    (setq lind (car tlist))
    (setq tlist (cdr tlist))
    (setq rind (car tlist))
    (setq tlist (cdr tlist))
    ;###        (comment the next line differs in and-left & -right)
    (setq res (funcall tst (gelm memdp rind) (gelm dp lind)))
    (cond (res (go tloop))
	  (t (go fail)))
    succ 
    ;###	(comment the next line differs in and-left & -right)
    (sendto fp (cons (car memdp) dp) 'left outs)
    (go fail))) 

(defun and-right (outs mem tests)
  (prog (fp dp memdp tlist tst lind rind res)
    (setq fp *flag-part*)
    (setq dp *data-part*)
    fail (and (null mem) (return nil))
    (setq memdp (car mem))
    (setq mem (cdr mem))
    (setq tlist tests)
    tloop (and (null tlist) (go succ))
    (setq tst (car tlist))
    (setq tlist (cdr tlist))
    (setq lind (car tlist))
    (setq tlist (cdr tlist))
    (setq rind (car tlist))
    (setq tlist (cdr tlist))
    ;###        (comment the next line differs in and-left & -right)
    (setq res (funcall tst (gelm dp rind) (gelm memdp lind)))
    (cond (res (go tloop))
	  (t (go fail)))
    succ 
    ;###        (comment the next line differs in and-left & -right)
    (sendto fp (cons (car dp) memdp) 'right outs)
    (go fail))) 


(defun teqb (new eqvar)
  (cond ((eq new eqvar) t)
	((not (numberp new)) nil)
	((not (numberp eqvar)) nil)
	((=alg new eqvar) t)
	(t nil))) 

(defun tneb (new eqvar)
  (cond ((eq new eqvar) nil)
	((not (numberp new)) t)
	((not (numberp eqvar)) t)
	((=alg new eqvar) nil)
	(t t))) 

(defun tltb (new eqvar)
  (cond ((not (numberp new)) nil)
	((not (numberp eqvar)) nil)
	((> eqvar new) t)
	(t nil))) 

(defun tgtb (new eqvar)
  (cond ((not (numberp new)) nil)
	((not (numberp eqvar)) nil)
	((> new eqvar) t)
	(t nil))) 

(defun tgeb (new eqvar)
  (cond ((not (numberp new)) nil)
	((not (numberp eqvar)) nil)
	((not (> eqvar new)) t)
	(t nil))) 

(defun tleb (new eqvar)
  (cond ((not (numberp new)) nil)
	((not (numberp eqvar)) nil)
	((not (> new eqvar)) t)
	(t nil))) 

(defun txxb (new eqvar)
  (cond ((numberp new)
	 (cond ((numberp eqvar) t)
	       (t nil)))
	(t
	 (cond ((numberp eqvar) nil)
	       (t t))))) 


(defun &p (rating name var-dope ce-var-dope rhs)
  (declare (ignore var-dope ce-var-dope rhs))
  (prog (fp dp)
    (cond (*sendtocall*
	   (setq fp *flag-part*)
	   (setq dp *data-part*))
	  (t
	   (setq fp *alpha-flag-part*)
	   (setq dp *alpha-data-part*)))
    (and (member fp '(nil old)) (removecs name dp))
    (and fp (insertcs name dp rating)))) 

(defun &old (a b c d e)
  (declare (ignore a b c d e))
  nil) 

(defun &not (outs lmem rpred tests)
  (cond ((and (eq *side* 'right) (eq *flag-part* 'old)) nil)
	((eq *side* 'right) (not-right outs (car lmem) tests))
	(t (not-left outs (memory-part rpred) tests lmem)))) 

(defun not-left (outs mem tests own-mem)
  (prog (fp dp memdp tlist tst lind rind res c)
    (setq fp *flag-part*)
    (setq dp *data-part*)
    (setq c 0.)
    fail (and (null mem) (go fin))
    (setq memdp (car mem))
    (setq mem (cdr mem))
    (setq tlist tests)
    tloop (and (null tlist) (setq c (1+ c)) (go fail))
    (setq tst (car tlist))
    (setq tlist (cdr tlist))
    (setq lind (car tlist))
    (setq tlist (cdr tlist))
    (setq rind (car tlist))
    (setq tlist (cdr tlist))
    ;###        (comment the next line differs in not-left & -right)
    (setq res (funcall tst (gelm memdp rind) (gelm dp lind)))
    (cond (res (go tloop))
	  (t (go fail)))
    fin  (add-token own-mem fp dp c)
    (and (== c 0.) (sendto fp dp 'left outs)))) 

(defun not-right (outs mem tests)
  (prog (fp dp memdp tlist tst lind rind res newfp inc newc)
    (setq fp *flag-part*)
    (setq dp *data-part*)
    (cond ((not fp) (setq inc -1.) (setq newfp 'new))
	  ((eq fp 'new) (setq inc 1.) (setq newfp nil))
	  (t (return nil)))
    fail (and (null mem) (return nil))
    (setq memdp (car mem))
    (setq newc (cadr mem))
    (setq tlist tests)
    tloop (and (null tlist) (go succ))
    (setq tst (car tlist))
    (setq tlist (cdr tlist))
    (setq lind (car tlist))
    (setq tlist (cdr tlist))
    (setq rind (car tlist))
    (setq tlist (cdr tlist))
    ;###        (comment the next line differs in not-left & -right)
    (setq res (funcall tst (gelm dp rind) (gelm memdp lind)))
    (cond (res (go tloop))
	  (t (setq mem (cddr mem)) (go fail)))
    succ (setq newc (+ inc newc))		;"plus" changed to "+" by gdw
    (rplaca (cdr mem) newc)
    (cond ((or (and (== inc -1.) (== newc 0.))
	       (and (== inc 1.) (== newc 1.)))
	   (sendto newfp memdp 'right outs)))
    (setq mem (cddr mem))
    (go fail))) 



;;; Node memories


(defun add-token (memlis flag data-part num)
  (prog (was-present)
    (cond ((eq flag 'new)
	   (setq was-present nil)
	   (real-add-token memlis data-part num))
	  ((not flag) 
	   (setq was-present (remove-old memlis data-part num)))
	  ((eq flag 'old) (setq was-present t)))
    (return was-present))) 

(defun real-add-token (lis data-part num)
  (setq *current-token* (1+ *current-token*))
  (cond (num (rplaca lis (cons num (car lis)))))
  (rplaca lis (cons data-part (car lis)))) 

(defun remove-old (lis data num)
  (cond (num (remove-old-num lis data))
	(t (remove-old-no-num lis data)))) 

(defun remove-old-num (lis data)
  (prog (m next last)
    (setq m (car lis))
    (cond ((atom m) (return nil))
	  ((top-levels-eq data (car m))
	   (setq *current-token* (1- *current-token*))
	   (rplaca lis (cddr m))
	   (return (car m))))
    (setq next m)
    loop (setq last next)
    (setq next (cddr next))
    (cond ((atom next) (return nil))
	  ((top-levels-eq data (car next))
	   (rplacd (cdr last) (cddr next))
	   (setq *current-token* (1- *current-token*))
	   (return (car next)))
	  (t (go loop))))) 

(defun remove-old-no-num (lis data)
  (prog (m next last)
    (setq m (car lis))
    (cond ((atom m) (return nil))
	  ((top-levels-eq data (car m))
	   (setq *current-token* (1- *current-token*))
	   (rplaca lis (cdr m))
	   (return (car m))))
    (setq next m)
    loop (setq last next)
    (setq next (cdr next))
    (cond ((atom next) (return nil))
	  ((top-levels-eq data (car next))
	   (rplacd last (cdr next))
	   (setq *current-token* (1- *current-token*))
	   (return (car next)))
	  (t (go loop))))) 



;;; Conflict Resolution
;
;
; each conflict set element is a list of the following form:
; ((p-name . data-part) (sorted wm-recency) special-case-number)

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

(defun order-part (conflict-elem) (cdr conflict-elem)) 

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



;;; WM maintaining functions
;
; The order of operations in the following two functions is critical.
; add-to-wm order: (1) change wm (2) record change (3) match 
; remove-from-wm order: (1) record change (2) match (3) change wm
; (back will not restore state properly unless wm changes are recorded
; before the cs changes that they cause)  (match will give errors if 
; the thing matched is not in wm at the time)

; remove-from-wm uses eq, not equal to determine if wme is present

; mapwm maps down the elements of wm, applying fn to each element
; each element is of form (datum . creation-time)

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

(defun get-wm (z)
  (setq *wm-filter* z)
  (setq *wm* nil)
  (mapwm (function get-wm2))
  (prog2 nil *wm* (setq *wm* nil))) 

(defun get-wm2 (elem) 
  (cond ((or (null *wm-filter*) (member (cdr elem) *wm-filter*))
	(setq *wm* (cons (car elem) *wm*)))))

(defun creation-time (wme)
  (cdr (soarassq wme (get (wm-hash wme) 'wmpart*)))) 

(defun refresh nil
  (prog nil
    (setq *old-wm* nil)
    (mapwm (function refresh-collect))
    (mapc (function refresh-del) *old-wm*)
    (mapc (function refresh-add) *old-wm*)
    (setq *old-wm* nil))) 

(defun refresh-collect (x) (setq *old-wm* (cons x *old-wm*))) 

(defun refresh-del (x) (remove-from-wm (car x))) 

;;;soar redefined add-to-wm 
;;;;(defun refresh-add (x) (add-to-wm (car x) (cdr x))) 

(defun trace-file ()
  (prog (port)
    (setq port *standard-output*)
    (cond (*trace-file*
	   (setq port ($ofile *trace-file*))
	   (cond ((null port)
		  (%warn '|trace: file has been closed| *trace-file*)
		  (setq port *standard-output*)))))
    (return port)))

;;; Basic functions for RHS evaluation

(defun time-tag-print (data port)
  (cond ((not (null data))
	 (time-tag-print (cdr data) port)
	 (princq '| | port)
	 (princq (creation-time (car data)) port))))

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
    (setq r (soarassq x *ce-variable-memory*))
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


(defun $change (x)
  (prog nil
    (cond ((consp  x) (eval-function x))	;dtpr\consp gdw
	  (t ($value ($varbind x)))))) 

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


(defun eval-function (form)
  (cond ((not *in-rhs*)
	 (%warn '|functions cannot be used at top level| (car form)))
	(t (eval form))))

;;; Functions to manipulate the result array

(defun $reset nil
  (setq *max-index* 0.)
  (setq *next-index* 1.)) 

(defun rhs-tab (z) ($tab ($varbind z)))

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


(defun use-result-array nil
  (prog (k r)
    (setq k *max-index*)
    (setq r nil)
    top  (and (== k 0.) (return r))
    (setq r (cons (getvector *result-array* k) r))
    (setq k (1- k))
    (go top))) 

(defun $assert nil
  (setq *last* (use-result-array))
  (add-to-wm *last*))

(defun $parametercount nil *max-index*)

(defun $parameter (k)
  (cond ((or (not (numberp k)) (> k *size-result-array*) (< k 1.))
	 (%warn '|illegal parameter number | k)
	 nil)
	((> k *max-index*) nil)
	(t (getvector *result-array* k))))


;;; RHS actions

(defun ops-make (z)
  (prog nil
    ($reset)
    (eval-args z)
    ($assert))) 

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

(defun ops-call1 (z)
  (prog (f)
    (setq f (car z))
    ($reset)
    (eval-args (cdr z))
    (funcall f))) 

(defmacro append-string (x)
  `(setq wrstring (concatenate 'simple-string wrstring ,x)))

(defun default-write-file ()
  (prog (port)
    (setq port *standard-output*)
    (cond (*write-file*
	   (setq port ($ofile *write-file*))
	   (cond ((null port) 
		  (%warn '|write: file has been closed| *write-file*)
		  (setq port *standard-output*)))))
    (return port)))

(defun do-rjust (width value port)
  (prog (size)
    (cond ((eq value '|=== T A B T O ===|)
	   (%warn '|rjust cannot precede this function| 'tabto)
	   (return nil))
	  ((eq value '|=== C R L F ===|)
	   (%warn '|rjust cannot precede this function| 'crlf)
	   (return nil))
	  ((eq value '|=== R J U S T ===|)
	   (%warn '|rjust cannot precede this function| 'rjust)
	   (return nil)))
    ;original->        (setq size (flatc value (1+ width)))
    (setq size (min value (1+ width)))  ;### KLUGE
    (cond ((> size width)
	   (princq '| | port)
	   (princq value port)
	   (return nil)))
    (princq value port)))

(defun do-tabto (col &optional port)
  (prog (pos)
    (finish-output port);kluge
    (setq pos 0);kluge
    (do ((k (- col pos) (1- k))) ((not (> k 0))) (princq '| | port))
    (return nil)))

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

(defun ops-openfile (z)
  (prog (file mode id)
    ($reset)
    (eval-args z)
    (cond ((not (equal ($parametercount) 3.))
	   (%warn '|openfile: wrong number of arguments| z)
	   (return nil)))
    (setq id ($parameter 1))
    (setq file ($parameter 2))
    (setq mode ($parameter 3))
    (cond ((not (symbolp id))
	   (%warn '|openfile: file id must be a symbolic atom| id)
	   (return nil))
	  ((null id)
	   (%warn '|openfile: 'nil' is reserved for the terminal| nil)
	   (return nil))
	  ((or ($ifile id)($ofile id))
	   (%warn '|openfile: name already in use| id)
	   (return nil)))
       (cond ((eq mode 'in) (soarputprop id (setq id (infile file)) 'inputfile))
	  ((eq mode 'out) (soarputprop id (setq id (outfile file)) 'outputfile))
	  (t (%warn '|openfile: illegal mode| mode)
	     (return nil)))
    (return nil)))

(defun $ifile (x) 
  (cond ((symbolp x) (get x 'inputfile))
	(t nil)))

(defun $ofile (x) 
  (cond ((symbolp x) (get x 'outputfile))
	(t nil)))

(defun ops-closefile (z)
  ($reset)
  (eval-args z)
  (mapc (function closefile2) (use-result-array)))

(defun closefile2 (file)
  (prog (port)
    (cond ((not (symbolp file))
	   (%warn '|closefile: illegal file identifier| file))
	  ((setq port ($ifile file))
	   (close port)
	   (remprop file 'inputfile))
	  ((setq port ($ofile file))
	   (close port)
	   (remprop file 'outputfile)))
    (return nil)))

(defun ops-default (z)
  (prog (file use)
    ($reset)
    (eval-args z)
    (cond ((not (equal ($parametercount) 2.))
	   (%warn '|default: wrong number of arguments| z)
	   (return nil)))
    (setq file ($parameter 1))
    (setq use ($parameter 2))
    (cond ((not (symbolp file))
	   (%warn '|default: illegal file identifier| file)
	   (return nil))
	  ((not (member use '(write accept trace) :test #'equal))
	   (%warn '|default: illegal use for a file| use)
	   (return nil))
	  ((and (member use '(write trace) :test #'equal)
		(not (null file))
		(not ($ofile file)))
	   (%warn '|default: file has not been opened for output| file)
	   (return nil))
	  ((and (equal use 'accept) 
		(not (null file))
		(not ($ifile file)))
	   (%warn '|default: file has not been opened for input| file)
	   (return nil))
	  ((equal use 'write) (setq *write-file* file))
	  ((equal use 'accept) (setq *accept-file* file))
	  ((equal use 'trace) (setq *trace-file* file)))
    (return nil)))

(defun flat-value (x)
  (cond ((atom x) ($value x))
	(t (mapc (function flat-value) x)))) 

(defun ops-accept (z)
  (prog (port arg)
    (cond ((> (length z) 1.)
	   (%warn '|accept: wrong number of arguments| z)
	   (return nil)))
    (setq port *standard-input*)
    (cond (*accept-file*
	   (setq port ($ifile *accept-file*))
	   (cond ((null port) 
		  (%warn '|accept: file has been closed| *accept-file*)
		  (return nil)))))
    (cond ((= (length z) 1)
	   (setq arg ($varbind (car z)))
	   (cond ((not (symbolp arg))
		  (%warn '|accept: illegal file name| arg)
		  (return nil)))
	   (setq port ($ifile arg))
	   (cond ((null port) 
		  (%warn '|accept: file not open for input| arg)
		  (return nil)))))
    (cond ((equal (peek-char t port nil "eof" ) "eof" )
	   ($value 'end-of-file)
	   (return nil)))
    (flat-value (read port)))) 

(defun ops-acceptline (z)
  (let ((port *standard-input*)
	(def z))
    (cond (*accept-file*
	   (setq port ($ifile *accept-file*))
	   (cond ((null port) 
		  (%warn '|acceptline: file has been closed| 
			 *accept-file*)
		  (return-from ops-acceptline nil)))))
    (cond ((> (length def) 0)
	   (let ((arg ($varbind (car def))))
	     (cond ((and (symbolp arg) ($ifile arg))
		    (setq port ($ifile arg))
		    (setq def (cdr def)))))))
    (let ((line (read-line port nil 'eof)))
      (declare (simple-string line))
      ;; Strip meaningless characters from start and end of string.
      (setq line (string-trim '(#\( #\) #\, #\tab #\space) line))
      (when (equal line "")
	(mapc (function $change) def)
	(return-from ops-acceptline nil))
      (setq line (concatenate 'simple-string "(" line ")"))
      ;; Read all items from the line
      (flat-value (read-from-string line)))))

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
    (setq k 1.)
    la   (cond ((> k end) (return nil))
	       ((not (< k start)) ($value (car elm))))
    (setq elm (cdr elm))
    (setq k (1+ k))
    (go la))) 

(defun ops-compute (z) ($value (ari z))) 

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

;;; Printing WM

(defun ppwm2 (elm-tag)
  (cond ((filter (car elm-tag))
	 (terpri) (ppelm (car elm-tag) (default-write-file))))) 

(defun filter (elm)
  (prog (fl indx val)
    (setq fl *filters*)
    top  (and (atom fl) (return t))
    (setq indx (car fl))
    (setq val (cadr fl))
    (setq fl (cddr fl))
    (and (ident (nth (1- indx) elm) val) (go top))
    (return nil))) 

(defun ident (x y)
  (cond ((eq x y) t)
	((not (numberp x)) nil)
	((not (numberp y)) nil)
	((=alg x y) t)
	(t nil))) 

; the new ppelm is designed especially to handle literalize format
; however, it will do as well as the old ppelm on other formats

(defun ppelm (elm port)
  (prog (ppdat sep val att mode lastpos)
    (princq (creation-time elm) port)
    (princq '|:  | port)
    (setq mode 'vector)
    (setq ppdat (get (car elm) 'ppdat))
    (and ppdat (setq mode 'a-v))
    (setq sep "(")				; ")" 
    (setq lastpos 0)
    (do ((curpos 1 (1+ curpos)) (vlist elm (cdr vlist)))
	((atom vlist) nil)					; terminate
      (setq val (car vlist))				; tagbody begin
      (setq att (assoc curpos ppdat))	;should ret (curpos attr-name) 
      (cond (att (setq att (cdr att)))	; att = (attr-name) ??
	    (t (setq att curpos)))
      (and (symbolp att) (is-vector-attribute att) (setq mode 'vector))
      (cond ((or (not (null val)) (eq mode 'vector))
	     (princq sep port)
	     (ppval val att lastpos port)
	     (setq sep '|    |)
	     (setq lastpos curpos))))
    (princq '|)| port)))

(defun ppval (val att lastpos port)
  ;  (break "in ppval")		
  (cond ((not (equal att (1+ lastpos)))		; ok, if we got an att 
	 (princq '^ port)
	 (princq att port)
	 (princq '| | port)))
  (princq val port))

;;; printing production memory

(defun ppline2 ()
  (prog (needspace)
    (setq needspace nil)
    top  (and (atom *ppline*) (return nil))
    (and needspace (princ '| |))
    (cond ((eq (car *ppline*) '^) (ppattval))
	  (t (pponlyval)))
    (setq needspace t)
    (go top)))

(defun getval ()
  (prog (res v1)
    (setq v1 (car *ppline*))
    (setq *ppline* (cdr *ppline*))
    (cond ((member v1 '(= <> < <= => > <=>))
	   (setq res (cons v1 (getval))))
	  ((eq v1 '{)
	   (setq res (cons v1 (getupto '}))))
	  ((eq v1 '<<)
	   (setq res (cons v1 (getupto '>>))))
	  ((eq v1 '//)
	   (setq res (list v1 (car *ppline*)))
	   (setq *ppline* (cdr *ppline*)))
	  (t (setq res (list v1))))
    (return res)))

(defun getupto (end)
  (prog (v)
    (and (atom *ppline*) (return nil))
    (setq v (car *ppline*))
    (setq *ppline* (cdr *ppline*))
    (cond ((eq v end) (return (list v)))
	  (t (return (cons v (getupto end))))))) 

;;; backing up

(defun record-index-plus (k)
  (setq *record-index* (+ k *record-index*))	;"plus" changed to "+" by gdw
  (cond ((< *record-index* 0.)
	 (setq *record-index* *max-record-index*))
	((> *record-index* *max-record-index*)
	 (setq *record-index* 0.)))) 

(defun initialize-record nil
  (setq *record-index* 0.)
  (setq *recording* nil)
  (setq *max-record-index* 31.)
  (putvector *record-array* 0. nil)) 

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

(defun record-refract (rule data)
  (and *recording*
       (setq *record* (cons '<=refract (cons rule (cons data *record*))))))

(defun refracted (rule data)
  (prog (z)
    (and (null *refracts*) (return nil))
    (setq z (cons rule data))
    (return (member z *refracts* :test #'equal))))

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

(defun undo-record (r)
  (prog (save act a b rate)
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


;;; Check the RHSs of productions 

(defun check-rhs (rhs) (mapc (function check-action) rhs))

(defun check-build (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-build-collect (cdr z)))

(defun check-build-collect (args)
  (prog (r)
    top	(and (null args) (return nil))
    (setq r (car args))
    (setq args (cdr args))
    (cond ((consp  r) (check-build-collect r))	;dtpr\consp gdw
	  ((eq r '\\)
	   (and (null args) (%warn '|nothing to evaluate| r))
	   (check-rhs-value (car args))
	   (setq args (cdr args))))
    (go top)))

(defun check-remove (z) 				;@@@ kluge by gdw
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (mapc (function check-rhs-ce-var) (cdr z))) 

(defun check-openfile (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-closefile (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-default (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-write (z)				;note this works w/write
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-call1 (z)
  (prog (f)
    (and (null (cdr z)) (%warn '|needs arguments| z))
    (setq f (cadr z))
    (and (variablep f)
	 (%warn '|function name must be a constant| z))
    (or (symbolp f)
	(%warn '|function name must be a symbolic atom| f))
    (or (externalp f)
	(%warn '|function name not declared external| f))
    (check-change& (cddr z)))) 

(defun check-halt (z)
  (or (null (cdr z)) (%warn '|does not take arguments| z))) 

(defun check-cbind (z)
  (prog (v)
    (or (= (length z) 2.) (%warn '|takes only one argument| z))
    (setq v (cadr z))
    (or (variablep v) (%warn '|takes variable as argument| z))
    (note-ce-variable v))) 

(defun check-bind (z)
  (prog (v)
    (or (> (length z) 1.) (%warn '|needs arguments| z))
    (setq v (cadr z))
    (or (variablep v) (%warn '|takes variable as argument| z))
    (note-variable v)
    (check-change& (cddr z)))) 


(defun check-change& (z)
  (prog (r tab-flag)
    (setq tab-flag nil)
    la   (and (atom z) (return nil))
    (setq r (car z))
    (setq z (cdr z))
    (cond ((eq r '^)
	   (and tab-flag
		(%warn '|no value before this tab| (car z)))
	   (setq tab-flag t)
	   (check-tab-index (car z))
	   (setq z (cdr z)))
	  ((eq r '//) (setq tab-flag nil) (setq z (cdr z)))
	  (t (setq tab-flag nil) (check-rhs-value r)))
    (go la))) 

(defun check-rhs-ce-var (v)
  (cond ((and (not (numberp v)) (not (ce-bound? v)))
	 (%warn '|unbound element variable| v))
	((and (numberp v) (or (< v 1.) (> v *ce-count*)))
	 (%warn '|numeric element designator out of bounds| v)))) 

(defun check-rhs-atomic (x)
  (and (variablep x) 
       (not (bound? x)) 
       (%warn '|unbound variable| x)))

(defun check-rhs-function (x)
  (prog (a)
    (setq a (car x))
    (cond ((eq a 'compute) (check-compute x))
	  ((eq a 'arith) (check-compute x))
	  ((eq a 'substr) (check-substr x))
	  ((eq a 'accept) (check-accept x))
	  ((eq a 'acceptline) (check-acceptline x))
	  ((eq a 'crlf) (check-crlf x))
	  ((eq a 'genatom) (check-genatom x))
	  ((eq a 'litval) (check-litval x))
	  ((eq a 'tabto) (check-tabto x))
	  ((eq a 'rjust) (check-rjust x))
	  ((not (externalp a))
	   (%warn '"rhs function not declared external" a)))))

(defun check-litval (x) 
  (or (= (length x) 2) (%warn '|wrong number of arguments| x))
  (check-rhs-atomic (cadr x)))

(defun check-accept (x)
  (cond ((= (length x) 1) nil)
	((= (length x) 2) (check-rhs-atomic (cadr x)))
	(t (%warn '|too many arguments| x))))

(defun check-acceptline (x)
  (mapc (function check-rhs-atomic) (cdr x)))

(defun check-crlf (x) 
  (check-0-args x)) 

(defun check-genatom (x) (check-0-args x)) 

(defun check-tabto (x)
  (or (= (length x) 2) (%warn '|wrong number of arguments| x))
  (check-print-control (cadr x)))

(defun check-rjust (x)
  (or (= (length x) 2) (%warn '|wrong number of arguments| x))
  (check-print-control (cadr x)))

(defun check-0-args (x)
  (or (= (length x) 1.) (%warn '|should not have arguments| x))) 

(defun check-substr (x)
  (or (= (length x) 4.) (%warn '|wrong number of arguments| x))
  (check-rhs-ce-var (cadr x))
  (check-substr-index (caddr x))
  (check-last-substr-index (cadddr x))) 

(defun check-compute (x) (check-arithmetic (cdr x))) 

(defun check-arithmetic (l)
  (cond ((atom l)
	 (%warn '|syntax error in arithmetic expression| l))
	((atom (cdr l)) (check-term (car l)))
	((not (member (cadr l) '(+ - * // \\)))	;"plus" changed to "+" by gdw
	 (%warn '|unknown operator| l))
	(t (check-term (car l)) (check-arithmetic (cddr l))))) 

(defun check-term (x)
  (cond ((consp  x) (check-arithmetic x))	;dtpr\consp gdw
	(t (check-rhs-atomic x)))) 

(defun check-last-substr-index (x)
  (or (eq x 'inf) (check-substr-index x))) 

(defun check-substr-index (x)
  (prog (v)
    (cond ((bound? x) (return x)))
    (setq v ($litbind x))
    (cond ((not (numberp v))
	   (%warn '|unbound symbol used as index in substr| x))
	  ((or (< v 1.) (> v 127.))
	   (%warn '|index out of bounds in tab| x))))) 

(defun check-print-control (x)
  (prog ()
    (cond ((bound? x) (return x)))
    (cond ((or (not (numberp x)) (< x 1.) (> x 127.))
	   (%warn '|illegal value for printer control| x))))) 

(defun check-tab-index (x)
  (prog (v)
    (cond ((bound? x) (return x)))
    (setq v ($litbind x))
    (cond ((not (numberp v))
	   (%warn '|unbound symbol occurs after ^| x))
	  ((or (< v 1.) (> v 127.))
	   (%warn '|index out of bounds after ^| x))))) 

(defun note-variable (var)
  (setq *rhs-bound-vars* (cons var *rhs-bound-vars*)))

(defun bound? (var)
  (or (member var *rhs-bound-vars*)
      (var-dope var)))

(defun note-ce-variable (ce-var)
  (setq *rhs-bound-ce-vars* (cons ce-var *rhs-bound-ce-vars*)))

(defun ce-bound? (ce-var)
  (or (member ce-var *rhs-bound-ce-vars*)
      (ce-var-dope ce-var)))

;;; Top level routines

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

(defun check-limits nil
  (cond ((> (length *conflict-set*) *limit-cs*)
	 (format t "~%~%conflict set size exceeded the limit of ~D after ~D~%"
		 *limit-cs* *p-name*)
	 (setq *halt-flag* t)))
  (cond ((> *current-token* *limit-token*)
	 (format t "~%~%token memory size exceeded the limit of ~D after ~D~%"
		 *limit-token* *p-name*)
	 (setq *halt-flag* t))))

(defun top-level-remove (z)
  (cond ((equal z '(*)) (process-changes nil (get-wm nil)))
	(t (process-changes nil (get-wm z))))) 

(defun ops-excise (z) (mapc (function excise-p) z))

(defun ops-strategy (z)
  (cond ((atom z) *strategy*)
	((equal z '(lex)) (setq *strategy* 'lex))
	((equal z '(mea)) (setq *strategy* 'mea))
	(t 'what?))) 

(defun ops-cs (z)
  (cond ((atom z) (conflict-set))
	(t 'what?))) 

(defun ops-external (z) (catch '!error! (external2 z)))

(defun external2 (z) (mapc (function external3) z))

(defun external3 (x) 
  (cond ((symbolp x) (soarputprop x t 'external-routine))
	(t (%error '|not a legal function name| x))))

(defun externalp (x)
  (cond ((symbolp x) t)
	(t (%warn '|not a legal function name| x) nil)))

(defun rematm (atm list)
  (cond ((atom list) list)
	((eq atm (car list)) (rematm atm (cdr list)))
	(t (cons (car list) (rematm atm (cdr list))))))

(defun broken (rule) (member rule *brkpts*))

(defun fix (x) (floor x))

(defun infile (f_name)
  (open f_name :direction :input))

(defun sequencep (x) (typep x 'sequence))

(defun outfile (f_name)
  (open f_name :direction :output :if-exists :new-version))

;**********
; Soar 4.0
;**********

(proclaim '(special *3600-display* *action-closure* *action-list* 
	            *already-fired* *always-learn*  *attribute-index*
		    *begin-time* *big-index* *big-info-index* 
                    *brknames* *brkrun* *cav* *cavi* *chunk-all-paths* 
		    *chunk-classes* *chunk-free-problem-spaces* *chunks* 
	            *closure* *condition-list* *condition-vars* *context* 
                    *context-field-found* *context-stack* *copy-action-list* 
		    *created-list* *creating-subgoal* 
	            *current-goal* *current-list* *current-preferences*
		    *current-production-trace* *data-matched* *decide-count*
	            *decide-trace* *default-multi* *default-user-select*
		    *dtrace* *elaborations-count* *elapsed-time*
	            *first-action* *first-remove* *free-gensym-list* 
                    *free-pname-list* *global-pair* *goal-index* *gtrace* 
                    *id-index* *impasse-subset-not-equal* *indent* *init-wm* 
                    *instance-attributes* *interlisp* *last-value-var* 
		    *learn-ids* *learning* *ltrace* *max-chunk-conditions*
	            *max-elaborations* *max-recurse* *multi-attribute*
	            *never-learn* *new-chunks* *new-contexts* *new-root*
	            *operator-index* *ops5-actions* *order-trace* *otrace*
	            *p-type* *pick-first-condition* *pnames* 
		    *preference-found* *print-attribute-list* *print-learn* 
                    *print-pname* *print-spo-list* *prior-operator* 
                    *problem-space-index* *prod-count* *rcontext* 
		    *rcontext-stack* *remaining-decide* *result*
	            *save-action* *save-class-list* *save-condition*
	            *select-equal* *slot-index* *smaller-chunks* 
                    *sp-classes* *split-actions* *spo-default-depth* 
		    *state-index* *strace* *sub-context* *subgoal-results*
	            *subgoal-tabs* *suspected-duplicates* *trace-number*
	            *tracep* *tracep-list* *ttrace* *unbound* 
	            *unbound-action-ids* *used-char-list* *used-gensym-list*
	            *used-var-list* *user-ids* *value-index* *variable-pairs*
	            *version-number* *warning* *watch-free-problem-spaces*
		    *wme-list-stack*))

; ******************************

;;;  Macros and functions to help in the conversion of the Soar system
;;;     from Interlisp to Commonlisp.

(defmacro soarterpri (&body z)
  (cons 'terpri z))

(defmacro smake (&body z)
  (cond ((assoc (car z) *sp-classes*)
	 (spm-to-make z))
	(t
	 (cons 'make z))))

(defmacro soarwarn (&body z)
	(cons '%warn z))	; Using Ops5 %warn function	
(defmacro %%warn (&body z)
	(cons '%warn z))	; Using Ops5 %warn function	
(defmacro %%error (&body z)
	(cons '%error z))	; Using Ops5 %error function	

(defmacro soarsort (list)
	`(sort ,list #'string-lessp))

(defmacro soardelq (item list)
	`(delete ,item ,list :test #'eq))
(defmacro dremove (item list)
	`(delete ,item ,list :test #'eq))

(defmacro soarassoc (item alist)
	`(assoc ,item ,alist :test #'equalp))

(defmacro soarmemq (item list)
	`(member ,item ,list :test #'eq))
(defmacro soarmember (item list)
	`(member ,item ,list :test #'equalp))

(defun addprop (n a v)
	(soarputprop n (cons v (get n a)) a))

(defmacro soarmapc (func list)
	`(mapc ,func ,list))

(defmacro soarmapcar (func list)
	`(mapcar ,func ,list))

(defmacro soarmapconc (func list)
	`(mapcan ,func ,list))

(defmacro soarpush (item list)
	`(push ,item ,list))

(defmacro quotient (x y)
	`(float (/ ,x ,y)))

(defmacro neq (&body z)
	(list 'not  (cons 'eq z)))

(defmacro copy (thing)
	`(copy-tree ,thing))

(defmacro soarnth (ind list)
	`(nth ,ind ,list))

(defmacro soarnthcdr (ind list)
	`(nthcdr ,ind ,list))

(defmacro alwaystime ()
	`(get-internal-run-time))

(defmacro time-conversion (tempus)
        `(float (/ ,tempus internal-time-units-per-second)))

(defmacro init-gensym (&body z)
	(declare (ignore z))
	`(gensym 0) )	

(defun soargensymbol (x)
	(gentemp (string x)))

(defun soarnthchar (sym num)
  (char (string sym) (- num 1)))

(defun soarpack (a b c d)
	(intern (format nil "~A~A~D~A" a b c d)))

(defun soarpack2 (a b)
	(intern (format nil "~A~A" a b)))

(defmacro soarload (f)
        `(load ,f))

(defmacro soar-do (&body z)
	(cons 'do z))

(defmacro soarwhile (test &body z)
  (append (list 'do '() (list (list 'not test))) z))

(defun rplnode (x c d)
        (rplaca x c)
        (rplacd x d))

(defmacro soarcarsort (x)
	`(sort ,x #'string-lessp :key #'car))

(defmacro array (n)
	`(make-array ,n :initial-element () ))
(defmacro makevector (n)
	`(make-array ,n :initial-element () ))

(defmacro soarcatch (&body z)
	(cons 'catch z))

(defmacro *throw (&body z)
	(cons 'throw z))

(defmacro readp (z)
	`(listen ,z))

(defmacro concat (&body z)
	(cons 'concatenate (cons '(quote string) z)))

(defmacro dreverse (x)
	`(nreverse ,x))


(eval-when (compile load eval)
  (set-macro-character #\{ #'(lambda (s c)
			       (declare (ignore s c))
			       '\{))   ;5/5/83
  (set-macro-character #\} #'(lambda (s c)
			       (declare (ignore s c))
			       '\}))   ;5/5/83
  (set-macro-character #\^ #'(lambda (s c)
			       (declare (ignore s c))
			       '\^))   ;5/5/83
  )

(defmacro SETSYNTAX (&body z)
	`() )		; I'm betting that these can be ignored

(defun soarsyntax ()
)

(defun soarresetsyntax ()
)

(defmacro selectq (&body z)
	(setq z (nreverse z))
	(cond
	  ((eq 'err (car z))
	   (cons 'ecase (nreverse (cdr z))))
	  (t
	   (cons 'case (nreverse z))))
)

(defun soarclearprops (sym)
	(setf (symbol-plist sym)  () )
)

(defun soarmachinetype ()
	(setq *3600-display* nil)
	(setq *interlisp* nil)
)


(defun interlisp-menu (x)
	(declare (ignore x))
	t
)

(defun pm-size () 
	(terpri)
	(printlinec (list *pcount* 'productions
				(list *real-cnt* '// *virtual-cnt* 'nodes))))

(defun printlinec (x)
	(mapc (function printlinec*) x))

(defun printlinec* (x)
	(princ " ")
	(princ  x))

(defun flatc (x)
    (cond ((numberp x)
	   3)
	  ((atom x)
	   (length (string x)))
          (t
	   (+ (flatc (car x)) (flatc (cdr x)) 1))))

(defun flast (list)
	 (last list))
(defun soarlast (list)
	 (car (last list)))

(defun soarlistp (thing)
  (cond ((null thing)
	 NIL)
	(t
	 (listp thing))))

(defmacro comment (&body z)
	(declare (ignore z))
	t)


(defmacro pop-match-elt (x)
	`(prog (temp)
	  (setq temp (classify-match-elt ,x))
	  (setq ,x (car temp))
	  (return (cdr temp))))

(defmacro pop-term (x)
	`(prog (temp)
	  (setq temp (classify-term ,x))
	  (setq ,x (car temp))
	  (return (cdr temp))))

(defun variablep (x)
  (and (symbolp x) (equalp (soarnthchar x 1) #\<)
       (not (predicatep x)) (not (eq x '<<))))

; ******************************

;;; Do not print a decimal point after integers
(setq *nopoint* t)

(defun start-soar nil
	(i-g-v))

(defun nwritn (x)
  (send standard-output ':read-cursorpos ':character))

; For version 4.0
(defmacro class (x) `(car ,x))
(defmacro identifier (x) `(cadr ,x))
(defmacro sattribute (x) `(caddr ,x))
(defmacro svalue (x) `(cadddr ,x))

(defmacro call2 (&body z)
  (list 'ops-call2 (list 'quote z)))

(defmacro d (&body z)
  (list 'ops-d (list 'quote z)))

(defmacro sremove (&body z)
  (list 'ops-sremove (list 'quote z)))

(defmacro po (&body z)
  (list 'ops-po (list 'quote z)))

(defmacro pi (&body z)
  (list 'ops-pi (list 'quote z)))

(defmacro pop-goal (&body z)
  (list 'ops-pop-goal (list 'quote z)))

(defmacro ptrace (&body z)
  (list 'ops-ptrace (list 'quote z)))

(defmacro smatches (&body z)
  (list 'ops-smatches (list 'quote z)))

(defmacro spop (&body z)
  (list 'ops-spop (list 'quote z)))

(defmacro swatch (&body z)
  (list 'ops-swatch (list 'quote z)))

(defmacro tabstop (&body z)
  (list 'ops-tabstop (list 'quote z)))

(defmacro unpbreak (&body z)
  (list 'ops-unpbreak (list 'quote z)))

(defmacro write1 (&body z)
  (list 'ops-write1 (list 'quote z)))

(defmacro write2 (&body z)
  (list 'ops-write2 (list 'quote z)))

(defmacro sp (&body z)
  (list 'ops-sp (list 'quote z)))

(defmacro spm (&body z)
  (list 'ops-spm (list 'quote z)))

(defmacro spo (&body z)
  (list 'ops-spo (list 'quote z)))

(defmacro sppwm (&body z)
  (list 'ops-sppwm (list 'quote z)))

(defmacro spr (&body z)
  (list 'ops-spr (list 'quote z)))

(defmacro swm (&body z)
  (list 'ops-swm (list 'quote z)))

; common lisp ops should fix this with package
;; (defmacro write (&body z)
;;  (list 'ops-write (list 'quote z)))

(defmacro back-trace (&body z)
  (list 'ops-back-trace (list 'quote z)))

(defmacro learn (&body z)
  (list 'ops-learn (list 'quote z)))

(defun back-trace-conditions (current-goal action-list trace-flag)
  (prog (production-traces production-trace previously-traced action actions return-list
         depth)
        (cond (trace-flag
               (soarprint " ")
               (soarprinc "Backtracing to determine conditions")
               (soarprinc " for goal: ")
               (soarprint current-goal)
               (soarprint "Working-memory elements being traced:")
               (soarmapc #'ppline
                (p-conds-to-sp (soarmapcar #'clean-up-element action-list)))
               (soarprint " ")
               (soarprinc "Productions and conditions traced through:")))
        (setq depth 0)
        (setq return-list nil)
        (setq actions action-list)
        (setq previously-traced nil)
        (setq production-traces (get current-goal 'production-trace))
     l0 (cond ((null action-list) (return return-list)))
        (setq action (pop action-list))
        (cond ((eq action 'mark) (setq depth (1- depth)) (go l0))
              ((eq action '-)
               (setq action (pop action-list))
               (cond ((and (neq current-goal (get (svalue action) 'in-id-field))
                           (not (soarmember action return-list)))
                      (trace-new-condition action trace-flag depth '-)
                      (soarpush action return-list)
                      (soarpush '- return-list)))
               (go l0))
              ((and (eq (class action) 'goal-context-info)
                    (or (not (soarmember action *created-list*))
                        (and *smaller-chunks*
                             (not (soarmemq action actions))
                             (soarmember action *subgoal-results*))))
               (cond ((not (soarmember action return-list))
                      (trace-new-condition action trace-flag depth nil)
                      (soarpush action return-list)))
               (go l0))
              ((and (neq (class action) 'goal-context-info)
                    (or (not (soarmemq action *created-list*))
                        (and *smaller-chunks*
                             (not (soarmemq action actions))
                             (soarmemq action *subgoal-results*))))
               (cond ((not (soarmemq action return-list))
                      (trace-new-condition action trace-flag depth nil)
                      (soarpush action return-list)))
               (go l0))
              ((null action) (go l0)))
        (setq production-trace (find-back-production action production-traces))
        (cond ((null production-trace) (go l0)))
        (cond ((soarmemq (car production-trace) previously-traced) (go l0))
              (t (soarpush (car production-trace) previously-traced)))
        (setq action-list (append (cadr production-trace) (cons 'mark action-list)))
        (cond (trace-flag
               (do-tabto (* 3 depth) (trace-file))
               (soarprint (caddr production-trace))))
        (setq depth (1+ depth))
        (go l0)))

(defun broken2 (value)
  (cond ((or (soarlistp value)
             (null value))
         nil)
        ((or (soarmemq (get value 'name) *brknames*)
             (soarmemq value *brknames*)
             (and *brkrun*
                  (eq (get value 'name) *brkrun*))
             (eq value *brkrun*)))))

(defun build-sp-cond (avlist extralist)
  (prog (result)
        (soarmapc
         #'(lambda (x) (setq result (append (car x) (append (nreverse (cdr x)) result))))
         avlist)
        (setq result (append (nreverse extralist) result))
        (return result)))

(defun ops-call2 (z)
  (prog (sym val)
        (cond ((not *in-rhs*)
               (soarwarn "Cannot be called at top level " 'call2)
               (return nil))
              ((not (symbolp (car z)))
               (soarwarn "Call2: illegal argument " (car z))
               (return nil)))
        ($reset)
        (eval-args z)
        (eval (use-result-array))))

(defun cdrmember (element list)
  (prog nil
     l0 (cond ((null list) (return nil))
              ((equal (cdr element) (cdr (pop list))) (return t)))
        (go l0)))

(defun check-decide nil
  (or (eq *p-type* 'decide)
      (soarwarn "Illegal decide in production type " *p-type*)))

(defun check-default-make (z)
  (or (soarmemq *p-type* '(elaborate-once elaborate))
      (soarwarn "Illegal make in production type " *p-type*))
  (and (null z)
       (soarwarn "Arguments missing from make action " z))
  (check-change& z))

(defun check-remove-ops (z)
  (soarwarn "Illegal remove in Soar production" *p-type*)
  (and (null (cdr z))
       (soarwarn "Missing arguments for remove " z))
  (soarmapc #'check-rhs-ce-var (cdr z)))

(defun clean-up-element (element)
  (clean-up-clause element 'nil))

(defun compare-conditions (elm1 elm2)
  (cond ((and (null elm1)
              (null elm2))
         t)
        ((or (null elm1)
             (null elm2))
         nil)
        ((eq (car elm1) (car elm2)) (compare-conditions (cdr elm1) (cdr elm2)))
        ((and (eq (get (car elm1) 'tested) 1)
              (eq (get (car elm2) 'tested) 1))
         (soarpush (car elm1) *suspected-duplicates*)
         (compare-conditions (cdr elm1) (cdr elm2)))
        (t nil)))

(defun compute-choice (choice)
  (cond ((and (get choice 'name)
              (get choice 'instance))
         (concat (get choice 'name) (soarmapcar #'compute-choice (get choice 'instance))))
        ((get choice 'name) (get choice 'name))
        ((get choice 'instance) (soarmapcar #'compute-choice (get choice 'instance)))
        (t choice)))

(defun compute-negation-index (matrix)
  (prog (save-matrix negation-vars negation-list count1 neg-var save-negation-vars
         negated-conditions condition)
        (setq negated-conditions nil)
        (setq negation-vars nil)
        (setq negation-list nil)
        (setq save-matrix matrix)
     l1 (cond (matrix
               (cond ((equal (car matrix) '-)
                      (pop matrix)
                      (soarpush (remove-condition-trash (car matrix)) negated-conditions)
                      (setq negation-vars
                             (merge-not-time (find-neg-variables (car negated-conditions))
                              negation-vars))))
               (pop matrix)
               (go l1)))
        (and (null negated-conditions)
             (return nil))
        (setq count1 0)
     l3 (cond ((or (null save-matrix)
                   (eq condition '-->))
               (return (list negated-conditions negation-list))))
        (setq condition (pop save-matrix))
        (cond ((eq condition '-) (setq save-matrix (cdr save-matrix)) (go l3)))
        (setq save-negation-vars negation-vars)
        (setq negation-vars nil)
     l4 (cond ((null save-negation-vars) (setq count1 (1+ count1)) (go l3)))
        (setq neg-var (pop save-negation-vars))
        (cond ((eq condition '-->) (soarpush neg-var negation-vars))
              ((soarmemq neg-var condition)
               (soarpush (list neg-var count1 (find-position neg-var condition))
                negation-list))
              (t (soarpush neg-var negation-vars)))
        (go l4)))

(defun condition-trace (x)
  (trace-condition-action x 'condition))

(defun conv-input-wme (input)
  (cond ((numberp input) (get-wm (list input)))
        ((and (soarlistp input)
              (neq (car input) 'preference))
         (soarmapconc #'sswm (sp-info-expand nil input nil)))
        ((soarlistp input) (sswm input))
        (t (sswm (append '(^ identifier) (list input))))))

(defun create-goal-info (data-matched goal-id)
  (setq *data-matched* data-matched)
  (setq *first-action* t))

(defun created (g)
  (soarmapc #'print (get g 'created))
  t)

(defun ops-d (dc)
  (cond ((atom dc) (run)) (t (eval (list 'run (car dc) 'd)))))

(defun delprop (name attribute value)
  (soarputprop name (dremove value (get name attribute)) attribute))

(defun eliminate-trailing-nil (wme)
  (prog nil
        (cond ((atom wme) (return wme)) ((soarlast wme) (return wme)))
        (setq wme (dreverse wme))
     l1 (cond ((car wme) (return (dreverse wme))))
        (setq wme (cdr wme))
        (go l1)))

(defun excise-chunks nil
  (prog (chunks)
        (setq chunks (append *chunks* nil))
        (soarmapc #'(lambda (x) (setq *tracep-list* (dremove x *tracep-list*))) chunks)
        (eval (cons 'excise chunks))))

(defun fill-up (wme)
  (fill-up-c wme 'new)
  (sendto 'new (list wme) 'left (list *new-root*)))

(defun fill-up-c (wme flag)
  (prog (dp)
        (setq *alpha-flag-part* flag)
        (setq *alpha-data-part* (list wme))
        (setq dp wme)
        (setq *c1* (car dp))
        (setq dp (cdr dp))
        (setq *c2* (car dp))
        (setq dp (cdr dp))
        (setq *c3* (car dp))
        (setq dp (cdr dp))
        (setq *c4* (car dp))
        (setq dp (cdr dp))
        (setq *c5* (car dp))
        (setq dp (cdr dp))
        (setq *c6* (car dp))
        (setq dp (cdr dp))
        (setq *c7* (car dp))
        (setq dp (cdr dp))
        (setq *c8* (car dp))
        (setq dp (cdr dp))
        (setq *c9* (car dp))
        (setq dp (cdr dp))
        (setq *c10* (car dp))
        (setq dp (cdr dp))
        (setq *c11* (car dp))
        (setq dp (cdr dp))
        (setq *c12* (car dp))
        (setq dp (cdr dp))
        (setq *c13* (car dp))
        (setq dp (cdr dp))
        (setq *c14* (car dp))
        (setq dp (cdr dp))
        (setq *c15* (car dp))
        (setq dp (cdr dp))
        (setq *c16* (car dp))))

(defun fill-up-wm (wm)
  (soarmapc #'fill-up wm))

(defun fill-up2 (wme)
  (fill-up-c (car wme) 'new)
  (sendto 'new (list (car wme)) 'left (list *new-root*)))

(defun find-acceptable-preference (preferences id)
  (prog (preference)
     l0 (cond ((null preferences) (return nil)))
        (setq preference (pop preferences))
        (cond ((and (eq 'preference (class preference))
                    (eq id (identifier preference))
                    (eq 'acceptable (svalue preference)))
               (return preference)))
        (go l0)))

(defun find-action-sets (actions subgoal)
  (prog (return-list new-action-list current-closure moved-action action possible-actions)
        (setq return-list nil)
     l0 (and (null actions)
             (return (nreverse return-list)))
        (setq action (pop actions))
        (setq new-action-list (list action))
        (setq current-closure (find-unbound-symbols action))
        (cond ((null current-closure)
               (soarpush (nreverse new-action-list) return-list)
               (go l0)))
     l2 (setq moved-action nil)
        (setq possible-actions actions)
        (setq actions nil)
     l1 (cond ((and (null possible-actions)
                    moved-action)
               (go l2))
              ((null possible-actions)
               (soarpush (nreverse new-action-list) return-list)
               (go l0)))
        (setq action (pop possible-actions))
        (cond ((intrq (cons (cadr action) (cdddr action)) current-closure)
               (setq current-closure
                      (append (find-unbound-symbols action) current-closure))
               (soarpush action new-action-list)
               (setq moved-action t))
              ((and *smaller-chunks*
                    (neq (car action) 'preference)
                    (intrq (list (cadr action)) current-closure))
               (setq current-closure
                      (append (find-unbound-symbols action) current-closure))
               (soarpush action new-action-list)
               (setq moved-action t))
              ((and *smaller-chunks*
                    (eq (car action) 'preference)
                    (intrq (list (cddddr action)) current-closure))
               (setq current-closure
                      (append (find-unbound-symbols action) current-closure))
               (soarpush action new-action-list)
               (setq moved-action t))
              (t (soarpush action actions)))
        (go l1)))

(defun find-back-production (action traces)
  (cdr (cond ((eq (class action) 'goal-context-info) (soarassoc action traces))
             (t (soarassq action traces)))))

(defun find-max-goal-depth (goal-list)
  (prog (max-depth max-depth-goal goal-depth)
        (setq max-depth -1)
        (setq max-depth-goal nil)
     l0 (cond ((null goal-list) (return max-depth-goal)))
        (setq goal-depth (get (cadar goal-list) 'goal-depth))
        (cond ((and goal-depth
                    (< max-depth goal-depth))
               (setq max-depth goal-depth)
               (setq max-depth-goal (cadar goal-list))))
        (pop goal-list)
        (go l0)))

(defun find-neg-variables (condition)
  (prog (return-list)
        (setq return-list nil)
     l1 (cond ((null condition) (return return-list))
              ((variablep (car condition)) (soarpush (car condition) return-list)))
        (pop condition)
        (go l1)))

(defun find-position (var condition)
  (prog (attribute)
     l1 (cond ((or (null condition)
                   (eq var (car condition)))
               (return (1- (literal-binding-of attribute))))
              ((and (car condition)
                    (predicatep (car condition)))
               (setq condition (cddr condition)))
              ((and (car condition)
                    (eq (car condition) '^))
               (setq attribute (cadr condition))
               (setq condition (cddr condition)))
              ((and (car condition)
                    (soarmemq (car condition) '({ })))
               (pop condition))
              (t (pop condition)))
        (go l1)))

(defun find-selected-result (choice results)
  (prog (result)
        (cond ((numberp choice)
               (cond ((or (< choice 1)
                          (> choice (length results)))
                      (return nil)))
               (return (soarnth (1- choice) results))))
     l1 (setq result (pop results))
        (cond ((test-selected-result choice result)
               (and (null *select-equal*)
                    (setq *select-equal* *default-user-select*))
               (return result))
              ((null results) (return nil)))
        (go l1)))

(defun find-small-action-sets (actions subgoal)
  (prog (action action-pi pi-action-list return-list rest-pi-list)
        (setq pi-action-list nil)
     l1 (cond ((null actions) (return (soarmapcar #'cdr pi-action-list))))
        (setq action (pop actions))
        (setq action-pi
               (car (find-back-production action (get subgoal 'production-trace))))
        (cond ((setq rest-pi-list (soarassq action-pi pi-action-list))
               (rplacd rest-pi-list (cons action (cdr rest-pi-list))))
              (t (soarpush (list action-pi action) pi-action-list)))
        (go l1)))

;** Chris Fedor 11/26/85
;** added (not (numberp value)) test to avoid doing get on a number
(defun find-subgoal-nonresults (subgoal supergoal created-list)
  (prog (change-flag action id nonresult-list goal value)
        (setq *subgoal-results* nil)
     l1 (setq change-flag nil)
        (setq nonresult-list nil)
     l2 (cond ((and (null created-list)
                    (null change-flag))
               (setq *subgoal-results* (nreverse *subgoal-results*))
               (return nonresult-list))
              ((null created-list)
	       (setq created-list (nreverse nonresult-list)) (go l1)))
        (setq action (pop created-list))
        (setq id (identifier action))
        (setq goal (get id 'in-id-field))
        (cond ((or (and (eq (car action) 'preference)
                        (closure-preference action subgoal))
                   (and (neq (car action) 'preference)
                        (neq goal subgoal)))
               (setq change-flag t)
               (soarpush action *subgoal-results*)
               (setq value (svalue action))
	       (cond ((and (eq (car action) 'preference)
			   (eq goal subgoal))
		      (soarpush id *unbound-action-ids*)
		      (and supergoal
			   (soarputprop id supergoal 'in-id-field)))
		     ((and (neq (car action) 'preference)
			   (not (numberp value))
			   (eq (get value 'in-id-field) subgoal))
		      (and supergoal
			   (soarputprop value supergoal 'in-id-field))
		      (soarpush value *unbound-action-ids*)))
	       (and supergoal (addprop supergoal 'created action)))
	      (t (soarpush action nonresult-list)))
	(go l2)))

(defun find-variables (list)
  (cond ((null list) nil)
        ((variablep (car list)) (cons (car list) (find-variables (cdr list))))
        (t (find-variables (cdr list)))))

(defun find-reject-preferences (preferences)
  (prog (preference return-list)
        (setq return-list nil)
     l0 (cond ((null preferences) (return return-list)))
        (setq preference (pop preferences))
        (cond ((and (eq 'preference (class preference))
                    (eq 'reject (svalue preference)))
               (soarpush preference return-list)))
        (go l0)))

(defun find-unbound-symbols (action)
  (prog (return-list)
        (setq return-list nil)
     l1 (cond ((null action) (return return-list))
              ((and (get (car action) 'in-id-field)
                    (soarmemq (car action) *unbound-action-ids*))
               (soarpush (car action) return-list)))
        (pop action)
        (go l1)))

(defun flatten (l)
  (soarmapconc #'(lambda (item) (cond ((atom item) (list item)) (t (flatten item)))) l))

(defun gensyminit nil
  (soarmapc #'soarclearprops *used-gensym-list*)
  (soarmapc #'move-to-free-gensym-list *used-gensym-list*)
  (setq *used-gensym-list* nil))

(defun handle-back-trace-actions (action-list old-context prior-context new-context
                                  subgoaled)
  (prog (duplicate-list copy-list return-value closure condition-list negation-list
         wme-list)
        (clear-var-list)
        (setq wme-list
               (back-trace-conditions (get-current old-context 'goal) action-list
                *ltrace*))
        (setq *p-name* (get-current old-context 'goal))
        (setq *first-action* t)
        (soarmapc
         #'(lambda (x) (save-production-trace x wme-list (get-current new-context 'goal)))
         action-list)
        (cond ((and (not *always-learn*)
                    (or subgoaled
                        (get-context-subgoaled old-context)))
               (return t))
              ((or (not *learning*)
                   (soarmemq (get (get-current old-context 'problem-space) 'name)
                    *chunk-free-problem-spaces*)
                   (not (test-context prior-context)))
               (return nil)))
        (setq closure (find-closure wme-list))
        (soarmapc #'build-variable-list closure)
        (setq *action-closure* (find-action-closure action-list))
        (setq condition-list
               (soarmapcar #'(lambda (x) (clean-up-clause x 'condition)) wme-list))
        (setq action-list (soarmapcar #'clean-up-action action-list))
        (setq condition-list (process-negations condition-list))
        (setq negation-list (cdr condition-list))
        (setq condition-list (build-terse (soarcarsort (car condition-list))))
        (cond ((test-chunk-for-content condition-list)
               (and *ltrace*
                    (soarmapc #'ppline (p-conds-to-sp condition-list)))
               (return nil)))
        (setq duplicate-list
               (compactify-conditions (append condition-list negation-list) action-list))
        (setq copy-list (bind-it *action-list*))
        (setq *action-list* (rebind-action-ids *action-list* copy-list))
        (setq *copy-action-list* *action-list*)
        (setq return-value (build-a-production *condition-list* *action-list* wme-list))
        (and duplicate-list
             (soarmapc #'build-copies duplicate-list))
        (return return-value)))

(defun init-literalizes nil
  (literalize preference object role value reference goal problem-space state operator)
  (literalize goal-context-info identifier attribute value)
  (literalize space-info identifier attribute value)
  (literalize state-info identifier attribute value)
  (literalize op-info identifier attribute value)
  (literalize eval-info identifier attribute value)
  (literalize desired-info identifier attribute value)
  (literalize info identifier attribute value))

(defun init-soar nil
  (setq *in-rhs* nil)
  (sremove)
  (setq *init-wm* nil)
  (setq *conflict-set* nil)
  (setq *wmpart-list* nil)
  (setq *p-name* nil)
  (setq *phase* 'elaborate)
  (setq *prod-count* (setq *decide-count* (setq *cycle-count* (setq *action-count* 0))))
  (setq *total-token* (setq *max-token* (setq *current-token* 0)))
  (setq *first-action* nil)
  (gensyminit)
  (setq *current-production-trace* 'cpt)
  (soarmapc #'soarclearprops *user-ids*)
  (setq *user-ids* nil)
  (and *3600-display*
       (clear-soar2-window))
  (setq *total-cs* (setq *max-cs* 0))
  (setq *total-wm* (setq *max-wm* (setq *current-wm* 0)))
  (setq *elapsed-time* 0)
  (setq *trace-number* 0)
  (setq *context*
         (list (soargensym 'goal)
               'undecided
               'undecided
               'undecided
               1
               'nil
               'nil
               'nil
               'nil
               'nil
               0))
  (setq *context-stack* (list *context*))
  (setq *data-matched* nil))

(defun init-wm nil
  (prog (cg cp cs co)
        (cond ((not *init-wm*)
               (setq *phase* 'decide)
               (setq cg (get-current *context* 'goal))
               (setq cp (get-current *context* 'problem-space))
               (setq cs (get-current *context* 'state))
               (setq co (get-current *context* 'operator))
               (soarputprop cg 1 'goal-depth)
               (and *learning*
                    (learn))
               (make-info 'goal-context-info cg 'problem-space cp)
               (trace-object 'goal cg 0)
               (trace-object 'problem-space cp 0)
               (make-info 'goal-context-info cg 'state cs)
               (trace-object 'state cs 0)
               (make-info 'goal-context-info cg 'operator co)
               (trace-object 'operator co 0)
               (setq *init-wm* t)
               (setq *phase* 'elaborate)))))

(defun intrq (x y)
  (cond ((atom x) nil)
        ((soarmemq (car x) y) (cons (car x) (intrq (cdr x) y)))
        (t (intrq (cdr x) y))))

(defun last-chunk nil
  (eval (cons 'spm (list (car *chunks*)))))

(defun list-chunks nil
  (eval (cons 'spm (reverse *chunks*))))

(defun make-new-root (pname)
  (link-new-node (list '&priv nil pname)))

(defun make-nil-list (list-length)
  (prog (nil-list)
        (setq nil-list '(nil))
     l1 (cond ((eq list-length (length nil-list)) (return nil-list)))
        (soarpush 'nil nil-list)
        (go l1)))

(defun mark-in-id (wme)
  (prog (id att value port)
        (setq port (trace-file))
        (setq id (identifier wme))
        (setq att (sattribute wme))
        (and (not id)
             (return nil))
        (cond ((and (neq *phase* 'decide)
                    (eq 'goal-context-info (class wme))
                    (soarmemq att '(problem-space state operator)))
               (soarwarn "Illegal attempt to modify context" wme)
               (return t))
              ((eq *phase* 'decide) (setq *current-goal* (get-current *context* 'goal)))
              ((and (not *current-goal*)
                    (setq *current-goal* 'undecided)))
              ((and *first-action*
                    (eq (get *p-name* 'type) 'elaborate-once))
               (and (eq (cdr (soarassq *p-name* *already-fired*)) *current-goal*)
                    (return t))
               (soarpush (cons *p-name* *current-goal*) *already-fired*)))
        (cond ((and (not (get id 'gensymed))
                    (not (soarmemq id *user-ids*)))
               (soarpush id *user-ids*)))
        (cond ((not (get id 'in-id-field)) (soarputprop id *current-goal* 'in-id-field)))
        (setq value (svalue wme))
        (cond ((eq att 'name) (soarputprop id value 'name))
              ((soarmember (list (class wme) att) *instance-attributes*)
               (addprop id 'instance value)))
        (cond ((not *never-learn*)
               (save-production-trace wme (reverse *data-matched*) *current-goal*)))
        (cond (*first-action*
               (cond ((and *ptrace*
                           *ttrace*
                           (not *wtrace*)
                           (trace-problem-space?))
                      (soarprinc "-->")))
               (setq *first-remove* t)
               (setq *first-action* nil)))
        (addprop *current-goal* 'created wme)
        (return nil)))

(defun match-update (wme)
  (fill-up-c (car wme) 'new)
  (sendto 'new (list (car wme)) 'left (list *new-root*)))

(defun mem-length (mem)
  (prog (n)
        (setq n 0)
        (soarmapc #'(lambda (ci) (cond ((soarlistp ci) (setq n (+ n 1))))) mem)
        (return n)))

(defun merge-not-time (lista listb)
  (prog (elm)
        (and (null lista)
             (return listb))
        (setq elm (car lista))
        (cond ((null listb) (return lista))
              ((numberp elm) (return (merge-not-time (cdr lista) listb)))
              ((soarmemq elm listb) (return (merge-not-time (cdr lista) listb)))
              (t (return (cons elm (merge-not-time (cdr lista) listb)))))))

(defun most-recent-subgoal (context-stack)
  (cond ((null (cdr context-stack)) (caar context-stack))
        (t (most-recent-subgoal (cadr context-stack)))))

(defun move-to-free-gensym-list (symbol)
  (prog (char sym-list)
        (setq char (soarnthchar symbol 1))
        (setq sym-list (soarassq char *free-gensym-list*))
        (cond ((null sym-list)
               (setq *free-gensym-list*
                      (append *free-gensym-list* (list (list char symbol)))))
              (t (rplacd sym-list (append (list symbol) (cdr sym-list)))))))

(defun multi-attribute (x)
  (prog (class)
        (cond ((soarassq (car x) *sp-classes*)
               (setq class (cdr (soarassq (car x) *sp-classes*))))
              (t (setq class (soarpack2 (car x) '-info))
                 (soarpush (cons (car x) class) *sp-classes*)))
        (cond ((not (soarmemq class *save-class-list*))
               (soarpush class *save-class-list*)
               (soarputprop class (get 'state-info 'ppdat) 'ppdat)))
        (setq x (cdr x))
        (cond ((and (cdr x)
                    (numberp (cadr x))
                    (or (> (cadr x) 99)
                        (< (cadr x) 1)))
               (soarwarn "Illegal multi-attribute value" (cadr x))
               (setq x (list (car x)))))
        (soarpush (cons class x) *multi-attribute*)))

(defun multi-attributes (x)
  (soarmapc #'multi-attribute x))

(defun new-variables (condition stack)
  (prog (first elm)
        (setq first (car stack))
     l1 (setq elm (pop condition))
        (cond ((null condition) (return stack))
              ((and (variablep elm)
                    (neq first elm))
               (soarpush elm stack)))
        (go l1)))

(defun new-variable-list (condition list)
  (prog (elm)
     l1 (setq condition (cdr condition))
        (setq elm (car condition))
        (cond ((null condition) (return list))
              ((and (variablep elm)
                    (not (soarmemq elm list)))
               (soarpush elm list)))
        (go l1)))

(defun ops-sremove (z)
  (cond ((equal z '*) (process-changes nil (get-wm nil)))
        (t (process-changes nil (get-wm z)))))


(defun sswm (avlist)
  (cond ((and (cdddr avlist)
              (variablep (cadddr avlist)))
         (rplacd avlist (cddddr avlist))))
  (cond ((not (conv-to-attr-nums avlist nil)) nil)
        (t (mapwm #'sswm2) (prog2 nil *wm* (setq *wm* nil)))))

(defun sswm2 (elm-tag)
  (cond ((filter (car elm-tag)) (soarpush (car elm-tag) *wm*))))

(defun pgs nil
  (pgs-frame *context-stack* 1 nil nil nil)
  (soarprintc "Decision cycle ")
  (soarprinc *decide-count*)
  (terpri (trace-file)))

(defun pgs-frame (context-stack depth difficulty results second-time)
  (prog (slots slot id context difficulty2 super-op result port)
        (setq port (trace-file))
        (setq context (pop context-stack))
        (cond ((null context) (return nil)) (second-time (terpri (trace-file))))
        (setq super-op (get-context-super-operator context))
        (setq difficulty2
               (list (get-context-difficulty context) (get-context-slot context)))
        (setq result (get-context-results context))
        (setq slots '(g p s o))
     l2 (cond (slots
               (setq id (pop context))
               (setq slot (pop slots))
               (cond ((neq id 'undecided)
                      (terpri port)
                      (cond (*subgoal-tabs* (do-tabto (+ 2 (* depth 3)) port))
                            (t (do-tabto 5 port)
                               (soarprinc3 "(" (1- depth) ")")
                               (do-tabto 11 port)))
                      (soarprinc2 slot ": ")
                      (print-id id)
                      (setq difficulty nil)))
               (go l2)))
        (setq second-time nil)
        (soarmapc
         #'(lambda (x)
             (pgs-frame x (1+ depth) difficulty2 result second-time)
             (setq second-time t))
         context-stack)))

(defun ops-po (object)
  (print-object (car object) (cadr object)))


(defun ops-pi (z)
  (print-partial-instantiation (car z) (cond ((cdr z) (cadr z)) (t 1))))


(defun ppi3 (p i nodes ce part)
  (prog (saved-mems n smatch-flg)
        (cond ((null nodes)
               (setq n 0)
               (soarmapc
                #'(lambda (ci)
                    (cond ((eq (car ci) p)
                           (setq n (+ n 1))
                           (cond ((equal n i)
                                  (soarmapc
                                   #'(lambda (wme) (eval (list 'wm (creation-time wme))))
                                   (reverse (cdr ci)))
                                  (terpri (trace-file)))))))
                *conflict-set*)
               (terpri (trace-file))
               (return ce))
              ((null (setq saved-mems (find-left-mem (car nodes)))) (return nil))
              ((not (setq smatch-flg (ppi3 p i (cdr nodes) (1+ ce) (cons ce part))))
               (soarmapc #'(lambda (ci) (eval (list 'wm (creation-time ci))))
                (reverse (soarnth (1- i) saved-mems)))
               (terpri (trace-file))
               (return ce)))
        (return smatch-flg)))

(defun ops-pop-goal (goals)
  (cond ((null goals)
         (soarmapc
          #'(lambda (x)
              (pop-subgoals x (most-recent-subgoal *context-stack*) *context-stack*
               (car *context-stack*)))
          (cdr *context-stack*)))
        (t (soarmapc
            #'(lambda (y)
                (soarmapc
                 #'(lambda (x) (pop-subgoals x y *context-stack* (car *context-stack*)))
                 (cdr *context-stack*)))
            goals)))
  nil)


(defun pop-subgoals (stack g prior-stack prior-context)
  (cond (stack
         (cond ((eq (caar stack) g)
                (remove-subgoal-objects stack)
                (rplacd prior-stack nil)
                (rplaca (soarnthcdr 4 prior-context) 0)
                (rplaca (soarnthcdr 5 prior-context) nil))
               (t (soarmapc #'(lambda (x) (pop-subgoals x g stack (car stack)))
                   (cdr stack)))))))

(defun print-choice (choice)
  (print-id choice)
  (soarprint " "))

(defun print-id (id)
  (soarprinc2 id " ")
  (cond ((and id
              (atom id)
              (neq id 'undecided))
         (and (get id 'name)
              (soarprinc (get id 'name)))
         (setq *print-attribute-list* (list id))
         (print-instance id)
         (setq *print-attribute-list* nil))))

(defun print-instance (id)
  (prog (flag)
        (cond ((get id 'instance)
               (setq flag nil)
               (soarprinc "(")
               (soarmapc
                #'(lambda (object)
                    (and flag
                         (soarprinc " "))
                    (setq flag t)
                    (cond ((get object 'name)
                           (soarprinc (get object 'name))
                           (cond ((not (soarmemq object *print-attribute-list*))
                                  (soarpush object *print-attribute-list*)
                                  (print-instance object))))
                          (t (soarprinc object)
                             (cond ((not (soarmemq object *print-attribute-list*))
                                    (soarpush object *print-attribute-list*)
                                    (print-instance object))))))
                (get id 'instance))
               (soarprinc ")")))))

(defun print-instantiation (p i)
  (prog (n)
        (setq n 0)
        (soarmapc
         #'(lambda (inst)
             (cond ((eq (car inst) p)
                    (setq n (1+ n))
                    (cond ((equal n i)
                           (soarmapc
                            #'(lambda (wme) (eval (list 'wm (creation-time wme))))
                            (reverse (cdr inst)))
                           (terpri (trace-file)))))))
         *conflict-set*)))

(defun print-partial-instantiation (p i)
  (ppi3 p i (get p 'backpointers) 2 (ncons 1)))

(defun printline (x)
  (soarmapc #'soarprinc x)
  (terpri (trace-file)))

(defun print-object (object depth)
  (cond ((and *3600-display*) (draw-current-contents object))
        (t (eval (append '(ppwm ^ identifier) (list object))))))

(defun print-status nil
  (soarprintc "Learn status: ")
  (cond (*learning* (soarprinc "on "))
        (*never-learn* (soarprinc "never "))
        (t (soarprinc "off ")))
  (cond (*always-learn* (soarprinc "always ")) (t (soarprinc "bottom-up ")))
  (cond ((eq *print-learn* 0) (soarprinc "print "))
        (*print-learn* (soarprinc "full-print "))
        (t (soarprinc "noprint ")))
  (cond ((and *ltrace* *tracep*) (soarprint "full-trace "))
        (*tracep* (soarprint "trace "))
        (t (soarprint "notrace ")))
  t)

(defun process-instance (instance)
  (prog nil
        (setq *first-action* t)
        (accum-stats)
        (eval-rhs (car instance) (cdr instance))
        (check-limits)
        (and (broken (car instance))
             (setq *break-flag* t))))

(defun process-negations (conditions)
  (prog (return-conditions element negations)
        (setq negations nil)
     l2 (cond ((null conditions)
               (setq negations (variablize-negations negations))
               (and negations
                    (return (cons (nreverse return-conditions)
                                  (nreverse (tersify-negations negations)))))
               (return (list (nreverse return-conditions))))
              ((eq (car conditions) '-)
               (pop conditions)
               (soarpush (pop conditions) negations))
              (t (soarpush (pop conditions) return-conditions)))
        (go l2)))

(defun ops-ptrace (arg)
  (cond ((atom arg) *tracep-list*)
        (t (setq *tracep-list*
                  (append *tracep-list*
                          (soarmapcar
                           #'(lambda (x)
                               (cond ((or (numberp x)
                                          (soarlistp x))
                                      (car (conv-input-wme x)))
                                     (t x)))
                           arg))))))


(defun remove-condition-attributes (condition)
  ($reset)
  (cond ((neq (cadr condition) '^)
         (eval-args (append (cons (car condition) '(^ identifier)) (cdr condition))))
        (t (eval-args condition)))
  (use-result-array))

(defun remove-condition-trash (condition)
  (prog (return-condition pand-flag disjunction-flag)
        (setq return-condition nil)
        (setq pand-flag nil)
        (setq disjunction-flag nil)
     l1 (cond ((null condition) (return (nreverse return-condition)))
              ((eq (car condition) '<<) (pop condition) (setq disjunction-flag t))
              ((eq (car condition) '>>) (pop condition) (setq disjunction-flag nil))
              (disjunction-flag (pop condition))
              ((eq (car condition) '{) (pop condition) (setq pand-flag t))
              ((eq (car condition) '})
               (pop condition)
               (and pand-flag
                    (soarpush (soargenvar 'z) return-condition))
               (setq pand-flag nil))
              ((predicatep (car condition)) (setq condition (cddr condition)))
              (t (soarpush (pop condition) return-condition) (setq pand-flag nil)))
        (go l1)))

(defun remove-current (goal slot old-value new-value)
  (prog nil
        (cond ((soarlistp old-value)
               (soarmapc
                #'(lambda (x) (remove-from-wm (list 'goal-context-info goal slot x)))
                old-value))
              (t (remove-from-wm (list 'goal-context-info goal slot old-value))))
        (return (cond ((soarlistp new-value)
                       (soarmapcar #'(lambda (x) (list 'goal-context-info goal slot x))
                        new-value))
                      (t (list (list 'goal-context-info goal slot new-value)))))))

(defun remove-subgoal-objects (stack)
  (cond (stack
         (soarmapc #'remove-subgoal-objects (cdr stack))
         (soarmapc #'remove-from-wm (get (caar stack) 'created))
         (soarclearprops (caar stack)))))

(defun restart-soar nil
  (prog (pnames)
        (setq pnames (append *pnames* nil))
        (eval (cons 'excise pnames))
        (soarmapc #'soarclearprops
         (append *save-class-list* *class-list* *used-gensym-list*))
        (i-g-v)))

(defun save-production-trace (wme data-matched current-goal)
  (cond (data-matched
         (cond ((and *first-action* data-matched)
                (setq *current-production-trace*
                       (list (setq *trace-number* (1+ *trace-number*))
                             data-matched
                             *p-name*))))
         (addprop current-goal 'production-trace (cons wme *current-production-trace*))
         wme)))

(defun showload (file)
  (prog (in nextexpr)
        (setq in (openfile file 'input))
     l1 (setq nextexpr (read in))
        (cond ((eq nextexpr 'stop) (return)))
        (soarprinc "----> ")
        (soarprinc nextexpr)
        (terpri (trace-file))
        (soarprinc (eval nextexpr))
        (terpri (trace-file))
        (go l1)))

(defun ops-smatches (rule-list)
  (soarmapc #'smatches2 rule-list)
  (terpri (trace-file)))


(defun smatches2 (p)
  (cond ((atom p)
         (terpri (trace-file))
         (spprule p (smatches3 p (get p 'backpointers) 2 (ncons 1))))))

(defun smatches3 (p nodes ce part)
  (prog (saved-mems n smatch-flg temp-flg port)
        (setq port (trace-file))
        (cond ((null nodes)
               (terpri (trace-file))
               (cond ((not (soarassq p *conflict-set*)) (return nil)))
               (soarprinc3 "** MATCHES FOR " (reverse part) " ** ")
               (setq n 0)
               (soarmapc
                #'(lambda (ci)
                    (cond ((eq (car ci) p) (swrite-elms (cdr ci)) (setq n (+ n 1)))))
                *conflict-set*)
               (terpri (trace-file))
               (return (ncons n)))
              (t (setq saved-mems (find-left-mem (car nodes)))
                 (cond ((null saved-mems) (return nil)))
                 (setq temp-flg (smatches3 p (cdr nodes) (1+ ce) (cons ce part)))
                 (setq smatch-flg
                        (cons (mem-length saved-mems)
                              (cons (mem-length (find-right-mem (car nodes))) temp-flg)))
                 (cond ((not temp-flg)
                        (terpri (trace-file))
                        (soarprinc3 "** MATCHES FOR " (reverse part) " ** ")
                        (soarmapc #'swrite-elms saved-mems)
                        (terpri (trace-file))
                        (soarprinc3 "** MATCHES FOR " ce " ** ")
                        (terpri (trace-file))
                        (soarmapc
                         #'(lambda (x)
                             (cond ((soarlistp x) (soarmapc #'write-elms2 (reverse x)))))
                         (find-right-mem (car nodes)))
                        (terpri (trace-file))
                        (return smatch-flg)))
                 (return smatch-flg)))))

(defun soarprinc (warning)
  (princ warning (trace-file)))

(defun soarprinc1 (x1 x2)
  (princ x1 (trace-file))
  (princ x2 (trace-file)))

(defun soarprinc2 (x1 x2)
  (princ x1 (trace-file))
  (princ x2 (trace-file)))

(defun soarprinc3 (x1 x2 x3)
  (princ x1 (trace-file))
  (princ x2 (trace-file))
  (princ x3 (trace-file)))

(defun soarprint (warning)
  (prog2 nil (princ warning (trace-file)) (terpri (trace-file))))

(defun soarprintc (warning)
  (terpri (trace-file))
  (princ warning (trace-file)))

(defun soarwarn (warning warning2)
  (prog (port)
        (and (null *warning*)
             (return warning2))
        (setq port (trace-file))
        (terpri port)
        (princ "Warning: " port)
        (and *p-name*
             (princ *p-name*))
        (princ ".." port)
        (princ warning port)
        (princ " " port)
        (princ warning2 port)
        (return warning2)))

(defun ops-spop (object)
  (setq *indent* 0)
  (setq *print-spo-list* nil)
  (cond ((or (null object)
             (numberp (car object))))
        ((not (numberp (soarlast object)))
         (terpri (trace-file))
         (spo1 (car object) *spo-default-depth* t)
         (eval (cons 'spop (cdr object))))
        (t (terpri (trace-file))
           (spo1 (car object) (soarlast object) t)
           (eval (cons 'spop (cdr object)))))
  (setq *print-spo-list* nil))


(defun spprule (name cond-nums)
  (prog (matrix next lab cond-flg port)
        (and (not (symbolp name))
             (return nil))
        (setq matrix (get name 'production))
        (and (null matrix)
             (return nil))
        (setq port (trace-file))
        (setq cond-flg nil)
        (terpri port)
        (soarprinc3 "(" 'p " ")
        (soarprinc3 name " " (get name 'type))
    top (and (atom matrix)
             (go fin))
        (setq next (car matrix))
        (setq matrix (cdr matrix))
        (setq lab nil)
        (terpri port)
        (cond ((and cond-nums
                    (> (car cond-nums) 0))
               (princ (car cond-nums) port)
               (do-tabto 5 (trace-file))
               (setq cond-nums (cddr cond-nums)))
              ((eq next '-->) (princ "    " port) (setq cond-flg t))
              (cond-flg (princ "    " port))
              (t (princ ">>>>" port) (setq cond-flg t)))
        (cond ((eq next '-)
               (princ "- " port)
               (setq next (car matrix))
               (setq matrix (cdr matrix)))
              ((neq next '-->) (princ "  " port)))
        (ppline next)
        (cond (lab (soarprinc3 " " lab '})))
        (go top)
    fin (soarprint ")")))

(defun swrite-elms (wme-or-count)
  (cond ((soarlistp wme-or-count)
         (terpri (trace-file))
         (soarmapc #'write-elms2 (reverse wme-or-count)))))

(defun soargenpname (x)
  (prog (sym-list sym)
        (cond (*free-pname-list* (setq sym (pop *free-pname-list*)))
              (t (setq sym (soargensymbol (soarnthchar x 1)))))
        (soarputprop sym t 'gensymed)
        (return sym)))

(defun soargensym (x)
  (prog (sym-list charx sym)
        (setq charx (soarnthchar x 1))
        (setq sym-list (soarassq charx *free-gensym-list*))
        (cond ((null (cdr sym-list)) (setq sym (soargensymbol charx)))
              (t (setq sym (cadr sym-list)) (rplacd sym-list (cddr sym-list))))
        (soarputprop sym t 'gensymed)
        (soarpush sym *used-gensym-list*)
        (return sym)))

(defun soargenvar (id)
  (prog (first-char gen-number new-var)
        (setq first-char (soarnthchar id 1))
        (cond ((soarmemq first-char *used-char-list*)
               (setq gen-number (get first-char 'var-count)))
              (t (soarpush first-char *used-char-list*) (setq gen-number 1)))
        (setq new-var (soarpack '< first-char gen-number '>))
        (soarpush new-var *used-var-list*)
        (soarputprop first-char (1+ gen-number) 'var-count)
        (return new-var)))

(defun ops-swatch (z)
  (cond ((equal z '(0)) (setq *gtrace* nil) (setq *strace* nil) (setq *dtrace* nil) 0)
        ((equal z '(1)) (setq *gtrace* t) (setq *strace* nil) (setq *dtrace* nil) 1)
        ((equal z '(2)) (setq *gtrace* t) (setq *strace* t) (setq *dtrace* nil) 2)
        ((equal z '(3)) (setq *gtrace* t) (setq *strace* t) (setq *dtrace* t) 3)
        ((and (atom z)
              (null *gtrace*))
         0)
        ((and (atom z)
              (null *strace*))
         1)
        ((and (atom z)
              (null *dtrace*))
         2)
        ((atom z) 3)
        (t 'what?)))


(defun t-in-list? (x)
  (soarmemq 't (flatten x)))

(defun ops-tabstop (z)
  (prog (val)
        (cond ((not *in-rhs*)
               (soarwarn "TABSTOP can not be called at the top level" " ")
               (return nil))
              ((neq (length z) 1)
               (soarwarn "Wrong number of arguments for Tabstop:" z)
               (return nil))
              ((not (variablep (car z)))
               (soarwarn "Illegal argument for Tabstop:" (car z))
               (return nil))
              ((equalp (length z) 1)
               (setq val (+ 3 (* (get-context-depth *context*) 3)))))
        (make-var-bind (car z) val)))


(defun test-connected-actions (actions)
  (prog (changed-flag action save-action saved-actions found-variable)
        (setq changed-flag t)
        (setq saved-actions (cdr actions))
     l1 (setq actions saved-actions)
        (setq saved-actions nil)
        (cond ((null actions) (return))
              ((null changed-flag)
               (soarwarn "Unconnected actions in production" (p-to-sp actions))
               (return)))
        (setq changed-flag nil)
     l2 (cond ((null actions) (go l1)))
        (setq action (pop actions))
        (cond ((neq (car action) 'make) (go l2)))
        (setq save-action action)
        (setq found-variable nil)
     l3 (cond ((and (null action)
                    found-variable)
               (soarpush save-action saved-actions)
               (go l2))
              ((null action) (go l2))
              ((soarmemq (car action) *condition-vars*)
               (setq *condition-vars*
                      (append *condition-vars* (find-variables save-action)))
               (setq changed-flag t)
               (setq found-variable t)
               (go l2))
              ((variablep (car action)) (setq found-variable t)))
        (pop action)
        (go l3)))

(defun test-preference-field (field subgoal)
  (or (null field)
      (eq field 'undecided)
      (neq (get field 'in-id-field) subgoal)))

(defun test-selected-result (choice result)
  (or (eq choice result)
      (eq choice (get result 'name))
      (and (soarmemq choice (get result 'instance))
           t)
      (and (get result 'instance)
           (soarmemq t
            (flatten
             (soarmapcar #'(lambda (x) (test-selected-result choice x))
              (get result 'instance))))
           t)))

(defun test-subset (lista listb)
  (cond ((null lista) t)
        ((soarmemq (car lista) listb) (test-subset (cdr lista) listb))
        (t nil)))

(defun teqnilb (x y)
  (or (eq x y)
      (null x)))

(defun trace-attribute (x)
  (prog (class)
        (cond ((soarassq (car x) *sp-classes*)
               (soarpush (cons (cdr (soarassq (car x) *sp-classes*)) (cdr x))
                *instance-attributes*))
              (t (setq class (soarpack2 (car x) '-info))
                 (soarpush (cons (car x) class) *sp-classes*)
                 (soarpush class *save-class-list*)
                 (soarputprop class (get 'state-info 'ppdat) 'ppdat)
                 (soarpush (cons class (cdr x)) *instance-attributes*)))))

(defun trace-attributes (x)
  (soarmapc #'trace-attribute x))

(defun trace-condition-action (wme flag)
  (cond ((null *tracep-list*))
        ((or (soarmember wme *tracep-list*)
             (soarmemq (identifier wme) *tracep-list*)
             (soarmemq (get (identifier wme) 'name) *tracep-list*))
         (soarprintc *p-name*)
         (cond ((eq flag 'action) (soarprinc ": -->")) (t (soarprinc ": matches ")))
         (ppline (car (p-conds-to-sp (list (clean-up-clause wme 'action))))))))

(defun trace-ids (item)
  (print-id item))

(defun trace-new-condition (action trace-flag depth negation)
  (cond ((and trace-flag negation)
         (do-tabto (* 3 depth) (trace-file))
         (ppline (cons negation (car (p-conds-to-sp (list (clean-up-element action))))))
         (soarprint " "))
        (trace-flag
         (do-tabto (* 3 depth) (trace-file))
         (ppline (car (p-conds-to-sp (list (clean-up-element action)))))
         (soarprint " "))))

(defun trace-object (type id depth)
  (cond ((or (and (not *gtrace*)
                  (not *otrace*))
             (not (trace-problem-space?))
             (eq id 'undecided)))
        ((and *otrace* id)
         (cond ((and *3600-display* *gtrace*) (draw-current-object type id depth)))
         (soarprintc *decide-count*)
         (cond ((and *subgoal-tabs*
                     (neq depth 0)
                     (eq type 'goal))
                (do-tabto (+ 2 (* depth 3)) (trace-file))
                (princ '==> (trace-file)))
               (*subgoal-tabs* (do-tabto (+ 5 (* depth 3)) (trace-file)))
               (t (do-tabto 5 (trace-file))
                  (soarprinc3 "(" depth ")")
                  (do-tabto 11 (trace-file))))
         (soarprinc2
          (cond ((eq type 'goal) 'g)
                ((eq type 'problem-space) 'p)
                ((eq type 'state) 's)
                ((eq type 'operator) 'o))
          ": ")
         (print-id id))))

(defun trace-problem-space? nil
  (not (soarmemq (get (get-current *context* 'problem-space) 'name)
        *watch-free-problem-spaces*)))

(defun ops-unpbreak (z)
  (cond ((atom z) (setq *brkpts* nil) (setq *brknames* nil) nil)
        (t (soarmapc #'unpbreak2 z) nil)))


(defun unpbreak2 (rule)
  (prog (type blist)
        (cond ((or (not (symbolp rule))
                   (soarlistp rule))
               (soarwarn "Illegal argument:" rule))
              ((soarmemq rule *brkpts*) (setq *brkpts* (rematm rule *brkpts*)) rule)
              ((soarmemq rule *brknames*) (setq *brknames* (rematm rule *brknames*)) rule)
              (t nil))))

(defun unptrace nil
  (setq *tracep-list* nil))

(defun untest (element)
  (cond ((and element
              (variablep (caddr element)))
         (soarputprop (caddr element) (1- (get (caddr element) 'tested)) 'tested)
         (untest (cdddr element)))
        (element (untest (cdddr element)))))

(defun variablize-negations (negations)
  (prog (condition return-list new-condition id value)
        (setq return-list nil)
     l1 (cond ((null negations) (return return-list)))
        (setq condition (pop negations))
        (setq new-condition (list (pop condition)))
     l2 (cond ((null condition)
               (setq new-condition (nreverse new-condition))
               (and (soarmember new-condition return-list)
                    (go l1))
               (soarpush new-condition return-list)
               (go l1)))
        (soarpush (pop condition) new-condition)
        (soarpush (pop condition) new-condition)
        (setq id (pop condition))
        (setq value (cdr (soarassq id *learn-ids*)))
        (cond (value
               (soarputprop value
                (1+ (or (get value 'tested)
                        0))
                'tested))
              (t (setq value id)))
        (soarpush value new-condition)
        (go l2)))

(defun write-trace (arg1 arg2 arg3)
  (cond ((and *ptrace*
              (trace-problem-space?))
         (soarprintc *decide-count*)
         (soarprinc3 '":" *cycle-count* arg1)
         (soarprinc3 arg2 '" " arg3)
         (soarprinc '" "))))

(defun ops-write1 (z)
  (prog (port max k x needspace)
        (cond ((not *in-rhs*)
               (soarwarn "Write1 cannot be called at the top level." " ")
               (return nil)))
        ($reset)
        (eval-args z)
        (setq k 1)
        (setq max ($parametercount))
        (cond ((< max 1) (soarwarn "Write1: nothing to print:" z) (return nil)))
        (setq port (default-write-file))
        (setq x ($parameter 1))
        (cond ((and (symbolp x)
                    ($ofile x))
               (setq port ($ofile x))
               (setq k 2)))
        (setq needspace t)
     la (and (> k max)
             (return nil))
        (setq x ($parameter k))
        (cond ((equal x 'crlf) (setq needspace nil) (terpri port))
              ((equal x 'rjust)
               (setq k (+ 2 k))
               (do-rjust ($parameter (1- k)) ($parameter k) port))
              ((equal x 'tabto)
               (setq needspace nil)
               (setq k (1+ k))
               (do-tabto ($parameter k) port))
              (t (and needspace
                      (princ " " port))
                 (setq needspace t)
                 (princ x port)))
        (setq k (1+ k))
        (go la)))


(defun ops-write2 (z)
  (prog (port max k x)
        (cond ((not *in-rhs*)
               (soarwarn "Write2 cannot be called at the top level" " ")
               (return nil)))
        ($reset)
        (eval-args z)
        (setq k 1)
        (setq max ($parametercount))
        (cond ((< max 1) (soarwarn "Write2: nothing to print" " ") (return nil)))
        (setq port (default-write-file))
        (setq x ($parameter 1))
        (cond ((and (symbolp x)
                    ($ofile x))
               (setq port ($ofile x))
               (setq k 2)))
     la (and (> k max)
             (return nil))
        (setq x ($parameter k))
        (cond ((equal x 'crlf) (terpri port))
              ((equal x 'rjust)
               (setq k (+ 2 k))
               (do-rjust ($parameter (1- k)) ($parameter k) port))
              ((equal x 'tabto) (setq k (1+ k)) (do-tabto ($parameter k) port))
              (t (princ x port)))
        (setq k (1+ k))
        (go la)))


(defun compose-p-conds (cl)
  (soar-do
   ((avlist) (c) (cl1 cl (skip-whole-cond cl1)) (type (car (get-pos-cond cl)))
    (id-var (get-term (cdddr (get-pos-cond cl)))))
   ((null cl1) (cons (conv-type-to-sp type) (append id-var (build-sp-cond avlist nil))))
   (setq c (get-pos-cond cl1)) (cond ((eq (car c) 'make) (setq c (cdr c))))
   (soar-do ((c1 (cddr c) (cdr c1)) (attr '*undefined**) (val '*undefined**) (x))
    ((null c1)
     (cond ((not (eq attr '*undefined**))
            (setq x (soarassoc attr avlist))
            (cond ((not (eq val '*undefined**))
                   (cond (x (rplacd x (append val (cdr x))))
                         (t (soarpush (cons attr val) avlist))))
                  (t (soarpush (list attr) avlist))))
           ((not (eq val '*undefined**)) (soarpush val extravals))))
    (setq label (pop c1))
    (cond ((eq label 'attribute)
           (setq attr (cons '^ (get-term c1)))
           (and (eq (car cl1) '-)
                (soarpush '- attr)))
          ((eq label 'value) (setq val (nreverse (get-term c1)))))
    (pop-term c1))))

(defun conv-to-attr-nums (avlist warnflag)
  (prog (next a)
        (setq *filters* nil)
        (setq next 1)
      l (and (atom avlist)
             (return t))
        (setq a (car avlist))
        (setq avlist (cdr avlist))
        (cond ((eq a '^)
               (setq next (car avlist))
               (setq avlist (cdr avlist))
               (setq next ($litbind next))
               (cond ((or (not (numberp next))
                          (> next *size-result-array*)
                          (> 1 next))
                      (and warnflag
                           (soarwarn "Illegal index after ^" next))
                      (return nil))))
              ((variablep a) (soarwarn "(S)PPWM does not take variables:" a) (return nil))
              (t (setq *filters* (cons next (cons a *filters*))) (setq next (1+ next))))
        (go l)))

(defun conv-type-to-sp (type)
  (soar-do ((cl *sp-classes* (cdr cl))) ((null cl) type)
   (cond ((eq (cdar cl) type) (return (caar cl))))))

(defun get-ids (infolist oldids)
  (prog (newids pair)
        (soarmapc
         #'(lambda (x)
             (setq pair (cons (cadddr x) (car x)))
             (cond ((and (or (not oldids)
                             (soarmember pair oldids))
                         (not (soarmember pair newids)))
                    (soarpush pair newids))))
         infolist)
        (return newids)))

(defun get-pos-cond (cl)
  (cond ((eq (car cl) '-) (cadr cl)) (t (car cl))))

(defun get-similar-conds (cond cl)
  (prog (rest result)
        (setq rest (cons nil cl))
        (soar-do
         ((type (car cond)) (id-var (get-term (cdddr cond))) (negflag) (cl1 rest) (c))
         ((or (null cl)
              (eq (car cl) '-->)))
         (setq c (pop cl))
         (cond ((eq c '-) (setq c (pop cl)) (setq negflag t)) (t (setq negflag nil)))
         (cond ((eq (car c) 'make) (setq c (cdr c))))
         (cond ((and (equal id-var (get-term (cdddr c)))
                     (eq type (car c)))
                (cond (negflag (soarpush '- result) (rplacd cl1 (cddr cl1))))
                (soarpush c result)
                (rplacd cl1 (cddr cl1)))
               (t (cond (negflag (setq cl1 (cdr cl1)))) (setq cl1 (cdr cl1)))))
        (return (cons (nreverse result) (cdr rest)))))

(defun get-term (c)
  (list-head c (skip-term c)))

(defun indent nil
  (soar-do ((i *indent* (- i 1))) ((= i 0)) (princ " " (trace-file))))

(defun list-head (whole tail)
  (soar-do ((result)) ((null whole) (nreverse result))
   (cond ((eq whole tail) (return (nreverse result)))
         (t (setq result (cons (car whole) result)) (setq whole (cdr whole))))))

(defun p-conds-to-sp (cl)
  (prog (c pref cl1 result temp negs type)
        (setq cl1 (copy cl))
     l1 (setq c (get-pos-cond cl1))
        (cond ((or (null c)
                   (eq c '-->))
               (return (nreverse result)))
              ((eq (car c) 'make) (setq c (cdr c))))
        (setq type (car c))
        (cond ((soarmemq type *ops5-actions*) (soarpush c result) (setq cl1 (cdr cl1)))
              ((or (eq type (conv-type-to-sp type))
                   (not (eq 'identifier (caddr c))))
               (cond ((equal (car cl1) '-) (soarpush '- result)))
               (cond ((and (eq (caddr c) 'object)
                           (eq type 'preference))
                      (soarpush (cons 'preference (cdddr c)) result))
                     (t (soarpush c result)))
               (setq cl1 (skip-whole-cond cl1)))
              (t (setq temp (get-similar-conds c cl1))
                 (setq cl1 (cdr temp))
                 (soarpush (compose-p-conds (car temp)) result)))
        (go l1)))

(defun p-to-sp (rule)
  (prog (result)
        (setq result (p-conds-to-sp rule))
        (cond ((soarmemq '--> rule)
               (setq result
                      (append result
                              (cons '--> (p-conds-to-sp (cdr (soarmemq '--> rule))))))))
        (return result)))

(defun pp-class-ids (ids)
  (soarmapc
   #'(lambda (x)
       (soarmapc #'(lambda (y) (ppline y) (terpri (trace-file)))
        (p-conds-to-sp (sppwm1 (list (cdr x) '^ 'identifier (car x))))))
   ids))

(defun ppelm1 (elm)
  (prog (ppdat result val att vlist)
        (setq ppdat (get (car elm) 'ppdat))
        (setq result (list (car elm)))
        (setq vlist (cdr elm))
        (soar-do ((curpos 2 (+ 1 curpos))) ((atom vlist)) (setq val (car vlist))
         (setq vlist (cdr vlist)) (setq att (cdr (soarassq curpos ppdat)))
         (cond ((or (not (eq (car elm) 'preference))
                    (not (null val)))
                (soarpush '^ result)
                (soarpush att result)
                (soarpush val result))))
        (return (nreverse result))))

(defun skip-term (c)
  (car (classify-term c)))

(defun skip-whole-cond (cl)
  (cond ((eq (car cl) '-) (cddr cl)) (t (cdr cl))))

(defun ops-sp (xs)
  (setq *xs* xs)
  (soarcatch '!error! (eval (reorder-p-conds (sp-to-p *xs*))))
  nil)


(defun sp-form-expand (pname x in-actions follows-negation)
  (prog (nf new-class)
        (cond ((and (not (atom x))
                    (eq (car x) 'make))
               (setq x (cdr x))))
        (setq nf
               (cond ((atom x) (list x))
                     ((soarassq (class x) *sp-classes*)
                      (setq new-class (cdr (soarassq (class x) *sp-classes*)))
                      (cond ((not (soarmemq new-class *save-class-list*))
                             (soarpush new-class *save-class-list*)
                             (soarputprop new-class (get 'state-info 'ppdat) 'ppdat)))
                      (sp-info-expand pname x in-actions))
                     ((and (cdr x)
                           (not (eq (class x) 'preference))
                           (not (eq (cadr x) '^))
                           (or (not in-actions)
                               (not (soarmemq (class x) *ops5-actions*))))
                      (setq new-class (soarpack2 (class x) '-info))
                      (soarpush (cons (class x) new-class) *sp-classes*)
                      (soarpush new-class *save-class-list*)
                      (soarputprop new-class (get 'state-info 'ppdat) 'ppdat)
                      (sp-info-expand pname x in-actions))
                     ((and in-actions
                           (not (soarmemq (car x) *ops5-actions*)))
                      (cond ((and (cdr x)
                                  (not (eq (cadr x) '^))
                                  (not (eq (cadr x) '-)))
                             (list (cons 'make
                                         (cons (car x)
                                               (cons '^
                                                     (cons
                                                      (cond
                                                       ((eq (car x) 'preference) 'object)
                                                       (t 'identifier))
                                                      (cdr x)))))))
                            (t (list (cons 'make x)))))
                     ((and in-actions
                           (soarmemq (car x) *ops5-actions*))
                      (list x))
                     ((and (cdr x)
                           (not (eq (cadr x) '^))
                           (not (eq (cadr x) '-)))
                      (list (cons (car x)
                                  (cons '^
                                        (cons (cond ((eq (car x) 'preference) 'object)
                                                    (t 'identifier))
                                              (cdr x))))))
                     (t (list x))))
        (cond ((and follows-negation
                    (> (length nf) 1))
               (%error "Attempt to negate a compound object" pname))
              (t (return nf)))))

(defun sp-info-expand (pname form in-actions)
  (prog (avlist newform end-atom type id clist pand-id temp-form negated no-value)
        (setq pand-id nil)
        (setq clist nil)
        (setq type (cdr (soarassq (car form) *sp-classes*)))
        (cond ((variablep (cadr form)) (setq id (cadr form)) (setq avlist (cddr form)))
              ((equal (cadr form) '^)
               (setq id (soarpack '< 'x (soargensym (car form)) '>))
               (setq avlist (cdr form)))
              ((equal (cadr form) '{)
               (setq pand-id t)
               (setq avlist (cddr form))
               (setq id (ncons '{))
               (soarwhile
                (and avlist
                     (not (equal (car avlist) '})))
                (soarpush (car avlist) id) (pop avlist))
               (cond ((null avlist) (%error "Didn't find terminating }" form)))
               (setq id (dreverse (cons '} id)))
               (setq avlist (cdr avlist)))
              (t (setq id (cadr form)) (setq avlist (cddr form))))
   loop (cond ((not avlist) (return (reverse clist))))
        (setq negated nil)
        (cond ((equal (car avlist) '-) (pop avlist) (setq negated t)))
        (cond ((not (equal (car avlist) '^))
               (%error "Didn't find a ^ when expected" avlist)))
        (pop avlist)
        (setq end-atom
               (cond ((eq (car avlist) '<<) '>>) ((eq (car avlist) '{) '}) (t nil)))
        (cond (pand-id
               (setq newform
                      (append (list type '^ 'identifier)
                              (append id (list '^ 'attribute (pop avlist))))))
              (t (setq newform (list type '^ 'identifier id '^ 'attribute (pop avlist)))))
        (cond (end-atom
               (soarwhile
                (and avlist
                     (not (eq (car avlist) end-atom)))
                (setq newform (append newform (list (pop avlist)))))
               (cond ((null avlist) (%error "Didn't find terminator" end-atom)))
               (setq newform (append newform (list (pop avlist))))))
        (setq no-value t)
        (cond ((and avlist
                    (not (eq (car avlist) '^))
                    (not (eq (car avlist) '-))
                    (or (not (soarlistp (car avlist)))
                        (not (eq (caar avlist) '^))))
               (setq newform (append newform (list '^ 'value)))
               (setq no-value nil)))
        (setq temp-form newform)
        (soarwhile
         (and avlist
              (not (eq (car avlist) '^))
              (not (eq (car avlist) '-))
              (or (not (soarlistp (car avlist)))
                  (not (eq (caar avlist) '^))))
         (setq newform temp-form)
         (cond ((predicatep (car avlist))
                (setq newform (append newform (list (pop avlist))))))
         (setq end-atom
                (cond ((eq (car avlist) '<<) '>>) ((eq (car avlist) '{) '}) (t nil)))
         (setq newform (append newform (list (pop avlist))))
         (cond (end-atom
                (soarwhile
                 (and avlist
                      (not (eq (car avlist) end-atom)))
                 (setq newform (append newform (list (pop avlist)))))
                (cond ((null avlist) (%error "Didn't find terminator" end-atom)))
                (setq newform (append newform (list (pop avlist))))))
         (cond ((and avlist
                     (soarlistp (car avlist)))
                (setq newform (append newform (pop avlist)))))
         (cond (in-actions (soarpush 'make newform))) (cond (negated (soarpush '- clist)))
         (soarpush newform clist))
        (cond (no-value
               (cond ((and avlist
                           (soarlistp (car avlist)))
                      (setq newform (append newform (pop avlist)))))
               (cond (in-actions (soarpush 'make newform)))
               (cond (negated (soarpush '- clist)))
               (soarpush newform clist)))
        (go loop)))

(defun sp-to-p (xs)
  (prog (in-actions follows-negation forms pname)
        (setq in-actions nil)
        (setq follows-negation nil)
        (setq pname (car xs))
        (return (cons 'p
                      (soarmapconc
                       #'(lambda (x)
                           (cond ((equal x '-->) (setq in-actions t)))
                           (setq forms
                          (sp-form-expand pname x in-actions follows-negation))
                           (setq follows-negation (eq x '-))
                           forms)
                       xs)))))

(defun ops-spm (z)
  (setq *indent* 0)
  (soarmapc #'(lambda (x) (pprule x 'sp)) z)
  nil)


(defun spm-to-make (form)
  (soarmapc #'eval (sp-info-expand nil form t))
  nil)

(defun ops-spo (object)
  (setq *indent* 0)
  (cond ((or (null object)
             (numberp (car object))))
        ((not (numberp (soarlast object)))
         (terpri (trace-file))
         (spo1 (car object) *spo-default-depth* nil)
         (eval (cons 'spo (cdr object))))
        (t (terpri (trace-file))
           (spo1 (car object) (soarlast object) nil)
           (eval (cons 'spo (cdr object)))))
  (setq *print-spo-list* nil))


(defun spo1 (object depth prefflag)
  (prog (out class out2 out1)
        (cond ((soarmemq object *print-spo-list*) (return))
              (t (soarpush object *print-spo-list*)))
        (setq out (p-conds-to-sp (sppwm1 (append '(^ identifier) (list object)))))
        (setq out1 nil)
        (soarmapc
         #'(lambda (x)
             (cond ((eq prefflag (eq (car x) 'preference))
                    (ppline x)
                    (soarpush x out1)
                    (terpri (trace-file)))))
         out)
        (cond ((<= depth 1) (return)))
        (setq out out1)
        (setq *indent* (+ *indent* 3))
        (soar-do ((out2 out (cdr out2))) ((null out2))
         (soar-do ((out1 (cdar out2) (cdr out1))) ((null out1))
          (cond ((get (cadr out1) 'in-id-field) (spo1 (cadr out1) (- depth 1) prefflag)))))
        (setq *indent* (- *indent* 3))))

(defun ops-sppwm (avlist)
  (prog (noclassflag condlist cond1 ids avinfos)
        (soarterpri)
        (setq *indent* 0)
        (cond ((null avlist) (go l2))
              ((not (soarassoc (car avlist) *sp-classes*))
               (setq avlist (cons 'state avlist))
               (setq noclassflag t)))
        (cond ((null (cdr avlist))
               (setq avinfos (list (list (cdr (soarassoc (car avlist) *sp-classes*))))))
              ((null (cddr avlist))
               (setq avinfos
                      (list (list (cdr (soarassoc (car avlist) *sp-classes*))
                                  '^
                                  'identifier
                                  (cadr avlist)))))
              (t (soarcatch '!error! (setq avinfos (sp-form-expand nil avlist nil nil)))))
        (cond ((variablep (cadddr (car avinfos)))
               (soarmapc #'(lambda (x) (rplacd x (cdr (cdddr x)))) avinfos)))
        (cond (noclassflag
               (setq avinfos (soarmapcar #'cdr avinfos))
               (setq avlist (cdr avlist))))
        (setq ids (get-ids (sppwm1 (car avinfos)) nil))
     l1 (setq avinfos (cdr avinfos))
        (cond ((null avinfos) (go l2)))
        (setq ids (get-ids (sppwm1 (car avinfos)) ids))
        (cond ((null ids) (go l3)))
        (go l1)
     l2 (cond (avlist (setq ids (append ids (get-ids (sppwm1 avlist) nil)))))
        (pp-class-ids ids)
     l3 ))


(defun sppwm1 (avlist)
  (cond ((not (conv-to-attr-nums avlist nil)) nil)
        (t (mapwm #'sppwm2) (prog2 nil *wm* (setq *wm* nil)))))

(defun sppwm2 (elm-tag)
  (cond ((filter (car elm-tag)) (setq *wm* (cons (ppelm1 (car elm-tag)) *wm*)))))

(defun ops-spr (arg)
  (cond ((get (car arg) 'in-id-field) (eval (cons 'spo arg)))
        ((soarmemq (car arg) *pnames*) (eval (cons 'spm arg)))
        ((numberp (car arg)) (eval (cons 'swm arg)))
        ((null (car arg)) (pgs))
        (t (eval (cons 'sppwm arg)))))


(defun ops-swm (a)
  (pp-class-ids (soarmapcar #'(lambda (x) (cons (cadr x) (car x))) (get-wm a))))

(defun $varbind (x)
  (prog (r)
        (and (not *in-rhs*) (return x))
        (setq r (soarassq x *variable-memory*))
        (cond (r (return (cdr r)))
              ((variablep x)
               (setq r (soargensym (soarnthchar x 2)))
               (make-var-bind x r)
               (return r))
              (t (return x)))))

(defun &bus (outs)
  (prog (dp)
        (setq *alpha-flag-part* *flag-part*)
        (setq *alpha-data-part* *data-part*)
        (setq dp (car *data-part*))
        (setq *c1* (car dp))
        (setq dp (cdr dp))
        (setq *c2* (car dp))
        (setq dp (cdr dp))
        (setq *c3* (car dp))
        (setq dp (cdr dp))
        (setq *c4* (car dp))
        (setq dp (cdr dp))
        (setq *c5* (car dp))
        (setq dp (cdr dp))
        (setq *c6* (car dp))
        (setq dp (cdr dp))
        (setq *c7* (car dp))
        (setq dp (cdr dp))
        (setq *c8* (car dp))
        (setq dp (cdr dp))
        (setq *c9* (car dp))
        (setq dp (cdr dp))
        (setq *c10* (car dp))
        (setq dp (cdr dp))
        (setq *c11* (car dp))
        (setq dp (cdr dp))
        (setq *c12* (car dp))
        (setq dp (cdr dp))
        (setq *c13* (car dp))
        (setq dp (cdr dp))
        (setq *c14* (car dp))
        (setq dp (cdr dp))
        (setq *c15* (car dp))
        (setq dp (cdr dp))
        (setq *c16* (car dp))
        (eval-nodelist outs)))

(defun &priv (outs name)
  (prog nil
        (eval-nodelist outs)))

(defun accum-stats nil
  (setq *prod-count* (+ *prod-count* 1))
  (setq *total-token* (+ *total-token* *current-token*))
  (cond ((> *current-token* *max-token*) (setq *max-token* *current-token*)))
  (setq *total-wm* (+ *total-wm* *current-wm*))
  (cond ((> *current-wm* *max-wm*) (setq *max-wm* *current-wm*))))

(defun add-to-wm (wme)
  (prog (class part timetag port match-time)
        (setq port (trace-file))
        (setq wme (eliminate-trailing-nil wme))
        (setq class (class wme))
        (setq part (get class 'wmpart*))
        (cond ((soarassoc wme part)
               (and *chunk-all-paths*
                    (mark-in-id wme))
               (return nil))
              ((mark-in-id wme) (return nil))
              ((soarmember wme *brkrun*) (setq *break-flag* t)))
        (trace-condition-action wme 'action)
        (setq *critical* t)
        (setq *action-count* (1+ *action-count*))
        (setq *current-wm* (1+ *current-wm*))
        (cond ((> *current-wm* *max-wm*) (setq *max-wm* *current-wm*)))
        (or (soarmemq class *wmpart-list*)
            (soarpush class *wmpart-list*))
        (setq timetag *action-count*)
        (soarputprop class (cons (cons wme timetag) part) 'wmpart*)
        (match 'new wme)
        (setq *critical* nil)
        (cond ((or (not *in-rhs*)
                   (not (trace-problem-space?))))
              ((and (not *wtrace*)
                    *ptrace*
                    *ttrace*)
               (time-tag-print (list wme) port))
              (*wtrace* (soarprintc "=>WM: ") (ppelm wme port)))))

(defun best-of (set)
  (prog (cs entry best-list)
        (setq cs set)
        (setq best-list nil)
     l1 (setq entry (car cs))
        (setq cs (cdr cs))
        (cond ((null entry) (return best-list))
              ((or (eq *phase* (get (car entry) 'type))
                   (and (eq *phase* 'elaborate)
                        (eq (get (car entry) 'type) 'elaborate-once)))
               (soarpush entry best-list)
               (and (eq *phase* 'elaborate)
                    (remove-cs entry))))
        (go l1)))

(defun ops-bind (z)
  (prog (val)
        (cond ((not *in-rhs*)
               (soarwarn "Cannot be called at top level" 'bind)
               (return nil))
              ((< (length z) 1)
               (soarwarn "Bind: Wrong number of arguments to" z)
               (return nil))
              ((not (symbolp (car z)))
               (soarwarn "Bind: illegal argument" (car z))
               (return nil))
              ((equalp (length z) 1) (setq val (soargensym 's)))
              (t ($reset) (eval-args (cdr z)) (setq val ($parameter 1))))
        (make-var-bind (car z) val)))


(defun check-action (x)
  (prog (a)
        (cond ((atom x) (soarwarn "Atomic Action" x) (return nil)))
        (setq a (setq *action-type* (car x)))
        (cond ((eq a 'bind) (check-bind x))
              ((eq a 'cbind) (check-cbind x))
              ((eq a 'call2) (return nil))
              ((eq a 'tabstop) (check-bind x))
              ((eq a 'decide) (check-decide))
              ((eq a 'make) (check-make x))
              ((eq a 'modify) (check-modify x))
              ((eq a 'remove-ops) (check-remove-ops x))
              ((eq a 'write) (check-write x))
              ((eq a 'write1) (check-write x))
              ((eq a 'write2) (check-write x))
              ((eq a 'call1) (check-call1 x))
              ((eq a 'halt) (check-halt x))
              ((eq a 'openfile) (check-openfile x))
              ((eq a 'closefile) (check-closefile x))
              ((eq a 'default) (check-default x))
              ((eq a 'build) (check-build x))
              (t (soarwarn "Illegal Action" x)))))

(defun check-make (z)
  (or (soarmemq *p-type* '(elaborate-once elaborate))
      (soarwarn "Illegal make in production type" *p-type*))
  (and (null (cdr z))
       (soarwarn "Missing arguments in Make" " "))
  (check-change& (cdr z)))

(defun check-modify (z)
  (soarwarn "Illegal modify in Soar production" *p-type*)
  (and (null (cdr z))
       (soarwarn "Missing arguments in Modify" " "))
  (check-rhs-ce-var (cadr z))
  (and (null (cddr z))
       (soarwarn "No changes to make in Modify" " "))
  (check-change& (cddr z)))

(defun check-rhs-value (x)
  (cond ((soarlistp x) (check-rhs-function x))))

(defun cmp-ce nil
  (prog (z)
        (new-subnum 0)
        (setq *cur-vars* nil)
        (setq z (lex))
        (and (atom z)
             (%error "Atomic conditions are not allowed" z))
        (setq *preference-found* (eq (car z) 'preference))
        (prepare-sublex z)
     la (and (end-of-ce)
             (return nil))
        (incr-subnum)
        (cmp-element)
        (setq *context-field-found* nil)
        (go la)))

(defun cmp-tab nil
  (prog (r)
        (sublex)
        (setq r (sublex))
        (setq *context-field-found*
               (and *preference-found*
                    (soarmemq r '(goal problem-space state operator))))
        (setq r ($litbind r))
        (new-subnum r)))

(defun cmp-var (test)
  (prog (old name)
        (setq name (sublex))
        (setq old (soarassq name *cur-vars*))
        (cond ((and old
                    (eq (cadr old) 'eq))
               (cmp-old-eq-var test old))
              ((and old
                    (eq test 'eq))
               (cmp-new-eq-var name old))
              ((and (eq test 'eq)
                    *context-field-found*)
               (cmp-new-var name 'eqnil))
              (t (cmp-new-var name test)))))

(defun cmp-p (name type matrix)
  (prog (m bakptrs root)
        (cond ((soarlistp name) (%error "Illegal production name" name))
              ((equal (get name 'production) matrix) (return nil)))
        (cond ((not (soarmemq type '(elaborate elaborate-once decide)))
               (%error "Illegal production type" type)))
        (and *print-pname*
             (princ name (trace-file)))
        (prepare-lex matrix)
        (cond ((excise-p name) (princ (list 'excised name) (trace-file))))
        (setq bakptrs nil)
        (setq *last-node* *first-node*)
        (and (> *current-wm* 0)
             (make-new-root name))
        (setq root (setq *new-root* *last-node*))
        (setq *pcount* (1+ *pcount*))
        (setq *feature-count* 0)
        (setq *ce-count* 0)
        (setq *vars* nil)
        (setq *ce-vars* nil)
        (setq *rhs-bound-vars* nil)
        (setq *rhs-bound-ce-vars* nil)
        (setq *last-branch* nil)
        (setq m (rest-of-p))
     l1 (and (end-of-p)
             (%error "No '-->' in production" m))
        (setq *last-node* root)
        (cmp-prin)
        (setq bakptrs (cons *last-branch* bakptrs))
        (or (eq '--> (peek-lex))
            (go l1))
        (lex)
        (cond ((not (soarmemq name *pnames*)) (setq *pnames* (cons name *pnames*))))
        (check-rhs (rest-of-p))
        (link-new-node
         (list '&p
               *feature-count*
               name
               (encode-dope)
               (encode-ce-dope)
               (cons 'progn (rest-of-p))))
        (soarputprop name type 'type)
        (soarputprop name (cdr (nreverse bakptrs)) 'backpointers)
        (soarputprop name matrix 'production)
        (soarputprop name (compute-negation-index matrix) 'negation-index)
        (soarputprop name *last-node* 'topnode)
        (cond ((> *current-wm* 0) (mapwm #'match-update)))
        (return nil)))

(defun cmp-prin nil
  (cond ((null *last-branch*) (cmp-posce) (cmp-nobeta))
        ((eq (peek-lex) '-) (cmp-negce) (cmp-not))
        (t (cmp-posce) (cmp-and))))

(defun compile-production (name type matrix)
  (setq *p-type* type)
  (setq *matrix* matrix)
  (setq *p-name* name)
  (soarcatch '!error! (cmp-p *p-name* *p-type* *matrix*))
  (setq *p-name* nil))

(defun conflict-resolution nil
  (prog (best len)
        (setq len (length *conflict-set*))
        (cond ((> len *max-cs*) (setq *max-cs* len)))
        (setq *total-cs* (+ *total-cs* len))
        (cond (*conflict-set*
               (setq best (best-of *conflict-set*))
               (cond ((neq *phase* 'decide) (soarmapc #'remove-cs best)))
               (return best))
              (t (return nil)))))

(defun conflict-set nil
  (prog (cnts cs p z phase-type)
        (setq cnts nil)
        (setq cs *conflict-set*)
        (setq phase-type 'decide)
     l1 (and (atom cs)
             (go l2))
        (setq p (caar cs))
        (and (neq (get p 'type) 'decide)
             (setq phase-type 'elaborate))
        (setq cs (cdr cs))
        (setq z (soarassq p cnts))
        (cond ((null z) (setq cnts (cons (cons p 1) cnts))) (t (rplacd z (1+ (cdr z)))))
        (go l1)
     l2 (cond ((atom cnts) (terpri (trace-file)) (return (list 'phase phase-type))))
        (cond ((and (eq phase-type 'elaborate)
                    (eq (get (caar cnts) 'type) 'decide))
               (setq cnts (cdr cnts))
               (go l2)))
        (terpri (trace-file))
        (soarprinc3 (caar cnts) " " (get (caar cnts) 'type))
        (cond ((> (cdar cnts) 1) (soarprinc3 "	(" (cdar cnts) " OCCURRENCES)")))
        (setq cnts (cdr cnts))
        (go l2)))

(defun ops-crlf (z)
  (cond (z (soarwarn "CRLF: Does not take arguments" z)) (t ($value 'crlf))))


(defun do-continue (wmi)
  (cond (*critical* (soarprintc "WARNING: Network may be inconsistent")))
  (process-changes wmi nil)
  (main))

(defun eval-rhs (pname data)
  (prog (node port)
        (cond ((and (trace-problem-space?)
                    (or *ptrace*
                        (soarmemq pname *tracep-list*)))
               (setq port (trace-file))
               (soarprintc (1+ *decide-count*))
               (soarprinc3 ":" *cycle-count* " ")
               (soarprinc pname)
               (and *ptrace*
                    *ttrace*
                    (time-tag-print data port))))
        (setq *data-matched* data)
        (setq *p-name* pname)
        (setq *last* nil)
        (setq node (get pname 'topnode))
        (init-var-mem (var-part node))
        (init-ce-var-mem (ce-var-part node))
        (setq *in-rhs* t)
        (setq *current-goal* (find-max-goal-depth *data-matched*))
        (soarmapc #'condition-trace *data-matched*)
        (cond ((null *never-learn*)
               (setq *data-matched*
                      (append (add-negations *current-goal* (reverse *data-matched*))
                              *data-matched*))))
        (eval (rhs-part node))
        (setq *in-rhs* nil)))

(defun excise-p (name)
  (prog nil
        (cond ((and (symbolp name)
                    (get name 'topnode))
               (princ "#" (trace-file))
               (setq *pcount* (1- *pcount*))
               (remove-from-conflict-set name)
               (kill-node (get name 'topnode))
               (remprop name 'production)
               (remprop name 'backpointers)
               (remprop name 'topnode)
               (remprop name 'type)
               (remprop name 'negation-index)
               (setq *chunks* (dremove name *chunks*))
               (setq *pnames* (dremove name *pnames*))
               (and (get name 'gensymed)
                    (setq *free-pname-list* (soarsort (cons name *free-pname-list*))))
               (return t))
              (t (return nil)))))

(defun finish-literalize nil
  (cond ((not (null *class-list*))
         (soarmapc #'note-user-assigns *class-list*)
         (soarmapc #'assign-scalars *class-list*)
         (soarmapc #'assign-vectors *class-list*)
         (soarmapc #'put-ppdat *class-list*)
         (soarmapc #'erase-literal-info *class-list*)
         (setq *value-index* (1- (literal-binding-of 'value)))
         (setq *attribute-index* (1- (literal-binding-of 'attribute)))
         (setq *id-index* (1- (literal-binding-of 'identifier)))
         (setq *goal-index* (1- (literal-binding-of 'goal)))
         (setq *problem-space-index* (1- (literal-binding-of 'problem-space)))
         (setq *state-index* (1- (literal-binding-of 'state)))
         (setq *operator-index* (1- (literal-binding-of 'operator)))
         (setq *slot-index* (1- (literal-binding-of 'role)))
         (setq *big-info-index* (max *id-index* *attribute-index* *value-index*))
         (setq *big-index*
                (max *id-index*
                     *goal-index*
                     *problem-space-index*
                     *state-index*
                     *operator-index*
                     *value-index*
                     *slot-index*))
         (setq *save-class-list* *class-list*)
         (setq *class-list* nil)
         (setq *buckets* nil))))

(defun i-g-v nil
  (prog (x)
        (setq *save-class-list* nil)
        (setq *class-list* nil)
        (soarsyntax)
        (soarputprop 'identifier 2 'ops-bind)
        (soarputprop 'object 2 'ops-bind)
        (soarputprop 'attribute 3 'ops-bind)
        (soarputprop 'value 4 'ops-bind)
        (setq *sp-classes*
               '((state . state-info) (object . info) (problem-space . space-info)
                 (space . space-info) (operator . op-info) (desired . desired-info)
                 (gc . goal-context-info) (goal . goal-context-info)
                 (context . goal-context-info) (goal-context . goal-context-info)
                 (evaluation . eval-info)))
        (setq *ops5-actions*
               '(openfile closefile default write write1 write2 call1 halt decide bind
                 call2 cbind obind build tabto tabstop))
        (setq *impasse-subset-not-equal* nil)
        (setq *brkpts* nil)
        (setq *brkrun* nil)
        (setq *brknames* nil)
        (setq *default-multi* 5)
        (setq *warning* t)
        (setq *order-trace* nil)
        (setq *print-pname* nil)
        (setq *buckets* 16)
        (setq *accept-file* nil)
        (setq *write-file* nil)
        (setq *trace-file* nil)
        (setq *strategy* 'lex)
        (setq *user-ids* nil)
        (setq *in-rhs* nil)
        (setq *decide-trace* nil)
        (setq *pick-first-condition* t)
        (setq *max-recurse* 2)
        (setq *multi-attribute* nil)
        (multi-attributes '((problem-space operator) (goal item) (state evaluation 2)))
        (setq *instance-attributes* nil)
        (trace-attributes
         '((goal role) (goal impasse) (goal desired) (goal superoperator)
           (operator object) (operator instance)))
        (setq *watch-free-problem-spaces* nil)
        (setq *chunk-free-problem-spaces* nil)
        (setq *tracep-list* nil)
        (setq *cav* nil)
        (setq *cavi* nil)
        (setq *tracep* t)
        (setq *max-chunk-conditions* 200)
        (setq *subgoal-results* nil)
        (setq *chunk-all-paths* t)
        (setq *smaller-chunks* nil)
        (setq *chunks* nil)
        (setq *chunk-classes* '(problem-space state operator))
        (setq *always-learn* t)
        (setq *new-chunks* nil)
        (setq *wme-list-stack* nil)
        (setq *learn-ids* nil)
        (setq *learning* nil)
        (setq *ltrace* nil)
        (setq *never-learn* t)
        (setq *print-learn* 0)
        (setq *split-actions* t)
        (setq *select-equal* 'first)
        (setq *default-user-select* t)
        (setq *pnames* nil)
        (setq *free-pname-list* nil)
        (setq *used-gensym-list* nil)
        (setq *free-gensym-list* (list (list 'free-list)))
        (setq *used-var-list* nil)
        (setq *used-char-list* nil)
        (init-gensym)
        (soarmapc #'(lambda (x) (soarputprop x 't 'predicate)) '(< <= <=> <> = > >=))
        (setq *recording* nil)
        (setq *refracts* nil)
        (setq *real-cnt* (setq *virtual-cnt* 0))
        (setq *limit-token* 1000000)
        (setq *limit-cs* 1000000)
        (setq *critical* nil)
        (setq *wmpart-list* nil)
        (setq *global-pair* (list 'nil))
        (setq *subgoal-tabs* t)
        (setq *size-result-array* 15)
        (setq *result-array* (makevector 16))
        (setq *record-array* (makevector 16))
        (setq x 0)
   loop (putvector *result-array* x nil)
        (putvector *record-array* x nil)
        (setq x (1+ x))
        (and (not (> x *size-result-array*))
             (go loop))
        (make-bottom-node)
        (setq *pcount* 0)
        (setq *already-fired* nil)
        (setq *current-goal* nil)
        (soarmachinetype)
        (setq *spo-default-depth* 1)
        (setq *print-spo-list* nil)
        (setq *indent* 0)
        (watch 0)
        (swatch 0)
        (setq *max-elaborations* 100)
        (setq *remaining-cycles* 1000000)
        (setq *remaining-decide* 1000000)
        (setq *version-number* "4")
	(setq *minor-version* "0")
	(setq *release-number* "1")
	(setq *public-version* t)
	(setq *date-created* "November 14, 1985")
	(soar-greeting)
        (init-literalizes)
        (init-soar)))

(defun soar-greeting nil	
    (soarprintc "Soar ")
    (cond (*public-version*
	      (soarprinc "(Version ")
	      (soarprinc *version-number*)
	      (soarprinc ", Release ")
	      (soarprinc *release-number*)
	      (soarprinc ")"))
	  (t (soarprinc *version-number*)
	     (soarprinc ".")
	     (soarprinc *minor-version*)))
    (soarprintc "Created ")
    (soarprint *date-created*)
    (cond (*public-version*
	      (soarprint "Bugs and questions to soar@h.cs.cmu.edu")))
    (soarprint "Copyright (c) 1985 Xerox Corporation.  All Rights Reserved.")
    (terpri)
    (soarprint "Use of this software is permitted for non-commercial")
    (soarprint "research purposes, and it may be copied only for that use.")
    (soarprint "This software is made available AS IS, and Xerox")
    (soarprint "Corporation, Stanford University and Carnegie-Mellon")
    (soarprint "University make no warranty about the software or its")
    (soarprint "performance.")
)

(defun insertcs (name data rating)
  (prog (instan)
        (and (refracted name data)
             (return nil))
        (setq instan (cons name data))
        (and (atom *conflict-set*)
             (setq *conflict-set* nil))
        (return (setq *conflict-set* (cons instan *conflict-set*)))))

(defun instantiation (conflict-elem)
  (cdr (pname-instantiation conflict-elem)))

(defun main nil
  (prog (phase-set r)
        (setq *halt-flag* nil)
        (setq *break-flag* nil)
        (setq *elaborations-count* 0)
        (setq phase-set nil)
    dil (cond (*halt-flag* (setq r "End -- Explicit Halt") (go finis))
              ((or (zerop *remaining-decide*)
                   (zerop *remaining-cycles*))
               (setq *break-flag* t)))
        (cond ((or *break-flag*
;                   (readp t)
		   )
               (setq r '***break***)
               (go finis)))
        (setq *first-remove* (setq *first-action* t))
        (setq phase-set (conflict-resolution))
        (cond ((eq *phase* 'decide)
               (setq *remaining-cycles* (1- *remaining-cycles*))
               (setq *remaining-decide* (1- *remaining-decide*))
               (setq *cycle-count* (1+ *cycle-count*))
               (setq *decide-count* (1+ *decide-count*))
               (setq *elaborations-count* 0)
               (setq *in-rhs* t)
               (process-decide phase-set)
               (setq *in-rhs* nil)
               (setq *phase* 'elaborate))
              ((not phase-set) (setq *phase* 'decide))
              (t (setq *cycle-count* (1+ *cycle-count*))
                 (cond ((and *wtrace*
                             (trace-problem-space?))
                        (soarprintc "--Elaboration Phase--")))
                 (setq *already-fired* nil)
                 (soarmapc #'process-instance phase-set)
                 (setq *remaining-cycles* (1- *remaining-cycles*))
                 (setq *elaborations-count* (1+ *elaborations-count*))
                 (cond ((eq *max-elaborations* *elaborations-count*)
                        (soarwarn
                         "Exceeded *max-elaborations*. Proceeding to decision procedure."
                         *max-elaborations*)
                        (setq *phase* 'decide)))))
        (go dil)
  finis (setq *p-name* nil)
        (terpri (trace-file))
        (return r)))

(defun ops-p (z)
  (finish-literalize)
  (princ '* (trace-file))
  (cond ((soarlistp (cadr z)) (compile-production (car z) 'elaborate (cdr z)))
        (t (compile-production (car z) (cadr z) (cddr z)))))

(defun ops-pbreak (z)
  (cond ((atom z)
         (terpri (trace-file))
         (soarprinc2 "Production breaks:" *brkpts*)
         (terpri (trace-file))
         (soarprinc2 "Name/identifier breaks:" *brknames*)
         (terpri (trace-file))
         nil)
        (t (soarmapc #'pbreak2 z) z)))


(defun pbreak2 (rule)
  (prog (type blist)
        (cond ((or (not (symbolp rule))
                   (soarlistp rule))
               (soarwarn "Illegal argument:" rule))
              ((or (soarmemq rule *brkpts*)
                   (soarmemq rule *brknames*))
               nil)
              ((get rule 'topnode) (soarpush rule *brkpts*) rule)
              (t (soarpush rule *brknames*) rule))))

(defun pname-instantiation (conflict-elem)
  conflict-elem)

(defun ops-pm (z)
  (soarmapc #'(lambda (x) (pprule x nil)) z)
  nil)


(defun ppattval nil
  (prog (att val)
        (setq att (cadr *ppline*))
        (setq *ppline* (cddr *ppline*))
        (cond ((or (null *ppline*)
                   (eq (car *ppline*) '^))
               (setq val nil))
              (t (setq val (getval))))
        (cond ((> (+ (nwritn t) (flatc att) (flatc val)) 76) 
	       (soarprintc " ") (indent)))
        (soarprinc2 '^ att)
        (soarmapc #'(lambda (z) (soarprinc2 " " z)) val)))

(defun ppline (line)
  (prog nil
        (indent)
        (cond ((atom line) (princ line (trace-file)))
              (t (princ "(" (trace-file))
                 (setq *ppline* line)
                 (ppline2)
                 (princ ")" (trace-file))))
        (return nil)))

(defun pponlyval nil
  (prog (val needspace)
        (setq val (getval))
        (setq needspace nil)
        (cond ((> (+ (nwritn t) (flatc val)) 76)
               (setq needspace nil)
               (soarprintc " ")
               (indent)))
    top (and (atom val)
             (return nil))
        (and needspace
             (princ " " (trace-file)))
        (setq needspace t)
        (princ (car val) (trace-file))
        (setq val (cdr val))
        (go top)))

(defun pprule (name flag)
  (prog (matrix next lab port)
        (and (not (symbolp name))
             (return nil))
        (setq matrix (get name 'production))
        (and (null matrix)
             (return nil))
        (setq port (trace-file))
        (soarprintc "(")
        (cond ((eq flag 'sp) (setq matrix (p-to-sp matrix)) (princ 'sp port))
              (t (princ 'p port)))
        (soarprinc2 " " name)
        (and (eq 'decide (get name 'type))
             (soarprinc2 " " 'decide))
    top (and (atom matrix)
             (go fin))
        (setq next (car matrix))
        (setq matrix (cdr matrix))
        (setq lab nil)
        (terpri port)
        (cond ((eq next '-)
               (princ "  - " port)
               (setq next (car matrix))
               (setq matrix (cdr matrix)))
              ((eq next '-->) (princ "  " port))
              ((and (eq next '{)
                    (atom (car matrix)))
               (princ "   {" port)
               (setq lab (car matrix))
               (setq next (cadr matrix))
               (setq matrix (cdddr matrix)))
              ((eq next '{)
               (princ "   {" port)
               (setq lab (cadr matrix))
               (setq next (car matrix))
               (setq matrix (cdddr matrix)))
              (t (princ "    " port)))
        (ppline next)
        (cond (lab (soarprinc3 " " lab "}")))
        (go top)
    fin (soarprint ")")))

(defun ops-ppwm (avlist)
  (conv-to-attr-nums avlist t)
  (mapwm #'ppwm2)
  nil)


(defun print-stats nil
  (setq *break-flag* nil)
  (print-times "Run Statistics"))

(defun print-times (mess)
  (prog (cc ac pc dc time d-time)
        (setq time (time-conversion *elapsed-time*))
        (setq dc (+ (float *decide-count*) 1.0e-20))
        (setq cc (+ (float *cycle-count*) 1.0e-20))
        (setq pc (+ (float *prod-count*) 1.0e-20))
        (setq ac (+ (float *action-count*) 1.0e-20))
        (soarprintc mess)
        (pm-size)
        (cond ((eq *decide-count* 0) (terpri (trace-file)) (return)))
        (printline (list time " Seconds Elapsed"))
        (cond ((eq time 0) (setq time 1.0e-8)))
        (printline
         (list *decide-count*
               " Decision Cycles "
               (list (quotient (float *decide-count*) time) " Per Sec.")))
        (printline
         (list *cycle-count*
               " Prod Cycles "
               (list (quotient (float *cycle-count*) time) " Per Sec.")
               " "
               (list (- (quotient (float *cycle-count*) dc) 1)
                     " E cycles/ D cycle")))
        (printline
         (list *prod-count*
               " Prod. Firings "
               (list (quotient (float *prod-count*) time) " Per Sec.")
               " "
               (and (< 0.01 (- cc *decide-count*))
                    (list (quotient (float (- *prod-count* *decide-count*))
                           (- cc *decide-count*))
                          " Elab. prod. in parallel"))))
        (printline
         (list *action-count*
               " RHS Actions "
               (list (quotient (float *action-count*) time) " Per Sec.")))
        (printline
         (list (round (quotient (float *total-cs*) cc))
               " Mean conflict set size "
               (list *max-cs* " Maximum")))
        (printline
         (list (round (quotient (float *total-wm*) pc))
               " Mean working memory size "
               (list *max-wm* " Maximum " *current-wm* " Current ")))
        (printline
         (list (round (quotient (float *total-token*) pc))
               " Mean token memory size "
               (list *max-token* " Maximum " *current-token* " Current ")))))

(defun promote-var (dope)
  (prog (vname vpred vpos new)
        (setq vname (car dope))
        (setq vpred (cadr dope))
        (setq vpos (caddr dope))
        (cond ((eq vpred 'eqnil) (setq vpred 'eq))
              ((not (eq vpred 'eq))
               (%error "Illegal predicate for first occurrence" (list vname vpred))))
        (setq new (list vname 0 vpos))
        (setq *vars* (cons new *vars*))))

(defun remove-cs (entry)
  (setq *conflict-set* (dremove entry *conflict-set*)))

(defun remove-from-conflict-set (name)
  (prog (cs entry)
     l1 (setq cs *conflict-set*)
     l2 (cond ((atom cs) (return nil)))
        (setq entry (car cs))
        (setq cs (cdr cs))
        (cond ((eq name (car entry))
               (setq *conflict-set* (dremove entry *conflict-set*))
               (go l1))
              (t (go l2)))))

(defun remove-from-wm (wme)
  (prog (class z part timetag port match-time)
        (setq port (trace-file))
        (setq wme (eliminate-trailing-nil wme))
        (setq class (wm-hash wme))
        (setq part (get class 'wmpart*))
        (setq z
               (cond ((eq class 'goal-context-info) (soarassoc wme part))
                     (t (soarassq wme part))))
        (or z
            (return nil))
        (setq wme (car z))
        (setq timetag (cdr z))
        (cond ((and *in-rhs* *first-remove*)
               (and *ptrace*
                    *ttrace*
                    (trace-problem-space?)
                    (not *wtrace*)
                    (princ " <--" port))
               (setq *first-action* t)
               (setq *first-remove* nil)))
        (cond ((or (not *in-rhs*)
                   (not (trace-problem-space?))))
              ((and (not *wtrace*)
                    *ptrace*
                    *ttrace*)
               (time-tag-print (list wme) port))
              (*wtrace* (soarprintc "<=WM: ") (ppelm wme port)))
        (setq *action-count* (1+ *action-count*))
        (setq *critical* t)
        (setq *current-wm* (1- *current-wm*))
        (match nil wme)
        (soarputprop class (dremove z part) 'wmpart*)
        (setq *critical* nil)))

(defun removecs (name data)
  (prog (cr-data inst cs)
        (setq cr-data (cons name data))
        (setq cs *conflict-set*)
      l (cond ((null cs) (record-refract name data) (return nil)))
        (setq inst (car cs))
        (setq cs (cdr cs))
        (and (not (top-levels-eq inst cr-data))
             (go l))
        (setq *conflict-set* (dremove inst *conflict-set*))))

(defun ops-rjust (z)
  (prog (val)
        (cond ((not (== (length z) 1))
               (soarwarn "RJUST: Wrong number of arguments" z)
               (return nil)))
        (setq val ($varbind (car z)))
        (cond ((or (not (numberp val))
                   (< val 1)
                   (> val 127))
               (soarwarn "RJUST: Illegal value for field width" val)
               (return nil)))
        ($value 'rjust)
        ($value val)))


(defun ops-run (z)
  (prog (result)
        (cond ((eq *pcount* 0)
               (terpri (trace-file))
               (soarprint "Please load in some productions first.")
               (return nil)))
        (init-wm)
        (setq *brkrun* nil)
        (setq *begin-time* (alwaystime))
        (setq *remaining-decide* 1000000)
        (setq *remaining-cycles* 1000000)
        (cond ((atom z))
              ((and (atom (cdr z))
                    (numberp (car z))
                    (> (car z) -1))
               (setq *remaining-cycles* (car z)))
              ((and (numberp (car z))
                    (eq (cadr z) 'd))
               (setq *remaining-decide* (car z)))
              ((soarlistp (car z))
               (setq *brkrun* (conv-input-wme (car z)))
               (and (null *brkrun*)
                    (return "What?")))
              (t (setq *brkrun* (car z))))
        (setq result (do-continue nil))
        (setq *elapsed-time*
               (+ *elapsed-time* (- (alwaystime) *begin-time*)))
        (return result)))


(defun ops-tabto (z)
  (prog (val)
        (cond ((not (== (length z) 1))
               (soarwarn "TABTO: Wrong number of arguments" z)
               (return nil)))
        (setq val ($varbind (car z)))
        (cond ((or (not (numberp val))
                   (< val 1)
                   (> val 127))
               (soarwarn "TABTO: Illegal column number" val)
               (return nil)))
        ($value 'tabto)
        ($value val)))

(defun ops-watch (z)
  (cond ((equal z '(-1)) (setq *wtrace* nil)
			 (setq *ptrace* nil) (setq *otrace* nil) -1)
        ((equal z '(0)) (setq *wtrace* nil)
			(setq *ptrace* nil) (setq *otrace* t) 0)
        ((equal z '(0.5))
         (setq *wtrace* nil)
         (setq *ptrace* t)
         (setq *ttrace* nil)
         (setq *otrace* t)
         0.5)
        ((equal z '(1))
         (setq *wtrace* nil)
         (setq *ptrace* t)
         (setq *ttrace* t)
         (setq *otrace* t)
         1)
        ((equal z '(1.5))
         (setq *wtrace* t)
         (setq *ttrace* nil)
         (setq *ptrace* t)
         (setq *otrace* t)
         1.5)
        ((equal z '(2))
         (setq *wtrace* t)
         (setq *ttrace* t)
         (setq *ptrace* t)
         (setq *otrace* t)
         2)
        ((equal z '(3))
         (setq *otrace* t)
         (setq *wtrace* t)
         (setq *ptrace* t)
         '(2 -- conflict set trace not supported))
        ((and (atom z)
              (null *otrace*))
         -1)
        ((and (atom z)
              (null *ptrace*))
         0)
        ((and (atom z)
              (null *wtrace*))
         1)
        ((atom z) 2)
        (t 'what?)))


(defun wm-hash (x)
  (car x))

(defun ops-write (z)
  (prog (port max k x needspace)
        (cond ((not *in-rhs*)
               (soarwarn "Write cannot be called at the top level" " ")
               (return nil)))
        ($reset)
        (eval-args z)
        (setq k 1)
        (setq max ($parametercount))
        (cond ((< max 1) (soarwarn "Write: nothing to print:" z) (return nil)))
        (setq port (default-write-file))
        (setq x ($parameter 1))
        (cond ((and (symbolp x)
                    ($ofile x))
               (setq port ($ofile x))
               (setq k 2)))
        (setq needspace t)
     la (and (> k max)
             (return nil))
        (setq x ($parameter k))
        (cond ((equal x 'crlf) (setq needspace nil) (terpri port))
              ((equal x 'rjust)
               (setq k (+ 2 k))
               (do-rjust ($parameter (1- k)) ($parameter k) port))
              ((equal x 'tabto)
               (setq needspace nil)
               (setq k (1+ k))
               (do-tabto ($parameter k) port))
              (t (and needspace
                      (princ " " port))
                 (setq needspace t)
                 (princ x port)))
        (setq k (1+ k))
        (go la)))


(defun add-negations (current-goal data-matched)
  (prog (negation-list return-list negation var-list condition new-condition element)
        (and (null *p-name*)
             (return nil))
        (and (null (get *p-name* 'negation-index))
             (return nil))
        (setq return-list nil)
        (setq negation-list (car (get *p-name* 'negation-index)))
        (setq var-list (cadr (get *p-name* 'negation-index)))
     l1 (cond ((null negation-list) (return return-list)))
        (setq condition (pop negation-list))
        (setq new-condition (list (pop condition)))
     l2 (cond ((null condition)
               (setq new-condition (nreverse new-condition))
               (soarpush '- return-list)
               (soarpush new-condition return-list)
               (go l1)))
        (soarpush (pop condition) new-condition)
        (soarpush (pop condition) new-condition)
        (cond ((variablep (car condition))
               (setq element (cdr (soarassq (pop condition) var-list)))
               (cond (element
                      (soarpush
                       (soarnth (cadr element) (soarnth (car element) data-matched))
                       new-condition))
                     (t (soarpush '<unbound> new-condition))))
              ((eq (car condition) '<<)
               (prog nil
                  l3 (soarpush (pop condition) new-condition)
                     (and condition
                          (neq (car new-condition) '>>)
                          (go l3))))
              (t (soarpush (pop condition) new-condition)))
        (cond ((eq current-goal (get (car new-condition) 'in-id-field)) (go l1)))
        (go l2)))

(defun ops-back-trace (objects)
  (prog (wme goal)
	(setq *unbound-action-ids* nil)
        (cond ((and (not (numberp (car objects)))
                    (get (car objects) 'goal-depth))
               (setq goal (pop objects)))
              ((null objects) (setq goal (most-recent-subgoal *context-stack*))))
        (cond ((null objects)
               (setq *created-list* (get goal 'created))
               (find-subgoal-nonresults goal nil *created-list*)
               (soarmapcar #'(lambda (x) (back-trace-conditions goal x t))
                (cond (*split-actions* (find-action-sets *subgoal-results* goal))
                      (t (list *subgoal-results*)))))
              (t (setq wme (conv-input-wme (pop objects)))
                 (setq goal
                        (cond ((null objects) (most-recent-subgoal *context-stack*))
                              (t (car objects))))
                 (setq *created-list* (get goal 'created))
                 (setq *subgoal-results* wme)
                 (back-trace-conditions goal wme t)))
        (setq *created-list* nil)
        (setq *subgoal-results* nil)
        (soarprint " ")))


(defun bind-it (action-list)
  (prog (variable-list)
        (setq variable-list (find-constant-ids action-list))
        (setq *learn-ids* (append variable-list *learn-ids*))
        (return variable-list)))

(defun build-a-production (condition-list action-list wme-list)
  (prog (new-chunk)
        (cond ((null action-list)
               (soarwarn "No chunk was built because there were no actions" " ")
               (return nil))
              ((> (length condition-list) *max-chunk-conditions*)
               (soarwarn
                "No chunk was built because *max-chunk-conditions* was exceeded:"
                (length condition-list))
               (return nil)))
     l1 (setq *p-name* (soargenpname 'p))
        (cond ((soarmemq *p-name* *pnames*) (go l1)))
        (setq *unbound* nil)
        (setq condition-list
               (nreverse (knotify-conditions (re-order-conditions condition-list))))
        (and *unbound*
             (setq condition-list (re-order-conditions condition-list)))
        (setq new-chunk
               (list (list 'quote *p-name*)
                     (list 'quote 'elaborate)
                     (list 'quote (nconc (nconc condition-list (list '-->)) action-list))))
        (cond ((cdrmember new-chunk *new-chunks*)
               (and *print-learn*
                    (soarprintc "Duplicate chunk"))
               (return t)))
        (and *tracep*
             (soarpush *p-name* *tracep-list*))
        (soarpush *p-name* *chunks*)
        (soarpush new-chunk *new-chunks*)
        (soarpush wme-list *wme-list-stack*)
        (return t)))

(defun build-copies (duplicate)
  (build-a-production
   (append *condition-list* (soarmapcar #'cdr *copy-action-list*) (list (car duplicate)))
   (rebind-action-ids (cdr duplicate) *learn-ids*) nil))

(defun build-duplicates (action-list class-list duplicate-variable duplicates negations)
  (prog (cond-list condition dup-list class)
        (setq dup-list nil)
        (setq *action-list* (prune-actions action-list duplicate-variable))
        (setq *condition-list* nil)
        (setq class-list (dremove 'goal-context-info class-list))
        (soarpush 'goal-context-info class-list)
     l1 (setq class (pop class-list))
        (cond ((null class)
               (cond ((null *action-list*)
                      (setq *condition-list* *save-condition*)
                      (setq *action-list* *save-action*)
                      (return nil)))
               (setq *condition-list* (nreverse (append negations *condition-list*)))
               (return dup-list)))
        (setq cond-list (get class 'condition))
        (soarputprop class nil 'condition)
     l2 (setq condition (pop cond-list))
        (cond ((null condition) (go l1))
              ((soarmemq condition duplicates)
               (soarpush (cons condition (dup-action condition)) dup-list))
              (t (soarpush condition *condition-list*)))
        (go l2)))

(defun build-terse (conditions)
  (prog (return-conditions change-flag element)
        (and *ltrace*
             (soarprinc "Conditions that are tersed out: "))
     l0 (setq change-flag nil)
        (setq return-conditions nil)
     l1 (setq element (pop conditions))
        (cond ((and (null element)
                    change-flag)
               (setq conditions (nreverse return-conditions))
               (go l0))
              ((null element)
               (and *ltrace*
                    (terpri (trace-file)))
               (return (nreverse return-conditions)))
              ((and (neq (class element) 'preference)
                    (free-condition element))
               (setq change-flag t)
               (and *ltrace*
                    (ppline (car (p-conds-to-sp (list (clean-up-element element))))))
               (untest (cdr element)))
              (t (soarpush element return-conditions)))
        (go l1)))

(defun build-variable-list (element)
  (and element
       (get element 'in-id-field)
       (setq *learn-ids* (cons (cons element (soargenvar element)) *learn-ids*))))

(defun clean-up-action (action)
  (cons 'make (clean-up-clause action 'action)))

(defun clean-up-clause (condition c-or-a)
  (prog (new-condition curpos att ppdat val class found-value element)
        (cond ((or (eq condition '-)
                   (eq (cadr condition) '^))
               (return condition)))
        (setq class (pop condition))
        (setq found-value nil)
        (setq new-condition (list class))
        (setq ppdat (get class 'ppdat))
        (cond ((null ppdat) (setq ppdat (get 'state-info 'ppdat))))
        (setq curpos 1)
     l1 (setq curpos (1+ curpos))
        (cond ((null condition)
               (and found-value
                    (return new-condition))
               (return (nconc new-condition '(^ value nil)))))
        (setq element (pop condition))
        (and (null element)
             (go l1))
        (setq att (cdr (soarassq curpos ppdat)))
        (and (eq att 'value)
             (setq found-value t))
        (or att
            (setq att curpos))
        (and (eq att 'value)
             (setq found-value t))
        (setq val (soarassq element *learn-ids*))
        (cond (val
               (setq val (cdr val))
               (and (eq c-or-a 'condition)
                    (soarputprop val
                     (1+ (or (get val 'tested)
                             0))
                     'tested)))
              (t (setq val element)))
        (nconc new-condition (list '^ att val))
        (go l1)))

(defun clear-var-list nil
  (setq *learn-ids* nil)
  (soarmapc #'soarclearprops *used-var-list*)
  (soarmapc #'(lambda (x) (soarputprop x 1 'var-count)) *used-char-list*)
  (setq *used-var-list* nil))

(defun closure-preference (action subgoal)
  (and (test-preference-field (soarnth *goal-index* action) subgoal)
       (test-preference-field (soarnth *problem-space-index* action) subgoal)
       (test-preference-field (soarnth *state-index* action) subgoal)
       (test-preference-field (soarnth *operator-index* action) subgoal)))

(defun compactify-conditions (condition-list action-list)
  (prog (class-list class-name same-class element duplicates duplicate-variable sclass
         negations)
        (setq duplicates nil)
        (setq *save-condition* condition-list)
        (setq *save-action* action-list)
        (setq negations nil)
        (setq class-list nil)
        (setq duplicate-variable nil)
     l1 (setq element (pop condition-list))
        (cond ((null element)
               (return (build-duplicates action-list (reverse class-list)
                        duplicate-variable duplicates negations)))
              ((eq element '-)
               (soarpush '- negations)
               (soarpush (pop condition-list) negations)
               (go l1)))
        (setq class-name (car element))
        (setq same-class (get class-name 'condition))
        (cond ((and (neq class-name 'preference)
                    (free-condition element))
               (setq same-class nil)))
     l4 (setq sclass (pop same-class))
        (cond ((null sclass)
               (addprop class-name 'condition element)
               (and (not (soarmemq class-name class-list))
                    (soarpush class-name class-list))
               (go l1)))
        (setq *suspected-duplicates* nil)
        (cond ((compare-conditions element sclass)
               (and (not (soarmemq sclass duplicates))
                    (soarpush sclass duplicates))
               (setq duplicate-variable (append duplicate-variable *suspected-duplicates*))
               (go l1)))
        (go l4)))

(defun create-production nil
  (prog (chunk wme-list)
        (and (null *new-chunks*)
             (return))
        (setq *new-chunks* (nreverse *new-chunks*))
        (setq *wme-list-stack* (nreverse *wme-list-stack*))
     l1 (cond ((null *new-chunks*) (setq *wme-list-stack* nil) (return)))
        (setq chunk (pop *new-chunks*))
        (setq wme-list (pop *wme-list-stack*))
        (eval (cons 'compile-production chunk))
        (cond ((eq 1 *print-learn*)
               (terpri (trace-file))
               (eval (list 'spm (cadar chunk))))
              (*print-learn* (soarprintc "Build:") (soarprinc (cadar chunk))))
        (refract-new-p chunk wme-list)
        (go l1)))

(defun dup-action (condition)
  (prog (element action action-list return-list save-action)
        (setq action-list *action-list*)
        (setq *action-list* nil)
        (setq return-list nil)
     l1 (setq action (pop action-list))
        (and (null action)
             (return return-list))
        (cond ((eq (cadr action) 'preference) (soarpush action *action-list*) (go l1)))
        (setq save-action action)
     l2 (setq element (pop action))
        (cond ((null element) (soarpush save-action *action-list*) (go l1))
              ((and (variablep element)
                    (soarmemq element (cddddr condition)))
               (soarpush save-action return-list)
               (go l1)))
        (go l2)))

(defun find-action-closure (listx)
  (soarmapcar #'(lambda (x) (cdr (soarassq x *learn-ids*))) (find-closure listx)))

(defun find-closure (listx)
  (cond ((null listx) listx)
        ((eq (car listx) '-) (find-closure (cddr listx)))
        (t (merge-not-time (cdar listx) (find-closure (cdr listx))))))

(defun find-constant-ids (action-list)
  (prog (action-ids element action)
        (setq action-ids nil)
     l1 (setq action (pop action-list))
        (and (null action)
             (return action-ids))
        (setq element (soarnth 4 action))
        (cond ((and (not (variablep element))
                    (not (soarassq element action-ids)))
               (soarpush (cons element (soargenvar element)) action-ids)))
        (go l1)))

(defun free-condition (elm1)
  (prog (carelm found-loser)
        (setq elm1 (cddddr elm1))
        (setq found-loser nil)
     l1 (setq elm1 (cddr elm1))
        (cond ((null elm1) (return found-loser)))
        (setq carelm (pop elm1))
        (and (numberp carelm)
             (go l1))
        (cond ((eq (get carelm 'tested) 1)
               (cond ((soarmemq carelm *action-closure*) (return nil))
                     (t (setq found-loser t)))))
        (go l1)))

(defun get-cav-value (class attribute)
  (cdr (soarassq attribute (cdr (soarassq class *cav*)))))

(defun get-cavi-id (class attribute value)
  (cdr (soarassq value (cdr (soarassq attribute (cdr (soarassq class *cavi*)))))))

(defun knotify (val variables)
  (prog (result)
        (setq result nil)
     l1 (setq result (append result (list '<> (car variables))))
        (setq variables (cdr variables))
        (and variables (go l1))
        (cond ((eq val '<unbound>) (setq *unbound* t) (return (append result '(}))))
              (t (return (append result (list val '})))))))

(defun knotify-conditions (conditions)
  (prog (condition attribute return-conditions new-condition class value values id ids
         idds s-condition variable-list)
        (setq variable-list
               (setq return-conditions
                      (setq *cav* (setq *cavi* (setq *variable-pairs* nil)))))
     l1 (setq condition (pop conditions))
        (cond ((eq condition '-) (soarpush condition return-conditions) (go l1))
              ((null condition) (setq *cav* (setq *cavi* nil)) (return return-conditions)))
        (setq s-condition condition)
        (setq new-condition (list (setq class (car condition))))
        (setq condition (cddr condition))
        (setq attribute 'worst)
        (setq id 'worst)
     l2 (cond ((eq (car condition) 'identifier) (setq id (cadr condition)))
              ((eq (car condition) 'attribute) (setq attribute (cadr condition)))
              ((eq (car condition) 'value)
               (setq value (cadr condition))
               (cond ((variablep value)
                      (setq values (get-cav-value class attribute))
                      (cond ((null values)
                             (put-cav-attribute class attribute)
                             (put-cav-value class attribute (list value))
                             (and (not (soarmemq value variable-list))
                                  (soarpush value variable-list)))
                            ((and (not (soarmemq value values))
                                  (not (soarmemq value variable-list)))
                             (put-cav-value class attribute (cons value values))
                             (setq new-condition
                                    (append new-condition
                                            '(^ value {)
                                            (knotify value values)))
                             (soarpush (cons value values) *variable-pairs*)
                             (setq variable-list (cons value variable-list))
                             (go l4))
                            ((not (soarmemq value values))
                             (put-cav-value class attribute (cons value values)))))
                     ((and (neq id 'worst)
                           (neq attribute 'worst)
                           (neq value '{))
                      (setq ids (get-cavi-id class attribute value))
                      (setq idds (cons id ids))
                      (cond ((null ids)
                             (put-cavi-attribute class attribute)
                             (put-cavi-value class attribute value)
                             (put-cavi-id class attribute value idds))
                            ((and (not (soarmemq id ids))
                                  (not (soarassoc id *variable-pairs*)))
                             (put-cavi-id class attribute value idds)
                             (soarpush idds *variable-pairs*)
                             (setq new-condition
                                    (append (list class '^ 'identifier '{)
                                            (knotify id ids)
                                            (list '^ 'attribute attribute '^ 'value value)))
                             (go l4))))
                     ((eq value '{) (nconc new-condition (cons '^ condition)) (go l4)))))
        (nconc new-condition (list '^ (car condition) (cadr condition)))
        (setq condition (cdddr condition))
        (and condition (go l2))
     l4 (soarpush new-condition return-conditions)
        (go l1)))

(defun ops-learn (z)
  (cond ((atom z) (print-status))
        (t (cond ((eq (car z) 'on)
                  (cond ((and *never-learn*
                              (neq *cycle-count* 0))
                         (terpri (trace-file))
                         (soarprint
                          "Error: Can not turn learning on.  Must init-soar first"))
                        (t (setq *learning* t) (setq *never-learn* nil))))
                 ((eq (car z) 'off)
                  (cond ((and *never-learn*
                              (neq *cycle-count* 0))
                         (terpri (trace-file))
                         (soarprint
                          "Error: Can not turn learning to off.  Must init-soar first"))
                        (t (setq *learning* nil) (setq *never-learn* nil))))
                 ((eq (car z) 'never) (setq *learning* nil) (setq *never-learn* t))
                 ((eq (car z) 'full-print) (setq *print-learn* 1))
                 ((eq (car z) 'print) (setq *print-learn* 0))
                 ((eq (car z) 'noprint) (setq *print-learn* nil))
                 ((eq (car z) 'full-trace) (setq *ltrace* t) (setq *tracep* t))
                 ((eq (car z) 'trace) (setq *tracep* t) (setq *ltrace* nil))
                 ((eq (car z) 'notrace) (setq *tracep* nil) (setq *ltrace* nil))
                 ((eq (car z) 'always) (setq *always-learn* t))
                 ((eq (car z) 'bottom-up) (setq *always-learn* nil))
                 (t (terpri (trace-file)) (soarprint "Error: unknown option")))
           (eval (cons 'learn (cdr z))))))


(defun learn-back-trace (old-context prior-context new-context subgoaled)
  (prog (return-value condition-list subgoal)
        (setq subgoal (get-current old-context 'goal))
        (setq *created-list* (get subgoal 'created))
        (setq *unbound-action-ids* nil)
        (soarmapc #'remove-from-wm
         (find-subgoal-nonresults subgoal (get-current prior-context 'goal)
          *created-list*))
        (cond (*never-learn* (soarclearprops subgoal) (return t)))
        (setq return-value
               (soarmapcar
                #'(lambda (x)
                    (handle-back-trace-actions x old-context prior-context new-context
                     subgoaled))
                (cond (*split-actions* (find-action-sets *subgoal-results* subgoal))
                      (t (list *subgoal-results*)))))
        (soarclearprops subgoal)
        (setq *subgoal-results* nil)
        (setq *learn-ids* nil)
        (return (t-in-list? return-value))))

(defun prune-actions (action-list duplicate-variable)
  (prog (action save-action return-list)
        (setq return-list nil)
     l1 (and (null action-list)
             (return return-list))
        (setq action (pop action-list))
        (setq save-action action)
     l2 (cond ((null action) (soarpush save-action return-list) (go l1))
              ((soarmemq (pop action) duplicate-variable) (go l1)))
        (go l2)))

(defun put-cav-attribute (class attribute)
  (cond ((soarassq class *cav*)
         (rplacd (soarassq class *cav*)
                 (cons (list attribute) (cdr (soarassq class *cav*)))))
        (t (setq *cav* (cons (cons class (list (list attribute))) *cav*)))))

(defun put-cav-value (class attribute value)
  (rplacd (soarassq attribute (cdr (soarassq class *cav*))) value))

(defun put-cavi-attribute (class attribute)
  (cond ((soarassq attribute (cdr (soarassq class *cavi*))))
        ((soarassq class *cavi*)
         (rplacd (soarassq class *cavi*)
                 (cons (list attribute) (cdr (soarassq class *cavi*)))))
        (t (setq *cavi* (cons (cons class (list (list attribute))) *cavi*)))))

(defun put-cavi-id (class attribute value id)
  (rplacd (soarassq value (cdr (soarassq attribute (cdr (soarassq class *cavi*))))) id))

(defun put-cavi-value (class attribute value)
  (rplacd (soarassq attribute (cdr (soarassq class *cavi*)))
          (cons (list value) (cdr (soarassq attribute (cdr (soarassq class *cavi*)))))))

(defun rebind-action-ids (action-list copy-list)
  (prog (element new-element return-list val val2)
        (setq return-list nil)
     l1 (and (null action-list)
             (return return-list))
        (setq element (pop action-list))
        (setq new-element nil)
     l2 (and (null element)
             (go l3))
        (setq val2 (pop element))
        (setq val (cdr (soarassq val2 copy-list)))
        (or val
            (setq val val2))
        (soarpush val new-element)
        (go l2)
     l3 (soarpush (nreverse new-element) return-list)
        (go l1)))

(defun refract-new-p (new-p wme-list)
  (prog (element prior-element)
        (setq prior-element nil)
        (setq new-p (cadar new-p))
     l1 (cond ((setq element (soarassq new-p *conflict-set*))
               (cond ((soarmemq element prior-element) (return t))
                     ((or (null wme-list)
                          (test-subset (cdr element) wme-list))
                      (setq *conflict-set* (dremove element *conflict-set*)))
		     (t (setq *conflict-set*
			      (append (dremove element *conflict-set*)
				      (list element)))))
               (soarpush element prior-element)
               (go l1))
              (t (return t)))))

(defun tersify-negations (negations)
  (prog (condition return-list new-condition id value)
        (setq return-list nil)
     l1 (cond ((null negations) (return return-list)))
        (setq condition (pop negations))
        (setq new-condition (list (pop condition)))
     l2 (cond ((null condition)
               (setq new-condition (nreverse new-condition))
               (and (soarmember new-condition return-list)
                    (go l1))
               (soarpush '- return-list)
               (soarpush new-condition return-list)
               (go l1)))
        (soarpush (pop condition) new-condition)
        (soarpush (pop condition) new-condition)
        (setq id (pop condition))
        (cond ((eq 1 (get id 'tested)) (soarpush '<unbound> new-condition) (go l2))
              (t (soarpush id new-condition) (go l2)))))

(defun test-chunk-for-content (conditions)
  (prog (condition)
     l1 (cond ((null conditions)
               (soarwarn
                "No chunk was built because no conditions had a class in *chunk-classes*"
                " ")
               (clear-var-list)
               (return t)))
        (setq condition (pop conditions))
        (cond ((eq condition '-) (go l1))
              ((soarmemq (conv-type-to-sp (car condition)) *chunk-classes*) (return nil)))
        (go l1)))

(defun ask-for-choice (choice-list)
  (prog (choice count)
        (setq *elapsed-time*
               (+ *elapsed-time* (- (alwaystime) *begin-time*)))
        (cond (*interlisp* (return (interlisp-menu choice-list))))
     l1 (terpri (trace-file))
        (soarprint "Choose from this list by position in list (1-n)")
        (setq count 1)
        (soarmapc
         #'(lambda (x) (soarprinc2 count ": ") (print-choice x) (setq count (1+ count)))
         choice-list)
        (princ "? " (trace-file))
        (setq choice (read))
        (cond ((and (numberp choice)
                    (> choice 0)
                    (< choice (1+ (length choice-list))))
               (setq *begin-time* (alwaystime))
               (return (soarnth (1- choice) choice-list))))
        (soarprintc "Your answer was not a number between 1 and n.")
        (go l1)))

(defun classify-decide (instance)
  (prog (pname node action goal slot value id id2 compared)
        (setq pname (car instance))
        (setq *data-matched* (cdr instance))
        (setq node (get pname 'topnode))
        (init-var-mem (var-part node))
        (init-ce-var-mem (ce-var-part node))
        (setq action (cdddr (cadr (rhs-part node))))
        (setq goal ($varbind (car action)))
        (setq action (cdddr action))
        (setq id ($varbind (car action)))
        (setq action (cdddr action))
        (setq slot ($varbind (car action)))
        (setq action (cdddr action))
        (setq value ($varbind (car action)))
        (setq action (cdddr action))
        (setq compared ($varbind (car action)))
        (cond ((eq value 'indifferent) (cond (compared (setq value 'equal))))
              ((eq value 'better)
               (setq value 'worse)
               (setq id2 compared)
               (setq compared id)
               (setq id id2)))
        (addprop goal 'preferences (list slot value id compared (car *data-matched*)))))

(defun decide-trace (arg)
  (setq *decide-trace* arg))

(defun decide-tracer (label list)
  (cond (*decide-trace*
         (terpri (trace-file))
         (soarprinc3 " " label " ")
         (soarmapc #'print-id list))))

(defun decision-procedure (value-list current)
  (prog (new-values saved-values)
        (setq new-values (find-acceptable value-list))
        (decide-tracer 'acceptable new-values)
        (cond ((null new-values)
               (cond ((and (neq current 'undecided)
                           (null (prune-rejects (list (list current)) value-list current)))
                      (return '(rejection))))
               (return '(no-change))))
        (setq new-values (process-indifferent new-values value-list))
        (decide-tracer 'post-indifferent new-values)
        (setq new-values (set-equals new-values value-list))
        (decide-tracer 'post-equal new-values)
        (setq new-values (prune-rejects new-values value-list current))
        (decide-tracer 'post-reject new-values)
        (cond ((or (null new-values)
                   (null (car new-values)))
               (return '(rejection))))
        (setq saved-values new-values)
        (setq new-values (prune-worse new-values value-list))
        (decide-tracer 'post-worse new-values)
        (and (null new-values)
             (return (cons 'conflict (soarsort (flatten saved-values)))))
        (and (null (car new-values))
             (return (cons 'conflict (soarsort (flatten saved-values)))))
        (setq new-values (select-best new-values value-list))
        (decide-tracer 'post-best new-values)
        (setq new-values (set-worst new-values value-list))
        (decide-tracer 'post-worst new-values)
        (and (cdr new-values)
             (return (test-parallel (soarsort (flatten new-values)) value-list 'tie)))
        (return (test-parallel new-values value-list nil))))

(defun find-acceptable (value-list)
  (dremove 'nil (flatten (find-all-assq 'acceptable value-list))))

(defun find-all-assq (item alist)
  (prog (alist2 element)
        (setq alist2 nil)
     l1 (and (null alist)
             (return alist2))
        (setq element (pop alist))
        (cond ((and (eq item (car element))
                    (not (soarmember (cdr element) alist2)))
               (soarpush (cdr element) alist2)))
        (go l1)))

(defun find-goal-preferences (context)
  (get (car context) 'preferences))

(defun find-no-change-slot (context)
  (cond ((eq (cadr context) 'undecided) 'goal)
        ((eq (caddr context) 'undecided) 'problem-space)
        ((eq (cadddr context) 'undecided) 'state)
        (t 'operator)))

(defun find-preference (data)
  (prog nil
     l1 (cond ((null data) (return nil))
              ((eq (caar data) 'preference) (return (car data))))
        (setq data (cdr data))
        (go l1)))

(defun find-worse (x ylist)
  (cond ((null ylist) nil) ((soarmemq x (car ylist)) t) (t (find-worse x (cdr ylist)))))

(defun make-info (info goal-id attribute value)
  (prog (array)
        (setq array (list info goal-id attribute value))
        (add-to-wm array)
        (return array)))

(defun process-decide (phase-set)
  (accum-stats)
  (setq *new-chunks* nil)
  (soarmapcar #'classify-decide phase-set)
  (process-context-stack *context-stack*))

(defun process-indifferent (new-values value-list)
  (prog (worst-set return-list new-equals old-equals worst-list item)
        (setq worst-list (find-all-assq 'indifferent value-list))
        (setq new-equals nil)
        (setq return-list nil)
     l1 (cond ((null new-values)
               (and new-equals
                    (return (cons new-equals return-list)))
               (return return-list)))
        (setq old-equals (pop new-values))
        (cond ((soarassq old-equals worst-list) (soarpush old-equals new-equals))
              (t (soarpush (list old-equals) return-list)))
        (go l1)))

(defun process-results (results id)
  (cond ((null (cdr results)) (car results))
        ((soarmemq id results) id)
        ((eq *select-equal* 'first) (car results))
        ((soarlistp *select-equal*)
         (cond ((find-selected-result (pop *select-equal*) results))
               (t (setq *select-equal* t) (ask-for-choice results))))
        (*select-equal* (ask-for-choice results))
        (t (soarnth (random (1- (length results))) results))))

(defun prune-rejects (new-values value-list current)
  (prog (return-list reject-list new-value new-value-list)
        (setq reject-list (dremove 'nil (flatten (find-all-assq 'reject value-list))))
        (setq return-list nil)
     l1 (cond ((null new-values)
               (cond ((and (null return-list)
                           (eq current 'undecided))
                      (return nil))
                     ((and (null return-list)
                           (soarlistp current))
                      (return (list (soarmapc
                                     #'(lambda (x) (setq current (remove x current)))
                                     reject-list))))
                     ((and (null return-list)
                           (not (soarmemq current reject-list)))
                      (return (list current))))
               (return return-list)))
        (setq new-value (pop new-values))
        (setq new-value-list nil)
     l2 (cond ((null new-value)
               (and new-value-list
                    (soarpush new-value-list return-list))
               (go l1))
              ((not (soarmemq (car new-value) reject-list))
               (soarpush (car new-value) new-value-list)))
        (pop new-value)
        (go l2)))

(defun prune-remove (remove-list)
  (prog (non-context-list wme)
        (setq non-context-list nil)
     l1 (and (null remove-list)
             (return (nreverse non-context-list)))
        (setq wme (pop remove-list))
        (cond ((and (eq (class wme) 'goal-context-info)
                    (soarmemq (sattribute wme) '(problem-space state operator)))
               (remove-from-wm wme))
              (t (soarpush wme non-context-list)))
        (go l1)))

(defun prune-worse (new-values value-list)
  (prog (equal-set return-list equals old-equals old-values item item2 worse-list hit
         worse-list2)
        (setq worse-list (find-all-assq 'worse value-list))
        (setq return-list nil)
        (setq old-values new-values)
     l1 (and (null new-values)
             (return return-list))
        (setq old-equals (setq equal-set (pop new-values)))
        (setq equals nil)
     l2 (cond ((null equal-set) (soarpush equals return-list) (go l1)))
        (setq item (pop equal-set))
        (setq worse-list2 worse-list)
     l4 (cond ((null worse-list2) (soarpush item equals) (go l2)))
        (setq item2 (pop worse-list2))
        (cond ((and (eq (car item2) item)
                    (neq item (cadr item2)))
               (setq hit (cadr item2))
               (cond ((soarmemq hit old-equals) (go l2))
                     ((find-worse hit old-values) (go l1)))))
        (go l4)))

(defun select-best (new-values value-list)
  (prog (return-list equal-list old-values best-list save-e-list e-item)
        (setq best-list (find-all-assq 'best value-list))
        (setq return-list nil)
        (setq old-values new-values)
     l1 (and (null new-values)
             (null return-list)
             (return old-values))
        (and (null new-values)
             (return (list return-list)))
        (setq equal-list (pop new-values))
        (setq save-e-list equal-list)
     l2 (and (null equal-list)
             (go l1))
        (setq e-item (pop equal-list))
        (cond ((soarassq e-item best-list) (soarpush e-item return-list)))
        (go l2)))

(defun user-select (x)
  (setq *select-equal* x))

(defun set-equals (new-values value-list)
  (prog (equal-set new-equals return-list hit hit-list old-hit-list equal-list
         equal-list2)
        (setq equal-list (find-all-assq 'equal value-list))
        (setq return-list nil)
        (setq new-equals (pop new-values))
        (setq equal-set nil)
        (setq old-hit-list nil)
        (setq hit-list nil)
     l2 (cond ((null new-equals)
               (and equal-set
                    (soarpush equal-set return-list))
               (and (null new-values)
                    (return return-list))
               (setq old-hit-list (append old-hit-list hit-list))
               (setq new-equals (pop new-values))
               (setq equal-set nil)
               (cond ((soarmemq (car new-equals) old-hit-list) (pop new-equals) (go l2)))))
        (setq equal-list2 equal-list)
     l4 (cond ((null equal-list2) (soarpush (pop new-equals) equal-set) (go l2)))
        (cond ((equal (caar equal-list2) (car new-equals))
               (setq hit (cadar equal-list2))
               (cond ((and (not (soarmemq hit equal-set))
                           (not (soarmemq hit new-equals))
                           (soarmemq hit (flatten new-values)))
                      (setq new-equals (append new-equals (list hit)))
                      (soarpush hit hit-list)))))
        (pop equal-list2)
        (go l4)))

(defun set-worst (new-values value-list)
  (prog (worst-set return-list new-equals old-equals worst-list item)
        (setq worst-list (find-all-assq 'worst value-list))
        (setq return-list nil)
     l1 (and (null new-values)
             (null return-list)
             (return (list worst-set)))
        (and (null new-values)
             (return return-list))
        (setq old-equals (pop new-values))
        (setq new-equals nil)
     l2 (cond ((null old-equals)
               (and new-equals
                    (soarpush new-equals return-list))
               (go l1)))
        (setq item (pop old-equals))
        (cond ((soarassq item worst-list) (soarpush item worst-set))
              (t (soarpush item new-equals)))
        (go l2)))

(defun test-parallel (results value-list difficulty)
  (prog (parallel-list parallel-results new-results)
        (setq parallel-list (find-all-assq 'parallel value-list))
        (setq results (flatten results))
        (and (null parallel-list)
             (return (cons difficulty results)))
        (setq parallel-results (list (car results)))
        (setq new-results (cdr results))
     l1 (cond ((null new-results)
               (decide-tracer 'parallel results)
               (return (cons 'parallel results))))
        (cond ((soarmember (list (car new-results) (car parallel-results)) parallel-list)
               (soarpush (pop new-results) parallel-results)
               (go l1))
              (t (return (cons difficulty results))))))

(defun change-context (difficulty results old-context slot context-stack preferences)
  (prog (new-context subgoaled)
        (cond ((and *wtrace*
                    (cdr context-stack)
                    (trace-problem-space?))
               (soarprintc "--Removal Phase--")))
        (setq new-context (create-new-context difficulty results old-context slot))
        (setq subgoaled
               (soarmapcar #'(lambda (x) (process-sub-contexts x new-context old-context))
                (cdr context-stack)))
        (cond ((t-in-list? subgoaled)
               (rplaca (soarnthcdr 9 old-context) t)
               (rplaca (soarnthcdr 9 new-context) t)))
        (cond ((and *wtrace*
                    (trace-problem-space?))
               (soarprintc "--Decision Phase--")))
        (cond (difficulty
               (write-trace " " difficulty slot)
               (create-production)
               (cond ((and (eq difficulty 'no-change)
                           (soarlistp results))
                      (rplacd context-stack
                              (soarmapcar
                               #'(lambda (x)
                                   (create-goal-context difficulty slot x old-context
                                    preferences))
                               results)))
                     (t (rplacd context-stack
                                (list (create-goal-context difficulty slot results
                                       old-context preferences)))))
               (return new-context))
              (t (write-trace " DECIDE " slot results)
                 (rplacd context-stack nil)
                 (change-to-new-context new-context old-context preferences)
                 (return new-context)))))

(defun change-to-new-context (new-context old-context preferences)
  (prog (slots new-contexts slot depth context-changes)
        (setq slots '(operator state problem-space))
        (setq new-contexts nil)
        (setq *p-name* 'decision-procedure)
        (setq *context* new-context)
        (setq depth (get-context-depth new-context))
        (setq context-changes nil)
     l1 (setq slot (pop slots))
        (cond ((null slot)
               (create-production)
               (soarmapc #'(lambda (x) (trace-object (car x) (cdr x) depth))
                context-changes)
               (soarmapc
                #'(lambda (x)
                    (cond ((neq (svalue x) 'undecided)
                           (setq *data-matched*
                                  (list (find-acceptable-preference preferences (svalue x))))
                           (setq *first-action* t))
                          (t (setq *data-matched* nil) (setq *first-action* t)))
                    (add-to-wm x))
                new-contexts)
               (return))
              ((neq (get-current new-context slot) (get-current old-context slot))
               (setq new-contexts
                      (nconc (remove-current (get-current old-context 'goal) slot
                              (get-current old-context slot)
                              (get-current new-context slot))
                             new-contexts))
               (soarpush (cons slot (get-current new-context slot)) context-changes)))
        (go l1)))

(defun context-stack nil
  (soarmapc #'print *context-stack*)
  t)

(defun create-goal-context (desired slot results context preferences)
  (prog (goal-id created-list result superoperator temp supergoal)
        (setq *p-name* 'decision-procedure)
        (setq *data-matched* nil)
        (setq supergoal (get-current context 'goal))
        (setq goal-id (soargensym 'g))
        (soarputprop goal-id (1+ (get supergoal 'goal-depth)) 'goal-depth)
        (setq *context*
               (list goal-id
                     'undecided
                     'undecided
                     'undecided
                     0
                     nil
                     nil
                     nil
                     results
                     nil
                     (1+ (get-context-depth context))))
        (setq created-list
               (list (make-info 'goal-context-info goal-id 'problem-space 'undecided)
                     (make-info 'goal-context-info goal-id 'supergoal supergoal)
                     (make-info 'goal-context-info goal-id 'state 'undecided)
                     (make-info 'goal-context-info goal-id 'operator 'undecided)
                     (make-info 'goal-context-info goal-id 'choices
                      (cond ((or (eq desired 'no-change)
                                 (eq desired 'rejection))
                             'none)
                            (t 'multiple)))))
        (cond ((eq desired 'rejection)
               (setq slot (cadr (soarmemq slot '(operator state problem-space goal))))
               (create-goal-info (find-reject-preferences preferences) goal-id)))
        (nconc created-list
               (list (make-info 'goal-context-info goal-id 'impasse desired)))
        (setq superoperator 'undecided)
        (setq temp nil)
        (cond ((eq desired 'no-change)
               (cond ((eq slot 'operator) (setq superoperator results))
                     ((eq slot 'state)
                      (setq temp
                             (list (list 'goal-context-info
                                         supergoal
                                         'operator
                                         'undecided))))
                     ((eq slot 'problem-space)
                      (setq temp
                             (list (list 'goal-context-info supergoal 'state 'undecided))))
                     ((eq slot 'goal)
                      (setq temp
                             (list (list 'goal-context-info
                                         supergoal
                                         'problem-space
                                         'undecided)))))))
        (create-goal-info temp goal-id)
        (nconc created-list (list (make-info 'goal-context-info goal-id 'role slot)))
        (create-goal-info
         (list (list 'goal-context-info supergoal 'operator superoperator)) goal-id)
        (nconc created-list
               (list (make-info 'goal-context-info goal-id 'superoperator superoperator)))
     l4 (cond ((or (not results)
                   (atom results))
               (soarputprop goal-id created-list 'created)
               (trace-object 'goal goal-id (1+ (get-context-depth context)))
               (return (list *context*))))
        (setq result (pop results))
        (cond ((eq slot 'state)
               (create-goal-info
                (list (list 'goal-context-info
                            supergoal
                            'operator
                            (get-current context 'operator))
                      (find-acceptable-preference preferences result))
                goal-id))
              (t (create-goal-info (list (find-acceptable-preference preferences result))
                  goal-id)))
        (nconc created-list (list (make-info 'goal-context-info goal-id 'item result)))
        (go l4)))

(defun create-new-context (difficulty result old-context slot)
  (prog (slots found-slot aslot return-list)
        (cond (difficulty
               (return (list (get-current old-context 'goal)
                             (get-current old-context 'problem-space)
                             (get-current old-context 'state)
                             (get-current old-context 'operator)
                             (get-context-pnumber old-context)
                             slot
                             difficulty
                             result
                             (get-context-super-operator old-context)
                             (get-context-subgoaled old-context)
                             (get-context-depth old-context)))))
        (setq slots '(goal problem-space state operator))
        (setq return-list nil)
        (setq found-slot nil)
     l1 (setq aslot (pop slots))
        (cond ((null aslot)
               (return (append return-list
                               (list (get-context-pnumber old-context)
                                     nil
                                     nil
                                     nil
                                     (get-context-super-operator old-context)
                                     (get-context-subgoaled old-context)
                                     (get-context-depth old-context))))))
        (cond (found-slot (setq return-list (append return-list '(undecided))))
              ((eq aslot slot)
               (setq found-slot t)
               (setq return-list (append return-list (list result))))
              (t (setq return-list
                        (append return-list (list (get-current old-context aslot))))))
        (go l1)))

(defun get-context-depth (context)
  (soarnth 10 context))

(defun get-context-difficulty (context)
  (soarnth 6 context))

(defun get-context-pnumber (context)
  (soarnth 4 context))

(defun get-context-results (context)
  (soarnth 7 context))

(defun get-context-slot (context)
  (soarnth 5 context))

(defun get-context-subgoaled (context)
  (soarnth 9 context))

(defun get-context-super-operator (context)
  (soarnth 8 context))

(defun get-current (context slot)
  (soarnth
   (cdr (soarassq slot '((goal . 0) (problem-space . 1) (state . 2) (operator . 3))))
   context))

(defun init-context (problem-space state operator)
  (prog (goal-name)
        (init-soar)
        (setq goal-name (soargensym 'g))
        (setq *context*
               (list goal-name
                     problem-space
                     state
                     operator
                     '0
                     'nil
                     'nil
                     'nil
                     'nil
                     'nil
                     '0))
        (setq *context-stack* (list *context*))
        (return goal-name)))

(defun neq-result (difficulty result context slot)
  (cond (difficulty
         (or (neq difficulty (get-context-difficulty context))
             (neq slot (get-context-slot context))
             (not (test-results-subset result context))))
        ((soarlistp result) (not (test-slot-subset result context)))
        (t (neq result (get-current context slot)))))

(defun process-a-parallel-operator (context-stack results prior-context)
  (prog nil
        (and (soarmemq (get-context-super-operator (car context-stack)) results)
             (return (list context-stack)))
        (process-sub-contexts context-stack prior-context prior-context)
        (return nil)))

(defun process-context (context context-stack)
  (prog (context-preferences slot-preferences preferences slots slot ids results
         difficulty pnumber current)
        (setq context-preferences (find-goal-preferences context))
        (remprop (car context) 'preferences)
        (setq pnumber (length context-preferences))
        (rplaca (soarnthcdr 4 context) pnumber)
        (setq slots '(problem-space state operator))
        (setq ids (cdr context))
     l1 (setq current (pop ids))
        (setq slot (pop slots))
        (cond ((null slot)
               (setq difficulty 'no-change)
               (setq slot (find-no-change-slot context))
               (setq current (get-current context slot))
               (cond ((neq-result difficulty nil context slot)
                      (rplaca context-stack
                              (change-context difficulty current context slot
                               context-stack nil))
                      (return t))
                     ((and (eq slot 'operator)
                           (soarlistp current))
                      (process-parallel-operators context context-stack)
                      (return nil))
                     (t (return nil)))))
        (setq slot-preferences (find-all-assq slot context-preferences))
        (setq preferences (soarmapcar #'(lambda (x) (car (reverse x))) slot-preferences))
        (setq slot-preferences
               (soarmapcar #'(lambda (x) (reverse (cdr (reverse x)))) slot-preferences))
        (cond (*decide-trace* (soarprintc slot) (soarprinc " ") (print-id current)))
        (setq results (decision-procedure slot-preferences current))
        (setq difficulty (pop results))
        (cond ((eq difficulty 'no-change) (go l1))
              ((not difficulty) (setq results (process-results results current)))
              ((eq difficulty 'parallel) (setq difficulty nil)))
        (cond ((neq-result difficulty results context slot)
               (and (broken2 results)
                    (setq *remaining-decide* 0))
               (rplaca context-stack
                       (change-context difficulty results context slot context-stack
                        preferences))
               (return t))
              (difficulty (return))
              ((eq results 'undecided) (setq slots nil)))
        (go l1)))

(defun process-context-stack (context-stack)
  (prog (context)
        (setq context (car context-stack))
        (cond ((null (cdr context-stack))
               (process-context context context-stack)
               (check-limits)
               (return))
              ((test-context context)
               (cond ((process-context context context-stack) (check-limits) (return))))
              (t (remprop (car context) 'preferences)))
        (soarmapc #'(lambda (x) (process-context-stack x)) (cdr context-stack))))

(defun process-parallel-operators (context context-stack)
  (rplacd context-stack
          (soarmapconc
           #'(lambda (x)
               (process-a-parallel-operator x (get-current context 'operator) context))
           (cdr context-stack)))
  (create-production))

(defun process-sub-contexts (context-stack new-context prior-context)
  (prog (old-context subgoaled)
        (setq old-context (car context-stack))
        (setq subgoaled nil)
        (cond ((cdr context-stack)
               (setq subgoaled
                      (t-in-list?
                       (soarmapcar
                        #'(lambda (x) (process-sub-contexts x old-context old-context))
                        (cdr context-stack))))))
        (return (learn-back-trace old-context prior-context new-context subgoaled))))

(defun push-context-stack (context)
  (soarpush context *context-stack*)
  (setq *context* context))

(defun test-context (context)
  (neq (length (find-goal-preferences context)) (get-context-pnumber context)))

(defun test-if-subset (subset set)
  (cond ((null subset) t)
        ((atom set) nil)
        ((soarmemq (car subset) set) (test-if-subset (cdr subset) set))
        (t nil)))

(defun test-results-subset (result context)
  (cond (*impasse-subset-not-equal* (equal result (get-context-results context)))
        ((test-if-subset result (get-context-results context))
         (rplaca (soarnthcdr 7 context) result)
         t)
        (t nil)))

(defun test-slot-subset (result context)
  (cond ((test-if-subset result (get-current context 'operator))
         (rplaca (soarnthcdr 3 context) result)
         t)
        (t nil)))

(defun all-bound? (vlist)
  (soar-do ((vl1 vlist (cdr vl1))) ((null vl1) t)
   (cond ((not (bound-var? (car vl1))) (return nil)))))

(defun bound-preference? (pr)
  (all-bound? (get-context-vars-in-pref pr)))

(defun bound-var? (v)
  (get v 'bound))

(defun check-negations (set-vars negations)
  (soar-do ((neg negations (cddr neg)) (active) (unactive))
   ((null neg) (cons active unactive))
   (cond ((all-bound? (get-all-vars-in-neg (car neg)))
          (soarpush '- active)
          (soarpush (strip-condition (car neg)) active))
         (t (soarpush '- unactive) (soarpush (car neg) unactive)))))

(defun classify (set-vars dup-vars conditions flag)
  (prog (attr-type attr-val val-type val-val temp c i mini result)
        (setq mini 102)
     l1 (setq c (pop conditions))
        (cond ((null c) (return (rplnode *global-pair* mini (nreverse result))))
              ((setq i (get-prev-rank c)) (cond (i (go l2))))
              ((eq (get-condition-type c) 'preference)
               (cond ((and (soarmemq 'role (cdr c))
                           (eq 'operator (cadr (soarmemq 'role (cdr c)))))
                      (setq i (+ 6 *default-multi*)))
                     (t (setq i 3)))
               (and flag
                    (put-rank c i)))
              (t (setq temp (get-attr-val-types c))
                 (setq attr-type (caar temp))
                 (setq val-type (cadr temp))
                 (setq attr-val (cdar temp))
                 (setq val-val (cddr temp))
                 (cond ((and (eq attr-type 'var)
                             (bound-var? attr-val))
                        (setq attr-type 'set-var)))
                 (cond ((and (eq val-type 'var)
                             (bound-var? val-val))
                        (setq val-type 'set-var)))
                 (cond ((and (eq attr-type 'const)
                             (eq val-type 'const))
                        (setq i 1)
                        (put-rank c 1))
                       ((and (soarmemq attr-type '(const set-var))
                             (soarmemq val-type '(const set-var)))
                        (setq i 2)
                        (and flag
                             (put-rank c 2)))
                       ((eq (get-condition-type c) 'goal-context-info)
                        (cond ((eq (get-mult-num (get-condition-type c) attr-val) 1)
                               (setq i 4))
                              (t (setq i 5)))
                        (put-rank c i))
                       ((and (eq attr-type 'const)
                             (eq val-type 'var)
                             (soarmemq val-val dup-vars))
                        (setq i (+ 6 (get-mult-num (get-condition-type c) attr-val))))
                       ((and (soarmemq attr-type '(var set-var))
                             (soarmemq attr-val dup-vars)
                             (soarmemq val-type '(var set-var))
                             (soarmemq val-val dup-vars))
                        (setq i 100))
                       (t (setq i 101) (put-rank c 101)))))
     l2 (cond ((< i mini) (setq mini i) (setq result (list c)))
              ((= i mini) (soarpush c result)))
        (go l1)))

(defun classify-match-elt (x)
  (cond ((eq (car x) '<<) (rplnode *global-pair* (skip-disjunction x) '(other)))
        ((predicatep (car x)) (rplnode *global-pair* (next-sym (next-sym x)) '(other)))
        ((variablep (car x)) (rplnode *global-pair* (next-sym x) (cons 'var (car x))))
        ((eq (car x) '//) (rplnode *global-pair* (next-sym x) (cons 'const (cadr x))))
        (t (rplnode *global-pair* (next-sym x) (cons 'const (car x))))))

(defun classify-term (x)
  (prog (result)
        (setq result '(other))
        (cond ((eq (car x) '{)
               (return (prog (next-elt)
                             (setq x (cdr x))
                        loop (cond ((eq (car x) '})
                                    (return (rplnode *global-pair* (cdr x) result)))
                                   (t (setq next-elt (pop-match-elt x))
                                      (cond ((soarmemq (car next-elt) '(var const))
                                             (setq result next-elt)))
                                      (go loop))))))
              (t (return (classify-match-elt x))))))

(defun find-all-vars-in-condition (condition)
  (soar-do ((c (cdddr condition)) (vars) (next-elt)) ((null c) vars)
   (cond ((variablep (car c)) (soarpush (car c) vars) (setq c (next-sym c)))
         ((eq (car c) '<<) (setq c (skip-disjunction c)))
         (t (setq c (next-sym c))))))

(defun find-best-condition (set-vars dup-vars active-conditions un-active-conditions ccl
                            recurse-level minsofar)
  (prog (ccl1 i c tie-conds minc minrank temp new-vars act-conds un-act-conds s-v best)
        (setq i (car ccl))
        (setq tie-conds (cdr ccl))
        (cond ((> i 101) (return nil))
              ((or (<= i 6)
                   (= i 101)
                   (and (null (cdr tie-conds))
                        (or (not (= i 7))
                            (= recurse-level 0)))
                   (= recurse-level *max-recurse*))
               (setq c (car tie-conds))
               (return (cons (list i) c))))
        (setq minrank '(102))
        (cond ((and minsofar
                    (or (> i (car minsofar))
                        (and (= i (car minsofar))
                             (cdr minsofar)
                             (= 1 (cadr minsofar)))))
               (return '((100)))))
     l2 (setq c (pop tie-conds))
        (and (null c)
             (go l3))
        (setq temp (get-attr-val-types c))
        (cond ((eq (cadr temp) 'const) (setq new-vars (list (cdar temp))))
              (t (setq new-vars (list (cddr temp)))))
        (setq act-conds (remove c active-conditions))
        (setq s-v (mark-as-set new-vars))
        (setq temp (get-conditions-with-vars s-v un-active-conditions))
        (setq act-conds (append (car temp) act-conds))
        (setq un-act-conds (cdr temp))
        (setq ccl1 (classify s-v dup-vars act-conds nil))
        (setq best
               (find-best-condition s-v dup-vars act-conds un-act-conds ccl1
                (+ 1 recurse-level) (cdr minrank)))
        (unmark-as-set s-v)
        (cond ((or (and (equal (car best) minrank)
                        (eq (get-id-var c) *last-value-var*))
                   (lexless (car best) minrank))
               (setq minrank (car best))
               (setq minc c)))
        (cond ((eq (car minrank) 1) (go l3)))
        (go l2)
     l3 (return (cons (cons i minrank) minc))))

(defun find-info-on-cond (c flag)
  (prog (label attr val id vars next-term prefvars)
        (setq attr '(const))
        (setq val '(const))
        (setq c (cdr c))
     l1 (setq c (cdr c))
        (cond ((null c)
               (cond (flag (return (list id vars prefvars nil)))
                     (t (return (list id vars (cons attr val) nil))))))
        (setq label (pop c))
        (setq next-term (pop-term c))
        (cond ((eq (car next-term) 'var) (soarpush (cdr next-term) vars)))
        (cond ((and flag
                    (soarmemq label '(goal problem-space state operator))
                    (eq (car next-term) 'var)
                    (soarpush (cdr next-term) prefvars))))
        (cond ((eq label 'identifier)
               (cond ((eq (car next-term) 'const)
                      (soarwarn "Constant identifier field in:  " c)
                      (setq id 'const))
                     ((eq (car next-term) 'other)
                      (soarwarn "Identifier field not constant or variable in:  " c)
                      (setq id nil))
                     (t (setq id (cdr next-term)))))
              ((eq label 'object)
               (cond ((eq (car next-term) 'const)
                      (soarwarn "Constant object field in:  " c)
                      (setq id 'const))
                     ((eq (car next-term) 'other)
                      (soarwarn "Object field not constant or variable in:  " c)
                      (setq id nil))
                     (t (setq id (cdr next-term)))))
              ((eq label 'attribute) (setq attr next-term))
              ((eq label 'value) (setq val next-term)))
        (go l1)))

(defun find-pred-vars-in-condition (c)
  (soar-do ((type) (varlist)) ((null c) varlist)
   (cond ((predicatep (car c))
          (setq c (next-sym c))
          (setq type (pop-match-elt c))
          (cond ((eq (car type) 'var) (setq varlist (cons (cdr type) varlist)))))
         ((eq (car c) '<<) (setq c (skip-disjunction c)))
         (t (setq c (next-sym c))))))

(defun get-all-vars-in-neg (neg)
  (car neg))

(defun get-attr-val-types (x)
  (cadr (cddar x)))

(defun get-condition-type (c)
  (cadr c))

(defun get-conditions-with-vars (vars cl)
  (cond ((null vars) nil)
        (t (soar-do ((cl1 cl (cdr cl1)) (result-with) (result-wo) (id))
            ((null cl1) (cons (nreverse result-with) (nreverse result-wo)))
            (setq id (get-id-var (car cl1)))
            (cond ((and (or (and (not (eq (get-condition-type (car cl1)) 'preference))
                                 (bound-var? id))
                            (eq id 'const)
                            (and (eq (get-condition-type (car cl1)) 'preference)
                                 (bound-preference? (car cl1))))
                        (no-unbound-predicates? vars (car cl1)))
                   (soarpush (car cl1) result-with))
                  (t (soarpush (car cl1) result-wo)))))))

(defun get-context-vars-in-pref (pr)
  (cadr (cddar pr)))

(defun get-id-var (c)
  (cadr (car c)))

(defun get-mult-num (type attr)
  (soar-do ((l *multi-attribute* (cdr l))) ((null l) 1)
   (cond ((and (eq (caar l) type)
               (eq (cadar l) attr))
          (cond ((car (cddar l)) (return (car (cddar l)))) (t (return *default-multi*)))))))

(defun get-pred-vars-in-condition (c)
  (car (car c)))

(defun get-prev-rank (c)
  (car (cddddr (car c))))

(defun get-twice-used-vars (cl)
  (prog (varlist dup-vars)
        (soar-do ((cl1 cl (cdr cl1))) ((null cl1))
         (setq varlist
                (append (get-pred-vars-in-condition (car cl1))
                        (append (get-vars-in-condition (car cl1)) varlist))))
        (soar-do ((vl varlist (cdr vl))) ((null vl))
         (cond ((and (soarmemq (car vl) (cdr vl))
                     (not (soarmemq (car vl) dup-vars)))
                (soarpush (car vl) dup-vars))))
        (return dup-vars)))

(defun get-vars-in-condition (c)
  (caddr (car c)))

(defun insert-ne-undec (c2 dup-vars)
  (prog (undecflag braceflag c1 var label c x)
        (setq c c2)
        (setq c (cdr c))
     l1 (cond ((null c) (return c2)))
        (setq undecflag nil)
        (setq braceflag nil)
        (setq var nil)
        (pop c)
        (setq label (pop c))
        (cond ((and (eq label 'attribute)
                    (not (soarmemq (car c) '(problem-space state operator))))
               (return c2)))
        (setq c1 c)
     l2 (setq x (car c))
        (cond ((eq x '{) (setq braceflag t) (pop c))
              ((and (eq x '<>)
                    (eq (cadr c) 'undecided))
               (setq undecflag t)
               (pop c)
               (pop c))
              ((and (not undecflag)
                    (not (eq label 'identifier))
                    (not (eq label 'object))
                    (variablep x)
                    (not (soarmemq x dup-vars)))
               (setq var x)
               (pop c))
              ((eq x '}) (go l3))
              (t (pop-match-elt c)))
        (cond (braceflag (go l2)))
     l3 (cond ((and var
                    (not undecflag))
               (cond (braceflag (rplacd c1 (append '(<> undecided) (cdr c1))))
                     (t (rplacd c1 (append '(<> undecided) (cons x (cons '} (cdr c1)))))
                        (rplaca c1 '{)))))
        (go l1)))

(defun lexless (a b)
  (soar-do ((a1 a (cdr a1)) (b1 b (cdr b1))) ((null a1) t)
   (cond ((null a1) (return t))
         ((null b1) (return nil))
         ((< (car a1) (car b1)) (return t))
         ((< (car b1) (car a1)) (return nil)))))

(defun mark-as-set (vlist)
  (cond ((or (null vlist)
             (equal vlist '(nil)))
         nil)
        (t (soar-do
            ((vl1 (cond ((atom vlist) (list vlist)) (t vlist)) (cdr vl1)) (result))
            ((null vl1) result)
            (cond ((and (atom (car vl1))
                        (not (get (car vl1) 'bound)))
                   (soarputprop (car vl1) t 'bound)
                   (soarpush (car vl1) result)))))))

(defun next-sym (x)
  (cond ((eq (car x) '//) (cddr x)) (t (cdr x))))

(defun no-unbound-predicates? (vars c)
  (all-bound? (get-pred-vars-in-condition c)))

(defun predicatep (x)
  (and (symbolp x)
       (get x 'predicate)))

(defun put-rank (c n)
  (rplaca (cddddr (car c)) n))

(defun re-order-conditions (cl)
  (prog (un-active-conditions new-conditions set-vars dup ccl best active-conditions
         negations temp already-no-active-conditions)
        (cond ((null cl) (return nil)))
        (setq cl (sort-conditions cl))
        (setq *last-value-var* nil)
        (setq negations (cadr cl))
        (setq cl (car cl))
        (setq dup (get-twice-used-vars cl))
        (setq un-active-conditions cl)
        (setq active-conditions nil)
        (go l4)
     l0 (cond ((null un-active-conditions)
               (cond (negations
                      (soar-do nil ((null negations)) (soarpush '- new-conditions)
                       (soarpush (strip-condition (car negations)) new-conditions)
                       (setq negations (cddr negations)))))
               (unmark-as-set set-vars)
               (setq *condition-vars* set-vars)
               (return (nreverse new-conditions)))
              ((or already-no-active-conditions
                   (neq 'goal-context-info
                        (get-condition-type (car un-active-conditions))))
               (soarpush (strip-condition (car un-active-conditions)) new-conditions)
               (setq un-active-conditions (cdr un-active-conditions))
               (setq already-no-active-conditions nil)
               (cond ((neq (class (car new-conditions)) 'preference)
                      (soarwarn " Condition not linked to previous conditions "
                       (car (p-to-sp (list (car new-conditions)))))))
               (go l0)))
     l4 (setq already-no-active-conditions t)
        (setq set-vars
               (append (mark-as-set (get-id-var (car un-active-conditions))) set-vars))
     l3 (setq temp (get-conditions-with-vars set-vars un-active-conditions))
        (setq active-conditions (append (car temp) active-conditions))
        (setq un-active-conditions (cdr temp))
     l1 (cond ((null active-conditions) (go l0)))
        (setq ccl (classify set-vars dup active-conditions t))
        (setq best
               (find-best-condition set-vars dup active-conditions un-active-conditions
                ccl 0 nil))
        (setq already-no-active-conditions nil)
        (cond ((null best) (go l0)))
        (cond (*order-trace*
               (progn (soarprint (cons (car best) (strip-condition (cdr best)))))))
        (setq best (cdr best))
        (setq active-conditions (dremove best active-conditions))
     l2 (setq set-vars (append (mark-as-set (get-vars-in-condition best)) set-vars))
        (setq temp (get-attr-val-types best))
        (cond ((eq (cadr temp) 'var) (setq *last-value-var* (cddr temp)))
              (t (setq *last-value-var* nil)))
        (setq temp (strip-condition best))
        (cond ((eq (get-condition-type best) 'goal-context-info)
               (insert-ne-undec temp dup)))
        (soarpush temp new-conditions)
        (setq temp (check-negations set-vars negations))
        (setq new-conditions (append (car temp) new-conditions))
        (setq negations (cdr temp))
        (go l3)))

(defun reorder-p-conds (spxs)
  (prog (type conds action)
        (pop spxs)
        (setq *p-name* (pop spxs))
        (setq type
               (cond ((or (eq (car spxs) 'elaborate-once)
                          (eq (car spxs) 'elaborate)
                          (eq (car spxs) 'decide))
                      (pop spxs))
                     (t 'elaborate)))
        (setq conds nil)
condloop (soarpush (pop spxs) conds)
        (cond ((and spxs
                    (neq (car spxs) '-->))
               (go condloop)))
        (setq action spxs)
        (setq spxs (append (re-order-conditions (nreverse conds)) spxs))
        (test-connected-actions action)
        (soarpush type spxs)
        (soarpush *p-name* spxs)
        (soarpush 'p spxs)
        (return spxs)))

(defun skip-disjunction (x)
  (soar-do nil ((eq (car x) '>>) (cdr x)) (setq x (next-sym x))))

(defun sort-conditions (cl)
  (prog (regulars c negations goal-contexts)
        (setq negations nil)
        (setq regulars nil)
        (setq goal-contexts nil)
     l1 (setq c (pop cl))
        (cond ((null c)
               (return (list (append (nreverse goal-contexts) (nreverse regulars))
                             (nreverse negations))))
              ((eq c '-)
               (setq c (pop cl))
               (soarpush (cons (find-all-vars-in-condition c) c) negations)
               (unmark-as-set (get-all-vars-in-neg (car negations)))
               (soarpush '- negations))
              ((eq (car c) 'preference)
               (soarpush
                (cons (cons (find-pred-vars-in-condition c)
                            (find-info-on-cond c 'preference))
                      c)
                regulars)
               (unmark-as-set (get-pred-vars-in-condition (car regulars)))
               (unmark-as-set (get-vars-in-condition (car regulars))))
              ((eq (car c) 'goal-context-info)
               (soarpush
                (cons (cons (find-pred-vars-in-condition c) (find-info-on-cond c nil)) c)
                goal-contexts)
               (unmark-as-set (get-pred-vars-in-condition (car goal-contexts)))
               (unmark-as-set (get-vars-in-condition (car goal-contexts))))
              (t (soarpush
                  (cons (cons (find-pred-vars-in-condition c) (find-info-on-cond c nil))
                        c)
                  regulars)
                 (unmark-as-set (get-pred-vars-in-condition (car regulars)))
                 (unmark-as-set (get-vars-in-condition (car regulars)))))
        (go l1)))

(defun strip-condition (x)
  (cond ((atom x) x) (t (cdr x))))

(defun unmark-as-set (vlist)
  (soar-do ((vl1 (cond ((atom vlist) (list vlist)) (t vlist)) (cdr vl1)))
   ((null vl1) vlist) (remprop (car vl1) 'bound)))

(start-soar)
(terpri)
