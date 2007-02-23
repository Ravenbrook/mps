;;; -*- Log: code.log; Mode: Lisp; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/backq.lisp,v 1.13 2004/07/02 16:29:04 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    BACKQUOTE: Code Spice Lispified by Lee Schumacher.
;;;   		  (unparsing by Miles Bader)
;;;
(in-package "LISP")


;;; The flags passed back by BACKQUOTIFY can be interpreted as follows:
;;;
;;;   |`,|: [a] => a
;;;    NIL: [a] => a		;the NIL flag is used only when a is NIL
;;;      T: [a] => a		;the T flag is used when a is self-evaluating
;;;  QUOTE: [a] => (QUOTE a)
;;; APPEND: [a] => (APPEND . a)
;;;  NCONC: [a] => (NCONC . a) 
;;;   LIST: [a] => (LIST . a)
;;;  LIST*: [a] => (LIST* . a)
;;;
;;; The flags are combined according to the following set of rules:
;;;  ([a] means that a should be converted according to the previous table)
;;;
;;;   \ car  ||    otherwise    |    QUOTE or     |     |`,@|      |     |`,.|
;;;cdr \     ||                 |    T or NIL     |                |		 
;;;================================================================================
;;;  |`,|    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a [d])
;;;  NIL     || LIST    ([a])   | QUOTE    (a)    | <hair>    a    | <hair>    a
;;;QUOTE or T|| LIST* ([a] [d]) | QUOTE  (a . d)  | APPEND (a [d]) | NCONC (a [d])
;;; APPEND   || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a . d) | NCONC (a [d])
;;; NCONC    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC (a . d)
;;;  LIST    || LIST  ([a] . d) | LIST  ([a] . d) | APPEND (a [d]) | NCONC (a [d])
;;;  LIST*   || LIST* ([a] . d) | LIST* ([a] . d) | APPEND (a [d]) | NCONC  (a [d])
;;;
;;;<hair> involves starting over again pretending you had read ".,a)" instead
;;; of ",@a)"

(defvar *backquote-count* 0  "How deep we are into backquotes")
(defvar *bq-comma-flag* '(|,|))
(defvar *bq-at-flag* '(|,@|))
(defvar *bq-dot-flag* '(|,.|))
(defvar *bq-vector-flag* '(|bqv|))
(defvar *bq-tokens*
  '(backq-comma backq-comma-at backq-comma-dot backq-list
    backq-list* backq-append backq-nconc backq-cons backq-vector))
  
;; This is the actual character macro.
(defun backquote-macro (stream ignore)
  (declare (ignore ignore))
  (let ((*backquote-count* (1+ *backquote-count*)))
    (multiple-value-bind (flag thing)
			 (backquotify stream (read stream t nil t))
      (if (eq flag *bq-at-flag*)
	  (%reader-error stream ",@ after backquote in ~S" thing))
      (if (eq flag *bq-dot-flag*)
	  (%reader-error stream ",. after backquote in ~S" thing))
      (values (backquotify-1 flag thing) 'list))))

(defun comma-macro (stream ignore)
  (declare (ignore ignore))
  (unless (> *backquote-count* 0)
    (when *read-suppress*
      (return-from comma-macro nil))
    (%reader-error stream "Comma not inside a backquote."))
  (let ((c (read-char stream))
	(*backquote-count* (1- *backquote-count*)))
    (values
     (cond ((char= c #\@)
	    (cons *bq-at-flag* (read stream t nil t)))
	   ((char= c #\.)
	    (cons *bq-dot-flag* (read stream t nil t)))
	   (t (unread-char c stream)
	      (cons *bq-comma-flag* (read stream t nil t))))
     'list)))

;;;
(defun expandable-backq-expression-p (object)
  (and (consp object)
       (let ((flag (car object)))
         (or (eq flag *bq-at-flag*)
             (eq flag *bq-dot-flag*)))))


;;; This does the expansion from table 2.
(defun backquotify (stream code)
  (cond ((atom code)
	 (cond ((null code) (values nil nil))
	       ((or (consp code)
		    (symbolp code))
		;; Keywords are self evaluating. Install after packages.
		(values 'quote code))
	       (t (values t code))))
	((or (eq (car code) *bq-at-flag*)
	     (eq (car code) *bq-dot-flag*))
	 (values (car code) (cdr code)))
	((eq (car code) *bq-comma-flag*)
	 (comma (cdr code)))
	((eq (car code) *bq-vector-flag*)
	 (multiple-value-bind (dflag d) (backquotify stream (cdr code))
	   (values 'vector (backquotify-1 dflag d))))
	(t (multiple-value-bind (aflag a) (backquotify stream (car code))
	     (multiple-value-bind (dflag d) (backquotify stream (cdr code))
	       (if (eq dflag *bq-at-flag*)
		   ;; get the errors later.
		   (%reader-error stream ",@ after dot in ~S" code))
	       (if (eq dflag *bq-dot-flag*)
		   (%reader-error stream ",. after dot in ~S" code))
	       (cond
		((eq aflag *bq-at-flag*)
		 (if (null dflag)
		     (if (expandable-backq-expression-p a)
                         (values 'append (list a))
                         (comma a))
		     (values 'append
			     (cond ((eq dflag 'append)
				    (cons a d ))
				   (t (list a (backquotify-1 dflag d)))))))
		((eq aflag *bq-dot-flag*)
		 (if (null dflag)
		     (if (expandable-backq-expression-p a)
                         (values 'nconc (list a))
                         (comma a))
		     (values 'nconc
			     (cond ((eq dflag 'nconc)
				    (cons a d))
				   (t (list a (backquotify-1 dflag d)))))))
		((null dflag)
		 (if (memq aflag '(quote t nil))
		     (values 'quote (list a))
		     (values 'list (list (backquotify-1 aflag a)))))
		((memq dflag '(quote t))
		 (if (memq aflag '(quote t nil))
		     (values 'quote (cons a d ))
		     (values 'list* (list (backquotify-1 aflag a)
					  (backquotify-1 dflag d)))))
		(t (setq a (backquotify-1 aflag a))
		   (if (memq dflag '(list list*))
		       (values dflag (cons a d))
		       (values 'list*
			       (list a (backquotify-1 dflag d)))))))))))

;;; This handles the <hair> cases 
(defun comma (code)
  (cond ((atom code)
	 (cond ((null code)
		(values nil nil))
	       ((or (numberp code) (eq code 't))
		(values t code))
	       (t (values *bq-comma-flag* code))))
	((and (eq (car code) 'quote)
	      (not (expandable-backq-expression-p (cadr code))))
	 (values (car code) (cadr code)))
	((memq (car code) '(append list list* nconc))
	 (values (car code) (cdr code)))
	((eq (car code) 'cons)
	 (values 'list* (cdr code)))
	(t (values *bq-comma-flag* code))))

;;; This handles table 1.
(defun backquotify-1 (flag thing)
  (cond ((or (eq flag *bq-comma-flag*)
	     (memq flag '(t nil)))
	 thing)
	((eq flag 'quote)
	 (list  'quote thing))
	((eq flag 'list*)
         (cond ((and (null (cddr thing))
                     (not (expandable-backq-expression-p (cadr thing))))
		(cons 'backq-cons thing))
	       ((expandable-backq-expression-p (car (last thing)))
                (list 'backq-append
                      (cons 'backq-list (butlast thing))
                      ;; Can it be optimized further? -- APD, 2001-12-21
                      (car (last thing))))
               (t
		(cons 'backq-list* thing))))
	((eq flag 'vector)
	 (list 'backq-vector thing))
	(t (cons (cdr
		  (assq flag
			'((cons . backq-cons)
			  (list . backq-list)
			  (append . backq-append)
			  (nconc . backq-nconc))))
		 thing))))


;;;; Magic backq- versions of builtin functions.

;;; Use synonyms for the lisp functions we use, so we can recognize backquoted
;;; material when pretty-printing

(defun backq-list (&rest args)
  args)
(defun backq-list* (&rest args)
  (apply #'list* args))
(defun backq-append (&rest args)
  (apply #'append args))
(defun backq-nconc (&rest args)
  (apply #'nconc args))
(defun backq-cons (x y)
  (cons x y))

(macrolet ((frob (b-name name)
	     `(define-compiler-macro ,b-name (&rest args)
		`(,',name ,@args))))
  (frob backq-list list)
  (frob backq-list* list*)
  (frob backq-append append)
  (frob backq-nconc nconc)
  (frob backq-cons cons))

(defun backq-vector (list)
  (declare (list list))
  (coerce list 'simple-vector))


;;;; Unparsing

(defun backq-unparse-expr (form splicing)
  (ecase splicing
    ((nil)
     `(backq-comma ,form))
    ((t)
     `((backq-comma-at ,form)))
    (:nconc
     `((backq-comma-dot ,form)))
    ))

(defun backq-unparse (form &optional splicing)
  "Given a lisp form containing the magic functions BACKQ-LIST, BACKQ-LIST*,
  BACKQ-APPEND, etc. produced by the backquote reader macro, will return a
  corresponding backquote input form.  In this form, `,' `,@' and `,.' are
  represented by lists whose cars are BACKQ-COMMA, BACKQ-COMMA-AT, and
  BACKQ-COMMA-DOT respectively, and whose cadrs are the form after the comma.
  SPLICING indicates whether a comma-escape return should be modified for
  splicing with other forms: a value of T or :NCONC meaning that an extra
  level of parentheses should be added."
  (cond
   ((atom form)
    (backq-unparse-expr form splicing))
   ((not (null (cdr (last form))))
    "### illegal dotted backquote form ###")
   (t
    (case (car form)
      (backq-list
       (mapcar #'backq-unparse (cdr form)))
      (backq-list*
       (do ((tail (cdr form) (cdr tail))
	    (accum nil))
	   ((null (cdr tail))
	    (nconc (nreverse accum)
		   (backq-unparse (car tail) t)))
	 (push (backq-unparse (car tail)) accum)))
      (backq-append
       (mapcar #'(lambda (el) (backq-unparse el t))
	       (cdr form)))
      (backq-nconc
       (mapcar #'(lambda (el) (backq-unparse el :nconc))
	       (cdr form)))
      (backq-cons
       (cons (backq-unparse (cadr form) nil)
	     (backq-unparse (caddr form) t)))
      (backq-vector
       (coerce (backq-unparse (cadr form)) 'vector))
      (quote
       (cadr form))
      (t
       (backq-unparse-expr form splicing))))))

(defun pprint-backquote (stream form &rest noise)
  (declare (ignore noise))
  (write-char #\` stream)
  (write (backq-unparse form) :stream stream))

(defun pprint-backq-comma (stream form &rest noise)
  (declare (ignore noise))
  (ecase (car form)
    (backq-comma
     (write-char #\, stream))
    (backq-comma-at
     (princ ",@" stream))
    (backq-comma-dot
     (princ ",." stream)))
  (write (cadr form) :stream stream))


;;;; BACKQ-INIT and BACKQ-PP-INIT

(set-macro-character #\` #'backquote-macro)
(set-macro-character #\, #'comma-macro)

;;; BACKQ-PP-INIT -- interface.
;;;
;;; This is called by PPRINT-INIT.  This must be seperate from BACKQ-INIT
;;; because SET-PPRINT-DISPATCH doesn't work until the compiler is loaded.
;;;
(defun backq-pp-init ()
  (set-pprint-dispatch '(cons (eql backq-list)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-list*)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-append)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-nconc)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-cons)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-vector)) #'pprint-backquote)
  
  (set-pprint-dispatch '(cons (eql backq-comma)) #'pprint-backq-comma)
  (set-pprint-dispatch '(cons (eql backq-comma-at)) #'pprint-backq-comma)
  (set-pprint-dispatch '(cons (eql backq-comma-dot)) #'pprint-backq-comma))
