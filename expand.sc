;;;; expand.sc -- expands syntax into more primitive forms for compilation
;;;;
;;;; Richard Brooksby, ????-??-??
;;;;
;;;; $Id$
;;;;
;;;; 2004-08-08  RB  Added document history.  Implemented expand-quasiquote.
;;;;

(define (singleton? list) (and (pair? list) (null? (cdr list))))

(define (expand-sequence sequence)
  (cond
    ((null? sequence) (error "empty sequence"))
    ((singleton? sequence) (car sequence))
    (else (cons 'begin sequence))))

;;; expand-cond cond
;;;
;;; Expands "cond" conditionals into expressions using only
;;; "if" and "or".
;;;
;;; @@@@ Doesn't cope with "=>" syntax.

(define (expand-cond cond)
  (if (not (cond? cond))
    (error "expand-cond applied to non-cond expression"))
  (if (null? (cond-clauses cond))
    (error "no clauses"))
  (let expand-clauses ((clauses (cond-clauses cond)))
    (and (not (null? clauses))
      (let ((clause (car clauses))
            (rest (expand-clauses (cdr clauses))))
        (if (not (pair? clause))
          (error "illegal clause"))
        (if (cond-else-clause? clause)
          (if rest
            (error "else clause must come last")
            (expand-sequence (cond-actions clause)))
          (if (singleton? clause)
            (if rest
              (list 'or (cond-predicate clause) rest)
              (cond-predicate clause))
            (let ((actions (expand-sequence (cond-actions clause))))
              (if rest
                (list 'if (cond-predicate clause) actions rest)
                (list 'if (cond-predicate clause) actions)))))))))

;;; expand-named-let

(define (expand-named-let let)
  (if (not (named-let? let))
    (error "expand-named-let applied to non-named-let expression"))
  `((letrec ((,(named-let-name let)
              (lambda (,@(map let-binding-variable (named-let-bindings let)))
                ,@(named-let-body let))))
      ,(named-let-name let))
    ,@(map let-binding-value (named-let-bindings let))))

;;; expand-case
;;;
;;; @@@@ Too much duplicated code with "expand-cond"

(define (expand-case case)
  (if (not (case? case))
    (error "expand-case applied to non-case expression"))
  (if (null? (case-clauses case))
    (error "no clauses"))
  (define key-symbol (generate-symbol))
  `(let ((,key-symbol ,(case-key case)))
    ,(let expand-clauses ((clauses (case-clauses case)))
      (and (not (null? clauses))
        (let ((clause (car clauses))
              (rest (expand-clauses (cdr clauses))))
          (if (not (pair? clause))
            (error "illegal clause"))
          (if (case-else-clause? clause)
            (if rest
              (error "else clause must come last")
              (expand-sequence (case-actions clause)))
            (let ((actions (expand-sequence (case-actions clause)))
                  (test `(memv ,key-symbol ',(case-data clause))))
              (if (not (list? (case-data clause)))
                (error "case data must be in a list"))
              (if rest
                (list 'if test actions rest)
                (list 'if test actions)))))))))

;;; expand-delay
;;;
;;; Expands "delay" into a call to "make-promise" much as described in
;;; R5RS.

(define (expand-delay delay)
  `(make-promise (lambda () ,(delay-exp delay))))

;;; expand-quasiquote
;;;
;;; Expands "quasiquote" into quite a large expression which will cons
;;; up its arguments with stuff unquoted.

(define (expand-quasiquote form)
  (define (expand exp level)
    (cond
      ((pair? exp)
        (cond
          ;; ,<exp>
          ;; If level is zero then expands to exp, else
          ;; expands to (list 'unquote <expand exp (- level 1)>)
          ((eq? (car exp) 'unquote)
            (if (not (and (pair? (cdr exp)) (null? (cddr exp))))
              (error "quasiquote: malformed unquote"))
            (if (= level 0)
              (cadr exp)
              (list 'list
                    ''unquote
                    (expand (cadr exp) (- level 1)))))
          ;; `<template>
          ;; Expands to (list 'quasiquote <expand template (+ level 1)>)
          ((eq? (car exp) 'quasiquote)
            (if (not (and (pair? (cdr exp)) (null? (cddr exp))))
              (error "quasiquote: malformed nested quasiquote"))
            (list 'list
                  ''quasiquote
                  (expand (cadr exp) (+ level 1))))
          ;; ,@<exp>
          ((eq? (car exp) 'unquote-splicing)
            (error "quasiquote: found unquote-splicing outside list"))
          ;; (,@<exp> . <template>)
          ;; If level is zero then expands to
          ;; (append exp <expand template level>) else expands to
          ;; (cons (list 'unquote-splicing <expand exp (- level 1)>)
          ;;       <expand template level>)
          ((and (pair? (car exp)) (eq? (caar exp) 'unquote-splicing))
            (if (not (and (pair? (cdar exp)) (null? (cddar exp))))
              (error "quasiquote: malformed unquote-splicing"))
            (if (= level 0)
              (list 'append
                    (cadar exp)
                    (expand (cdr exp) level))
              (list 'cons
                    (list 'list
                          ''unquote-splicing
                          (expand (cadar exp) (- level 1)))
                    (expand (cdr exp) level))))
          ;; (<template 1> . <template 2>)
          (else
            (list 'cons
                  (expand (car exp) level)
                  (expand (cdr exp) level)))))
      ((vector? exp)
        (list 'list->vector
              (expand (vector->list exp) level)))
      (else
        (list 'quote
              exp))))
  (if (not (and (pair? form)
                (list? form)
                (eq? (car form) 'quasiquote)))
    (error "quasiquote: illegal syntax"))
  (expand (cadr form) 0))

;;; expand-do

(define (expand-do form)
  (define bindings (do-bindings form))
  (define exit (do-exit form))
  (define commands (do-commands form))
  (define vars (map1 do-binding-variable bindings))
  (define inits (map1 do-binding-init bindings))
  (define steps (map1 do-binding-step bindings))
  (define test (do-exit-test exit))
  (define exit-exps (do-exit-exps exit))
  (define loop-symbol (generate-symbol))
  `(letrec ((,loop-symbol
              (lambda ,vars
                (if ,test
                  ,(if (null? exit-exps)
                     '(values)
                     (cons 'begin exit-exps))
                  (begin
                    ,@commands
                    (,loop-symbol ,@steps))))))
    (,loop-symbol ,@inits)))
