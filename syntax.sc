;;;; syntax.sc -- procedures for recognizing and decomposing Scheme syntax
;;;;
;;;; Richard Brooksby, @@@@
;;;;
;;;; $Id$
;;;;
;;;; DOCUMENT HISTORY
;;;;
;;;; 2004-07-05  RB  Started document history.  Fixed
;;;; "procedure-definition?" to cope with improper argument lists.  Separating
;;;; test for "if" alternative from access, so that "(if x y #f)" works.

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (char? exp)
      (boolean? exp)))

(define (quoted? exp) (tagged-list? exp 'quote))
(define text-of-quotation cadr)

(define variable? symbol?)

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (procedure-definition? exp)
  (and (definition? exp)
       (pair? (cadr exp))))
(define (definition-variable exp)
  (if (procedure-definition? exp)
    (caadr exp)
    (cadr exp)))
(define (definition-value exp)
  (if (procedure-definition? exp)
    (cons 'lambda		; @@@@ What if "lambda" rebound?
          (cons (cdadr exp)
                (cddr exp)))
    (caddr exp)))

(define (sequence? exp) (tagged-list? exp 'begin))
(define sequence-clauses cdr)

(define (lambda? exp) (tagged-list? exp 'lambda))
(define lambda-parameters cadr)
(define lambda-body cddr)

(define application? pair?)
(define operator car)
(define operands cdr)
(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (if? exp) (tagged-list? exp 'if))
(define if-predicate cadr)
(define if-consequent caddr)
(define (if-alternative? exp)
  (not (null? (cdddr exp))))
(define if-alternative cadddr)

(define (values? exp) (tagged-list? exp 'values))
(define values-operands cdr)

(define (last-exp? seq) (null? (cdr seq)))
(define first-exp car)
(define rest-exps cdr)

; @@@@ Does't cope with "=>" syntax or empty clauses.
(define (cond? exp) (tagged-list? exp 'cond))
(define cond-clauses cdr)
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define cond-predicate car)
(define cond-actions cdr)

(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(define (letrec? exp) (tagged-list? exp 'letrec))
(define let-bindings cadr)
(define let-body cddr)
(define let-binding-variable car)
(define let-binding-value cadr)

(define (named-let? exp)
  (and (let? exp)
       (symbol? (cadr exp))))
(define named-let-name cadr)
(define named-let-bindings caddr)
(define named-let-body cdddr)

(define (cons? exp) (tagged-list? exp 'cons))
(define cons-car cadr)
(define cons-cdr caddr)

(define (make-vector? exp) (tagged-list? exp 'vector))
(define make-vector-operands cdr)

(define (set? exp) (tagged-list? exp 'set!))
(define set-variable cadr)
(define set-value caddr)

(define (case? exp) (tagged-list? exp 'case))
(define case-key cadr)
(define case-clauses cddr)
(define (case-else-clause? clause)
  (eq? (case-data clause) 'else))
(define case-data car)
(define case-actions cdr)

(define delay-exp cadr)

(define (do? exp) (tagged-list? exp 'do))
(define do-bindings cadr)
(define do-exit caddr)
(define do-commands cdddr)
(define do-binding-variable car)
(define do-binding-init cadr)
(define (do-binding-step binding)
  (case (length binding)
    ((2) (car binding))
    ((3) (caddr binding))
    (else (error "do: illegal binding"))))
(define do-exit-test car)
(define do-exit-exps cdr)
