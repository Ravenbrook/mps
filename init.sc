;;;; init.sc -- INITIAL ENVIRONMENT
;;;;
;;;; The definitions in this file build as complete an R5RS environment
;;;; as possible on top of the procedures and syntax built in to the
;;;; evaluator.
;;;;
;;;; See also "test.sc" which tests this code.
;;;;
;;;; Copyright (C) 1997-2001 Richard Brooksby.  This document is provided
;;;; "as is", without any express or implied warranty.  In no event will
;;;; the authors be held liable for any damages arising from the use of
;;;; this document.  You may not duplicate or reproduce this document in
;;;; any form without the express permission of the copyright holder.
;;;;
;;;;
;;;; DOCUMENT HISTORY
;;;;
;;;; 2004-07-27  RB  Added document history and Perforce Id keyword.
;;;;
;;;; 2004-08-05  RB  Modified use of "catch" now that it's no longer an
;;;; operator.
;;;;
;;;; 2004-08-11  RB  Add basic module import operator.
;;;;
;;;; $Id$


;;; ERRORS

(define (error message)
  (throw (make-exception message)))


;;; TYPES

(define procedure-type #x21BEB60C)
(define symbol-type #x21BE513B)
(define pair-type #x21BEBA16)
(define integer-type #x21BE142E)
(define special-type #x21BE5BEC)
(define input-port-type #x21BE14B0)
(define output-port-type #x21BE002B)
(define string-type #x21BE5261)
(define vector-type #x21BEFEC2)
(define exception-type #x21BEE8CE)
(define character-type #x21BEC8A6)
(define forward-type #x21BEF063)

(define (procedure? obj) (= (type obj) procedure-type))
(define (symbol? obj) (= (type obj) symbol-type))
(define (pair? obj) (= (type obj) pair-type))
(define (integer? obj) (= (type obj) integer-type))
(define (special? obj) (= (type obj) special-type))
(define (input-port? obj) (= (type obj) input-port-type))
(define (output-port? obj) (= (type obj) output-port-type))
(define (string? obj) (= (type obj) string-type))
(define (vector? obj) (= (type obj) vector-type))
(define (exception? obj) (= (type obj) exception-type))
(define (char? obj) (= (type obj) character-type))
(define (forward? obj) (= (type obj) forward-type))


;;; EQUIVALENCE PREDICATES
;;;
;;;  "A predicate is a procedure that always returns a boolean value (#t
;;;  or #f). An equivalence predicate is the computational analogue of
;;;  a mathematical equivalence relation (it is symmetric, reflexive,
;;;  and transitive). Of the equivalence predicates described in this
;;;  section, `eq?' is the finest or most discriminating, and `equal?'
;;;  is the coarsest. `Eqv?' is slightly less discriminating than
;;;  `eq?'." -- R5RS

;; equal? obj1 obj2
;;
;; "`Equal?' recursively compares the contents of pairs, vectors, and
;;  strings, applying `eqv?' on other objects such as numbers and
;;  symbols. A rule of thumb is that objects are generally `equal?' if
;;  they print the same. `Equal?' may fail to terminate if its arguments
;;  are circular data structures." -- R5RS
;;
;; @@@@ Could explose TYPEs as symbols to make this easier.

(define (equal? obj1 obj2)
  (cond
    ((pair? obj1)
      (and (pair? obj2)
           (equal? (car obj1) (car obj2))
           (equal? (cdr obj1) (cdr obj2))))
    ((string? obj1)
      (and (string? obj2)
           (string=? obj1 obj2)))
    ((vector? obj1)
      (and (vector? obj2)
           (vector=? obj1 obj2)))
    (else
      (eqv? obj1 obj2))))


;;; NUMBERS
;;;
;;; "Implementations of Scheme are not required to implement the whole
;;;  tower of subtypes given in section section Numerical types, but
;;;  they must implement a coherent subset consistent with both the
;;;  purposes of the implementation and the spirit of the Scheme
;;;  language. For example, an implementation in which all numbers are
;;;  real may still be quite useful." -- R5RS
;;;
;;; This Scheme only has integers.
;;;
;;; "Implementations may also support only a limited range of numbers
;;;  of any type, subject to the requirements of this section." -- R5RS
;;;
;;; They're just C's "long integer" type.

(define number? integer?)
(define complex? integer?)
(define real? integer?)
(define rational? integer?)

(define (zero? number) (= number 0))
(define (positive? number) (> number 0))
(define (negative? number) (< number 0))
(define (even? number) (= (* (/ number 2) 2) number))
(define (odd? number) (not (even? number)))
(define (max2 x y) (if (< x y) y x))
(define (min2 x y) (if (< x y) x y))
(define (max arg . args) (list-fold max2 arg args))
(define (min arg . args) (list-fold min2 arg args))
(define (quotient n m) (/ n m))
(define (remainder n m) (- n (* (quotient n m) m)))

;; @@@ This doesn't work for (modulo x -x) etc.
(define (modulo n m)
  (define (modulo2 n m)
    (if (positive? n)
      (remainder n m)
      (- m (remainder (- n) m))))
  (if (positive? m)
    (modulo2 n m)
    (- (modulo2 (- n) (- m)))))

;; @@@ This doesn't seem to work at all.  Probably too many ints.
;; Anyway, the sherry is getting to me now.
;(define (gcd2 a b)
;  (if (= b 0)
;      a
;      (gcd b (remainder a b))))
;
;(define (gcd . args) (list-fold gcd2 0 args))

(define (exact? number)
  (if (not (integer? number))
    (error "exact? applied to non-number"))
  #t)

(define (inexact? number)
  (if (not (integer? number))
    (error "exact? applied to non-number"))
  #f)

(define (monotonic predicate)
  (define (compare number . numbers)
    (or (null? numbers)
        (and (predicate number (car numbers))
             (apply1 compare numbers))))
  compare)

(define <= (monotonic (lambda (n1 n2) (not (> n1 n2)))))
(define >= (monotonic (lambda (n1 n2) (not (< n1 n2)))))

;; abs x 
;;
;; "`Abs' returns the absolute value of its argument." -- R5RS

(define (abs x) (if (< x 0) (- x) x))

;; number->string z radix 
;; 
;; "Radix must be an exact integer, either 2, 8, 10, or 16. If
;;  omitted, radix defaults to 10. The procedure `number->string'
;;  takes a number and a radix and returns as a string an external
;;  representation of the given number..." -- R5RS

(define (number->string number . args)
  (if (= number 0)
    "0"
    (begin
      (define base
        (case (length args)
          ((0) 10)
          ((1)
            (if (memv (car args) '(#b10 #o10 10 #x10))
              (car args)
              (error "number->string can only do base 2, 8, 10, or 16")))
          (else (error "number->string takes one or two arguments"))))
      (define digits
        (let loop ((n (abs number))
                   (digits '()))
          (if (= n 0)
            digits
            (loop (/ n base)
                  (cons (string-ref "0123456789ABCDEF"
                                    (remainder n base))
                        digits)))))
      (define signed
        (if (negative? number)
          (cons #\- digits)
          digits))
      (list->string signed))))


;;; VECTORS

;; vector obj ... 
;;
;; "Returns a newly allocated vector whose elements contain the given
;;  arguments. Analogous to `list'." -- R5RS

(define (vector . args) (list->vector args))

;; vector=? vec1 vec2
;;
;; A non-standard procedure used by "equal?".

(define (vector=? vec1 vec2)
  (if (not (and (vector? vec1) (vector? vec2)))
    (error "vector=?: both arguments must be a vectors"))
  (define length (vector-length vec1))
  (and (= length (vector-length vec2))
       (let loop ((n 0))
         (or (= n length)
             (and (equal? (vector-ref vec1 n) (vector-ref vec2 n))
                  (loop (+ n 1)))))))

;; vector-fill! vector fill 
;;
;; "Stores fill in every element of vector. The value returned by
;;  `vector-fill!' is unspecified." -- R5RS

(define (vector-fill! vector fill)
  (define length (vector-length vector))
  (let loop ((n 0))
    (if (< n length)
      (begin
        (vector-set! vector n fill)
        (loop (+ n 1))))))

;; list->vector list 
;;
;; "`Vector->list' returns a newly allocated list of the objects
;;  contained in the elements of vector. `List->vector' returns a
;;  newly created vector initialized to the elements of the list list." -- R5RS

(define (vector->list vector)
  (define length (vector-length vector))
  (let loop ((result '()) (n 0))
    (if (= n length)
      result
      (loop (cons (vector-ref vector (- length n 1)) result) (+ n 1)))))


;;; CONTROL FEATURES
    
;; apply proc arg1 ... args 
;;
;; "Proc must be a procedure and args must be a list. Calls proc with
;;  the elements of the list `(append (list arg1 ...) args)' as the
;;  actual arguments." -- R5RS

(define (apply proc . args)
  (apply1 proc (apply1 append args)))


;; map proc list1 list2 ... 
;;
;; "The lists must be lists, and proc must be a procedure taking as many
;;  arguments as there are lists and returning a single value. If more
;;  than one list is given, then they must all be the same length. `Map'
;;  applies proc element-wise to the elements of the lists and returns a
;;  list of the results, in order. The dynamic order in which proc is
;;  applied to the elements of the lists is unspecified." -- R5RS
;;
;; @@@@ Needs better error checking.  What if there are no lists, or
;; they're not the same length?

(define (map proc . lists)

  (let loop ((mapped '()) (lists lists))

    (define (next cars cdrs lists)
      (cond
        ((pair? lists)
          (next (cons (caar lists) cars)
                (cons (cdar lists) cdrs)
                (cdr lists)))
        ((null? lists)
          (loop (cons (apply1 proc (reverse cars)) mapped) (reverse cdrs)))
        (else
          (error "map applied to list of non-lists"))))

    (cond
      ((null? (car lists))
        (reverse mapped))
      ((pair? (car lists))
        (next '() '() lists))
      (else
        (error "map applied to non-list of lists")))))

(define (map1 proc tail)
  (let loop ((result '()) (last #f) (tail tail))
    (if (null? tail)
      result
      (let ((pair (list (proc (car tail)))))
        (if last
          (begin
            (set-cdr! last pair)
            (loop result pair (cdr tail)))
          (loop pair pair (cdr tail)))))))


;; for-each proc list1 list2 ... 
;;
;; "The arguments to `for-each' are like the arguments to `map', but
;;  `for-each' calls proc for its side effects rather than for its
;;  values. Unlike `map', `for-each' is guaranteed to call proc on the
;;  elements of the lists in order from the first element(s) to the
;;  last, and the value returned by `for-each' is unspecified." -- R5RS

(define (for-each proc . lists)
  (let loop ((lists lists))
    (cond
      ((null? (car lists)) (values))
      ((pair? (car lists))
        (let next ((cars '()) (cdrs '()) (lists lists))
          (cond
            ((pair? lists)
              (next (cons (caar lists) cars)
                    (cons (cdar lists) cdrs)
                    (cdr lists)))
            ((null? lists)
              (apply1 proc (reverse cars))
              (loop (reverse cdrs)))
            (else
              (error "for-each applied to list of non-lists")))))
      (else
        (error "for-each applied to non-list of lists")))))
  
(define (for-each1 proc list)
  (if (not (null? list))
    (begin
      (proc (car list))
      (for-each1 proc (cdr list)))))

;; call-with-current-continuation proc 
;;
;; "Proc must be a procedure of one argument. The procedure
;;  `call-with-current-continuation' packages up the current
;;  continuation (see the rationale below) as an "escape procedure"
;;  and passes it as an argument to proc. The escape procedure is a
;;  Scheme procedure that, if it is later called, will abandon
;;  whatever continuation is in effect at that later time and will
;;  instead use the continuation that was in effect when the escape
;;  procedure was created. Calling the escape procedure may cause the
;;  invocation of before and after thunks installed using
;;  dynamic-wind.
;;  
;; "The escape procedure accepts the same number of arguments as the
;;  continuation to the original call to
;;  call-with-current-continuation. Except for continuations created
;;  by the `call-with-values' procedure, all continuations take
;;  exactly one value. The effect of passing no value or more than one
;;  value to continuations that were not created by call-with-values
;;  is unspecified.
;;  
;; "The escape procedure that is passed to proc has unlimited extent
;;  just like any other procedure in Scheme. It may be stored in
;;  variables or data structures and may be called as many times as
;;  desired." -- R5RS

(define call-with-current-continuation call/cc)


;; delay <expression> 
;;
;; "The `delay' construct is used together with the procedure force to
;;  implement lazy evaluation or call by need. (delay <expression>)
;;  returns an object called a promise which at some point in the
;;  future may be asked (by the `force' procedure) to evaluate
;;  <expression>, and deliver the resulting value. The effect of
;;  <expression> returning multiple values is unspecified." -- R5RS
;;
;; "A promise may refer to its own value, as in the last example
;;  above. Forcing such a promise may cause the promise to be forced a
;;  second time before the value of the first force has been computed.
;;  This complicates the definition of [`delay']" -- R5RS

(define (make-promise proc)
  (define result-ready? #f)
  (define result #f)
  (lambda ()
    (if result-ready?
      result
      (begin
        (define x (proc))
        (if result-ready?
          result
          (begin
            (set! result-ready? #t)
            (set! result x)
            result))))))

(bind (cdr (the-environment)) 'delay
  (lambda (env operands)
    (if (not (= 1 (length operands)))
      (error "delay takes exactly one expression"))
    (make-promise (eval (list 'lambda '() (car operands)) env))))

;; force promise 
;;
;; "Forces the value of promise (see delay, section see section
;;  Delayed evaluation). If no value has been computed for the
;;  promise, then a value is computed and returned. The value of the
;;  promise is cached (or "memoized") so that if it is forced a second
;;  time, the previously computed value is returned." -- R5RS

(define (force promise) (promise))


;; quasiquote <qq template> 
;;
;; `<qq template> 
;;
;; "`Backquote' or `quasiquote' expressions are useful for
;;  constructing a list or vector structure when most but not all of
;;  the desired structure is known in advance. If no commas appear
;;  within the <qq template>, the result of evaluating `<qq template>
;;  is equivalent to the result of evaluating '<qq template>. If a
;;  comma appears within the <qq template>, however, the expression
;;  following the comma is evaluated (`unquoted') and its result is
;;  inserted into the structure instead of the comma and the
;;  expression. If a comma appears followed immediately by an at-sign
;;  (@), then the following expression must evaluate to a list; the
;;  opening and closing parentheses of the list are then `stripped
;;  away' and the elements of the list are inserted in place of the
;;  comma at-sign expression sequence. A comma at-sign should only
;;  appear within a list or vector <qq template>." -- R5RS
;;
;; "Quasiquote forms may be nested. Substitutions are made only for
;;  unquoted components appearing at the same nesting level as the
;;  outermost backquote. The nesting level increases by one inside
;;  each successive quasiquotation, and decreases by one inside each
;;  unquotation." -- R5RS

(define (quasiquote-expand env operands)
  (define (expand exp level)
    (cond
      ((pair? exp)
        (cond
          ;; ,<exp>
          ((eq? (car exp) 'unquote)
            (if (not (and (pair? (cdr exp)) (null? (cddr exp))))
              (error "quasiquote: malformed unquote"))
            (if (= level 0)
              (eval (cadr exp) env)
              (list 'unquote (expand (cadr exp) (- level 1)))))
          ;; `<template>
          ((eq? (car exp) 'quasiquote)
            (if (not (and (pair? (cdr exp)) (null? (cddr exp))))
              (error "quasiquote: malformed nested quasiquote"))
            (list 'quasiquote (expand (cadr exp) (+ level 1))))
          ;; ,@<exp>
          ((eq? (car exp) 'unquote-splicing)
            (error "quasiquote: found unquote-splicing outside list"))
          ;; (,@<exp> . <template>)
          ((and (pair? (car exp)) (eq? (caar exp) 'unquote-splicing))
            (if (not (and (pair? (cdar exp)) (null? (cddar exp))))
              (error "quasiquote: malformed unquote-splicing"))
            (if (= level 0)
              (let ((expansion (eval (cadar exp) env)))
                (if (not (list? expansion))
                  (error "quasiquote: unquote-splicing evaluated to non-list")) 
                (append expansion (expand (cdr exp) level)))
              (cons (list 'unquote-splicing (expand (cadar exp) (- level 1)))
                    (expand (cdr exp) level))))
          ;; (<template 1> . <template 2>)
          (else
            (cons (expand (car exp) level) (expand (cdr exp) level)))))
      ((vector? exp)
        (list->vector (expand (vector->list exp) level)))
      (else exp)))
  (if (not (and (pair? operands)
                (null? (cdr operands))))
    (error "quasiquote: illegal syntax"))
  (expand (car operands) 0))

(bind (cdr (the-environment)) 'quasiquote quasiquote-expand)


;;; (define-macro <symbol> <expander>)
;;; (define-macro (<symbol> <operands>) <expander-body>)
;;;
;;; This is a non-R5RS extension.

(define (define-macro? form)
  (and (pair? form)
       (list? form)
       (eq? (car form) 'define-macro)))

(define (define-macro-procedure? form)
  (and (define-macro? form)
       (pair? (cadr form))))

;;; Extract the symbol to which to bind the macro from the
;;; operands.

(define (define-macro-symbol form)
  (if (define-macro-procedure? form)
    (caadr form)
    (cadr form)))

;;; Extract the expression which will evaluate to the expander
;;; procedure from the operands.

(define (define-macro-value form)
  (if (define-macro-procedure? form)
    (cons 'lambda
          (cons (cdadr form)
          (cddr form)))
    (caddr form)))

(define (make-macro-expander env operands)
  ;; Check the syntax of the operands.
  (if (not (or (and (symbol? (car operands))
                    (null? (cddr operands)))
               (and (pair? (car operands))
                    (symbol? (caar operands))
                    (list? (cdr operands))
                    (pair? (cdr operands)))))
    (error "define-macro: illegal syntax"))
  (define form (cons 'define-macro operands))
  ;; Evaluate the expression to get the expander procedure.
  (define expander (eval (define-macro-value form) env))
  (if (not (procedure? expander))
    (error "define-macro: macros must be expansion procedures"))
  (define variable (define-macro-symbol form))
  ;; Set the procedure name of the expander to the name of the macro
  ;; so that it's useful in error messages and tracebacks.
  (set-procedure-name! expander variable)
  (cons variable expander))

(define (define-macro env operands)
  (define binding (make-macro-expander env operands))
  ;; Wrap the expander in an operator procedure.
  (bind (cdr env) (car binding)
    (lambda (env operands)
      (eval (apply1 (cdr binding) operands) env))))

(bind (cdr (the-environment)) 'define-macro define-macro)

;; (generate-symbol)
;;
;; This is a non-R5RS extension.

(define generate-symbol-counter 0)
(define (generate-symbol)
  (define symbol
    (string->symbol
      (string-append "generated symbol #"
                     (number->string generate-symbol-counter))))
  (set! generate-symbol-counter (+ generate-symbol-counter 1))
  symbol)


;; @@@@ Need "dynamic-wind".  Blech.


;; BOOLEANS

;; boolean?
;;
;; All instances of #t and #f are eq in this implementation.

(define (boolean? obj)
  (or (eq? obj #t) (eq? obj #f)))


;; not

(define (not obj)
  (if (eq? obj #f)
    #t
    #f))

;; function versions of "and" and "or"

(define (and . objects)
  (list-fold (lambda (x y) (and x y)) #t objects))

(define (or . objects)
  (list-fold (lambda (x y) (or x y)) #f objects))


;;; PAIRS AND LISTS

;; c...r -- compositions of car and cdr, up to 4 deep

(define (caar obj) (car (car obj)))
(define (cadr obj) (car (cdr obj)))
(define (cdar obj) (cdr (car obj)))
(define (cddr obj) (cdr (cdr obj)))
(define (caaar obj) (car (caar obj)))
(define (caadr obj) (car (cadr obj)))
(define (cadar obj) (car (cdar obj)))
(define (caddr obj) (car (cddr obj)))
(define (cdaar obj) (cdr (caar obj)))
(define (cdadr obj) (cdr (cadr obj)))
(define (cddar obj) (cdr (cdar obj)))
(define (cdddr obj) (cdr (cddr obj)))
(define (caaaar obj) (car (caaar obj)))
(define (caaadr obj) (car (caadr obj)))
(define (caadar obj) (car (cadar obj)))
(define (caaddr obj) (car (caddr obj)))
(define (cadaar obj) (car (cdaar obj)))
(define (cadadr obj) (car (cdadr obj)))
(define (caddar obj) (car (cddar obj)))
(define (cadddr obj) (car (cdddr obj)))
(define (cdaaar obj) (cdr (caaar obj)))
(define (cdaadr obj) (cdr (caadr obj)))
(define (cdadar obj) (cdr (cadar obj)))
(define (cdaddr obj) (cdr (caddr obj)))
(define (cddaar obj) (cdr (cdaar obj)))
(define (cddadr obj) (cdr (cdadr obj)))
(define (cdddar obj) (cdr (cddar obj)))
(define (cddddr obj) (cdr (cdddr obj)))


;; null? obj 
;;
;; "Returns #t if obj is the empty list, otherwise returns #f." -- R5RS

(define (null? obj)
  (eq? obj '()))


;; list? obj
;;
;; "Returns #t if obj is a list, otherwise returns #f. By definition,
;;  all lists have finite length and are terminated by the empty list." -- R5RS
;;
;; Infinite (i.e. circular) lists are detected by stepping a second
;; pointer along the list at twice the rate.  If the pointers meet,
;; the list is infinite.

(define (list? obj)
  (let loop ((tortoise obj) (hare obj))
    (if (pair? hare)
      (if (pair? (cdr hare))
        (if (eq? (cdr tortoise) (cddr hare))
          #f
          (loop (cdr tortoise) (cddr hare)))
        (null? (cdr hare)))
      (null? hare))))


;; list obj ... 
;;
;; "Returns a newly allocated list of its arguments." -- R5RS

(define (list . args) args)


;; append list ... 
;;
;; "Returns a list consisting of the elements of the first list followed
;;  by the elements of the other lists." -- R5RS

(define (append . lists)

  (define (outer-loop tail lists)
    (cond
      ;; If there are no lists left to append, do nothing.
      ((null? lists))

      ;; If there's only one list left to append, it can just be joined on
      ;; to the end of the new list.
      ((null? (cdr lists))
        (set-cdr! tail (car lists)))

      ;; If the first list to append is empty, it can be skipped.
      ((null? (car lists))
        (outer-loop tail (cdr lists)))

      (else

        (define (inner-loop tail list)
          (cond
            ((pair? list)
              (set-cdr! tail (cons (car list) '()))
              (inner-loop (cdr tail) (cdr list)))
            ((null? list) (outer-loop tail (cdr lists)))
            (else (error "append applied to non-list"))))

        (inner-loop tail (car lists)))))

  (define sentinel (cons 'dummy '()))
  (outer-loop sentinel lists)
  (cdr sentinel))


;; reverse list 
;;
;; Returns a newly allocated list consisting of the elements of list in
;; reverse order.

(define (reverse list)
  (let loop ((old list) (new '()))
    (cond
      ((pair? old) (loop (cdr old) (cons (car old) new)))
      ((null? old) new)
      (else (error "reverse applied to non-list")))))


;; list-tail list k
;;
;; Returns the sublist of list obtained by omitting the first k
;; elements. It is an error if list has fewer than k elements.

(define (list-tail list k)
  (if (zero? k)
    list
    (list-tail (cdr list) (- k 1))))


;; list-ref list k 
;;
;; Returns the kth element of list. (This is the same as the car of
;; (list-tail list k).) It is an error if list has fewer than k
;; elements.

(define (list-ref list k)
  (if (zero? k)
    (car list)
    (list-ref (cdr list) (- k 1))))


;; memq, memv, member obj list 
;;
;; These procedures return the first sublist of list whose car is obj,
;; where the sublists of list are the non-empty lists returned by
;; (list-tail list k) for k less than the length of list. If obj does
;; not occur in list, then #f (not the empty list) is returned. `Memq'
;; uses `eq?' to compare obj with the elements of list, while `memv'
;; uses `eqv?' and `member' uses `equal?'

(define (memq obj list)
  (cond
    ((null? list) #f)
    ((eq? (car list) obj) list)
    (else (memq obj (cdr list)))))

(define (memv obj list)
  (cond
    ((null? list) #f)
    ((eqv? (car list) obj) list)
    (else (memv obj (cdr list)))))

(define (member obj list)
  (cond
    ((null? list) #f)
    ((equal? (car list) obj) list)
    (else (member obj (cdr list)))))


;; assq, assv, assoc -- search an association list
;;
;; "Alist (for "association list") must be a list of pairs. These
;;  procedures find the first pair in alist whose car field is obj, and
;;  returns that pair. If no pair in alist has obj as its car, then #f
;;  (not the empty list) is returned. 'Assq' uses 'eq?' to compare obj
;;  with the car fields of the pairs in alist, while 'assv' uses 'eqv?'
;;  and 'assoc' uses 'equal?'." -- R5RS

(define (assq obj alist)
  (cond
    ((null? alist) #f)
    ((eq? obj (caar alist)) (car alist))
    (else (assq obj (cdr alist)))))

(define (assv obj alist)
  (cond
    ((null? alist) #f)
    ((eqv? obj (caar alist)) (car alist))
    (else (assv obj (cdr alist)))))

(define (assoc obj alist)
  (cond
    ((null? alist) #f)
    ((equal? obj (caar alist)) (car alist))
    (else (assoc obj (cdr alist)))))


;; Non-R5RS extensions

(define (list-fold procedure identity list)
  (cond
    ((null? list) identity)
    ((pair? list)
      (list-fold procedure (procedure identity (car list)) (cdr list)))
    (else
      (error "list-fold applied to non-list"))))

(define (list-filter predicate list)
  (define (loop result list)
    (cond
      ((null? list) (reverse result))
      ((pair? list)
        (loop (if (predicate (car list)) (cons (car list) result) result)
              (cdr list)))
      (else
        (error "list-filter applied to non-list"))))
  (loop '() list))


;;; CHARACTERS

;; char=? char1 char2 
;;
;; char<? char1 char2 
;;
;; char>? char1 char2 
;;
;; char<=? char1 char2 
;;
;; char>=? char1 char2 
;;
;; "These procedures impose a total ordering on the set of characters.
;;  It is guaranteed that under this ordering:
;;  
;;  
;; "- The upper case characters are in order. For example, `(char<?
;;  #\A #\B)' returns #t.
;;  
;; "- The lower case characters are in order. For example, `(char<?
;;  #\a #\b)' returns #t.
;;  
;; "- The digits are in order. For example, `(char<? #\0 #\9)' returns
;;  #t.
;;  
;; "- Either all the digits precede all the upper case letters, or
;;  vice versa.
;;  
;; "- Either all the digits precede all the lower case letters, or
;;  vice versa.
;;  
;; "Some implementations may generalize these procedures to take more
;;  than two arguments, as with the corresponding numerical
;;  predicates." -- R5RS

(define (char-compare integer-proc)
  (lambda (char1 char2)
    (integer-proc (char->integer char1) (char->integer char2))))

(define (char-ci-compare integer-proc)
  (lambda (char1 char2)
    (integer-proc (char->integer (char-downcase char1))
                  (char->integer (char-downcase char2)))))

;; @@@@ Could use "monotonic" proc here to make these take more args.

(define char=? (char-compare =))

(define char<? (char-compare <))

(define char>? (char-compare >))

(define char<=? (char-compare <=))

(define char>=? (char-compare >=))

(define char-ci=? (char-ci-compare =))

(define char-ci<? (char-ci-compare <))

(define char-ci>? (char-ci-compare >))

(define char-ci<=? (char-ci-compare <=))

(define char-ci>=? (char-ci-compare >=))



;;; STRINGS

;; string char ... 
;;
;; "Returns a newly allocated string composed of the arguments." -- R5RS

(define (string . chars) (list->string chars))


;; string=? string1 string2 
;; string-ci=? string1 string2 
;;  
;; "Returns #t if the two strings are the same length and contain the
;;  same characters in the same positions, otherwise returns #f.
;;  `String-ci=?' treats upper and lower case letters as though they
;;  were the same character, but `string=?' treats upper and lower
;;  case as distinct characters." -- R5RS

(define (string=? string1 string2)
  (if (not (and (string? string1) (string? string2)))
    (error "string=?: both arguments must be strings"))
  (define length (string-length string1))
  (and (= length (string-length string2))
       (let loop ((n 0))
         (or (= n length)
             (and (eqv? (string-ref string1 n) (string-ref string2 n))
                  (loop (+ n 1)))))))

;; substring string start end 
;;
;; "String must be a string, and start and end must be exact integers
;;  satisfying
;;
;;    0 <= start <= end <= (string-length string).
;;
;;  `Substring' returns a newly allocated string formed from the
;;  characters of string beginning with index start (inclusive) and
;;  ending with index end (exclusive)." -- R5RS

(define (substring string start end)
  (if (not (and (<= 0 start)
                (<= start end)
                (<= end (string-length string))))
    (error "substring arguments out of bounds"))
  (define len (- end start))
  (define str (make-string len))
  (let loop ((i 0))
    (if (< i len)
      (begin
        (string-set! str i (string-ref string (+ start i)))
        (loop (+ i 1)))))
  str)


;; string-append string ... 
;;
;; "Returns a newly allocated string whose characters form the
;;  concatenation of the given strings." -- R5RS

(define (string-append . strings)
  (define len (apply1 + (map string-length strings)))
  (define str (make-string len))
  (define (put index sub)
    (define len (string-length sub))
    (let loop ((i 0))
      (if (< i len)
        (begin
          (string-set! str (+ index i) (string-ref sub i))
          (loop (+ i 1)))
        (+ index i))))
  (list-fold put 0 strings)
  str)


;; string->list string
;; list->string list 
;;
;; "`String->list' returns a newly allocated list of the characters
;;  that make up the given string. `List->string' returns a newly
;;  allocated string formed from the characters in the list list,
;;  which must be a list of characters. `String->list' and
;;  `list->string' are inverses so far as `equal?' is concerned." -- R5RS

(define (string->list string)
  (let loop ((i (string-length string))
             (result '()))
    (if (> i 0)
      (loop (- i 1) (cons (string-ref string (- i 1)) result))
      result)))

(define (list->string list)
  (define len (length list))
  (define str (make-string len))
  (let loop ((i 0) (chars list))
    (if (null? chars)
      str
      (begin
        (string-set! str i (car chars))
        (loop (+ i 1) (cdr chars))))))

;; @@@@ Need string comparisons and string-copy to complete string operations.

(define (string<? string1 string2)
  (define length1 (string-length string1))
  (define length2 (string-length string2))
  (let loop ((i 0))
    (cond
      ((= i length1) (< i length2))
      ((= i length2) #f)
      ((char<? (string-ref string1 i) (string-ref string2 i)) #t)
      ((char=? (string-ref string1 i) (string-ref string2 i)) (loop (+ i 1)))
      (else #f))))


;;; INPUT AND OUTPUT

;; call-with-input-file string proc 
;;
;; call-with-output-file string proc 
;;
;; "String should be a string naming a file, and proc should be a
;;  procedure that accepts one argument. For `call-with-input-file',
;;  the file should already exist; for `call-with-output-file', the
;;  effect is unspecified if the file already exists. These procedures
;;  call proc with one argument: the port obtained by opening the
;;  named file for input or output. If the file cannot be opened, an
;;  error is signalled. If proc returns, then the port is closed
;;  automatically and the value(s) yielded by the proc is(are)
;;  returned. If proc does not return, then the port will not be
;;  closed automatically unless it is possible to prove that the port
;;  will never again be used for a read or write operation." -- R5RS

(define (call-with-input-file string proc)
  (define port (open-input-file string))
  (define result (proc port))
  (close-input-port port)
  result)

(define (call-with-output-file string proc)
  (define port (open-output-file string))
  (define result (proc port))
  (close-output-port port)
  result)

;; with-input-from-file string thunk 
;;
;; with-output-to-file string thunk 
;;
;; "String should be a string naming a file, and proc should be a
;;  procedure of no arguments. For `with-input-from-file', the file
;;  should already exist; for `with-output-to-file', the effect is
;;  unspecified if the file already exists. The file is opened for
;;  input or output, an input or output port connected to it is made
;;  the default value returned by `current-input-port' or
;;  `current-output-port' (and is used by (read), (write obj), and so
;;  forth), and the thunk is called with no arguments. When the thunk
;;  returns, the port is closed and the previous default is restored.
;;  `With-input-from-file' and `with-output-to-file' return(s) the
;;  value(s) yielded by thunk. If an escape procedure is used to
;;  escape from the continuation of these procedures, their behavior
;;  is implementation dependent." -- R5RS

(define (with-input-from-file string thunk)
  (define previous-input-port (current-input-port))
  (set-current-input-port! (open-input-file string))
  (call-with-values
    (lambda ()
      (catch
        (lambda (exception continuation)
          (close-input-port (current-input-port))
          (set-current-input-port! previous-input-port)
          (throw exception continuation))
        thunk))
    (lambda results
      (close-input-port (current-input-port))
      (set-current-input-port! previous-input-port)
      (apply1 values results))))

(define (with-output-to-file string thunk)
  (define previous-output-port (current-output-port))
  (set-current-output-port! (open-output-file string))
  (call-with-values
    (lambda ()
      (catch
        (lambda (exception continuation)
          (close-output-port (current-output-port))
          (set-current-output-port! previous-output-port)
          (throw exception continuation))
        thunk))
    (lambda results
      (close-output-port (current-output-port))
      (set-current-output-port! previous-output-port)
      (apply1 values results))))

;; @@@@ need read-char, peek-char, eof-object? char-ready? display write-char
;; newline

;; display obj port 
;;
;; "Writes a representation of obj to the given port. Strings that
;;  appear in the written representation are not enclosed in
;;  doublequotes, and no characters are escaped within those strings.
;;  Character objects appear in the representation as if written by
;;  `write-char' instead of by `write'. `Display' returns an
;;  unspecified value. The port argument may be omitted, in which case
;;  it defaults to the value returned by `current-output-port'." -- R5RS

(define (optional-output-port function-name args)
  (cond
    ((= (length args) 0) (current-output-port))
    ((= (length args) 1)
      (if (output-port? (car args))
        (car args)
        (error (string-append function-name 
                              ": second argument must be an output port"))))
    (else
      (error "display takes one or two arguments"))))

(define (display obj . args)
  (define port (optional-output-port "display" args))
  (cond
    ((string? obj) (write-string obj port))
    ((char? obj) (write-char obj port))
    (else (write obj port))))

(define newline-string (string #\newline))

(define (newline . args)
  (write-string newline-string
    (optional-output-port "newline" args)))


;;; ITERATION

;; (do ((<variable1> <init1> <step1>) ...)
;;     (<test> <expression> ...)
;;     <command> ...)
;;
;; "`Do' is an iteration construct. It specifies a set of variables to
;;  be bound, how they are to be initialized at the start, and how
;;  they are to be updated on each iteration. When a termination
;;  condition is met, the loop exits after evaluating the
;;  <expression>s.
;;  
;; "`Do' expressions are evaluated as follows: The <init> expressions
;;  are evaluated (in some unspecified order), the <variable>s are
;;  bound to fresh locations, the results of the <init> expressions
;;  are stored in the bindings of the <variable>s, and then the
;;  iteration phase begins.
;;  
;; "Each iteration begins by evaluating <test>; if the result is false
;;  (see section see section Booleans), then the <command> expressions
;;  are evaluated in order for effect, the <step> expressions are
;;  evaluated in some unspecified order, the <variable>s are bound to
;;  fresh locations, the results of the <step>s are stored in the
;;  bindings of the <variable>s, and the next iteration begins.
;;  
;; "If <test> evaluates to a true value, then the <expression>s are
;;  evaluated from left to right and the value(s) of the last
;;  <expression> is(are) returned. If no <expression>s are present,
;;  then the value of the `do' expression is unspecified.
;;  
;; "The region of the binding of a <variable> consists of the entire
;;  `do' expression except for the <init>s. It is an error for a
;;  <variable> to appear more than once in the list of `do' variables.
;;  
;; "A <step> may be omitted, in which case the effect is the same as
;;  if `(<variable> <init> <variable>)' had been written instead of
;;  `(<variable> <init>)'." -- R5RS 4.2.4
;;
;; @@@@ This will break if dos are nested, because the variable "loop"
;; is captured.

(define-macro (do bindings exit . commands)
  (define binding-variable car)
  (define binding-init cadr)
  (define (binding-step binding)
    (case (length binding)
      ((2) (car binding))
      ((3) (caddr binding))
      (else (error "do: illegal binding"))))
  (define vars (map1 binding-variable bindings))
  (define inits (map1 binding-init bindings))
  (define steps (map1 binding-step bindings))
  (define test (car exit))
  (define exit-exps (cdr exit))
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


;;; EVAL

;; scheme-report-environment version 
;;
;; null-environment version 
;;
;; "Version must be the exact integer `5', corresponding to this
;;  revision of the Scheme report (the Revised^5 Report on Scheme).
;;  `Scheme-report-environment' returns a specifier for an environment
;;  that is empty except for all bindings defined in this report that
;;  are either required or both optional and supported by the
;;  implementation. `Null-environment' returns a specifier for an
;;  environment that is empty except for the (syntactic) bindings for
;;  all syntactic keywords defined in this report that are either
;;  required or both optional and supported by the implementation.
;;  
;; "Other values of version can be used to specify environments
;;  matching past revisions of this report, but their support is not
;;  required. An implementation will signal an error if version is
;;  neither `5' nor another value supported by the implementation.
;;  
;; "The effect of assigning (through the use of `eval') a variable
;;  bound in a `scheme-report-environment' (for example `car') is
;;  unspecified. Thus the environments specified by
;;  `scheme-report-environment' may be immutable." -- R5RS

(define scheme-report-5-syntax-symbols
  '(and begin case cond define delay do if lambda let let* let-syntax
    letrec letrec-syntax load or quasiquote quote set! syntax-rules))

(define scheme-report-5-value-symbols
  '(* + - / < <= = > >= abs acos angle append apply asin assoc
    assq assv atan boolean? caar cadr
    call-with-current-continuation call-with-input-file
    call-with-output-file call-with-values caaaar caaadr caaar
    caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr
    caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar
    cddaar cddadr cddar cdddar cddddr cdddr cddr cdr ceiling
    char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=?
    char-ci>=? char-ci>? char-downcase char-lower-case?
    char-numeric? char-ready? char-upcase char-upper-case?
    char-whitespace? char<=? char<? char=? char>=? char>? char?
    close-input-port close-output-port complex? cons cos
    current-input-port current-output-port denominator display
    dynamic-wind eof-object? eq? equal? eqv? eval even?
    exact->inexact exact? exp expt floor for-each force gcd
    imag-part inexact->exact inexact? input-port? integer->char
    integer? interaction-environment lcm length list list->string
    list->vector list-ref list-tail list? log magnitude make-polar
    make-rectangular make-string make-vector make-vector map max
    member memq memv min modulo negative? newline not
    null-environment null? number->string number? numerator odd?
    open-input-file open-output-file output-port? pair? peek-char
    positive? procedure? quotient rational? rationalize read
    read-char real-part real? remainder reverse round
    scheme-report-environment set-car! set-cdr! sin sqrt string
    string->list string->number string->symbol string-append
    string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
    string-copy string-fill! string-length string-ref string-set!
    string<=? string<? string=? string>=? string>? string?
    substring symbol->string symbol? tan template transcript-off
    transcript-on truncate values vector vector->list vector-fill!
    vector-length vector-ref vector-ref vector-set! vector?
    with-input-from-file with-output-to-file write write-char
    zero?))

;; Note that scheme-report-environment and null-environment must cons
;; an empty frame onto the front of the environments so that new
;; definitions made within an "eval" don't extend the standard
;; environments.  Unfortunately, they can still rebind symbols
;; in the standard environments.

(define (scheme-report-environment version)
  (if (not (= version 5))
    (error "scheme-report-environment: only R5RS environment available"))
  (cons (cons '() (car (force scheme-report-environment-5)))
        (cons '() (cdr (force scheme-report-environment-5)))))
  
(define (null-environment version)
  (if (not (= version 5))
    (error "null-environment: only R5RS environment available"))
  (cons (list '())
        (cons '() (cdr (force scheme-report-environment-5)))))

(define (sc-init-environment)
  init-environment)

;; This declaration must come last, so that it captures the
;; previous definitions.

(define init-environment (the-environment))

(define scheme-report-environment-5
  (letrec ((copy-pair (lambda (pair) (cons (car pair) (cdr pair))))
           (copy-filtered-frame (lambda (symbols) (lambda (frame)
             (map1 copy-pair
               (list-filter
                 (lambda (binding) (memq (car binding) symbols))
                 frame)))))
           (copy-filtered-env (lambda (env symbols)
             (map1 (copy-filtered-frame symbols) env))))
    (delay
      (cons (copy-filtered-env (car init-environment)
                               scheme-report-5-value-symbols)
            (copy-filtered-env (cdr init-environment)
                               scheme-report-5-syntax-symbols)))))


;;; UNITS


;; load-source-unit

(define (load-source-unit symbol filename)
  (define port (open-input-file filename))
  (define forms
    (let loop ((form (read port)) (forms '()))
      (if (eof-object? form)
        (reverse forms)
        (loop (read port) (cons form forms)))))
  (define (unit env)
    (for-each (lambda (form) (eval form env)) forms))
  (set-procedure-name! unit symbol)
  unit)


;; load-compiled-unit
;;
;; (is built-in, see "dynload_*.c")


;; load-file-exists? -- check whether a file can be opened for input
;;
;; @@@@ This is pretty hacky.

(define (load-file-exists? filename)
  (catch
    (lambda (exception cont) #f)
    (lambda () (close-input-port (open-input-file filename)) #t)))


;; load-search -- search for a file on the load-path
;;
;; This is very simple-minded and Unix specific.

(define load-path '("." "/Users/rb" "/usr/local/lib/sc"))

(define (load-search filename)
  (if (char=? (string-ref filename 0) #\/)
    (and (load-file-exists? filename)
         filename)
    (begin
      (define (search path)
        (if (null? path)
          #f
          (begin
            (define absolute (string-append (car path) "/" filename))
            (if (load-file-exists? absolute)
              absolute
              (search (cdr path))))))
      (search load-path))))


;; import unit-name
;;
;; Finds a loads a unit into the current environment.  If the unit has
;; been loaded before then its linker function is applied to the current
;; environment instead.

(define (import env operands)
  (if (not (and (pair? operands)
                (null? (cdr operands))
                (symbol? (car operands))))
    (error "takes one symbol"))
  (define symbol (car operands))
  (define binding (assq symbol (car (the-unit-environment))))
  (if binding
    ((cdr binding) env)
    (begin
      (define linker
        (begin
          (define basename (symbol->string symbol))
          (define absolute (load-search (string-append basename ".so")))
          (if absolute
            (load-compiled-unit symbol absolute)
            (begin
              (define absolute (load-search (string-append basename ".sc")))
              (if absolute
                (load-source-unit symbol absolute)
                (error "couldn't find unit"))))))
      (bind (the-unit-environment) symbol linker)
      (linker env))))

(bind (cdr (the-environment)) 'import import)
