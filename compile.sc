;;;; compile.sc -- SC compiler
;;;;
;;;; INTRODUCTION
;;;;
;;;; This is a compiler for SC, a language almost conforming to "Revised(5)
;;;; Report on the Algorithmic Language Scheme" [R5RS].  It compiles to
;;;; opcodes which can be translated by the C preprocessor, compiled with a
;;;; C compiler, and linked with the SC interpreter and run-time system.
;;;;
;;;; @@@@ Not enough syntax checking.
;;;; @@@@ What if a variable is defined on just one branch of an "if"?
;;;; @@@@ It would probably be more efficient to create constants by invoking
;;;; "read" on a string.
;;;; @@@@ Need to add all the basic syntax, and allow for macros.
;;;; @@@@ Can there ever be a target if the context is tail?
;;;; @@@@ Need to detect variables defined in only one branch of an "if", "and", etc.
;;;;
;;;; NOTES
;;;;
;;;; - It's OK to re-use registers because they're duplicated by the abstract machine
;;;;   if a call/cc happens.  Local variables, however, are not duplicated, and so
;;;;   each one must have its own slot, even if it's out of scope.
;;;;
;;;; - Register allocation is stack-like, so the number of registers used
;;;;   is proportional to the nesting complexity of the code compiled.  A
;;;;   stack-like discipline is possible because variables, which don't
;;;;   have nested live ranges, aren't kept in registers.  If the compiler
;;;;   were enhanced to cache variable values in registers then a proper
;;;;   register allocator would be needed.
;;;;
;;;; - In many Scheme compilers, the enter environment is stored when a
;;;;   procedure is closed over.  This compiler compiles code which extracts
;;;;   the free variable bindings used by the procedure and creates a vector.
;;;;   This makes free variable access fast, and allows the rest of the
;;;;   environment to be garbage collected away, at the cost of building the
;;;;   vector when the function is defined (i.e. when "lambda" is "executed").
;;;;
;;;; - Free variables which bubble up to the top-level of a compilation unit
;;;;   are looked up in the dynamic environment when the unit is executed.
;;;;   (This can be thought of as "linking" the code.)  A special binding
;;;;   is created for any which aren't defined, so that when the variable
;;;;   is looked up, an unbound variable error is generated.  This allows
;;;;   forward references to be compiled.
;;;;
;;;; - Similarly, bindings for local variables are initially created in a
;;;;   similar way.
;;;;
;;;; - Similarly, constants (including quoted objects), are loaded from a
;;;;   vector created by the procedure's definer, and bubble up to the
;;;;   top-level, where they are created all at once when the compilation
;;;;   unit is executed.
;;;;
;;;; - I wrote most of this compiler before I read [SICP].  The only ideas I
;;;;   lifted from [SICP] are to pass a "target" register (which helps a lot
;;;;   with stack-like register allocation) and the "context" ("linkage" in
;;;;   [SICP]) which deals with tail calls and optimises branches nicely.
;;;;
;;;; LICENCE
;;;;
;;;; Copyright (C) 1997-2004 Richard Brooksby.  This document is provided
;;;; "as is", without any express or implied warranty.  In no event will
;;;; the authors be held liable for any damages arising from the use of
;;;; this document.  You may not duplicate or reproduce this document in
;;;; any form without the express permission of the copyright holder.
;;;;
;;;; REFERENCES
;;;;
;;;; [R5RS]     "Revised(5) Report on the Algorithmic Language Scheme";
;;;;            Richard Kelsey, William Clinger, Jonathan Rees (Eds).
;;;;
;;;; [SICP]     "Structure and Interpretation of Computer Programs" (second
;;;;            edition); Harold Abelson and Gerald Jay Sussman with Julie
;;;;            Sussman; The MIT Press; 1996.
;;;;
;;;; DOCUMENT HISTORY
;;;;
;;;; 2004-08-04  RB  Started document history.  Added "environment-register"
;;;; to result so that "the-environment" can be used if there is one around,
;;;; which there is in top-level forms.  Added basic warning output.
;;;;
;;;; 2004-08-05  RB  Separating test for "if" alternative from access, so
;;;; that "(if x y #f)" works.  Fixing block numbering in lambdas within
;;;; letrecs.  Rewriting "(if x y)" to "(if x y (values))" when a result
;;;; is expected.  Fixed top-level references to "the-environment" to get
;;;; the whole environment, not just the value environment.  Fixing
;;;; compile-defintion to allow locally defined functions to be recursive.
;;;;
;;;; 2004-08-09  RB  Building data pairs cdr-first and re-using the cdr
;;;; register so that lists are built using two registers instead of a
;;;; number equal to their length.  This reduces the number of registers
;;;; needed by the top-level procedure to O(depth of data) rather than
;;;; O(length of longest list in data).

(load "define-structure.sc")
(load "syntax.sc")
(load "expand.sc")

;;; OPTIONS
;;;
;;; *compile-flat-constants* if true, constants vectors are created
;;; piece-by-piece at procedure definition, which means that any
;;; elements are shared with those of other constants vectors in the
;;; compilation unit, but procedure defintion is slower.  If false,
;;; the constants vector is itself treated as a constant, and not
;;; shared.

(define *compile-flat-constants* #t)

;;; COMPILATION RESULT STRUCTURE

(define-structure result
  next-register         ; integer, next available virtual register
  max-register          ; integer, number of registers needed
  next-block            ; integer, next free block number
  environment           ; compilation environment, list of frames of bindings
  syntax-environment    ; syntax environment, list of frames of bindings
  free-variables        ; list of free variables in run-time environment
  local-variables       ; list of local variables in run-time environment
  constants             ; list of constants in run-time environment
  block                 ; final block label
  instructions          ; final instruction list in reverse (because appending is very common)
  blocks                ; list of (<block number> . <instructions>)
  environment-register  ; #f or register that contains top-level environment
)

;;; warning obj

(define (warning obj)
  (define port (current-error-port))
  (display "WARNING: " port)
  (display obj port)
  (newline port))

;;; for-each-indexed proc list
;;;
;;; Applies proc to each member of the list in turn, and to the
;;; position of the member within the list.  This is useful for
;;; handling compile-time lists which represent run-time vectors.

(define (for-each-indexed proc list)
  (let loop ((list list) (index 0))
    (if (not (null? list))
      (begin
        (proc (car list) index)
        (loop (cdr list) (+ index 1))))))

;;; new-register result
;;;
;;; Allocates a new virtual register from the result, and returns the
;;; register number.  It also records the maximum number of registers
;;; allocated.

(define (new-register result)
  (define register (result-next-register result))
  (set-result-next-register! result (+ register 1))
  (if (>= register (result-max-register result))
    (set-result-max-register! result (+ register 1)))
  register)

;;; add-instruction result instruction
;;;
;;; Appends an instruction to the end of a compilation result.
;;; Note that because the final instruction list is in reverse
;;; we can just cons the instruction onto the front.

(define (add-instruction result opcode . arguments)
  (set-result-instructions!
    result
    (cons (cons opcode arguments)
          (result-instructions result))))

;;; prepend-instruction result instruction
;;;
;;; Like add-instruction, but prepends the instruction to the
;;; front of the final instruction list.  (This actually means
;;; appending it, because the list is in reverse.)

(define (prepend-instruction result opcode . arguments)
  (set-result-instructions!
    result
    (append (result-instructions result)
            (list (cons opcode arguments)))))

;;; always-append obj list
;;;
;;; Appends obj to list, and returns two values: the position of
;;; obj in the list, and the new list with obj in it.

(define (always-append obj list)
  (let loop ((index 0)
             (prev #f)
             (rest list))
    (cond
      ((null? rest)
        (let ((new (cons obj '())))
          (if prev
            (begin
              (set-cdr! prev new)
              (values index list))
            (values index new))))
      ((pair? rest)
        (loop (+ index 1) rest (cdr rest)))
      (else
        (error "always-append applied to non-list")))))

;;; find-or-append obj list
;;;
;;; Like always-append, but if the object is already in the list
;;; (according to "equal?") then it returns the index of the existing
;;; object and doesn't append.

(define (find-or-append obj list)
  (let loop ((index 0)
             (prev #f)
             (rest list))
    (cond
      ((null? rest)
        (let ((new (cons obj '())))
          (if prev
            (begin
              (set-cdr! prev new)
              (values index list))
            (values index new))))
      ((equal? obj (car rest))
        (values index list))
      ((pair? rest)
        (loop (+ index 1) rest (cdr rest)))
      (else
        (error "find-or-append applied to non-list")))))

(define (add-element result searcher getter setter! object)
  (call-with-values
    (lambda () (searcher object (getter result)))
    (lambda (index constants)
      (setter! result constants)
      index)))

;;; find-constant result constant
;;;
;;; Finds the constant in the constants vector of the compilation
;;; result, and returns its index.  Adds the constant to the vector
;;; if necessary.

(define (find-constant result constant)
  (add-element result
               find-or-append
               result-constants
               set-result-constants!
               constant))

;;; find-free-variable result variable
;;;
;;; Finds the free variable in the free variables vector of the
;;; compilation result, and returns its index.  Adds the variable
;;; to the vector if necessary.

(define (find-free-variable result variable)
  (add-element result
               find-or-append
               result-free-variables
               set-result-free-variables!
               variable))

;;; add-local-variable result variable
;;;
;;; Appends the local variable to the local variables vector of
;;; the compilation result, and returns its index.

(define (add-local-variable result variable)
  (add-element result
               always-append
               result-local-variables
               set-result-local-variables!
               variable))

;;; frame-lookup frame variable
;;;
;;; Looks up a variable in a frame, which is a list of binding
;;; pairs associating variables with registers.  Returns the
;;; binding pair whose symbol matches the variable, or #f if
;;; there is none.

(define (frame-lookup frame variable)
  (assq variable frame))

;;; environment-lookup environment variable
;;;
;;; Looks up a variable in an environment, which is a list of
;;; frames (see frame-lookup).

(define (environment-lookup environment variable)
  (and (pair? environment)
       (or (frame-lookup (car environment) variable)
           (environment-lookup (cdr environment) variable))))

;;; result-lookup result variable
;;;
;;; Looks up a variable in the environment of a compilation
;;; result (see environment-lookup).

(define (result-lookup result variable)
  (environment-lookup (result-environment result) variable))

;;; environment-define environment variable register
;;;
;;; Defines a variable in the environment.  If the variable
;;; is already bound in the first frame then the variable's
;;; index is returned, otherwise a new binding is added in
;;; the first frame binding the variable to a fresh location
;;; in the locals vector, and its index is returned.

(define (environment-define result environment variable)
  (define frame (car environment))
  (define binding (frame-lookup frame variable))
  (if binding
    (cdr binding)
    (let ((index (add-local-variable result variable)))
      (set-car! environment
                (cons (cons variable index) frame))
      index)))

;;; new-block result
;;;
;;; Allocates and returns a new block number from the result.

(define (new-block result)
  (define block (result-next-block result))
  (set-result-next-block! result (+ block 1))
  block)

;;; label-block result label
;;;
;;; Promotes the final instruction list to a block of its own
;;; (effectively finishing it off) and starts a new list whose
;;; label will be "label" when it is promoted later.
;;; Essentially, inserts a label into the instruction stream.
;;; The final instruction list should end with a RET or JUMP
;;; or some other non-conditional control transfer, or the flow
;;; of control will drop off the end of the new block.

(define (label-block result label)
  (define instructions (result-instructions result))
  (if (null? instructions)
    (error "attempt to label empty instruction sequence"))
  (if (not (memq (caar instructions) '(ret jump call tail)))
    (error "attempt to label unterminated instruction sequence"))
  (set-result-blocks! result
    (cons (cons (result-block result)
                (reverse (result-instructions result)))
          (result-blocks result)))
  (set-result-block! result label)
  (set-result-instructions! result '()))

;;; send-to-variable result variable register environment
;;;
;;; Ensures that the register's value is stored in a variable in
;;; the environment, binding the variable if necessary.

(define (send-to-variable result variable register environment)
  (define variable-index (environment-define result environment variable))
  (add-instruction result 'set-local variable-index register))

;;; compile-values result
;;;
;;; Compiles instructions to build a list out of the latest values
;;; and store them in the ARGS register.  This can be used in
;;; preparation for a CALL or a RET.

(define (compile-values result values)
  (add-instruction result 'comment "values" values)
  (define mark (result-next-register result))
  (define args-register (new-register result))
  (add-instruction result 'load-empty args-register)
  (let loop ((values values))
    (if (not (null? values))
      (begin
        (add-instruction result
                         'make-pair
                         args-register
                         (car values)
                         args-register)
        (loop (cdr values)))))
  (add-instruction result 'store-args args-register (length values))
  (set-result-next-register! result mark))

;;; compile-context result context
;;;
;;; Compiles instructions to satisfy the context.  If the context is
;;; "tail", then compile the value into the argument list and return.
;;; If the context is an integer, compile a jump to the label.
;;; If the context is "body", do nothing, as the next instructions
;;; will be appended.

(define (compile-context result value context)
  (cond
    ((eq? context 'tail)
      (compile-values result (if value (list value) '()))
      (add-instruction result 'leave)
      (add-instruction result 'ret))
    ((eq? context 'body)
      (values))
    ((integer? context)
      (add-instruction result 'jump context))
    (else
      (error "malformed context"))))

;;; compile-constant result variable target context
;;;
;;; Compiles any object as a constant.  The constant is added to the
;;; constants vector, to be created by the defining function.

(define (compile-constant result object target context)
  (if target
    (add-instruction result 'load-constant target (find-constant result object))
    (add-instruction result 'comment "(constant omitted because result not used)" target))
  (compile-context result target context))

;;; compile-self-evaluating result object target context
;;;
;;; Compiles a self-evaluating object such as an integer, string, or boolean.

(define (compile-self-evaluating result object target context)
  (add-instruction result 'comment "self-evaluating" object)
  (compile-constant result object target context))

;;; compile-quotation result quotation target context
;;;
;;; Compiles a form like 'x.

(define (compile-quotation result quotation target context)
  (add-instruction result 'comment "quotation" (text-of-quotation quotation))
  (compile-constant result (text-of-quotation quotation) target context))

;;; compile-variable-reference result variable target context
;;;
;;; Compiles a variable reference.  The variable is either bound
;;; (defined in the current procedure) or free (defined outside).
;;; If it's bound then load its value (the cdr of its binding)
;;; into a register.
;;; If it's free then load it from the closure environment.
;;; Of course it may be undefined, but this is handled by the
;;; LOAD-LOCAL and LOAD-FREE instructions at run-time.

(define (compile-variable-reference result variable target context)
  (define binding (result-lookup result variable))
  (if binding
    (begin
      (add-instruction result 'comment "local variable" variable)
      (if target
        (add-instruction result 'load-local target (cdr binding))
        (add-instruction result 'comment "(local variable omitted because result not used)" target)))
    (begin
      (add-instruction result 'comment "free variable" variable)
      (if target
        (add-instruction result 'load-free target (find-free-variable result variable))
        (add-instruction result 'comment "(free variable omitted because result not used)" target))))
  (compile-context result target context))

;;; compile-sequence result forms target context
;;;
;;; Compile a sequence (list) of forms so that they execute one after
;;; the other.  The values of the sequence are the values of the last
;;; form.  All but the last form are compiled with context "body" so
;;; that they continue on to the next, and with target "#f" so that
;;; their results are discarded.  But the last has the same
;;; context and target as the sequence itself, because it is a
;;; "tail expression" as defined by R5RS, and the result of a sequence
;;; is the result of the last expression.

(define (compile-sequence result forms target context)
  (if (last-exp? forms)
    (compile-form result (first-exp forms) target context)
    (begin
      (compile-form result (first-exp forms) #f 'body)
      (compile-sequence result (rest-exps forms) target context))))

;;; compile-definition result definition target context
;;;
;;; Compiles a form like (define x y) and (define (f x) y).
;;;
;;; @@@@ Unlike R5RS it is _not_ an error to refer to previous definitions,
;;; and definitions are evaluated in order.  (R5RS makes them equivalent
;;; to letrec.)
;;;
;;; @@@@ But what about (define f (lambda () f))?  It should be recursive
;;; but won't be compiled as such by this code.

(define (compile-definition result definition target context)
  (if target
    (error "definition used where a result was expected"))
  (add-instruction result 'comment "definition" definition)
  (define mark (result-next-register result))
  (define variable (definition-variable definition))
  (define value (definition-value definition))
  (define value-register (new-register result))
  ;; Function definitions can be recursive, so we must predefine the variable
  ;; so that it's bound correctly if found free in the body.  c.f. letrec
  (if (lambda? value)
    (environment-define result (result-environment result) variable))
  (compile-form result value value-register 'body)
  (if (lambda? value)                   ; @@@@ What if lambda has been rebound?
    (let ((name-register (new-register result)))
      (add-instruction result 'comment "setting procedure name" variable definition)
      (compile-constant result variable name-register 'body)
      (add-instruction result 'set-proc-name value-register name-register)))
  (send-to-variable result variable value-register (result-environment result))
  (set-result-next-register! result mark)
  (compile-context result target context))

;;; compile-operands result operands
;;;
;;; Compiles instructions to build an argument list out of a list
;;; of expressions.  This can be used in preparation for a CALL or RET.

(define (compile-operands result operands)
  (add-instruction result 'comment "operands" operands)
  (define mark (result-next-register result))
  (define args-register (new-register result))
  (add-instruction result 'load-empty args-register)
  (define value-register (new-register result))
  (for-each
    (lambda (operand)
      (add-instruction result 'comment "operand" operand)
      (compile-form result operand value-register 'body)
      (add-instruction result 'make-pair args-register value-register args-register))
    (reverse operands))
  (add-instruction result 'store-args args-register (length operands))
  (set-result-next-register! result mark))

;;; compile-application result application target context
;;;
;;; Compiles an application of a procedure.  In tail context, the
;;; application compiles to a tail call, otherwise it compiles to
;;; an ordinary call.

(define (compile-application result application target context)
  (add-instruction result 'comment "application" application)
  (define mark (result-next-register result))
  (compile-operands result (operands application))
  (define operator-register (new-register result))
  (compile-form result (operator application) operator-register 'body)
  (if (eq? context 'tail)
    (begin
      (add-instruction result 'leave)
      (add-instruction result 'tail operator-register)
      (set-result-next-register! result mark))
    (begin
      ;; Note that this only copes with single value returns.
      ;; We leave multi-value returns to "call-with-values".
      (define block (new-block result))
      (add-instruction result 'call operator-register block)
      (label-block result block)
      ;; Note that multiple values are an error on a non-tail context
      ;; and this is handled at run-time by LOAD-RESULT.
      (if target
        (add-instruction result 'load-result target)
        (add-instruction result 'comment "(result not used)" target))
      (set-result-next-register! result mark)
      (compile-context result target context))))

;;; compile-parameters result parameters
;;;
;;; Compiles instructions to unpack an argument list and define
;;; variables for each parameter.

(define (compile-parameters result parameters)
  (add-instruction result 'comment "parameters" parameters)
  (define mark (result-next-register result))
  (define args-register (new-register result))
  (add-instruction result 'load-args args-register)
  (define first-arg-register (new-register result))
  (define environment (result-environment result))
  (let loop ((parameters parameters) (variables-seen '()))
    (cond
      ((null? parameters)
        (add-instruction result 'last-arg args-register))
      ((and (pair? parameters) (symbol? (car parameters)))
        (let ((parameter (car parameters)))
          (if (memq parameter variables-seen)
            (error "parameter variable appears more than once"))
          (add-instruction result 'comment "parameter" parameter)
          (add-instruction result 'load-arg first-arg-register args-register)
          (send-to-variable result parameter first-arg-register environment)
          (loop (cdr parameters) (cons parameter variables-seen))))
      ((symbol? parameters)
        (if (memq parameters variables-seen)
          (error "parameter variable appears more than once"))
        (send-to-variable result parameters args-register environment))
      (else
        (error "malformed lambda parameters"))))
  (set-result-next-register! result mark))

;;; compile-preamble result start-block
;;;
;;; Prepends a procedure preamble to a compilation result in which the procedure
;;; body has already been compiled, and returns the entry point block.
;;; The preamble includes the ENTER instruction, and code to set up the bindings
;;; for local variables.

(define (compile-preamble result start-block)
  (define mark (result-next-register result))
  ;; Prepend an entry block to do function entry set-up
  (define entry-point (new-block result))
  (label-block result entry-point)
  ;; Compile code to create fresh bindings for all the local variables in the function.
  (if (not (null? (result-local-variables result)))
    (let ((symbol-register (new-register result))
          (binding-register (new-register result))
          (unbound-register (new-register result)))
      (add-instruction result 'comment "make local bindings" (result-local-variables result))
      (add-instruction result 'load-unbound unbound-register)
      (for-each-indexed
        (lambda (variable index)
          (compile-constant result variable symbol-register 'body)
          (add-instruction result 'make-pair binding-register symbol-register unbound-register)
          (add-instruction result 'set-local-binding index binding-register))
        (result-local-variables result))))
  ;; Prepend an ENTER instruction to allocate registers and locals
  (prepend-instruction result
                       'enter
                       (result-max-register result)
                       (length (result-local-variables result)))
  ;; Add an instruction to jump to the start of the procedure code
  (add-instruction result 'jump start-block)
  ;; Close off the procedure.
  (label-block result (new-block result))
  (set-result-next-register! result mark)
  ;; Return the entry point
  entry-point)

;;; compile-make-procedure outer-result inner-result entry-point target context
;;;
;;; Compiles code to make a procedure object which will call the procedure
;;; compile in the inner-result, starting at the entry-point.  The code
;;; to make the procedure includes code to create the procedure's constants
;;; and free variables, i.e. build the closure for the procedure.

(define (compile-make-procedure outer-result inner-result entry-point target context)
  (if (not target)
    (error "make-procedure called without target"))
  (define mark (result-next-register outer-result))
  ;; Compile code to create the constants vector
  (add-instruction outer-result 'comment "procedure constants" (result-constants inner-result))
  (define constants-vector-register (new-register outer-result))
  (if *compile-flat-constants*
    (begin
      ;; Merged (flattened) constants
      (define constants (result-constants inner-result))
      (add-instruction outer-result 'make-vector constants-vector-register (length constants))
      (if (not (null? constants))
        (let ((constant-register (new-register outer-result)))
          (for-each-indexed
            (lambda (constant index)
              (compile-constant outer-result constant constant-register 'body)
              (add-instruction outer-result
                               'vector-set
                               constants-vector-register
                               index
                               constant-register))
            constants))))
    (compile-constant outer-result
                      (list->vector (result-constants inner-result))
                      constants-vector-register
                      'body))
  ;; Copy the binding pairs for the free variables of the procedure into its free-variables vector.
  (define free-variables (result-free-variables inner-result))
  (add-instruction outer-result 'comment "procedure free variables" free-variables)
  (define free-vector-register (new-register outer-result))
  (add-instruction outer-result 'make-vector free-vector-register (length free-variables))
  (if (not (null? free-variables))
    (let ((binding-register (new-register outer-result)))
      (for-each-indexed
        (lambda (variable index)
          (define binding (result-lookup outer-result variable))
          (add-instruction outer-result 'comment "procedure free variable" variable)
          (if binding
            (add-instruction outer-result
                             'load-local-binding
                             binding-register
                             (cdr binding))
            (add-instruction outer-result
                             'load-free-binding
                             binding-register
                             (find-free-variable outer-result variable)))
          (add-instruction outer-result 'vector-set free-vector-register index binding-register))
        free-variables)))
  ;; Finally, make the procedure object
  (add-instruction outer-result
                   'make-proc
                   target
                   constants-vector-register
                   free-vector-register
                   entry-point)
  (set-result-next-register! outer-result mark)
  (compile-context outer-result target context))

;;; compile-lambda result lambda target context
;;;
;;; Compiles a form like (lambda <parameters> <body>).
;;; The body is compiled in a separate compilation result, with its own environments.
;;; Any free variables and constants are supplied here.  The compiled code blocks
;;; from the inner result are merged with those of the outer.

(define (compile-lambda outer-result lambda target context)
  (add-instruction outer-result 'comment "lambda" lambda)
  (if target
    (begin
      ;; Compile the body of the procedure
      (define start-block (result-next-block outer-result))
      (define inner-result (new-result start-block
                                       (cons '() (result-syntax-environment outer-result))))
      (compile-parameters inner-result (lambda-parameters lambda))
      (define body-register (new-register inner-result))
      (compile-sequence inner-result (lambda-body lambda) body-register 'tail)
      (define entry-point (compile-preamble inner-result start-block))
      ;; Merge the procedure blocks with the outer result
      (set-result-blocks! outer-result
                          (append (result-blocks outer-result)
                                  (result-blocks inner-result)))
      (set-result-next-block! outer-result (result-next-block inner-result))
      ;; Compile code in the outer-result to make the procedure object
      (compile-make-procedure outer-result inner-result entry-point target context))
    (begin
      (add-instruction outer-result 'comment "(procedure omitted because result not used)" target)
      (compile-context outer-result target context))))

;;; compile-if result if target context

(define (compile-if result if target context)
  (add-instruction result 'comment "if" if)
  (define mark (result-next-register result))
  (define predicate-register (new-register result))
  (compile-form result (if-predicate if) predicate-register 'body)
  (if (and target (not (if-alternative? if)))
    (begin
      (warning "if without alternative used where result is expected")
      ;; When a result is expected the interpreter returns the empty set of
      ;; values.  Do the same.  The load-result opcode will cause an error
      ;; if the result is used.
      ;; @@@@ This seems to happen a lot, and it's not very efficient.
      (set! if (append if '(values)))))
  (if (if-alternative? if)
    (begin
      (define alternative-block (new-block result))
      (add-instruction result
                       'branch-false
                       alternative-block
                       predicate-register)
      (set-result-next-register! result mark)
      (if (eq? context 'body)
        (begin
          (define next-block (new-block result))
          (compile-form result (if-consequent if) target 'body)
          (add-instruction result 'jump next-block)
          (label-block result alternative-block)
          (compile-form result (if-alternative if) target 'body)
          (add-instruction result 'jump next-block)
          (label-block result next-block))
        (begin
          (compile-form result (if-consequent if) target context)
          (label-block result alternative-block)
          (compile-form result (if-alternative if) target context))))
    (begin
      (if target                        ; shouldn't happen -- see above
        (error "if without alternative used where result is expected"))
      (if (integer? context)
        (begin
          (add-instruction result 'branch-false context predicate-register)
          (set-result-next-register! result mark)
          (compile-form result (if-consequent if) target context))
        (begin
          (define next-block (new-block result))
          (add-instruction result 'branch-false next-block predicate-register)
          (set-result-next-register! result mark)
          (compile-form result
                        (if-consequent if)
                        target
                        (if (eq? context 'tail) context next-block))
          (label-block result next-block)
          (compile-context result target context))))))

;;; compile-let-bindings result environment bindings

(define (compile-let-bindings result environment bindings)
  (define value-register (new-register result))
  (for-each
    (lambda (binding)
      (define value (let-binding-value binding))
      (define variable (let-binding-variable binding))
      (compile-form result value value-register 'body)
      (if (lambda? value)                       ; @@@@ What if lambda has been rebound?
        (let ((name-register (new-register result)))
          (add-instruction result 'comment "setting procedure name" variable)
          (compile-constant result variable name-register 'body)
          (add-instruction result 'set-proc-name value-register name-register)))
      (send-to-variable result (let-binding-variable binding) value-register environment))
    bindings))

;;; compile-let result let target context
;;;
;;; If the let is named, expand it into more basic syntax and compile that,
;;; otherwise call compile-simple-let.

(define (compile-let result let target context)
  (cond
    ((symbol? (cadr let)) (compile-form result (expand-named-let let) target context))
    ((list? (cadr let))   (compile-simple-let result let target context))
    (else (error "illegal syntax"))))

;;; compile-simple-let result let target context
;;;
;;; In a plain let the binding values are evaluated in the outer environment,
;;; and don't interfere with each other.  This is achieved by compiling them
;;; before the result environment is extended.  Compare with compile-let*
;;; and compile-letrec.
;;;
;;; @@@@ Probably should extend the syntax environment too.

(define (compile-simple-let result let target context)
  (add-instruction result 'comment "let" let)
  (define outer-environment (result-environment result))
  (define inner-environment (cons '() outer-environment))
  (compile-let-bindings result inner-environment (let-bindings let))
  (set-result-environment! result inner-environment)
  (compile-sequence result (let-body let) target context)
  (set-result-environment! result outer-environment))

;;; compile-let* result let context
;;;
;;; In a let* the binding values are evaluated in an environment with the
;;; previous binding variables defined.  This is achieved by compiling them
;;; in order, adding them to the result environment.  Compare with compile-let
;;; and compile-letrec.
;;;
;;; @@@@ Probably should extend the syntax environment too.

(define (compile-let* result let target context)
  (add-instruction result 'comment "let*" let)
  (define outer-environment (result-environment result))
  (define inner-environment (cons '() outer-environment))
  (set-result-environment! result inner-environment)
  (compile-let-bindings result inner-environment (let-bindings let))
  (compile-sequence result (let-body let) target context)
  (set-result-environment! result outer-environment))

;;; compile-letrec result let context
;;;
;;; In a letrec, the binding values are evaluated in an environment with
;;; all the binding variables defined, but with undefined values.  This is
;;; achieved by first extending the environment with all of the variables
;;; bound to new registers, then continuing as for compile-let*.
;;;
;;; @@@@ It's an error to directly refer to another letrec variable, and
;;; this isn't detected at the moment it will work if the reference is after
;;; the definition, and give an undefined variable error otherwise, because
;;; the binding will contain obj_unbound, at least the first time around
;;; it will.  If the letrec is in a loop, you get the old value!
;;;
;;; @@@@ Probably should extend the syntax environment too.

(define (compile-letrec result let target context)
  (add-instruction result 'comment "let*" let)
  (define outer-environment (result-environment result))
  (define inner-environment (cons '() outer-environment))
  (for-each
    (lambda (binding)
      (environment-define result inner-environment (let-binding-variable binding)))
    (let-bindings let))
  (set-result-environment! result inner-environment)
  (compile-let-bindings result inner-environment (let-bindings let))
  (compile-sequence result (let-body let) target context)
  (set-result-environment! result outer-environment))

;;; compile-set result set context

(define (compile-set result set target context)
  (if target
    (error "set! used where a result was expected"))
  (add-instruction result 'comment "set" set)
  (define mark (result-next-register result))
  (define value-register (new-register result))
  (compile-form result (set-value set) value-register 'body)
  (define binding (result-lookup result (set-variable set)))
  (if binding
    (add-instruction result 'set-local (cdr binding) value-register)
    (add-instruction result 'set-free (find-free-variable result (set-variable set)) value-register))
  (set-result-next-register! result mark)
  (compile-context result target context))

(define (compile-and result and target context)
  (add-instruction result 'comment "and" and)
  (define clauses (cdr and))
  (if (null? clauses)
    (compile-constant result #t target context)
    (let ((fail-block (if (integer? context) context (new-block result))))
      (let loop ((clauses clauses))
        (if (null? (cdr clauses))
          (compile-form result (car clauses) target context)
          (begin
            (compile-form result (car clauses) target 'body)
            (add-instruction result 'branch-false fail-block target)
            (loop (cdr clauses)))))
      (cond
        ((eq? context 'body)
          (add-instruction result 'jump fail-block)
          (label-block result fail-block))
        ((eq? context 'tail)
          (label-block result fail-block)
          (compile-context result target context))
        ((integer? context))
        (else
          (error "malformed context"))))))

(define (compile-or result or target context)
  (add-instruction result 'comment "or" or)
  (define clauses (cdr or))
  (if (null? clauses)
    (compile-constant result #f target context)
    (let ((succeed-block (new-block result)))
      (let loop ((clauses clauses))
        (if (null? (cdr clauses))
          (compile-form result (car clauses) target context)
          (begin
            (compile-form result (car clauses) target 'body)
            (add-instruction result 'branch-true succeed-block target)
            (loop (cdr clauses)))))
      (cond
        ((eq? context 'body)
          (add-instruction result 'jump succeed-block)
          (label-block result succeed-block))
        ((eq? context 'tail)
          (label-block result succeed-block)
          (compile-context result target context))
        ((integer? context))
        (else
          (error "malformed context"))))))

(define (compile-begin result begin target context)
  (compile-sequence result (sequence-clauses begin) target context))

(define (compile-case result form target context)
  (compile-form result (expand-case form) target context))

(define (compile-cond result form target context)
  (compile-form result (expand-cond form) target context))

(define (compile-delay result form target context)
  (compile-form result (expand-delay form) target context))

(define (compile-do result form target context)
  (compile-form result (expand-do form) target context))

(define (compile-quasiquote result form target context)
  (compile-form result (expand-quasiquote form) target context))

(define (compile-load result load target context)
  (error "load not implemented"))

(define (compile-the-environment result form target context)
  (if (result-environment-register result)
    (if target
      (add-instruction result 'move target (result-environment-register result))
      (add-instruction result 'comment "(the-environment omitted because result not used)" target))
    (error "the-environment not permitted in compiled code except at top level")))

;;; compile-define-macro result form target context
;;;
;;; @@@@ Which environment should macro bodies be evaluated in?
;;; Here it's the init-environment, but when evaluating it's
;;; whatever's available at the time.  Perhaps that's wrong.

(define (compile-define-macro result form target context)
  (if target (error "macro definition used where a result was expected"))
  (define new-binding (make-macro-expander (sc-init-environment) (cdr form)))
  (define variable (car new-binding))
  (define expander (cdr new-binding))
  (define (compile result form target context)
    (compile-form result (apply1 expander (cdr form)) target context))
  (define environment (result-syntax-environment result))
  (define frame (car environment))
  (define old-binding (frame-lookup frame variable))
  (if old-binding
    (set-cdr! old-binding compile)
    (set-car! environment (cons (cons variable compile) frame)))
  ;; If we're at top level we must also compile the expander and code to
  ;; bind it in the top-level environment, so that it can be used later.
  (if (result-environment-register result)
    (compile-form
      result
      `(bind (cdr (the-environment))
             ',(define-macro-symbol form)
             (lambda (env operands)
               (eval (apply1 ,(define-macro-value form) operands) env)))
      target ; @@@@ #f?
      context)
    (compile-context result target context)))

(define basic-syntax
  `(;; R5RS standard syntax
    (and        . ,compile-and)
    (begin      . ,compile-begin)
    (case       . ,compile-case)
    (cond       . ,compile-cond)
    (define     . ,compile-definition)
    (delay      . ,compile-delay)
    (do         . ,compile-do)
    (if         . ,compile-if)
    (lambda     . ,compile-lambda)
    (let        . ,compile-let)
    (let*       . ,compile-let*)
    (letrec     . ,compile-letrec)
    (or         . ,compile-or)
    (quote      . ,compile-quotation)
    (set!       . ,compile-set)
    (,'quasiquote . ,compile-quasiquote)
    ;; Non-R5RS stuff
    (load       . ,compile-load)
    (the-environment . ,compile-the-environment)
    (define-macro . ,compile-define-macro)))

(define (new-result block syntax-environment)
  (make-result 0                        ; next-register
               0                        ; max-register
               (+ block 1)              ; next-block
               (list '())               ; environment
               syntax-environment       ; syntax-environment
               '()                      ; free-variables
               '()                      ; local-variables
               '()                      ; constants
               block                    ; block
               '()                      ; instructions
               '()                      ; blocks
               #f                       ; environment-register
))

;;; compile-form result form target context
;;;
;;; Compile a form, appending it to the result.

(define (compile-form result form target context)
  (cond
    ((self-evaluating? form) (compile-self-evaluating result form target context))
    ((variable? form) (compile-variable-reference result form target context))
    ((pair? form)
      (define operator (car form))
      (define syntax (and (symbol? operator)
                          (environment-lookup (result-syntax-environment result) operator)))
      (if syntax
        ((cdr syntax) result form target context)
        (compile-application result form target context)))
    (else (error "unknown syntax"))))

;;; compile-data result object
;;;
;;; Compiles code which will create the object at run-time.
;;; (Note that compile-data doesn't detect or reproduce sharing
;;; or cycles within the object.)

(define (compile-data result data target)
  (define mark (result-next-register result))
  (cond
    ((boolean? data)
      (add-instruction result (if data 'load-true 'load-false) target))
    ((integer? data)
      (add-instruction result 'make-integer target data))
    ((string? data)
      (add-instruction result
                       'make-string
                       target
                       (string-length data)
                       data))
    ((char? data)
      (add-instruction result
                       'make-character
                       target
                       (char->integer data)))
    ((symbol? data)
      (define string (symbol->string data))
      (add-instruction result
                       'make-symbol
                       target
                       (string-length string)
                       string))
    ((pair? data)
      ;; Build pairs backwards because they're usually lists.
      (compile-data result (cdr data) target)
      (let ((car-register (new-register result)))
        (compile-data result (car data) car-register)
        (add-instruction result 'make-pair target car-register target)))
    ((null? data)
      (add-instruction result 'load-empty target))
    ((vector? data)
      (let ((element-register (new-register result)))
        (add-instruction result 'make-vector target (vector-length data))
        (let loop ((index 0))
          (if (< index (vector-length data))
            (begin
              (compile-data result (vector-ref data index) element-register)
              (add-instruction result 'vector-set target index element-register)
              (loop (+ index 1)))))))
    (else (error "unknown data type")))
  (set-result-next-register! result mark))

;;; compile-top-level-form result form env-args-register
;;;
;;; Each top-level form is compiled into a function which takes the
;;; environment and modifies it with any new bindings.
;;;
;;; The env-args-register is the register in which the argument list
;;; containing the environment is stored.
;;;
;;; @@@@ Too much code in common with compile-lambda.
;;; @@@@ A top-level form like (begin ...) isn't equivalent to its contents
;;; as required by R5RS because all the binding is done at the end.
;;; @@@@ Does this really need to be a function?

(define (compile-top-level-form outer-result form env-args-register)
  (add-instruction outer-result 'comment "top-level form" form)
  (define mark (result-next-register outer-result))
  (define start-block (result-next-block outer-result))
  (define inner-result (new-result start-block (result-syntax-environment outer-result)))
  (add-instruction inner-result 'comment "binder for top-level form" form)
  ;; Compile code to load the environment argument into a register
  (define args-register (new-register inner-result))
  (define env-register (new-register inner-result))
  (define venv-register (new-register inner-result))
  (add-instruction inner-result 'load-args args-register)
  (add-instruction inner-result 'load-car env-register args-register)
  (add-instruction inner-result 'load-car venv-register env-register)
  ;; Allow references to "the-environment" in top-level forms
  (set-result-environment-register! inner-result env-register)
  ;; Compile the form itself
  (compile-form inner-result form #f 'body)
  ;; Compile code to bind the symbols defined in the form in the environment
  (define bindings (car (result-environment inner-result)))
  (add-instruction inner-result 'comment "binding symbols" bindings)
  (if (not (null? bindings))
    (let ((value-register (new-register inner-result))
          (symbol-register (new-register inner-result)))
      (for-each
        (lambda (binding)
          (add-instruction inner-result 'comment "bind" (car binding))
          (compile-constant inner-result (car binding) symbol-register 'body)
          (add-instruction inner-result 'load-local value-register (cdr binding))
          (add-instruction inner-result 'bind venv-register symbol-register value-register))
        bindings)))
  ;; Finish off the procedure
  (compile-context inner-result #f 'tail)
  ;; Compile the procedure preamble
  (define entry-point (compile-preamble inner-result start-block))
  ;; Merge the procedure blocks with the outer result
  (set-result-blocks! outer-result
                      (append (result-blocks outer-result)
                              (result-blocks inner-result)))
  (set-result-next-block! outer-result (result-next-block inner-result))
  ;; Compile code in the outer result to make and call the procedure object.
  (define procedure-register (new-register outer-result))
  (compile-make-procedure outer-result inner-result entry-point procedure-register 'body)
  (add-instruction outer-result 'store-args env-args-register 1)
  (define block (new-block outer-result))
  (add-instruction outer-result 'call procedure-register block)
  (label-block outer-result block)
  (set-result-next-register! outer-result mark))

;;; compile-unit forms
;;;
;;; Compiles a compilation unit which is a list of top-level forms.
;;; Returns the compilation result.
;;;
;;; The compilation result is a function with entry point 0 which,
;;; when applied to an environment, executes the top-level forms and
;;; binds any symbols they define in the environment.
;;;
;;; Free variables in the forms are looked up in the environment.
;;; If they aren't found, they're bound to a special object which
;;; causes an unbound variable error if it's looked up.  This allows
;;; the symbols to be bound later.
;;;
;;; @@@@ Each new compilation shouldn't start with the basic syntax.
;;; Somehow, the syntax from previous compilations needs to be threaded
;;; through.  This implies that compilation is an operator, not a function.

(define (compile-unit forms)
  (define entry-point 0)
  (define result (new-result (+ entry-point 1) (list basic-syntax)))
  (define start-block (result-block result))
  (define env-args-register (new-register result))
  (add-instruction result 'comment "unit forms" forms)
  (for-each
    (lambda (form)
      ;; @@@@ Display hacky progress
      (display form (current-error-port))
      (newline (current-error-port))
      (compile-top-level-form result form env-args-register))
    forms)
  (compile-context result #f 'tail)
  ;; Compile a preamble for the compilation unit
  (label-block result entry-point)
  (add-instruction result 'comment "compile unit entry point" forms)
  (define env-register (new-register result))
  (define venv-register (new-register result))
  (add-instruction result 'load-args env-args-register)
  ;; Extract the first argument (the environment)
  (add-instruction result 'load-car env-register env-args-register)
  ;; Extract the value environment
  (add-instruction result 'load-car venv-register env-register)
  ;; @@@@ Should check argument list
  ;; Make sure all the free variable symbols are in the constants vector, so that we can use
  ;; them to find the free variable bindings later.
  (for-each (lambda (symbol) (find-constant result symbol)) (result-free-variables result))
  ;; Compile code to make the top-level constants vector from scratch.
  (add-instruction result 'comment "making constants" (result-constants result))
  (define constants-register (new-register result))
  (compile-data result (list->vector (result-constants result)) constants-register)
  (add-instruction result 'set-constants constants-register)
  ;; Compile code to make the top-level free variables vector by looking up the free
  ;; variables in the environment passed to the compiled unit entry point.  Any unbound
  ;; variables have bindings created at this point by FIND-FREE, but marked so that
  ;; they cause an "unbound variable" error if used.
  (add-instruction result 'comment "making free vector" (result-free-variables result))
  (define free-vector-register (new-register result))
  (add-instruction result 'make-vector free-vector-register (length (result-free-variables result)))
  (if (not (null? (result-free-variables result)))
    (let ((binding-register (new-register result))
          (symbol-register (new-register result)))
      (for-each-indexed
        (lambda (variable index)
          (add-instruction result 'comment "free variable" variable)
          (compile-constant result variable symbol-register 'body)
          (add-instruction result 'find-free binding-register venv-register symbol-register)
          (add-instruction result 'vector-set free-vector-register index binding-register))
        (result-free-variables result))))
  (add-instruction result 'set-frees free-vector-register)
  ;; Prepend an ENTER instruction to allocate registers and locals
  (if (not (null? (result-local-variables result)))
    (error "some local variables somehow got defined by compile-unit"))
  (prepend-instruction result 'enter (result-max-register result) 0)
  (add-instruction result 'jump start-block)
  (label-block result (new-block result))
  result)

;;; compile-file file-name
;;;
;;; Compiles a file of top-level forms and emits the result.
;;;
;;; @@@@ The unit identified should be more carefully derived to make sure
;;; it's a legal C identifier, and would be the same as the unit identifier
;;; derived from the unit's symbolic name when loaded.  See dynload_macosx.c.

(define (compile-file file-name)

  (define (basename file-name)
    (define (strchr string character)
      (define length (string-length string))
      (let loop ((i 0))
        (if (< i length)
          (if (char=? (string-ref string i) character)
            i
            (loop (+ i 1)))
          #f)))
    (define index (strchr file-name #\.))
    (if index
      (substring file-name 0 index)
      file-name))

  (define port (open-input-file file-name))

  (define forms
    (let loop ((form (read port)) (forms '()))
      (if (eof-object? form)
        (reverse forms)
        (loop (read port) (cons form forms)))))

  (define result (compile-unit forms))

  (result-emit result (basename file-name)))

;;; result-emit result
;;;
;;; Writes C code which can be compiled and loaded into the SC
;;; system.

(define (result-emit result unit-identifier)
  (if (not (null? (result-instructions result)))
    (error "non-empty final list of instructions"))
  (write-string "#include \"opcodes.h\"\n\n")
  (write-string "UNIT_BEGIN(")
  (write-string unit-identifier)
  (write-string ")\n\n")
  (for-each
    (lambda (block)
      (write-string "BLOCK_DECL(")
      (write-string (number->string (car block)))
      (write-string ");\n"))
    (result-blocks result))
  (newline)
  (for-each
    (lambda (block)
      (write-string "BLOCK_BEGIN(")
      (write-string (number->string (car block)))
      (write-string ")\n")
      (for-each
        (lambda (instruction)
          (write-string "  ");
          (if (eq? (car instruction) 'comment)
            (begin
              (write-string "/* ")
              (write-string (cadr instruction))
              (write-string " ")
              (write (caddr instruction))
              (write-string " */\n"))
            (begin
              (write-string
                (case (car instruction)
                  ((move) "OP_MOVE")
                  ((load-static) "OP_LOAD_STATIC")
                  ((load-constant) "OP_LOAD_CONSTANT")
                  ((load-free) "OP_LOAD_FREE")
                  ((load-free-binding) "OP_LOAD_FREE_BINDING")
                  ((set-free) "OP_SET_FREE")
                  ((load-local) "OP_LOAD_LOCAL")
                  ((load-local-binding) "OP_LOAD_LOCAL_BINDING")
                  ((set-local) "OP_SET_LOCAL")
                  ((set-local-binding) "OP_SET_LOCAL_BINDING")
                  ((load-result) "OP_LOAD_RESULT")
                  ((load-args) "OP_LOAD_ARGS")
                  ((load-car) "OP_LOAD_CAR")
                  ((load-cdr) "OP_LOAD_CDR")
                  ((load-arg) "OP_LOAD_ARG")
                  ((last-arg) "OP_LAST_ARG")
                  ((make-pair) "OP_MAKE_PAIR")
                  ((make-vector) "OP_MAKE_VECTOR")
                  ((vector-set) "OP_VECTOR_SET")
                  ((load-true) "OP_LOAD_TRUE")
                  ((load-false) "OP_LOAD_FALSE")
                  ((load-unbound) "OP_LOAD_UNBOUND")
                  ((make-integer) "OP_MAKE_INTEGER")
                  ((make-string) "OP_MAKE_STRING")
                  ((make-character) "OP_MAKE_CHARACTER")
                  ((make-symbol) "OP_MAKE_SYMBOL")
                  ((bind) "OP_BIND")
                  ((find-free) "OP_FIND_FREE")
                  ((set-constants) "OP_SET_CONSTANTS")
                  ((set-frees) "OP_SET_FREES")
                  ((set-locals-vector) "OP_SET_LOCALS_VECTOR")
                  ((load-empty) "OP_LOAD_EMPTY")
                  ((store-args) "OP_STORE_ARGS")
                  ((enter) "OP_ENTER")
                  ((leave) "OP_LEAVE")
                  ((ret) "OP_RET")
                  ((jump) "OP_JUMP")
                  ((branch-false) "OP_BRANCH_FALSE")
                  ((branch-true) "OP_BRANCH_TRUE")
                  ((call) "OP_CALL")
                  ((tail) "OP_TAIL")
                  ((make-proc) "OP_MAKE_PROC")
                  ((set-proc-name) "OP_SET_PROC_NAME")
                  ((load-unspecified) "OP_LOAD_UNSPECIFIED")
                  (else
                    (error (string-append "unknown instruction: "
                                          (symbol->string (car instruction)))))))
              (write-string "(")
              (let loop ((arguments (cdr instruction)))
                (if (pair? arguments)
                  (begin
                    (write (car arguments))
                    (if (not (null? (cdr arguments)))
                      (begin
                        (write-string ", ")
                        (loop (cdr arguments)))))))
              (write-string ");\n"))))
        (cdr block))
      (write-string "BLOCK_END()\n\n"))
    (result-blocks result))
  (write-string "UNIT_END()\n"))


;;; try-compile form
;;;
;;; A test harness.

(define (try-compile form)
  (define result (new-result 1000 (list basic-syntax)))
  (define block (new-block result))
  (define target (new-register result))
  (compile-form result form target block)
  (label-block result block)
  (result-emit result)
  result)
