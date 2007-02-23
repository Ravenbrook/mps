(in-package :C)

(setf lisp::*enable-package-locked-errors* nil)
(handler-bind ((error (lambda (c)
			(declare (ignore c))
			(invoke-restart 'continue))))
  (def-boolean-attribute ir1
      ;;
      ;; May call functions that are passed as arguments.  In order to determine
      ;; what other effects are present, we must find the effects of all arguments
      ;; that may be functions.
      call
    ;;
    ;; May incorporate function or number arguments into the result or somehow
    ;; pass them upward.  Note that this applies to any argument that *might* be
    ;; a function or number, not just the arguments that always are.
    unsafe
    ;;
    ;; May fail to return during correct execution.  Errors are O.K.
    unwind
    ;;
    ;; The (default) worst case.  Includes all the other bad things, plus any
    ;; other possible bad thing.  If this is present, the above bad attributes
    ;; will be explicitly present as well.
    any
    ;;
    ;; May be constant-folded.  The function has no side effects, but may be
    ;; affected by side effects on the arguments.  e.g. SVREF, MAPC.  Functions
    ;; that side-effect their arguments are not considered to be foldable.
    ;; Although it would be "legal" to constant fold them (since it "is an error"
    ;; to modify a constant), we choose not to mark theses functions as foldable
    ;; in this database.
    foldable
    ;;
    ;; May be eliminated if value is unused.  The function has no side effects
    ;; except possibly CONS.  If a function is defined to signal errors, then it
    ;; is not flushable even if it is movable or foldable.
    flushable
    ;;
    ;; May be moved with impunity.  Has no side effects except possibly CONS, and
    ;; is affected only by its arguments.
    movable
    ;;
    ;; Function is a true predicate likely to be open-coded.  Convert any
    ;; non-conditional uses into (IF <pred> T NIL).
    predicate
    ;;
    ;; Inhibit any warning for compiling a recursive definition.  [Normally the
    ;; compiler warns when compiling a recursive definition for a known function,
    ;; since it might be a botched interpreter stub.]
    recursive
    ;;
    ;; Function does explicit argument type checking, so the declared type should
    ;; not be asserted when a definition is compiled.
    explicit-check
    ;;
    ;; Safe to stack-allocate function args that are closures.
    dynamic-extent-closure-safe
    ;; Return value is important and ignoring it is probably a mistake.
    ;; This is for things like not using the result of nreverse.  This
    ;; is only for warnings and has no effect on optimization.
    important-result
    ))

(handler-bind ((error (lambda (c)
			(declare (ignore c))
			(invoke-restart 'kernel::clobber-it))))

  (defstruct (function-info
	       (:print-function %print-function-info)
	       (:pure t))
    ;;
    ;; Boolean attributes of this function.
    (attributes (required-argument) :type attributes)
    ;;
    ;; A list of Transform structures describing transforms for this function.
    (transforms () :type list)
    ;;
    ;; A function which computes the derived type for a call to this function by
    ;; examining the arguments.  This is null when there is no special method for
    ;; this function.
    (derive-type nil :type (or function null))
    ;;
    ;; A function that does random unspecified code transformations by directly
    ;; hacking the IR.  Returns true if further optimizations of the call
    ;; shouldn't be attempted.
    (optimizer nil :type (or function null))
    ;;
    ;; If true, a special-case LTN annotation method that is used in place of the
    ;; standard type/policy template selection.  It may use arbitrary code to
    ;; choose a template, decide to do a full call, or conspire with the
    ;; IR2-Convert method to do almost anything.  The Combination node is passed
    ;; as the argument.
    (ltn-annotate nil :type (or function null))
    ;;
    ;; If true, the special-case IR2 conversion method for this function.  This
    ;; deals with funny functions, and anything else that can't be handled using
    ;; the template mechanism.  The Combination node and the IR2-Block are passed
    ;; as arguments.
    (ir2-convert nil :type (or function null))
    ;;
    ;; A list of all the templates that could be used to translate this function
    ;; into IR2, sorted by increasing cost.
    (templates nil :type list)
    ;;
    ;; If non-null, then this function is a unary type predicate for this type.
    (predicate-type nil :type (or ctype null))
    ;;
    ;; If non-null, use this function to annotate the known call for the byte
    ;; compiler.  If it returns NIL, then change the call to :full.
    (byte-annotate nil :type (or function null))
    ;;
    ;; If non-null, use this function to generate the byte code for this known
    ;; call.  This function can only give up if there is a byte-annotate function
    ;; that arranged for the functional to be pushed onto the stack.
    (byte-compile nil :type (or function null))
    ;; A function computing the constant or literal arguments which are
    ;; destructively modified by the call.
    (destroyed-constant-args nil :type (or function null))
    ))
