#include "opcodes.h"

BLOCK_DECL(0);
BLOCK_DECL(40);
BLOCK_DECL(22);
BLOCK_DECL(1);
BLOCK_DECL(21);
BLOCK_DECL(2);
BLOCK_DECL(20);
BLOCK_DECL(5);
BLOCK_DECL(19);
BLOCK_DECL(18);
BLOCK_DECL(14);
BLOCK_DECL(17);
BLOCK_DECL(16);
BLOCK_DECL(15);
BLOCK_DECL(13);
BLOCK_DECL(12);
BLOCK_DECL(11);
BLOCK_DECL(10);
BLOCK_DECL(9);
BLOCK_DECL(8);
BLOCK_DECL(7);
BLOCK_DECL(6);
BLOCK_DECL(4);
BLOCK_DECL(3);
BLOCK_DECL(39);
BLOCK_DECL(23);
BLOCK_DECL(38);
BLOCK_DECL(29);
BLOCK_DECL(37);
BLOCK_DECL(31);
BLOCK_DECL(28);
BLOCK_DECL(27);
BLOCK_DECL(26);
BLOCK_DECL(25);
BLOCK_DECL(24);
BLOCK_DECL(43);
BLOCK_DECL(29);
BLOCK_DECL(42);
BLOCK_DECL(33);
BLOCK_DECL(41);
BLOCK_DECL(40);
BLOCK_DECL(39);
BLOCK_DECL(38);
BLOCK_DECL(37);
BLOCK_DECL(36);
BLOCK_DECL(35);
BLOCK_DECL(34);
BLOCK_DECL(32);
BLOCK_DECL(31);
BLOCK_DECL(30);
BLOCK_DECL(30);
BLOCK_DECL(29);
BLOCK_DECL(36);
BLOCK_DECL(35);
BLOCK_DECL(34);
BLOCK_DECL(33);
BLOCK_DECL(32);

BLOCK_BEGIN(0)
  OP_ENTER(83, 0);
  /* compile unit entry point ((define (backtrace continuation) (if (procedure? continuation) (if (eq? continuation (procedure-continuation continuation)) (values) (begin (display "backtrace ") (display (...)) (case (...) (...)) (newline) (backtrace (...)))) (error "backtrace of non-procedure"))) (define (read-eval-print env) (display "SC> ") (define form (read)) (if (not (eof-object? form)) (begin (catch (lambda (...) (...) (...) (...) (...)) (call-with-values (...) (...))) (read-eval-print env))))) */
  /* making constants ("backtrace " (eval define) " " 1 "backtrace of non-procedure" continuation |generated symbol #11| backtrace "SC> " 0 "=" "> " results index loop "exception> " exception env form read-eval-print) */
  OP_MAKE_VECTOR(32, 20);
  OP_MAKE_STRING(33, 10, "backtrace ");
  OP_VECTOR_SET(32, 0, 33);
  OP_MAKE_SYMBOL(35, 4, "eval");
  OP_MAKE_SYMBOL(37, 6, "define");
  OP_LOAD_EMPTY(38);
  OP_MAKE_PAIR(36, 37, 38);
  OP_MAKE_PAIR(34, 35, 36);
  OP_VECTOR_SET(32, 1, 34);
  OP_MAKE_STRING(39, 1, " ");
  OP_VECTOR_SET(32, 2, 39);
  OP_MAKE_INTEGER(40, 1);
  OP_VECTOR_SET(32, 3, 40);
  OP_MAKE_STRING(41, 26, "backtrace of non-procedure");
  OP_VECTOR_SET(32, 4, 41);
  OP_MAKE_SYMBOL(42, 12, "continuation");
  OP_VECTOR_SET(32, 5, 42);
  OP_MAKE_SYMBOL(43, 20, "generated symbol #11");
  OP_VECTOR_SET(32, 6, 43);
  OP_MAKE_SYMBOL(44, 9, "backtrace");
  OP_VECTOR_SET(32, 7, 44);
  OP_MAKE_STRING(45, 4, "SC> ");
  OP_VECTOR_SET(32, 8, 45);
  OP_MAKE_INTEGER(46, 0);
  OP_VECTOR_SET(32, 9, 46);
  OP_MAKE_STRING(47, 1, "=");
  OP_VECTOR_SET(32, 10, 47);
  OP_MAKE_STRING(48, 2, "> ");
  OP_VECTOR_SET(32, 11, 48);
  OP_MAKE_SYMBOL(49, 7, "results");
  OP_VECTOR_SET(32, 12, 49);
  OP_MAKE_SYMBOL(50, 5, "index");
  OP_VECTOR_SET(32, 13, 50);
  OP_MAKE_SYMBOL(51, 4, "loop");
  OP_VECTOR_SET(32, 14, 51);
  OP_MAKE_STRING(52, 11, "exception> ");
  OP_VECTOR_SET(32, 15, 52);
  OP_MAKE_SYMBOL(53, 9, "exception");
  OP_VECTOR_SET(32, 16, 53);
  OP_MAKE_SYMBOL(54, 3, "env");
  OP_VECTOR_SET(32, 17, 54);
  OP_MAKE_SYMBOL(55, 4, "form");
  OP_VECTOR_SET(32, 18, 55);
  OP_MAKE_SYMBOL(56, 15, "read-eval-print");
  OP_VECTOR_SET(32, 19, 56);
  OP_LOAD_ARGS(0);
  OP_LOAD_CAR(57, 0);
  /* making free vector (procedure? procedure-continuation eq? values display procedure-name memv procedure-local write newline backtrace error read eof-object? not null? car + cdr eval call-with-values catch read-eval-print) */
  OP_MAKE_VECTOR(58, 23);
  /* free variable procedure? */
  OP_MAKE_SYMBOL(60, 10, "procedure?");
  OP_FIND_FREE(59, 57, 60);
  OP_VECTOR_SET(58, 0, 59);
  /* free variable procedure-continuation */
  OP_MAKE_SYMBOL(61, 22, "procedure-continuation");
  OP_FIND_FREE(59, 57, 61);
  OP_VECTOR_SET(58, 1, 59);
  /* free variable eq? */
  OP_MAKE_SYMBOL(62, 3, "eq?");
  OP_FIND_FREE(59, 57, 62);
  OP_VECTOR_SET(58, 2, 59);
  /* free variable values */
  OP_MAKE_SYMBOL(63, 6, "values");
  OP_FIND_FREE(59, 57, 63);
  OP_VECTOR_SET(58, 3, 59);
  /* free variable display */
  OP_MAKE_SYMBOL(64, 7, "display");
  OP_FIND_FREE(59, 57, 64);
  OP_VECTOR_SET(58, 4, 59);
  /* free variable procedure-name */
  OP_MAKE_SYMBOL(65, 14, "procedure-name");
  OP_FIND_FREE(59, 57, 65);
  OP_VECTOR_SET(58, 5, 59);
  /* free variable memv */
  OP_MAKE_SYMBOL(66, 4, "memv");
  OP_FIND_FREE(59, 57, 66);
  OP_VECTOR_SET(58, 6, 59);
  /* free variable procedure-local */
  OP_MAKE_SYMBOL(67, 15, "procedure-local");
  OP_FIND_FREE(59, 57, 67);
  OP_VECTOR_SET(58, 7, 59);
  /* free variable write */
  OP_MAKE_SYMBOL(68, 5, "write");
  OP_FIND_FREE(59, 57, 68);
  OP_VECTOR_SET(58, 8, 59);
  /* free variable newline */
  OP_MAKE_SYMBOL(69, 7, "newline");
  OP_FIND_FREE(59, 57, 69);
  OP_VECTOR_SET(58, 9, 59);
  /* free variable backtrace */
  OP_MAKE_SYMBOL(70, 9, "backtrace");
  OP_FIND_FREE(59, 57, 70);
  OP_VECTOR_SET(58, 10, 59);
  /* free variable error */
  OP_MAKE_SYMBOL(71, 5, "error");
  OP_FIND_FREE(59, 57, 71);
  OP_VECTOR_SET(58, 11, 59);
  /* free variable read */
  OP_MAKE_SYMBOL(72, 4, "read");
  OP_FIND_FREE(59, 57, 72);
  OP_VECTOR_SET(58, 12, 59);
  /* free variable eof-object? */
  OP_MAKE_SYMBOL(73, 11, "eof-object?");
  OP_FIND_FREE(59, 57, 73);
  OP_VECTOR_SET(58, 13, 59);
  /* free variable not */
  OP_MAKE_SYMBOL(74, 3, "not");
  OP_FIND_FREE(59, 57, 74);
  OP_VECTOR_SET(58, 14, 59);
  /* free variable null? */
  OP_MAKE_SYMBOL(75, 5, "null?");
  OP_FIND_FREE(59, 57, 75);
  OP_VECTOR_SET(58, 15, 59);
  /* free variable car */
  OP_MAKE_SYMBOL(76, 3, "car");
  OP_FIND_FREE(59, 57, 76);
  OP_VECTOR_SET(58, 16, 59);
  /* free variable + */
  OP_MAKE_SYMBOL(77, 1, "+");
  OP_FIND_FREE(59, 57, 77);
  OP_VECTOR_SET(58, 17, 59);
  /* free variable cdr */
  OP_MAKE_SYMBOL(78, 3, "cdr");
  OP_FIND_FREE(59, 57, 78);
  OP_VECTOR_SET(58, 18, 59);
  /* free variable eval */
  OP_MAKE_SYMBOL(79, 4, "eval");
  OP_FIND_FREE(59, 57, 79);
  OP_VECTOR_SET(58, 19, 59);
  /* free variable call-with-values */
  OP_MAKE_SYMBOL(80, 16, "call-with-values");
  OP_FIND_FREE(59, 57, 80);
  OP_VECTOR_SET(58, 20, 59);
  /* free variable catch */
  OP_MAKE_SYMBOL(81, 5, "catch");
  OP_FIND_FREE(59, 57, 81);
  OP_VECTOR_SET(58, 21, 59);
  /* free variable read-eval-print */
  OP_MAKE_SYMBOL(82, 15, "read-eval-print");
  OP_FIND_FREE(59, 57, 82);
  OP_VECTOR_SET(58, 22, 59);
  OP_SET_ENV(32, 58);
  OP_JUMP(1);
BLOCK_END()

BLOCK_BEGIN(40)
  /* values () */
  OP_LOAD_EMPTY(31);
  OP_STORE_ARGS(31, 0);
  OP_LEAVE();
  OP_RET();
BLOCK_END()

BLOCK_BEGIN(22)
  /* top-level form (define (read-eval-print env) (display "SC> ") (define form (read)) (if (not (eof-object? form)) (begin (catch (lambda (exception continuation) (display "exception> ") (write exception) (newline) (backtrace continuation)) (call-with-values (lambda () (...)) (lambda results (...)))) (read-eval-print env)))) */
  /* procedure constants ("SC> " 0 "=" "> " 1 results index loop "exception> " exception continuation env form read-eval-print) */
  OP_MAKE_VECTOR(13, 14);
  OP_LOAD_CONSTANT(14, 8);
  OP_VECTOR_SET(13, 0, 14);
  OP_LOAD_CONSTANT(15, 9);
  OP_VECTOR_SET(13, 1, 15);
  OP_LOAD_CONSTANT(16, 10);
  OP_VECTOR_SET(13, 2, 16);
  OP_LOAD_CONSTANT(17, 11);
  OP_VECTOR_SET(13, 3, 17);
  OP_LOAD_CONSTANT(18, 3);
  OP_VECTOR_SET(13, 4, 18);
  OP_LOAD_CONSTANT(19, 12);
  OP_VECTOR_SET(13, 5, 19);
  OP_LOAD_CONSTANT(20, 13);
  OP_VECTOR_SET(13, 6, 20);
  OP_LOAD_CONSTANT(21, 14);
  OP_VECTOR_SET(13, 7, 21);
  OP_LOAD_CONSTANT(22, 15);
  OP_VECTOR_SET(13, 8, 22);
  OP_LOAD_CONSTANT(23, 16);
  OP_VECTOR_SET(13, 9, 23);
  OP_LOAD_CONSTANT(24, 5);
  OP_VECTOR_SET(13, 10, 24);
  OP_LOAD_CONSTANT(25, 17);
  OP_VECTOR_SET(13, 11, 25);
  OP_LOAD_CONSTANT(26, 18);
  OP_VECTOR_SET(13, 12, 26);
  OP_LOAD_CONSTANT(27, 19);
  OP_VECTOR_SET(13, 13, 27);
  /* procedure free variables (display read eof-object? not null? car write newline + cdr eval call-with-values backtrace catch read-eval-print) */
  OP_MAKE_VECTOR(28, 15);
  /* procedure free variable display */
  OP_LOAD_FREE_BINDING(29, 4);
  OP_VECTOR_SET(28, 0, 29);
  /* procedure free variable read */
  OP_LOAD_FREE_BINDING(29, 12);
  OP_VECTOR_SET(28, 1, 29);
  /* procedure free variable eof-object? */
  OP_LOAD_FREE_BINDING(29, 13);
  OP_VECTOR_SET(28, 2, 29);
  /* procedure free variable not */
  OP_LOAD_FREE_BINDING(29, 14);
  OP_VECTOR_SET(28, 3, 29);
  /* procedure free variable null? */
  OP_LOAD_FREE_BINDING(29, 15);
  OP_VECTOR_SET(28, 4, 29);
  /* procedure free variable car */
  OP_LOAD_FREE_BINDING(29, 16);
  OP_VECTOR_SET(28, 5, 29);
  /* procedure free variable write */
  OP_LOAD_FREE_BINDING(29, 8);
  OP_VECTOR_SET(28, 6, 29);
  /* procedure free variable newline */
  OP_LOAD_FREE_BINDING(29, 9);
  OP_VECTOR_SET(28, 7, 29);
  /* procedure free variable + */
  OP_LOAD_FREE_BINDING(29, 17);
  OP_VECTOR_SET(28, 8, 29);
  /* procedure free variable cdr */
  OP_LOAD_FREE_BINDING(29, 18);
  OP_VECTOR_SET(28, 9, 29);
  /* procedure free variable eval */
  OP_LOAD_FREE_BINDING(29, 19);
  OP_VECTOR_SET(28, 10, 29);
  /* procedure free variable call-with-values */
  OP_LOAD_FREE_BINDING(29, 20);
  OP_VECTOR_SET(28, 11, 29);
  /* procedure free variable backtrace */
  OP_LOAD_FREE_BINDING(29, 10);
  OP_VECTOR_SET(28, 12, 29);
  /* procedure free variable catch */
  OP_LOAD_FREE_BINDING(29, 21);
  OP_VECTOR_SET(28, 13, 29);
  /* procedure free variable read-eval-print */
  OP_LOAD_FREE_BINDING(29, 22);
  OP_VECTOR_SET(28, 14, 29);
  OP_MAKE_PROC(30, 13, 28, 39);
  OP_STORE_ARGS(0, 1);
  OP_CALL(30, 40);
BLOCK_END()

BLOCK_BEGIN(1)
  /* unit forms ((define (backtrace continuation) (if (procedure? continuation) (if (eq? continuation (procedure-continuation continuation)) (values) (begin (display "backtrace ") (display (...)) (case (...) (...)) (newline) (backtrace (...)))) (error "backtrace of non-procedure"))) (define (read-eval-print env) (display "SC> ") (define form (read)) (if (not (eof-object? form)) (begin (catch (lambda (...) (...) (...) (...) (...)) (call-with-values (...) (...))) (read-eval-print env))))) */
  /* top-level form (define (backtrace continuation) (if (procedure? continuation) (if (eq? continuation (procedure-continuation continuation)) (values) (begin (display "backtrace ") (display (procedure-name continuation)) (case (procedure-name continuation) ((...) (...) (...))) (newline) (backtrace (procedure-continuation continuation)))) (error "backtrace of non-procedure"))) */
  /* procedure constants ("backtrace " (eval define) " " 1 "backtrace of non-procedure" continuation |generated symbol #11| backtrace) */
  OP_MAKE_VECTOR(1, 8);
  OP_LOAD_CONSTANT(2, 0);
  OP_VECTOR_SET(1, 0, 2);
  OP_LOAD_CONSTANT(3, 1);
  OP_VECTOR_SET(1, 1, 3);
  OP_LOAD_CONSTANT(4, 2);
  OP_VECTOR_SET(1, 2, 4);
  OP_LOAD_CONSTANT(5, 3);
  OP_VECTOR_SET(1, 3, 5);
  OP_LOAD_CONSTANT(6, 4);
  OP_VECTOR_SET(1, 4, 6);
  OP_LOAD_CONSTANT(7, 5);
  OP_VECTOR_SET(1, 5, 7);
  OP_LOAD_CONSTANT(8, 6);
  OP_VECTOR_SET(1, 6, 8);
  OP_LOAD_CONSTANT(9, 7);
  OP_VECTOR_SET(1, 7, 9);
  /* procedure free variables (procedure? procedure-continuation eq? values display procedure-name memv procedure-local write newline backtrace error) */
  OP_MAKE_VECTOR(10, 12);
  /* procedure free variable procedure? */
  OP_LOAD_FREE_BINDING(11, 0);
  OP_VECTOR_SET(10, 0, 11);
  /* procedure free variable procedure-continuation */
  OP_LOAD_FREE_BINDING(11, 1);
  OP_VECTOR_SET(10, 1, 11);
  /* procedure free variable eq? */
  OP_LOAD_FREE_BINDING(11, 2);
  OP_VECTOR_SET(10, 2, 11);
  /* procedure free variable values */
  OP_LOAD_FREE_BINDING(11, 3);
  OP_VECTOR_SET(10, 3, 11);
  /* procedure free variable display */
  OP_LOAD_FREE_BINDING(11, 4);
  OP_VECTOR_SET(10, 4, 11);
  /* procedure free variable procedure-name */
  OP_LOAD_FREE_BINDING(11, 5);
  OP_VECTOR_SET(10, 5, 11);
  /* procedure free variable memv */
  OP_LOAD_FREE_BINDING(11, 6);
  OP_VECTOR_SET(10, 6, 11);
  /* procedure free variable procedure-local */
  OP_LOAD_FREE_BINDING(11, 7);
  OP_VECTOR_SET(10, 7, 11);
  /* procedure free variable write */
  OP_LOAD_FREE_BINDING(11, 8);
  OP_VECTOR_SET(10, 8, 11);
  /* procedure free variable newline */
  OP_LOAD_FREE_BINDING(11, 9);
  OP_VECTOR_SET(10, 9, 11);
  /* procedure free variable backtrace */
  OP_LOAD_FREE_BINDING(11, 10);
  OP_VECTOR_SET(10, 10, 11);
  /* procedure free variable error */
  OP_LOAD_FREE_BINDING(11, 11);
  OP_VECTOR_SET(10, 11, 11);
  OP_MAKE_PROC(12, 1, 10, 21);
  OP_STORE_ARGS(0, 1);
  OP_CALL(12, 22);
BLOCK_END()

BLOCK_BEGIN(21)
  OP_ENTER(19, 1);
  /* make local bindings (backtrace) */
  OP_LOAD_UNBOUND(17);
  OP_LOAD_CONSTANT(18, 7);
  OP_MAKE_PAIR(16, 18, 17);
  OP_SET_LOCAL_BINDING(0, 16);
  OP_JUMP(2);
BLOCK_END()

BLOCK_BEGIN(2)
  OP_LOAD_ARGS(1);
  OP_LOAD_CAR(0, 1);
  /* definition (define (backtrace continuation) (if (procedure? continuation) (if (eq? continuation (procedure-continuation continuation)) (values) (begin (display "backtrace ") (display (procedure-name continuation)) (case (procedure-name continuation) ((...) (...) (...))) (newline) (backtrace (procedure-continuation continuation)))) (error "backtrace of non-procedure"))) */
  /* lambda (lambda (continuation) (if (procedure? continuation) (if (eq? continuation (procedure-continuation continuation)) (values) (begin (display "backtrace ") (display (procedure-name continuation)) (case (procedure-name continuation) ((...) (...) (...))) (newline) (backtrace (procedure-continuation continuation)))) (error "backtrace of non-procedure"))) */
  /* procedure constants ("backtrace " (eval define) " " 1 "backtrace of non-procedure" continuation |generated symbol #11|) */
  OP_MAKE_VECTOR(2, 7);
  OP_LOAD_CONSTANT(3, 0);
  OP_VECTOR_SET(2, 0, 3);
  OP_LOAD_CONSTANT(4, 1);
  OP_VECTOR_SET(2, 1, 4);
  OP_LOAD_CONSTANT(5, 2);
  OP_VECTOR_SET(2, 2, 5);
  OP_LOAD_CONSTANT(6, 3);
  OP_VECTOR_SET(2, 3, 6);
  OP_LOAD_CONSTANT(7, 4);
  OP_VECTOR_SET(2, 4, 7);
  OP_LOAD_CONSTANT(8, 5);
  OP_VECTOR_SET(2, 5, 8);
  OP_LOAD_CONSTANT(9, 6);
  OP_VECTOR_SET(2, 6, 9);
  /* procedure free variables (procedure? procedure-continuation eq? values display procedure-name memv procedure-local write newline backtrace error) */
  OP_MAKE_VECTOR(10, 12);
  /* procedure free variable procedure? */
  OP_LOAD_FREE_BINDING(11, 0);
  OP_VECTOR_SET(10, 0, 11);
  /* procedure free variable procedure-continuation */
  OP_LOAD_FREE_BINDING(11, 1);
  OP_VECTOR_SET(10, 1, 11);
  /* procedure free variable eq? */
  OP_LOAD_FREE_BINDING(11, 2);
  OP_VECTOR_SET(10, 2, 11);
  /* procedure free variable values */
  OP_LOAD_FREE_BINDING(11, 3);
  OP_VECTOR_SET(10, 3, 11);
  /* procedure free variable display */
  OP_LOAD_FREE_BINDING(11, 4);
  OP_VECTOR_SET(10, 4, 11);
  /* procedure free variable procedure-name */
  OP_LOAD_FREE_BINDING(11, 5);
  OP_VECTOR_SET(10, 5, 11);
  /* procedure free variable memv */
  OP_LOAD_FREE_BINDING(11, 6);
  OP_VECTOR_SET(10, 6, 11);
  /* procedure free variable procedure-local */
  OP_LOAD_FREE_BINDING(11, 7);
  OP_VECTOR_SET(10, 7, 11);
  /* procedure free variable write */
  OP_LOAD_FREE_BINDING(11, 8);
  OP_VECTOR_SET(10, 8, 11);
  /* procedure free variable newline */
  OP_LOAD_FREE_BINDING(11, 9);
  OP_VECTOR_SET(10, 9, 11);
  /* procedure free variable backtrace */
  OP_LOAD_FREE_BINDING(11, 10);
  OP_VECTOR_SET(10, 10, 11);
  /* procedure free variable error */
  OP_LOAD_FREE_BINDING(11, 11);
  OP_VECTOR_SET(10, 11, 11);
  OP_MAKE_PROC(12, 2, 10, 20);
  OP_SET_LOCAL(0, 12);
  /* binding symbols ((backtrace . 0)) */
  /* bind backtrace */
  OP_LOAD_CONSTANT(14, 7);
  OP_LOAD_LOCAL(13, 0);
  OP_BIND(0, 14, 13);
  /* values () */
  OP_LOAD_EMPTY(15);
  OP_STORE_ARGS(15, 0);
  OP_LEAVE();
  OP_RET();
BLOCK_END()

BLOCK_BEGIN(20)
  OP_ENTER(64, 2);
  /* make local bindings (continuation |generated symbol #11|) */
  OP_LOAD_UNBOUND(61);
  OP_LOAD_CONSTANT(62, 5);
  OP_MAKE_PAIR(60, 62, 61);
  OP_SET_LOCAL_BINDING(0, 60);
  OP_LOAD_CONSTANT(63, 6);
  OP_MAKE_PAIR(60, 63, 61);
  OP_SET_LOCAL_BINDING(1, 60);
  OP_JUMP(3);
BLOCK_END()

BLOCK_BEGIN(5)
  /* application (error "backtrace of non-procedure") */
  /* operands ("backtrace of non-procedure") */
  OP_LOAD_EMPTY(57);
  /* operand "backtrace of non-procedure" */
  /* self-evaluating "backtrace of non-procedure" */
  OP_LOAD_CONSTANT(58, 4);
  OP_MAKE_PAIR(57, 58, 57);
  OP_STORE_ARGS(57, 1);
  /* free variable error */
  OP_LOAD_FREE(59, 11);
  OP_LEAVE();
  OP_TAIL(59);
BLOCK_END()

BLOCK_BEGIN(19)
  OP_LOAD_RESULT(55);
  OP_MAKE_PAIR(51, 55, 51);
  OP_STORE_ARGS(51, 1);
  /* free variable backtrace */
  OP_LOAD_FREE(56, 10);
  OP_LEAVE();
  OP_TAIL(56);
BLOCK_END()

BLOCK_BEGIN(18)
  OP_LOAD_RESULT(50);
  /* application (backtrace (procedure-continuation continuation)) */
  /* operands ((procedure-continuation continuation)) */
  OP_LOAD_EMPTY(51);
  /* operand (procedure-continuation continuation) */
  /* application (procedure-continuation continuation) */
  /* operands (continuation) */
  OP_LOAD_EMPTY(52);
  /* operand continuation */
  /* local variable continuation */
  OP_LOAD_LOCAL(53, 0);
  OP_MAKE_PAIR(52, 53, 52);
  OP_STORE_ARGS(52, 1);
  /* free variable procedure-continuation */
  OP_LOAD_FREE(54, 1);
  OP_CALL(54, 19);
BLOCK_END()

BLOCK_BEGIN(14)
  /* application (newline) */
  /* operands () */
  OP_LOAD_EMPTY(48);
  OP_STORE_ARGS(48, 0);
  /* free variable newline */
  OP_LOAD_FREE(49, 9);
  OP_CALL(49, 18);
BLOCK_END()

BLOCK_BEGIN(17)
  OP_LOAD_RESULT(47);
  OP_JUMP(14);
BLOCK_END()

BLOCK_BEGIN(16)
  OP_LOAD_RESULT(45);
  OP_MAKE_PAIR(40, 45, 40);
  OP_STORE_ARGS(40, 1);
  /* free variable write */
  OP_LOAD_FREE(46, 8);
  OP_CALL(46, 17);
BLOCK_END()

BLOCK_BEGIN(15)
  OP_LOAD_RESULT(39);
  /* application (write (procedure-local continuation 1)) */
  /* operands ((procedure-local continuation 1)) */
  OP_LOAD_EMPTY(40);
  /* operand (procedure-local continuation 1) */
  /* application (procedure-local continuation 1) */
  /* operands (continuation 1) */
  OP_LOAD_EMPTY(41);
  /* operand 1 */
  /* self-evaluating 1 */
  OP_LOAD_CONSTANT(42, 3);
  OP_MAKE_PAIR(41, 42, 41);
  /* operand continuation */
  /* local variable continuation */
  OP_LOAD_LOCAL(43, 0);
  OP_MAKE_PAIR(41, 43, 41);
  OP_STORE_ARGS(41, 2);
  /* free variable procedure-local */
  OP_LOAD_FREE(44, 7);
  OP_CALL(44, 16);
BLOCK_END()

BLOCK_BEGIN(13)
  OP_LOAD_RESULT(35);
  OP_BRANCH(14, 35);
  /* application (display " ") */
  /* operands (" ") */
  OP_LOAD_EMPTY(36);
  /* operand " " */
  /* self-evaluating " " */
  OP_LOAD_CONSTANT(37, 2);
  OP_MAKE_PAIR(36, 37, 36);
  OP_STORE_ARGS(36, 1);
  /* free variable display */
  OP_LOAD_FREE(38, 4);
  OP_CALL(38, 15);
BLOCK_END()

BLOCK_BEGIN(12)
  OP_LOAD_RESULT(30);
  OP_SET_LOCAL(1, 30);
  /* if (if (memv |generated symbol #11| '(eval define)) (begin (display " ") (write (procedure-local continuation 1)))) */
  /* application (memv |generated symbol #11| '(eval define)) */
  /* operands (|generated symbol #11| '(eval define)) */
  OP_LOAD_EMPTY(31);
  /* operand '(eval define) */
  /* quotation (eval define) */
  OP_LOAD_CONSTANT(32, 1);
  OP_MAKE_PAIR(31, 32, 31);
  /* operand |generated symbol #11| */
  /* local variable |generated symbol #11| */
  OP_LOAD_LOCAL(33, 1);
  OP_MAKE_PAIR(31, 33, 31);
  OP_STORE_ARGS(31, 2);
  /* free variable memv */
  OP_LOAD_FREE(34, 6);
  OP_CALL(34, 13);
BLOCK_END()

BLOCK_BEGIN(11)
  OP_LOAD_RESULT(26);
  /* let (let ((|generated symbol #11| (procedure-name continuation))) (if (memv |generated symbol #11| '(eval define)) (begin (display " ") (write (procedure-local continuation 1))))) */
  /* application (procedure-name continuation) */
  /* operands (continuation) */
  OP_LOAD_EMPTY(27);
  /* operand continuation */
  /* local variable continuation */
  OP_LOAD_LOCAL(28, 0);
  OP_MAKE_PAIR(27, 28, 27);
  OP_STORE_ARGS(27, 1);
  /* free variable procedure-name */
  OP_LOAD_FREE(29, 5);
  OP_CALL(29, 12);
BLOCK_END()

BLOCK_BEGIN(10)
  OP_LOAD_RESULT(24);
  OP_MAKE_PAIR(20, 24, 20);
  OP_STORE_ARGS(20, 1);
  /* free variable display */
  OP_LOAD_FREE(25, 4);
  OP_CALL(25, 11);
BLOCK_END()

BLOCK_BEGIN(9)
  OP_LOAD_RESULT(19);
  /* application (display (procedure-name continuation)) */
  /* operands ((procedure-name continuation)) */
  OP_LOAD_EMPTY(20);
  /* operand (procedure-name continuation) */
  /* application (procedure-name continuation) */
  /* operands (continuation) */
  OP_LOAD_EMPTY(21);
  /* operand continuation */
  /* local variable continuation */
  OP_LOAD_LOCAL(22, 0);
  OP_MAKE_PAIR(21, 22, 21);
  OP_STORE_ARGS(21, 1);
  /* free variable procedure-name */
  OP_LOAD_FREE(23, 5);
  OP_CALL(23, 10);
BLOCK_END()

BLOCK_BEGIN(8)
  /* application (display "backtrace ") */
  /* operands ("backtrace ") */
  OP_LOAD_EMPTY(16);
  /* operand "backtrace " */
  /* self-evaluating "backtrace " */
  OP_LOAD_CONSTANT(17, 0);
  OP_MAKE_PAIR(16, 17, 16);
  OP_STORE_ARGS(16, 1);
  /* free variable display */
  OP_LOAD_FREE(18, 4);
  OP_CALL(18, 9);
BLOCK_END()

BLOCK_BEGIN(7)
  OP_LOAD_RESULT(13);
  OP_BRANCH(8, 13);
  /* application (values) */
  /* operands () */
  OP_LOAD_EMPTY(14);
  OP_STORE_ARGS(14, 0);
  /* free variable values */
  OP_LOAD_FREE(15, 3);
  OP_LEAVE();
  OP_TAIL(15);
BLOCK_END()

BLOCK_BEGIN(6)
  OP_LOAD_RESULT(10);
  OP_MAKE_PAIR(6, 10, 6);
  /* operand continuation */
  /* local variable continuation */
  OP_LOAD_LOCAL(11, 0);
  OP_MAKE_PAIR(6, 11, 6);
  OP_STORE_ARGS(6, 2);
  /* free variable eq? */
  OP_LOAD_FREE(12, 2);
  OP_CALL(12, 7);
BLOCK_END()

BLOCK_BEGIN(4)
  OP_LOAD_RESULT(5);
  OP_BRANCH(5, 5);
  /* if (if (eq? continuation (procedure-continuation continuation)) (values) (begin (display "backtrace ") (display (procedure-name continuation)) (case (procedure-name continuation) ((eval define) (display " ") (write (procedure-local continuation 1)))) (newline) (backtrace (procedure-continuation continuation)))) */
  /* application (eq? continuation (procedure-continuation continuation)) */
  /* operands (continuation (procedure-continuation continuation)) */
  OP_LOAD_EMPTY(6);
  /* operand (procedure-continuation continuation) */
  /* application (procedure-continuation continuation) */
  /* operands (continuation) */
  OP_LOAD_EMPTY(7);
  /* operand continuation */
  /* local variable continuation */
  OP_LOAD_LOCAL(8, 0);
  OP_MAKE_PAIR(7, 8, 7);
  OP_STORE_ARGS(7, 1);
  /* free variable procedure-continuation */
  OP_LOAD_FREE(9, 1);
  OP_CALL(9, 6);
BLOCK_END()

BLOCK_BEGIN(3)
  /* parameters (continuation) */
  OP_LOAD_ARGS(0);
  /* parameter continuation */
  OP_LOAD_CAR(1, 0);
  OP_SET_LOCAL(0, 1);
  OP_LOAD_CDR(0, 0);
  /* if (if (procedure? continuation) (if (eq? continuation (procedure-continuation continuation)) (values) (begin (display "backtrace ") (display (procedure-name continuation)) (case (procedure-name continuation) ((eval define) (display " ") (write (...)))) (newline) (backtrace (procedure-continuation continuation)))) (error "backtrace of non-procedure")) */
  /* application (procedure? continuation) */
  /* operands (continuation) */
  OP_LOAD_EMPTY(2);
  /* operand continuation */
  /* local variable continuation */
  OP_LOAD_LOCAL(3, 0);
  OP_MAKE_PAIR(2, 3, 2);
  OP_STORE_ARGS(2, 1);
  /* free variable procedure? */
  OP_LOAD_FREE(4, 0);
  OP_CALL(4, 4);
BLOCK_END()

BLOCK_BEGIN(39)
  OP_ENTER(25, 1);
  /* make local bindings (read-eval-print) */
  OP_LOAD_UNBOUND(23);
  OP_LOAD_CONSTANT(24, 13);
  OP_MAKE_PAIR(22, 24, 23);
  OP_SET_LOCAL_BINDING(0, 22);
  OP_JUMP(23);
BLOCK_END()

BLOCK_BEGIN(23)
  OP_LOAD_ARGS(1);
  OP_LOAD_CAR(0, 1);
  /* definition (define (read-eval-print env) (display "SC> ") (define form (read)) (if (not (eof-object? form)) (begin (catch (lambda (exception continuation) (display "exception> ") (write exception) (newline) (backtrace continuation)) (call-with-values (lambda () (...)) (lambda results (...)))) (read-eval-print env)))) */
  /* lambda (lambda (env) (display "SC> ") (define form (read)) (if (not (eof-object? form)) (begin (catch (lambda (exception continuation) (display "exception> ") (write exception) (newline) (backtrace continuation)) (call-with-values (lambda () (...)) (lambda results (...)))) (read-eval-print env)))) */
  /* procedure constants ("SC> " 0 "=" "> " 1 results index loop "exception> " exception continuation env form) */
  OP_MAKE_VECTOR(2, 13);
  OP_LOAD_CONSTANT(3, 0);
  OP_VECTOR_SET(2, 0, 3);
  OP_LOAD_CONSTANT(4, 1);
  OP_VECTOR_SET(2, 1, 4);
  OP_LOAD_CONSTANT(5, 2);
  OP_VECTOR_SET(2, 2, 5);
  OP_LOAD_CONSTANT(6, 3);
  OP_VECTOR_SET(2, 3, 6);
  OP_LOAD_CONSTANT(7, 4);
  OP_VECTOR_SET(2, 4, 7);
  OP_LOAD_CONSTANT(8, 5);
  OP_VECTOR_SET(2, 5, 8);
  OP_LOAD_CONSTANT(9, 6);
  OP_VECTOR_SET(2, 6, 9);
  OP_LOAD_CONSTANT(10, 7);
  OP_VECTOR_SET(2, 7, 10);
  OP_LOAD_CONSTANT(11, 8);
  OP_VECTOR_SET(2, 8, 11);
  OP_LOAD_CONSTANT(12, 9);
  OP_VECTOR_SET(2, 9, 12);
  OP_LOAD_CONSTANT(13, 10);
  OP_VECTOR_SET(2, 10, 13);
  OP_LOAD_CONSTANT(14, 11);
  OP_VECTOR_SET(2, 11, 14);
  OP_LOAD_CONSTANT(15, 12);
  OP_VECTOR_SET(2, 12, 15);
  /* procedure free variables (display read eof-object? not null? car write newline + cdr eval call-with-values backtrace catch read-eval-print) */
  OP_MAKE_VECTOR(16, 15);
  /* procedure free variable display */
  OP_LOAD_FREE_BINDING(17, 0);
  OP_VECTOR_SET(16, 0, 17);
  /* procedure free variable read */
  OP_LOAD_FREE_BINDING(17, 1);
  OP_VECTOR_SET(16, 1, 17);
  /* procedure free variable eof-object? */
  OP_LOAD_FREE_BINDING(17, 2);
  OP_VECTOR_SET(16, 2, 17);
  /* procedure free variable not */
  OP_LOAD_FREE_BINDING(17, 3);
  OP_VECTOR_SET(16, 3, 17);
  /* procedure free variable null? */
  OP_LOAD_FREE_BINDING(17, 4);
  OP_VECTOR_SET(16, 4, 17);
  /* procedure free variable car */
  OP_LOAD_FREE_BINDING(17, 5);
  OP_VECTOR_SET(16, 5, 17);
  /* procedure free variable write */
  OP_LOAD_FREE_BINDING(17, 6);
  OP_VECTOR_SET(16, 6, 17);
  /* procedure free variable newline */
  OP_LOAD_FREE_BINDING(17, 7);
  OP_VECTOR_SET(16, 7, 17);
  /* procedure free variable + */
  OP_LOAD_FREE_BINDING(17, 8);
  OP_VECTOR_SET(16, 8, 17);
  /* procedure free variable cdr */
  OP_LOAD_FREE_BINDING(17, 9);
  OP_VECTOR_SET(16, 9, 17);
  /* procedure free variable eval */
  OP_LOAD_FREE_BINDING(17, 10);
  OP_VECTOR_SET(16, 10, 17);
  /* procedure free variable call-with-values */
  OP_LOAD_FREE_BINDING(17, 11);
  OP_VECTOR_SET(16, 11, 17);
  /* procedure free variable backtrace */
  OP_LOAD_FREE_BINDING(17, 12);
  OP_VECTOR_SET(16, 12, 17);
  /* procedure free variable catch */
  OP_LOAD_FREE_BINDING(17, 13);
  OP_VECTOR_SET(16, 13, 17);
  /* procedure free variable read-eval-print */
  OP_LOAD_FREE_BINDING(17, 14);
  OP_VECTOR_SET(16, 14, 17);
  OP_MAKE_PROC(18, 2, 16, 38);
  OP_SET_LOCAL(0, 18);
  /* binding symbols ((read-eval-print . 0)) */
  /* bind read-eval-print */
  OP_LOAD_CONSTANT(20, 13);
  OP_LOAD_LOCAL(19, 0);
  OP_BIND(0, 20, 19);
  /* values () */
  OP_LOAD_EMPTY(21);
  OP_STORE_ARGS(21, 0);
  OP_LEAVE();
  OP_RET();
BLOCK_END()

BLOCK_BEGIN(38)
  OP_ENTER(52, 2);
  /* make local bindings (env form) */
  OP_LOAD_UNBOUND(49);
  OP_LOAD_CONSTANT(50, 11);
  OP_MAKE_PAIR(48, 50, 49);
  OP_SET_LOCAL_BINDING(0, 48);
  OP_LOAD_CONSTANT(51, 12);
  OP_MAKE_PAIR(48, 51, 49);
  OP_SET_LOCAL_BINDING(1, 48);
  OP_JUMP(24);
BLOCK_END()

BLOCK_BEGIN(29)
  /* values () */
  OP_LOAD_EMPTY(47);
  OP_STORE_ARGS(47, 0);
  OP_LEAVE();
  OP_RET();
BLOCK_END()

BLOCK_BEGIN(37)
  OP_LOAD_RESULT(43);
  /* application (read-eval-print env) */
  /* operands (env) */
  OP_LOAD_EMPTY(44);
  /* operand env */
  /* local variable env */
  OP_LOAD_LOCAL(45, 0);
  OP_MAKE_PAIR(44, 45, 44);
  OP_STORE_ARGS(44, 1);
  /* free variable read-eval-print */
  OP_LOAD_FREE(46, 14);
  OP_LEAVE();
  OP_TAIL(46);
BLOCK_END()

BLOCK_BEGIN(31)
  OP_LOAD_RESULT(34);
  OP_MAKE_PAIR(16, 34, 16);
  /* operand (lambda (exception continuation) (display "exception> ") (write exception) (newline) (backtrace continuation)) */
  /* lambda (lambda (exception continuation) (display "exception> ") (write exception) (newline) (backtrace continuation)) */
  /* procedure constants ("exception> " exception continuation) */
  OP_MAKE_VECTOR(35, 3);
  OP_LOAD_CONSTANT(36, 8);
  OP_VECTOR_SET(35, 0, 36);
  OP_LOAD_CONSTANT(37, 9);
  OP_VECTOR_SET(35, 1, 37);
  OP_LOAD_CONSTANT(38, 10);
  OP_VECTOR_SET(35, 2, 38);
  /* procedure free variables (display write newline backtrace) */
  OP_MAKE_VECTOR(39, 4);
  /* procedure free variable display */
  OP_LOAD_FREE_BINDING(40, 0);
  OP_VECTOR_SET(39, 0, 40);
  /* procedure free variable write */
  OP_LOAD_FREE_BINDING(40, 6);
  OP_VECTOR_SET(39, 1, 40);
  /* procedure free variable newline */
  OP_LOAD_FREE_BINDING(40, 7);
  OP_VECTOR_SET(39, 2, 40);
  /* procedure free variable backtrace */
  OP_LOAD_FREE_BINDING(40, 12);
  OP_VECTOR_SET(39, 3, 40);
  OP_MAKE_PROC(41, 35, 39, 36);
  OP_MAKE_PAIR(16, 41, 16);
  OP_STORE_ARGS(16, 2);
  /* free variable catch */
  OP_LOAD_FREE(42, 13);
  OP_CALL(42, 37);
BLOCK_END()

BLOCK_BEGIN(28)
  OP_LOAD_RESULT(15);
  OP_BRANCH(29, 15);
  /* application (catch (lambda (exception continuation) (display "exception> ") (write exception) (newline) (backtrace continuation)) (call-with-values (lambda () (eval form env)) (lambda results (let loop ((results results) (index 0)) (if (not (...)) (begin (...) (...) (...) (...) (...) (...))))))) */
  /* operands ((lambda (exception continuation) (display "exception> ") (write exception) (newline) (backtrace continuation)) (call-with-values (lambda () (eval form env)) (lambda results (let loop ((results results) (index 0)) (if (not (...)) (begin (...) (...) (...) (...) (...) (...))))))) */
  OP_LOAD_EMPTY(16);
  /* operand (call-with-values (lambda () (eval form env)) (lambda results (let loop ((results results) (index 0)) (if (not (null? results)) (begin (display "=") (display index) (display "> ") (write (...)) (newline) (loop (...) (...))))))) */
  /* application (call-with-values (lambda () (eval form env)) (lambda results (let loop ((results results) (index 0)) (if (not (null? results)) (begin (display "=") (display index) (display "> ") (write (...)) (newline) (loop (...) (...))))))) */
  /* operands ((lambda () (eval form env)) (lambda results (let loop ((results results) (index 0)) (if (not (null? results)) (begin (display "=") (display index) (display "> ") (write (...)) (newline) (loop (...) (...))))))) */
  OP_LOAD_EMPTY(17);
  /* operand (lambda results (let loop ((results results) (index 0)) (if (not (null? results)) (begin (display "=") (display index) (display "> ") (write (car results)) (newline) (loop (cdr results) (+ index 1)))))) */
  /* lambda (lambda results (let loop ((results results) (index 0)) (if (not (null? results)) (begin (display "=") (display index) (display "> ") (write (car results)) (newline) (loop (cdr results) (+ index 1)))))) */
  /* procedure constants (0 "=" "> " 1 results index loop) */
  OP_MAKE_VECTOR(18, 7);
  OP_LOAD_CONSTANT(19, 1);
  OP_VECTOR_SET(18, 0, 19);
  OP_LOAD_CONSTANT(20, 2);
  OP_VECTOR_SET(18, 1, 20);
  OP_LOAD_CONSTANT(21, 3);
  OP_VECTOR_SET(18, 2, 21);
  OP_LOAD_CONSTANT(22, 4);
  OP_VECTOR_SET(18, 3, 22);
  OP_LOAD_CONSTANT(23, 5);
  OP_VECTOR_SET(18, 4, 23);
  OP_LOAD_CONSTANT(24, 6);
  OP_VECTOR_SET(18, 5, 24);
  OP_LOAD_CONSTANT(25, 7);
  OP_VECTOR_SET(18, 6, 25);
  /* procedure free variables (null? not display car write newline + cdr) */
  OP_MAKE_VECTOR(26, 8);
  /* procedure free variable null? */
  OP_LOAD_FREE_BINDING(27, 4);
  OP_VECTOR_SET(26, 0, 27);
  /* procedure free variable not */
  OP_LOAD_FREE_BINDING(27, 3);
  OP_VECTOR_SET(26, 1, 27);
  /* procedure free variable display */
  OP_LOAD_FREE_BINDING(27, 0);
  OP_VECTOR_SET(26, 2, 27);
  /* procedure free variable car */
  OP_LOAD_FREE_BINDING(27, 5);
  OP_VECTOR_SET(26, 3, 27);
  /* procedure free variable write */
  OP_LOAD_FREE_BINDING(27, 6);
  OP_VECTOR_SET(26, 4, 27);
  /* procedure free variable newline */
  OP_LOAD_FREE_BINDING(27, 7);
  OP_VECTOR_SET(26, 5, 27);
  /* procedure free variable + */
  OP_LOAD_FREE_BINDING(27, 8);
  OP_VECTOR_SET(26, 6, 27);
  /* procedure free variable cdr */
  OP_LOAD_FREE_BINDING(27, 9);
  OP_VECTOR_SET(26, 7, 27);
  OP_MAKE_PROC(28, 18, 26, 43);
  OP_MAKE_PAIR(17, 28, 17);
  /* operand (lambda () (eval form env)) */
  /* lambda (lambda () (eval form env)) */
  /* procedure constants () */
  OP_MAKE_VECTOR(29, 0);
  /* procedure free variables (env form eval) */
  OP_MAKE_VECTOR(30, 3);
  /* procedure free variable env */
  OP_LOAD_LOCAL_BINDING(31, 0);
  OP_VECTOR_SET(30, 0, 31);
  /* procedure free variable form */
  OP_LOAD_LOCAL_BINDING(31, 1);
  OP_VECTOR_SET(30, 1, 31);
  /* procedure free variable eval */
  OP_LOAD_FREE_BINDING(31, 10);
  OP_VECTOR_SET(30, 2, 31);
  OP_MAKE_PROC(32, 29, 30, 30);
  OP_MAKE_PAIR(17, 32, 17);
  OP_STORE_ARGS(17, 2);
  /* free variable call-with-values */
  OP_LOAD_FREE(33, 11);
  OP_CALL(33, 31);
BLOCK_END()

BLOCK_BEGIN(27)
  OP_LOAD_RESULT(13);
  OP_MAKE_PAIR(9, 13, 9);
  OP_STORE_ARGS(9, 1);
  /* free variable not */
  OP_LOAD_FREE(14, 3);
  OP_CALL(14, 28);
BLOCK_END()

BLOCK_BEGIN(26)
  OP_LOAD_RESULT(8);
  OP_SET_LOCAL(1, 8);
  /* if (if (not (eof-object? form)) (begin (catch (lambda (exception continuation) (display "exception> ") (write exception) (newline) (backtrace continuation)) (call-with-values (lambda () (eval form env)) (lambda results (let loop (...) (...))))) (read-eval-print env))) */
  /* application (not (eof-object? form)) */
  /* operands ((eof-object? form)) */
  OP_LOAD_EMPTY(9);
  /* operand (eof-object? form) */
  /* application (eof-object? form) */
  /* operands (form) */
  OP_LOAD_EMPTY(10);
  /* operand form */
  /* local variable form */
  OP_LOAD_LOCAL(11, 1);
  OP_MAKE_PAIR(10, 11, 10);
  OP_STORE_ARGS(10, 1);
  /* free variable eof-object? */
  OP_LOAD_FREE(12, 2);
  OP_CALL(12, 27);
BLOCK_END()

BLOCK_BEGIN(25)
  OP_LOAD_RESULT(5);
  /* definition (define form (read)) */
  /* application (read) */
  /* operands () */
  OP_LOAD_EMPTY(6);
  OP_STORE_ARGS(6, 0);
  /* free variable read */
  OP_LOAD_FREE(7, 1);
  OP_CALL(7, 26);
BLOCK_END()

BLOCK_BEGIN(24)
  /* parameters (env) */
  OP_LOAD_ARGS(0);
  /* parameter env */
  OP_LOAD_CAR(1, 0);
  OP_SET_LOCAL(0, 1);
  OP_LOAD_CDR(0, 0);
  /* application (display "SC> ") */
  /* operands ("SC> ") */
  OP_LOAD_EMPTY(2);
  /* operand "SC> " */
  /* self-evaluating "SC> " */
  OP_LOAD_CONSTANT(3, 0);
  OP_MAKE_PAIR(2, 3, 2);
  OP_STORE_ARGS(2, 1);
  /* free variable display */
  OP_LOAD_FREE(4, 0);
  OP_CALL(4, 25);
BLOCK_END()

BLOCK_BEGIN(43)
  OP_ENTER(19, 2);
  /* make local bindings (results loop) */
  OP_LOAD_UNBOUND(16);
  OP_LOAD_CONSTANT(17, 4);
  OP_MAKE_PAIR(15, 17, 16);
  OP_SET_LOCAL_BINDING(0, 15);
  OP_LOAD_CONSTANT(18, 6);
  OP_MAKE_PAIR(15, 18, 16);
  OP_SET_LOCAL_BINDING(1, 15);
  OP_JUMP(29);
BLOCK_END()

BLOCK_BEGIN(29)
  /* parameters results */
  OP_LOAD_ARGS(0);
  OP_SET_LOCAL(0, 0);
  /* application ((letrec ((loop (lambda (results index) (if (...) (...))))) loop) results 0) */
  /* operands (results 0) */
  OP_LOAD_EMPTY(2);
  /* operand 0 */
  /* self-evaluating 0 */
  OP_LOAD_CONSTANT(3, 0);
  OP_MAKE_PAIR(2, 3, 2);
  /* operand results */
  /* local variable results */
  OP_LOAD_LOCAL(4, 0);
  OP_MAKE_PAIR(2, 4, 2);
  OP_STORE_ARGS(2, 2);
  /* let* (letrec ((loop (lambda (results index) (if (not (...)) (begin (...) (...) (...) (...) (...) (...)))))) loop) */
  /* lambda (lambda (results index) (if (not (null? results)) (begin (display "=") (display index) (display "> ") (write (car results)) (newline) (loop (cdr results) (+ index 1))))) */
  /* procedure constants ("=" "> " 1 results index) */
  OP_MAKE_VECTOR(5, 5);
  OP_LOAD_CONSTANT(6, 1);
  OP_VECTOR_SET(5, 0, 6);
  OP_LOAD_CONSTANT(7, 2);
  OP_VECTOR_SET(5, 1, 7);
  OP_LOAD_CONSTANT(8, 3);
  OP_VECTOR_SET(5, 2, 8);
  OP_LOAD_CONSTANT(9, 4);
  OP_VECTOR_SET(5, 3, 9);
  OP_LOAD_CONSTANT(10, 5);
  OP_VECTOR_SET(5, 4, 10);
  /* procedure free variables (null? not display car write newline + cdr loop) */
  OP_MAKE_VECTOR(11, 9);
  /* procedure free variable null? */
  OP_LOAD_FREE_BINDING(12, 0);
  OP_VECTOR_SET(11, 0, 12);
  /* procedure free variable not */
  OP_LOAD_FREE_BINDING(12, 1);
  OP_VECTOR_SET(11, 1, 12);
  /* procedure free variable display */
  OP_LOAD_FREE_BINDING(12, 2);
  OP_VECTOR_SET(11, 2, 12);
  /* procedure free variable car */
  OP_LOAD_FREE_BINDING(12, 3);
  OP_VECTOR_SET(11, 3, 12);
  /* procedure free variable write */
  OP_LOAD_FREE_BINDING(12, 4);
  OP_VECTOR_SET(11, 4, 12);
  /* procedure free variable newline */
  OP_LOAD_FREE_BINDING(12, 5);
  OP_VECTOR_SET(11, 5, 12);
  /* procedure free variable + */
  OP_LOAD_FREE_BINDING(12, 6);
  OP_VECTOR_SET(11, 6, 12);
  /* procedure free variable cdr */
  OP_LOAD_FREE_BINDING(12, 7);
  OP_VECTOR_SET(11, 7, 12);
  /* procedure free variable loop */
  OP_LOAD_LOCAL_BINDING(12, 1);
  OP_VECTOR_SET(11, 8, 12);
  OP_MAKE_PROC(13, 5, 11, 42);
  OP_SET_LOCAL(1, 13);
  /* local variable loop */
  OP_LOAD_LOCAL(14, 1);
  OP_LEAVE();
  OP_TAIL(14);
BLOCK_END()

BLOCK_BEGIN(42)
  OP_ENTER(47, 2);
  /* make local bindings (results index) */
  OP_LOAD_UNBOUND(44);
  OP_LOAD_CONSTANT(45, 3);
  OP_MAKE_PAIR(43, 45, 44);
  OP_SET_LOCAL_BINDING(0, 43);
  OP_LOAD_CONSTANT(46, 4);
  OP_MAKE_PAIR(43, 46, 44);
  OP_SET_LOCAL_BINDING(1, 43);
  OP_JUMP(30);
BLOCK_END()

BLOCK_BEGIN(33)
  /* values () */
  OP_LOAD_EMPTY(42);
  OP_STORE_ARGS(42, 0);
  OP_LEAVE();
  OP_RET();
BLOCK_END()

BLOCK_BEGIN(41)
  OP_LOAD_RESULT(40);
  OP_MAKE_PAIR(31, 40, 31);
  OP_STORE_ARGS(31, 2);
  /* free variable loop */
  OP_LOAD_FREE(41, 8);
  OP_LEAVE();
  OP_TAIL(41);
BLOCK_END()

BLOCK_BEGIN(40)
  OP_LOAD_RESULT(36);
  OP_MAKE_PAIR(31, 36, 31);
  /* operand (cdr results) */
  /* application (cdr results) */
  /* operands (results) */
  OP_LOAD_EMPTY(37);
  /* operand results */
  /* local variable results */
  OP_LOAD_LOCAL(38, 0);
  OP_MAKE_PAIR(37, 38, 37);
  OP_STORE_ARGS(37, 1);
  /* free variable cdr */
  OP_LOAD_FREE(39, 7);
  OP_CALL(39, 41);
BLOCK_END()

BLOCK_BEGIN(39)
  OP_LOAD_RESULT(30);
  /* application (loop (cdr results) (+ index 1)) */
  /* operands ((cdr results) (+ index 1)) */
  OP_LOAD_EMPTY(31);
  /* operand (+ index 1) */
  /* application (+ index 1) */
  /* operands (index 1) */
  OP_LOAD_EMPTY(32);
  /* operand 1 */
  /* self-evaluating 1 */
  OP_LOAD_CONSTANT(33, 2);
  OP_MAKE_PAIR(32, 33, 32);
  /* operand index */
  /* local variable index */
  OP_LOAD_LOCAL(34, 1);
  OP_MAKE_PAIR(32, 34, 32);
  OP_STORE_ARGS(32, 2);
  /* free variable + */
  OP_LOAD_FREE(35, 6);
  OP_CALL(35, 40);
BLOCK_END()

BLOCK_BEGIN(38)
  OP_LOAD_RESULT(27);
  /* application (newline) */
  /* operands () */
  OP_LOAD_EMPTY(28);
  OP_STORE_ARGS(28, 0);
  /* free variable newline */
  OP_LOAD_FREE(29, 5);
  OP_CALL(29, 39);
BLOCK_END()

BLOCK_BEGIN(37)
  OP_LOAD_RESULT(25);
  OP_MAKE_PAIR(21, 25, 21);
  OP_STORE_ARGS(21, 1);
  /* free variable write */
  OP_LOAD_FREE(26, 4);
  OP_CALL(26, 38);
BLOCK_END()

BLOCK_BEGIN(36)
  OP_LOAD_RESULT(20);
  /* application (write (car results)) */
  /* operands ((car results)) */
  OP_LOAD_EMPTY(21);
  /* operand (car results) */
  /* application (car results) */
  /* operands (results) */
  OP_LOAD_EMPTY(22);
  /* operand results */
  /* local variable results */
  OP_LOAD_LOCAL(23, 0);
  OP_MAKE_PAIR(22, 23, 22);
  OP_STORE_ARGS(22, 1);
  /* free variable car */
  OP_LOAD_FREE(24, 3);
  OP_CALL(24, 37);
BLOCK_END()

BLOCK_BEGIN(35)
  OP_LOAD_RESULT(16);
  /* application (display "> ") */
  /* operands ("> ") */
  OP_LOAD_EMPTY(17);
  /* operand "> " */
  /* self-evaluating "> " */
  OP_LOAD_CONSTANT(18, 1);
  OP_MAKE_PAIR(17, 18, 17);
  OP_STORE_ARGS(17, 1);
  /* free variable display */
  OP_LOAD_FREE(19, 2);
  OP_CALL(19, 36);
BLOCK_END()

BLOCK_BEGIN(34)
  OP_LOAD_RESULT(12);
  /* application (display index) */
  /* operands (index) */
  OP_LOAD_EMPTY(13);
  /* operand index */
  /* local variable index */
  OP_LOAD_LOCAL(14, 1);
  OP_MAKE_PAIR(13, 14, 13);
  OP_STORE_ARGS(13, 1);
  /* free variable display */
  OP_LOAD_FREE(15, 2);
  OP_CALL(15, 35);
BLOCK_END()

BLOCK_BEGIN(32)
  OP_LOAD_RESULT(8);
  OP_BRANCH(33, 8);
  /* application (display "=") */
  /* operands ("=") */
  OP_LOAD_EMPTY(9);
  /* operand "=" */
  /* self-evaluating "=" */
  OP_LOAD_CONSTANT(10, 0);
  OP_MAKE_PAIR(9, 10, 9);
  OP_STORE_ARGS(9, 1);
  /* free variable display */
  OP_LOAD_FREE(11, 2);
  OP_CALL(11, 34);
BLOCK_END()

BLOCK_BEGIN(31)
  OP_LOAD_RESULT(6);
  OP_MAKE_PAIR(2, 6, 2);
  OP_STORE_ARGS(2, 1);
  /* free variable not */
  OP_LOAD_FREE(7, 1);
  OP_CALL(7, 32);
BLOCK_END()

BLOCK_BEGIN(30)
  /* parameters (results index) */
  OP_LOAD_ARGS(0);
  /* parameter results */
  OP_LOAD_CAR(1, 0);
  OP_SET_LOCAL(0, 1);
  OP_LOAD_CDR(0, 0);
  /* parameter index */
  OP_LOAD_CAR(1, 0);
  OP_SET_LOCAL(1, 1);
  OP_LOAD_CDR(0, 0);
  /* if (if (not (null? results)) (begin (display "=") (display index) (display "> ") (write (car results)) (newline) (loop (cdr results) (+ index 1)))) */
  /* application (not (null? results)) */
  /* operands ((null? results)) */
  OP_LOAD_EMPTY(2);
  /* operand (null? results) */
  /* application (null? results) */
  /* operands (results) */
  OP_LOAD_EMPTY(3);
  /* operand results */
  /* local variable results */
  OP_LOAD_LOCAL(4, 0);
  OP_MAKE_PAIR(3, 4, 3);
  OP_STORE_ARGS(3, 1);
  /* free variable null? */
  OP_LOAD_FREE(5, 0);
  OP_CALL(5, 31);
BLOCK_END()

BLOCK_BEGIN(30)
  OP_ENTER(6, 0);
  OP_JUMP(29);
BLOCK_END()

BLOCK_BEGIN(29)
  /* parameters () */
  OP_LOAD_ARGS(0);
  /* application (eval form env) */
  /* operands (form env) */
  OP_LOAD_EMPTY(2);
  /* operand env */
  /* free variable env */
  OP_LOAD_FREE(3, 0);
  OP_MAKE_PAIR(2, 3, 2);
  /* operand form */
  /* free variable form */
  OP_LOAD_FREE(4, 1);
  OP_MAKE_PAIR(2, 4, 2);
  OP_STORE_ARGS(2, 2);
  /* free variable eval */
  OP_LOAD_FREE(5, 2);
  OP_LEAVE();
  OP_TAIL(5);
BLOCK_END()

BLOCK_BEGIN(36)
  OP_ENTER(20, 2);
  /* make local bindings (exception continuation) */
  OP_LOAD_UNBOUND(17);
  OP_LOAD_CONSTANT(18, 1);
  OP_MAKE_PAIR(16, 18, 17);
  OP_SET_LOCAL_BINDING(0, 16);
  OP_LOAD_CONSTANT(19, 2);
  OP_MAKE_PAIR(16, 19, 17);
  OP_SET_LOCAL_BINDING(1, 16);
  OP_JUMP(32);
BLOCK_END()

BLOCK_BEGIN(35)
  OP_LOAD_RESULT(12);
  /* application (backtrace continuation) */
  /* operands (continuation) */
  OP_LOAD_EMPTY(13);
  /* operand continuation */
  /* local variable continuation */
  OP_LOAD_LOCAL(14, 1);
  OP_MAKE_PAIR(13, 14, 13);
  OP_STORE_ARGS(13, 1);
  /* free variable backtrace */
  OP_LOAD_FREE(15, 3);
  OP_LEAVE();
  OP_TAIL(15);
BLOCK_END()

BLOCK_BEGIN(34)
  OP_LOAD_RESULT(9);
  /* application (newline) */
  /* operands () */
  OP_LOAD_EMPTY(10);
  OP_STORE_ARGS(10, 0);
  /* free variable newline */
  OP_LOAD_FREE(11, 2);
  OP_CALL(11, 35);
BLOCK_END()

BLOCK_BEGIN(33)
  OP_LOAD_RESULT(5);
  /* application (write exception) */
  /* operands (exception) */
  OP_LOAD_EMPTY(6);
  /* operand exception */
  /* local variable exception */
  OP_LOAD_LOCAL(7, 0);
  OP_MAKE_PAIR(6, 7, 6);
  OP_STORE_ARGS(6, 1);
  /* free variable write */
  OP_LOAD_FREE(8, 1);
  OP_CALL(8, 34);
BLOCK_END()

BLOCK_BEGIN(32)
  /* parameters (exception continuation) */
  OP_LOAD_ARGS(0);
  /* parameter exception */
  OP_LOAD_CAR(1, 0);
  OP_SET_LOCAL(0, 1);
  OP_LOAD_CDR(0, 0);
  /* parameter continuation */
  OP_LOAD_CAR(1, 0);
  OP_SET_LOCAL(1, 1);
  OP_LOAD_CDR(0, 0);
  /* application (display "exception> ") */
  /* operands ("exception> ") */
  OP_LOAD_EMPTY(2);
  /* operand "exception> " */
  /* self-evaluating "exception> " */
  OP_LOAD_CONSTANT(3, 0);
  OP_MAKE_PAIR(2, 3, 2);
  OP_STORE_ARGS(2, 1);
  /* free variable display */
  OP_LOAD_FREE(4, 0);
  OP_CALL(4, 33);
BLOCK_END()

