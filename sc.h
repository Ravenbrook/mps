/* sc.h -- interface to SC language abstract machine and interpreter
 *
 * Richard Brooksby, 2004-07-28
 *
 * $Id$
 *
 *
 * LICENCE
 *
 * Copyright (C) 1997-2004 Richard Brooksby.  This document is provided
 * "as is", without any express or implied warranty.  In no event will
 * the authors be held liable for any damages arising from the use of
 * this document.  You may not duplicate or reproduce this document in
 * any form without the express permission of the copyright holder.
 *
 * REFERENCES
 *
 * [R5RS]	"Revised(5) Report on the Algorithmic Language Scheme";
 *		Richard Kelsey, William Clinger, Jonathan Rees (Eds).
 *
 * [C90]	"American National Standard for Programming Languages -- C";
 *		ANSI/ISO 9899:1990.
 *
 * [C99]	"Internation Standard for Programming Languages -- C";
 *		ISO/IEC 9899:1999(E).
 *
 * NOTES
 *
 * [1] The length cannot exceed the size of an integer, otherwise it'll be
 * unreferenceable.
 *
 * [2] This is the maximum number of local registers used by any procedure
 * in the interpreter (actually, in "eval").
 */

#ifndef sc_h
#define sc_h


#include <stddef.h>
#include <stdio.h>
#include <setjmp.h>
#include <string.h>


/* OPTIONS */

/* #define NDEBUG		   disable assertions */
#define GC_CHECKING		/* check the heap during GC */
/* #define GC_STATS		   print statistics during GC */
#define FIELD_CHECKING		/* check types in field selectors */
#define ALLOC_CHECKING		/* check for nested allocation */
/* #define WRITE_PROC		   write procedure details */
#define REP_BACKTRACE		/* print backtrace on error */
/* #define AGE_STATS		   collect age statistics on heap contents */
#define COUNT_STATS		/* count operations */


/* CONSTANTS */


#define ERRMAX		((size_t)255)		/* max length of error message */
#define SYMMAX		((size_t)255)		/* max length of a symbol when reading */
#define STATICSYMMAX	((size_t)17)		/* max length of a static symbol */
#define STRMAX		((size_t)255)		/* max length of a string when reading */
#define CHARNAMEMAX	((size_t)16)		/* max length of a character name */
#define TEMPS		((size_t)3)		/* number of temporary registers */
#define HEAP		((size_t)1uL<<24)	/* initial heap size */
#define SYMTAB		128			/* initial symbol table size */
#define ALIGN		8			/* minimum alignment for objects */
#define LJ_ERROR	1			/* longjump code for thrown error */
#define LJ_RETURN	2			/* longjump code for return */
#define LJ_FATAL	3			/* longjump code for uncaught error */
#define WRITE_DEPTH	6			/* default depth argument to write_obj */

#define ALIGN_UP(n)	(((n) + ALIGN - 1) & ~(ALIGN - 1))
#define ARRAYLEN(array) (sizeof(array) / sizeof((array)[0]))


/* LANGUAGE EXTENSIONS */


#define unless(c)	if(!(c))
#define until(c)	while(!(c))
#define repeat		for(;;)
#define BEGIN		do {
#define END		} while(0)
#define NOOP		BEGIN END


/* GNU C EXTENSIONS
 *
 * I mostly use these to supress warnings, and don't rely on them in any way.
 */

#ifndef __GNUC__
#define __attribute__(attrs)
#endif


/* DATA TYPES */

typedef unsigned long ulong;

typedef struct mms_s *mms_t;            /* memory management state */
typedef struct state_s *state_t;	/* abstract machine state */
typedef union obj_u *obj_t;		/* Scheme object type */
typedef unsigned type_t;		/* run-time Scheme type */
typedef void (*label_t)(state_t);	/* abstract machine pc type */
typedef long int_t;                     /* Scheme integer representation */
#define INT_T_MAX       LONG_MAX
typedef int_t veclen_t;                 /* vector length or index rep. [1] */
#define VECLEN_T_MAX    INT_T_MAX
typedef int_t strlen_t;                 /* string length or index rep. [1] */
#define STRLEN_T_MAX    INT_T_MAX

typedef struct header_s {
  type_t type;
#ifdef AGE_STATS
  ulong birthday;
#endif
} header_s;

#define TYPE_PROC	((type_t)0x21BEB60C)

typedef struct proc_s {
  header_s header;		/* type = TYPE_PROC */
  obj_t name;			/* printed name, a symbol; see "write_obj" */
  label_t entry;		/* entry point */
  obj_t cont;			/* continuation */
  int read_only;		/* duplicate on return? */
  size_t regs;                  /* number of local registers */
  obj_t locs[1];		/* local registers */
} proc_s;

#define TYPE_SYMBOL	((type_t)0x21BE513B)

typedef struct symbol_s {
  header_s header;		/* type = TYPE_SYMBOL */
  size_t length;		/* length of symbol string (excl. NUL) */
  char string[STATICSYMMAX+1];	/* symbol string, NUL terminated */
} symbol_s;

#define TYPE_PAIR	((type_t)0x21BEBA16)

typedef struct pair_s {
  header_s header;		/* type = TYPE_PAIR */
  obj_t car, cdr;
} pair_s;

#define TYPE_INTEGER	((type_t)0x21BE142E)

typedef struct integer_s {
  header_s header;		/* type = TYPE_INTEGER */
  int_t integer;		/* the integer */
} integer_s;

#define TYPE_SPECIAL	((type_t)0x21BE5BEC)

typedef struct special_s {
  header_s header;		/* type = TYPE_SPECIAL */
  const char *name;		/* printed representation, NUL terminated */
} special_s;

#define TYPE_INPORT	((type_t)0x21BE14B0)

#define TYPE_OUTPORT	((type_t)0x21BE002B)

typedef struct port_s {
  header_s header;		/* type = TYPE_INPORT or TYPE_OUTPORT */
  FILE *stream;
} port_s;

#define TYPE_STRING	((type_t)0x21BE5261)

typedef struct string_s {
  header_s header;		/* type = TYPE_STRING */
  strlen_t length;		/* number of chars in string [1] */
  char string[1];		/* string, NUL terminated */
} string_s;

#define TYPE_VECTOR	((type_t)0x21BEFEC2)

typedef struct vector_s {
  header_s header;		/* type = TYPE_VECTOR */
  veclen_t length;		/* number of elements [1] */
  obj_t elements[1];		/* the elements */
} vector_s;

#define TYPE_EXCEPTION	((type_t)0x21BEE8CE)

typedef struct exception_s {
  header_s header;		/* type = TYPE_EXCEPTION */
  obj_t object;			/* exception object */
} exception_s;

#define TYPE_CHARACTER	((type_t)0x21BEC8A6)

typedef struct character_s {
  header_s header;		/* type = TYPE_CHARACTER */
  char c;			/* the character */
} character_s;

#define TYPE_FORWARD	((type_t)0x21BEF063)

typedef struct forward_s {
  header_s header;		/* type = TYPE_FORWARD */
  obj_t object;			/* new copy of object */
} forward_s;

typedef union obj_u {
  header_s header;
  proc_s proc;
  pair_s pair;
  symbol_s symbol;
  integer_s integer;
  special_s special;
  port_s port;
  string_s string;
  vector_s vector;
  exception_s exception;
  character_s character;
  forward_s forward;
} obj_u;


/* COUNTING STATISTICS
 *
 * If enabled, the abstract machine counts the number of times certain operations
 * are executed.  This can help a great deal with optimization and debugging.
 * The counters are stored in state->count.
 */

#ifdef COUNT_STATS

enum {
  COUNT_GC_GC,			/* garbage collections */
  COUNT_GC_ALLOC,		/* allocations, including GC copies */
  COUNT_GC_COPY,		/* GC copies */
  COUNT_GC_SNAP,		/* GC snap-outs */
  COUNT_MAKE_PAIR,		/* pairs created */
  COUNT_MAKE_INTEGER,		/* integers created */
  COUNT_MAKE_PROC,		/* etc. */
  COUNT_DUP_PROC,
  COUNT_MAKE_VECTOR,
  COUNT_MAKE_SYMBOL,
  COUNT_MAKE_STRING,
  COUNT_MAKE_SPECIAL,
  COUNT_MAKE_INPORT,
  COUNT_MAKE_OUTPORT,
  COUNT_MAKE_EXCEPTION,
  COUNT_MAKE_CHARACTER,
  COUNT_AM_ENTER,		/* abstract machine ENTER operations */
  COUNT_AM_JUMP,		/* JUMPs */
  COUNT_AM_CALL,		/* etc. */
  COUNT_AM_TAIL,
  COUNT_AM_RET_DUP,
  COUNT_AM_RET,
  COUNT_ENV_LOOKUP,		/* symbol lookups in environment */
  COUNT_ENV_LOOKUP_IN_FRAME,
  COUNT_ENV_LOOKUP_SCAN,
  COUNT_MAX			/* number of counters */
};

#define COUNT(counter) \
  BEGIN \
    ++state->count[COUNT_ ## counter]; \
  END

#else /* COUNT_STATS not */

#define COUNT(counter) NOOP

#endif /* COUNT_STATS */


/* MACHINE STATE */

typedef struct state_s {

  mms_t mms;            /* memory management state */
  obj_t baby;           /* newly allocated object */

  /* Debugging Data */

  int trace;		/* print tracing information? */
  int heap_checking;	/* heap checking in driver loop? */
  ulong step;		/* step count (see machine loop) */
  int inited;		/* have the fields in the state been initialized? */

  /* Procedures
   *
   * These fields are initialized to point to the binding pairs
   * of procedures used by the interpreter.  The binding pairs, rather
   * than the procedures, are used so that the procedures can be
   * re-bound.
   *
   * @@@@ In compiled code, references to procedures needed by a
   * procedure would be in the procedure's closure, rather than tucked
   * away in a special place like this.	 It might be better to simulate
   * that.
   *
   * IMPORTANT: The "proc_*" macros must appear in exactly the same order
   * as the entries in the "global_procs" table in "sc.c".
   */

  obj_t procs[10];
#define proc_rep		CDR(state->procs[0])
#define proc_read		CDR(state->procs[1])
#define proc_eval		CDR(state->procs[2])
#define proc_write		CDR(state->procs[3])
#define proc_list_to_vec	CDR(state->procs[4])
#define proc_string_to_sym	CDR(state->procs[5])
#define proc_open_in		CDR(state->procs[6])
#define proc_length		CDR(state->procs[7])
#define proc_close_in		CDR(state->procs[8])
#define proc_throw		CDR(state->procs[9])

  /* Abstract Machine State
   *
   * The most important piece of state is "here", the current procedure.
   * To put the state in traditional machine terms, "here" is a
   * combination of the registers and top stack frame.	The "here->proc.entry"
   * is the PC of the abstract machine, "here->proc.cont" is the next "stack
   * frame", etc.
   *
   * The only state that isn't part of "here" is the argument list, which
   * is exactly the information that travels between functions.
   */

  obj_t here;			/* current procedure */
  obj_t cont;			/* caller procedure (continuation) */
  obj_t argl;			/* argument list */
  ulong args;			/* argument count */
  obj_t temp[TEMPS];		/* temporary registers (caller save) */
  jmp_buf driver_loop;		/* driver loop restart */
  obj_t errproc;		/* error continuation */
  obj_t inport;			/* current input port */
  obj_t outport;		/* current output port */
  obj_t errport;		/* current error port */

  /* symtab -- symbol table
   *
   * The symbol table is a hash-table containing objects of TYPE_SYMBOL.
   * When a string is "interned" it is looked up in the table, and added
   * only if it is not there.  This guarantees that all symbols which
   * are equal are actually the same object.
   */

  obj_t obj_symtab;		/* symbol table */

  obj_t units;                  /* loaded units */

  /* Count statistics */
#ifdef COUNT_STATS
  ulong count[COUNT_MAX];
#endif

} state_s;


/* structure macros */

#define TYPE(obj)	((obj)->header.type)
#define ISNTTYPE(type)	(((type) & 0xFFFF0000) != 0x21BE0000)

#ifdef FIELD_CHECKING

#define FIELD(obj, type, field) \
  ((obj)->field)

/*
#define FIELD(obj, type, field) \
  (AVER(TYPE(obj) == (type)), (obj)->field)
 */

#define SETFIELD(obj, type, field, value) \
  BEGIN \
    ASSERT(TYPE(obj) == (type)); \
    (obj)->field = (value); \
  END

#else /* FIELD_CHECKING not */

#define FIELD(obj, type, field) \
  ((obj)->field)

#define SETFIELD(obj, type, field, value) \
  BEGIN \
    (obj)->field = (value); \
  END

#endif /* FIELD_CHECKING */

#define CAR(obj)	FIELD(obj, TYPE_PAIR, pair.car)
#define CDR(obj)	FIELD(obj, TYPE_PAIR, pair.cdr)
#define CAAR(obj)	CAR(CAR(obj))
#define CADR(obj)	CAR(CDR(obj))
#define CDAR(obj)	CDR(CAR(obj))
#define CDDR(obj)	CDR(CDR(obj))
#define CADDR(obj)	CAR(CDDR(obj))
#define CDDDR(obj)	CDR(CDDR(obj))
#define CADDDR(obj)	CAR(CDDDR(obj))
#define SETCAR(obj, v)	SETFIELD(obj, TYPE_PAIR, pair.car, v)
#define SETCDR(obj, v)	SETFIELD(obj, TYPE_PAIR, pair.cdr, v)
#define INT(obj)	FIELD(obj, TYPE_INTEGER, integer.integer)
#define SETINT(obj, i)	SETFIELD(obj, TYPE_INTEGER, integer.integer, i)
#define VECLEN(obj)	FIELD(obj, TYPE_VECTOR, vector.length)
#define INPORTSTREAM(obj)	FIELD(obj, TYPE_INPORT, port.stream)
#define OUTPORTSTREAM(obj)	FIELD(obj, TYPE_OUTPORT, port.stream)
#define STR(obj)	FIELD(obj, TYPE_STRING, string.string)
#define STRLEN(obj)	FIELD(obj, TYPE_STRING, string.length)
#define SYMSTR(obj)	FIELD(obj, TYPE_SYMBOL, symbol.string)
#define SYMLEN(obj)	FIELD(obj, TYPE_SYMBOL, symbol.length)
#define VECREF(obj, i) \
  (AVER(TYPE(obj) == TYPE_VECTOR), \
   AVER(0 <= (i) && (i) < VECLEN(obj)), \
   (obj)->vector.elements[i])
#define VECSET(obj, i, v) \
  BEGIN \
    ASSERT(TYPE(obj) == TYPE_VECTOR); \
    ASSERT(0 <= (i)); \
    ASSERT((i) < VECLEN(obj)); \
    (obj)->vector.elements[i] = (v); \
  END
#define LOC(obj, n)	FIELD(obj, TYPE_PROC, proc.locs[n])


/* ASSERTION HANDLER
 *
 * The handler provided by "assert.h" isn't used because it's often
 * hard to put a breakpoint inside it from the debugger.  When debugging,
 * put a breakpoint on "assert_fail".
 *
 * Note that we're careful to put the condition straight into an "if"
 * statement so that we benefit from any compiler warnings (unintended
 * assignments -- deadly in assertions, etc.).
 */

#define UNUSED(var)             BEGIN (void)var; END

#ifdef NDEBUG

#define ASSERT(condition)	NOOP
#define AVER(condition)		((void)0)

#else /* NDEBUG, not */

#define ASSERT(condition) \
  BEGIN \
    if(condition) \
      NOOP; \
    else \
      assert_fail(#condition, __FILE__, __LINE__); \
  END

#define AVER(condition) \
  ((condition) ? \
   (void)0 : \
   assert_fail(#condition, __FILE__, __LINE__))

extern void assert_fail(const char *, const char *, ulong) __attribute((noreturn));

#endif /* NDEBUG, not */


/* STATIC DATA */

extern /* const */ struct special_s static_specials[];
extern const size_t static_specials_length;

#define STATIC_SPECIAL(i)	((obj_t)&static_specials[i])

#define obj_empty		STATIC_SPECIAL(0)
#define obj_eof			STATIC_SPECIAL(1)
#define obj_true		STATIC_SPECIAL(2)
#define obj_false		STATIC_SPECIAL(3)
#define obj_unspec		STATIC_SPECIAL(4)
#define obj_undef		STATIC_SPECIAL(5)
#define obj_uninit		STATIC_SPECIAL(6)
#define obj_unbound		STATIC_SPECIAL(7)

extern /* const */ struct symbol_s static_symbols[];
extern const size_t static_symbols_length;

#define STATIC_SYMBOL(i)	((obj_t)&static_symbols[i])

#define sym_anonymous		STATIC_SYMBOL(0)
#define sym_arrow		STATIC_SYMBOL(1)
#define sym_begin		STATIC_SYMBOL(2)
#define sym_continuation	STATIC_SYMBOL(3)
#define sym_define		STATIC_SYMBOL(4)
#define sym_else		STATIC_SYMBOL(5)
#define sym_lambda		STATIC_SYMBOL(6)
#define sym_quasiquote		STATIC_SYMBOL(7)
#define sym_quote		STATIC_SYMBOL(8)
#define sym_unquote		STATIC_SYMBOL(9)
#define sym_unquote_splicing	STATIC_SYMBOL(10)
#define sym_run			STATIC_SYMBOL(11)
#define sym_fatal		STATIC_SYMBOL(12)
#define sym_start		STATIC_SYMBOL(13)
#define sym_eval		STATIC_SYMBOL(14)
#define sym_plus		STATIC_SYMBOL(15)
#define sym_minus		STATIC_SYMBOL(16)
#define sym_dotdotdot		STATIC_SYMBOL(17)


/* CONSTRUCTORS
 *
 * Constructors use a single local variable, "_baby", to store a pointer
 * to the newly allocated object.  Extreme care must be taken no to
 * attempt to allocate another object while this variable is in scope,
 * because it will not be scanned by the garbage collector.
 *
 * Newly constructed functions have no continuation.  They are never
 * executed in place, but always copied to a new "stack frame" by
 * CALL.
 */

extern obj_t alloc(state_t, size_t);

#ifdef ALLOC_CHECKING

/* Prevent constructors from being called within each other */

extern int allocating;

#define BEGIN_MAKE \
  BEGIN \
    ASSERT(!allocating); \
    allocating = 1; \
    BEGIN

#define END_MAKE \
    END; \
    allocating = 0; \
  END

#else /* ALLOC_CHECKING not */

#define BEGIN_MAKE BEGIN
#define END_MAKE END

#endif /* ALLOC_CHECKING */

#ifdef AGE_STATS
#define MAKE_OBJ(var, _type, size) \
  BEGIN \
    (var) = alloc(state, size); \
    (var)->header.type = (_type); \
    (var)->header.birthday = state->step; \
  END
#else
#define MAKE_OBJ(var, _type, size) \
  BEGIN \
    (var) = alloc(state, size); \
    (var)->header.type = (_type); \
  END
#endif

#define MAKE_PAIR(var, pair_car, pair_cdr) \
  BEGIN_MAKE \
    make_pair(state); \
    state->baby->pair.car = (pair_car); \
    state->baby->pair.cdr = (pair_cdr); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_INTEGER(var, int_int) \
  BEGIN_MAKE \
    make_integer(state); \
    state->baby->integer.integer = (int_int); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_PROC_REGS(var, fun_name, fun_entry, len) \
  BEGIN_MAKE \
    make_proc_regs(state, len); \
    state->baby->proc.name  = (fun_name); \
    state->baby->proc.entry = (fun_entry); \
    state->baby->proc.cont = obj_undef; /* @@@@ why? */ \
    (var) = state->baby; \
  END_MAKE

#define MAKE_PROC(var, fun_name, fun_entry) \
  MAKE_PROC_REGS(var, fun_name, fun_entry, 0)

#ifdef AGE_STATS
#define DUP_PROC_BIRTHDAY \
  BEGIN \
    state->baby->header.birthday = state->step; \
  END
#else
#define DUP_PROC_BIRTHDAY       NOOP
#endif

#define DUP_PROC_REGS(var, f, len) \
  BEGIN_MAKE \
    size_t _len = (len), _i, _regs = (f)->proc.regs; \
    size_t _size = offsetof(proc_s, locs[_len]); \
    make_proc_regs(state, _len); \
    ASSERT(_len >= _regs); \
    memcpy(state->baby, (f), _size); \
    for(_i = _regs; _i < _len; ++_i) \
      state->baby->proc.locs[_i] = obj_uninit; \
    state->baby->proc.regs = _len; \
    DUP_PROC_BIRTHDAY; \
    (var) = state->baby; \
    COUNT(DUP_PROC); \
  END_MAKE

#define DUP_PROC(var, f) \
  DUP_PROC_REGS(var, f, (f)->proc.regs)

#define MAKE_VECTOR(var, len, fill) \
  BEGIN_MAKE \
    size_t _len = (len), _i; \
    make_vector(state, _len); \
    for(_i = 0; _i < _len; ++_i) \
      state->baby->vector.elements[_i] = (fill); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_SYMBOL(var, len, str) \
  BEGIN_MAKE \
    make_symbol(state, len, str); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_STRING_UNINIT(var, len) \
  BEGIN_MAKE \
    make_string_uninit(state, len); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_STRING(var, len, str) \
  BEGIN_MAKE \
    size_t _len = (len); \
    make_string_uninit(state, _len); \
    memcpy(state->baby->string.string, (str), _len + 1); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_SPECIAL(var, str) \
  BEGIN_MAKE \
    make_special(state, str); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_INPORT(var, str) \
  BEGIN_MAKE \
    make_inport(state, str); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_OUTPORT(var, str) \
  BEGIN_MAKE \
    make_outport(state, str); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_EXCEPTION(var, obj) \
  BEGIN_MAKE \
    make_exception(state); \
    state->baby->exception.object = (obj); \
    (var) = state->baby; \
  END_MAKE

#define MAKE_CHARACTER(var, ch) \
  BEGIN_MAKE \
    make_character(state, ch); \
    (var) = state->baby; \
  END_MAKE


/* ABSTRACT MACHINE INSTRUCTIONS
 *
 * NOTE: The argument registers (A0 etc.) may not be passed to CALL,
 * TAIL, or RET, because they are used by those macros to construct the
 * arguments for the callee (or returnee, which is the same thing in
 * CPS).  The arguments of CALL, TAIL, and RET may not contain functions
 * that allocate.
 */

#define ARGS	state->args	/* argument count */
#define ARGL	state->argl	/* argument list */
#define A0	CAR(ARGL)	/* first argument */
#define A1	CADR(ARGL)	/* second argument */
#define A2	CADDR(ARGL)	/* third argument */
#define A3	CADDDR(ARGL)	/* fourth argument */
#define L0	LOC(state->here, 0)
#define L1	LOC(state->here, 1)
#define L2	LOC(state->here, 2)
#define L3	LOC(state->here, 3)
#define L4	LOC(state->here, 4)
#define L5	LOC(state->here, 5)
#define L6	LOC(state->here, 6)
#define L7	LOC(state->here, 7)
#define T0	state->temp[0]
#define T1	state->temp[1]
#define T2	state->temp[2]


/* JUMP(label) -- jump to another block
 *
 * To jump to another block we simply update "here->proc.entry".  In abstract
 * terms, we're creating a new function which has our local state as its
 * closure and has an entry point at the block, and executing that function.
 * But we take advantage of the fact that there is only one reference to "here"
 * to re-use the function.
 *
 * JUMP works by setting "here->proc.entry" (the pc) and returning to the
 * abstract machine driver loop.
 *
 * Leaf procedures may not JUMP, because "here" points to the original procedure,
 * an isn't necessarily unique.	 Updating "here->proc.entry" would affect other
 * invocations of the procedure.
 */

#define JUMP(label) \
  BEGIN \
    ASSERT(state->here->proc.cont != obj_undef); \
    state->here->proc.entry = (label); \
    COUNT(AM_JUMP); \
    return; \
  END


/* CALL_TRANS(procedure, label) -- call a procedure
 *
 * Calls the procedure, using label as its return address.  If the procedure
 * returns, execution will resume at the label.	 Abstractly, we create a new
 * function which has our local state as its closure and an entry point at
 * the label, and pass that to the procedure to use as its continuation.  We
 * take advantage of the fact that there is only one reference to "here" to
 * re-use the current function as the continuation.
 *
 * CALL works by updating "here->proc.entry" to the label, setting "state->cont"
 * (the current continuation) to "here", and setting "here" to the procedure,
 * so that execution starts there.  To "return", the procedure calls the
 * current continuation using RET.  There is almost no difference between
 * RET and CALL(state->cont).
 *
 * Leaf procedures may not CALL, because (1) "here" points to the original
 * procedure, and isn't necessarily unique, so updating "here->proc.entry"
 * would affect other invocations, and (2) calling corrupts "state->cont",
 * which is not preserved in leaf procedures.
 */

#define CALL_TRANS(procedure, label) \
  BEGIN \
    ASSERT(state->here->proc.cont != obj_undef); \
    ASSERT(TYPE(procedure) == TYPE_PROC); \
    state->here->proc.entry = (label); \
    state->cont = state->here; \
    state->here = (procedure); \
    COUNT(AM_CALL); \
  END

#define CALL(procedure, label) \
  BEGIN \
    CALL_TRANS(procedure, label); \
    return; \
  END  


/* ENTER(n) and LEAVE -- non-leaf procedures
 *
 * ENTER creates local state for a procedure and checks that it has at least
 * "n" local registers, and preserves "state->cont", the current
 * continuation, so that the procedure may call other procedures. LEAVE
 * restores "state->cont".
 *
 * A non-leaf procedure must ENTER before it CALLs or JUMPs, and must
 * LEAVE before it RETs or TAILs.
 */

#define ENTER(n) \
  BEGIN \
    ASSERT(state->here->proc.cont == obj_undef); \
    DUP_PROC_REGS(state->here, state->here, n); \
    state->here->proc.cont = state->cont; \
    COUNT(AM_ENTER); \
  END

#define LEAVE() \
  BEGIN \
    ASSERT(state->here->proc.cont != obj_undef); \
    state->cont = state->here->proc.cont; \
  END


/* RET -- return from a procedure
 *
 * RET returns to the calling procedure by invoking "state->cont",
 * the current continuation.  In the absence of "call/cc" all it
 * would have to do is set "state->here" to "state->cont".
 *
 * To implement "call/cc" we have the idea of a "read only" active
 * procedure.  Call/cc sets the read only flag on its caller so that
 * the caller's state is frozen, and when the continuation is invoked
 * execution will continue from wherever "call/cc" was called.	This
 * can be done by taking a copy of the current continution whenever
 * "call/cc" is called, but we do this lazily by duplicating any
 * read only procedure we return to, and making _it's_ continuation
 * read only.  This effectively peels off a copy of the continuation
 * stack.
 *
 * The "read only" flag can be seen as a kind of reference count.
 * Normally, there's only one reference to an active procedure: either
 * from "here" or from another procedure's continuation.  That's why
 * stacks can be used to represent continuations most of the time.
 * However, if "call/cc" is used, a second reference is created, so
 * we set "read only".	And when we duplicate an active procedure
 * on return, that creates an extra reference in that procedure's
 * continuation, so we have to set "read only" there too.
 *
 * The last procedure on the stack is a sentinel, with itself as a
 * continuation, so that RET doesn't have to detect this special case.
 */

#define RET_DUP() \
  BEGIN \
    if(state->here->proc.read_only) { \
      DUP_PROC(state->here, state->here); \
      state->here->proc.read_only = 0; \
      ASSERT(TYPE(state->here->proc.cont) == TYPE_PROC); \
      state->here->proc.cont->proc.read_only = 1; \
      COUNT(AM_RET_DUP); \
    } \
  END

#define RET_TRANS() \
  BEGIN \
    ASSERT(state->here->proc.cont == obj_undef || \
	   state->here->proc.cont == state->cont); \
    state->here = state->cont; \
    RET_DUP(); \
    COUNT(AM_RET); \
  END

#define RET() \
  BEGIN \
    RET_TRANS(); \
    return; \
  END


/* TAIL -- tail call a procedure
 *
 * TAIL is equivalent to CALL followed by RET, except that it cuts
 * out the middle continuation.	 This is very important, because it
 * means that tail recursive functions can have an indefinite number
 * of invocations, as required by R5RS.
 *
 * TAIL can be used by leaf procedures, or by non-leaf procedures
 * after a LEAVE.  In other words, "state->cont", the current
 * continuaton, must be intact, so that the tailed-to procedure
 * can continue to there when it's finished.
 */

#define TAIL_TRANS(f) \
  BEGIN \
    ASSERT(state->here->proc.cont == obj_undef || \
	   state->here->proc.cont == state->cont); \
    state->here = (f); \
    COUNT(AM_TAIL); \
  END

#define TAIL(f) \
  BEGIN \
    TAIL_TRANS(f); \
    return; \
  END

#define NEW_ARGS() \
  BEGIN \
    state->args = 0; \
    state->argl = obj_empty; \
  END

#define PUSH_ARG(arg) \
  BEGIN \
    MAKE_PAIR(state->argl, arg, state->argl); \
    ++state->args; \
  END

#define CALL0(f, link) \
  BEGIN \
    NEW_ARGS(); \
    CALL(f, link); \
  END

#define CALL1(f, a0, link) \
  BEGIN \
    NEW_ARGS(); \
    PUSH_ARG(a0); \
    CALL(f, link); \
  END

#define CALL2(f, a0, a1, link) \
  BEGIN \
    NEW_ARGS(); \
    PUSH_ARG(a1); \
    PUSH_ARG(a0); \
    CALL(f, link); \
  END

#define CALL3(f, a0, a1, a2, link) \
  BEGIN \
    NEW_ARGS(); \
    PUSH_ARG(a2); \
    PUSH_ARG(a1); \
    PUSH_ARG(a0); \
    CALL(f, link); \
  END

#define CALL4(f, a0, a1, a2, a3, link) \
  BEGIN \
    NEW_ARGS(); \
    PUSH_ARG(a3); \
    PUSH_ARG(a2); \
    PUSH_ARG(a1); \
    PUSH_ARG(a0); \
    CALL(f, link); \
  END

#define TAIL1(f, a0) \
  BEGIN \
    NEW_ARGS(); \
    PUSH_ARG(a0); \
    TAIL(f); \
  END

#define TAIL2(f, a0, a1) \
  BEGIN \
    NEW_ARGS(); \
    PUSH_ARG(a1); \
    PUSH_ARG(a0); \
    TAIL(f); \
  END

#define TAIL4(f, a0, a1, a2, a3) \
  BEGIN \
    NEW_ARGS(); \
    PUSH_ARG(a3); \
    PUSH_ARG(a2); \
    PUSH_ARG(a1); \
    PUSH_ARG(a0); \
    TAIL(f); \
  END

#define RET0() \
  BEGIN \
    NEW_ARGS(); \
    RET(); \
  END

#define RET1(r0) \
  BEGIN \
    NEW_ARGS(); \
    PUSH_ARG(r0); \
    RET(); \
  END


/* bind -- bind symbol in environment
 *
 * In Scheme, define will actually rebind (i.e. set) a symbol in the
 * same frame of the environment, or add a binding if it wasn't already
 * set.	 This has the effect of making bindings local to functions
 * (see how exec_entry adds an empty frame to the environments),
 * allowing recursion, and allowing redefinition at the top level.
 * See R4R2 section 5.2 for details.
 *
 * Because bind can allocate, an in-line BIND is needed for the VM.
 */

extern obj_t lookup_in_frame(state_t, obj_t, obj_t);

#define BIND(env, symbol, value, temp) \
  BEGIN \
    ASSERT(TYPE(env) == TYPE_PAIR); \
    (temp) = lookup_in_frame(state, CAR(env), symbol); \
    if((temp) != obj_undef) \
      SETCDR(temp, value); \
    else { \
      MAKE_PAIR(temp, symbol, value); \
      MAKE_PAIR(CAR(env), temp, CAR(env)); \
    } \
  END


/* proctab_s -- procedure table entry */

typedef struct proctab_s {
  const char *name;
  label_t entry;
} proctab_s;


/* @@@@ FUNCTIONS */

/* from scrw.c */
extern void write_char(state_t, FILE *, char);
extern void write_string(state_t, FILE *, const char *);
extern void write_formatted(state_t, FILE *, const char *, ...)
  __attribute__((format(printf, 3, 4)));

/* from sceval.c, used in tracing and backtrace */
extern void eval_entry(state_t);

/* from scsyntax.c, used in tracing and backtrace */
extern void define_entry(state_t);

/* from scmm.c */
extern mms_t mms_create(void);
extern void *mms_alloc(mms_t, size_t);
extern void heap_check(state_t);
extern void gc(state_t, size_t);
extern void stats(state_t, void *, void *);
extern int register_state(state_t);
extern void make_pair(state_t);
extern void make_integer(state_t);
extern void make_vector(state_t, size_t);
extern void make_symbol(state_t, size_t, char *);
extern void make_string_uninit(state_t, size_t);
extern void make_proc_regs(state_t, size_t);
extern void make_special(state_t, char *);
extern void make_inport(state_t, FILE *);
extern void make_outport(state_t, FILE *);
extern void make_exception(state_t);
extern void make_character(state_t, char);

/* from scrun.c */
extern void no_entry(state_t) __attribute__((noreturn));

extern void intern(state_t);
extern void error(state_t, const char *, ...) __attribute__((noreturn, format(printf, 2, 3)));
extern state_t make_state(void);
extern int run(state_t);
extern void write_obj(state_t, obj_t, unsigned, FILE *);
extern obj_t lookup(state_t, obj_t, obj_t);
extern void check_args(state_t, ulong, ...);
extern void check_results(state_t, ulong, ...);
extern const char *type_name(type_t);
extern void bind_procs(state_t state, const proctab_s *, const proctab_s *);
extern int eqvp(obj_t, obj_t);

#endif /* sc_h */
