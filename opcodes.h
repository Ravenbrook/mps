/* opcodes.h -- compiler opcodes used in output of SC compiler
 *
 * The compiler outputs code in terms of opcodes which are defined here.
 * It outputs a #include of opcodes.h at the start of each unit.
 *
 * $Id$
 */

#include "sc.h"

#define BEGIN			do {
#define END			} while(0)
#define NOOP			BEGIN END
#define BLOCK(label)		block ## label
#define REG(reg)		LOC(state->here, (reg) + 3)
#define CONSTANT(index)		VECREF(L0, index)
#define FREE(index)		VECREF(L1, index)
#define LOCAL(index)		VECREF(L2, index)
#define SETLOCAL(index, value)	VECSET(L2, index, value)

#define SETREG(reg, value) \
  BEGIN \
    ASSERT(0 <= (reg) && (reg) <= state->here->proc.regs); \
    REG(reg) = (value); \
  END

#define BLOCK_DECL(label) \
  static void BLOCK(label)(state_t state)

#define BLOCK_BEGIN(label) \
  BLOCK_DECL(label) \
  {

#define OP_ENTER(regs, locals) \
  BEGIN \
    ENTER((regs) + 3); \
    MAKE_VECTOR(L2, locals, obj_uninit); \
  END

#define OP_LEAVE()			LEAVE()
#define OP_JUMP(label)			JUMP(BLOCK(label))
#define OP_RET()			RET()
#define OP_CALL(reg, label)		CALL(REG(reg), BLOCK(label))
#define OP_TAIL(proc_reg)		TAIL(REG(proc_reg))

#define OP_MOVE(reg, from)              SETREG(reg, REG(from))

#define OP_LOAD_STATIC(reg, obj)	SETREG(reg, obj)
#define OP_LOAD_CONSTANT(reg, index)	SETREG(reg, CONSTANT(index))
#define OP_LOAD_ARGS(reg)		SETREG(reg, ARGL)
#define OP_LOAD_EMPTY(reg)		SETREG(reg, obj_empty)
#define OP_LOAD_UNBOUND(reg)		SETREG(reg, obj_unbound)
#define OP_LOAD_UNSPECIFIED(reg)        SETREG(reg, obj_unspec)
#define OP_LOAD_TRUE(reg)               SETREG(reg, obj_true)
#define OP_LOAD_FALSE(reg)              SETREG(reg, obj_false)


#define OP_MAKE_PAIR(reg, car, cdr)	MAKE_PAIR(REG(reg), REG(car), REG(cdr))
#define OP_MAKE_VECTOR(reg, length)	MAKE_VECTOR(REG(reg), length, obj_uninit)
#define OP_MAKE_INTEGER(reg, integer)	MAKE_INTEGER(REG(reg), integer)
#define OP_MAKE_CHARACTER(reg, c)       MAKE_CHARACTER(REG(reg), c)

#define OP_SET_CONSTANTS(constants_reg)	L0 = REG(constants_reg)
#define OP_SET_FREES(free_vector_reg)	L1 = REG(free_vector_reg)

#define OP_STORE_ARGS(reg, length) \
  BEGIN \
    ARGL = REG(reg); \
    ARGS = (length); \
  END

#define OP_MAKE_STRING(reg, length, string) \
  BEGIN \
    MAKE_STRING(REG(reg), length, string); \
  END

#define OP_MAKE_SYMBOL(reg, length, string) \
  BEGIN \
    MAKE_STRING(T0, length, string); \
    intern(state); \
    REG(reg) = T0; \
  END

#define OP_VECTOR_SET(vector_reg, index, value_reg) \
  VECSET(REG(vector_reg), index, REG(value_reg))

#define OP_BIND(env_reg, symbol_reg, value_reg) \
  BEGIN \
    BIND(REG(env_reg), REG(symbol_reg), REG(value_reg), T0); \
  END

#define OP_LOAD_FREE(reg, index) \
  BEGIN \
    T0 = FREE(index); \
    if(CDR(T0) == obj_unbound) goto fail_unbound; \
    REG(reg) = CDR(T0); \
  END

#define OP_LOAD_FREE_BINDING(reg, index) \
  BEGIN \
    REG(reg) = FREE(index); \
  END

#define OP_SET_FREE(index, value_reg) \
  BEGIN \
    T0 = FREE(index); \
    if(CDR(T0) == obj_unbound) goto fail_unbound; \
    SETCDR(T0, REG(value_reg)); \
  END

#define OP_LOAD_LOCAL(reg, index) \
  BEGIN \
    T0 = LOCAL(index); \
    if(CDR(T0) == obj_unbound) goto fail_unbound; \
    REG(reg) = CDR(T0); \
  END

#define OP_SET_LOCAL(index, value_reg) \
  BEGIN \
    SETCDR(LOCAL(index), REG(value_reg)); \
  END

#define OP_LOAD_LOCAL_BINDING(reg, index) \
  BEGIN \
    REG(reg) = LOCAL(index); \
  END

#define OP_SET_LOCAL_BINDING(index, pair_reg) \
  BEGIN \
    ASSERT(TYPE(REG(pair_reg)) == TYPE_PAIR); \
    ASSERT(TYPE(CAR(REG(pair_reg))) == TYPE_SYMBOL); \
    SETLOCAL(index, REG(pair_reg)); \
  END

#define OP_FIND_FREE(binding_reg, env_reg, symbol_reg) \
  BEGIN \
    T0 = lookup(state, REG(env_reg), REG(symbol_reg)); \
    if(T0 == obj_undef) { \
      BIND(REG(env_reg), REG(symbol_reg), obj_unbound, T0); \
      T0 = lookup(state, REG(env_reg), REG(symbol_reg)); \
      ASSERT(T0 != obj_undef); \
    } \
    REG(binding_reg) = T0; \
  END

#define OP_MAKE_PROC(proc_reg, constants_reg, free_vector_reg, entry_point) \
  BEGIN \
    MAKE_PROC_REGS(REG(proc_reg), sym_anonymous, BLOCK(entry_point), 2); \
    LOC(REG(proc_reg), 0) = REG(constants_reg); \
    LOC(REG(proc_reg), 1) = REG(free_vector_reg); \
  END

#define OP_SET_PROC_NAME(proc_reg, name_reg) \
  BEGIN \
    ASSERT(TYPE(REG(name_reg)) == TYPE_SYMBOL); \
    ASSERT(TYPE(REG(proc_reg)) == TYPE_PROC); \
    REG(proc_reg)->proc.name = REG(name_reg); \
  END

#define OP_LOAD_RESULT(reg) \
  BEGIN \
    if(ARGS != 1) goto fail_results; \
    REG(reg) = A0; \
  END

#define OP_LOAD_CAR(reg, pair_reg) \
  BEGIN \
    if(TYPE(REG(pair_reg)) != TYPE_PAIR) goto fail_car; \
    REG(reg) = CAR(REG(pair_reg)); \
  END

#define OP_LOAD_CDR(reg, pair_reg) \
  BEGIN \
    if(TYPE(REG(pair_reg)) != TYPE_PAIR) goto fail_cdr; \
    REG(reg) = CDR(REG(pair_reg)); \
  END

#define OP_LOAD_ARG(reg, args_reg) \
  BEGIN \
    if(REG(args_reg) == obj_empty) goto fail_args_few; \
    ASSERT(TYPE(REG(args_reg)) == TYPE_PAIR); \
    REG(reg) = CAR(REG(args_reg)); \
    REG(args_reg) = CDR(REG(args_reg)); \
  END

#define OP_LAST_ARG(args_reg) \
  BEGIN \
    if(REG(args_reg) != obj_empty) goto fail_args_many; \
  END

#define OP_BRANCH_FALSE(label, reg) \
  BEGIN \
    if(REG(reg) == obj_false) \
      JUMP(BLOCK(label)); \
  END

#define OP_BRANCH_TRUE(label, reg) \
  BEGIN \
    if(REG(reg) != obj_false) \
      JUMP(BLOCK(label)); \
  END

/* We hope that dead-code elimintation will remove all the failure cases */
/* not referenced from the block. */

#define BLOCK_END() \
  fail_results: \
    error(state, "expected one result"); \
  fail_car: \
    error(state, "argument of car must be of type pair"); \
  fail_cdr: \
    error(state, "argument of cdr must be of type pair"); \
  fail_unbound: \
    /* @@@@ Assumes symbol not funny. */ \
    error(state, "unbound symbol \"%s\"", SYMSTR(CAR(T0))); \
  fail_args_few: \
    error(state, "too few arguments"); \
  fail_args_many: \
    error(state, "too many arguments"); \
  }

#define UNIT_BEGIN(ident) \
  static void block0(state_t); \
  const label_t sc_unit_ ## ident = &block0;

#define UNIT_END()
