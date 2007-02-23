#ifndef _PPC_LISPREGS_H_
#define _PPC_LISPREGS_H_

#if defined DARWIN
#if defined LANGUAGE_ASSEMBLY
#define REG(num) r/**/num
#else
#define REG(num) num
#endif
#else
#define REG(num) num
#endif

#define REG_NUM(num) num

#define NREGS 32

#define reg_ZERO      REG(0)	/* Should alwasy contain 0 in lisp */
#define reg_NSP       REG(1)	/* The number/C stack pointer */
#define reg_POLL      REG(2)	/* Lisp preemption/Mystery SVR4 ABI reg */
#define reg_NL0       REG(3)	/* FF param/result 1 */
#define reg_NL1       REG(4)	/* FF param/result 2 */
#define reg_NL2       REG(5)	/* FF param 3 */
#define reg_NL3       REG(6)
#define reg_NL4       REG(7)
#define reg_NL5       REG(8)
#define reg_NL6       REG(9)
#define reg_NL7       REG(10)	/* Last (8th) FF param */
#define reg_NARGS     REG(11)
#define reg_CFUNC     REG(12)	/* Silly to blow a reg on FF-name */
#define reg_NFP       REG(13)	/* Lisp may save around FF-call */
#define reg_BSP       REG(14)	/* Binding stack pointer */
#define reg_CFP       REG(15)	/* Control/value stack frame pointer */
#define reg_CSP       REG(16)	/* Control/value stack top */
#define reg_ALLOC     REG(17)	/* (Global) dynamic free pointer */
#define reg_NULL      REG(18)	/* NIL and globals nearby */
#define reg_CODE      REG(19)	/* Current function object */
#define reg_CNAME     REG(20)	/* Current function name */
#define reg_LEXENV    REG(21)	/* And why burn a register for this ? */
#define reg_OCFP      REG(22)	/* The caller's reg_CFP */
#define reg_LRA       REG(23)	/* Tagged lisp return address */
#define reg_A0        REG(24)	/* First function arg/return value */
#define reg_A1        REG(25)	/* Second. */
#define reg_A2        REG(26)	/*  */
#define reg_A3        REG(27)	/* Last of (only) 4 arg regs */
#define reg_L0	      REG(28)	/* Tagged temp regs */
#define reg_L1        REG(29)
#define reg_FDEFN     REG(30)
#define reg_LIP       REG(31)	/* Lisp Interior Pointer, e.g., locative */

/*
 * Same as above, but we just want the register number, not the
 * register name
 */

#define reg_ZERO_NUM      0	/* Should alwasy contain 0 in lisp */
#define reg_NSP_NUM       1	/* The number/C stack pointer */
#define reg_POLL_NUM      2	/* Lisp preemption/Mystery SVR4 ABI reg */
#define reg_NL0_NUM       3	/* FF param/result 1 */
#define reg_NL1_NUM       4	/* FF param/result 2 */
#define reg_NL2_NUM       5	/* FF param 3 */
#define reg_NL3_NUM       6
#define reg_NL4_NUM       7
#define reg_NL5_NUM       8
#define reg_NL6_NUM       9
#define reg_NL7_NUM       10	/* Last (8th) FF param */
#define reg_NARGS_NUM     11
#define reg_CFUNC_NUM     12	/* Silly to blow a reg on FF-name */
#define reg_NFP_NUM       13	/* Lisp may save around FF-call */
#define reg_BSP_NUM       14	/* Binding stack pointer */
#define reg_CFP_NUM       15	/* Control/value stack frame pointer */
#define reg_CSP_NUM       16	/* Control/value stack top */
#define reg_ALLOC_NUM     17	/* (Global) dynamic free pointer */
#define reg_NULL_NUM      18	/* NIL and globals nearby */
#define reg_CODE_NUM      19	/* Current function object */
#define reg_CNAME_NUM     20	/* Current function name */
#define reg_LEXENV_NUM    21	/* And why burn a register for this ? */
#define reg_OCFP_NUM      22	/* The caller's reg_CFP_NUM */
#define reg_LRA_NUM       23	/* Tagged lisp return address */
#define reg_A0_NUM        24	/* First function arg/return value */
#define reg_A1_NUM        25	/* Second. */
#define reg_A2_NUM        26	/*  */
#define reg_A3_NUM        27	/* Last of (only) 4 arg regs */
#define reg_L0_NUM	  28	/* Tagged temp regs */
#define reg_L1_NUM        29
#define reg_FDEFN_NUM     30
#define reg_LIP_NUM       31	/* Lisp Interior Pointer, e.g., locative */

#define reg_LR		  34    /* LR register.  See Darwin-os.c */
#define reg_CTR           35    /* CTR register.  See Darwin-os.c */
#define REGNAMES \
        "ZERO",		"NSP",	        "POLL",		"NL0", \
	"NL1",		"NL2",		"NL3",		"NL4", \
        "NL5",		"NL6",		"NL7",		"NARGS", \
        "CFUNC",	"NFP",		"BSP",		"CFP",	 \
        "CSP",		"ALLOC",	"NULL",		"CODE", \
        "CNAME",	"LEXENV",	"OCFP",		"LRA", \
        "A0",	        "A1",	        "A2",		"A3", \
        "L0",		"L1",		"FDEFN",	"LIP"

#define BOXED_REGISTERS { \
    reg_FDEFN, reg_CODE, reg_CNAME, reg_LEXENV, reg_OCFP, reg_LRA, \
    reg_A0, reg_A1, reg_A2, reg_A3, \
    reg_L0, reg_L1 \
}

#ifndef LANGUAGE_ASSEMBLY
#if defined DARWIN
#define SC_REG(sc,reg) (*sc_reg(sc,reg))
#define SC_PC(sc) (sc->uc_mcontext->ss.srr0)
#else
#define SC_REG(sc,reg) (((unsigned long *)(sc->regs))[(reg)])
#define SC_PC(sc) (((unsigned long *)(sc->regs))[PT_NIP])
#endif

#endif

#endif /* _PPC_LISPREGS_H_ */
