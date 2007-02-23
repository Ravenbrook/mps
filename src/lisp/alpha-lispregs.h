/* $Header: /project/cmucl/cvsroot/src/lisp/alpha-lispregs.h,v 1.4 2005/01/13 19:55:00 fgilham Exp $ */

#ifndef _ALPHA_LISPREGS_H_
#define _ALPHA_LISPREGS_H_

#define NREGS	(32)

#ifdef LANGUAGE_ASSEMBLY
#ifdef __linux__
#define R(s) $ ## s
#define REG(num) $##num
#else
#define R(s) s
#define REG(num) $/**/num
#endif
#else
#define REG(num) num
#endif

#define reg_LIP REG(0)
#define reg_A0 REG(1)
#define reg_A1 REG(2)
#define reg_A2 REG(3)
#define reg_A3 REG(4)
#define reg_A4 REG(5)
#define reg_A5 REG(6)
#define reg_L0 REG(7)
#define reg_NARGS REG(8)
#define reg_CSP REG(9)
#define reg_CFP REG(10)
#define reg_OCFP REG(11)
#define reg_BSP REG(12)
#define reg_LEXENV REG(13)
#define reg_CODE REG(14)
#define reg_NULL REG(15)
#define reg_NL0 REG(16)
#define reg_NL1 REG(17)
#define reg_NL2 REG(18)
#define reg_NL3 REG(19)
#define reg_NL4 REG(20)
#define reg_NL5 REG(21)
#define reg_ALLOC REG(22)
#define reg_FDEFN REG(23)
#define reg_CFUNC REG(24)
#define reg_NFP REG(25)
#define reg_LRA REG(26)
#define reg_L1 REG(27)
#define reg_L2 REG(28)
#define reg_GP REG(29)
#define reg_NSP REG(30)
#define reg_ZERO REG(31)


#define REGNAMES \
    "LIP", "NL0", "NL1", "NL2", "NL3", "NL4", "NL5", "L0", "NARGS", \
    "CSP", "CFP", "OCFP", "BSP", "LEXENV", "CODE", "FDEFN", "A0", "A1", \
    "A2", "A3", "A4", "A5", "ALLOC", "NULL", "CFUNC", "NFP", "LRA", "L1", \
    "L2", "GP", "NSP", "ZERO"

#define BOXED_REGISTERS { \
    reg_CODE, reg_FDEFN, reg_LEXENV, reg_NARGS, reg_OCFP, reg_LRA, \
    reg_A0, reg_A1, reg_A2, reg_A3, reg_A4, reg_A5, \
    reg_L0, reg_L1, reg_L2 \
}

#define SC_REG(sc, n) ((sc)->sc_regs[n])
#define SC_PC(sc) ((sc)->sc_pc)

#define call_into_lisp_LRA_page 0x10000

#endif /* _ALPHA_LISPREGS_H_ */
