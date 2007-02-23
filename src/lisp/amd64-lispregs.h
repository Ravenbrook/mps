/* x86-lispregs.h -*- Mode: C; -*-
 * $Header: /project/cmucl/cvsroot/src/lisp/amd64-lispregs.h,v 1.3 2005/01/13 19:55:00 fgilham Exp $
 */

/* These register names and offsets correspond to definitions
 * in compiler/amd64/vm.lisp. They map into accessors in the
 * os dependent <machine/signal.h> structure via the sc_reg
 * os dependent function.
 */

#ifndef _AMD64_LISPREGS_H_
#define _AMD64_LISPREGS_H_

#define NREGS	(16)

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define reg_RAX REG( 0)
#define reg_RCX REG( 2)
#define reg_RDX REG( 4)
#define reg_RBX REG( 6)
#define reg_RSP REG( 8)
#define reg_RBP REG(10)
#define reg_RSI REG(12)
#define reg_RDI REG(14)
#define reg_R8  REG(16)
#define reg_R9  REG(18)
#define reg_R10 REG(20)
#define reg_R11 REG(22)
#define reg_R12 REG(24)
#define reg_R13 REG(26)
#define reg_R14 REG(28)
#define reg_R15 REG(30)

#define reg_SP reg_RSP
#define reg_FP reg_RBP

#define REGNAMES "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"

/* These registers can contain lisp object pointers */
#define BOXED_REGISTERS {\
  reg_RAX, reg_RCX, reg_RDX, reg_RBX, reg_RSI, reg_RDI, reg_R8, reg_R9, reg_R10, reg_R11, reg_R12, reg_R13, reg_R14, reg_R15 \
  }

/* N is offset in storage class (SC) as defined in vm.lisp.
 * Ordering in sigcontext is probably os dependent so let
 * xxx-os.c handle it.
 */

#define SC_REG(sc, n) sc_reg(sc,n)
#define SC_PC(sc) ((sc)->sc_pc)

#endif /* _AMD64_LISPREGS_H_ */
