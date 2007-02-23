/* x86-lispregs.h -*- Mode: C; -*-
 * $Header: /project/cmucl/cvsroot/src/lisp/x86-lispregs.h,v 1.4 2005/01/13 19:55:01 fgilham Exp $
 */

#ifndef _X86_LISPREGS_H_
#define _X86_LISPREGS_H_

/* These register names and offsets correspond to definitions
 * in compiler/x86/vm.lisp. They map into accessors in the
 * os dependent <machine/signal.h> structure via the sc_reg
 * os dependent function.
 */

#define NREGS	(8)

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define reg_EAX REG( 0)
#define reg_ECX REG( 2)
#define reg_EDX REG( 4)
#define reg_EBX REG( 6)
#define reg_ESP REG( 8)
#define reg_EBP REG(10)
#define reg_ESI REG(12)
#define reg_EDI REG(14)

#define reg_SP reg_ESP
#define reg_FP reg_EBP

#define REGNAMES "EAX", "ECX", "EDX", "EBX", "ESP", "EBP", "ESI", "EDI"

/* These registers can contain lisp object pointers */
#define BOXED_REGISTERS {\
  reg_EAX, reg_ECX, reg_EDX, reg_EBX, reg_ESI, reg_EDI \
  }

/* N is offset in storage class (SC) as defined in vm.lisp.
 * Ordering in sigcontext is probably os dependent so let
 * xxx-os.c handle it.
 */

#define SC_REG(sc, n) sc_reg(sc,n)

#ifdef __NetBSD__
#define SC_PC(uc) ((uc)->uc_mcontext.__gregs[_REG_EIP])
#define SC_SP(uc) ((uc)->uc_mcontext.__gregs[_REG_ESP])
#else
#define SC_PC(sc) ((sc)->sc_pc)
#define SC_SP(sc) ((sc)->sc_esp)
#endif

#endif /* _X86_LISPREGS_H_ */
