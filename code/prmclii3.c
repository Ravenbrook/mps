/* prmclii3.c: MUTATOR CONTEXT INTEL 386 (LINUX)
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Implement the mutator context module. <design/prmc>.
 *
 *
 * SOURCES
 *
 * .source.i486: Intel486 Microprocessor Family Programmer's
 * Reference Manual
 *
 * .source.linux.kernel: Linux kernel source files.
 *
 *
 * ASSUMPTIONS
 *
 * .sp: The stack pointer in the context is ESP.
 *
 * .assume.regref: The registers in the context can be modified by
 * storing into an MRef pointer.
 */

#include "prmcix.h"
#include "prmci3.h"

SRCID(prmclii3, "$Id$");

#if !defined(MPS_OS_LI) || !defined(MPS_ARCH_I3)
#error "prmclii3.c is specific to MPS_OS_LI and MPS_ARCH_I3"
#endif


/* Prmci3AddressHoldingReg -- return an address of a register in a context */

MRef Prmci3AddressHoldingReg(MutatorContext context, unsigned int regnum)
{
  MRef gregs;

  AVERT(MutatorContext, context);
  AVER(NONNEGATIVE(regnum));
  AVER(regnum <= 7);

  /* TODO: The current arrangement of the fix operation (taking a Ref *)
     forces us to pun these registers (actually `int` on LII3GC).  We can
     suppress the warning by casting through `void *` and this might make
     it safe, but does it really?  RB 2012-09-10 */
  AVER(sizeof(void *) == sizeof(*context->ucontext->uc_mcontext.gregs));
  gregs = (void *)context->ucontext->uc_mcontext.gregs;

  /* .source.i486 */
  /* .assume.regref */
  /* The register numbers (REG_EAX etc.) are defined in <ucontext.h>
     but only if _GNU_SOURCE is defined: see .feature.li in
     config.h. */
  switch (regnum) {
    case 0: return &gregs[REG_EAX];
    case 1: return &gregs[REG_ECX];
    case 2: return &gregs[REG_EDX];
    case 3: return &gregs[REG_EBX];
    case 4: return &gregs[REG_ESP];
    case 5: return &gregs[REG_EBP];
    case 6: return &gregs[REG_ESI];
    case 7: return &gregs[REG_EDI];
    default:
      NOTREACHED;
      return NULL;  /* Avoids compiler warning. */
  }
}


/* Prmci3DecodeFaultContext -- decode fault to find faulting address and IP */

void Prmci3DecodeFaultContext(MRef *faultmemReturn,
                              Byte **insvecReturn,
                              MutatorContext context)
{
  AVER(faultmemReturn != NULL);
  AVER(insvecReturn != NULL);
  AVERT(MutatorContext, context);
  AVER(context->var == MutatorContextFAULT);

  /* .source.linux.kernel (linux/arch/i386/mm/fault.c). */
  *faultmemReturn = (MRef)context->info->si_addr;
  *insvecReturn = (Byte*)context->ucontext->uc_mcontext.gregs[REG_EIP];
}


/* Prmci3StepOverIns -- modify context to step over instruction */

void Prmci3StepOverIns(MutatorContext context, Size inslen)
{
  AVERT(MutatorContext, context);

  context->ucontext->uc_mcontext.gregs[REG_EIP] += (unsigned long)inslen;
}


Addr MutatorContextSP(MutatorContext context)
{
  AVERT(MutatorContext, context);

  return (Addr)context->ucontext->uc_mcontext.gregs[REG_ESP];
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
