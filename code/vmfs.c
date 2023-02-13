/* vmfs.c: FREESTANDING VM: VIRTUAL MEMORY MAPPING STUB
 *
 * $Id$
 * Copyright (c) 2001-2023 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: The purpose of this code is to test at compile time
 * whether the code MPS can be compiled as freestanding [FIXME: ref?]
 * with a command like::
 *
 *   gcc -nostdlib -DCONFIG_PLINTH_NONE -DCONFIG_PF_ANSI \
 *       --entry mps_lib_assert_fail mps.c mpslibfs.c vmfs.c
 *
 * .sources: design.mps.lib
 */

#include "mpm.h"
#include "vm.h"

SRCID(vmfs, "$Id$");


Size PageSize(void)
{
  NOTREACHED;
}


Res VMParamFromArgs(void *params, size_t paramSize, ArgList args)
{
  NOTREACHED;
}


/* VMInit -- reserve some virtual address space, and create a VM structure */

Res VMInit(VM vm, Size size, Size grainSize, void *params)
{
  NOTREACHED;
}


/* VMFinish -- release all address space and finish VM structure */

void VMFinish(VM vm)
{
  NOTREACHED;
}


/* VMMap -- map the given range of memory */

Res VMMap(VM vm, Addr base, Addr limit)
{
  NOTREACHED;
}


/* VMUnmap -- unmap the given range of memory */

void VMUnmap(VM vm, Addr base, Addr limit)
{
  NOTREACHED;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2023 Ravenbrook Limited <https://www.ravenbrook.com/>.
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
