/* impl.c.vman: ANSI VM: MALLOC-BASED PSUEDO MEMORY MAPPING
 *
 * $HopeName: MMsrc!vman.c(trunk.15) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 */

#include "mpm.h"

#ifdef VM_RM
#error "vman.c compiled with VM_RM set"
#endif /* VM_RM */

#include <stdlib.h>     /* for malloc and free */
#include <string.h>     /* for memset */

SRCID(vman, "$HopeName: MMsrc!vman.c(trunk.15) $");

Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->base != (Addr)0);
  CHECKL(vm->limit != (Addr)0);
  CHECKL(vm->base < vm->limit);
  CHECKL(AddrIsAligned(vm->base, VMAN_ALIGN));
  CHECKL(AddrIsAligned(vm->limit, VMAN_ALIGN));
  CHECKL(vm->block != NULL);
  CHECKL((Addr)vm->block <= vm->base);
  CHECKL(vm->mapped <= vm->reserved);
  return TRUE;
}

Align VMAlign()
{
  return VMAN_ALIGN;
}


Res VMInit(VM vm, Size size, Addr base)
{
  AVER(vm != NULL);
  AVER(size != 0);
  AVER(base == NULL);

  /* Note that because we add VMAN_ALIGN rather than */
  /* VMAN_ALIGN-1 we are not in danger of overflowing */
  /* vm->limit even if malloc were peverse enough to give us */
  /* a block at the end of memory. */

  vm->block = malloc((Size)(size + VMAN_ALIGN));
  if(vm->block == NULL)
    return ResMEMORY;

  vm->base  = AddrAlignUp((Addr)vm->block, VMAN_ALIGN);
  vm->limit = AddrAdd(vm->base, size);
  AVER(vm->limit < AddrAdd((Addr)vm->block, size + VMAN_ALIGN));

  memset((void *)vm->base, VM_JUNKBYTE, size);
  
  /* Lie about the reserved address space, to simulate real */
  /* virtual memory. */
  vm->reserved = size;
  vm->mapped = (Size)0;
  
  vm->sig = VMSig;

  AVERT(VM, vm);
  
  EVENT4(VMCreate, vm, arena, vm->base, vm->limit);

  return ResOK;
}

void VMFinish(VM vm)
{
  AVERT(VM, vm);

  /* All vm areas should have been unmapped. */
  AVER(vm->mapped == (Size)0);
  AVER(vm->reserved == AddrOffset(vm->base, vm->limit));

  memset((void *)vm->base, VM_JUNKBYTE, AddrOffset(vm->base, vm->limit));
  free(vm->block);
  
  vm->sig = SigInvalid;
  
  EVENT1(VMDestroy, vm);
}

Addr (VMBase)(VM vm)
{
  AVERT(VM, vm);
  return vm->base;
}

Addr (VMLimit)(VM vm)
{
  AVERT(VM, vm);
  return vm->limit;
}


Size VMReserved(VM vm)
{
  AVERT(VM, vm);
  return vm->reserved;
}

Size VMMapped(VM vm)
{
  AVERT(VM, vm);
  return vm->mapped;
}


Res VMMap(VM vm, Addr base, Addr limit)
{
  Size size;

  AVERT(VM, vm);
  AVER(base != (Addr)0);
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, VMAN_ALIGN));
  AVER(AddrIsAligned(limit, VMAN_ALIGN));
  
  size = AddrOffset(base, limit);
  memset((void *)base, (int)0, size);
  
  vm->mapped += size;

  EVENT3(VMMap, vm, base, limit);

  return ResOK;
}

void VMUnmap(VM vm, Addr base, Addr limit)
{
  Size size;

  AVERT(VM, vm);
  AVER(base != (Addr)0);
  AVER(vm->base <= base);
  AVER(base < limit);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, VMAN_ALIGN));
  AVER(AddrIsAligned(limit, VMAN_ALIGN));
  
  size = AddrOffset(base, limit);
  memset((void *)base, VM_JUNKBYTE, size);

  AVER(vm->mapped >= size);
  vm->mapped -= size;

  EVENT3(VMUnmap, vm, base, limit);
}
