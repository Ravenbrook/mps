/* impl.c.vman: ANSI VM: MALLOC-BASED PSUEDO MEMORY MAPPING
 *
 * $HopeName: MMsrc!vman.c(MMdevel_assertid.2) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 */

#include "mpm.h"

#ifdef VM_RM
#error "vman.c compiled with VM_RM set"
#endif /* VM_RM */

#include <stdlib.h>     /* for malloc and free */
#include <string.h>     /* for memset */

SRCID(vman, "$HopeName: MMsrc!vman.c(MMdevel_assertid.2) $");

#define SpaceVM(_space) (&(_space)->arenaStruct.vmStruct)

Bool VMCheck(VM vm)
{
  CHECKS(0xF3A40000, VM, vm);
  CHECKL(0xF3A40001, vm->base != (Addr)0);
  CHECKL(0xF3A40002, vm->limit != (Addr)0);
  CHECKL(0xF3A40003, vm->base < vm->limit);
  CHECKL(0xF3A40004, AddrIsAligned(vm->base, VMAN_ALIGN));
  CHECKL(0xF3A40005, AddrIsAligned(vm->limit, VMAN_ALIGN));
  CHECKL(0xF3A40006, vm->block != NULL);
  CHECKL(0xF3A40007, (Addr)vm->block <= vm->base);
  CHECKL(0xF3A40008, vm->mapped <= vm->reserved);
  return TRUE;
}

Align VMAlign()
{
  return VMAN_ALIGN;
}


Res VMCreate(Space *spaceReturn, Size size, Addr base)
{
  Space space;
  VM vm;

  AVER(0xF3A40009, spaceReturn != NULL);
  AVER(0xF3A4000A, size != 0);
  AVER(0xF3A4000B, base == NULL);

  space = (Space)malloc(sizeof(SpaceStruct));
  if(space == NULL)
    return ResMEMORY;
  vm = SpaceVM(space);

  /* Note that because we add VMAN_ALIGN rather than */
  /* VMAN_ALIGN-1 we are not in danger of overflowing */
  /* vm->limit even if malloc were peverse enough to give us */
  /* a block at the end of memory. */

  vm->block = malloc((Size)(size + VMAN_ALIGN));
  if(vm->block == NULL) {
    free(space);
    return ResMEMORY;
  }

  vm->base  = AddrAlignUp((Addr)vm->block, VMAN_ALIGN);
  vm->limit = AddrAdd(vm->base, size);
  AVER(0xF3A4000C, vm->limit < AddrAdd((Addr)vm->block, size + VMAN_ALIGN));

  memset((void *)vm->base, VM_JUNKBYTE, size);
  
  /* Lie about the reserved address space, to simulate real */
  /* virtual memory. */
  vm->reserved = size;
  vm->mapped = (Size)0;
  
  vm->sig = VMSig;

  AVERT(0xF3A4000D, VM, vm);
  
  EVENT4(VMCreate, vm, space, vm->base, vm->limit);

  *spaceReturn = space;  
  return ResOK;
}

void VMDestroy(Space space)
{
  VM vm = SpaceVM(space);
  
  /* All vm areas should have been unmapped. */
  AVER(0xF3A4000E, vm->mapped == (Size)0);
  AVER(0xF3A4000F, vm->reserved == AddrOffset(vm->base, vm->limit));

  memset((void *)vm->base, VM_JUNKBYTE, AddrOffset(vm->base, vm->limit));
  free(vm->block);
  
  vm->sig = SigInvalid;
  free(space);
  
  EVENT1(VMDestroy, vm);
}

Addr (VMBase)(Space space)
{
  VM vm = SpaceVM(space);
  return vm->base;
}

Addr (VMLimit)(Space space)
{
  VM vm = SpaceVM(space);
  return vm->limit;
}


Size VMReserved(Space space)
{
  VM vm = SpaceVM(space);
  AVERT(0xF3A40010, VM, vm);
  return vm->reserved;
}

Size VMMapped(Space space)
{
  VM vm = SpaceVM(space);
  AVERT(0xF3A40011, VM, vm);
  return vm->mapped;
}


Res VMMap(Space space, Addr base, Addr limit)
{
  VM vm = SpaceVM(space);
  Size size;

  AVER(0xF3A40012, base != (Addr)0);
  AVER(0xF3A40013, vm->base <= base);
  AVER(0xF3A40014, base < limit);
  AVER(0xF3A40015, limit <= vm->limit);
  AVER(0xF3A40016, AddrIsAligned(base, VMAN_ALIGN));
  AVER(0xF3A40017, AddrIsAligned(limit, VMAN_ALIGN));
  
  size = AddrOffset(base, limit);
  memset((void *)base, (int)0, size);
  
  vm->mapped += size;

  EVENT3(VMMap, vm, base, limit);

  return ResOK;
}

void VMUnmap(Space space, Addr base, Addr limit)
{
  VM vm = SpaceVM(space);
  Size size;

  AVER(0xF3A40018, base != (Addr)0);
  AVER(0xF3A40019, vm->base <= base);
  AVER(0xF3A4001A, base < limit);
  AVER(0xF3A4001B, limit <= vm->limit);
  AVER(0xF3A4001C, AddrIsAligned(base, VMAN_ALIGN));
  AVER(0xF3A4001D, AddrIsAligned(limit, VMAN_ALIGN));
  
  size = AddrOffset(base, limit);
  memset((void *)base, VM_JUNKBYTE, size);

  AVER(0xF3A4001E, vm->mapped >= size);
  vm->mapped -= size;

  EVENT3(VMUnmap, vm, base, limit);
}
