/*  impl.c.protnt
 *
 *               PROTECTION FOR WIN32
 *  $HopeName: MMsrc!protnt.c(MMdevel_assertid.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 */

#include "mpm.h"

#ifndef MPS_OS_W3
#error "protnt.c is Win32 specific, but MPS_OS_W3 is not set"
#endif

#include <windows.h>

SRCID(protnt, "$HopeName: MMsrc!protnt.c(MMdevel_assertid.2) $");


void ProtSetup(void)
{
  return;
}

void ProtSet(Addr base, Addr limit, AccessSet mode)
{
  DWORD newProtect;
  DWORD oldProtect;

  AVER(0xB6420000, sizeof(int) == sizeof(Addr));
  AVER(0xB6420001, base < limit);
  AVER(0xB6420002, base != 0);

  newProtect = PAGE_EXECUTE_READWRITE;
  if((mode & AccessWRITE) != 0)
    newProtect = PAGE_EXECUTE_READ;
  if((mode & AccessREAD) != 0)
    newProtect = PAGE_NOACCESS;

  if(VirtualProtect((LPVOID)base, (DWORD)AddrOffset(base, limit),
                    newProtect, &oldProtect) == 0)
    NOTREACHED(0xB6420003);
}

LONG ProtSEHfilter(LPEXCEPTION_POINTERS info)
{
  LPEXCEPTION_RECORD er;
  DWORD iswrite;
  DWORD address;
  AccessSet mode;
  Addr base, limit;
  LONG action;

  er = info->ExceptionRecord;

  if(er->ExceptionCode != EXCEPTION_ACCESS_VIOLATION)
    return EXCEPTION_CONTINUE_SEARCH;

  AVER(0xB6420004, er->ExceptionFlags == 0); /* continuable exception */

  /* er->ExceptionRecord is pointer to next exception in chain */
  /* er->ExceptionAddress is where exception occurred */

  AVER(0xB6420005, er->NumberParameters >= 2);

  iswrite = er->ExceptionInformation[0]; /* 0 read; 1 write */
  AVER(0xB6420006, iswrite == 0 || iswrite == 1);

  /* Pages cannot be made write-only, so an attempt to write must
   * also cause a read-access if necessary */
  if(iswrite)
    mode = AccessREAD | AccessWRITE;
  else
    mode = AccessREAD;

  address = er->ExceptionInformation[1];

  base = (Addr)address;
  limit = AddrAdd(address, sizeof(Addr));

  AVER(0xB6420007, base < limit);  /* nasty case (base = -1): continue search? @@@ */

  if(SpaceAccess(base, mode))
    action = EXCEPTION_CONTINUE_EXECUTION;
  else
    action = EXCEPTION_CONTINUE_SEARCH;

  return action;
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * This does nothing under Win32.
 */

void ProtSync(Space space)
{
  NOOP;
}


void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  void *result;

  __try {
    result = f(p, s);
  } __except(ProtSEHfilter(GetExceptionInformation())) {
    NOTREACHED(0xB6420008);
  }

  *resultReturn = result;
}
