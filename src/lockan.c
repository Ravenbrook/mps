/*  impl.c.lockan
 *
 *                  ANSI RECURSIVE LOCKS
 *
 *  $HopeName: MMsrc!lockan.c(MMdevel_assertid.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a trivial implemenation of recursive locks
 *  that assumes we are not running in a multi-threaded
 *  environment.
 *
 *  This provides stubs for the locking code where locking
 *  is not applicable.  The stubs provide some amount of
 *  checking.
 *
 *  The limit on the number of recursive claims is ULONG_MAX.
 */

#include "mpm.h"

SRCID(lockan, "$HopeName: MMsrc!lockan.c(MMdevel_assertid.2) $");

Bool LockCheck(Lock lock)
{
  CHECKS(0x7CA40000, Lock, lock);
  return TRUE;
}

void LockInit(Lock lock)
{
  AVER(0x7CA40001, lock != NULL);
  lock->claims = 0;
  lock->sig = LockSig;
  AVERT(0x7CA40002, Lock, lock);
}

void LockFinish(Lock lock)
{
  AVERT(0x7CA40003, Lock, lock);
  AVER(0x7CA40004, lock->claims == 0);
  lock->sig = SigInvalid;
}

void LockClaim(Lock lock)
{
  AVERT(0x7CA40005, Lock, lock);
  AVER(0x7CA40006, lock->claims == 0);
  lock->claims = 1;
}

void LockReleaseMPM(Lock lock)
{
  AVERT(0x7CA40007, Lock, lock);
  AVER(0x7CA40008, lock->claims == 1);
  lock->claims = 0;
}

void LockClaimRecursive(Lock lock)
{
  AVERT(0x7CA40009, Lock, lock);
  ++lock->claims;
  AVER(0x7CA4000A, lock->claims>0);
}

void LockReleaseRecursive(Lock lock)
{
  AVERT(0x7CA4000B, Lock, lock);
  AVER(0x7CA4000C, lock->claims > 0);
  --lock->claims;
}
