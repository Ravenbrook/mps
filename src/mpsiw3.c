/* impl.c.mpsint:
 *
 *       WIN32 MEMORY POOL SYSTEM INTERFACE LAYER EXTRAS
 *
 *  $HopeName: !mpsiw3.c(MM_dylan_honeybee.1) $
 *
 *  Copyright (C) 1996 Harlequin Group, all rights reserved
 */

#include "mpm.h"
#include "mps.h"
#include <windows.h>

SRCID(mpsint, "$HopeName: !mpsiw3.c(MM_dylan_honeybee.1) $");

/* This is defined in protnt.c */
extern LONG ProtSEHfilter(LPEXCEPTION_POINTERS info);

LONG mps_SEH_filter(LPEXCEPTION_POINTERS info,
                    void **hp_o, size_t *hs_o)
{
  UNUSED(hp_o);
  UNUSED(hs_o);
  return ProtSEHfilter(info);
}

void mps_SEH_handler(void *p, size_t s)
{
  NOTREACHED;
}
