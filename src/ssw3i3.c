/* impl.c.ssw3i3: WIN32/INTEL STACK SCANNING
 *
 * $HopeName: $
 * Copyright (C) 1999.  Harlequin Group plc.  All rights reserved.
 *
 *  This scans the stack and fixes the registers which may contain 
 *  roots.  See design.mps.thread-manager
 *
 *  The registers edi, esi, ebx are the registers defined to be preserved
 *  across function calls and therefore may contain roots.
 *  These are pushed on the stack for scanning.
 *
 * ASSUMPTIONS
 *
 * .align: The stack pointer is assumed to be aligned on a word
 * boundary.
 */


#include "mpm.h"

SRCID(ssw3i3, "$HopeName: MMsrc!ssw3i3.c(MM_dylan_jackdaw.1) $");


Res StackScan(ScanState ss, Addr *stackBot)
{
  Addr *stackTop;

  __asm {
    push edi           /* these registers are the save registers  */
    push esi           /* and so may contain roots.  They are pushed */
    push ebx           /* for scanning */
    mov stackTop, esp  /* stack pointer */
  }

  AVER(AddrIsAligned((Addr)stackTop, sizeof(Addr)));  /* .align */

  return TraceScanArea(ss, stackTop, stackBot);
}
