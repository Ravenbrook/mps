/*  impl.c.ref
 *
 *                    REFERENCES
 *
 *  $HopeName: !ref.c(trunk.3) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 */

#include "std.h"
#include "ref.h"

SRCID("$HopeName: !ref.c(trunk.3) $");


#ifdef DEBUG

Bool RefRankIsValid(RefRank rank, ValidationType validParam)
{
  AVER(rank >= 0);
  AVER(rank < RefRankMAX);
  return TRUE;
}

#endif /* DEBUG */
