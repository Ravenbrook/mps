/* impl.c.testlib: Test library
 *
 * $HopeName: MMsrc!testlib.c(MMdevel_ramp_alloc.1) $
 *
 * Copyright (C) 1995, 1998 Harlequin Group, all rights reserved
 *
 * .purpose: A library of functions that may be of use to unit tests.
 */

#include "mps.h"
#include "testlib.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif
#include "mpm.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>


/* rnd -- a random number generator
 *
 * I nabbed it from "ML for the Working Programmer"
 * Originally from:
 * Stephen K Park & Keith W Miller (1988). Random number generators:
 * good one are to find.  Communications of the ACM, 31:1192-1201
 */
unsigned long rnd(void)
{
  static unsigned long seed = 1;
  double s;
  s = seed;
  s *= 16807.0;
  s = fmod(s, 2147483647.0);  /* 2^31 - 1 */
  seed = (unsigned long)s;
  return seed;
}


/* die -- Test a return code, and exit on error */
void die(mps_res_t res, const char *s)
{
  if(res != MPS_RES_OK)
  {
    fflush(stdout); /* synchronize */
    fprintf(stderr, "\n%s: %d\n", s, res);
    exit(1);
  }
}


/* adjust_collection_freq -- multiply all collection frequencies by
 *                           a given factor
 */

void adjust_collection_freq(double multiplier)
{
  AMCGen0Frequency *= multiplier;
  if(AMCGen0Frequency == 0) AMCGen0Frequency = 1;
  AMCGen1Frequency *= multiplier;
  AMCGen2Frequency *= multiplier;
  AMCGen2plusFrequencyMultiplier *= multiplier;
  AMCGen0RampmodeFrequency *= multiplier;
  if(AMCGen0RampmodeFrequency == 0) AMCGen0RampmodeFrequency = 1;
  AMCGen1RampmodeFrequency *= multiplier;
  AMCRampGenFrequency *= multiplier;
  AMCGen2RampmodeFrequency *= multiplier;
  AMCGen2plusRampmodeFrequencyMultiplier *= multiplier;
}
