/* impl.c.assert: ASSERTION IMPLEMENTATION
 *
 * $HopeName: MMsrc!assert.c(MMdevel_assertid.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This source provides the AssertFail function which is
 * invoked by the assertion macros (see impl.h.assert).
 * It also provides for user-installed assertion failure handlers.
 *
 * Notes
 *
 * 3. To be really solid, assert should write the information into a
 * buffer before reaching the handler, so that it can be recovered
 * even if printing fails.  richard 1994-11-15
 */

#include "mpm.h"


static void AssertLib(const char *cond, const char *hopename,
                      const char *file, unsigned line,
                      unsigned id)
{
  WriteF(mps_lib_stderr,
         "\n"
         "PROBLEM DETECTED BY HARLEQUIN MPS\n"
         "\n"
         "ID:        $W\n", (WriteFW)id,        /* @@@@ Should be special format */
         "HopeName:  $S\n", hopename,
         "File:      $S\n", file,
         "Line:      $U\n", (WriteFU)line,
         "Condition: $S\n", cond,
         "\n",
         NULL);

  mps_lib_abort();
}

static AssertHandler handler = AssertLib;

AssertHandler AssertDefault(void)
{
  return AssertLib;
}

AssertHandler AssertInstall(AssertHandler new)
{
  AssertHandler prev = handler;
  handler = new;
  return prev;
}


/* AssertFail -- fail an assertion
 *
 * This function is called when an ASSERT macro fails a test.  It
 * calls the installed assertion handler, if it is not NULL.  If
 * handler returns the progam continues.
 */

void AssertFail(const char *cond, const char *hopename,
                const char *file, unsigned line, unsigned id)
{
  if(handler != NULL)
    (*handler)(cond, hopename, file, line, id);
}
