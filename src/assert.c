/* impl.c.assert: ASSERTION IMPLEMENTATION
 *
 * $HopeName: MMsrc!assert.c(MMdevel_lib.2) $
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


static void AssertLib(const char *cond, const char *id,
                      const char *file, unsigned line)
{
  mps_lib_fputs("\nMPS ASSERTION FAILURE\n"
                "\nId:        ", mps_lib_stderr);
  mps_lib_fputs(id, mps_lib_stderr);
  mps_lib_fputs("\nFile:      ", mps_lib_stderr);
  mps_lib_fputs(file, mps_lib_stderr);
  mps_lib_fputs("\nLine:      ", mps_lib_stderr);
  WriteWord(mps_lib_stderr, (Word)line, 10, 0);
  mps_lib_fputs("\nCondition: ", mps_lib_stderr);
  mps_lib_fputs(cond, mps_lib_stderr);
  mps_lib_fputs("\n\n", mps_lib_stderr);
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

void AssertFail(const char *cond, const char *id,
                const char *file, unsigned line)
{
  if(handler != NULL)
    (*handler)(cond, id, file, line);
}
