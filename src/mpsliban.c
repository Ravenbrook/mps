/* impl.c.mpsliban: HARLEQUIN MEMORY POOL SYSTEM LIBRARY INTERFACE (ANSI)
 *
 * $HopeName: MMsrc!mpsliban.c(MMdevel_lib.2) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MPS client application developers, MPS developers.
 * .sources: design.mps.lib
 */

#include "mpslib.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


int mps_lib_get_EOF(void)
{
  return EOF;
}

mps_lib_FILE *mps_lib_get_stderr(void)
{
  return (mps_lib_FILE *)stderr;
}

mps_lib_FILE *mps_lib_get_stdout(void)
{
  return (mps_lib_FILE *)stdout;
}

int mps_lib_fprintf(mps_lib_FILE *stream, const char *format, ...)
{
  int result;
  va_list args;
  va_start(args, format);
  result = vfprintf((FILE *)stream, format, args);
  va_end(args);
  return result;
}

int mps_lib_vfprintf(mps_lib_FILE *stream,
                     const char *format,
                     va_list args)
{
  return vfprintf((FILE *)stream, format, args);
}

int mps_lib_fputc(int c, mps_lib_FILE *stream)
{
  return fputc(c, (FILE *)stream);
}

int mps_lib_fputs(const char *s, mps_lib_FILE *stream)
{
  return fputs(s, (FILE *)stream);
}

void mps_lib_abort(void)
{
  abort();
}

void *mps_lib_memset(void *s, int c, size_t n)
{
  return memset(s, c, n);
}
