/* mpslibfs.c: RAVENBROOK MEMORY POOL SYSTEM LIBRARY INTERFACE (FREESTANDING)
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * .purpose: The purpose of this code is to test at compile time
 * whether the code MPS can be compiled as freestanding [FIXME: ref?]
 *
 * .readership: For MPS client application developers and MPS developers.
 * .sources: <design/lib>
 */

#include "mpslib.h"
#include "mpsio.h"

#include "mpstd.h"
#include "event.h"

int mps_lib_get_EOF(void)
{
  NOTREACHED;
}

mps_lib_FILE *mps_lib_get_stderr(void)
{
  NOTREACHED;
}

mps_lib_FILE *mps_lib_get_stdout(void)
{
  NOTREACHED;
}

int mps_lib_fputc(int c, mps_lib_FILE *stream)
{
  NOTREACHED;
}

int mps_lib_fputs(const char *s, mps_lib_FILE *stream)
{ 
  NOTREACHED;
}

void mps_lib_assert_fail(const char *file,
                          unsigned line,
                          const char *condition)
{
  for (;;)
    NOOP;
}

mps_lib_assert_fail_t mps_lib_assert_fail_install(mps_lib_assert_fail_t handler)
{
  NOTREACHED;
}


void *(mps_lib_memset)(void *s, int c, size_t n)
{
  NOTREACHED;
}

void *(mps_lib_memcpy)(void *s1, const void *s2, size_t n)
{
  NOTREACHED;
}

int (mps_lib_memcmp)(const void *s1, const void *s2, size_t n)
{
  NOTREACHED;
}

mps_clock_t mps_clock(void)
{
  NOTREACHED;
}


mps_clock_t mps_clocks_per_sec(void)
{
  NOTREACHED;
}

unsigned long mps_lib_telemetry_control(void)
{
  NOTREACHED;
}


mps_res_t mps_io_create(mps_io_t *mps_io_r)
{
  NOTREACHED;
}


void mps_io_destroy(mps_io_t mps_io)
{
  NOTREACHED;
}


mps_res_t mps_io_write(mps_io_t mps_io, void *buf, size_t size)
{
  NOTREACHED;
}


mps_res_t mps_io_flush(mps_io_t mps_io)
{
  NOTREACHED;
}


/* main -- dummy entry point
 *
 * Main is included here so that a command like::
 *
 *   gcc -nostdlib -DCONFIG_PLINTH_NONE -DCONFIG_PF_ANSI mps.c mpslibfs.c
 *
 * can run to completion of the linker and reveal external
 * dependencies.
 */

int main(void)
{
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2023 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
