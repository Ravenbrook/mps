/* mpsliban.c: RAVENBROOK MEMORY POOL SYSTEM LIBRARY INTERFACE (ANSI)
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * .purpose: The purpose of this code is
 *   1. to connect the MPS Library Interface to the ANSI C libraries,
 *      where they exist, and
 *   2. to provide an example of how to implement the MPS Library
 *      Interface.
 *
 * .readership: For MPS client application developers and MPS developers.
 * .sources: <design/lib/>
 *
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * .trans.file: The ANSI standard says (in section 7.9.1) that FILE is an
 * object type, and hence the casts between FILE and mps_lib_FILE (an
 * incomplete type) are not necessarily valid.  We assume that this trick
 * works, however, in all current environments.
 */

#include "mpslib.h"

#include "mpstd.h"
#include "event.h"

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>


int mps_lib_get_EOF(void)
{
  return EOF;
}

mps_lib_FILE *mps_lib_get_stderr(void)
{
  return (mps_lib_FILE *)stderr; /* see .trans.file */
}

mps_lib_FILE *mps_lib_get_stdout(void)
{
  return (mps_lib_FILE *)stdout; /* see .trans.file */
}

int mps_lib_fputc(int c, mps_lib_FILE *stream)
{
  return fputc(c, (FILE *)stream); /* see .trans.file */
}

int mps_lib_fputs(const char *s, mps_lib_FILE *stream)
{
  return fputs(s, (FILE *)stream); /* see .trans.file */
}


static void mps_lib_assert_fail_default(const char *file, unsigned line,
                                        const char *condition)
{
  /* Synchronize with stdout. */
  (void)fflush(stdout);
  (void)fprintf(stderr,
                "The MPS detected a problem!\n"
                "%s:%u: MPS ASSERTION FAILED: %s\n"
                "See the \"Assertions\" section in the reference manual:\n"
                "http://www.ravenbrook.com/project/mps/master/manual/html/topic/error.html#assertions\n",
                file, line, condition);
  /* Ensure the message is output even if stderr is buffered. */
  (void)fflush(stderr);
  mps_telemetry_flush();
  ASSERT_ABORT(); /* see config.h */
}

static mps_lib_assert_fail_t mps_lib_assert_handler = mps_lib_assert_fail_default;

void mps_lib_assert_fail(const char *file,
                         unsigned line,
                         const char *condition)
{
  mps_lib_assert_handler(file, line, condition);
}

mps_lib_assert_fail_t mps_lib_assert_fail_install(mps_lib_assert_fail_t handler)
{
  mps_lib_assert_fail_t old_handler = mps_lib_assert_handler;
  mps_lib_assert_handler = handler;
  return old_handler;
}


void *(mps_lib_memset)(void *s, int c, size_t n)
{
  return memset(s, c, n);
}

void *(mps_lib_memcpy)(void *s1, const void *s2, size_t n)
{
  return memcpy(s1, s2, n);
}

int (mps_lib_memcmp)(const void *s1, const void *s2, size_t n)
{
  return memcmp(s1, s2, n);
}


/* If your platform has a low-resolution clock(), and there are
 * higher-resolution clocks readily available, then using one of those
 * will improve MPS scheduling decisions and the quality of telemetry
 * output.  For instance, with getrusage():
 * 
 *   #include <sys/resource.h>
 *   struct rusage s;
 *   int res = getrusage(RUSAGE_SELF, &s);
 *   if (res != 0) {
 *     ...
 *   }
 *   return ((mps_clock_t)s.ru_utime.tv_sec) * 1000000 + s.ru_utime.tv_usec;
 */

mps_clock_t mps_clock(void)
{
  /* The clock values need to fit in mps_clock_t.  If your platform
     has a very wide clock type, trim or truncate it. */
  assert(sizeof(mps_clock_t) >= sizeof(clock_t));

  return (mps_clock_t)clock();
}


mps_clock_t mps_clocks_per_sec(void)
{
  /* must correspond to whatever mps_clock() does */
  return (mps_clock_t)CLOCKS_PER_SEC;
}


/* mps_lib_telemetry_control -- get and interpret MPS_TELEMETRY_CONTROL */

#ifdef MPS_BUILD_MV
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: getenv.  See job001934. */
#pragma warning( disable : 4996 )
#endif

/* Simple case-insensitive string comparison */
static int striequal(const char *s0, const char *s1)
{
  int c;
  do {
    c = *s0;
    if (tolower(c) != tolower(*s1)) /* note: works for '\0' */
      return 0;
    ++s0;
    ++s1;
  } while (c != '\0');
  return 1;
}

unsigned long mps_lib_telemetry_control(void)
{
  char *s;
  char **null = NULL;
  unsigned long mask;
  char buf[256];
  char *word;
  const char *sep = " ";

  s = getenv("MPS_TELEMETRY_CONTROL");
  if (s == NULL)
    return 0;

  /* If the value can be read as a number, use it. */
  mask = strtoul(s, null, 0);
  if (mask != 0)
    return mask;

  /* copy the envar to a buffer so we can mess with it. */
  strncpy(buf, s, sizeof(buf) - 1);
  buf[sizeof(buf) - 1] = '\0';
  
  /* Split the value at spaces and try to match the words against the names
     of event kinds, enabling them if there's a match. */
  for (word = strtok(buf, sep); word != NULL; word = strtok(NULL, sep)) {
    if (striequal(word, "all")) {
      mask = (unsigned long)-1;
      return mask;
    }
#define TELEMATCH(X, name, rowDoc) \
    if (striequal(word, #name)) \
      mask |= (1ul << EventKind##name);
    EventKindENUM(TELEMATCH, X)
#undef TELEMATCH
  }
  
  return mask;
}


/* FIXME -- interpret keyword arguments from environment variables */

static mps_bool_t parse_size(mps_arg_s *arg, const char *s)
{
  char *end;
  arg->val.size = strtoul(s, &end, 10);
  /* TODO: KMGTPE suffixes */
  return end > s && *end == '\0';
}

static mps_bool_t parse_b(mps_arg_s *arg, const char *s)
{
  struct {
    const char *string;
    mps_bool_t value;
  } map[] = {
    {"true", 1}, {"yes", 1}, {"1", 1},
    {"false", 0}, {"no", 0}, {"0", 0}
  };
  size_t i;
  for (i = 0; i < sizeof(map)/sizeof(map[0]); ++i)
    if (striequal(s, map[i].string)) {
      arg->val.b = map[i].value;
      return 1;
    }
  return 0;
}

static mps_bool_t parse_d(mps_arg_s *arg, const char *s)
{
  char *end;
  arg->val.d = strtod(s, &end);
  return end > s && *end == '\0';
}

static mps_bool_t parse_u(mps_arg_s *arg, const char *s)
{
  char *end;
  unsigned long ul = strtoul(s, &end, 10);
  arg->val.u = (unsigned)strtoul(s, &end, 10);
  return ul <= UINT_MAX && end > s && *end == '\0';
}

static mps_bool_t parse_align(mps_arg_s *arg, const char *s)
{
  char *end;
  arg->val.align = strtoul(s, &end, 10);
  /* TODO: KMGTPE suffixes */
  return arg->val.align > 0 &&
    (arg->val.align & (arg->val.align - 1)) == 0 &&
    end > s && *end == '\0';
}

#define KEY_ROW(X, ident, kind, doc) KEY_ROW_##kind(X, ident, kind, doc)
#define KEY_ROW_YES(_, ident, kind, doc) \
  {#ident, MPS_KEY_##ident, parse_##kind, doc},
#define KEY_ROW_NO(_, ident, kind, doc)
#define KEY_ROW_size KEY_ROW_YES
#define KEY_ROW_b KEY_ROW_YES
#define KEY_ROW_d KEY_ROW_YES
#define KEY_ROW_format KEY_ROW_NO
#define KEY_ROW_chain KEY_ROW_NO
#define KEY_ROW_u KEY_ROW_YES
#define KEY_ROW_rank KEY_ROW_NO
#define KEY_ROW_align KEY_ROW_YES
#define KEY_ROW_fmt_scan KEY_ROW_NO
#define KEY_ROW_fmt_skip KEY_ROW_NO
#define KEY_ROW_fmt_fwd KEY_ROW_NO
#define KEY_ROW_fmt_isfwd KEY_ROW_NO
#define KEY_ROW_fmt_pad KEY_ROW_NO
#define KEY_ROW_fmt_class KEY_ROW_NO

static struct {
  const char *ident;
  mps_key_t key;
  mps_bool_t (*parse)(mps_arg_s *arg, const char *s);
  const char *doc;
} keywords[] = {
  _mps_KEY_ENUM(KEY_ROW, _)
};


/* parse_setting -- parse a keyword assignment setting
 *
 * Parse a string like "SIZE=16K" into a keyword argument.
 */

static mps_res_t parse_setting(mps_arg_s *arg_out, const char *opt)
{
  char *val;
  size_t i;
  val = strchr(opt, '=');
  if (val == NULL)
    return MPS_RES_PARAM;
  *val++ = '\0';
  for (i = 0; i < sizeof(keywords)/sizeof(keywords[0]); ++i)
    if (striequal(opt, keywords[i].ident))
      goto found;
  return MPS_RES_PARAM;
found:
  if (!keywords[i].parse(arg_out, val))
    return MPS_RES_PARAM;
  arg_out->key = keywords[i].key;
  return MPS_RES_OK;
}


/* parse_settings -- parse multiple settings into keyword arguments
 *
 * Parses a string like "SIZE=16K ZONED=yes" into an argument array.
 * Ignores unparseable arguments.  Uses no more than length array entries.
 * Returns number of arguments parsed.
 */

static unsigned parse_settings(mps_arg_s args[], unsigned length, char *opts)
{
  unsigned count = 0;
  char *word;
  const char *sep = " ";
  for (word = strtok(opts, sep); word != NULL; word = strtok(NULL, opts)) {
    if (count >= length)
      return count;
    if (parse_setting(&args[count], word) == MPS_RES_OK)
      ++count;
  }
  return count;
}

static unsigned parse_envar(mps_arg_s args[], unsigned length, const char *name)
{
  return parse_settings(args, length, getenv(name));
}

static void strupper(char *s)
{
  while (*s != '\0') {
    *s = (char)toupper(*s);
    ++s;
  }
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
