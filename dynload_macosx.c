/* dynload_macosx.c -- dynamic loader for Mac OS X
 *
 * Richard Brooksby, 2004-07-29.  See end of file for licence.
 *
 * This file implements the dynamic loading of compiled units.
 *
 * See "Loading Plug-In Code With Bundles" [Apple 2003-08-07, pp25-26] for
 * background and references for the various Mac OS X specific functions.
 *
 *
 * NOTES
 *
 * 1. Should we be using CFBundle
 * <http://developer.apple.com/documentation/CoreFoundation/Conceptual/CFBundles/>?
 * A higher level interface, but probably more Mac-like and less portable.
 *
 * 2. Should un-link, unload, etc. when an error occurs.
 *
 * 3. Could we create a finalized proxy object that represents the loaded unit,
 * so that it can be unloaded when the last reference dies?  Otherwise the
 * process will grow monotonically, which could be a problem for indefinitely
 * long lived processes.
 *
 * 4. How do we avoid loading the same module more than once?
 */
 
#include "sc.h"

#include <string.h>


/* On Mac OS X 10.3.4, at least, there are header which use a non-standard */
/* declaration "inline".  Fortunately, we can switch it off by defining */
/* OS_INLINE as blank, and so keep all our strict compiler warnings. */

#define OS_INLINE
#include <mach-o/dyld.h>
#undef OS_INLINE


/* load-compiled-unit -- loads a bundle and returns its top-level linker
 *
 * (load-compiled-unit <symbol> <string>)
 */

static void load_compiled_unit_entry(state_t state)
{
  NSObjectFileImageReturnCode rc;
  NSObjectFileImage image;
  NSModule module;
  NSSymbol symbol;
  label_t *unit;

  check_args(state, 2, TYPE_SYMBOL, TYPE_STRING);

  rc = NSCreateObjectFileImageFromFile(STR(A1), &image);
  if(rc != NSObjectFileImageSuccess)
    /* @@@@ Need to decode return code and produce better messages. */
    error(state, "couldn't create image object: %d", rc);

  module = NSLinkModule(image, STR(A1),
                        NSLINKMODULE_OPTION_BINDNOW |
                        NSLINKMODULE_OPTION_RETURN_ON_ERROR |
                        NSLINKMODULE_OPTION_PRIVATE);
  if(module == NULL)
    /* @@@@ Need to decode "errno" thingy and produce better messages. */
    error(state, "couldn't link module");

  /* The name of the file's entry point is determined by its symbolic name. */
  /* See "opcodes.h" macro "UNIT_BEGIN". */
  /* @@@@ Should perhaps have a scheme for translating funny characters in */
  /* the symbol. */
  MAKE_STRING_UNINIT(T1, sizeof "_sc_unit_" - 1 + SYMLEN(A0));
  strcpy(STR(T1), "_sc_unit_");
  strncat(STR(T1), SYMSTR(A0), SYMLEN(A0));

  symbol = NSLookupSymbolInModule(module, STR(T1));
  if(symbol == NULL)
    error(state, "couldn't find entry point");

  unit = NSAddressOfSymbol(symbol);

  /* Make a procedure to run the unit top-level linker. */
  MAKE_PROC(T0, A0, *unit);

  RET1(T0);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"load-compiled-unit", load_compiled_unit_entry},
  {NULL, NULL}
};


/* syntab -- syntax table
 *
 * This table used by "unit_entry" to create the initial syntax environment.
 */

static const proctab_s syntab[] = {
  {NULL, NULL}
};


static void unit_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR); /* the environment */
  bind_procs(state, proctab, syntab);
  RET0();
}


const label_t sc_unit_scdynload = &unit_entry;


/* A. REFERENCES
 *
 * [Apple 2003-08-07] "Mach-O Runtime Architecture"; Apple Computer, Inc.;
 * 2003-08-07;
 * <http://developer.apple.com/documentation/DeveloperTools/Conceptual/MachORuntime/>.
 *
 *
 * B. DOCUMENT HISTORY
 *
 * 2004-07-29  RB  Added documentation and cross references to Apple
 * documents.
 *
 * 2004-08-09  RB  Changed to derive entry point identifier from filename.
 *
 * 2004-08-11  RB  Changing "dynload" operator to "load-compiled-unit"
 * function which can be used to implement stuff in Scheme rather than C.
 *
 *
 * C. COPYRIGHT AND LICENCE
 *
 * Copyright (C) 1997-2004 Richard Brooksby.  This document is provided
 * "as is", without any express or implied warranty.  In no event will
 * the authors be held liable for any damages arising from the use of
 * this document.  You may not duplicate or reproduce this document in
 * any form without the express permission of the copyright holder.
 *
 * Made in England.
 *
 *
 * $Id$
 */
