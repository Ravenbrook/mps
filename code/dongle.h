/* impl.h.dongle: DONGLE INTERFACE
 *
 * $Id$
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 */

#ifndef dongle_h
#define dongle_h

#include "config.h"
#include "misc.h"


#if defined(DONGLE)


extern unsigned int dongleCtr;

extern int DongleTestFull(void);
extern int DongleTest(void);

#define DONGLE_TEST_QUICK() ((--dongleCtr == 0) ? DongleTest() : TRUE)


#elif defined(DONGLE_NONE)


#define DongleTestFull() TRUE
#define DONGLE_TEST_QUICK() TRUE


#else
#error "No dongle configured."
#endif


#endif /* dongle_h */
