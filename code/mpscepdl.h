/* impl.h.mpscepdl: MEMORY POOL SYSTEM CLASS "EPDL"
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 */

#ifndef mpscepdl_h
#define mpscepdl_h

#include "mps.h"

extern size_t mps_epdl_free_size(mps_pool_t mps_pool);
extern size_t mps_epdl_size(mps_pool_t mps_pool);
extern mps_class_t mps_class_epdl(void);
extern mps_class_t mps_class_epdr(void);
extern mps_class_t mps_class_epdl_debug(void);
extern mps_class_t mps_class_epdr_debug(void);

#endif /* mpscepdl_h */
