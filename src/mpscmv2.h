/* impl.h.mpscmv2: MEMORY POOL SYSTEM CLASS "MV2"
 *
 * $HopeName: MMsrc!mpscmv2.h(MMdevel_gavinm_splay.1) $
 * Copyright (C) 1998 Harlequin Group plc. All rights reserved.
 */

#ifndef mpscmv2_h
#define mpscmv2_h

#include "mps.h"

/* The mv2 pool class has four extra parameters to mps_pool_create:
 *  mps_res_t mps_pool_create(mps_pool_t * pool, mps_arena_t arena,
 *                            mps_class_t mv2_class,
 *                            size_t min_size,
 *                            size_t median_size,
 *                            size_t maximum_size,
 *                            mps_count_t reserve_depth);
 * min_, median_, and maximum_size are the mimimum, median, and
 * maximum (typical) size of objects expected to be allocated in the
 * pool.  reserve_depth is a measure of the expected hysteresis of the
 * object population.
 */
extern mps_class_t mps_class_mv2(void);

#endif /* mpscmv2_h */
