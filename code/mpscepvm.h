/* impl.h.mpscepvm: MEMORY POOL SYSTEM CLASS "EPVM"
 *
 * $HopeName: MMsrc!mpscepvm.h(trunk.2) $
 * Copyright (C) 1997, 1998 The Harlequin Group Limited.  All rights reserved.
 */

#ifndef mpscepvm_h
#define mpscepvm_h

#include "mps.h"


/* this concrete type is used for save levels in the interface */
typedef unsigned int mps_epvm_save_level_t;


extern mps_class_t mps_class_epvm(void);
/* This pool class has three extra parameters to mps_pool_create:
 *  mps_res_t mps_pool_create(mps_pool_t * pool, mps_arena_t arena,
 *                            mps_class_t epvm_class, mps_fmt_t format,
 *                            mps_epvm_save_level_t max_save,
 *                            mps_epvm_save_level_t init_save);
 * being the format used for the objects, the maximum save level and the
 * initial save level.  There's one extra parameter for mps_ap_create:
 *  mps_res_t mps_ap_create(mps_ap_t *ap, mps_pool_t pool,
 *                          mps_bool_t is_obj);
 * Specifying true for is_obj means you want to allocate objects with
 * references of RANKExact in them, false means only objects with no
 * references in them.
 */


extern mps_bool_t mps_epvm_check(mps_pool_t* /* pool (output) */,
				 mps_epvm_save_level_t* /* (output) */,
				 mps_arena_t /* arena */,
				 mps_addr_t /* address */);

extern void mps_epvm_save(mps_pool_t /* pool */);

extern void mps_epvm_restore(mps_pool_t /* pool */,
			     mps_epvm_save_level_t /* save_level */);

extern size_t mps_epvm_size(mps_pool_t /* pool */);
extern size_t mps_epvm_free_size(mps_pool_t /* pool */);

extern mps_res_t mps_epvm_collect(mps_pool_t /* pool */);


#endif /* mpscepvm_h */
