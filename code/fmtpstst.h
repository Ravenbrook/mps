/* impl.h.fmtpstst: POSTSCRIPT OBJECT FORMAT TEST VERSION
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .readership: MPS developers, SW developers
 */

#ifndef fmtpstst_h
#define fmtpstst_h

#include "mps.h"
#include "fmtpscon.h"


extern mps_fmt_A_s *ps_fmt_A(void);

extern mps_res_t ps_scan(mps_ss_t scan_state,
                         mps_addr_t base, mps_addr_t limit);

extern mps_res_t ps_string_init(OBJECT *arr, mps_addr_t p, uint16 length);

extern mps_res_t ps_array_init(OBJECT *arr, mps_addr_t p, uint16 length,
                               OBJECT *refs, size_t nr_refs);

extern void ps_write(OBJECT *obj, OBJECT *refs, size_t nr_refs);

extern int ps_check(OBJECT *obj);


#endif /* protection for multiple inclusion */
