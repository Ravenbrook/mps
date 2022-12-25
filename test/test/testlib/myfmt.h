/* $Id$
myfmt.h
   a format for scannable objects
*/

#ifndef myfmt_h
#define myfmt_h

#include "testlib.h"

/* some options on the format are controlled by global
   variables. Of course for efficiency we'd do it in the
   pre-processor, but that would require recompilation...

 variable      default function

 formatcomments   1   print comments on scanning, fixing
*/

extern int formatcomments;

typedef struct mycell
  {
   mps_word_t    tag;
   mps_word_t    data;
   mps_word_t    size;
   struct mycell *ref[2];
  } mycell;

/* we don't have a separate type for leaf nodes;
   instead the scanning function doesn't fix null refs
*/

mycell *allocone(mps_ap_t ap, mps_word_t data,
 mycell *ref0, mycell *ref1, size_t size);

mycell *allocheader(mps_ap_t ap, mps_word_t data,
 mycell *ref0, mycell *ref1, size_t size, size_t header);

mps_res_t make_format(mps_fmt_t *fmt_o, mps_arena_t arena);
mps_res_t make_format_header(mps_fmt_t *fmt_o, mps_arena_t arena, size_t header);

#endif
