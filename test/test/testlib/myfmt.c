/* $Id$
myfmt.c
   a format for scannable objects
*/

#include "myfmt.h"
#include <string.h>

enum {MCpadsingle, MCpadmany, MCheart, MCdata};

/* some options on the format are controlled by global
   variables. Of course for efficiency we'd do it in the
   pre-processor, but that would require recompilation...

 variable      default function

 formatcomments   1   print comments on scanning, fixing
*/

int formatcomments=1;

/* we don't have a separate type for leaf nodes;
   instead the scanning function doesn't fix null refs
*/

static mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t myskip(mps_addr_t object);
static void myfwd(mps_addr_t object, mps_addr_t to);
static mps_addr_t myisfwd(mps_addr_t object);
static void mypad(mps_addr_t base, size_t size);

mps_res_t make_format_header(mps_fmt_t *fmt_o, mps_arena_t arena, size_t header)
{
 mps_res_t res;
 MPS_ARGS_BEGIN(args) {
  MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, myscan);
  MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, myskip);
  MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, myfwd);
  MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, myisfwd);
  MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, mypad);
  MPS_ARGS_ADD(args, MPS_KEY_FMT_HEADER_SIZE, header);
  res = mps_fmt_create_k(fmt_o, arena, args);
 } MPS_ARGS_END(args);
 return res;
}

mps_res_t make_format(mps_fmt_t *fmt_o, mps_arena_t arena)
{
  return make_format_header(fmt_o, arena, 0);
}

mycell *allocheader(mps_ap_t ap, mps_word_t data,
 mycell *ref0, mycell *ref1, size_t size, size_t header)
{
 mps_addr_t p;
 mycell *q;
 size_t align;

 align = MPS_PF_ALIGN; /* makes it long enough for ~ to work */

 if (size < sizeof(mycell))
 {
  error("Tried to allocate too small an object.");
 }

/* twiddle the value of size to make it aligned */
 size = (size+header+align-1) & ~(align-1);

 do
 {
  die(mps_reserve(&p, ap, size), "Reserve: ");
  q=(void *)((char *)p + header);
  q->tag = MCdata;
  q->data = data;
  q->size = size;
  q->ref[0] = ref0;
  q->ref[1] = ref1;
 }
 while (!mps_commit(ap, p, size));
 return q;
}

mycell *allocone(mps_ap_t ap, mps_word_t data,
 mycell *ref0, mycell *ref1, size_t size)
{
  return allocheader(ap, data, ref0, ref1, size, 0);
}

mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
 MPS_SCAN_BEGIN(ss)
 {
  while (base < limit)
  {
   mycell *obj = base;
   unsigned long data = (unsigned long)obj->data;
   mps_res_t res;

   commentif(formatcomments, "scan %lu at %p", data, obj);
   switch (obj->tag)
   {
    case MCpadsingle:
     base = (mps_addr_t) ((mps_word_t) obj + MPS_PF_ALIGN);
     break;
    case MCpadmany:
     base = (mps_addr_t) (obj->data);
     break;
    case MCdata:
     /* actual scanning is done in here */

     if (obj->ref[0] != NULL)
     {
      commentif(formatcomments, "fix %lu[0] -> %p", data, obj->ref[0]);
      res = MPS_FIX12(ss, (mps_addr_t *) &(obj->ref[0])); /* pun! */
      if (res != MPS_RES_OK)
      {
       return res;
      }
     }
     if (obj->ref[1] != NULL)
     {
      commentif(formatcomments, "fix %lu[1] -> %p", data, obj->ref[1]);
      res = MPS_FIX12(ss, (mps_addr_t *) &(obj->ref[1])); /* pun! */
      if (res != MPS_RES_OK)
      {
       return res;
      }
     }
     /* \/ fall through \/ */

    case MCheart:
     base = (mps_addr_t) ((char *) obj + (obj->size));
   }
  }
  asserts(base == limit, "base <> limit in scan!");
 }
 MPS_SCAN_END(ss);
 return MPS_RES_OK;
}

mps_addr_t myskip(mps_addr_t object)
{
 mycell *obj = object;
 
 switch(obj->tag)
 {
  case MCpadsingle:
   return (mps_addr_t) ((mps_word_t) obj+MPS_PF_ALIGN);
  case MCpadmany:
   return (mps_addr_t) (obj->data);
  case MCheart: case MCdata:
   return (mps_addr_t) ((char *) obj + (obj->size));
  default:
   asserts(0, "skip: bizarre obj tag at %p.", obj);
   return 0; /* just to satisfy the compiler! */
 }
}

void mypad(mps_addr_t base, size_t size)
{
 mycell *obj = base;

 asserts(size >= MPS_PF_ALIGN, "size too small for pad");
 if (size == MPS_PF_ALIGN)
 {
  obj->tag = MCpadsingle;
 }
 else
 {
  obj->tag = MCpadmany;
  obj->data = ((mps_word_t) base) + size;
 }
}

mps_addr_t myisfwd(mps_addr_t object)
{
 mycell *obj = object;
 
 if (obj->tag != MCheart)
 {
  return NULL;
 }
 else
 {
  return (mps_addr_t) obj->data;
 }
}

void myfwd(mps_addr_t object, mps_addr_t to)
{
 mycell *obj = object;

 obj->tag = MCheart;
 obj->data = (mps_word_t) to;
}
