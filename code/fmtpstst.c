/* impl.c.fmtpstst: POSTSCRIPT OBJECT FORMAT TEST VERSION
 *
 *  $Id$
 *  $HopeName: MMsrc!fmtpstst.c(trunk.8) $
 *  Copyright (c) 2001 Ravenbrook Limited.
 *
 * .readership: MPS developers, SW developers
 *
 * .purpose: This is a simplified version of the PS object format for
 * internal MM tests.  Cf. SWmm_common!src:mmps.c.
 */

#include "testlib.h"
#include "fmtpscon.h"
#include "fmtpstst.h"
#include "mps.h"
#include "misc.h"


#define ALIGNMENT sizeof(OBJECT)


#define NOTREACHED              cdie(0, "NOTREACHED" STR(__LINE__))
#define HQASSERT(cond, msg)     cdie(cond, msg)
#define UNUSED_PARAM(type, v)   testlib_unused(v)

#define DWORD_ALIGN_UP(x)       (((int32)(x)+7)&~7)
#define DWORD_ALIGN_DOWN(x)     ((int32)(x)&~7)
#define DWORD_IS_ALIGNED(x)     (((int32)(x)&7)==0)
#define ADDR_ADD(p, s)          ((mps_addr_t)((char *)(p) + (s)))


/* dummy save level */

int8 ps_save_level = 0;


/* ps_scan -- a scan method for the format */
mps_res_t ps_scan(mps_ss_t scan_state,
                  mps_addr_t base, mps_addr_t limit)
{
  OBJECT *obj;
  OBJECT *obj_limit;
  mps_addr_t ref;
  mps_addr_t ref_limit = NULL;
  mps_res_t res;

  HQASSERT(DWORD_IS_ALIGNED(base), "unaligned base");
  HQASSERT(DWORD_IS_ALIGNED(limit), "unaligned limit");
  obj_limit = limit;
  MPS_SCAN_BEGIN(scan_state)
    for(obj = base; obj < obj_limit; obj++) {
      ref = (mps_addr_t)theID1bit(obj);
      switch(theType(theITags(obj))) {
      case ONAME:
        NOTREACHED;
        break;
      case OSAVE:
        continue;
      case ODICTIONARY:
        NOTREACHED;
        break;
      case OSTRING:
        ref_limit = ADDR_ADD(ref, theILen(obj));
        break;
      case OFILE:
        NOTREACHED;
        break;
      case OGSTATE:
        NOTREACHED;
        break;
      case OARRAY:
      case OPACKEDARRAY:
        ref_limit = ADDR_ADD(ref, theILen(obj) * sizeof(OBJECT));
        break;
      default: continue; /* not a composite object */
      }
      for(; ref < ref_limit; ref = ADDR_ADD(ref, ALIGNMENT)) {
        /* We know it's not going to move, so ref will not change. */
        res = MPS_FIX12(scan_state, &ref);
        if(res != MPS_RES_OK) return res;
      }
    }
  MPS_SCAN_END(scan_state);
  return MPS_RES_OK;
}


/* ps_skip -- skip method for the format
 *
 * Skips to the next alignment grain.  Note that this can be called with
 * unaligned pointers to strings.
 */
static mps_addr_t ps_skip(mps_addr_t object)
{
  return ADDR_ADD(DWORD_ALIGN_DOWN(object), sizeof(OBJECT));
}


/* ps_no_copy -- a dummy copy method for the format */
static void ps_no_copy(mps_addr_t old, mps_addr_t new)
{
  UNUSED_PARAM(mps_addr_t, old); UNUSED_PARAM(mps_addr_t, new);
  NOTREACHED;
}


/* ps_no_fwd -- a dummy forwarding method for the format */
static void ps_no_fwd(mps_addr_t old, mps_addr_t new)
{
  UNUSED_PARAM(mps_addr_t, old); UNUSED_PARAM(mps_addr_t, new);
  NOTREACHED;
}


/* ps_no_isfwd -- a dummy isfwd method for the format */
static mps_addr_t ps_no_isfwd(mps_addr_t object)
{
  UNUSED_PARAM(mps_addr_t, object);
  NOTREACHED; return NULL;
}


/* ps_no_pad -- a dummy pad method for the format */
static void ps_no_pad(mps_addr_t addr, size_t size)
{
  UNUSED_PARAM(mps_addr_t, addr); UNUSED_PARAM(size_t, size);
  NOTREACHED;
}


/* ps_format_A -- the actual format object */
static struct mps_fmt_A_s ps_format_A = {
  (mps_align_t)ALIGNMENT,
  ps_scan, ps_skip, ps_no_copy, ps_no_fwd, ps_no_isfwd, ps_no_pad
};


mps_fmt_A_s *ps_fmt_A(void)
{
  return &ps_format_A;
}


mps_res_t ps_string_init(OBJECT *objOutput, mps_addr_t p, uint16 length)
{
  uint8 *cp = (uint8 *)p;
  uint16 n;

  theITags(objOutput) = OSTRING | LITERAL | UNLIMITED;
  SETIGLOBJECT(objOutput);
  theILen(objOutput) = length;
  theCList(theIValue(objOutput)) = p;

  for(n = length; n > 0; --n, ++cp)
    *cp = (uint8)((unsigned int)cp % 256);

  return MPS_RES_OK;
}


mps_res_t ps_array_init(OBJECT *objOutput, mps_addr_t p, uint16 length,
                        OBJECT *refs, size_t nr_refs)
{
  OBJECT *arr = (OBJECT *)p;
  size_t r;

  UNUSED_PARAM(OBJECT *, refs); UNUSED_PARAM(size_t, nr_refs);

  theITags(objOutput) = OARRAY | LITERAL | UNLIMITED;
  SETIGLOBJECT(objOutput);
  theILen(objOutput) = length;
  theOList(theIValue(objOutput)) = p;

  if(refs == NULL)
    for(r = 0; r < length; ++r) {
      theTags(arr[r]) = ONULL | LITERAL;
      NEWISSAVEDOBJECT(&arr[r]);
    }
  else
    for(r = 0; r < length; ++r) {
      size_t rr = (size_t)rnd() % nr_refs;
      uint16 oldlen, offset, newlen;

      OCopy(arr[r], refs[rr]);
      SETICSAVEDOBJECT(&arr[r], theIGLObjMode(objOutput));
      /* truncate it to a substring or subarray */
      oldlen = theLen(arr[r]);
      offset = (uint16)((uint16)rnd() % (oldlen + 1));
      newlen = (uint16)((uint16)rnd() % (oldlen - offset + 1));
      theLen(arr[r]) = newlen;
      if(theType(theTags(arr[r])) == OSTRING)
        theCList(theValue(arr[r])) += offset;
      else
        theOList(theValue(arr[r])) += offset;
    }
  return MPS_RES_OK;
}


void ps_write(OBJECT *obj, OBJECT *refs, size_t nr_refs)
{
  if (theType(theITags(obj)) == OARRAY) {
    size_t t = theILen(obj);

    if (t > 0) {
      size_t i = rnd() % t;
      size_t rr = (size_t)rnd() % nr_refs;
      OBJECT *body = theOList(theIValue(obj));

      OCopy(body[i], refs[rr]);
    }
  }
}


static void ps_check_obj(OBJECT *obj, int level)
{
  size_t r;

  cdie(DWORD_IS_ALIGNED(obj), "ps_check_obj unaligned");
  if(level == 0) return;
  --level;

  switch(theType(theITags(obj))) {
  case ONULL: break;
  case ONAME:
  case OSAVE:
  case ODICTIONARY:
    NOTREACHED;
    break;
  case OSTRING:
    {
      uint8 *cp = theCList(theIValue(obj));
      size_t n;

      for(n = theILen(obj); n > 0; --n, ++cp)
        cdie(*cp == (uint8)((unsigned int)cp % 256), "ps_check_obj");
    }
    break;
  case OFILE:
  case OGSTATE:
    NOTREACHED;
    break;
  case OARRAY:
  case OPACKEDARRAY:
    {
      OBJECT *cont = theOList(theIValue(obj));

      for(r = 0; r < theILen(obj); ++r)
        ps_check_obj(&cont[r], level);
    }
    break;
  default: NOTREACHED;
  }
}


int ps_check(OBJECT *obj)
{
  ps_check_obj(obj, 4);
  return 1;
}
