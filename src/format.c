/* impl.c.format: OBJECT FORMATS
 *
 *  $HopeName: MMsrc!format.c(MMdevel_assertid.2) $
 */

#include "mpm.h"

SRCID(format, "$HopeName: MMsrc!format.c(MMdevel_assertid.2) $");


Bool FormatCheck(Format format)
{
  CHECKS(0xF0630000, Format, format);
  CHECKU(0xF0630001, Space, format->space);
  CHECKL(0xF0630002, format->serial < format->space->formatSerial);
  CHECKL(0xF0630003, RingCheck(&format->spaceRing));
  CHECKL(0xF0630004, AlignCheck(format->alignment));
  /* @@@@ alignment should be less than maximum allowed */
  CHECKL(0xF0630005, format->scan != NULL);
  CHECKL(0xF0630006, format->skip != NULL);
  CHECKL(0xF0630007, format->move != NULL);
  CHECKL(0xF0630008, format->isMoved != NULL);
  CHECKL(0xF0630009, format->copy != NULL);
  CHECKL(0xF063000A, format->pad != NULL);
  return TRUE;
}


Res FormatCreate(Format *formatReturn, Space space,
                 Align alignment,
                 FormatScanMethod scan,
                 FormatSkipMethod skip,
                 FormatMoveMethod move,
                 FormatIsMovedMethod isMoved,
                 FormatCopyMethod copy,
                 FormatPadMethod pad)
{
  Format format;
  Res res;
  void *p;

  AVER(0xF063000B, formatReturn != NULL);

  res = SpaceAlloc(&p, space, sizeof(FormatStruct));
  if(res != ResOK)
    return res;
  format = (Format)p; /* avoid pun */

  format->space = space;
  RingInit(&format->spaceRing);
  format->alignment = alignment;
  format->scan = scan;
  format->skip = skip;
  format->move = move;
  format->isMoved = isMoved;
  format->copy = copy;
  format->pad = pad;

  format->sig = FormatSig;
  format->serial = space->formatSerial;
  ++space->formatSerial;

  AVERT(0xF063000C, Format, format);
  
  RingAppend(&space->formatRing, &format->spaceRing);

  *formatReturn = format;
  return ResOK;
}


void FormatDestroy(Format format)
{
  AVERT(0xF063000D, Format, format);

  RingRemove(&format->spaceRing);

  format->sig = SigInvalid;
  
  RingFinish(&format->spaceRing);

  SpaceFree(format->space, (Addr)format, sizeof(FormatStruct));
}

/* Must be thread safe.  See design.mps.interface.c.thread-safety. */
Space FormatSpace(Format format)
{
  return format->space;
}


Res FormatDescribe(Format format, mps_lib_FILE *stream)
{
  Res res;
  
  res = WriteF(stream,
               "Format $P ($U) {\n", (WriteFP)format, (WriteFU)format->serial,
               "  space $P ($U)\n", 
               (WriteFP)format->space, (WriteFU)format->space->serial,
               "  alignment $W\n", (WriteFW)format->alignment,
               "  scan $F\n", (WriteFF)format->scan,
               "  skip $F\n", (WriteFF)format->skip,
               "  move $F\n", (WriteFF)format->move,
               "  isMoved $F\n", (WriteFF)format->isMoved,
               "  copy $F\n", (WriteFF)format->copy,
               "  pad $F\n", (WriteFF)format->pad,
               "} Format $P ($U)\n", (WriteFP)format, (WriteFU)format->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}
