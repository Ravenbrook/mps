/* impl.c.format: OBJECT FORMATS
 *
 *  $HopeName: MMsrc!format.c(trunk.14) $
 */

#include "mpm.h"

SRCID(format, "$HopeName: MMsrc!format.c(trunk.14) $");


Bool FormatCheck(Format format)
{
  CHECKS(Format, format);
  CHECKU(Arena, format->arena);
  CHECKL(format->serial < format->arena->formatSerial);
  CHECKL(RingCheck(&format->arenaRing));
  CHECKL(AlignCheck(format->alignment));
  /* @@@@ alignment should be less than maximum allowed */
  CHECKL(format->scan != NULL);
  CHECKL(format->skip != NULL);
  CHECKL(format->move != NULL);
  CHECKL(format->isMoved != NULL);
  CHECKL(format->copy != NULL);
  CHECKL(format->pad != NULL);
  return TRUE;
}


Res FormatCreate(Format *formatReturn, Arena arena,
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

  AVER(formatReturn != NULL);

  res = ArenaAlloc(&p, arena, sizeof(FormatStruct));
  if(res != ResOK)
    return res;
  format = (Format)p; /* avoid pun */

  format->arena = arena;
  RingInit(&format->arenaRing);
  format->alignment = alignment;
  format->scan = scan;
  format->skip = skip;
  format->move = move;
  format->isMoved = isMoved;
  format->copy = copy;
  format->pad = pad;

  format->sig = FormatSig;
  format->serial = arena->formatSerial;
  ++arena->formatSerial;

  AVERT(Format, format);
  
  RingAppend(&arena->formatRing, &format->arenaRing);

  *formatReturn = format;
  return ResOK;
}


void FormatDestroy(Format format)
{
  AVERT(Format, format);

  RingRemove(&format->arenaRing);

  format->sig = SigInvalid;
  
  RingFinish(&format->arenaRing);

  ArenaFree(format->arena, (Addr)format, sizeof(FormatStruct));
}

/* Must be thread safe.  See design.mps.interface.c.thread-safety. */
Arena FormatArena(Format format)
{
  return format->arena;
}


Res FormatDescribe(Format format, mps_lib_FILE *stream)
{
  Res res;
  
  res = WriteF(stream,
               "Format $P ($U) {\n", (WriteFP)format, (WriteFU)format->serial,
               "  arena $P ($U)\n", 
               (WriteFP)format->arena, (WriteFU)format->arena->serial,
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
