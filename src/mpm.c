/* impl.c.mpm: GENERAL MPM SUPPORT
 *
 * $HopeName: MMsrc!mpm.c(MMdevel_lib.1) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 */

#include "mpm.h"

SRCID(mpm, "$HopeName: MMsrc!mpm.c(MMdevel_lib.1) $");


/* MPMCheck -- test MPM assumptions */

Bool MPMCheck(void)
{
  CHECKL(sizeof(char) == 1);
  CHECKL(sizeof(Word) * CHAR_BIT == WORD_WIDTH);
  CHECKL(1uL << WORD_SHIFT == WORD_WIDTH);
  CHECKL(AlignCheck(ARCH_ALIGN));
  /* impl.c.mpm.check.ti: Check that trace ids will fit in the */
  /* TraceId type. */
  CHECKL(TRACE_MAX <= TraceIdNONE);
  CHECKL(TRACE_MAX <= UINT_MAX);
  /* impl.c.mpm.check.ts: Check that there are enough bits in */
  /* a TraceSet to store all possible trace ids. */
  CHECKL(sizeof(TraceSet) * CHAR_BIT >= TRACE_MAX);

  CHECKL((SizeAlignUp(0, 2048) == 0));
  CHECKL(!SizeIsAligned(64, (unsigned) -1));
  CHECKL(SizeIsAligned(0, 32));
  CHECKL((SizeAlignUp(1024, 16) == 1024));
  /* .prime: 31051 is prime */
  CHECKL(SizeIsAligned(SizeAlignUp(31051, 256), 256));
  CHECKL(SizeIsAligned(SizeAlignUp(31051, 512), 512));
  CHECKL(!SizeIsAligned(31051, 1024));
  CHECKL(!SizeIsP2(0));
  CHECKL(SizeIsP2(128));
  CHECKL(SizeLog2(1L) == 0);
  CHECKL(SizeLog2(256L) == 8);
  CHECKL(SizeLog2(65536L) == 16);
  CHECKL(SizeLog2(131072L) == 17);

  return TRUE;  
}


/* AlignCheck -- check that an alignment is valid */

Bool AlignCheck(Align align)
{
  CHECKL(align > 0 && (align & (align - 1)) == 0);
  return TRUE;
}


/* WordIsAligned -- test whether a word is aligned */

Bool (WordIsAligned)(Word word, Align align)
{
  return (word & (align - 1)) == 0;
}


/* WordAlignUp -- round up a word to the nearest aligned value */

Word (WordAlignUp)(Word word, Align align)
{
  AVER(AlignCheck(align));
  return (word + align - 1) & ~(align - 1);
}


/* SizeIsP2 -- test whether a size is a power of two */

Bool SizeIsP2(Size size)
{
  return size > 0 && (size & (size - 1)) == 0;
}


/* Logarithms */

Shift SizeFloorLog2(Size size)
{
  Shift l = 0;

  while(size > 1) {
    ++l;
    size >>= 1;
  }

  return l;
}

Shift SizeLog2(Size size)
{
  AVER(SizeIsP2(size));

  return SizeFloorLog2(size);
}


/* AddrAdd -- add a size to an address */

Addr (AddrAdd)(Addr addr, Size size)
{
  Addr next = (Addr)((Word)addr + size);
  AVER(next >= addr);   /* overflow check */
  return next;
}


/* AddrSub -- subtract a size from an address */

Addr (AddrSub)(Addr addr, Size size)
{
  Addr next = (Addr)((Word)addr - size);
  AVER(next <= addr);   /* overflow check */
  return next;
}


/* AddrOffset -- calculate the offset between two addresses */

Size (AddrOffset)(Addr base, Addr limit)
{
  AVER(base <= limit);
  return (Size)((Word)limit - (Word)base);
}


/* WriteWord
 *
 * width is zero for the minimum width
 */
 
Res WriteWord(mps_lib_FILE *stream, Word w, unsigned base, unsigned width)
{
  static const char digit[16] = "0123456789ABCDEF";
  char buf[MPS_WORD_WIDTH + 1]; /* enough for binary, plus one for terminator */
  unsigned i;
  int r;

  AVER(stream != NULL);
  AVER(2 <= base && base <= 16);
  AVER(width <= MPS_WORD_WIDTH);
  
  /* Add digits to the buffer starting at the right-hand end, so that */
  /* the buffer forms a string representing the number.  A do...while */
  /* loop is used to ensure that at least one digit (zero) is written */
  /* when the number is zero. */
  i = MPS_WORD_WIDTH;
  buf[i] = '\0';
  do {
    --i;
    buf[i] = digit[w % base];
    w /= base;
  } while(w > 0);

  /* If the number is not as wide as the requested field, pad out the */
  /* buffer with zeros. */
  while(i > MPS_WORD_WIDTH - width) {
    buf[i] = digit[0];
    --i;
  }

  r = mps_lib_fputs(&buf[i], stream);
  if(r == mps_lib_EOF)
    return ResIO;

  return ResOK;
}


Res WriteAddr(mps_lib_FILE *stream, Addr addr)
{
  return WriteWord(stream, (Word)addr, 0x10, MPS_WORD_WIDTH / 4);
}

Res WriteP(mps_lib_FILE *stream, void *p)
{
  return WriteWord(stream, (Word)p, 0x10, MPS_WORD_WIDTH / 4);
}

