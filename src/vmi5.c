/* impl.c.vmi5: VIRTUAL MEMORY MAPPING FOR IRIX 5
 *
 * $HopeName: MMsrc!vmi5.c(MM_epcore_brisling.3) $
 * Copyright (C) 1997, 1998, 1999 Harlequin Group plc.  All rights reserved.
 *
 * Design: design.mps.vm
 *
 * This is the implementation of the virtual memory mapping interface
 * (vm.h) for IRIX 5.x.
 *
 * mmap(2) is used to reserve address space by creating a mapping to
 * /dev/zero with page access none.  mmap(2) is used to map pages
 * onto store by creating a copy-on-write mapping to /dev/zero.
 *
 * .assume.not-last: The implementation of VMCreate assumes that
 * mmap() will not choose a region which contains the last page
 * in the address space, so that the limit of the mapped area
 * is representable.
 *
 * .assume.mmap.err: EAGAIN is the only error we really expect to get
 * from mmap when committing and ENOMEM when reserving or committing (we
 * have actually observed ENOMEM when committing).  The others are
 * either caused by invalid params or features we don't use.  See
 * mmap(2) for details.
 *
 * .map.chunk: IRIX 5 has a limit on the number of pages a process can
 * have mapped at the same time.  VMMap replaces one mapping with
 * another, and in the meanwhile, both exist.  So do large maps in small
 * chunks.
 *
 * TRANSGRESSIONS
 *
 * .fildes.name: VMStruct has one fields whose name violates our naming
 * conventions.  It's called zero_fd to emphasize that it's a file
 * descriptor and this fact is not reflected in the type.
 */

#include "mpm.h"

#if !defined(MPS_OS_I5)
#error "vmi5.c is IRIX-5-specific, but MPS_OS_I5 is not set"
#endif

#define _POSIX_SOURCE
#define _POSIX_C_SOURCE 199309L

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

/* No constant for the mmap error return on IRIX 5, so define one. */
#if !defined(MAP_FAILED)
#define MAP_FAILED ((void *)-1)
#endif

SRCID(vmi5, "$HopeName: MMsrc!vmi5.c(MM_epcore_brisling.3) $");


/* mapChunkSHIFT -- shift to compute mapChunkSize from arena size
 *
 * mapChunkSize is set to a fraction of arena size, by shifting it it
 * down by this amount.  This should be a config parameter or something.
 */

#define mapChunkSHIFT 7


/* VMStruct -- virtual memory structure */

#define VMSig           ((Sig)0x519B3999) /* SIGnature VM */

/* The name zero_fd is a transgression, see .fildes.name. */
typedef struct VMStruct {
  Sig sig;                      /* design.mps.sig */
  int zero_fd;                  /* fildes for mmap */
  Align align;                  /* page size */
  Addr base, limit;             /* boundaries of reserved space */
  Size reserved;                /* total reserved address space */
  Size mapped;                  /* total mapped memory */
  Size mapChunkSize;            /* map chunk size, see .map.chunk */
} VMStruct;


Align VMAlign(VM vm)
{
  return vm->align;
}


Bool VMCheck(VM vm)
{
  CHECKS(VM, vm);
  CHECKL(vm->zero_fd >= 0);
  CHECKL(vm->base != 0);
  CHECKL(vm->limit != 0);
  CHECKL(vm->base < vm->limit);
  CHECKL(vm->mapped <= vm->reserved);
  CHECKL(SizeIsP2(vm->align));
  CHECKL(AddrIsAligned(vm->base, vm->align));
  CHECKL(AddrIsAligned(vm->limit, vm->align));
  return TRUE;
}


/* VMCreate -- reserve some address space, and create a VM structure */

Res VMCreate(VM *vmReturn, Size size)
{
  void *addr;
  Align align;
  int zero_fd;
  VM vm;
  Res res;
  size_t chunkSize;

  AVER(vmReturn != NULL);

  align = (Align)sysconf(_SC_PAGESIZE);
  AVER(SizeIsP2(align));
  size = SizeAlignUp(size, align);
  if((size == 0) || (size > (Size)(size_t)-1))
    return ResRESOURCE;

  zero_fd = open("/dev/zero", O_RDONLY);
  if(zero_fd == -1)
    return ResFAIL;

  /* Map in a page to store the descriptor on. */
  addr = mmap((void *)0, (size_t)SizeAlignUp(sizeof(VMStruct), align),
              PROT_READ | PROT_WRITE, MAP_PRIVATE,
              zero_fd, (off_t)0);
  if(addr == MAP_FAILED) {
    AVER(errno == ENOMEM || errno == EAGAIN); /* .assume.mmap.err */
    res = (errno == ENOMEM || errno == EAGAIN) ? ResMEMORY : ResFAIL;
    goto failVMMap;
  }
  vm = (VM)addr;

  vm->zero_fd = zero_fd;
  vm->align = align;

  /* .map.reserve: MAP_AUTORESRV is necessary to avoid reserving swap. */
  addr = mmap((void *)0, (size_t)size, PROT_NONE, MAP_SHARED | MAP_AUTORESRV,
	      zero_fd, (off_t)0);
  if(addr == MAP_FAILED) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    res = (errno == ENOMEM) ? ResRESOURCE : ResFAIL;
    goto failReserve;
  }

  vm->base = (Addr)addr;
  vm->limit = AddrAdd(vm->base, size);
  vm->reserved = size;
  vm->mapped = (Size)0;
  vm->mapChunkSize = SizeAlignUp(size >> mapChunkSHIFT, align);

  /* Check that you can VMMap (see .map.chunk). */
  addr = mmap((void *)vm->base, vm->mapChunkSize,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_FIXED,
	      vm->zero_fd, (off_t)0);
  if(addr == MAP_FAILED) {
    AVER(errno == ENOMEM); /* .assume.mmap.err */
    res = (errno == ENOMEM) ? ResRESOURCE : ResFAIL;
    goto failCheck;
  }
  /* Checked; now return the chunk. */
  addr = mmap((void *)vm->base, vm->mapChunkSize,
              PROT_NONE, MAP_SHARED | MAP_FIXED | MAP_AUTORESRV,
              vm->zero_fd, (off_t)0);
  AVER(addr == (void *)vm->base);

  vm->sig = VMSig;

  AVERT(VM, vm);

  EVENT_PAA(VMCreate, vm, vm->base, vm->limit);

  *vmReturn = vm;
  return ResOK;

failCheck:
  (void)munmap((void *)vm->base, (size_t)size);
failReserve:
  (void)munmap((void *)vm, (size_t)SizeAlignUp(sizeof(VMStruct), align));
failVMMap:
  (void)close(zero_fd);
  return res;
}


void VMDestroy(VM vm)
{
  int r;
  int zero_fd;

  AVERT(VM, vm);
  AVER(vm->mapped == (Size)0);

  /* This appears to be pretty pointless, since the descriptor */
  /* page is about to vanish completely.  However, munmap might fail */
  /* for some reason, and this would ensure that it was still */
  /* discovered if sigs were being checked. */
  vm->sig = SigInvalid;

  zero_fd = vm->zero_fd;
  r = munmap((void *)vm->base, (size_t)AddrOffset(vm->base, vm->limit));
  AVER(r == 0);
  r = munmap((void *)vm, (size_t)SizeAlignUp(sizeof(VMStruct), vm->align));
  AVER(r == 0);
  r = close(zero_fd);
  AVER(r == 0);

  EVENT_P(VMDestroy, vm);
}


/* VMBase, VMLimit -- return the base & limit of the memory reserved */

Addr VMBase(VM vm)
{
  AVERT(VM, vm);
  return vm->base;
}

Addr VMLimit(VM vm)
{
  AVERT(VM, vm);
  return vm->limit;
}


Size VMReserved(VM vm)
{
  AVERT(VM, vm);
  return vm->reserved;
}

Size VMMapped(VM vm)
{
  AVERT(VM, vm);
  return vm->mapped;
}


/* VMMap -- commit memory between base & limit */

Res VMMap(VM vm, Addr base, Addr limit)
{
  Size size;
  void *chunkBase;
  size_t left;
  size_t chunkSize;
  void *addr;
  Res res;

  AVERT(VM, vm);
  AVER(base < limit);
  AVER(base >= vm->base);

  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  /* Map /dev/zero onto the area with a copy-on-write policy.  This */
  /* effectively populates the area with zeroed memory. */
  size = AddrOffset(base, limit);
  /* Check it won't lose any bits. */
  AVER(size <= (Size)(size_t)-1);
  /* See .map.chunk. */
  for(chunkBase = (void *)base, left = (size_t)size;
      left > 0;
      chunkBase = PointerAdd(chunkBase, chunkSize), left -= chunkSize) {
    chunkSize = (left > vm->mapChunkSize) ? vm->mapChunkSize : left;
    addr = mmap(chunkBase, chunkSize,
                PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_FIXED,
                vm->zero_fd, (off_t)0);
    if(addr == MAP_FAILED) {
      AVER(errno == ENOMEM || errno == EAGAIN); /* .assume.mmap.err */
      res = ResMEMORY;
      goto failMap;
    }
    AVER(addr == chunkBase);
  }

  vm->mapped += size;

  EVENT_PAA(VMMap, vm, base, limit);
  return ResOK;

failMap:
  /* Back up 'chunkBase' and 'left', undoing in opposite order, but */
  /* let 'left' trail by one chunk, because we can't let it pass */
  /* 'size' and possibly overflow.  All the chunks are the same size, */
  /* because the commit loop failed before it mapped the small one. */
  for(chunkBase = PointerSub(chunkBase, vm->mapChunkSize);
      left < (size_t)size;
      chunkBase = PointerSub(chunkBase, vm->mapChunkSize),
        left += vm->mapChunkSize) {
    (void)mmap(chunkBase, vm->mapChunkSize,
               PROT_NONE, MAP_SHARED | MAP_FIXED | MAP_AUTORESRV,
               vm->zero_fd, (off_t)AddrOffset(vm->base, chunkBase));
  }
  return res;
}


/* VMUnmap -- decommit memory between base & limit */

void VMUnmap(VM vm, Addr base, Addr limit)
{
  Size size;
  void *chunkBase;
  size_t left;
  size_t chunkSize;
  void *addr;

  AVERT(VM, vm);
  AVER(base < limit);
  AVER(base >= vm->base);
  AVER(limit <= vm->limit);
  AVER(AddrIsAligned(base, vm->align));
  AVER(AddrIsAligned(limit, vm->align));

  /* .unmap.reserve: Map /dev/zero onto the area, allowing no access. */
  /* This effectively depopulates the area from memory, but keeps */
  /* it "busy" as far as the OS is concerned, so that it will not */
  /* be re-used by other calls to mmap which do not specify */
  /* MAP_FIXED.  See also .map.reserve. */
  /* The OS doesn't merge this mapping with any neighbours, but it */
  /* can keep track of at least 16K mappings, so it's good enough. */
  size = AddrOffset(base, limit);
  /* Check it won't lose any bits. */
  AVER(size <= (Size)(size_t)-1);
  /* See .map.chunk. */
  for(chunkBase = (void *)base, left = (size_t)size;
      left > 0;
      chunkBase = PointerAdd(chunkBase, chunkSize), left -= chunkSize) {
    chunkSize = (left > vm->mapChunkSize) ? vm->mapChunkSize : left;
    addr = mmap(chunkBase, chunkSize,
                PROT_NONE, MAP_SHARED | MAP_FIXED | MAP_AUTORESRV,
                vm->zero_fd, (off_t)AddrOffset(vm->base, chunkBase));
    if(addr == MAP_FAILED) {
      AVER(errno == ENOMEM); /* ran out of map space */
    } else {
      AVER(addr == chunkBase);
    }
  }

  vm->mapped -= size;

  EVENT_PAA(VMUnmap, vm, base, limit);
}
