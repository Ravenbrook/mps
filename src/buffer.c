/* impl.c.buffer: ALLOCATION BUFFER IMPLEMENTATION
 *
 * $HopeName: MMsrc!buffer.c(MMdevel_remem.2) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved
 *
 * This is the interface to allocation buffers.
 *
 * An allocation buffer is an interface to a pool which provides
 * very fast allocation, and defers the need for synchronization in
 * a multi-threaded environment.
 *
 * Pools which contain formatted objects must be synchronized so
 * that the pool can know when an object is valid.  Allocation from
 * such pools is done in two stages: reserve and commit.  The client
 * first reserves memory, then initializes it, then commits.
 * Committing the memory declares that it contains a valid formatted
 * object.  Under certain conditions, some pools may cause the
 * commit operation to fail.  (See the documentation for the pool.)
 * Failure to commit indicates that the whole allocation failed and
 * must be restarted.  When a pool with commit failure, the
 * allocation sequence could look something like this:
 *
 * do {
 *   res = BufferReserve(&p, buffer, size);
 *   if(res != ResOK) return res;       // allocation fails, reason res
 *   initialize(p);                     // p now points at valid object
 * } while(!BufferCommit(buffer, p, size));
 *
 * Pools which do not contain formatted objects can use a one-step
 * allocation as usual.  Effectively any random rubbish counts as a
 * "valid object" to such pools.
 *
 * An allocation buffer is an area of memory which is pre-allocated
 * from a pool, plus a buffer descriptor, which contains, inter
 * alia, four pointers: base, init, alloc, and limit.  Base points
 * to the base address of the area, limit to the last address plus
 * one.  Init points to the first uninitialized address in the
 * buffer, and alloc points to the first unallocated address.
 *
 *    L . - - - - - .
 *      |           |
 *      |   junk    |
 *      |           |       the "busy" state, after Reserve
 *    A |-----------|
 *      |  uninit   |
 *    I |-----------|
 *      |   init    |
 *      |           |
 *    B `-----------'
 *
 *    L . - - - - - .
 *      |           |
 *      |   junk    |
 *      |           |       the "ready" state, after Commit
 *  A=I |-----------|
 *      |           |
 *      |           |
 *      |   init    |
 *      |           |
 *    B `-----------'
 *
 * Access to these pointers is restricted in order to allow
 * synchronization between the pool and the client.  The client may
 * only write to init and alloc, but in a restricted and atomic way
 * detailed below.  The pool may read the contents of the buffer
 * descriptor at _any_ time.  During calls to the fill and trip
 * methods, the pool may update any or all of the fields
 * in the buffer descriptor.  The pool may update the limit at _any_
 * time.
 *
 * Only one thread may use a buffer at once, unless the client
 * places a mutual exclusion around the buffer access in the usual
 * way.  In such cases it is usually better to create one buffer for
 * each thread.
 *
 * Here are pseudo-code descriptions of the reserve and commit
 * operations.  These may be implemented in-line by the client.
 * Note that the client is responsible for ensuring that the size
 * (and therefore the alloc and init pointers) are aligned according
 * to the buffer's alignment.
 *
 * Reserve(buf, size)                   ; size must be aligned to pool
 *   if buf->limit != 0 && buf->limit - buf->alloc >= size then
 *     buf->alloc +=size                ; must be atomic update
 *     p = buf->init
 *   else
 *     res = BufferFill(&p, buf, size)  ; buf contents may change
 *
 * Commit(buf, p, size)
 *   buf->init = buf->alloc             ; must be atomic update
 *   if buf->limit == 0 then
 *     b = BufferTrip(buf, p, size)     ; buf contents may change
 *
 * The pool must allocate the buffer descriptor and initialize it by
 * calling BufferInit.  The descriptor this creates will fall
 * through to the fill method on the first allocation.  In general,
 * pools should not assign resources to the buffer until the first
 * allocation, since the buffer may never be used.
 *
 * The pool may update the base, init, alloc, and limit fields when
 * the fallback methods are called.  In addition, the pool may set
 * the limit to zero at any time.  The effect of this is either:
 *
 *   1. cause the _next_ allocation in the buffer to fall through to
 *      the buffer fill method, and allow the buffer to be flushed
 *      and relocated;
 *
 *   2. cause the buffer trip method to be called if the client was
 *      between reserve and commit.
 *
 * A buffer may not be relocated under other circumstances because
 * there is a race between updating the descriptor and the client
 * allocation sequence.
 */

#include "mpm.h"

SRCID(buffer, "$HopeName: MMsrc!buffer.c(MMdevel_remem.2) $");


Ring BufferPoolRing(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return &buffer->poolRing;
}

Pool (BufferPool)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return buffer->pool;
}


/* BufferCreate -- create an allocation buffer in a pool
 *
 * Iff successful, *bufferReturn is updated with a pointer to the
 * buffer descriptor, and ResOK is returned.
 */

Res BufferCreate(Buffer *bufferReturn, Pool pool)
{
  AVER(bufferReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class->bufferCreate != NULL);

  return (*pool->class->bufferCreate)(bufferReturn, pool);
}


/* BufferDestroy -- destroy an allocation buffer
 *
 * Destroy frees a buffer descriptor.  The buffer must be in the
 * "ready" state, i.e. not between a Reserve and Commit.  Allocation
 * in the area of memory to which the descriptor refers must cease
 * after Destroy is called.  This does not affect objects that have
 * been allocated in the buffer.
 */

void BufferDestroy(Buffer buffer)
{
  Pool pool;

  AVERT(Buffer, buffer);

  pool = BufferPool(buffer);

  AVER(pool->class->bufferDestroy != NULL);
  AVER(buffer->ap.init == buffer->ap.alloc);

  (*pool->class->bufferDestroy)(pool, buffer);
}


Bool BufferCheck(Buffer buffer)
{
  CHECKS(Buffer, buffer);
  CHECKU(Pool, buffer->pool);
  CHECKL(buffer->serial < buffer->pool->bufferSerial);
  CHECKU(Space, buffer->space);
  CHECKL(RingCheck(&buffer->poolRing));
  CHECKL(TraceSetCheck(buffer->grey));
  CHECKL(RingCheck(&buffer->traceRing));
  CHECKL(buffer->base <= buffer->ap.init);
  CHECKL(buffer->ap.init <= buffer->ap.alloc);
  CHECKL(buffer->ap.alloc <= buffer->ap.limit || buffer->ap.limit == 0);
  CHECKL(buffer->alignment == buffer->pool->alignment);
  CHECKL(AlignCheck(buffer->alignment));
  CHECKL(AddrIsAligned(buffer->base, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->ap.init, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->ap.alloc, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->ap.limit, buffer->alignment));
  if(buffer->seg != NULL)
    CHECKL(SegCheck(buffer->seg));
  return TRUE;
}

/* BufferSet/Reset/Trap -- set/reset/trap a buffer
 *
 * The collector's view:
 * .states: A buffer may be in one of three mutually exclusive states.
 *
 * .state.set: the buffer is attached to an area of memory [base,limit)
 * within a seg.  Allocation will continue at init.
 *
 * .state.reset: a buffer in this state is not attached to any seg and
 * will cause a call to PoolBufferFill on the next reserve.
 *
 * .state.trapped: no further allocation may occur in a trapped buffer.
 * The next reserve or commit will cause a call to PoolBufferTrip.
 *
 * .busy: A buffer is busy if the mutator may be using it and the using
 * thread is not in a call to Fill or Trip.
 * This may depend on a promise between a pool and its user.
 *
 * BufferSet sets the buffer base, init, and limit fields and the Ap is
 * set so that the buffer is ready to start allocating in that area of
 * memory.
 *
 * BufferReset changes the state from set to reset.  This may only be
 * used on a non-busy buffer.
 *
 * BufferTrap puts a set buffer in to trapped state.
 * 
 * BufferUntrap turns a trapped buffer back into a set buffer.
 * A buffer may not be untrapped before it is determined it is not
 * busy.  This also makes the buffer ready.
 *
 * BufferIsReset/Set/Trapped
 *   returns TRUE iff the buffer is in the reset/set/trapped state,
 * These may be used even while the buffer is busy.
 *
 * Allocation in buffers
 *
 * Allocation in buffers may happen asynchronously with collector activity.
 * 
 * At all points there is a consistent view as to what is defined to be
 * intialised.  This is defined to be all memory between base and init.
 * init is read by using BufferGetInit().
 *
 * Objects are allocated contiguously in memory upwards from init until
 * limit.
 *
 * Allocation may happen in a perticular buffer, in a single thread at
 * a time, using the following protocol.
 * The intention is that multiple threads use different buffers to
 * allocate in, avoiding the need for interlocking.
 *
 * do {
 *   res = BufferReserve(&p, buffer, size);
 *   if(res != ResOK) return res;       // allocation fails, reason res
 *   initialize(p);                     // p now points at valid object
 * } while(!BufferCommit(buffer, p, size));
 *
 * If a trap happens PoolBufferTrip will be called before another further
 * action.
 *
 * If BufferReserve is called the allocation will be passed onto
 * PoolBufferFill iff:
 *     the buffer is set AND the allocation of size will not fit
 *  OR the buffer is reset
 * The buffer should be in a set or trapped state on return, with
 * *pReturn set to where object is to be allocated, if there is no
 * error.
 *
 * If the buffer is trapped and BufferCommit or BufferReserve is called
 * (this can only happen if the buffer is trapped while it is busy)
 * then PoolBufferTrip is called iff:
 *   the buffer is trapped
 * 
 * Allocation Point Implementation
 *
 * An AP or allocation point contains three pointers:
 * init, alloc and limit
 *
 * .ap.states: the states of the buffer are then identified as follows:
 *
 * set     iff ap.limit != 0 != ap.init
 * trapped iff ap.limit == 0 != ap.init
 * reset   iff ap.limit == 0 == ap.init
 *
 * .ap.inited: the initialised data in a buffer is defined to be from base
 * to ap.init iff buffer is set and to init iff buffer is trapped
 *
 * .ap.ready: a set buffer is ready iff ap.init == ap.alloc
 * an a set buffer should be busy or ready.
 *
 * if the buffer is busy then:
 *   ap.init  must not be written
 *   ap.alloc should not be read and must not be written
 *
 * .ap.thread.safety: Thread safety
 *   Synchronisation is achieved whenever BufferFill or BufferTrip is
 * called.  The case where there may be conflict over use of the AP
 * is when the allocator may be at any point of allocation (.busy).
 * In the busy state, both allocator and owner may be accessing the
 * AP fields.
 * In this case synchronisation happens through atomic memory reads
 * and writes.
 * During this time
 * ap.init is writeable only by the allocator.
 * ap.limit is writeable only by the owner.
 * ap.init and ap.limit are atomically readable by both.
 * ap.alloc is accessible only by the allocator.
 * 
 * Reserve(size)
 *   ap.alloc = ap.init + size
 *   if ap.alloc is in (ap.init, ap.limit] then
 *   allocation succeeds at ap.init.
 *   otherwise BufferFill(size)
 *
 * Commit
 *   // .commit.time: the write of ap.init is the time of commit
 *   ap.init = ap.alloc
 *   if ap.limit !=0 then success
 *   else BufferTrip(size)
 *
 * Trap
 *   ap.limit = 0
 *   init = ap.init // .trap.time: the read of ap.init is the time of the trap
 * 
 * It can be seen that BufferFill will be called iff:
 *     the buffer is set AND the allocation of size will not fit
 *  OR the buffer is reset
 *  OR the buffer is trapped
 *
 * BufferTrip is called iff:
 *   the buffer is trapped
 * 
 * BufferFill and BufferTrip, in turn call the pool specific methods
 * to allow them to manipulate the buffer state.
 *
 * Also note none of the operations affect which of the three states
 * the buffer is in.  The tricky thing to notice is that Commit can
 * change ap.init, but not (ap.init == 0).  It only changes one
 * non-zero value into another.  To have succeeded the corresponding
 * Reserve, the buffer must have been set, and so
 * 0 < ap.init < ap.alloc.
 *
 * Note:
 * As ap.alloc is only accessed by the allocator, it is not a necessary
 * part of the protocol, a local variable could be used instead, or the
 * addition be recalculated.  However the following invariant is also
 * maintained.
 * .ap.alloc: in a non-busy set buffer ap.alloc == ap.init (i.e. IsReady)
 *
 */

void BufferSet(Buffer buffer, Seg seg, Addr base, Addr init, Addr limit)
{
  AVERT(Buffer, buffer);

  buffer->seg = seg;
  buffer->base = base;
  buffer->init = init;
  buffer->limit = limit;
  buffer->ap.init = init;
  buffer->ap.alloc = init;
  buffer->ap.limit = limit;

  AVER(BufferIsSet(buffer));
  AVER(BufferIsReady(buffer));
}

void BufferReset(Buffer buffer)
{
  AVERT(Buffer, buffer);

  buffer->ap.init = 0;
  buffer->ap.limit = 0;

  /* reset the other fields for extra safety */
  buffer->seg = NULL;
  buffer->base = 0;
  buffer->ap.alloc = 0;

  AVER(BufferIsReset(buffer));
}

void BufferTrap(Buffer buffer)
{
  AVER(BufferIsSet(buffer));

  /* .trap.order: The order of the following instructions is important.
   * A fill or trap _may_ be triggered after the first instruction.
   * An allocation must fail if ap.init is advanced after ap.init
   * is read.  This can only be ensured if limit is zeroed first.
   * see BufferCommit
   */
  buffer->ap.limit = 0;           /* .trap.order */
  /* **** Memory barrier here on the DEC Alpha may be necessary */
  buffer->init = buffer->ap.init; /* .trap.order */

  AVER(BufferIsTrapped(buffer));
}

void BufferUntrap(Buffer buffer)
{
  AVER(BufferIsTrapped(buffer));

  buffer->ap.limit = buffer->limit;
  buffer->ap.init = buffer->init;
  buffer->ap.alloc = buffer->init;

  AVER(BufferIsSet(buffer));
  AVER(BufferIsReady(buffer));
}

/* Buffer Information
 *
 * BufferPoolRing is a convenience function for accessing the ring
 * node which attaches a buffer descriptor to a pool.
 *
 * BufferPool returns the pool to which a buffer is attached.
 *
 * BufferIsReady returns TRUE if the buffer is ready.
 *
 * BufferAP returns the APStruct substructure of a buffer.
 *
 * BufferOfAP is a thread-safe (impl.c.mpsi.thread-safety) method of
 * getting the buffer which owns an APStruct.
 *
 * BufferSpace is a thread-safe (impl.c.mpsi.thread-safety) method of
 * getting the space which owns a buffer.
 */

Bool BufferIsSet(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->ap.limit != 0) {
    AVER(buffer->ap.init != 0);
    AVER(buffer->base != 0);
    AVER(buffer->limit != 0);
    AVER(buffer->seg != NULL);
    return TRUE;
  }
  return FALSE;
}

Bool BufferIsReset(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->ap.init == 0 && buffer->ap.limit == 0) {
    AVER(buffer->base == 0);
    AVER(buffer->ap.alloc == 0);
    AVER(buffer->seg == NULL);
    return TRUE;
  }
  return FALSE;
}

Bool BufferIsTrapped(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->ap.limit == 0 && buffer->ap.init != 0) {
    AVER(buffer->base != 0);
    AVER(buffer->limit != 0);
    AVER(buffer->seg != NULL);
    return TRUE;
  }
  return FALSE;
}


Bool BufferIsReady(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->ap.init == buffer->ap.alloc)
    return TRUE;

  return FALSE;
}

Addr BufferGetInit(Buffer buffer) /* see .ap.inited */
{
  AVERT(Buffer, buffer);

  if(buffer->limit == 0)
    return buffer->init;
  else
    return buffer->ap.init;
}

AP BufferAP(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return &buffer->ap;
}

/* This method must be thread-safe.  See impl.c.mpsi.thread-safety. */
Buffer BufferOfAP(AP ap)
{
  return PARENT(BufferStruct, ap, ap);
}

/* This method must be thread-safe.  See impl.c.mpsi.thread-safety. */
Space BufferSpace(Buffer buffer)
{
  return buffer->space;
}


/* BufferInit/Finish -- initialize/finish a buffer descriptor
 *
 * Init is used by pool classes to initialize a buffer descriptor
 * before it is passed to the client.  The descriptor is initialized
 * with the fill and trip methods, and is attached to the pool
 * buffer ring.  The base, init, alloc, and limit fields are set to
 * zero, so that the fill method will get called the first time a
 * reserve operation is attempted.
 */

void BufferInit(Buffer buffer, Pool pool)
{
  AVERT(Pool, pool);
  AVER(buffer != NULL);

  buffer->base = 0;
  buffer->pool = pool;
  buffer->space = PoolSpace(pool);
  buffer->ap.init = 0;
  buffer->ap.alloc = 0;
  buffer->ap.limit = 0;
  buffer->alignment = pool->alignment;
  buffer->seg = NULL;
  buffer->p = NULL;
  buffer->i = 0;
  buffer->prop = 0;
  buffer->grey = TraceSetEMPTY;
  RingInit(&buffer->traceRing);

  RingInit(&buffer->poolRing);
  RingAppend(&pool->bufferRing, &buffer->poolRing);

  buffer->sig = BufferSig;
  buffer->serial = pool->bufferSerial;
  ++pool->bufferSerial;

  AVERT(Buffer, buffer);
}


void BufferFinish(Buffer buffer)
{
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));

  RingRemove(&buffer->poolRing);
  RingRemove(&buffer->traceRing); /* relies on double remove @@@@ */

  buffer->sig = SigInvalid;
}


/* BufferReserve -- reserve memory from an allocation buffer
 *
 * This is a provided version of the reserve procedure described
 * above.  The size must be aligned according to the buffer
 * alignment.  Iff successful, ResOK is returned and
 * *pReturn updated with a pointer to the reserved memory.
 * Otherwise *pReturn it not touched.  The reserved memory is not
 * guaranteed to have any particular contents.  The memory must be
 * initialized with a valid object (according to the pool to which
 * the buffer belongs) and then passed to the Commit method (see
 * below).  Reserve may not be applied twice to a buffer without a
 * commit in-between.  In other words, Reserve/Commit pairs do not
 * nest.
 */

Res BufferReserve(Addr *pReturn, Buffer buffer, Word size)
{
  Addr next;

  AVER(pReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(BufferIsReady(buffer));

  /* Is there enough room in the unallocated portion of the buffer to */
  /* satisfy the request?  If so, just increase the alloc marker and */
  /* return a pointer to the area below it. */

  next = AddrAdd(buffer->ap.init, size);
  if(next > buffer->ap.init && next <= buffer->ap.limit)
  {
    buffer->ap.alloc = next;
    *pReturn = buffer->ap.init;
    return ResOK;
  }

  /* The buffer can't accommodate the request, the buffer must be
   * trapped, full, or reset.  
   */

  return BufferFill(pReturn, buffer, size);
}


/* BufferFill -- refill a buffer
 *
 * If there is not enough space in a buffer to allocate in-line,
 * BufferFill must be called to "refill" the buffer.  (See the
 * description of the in-line Reserve method in the leader comment.)
 */

Res BufferFill(Addr *pReturn, Buffer buffer, Word size)
{
  Addr next;
  Res res;
  Pool pool;

  AVER(pReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(BufferIsReady(buffer));

  /* If the buffer is trapped then call bufferTrip */
  if(BufferIsTrapped(buffer)) {

    /* Deal with the trapped buffer first, then think about the
     * reserve */
    pool = BufferPool(buffer);
    (*pool->class->bufferTrip)(pool, buffer);

    /* retry the reserve */
    next = AddrAdd(buffer->ap.init, size);
    if(next > buffer->ap.init && next <= buffer->ap.limit)
      goto reserve;
    /* buffer must now be full or reset */
  }
  AVER(!BufferIsTrapped(buffer));
    
  pool = BufferPool(buffer);
  res = (*pool->class->bufferFill)(pool, buffer, size);
  if(res) return res;

  next = AddrAdd(buffer->ap.init, size);
  AVER(next > buffer->ap.init && next <= buffer->ap.limit);

reserve:
  buffer->ap.alloc = next;
  *pReturn = buffer->ap.init;
  return ResOK;
}


/* BufferCommit -- commit memory previously reserved
 *
 * Commit notifies the pool that memory which has been previously
 * reserved (see above) has been initialized with a valid object
 * (according to the pool to which the buffer belongs).  The pointer
 * p must be the same as that returned by Reserve, and the size must
 * match the size passed to Reserve.
 *
 * Commit may not be applied twice to a buffer without a reserve
 * in-between.  In other words, objects must be reserved,
 * initialized, then committed only once.
 *
 * Commit returns TRUE iff successful.  If commit fails and returns
 * FALSE, the client may try to allocate again by going back to the
 * reserve stage, and may not use the memory at p again for any
 * purpose.
 *
 * Some classes of pool may cause commit to fail under rare
 * circumstances.
 */

Bool BufferCommit(Buffer buffer, Addr p, Word size)
{
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));

  AVER(p == buffer->ap.init);
  AVER(AddrAdd(buffer->ap.init, size) == buffer->ap.alloc);

  /* Atomically update the init pointer to declare that the object */
  /* is initialized. */

  /* If a trap occurs before this point, then the allocation fails */
  buffer->ap.init = buffer->ap.alloc;
  /* If a trap occurs after this store, then the allocation succeeds */
  /* See BufferTrap and .ap.inited */

  /* **** Memory barrier here on the DEC Alpha. */

  /* call BufferTrip the buffer if a trap has occurred. */

  if(buffer->ap.limit == 0)
    return BufferTrip(buffer, p, size);

  /* buffer not trapped, so succeed. */

  return TRUE;
}


/* BufferTrip -- act on a tripped buffer
 *
 * The pool which owns a buffer may asyncronously set the buffer limit
 * to zero in order to get control over the buffer.  If this occurs
 * after a Reserve, then the Commit method calls BufferTrip.  (See
 * the description of the in-line Commit in the leader comment.)
 */

Bool BufferTrip(Buffer buffer, Addr p, Word size)
{
  Bool alloced;
  Pool pool;

  AVER(buffer->limit == 0);
  AVER(p != 0);
  AVER(size > 0);

  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));

  alloced = (buffer->ap.init == buffer->init);

  pool = BufferPool(buffer);
  (*pool->class->bufferTrip)(pool, buffer);

  return alloced;
}

Res BufferDescribe(Buffer buffer, Lib_FILE *stream)
{
  AVERT(Buffer, buffer);
  AVER(stream != NULL);

  Lib_fprintf(stream,
             "Buffer %p {\n"
             "  Pool %p\n"
             "  alignment %lu\n"
             "  base 0x%lX  init 0x%lX  alloc 0x%lX  limit 0x%lX\n"
             "  grey 0x%lX"
             "} Buffer %p\n",
             (void *)buffer,
             (void *)BufferPool(buffer),
             (unsigned long)buffer->alignment,
             (unsigned long)buffer->base,
             (unsigned long)buffer->ap.init,
             (unsigned long)buffer->ap.alloc,
             (unsigned long)buffer->ap.limit,
             (unsigned long)buffer->grey,
             (void *)buffer);

  return ResOK;
}
