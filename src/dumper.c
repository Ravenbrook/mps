/*  impl.c.dumper
 *
 *                   TEST DATABASE DUMPER
 *
 *  $HopeName: MMsrc!dumper.c(MMdevel_protoposm_1.1) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 */


#include "std.h"
#include "pool.h"
#include "poolps.h"
#include "poolpsst.h"
#include "space.h"
#include "trace.h"
#include "prot.h"

#include "dbm.h"
#include "db.h"
#include "mapping.h"

#include "fmtcv.h"

#include <stdio.h>

void dump_db(database_t *db, PoolPS poolPS, FILE *fp);


#define DUMP_MAX_FIELDS 100
typedef struct {
  char offsets[DUMP_MAX_FIELDS];
  Addr *base;
}  object_dump_env_str;

static Error dump_object_fix(Env env, void **pSlot)
{
  object_dump_env_str *s = (object_dump_env_str *) env;
  Addr index = (Addr *) pSlot - s->base;
  if (index < DUMP_MAX_FIELDS)
    s->offsets[index] = 1;
  return ErrSUCCESS;
}

static void dump_object(Addr *objectIO, Format format, FILE *fp)
{
  int i;
  Addr length;
  object_dump_env_str s;
  Addr *object = (Addr *) *objectIO;
  Bool overlength = FALSE;

  length = ((*format->length)((Addr) object)) / sizeof(Addr) ;

  fprintf(fp,"[%8lX]:   %lX  {", (unsigned long) object,
	  (unsigned long) length);

  if (length >= DUMP_MAX_FIELDS) {
    overlength = TRUE;
    length = DUMP_MAX_FIELDS;
  }
  s.base = object;
  for (i = 0; (i < length); i++)
    s.offsets[i] = 0;
  (*format->scan)(objectIO, dump_object_fix,(Env) &s, FALSE);

  for (i = 0; i < length; i++) {
    if (! (i % 4)) fprintf(fp,"\n");
    if (s.offsets[i] == 1)
      fprintf(fp, "   [%8lX] ",(unsigned long) object[i]);
    else
      fprintf(fp, "    %8lX  ",(unsigned long) object[i]);
  }
  if(overlength)
    fprintf(fp, "  ........ }\n");
  else
    fprintf(fp, "  }\n");
}

static void dump_area(Addr base, Addr limit, Format format, FILE* fp)
{
  Addr object = base;
  while (object < limit)
    dump_object(&object,format,fp);
}

static void dump_chunks(database_t *db, PoolPS poolps, FILE *fp)
{
  Addr i;
  Space space;
  Arena arena;
  Shield shield;

  UNUSED(db);

  space = PoolSpace(PoolPSPool(poolps));
  arena = SpaceArena(space);
  shield = SpaceShield(space);

  for (i=0; i<poolps->chunkCount;i++)
  {
    PoolPSChunk chunk = db_GetChunkUsingChunkOffset(poolps, i);

    if (db_GetChunkIdentity(chunk) != kUndefinedChunk)
      fprintf(fp, "\nDISK  CHUNK %6lX", i);
    switch(db_GetChunkIdentity(chunk))
    {
    case kDataChunk:
      if (db_GetChunkState(chunk) != PoolPSChunkStateFREE)
      {
	Addr seg;
	Addr size, limit;
	Addr diskpages;

	ShieldEnter(shield);

        ensureSwizzled(poolps, chunk);

	ShieldLeave(shield);

	seg = db_GetChunkSegment(chunk);
	size = ArenaSegSize(arena, seg);
	limit = seg + size;
	                   /* @@@@ should be disk page size */
	diskpages = size / ArenaGrain(arena);

	if(diskpages > 1)
	{ 
	  i+=diskpages-1;
	  fprintf(fp, " to PAGE %6lX", i);
	}

	fprintf(fp, ":\nData. seg:[%8lX, %8lX)\n", seg, limit);

	dump_area(seg, limit, poolps->format, fp);
      }
      break;
    case kControlChunk:
      fprintf(fp, ": Control Chunk");
      /* @@@@ could call header page dump functions here */
      break;
    case kUndefinedChunk:
      break;
    default:
      NOTREACHED;
    }
  }
  fputc('\n', fp);
}

void dump_db(database_t *db, PoolPS poolPS, FILE *fp)
{
  fprintf(fp, "\nDump of database: %s\n", db->name);
  /* @@@@ more info here */
  dump_chunks(db, poolPS, fp);
}

struct args
{
  Space space;
  char *name;
};


static void *trampfn(void *p, size_t s)
{
  int e;
  Space space;
  Format format;
  Pool pool;
  database_t *db;
  Addr null;
  struct args *args;

  args = (struct args *)p;
  space = args->space;
  UNUSED(s);

  /* We assume the format used in database is the same as the
   * one we are using here
   */
  e = fmtInit(&format, &null, space);
  if(e != ErrSUCCESS) goto ret;

  e = PoolCreate(&pool, PoolClassPS(), space, format);
  if(e != ErrSUCCESS) goto ret;

  e = dbm_OpenDatabase(&db, args->name, args->name, (PoolPS) pool, 0);
  if(e != ErrSUCCESS) goto ret;

  dump_db(db, PoolPoolPS(pool), stdout);

  dbm_CloseDatabase(db);

  PoolDestroy(pool);

ret:
  return (void *)e;
}

int main(int argc, char *argv[])
{
  Error e;
  Space space;
  struct args args;

  if(argc != 2){
    fprintf(stderr, "Usage: %s db-name\n", argv[0]);
    return 1;
  }
  
  args.name = argv[1];

  e = SpaceCreate(&space);
  if(e != ErrSUCCESS) return e;

  args.space = space;

  ProtTramp((void **)&e, trampfn, (void *)&args, 0);

  SpaceDestroy(space);

  return e;
}
