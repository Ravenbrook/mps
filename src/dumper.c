/*  impl.c.dumper
 *
 *                   TEST DATABASE DUMPER
 *
 *  $HopeName: MMsrc!dumper.c(MMdevel_protoposm_1.4) $
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

#include "iohigh.h"
#include "jonathan.h"

void dump_db(database_t *db, PoolPS poolPS, FILE *fp);


#define DUMP_MAX_FIELDS 100
typedef struct {
  char offsets[DUMP_MAX_FIELDS];
  Addr *base;
}  object_dump_env_str;

/*  COPIED FROM BROKER.c */
typedef struct SurrogateData {
  Index       index;
  SurrogateTag tag;
  Value        x;
  Value        y;
} SurrogateData;

static Value readValue (FILE *stream) {
  Value value;
  int   result;
  if (feof(stream))
    seriousError("At EOF");
  if ((result = fread(&value, sizeof(Value), 1, stream)) < 1)
    seriousError
      ("Unable to Read value, result %d, error %d", result, ferror(stream));
  /* printf("Get %d\n", value); */
  return(value);
}
  
static long readLong (FILE *stream) {
  return((long)readValue(stream));
}
static Index readIndex (FILE *stream) {
  return((Index)readValue(stream));
}

static String readString (FILE *stream) {
  long  size = readLong(stream);
  char *buf  = (String)allocate(size + 1);
  if (fread(buf, sizeof(char), size + 1, stream) < (size_t)(size + 1))
    seriousError("Unable to Read string, error %d", ferror(stream));
  /* printf("Get %s\n", buf); */
  return(buf);  
}

static void readImport (FILE* stream, Surrogate surrogate) {
  fread(surrogate, sizeof(SurrogateData), 1, stream);
}

/*  end copied from broker.c */



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

static int tmp_get_chunk_size(void)
{
  return 1;
}


static void dump_data_chuck(PoolPSChunk chunk, Shield shield, PoolPS poolps,
			    Arena arena, FILE *fp)
{
  Addr seg;
  Addr size, limit;
  Addr diskpages;

  ShieldEnter(shield);

/*  ensureSwizzled(poolps, chunk); */
  segLoad(poolps,chunk);

  ShieldLeave(shield);

  seg = chunk_GetSegment(chunk);
  size = ArenaSegSize(arena, seg);
  limit = seg + size;
  /* @@@@ should be disk page size */
  diskpages = size / ArenaGrain(arena);

  tmp_get_chunk_size();

#if 0
  if(diskpages > 1)
    { 
      i+=diskpages-1;
      fprintf(fp, " to PAGE %6lX", i);
    }
#endif

  fprintf(fp, ":\nData. seg:[%8lX, %8lX)\n", seg, limit);

  dump_area(seg, limit, poolps->format, fp);
}

static void new_dump_data_chunk(database_t *db, PoolPS poolps, FILE *fp,
				int pageOffset, int pageCount)
{
  char *buffers;
  int readStatus;
  int buff_size;
  
  buff_size = sizeof(char) * pageCount * kPageSize;
  buffers = (char *) malloc(buff_size);
  readStatus = db_ReadPage(poolps->file,
			   pageOffset,
			   pageCount,
			   (pageCount * kPageSize),
			   buffers);
  if (readStatus == 0)
    seriousError("Unable to read chunk from file on page %d + %d ",
		 pageOffset,pageCount);

  dump_area((Addr) buffers, (Addr) (buffers + buff_size -1),
	    poolps->format,fp);
  
}

static void old_dump_chunks(database_t *db, PoolPS poolps, FILE *fp)
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

fprintf(stderr,"\n\n****** old_dump_chunks %x\n",(int) i);

    if (chunk_GetIdentity(chunk) != kUndefinedChunk)
      fprintf(fp, "\nDISK  CHUNK %6lX", i);
    switch(chunk_GetIdentity(chunk))
    {
    case kDataChunk:
      if (chunk_GetState(chunk) != PoolPSChunkStateFREE)
	dump_data_chuck(chunk,shield,poolps,arena,fp);
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

static headerPage0_t* dump_db_header_page0(database_t *db, PoolPS poolps,
					   FILE *fp)
{
  headerPage0_t* page0;
  fprintf(fp, "\n\nHeader page 0: %s", db->name);
/*  page0 = db_ReadHeaderPage0(db->file); @@@@  correct but does not work*/
  page0 = db_ReadHeaderPage0(poolps->file); /* @@@@ temp hack */
  fprintf(fp,"\n fileType: %s",page0->fileType);
  fprintf(fp,"\n productName:  %s",page0->productName);
  fprintf(fp,"\n productCode:  %s",page0->productCode);
  fprintf(fp,"\n productVersion:  %s",page0->productVersion);
  fprintf(fp,"\n databaseName:  %s",page0->databaseName);
  fprintf(fp,"\n dbUid:  %s","@@@@");
  fprintf(fp,"\n headerVersion:  %s","@@@@");
  fprintf(fp,"\n sizeOfFile:  %d",(int) page0->sizeOfFile);  /* @@@@ */
  fprintf(fp,"\n endianTest1:  %d",(int) page0->endianTest1);  /* @@@@ */
  return page0;
}

static headerPage1_t* dump_db_header_page1(database_t *db, PoolPS poolps, 
					   FILE *fp)
{
  headerPage1_t* page1;
  fprintf(fp, "\n\nHeader page 1: %s", db->name);
  page1 = db_ReadHeaderPage1(poolps->file); /* @@@@ temp hack */
  
  fprintf(fp, "\n controlClusterTable:  %lx",
	  (unsigned long) page1->controlClusterTable);  /* @@@@ */
  fprintf(fp, "\n controlClusterTableSize:  %lx",
	  (unsigned long) page1->controlClusterTableSize);  /* @@@@ */
  fprintf(fp, "\n dataClusterTable:  %lx",
	  (unsigned long) page1->dataClusterTable);  /* @@@@ */
  fprintf(fp, "\n dataClusterTableSize:  %lx",
	  (unsigned long) page1->dataClusterTableSize);  /* @@@@ */
  fprintf(fp, "\n freeSpace:  %d", (int) page1->freeSpace);  /* @@@@ */
  fprintf(fp, "\n schemaVersion:  %d", (int) page1->schemaVersion);  /* @@@@ */
  fprintf(fp, "\n currentLogUid:  %d", (int) page1->currentLogUid);  /* @@@@ */
  fprintf(fp, "\n currentLogName:  %d", (int) page1->currentLogName);  /* @@@@ */
  return page1;
}

static void dump_db_header(database_t *db, PoolPS poolps, FILE *fp,
			   headerPage0_t** page0, headerPage1_t** page1)
{
  fprintf(fp, "\nDump Header: %s\n", db->name);
  *page0 = dump_db_header_page0(db, poolps, fp);
  *page1 = dump_db_header_page1(db, poolps, fp);
  fprintf(fp, "\ndone dumping header: %s\n", db->name);
}

static void dump_db_cluster_table (database_t *db, PoolPS poolps, FILE *fp,
				   int ctOffSet, int maxElementCount,
				   fileClusterTable_t** return_ct)
{
  int size;
  int fillPointer;
  int buffCount = 0;  
  char *ct_buffer;
  fileClusterTable_t* ct;
  int		readStatus;
  int i;

  if (ctOffSet == 0)
    { fprintf(fp,"\n\n ERROR: cluster table offset is hex: %x\n\n",ctOffSet);
      return; }


  size = sizeof(fileClusterTable_t)
    + (sizeof(fileCluster_t) * (maxElementCount - 1));
  fprintf(fp,"\nsize of cluster table in hex %x", size);

  if (size > kPageSize)
    seriousError("Cluster Table to large %d", size);
  else 
    buffCount = 1;  /* @@@@@ */

  ct_buffer = (char *) malloc(sizeof(char) * size);

  readStatus = db_ReadPage(poolps->file,
			   ctOffSet,
			   buffCount,
			   size,
			   ct_buffer);

  if (readStatus == 0)
  {
    seriousError("Unable to find Cluster Table %d", ctOffSet);
  }
  
  ct = (fileClusterTable_t*) ct_buffer;

  fillPointer = ct->fillPointer;

  if (fillPointer > 20)
    { fprintf(fp,"\n Assuming bad cluster table fillPointer %d",fillPointer);
      fprintf(fp,"\n   only printing 4 entries");
      fillPointer = 4; }

  fprintf(fp,"\n\nCluster Table");
  fprintf(fp,"\n location: (in hex) %x", ctOffSet);
  fprintf(fp,"\n fillPointer: (base 10) %d", fillPointer);
  fprintf(fp,"\n maxElementCount: (base 10) %d", maxElementCount);
  for (i = 0; (i < fillPointer); i++)
    fprintf(fp,
	    "\n   [%d]: {id: %x(base 8) offset: %x(base 8) size: %d(base 10)}",
	    i,
	    (unsigned int) ct->data[i].clusterId,
	    (unsigned int) ct->data[i].chunkTable,
	    ct->data[i].chunkTableSize);

  *return_ct = (fileClusterTable_t*) ct_buffer;

/* @@@@  This is returning something the buffer that contains the
   clusterTable, so it has crap after the ct */
}

/* static void dump_chunk_table(void)*/

static void dump_broker(database_t *db, PoolPS poolPS, FILE *fp)
{
  FILE* stream;
  int i;
  String* names;
  fprintf(fp,"\n\nBROKER");
  stream = fopen(poolPS->db->name,"rb");
  { long nameCount = readLong(stream);
    names = (char **) malloc((sizeof(String) * nameCount));
    fprintf(fp,"\nNames %d", (int) nameCount);
    for (i = 0; i < nameCount; i++) {
      int index;
      String value;
      index = (int) readIndex(stream);
      value = readString(stream);
      fprintf(fp,"\n   %d: \"%s\"", index, value);
      names[index] = value;
    } }
  { long importCount = readLong(stream);
    fprintf(fp,"\nImports %d", (int) importCount);
    for (i = 0; i < importCount; i++) {
      SurrogateData surrogate;
      int poolNameIndex;
      int variableNameIndex;

      readImport(stream,&surrogate);

      if (surrogate.tag == NameSurrogate) {
	poolNameIndex = (int)surrogate.x;
	variableNameIndex = (int)surrogate.y;
	fprintf(fp,"\n   %d:  nameSurogate \"%s\":\"%s\"", i,
		names[poolNameIndex],names[variableNameIndex]);
      } else 
	error("Unknown surrogate type tag: %d  i: %d",surrogate.tag,i);
    } }
  { long exportCount = readLong(stream);
    fprintf(fp,"\n\nExports %d", (int) exportCount);
    for (i = 0; i < exportCount; i++) {
      fprintf(fp,"\n   %d  [ %8lX ]",
	      (int) readValue(stream),
	      (unsigned long) readValue(stream));
    } }
  fprintf(fp,"\n end of broker\n");
}

static void dump_chucks_in_ct(database_t *db, PoolPS poolPS, FILE *fp,
			      fileClusterTable_t* control_ct)
{ int i;
  for (i = 6; i < 7; i++) {
    fprintf(fp,"\n\nCHUNK: %8d",i);  /* @@@@ */
    new_dump_data_chunk(db, poolPS, fp,i,1);  /* @@@@ */
  }
}

static void dump_cluster_tables(database_t *db, PoolPS poolPS, FILE *fp,
				headerPage1_t* page1)
{
  fileClusterTable_t *control_ct;
  fileClusterTable_t *data_ct;

  fprintf(fp,"\nControl_ClusterTable: offset: %d  size: %d",
	  (int) page1->controlClusterTable,
	  (int) page1->controlClusterTableSize);
  if (page1->controlClusterTable != 0) {
    dump_db_cluster_table(db, poolPS, fp,
			  page1->controlClusterTable,
			  page1->controlClusterTableSize,
			  &control_ct);
    dump_chucks_in_ct(db, poolPS, fp, control_ct);
  }

  fprintf(fp,"\nData_ClusterTable: offset: %d  size: %d",
	  (int) page1->dataClusterTable,
	  (int) page1->dataClusterTableSize);
  if (page1->dataClusterTable != 0) {
    dump_db_cluster_table(db, poolPS, fp,
			  page1->dataClusterTable,
			  page1->dataClusterTableSize,
			  &data_ct);
    dump_chucks_in_ct(db, poolPS, fp, data_ct);
  }

  if (data_ct == 0) {
    fprintf(fp,"\n old style old_dump_chunks ");
    old_dump_chunks(db, poolPS, fp);
  }
}		

void dump_db(database_t *db, PoolPS poolPS, FILE *fp)
{
  headerPage0_t* page0;
  headerPage1_t* page1;

  fprintf(fp, "\nDump of database: %s\n", db->name);

  dump_db_header(db, poolPS, fp, &page0, &page1);
  
  dump_cluster_tables(db, poolPS, fp, page1);

  dump_broker(db, poolPS, fp);

  fprintf(fp,"\n");
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
