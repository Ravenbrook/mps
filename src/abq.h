/* temporary -- goes in mpm.h */

/* mpmtypes.h */
typedef struct ABQStruct *ABQ;
extern Bool ABQCheck(ABQ abq);
extern Res ABQDescribe(ABQ abq, mps_lib_FILE *stream);
extern Res ABQInit(Arena arena, ABQ abq, Count count);
extern Bool ABQIsEmpty(ABQ abq);
extern void ABQFinish(Arena arena, ABQ abq);
extern Res ABQPop(ABQ abq, CBSBlock *blockReturn);
extern Res ABQPeek(ABQ abq, CBSBlock *blockReturn);
extern Res ABQPush(ABQ abq, CBSBlock block);
extern Res ABQDelete(ABQ abq, CBSBlock block);

/* mpmst.h */
typedef struct ABQStruct
{
  Count count;
  Index head, tail;
  CBSBlock *queue;
  Sig sig;
}ABQStruct;

