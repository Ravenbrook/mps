/* richards.h */
/* Richards benchmark in C++, translated from Smalltalk */
/* uh 2/2/89  */

#include "rbase.h" 
#include "tasks.h"

/*
// RBench class definition
*/

class RBench {
    private:
         TaskControlBlock *taskList, *currentTask;
         Identity currentTaskIdent;
         TaskControlBlock *taskTable[NTASKS];
         int layout;
         int holdCount, queuePacketCount;

         /* creation */
         void CreateDevice (Identity id, int prio,  Packet *work,
                           TaskState *state);
         void CreateHandler(Identity id, int prio,  Packet *work,
                           TaskState *state);
         void CreateIdler  (Identity id, int prio,  Packet *work,
                           TaskState *state);
         void CreateWorker (Identity id, int prio,  Packet *work,
                           TaskState *state);
         void EnterTask(Identity id, TaskControlBlock *t);

         /* scheduling */
         void Schedule();

         /* initializing */
         void InitScheduler();
         void InitTrace();
    public:
         /* task management */
         TaskControlBlock *FindTask(Identity id);
         TaskControlBlock *HoldSelf();
         TaskControlBlock *QueuePacket(Packet *p);
         TaskControlBlock *Release(Identity id);
         TaskControlBlock *Wait();
         /* tracing */
         BOOLEAN tracing;
         void Trace(Identity id);

         void Start(BOOLEAN askTrace);
};

RBench bm;		// benchmark currently executing

