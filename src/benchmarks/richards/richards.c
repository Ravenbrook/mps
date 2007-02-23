/* richards.c - Richards Benchmark in C++ */
/* uh 2/2/89 */

#include <time.h>
#define  clock_t long		/* because Sun library isn't ANSI */
#define  CLK_TCK 1000000
#include "richards.h"   


/*
// creation
*/

void RBench::CreateDevice (Identity id, int prio,  Packet *work,
                           TaskState *state)
{   DeviceTaskRec *data;
    TaskControlBlock *t;

    data = new DeviceTaskRec;
    t = new DeviceTCB(taskList, id, prio, work, state, data);
    EnterTask(id, t);
}


void RBench::CreateHandler(Identity id, int prio,  Packet *work,
                           TaskState *state)
{   HandlerTaskRec *data;
    TaskControlBlock *t;

    data = new HandlerTaskRec;
    t = new HandlerTCB(taskList, id, prio, work, state, data);
    EnterTask(id, t);
}


void RBench::CreateIdler  (Identity id, int prio,  Packet *work,
                           TaskState *state)
{   IdleTaskRec *data;
    TaskControlBlock *t;

    data = new IdleTaskRec;
    t = new IdlerTCB(taskList, id, prio, work, state, data);
    EnterTask(id, t);
}


void RBench::CreateWorker (Identity id, int prio,  Packet *work,
                           TaskState *state)
{   WorkerTaskRec *data;
    TaskControlBlock *t;

    data = new WorkerTaskRec;
    t = new WorkerTCB(taskList, id, prio, work, state, data);
    EnterTask(id, t);
}


void RBench::EnterTask(Identity id, TaskControlBlock *t)
{
    taskList = t;
    taskTable[id] = t;
}


/*
//private
*/

TaskControlBlock *RBench::FindTask(Identity id)
{   TaskControlBlock *t;

    t = taskTable[id];
    if (t == NULL) printf("***error: FindTask failed! ");
    return t;
}


TaskControlBlock *RBench::HoldSelf()
{
    holdCount++;
    currentTask->SetTaskHolding(TRUE);
    return currentTask->Link();
}


TaskControlBlock *RBench::QueuePacket(Packet *p)
{   TaskControlBlock *t;

    t = FindTask(p->Ident());
    queuePacketCount++;
    p->SetLink(NoWork);
    p->SetIdent(currentTaskIdent);
    return t->AddPacket(p, currentTask);
}


TaskControlBlock *RBench::Release(Identity id)
{   TaskControlBlock *t;

    t = FindTask(id);
    t->SetTaskHolding(FALSE);
    return (t->Priority() > currentTask->Priority()) ? t : currentTask;
}


void RBench::Trace(Identity id)
{
    if(! --layout) {printf("\n"); layout = 50;} 
    printf("%d", id + 1);
}


TaskControlBlock *RBench::Wait()
{
    currentTask->SetTaskWaiting(TRUE);
    return currentTask;
}


/*
//scheduling
*/

void RBench::Schedule()
{
    currentTask = taskList;
    while (currentTask != NoTask) {
         if (currentTask->IsTaskHoldingOrWaiting()) 
              currentTask = currentTask->Link();
         else {
              currentTaskIdent = currentTask->Ident();
              if (tracing) Trace(currentTaskIdent);
              currentTask = currentTask->RunTask();
         }
    }
}

/*
//initializing
*/

void RBench::InitScheduler()
{
    queuePacketCount = 0;
    holdCount = 0;
    for (int i = 0; i < NTASKS; i++) {taskTable[i] = NoTask;}
    taskList = NoTask;
}


void RBench::InitTrace()
{   char c;

    printf("Trace (y/n)? ");
    c = getchar();
    tracing = (_toupper(c) == 'Y');
}


void RBench::Start(BOOLEAN trace) {
//    clock_t t1, t2, t3, t4;
    TaskState *t;
    Packet *workQ;

    if (trace) InitTrace(); else tracing = FALSE;
    InitScheduler();
//    t1 = clock();
//    printf("\nRichards benchmark: initializing...\n");
    t = new TaskState; t->Running();				// Idler
    CreateIdler(Idler, 0, NoWork, t);

    workQ = new Packet(NoWork, Worker, WorkPacket);		// Worker
    workQ = new Packet(workQ , Worker, WorkPacket);
    t = new TaskState; t->WaitingWithPacket();
    CreateWorker(Worker, 1000, workQ, t);

    workQ = new Packet(NoWork, DeviceA, DevicePacket);		// HandlerA
    workQ = new Packet(workQ , DeviceA, DevicePacket);
    workQ = new Packet(workQ , DeviceA, DevicePacket);
    t = new TaskState; t->WaitingWithPacket();
    CreateHandler(HandlerA, 2000, workQ, t);

    workQ = new Packet(NoWork, DeviceB, DevicePacket);		// HandlerB
    workQ = new Packet(workQ , DeviceB, DevicePacket);
    workQ = new Packet(workQ , DeviceB, DevicePacket);
    t = new TaskState; t->WaitingWithPacket();
    CreateHandler(HandlerB, 3000, workQ, t);

    t = new TaskState; t->Waiting();				// DeviceA
    CreateDevice(DeviceA, 4000, NoWork, t);
    t = new TaskState; t->Waiting();				// DeviceB
    CreateDevice(DeviceB, 5000, NoWork, t);

//    printf("starting...\n");
//    t2 = clock();
    Schedule();
//    t3 = clock();
//    printf("done.\n");

//    printf("QueuePacketCount = %d, HoldCount = %d.\nThese results are %s",
//           queuePacketCount, holdCount,
//           (queuePacketCount == 23246 && holdCount == 9297) ? 
//                "correct." : "wrong!"
//          );
    if (! (queuePacketCount == 23246 && holdCount == 9297)) {
      printf("error: richards results are incorrect\n");
    }

//    t4 = clock();
//    printf("\nScheduler time = %g seconds, total time = %g\n",
//           (double)(t3 - t2) / CLK_TCK,
//           (double)(t4 - t1) / CLK_TCK);
}


#define ITER  10		/* # of iterations in main loop  */

int main()
{   clock_t t_start, t_stop;

    t_start = clock();
    for (int i = 0; i < ITER; i++) 
        bm.Start(ITER == 1);
    t_stop = clock();

    clock_t diff = (t_stop - t_start)/ITER;

    printf("richards: %d ms\n", diff/(CLK_TCK/1000));

//    if (ITER > 1) 
//         printf("\n*** %d iterations, average of %g secs / iteration.\n",
//		ITER,
//		(double)(t_stop - t_start) / (ITER * CLK_TCK));
}

