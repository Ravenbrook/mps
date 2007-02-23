/* tasks.c */

#include "tasks.h"
#include "richards.h"

/*
// TaskControlBlock
*/

TaskControlBlock::TaskControlBlock(TaskControlBlock *l, Identity id, int prio,
                         Packet *initialWork, TaskState *initialState,
                         void *privateData)
{
    link = l; ident = id; priority = prio; input = initialWork;
    packetPending = initialState->IsPacketPending();
    taskWaiting = initialState->IsTaskWaiting();
    taskHolding = initialState->IsTaskHolding();
    handle = privateData;
}


TaskControlBlock *TaskControlBlock::AddPacket(Packet *p,
                                              TaskControlBlock *oldTask)
{
    if (input == NoWork) {
         input = p;
         packetPending = TRUE;
         if (priority > oldTask->priority) return this;
    }
    else AddToList(input, p);
    return oldTask;
}


TaskControlBlock *TaskControlBlock::RunTask()
{   Packet *msg;

    if (IsWaitingWithPacket()) {
         msg = input;
         input = input->Link();
         if (input == NoWork)
              Running();
         else PacketPending();
    }
    else msg = NoWork;
    return this->ActionFunc(msg, handle);
}


/*
// action functions
*/

TaskControlBlock *TaskControlBlock::ActionFunc(Packet *p, void *handle)
{
    p, handle, 0;
    printf("***error: virtual TaskControlBlock.ActionFunc called!\n");
    return NoTask;
}


TaskControlBlock *DeviceTCB::ActionFunc(Packet *p, void *handle)
{   DeviceTaskRec *data = (DeviceTaskRec *)handle;

    if (p == NoWork) {
         if ((p = data->Pending()) == NoWork)
              return bm.Wait();
         else {
              data->SetPending(NoWork);
              return bm.QueuePacket(p);
         }
    }
    else {
         data->SetPending(p);
         if (bm.tracing) bm.Trace(p->Datum());
         return bm.HoldSelf();
    }
}


TaskControlBlock *HandlerTCB::ActionFunc(Packet *p, void *handle)
{   HandlerTaskRec *data = (HandlerTaskRec *)handle;
    Packet *work, *devicePacket;
    int count;

    if (p != NoWork) {
         if (p->Kind() == WorkPacket)
              data->WorkInAdd(p);
         else data->DeviceInAdd(p);
    }
    if ((work = data->WorkIn()) == NoWork)
         return bm.Wait();
    else {
         count = work->Datum();
         if (count > 4) {
              data->SetWorkIn(work->Link());
              return bm.QueuePacket(work);
         }
         else {
              if ((devicePacket = data->DeviceIn()) == NoWork)
                   return bm.Wait();
              else {
                   data->SetDeviceIn(devicePacket->Link());
                   devicePacket->SetDatum(work->Data()[count-1]);
                   work->SetDatum(count + 1);
                   return bm.QueuePacket(devicePacket);
              }
         }
    }
}


TaskControlBlock *IdlerTCB::ActionFunc(Packet *p, void *handle)
/* p is not used here */
{   IdleTaskRec *data = (IdleTaskRec *)handle;

    p, 0;

    data->SetCount(data->Count() - 1);
    if (data->Count() == 0)
         return bm.HoldSelf();
    else if ((data->Control() & 1) == 0) {
              data->SetControl(data->Control() / 2);
              return bm.Release(DeviceA);
         }
         else {
              data->SetControl((data->Control() / 2) ^ 53256);
              return bm.Release(DeviceB);
         }
}


TaskControlBlock *WorkerTCB::ActionFunc(Packet *p, void *handle)
{   WorkerTaskRec *data = (WorkerTaskRec *)handle;
    Identity dest;

    if (p == NoWork)
         return bm.Wait();
    else {
         dest = (data->Destination() == HandlerA) ? HandlerB : HandlerA;
         data->SetDestination(dest);
         p->SetIdent(dest);
         p->SetDatum(1);
         for(int i = 0; i < 4; i++) {
              data->SetCount(data->Count() + 1);
              if (data->Count() > 26) data->SetCount(1);
              p->Data()[i] = 'A' + data->Count();
         }
         return bm.QueuePacket(p);
    }
}


