/* tasks.h - definitions of TaskState, TaskControlBlock and xxxTCB 
 * The subclasses of TaskControlBlock differ only in their
 * implementation of the action function 				  */

#ifndef TASKS

#define TASKS

#include "types.h"

/*
// TaskState
*/

class TaskState {
    protected:
        BOOLEAN packetPending, taskWaiting, taskHolding;
	
    public:
        /* initializing */
        TaskState() {packetPending = TRUE; taskWaiting = taskHolding = FALSE;}
	void PacketPending() {packetPending = TRUE; taskHolding = taskWaiting = FALSE;}
	void Waiting() {packetPending = taskHolding = FALSE; taskWaiting = TRUE;}
	void Running() {packetPending = taskWaiting = taskHolding = FALSE;}
	void WaitingWithPacket() {packetPending = taskWaiting = TRUE; taskHolding = FALSE;}
	/* accessing */
	BOOLEAN IsPacketPending() {return packetPending;}
	BOOLEAN IsTaskWaiting() {return taskWaiting;}
	BOOLEAN IsTaskHolding() {return taskHolding;}
	void SetTaskHolding(BOOLEAN state) {taskHolding = state;}
	void SetTaskWaiting(BOOLEAN state) {taskWaiting = state;}
	/* testing */
	BOOLEAN IsRunning() {return !packetPending && !taskWaiting && !taskHolding;}
	BOOLEAN IsTaskHoldingOrWaiting() {return taskHolding || !packetPending && taskWaiting;}
	BOOLEAN IsWaiting() {return !packetPending && taskWaiting && !taskHolding;}
	BOOLEAN IsWaitingWithPacket() {return packetPending && taskWaiting && !taskHolding;}
};

/*
// TaskControlBlock
*/

class TaskControlBlock : public TaskState {

    protected:
        TaskControlBlock *link;
	Identity ident;
	int priority;
	Packet *input;
	void *handle;
	
    public:
        /* initializing */
        TaskControlBlock(TaskControlBlock *l, Identity id, int prio,
                         Packet *initialWork, TaskState *initialState,
                         void *privateData);
	/* accessing */
        Identity Ident() {return ident;}
        int Priority() {return priority;}
        TaskControlBlock *Link() {return link;}
	/* scheduling */
        TaskControlBlock *AddPacket(Packet *p, TaskControlBlock *old);
        virtual TaskControlBlock *ActionFunc(Packet *p, void *handle);
        TaskControlBlock *RunTask();
};

/*
// DeviceTCB 
*/

class DeviceTCB : public TaskControlBlock {
    public:
        DeviceTCB(TaskControlBlock *l, Identity id, int prio,
                  Packet *initialWork, TaskState *initialState,
                  void *privateData) :
                 (l, id, prio, initialWork, initialState, privateData){}
        TaskControlBlock *ActionFunc(Packet *p, void *handle);
};


/*
// HandlerTCB
*/

class HandlerTCB : public TaskControlBlock {
    public:
        HandlerTCB(TaskControlBlock *l, Identity id, int prio,
                   Packet *initialWork, TaskState *initialState,
                   void *privateData) :
                  (l, id, prio, initialWork, initialState, privateData){}
        TaskControlBlock *ActionFunc(Packet *p, void *handle);
};


/*
// IdlerTCB
*/

class IdlerTCB : public TaskControlBlock {
    public:
        IdlerTCB(TaskControlBlock *l, Identity id, int prio,
                 Packet *initialWork, TaskState *initialState,
                 void *privateData) :
                (l, id, prio, initialWork, initialState, privateData){}
        TaskControlBlock *ActionFunc(Packet *p, void *handle);
};


/*
// WorkerTCB
*/

class WorkerTCB : public TaskControlBlock {
    public:
        WorkerTCB(TaskControlBlock *l, Identity id, int prio,
                  Packet *initialWork, TaskState *initialState,
                  void *privateData) :
                 (l, id, prio, initialWork, initialState, privateData){}
        TaskControlBlock *ActionFunc(Packet *p, void *handle);
};

#endif
