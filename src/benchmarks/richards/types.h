/* types.h -- basic classes (Packet, {Device,Idle,...}TaskRec)	*/

#include "cbase.h"
#include "rbase.h"

/*
// Packet
*/

class Packet {
    private:
        Packet *link;		// next packet in queue
	Identity ident;		// Idle, Worker, DeviceA, ...
	PacketKind kind;	// DevicePacket or WorkPacket
	int datum;
	char data[4];
    public:
        Packet(Packet *l, Identity id, PacketKind k);
	char *Data() {return data;}
	void SetData(char d[4]) {for(int i=0; i < 4; i++) data[i] = d[i];}
        int Datum() {return datum;}
        void SetDatum(int n) {datum = n;}
	Identity Ident() {return ident;}
	void SetIdent(Identity i) {ident = i;}
	PacketKind Kind() {return kind;}
	void SetKind(PacketKind k) {kind = k;}
	Packet *Link() {return link;}
	void SetLink(Packet *l) {link = l;}
};

/*
// AddToList - list utility (append elem at end of list, return head)
*/
Packet *AddToList(Packet *list, Packet *elem);

/*
// DeviceTaskRec
*/

class DeviceTaskRec {
    private:
        Packet *pending;
    public:
        DeviceTaskRec() {pending = NoWork;}
	Packet *Pending() {return pending;}
	void SetPending(Packet *p) {pending = p;}
};



/*
// IdleTaskRec 
*/

class IdleTaskRec {
    private:
        int control, count;
    public:
        IdleTaskRec() {control = 1; count = 10000;}
        int Control() {return control;}
        void SetControl(int n) {control = n;}
        int Count() {return count;}
        void SetCount(int n) {count = n;}
};


/*
// HandlerTaskRec
*/

class HandlerTaskRec {
    private:
        Packet *workIn, *deviceIn;
    public:
        HandlerTaskRec() {workIn = deviceIn = NoWork;}
        Packet *WorkIn() {return workIn;}
        void SetWorkIn(Packet *p) {workIn = p;}
        Packet *WorkInAdd(Packet *p) {return workIn = AddToList(workIn, p);}
        Packet *DeviceIn() {return deviceIn;}
        void SetDeviceIn(Packet *p) {deviceIn = p;}
        Packet *DeviceInAdd(Packet *p) {return deviceIn = AddToList(deviceIn, p);}
};


/*
// WorkerTaskRec
*/

class WorkerTaskRec {
    private:
        Identity destination;
	int count;
    public:
        WorkerTaskRec() {destination = HandlerA; count = 0;}
        int Count() {return count;}
        void SetCount(int n) {count = n;}
        Identity Destination() {return destination;}
        void SetDestination(Identity d) {destination = d;}
};

