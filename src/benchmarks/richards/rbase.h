/* rbase.h */
#ifndef RBASE

#define RBASE

typedef enum {DevicePacket, WorkPacket} 
	PacketKind;
typedef enum {Idler, Worker, HandlerA, HandlerB, DeviceA, DeviceB}
	Identity;

#define NTASKS 6                        /* # elements of Identity */
				 	/* = # of tasks		  */
#define NoWork NULL
#define NoTask NULL

#endif
