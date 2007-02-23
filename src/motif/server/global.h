/*

 $Header: /project/cmucl/cvsroot/src/motif/server/global.h,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#define PACKET_SIZE 4096
#define HEADER_LENGTH 12

#define ExtRResourceList "ResourceList"
#define ExtRStringToken "StringToken"
#define ExtRGrabKind "GrabKind"
#define ExtRResourceNames "ResourceNames"
#define ExtRCallbackReason "CallbackReason"
#define ExtREvent "Event"
#define ExtRIntList "IntList"

typedef enum {GarbageData, GarbageXmString} garbage_kind;

extern int global_argc,client_socket;
extern char **global_argv;
extern XtAppContext app_context;
extern Display *display;
extern String global_app_name,global_app_class;

extern int global_will_trace,will_fork;
extern Boolean terminate_server,swap_bytes,must_confirm;
extern long next_serial;

/* Function prototypes */

extern void fatal_error(char *);
extern void close_sockets();
extern void register_garbage(void *,garbage_kind);

extern void CallbackHandler();
extern void ProtocolHandler();
extern void EventHandler();
extern void NonMaskableHandler();
extern void LispActionProc();
