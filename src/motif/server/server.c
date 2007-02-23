/*

 $Header: /project/cmucl/cvsroot/src/motif/server/server.c,v 1.4 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>

#include "global.h"
#include "types.h"
#include "datatrans.h"
#include "tables.h"

extern void process_request(message_t m,int socket);

XtAppContext app_context;
Display *display;

XtActionsRec LispAction;
Boolean terminate_server = False;
Boolean swap_bytes = False;
String global_app_class = NULL, global_app_name = NULL;
 
long next_serial = 0;
int client_socket;
jmp_buf env;

void fatal_error(char *message)
{
  fprintf(stderr,"%s\n",message);
  if( errno )
    perror(NULL);
  fflush(stderr);

  close(client_socket);
  if( !will_fork ) {
    close_sockets();
    abort();
  } else
    exit(-1);
}

void MyErrorHandler(String errmsg)
{
  message_t reply = message_new(next_serial++);
  message_add_packet(reply);

  fprintf(stderr,"Error: %s\n",errmsg);
  fflush(stderr);
  message_put_dblword(reply,ERROR_REPLY);
  message_write_string(reply,errmsg,string_tag);
  message_send(client_socket,reply);
  message_free(reply);

  longjmp(env,1);
  exit(-1);
}

void MyWarningHandler(String errmsg)
{
  message_t reply = message_new(next_serial++);
  message_add_packet(reply);

  fprintf(stderr,"Warning: %s\n",errmsg);
  fflush(stderr);
  message_put_dblword(reply,WARNING_REPLY);
  message_write_string(reply,errmsg,string_tag);
  message_send(client_socket,reply);
  message_free(reply);
}


void get_input(caddr_t closure, int *socket, XtInputId *id)
{
  message_t message;

  if( global_will_trace ) {
    printf("get_input:  Receiving incoming packet.\n");
    fflush(stdout);}
  message = message_read(*socket);

  process_request(message,*socket);

  if( global_will_trace ) {
    printf("get_input:  Successfully digested packet.  Now freeing.\n");
    fflush(stdout);}

  message_free(message);
}

void greet_client(int socket) {
  short byte;
  int result,pad;
  char *dpy_name,*app_class,*app_name;
  message_t first;

  /* Read byte-swap thing */
  result = read(socket,&byte,2);
  if( !result )  fatal_error("greet_client:  Unable to read initial data.");
  else if( result == 1 )  read(socket,&byte+1,1);

  swap_bytes = (byte!=1);
  if( global_will_trace ) {
    printf("swap_bytes is: %d (from %d)\n",swap_bytes,byte);
    fflush(stdout);}

  /* Read initial packet */
  first = message_read(socket);
  if( !first )
    fatal_error("greet_client:  No greeting packet sent.");

  toolkit_read_value(first,&dpy_name,XtRString);
  toolkit_read_value(first,&app_name,XtRString);
  toolkit_read_value(first,&app_class,XtRString);

  global_app_class = XtNewString(app_class);
  global_app_name  = XtNewString(app_name);

  if( global_will_trace ) {
    printf("Opening display on '%s' for app '%s' class '%s'\n",dpy_name,
	   app_name,app_class);
    fflush(stdout);};
  display = XtOpenDisplay(app_context, dpy_name,
			  app_name, app_class,
			  NULL, 0,
			  &global_argc, global_argv);
  if( !display )
    fatal_error("greet_client:  Unable to open display.");

  message_free(first);
}


void serve_client(int socket)
{
  XEvent event;

  XtToolkitInitialize();
  app_context = XtCreateApplicationContext();

  LispAction.string = "Lisp";
  LispAction.proc   = LispActionProc;
  XtAppAddActions(app_context, &LispAction, 1);

  string_token_tag = find_type_entry(ExtRStringToken);
  string_tag = find_type_entry(XtRString);
  xm_string_tag = find_type_entry(XmRXmString);
  enum_tag = find_type_entry(XtREnum);
  int_tag = find_type_entry(XtRInt);
  window_tag = find_type_entry(XtRWindow);
  boolean_tag = find_type_entry(XtRBoolean);
  widget_tag = find_type_entry(XtRWidget);
  function_tag = find_type_entry(XtRFunction);
  callback_reason_tag = find_type_entry(ExtRCallbackReason);
  event_tag = find_type_entry(ExtREvent);
  resource_list_tag = find_type_entry(ExtRResourceList);
  translation_table_tag = find_type_entry(XtRTranslationTable);
  accelerator_table_tag = find_type_entry(XtRAcceleratorTable);
  atom_tag = find_type_entry(XtRAtom);
  font_list_tag = find_type_entry(XmRFontList);
  string_table_tag = find_type_entry(XtRStringTable);
  xm_string_table_tag = find_type_entry(XmRXmStringTable);
  int_list_tag = find_type_entry(ExtRIntList);
  cursor_tag = find_type_entry(XtRCursor);

  client_socket = socket;
  greet_client(socket);

  XtAppAddInput(app_context, socket, (XtPointer)XtInputReadMask,
		(XtInputCallbackProc)get_input, (XtPointer)NULL);

  XtAppSetErrorHandler(app_context, MyErrorHandler);
  XtAppSetWarningHandler(app_context, MyWarningHandler);

  /* Here is where we want to return on errors */
  if( setjmp(env) ) {
    printf("Attempting to recover from error.\n");
    fflush(stdout);}
  while( !terminate_server ) {
    XtAppNextEvent(app_context, &event);
    XtDispatchEvent(&event);
  }

  close(socket);
  XtDestroyApplicationContext(app_context);

  exit(0);
}
