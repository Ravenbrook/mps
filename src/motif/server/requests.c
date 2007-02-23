/*

 $Header: /project/cmucl/cvsroot/src/motif/server/requests.c,v 1.5 1997/12/31 18:57:55 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "functions.h"
#include "tables.h"

typedef int (*request_f)(message_t);
Boolean must_confirm = False;
int processing_depth = 0;
Garbage garbage_list = NULL;

#include "Interface.h"

static int
already_garbage(void*stuff)
{
  Garbage current = garbage_list;
  while( current ) {
    if (current->junk == (char*)stuff){
      fprintf(stderr,"Redundant add to garbage list.\n");
      return 1;
    }
    current = current->next;
  }
  return 0;
}
    
void register_garbage(void *stuff,garbage_kind kind)
{
  if (!already_garbage(stuff)) {
    Garbage garbage = XtNew(struct garbage_node);
    garbage->junk = (char *)stuff;
    garbage->kind = kind;
    garbage->next = garbage_list;
    garbage_list = garbage;
  }
}

void cleanup_garbage()
{
  Garbage doomed,current = garbage_list;
  
  garbage_list = NULL;
  while( current ) {
    if( current->kind == GarbageXmString )
      XmStringFree( (XmString)current->junk );
    else
      XtFree( current->junk );

    doomed = current;
    current = current->next;
    XtFree( (char *)doomed );
  }
}

void send_confirmation(message_t message,int socket)
{
  message_t reply = message_new(message->serial);

  message_add_packet(reply);
  message_put_dblword(reply,CONFIRM_REPLY);
  message_send(socket,reply);
  message_free(reply);
}

message_t prepare_reply(message_t message)
{
  message_t reply;

  reply = message_new(message->serial);
  /* This is only because the message_new doesn't auto-add one itself, */
  /* which is something that it really ought to do. */
  message_add_packet(reply);
  message_put_dblword(reply,VALUES_REPLY);
  return reply;
}

void reply_with_widget(message_t message,Widget widget)
{
  message_t reply = prepare_reply(message);

  message_write_widget(reply,widget,widget_tag);
  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
}

void reply_with_widgets(message_t message,Widget w1,Widget w2)
{
  message_t reply = prepare_reply(message);

  message_write_widget(reply,w1,widget_tag);
  message_write_widget(reply,w2,widget_tag);
  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
}


void reply_with_integer(message_t message,long value)
{
  message_t reply = prepare_reply(message);

  message_write_int(reply,value,int_tag);
  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
}

void reply_with_boolean(message_t message,Boolean value)
{
  message_t reply = prepare_reply(message);

  message_write_boolean(reply,value,boolean_tag);
  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
}

void reply_with_font_list(message_t message,XmFontList value)
{
  message_t reply = prepare_reply(message);

  message_write_font_list(reply,value,font_list_tag);
  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
}

void reply_with_xmstring(message_t message,XmString s)
{
  message_t reply=prepare_reply(message);

  message_write_xm_string(reply,s,xm_string_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm=False;
}

void reply_with_string(message_t message,String s)
{
  message_t reply=prepare_reply(message);

  message_write_string(reply,s,string_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm=False;
}

void process_request(message_t message,int socket)
{
  int request_op;
  Boolean old_must_confirm = must_confirm;
  request_f handler;

  processing_depth++;
  request_op = message_get_word(message);
  must_confirm = message_get_byte(message);
  message_get_byte(message);   /* This is ignored at the moment */

  handler = request_table[request_op];
  if( global_will_trace ) {
    printf("Dispatching request for %d\n",request_op);
    fflush(stdout);}
  (*handler)(message);
  if( global_will_trace ) {
    printf("Request for %d dispatched, must_confirm: %d\n",request_op,must_confirm);
    fflush(stdout);}
  if( must_confirm )
    send_confirmation(message,socket);
  processing_depth--;
  if( !processing_depth )
    cleanup_garbage();
  must_confirm = old_must_confirm;
}

int QuitServer(message_t message)
{
  close(client_socket);
  if( !will_fork )
    close_sockets();

  exit(0);
}
