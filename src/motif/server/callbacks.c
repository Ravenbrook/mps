/*

 $Header: /project/cmucl/cvsroot/src/motif/server/callbacks.c,v 1.4 1997/08/22 20:49:31 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/DrawnB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/DrawingA.h>
#include <Xm/DialogS.h>
#include <Xm/BulletinB.h>
#include <Xm/Command.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/ScrollBar.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/Frame.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/ScrolledW.h>
#include <Xm/PanedW.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"

int end_callback_loop = 0;
extern message_t prepare_reply(message_t m);
extern void really_write_string(message_t,String,int);

typedef struct protocol_closure {
  struct protocol_closure *next;
  Widget widget;
  Atom property,protocol;
} ProtocolClosure;

ProtocolClosure *protocol_list = NULL;

XmAnyCallbackStruct *callback_info = NULL;



int TerminateCallback(message_t message)
{
  end_callback_loop--;
}


int RXtAddCallback(message_t message)
{
  Widget w;
  String callback_name;
  int name_token;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&name_token,ExtRStringToken);

  callback_name = string_table[name_token];
  XtAddCallback(w,callback_name,CallbackHandler,(caddr_t)name_token);
}

int RXtRemoveCallback(message_t message)
{
  Widget w;
  int name_token;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&name_token,ExtRStringToken);

  XtRemoveCallback(w,string_table[name_token],
		   CallbackHandler,(caddr_t)name_token);
}

int RXmAddProtocolCallback(message_t message)
{
  Widget w;
  Atom prop,protocol;
  ProtocolClosure *closure;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&prop,XtRAtom);
  toolkit_read_value(message,&protocol,XtRAtom);
  closure = XtNew(ProtocolClosure);
  closure->widget = w;
  closure->property = prop;
  closure->protocol = protocol;
  closure->next = protocol_list;
  protocol_list = closure;
  XmAddProtocolCallback(w,prop,protocol,ProtocolHandler,closure);
}

int RXmRemoveProtocolCallback(message_t message)
{
  Widget w;
  Atom prop,protocol;
  ProtocolClosure *closure,*current;
  int found = False;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&prop,XtRAtom);
  toolkit_read_value(message,&protocol,XtRAtom);

  current = protocol_list;
  if( current->widget==w && current->property==prop &&
     current->protocol==protocol ) {
    protocol_list = current->next;
    closure = current;
  }
  else
    while( current->next && !found ) {
      closure = current->next;
      found = (closure->widget==w && closure->property==prop &&
	       closure->protocol==protocol);
      if( found )
	current->next = closure->next;
      else
	current = current->next;
    }

  if( !found ) XtError("RXmRemoveProtocolCallback:  Unable to find closure.");

  XmRemoveProtocolCallback(w,prop,protocol,ProtocolHandler,closure);
  XtFree( (char *)closure );
}


int RReturnTextCallbackDoit(message_t message)
{
  Boolean doit;
  XmTextVerifyPtr info = (XmTextVerifyPtr)callback_info;

  toolkit_read_value(message,&doit,XtRBoolean);
  
  if( info )
    info->doit = doit;
  else
    XtWarning("RReturnTextCallbackDoit:  No valid callback info exists.");
}



void write_button_callback(message_t reply,XmPushButtonCallbackStruct *info)
{
  message_write_int(reply,info->click_count,int_tag);
}

void write_drawing_callback(message_t reply,XmDrawingAreaCallbackStruct *info)
{
  message_write_xid(reply,info?info->window:0,window_tag);
}

void write_drawn_button_callback(message_t reply,
				 XmDrawnButtonCallbackStruct *info)
{
  message_write_xid(reply,info?info->window:0,window_tag);
  message_write_int(reply,info?info->click_count:0,int_tag);
}

void write_scrollbar_callback(message_t reply,XmScrollBarCallbackStruct *info)
{
  message_write_int(reply,info?info->value:0,int_tag);
  message_write_int(reply,info?info->pixel:0,int_tag);
}

void write_toggle_callback(message_t reply,XmToggleButtonCallbackStruct *info)
{
  message_write_boolean(reply,info?info->set:0,boolean_tag);
}

void write_text_callback(message_t reply,XmTextVerifyCallbackStruct *info)
{
  message_write_boolean(reply,info?info->doit:0,boolean_tag);
  message_write_int(reply,info?info->currInsert:0,int_tag);
  message_write_int(reply,info?info->newInsert:0,int_tag);

  if(info) {
     if( info->reason == XmCR_LOSING_FOCUS ) {
        message_write_int(reply,info->startPos,int_tag);
        message_write_int(reply,info->endPos,int_tag);
        }
     else if( info->reason == XmCR_MODIFYING_TEXT_VALUE ) {
        message_write_int(reply,info->startPos,int_tag);
        message_write_int(reply,info->endPos,int_tag);
        really_write_string(reply,info->text->ptr,info->text->length+1);
        /* ***** Perhaps this should be an enumerated type ***** */
        message_write_int(reply,info->text->format,int_tag);
        printf("modifying_text_value: %s ; length=%d\n",info->text->ptr,
               info->text->length);
        fflush(stdout);
        }
     }
}

void write_list_callback(message_t reply,XmListCallbackStruct *info)
{
  StringTable item_table;
  IntList pos_table;

  message_write_xm_string(reply,info?info->item:0,xm_string_tag);
  message_write_int(reply,info?info->item_position:0,int_tag);

  if(info && (info->reason==XmCR_MULTIPLE_SELECT || info->reason==XmCR_EXTENDED_SELECT)){
    item_table.data = (char **)info->selected_items;
    item_table.length = info->selected_item_count;
    message_write_xm_string_table(reply,&item_table,xm_string_table_tag);

    pos_table.data = info->selected_item_positions;
    pos_table.length = info->selected_item_count;
    message_write_int_list(reply,&pos_table,int_list_tag);
    message_write_int(reply,info->selection_type,int_tag);
  }
}

void write_selection_callback(message_t reply, XmCommandCallbackStruct *info)
{
  message_write_xm_string(reply,info?info->value:0,xm_string_tag);
}

void write_fileselection_callback(message_t reply,
				  XmFileSelectionBoxCallbackStruct *info)
{
  message_write_xm_string(reply,info?info->value:0,xm_string_tag);
  message_write_xm_string(reply,info?info->mask:0,xm_string_tag);
  message_write_xm_string(reply,info?info->dir:0,xm_string_tag);
  message_write_xm_string(reply,info?info->pattern:0,xm_string_tag);
}

void write_scale_callback(message_t reply,XmScaleCallbackStruct *info)
{
  message_write_int(reply,info?info->value:0,int_tag);
}

void CallbackHandler(Widget *w, int name_token, XmAnyCallbackStruct *info)
{
  WidgetClass class = XtClass(*w);
  int exit_value;
  XEvent event;
  message_t reply = message_new(next_serial++);
  message_add_packet(reply);

  callback_info = info;

  message_put_dblword(reply,CALLBACK_REPLY);
  message_write_widget(reply,*w,widget_tag);
  message_write_string_token(reply,name_token,string_token_tag);
  
  /* Now, we write the Reason structure into the message */
  message_write_enum(reply,info?info->reason:0,callback_reason_tag);
  message_write_int(reply,info?info->event:0,int_tag);

  if( class==xmArrowButtonWidgetClass || class==xmArrowButtonGadgetClass ||
     class==xmPushButtonWidgetClass || class==xmPushButtonGadgetClass )
    write_button_callback(reply,(XmPushButtonCallbackStruct *)info);
  else if( class==xmDrawingAreaWidgetClass )
    write_drawing_callback(reply,(XmDrawingAreaCallbackStruct *)info);
  else if( class==xmDrawnButtonWidgetClass )
    write_drawn_button_callback(reply,(XmDrawnButtonCallbackStruct *)info);
  else if( class==xmScrollBarWidgetClass )
    write_scrollbar_callback(reply,(XmScrollBarCallbackStruct *)info);
  else if( class==xmToggleButtonWidgetClass ||
	   class==xmToggleButtonGadgetClass )
    write_toggle_callback(reply,(XmToggleButtonCallbackStruct *)info);
  else if( class==xmListWidgetClass )
    write_list_callback(reply,(XmListCallbackStruct *)info);
  else if( class==xmSelectionBoxWidgetClass || class==xmCommandWidgetClass )
    write_selection_callback(reply,(XmCommandCallbackStruct *)info);
  else if( class==xmFileSelectionBoxWidgetClass )
    write_fileselection_callback(reply,
				 (XmFileSelectionBoxCallbackStruct *)info);
  else if( class==xmScaleWidgetClass )
    write_scale_callback(reply,(XmScaleCallbackStruct *)info);
  else if( class==xmTextWidgetClass || class==xmTextFieldWidgetClass )
    if( info && (info->reason==XmCR_LOSING_FOCUS ||
        info->reason==XmCR_MODIFYING_TEXT_VALUE ||
        info->reason==XmCR_MOVING_INSERT_CURSOR ))
      write_text_callback(reply,(XmTextVerifyCallbackStruct *)info);

  message_send(client_socket,reply);
  message_free(reply);

  exit_value = end_callback_loop++;

/* *** This version handles no events until callbacks are done processing */
  while( exit_value<end_callback_loop )
    XtAppProcessEvent(app_context,XtIMAlternateInput);

/* *** Use this version to allow callbacks to occur during callback processing
   *** In general, this is not something the you want to do.  For instance,
   *** the callback_info mechanism for supporting modifications to the
   *** callback structure would no longer work if callbacks could be nested.
  while( exit_value<end_callback_loop ) {
    XtAppNextEvent(app_context, &event);
    XtDispatchEvent(&event);
  }
*/

  callback_info = NULL;
}



void ProtocolHandler(Widget *w, ProtocolClosure *c, XmAnyCallbackStruct *info)
{
  int exit_value;
  XEvent event;
  message_t reply = message_new(next_serial++);
  message_add_packet(reply);

  message_put_dblword(reply,PROTOCOL_REPLY);
  message_write_widget(reply,*w,widget_tag);
  message_write_atom(reply,c->property,atom_tag);
  message_write_atom(reply,c->protocol,atom_tag);
  message_write_int(reply,info->event,int_tag);

  message_send(client_socket,reply);
  message_free(reply);

  exit_value = end_callback_loop++;
  while( exit_value<end_callback_loop )
    XtAppProcessEvent(app_context,XtIMAlternateInput);

  /* **** The non-blocking version
  while( exit_value<end_callback_loop ) {
    XtAppNextEvent(app_context, &event);
    XtDispatchEvent(&event);
  }
  */
}
