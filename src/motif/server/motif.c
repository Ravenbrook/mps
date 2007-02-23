/*

 $Header: /project/cmucl/cvsroot/src/motif/server/motif.c,v 1.4 2001/03/08 00:08:50 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/Command.h>
#include <Xm/MessageB.h>
#include <Xm/FileSB.h>
#include <Xm/SelectioB.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"

int RXmUpdateDisplay(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmUpdateDisplay(w);
}

int RXmIsMotifWMRunning(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  reply_with_boolean(message,XmIsMotifWMRunning(w));
}

int RXmScrolledWindowSetAreas(message_t message)
{
  Widget w;
  Widget hscroll,vscroll,work_region;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&hscroll,XtRWidget);
  toolkit_read_value(message,&vscroll,XtRWidget);
  toolkit_read_value(message,&work_region,XtRWidget);
  XmScrolledWindowSetAreas(w,hscroll,vscroll,work_region);
}

int RXmTrackingLocate(message_t message)
{
  Widget w;
  Cursor cursor;
  Boolean confine_to;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&cursor,XtRCursor);
  toolkit_read_value(message,&confine_to,XtRBoolean);
  XmTrackingLocate(w,cursor,confine_to);
}

int RXmMenuPosition(message_t message)
{
  Widget w;
  XButtonPressedEvent *event;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&event,ExtREvent);

  XmMenuPosition(w,event);
}



/* Functions for accessing XmCommand widgets */

int RXmCommandAppendValue(message_t message)
{
  Widget widget;
  XmString command;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&command,XmRXmString);
  XmCommandAppendValue(widget,command);
}

int RXmCommandError(message_t message)
{
  Widget widget;
  XmString error;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&error,XmRXmString);
  XmCommandError(widget,error);
}

int RXmCommandSetValue(message_t message)
{
  Widget w;
  XmString value;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&value,XmRXmString);
  XmCommandSetValue(w,value);
}



/* Functions for dealing with XmScale widgets */

int RXmScaleGetValue(message_t message)
{
  Widget widget;
  int value;

  toolkit_read_value(message,&widget,XtRWidget);
  XmScaleGetValue(widget,&value);
  reply_with_integer(message,value);
}

int RXmScaleSetValue(message_t message)
{
  Widget widget;
  int value;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&value,XtRInt);
  XmScaleSetValue(widget,value);
}



/* Functions for dealing with XmToggleButton widgets */

int RXmToggleButtonGetState(message_t message)
{
  Widget w;
  int state;

  toolkit_read_value(message,&w,XtRWidget);
  state = XmToggleButtonGetState(w);
  reply_with_boolean(message,state);
}

int RXmToggleButtonSetState(message_t message)
{
  Widget w;
  Boolean state,notify;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&state,XtRBoolean);
  toolkit_read_value(message,&notify,XtRBoolean);
  XmToggleButtonSetState(w,state,notify);
}



/* Functions for using Tab groups */

int RXmAddTabGroup(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmAddTabGroup(w);
}

int RXmRemoveTabGroup(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmRemoveTabGroup(w);
}

int RXmProcessTraversal(message_t message)
{
  Widget w;
  int direction;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&direction,XtRInt);
  reply_with_boolean(message,XmProcessTraversal(w,direction));
}



/* Functions for getting children from Command/[File]SelectionBox/MessageBox */

#define DEFINE_CHILD_QUERY(query_func)                 \
  Widget w;                                            \
  int which_child;                                     \
                                                       \
  toolkit_read_value(message,&w,XtRWidget);            \
  toolkit_read_value(message,&which_child,XtREnum);    \
  reply_with_widget(message,query_func(w,which_child))

int RXmMessageBoxGetChild(message_t message)
{
  DEFINE_CHILD_QUERY(XmMessageBoxGetChild);
}

int RXmSelectionBoxGetChild(message_t message)
{
  DEFINE_CHILD_QUERY(XmSelectionBoxGetChild);
}

int RXmFileSelectionBoxGetChild(message_t message)
{
  DEFINE_CHILD_QUERY(XmFileSelectionBoxGetChild);
}

int RXmCommandGetChild(message_t message)
{
  DEFINE_CHILD_QUERY(XmCommandGetChild);
}



/* Functions for getting/setting values of XmScrollBar widgets */

int RXmScrollBarGetValues(message_t message)
{
  Widget widget;
  int value,slider_size,increment,page_increment;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&widget,XtRWidget);

  XmScrollBarGetValues(widget,&value,&slider_size,&increment,&page_increment);

  message_write_int(reply,value,int_tag);
  message_write_int(reply,slider_size,int_tag);
  message_write_int(reply,increment,int_tag);
  message_write_int(reply,page_increment,int_tag);

  message_send(client_socket,reply);
  message_free(reply);
  must_confirm = False;
}

int RXmScrollBarSetValues(message_t message)
{
  Widget w;
  int value,slider_size,increment,page_increment;
  Boolean notify;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&value,XtRInt);
  toolkit_read_value(message,&slider_size,XtRInt);
  toolkit_read_value(message,&increment,XtRInt);
  toolkit_read_value(message,&page_increment,XtRInt);
  toolkit_read_value(message,&notify,XtRBoolean);

  XmScrollBarSetValues(w,value,slider_size,increment,page_increment,notify);
}
