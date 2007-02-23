/*

 $Header: /project/cmucl/cvsroot/src/motif/server/text.c,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"


/* Functions for using the XmText widgets */

int RXmTextClearSelection(message_t message)
{
  Widget w;
  Time t;

  toolkit_read_value(message,&w,XtRWidget);
  t = XtLastTimestampProcessed(display);
  XmTextClearSelection(w,t);
}

int RXmTextCopy(message_t message)
{
  Widget w;
  Time t;

  toolkit_read_value(message,&w,XtRWidget);
  t = XtLastTimestampProcessed(display);
  reply_with_boolean(message,XmTextCopy(w,t));
}

int RXmTextCut(message_t message)
{
  Widget w;
  Time t;

  toolkit_read_value(message,&w,XtRWidget);
  t = XtLastTimestampProcessed(display);
  reply_with_boolean(message,XmTextCut(w,t));
}

#define DEFINE_TEXT_QUERY(query_func,reply_func) \
  Widget w;                                      \
                                                 \
  toolkit_read_value(message,&w,XtRWidget);      \
  reply_func(message,query_func(w))

int RXmTextGetBaseline(message_t message)
{
  DEFINE_TEXT_QUERY(XmTextGetBaseline,reply_with_integer);
}

int RXmTextGetEditable(message_t message)
{
  DEFINE_TEXT_QUERY(XmTextGetEditable,reply_with_boolean);
}

int RXmTextGetInsertionPosition(message_t message)
{
  DEFINE_TEXT_QUERY(XmTextGetInsertionPosition,reply_with_integer);
}

int RXmTextGetLastPosition(message_t message)
{
  DEFINE_TEXT_QUERY(XmTextGetLastPosition,reply_with_integer);
}

int RXmTextGetMaxLength(message_t message)
{
  DEFINE_TEXT_QUERY(XmTextGetMaxLength,reply_with_integer);
}

int RXmTextGetTopCharacter(message_t message)
{
  DEFINE_TEXT_QUERY(XmTextGetTopCharacter,reply_with_integer);
}

int RXmTextGetSelection(message_t message)
{
  Widget w;
  char *sel;

  toolkit_read_value(message,&w,XtRWidget);
  sel = XmTextGetSelection(w);
  reply_with_string(message,sel);
  register_garbage(sel,GarbageData);
}

int RXmTextGetSelectionPosition(message_t message)
{
  Widget w;
  Boolean result;
  XmTextPosition left,right;
  message_t reply=prepare_reply(message);

  toolkit_read_value(message,&w,XtRWidget);
  result=XmTextGetSelectionPosition(w,&left,&right);

  message_write_boolean(reply,result,boolean_tag);
  message_write_int(reply,left,int_tag);
  message_write_int(reply,right,int_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm=False;
}

int RXmTextGetString(message_t message)
{
  Widget w;
  char *s;

  toolkit_read_value(message,&w,XtRWidget);
  s = XmTextGetString(w);
  reply_with_string(message,s);
  register_garbage(s,GarbageData);
}

int RXmTextInsert(message_t message)
{
  Widget w;
  XmTextPosition pos;
  String value;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  toolkit_read_value(message,&value,XtRString);
  XmTextInsert(w,pos,value);
}

/* These aren't really query functions, but they fit the model of one */
int RXmTextPaste(message_t message)
{
  DEFINE_TEXT_QUERY(XmTextPaste,reply_with_boolean);
}

int RXmTextRemove(message_t message)
{
  DEFINE_TEXT_QUERY(XmTextRemove,reply_with_boolean);
}

int RXmTextPosToXY(message_t message)
{
  Widget w;
  XmTextPosition pos;
  Boolean result;
  Position x,y;
  message_t reply=prepare_reply(message);

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  result=XmTextPosToXY(w,pos,&x,&y);

  message_write_boolean(reply,result,boolean_tag);
  message_write_int(reply,x,int_tag);
  message_write_int(reply,y,int_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm=False;
}

int RXmTextReplace(message_t message)
{
  Widget w;
  XmTextPosition from,to;
  String value;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&from,XtRInt);
  toolkit_read_value(message,&to,XtRInt);
  toolkit_read_value(message,&value,XtRString);
  XmTextReplace(w,from,to,value);
}

#define DEFINE_TEXT_SET(setter,type,reptype)  \
  Widget w;                                   \
  type value;                                 \
                                              \
  toolkit_read_value(message,&w,XtRWidget);   \
  toolkit_read_value(message,&value,reptype); \
  setter(w,value)

int RXmTextScroll(message_t message)
{
  DEFINE_TEXT_SET(XmTextScroll,int,XtRInt);
}

int RXmTextSetAddMode(message_t message)
{
  DEFINE_TEXT_SET(XmTextSetAddMode,int,XtRBoolean);
}

int RXmTextSetEditable(message_t message)
{
  DEFINE_TEXT_SET(XmTextSetEditable,int,XtRBoolean);
}

int RXmTextSetInsertionPosition(message_t message)
{
  DEFINE_TEXT_SET(XmTextSetInsertionPosition,XmTextPosition,XtRInt);
}

int RXmTextSetMaxLength(message_t message)
{
  DEFINE_TEXT_SET(XmTextSetMaxLength,int,XtRInt);
}

int RXmTextSetString(message_t message)
{
  DEFINE_TEXT_SET(XmTextSetString,String,XtRString);
}

int RXmTextSetTopCharacter(message_t message)
{
  DEFINE_TEXT_SET(XmTextSetTopCharacter,XmTextPosition,XtRInt);
}

int RXmTextShowPosition(message_t message)
{
  DEFINE_TEXT_SET(XmTextShowPosition,XmTextPosition,XtRInt);
}

int RXmTextSetHighlight(message_t message)
{
  Widget w;
  XmTextPosition left,right;
  XmHighlightMode mode;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&left,XtRInt);
  toolkit_read_value(message,&right,XtRInt);
  toolkit_read_value(message,&mode,XtREnum);
  XmTextSetHighlight(w,left,right,mode);
}

int RXmTextSetSelection(message_t message)
{
  Widget w;
  XmTextPosition first,last;
  Time t;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&first,XtRInt);
  toolkit_read_value(message,&last,XtRInt);
  t = XtLastTimestampProcessed(display);
  XmTextSetSelection(w,first,last,t);
}

int RXmTextXYToPos(message_t message)
{
  Widget w;
  Position x,y;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&x,XtRInt);
  toolkit_read_value(message,&y,XtRInt);
  reply_with_integer(message,XmTextXYToPos(w,x,y));
}
