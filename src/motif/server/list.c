/*

 $Header: /project/cmucl/cvsroot/src/motif/server/list.c,v 1.3 1997/08/22 20:49:34 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#include <Xm/List.h>
#include <Xm/Command.h>
#include <Xm/SelectioB.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"


/* Functions for interacting with XmList widgets */

void find_items_resources(Widget w, String *items, String *count)
{
  if( XtClass(w) == xmListWidgetClass ) {
    *items = XmNitems;
    *count = XmNitemCount;
  }
  else if( XtClass(w) == xmSelectionBoxWidgetClass ) {
    *items = XmNlistItems;
    *count = XmNlistItemCount;
  }
  else if( XtClass(w) == xmCommandWidgetClass ) {
    *items = XmNhistoryItems;
    *count = XmNhistoryItemCount;
  }
  else
    *items = *count = NULL;
}

int RSetItems(message_t message)
{
  Widget w;
  StringTable items;
  String items_name,count_name;
  Arg args[2];

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&items,XmRXmStringTable);

  find_items_resources(w,&items_name,&count_name);
  if( !items_name ) {
    XtWarning("Invalid widget class in SET-ITEMS.  Ignoring request.");
    return NULL;
  }

  XtSetArg(args[0], items_name, items.data);
  XtSetArg(args[1], count_name, items.length);
  XtSetValues(w, args, 2);
}

int RGetItems(message_t message)
{
  Widget w;
  StringTable items;
  String items_name,count_name;
  Arg args[2];
  message_t reply=prepare_reply(message);

  toolkit_read_value(message,&w,XtRWidget);

  find_items_resources(w,&items_name,&count_name);
  if( !items_name ) {
    XtWarning("Invalid widget class in GET-ITEMS.  Ignoring request.");
    return NULL;
  }

  XtSetArg(args[0], items_name, &items.data);
  XtSetArg(args[1], count_name, &items.length);
  XtGetValues(w, args, 2);

  message_write_xm_string_table(reply, &items, xm_string_table_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm = False;
}

int RXmListAddItem(message_t message)
{
  Widget w;
  XmString item;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  toolkit_read_value(message,&pos,XtRInt);
  XmListAddItem(w,item,pos);
}

int RXmListAddItemUnselected(message_t message)
{
  Widget w;
  XmString item;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  toolkit_read_value(message,&pos,XtRInt);
  XmListAddItemUnselected(w,item,pos);
}

int RXmListDeleteItem(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  XmListDeleteItem(w,item);
}


int RXmListDeletePos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListDeletePos(w,pos);
}

int RXmListDeselectAllItems(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmListDeselectAllItems(w);
}

int RXmListDeselectItem(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  XmListDeselectItem(w,item);
}

int RXmListDeselectPos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListDeselectPos(w,pos);
}

int RXmListSelectItem(message_t message)
{
  Widget w;
  XmString item;
  Boolean notify;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  toolkit_read_value(message,&notify,XtRBoolean);
  XmListSelectItem(w,item,notify);
}

int RXmListSelectPos(message_t message)
{
  Widget w;
  int pos;
  Boolean notify;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  toolkit_read_value(message,&notify,XtRBoolean);
  XmListSelectPos(w,pos,notify);
}

int RXmListSetBottomItem(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  XmListSetBottomItem(w,item);
}

int RXmListSetBottomPos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListSetBottomPos(w,pos);
}

int RXmListSetHorizPos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListSetHorizPos(w,pos);
}

int RXmListSetItem(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  XmListSetItem(w,item);
}

int RXmListSetPos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListSetPos(w,pos);
}

int RXmListAddItems(message_t message)
{
  Widget w;
  StringTable items;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&items,XmRXmStringTable);
  toolkit_read_value(message,&pos,XtRInt);
  XmListAddItems(w,(XmString *)items.data,items.length,pos);
}

int RXmListDeleteAllItems(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmListDeleteAllItems(w);
}

int RXmListDeleteItems(message_t message)
{
  Widget w;
  StringTable items;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&items,XmRXmStringTable);
  XmListDeleteItems(w,(XmString *)items.data,items.length);
}

int RXmListDeleteItemsPos(message_t message)
{
  Widget w;
  int count,pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&count,XtRInt);
  toolkit_read_value(message,&pos,XtRInt);
  XmListDeleteItemsPos(w,count,pos);
}

int RXmListItemExists(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  reply_with_boolean(message,XmListItemExists(w,item));
}

int RXmListItemPos(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  reply_with_integer(message,XmListItemPos(w,item));
}

int RXmListReplaceItems(message_t message)
{
  Widget w;
  StringTable old,new;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&old,XmRXmStringTable);
  toolkit_read_value(message,&new,XmRXmStringTable);
  XmListReplaceItems(w,(XmString *)old.data,old.length,(XmString *)new.data);
}

int RXmListReplaceItemsPos(message_t message)
{
  Widget w;
  StringTable new;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&new,XmRXmStringTable);
  toolkit_read_value(message,&pos,XtRInt);
  XmListReplaceItemsPos(w,(XmString *)new.data,new.length,pos);
}

int RXmListSetAddMode(message_t message)
{
  Widget w;
  Boolean state;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&state,XtRBoolean);
  XmListSetAddMode(w,state);
}

int RXmListGetSelectedPos(message_t message)
{
  Widget w;
  IntList pos;
  Boolean result;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&w,XtRWidget);

  result = XmListGetSelectedPos(w,&pos.data,&pos.length);
  if( !result )
    pos.length = 0;

  message_write_int_list(reply,&pos,int_list_tag);
  message_write_boolean(reply,result,boolean_tag);

  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
  if( result )
    register_garbage(pos.data,GarbageData);
}
