/*

 $Header: /project/cmucl/cvsroot/src/motif/server/resources.c,v 1.3 1997/12/31 18:57:58 pw Exp $

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
#include "tables.h"

extern message_t prepare_reply(message_t m);


int RXtSetValues(message_t message)
{
  Widget w;
  ResourceList resources;

  toolkit_read_value(message,&w,XtRWidget);
  resources.class = XtClass(w);
  resources.parent = XtParent(w);
  toolkit_read_value(message,&resources,ExtRResourceList);

  XtSetValues(w,resources.args,resources.length);
}

int RXtGetValues(message_t message)
{
  message_t reply;
  Widget w;
  ResourceList resources;

  toolkit_read_value(message,&w,XtRWidget);
  resources.class = XtClass(w);
  resources.parent = XtParent(w);
  toolkit_read_value(message,&resources,ExtRResourceNames);

  XtGetValues(w,resources.args,resources.length);

  reply = prepare_reply(message);
  message_write_resource_list(reply,&resources,resource_list_tag);
  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
}
