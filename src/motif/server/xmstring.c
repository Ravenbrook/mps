/*

 $Header: /project/cmucl/cvsroot/src/motif/server/xmstring.c,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"


/* Functions for building XmFontLists */

int RXmFontListAdd(message_t message)
{
  XmFontList oldlist,newlist;
  XFontStruct *fstruct;
  Font font;
  XmStringCharSet charset;

  toolkit_read_value(message,&oldlist,XmRFontList);
  toolkit_read_value(message,&font,XtRFont);
  toolkit_read_value(message,&charset,XtRString);

  fstruct = XQueryFont(display,font);
  newlist = XmFontListAdd(oldlist,fstruct,charset);
  reply_with_font_list(message,newlist);
}

#define BOLD_FONT "-*-helvetica-bold-r-normal--11-*"

int RXmFontListCreate(message_t message)
{
  XFontStruct *fstruct;
  Font font;
  XmStringCharSet charset;
  XmFontList flist;

  toolkit_read_value(message,&font,XtRFont);
  toolkit_read_value(message,&charset,XtRString);

  fstruct = XQueryFont(display,font);

  flist = XmFontListCreate(fstruct,charset);
  reply_with_font_list(message,flist);
}

int RXmFontListFree(message_t message)
{
  XmFontList flist;

  toolkit_read_value(message,&flist,XmRFontList);
  XmFontListFree(flist);
}



/* Functions for using XmStrings */

int RXmStringBaseline(message_t message)
{
  XmFontList flist;
  XmString xs;

  toolkit_read_value(message,&flist,XmRFontList);
  toolkit_read_value(message,&xs,XmRXmString);
  reply_with_integer(message,XmStringBaseline(flist,xs));
}

int RXmStringByteCompare(message_t message)
{
  XmString s1,s2;

  toolkit_read_value(message,&s1,XmRXmString);
  toolkit_read_value(message,&s2,XmRXmString);
  reply_with_boolean(message,XmStringByteCompare(s1,s2));
}

int RXmStringCompare(message_t message)
{
  XmString s1,s2;

  toolkit_read_value(message,&s1,XmRXmString);
  toolkit_read_value(message,&s2,XmRXmString);
  reply_with_boolean(message,XmStringCompare(s1,s2));
}

int RXmStringConcat(message_t message)
{
  XmString s1,s2;
  
  toolkit_read_value(message,&s1,XmRXmString);
  toolkit_read_value(message,&s2,XmRXmString);
  reply_with_xmstring(message,XmStringConcat(s1,s2));
}

int RXmStringCopy(message_t message)
{
  XmString s;

  toolkit_read_value(message,&s,XmRXmString);
  reply_with_xmstring(message,XmStringCopy(s));
}

int RXmStringCreate(message_t message)
{
  String s,charset;

  toolkit_read_value(message,&s,XtRString);
  toolkit_read_value(message,&charset,XtRString);
  reply_with_xmstring(message,XmStringCreate(s,charset));
}

int RXmStringCreateLtoR(message_t message)
{
  String s,charset;

  toolkit_read_value(message,&s,XtRString);
  toolkit_read_value(message,&charset,XtRString);
  reply_with_xmstring(message,XmStringCreateLtoR(s,charset));
}

int RXmStringGetLtoR(message_t message)
{
  XmString xs;
  String text,charset;
  Boolean result;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&xs,XmRXmString);
  toolkit_read_value(message,&charset,XtRString);

  result = XmStringGetLtoR(xs,charset,&text);

  message_write_string(reply,text,string_tag);
  message_write_boolean(reply,text,boolean_tag);
  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
}

int RXmStringCreateSimple(message_t message)
{
  String s;

  toolkit_read_value(message,&s,XtRString);
  reply_with_xmstring(message,XmStringCreateSimple(s));
}

int RXmStringEmpty(message_t message)
{
  XmString s;

  toolkit_read_value(message,&s,XmRXmString);
  reply_with_boolean(message,XmStringEmpty(s));
}

int RXmStringExtent(message_t message)
{
  XmFontList flist;
  XmString s;
  Dimension width,height;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&flist,XmRFontList);
  toolkit_read_value(message,&s,XmRXmString);
  XmStringExtent(flist,s,&width,&height);
  message_write_int(reply,width,int_tag);
  message_write_int(reply,height,int_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm=False;
}

int RXmStringFree(message_t message)
{
  XmString s;

  toolkit_read_value(message,&s,XmRXmString);
  XmStringFree(s);
}

int RXmStringHasSubstring(message_t message)
{
  XmString s,subs;

  toolkit_read_value(message,&s,XmRXmString);
  toolkit_read_value(message,&subs,XmRXmString);
  reply_with_boolean(message,XmStringHasSubstring(s,subs));
}

int RXmStringHeight(message_t message)
{
  XmFontList flist;
  XmString s;

  toolkit_read_value(message,&flist,XmRFontList);
  toolkit_read_value(message,&s,XmRXmString);
  reply_with_integer(message,XmStringHeight(flist,s));
}

int RXmStringLength(message_t message)
{
  XmString s;

  toolkit_read_value(message,&s,XmRXmString);
  reply_with_integer(message,XmStringLength(s));
}

int RXmStringLineCount(message_t message)
{
  XmString s;

  toolkit_read_value(message,&s,XmRXmString);
  reply_with_integer(message,XmStringLineCount(s));
}

int RXmStringNConcat(message_t message)
{
  XmString s1,s2;
  int bytes;

  toolkit_read_value(message,&s1,XmRXmString);
  toolkit_read_value(message,&s2,XmRXmString);
  toolkit_read_value(message,&bytes,XtRInt);
  reply_with_xmstring(message,XmStringNConcat(s1,s2,bytes));
}

int RXmStringNCopy(message_t message)
{
  XmString s;
  int bytes;

  toolkit_read_value(message,&s,XmRXmString);
  toolkit_read_value(message,&bytes,XtRInt);
  reply_with_xmstring(message,XmStringNCopy(s,bytes));
}

int RXmStringSeparatorCreate(message_t message)
{
  reply_with_xmstring(message,XmStringSeparatorCreate());
}

int RXmStringWidth(message_t message)
{
  XmFontList flist;
  XmString s;

  toolkit_read_value(message,&flist,XmRFontList);
  toolkit_read_value(message,&s,XmRXmString);
  reply_with_integer(message,XmStringWidth(flist,s));
}
