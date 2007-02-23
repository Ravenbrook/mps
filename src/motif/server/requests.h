/*

 $Header: /project/cmucl/cvsroot/src/motif/server/requests.h,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

extern message_t prepare_reply(message_t message);
extern void reply_with_widget(message_t,Widget);
extern void reply_with_widgets(message_t,Widget,Widget);
extern void reply_with_integer(message_t,long);
extern void reply_with_boolean(message_t,Boolean);
extern void reply_with_font_list(message_t,XmFontList);
extern void reply_with_xmstring(message_t,XmString);
extern void reply_with_string(message_t,String);
