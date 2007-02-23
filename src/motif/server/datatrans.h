/*

 $Header: /project/cmucl/cvsroot/src/motif/server/datatrans.h,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef DATATRANS_H
#define DATATRANS_H

#include "message.h"

#define combine_type_and_data(type,data) ((type<<24)|data)

extern void message_write_string();
extern void message_write_widget();
extern void message_write_widget_class();
extern void message_write_function();
extern void message_write_short();
extern void message_write_boolean();
extern void message_write_int();
extern void message_write_xid();
extern void message_write_atom();
extern void message_write_string_token();
extern void message_write_resource_list();
extern void message_write_xm_string();
extern void message_write_enum();
extern void message_write_resource_names();
extern void message_write_widget_list();
extern void message_write_translation_table();
extern void message_write_accelerator_table();
extern void message_write_font_list();
extern void message_write_string_table();
extern void message_write_xm_string_table();
extern void message_write_int_list();
#define message_write_event message_write_int
extern void message_write_color();
/* GCC complains without the full prototype */
extern void message_write_float(message_t,float,int);



extern void message_read_string();
extern void message_read_widget();
extern void message_read_widget_class();
extern void message_read_function();
extern void message_read_int();
extern void message_read_short();
extern void message_read_boolean();
extern void message_read_xid();
extern void message_read_atom();
extern void message_read_string_token();
extern void message_read_resource_list();
extern void message_read_xm_string();
extern void message_read_enum();
extern void message_read_resource_names();
extern void message_read_widget_list();
extern void message_read_translation_table();
extern void message_read_accelerator_table();
extern void message_read_font_list();
extern void message_read_string_table();
extern void message_read_xm_string_table();
extern void message_read_int_list();
#define message_read_event message_read_int
extern void message_read_color();
extern void message_read_float();



extern void toolkit_write_value();
extern void toolkit_read_value();

#endif
