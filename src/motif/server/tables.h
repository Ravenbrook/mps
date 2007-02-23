/*

 $Header: /project/cmucl/cvsroot/src/motif/server/tables.h,v 1.3 1997/08/22 20:49:37 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef TABLES_H
#define TABLES_H

typedef void (*type_writer)(message_t out,caddr_t src,int type_tag);
typedef void (*type_reader)(message_t in,caddr_t dest,int type_tag,int data);

typedef struct {
  String type;
  type_writer writer;
  type_reader reader;
  int size;
} type_entry;

extern String string_table[];
extern WidgetClass *class_table[];
extern type_entry type_table[];

/* These are precomputed */
extern int string_token_tag,string_tag,xm_string_tag,enum_tag;
extern int int_tag,window_tag,boolean_tag,widget_tag,function_tag;
extern int callback_reason_tag,event_tag,resource_list_tag;
extern int translation_table_tag,accelerator_table_tag;
extern int atom_tag,font_list_tag,string_table_tag,xm_string_table_tag;
extern int int_list_tag,cursor_tag;

extern long binary_search(char *tbl,int len,int size,char *t,int (*p)());
extern long tokenize_string(String s);
extern long find_type_entry(String type);
extern long find_widget_class_id(WidgetClass class);
extern String query_resource_type(int classid,Widget Parent,String rsrc);

#endif
