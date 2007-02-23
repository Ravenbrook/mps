/*

 $Header: /project/cmucl/cvsroot/src/motif/server/datatrans.c,v 1.8 2000/02/15 11:59:25 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#ifdef SVR4
#define bzero(a,n) memset(a, 0, n)
#endif

#include "global.h"
#include "types.h"
#include "datatrans.h"
#include "tables.h"

void packet_write_string(packet_t packet,String string,int count)
{
  strncpy(packet->fill,string,count);
  packet->fill += count;
  packet->length += count;
}

void message_write_string_token(message_t message,int token,int tag)
{
  message_put_dblword(message,combine_type_and_data(tag,token));
}

void really_write_string(message_t message,String string,int length)
{
  int pad,i;
  packet_t packet;

  message_put_dblword(message,combine_type_and_data(string_tag,length));
  packet = message->packets;
  if( length+packet->length < PACKET_SIZE )
    packet_write_string(packet,string,length);
  else if( length < (PACKET_SIZE-HEADER_LENGTH) ) {
    message_add_packet(message);
    packet_write_string(message->packets,string,length);
  }
  else {
    if(0)fatal_error("really_write_string:  Attempt to send huge string.");
    for(i=0; i<length; i++)
      message_put_byte(message,string[i]);
    packet = message->packets;
  }
  /* Add in the padding bytes at the end of the string */
  pad = (4-((packet->fill-packet->data)%4))%4;
  for(i=0;i<pad;i++)
    packet_put_byte(packet,0);
}


void message_write_string(message_t message,String string,int type_tag)
{
  long token = tokenize_string(string);

  if( token >= 0 )
    message_put_dblword(message,combine_type_and_data(string_token_tag,token));
  else
    really_write_string(message,string,strlen(string)+1);
}

void message_write_short(message_t message,int value,int type_tag)
{
  message_put_dblword(message,combine_type_and_data(type_tag,value));
}

void message_write_boolean(message_t message,int value,int type_tag)
{
  message_put_dblword(message,combine_type_and_data(type_tag,value));
}

void message_write_int(message_t message,int value,int type_tag)
{
  message_put_dblword(message,combine_type_and_data(type_tag,0));
  message_put_dblword(message,value);
}

void message_write_function(message_t message, int value,int type_tag)
{
  message_put_dblword(message,combine_type_and_data(type_tag,value));
}

void message_write_widget(message_t message,Widget widget,int type_tag)
{
  message_put_dblword(message,combine_type_and_data(type_tag,0));
  message_put_dblword(message,(long)widget);
}

void message_write_widget_class(message_t message,WidgetClass class,int tag)
{
  int i=0;

  do {
    if( *class_table[i] == class ) {
      message_put_dblword(message,combine_type_and_data(tag,i));
      return;
    }
  } while( class_table[++i] );

  fatal_error("message_write_widget_class:  Unknown widget class.");
}

void message_write_xid(message_t message,XID value,int tag)
{
  message_put_dblword(message,combine_type_and_data(tag,0));
  message_put_dblword(message,value);
}

void message_write_atom(message_t message,Atom value,int tag)
{
  message_put_dblword(message,combine_type_and_data(tag,0));
  message_put_dblword(message,value);
}

void message_write_enum(message_t message,int enumval,int tag)
{
  message_put_dblword(message,combine_type_and_data(tag,enumval));
}

void warn_bogus_resource(String name)
{
  char buf[128];

  sprintf(buf,"Unknown resource: %s",name);
  XtWarning(buf);
}

void message_write_resource_list(message_t message,ResourceList *list,int tag)
{
  String type;
  int i,classid = find_widget_class_id(list->class);

  message_put_dblword(message,combine_type_and_data(tag,list->length));
  /* Note:  We will only write the values, not the names */
  for(i=0;i<list->length;i++) {
    type = query_resource_type(classid,list->parent,list->args[i].name);

    if( !type ) {
      warn_bogus_resource(list->args[i].name);
      message_write_boolean(message,0,boolean_tag);  /* Sends nil */
    }
    else {
       int size;
       long type_tag = find_type_entry(type);
       unsigned long val;
       if( type_tag < 0 ) {
          fprintf(stderr,"Choked on type %s\n",type);
          fflush(stderr);
          fatal_error("Illegal type specified.");
          }
    
       size = type_table[type_tag].size;
       switch(size) {
        case 1:
          val = *((unsigned char*)(list->args[i].value));
          break;
        case 2:
          val = *((unsigned short*)(list->args[i].value));
          break;
        case 4:
          val = *((unsigned long*)(list->args[i].value));
          break;
        default:
          fprintf(stderr, "Bad size %d for %s\n", size, type);
          fflush(stderr);
          fatal_error("Bad size.");
          break;
          }
      toolkit_write_value(message,val,type);
      if( !strcmp(type,XmRXmString) )
	/* All XmString's returned as resource values are garbage */
	register_garbage(*((XmString *)(list->args[i].value)),GarbageXmString);
    }
  }
}

void message_write_int_list(message_t message,IntList *list,int tag)
{
  int i;

  message_put_dblword(message,combine_type_and_data(tag,list->length));
  for(i=0;i<list->length;i++)
    message_write_int(message,list->data[i],int_tag);
}

void message_write_widget_list(message_t message,MyWidgetList *list,int tag)
{
  fprintf(stderr,">>>>> Warning:write_widget_list:We shouldn't be here!\n");
  fflush(stderr);
}

void message_write_resource_names(message_t message,ResourceList *list,int tag)
{
  fprintf(stderr,">>>>> Warning:write_resource_names:We shouldn't be here!\n");
  fflush(stderr);
}

void message_write_xm_string(message_t message,XmString xs,int tag)
{
  message_put_dblword(message,combine_type_and_data(tag,0));
  message_put_dblword(message,(long)xs);
}

void message_write_translation_table(message_t m,XtTranslations t,int tag)
{
  message_put_dblword(m,combine_type_and_data(tag,0));
  message_put_dblword(m,(unsigned long)t);
}

void message_write_accelerator_table(message_t m,XtAccelerators a,int tag)
{
  message_put_dblword(m,combine_type_and_data(tag,0));
  message_put_dblword(m,(unsigned long)a);
}

void message_write_font_list(message_t m,XmFontList flist,int tag)
{
  message_put_dblword(m,combine_type_and_data(tag,0));
  message_put_dblword(m,(unsigned long)flist);
}

void message_write_string_table(message_t m,StringTable *items,int tag)
{
  int i;

  message_put_dblword(m,combine_type_and_data(tag,items->length));
  for(i=0;i<items->length;i++)
    message_write_string(m,items->data[i],string_tag);
}

void message_write_xm_string_table(message_t m,StringTable *items,int tag)
{
  int i;

  message_put_dblword(m,combine_type_and_data(tag,items->length));
  for(i=0;i<items->length;i++)
    message_write_xm_string(m,(XmString)items->data[i],xm_string_tag);
}

void message_write_color(message_t m,XColor *color,int tag)
{
  message_put_dblword(m,combine_type_and_data(tag,color->red));
  message_put_word(m,color->green);
  message_put_word(m,color->blue);
}

void message_write_float(message_t m,float f,int tag)
{
  message_put_dblword(m,combine_type_and_data(tag,0));
  message_put_dblword(m,f);
}



void message_read_widget(message_t message,Widget *w,int tag,int data)
{
  *w = (Widget)message_get_dblword(message);
}

void message_read_widget_class(message_t message,WidgetClass *c,
			       int tag,int data)
{
  *c = *(class_table[data]);
}

void message_read_function(message_t message,int *f,int tag,int data)
{
  *f = data;
}

String really_read_string(message_t message,int length)
{
  int pad,i;
  packet_t packet=message->packets;
  String string;
  if( length <= packet->length - (packet->fill - packet->data)){
    string = packet->fill;
    packet->fill += length;
  }
  else if (length < (PACKET_SIZE - HEADER_LENGTH)){
    packet = message->packets = packet->next;
    string = packet->fill;
    packet->fill += length;
  }
  else {
    string = XtMalloc(length * sizeof(*string));
    register_garbage(string, GarbageData);
    for(i=0; i<length; i++)
      string[i] = message_get_byte(message);
    packet = message->packets;
  }

  /*
   * This is a pretty gross way of finding out how many padding bytes
   * there should be.  The outermost %4 should perhaps be replaced with
   * an if(pad<4) around the for loop.
   */
  pad = (4-((packet->fill-packet->data)%4))%4;
  for(i=0;i<pad;i++)
    packet_get_byte(packet);

  return string;
}

void message_read_string_token(message_t message,int *t,int tag,int data)
{
  *t = data;
}

void message_read_string(message_t message,String *s,int tag,int data)
{
  if( tag == string_token_tag )
    *s = string_table[data];
  else 
    *s = really_read_string(message,data);
}

void message_read_xm_string(message_t message,XmString *xs,int tag,int data)
{
  String tmp;
  XmString xmstring;

  if( tag == string_tag ) {
    tmp = really_read_string(message,data);
    xmstring = XmStringCreateLtoR(tmp,XmSTRING_DEFAULT_CHARSET);
    if( !xmstring )
      fatal_error("message_read_xm_string:  Failed to convert string.");
    *xs = xmstring;
    register_garbage(xmstring,GarbageXmString);
  }
  else
    *xs = (XmString)message_get_dblword(message);
}

/* used to be int *val here, but many places pass address of Boolean into
   things that call this function, resulting in memory overwrite and
   occasional bus errors. But, the resource list function calls this on the
   resource list value, which is an int. So the solution is to make everything
   that calls this function pass the address of a Boolean except for the
   resource list function, which zeros the value beforehand because only part
   is written. I still haven't looked up the real definition of an Xlib
   boolean to see if this is valid or not. */
void message_read_boolean(message_t message,Boolean *val,int tag,int data)
{
  *val = (data?1:0);
}

void message_read_int(message_t message,int *i,int tag,int data)
{
  *i = message_get_dblword(message);
}

void message_read_short(message_t message,int *i,int tag,int data)
{
  *i = data;
}

void message_read_xid(message_t message,XID *id,int tag,int data)
{
  *id = message_get_dblword(message);
}

void message_read_atom(message_t message,Atom *a,int tag,int data)
{
  *a = message_get_dblword(message);
}

void message_read_enum(message_t message,int *enumval,int tag,int data)
{
  *enumval = data;
}

void message_read_resource_names(message_t message,ResourceList *list,
				 int tag,int length)
{
  int i,classid=find_widget_class_id(list->class);
  String type;
  long *data;

  list->length = length;
  if( length>0 ) {
    list->args = (ArgList)XtMalloc(length*sizeof(Arg));
    register_garbage(list->args, GarbageData);

    /* Allocate region to store data into */
    data = (long *)XtMalloc( sizeof(long)*length );
    bzero(data,sizeof(long)*length);
    register_garbage(data,GarbageData);

    for(i=0;i<length;i++) {
      toolkit_read_value(message,&list->args[i].name,XtRString);
      list->args[i].value = (long)&data[i];
    }
  } else list->args = NULL;
}


void message_read_resource_list(message_t message,ResourceList *list,
				int tag,int length)
{
  int i,classid;
  String name,type;

  length /= 2;
  list->length = length;

  classid = find_widget_class_id(list->class);
  if( length > 0 ) {
    list->args = (ArgList)XtMalloc(length*sizeof(Arg));
    register_garbage(list->args,GarbageData);
    for(i=0;i<length;i++) {
      toolkit_read_value(message,&name,XtRString);
      type = query_resource_type(classid,list->parent,name);
      if( !type )
	warn_bogus_resource(name);
      list->args[i].value = 0;  /* booleans don't write whole word; see
                                   message_read_boolean comment */
      toolkit_read_value(message,&(list->args[i].value),type);
      list->args[i].name = name;

    }
  } else list->args = NULL;
}

void message_read_widget_list(message_t message,MyWidgetList *list,
			      int tag,int length)
{
  int i;

  list->length = length;
  if( length>0 ) {
    list->widgets = (WidgetList)XtMalloc(length*sizeof(Widget));
    register_garbage(list->widgets,GarbageData);
    for(i=0;i<length;i++)
      toolkit_read_value(message,&(list->widgets[i]),XtRWidget);
  } else list->widgets = NULL;
}

void message_read_int_list(message_t message,IntList *list,int tag,int length)
{
  fprintf(stderr,">>>>> Warning:message_read_int_list: Shouldn't be here.\n");
  fflush(stderr);
}

void message_read_translation_table(message_t m,XtTranslations *t,
				    int tag,int data)
{
  *t = (XtTranslations)message_get_dblword(m);
}

void message_read_accelerator_table(message_t m,XtAccelerators *a,
				    int tag,int data)
{
  *a = (XtAccelerators)message_get_dblword(m);
}

void message_read_font_list(message_t m,XmFontList *flist,int tag,int data)
{
  *flist = (XmFontList)message_get_dblword(m);
}

void message_read_string_table(message_t m,StringTable *items,int tag,int len)
{
  int i;

  items->length = len;
  items->data = (char **)XtMalloc( len*sizeof(char *) );
  register_garbage(items->data,GarbageData);
  for(i=0;i<len;i++)
    toolkit_read_value(m,&(items->data[i]),XtRString);
}

void message_read_xm_string_table(message_t m,StringTable *items,
				  int tag,int len)
{
  int i;

  items->length = len;
  items->data = (char **)XtMalloc( len*sizeof(char *) );
  register_garbage(items->data,GarbageData);
  for(i=0;i<len;i++)
    toolkit_read_value(m,&(items->data[i]),XmRXmString);
}

void message_read_color(message_t m,XColor *color,int tag, int red)
{
  color->red = red;
  color->green = message_get_word(m);
  color->blue = message_get_word(m);
}

void message_read_float(message_t m,float *f,int tag,int data)
{
  fprintf(stderr,">>>>> Warning:message_read_float: Not implemented.\n");
  fflush(stderr);
}



void toolkit_write_value(message_t message, caddr_t value, String type)
{
  long type_tag;
  type_writer write_value;

  type_tag = find_type_entry(type);
  if( type_tag < 0 ) {
    fprintf(stderr,"Choked on type %s\n",type);
    fflush(stderr);
    fatal_error("Illegal type specified.");
  }
    
  write_value = type_table[type_tag].writer;

  (*write_value)(message,value,type_tag);
}

void toolkit_read_value(message_t message,char *dest,String type)
{
  int tag,data,stuff;
  type_reader read_value;

  data = message_get_dblword(message);
  tag = data >> 24;
  data &= 0x00ffffff;

  read_value = type_table[tag].reader;
  if((tag == string_tag || tag == string_token_tag) &&
     !strcmp(type,XmRXmString))
    read_value = (type_reader)message_read_xm_string;
  else if( tag == string_token_tag && !strcmp(type,XtRString) )
    read_value = (type_reader)message_read_string;

  (*read_value)(message,dest,tag,data);
}
