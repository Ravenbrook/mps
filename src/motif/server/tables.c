/*

 $Header: /project/cmucl/cvsroot/src/motif/server/tables.c,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/Core.h>
#include <X11/CoreP.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#include "global.h"
#include "datatrans.h"
#include "tables.h"

extern WidgetClass overrideShellWidgetClass,transientShellWidgetClass,
  topLevelShellWidgetClass,applicationShellWidgetClass;
extern WidgetClass xmDialogShellWidgetClass,xmMenuShellWidgetClass;
extern WidgetClass xmLabelWidgetClass,xmLabelGadgetClass,
  xmArrowButtonWidgetClass,xmArrowButtonGadgetClass,xmPushButtonWidgetClass,
  xmPushButtonGadgetClass,xmToggleButtonWidgetClass,xmToggleButtonGadgetClass,
  xmCascadeButtonWidgetClass,xmCascadeButtonGadgetClass,xmSeparatorWidgetClass,
  xmSeparatorGadgetClass, xmDrawnButtonWidgetClass, xmMenuShellWidgetClass,
  xmDrawingAreaWidgetClass,xmDialogShellWidgetClass,xmBulletinBoardWidgetClass,
  xmCommandWidgetClass, xmFileSelectionBoxWidgetClass, xmFormWidgetClass,
  xmMessageBoxWidgetClass, xmSelectionBoxWidgetClass, xmScrollBarWidgetClass,
  xmTextWidgetClass, xmTextFieldWidgetClass, xmRowColumnWidgetClass,
  xmScaleWidgetClass, xmFrameWidgetClass, xmListWidgetClass,
  xmMainWindowWidgetClass,xmScrolledWindowWidgetClass,xmPanedWindowWidgetClass;

#include "StringTable.h"
#include "ClassTable.h"
#include "TypeTable.h"

typedef struct {
  String name,type;
} resource_entry, *resource_entry_list;

typedef struct {
  resource_entry_list resource_list,constraint_list;
  int resource_count,constraint_count;
} class_resources;

class_resources resource_table[CLASS_TABLE_SIZE];
int string_token_tag,string_tag,xm_string_tag,enum_tag;
int int_tag,window_tag,boolean_tag,widget_tag,function_tag;
int callback_reason_tag,event_tag,resource_list_tag;
int translation_table_tag,accelerator_table_tag;
int atom_tag,font_list_tag,string_table_tag,xm_string_table_tag;
int int_list_tag,cursor_tag;

long binary_search(char *table,int length,int entry_size,
			    char *target, int (*pred)())
{
  long start=0,end = length-1;
  long current = length/2;
  int result;

  while( start<=end ) {
    result = (*pred)(target,table+current*entry_size);
    if( !result )
      return current;
    else if( result<0 )
      end = current-1;
    else
      start = current+1;

    current = (start+end)/2;
  }

  /* Target not found */
  return (-1);
}

int resource_pred(String target,resource_entry *current) {
  return strcmp(target,current->name);
}

int type_pred(String target,type_entry *current) {
  return strcmp(target,current->type);
}

int string_pred(String target, String *current) {
  return strcmp(target,*current);
}

int resource_cmp(resource_entry *a,resource_entry *b) {
  return strcmp(a->name,b->name);
}

void assure_class_initialized(WidgetClass class)
{
  if( !class->core_class.class_inited )
    XtInitializeWidgetClass(class);
}

void record_class_resources(WidgetClass class,class_resources *r)
{
  XtResourceList resource_list,constraint_list;
  int resource_count,constraint_count,index,extra=0;

  XtGetResourceList(class,&resource_list,&resource_count);
  XtGetConstraintResourceList(class,&constraint_list,&constraint_count);

  if( class == xmTextWidgetClass )
    extra = 14;
  else if(class == applicationShellWidgetClass ||
	  class == topLevelShellWidgetClass    ||
	  class == transientShellWidgetClass   ||
	  class == xmDialogShellWidgetClass)
    extra = 4;

  resource_count += extra;
  r->resource_list =
    (resource_entry_list)XtMalloc( sizeof(resource_entry)*resource_count );
  r->constraint_list =
    (resource_entry_list)XtMalloc( sizeof(resource_entry)*constraint_count );

  for(index=0;index<resource_count-extra;index++) {
    r->resource_list[index].name =
      XtNewString(resource_list[index].resource_name);
    r->resource_list[index].type =
      XtNewString(resource_list[index].resource_type);
  }

  if( class == xmTextWidgetClass ) {
    r->resource_list[index].name   = XmNpendingDelete;
    r->resource_list[index++].type = XmRBoolean;
    r->resource_list[index].name   = XmNselectThreshold;
    r->resource_list[index++].type = XmRInt;
    r->resource_list[index].name   = XmNblinkRate;
    r->resource_list[index++].type = XmRInt;
    r->resource_list[index].name   = XmNcolumns;
    r->resource_list[index++].type = XmRShort;
    r->resource_list[index].name   = XmNcursorPositionVisible;
    r->resource_list[index++].type = XmRBoolean;
    r->resource_list[index].name   = XmNfontList;
    r->resource_list[index++].type = XmRFontList;
    r->resource_list[index].name   = XmNresizeHeight;
    r->resource_list[index++].type = XmRBoolean;
    r->resource_list[index].name   = XmNresizeWidth;
    r->resource_list[index++].type = XmRBoolean;
    r->resource_list[index].name   = XmNrows;
    r->resource_list[index++].type = XmRShort;
    r->resource_list[index].name   = XmNwordWrap;
    r->resource_list[index++].type = XmRBoolean;
    r->resource_list[index].name   = XmNscrollHorizontal;
    r->resource_list[index++].type = XmRBoolean;
    r->resource_list[index].name   = XmNscrollVertical;
    r->resource_list[index++].type = XmRBoolean;
    r->resource_list[index].name   = XmNscrollLeftSide;
    r->resource_list[index++].type = XmRBoolean;
    r->resource_list[index].name   = XmNscrollTopSide;
    r->resource_list[index++].type = XmRBoolean;
  } else if(class == applicationShellWidgetClass ||
	    class == topLevelShellWidgetClass    ||
	    class == transientShellWidgetClass   ||
	    class == xmDialogShellWidgetClass) {
    r->resource_list[index].name   = XmNdefaultFontList;
    r->resource_list[index++].type = XmRFontList;
    r->resource_list[index].name   = XmNdeleteResponse;
    r->resource_list[index++].type = XmRDeleteResponse;
    r->resource_list[index].name   = XmNkeyboardFocusPolicy;
    r->resource_list[index++].type = XmRKeyboardFocusPolicy;
    r->resource_list[index].name   = XmNshellUnitType;
    r->resource_list[index++].type = XmRShellUnitType;
  }

  for(index=0;index<constraint_count;index++) {
    r->constraint_list[index].name = 
      XtNewString(constraint_list[index].resource_name);
    r->constraint_list[index].type = 
      XtNewString(constraint_list[index].resource_type);
  }

  qsort(r->resource_list,resource_count,sizeof(resource_entry),resource_cmp);
  qsort(r->constraint_list,constraint_count,
	sizeof(resource_entry),resource_cmp);

  r->resource_count = resource_count;
  r->constraint_count = constraint_count;

  XtFree( (char *)resource_list );
  XtFree( (char *)constraint_list );
}


String query_resource_type(int classid,Widget parent,String resource)
{
  WidgetClass class;
  resource_entry_list resources,constraints;
  int result;

  if( !resource_table[classid].resource_list ) {
    class = *class_table[classid];
    assure_class_initialized(class);
    record_class_resources(class,&resource_table[classid]);
  }

  resources = resource_table[classid].resource_list;
  constraints = resource_table[classid].constraint_list;


  result = binary_search((char *)resources,
			 resource_table[classid].resource_count,
			 sizeof(resource_entry),resource,resource_pred);

  if( result<0 )
    result = binary_search((char *)constraints,
			   resource_table[classid].constraint_count,
			   sizeof(resource_entry),resource,resource_pred);
  else return resources[result].type;

  if( result<0 ) {
    if( parent ) {
      class = XtClass(parent);
      classid = find_widget_class_id(class);
      if( !resource_table[classid].resource_list ) {
	assure_class_initialized(class);
	record_class_resources(class,&resource_table[classid]);
      }
      constraints = resource_table[classid].constraint_list;
      result = binary_search((char *)constraints,
			     resource_table[classid].constraint_count,
			     sizeof(resource_entry),resource,resource_pred);
    }
  }
  else return constraints[result].type;

  if( result<0 ) return NULL;
  else return constraints[result].type;
}

long tokenize_string(String string)
{
  return binary_search((char *)string_table,STRING_TABLE_SIZE,sizeof(char *),
		       string,string_pred);
}

long find_type_entry(String type)
{
   return binary_search((char *)type_table,TYPE_TABLE_SIZE,sizeof(type_entry),
		       type,type_pred);
}

long find_widget_class_id(WidgetClass class)
{
  int id=0;

  while( id<CLASS_TABLE_SIZE )
    if( *class_table[id] == class ) return id;
    else id++;

  return (-1);
}
