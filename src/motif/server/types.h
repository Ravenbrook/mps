/*

 $Header: /project/cmucl/cvsroot/src/motif/server/types.h,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

/*
 * Response tags used in replying to requests.
 *
 */

#define CONFIRM_REPLY  0
#define VALUES_REPLY   1
#define CALLBACK_REPLY 2
#define EVENT_REPLY    3
#define ERROR_REPLY    4
#define WARNING_REPLY  5
#define PROTOCOL_REPLY 6
#define ACTION_REPLY   7

typedef struct {
  WidgetClass class;
  Widget parent;
  unsigned long length;
  ArgList args;
} ResourceList;

typedef struct {
  int length;
  WidgetList widgets;
} MyWidgetList;

typedef struct {
  int length;
  char **data; /* This may be either an array of Strings or XmStrings */
} StringTable;

typedef struct garbage_node {
  struct garbage_node *next;
  garbage_kind kind;
  char *junk;
} *Garbage;

typedef struct {
  int length;
  int *data;
} IntList;
