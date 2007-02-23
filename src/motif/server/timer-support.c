/* -*- Mode: C -*- */

/* $Header: /project/cmucl/cvsroot/src/motif/server/timer-support.c,v 1.3 2001/02/22 20:28:55 pw Exp $ */

/* timer_support.c --
 * Extension to CMUCL Motif Interface. Adding missing call for
 * 'XtAppAddTimeOut'.
 *
 * Copyright (C) Marco Antoniotti 1994
 *
 * Author: Marco Antoniotti
 * 
 * Address: Robotics Laboratory
 *          Courant Institute of Mathematical Sciences
 *          719 Broadway, Room 1220
 *          New York, NY, 10003
 */

/* Some of the include files are useless. */

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/SelectioB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/Form.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"

/*-----------------------------------------------------------------------------
 * Functions
 */


void
reply_with_xt_interval(message_t message, XtIntervalId timer) {
  reply_with_integer(message, (unsigned long) timer);
				/*  Pretty redundant! */
}


void
RawTimerHandler(XtPointer lisp_timer_id, XtIntervalId* timer) {
  int exit_value;
  extern int end_callback_loop;
  message_t reply = message_new(next_serial++);

  message_add_packet(reply);
  message_put_dblword(reply, TIMEOUT_REPLY); /* This is new. */
  message_write_int(reply, (unsigned long) *lisp_timer_id);
  message_write_int(reply, (unsigned long) *timer);
  message_send(client_socket, reply);
  message_free(reply);

  exit_value = end_callback_loop++;

  /* Code lifted from original CMU */
  /* *** This version handles no events until callbacks are done
     *** processing
   */
  while( exit_value<end_callback_loop )
    XtAppProcessEvent(app_context,XtIMAlternateInput);

  /* *** Use this version to allow callbacks to occur during callback
     *** processing 
     *** In general, this is not something the you want to do.  For instance,
     *** the callback_info mechanism for supporting modifications to the
     *** callback structure would no longer work if callbacks could be nested.
  while( exit_value<end_callback_loop ) {
    XtAppNextEvent(app_context, &event);
    XtDispatchEvent(&event);
   }
  */

}


/* RXtAppAddTimeOut -- */

int
RXtAppAddTimeOut(message_t message)
{
  XtIntervalId timer;

  Widget widget;
  int interval_read, lisp_timer_id;
  unsigned long interval;
  void reply_with_xt_interval(message_t, XtIntervalId);

  toolkit_read_value(message, &interval_read, XtRInt);
  interval = (unsigned long) interval_read;
  /* I have not seen any simple way to pass an 'unsigned long' across */
  /* the socket. So I use this trick to get at least an integer, which */
  /* I hope should be at least 2^24 (i.e. a CL fixnum). */
  /* It is true that on Sparcs, sizeof(int) == sizeof(long), but this */
  /* is a nasty assumption to make. Maybe HP-UX works differently. I */
  /* don't know. */

  toolkit_read_value(message, &lisp_timer_id, XtRInt);
  /* Contrary to CLM, I must keep everything on the Lisp side, since I */
  /* want to provide the "raw" XtAppAddTimeOut capability. */
  /* Hence I keep a registry of the timers on the lisp side and use */
  /* the C side Timer Proc to refer to the Lisp side by the proper ID. */

  timer = XtAppAddTimeOut(app_context,
			  interval,
			  (XtTimerCallbackProc) RawTimerHandler,
			  (XtPointer) &lisp_timer_id);

  /* Maybe I should also return the 'lisp_timer_id' value in order to */
  /* check on the lisp side. */
  reply_with_xt_interval(message, timer);
}

