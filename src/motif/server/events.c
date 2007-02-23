/*

 $Header: /project/cmucl/cvsroot/src/motif/server/events.c,v 1.3 1994/10/27 17:16:51 ram Exp $

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

extern int end_callback_loop;

void write_any_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,combine_type_and_data(event_tag,0));
  message_put_dblword(reply,(unsigned long)event);
  message_put_dblword(reply,event->xany.type);
  message_put_dblword(reply,event->xany.serial);
  message_put_dblword(reply,event->xany.send_event);
  message_put_dblword(reply,event->xany.window);
}

void write_key_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xkey.root);
  message_put_dblword(reply,event->xkey.subwindow);
  message_put_dblword(reply,event->xkey.time);
  message_put_dblword(reply,event->xkey.x);
  message_put_dblword(reply,event->xkey.y);
  message_put_dblword(reply,event->xkey.x_root);
  message_put_dblword(reply,event->xkey.y_root);
  message_put_dblword(reply,event->xkey.state);
  message_put_dblword(reply,event->xkey.keycode);
  message_put_dblword(reply,event->xkey.same_screen);
}

void write_button_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xbutton.root);
  message_put_dblword(reply,event->xbutton.subwindow);
  message_put_dblword(reply,event->xbutton.time);
  message_put_dblword(reply,event->xbutton.x);
  message_put_dblword(reply,event->xbutton.y);
  message_put_dblword(reply,event->xbutton.x_root);
  message_put_dblword(reply,event->xbutton.y_root);
  message_put_dblword(reply,event->xbutton.state);
  message_put_dblword(reply,event->xbutton.button);
  message_put_dblword(reply,event->xbutton.same_screen);
}

void write_motion_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xmotion.root);
  message_put_dblword(reply,event->xmotion.subwindow);
  message_put_dblword(reply,event->xmotion.time);
  message_put_dblword(reply,event->xmotion.x);
  message_put_dblword(reply,event->xmotion.y);
  message_put_dblword(reply,event->xmotion.x_root);
  message_put_dblword(reply,event->xmotion.y_root);
  message_put_dblword(reply,event->xmotion.state);
  message_put_byte(reply,event->xmotion.is_hint);
  message_put_dblword(reply,event->xmotion.same_screen);
}

void write_crossing_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xcrossing.root);
  message_put_dblword(reply,event->xcrossing.subwindow);
  message_put_dblword(reply,event->xcrossing.time);
  message_put_dblword(reply,event->xcrossing.x);
  message_put_dblword(reply,event->xcrossing.y);
  message_put_dblword(reply,event->xcrossing.x_root);
  message_put_dblword(reply,event->xcrossing.y_root);
  message_put_dblword(reply,event->xcrossing.mode);
  message_put_dblword(reply,event->xcrossing.detail);
  message_put_dblword(reply,event->xcrossing.same_screen);
  message_put_dblword(reply,event->xcrossing.focus);
  message_put_dblword(reply,event->xcrossing.state);
}

void write_focus_change_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xfocus.mode);
  message_put_dblword(reply,event->xfocus.detail);
}

void write_keymap_event(message_t reply,XEvent *event)
{
  int i;

  for(i=0;i<32;i++)
    message_put_byte(reply,event->xkeymap.key_vector[i]);
}

void write_expose_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xexpose.x);
  message_put_dblword(reply,event->xexpose.y);
  message_put_dblword(reply,event->xexpose.width);
  message_put_dblword(reply,event->xexpose.height);
  message_put_dblword(reply,event->xexpose.count);
}

void write_graphics_expose_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xgraphicsexpose.x);
  message_put_dblword(reply,event->xgraphicsexpose.y);
  message_put_dblword(reply,event->xgraphicsexpose.width);
  message_put_dblword(reply,event->xgraphicsexpose.height);
  message_put_dblword(reply,event->xgraphicsexpose.count);
  message_put_dblword(reply,event->xgraphicsexpose.major_code);
  message_put_dblword(reply,event->xgraphicsexpose.minor_code);
}

void write_no_expose_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xnoexpose.major_code);
  message_put_dblword(reply,event->xnoexpose.minor_code);
}

void write_visibility_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xvisibility.state);
}

void write_create_window_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xcreatewindow.window);
  message_put_dblword(reply,event->xcreatewindow.x);
  message_put_dblword(reply,event->xcreatewindow.y);
  message_put_dblword(reply,event->xcreatewindow.width);
  message_put_dblword(reply,event->xcreatewindow.height);
  message_put_dblword(reply,event->xcreatewindow.override_redirect);
}

void write_destroy_window_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xdestroywindow.window);
}

void write_unmap_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xunmap.window);
  message_put_dblword(reply,event->xunmap.from_configure);
}

void write_map_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xmap.window);
  message_put_dblword(reply,event->xmap.override_redirect);
}

void write_map_request_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xmaprequest.window);
}

void write_reparent_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xreparent.parent);
  message_put_dblword(reply,event->xreparent.x);
  message_put_dblword(reply,event->xreparent.y);
  message_put_dblword(reply,event->xreparent.override_redirect);
}

void write_configure_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xconfigure.window);
  message_put_dblword(reply,event->xconfigure.x);
  message_put_dblword(reply,event->xconfigure.y);
  message_put_dblword(reply,event->xconfigure.width);
  message_put_dblword(reply,event->xconfigure.height);
  message_put_dblword(reply,event->xconfigure.border_width);
  message_put_dblword(reply,event->xconfigure.above);
  message_put_dblword(reply,event->xconfigure.override_redirect);
}

void write_gravity_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xgravity.window);
  message_put_dblword(reply,event->xgravity.x);
  message_put_dblword(reply,event->xgravity.y);
}

void write_resize_request_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xresizerequest.width);
  message_put_dblword(reply,event->xresizerequest.height);
}

void write_configure_request_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xconfigurerequest.window);
  message_put_dblword(reply,event->xconfigurerequest.x);
  message_put_dblword(reply,event->xconfigurerequest.y);
  message_put_dblword(reply,event->xconfigurerequest.width);
  message_put_dblword(reply,event->xconfigurerequest.height);
  message_put_dblword(reply,event->xconfigurerequest.border_width);
  message_put_dblword(reply,event->xconfigurerequest.above);
  message_put_dblword(reply,event->xconfigurerequest.detail);
  message_put_dblword(reply,event->xconfigurerequest.value_mask);
}

void write_circulate_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xcirculate.window);
  message_put_dblword(reply,event->xcirculate.place);
}

void write_circulate_request_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xcirculaterequest.window);
  message_put_dblword(reply,event->xcirculaterequest.place);
}

void write_property_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xproperty.atom);
  message_put_dblword(reply,event->xproperty.time);
  message_put_dblword(reply,event->xproperty.state);
}

void write_selection_clear_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xselectionclear.selection);
  message_put_dblword(reply,event->xselectionclear.time);
}

void write_selection_request_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xselectionrequest.requestor);
  message_put_dblword(reply,event->xselectionrequest.selection);
  message_put_dblword(reply,event->xselectionrequest.target);
  message_put_dblword(reply,event->xselectionrequest.property);
  message_put_dblword(reply,event->xselectionrequest.time);
}

void write_selection_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xselection.selection);
  message_put_dblword(reply,event->xselection.target);
  message_put_dblword(reply,event->xselection.property);
  message_put_dblword(reply,event->xselection.time);
}

void write_colormap_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xcolormap.colormap);
  message_put_dblword(reply,event->xcolormap.new);
  message_put_dblword(reply,event->xcolormap.state);
}

/* ***** Client message transport goes here */

void write_mapping_event(message_t reply,XEvent *event)
{
  message_put_dblword(reply,event->xmapping.request);
  message_put_dblword(reply,event->xmapping.first_keycode);
  message_put_dblword(reply,event->xmapping.count);
}



/* Function that writes XEvent structures into messages for transport
   to the client Lisp */

void write_event(message_t reply,XEvent *event)
{
  write_any_event(reply,event);

  switch( event->type ) {
  case ButtonPress:
  case ButtonRelease:
    write_button_event(reply,event);
    break;
  case Expose:
    write_expose_event(reply,event);
    break;
  case KeyPress:
  case KeyRelease:
    write_key_event(reply,event);
    break;
  case MotionNotify:
    write_motion_event(reply,event);
    break;
  case ColormapNotify:
    write_colormap_event(reply,event);
    break;
  case EnterNotify:
  case LeaveNotify:
    write_crossing_event(reply,event);
    break;
  case GraphicsExpose:
    write_graphics_expose_event(reply,event);
    break;
  case NoExpose:
    write_no_expose_event(reply,event);
    break;
  case FocusIn:
  case FocusOut:
    write_focus_change_event(reply,event);
    break;
  case KeymapNotify:
    write_keymap_event(reply,event);
    break;
  case PropertyNotify:
    write_property_event(reply,event);
    break;
  case ResizeRequest:
    write_resize_request_event(reply,event);
    break;
  case CirculateNotify:
    write_circulate_event(reply,event);
    break;
  case ConfigureNotify:
    write_configure_event(reply,event);
    break;
  case DestroyNotify:
    write_destroy_window_event(reply,event);
    break;
  case GravityNotify:
    write_gravity_event(reply,event);
    break;
  case MapNotify:
    write_map_event(reply,event);
    break;
  case ReparentNotify:
    write_reparent_event(reply,event);
    break;
  case UnmapNotify:
    write_unmap_event(reply,event);
    break;
  case CreateNotify:
    write_create_window_event(reply,event);
    break;
  case CirculateRequest:
    write_circulate_request_event(reply,event);
    break;
  case ConfigureRequest:
    write_configure_request_event(reply,event);
    break;
  case MapRequest:
    write_map_request_event(reply,event);
    break;
/*case ClientMessage: */
  case MappingNotify:
    write_mapping_event(reply,event);
    break;
  case SelectionClear:
    write_selection_clear_event(reply,event);
    break;
  case SelectionNotify:
    write_selection_event(reply,event);
    break;
  case SelectionRequest:
    write_selection_request_event(reply,event);
    break;
  case VisibilityNotify:
    write_visibility_event(reply,event);
    break;
  default:
    printf("Unknown event type %d\n",event->type);
    fflush(stdout);
    break;
  }
}



int RTransportEvent(message_t message)
{
  XEvent *event;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&event,XtRInt);

  write_event(reply,event);

  message_send(client_socket,reply);
  must_confirm=False;
  message_free(reply);
}



int RXtAddEventHandler(message_t message)
{
  Widget widget;
  int mask,non_maskable;
  XtEventHandler f;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&mask,XtRInt);
  toolkit_read_value(message,&non_maskable,XtRInt);

  if( non_maskable )
    f = NonMaskableHandler;
  else
    f = EventHandler;

  XtAddEventHandler(widget,mask,non_maskable,f,mask);
}

int RXtRemoveEventHandler(message_t message)
{
  Widget widget;
  int mask,non_maskable;
  XtEventHandler f;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&mask,XtRInt);
  toolkit_read_value(message,&non_maskable,XtRInt);

  if( non_maskable )
    f = NonMaskableHandler;
  else
    f = EventHandler;

  XtRemoveEventHandler(widget,mask,non_maskable,f,mask);
}

void CoreEventHandler(Widget widget,int mask,int nonmaskable,XEvent *event)
{
  int exit_value;
  message_t reply = message_new(next_serial++);

  message_add_packet(reply);
  message_put_dblword(reply,EVENT_REPLY);
  message_write_widget(reply,widget,widget_tag);
  message_write_int(reply,mask,int_tag);
  message_write_boolean(reply,nonmaskable,boolean_tag);
  write_event(reply,event);

  message_send(client_socket,reply);
  message_free(reply);

  exit_value = end_callback_loop++;
  while( exit_value<end_callback_loop )
    XtAppProcessEvent(app_context,XtIMAlternateInput);
}

void EventHandler(Widget widget,int mask,XEvent *event)
{
  CoreEventHandler(widget,mask,False,event);
}

void NonMaskableHandler(Widget widget,int mask,XEvent *event)
{
  CoreEventHandler(widget,mask,True,event);
}
