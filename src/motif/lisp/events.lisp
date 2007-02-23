;;;; -*- Mode: Lisp ; Package: Toolkit-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/events.lisp,v 1.2 1994/10/31 04:54:48 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; Alien definitions for all the various XEvent structures.
;;;

(in-package "TOOLKIT-INTERNALS")



;;;; Definitions of the various XEvent structures
;;; We never include the (Display *) because it would be useless in Lisp and
;;; we add the HANDLE field so that we can pass this event back to C.

(def-alien-type xid (unsigned 32))
(def-alien-type window xid)
(def-alien-type colormap xid)
(def-alien-type drawable xid)
(def-alien-type atom (unsigned 32))
(def-alien-type bool (boolean 32))
(def-alien-type time (unsigned 32))

(def-alien-type x-key-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (root window)
    (subwindow window)
    (time time)
    (x int)
    (y int)
    (x-root int)
    (y-root int)
    (state unsigned-int)
    (keycode unsigned-int)
    (same-screen bool)))

(def-alien-type x-button-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (root window)
    (subwindow window)
    (time time)
    (x int)
    (y int)
    (x-root int)
    (y-root int)
    (state unsigned-int)
    (button unsigned-int)
    (same-screen bool)))

(def-alien-type x-motion-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (root window)
    (subwindow window)
    (time time)
    (x int)
    (y int)
    (x-root int)
    (y-root int)
    (state unsigned-int)
    (is-hint char)
    (same-screen bool)))

(def-alien-type x-crossing-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (root window)
    (subwindow window)
    (time time)
    (x int)
    (y int)
    (x-root int)
    (y-root int)
    (mode int)
    (detail int)
    (same-screen bool)
    (focus bool)
    (state unsigned-int)))

(def-alien-type x-focus-change-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (mode int)
    (detail int)))

(def-alien-type x-keymap-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (key-vector (array char 32))))

(def-alien-type x-expose-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (x int)
    (y int)
    (width int)
    (height int)
    (count int)))

(def-alien-type x-graphics-expose-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (drawable drawable)
    (x int)
    (y int)
    (width int)
    (height int)
    (count int)
    (major-code int)
    (minor-code int)))

(def-alien-type x-no-expose-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (drawable drawable)
    (major-code int)
    (minor-code int)))

(def-alien-type x-visibility-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (state int)))

(def-alien-type x-create-window-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (parent window)
    (window window)
    (x int)
    (y int)
    (width int)
    (height int)
    (override-redirect bool)))

(def-alien-type x-destroy-window-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (event window)
    (window window)))

(def-alien-type x-unmap-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (event window)
    (window window)
    (from-configure bool)))

(def-alien-type x-map-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (event window)
    (window window)
    (override-redirect bool)))

(def-alien-type x-map-request-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (parent window)
    (window window)))

(def-alien-type x-reparent-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (event window)
    (parent window)
    (x int)
    (y int)
    (override-redirect bool)))

(def-alien-type x-configure-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (event window)
    (window window)
    (x int)
    (y int)
    (width int)
    (height int)
    (border-width int)
    (above window)
    (override-redirect bool)))

(def-alien-type x-gravity-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (event window)
    (window window)
    (x int)
    (y int)))

(def-alien-type x-resize-request-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (width int)
    (height int)))

(def-alien-type x-configure-request-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (parent window)
    (window window)
    (x int)
    (y int)
    (width int)
    (height int)
    (border-width int)
    (above window)
    (detail int)
    (value-mask unsigned-long)))

(def-alien-type x-circulate-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (event window)
    (window window)
    (place int)))

(def-alien-type x-circulate-request-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (parent window)
    (window window)
    (place int)))

(def-alien-type x-property-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (atom atom)
    (time time)
    (state int)))

(def-alien-type x-selection-clear-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (selection atom)
    (time time)))

(def-alien-type x-selection-request-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (owner window)
    (requestor window)
    (selection atom)
    (target atom)
    (property atom)
    (time time)))

(def-alien-type x-selection-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (requestor window)
    (selection atom)
    (target atom)
    (property atom)
    (time time)))

(def-alien-type x-colormap-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (colormap colormap)
    (new bool)
    (state int)))

(def-alien-type x-client-message-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (message-type atom)
    (format int)
    (data (union nil
		 (b (array char 20))
		 (s (array short 10))
		 (l (array long 5))))))

(def-alien-type x-mapping-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)
    (request int)
    (first-keycode int)
    (count int)))

(def-alien-type x-any-event
  (struct nil
    (handle unsigned-long)
    (type int)
    (serial unsigned-long)
    (send-event bool)
    (window window)))

(def-alien-type xevent
  (union nil
    (xany x-any-event)
    (xkey x-key-event)
    (xbutton x-button-event)
    (xmotion x-motion-event)
    (xcrossing x-crossing-event)
    (xfocus x-focus-change-event)
    (xexpose x-expose-event)
    (xgraphicsexpose x-graphics-expose-event)
    (xnoexpose x-no-expose-event)
    (xvisibility x-visibility-event)
    (xcreatewindow x-create-window-event)
    (xdestroywindow x-destroy-window-event)
    (xunmap x-unmap-event)
    (xmap x-map-event)
    (xmaprequest x-map-request-event)
    (xreparent x-reparent-event)
    (xconfigure x-configure-event)
    (xgravity x-gravity-event)
    (xresizerequest x-resize-request-event)
    (xconfigurerequest x-configure-request-event)
    (xcirculate x-circulate-event)
    (xcirculaterequest x-circulate-request-event)
    (xproperty x-property-event)
    (xselectionclear x-selection-clear-event)
    (xselectionrequest x-selection-request-event)
    (xselection x-selection-event)
    (xcolormap x-colormap-event)
    (xclient x-client-message-event)
    (xmapping x-mapping-event)
    (xkeymap x-keymap-event)))

(deftype toolkit-event () '(alien xevent))



;;;; Functions for accessing the common fields of all XEvents

(eval-when (compile eval)
  (defmacro def-event-access (name union-slot slot)
    `(defun ,name (event)
       (declare (type toolkit-event event))
       (alien:slot (alien:slot event ,union-slot) ,slot)))
  
  (defmacro def-event-window-access (name union-slot slot)
    `(defun ,name (event)
       (declare (type toolkit-event event))
       (xlib::lookup-window
	*x-display*
	(alien:slot (alien:slot event ,union-slot) ,slot))))
  
  (defmacro def-event-drawable-access (name union-slot slot)
    `(defun ,name (event)
       (declare (type toolkit-event event))
       (xlib::lookup-drawable
	*x-display*
	(alien:slot (alien:slot event ,union-slot) ,slot))))
  
  (defmacro def-event-colormap-access (name union-slot slot)
    `(defun ,name (event)
       (declare (type toolkit-event event))
       (xlib::lookup-colormap
	*x-display*
	(alien:slot (alien:slot event ,union-slot) ,slot))))
  
  (defmacro def-event-atom-access (name union-slot slot)
    `(defun ,name (event)
       (declare (type toolkit-event event))
       (xlib::atom-name *x-display*
			(alien:slot (alien:slot event ,union-slot) ,slot))))
) ;; EVAL-WHEN

(declaim (inline event-handle event-serial event-send-event event-window
		 event-type))

(def-event-access event-handle 'xany 'handle)
(def-event-access event-serial 'xany 'serial)
(def-event-access event-send-event 'xany 'send-event)
(def-event-window-access event-window 'xany 'window)
(defun event-type (event)
  (declare (type toolkit-event event))
  (svref xlib::*event-key-vector*
	 (alien:slot (alien:slot event 'xany) 'type)))


;;;; Functions for accessing the fields of XButton events

(declaim (inline button-event-root button-event-subwindow button-event-time
		 button-event-x button-event-y button-event-x-root
		 button-event-y-root button-event-state
		 button-event-same-screen button-event-button))

(def-event-window-access button-event-root 'xbutton 'root)
(def-event-window-access button-event-subwindow 'xbutton 'subwindow)
(def-event-access button-event-time 'xbutton 'time)
(def-event-access button-event-x 'xbutton 'x)
(def-event-access button-event-y 'xbutton 'y)
(def-event-access button-event-x-root 'xbutton 'x-root)
(def-event-access button-event-y-root 'xbutton 'y-root)
(def-event-access button-event-state 'xbutton 'state)
(def-event-access button-event-button 'xbutton 'button)
(def-event-access button-event-same-screen 'xbutton 'same-screen)



;;;; Functions for accessing the fields of XKey events

(declaim (inline key-event-root key-event-subwindow key-event-time
		 key-event-x key-event-y key-event-x-root key-event-y-root
		 key-event-state key-event-keycode key-event-same-screen))

(def-event-window-access key-event-root 'xkey 'root)
(def-event-window-access key-event-subwindow 'xkey 'subwindow)
(def-event-access key-event-time 'xkey 'time)
(def-event-access key-event-x 'xkey 'x)
(def-event-access key-event-y 'xkey 'y)
(def-event-access key-event-x-root 'xkey 'x-root)
(def-event-access key-event-y-root 'xkey 'y-root)
(def-event-access key-event-state 'xkey 'state)
(def-event-access key-event-keycode 'xkey 'keycode)
(def-event-access key-event-same-screen 'xkey 'same-screen)



;;;; Functions for accessing XMotion event slots

(declaim (inline motion-event-root motion-event-subwindow motion-event-time
		 motion-event-x motion-event-y motion-event-x-root
		 motion-event-y-root motion-event-state motion-event-is-hint
		 motion-event-same-screen))

(def-event-window-access motion-event-root 'xmotion 'root)
(def-event-window-access motion-event-subwindow 'xmotion 'subwindow)
(def-event-access motion-event-time 'xmotion 'time)
(def-event-access motion-event-x 'xmotion 'x)
(def-event-access motion-event-y 'xmotion 'y)
(def-event-access motion-event-x-root 'xmotion 'x-root)
(def-event-access motion-event-y-root 'xmotion 'y-root)
(def-event-access motion-event-state 'xmotion 'state)
(def-event-access motion-event-is-hint 'xmotion 'is-hint)
(def-event-access motion-event-same-screen 'xmotion 'same-screen)



;;;; Functions for accessing XCrossingEvent slots

(declaim (inline crossing-event-root crossing-event-subwindow
		 crossing-event-time crossing-event-x crossing-event-y
		 crossing-event-x-root crossing-event-y-root
		 crossing-event-mode crossing-event-detail
		 crossing-event-same-screen crossing-event-focus
		 crossing-event-state))

(def-event-window-access crossing-event-root 'xcrossing 'root)
(def-event-window-access crossing-event-subwindow 'xcrossing 'subwindow)
(def-event-access crossing-event-time 'xcrossing 'time)
(def-event-access crossing-event-x 'xcrossing 'x)
(def-event-access crossing-event-y 'xcrossing 'y)
(def-event-access crossing-event-x-root 'xcrossing 'x-root)
(def-event-access crossing-event-y-root 'xcrossing 'y-root)
(def-event-access crossing-event-mode 'xcrossing 'mode)
(def-event-access crossing-event-detail 'xcrossing 'detail)
(def-event-access crossing-event-same-screen 'xcrossing 'same-screen)
(def-event-access crossing-event-focus 'xcrossing 'focus)
(def-event-access crossing-event-state 'xcrossing 'state)



;;;; Functions for accessing XFocusChangeEvent slots

(declaim (inline focus-change-event-mode focus-change-event-detail))

(def-event-access focus-change-event-mode 'xfocus 'mode)
(def-event-access focus-change-event-detail 'xfocus 'detail)



;;;; Functions for accessing XKeymapEvent slots

;; **** These need to be added



;;;; Functions for accessing XExposeEvent slots

(declaim (inline expose-event-x expose-event-y expose-event-width
		 expose-event-height expose-event-count))

(def-event-access expose-event-x 'xexpose 'x)
(def-event-access expose-event-y 'xexpose 'y)
(def-event-access expose-event-width 'xexpose 'width)
(def-event-access expose-event-height 'xexpose 'height)
(def-event-access expose-event-count 'xexpose 'count)



;;;; Functions for accessing XGraphicsExposeEvent structures

(declaim (inline graphics-expose-event-drawable graphics-expose-event-x
		 graphics-expose-event-y graphics-expose-event-width
		 graphics-expose-event-height graphics-expose-event-count
		 graphics-expose-event-major-code
		 graphics-expose-event-minor-code))

(def-event-drawable-access graphics-expose-event-drawable
			   'xgraphicsexpose 'drawable)
(def-event-access graphics-expose-event-x 'xgraphicsexpose 'x)
(def-event-access graphics-expose-event-y 'xgraphicsexpose 'y)
(def-event-access graphics-expose-event-width 'xgraphicsexpose 'width)
(def-event-access graphics-expose-event-height 'xgraphicsexpose 'height)
(def-event-access graphics-expose-event-count 'xgraphicsexpose 'count)
(def-event-access graphics-expose-event-major-code
		  'xgraphicsexpose 'major-code)
(def-event-access graphics-expose-event-minor-code
		  'xgraphicsexpose 'minor-code)



;;;; Functions for accessing XNoExposeEvent slots

(declaim (inline no-expose-event-drawable no-expose-event-major-code
		 no-expose-event-minor-code))

(def-event-drawable-access no-expose-event-drawable 'xnoexpose 'drawable)
(def-event-access no-expose-event-major-code 'xnoexpose 'major-code)
(def-event-access no-expose-event-minor-code 'xnoexpose 'minor-code)



;;;; Functions for accesssing XVisibilityEvent slots

(declaim (inline visibility-event-state))

(def-event-access visibility-event-state 'xvisibility 'state)



;;;; Function for accessing XCreateWindowEvent data

(declaim (inline create-window-event-parent create-window-event-window
		 create-window-event-x create-window-event-y
		 create-window-event-width create-window-event-height
		 create-window-event-override-redirect))

(def-event-window-access create-window-event-parent 'xcreatewindow 'parent)
(def-event-window-access create-window-event-window 'xcreatewindow 'window)
(def-event-access create-window-event-x 'xcreatewindow 'x)
(def-event-access create-window-event-y 'xcreatewindow 'y)
(def-event-access create-window-event-width 'xcreatewindow 'width)
(def-event-access create-window-event-height 'xcreatewindow 'height)
(def-event-access create-window-event-override-redirect
		  'xcreatewindow 'override-redirect)



;;;; Functions for accessing XDestroyWindowEvent slots

(declaim (inline destroy-window-event-event destroy-window-event-window))

(def-event-window-access destroy-window-event-event 'xdestroywindow 'event)
(def-event-window-access destroy-window-event-window 'xdestroywindow 'window)



;;;; Functions for accessing XUnmapEvent structures

(declaim (inline unmap-event-event unmap-event-window
		 unmap-event-from-configure))

(def-event-window-access unmap-event-event 'xunmap 'event)
(def-event-window-access unmap-event-window 'xunmap 'window)
(def-event-access unmap-event-from-configure 'xunmap 'from-configure)



;;;; Functions for accessing XMapRequestEvent slots

(declaim (inline map-request-event-parent map-request-event-window))

(def-event-window-access map-request-event-parent 'xmaprequest 'parent)
(def-event-window-access map-request-event-window 'xmaprequest 'window)



;;;; Functions to access XReparentEvent structures

(declaim (inline reparent-event-event reparent-event-parent
		 reparent-event-x reparent-event-y
		 reparent-event-override-redirect))

(def-event-window-access reparent-event-event 'xreparent 'event)
(def-event-window-access reparent-event-parent 'xreparent 'parent)
(def-event-access reparent-event-x 'xreparent 'x)
(def-event-access reparent-event-y 'xreparent 'y)
(def-event-access reparent-event-override-redirect
		  'xreparent 'override-redirect)



;;;; Functions to access XConfigureEvent slots

(declaim (inline configure-event-event configure-event-window
		 configure-event-x configure-event-y configure-event-width
		 configure-event-height configure-event-border-width
		 configure-event-above configure-event-override-redirect))

(def-event-window-access configure-event-event 'xconfigure 'event)
(def-event-window-access configure-event-window 'xconfigure 'window)
(def-event-access configure-event-x 'xconfigure 'x)
(def-event-access configure-event-y 'xconfigure 'y)
(def-event-access configure-event-width 'xconfigure 'width)
(def-event-access configure-event-height 'xconfigure 'height)
(def-event-access configure-event-border-width 'xconfigure 'border-width)
(def-event-window-access configure-event-above 'xconfigure 'above)
(def-event-access configure-event-override-redirect
		  'xconfigure 'override-redirect)



;;;; Functions for accessing XGravityEvent slots

(declaim (inline gravity-event-event gravity-event-window gravity-event-x
		 gravity-event-y))

(def-event-window-access gravity-event-event 'xgravity 'event)
(def-event-window-access gravity-event-window 'xgravity 'window)
(def-event-access gravity-event-x 'xgravity 'x)
(def-event-access gravity-event-y 'xgravity 'y)



;;;; Functions for accessing XResizeRequestEvent structures

(declaim (inline resize-request-event-width resize-request-event-height))

(def-event-access resize-request-event-width 'xresizerequest 'width)
(def-event-access resize-request-event-height 'xresizerequest 'height)



;;;; Functions for accessing XConfigureRequestEvent structures

(declaim (inline configure-request-event-parent
		 configure-request-event-window configure-request-event-x
		 configure-request-event-y configure-request-event-width
		 configure-request-event-height
		 configure-request-event-border-width
		 configure-request-event-above configure-request-event-detail
		 configure-request-event-value-mask))

(def-event-window-access configure-request-event-parent
			 'xconfigurerequest 'parent)
(def-event-window-access configure-request-event-window
			 'xconfigurerequest 'window)
(def-event-access configure-request-event-x 'xconfigurerequest 'x)
(def-event-access configure-request-event-y 'xconfigurerequest 'y)
(def-event-access configure-request-event-width 'xconfigurerequest 'width)
(def-event-access configure-request-event-height 'xconfigurerequest 'height)
(def-event-access configure-request-event-border-width
		  'xconfigurerequest 'border-width)
(def-event-window-access configure-request-event-above
			 'xconfigurerequest 'above)
(def-event-access configure-request-event-detail 'xconfigurerequest 'detail)
(def-event-access configure-request-value-mask 'xconfigurerequest 'value-mask)



;;;; Functions for accessing XCirculateEvent structures

(declaim (inline circulate-event-event circulate-event-window
		 circulate-event-place))

(def-event-window-access circulate-event-event 'xcirculate 'event)
(def-event-window-access circulate-event-window 'xcirculate 'window)
(def-event-access circulate-event-place 'xcirculate 'place)



;;;; Functions for accessing XCirculateRequestEvent slots

(declaim (inline circulate-request-event-parent
		 circulate-request-event-window circulate-request-event-place))

(def-event-window-access circulate-request-event-parent
			 'xcirculaterequest 'parent)
(def-event-window-access circulate-request-event-window
			 'xcirculaterequest 'window)
(def-event-access circulate-request-event-place 'xcirculaterequest 'place)



;;;; Functions for accessing XPropertyEvent slots

(declaim (inline property-event-atom property-event-time property-event-state))

(def-event-atom-access property-event-atom 'xproperty 'atom)
(def-event-access property-event-time 'xproperty 'time)
(def-event-access property-event-state 'xproperty 'state)



;;;; Functions for accessing XSelectionClearEvent slots

(declaim (inline selection-clear-event-selection selection-clear-event-time))

(def-event-atom-access selection-clear-event-selection
		       'xselectionclear 'selection)
(def-event-access selection-clear-event-time 'xselectionclear 'time)



;;;; Functions for accessing XSelectionRequestEvent slots

(declaim (inline selection-request-event-owner
		 selection-request-event-requestor
		 selection-request-event-selection
		 selection-request-event-target
		 selection-request-event-property selection-request-event-time))

(def-event-window-access selection-request-event-owner
			 'xselectionrequest 'owner)
(def-event-window-access selection-request-event-requestor
			 'xselectionrequest 'requestor)
(def-event-atom-access selection-request-event-selection
		       'xselectionrequest 'selection)
(def-event-atom-access selection-request-event-target
		       'xselectionrequest 'target)
(def-event-atom-access selection-request-event-property
		       'xselectionrequest 'property)
(def-event-access selection-request-event-time 'xselectionrequest 'time)



;;;; Functions for accessing XSelectionEvent structures

(declaim (inline selection-event-requestor selection-event-selection
		 selection-event-target selection-event-property
		 selection-event-time))

(def-event-window-access selection-event-requestor 'xselection 'requestor)
(def-event-atom-access selection-event-selection 'xselection 'selection)
(def-event-atom-access selection-event-target 'xselection 'target)
(def-event-atom-access selection-event-property 'xselection 'property)
(def-event-access selection-event-time 'xselection 'time)



;;;; Functions for accessing XColormapEvent structures

(declaim (inline colormap-event-colormap colormap-event-new
		 colormap-event-state))

(def-event-colormap-access colormap-event-colormap 'xcolormap 'colormap)
(def-event-access colormap-event-new 'xcolormap 'new)
(def-event-access colormap-event-state 'xcolormap 'state)



;;;; Functions for accessing XClientMessageEvent structures

(declaim (inline client-message-event-message-type client-message-event-format))

(def-event-atom-access client-message-event-message-type
		       'xclient 'message-type)
(def-event-access client-message-event-format 'xclient 'format)
;; ***** Need to access client-msg data



;;;; Functions for accessing XMappingEvent slots

(declaim (inline mapping-event-request mapping-event-first-keycode
		 mapping-event-count))

(def-event-access mapping-event-request 'xmapping 'request)
(def-event-access mapping-event-first-keycode 'xmapping 'first-keycode)
(def-event-access mapping-event-count 'xmapping 'count)
