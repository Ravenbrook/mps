;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/rompsite.lisp,v 1.17 2003/05/05 16:13:59 emarsden Exp $")
;;;
;;; **********************************************************************
;;;
;;; "Site dependent" stuff for the Hemlock editor
;;;

;;; If we were compiled with CLX support, we require it at runtime
#+clx
(require :clx)

;;; Stuff to set up the packages Hemlock uses.
;;;
(unless (find-package "HEMLOCK-INTERNALS")
  (make-package "HEMLOCK-INTERNALS"
		:nicknames '("HI")
		:use '("LISP" "EXTENSIONS" "SYSTEM")))
;;;
(unless (find-package "HEMLOCK")
  (make-package "HEMLOCK"
		:nicknames '("ED")
		:use '("LISP" "HEMLOCK-INTERNALS" "EXTENSIONS" "SYSTEM")))
;;;
(in-package "SYSTEM")
(export '(without-hemlock %sp-byte-blt %sp-find-character
			  %sp-find-character-with-attribute 
			  %sp-reverse-find-character-with-attribute))
;;;
(in-package "HI")

(export '(show-mark editor-sleep *input-transcript* fun-defined-from-pathname
	  editor-describe-function pause-hemlock store-cut-string
	  fetch-cut-string schedule-event remove-scheduled-event
	  enter-window-autoraise directoryp merge-relative-pathnames
	  ;;
	  ;; Export default-font to prevent a name conflict that occurs due to
	  ;; the Hemlock variable "Default Font" defined in SITE-INIT below.
	  ;;
	  default-font))


;;; SYSTEM:WITHOUT-HEMLOCK -- Public.
;;;
;;; Code:lispinit.lisp uses this for a couple interrupt handlers, and
;;; eval-server.lisp.
;;; 
(defmacro system:without-hemlock (&body body)
  "When in the editor and not in the debugger, call the exit method of Hemlock's
   device, so we can type.  Do the same thing on exit but call the init method."
  `(progn
     (when (and *in-the-editor* (not debug::*in-the-debugger*))
       (let ((device (device-hunk-device (window-hunk (current-window)))))
	 (funcall (device-exit device) device)))
     ,@body
     (when (and *in-the-editor* (not debug::*in-the-debugger*))
       (let ((device (device-hunk-device (window-hunk (current-window)))))
	 (funcall (device-init device) device)))))



;;;; SITE-INIT.

;;; *key-event-history* is defined in input.lisp, but it needs to be set in
;;; SITE-INIT, since MAKE-RING doesn't exist at load time for this file.
;;;
(declaim (special *key-event-history*))

;;; SITE-INIT  --  Internal
;;;
;;;    This function is called at init time to set up any site stuff.
;;;
(defun site-init ()
  (defhvar "Beep Border Width"
    "Width in pixels of the border area inverted by beep."
    :value 20)
  (defhvar "Default Window Width"
    "This is used to make a window when prompting the user.  The value is in
     characters."
    :value 80)
  (defhvar "Default Window Height"
    "This is used to make a window when prompting the user.  The value is in
     characters."
    :value 24)
  (defhvar "Default Initial Window Width"
    "This is used when Hemlock first starts up to make its first window.
     The value is in characters."
    :value 80)
  (defhvar "Default Initial Window Height"
    "This is used when Hemlock first starts up to make its first window.
     The value is in characters."
    :value 24)
  (defhvar "Default Initial Window X"
    "This is used when Hemlock first starts up to make its first window.
     The value is in pixels."
    :value nil)
  (defhvar "Default Initial Window Y"
    "This is used when Hemlock first starts up to make its first window.
     The value is in pixels."
    :value nil)
  (defhvar "Bell Style"
    "This controls what beeps do in Hemlock.  Acceptable values are :border-flash
     (which is the default), :feep, :border-flash-and-feep, :flash,
     :flash-and-feep, and NIL (do nothing)."
    :value :border-flash)
  (defhvar "Reverse Video"
    "Paints white on black in window bodies, black on white in modelines."
    :value nil
    :hooks '(reverse-video-hook-fun))
  (defhvar "Cursor Bitmap File"
    "File to read to setup cursors for Hemlock windows.  The mask is found by
     merging this name with \".mask\"."
    :value "library:hemlock11.cursor")
  (defhvar "Enter Window Hook"
    "When the mouse enters an editor window, this hook is invoked.  These
     functions take the Hemlock Window as an argument."
    :value nil)
  (defhvar "Exit Window Hook"
    "When the mouse exits an editor window, this hook is invoked.  These
     functions take the Hemlock Window as an argument."
    :value nil)
  (defhvar "Set Window Autoraise"
    "When non-nil, setting the current window will automatically raise that
     window via a function on \"Set Window Hook\".  If the value is :echo-only
     (the default), then only the echo area window will be raised
     automatically upon becoming current."
    :value :echo-only)
  (defhvar "Default Font"
    "The string name of the font to be used for Hemlock -- buffer text,
     modelines, random typeout, etc.  The font is loaded when initializing
     Hemlock."
    :value "*-courier-medium-r-normal--*-120-*")
  (defhvar "Active Region Highlighting Font"
    "The string name of the font to be used for highlighting active regions.
     The font is loaded when initializing Hemlock."
    :value "*-courier-medium-o-normal--*-120-*")
  (defhvar "Open Paren Highlighting Font"
    "The string name of the font to be used for highlighting open parens.
     The font is loaded when initializing Hemlock."
    :value "*-courier-bold-r-normal--*-120-*")
  (defhvar "Thumb Bar Meter"
    "When non-nil (the default), windows will be created to be displayed with
     a ruler in the bottom border of the window."
    :value t)

  (setf *key-event-history* (make-ring 60))
  nil)


;;;; Some generally useful file-system functions.

;;; MERGE-RELATIVE-PATHNAMES takes a pathname that is either absolute or
;;; relative to default-dir, merging it as appropriate and returning a definite
;;; directory pathname.
;;;
;;; This function isn't really needed anymore now that merge-pathnames does
;;; this, but the semantics are slightly different.  So it's easier to just
;;; keep this around instead of changing all the uses of it.
;;; 
(defun merge-relative-pathnames (pathname default-directory)
  "Merges pathname with default-directory.  If pathname is not absolute, it
   is assumed to be relative to default-directory.  The result is always a
   directory."
  (let ((pathname (merge-pathnames pathname default-directory)))
    (if (directoryp pathname)
	pathname
	(pathname (concatenate 'simple-string
			       (namestring pathname)
			       "/")))))

(defun directoryp (pathname)
  "Returns whether pathname names a directory, that is whether it has no
   name and no type components."
  (not (or (pathname-name pathname) (pathname-type pathname))))



;;;; I/O specials and initialization

;;; File descriptor for the terminal.
;;; 
(defvar *editor-file-descriptor*)


;;; This is a hack, so screen can tell how to initialize screen management
;;; without re-opening the display.  It is set in INIT-RAW-IO and referenced
;;; in WINDOWED-MONITOR-P.
;;; 
(defvar *editor-windowed-input* nil)

;;; These are used for selecting X events.
#+clx
(eval-when (compile load eval)
  (defconstant group-interesting-xevents
    '(:structure-notify)))
#+clx
(defconstant group-interesting-xevents-mask
  (apply #'xlib:make-event-mask group-interesting-xevents))

#+clx
(eval-when (compile load eval)
  (defconstant child-interesting-xevents
    '(:key-press :button-press :button-release :structure-notify :exposure
		 :enter-window :leave-window)))
#+clx
(defconstant child-interesting-xevents-mask
  (apply #'xlib:make-event-mask child-interesting-xevents))

#+clx
(eval-when (compile load eval)
  (defconstant random-typeout-xevents
    '(:key-press :button-press :button-release :enter-window :leave-window
		 :exposure)))
#+clx
(defconstant random-typeout-xevents-mask
  (apply #'xlib:make-event-mask random-typeout-xevents))


#+clx
(declaim (special ed::*open-paren-highlight-font*
		  ed::*active-region-highlight-font*))

#+clx
(defparameter lisp-fonts-pathnames '("library:fonts/"))

(declaim (special *editor-input* *real-editor-input*))

(declaim (special *editor-input* *real-editor-input*))

;;; INIT-RAW-IO  --  Internal
;;;
;;;    This function should be called whenever the editor is entered in a new
;;; lisp.  It sets up process specific data structures.
;;;
(defun init-raw-io (display)
  #-clx (declare (ignore display))
  (setf *editor-windowed-input* nil)
  (cond #+clx
	(display
	 (setf *editor-windowed-input* (ext:open-clx-display display))
	 (setf *editor-input* (make-windowed-editor-input))
	 (setup-font-family *editor-windowed-input*))
	(t ;; The editor's file descriptor is Unix standard input (0).
	   ;; We don't need to affect system:*file-input-handlers* here
	   ;; because the init and exit methods for tty redisplay devices
	   ;; take care of this.
	   ;;
	 (setf *editor-file-descriptor* 0)
	 (setf *editor-input* (make-tty-editor-input 0))))
  (setf *real-editor-input* *editor-input*)
  *editor-windowed-input*)

;;; Stop flaming from compiler due to CLX macros expanding into illegal
;;; declarations.
;;;
(declaim (declaration values))
(declaim (special *default-font-family*))

;;; font-map-size should be defined in font.lisp, but SETUP-FONT-FAMILY would
;;; assume it to be special, issuing a nasty warning.
;;;
#+clx
(defconstant font-map-size 16
  "The number of possible fonts in a font-map.")
#-clx
(defconstant font-map-size 16)

;;; SETUP-FONT-FAMILY sets *default-font-family*, opening the three font names
;;; passed in.  The font family structure is filled in from the first argument.
;;; Actually, this ignores default-highlight-font and default-open-paren-font
;;; in lieu of "Active Region Highlighting Font" and "Open Paren Highlighting
;;; Font" when these are defined.
;;;
#+clx
(defun setup-font-family (display)
  (let* ((font-family (make-font-family :map (make-array font-map-size
							 :initial-element 0)
					:cursor-x-offset 0
					:cursor-y-offset 0))
	 (font-family-map (font-family-map font-family)))
    (declare (simple-vector font-family-map))
    (setf *default-font-family* font-family)
    (let ((font (xlib:open-font display (variable-value 'ed::default-font))))
      (unless font
	(error "Cannot open font -- ~S" (variable-value 'ed::default-font)))
      (fill font-family-map font)
      (let ((width (xlib:max-char-width font)))
	(setf (font-family-width font-family) width)
	(setf (font-family-cursor-width font-family) width))
      (let* ((baseline (xlib:font-ascent font))
	     (height (+ baseline (xlib:font-descent font))))
	(setf (font-family-height font-family) height)
	(setf (font-family-cursor-height font-family) height)
	(setf (font-family-baseline font-family) baseline)))
    (setup-one-font display
		    (variable-value 'ed::open-paren-highlighting-font)
		    font-family-map
		    ed::*open-paren-highlight-font*)
    (setup-one-font display
		    (variable-value 'ed::active-region-highlighting-font)
		    font-family-map
		    ed::*active-region-highlight-font*)))

;;; SETUP-ONE-FONT tries to open font-name for display, storing the result in
;;; font-family-map at index.  XLIB:OPEN-FONT will return font stuff regardless
;;; if the request is valid or not, so we finish the output to get synch'ed
;;; with the server which will cause any errors to get signaled.  At this
;;; level, we want to deal with this error here returning nil if the font
;;; couldn't be opened.
;;;
#+clx
(defun setup-one-font (display font-name font-family-map index)
  (handler-case (let ((font (xlib:open-font display (namestring font-name))))
		  (xlib:display-finish-output display)
		  (setf (svref font-family-map index) font))
    (xlib:name-error ()
     (warn "Cannot open font -- ~S" font-name)
     nil)))


;;;; HEMLOCK-BEEP.

(defvar *editor-bell* (make-string 1 :initial-element #\bell))

;;; TTY-BEEP is used in Hemlock for beeping when running under a terminal.
;;; Send a #\bell to unix standard output.
;;;
(defun tty-beep (&optional device stream)
  (declare (ignore device stream))
  (when (variable-value 'ed::bell-style)
    (unix:unix-write 1 *editor-bell* 0 1)))

(declaim (special *current-window*))

;;; BITMAP-BEEP is used in Hemlock for beeping when running under windowed
;;; input.
;;;
#+clx
(defun bitmap-beep (device stream)
  (declare (ignore stream))
  (let ((display (bitmap-device-display device)))
    (ecase (variable-value 'ed::bell-style)
      (:border-flash
       (flash-window-border *current-window*))
      (:feep
       (xlib:bell display)
       (xlib:display-force-output display))
      (:border-flash-and-feep
       (xlib:bell display)
       (xlib:display-force-output display)
       (flash-window-border *current-window*))
      (:flash
       (flash-window *current-window*))
      (:flash-and-feep
       (xlib:bell display)
       (xlib:display-force-output display)
       (flash-window *current-window*))
      ((nil) ;Do nothing.
       ))))

#+clx
(declaim (special *foreground-background-xor*))

#+clx
(defun flash-window-border (window)
  (let* ((hunk (window-hunk window))
	 (xwin (bitmap-hunk-xwindow hunk))
	 (gcontext (bitmap-hunk-gcontext hunk))
	 (display (bitmap-device-display (device-hunk-device hunk)))
	 (border (variable-value 'ed::beep-border-width))
	 (h (or (bitmap-hunk-modeline-pos hunk) (bitmap-hunk-height hunk)))
	 (top-border (min (ash h -1) border))
	 (w (bitmap-hunk-width hunk))
	 (side-border (min (ash w -1) border))
	 (top-width (max 0 (- w (ash side-border 1))))
	 (right-x (- w side-border))
	 (bottom-y (- h top-border)))
    (xlib:with-gcontext (gcontext :function xlib::boole-xor
				  :foreground *foreground-background-xor*)
      (flet ((zot ()
	       (xlib:draw-rectangle xwin gcontext 0 0 side-border h t)
	       (xlib:draw-rectangle xwin gcontext side-border bottom-y
				    top-width top-border t)
	       (xlib:draw-rectangle xwin gcontext right-x 0 side-border h t)
	       (xlib:draw-rectangle xwin gcontext side-border 0
				    top-width top-border t)))
	(zot)
	(xlib:display-force-output display)
	(sleep 0.1)
	(zot)
	(xlib:display-force-output display)))))

#+clx
(defun flash-window (window)
  (let* ((hunk (window-hunk window))
	 (xwin (bitmap-hunk-xwindow hunk))
	 (gcontext (bitmap-hunk-gcontext hunk))
	 (display (bitmap-device-display (device-hunk-device hunk)))
	 (width (bitmap-hunk-width hunk))
	 (height (or (bitmap-hunk-modeline-pos hunk)
		     (bitmap-hunk-height hunk))))
    (xlib:with-gcontext (gcontext :function xlib::boole-xor
				  :foreground *foreground-background-xor*)
      (xlib:draw-rectangle xwin gcontext 0 0 width height t)
      (xlib:display-force-output display)
      (sleep 0.1)
      (xlib:draw-rectangle xwin gcontext 0 0 width height t)
      (xlib:display-force-output display))))

(defun hemlock-beep (stream)
  "Using the current window, calls the device's beep function on stream."
  (let ((device (device-hunk-device (window-hunk (current-window)))))
    (funcall (device-beep device) device stream)))



;;;; GC messages.

;;; HEMLOCK-GC-NOTIFY-BEFORE and HEMLOCK-GC-NOTIFY-AFTER both MESSAGE GC
;;; notifications when Hemlock is not running under X11.  It cannot affect
;;; its window's without using its display connection.  Since GC can occur
;;; inside CLX request functions, using the same display confuses CLX.
;;;

(defun hemlock-gc-notify-before (bytes-in-use)
  (let ((control "~%[GC threshold exceeded with ~:D bytes in use.  ~
  		  Commencing GC.]~%"))
    (cond ((not hi::*editor-windowed-input*)
	   (beep)
	   #|(message control bytes-in-use)|#)
	  (t
	   ;; Can't call BEEP since it would use Hemlock's display connection.
	   (lisp::default-beep-function *standard-output*)
	   (format t control bytes-in-use)
	   (finish-output)))))

(defun hemlock-gc-notify-after (bytes-retained bytes-freed trigger)
  (let ((control
	 "[GC completed with ~:D bytes retained and ~:D bytes freed.]~%~
	  [GC will next occur when at least ~:D bytes are in use.]~%"))
    (cond ((not hi::*editor-windowed-input*)
	   (beep)
	   #|(message control bytes-retained bytes-freed)|#)
	  (t
	   ;; Can't call BEEP since it would use Hemlock's display connection.
	   (lisp::default-beep-function *standard-output*)
	   (format t control bytes-retained bytes-freed trigger)
	   (finish-output)))))



;;;; Site-Wrapper-Macro and standard device init/exit functions.

(defun in-hemlock-standard-input-read (stream &rest ignore)
  (declare (ignore ignore))
  (error "You cannot read off this stream while in Hemlock -- ~S"
	 stream))

(defvar *illegal-read-stream*
  (lisp::make-lisp-stream :in #'in-hemlock-standard-input-read))

(defmacro site-wrapper-macro (&body body)
  `(unwind-protect
     (progn
       (when *editor-has-been-entered*
	 (let ((device (device-hunk-device (window-hunk (current-window)))))
	   (funcall (device-init device) device)))
       (let ((*beep-function* #'hemlock-beep)
	     (*gc-notify-before* #'hemlock-gc-notify-before)
	     (*gc-notify-after* #'hemlock-gc-notify-after)
	     (*standard-input* *illegal-read-stream*)
	     (*query-io* *illegal-read-stream*))
	 (cond ((not *editor-windowed-input*)
		,@body)
	       (t
		#+clx
		(ext:with-clx-event-handling
		    (*editor-windowed-input* #'ext:object-set-event-handler)
		  ,@body)))))
     (let ((device (device-hunk-device (window-hunk (current-window)))))
       (funcall (device-exit device) device))))

(defun standard-device-init ()
  (setup-input))

(defun standard-device-exit ()
  (reset-input))

(declaim (special *echo-area-window*))

;;; Maybe bury/unbury hemlock window when we go to and from Lisp.
;;; This should do something more sophisticated when we know what that is.
;;; 
#+clx
(defun default-hemlock-window-mngt (display on)
  (let ((xparent (window-group-xparent
		  (bitmap-hunk-window-group (window-hunk *current-window*))))
	(echo-xparent (window-group-xparent
		       (bitmap-hunk-window-group
			(window-hunk *echo-area-window*)))))
    (cond (on (setf (xlib:window-priority echo-xparent) :above)
	      (clear-editor-input *editor-input*)
	      (setf (xlib:window-priority xparent) :above))
	  (t (setf (xlib:window-priority echo-xparent) :below)
	     (setf (xlib:window-priority xparent) :below))))
  (xlib:display-force-output display))

(defvar *hemlock-window-mngt* nil;#'default-hemlock-window-mngt
  "This function is called by HEMLOCK-WINDOW, passing its arguments.  This may
   be nil.")

(defun hemlock-window (display on)
  "Calls *hemlock-window-mngt* on the argument ON when *current-window* is
  bound.  This is called in the device init and exit methods for X bitmap
  devices."
  (when (and *hemlock-window-mngt* *current-window*)
    (funcall *hemlock-window-mngt* display on)))



;;;; Line Wrap Char.

(defvar *line-wrap-char* #\!
  "The character to be displayed to indicate wrapped lines.")


;;;; Current terminal character translation.

(defvar termcap-file "/etc/termcap")



;;;; Event scheduling.

;;; The time queue provides a ROUGH mechanism for scheduling events to
;;; occur after a given amount of time has passed, optionally repeating
;;; using the given time as an interval for rescheduling.  When the input
;;; loop goes around, it will check the current time and process all events
;;; that should have happened before or at this time.  The function gets
;;; called on the number of seconds that have elapsed since it was last
;;; called.
;;;
;;; NEXT-SCHEDULED-EVENT-WAIT and INVOKE-SCHEDULED-EVENTS are used in the
;;; editor stream in methods.
;;;
;;; SCHEDULE-EVENT and REMOVE-SCHEDULED-EVENT are exported interfaces.

(defstruct (tq-event (:print-function print-tq-event)
		     (:constructor make-tq-event
				   (time last-time interval function)))
  time		; When the event should happen.
  last-time	; When the event was scheduled.
  interval	; When non-nil, how often the event should happen.
  function)	; What to do.

(defun print-tq-event (obj stream n)
  (declare (ignore n))
  (format stream "#<Tq-Event ~S>" (tq-event-function obj)))

(defvar *time-queue* nil
  "This is the time priority queue used in Hemlock input streams for event
   scheduling.")

;;; QUEUE-TIME-EVENT inserts event into the time priority queue *time-queue*.
;;; Event is inserted before the first element that it is less than (which
;;; means that it gets inserted after elements that are the same).
;;; *time-queue* is returned.
;;; 
(defun queue-time-event (event)
  (let ((time (tq-event-time event)))
    (if *time-queue*
	(if (< time (tq-event-time (car *time-queue*)))
	    (push event *time-queue*)
	    (do ((prev *time-queue* rest)
		 (rest (cdr *time-queue*) (cdr rest)))
		((or (null rest)
		     (< time (tq-event-time (car rest))))
		 (push event (cdr prev))
		 *time-queue*)))
	(push event *time-queue*))))

;;; NEXT-SCHEDULED-EVENT-WAIT returns nil or the number of seconds to wait for
;;; the next event to happen.
;;; 
(defun next-scheduled-event-wait ()
  (if *time-queue*
      (let ((wait (round (- (tq-event-time (car *time-queue*))
			    (get-internal-real-time))
			 internal-time-units-per-second)))
	(if (plusp wait) wait 0))))

;;; INVOKE-SCHEDULED-EVENTS invokes all the functions in *time-queue* whose
;;; time has come.  If we run out of events, or there are none, then we get
;;; out.  If we popped an event whose time hasn't come, we push it back on the
;;; queue.  Each function is called on how many seconds, roughly, went by since
;;; the last time it was called (or scheduled).  If it has an interval, we
;;; re-queue it.  While invoking the function, bind *time-queue* to nothing in
;;; case the event function tries to read off *editor-input*.
;;;
(defun invoke-scheduled-events ()
  (let ((time (get-internal-real-time)))
    (loop
      (unless *time-queue* (return))
      (let* ((event (car *time-queue*))
	     (event-time (tq-event-time event)))
	(cond ((>= time event-time)
	       (let ((*time-queue* nil))
		 (funcall (tq-event-function event)
			  (round (- time (tq-event-last-time event))
				 internal-time-units-per-second)))
	       (without-interrupts
		(let ((interval (tq-event-interval event)))
		  (when interval
		    (setf (tq-event-time event) (+ time interval))
		    (setf (tq-event-last-time event) time)
		    (pop *time-queue*)
		    (queue-time-event event)))))
	      (t (return)))))))

(defun schedule-event (time function &optional (repeat t))
  "This causes function to be called after time seconds have passed,
   optionally repeating every time seconds.  This is a rough mechanism
   since commands can take an arbitrary amount of time to run; the function
   will be called at the first possible moment after time has elapsed.
   Function takes the time that has elapsed since the last time it was
   called (or since it was scheduled for the first invocation)."
  (let ((now (get-internal-real-time))
	(itime (* internal-time-units-per-second time)))
    (queue-time-event (make-tq-event (+ itime now) now (if repeat itime)
				     function))))

(defun remove-scheduled-event (function)
  "Removes function queued with SCHEDULE-EVENT."
  (setf *time-queue* (delete function *time-queue* :key #'tq-event-function)))



;;;; Editor sleeping.

(defun editor-sleep (time)
  "Sleep for approximately Time seconds."
  (unless (or (zerop time) (listen-editor-input *editor-input*))
    (internal-redisplay)
    (sleep-for-time time)
    nil))

(defun sleep-for-time (time)
  (let ((nrw-fun (device-note-read-wait
		  (device-hunk-device (window-hunk (current-window)))))
	(end (+ (get-internal-real-time)
		(truncate (* time internal-time-units-per-second)))))
    (loop
      (when (listen-editor-input *editor-input*)
	(return))
      (let ((left (- end (get-internal-real-time))))
	(unless (plusp left) (return nil))
	(when nrw-fun (funcall nrw-fun t))
	(system:serve-event (/ (float left)
			       (float internal-time-units-per-second)))))
    (when nrw-fun (funcall nrw-fun nil))))



;;;; Showing a mark.

(defun show-mark (mark window time)
  "Highlights the position of Mark within Window for Time seconds,
   possibly by moving the cursor there.  If Mark is not displayed within
   Window return NIL.  The wait may be aborted if there is pending input."
  (let* ((result t))
    (catch 'redisplay-catcher
      (redisplay-window window)
      (setf result
	    (multiple-value-bind (x y) (mark-to-cursorpos mark window)
	      (funcall (device-show-mark
			(device-hunk-device (window-hunk window)))
		       window x y time))))
    result))

(defun tty-show-mark (window x y time)
  (cond ((listen-editor-input *editor-input*))
	(x (internal-redisplay)
	   (let* ((hunk (window-hunk window))
		  (device (device-hunk-device hunk)))
	     (funcall (device-put-cursor device) hunk x y)
	     (when (device-force-output device)
	       (funcall (device-force-output device)))
	     (sleep-for-time time))
	   t)
	(t nil)))

#+clx
(defun bitmap-show-mark (window x y time)
  (cond ((listen-editor-input *editor-input*))
	(x (let* ((hunk (window-hunk window))
		  (display (bitmap-device-display (device-hunk-device hunk))))
	     (internal-redisplay)
	     (hunk-show-cursor hunk x y)
	     (drop-cursor)
	     (xlib:display-finish-output display)
	     (sleep-for-time time)
	     (lift-cursor)
	     t))
	(t nil)))


;;;; Function description and defined-from.

;;; FUN-DEFINED-FROM-PATHNAME takes a symbol or function object.  It
;;; returns a pathname for the file the function was defined in.  If it was
;;; not defined in some file, then nil is returned.
;;; 
(defun fun-defined-from-pathname (function)
  "Takes a symbol or function and returns the pathname for the file the
   function was defined in.  If it was not defined in some file, nil is
   returned."
  (flet ((frob (code)
	   (let ((info (kernel:%code-debug-info code)))
	     (when info
	       (let ((sources (c::debug-info-source info)))
		 (when sources
		   (let ((source (car sources)))
		     (when (eq (c::debug-source-from source) :file)
		       (c::debug-source-name source)))))))))
    (typecase function
      (symbol (fun-defined-from-pathname (fdefinition function)))
      (kernel:byte-closure
       (fun-defined-from-pathname (kernel:byte-closure-function function)))
      (kernel:byte-function
       (frob (c::byte-function-component function)))
      (function
       (frob (kernel:function-code-header (kernel:%function-self function))))
      (t nil))))


(defvar *editor-describe-stream*
  (system:make-indenting-stream *standard-output*))

;;; EDITOR-DESCRIBE-FUNCTION has to mess around to get indenting streams to
;;; work.  These apparently work fine for DESCRIBE, for which they were defined,
;;; but not in general.  It seems they don't indent initial text, only that
;;; following a newline, so inside our use of INDENTING-FURTHER, we need some
;;; form before the WRITE-STRING.  To get this to work, I had to remove the ~%
;;; from the FORMAT string, and use FRESH-LINE; simply using FRESH-LINE with
;;; the ~% caused an extra blank line.  Possibly I should not have glommed onto
;;; this hack whose interface comes from three different packages, but it did
;;; the right thing ....
;;;
;;; Also, we have set INDENTING-STREAM-STREAM to make sure the indenting stream
;;; is based on whatever *standard-output* is when we are called.
;;;
(defun editor-describe-function (fun sym)
  "Calls DESCRIBE on fun.  If fun is compiled, and its original name is not sym,
   then this also outputs any 'function documentation for sym to
   *standard-output*."
  (describe fun)
  (when (and (compiled-function-p fun)
	     (not (eq (kernel:%function-name (kernel:%closure-function fun))
		      sym)))
    (let ((doc (documentation sym 'function)))
      (when doc
	(format t "~&Function documentation for ~S:" sym)
	(setf (lisp::indenting-stream-stream *editor-describe-stream*)
	      *standard-output*)
	(ext:indenting-further *editor-describe-stream* 2
	  (fresh-line *editor-describe-stream*)
	  (write-string doc *editor-describe-stream*))))))




;;;; X Stuff.
;;; Setting window cursors ...
;;; 

#+clx
(declaim (special *default-foreground-pixel* *default-background-pixel*))

#+clx
(defvar *hemlock-cursor* nil "Holds cursor for Hemlock windows.")

;;; DEFINE-WINDOW-CURSOR in shoved on the "Make Window Hook".
;;; 
#+clx
(defun define-window-cursor (window)
  (setf (xlib:window-cursor (bitmap-hunk-xwindow (window-hunk window)))
	*hemlock-cursor*))

;;; These are set in INIT-BITMAP-SCREEN-MANAGER and REVERSE-VIDEO-HOOK-FUN.
;;;
#+clx
(defvar *cursor-foreground-color* nil)
#+clx
(defvar *cursor-background-color* nil)
#+clx
(defun make-white-color () (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
#+clx
(defun make-black-color () (xlib:make-color :red 0.0 :green 0.0 :blue 0.0))


;;; GET-HEMLOCK-CURSOR is used in INIT-BITMAP-SCREEN-MANAGER to load the
;;; hemlock cursor for DEFINE-WINDOW-CURSOR.
;;;
#+clx
(defun get-hemlock-cursor (display)
  (when *hemlock-cursor* (xlib:free-cursor *hemlock-cursor*))
  (let* ((cursor-file (truename (variable-value 'ed::cursor-bitmap-file)))
	 (mask-file (probe-file (make-pathname :type "mask"
					       :defaults cursor-file)))
	 (root (xlib:screen-root (xlib:display-default-screen display)))
	 (mask-pixmap (if mask-file (get-cursor-pixmap root mask-file))))
    (multiple-value-bind (cursor-pixmap cursor-x-hot cursor-y-hot)
			 (get-cursor-pixmap root cursor-file)
      (setf *hemlock-cursor*
	    (xlib:create-cursor :source cursor-pixmap :mask mask-pixmap
				:x cursor-x-hot :y cursor-y-hot
				:foreground *cursor-foreground-color*
				:background *cursor-background-color*))
      (xlib:free-pixmap cursor-pixmap)
      (when mask-pixmap (xlib:free-pixmap mask-pixmap)))))

#+clx
(defun get-cursor-pixmap (root pathname)
  (let* ((image (xlib:read-bitmap-file pathname))
	 (pixmap (xlib:create-pixmap :width 16 :height 16
				     :depth 1 :drawable root))
	 (gc (xlib:create-gcontext
	      :drawable pixmap :function boole-1
	      :foreground *default-foreground-pixel*
	      :background *default-background-pixel*)))
    (xlib:put-image pixmap gc image :x 0 :y 0 :width 16 :height 16)
    (xlib:free-gcontext gc)
    (values pixmap (xlib:image-x-hot image) (xlib:image-y-hot image))))


;;; Setting up grey borders ...
;;; 

#+clx
(defparameter hemlock-grey-bitmap-data
  '(#*10 #*01))

#+clx
(defun get-hemlock-grey-pixmap (display)
  (let* ((screen (xlib:display-default-screen display))
	 (depth (xlib:screen-root-depth screen))
	 (root (xlib:screen-root screen))
	 (height (length hemlock-grey-bitmap-data))
	 (width (length (car hemlock-grey-bitmap-data)))
	 (image (apply #'xlib:bitmap-image hemlock-grey-bitmap-data))
	 (pixmap (xlib:create-pixmap :width width :height height
				     :depth depth :drawable root))
	 (gc (xlib:create-gcontext :drawable pixmap
				   :function boole-1
				   :foreground *default-foreground-pixel*
				   :background *default-background-pixel*)))
    (xlib:put-image pixmap gc image
		    :x 0 :y 0 :width width :height height :bitmap-p t)
    (xlib:free-gcontext gc)
    pixmap))


;;; Cut Buffer manipulation ...
;;;

#+clx
(defun store-cut-string (display string)
  (check-type string simple-string)
  (setf (xlib:cut-buffer display) string))

#+clx
(defun fetch-cut-string (display)
  (xlib:cut-buffer display))


;;; Window naming ...
;;;
#+clx
(defun set-window-name-for-buffer-name (buffer new-name)
  (dolist (ele (buffer-windows buffer))
    (xlib:set-standard-properties (bitmap-hunk-xwindow (window-hunk ele))
				  :icon-name new-name)))
  
#+clx
(defun set-window-name-for-window-buffer (window new-buffer)
  (xlib:set-standard-properties (bitmap-hunk-xwindow (window-hunk window))
				:icon-name (buffer-name new-buffer)))


;;;; Some hacks for supporting Hemlock under Mach.

;;; WINDOWED-MONITOR-P is used by the reverse video variable's hook function
;;; to determine if it needs to go around fixing all the windows.
;;;
(defun windowed-monitor-p ()
  "This returns whether the monitor is being used with a window system.  It
   returns the console's CLX display structure."
  *editor-windowed-input*)

(defun get-terminal-name ()
  (cdr (assoc :term *environment-list* :test #'eq)))

(defun get-termcap-env-var ()
  (cdr (assoc :termcap *environment-list* :test #'eq)))


;;; GET-EDITOR-TTY-INPUT reads from stream's Unix file descriptor queuing events
;;; in the stream's queue.
;;;
(defun get-editor-tty-input (fd)
  (alien:with-alien ((buf (alien:array c-call:unsigned-char 256)))
    (multiple-value-bind
	(len errno)
	(unix:unix-read fd (alien:alien-sap buf) 256)
      (declare (type (or null fixnum) len))
      (unless len
	(error "Problem with tty input: ~S"
	       (unix:get-unix-error-msg errno)))
      (dotimes (i len t)
	(q-event *real-editor-input*
		 (ext:char-key-event (code-char (alien:deref buf i))))))))

(defun editor-tty-listen (stream)
  (alien:with-alien ((nc c-call:int))
    (and (unix:unix-ioctl (tty-editor-input-fd stream)
			  unix::FIONREAD
			  (alien:alien-sap (alien:addr nc)))
	 (> nc 0))))

(defvar old-flags)

(defvar old-tchars)

#-glibc2
(defvar old-ltchars)

#+(or hpux irix bsd glibc2)
(progn
  (defvar old-c-iflag)
  (defvar old-c-oflag)
  (defvar old-c-cflag)
  (defvar old-c-lflag)
  (defvar old-c-cc))

(defun setup-input ()
  (let ((fd *editor-file-descriptor*))
    (when (unix:unix-isatty 0)
      #+(or hpux irix bsd glibc2)
      (alien:with-alien ((tios (alien:struct unix:termios)))
	(multiple-value-bind
	    (val err)
	    (unix:unix-tcgetattr fd (alien:alien-sap tios))
	  (when (null val)
	    (error "Could not tcgetattr, unix error ~S."
		   (unix:get-unix-error-msg err))))
	(setf old-c-iflag (alien:slot tios 'unix:c-iflag))
	(setf old-c-oflag (alien:slot tios 'unix:c-oflag))
	(setf old-c-cflag (alien:slot tios 'unix:c-cflag))
	(setf old-c-lflag (alien:slot tios 'unix:c-lflag))
	(setf old-c-cc
	      (vector (alien:deref (alien:slot tios 'unix:c-cc) unix:vdsusp)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:veof)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vintr)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vquit)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vstart)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vstop)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vsusp)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vmin)
		      (alien:deref (alien:slot tios 'unix:c-cc) unix:vtime)))
	(setf (alien:slot tios 'unix:c-lflag)
	      (logand (alien:slot tios 'unix:c-lflag)
		      (lognot (logior unix:tty-echo unix:tty-icanon))))
	(setf (alien:slot tios 'unix:c-iflag)
	      (logand (alien:slot tios 'unix:c-iflag)
		      (lognot (logior unix:tty-icrnl unix:tty-ixon))))
	(setf (alien:slot tios 'unix:c-oflag)
	      (logand (alien:slot tios 'unix:c-oflag)
		      (lognot #-bsd unix:tty-ocrnl
			      #+bsd unix:tty-onlcr)))
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vdsusp) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:veof) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vintr)
	      (if *editor-windowed-input* #xff 28))
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vquit) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vstart) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vstop) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vsusp) #xff)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vmin) 1)
	(setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vtime) 0)
	(multiple-value-bind
	    (val err)
	    (unix:unix-tcsetattr fd unix:tcsaflush (alien:alien-sap tios))
	  (when (null val)
	    (error "Could not tcsetattr, unix error ~S."
		   (unix:get-unix-error-msg err)))))
      #-(or hpux irix bsd glibc2)
      (alien:with-alien ((sg (alien:struct unix:sgttyb)))
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCGETP (alien:alien-sap sg))
	  (unless val
	    (error "Could not get tty information, unix error ~S."
		   (unix:get-unix-error-msg err))))
	(let ((flags (alien:slot sg 'unix:sg-flags)))
	  (setq old-flags flags)
	  (setf (alien:slot sg 'unix:sg-flags)
		(logand #-(or hpux irix bsd glibc2) (logior flags unix:tty-cbreak)
			(lognot unix:tty-echo)
			(lognot unix:tty-crmod)))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-ioctl fd unix:TIOCSETP (alien:alien-sap sg))
	    (if (null val)
		(error "Could not set tty information, unix error ~S."
		       (unix:get-unix-error-msg err))))))
      #-(or hpux irix bsd glibc2)
      (alien:with-alien ((tc (alien:struct unix:tchars)))
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCGETC (alien:alien-sap tc))
	  (unless val
	    (error "Could not get tty tchars information, unix error ~S."
		   (unix:get-unix-error-msg err))))
	(setq old-tchars
	      (vector (alien:slot tc 'unix:t-intrc)
		      (alien:slot tc 'unix:t-quitc)
		      (alien:slot tc 'unix:t-startc)
		      (alien:slot tc 'unix:t-stopc)
		      (alien:slot tc 'unix:t-eofc)
		      (alien:slot tc 'unix:t-brkc)))
	(setf (alien:slot tc 'unix:t-intrc)
	      (if *editor-windowed-input* -1 28))
	(setf (alien:slot tc 'unix:t-quitc) -1)
	(setf (alien:slot tc 'unix:t-startc) -1)
	(setf (alien:slot tc 'unix:t-stopc) -1)
	(setf (alien:slot tc 'unix:t-eofc) -1)
	(setf (alien:slot tc 'unix:t-brkc) -1)
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCSETC (alien:alien-sap tc))
	  (unless val
	    (error "Failed to set tchars, unix error ~S."
		   (unix:get-unix-error-msg err)))))

      ;; Needed even under HpUx to suppress dsuspc.
      #-(or glibc2 irix)
      (alien:with-alien ((tc (alien:struct unix:ltchars)))
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCGLTC (alien:alien-sap tc))
	  (unless val
	    (error "Could not get tty ltchars information, unix error ~S."
		   (unix:get-unix-error-msg err))))
	(setq old-ltchars
	      (vector (alien:slot tc 'unix:t-suspc)
		      (alien:slot tc 'unix:t-dsuspc)
		      (alien:slot tc 'unix:t-rprntc)
		      (alien:slot tc 'unix:t-flushc)
		      (alien:slot tc 'unix:t-werasc)
		      (alien:slot tc 'unix:t-lnextc)))
	(setf (alien:slot tc 'unix:t-suspc) -1)
	(setf (alien:slot tc 'unix:t-dsuspc) -1)
	(setf (alien:slot tc 'unix:t-rprntc) -1)
	(setf (alien:slot tc 'unix:t-flushc) -1)
	(setf (alien:slot tc 'unix:t-werasc) -1)
	(setf (alien:slot tc 'unix:t-lnextc) -1)
	(multiple-value-bind
	    (val err)
	    (unix:unix-ioctl fd unix:TIOCSLTC (alien:alien-sap tc))
	  (unless val
	    (error "Failed to set ltchars, unix error ~S."
		   (unix:get-unix-error-msg err))))))))

(defun reset-input ()
  (when (unix:unix-isatty 0)
    (let ((fd *editor-file-descriptor*))
      #+(or hpux irix bsd glibc2)
      (when (boundp 'old-c-lflag)
	(alien:with-alien ((tios (alien:struct unix:termios)))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-tcgetattr fd (alien:alien-sap tios))
	    (when (null val)
	      (error "Could not tcgetattr, unix error ~S."
		     (unix:get-unix-error-msg err))))
	  (setf (alien:slot tios 'unix:c-iflag) old-c-iflag)
	  (setf (alien:slot tios 'unix:c-oflag) old-c-oflag)
	  (setf (alien:slot tios 'unix:c-cflag) old-c-cflag)
	  (setf (alien:slot tios 'unix:c-lflag) old-c-lflag)
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vdsusp)
		(svref old-c-cc 0))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:veof)
		(svref old-c-cc 1))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vintr)
		(svref old-c-cc 2))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vquit)
		(svref old-c-cc 3))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vstart)
		(svref old-c-cc 4))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vstop)
		(svref old-c-cc 5))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vsusp)
		(svref old-c-cc 6))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vmin)
		(svref old-c-cc 7))
	  (setf (alien:deref (alien:slot tios 'unix:c-cc) unix:vtime)
		(svref old-c-cc 8))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-tcsetattr fd unix:tcsaflush (alien:alien-sap tios))
	    (when (null val)
	      (error "Could not tcsetattr, unix error ~S."
		     (unix:get-unix-error-msg err))))))
      #-(or hpux irix bsd glibc2)
      (when (boundp 'old-flags)
	(alien:with-alien ((sg (alien:struct unix:sgttyb)))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-ioctl fd unix:TIOCGETP (alien:alien-sap sg))
	    (unless val
	      (error "Could not get tty information, unix error ~S."
		     (unix:get-unix-error-msg err)))
	    (setf (alien:slot sg 'unix:sg-flags) old-flags)
	    (multiple-value-bind
		(val err)
		(unix:unix-ioctl fd unix:TIOCSETP (alien:alien-sap sg))
	      (unless val
		(error "Could not set tty information, unix error ~S."
		       (unix:get-unix-error-msg err)))))))
      #-(or hpux irix bsd glibc2)
      (when (and (boundp 'old-tchars)
		 (simple-vector-p old-tchars)
		 (eq (length old-tchars) 6))
	(alien:with-alien ((tc (alien:struct unix:tchars)))
	  (setf (alien:slot tc 'unix:t-intrc) (svref old-tchars 0))
	  (setf (alien:slot tc 'unix:t-quitc) (svref old-tchars 1))
	  (setf (alien:slot tc 'unix:t-startc) (svref old-tchars 2))
	  (setf (alien:slot tc 'unix:t-stopc) (svref old-tchars 3))
	  (setf (alien:slot tc 'unix:t-eofc) (svref old-tchars 4))
	  (setf (alien:slot tc 'unix:t-brkc) (svref old-tchars 5))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-ioctl fd unix:TIOCSETC (alien:alien-sap tc))
	    (unless val
	      (error "Failed to set tchars, unix error ~S."
		     (unix:get-unix-error-msg err))))))
      #-glibc2
      (when (and (boundp 'old-ltchars)
		 (simple-vector-p old-ltchars)
		 (eq (length old-ltchars) 6))
	(alien:with-alien ((tc (alien:struct unix:ltchars)))
	  (setf (alien:slot tc 'unix:t-suspc) (svref old-ltchars 0))
	  (setf (alien:slot tc 'unix:t-dsuspc) (svref old-ltchars 1))
	  (setf (alien:slot tc 'unix:t-rprntc) (svref old-ltchars 2))
	  (setf (alien:slot tc 'unix:t-flushc) (svref old-ltchars 3))
	  (setf (alien:slot tc 'unix:t-werasc) (svref old-ltchars 4))
	  (setf (alien:slot tc 'unix:t-lnextc) (svref old-ltchars 5))
	  (multiple-value-bind
	      (val err)
	      (unix:unix-ioctl fd unix:TIOCSLTC (alien:alien-sap tc))
	    (unless val
	      (error "Failed to set ltchars, unix error ~S."
		     (unix:get-unix-error-msg err)))))))))

(defun pause-hemlock ()
  "Pause hemlock and pop out to the Unix Shell."
  (system:without-hemlock
   (unix:unix-kill (unix:unix-getpid) :sigstop))
  T)


(provide :hemlock)
