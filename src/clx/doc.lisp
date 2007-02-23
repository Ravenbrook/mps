;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; Copyright 1987, 1988 Massachusetts Institute of Technology, and
;;;			 Texas Instruments Incorporated

;;; Permission to use, copy, modify, and distribute this document for any purpose
;;; and without fee is hereby granted, provided that the above copyright notice
;;; appear in all copies and that both that copyright notice and this permission
;;; notice are retained, and that the name of M.I.T. not be used in advertising or
;;; publicity pertaining to this document without specific, written prior
;;; permission.  M.I.T. makes no representations about the suitability of this
;;; document or the protocol defined in this document for any purpose.  It is
;;; provided "as is" without express or implied warranty.

;;; Texas Instruments Incorporated provides this document "as is" without
;;; express or implied warranty.
#+cmu
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/clx/doc.lisp,v 1.4 1998/12/19 15:21:16 dtc Exp $")

;; Version 4

;; This is considered a somewhat changeable interface.  Discussion of better
;; integration with CLOS, support for user-specified subclassess of basic
;; objects, and the additional functionality to match the C Xlib is still in
;; progress.

;; Primary Interface Author:
;;	Robert W. Scheifler
;;	MIT Laboratory for Computer Science
;;	545 Technology Square, Room 418
;;	Cambridge, MA 02139
;;	rws@zermatt.lcs.mit.edu

;; Design Contributors:
;;	Dan Cerys, Texas Instruments
;;	Scott Fahlman, CMU
;;      Charles Hornig, Symbolics
;;      John Irwin, Franz
;;	Kerry Kimbrough, Texas Instruments
;;	Chris Lindblad, MIT
;;	Rob MacLachlan, CMU
;;	Mike McMahon, Symbolics
;;	David Moon, Symbolics
;;	LaMott Oren, Texas Instruments
;;	Daniel Weinreb, Symbolics
;;	John Wroclawski, MIT
;;	Richard Zippel, Symbolics

;; CLX Extensions
;; Adds some of the functionality provided by the C XLIB library.
;;
;; Primary Author
;;	LaMott G. Oren
;;	Texas Instruments
;; 
;; Design Contributors:
;;	Robert W. Scheifler, MIT


;; Note: all of the following is in the package XLIB.

(declaim (declaration arglist clx-values))

;; Note: if you have read the Version 11 protocol document or C Xlib manual, most of
;; the relationships should be fairly obvious.  We have no intention of writing yet
;; another moby document for this interface.

(deftype card32 () '(unsigned-byte 32))

(deftype card29 () '(unsigned-byte 29))

(deftype int32 () '(signed-byte 32))

(deftype card16 () '(unsigned-byte 16))

(deftype int16 () '(signed-byte 16))

(deftype card8 () '(unsigned-byte 8))

(deftype int8 () '(signed-byte 8))

(deftype mask32 () 'card32)

(deftype mask16 () 'card16)

(deftype resource-id () 'card29)

;; Types employed: display, window, pixmap, cursor, font, gcontext, colormap, color.
;; These types are defined solely by a functional interface; we do not specify
;; whether they are implemented as structures or flavors or ...  Although functions
;; below are written using DEFUN, this is not an implementation requirement (although
;; it is a requirement that they be functions as opposed to macros or special forms).
;; It is unclear whether with-slots in the Common Lisp Object System must work on
;; them.

;; Windows, pixmaps, cursors, fonts, gcontexts, and colormaps are all represented as
;; compound objects, rather than as integer resource-ids.  This allows applications
;; to deal with multiple displays without having an explicit display argument in the
;; most common functions.  Every function uses the display object indicated by the
;; first argument that is or contains a display; it is an error if arguments contain
;; different displays, and predictable results are not guaranteed.

;; Each of window, pixmap, drawable, cursor, font, gcontext, and colormap have the
;; following five functions:

(defun <mumble>-display (<mumble>)
  (declare (type <mumble> <mumble>)
	   (clx-values display)))

(defun <mumble>-id (<mumble>)
  (declare (type <mumble> <mumble>)
	   (clx-values resource-id)))

(defun <mumble>-equal (<mumble>-1 <mumble>-2)
  (declare (type <mumble> <mumble>-1 <mumble>-2)))

(defun <mumble>-p (<mumble>)
  (declare (type <mumble> <mumble>)
	   (clx-values boolean)))

;; The following functions are provided by color objects:

;; The intention is that IHS and YIQ and CYM interfaces will also exist.  Note that
;; we are explicitly using a different spectrum representation than what is actually
;; transmitted in the protocol.

(deftype rgb-val () '(real 0 1))

(defun make-color (&key red green blue &allow-other-keys)	; for expansion
  (declare (type rgb-val red green blue)
	   (clx-values color)))

(defun color-rgb (color)
  (declare (type color color)
	   (clx-values red green blue)))

(defun color-red (color)
  ;; setf'able
  (declare (type color color)
	   (clx-values rgb-val)))

(defun color-green (color)
  ;; setf'able
  (declare (type color color)
	   (clx-values rgb-val)))

(defun color-blue (color)
  ;; setf'able
  (declare (type color color)
	   (clx-values rgb-val)))

(deftype drawable () '(or window pixmap))

;; Atoms are accepted as strings or symbols, and are always returned as keywords.
;; Protocol-level integer atom ids are hidden, using a cache in the display object.

(deftype xatom () '(or string symbol))

(deftype stringable () '(or string symbol))

(deftype fontable () '(or stringable font))

;; Nil stands for CurrentTime.

(deftype timestamp () '(or null card32))

(deftype bit-gravity () '(member :forget :static :north-west :north :north-east
				 :west :center :east :south-west :south :south-east))

(deftype win-gravity () '(member :unmap :static :north-west :north :north-east
				 :west :center :east :south-west :south :south-east))

(deftype grab-status ()
  '(member :success :already-grabbed :frozen :invalid-time :not-viewable))

(deftype boolean () '(or null (not null)))

(deftype pixel () '(unsigned-byte 32))
(deftype image-depth () '(integer 0 32))

(deftype keysym () 'card32)

(deftype array-index () `(integer 0 ,array-dimension-limit))

;; An association list.

(deftype alist (key-type-and-name datum-type-and-name) 'list)

(deftype clx-list (&optional element-type) 'list)
(deftype clx-sequence (&optional element-type) 'sequence)

;; A sequence, containing zero or more repetitions of the given elements,
;; with the elements expressed as (type name).

(deftype repeat-seq (&rest elts) 'sequence)

(deftype point-seq () '(repeat-seq (int16 x) (int16 y)))

(deftype seg-seq () '(repeat-seq (int16 x1) (int16 y1) (int16 x2) (int16 y2)))

(deftype rect-seq () '(repeat-seq (int16 x) (int16 y) (card16 width) (card16 height)))

;; Note that we are explicitly using a different angle representation than what
;; is actually transmitted in the protocol.

(deftype angle () '(real #.(* -2 pi) #.(* 2 pi)))

(deftype arc-seq () '(repeat-seq (int16 x) (int16 y) (card16 width) (card16 height)
				 (angle angle1) (angle angle2)))

(deftype event-mask-class ()
  '(member :key-press :key-release :owner-grab-button :button-press :button-release
	   :enter-window :leave-window :pointer-motion :pointer-motion-hint
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion :exposure :visibility-change
	   :structure-notify :resize-redirect :substructure-notify :substructure-redirect
	   :focus-change :property-change :colormap-change :keymap-state))

(deftype event-mask ()
  '(or mask32 (clx-list event-mask-class)))

(deftype pointer-event-mask-class ()
  '(member :button-press :button-release
	   :enter-window :leave-window :pointer-motion :pointer-motion-hint
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion :keymap-state))

(deftype pointer-event-mask ()
  '(or mask32 (clx-list pointer-event-mask-class)))

(deftype device-event-mask-class ()
  '(member :key-press :key-release :button-press :button-release :pointer-motion
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion))

(deftype device-event-mask ()
  '(or mask32 (clx-list device-event-mask-class)))

(deftype modifier-key ()
  '(member :shift :lock :control :mod-1 :mod-2 :mod-3 :mod-4 :mod-5))

(deftype modifier-mask ()
  '(or (member :any) mask16 (clx-list modifier-key)))

(deftype state-mask-key ()
  '(or modifier-key (member :button-1 :button-2 :button-3 :button-4 :button-5)))

(deftype gcontext-key ()
  '(member :function :plane-mask :foreground :background
	   :line-width :line-style :cap-style :join-style :fill-style :fill-rule
	   :arc-mode :tile :stipple :ts-x :ts-y :font :subwindow-mode
	   :exposures :clip-x :clip-y :clip-mask :dash-offset :dashes))

(deftype event-key ()
  '(member :key-press :key-release :button-press :button-release :motion-notify
	   :enter-notify :leave-notify :focus-in :focus-out :keymap-notify
	   :exposure :graphics-exposure :no-exposure :visibility-notify
	   :create-notify :destroy-notify :unmap-notify :map-notify :map-request
	   :reparent-notify :configure-notify :gravity-notify :resize-request
	   :configure-request :circulate-notify :circulate-request :property-notify
	   :selection-clear :selection-request :selection-notify
	   :colormap-notify :client-message))

(deftype error-key ()
  '(member :access :alloc :atom :colormap :cursor :drawable :font :gcontext :id-choice
	   :illegal-request :implementation :length :match :name :pixmap :value :window))

(deftype draw-direction ()
  '(member :left-to-right :right-to-left))

(defstruct bitmap-format
  (unit <unspec> :type (member 8 16 32))
  (pad <unspec> :type (member 8 16 32))
  (lsb-first-p <unspec> :type boolean))

(defstruct pixmap-format
  (depth <unspec> :type image-depth)
  (bits-per-pixel <unspec> :type (member 1 4 8 16 24 32))
  (pad <unspec> :type (member 8 16 32)))

(defstruct visual-info
  (id <unspec> :type resource-id)
  (display <unspec> :type display)
  (class <unspec> :type (member :static-gray :static-color :true-color
				:gray-scale :pseudo-color :direct-color))
  (red-mask <unspec> :type pixel)
  (green-mask <unspec> :type pixel)
  (blue-mask <unspec> :type pixel)
  (bits-per-rgb <unspec> :type card8)
  (colormap-entries <unspec> :type card16))

(defstruct screen
  (root <unspec> :type window)
  (width <unspec> :type card16)
  (height <unspec> :type card16)
  (width-in-millimeters <unspec> :type card16)
  (height-in-millimeters <unspec> :type card16)
  (depths <unspec> :type (alist (image-depth depth) ((clx-list visual-info) visuals)))
  (root-depth <unspec> :type image-depth)
  (root-visual-info <unspec> :type visual-info)
  (default-colormap <unspec> :type colormap)
  (white-pixel <unspec> :type pixel)
  (black-pixel <unspec> :type pixel)
  (min-installed-maps <unspec> :type card16)
  (max-installed-maps <unspec> :type card16)
  (backing-stores <unspec> :type (member :never :when-mapped :always))
  (save-unders-p <unspec> :type boolean)
  (event-mask-at-open <unspec> :type mask32))

(defun screen-root-visual (screen)
  (declare (type screen screen)
	   (clx-values resource-id)))

;; The list contains alternating keywords and integers.

(deftype font-props () 'list)

(defun open-display (host &key (display 0) protocol)
  ;; A string must be acceptable as a host, but otherwise the possible types for host
  ;; and protocol are not constrained, and will likely be very system dependent.  The
  ;; default protocol is system specific.  Authorization, if any, is assumed to come
  ;; from the environment somehow.
  (declare (type integer display)
	   (clx-values display)))

(defun display-protocol-major-version (display)
  (declare (type display display)
	   (clx-values card16)))

(defun display-protocol-minor-version (display)
  (declare (type display display)
	   (clx-values card16)))

(defun display-vendor-name (display)
  (declare (type display display)
	   (clx-values string)))

(defun display-release-number (display)
  (declare (type display display)
	   (clx-values card32)))

(defun display-image-lsb-first-p (display)
  (declare (type display display)
	   (clx-values boolean)))

(defun display-bitmap-formap (display)
  (declare (type display display)
	   (clx-values bitmap-format)))

(defun display-pixmap-formats (display)
  (declare (type display display)
	   (clx-values (clx-list pixmap-formats))))

(defun display-roots (display)
  (declare (type display display)
	   (clx-values (clx-list screen))))

(defun display-motion-buffer-size (display)
  (declare (type display display)
	   (clx-values card32)))

(defun display-max-request-length (display)
  (declare (type display display)
	   (clx-values card16)))

(defun display-min-keycode (display)
  (declare (type display display)
	   (clx-values card8)))

(defun display-max-keycode (display)
  (declare (type display display)
	   (clx-values card8)))

(defun close-display (display)
  (declare (type display display)))

(defun display-error-handler (display)
  (declare (type display display)
	   (clx-values handler)))

(defsetf display-error-handler (display) (handler)
  ;; All errors (synchronous and asynchronous) are processed by calling an error
  ;; handler in the display.  If handler is a sequence it is expected to contain
  ;; handler functions specific to each error; the error code is used to index the
  ;; sequence, fetching the appropriate handler.  Any results returned by the handler
  ;; are ignored; it is assumed the handler either takes care of the error
  ;; completely, or else signals. For all core errors, the keyword/value argument
  ;; pairs are:
  ;;    :major card8
  ;;    :minor card16
  ;;    :sequence card16
  ;;    :current-sequence card16
  ;;	:asynchronous (member t nil)
  ;; For :colormap, :cursor, :drawable, :font, :gcontext, :id-choice, :pixmap, and
  ;; :window errors another pair is:
  ;;    :resource-id card32
  ;; For :atom errors, another pair is:
  ;;    :atom-id card32
  ;; For :value errors, another pair is:
  ;;    :value card32
  (declare (type display display)
	   (type (or (clx-sequence (function (display symbol &key &allow-other-keys)))
		     (function (display symbol &key &allow-other-keys)))
		 handler)))

(defsetf display-report-asynchronous-errors (display) (when)
  ;; Most useful in multi-process lisps.
  ;;
  ;; Synchronous errors are always signalled in the process that made the
  ;; synchronous request.  An error is considered synchronous if a process is
  ;; waiting for a reply with the same request-id as the error.
  ;; 
  ;; Asynchronous errors can be signalled at any one of these three times:
  ;; 
  ;; 1.  As soon as they are read.  They get signalled in whichever process 
  ;; was doing the reading.  This is enabled by
  ;;       (setf (xlib:display-report-asynchronous-errors display)
  ;;             '(:immediately))
  ;; This is the default.
  ;; 
  ;; 2.  Before any events are to be handled.  You get these by doing an
  ;; event-listen with any timeout value other than 0, or in of the event
  ;; processing forms.  This is useful if you using a background process to
  ;; handle input.  This is enabled by
  ;;       (setf (xlib:display-report-asynchronous-errors display)
  ;;             '(:before-event-handling)) 
  ;; 
  ;; 3.  After a display-finish-output.  You get these by doing a
  ;; display-finish-output.  A cliche using this might have a with-display
  ;; wrapped around the display operations that possibly cause an asynchronous
  ;; error, with a display-finish-output right the end of the with-display to
  ;; catch any asynchronous errors.  This is enabled by
  ;;       (setf (xlib:display-report-asynchronous-errors display)
  ;;             '(:after-finish-output))
  ;; 
  ;; You can select any combination of the three keywords.  For example, to
  ;; get errors reported before event handling and after finish-output,
  ;;       (setf (xlib:display-report-asynchronous-errors display)
  ;;             '(:before-event-handling :after-finish-output))
  (declare (type list when))
  )

(defmacro define-condition (name base &body items)
  ;; just a place-holder here for the real thing
  )

(define-condition request-error error
  display
  major
  minor
  sequence
  current-sequence
  asynchronous)

(defun default-error-handler (display error-key &key &allow-other-keys)
  ;; The default display-error-handler.
  ;; It signals the conditions listed below.
  (declare (type display display)
	   (type symbol error-key))
  )

(define-condition resource-error request-error
  resource-id)

(define-condition access-error request-error)

(define-condition alloc-error request-error)

(define-condition atom-error request-error
  atom-id)

(define-condition colormap-error resource-error)

(define-condition cursor-error resource-error)

(define-condition drawable-error resource-error)

(define-condition font-error resource-error)

(define-condition gcontext-error resource-error)

(define-condition id-choice-error resource-error)

(define-condition illegal-request-error request-error)

(define-condition implementation-error request-error)

(define-condition length-error request-error)

(define-condition match-error request-error)

(define-condition name-error request-error)

(define-condition pixmap-error resource-error)

(define-condition value-error request-error
  value)

(define-condition window-error resource-error)

(defmacro with-display ((display) &body body)
  ;; This macro is for use in a multi-process environment.  It provides exclusive
  ;; access to the local display object for multiple request generation.  It need not
  ;; provide immediate exclusive access for replies; that is, if another process is
  ;; waiting for a reply (while not in a with-display), then synchronization need not
  ;; (but can) occur immediately.  Except where noted, all routines effectively
  ;; contain an implicit with-display where needed, so that correct synchronization
  ;; is always provided at the interface level on a per-call basis.  Nested uses of
  ;; this macro will work correctly.  This macro does not prevent concurrent event
  ;; processing; see with-event-queue.
  )

(defun display-force-output (display)
  ;; Output is normally buffered; this forces any buffered output.
  (declare (type display display)))

(defun display-finish-output (display)
  ;; Forces output, then causes a round-trip to ensure that all possible errors and
  ;; events have been received.
  (declare (type display display)))

(defun display-after-function (display)
  ;; setf'able
  ;; If defined, called after every protocol request is generated, even those inside
  ;; explicit with-display's, but never called from inside the after-function itself.
  ;; The function is called inside the effective with-display for the associated
  ;; request.  Default value is nil.  Can be set, for example, to
  ;; #'display-force-output or #'display-finish-output.
  (declare (type display display)
	   (clx-values (or null (function (display))))))

(defun create-window (&key parent x y width height (depth 0) (border-width 0)
		      (class :copy) (visual :copy)
		      background border gravity bit-gravity
		      backing-store backing-planes backing-pixel save-under
		      event-mask do-not-propagate-mask override-redirect
		      colormap cursor)
  ;; Display is obtained from parent.  Only non-nil attributes are passed on in the
  ;; request: the function makes no assumptions about what the actual protocol
  ;; defaults are.  Width and height are the inside size, excluding border.
  (declare (type window parent)
	   (type int16 x y)
	   (type card16 width height depth border-width)
	   (type (member :copy :input-output :input-only) class)
	   (type (or (member :copy) visual-info) visual)
	   (type (or null (member :none :parent-relative) pixel pixmap) background)
	   (type (or null (member :copy) pixel pixmap) border)
	   (type (or null win-gravity) gravity)
	   (type (or null bit-gravity) bit-gravity)
	   (type (or null (member :not-useful :when-mapped :always) backing-store))
	   (type (or null pixel) backing-planes backing-pixel)
	   (type (or null event-mask) event-mask)
	   (type (or null device-event-mask) do-not-propagate-mask)
	   (type (or null (member :on :off)) save-under override-redirect)
	   (type (or null (member :copy) colormap) colormap)
	   (type (or null (member :none) cursor) cursor)
	   (clx-values window)))

(defun window-class (window)
  (declare (type window window)
	   (clx-values (member :input-output :input-only))))

(defun window-visual-info (window)
  (declare (type window window)
	   (clx-values visual-info)))

(defun window-visual (window)
  (declare (type window window)
	   (clx-values resource-id)))

(defsetf window-background (window) (background)
  (declare (type window window)
	   (type (or (member :none :parent-relative) pixel pixmap) background)))

(defsetf window-border (window) (border)
  (declare (type window window)
	   (type (or (member :copy) pixel pixmap) border)))

(defun window-gravity (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values win-gravity)))

(defun window-bit-gravity (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values bit-gravity)))

(defun window-backing-store (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values (member :not-useful :when-mapped :always))))

(defun window-backing-planes (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values pixel)))

(defun window-backing-pixel (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values pixel)))

(defun window-save-under (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values (member :on :off))))

(defun window-event-mask (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values mask32)))

(defun window-do-not-propagate-mask (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values mask32)))

(defun window-override-redirect (window)
  ;; setf'able
  (declare (type window window)
	   (clx-values (member :on :off))))

(defun window-colormap (window)
  (declare (type window window)
	   (clx-values (or null colormap))))

(defsetf window-colormap (window) (colormap)
  (declare (type window window)
	   (type (or (member :copy) colormap) colormap)))

(defsetf window-cursor (window) (cursor)
  (declare (type window window)
	   (type (or (member :none) cursor) cursor)))

(defun window-colormap-installed-p (window)
  (declare (type window window)
	   (clx-values boolean)))

(defun window-all-event-masks (window)
  (declare (type window window)
	   (clx-values mask32)))

(defun window-map-state (window)
  (declare (type window window)
	   (clx-values (member :unmapped :unviewable :viewable))))

(defsetf drawable-x (window) (x)
  (declare (type window window)
	   (type int16 x)))

(defsetf drawable-y (window) (y)
  (declare (type window window)
	   (type int16 y)))

(defsetf drawable-width (window) (width)
  ;; Inside width, excluding border.
  (declare (type window window)
	   (type card16 width)))

(defsetf drawable-height (window) (height)
  ;; Inside height, excluding border.
  (declare (type window window)
	   (type card16 height)))

(defsetf drawable-border-width (window) (border-width)
  (declare (type window window)
	   (type card16 border-width)))

(defsetf window-priority (window &optional sibling) (mode)
  ;; A bit strange, but retains setf form.
  (declare (type window window)
	   (type (or null window) sibling)
	   (type (member :above :below :top-if :bottom-if :opposite) mode)))

(defmacro with-state ((drawable) &body body)
  ;; Allows a consistent view to be obtained of data returned by GetWindowAttributes
  ;; and GetGeometry, and allows a coherent update using ChangeWindowAttributes and
  ;; ConfigureWindow.  The body is not surrounded by a with-display.  Within the
  ;; indefinite scope of the body, on a per-process basis in a multi-process
  ;; environment, the first call within an Accessor Group on the specified drawable
  ;; (the object, not just the variable) causes the complete results of the protocol
  ;; request to be retained, and returned in any subsequent accessor calls.  Calls
  ;; within a Setf Group are delayed, and executed in a single request on exit from
  ;; the body.  In addition, if a call on a function within an Accessor Group follows
  ;; a call on a function in the corresponding Setf Group, then all delayed setfs for
  ;; that group are executed, any retained accessor information for that group is
  ;; discarded, the corresponding protocol request is (re)issued, and the results are
  ;; (again) retained, and returned in any subsequent accessor calls.

  ;; Accessor Group A (for GetWindowAttributes):
  ;; window-visual-info, window-visual, window-class, window-gravity, window-bit-gravity,
  ;; window-backing-store, window-backing-planes, window-backing-pixel,
  ;; window-save-under, window-colormap, window-colormap-installed-p,
  ;; window-map-state, window-all-event-masks, window-event-mask,
  ;; window-do-not-propagate-mask, window-override-redirect

  ;; Setf Group A (for ChangeWindowAttributes):
  ;; window-gravity, window-bit-gravity, window-backing-store, window-backing-planes,
  ;; window-backing-pixel, window-save-under, window-event-mask,
  ;; window-do-not-propagate-mask, window-override-redirect, window-colormap,
  ;; window-cursor

  ;; Accessor Group G (for GetGeometry):
  ;; drawable-root, drawable-depth, drawable-x, drawable-y, drawable-width,
  ;; drawable-height, drawable-border-width

  ;; Setf Group G (for ConfigureWindow):
  ;; drawable-x, drawable-y, drawable-width, drawable-height, drawable-border-width,
  ;; window-priority
  )

(defun destroy-window (window)
  (declare (type window window)))

(defun destroy-subwindows (window)
  (declare (type window window)))

(defun add-to-save-set (window)
  (declare (type window window)))

(defun remove-from-save-set (window)
  (declare (type window window)))

(defun reparent-window (window parent x y)
  (declare (type window window parent)
	   (type int16 x y)))

(defun map-window (window)
  (declare (type window window)))

(defun map-subwindows (window)
  (declare (type window window)))

(defun unmap-window (window)
  (declare (type window window)))

(defun unmap-subwindows (window)
  (declare (type window window)))

(defun circulate-window-up (window)
  (declare (type window window)))

(defun circulate-window-down (window)
  (declare (type window window)))

(defun drawable-root (drawable)
  (declare (type drawable drawable)
	   (clx-values window)))

(defun drawable-depth (drawable)
  (declare (type drawable drawable)
	   (clx-values card8)))

(defun drawable-x (drawable)
  (declare (type drawable drawable)
	   (clx-values int16)))

(defun drawable-y (drawable)
  (declare (type drawable drawable)
	   (clx-values int16)))

(defun drawable-width (drawable)
  ;; For windows, inside width, excluding border.
  (declare (type drawable drawable)
	   (clx-values card16)))

(defun drawable-height (drawable)
  ;; For windows, inside height, excluding border.
  (declare (type drawable drawable)
	   (clx-values card16)))

(defun drawable-border-width (drawable)
  (declare (type drawable drawable)
	   (clx-values card16)))

(defun query-tree (window &key (result-type 'list))
  (declare (type window window)
	   (type type result-type)
	   (clx-values (clx-sequence window) parent root)))

(defun change-property (window property data type format
			&key (mode :replace) (start 0) end transform)
  ;; Start and end affect sub-sequence extracted from data.
  ;; Transform is applied to each extracted element.
  (declare (type window window)
	   (type xatom property type)
	   (type (member 8 16 32) format)
	   (type sequence data)
	   (type (member :replace :prepend :append) mode)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type (or null (function (t) integer)) transform)))

(defun delete-property (window property)
  (declare (type window window)
	   (type xatom property)))

(defun get-property (window property
		     &key type (start 0) end delete-p (result-type 'list) transform)
  ;; Transform is applied to each integer retrieved.
  ;; Nil is returned for type when the protocol returns None.
  (declare (type window window)
	   (type xatom property)
	   (type (or null xatom) type)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type boolean delete-p)
	   (type type result-type)
	   (type (or null (function (integer) t)) transform)
	   (clx-values data type format bytes-after)))

(defun rotate-properties (window properties &optional (delta 1))
  ;; Postive rotates left, negative rotates right (opposite of actual protocol request).
  (declare (type window window)
	   (type (clx-sequence xatom) properties)
	   (type int16 delta)))

(defun list-properties (window &key (result-type 'list))
  (declare (type window window)
	   (type type result-type)
	   (clx-values (clx-sequence keyword))))

;; Although atom-ids are not visible in the normal user interface, atom-ids might
;; appear in window properties and other user data, so conversion hooks are needed.

(defun intern-atom (display name)
  (declare (type display display)
	   (type xatom name)
	   (clx-values resource-id)))

(defun find-atom (display name)
  (declare (type display display)
	   (type xatom name)
	   (clx-values (or null resource-id))))

(defun atom-name (display atom-id)
  (declare (type display display)
	   (type resource-id atom-id)
	   (clx-values keyword)))

(defun selection-owner (display selection)
  (declare (type display display)
	   (type xatom selection)
	   (clx-values (or null window))))

(defsetf selection-owner (display selection &optional time) (owner)
  ;; A bit strange, but retains setf form.
  (declare (type display display)
	   (type xatom selection)
	   (type (or null window) owner)
	   (type timestamp time)))

(defun convert-selection (selection type requestor &optional property time)
  (declare (type xatom selection type)
	   (type window requestor)
	   (type (or null xatom) property)
	   (type timestamp time)))

(defun send-event (window event-key event-mask &rest args
		   &key propagate-p display &allow-other-keys)
  ;; Additional arguments depend on event-key, and are as specified further below
  ;; with declare-event, except that both resource-ids and resource objects are
  ;; accepted in the event components.  The display argument is only required if the
  ;; window is :pointer-window or :input-focus.  If an argument has synonyms, it is
  ;; only necessary to supply a value for one of them; it is an error to specify
  ;; different values for synonyms.
  (declare (type (or window (member :pointer-window :input-focus)) window)
	   (type (or null event-key) event-key)
	   (type event-mask event-mask)
	   (type boolean propagate-p)
	   (type (or null display) display)))

(defun grab-pointer (window event-mask
		     &key owner-p sync-pointer-p sync-keyboard-p confine-to cursor time)
  (declare (type window window)
	   (type pointer-event-mask event-mask)
	   (type boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or null window) confine-to)
	   (type (or null cursor) cursor)
	   (type timestamp time)
	   (clx-values grab-status)))

(defun ungrab-pointer (display &key time)
  (declare (type display display)
	   (type timestamp time)))

(defun grab-button (window button event-mask
		    &key (modifiers 0)
			 owner-p sync-pointer-p sync-keyboard-p confine-to cursor)
  (declare (type window window)
	   (type (or (member :any) card8) button)
	   (type modifier-mask modifiers)
	   (type pointer-event-mask event-mask)
	   (type boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or null window) confine-to)
	   (type (or null cursor) cursor)))

(defun ungrab-button (window button &key (modifiers 0))
  (declare (type window window)
	   (type (or (member :any) card8) button)
	   (type modifier-mask modifiers)))

(defun change-active-pointer-grab (display event-mask &optional cursor time)
  (declare (type display display)
	   (type pointer-event-mask event-mask)
	   (type (or null cursor) cursor)
	   (type timestamp time)))

(defun grab-keyboard (window &key owner-p sync-pointer-p sync-keyboard-p time)
  (declare (type window window)
	   (type boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type timestamp time)
	   (clx-values grab-status)))

(defun ungrab-keyboard (display &key time)
  (declare (type display display)
	   (type timestamp time)))

(defun grab-key (window key &key (modifiers 0) owner-p sync-pointer-p sync-keyboard-p)
  (declare (type window window)
	   (type boolean owner-p sync-pointer-p sync-keyboard-p)
	   (type (or (member :any) card8) key)
	   (type modifier-mask modifiers)))

(defun ungrab-key (window key &key (modifiers 0))
  (declare (type window window)
	   (type (or (member :any) card8) key)
	   (type modifier-mask modifiers)))

(defun allow-events (display mode &optional time)
  (declare (type display display)
	   (type (member :async-pointer :sync-pointer :reply-pointer
			 :async-keyboard :sync-keyboard :replay-keyboard
			 :async-both :sync-both)
		 mode)
	   (type timestamp time)))

(defun grab-server (display)
  (declare (type display display)))

(defun ungrab-server (display)
  (declare (type display display)))

(defmacro with-server-grabbed ((display) &body body)
  ;; The body is not surrounded by a with-display.
  )

(defun query-pointer (window)
  (declare (type window window)
	   (clx-values x y same-screen-p child mask root-x root-y root)))

(defun pointer-position (window)
  (declare (type window window)
	   (clx-values x y same-screen-p)))

(defun global-pointer-position (display)
  (declare (type display display)
	   (clx-values root-x root-y root)))

(defun motion-events (window &key start stop (result-type 'list))
  (declare (type window window)
	   (type timestamp start stop)
	   (type type result-type)
	   (clx-values (repeat-seq (int16 x) (int16 y) (timestamp time)))))

(defun translate-coordinates (src src-x src-y dst)
  ;; If src and dst are not on the same screen, nil is returned.
  (declare (type window src)
	   (type int16 src-x src-y)
	   (type window dst)
	   (clx-values dst-x dst-y child)))

(defun warp-pointer (dst dst-x dst-y)
  (declare (type window dst)
	   (type int16 dst-x dst-y)))

(defun warp-pointer-relative (display x-off y-off)
  (declare (type display display)
	   (type int16 x-off y-off)))

(defun warp-pointer-if-inside (dst dst-x dst-y src src-x src-y
			       &optional src-width src-height)
  ;; Passing in a zero src-width or src-height is a no-op.  A null src-width or
  ;; src-height translates into a zero value in the protocol request.
  (declare (type window dst src)
	   (type int16 dst-x dst-y src-x src-y)
	   (type (or null card16) src-width src-height)))

(defun warp-pointer-relative-if-inside (x-off y-off src src-x src-y
					&optional src-width src-height)
  ;; Passing in a zero src-width or src-height is a no-op.  A null src-width or
  ;; src-height translates into a zero value in the protocol request.
  (declare (type window src)
	   (type int16 x-off y-off src-x src-y)
	   (type (or null card16) src-width src-height)))

(defun set-input-focus (display focus revert-to &optional time)
  ;; Setf ought to allow multiple values.
  (declare (type display display)
	   (type (or (member :none :pointer-root) window) focus)
	   (type (member :none :parent :pointer-root) revert-to)
	   (type timestamp time)))

(defun input-focus (display)
  (declare (type display display)
	   (clx-values focus revert-to)))

(defun query-keymap (display)
  (declare (type display display)
	   (clx-values (bit-vector 256))))

(defun open-font (display name)
  ;; Font objects may be cached and reference counted locally within the display
  ;; object.  This function might not execute a with-display if the font is cached.
  ;; The protocol QueryFont request happens on-demand under the covers.
  (declare (type display display)
	   (type stringable name)
	   (clx-values font)))

;; We probably want a per-font bit to indicate whether caching on
;; text-extents/width calls is desirable.  But what to name it?

(defun discard-font-info (font)
  ;; Discards any state that can be re-obtained with QueryFont.  This is simply
  ;; a performance hint for memory-limited systems.
  (declare (type font font)))

;; This can be signalled anywhere a pseudo font access fails.

(define-condition invalid-font error
  font)

;; Note: font-font-info removed.

(defun font-name (font)
  ;; Returns nil for a pseudo font returned by gcontext-font.
  (declare (type font font)
	   (clx-values (or null string))))

(defun font-direction (font)
  (declare (type font font)
	   (clx-values draw-direction)))

(defun font-min-char (font)
  (declare (type font font)
	   (clx-values card16)))

(defun font-max-char (font)
  (declare (type font font)
	   (clx-values card16)))

(defun font-min-byte1 (font)
  (declare (type font font)
	   (clx-values card8)))

(defun font-max-byte1 (font)
  (declare (type font font)
	   (clx-values card8)))

(defun font-min-byte2 (font)
  (declare (type font font)
	   (clx-values card8)))

(defun font-max-byte2 (font)
  (declare (type font font)
	   (clx-values card8)))

(defun font-all-chars-exist-p (font)
  (declare (type font font)
	   (clx-values boolean)))

(defun font-default-char (font)
  (declare (type font font)
	   (clx-values card16)))

(defun font-ascent (font)
  (declare (type font font)
	   (clx-values int16)))

(defun font-descent (font)
  (declare (type font font)
	   (clx-values int16)))

;; The list contains alternating keywords and int32s.

(deftype font-props () 'list)

(defun font-properties (font)
  (declare (type font font)
	   (clx-values font-props)))

(defun font-property (font name)
  (declare (type font font)
	   (type keyword name)
	   (clx-values (or null int32))))

;; For each of left-bearing, right-bearing, width, ascent, descent, attributes:

(defun char-<metric> (font index)
  ;; Note: I have tentatively chosen to return nil for an out-of-bounds index
  ;; (or an in-bounds index on a pseudo font), although returning zero or
  ;; signalling might be better.
  (declare (type font font)
	   (type card16 index)
	   (clx-values (or null int16))))

(defun max-char-<metric> (font)
  ;; Note: I have tentatively chosen separate accessors over allowing :min and
  ;; :max as an index above.
  (declare (type font font)
	   (clx-values int16)))

(defun min-char-<metric> (font)
  (declare (type font font)
	   (clx-values int16)))

;; Note: char16-<metric> accessors could be defined to accept two-byte indexes.

(defun close-font (font)
  ;; This might not generate a protocol request if the font is reference
  ;; counted locally or if it is a pseudo font.
  (declare (type font font)))

(defun list-font-names (display pattern &key (max-fonts 65535) (result-type 'list))
  (declare (type display display)
	   (type string pattern)
	   (type card16 max-fonts)
	   (type type result-type)
	   (clx-values (clx-sequence string))))

(defun list-fonts (display pattern &key (max-fonts 65535) (result-type 'list))
  ;; Returns "pseudo" fonts that contain basic font metrics and properties, but
  ;; no per-character metrics and no resource-ids.  These pseudo fonts will be
  ;; converted (internally) to real fonts dynamically as needed, by issuing an
  ;; OpenFont request.  However, the OpenFont might fail, in which case the
  ;; invalid-font error can arise.
  (declare (type display display)
	   (type string pattern)
	   (type card16 max-fonts)
	   (type type result-type)
	   (clx-values (clx-sequence font))))

(defun font-path (display &key (result-type 'list))
  (declare (type display display)
	   (type type result-type)
	   (clx-values (clx-sequence (or string pathname)))))

(defsetf font-path (display) (paths)
  (declare (type display display)
	   (type (clx-sequence (or string pathname)) paths)))

(defun create-pixmap (&key width height depth drawable)
  (declare (type card16 width height)
	   (type card8 depth)
	   (type drawable drawable)
	   (clx-values pixmap)))

(defun free-pixmap (pixmap)
  (declare (type pixmap pixmap)))

(defun create-gcontext (&key drawable function plane-mask foreground background
			     line-width line-style cap-style join-style fill-style fill-rule
			     arc-mode tile stipple ts-x ts-y font subwindow-mode
			     exposures clip-x clip-y clip-mask clip-ordering
			     dash-offset dashes
			     (cache-p t))
  ;; Only non-nil components are passed on in the request, but for effective caching
  ;; assumptions have to be made about what the actual protocol defaults are.  For
  ;; all gcontext components, a value of nil causes the default gcontext value to be
  ;; used.  For clip-mask, this implies that an empty rect-seq cannot be represented
  ;; as a list.  Note:  use of stringable as font will cause an implicit open-font.
  ;; Note:  papers over protocol SetClipRectangles and SetDashes special cases.  If
  ;; cache-p is true, then gcontext state is cached locally, and changing a gcontext
  ;; component will have no effect unless the new value differs from the cached
  ;; value.  Component changes (setfs and with-gcontext) are always deferred
  ;; regardless of the cache mode, and sent over the protocol only when required by a
  ;; local operation or by an explicit call to force-gcontext-changes.
  (declare (type drawable drawable)
	   (type (or null boole-constant) function)
	   (type (or null pixel) plane-mask foreground background)
	   (type (or null card16) line-width dash-offset)
	   (type (or null int16) ts-x ts-y clip-x clip-y)
	   (type (or null (member :solid :dash :double-dash)) line-style)
	   (type (or null (member :not-last :butt :round :projecting)) cap-style)
	   (type (or null (member :miter :round :bevel)) join-style)
	   (type (or null (member :solid :tiled :opaque-stippled :stippled)) fill-style)
	   (type (or null (member :even-odd :winding)) fill-rule)
	   (type (or null (member :chord :pie-slice)) arc-mode)
	   (type (or null pixmap) tile stipple)
	   (type (or null fontable) font)
	   (type (or null (member :clip-by-children :include-inferiors)) subwindow-mode)
	   (type (or null (member :on :off)) exposures)
	   (type (or null (member :none) pixmap rect-seq) clip-mask)
	   (type (or null (member :unsorted :y-sorted :yx-sorted :yx-banded)) clip-ordering)
	   (type (or null (or card8 (clx-sequence card8))) dashes)
	   (type boolean cache)
	   (clx-values gcontext)))

;; For each argument to create-gcontext (except font, clip-mask and
;; clip-ordering) declared as (type <type> <name>), there is an accessor:

(defun gcontext-<name> (gcontext)
  ;; The value will be nil if the last value stored is unknown (e.g., the cache was
  ;; off, or the component was copied from a gcontext with unknown state).
  (declare (type gcontext gcontext)
	   (clx-values <type>)))

;; For each argument to create-gcontext (except clip-mask and clip-ordering) declared
;; as (type (or null <type>) <name>), there is a setf for the corresponding accessor:

(defsetf gcontext-<name> (gcontext) (value)
  (declare (type gcontext gcontext)
	   (type <type> value)))

(defun gcontext-font (gcontext &optional metrics-p)
  ;; If the stored font is known, it is returned.  If it is not known and
  ;; metrics-p is false, then nil is returned.  If it is not known and
  ;; metrics-p is true, then a pseudo font is returned.  Full metric and
  ;; property information can be obtained, but the font does not have a name or
  ;; a resource-id, and attempts to use it where a resource-id is required will
  ;; result in an invalid-font error.
  (declare (type gcontext gcontext)
	   (type boolean metrics-p)
	   (clx-values (or null font))))

(defun gcontext-clip-mask (gcontext)
  (declare (type gcontext gcontext)
	   (clx-values (or null (member :none) pixmap rect-seq)
		   (or null (member :unsorted :y-sorted :yx-sorted :yx-banded)))))

(defsetf gcontext-clip-mask (gcontext &optional ordering) (clip-mask)
  ;; Is nil illegal here, or is it transformed to a vector?
  ;; A bit strange, but retains setf form.
  (declare (type gcontext gcontext)
	   (type (or null (member :unsorted :y-sorted :yx-sorted :yx-banded)) clip-ordering)
	   (type (or (member :none) pixmap rect-seq) clip-mask)))

(defun force-gcontext-changes (gcontext)
  ;; Force any delayed changes.
  (declare (type gcontext gcontext)))

(defmacro with-gcontext ((gcontext &key
			  function plane-mask foreground background
			  line-width line-style cap-style join-style fill-style fill-rule
			  arc-mode tile stipple ts-x ts-y font subwindow-mode
			  exposures clip-x clip-y clip-mask clip-ordering
			  dashes dash-offset)
			 &body body)
  ;; Changes gcontext components within the dynamic scope of the body (i.e.,
  ;; indefinite scope and dynamic extent), on a per-process basis in a multi-process
  ;; environment.  The values are all evaluated before bindings are performed.  The
  ;; body is not surrounded by a with-display.  If cache-p is nil or the some
  ;; component states are unknown, this will implement save/restore by creating a
  ;; temporary gcontext and doing gcontext-components to and from it.
  )

(defun copy-gcontext-components (src dst &rest keys)
  (declare (type gcontext src dst)
	   (type (clx-list gcontext-key) keys)))

(defun copy-gcontext (src dst)
  (declare (type gcontext src dst))
  ;; Copies all components.
  )
	   
(defun free-gcontext (gcontext)
  (declare (type gcontext gcontext)))

(defun clear-area (window &key (x 0) (y 0) width height exposures-p)
  ;; Passing in a zero width or height is a no-op.  A null width or height translates
  ;; into a zero value in the protocol request.
  (declare (type window window)
	   (type int16 x y)
	   (type (or null card16) width height)
	   (type boolean exposures-p)))

(defun copy-area (src gcontext src-x src-y width height dst dst-x dst-y)
  (declare (type drawable src dst)
	   (type gcontext gcontext)
	   (type int16 src-x src-y dst-x dst-y)
	   (type card16 width height)))

(defun copy-plane (src gcontext plane src-x src-y width height dst dst-x dst-y)
  (declare (type drawable src dst)
	   (type gcontext gcontext)
	   (type pixel plane)
	   (type int16 src-x src-y dst-x dst-y)
	   (type card16 width height)))

(defun draw-point (drawable gcontext x y)
  ;; Should be clever about appending to existing buffered protocol request, provided
  ;; gcontext has not been modified.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)))

(defun draw-points (drawable gcontext points &optional relative-p)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type point-seq points)
	   (type boolean relative-p)))

(defun draw-line (drawable gcontext x1 y1 x2 y2 &optional relative-p)
  ;; Should be clever about appending to existing buffered protocol request, provided
  ;; gcontext has not been modified.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x1 y1 x2 y2)
	   (type boolean relative-p)))

(defun draw-lines (drawable gcontext points &key relative-p fill-p (shape :complex))
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type point-seq points)
	   (type boolean relative-p fill-p)
	   (type (member :complex :non-convex :convex) shape)))

(defun draw-segments (drawable gcontext segments)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type seg-seq segments)))

(defun draw-rectangle (drawable gcontext x y width height &optional fill-p)
  ;; Should be clever about appending to existing buffered protocol request, provided
  ;; gcontext has not been modified.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type card16 width height)
	   (type boolean fill-p)))

(defun draw-rectangles (drawable gcontext rectangles &optional fill-p)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type rect-seq rectangles)
	   (type boolean fill-p)))

(defun draw-arc (drawable gcontext x y width height angle1 angle2 &optional fill-p)
  ;; Should be clever about appending to existing buffered protocol request, provided
  ;; gcontext has not been modified.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type card16 width height)
	   (type angle angle1 angle2)
	   (type boolean fill-p)))

(defun draw-arcs (drawable gcontext arcs &optional fill-p)
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type arc-seq arcs)
	   (type boolean fill-p)))

;; The following image routines are bare minimum.  It may be useful to define some
;; form of "image" object to hide representation details and format conversions.  It
;; also may be useful to provide stream-oriented interfaces for reading and writing
;; the data.

(defun put-raw-image (drawable gcontext data
		      &key (start 0) depth x y width height (left-pad 0) format)
  ;; Data must be a sequence of 8-bit quantities, already in the appropriate format
  ;; for transmission; the caller is responsible for all byte and bit swapping and
  ;; compaction.  Start is the starting index in data; the end is computed from the
  ;; other arguments.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type (clx-sequence card8) data)
	   (type array-index start)
	   (type card8 depth left-pad)
	   (type int16 x y)
	   (type card16 width height)
	   (type (member :bitmap :xy-pixmap :z-pixmap) format)))

(defun get-raw-image (drawable &key data (start 0) x y width height
				    (plane-mask 0xffffffff) format
				    (result-type '(vector (unsigned-byte 8))))
  ;; If data is given, it is modified in place (and returned), otherwise a new
  ;; sequence is created and returned, with a size computed from the other arguments
  ;; and the returned depth.  The sequence is filled with 8-bit quantities, in
  ;; transmission format; the caller is responsible for any byte and bit swapping and
  ;; compaction required for further local use.
  (declare (type drawable drawable)
	   (type (or null (clx-sequence card8)) data)
	   (type array-index start)
	   (type int16 x y)
	   (type card16 width height)
	   (type pixel plane-mask)
	   (type (member :xy-pixmap :z-pixmap) format)
	   (clx-values (clx-sequence card8) depth visual-info)))

(defun translate-default (src src-start src-end font dst dst-start)
  ;; dst is guaranteed to have room for (- src-end src-start) integer elements,
  ;; starting at dst-start; whether dst holds 8-bit or 16-bit elements depends
  ;; on context.  font is the current font, if known.  The function should
  ;; translate as many elements of src as possible into indexes in the current
  ;; font, and store them into dst.  The first return value should be the src
  ;; index of the first untranslated element.  If no further elements need to
  ;; be translated, the second return value should be nil.  If a horizontal
  ;; motion is required before further translation, the second return value
  ;; should be the delta in x coordinate.  If a font change is required for
  ;; further translation, the second return value should be the new font.  If
  ;; known, the pixel width of the translated text can be returned as the third
  ;; value; this can allow for appending of subsequent output to the same
  ;; protocol request, if no overall width has been specified at the higher
  ;; level.
  (declare (type sequence src)
	   (type array-index src-start src-end dst-start)
	   (type (or null font) font)
	   (type vector dst)
	   (clx-values array-index (or null int16 font) (or null int32))))

;; There is a question below of whether translate should always be required, or
;; if not, what the default should be or where it should come from.  For
;; example, the default could be something that expected a string as src and
;; translated the CL standard character set to ASCII indexes, and ignored fonts
;; and bits.  Or the default could expect a string but otherwise be "system
;; dependent".  Or the default could be something that expected a vector of
;; integers and did no translation.  Or the default could come from the
;; gcontext (but what about text-extents and text-width?).

(defun text-extents (font sequence &key (start 0) end translate)
  ;; If multiple fonts are involved, font-ascent and font-descent will be the
  ;; maximums.  If multiple directions are involved, the direction will be nil.
  ;; Translate will always be called with a 16-bit dst buffer.
  (declare (type sequence sequence)
	   (type (or font gcontext) font)
	   (type translate translate)
	   (clx-values width ascent descent left right font-ascent font-descent direction
		   (or null array-index))))

(defun text-width (font sequence &key (start 0) end translate)
  ;; Translate will always be called with a 16-bit dst buffer.
  (declare (type sequence sequence)
	   (type (or font gcontext) font)
	   (type translate translate)
	   (clx-values int32 (or null array-index))))

;; This controls the element size of the dst buffer given to translate.  If
;; :default is specified, the size will be based on the current font, if known,
;; and otherwise 16 will be used.  [An alternative would be to pass the buffer
;; size to translate, and allow it to return the desired size if it doesn't
;; like the current size.  The problem is that the protocol doesn't allow
;; switching within a single request, so to allow switching would require
;; knowing the width of text, which isn't necessarily known.  We could call
;; text-width to compute it, but perhaps that is doing too many favors?]  [An
;; additional possibility is to allow an index-size of :two-byte, in which case
;; translate would be given a double-length 8-bit array, and translate would be
;; expected to store first-byte/second-byte instead of 16-bit integers.]

(deftype index-size () '(member :default 8 16))

;; In the glyph functions below, if width is specified, it is assumed to be the
;; total pixel width of whatever string of glyphs is actually drawn.
;; Specifying width will allow for appending the output of subsequent calls to
;; the same protocol request, provided gcontext has not been modified in the
;; interim.  If width is not specified, appending of subsequent output might
;; not occur (unless translate returns the width).  Specifying width is simply
;; a hint, for performance.

(defun draw-glyph (drawable gcontext x y elt
		   &key translate width (size :default))
  ;; Returns true if elt is output, nil if translate refuses to output it.
  ;; Second result is width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type translate translate)
	   (type (or null int32) width)
	   (type index-size size)
	   (clx-values boolean (or null int32))))

(defun draw-glyphs (drawable gcontext x y sequence
		    &key (start 0) end translate width (size :default))
  ;; First result is new start, if end was not reached.  Second result is
  ;; overall width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type sequence sequence)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type (or null int32) width)
	   (type translate translate)
	   (type index-size size)
	   (clx-values (or null array-index) (or null int32))))

(defun draw-image-glyph (drawable gcontext x y elt
			 &key translate width (size :default))
  ;; Returns true if elt is output, nil if translate refuses to output it.
  ;; Second result is overall width, if known.  An initial font change is
  ;; allowed from translate.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type translate translate)
	   (type (or null int32) width)
	   (type index-size size)
	   (clx-values boolean (or null int32))))

(defun draw-image-glyphs (drawable gcontext x y sequence
			  &key (start 0) end width translate (size :default))
  ;; An initial font change is allowed from translate, but any subsequent font
  ;; change or horizontal motion will cause termination (because the protocol
  ;; doesn't support chaining).  [Alternatively, font changes could be accepted
  ;; as long as they are accompanied with a width return value, or always
  ;; accept font changes and call text-width as required.  However, horizontal
  ;; motion can't really be accepted, due to semantics.]  First result is new
  ;; start, if end was not reached.  Second result is overall width, if known.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type sequence sequence)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type (or null int32) width)
	   (type translate translate)
	   (type index-size size)
	   (clx-values (or null array-index) (or null int32))))

(defun create-colormap (visual window &optional alloc-p)
  (declare (type visual-info visual)
	   (type window window)
	   (type boolean alloc-p)
	   (clx-values colormap)))

(defun free-colormap (colormap)
  (declare (type colormap colormap)))

(defun copy-colormap-and-free (colormap)
  (declare (type colormap colormap)
	   (clx-values colormap)))

(defun install-colormap (colormap)
  (declare (type colormap colormap)))

(defun uninstall-colormap (colormap)
  (declare (type colormap colormap)))

(defun installed-colormaps (window &key (result-type 'list))
  (declare (type window window)
	   (type type result-type)
	   (clx-values (clx-sequence colormap))))

(defun alloc-color (colormap color)
  (declare (type colormap colormap)
	   (type (or stringable color) color)
	   (clx-values pixel screen-color exact-color)))

(defun alloc-color-cells (colormap colors &key (planes 0) contiguous-p (result-type 'list))
  (declare (type colormap colormap)
	   (type card16 colors planes)
	   (type boolean contiguous-p)
	   (type type result-type)
	   (clx-values (clx-sequence pixel) (clx-sequence mask))))

(defun alloc-color-planes (colormap colors
			   &key (reds 0) (greens 0) (blues 0)
				contiguous-p (result-type 'list))
  (declare (type colormap colormap)
	   (type card16 colors reds greens blues)
	   (type boolean contiguous-p)
	   (type type result-type)
	   (clx-values (clx-sequence pixel) red-mask green-mask blue-mask)))

(defun free-colors (colormap pixels &optional (plane-mask 0))
  (declare (type colormap colormap)
	   (type (clx-sequence pixel) pixels)
	   (type pixel plane-mask)))

(defun store-color (colormap pixel spec &key (red-p t) (green-p t) (blue-p t))
  (declare (type colormap colormap)
	   (type pixel pixel)
	   (type (or stringable color) spec)
	   (type boolean red-p green-p blue-p)))

(defun store-colors (colormap specs &key (red-p t) (green-p t) (blue-p t))
  ;; If stringables are specified for colors, it is unspecified whether all
  ;; stringables are first resolved and then a single StoreColors protocol request is
  ;; issued, or whether multiple StoreColors protocol requests are issued.
  (declare (type colormap colormap)
	   (type (repeat-seq (pixel pixel) ((or stringable color) color)) specs)
	   (type boolean red-p green-p blue-p)))

(defun query-colors (colormap pixels &key (result-type 'list))
  (declare (type colormap colormap)
	   (type (clx-sequence pixel) pixels)
	   (type type result-type)
	   (clx-values (clx-sequence color))))

(defun lookup-color (colormap name)
  (declare (type colormap colormap)
	   (type stringable name)
	   (clx-values screen-color true-color)))

(defun create-cursor (&key source mask x y foreground background)
  (declare (type pixmap source)
	   (type (or null pixmap) mask)
	   (type card16 x y)
	   (type color foreground background)
	   (clx-values cursor)))

(defun create-glyph-cursor (&key source-font source-char mask-font mask-char
				 foreground background)
  (declare (type font source-font)
	   (type card16 source-char)
	   (type (or null font) mask-font)
	   (type (or null card16) mask-char)
	   (type color foreground background)
	   (clx-values cursor)))

(defun free-cursor (cursor)
  (declare (type cursor cursor)))

(defun recolor-cursor (cursor foreground background)
  (declare (type cursor cursor)
	   (type color foreground background)))

(defun query-best-cursor (width height drawable)
  (declare (type card16 width height)
	   (type drawable display)
	   (clx-values width height)))

(defun query-best-tile (width height drawable)
  (declare (type card16 width height)
	   (type drawable drawable)
	   (clx-values width height)))

(defun query-best-stipple (width height drawable)
  (declare (type card16 width height)
	   (type drawable drawable)
	   (clx-values width height)))

(defun query-extension (display name)
  (declare (type display display)
	   (type stringable name)
	   (clx-values major-opcode first-event first-error)))

(defun list-extensions (display &key (result-type 'list))
  (declare (type display display)
	   (type type result-type)
	   (clx-values (clx-sequence string))))

;; Should pointer-mapping setf be changed to set-pointer-mapping?

(defun set-modifier-mapping (display &key shift lock control mod1 mod2 mod3 mod4 mod5)
  ;; Can signal device-busy.
  ;; Setf ought to allow multiple values.
  ;; Returns true for success, nil for failure
  (declare (type display display)
	   (type (clx-sequence card8) shift lock control mod1 mod2 mod3 mod4 mod5)
	   (clx-values (member :success :busy :failed))))

(defun modifier-mapping (display)
  ;; each value is a list of card8s
  (declare (type display display)
	   (clx-values shift lock control mod1 mod2 mod3 mod4 mod5)))

;; Either we will want lots of defconstants for well-known values, or perhaps
;; an integer-to-keyword translation function for well-known values.

(defun change-keyboard-mapping (display keysyms
				&key (start 0) end (first-keycode start))
  ;; start/end give subrange of keysyms
  ;; first-keycode is the first-keycode to store at
  (declare (type display display)
	   (type (array * (* *)) keysyms)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type card8 first-keycode)))

(defun keyboard-mapping (display &key first-keycode start end data)
  ;; First-keycode specifies which keycode to start at (defaults to
  ;; min-keycode).  Start specifies where (in result) to put first-keycode
  ;; (defaults to first-keycode).  (- end start) is the number of keycodes to
  ;; get (end defaults to (1+ max-keycode)).  If data is specified, the results
  ;; are put there.
  (declare (type display display)
	   (type (or null card8) first-keycode)
	   (type (or null array-index) start end)
	   (type (or null (array * (* *))) data)
	   (clx-values (array * (* *)))))

(defun change-keyboard-control (display &key key-click-percent
				bell-percent bell-pitch bell-duration
				led led-mode key auto-repeat-mode)
  (declare (type display display)
	   (type (or null (member :default) int16) key-click-percent
						   bell-percent bell-pitch bell-duration)
	   (type (or null card8) led key)
	   (type (or null (member :on :off)) led-mode)
	   (type (or null (member :on :off :default)) auto-repeat-mode)))

(defun keyboard-control (display)
  (declare (type display display)
	   (clx-values key-click-percent bell-percent bell-pitch bell-duration
		   led-mask global-auto-repeat auto-repeats)))

(defun bell (display &optional (percent-from-normal 0))
  ;; It is assumed that an eventual audio extension to X will provide more complete
  ;; control.
  (declare (type display display)
	   (type int8 percent-from-normal)))

(defun pointer-mapping (display &key (result-type 'list))
  (declare (type display display)
	   (type type result-type)
	   (clx-values (clx-sequence card8))))

(defsetf pointer-mapping (display) (map)
  ;; Can signal device-busy.
  (declare (type display display)
	   (type (clx-sequence card8) map)))

(defun change-pointer-control (display &key acceleration threshold)
  ;; Acceleration is rationalized if necessary.
  (declare (type display display)
	   (type (or null (member :default) number) acceleration)
	   (type (or null (member :default) integer) threshold)))

(defun pointer-control (display)
  (declare (type display display)
	   (clx-values acceleration threshold)))

(defun set-screen-saver (display timeout interval blanking exposures)
  ;; Setf ought to allow multiple values.
  ;; Timeout and interval are in seconds, will be rounded to minutes.
  (declare (type display display)
	   (type (or (member :default) int16) timeout interval)
	   (type (member :on :off :default) blanking exposures)))

(defun screen-saver (display)
  ;; Returns timeout and interval in seconds.
  (declare (type display display)
	   (clx-values timeout interval blanking exposures)))

(defun activate-screen-saver (display)
  (declare (type display display)))

(defun reset-screen-saver (display)
  (declare (type display display)))

(defun add-access-host (display host)
  ;; A string must be acceptable as a host, but otherwise the possible types for host
  ;; are not constrained, and will likely be very system dependent.
  (declare (type display display)))

(defun remove-access-host (display host)
  ;; A string must be acceptable as a host, but otherwise the possible types for host
  ;; are not constrained, and will likely be very system dependent.
  (declare (type display display)))

(defun access-hosts (display &key (result-type 'list))
  ;; The type of host objects returned is not constrained, except that the hosts must
  ;; be acceptable to add-access-host and remove-access-host.
  (declare (type display display)
	   (type type result-type)
	   (clx-values (clx-sequence host) enabled-p)))

(defun access-control (display)
  ;; setf'able
  (declare (type display display)
	   (clx-values boolean)))

(defun close-down-mode (display)
  ;; setf'able
  ;; Cached locally in display object.
  (declare (type display display)
	   (clx-values (member :destroy :retain-permanent :retain-temporary))))

(defun kill-client (display resource-id)
  (declare (type display display)
	   (type resource-id resource-id)))

(defun kill-temporary-clients (display)
  (declare (type display display)))

(defun make-event-mask (&rest keys)
  ;; This is only defined for core events.
  ;; Useful for constructing event-mask, pointer-event-mask, device-event-mask.
  (declare (type (clx-list event-mask-class) keys)
	   (clx-values mask32)))

(defun make-event-keys (event-mask)
  ;; This is only defined for core events.
  (declare (type mask32 event-mask)
	   (clx-values (clx-list event-mask-class))))

(defun make-state-mask (&rest keys)
  ;; Useful for constructing modifier-mask, state-mask.
  (declare (type (clx-list state-mask-key) keys)
	   (clx-values mask16)))

(defun make-state-keys (state-mask)
  (declare (type mask16 mask)
	   (clx-values (clx-list state-mask-key))))

(defmacro with-event-queue ((display) &body body)
  ;; Grants exclusive access to event queue.
  )

(defun event-listen (display &optional (timeout 0))
  (declare (type display display)
	   (type (or null number) timeout)
	   (clx-values (or null number) (or null (member :timeout) (not null))))
  ;; Returns the number of events queued locally, if any, else nil.  Hangs
  ;; waiting for events, forever if timeout is nil, else for the specified
  ;; number of seconds.  The second value returned is :timeout if the
  ;; operation timed out, and some other non-nil value if an EOF has been
  ;; detected.
  )

(defun process-event (display &key handler timeout peek-p discard-p (force-output-p t))
  ;; If force-output-p is true, first invokes display-force-output.  Invokes
  ;; handler on each queued event until handler returns non-nil, and that
  ;; returned object is then returned by process-event.  If peek-p is true,
  ;; then the event is not removed from the queue.  If discard-p is true, then
  ;; events for which handler returns nil are removed from the queue,
  ;; otherwise they are left in place.  Hangs until non-nil is generated for
  ;; some event, or for the specified timeout (in seconds, if given); however,
  ;; it is acceptable for an implementation to wait only once on network data,
  ;; and therefore timeout prematurely.  Returns nil on timeout or EOF, with a
  ;; second return value being :timeout for a timeout and some other non-nil
  ;; value for EOF.  If handler is a sequence, it is expected to contain
  ;; handler functions specific to each event class; the event code is used to
  ;; index the sequence, fetching the appropriate handler.  The arguments to
  ;; the handler are described further below using declare-event.  If
  ;; process-event is invoked recursively, the nested invocation begins with
  ;; the event after the one currently being processed.
  (declare (type display display)
	   (type (or (clx-sequence (function (&key &allow-other-keys) t))
		     (function (&key &allow-other-keys) t))
		 handler)
	   (type (or null number) timeout)
	   (type boolean peek-p)))

(defun make-event-handlers (&key (type 'array) default)
  (declare (type t type)			;Sequence type specifier
	   (type function default)
	   (clx-values sequence))			;Default handler for initial content
  ;; Makes a handler sequence suitable for process-event
  )
   
(defun event-handler (handlers event-key)
  (declare (type sequence handlers)
	   (type event-key event-key)
	   (clx-values function))
  ;; Accessor for a handler sequence
  )

(defsetf event-handler (handlers event-key) (handler)
  (declare (type sequence handlers)
	   (type event-key event-key)
	   (type function handler)
	   (clx-values function))
  ;; Setf accessor for a handler sequence
  )

(defmacro event-case ((display &key timeout peek-p discard-p (force-output-p t))
		      &body clauses)
  (declare (arglist (display &key timeout peek-p discard-p force-output-p)
		    (event-or-events ((&rest args) |...|) &body body) |...|))
  ;; If force-output-p is true, first invokes display-force-output.  Executes
  ;; the matching clause for each queued event until a clause returns non-nil,
  ;; and that returned object is then returned by event-case.  If peek-p is
  ;; true, then the event is not removed from the queue.  If discard-p is
  ;; true, then events for which the clause returns nil are removed from the
  ;; queue, otherwise they are left in place.  Hangs until non-nil is
  ;; generated for some event, or for the specified timeout (in seconds, if
  ;; given); however, it is acceptable for an implementation to wait only once
  ;; on network data, and therefore timeout prematurely.  Returns nil on
  ;; timeout or EOF with a second return value being :timeout for a timeout
  ;; and some other non-nil value for EOF.  In each clause, event-or-events is
  ;; an event-key or a list of event-keys (but they need not be typed as
  ;; keywords) or the symbol t or otherwise (but only in the last clause).
  ;; The keys are not evaluated, and it is an error for the same key to appear
  ;; in more than one clause.  Args is the list of event components of
  ;; interest; corresponding values (if any) are bound to variables with these
  ;; names (i.e., the args are variable names, not keywords, the keywords are
  ;; derived from the variable names).  An arg can also be a (keyword var)
  ;; form, as for keyword args in a lambda lists.  If no t/otherwise clause
  ;; appears, it is equivalent to having one that returns nil.  If
  ;; process-event is invoked recursively, the nested invocation begins with
  ;; the event after the one currently being processed.
  )

(defmacro event-cond ((display &key timeout peek-p discard-p (force-output-p t))
		      &body clauses)
  ;; The clauses of event-cond are of the form:
  ;; (event-or-events binding-list test-form . body-forms)
  ;;
  ;; EVENT-OR-EVENTS	event-key or a list of event-keys (but they
  ;;			need not be typed as keywords) or the symbol t
  ;;			or otherwise (but only in the last clause).  If
  ;;			no t/otherwise clause appears, it is equivalent
  ;;			to having one that returns nil.  The keys are
  ;;			not evaluated, and it is an error for the same
  ;;			key to appear in more than one clause.
  ;;
  ;; BINDING-LIST	The list of event components of interest.
  ;;			corresponding values (if any) are bound to
  ;;			variables with these names (i.e., the binding-list
  ;;			has variable names, not keywords, the keywords are
  ;;			derived from the variable names).  An arg can also
  ;;			be a (keyword var) form, as for keyword args in a
  ;;			lambda list.
  ;;
  ;; The matching TEST-FORM for each queued event is executed until a
  ;; clause's test-form returns non-nil.  Then the BODY-FORMS are
  ;; evaluated, returning the (possibly multiple) values of the last
  ;; form from event-cond.  If there are no body-forms then, if the
  ;; test-form is non-nil, the value of the test-form is returned as a
  ;; single value.
  ;;
  ;; Options:
  ;; FORCE-OUTPUT-P	When true, first invoke display-force-output if no
  ;;		  	input is pending.
  ;;
  ;; PEEK-P		When true, then the event is not removed from the queue.
  ;;
  ;; DISCARD-P		When true, then events for which the clause returns nil
  ;; 			are removed from the queue, otherwise they are left in place.
  ;;
  ;; TIMEOUT		If NIL, hang until non-nil is generated for some event's
  ;;			test-form. Otherwise return NIL after TIMEOUT seconds have
  ;;			elapsed.  NIL is also returned whenever EOF is read.
  ;;			Whenever NIL is returned a second value is returned which
  ;;			is either :TIMEOUT if a timeout occurred or some other
  ;;			non-NIL value if an EOF is detected.
  ;;
  (declare (arglist (display &key timeout peek-p discard-p force-output-p)
		   (event-or-events (&rest args) test-form &body body) |...|))
  )

(defun discard-current-event (display)
  (declare (type display display)
	   (clx-values boolean))
  ;; Discard the current event for DISPLAY.
  ;; Returns NIL when the event queue is empty, else T.
  ;; To ensure events aren't ignored, application code should only call
  ;; this when throwing out of event-case or process-next-event, or from
  ;; inside even-case, event-cond or process-event when :peek-p is T and
  ;; :discard-p is NIL.
 )

(defmacro declare-event (event-codes &rest declares)
  ;; Used to indicate the keyword arguments for handler functions in process-event
  ;; and event-case.  In the declares, an argument listed as (name1 name2) indicates
  ;; synonyms for the same argument.  All process-event handlers can have
  ;; (display display), (event-key event-key), and (boolean send-event-p) as keyword
  ;; arguments, and an event-case clause can also have event-key and send-event-p as
  ;; arguments.
  (declare (arglist event-key-or-keys &rest (type &rest keywords))))

(declare-event (:key-press :key-release :button-press :button-release)
  (card16 sequence)
  (window (window event-window) root)
  ((or null window) child)
  (boolean same-screen-p)
  (int16 x y root-x root-y)
  (card16 state)
  ((or null card32) time)
  ;; for key-press and key-release, code is the keycode
  ;; for button-press and button-release, code is the button number
  (card8 code))

(declare-event :motion-notify
  (card16 sequence)
  (window (window event-window) root)
  ((or null window) child)
  (boolean same-screen-p)
  (int16 x y root-x root-y)
  (card16 state)
  ((or null card32) time)
  (boolean hint-p))

(declare-event (:enter-notify :leave-notify)
  (card16 sequence)
  (window (window event-window) root)
  ((or null window) child)
  (boolean same-screen-p)
  (int16 x y root-x root-y)
  (card16 state)
  ((or null card32) time)
  ((member :normal :grab :ungrab) mode)
  ((member :ancestor :virtual :inferior :nonlinear :nonlinear-virtual) kind)
  (boolean focus-p))

(declare-event (:focus-in :focus-out)
  (card16 sequence)
  (window (window event-window))
  ((member :normal :while-grabbed :grab :ungrab) mode)
  ((member :ancestor :virtual :inferior :nonlinear :nonlinear-virtual
	   :pointer :pointer-root :none)
   kind))

(declare-event :keymap-notify
  ((bit-vector 256) keymap))

(declare-event :exposure
  (card16 sequence)
  (window (window event-window))
  (card16 x y width height count))

(declare-event :graphics-exposure
  (card16 sequence)
  (drawable (drawable event-window))
  (card16 x y width height count)
  (card8 major)
  (card16 minor))

(declare-event :no-exposure
  (card16 sequence)
  (drawable (drawable event-window))
  (card8 major)
  (card16 minor))

(declare-event :visibility-notify
  (card16 sequence)
  (window (window event-window))
  ((member :unobscured :partially-obscured :fully-obscured) state))

(declare-event :create-notify
  (card16 sequence)
  (window window (parent event-window))
  (int16 x y)
  (card16 width height border-width)
  (boolean override-redirect-p))

(declare-event :destroy-notify
  (card16 sequence)
  (window event-window window))

(declare-event :unmap-notify
  (card16 sequence)
  (window event-window window)
  (boolean configure-p))

(declare-event :map-notify
  (card16 sequence)
  (window event-window window)
  (boolean override-redirect-p))

(declare-event :map-request
  (card16 sequence)
  (window (parent event-window) window))

(declare-event :reparent-notify
  (card16 sequence)
  (window event-window window parent)
  (int16 x y)
  (boolean override-redirect-p))

(declare-event :configure-notify
  (card16 sequence)
  (window event-window window)
  (int16 x y)
  (card16 width height border-width)
  ((or null window) above-sibling)
  (boolean override-redirect-p))

(declare-event :gravity-notify
  (card16 sequence)
  (window event-window window)
  (int16 x y))

(declare-event :resize-request
  (card16 sequence)
  (window (window event-window))
  (card16 width height))

(declare-event :configure-request
  (card16 sequence)
  (window (parent event-window) window)
  (int16 x y)
  (card16 width height border-width)
  ((member :above :below :top-if :bottom-if :opposite) stack-mode)
  ((or null window) above-sibling)
  (mask16 value-mask))

(declare-event :circulate-notify
  (card16 sequence)
  (window event-window window)
  ((member :top :bottom) place))

(declare-event :circulate-request
  (card16 sequence)
  (window (parent event-window) window)
  ((member :top :bottom) place))

(declare-event :property-notify
  (card16 sequence)
  (window (window event-window))
  (keyword atom)
  ((member :new-value :deleted) state)
  ((or null card32) time))

(declare-event :selection-clear
  (card16 sequence)
  (window (window event-window))
  (keyword selection)
  ((or null card32) time))

(declare-event :selection-request
  (card16 sequence)
  (window (window event-window) requestor)
  (keyword selection target)
  ((or null keyword) property)
  ((or null card32) time))

(declare-event :selection-notify
  (card16 sequence)
  (window (window event-window))
  (keyword selection target)
  ((or null keyword) property)
  ((or null card32) time))

(declare-event :colormap-notify
  (card16 sequence)
  (window (window event-window))
  ((or null colormap) colormap)
  (boolean new-p installed-p))

(declare-event :mapping-notify
  (card16 sequence)
  ((member :modifier :keyboard :pointer) request)
  (card8 start count))

(declare-event :client-message
  (card16 sequence)
  (window (window event-window))
  ((member 8 16 32) format)
  (sequence data))

(defun queue-event (display event-key &rest args &key append-p &allow-other-keys)
  ;; The event is put at the head of the queue if append-p is nil, else the tail.
  ;; Additional arguments depend on event-key, and are as specified above with
  ;; declare-event, except that both resource-ids and resource objects are accepted
  ;; in the event components.
  (declare (type display display)
	   (type event-key event-key)
	   (type boolean append-p)))



;;; From here on, there has been less coherent review of the interface:

;;;-----------------------------------------------------------------------------
;;; Window Manager Property functions

(defun wm-name (window)
  (declare (type window window)
	   (clx-values string)))

(defsetf wm-name (window) (name))

(defun wm-icon-name (window)
  (declare (type window window)
	   (clx-values string)))

(defsetf wm-icon-name (window) (name))

(defun get-wm-class (window)
  (declare (type window window)
	   (clx-values (or null name-string) (or null class-string))))

(defun set-wm-class (window resource-name resource-class)
  (declare (type window window)
	   (type (or null stringable) resource-name resource-class)))

(defun wm-command (window)
  ;; Returns a list whose car is a command string and 
  ;; whose cdr is the list of argument strings.
  (declare (type window window)
	   (clx-values (clx-list string))))

(defsetf wm-command (window) (command)
  ;; Uses PRIN1 inside the ANSI common lisp form WITH-STANDARD-IO-SYNTAX (or
  ;; equivalent), with elements of command separated by NULL characters.  This
  ;; enables 
  ;;   (with-standard-io-syntax (mapcar #'read-from-string (wm-command window)))
  ;; to recover a lisp command.
  (declare (type window window)
	   (type (clx-list stringable) command)))

(defun wm-client-machine (window)
  ;; Returns a list whose car is a command string and 
  ;; whose cdr is the list of argument strings.
  (declare (type window window)
	   (clx-values string)))

(defsetf wm-client-machine (window) (string)
  (declare (type window window)
	   (type stringable string)))

(defstruct wm-hints
  (input nil :type (or null (member :off :on)))
  (initial-state nil :type (or null (member :normal :iconic)))
  (icon-pixmap nil :type (or null pixmap))
  (icon-window nil :type (or null window))
  (icon-x nil :type (or null card16))
  (icon-y nil :type (or null card16))
  (icon-mask nil :type (or null pixmap))
  (window-group nil :type (or null resource-id))
  (flags 0 :type card32)    ;; Extension-hook.  Exclusive-Or'ed with the FLAGS field
  ;; may be extended in the future
  )

(defun wm-hints (window)
  (declare (type window window)
	   (clx-values wm-hints)))

(defsetf wm-hints (window) (wm-hints))


(defstruct wm-size-hints
  ;; Defaulted T to put the burden of remembering these on widget programmers.
  (user-specified-position-p t :type boolean) ;; True when user specified x y
  (user-specified-size-p t :type boolean)     ;; True when user specified width height
  (x nil :type (or null int16))		      ;; Obsolete
  (y nil :type (or null int16))		      ;; Obsolete
  (width nil :type (or null card16))	      ;; Obsolete
  (height nil :type (or null card16))	      ;; Obsolete
  (min-width nil :type (or null card16))
  (min-height nil :type (or null card16))
  (max-width nil :type (or null card16))
  (max-height nil :type (or null card16))
  (width-inc nil :type (or null card16))
  (height-inc nil :type (or null card16))
  (min-aspect nil :type (or null number))
  (max-aspect nil :type (or null number))
  (base-width nil :type (or null card16))
  (base-height nil :type (or null card16))
  (win-gravity nil :type (or null win-gravity)))

(defun wm-normal-hints (window)
  (declare (type window window)
	   (clx-values wm-size-hints)))

(defsetf wm-normal-hints (window) (wm-size-hints))

;; ICON-SIZES uses the SIZE-HINTS structure

(defun icon-sizes (window)
  (declare (type window window)
	   (clx-values wm-size-hints)))
  
(defsetf icon-sizes (window) (wm-size-hints))

(defun wm-protocols (window)
  (declare (type window window)
	   (clx-values protocols)))
  
(defsetf wm-protocols (window) (protocols)
  (declare (type window window)
	   (type (clx-list keyword) protocols)))

(defun wm-colormap-windows (window)
  (declare (type window window)
	   (clx-values windows)))
  
(defsetf wm-colormap-windows (window) (windows)
  (declare (type window window)
	   (type (clx-list window) windows)))

(defun transient-for (window)
  (declare (type window window)
	   (clx-values window)))

(defsetf transient-for (window) (transient)
  (declare (type window window transient)))

(defun set-wm-properties (window &rest options &key 
			  name icon-name resource-name resource-class command
			  hints normal-hints
			  ;; the following are used for wm-normal-hints
			  user-specified-position-p user-specified-size-p
			  program-specified-position-p program-specified-size-p
			  min-width min-height max-width max-height
			  width-inc height-inc min-aspect max-aspect
			  base-width base-height win-gravity
			  ;; the following are used for wm-hints
			  input initial-state icon-pixmap icon-window
			  icon-x icon-y icon-mask window-group)
  ;; Set properties for WINDOW.
  (declare (type window window)
	   (type (or null stringable) name icoin-name resource-name resource-class)
	   (type (or null list) command)
	   (type (or null wm-hints) hints)
	   (type (or null wm-size-hints) normal-hints)
	   (type boolean user-specified-position-p user-specified-size-p)
	   (type boolean program-specified-position-p program-specified-size-p)
	   (type (or null card16) min-width min-height max-width max-height width-inc height-inc base-width base-height win-gravity)
	   (type (or null number) min-aspect max-aspect)
	   (type (or null (member :off :on)) input)
	   (type (or null (member :normal :iconic)) initial-state)
	   (type (or null pixmap) icon-pixmap icon-mask)
	   (type (or null window) icon-window)
	   (type (or null card16) icon-x icon-y)
	   (type (or null resource-id) window-group)))

(defun iconify-window (window)
  (declare (type window window)))

(defun withdraw-window (window)
  (declare (type window window)))
 
(defstruct standard-colormap
  (colormap nil :type (or null colormap))
  (base-pixel 0 :type pixel)
  (max-color nil :type (or null color))
  (mult-color nil :type (or null color))
  (visual nil :type (or null visual-info))
  (kill nil :type (or (member nil :release-by-freeing-colormap)
		      drawable gcontext cursor colormap font)))

(defun rgb-colormaps (window property)
  (declare (type window window)
	   (type (member :rgb_default_map :rgb_best_map :rgb_red_map
			 :rgb_green_map :rgb_blue_map) property)
	   (clx-values (clx-list standard-colormap))))

(defsetf rgb-colormaps (window property) (standard-colormaps)
  (declare (type window window)
	   (type (member :rgb_default_map :rgb_best_map :rgb_red_map
			 :rgb_green_map :rgb_blue_map) property)
	   (type (clx-list standard-colormap) standard-colormaps)))

(defun cut-buffer (display &key (buffer 0) (type :string) (result-type 'string)
		                (transform #'card8->char) (start 0) end)
  ;; Return the contents of cut-buffer BUFFER
  (declare (type display display)
	   (type (integer 0 7) buffer)
	   (type xatom type)
	   (type array-index start)
	   (type (or null array-index) end)
	   (type t result-type)			;a sequence type
	   (type (or null (function (integer) t)) transform)
	   (clx-values sequence type format bytes-after)))

(defsetf cut-buffer (display buffer &key (type :string) (format 8)
			     (transform #'char->card8) (start 0) end) (data))

(defun rotate-cut-buffers (display &optional (delta 1) (careful-p t))
  ;; Positive rotates left, negative rotates right (opposite of actual
  ;; protocol request).  When careful-p, ensure all cut-buffer
  ;; properties are defined, to prevent errors.
  (declare (type display display)
	   (type int16 delta)
	   (type boolean careful-p)))

;;;-----------------------------------------------------------------------------
;;; Keycode mapping

(defun define-keysym-set (set first-keysym last-keysym)
  ;; Define all keysyms from first-keysym up to and including
  ;; last-keysym to be in SET (returned from the keysym-set function).
  ;; Signals an error if the keysym range overlaps an existing set.
 (declare (type keyword set)
	  (type keysym first-keysym last-keysym)))

(defun keysym-set (keysym)
  ;; Return the character code set name of keysym
  ;; Note that the keyboard set (255) has been broken up into its parts.
  (declare (type keysym keysym)
	   (clx-values keyword)))

(defun define-keysym (object keysym &key lowercase translate modifiers mask display)	              
  ;; Define the translation from keysym/modifiers to a (usually
  ;; character) object.  ANy previous keysym definition with
  ;; KEYSYM and MODIFIERS is deleted before adding the new definition.
  ;;
  ;; MODIFIERS is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying when to use this
  ;; keysym-translation.  The default is NIL.
  ;;
  ;; MASK is either a modifier-mask or list containing intermixed
  ;; keysyms and state-mask-keys specifying which modifiers to look at
  ;; (i.e.  modifiers not specified are don't-cares).
  ;; If mask is :MODIFIERS then the mask is the same as the modifiers
  ;; (i.e.  modifiers not specified by modifiers are don't cares)
  ;; The default mask is *default-keysym-translate-mask*
  ;;
  ;; If DISPLAY is specified, the translation will be local to DISPLAY,
  ;; otherwise it will be the default translation for all displays.
  ;;
  ;; LOWERCASE is used for uppercase alphabetic keysyms.  The value
  ;; is the associated lowercase keysym.  This information is used
  ;; by the keysym-both-case-p predicate (for caps-lock computations)
  ;; and by the keysym-downcase function.
  ;;
  ;; TRANSLATE will be called with parameters (display state OBJECT)
  ;; when translating KEYSYM and modifiers and mask are satisfied.
  ;; [e.g (zerop (logxor (logand state (or mask *default-keysym-translate-mask*))
  ;;                     (or modifiers 0)))
  ;;      when mask and modifiers aren't lists of keysyms]
  ;; The default is #'default-keysym-translate
  ;;
  (declare (type (or base-char t) object)
	   (type keysym keysym)
	   (type (or null mask16 (clx-list (or keysym state-mask-key)))
	         modifiers)
	   (type (or null (member :modifiers) mask16 (clx-list (or keysym state-mask-key)))
	         mask)
	   (type (or null display) display)
           (type (or null keysym) lowercase)
	   (type (function (display card16 t) t) translate)))

(defvar *default-keysym-translate-mask*
	(the (or (member :modifiers) mask16 (clx-list (or keysym state-mask-key)))
	     (logand #xff (lognot (make-state-mask :lock))))
  "Default keysym state mask to use during keysym-translation.")

(defun undefine-keysym (object keysym &key display modifiers &allow-other-keys)	              
  ;; Undefine the keysym-translation translating KEYSYM to OBJECT with MODIFIERS.
  ;; If DISPLAY is non-nil, undefine the translation for DISPLAY if it exists.
  (declare (type (or base-char t) object)
	   (type keysym keysym)
	   (type (or null mask16 (clx-list (or keysym state-mask-key)))
	         modifiers)
	   (type (or null display) display)))

(defun default-keysym-translate (display state object)
  ;; If object is a character, char-bits are set from state.
  ;; If object is a list, it is an alist with entries:
  ;; (base-char [modifiers] [mask-modifiers)
  ;; When MODIFIERS are specified, this character translation
  ;; will only take effect when the specified modifiers are pressed.
  ;; MASK-MODIFIERS can be used to specify a set of modifiers to ignore.
  ;; When MASK-MODIFIERS is missing, all other modifiers are ignored.
  ;; In ambiguous cases, the most specific translation is used.
  (declare (type display display)
	   (type card16 state)
	   (type t object)
	   (clx-values t))) ;; Object returned by keycode->character
   
(defmacro keysym (keysym &rest bytes)
  ;; Build a keysym.
  ;; If KEYSYM is an integer, it is used as the most significant bits of
  ;; the keysym, and BYTES are used to specify low order bytes. The last
  ;; parameter is always byte4 of the keysym.  If KEYSYM is not an
  ;; integer, the keysym associated with KEYSYM is returned.
  ;;
  ;; This is a macro and not a function macro to promote compile-time
  ;; lookup. All arguments are evaluated.
  (declare (type t keysym)
	   (type (clx-list card8) bytes)
	   (clx-values keysym)))

(defun character->keysyms (character &optional display)
  ;; Given a character, return a list of all matching keysyms.
  ;; If DISPLAY is given, translations specific to DISPLAY are used,
  ;; otherwise only global translations are used.
  ;; Implementation dependent function.
  ;; May be slow [i.e. do a linear search over all known keysyms]
  (declare (type t character)
	   (type (or null display) display)
	   (clx-values (clx-list keysym))))

(defun keycode->keysym (display keycode keysym-index)
  (declare (type display display)
	   (type card8 code)
	   (type card16 state)
	   (type card8 keysym-index)
	   (clx-values keysym)))

(defun keysym->keycodes (display keysym)
  ;; Return keycodes for keysym, as multiple values
  (declare (type display display)
	   (type keysym keysym)
	   (clx-values (or null keycode) (or null keycode) (or null keycode)))
  )

(defun keysym->character (display keysym &optional state)
  ;; Find the character associated with a keysym.
  ;; STATE is used for adding char-bits to character as follows:
  ;;    control -> char-control-bit
  ;;    mod-1 -> char-meta-bit
  ;;    mod-2 -> char-super-bit
  ;;    mod-3 -> char-hyper-bit
  ;; Implementation dependent function.
  (declare (type display display)
	   (type keysym keysym)
	   (type (or null card16) state)
	   (clx-values (or null character))))

(defun keycode->character (display keycode state &key keysym-index
	                   (keysym-index-function #'default-keysym-index))
  ;; keysym-index defaults to the result of keysym-index-function which
  ;; is called with the following parameters:
  ;; (char0 state caps-lock-p keysyms-per-keycode)
  ;; where char0 is the "character" object associated with keysym-index 0 and
  ;; caps-lock-p is non-nil when the keysym associated with the lock
  ;; modifier is for caps-lock.
  ;; STATE is also used for setting char-bits:
  ;;    control -> char-control-bit
  ;;    mod-1 -> char-meta-bit
  ;;    mod-2 -> char-super-bit
  ;;    mod-3 -> char-hyper-bit
  ;; Implementation dependent function.
  (declare (type display display)
	   (type card8 keycode)
	   (type card16 state)
	   (type (or null card8) keysym-index)
	   (type (or null (function (char0 state caps-lock-p keysyms-per-keycode) card8))
		 keysym-index-function)
	   (clx-values (or null character))))

(defun default-keysym-index (display keycode state)
  ;; Returns a keysym-index for use with keycode->character
  (declare (clx-values card8))
)

;;; default-keysym-index implements the following tables:
;;;
;;; control shift caps-lock character               character
;;;   0       0       0       #\a                      #\8
;;;   0       0       1       #\A                      #\8
;;;   0       1       0       #\A                      #\*
;;;   0       1       1       #\A                      #\*
;;;   1       0       0       #\control-A              #\control-8
;;;   1       0       1       #\control-A              #\control-8
;;;   1       1       0       #\control-shift-a        #\control-*
;;;   1       1       1       #\control-shift-a        #\control-*
;;;
;;; control shift shift-lock character               character
;;;   0       0       0       #\a                      #\8
;;;   0       0       1       #\A                      #\*
;;;   0       1       0       #\A                      #\*
;;;   0       1       1       #\A                      #\8
;;;   1       0       0       #\control-A              #\control-8
;;;   1       0       1       #\control-A              #\control-*
;;;   1       1       0       #\control-shift-a        #\control-*
;;;   1       1       1       #\control-shift-a        #\control-8

(defun state-keysymp (display state keysym)
  ;; Returns T when a modifier key associated with KEYSYM is on in STATE
  (declare (type display display)
	   (type card16 state)
	   (type keysym keysym)
	   (clx-values boolean)))

(defun mapping-notify (display request start count)
  ;; Called on a mapping-notify event to update
  ;; the keyboard-mapping cache in DISPLAY
  (declare (type display display)
	   (type (member :modifier :keyboard :pointer) request)
	   (type card8 start count)))

(defun keysym-in-map-p (display keysym keymap)
  ;; Returns T if keysym is found in keymap
  (declare (type display display)
	   (type keysym keysym)
	   (type (bit-vector 256) keymap)
	   (value boolean)))

(defun character-in-map-p (display character keymap)
  ;; Implementation dependent function.
  ;; Returns T if character is found in keymap
  (declare (type display display)
	   (type t character)
	   (type (bit-vector 256) keymap)
	   (value boolean)))

;;;-----------------------------------------------------------------------------
;;; Extensions

(defmacro define-extension (name &key events errors)
  ;; Define extension NAME with EVENTS and ERRORS.
  ;; Note: The case of NAME is important.
  ;; To define the request, Use:
  ;;     (with-buffer-request (display (extension-opcode ,name)) ,@body)
  ;;     See the REQUESTS file for lots of examples.
  ;; To define event handlers, use declare-event.
  ;; To define error handlers, use declare-error and define-condition.
  (declare (type stringable name)
	   (type (clx-list symbol) events errors)))

(defmacro extension-opcode (display name)
  ;; Returns the major opcode for extension NAME.
  ;; This is a macro to enable NAME to be interned for fast run-time
  ;; retrieval. 
  ;; Note: The case of NAME is important.
  (declare (type display display)
	   (type stringable name)
	   (clx-values card8)))

(defmacro define-error (error-key function)
  ;; Associate a function with ERROR-KEY which will be called with
  ;; parameters DISPLAY and REPLY-BUFFER and returns a plist of
  ;; keyword/value pairs which will be passed on to the error handler.
  ;; A compiler warning is printed when ERROR-KEY is not defined in a
  ;; preceding DEFINE-EXTENSION.
  ;; Note: REPLY-BUFFER may used with the READING-EVENT and READ-type
  ;;       macros for getting error fields. See DECODE-CORE-ERROR for
  ;        an example.
  (declare (type symbol error-key)
	   (type function function)))

;; All core errors use this, so we make it available to extensions.
(defun decode-core-error (display event &optional arg)
  ;; All core errors have the following keyword/argument pairs:
  ;;    :major integer
  ;;    :minor integer
  ;;    :sequence integer
  ;;    :current-sequence integer
  ;; In addition, many have an additional argument that comes from the
  ;; same place in the event, but is named differently.  When the ARG
  ;; argument is specified, the keyword ARG with card32 value starting
  ;; at byte 4 of the event is returned with the other keyword/argument
  ;; pairs.
  (declare (type display display)
	   (type reply-buffer event)
	   (type (or null keyword) arg)
	   (clx-values keyword/arg-plist)))

;; This isn't new, just extended.
(defmacro declare-event (event-codes &body declares)
  ;; Used to indicate the keyword arguments for handler functions in
  ;; process-event and event-case.
  ;; Generates functions used in SEND-EVENT.
  ;; A compiler warning is printed when all of EVENT-CODES are not
  ;; defined by a preceding DEFINE-EXTENSION.
  ;; See the INPUT file for lots of examples.
  (declare (type (or keyword (clx-list keywords)) event-codes)
	   (type (alist (field-type symbol) (field-names (clx-list symbol)))
                 declares)))

(defmacro define-gcontext-accessor (name &key default set-function copy-function)
  ;; This will define a new gcontext accessor called NAME.
  ;; Defines the gcontext-NAME accessor function and its defsetf.
  ;; Gcontext's will cache DEFAULT-VALUE and the last value SETF'ed when
  ;; gcontext-cache-p is true.  The NAME keyword will be allowed in
  ;; CREATE-GCONTEXT, WITH-GCONTEXT, and COPY-GCONTEXT-COMPONENTS.
  ;; SET-FUNCTION will be called with parameters (GCONTEXT NEW-VALUE)
  ;; from create-gcontext, and force-gcontext-changes.
  ;; COPY-FUNCTION will be called with parameters (src-gc dst-gc src-value)
  ;; from copy-gcontext and copy-gcontext-components.
  ;; The copy-function defaults to:
  ;; (lambda (ignore dst-gc value)
  ;;    (if value
  ;;	    (,set-function dst-gc value)
  ;;	  (error "Can't copy unknown GContext component ~a" ',name)))
  (declare (type symbol name)
	   (type t default)
	   (type symbol set-function) ;; required
	   (type symbol copy-function)))


;; To aid extension implementors in attaching additional information to
;; clx data structures, the following accessors (with SETF's) are
;; defined.  GETF can be used on these to extend the structures.

display-plist
screen-plist
visual-info-plist
gcontext-plist
font-plist
drawable-plist



;;; These have had perhaps even less review.

;;; Add some of the functionality provided by the C XLIB library.
;;;
;;; LaMott G. Oren, Texas Instruments  10/87
;;; 
;;; Design Contributors:
;;;	Robert W. Scheifler, MIT

;;;-----------------------------------------------------------------------------
;;; Regions (not yet implemented)

;;; Regions are arbitrary collections of pixels.  This is represented
;;; in the region structure as either a list of rectangles or a bitmap.

(defun make-region (&optional x y width height)
  ;; With no parameters, returns an empty region
  ;; If some parameters are given, all must be given.
  (declare (type (or null int16) x y width height)
	   (clx-values region)))

(defun region-p (thing))

(defun copy-region (region))

(defun region-empty-p (region)
  (declare (type region region)
	   (clx-values boolean)))

(defun region-clip-box (region)
  ;; Returns a region which is the smallest enclosing rectangle
  ;; enclosing REGION
  (declare (type region region)
	   (clx-values region)))

;; Accessors that return the boundaries of a region
(defun region-x (region))
(defun region-y (region))
(defun region-width (region))
(defun region-height (region))

(defsetf region-x (region) (x))
(defsetf region-y (region) (y))
;; Setting a region's X/Y translates the region

(defun region-intersection (&rest regions)
  "Returns a region which is the intersection of one or more REGIONS.
Returns an empty region if the intersection is empty.
If there are no regions given, return a very large region."
  (declare (type (clx-list region) regions)
	   (clx-values region)))

(defun region-union (&rest regions)
  "Returns a region which is the union of a number of REGIONS
 (i.e. the smallest region that can contain all the other regions)
 Returns the empty region if no regions are given."
  (declare (type (clx-list region) regions)
	   (clx-values region)))

(defun region-subtract (region subtract)
  "Returns a region containing the points that are in REGION but not in SUBTRACT"
  (declare (type region region subtract)
	   (clx-values region)))

(defun point-in-region-p (region x y)
  ;; Returns T when X/Y are a point within REGION.
  (declare (type region region)
	   (type int16 x y)
	   (clx-values boolean)))

(defun region-equal (a b)
  ;; Returns T when regions a and b contain the same points.
  ;; That is, return t when for every X/Y (point-in-region-p a x y)
  ;; equals (point-in-region-p b x y)
  (declare (type region a b)
	   (clx-values boolean)))

(defun subregion-p (large small)
  "Returns T if SMALL is within LARGE.
 That is, return T when for every X/Y (point-in-region-p small X Y)
 implies (point-in-region-p large X Y)."
  (declare (type region large small)
	   (clx-values boolean)))

(defun region-intersect-p (a b)
  "Returns T if A intersects B.
 That is, return T when there is some point common to regions A and B."
  (declare (type region a b)
	   (clx-values boolean)))

(defun map-region (region function &rest args)
  ;; Calls function with arguments (x y . args) for every point in REGION.
  (declare (type region region)
	   (type (function x y &rest args) function)))

;;   Why isn't it better to augment
;;   gcontext-clip-mask to deal with
;;   	(or null (member :none) pixmap rect-seq region)
;;   and force conversions on the caller?
;; Good idea.

;;(defun gcontext-clip-region (gcontext)
;;  ;; If the clip-mask of GCONTEXT is known, return it as a region.
;;  (declare (type gcontext gcontext)
;;	   (clx-values (or null region))))

;;(defsetf gcontext-clip-region (gcontext) (region)
;;  ;; Set the clip-rectangles or clip-mask for for GCONTEXT to include
;;  ;; only the pixels within REGION.
;;  (declare (type gcontext gcontext)
;;	   (type region region)))

(defun image->region (image)
  ;; Returns a region containing the 1 bits of a depth-1 image
  ;; Signals an error if image isn't of depth 1.
  (declare (type image image)
	   (clx-values region)))

(defun region->image (region)
  ;; Returns a depth-1 image containg 1 bits for every pixel in REGION.
  (declare (type region region)
	   (clx-values image)))

(defun polygon-region (points &optional (fill-rule :even-odd))
  (declare (type sequence points) ;(repeat-seq (integer x) (integer y))
	   (type (member :even-odd :winding) fill-rule)
	   (clx-values region)))

;;;-----------------------------------------------------------------------------
;;; IMAGE functions


(deftype bitmap () '(array bit (* *)))
(deftype pixarray () '(array pixel (* *)))

(defconstant *lisp-byte-lsb-first-p* #+lispm t #-lispm nil
	     "Byte order in pixel arrays")

(defstruct image
  ;; Public structure
  (width 0 :type card16 :read-only t)
  (height 0 :type card16 :read-only t)
  (depth 1 :type card8 :read-only t)
  (plist nil :type list))

;; Image-Plist accessors:
(defun image-name (image))
(defun image-x-hot (image))
(defun image-y-hot (image))
(defun image-red-mask (image))
(defun image-blue-mask (image))
(defun image-green-mask (image))

(defsetf image-name (image) (name))
(defsetf image-x-hot (image) (x))
(defsetf image-y-hot (image) (y))
(defsetf image-red-mask (image) (mask))
(defsetf image-blue-mask (image) (mask))
(defsetf image-green-mask (image) (mask))

(defstruct (image-x (:include image))
  ;; Use this format for shoveling image data
  ;; Private structure. Accessors for these NOT exported.
  (format :z-pixmap :type (member :bitmap :xy-pixmap :z-pixmap))
  (bytes-per-line 0 :type card16)
  (scanline-pad 32 :type (member 8 16 32))
  (bits-per-pixel 0 :type (member 1 4 8 16 24 32))
  (bit-lsb-first-p nil :type boolean)		; Bit order
  (byte-lsb-first-p nil :type boolean)		; Byte order
  (data #() :type (array card8 (*))))		; row-major

(defstruct (image-xy (:include image))
  ;; Public structure
  ;; Use this format for image processing
  (bitmap-list nil :type (clx-list bitmap)))

(defstruct (image-z (:include image))
  ;; Public structure
  ;; Use this format for image processing
  (bits-per-pixel 0 :type (member 1 4 8 16 24 32))
  (pixarray #() :type pixarray))

(defun create-image (&key (width (required-arg width))
		          (height (required-arg height))
		     depth data plist name x-hot y-hot
		     red-mask blue-mask green-mask
		     bits-per-pixel format scanline-pad bytes-per-line
		     byte-lsb-first-p bit-lsb-first-p )
  ;; Returns an image-x image-xy or image-z structure, depending on the
  ;; type of the :DATA parameter.
  (declare
    (type card16 width height)			; Required
    (type (or null card8) depth)		; Defualts to 1
    (type (or (array card8 (*))			;Returns image-x
	      (clx-list bitmap)			;Returns image-xy
	      pixarray) data)			;Returns image-z
    (type list plist)
    (type (or null stringable) name)
    (type (or null card16) x-hot y-hot)
    (type (or null pixel) red-mask blue-mask green-mask)
    (type (or null (member 1 4 8 16 24 32)) bits-per-pixel)

    ;; The following parameters are ignored for image-xy and image-z:
    (type (or null (member :bitmap :xy-pixmap :z-pixmap))
	  format)				; defaults to :z-pixmap
    (type (or null (member 8 16 32)) scanline-pad)
    (type (or null card16) bytes-per-line) ;default from width and scanline-pad
    (type boolean byte-lsb-first-p bit-lsb-first-p)
    (clx-values image)))

(defun get-image (drawable &key 
		  (x (required-arg x))
		  (y (required-arg y))
		  (width (required-arg width))
		  (height (required-arg height))
		  plane-mask format result-type)
  ;; Get an image from the server.
  ;; Format defaults to :z-pixmap.  Result-Type defaults from Format,
  ;; image-z for :z-pixmap, and image-xy for :xy-pixmap.
  ;; Plane-mask defaults to #xFFFFFFFF.
  ;; Returns an image-x image-xy or image-z structure, depending on the
  ;; result-type parameter.
  (declare (type drawable drawable)
	   (type int16 x y) ;; required
	   (type card16 width height) ;; required
	   (type (or null pixel) plane-mask)
	   (type (or null (member :xy-pixmap :z-pixmap)) format)
	   (type (or null (member image-x image-xy image-z)) result-type)
	   (clx-values image)))

(defun put-image (drawable gcontext image &key
		  (src-x 0) (src-y 0)
		  (x (required-arg x))
		  (y (required-arg y))
		  width height
		  bitmap-p)
  ;; When BITMAP-P, force format to be :bitmap when depth=1
  ;; This causes gcontext to supply foreground & background pixels.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type image image)
	   (type int16 x y) ;; required
	   (type (or null card16) width height)
	   (type boolean bitmap-p)))

(defun copy-image (image &key (x 0) (y 0) width height result-type)
  ;; Copy with optional sub-imaging and format conversion.
  ;; result-type defaults to (type-of image)
  (declare (type image image)
	   (type card16 x y)
	   (type (or null card16) width height) ;; Default from image
	   (type (or null (member image-x image-xy image-z)) result-type)
	   (clx-values image)))

(defun read-bitmap-file (pathname)
  ;; Creates an image from a C include file in standard X11 format
  (declare (type (or pathname string stream) pathname)
	   (clx-values image)))

(defun write-bitmap-file (pathname image &optional name)
  ;; Writes an image to a C include file in standard X11 format
  ;; NAME argument used for variable prefixes.  Defaults to "image"
  (declare (type (or pathname string stream) pathname)
	   (type image image)
	   (type (or null stringable) name)))

;;;-----------------------------------------------------------------------------
;;; Resource data-base


(defun make-resource-database ()
  ;; Returns an empty resource data-base
  (declare (clx-values resource-database)))

(defun get-resource (database value-name value-class full-name full-class)
  ;; Return the value of the resource in DATABASE whose partial name
  ;; most closely matches (append full-name (list value-name)) and
  ;;                      (append full-class (list value-class)).
  (declare (type resource-database database)
	   (type stringable value-name value-class)
	   (type (clx-list stringable) full-name full-class)
	   (clx-values value)))

(defun add-resource (database name-list value)
  ;; name-list is a list of either strings or symbols. If a symbol, 
  ;; case-insensitive comparisons will be used, if a string,
  ;; case-sensitive comparisons will be used.  The symbol '* or
  ;; string "*" are used as wildcards, matching anything or nothing.
  (declare (type resource-database database)
	   (type (clx-list stringable) name-list)
	   (type t value)))

(defun delete-resource (database name-list)
  (declare (type resource-database database)
	   (type (clx-list stringable) name-list)))

(defun map-resource (database function &rest args)
  ;; Call FUNCTION on each resource in DATABASE.
  ;; FUNCTION is called with arguments (name-list value . args)
  (declare (type resource-database database)
	   (type (function ((clx-list stringable) t &rest t) t) function)
	   (clx-values nil)))

(defun merge-resources (database with-database)
  (declare (type resource-database database with-database)
	   (clx-values resource-database))
  (map-resource #'add-resource database with-database)
  with-database)

;; Note: with-input-from-string can be used with read-resources to define
;;       default resources in a program file.

(defun read-resources (database pathname &key key test test-not)
  ;; Merges resources from a file in standard X11 format with DATABASE.
  ;; KEY is a function used for converting value-strings, the default is
  ;; identity.  TEST and TEST-NOT are predicates used for filtering
  ;; which resources to include in the database.  They are called with
  ;; the name and results of the KEY function.
  (declare (type resource-database database)
	   (type (or pathname string stream) pathname)
	   (type (or null (function (string) t)) key)
	   (type (or null (function ((clx-list string) t) boolean))
                 test test-not)
	   (clx-values resource-database)))

(defun write-resources (database pathname &key write test test-not)
  ;; Write resources to PATHNAME in the standard X11 format.
  ;; WRITE is a function used for writing values, the default is #'princ
  ;; TEST and TEST-NOT are predicates used for filtering which resources
  ;; to include in the database.  They are called with the name and value.
  (declare (type resource-database database)
	   (type (or pathname string stream) pathname)
	   (type (or null (function (string stream) t)) write)
	   (type (or null (function ((clx-list string) t) boolean))
                 test test-not)))

(defun root-resources (screen &key database key test test-not)
  "Returns a resource database containing the contents of the root window
   RESOURCE_MANAGER property for the given SCREEN. If SCREEN is a display,
   then its default screen is used. If an existing DATABASE is given, then
   resource values are merged with the DATABASE and the modified DATABASE is
   returned.

   TEST and TEST-NOT are predicates for selecting which resources are
   read.  Arguments are a resource name list and a resource value. The KEY
   function, if given, is called to convert a resource value string to the
   value given to TEST or TEST-NOT."

  (declare (type (or screen display) screen)
	   (type (or null resource-database) database)
	   (type (or null (function (string) t)) key)
	   (type (or null (function list boolean)) test test-not)
	   (clx-values resource-database)))

(defsetf root-resources (screen &key test test-not (write 'princ)) (database)
  "Changes the contents of the root window RESOURCE_MANAGER property for the
   given SCREEN. If SCREEN is a display, then its default screen is used. 

   TEST and TEST-NOT are predicates for selecting which resources from the
   DATABASE are written.  Arguments are a resource name list and a resource
   value.  The WRITE function is used to convert a resource value into a
   string stored in the property."

  (declare (type (or screen display) screen)
	(type (or null resource-database) database)
	(type (or null (function list boolean)) test test-not)
	(type (or null (function (string stream) t)) write)
	(clx-values resource-database)))

;;;-----------------------------------------------------------------------------
;;; Shared GContext's

(defmacro using-gcontext ((var &rest options &key drawable
			       function plane-mask foreground background
			       line-width line-style cap-style
			       join-style fill-style fill-rule arc-mode
			       tile stipple ts-x ts-y font
			       subwindow-mode exposures clip-x clip-y
			       clip-mask clip-ordering dash-offset
			       dashes)
			  &body body)
  ;; Equivalent to (let ((var (apply #'make-gcontext options))) ,@body)
  ;; but more efficient because it uses a gcontext cache associated with
  ;; drawable's display.
  )



 X11 Request Name       CLX Function Name
-----------------       -----------------
AllocColor              ALLOC-COLOR
AllocColorCells         ALLOC-COLOR-CELLS
AllocColorPlanes        ALLOC-COLOR-PLANES
AllocNamedColor         ALLOC-COLOR
AllowEvents             ALLOW-EVENTS
Bell                    BELL
ChangeAccessControl     (setf (ACCESS-CONTROL display) boolean)
ChangeActivePointerGrab CHANGE-ACTIVE-POINTER-GRAB
ChangeCloseDownMode     (setf (CLOSE-DOWN-MODE display) mode)
ChangeGC                FORCE-GCONTEXT-CHANGES
     ;; See WITH-GCONTEXT
     (setf (gcontext-function gc) boole-constant)
     (setf (gcontext-plane-mask gc) card32)
     (setf (gcontext-foreground gc) card32)
     (setf (gcontext-background gc) card32)
     (setf (gcontext-line-width gc) card16)
     (setf (gcontext-line-style gc) keyword)
     (setf (gcontext-cap-style gc) keyword)
     (setf (gcontext-join-style gc) keyword)
     (setf (gcontext-fill-style gc) keyword)
     (setf (gcontext-fill-rule gc) keyword)
     (setf (gcontext-tile gc) pixmap)
     (setf (gcontext-stipple gc) pixmap)
     (setf (gcontext-ts-x gc) int16) ;; Tile-Stipple-X-origin
     (setf (gcontext-ts-y gc) int16) ;; Tile-Stipple-Y-origin
     (setf (gcontext-font gc &optional metrics-p) font)
     (setf (gcontext-subwindow-mode gc) keyword)
     (setf (gcontext-exposures gc) (member :on :off))
     (setf (gcontext-clip-x gc) int16)
     (setf (gcontext-clip-y gc) int16)
     (setf (gcontext-clip-mask gc &optional ordering)
	   (or (member :none) pixmap rect-seq))
     (setf (gcontext-dash-offset gc) card16)
     (setf (gcontext-dashes gc) (or card8 sequence))
     (setf (gcontext-arc-mode gc) (member :chord :pie-slice))
     (setf (gcontext-clip-ordering gc) keyword)

ChangeHosts             ADD-ACCESS-HOST
ChangeHosts             REMOVE-ACCESS-HOST
ChangeKeyboardControl   CHANGE-KEYBOARD-CONTROL
ChangePointerControl    CHANGE-POINTER-CONTROL
ChangeProperty          CHANGE-PROPERTY
ChangeSaveSet           REMOVE-FROM-SAVE-SET
ChangeSaveSet           ADD-TO-SAVE-SET
ChangeWindowAttributes
     ;; See WITH-STATE
     (setf (window-background window) value)
     (setf (window-border window) value)
     (setf (window-bit-gravity window) value)
     (setf (window-gravity window) value)
     (setf (window-backing-store window) value)
     (setf (window-backing-planes window) value)
     (setf (window-backing-pixel window) value)
     (setf (window-override-redirect window) value)
     (setf (window-save-under window) value)
     (setf (window-colormap window) value)
     (setf (window-cursor window) value)
     (setf (window-event-mask window) value)
     (setf (window-do-not-propagate-mask window) value)

CirculateWindow         CIRCULATE-WINDOW-DOWN
CirculateWindow         CIRCULATE-WINDOW-UP
ClearToBackground       CLEAR-AREA
CloseFont               CLOSE-FONT
ConfigureWindow
     ;; See WITH-STATE
     (setf (drawable-x drawable) integer)
     (setf (drawable-y drawable) integer)
     (setf (drawable-width drawable) integer)
     (setf (drawable-height drawable) integer)
     (setf (drawable-depth drawable) integer)
     (setf (drawable-border-width drawable) integer)
     (setf (window-priority window &optional sibling) integer)

ConvertSelection        CONVERT-SELECTION
CopyArea                COPY-AREA
CopyColormapAndFree     COPY-COLORMAP-AND-FREE
CopyGC                  COPY-GCONTEXT
CopyGC                  COPY-GCONTEXT-COMPONENTS
CopyPlane               COPY-PLANE
CreateColormap          CREATE-COLORMAP
CreateCursor            CREATE-CURSOR
CreateGC                CREATE-GCONTEXT
CreateGlyphCursor       CREATE-GLYPH-CURSOR
CreatePixmap            CREATE-PIXMAP
CreateWindow            CREATE-WINDOW
DeleteProperty          DELETE-PROPERTY
DestroySubwindows       DESTROY-SUBWINDOWS
DestroyWindow           DESTROY-WINDOW
FillPoly                DRAW-LINES
ForceScreenSaver        RESET-SCREEN-SAVER
ForceScreenSaver        ACTIVATE-SCREEN-SAVER
FreeColormap            FREE-COLORMAP
FreeColors              FREE-COLORS
FreeCursor              FREE-CURSOR
FreeGC                  FREE-GCONTEXT
FreePixmap              FREE-PIXMAP
GetAtomName             ATOM-NAME
GetFontPath             FONT-PATH
GetGeometry             ;; See WITH-STATE
                        DRAWABLE-ROOT
                        DRAWABLE-X
                        DRAWABLE-Y
                        DRAWABLE-WIDTH
                        DRAWABLE-HEIGHT
                        DRAWABLE-DEPTH
                        DRAWABLE-BORDER-WIDTH

GetImage                GET-RAW-IMAGE
GetInputFocus           INPUT-FOCUS
GetKeyboardControl      KEYBOARD-CONTROL
GetKeyboardMapping      KEYBOARD-MAPPING
GetModifierMapping      MODIFIER-MAPPING
GetMotionEvents         MOTION-EVENTS
GetPointerControl       POINTER-CONTROL
GetPointerMapping       POINTER-MAPPING
GetProperty             GET-PROPERTY
GetScreenSaver          SCREEN-SAVER
GetSelectionOwner       SELECTION-OWNER
GetWindowAttributes     ;; See WITH-STATE
                        WINDOW-VISUAL-INFO
                        WINDOW-CLASS
                        WINDOW-BIT-GRAVITY
                        WINDOW-GRAVITY
                        WINDOW-BACKING-STORE
                        WINDOW-BACKING-PLANES
                        WINDOW-BACKING-PIXEL
                        WINDOW-SAVE-UNDER
                        WINDOW-OVERRIDE-REDIRECT
                        WINDOW-EVENT-MASK
                        WINDOW-DO-NOT-PROPAGATE-MASK
                        WINDOW-COLORMAP
                        WINDOW-COLORMAP-INSTALLED-P
                        WINDOW-ALL-EVENT-MASKS
                        WINDOW-MAP-STATE

GrabButton              GRAB-BUTTON
GrabKey                 GRAB-KEY
GrabKeyboard            GRAB-KEYBOARD
GrabPointer             GRAB-POINTER
GrabServer              GRAB-SERVER
ImageText16             DRAW-IMAGE-GLYPHS
ImageText16             DRAW-IMAGE-GLYPH
ImageText8              DRAW-IMAGE-GLYPHS
InstallColormap         INSTALL-COLORMAP
InternAtom              FIND-ATOM
InternAtom              INTERN-ATOM
KillClient              KILL-TEMPORARY-CLIENTS
KillClient              KILL-CLIENT
ListExtensions          LIST-EXTENSIONS
ListFonts               LIST-FONT-NAMES
ListFontsWithInfo       LIST-FONTS
ListHosts               ACCESS-CONTROL
ListHosts               ACCESS-HOSTS
ListInstalledColormaps  INSTALLED-COLORMAPS
ListProperties          LIST-PROPERTIES
LookupColor             LOOKUP-COLOR
MapSubwindows           MAP-SUBWINDOWS
MapWindow               MAP-WINDOW
OpenFont                OPEN-FONT
PolyArc                 DRAW-ARC
PolyArc                 DRAW-ARCS
PolyFillArc             DRAW-ARC
PolyFillArc             DRAW-ARCS
PolyFillRectangle       DRAW-RECTANGLE
PolyFillRectangle       DRAW-RECTANGLES
PolyLine                DRAW-LINE
PolyLine                DRAW-LINES
PolyPoint               DRAW-POINT
PolyPoint               DRAW-POINTS
PolyRectangle           DRAW-RECTANGLE
PolyRectangle           DRAW-RECTANGLES
PolySegment             DRAW-SEGMENTS
PolyText16              DRAW-GLYPH
PolyText16              DRAW-GLYPHS
PolyText8               DRAW-GLYPHS
PutImage                PUT-RAW-IMAGE
QueryBestSize           QUERY-BEST-CURSOR
QueryBestSize           QUERY-BEST-STIPPLE
QueryBestSize           QUERY-BEST-TILE
QueryColors             QUERY-COLORS
QueryExtension          QUERY-EXTENSION
QueryFont               FONT-NAME
                        FONT-NAME
                        FONT-DIRECTION
                        FONT-MIN-CHAR
                        FONT-MAX-CHAR
                        FONT-MIN-BYTE1
                        FONT-MAX-BYTE1
                        FONT-MIN-BYTE2
                        FONT-MAX-BYTE2
                        FONT-ALL-CHARS-EXIST-P
                        FONT-DEFAULT-CHAR
                        FONT-ASCENT
                        FONT-DESCENT
                        FONT-PROPERTIES
                        FONT-PROPERTY
     
                        CHAR-LEFT-BEARING
                        CHAR-RIGHT-BEARING
                        CHAR-WIDTH
                        CHAR-ASCENT
                        CHAR-DESCENT
                        CHAR-ATTRIBUTES
     
                        MIN-CHAR-LEFT-BEARING
                        MIN-CHAR-RIGHT-BEARING
                        MIN-CHAR-WIDTH
                        MIN-CHAR-ASCENT
                        MIN-CHAR-DESCENT
                        MIN-CHAR-ATTRIBUTES
     
                        MAX-CHAR-LEFT-BEARING
                        MAX-CHAR-RIGHT-BEARING
                        MAX-CHAR-WIDTH
                        MAX-CHAR-ASCENT
                        MAX-CHAR-DESCENT
                        MAX-CHAR-ATTRIBUTES

QueryKeymap             QUERY-KEYMAP
QueryPointer            GLOBAL-POINTER-POSITION
QueryPointer            POINTER-POSITION
QueryPointer            QUERY-POINTER
QueryTextExtents        TEXT-EXTENTS
QueryTextExtents        TEXT-WIDTH
QueryTree               QUERY-TREE
RecolorCursor           RECOLOR-CURSOR
ReparentWindow          REPARENT-WINDOW
RotateProperties        ROTATE-PROPERTIES
SendEvent               SEND-EVENT
SetClipRectangles       FORCE-GCONTEXT-CHANGES
     ;; See WITH-GCONTEXT
     (setf (gcontext-clip-x gc) int16)
     (setf (gcontext-clip-y gc) int16)
     (setf (gcontext-clip-mask gc &optional ordering)
	   (or (member :none) pixmap rect-seq))
     (setf (gcontext-clip-ordering gc) keyword)

SetDashes               FORCE-GCONTEXT-CHANGES
     ;; See WITH-GCONTEXT
     (setf (gcontext-dash-offset gc) card16)
     (setf (gcontext-dashes gc) (or card8 sequence))

SetFontPath
     (setf (font-path font) paths)
	Where paths is (type (clx-sequence (or string pathname)))

SetInputFocus           SET-INPUT-FOCUS
SetKeyboardMapping      CHANGE-KEYBOARD-MAPPING
SetModifierMapping      SET-MODIFIER-MAPPING
SetPointerMapping       SET-POINTER-MAPPING
SetScreenSaver          SET-SCREEN-SAVER
SetSelectionOwner       SET-SELECTION-OWNER
StoreColors             STORE-COLOR
StoreColors             STORE-COLORS
StoreNamedColor         STORE-COLOR
StoreNamedColor         STORE-COLORS
TranslateCoords         TRANSLATE-COORDINATES
UngrabButton            UNGRAB-BUTTON
UngrabKey               UNGRAB-KEY
UngrabKeyboard          UNGRAB-KEYBOARD
UngrabPointer           UNGRAB-POINTER
UngrabServer            UNGRAB-SERVER
UninstallColormap       UNINSTALL-COLORMAP
UnmapSubwindows         UNMAP-SUBWINDOWS
UnmapWindow             UNMAP-WINDOW
WarpPointer             WARP-POINTER
WarpPointer             WARP-POINTER-IF-INSIDE
WarpPointer             WARP-POINTER-RELATIVE
WarpPointer             WARP-POINTER-RELATIVE-IF-INSIDE
NoOperation             NO-OPERATION



 X11 Request Name       CLX Function Name
-----------------       -----------------
ListHosts               ACCESS-CONTROL
ListHosts               ACCESS-HOSTS
ForceScreenSaver        ACTIVATE-SCREEN-SAVER
ChangeHosts             ADD-ACCESS-HOST
ChangeSaveSet           ADD-TO-SAVE-SET
AllocColor              ALLOC-COLOR
AllocNamedColor         ALLOC-COLOR
AllocColorCells         ALLOC-COLOR-CELLS
AllocColorPlanes        ALLOC-COLOR-PLANES
AllowEvents             ALLOW-EVENTS
GetAtomName             ATOM-NAME
Bell                    BELL
ChangeActivePointerGrab CHANGE-ACTIVE-POINTER-GRAB
ChangeKeyboardControl   CHANGE-KEYBOARD-CONTROL
SetKeyboardMapping      CHANGE-KEYBOARD-MAPPING
ChangePointerControl    CHANGE-POINTER-CONTROL
ChangeProperty          CHANGE-PROPERTY
QueryFont               CHAR-ASCENT
QueryFont               CHAR-ATTRIBUTES
QueryFont               CHAR-DESCENT
QueryFont               CHAR-LEFT-BEARING
QueryFont               CHAR-RIGHT-BEARING
QueryFont               CHAR-WIDTH
CirculateWindow         CIRCULATE-WINDOW-DOWN
CirculateWindow         CIRCULATE-WINDOW-UP
ClearToBackground       CLEAR-AREA
CloseFont               CLOSE-FONT
ConvertSelection        CONVERT-SELECTION
CopyArea                COPY-AREA
CopyColormapAndFree     COPY-COLORMAP-AND-FREE
CopyGC                  COPY-GCONTEXT
CopyGC                  COPY-GCONTEXT-COMPONENTS
CopyPlane               COPY-PLANE
CreateColormap          CREATE-COLORMAP
CreateCursor            CREATE-CURSOR
CreateGC                CREATE-GCONTEXT
CreateGlyphCursor       CREATE-GLYPH-CURSOR
CreatePixmap            CREATE-PIXMAP
CreateWindow            CREATE-WINDOW
DeleteProperty          DELETE-PROPERTY
DestroySubwindows       DESTROY-SUBWINDOWS
DestroyWindow           DESTROY-WINDOW
PolyArc                 DRAW-ARC
PolyArc                 DRAW-ARCS
PolyText16              DRAW-GLYPH
PolyText16              DRAW-GLYPHS
PolyText8               DRAW-GLYPHS
ImageText16             DRAW-IMAGE-GLYPH
ImageText16             DRAW-IMAGE-GLYPHS
ImageText8              DRAW-IMAGE-GLYPHS
PolyLine                DRAW-LINE
PolyLine                DRAW-LINES
PolyPoint               DRAW-POINT
PolyPoint               DRAW-POINTS
PolyFillRectangle       DRAW-RECTANGLE
PolyRectangle           DRAW-RECTANGLE
PolyFillRectangle       DRAW-RECTANGLES
PolyRectangle           DRAW-RECTANGLES
PolySegment             DRAW-SEGMENTS
GetGeometry             DRAWABLE-BORDER-WIDTH
GetGeometry             DRAWABLE-DEPTH
GetGeometry             DRAWABLE-HEIGHT
GetGeometry             DRAWABLE-ROOT
GetGeometry             DRAWABLE-WIDTH
GetGeometry             DRAWABLE-X
GetGeometry             DRAWABLE-Y
FillPoly                FILL-POLYGON
InternAtom              FIND-ATOM
QueryFont               FONT-ALL-CHARS-EXIST-P
QueryFont               FONT-ASCENT
QueryFont               FONT-DEFAULT-CHAR
QueryFont               FONT-DESCENT
QueryFont               FONT-DIRECTION
QueryFont               FONT-MAX-BYTE1
QueryFont               FONT-MAX-BYTE2
QueryFont               FONT-MAX-CHAR
QueryFont               FONT-MIN-BYTE1
QueryFont               FONT-MIN-BYTE2
QueryFont               FONT-MIN-CHAR
QueryFont               FONT-NAME
QueryFont               FONT-NAME
GetFontPath             FONT-PATH
QueryFont               FONT-PROPERTIES
QueryFont               FONT-PROPERTY
ChangeGC                FORCE-GCONTEXT-CHANGES
SetClipRectangles       FORCE-GCONTEXT-CHANGES
SetDashes               FORCE-GCONTEXT-CHANGES
FreeColormap            FREE-COLORMAP
FreeColors              FREE-COLORS
FreeCursor              FREE-CURSOR
FreeGC                  FREE-GCONTEXT
FreePixmap              FREE-PIXMAP
GetProperty             GET-PROPERTY
GetImage                GET-RAW-IMAGE
QueryPointer            GLOBAL-POINTER-POSITION
GrabButton              GRAB-BUTTON
GrabKey                 GRAB-KEY
GrabKeyboard            GRAB-KEYBOARD
GrabPointer             GRAB-POINTER
GrabServer              GRAB-SERVER
GrabServer              WITH-SERVER-GRABBED
GetInputFocus           INPUT-FOCUS
InstallColormap         INSTALL-COLORMAP
ListInstalledColormaps  INSTALLED-COLORMAPS
InternAtom              INTERN-ATOM
GetKeyboardControl      KEYBOARD-CONTROL
GetKeyboardMapping      KEYBOARD-MAPPING
KillClient              KILL-CLIENT
KillClient              KILL-TEMPORARY-CLIENTS
ListExtensions          LIST-EXTENSIONS
ListFonts               LIST-FONT-NAMES
ListFontsWithInfo       LIST-FONTS
ListProperties          LIST-PROPERTIES
LookupColor             LOOKUP-COLOR
MapSubwindows           MAP-SUBWINDOWS
MapWindow               MAP-WINDOW
QueryFont               MAX-CHAR-ASCENT
QueryFont               MAX-CHAR-ATTRIBUTES
QueryFont               MAX-CHAR-DESCENT
QueryFont               MAX-CHAR-LEFT-BEARING
QueryFont               MAX-CHAR-RIGHT-BEARING
QueryFont               MAX-CHAR-WIDTH
QueryFont               MIN-CHAR-ASCENT
QueryFont               MIN-CHAR-ATTRIBUTES
QueryFont               MIN-CHAR-DESCENT
QueryFont               MIN-CHAR-LEFT-BEARING
QueryFont               MIN-CHAR-RIGHT-BEARING
QueryFont               MIN-CHAR-WIDTH
GetModifierMapping      MODIFIER-MAPPING
GetMotionEvents         MOTION-EVENTS
NoOperation             NO-OPERATION
OpenFont                OPEN-FONT
GetPointerControl       POINTER-CONTROL
GetPointerMapping       POINTER-MAPPING
QueryPointer            POINTER-POSITION
PutImage                PUT-RAW-IMAGE
QueryBestSize           QUERY-BEST-CURSOR
QueryBestSize           QUERY-BEST-STIPPLE
QueryBestSize           QUERY-BEST-TILE
QueryColors             QUERY-COLORS
QueryExtension          QUERY-EXTENSION
QueryKeymap             QUERY-KEYMAP
QueryPointer            QUERY-POINTER
QueryTree               QUERY-TREE
RecolorCursor           RECOLOR-CURSOR
ChangeHosts             REMOVE-ACCESS-HOST
ChangeSaveSet           REMOVE-FROM-SAVE-SET
ReparentWindow          REPARENT-WINDOW
ForceScreenSaver        RESET-SCREEN-SAVER
RotateProperties        ROTATE-PROPERTIES
GetScreenSaver          SCREEN-SAVER
GetSelectionOwner       SELECTION-OWNER
SendEvent               SEND-EVENT
ChangeAccessControl     SET-ACCESS-CONTROL
ChangeCloseDownMode     SET-CLOSE-DOWN-MODE
SetInputFocus           SET-INPUT-FOCUS
SetModifierMapping      SET-MODIFIER-MAPPING
SetPointerMapping       SET-POINTER-MAPPING
SetScreenSaver          SET-SCREEN-SAVER
SetSelectionOwner       SET-SELECTION-OWNER
StoreColors             STORE-COLOR
StoreColors             STORE-COLORS
StoreNamedColor         STORE-COLOR
StoreNamedColor         STORE-COLORS
QueryTextExtents        TEXT-EXTENTS
QueryTextExtents        TEXT-WIDTH
TranslateCoords         TRANSLATE-COORDINATES
UngrabButton            UNGRAB-BUTTON
UngrabKey               UNGRAB-KEY
UngrabKeyboard          UNGRAB-KEYBOARD
UngrabPointer           UNGRAB-POINTER
UngrabServer            UNGRAB-SERVER
UngrabServer            WITH-SERVER-GRABBED
UninstallColormap       UNINSTALL-COLORMAP
UnmapSubwindows         UNMAP-SUBWINDOWS
UnmapWindow             UNMAP-WINDOW
WarpPointer             WARP-POINTER
WarpPointer             WARP-POINTER-IF-INSIDE
WarpPointer             WARP-POINTER-RELATIVE
WarpPointer             WARP-POINTER-RELATIVE-IF-INSIDE
GetWindowAttributes     WINDOW-ALL-EVENT-MASKS
GetWindowAttributes     WINDOW-BACKING-PIXEL
GetWindowAttributes     WINDOW-BACKING-PLANES
GetWindowAttributes     WINDOW-BACKING-STORE
GetWindowAttributes     WINDOW-BIT-GRAVITY
GetWindowAttributes     WINDOW-CLASS
GetWindowAttributes     WINDOW-COLORMAP
GetWindowAttributes     WINDOW-COLORMAP-INSTALLED-P
GetWindowAttributes     WINDOW-DO-NOT-PROPAGATE-MASK
GetWindowAttributes     WINDOW-EVENT-MASK
GetWindowAttributes     WINDOW-GRAVITY
GetWindowAttributes     WINDOW-MAP-STATE
GetWindowAttributes     WINDOW-OVERRIDE-REDIRECT
GetWindowAttributes     WINDOW-SAVE-UNDER
GetWindowAttributes     WINDOW-VISUAL-INFO

ConfigureWindow         (SETF (DRAWABLE-BORDER-WIDTH DRAWABLE) INTEGER)
ConfigureWindow         (SETF (DRAWABLE-DEPTH DRAWABLE) INTEGER)
ConfigureWindow         (SETF (DRAWABLE-HEIGHT DRAWABLE) INTEGER)
ConfigureWindow         (SETF (DRAWABLE-WIDTH DRAWABLE) INTEGER)
ConfigureWindow         (SETF (DRAWABLE-X DRAWABLE) INTEGER)
ConfigureWindow         (SETF (DRAWABLE-Y DRAWABLE) INTEGER)
SetFontPath             (SETF (FONT-PATH FONT) PATHS)
ChangeGC                (SETF (GCONTEXT-ARC-MODE GC) (MEMBER CHORD PIE-SLICE))
ChangeGC                (SETF (GCONTEXT-BACKGROUND GC) CARD32)
ChangeGC                (SETF (GCONTEXT-CAP-STYLE GC) KEYWORD)
SetClipRectangles       (SETF (GCONTEXT-CLIP-MASK GC &OPTIONAL ORDERING)
	                      (OR (MEMBER NONE) PIXMAP RECT-SEQ))
SetClipRectangles       (SETF (GCONTEXT-CLIP-ORDERING GC) KEYWORD)
SetClipRectangles       (SETF (GCONTEXT-CLIP-X GC) INT16)
SetClipRectangles       (SETF (GCONTEXT-CLIP-Y GC) INT16)
SetDashes               (SETF (GCONTEXT-DASH-OFFSET GC) CARD16)
SetDashes               (SETF (GCONTEXT-DASHES GC) (OR CARD8 SEQUENCE))
ChangeGC                (SETF (GCONTEXT-EXPOSURES GC) (MEMBER ON OFF))
ChangeGC                (SETF (GCONTEXT-FILL-RULE GC) KEYWORD)
ChangeGC                (SETF (GCONTEXT-FILL-STYLE GC) KEYWORD)
ChangeGC                (SETF (GCONTEXT-FONT GC &OPTIONAL METRICS-P) FONT)
ChangeGC                (SETF (GCONTEXT-FOREGROUND GC) CARD32)
ChangeGC                (SETF (GCONTEXT-FUNCTION GC) BOOLE-CONSTANT)
ChangeGC                (SETF (GCONTEXT-JOIN-STYLE GC) KEYWORD)
ChangeGC                (SETF (GCONTEXT-LINE-STYLE GC) KEYWORD)
ChangeGC                (SETF (GCONTEXT-LINE-WIDTH GC) CARD16)
ChangeGC                (SETF (GCONTEXT-PLANE-MASK GC) CARD32)
ChangeGC                (SETF (GCONTEXT-STIPPLE GC) PIXMAP)
ChangeGC                (SETF (GCONTEXT-SUBWINDOW-MODE GC) KEYWORD)
ChangeGC                (SETF (GCONTEXT-TILE GC) PIXMAP)
ChangeGC                (SETF (GCONTEXT-TS-X GC) INT16)
ChangeGC                (SETF (GCONTEXT-TS-Y GC) INT16)
ChangeWindowAttributes  (SETF (WINDOW-BACKGROUND WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-BACKING-PIXEL WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-BACKING-PLANES WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-BACKING-STORE WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-BIT-GRAVITY WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-BORDER WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-COLORMAP WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-CURSOR WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-DO-NOT-PROPAGATE-MASK WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-EVENT-MASK WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-GRAVITY WINDOW) VALUE)
ChangeWindowAttributes  (SETF (WINDOW-OVERRIDE-REDIRECT WINDOW) VALUE)
ConfigureWindow         (SETF (WINDOW-PRIORITY WINDOW &OPTIONAL SIBLING) INTEGER)
ChangeWindowAttributes  (SETF (WINDOW-SAVE-UNDER WINDOW) VALUE)



;; Here's a list of the CLX functions that don't directly correspond to 
;; X Window System requests.  The've been categorized by function:

					   ;Display Management
CLOSE-DISPLAY
CLOSE-DOWN-MODE
DISPLAY-AFTER-FUNCTION ;; SETF'able
DISPLAY-FINISH-OUTPUT
DISPLAY-FORCE-OUTPUT
DISPLAY-INVOKE-AFTER-FUNCTION
OPEN-DISPLAY
WITH-DISPLAY
WITH-EVENT-QUEUE
					   ;Extensions
DECLARE-EVENT
DECODE-CORE-ERROR
DEFAULT-ERROR-HANDLER
DEFINE-CONDITION
DEFINE-ERROR
DEFINE-EXTENSION
DEFINE-GCONTEXT-ACCESSOR
EXTENSION-OPCODE
					   ;Events
EVENT-CASE
EVENT-LISTEN
MAPPING-NOTIFY
PROCESS-EVENT
EVENT-HANDLER
MAKE-EVENT-HANDLERS
QUEUE-EVENT
					   ;Image
COPY-IMAGE
CREATE-IMAGE
GET-IMAGE
IMAGE-BLUE-MASK
IMAGE-DEPTH
IMAGE-GREEN-MASK
IMAGE-HEIGHT
IMAGE-NAME
IMAGE-PIXMAP
IMAGE-PLIST
IMAGE-RED-MASK
IMAGE-WIDTH
IMAGE-X-HOT
IMAGE-Y-HOT
PUT-IMAGE
READ-BITMAP-FILE
WRITE-BITMAP-FILE
					   ;Keysyms
CHARACTER->KEYSYMS
CHARACTER-IN-MAP-P
DEFAULT-KEYSYM-INDEX
DEFAULT-KEYSYM-TRANSLATE
DEFINE-KEYSYM
DEFINE-KEYSYM-SET
KEYCODE->CHARACTER
KEYCODE->KEYSYM
KEYSYM
KEYSYM->CHARACTER
KEYSYM-IN-MAP-P
KEYSYM-SET
UNDEFINE-KEYSYM
					   ;Properties
CUT-BUFFER
GET-STANDARD-COLORMAP
GET-WM-CLASS
ICON-SIZES
MAKE-WM-HINTS
MAKE-WM-SIZE-HINTS
ROTATE-CUT-BUFFERS
SET-STANDARD-COLORMAP
SET-WM-CLASS
TRANSIENT-FOR
WM-CLIENT-MACHINE
WM-COMMAND
WM-HINTS
WM-HINTS-FLAGS
WM-HINTS-ICON-MASK
WM-HINTS-ICON-PIXMAP
WM-HINTS-ICON-WINDOW
WM-HINTS-ICON-X
WM-HINTS-ICON-Y
WM-HINTS-INITIAL-STATE
WM-HINTS-INPUT
WM-HINTS-P
WM-HINTS-WINDOW-GROUP
WM-ICON-NAME
WM-NAME
WM-NORMAL-HINTS
WM-SIZE-HINTS-HEIGHT
WM-SIZE-HINTS-HEIGHT-INC
WM-SIZE-HINTS-MAX-ASPECT
WM-SIZE-HINTS-MAX-HEIGHT
WM-SIZE-HINTS-MAX-WIDTH
WM-SIZE-HINTS-MIN-ASPECT
WM-SIZE-HINTS-MIN-HEIGHT
WM-SIZE-HINTS-MIN-WIDTH
WM-SIZE-HINTS-P
WM-SIZE-HINTS-USER-SPECIFIED-POSITION-P
WM-SIZE-HINTS-USER-SPECIFIED-SIZE-P
WM-SIZE-HINTS-WIDTH
WM-SIZE-HINTS-WIDTH-INC
WM-SIZE-HINTS-X
WM-SIZE-HINTS-Y
WM-ZOOM-HINTS
					   ;Misc.
MAKE-COLOR
MAKE-EVENT-KEYS
MAKE-EVENT-MASK
MAKE-RESOURCE-DATABASE
MAKE-STATE-KEYS
MAKE-STATE-MASK
DISCARD-FONT-INFO
TRANSLATE-DEFAULT
					   ;Structures
BITMAP-FORMAT-LSB-FIRST-P
BITMAP-FORMAT-P
BITMAP-FORMAT-PAD
BITMAP-FORMAT-UNIT
BITMAP-IMAGE

COLOR-BLUE
COLOR-GREEN
COLOR-P
COLOR-RED
COLOR-RGB
COLORMAP-DISPLAY
COLORMAP-EQUAL
COLORMAP-ID
COLORMAP-P
COLORMAP-VISUAL-INFO

CURSOR-DISPLAY
CURSOR-EQUAL
CURSOR-ID
CURSOR-P

DRAWABLE-DISPLAY
DRAWABLE-EQUAL
DRAWABLE-ID
DRAWABLE-P

FONT-DISPLAY
FONT-EQUAL
FONT-ID
FONT-MAX-BOUNDS
FONT-MIN-BOUNDS
FONT-P
FONT-PLIST

GCONTEXT-DISPLAY
GCONTEXT-EQUAL
GCONTEXT-ID
GCONTEXT-P
GCONTEXT-PLIST

DISPLAY-AUTHORIZATION-DATA
DISPLAY-AUTHORIZATION-NAME
DISPLAY-BITMAP-FORMAT
DISPLAY-BYTE-ORDER
DISPLAY-DEFAULT-SCREEN
DISPLAY-DISPLAY
DISPLAY-ERROR-HANDLER
DISPLAY-IMAGE-LSB-FIRST-P
DISPLAY-KEYCODE-RANGE
DISPLAY-MAX-KEYCODE
DISPLAY-MAX-REQUEST-LENGTH
DISPLAY-MIN-KEYCODE
DISPLAY-MOTION-BUFFER-SIZE
DISPLAY-NSCREENS
DISPLAY-P
DISPLAY-PIXMAP-FORMATS
DISPLAY-PLIST
DISPLAY-PROTOCOL-MAJOR-VERSION
DISPLAY-PROTOCOL-MINOR-VERSION
DISPLAY-PROTOCOL-VERSION
DISPLAY-RELEASE-NUMBER
DISPLAY-RESOURCE-ID-BASE
DISPLAY-RESOURCE-ID-MASK
DISPLAY-ROOTS
DISPLAY-SQUISH
DISPLAY-VENDOR
DISPLAY-VENDOR-NAME
DISPLAY-VERSION-NUMBER
DISPLAY-XDEFAULTS
DISPLAY-XID

PIXMAP-DISPLAY
PIXMAP-EQUAL
PIXMAP-FORMAT-BITS-PER-PIXEL
PIXMAP-FORMAT-DEPTH
PIXMAP-FORMAT-P
PIXMAP-FORMAT-SCANLINE-PAD
PIXMAP-ID
PIXMAP-P
PIXMAP-PLIST

SCREEN-BACKING-STORES
SCREEN-BLACK-PIXEL
SCREEN-DEFAULT-COLORMAP
SCREEN-DEPTHS
SCREEN-EVENT-MASK-AT-OPEN
SCREEN-HEIGHT
SCREEN-HEIGHT-IN-MILLIMETERS
SCREEN-MAX-INSTALLED-MAPS
SCREEN-MIN-INSTALLED-MAPS
SCREEN-P
SCREEN-PLIST
SCREEN-ROOT
SCREEN-ROOT-DEPTH
SCREEN-ROOT-VISUAL-INFO
SCREEN-SAVE-UNDERS-P
SCREEN-WHITE-PIXEL
SCREEN-WIDTH
SCREEN-WIDTH-IN-MILLIMETERS

VISUAL-INFO
VISUAL-INFO-BITS-PER-RGB
VISUAL-INFO-BLUE-MASK
VISUAL-INFO-CLASS
VISUAL-INFO-COLORMAP-ENTRIES
VISUAL-INFO-GREEN-MASK
VISUAL-INFO-ID
VISUAL-INFO-P
VISUAL-INFO-PLIST
VISUAL-INFO-RED-MASK

WINDOW-DISPLAY
WINDOW-EQUAL
WINDOW-ID
WINDOW-P
WINDOW-PLIST
