;;;; -*- Mode: Lisp, Fill ; Package: Toolkit -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please
;;; contact Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/prototypes.lisp,v 1.9 1998/12/20 04:22:54 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; This file contains the prototyping code for RPC requests between the
;;; Lisp client and the C server.
;;;

(in-package "TOOLKIT")

;;; This table is accessed at compile time so that we know the request ID as a
;;; numeric constant, and thus don't have to close over it at load time.  The
;;; same updates are done in parallel at compile and load time.
;;;
(eval-when (compile load eval) 
  (defparameter *request-table* 
    (make-array 50 :element-type 'simple-string
		:adjustable t :fill-pointer 0)))

(declaim (vector *request-table*))



;;;; Macros for defining toolkit request operations

(eval-when (compile eval)
(defmacro def-toolkit-request (string-name symbol-name options
			       doc-string args return &body forms)
  (declare (simple-string string-name doc-string)
	   (list args return))
  (let ((arg-list (mapcar #'car args))
	(type-list (mapcar #'cadr args))
	(code (fill-pointer *request-table*)))
    `(progn
       (eval-when (compile load eval)
	 (vector-push-extend (format nil "R~a" ,string-name) *request-table*))
       (defun ,symbol-name ,arg-list
	 ,doc-string
	 ;; *** This generates lots of warnings at the moment
	 ;; (declare (inline toolkit-write-value))
	 ,(cons 'declare
		(mapcar #'(lambda (arg type) (list 'type type arg))
			arg-list type-list))
	 (let ((message (prepare-request ,code ,options ,(length args))))
	   ,(cons 'progn
		  (mapcar #'(lambda (arg)
			      (let ((name (first arg))
				    (type (third arg)))
				(if type
				    (list 'toolkit-write-value 'message
					  name type)
				    (list 'toolkit-write-value 'message name))))
			  args))
	   (let ((reply (send-request-to-server message ,options)))
	     ,(if (eq options :confirm)
		  `(multiple-value-prog1
		       ,(ecase (length return)
			  (0)
			  (1 `(let ((result (toolkit-read-value reply)))
				,@forms
				result))
			  (2 `(let* ((first (toolkit-read-value reply))
				     (second (toolkit-read-value reply)))
				,@forms
				(values first second)))
			  (3 `(let* ((first (toolkit-read-value reply))
				     (second (toolkit-read-value reply))
				     (third (toolkit-read-value reply)))
				,@forms
				(values first second third)))
			  (4 `(let* ((first (toolkit-read-value reply))
				     (second (toolkit-read-value reply))
				     (third (toolkit-read-value reply))
				     (fourth (toolkit-read-value reply)))
				,@forms
				(values first second third fourth))))
		     (destroy-message reply))
		  '(declare (ignore reply)))))))))

;;; This is for sending commands to the server, not requesting Motif
;;; services.  These calls take no arguments and return a value indicating
;;; whether they were successful.

(defmacro def-toolkit-command (symbol-name options &body forms)
  (let ((string-name (symbol-class symbol-name))
	(code (fill-pointer *request-table*)))
    `(progn
       (eval-when (compile load eval)
	 (vector-push-extend ,string-name *request-table*)
	 (defun ,symbol-name ()
	   (let* ((message (prepare-request ,code ,options 0))
		  (reply (send-request-to-server message ,options)))
	     ,(if (eq options :confirm)
		  `(let ((result (toolkit-read-value reply)))
		     (destroy-message reply)
		     ,@forms
		     result)
		  '(declare (ignore reply)))))))))

); eval-when (compile eval)



;;;; Direct commands to server

(def-toolkit-command quit-server :no-confirm)

(def-toolkit-command terminate-callback :no-confirm)




;;;; Request definitions for Xt Intrinsic functions

(def-toolkit-request "TransportEvent" transport-event :confirm
  ""
  ((event-handle (unsigned-byte 32))) ((alien xevent)))

(def-toolkit-request "XtAppCreateShell" %create-application-shell :confirm
  ""
  ((resources list :resource-list))
  (widget)
  (setf (widget-type result) :application-shell))

(def-toolkit-request "XtRealizeWidget" realize-widget :no-confirm
  "Realizes the given widget."
  ((widget widget)) ())

(def-toolkit-request "XtCreateManagedWidget" %create-managed-widget :confirm
  ""
  ((name simple-string) (widget-class keyword) (parent widget)
   (resources list :resource-list))
  (widget)
  (setf (widget-type result) widget-class)
  (widget-add-child parent result))

(def-toolkit-request "XtCreateWidget" %create-widget :confirm
  ""
  ((name simple-string) (widget-class keyword) (parent widget)
   (resources list :resource-list))
  (widget)
  (setf (widget-type result) widget-class)
  (widget-add-child parent result))

(def-toolkit-request "XtAddCallback" %add-callback :no-confirm
  ""
  ((widget widget) (name simple-string)) ())

(def-toolkit-request "XtRemoveCallback"  %remove-callback :no-confirm
  ""
  ((widget widget) (name simple-string)) ())

(def-toolkit-request "XtSetValues" %set-values :no-confirm
  ""
  ((widget widget) (resources list :resource-list)) ())

(def-toolkit-request "XtGetValues" %get-values :confirm
  ""
  ((widget widget) (resource-names list :resource-names))
  (list))

(def-toolkit-request "XtUnrealizeWidget" unrealize-widget :no-confirm
  "Unrealizes the given widget."
  ((widget widget)) ())

;; Confirm because we rely on callbacks having been called by the time it
;; returns so we can blithely free our copies of the widget(s)
(def-toolkit-request "XtDestroyWidget" %destroy-widget :confirm
  ""
  ((widget widget)) ())

(def-toolkit-request "XtMapWidget" map-widget :no-confirm
  "Maps the X window associated with the given widget."
  ((widget widget)) ())

(def-toolkit-request "XtUnmapWidget" unmap-widget :no-confirm
  "Unmaps the X window associated with the given widget."
  ((widget widget)) ())

(def-toolkit-request "XtSetSensitive" set-sensitive :no-confirm
  "Sets the event sensitivity of the given widget."
  ((widget widget) (sensitivep (member t nil))) ())

(def-toolkit-request "XtCreatePopupShell" %create-popup-shell :confirm
  ""
  ((name simple-string) (class keyword)
   (parent widget) (resources list :resource-list))
  (widget)
  (setf (widget-type result) class)
  (widget-add-child parent result))

(def-toolkit-request "XtPopup" popup :no-confirm
  "Pops up a popup dialog shell."
  ((shell widget) (grab-kind keyword)) ())

(def-toolkit-request "XtPopdown" popdown :no-confirm
  "Pops down a popup dialog shell."
  ((shell widget)) ())

(def-toolkit-request "XtManageChild" manage-child :no-confirm
  "Manages the given child widget."
  ((child widget)) ())

(def-toolkit-request "XtUnmanageChild" unmanage-child :no-confirm
  "Unmanages the given child widget."
  ((child widget)) ())

(def-toolkit-request "XtManageChildren" %manage-children :no-confirm
  ""
  ((child-list list :widget-list)) ())

(def-toolkit-request "XtUnmanageChildren" %unmanage-children :no-confirm
  ""
  ((child-list list :widget-list)) ())

(def-toolkit-request "XtParseTranslationTable" parse-translation-table :confirm
  "Compiles a translation table string into its internal representation."
  ((table simple-string)) (translations))

(def-toolkit-request "XtAugmentTranslations" augment-translations :no-confirm
  "Augments the translation table of the specified widget with the given
   translations."
  ((w widget) (table translations)) ())

(def-toolkit-request "XtOverrideTranslations" override-translations :no-confirm
  "Overrides the translation table of the specified widget with the given
   translations."
  ((w widget) (table translations)) ())

(def-toolkit-request "XtUninstallTranslations" uninstall-translations
		     :no-confirm
  "Unintalls all translations on the given widget."
  ((w widget)) ())

(def-toolkit-request "XtParseAcceleratorTable" parse-accelerator-table :confirm
  "Parses an accelerator string into its internal representation."
  ((source simple-string)) (accelerators))

(def-toolkit-request "XtInstallAccelerators" install-accelerators :no-confirm
  "Installs accelerators from the source widget into the destination widget."
  ((dest widget) (src widget)) ())

(def-toolkit-request "XtInstallAllAccelerators" install-all-accelerators
		     :no-confirm
  "Installs all accelerators from the source widget into the destination
   widget."
  ((dest widget) (src widget)) ())

(def-toolkit-request "XtIsManaged" is-managed :confirm
  "Returns a value indicating whether the specified widget is managed or not."
  ((widget widget)) ((member t nil)))

(def-toolkit-request "XtPopupSpringLoaded" popup-spring-loaded :no-confirm
  "Pops up a spring loaded popup dialog shell."
  ((shell widget)) ())

(def-toolkit-request "XtIsRealized" is-realized :confirm
  "Returns a value indicating whether the specified widget is realized or not."
  ((widget widget)) ((member t nil)))

(def-toolkit-request "XtWindow" widget-window :confirm
  "Returns the X window associated with the given widget."
  ((widget widget)) (xlib:window))

(def-toolkit-request "XtName" widget-name :confirm
  "Returns the name of the given widget."
  ((widget widget)) (string))

(def-toolkit-request "XtIsSensitive" is-sensitive :confirm
  "Returns the sensitivity state of the given widget."
  ((widget widget)) ((member t nil)))

(defmacro def-xt-is-request (name)
  (let* ((string-name (concatenate 'string "Xt" (symbol-class name)))
         (class-name (subseq (symbol-class name) 2)))
    `(def-toolkit-request ,string-name ,name :confirm
      ,(concatenate 'string "is widget a subclass of " class-name)
      ((widget widget)) ((member t nil)))))

(def-xt-is-request is-application-shell)
(def-xt-is-request is-composite)
(def-xt-is-request is-constraint)
(def-xt-is-request is-object)
(def-xt-is-request is-override-shell)
(def-xt-is-request is-rect-obj)
(def-xt-is-request is-shell)
(def-xt-is-request is-top-level-shell)
(def-xt-is-request is-transient-shell)
(def-xt-is-request is-vendor-shell)
(def-xt-is-request is-w-m-shell)

(def-toolkit-request "XtNameToWidget" name-to-widget :confirm
  "find a widget by name"
  ((widget widget) (name simple-string))
  (widget))

(def-toolkit-request "XtParent" %widget-parent :confirm
  ""
  ((widget widget))
  (widget)
  (if (/= 0 (widget-id result))
      (widget-add-child result widget)
      ;; we leave the nil widget around so gets looked up faster in the future
      ;; only potential problem should be that it might accumulate parent or
      ;; child values in its slots or something
      (setf result nil)
      ))

(defun xt-widget-parent (widget)
  "Parent of widget, even if unknown to lisp"
  (or (widget-parent widget) (%widget-parent widget)))

(def-toolkit-request "XtAddEventHandler" %add-event-handler :no-confirm
  ""
  ((widget widget) (mask (unsigned-byte 32)) (nonmaskable_p (member t nil)))
  ())

(def-toolkit-request "XtRemoveEventHandler" %remove-event-handler :no-confirm
  ""
  ((widget widget) (mask (unsigned-byte 32)) (nonmaskable_p (member t nil)))
  ())

(def-toolkit-request "XtTranslateCoords" translate-coords :confirm
  "Translates coordinates (x,y) in the window of the given widget into the
   corresponding coordinates in the root window."
  ((widget widget) (x fixnum) (y fixnum))
  (fixnum fixnum))

(def-toolkit-request "XCreateFontCursor" create-font-cursor :confirm
  "Creates an X cursor from the standard cursor font."
  ((shape fixnum))
  (xlib:cursor))



;;;; Request definitions for Motif functions

;; We will ask for confirmation here just to resync things
(def-toolkit-request "XmUpdateDisplay" update-display :confirm
  "Processes all pending exposure events and synchronizes with the server."
  ((w widget)) ())

(def-toolkit-request "XmIsMotifWMRunning" is-motif-wm-running :confirm
  "Specifies if the MWM window manager is running."
  ((shell widget)) ((member t nil)))

(def-toolkit-request "XmMenuPosition" %menu-position :no-confirm
  ""
  ((widget widget) (event-handle (unsigned-byte 32) :event)) ())

(def-toolkit-request "XmCreateMenuBar" %create-menu-bar :confirm
  ""
  ((parent widget) (name simple-string) (resources list :resource-list))
  (widget)
  (setf (widget-type result) :row-column)
  (widget-add-child parent result))

(def-toolkit-request "XmCreateOptionMenu" %create-option-menu :confirm
  ""
  ((parent widget) (name simple-string) (resources list :resource-list))
  (widget)
  (setf (widget-type result) :row-column)
  (widget-add-child parent result))

(def-toolkit-request "XmCreateRadioBox" %create-radio-box :confirm
  ""
  ((parent widget) (name simple-string) (resources list :resource-list))
  (widget)
  (setf (widget-type result) :row-column)
  (widget-add-child parent result))

(macrolet ((def-double-widget-stub (name parent-class child-class)
	     (let ((strname (format nil "Xm~a"(symbol-class name)))
		   (fn-name (read-from-string
			     (format nil "%~a" name))))
	       `(def-toolkit-request ,strname ,fn-name :confirm
		  ""
		  ((parent widget) (name simple-string)
		   (resources list :resource-list))
		  (widget widget)
		  (setf (widget-type second) ,parent-class)
		  (setf (widget-type first) ,child-class)
		  (widget-add-child parent second)
		  (widget-add-child second first)))))
  
  (def-double-widget-stub create-warning-dialog :dialog-shell :message-box)
  (def-double-widget-stub create-bulletin-board-dialog :dialog-shell
    :bulletin-board)
  (def-double-widget-stub create-error-dialog :dialog-shell :message-box)
  (def-double-widget-stub create-file-selection-dialog :dialog-shell
    :file-selection-box)
  (def-double-widget-stub create-form-dialog :dialog-shell :form)
  (def-double-widget-stub create-information-dialog :dialog-shell
    :message-box)
  (def-double-widget-stub create-message-dialog :dialog-shell :message-box)
  (def-double-widget-stub create-popup-menu :menu-shell :row-column)
  (def-double-widget-stub create-prompt-dialog :dialog-shell :selection-box)
  (def-double-widget-stub create-pulldown-menu :menu-shell :row-column)
  (def-double-widget-stub create-question-dialog :dialog-shell :message-box)
  (def-double-widget-stub create-scrolled-list :scrolled-window :list)
  (def-double-widget-stub create-scrolled-text :scrolled-window :text)
  (def-double-widget-stub create-selection-dialog :dialog-shell
    :selection-box)
  (def-double-widget-stub create-working-dialog :dialog-shell :message-box))

(def-toolkit-request "XmCommandAppendValue" command-append-value :no-confirm
  "Appends the given string to the end of the string displayed in the
   command area of the widget."
  ((w widget) (command (or simple-string xmstring))) ())

(def-toolkit-request "XmCommandError" command-error :no-confirm
  "Displays an error message in the Command widget."
  ((w widget) (error (or simple-string xmstring))) ())

(def-toolkit-request "XmCommandSetValue" command-set-value :no-confirm
  "Replaces the displayed string in a Command widget."
  ((w widget) (c (or simple-string xmstring))) ())

(def-toolkit-request "XmScaleGetValue" scale-get-value :confirm
  "Returns the current slider position."
  ((w widget)) (fixnum))

(def-toolkit-request "XmScaleSetValue" scale-set-value :no-confirm
  "Sets the current slider position."
  ((w widget) (val fixnum)) ())

(def-toolkit-request "XmToggleButtonGetState" toggle-button-get-state :confirm
  "Obtains the state of a ToggleButton."
  ((w widget)) ((member t nil)))

(def-toolkit-request "XmToggleButtonSetState" toggle-button-set-state
		     :no-confirm
  "Sets the state of a ToggleButton."
  ((w widget) (state (member t nil)) (notify (member t nil))) ())

(def-toolkit-request "XmListAddItem" list-add-item :no-confirm
  "Adds an item to the given List widget."
  ((w widget) (item (or simple-string xmstring)) (pos fixnum)) ())

(def-toolkit-request "XmListAddItemUnselected" list-add-item-unselected
		     :no-confirm
  "Adds an item to the List widget as an unselected entry."
  ((w widget) (item (or simple-string xmstring)) (pos fixnum)) ())

(def-toolkit-request "XmListDeleteItem" list-delete-item :no-confirm
  "Deletes an item from the given List widget."
  ((w widget) (item (or simple-string xmstring))) ())

(def-toolkit-request "XmListDeletePos" list-delete-pos :no-confirm
  "Deletes and item from a List widget at the specified position."
  ((w widget) (pos fixnum)) ())

(def-toolkit-request "XmListDeselectAllItems" list-deselect-all-items
		     :no-confirm
  "Unhighlights and removes all items from the selected list."
  ((w widget)) ())

(def-toolkit-request "XmListDeselectItem" list-deselect-item :no-confirm
  "Deselects the specified item from the selected list."
  ((w widget) (item (or simple-string xmstring))) ())

(def-toolkit-request "XmListDeselectPos" list-deselect-pos :no-confirm
  "Deselects an item at a specified position in a List widget."
  ((w widget) (pos fixnum)) ())

(def-toolkit-request "XmListSelectItem" list-select-item :no-confirm
  "Selects an item in the List widget."
  ((w widget) (item (or simple-string xmstring)) (notify (member t nil))) ())

(def-toolkit-request "XmListSelectPos" list-select-pos :no-confirm
  "Selects an item at a specified position in the List widget."
  ((w widget) (pos fixnum) (notify (member t nil))) ())

(def-toolkit-request "XmListSetBottomItem" list-set-bottom-item :no-confirm
  "Makes an existing item the last visible in the List widget."
  ((w widget) (item (or simple-string xmstring))) ())

(def-toolkit-request "XmListSetBottomPos" list-set-bottom-pos :no-confirm
  "Makes the item at the specified position the last visible item in the
   given List widget."
  ((w widget) (pos fixnum)) ())

(def-toolkit-request "XmListSetHorizPos" list-set-horiz-pos :no-confirm
  "Scrolls to the specified position in the List widget."
  ((w widget) (pos fixnum)) ())

(def-toolkit-request "XmListSetItem" list-set-item :no-confirm
  "Makes an existing item the first visible in the List widget."
  ((w widget) (item (or simple-string xmstring))) ())

(def-toolkit-request "XmListSetPos" list-set-pos :no-confirm
  "Makes the item at the given position the first visible item in the List."
  ((w widget) (pos fixnum)) ())

(def-toolkit-request "XmListAddItems" list-add-items :no-confirm
  "Adds items to the given List widget."
  ((w widget) (items list :xm-string-table) (pos fixnum)) ())

(def-toolkit-request "XmListDeleteAllItems" list-delete-all-items :no-confirm
  "Deletes all items from the List widget."
  ((w widget)) ())

(def-toolkit-request "XmListDeleteItems" list-delete-items :no-confirm
  "Deletes specified items from the List widget."
  ((w widget) (items list :xm-string-table)) ())

(def-toolkit-request "XmListDeleteItemsPos" list-delete-items-pos :no-confirm
  "Deletes items from the list starting at the given position."
  ((w widget) (item-count fixnum) (pos fixnum)) ())

(def-toolkit-request "XmListItemExists" list-item-exists :confirm
  "Checks if a specified item is in the List widget."
  ((w widget) (item (or simple-string xmstring))) ((member t nil)))

(def-toolkit-request "XmListItemPos" list-item-pos :confirm
  "Returns the position of an item in the List widget."
  ((w widget) (item (or simple-string xmstring))) (fixnum))

(def-toolkit-request "XmListReplaceItems" list-replace-items :no-confirm
  "Replaces the specified elements in the list."
  ((w widget) (old list :xm-string-table) (new list :xm-string-table)) ())

(def-toolkit-request "XmListReplaceItemsPos" list-replace-items-pos :no-confirm
  "Replaces items in the list, starting at the given position."
  ((w widget) (new-items list :xm-string-table) (pos fixnum)) ())

(def-toolkit-request "XmListSetAddMode" list-set-add-mode :no-confirm
  "Sets the state of Add Mode in the list."
  ((w widget) (mode (member t nil))) ())

(def-toolkit-request "XmListGetSelectedPos" list-get-selected-pos :confirm
  "Returns the position of every selected item in the given List."
  ((w widget))
  (list (member t nil)))

(def-toolkit-request "XmAddTabGroup" add-tab-group :no-confirm
  "Adds a manager or a primitive widget to the list of tab groups."
  ((w widget)) ())

(def-toolkit-request "XmRemoveTabGroup" remove-tab-group :no-confirm
  "Removes a manager or a primitive widget from the list of tab groups."
  ((w widget)) ())

(def-toolkit-request "XmProcessTraversal" process-traversal :confirm
  "Determines which component of a widget hierarchy receives keyboard
   events when a widget has the keyboard focus."
  ((w widget) (direction keyword)) ((member t nil)))

(def-toolkit-request "XmFontListAdd" font-list-add :confirm
  "Adds a new font to a font-list and destroys the old list."
  ((flist font-list) (font xlib:font) (charset simple-string))
  (font-list))

(def-toolkit-request "XmFontListCreate" font-list-create :confirm
  "Creates a new font-list with the specified font."
  ((font xlib:font) (charset simple-string))
  (font-list))

(def-toolkit-request "XmFontListFree" font-list-free :no-confirm
  "Destroys the given font-list."
  ((flist font-list)) ())

(def-toolkit-request "XmStringBaseline" compound-string-baseline :confirm
  "Returns the number of pixels between the top of the character box and
   the basline of the first line of text."
  ((flist font-list) (string xmstring)) (fixnum))

(def-toolkit-request "XmStringByteCompare" compound-string-byte-compare
		     :confirm
  "Indicates the result of a byte-by-byte comparison of two compound strings."
  ((s1 xmstring) (s2 xmstring)) ((member t nil)))

(def-toolkit-request "XmStringCompare" compound-string-compare :confirm
  "Indicates whether two compound strings are semantically equivalent."
  ((s1 xmstring) (s2 xmstring)) ((member t nil)))

(def-toolkit-request "XmStringConcat" compound-string-concat :confirm
  "Appends one compound string to another.  The original strings are preserved."
  ((s1 xmstring) (s2 xmstring)) (xmstring))

(def-toolkit-request "XmStringCopy" compound-string-copy :confirm
  "Makes a copy of a compound string."
  ((s xmstring)) (xmstring))

(def-toolkit-request "XmStringCreate" compound-string-create :confirm
  "Creates a new compound string."
  ((s simple-string) (charset simple-string)) (xmstring))

(def-toolkit-request "XmStringCreateLtoR" compound-string-create-ltor :confirm
  "Creates a new compound string and translates newline characters into
   line separators."
  ((s simple-string) (charset simple-string)) (xmstring))

(def-toolkit-request "XmStringGetLtoR" compound-string-get-ltor :confirm
  "Returns True if a segment can be found in the input compound string that
   matches the specified character set."
  ((string xmstring) (charset simple-string))
  (simple-string (member t nil)))

(def-toolkit-request "XmStringCreateSimple" compound-string-create-simple
		     :confirm
  "Creates a compound string in the language environment of a widget."
  ((s simple-string)) (xmstring))

(def-toolkit-request "XmStringEmpty" compound-string-empty :confirm
  "Provides information on the existence of non-zero length text components."
  ((s xmstring)) ((member t nil)))

(def-toolkit-request "XmStringExtent" compound-string-extent :confirm
  "Determines the size of the smallest rectangle that will enclose the
   given compound string."
  ((flist font-list) (x xmstring)) (fixnum fixnum))

(def-toolkit-request "XmStringFree" compound-string-free :no-confirm
  "Recovers memory used by a compound string."
  ((s xmstring)) ()
  (remhash (xti::motif-object-id s)
	   (xti::motif-connection-id-table *motif-connection*)))

(def-toolkit-request "XmStringHasSubstring" compound-string-has-substring
		     :confirm
  "Indicates whether one compound string is contained within another."
  ((string xmstring) (substring xmstring)) ((member t nil)))

(def-toolkit-request "XmStringHeight" compound-string-height :confirm
  "Returns the line height of the given compound string."
  ((flist font-list) (string xmstring)) (fixnum))

(def-toolkit-request "XmStringLength" compound-string-length :confirm
  "Obtains the length of a compound string."
  ((string xmstring)) (fixnum))

(def-toolkit-request "XmStringLineCount" compound-string-line-count :confirm
  "Returns the number of separators plus one in the provided compound string."
  ((string xmstring)) (fixnum))

(def-toolkit-request "XmStringNConcat" compound-string-nconcat :confirm
  "Appends a specified number of bytes to a compound string."
  ((s1 xmstring) (s2 xmstring) (num_bytes fixnum)) (xmstring))

(def-toolkit-request "XmStringNCopy" compound-string-ncopy :confirm
  "Copies a specified number of bytes into a new compound string."
  ((s string) (num_bytes fixnum)) (xmstring))

(def-toolkit-request "XmStringSeparatorCreate" compound-string-separator-create
		     :confirm
  "Creates a compound string with a single component, a separator."
  () (xmstring))

(def-toolkit-request "XmStringWidth" compound-string-width :confirm
  "Returns the width of the longest sequence of text components in a
   compound string."
  ((flist font-list) (s xmstring)) (fixnum))

(def-toolkit-request "XmTextClearSelection" text-clear-selection :no-confirm
  "Clears the primary selection."
  ((w widget)) ())

(def-toolkit-request "XmTextCopy" text-copy :confirm
  "Copies the primary selection to the clipboard."
  ((w widget)) ((member t nil)))

(def-toolkit-request "XmTextCut" text-cut :confirm
  "Copies the primary selection to the clipboard and deletes the selected text."
  ((w widget)) ((member t nil)))

(def-toolkit-request "XmTextGetBaseline" text-get-baseline :confirm
  "Accesses the x position of the first baseline."
  ((w widget)) (fixnum))

(def-toolkit-request "XmTextGetEditable" text-get-editable :confirm
  "Accesses the edit permission state of the Text widget."
  ((w widget)) ((member t nil)))

(def-toolkit-request "XmTextGetInsertionPosition" text-get-insertion-position 
		     :confirm
  "Accesses the positions of the insert cursor."
  ((w widget)) (fixnum))

(def-toolkit-request "XmTextGetLastPosition" text-get-last-position :confirm
  "Accesses the positio of the last text character."
  ((w widget)) (fixnum))

(def-toolkit-request "XmTextGetMaxLength" text-get-max-length :confirm
  "Accesses the value of the current maximum allowable length of a text
   string entered from the keyboard."
  ((w widget)) (fixnum))

(def-toolkit-request "XmTextGetSelection" text-get-selection :confirm
  "Retrieves the value of the primary selection."
  ((w widget)) (simple-string))

(def-toolkit-request "XmTextGetSelectionPosition" text-get-selection-position
		     :confirm
  "Accesses the position of the primary selection."
  ((w widget))
  ((member t nil) fixnum fixnum))

(def-toolkit-request "XmTextGetString" text-get-string :confirm
  "Accesses the string value of a Text widget."
  ((w widget)) (simple-string))

(def-toolkit-request "XmTextGetTopCharacter" text-get-top-character :confirm
  "Accesses the position of the first character displayed."
  ((w widget)) (fixnum))

(def-toolkit-request "XmTextInsert" text-insert :no-confirm
  "Inserts a character string into a Text widget."
  ((w widget) (pos fixnum) (value simple-string)) ())

(def-toolkit-request "XmTextPaste" text-paste :confirm
  "Inserts the clipboard selection."
  ((w widget)) ((member t nil)))

(def-toolkit-request "XmTextPosToXY" text-pos-to-xy :confirm
  "Accesses the x and y position of a character position."
  ((w widget) (pos fixnum))
  ((member t nil) fixnum fixnum))

(def-toolkit-request "XmTextRemove" text-remove :confirm
  "Deletes the primary selection."
  ((w widget)) ((member t nil)))

(def-toolkit-request "XmTextReplace" text-replace :no-confirm
  "Replaces part of the text of a Text widget."
  ((w widget) (from-pos fixnum) (to-pos fixnum) (value simple-string)) ())

(def-toolkit-request "XmTextScroll" text-scroll :no-confirm
  "Scrolls the text of a Text widget."
  ((w widget) (lines fixnum)) ())

(def-toolkit-request "XmTextSetAddMode" text-set-add-mode :no-confirm
  "Sets the state of Add Mode."
  ((w widget) (state (member t nil))) ())

(def-toolkit-request "XmTextSetEditable" text-set-editable :no-confirm
  "Sets the edit permission on a Text widget."
  ((w widget) (editable (member t nil))) ())

(def-toolkit-request "XmTextSetHighlight" text-set-highlight :no-confirm
  "Highlights text."
  ((w widget) (left fixnum) (right fixnum) (mode keyword)) ())

(def-toolkit-request "XmTextSetInsertionPosition" text-set-insertion-position
		     :no-confirm
  "Sets position of the insert cursor."
  ((w widget) (pos fixnum)) ())

(def-toolkit-request "XmTextSetMaxLength" text-set-max-length :no-confirm
  "Sets the value of the current maximum allowable length of a text string
   entered from the keyboard."
  ((w widget) (max-length fixnum)) ())

(def-toolkit-request "XmTextSetSelection" text-set-selection :no-confirm
  "Sets the primary selection of the Text widget."
  ((w widget) (first fixnum) (last fixnum)) ())

(def-toolkit-request "XmTextSetString" text-set-string :no-confirm
  "Sets the string value of a Text widget."
  ((w widget) (value simple-string)) ())

(def-toolkit-request "XmTextSetTopCharacter" text-set-top-character :no-confirm
  "Sets the position of the first character displayed."
  ((w widget) (top-char fixnum)) ())

(def-toolkit-request "XmTextShowPosition" text-show-position :no-confirm
  "Forces text at a given position to be displayed."
  ((w widget) (pos fixnum)) ())

(def-toolkit-request "XmTextXYToPos" text-xy-to-pos :confirm
  "Accesses the character position nearest an x and y position."
  ((w widget) (x fixnum) (y fixnum)) (fixnum))

(def-toolkit-request "XmAddProtocolCallback" %add-protocol-callback
		     :no-confirm
  ""
  ((w widget) (property keyword :atom) (protocol keyword :atom)) ())

(def-toolkit-request "XmRemoveProtocolCallback" %remove-protocol-callback
		     :no-confirm
  ""
  ((w widget) (property keyword :atom) (protocol keyword :atom)) ())

(def-toolkit-request "XmSelectionBoxGetChild" selection-box-get-child :confirm
  "Accesses a child component of a SelectionBox widget."
  ((w widget) (child keyword)) (widget)
  (widget-add-child w result)
  (setf (widget-type result) :unknown))

(def-toolkit-request "XmFileSelectionBoxGetChild" file-selection-box-get-child
		     :confirm
  "Accesses a child component of a FileSelectionBox widget."
  ((w widget) (child keyword)) (widget)
  (widget-add-child w result)
  (setf (widget-type result) :unknown))

(def-toolkit-request "XmMessageBoxGetChild" message-box-get-child :confirm
  "Accesses a child component of a MessageBox widget."
  ((w widget) (child keyword)) (widget)
  (widget-add-child w result)
  (setf (widget-type result) :unknown))

(def-toolkit-request "XmCommandGetChild" command-get-child :confirm
  "Accesses a child component of a Command widget."
  ((w widget) (child keyword))
  (widget)
  (widget-add-child w result)
  (setf (widget-type result) :unknown))

(def-toolkit-request "XmScrolledWindowSetAreas" scrolled-window-set-areas
		     :no-confirm
  "Adds or changes a window work region and a horizontal or vertical
   ScrollBar widget to the ScrolledWindow widget."
  ((widget widget) (horiz-scroll (or null widget))
   (vert-scroll (or null widget)) (work-region (or null widget)))
  ())

(def-toolkit-request "XmTrackingLocate" tracking-locate :confirm
  "Provides a modal interface for the selection of a component."
  ((w widget) (cursor xlib:cursor) (confine-to (member t nil)))
  (widget))

(def-toolkit-request "XmScrollBarGetValues" scroll-bar-get-values :confirm
  "Returns the ScrollBar's increment values."
  ((widget widget))
  (fixnum fixnum fixnum fixnum))

(def-toolkit-request "XmScrollBarSetValues" scroll-bar-set-values :no-confirm
  "Changes the ScrollBar's increments values and the slider's size and
   position."
  ((widget widget) (value fixnum) (slider-size fixnum)
   (increment fixnum) (page-increment fixnum) (notify (member t nil)))
  ())

(def-toolkit-request "SetItems" set-items :no-confirm
  "Set the items of a List widget."
  ((widget widget) (items list :xm-string-table)) ())

(def-toolkit-request "GetItems" get-items :confirm
  "Get the items of a List widget."
  ((widget widget))
  (list))

(def-toolkit-request "ReturnTextCallbackDoit" return-text-callback-doit
		     :no-confirm
  "Return a boolean value determining whether the proposed text action will
   actually be performed."
  ((doit (member t nil)))
  ())
