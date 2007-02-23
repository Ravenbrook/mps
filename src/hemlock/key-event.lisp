;;; -*- Log: hemlock.log; Package: extensions -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/key-event.lisp,v 1.5 2001/03/13 15:49:53 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file implements key-events for representing editor input.  It also
;;; provides a couple routines to interface this to X11.
;;;
;;; Written by Blaine Burks and Bill Chiles.
;;;

;;; The following are the implementation dependent parts of this code (what
;;; you would have to change if you weren't using X11):
;;;    *modifier-translations*
;;;    DEFINE-CLX-MODIFIER
;;;    TRANSLATE-KEY-EVENT
;;;    TRANSLATE-MOUSE-KEY-EVENT
;;;    DEFINE-KEYSYM
;;;    DEFINE-MOUSE-KEYSYM
;;;    DO-ALPHA-KEY-EVENTS
;;; If the window system didn't use a keysym mechanism to represent keys, you
;;; would also need to write something that mapped whatever did encode the
;;; keys to the keysyms defined with DEFINE-KEYSYM.
;;;

(in-package "EXTENSIONS")

(export '( define-keysym define-mouse-keysym name-keysym keysym-names
	   keysym-preferred-name define-key-event-modifier define-clx-modifier
	   make-key-event-bits key-event-modifier-mask key-event-bits-modifiers
	   *all-modifier-names* translate-key-event translate-mouse-key-event
	   make-key-event key-event key-event-p key-event-bits key-event-keysym
	   char-key-event key-event-char key-event-bit-p do-alpha-key-events
	   print-pretty-key print-pretty-key-event))



;;;; Keysym <==> Name translation.

;;; Keysyms are named by case-insensitive names.  However, if the name
;;; consists of a single character, the name is case-sensitive.
;;;

;;; This table maps a keysym to a list of names.  The first name is the
;;; preferred printing name.
;;;
(defvar *keysyms-to-names*)
 
;;; This table maps all keysym names to the appropriate keysym.
;;;
(defvar *names-to-keysyms*)

(declaim (inline name-keysym keysym-names keysym-preferred-name))

(defun name-keysym (name)
  "This returns the keysym named name.  If name is unknown, this returns nil."
  (gethash (get-name-case-right name) *names-to-keysyms*))

(defun keysym-names (keysym)
  "This returns the list of all names for keysym.  If keysym is undefined,
   this returns nil."
  (gethash keysym *keysyms-to-names*))

(defun keysym-preferred-name (keysym)
  "This returns the preferred name for keysym, how it is typically printed.
   If keysym is undefined, this returns nil."
  (car (gethash keysym *keysyms-to-names*)))



;;;; Character key-event stuff.

;;; GET-NAME-CASE-RIGHT -- Internal.
;;;
;;; This returns the canonical string for a keysym name for use with
;;; hash tables.
;;;
(defun get-name-case-right (string)
  (if (= (length string) 1) string (string-downcase string)))

;;; DEFINE-KEYSYM -- Public.
;;;
(defun define-keysym (keysym preferred-name &rest other-names)
  "This establishes a mapping from preferred-name to keysym for purposes of
   specifying key-events in #k syntax.  Other-names also map to keysym, but the
   system uses preferred-name when printing key-events.  The names are
   case-insensitive simple-strings.  Redefining a keysym or re-using names has
   undefined effects."
  (setf (gethash keysym *keysyms-to-names*) (cons preferred-name other-names))
  (dolist (name (cons preferred-name other-names))
    (setf (gethash (get-name-case-right name) *names-to-keysyms*) keysym)))

;;; This is an a-list mapping CLX modifier masks to defined key-event
;;; modifier names.  DEFINE-CLX-MODIFIER fills this in, so TRANSLATE-KEY-EVENT
;;; and TRANSLATE-MOUSE-KEY-EVENT can work.
;;;
(defvar *modifier-translations*)

;;; This is an ordered a-list mapping defined key-event modifier names to the
;;; appropriate mask for the modifier.  Modifier names have a short and a long
;;; version.  For each pair of names for the same mask, the names are
;;; contiguous in this list, and the short name appears first.
;;; PRINT-PRETTY-KEY-EVENT and KEY-EVENT-BITS-MODIFIERS rely on this.
;;;
(defvar *modifiers-to-internal-masks*)

;;; TRANSLATE-KEY-EVENT -- Public.
;;;
#+clx
(defun translate-key-event (display scan-code bits)
  "Translates the X scan-code and X bits to a key-event.  First this maps
   scan-code to an X keysym using XLIB:KEYCODE->KEYSYM looking at bits and
   supplying index as 1 if the X shift bit is on, 0 otherwise.

   If the resulting keysym is undefined, and it is not a modifier keysym, then
   this signals an error.  If the keysym is a modifier key, then this returns
   nil.

   If the following conditions are satisfied
      the keysym is defined
      the X shift bit is off
      the X lock bit is on
      the X keysym represents a lowercase letter
   then this maps the scan-code again supplying index as 1 this time, treating
   the X lock bit as a caps-lock bit.  If this results in an undefined keysym,
   this signals an error.  Otherwise, this makes a key-event with the keysym
   and bits formed by mapping the X bits to key-event bits.

   If any state bit is set that has no suitable modifier translation, it is
   passed to XLIB:DEFAULT-KEYSYM-INDEX in order to handle Mode_Switch keys
   appropriately.

   Otherwise, this makes a key-event with the keysym and bits formed by mapping
   the X bits to key-event bits."
  (let ((new-bits 0)
	shiftp lockp)
    (dolist (map *modifier-translations*)
      (unless (zerop (logand (car map) bits))
	;; ignore the bits of the mapping for the determination of a key index
	(setq bits (logxor bits (car map)))
	(cond
	 ((string-equal (cdr map) "Shift")
	  (setf shiftp t))
	 ((string-equal (cdr map) "Lock")
	  (setf lockp t))
	 (t (setf new-bits
		  (logior new-bits (key-event-modifier-mask (cdr map))))))))
    ;; here pass any remaining modifier bits to clx
    (let* ((index  (and (not (zerop bits))
			(xlib:default-keysym-index display scan-code bits)))
	   (keysym (xlib:keycode->keysym display scan-code (or index (if shiftp 1 0)))))
      (cond ((null (keysym-names keysym))
	     nil)
	    ((and (not shiftp) lockp (<= 97 keysym 122)) ; small-alpha-char-p
	     (let ((keysym (xlib:keycode->keysym display scan-code 1)))
	       (if (keysym-names keysym)
		   (make-key-event keysym new-bits)
		   nil)))
	    (t
	     (make-key-event keysym new-bits))))))



;;;; Mouse key-event stuff.

;;; Think of this data as a three dimensional array indexed by the following
;;; domains:
;;;    1-5
;;;       for the mouse scan-codes (button numbers) delivered by X.
;;;    :button-press or :button-release
;;;       whether the button was pressed or released.
;;;    :keysym or :shifted-modifier-name
;;;       whether the X shift bit was set.
;;; For each button, pressed and released, we store a keysym to be used in a
;;; key-event representing the button and whether it was pressed or released.
;;; We also store a modifier name that TRANSLATE-MOUSE-KEY-EVENT turns on
;;; whenever a mouse event occurs with the X shift bit on.  This is basically
;;; an archaic feature since we now can specify key-events like the following:
;;;    #k"shift-leftdown"
;;; Previously we couldn't, so we mapped the shift bit to a bit we could
;;; talke about, such as super.
;;;
(defvar *mouse-translation-info*)

(eval-when (compile eval)
  (defmacro button-press-info (event-dispatch) `(car ,event-dispatch))
  (defmacro button-release-info (event-dispatch) `(cdr ,event-dispatch))
  (defmacro button-keysym (info) `(car ,info))
  (defmacro button-shifted-modifier-name (info) `(cdr ,info))
) ;eval-when

;;; MOUSE-TRANSLATION-INFO -- Internal.
;;;
;;; This returns the requested information, :keysym or :shifted-modifier-name,
;;; for the button cross event-key.  If the information is undefined, this
;;; signals an error.
;;;
(defun mouse-translation-info (button event-key info)
  (let ((event-dispatch (svref *mouse-translation-info* button)))
    (unless event-dispatch
      (error "No defined mouse translation information for button ~S." button))
    (let ((data (ecase event-key
		  (:button-press (button-press-info event-dispatch))
		  (:button-release (button-release-info event-dispatch)))))
      (unless data
	(error
	 "No defined mouse translation information for button ~S and event ~S."
	 button event-key))
      (ecase info
	(:keysym (button-keysym data))
	(:shifted-modifier-name (button-shifted-modifier-name data))))))

;;; %SET-MOUSE-TRANSLATION-INFO -- Internal.
;;;
;;; This walks into *mouse-translation-info* the same way MOUSE-TRANSLATION-INFO
;;; does, filling in the data structure on an as-needed basis, and stores
;;; the value for the indicated info.
;;;
(defun %set-mouse-translation-info (button event-key info value)
  (let ((event-dispatch (svref *mouse-translation-info* button)))
    (unless event-dispatch
      (setf event-dispatch
	    (setf (svref *mouse-translation-info* button) (cons nil nil))))
    (let ((data (ecase event-key
		  (:button-press (button-press-info event-dispatch))
		  (:button-release (button-release-info event-dispatch)))))
      (unless data
	(setf data
	      (ecase event-key
		(:button-press
		 (setf (button-press-info event-dispatch) (cons nil nil)))
		(:button-release
		 (setf (button-release-info event-dispatch) (cons nil nil))))))
      (ecase info
	(:keysym
	 (setf (button-keysym data) value))
	(:shifted-modifier-name
	 (setf (button-shifted-modifier-name data) value))))))
;;;
(defsetf mouse-translation-info %set-mouse-translation-info)

;;; DEFINE-MOUSE-KEYSYM -- Public.
;;;
(defun define-mouse-keysym (button keysym name shifted-bit event-key)
  "This defines keysym named name for the X button cross the X event-key.
   Shifted-bit is a defined modifier name that TRANSLATE-MOUSE-KEY-EVENT sets
   in the key-event it returns whenever the X shift bit is on."
  (unless (<= 1 button 5)
    (error "Buttons are number 1-5, not ~D." button))
  (setf (gethash keysym *keysyms-to-names*) (list name))
  (setf (gethash  (get-name-case-right name) *names-to-keysyms*) keysym)
  (setf (mouse-translation-info button event-key :keysym) keysym)
  (setf (mouse-translation-info button event-key :shifted-modifier-name)
	shifted-bit))

;;; TRANSLATE-MOUSE-KEY-EVENT -- Public.
;;;
(defun translate-mouse-key-event (scan-code bits event-key)
  "This translates the X button code, scan-code, and modifier bits, bits, for
   the X event-key into a key-event.  See DEFINE-MOUSE-KEYSYM."
  (let ((keysym (mouse-translation-info scan-code event-key :keysym))
	(new-bits 0))
    (dolist (map *modifier-translations*)
      (when (logtest (car map) bits)
	(setf new-bits
	      (if (string-equal (cdr map) "Shift")
		  (logior new-bits
			  (key-event-modifier-mask
			   (mouse-translation-info
			    scan-code event-key :shifted-modifier-name)))
		  (logior new-bits
			  (key-event-modifier-mask (cdr map)))))))
    (make-key-event keysym new-bits)))



;;;; Stuff for parsing #k syntax.

(defstruct (key-event (:print-function %print-key-event)
		      (:constructor %make-key-event (keysym bits)))
  (bits nil :type fixnum)
  (keysym nil :type fixnum))

(defun %print-key-event (object stream ignore)
  (declare (ignore ignore))
  (write-string "#<Key-Event " stream)
  (print-pretty-key-event object stream)
  (write-char #\> stream))

;;; This maps Common Lisp CHAR-CODE's to character classes for parsing #k
;;; syntax.
;;;
(defvar *key-character-classes* (make-array char-code-limit
					    :initial-element :other))

;;; These characters are special:
;;;    #\<  ..........  :ISO-start - Signals start of an ISO character.
;;;    #\>  ..........  :ISO-end - Signals end of an ISO character.
;;;    #\-  ..........  :modifier-terminator - Indicates last *id-namestring*
;;;                                            was a modifier.
;;;    #\"  ..........  :EOF - Means we have come to the end of the character.
;;;    #\{a-z, A-Z} ..  :letter - Means the char is a letter.
;;;    #\space .......  :event-terminator- Indicates the last *id-namestring*
;;;                                        was a character name.
;;;
;;; Every other character has class :other.
;;;
(hi::do-alpha-chars (char :both)
  (setf (svref *key-character-classes* (char-code char)) :letter))
(setf (svref *key-character-classes* (char-code #\<)) :ISO-start)
(setf (svref *key-character-classes* (char-code #\>)) :ISO-end)
(setf (svref *key-character-classes* (char-code #\-)) :modifier-terminator)
(setf (svref *key-character-classes* (char-code #\space)) :event-terminator)
(setf (svref *key-character-classes* (char-code #\")) :EOF)
  
;;; This holds the characters built up while lexing a potential keysym or
;;; modifier identifier.
;;;
(defvar *id-namestring*
  (make-array 30 :adjustable t :fill-pointer 0 :element-type 'base-char))

;;; PARSE-KEY-FUN -- Internal.
;;;
;;; This is the #k dispatch macro character reader.  It is a FSM that parses
;;; key specifications.  It returns either a VECTOR form or a MAKE-KEY-EVENT
;;; form.  Since key-events are unique at runtime, we cannot create them at
;;; readtime, returning the constant object from READ.  Wherever a #k appears,
;;; there's a for that at loadtime or runtime will return the unique key-event
;;; or vector of unique key-events.
;;;
(defun parse-key-fun (stream sub-char count)
  (declare (ignore sub-char count))
  (setf (fill-pointer *id-namestring*) 0)
  (prog ((bits 0)
	 (key-event-list ())
	 char class)
	(unless (char= (read-char stream) #\")
	  (error "Keys must be delimited by ~S." #\"))
	;; Skip any leading spaces in the string.
	(skip-whitespace stream)
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :other :escaped) (go ID))
	  (:ISO-start (go ISOCHAR))
	  (:ISO-end (error "Angle brackets must be escaped."))
	  (:modifier-terminator (error "Dash must be escaped."))
	  (:EOF (error "No key to read.")))
	ID
	(vector-push-extend char *id-namestring*)
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :other :escaped) (go ID))
	  (:event-terminator (go GOT-CHAR))
	  (:modifier-terminator (go GOT-MODIFIER))
	  ((:ISO-start :ISO-end) (error "Angle brackets must be escaped."))
	  (:EOF (go GET-LAST-CHAR)))
	GOT-CHAR
	(push `(make-key-event ,(copy-seq *id-namestring*) ,bits)
	      key-event-list)
	(setf (fill-pointer *id-namestring*) 0)
	(setf bits 0)
	;; Skip any whitespace between characters.
	(skip-whitespace stream)
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :other :escaped) (go ID))
	  (:ISO-start (go ISOCHAR))
	  (:ISO-end (error "Angle brackets must be escaped."))
	  (:modifier-terminator (error "Dash must be escaped."))
	  (:EOF (go FINAL)))
	GOT-MODIFIER
	(let ((modifier-name (car (assoc *id-namestring*
					 *modifiers-to-internal-masks*
					 :test #'string-equal))))
	  (unless modifier-name
	    (error "~S is not a defined modifier." *id-namestring*))
	  (setf (fill-pointer *id-namestring*) 0)
	  (setf bits (logior bits (key-event-modifier-mask modifier-name))))
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :other :escaped) (go ID))
	  (:ISO-start (go ISOCHAR))
	  (:ISO-end (error "Angle brackets must be escaped."))
	  (:modifier-terminator (error "Dash must be escaped."))
	  (:EOF (error "Expected something naming a key-event, got EOF.")))
	ISOCHAR
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :event-terminator :other :escaped)
	   (vector-push-extend char *id-namestring*)
	   (go ISOCHAR))
	  (:ISO-start (error "Open Angle must be escaped."))
	  (:modifier-terminator (error "Dash must be escaped."))
	  (:EOF (error "Bad syntax in key specification, hit eof."))
	  (:ISO-end (go GOT-CHAR)))
	GET-LAST-CHAR
	(push `(make-key-event ,(copy-seq *id-namestring*) ,bits)
	      key-event-list)
	FINAL
	(return (if (cdr key-event-list)
		    `(vector ,@(nreverse key-event-list))
		    `,(car key-event-list)))))

(set-dispatch-macro-character #\# #\k #'parse-key-fun)

(defconstant key-event-escape-char #\\
  "The escape character that #k uses.")

;;; GET-KEY-CHAR -- Internal.
;;;
;;; This is used by PARSE-KEY-FUN.
;;;
(defun get-key-char (stream)
  (let ((char (read-char stream t nil t)))
    (cond ((char= char key-event-escape-char)
	   (let ((char (read-char stream t nil t)))
	     (values char :escaped)))
	  (t (values char (svref *key-character-classes* (char-code char)))))))



;;;; Code to deal with modifiers.

(defvar *modifier-count* 0
  "The number of modifiers that is currently defined.")

(eval-when (compile eval load)

(defconstant modifier-count-limit 6
  "The maximum number of modifiers supported.")

); eval-when

;;; This is purely a list for users.
;;;
(defvar *all-modifier-names* ()
  "A list of all the names of defined modifiers.")

;;; DEFINE-KEY-EVENT-MODIFIER -- Public.
;;;
;;; Note that short-name is pushed into *modifiers-to-internal-masks* after
;;; long-name.  PRINT-PRETTY-KEY-EVENT and KEY-EVENT-BITS-MODIFIERS rely on
;;; this feature.
;;;
(defun define-key-event-modifier (long-name short-name)
  "This establishes long-name and short-name as modifier names for purposes
   of specifying key-events in #k syntax.  The names are case-insensitive and
   must be strings.  If either name is already defined, this signals an error."
  (when (= *modifier-count* modifier-count-limit)
    (error "Maximum of ~D modifiers allowed." modifier-count-limit))
  (let ((long-name (string-capitalize long-name))
	(short-name (string-capitalize short-name)))
    (flet ((frob (name)
	     (when (assoc name *modifiers-to-internal-masks*
			  :test #'string-equal)
	       (restart-case
		   (error "Modifier name has already been defined -- ~S" name)
		 (blow-it-off ()
		  :report "Go on without defining this modifier."
		  (return-from define-key-event-modifier nil))))))
      (frob long-name)
      (frob short-name))
    (unwind-protect
	(let ((new-bits (ash 1 *modifier-count*)))
	  (push (cons long-name new-bits) *modifiers-to-internal-masks*)
	  (push (cons short-name new-bits) *modifiers-to-internal-masks*)
	  (pushnew long-name *all-modifier-names* :test #'string-equal)
	  ;; Sometimes the long-name is the same as the short-name.
	  (pushnew short-name *all-modifier-names* :test #'string-equal))
      (incf *modifier-count*))))

;;;
;;; RE-INITIALIZE-KEY-EVENTS at the end of this file defines the system
;;; default key-event modifiers.
;;; 

;;; DEFINE-CLX-MODIFIER -- Public.
;;;
(defun define-clx-modifier (clx-mask modifier-name)
  "This establishes a mapping from clx-mask to a define key-event modifier-name.
   TRANSLATE-KEY-EVENT and TRANSLATE-MOUSE-KEY-EVENT can only return key-events
   with bits defined by this routine."
  (let ((map (assoc modifier-name *modifiers-to-internal-masks*
		    :test #'string-equal)))
    (unless map (error "~S an undefined modifier name." modifier-name))
    (push (cons clx-mask (car map)) *modifier-translations*)))

;;;
;;; RE-INITIALIZE-KEY-EVENTS at the end of this file defines the system
;;; default clx modifiers, mapping them to some system default key-event
;;; modifiers.
;;; 

;;; MAKE-KEY-EVENT-BITS -- Public.
;;;
(defun make-key-event-bits (&rest modifier-names)
  "This returns bits suitable for MAKE-KEY-EVENT from the supplied modifier
   names.  If any name is undefined, this signals an error."
  (let ((mask 0))
    (dolist (mod modifier-names mask)
      (let ((this-mask (cdr (assoc mod *modifiers-to-internal-masks*
				   :test #'string-equal))))
	(unless this-mask (error "~S is an undefined modifier name." mod))
	(setf mask (logior mask this-mask))))))

;;; KEY-EVENT-BITS-MODIFIERS -- Public.
;;;
(defun key-event-bits-modifiers (bits)
  "This returns a list of key-event modifier names, one for each modifier
   set in bits."
  (let ((res nil))
    (do ((map (cdr *modifiers-to-internal-masks*) (cddr map)))
	((null map) res)
      (when (logtest bits (cdar map))
	(push (caar map) res)))))

;;; KEY-EVENT-MODIFIER-MASK -- Public.
;;;
(defun key-event-modifier-mask (modifier-name)
  "This function returns a mask for modifier-name.  This mask is suitable
   for use with KEY-EVENT-BITS.  If modifier-name is undefined, this signals
   an error."
  (let ((res (cdr (assoc modifier-name *modifiers-to-internal-masks*
			 :test #'string-equal))))
    (unless res (error "Undefined key-event modifier -- ~S." modifier-name))
    res))



;;;; Key event lookup -- GET-KEY-EVENT and MAKE-KEY-EVENT.

(defvar *keysym-high-bytes*)

(defconstant modifier-bits-limit (ash 1 modifier-count-limit))

;;; GET-KEY-EVENT -- Internal.
;;;
;;; This finds the key-event specified by keysym and bits.  If the key-event
;;; does not already exist, this creates it.  This assumes keysym is defined,
;;; and if it isn't, this will make a key-event anyway that will cause an
;;; error when the system tries to print it.
;;;
(defun get-key-event (keysym bits)
  (let* ((high-byte (ash keysym -8))
	 (low-byte-vector (svref *keysym-high-bytes* high-byte)))
    (unless low-byte-vector
      (let ((new-vector (make-array 256 :initial-element nil)))
	(setf (svref *keysym-high-bytes* high-byte) new-vector)
	(setf low-byte-vector new-vector)))
    (let* ((low-byte (ldb (byte 8 0) keysym))
	   (bit-vector (svref low-byte-vector low-byte)))
      (unless bit-vector
	(let ((new-vector (make-array modifier-bits-limit
				      :initial-element nil)))
	  (setf (svref low-byte-vector low-byte) new-vector)
	  (setf bit-vector new-vector)))
      (let ((key-event (svref bit-vector bits)))
	(if key-event
	    key-event
	    (setf (svref bit-vector bits) (%make-key-event keysym bits)))))))

;;; MAKE-KEY-EVENT --  Public.
;;;
(defun make-key-event (object &optional (bits 0))
  "This returns a key-event described by object with bits.  Object is one of
   keysym, string, or key-event.  When object is a key-event, this uses
   KEY-EVENT-KEYSYM.  You can form bits with MAKE-KEY-EVENT-BITS or
   KEY-EVENT-MODIFIER-MASK."
  (etypecase object
    (integer
     (unless (keysym-names object)
       (error "~S is an undefined keysym." object))
     (get-key-event object bits))
    #|(character
     (let* ((name (char-name object))
	    (keysym (name-keysym (or name (string object)))))
       (unless keysym
	 (error "~S is an undefined keysym." object))
       (get-key-event keysym bits)))|#
    (string
     (let ((keysym (name-keysym object)))
       (unless keysym
	 (error "~S is an undefined keysym." object))
       (get-key-event keysym bits)))
    (key-event
     (get-key-event (key-event-keysym object) bits))))

;;; KEY-EVENT-BIT-P -- Public.
;;;
(defun key-event-bit-p (key-event bit-name)
  "This returns whether key-event has the bit set named by bit-name.  This
   signals an error if bit-name is undefined."
  (let ((mask (cdr (assoc bit-name *modifiers-to-internal-masks*
			  :test #'string-equal))))
    (unless mask
      (error "~S is not a defined modifier." bit-name))
    (not (zerop (logand (key-event-bits key-event) mask)))))



;;;; KEY-EVENT-CHAR and CHAR-KEY-EVENT.

;;; This maps key-events to characters.  Users modify this by SETF'ing
;;; KEY-EVENT-CHAR.
;;;
(defvar *key-event-characters*)

(defun key-event-char (key-event)
  "Returns the character associated with key-event. This is SETF'able."
  (check-type key-event key-event)
  (gethash key-event *key-event-characters*))

(defun %set-key-event-char (key-event character)
  (check-type character character)
  (check-type key-event key-event)
  (setf (gethash key-event *key-event-characters*) character))
;;;
(defsetf key-event-char %set-key-event-char)


;;; This maps characters to key-events.  Users modify this by SETF'ing
;;; CHAR-KEY-EVENT.
;;;
(defvar *character-key-events*)

(defun char-key-event (char)
  "Returns the key-event associated with char.  This is SETF'able."
  (check-type char character)
  (svref *character-key-events* (char-code char)))

(defun %set-char-key-event (char key-event)
  (check-type char character)
  (check-type key-event key-event)
  (setf (svref *character-key-events* (char-code char)) key-event))
;;;
(defsetf char-key-event %set-char-key-event)



;;;; DO-ALPHA-KEY-EVENTS.

(defmacro alpha-key-events-loop (var start-keysym end-keysym result body)
  (let ((n (gensym)))
    `(do ((,n ,start-keysym (1+ ,n)))
	 ((> ,n ,end-keysym) ,result)
       (let ((,var (make-key-event ,n 0)))
	 (when (alpha-char-p (key-event-char ,var))
	   ,@body)))))

(defmacro do-alpha-key-events ((var kind &optional result) &rest forms)
  "(DO-ALPHA-KEY-EVENTS (var kind [result]) {form}*)
   This macro evaluates each form with var bound to a key-event representing an
   alphabetic character.  Kind is one of :lower, :upper, or :both, and this
   binds var to each key-event in order as specified in the X11 protocol
   specification.  When :both is specified, this processes lowercase letters
   first."
  (case kind
    (:both
     `(progn (alpha-key-events-loop ,var 97 122 nil ,forms)
	     (alpha-key-events-loop ,var 65 90 ,result ,forms)))
    (:lower
     `(alpha-key-events-loop ,var 97 122 ,result ,forms))
    (:upper
     `(alpha-key-events-loop ,var 65 90 ,result ,forms))
    (t (error "Kind argument not one of :lower, :upper, or :both -- ~S."
	      kind))))



;;;; PRINT-PRETTY-KEY and PRINT-PRETTY-KEY-EVENT.

;;; PRINT-PRETTY-KEY -- Public.
;;;
(defun print-pretty-key (key &optional (stream *standard-output*) long-names-p)
  "This prints key, a key-event or vector of key-events, to stream in a
   user-expected fashion.  Long-names-p indicates whether modifiers should
   print with their long or short name."
  (declare (type (or vector key-event) key) (type stream stream))
  (etypecase key
    (key-event (print-pretty-key-event key stream long-names-p))
    (vector
     (let ((length-1 (1- (length key))))
       (dotimes (i (length key))
	 (let ((key-event (aref key i)))
	   (print-pretty-key-event key-event stream long-names-p)
	   (unless (= i length-1) (write-char #\space stream))))))))

;;; PRINT-PRETTY-KEY-EVENT -- Public.
;;;
;;; Note, this makes use of the ordering in the a-list
;;; *modifiers-to-internal-masks* by CDDR'ing down it by starting on a short
;;; name or a long name.
;;;
(defun print-pretty-key-event (key-event &optional (stream *standard-output*)
					 long-names-p)
  "This prints key-event to stream.  Long-names-p indicates whether modifier
   names should appear using the long name or short name."
  (do ((map (if long-names-p
		(cdr *modifiers-to-internal-masks*)
		*modifiers-to-internal-masks*)
	    (cddr map)))
      ((null map))
    (when (not (zerop (logand (cdar map) (key-event-bits key-event))))
      (write-string (caar map) stream)
      (write-char #\- stream)))
  (let* ((name (keysym-preferred-name (key-event-keysym key-event)))
	 (spacep (position #\space (the simple-string name))))
    (when spacep (write-char #\< stream))
    (write-string name stream)
    (when spacep (write-char #\> stream))))



;;;; Re-initialization.

;;; RE-INITIALIZE-KEY-EVENTS -- Internal.
;;;
(defun re-initialize-key-events ()
  "This blows away all data associated with keysyms, modifiers, mouse
   translations, and key-event/characters mapping.  Then it re-establishes
   the system defined key-event modifiers and the system defined CLX
   modifier mappings to some of those key-event modifiers.

   When recompiling this file, you should load it and call this function
   before using any part of the key-event interface, especially before
   defining all your keysyms and using #k syntax."
  (setf *keysyms-to-names* (make-hash-table :test #'eql))
  (setf *names-to-keysyms* (make-hash-table :test #'equal))
  (setf *modifier-translations* ())
  (setf *modifiers-to-internal-masks* ())
  (setf *mouse-translation-info* (make-array 6 :initial-element nil))
  (setf *modifier-count* 0)
  (setf *all-modifier-names* ())
  (setf *keysym-high-bytes* (make-array 256 :initial-element nil))
  (setf *key-event-characters* (make-hash-table))
  (setf *character-key-events*
	(make-array char-code-limit :initial-element nil))
  
  (define-key-event-modifier "Hyper" "H")
  (define-key-event-modifier "Super" "S")
  (define-key-event-modifier "Meta" "M")
  (define-key-event-modifier "Control" "C")
  (define-key-event-modifier "Shift" "Shift")
  (define-key-event-modifier "Lock" "Lock")

  #+clx (define-clx-modifier (xlib:make-state-mask :shift) "Shift")
  #+clx (define-clx-modifier (xlib:make-state-mask :mod-1) "Meta")
  #+clx (define-clx-modifier (xlib:make-state-mask :control) "Control")
  #+clx (define-clx-modifier (xlib:make-state-mask :lock) "Lock"))

;;; Initialize stuff if not already initialized.
;;;
(unless (boundp '*keysyms-to-names*)
  (re-initialize-key-events))
