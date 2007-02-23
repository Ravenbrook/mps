;;; -*- Mode: Lisp; Package: HEMLOCK-INTERNALS  -*-

(in-package "HEMLOCK-INTERNALS")

;;; todd kaufmann     24 mar 88

;;; because we will have lots of windows, with users moving between them lots,
;;; we would like the window the mouse is in to be the one input goes to.
;;; 
;;; also, this is more in line with the way most X windows work
;;;   (unless you like to use focus)

;;; Modified by Dave Touretzky 5/23/88 to fix incremental search problem,
;;; to change windows upon leaving the echo area if the mouse moves
;;; while we're prompting for input, and to select the correct window
;;; when we split a window, based on where the mouse ends up.

;;; Bugs:
;;;  - a few more prompt-for-* functions need to be modified.  they're
;;;    flagged below.

(defhvar "Follow Mouse to Window"
  "When T, the current window becomes the one the mouse enters; when NIL,
  behaves the old way"
  :value T)

(defhvar "Follow Mouse To Read-only Buffers"
  "Whether to follow the mouse to Read-only buffers or not"
  :value T)

(defvar *last-window-mouse-entered* nil)  ;set to the most recent LEGAL window entered

(defun change-to-current-entered-window (window)
  "Make the buffer the mouse has moved to be the current buffer."
  (let ((buffer (window-buffer window)))
    (when
	(and *in-the-editor*
	     (value ed::follow-mouse-to-window)
	     ;; don't move cursor to menus
	     (not (string= (buffer-major-mode buffer) "HMenu"))
	     ;; don't move TO echo area
	     (not (eq buffer *echo-area-buffer*))
	     ;; can move to read-only buffers if user says it's okay
	     (if (buffer-writable buffer) t
		 (value ed::follow-mouse-to-read-only-buffers))
	     ;; if we get here, this is a valid window to move to:  remember it
	     (setf *last-window-mouse-entered* window)
	     ;; but don't move OUT of the echo area if it's currently prompting.
	     (not (eq (current-window) *echo-area-window*))
	     ;; and don't move out of current window during an incremental search
	     (not *during-incremental-search*)
	     ;; Make sure it's not a random typeout buffer.
	     (member window *window-list*)
	     )
      (setf (current-window) window
	    (current-buffer) buffer
	    *last-window-mouse-entered* nil))))


(add-hook ed::enter-window-hook 'change-to-current-entered-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; If the user is being prompted in the echo area and moves the
;;; mouse, we can't switch windows.  So remember where the mouse
;;; has gone to, and switch when he leaves the echo area buffer.

(defun change-windows-if-mouse-moved ()
  (when (and *last-window-mouse-entered*
	     (not (eq (current-window) *last-window-mouse-entered*)))
    (change-to-current-entered-window *last-window-mouse-entered*)))


;from echo.lisp

(defun parse-for-something ()
  (display-prompt-nicely)
  (let ((start-window (current-window)))
    (move-mark *parse-starting-mark* (buffer-point *echo-area-buffer*))
    (setf (current-window) *echo-area-window*)
    (unwind-protect
	(use-buffer *echo-area-buffer*
		    (recursive-edit nil))
      (setf (current-window) start-window)
      (change-windows-if-mouse-moved))))  ; <--- here's the change

;---> Note:  must make the same change to the following:
;	prompt-for-y-or-n
;	prompt-for-character*
;	prompt-for-key


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fix for the problem of switching buffers in the middle of an
;;; incremental search.
;;;
;;; Modify %i-search from searchcoms.lisp to bind a global variable
;;; hi::*during-incremental-search* to T.  Check for this in body of
;;; change-to-current-entered-window above.

(defvar *during-incremental-search* nil)

(in-package "HEMLOCK")

(defun %i-search (string point trailer direction failure)
  (unwind-protect			           ;<---- here's the change
   (do* ((hi::*during-incremental-search* t)       ;<---- here's the change
	 (curr-point (copy-mark point :temporary))
	 (curr-trailer (copy-mark trailer :temporary))
	 (next-char (get-key-event *editor-input* t)
		    (get-key-event *editor-input* t)))
	(nil)
     (case (%i-search-char-eval next-char string point trailer direction failure)
       (:cancel
	(%i-search-echo-refresh string direction failure)
	(unless (zerop (length string))
	  (i-search-pattern string direction)))
       (:return-cancel
	(unless (zerop (length string)) (return :cancel))
	(beep))
       (:control-g
	(when failure (return :control-g))
	(%i-search-echo-refresh string direction nil)
	(unless (zerop (length string))
	  (i-search-pattern string direction))))
     (move-mark point curr-point)
     (move-mark trailer curr-trailer))
   (hi::change-windows-if-mouse-moved)))	   ;<---- here's the change


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fix Split Window to not change the current window.
;;; That way, if the mouse is still in the current window,
;;; everything's fine.  If the mouse ended up in the new
;;; window, a mouse-entered-window event will handle the
;;; change to the new window.

; From filecoms.lisp:

(defcommand "Split Window" (p)
  "Make a new window by splitting the current window.
   The new window is made the current window and displays starting at
   the same place as the current window."
  "Create a new window which displays starting at the same place
   as the current window."
  (declare (ignore p))
  (let ((new (make-window (window-display-start (current-window)))))
    (unless new (editor-error "Could not make a new window."))
;    (setf (current-window) new)     <--- commented out this line
 ))
