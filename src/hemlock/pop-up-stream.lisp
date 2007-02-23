;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/pop-up-stream.lisp,v 1.4 2001/03/13 15:49:57 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contatins the stream operations for pop-up-displays.
;;;
;;; Written by Blaine Burks.
;;;

(in-package "HEMLOCK-INTERNALS")



;;;; Line-buffered Stream Methods.

(defun random-typeout-line-out (stream char)
  (insert-character (random-typeout-stream-mark stream) char)
  (when (and (char= char #\newline)
	     (not (random-typeout-stream-no-prompt stream)))
    (funcall (device-random-typeout-line-more
	      (device-hunk-device
	       (window-hunk (random-typeout-stream-window stream))))
	     stream 1)))

(defun random-typeout-line-sout (stream string start end)
  (insert-string (random-typeout-stream-mark stream) string start end)
  (unless (random-typeout-stream-no-prompt stream)
    (let ((count (count #\newline string)))
      (when count
	(funcall (device-random-typeout-line-more
		  (device-hunk-device
		   (window-hunk (random-typeout-stream-window stream))))
		 stream count)))))

(defun random-typeout-line-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    ((:force-output :finish-output)
     (random-typeout-redisplay (random-typeout-stream-window stream)))
    (:charpos
     (mark-charpos (random-typeout-stream-mark stream)))))


;;; Bitmap line-buffered support.

;;; UPDATE-BITMAP-LINE-BUFFERED-STREAM is called when anything is written to
;;; a line-buffered-random-typeout-stream on the bitmap.  It does a lot of
;;; checking to make sure that strings of characters longer than the width of
;;; the window don't screw us.  The code is a little wierd, so a brief
;;; explanation is below.
;;;
;;; The more-mark is how we tell when we will next need to more.  Each time
;;; we do a more-prompt, we point the mark at the last visible character in
;;; the random typeout window.  That way, when the mark is no longer
;;; DISPLAYED-P, we know it's time to do another more prompt.
;;;
;;; If the buffer-end-mark is DISPLAYED-P, then we return, only redisplaying
;;; if there was at least one newline in the last batch of output.  If we
;;; haven't done a more prompt yet (indicated by a value of T for
;;; first-more-p), then since we know the end of the buffer isn't visible, we
;;; need to do a more-prompt.  If neither of the first two tests returns T,
;;; then we can only need to do a more-prompt if our more-mark has scrolled
;;; off the top of the screen.  If it hasn't, everything is peechy-keen, so
;;; we scroll the screen one line and redisplay.
;;;
(defun update-bitmap-line-buffered-stream (stream newline-count)
  (let* ((window (random-typeout-stream-window stream))
	 (count 0))
    (when (plusp newline-count) (random-typeout-redisplay window))
    (loop
      (cond ((no-text-past-bottom-p window)
	     (return))
	    ((or (random-typeout-stream-first-more-p stream)
		 (not (displayed-p (random-typeout-stream-more-mark stream)
				   window)))
	     (do-bitmap-more-prompt stream)
	     (return))
	    (t
	     (scroll-window window 1)
	     (random-typeout-redisplay window)))
      (when (= (incf count) newline-count) (return)))))

;;; NO-TEXT-PAST-BOTTOM-P determines whether there is text left to be displayed
;;; in the random-typeout window.  It does this by first making sure there is a
;;; line past the WINDOW-DISPLAY-END of the window.  If there is, this line
;;; must be empty, and BUFFER-END-MARK must be on this line.  The final test is
;;; that the window-end is displayed within the window.  If it is not, then the
;;; last line wraps past the end of the window, and there is text past the
;;; bottom.
;;;
;;; Win-end is bound after the call to DISPLAYED-P because it updates the
;;; window's image moving WINDOW-DISPLAY-END.  We want this updated value for
;;; the display end.
;;;
(defun no-text-past-bottom-p (window)
  (let* ((window-end (window-display-end window))
	 (window-end-displayed-p (displayed-p window-end window)))
    (with-mark ((win-end window-end))
      (let ((one-after-end (line-offset win-end 1)))
	(if one-after-end
	    (and (empty-line-p win-end)
		 (same-line-p win-end (buffer-end-mark (window-buffer window)))
		 window-end-displayed-p)
	    window-end-displayed-p)))))

(defun reset-more-mark (stream)
  (let* ((window (random-typeout-stream-window stream))
	 (more-mark (random-typeout-stream-more-mark stream))
	 (end (window-display-end window)))
    (move-mark more-mark end)
    (unless (displayed-p end window) (character-offset more-mark -1))))

;;; DO-BITMAP-MORE-PROMPT is the function that atually displays the more prompt
;;; and reacts to it.  Things are pretty clear.  The loop is neccessary because
;;; someone could screw us by never outputting newlines.  Improbable, but
;;; possible.
;;;
(defun do-bitmap-more-prompt (stream)
  (let* ((window (random-typeout-stream-window stream))
	 (height (window-height window)))
    (setf (random-typeout-stream-first-more-p stream) nil)
    (reset-more-mark stream)
    (loop
      (when (no-text-past-bottom-p window) (return))
      (display-more-prompt stream)
      (do ((i 0 (1+ i)))
	  ((or (= i height) (no-text-past-bottom-p window)))
	(scroll-window window 1)
	(random-typeout-redisplay window)))
    (unless (displayed-p (random-typeout-stream-more-mark stream) window)
      (reset-more-mark stream))))


;;; Tty line-buffered support.

;;; UPDATE-TTY-LINE-BUFFERED-STREAM is called when anything is written to
;;; a line-buffered-random-typeout-stream on the tty.  It just makes sure
;;; hemlock doesn't choke on extra-long strings.
;;;
(defun update-tty-line-buffered-stream (stream newline-count)
  (let ((window (random-typeout-stream-window stream)))
    (when (plusp newline-count) (random-typeout-redisplay window))
    (loop
      (when (no-text-past-bottom-p window) (return))
      (display-more-prompt stream)
      (scroll-window window (window-height window))
      (random-typeout-redisplay window))))


;;;; Full-buffered Stream Methods.

(defun random-typeout-full-out (stream char)
  (insert-character (random-typeout-stream-mark stream) char))

(defun random-typeout-full-sout (stream string start end)
  (insert-string (random-typeout-stream-mark stream) string start end))

(defun random-typeout-full-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos
     (mark-charpos (random-typeout-stream-mark stream)))))


;;; Bitmap full-buffered support.

;;; DO-BITMAP-FULL-MORE and DO-TTY-FULL-MORE scroll through the fresh text in
;;; random typeout buffer.  The bitmap function does some checking so that
;;; we don't overshoot the end of the buffer.
;;;
(defun do-bitmap-full-more (stream)
  (let* ((window (random-typeout-stream-window stream))
	 (buffer (window-buffer window))
	 (height (window-height window)))
    (with-mark ((end-check (buffer-end-mark buffer)))
      (when (and (mark/= (buffer-start-mark buffer) end-check)
		 (empty-line-p end-check))
	(line-end (line-offset end-check -1)))
      (loop
	(when (displayed-p end-check window)
	  (return))
	(display-more-prompt stream)
	(do ((i 0 (1+ i)))
	    ((or (= i height) (displayed-p end-check window)))
	  (scroll-window window 1)
	  (random-typeout-redisplay window))))))


;;; Tty full-buffered support.

(defun do-tty-full-more (stream)
  (let* ((window (random-typeout-stream-window stream))
	 (buffer (window-buffer window)))
    (with-mark ((end-check (buffer-end-mark buffer)))
      (when (and (mark/= (buffer-start-mark buffer) end-check)
		 (empty-line-p end-check))
	(line-end (line-offset end-check -1)))
      (loop
	(when (displayed-p end-check window)
	  (return))
	(display-more-prompt stream)
	(scroll-window window (window-height window))))))


;;; Proclaim this special so the compiler doesn't warn me.  I hate that.
;;;
(declaim (special *more-prompt-action*))

(defun display-more-prompt (stream)
  (unless (random-typeout-stream-no-prompt stream)
    (let ((window (random-typeout-stream-window stream))
	  (*more-prompt-action* :more))
      (update-modeline-field (window-buffer window) window :more-prompt)
      (random-typeout-redisplay window)
      (wait-for-more stream)
      (let ((*more-prompt-action* :empty))
	(update-modeline-field (window-buffer window) window :more-prompt)))))
