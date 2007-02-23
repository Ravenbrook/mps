;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/font.lisp,v 1.3 1994/10/31 04:50:12 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Rob MacLachlan
;;; Modified by Bill Chiles toward Hemlock running under X.
;;;
;;;    This file contains various functions that make up the user interface to
;;; fonts.
;;;

(in-package "HEMLOCK-INTERNALS")

(export '(font-mark delete-font-mark delete-line-font-marks move-font-mark
		    window-font))
;;; Default-font used to be in the above list, but when I cleaned up the way
;;; Hemlock compiles, a name conflict occurred because "Default Font" is a
;;; Hemlock variable.  It is now exported by the export list in rompsite.lisp.

(defvar *default-font-family* (make-font-family))



;;;; Creating, Deleting, and Moving.

(defun font-mark (line charpos font &optional (kind :right-inserting))
  "Returns a font on line at charpos with font.  Font marks must be permanent
   marks."
  (unless (or (eq kind :right-inserting) (eq kind :left-inserting))
    (error "A Font-Mark must be :left-inserting or :right-inserting."))
  (unless (and (>= font 0) (< font font-map-size))
    (error "Font number ~S out of range." font))
  (let ((new (internal-make-font-mark line charpos kind font)))
    (new-font-mark new line)
    (push new (line-marks line))
    new))

(defun delete-font-mark (font-mark)
  "Deletes a font mark."
  (check-type font-mark font-mark)
  (let ((line (mark-line font-mark)))
    (when line
      (setf (line-marks line) (delq font-mark (line-marks line)))
      (nuke-font-mark font-mark line)
      (setf (mark-line font-mark) nil))))

(defun delete-line-font-marks (line)
  "Deletes all font marks on line."
  (dolist (m (line-marks line))
    (when (fast-font-mark-p m)
      (delete-font-mark m))))

(defun move-font-mark (font-mark new-position)
  "Moves font mark font-mark to location of mark new-position."
  (check-type font-mark font-mark)
  (let ((old-line (mark-line font-mark))
	(new-line (mark-line new-position)))
    (nuke-font-mark font-mark old-line)
    (move-mark font-mark new-position)
    (new-font-mark font-mark new-line)
    font-mark))

(defun nuke-font-mark (mark line)
  (new-font-mark mark line))

(defun new-font-mark (mark line)
  (declare (ignore mark))
  (let ((buffer (line-%buffer line))
	(number (line-number line)))
    (when (bufferp buffer)
      (dolist (w (buffer-windows buffer))
	(setf (window-tick w) (1- (buffer-modified-tick buffer)))
	(let ((first (cdr (window-first-line w))))
	  (unless (or (> (line-number (dis-line-line (car first))) number)
		      (> number
			 (line-number
			  (dis-line-line (car (window-last-line w))))))
	    (do ((dl first (cdr dl)))
		((or (null dl)
		     (eq (dis-line-line (car dl)) line))
		 (when dl
		   (setf (dis-line-old-chars (car dl)) :font-change))))))))))



;;;; Referencing and setting font ids.

(defun window-font (window font)
  "Returns a font id for window and font."
  (svref (font-family-map (bitmap-hunk-font-family (window-hunk window))) font))

(defun %set-window-font (window font font-object)
  (unless (and (>= font 0) (< font font-map-size))
    (error "Font number ~S out of range." font))
  (setf (bitmap-hunk-trashed (window-hunk window)) :font-change)
  (let ((family (bitmap-hunk-font-family (window-hunk window))))
    (when (eq family *default-font-family*)
      (setq family (copy-font-family family))
      (setf (font-family-map family) (copy-seq (font-family-map family)))
      (setf (bitmap-hunk-font-family (window-hunk window)) family))
    (setf (svref (font-family-map family) font) font-object)))

(defun default-font (font)
  "Returns the font id for font out of the default font family."
  (svref (font-family-map *default-font-family*) font))

(defun %set-default-font (font font-object)
  (unless (and (>= font 0) (< font font-map-size))
    (error "Font number ~S out of range." font))
  (dolist (w *window-list*)
    (when (eq (bitmap-hunk-font-family (window-hunk w)) *default-font-family*)
      (setf (bitmap-hunk-trashed (window-hunk w)) :font-change)))
  (setf (svref (font-family-map *default-font-family*) font) font-object))
