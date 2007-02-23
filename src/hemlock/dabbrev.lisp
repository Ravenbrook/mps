;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/dabbrev.lisp,v 1.1 2003/02/25 15:18:34 emarsden Exp $")

;;; **********************************************************************
;;; Dynamic abbreviation (dabbrev) command, knocked off from GNU Emacs.
;;; Written by Luke Gorrie <luke@bluetail.com> in February 2002.
;;; This code has been placed in the public domain.

(in-package "HEMLOCK")

;;; ----------------------------------------------------------------------
;;; Internal state for continuing expansions. This is only maintained
;;; between consecutive calls to Dabbrev Expand, and all gets reset when a
;;; new expansion is staretd.

(defvar *expanded-suffix-length* nil
  "Length of the previously expanded suffix, or Nil if no expansion has
   been made. This length is needed to undo an expansion.")

(defvar *seen-dabbrevs* nil
  "List of abbreviations that have already been offered, and will be
   skipped in future.")

(defvar *dabbrev-continuation* nil
  "Closure which, when called with no arguments, continues from the
   previous expansion.")

(defcommand "Dabbrev Expand" (p)
  "Expand previous word \"dynamically\".

   Expands to the most recent, preceding word for which this is a prefix.
   If no suitable preceding word is found, words following point are
   considered.

  Repeated calls continue by finding new expansions."
  "See command docstring. I mean, really."
  (declare (ignore p))
  (if (eq (last-command-type) :dabbrev-expand)
      (continue-dabbrev-search)
      (new-dabbrev-search)))

(defun continue-dabbrev-search ()
  "Replace the previous expansion with the next new one."
  (funcall *dabbrev-continuation*))

(defun new-dabbrev-search ()
  "Start a new search for an expansion."
  (reset-dabbrev-state)
  (let ((mark (copy-mark (current-point) :temporary)))
    (when (start-of-dabbrev-prefix mark)
      (let ((prefix (region-to-string (region mark (current-point)))))
	(if (string= prefix "")
	    (editor-error "No possible abbreviation preceding point")
	    (dabbrev-find-expansion mark :backward prefix))))))

(defun reset-dabbrev-state ()
  (setq *expanded-suffix-length* nil
	*seen-dabbrevs* nil
	*dabbrev-continuation* nil))

(defun start-of-dabbrev-prefix (mark)
  "Move Mark to the beginning of the word containing it. Returns NIL if
   there is no matching word."
  (or (reverse-find-attribute mark :lisp-syntax #'not-constituent-p)
      (or (start-line-p mark)
	  (line-start mark))))

;;; ----------------------------------------------------------------------
;;; Main searching engine

(defun dabbrev-find-expansion (start-mark direction string)
  "Try to find an expansion of STRING in DIRECTION, starting from
   START-MARK. The expansion suffix is returned if found, otherwise NIL."
  (let ((searchm (copy-mark start-mark :temporary)))
    (if (find-pattern searchm
		      (new-search-pattern :string-sensitive
					  direction
					  string))
	;; Marks to be placed for the region of the new suffix.
	(let ((start (copy-mark searchm :temporary))
	      (end   (copy-mark searchm :temporary)))
	  (character-offset start (length string))
	  (move-mark end start)
	  (or (find-attribute end :lisp-syntax #'not-constituent-p)
	      (line-end end))
	  (let ((match-region (region start end)))
	    (cond ((and (> (count-characters match-region) 0)
			(at-beginning-of-word-p searchm)
			(not (member (region-to-string match-region)
				     *seen-dabbrevs*
				     :test #'string=)))
		   (dabbrev-apply-expansion searchm match-region direction string))
		  ((and (eq direction :forward)
			(next-character end))
		   (dabbrev-find-expansion end direction string))
		  ((and (eq direction :backward)
			(previous-character searchm))
		   (dabbrev-find-expansion searchm direction string))
		  (t
		   (continue-failed-expansion direction string)))))
	(continue-failed-expansion direction string))))

(defun continue-failed-expansion (direction string)
  "Continue (or not) the search, after one avenue has failed."
  (cond ((eq direction :backward)
	 ;; Turn around -- now try forwards from Point
	 (dabbrev-find-expansion (current-point) :forward string))
	(t
	 ;; We've tried both directions, so just give up.
	 ;; Alternatively we could try other sources of abbreviations next.
	 (undo-previous-expansion)
	 (editor-error (if *seen-dabbrevs*
			   "No more expansions of `~A'"
			   "No expansion for `~A'")
		       string))))

(defun dabbrev-apply-expansion (match region direction prefix)
  "Apply the expansion found at Match to the buffer by inserting the
   suffix in Region after the original prefix."
  (undo-previous-expansion)
  (setq *expanded-suffix-length* (count-characters region))
  (let ((suffix (region-to-string region))
	(search-continue-pos (if (eq direction :forward)
				 (region-end region)
			       match)))
    (push suffix *seen-dabbrevs*)
    (insert-string (current-point) suffix)
    (dabbrev-install-continuation
     (lambda ()
       (dabbrev-find-expansion search-continue-pos direction prefix)))))

(defun undo-previous-expansion ()
  (when *expanded-suffix-length*
    (delete-characters (current-point) (- *expanded-suffix-length*))))

(defun dabbrev-install-continuation (k)
  (setf (last-command-type) :dabbrev-expand)
  (setq *dabbrev-continuation* k))

;;; ----------------------------------------------------------------------
;;; Little helpers

(defun at-beginning-of-word-p (mark)
  (or (start-line-p mark)
      (not (eq (character-attribute
		:lisp-syntax
		(previous-character mark))
	       :constituent))))

(defun not-constituent-p (property)
  (not (eq property :constituent)))

