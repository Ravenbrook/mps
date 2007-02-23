;;; -*- Package: Hemlock; Log: hemlock.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/filecoms.lisp,v 1.5 1994/10/31 04:50:12 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains file/buffer manipulating commands.
;;;

(in-package "HEMLOCK")



;;;; PROCESS-FILE-OPTIONS.

(defvar *mode-option-handlers* ()
  "Do not modify this; use Define-File-Option instead.")

(defvar *file-type-hooks* ()
  "Do not modify this; use Define-File-Type-Hook instead.")  

(defun trim-subseq (string start end)
  (declare (simple-string string))
  (string-trim '(#\Space #\Tab) (subseq string start end)))

;;; PROCESS-FILE-OPTIONS checks the first line of buffer for the file options
;;; indicator "-*-".  IF it finds this, then it enters a do-file-options block.
;;; If any parsing errors occur while picking out options, we return from this
;;; block.  Staying inside this function at this point, allows us to still set
;;; a major mode if no file option specified one.
;;;
;;; We also cater to old style mode comments:
;;;    -*- Lisp -*-
;;;    -*- Text -*-
;;; This kicks in if we find no colon on the file options line.
;;;
(defun process-file-options (buffer &optional
				    (pathname (buffer-pathname buffer)))
  "Checks for file options and invokes handlers if there are any.  If no
   \"Mode\" mode option is specified, then this tries to invoke the appropriate
   file type hook."
  (let* ((string
	  (line-string (mark-line (buffer-start-mark buffer))))
	 (found (search "-*-" string))
	 (no-major-mode t)
	 (type (if pathname (pathname-type pathname))))
    (declare (simple-string string))
    (when found
      (block do-file-options
	(let* ((start (+ found 3))
	       (end (search "-*-" string :start2 start)))
	  (unless end
	    (loud-message "No closing \"-*-\".  Aborting file options.")
	    (return-from do-file-options))
	  (cond
	   ((find #\: string :start start :end end)
	    (do ((opt-start start (1+ semi)) colon semi)
		(nil)
	      (setq colon (position #\: string :start opt-start :end end))
	      (unless colon
		(loud-message "Missing \":\".  Aborting file options.")
		(return-from do-file-options))
	      (setq semi (or (position #\; string :start colon :end end) end))
	      (let* ((option (nstring-downcase
			      (trim-subseq string opt-start colon)))
		     (handler (assoc option *mode-option-handlers*
				     :test #'string=)))
		(declare (simple-string option))
		(cond
		 (handler
		  (let ((result (funcall (cdr handler) buffer
					 (trim-subseq string (1+ colon) semi))))
		    (when (string= option "mode")
		      (setq no-major-mode (not result)))))
		 (t (message "Unknown file option: ~S" option)))
		(when (= semi end) (return nil)))))
	   (t
	    ;; Old style mode comment.
	    (setq no-major-mode nil)
	    (funcall (cdr (assoc "mode" *mode-option-handlers* :test #'string=))
		     buffer (trim-subseq string start end)))))))
    (when (and no-major-mode type)
      (let ((hook (assoc (string-downcase type) *file-type-hooks*
			 :test #'string=)))
	(when hook (funcall (cdr hook) buffer type))))))



;;;; File options and file type hooks.

(defmacro define-file-option (name lambda-list &body body)
  "Define-File-Option Name (Buffer Value) {Form}*
   Defines a new file option to be user in the -*- line at the top of a file.
   The body is evaluated with Buffer bound to the buffer the file has been read
   into and Value to the string argument to the option."
  (let ((name (string-downcase name)))
    `(setf (cdr (or (assoc ,name *mode-option-handlers*  :test #'string=)
		    (car (push (cons ,name nil) *mode-option-handlers*))))
	   #'(lambda ,lambda-list ,@body))))

(define-file-option "Mode" (buffer str)
  (let ((seen-major-mode-p nil)
	(lastpos 0))
    (loop
      (let* ((pos (position #\, str :start lastpos))
	     (substr (trim-subseq str lastpos pos)))
	(cond ((getstring substr *mode-names*)
	       (cond ((mode-major-p substr)
		      (when seen-major-mode-p
			(loud-message
			 "Major mode already processed. Using ~S now."
			 substr))
		      (setf seen-major-mode-p t)
		      (setf (buffer-major-mode buffer) substr))
		     (t
 		      (setf (buffer-minor-mode buffer substr) t))))
	      (t
	       (loud-message "~S is not a defined mode -- ignored." substr)))
	(unless pos
	  (return seen-major-mode-p))
	(setf lastpos (1+ pos))))))


(defmacro define-file-type-hook (type-list (buffer type) &body body)
  "Define-File-Type-Hook ({Type}*) (Buffer Type) {Form}*
  Define some code to be evaluated when a file having one of the specified
  Types is read by a file command.  Buffer is bound to the buffer the
  file is in, and Type is the actual type read."
  (let ((fun (gensym)) (str (gensym)))
    `(flet ((,fun (,buffer ,type) ,@body))
       (dolist (,str ',(mapcar #'string-downcase type-list))
	 (setf (cdr (or (assoc ,str *file-type-hooks*  :test #'string=)
			(car (push (cons ,str nil) *file-type-hooks*))))
	       #',fun)))))

(define-file-type-hook ("pas" "pasmac" "macro" "defs" "spc" "bdy")
  		       (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Pascal"))

(define-file-type-hook ("lisp" "slisp" "l" "lsp" "mcl") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Lisp"))

(define-file-type-hook ("txt" "text" "tx") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Text"))



;;;; Support for file hacking commands:

(defhvar "Pathname Defaults"
  "This variable contains a pathname which is used to supply defaults
   when we don't have anything better."
  :value (pathname "gazonk.del"))

(defhvar "Last Resort Pathname Defaults"
  "This variable contains a pathname which is used to supply defaults when
   we don't have anything better, but unlike \"Pathname Defaults\", this is
   never set to some buffer's pathname."
  :value (pathname "gazonk"))

(defhvar "Last Resort Pathname Defaults Function"
  "This variable contains a function that is called when a default pathname is
   needed, the buffer has no pathname, and the buffer's name is not entirely
   composed of alphanumerics.  The default value is a function that simply
   returns \"Last Resort Pathname Defaults\".  The function must take a buffer
   as an argument, and it must return some pathname."
  :value #'(lambda (buffer)
	     (declare (ignore buffer))
	     (merge-pathnames (value last-resort-pathname-defaults)
			      (value pathname-defaults))))

(defun buffer-default-pathname (buffer)
  "Returns \"Buffer Pathname\" if it is bound.  If it is not, and buffer's name
   is composed solely of alphnumeric characters, then return a pathname formed
   from the buffer's name.  If the buffer's name has other characters in it,
   then return the value of \"Last Resort Pathname Defaults Function\" called
   on buffer."
  (or (buffer-pathname buffer)
      (if (every #'alphanumericp (the simple-string (buffer-name buffer)))
	  (merge-pathnames (make-pathname :name (buffer-name buffer))
			   (value pathname-defaults))
	  (funcall (value last-resort-pathname-defaults-function) buffer))))


(defun pathname-to-buffer-name (pathname)
  "Returns a simple-string using components from pathname."
  (let ((pathname (pathname pathname)))
    (concatenate 'simple-string
		 (file-namestring pathname)
		 " "
		 (directory-namestring pathname))))



;;;; File hacking commands.

(defcommand "Process File Options" (p)
  "Reprocess this buffer's file options."
  "Reprocess this buffer's file options."
  (declare (ignore p))
  (process-file-options (current-buffer)))

(defcommand "Insert File" (p &optional pathname (buffer (current-buffer)))
  "Inserts a file which is prompted for into the current buffer at the point.
  The prefix argument is ignored."
  "Inserts the file named by Pathname into Buffer at the point."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file :default (buffer-default-pathname buffer)
				  :prompt "Insert File: "
				  :help "Name of file to insert")))
	 (point (buffer-point buffer))
	 ;; start and end will be deleted by undo stuff
	 (start (copy-mark point :right-inserting))
	 (end (copy-mark point :left-inserting))
	 (region (region start end)))
    (setv pathname-defaults pn)
    (push-buffer-mark (copy-mark end))
    (read-file pn end)
    (make-region-undo :delete "Insert File" region)))

(defcommand "Write Region" (p &optional pathname)
  "Writes the current region to a file. "
  "Writes the current region to a file. "
  (declare (ignore p))
  (let ((region (current-region))
	(pn (or pathname
		(prompt-for-file :prompt "File to Write: "
				 :help "The name of the file to write the region to. "
				 :default (buffer-default-pathname
					   (current-buffer))
				 :must-exist nil))))
    (write-file region pn)
    (message "~A written." (namestring (truename pn)))))



;;;; Visiting and reverting files.

(defcommand "Visit File" (p &optional pathname (buffer (current-buffer)))
  "Replaces the contents of Buffer with the file Pathname.  The prefix
   argument is ignored.  The buffer is set to be writable, so its region
   can be deleted."
  "Replaces the contents of the current buffer with the text in the file
   which is prompted for.  The prefix argument is, of course, ignored p times."
  (declare (ignore p))
  (when (and (buffer-modified buffer)
	     (prompt-for-y-or-n :prompt "Buffer is modified, save it? "))
    (save-file-command () buffer))
  (let ((pn (or pathname
		(prompt-for-file :prompt "Visit File: "
				 :must-exist nil
				 :help "Name of file to visit."
				 :default (buffer-default-pathname buffer)))))
    (setf (buffer-writable buffer) t)
    (read-buffer-file pn buffer)
    (let ((n (pathname-to-buffer-name (buffer-pathname buffer))))
      (unless (getstring n *buffer-names*)
	(setf (buffer-name buffer) n))
      (warn-about-visit-file-buffers buffer))))

(defun warn-about-visit-file-buffers (buffer)
  (let ((buffer-pn (buffer-pathname buffer)))
    (dolist (b *buffer-list*)
      (unless (eq b buffer)
	(let ((bpn (buffer-pathname b)))
	  (when (equal bpn buffer-pn)
	    (loud-message "Buffer ~A also contains ~A."
			  (buffer-name b) (namestring buffer-pn))
	    (return)))))))


(defhvar "Revert File Confirm"
  "If this is true, Revert File will prompt before reverting."
  :value t)

(defcommand "Revert File" (p)
  "Unless in Save Mode, reads in the last saved version of the file in
   the current buffer. When in Save Mode, reads in the last checkpoint or
   the last saved version, whichever is more recent. An argument will always
   force Revert File to use the last saved version. In either case, if the
   buffer has been modified and \"Revert File Confirm\" is true, then Revert
   File will ask for confirmation beforehand. An attempt is made to maintain
   the point's relative position."
  "With an argument reverts to the last saved version of the file in the
   current buffer. Without, reverts to the last checkpoint or last saved
   version, whichever is more recent."
  (let* ((buffer (current-buffer))
	 (buffer-pn (buffer-pathname buffer))
	 (point (current-point))
	 (lines (1- (count-lines (region (buffer-start-mark buffer) point)))))
    (multiple-value-bind (revert-pn used-checkpoint)
			 (if p buffer-pn (revert-pathname buffer))
      (unless revert-pn
	(editor-error "No file associated with buffer to revert to!"))
      (when (or (not (value revert-file-confirm))
		(not (buffer-modified buffer))
		(prompt-for-y-or-n
		 :prompt
		 "Buffer contains changes, are you sure you want to revert? "
		 :help (list
 "Reverting the file will undo any changes by reading in the last ~
 ~:[saved version~;checkpoint file~]." used-checkpoint)
		 :default t))
	(read-buffer-file revert-pn buffer)
	(when used-checkpoint
	  (setf (buffer-modified buffer) t)
	  (setf (buffer-pathname buffer) buffer-pn)
	  (message "Reverted to checkpoint file ~A." (namestring revert-pn)))
	(unless (line-offset point lines)
	  (buffer-end point))))))

;;; REVERT-PATHNAME -- Internal
;;;
;;; If in Save Mode, return either the checkpoint pathname or the buffer
;;; pathname whichever is more recent. Otherwise return the buffer-pathname
;;; if it exists. If neither file exists, return NIL.
;;; 
(defun revert-pathname (buffer)
  (let* ((buffer-pn (buffer-pathname buffer))
	 (buffer-pn-date (file-write-date buffer-pn))
	 (checkpoint-pn (get-checkpoint-pathname buffer))
	 (checkpoint-pn-date (and checkpoint-pn
				  (file-write-date checkpoint-pn))))
    (cond (checkpoint-pn-date
	   (if (> checkpoint-pn-date (or buffer-pn-date 0))
	       (values checkpoint-pn t)
	       (values buffer-pn nil)))
	  (buffer-pn-date (values buffer-pn nil))
	  (t (values nil nil)))))



;;;; Find file.

(defcommand "Find File" (p &optional pathname)
  "Visit a file in its own buffer.
   If the file is already in some buffer, select that buffer,
   otherwise make a new buffer with the same name as the file and
   read the file into it."
  "Make a buffer containing the file Pathname current, creating a buffer
   if necessary.  The buffer is returned."
  (declare (ignore p))
  (let* ((pn (or pathname
		 (prompt-for-file 
		  :prompt "Find File: "
		  :must-exist nil
		  :help "Name of file to read into its own buffer."
		  :default (buffer-default-pathname (current-buffer)))))
	 (buffer (find-file-buffer pn)))
    (change-to-buffer buffer)
    buffer))

(defun find-file-buffer (pathname)
  "Return a buffer assoicated with the file Pathname, reading the file into a
   new buffer if necessary.  The second value is T if we created a buffer, NIL
   otherwise.  If the file has already been read, we check to see if the file
   has been modified on disk since it was read, giving the user various
   recovery options."
  (let* ((pathname (pathname pathname))
	 (trial-pathname (or (probe-file pathname)
			     (merge-pathnames pathname (default-directory))))
	 (found (find trial-pathname (the list *buffer-list*)
		     :key #'buffer-pathname :test #'equal)))
    (cond ((not found)
	   (let* ((name (pathname-to-buffer-name trial-pathname))
		  (found (getstring name *buffer-names*))
		  (use (if found
			   (prompt-for-buffer
			    :prompt "Buffer to use: "
			    :help
  "Buffer name in use; give another buffer name, or confirm to reuse."
			    :default found :must-exist nil)
			   (make-buffer name)))
		  (buffer (if (stringp use) (make-buffer use) use)))
	     (when (and (buffer-modified buffer)
			(prompt-for-y-or-n :prompt
					   "Buffer is modified, save it? "))
	       (save-file-command () buffer))
	     (read-buffer-file pathname buffer)
	     (values buffer (stringp use))))
	  ((check-disk-version-consistent pathname found)
	   (values found nil))
	  (t
	   (read-buffer-file pathname found)
	   (values found nil)))))

;;; Check-Disk-Version-Consistent  --  Internal
;;;
;;;    Check that Buffer contains a valid version of the file Pathname,
;;; harrassing the user if not.  We return true if the buffer is O.K., and
;;; false if the file should be read. 
;;;
(defun check-disk-version-consistent (pathname buffer)
  (let ((ndate (file-write-date pathname))
	(odate (buffer-write-date buffer)))
    (cond ((not (and ndate odate (/= ndate odate)))
	   t)
	  ((buffer-modified buffer)
	   (beep)
	   (clear-input)
	   (command-case (:prompt (list
 "File has been changed on disk since it was read and you have made changes too!~
 ~%Read in the disk version of ~A? [Y] " (namestring pathname))
			  :help
 "The file in disk has been changed since Hemlock last saved it, meaning that
 someone else has probably overwritten it.  Since the version read into Hemlock
 has been changed as well, the two versions may have inconsistent changes.  If
 this is the case, it would be a good idea to save your changes in another file
 and compare the two versions.
 
 Type one of the following commands:")
	     ((:confirm :yes)
 "Prompt for a file to write the buffer out to, then read in the disk version."
	      (write-buffer-file
	       buffer
	       (prompt-for-file
		:prompt "File to save changes in: "
		:help (list "Save buffer ~S to this file before reading ~A."
			    (buffer-name buffer) (namestring pathname))
		:must-exist nil
		:default (buffer-default-pathname buffer)))
	      nil)
	     (:no
	      "Change to the buffer without reading the new version."
	      t)
	     (#\r
	      "Read in the new version, clobbering the changes in the buffer."
	      nil)))
	   (t
	    (not (prompt-for-yes-or-no :prompt
				       (list
 "File has been changed on disk since it was read.~
 ~%Read in the disk version of ~A? "
					(namestring pathname))
				       :help
 "Type Y to read in the new version or N to just switch to the buffer."
				       :default t))))))


(defhvar "Read File Hook"
  "These functions are called when a file is read into a buffer.  Each function
   must take two arguments -- the buffer the file was read into and whether the
   file existed (non-nil) or not (nil).")

(defun read-buffer-file (pathname buffer)
  "Delete the buffer's region, and uses READ-FILE to read pathname into it.
   If the file exists, set the buffer's write date to the file's; otherwise,
   MESSAGE that this is a new file and set the buffer's write date to nil.
   Move buffer's point to the beginning, set the buffer unmodified.  If the
   file exists, set the buffer's pathname to the probed pathname; else, set it
   to pathname merged with DEFAULT-DIRECTORY.  Set \"Pathname Defaults\" to the
   same thing.  Process the file options, and then invoke \"Read File Hook\"."
  (setf (buffer-writable buffer) t)
  (delete-region (buffer-region buffer))
  (let* ((pathname (pathname pathname))
	 (probed-pathname (probe-file pathname)))
    (cond (probed-pathname
	   (read-file probed-pathname (buffer-point buffer))
	   (setf (buffer-write-date buffer) (file-write-date probed-pathname)))
	  (t
	   (message "(New File)")
	   (setf (buffer-write-date buffer) nil)))
    (buffer-start (buffer-point buffer))
    (setf (buffer-modified buffer) nil)
    (let ((stored-pathname (or probed-pathname
			       (merge-pathnames pathname (default-directory)))))
      (setf (buffer-pathname buffer) stored-pathname)
      (setf (value pathname-defaults) stored-pathname)
      (process-file-options buffer stored-pathname)
      (invoke-hook read-file-hook buffer probed-pathname))))



;;;; File writing.

(defhvar "Add Newline at EOF on Writing File"
  "This controls whether WRITE-BUFFER-FILE adds a newline at the end of the
   file when it ends at the end of a non-empty line.  When set, this may be
   :ask-user and WRITE-BUFFER-FILE will prompt; otherwise, just add one and
   inform the user.  When nil, never add one and don't ask."
  :value :ask-user)

(defhvar "Keep Backup Files"
  "When set, .BAK files will be saved upon file writing.  This defaults to nil."
  :value nil)

(defhvar "Write File Hook"
  "These functions are called when a buffer has been written.  Each function
   must take the buffer as an argument.")

(defun write-buffer-file (buffer pathname)
  "Write's buffer to pathname.  This assumes pathname is somehow related to
   the buffer's pathname, and if the buffer's write date is not the same as
   pathname's, then this prompts the user for confirmation before overwriting
   the file.  This consults \"Add Newline at EOF on Writing File\" and
   interacts with the user if necessary.  This sets \"Pathname Defaults\", and
   the buffer is marked unmodified.  The buffer's pathname and write date are
   updated, and the buffer is renamed according to the new pathname if possible.
   This invokes \"Write File Hook\"."
  (let ((buffer-pn (buffer-pathname buffer)))
    (let ((date (buffer-write-date buffer))
	  (file-date (when (probe-file pathname) (file-write-date pathname))))
      (when (and buffer-pn date file-date
		 (equal (make-pathname :version nil :defaults buffer-pn)
			(make-pathname :version nil :defaults pathname))
		 (/= date file-date))
	(unless (prompt-for-yes-or-no :prompt (list
 "File has been changed on disk since it was read.~%Overwrite ~A anyway? "
 (namestring buffer-pn))
				      :help
				      "Type No to abort writing the file or Yes to overwrite the disk version."
				      :default nil)
	  (editor-error "Write aborted."))))
    (let ((val (value add-newline-at-eof-on-writing-file)))
      (when val
	(let ((end (buffer-end-mark buffer)))
	  (unless (start-line-p end)
	    (when (if (eq val :ask-user)
		      (prompt-for-y-or-n
		       :prompt
		       (list "~A~%File does not have a newline at EOF, add one? "
			     (buffer-name buffer))
		       :default t)
		      t)
	      (insert-character end #\newline)
	      (message "Added newline at EOF."))))))
    (setv pathname-defaults pathname)
    (write-file (buffer-region buffer) pathname)
    (let ((tn (truename pathname)))
      (message "~A written." (namestring tn))
      (setf (buffer-modified buffer) nil)
      (unless (equal tn buffer-pn)
	(setf (buffer-pathname buffer) tn))
      (setf (buffer-write-date buffer) (file-write-date tn))
      (let ((name (pathname-to-buffer-name tn)))
	(unless (getstring name *buffer-names*)
	  (setf (buffer-name buffer) name)))))
  (invoke-hook write-file-hook buffer))
 
(defcommand "Write File" (p &optional pathname (buffer (current-buffer)))
  "Writes the contents of Buffer, which defaults to the current buffer to
  the file named by Pathname.  The prefix argument is ignored."
  "Prompts for a file to write the contents of the current Buffer to.
  The prefix argument is ignored."
  (declare (ignore p))
  (write-buffer-file
   buffer
   (or pathname
       (prompt-for-file :prompt "Write File: "
			:must-exist nil
			:help "Name of file to write to"
			:default (buffer-default-pathname buffer)))))

(defcommand "Save File" (p &optional (buffer (current-buffer)))
  "Writes the contents of the current buffer to the associated file.  If there
  is no associated file, once is prompted for."
  "Writes the contents of the current buffer to the associated file."
  (declare (ignore p))
  (when (or (buffer-modified buffer)
	    (prompt-for-y-or-n 
	     :prompt "Buffer is unmodified, write it anyway? "
	     :default t))
    (write-buffer-file
     buffer
     (or (buffer-pathname buffer)
	 (prompt-for-file :prompt "Save File: "
			  :help "Name of file to write to"
			  :default (buffer-default-pathname buffer)
			  :must-exist nil)))))

(defhvar "Save All Files Confirm"
  "When non-nil, prompts for confirmation before writing each modified buffer."
  :value t)

(defcommand "Save All Files" (p)
  "Saves all modified buffers in their associated files.
  If a buffer has no associated file it is ignored even if it is modified.."
  "Saves each modified buffer that has a file."
  (declare (ignore p))
  (let ((saved-count 0))
    (dolist (b *buffer-list*)
      (let ((pn (buffer-pathname b))
	    (name (buffer-name b)))
	(when
	    (and (buffer-modified b)
		 pn
		 (or (not (value save-all-files-confirm))
		     (prompt-for-y-or-n
		      :prompt (list
			       "Write ~:[buffer ~A as file ~S~;file ~*~S~], ~
			       Y or N: "
			       (string= (pathname-to-buffer-name pn) name)
			       name (namestring pn))
		      :default t)))
	  (write-buffer-file b pn)
	  (incf saved-count))))
    (if (zerop saved-count)
	(message "No files were saved.")
	(message "Saved ~S file~:P." saved-count))))

(defcommand "Save All Files and Exit" (p)
  "Save all modified buffers in their associated files and exit;
  a combination of \"Save All Files\" and \"Exit Hemlock\"."
  "Do a save-all-files-command and then an exit-hemlock."
  (declare (ignore p))
  (save-all-files-command ())
  (exit-hemlock))

(defcommand "Backup File" (p)
  "Write the buffer to a file without changing the associated name."
  "Write the buffer to a file without changing the associated name."
  (declare (ignore p))
  (let ((file (prompt-for-file :prompt "Backup to File: "
			       :help
 "Name of a file to backup the current buffer in."
			       :default (buffer-default-pathname (current-buffer))
			       :must-exist nil)))
    (write-file (buffer-region (current-buffer)) file)
    (message "~A written." (namestring (truename file)))))



;;;; Buffer hacking commands:

(defvar *buffer-history* ()
  "A list of buffers, in order from most recently to least recently selected.")

(defun previous-buffer ()
  "Returns some previously selected buffer that is not the current buffer.
   Returns nil if no such buffer exists."
  (let ((b (car *buffer-history*)))
    (or (if (eq b (current-buffer)) (cadr *buffer-history*) b)
	(find-if-not #'(lambda (x)
			 (or (eq x (current-buffer))
			     (eq x *echo-area-buffer*)))
		     (the list *buffer-list*)))))

;;; ADD-BUFFER-HISTORY-HOOK makes sure every buffer will be visited by
;;; "Circulate Buffers" even if it has never been before.
;;;
(defun add-buffer-history-hook (buffer)
  (let ((ele (last *buffer-history*))
	(new-stuff (list buffer)))
    (if ele
	(setf (cdr ele) new-stuff)
	(setf *buffer-history* new-stuff))))
;;;
(add-hook make-buffer-hook 'add-buffer-history-hook)

;;; DELETE-BUFFER-HISTORY-HOOK makes sure we never end up in a dead buffer.
;;;
(defun delete-buffer-history-hook (buffer)
  (setq *buffer-history* (delq buffer *buffer-history*)))
;;;
(add-hook delete-buffer-hook 'delete-buffer-history-hook)
  
(defun change-to-buffer (buffer)
  "Switches to buffer in the current window maintaining *buffer-history*."
  (setq *buffer-history*
	(cons (current-buffer) (delq (current-buffer) *buffer-history*)))
  (setf (current-buffer) buffer)
  (setf (window-buffer (current-window)) buffer))

(defun delete-buffer-if-possible (buffer)
  "Deletes a buffer if at all possible.  If buffer is the only buffer, other
   than the echo area, signals an error.  Otherwise, find some recently current
   buffer, and make all of buffer's windows display this recent buffer.  If
   buffer is current, set the current buffer to be this recently current
   buffer."
  (let ((new-buf (flet ((frob (b)
			  (or (eq b buffer) (eq b *echo-area-buffer*))))
		   (or (find-if-not #'frob (the list *buffer-history*))
		       (find-if-not #'frob (the list *buffer-list*))))))
    (unless new-buf
      (error "Cannot delete only buffer ~S." buffer))
    (dolist (w (buffer-windows buffer))
      (setf (window-buffer w) new-buf))
    (when (eq buffer (current-buffer))
      (setf (current-buffer) new-buf)))
  (delete-buffer buffer))


(defvar *create-buffer-count* 0)

(defcommand "Create Buffer" (p &optional buffer-name)
  "Create a new buffer.  If a buffer with the specified name already exists,
   then go to it."
  "Create or go to the buffer with the specifed name."
  (declare (ignore p))
  (let ((name (or buffer-name
		  (prompt-for-buffer :prompt "Create Buffer: "
				     :default-string
				     (format nil "Buffer ~D"
					     (incf *create-buffer-count*))
				     :must-exist nil))))
    (if (bufferp name)
	(change-to-buffer name)
	(change-to-buffer (or (getstring name *buffer-names*)
			      (make-buffer name))))))

(defcommand "Select Buffer" (p)
  "Select a different buffer.
   The buffer to go to is prompted for."
  "Select a different buffer.
   The buffer to go to is prompted for."
  (declare (ignore p))
  (let ((buf (prompt-for-buffer :prompt "Select Buffer: "
				:default (previous-buffer))))
    (when (eq buf *echo-area-buffer*)
      (editor-error "Cannot select Echo Area buffer."))
    (change-to-buffer buf)))


(defvar *buffer-history-ptr* ()
  "The successively previous buffer to the current buffer.")

(defcommand "Select Previous Buffer" (p)
  "Select the buffer selected before this one.  If called repeatedly
   with an argument, select the successively previous buffer to the
   current one leaving the buffer history as it is."
  "Select the buffer selected before this one."
  (if p
      (circulate-buffers-command nil)
      (let ((b (previous-buffer)))
	(unless b (editor-error "No previous buffer."))
	(change-to-buffer b)
	;;
	;; If the pointer goes to nil, then "Circulate Buffers" will keep doing
	;; "Select Previous Buffer".
	(setf *buffer-history-ptr* (cddr *buffer-history*))
	(setf (last-command-type) :previous-buffer))))

(defcommand "Circulate Buffers" (p)
  "Advance through buffer history, selecting successively previous buffer."
  "Advance through buffer history, selecting successively previous buffer."
  (declare (ignore p))
  (if (and (eq (last-command-type) :previous-buffer)
	   *buffer-history-ptr*) ;Possibly nil if never CHANGE-TO-BUFFER.
      (let ((b (pop *buffer-history-ptr*)))
	(when (eq b (current-buffer))
	  (setf b (pop *buffer-history-ptr*)))
	(unless b
	  (setf *buffer-history-ptr*
		(or (cdr *buffer-history*) *buffer-history*))
	  (setf b (car *buffer-history*)))
	(setf (current-buffer) b)
	(setf (window-buffer (current-window)) b)
	(setf (last-command-type) :previous-buffer))
      (select-previous-buffer-command nil)))
  

(defcommand "Buffer Not Modified" (p)
  "Make the current buffer not modified."
  "Make the current buffer not modified."
  (declare (ignore p))
  (setf (buffer-modified (current-buffer)) nil)
  (message "Buffer marked as unmodified."))

(defcommand "Check Buffer Modified" (p)
  "Say whether the buffer is modified or not."
  "Say whether the current buffer is modified or not."
  (declare (ignore p))
  (clear-echo-area)
  (message "Buffer ~S ~:[is not~;is~] modified."
	   (buffer-name (current-buffer)) (buffer-modified (current-buffer))))

(defcommand "Set Buffer Read-Only" (p)
  "Toggles the read-only flag for the current buffer."
  "Toggles the read-only flag for the current buffer."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (message "Buffer ~S is now ~:[read-only~;writable~]."
	     (buffer-name buffer)
	     (setf (buffer-writable buffer) (not (buffer-writable buffer))))))

(defcommand "Set Buffer Writable" (p)
  "Make the current buffer modifiable."
  "Make the current buffer modifiable."
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (setf (buffer-writable buffer) t)
    (message "Buffer ~S is now writable." (buffer-name buffer))))

(defcommand "Kill Buffer" (p &optional buffer-name)
  "Prompts for a buffer to delete.
  If the buffer is modified, then let the user save the file before doing so.
  When deleting the current buffer, prompts for a new buffer to select.  If
  a buffer other than the current one is deleted then any windows into it
  are deleted."
  "Delete buffer Buffer-Name, doing sensible things if the buffer is displayed
  or current."
  (declare (ignore p))
  (let ((buffer (if buffer-name
		    (getstring buffer-name *buffer-names*)
		    (prompt-for-buffer :prompt "Kill Buffer: "
				       :default (current-buffer)))))
    (if (not buffer)
	(editor-error "No buffer named ~S" buffer-name))
    (if (and (buffer-modified buffer)
	     (prompt-for-y-or-n :prompt "Save it first? "))
	(save-file-command () buffer))
    (if (eq buffer (current-buffer))
	(let ((new (prompt-for-buffer :prompt "New Buffer: "
				      :default (previous-buffer)
 :help "Buffer to change to after the current one is killed.")))
	  (when (eq new buffer)
	    (editor-error "You must select a different buffer."))
	  (dolist (w (buffer-windows buffer))
	    (setf (window-buffer w) new))
	  (setf (current-buffer) new))
	(dolist (w (buffer-windows buffer))
	  (delete-window w)))
    (delete-buffer buffer)))

(defcommand "Rename Buffer" (p)
  "Change the current buffer's name.
  The name, which is prompted for, defaults to the name of the associated
  file."
  "Change the name of the current buffer."
  (declare (ignore p))
  (let* ((buf (current-buffer))
	 (pn (buffer-pathname buf))
	 (name (if pn (pathname-to-buffer-name pn) (buffer-name buf)))
	 (new (prompt-for-string :prompt "New Name: "
				 :help "Give a new name for the current buffer"
				 :default name)))
    (multiple-value-bind (entry foundp) (getstring new *buffer-names*)
      (cond ((or (not foundp) (eq entry buf))
	     (setf (buffer-name buf) new))
	    (t (editor-error "Name ~S already in use." new))))))


(defcommand "Insert Buffer" (p)
  "Insert the contents of a buffer.
  The name of the buffer to insert is prompted for."
  "Prompt for a buffer to insert at the point."
  (declare (ignore p))
  (let ((point (current-point))
	(region (buffer-region (prompt-for-buffer
				:default (previous-buffer) 
				:help 
				"Type the name of a buffer to insert."))))
    ;;
    ;; start and end will be deleted by undo stuff
    (let ((save (region (copy-mark point :right-inserting)
			(copy-mark point :left-inserting))))
      (push-buffer-mark (copy-mark point))
      (insert-region point region)
      (make-region-undo :delete "Insert Buffer" save))))



;;;; File utility commands:

(defcommand "Directory" (p)
  "Do a directory into a pop-up window.  If an argument is supplied, then
   dot files are listed too (as with ls -a).  Prompts for a pathname which
   may contain wildcards in the name and type."
  "Do a directory into a pop-up window."
  (let* ((dpn (value pathname-defaults))
	 (pn (prompt-for-file
	      :prompt "Directory: "
	      :help "Pathname to do directory on."
	      :default (make-pathname :device (pathname-device dpn)
				      :directory (pathname-directory dpn))
	      :must-exist nil)))
    (setf (value pathname-defaults) (merge-pathnames pn dpn))
    (with-pop-up-display (s)
      (print-directory pn s :all p))))

(defcommand "Verbose Directory" (p)
  "Do a directory into a pop-up window.  If an argument is supplied, then
   dot files are listed too (as with ls -a).  Prompts for a pathname which
   may contain wildcards in the name and type."
  "Do a directory into a pop-up window."
  (let* ((dpn (value pathname-defaults))
	 (pn (prompt-for-file
	      :prompt "Verbose Directory: "
	      :help "Pathname to do directory on."
	      :default (make-pathname :device (pathname-device dpn)
				      :directory (pathname-directory dpn))
	      :must-exist nil)))
    (setf (value pathname-defaults) (merge-pathnames pn dpn))
    (with-pop-up-display (s)
      (print-directory pn s :verbose t :all p))))



;;;; Change log stuff:

(define-file-option "Log" (buffer value)
  (defhvar "Log File Name"
    "The name of the file for the change log for the file in this buffer."
    :buffer buffer  :value value))

(defhvar "Log Entry Template"
  "The format string used to generate the template for a change-log entry.
  Three arguments are given: the file, the date (create if available, now
  otherwise) and the file author, or NIL if not available.  The last \"@\"
  is deleted and the point placed where it was."
  :value "~A, ~A, Edit by ~:[???~;~:*~:(~A~)~].~%  @~2%")

(defmode "Log"
  :major-p t
  :setup-function
  #'(lambda (buffer)
      (setf (buffer-minor-mode buffer "Fill") t))
  :cleanup-function
  #'(lambda (buffer)
      (setf (buffer-minor-mode buffer "Fill") nil)))

(defhvar "Fill Prefix" "The fill prefix in Log mode."
  :value "  "  :mode "Log")

(define-file-type-hook ("log") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Log"))

(defun universal-time-to-string (ut)
  (multiple-value-bind (sec min hour day month year)
		       (decode-universal-time ut)
    (format nil "~2,'0D-~A-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
	    day (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
			  "Sep" "Oct" "Nov" "Dec")
		       (1- month))
	    (rem year 100)
	    hour min sec)))

(defvar *back-to-@-pattern* (new-search-pattern :character :backward #\@))
(defcommand "Log Change" (p)
  "Make an entry in the change-log file for this buffer.
  Saves the file in the current buffer if it is modified, then finds the file
  specified in the \"Log\" file option, adds the template for a change-log
  entry at the beginning, then does a recursive edit, saving the log file on
  exit."
  "Find the change-log file as specified by \"Log File Name\" and edit it."
  (declare (ignore p))
  (unless (hemlock-bound-p 'log-file-name)
    (editor-error "No log file defined."))
  (let* ((buffer (current-buffer))
	 (pathname (buffer-pathname buffer)))
    (when (or (buffer-modified buffer) (null pathname))
      (save-file-command ()))
    (unwind-protect
	(progn
	  (find-file-command nil (merge-pathnames
				  (value log-file-name)
				  (buffer-default-pathname buffer)))
	  (let ((point (current-point)))
	    (buffer-start point)
	    (with-output-to-mark (s point :full)
	      (format s (value log-entry-template)
		      (namestring pathname)
		      (universal-time-to-string
		       (or (file-write-date pathname)
			   (get-universal-time)))
		      (file-author pathname)))
	    (when (find-pattern point *back-to-@-pattern*)
	      (delete-characters point 1)))
	  (do-recursive-edit)
	  (when (buffer-modified (current-buffer)) (save-file-command ())))
      (if (member buffer *buffer-list* :test #'eq)
	  (change-to-buffer buffer)
	  (editor-error "Old buffer has been deleted.")))))



;;;; Window hacking commands:

(defcommand "Next Window" (p)
  "Change the current window to be the next window and the current buffer
  to be it's buffer."
  "Go to the next window.
  If the next window is the bottom window then wrap around to the top window."
  (declare (ignore p))
  (let* ((next (next-window (current-window)))
	 (buffer (window-buffer next)))
    (setf (current-buffer) buffer  (current-window) next)))

(defcommand "Previous Window" (p)
  "Change the current window to be the previous window and the current buffer
  to be it's buffer."
  "Go to the previous window.
  If the Previous window is the top window then wrap around to the bottom."
  (declare (ignore p))
  (let* ((previous (previous-window (current-window)))
	 (buffer (window-buffer previous)))
    (setf (current-buffer) buffer  (current-window) previous)))

(defcommand "Split Window" (p)
  "Make a new window by splitting the current window.
   The new window is made the current window and displays starting at
   the same place as the current window."
  "Create a new window which displays starting at the same place
   as the current window."
  (declare (ignore p))
  (let ((new (make-window (window-display-start (current-window)))))
    (unless new (editor-error "Could not make a new window."))
    (setf (current-window) new)))

(defcommand "New Window" (p)
  "Make a new window and go to it.
   The window will display the same buffer as the current one."
  "Create a new window which displays starting at the same place
   as the current window."
  (declare (ignore p))
  (let ((new (make-window (window-display-start (current-window))
			  :ask-user t)))
    (unless new (editor-error "Could not make a new window."))
    (setf (current-window) new)))

(defcommand "Delete Window" (p)
  "Delete the current window, going to the previous window."
  "Delete the window we are in, going to the previous window."
  (declare (ignore p))
  (when (= (length *window-list*) 2)
    (editor-error "Cannot delete only window."))
  (let ((window (current-window)))
    (previous-window-command nil)  
    (delete-window window)))

(defcommand "Line to Top of Window" (p)
  "Move current line to top of window."
  "Move current line to top of window."
  (declare (ignore p))
  (with-mark ((mark (current-point)))
    (move-mark (window-display-start (current-window)) (line-start mark))))

(defcommand "Delete Next Window" (p)
  "Deletes the next window on display."
  "Deletes then next window on display."
  (declare (ignore p))
  (if (<= (length *window-list*) 2)
      (editor-error "Cannot delete only window")
      (delete-window (next-window (current-window)))))

(defcommand "Go to One Window" (p)
  "Deletes all windows leaving one with the \"Default Initial Window X\",
   \"Default Initial Window Y\", \"Default Initial Window Width\", and
   \"Default Initial Window Height\"."
  "Deletes all windows leaving one with the \"Default Initial Window X\",
   \"Default Initial Window Y\", \"Default Initial Window Width\", and
   \"Default Initial Window Height\"."
  (declare (ignore p))
  (let ((win (make-window (window-display-start (current-window))
			  :ask-user t
			  :x (value default-initial-window-x)
			  :y (value default-initial-window-y)
			  :width (value default-initial-window-width)
			  :height (value default-initial-window-height))))
    (setf (current-window) win)
    (dolist (w *window-list*)
      (unless (or (eq w win)
		  (eq w *echo-area-window*))
	(delete-window w)))))

(defcommand "Line to Center of Window" (p)
  "Moves current line to the center of the window."
  "Moves current line to the center of the window."
  (declare (ignore p))
  (center-window (current-window) (current-point)))
