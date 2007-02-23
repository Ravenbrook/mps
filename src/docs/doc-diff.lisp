;;; -*- Package: Hemlock -*-
;;;
;;;    A hack to compare the functions and variables defined by the hemlock
;;; documents with the ones defined in the core.
;;;
;;; Use GROVEL-LABELS.
;;;

(in-package "HEMLOCK")


(defvar *defined-labels* (make-hash-table :test #'equal))

;;; Ignore these because they would be internal (not for the user) if Hemlock
;;; had that kind of definition power.
;;;
(defvar *hvars-to-ignore*
  '(auto-save-state current-package draft-information headers-buffer
    headers-information message-buffer message-information spell-information
    default-message-modeline-fields current-compile-server current-eval-server))

(defvar *cmds-to-ignore*
  '("Beginning Of Parse" "Echo Area Backward Character"
    "Echo Area Backward Word" "Echo Area Delete Previous Character"
    "Echo Area Kill Previous Word" "Do Nothing" "Illegal" "Insert Parse Default"
    "Italic Comment Mode" "Kill Parse" "Lisp Insert )" "Next Parse"
    "Previous Parse" "Start Italic Comment" "Insert ()" "Move over )"
    "Current Compile Server" "Current Eval Server" "Defhvar" "Defindent"))

;;; These do not get removed from *defined-labels* because they are not
;;; command names, variable names, or "HI" function names.  These are now
;;; documented in the Command Implementor's Manual, but we don't want to call
;;; FIND-UNDOCUMENTED-SYMBOLS on these packages due to all the uninteresting
;;; symbols they hold.  In the case of routines defined in the "ED" package,
;;; they aren't exported anyway.
;;;
;;; Do not add names to this list that occur in the ED package and have
;;; asterisks (e.g., specials like *kill-ring* and *last-search-string*).  Use
;;; the variable below, *unimplemented-strings-to-ignore*.
;;;
(defvar *unimplemented-to-ignore*
  '(spell:spell-try-word spell:maybe-read-spell-dictionary spell:spell-root-word
    spell:max-entry-length spell:spell-read-dictionary
    spell:spell-collect-close-words spell:correct-spelling
    spell:spell-add-entry spell:spell-remove-entry spell:spell-root-flags

    ext:translate-character ext:define-keyboard-modifier
    ext:define-mouse-code ext:translate-mouse-character ext:define-keysym

    dired:find-file dired:make-directory dired:delete-file
    dired:pathnames-from-pattern dired:copy-file dired:rename-file

    get-search-pattern current-mark file-compile kill-characters
    indent-region-for-commands display-page-directory previous-buffer
    sentence-offset interactive buffer-default-pathname
    add-definition-dir-translation push-buffer-mark do-active-group
    paragraph-offset word-offset create-slave make-region-undo
    process-file-options pre-command-parse-check top-level-offset fill-region
    pop-buffer-mark region-eval get-current-compile-server mark-top-level-form
    ed page-directory find-file-buffer deactivate-region valid-spot
    buffer-history kill-region string-eval backward-up-list
    define-file-type-hook buffer-history check-region-query-size
    change-to-buffer region-compile current-region mark-paragraph form-offset
    check-region-active read-buffer-file fill-region-by-paragraphs
    forward-up-list define-file-option buffer-mark region-active-p
    inside-defun-p activate-region start-defun-p delete-buffer-if-possible
    get-current-eval-server goto-page write-buffer-file save-for-undo
    eval-form-in-server indent-region in-lisp pathname-to-buffer-name
    page-offset defun-region delete-definition-dir-translation
    delete-horizontal-space supply-generic-pointer-up-function))

;;; This is just like *unimplemented-to-ignore*, but these names are hard to
;;; deal with in *unimplemented-to-ignore* due to one of the following reasons:
;;;    Scribe,
;;;    The name is an example and truly unimplemented, or
;;;    The name has asterisks in core but not in the Scribe label name.
;;;
(defvar *unimplemented-strings-to-ignore*
  '("SAMPLECOMMAND" "SAMPLEVARIABLE"

    "MARK-GTR" "MARK-NEQ" "MARK-LSS" "MARK-LEQ" "MARK-GEQ" "MARK-EQL"
    "LINE-GEQ" "LINE-LSS" "LINE-GTR" "LINE-LEQ"

    "KILL-RING" "LAST-SEARCH-STRING" "EPHEMERALLY-ACTIVE-COMMAND-TYPES"
    "HEMLOCK-BEEP" "LAST-SEARCH-PATTERN" "ACTIVE-FILE-GROUP"

    "ERROR-FUNCTION" "REPORT-FUNCTION" "UPDATE-DEFAULT" "YESP-FUNCTION"
    "CLOBBER-DEFAULT" "RECURSIVE-DEFAULT"))


(defun grovel-labels (aux-files output-file)
  "Read each of the files in the list Aux-Files to find what commands are
  documented, then compare it with the commands defined in core.  We write
  documentation forms to the output-file for things defined but not documented,
  and we put a list of things documented but not implemented in a comment."
  (clrhash *defined-labels*)
  (dolist (labels-file aux-files)
    (with-open-file (s labels-file :direction :input)
      (loop
	(let ((l (read-line s nil nil)))
	  (unless l (return))
	  (multiple-value-bind (kind label)
			       (parse-label l)
	    (when kind
	      (let ((old (gethash label *defined-labels*)))
		(when (and old
			   (not (eq old :hemlock-variable))
			   (not (eq kind :hemlock-variable)))
		  (format t "~S multiply defined as ~S and ~S.~%"
			  label old kind))
		(setf (gethash label *defined-labels*) kind))))))))
  (with-open-file (s output-file :direction :output
		     :if-exists :rename-and-delete)
    (map-undocumented-hemlock-things *command-names* :command s
				     #'document-command *cmds-to-ignore*)
    (terpri s)
    (map-undocumented-hemlock-things *global-variable-names* :hemlock-variable s
				     #'document-variable *hvars-to-ignore*)
    (terpri s)
    (find-undocumented-symbols "HEMLOCK-INTERNALS" s)
    (terpri s)
    (write-line "@begin[comment]" s)
    (let ((ignored-symbols (copy-list *unimplemented-to-ignore*))
	  (ignored-strings (copy-list *unimplemented-strings-to-ignore*)))
      (maphash #'(lambda (name type)
		   (cond ((member name ignored-symbols
				  :test #'string= :key #'symbol-name)
			  (setf ignored-symbols
				(delete name ignored-symbols
					:test #'string= :key #'symbol-name)))
			 ((member name ignored-strings :test #'string=)
			  (setf ignored-strings
				(delete name ignored-strings :test #'string=)))
			 (t
			  (format s "~A ~S is not implemented.~%" type name))))
	       *defined-labels*)
      (when ignored-symbols
	(format s
		"~&*******************  These ignored \"unimplemented\" symbols ~
		 were not used.~%~S~%********************~2%"
		ignored-symbols))
      (when ignored-strings
	(format s
		"~&*******************  These ignored \"unimplemented\" strings ~
		 were not used.~%~S~%********************~2%"
		ignored-strings)))
    (write-line "@end[comment]" s)
    (values)))


;;; Iterate over a string table, checking that each thing has a corresponding
;;; label of the specified kind.  If there is no label, then call the function
;;; with the value and stream.  If the label is the wrong kind, print a comment
;;; on Stream before calling the function.  We also blast the label so we will
;;; know that it was defined.
;;;
(defun map-undocumented-hemlock-things (table kind stream function ignore-stuff)
  (do-strings (string value table)
    (let* ((lab (nstring-upcase (remove #\space string)))
	   (lkind (gethash lab *defined-labels*)))
      (cond ((and (eq kind :command)
		  (member (command-name value) ignore-stuff
			  :test #'string-equal))
	     (setf ignore-stuff
		   (remove (command-name value) ignore-stuff
			   :test #'string-equal)))
	    ((member value ignore-stuff)
	     (setf ignore-stuff (remove value ignore-stuff)))
	    (t
	     (unless (eq lkind kind)
	       (when lkind
		 (format stream
			 "@comment{~S documented as a ~A, ~
			  but defined as a ~A.}~2%"
			 string lkind kind))
	       (funcall function value stream))))
      (remhash lab *defined-labels*)))
  (when ignore-stuff
    (format stream
	    "~&********************  These ignored ~Ss were not used.~%~
	     ~S~%********************~2%"
	    kind ignore-stuff)))



(defvar *undocumented-symbols-to-ignore*
  '(make-xwindow-like-hwindow mark/= default-font input-waiting mark=
    modify-kbdmac-stream delete-line-font-marks font-mark hemlock-output-stream
    command reprompt store-cut-string make-kbdmac-stream window window-font
    delete-font-mark fetch-cut-string fun-defined-from-pathname
    hemlock-region-stream line< buffer mark< move-font-mark
    editor-describe-function enter-window-autoraise ring mark<= search-pattern
    *print-region* mark>= string-table line mark> line> line>= line<=
    after-editor-initializations *invoke-hook* defhvar))

(defun find-undocumented-symbols (package stream)
  (let ((ignore-symbols *undocumented-symbols-to-ignore*))
    (do-external-symbols (sym package)
      (let* ((name (string-trim "*" (symbol-name sym)))
	     (kind (gethash name *defined-labels*)))
	(ecase kind
	  ((nil)
	   (if (member sym ignore-symbols)
	       (setf ignore-symbols (remove sym ignore-symbols))
	       (let ((*standard-output* stream))
		 ;; Bind this to squelch CLOS/DESCRIBE bad interaction.
		 (describe sym)
		 (terpri)
		 (terpri))))
	  ((:function :macro :special-form)
	   (let ((def (cond ((macro-function sym) :macro)
				((special-form-p sym) :special-form)
				((fboundp sym) :function))))
		 (unless (eq kind def)
		   (format stream
			   "@comment{~S is ~:[not defined~;~:*defined as a ~A~]~
			    , but is documented as a ~A}~%" sym def kind))))
	  (:constant
	   (unless (constantp sym)
	     (format stream
		     "@comment{~S is documented as a constant, but isn't ~
		      defined.}~%"
		     sym)))
	  (:variable
	   (unless (or (get sym 'lisp::globally-special)
		       (string= name (symbol-name sym)))
	       (format stream
		       "@comment{~S is documented as a special, but isn't ~
			declared.}~%"
		       sym))))
	(remhash name *defined-labels*)))
    (when ignore-symbols
      (format stream
	      "~&********************  These ignored symbols were not used.~%~
	       ~S~%********************~2%"
	      ignore-symbols))))

  
(defvar *suffix-codes* (make-hash-table :test #'equal))
(setf (gethash "COM" *suffix-codes*) :command)
(setf (gethash "HVAR" *suffix-codes*) :hemlock-variable)
(setf (gethash "FUN" *suffix-codes*) :function)
(setf (gethash "MAC" *suffix-codes*) :macro)
(setf (gethash "SPEC" *suffix-codes*) :special-form)
(setf (gethash "VAR" *suffix-codes*) :variable)
(setf (gethash "CON" *suffix-codes*) :constant)


;;; Parse a line from a Scribe .Aux file, returning the kind of the thing
;;; documented and its name.
;;;
(defun parse-label (entry)
  (let* ((end (search "), Value" entry :start2 28))
	 (hpos (position #\- entry :start 28 :end end :from-end t)))
    (if hpos
	(let* ((suffix (subseq entry (1+ hpos) end))
	       (found (gethash suffix *suffix-codes*)))
	  (if found
	      (values found (subseq entry 28 hpos))
	      (values nil nil)))
	(values nil nil))))


(defun document-command (command stream)
  (format stream "@defcom[com ~S" (command-name command))
  (let ((binds (command-bindings command)))
    (when binds
      (format stream ", bind (")
      (print-command-bindings binds stream)
      (format stream ")"))
    (format stream "]~%~A~%@enddefcom~2%"
	    (command-documentation command))))


(defun document-variable (var stream)
  (let* ((name (variable-name var :global))
	 (len (length name)))
    (unless (string= name "Mode Hook" :start1 (- len 9))
      (format stream "@defhvar[var ~S~@[, val {~(~S~)}~]]~%~A~%@enddefhvar~2%"
	      name (variable-value var :global)
	      (variable-documentation var :global)))))


(defvar *definition-pattern*
  (new-search-pattern :string-insensitive :forward "
@def"))

(defvar *insert-pattern*
  (new-search-pattern :string-insensitive :backward "

"))

(defvar *definition-macros* (make-hash-table :test #'equal))
(setf (gethash "COM" *definition-macros*) :command)
(setf (gethash "HVAR" *definition-macros*) :hemlock-variable)
(setf (gethash "UN" *definition-macros*) :function)
(setf (gethash "MAC" *definition-macros*) :macro)
(setf (gethash "SPEC" *definition-macros*) :special-form)
(setf (gethash "VAR" *definition-macros*) :variable)
(setf (gethash "CON" *definition-macros*) :constant)
(setf (gethash "COM1" *definition-macros*) :command)
(setf (gethash "HVAR1" *definition-macros*) :hemlock-variable)
(setf (gethash "UN1" *definition-macros*) :function)
(setf (gethash "MAC1" *definition-macros*) :macro)
(setf (gethash "SPEC1" *definition-macros*) :special-form)
(setf (gethash "VAR1" *definition-macros*) :variable)
(setf (gethash "CON1" *definition-macros*) :constant)

(defun parse-doc-macro (line)
  (let* ((bracket (or (position #\[ line)
		      (error "No opening #\[ ???")))
	 (name (nstring-upcase (subseq line 4 bracket)))
	 (kind (gethash name *definition-macros*))
	 (nend (case (char line (+ bracket 5))
		 (#\"
		  (position #\" line :start (+ bracket 6)))
		 (#\{
		  (position #\} line :start (+ bracket 6)))
		 (t nil))))
    (cond ((not kind)
	   (format t "Unknown definition macro:~%~A~%" line)
	   (values nil nil))
	  ((not nend)
	   (format t "Can't parse name:~%~A~%" line)
	   (values nil nil))
	  (t
	   (values kind (subseq line (+ bracket 6) nend))))))


(defun annotate-with-online-documentation (input-file output-file)
  "Take a Scribe input file and produce a Scribe output file with the online
  documentation for each thing inserted before the offline documentation."
  (let* ((temp-buffer (make-buffer "Annotate Temporary"))
	 (point (buffer-point temp-buffer)))
    (unwind-protect
	(progn
	  (read-file input-file point)
	  (buffer-start point)
	  (loop
	    (unless (find-pattern point *definition-pattern*)
	      (return))
	    (line-offset point 1)
	    (multiple-value-bind
		(kind name)
		(parse-doc-macro (line-string (mark-line point)))
	      (when kind
		(with-mark ((insert point :left-inserting))
		  (unless (find-pattern insert *insert-pattern*)
		    (buffer-start insert))
		  (line-offset insert 2 0)
		  (with-output-to-mark (stream insert :full)
		    (ecase kind
		      ((:function :macro :special-form :constant)
		       (format stream "@begin[format]~%")
		       (let ((*standard-output* stream))
			 (describe (intern (string-upcase name))))
		       (format stream "~&@end[format]~2%"))
		      (:variable
		       (format stream "@begin[format]~%")
		       (let ((*standard-output* stream))
			 (describe (intern (concatenate 'string "*"
							(string-upcase name)
							"*"))))
		       (format stream "~&@end[format]~2%"))
		      (:command
		       (let ((command (getstring name *command-names*)))
			 (when command
			   (format stream "@begin[verse]~%Command @hid[~A]:  ("
				   (command-name command))
			   (print-command-bindings (command-bindings command)
						   stream)
			   (format stream ")~%@end[verse]~%~A~2&"
				   (command-documentation command)))))
		      (:hemlock-variable
		       (let ((var (getstring name *global-variable-names*)))
			 (when var
			   (format stream "@begin[verse]~%Variable @hid[~A]: ~
			   (~(~S~))~%@end[verse]~%~A~2&"
				   (variable-name var :global)
				   (variable-value var :global)
				   (variable-documentation var :global))))))
		    )))))
	  (write-file (buffer-region temp-buffer) output-file))
      (delete-buffer temp-buffer))))
