;;; -*- Mode: Lisp; Package: Extensions; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/commandline.lisp,v 1.15 2004/08/17 20:24:37 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to eat the command line passed to us from the shell.
;;; Written by Bill Chiles.
;;;

(in-package "EXTENSIONS")
(export '(*command-line-words* *command-line-switches*
	  *command-switch-demons* *command-line-utility-name*
	  *command-line-strings* *batch-mode*
	  cmd-switch-string command-line-switch-p
	  cmd-switch-name cmd-switch-value cmd-switch-words command-line-switch
	  defswitch cmd-switch-arg get-command-line-switch))

(defvar *command-line-switches* ()
  "A list of cmd-switch's representing the arguments used to invoke
  this process.")

(defvar *command-line-utility-name* ""
  "The string name that was used to invoke this process.")

(defvar *command-line-words* ()
  "A list of words between the utility name and the first switch.")

(defvar *command-line-strings* ()
  "A list of strings obtained from the command line that invoked this process.")

(defvar *command-switch-demons* ()
  "An Alist of (\"argument-name\" . demon-function)")

(defvar *batch-mode* nil
  "When True runs lisp with its input coming from standard-input.
   If an error is detected returns error code 1, otherwise 0.")

(defstruct (command-line-switch (:conc-name cmd-switch-)
				(:constructor make-cmd-switch
					      (name value words))
				(:print-function print-command-line-switch))
  name         		;the name of the switch
  value                 ;the value of that switch
  words                 ;random words dangling between switches assigned to the
                        ;preceeding switch
  )

(defun print-command-line-switch (object stream n)
  (declare (ignore n))
  (write-string "#<Command Line Switch " stream)
  (prin1 (cmd-switch-name object) stream)
  (let ((value (cmd-switch-value object))
	(words (cmd-switch-words object)))
    (when (or value words) (write-string " -- " stream)
      (when value (prin1 value stream))
      (when words (prin1 words stream))))
  (write-string ">" stream))



;;;; Processing the command strings.

(defun process-command-strings ()
  (setq *command-line-words* nil)
  (setq *command-line-switches* nil)
  (let ((cmd-strings lisp::lisp-command-line-list)
	str)
    (declare (special lisp::lisp-command-line-list))
    ;; Set some initial variables.
    ;; 
    (setf *command-line-strings* (copy-list lisp::lisp-command-line-list))
    (setf *command-line-utility-name* (pop cmd-strings))
    (setq str (pop cmd-strings))
    ;; Set initial command line words.
    ;; 
    (loop
      (unless str (return nil))
      (unless (zerop (length (the simple-string str)))
	(when (char= (schar str 0) #\-) 
	  (setq *command-line-words* (reverse *command-line-words*))
	  (return nil))
	(push str *command-line-words*))
      (setq str (pop cmd-strings)))
    ;; Set command line switches.
    ;; 
    (loop
      (unless str
	(return (setf *command-line-switches*
		      (nreverse *command-line-switches*))))
      (let* ((position (position #\= (the simple-string str) :test #'char=))
	     (switch (subseq (the simple-string str) 1 position))
	     (value (if position
			(subseq (the simple-string str) (1+ position)
				(length (the simple-string str))))))
	(setq str (pop cmd-strings))
	;; Set this switch's words until the next switch.
	;; 
	(let (word-list)
	  (loop
	    (unless str
	      (push (make-cmd-switch switch value (nreverse word-list))
		    *command-line-switches*)
	      (return nil))
	    (unless (zerop (length (the simple-string str)))
	      (when (char= #\- (schar str 0))
		(push (make-cmd-switch switch value (nreverse word-list))
		      *command-line-switches*)
		(return nil))
	      (push str word-list))
	    (setq str (pop cmd-strings))))))))

(defun get-command-line-switch (sname)
  "Accepts the name of a switch as a string and returns the value of the
   switch.  If no value was specified, then any following words are returned.
   If there are no following words, then t is returned.  If the switch was not
   specified, then nil is returned."
  (let* ((name (if (char= (schar sname 0) #\-) (subseq sname 1) sname))
	 (switch (find name *command-line-switches*
		       :test #'string-equal
		       :key #'cmd-switch-name)))
    (when switch
      (or (cmd-switch-value switch)
	  (cmd-switch-words switch)
	  T))))



;;;; Defining Switches and invoking demons.

(defvar *complain-about-illegal-switches* t
  "When set, invoking switch demons complains about illegal switches that have
   not been defined with DEFSWITCH.")

;;; This is a list of legal switch names.  DEFSWITCH sets this, and
;;; INVOKE-SWITCH-DEMONS makes sure all the switches it sees are on this
;;; list.
;;;
(defvar *legal-cmd-line-switches* nil)

;;; INVOKE-SWITCH-DEMONS cdrs down the list of *command-line-switches*.  For
;;; each switch, it checks to see if there is a switch demon with the same
;;; name.  If there is, then that demon is called as a function on the switch.
;;;
(defun invoke-switch-demons (&optional (switches *command-line-switches*)
					 (demons *command-switch-demons*))
  (dolist (switch switches t)
    (let* ((name (cmd-switch-name switch))
	   (demon (cdr (assoc name demons :test #'string-equal))))
      (cond (demon (funcall demon switch))
	    ((or (member name *legal-cmd-line-switches* :test #'string-equal)
		 (not *complain-about-illegal-switches*)))
	    (t (warn "~S is an illegal switch" switch)))
      (lisp::finish-standard-output-streams))))

(defmacro defswitch (name &optional function)
  "Associates function with the switch name in *command-switch-demons*.  Name
   is a simple-string that does not begin with a hyphen, unless the switch name
   really does begin with one.  Function is optional, but defining the switch
   is necessary to keep invoking switch demons from complaining about illegal
   switches.  This can be inhibited with *complain-about-illegal-switches*."
  (let ((gname (gensym))
	(gfunction (gensym)))
    `(let ((,gname ,name)
	   (,gfunction ,function))
       (check-type ,gname simple-string)
       (check-type ,gfunction (or symbol function) "a symbol or function")
       (push ,gname *legal-cmd-line-switches*)
       (when ,gfunction
	 (push (cons ,gname ,gfunction) *command-switch-demons*)))))


(defun eval-switch-demon (switch)
  (let ((cmds (cmd-switch-arg switch)))
    (do ((length (length cmds))
	 (start 0))
	((>= start length))
      (multiple-value-bind (form next)
	  (read-from-string cmds nil nil :start start)
	(eval form)
	(lisp::finish-standard-output-streams)
	(setf start next)))))
(defswitch "eval" #'eval-switch-demon)

(defun load-switch-demon (switch)
  (load (cmd-switch-arg switch)))
(defswitch "load" #'load-switch-demon)

(defun cmd-switch-arg (switch)
  (or (cmd-switch-value switch)
      (car (cmd-switch-words switch))
      (car *command-line-words*)))

(defswitch "core")
(defswitch "init")
(defswitch "noinit")
(defswitch "nositeinit")
(defswitch "hinit")
(defswitch "batch")
(defswitch "dynamic-space-size")
(defswitch "lib")
(defswitch "quiet")
(defswitch "debug-lisp-search")
