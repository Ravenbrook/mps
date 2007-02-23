;;;; -*- Mode: Lisp ; Package: Interface -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/interface/interface.lisp,v 1.13 2003/06/18 11:26:06 emarsden Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; This provides utilities used for building Lisp interface components
;;; using the Motif toolkit.  Specifically, it is meant to be used by the
;;; inspector and the debugger.
;;;

(in-package "INTERFACE")



;;;; Globally defined variables

(defparameter entry-font-name "-adobe-helvetica-medium-r-normal--*-120-75-*")
(defparameter header-font-name "-adobe-helvetica-bold-r-normal--*-120-75-*")
(defparameter italic-font-name "-adobe-helvetica-medium-o-normal--*-120-75-*")


(defvar *header-font*)
(defvar *italic-font*)
(defvar *entry-font*)
(defvar *all-fonts*)

(defvar *system-motif-server* nil)

(defvar *lisp-interface-shell* nil)

(defvar *lisp-interface-connection* nil)

(defvar *lisp-interface-panes* nil)

(defvar *lisp-interface-menus* nil)

(defvar *busy-cursor*)


;;; *INTERFACE-MODE* may be one of:
;;;       :normal  -- normal interaction mode
;;;       :edit    -- targetting object to edit
;;;       :copy    -- targetting object to copy
;;;       :paste   -- targetting object to copy into
;;;
(defvar *interface-mode* :normal)

;;; This is where an object is stored when copied, and where paste looks to
;;; find the value it needs to insert.
;;;
(defvar *copy-object* nil)

(defvar *interface-style* :graphics
  "This specifies the default interface mode for the debugger and inspector.
   The allowable values are :GRAPHICS and :TTY.")



;;;; Functions for dealing with interface widgets

(defun create-interface-shell ()
  (if *lisp-interface-shell*
      (values *lisp-interface-shell* *lisp-interface-connection*)
      (let ((con (xt::open-motif-connection
		  *default-server-host* *default-display*
		  "lisp" "Lisp"
		  (and *system-motif-server*
		       (ext:process-pid *system-motif-server*)))))
	(with-motif-connection (con)
	  (setf (xti:motif-connection-close-hook *motif-connection*)
		#'close-connection-hook)
	  (setf *header-font*
		(build-simple-font-list "HeaderFont" header-font-name))
	  (setf *italic-font*
		(build-simple-font-list "ItalicFont" italic-font-name))
	  (setf *entry-font*
		(build-simple-font-list "EntryFont" entry-font-name))
	  (setf *all-fonts*
		(build-font-list `(("EntryFont" ,entry-font-name)
				   ("HeaderFont" ,header-font-name)
 				   ("ItalicFont" ,italic-font-name))))

	  (let ((shell (create-application-shell
			:default-font-list *entry-font*)))
	    (setf *lisp-interface-panes* (make-hash-table))
	    (setf *lisp-interface-menus* (make-hash-table :test #'equal))
	    (setf *lisp-interface-connection* con)
	    (setf *lisp-interface-shell* shell)
	    (setf *busy-cursor* (xt:create-font-cursor 150))
	    (values shell con))))))

(declaim (inline popup-interface-pane))
(defun popup-interface-pane (pane)
  (declare (type widget pane))
  (popup pane :grab-none))

(defun create-interface-pane-shell (title tag)
  (declare (simple-string title))
  (let* ((shell *lisp-interface-shell*)
	 (existing (gethash tag *lisp-interface-panes*))
	 (pane (or existing
		   (create-popup-shell "interfacePaneShell"
				       :top-level-shell shell
				       :default-font-list *entry-font*
				       :keyboard-focus-policy :pointer
				       :title title
				       :icon-name title))))
    (setf (gethash tag *lisp-interface-panes*) pane)
    (values pane (not existing))))

(defun find-interface-pane (tag)
  (if *lisp-interface-panes*
      (gethash tag *lisp-interface-panes*)))

(defun destroy-interface-pane (tag)
  (let ((pane (and *lisp-interface-panes*
		   (gethash tag *lisp-interface-panes*))))
    (when pane
      (destroy-widget pane)
      (remhash tag *lisp-interface-panes*))))



;;;; Functions for dealing with menus

;; `(("New" ,#'new-thing-callback this that)
;;   ("Load" ,#'load-thing-callback loadee))

(defun create-interface-menu (menu-bar name menu-spec)
  (declare (simple-string name))
  (let* ((pulldown (create-pulldown-menu menu-bar "pulldown"))
	 (cascade (create-cascade-button menu-bar "cascade"
					 :sub-menu-id pulldown
					 :label-string name))
	 (widgets))

    (dolist (entry menu-spec)
      (if (and entry (listp entry))
	  (let ((widget (create-push-button pulldown "menuEntry"
						   :label-string (car entry))))
	    (when (cdr entry)
	      (apply #'add-callback widget :activate-callback (cdr entry)))
	    (push widget widgets))
	  (let ((widget (create-separator pulldown "menuSeparator")))
	    (push widget widgets))))
    (apply #'manage-children widgets)
    cascade))

(defun create-cached-menu (menu-bar name &optional menu-spec)
  (if menu-spec
      (setf (gethash name *lisp-interface-menus*) menu-spec)
      (setf menu-spec (gethash name *lisp-interface-menus*)))
  (create-interface-menu menu-bar name menu-spec))

(defun create-highlight-button (parent name label)
  (create-push-button parent name
			     :label-string label
			     :highlight-on-enter t
			     :shadow-thickness 0))



;;;; Functions for making/changing value boxes

(defparameter *string-cutoff* 60)

(defun trim-string (string)
  (declare (simple-string string))
  (if (> (length string) *string-cutoff*)
      (let ((new (make-string (+ 4 *string-cutoff*))))
        (replace new string :end2 *string-cutoff*)
        (replace new " ..." :start1 *string-cutoff*))
      string))

(defun print-for-widget-display (string arg)
  (let ((*print-pretty* nil)
	(*print-length* 20)
	(*print-level* 2))
    (trim-string (format nil string arg))))

(defun create-value-box (parent name value
			 &key callback client-data (activep t))
  (let* ((rc (create-row-column parent "valueBox"
				:margin-height 0
				:margin-width 0
				:orientation :horizontal))
	 (label (create-label rc "valueLabel"
				     :font-list *header-font*
				     :label-string name))
	 (button (if activep
		     (create-highlight-button rc "valueObject"
					      (print-for-widget-display
					       "~S" value))
		     (create-label rc "valueObject"
					  :font-list *italic-font*
					  :label-string
					  (format nil "~A" value)))))
    (manage-children label button)
    (when (and callback activep)
      (add-callback button :activate-callback
		    callback (or client-data value)))
    rc))

(defmacro with-widget-children ((this-child widget) &rest clauses)
  `(let ((children (xti:widget-children ,widget)))
     (dolist (,this-child children)
       ,(cons 'case
	      (cons `(xti:widget-type ,this-child) clauses)))))

(defun set-value-box (vbox name value &key callback client-data)
  (with-widget-children (child vbox)
    (:push-button
     (set-values child :label-string
		 (print-for-widget-display "~S" value))
     (remove-all-callbacks child :activate-callback)
     (when callback
       (add-callback child :activate-callback callback (or client-data value))))
    (:label
     (set-values child :label-string name))))



;;;; Misc. stuff

(defun nuke-interface ()
  (with-motif-connection (*lisp-interface-connection*)
    (quit-application)))

(defun interface-error (text &optional (pane *lisp-interface-shell*))
  (declare (simple-string text))
  (multiple-value-bind
      (dialog shell)
      (create-error-dialog pane "lispInterfaceError" :message-string text)
    (set-values shell :title "Error")
    (manage-child dialog)))

(defmacro with-busy-cursor ((pane) &body forms)
  `(let ((window (widget-window ,pane)))
     (unwind-protect
	 (progn
	   (with-clx-requests (setf (xlib:window-cursor window) *busy-cursor*))
	   ,@forms)
       (setf (xlib:window-cursor window) :none))))

(defmacro grab-output-as-string (&body forms)
  `(let* ((stream (make-string-output-stream))
	  (*standard-output* stream)
	  (value (progn ,@forms)))
     (close stream)
     (values (get-output-stream-string stream) value)))

(defun ask-for-confirmation (pane message callback)
  (declare (simple-string message))
  (multiple-value-bind
      (dialog shell)
      (create-question-dialog pane "lispConfirmation" :message-string message)
    (set-values shell :title "Are You Sure?")
    (add-callback dialog :ok-callback callback)
    (manage-child dialog)))

(defun use-graphics-interface (&optional (kind *interface-style*))
  (cond
   ((not (assoc :display ext:*environment-list*)) nil)
   ((member kind '(:window :windows :graphics :graphical :x)) t)
   ((member kind '(:command-line :tty)) nil)
   (t
    (let ((*interface-style* :tty))
      (error "Interface specification must be one of :window, :windows, ~%~
	      :graphics, :graphical, :x, :command-line, or :tty -- ~%~
	      not ~S." kind)))))

(defun close-connection-hook (connection)
  (declare (ignore connection))
  (setf *lisp-interface-panes* nil)
  (setf *lisp-interface-menus* nil)
  (setf *lisp-interface-connection* nil)
  (setf *lisp-interface-shell* nil))

(defun system-server-status-hook (process)
  (unless (ext:process-alive-p process)
    (setf *system-motif-server* nil)
    (warn "Motif server died.~@
	   Status = ~S, exit code = ~D."
	  (ext:process-status process)
	  (ext:process-exit-code process))))

(defvar *server-startup-timeout* 30)

(defun verify-system-server-exists ()
  (when (and (not xt:*default-server-host*)
	     (not (and *system-motif-server*
		       (ext:process-alive-p *system-motif-server*))))
    (let ((process (ext:run-program
		    (merge-pathnames *clm-binary-name*
				     *clm-binary-directory*)
		    '("-nofork" "-local")
		    :output *error-output*
		    :error :output
		    :wait nil
		    :status-hook #'system-server-status-hook)))
      (unless (and process (ext:process-alive-p process))
	(xti:toolkit-error "Could not start Motif server process.~@
			    Status = ~S, exit code = ~D."
			   (ext:process-status process)
			   (ext:process-exit-code process)))
      ;;
      ;; Wait until the server has started up
      (let ((sock-name (format nil "/tmp/.motif_socket-p~D"
			       (ext:process-pid process)))
	    (end-time (+ (get-internal-real-time)
			 (* internal-time-units-per-second
			    *server-startup-timeout*))))
	(loop
	  (when (probe-file sock-name)
	    (return))
	  (system:serve-event 1)
	  (when (> (get-internal-real-time) end-time)
	    (xti:toolkit-error
	     "Timed out waiting for Motif server to start up.")))

	(setf *system-motif-server* process)))))



;;;; Handling of the Inspector items in the Control Panel

(defconstant *history-size* 25)

(defvar *inspector-history*)
(defvar *current-inspector-objects* nil)

(defstruct (inspector-history
	    (:print-function print-inspector-history)
	    (:conc-name history-)
	    (:constructor make-inspector-history (widget)))
  (record (make-array *history-size*) :type simple-vector)
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (widget nil :type (or nil widget)))

(defun print-inspector-history (hist stream d)
  (declare (ignore hist d))
  (write-string "#<Inspector History>" stream))

(defun inspector-add-history-item (object)
  (let* ((h *inspector-history*)
	 (tail (history-tail h))
	 (head (history-head h))
	 (widget (history-widget h)))
    ;;
    ;; Only add new items to the history if they're not there already
    (unless (position object (history-record h))
      (setf (svref (history-record h) tail) object)
      (setf tail (mod (1+ tail) *history-size*))
      (setf (history-tail h) tail)
      ;;
      ;; Add new item at the top of the history list
      (list-add-item widget (print-for-widget-display "~S" object) 1)
      (when (= tail head)
	(setf (history-head h) (mod (1+ head) *history-size*))
	;;
	;; Nuke old item at bottom of history
	(list-delete-pos widget 0)))))

(defun eval-and-inspect-callback (widget call-data pane)
  (declare (ignore call-data))
  (let ((input (car (get-values widget :value))))
    (handler-case
	(let ((object (eval (read-from-string input))))
	  (with-busy-cursor (pane)
	    (text-set-string widget "")
	    (display-inspector-pane object)
	    (push object *current-inspector-objects*)
	    (inspector-add-history-item object)))
      (error (e)
        (interface-error (format nil "~a" e) (xti:widget-user-data widget))))))

(defun inspector-history-callback (widget call-data pane)
  (let* ((pos (list-callback-item-position call-data))
	 (object (svref (history-record *inspector-history*)
			(mod (- (history-tail *inspector-history*) pos)
			     *history-size*))))
    (with-busy-cursor (pane)
      (update-display widget)
      (display-inspector-pane object)
      (push object *current-inspector-objects*)
      (list-deselect-pos widget pos))))



;;;; The CLOS generic functions for the inspector

(defgeneric inspector-pane-title (object)
  (:documentation
   "Returns a string which is meant to be the title of the inspection pane
    displaying the given object."))

(defgeneric display-inspector-pane (object)
  (:documentation
   "Creates a window pane which displays relevant information about the
    given object."))



;;;; Build the Lisp Control Panel

(defconstant *control-cookie* (cons :lisp :control))

(defvar *file-selection-hook* #'load)

(defvar *file-list* nil)

(defun close-all-callback (widget call-data)
  (declare (ignore widget call-data))
  (dolist (object *current-inspector-objects*)
    (destroy-interface-pane object))
  (setf *current-inspector-objects* nil))

(defun file-selection-callback (widget call-data)
  (declare (ignore widget))
  (let ((string (compound-string-get-ltor
		 (file-selection-callback-value call-data) "")))
    (with-callback-deferred-actions
      (funcall *file-selection-hook* string))))

(defun add-file-callback (widget call-data fsel files)
  (declare (ignore widget call-data))
  (setf *file-selection-hook*
	#'(lambda (fname)
	    (let* ((base-name (pathname-name fname))
		   (path (directory-namestring fname))
		   (name (format nil "~a~a" path base-name))
		   (xs (compound-string-create
		       (format nil "~a  ~a" base-name path) "")))
	      (list-add-item files xs 1)
	      (push (cons name xs) *file-list*))))
  (manage-child fsel))


(defun remove-files-callback (widget call-data files)
  (declare (ignore widget call-data))
  (let ((selections (list-get-selected-pos files))
	(items))
    (dolist (pos selections)
      (let ((item (elt *file-list* (1- pos))))
	(list-delete-item files (cdr item))
	(push item items)))

    (dolist (item items)
      (setf *file-list* (delete item *file-list*)))))

(defun load-files-callback (widget call-data files)
  (declare (ignore widget call-data))
  (let ((selections (list-get-selected-pos files)))
    (with-callback-deferred-actions
      (dolist (pos selections)
	(load (car (elt *file-list* (1- pos))))))))

(defun compile-files-callback (widget call-data files)
  (declare (ignore widget call-data))
  (let ((selections (list-get-selected-pos files)))
    (with-callback-deferred-actions
      (with-compilation-unit ()
	(dolist (pos selections)
	  (compile-file (car (elt *file-list* (1- pos)))))))))

(defun apropos-callback (widget call-data pane)
  (declare (ignore call-data))
  (with-busy-cursor (pane)
    (let* ((input (car (get-values widget :value)))
	   (results (apropos-list input)))
      (text-set-string widget "")
      (multiple-value-bind (form shell)
			   (create-form-dialog pane "aproposDialog")
	(let* ((done (create-push-button form "aproposDone"
						:left-attachment :attach-form
						:right-attachment :attach-form
						:label-string "Done"))
	       (list (create-scrolled-list form "aproposList"
					   :visible-item-count 10
					   :left-attachment :attach-form
					   :right-attachment :attach-form
					   :bottom-attachment :attach-form
					   :top-attachment :attach-widget
					   :top-widget done)))
	  (set-values shell :title "Apropos Results"
	                    :keyboard-focus-policy :pointer)
	  (dolist (sym results)
	    (list-add-item list (symbol-name sym) 0))
	  (add-callback done :activate-callback #'destroy-callback shell)
	  (add-callback list :browse-selection-callback
			#'(lambda (w c) (declare (ignore w))
			    (with-busy-cursor (shell)
			      (let ((pos (list-callback-item-position c)))
				(inspect (elt results (1- pos)))))))
	  (manage-child done)
	  (manage-child list)
	  (manage-child form))))))

(defun about-callback (widget call-data pane)
  (declare (ignore widget call-data))
  (multiple-value-bind
      (msg shell)
      (create-message-dialog pane "aboutDialog"
			     :dialog-title "About Lisp"
			     :message-string (grab-output-as-string
					    (print-herald)))
    (declare (ignore shell))
    (let ((help (message-box-get-child msg :dialog-help-button))
	  (cancel (message-box-get-child msg :dialog-cancel-button)))
      (destroy-widget help)
      (destroy-widget cancel)
      (manage-child msg))))

(defun set-compile-policy-callback (widget call-data speed space safety
					   debug cspeed brevity)
  (declare (ignore widget call-data))
  (flet ((get-policy-value (widget)
	   (coerce (/ (car (get-values widget :value)) 10) 'single-float)))
    (let ((speed-val (get-policy-value speed))
	  (space-val (get-policy-value space))
	  (safety-val (get-policy-value safety))
	  (debug-val (get-policy-value debug))
	  (cspd-val (get-policy-value cspeed))
	  (brev-val (get-policy-value brevity)))

      (proclaim (list 'optimize
		      (list 'speed speed-val)
		      (list 'space space-val)
		      (list 'safety safety-val)
		      (list 'debug debug-val)
		      (list 'compilation-speed cspd-val)
		      (list 'ext:inhibit-warnings brev-val))))))

(defun compile-policy-callback (widget call-data pane)
  (declare (ignore widget call-data))
  (multiple-value-bind (form shell)
		       (create-form-dialog pane "compilationDialog")
    (flet ((create-policy (parent title value)
	     (create-scale parent "policy"
			   :decimal-points 1
			   :orientation :horizontal
			   :maximum 30
			   :show-value t
			   :title-string title
			   :value (truncate (* 10 value)))))
      (let* ((cookie c::*default-cookie*)
	     (rc (create-row-column form "policies"
				    :packing :pack-column
				    :num-columns 2))
	     (speed (create-policy rc "Speed" (c::cookie-speed cookie)))
	     (space (create-policy rc "Space" (c::cookie-space cookie)))
	     (safety (create-policy rc "Safety" (c::cookie-safety cookie)))
	     (debug (create-policy rc "Debug" (c::cookie-debug cookie)))
	     (cspeed (create-policy rc "Compilation Speed"
				    (c::cookie-cspeed cookie)))
	     (brevity (create-policy rc "Inhibit Warnings"
				     (c::cookie-brevity cookie)))
	     (sep (create-separator form "separator"
					   :top-attachment :attach-widget
					   :top-widget rc
					   :left-attachment :attach-form
					   :right-attachment :attach-form))
	     (done (create-push-button form "done"
					    :label-string "Done"
					    :top-attachment :attach-widget
					    :top-widget sep
					    :left-attachment :attach-position
					    :right-attachment :attach-position
					    :left-position 35
					    :right-position 65)))
	(set-values shell :title "Compilation Options"
		    :keyboard-focus-policy :pointer)
	(add-callback done :activate-callback
		      'set-compile-policy-callback
		      speed space safety debug cspeed brevity)
	(manage-children speed space safety debug cspeed brevity)
	(manage-children sep done rc)
	(manage-child form)))))

(defun display-control-pane ()
  (let* ((pane (create-interface-pane-shell (lisp-implementation-type)
					    *control-cookie*))
	 (form (create-form pane "form"))
	 (fsel (create-file-selection-dialog pane "lispFileSelector"
					     :auto-unmanage t))
	 (menu-bar (create-menu-bar form "lispMenu"
				    :left-attachment :attach-form
				    :right-attachment :attach-form))
	 (lmenu (create-interface-menu
		 menu-bar "Lisp"
		 `(("About ..." about-callback ,pane)
		   "-----"
		   ("Load File" ,#'(lambda (w c)
				     (declare (ignore w c))
				     (setf *file-selection-hook* #'load)
				     (manage-child fsel)))
		   ("Compile File" ,#'(lambda (w c) (declare (ignore w c))
					(setf *file-selection-hook*
					      #'compile-file)
					(manage-child fsel)))
		   "-----"
		   ("Close Inspection Panes" ,#'close-all-callback)
		   ("Close Control Panel" ,#'popdown-callback ,pane)
		   ("Quit Lisp" ,#'(lambda (w c pane)
				     (declare (ignore w c))
				     (ask-for-confirmation
				      pane "Do you really want to quit?"
				      #'(lambda (w c) (declare (ignore w c))
					  (xt::quit-server)
					  (quit))))
		    ,pane))))
	 (fmenu (create-interface-menu
		 menu-bar "Files"
		 `(("Load File Group")
		   ("Save File Group"))))
	 (omenu (create-interface-menu
		 menu-bar "Options"
		 `(("Compilation policy ..." compile-policy-callback ,pane))))
	 (vsep (create-separator form "separator"
				 :orientation :vertical
				 :top-attachment :attach-widget
				 :top-widget menu-bar
				 :bottom-attachment :attach-form
				 :right-attachment :attach-position
				 :right-position 50))
	 (prompt (create-label form "inspectPrompt"
				      :top-attachment :attach-widget
				      :top-widget menu-bar
				      :font-list *header-font*
				      :label-string "Inspect new object:"))
	 (entry (create-text form "inspectEval"
			     :top-attachment :attach-widget
			     :top-widget prompt
			     :left-offset 4
			     :right-offset 4
			     :left-attachment :attach-form
			     :right-attachment :attach-widget
			     :right-widget vsep))
	 (hlabel (create-label form "inspectHistoryLabel"
				      :top-attachment :attach-widget
				      :top-widget entry
				      :font-list *header-font*
				      :label-string "Inspector History:"))
	 (hview (create-scrolled-list form "inspectHistory"
				      :visible-item-count 5
				      :left-offset 4
				      :right-offset 4
				      :bottom-offset 4
				      :top-attachment :attach-widget
				      :top-widget hlabel
				      :left-attachment :attach-form
				      :right-attachment :attach-widget
				      :right-widget vsep
				      :bottom-attachment :attach-form))
	 (flabel (create-label form "filesLabel"
				      :left-attachment :attach-widget
				      :left-widget vsep
				      :top-attachment :attach-widget
				      :top-widget menu-bar
				      :label-string "Files:"
				      :font-list *header-font*))
	 (frc (create-row-column form "filesButtons" 
				 :packing :pack-column
				 :num-columns 2
				 :left-attachment :attach-widget
				 :left-widget vsep
				 :top-attachment :attach-widget
				 :top-widget flabel
				 :right-attachment :attach-form
				 :right-offset 4))
	 (add (create-push-button frc "fileAdd"
					 :label-string "Add File"))
	 (remove (create-push-button frc "fileRemove"
					    :label-string "Remove Files"))
	 (load (create-push-button frc "fileLoad"
					  :label-string "Load Files"))
	 (compile (create-push-button frc "fileCompile"
					     :label-string "Compile Files"))
	 (apropos (create-text form "apropos"
			       :left-attachment :attach-widget
			       :left-widget vsep
			       :right-attachment :attach-form
			       :bottom-attachment :attach-form
			       :left-offset 4
			       :right-offset 4
			       :bottom-offset 4))
	 (alabel (create-label form "aproposLabel"
				      :label-string "Apropos:"
				      :font-list *header-font*
				      :left-attachment :attach-widget
				      :left-widget vsep
				      :bottom-attachment :attach-widget
				      :bottom-widget apropos))
	 (hsep (create-separator  form "separator"
					:left-attachment :attach-widget
					:left-widget vsep
					:right-attachment :attach-form
					:bottom-attachment :attach-widget
					:bottom-widget alabel))
	 (files (create-scrolled-list form "files"
				      :visible-item-count 5
				      :selection-policy :multiple-select
				      :top-attachment :attach-widget
				      :top-widget frc
				      :left-attachment :attach-widget
				      :left-widget vsep
				      :left-offset 4
				      :right-attachment :attach-form
				      :right-offset 4
				      :bottom-attachment :attach-widget
				      :bottom-widget hsep
				      :bottom-offset 4)))
				      
    (manage-child form)
    (manage-children lmenu fmenu omenu)
    (manage-child files)
    (manage-children menu-bar vsep prompt entry hlabel flabel frc
		     apropos alabel hsep)
    (manage-children add remove load compile)
    (manage-child hview)
    (set-values fmenu :sensitive nil)

    (setf *inspector-history* (make-inspector-history hview))
    (setf (xti:widget-user-data entry) pane)

    (add-callback entry :activate-callback #'eval-and-inspect-callback pane)
    (add-callback hview :browse-selection-callback
		  #'inspector-history-callback pane)
    (add-callback fsel :ok-callback 'file-selection-callback)
    (add-callback add :activate-callback 'add-file-callback fsel files)
    (add-callback remove :activate-callback 'remove-files-callback files)
    (add-callback load :activate-callback 'load-files-callback files)
    (add-callback compile :activate-callback 'compile-files-callback files)
    (add-callback apropos :activate-callback 'apropos-callback pane)
    (popup-interface-pane pane)
    pane))

(defun verify-control-pane-displayed ()
  (let ((pane (find-interface-pane *control-cookie*)))
    (unless pane
      (setf pane (display-control-pane)))
    (unless (is-managed pane)
      (popup-interface-pane pane))))

(defun lisp-control-panel ()
  (when (use-graphics-interface)
    (verify-system-server-exists)
    (multiple-value-bind (shell connection)
			 (create-interface-shell) 
      (declare (ignore shell))
      (with-motif-connection (connection)
	(verify-control-pane-displayed)))))



;;;; Fix up QUIT

(defun cleanup-motif ()
  (when (and *system-motif-server*
	     (ext:process-alive-p *system-motif-server*))
    (ext:process-kill *system-motif-server* :sigint))

  (when (and xt::*local-motif-server*
	     (ext:process-alive-p xt::*local-motif-server*))
    (ext:process-kill xt::*local-motif-server* :sigint)))

(pushnew #'cleanup-motif lisp::*cleanup-functions*)

;;(in-package "EXT")
;;
;;(defun quit (&optional recklessly-p)
;;  "Terminates the current Lisp.  Things are cleaned up unless Recklessly-P is
;;  non-Nil."
;;  (if recklessly-p
;;      (unix:unix-exit 0)
;;      (progn
;;	(interface::cleanup-motif)
;;	(throw 'cl::%end-of-the-world 0))))
