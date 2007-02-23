;;;; -*- Mode: Lisp ; Package: Debug -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/interface/debug.lisp,v 1.9 2001/12/12 20:18:25 pmai Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; This file implements the graphical interface to the debugger.
;;;

(in-package "DEBUG")
(use-package '("TOOLKIT" "INTERFACE"))

;;; We need to record three things globally:
;;;    - The structure decribing the current debugger display
;;;    - The frame displays which are currently active (ie. visible)
;;;    - The husks of old debugger displays for reuse
;;;
(defvar *current-debug-display* nil)
(defvar *debug-active-frames* nil)
(defvar *old-display-frames* nil)



;;;; Structures used by the graphical debugger

(defstruct (debug-display
	    (:conc-name dd-info-)
	    (:print-function print-debug-display)
	    (:constructor make-debug-display
			  (debug-pane errmsg restarts backtrace)))
  (debug-pane nil :type (or null widget))
  (errmsg nil :type (or null widget))
  (restarts nil :type (or null widget))
  (backtrace nil :type (or null widget))
  (level 0 :type fixnum)
  (connection nil :type (or null xt::motif-connection)))

(defun print-debug-display (info stream d)
  (declare (ignore d))
  (format stream "#<Debugger Display Info level ~d" (dd-info-level info)))



;;;; Callback functions used by debugger

(defun quit-debugger-callback (widget call-data)
  (declare (ignore widget call-data))
  (close-motif-debugger *debug-condition*)
  (throw 'lisp::top-level-catcher nil))

(defun restart-callback (widget call-data restart)
  (declare (ignore widget call-data))
  (invoke-restart-interactively restart))

(defun stack-frame-callback (widget call-data frame)
  (declare (ignore widget call-data))
  (unless (assoc frame *debug-active-frames*)
    ;; Should wrap this in a busy cursor
    (debug-display-frame frame)))

(defun close-all-callback (widget call-data)
  (declare (ignore widget call-data))
  (dolist (info *debug-active-frames*)
    (destroy-widget (cdr info)))
  (setf *debug-active-frames* nil))

;;; This is to provide a means for recording the stack backtrace.  In
;;; particular, this is important for sending bug reports.
;;;
(defun dump-backtrace-callback (widget call-data)
  (declare (ignore widget call-data))
  (let ((*current-frame* (di:top-frame)))
    (format t "~%Stack Backtrace:~%")
    (backtrace)))

(defun frame-view-callback (widget call-data thing)
  (declare (ignore widget call-data))
  ;; Should wrap this in a busy cursor
  (inspect thing))

(defun close-frame-callback (widget call-data frame)
  (declare (ignore widget call-data))
  (setf *debug-active-frames*
	(delete frame *debug-active-frames*
		:test #'(lambda (a b) (eql a (car b)))))
  (destroy-interface-pane frame))

(defun edit-source-callback (widget call-data frame)
  (declare (ignore widget call-data))
  (handler-case
      (let ((*current-frame* frame))
	(funcall (debug-command-p :edit-source)))
    (error (cond)
	   (interface-error (safe-condition-message cond)))))

(defun frame-eval-callback (widget call-data frame output)
  (declare (ignore call-data))
  (let* ((input (car (get-values widget :value)))
	 (mark (text-get-last-position output))
	 (response
	  (format nil "Eval>> ~a~%~a--------------------~%"
		  input
		  (handler-case
		      (multiple-value-bind
			  (out val)
			  (let ((*current-frame* frame))
			    (grab-output-as-string
			     (di:eval-in-frame frame (read-from-string input))))
			(format nil "~a~s~%" out val))
		    (error (cond)
			   (safe-condition-message cond)))))
	 (length (length response)))
    (declare (simple-string response))
	
    (text-set-string widget "")
    (text-insert output mark response)
    ;; This is to make sure that things stay visible
    (text-set-insertion-position output (+ length mark))))

(defun source-verbosity-callback (widget call-data frame srcview delta)
  (declare (ignore widget call-data))
  (let* ((current (car (get-values srcview :user-data)))
	 (new (+ current delta)))
    (when (minusp new)
      (setf new 0))
    (let ((source (handler-case
		     (grab-output-as-string
		      (print-code-location-source-form
		       (di:frame-code-location frame) new))
		   (di:debug-condition (cond)
		     (declare (ignore cond))
		     "Source form not available."))))
      (set-values srcview
		  :label-string source
		  :user-data new))))



;;; DEBUG-DISPLAY-FRAME-LOCALS -- Internal
;;;
;;; This sets up the display of the available local variables for the given
;;; stack frame.
;;;
(defun debug-display-frame-locals (frame debug-fun location frame-view)
  (let (widgets)
    (if (di:debug-variable-info-available debug-fun)
	(let ((any-p nil)
	      (any-valid-p nil))
	  (di:do-debug-function-variables (v debug-fun)
	    (unless any-p
	      (setf any-p t)
	      (push (create-label frame-view "localsLabel"
					 :font-list *header-font*
					 :label-string "Local variables:")
		    widgets))
	    (when (eq (di:debug-variable-validity v location) :valid)
	      (let ((value (di:debug-variable-value v frame))
		    (id (di:debug-variable-id v)))
		(setf any-valid-p t)
		(push
		 (create-value-box frame-view
				   (format nil "   ~A~:[#~D~;~*~]:"
					   (di:debug-variable-name v)
					   (zerop id) id)
				   value
				   :callback 'frame-view-callback)
		 widgets))))
	  (cond
	   ((not any-p)
	    (push
	     (create-label frame-view "noLocals"
				  :font-list *italic-font*
				  :label-string
				  "   No local variables in function.")
	     widgets))
	   ((not any-valid-p)
	    (push
	     (create-label frame-view "noValidLocals"
				  :font-list *italic-font*
				  :label-string
				  "   All variables have invalid values.")
	     widgets))))

	(push (create-label frame-view "noVariableInfo"
				   :font-list *italic-font*
				   :label-string
				   "   No variable information available.")
	      widgets))
    (apply #'manage-children widgets)))

;;; DEBUG-DISPLAY-FRAME-PROMPT -- Internal
;;;
;;; Every frame window has a Frame Eval area.  This function creates the
;;; Eval area and attaches the necessary callbacks.
;;;
(defun debug-display-frame-prompt (frame frame-view)
  (let* ((form (create-form frame-view "promptForm"))
	 (label (create-label form "framePrompt"
				     :label-string "Frame Eval:"
				     :font-list *header-font*))
	 (entry (create-text form "frameEval"
			     :top-attachment :attach-widget
			     :top-widget label
			     :left-attachment :attach-form
			     :right-attachment :attach-form))
	 (output (create-text form "frameOutput"
			      :edit-mode :multi-line-edit
			      :editable nil
			      :rows 8
			      :columns 40
			      :top-attachment :attach-widget
			      :top-widget entry
			      :bottom-attachment :attach-form
			      :left-attachment :attach-form
			      :right-attachment :attach-form)))

    (manage-child form)
    (manage-children label entry output)
    (add-callback entry :activate-callback 'frame-eval-callback
		  frame output)))

;;; DEBUG-DISPLAY-FRAME -- Internal
;;;
;;; Function to generate the graphical display for the given frame.  Each
;;; frame window is composed of the following parts:
;;;    - The function called
;;;    - The source form
;;;    - Local variables
;;;    - Frame Eval window
;;;
(defun debug-display-frame (frame)
  (let* ((debug-fun (di:frame-debug-function frame))
	 (location (di:frame-code-location frame))
	 (name (di:debug-function-name debug-fun))
	 (title (format nil "Stack Frame: ~A" name))
	 (frame-shell (create-interface-pane-shell title frame))
	 (frame-view (create-row-column frame-shell "debugFrameView"))
	 (menu-bar (create-menu-bar frame-view "frameMenu"))
	 (fcall (create-label frame-view "frameCall"
				     :label-string
				     (format nil "Frame Call: ~a"
					     (grab-output-as-string
					      (print-frame-call frame)))))
	 (fbox (create-value-box frame-view "Function:"
				 name
				 :callback 'frame-view-callback
				 :client-data
				 (di:debug-function-function debug-fun)))
	 (slabel (create-label frame-view "sourceLabel"
				      :font-list *header-font*
				      :label-string "Source form:"))
	 (swindow (create-scrolled-window frame-view "frameSourceWindow"
					  :scrolling-policy :automatic
					  :scroll-bar-placement :bottom-right))

	 (source (handler-case
		     (grab-output-as-string
		      (print-code-location-source-form location 0))
		   (di:debug-condition (cond)
		     (declare (ignore cond))
		     "Source form not available.")))
	 (srcview (create-label swindow "sourceForm"
				:alignment :alignment-beginning
				:user-data 0
				:label-string source))
	 (cascade1
	  (create-interface-menu menu-bar "Frame"
	   `(("Edit Source" edit-source-callback ,frame)
	     ("Expand Source Form" source-verbosity-callback ,frame ,srcview 1)
	     ("Shrink Source Form" source-verbosity-callback ,frame ,srcview -1)
	     ("Close Frame" close-frame-callback ,frame))))
	 (cascade2 (create-cached-menu menu-bar "Debug")))

    (manage-child frame-view)
    (manage-children menu-bar fcall fbox slabel swindow)
    (manage-child srcview)
    (manage-children cascade1 cascade2)

    (debug-display-frame-locals frame debug-fun location frame-view)
    (debug-display-frame-prompt frame frame-view)

    (popup-interface-pane frame-shell)
    (push (cons frame frame-shell) *debug-active-frames*)))



;;;; Functions to display the debugger control panel

;;; DEBUG-DISPLAY-ERROR -- Internal
;;;
;;; Fills in the given widget with the error message for the given
;;; condition.
;;;
(defun debug-display-error (errmsg condition)
  (set-values errmsg :label-string (safe-condition-message condition)))

;;; DEBUG-DISPLAY-RESTARTS -- Internal
;;;
;;; Fills in a RowColumn box with buttons corresponding to the currently
;;; active restarts.
;;;
(defun debug-display-restarts (restart-view)
  (let ((widgets (reverse (xti:widget-children restart-view)))
	(used-ones))

    (dolist (r *debug-restarts*)
      (let* ((label (format nil "~A" r))
	     (button (if widgets
			 (let ((w (pop widgets)))
			   (set-values w :label-string label)
			   (remove-all-callbacks w :activate-callback)
			   w)
			 (create-highlight-button restart-view
						  "restartButton"
						  label))))
	
	(add-callback button :activate-callback 'restart-callback r)
	(push button used-ones)))
    (apply #'manage-children used-ones)
    (when widgets
      (apply #'unmanage-children widgets))))

;;; DEBUG-DISPLAY-STACK -- Internal
;;;
;;; Fills in a RowColumn box with buttons corresponding to the stack frames
;;; found on the stack.
;;;
(defun debug-display-stack (backtrace)
  (let ((widgets (reverse (xti:widget-children backtrace)))
	(used-ones)
	(frames))

    (do ((frame *current-frame* (di:frame-down frame)))
	((null frame))
      (push frame frames))
    (setf frames (nreverse frames))

    (dolist (frame frames)
      (let* ((label (grab-output-as-string
		     (print-frame-call frame)))
	     (button (if widgets
			 (let ((w (pop widgets)))
			   (set-values w :label-string label)
			   (remove-all-callbacks w :activate-callback)
			   w)
			 (create-highlight-button
			  backtrace "stackFrame" label))))
	(add-callback button :activate-callback 'stack-frame-callback frame)
	(push button used-ones)))
    (apply #'manage-children used-ones)
    (when widgets
      (apply #'unmanage-children widgets))))

;;; REALLY-CREATE-DEBUGGER -- Internal
;;;
;;; This creates all the widgets used by the main debugger window.  It
;;; calls various sub-functions such as DEBUG-DISPLAY-STACK to fill in the
;;; various display sections.  It should only be called if there are no old
;;; debugger panes available for recycling.
;;;
(defun really-create-debugger (condition)
  (let* ((debug-pane (create-interface-pane-shell "Debugger" condition))
	 (frame (create-frame debug-pane "debugFrame"))
	 (form (create-form frame "debugForm"))
	 (menu-bar (create-menu-bar form "debugMenu"
				    :left-attachment :attach-form
				    :right-attachment :attach-form))
	 (cascade (create-cached-menu
		   menu-bar "Debug"
		   '(("Close All Frames" close-all-callback)
		     ("Dump Backtrace" dump-backtrace-callback)
		     ("Quit Debugger" quit-debugger-callback))))
 	 (errlabel (create-label form "errorLabel"
					:top-attachment :attach-widget
					:top-widget menu-bar
					:left-attachment :attach-form
					:font-list *header-font*
					:label-string "Error Message:"))
	 (errmsg (create-label form "errorMessage"
				      :top-attachment :attach-widget
				      :top-widget errlabel
				      :left-attachment :attach-form
				      :right-attachment :attach-form))
	 (rlabel (create-label form "restartLabel"
				      :top-attachment :attach-widget
				      :top-widget errmsg
				      :left-attachment :attach-form
				      :font-list *header-font*))
	 (restarts (create-row-column form "debugRestarts"
				      :adjust-last nil
				      :top-attachment :widget
				      :top-widget rlabel
				      :left-attachment :attach-form
				      :right-attachment :attach-form
				      :left-offset 10))
	 (btlabel (create-label form "backtraceLabel"
				       :label-string "Stack Backtrace:"
				       :font-list *header-font*
				       :top-attachment :attach-widget
				       :top-widget restarts
				       :left-attachment :attach-form))
	 (btwindow (create-scrolled-window form "backtraceWindow"
					   :scrolling-policy :automatic
					   :scroll-bar-placement :bottom-right
					   :left-attachment :attach-form
					   :right-attachment :attach-form
					   :left-offset 4
					   :right-offset 4
					   :bottom-offset 4
					   :bottom-attachment :attach-form
					   :top-attachment :attach-widget
					   :top-widget btlabel))
	 (backtrace (create-row-column btwindow "debugBacktrace"
				       :adjust-last nil
				       :spacing 1)))

    (manage-child frame) (manage-child form)
    (manage-children menu-bar errlabel errmsg rlabel restarts btlabel btwindow)
    (manage-child backtrace)
    (manage-child cascade)

    (debug-display-error errmsg condition)
    
    (if *debug-restarts*
	(progn
	  (set-values rlabel :label-string "Restarts:")
	  (debug-display-restarts restarts))
	(set-values rlabel :label-string "No restarts available"))

    (let ((quick-stack (create-highlight-button backtrace "quickStack"
						"Display Stack")))
      (add-callback quick-stack :activate-callback
		    #'(lambda (w c) (declare (ignore w c))
			(debug-display-stack backtrace)))
      (manage-child quick-stack))


    (setf *current-debug-display*
	  (make-debug-display debug-pane errmsg restarts backtrace))

    (popup-interface-pane debug-pane)
    debug-pane))

;;; REUSE-DEBUGGER -- Internal
;;;
;;; Takes an old debugger pane and a new condition.  It renovates the old
;;; display to reflect the current debugging state.  This should be used
;;; whenever possible since it is quite a bit faster than creating a new
;;; debugger pane from scratch.
;;;
(defun reuse-debugger (condition info)
  (let ((debug-pane (dd-info-debug-pane info))
	(errmsg (dd-info-errmsg info))
	(restarts (dd-info-restarts info))
	(backtrace (dd-info-backtrace info)))

    (debug-display-error errmsg condition)
    (debug-display-restarts restarts)

    (let* ((buttons (xti:widget-children backtrace))
	   (quick-stack (car buttons)))
      (remove-all-callbacks quick-stack :activate-callback)
      (set-values quick-stack :label-string "Display Stack")
      (add-callback quick-stack :activate-callback
		    #'(lambda (w c) (declare (ignore w c))
			(debug-display-stack backtrace)))
      (manage-child quick-stack)
      (apply #'unmanage-children (cdr buttons)))

    (setf *current-debug-display* info)
    (popup-interface-pane debug-pane)
    debug-pane))

;;; CREATE-DEBUGGER -- Internal
;;;
;;; Creates a graphical debugger display for the given condition.  It will
;;; attempt to reuse any available old panes.  However, if none are
;;; available, it will create a new display frame.
;;;
(defun create-debugger (condition)
  (if *old-display-frames*
      (reuse-debugger condition (pop *old-display-frames*))
      (really-create-debugger condition)))

;;; CLOSE-MOTIF-DEBUGGER -- Internal
;;;
;;; This function should always be called before leaving the context of the
;;; debugger.  It closes down the frame windows and marks the main debug
;;; display pane as ready for recycling.
;;;
(defun close-motif-debugger (condition)
  (declare (ignore condition))
  (push *current-debug-display* *old-display-frames*)
  ;;
  ;; Destroy all frame panes
  (dolist (info *debug-active-frames*)
    (destroy-widget (cdr info)))
  (setf *debug-active-frames* nil)
  ;;
  ;; Remove the restart/backtrace window
  (popdown (dd-info-debug-pane *current-debug-display*))
  (setf *current-debug-display* nil)

  (format t "Leaving debugger.~%"))

;;; INVOKE-MOTIF-DEBUGGER -- Internal
;;;
;;; This function essentially mimics the functions which manage the TTY
;;; debugger, but uses a graphical debugging display instead.
;;;
(defun invoke-motif-debugger (condition)
  (let* ((*in-the-debugger* t)
	 (frame (di:top-frame))
	 (previous-display *current-debug-display*)
	 (*current-debug-display* nil)
	 (*debug-active-frames* nil))
    (declare (ignore previous-display))
    (verify-system-server-exists)
    (multiple-value-bind (shell connection)
			 (create-interface-shell)
      (declare (ignore shell))
      (if connection
	  (with-motif-connection (connection)
	    (let ((pane (find-interface-pane condition))
		  (*current-frame* frame))
	      (unless pane
		(setf pane (create-debugger condition)))
	      (unless (is-managed pane)
		(popup-interface-pane pane))
	      (setf (dd-info-level *current-debug-display*)
		    *debug-command-level*)
	      (setf (dd-info-connection *current-debug-display*) connection)
	      (unwind-protect
		  (handler-case
		      (loop
			(system:serve-event))
		    (error (err)
			   (if *flush-debug-errors*
			       (interface-error (safe-condition-message err)
						pane)
			       (interface-error
				"Do not yet support recursive debugging"
				pane))))
		(when (and connection *current-debug-display*)
		  (with-motif-connection (connection)
		    (close-motif-debugger condition))))))
	  (invoke-tty-debugger condition)))))



;;; Used to prevent recursive invocations of the windowing debugger.
;;;
(defvar *in-windowing-debugger* nil)


;;; REAL-INVOKE-DEBUGGER -- Internal
;;;
;;; Invokes the Lisp debugger.  It decides whether to invoke the TTY
;;; debugger or the Motif debugger.
;;;
(defun real-invoke-debugger (condition)
  (if (or (not (use-graphics-interface))
	  *in-windowing-debugger*
	  (typep condition 'xti:toolkit-error))
      (invoke-tty-debugger condition)
      (let ((*in-windowing-debugger* t))
	(write-line "Invoking debugger...")
	(invoke-motif-debugger condition))))
