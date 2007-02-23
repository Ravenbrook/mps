;;;; -*- Mode: Lisp ; Package: Interface -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/interface/inspect.lisp,v 1.11 2003/12/03 07:56:09 emarsden Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; This file implements the methods used in graphically inspecting Lisp
;;; objects.
;;;
;;; The inspector mechanism revolves around two generic functions:
;;;      - INSPECTOR-PANE-TITLE which returns a string meant to be the
;;; title of the inspection pane displaying the given object
;;;      - DISPLAY-INSPECTOR-PANE which creates a window pane displaying
;;; the relevant information about the given object
;;;
;;; You can add new display mechanisms by defining new methods for these
;;; generic functions.  Specific functions for aiding in the construction
;;; of inspection panes are given below.
;;;

(in-package "INTERFACE")


(defvar *inspector-huge-object-threshold* 20)
(defvar *inspector-sequence-initial-display* 5)



;;;; Inspector callbacks

(defun destroy-pane-callback (widget call-data object)
  (declare (ignore widget call-data))
  (setf *current-inspector-objects*
	(delete object *current-inspector-objects*))
  (destroy-interface-pane object))

(defun inspect-object-callback (widget call-data object)
  (declare (ignore widget call-data))
  (inspect object))

(defun inspect-eval-callback (widget call-data output shell)
  (declare (ignore widget call-data))
  (with-busy-cursor (shell)
    (inspect (xti:widget-user-data output))))

(defun eval-callback (widget call-data object output)
  (declare (ignore call-data))
  (let* ((input (car (get-values widget :value)))
	 (mark (text-get-last-position output))
	 (response
	  (format nil "* ~a~%~a~%"
		  input
		  (handler-case
		      (multiple-value-bind (out val)
					   (grab-output-as-string
					    (let ((* object))
					      (eval (read-from-string input))))
			(setf (xti:widget-user-data output) val)
			(format nil "~a~s~%" out val))
		    (error (cond)
		      (format nil "~2&~A~2&" cond)))))
	 (length (length response)))
    (declare (simple-string response))

    (text-set-string widget "")
    (text-insert output mark response)
    (text-set-insertion-position output (+ length mark))))

(defun popup-eval-callback (widget call-data pane object)
  (declare (ignore widget call-data))
  (multiple-value-bind (form shell)
		       (create-form-dialog pane "evalDialog")
    (let* ((s1 (compound-string-create "Eval: " "HeaderFont"))
	   (s2 (compound-string-create
		(format nil "[~a]" (print-for-widget-display "~S" object))
		"EntryFont"))
	   (s3 (compound-string-concat s1 s2))
	   (done (create-push-button-gadget form "evalDone"
					    :label-string "Done"
					    :bottom-attachment :attach-form))
	   (inspect (create-push-button-gadget form "evalInspect"
					       :label-string "Inspect Last"
					       :bottom-attachment :attach-form
					       :left-attachment :attach-widget
					       :left-widget done))
	   (entry (create-text form "evalEntry"
			       :bottom-attachment :attach-widget
			       :bottom-widget done
			       :left-attachment :attach-form
			       :right-attachment :attach-form))
	   (prompt (create-label-gadget form "evalPrompt"
					:bottom-attachment :attach-widget
					:bottom-widget entry
					:font-list *all-fonts*
					:label-string s3))
	   (output (create-text form "evalOutput"
				:edit-mode :multi-line-edit
				:editable nil
				:rows 8
				:columns 40
				:top-attachment :attach-form
				:bottom-attachment :attach-widget
				:bottom-widget prompt
				:left-attachment :attach-form
				:right-attachment :attach-form)))
      (compound-string-free s1)
      (compound-string-free s2)

      (set-values shell :title "Inspector Eval"
		        :keyboard-focus-policy :pointer)
      (add-callback entry :activate-callback 'eval-callback object output)
      (add-callback inspect :activate-callback
		    'inspect-eval-callback output shell)
      (add-callback done :activate-callback #'destroy-callback shell)
      (manage-children done inspect entry prompt output)
      (manage-child form))))



;;;; Methods for constructing the title of inspection panes

;;; INSPECTOR-PANE-TITLE -- Public
;;;
;;; This function takes a Lisp object and returns a string which is meant
;;; to be the title of the inspection pane displaying the given object.

;;; This particular method is a catch-all for the types which PCL does not
;;; allow us to discriminate.
;;;
(defmethod inspector-pane-title (object)
  (let ((*print-level* (or debug:*debug-print-level* *print-level*))
	(*print-length* (or debug:*debug-print-length* *print-length*)))
    (typecase object
      (standard-object
       (format nil "Instance ~a of Class ~a" object (type-of object)))
      (function (format nil "~a" object))
      (structure-object
       (let ((default (format nil "~a" object)))
	 (declare (simple-string default))
	 (if (and (> (length default) 2)
		  (char= (schar default 0) #\#)
		  (char= (schar default 1) #\S))
	     (format nil "#<~a Structure>" (type-of object))
	     default)))
      (t
       (format nil "~a ~a" (string-capitalize (type-of object))
	       (print-for-widget-display "~s" object))))))

(defmethod inspector-pane-title ((sym symbol))
  (format nil "Symbol ~s" sym))

(defmethod inspector-pane-title ((v vector))
  (declare (vector v))
  (let ((length (length v))
	(type (type-of v)))
    (format nil "~a of length ~a"
	    (string-capitalize (if (listp type) (car type) type))
	    length)))

(defmethod inspector-pane-title ((a array))
  (let ((dimensions (array-dimensions a)))
    (format nil "Array of ~a  Dimensions = ~s"
	    (array-element-type a) dimensions)))

(defmethod inspector-pane-title ((l list))
  (if (listp (cdr l))
      (format nil "List of length ~a" (length l))
      (format nil "Dotted Pair")))

(defmethod inspector-pane-title ((i integer))
  (format nil "Integer ~D" i))



;;;; Methods for displaying object inspection panes

;;; WITH-INSPECTOR-PANE -- Public
;;;
;;; This macro is the primary tool for building inspection panes.  It
;;; creates all the fundamental pieces of the display pane and then calls
;;; the supplied body to create the rest.  A typical display method would
;;; like something like:
;;;    (defmethod display-inspector-pane ((x mytype))
;;;      (with-inspector-pane (x)
;;;        ... custom forms ...
;;;      ))
;;;
(defmacro with-inspector-pane ((object) &body forms)
  `(multiple-value-bind
       (pane is-new)
       (create-interface-pane-shell (format nil "Inspect: ~a" (type-of ,object))
				    ,object)
     (when is-new
       (let* ((frame (create-frame pane "inspectFrame"))
	      (over-form (create-form frame "inspectForm"))
	      (menu-bar (create-menu-bar over-form "menubar"
					 :left-attachment :attach-form
					 :right-attachment :attach-form))
	      (obmenu (create-interface-menu
		       menu-bar "Object"
		       ,``(("Eval Expression" popup-eval-callback ,pane ,,object)
			   ("Close Pane" destroy-pane-callback ,,object)
			   ("Close All Panes" close-all-callback))))
	      (title (create-label-gadget
		      over-form "inspectTitle"
		      :label-string (inspector-pane-title ,object)
		      :font-list *header-font*
		      :top-attachment :attach-widget
		      :top-widget menu-bar
		      :left-attachment :attach-form
		      :right-attachment :attach-form))
	      (form (create-form over-form "inspectForm"
				 :left-attachment :attach-form
				 :right-attachment :attach-form
				 :bottom-attachment :attach-form
				 :top-attachment :attach-widget
				 :top-widget title)))

	 ,@forms
	 (manage-child frame)
	 (manage-child over-form)
	 (manage-child obmenu)
	 (manage-children menu-bar title form)))
     (popup-interface-pane pane)))

;;; DISPLAY-INSPECTOR-PANE -- Public
;;;
;;; This function takes an object and creates a graphical inspection pane
;;; for displaying it.

;;; This particular method is a catch all for the types which PCL won't
;;; specialize on.
;;;
(defmethod display-inspector-pane (object)
  (typecase object
    (standard-object (display-clos-pane object))
    (function (display-function-pane object))
    (structure-object (display-structure-pane object))
    (t
     (with-inspector-pane (object)
       (let ((label (create-label-gadget
		     form "label"
		     :label-string (format nil "~s" object))))
	 (manage-child label))))))



(defmethod display-inspector-pane ((sym symbol))
  (with-inspector-pane (sym)
    (let* ((value (if (boundp sym) (symbol-value sym) "Unbound"))
	   (function (if (fboundp sym) (symbol-function sym) "Undefined"))
	   (plist (symbol-plist sym))
	   (package (symbol-package sym))
	   (rc (create-row-column form "rowColumn"))
	   (vview (create-value-box rc "Value:" value
				    :callback 'inspect-object-callback
				    :activep (boundp sym)))
	   (fview (create-value-box rc "Function:" function
				    :callback 'inspect-object-callback
				    :activep (fboundp sym)))
	   (plview (create-value-box rc "PList:" plist
				     :callback 'inspect-object-callback))
	   (pview (create-value-box rc "Package:" package
				    :callback 'inspect-object-callback)))
      (manage-child rc)
      (manage-children vview fview plview pview))))

(defun is-traced (function)
  (let ((fun (debug::trace-fdefinition function)))
    (if (gethash fun debug::*traced-functions*) t)))

(defun trace-function-callback (widget call-data function)
  (declare (ignore widget))
  (if (toggle-button-callback-set call-data)
      (debug::trace-1 function (debug::make-trace-info))
      (debug::untrace-1 function)))

(defun display-function-pane (f)
  (with-inspector-pane (f)
    (multiple-value-bind
	(dstring dval)
	(grab-output-as-string (describe f))
      (declare (ignore dval))
      (let* ((trace (create-toggle-button-gadget form "functionTrace"
						 :bottom-attachment :attach-form
						 :label-string "Trace Function"
						 :set (is-traced f)))
	     (sep (create-separator-gadget form "separator"
					   :left-attachment :attach-form
					   :right-attachment :attach-form
					   :bottom-attachment :attach-widget
					   :bottom-widget trace))
	     (descview (create-scrolled-window form "scrolledView"
					       :left-attachment :attach-form
					       :right-attachment :attach-form
					       :bottom-attachment :attach-widget
					       :bottom-widget sep
					       :top-attachment :attach-form
					       :scrolling-policy :automatic))
	     (desc (create-label descview "functionDescription"
				 :alignment :alignment-beginning
				 :label-string dstring)))

	(add-callback trace :value-changed-callback
		      'trace-function-callback f)
	(manage-child desc)
	(manage-children trace sep descview)))))

(defun display-structure-pane (s)
  (with-inspector-pane (s)
    (let* ((dd (kernel:layout-info (kernel:%instance-layout s)))
	   (dsds (kernel:dd-slots dd))
	   (viewer (when (> (length dsds) *inspector-huge-object-threshold*)
		     (create-scrolled-window form "structureViewer"
					     :left-attachment :attach-form
					     :right-attachment :attach-form
					     :top-attachment :attach-form
					     :bottom-attachment :attach-form
					     :scrolling-policy :automatic)))
	   (rc (create-row-column (or viewer form) "rowColumn"))
	   (widgets))
      (declare (list dsds))
      (dolist (dsd dsds)
	(push
	 (create-value-box rc (format nil "~A:"
				      (string-capitalize
				       (kernel:dsd-%name dsd)))
			   (funcall (kernel:dsd-accessor dsd) s)
			   :callback #'inspect-object-callback)
	 widgets))
      (apply #'manage-children widgets)
      (manage-child rc)
      (when viewer (manage-child viewer)))))


(defun sequence-redisplay-callback (widget call-data v s-text c-text view pane)
  (declare (ignore widget call-data))
  (handler-case
      (let* ((start (read-from-string (car (get-values s-text :value))))
	     (count (read-from-string (car (get-values c-text :value))))
	     (length (length v))
	     (widgets (reverse (xti:widget-children view)))
	     (unused-ones)
	     (used-ones))

	(when (> (+ start count) length)
	  (setf count (- length start))
	  (set-values c-text :value (format nil "~a" count)))

	(when (minusp start)
	  (setf start 0)
	  (set-values s-text :value (format nil "~a" 0)))

	(dolist (widget widgets)
	  (if (zerop count)
	      (push widget unused-ones)
	      (progn
		(set-value-box widget (format nil "~a:" start) (elt v start)
			       :callback #'inspect-object-callback)
		(push widget used-ones)
		(incf start)
		(decf count))))

	(dotimes (i count)
	  (let ((pos (+ start i)))
	    (push (create-value-box view (format nil "~a:" pos) (elt v pos)
				    :callback #'inspect-object-callback)
		  used-ones)))
	(when unused-ones
	    (apply #'unmanage-children unused-ones))
	(apply #'manage-children used-ones))
    (error (e)
      (interface-error (format nil "~a" e) pane))))

(defun sequence-filter-callback (widget call-data v fexp view pane)
  (declare (ignore widget call-data))
  (handler-case
      (let* ((exp (read-from-string (car (get-values fexp :value))))
	     (length (length v))
	     (widgets (reverse (xti:widget-children view)))
	     (used-ones))
	(dotimes (index length)
	  (let* ((item (elt v index))
		 (* item)
		 (** index))
	    (when (eval exp)
	      (let ((widget (pop widgets)))
		(if widget
		    (set-value-box widget (format nil "~a:" index) item
				   :callback #'inspect-object-callback)
		    (setf widget (create-value-box
				  view (format nil "~a:" index) item
				  :callback #'inspect-object-callback)))
		(push widget used-ones)))))
	(when widgets
	  (apply #'unmanage-children widgets))
	(apply #'manage-children used-ones))
    (error (e)
	   (interface-error (format nil "~a" e) pane))))

(defun display-cons-pane (p form)
  (let* ((rc (create-row-column form "rowColumn"))
	 (car-view (create-value-box rc "Car:" (car p)
				     :callback #'inspect-object-callback))
	 (cdr-view (create-value-box rc "Cdr:" (cdr p)
				     :callback #'inspect-object-callback)))
    (manage-children car-view cdr-view)
    (manage-child rc)))

(defmethod display-inspector-pane ((v sequence))
  (with-inspector-pane (v)
    (if (and (listp v)
	     (not (listp (cdr v))))
	(display-cons-pane v form)
	(let* ((length (length v))
	       (controls (create-row-column form "sequenceStartHolder"
					    :left-attachment :attach-form
					    :right-attachment :attach-form
					    :orientation :horizontal))
	       (slabel (create-label-gadget controls "sequenceStartLabel"
					    :font-list *header-font*
					    :label-string "Start:"))
	       (start (create-text controls "sequenceStart"
				   :value "0"
				   :columns 4))
	       (clabel (create-label-gadget controls "sequenceCountLabel"
					    :font-list *header-font*
					    :label-string "Count:"))
	       (count (create-text controls "sequenceCount"
				   :value "5"
				   :columns 4))
	       (filter (create-row-column form "sequenceFilterHolder"
					  :top-attachment :attach-widget
					  :top-widget controls
					  :left-attachment :attach-form
					  :right-attachment :attach-form
					  :orientation :horizontal))
	       (flabel (create-label-gadget filter "sequenceFilterLabel"
					    :font-list *header-font*
					    :label-string "Filter:"))
	       (fexp (create-text filter "sequenceFilterExp" :value "T"))
	       (apply (create-push-button-gadget filter "sequenceFilterApply"
						 :label-string "Apply"))
	       (unapply (create-push-button-gadget filter
						   "sequenceFilterUnapply"
						   :label-string "No Filter"))
	       (view (create-scrolled-window form "sequenceViewPort"
					     :scrolling-policy :automatic
					     :top-attachment :attach-widget
					     :top-widget filter
					     :bottom-attachment :attach-form
					     :left-attachment :attach-form
					     :right-attachment :attach-form))
	       (rc (create-row-column view "sequenceView"
				      :spacing 0))
	       (widgets))
	  
	  (manage-children slabel start clabel count)
	  (manage-children flabel fexp apply unapply)
	  
	  (dotimes (i (min length *inspector-sequence-initial-display*))
	    (let ((item (elt v i)))
	      (push (create-value-box rc (format nil "~a:" i) item
				      :callback #'inspect-object-callback)
		    widgets)))
	  
	  (apply #'manage-children widgets)
	  
	  (add-callback start :activate-callback 'sequence-redisplay-callback
			v start count rc pane)
	  (add-callback count :activate-callback 'sequence-redisplay-callback
			v start count rc pane)
	  (add-callback apply :activate-callback 'sequence-filter-callback
			v fexp rc pane)
	  (add-callback fexp :activate-callback 'sequence-filter-callback
			v fexp rc pane)
	  (add-callback unapply :activate-callback
			#'(lambda (widget call-data)
			    (declare (ignore widget call-data))
			    (sequence-redisplay-callback
			     nil nil v start count rc pane)))
	  
	  (manage-children view controls filter)
	  (manage-child rc)))))

(defun show-slot-list (object slot-list view allocp label)
  (let ((label (create-label-gadget view "slotLabel"
				    :label-string label
				    :font-list *header-font*))
	(widgets))
    (dolist (slotd slot-list)
      (with-slots ((slot pcl::name) (allocation pcl::allocation))
		      slotd
	(let* ((slot-label (if allocp
			       (format nil "~a: " slot)
			       (format nil "~a [~a]: " slot allocation)))
	       (slot-bound (slot-boundp object slot))
	       (slot-value (if slot-bound
			       (slot-value object slot)
			       "Unbound")))
	  (push
	   (create-value-box view slot-label slot-value
			     :callback #'inspect-object-callback
			     :activep slot-bound)
	   widgets)))
      (apply #'manage-children label widgets))))

(defun display-clos-pane (object)
  (with-inspector-pane (object)
    (let* ((class (pcl::class-of object))
	   (slotds (pcl::slots-to-inspect class object))
	   (view (create-row-column form "rowColumn"
				    :left-attachment :attach-form
				    :right-attachment :attach-form
				    :top-attachment :attach-form
				    :bottom-attachment :attach-form))
	   (instance-slots ())
	   (class-slots ())
	   (other-slots ()))
      
      (dolist (slotd slotds)
	(with-slots ((slot pcl::name) (allocation pcl::allocation))
	  slotd
	  (case allocation
	    (:instance (push slotd instance-slots))
	    (:class (push slotd class-slots))
	    (otherwise (push slotd other-slots)))))
      
      (when instance-slots
	(show-slot-list object instance-slots view t
			"Slots with Instance allocation:"))
      (when class-slots
	(show-slot-list object class-slots view t
			"Slots with Class allocation:"))
      (when other-slots
	(show-slot-list object other-slots view nil
			"Slots with Other allocation:"))

      (when view (manage-child view)))))



;;;; Functions for creating the Motif inspector

(defun start-motif-inspector (object)
  (verify-system-server-exists)
  (multiple-value-bind (shell connection)
		       (create-interface-shell)
    (declare (ignore shell))
    (with-motif-connection (connection)
      (verify-control-pane-displayed)
      (display-inspector-pane object)
      (push object *current-inspector-objects*)
      (inspector-add-history-item object)))
  object)



;;;; User visible INSPECT function

;;; INSPECT -- Public.
;;;
(defun inspect (object &optional (style interface:*interface-style*))
  "This function allows the user to interactively examine Lisp objects.
   STYLE indicates whether this should run with a :graphics interface
   or a :command-line oriented one; when running without X, there is no
   choice.  Supplying :window, :windows, :graphics, :graphical, and :x gets
   a windowing interface, and supplying :command-line or :tty gets the
   other style."

  (let ((interface:*interface-style* style))
    (if (use-graphics-interface)
	(start-motif-inspector object)
        (inspect::tty-inspect object))))
