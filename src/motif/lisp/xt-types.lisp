;;;; -*- Mode: Lisp, Fill ; Package: Toolkit -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/motif/lisp/xt-types.lisp,v 1.4 1997/08/22 20:49:29 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; Written by Michael Garland
;;;
;;; This file defines the data types allowed in communication between the
;;; Lisp client and the C server.
;;;

(in-package "TOOLKIT")

(declaim (simple-vector *type-table*))

(defparameter *class-table* 
  (make-array 40 :element-type 'cons
	      :adjustable t :fill-pointer 0))
(declaim (vector *class-table*))

(defparameter next-type-tag 0)



;;;; Functions for defining data types

;;; enumeration values will have a :enum-value on the symbol plist
(defun def-toolkit-enum (type values)
  ;; Values begin at zero by default and increase by one each time
  (let ((current 0)
	(values-alist)
	(main-value))
    (declare (fixnum current))
    (dolist (value values)
      (setf main-value value)
      (when (listp value)
	(when (numberp (car value))
	  (setf current (car value))
	  (setf value (cdr value)))
	(setf main-value (car value))
	(dolist (synonym (cdr value))
	  (setf (get synonym :enum-value) current)))
      (setf (get main-value :enum-value) current)
      (push (cons current main-value) values-alist)
      (incf current))
    (setf (gethash type *enum-table*) values-alist)))
    
(defun def-toolkit-types (types)
  (dolist (type types)
    (let ((name (first type))
	  (kind (second type)))
      (setf (get name :xtk-type-tag) next-type-tag)
      (when (eq kind :enum) (setf kind name))
      (setf (svref *type-table* next-type-tag) (cons name kind))
      (incf next-type-tag))))



;;;; Defining of widget classes

(defun def-motif-classes (shells motif-shells classes)
  (flet ((register-shell (name format-arg)
	   (setf (get name :widget-class) (fill-pointer *class-table*))
	   (vector-push-extend
	    (cons name (format nil format-arg (symbol-resource name)))
	    *class-table*))
	 (register-class (name format-arg)
	   (setf (get name :widget-class) (fill-pointer *class-table*))
	   (vector-push-extend
	    (cons name (format nil format-arg (symbol-class name)))
	    *class-table*)))
    (dolist (shell shells)
      (register-shell shell "~aWidgetClass"))
    (dolist (motif-shell motif-shells)
      (register-class motif-shell "xm~aWidgetClass"))
    (dolist (thing classes)
      (let ((widget (first thing))
	    (gadget (second thing)))
	(register-class widget "xm~aWidgetClass")
	(when gadget
	  (register-class gadget "xm~aClass"))))))



;;;; Widget classes

(def-motif-classes
  ;; These are the generic Xt shell widget classes
  '(:override-shell :transient-shell :top-level-shell :application-shell)
  ;;
  ;; These are specific Motif shell widget classes
  '(:dialog-shell :menu-shell)
  ;;
  ;; These are the various other widget classes (all Motif)
  '((:label :label-gadget)
    (:arrow-button :arrow-button-gadget)
    (:push-button :push-button-gadget)
    (:toggle-button :toggle-button-gadget)
    (:cascade-button :cascade-button-gadget)
    (:separator :separator-gadget)
    (:drawn-button)
    (:menu-shell)
    (:drawing-area)
    (:dialog-shell)
    (:bulletin-board)
    (:command)
    (:file-selection-box)
    (:form)
    (:message-box)
    (:selection-box)
    (:scroll-bar)
    (:text)
    (:text-field)
    (:row-column)
    (:scale)
    (:frame)
    (:list)
    (:main-window)
    (:scrolled-window)
    (:paned-window)))



;;;; Motif data types
;;;
;;; The types MUST be listed in alphabetical order
;;;

(def-toolkit-types
  '((:accelerator-table :accelerator-table) (:alignment :enum)
    (:arrow-direction :enum) (:atom :atom) (:attachment :enum)
    (:bitmap :xid) (:bool :boolean) (:boolean :boolean) (:callback-reason :enum)
    (:cardinal :int) (:char :short) (:color :color) (:colormap :xid)
    (:command-window-location :enum) (:cursor :xid) (:default-button-type :enum)
    (:dialog-style :enum) (:dialog-type :enum) (:dimension :short)
    (:edit-mode :enum) (:enum :enum) (:event :event) (:file-type-mask :enum)
    (:float :float) (:font :xid) (:font-list :font-list)
    (:function :function) (:grab-kind :enum) (:highlight-mode :enum)
    (:horizontal-dimension :short) (:horizontal-int :int)
    (:horizontal-position :short)
    (:indicator-type :enum) (:initial-state :int) (:int :int)
    (:int-list :int-list) (:keyboard-focus-policy :enum) (:label-type :enum)
    (:list-size-policy :enum) (:multi-click :enum) (:navigation-type :enum)
    (:packing :enum) (:pixel :int) (:pixmap :xid) (:pointer :int)
    (:position :short) (:processing-direction :enum) (:resize-policy :enum)
    (:resource-list :resource-list) (:resource-names :resource-names)
    (:row-column-type :enum) (:scroll-bar-display-policy :enum)
    (:scroll-bar-placement :enum) (:scrolling-policy :enum)
    (:selection-policy :enum) (:separator-type :enum) (:shadow-type :enum)
    (:shell-horiz-dim :short) (:shell-horiz-pos :short) (:shell-vert-dim :short)
    (:shell-vert-pos :short)
    (:short :short) (:string :string) (:string-direction :enum)
    (:string-table :string-table) (:string-token :string-token)
    (:translation-table :translation-table) (:traversal-direction :enum)
    (:unit-type :enum) (:unsigned-char :short)
    (:vertical-dimension :short) (:vertical-int :int)
    (:vertical-position :short)(:visual-policy :enum)
    (:widget :widget) (:widget-class :widget-class)
    (:widget-list :widget-list) (:window :xid) (:xm-string :xm-string)
    (:xm-string-table :xm-string-table)))

(def-toolkit-enum :arrow-direction
  '(:arrow-up :arrow-down :arrow-left :arrow-right))

(def-toolkit-enum :shadow-type
  '((5 :shadow-etched-in :etched-in) (:shadow-etched-out :etched-out)
    (:shadow-in :in) (:shadow-out :out)))

(def-toolkit-enum :alignment
  '((:alignment-beginning :beginning) (:alignment-center :center)
    (:alignment-end :end)))

(def-toolkit-enum :attachment
  '((:attach-none :none) :attach-form
    (:attach-opposite-form :opposite-form) (:attach-widget :widget)
    (:attach-opposite-widget :opposite-widget) (:attach-position :position)
    (:attach-self :self)))

(def-toolkit-enum :resize-policy
  '((:resize-none :none) (:resize-grow :grow) (:resize-any :any)))

(def-toolkit-enum :separator-type
  '(:no-line :single-line :double-line :single-dashed-line
   :double-dashed-line :shadow-etched-in :shadow-etched-out))

(def-toolkit-enum :keyboard-focus-policy
  '(:explicit :pointer))

(def-toolkit-enum :row-column-type
  '(:work-area :menu-bar :menu-pulldown :menu-popup :menu-option))

(def-toolkit-enum :orientation
  '(:no-orientation :vertical :horizontal))

(def-toolkit-enum :grab-kind
  '(:grab-none :grab-nonexclusive :grab-exclusive))

(def-toolkit-enum :edit-mode
  '(:multi-line-edit :single-line-edit))

(def-toolkit-enum :callback-reason
  '(:cr-none :cr-help :cr-value-changed :cr-increment :cr-decrement
    :cr-page-increment :cr-page-decrement :cr-to-top :cr-to-bottom :cr-drag
    :cr-activate :cr-arm :cr-disarm (16 :cr-map) :cr-unmap :cr-focus
    :cr-losing-focus :cr-modifying-text-value :cr-moving-insert-cursor
    :cr-execute :cr-single-select :cr-multiple-select :cr-extended-select
    :cr-browse-select :cr-default-action :cr-clipboard-data-request
    :cr-clipboard-data-delete :cr-cascading :cr-ok :cr-cancel (34 :cr-apply)
    :cr-no-match :cr-command-entered :cr-command-changed :cr-expose
    :cr-resize :cr-input :cr-gain-primary :cr-lose-primary :cr-create
    (6666 :cr-protocols)))

(def-toolkit-enum :default-button-type
  '(:dialog-none :dialog-apply-button :dialog-cancel-button
    :dialog-default-button :dialog-ok-button :dialog-filter-label
    :dialog-filter-text :dialog-help-button
    (:dialog-list :dialog-history-list :dialog-file-list)
    (:dialog-list-label :dialog-file-list-label) :dialog-message-label
    (:dialog-selection-label :dialog-prompt-label) :dialog-symbol-label
    (:dialog-text :dialog-value-text :dialog-command-text)
    :dialog-separator :dialog-dir-list :dialog-dir-list-label))

(def-toolkit-enum :dialog-style
  '((:dialog-modeless :dialog-work-area)
    (:dialog-primary-application-modal :dialog-application-modal)
    (:dialog-full-application-modal) (:dialog-system-modal)))

(def-toolkit-enum :dialog-type
  '(:dialog-work-area
    (:dialog-error       :dialog-prompt)
    (:dialog-information :dialog-selection)
    (:dialog-message     :dialog-command)
    (:dialog-question    :dialog-file-selection)
    :dialog-warning :dialog-working))

(def-toolkit-enum :file-type-mask
  '((1 :file-directory) :file-regular :file-any-type))

(def-toolkit-enum :command-window-location
  '(:command-above-workspace :command-below-workspace))

(def-toolkit-enum :multi-click '(:multiclick-discard :multiclick-keep))

(def-toolkit-enum :navigation-type
  '(:none :tab-group :sticky-tab-group :exclusive-tab-group))

(def-toolkit-enum :processing-direction
  '(:max-on-top :max-on-bottom :max-on-left :max-on-right))

(def-toolkit-enum :list-size-policy '(:variable :constant :resize-if-possible))

(def-toolkit-enum :unit-type
  '(:pixels :100th-millimeters :1000th-inches :100th-points :100th-font-units))

(def-toolkit-enum :indicator-type '((1 :n-of-many) :one-of-many))

(def-toolkit-enum :selection-policy
  '(:single-select :multiple-select :extended-select :browse-select))

(def-toolkit-enum :string-direction
  '(:string-direction-l-to-r :string-direction-r-to-l))

(def-toolkit-enum :scroll-bar-display-policy '(:static :as-needed))

(def-toolkit-enum :scroll-bar-placement
  '(:bottom-right :top-right :bottom-left :top-left))

(def-toolkit-enum :scrolling-policy '(:automatic :application-defined))

(def-toolkit-enum :visual-policy '(:variable :constant))

(def-toolkit-enum :label-type '((1 :pixmap) :string))

(def-toolkit-enum :packing
  '(:no-packing :pack-tight :pack-column :pack-none))

(def-toolkit-enum :traversal-direction
  '(:traverse-current :traverse-next :traverse-next :traverse-prev
    :traverse-home :traverse-next-tab-group :traverse-prev-tab-group
    :traverse-up :traverse-down :traverse-left :traverse-right))

(def-toolkit-enum :highlight-mode
  '(:highlight-normal :highlight-selected :highlight-secondary-selected))
