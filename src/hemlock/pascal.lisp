;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/pascal.lisp,v 1.4 1994/10/31 04:50:12 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Just barely enough to be a Pascal/C mode.  Maybe more some day.
;;; 
(in-package "HEMLOCK")

(defmode "Pascal" :major-p t)
(defcommand "Pascal Mode" (p)
  "Put the current buffer into \"Pascal\" mode."
  "Put the current buffer into \"Pascal\" mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Pascal"))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value #'generic-indent
  :mode "Pascal")

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "Pascal" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Pascal" :value "(*")

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Pascal" :value " *)")

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Pascal" :value "(* ")

(shadow-attribute :scribe-syntax #\< nil "Pascal")
