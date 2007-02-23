;;; -*- Package: HEMLOCK -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/hemlock/dylan.lisp,v 1.3 1994/10/31 04:50:12 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains a minimal dylan mode.
;;;
(in-package "HEMLOCK")

;;; hack ..

(setf (getstring "dylan" *mode-names*) nil)


(defmode "Dylan" :major-p t)
(defcommand "Dylan Mode" (p)
  "Put the current buffer into \"Dylan\" mode."
  "Put the current buffer into \"Dylan\" mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Dylan"))

(define-file-type-hook ("dylan") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Dylan"))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value #'generic-indent
  :mode "Dylan")

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "Dylan" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Dylan" :value "//")

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Dylan" :value nil)

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Dylan" :value "// ")

(bind-key "Delete Previous Character Expanding Tabs" #k"backspace"
	  :mode "Dylan")
(bind-key "Delete Previous Character Expanding Tabs" #k"delete" :mode "Dylan")

;;; hacks...

(shadow-attribute :scribe-syntax #\< nil "Dylan")
(shadow-attribute :scribe-syntax #\> nil "Dylan")
(bind-key "Self Insert" #k"\>" :mode "Dylan")
(bind-key "Scribe Insert Bracket" #k")" :mode "Dylan")
(bind-key "Scribe Insert Bracket" #k"]" :mode "Dylan")
(bind-key "Scribe Insert Bracket" #k"}" :mode "Dylan")
