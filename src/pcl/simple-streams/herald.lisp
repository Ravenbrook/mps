;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/simple-streams/herald.lisp,v 1.1 2003/06/07 17:56:28 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; 

(in-package "STREAM")

(setf (getf ext:*herald-items* :simple-streams)
      `("    Simple Streams"))

(pushnew :simple-streams *features*)
(provide :simple-streams)
