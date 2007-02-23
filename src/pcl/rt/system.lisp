;;; Copyright (C) 2002 Gerd Moellmann <gerd.moellmann@t-online.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

#+cmu
(ext:file-comment "$Header: /project/cmucl/cvsroot/src/pcl/rt/system.lisp,v 1.3 2003/06/13 21:08:22 gerd Exp $")

(setf (logical-pathname-translations "pcl-test")
      `(("*.*.*" "/home/gerd/cmucl/src/pcl/rt/")))

(mk:defsystem :pcl-test
    :initially-do (progn )
    :source-pathname "pcl-test:"
    :binary-pathname "pcl-test:"
    :components
    ((:file "pkg"
	    :source-extension "lisp")
     #+gerds-pcl
     (:file "ctor"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "defclass"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "make-instance"
	    :source-extension "lisp"
	    :depends-on ("pkg" #+gerds-pcl "ctor"))
     (:file "reinitialize-instance"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-value"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-boundp"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-missing"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-accessors"
	    :source-extension "lisp"
	    :depends-on ("pkg" "make-instance"))
     (:file "slot-type"
	    :source-extension "lisp"
	    :depends-on ("pkg" "slot-value"))
     (:file "inline-access"
	    :source-extension "lisp"
	    :depends-on ("pkg" "slot-type"))
     (:file "method-combination"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "pv"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "defgeneric"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "defmethod"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "find-method"
	    :source-extension "lisp"
	    :depends-on ("pkg"))
     (:file "methods"
	    :source-extension "lisp"
	    :depends-on ("pkg"))))
