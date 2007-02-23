;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/tools/hemload.lisp,v 1.3 1991/07/29 08:56:59 chiles Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file loads all of Hemlock.
;;;

;;; Stuff to set up the packages Hemlock uses.
;;;
(in-package "HEMLOCK-INTERNALS"
	    :nicknames '("HI")
	    :use '("LISP" "EXTENSIONS" "SYSTEM"))
;;;
(in-package "HEMLOCK"
	    :nicknames '("ED")
	    :use '("LISP" "HEMLOCK-INTERNALS" "EXTENSIONS" "SYSTEM"))
;;;
(in-package "SYSTEM")
(export '(%sp-byte-blt %sp-find-character %sp-find-character-with-attribute
		       %sp-reverse-find-character-with-attribute))
;;;
(in-package "HI")


(defun build-hemlock ()
  (load "hem:struct")
;  (load "hem:struct-ed")
  (load "hem:charmacs")
  (load "hem:key-event")
  (ext::re-initialize-key-events)
  (load "hem:keysym-defs")
  (load "hem:input")
  (load "hem:line")
  (load "hem:ring")
  (load "hem:vars")
  (load "hem:buffer")
  (load "hem:macros")
  (load "hem:interp")
  (load "hem:syntax")
  (load "hem:htext1")
  (load "hem:htext2")
  (load "hem:htext3")
  (load "hem:htext4")
  (load "hem:files")
  (load "hem:search1")
  (load "hem:search2")
  (load "hem:table")
  #+clx (load "hem:hunk-draw")
;  (load "hem:bit-stream")
  (load "hem:window")
  (load "hem:screen")
  (load "hem:winimage")
  (load "hem:linimage")
  (load "hem:display")
  (load "hem:termcap")
  #+clx (load "hem:bit-display")
  (load "hem:tty-disp-rt")
  (load "hem:tty-display")
;  (load "hem:tty-stream")
  (load "hem:pop-up-stream")
  #+clx (load "hem:bit-screen")
  (load "hem:tty-screen")
  (load "hem:cursor")
  (load "hem:font")
  (load "hem:streams")
  (load "hem:main")
  (load "hem:hacks")
  (%init-hemlock)
  (load "hem:echo")
  (load "hem:echocoms")
  (load "hem:command")
  (load "hem:indent")
  (load "hem:comments")
  (load "hem:morecoms")
  (load "hem:undo")
  (load "hem:killcoms")
  (load "hem:searchcoms")
  (load "hem:filecoms")
  (load "hem:doccoms")
  (load "hem:srccom")
  (load "hem:group")
  (load "hem:fill")
  (load "hem:text")
  (load "hem:lispmode")
  (load "hem:ts-buf")
  (load "hem:ts-stream")
  (load "hem:eval-server")
  (load "hem:lispbuf")
  (load "hem:lispeval")
  (load "hem:spell-rt")
  (load "hem:spell-corr")
  (load "hem:spell-aug")
  (load "hem:spellcoms")
  (load "hem:overwrite")
  (load "hem:abbrev")
  (load "hem:icom")
  (load "hem:kbdmac")
  (load "hem:defsyn")
  (load "hem:scribe")
  (load "hem:pascal")
  (load "hem:edit-defs")
  (load "hem:auto-save")
  (load "hem:register")
  (load "hem:xcoms")
  (load "hem:unixcoms")
  (load "hem:mh")
  (load "hem:highlight")
  (load "hem:dired")
  (load "hem:diredcoms")
  (load "hem:bufed")
  (load "hem:lisp-lib")
  (load "hem:completion")
  (load "hem:shell")
  (load "hem:debug")
  (load "hem:netnews")
  (load "hem:bindings"))
