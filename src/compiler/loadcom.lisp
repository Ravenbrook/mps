;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/loadcom.lisp,v 1.51 2004/11/05 22:02:38 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Load up the compiler.
;;;
(in-package "C")

(load "c:backend")
(load "c:macros")
(load "c:sset")
(load "c:node")
(load "c:alloc")
(load "c:ctype")
(load "c:knownfun")
(load "c:fndb")
(load "vm:vm-fndb")
(load "c:ir1util")
(load "c:ir1tran")
(load "c:loop")
(load "c:ir1final")
(load "c:array-tran")
(load "c:seqtran")
(load "c:typetran")
(load "vm:vm-typetran")
(load "vm:vm-tran")
(load "c:float-tran")
(load "c:saptran")
(load "c:srctran")
(load "c:locall")
(load "c:dfo")
(load "c:ir1opt")
(load "c:checkgen")
(load "c:constraint")
(load "c:envanal")
(load "c:vop")
(load "c:tn")
(load "c:bit-util")
(load "c:life")
(load "c:vmdef")
(load "c:gtn")
(load "c:ltn")
(load "c:stack")
(load "c:control")
(load "c:entry")
(load "c:ir2tran")
(load "vm:vm-ir2tran")
(load "c:pack")
(load "c:dyncount")
(load "c:codegen")
(load "c:main")
(load "c:meta-vmdef")
(load "c:disassem")
(load "c:new-assem")
(load "c:aliencomp")
(load "c:ltv")
(load "c:debug-dump")

(load "c:dump")
(load "c:debug")
(load "c:xref")
(load "c:copyprop")
(load "c:represent")

(load "c:eval-comp")
(load "c:eval")
(load "c:byte-comp")

(load "vm:core")
