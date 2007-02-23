;;; Bootfile to bootstrap the fixes to loop to handle loop intializers
;;; correctly.  (See cmucl-imp, 2004-08-20.) Might be something
;;; simpler, but this is easy.

(setf lisp::*enable-package-locked-errors* nil)
(load "target:code/loop.lisp")


