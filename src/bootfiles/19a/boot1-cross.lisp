;;; Simple cross-compile script for bootstrapping the addition of a
;;; slot to the weak-pointer object.

#+x86
(load "target:tools/cross-scripts/cross-x86-x86.lisp")
#+sparc
(load "target:tools/cross-scripts/cross-sparc-sparc.lisp")
