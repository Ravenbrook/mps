;;; Cross-compile bootstrap file for new sxhash function.
;;;
;;; Nothing fancy needed.  Just load up the default cross-compile script.

#+sparc
(load "target:tools/cross-scripts/cross-sparc-sparc")

#+x86
(load "target:tools/cross-scripts/cross-x86-x86")
