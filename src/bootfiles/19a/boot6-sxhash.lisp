;;; Cross-compile bootstrap file for new sxhash function.
;;;
;;; Nothing fancy needed.  Just load up the default cross-compile script.

#+ppc
(load "target:tools/cross-scripts/cross-ppc-ppc-darwin")
