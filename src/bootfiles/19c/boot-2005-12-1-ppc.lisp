;; This is a cross-compile script for ppc/darwin for moving the FDEFN
;; register.  R10 is an integer arg register for calling C, so FDEFN
;; shouldn't be there.

(load "target:tools/cross-scripts/cross-ppc-ppc-darwin")
