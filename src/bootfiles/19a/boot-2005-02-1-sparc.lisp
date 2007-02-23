;; Cross-compile bootfile used to change the gtemp register from %g7
;; (reserved) to reg l0.  L0 is no longer used by Lisp for anything.

;; Don't need anything special, so the default sample script is ok.
(load "target:tools/cross-scripts/cross-sparc-sparc")
