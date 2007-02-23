;; This simple file is used to bootstrap the weak hash table changes.
;; The cross-compile is only needed because we want to add four new
;; static symbols so the C code can access them.

#+ppc
(load "target:tools/cross-scripts/cross-ppc-ppc-darwin.lisp")

#+sparc
(load "target:tools/cross-scripts/cross-sparc-sparc.lisp")

#+x86
(load "target:tools/cross-scripts/cross-x86-x86.lisp")



