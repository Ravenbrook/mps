;; Bootstrap the pseudo-atomic trap genesis change to put
;; trap_PseudoAtomic into internals.h.
#+sparc
(load "target:code/exports.lisp")
#+sparc
(in-package :sparc)
#+sparc
(defconstant pseudo-atomic-trap 16)

