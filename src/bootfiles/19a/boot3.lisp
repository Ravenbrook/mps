;;; Bootstrap for modular arithmetic.  (Taken from 18e/boot22.lisp)
;;; (Primarily for ppc.)

#+ppc
(pushnew :modular-arith *features*)

(in-package "KERNEL")
#-+ppc
(defvar *modular-funs*
  (make-hash-table :test 'eq))
