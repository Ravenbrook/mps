;;; Bootstrap for modular arithmetic.

(pushnew :modular-arith *features*)

(in-package "KERNEL")
(defvar *modular-funs*
  (make-hash-table :test 'eq))
