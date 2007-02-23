;; Add the constants for precision control on x86.
;;
#+x86
(in-package :x86)
#+x86
(progn
  (defconstant float-precision-24-bit  0)
  (defconstant float-precision-53-bit  2)
  (defconstant float-precision-64-bit  3))

