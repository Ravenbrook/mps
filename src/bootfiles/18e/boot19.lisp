;; For Sparc only, to make these constants available when building
;; from a release that didn't have these defined.

#+sparc
(in-package :vm)
#+sparc
(defconstant pseudo-atomic-value (ash 1 (1- vm:lowtag-bits)))
#+sparc
(defconstant pseudo-atomic-interrupted-value 1)
