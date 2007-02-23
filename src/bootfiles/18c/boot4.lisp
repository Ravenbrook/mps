(in-package "VM")

;; Add some new constants for the Sparc backend.
#+sparc
(progn
  (defconstant fixnum-tag-bits (1- lowtag-bits)
    "Number of tag bits used for a fixnum")

  (defconstant fixnum-tag-mask (1- (ash 1 fixnum-tag-bits))
    "Mask to get the fixnum tag")

  (defconstant positive-fixnum-bits (- word-bits fixnum-tag-bits 1)
    "Maximum number of bits in a positive fixnum")
  )
