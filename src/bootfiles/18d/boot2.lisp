#+sparc
(in-package "SPARC")
#+sparc
(defmacro allocation (result-tn size lowtag)
  ;; We assume we're in a pseudo-atomic so the pseudo-atomic bit is
  ;; set.  If the lowtag also has a 1 bit in the same position, we're all
  ;; set.  Otherwise, we need to zap out the lowtag from alloc-tn, and
  ;; then or in the lowtag.
  `(if (logbitp (1- lowtag-bits) ,lowtag)
     (progn
       (inst or ,result-tn alloc-tn ,lowtag)
       (inst add alloc-tn ,size))
     (progn
       (inst andn ,result-tn alloc-tn lowtag-mask)
       (inst or ,result-tn ,lowtag)
       (inst add alloc-tn ,size))))
