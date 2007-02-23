;; Allocation macro for PPC to abstract out all allocation.
#+ppc
(in-package "PPC")
(setf lisp::*enable-package-locked-errors* nil)
#+ppc
(defmacro allocation (result-tn size lowtag &key stack-p temp-tn)
  (declare (ignore stack-p temp-tn))
  (let ((alloc-size (gensym)))
    `(let ((,alloc-size ,size))
       (if (logbitp (1- lowtag-bits) ,lowtag)
	   (progn
	     (inst ori ,result-tn alloc-tn ,lowtag))
	   (progn
	     (inst clrrwi ,result-tn alloc-tn lowtag-bits)
	     (inst ori ,result-tn ,result-tn ,lowtag)))
       (if (numberp ,alloc-size)
	   (inst addi alloc-tn alloc-tn ,alloc-size)
	   (inst add alloc-tn alloc-tn ,alloc-size)))))

