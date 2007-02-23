
(in-package "KERNEL")
(use-package "VM")

(defun decode-single-float (x)
  (declare (single-float x))
  (let ((bits (single-float-bits (abs x))))
    (values (make-single-float
	     (dpb single-float-bias single-float-exponent-byte bits))
	    (truly-the single-float-exponent
		       (- (ldb single-float-exponent-byte bits)
			  single-float-bias))
	    (float-sign x))))


