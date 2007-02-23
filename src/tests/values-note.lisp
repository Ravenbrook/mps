(defun grue (x)
  (declare (fixnum x))
  (cond ((minusp x)
	 (values x t))
	((oddp x)
	 nil)
	(t
	 (grue (grue (1- x))))))

(defun blue (x)
  (declare (fixnum x))
  (cond ((minusp x)
	 (values x t))
	((= x 42)
	 (snoo x))
	((evenp x)
	 (values (list (snoo x) (snoo x)) nil))
	(t
	 (blue (1- x)))))

(defun snoo (x)
  (declare (fixnum x))
  (if (zerop x)
      :yow!
      (snoo (1- x))))

(defun no-problem (x)
  (if (eq x :foo)
      (values :bar t)
      (values :zoom!)))
