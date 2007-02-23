;;; -*- Package: C -*-

(in-package 'c)

(defun foo (x)
  (multiple-value-bind (y z)
		       (glortzx x)
    (declare (type fixnum y z))
    (the fixnum (+ y z))))
