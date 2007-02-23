(in-package 'c)

(defun foo (x)
  (declare (fixnum x))
  (ldb (byte 3 5) x))
