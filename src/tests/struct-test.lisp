(in-package 'c)

(defstruct test
  (a nil :type fixnum))

(defun test (x y)
  (setf (test-a x) (test-a y)))
