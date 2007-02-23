(defun foo (n)
  (dotimes (i n (or *undefined* n))))
