(defun ! (n)
  (declare (integer n))
  (if (zerop n)
      1
      (* (! (1- n)) n)))

(defun ! (n res)
  (declare (integer n res))
  (if (zerop n)
      res
      (! (1- n) (* n res))))
