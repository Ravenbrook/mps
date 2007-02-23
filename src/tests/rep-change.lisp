(defun foo (x)
  (etypecase x
    (single-float
     (let ((x x))
       (declare (single-float x))
       (+ x x)))))
