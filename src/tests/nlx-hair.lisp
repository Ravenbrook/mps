(in-package 'c)

(defun hair ()
  (multiple-value-bind
      (x y z)
      (block foo
	(hair-aux #'(lambda () (return-from foo (values 1 2 3)))))
    (declare (type frob x y z))
    (values x y z)))

(defun hair-aux (fun)
  (funcall fun))
