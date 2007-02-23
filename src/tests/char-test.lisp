(in-package 'c)

(defun test (x y)
  (declare (simple-string x y))
  (declare (optimize speed))
  (dotimes (i 10)
    (setf (aref x i)
	  (code-char (char-code (aref y i))))))
