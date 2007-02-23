(defun test (x y)
  (snork (list y y y))
  (prog1 (car x)
    (snork 3)
    (snork (list y y y))))

(defun snork (x)
  (funcall 'identity x))
