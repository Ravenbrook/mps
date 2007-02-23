(defun raz (foo)
  (let ((x (case foo
	     (:this 13)
	     (:that 9)
	     (:the-other 42))))
    (declare (fixnum x))
    (+ x 55 x)))

(defun bar (x)
  (let (a)
    (declare (fixnum a))
    (setq a (foo x))
    a))

(defstruct (foo (:print-function fnord))
  (a (make-symbol "FOO") :type float)
  (b nil :type fixnum))
