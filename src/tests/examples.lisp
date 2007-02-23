(defun bar (x)
  (let (a)
    (declare (fixnum a))
    (setq a (foo x))
    a))

(defun foo (e l)
  (do ((current l (cdr current))
       ((atom current) nil))
      (when (eq (car current) e) (return current))))

(defun raz (foo)
  (let ((x (case foo
	     (:this 13)
	     (:that 9)
	     (:the-other 42))))
    (declare (fixnum x))
    (foo x)))

(defun f+ (x y)
  (declare (single-float x y))
  (+ x y))

(defun if-f+ (x y)
  (declare (single-float x y))
  (+ (if (yow) (zunk) x) y))

(defun cf+ (x y)
  (declare (single-float x y))
  (cons (+ x y) t))

(defun set-flo (x)
  (declare (single-float x))
  (prog ((var 0.0))
    (setq var (gorp))
    (setq var x)
    (return var)))

(defun if-cf+ (x y)
  (declare (single-float x y))
  (cons (if (grue) (+ x y) (snoc)) t))

(defun zf+ (x y)
  (declare (single-float x y))
  (zork (+ x y)))
