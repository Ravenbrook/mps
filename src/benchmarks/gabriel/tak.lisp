;;; TAK -- A vanilla version of the TAKeuchi function.

(in-package "USER")
(proclaim '(optimize (speed 1) (safety 1) (brevity 0)))

(defun tak (x y z)
  (declare (fixnum x y z))
  (if (not (< y x))
      z
      (tak (tak (the fixnum (1- x)) y z)
	   (tak (the fixnum (1- y)) z x)
	   (tak (the fixnum (1- z)) x y))))

(defun time-tak ()
  (time (tak 18 12 6)))

(proclaim '(optimize (speed 3) (safety 0)))

(defun fast-tak (x y z)
  (declare (fixnum x y z))
  (if (not (< y x))
      z
      (fast-tak (fast-tak (the fixnum (1- x)) y z)
		(fast-tak (the fixnum (1- y)) z x)
		(fast-tak (the fixnum (1- z)) x y))))

(defun time-fast-tak ()
  (time (fast-tak 18 12 6)))
