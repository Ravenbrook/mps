(proclaim '(optimize (speed 3) (safety 0)))

(defun eff-note (x y z)
  (declare (fixnum x y z))
  (the fixnum (+ x y z)))

(defun eff-note (x y z)
  (declare (fixnum x y z))
  (the fixnum (+ (the fixnum (+ x y)) z)))

(proclaim '(optimize (speed 1) (safety 1) (c::brevity 0)))

(defun eff-note (x y z)
  (declare (type (unsigned-byte 18) x y z))
  (+ x y z))

(defun three-p (x)
  (= x 3))

(defun eol-pos (x)
  (position #\newline x))
