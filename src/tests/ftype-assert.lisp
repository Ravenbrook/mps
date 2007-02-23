(proclaim '(ftype (function (fixnum fixnum) fixnum) m+))

(defun m+ (x y)
  (+ x y))


(proclaim '(ftype (function (fixnum cons) fixnum) error1))

(defun error1 (x y)
  (declare (fixnum x y))
  (+ x y))


(proclaim '(ftype (function (t &optional list
			       &key (b fixnum) (c bit)))
		  error2))

(defun error2 (a &key b) (list a b))

(defun error2 (a &optional y &key (b 13) (c 0.0))
  (declare (float c))
  (list a y b c))

(defun error2 (a &optional y &key (b 3) (c 0) (d 4)) (list a y b c d))
(defun error2 (a &optional y &key (b 3)) (list a y b))
(defun error2 (a y &key b c &allow-other-keys) (list a y b c))

(proclaim '(ftype (function (fixnum float &key (do-we-ever bit))) yow!))

(defun yow! (x y)
  (+ x y))

(defun error2 (a &optional y &key (b 13) (c 0.0))
  (list a y b c))

(defun defun-test (a &optional y &key b c)
  (list a y b c))

(defun defun-test (a &optional y &key b d)
  (list a y b d))

(defun defun-test (a &optional y &rest foo &key b d)
  (list foo a y b d))

(defun foo (a b)
  (declare (fixnum a))
  (list a b))

(defun foo (a)
  (declare (float a))
  (list a))

(defun gronk (&optional d)
  (list d))

(defun gronk (&optional d &rest c &key)
  (list d c))
