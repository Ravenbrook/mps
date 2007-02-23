(defun foo ()
  (if t
      (write-line "True.")
      (write-line "False.")))

(defstruct foo a b)

(defun lose (x)
  (let ((a (foo-a x))
	(b (if x (foo-b x) :none)))
    (list a b)))

(defun count-a (string)
  (do ((pos 0 (position #\a string :start (1+ pos)))
       (count 0 (1+ count)))
      ((null pos) count)
    (declare (fixnum pos))))

(defmacro t-and-f (var form)
  `(if ,var ,form ,form))

(defun foo (x)
  (t-and-f x (if x "True." "False.")))
