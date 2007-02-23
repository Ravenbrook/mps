(defun foo (s)
  (do ((i 0 (position #\space s :start i))
       (count 0 (1+ count)))
      ((null i) count)
    (declare (fixnum i count))))

(defun mm (x l)
  (declare (inline member))
  (member x l :key #'(lambda (x) (car (fnord x)))))
