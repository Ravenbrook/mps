;;;
;;; Some stuff to test hairy lambda arguments...

(defun optional-test (arg1 arg2 &optional opt1 (opt2 arg1 2-p) (opt3 opt1))
  (list arg1 arg2 opt1 opt2 2-p opt3))

(defun supplied-test (arg1 arg2 &optional (opt1 nil 1-p) (opt2 (length opt1)))
  (list arg1 arg2 opt1 1-p opt2))

(defun rest-test (&rest rest)
  rest)

(defun key-test (&key key1 (key2 'foo) (key3 (length key1)) (key4 nil 4-p))
  (list key1 key2 key3 key4 4-p))

(defun aux-test (arg1 &aux aux1 (aux2) (aux3 arg1))
  (list arg1 aux1 aux2 aux3))
