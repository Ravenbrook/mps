(eval-when (compile eval)
  (defmacro dbg-write (string)
    `(system:%primitive write-string ,string 0 ,(length string)))

  (defmacro receive-values (n form)
    (c::collect ((vars))
      (dotimes (i n)
	(vars (gensym)))
      `(multiple-value-bind ,(vars)
			    ,form
	 (dbg-write "vars =")
	 (system:%primitive print ',n)
	 ,@(mapcar #'(lambda (x)
		       `(system:%primitive print ,x))
		   (vars))))))

(defun n-values (n)
  (dbg-write "values =")
  (system:%primitive print n)
  (ecase n
    (0 (values))
    (1 (values 0))
    (2 (values 0 1))
    (3 (values 0 1 2))
    (4 (values 0 1 2 3))
    (5 (values 0 1 2 3 4))))


(defun lisp::%initial-function ()
  (dbg-write "Starting...
")

  (receive-values 3 (n-values 1))
  (receive-values 3 (n-values 2))
  (receive-values 5 (n-values 3))
  (receive-values 1 (n-values 5))

  (dbg-write "
done.
")
  (system:%primitive halt))
