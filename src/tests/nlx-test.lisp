(in-package 'c)

(eval-when (compile eval)
  (defmacro dbg-write (string)
    `(system:%primitive write-string ,string 0 ,(length string)))
  
  (defmacro receive-values (n form)
    (collect ((vars))
      (dotimes (i n)
	(vars (gensym)))
      `(multiple-value-bind ,(vars)
			    ,form
	 (dbg-write "vars =")
	 (system:%primitive print ',n)
	 ,@(mapcar #'(lambda (x)
		       `(system:%primitive print ,x))
		   (vars)))))
  
  (defmacro receive-any-values (form)
    (once-only ((n-res `(multiple-value-list ,form)))
      `(progn
	 (dbg-write "result =")
	 (system:%primitive print ,n-res)))))


(proclaim '(special *foo*))

(defun nlx-test (n)
  (let ((*foo* 'nlx-test))
    (multiple-value-prog1
	(block foo
	  (nlx-test-aux
	   #'(lambda ()
	       (dbg-write "In escape: ")
	       (system:%primitive print *foo*)
	       (return-from foo (n-values n)))))
      (dbg-write "After unwind: ")
      (system:%primitive print *foo*))))

(defun nlx-test-aux (fun)
  (let ((*foo* 'nlx-test-aux))
    (unwind-protect 
	(funcall fun)
      (dbg-write "Unwind: ")
      (%primitive print *foo*)
      (dbg-write "
"))))


(defun n-values (n)
  (declare (type (integer 0 5) n))
  (dbg-write "values =")
  (system:%primitive print n)
  (ecase n
    (0 (values))
    (1 (values 0))
    (2 (values 0 1))
    (3 (values 0 1 2))
    (4 (values 0 1 2 3))
    (5 (values 0 1 2 3 4))))


(defun list (&rest x)
  x)


(defun lisp::%initial-function ()
  (dbg-write "Starting...
")
  (let ((*foo* 'lisp::%initial-function))
    (dbg-write "Before NLX-Test: ")
    (system:%primitive print *foo*)
    (receive-any-values (nlx-test 5))
    (dbg-write "After NLX-Test: ")
    (system:%primitive print *foo*))


  (dbg-write "
done.
")
  (system:%primitive halt))
