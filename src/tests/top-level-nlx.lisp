(let ((count 0))
  (block crock
    (defun test (x)
      (if x
	  (return-from crock (values 2 2))
	  (print 'yow!)))
    (test nil)
    (defun increment ()
      (incf count))
    (unwind-protect
	(test t)
      (print "Unwound."))))

(catch 'goo
  (unwind-protect
      (throw 'goo nil)
    (print "Unwound.")))

(collect ((stuff))
  (dotimes (i 10)
    (stuff #'(lambda (x)
	       (when x (return i)))))
  (defparameter *stuff* (stuff)))
