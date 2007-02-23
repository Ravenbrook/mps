;;; Some tests of unknown stack values.
;;;

(defun twisted ()
  (block nil
    (tagbody
      (return
       (multiple-value-prog1 (foo)
	 (when (bar)
	   (go UNWIND))))
      
     UNWIND
      (return
       (multiple-value-prog1 (baz)
	 (bletch))))))

(defun nested ()
  (multiple-value-call #'foo
    :foo
    (if (bar)
	(multiple-value-prog1 (grinch) (finch))
	(bletch)) 
    (quaz)))

(defun dead-generator ()
  (multiple-value-call #'foo
    (if (bar)
	(multiple-value-prog1 (grinch) (return-from dead-generator nil))
	(finch))
    (quaz)))

(defun deleted-use ()
  (multiple-value-call #'foo
    (multiple-value-prog1 (grinch) (return-from deleted-use nil))
    (quaz)))

(defun backward ()
  (tagbody
   FOO
    (multiple-value-call #'foo
      :foo
      (if (bar)
	  (baz)
	  (go FOO))
      (if (zab) 1 2)
      (quaz))))

(defun dead-nested ()
  (multiple-value-prog1 (foo)
    (block punt
      (multiple-value-call #'bar
	(multiple-value-prog1 (baz)
	  (when (gronk)
	    (return-from punt)))))))
