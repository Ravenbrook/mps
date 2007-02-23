
;;; TRIANG -- Board game benchmark.  

(defvar board (make-array 16. :initial-element 1))
(setf (aref board 5) 0)

(defvar sequence (make-array 14. :initial-element 0))
(defvar a (make-array 37. :initial-contents '(1 2 4 3 5 6 1 3 6 2 5 4 11. 12. 13. 7 8. 4 4 7 11 8 12
						13. 6 10. 15. 9. 14. 13. 13. 14. 15. 9. 10. 6 6)))
(defvar b (make-array 37. :initial-contents  '(2 4 7 5 8. 9. 3 6 10. 5 9. 8. 12. 13. 14. 8. 9. 5
						 2 4 7 5 8. 9. 3 6 10. 5 9. 8. 12. 13. 14. 8. 9. 5 5)))
(defvar c (make-array 37. :initial-contents  '(4 7 11. 8. 12. 13. 6 10. 15. 9. 14. 13. 13. 14. 15. 9. 10. 6
						 1 2 4 3 5 6 1 3 6 2 5 4 11. 12. 13. 7 8. 4 4)))
(defvar answer)
(defvar final)

(proclaim '(type simple-vector board sequence a b c))

(defun last-position ()
  (do ((i 1 (1+ i)))
      ((= i 16.) 0)
    (declare (fixnum i))
    (if (= 1 (the fixnum (aref board i)))
	(return i))))

(defun try (i depth)
  (declare (fixnum i depth))
  (cond ((= depth 14) 
	 (let ((lp (last-position)))
	   (unless (member lp final)
	     (push lp final)))
	 (push (cdr (coerce (the simple-vector sequence) 'list))
	       answer) t)	; this is a hack to replace LISTARRAY
	((and (= 1 (the fixnum (aref board (aref a i))))
	      (= 1 (the fixnum (aref board (aref b i))))
	      (= 0 (the fixnum (aref board (aref c i)))))
	 (setf (aref board (aref a i)) 0)
	 (setf (aref board (aref b i)) 0)
	 (setf (aref board (aref c i)) 1)
	 (setf (aref sequence depth) i)
	 (do ((j 0 (1+ j))
	      (depth (1+ depth)))
	     ((or (= j 36.)
		  (try j depth)) ())
	   (declare (fixnum j depth)))
	 (setf (aref board (aref a i)) 1) 
	 (setf (aref board (aref b i)) 1)
	 (setf (aref board (aref c i)) 0) ())))

(defun triangle (i)
  (let ((answer ())
	(final ()))
    (try i 1)))
#|
(defun time-triangle ()
  (time (triangle 22)))
|#
