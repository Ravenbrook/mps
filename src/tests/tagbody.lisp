(in-package 'c)

(defun foo ()
  (tagbody
   A
    (when (frob) (go a))))
