;;; TAK -- A vanilla version of the TAKeuchi function.

(eval-when (compile eval)
  (defmacro dbg-write (string)
    `(system:%primitive write-string ,string 0 ,(length string))))

(dbg-write "Form1
")
(dbg-write "Form2
")

(defun lisp::%initial-function ()
  (dbg-write "Starting...
")
  (dolist (fun lisp::*lisp-initialization-functions*)
    (funcall fun))

  (dbg-write "
done.
")
  (system:%primitive halt))

(dbg-write "Form1
")
