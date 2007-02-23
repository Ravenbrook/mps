;;;
;;; Bootstrap file for removing the function definition of
;;; FIXNUM, which must not exist according to CLtS.
;;;

(in-package :vm)

(export 'fixnumize)

(when (fboundp 'fixnum)
  (setf (fdefinition 'fixnumize) (fdefinition 'fixnum)))
      
;;; End of file.