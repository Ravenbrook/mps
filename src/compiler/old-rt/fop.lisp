;;;
;;; Define new FOPs for bootstrapping...

(in-package 'lisp)

(eval-when (compile load eval)

  (clone-fop (fop-alter-code 140) (fop-byte-alter-code 141)
   )

  (define-fop (fop-function-entry 142)
   )

); Eval-When (Compile load Eval)
