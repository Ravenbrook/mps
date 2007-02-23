(unless (find-package "OPS") (make-package "OPS"))

(compile-file 
 '("library:contrib/ops/ops-util.lisp"
   "library:contrib/ops/ops-compile.lisp"
   "library:contrib/ops/ops-rhs.lisp"
   "library:contrib/ops/ops-match.lisp"
   "library:contrib/ops/ops-main.lisp"
   "library:contrib/ops/ops-backup.lisp"
   "library:contrib/ops/ops-io.lisp"
   "library:contrib/ops/ops.lisp")
 :output-file "library:contrib/ops/ops.fasl")
