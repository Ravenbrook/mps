(in-package :cl-user)

(ext:without-package-locks
 (unintern 'lisp::simple-stream-error :lisp)
 (unintern 'conditions::simple-stream-error :conditions)
 (export 'kernel::simple-stream-error :kernel))
