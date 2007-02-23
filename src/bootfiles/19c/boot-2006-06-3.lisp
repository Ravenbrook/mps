;; Get rid of LISP::SOCKET-ERROR.  It's really EXT:SOCKET-ERROR.
(in-package "LISP")

(eval-when (compile load eval)
(without-package-locks
  (unintern 'socket-error))
)
