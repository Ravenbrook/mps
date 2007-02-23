#+x86
(in-package :x86)

#+x86
(progn
  (defparameter target-static-space-start    #+FreeBSD #x28F00000
		#-FreeBSD #x28000000)

  (defparameter nil-value (+ target-static-space-start #xB)))
