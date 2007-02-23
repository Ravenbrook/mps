;; Implement float-accuracy compilation policy (intended for x86).
;; This means that on x86 cmucl will run with double-float precision
;; (53-bit) for all floating-point operations, unless float-accuracy
;; is not 3, in which case extended-float (64-bit) precision will be
;; used.
;;
;; By changing the default to 53-bit, double-float-epsilon really is
;; double-float epsilon.

(in-package :extensions)
(export '(float-accuracy))

(in-package :c)

(handler-bind ((error (lambda (c)
			(declare (ignore c))
			(invoke-restart 'kernel::continue))))
  (defconstant policy-parameter-slots
    '((speed . cookie-speed) (space . cookie-space) (safety . cookie-safety)
      (cspeed . cookie-cspeed) (brevity . cookie-brevity)
      (debug . cookie-debug) (float-accuracy . cookie-float-accuracy))))
