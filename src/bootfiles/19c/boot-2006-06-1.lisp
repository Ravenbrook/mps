;;;; Bootstrap file for doing a full build from a cross-compile for
;;;; double-double-float support.  This needs to be done using the
;;;; build from the cross-compile for double-double-float.  See
;;;; boot-2006-06-1-cross-dd-sparc.lisp for more info.
(in-package :cl-user)
(pushnew :double-double *features*)
(setf lisp::*enable-package-locked-errors* nil)

;;; This is a hack.  I need make-double-double-float,
;;; double-double-hi, double-double-lo, and %double-double-float to
;;; work.  For some reason, the cross-compile makes these known
;;; functions be infinite loops, but if we compile them here and setf
;;; fdefinition we get workingness.  
(defun %foo (x y)
  (declare (double-float x y))
  (kernel:%make-double-double-float x y))
(compile '%foo)



(defun foo-hi (x)
  (declare (type kernel:double-double-float x))
  (kernel:double-double-hi x))
(compile 'foo-hi)
(defun foo-lo (x)
  (declare (type kernel:double-double-float x))
  (kernel:double-double-lo x))
(compile 'foo-lo)
(defun %coerce-foo (x)
  (if (typep x 'kernel:double-double-float)
      x
      (kernel:make-double-double-float (float x 1d0) 0d0)))
(compile '%foo)

(setf (fdefinition 'kernel::%make-double-double-float) #'%foo)
(setf (fdefinition 'kernel::double-double-hi) #'foo-hi)
(setf (fdefinition 'kernel::double-double-lo) #'foo-lo)
(setf (fdefinition 'kernel::%double-double-float) #'%coerce-foo)

(in-package "KERNEL")
(defun make-double-double-float (hi lo)
  ;; Make sure the parts make sense for a double-double
  (if (or (float-infinity-p hi) (float-nan-p hi))
      (%make-double-double-float hi lo)
      (multiple-value-bind (s e)
	  (c::two-sum hi lo)
	(%make-double-double-float s e))))
