;;;;
;;;; This boot-file is needed only when cross-compiling from a lisp which
;;;; doesn't have the type-union related type-system changes to one which
;;;; has.  If one does a normal recompile to do the update, this isn't
;;;; needed, but if one wants to e.g. go from 18e directly to current HEAD,
;;;; a cross-compile is necessary, as is this boot file.
;;;;
;;;; This needs to be placed in the target:bootstrap.lisp file, while doing
;;;; the cross-compile, which is loaded after compiling the cross-compiler,
;;;; unlike the target:cross-bootstrap.lisp file.
;;;;

(in-package :C)

(defun make-canonical-union-type (type-list)
  (let ((members '())
	(misc-types '()))
    (dolist (type type-list)
      (if (member-type-p type)
	  (setf members (union members (member-type-members type)))
	  (push type misc-types)))
    #+long-float
    (when (null (set-difference '(-0l0 0l0) members))
      #-negative-zero-is-not-zero
      (push (specifier-type '(long-float 0l0 0l0)) misc-types)
      #+negative-zero-is-not-zero
      (push (specifier-type '(long-float -0l0 0l0)) misc-types)
      (setf members (set-difference members '(-0l0 0l0))))
    (when (null (set-difference '(-0d0 0d0) members))
      #-negative-zero-is-not-zero
      (push (specifier-type '(double-float 0d0 0d0)) misc-types)
      #+negative-zero-is-not-zero
      (push (specifier-type '(double-float -0d0 0d0)) misc-types)
      (setf members (set-difference members '(-0d0 0d0))))
    (when (null (set-difference '(-0f0 0f0) members))
      #-negative-zero-is-not-zero
      (push (specifier-type '(single-float 0f0 0f0)) misc-types)
      #+negative-zero-is-not-zero
      (push (specifier-type '(single-float -0f0 0f0)) misc-types)
      (setf members (set-difference members '(-0f0 0f0))))
    (cond ((null members)
	   (let ((res (first misc-types)))
	     (dolist (type (rest misc-types))
	       (setq res (type-union res type)))
	     res))
	  ((null misc-types)
	   (make-member-type :members members))
	  (t
	   (let ((res (first misc-types)))
	     (dolist (type (rest misc-types))
	       (setq res (type-union res type)))
	     (dolist (type members)
	       (setq res (type-union
			  res (make-member-type :members (list type)))))
	     res)))))
