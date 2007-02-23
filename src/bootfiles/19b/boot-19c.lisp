;;;;
;;;; Boot file for changing the fasl file version numbers to 19c.
;;;;

(in-package :c)

(setf lisp::*enable-package-locked-errors* nil)

;;;
;;; Note that BYTE-FASL-FILE-VERSION is a constant.
;;;
(setf (symbol-value 'byte-fasl-file-version)       #x19c)
(setf (backend-fasl-file-version *target-backend*) #x19c)

;;;
;;; Don't check fasl versions in the compiling Lisp because we'll
;;; load files compiled with the new version numbers.
;;;
(setq lisp::*skip-fasl-file-version-check* t)

;;;
;;; This is here because BYTE-FASL-FILE-VERSION is constant-folded in
;;; OPEN-FASL-FILE.  To make the new version number take effect, we
;;; have to redefine the function.
;;;
(defun open-fasl-file (name where &optional byte-p)
  (declare (type pathname name))
  (let* ((stream (open name :direction :output
		       :if-exists :new-version
		       :element-type '(unsigned-byte 8)))
	 (res (make-fasl-file :stream stream)))
    (multiple-value-bind
	(version f-vers f-imp)
	(if byte-p
	    (values "Byte code"
		    byte-fasl-file-version
		    (backend-byte-fasl-file-implementation *backend*))
	    (values (backend-version *backend*)
		    (backend-fasl-file-version *backend*)
		    (backend-fasl-file-implementation *backend*)))
      (format stream
	      "FASL FILE output from ~A.~@
	       Compiled ~A on ~A~@
	       Compiler ~A, Lisp ~A~@
	       Targeted for ~A, FASL version ~X~%"
	      where
	      (ext:format-universal-time nil (get-universal-time))
	      (machine-instance) compiler-version
	      (lisp-implementation-version)
	      version f-vers)
      ;;
      ;; Terminate header.
      (dump-byte 255 res)
      ;;
      ;; Specify code format.
      (dump-fop 'lisp::fop-long-code-format res)
      (dump-byte f-imp res)
      (dump-unsigned-32 f-vers res))
    res))

;;;; end of boot-19c.lisp
