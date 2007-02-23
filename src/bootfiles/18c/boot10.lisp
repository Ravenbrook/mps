;; bootstrap changes in the way FASL-FILE-VERSION is checked
;;
;; Given that FASL files are incompatible between releases, the
;; FASL-FILE-NUMBER was set to #x18d on all platforms, so that users
;; loading old compiled lisp files into their core will see a useful
;; error message, rather than unexplained lossage.
;;
;; This check is made when loading a FASL file into core, in the
;; function FOP-CODE-FORMAT. We work around the check so that it is
;; possible to build from 18c, without requiring a cross-compile.
;;
;; Additionally, the FASL file format was changed slightly: We use the new
;; FOP-LONG-CODE-FORMAT with code 157, because the version number is now
;; an uint32 instead of a single octet.  The old FOP-CODE-FORMAT with code
;; 57 is retained, in order to correctly read/detect old version numbers.

(in-package :cl)

;; Declare new variable defined in code/load.lisp, so that new-genesis
;; can reference it, before code/load is loaded.
(defvar *skip-fasl-file-version-check* nil)

;; Define nop FOP for the new FOP code of FOP-CODE-FORMAT
(define-fop (fop-long-code-format 157 :nope)
  (let ((implementation (read-arg 1))
	(version (read-arg 4)))
    (declare (ignore implementation version))
    (warn "FOP-LONG-CODE-FORMAT ignoring FASL file versions during rebuild")))

(in-package :c)

;; this is a DEFCONSTANT, so can't use SETQ
(setf (symbol-value 'byte-fasl-file-version) #x18d)

(setf (backend-fasl-file-version *target-backend*) #x18d)

;; Use the new format for newly created FASLs

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
