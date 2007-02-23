;;;
;;; Bootstrap file for adding support for source location recording
;;; from Helmut Eller.
;;;

(in-package "C")

(setf lisp::*enable-package-locked-errors* nil)

(defstruct (form-numbers)
  ;; The tlf-number and form-number encoded in a fixnum.
  (form-numbers (required-argument) :type fixnum))

(defstruct (file-source-location
	     (:include form-numbers)
	     (:make-load-form-fun :just-dump-it-normally)
	     (:pure t))
  (pathname (required-argument) :type simple-string))

(defstruct (stream-source-location
	     (:include form-numbers)
	     (:make-load-form-fun :just-dump-it-normally)
	     (:pure t))
  user-info)

(defun encode-form-numbers (tlf-number form-number)
  "Return the TLF-NUMBER and FORM-NUMBER encoded as fixnum."
  (declare (type (unsigned-byte 14) tlf-number form-number))
  (logior tlf-number (ash form-number 14)))

(defun namestring-for-debug-source (file-info)
  "Extract the namestring from FILE-INFO for the DEBUG-SOURCE.  
Return FILE-INFO's untruename (e.g., target:foo) if it is absolute;
otherwise the truename."
  (let* ((untruename (file-info-untruename file-info))
	 (dir (pathname-directory untruename)))
    (namestring (if (and dir (eq (first dir) :absolute))
		    untruename
		    (file-info-name file-info)))))

(defun source-location ()
  "Return a source-location for the call site."
  nil)
