;;; Add a new superclass, required by the ANSI Spec, to fd-stream
;;; without causing a blowup in genesis.

(in-package "LISP")

(defstruct (file-stream
	     (:include lisp-stream)
	     (:constructor nil)
	     (:copier nil)))

(handler-bind ((error #'(lambda (c)
			  (declare (ignore c))
			  (invoke-restart 'kernel::clobber-it))))
  (defstruct (fd-stream
	       (:print-function %print-fd-stream)
	       (:constructor %make-fd-stream)
	       (:include file-stream
			 (misc #'fd-stream-misc-routine)))

    (name nil)				; The name of this stream
    (file nil)				; The file this stream is for
    ;;
    ;; The backup file namestring for the old file, for :if-exists :rename or
    ;; :rename-and-delete.
    (original nil :type (or simple-string null))
    (delete-original nil)		; for :if-exists :rename-and-delete
    ;;
    ;; Number of bytes per element.
    (element-size 1 :type index)
    (element-type 'base-char)		; The type of element being transfered.
    (fd -1 :type fixnum)		; The file descriptor
    ;;
    ;; Controls when the output buffer is flushed.
    (buffering :full :type (member :full :line :none))
    ;;
    ;; Character position if known.
    (char-pos nil :type (or index null))
    ;;
    ;; T if input is waiting on FD.  :EOF if we hit EOF.
    (listen nil :type (member nil t :eof))
    ;;
    ;; The input buffer.
    (unread nil)
    (ibuf-sap nil :type (or system-area-pointer null))
    (ibuf-length nil :type (or index null))
    (ibuf-head 0 :type index)
    (ibuf-tail 0 :type index)

    ;; The output buffer.
    (obuf-sap nil :type (or system-area-pointer null))
    (obuf-length nil :type (or index null))
    (obuf-tail 0 :type index)

    ;; Output flushed, but not written due to non-blocking io.
    (output-later nil)
    (handler nil)
    ;;
    ;; Timeout specified for this stream, or NIL if none.
    (timeout nil :type (or index null))
    ;;
    ;; Pathname of the file this stream is opened to (returned by PATHNAME.)
    (pathname nil :type (or pathname null))))


