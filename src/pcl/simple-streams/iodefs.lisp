;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/pcl/simple-streams/iodefs.lisp,v 1.3 2003/06/26 13:27:43 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Macros needed by the simple-streams implementation

(in-package "STREAM")

(defmacro def-stream-class (name superclasses slots &rest options)
  `(defclass ,name ,superclasses ,slots ,@options))


(defconstant +flag-bits+ '(:simple		; instance is valid
			   :input :output	; direction
			   :dual :string	; type of stream
			   :eof			; latched EOF
			   :dirty		; output buffer needs write
			   :interactive))	; interactive stream

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %flags (flags)
    (loop for flag in flags
	  as pos = (position flag +flag-bits+)
	when (eq flag :gray) do
	  (error "Gray streams not supported.")
	if pos
	  sum (ash 1 pos) into bits
	else
	  collect flag into unused
      finally (when unused
		(warn "Invalid stream instance flag~P: ~{~S~^, ~}"
		      (length unused) unused))
	      (return bits))))

(defmacro with-stream-class ((class-name &optional stream) &body body)
  (if stream
    (let ((stm (gensym "STREAM"))
	  (slt (gensym "SV")))
      `(let* ((,stm ,stream)
	      (,slt (kernel:%instance-ref ,stm 1)))
	 (declare (type ,class-name ,stm)
		  (type simple-vector ,slt)
		  (ignorable ,slt))
	 (macrolet ((sm (slot-name stream)
		      (declare (ignore stream))
		      #-count-sm
		      `(slot-value ,',stm ',slot-name)
		      #+count-sm
		      `(%sm ',slot-name ,',stm))
		    (add-stream-instance-flags (stream &rest flags)
		      (declare (ignore stream))
		      `(setf (sm %flags ,',stm) (logior (sm %flags ,',stm)
							,(%flags flags))))
		    (remove-stream-instance-flags (stream &rest flags)
		      (declare (ignore stream))
		      `(setf (sm %flags ,',stm) (logandc2 (sm %flags ,',stm)
							  ,(%flags flags))))
		    (any-stream-instance-flags (stream &rest flags)
		      (declare (ignore stream))
		      `(not (zerop (logand (sm %flags ,',stm)
					   ,(%flags flags))))))
	   ,@body)))
    `(macrolet ((sm (slot-name stream)
		  #-count-sm
		  `(slot-value ,stream ',slot-name)
		  #+count-sm
		  `(%sm ',slot-name ,stream)))
       ,@body)))

(defmacro sm (slot-name stream)
  "Access the named slot in Stream."
  (warn "Using ~S macro outside ~S." 'sm 'with-stream-class)
  `(slot-value ,stream ',slot-name))

(defmacro funcall-stm-handler (slot-name stream &rest args)
  "Call the strategy function named by Slot-Name on Stream."
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (funcall (sm ,slot-name ,s) ,s ,@args))))

(defmacro funcall-stm-handler-2 (slot-name arg1 stream &rest args)
  "Call the strategy function named by Slot-Name on Stream."
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (funcall (sm ,slot-name ,s) ,arg1 ,s ,@args))))

(defmacro add-stream-instance-flags (stream &rest flags)
  "Set the given Flags in Stream."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
	 (add-stream-instance-flags ,s ,@flags)))))

(defmacro remove-stream-instance-flags (stream &rest flags)
  "Clear the given Flags in Stream."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
	 (remove-stream-instance-flags ,s ,@flags)))))

(defmacro any-stream-instance-flags (stream &rest flags)
  "Determine whether any one of the Flags is set in Stream."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
	 (any-stream-instance-flags ,s ,@flags)))))

(defmacro simple-stream-dispatch (stream single dual string)
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
	 (let ((%flags (sm %flags ,s)))
	   (cond ((zerop (logand %flags ,(%flags '(:string :dual))))
		  ,single)
		 ((zerop (logand %flags ,(%flags '(:string))))
		  ,dual)
		 (t
		  ,string)))))))

(defmacro simple-stream-dispatch-2 (stream non-string string)
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
	 (let ((%flags (sm %flags ,s)))
	   (cond ((zerop (logand %flags ,(%flags '(:string))))
		  ,non-string)
		 (t
		  ,string)))))))

(provide :iodefs)


;;;; Franz source-compatibility

(defpackage "EXCL"
  (:use "STREAM")
  (:import-from "STREAM"
        BUFFER BUFFPOS BUFFER-PTR
        OUT-BUFFER MAX-OUT-POS
        INPUT-HANDLE OUTPUT-HANDLE
        MELDED-STREAM
	CONTROL-IN CONTROL-OUT
        J-READ-CHARS))
