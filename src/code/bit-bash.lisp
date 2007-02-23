;;; -*- Log: code.log; Package: VM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/bit-bash.lisp,v 1.23 2004/07/14 20:52:38 cwang Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions to implement bit bashing.
;;;
;;; Written by William Lott.
;;;

(in-package "VM")



;;;; Constants and Types.


(eval-when (compile load eval)

(defconstant unit-bits vm:word-bits
  "The number of bits to process at a time.")

(defconstant max-bits (ash most-positive-fixnum -2)
  "The maximum number of bits that can be delt with during a single call.")


(deftype unit ()
  `(unsigned-byte ,unit-bits))

(deftype offset ()
  `(integer 0 ,max-bits))

(deftype bit-offset ()
  `(integer 0 (,unit-bits)))

(deftype bit-count ()
  `(integer 1 (,unit-bits)))

(deftype word-offset ()
  `(integer 0 (,(ceiling max-bits unit-bits))))


); eval-when



;;;; Support routines.

;;; A particular implementation must offer either VOPs to translate these, or
;;; deftransforms to convert them into something supported by the architecture.
;;;
(macrolet ((frob (name &rest args)
	     `(defun ,name ,args
		(,name ,@args))))
  (frob 32bit-logical-not x)
  (frob 32bit-logical-and x y)
  (frob 32bit-logical-or x y)
  (frob 32bit-logical-xor x y)
  (frob 32bit-logical-nor x y)
  (frob 32bit-logical-eqv x y)
  (frob 32bit-logical-nand x y)
  (frob 32bit-logical-andc1 x y)
  (frob 32bit-logical-andc2 x y)
  (frob 32bit-logical-orc1 x y)
  (frob 32bit-logical-orc2 x y))


(eval-when (compile eval)
  (defmacro byte-order-dispatch (big-endian little-endian)
    (ecase (c:backend-byte-order c:*target-backend*)
      (:big-endian big-endian)
      (:little-endian little-endian))))

(defun shift-towards-start (number count)
  "Shift NUMBER by COUNT bits, adding zero bits at the ``end'' and removing
  bits from the ``start.''  On big-endian machines this is a left-shift and
  on little-endian machines this is a right-shift.  Note: only the low 5/6 bits
  of count are significant."
  (declare (type unit number) (fixnum count))
  (let ((count (ldb (byte (1- (integer-length unit-bits)) 0) count)))
    (declare (type bit-offset count))
    (if (zerop count)
	number
	(byte-order-dispatch
	 (ash (ldb (byte (- unit-bits count) 0) number) count)
	 (ash number (- count))))))

(defun shift-towards-end (number count)
  "Shift NUMBER by COUNT bits, adding zero bits at the ``start'' and removing
  bits from the ``end.''  On big-endian machines this is a right-shift and
  on little-endian machines this is a left-shift."
  (declare (type unit number) (fixnum count))
  (let ((count (ldb (byte (1- (integer-length unit-bits)) 0) count)))
    (declare (type bit-offset count))
    (if (zerop count)
	number
	(byte-order-dispatch
	 (ash number (- count))
	 (ash (ldb (byte (- unit-bits count) 0) number) count)))))

(declaim (inline start-mask end-mask fix-sap-and-offset))
(defun start-mask (count)
  "Produce a mask that contains 1's for the COUNT ``start'' bits and 0's for
  the remaining ``end'' bits.  Only the lower 5 bits of COUNT are significant."
  (declare (fixnum count))
  (shift-towards-start (1- (ash 1 unit-bits)) (- count)))

(defun end-mask (count)
  "Produce a mask that contains 1's for the COUNT ``end'' bits and 0's for
  the remaining ``start'' bits.  Only the lower 5 bits of COUNT are
  significant."
  (declare (fixnum count))
  (shift-towards-end (1- (ash 1 unit-bits)) (- count)))

(defun fix-sap-and-offset (sap offset)
  "Align the SAP to a word boundry, and update the offset accordingly."
  (declare (type system-area-pointer sap)
	   (type index offset)
	   (values system-area-pointer index))
  (let ((address (sap-int sap)))
    (values (int-sap #-(or alpha amd64) (32bit-logical-andc2 address 3)
		     #+alpha (ash (ash address -2) 2)
		     #+amd64 (ash (ash address -3) 3))
	    (+ (* (logand address #+amd64 7 #-amd64 3) byte-bits) offset))))

(declaim (inline word-sap-ref %set-word-sap-ref))
;;;
(defun word-sap-ref (sap offset)
  (declare (type system-area-pointer sap)
	   (type index offset)
	   (values (unsigned-byte #+amd64 64 #-amd64 32))
	   (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  #+amd64 (sap-ref-64 sap (the index (ash offset 3)))
  #-amd64 (sap-ref-32 sap (the index (ash offset 2))))
;;;
(defun %set-word-sap-ref (sap offset value)
  (declare (type system-area-pointer sap)
	   (type index offset)
	   (type (unsigned-byte #+amd64 64 #-amd64 32) value)
	   (values (unsigned-byte #+amd64 64 #-amd64 32))
	   (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  (setf #+amd64 (sap-ref-64 sap (the index (ash offset 3)))
	#-amd64 (sap-ref-32 sap (the index (ash offset 2))) value))
;;;
(defsetf word-sap-ref %set-word-sap-ref)



;;;; DO-CONSTANT-BIT-BASH

(declaim (inline do-constant-bit-bash))
(defun do-constant-bit-bash (dst dst-offset length value dst-ref-fn dst-set-fn)
  "Fill DST with VALUE starting at DST-OFFSET and continuing for LENGTH bits."
  (declare (type offset dst-offset) (type unit value)
	   (type function dst-ref-fn dst-set-fn))
  (multiple-value-bind (dst-word-offset dst-bit-offset)
		       (floor dst-offset unit-bits)
    (declare (type word-offset dst-word-offset)
	     (type bit-offset dst-bit-offset))
    (multiple-value-bind (words final-bits)
			 (floor (+ dst-bit-offset length) unit-bits)
      (declare (type word-offset words) (type bit-offset final-bits))
      (if (zerop words)
	  (unless (zerop length)
	    (funcall dst-set-fn dst dst-word-offset
		     (if (= length unit-bits)
			 value
			 (let ((mask (shift-towards-end (start-mask length)
							dst-bit-offset)))
			   (declare (type unit mask))
			   (32bit-logical-or
			    (32bit-logical-and value mask)
			    (32bit-logical-andc2
			     (funcall dst-ref-fn dst dst-word-offset)
			     mask))))))
	  (let ((interior (floor (- length final-bits) unit-bits)))
	    (unless (zerop dst-bit-offset)
	      (let ((mask (end-mask (- dst-bit-offset))))
		(declare (type unit mask))
		(funcall dst-set-fn dst dst-word-offset
			 (32bit-logical-or
			  (32bit-logical-and value mask)
			  (32bit-logical-andc2
			   (funcall dst-ref-fn dst dst-word-offset)
			   mask))))
	      (incf dst-word-offset))
	    (dotimes (i interior)
	      (funcall dst-set-fn dst dst-word-offset value)
	      (incf dst-word-offset))
	    (unless (zerop final-bits)
	      (let ((mask (start-mask final-bits)))
		(declare (type unit mask))
		(funcall dst-set-fn dst dst-word-offset
			 (32bit-logical-or
			  (32bit-logical-and value mask)
			  (32bit-logical-andc2
			   (funcall dst-ref-fn dst dst-word-offset)
			   mask)))))))))
  (undefined-value))


;;;; DO-UNARY-BIT-BASH

(declaim (inline do-unary-bit-bash))
(defun do-unary-bit-bash (src src-offset dst dst-offset length
			      dst-ref-fn dst-set-fn src-ref-fn)
  (declare (type offset src-offset dst-offset length)
	   (type function dst-ref-fn dst-set-fn src-ref-fn))
  (multiple-value-bind (dst-word-offset dst-bit-offset)
		       (floor dst-offset unit-bits)
    (declare (type word-offset dst-word-offset)
	     (type bit-offset dst-bit-offset))
    (multiple-value-bind (src-word-offset src-bit-offset)
			 (floor src-offset unit-bits)
      (declare (type word-offset src-word-offset)
	       (type bit-offset src-bit-offset))
      (cond
       ((<= (+ dst-bit-offset length) unit-bits)
	;; We are only writing one word, so it doesn't matter what order
	;; we do it in.  But we might be reading from multiple words, so take
	;; care.
	(cond
	 ((zerop length)
	  ;; Actually, we arn't even writing one word.  This is real easy.
	  )
	 ((= length unit-bits)
	  ;; dst-bit-offset must be equal to zero, or we would be writing
	  ;; multiple words.  If src-bit-offset is also zero, then we
	  ;; just transfer the single word.  Otherwise we have to extract bits
	  ;; from two src words.
	  (funcall dst-set-fn dst dst-word-offset
		   (if (zerop src-bit-offset)
		       (funcall src-ref-fn src src-word-offset)
		       (32bit-logical-or
			(shift-towards-start
			 (funcall src-ref-fn src src-word-offset)
			 src-bit-offset)
			(shift-towards-end
			 (funcall src-ref-fn src (1+ src-word-offset))
			 (- src-bit-offset))))))
	 (t
	  ;; We are only writing some portion of the dst word, so we need to
	  ;; preserve the extra bits.  Also, we still don't know if we need
	  ;; one or two source words.
	  (let ((mask (shift-towards-end (start-mask length) dst-bit-offset))
		(orig (funcall dst-ref-fn dst dst-word-offset))
		(value
		 (if (> src-bit-offset dst-bit-offset)
		     ;; The source starts further into the word than does
		     ;; the dst, so the source could extend into the next
		     ;; word.  If it does, we have to merge the two words,
		     ;; and if not, we can just shift the first word.
		     (let ((src-bit-shift (- src-bit-offset dst-bit-offset)))
		       (if (> (+ src-bit-offset length) unit-bits)
			   (32bit-logical-or
			    (shift-towards-start
			     (funcall src-ref-fn src src-word-offset)
			     src-bit-shift)
			    (shift-towards-end
			     (funcall src-ref-fn src (1+ src-word-offset))
			     (- src-bit-shift)))
			   (shift-towards-start
			    (funcall src-ref-fn src src-word-offset)
			    src-bit-shift)))
		     ;; The dst starts further into the word than does the
		     ;; source, so we know the source can not extend into
		     ;; a second word (or else the dst would too, and we
		     ;; wouldn't be in this branch.
		     (shift-towards-end
		      (funcall src-ref-fn src src-word-offset)
		      (- dst-bit-offset src-bit-offset)))))
	    (declare (type unit mask orig value))
	    ;; Replace the dst word.
	    (funcall dst-set-fn dst dst-word-offset
		     (32bit-logical-or
		      (32bit-logical-and value mask)
		      (32bit-logical-andc2 orig mask)))))))
       ((= src-bit-offset dst-bit-offset)
	;; The source and dst are aligned, so we don't need to shift
	;; anything.  But we have to pick the direction of the loop
	;; in case the source and dst are really the same thing.
	(multiple-value-bind (words final-bits)
			     (floor (+ dst-bit-offset length) unit-bits)
	  (declare (type word-offset words) (type bit-offset final-bits))
	  (let ((interior (floor (- length final-bits) unit-bits)))
	    (declare (type word-offset interior))
	    (cond
	     ((<= dst-offset src-offset)
	      ;; We need to loop from left to right
	      (unless (zerop dst-bit-offset)
		;; We are only writing part of the first word, so mask off the
		;; bits we want to preserve.
		(let ((mask (end-mask (- dst-bit-offset)))
		      (orig (funcall dst-ref-fn dst dst-word-offset))
		      (value (funcall src-ref-fn src src-word-offset)))
		  (declare (type unit mask orig value))
		  (funcall dst-set-fn dst dst-word-offset
			   (32bit-logical-or (32bit-logical-and value mask)
					     (32bit-logical-andc2 orig mask))))
		(incf src-word-offset)
		(incf dst-word-offset))
	      ;; Just copy the interior words.
	      (dotimes (i interior)
		(funcall dst-set-fn dst dst-word-offset
			 (funcall src-ref-fn src src-word-offset))
		(incf src-word-offset)
		(incf dst-word-offset))
	      (unless (zerop final-bits)
		;; We are only writing part of the last word.
		(let ((mask (start-mask final-bits))
		      (orig (funcall dst-ref-fn dst dst-word-offset))
		      (value (funcall src-ref-fn src src-word-offset)))
		  (declare (type unit mask orig value))
		  (funcall dst-set-fn dst dst-word-offset
			   (32bit-logical-or
			    (32bit-logical-and value mask)
			    (32bit-logical-andc2 orig mask))))))
	     (t
	      ;; We need to loop from right to left.
	      (incf dst-word-offset words)
	      (incf src-word-offset words)
	      (unless (zerop final-bits)
		(let ((mask (start-mask final-bits))
		      (orig (funcall dst-ref-fn dst dst-word-offset))
		      (value (funcall src-ref-fn src src-word-offset)))
		  (declare (type unit mask orig value))
		  (funcall dst-set-fn dst dst-word-offset
			   (32bit-logical-or
			    (32bit-logical-and value mask)
			    (32bit-logical-andc2 orig mask)))))
	      (dotimes (i interior)
		(decf src-word-offset)
		(decf dst-word-offset)
		(funcall dst-set-fn dst dst-word-offset
			 (funcall src-ref-fn src src-word-offset)))
	      (unless (zerop dst-bit-offset)
		(decf src-word-offset)
		(decf dst-word-offset)
		(let ((mask (end-mask (- dst-bit-offset)))
		      (orig (funcall dst-ref-fn dst dst-word-offset))
		      (value (funcall src-ref-fn src src-word-offset)))
		  (declare (type unit mask orig value))
		  (funcall dst-set-fn dst dst-word-offset
			   (32bit-logical-or
			    (32bit-logical-and value mask)
			    (32bit-logical-andc2 orig mask))))))))))
       (t
	;; They aren't aligned.
	(multiple-value-bind (words final-bits)
			     (floor (+ dst-bit-offset length) unit-bits)
	  (declare (type word-offset words) (type bit-offset final-bits))
	  (let ((src-shift (mod (- src-bit-offset dst-bit-offset) unit-bits))
		(interior (floor (- length final-bits) unit-bits)))
	    (declare (type bit-offset src-shift)
		     (type word-offset interior))
	    (cond
	     ((<= dst-offset src-offset)
	      ;; We need to loop from left to right
	      (let ((prev 0)
		    (next (funcall src-ref-fn src src-word-offset)))
		(declare (type unit prev next))
		(flet ((get-next-src ()
			 (setf prev next)
			 (setf next (funcall src-ref-fn src
					     (incf src-word-offset)))))
		  (declare (inline get-next-src))
		  (unless (zerop dst-bit-offset)
		    (when (> src-bit-offset dst-bit-offset)
		      (get-next-src))
		    (let ((mask (end-mask (- dst-bit-offset)))
			  (orig (funcall dst-ref-fn dst dst-word-offset))
			  (value (32bit-logical-or
				  (shift-towards-start prev src-shift)
				  (shift-towards-end next (- src-shift)))))
		      (declare (type unit mask orig value))
		      (funcall dst-set-fn dst dst-word-offset
			       (32bit-logical-or
				(32bit-logical-and value mask)
				(32bit-logical-andc2 orig mask)))
		      (incf dst-word-offset)))
		  (dotimes (i interior)
		    (get-next-src)
		    (let ((value (32bit-logical-or
				  (shift-towards-end next (- src-shift))
				  (shift-towards-start prev src-shift))))
		      (declare (type unit value))
		      (funcall dst-set-fn dst dst-word-offset value)
		      (incf dst-word-offset)))
		  (unless (zerop final-bits)
		    (let ((value
			   (if (> (+ final-bits src-shift) unit-bits)
			       (progn
				 (get-next-src)
				 (32bit-logical-or
				  (shift-towards-end next (- src-shift))
				  (shift-towards-start prev src-shift)))
			       (shift-towards-start next src-shift)))
			  (mask (start-mask final-bits))
			  (orig (funcall dst-ref-fn dst dst-word-offset)))
		      (declare (type unit mask orig value))
		      (funcall dst-set-fn dst dst-word-offset
			       (32bit-logical-or
				(32bit-logical-and value mask)
				(32bit-logical-andc2 orig mask))))))))
	     (t
	      ;; We need to loop from right to left.
	      (incf dst-word-offset words)
	      (incf src-word-offset
		    (1- (ceiling (+ src-bit-offset length) unit-bits)))
	      (let ((next 0)
		    (prev (funcall src-ref-fn src src-word-offset)))
		(declare (type unit prev next))
		(flet ((get-next-src ()
		       (setf next prev)
		       (setf prev (funcall src-ref-fn src
					   (decf src-word-offset)))))
		  (declare (inline get-next-src))
		  (unless (zerop final-bits)
		    (when (> final-bits (- unit-bits src-shift))
		      (get-next-src))
		    (let ((value (32bit-logical-or
				  (shift-towards-end next (- src-shift))
				  (shift-towards-start prev src-shift)))
			  (mask (start-mask final-bits))
			  (orig (funcall dst-ref-fn dst dst-word-offset)))
		      (declare (type unit mask orig value))
		      (funcall dst-set-fn dst dst-word-offset
			       (32bit-logical-or
				(32bit-logical-and value mask)
				(32bit-logical-andc2 orig mask)))))
		  (decf dst-word-offset)
		  (dotimes (i interior)
		    (get-next-src)
		    (let ((value (32bit-logical-or
				  (shift-towards-end next (- src-shift))
				  (shift-towards-start prev src-shift))))
		      (declare (type unit value))
		      (funcall dst-set-fn dst dst-word-offset value)
		      (decf dst-word-offset)))
		  (unless (zerop dst-bit-offset)
		    (if (> src-bit-offset dst-bit-offset)
			(get-next-src)
			(setf next prev prev 0))
		    (let ((mask (end-mask (- dst-bit-offset)))
			  (orig (funcall dst-ref-fn dst dst-word-offset))
			  (value (32bit-logical-or
				  (shift-towards-start prev src-shift)
				  (shift-towards-end next (- src-shift)))))
		      (declare (type unit mask orig value))
		      (funcall dst-set-fn dst dst-word-offset
			       (32bit-logical-or
				(32bit-logical-and value mask)
				(32bit-logical-andc2 orig mask)))))))))))))))
  (undefined-value))


;;;; The actual bashers.

(defun bit-bash-fill (value dst dst-offset length)
  (declare (type unit value) (type offset dst-offset length))
  (locally
   (declare (optimize (speed 3) (safety 0)))
   (do-constant-bit-bash dst dst-offset length value
			 #'%raw-bits #'%set-raw-bits)))

(defun system-area-fill (value dst dst-offset length)
  (declare (type unit value) (type offset dst-offset length))
  (locally
   (declare (optimize (speed 3) (safety 0)))
   (multiple-value-bind (dst dst-offset)
			(fix-sap-and-offset dst dst-offset)
     (do-constant-bit-bash dst dst-offset length value
			   #'word-sap-ref #'%set-word-sap-ref))))

(defun bit-bash-copy (src src-offset dst dst-offset length)
  (declare (type offset src-offset dst-offset length))
  (locally
   (declare (optimize (speed 3) (safety 0))
	    (inline do-unary-bit-bash))
   (do-unary-bit-bash src src-offset dst dst-offset length
		      #'%raw-bits #'%set-raw-bits #'%raw-bits)))

(defun system-area-copy (src src-offset dst dst-offset length)
  (declare (type offset src-offset dst-offset length))
  (locally
   (declare (optimize (speed 3) (safety 0)))
   (multiple-value-bind (src src-offset)
			(fix-sap-and-offset src src-offset)
     (declare (type system-area-pointer src))
     (multiple-value-bind (dst dst-offset)
			  (fix-sap-and-offset dst dst-offset)
       (declare (type system-area-pointer dst))
       (do-unary-bit-bash src src-offset dst dst-offset length
			  #'word-sap-ref #'%set-word-sap-ref
			  #'word-sap-ref)))))

(defun copy-to-system-area (src src-offset dst dst-offset length)
  (declare (type offset src-offset dst-offset length))
  (locally
   (declare (optimize (speed 3) (safety 0)))
   (multiple-value-bind (dst dst-offset)
			(fix-sap-and-offset dst dst-offset)
     (do-unary-bit-bash src src-offset dst dst-offset length
			#'word-sap-ref #'%set-word-sap-ref #'%raw-bits))))

(defun copy-from-system-area (src src-offset dst dst-offset length)
  (declare (type offset src-offset dst-offset length))
  (locally
   (declare (optimize (speed 3) (safety 0)))
   (multiple-value-bind (src src-offset)
			(fix-sap-and-offset src src-offset)
     (do-unary-bit-bash src src-offset dst dst-offset length
			#'%raw-bits #'%set-raw-bits #'word-sap-ref))))
  
