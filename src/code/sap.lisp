;;; -*- Package: SYSTEM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/sap.lisp,v 1.18 2003/10/15 13:16:37 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file holds the support for System Area Pointers (saps).
;;;
(in-package "SYSTEM")

(export '(system-area-pointer sap-ref-8 sap-ref-16 sap-ref-32 sap-ref-sap
	  signed-sap-ref-8 signed-sap-ref-16 signed-sap-ref-32
	  sap-ref-64 signed-sap-ref-64
	  sap+ sap- sap< sap<= sap= sap>= sap>
	  allocate-system-memory allocate-system-memory-at
	  reallocate-system-memory deallocate-system-memory))

(in-package "KERNEL")
(export '(%set-sap-ref-sap %set-sap-ref-single %set-sap-ref-double
	  %set-sap-ref-8 %set-signed-sap-ref-8
	  %set-sap-ref-16 %set-signed-sap-ref-16
	  %set-sap-ref-32 %set-signed-sap-ref-32
	  %set-sap-ref-64 %set-signed-sap-ref-64))
(in-package "SYSTEM")

(use-package "KERNEL")



;;;; Primitive SAP operations.

(defun sap< (x y)
  "Return T iff the SAP X points to a smaller address then the SAP Y."
  (declare (type system-area-pointer x y))
  (sap< x y))

(defun sap<= (x y)
  "Return T iff the SAP X points to a smaller or the same address as
   the SAP Y."
  (declare (type system-area-pointer x y))
  (sap<= x y))

(defun sap= (x y)
  "Return T iff the SAP X points to the same address as the SAP Y."
  (declare (type system-area-pointer x y))
  (sap= x y))

(defun sap>= (x y)
  "Return T iff the SAP X points to a larger or the same address as
   the SAP Y."
  (declare (type system-area-pointer x y))
  (sap>= x y))

(defun sap> (x y)
  "Return T iff the SAP X points to a larger address then the SAP Y."
  (declare (type system-area-pointer x y))
  (sap> x y))

(defun sap+ (sap offset)
  "Return a new sap OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.vm:word-bits #+alpha 64) offset))
  (sap+ sap offset))

(defun sap- (sap1 sap2)
  "Return the byte offset between SAP1 and SAP2."
  (declare (type system-area-pointer sap1 sap2))
  (sap- sap1 sap2))

(defun sap-int (sap)
  "Converts a System Area Pointer into an integer."
  (declare (type system-area-pointer sap))
  (sap-int sap))

(defun int-sap (int)
  "Converts an integer into a System Area Pointer."
  (declare (type (unsigned-byte #-alpha #.vm:word-bits #+alpha 64) int))
  (int-sap int))

(defun sap-ref-8 (sap offset)
  "Returns the 8-bit byte at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.vm:word-bits #+alpha 64) offset))
  (sap-ref-8 sap offset))

(defun sap-ref-16 (sap offset)
  "Returns the 16-bit word at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.(1- vm:word-bits) #+alpha 63) offset))
  (sap-ref-16 sap offset))

(defun sap-ref-32 (sap offset)
  "Returns the 32-bit dualword at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-32 sap offset))

(defun sap-ref-64 (sap offset)
  "Returns the 64-bit quadword at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-64 sap offset))

(defun sap-ref-sap (sap offset)
  "Returns the 32-bit system-area-pointer at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.vm:word-bits #+alpha 64) offset))
  (sap-ref-sap sap offset))

(defun sap-ref-single (sap offset)
  "Returns the 32-bit single-float at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-single sap offset))

(defun sap-ref-double (sap offset)
  "Returns the 64-bit double-float at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-double sap offset))

#+(or x86 long-float)
(defun sap-ref-long (sap offset)
  "Returns the long-float at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-long sap offset))

(defun signed-sap-ref-8 (sap offset)
  "Returns the signed 8-bit byte at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.vm:word-bits #+alpha 64) offset))
  (signed-sap-ref-8 sap offset))

(defun signed-sap-ref-16 (sap offset)
  "Returns the signed 16-bit word at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.(1- vm:word-bits) #+alpha 63) offset))
  (signed-sap-ref-16 sap offset))

(defun signed-sap-ref-32 (sap offset)
  "Returns the signed 32-bit dualword at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-32 sap offset))

(defun signed-sap-ref-64 (sap offset)
  "Returns the signed 64-bit quadword at OFFSET bytes from SAP."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-64 sap offset))

(defun %set-sap-ref-8 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.vm:word-bits #+alpha 64) offset)
	   (type (unsigned-byte 8) new-value))
  (setf (sap-ref-8 sap offset) new-value))

(defun %set-sap-ref-16 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.(1- vm:word-bits) #+alpha 63) offset)
	   (type (unsigned-byte 16) new-value))
  (setf (sap-ref-16 sap offset) new-value))

(defun %set-sap-ref-32 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 32) new-value))
  (setf (sap-ref-32 sap offset) new-value))

(defun %set-sap-ref-64 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 64) new-value))
  (setf (sap-ref-64 sap offset) new-value))

(defun %set-signed-sap-ref-8 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.vm:word-bits #+alpha 64) offset)
	   (type (signed-byte 8) new-value))
  (setf (signed-sap-ref-8 sap offset) new-value))

(defun %set-signed-sap-ref-16 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (type (signed-byte #-alpha #.(1- vm:word-bits) #+alpha 63) offset)
	   (type (signed-byte 16) new-value))
  (setf (signed-sap-ref-16 sap offset) new-value))

(defun %set-signed-sap-ref-32 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 32) new-value))
  (setf (signed-sap-ref-32 sap offset) new-value))

(defun %set-signed-sap-ref-64 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 64) new-value))
  (setf (signed-sap-ref-64 sap offset) new-value))

(defun %set-sap-ref-sap (sap offset new-value)
  (declare (type system-area-pointer sap new-value)
	   (type (signed-byte #-alpha #.vm:word-bits #+alpha 64) offset))
  (setf (sap-ref-sap sap offset) new-value))

(defun %set-sap-ref-single (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type single-float new-value))
  (setf (sap-ref-single sap offset) new-value))

(defun %set-sap-ref-double (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type double-float new-value))
  (setf (sap-ref-double sap offset) new-value))

#+long-float
(defun %set-sap-ref-long (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type long-float new-value))
  (setf (sap-ref-long sap offset) new-value))



;;;; System memory allocation.

(alien:def-alien-routine ("os_allocate" allocate-system-memory)
			 system-area-pointer
  (bytes c-call:unsigned-long))

(alien:def-alien-routine ("os_allocate_at" allocate-system-memory-at)
			 system-area-pointer
  (address system-area-pointer)
  (bytes c-call:unsigned-long))

(alien:def-alien-routine ("os_reallocate" reallocate-system-memory)
			 system-area-pointer
  (old system-area-pointer)
  (old-size c-call:unsigned-long)
  (new-size c-call:unsigned-long))

(alien:def-alien-routine ("os_deallocate" deallocate-system-memory)
			 c-call:void
  (addr system-area-pointer)
  (bytes c-call:unsigned-long))
