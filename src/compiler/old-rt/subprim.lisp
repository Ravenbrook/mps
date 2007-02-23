;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Linkage information for standard miscops, both primitive and
;;; sub-primitive.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;;; Subprimitive miscops:

#| Inline versions exist, and we can't currently support multiple
implementations, since they are accessed through %primitive.

(define-miscop 16bit-system-ref (s i))
(define-miscop 16bit-system-set (s i v))
(define-miscop 8bit-system-ref (s i))
(define-miscop 8bit-system-set (s i v))
|#

(define-miscop active-call-frame ())
(define-miscop active-catch-frame ())
(define-miscop alloc-array (n))
(define-miscop alloc-bit-vector (n))
(define-miscop alloc-code (n))
(define-miscop alloc-function (n))
(define-miscop alloc-g-vector (n i))
(define-miscop alloc-i-vector (n a))
(define-miscop alloc-static-g-vector (len))
(define-miscop alloc-string (n))
(define-miscop bit-bash (v1 v2 res op) :results ())
(define-miscop break-return (stack) :results ())
(define-miscop call-foreign (cfcn args nargs))
#|Inline implementation...
(define-miscop check-<= (n l))
|#
(define-miscop collect-garbage () :results ())
(define-miscop current-binding-pointer ())
(define-miscop current-stack-pointer ())
(define-miscop dynamic-space-in-use ())
(define-miscop error0 (code) :results ())
(define-miscop error1 (code x) :results ())
(define-miscop error2 (code x y) :results ())
(define-miscop find-character (string start end character))
(define-miscop find-character-with-attribute (a b c d e))
(define-miscop float-long (x))
(define-miscop float-short (x))
(define-miscop get-allocation-space ())
(define-miscop get-space (x))
(define-miscop get-type (x))
(define-miscop get-vector-access-code (v))
#|Inline implementation...
(define-miscop get-vector-subtype (v))
|#
(define-miscop halt ())
#|Inline implementation...
(define-miscop header-length (x))
(define-miscop header-ref (x i))
(define-miscop header-set (x i v))
|#
(define-miscop logdpb (v s p n))
(define-miscop logldb (s p n))
(define-miscop long-float-ref (l i))
(define-miscop long-float-set (l i x))
(define-miscop lsh (n b))
(define-miscop make-complex (re im))
(define-miscop make-immediate-type (obj type))
(define-miscop make-ratio (num den))
(define-miscop negate (n))
(define-miscop newspace-bit ())
(define-miscop pointer-system-set (s i p))
(define-miscop purify () :results ())
(define-miscop putf (x y z))
(define-miscop read-binding-stack (b))
(define-miscop read-control-stack (f))
(define-miscop reset-c-stack (sp) :results ())
(define-miscop return-to-c (sp value) :results ())
(define-miscop sap-system-ref (s i))
(define-miscop save (x y z))
(define-miscop set-allocation-space (space) :results ())
(define-miscop set-c-procedure-pointer (s o v))
(define-miscop set-call-frame (p))
(define-miscop set-catch-frame (x))
(define-miscop set-vector-subtype (v x))
(define-miscop shrink-vector (v n))
#|Inline implementation...
(define-miscop signed-16bit-system-ref (s i))
|#
(define-miscop signed-32bit-system-ref (s i))
(define-miscop signed-32bit-system-set (s i v) :translate (setf sap-ref-32))
(define-miscop sxhash-simple-string (string))
(define-miscop sxhash-simple-substring (string length))
(define-miscop syscall0 (n) :results (res err))
(define-miscop syscall1 (n x) :results (res err))
(define-miscop syscall2 (n x y) :results (res err))
(define-miscop syscall3 (n x y z) :results (res err))
(define-miscop syscall4 (n x y z xx) :results (res err))
(define-miscop typed-vref (a v i))
(define-miscop typed-vset (a v i x))
(define-miscop unbind-to-here (x) :results ())
(define-miscop unix-fork (code-ptr) :results (res err))
(define-miscop unix-pipe () :results (res err))
(define-miscop unix-wait () :results (res err))
(define-miscop unix-write (fd buf offset len) :results (res err))
(define-miscop unsigned-32bit-system-ref (s i) :translate sap-ref-32)
(define-miscop vector-length (vec))
(define-miscop write-binding-stack (b v))
(define-miscop write-control-stack (f v))

(define-vop (syscall n-arg-two-value-miscop)
  (:variant 'clc::syscall))

(def-primitive-translator float-single (x)
  `(%primitive float-short ,x))


;;;; Primitive miscops:

(defknown ext:assq (t list) list (foldable flushable))
(defknown ext:memq (t list) list (foldable flushable))

(define-miscop abs (num) :translate abs)
(define-miscop alloc-symbol (name) :translate make-symbol)
(define-miscop ash (i n) :translate ash)
(define-miscop assq (key alist) :translate ext:assq)
(define-miscop atan (x) :translate atan)
(define-miscop cos (x) :translate cos)
(define-miscop decode-float (f) :results (frac exp sign) :translate decode-float)
(define-miscop exp (x) :translate exp)
(define-miscop gcd (x y) :translate gcd)
(define-miscop get-real-time () :translate get-internal-real-time)
(define-miscop get-run-time () :translate get-internal-run-time)
(define-miscop integer-length (i) :translate integer-length)
(define-miscop last (l) :translate last)
(define-miscop log (n) :translate log)
(define-miscop logcount (i) :translate logcount)
(define-miscop memq (item list) :translate ext:memq)
(define-miscop nthcdr (n l) :translate nthcdr)
(define-miscop put (sym prop val) :translate %put)
(define-miscop sap-int (sap) :translate sap-int)
(define-miscop scale-float (f exp) :translate scale-float)
(define-miscop sin (x) :translate sin)
(define-miscop sqrt (x) :translate sqrt)
(define-miscop tan (x) :translate tan)

(define-vop (list n-arg-miscop)
  (:variant 'list))

(define-vop (list* n-arg-miscop)
  (:variant 'list*))

;;; Continuation to allow %sp-byte-blt for now...
(defknown lisp::%sp-byte-blt (t index t index index) void ())
(define-miscop byte-blt (src src-start dst dst-start dst-end)
  :translate lisp::%sp-byte-blt)

