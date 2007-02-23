;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the RT definitions for array operations.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(define-miscop aref1 (a i) :translate aref)
(define-miscop caref2 (a i j) :translate aref)
(define-miscop caref3 (a i j k) :translate aref)
(define-miscop aset1 (a i val) :translate %aset)
(define-miscop caset2 (a i j val) :translate %aset)
(define-miscop caset3 (a i j k val) :translate %aset)
(define-miscop svref (v i) :translate aref
  :arg-types (simple-vector t))
(define-miscop svset (v i val) :translate %aset
  :arg-types (simple-vector t t))
(define-miscop schar (v i) :translate aref
  :arg-types (simple-string t))
(define-miscop scharset (v i val) :translate %aset
  :arg-types (simple-string t t))
(define-miscop sbit (v i) :translate aref
  :arg-types (simple-bit-vector t))
(define-miscop sbitset (v i val) :translate %aset
  :arg-types (simple-bit-vector t t))
(define-miscop g-vector-length (v) :translate length
  :arg-types (simple-vector))
(define-miscop simple-string-length (s) :translate length
  :arg-types (simple-string))
(define-miscop simple-bit-vector-length (b) :translate length
  :arg-types (simple-bit-vector))
(define-miscop length (s) :translate length :cost 50)

(define-vop (fast-length/simple-string)
  (:args (vec :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:translate length)
  (:arg-types simple-string)
  (:policy :fast-safe)
  (:generator 6
    (loadw res vec (/ clc::string-header-entries 4))
    (inst niuo res res clc::i-vector-entries-mask-16)))

(define-vop (fast-schar byte-index-ref)
  (:results (value :scs (string-char-reg)))
  (:variant clc::i-vector-header-size)
  (:translate aref)
  (:policy :fast)
  (:arg-types simple-string *)
  (:result-types string-char))

(define-vop (fast-scharset byte-index-set)
  (:args (object :scs (descriptor-reg))
	 (index :scs (any-reg descriptor-reg short-immediate
			      unsigned-immediate))
	 (value :scs (string-char-reg)))
  (:results (result :scs (string-char-reg)))
  (:variant clc::i-vector-header-size)
  (:translate %aset)
  (:policy :fast)
  (:arg-types simple-string * string-char)
  (:result-types string-char))

(define-vop (header-length)
  (:args (vec :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadw res vec (/ clc::g-vector-header-words 4))
    (inst dec res clc::g-vector-header-size-in-words)
    (inst niuo res res clc::g-vector-words-mask-16)))

(define-vop (fast-length/simple-vector header-length)
  (:translate length)
  (:policy :fast-safe)
  (:arg-types simple-vector))

(define-vop (get-vector-subtype)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 6
    (loadc res x clc::vector-subtype-byte)
    (inst nilz res res clc::right-shifted-subtype-mask-16)))
	 
(define-vop (header-ref word-index-ref)
  (:variant clc::g-vector-header-size-in-words))

(define-vop (fast-svref header-ref)
  (:translate aref)
  (:arg-types simple-vector *)
  (:policy :fast))

(define-vop (header-set word-index-set)
  (:variant clc::g-vector-header-size-in-words))

(define-vop (fast-svset header-set)
  (:translate %aset)
  (:arg-types simple-vector * *)
  (:policy :fast))
