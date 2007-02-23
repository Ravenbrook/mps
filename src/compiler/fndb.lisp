;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/fndb.lisp,v 1.135 2005/11/09 19:08:06 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file defines all the standard functions to be known functions.
;;; Each function has type and side-effect information, and may also have IR1
;;; optimizers.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

(in-package "LISP")
(import '(
	  %aset
	  %bitset
	  %charset
	  %primitive
	  %put
	  %puthash
	  %rplaca
	  %rplacd
	  %sbitset
	  %scharset
	  %set-fdefinition
	  %set-fill-pointer
	  %set-row-major-aref
	  %setelt
	  %setnth
	  %standard-char-p
	  %svset
	  %typep
	  array-header-p
	  base-char-p
	  double-float-p
	  long-float-p
	  short-float-p
	  single-float-p
	  string<*
	  string>*
	  string<=*
	  string>=*
	  string=*
	  string/=*
	  %sp-string-compare
	  )
	"C")

(in-package "KERNEL")

(export '(%caller-frame-and-pc %with-array-data))

(in-package "C")

(deftype lexenv-or-null () '(or null lexical-environment))


;;;; Information for known functions:

(defknown coerce (t type-specifier) t
	  (movable foldable)			  ; Is defined to signal errors. 
	  ;; :derive-type (result-type-specifier-nth-arg 2)
	  ;; This is wrong.  (coerce 1 'complex) returns 1, not COMPLEX.
  )

(defknown type-of (t) t (foldable flushable))

;;; Can be affected by type definitions...
(defknown (upgraded-complex-part-type upgraded-array-element-type)
	  (type-specifier &optional lexenv-or-null)
          type-specifier
	  (flushable))


;;;; In the "Predicates" chapter:

(defknown typep (t type-specifier &optional lexenv-or-null)
          boolean (foldable flushable))
(defknown subtypep (type-specifier type-specifier &optional lexenv-or-null)
          (values boolean boolean)
	  (foldable flushable))

(defknown (null symbolp atom consp listp numberp integerp rationalp floatp
		complexp characterp stringp bit-vector-p vectorp
		simple-vector-p simple-string-p simple-bit-vector-p arrayp
		packagep functionp compiled-function-p not)
  (t) boolean (movable foldable flushable))


(defknown (eq eql) (t t) boolean (movable foldable flushable))
(defknown (equal equalp) (t t) boolean (foldable flushable recursive))


;;;; Classes:

(defknown kernel::find-class (t &optional t lexenv-or-null)
  (or kernel::class null) ())
(defknown kernel::class-of (t) kernel::class (flushable))
(defknown layout-of (t) layout (flushable))
(defknown copy-structure (structure-object) structure-object
  (flushable unsafe))


;;;; In the "Control Structure" chapter:

;;; Not flushable, since required to signal an error if unbound.
(defknown symbol-value (symbol) t ())
(defknown symbol-function (symbol) function ())

(defknown boundp (symbol) boolean (flushable))
(defknown fboundp ((or symbol cons)) boolean (flushable explicit-check))
(defknown special-operator-p (symbol) t (movable foldable flushable)) ; They never change...
(defknown set (symbol t) t (unsafe)
  :derive-type #'result-type-last-arg)
(defknown fdefinition ((or symbol cons)) function (unsafe explicit-check))
(defknown %set-fdefinition ((or symbol cons) function) function
  (unsafe explicit-check))
(defknown makunbound (symbol) symbol)
(defknown fmakunbound ((or symbol cons)) (or symbol cons)
  (unsafe explicit-check))
(defknown get-setf-expansion ((or list symbol) &optional lexenv-or-null)
 (values list list list form form)
 (flushable))

(defknown apply (callable t &rest t) *) ; ### Last arg must be List...
(defknown funcall (callable &rest t) *)

(defknown (mapcar maplist mapcan mapcon) (callable list &rest list) list
  (call dynamic-extent-closure-safe))

(defknown (mapc mapl) (callable list &rest list) list
  (foldable call dynamic-extent-closure-safe))

;;; We let values-list be foldable, since constant-folding will turn it into
;;; VALUES.  VALUES is not foldable, since MV constants are represented by a
;;; call to VALUES.
;;; 
(defknown values (&rest t) * (movable flushable unsafe))
(defknown values-list (list) * (movable foldable flushable))


;;;; In the "Macros" chapter:

(defknown macro-function (symbol &optional lexenv-or-null)
  (or function null)
  (flushable))
(defknown (macroexpand macroexpand-1) (t &optional lexenv-or-null)
  (values form &optional boolean))

(defknown compiler-macro-function (t &optional lexenv-or-null)
  (or function null)
  (flushable))


;;;; In the "Declarations" chapter:

(defknown proclaim (list) void)


;;;; In the "Symbols" chapter:

(defknown get (symbol t &optional t) t (flushable))
(defknown remprop (symbol t) t)
(defknown symbol-plist (symbol) list (flushable))
(defknown getf (list t &optional t) t (foldable flushable))
(defknown get-properties (list list) (values t t list) (foldable flushable))
(defknown symbol-name (symbol) simple-string (movable foldable flushable))
(defknown make-symbol (string) symbol (flushable))
(defknown copy-symbol (symbol &optional t) symbol (flushable))
(defknown gensym (&optional (or string unsigned-byte)) symbol ())
(defknown symbol-package (symbol) (or package null) (flushable))
(defknown keywordp (t) boolean (flushable))	  ; If someone uninterns it...


;;;; In the "Packages" chapter:


(deftype packagelike () '(or stringable package))
(deftype symbols () '(or list symbol))

;;; Should allow a package name, I think, tho CLtL II doesn't say so...
(defknown gentemp (&optional string packagelike) symbol)

(defknown make-package (stringable &key (:use list) (:nicknames list)
				   ;; ### Extensions...
				   (:internal-symbols index) (:external-symbols index))
	  package)
(defknown find-package (packagelike) (or package null) (flushable))
(defknown package-name (packagelike) (or simple-string null) (flushable))
(defknown package-nicknames (packagelike) list (flushable))
(defknown rename-package (packagelike packagelike &optional list) package)
(defknown package-use-list (packagelike) list (flushable))
(defknown package-used-by-list (packagelike) list (flushable))
(defknown package-shadowing-symbols (packagelike) list (flushable))
(defknown list-all-packages () list (flushable))
(defknown intern (string &optional packagelike)
  (values symbol (member :internal :external :inherited nil))
  ())
(defknown find-symbol (string &optional packagelike)
	  (values symbol (member :internal :external :inherited nil))
	  (flushable))
(defknown (export import) (symbols &optional packagelike) truth)
(defknown unintern (symbol &optional packagelike) boolean)
(defknown unexport (symbols &optional packagelike) truth)
(defknown shadowing-import (symbols &optional packagelike) truth)
(defknown shadow ((or symbol string list) &optional packagelike) truth)
(defknown (use-package unuse-package) ((or list packagelike) &optional packagelike) truth)
(defknown find-all-symbols (stringable) list (flushable))


;;;; In the "Numbers" chapter:

(defknown zerop (number) boolean (movable foldable flushable explicit-check))
(defknown (plusp minusp) (real) boolean
  (movable foldable flushable explicit-check))
(defknown (oddp evenp) (integer) boolean
  (movable foldable flushable explicit-check))
(defknown (= /=) (number &rest number) boolean
  (movable foldable flushable explicit-check))
(defknown (< > <= >=) (real &rest real) boolean
  (movable foldable flushable explicit-check))
(defknown (max min) (real &rest real) real
  (movable foldable flushable explicit-check))

(defknown + (&rest number) number
  (movable foldable flushable explicit-check))
(defknown - (number &rest number) number
  (movable foldable flushable explicit-check))
(defknown * (&rest number) number
  (movable foldable flushable explicit-check))
(defknown / (number &rest number) number
  (movable foldable flushable explicit-check))
(defknown (1+ 1-) (number) number
  (movable foldable flushable explicit-check))

(defknown conjugate (number) number
  (movable foldable flushable explicit-check))

(defknown gcd (&rest integer) unsigned-byte
  (movable foldable flushable explicit-check)
  #|:derive-type 'boolean-result-type|#)
(defknown lcm (&rest integer) unsigned-byte
  (movable foldable flushable explicit-check))

(defknown exp (number) irrational
  (movable foldable flushable explicit-check recursive))


(defknown expt (number number) number
  (movable foldable flushable explicit-check recursive))
(defknown log (number &optional real) irrational
  (movable foldable flushable explicit-check))
(defknown sqrt (number) irrational
  (movable foldable flushable explicit-check))
(defknown isqrt (unsigned-byte) unsigned-byte
  (movable foldable flushable explicit-check))

(defknown (abs phase signum) (number) number
  (movable foldable flushable explicit-check))
(defknown cis (real) (complex float)
  (movable foldable flushable explicit-check))

(defknown (sin cos) (number)
  (or (float -1.0 1.0) (complex float))
  (movable foldable flushable explicit-check recursive))

(defknown atan
  (number &optional real) irrational
  (movable foldable flushable explicit-check recursive))

(defknown (tan sinh cosh tanh asinh)
  (number) irrational (movable foldable flushable explicit-check recursive))

(defknown (asin acos acosh atanh)
  (number) irrational
  (movable foldable flushable explicit-check recursive))

(defknown float (real &optional float) float
  (movable foldable flushable explicit-check))

(defknown (rational rationalize) (real) rational
  (movable foldable flushable explicit-check))

(defknown (numerator denominator) (rational) integer
  (movable foldable flushable))

(defknown (floor ceiling truncate round)
  (real &optional real) (values integer real)
  (movable foldable flushable explicit-check))

(defknown (mod rem) (real real) real
  (movable foldable flushable explicit-check))

(defknown (ffloor fceiling fround ftruncate)
  (real &optional real) (values float real)
  (movable foldable flushable explicit-check))

(defknown decode-float (float)
  (values (float 0.5d0 (1d0))
	  float-exponent
	  (member 1f0 -1f0 -1d0 1d0))
  (movable foldable flushable explicit-check))
(defknown scale-float (float float-exponent) float
  (movable foldable flushable explicit-check))
(defknown float-radix (float) float-radix
  (movable foldable flushable explicit-check))
(defknown float-sign (float &optional float) float
  (movable foldable flushable explicit-check))
(defknown (float-digits float-precision) (float) float-digits
  (movable foldable flushable explicit-check))
(defknown integer-decode-float (float)
	  (values integer float-exponent (member -1 1))
	  (movable foldable flushable explicit-check))

(defknown complex (real &optional real) number
  (movable foldable flushable explicit-check))

(defknown (realpart imagpart) (number) real (movable foldable flushable))

(defknown (logior logxor logand logeqv) (&rest integer) integer
  (movable foldable flushable explicit-check))

(defknown (lognand lognor logandc1 logandc2 logorc1 logorc2)
	  (integer integer) integer
  (movable foldable flushable explicit-check))

(defknown boole (boole-code integer integer) integer
  (movable foldable flushable))

(defknown lognot (integer) integer (movable foldable flushable explicit-check))
(defknown logtest (integer integer) boolean (movable foldable flushable))
(defknown logbitp (unsigned-byte integer) boolean (movable foldable flushable))
(defknown ash (integer ash-index) integer (movable foldable flushable explicit-check))
(defknown (logcount integer-length) (integer) bit-index
  (movable foldable flushable explicit-check))
(defknown byte (bit-index bit-index) byte-specifier
  (movable foldable flushable))
(defknown (byte-size byte-position) (byte-specifier) bit-index
  (movable foldable flushable)) 
(defknown ldb (byte-specifier integer) integer (movable foldable flushable))
(defknown ldb-test (byte-specifier integer) boolean
  (movable foldable flushable))
(defknown mask-field (byte-specifier integer) integer
  (movable foldable flushable))
(defknown dpb (integer byte-specifier integer) integer
  (movable foldable flushable))
(defknown deposit-field (integer byte-specifier integer) integer
  (movable foldable flushable))
(defknown random (real &optional random-state) real ())
(defknown make-random-state (&optional (or (member nil t) random-state))
  random-state (flushable))
(defknown random-state-p (t) boolean (movable foldable flushable))

;;; In "Characters" chapter:
(defknown (standard-char-p graphic-char-p alpha-char-p
			   upper-case-p lower-case-p both-case-p alphanumericp)
  (character) boolean (movable foldable flushable))

(defknown digit-char-p (character &optional unsigned-byte)
  (or (integer 0 35) null) (movable foldable flushable))

(defknown (char= char/= char< char> char<= char>= char-equal char-not-equal
		 char-lessp char-greaterp char-not-greaterp char-not-lessp)
  (character &rest character) boolean (movable foldable flushable))

(defknown character (t) character (movable foldable flushable))
(defknown char-code (character) char-code (movable foldable flushable))
(defknown code-char (char-code) base-char (movable foldable flushable))
(defknown (char-upcase char-downcase) (character) character
  (movable foldable flushable))
(defknown digit-char (integer &optional integer)
  (or character null) (movable foldable flushable))
(defknown char-int (character) char-code (movable foldable flushable))
(defknown char-name (character) (or simple-string null)
  (movable foldable flushable))
(defknown name-char (stringable) (or character null)
  (movable foldable flushable))


;;;; In the "Sequences" chapter:

(defknown elt (sequence index) t (foldable flushable))

(defknown subseq (sequence index &optional sequence-end) consed-sequence
  (flushable)
  :derive-type (sequence-result-nth-arg 1))

(defknown copy-seq (sequence) consed-sequence (flushable)
  :derive-type (sequence-result-nth-arg 1))


(defknown length (sequence) index (foldable flushable))

(defknown reverse (sequence) consed-sequence (flushable)
  :derive-type #'result-type-first-arg/reverse)

(defknown nreverse (sequence) sequence ()
  :derive-type #'result-type-first-arg/reverse
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1)
  :result-not-used (list-function-result-not-used 1))

(defknown make-sequence (type-specifier index &key (:initial-element t)) consed-sequence
  (movable flushable unsafe)
  ;; Nope:  If the type-specifier isn't a consed-sequence, we get confused.
  ;;:derive-type (result-type-specifier-nth-arg 1)
  )

(defknown concatenate (type-specifier &rest sequence) consed-sequence
  (flushable)
  ;; Nope:  If the type-specifier isn't a consed-sequence, we get confused.
  ;;:derive-type (result-type-specifier-nth-arg 1)
  )

(defknown map (type-specifier callable sequence &rest sequence) consed-sequence
  (flushable call dynamic-extent-closure-safe)
;  :derive-type 'type-spec-arg1  Nope... (map nil ...) returns null, not nil.
  )

(defknown map-into (sequence callable &rest sequence)
  sequence
  (call dynamic-extent-closure-safe)
  :derive-type #'result-type-first-arg
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

;;; Returns predicate result... 
(defknown some (callable sequence &rest sequence) t
  (foldable flushable call dynamic-extent-closure-safe))

(defknown (every notany notevery) (callable sequence &rest sequence) boolean
  (foldable flushable call dynamic-extent-closure-safe))

;;; Unsafe for :Initial-Value...
(defknown reduce (callable sequence &key (:from-end t) (:start index)
			   (:end sequence-end) (:initial-value t) (:key callable))
  t
  (foldable flushable call unsafe dynamic-extent-closure-safe))

(defknown fill (sequence t &key (:start index) (:end sequence-end)) sequence
  (unsafe)
  :derive-type #'result-type-first-arg
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

(defknown replace (sequence sequence &key (:start1 index) (:end1 sequence-end)
			    (:start2 index) (:end2 sequence-end))
  sequence ()
  :derive-type #'result-type-first-arg
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

(defknown remove
  (t sequence &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  consed-sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 2))

(defknown substitute
  (t t sequence &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  consed-sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 3))

(defknown (remove-if remove-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:count sequence-count) (:key callable))
  consed-sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 2))

(defknown (substitute-if substitute-if-not)
  (t callable sequence &key (:from-end t) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  consed-sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 3))

(defknown delete
  (t sequence &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 2)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2)
  :result-not-used (list-function-result-not-used 2))

(defknown nsubstitute
  (t t sequence &key (:from-end t) (:test callable)
     (:test-not callable) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 3)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 3))

(defknown (delete-if delete-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:count sequence-count) (:key callable))
  sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 2)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2)
  :result-not-used (list-function-result-not-used 2))

(defknown (nsubstitute-if nsubstitute-if-not)
  (t callable sequence &key (:from-end t) (:start index) (:end sequence-end)
     (:count sequence-count) (:key callable))
  sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 3)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 3))

(defknown remove-duplicates
  (sequence &key (:test callable) (:test-not callable) (:start index) (:from-end t)
	    (:end sequence-end) (:key callable))
  consed-sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 1))

(defknown delete-duplicates
  (sequence &key (:test callable) (:test-not callable) (:start index) (:from-end t)
	    (:end sequence-end) (:key callable))
  sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 1)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1)
  :result-not-used (list-function-result-not-used 1))

(defknown find (t sequence &key (:test callable) (:test-not callable)
		  (:start index) (:from-end t) (:end sequence-end) (:key callable))
  t
  (foldable flushable call dynamic-extent-closure-safe))

(defknown (find-if find-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:key callable))
  t
  (foldable flushable call dynamic-extent-closure-safe))

(defknown position (t sequence &key (:test callable) (:test-not callable)
		      (:start index) (:from-end t) (:end sequence-end)
		      (:key callable))
  (or index null)
  (foldable flushable call dynamic-extent-closure-safe))

(defknown (position-if position-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:key callable))
  (or index null)
  (foldable flushable call dynamic-extent-closure-safe))

(defknown count (t sequence &key (:test callable) (:test-not callable)
		      (:start index) (:from-end t) (:end sequence-end)
		      (:key callable))
  index
  (foldable flushable call dynamic-extent-closure-safe))

(defknown (count-if count-if-not)
  (callable sequence &key (:from-end t) (:start index) (:end sequence-end)
	    (:key callable))
  index
  (foldable flushable call dynamic-extent-closure-safe))

(defknown (mismatch search)
  (sequence sequence &key (:from-end t) (:test callable) (:test-not callable)
	    (:start1 index) (:end1 sequence-end) (:start2 index) (:end2 sequence-end)
	    (:key callable))
  (or index null)
  (foldable flushable call dynamic-extent-closure-safe))

;;; Not flushable, since vector sort guaranteed in-place...
(defknown (stable-sort sort) (sequence callable &key (:key callable)) sequence
  (call dynamic-extent-closure-safe)
  :derive-type (sequence-result-nth-arg 1)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1)
  :result-not-used (list-function-result-not-used 1))

(defknown merge (type-specifier sequence sequence callable
				&key (:key callable))
  sequence
  (flushable call dynamic-extent-closure-safe)
  :derive-type (result-type-specifier-nth-arg 1)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2 3)
  ;; FIXME!  This is a little complicated.  
  ;;:result-not-used #'function-result-not-used-p
  )

(defknown read-sequence (sequence stream &key (:start index)
					      (:end sequence-end)
					      (:partial-fill boolean))
  (index) ())

(defknown write-sequence (sequence stream &key (:start index)
				   (:end sequence-end))
  sequence ()
  :derive-type (sequence-result-nth-arg 1))


;;;; In the "Manipulating List Structure" chapter:

(defknown (car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
	       cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar
	       cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	       first second third fourth fifth sixth seventh eighth ninth tenth
	       rest)
  (list) t (foldable flushable))

(defknown cons (t t) cons (movable flushable unsafe))

(defknown tree-equal (t t &key (:test callable) (:test-not callable)) boolean
  (foldable flushable call dynamic-extent-closure-safe))
(defknown endp (list) boolean (foldable flushable movable))
(defknown list-length (list) (or index null) (foldable flushable))
(defknown (nth nthcdr) (unsigned-byte list) t (foldable flushable))
(defknown last (list &optional unsigned-byte) list (foldable flushable))
(defknown list (&rest t) list (movable flushable unsafe))
(defknown list* (t &rest t) t (movable flushable unsafe))
(defknown make-list (index &key (:initial-element t)) list
  (movable flushable unsafe))

;;;
;;; All but last must be list...
(defknown append (&rest t) t (flushable))

(defknown copy-list (list) list (flushable))
(defknown copy-alist (list) list (flushable))
(defknown copy-tree (t) t (flushable))
(defknown revappend (list t) t (flushable))
(defknown nconc (&rest t) t ()
  :destroyed-constant-args (remove-non-constants-and-nils #'butlast))
(defknown nreconc (list t) list ()
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1)
  :result-not-used #'function-result-not-used-p)
(defknown butlast (list &optional unsigned-byte) list (flushable))
(defknown nbutlast (list &optional unsigned-byte) list ()
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))
(defknown ldiff (list t) list (flushable))
(defknown (rplaca rplacd) (cons t) list (unsafe)
  :destroyed-constant-args (nth-constant-args 1))

(defknown subst (t t t &key (:key callable) (:test callable)
                   (:test-not callable))
  t (flushable unsafe call))

(defknown (nsubst) (t t t &key (:key callable) (:test callable)
			    (:test-not callable))
  list (flushable unsafe call dynamic-extent-closure-safe)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 3))

(defknown (subst-if subst-if-not nsubst-if nsubst-if-not)
	  (t t t &key (:key callable))
  list (flushable unsafe call dynamic-extent-closure-safe)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 3))

(defknown (sublis) (list t &key (:key callable) (:test callable)
				 (:test-not callable))
  list (flushable unsafe call dynamic-extent-closure-safe))

(defknown nsublis (list t &key (:key callable) (:test callable)
                        (:test-not callable))
  t (flushable unsafe call)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2))

(defknown member (t list &key (:key callable) (:test callable)
		    (:test-not callable))
  list (foldable flushable call dynamic-extent-closure-safe))
(defknown (member-if member-if-not) (callable list &key (:key callable))
  list (foldable flushable call dynamic-extent-closure-safe))

(defknown tailp (t list) boolean (foldable flushable))

(defknown adjoin (t list &key (:key callable) (:test callable)
		    (:test-not callable))
  list (foldable flushable unsafe call dynamic-extent-closure-safe))

(defknown (union intersection set-difference set-exclusive-or)
	  (list list &key (:key callable) (:test callable) (:test-not callable))
  list
  (foldable flushable call dynamic-extent-closure-safe))

(defknown (nunion nintersection nset-difference nset-exclusive-or)
	  (list list &key (:key callable) (:test callable) (:test-not callable))
  list
  (foldable flushable call dynamic-extent-closure-safe)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1 2)
  :result-not-used #'function-result-not-used-p)

(defknown subsetp (list list &key (:key callable) (:test callable)
			(:test-not callable))
  boolean
  (foldable flushable call dynamic-extent-closure-safe))

(defknown acons (t t t) list (movable flushable unsafe))
(defknown pairlis (t t &optional t) list (flushable unsafe))

(defknown (rassoc assoc)
	  (t list &key (:key callable) (:test callable) (:test-not callable))
  list (foldable flushable call dynamic-extent-closure-safe))
(defknown (assoc-if-not assoc-if rassoc-if rassoc-if-not)
	  (callable list &key (:key callable)) list
	  (foldable flushable call dynamic-extent-closure-safe))

(defknown (memq assq) (t list) list (foldable flushable unsafe))
(defknown delq (t list) list (flushable unsafe)
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 2))
  

;;;; In the "Hash Tables" chapter:

(defknown make-hash-table
  (&key (:test callable) (:size index)
	(:rehash-size (or (integer 1) (float (1.0))))
	(:rehash-threshold (real 0 1))
	(:weak-p t))
  hash-table
  (flushable unsafe))
(defknown hash-table-p (t) boolean (movable foldable flushable))
(defknown gethash (t hash-table &optional t) (values t boolean)
  (foldable flushable unsafe))
(defknown %puthash (t hash-table t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 2))
(defknown remhash (t hash-table) boolean ()
  :destroyed-constant-args (nth-constant-args 2))
(defknown maphash (callable hash-table) null
  (foldable flushable call dynamic-extent-closure-safe))
(defknown clrhash (hash-table) hash-table ()
  :destroyed-constant-args (nth-constant-args 1))
(defknown hash-table-count (hash-table) index (foldable flushable))
(defknown hash-table-rehash-size (hash-table) (or (integer 1) (float (1.0)))
  (foldable flushable))
(defknown hash-table-rehash-threshold (hash-table) (real 0 1)
  (foldable flushable))
(defknown hash-table-size (hash-table) index (foldable flushable))
(defknown hash-table-test (hash-table) symbol (foldable flushable))
(deftype non-negative-fixnum () `(integer 0 ,most-positive-fixnum))
(defknown sxhash (t) non-negative-fixnum (foldable flushable))


;;;; In the "Arrays" chapter:

(defknown make-array ((or index list) &key (:element-type type-specifier)
		      (:initial-element t) (:initial-contents t)
		      (:adjustable t) (:fill-pointer t)
		      (:displaced-to (or array null))
		      (:displaced-index-offset index))
  array (flushable unsafe))

(defknown vector (&rest t) simple-vector (flushable unsafe))

(defknown aref (array &rest index) t (foldable flushable))
(defknown row-major-aref (array index) t (foldable flushable))

(defknown array-element-type (array) type-specifier (foldable flushable))
(defknown array-rank (array) array-rank (foldable flushable))
(defknown array-dimension (array array-rank) index (foldable flushable))
(defknown array-dimensions (array) list (foldable flushable))
(defknown array-in-bounds-p (array &rest integer) boolean (foldable flushable))
(defknown array-row-major-index (array &rest index) array-total-size
  (foldable flushable)) 
(defknown array-total-size (array) array-total-size (foldable flushable))
(defknown adjustable-array-p (array) boolean (movable foldable flushable))

(defknown svref (simple-vector index) t (foldable flushable))
(defknown bit ((array bit) &rest index) bit (foldable flushable))
(defknown sbit ((simple-array bit) &rest index) bit (foldable flushable))

;;; FIXME: :DESTROYED-CONSTANT-ARGS for these is complicated.
(defknown (bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2
		   bit-orc1 bit-orc2)
  ((array bit) (array bit) &optional (or (array bit) (member nil t)))
  (array bit)
  (foldable)
  #|:derive-type #'result-type-last-arg|#)

(defknown bit-not ((array bit) &optional (or (array bit) (member nil t)))
  (array bit)
  (foldable)
  #|:derive-type #'result-type-last-arg|#)

(defknown array-has-fill-pointer-p (array) boolean (movable foldable flushable))
(defknown fill-pointer (vector) index (foldable flushable))
(defknown vector-push (t vector) (or index null) ()
  :destroyed-constant-args (nth-constant-args 2))
(defknown vector-push-extend (t vector &optional index) index ()
  :destroyed-constant-args (nth-constant-args 2))
(defknown vector-pop (vector) t ()
  :destroyed-constant-args (nth-constant-args 1))

;;; FIXME: complicated :DESTROYED-CONSTANT-ARGS
;;; Also, an important-result warning could be provided if the array
;;; is known to be not expressly adjustable.
(defknown adjust-array
  (array (or index list) &key (:element-type type-specifier)
	 (:initial-element t) (:initial-contents t)
	 (:fill-pointer t) (:displaced-to (or array null))
	 (:displaced-index-offset index))
  array (unsafe)
  :result-not-used #'adjust-array-result-not-used-p)
;  :derive-type 'result-type-arg1) Not even close...


;;;; In the "Strings" chapter:

(defknown char (string index) character (foldable flushable))
(defknown schar (simple-string index) character (foldable flushable))

(deftype stringable () '(or character string symbol))

(defknown (string= string-equal)
  (stringable stringable &key (:start1 index) (:end1 sequence-end)
	      (:start2 index) (:end2 sequence-end))
  boolean
  (foldable flushable))

(defknown (string< string> string<= string>= string/= string-lessp
		   string-greaterp string-not-lessp string-not-greaterp
		   string-not-equal)
  (stringable stringable &key (:start1 index) (:end1 sequence-end)
	      (:start2 index) (:end2 sequence-end))
  (or index null)
  (foldable flushable))

(defknown make-string (index &key (:element-type type-specifier)
		       (:initial-element character))
  simple-string (flushable))

(defknown (string-trim string-left-trim string-right-trim)
  (sequence stringable) simple-string (flushable))

(defknown (string-upcase string-downcase string-capitalize)
  (stringable &key (:start index) (:end sequence-end))
  simple-string (flushable))

(defknown (nstring-upcase nstring-downcase nstring-capitalize)
  (string &key (:start index) (:end sequence-end))
  string ()
  :destroyed-constant-args (nth-constant-nonempty-sequence-args 1))

(defknown string (stringable) string
  (flushable explicit-check))


;;; Internal non-keyword versions of string predicates:

(defknown (string<* string>* string<=* string>=* string/=*)
  (stringable stringable index sequence-end index sequence-end)
  (or index null)
  (foldable flushable))

(defknown string=*
  (stringable stringable index sequence-end index sequence-end)
  boolean
  (foldable flushable))


;;;; In the "Eval" chapter:

(defknown eval (t) *)
(defknown constantp (t &optional lexenv-or-null) boolean
  (foldable flushable))


;;;; In the "Streams" chapter:

(defknown make-synonym-stream (symbol) stream (flushable))
(defknown make-broadcast-stream (&rest stream) stream (flushable))
(defknown make-concatenated-stream (&rest stream) stream (flushable))
(defknown make-two-way-stream (stream stream) stream (flushable))
(defknown make-echo-stream (stream stream) stream (flushable))
(defknown make-string-input-stream (string &optional index index) stream (flushable unsafe))
(defknown make-string-output-stream (&key (:element-type type-specifier))
  stream
  (flushable))
(defknown get-output-stream-string (stream) simple-string ())
(defknown streamp (t) boolean (movable foldable flushable))
(defknown stream-element-type (stream) type-specifier (movable foldable flushable))
(defknown (output-stream-p input-stream-p) (stream) boolean (movable foldable
								     flushable))
(defknown close (stream &key (:abort t)) t ())


;;;; In the "Input/Output" chapter:

;;; The I/O functions are currently given effects ANY under the theory that
;;; code motion over I/O operations is particularly confusing and not very
;;; important for efficency.

(defknown copy-readtable (&optional (or readtable null) (or readtable null))
  readtable
  ())
(defknown readtablep (t) boolean (movable foldable flushable))

(defknown set-syntax-from-char
  (character character &optional (or readtable null) readtable) void
  ())

(defknown set-macro-character (character callable &optional t readtable) void
  (unsafe))
(defknown get-macro-character (character &optional (or readtable null))
  (values callable boolean) (flushable))

(defknown make-dispatch-macro-character (character &optional t readtable)
  void ())
(defknown set-dispatch-macro-character
  (character character callable &optional readtable) void
  (unsafe))
(defknown get-dispatch-macro-character
  (character character &optional (or readtable null)) callable
  (flushable))

;;; May return any type due to eof-value...
(defknown (read read-preserving-whitespace read-char-no-hang read-char)
  (&optional streamlike t t t) t  (explicit-check))

(defknown read-delimited-list (character &optional streamlike t) list
  (explicit-check))
(defknown read-line (&optional streamlike t t t) (values t boolean)
  (explicit-check))
(defknown unread-char (character &optional streamlike) t
  (explicit-check))
(defknown peek-char (&optional (or character (member nil t)) streamlike t t t)
  t
  (explicit-check))
(defknown listen (&optional streamlike
			    (or null (integer 1 10) (member character)))
  boolean (flushable explicit-check))

(defknown clear-input (&optional stream boolean) null (explicit-check))

(defknown read-from-string
  (string &optional t t &key (:start index) (:end sequence-end)
	  (:preserve-whitespace t))
  (values t index))
(defknown parse-integer
  (string &key (:start index) (:end sequence-end) (:radix (integer 2 36))
	  (:junk-allowed t)) 
  (values (or integer null ()) index))

(defknown read-byte (stream &optional t t) t (explicit-check))

(defknown write
  (t &key (:stream streamlike) (:escape t) (:radix t) (:base (integer 2 36))
     (:circle t) (:pretty t) (:level (or unsigned-byte null)) (:readably t)
     (:length (or unsigned-byte null)) (:case t) (:array t) (:gensym t)
     (:lines (or unsigned-byte null)) (:right-margin (or unsigned-byte null))
     (:miser-width (or unsigned-byte null)) (:pprint-dispatch t))
  t
  (any explicit-check)
  :derive-type #'result-type-first-arg)

(defknown (prin1 print princ) (t &optional streamlike) t (any explicit-check)
  :derive-type #'result-type-first-arg)

;;; xxx-TO-STRING not foldable because they depend on the dynamic environment. 
(defknown write-to-string
  (t &key (:escape t) (:radix t) (:base (integer 2 36)) (:readably t)
     (:circle t) (:pretty t) (:level (or unsigned-byte null))
     (:length (or unsigned-byte null)) (:case t) (:array t) (:gensym t)
     (:lines (or unsigned-byte null)) (:right-margin (or unsigned-byte null))
     (:miser-width (or unsigned-byte null)) (:pprint-dispatch t))
  simple-string
  (foldable flushable explicit-check))

(defknown (prin1-to-string princ-to-string) (t) simple-string (flushable))

(defknown write-char (character &optional streamlike) character
  (explicit-check))
(defknown (write-string write-line)
  (string &optional streamlike &key (:start index) (:end sequence-end))
  string
  (explicit-check))

(defknown (terpri finish-output force-output clear-output)
  (&optional streamlike) null
  (explicit-check))

(defknown fresh-line (&optional streamlike) boolean
  (explicit-check))

(defknown write-byte (integer stream) integer
  (explicit-check))

;;; FIXME: complicated :DESTROYED-CONSTANT-ARGS
(defknown format ((or streamlike string) (or string function) &rest t)
  (or string null)
  (explicit-check))

(defknown (y-or-n-p yes-or-no-p) (&optional string &rest t) boolean
  (explicit-check))


;;;; In the "File System Interface" chapter:

;;; No pathname functions are foldable because they all potentially depend on
;;; *default-pathname-defaults*, e.g. to provide a default host when parsing a
;;; namestring.

(defknown wild-pathname-p (pathnamelike &optional (member nil :host :device
							  :directory :name
							  :type :version))
  boolean
  (flushable))
(defknown pathname-match-p (pathnamelike pathnamelike) boolean
  (flushable))
(defknown translate-pathname (pathnamelike pathnamelike pathnamelike &key)
  pathname
  (flushable))

;;; Need to add the logical pathname stuff here.

(defknown pathname (pathnamelike) pathname (flushable))
(defknown truename (pathnamelike) pathname ())

(defknown parse-namestring
  (pathnamelike &optional (or string pathname-host) pathnamelike
		&key (:start index) (:end sequence-end) (:junk-allowed t))
  (values (or pathname null) sequence-end)
  ())

(defknown merge-pathnames
  (pathnamelike &optional pathnamelike pathname-version)
  pathname
  (flushable))

(defknown make-pathname
 (&key (:defaults pathnamelike)
       (:host (or null string pathname-host))
       (:device (or string pathname-device))
       (:directory (or pathname-directory string (member :wild)))
       (:name (or pathname-name string (member :wild)))
       (:type (or pathname-type string (member :wild)))
       (:version pathname-version) (:case (member :local :common)))
  pathname (flushable))

(defknown pathnamep (t) boolean (movable  flushable))

(defknown pathname-host (pathnamelike &key (:case (member :local :common)))
  (or simple-string null) (flushable))
(defknown pathname-device (pathnamelike &key (:case (member :local :common)))
  pathname-device (flushable))
(defknown pathname-directory (pathnamelike &key (:case (member :local :common)))
  pathname-directory (flushable))
(defknown pathname-name (pathnamelike &key (:case (member :local :common)))
  pathname-name (flushable))
(defknown pathname-type (pathnamelike &key (:case (member :local :common)))
  pathname-type (flushable))
(defknown pathname-version (pathnamelike)
  pathname-version (flushable))

(defknown (namestring file-namestring directory-namestring host-namestring)
  (pathnamelike) (or null simple-string)
  (flushable))

(defknown enough-namestring (pathnamelike &optional pathnamelike)
  simple-string
  (flushable))

(defknown user-homedir-pathname (&optional t) pathname (flushable))

(defknown open (t &rest t
		  &key (:direction (member :input :output :io :probe))
		       (:element-type type-specifier)
		       (:if-exists (member :error :new-version :rename
					   :rename-and-delete :overwrite
					   :append :supersede nil))
		       (:if-does-not-exist (member :error :create nil))
		       (:external-format (member :default))
		       (:class (or symbol class))
		       (:mapped boolean)
		       (:input-handle (or null fixnum stream))
		       (:output-handle (or null fixnum stream))
		  &allow-other-keys)
  (or stream null)
  ()
  :derive-type #'result-type-open-class)

(defknown rename-file (pathnamelike filename) (values pathname pathname pathname))
(defknown delete-file (pathnamelike) t)
(defknown probe-file (pathnamelike) (or pathname null) (flushable))
(defknown file-write-date (pathnamelike) (or unsigned-byte null) (flushable))
(defknown file-author (pathnamelike) (or simple-string null) (flushable))

(defknown file-position (stream &optional
				(or unsigned-byte (member :start :end)))
  (or unsigned-byte (member t nil)))
(defknown file-length (stream) (or unsigned-byte null) (flushable))

(defknown load
  ((or filename stream)
   &key (:verbose t) (:print t) (:if-does-not-exist (member :error :create nil))
   (:if-source-newer (member :load-source :load-object :query :compile))
   (:contents (or null (member :source :binary)))
   (:external-format t))
  t)

(defknown directory (pathnamelike &key (:check-for-subdirs t) (:all t)
				  (:truenamep t) (:follow-links t))
  list (flushable))


;;;; In the "Errors" chapter:

(defknown error (t &rest t) nil) ; Never returns...
(defknown cerror ((or string function) t &rest t) null)
(defknown warn (t &rest t) null)
(defknown break (&optional t &rest t) null)


;;;; In the "Miscellaneous" Chapter.

;;; ### Compiler interface non-standard...
(defknown compile ((or symbol cons) &optional (or list function null))
  (values (or function symbol cons) boolean boolean))

(deftype optional-filename () '(or filename (member t nil)))

(defknown compile-file
  ((or optional-filename stream list) &key (:output-file optional-filename)
   (:error-file optional-filename)
   (:trace-file optional-filename) (:error-output t) (:load t) (:verbose t)
   (:print t) (:progress t)
   (:block-compile (member t nil :specified))
   (:entry-points list)
   (:byte-compile (member t nil :maybe))
   (:external-format (member :default))
   (:xref t))
  (values (or pathname null) boolean boolean))

(defknown disassemble ((or callable cons)
		       &key (:stream stream) (:backend backend)
		       (:use-labels t))
  (values))

(defknown documentation (t symbol)
  (or string null)
  (flushable))

(defknown describe (t &optional (or stream (member t nil))) (values))
(defknown inspect (t) t)

(defknown room (&optional (member t nil :default)) (values))
(defknown ed (&optional (or symbol cons filename) &key (:init t) (:display t))
  t)
(defknown dribble (&optional filename &key (:if-exists t)) (values))

(defknown apropos (stringable &optional packagelike) (values))
(defknown apropos-list (stringable &optional packagelike) list (flushable))

(defknown get-decoded-time ()
  (values (integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
	  (integer 1 12) unsigned-byte (integer 0 6) boolean (rational -24 24))
  (flushable))

(defknown get-universal-time () unsigned-byte (flushable))

(defknown decode-universal-time
	  (unsigned-byte &optional (or null (rational -24 24)))
  (values (integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
	  (integer 1 12) unsigned-byte (integer 0 6) boolean (rational -24 24))
  (flushable))

(defknown encode-universal-time
  ((integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
   (integer 1 12) unsigned-byte &optional (or null (rational -24 24)))
  unsigned-byte
  (flushable))

(defknown (get-internal-run-time get-internal-real-time)
  () internal-time (flushable))

(defknown sleep ((or (rational 0) (float 0.0))) null)

(defknown (lisp-implementation-type
	   lisp-implementation-version machine-type machine-version
	   machine-instance software-type software-version short-site-name
	   long-site-name)
  () simple-string (flushable))

(defknown identity (t) t (movable foldable flushable unsafe)
  :derive-type #'result-type-first-arg)

;;; &optional is to agree with the optimization in the interpreter stub.
(defknown constantly (t &optional t t &rest t) function (movable flushable))
(defknown complement (function) function (movable flushable))


;;;; Magical compiler frobs:

;;; Can't fold in general because of SATISFIES.  There is a special optimizer
;;; anyway.
(defknown %typep (t (or type-specifier ctype)) boolean
  (movable flushable explicit-check))
(defknown %instance-typep (t (or type-specifier ctype)) boolean
  (movable flushable explicit-check))

(defknown %dynamic-extent (t t) void)
(defknown %dynamic-extent-start () t)
(defknown %dynamic-extent-end (t t) void)
(defknown %cleanup-point () void)
(defknown %special-bind (t t) void)
(defknown %special-unbind (t) void)
(defknown %listify-rest-args (t index t) list (flushable))
(defknown %more-arg-context (t t) (values t index) (flushable))
(defknown %more-arg (t index) t)
(defknown %more-arg-values (t index index) * (flushable))
(defknown %verify-argument-count (index index) (values))
(defknown %argument-count-error (t) nil)
(defknown %unknown-values () *)
(defknown %catch (t t) void)
(defknown %unwind-protect (t t) void)
(defknown (%catch-breakup %unwind-protect-breakup) () void)
(defknown %lexical-exit-breakup (t) void)
(defknown %continue-unwind (t t t) nil)
(defknown %throw (t &rest t) nil); This is MV-called.
(defknown %nlx-entry (t) *)
(defknown %%primitive (t t &rest t) *)
(defknown %pop-values (t) void)
(defknown %type-check-error (t t) nil)
(defknown %odd-keyword-arguments-error () nil)
(defknown %unknown-keyword-argument-error (t) nil)
(defknown (%ldb %mask-field) (bit-index bit-index integer) unsigned-byte
  (movable foldable flushable explicit-check))
(defknown (%dpb %deposit-field) (integer bit-index bit-index integer) integer
  (movable foldable flushable explicit-check))
(defknown %negate (number) number (movable foldable flushable explicit-check))
(defknown %check-bound (array index fixnum) index (movable foldable flushable))
(defknown data-vector-ref (array index) t (foldable flushable explicit-check))
(defknown data-vector-set (array index t) t (unsafe explicit-check))
(defknown kernel:%caller-frame-and-pc () (values t t) (flushable))
(defknown kernel:%with-array-data (array index (or index null))
  (values (simple-array * (*)) index index index)
  (foldable flushable))
(defknown %set-symbol-package (symbol t) t (unsafe))
(defknown %coerce-to-function (t) function (flushable))

;;; Structure slot accessors or setters are magically "known" to be these
;;; functions, although the var remains the Slot-Accessor describing the actual
;;; function called.
;;;
(defknown %slot-accessor (t) t (flushable))
(defknown %slot-setter (t t) t (unsafe))


;;;; Setf inverses:

(defknown %aset (array &rest t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %set-row-major-aref (array index t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %rplaca (cons t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %rplacd (cons t) t (unsafe))
(defknown %put (symbol t t) t (unsafe))
(defknown %setelt (sequence index t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %svset (simple-vector index t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %bitset ((array bit) &rest index) bit (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %sbitset ((simple-array bit) &rest index) bit (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %charset (string index character) character (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %scharset (simple-string index character) character (unsafe)
  :destroyed-constant-args (nth-constant-args 1))
(defknown %set-symbol-value (symbol t) t (unsafe))
(defknown fset (symbol function) function (unsafe))
(defknown %set-symbol-plist (symbol t) t (unsafe))
(defknown (setf documentation) ((or string null) t symbol)
  (or string null)
  ())
(defknown %setnth (index list t) t (unsafe)
  :destroyed-constant-args (nth-constant-args 2))
(defknown %set-fill-pointer (vector index) index (unsafe)
  :destroyed-constant-args (nth-constant-args 1))


;;;; Internal type predicates:
;;;
;;;    Simple typep uses that don't have any standard predicate are translated
;;; into non-standard unary predicates.

(defknown (fixnump bignump ratiop short-float-p single-float-p double-float-p
	   long-float-p base-char-p %standard-char-p %instancep
	   array-header-p)
  (t) boolean (movable foldable flushable))


;;;; Miscellaneous "sub-primitives":

(defknown %sp-string-compare
  (simple-string index index simple-string index index)
  (or index null)
  (foldable flushable))

(defknown compiler-error (string &rest t) nil ())
(defknown (compiler-warning compiler-note compiler-mumble)
    (string &rest t) (values) ())

