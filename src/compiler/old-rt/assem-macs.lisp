;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Some macros and stuff to make emitting RT assembler easier.
;;;
(in-package 'c)

#-new-compiler
(setq lisp::*bootstrap-defmacro* :both)

(defmacro no-op ()
  "A 2 byte no-op."
  '(inst lr zero-tn zero-tn))


;;; Loadi loads the specified Constant into the given Register.

(defmacro loadi (register constant)
  (once-only ((n-constant constant))
    `(cond ((<= 0 ,n-constant 15)
	    (inst lis ,register ,n-constant))
	   ((<= -32768 ,n-constant 32767)
	    (inst cal ,register zero-tn ,n-constant))
	   ((= (logand ,n-constant 65535) 0)
	    (inst cau ,register zero-tn (logand (ash ,n-constant -16) #xFFFF)))
	   (t
	    (inst cau ,register zero-tn (logand (ash ,n-constant -16) #xFFFF))
	    (inst oil ,register ,register (logand ,n-constant 65535))))))


(defmacro cmpi (register constant)
  (once-only ((n-constant constant))
    `(cond ((<= 0 ,n-constant 15)
	    (inst cis ,register ,n-constant))
	   ((<= -32768 ,n-constant 32767)
	    (inst ci ,register ,n-constant))
	   (T
	    (error "~A is to big for a compare immediate instruction."
		   ,n-constant)))))


;;; Memory reference macros that take a scaled immediate offset and use a short
;;; form instruction when possible.
;;;
(defmacro defmemref (name ds d shift)
  `(defmacro ,name (register index-register &optional (offset 0))
     (once-only ((n-offset offset))
       `(if (<= 0 ,n-offset ,,15)
	    (inst ,',ds ,n-offset ,register ,index-register)
	    (inst ,',d ,register ,index-register (ash ,n-offset ,,shift))))))


(defmacro loadh (register index-register &optional (offset 0))
  (once-only ((n-offset offset))
    `(if (zerop ,n-offset)
	 (inst lhs ,register ,index-register)
	 (inst lh ,register ,index-register (ash ,n-offset 1)))))


(defmemref loadc lcs lc 0)		; loads a character or byte
(defmemref loadha lhas lha 1)		; loads a halfword, sign-extending
(defmemref loadw ls l 2)		; loads a fullword

(defmemref storec stcs stc 0)		; stores a character or byte
(defmemref storeha sths sth 1)		; stores a halfword
(defmemref storew sts st 2)		; stores a fullword


;;; Load-Global, Store-Global  --  Public
;;;
;;;    Load or store a word in the assember global data area.  Base is a
;;; temporary register used to compute the base of the assembler data area, and
;;; must not be NL0 (i.e. should be Descriptor-Reg.)  In Load-Global, if base
;;; it omitted, we default it to the result Register.  Offset is a byte offset,
;;; but must be word-aligned.
;;;
(defmacro load-global (register offset &optional (base register))
  `(progn
     (inst cau ,base zero-tn clc::romp-data-base)
     (loadw ,register ,base (/ ,offset 4))))
;;;
(defmacro store-global (register offset base)
  `(progn
     (inst cau ,base zero-tn clc::romp-data-base)
     (storew ,register ,base (/ ,offset 4))))


;;; Load-Slot, Store-Slot  --  Public
;;;
;;;    Load or store a slot in a g-vector-like object.  We just add in the
;;; g-vector header size and do the load/store.
;;;
(defmacro load-slot (register object index)
  `(loadw ,register ,object
	  (+ ,index clc::g-vector-header-size-in-words)))
;;;
(defmacro store-slot (register object index)
  `(storew ,register ,object
	   (+ ,index clc::g-vector-header-size-in-words)))


;;; Load-Stack-TN, Store-Stack-TN  --  Interface
;;;
;;;    Move a stack TN to a register and vice-versa.
;;;
(defmacro load-stack-tn (reg stack)
  (once-only ((n-reg reg)
	      (n-stack stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg string-char-reg)
	(sc-case ,n-stack
	  ((stack string-char-stack)
	   (loadw ,n-reg fp-tn (tn-offset ,n-stack))))))))


(defmacro store-stack-tn (stack reg)
  (once-only ((n-stack stack)
	      (n-reg reg))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg string-char-reg)
	(sc-case ,n-stack
	  ((stack string-char-stack)
	   (storew ,n-reg fp-tn (tn-offset ,n-stack))))))))


;;; MAYBE-LOAD-STACK-TN  --  Interface
;;;
(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
	      (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg string-char-reg)
	(sc-case ,n-stack
	  ((any-reg descriptor-reg string-char-reg)
	   (unless (location= ,n-reg ,n-stack)
	     (inst lr ,n-reg ,n-stack)))
	  ((stack string-char-stack)
	   (loadw ,n-reg fp-tn (tn-offset ,n-stack))))))))


(defmacro test-simple-type (register temp target not-p type-code)
  "Emit conditional code that tests whether Register holds an object with the
  specified Type-Code.  Temp is an unboxed temporary."
  (once-only ((n-register register)
	      (n-temp temp)
	      (n-target target)
	      (n-not-p not-p)
	      (n-type-code type-code))
    `(progn
       (inst niuz ,n-temp ,n-register clc::type-mask-16)
       (inst xiu ,n-temp ,n-temp (ash ,n-type-code clc::type-shift-16))
       (if ,n-not-p
	   (inst bnb :eq ,n-target)
	   (inst bb :eq ,n-target)))))



(defmacro test-hairy-type (register temp target not-p &rest types)
  "Test-Hairy-Type Register Temp Target Not-P
                   {Type | (Low-Type High-Type)}*
   Test whether Register holds a value with one of a specified union of type
   codes.  Each separately specified Type is matched, and also all types
   between a Low-Type and High-Type pair (inclusive) are matched.  All of the
   type-code expressions are evaluated at macroexpand time.
   Temp may be any register."
  (once-only ((n-register register)
	      (n-temp temp)
	      (n-target target)
	      (n-not-p not-p))
    (assert types)
    (let ((codes
	   (sort (mapcar #'(lambda (x)
			     (if (listp x)
				 (cons (eval (first x)) (eval (second x)))
				 (eval x)))
			 types)
		 #'<
		 :key #'(lambda (x)
			  (if (consp x) (car x) x))))
	  (n-drop-thru (gensym)) (n-in-lab (gensym)) (n-out-lab (gensym)))

      (collect ((tests))
	(do ((codes codes (cdr codes)))
	    ((null codes))
	  (let ((code (car codes))
		(last (null (cdr codes))))
	    (cond
	     ((consp code)
	      (tests
	       `(progn
		  (cmpi ,n-temp ,(car code))
		  (inst bb :lt ,n-out-lab)
		  (cmpi ,n-temp ,(cdr code))))

	      (if last
		  (tests `(if ,n-not-p
			      (inst bb :gt ,n-target)
			      (inst bnb :gt ,n-target)))
		  (tests `(inst bnb :gt ,n-in-lab))))
	     (t
	      (tests `(cmpi ,n-temp ,code))
	      (if last
		  (tests `(if ,n-not-p
			      (inst bnb :eq ,n-target)
			      (inst bb :eq ,n-target)))
		  (tests `(inst bb :eq ,n-in-lab)))))))
		
		
	`(let* ((,n-drop-thru (gen-label))
		(,n-in-lab (if ,n-not-p ,n-drop-thru ,n-target))
		(,n-out-lab (if ,n-not-p ,n-target ,n-drop-thru)))
	   ,n-out-lab
	   (unless (location= ,n-temp ,n-register)
	     (inst lr ,n-temp ,n-register))
	   (inst sri16 ,n-temp clc::type-shift-16)
	   
	   ,@(tests)
	   
	   (emit-label ,n-drop-thru))))))


(defmacro test-special-value (reg temp value target not-p)
  "Test whether Reg holds the specified special Value (T, NIL, %Trap-Object).
  Temp is an unboxed register."
  (once-only ((n-reg reg)
	      (n-temp temp)
	      (n-value value)
	      (n-target target)
	      (n-not-p not-p))
    `(progn
       (inst xiu ,n-temp ,n-reg
	     (or (cdr (assoc ,n-value
			     `((t . ,',clc::t-16)
			       (nil . ,',clc::nil-16)
			       (%trap-object . ,',clc::trap-16))))
		 (error "Unknown special value: ~S." ,n-value)))
       (if ,n-not-p
	   (inst bnb :eq ,n-target)
	   (inst bb :eq ,n-target)))))


(defmacro pushm (reg)
  `(progn
     (inst inc sp-tn 4)
     (storew ,reg sp-tn 0)))

(defmacro popm (reg)
  `(progn
     (loadw ,reg sp-tn 0)
     (inst dec sp-tn 4)))


;;; ### Note sleazy use of the argument registers without allocating them.
;;; This allows expressions to be targeted to the argument registers when error
;;; checking is going on, but causes problems with parallel assignment
;;; semantics.  For error2, we have to start using the stack as a temporary.
;;;
(defmacro error-call (error-code &rest values)
  (once-only ((n-error-code error-code))
    `(progn
       ,@(case (length values)
	   (0
	    '((inst miscopx 'clc::error0)))
	   (1
	    `((inst lr (second register-argument-tns) ,(first values))
	      (inst miscopx 'clc::error1)))
	   (2
	    `((pushm ,(second values))
	      (inst lr (second register-argument-tns) ,(first values))
	      (popm (third register-argument-tns))
	      (inst miscopx 'clc::error2)))
	   (t
	    (error "Can't use Error-Call with ~D values." (length values))))
       ;; Always do a 32bit load so that we can NOTE-THIS-LOCATION.
       (inst cal (first register-argument-tns) zero-tn ,n-error-code))))


(defmacro generate-error-code (vop error-code &rest values)
  "Generate-Error-Code VOP Error-code Value*
  Emit code for an error with the specified Error-Code and context Values.
  VOP is used for source context and lifetime information."
  (once-only ((n-vop vop))
    `(unassemble
       (assemble-elsewhere (vop-node ,n-vop)
	 (let ((start-lab (gen-label)))
	   (emit-label start-lab)
	   (error-call ,error-code ,@values)
	   (note-this-location ,n-vop :internal-error)
	   start-lab)))))