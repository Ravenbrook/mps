;;; -*- Mode: Lisp; Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/pmax-disassem.lisp,v 1.19 2001/03/04 20:12:40 pw Exp $")
;;;
;;; **********************************************************************
;;;
;;; A simple dissambler for the MIPS R2000.
;;;
;;; Written by Christopher Hoover.
;;; 

(in-package "MIPS" :use '("LISP"))

(export '(register-name disassemble-code-vector))


;;;; Instruction Layout

;;;
;;; Each instrunction on the MIPS R2000 consists of a single word (32
;;; bits) aligned on a single word boundaray.  There are three
;;; instrunction formats:
;;; 
;;; 	I-Type (Immediate)
;;;
;;; 	3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;; 	1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;;     ---------------------------------------------------------------
;;;	[   op    ] [  rs   ] [  rt   ] [         immediate           ]
;;;     ---------------------------------------------------------------
;;;
;;; 
;;;	J-Type (Jump)
;;;
;;; 	3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;; 	1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;;     ---------------------------------------------------------------
;;;	[   op    ] [                target                           ]
;;;     ---------------------------------------------------------------
;;;
;;;
;;; 	R-Type (Register)
;;;
;;; 	3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
;;; 	1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;;     ---------------------------------------------------------------
;;;	[   op    ] [  rs   ] [  rt   ] [  rd   ] [ shmat ] [  funct  ]
;;;     ---------------------------------------------------------------
;;;
;;; These instructions fall into 5 categories: Load/Store,
;;; Computational, Jump/Branch, Coprocessor, and Special.
;;;


;;;; Register Names

(defun register-name (register-number)
  (unless (<= 0 register-number 31)
    (error "Illegal register number!"))
  (svref *register-names* register-number))



;;;; Instruction Type Definition

;;;
;;; These instruction types correspond to the various ways the decoded
;;; instructions are printed which in turn corresponds somewhat to the
;;; way the instructions are decoded.
;;; 

(defvar *mips-instruction-types* (make-hash-table :test #'eq))

(defmacro def-mips-instruction-type (types &body body)
  `(let ((closure #'(lambda (name word stream) ,@body)))
     (dolist (type ',types)
       (when (gethash type *mips-instruction-types*)
	 (warn "Instruction type ~S being redefined" type))
       (setf (gethash type *mips-instruction-types*) closure))))

(defun mips-instruction-type-p (type)
  (not (not (gethash type *mips-instruction-types*))))


;;;; Instruction Types

;;;
;;; Used later for relative branches.
(defvar *current-instruction-number* 0)

(def-mips-instruction-type (:ls-type)
  (let ((rs (ldb (byte 5 21) word))
	(rt (ldb (byte 5 16) word))
	(immed (signed-ldb (byte 16 0) word)))
    (format stream "~16,8T~A~8,8T~A, #x~X(~A)~%"
	    name (register-name rt) immed (register-name rs))))

(def-mips-instruction-type (:si-type)
  (let ((rs (ldb (byte 5 21) word))
	(rt (ldb (byte 5 16) word))
	(immed (signed-ldb (byte 16 0) word)))
    (cond ((and (zerop rs) (or (string= name "ADDI") (string= name "ADDIU")))
	   (format stream "~16,8TLOADI~8,8T~A, #x~X~%"
		   (register-name rt) immed))
	  ((and (= rs null-offset) (string= name "ADDI"))
	   ;; Major hack ...
	   (format stream "~16,8T~A~8,8T~A, ~A, #x~X~48,8T; ~S~%"
		   name (register-name rt) (register-name rs) immed
		   (vm:offset-static-symbol immed)))
	  (t
	   (format stream "~16,8T~A~8,8T~A, ~A, #x~X~%"
		   name (register-name rt) (register-name rs) immed)))))

(def-mips-instruction-type (:ui-type)
  (let ((rs (ldb (byte 5 21) word))
	(rt (ldb (byte 5 16) word))
	(immed (ldb (byte 16 0) word)))
    (cond ((and (zerop rs) (or (string= name "ORI") (string= name "XORI")))
	   (format stream "~16,8TLOADI~8,8T~A, #x~X~%"
		   (register-name rt) immed))
	  (t
	   (format stream "~16,8T~A~8,8T~A, ~A, #x~X~%"
		   name (register-name rt) (register-name rs) immed)))))

(def-mips-instruction-type (:lui-type)
  (let ((rt (ldb (byte 5 16) word))
	(immed (ldb (byte 16 0) word)))
    (format stream "~16,8T~A~8,8T~A, #x~X~%" name (register-name rt) immed)))

(def-mips-instruction-type (:j-type)
  (let ((target (ldb (byte 26 0) word)))
    (format stream "~16,8T~A~8,8Ttarget = ~D~%" name target)))

(def-mips-instruction-type (:jr-type)
  (let ((rs (ldb (byte 5 21) word)))
    (format stream "~16,8T~A~8,8T~A~%" name (register-name rs))))

(def-mips-instruction-type (:jalr-type)
  (let ((rs (ldb (byte 5 21) word))
	(rd (ldb (byte 5 11) word)))
    (format stream "~16,8T~A~8,8T~A, ~A~%" name
	    (register-name rd) (register-name rs))))

(defun branch-target (offset)
  (+ *current-instruction-number* offset 1))

(def-mips-instruction-type (:branch-type)
  (let ((rs (ldb (byte 5 21) word))
	(offset (signed-ldb (byte 16 0) word)))
    (format stream "~16,8T~A~8,8T~A, ~D~%" name
	    (register-name rs) (branch-target offset))))

(def-mips-instruction-type (:branch2-type)
  (let* ((rs (ldb (byte 5 21) word))
	 (rt (ldb (byte 5 16) word))
	 (offset (signed-ldb (byte 16 0) word))
	 (target (branch-target offset)))
    (cond ((and (zerop rs) (zerop rt) (string= name "BEQ"))
	   (format stream "~16,8TB~8,8T~D~%" target))
	  (t
	   (format stream "~16,8T~A~8,8T~A, ~A, ~D~%" name
		   (register-name rs) (register-name rt) target)))))

(def-mips-instruction-type (:r3-type)
  (let ((rs (ldb (byte 5 21) word))
	(rt (ldb (byte 5 16) word))
	(rd (ldb (byte 5 11) word)))
    (cond ((zerop rd)
	   ;; Hack for NOP
	   (format stream "~16,8TNOP~%"))
	  ((and (zerop rt) (or (string= name "OR") (string= name "ADDU")))
	   ;; Hack for MOVE
	   (format stream "~16,8TMOVE~8,8T~A, ~A~%"
		   (register-name rd) (register-name rs)))
	  (t
	   (format stream "~16,8T~A~8,8T~A, ~A, ~A~%"
		   name (register-name rd) (register-name rs)
		   (register-name rt))))))

(def-mips-instruction-type (:mf-type)
  (let ((rd (ldb (byte 5 11) word)))
    (format stream "~16,8T~A~8,8T~A~%" name (register-name rd))))

(def-mips-instruction-type (:mt-type)
  (let ((rs (ldb (byte 5 21) word)))
    (format stream "~16,8T~A~8,8T~A~%" name (register-name rs))))

(def-mips-instruction-type (:mult-type)
  (let ((rs (ldb (byte 5 21) word))
	(rt (ldb (byte 5 16) word)))
    (format stream "~16,8T~A~8,8T~A, ~A~%" name
	    (register-name rs) (register-name rt))))

(def-mips-instruction-type (:shift-type)
  (let ((rt (ldb (byte 5 16) word))
	(rd (ldb (byte 5 11) word))
	(shamt (ldb (byte 5 6) word)))
    ;; Hack for NOP
    (cond ((= word 0)
	   (format stream "~16,8TNOP~%"))
	  (t
	   (format stream "~16,8T~A~8,8T~A, ~A, #x~X~%"
		   name (register-name rd) (register-name rt) shamt)))))

(def-mips-instruction-type (:shiftv-type)
  (let ((rs (ldb (byte 5 21) word))
	(rt (ldb (byte 5 16) word))
	(rd (ldb (byte 5 11) word)))
    (format stream "~16,8T~A~8,8T~A, ~A, ~A~%"
	    name (register-name rd) (register-name rt) (register-name rs))))

(def-mips-instruction-type (:break-type)
  (let ((code (ldb (byte 10 16) word))) ; The entire field is (byte 20 6)
    (format stream "~16,8T~A~8,8T#x~X~%" name code)))

(def-mips-instruction-type (:syscall-type)
  (declare (ignore word))
  (format stream "~16,8T~A~%" name))

(def-mips-instruction-type (:cop0-type :cop1-type :cop2-type :cop3-type)
  (format stream "~16,8T~A~8,8T(#x~X)~%" name word))


;;;; Instruction Definition

(defstruct (mips-instruction
	    (:constructor make-mips-instruction (name type))
	    (:print-function %print-mips-instruction))
  (name "" :type simple-string)
  type)

(defun %print-mips-instruction (instr stream depth)
  (declare (ignore depth))
  (format stream "#<MIPS instruction ~A>" (mips-instruction-name instr)))


(defconstant mips-instruction-bits 6)

(defvar *mips-instructions*
  (make-array (ash 1 mips-instruction-bits) :initial-element nil))
(declaim (type simple-vector *mips-instructions*))

(defmacro def-mips-instr (name op-code type)
  `(let ((name ,name)
	 (type ,type))
     (unless (mips-instruction-type-p type)
       (warn "~S is an unknown instruction type" type))
     (setf (svref *mips-instructions* ,op-code)
	   (make-mips-instruction name ,type))
     name))


(defconstant mips-special-instruction-bits 6)

(defvar *mips-special-instructions*
  (make-array (ash 1 mips-special-instruction-bits) :initial-element nil))
(declaim (type simple-vector *mips-special-instructions*))

(defmacro def-mips-special-instr (name op-code type)
  `(let ((name ,name)
	 (type ,type))
     (unless (mips-instruction-type-p type)
       (warn "~S is an unknown instruction type" type))
     (setf (svref *mips-special-instructions* ,op-code)
	   (make-mips-instruction name ,type))
     name))


(defconstant mips-bcond-instruction-bits 6)

(defvar *mips-bcond-instructions*
  (make-array (ash 1 mips-bcond-instruction-bits) :initial-element nil))
(declaim (type simple-vector *mips-bcond-instructions*))

(defmacro def-mips-bcond-instr (name op-code type)
  `(let ((name ,name)
	 (type ,type))
     (unless (mips-instruction-type-p type)
       (warn "~S is an unknown instruction type" type))
     (setf (svref *mips-bcond-instructions* ,op-code)
	   (make-mips-instruction name ,type))
     name))


;;;; Normal Opcodes

(def-mips-instr "J" #b000010 :j-type)
(def-mips-instr "JAL" #b000011 :j-type)
(def-mips-instr "BEQ" #b000100 :branch2-type)
(def-mips-instr "BNE" #b000101 :branch2-type)
(def-mips-instr "BLEZ" #b000110 :branch-type)
(def-mips-instr "BGTZ" #b000111 :branch-type)

(def-mips-instr "ADDI" #b001000 :si-type)
(def-mips-instr "ADDIU" #b001001 :si-type)
(def-mips-instr "SLTI" #b001010 :si-type)
(def-mips-instr "SLTIU" #b001011 :si-type)
(def-mips-instr "ANDI" #b001100 :ui-type)
(def-mips-instr "ORI" #b001101 :ui-type)
(def-mips-instr "XORI" #b001110 :ui-type)
(def-mips-instr "LUI" #b001111 :lui-type)

(def-mips-instr "COP0" #b010000 :cop0-type)
(def-mips-instr "COP1" #b010001 :cop1-type)
(def-mips-instr "COP2" #b010010 :cop2-type)
(def-mips-instr "COP3" #b010011 :cop3-type)

(def-mips-instr "LB" #b100000 :ls-type)
(def-mips-instr "LH" #b100001 :ls-type)
(def-mips-instr "LWL" #b100010 :ls-type)
(def-mips-instr "LW" #b100011 :ls-type)
(def-mips-instr "LBU" #b100100 :ls-type)
(def-mips-instr "LHU" #b100101 :ls-type)
(def-mips-instr "LWR" #b100110 :ls-type)

(def-mips-instr "SB" #b101000 :ls-type)
(def-mips-instr "SH" #b101001 :ls-type)
(def-mips-instr "SWL" #b101010 :ls-type)
(def-mips-instr "SW" #b101011 :ls-type)
(def-mips-instr "SWR" #b101110 :ls-type)

(def-mips-instr "LWC0" #b110000 :cop0-type)
(def-mips-instr "LWC1" #b110001 :cop1-type)
(def-mips-instr "LWC2" #b110010 :cop2-type)
(def-mips-instr "LWC3" #b110011 :cop3-type)

(def-mips-instr "SWC0" #b111000 :cop0-type)
(def-mips-instr "SWC1" #b111001 :cop1-type)
(def-mips-instr "SWC2" #b111010 :cop2-type)
(def-mips-instr "SWC3" #b111011 :cop3-type)


;;;; SPECIAL Opcodes

(defconstant special-op #b000000)

(def-mips-special-instr "SLL" #b000000 :shift-type)
(def-mips-special-instr "SRL" #b000010 :shift-type)
(def-mips-special-instr "SRA" #b000011 :shift-type)
(def-mips-special-instr "SLLV" #b000100 :shiftv-type)
(def-mips-special-instr "SRLV" #b000110 :shiftv-type)
(def-mips-special-instr "SRAV" #b000111 :shiftv-type)

(def-mips-special-instr "JR" #b001000 :jr-type)
(def-mips-special-instr "JALR" #b001001 :jalr-type)
(def-mips-special-instr "SYSCALL" #b001100 :syscall-type)
(def-mips-special-instr "BREAK" #b001101 :break-type)

(def-mips-special-instr "MFHI" #b010000 :mf-type)
(def-mips-special-instr "MTHI" #b010001 :mt-type)
(def-mips-special-instr "MFLO" #b010010 :mf-type)
(def-mips-special-instr "MTLO" #b010011 :mt-type)

(def-mips-special-instr "MULT" #b011000 :mult-type)
(def-mips-special-instr "MULTU" #b011001 :mult-type)
(def-mips-special-instr "DIV" #b011010 :mult-type)
(def-mips-special-instr "DIVU" #b011011 :mult-type)

(def-mips-special-instr "ADD" #b100000 :r3-type)
(def-mips-special-instr "ADDU" #b100001 :r3-type)
(def-mips-special-instr "SUB" #b100010 :r3-type)
(def-mips-special-instr "SUBU" #b100011 :r3-type)
(def-mips-special-instr "AND" #b100100 :r3-type)
(def-mips-special-instr "OR" #b100101 :r3-type)
(def-mips-special-instr "XOR" #b100110 :r3-type)
(def-mips-special-instr "NOR" #b100111 :r3-type)

(def-mips-special-instr "SLT" #b101010 :r3-type)
(def-mips-special-instr "SLTU" #b101011 :r3-type)


;;;; BCOND Opcodes

(defconstant bcond-op #b000001)

(def-mips-bcond-instr "BLTZ" #b00000 :branch-type)
(def-mips-bcond-instr "BLTZAL" #b00001 :branch-type)

(def-mips-bcond-instr "BLTZAL" #b10000 :branch-type)
(def-mips-bcond-instr "BGEZAL" #b10001 :branch-type)


;;;; Signed-Ldb

(defun signed-ldb (byte-spec integer)
  (let ((unsigned (ldb byte-spec integer))
	(length (byte-size byte-spec)))
    (if (logbitp (1- length) unsigned)
	(- unsigned (ash 1 length))
	unsigned)))


;;;; Instruction Decoding

(defun mips-instruction (word)
  (let* ((opcode (ldb (byte 6 26) word)))
    (cond ((= opcode special-op)
	   (let ((function (ldb (byte 6 0) word)))
	     (svref *mips-special-instructions* function)))
	  ((= opcode bcond-op)
	   (let ((cond (ldb (byte 5 16) word)))
	     (svref *mips-bcond-instructions* cond)))
	  (t
	   (svref *mips-instructions* opcode)))))


;;;; Disassemble-Instruction

(defun disassemble-instruction (word &optional (stream t))
  (let* ((instr (mips-instruction word)))
    (cond (instr
	   (let* ((instr-name (mips-instruction-name instr))
		  (instr-type (mips-instruction-type instr))
		  (closure (gethash instr-type *mips-instruction-types*)))
	     (cond (closure
		    (funcall closure instr-name word stream))
		   (t
		    (format stream "UNKNOWN TYPE (~A/~S/#x~X)~%"
			    instr-name instr-type word)))
	     (values instr-name instr-type)))
	  (t
	   (format stream "~16,8TDATA~8,8T#x~X~%" word)
	   (return-from disassemble-instruction (values nil nil))))))



;;; Dissassemble-Code-Vector

(defconstant delay-slot-instruction-types
  '(:j-type :jr-type :jalr-type :branch-type :branch2-type))

(defun disassemble-code-vector (code-vector length &optional (stream t))
  (do ((i 0 (+ i 4))
       (*current-instruction-number* 0 (1+ *current-instruction-number*))
       (instruction-in-delay-slot-p nil))
      ((>= i length))
    (unless instruction-in-delay-slot-p
      (format stream "~6D:" *current-instruction-number*))
    (multiple-value-bind
	(name type)
	(disassemble-instruction (logior (aref code-vector i)
					 (ash (aref code-vector (+ i 1)) 8)
					 (ash (aref code-vector (+ i 2)) 16)
					 (ash (aref code-vector (+ i 3)) 24))
				 stream)
      (declare (ignore name))
      (cond ((member type delay-slot-instruction-types :test #'eq)
	     (setf instruction-in-delay-slot-p t))
	    (t
	     (setf instruction-in-delay-slot-p nil))))))



;;;; Disassemble-code-sap

(defun disassemble-code-sap (sap length &optional (stream t))
  (do ((*current-instruction-number* 0 (1+ *current-instruction-number*))
       (instruction-in-delay-slot-p nil))
      ((>= *current-instruction-number* length))
    (unless instruction-in-delay-slot-p
      (format stream "~6D:" *current-instruction-number*))
    (multiple-value-bind
	(name type)
	(disassemble-instruction (system:sap-ref-32
				  sap
				  *current-instruction-number*)
				 stream)
      (declare (ignore name))
      (cond ((member type delay-slot-instruction-types :test #'eq)
	     (setf instruction-in-delay-slot-p t))
	    (t
	     (setf instruction-in-delay-slot-p nil))))))


;;;; Disassemble

(defun compile-function-lambda-expr (function)
  (multiple-value-bind
      (lambda closurep name)
      (function-lambda-expression function)
    (declare (ignore name))
    (when closurep
      (error "Cannot compile lexical closure."))
    (compile nil lambda)))

(defun disassemble (object &optional (stream *standard-output*))
  (let* ((function (cond ((or (symbolp object)
			      (and (listp object)
				   (eq (car object) 'lisp:setf)))
			  (let ((temp (fdefinition object)))
			    (if (eval:interpreted-function-p temp)
				(compile-function-lambda-expr temp)
				temp)))
			 ((eval:interpreted-function-p object)
			  (compile-function-lambda-expr object))
			 ((functionp object)
			  object)
			 ((and (listp object)
			       (eq (car object) 'lisp::lambda))
			  (compile nil object))
			 (t
			  (error "Invalid argument to disassemble - ~S"
				 object))))
	 (self (system:%primitive function-self function))
	 (code (di::function-code-header self)))
    (disassemble-code-sap (truly-the system:system-area-pointer
				     (system:%primitive code-instructions
							code))
			  (system:%primitive code-code-size code)
			  stream)))
