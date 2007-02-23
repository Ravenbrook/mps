;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    Assembler instruction definitions for the IBM RT PC.
;;;
(in-package 'c)

(def-instruction-format R 2 ; Instruction format "R"
  (:unsigned 8 :instruction-constant) ; OP
  (:unsigned 4 :register) ; R2
  (:unsigned 4 :register)) ; R3

(def-instruction-format R1 2 ; Instruction format "R"
  (:unsigned 8 :instruction-constant) ; OP
  (:unsigned 4 :enumeration '((:pz . #b1000)
			      (:lt . #b1001)
			      (:eq . #b1010)
			      (:gt . #b1011)
			      (:c0 . #b1100)
			      (:ov . #b1110)
			      (:tb . #b1111))) ; Condition code
  (:unsigned 4 :register)) ; R3

(def-instruction-format R2 2 ; Instruction format "R"
  (:unsigned 8 :instruction-constant) ; OP
  (:unsigned 4 :register) ; R2
  (:signed 4 :immediate)) ; R3

(def-instruction-format DS 2 ; Instruction format "D-Short"
  (:unsigned 4 :instruction-constant) ; OP
  (:signed 4 :immediate) ; I
  (:unsigned 4 :register) ; R2
  (:unsigned 4 :register)) ; R3

(def-instruction-format D 4 ; Instruction format "D"
  (:unsigned 8 :instruction-constant) ; OP
  (:unsigned 4 :register) ; R2
  (:unsigned 4 :register) ; R3
  (:signed 16 :immediate)) ; I

(def-instruction-format Dr2z 4 ; Instruction format "D" with R2 0 (ci...)
  (:unsigned 8 :instruction-constant) ; OP
  (:unsigned 4 :constant 0) ; R2
  (:unsigned 4 :register) ; R3
  (:signed 16 :immediate)) ; I

(def-instruction-format X 2 ; Instruction format "X"
  (:unsigned 4 :instruction-constant)
  (:unsigned 4 :register) ; 
  (:unsigned 4 :register) ; R2
  (:unsigned 4 :register)) ; R3

(def-instruction-format x0 2 ; Special format for the LR pseudo-instruction
  (:unsigned 4 :instruction-constant)
  (:unsigned 4 :register)
  (:unsigned 4 :register)
  (:unsigned 4 :constant 0))

(def-instruction-format BM 4 ;Instruction format for Miscop branches
  (:unsigned 8 :instruction-constant)
  (:signed 24 :fixup :miscop))

(def-instruction-format JI 2 ; Instruction format "JI"
  (:unsigned 5 :instruction-constant) ; OP
  (:unsigned 3 :enumeration '((:pz . #b000)
			      (:lt . #b001)
			      (:eq . #b010)
			      (:gt . #b011)
			      (:c0 . #b100)
			      (:ov . #b110)
			      (:tb . #b111))) ; Condition code
  (:signed 8 :branch #'(lambda (x) (ash x -1)))) ; J1

(def-instruction-format BI 4 ; Instruction format "BI"
  (:unsigned 8 :instruction-constant) ; OP
  (:unsigned 4 :enumeration '((:pz . #b1000)
			      (:lt . #b1001)
			      (:eq . #b1010)
			      (:gt . #b1011)
			      (:c0 . #b1100)
			      (:ov . #b1110)
			      (:tb . #b1111))) ; Condition code
  (:signed 20 :branch #'(lambda (x) (ash x -1)))) ; B1

; Instruction format "BI" with register arg (bal,...)

(def-instruction-format BIR 4
  (:unsigned 8 :instruction-constant) ; OP
  (:unsigned 4 :register)
  (:signed 20 :branch #'(lambda (x) (ash x -1)))) ; B1


(def-instruction-format BA 4
  (:unsigned 8 :instruction-constant)
  (:signed 24 :branch #'(lambda (x) (ash x -1))))

(def-instruction-format SR 2
  (:unsigned 12 :instruction-constant)
  (:unsigned 4 :register))

(def-instruction-format SN 2
  (:unsigned 12 :instruction-constant)
  (:signed 4 :immediate))

;;; Storage Access instructions:

(def-instruction lcs  ds #x04)
(def-instruction lc   d  #xCE)
(def-instruction lhas ds #x05)
(def-instruction lha  d  #xCA)
(def-instruction lhs  r  #xEB)
(def-instruction lh   d  #xDA)
(def-instruction ls   ds #x07)
(def-instruction l    d  #xCD)
(def-instruction lm   d  #xC9)
(def-instruction tsh  d  #xCF)
(def-instruction stcs ds #x01)
(def-instruction stc  d  #xDE)
(def-instruction sths ds #x02)
(def-instruction sth  d  #xDC)
(def-instruction sts  ds #x03)
(def-instruction st   d  #xDD)
(def-instruction stm  d  #xD9)

;;; Address Computation instructions:

(def-instruction cal  d  #xC8)
(def-instruction cal16 d #xC2)
(def-instruction cau  d  #xD8)
(def-instruction cas  x  #x06)
(def-instruction lr   x0 #x06)
(def-instruction ca16 r  #xF3)
(def-instruction inc  r2 #x91)
(def-instruction dec  r2 #x93)
(def-instruction lis  r2 #xA4)

;;; Branching instructions:

(def-instruction bala-inst ba #x8A)
(def-instruction balax-inst ba #x8B)
(def-instruction miscop bm #x8A)
(def-instruction miscopx bm #x8B)
(def-instruction bali-inst bir #x8C)
(def-instruction balix-inst bir #x8D)
(def-instruction balr r  #xEC)
(def-instruction balrx r #xED)
(def-instruction jb-inst   ji #x01)
(def-instruction bb-inst   bi #x8E)
(def-instruction bbx-inst  bi #x8F)
(def-instruction bbr  r1 #xEE)
(def-instruction bbrx r1 #xEF)
(def-instruction jnb-inst  ji #x00)
(def-instruction bnb-inst  bi #x88)
(def-instruction bnbx-inst bi #x89)
(def-instruction bnbr r1  #xE8)
(def-instruction bnbrx r1  #xe9)

;;; Trap instructions:

(def-instruction ti   d  #xCC)
(def-instruction tgte r  #xBD)
(def-instruction tlt  r  #xBE)

;;; Move and Insert instructions.

(def-instruction mc03 r  #xF9)
(def-instruction mc13 r  #xFA)
(def-instruction mc23 r  #xFB)
(def-instruction mc33 r  #xFC)
(def-instruction mc30 r  #xFD)
(def-instruction mc31 r  #xFE)
(def-instruction mc32 r  #xFF)
(def-instruction mftb r  #xBC)
(def-instruction mftbil r2 #x9D)
(def-instruction mftbiu r2 #x9C)
(def-instruction mttb r  #xBF)
(def-instruction mttbil r2 #x9F)
(def-instruction mttbiu r2 #x9E)

;;; Arithmetic instructions.

(def-instruction a    r  #xE1)
(def-instruction ae   r  #xF1)
(def-instruction aei  d  #xD1)
(def-instruction ai   d  #xC1)
(def-instruction ais  r2 #x90)
(def-instruction abs  r  #xE0)
(def-instruction onec r  #xF4)
(def-instruction twoc r  #xE4)
(def-instruction c    r  #xB4)
(def-instruction cis  r2 #x94)
(def-instruction ci   dr2z #xD4)
(def-instruction cl   r  #xB3)
(def-instruction cli  dr2z #xD3)
(def-instruction exts r  #xB1)
(def-instruction s    r  #xE2)
(def-instruction sf   r  #xB2)
(def-instruction se   r  #xF2)
(def-instruction sfi  d  #xD2)
(def-instruction sis  r2 #x92)
(def-instruction d    r  #xB6)
(def-instruction m    r  #xE6)

;;; Logical instructions.

(def-instruction clrbl r2 #x99)
(def-instruction clrbu r2 #x98)
(def-instruction setbl r2 #x9B)
(def-instruction setbu r2 #x9A)
(def-instruction n     r  #xE5)
(def-instruction nilz  d  #xC5)
(def-instruction nilo  d  #xC6)
(def-instruction niuz  d  #xD5)
(def-instruction niuo  d  #xD6)
(def-instruction o     r  #xE3)
(def-instruction oil   d  #xC4)
(def-instruction oiu   d  #xC3)
(def-instruction x     r  #xE7)
(def-instruction xil   d  #xC7)
(def-instruction xiu   d  #xD7)
(def-instruction clz   r  #xF5)

;;; Shifting instructions.

(def-instruction sar   r  #xB0)
(def-instruction sari  r2 #xA0)
(def-instruction sari16 r2 #xA1)
(def-instruction sr    r  #xB8)
(def-instruction sri   r2 #xA8)
(def-instruction sri16 r2 #xA9)
(def-instruction srp   r  #xB9)
(def-instruction srpi  r2 #xAC)
(def-instruction srpi16 r2 #xAD)
(def-instruction sl    r  #xBA)
(def-instruction sli   r2 #xAA)
(def-instruction sli16 r2 #xAB)
(def-instruction slp   r  #xBB)
(def-instruction slpi  r2 #xAE)
(def-instruction slpi16 r2 #xAF)

;;; Special Purpose Register Manipulation instructions.

(def-instruction mtmq  sr #xB5A)
(def-instruction mfmq  sr #x96A)
(def-instruction mtcsr sr #xB5F)
(def-instruction mfcsr sr #x96F)
(def-instruction clrcb sn #x95F)
(def-instruction setcb sn #x97F)

;;; Execution Control instructions.

(def-instruction lps   d  #xD0)
(def-instruction svc   d  #xC0)


(def-branch bala (label) label
  (-32768 32767 (bala-inst label)))

(def-branch balax (label) label
  (-32768 32767 (balax-inst label)))

(def-branch bali (pc label) label
  (-32768 32767 (bali-inst pc label)))

(def-branch balix (pc label) label
  (-32768 32767 (balix-inst pc label)))

(def-branch bbx (n label) label
  (-32768 32767 (bbx-inst n label)))

(def-branch bb (n label) label
;  (-128 127 (jb-inst n label))
  (-32768 32767 (bb-inst n label)))

(def-branch bnbx (n label) label
  (-32768 32767 (bnbx-inst n label)))

(def-branch bnb (n label) label
;  (-128 127 (jnb-inst n label))
  (-32768 32767 (bnb-inst n label)))
