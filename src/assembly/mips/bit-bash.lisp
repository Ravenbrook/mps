;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/mips/bit-bash.lisp,v 1.16 2003/08/03 11:27:51 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/assembly/mips/bit-bash.lisp,v 1.16 2003/08/03 11:27:51 gerd Exp $
;;;
;;; Stuff to implement bit bashing.
;;;
;;; Written by William Lott.
;;;

(in-package "MIPS")


;;;; Blitting macros.  Used only at assemble time.

#+assembler
(eval-when (compile eval)

;;; The main dispatch.  Assumes that the following TNs are bound:
;;;   dst, dst-offset  --  where to put the stuff.
;;;   src, src-offset  --  where to get the stuff.
;;;   dst-bit-offset, src-bit-offset
;;;   length  -- the number of bits to move.
;;;   temp1, temp2 -- random descriptor temp
;;;   ntemp1, ntemp2, ntemp3  --  random non-descriptor temps.

(defmacro do-copy ()
  '(let ((wide-copy (gen-label))
	 (copy-left (gen-label))
	 (copy-right (gen-label)))
     (compute-bit/word-offsets src src-offset src-bit-offset)
     (compute-bit/word-offsets dst dst-offset dst-bit-offset)
     
     (inst addu temp1 dst-bit-offset length)
     (inst subu temp1 (fixnumize 32))
     (inst bgez temp1 wide-copy)
     (inst sltu ntemp1 src dst)

     (narrow-copy)

     (emit-label wide-copy)
     (inst bne ntemp1 copy-right)
     (inst nop)
     (inst bne src dst copy-left)
     (inst sltu ntemp1 src-bit-offset dst-bit-offset)
     (inst bne ntemp1 copy-right)
     (inst nop)
     (inst beq src-bit-offset dst-bit-offset done)
     (inst nop)

     (emit-label copy-left)
     (wide-copy-left)

     (emit-label copy-right)
     (wide-copy-right)))

(defmacro compute-bit/word-offsets (ptr offset bit)
  `(progn
     (inst and ,bit ,offset (fixnumize 31))
     (inst sra ntemp1 ,offset 7)
     (inst sll ntemp1 2)
     (inst addu ,ptr ntemp1)))

     
(defmacro narrow-copy ()
  '(let ((aligned (gen-label))
	 (only-one-src-word (gen-label))
	 (merge-and-write (gen-label)))
     (inst beq src-bit-offset dst-bit-offset aligned)
     (inst lw ntemp1 src)

     ;; It's not aligned, so get the source bits and align them.
     (inst sra ntemp2 src-bit-offset 2)
     (inst add temp1 src-bit-offset length)
     (inst sub temp1 (fixnumize 32))
     (inst blez temp1 only-one-src-word)
     (inst srl ntemp1 ntemp2)

     (inst lw ntemp3 src 4)
     (inst subu ntemp2 zero-tn ntemp2)
     (inst sll ntemp3 ntemp2)
     (inst or ntemp1 ntemp3)

     (emit-label only-one-src-word)
     (inst li ntemp3 (make-fixup "bit_bash_low_masks" :foreign))
     (inst addu ntemp3 length)
     (inst lw ntemp3 ntemp3)
     (inst sra ntemp2 dst-bit-offset 2)
     (inst and ntemp1 ntemp3)
     (inst sll ntemp1 ntemp2)
     (inst b merge-and-write)
     (inst sll ntemp3 ntemp2)

     (emit-label aligned)
     (inst li ntemp3 (make-fixup "bit_bash_low_masks" :foreign))
     (inst addu ntemp3 length)
     (inst lw ntemp3 ntemp3)
     (inst sra ntemp2 dst-bit-offset 2)
     (inst sll ntemp3 ntemp2)
     (inst and ntemp1 ntemp3)

     (emit-label merge-and-write)
     ;; ntemp1 has the bits we need to deposit, and ntemp3 has the mask.
     ;; Both are aligned with dst.
     (inst lw ntemp2 dst)
     (inst nor ntemp3 ntemp3 zero-tn)
     (inst and ntemp2 ntemp3)
     (inst or ntemp1 ntemp2)
     (inst b done)
     (inst sw ntemp1 dst)))


(defmacro wide-copy-left ()
  '(let ((aligned (gen-label)))
     ;; Check to see if they are aligned.
     (inst beq src-bit-offset dst-bit-offset aligned)

     ;; Calc src-shift
     (inst subu src-shift dst-offset src-offset)
     (inst and src-shift (fixnumize 31))

     (macrolet
	 ((middle (&rest before-last-inst)
	    `(progn
	       (inst lw ntemp1 src)
	       (inst lw ntemp2 src 4)
	       (inst addu src 4)
	       (inst sra ntemp3 src-shift 2)
	       (inst sll ntemp2 ntemp2 ntemp3)
	       (inst subu ntemp3 zero-tn ntemp3)
	       (inst srl ntemp1 ntemp1 ntemp3)
	       ,@before-last-inst
	       (inst or ntemp1 ntemp2)))
	  (get-src (where)
	     (ecase where
	       (:left
		'(let ((dont-load (gen-label))
		       (done-load (gen-label)))
		   (inst sltu ntemp1 src-bit-offset dst-bit-offset)
		   (inst bne ntemp1 dont-load)
		   (middle (inst b done-load))
		   (emit-label dont-load)
		   (inst sra ntemp3 src-shift 2)
		   (inst sll ntemp1 ntemp3)
		   (emit-label done-load)))
	       (:middle '(middle))
	       (:right
		'(let ((dont-load (gen-label))
		       (done-load (gen-label)))
		   (inst sltu ntemp1 src-shift final-bits)
		   (inst beq ntemp1 dont-load)
		   (middle (inst b done-load))
		   (emit-label dont-load)
		   (inst sra ntemp3 src-shift 2)
		   (inst subu ntemp3 zero-tn ntemp3)
		   (inst srl ntemp1 ntemp3)
		   (emit-label done-load))))))
       (wide-copy-left-aux))

     (macrolet
	 ((get-src (where)
	    (declare (ignore where))
	    '(progn
	       (inst lw ntemp1 src)
	       (inst addu src 4))))
       (emit-label aligned)
       (wide-copy-left-aux))))

(defmacro wide-copy-left-aux ()
  '(let ((left-aligned (gen-label))
	 (loop (gen-label))
	 (check-right (gen-label))
	 (final-bits temp1)
	 (interior temp2))
     (inst beq dst-bit-offset left-aligned)
     (inst nop)
     
     (get-src :left)
     (inst li ntemp3 (make-fixup "bit_bash_low_masks" :foreign))
     (inst addu ntemp3 dst-bit-offset)
     (inst lw ntemp3 ntemp3)
     (inst lw ntemp2 dst)
     (inst addu dst 4)
     (inst and ntemp2 ntemp3)
     (inst nor ntemp3 ntemp3 zero-tn)
     (inst and ntemp1 ntemp3)
     (inst or ntemp2 ntemp1)
     (inst sw ntemp2 dst -4)

     (emit-label left-aligned)

     (inst addu final-bits length dst-bit-offset)
     (inst and final-bits (fixnumize 31))
     (inst subu ntemp1 length final-bits)
     (inst srl ntemp1 7)
     (inst beq ntemp1 check-right)
     (inst sll interior ntemp1 2)

     (emit-label loop)
     (get-src :middle)
     (inst sw ntemp1 dst)
     (check-for-interrupts)
     (inst subu interior 4)
     (inst bgtz interior loop)
     (inst addu dst 4)

     (emit-label check-right)
     (inst beq final-bits done)
     (inst nop)

     (get-src :right)
     (inst li ntemp3 (make-fixup "bit_bash_low_masks" :foreign))
     (inst addu ntemp3 final-bits)
     (inst lw ntemp3 ntemp3)
     (inst lw ntemp2 dst)
     (inst and ntemp1 ntemp3)
     (inst nor ntemp3 zero-tn ntemp3)
     (inst and ntemp2 ntemp3)
     (inst or ntemp1 ntemp2)
     (inst b done)
     (inst sw ntemp1 dst)))


(defmacro wide-copy-right ()
  '(let ((aligned (gen-label))
	 (final-bits temp1))
     ;; Compute final-bits, the number of bits in the final dst word.
     (inst addu ntemp3 dst-bit-offset length)
     (inst and final-bits ntemp3 (fixnumize 31))

     ;; Increase src and dst so they point to the end of the buffers instead
     ;; of the beginning.
     ;; 
     (inst srl ntemp3 7)
     (inst sll ntemp3 2)
     (inst addu dst ntemp3)

     (inst addu ntemp3 src-bit-offset length)
     (inst subu ntemp3 final-bits)
     (inst srl ntemp3 7)
     (inst sll ntemp3 2)
     (inst add src ntemp3)

     ;; Check to see if they are aligned.
     (inst beq src-bit-offset dst-bit-offset aligned)

     ;; Calc src-shift
     (inst subu src-shift dst-bit-offset src-bit-offset)
     (inst and src-shift (fixnumize 31))

     (macrolet
	 ((merge (&rest before-last-inst)
	    `(progn
	       (inst sra ntemp3 src-shift 2)
	       (inst sll ntemp2 ntemp3)
	       (inst subu ntemp3 zero-tn ntemp3)
	       (inst srl ntemp1 ntemp3)
	       ,@before-last-inst
	       (inst or ntemp1 ntemp2)))
	  (get-src (where)
	     (ecase where
	       (:left
		'(let ((dont-load (gen-label))
		       (done-load (gen-label)))
		   (inst sltu ntemp1 src-bit-offset dst-bit-offset)
		   (inst bne ntemp1 dont-load)
		   (inst lw ntemp2 src 4)
		   (inst lw ntemp1 src)
		   (merge (inst b done-load))
		   (emit-label dont-load)
		   (inst sra ntemp3 src-shift 2)
		   (inst sll ntemp1 ntemp2 ntemp3)
		   (emit-label done-load)))
	       (:middle
		'(progn
		   (inst lw ntemp1 src)
		   (inst lw ntemp2 src 4)
		   (merge)
		   (inst subu src 4)))
	       (:right
		'(let ((dont-load (gen-label))
		       (done-load (gen-label)))
		   (inst sltu ntemp1 src-shift final-bits)
		   (inst beq ntemp1 dont-load)
		   (inst lw ntemp1 src)
		   (inst lw ntemp2 src 4)
		   (merge (inst b done-load))
		   (emit-label dont-load)
		   (inst sra ntemp3 src-shift 2)
		   (inst subu ntemp3 zero-tn ntemp3)
		   (inst srl ntemp1 ntemp3)
		   (emit-label done-load))))))
       (wide-copy-right-aux)
       (inst b done)
       (inst nop))

     (macrolet
	 ((get-src (where)
	    `(progn
	       (inst lw ntemp1 src)
	       ,@(unless (eq where :right)
		   '((inst subu src 4))))))
       (emit-label aligned)
       (wide-copy-right-aux))))

(defmacro wide-copy-right-aux ()
  '(let ((right-aligned (gen-label))
	 (loop (gen-label))
	 (check-left (gen-label))
	 (interior temp2))
     (inst beq final-bits right-aligned)
     (inst nop)
     
     (get-src :right)
     (inst li ntemp3 (make-fixup "bit_bash_low_masks" :foreign))
     (inst addu ntemp3 final-bits)
     (inst lw ntemp3 ntemp3)
     (inst lw ntemp2 dst)
     (inst and ntemp1 ntemp3)
     (inst nor ntemp3 ntemp3 zero-tn)
     (inst and ntemp2 ntemp3)
     (inst or ntemp2 ntemp1)
     (inst sw ntemp2 dst)

     (emit-label right-aligned)
     (inst subu dst 4)
     (inst subu src 4)

     (inst subu ntemp1 length final-bits)
     (inst srl ntemp1 7)
     (inst beq ntemp1 check-left)
     (inst sll interior ntemp1 2)

     (emit-label loop)
     (get-src :middle)
     (inst sw ntemp1 dst)
     (check-for-interrupts)
     (inst subu interior 4)
     (inst bgtz interior loop)
     (inst subu dst 4)

     (emit-label check-left)
     (inst beq dst-bit-offset done)
     (inst nop)

     (get-src :left)
     (inst li ntemp3 (make-fixup "bit_bash_low_masks" :foreign))
     (inst addu ntemp3 dst-bit-offset)
     (inst lw ntemp3 ntemp3)
     (inst lw ntemp2 dst)
     (inst nop)
     (inst and ntemp2 ntemp3)
     (inst nor ntemp3 zero-tn ntemp3)
     (inst and ntemp1 ntemp3)
     (inst or ntemp1 ntemp2)
     (inst sw ntemp1 dst)))
     
(defmacro check-for-interrupts ()
  nil)

) ; eval-when (compile eval)



;;;; The actual routines.

(define-assembly-routine (copy-to-system-area
			  (:policy :fast-safe)
			  (:translate copy-to-system-area)
			  (:arg-types * tagged-num
				      system-area-pointer tagged-num
				      tagged-num))
			 ((:arg src-arg descriptor-reg a0-offset)
			  (:arg src-offset any-reg a1-offset)
			  (:arg dst sap-reg nl0-offset)
			  (:arg dst-offset any-reg a2-offset)
			  (:arg length any-reg a3-offset)
			  (:res res descriptor-reg null-offset)
			  
			  (:temp src interior-reg lip-offset)
			  (:temp temp1 descriptor-reg a4-offset)
			  (:temp temp2 descriptor-reg a5-offset)
			  (:temp src-shift descriptor-reg cname-offset)
			  (:temp src-bit-offset descriptor-reg lexenv-offset)
			  (:temp dst-bit-offset descriptor-reg l0-offset)
			  (:temp ntemp1 non-descriptor-reg nl1-offset)
			  (:temp ntemp2 non-descriptor-reg nl2-offset)
			  (:temp ntemp3 non-descriptor-reg nl3-offset)
			  (:temp retaddr non-descriptor-reg nargs-offset))

  (progn res) ; don't complain that it's unused.

  ;; Save the return address.
  (inst subu retaddr lip-tn code-tn)

  (inst subu src src-arg vm:other-pointer-type)
  (inst and ntemp1 dst 3)
  (inst xor dst ntemp1)
  (inst sll ntemp1 5)
  (inst addu dst-offset ntemp1)
  (do-copy)
  done

  ;; Restore the return address.
  (inst addu lip-tn retaddr code-tn))

(define-assembly-routine (copy-from-system-area
			  (:policy :fast-safe)
			  (:translate copy-from-system-area)
			  (:arg-types system-area-pointer tagged-num
				      * tagged-num tagged-num))
			 ((:arg src sap-reg nl0-offset)
			  (:arg src-offset any-reg a0-offset)
			  (:arg dst-arg descriptor-reg a1-offset)
			  (:arg dst-offset any-reg a2-offset)
			  (:arg length any-reg a3-offset)
			  (:res res descriptor-reg null-offset)
			  
			  (:temp dst interior-reg lip-offset)
			  (:temp temp1 descriptor-reg a4-offset)
			  (:temp temp2 descriptor-reg a5-offset)
			  (:temp src-shift descriptor-reg cname-offset)
			  (:temp src-bit-offset descriptor-reg lexenv-offset)
			  (:temp dst-bit-offset descriptor-reg l0-offset)
			  (:temp ntemp1 non-descriptor-reg nl1-offset)
			  (:temp ntemp2 non-descriptor-reg nl2-offset)
			  (:temp ntemp3 non-descriptor-reg nl3-offset)
			  (:temp retaddr non-descriptor-reg nargs-offset))
  (progn res) ; don't complain that it's unused.

  ;; Save the return address.
  (inst sub retaddr lip-tn code-tn)

  (inst and ntemp1 src 3)
  (inst xor src ntemp1)
  (inst sll ntemp1 5)
  (inst addu src-offset ntemp1)
  (inst subu dst dst-arg vm:other-pointer-type)
  (do-copy)
  done

  ;; Restore the return address.
  (inst add lip-tn retaddr code-tn))

(define-assembly-routine (system-area-copy
			  (:policy :fast-safe)
			  (:translate system-area-copy)
			  (:arg-types system-area-pointer tagged-num
				      system-area-pointer tagged-num
				      tagged-num))
			 ((:arg src sap-reg nl1-offset)
			  (:arg src-offset any-reg a0-offset)
			  (:arg dst sap-reg nl0-offset)
			  (:arg dst-offset any-reg a1-offset)
			  (:arg length any-reg a2-offset)
			  (:res res descriptor-reg null-offset)
			  
			  (:temp temp1 descriptor-reg a4-offset)
			  (:temp temp2 descriptor-reg a5-offset)
			  (:temp src-shift descriptor-reg cname-offset)
			  (:temp src-bit-offset descriptor-reg lexenv-offset)
			  (:temp dst-bit-offset descriptor-reg l0-offset)
			  (:temp ntemp1 non-descriptor-reg nl2-offset)
			  (:temp ntemp2 non-descriptor-reg nl3-offset)
			  (:temp ntemp3 non-descriptor-reg nl4-offset))
  (progn res) ; don't complain that it's unused.

  (inst and ntemp1 src 3)
  (inst xor src ntemp1)
  (inst sll ntemp1 5)
  (inst addu src-offset ntemp1)
  (inst and ntemp1 dst 3)
  (inst xor dst ntemp1)
  (inst sll ntemp1 5)
  (inst addu dst-offset ntemp1)
  (do-copy)
  done)

(define-assembly-routine (bit-bash-copy
			  (:policy :fast-safe)
			  (:translate bit-bash-copy)
			  (:arg-types * tagged-num * tagged-num tagged-num))
			 ((:arg src-arg descriptor-reg a0-offset)
			  (:arg src-offset any-reg a1-offset)
			  (:arg dst-arg descriptor-reg a2-offset)
			  (:arg dst-offset any-reg a3-offset)
			  (:arg length any-reg a4-offset)
			  (:res res descriptor-reg null-offset)
			  
			  (:temp src non-descriptor-reg nl0-offset)
			  (:temp dst non-descriptor-reg nl1-offset)
			  (:temp temp1 descriptor-reg a5-offset)
			  (:temp temp2 descriptor-reg l0-offset)
			  (:temp src-shift descriptor-reg cname-offset)
			  (:temp src-bit-offset descriptor-reg lexenv-offset)
			  (:temp dst-bit-offset descriptor-reg l1-offset)
			  (:temp ntemp1 non-descriptor-reg nl2-offset)
			  (:temp ntemp2 non-descriptor-reg nl3-offset)
			  (:temp ntemp3 non-descriptor-reg nl4-offset))
  (progn res) ; don't complain that it's unused.

  (let ((done (gen-label)))
    (pseudo-atomic (ntemp1)
      (inst subu src src-arg vm:other-pointer-type)
      (inst subu dst dst-arg vm:other-pointer-type)
      (macrolet
	  ((check-for-interrupts ()
	     '(let ((label (gen-label)))
		(inst and ntemp1 flags-tn (ash 1 interrupted-flag))
		(inst beq ntemp1 label)
		(inst nop)
		(inst subu src src-arg)
		(inst subu dst dst-arg)
		(inst and flags-tn (logxor (ash 1 atomic-flag) #Xffff))
		(inst break vm:pending-interrupt-trap)
		(inst and flags-tn (logxor (ash 1 interrupted-flag) #Xffff))
		(inst or flags-tn flags-tn (ash 1 atomic-flag))
		(inst addu src src-arg)
		(inst addu dst dst-arg)
		(emit-label label))))
	(do-copy))
      (emit-label done))))
