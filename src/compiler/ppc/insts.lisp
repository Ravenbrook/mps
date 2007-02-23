;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/insts.lisp,v 1.19 2006/06/30 18:41:24 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the PPC architecture.
;;;
;;; Written by William Lott
;;;

(in-package "PPC")

(use-package "NEW-ASSEM")
(use-package "EXT")
(use-package "C")

(def-assembler-params
    :scheduler-p nil ; t when we trust the scheduler not to "fill delay slots"
  :max-locations 70)



;;;; Constants, types, conversion functions, some disassembler stuff.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (zero zero-offset)
    (null null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
	 (tn-offset tn)
	 (error "~S isn't a register." tn)))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error "~S isn't a floating-point register." tn))
  (tn-offset tn))

(disassem:set-disassem-params :instruction-alignment 32
			      :opcode-column-width 8)

(defvar *disassem-use-lisp-reg-names* t)

(def-vm-support-routine location-number (loc)
  (etypecase loc
    (null)
    (number)
    (label)
    (fixup)
    (tn
     (ecase (sb-name (sc-sb (tn-sc loc)))
       (immediate-constant
	;; Can happen if $ZERO or $NULL are passed in.
	nil)
       (registers
	(unless (zerop (tn-offset loc))
	  (tn-offset loc)))
       (float-registers
	(+ (tn-offset loc) 32))))
    (symbol
     (ecase loc
       (:memory 0)
       (:ccr 64)
       (:xer 65)
       (:lr 66)
       (:ctr 67)
       (:fpscr 68)))))

(defparameter reg-symbols
  (map 'vector
       #'(lambda (name)
	   (cond ((null name) nil)
		 (t (make-symbol (concatenate 'string "$" name)))))
       *register-names*))

(defun get-reg-name (index)
  (if *disassem-use-lisp-reg-names*
      (aref reg-symbols index)
    (format nil "~A~D" 'r index)))

(macrolet
    ((frob (&rest names)
       (let ((results (mapcar #'(lambda (n)
				  (let ((nn (intern (concatenate 'string (string n)
								 "-TYPE"))))
				    `(,(eval nn) ,nn)))
			      names)))
	 `(eval-when (compile load eval)
	   (defconstant header-word-type-alist
	     ',results)))))
  ;; This is the same list as in objdefs.
  (frob bignum
	ratio
	single-float
	double-float
	#+long-float long-float
	#+double-double double-double-float
	complex
	complex-single-float
	complex-double-float
	#+long-float complex-long-float
	#+double-double complex-double-double-float
  
	simple-array
	simple-string
	simple-bit-vector
	simple-vector
	simple-array-unsigned-byte-2
	simple-array-unsigned-byte-4
	simple-array-unsigned-byte-8
	simple-array-unsigned-byte-16
	simple-array-unsigned-byte-32
	simple-array-signed-byte-8
	simple-array-signed-byte-16
	simple-array-signed-byte-30
	simple-array-signed-byte-32
	simple-array-single-float
	simple-array-double-float
	#+long-float simple-array-long-float
	#+double-double simple-array-double-double-float
	simple-array-complex-single-float
	simple-array-complex-double-float
	#+long-float simple-array-complex-long-float
	#+double-double simple-array-complex-double-double-float
	complex-string
	complex-bit-vector
	complex-vector
	complex-array
  
	code-header
	function-header
	closure-header
	funcallable-instance-header
	byte-code-function
	byte-code-closure
	#-double-double dylan-function-header
	closure-function-header
	#-gengc return-pc-header
	#+gengc forwarding-pointer
	value-cell-header
	symbol-header
	base-char
	sap
	unbound-marker
	weak-pointer
	instance-header
	fdefn
	#+(or gengc gencgc) scavenger-hook))

(defvar *note-addis-inst* nil
  "An alist for the disassembler indicating the target register and
value used in an ADDIS instruction.  This is used to make annotations
about function addresses and register values.")

(defvar *pseudo-atomic-set* nil)

(defun handle-ld/st-inst (reg word dstate)
  (let ((ra (ldb (byte 5 16) word))
	(d (ldb (byte 16 0) word)))
    (when (= reg ra)
      (case ra
	(19
	 ;; A referece to a code constant (reg = %CODE)
	 (disassem:note-code-constant d dstate))
	(18
	 ;; A reference to a static symbol or static function (reg =
	 ;; %NULL)
	 (or (disassem:maybe-note-nil-indexed-symbol-slot-ref
	      d dstate)
	     (disassem:maybe-note-static-function d dstate)))))))

(defun handle-addi-inst (reg word dstate)
  (let ((rt (ldb (byte 5 21) word))
	(ra (ldb (byte 5 16) word))
	(d (ldb (byte 16 0) word)))
    (when (= reg ra)
      (let ((addis (assoc ra *note-addis-inst*)))
	(cond
	 (addis
	  ;; RA was used in a ADDIS instruction.  Assume that this is
	  ;; the offset part of the ADDIS instruction for a full
	  ;; 32-bit address or value.  Make a note about this usage as
	  ;; a Lisp assembly routine or a foreign routine.  If note,
	  ;; just note the final value.
	  (let ((addr (+ d (ash (cdr addis) 16))))
	    (or (disassem::note-code-constant-absolute addr dstate)
		(disassem::maybe-note-assembler-routine addr t dstate)
		(disassem::note (format nil "~A = #x~8,'0X"
					(get-reg-name rt) addr)
				dstate)))
	  ;; We're done with this ADDIS/ADDI instruction combination.
	  ;; Remove ADDIS.
	  (setf *note-addis-inst* (delete addis *note-addis-inst*)))
	 ((= ra null-offset)
	  ;; An ADDI rt, $NULL, <n> instruction.  This a
	  ;; reference to a static symbol.
	  (disassem::maybe-note-nil-indexed-object d dstate))
	 ((= ra alloc-offset)
	  ;; addi rt, $alloc, n.  This must be some allocation or
	  ;; pseudo-atomic stuff.  Look at the pseudo-atomic macro to
	  ;; make sure this is right.
	  (cond ((and (= d 4)
		      (= rt alloc-offset)
		      (not *pseudo-atomic-set*))
		 ;; "ADDI $ALLOC, $ALLOC, 4" sets the PA flag
		 (disassem:note "Set pseudo-atomic flag" dstate)
		 (setf *pseudo-atomic-set*
		       (sys:sap+ (disassem:dstate-segment-sap dstate)
				 (disassem:dstate-cur-offs dstate))))
		((and (= d (ldb (byte 16 0) -4))
		      (= rt alloc-offset)
		      *pseudo-atomic-set*)
		 ;; "ADDI $ALLOC, $ALLOC, -4" resets the PA flag
		 (disassem:note "Reset pseudo-atomic flag" dstate)
		 (setf *pseudo-atomic-set* nil))
		((and (= rt alloc-offset)
		      *pseudo-atomic-set*
		      (not (sys:sap= *pseudo-atomic-set*
				     (sys:sap+ (disassem:dstate-segment-sap dstate)
					       (disassem:dstate-cur-offs dstate)))))
		 ;; "ADD $ALLOC, $ALLOC, n" is allocating space
		 (disassem:note (format nil "Allocating ~D bytes" d)
				dstate))))
	 ((and (= ra zero-offset) *pseudo-atomic-set*)
	  ;; "ADDI RT, $ZERO, n" inside a pseudo-atomic is very likely
	  ;; loading up a header word.  Make a note to that effect.
	  (let ((type (second (assoc (logand d #xff) header-word-type-alist)))
		(size (ldb (byte 24 8) d)))
	    (when type
	      (disassem:note (format nil "Header word ~A, size ~D?" type size)
			     dstate)))))))))

(defun handle-and-inst (reg word dstate)
  (let ((rs (ldb (byte 5 21) word))
	(ra (ldb (byte 5 16) word)))
    (when (and (= reg 6)
	       (= rs alloc-offset)
	       (= ra alloc-offset))
      ;; We have "and $ALLOC, $ALLOC, $NL3".  This turns off
      ;; pseudo-atomic.  (See the pseudo-atomic macro.)
      (disassem:note "Reset pseudo-atomic flag" dstate)
      (setf *pseudo-atomic-set* nil))))

(defun update-addis-notes (word)
  ;; If this instruction writes to register that was the target of an
  ;; ADDIS instruction, we should remove it from *note-addis-inst* so
  ;; we don't get confused an print out erroneous notes.
  )

;; Look at the current instruction and see if we can't add some notes
;; about what's happening.
(defun maybe-add-notes (reg dstate)
  (let* ((word (disassem::sap-ref-int (disassem:dstate-segment-sap dstate)
				      (disassem:dstate-cur-offs dstate)
				      vm:word-bytes
				      (disassem::dstate-byte-order dstate)))
	 (opcode (ldb (byte 6 26) word)))
    ;; At this point we should look at the instruction and figure out
    ;; what it is so we can print out some notes.
    ;;
    ;; What are interesting instructions?
    ;;
    ;; o Setting/clearing the pseudo-atomic bit
    ;; o Load/store
    ;; o addis/ori (for loading 32-bit values)

    (cond ((or (= 32 opcode)
	       (= 36 opcode))
	   ;; An LWZ or STW instruction.
	   (handle-ld/st-inst reg word dstate))
	  ((or (= 14 opcode)
	       (= 24 opcode))
	   ;; An ADDI or ORI instruction.
	   (handle-addi-inst reg word dstate))
	  ((and (= 31 opcode)
		(= 28 (ldb (byte 10 1) word)))
	   ;; An AND instruction
	   (handle-and-inst reg word dstate))
	  )
    (update-addis-notes word)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun reg-arg-printer (value stream dstate)
  (declare (type stream stream) (fixnum value))
  (let ((regname (aref reg-symbols value)))
    (princ regname stream)
    (disassem:maybe-note-associated-storage-ref value
						'registers
						regname
						dstate)
    (maybe-add-notes value dstate)))
)					; eval-when

(disassem:define-argument-type reg
  :printer #'reg-arg-printer)

(defparameter float-reg-symbols
  (coerce 
   (loop for n from 0 to 31 collect (make-symbol (format nil "$F~d" n)))
   'vector))

(disassem:define-argument-type fp-reg
  :printer #'(lambda (value stream dstate)
	       (declare (type stream stream) (fixnum value))
	       (let ((regname (aref float-reg-symbols value)))
		 (princ regname stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'float-registers
		  regname
		  dstate))))

(defconstant bo-kind-names #(:bo-dnzf :bo-dnzfp :bo-dzf :bo-dzfp :bo-f :bo-fp nil nil
                             :bo-dnzt :bo-dnztp :bo-dzt :bo-dztp :bo-t :bo-tp nil nil
                             :bo-dnz :bo-dnzp :bo-dz :bo-dzp :bo-u nil nil nil
                             nil nil nil nil nil nil nil nil))

(disassem:define-argument-type bo-field
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate)
                        (type stream stream)
                        (type fixnum value))
               (princ (svref bo-kind-names value) stream)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun valid-bo-encoding (enc)
  (or (if (integerp enc)
        (and (= enc (logand #x1f enc))
             (not (null (svref bo-kind-names enc)))
             enc)
        (and enc (position enc bo-kind-names)))
      (error "Invalid BO field spec: ~s" enc)))
)


(defconstant cr-bit-names #(:lt :gt :eq :so))
(defconstant cr-bit-inverse-names #(:ge :le :ne :ns))

(defconstant cr-field-names #(:cr0 :cr1 :cr2 :cr3 :cr4 :cr5 :cr6 :cr7))

(defun valid-cr-bit-encoding (enc &optional error-p)
  (or (if (integerp enc)
        (and (= enc (logand 3 enc))
             enc))
      (position enc cr-bit-names)
      (if error-p (error "Invalid condition bit specifier : ~s" enc))))

(defun valid-cr-field-encoding (enc)
  (let* ((field (if (integerp enc) 
                  (and (= enc (logand #x7 enc)))
                  (position enc cr-field-names))))
    (if field
      (ash field 2)
      (error "Invalid condition register field specifier : ~s" enc))))
                
(defun valid-bi-encoding (enc)
  (or
   (if (atom enc) 
     (if (integerp enc) 
       (and (= enc (logand 31 enc)) enc)
       (position enc cr-bit-names))
     (+ (valid-cr-field-encoding (car enc))
        (valid-cr-bit-encoding (cadr enc))))
   (error "Invalid BI field spec : ~s" enc)))

(disassem:define-argument-type bi-field
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate)
                        (type stream stream)
                        (type (unsigned-byte 5) value))
               (let* ((bitname (svref cr-bit-names (logand 3 value)))
                      (crfield (ash value -2)))
                 (declare (type (unsigned-byte 3) crfield))
                 (if (= crfield 0)
                   (princ bitname stream)
                   (princ (list (svref cr-field-names crfield) bitname) stream)))))

(disassem:define-argument-type crf
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate)
                        (type stream stream)
                        (type (unsigned-byte 3) value))
               (princ (svref cr-field-names value) stream)))

(disassem:define-argument-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
		 (declare (type (signed-byte 14) value)
			  (type disassem:disassem-state dstate))
		 (+ (ash value 2) (disassem:dstate-cur-addr dstate))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant trap-values-alist '((:t . 31) (:lt . 16) (:le . 20) (:eq . 4) (:lng . 6)
                                   (:ge .12) (:ne . 24) (:ng . 20) (:llt . 2) (:f . 0)
                                   (:lle . 6) (:lge . 5) (:lgt . 1) (:lnl . 5))))
                                   
    
(defun valid-tcond-encoding (enc)
  (or (and (if (integerp enc) (= (logand 31 enc) enc)) enc)
      (cdr (assoc enc trap-values-alist))
      (error "Unknown trap condition: ~s" enc)))
        
(disassem:define-argument-type to-field
  :sign-extend nil
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate)
                        (type stream stream)
                        (type fixnum value))
               (princ (or (car (rassoc value trap-values-alist))
                          value) 
                      stream)))

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (system:sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system:system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (kernel:copy-from-system-area sap (* ppc:byte-bits (1+ offset))
                                         vector (* ppc:word-bits
                                                   ppc:vector-data-offset)
                                         (* length ppc:byte-bits))
           (collect ((sc-offsets)
                     (lengths))
             (lengths 1)                ; the length byte
             (let* ((index 0)
                    (error-number (c::read-var-integer vector index)))
               (lengths index)
               (loop
                 (when (>= index length)
                   (return))
                 (let ((old-index index))
                   (sc-offsets (c::read-var-integer vector index))
                   (lengths (- index old-index))))
               (values error-number
                       (1+ length)
                       (sc-offsets)
                       (lengths))))))))



(defun emit-conditional-branch (segment bo bi target &optional aa-p lk-p)
  (declare (type boolean aa-p lk-p))
  (let* ((bo (valid-bo-encoding bo))
         (bi (valid-bi-encoding bi))
         (aa-bit (if aa-p 1 0))
         (lk-bit (if lk-p 1 0)))
    (if aa-p                            ; Not bloody likely, bwth.
      (emit-b-form-inst segment 16 bo bi target aa-bit lk-bit)
      ;; the target may be >+-8k away, in which case we have to invert the
      ;; test and do an absolute branch
      (emit-chooser
       ;; We emit either 4 or 8 bytes, so I think we declare this as
       ;; preserving 4 byte alignment.  If this gives us no joy, we can
       ;; stick a nop in the long branch and then we will be
       ;; preserving 8 byte alignment
       segment 8 2 ; 2^2 is 4 byte alignment.  I think
       #'(lambda (segment posn magic-value)
	   (let ((delta (ash (- (label-position target posn magic-value) posn)
			     -2)))
	     (when (typep delta '(signed-byte 14))
	       (emit-back-patch segment 4
				#'(lambda (segment posn)
				    (emit-b-form-inst 
				     segment 16 bo bi
				     (ash (- (label-position target) posn) -2)
				     aa-bit lk-bit)))
	       t)))
       #'(lambda (segment posn)
	   (declare (ignore posn))
	   (let ((bo (logxor 8 bo))) ;; invert the test
	     (emit-b-form-inst segment 16 bo bi
			       2 ; skip over next instruction
			       0 0)
	     (emit-back-patch segment 4
			      #'(lambda (segment posn)
				  (declare (ignore posn))
				  (emit-i-form-branch segment target lk-p)))))
       ))))
	     


; non-absolute I-form: B, BL.
(defun emit-i-form-branch (segment target &optional lk-p)
  (let* ((lk-bit (if lk-p 1 0)))
    (etypecase target
      (fixup
       (note-fixup segment :b target)
       (emit-i-form-inst segment 18 0 0 lk-bit))
      (label
       (emit-back-patch segment 4
		        #'(lambda (segment posn)
		            (emit-i-form-inst 
                             segment
                             18
                             (ash (- (label-position target) posn) -2)
                             0
                             lk-bit)))))))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defparameter *spr-numbers-alist* '((:xer 1) (:lr 8) (:ctr 9))))

(disassem:define-argument-type spr
  :printer #'(lambda (value stream dstate)
               (declare (ignore dstate)
                        (type (unsigned-byte 10) value))
               (let* ((name (car (rassoc value *spr-numbers-alist*))))
                   (if name
                     (princ name stream)
                     (princ value stream)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter jump-printer
    #'(lambda (value stream dstate)
	(let ((addr (ash value 2)))
	  (disassem:maybe-note-assembler-routine addr t dstate)
	  (write addr :base 16 :radix t :stream stream)))))



;;;; dissassem:define-instruction-formats

(eval-when (:compile-toplevel :execute)
  (defmacro ppc-byte (startbit &optional (endbit startbit))
    (unless (and (typep startbit '(unsigned-byte 32))
                 (typep endbit '(unsigned-byte 32))
                 (>= endbit startbit))
      (error "Bad bits."))
    ``(byte ,(1+ ,(- endbit startbit)) ,(- 31 ,endbit)))

  (defparameter *ppc-field-specs-alist*
    `((aa :field ,(ppc-byte 30))
      (ba :field ,(ppc-byte 11 15) :type 'bi-field)
      (bb :field ,(ppc-byte 16 20) :type 'bi-field)
      (bd :field ,(ppc-byte 16 29) :type 'relative-label)
      (bf :field ,(ppc-byte 6 8) :type 'crf)
      (bfa :field ,(ppc-byte 11 13) :type 'crf)
      (bi :field ,(ppc-byte 11 15) :type 'bi-field)
      (bo :field ,(ppc-byte 6 10) :type 'bo-field)
      (bt :field ,(ppc-byte 6 10) :type 'bi-field)
      (d :field ,(ppc-byte 16 31) :sign-extend t)
      (flm :field ,(ppc-byte 7 14) :sign-extend nil)
      (fra :field ,(ppc-byte 11 15) :type 'fp-reg)
      (frb :field ,(ppc-byte 16 20) :type 'fp-reg)
      (frc :field ,(ppc-byte 21 25) :type 'fp-reg)
      (frs :field ,(ppc-byte 6 10) :type 'fp-reg)
      (frt :field ,(ppc-byte 6 10) :type 'fp-reg)
      (fxm :field ,(ppc-byte 12 19) :sign-extend nil)
      (l :field ,(ppc-byte 10) :sign-extend nil)
      (li :field ,(ppc-byte 6 29) :sign-extend t :type 'relative-label)
      (li-abs :field ,(ppc-byte 6 29) :sign-extend t :printer jump-printer)
      (lk :field ,(ppc-byte 31))
      (mb :field ,(ppc-byte 21 25) :sign-extend nil)
      (me :field ,(ppc-byte 26 30) :sign-extend nil)
      (nb :field ,(ppc-byte 16 20) :sign-extend nil)
      (oe :field ,(ppc-byte 21))
      (ra :field ,(ppc-byte 11 15) :type 'reg)
      (rb :field ,(ppc-byte 16 20) :type 'reg)
      (rc :field ,(ppc-byte 31))
      (rs :field ,(ppc-byte 6 10) :type 'reg)
      (rt :field ,(ppc-byte 6 10) :type 'reg)
      (sh :field ,(ppc-byte 16 20) :sign-extend nil)
      (si :field ,(ppc-byte 16 31) :sign-extend t)
      ;; FIXME?  The spr field is really a split field of length 10.
      ;; An spr value is encoded as 2 5-bit fields, swapped.  Should
      ;; we make this explicit and define spr-hi and spr-lo to make it
      ;; easier?  Or maybe make the register field smarter so we swap
      ;; the 5 bits automatically?  Or something else entirely?
      (spr :field ,(ppc-byte 11 20) :type 'spr)
      (to :field ,(ppc-byte 6 10) :type 'to-field)
      (u :field ,(ppc-byte 16 19) :sign-extend nil)
      (ui :field ,(ppc-byte 16 31) :sign-extend nil)
      (xo21-30 :field ,(ppc-byte 21 30) :sign-extend nil)
      (xo22-30 :field ,(ppc-byte 22 30) :sign-extend nil)
      (xo26-30 :field ,(ppc-byte 26 30) :sign-extend nil)))

(defmacro def-ppc-iformat ((name &optional default-printer) &rest specs)
  (flet ((specname-field (specname) 
           (or (assoc specname *ppc-field-specs-alist*)
               (error "Unknown ppc instruction field spec ~s" specname))))
    (labels ((spec-field (spec)
               (if (atom spec)
                 (specname-field spec)
                 (cons (car spec)
                       (cdr (specname-field (cadr spec)))))))
      (collect ((field (list '(op :field (byte 6 26)))))
        (dolist (spec specs) 
          (field (spec-field spec)))
        `(disassem:define-instruction-format (,name 32 ,@(if default-printer `(:default-printer ,default-printer)))
           ,@(field))))))
)
            

(disassem:define-instruction-format (instr 32)
  (op :field (byte 6 26))
  (other :field (byte 26 0)))

(disassem:define-instruction-format (xinstr 32 :default-printer '(:name :tab data))
  (op-to-a :field (byte 16 16))
  (data :field (byte 16 0)))

(disassem:define-instruction-format (sc 32 :default-printer '(:name :tab rest))
  (op :field (byte 6 26))
  (rest :field (byte 26 0) :value 2))

(def-ppc-iformat (i '(:name :tab li)) 
  li aa lk)

(def-ppc-iformat (i-abs '(:name :tab li-abs)) 
  li-abs aa lk)

(def-ppc-iformat (b '(:name :tab bo ", " bi ", " bd)) 
  bo bi bd aa lk)

(def-ppc-iformat (d '(:name :tab rt ", " d "(" ra ")"))
  rt ra d)

(def-ppc-iformat (d-si '(:name :tab rt ", " ra ", " si ))
  rt ra si)

(def-ppc-iformat (d-rs '(:name :tab rs ", " d "(" ra ")"))
  rs ra d)

(def-ppc-iformat (d-rs-ui '(:name :tab ra ", " rs ", " ui))
  rs ra ui)

(def-ppc-iformat (d-crf-si)
  bf l ra si)

(def-ppc-iformat (d-crf-ui)
  bf l ra ui)

(def-ppc-iformat (d-to '(:name :tab to ", " ra ", " si))
  to ra rb si)

(def-ppc-iformat (d-frt '(:name :tab frt ", " d "(" ra ")"))
  frt ra d)

(def-ppc-iformat (d-frs '(:name :tab frs ", " d "(" ra ")"))
  frs ra d)



;;; There are around ... oh, 28 or so ... variants on the "X" format.
;;;  Some of them are only used by one instruction; some are used by dozens.
;;;  Some aren't used by instructions that we generate ...

(def-ppc-iformat (x '(:name :tab rt ", " ra ", " rb))
  rt ra rb (xo xo21-30))

(def-ppc-iformat (x-1 '(:name :tab rt ", " ra ", " nb))
  rt ra nb (xo xo21-30))

(def-ppc-iformat (x-4 '(:name :tab rt))
  rt (xo xo21-30))

(def-ppc-iformat (x-5 '(:name :tab ra ", " rs ", " rb))
  rs ra rb (xo xo21-30) rc)

(def-ppc-iformat (x-7 '(:name :tab ra ", " rs ", " rb))
  rs ra rb (xo xo21-30))

(def-ppc-iformat (x-8 '(:name :tab ra ", " rs ", " nb))
  rs ra nb (xo xo21-30))

(def-ppc-iformat (x-9 '(:name :tab ra ", " rs ", " sh))
  rs ra sh (xo xo21-30) rc)

(def-ppc-iformat (x-10 '(:name :tab ra ", " rs))
  rs ra (xo xo21-30) rc)

(def-ppc-iformat (x-14 '(:name :tab bf ", " l ", " ra ", " rb))
  bf l ra rb (xo xo21-30))

(def-ppc-iformat (x-15 '(:name :tab bf ", " l ", " fra ", " frb))
  bf l fra frb (xo xo21-30))

(def-ppc-iformat (x-18 '(:name :tab bf))
  bf (xo xo21-30))

(def-ppc-iformat (x-19 '(:name :tab to ", " ra ", " rb))
  to ra rb (xo xo21-30))

(def-ppc-iformat (x-20 '(:name :tab frt ", " ra ", " rb))
  frt ra rb (xo xo21-30))

(def-ppc-iformat (x-21 '(:name :tab frt ", " frb))
  frt frb (xo xo21-30) rc)

(def-ppc-iformat (x-22 '(:name :tab frt))
  frt (xo xo21-30) rc)

(def-ppc-iformat (x-23 '(:name :tab ra ", " frs ", " rb))
  frs ra rb (xo xo21-30))

(def-ppc-iformat (x-24 '(:name :tab bt))
  bt (xo xo21-30) rc)

(def-ppc-iformat (x-25 '(:name :tab ra ", " rb))
  ra rb (xo xo21-30))

(def-ppc-iformat (x-26 '(:name :tab rb))
  rb (xo xo21-30))

(def-ppc-iformat (x-27 '(:name))
  (xo xo21-30))


;;;;

(def-ppc-iformat (xl '(:name :tab bt ", " ba ", " bb))
  bt ba bb (xo xo21-30))

(def-ppc-iformat (xl-bo-bi '(:name :tab bo ", " bi))
  bo bi (xo xo21-30) lk)

(def-ppc-iformat (xl-cr '(:name :tab bf ", " bfa))
  bf bfa (xo xo21-30))

(def-ppc-iformat (xl-xo '(:name))
  (xo xo21-30))


;;;;

(def-ppc-iformat (xfx)
  rt spr (xo xo21-30))

(def-ppc-iformat (xfx-fxm '(:name :tab fxm ", " rs))
  rs fxm (xo xo21-30))

(def-ppc-iformat (xfl '(:name :tab flm ", " frb))
  flm frb (xo xo21-30) rc)


;;;

(def-ppc-iformat (xo '(:name :tab rt ", " ra ", " rb))
  rt ra rb oe (xo xo22-30) rc)

(def-ppc-iformat (xo-oe '(:name :tab rt ", " ra ", " rb))
  rt ra rb (xo xo22-30) rc)

(def-ppc-iformat (xo-a '(:name :tab rt ", " ra))
  rt ra oe (xo xo22-30) rc)


;;;

(def-ppc-iformat (a '(:name :tab frt ", " fra ", " frb ", " frc))
  frt fra frb frc (xo xo26-30) rc)

(def-ppc-iformat (a-tab '(:name :tab frt ", " fra ", " frb))
  frt fra frb (xo xo26-30) rc)

(def-ppc-iformat (a-tac '(:name :tab frt ", " fra ", " frc))
  frt fra frc (xo xo26-30) rc)

(def-ppc-iformat (a-tbc '(:name :tab frt ", " frb ", " frc))
  frt frb frc (xo xo26-30) rc)


(def-ppc-iformat (m '(:name :tab ra ", " rs ", " rb ", " mb ", " me))
  rs ra rb mb me rc)

(def-ppc-iformat (m-sh '(:name :tab ra ", " rs ", " sh ", " mb ", " me))
  rs ra sh mb me rc)


;;;; Primitive emitters.


(define-emitter emit-word 32
  (byte 32 0))

(define-emitter emit-short 16
  (byte 16 0))

(define-emitter emit-i-form-inst 32
  (byte 6 26) (byte 24 2) (byte 1 1) (byte 1 0))

(define-emitter emit-b-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 14 2) (byte 1 1) (byte 1 0))

(define-emitter emit-sc-form-inst 32
  (byte 6 26) (byte 26 0))

(define-emitter emit-d-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 16 0))

; Also used for XL-form.  What's the difference ?
(define-emitter emit-x-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 10 1) (byte 1 0))

(define-emitter emit-xfx-form-inst 32
  (byte 6 26) (byte 5 21) (byte 10 11) (byte 10 1) (byte 1 0))

(define-emitter emit-xfl-form-inst 32
  (byte 6 26) (byte 10  16) (byte 5 11) (byte 10 1) (byte 1 0))

; XS is 64-bit only
(define-emitter emit-xo-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 1 10) (byte 9 1) (byte 1 0))

(define-emitter emit-a-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 5 6) (byte 5 1) (byte 1 0))




(defun unimp-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (disassem:note x dstate))))
    (case (xinstr-data chunk dstate)
      (#.vm:error-trap
       (nt "Error trap")
       (disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.vm:cerror-trap
       (nt "Cerror trap")
       (disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.vm:object-not-list-trap
       (nt "Object not list trap"))
      (#.vm:breakpoint-trap
       (nt "Breakpoint trap"))
      (#.vm:pending-interrupt-trap
       (nt "Pending interrupt trap"))
      (#.vm:halt-trap
       (nt "Halt trap"))
      (#.vm:function-end-breakpoint-trap
       (nt "Function end breakpoint trap"))
      (#.vm:object-not-instance-trap
       (nt "Object not instance trap"))
    )))

(eval-when (:compile-toplevel :execute)

(defun classify-dependencies (deplist)
  (collect ((reads) (writes))
    (dolist (dep deplist)
      (ecase (car dep)
        (reads (reads dep))
        (writes (writes dep))))
    (values (reads) (writes))))

(defmacro define-xo-instruction (name op xo oe-p rc-p always-reads-xer always-writes-xer cost)
  `(define-instruction ,name (segment rt ra rb)
     (:printer xo ((op ,op ) (xo ,xo) (oe ,(if oe-p 1 0)) (rc ,(if rc-p 1 0))))
     (:dependencies (reads ra) (reads rb) ,@(if always-reads-xer '((reads :xer))) 
                    (writes rt) ,@(if rc-p '((writes :ccr))) ,@(if (or oe-p always-writes-xer) '((writes :xer))) )
     (:cost ,cost)
     (:delay ,cost)
     (:emitter
      (emit-xo-form-inst segment ,op
                         (reg-tn-encoding rt) 
                         (reg-tn-encoding ra) 
                         (reg-tn-encoding rb)
                         ,(if oe-p 1 0)
                         ,xo
                         ,(if rc-p 1 0)))))

(defmacro define-xo-oe-instruction (name op xo rc-p always-reads-xer always-writes-xer cost)
  `(define-instruction ,name (segment rt ra rb)
     (:printer xo-oe ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
     (:dependencies (reads ra) (reads rb) ,@(if always-reads-xer '((reads :xer))) 
                    (writes rt) ,@(if rc-p '((writes :ccr))) ,@(if always-writes-xer '((writes :xer))))
     (:cost ,cost)
     (:delay ,cost)
     (:emitter
      (emit-xo-form-inst segment ,op
                         (reg-tn-encoding rt) 
                         (reg-tn-encoding ra) 
                         (reg-tn-encoding rb)
                         0
                         ,xo
                         (if ,rc-p 1 0)))))

(defmacro define-4-xo-instructions (base op xo &key always-reads-xer always-writes-xer (cost 1))
  `(progn
    (define-xo-instruction ,base ,op ,xo nil nil ,always-reads-xer ,always-writes-xer ,cost)
    (define-xo-instruction ,(symbolicate base ".") ,op ,xo nil t ,always-reads-xer ,always-writes-xer ,cost)
    (define-xo-instruction ,(symbolicate base "O") ,op ,xo t nil ,always-reads-xer ,always-writes-xer ,cost)
    (define-xo-instruction ,(symbolicate base "O.") ,op ,xo t t ,always-reads-xer ,always-writes-xer ,cost)))

(defmacro define-2-xo-oe-instructions (base op xo &key always-reads-xer always-writes-xer (cost 1))
  `(progn
    (define-xo-oe-instruction ,base ,op ,xo nil ,always-reads-xer ,always-writes-xer ,cost)
    (define-xo-oe-instruction ,(symbolicate base ".") ,op ,xo t ,always-reads-xer ,always-writes-xer ,cost)))

(defmacro define-xo-a-instruction (name op xo oe-p rc-p always-reads-xer always-writes-xer cost)
  `(define-instruction ,name (segment rt ra)
     (:printer xo-a ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0)) (oe ,(if oe-p 1 0))))
     (:dependencies (reads ra) ,@(if always-reads-xer '((reads :xer)))
                    (writes rt) ,@(if rc-p '((writes :ccr))) ,@(if always-writes-xer '((writes :xer))) )
     (:cost ,cost)
     (:delay ,cost)
     (:emitter
      (emit-xo-form-inst segment ,op
                         (reg-tn-encoding rt) 
                         (reg-tn-encoding ra) 
                         0
                         (if ,oe-p 1 0)
                         ,xo
                         (if ,rc-p 1 0)))))

(defmacro define-4-xo-a-instructions (base op xo &key always-reads-xer always-writes-xer (cost 1))
  `(progn
    (define-xo-a-instruction ,base ,op ,xo nil nil ,always-reads-xer ,always-writes-xer ,cost)
    (define-xo-a-instruction ,(symbolicate base ".") ,op ,xo nil t ,always-reads-xer ,always-writes-xer ,cost)
    (define-xo-a-instruction ,(symbolicate base "O")  ,op ,xo t nil ,always-reads-xer ,always-writes-xer ,cost)
    (define-xo-a-instruction ,(symbolicate base "O.") ,op ,xo t t ,always-reads-xer ,always-writes-xer ,cost)))

(defmacro define-x-instruction (name op xo &key (cost 2) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment rt ra rb)
       (:printer x ((op ,op) (xo ,xo)))
       (:delay ,cost)
       (:cost ,cost)
       (:dependencies (reads ra) (reads rb) ,@ other-reads 
                      (writes rt) ,@other-writes)
       (:emitter
        (emit-x-form-inst segment ,op 
                          (reg-tn-encoding rt) 
                          (reg-tn-encoding ra)
                          (reg-tn-encoding rb)
                          ,xo
                          0)))))

(defmacro define-x-20-instruction (name op xo &key (cost 2) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment frt ra rb)
       (:printer x-20 ((op ,op) (xo ,xo)))
       (:delay ,cost)
       (:cost ,cost)
       (:dependencies (reads ra) (reads rb) ,@other-reads 
                      (writes frt) ,@other-writes)
       (:emitter
        (emit-x-form-inst segment ,op 
                          (fp-reg-tn-encoding frt) 
                          (reg-tn-encoding ra)
                          (reg-tn-encoding rb)
                          ,xo
                          0)))))

(defmacro define-x-5-instruction (name op xo rc-p &key (cost 1) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment ra rs rb)
       (:printer x-5 ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
       (:delay ,cost)
       (:cost ,cost)
       (:dependencies (reads rb) (reads rs) ,@other-reads 
                      (writes ra) ,@other-writes)
       (:emitter
        (emit-x-form-inst segment ,op 
                          (reg-tn-encoding rs) 
                          (reg-tn-encoding ra)
                          (reg-tn-encoding rb)
                          ,xo
                          ,(if rc-p 1 0))))))


(defmacro define-x-5-st-instruction (name op xo rc-p &key (cost 1) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment rs ra rb)
       (:printer x-5 ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
       (:delay ,cost)
       (:cost ,cost)
       (:dependencies (reads ra) (reads rb) (reads rs) ,@other-reads 
                      ,@other-writes)
       (:emitter
        (emit-x-form-inst segment ,op 
                          (reg-tn-encoding rs) 
                          (reg-tn-encoding ra)
                          (reg-tn-encoding rb)
                          ,xo
                          ,(if rc-p 1 0))))))

(defmacro define-x-23-st-instruction (name op xo &key (cost 1) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment frs ra rb)
       (:printer x-23 ((op ,op) (xo ,xo)))
       (:delay ,cost)
       (:cost ,cost)
       (:dependencies (reads ra) (reads rb) (reads frs) ,@other-reads 
                      ,@other-writes)
       (:emitter
        (emit-x-form-inst segment ,op 
                          (fp-reg-tn-encoding frs) 
                          (reg-tn-encoding ra)
                          (reg-tn-encoding rb)
                          ,xo
                          0)))))


(defmacro define-x-10-instruction (name op xo rc-p &key (cost 1) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment ra rs)
       (:printer x-10 ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
       (:delay ,cost)
       (:cost ,cost)
       (:dependencies (reads rs) ,@other-reads 
                      (writes ra) ,@other-writes)
       (:emitter
        (emit-x-form-inst segment ,op 
                          (reg-tn-encoding rs) 
                          (reg-tn-encoding ra)
                          0
                          ,xo
                          ,(if rc-p 1 0))))))

(defmacro define-2-x-5-instructions (name op xo &key (cost 1) other-dependencies)
  `(progn
     (define-x-5-instruction ,name ,op ,xo nil :cost ,cost :other-dependencies ,other-dependencies)
     (define-x-5-instruction ,(symbolicate name ".") ,op ,xo t :cost ,cost 
       :other-dependencies ,other-dependencies)))

(defmacro define-2-x-10-instructions (name op xo &key (cost 1) other-dependencies)
  `(progn
     (define-x-10-instruction ,name ,op ,xo nil :cost ,cost :other-dependencies ,other-dependencies)
     (define-x-10-instruction ,(symbolicate name ".") ,op ,xo t :cost ,cost 
       :other-dependencies ,other-dependencies)))


(defmacro define-x-21-instruction (name op xo rc-p &key (cost 4) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment frt frb)
       (:printer x-21 ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
       (:cost ,cost)
       (:delay ,cost)
       (:dependencies (reads frb) ,@other-reads 
                      (writes frt) ,@other-writes)
       (:emitter
        (emit-x-form-inst segment ,op
                          (fp-reg-tn-encoding frt)
                          0
                          (fp-reg-tn-encoding frb)
                          ,xo
                          ,(if rc-p 1 0))))))

(defmacro define-2-x-21-instructions (name op xo &key (cost 4) other-dependencies)
  `(progn
     (define-x-21-instruction ,name ,op ,xo nil :cost ,cost :other-dependencies ,other-dependencies)
     (define-x-21-instruction ,(symbolicate name ".") ,op ,xo t :cost ,cost 
       :other-dependencies ,other-dependencies)))


(defmacro define-d-si-instruction (name op &key (fixup nil) (cost 1) other-dependencies si-printer)
  (multiple-value-bind (other-reads other-writes)
      (classify-dependencies other-dependencies)
  `(define-instruction ,name (segment rt ra si)
     (:declare (type (signed-byte 16)))
     (:printer d-si ((op ,op)
		     ,@(if si-printer `((si nil :printer ,si-printer)))))
     (:delay ,cost)
     (:cost ,cost)
     (:dependencies (reads ra) ,@other-reads 
                    (writes rt) ,@other-writes)
     (:emitter
      (when (typep si 'fixup)
	(ecase ,fixup
	  ((:ha :l) (note-fixup segment ,fixup si)))
	(setq si 0))      
      (emit-d-form-inst segment ,op (reg-tn-encoding rt) (reg-tn-encoding ra) si)))))


(defmacro define-d-rs-ui-instruction (name op &key (cost 1) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment ra rs ui)
       (:declare (type (unsigned-byte 16) ui))
       (:printer d-rs-ui ((op ,op)))
       (:cost ,cost)
       (:delay ,cost)
       (:dependencies (reads rs) ,@other-reads 
                      (writes ra) ,@other-writes)
       (:emitter
        (emit-d-form-inst segment ,op (reg-tn-encoding rs) (reg-tn-encoding ra) ui)))))

(defmacro define-d-instruction (name op &key (cost 2) other-dependencies pinned)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment rt ra si)
       (:declare (type (signed-byte 16) si))
       (:printer d ((op ,op)))
       (:delay ,cost)
       (:cost ,cost)
       ,@(when pinned '(:pinned))
       (:dependencies (reads ra) ,@other-reads 
                      (writes rt) ,@other-writes)
       (:emitter
        (emit-d-form-inst segment ,op (reg-tn-encoding rt) (reg-tn-encoding ra) si)))))

(defmacro define-d-frt-instruction (name op &key (cost 3) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
  `(define-instruction ,name (segment frt ra si)
     (:declare (type (signed-byte 16) si))
     (:printer d-frt ((op ,op)))
     (:delay ,cost)
     (:cost ,cost)
     (:dependencies (reads ra) ,@other-reads 
                    (writes frt) ,@other-writes)
     (:emitter
      (emit-d-form-inst segment ,op (fp-reg-tn-encoding frt) (reg-tn-encoding ra) si)))))

(defmacro define-d-rs-instruction (name op &key (cost 1) other-dependencies pinned)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment rs ra si)
       (:declare (type (signed-byte 16) si))
       (:printer d-rs ((op ,op)))
       (:delay ,cost)
       (:cost ,cost)
       ,@(when pinned '(:pinned))
       (:dependencies (reads rs) (reads ra) ,@other-reads 
                      (writes :memory :partially t) ,@other-writes)
       (:emitter
        (emit-d-form-inst segment ,op (reg-tn-encoding rs) (reg-tn-encoding ra) si)))))

(defmacro define-d-frs-instruction (name op &key (cost 1) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
  `(define-instruction ,name (segment frs ra si)
     (:declare (type (signed-byte 16) si))
     (:printer d-frs ((op ,op)))
     (:delay ,cost)
     (:cost ,cost)
     (:dependencies (reads frs) (reads ra) ,@other-reads 
                    (writes :memory :partially t) ,@other-writes)
     (:emitter
      (emit-d-form-inst segment ,op (fp-reg-tn-encoding frs) (reg-tn-encoding ra) si)))))

(defmacro define-a-instruction (name op xo rc &key (cost 1) other-dependencies)
  `(define-instruction ,name (segment frt fra frb frc)
     (:printer a ((op ,op) (xo ,xo) (rc ,rc)))
     (:cost ,cost)
     (:delay ,cost)
     (:dependencies (writes frt) (reads fra) (reads frb) (reads frc) ,@other-dependencies)
     (:emitter
        (emit-a-form-inst segment 
                          ,op 
                          (fp-reg-tn-encoding frt) 
                          (fp-reg-tn-encoding fra) 
                          (fp-reg-tn-encoding frb)
                          (fp-reg-tn-encoding frb)
                          ,xo
                          ,rc))))

(defmacro define-2-a-instructions (name op xo &key (cost 1) other-dependencies)
  `(progn
     (define-a-instruction ,name ,op ,xo 0 :cost ,cost :other-dependencies ,other-dependencies)
     (define-a-instruction ,(symbolicate name ".")
       ,op ,xo 1  :cost ,cost :other-dependencies ,other-dependencies)))

(defmacro define-a-tab-instruction (name op xo rc &key (cost 1) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment frt fra frb)
       (:printer a-tab ((op ,op) (xo ,xo) (rc ,rc)))
       (:cost ,cost)
       (:delay 1)
       (:dependencies (reads fra) (reads frb) ,@other-reads
                      (writes frt) ,@other-writes)
       (:emitter
        (emit-a-form-inst segment 
                          ,op 
                          (fp-reg-tn-encoding frt) 
                          (fp-reg-tn-encoding fra) 
                          (fp-reg-tn-encoding frb)
                          0
                          ,xo
                          ,rc)))))

(defmacro define-2-a-tab-instructions (name op xo &key (cost 1) other-dependencies)
  `(progn
     (define-a-tab-instruction ,name ,op ,xo 0 :cost ,cost :other-dependencies ,other-dependencies)
     (define-a-tab-instruction ,(symbolicate name ".")
       ,op ,xo 1  :cost ,cost :other-dependencies ,other-dependencies)))

(defmacro define-a-tac-instruction (name op xo rc &key (cost 1) other-dependencies)
  (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
    `(define-instruction ,name (segment frt fra frc)
       (:printer a-tac ((op ,op) (xo ,xo) (rc ,rc)))
       (:cost ,cost)
       (:delay 1)
       (:dependencies (reads fra) (reads frc) ,@other-reads
                      (writes frt) ,@other-writes)
       (:emitter
        (emit-a-form-inst segment 
                          ,op 
                          (fp-reg-tn-encoding frt) 
                          (fp-reg-tn-encoding fra) 
                          0
                          (fp-reg-tn-encoding frc)
                          ,xo
                          ,rc)))))

(defmacro define-2-a-tac-instructions (name op xo &key (cost 1) other-dependencies)
  `(progn
     (define-a-tac-instruction ,name ,op ,xo 0 :cost ,cost :other-dependencies ,other-dependencies)
     (define-a-tac-instruction ,(symbolicate name ".")
       ,op ,xo 1  :cost ,cost :other-dependencies ,other-dependencies)))

(defmacro define-crbit-instruction (name op xo)
  `(define-instruction ,name (segment dbit abit bbit)
     (:printer xl ((op ,op ) (xo ,xo)))
     (:delay 1)
     (:cost 1)
     (:dependencies (reads :ccr) (writes :ccr))
     (:emitter (emit-x-form-inst segment 19
                                 (valid-bi-encoding dbit)
                                 (valid-bi-encoding abit)
                                 (valid-bi-encoding bbit)
                                 ,xo
                                 0))))
);eval-when

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The instructions, in numerical order

(define-instruction unimp (segment data)
  (:declare (type (signed-byte 16) data))
  (:printer xinstr ((op-to-a #.(logior (ash 3 10) (ash 6 5) 0)))
	    :default :control #'unimp-control)
  :pinned
  (:delay 0)
  (:emitter (emit-d-form-inst segment 3 6 0 data)))

(define-instruction twi (segment tcond ra si)
  (:printer d-to ((op 3)))
  (:delay 1)
  :pinned
  (:emitter (emit-d-form-inst segment 3 (valid-tcond-encoding tcond) (reg-tn-encoding ra) si)))

(define-d-si-instruction mulli 7 :cost 5)
(define-d-si-instruction subfic 8)

(define-instruction cmplwi (segment crf ra &optional (ui nil ui-p))
  (:printer d-crf-ui ((op 10) (l 0)) '(:name :tab bf ", " ra ", " ui))
  (:dependencies (if ui-p (reads ra) (reads crf)) (writes :ccr))
  (:delay 1)
  (:emitter 
   (unless ui-p
     (setq ui ra ra crf crf :cr0))
   (emit-d-form-inst segment 
                     10
                     (valid-cr-field-encoding crf) 
                     (reg-tn-encoding ra)
                     ui)))

(define-instruction cmpwi (segment crf ra  &optional (si nil si-p))
  (:printer d-crf-si ((op 11) (l 0)) '(:name :tab bf ", " ra ", " si))
  (:dependencies (if si-p (reads ra) (reads crf)) (writes :ccr))
  (:delay 1)
  (:emitter 
   (unless si-p
     (setq si ra ra crf crf :cr0))
   (emit-d-form-inst segment 
                     11
                     (valid-cr-field-encoding crf) 
                     (reg-tn-encoding ra)
                     si)))

(define-d-si-instruction addic 12 :other-dependencies ((writes :xer)))
(define-d-si-instruction addic. 13 :other-dependencies ((writes :xer) (writes :ccr)))

(define-d-si-instruction addi 14 :fixup :l)
(eval-when (compile load eval)
  (defun addis-arg-printer (value stream dstate)
    (format stream "~D" value)
    ;; Save the immediate value and the destination register for this
    ;; addis instruction.  This is used later to print some possible
    ;; notes about the value loaded by addis.
    (let* ((shifted (ash value 16))
	   (word (disassem::sap-ref-int (disassem:dstate-segment-sap dstate)
					(disassem:dstate-cur-offs dstate)
					vm:word-bytes
					(disassem::dstate-byte-order dstate)))
	   (rd (ldb (byte 5 21) word)))
      (disassem:note (format nil "0x~x = ~D" shifted shifted)
		     dstate)
      (push (cons rd value) *note-addis-inst*))))
  
(define-d-si-instruction addis 15 :fixup :ha :si-printer #'addis-arg-printer)

; There's no real support here for branch options that decrement and test the CTR.
;  (a) the instruction scheduler doesn't know that anything's happening to the CTR
;  (b) Lisp may have to assume that the CTR always has a lisp object/locative in it.

(define-instruction bc (segment bo bi target)
  (:declare (type label target))
  (:printer b ((op 16) (aa 0) (lk 0)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr))
  (:emitter
   (emit-conditional-branch segment bo bi target)))

(define-instruction bcl (segment bo bi target)
  (:declare (type label target))
  (:printer b ((op 16) (aa 0) (lk 1)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (writes :lr))
  (:emitter
   (emit-conditional-branch segment bo bi target nil t)))

(define-instruction bca (segment bo bi target)
  (:declare (type label target))
  (:printer b ((op 16) (aa 1) (lk 0)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr))
  (:emitter
   (emit-conditional-branch segment bo bi target t)))

(define-instruction bcla (segment bo bi target)
  (:declare (type label target))
  (:printer b ((op 16) (aa 1) (lk 1)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (writes :lr))
  (:emitter
   (emit-conditional-branch segment bo bi target t t)))

;;; There may (or may not) be a good reason to use this in preference to "b[la] target".
;;; I can't think of a -bad- reason ...

(define-instruction bu (segment target)
  (:declare (type label target))
  (:printer b ((op 16) (bo #.(valid-bo-encoding :bo-u)) (bi 0) (aa 0) (lk 0)) 
            '(:name :tab bd))
  (:attributes branch)
  (:delay 1)
  (:emitter
   (emit-conditional-branch segment #.(valid-bo-encoding :bo-u) 0 target nil nil)))


(define-instruction bt (segment bi  target)
  (:printer b ((op 16) (bo #.(valid-bo-encoding :bo-t)) (aa 0) (lk 0))
            '(:name :tab bi ", " bd))
  (:attributes branch)
  (:dependencies (reads :ccr))
  (:delay 1)
  (:emitter
   (emit-conditional-branch segment #.(valid-bo-encoding :bo-t) bi target nil nil)))

(define-instruction bf (segment bi  target)
  (:printer b ((op 16) (bo #.(valid-bo-encoding :bo-f)) (aa 0) (lk 0))
            '(:name :tab bi ", " bd))
  (:attributes branch)
  (:dependencies (reads :ccr))
  (:delay 1)
  (:emitter
   (emit-conditional-branch segment #.(valid-bo-encoding :bo-f) bi target nil nil)))

(define-instruction b? (segment cr-field-name cr-name  &optional (target nil target-p))
  (:delay 1)
  (:attributes branch)
  (:dependencies (reads :ccr))
  (:emitter 
   (unless target-p
     (setq target cr-name cr-name cr-field-name cr-field-name :cr0))
   (let*  ((+cond (position cr-name cr-bit-names))
                    (-cond (position cr-name cr-bit-inverse-names))
                    (b0 (if +cond :bo-t 
                            (if -cond 
                              :bo-f
                              (error "Unknown branch condition ~s" cr-name))))
                    (cr-form (list cr-field-name (if +cond cr-name (svref cr-bit-names -cond)))))
              (emit-conditional-branch segment b0 cr-form target))))

(define-instruction sc (segment)
  (:printer sc ((op 17)))
  (:delay 1)
  :pinned
  (:emitter (emit-sc-form-inst segment 17 2)))

(define-instruction b (segment target)
  (:printer i ((op 18) (aa 0) (lk 0)))
  (:attributes branch)
  (:delay 1)
  (:emitter
   (emit-i-form-branch segment target nil)))

(define-instruction ba (segment target)
  (:printer i-abs ((op 18) (aa 1) (lk 0)))
  (:attributes branch)
  (:delay 1)
  (:emitter
   (when (typep target 'fixup)
     (note-fixup segment :ba target)
     (setq target 0))
   (emit-i-form-inst segment 18 (ash target -2) 1 0)))


(define-instruction bl (segment target)
  (:printer i ((op 18) (aa 0) (lk 1)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (writes :lr))
  (:emitter
   (emit-i-form-branch segment target t)))

(define-instruction bla (segment target)
  (:printer i-abs ((op 18) (aa 1) (lk 1)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (writes :lr))
  (:emitter
   (when (typep target 'fixup)
     (note-fixup segment :ba target)
     (setq target 0))
   (emit-i-form-inst segment 18 (ash target -2) 1 1)))

(define-instruction blr (segment)
  (:printer xl-bo-bi ((op 19) (xo 16) (bo #.(valid-bo-encoding :bo-u))(bi 0) (lk 0))  '(:name))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (reads :ctr))
  (:emitter
   (emit-x-form-inst segment 19 (valid-bo-encoding :bo-u) 0 0 16 0)))

(define-instruction bclr (segment bo bi)
  (:printer xl-bo-bi ((op 19) (xo 16)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (reads :lr))
  (:emitter
   (emit-x-form-inst segment 19 (valid-bo-encoding bo) (valid-bi-encoding bi) 0 16 0)))

(define-instruction bclrl (segment bo bi)
  (:printer xl-bo-bi ((op 19) (xo 16) (lk 1)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (reads :lr) (write :lr))
  (:emitter
   (emit-x-form-inst segment 19 (valid-bo-encoding bo) (valid-bi-encoding bi) 0 16 1)))

(define-crbit-instruction crnor 19 33)
(define-crbit-instruction crandc 19 129)
(define-instruction isync (segment)
  (:printer xl-xo ((op 19) (xo 150)))
  (:delay 1)
  :pinned
  (:emitter (emit-x-form-inst segment 19 0 0 0 150 0)))

(define-crbit-instruction crxor 19 193)
(define-crbit-instruction crnand 19 225)
(define-crbit-instruction crand 19 257)
(define-crbit-instruction creqv 19 289)
(define-crbit-instruction crorc 19 417)
(define-crbit-instruction cror 19 449)

(define-instruction bcctr (segment bo bi)
  (:printer xl-bo-bi ((op 19) (xo 528)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (reads :ctr))
  (:emitter
   (emit-x-form-inst segment 19 (valid-bo-encoding bo) (valid-bi-encoding bi) 0 528 0)))

(define-instruction bcctrl (segment bo bi)
  (:printer xl-bo-bi ((op 19) (xo 528) (lk 1)))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (reads :ctr) (writes :lr))
  (:emitter
   (emit-x-form-inst segment 19 (valid-bo-encoding bo) (valid-bi-encoding bi) 0 528 1)))

;; Really a bcctr inst
(define-instruction bctr (segment)
  (:printer xl-bo-bi ((op 19) (xo 528) (bo #.(valid-bo-encoding :bo-u)) (bi 0) (lk 0))  '(:name))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (reads :ctr))
  (:emitter
   (emit-x-form-inst segment 19 #.(valid-bo-encoding :bo-u) 0 0  528 0)))

;; Really a bclrl inst
(define-instruction blrl (segment)
  (:printer xl-bo-bi ((op 19) (xo 16) (bo #.(valid-bo-encoding :bo-u)) (bi 0) (lk 1))  '(:name))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (reads :ctr))
  (:emitter
   (emit-x-form-inst segment 19 #.(valid-bo-encoding :bo-u) 0 0  16 1)))

(define-instruction bctrl (segment)
  (:printer xl-bo-bi ((op 19) (xo 528) (bo #.(valid-bo-encoding :bo-u)) (bi 0) (lk 1))  '(:name))
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads :ccr) (reads :ctr))
  (:emitter
   (emit-x-form-inst segment 19 #.(valid-bo-encoding :bo-u) 0 0  528 1)))

(define-instruction rlwimi (segment ra rs sh mb me)
  (:printer m-sh ((op 20) (rc 0)))
  (:dependencies (reads rs) (writes ra))
  (:delay 1)
  (:emitter
   (emit-a-form-inst segment 20 (reg-tn-encoding rs) (reg-tn-encoding ra) sh mb me 0)))

(define-instruction rlwimi. (segment ra rs sh mb me)
  (:printer m-sh ((op 20) (rc 1)))
  (:dependencies (reads rs) (writes ra) (writes :ccr))
  (:delay 1)
  (:emitter
   (emit-a-form-inst segment 20 (reg-tn-encoding rs) (reg-tn-encoding ra) sh mb me 1)))

(eval-when (compile load eval)
  (defun rlwinm-printer (value stream dstate)
    ;; This arg printer is meant to be used by rlwinm printer.  It
    ;; should print out the arg as a decimal number, but also add a
    ;; note if this rlwinm instruction has a different, but clearer
    ;; mnenomic.  This is really just a hack until (when?) I can
    ;; figure out how to get the disassembler to print out the
    ;; mnenomic instead of RLWINM.
    (format stream "~D" value)
    ;; Get the instruction word and extract out the values for the ra,
    ;; rs, sh, mb, and me fields.
    (let* ((word (disassem::sap-ref-int (disassem:dstate-segment-sap dstate)
					(disassem:dstate-cur-offs dstate)
					vm:word-bytes
					(disassem::dstate-byte-order dstate))))
      (let ((rs (ldb (byte 5 21) word))
	    (ra (ldb (byte 5 16) word))
	    (sh (ldb (byte 5 11) word))
	    (mb (ldb (byte 5 6) word))
	    (me (ldb (byte 5 1) word))
	    (rc (if (zerop (ldb (byte 1 0) word))
		    ""
		    ".")))
	(cond ((and (= me 31) (= sh (- 32 mb)))
	       ;; srwi inst
	       (let ((note (format nil "~A~A ~A, ~A, ~A"
				   'srwi
				   rc
				   (aref reg-symbols ra)
				   (aref reg-symbols rs)
				   mb)))
		 (disassem:note note dstate)))
	      ((and (zerop mb) (= me (- 31 sh)))
	       (let ((note (format nil "~A~A ~A, ~A, ~A"
				   'slwi
				   rc
				   (aref reg-symbols ra)
				   (aref reg-symbols rs)
				   sh)))
		 (disassem:note note dstate)))
	      ((and (zerop sh) (zerop mb))
	       (let ((note (format nil "~A~A ~A, ~A, ~A"
				   'clrrwi
				   rc
				   (aref reg-symbols ra)
				   (aref reg-symbols rs)
				   (- 31 me))))
		 (disassem:note note dstate)))))))
  )

(define-instruction rlwinm (segment ra rs sh mb me)
  (:printer m-sh ((op 21) (rc 0) (sh nil :printer #'rlwinm-printer)))
  (:delay 1)
  (:dependencies (reads rs) (writes ra))
  (:emitter
   (emit-a-form-inst segment 21 (reg-tn-encoding rs) (reg-tn-encoding ra) sh mb me 0)))

(define-instruction rlwinm. (segment ra rs sh mb me)
  (:printer m-sh ((op 21) (rc 1) (sh nil :printer #'rlwinm-printer)))
  (:delay 1)
  (:dependencies (reads rs) (writes ra) (writes :ccr))
  (:emitter
   (emit-a-form-inst segment 21 (reg-tn-encoding rs) (reg-tn-encoding ra) sh mb me 1)))

(define-instruction rlwnm (segment ra rs rb mb me)
  (:printer m ((op 23) (rc 0) (rb nil :type 'reg)))
  (:delay 1)
  (:dependencies (reads rs) (writes ra) (reads rb))
  (:emitter
   (emit-a-form-inst segment 23 (reg-tn-encoding rs) (reg-tn-encoding ra) (reg-tn-encoding rb) mb me 0)))

(define-instruction rlwnm. (segment ra rs rb mb me)
  (:printer m ((op 23) (rc 1) (rb nil :type 'reg)))
  (:delay 1)
  (:dependencies (reads rs) (reads rb) (writes ra) (writes :ccr))
  (:emitter
   (emit-a-form-inst segment 23 (reg-tn-encoding rs) (reg-tn-encoding ra) (reg-tn-encoding rb) mb me 1)))


(define-d-rs-ui-instruction ori 24)

(define-instruction nop (segment)
  (:printer d-rs-ui ((op 24) (rs 0) (ra 0) (ui 0)) '(:name))
  (:cost 1)
  (:delay 1)
  (:emitter
   (emit-d-form-inst segment 24 0 0 0)))

(define-d-rs-ui-instruction oris 25)
(define-d-rs-ui-instruction xori 26)
(define-d-rs-ui-instruction xoris 27)
(define-d-rs-ui-instruction andi. 28 :other-dependencies ((writes :ccr)))
(define-d-rs-ui-instruction andis. 29 :other-dependencies ((writes :ccr)))

(define-instruction cmpw (segment crf ra  &optional (rb nil rb-p))
  (:printer x-14 ((op 31) (xo 0) (l 0)) '(:name :tab bf ", " ra ", " rb))
  (:delay 1)
  (:dependencies (reads ra) (if rb-p (reads rb) (reads crf)) (reads :xer) (writes :ccr))
  (:emitter 
   (unless rb-p
     (setq rb ra ra crf crf :cr0))
   (emit-x-form-inst segment 
                              31
                              (valid-cr-field-encoding crf) 
                              (reg-tn-encoding ra)
                              (reg-tn-encoding rb)
                              0
                              0)))

(define-instruction tw (segment tcond ra rb)
  (:printer x-19 ((op 31) (xo 4)))
  (:delay 1)
  :pinned
  (:emitter (emit-x-form-inst segment 31 (valid-tcond-encoding tcond) (reg-tn-encoding ra) (reg-tn-encoding rb) 4 0)))

(define-4-xo-instructions subfc 31 8 :always-writes-xer t)
(define-4-xo-instructions addc 31 10 :always-writes-xer t)
(define-2-xo-oe-instructions mulhwu 31 11 :cost 5)

(define-instruction mfcr (segment rd)
  (:printer x-4 ((op 31) (xo 19)))
  (:delay 1)
  (:dependencies (reads :ccr) (writes rd))
  (:emitter (emit-x-form-inst segment 31 (reg-tn-encoding rd) 0 0 19 0)))

(define-x-instruction lwarx 31 20)
(define-x-instruction lwzx 31 23)
(define-2-x-5-instructions slw 31 24)
(define-2-x-10-instructions cntlzw 31 26)
(define-2-x-5-instructions and 31 28)

(define-instruction cmplw (segment crf ra  &optional (rb nil rb-p))
  (:printer x-14 ((op 31) (xo 32) (l 0)) '(:name :tab bf ", " ra ", " rb))
  (:delay 1)
  (:dependencies (reads ra) (if rb-p (reads rb) (reads crf)) (reads :xer) (writes :ccr))
  (:emitter 
   (unless rb-p
     (setq rb ra ra crf crf :cr0))
   (emit-x-form-inst segment 
                     31
                     (valid-cr-field-encoding crf) 
                     (reg-tn-encoding ra)
                     (reg-tn-encoding rb)
                     32
                     0)))


(define-4-xo-instructions subf 31 40)
; dcbst
(define-x-instruction lwzux 31 55 :other-dependencies ((writes rt)))
(define-2-x-5-instructions andc 31 60)
(define-2-xo-oe-instructions mulhw 31 75 :cost 5)

(define-x-instruction lbzx 31 87)
(define-4-xo-a-instructions neg 31 104)
(define-x-instruction lbzux 31 119 :other-dependencies ((writes rt)))
(define-2-x-5-instructions nor 31 124)
(define-4-xo-instructions subfe 31 136 :always-reads-xer t :always-writes-xer t)

(define-instruction-macro sube (rt ra rb)
  `(inst subfe ,rt ,rb ,ra))

(define-instruction-macro sube. (rt ra rb)
  `(inst subfe. ,rt ,rb ,ra))

(define-instruction-macro subeo (rt ra rb)
  `(inst subfeo ,rt ,rb ,ra))

(define-instruction-macro subeo. (rt ra rb)
  `(inst subfeo ,rt ,rb ,ra))

(define-4-xo-instructions adde 31 138 :always-reads-xer t :always-writes-xer t)

(define-instruction mtcrf (segment mask rt)
  (:printer xfx-fxm ((op 31) (xo 144)))
  (:delay 1)
  (:dependencies (reads rt) (writes :ccr))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash mask 1) 144 0)))

(define-x-5-st-instruction stwcx. 31 150 t :other-dependencies ((writes :ccr)))
(define-x-5-st-instruction stwx 31 151 nil)
(define-x-5-st-instruction stwux 31 183 nil :other-dependencies ((writes ra)))
(define-4-xo-a-instructions subfze 31 200 :always-reads-xer t :always-writes-xer t)
(define-4-xo-a-instructions addze 31 202 :always-reads-xer t :always-writes-xer t)
(define-x-5-st-instruction stbx 31 215 nil)
(define-4-xo-a-instructions subfme 31 232 :always-reads-xer t :always-writes-xer t)
(define-4-xo-a-instructions addme 31 234 :always-reads-xer t :always-writes-xer t)
(define-4-xo-instructions mullw 31 235 :cost 5)
(define-x-5-st-instruction stbux 31 247 nil :other-dependencies ((writes ra)))
(define-4-xo-instructions add 31 266)
(define-x-instruction lhzx 31 279)
(define-2-x-5-instructions eqv 31 284)
(define-x-instruction lhzux 31 311 :other-dependencies ((writes ra)))
(define-2-x-5-instructions xor 31 316)

;; FIXME: Note that the spr field is split, so we need to convert the
;; spr value into two 5-bit pieces and swap them to get the right
;; instruction encoding, and the write printer.
(define-instruction mfmq (segment rt)
  (:printer xfx ((op 31) (xo 339) (spr 0)) '(:name :tab rt))
  (:delay 1)
  (:dependencies (reads :xer) (writes rt))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 0 5) 339 0)))

(define-instruction mfxer (segment rt)
  (:printer xfx ((op 31) (xo 339) (spr (ash 1 5))) '(:name :tab rt))
  (:delay 1)
  (:dependencies (reads :xer) (writes rt))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 1 5) 339 0)))

(define-instruction mflr (segment rt)
  (:printer xfx ((op 31) (xo 339) (spr (ash 8 5))) '(:name :tab rt))
  (:delay 1)
  (:dependencies (reads :lr) (writes rt))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 8 5) 339 0)))

(define-instruction mfctr (segment rt)
  (:printer xfx ((op 31) (xo 339) (spr (ash 9 5))) '(:name :tab rt))
  (:delay 1)
  (:dependencies (reads rt) (reads :ctr))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 9 5) 339 0)))


(define-x-instruction lhax 31 343)
(define-x-instruction lhaux 31 375 :other-dependencies ((writes ra)))
(define-x-5-st-instruction sthx 31 407 nil)
(define-2-x-5-instructions orc 31 412)
(define-x-5-st-instruction sthux 31 439 nil :other-dependencies ((writes ra)))

(define-instruction or (segment ra rs rb)
  (:printer x-5 ((op 31) (xo 444) (rc 0)) '((:cond
                                             ((rs :same-as rb) 'mr)
                                             (t :name))
                                            :tab
                                            ra ", " rs
                                            (:unless (:same-as rs) ", " rb)))
  (:delay 1)
  (:cost 1)
  (:dependencies (reads rb) (reads rs) (writes ra))
  (:emitter
   (emit-x-form-inst segment
                     31
                     (reg-tn-encoding rs) 
                     (reg-tn-encoding ra)
                     (reg-tn-encoding rb)
                     444
                     0)))

(define-instruction or. (segment ra rs rb)
  (:printer x-5 ((op 31) (xo 444) (rc 1)) '((:cond
                                             ((rs :same-as rb) 'mr.)
                                             (t :name))
                                            :tab
                                            ra ", " rs
                                            (:unless (:same-as rs) ", " rb)))
  (:delay 1)
  (:cost 1)
  (:dependencies (reads rb) (reads rs) (writes ra))
  (:emitter
   (emit-x-form-inst segment
                     31
                     (reg-tn-encoding rs) 
                     (reg-tn-encoding ra)
                     (reg-tn-encoding rb)
                     444
                     1)))

(define-instruction-macro mr (ra rs)
  `(inst or ,ra ,rs ,rs))

(define-instruction-macro mr. (ra rs)
  `(inst or. ,ra ,rs ,rs))

(define-4-xo-instructions divwu 31 459 :cost 36)

; This is a 601-specific instruction class.
(define-4-xo-instructions div 31 331 :cost 36)

; This is a 601-specific instruction.
(define-instruction mtmq (segment rt)
  (:printer xfx ((op 31) (xo 467) (spr (ash 0 5))) '(:name :tab rt))
  (:delay 1)
  (:dependencies (reads rt) (writes :xer))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 0 5) 467 0)))

(define-instruction mtxer (segment rt)
  (:printer xfx ((op 31) (xo 467) (spr (ash 1 5))) '(:name :tab rt))
  (:delay 1)
  (:dependencies (reads rt) (writes :xer))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 1 5) 467 0)))

(define-instruction mtlr (segment rt)
  (:printer xfx ((op 31) (xo 467) (spr (ash 8 5))) '(:name :tab rt))
  (:delay 1)
  (:dependencies (reads rt) (writes :lr))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 8 5) 467 0)))

(define-instruction mtctr (segment rt)
  (:printer xfx ((op 31) (xo 467) (spr (ash 9 5))) '(:name :tab rt))
  (:delay 1)
  (:dependencies (reads rt) (writes :ctr))
  (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 9 5) 467 0)))


(define-2-x-5-instructions nand 31 476)
(define-4-xo-instructions divw 31 491 :cost 36)

;; This instruction runs very slowly on a G5 (emulated?), so we don't
;; want to use this.  Comment this instruction out so we don't use it
;; by accident, but leave it here so the next person doesn't have to
;; define it from scratch.  Use mtxer zero-tn instead.
#+nil
(define-instruction mcrxr (segment crf)
  (:printer x-18 ((op 31) (xo 512)))
  (:delay 1)
  (:dependencies (reads :xer) (writes :ccr) (writes :xer))
  (:emitter (emit-x-form-inst segment 31 (valid-cr-field-encoding crf) 0 0 512 0)))

(define-instruction lswx (segment rs ra rb) 
  (:printer x ((op 31) (xo 533) (rc 0)))
  (:delay 1)
  :pinned
  (:cost 8) 
  (:emitter (emit-x-form-inst new-assem:segment 31 (reg-tn-encoding rs) (reg-tn-encoding ra) (reg-tn-encoding rb) 533 0)))
(define-x-instruction lwbrx 31 534)
(define-x-20-instruction lfsx 31 535)
(define-2-x-5-instructions srw 31 536)
(define-x-20-instruction lfsux 31 567 :other-dependencies ((writes ra)))

(define-instruction lswi (segment rt ra rb) 
  (:printer x-1 ((op 31) (xo 597) (rc 0)))
  :pinned
  (:delay 8)
  (:cost 8) 
  (:emitter (emit-x-form-inst new-assem:segment 31 (reg-tn-encoding rt) (reg-tn-encoding ra) rb 597 0)))

(define-instruction sync (segment)
  (:printer x-27 ((op 31) (xo 598)))
  (:delay 1)
  :pinned
  (:emitter (emit-x-form-inst segment 31 0 0 0 598 0)))
(define-x-20-instruction lfdx 31 599)
(define-x-20-instruction lfdux 31 631 :other-dependencies ((writes ra)))
(define-instruction stswx (segment rs ra rb) 
  (:printer x-5 ((op 31) (xo 661)))
  :pinned
  (:cost 8) 
  (:delay 1)
  (:emitter (emit-x-form-inst new-assem:segment 31 
                              (reg-tn-encoding rs) 
                              (reg-tn-encoding ra) 
                              (reg-tn-encoding rb) 
                              661 
                              0)))
(define-x-5-st-instruction stwbrx 31 662 nil)
(define-x-23-st-instruction stfsx 31 663)
(define-x-23-st-instruction stfsux 31 695 :other-dependencies ((writes ra)))
(define-instruction stswi (segment rs ra nb)
  (:printer x-8 ((op 31) (xo 725)))
  :pinned
  (:delay 1)
  (:emitter
   (emit-x-form-inst segment 31
                     (reg-tn-encoding rs) 
                     (reg-tn-encoding ra)
                     nb
                     725
                     0)))

(define-x-23-st-instruction stfdx 31 727)
(define-x-23-st-instruction stfdux 31 759 :other-dependencies ((writes ra)))
(define-x-instruction lhbrx 31 790)
(define-2-x-5-instructions sraw 31 792)

(define-instruction srawi (segment ra rs rb)
  (:printer x-9 ((op 31) (xo 824) (rc 0)))
  (:cost 1)
  (:delay 1)
  (:dependencies (reads rs) (writes ra))
  (:emitter
   (emit-x-form-inst segment 31
                        (reg-tn-encoding rs) 
                        (reg-tn-encoding ra)
                        rb
                        824
                        0)))

(define-instruction srawi. (segment ra rs rb)
  (:printer x-9 ((op 31) (xo 824) (rc 1)))
  (:cost 1)
  (:delay 1)
  (:dependencies (reads rs) (writes ra))
  (:emitter
   (emit-x-form-inst segment 31
                        (reg-tn-encoding rs) 
                        (reg-tn-encoding ra)
                        rb
                        824
                        1)))

(define-instruction eieio (segment)
  (:printer x-27 ((op 31) (xo 854)))
  :pinned
  (:delay 1)
  (:emitter (emit-x-form-inst segment 31 0 0 0 854 0)))

(define-x-5-st-instruction sthbrx 31 918 nil)


(define-2-x-10-instructions extsb 31 954)
(define-2-x-10-instructions extsh 31 922)
; Whew.

(define-instruction lwz (segment rt ra si)
  (:declare (type (or fixup (signed-byte 16)) si))
  (:printer d ((op 32)))
  (:delay 2)
  (:cost 2)
  (:dependencies (reads ra) (writes rt))
  (:emitter
   (when (typep si 'fixup)
     (note-fixup segment :l si)
     (setq si 0))
   (emit-d-form-inst segment 32 (reg-tn-encoding rt) (reg-tn-encoding ra) si)))

(define-d-instruction lwzu 33 :other-dependencies ((writes ra)))
(define-d-instruction lbz 34)
(define-d-instruction lbzu 35 :other-dependencies ((writes ra)))
(define-d-rs-instruction stw 36)
(define-d-rs-instruction stwu 37 :other-dependencies ((writes ra)))
(define-d-rs-instruction stb 38)
(define-d-rs-instruction stbu 39 :other-dependencies ((writes ra)))
(define-d-instruction lhz 40)
(define-d-instruction lhzu 41 :other-dependencies ((writes ra)))
(define-d-instruction lha 42)
(define-d-instruction lhau 43 :other-dependencies ((writes ra)))
(define-d-rs-instruction sth 44)
(define-d-rs-instruction sthu 45 :other-dependencies ((writes ra)))
(define-d-instruction lmw 46 :pinned t)
(define-d-rs-instruction stmw 47 :pinned t)
(define-d-frt-instruction lfs 48)
(define-d-frt-instruction lfsu 49 :other-dependencies ((writes ra)))
(define-d-frt-instruction lfd 50)
(define-d-frt-instruction lfdu 51 :other-dependencies ((writes ra)))
(define-d-frs-instruction stfs 52)
(define-d-frs-instruction stfsu 53 :other-dependencies ((writes ra)))
(define-d-frs-instruction stfd 54)
(define-d-frs-instruction stfdu 55 :other-dependencies ((writes ra)))

(define-2-a-tab-instructions fdivs 59 18 :cost 17)
(define-2-a-tab-instructions fsubs 59 20)
(define-2-a-tab-instructions fadds 59 21)
(define-2-a-tac-instructions fmuls 59 25)
(define-2-a-instructions fmsubs 59 28 :cost 4)
(define-2-a-instructions fmadds 59 29 :cost 4)
(define-2-a-instructions fnmsubs 59 30 :cost 4)
(define-2-a-instructions fnmadds 59 31 :cost 4)

(define-instruction fcmpu (segment crfd fra frb)
  (:printer x-15 ((op 63) (xo 0)))
  (:dependencies (reads fra) (reads frb) (reads :fpscr) 
                 (writes :fpscr) (writes :ccr))
  (:cost 4)
  (:delay 4)
  (:emitter (emit-x-form-inst segment 
                          63 
                          (valid-cr-field-encoding crfd)
                          (fp-reg-tn-encoding fra) 
                          (fp-reg-tn-encoding frb)
                          0
                          0)))


(define-2-x-21-instructions frsp 63 12)
(define-2-x-21-instructions fctiw 63 14)
(define-2-x-21-instructions fctiwz 63 15)

(define-2-a-tab-instructions fdiv 63 18 :cost 31)
(define-2-a-tab-instructions fsub 63 20)
(define-2-a-tab-instructions fadd 63 21)
(define-2-a-tac-instructions fmul 63 25 :cost 5)
(define-2-a-instructions fmsub 63 28 :cost 5)
(define-2-a-instructions fmadd 63 29 :cost 5)
(define-2-a-instructions fnmsub 63 30 :cost 5)
(define-2-a-instructions fnmadd 63 31 :cost 5)

(define-instruction fcmpo (segment crfd fra frb)
  (:printer x-15 ((op 63) (xo 32)))
  (:dependencies (reads fra) (reads frb) (reads :fpscr) 
                 (writes :fpscr) (writes :ccr))
  (:cost 4)
  (:delay 1)
  (:emitter (emit-x-form-inst segment 
                          63 
                          (valid-cr-field-encoding crfd)
                          (fp-reg-tn-encoding fra) 
                          (fp-reg-tn-encoding frb)
                          32
                          0)))

(define-2-x-21-instructions fneg 63 40)

(define-2-x-21-instructions fmr 63 72)
(define-2-x-21-instructions fnabs 63 136)
(define-2-x-21-instructions fabs 63 264)

(define-instruction mffs (segment frd)
  (:printer x-22 ((op 63)  (xo 583) (rc 0)))
  (:delay 1)
  (:dependencies (reads :fpscr) (writes frd))
  (:emitter (emit-x-form-inst segment 
                          63 
                          (fp-reg-tn-encoding frd)
                          0 
                          0
                          583
                          0)))

(define-instruction mffs. (segment frd)
  (:printer x-22 ((op 63)  (xo 583) (rc 1)))
  (:delay 1)
  (:dependencies (reads :fpscr) (writes frd) (writes :ccr))
  (:emitter (emit-x-form-inst segment 
                          63 
                          (fp-reg-tn-encoding frd)
                          0 
                          0
                          583
                          1)))

(define-instruction mtfsf (segment mask rb)
  (:printer xfl ((op 63) (xo 711) (rc 0)))
  (:dependencies (reads rb) (writes :fpscr))
  (:delay 1)
  (:emitter (emit-xfl-form-inst segment 63  (ash mask 1) (fp-reg-tn-encoding rb) 711 0)))

(define-instruction mtfsf. (segment mask rb)
  (:printer xfl ((op 63) (xo 711) (rc 1)))
  (:delay 1)
  (:dependencies (reads rb) (writes :ccr) (writes :fpscr))
  (:emitter (emit-xfl-form-inst segment 63  (ash mask 1) (fp-reg-tn-encoding rb) 711 1)))




;;; Here in the future, macros are our friends.

(define-instruction-macro subis (rt ra simm)
  `(inst addis ,rt ,ra (- ,simm)))

(define-instruction-macro sub (rt rb ra)
  `(inst subf ,rt ,ra ,rb))
(define-instruction-macro sub. (rt rb ra)
  `(inst subf. ,rt ,ra ,rb))
(define-instruction-macro subo (rt rb ra)
  `(inst subfo ,rt ,ra ,rb))
(define-instruction-macro subo. (rt rb ra)
  `(inst subfo. ,rt ,ra ,rb))


(define-instruction-macro subic (rt ra simm)
  `(inst addic ,rt ,ra (- ,simm)))


(define-instruction-macro subic. (rt ra simm)
  `(inst addic. ,rt ,ra (- ,simm)))



(define-instruction-macro subc (rt rb ra)
  `(inst subfc ,rt ,ra ,rb))
(define-instruction-macro subc. (rt rb ra)
  `(inst subfc. ,rt ,ra ,rb))
(define-instruction-macro subco (rt rb ra)
  `(inst subfco ,rt ,ra ,rb))
(define-instruction-macro subco. (rt rb ra)
  `(inst subfco. ,rt ,ra ,rb))

(define-instruction-macro subi (rt ra simm)
  `(inst addi ,rt ,ra (- ,simm)))

(define-instruction-macro li (rt val)
  `(inst addi ,rt zero-tn ,val))

(define-instruction-macro lis (rt val)
  `(inst addis ,rt zero-tn ,val))


(define-instruction-macro not (ra rs)
  `(inst nor ,ra ,rs ,rs))

(define-instruction-macro not. (ra rs)
  `(inst nor. ,ra ,rs ,rs))


(def-vm-support-routine emit-nop (segment)
  (emit-word segment #x60000000))

(define-instruction-macro extlwi (ra rs n b)
  `(inst rlwinm ,ra ,rs ,b 0 (1- ,n)))

(define-instruction-macro extlwi. (ra rs n b)
  `(inst rlwinm. ,ra ,rs ,b 0 (1- ,n)))

(define-instruction-macro srwi (ra rs n)
  `(inst rlwinm ,ra ,rs (- 32 ,n) ,n 31))

(define-instruction-macro srwi. (ra rs n)
  `(inst rlwinm. ,ra ,rs (- 32 ,n) ,n 31))

(define-instruction-macro clrrwi (ra rs n)
  `(inst rlwinm ,ra ,rs 0 0 (- 31 ,n)))

(define-instruction-macro clrrwi. (ra rs n)
  `(inst rlwinm. ,ra ,rs 0 0 (- 31 ,n)))

(define-instruction-macro inslw (ra rs n b)
  `(inst rlwimi ,ra ,rs (- 32 ,b) ,b (+ ,b (1- ,n))))

(define-instruction-macro inslw. (ra rs n b)
  `(inst rlwimi. ,ra ,rs (- 32 ,b) ,b (+ ,b (1- ,n))))

(define-instruction-macro rotlw (ra rs rb)
  `(inst rlwnm ,ra ,rs ,rb 0 31))

(define-instruction-macro rotlw. (ra rs rb)
  `(inst rlwnm. ,ra ,rs ,rb 0 31))

(define-instruction-macro slwi (ra rs n)
  `(inst rlwinm ,ra ,rs ,n 0 (- 31 ,n)))

(define-instruction-macro slwi. (ra rs n)
  `(inst rlwinm. ,ra ,rs ,n 0 (- 31 ,n)))




#|
(macrolet 
  ((define-conditional-branches (name bo-name)
     (let* ((bo-enc (valid-bo-encoding bo-name)))
       `(progn
          (define-instruction-macro ,(symbolicate name "A") (bi target)
            ``(inst bca ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "L") (bi target)
            ``(inst bcl ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "LA") (bi target)
            ``(inst bcla ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "CTR") (bi target)
            ``(inst bcctr ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "CTRL") (bi target)
            ``(inst bcctrl ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "LR") (bi target)
            ``(inst bclr ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "LRL") (bi target)
            ``(inst bclrl ,,,bo-enc ,,bi ,,target))))))
  (define-conditional-branches bt :bo-t)
  (define-conditional-branches bf :bo-f))
|#

(macrolet 
  ((define-positive-conditional-branches (name cr-bit-name)
     `(progn
        (define-instruction-macro ,name (crf &optional (target nil target-p))
          (unless target-p
            (setq target crf crf :cr0))
          `(inst bt `(,,crf ,,,cr-bit-name) ,target))
#|
        (define-instruction-macro ,(symbolicate name "A") (target &optional (cr-field :cr0))
          ``(inst bta (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "L") (target &optional (cr-field :cr0))
          ``(inst btl (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LA") (target &optional (cr-field :cr0))
          ``(inst btla (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "CTR") (target &optional (cr-field :cr0))
          ``(inst btctr (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "CTRL") (target &optional (cr-field :cr0))
          ``(inst btctrl (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LR") (target &optional (cr-field :cr0))
          ``(inst btlr (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LRL") (target &optional (cr-field :cr0))
          ``(inst btlrl (,,cr-field ,,,cr-bit-name) ,,target))
|#
        )))
  (define-positive-conditional-branches beq :eq)
  (define-positive-conditional-branches blt :lt)
  (define-positive-conditional-branches bgt :gt)
  (define-positive-conditional-branches bso :so)
  (define-positive-conditional-branches bun :so))

(macrolet 
  ((define-negative-conditional-branches (name cr-bit-name)
     `(progn
        (define-instruction-macro ,name (crf &optional (target nil target-p))
          (unless target-p
            (setq target crf crf :cr0))
          `(inst bf `(,,crf ,,,cr-bit-name) ,target))
#|
        (define-instruction-macro ,(symbolicate name "A") (target &optional (cr-field :cr0))
          ``(inst bfa (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "L") (target &optional (cr-field :cr0))
          ``(inst bfl (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LA") (target &optional (cr-field :cr0))
          ``(inst bfla (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "CTR") (target &optional (cr-field :cr0))
          ``(inst bfctr (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "CTRL") (target &optional (cr-field :cr0))
          ``(inst bfctrl (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LR") (target &optional (cr-field :cr0))
          ``(inst bflr (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LRL") (target &optional (cr-field :cr0))
          ``(inst bflrl (,,cr-field ,,,cr-bit-name) ,,target))
|#
)))
  (define-negative-conditional-branches bne :eq)
  (define-negative-conditional-branches bnl :lt)
  (define-negative-conditional-branches bge :lt)
  (define-negative-conditional-branches bng :gt)
  (define-negative-conditional-branches ble :gt)
  (define-negative-conditional-branches bns :so)
  (define-negative-conditional-branches bnu :so))



(define-instruction-macro j (func-tn offset)
  `(progn
     (inst addi lip-tn ,func-tn ,offset)
     (inst mtctr lip-tn)
     (inst bctr)))


#|
(define-instruction-macro bua (target)
  `(inst bca :bo-u 0 ,target))

(define-instruction-macro bul (target)
  `(inst bcl :bo-u 0 ,target))

(define-instruction-macro bula (target)
  `(inst bcla :bo-u 0 ,target))

|#


;;; Some more macros 

(defun %lr (reg value)
  (etypecase value
    ((signed-byte 16)
     (inst li reg value))
    ((unsigned-byte 16)
     (inst ori reg zero-tn value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (let* ((high-half (ldb (byte 16 16) value))
            (low-half (ldb (byte 16 0) value)))
       (declare (type (unsigned-byte 16) high-half low-half))
       (cond ((if (logbitp 15 low-half) (= high-half #xffff) (zerop high-half))
              (inst li reg low-half))
             (t
              (inst lis reg high-half)
              (unless (zerop low-half)
                (inst ori reg reg low-half))))))
    (fixup
     (inst lis reg value)
     (inst addi reg reg value))))

(define-instruction-macro lr (reg value)
  `(%lr ,reg ,value))
     


;;;; Instructions for dumping data and header objects.

(define-instruction word (segment word)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) word))
  :pinned
  (:delay 0)
  (:emitter
   (emit-word segment word)))

(define-instruction short (segment short)
  (:declare (type (or (unsigned-byte 16) (signed-byte 16)) short))
  :pinned
  (:delay 0)
  (:emitter
   (emit-short segment short)))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:delay 0)
  (:emitter
   (emit-byte segment byte)))

(define-emitter emit-header-object 32
  (byte 24 8) (byte 8 0))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (emit-word segment
		  (logior type
			  (ash (+ posn (component-header-length))
			       (- type-bits word-shift)))))))

(define-instruction function-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment function-header-type)))

(define-instruction lra-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment return-pc-header-type)))


;;;; Instructions for converting between code objects, functions, and lras.
(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 8 byte alignments.
   segment 12 3
   #'(lambda (segment posn delta-if-after)
       (let ((delta (funcall calc label posn delta-if-after)))
	 (when (typep delta '(signed-byte 16))
	   (emit-back-patch segment 4
			    #'(lambda (segment posn)
				(assemble (segment vop)
					  (inst addi dst src
						(funcall calc label posn 0)))))
	   t)))
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
	 (assemble (segment vop)
		   (inst lis temp (ldb (byte 16 16) delta))
		   (inst ori temp temp (ldb (byte 16 0) delta))
		   (inst add dst src temp))))))

;; code = fn - fn-ptr-type - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-fn (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- other-pointer-type
			     (label-position label posn delta-if-after)
			     (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- (+ (label-position label posn delta-if-after)
				(component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (+ (label-position label posn delta-if-after)
			     (component-header-length))))))
(define-instruction mftb (segment dst)
  (:printer x-9 ((op 31) (ra #b01100) (sh #b01000) (xo 371) (rc 0))
                 '(:name :tab rs))
  (:dependencies (writes dst))
  (:emitter (emit-x-form-inst segment 
			      31
			      (reg-tn-encoding dst)
			      #b01100
			      #b01000
			      371 0)))

(define-instruction mftbu (segment dst)
  (:printer x-9 ((op 31) (ra #b01101) (sh #b01000) (xo 371) (rc 0))
                 '(:name :tab rs))
  (:dependencies (writes dst))
  (:emitter (emit-x-form-inst segment
			      31
			      (reg-tn-encoding dst)
			      #b01101
			      #b01000
			      371 0)))
