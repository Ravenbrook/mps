;
;************************************************************************
;
;	VPS2 -- Interpreter for OPS5
;
;
;
; This Common Lisp version of OPS5 is in the public domain.  It is based
; in part on based on a Franz Lisp implementation done by Charles L. Forgy
; at Carnegie-Mellon University, which was placed in the public domain by
; the author in accordance with CMU policies.  This version has been
; modified by George Wood, Dario Giuse, Skef Wholey, Michael Parzen,
; and Dan Kuokka.
;
; This code is made available is, and without warranty of any kind by the
; authors or by Carnegie-Mellon University.
;

;;;; This file contains all the functions pertaining to I/O.

(in-package "OPS")
(shadow '(write))    ; Should get this by requiring ops-rhs


;;; Internal global variables.

(defvar *write-file*)
(defvar *trace-file*)
(defvar *accept-file*)
(defvar *ppline*)
(defvar *filters*)



;;; Initialization

(defun io-init ()
  (setq *write-file* nil)
  (setq *trace-file* nil)
  (setq *accept-file* nil))



;;; User I/O commands
;;; Dario Giuse - rewrote the (write) function to follow OPS-5 specifications.
;;; Michael Huhns fixed a few bugs in this rewrttien functions some years later.


(defmacro append-string (x)
  `(setq wrstring (concatenate 'simple-string wrstring ,x)))


(defun ops-write (z)
  (prog (port max k x)
    (cond ((not *in-rhs*)
	   (%warn '|cannot be called at top level| 'write)
	   (return nil)))
    ($reset)
    (eval-args z)
    (setq max ($parametercount))
    (cond ((< max 1)
	   (%warn '|write: nothing to print| z)
	   (return nil)))
    (setq x ($parameter 1))
    (cond ((and (symbolp x) ($ofile x)) 
	   (setq port ($ofile x))
	   (setq k 2))
	  (t
	   (setq port (default-write-file))
	   (setq k 1)))
    ;; Analyze and output all the parameters (write) was passed.
    (do* ((wrstring "")
	  (x ($parameter k) ($parameter k))
	  (field-width))
	 ((> k max)
	  (format port wrstring)
	  (force-output))     ; Dario Giuse - added to force output
      (incf k)
      (case x
	(|=== C R L F ===|
	 (format port "~A~%" wrstring)     ; Flush the previous line
	 (setq wrstring ""))
	(|=== R J U S T ===|
	 (setq field-width ($parameter k))           ; Number following (tabto)
	 (incf k)
	 (setq x (format nil "~A" ($parameter k)))   ; Next field to print
	 (when (<= (length x) field-width)
	   ;; Right-justify field
	   (append-string (format nil "~V@A" field-width x))
	   (incf k)))   ; Skip next field, since we printed it already
	(|=== T A B T O ===|
	 (setq x ($parameter k))         ; Position to tab to
	 (incf k)
	 (when (< x (length wrstring))
	   ;; Flush line, start a new one
	   (format port "~A~%" wrstring)
	   (setq wrstring ""))
	 (append-string (format nil "~V,1@T" (- x (length wrstring) 1))))
	(t
	 (append-string (format nil "~A " x)))))))


(defun ops-openfile (z)
  (prog (file mode id)
    ($reset)
    (eval-args z)
    (cond ((not (equal ($parametercount) 3.))
	   (%warn '|openfile: wrong number of arguments| z)
	   (return nil)))
    (setq id ($parameter 1))
    (setq file ($parameter 2))
    (setq mode ($parameter 3))
    (cond ((not (symbolp id))
	   (%warn '|openfile: file id must be a symbolic atom| id)
	   (return nil))
	  ((null id)
	   (%warn '|openfile: 'nil' is reserved for the terminal| nil)
	   (return nil))
	  ((or ($ifile id)($ofile id))
	   (%warn '|openfile: name already in use| id)
	   (return nil)))
;@@@	(cond ((eq mode 'in) (putprop id (infile file) 'inputfile))
;@@@	      ((eq mode 'out) (putprop id (outfile file) 'outputfile))
; dec 7 83 gdw added setq : is putprop needed ? )
    (cond ((eq mode 'in) (putprop id (setq id (infile file)) 'inputfile))
	  ((eq mode 'out) (putprop id (setq id (outfile file)) 'outputfile))
	  (t (%warn '|openfile: illegal mode| mode)
	     (return nil)))
    (return nil)))


(defun infile (f_name)
  (open f_name :direction :input))

(defun outfile (f_name)
  (open f_name :direction :output :if-exists :new-version))

(defun ops-closefile (z)
  ($reset)
  (eval-args z)
  (mapc (function closefile2) (use-result-array)))

(defun closefile2 (file)
  (prog (port)
    (cond ((not (symbolp file))
	   (%warn '|closefile: illegal file identifier| file))
	  ((setq port ($ifile file))
	   (close port)
	   (remprop file 'inputfile))
	  ((setq port ($ofile file))
	   (close port)
	   (remprop file 'outputfile)))
    (return nil)))


(defun ops-default (z)
  (prog (file use)
    ($reset)
    (eval-args z)
    (cond ((not (equal ($parametercount) 2.))
	   (%warn '|default: wrong number of arguments| z)
	   (return nil)))
    (setq file ($parameter 1))
    (setq use ($parameter 2))
    (cond ((not (symbolp file))
	   (%warn '|default: illegal file identifier| file)
	   (return nil))
	  ((not (member use '(write accept trace) :test #'equal))
	   (%warn '|default: illegal use for a file| use)
	   (return nil))
	  ((and (member use '(write trace) :test #'equal)
		(not (null file))
		(not ($ofile file)))
	   (%warn '|default: file has not been opened for output| file)
	   (return nil))
	  ((and (equal use 'accept) 
		(not (null file))
		(not ($ifile file)))
	   (%warn '|default: file has not been opened for input| file)
	   (return nil))
	  ((equal use 'write) (setq *write-file* file))
	  ((equal use 'accept) (setq *accept-file* file))
	  ((equal use 'trace) (setq *trace-file* file)))
    (return nil)))


(defun ops-accept (z)
  (prog (port arg)
    (cond ((> (length z) 1.)
	   (%warn '|accept: wrong number of arguments| z)
	   (return nil)))
    (setq port *standard-input*)
    (cond (*accept-file*
	   (setq port ($ifile *accept-file*))
	   (cond ((null port) 
		  (%warn '|accept: file has been closed| *accept-file*)
		  (return nil)))))
    (cond ((= (length z) 1)
	   (setq arg ($varbind (car z)))
	   (cond ((not (symbolp arg))
		  (%warn '|accept: illegal file name| arg)
		  (return nil)))
	   (setq port ($ifile arg))
	   (cond ((null port) 
		  (%warn '|accept: file not open for input| arg)
		  (return nil)))))
    (cond ((equal (peek-char t port nil "eof" ) "eof" )
	   ($value 'end-of-file)
	   (return nil)))
    (flat-value (read port)))) 



;;; Dario Giuse - completely changed the algorithm. It now uses one read-line
;;; and the read-from-string.
;;;
(defun ops-acceptline (z)
  (let ((port *standard-input*)
	(def z))
    (cond (*accept-file*
	   (setq port ($ifile *accept-file*))
	   (cond ((null port) 
		  (%warn '|acceptline: file has been closed| 
			 *accept-file*)
		  (return-from ops-acceptline nil)))))
    (cond ((> (length def) 0)
	   (let ((arg ($varbind (car def))))
	     (cond ((and (symbolp arg) ($ifile arg))
		    (setq port ($ifile arg))
		    (setq def (cdr def)))))))
    (let ((line (read-line port nil 'eof)))
      (declare (simple-string line))
      ;; Strip meaningless characters from start and end of string.
      (setq line (string-trim '(#\( #\) #\, #\tab #\space) line))
      (when (equal line "")
	(mapc (function $change) def)
	(return-from ops-acceptline nil))
      (setq line (concatenate 'simple-string "(" line ")"))
      ;; Read all items from the line
      (flat-value (read-from-string line)))))




(defun ops-rjust (z)
  (prog (val)
    (cond ((not (= (length z) 1.))
	   (%warn '|rjust: wrong number of arguments| z)
	   (return nil)))
    (setq val ($varbind (car z)))
    (cond ((or (not (numberp val)) (< val 1.) (> val 127.))
	   (%warn '|rjust: illegal value for field width| val)
	   (return nil)))
    ($value '|=== R J U S T ===|)
    ($value val)))


(defun ops-crlf (z)
  (cond  (z (%warn '|crlf: does not take arguments| z))
	 (t ($value '|=== C R L F ===|))))


(defun ops-tabto (z)
  (prog (val)
    (cond ((not (= (length z) 1.))
	   (%warn '|tabto: wrong number of arguments| z)
	   (return nil)))
    (setq val ($varbind (car z)))
    (cond ((or (not (numberp val)) (< val 1.) (> val 127.))
	   (%warn '|tabto: illegal column number| z)
	   (return nil)))
    ($value '|=== T A B T O ===|)
    ($value val)))



(defun do-rjust (width value port)
  (prog (size)
    (cond ((eq value '|=== T A B T O ===|)
	   (%warn '|rjust cannot precede this function| 'tabto)
	   (return nil))
	  ((eq value '|=== C R L F ===|)
	   (%warn '|rjust cannot precede this function| 'crlf)
	   (return nil))
	  ((eq value '|=== R J U S T ===|)
	   (%warn '|rjust cannot precede this function| 'rjust)
	   (return nil)))
    ;original->        (setq size (flatc value (1+ width)))
    (setq size (min value (1+ width)))  ;### KLUGE
    (cond ((> size width)
	   (princ '| | port)
	   (princ value port)
	   (return nil)))
    ;###        (do k (- width size) (1- k) (not (> k 0)) (princ '| | port))
    ;^^^KLUGE @@@do
    (princ value port)))

(defun do-tabto (col port)
  (prog (pos)
    ;### KLUGE: FLUSHES STREAM & SETS POS TO 0
    ;OIRGINAL->	(setq pos (1+ (nwritn port)))	;hmm-takes 1 arg @@@ port
    (finish-output port);kluge
    (setq pos 0);kluge
    (cond ((> pos col)
	   (terpri port)
	   (setq pos 1)))
    ;###(do k (- col pos) (1- k) (not (> k 0)) (princ '| | port))
    ;^^^KLUGE @@@do
    (return nil)))


(defun flat-value (x)
  (cond ((atom x) ($value x))
	(t (mapc (function flat-value) x)))) 



;;; Printing WM

(defun ops-ppwm (avlist)
  (prog (next a)
    (setq *filters* nil)
    (setq next 1.)
    loop   (and (atom avlist) (go print))
    (setq a (car avlist))
    (setq avlist (cdr avlist))
    ;this must be expecting (ppwm class ^ attr ^ attr2 ...) not ^attr
    (cond ((eq a '^)
	   (setq next (car avlist))
	   (setq avlist (cdr avlist))
	   (setq next ($litbind next))
	   (and (floatp next) (setq next (fix next)))
	   (cond ((or (not (numberp next))
		      (> next *size-result-array*)
		      (> 1. next))
		  (%warn '|illegal index after ^| next)
		  (return nil))))
	  ((variablep a)
	   (%warn '|ppwm does not take variables| a)
	   (return nil))
	  (t (setq *filters* (cons next (cons a *filters*)))
	     (setq next (1+ next))))
    (go loop)
    print (mapwm (function ppwm2))
    (terpri)
    (return nil))) 


(defun default-write-file ()
  (prog (port)
    (setq port *standard-output*)
    (cond (*write-file*
	   (setq port ($ofile *write-file*))
	   (cond ((null port) 
		  (%warn '|write: file has been closed| *write-file*)
		  (setq port *standard-output*)))))
    (return port)))


(defun trace-file ()
  (prog (port)
    (setq port *standard-output*)
    (cond (*trace-file*
	   (setq port ($ofile *trace-file*))
	   (cond ((null port)
		  (%warn '|trace: file has been closed| *trace-file*)
		  (setq port *standard-output*)))))
    (return port)))


(defun ppwm2 (elm-tag)
  (cond ((filter (car elm-tag))
	 (terpri) (ppelm (car elm-tag) (default-write-file))))) 

(defun filter (elm)
  (prog (fl indx val)
    (setq fl *filters*)
    top  (and (atom fl) (return t))
    (setq indx (car fl))
    (setq val (cadr fl))
    (setq fl (cddr fl))
    (and (ident (nth (1- indx) elm) val) (go top))
    (return nil))) 

(defun ident (x y)
  (cond ((eq x y) t)
	((not (numberp x)) nil)
	((not (numberp y)) nil)
	((=alg x y) t)
	(t nil))) 

; the new ppelm is designed especially to handle literalize format
; however, it will do as well as the old ppelm on other formats

(defun ppelm (elm port)
  (prog (ppdat sep val att mode lastpos)
    (princ (creation-time elm) port)
    (princ '|:  | port)
    (setq mode 'vector)
    (setq ppdat (get (car elm) 'ppdat))
    (and ppdat (setq mode 'a-v))
    (setq sep "(")				; ")" 
    (setq lastpos 0)
    (do ((curpos 1 (1+ curpos)) (vlist elm (cdr vlist)))
	((atom vlist) nil)					; terminate
      (setq val (car vlist))				; tagbody begin
      (setq att (assoc curpos ppdat))	;should ret (curpos attr-name) 
      (cond (att (setq att (cdr att)))	; att = (attr-name) ??
	    (t (setq att curpos)))
      (and (symbolp att) (is-vector-attribute att) (setq mode 'vector))
      (cond ((or (not (null val)) (eq mode 'vector))
	     (princ sep port)
	     (ppval val att lastpos port)
	     (setq sep '|    |)
	     (setq lastpos curpos))))
    (princ '|)| port)))

(defun ppval (val att lastpos port)
  ;  (break "in ppval")		
  (cond ((not (equal att (1+ lastpos)))		; ok, if we got an att 
	 (princ '^ port)
	 (princ att port)
	 (princ '| | port)))
  (princ val port))



;;; Printing production memory

(defun ops-pm (z) (mapc (function pprule) z) (terpri) nil)

(defun pprule (name)
  (prog (matrix next lab)
    (and (not (symbolp name)) (return nil))
    (setq matrix (get name 'production))
    (and (null matrix) (return nil))
    (terpri)
    (princ '|(p |)      ;)
    (princ name)
    top	(and (atom matrix) (go fin))
    (setq next (car matrix))
    (setq matrix (cdr matrix))
    (setq lab nil)
    (terpri)
    (cond ((eq next '-)
	   (princ '|  - |)
	   (setq next (car matrix))
	   (setq matrix (cdr matrix)))
	  ((eq next '-->)
	   (princ '|  |))
	  ((and (eq next '{) (atom (car matrix)))
	   (princ '|   {|)
	   (setq lab (car matrix))
	   (setq next (cadr matrix))
	   (setq matrix (cdddr matrix)))
	  ((eq next '{)
	   (princ '|   {|)
	   (setq lab (cadr matrix))
	   (setq next (car matrix))
	   (setq matrix (cdddr matrix)))
	  (t (princ '|    |)))
    (ppline next)
    (cond (lab (princ '| |) (princ lab) (princ '})))
    (go top)
    fin	(princ '|)|)))

(defun ppline (line)
  (prog ()
    (cond ((atom line) (princ line))
	  (t
	   (princ '|(|)      ;)
	   (setq *ppline* line)
	   (ppline2)
	   ;(
	   (princ '|)|)))
    (return nil)))

(defun ppline2 ()
  (prog (needspace)
    (setq needspace nil)
    top  (and (atom *ppline*) (return nil))
    (and needspace (princ '| |))
    (cond ((eq (car *ppline*) '^) (ppattval))
	  (t (pponlyval)))
    (setq needspace t)
    (go top)))

(defun ppattval ()
  (prog (att val)
    (setq att (cadr *ppline*))
    (setq *ppline* (cddr *ppline*))
    (setq val (getval))
    ;###	(cond ((> (+ (nwritn) (flatc att) (flatc val)) 76.)))
    ;@@@ nwritn no arg
    ;						;"plus" changed to "+" by gdw
    ;	       (terpri)
    ;	       (princ '|        |)
    (princ '^)
    (princ att)
    (mapc (function (lambda (z) (princ '| |) (princ z))) val)))

(defun pponlyval ()
  (prog (val needspace)
    (setq val (getval))
    (setq needspace nil)
    ;###	(cond ((> (+ (nwritn) (flatc val)) 76.)))
    ;"plus" changed to "+" by gdw
    ;	       (setq needspace nil)		;^nwritn no arg @@@
    ;	       (terpri)
    ;	       (princ '|        |)
    top	(and (atom val) (return nil))
    (and needspace (princ '| |))
    (setq needspace t)
    (princ (car val))
    (setq val (cdr val))
    (go top)))

(defun getval ()
  (prog (res v1)
    (setq v1 (car *ppline*))
    (setq *ppline* (cdr *ppline*))
    (cond ((member v1 '(= <> < <= => > <=>))
	   (setq res (cons v1 (getval))))
	  ((eq v1 '{)
	   (setq res (cons v1 (getupto '}))))
	  ((eq v1 '<<)
	   (setq res (cons v1 (getupto '>>))))
	  ((eq v1 '//)
	   (setq res (list v1 (car *ppline*)))
	   (setq *ppline* (cdr *ppline*)))
	  (t (setq res (list v1))))
    (return res)))

(defun getupto (end)
  (prog (v)
    (and (atom *ppline*) (return nil))
    (setq v (car *ppline*))
    (setq *ppline* (cdr *ppline*))
    (cond ((eq v end) (return (list v)))
	  (t (return (cons v (getupto end))))))) 

