;;; -*- Mode: lisp; Package: PRETTY-PRINT -*-
;;; Pretty printer for loop and various other utilities.
;;; This is taken from Dick Water's XP.  The license contained therein says:
;;;
;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted,
;;; provided that this copyright and permission notice appear in all
;;; copies and supporting documentation, and that the name of M.I.T. not
;;; be used in advertising or publicity pertaining to distribution of the
;;; software without specific, written prior permission. M.I.T. makes no
;;; representations about the suitability of this software for any
;;; purpose.  It is provided "as is" without express or implied warranty.
;;;
;;;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;    SOFTWARE.

;; The challenge here is that we have to effectively parse the clauses
;; of the loop in order to know how to print things.  Also you want to
;; do this in a purely incremental way so that all of the abbreviation
;; things work, and you wont blow up on circular lists or the like.
;; (More aesthic output could be produced by really parsing the
;; clauses into nested lists before printing them.)
;;
;; The following program assumes the following simplified grammar of
;; the loop clauses that explains how to print them.  Note that it
;; does not bare much resemblence to the right parsing grammar,
;; however, it produces half decent output.  The way to make the
;; output better is to make the grammar more detailed.
;;
;; loop == (LOOP {clause}*)      ;one clause on each line.
;; clause == block | linear | cond | finally
;; block == block-head {expr}*   ;as many exprs as possible on each line.
;; linear == linear-head {expr}* ;one expr on each line.
;; finally == FINALLY [DO | DOING | RETURN] {expr}* ;one expr on each line.
;; cond == cond-head [expr]
;;           clause
;;	    {AND clause}*       ;one AND on each line.
;;         [ELSE
;;           clause
;;	    {AND clause}*]      ;one AND on each line.
;;         [END]
;; block-head == FOR | AS | WITH | AND
;;               | REPEAT | NAMED | WHILE | UNTIL | ALWAYS | NEVER | THEREIS | RETURN
;;               | COLLECT | COLLECTING | APPEND | APPENDING | NCONC | NCONCING | COUNT
;;               | COUNTING | SUM | SUMMING | MAXIMIZE | MAXIMIZING | MINIMIZE | MINIMIZING 
;; linear-head == DO | DOING | INITIALLY
;; var-head == FOR | AS | WITH
;; cond-head == IF | WHEN | UNLESS
;; expr == <anything that is not a head symbol>
;;
;; Note all the string comparisons below are required to support some
;; existing implementations of LOOP.

(in-package "PRETTY-PRINT")

(defun pprint-loop-token-type (token &aux string)
  (cond ((not (symbolp token)) :expr)
	((string= (setq string (string token)) "FINALLY") :finally)
	((member string '("IF" "WHEN" "UNLESS") :test #'string=) :cond-head)
	((member string '("DO" "DOING" "INITIALLY") :test #'string=) :linear-head)
	((member string '("FOR" "AS" "WITH" "AND" "END" "ELSE"
			  "REPEAT" "NAMED" "WHILE" "UNTIL" "ALWAYS" "NEVER"
			  "THEREIS" "RETURN" "COLLECT" "COLLECTING" "APPEND"
			  "APPENDING" "NCONC" "NCONCING" "COUNT" "COUNTING"
			  "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
			  "MINIMIZE" "MINIMIZING")
		 :test #'string=)
	 :block-head)
	(T :expr)))

(defun pprint-loop (xp loop)
  (if (not (and (consp (cdr loop)) (symbolp (cadr loop)))) ; old-style loop
      (funcall (formatter "~:<~W~^~2I~@:_~@{~W~^~_~}~:>")
	       xp loop)
      (progn
	(pprint-logical-block (xp loop :prefix "(" :suffix ")")
	  (let (token type)
	    (labels ((next-token ()
		       (pprint-exit-if-list-exhausted)
		       (setq token (pprint-pop))
		       (setq type (pprint-loop-token-type token)))
		     (print-clause (xp)
		       (case type
			 (:linear-head (print-exprs xp nil :mandatory))
			 (:cond-head (print-cond xp))
			 (:finally (print-exprs xp T :mandatory))
			 (otherwise (print-exprs xp nil :fill))))
		     (print-exprs (xp skip-first-non-expr newline-type)
		       (let ((first token))
			 (next-token) ;so always happens no matter what
			 (pprint-logical-block (xp nil)
			   (write first :stream xp)
			   (when (and skip-first-non-expr (not (eq type :expr)))
			     (write-char #\space xp)
			     (write token :stream xp)
			     (next-token))
			   (when (eq type :expr)
			     (write-char #\space xp)
			     (pprint-indent :current 0 xp)
			     (loop (write token :stream xp)
				(next-token)
				(when (not (eq type :expr))
				  (return nil))
				(write-char #\space xp)
				(pprint-newline newline-type xp))))))
		     (print-cond (xp)
		       (let ((first token))
			 (next-token) ;so always happens no matter what
			 (pprint-logical-block (xp nil)
			   (write first :stream xp)
			   (when (eq type :expr)
			     (write-char #\space xp)
			     (write token :stream xp)
			     (next-token))
			   (write-char #\space xp)
			   (pprint-indent :block 2 xp)
			   (pprint-newline :linear xp)
			   (print-clause xp)
			   (print-and-list xp)
			   (when (and (symbolp token)
				      (string= (string token) "ELSE"))
			     (print-else-or-end xp)
			     (write-char #\space xp)
			     (pprint-newline :linear xp)
			     (print-clause xp)
			     (print-and-list xp))
			   (when (and (symbolp token)
				      (string= (string token) "END"))
			     (print-else-or-end xp)))))
		     (print-and-list (xp)
		       (loop (when (not (and (symbolp token)
					     (string= (string token) "AND")))
			       (return nil))
			  (write-char #\space xp)
			  (pprint-newline :mandatory xp)
			  (write token :stream xp)
			  (next-token)
			  (write-char #\space xp)
			  (print-clause xp)))
		     (print-else-or-end (xp)
		       (write-char #\space xp)
		       (pprint-indent :block 0 xp)
		       (pprint-newline :linear xp)
		       (write token :stream xp)
		       (next-token)
		       (pprint-indent :block 2 xp)))
	      ;;(pprint-exit-if-list-exhausted)
	      (write (pprint-pop) :stream xp)
	      (next-token)
	      (write-char #\space xp)
	      (pprint-indent :current 0 xp)
	      (loop (print-clause xp)
		 (write-char #\space xp)
		 (pprint-newline :linear xp))))))))

;;; LOOP-PP-INIT -- interface
;;;
;;; This is called by PPRINT-INIT.  This enables the pretty printer
;;; for loops.  We need this so that loop pprinter is enabled at the
;;; right time, since this is in a file separate from pprint.lisp.
;;;
(defun loop-pp-init ()
  (set-pprint-dispatch '(cons (eql loop)) #'pprint-loop))
