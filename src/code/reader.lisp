;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/reader.lisp,v 1.61 2006/06/30 18:41:22 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Spice Lisp Reader 
;;; Written by David Dill
;;; Package system interface by Lee Schumacher.
;;; Runs in the standard Spice Lisp environment.
;;;

(in-package "EXTENSIONS")
(export '*ignore-extra-close-parentheses*)

(in-package "LISP")
(export '(readtable readtable-case readtablep *read-base* *readtable*
	  copy-readtable set-syntax-from-char set-macro-character
	  get-macro-character make-dispatch-macro-character
	  set-dispatch-macro-character get-dispatch-macro-character read
	  *read-default-float-format* read-preserving-whitespace
	  read-delimited-list parse-integer read-from-string *read-suppress*
	  reader-error))


;;;Random global variables

(defvar *read-default-float-format* 'single-float "Float format for 1.0E1")
(declaim (type (member short-float single-float double-float long-float)
	       *read-default-float-format*))

(defvar *readtable*)
(declaim (type readtable *readtable*))
(setf (documentation '*readtable* 'variable)
       "Variable bound to current readtable.")


;;;; Reader errors:

(define-condition reader-error (parse-error stream-error)
  ((format-control
    :reader reader-error-format-control
    :initarg :format-control)
   (format-arguments
    :reader reader-error-format-arguments
    :initarg :format-arguments
    :initform '()))
  (:report
   (lambda (condition stream)
     (let ((error-stream (stream-error-stream condition)))
       (when c:*compiler-notification-function*
         (funcall c:*compiler-notification-function* :error
                  (apply #'format nil
                         (reader-error-format-control condition)
                         (reader-error-format-arguments condition))
                  nil error-stream
                  (file-position error-stream)))
       (format stream "Reader error ~@[at ~D ~]on ~S:~%~?"
	       (file-position error-stream) error-stream
	       (reader-error-format-control condition)
	       (reader-error-format-arguments condition))))))

(define-condition reader-package-error (reader-error) ())

;;; %READ-ERROR  --  Interface
;;;
;;;    Like, signal a READ-ERROR, man...
;;;
(defun %reader-error (stream control &rest args)
  (error 'reader-error :stream stream  :format-control control
	 :format-arguments args))

(define-condition reader-eof-error (end-of-file)
  ((context :reader reader-eof-error-context :initarg :context))
  (:report
   (lambda (condition stream)
     (format stream "Unexpected EOF on ~S ~A."
	     (stream-error-stream condition)
	     (reader-eof-error-context condition)))))

(defun reader-eof-error (stream context)
  (error 'reader-eof-error :stream stream  :context context))
  

;;;; Readtable implementation.


(defvar std-lisp-readtable ()
  "Standard lisp readtable. This is for recovery from broken
   read-tables, and should not normally be user-visible.")

(deftype attribute-table ()
  '(simple-array (unsigned-byte 8) (#.char-code-limit)))

(defstruct (readtable
	    (:conc-name nil)
	    (:predicate readtablep)
	    (:copier nil)
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (print-unreadable-object (s stream :identity t)
		 (prin1 'readtable stream)))))
  "Readtable is a data structure that maps characters into syntax
   types for the Common Lisp expression reader."
  ;; The CHARACTER-ATTRIBUTE-TABLE is a vector of CHAR-CODE-LIMIT integers for
  ;; describing the character type.  Conceptually, there are 4 distinct
  ;; "primary" character attributes: WHITESPACE, TERMINATING-MACRO, ESCAPE, and
  ;; CONSTITUENT.  Non-terminating macros (such as the symbol reader) have the
  ;; attribute CONSTITUENT.
  ;;
  ;; In order to make the READ-TOKEN fast, all this information is
  ;; stored in the character attribute table by having different varieties of
  ;; constituents.
  (character-attribute-table
   (make-array char-code-limit :element-type '(unsigned-byte 8)
	       :initial-element constituent)
   :type attribute-table)
  ;;
  ;; The CHARACTER-MACRO-TABLE is a vector of CHAR-CODE-LIMIT functions.  One
  ;; of these functions called with appropriate arguments whenever any
  ;; non-WHITESPACE character is encountered inside READ-PRESERVING-WHITESPACE.
  ;; These functions are used to implement user-defined read-macros, system
  ;; read-macros, and the number-symbol reader.
  (character-macro-table 
   (make-array char-code-limit :initial-element #'undefined-macro-char)
   :type (simple-vector #.char-code-limit))
  ;;
  ;; DISPATCH-TABLES entry, which is an alist from dispatch characters to
  ;; vectors of CHAR-CODE-LIMIT functions, for use in defining dispatching
  ;; macros (like #-macro).
  (dispatch-tables () :type list)
  (readtable-case :upcase :type (member :upcase :downcase :preserve :invert)))


;;;; Constants for character attributes.  These are all as in the manual.

(eval-when (compile load eval)
  (defconstant whitespace 0)
  (defconstant terminating-macro 1)
  (defconstant escape 2)
  (defconstant constituent 3)
  (defconstant constituent-dot 4)
  (defconstant constituent-expt 5)
  (defconstant constituent-slash 6)
  (defconstant constituent-digit 7)
  (defconstant constituent-sign 8)
  ; 9
  (defconstant multiple-escape 10)
  (defconstant package-delimiter 11)
  ;;fake attribute for use in read-unqualified-token
  (defconstant delimiter 12)
  ;; More fake attributes for used for reading numbers.
  ;;
  ;; The following two are not static but depend on *READ-BASE*.
  ;; DECIMAL-DIGIT is for characters being digits in base 10 but not in
  ;; base *READ-BASE* (which is therefore perforce smaller than 10);
  ;; DIGIT-OR-EXPT is for characters being both exponent markers and
  ;; digits in base *READ-BASE* (which is therefore perforce larger
  ;; than 10).
  
  (defconstant constituent-decimal-digit 13)
  (defconstant constituent-digit-or-expt 14)
  ;; Invalid constituent character
  (defconstant constituent-invalid 15))


;;;; Package specials.

(defvar *old-package* ()
  "Value of *package* at the start of the last read or Nil.")

;;; In case we get an error trying to parse a symbol, we want to rebind the
;;; above stuff so it's cool.

(declaim (special *package* *keyword-package* *read-base*))



;;;; Macros and functions for character tables.

(defmacro get-cat-entry (char rt)
  ;;only give this side-effect-free args.
  `(elt (character-attribute-table ,rt)
	(char-code ,char)))

(defun set-cat-entry (char newvalue &optional (rt *readtable*))
  (setf (elt (character-attribute-table rt)
	     (char-code char))
	newvalue))

(defmacro get-cmt-entry (char rt)
  `(the function
	(elt (the simple-vector (character-macro-table ,rt))
	     (char-code ,char))))

(defun set-cmt-entry (char newvalue &optional (rt *readtable*))
  (setf (elt (the simple-vector (character-macro-table rt))
	     (char-code char))
	(coerce newvalue 'function)))

(defun undefined-macro-char (stream char)
  (unless *read-suppress*
    (%reader-error stream "Undefined read-macro character ~S" char)))

;;; The character attribute table is a CHAR-CODE-LIMIT vector of integers. 

(defmacro test-attribute (char whichclass rt)
  `(= (the fixnum (get-cat-entry ,char ,rt)) ,whichclass))

;;; Predicates for testing character attributes

;;; Make this a function, since other people want to use it.
;;;
(declaim (inline whitespacep))
(defun whitespacep (char &optional (rt *readtable*))
  (test-attribute char whitespace rt))

(defmacro constituentp (char &optional (rt '*readtable*))
  `(test-attribute ,char #.constituent ,rt))

(defmacro terminating-macrop (char &optional (rt '*readtable*))
  `(test-attribute ,char #.terminating-macro ,rt))

(defmacro escapep (char &optional (rt '*readtable*))
  `(test-attribute ,char #.escape ,rt))

(defmacro multiple-escape-p (char &optional (rt '*readtable*))
  `(test-attribute ,char #.multiple-escape ,rt))

(defmacro token-delimiterp (char &optional (rt '*readtable*))
  ;;depends on actual attribute numbering above.
  `(<= (get-cat-entry ,char ,rt) #.terminating-macro))



;;;; Secondary attribute table.

;;; There are a number of "secondary" attributes which are constant properties
;;; of characters characters (as long as they are constituents).

(defvar secondary-attribute-table)
(declaim (type attribute-table secondary-attribute-table))

(defun set-secondary-attribute (char attribute)
  (setf (elt secondary-attribute-table (char-code char))
	attribute))


(defun init-secondary-attribute-table ()
  (setq secondary-attribute-table
	(make-array char-code-limit :element-type '(unsigned-byte 8)
		    :initial-element #.constituent))
  (set-secondary-attribute #\: #.package-delimiter)
  ;;(set-secondary-attribute #\| #.multiple-escape)	; |) [For EMACS]
  (set-secondary-attribute #\. #.constituent-dot)
  (set-secondary-attribute #\+ #.constituent-sign)
  (set-secondary-attribute #\- #.constituent-sign)
  (set-secondary-attribute #\/ #.constituent-slash)  
  (do ((i (char-code #\0) (1+ i)))
      ((> i (char-code #\9)))
    (set-secondary-attribute (code-char i) #.constituent-digit))
  (set-secondary-attribute #\E #.constituent-expt)
  (set-secondary-attribute #\F #.constituent-expt)
  (set-secondary-attribute #\D #.constituent-expt)
  (set-secondary-attribute #\S #.constituent-expt)
  (set-secondary-attribute #\L #.constituent-expt)
  (set-secondary-attribute #\e #.constituent-expt)
  (set-secondary-attribute #\f #.constituent-expt)
  (set-secondary-attribute #\d #.constituent-expt)
  (set-secondary-attribute #\s #.constituent-expt)
  (set-secondary-attribute #\l #.constituent-expt)
  #+double-double
  (progn
    (set-secondary-attribute #\W #.constituent-expt)
    (set-secondary-attribute #\w #.constituent-expt))
  ;; See CLHS 2.1.4.2 for the list of constituent characters that are
  ;; invalid constituent characters.
  (set-secondary-attribute #\Space #.constituent-invalid)
  (set-secondary-attribute #\Newline #.constituent-invalid)
  (dolist (c '(#\backspace #\tab #\page #\return #\rubout))
    (set-secondary-attribute c #.constituent-invalid)))

(defmacro get-secondary-attribute (char)
  `(elt secondary-attribute-table
	(char-code ,char)))



;;;; Readtable operations.

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  "A copy is made of from-readtable and place into to-readtable."
  (let ((from-readtable (or from-readtable std-lisp-readtable))
	(to-readtable (or to-readtable (make-readtable))))
    ;;physically clobber contents of internal tables.
    (replace (character-attribute-table to-readtable)
	     (character-attribute-table from-readtable))
    (replace (character-macro-table to-readtable)
	     (character-macro-table from-readtable))
    (setf (dispatch-tables to-readtable)
	  (mapcar #'(lambda (pair) (cons (car pair)
					 (copy-seq (cdr pair))))
		  (dispatch-tables from-readtable)))
    (setf (readtable-case to-readtable)
	  (readtable-case from-readtable))
    to-readtable))

(defun set-syntax-from-char (to-char from-char &optional
				     (to-readtable *readtable*)
				     (from-readtable ()))
  "Causes the syntax of to-char to be the same as from-char in the 
  optional readtable (defaults to the current readtable).  The
  from-table defaults the standard lisp readtable by being nil."
  (let ((from-readtable (or from-readtable std-lisp-readtable)))
    ;;copy from-char entries to to-char entries, but make sure that if
    ;;from char is a constituent you don't copy non-movable secondary
    ;;attributes (constituent types), and that said attributes magically
    ;;appear if you transform a non-constituent to a constituent.
    (let ((att (get-cat-entry from-char from-readtable))
	  (mac (get-cmt-entry from-char from-readtable))
	  (from-dpair (find from-char (dispatch-tables from-readtable)
			    :test #'char= :key #'car))
	  (to-dpair (find to-char (dispatch-tables to-readtable)
			  :test #'char= :key #'car)))
      #+nil
      (if (constituentp from-char from-readtable)
	  (setq att (get-secondary-attribute to-char)))
      (set-cat-entry to-char att to-readtable)
      (set-cmt-entry to-char
		     mac
		     to-readtable)
      ;; Copy the reader macro functions too if from-char is a
      ;; dispatching macro character.
      ;;(format t "from-dpair = ~A~%" from-dpair)
      (when from-dpair
	(cond (to-dpair
	       ;; The to-readtable already has a dispatching table for
	       ;; this character.  Replace it with a copy of the
	       ;; dispatching table from from-readtable.
	       (setf (cdr to-dpair) (copy-seq (cdr from-dpair))))
	      (t
	       ;; The to-readtable doesn't have such an entry.  Add a
	       ;; copy of dispatch table from from-readtable to the
	       ;; dispatch table of the to-readtable.
	       (let ((pair (cons to-char (copy-seq (cdr from-dpair)))))
		 (setf (dispatch-tables to-readtable)
		       (push pair (dispatch-tables to-readtable)))))))))
  t)

(defun set-macro-character (char function &optional
				 (non-terminatingp nil) (rt *readtable*))
  "Causes char to be a macro character which invokes function when
   seen by the reader.  The non-terminatingp flag can be used to
   make the macro character non-terminating.  The optional readtable
   argument defaults to the current readtable.  Set-macro-character
   returns T."
  (if non-terminatingp
      (set-cat-entry char (get-secondary-attribute char) rt)
      (set-cat-entry char #.terminating-macro rt))
  (set-cmt-entry char function rt)
  T)

(defun get-macro-character (char &optional (rt *readtable*))
  "Returns the function associated with the specified char which is a macro
  character.  The optional readtable argument defaults to the current
  readtable."
  (let ((rt (or rt std-lisp-readtable)))
    ;; Check macro syntax, return associated function if it's there.
    ;; Returns a value for all constituents.
    (cond ((constituentp char)
	   (values (get-cmt-entry char rt) t))
	  ((terminating-macrop char)
	   (values (get-cmt-entry char rt) nil))
	  (t nil))))


;;;; These definitions support internal programming conventions.

(defconstant eof-object '(*eof*))

(defmacro eofp (char) `(eq ,char eof-object))

(defun flush-whitespace (stream)
  ;; This flushes whitespace chars, returning the last char it read (a
  ;; non-white one).  It always gets an error on end-of-file.
  (let ((stream (in-synonym-of stream)))
    (stream-dispatch stream
      ;; simple-stream
      (do ((attribute-table (character-attribute-table *readtable*))
	   (char (stream::%read-char stream t nil t t)
		 (stream::%read-char stream t nil t t)))
	  ((/= (the fixnum (aref attribute-table (char-code char)))
	       #.whitespace)
	   char))
      ;; lisp-stream
      (prepare-for-fast-read-char stream
	(do ((attribute-table (character-attribute-table *readtable*))
	     (char (fast-read-char t) (fast-read-char t)))
	    ((/= (the fixnum (aref attribute-table (char-code char)))
		 #.whitespace)
	     (done-with-fast-read-char)
	     char)))
      ;; fundamental-stream
      (do ((attribute-table (character-attribute-table *readtable*))
	   (char (stream-read-char stream) (stream-read-char stream)))
	  ((or (eq char :eof)
	       (/= (the fixnum (aref attribute-table (char-code char)))
		   #.whitespace))
	   (if (eq char :eof)
	       (error 'end-of-file :stream stream)
	       char))))))


;;;; Temporary initialization hack.

(defun init-std-lisp-readtable ()
  (setq std-lisp-readtable (make-readtable))
  ;;all characters default to "constituent" in make-readtable
  ;;*** un-constituent-ize some of these ***
  (let ((*readtable* std-lisp-readtable))
    (set-cat-entry #\tab #.whitespace)
    (set-cat-entry #\linefeed #.whitespace)  
    (set-cat-entry #\space #.whitespace)
    (set-cat-entry #\page #.whitespace)
    (set-cat-entry #\return #.whitespace)
    (set-cat-entry #\\ #.escape)
    (set-cat-entry #\| #.multiple-escape)
    (set-cmt-entry #\\ #'read-token)
    (set-cmt-entry #\: #'read-token)
    (set-cmt-entry #\| #'read-token)
    ;;macro definitions
    (set-macro-character #\" #'read-string)
    ;;* # macro
    (set-macro-character #\' #'read-quote)
    (set-macro-character #\( #'read-list)
    (set-macro-character #\) #'read-right-paren)
    (set-macro-character #\; #'read-comment)
    ;;* backquote
    ;;all constituents
    (do ((ichar 0 (1+ ichar))
	 (char))
	((= ichar #O200))
      (setq char (code-char ichar))
      (when (constituentp char std-lisp-readtable)
	    (set-cat-entry char (get-secondary-attribute char))
	    (set-cmt-entry char #'read-token)))))



;;;; read-buffer implementation.

(defvar *read-buffer*)
(defvar *read-buffer-length*)

(defvar *inch-ptr*)
(defvar *ouch-ptr*)

(declaim (type index *read-buffer-length* *inch-ptr* *ouch-ptr*))
(declaim (simple-string *read-buffer*))

(defconstant +read-buffer-pool-size+ 16)
(defconstant +read-buffer-initial-size+ 16)

(defun make-read-buffer-stack (size count)
  (let ((stack (make-array count :fill-pointer 0)))
    (dotimes (i count)
      (vector-push (make-string size) stack))
    stack))

(defvar *read-buffer-stack*)

(defun init-read-buffer-stack ()
  (setq *read-buffer-stack*
	(make-read-buffer-stack +read-buffer-initial-size+ +read-buffer-pool-size+)))

(defun allocate-read-buffer ()
  (cond ((zerop (fill-pointer *read-buffer-stack*))
	 (make-string 32))
	(t (vector-pop  *read-buffer-stack*))))

(defun free-read-buffer (buffer)
  (vector-push buffer *read-buffer-stack*))

;;; Recursive reader functions use with-read-buffer to allocate a
;;; fresh buffer.  We currently allocate a fresh buffer only for the
;;; exported functions READ, READ-PRESERVING-WHITESPACE,
;;; READ-FROM-STRING, and READ-DELIMITED-LIST.  Some internal
;;; functions like READ-TOKEN, INTERNAL-READ-EXTENDED-TOKEN and
;;; READ-STRING avoid the overhead for the allocation and clobber the
;;; current read-buffer.  

(defmacro with-read-buffer (() &body body)
  "Bind *read-buffer* to a fresh buffer and execute Body."
  `(let* ((*read-buffer* (allocate-read-buffer))
	  (*read-buffer-length* (length *read-buffer*))
	  (*ouch-ptr* 0)
	  (*inch-ptr* 0))
    (unwind-protect (progn ,@body)
      (free-read-buffer *read-buffer*))))

(defmacro reset-read-buffer ()
  ;;turn *read-buffer* into an empty read-buffer.
  ;;*ouch-ptr* always points to next char to write
  `(progn
    (setq *ouch-ptr* 0)
    ;;*inch-ptr* always points to next char to read
    (setq *inch-ptr* 0)))

(defmacro ouch-read-buffer (char)
  `(progn
    (if (>= (the fixnum *ouch-ptr*)
	    (the fixnum *read-buffer-length*))
	;;buffer overflow -- double the size
	(grow-read-buffer))
    (setf (elt (the simple-string *read-buffer*) *ouch-ptr*) ,char)
    (setq *ouch-ptr* (1+ *ouch-ptr*))))

;; macro to move *ouch-ptr* back one.
(defmacro ouch-unread-buffer ()
  '(if (> (the fixnum *ouch-ptr*) (the fixnum *inch-ptr*))
       (setq *ouch-ptr* (1- (the fixnum *ouch-ptr*)))))

(defun grow-read-buffer ()
  (let ((rbl (length (the simple-string *read-buffer*))))
    (declare (fixnum rbl))
    (setq *read-buffer*
	  (concatenate 'simple-string
		       (the simple-string *read-buffer*)
		       (the simple-string (make-string rbl))))
    (setq *read-buffer-length* (* 2 rbl))))

(defun inchpeek-read-buffer ()
  (if (>= (the fixnum *inch-ptr*) (the fixnum *ouch-ptr*))
      eof-object
      (elt (the simple-string *read-buffer*) *inch-ptr*)))

(defun inch-read-buffer ()
  (cond ((>= (the fixnum *inch-ptr*) (the fixnum *ouch-ptr*))
	 eof-object)
	(t (prog1 (elt (the simple-string *read-buffer*) *inch-ptr*)
		  (setq *inch-ptr* (1+ (the fixnum *inch-ptr*)))))))

(defmacro unread-buffer ()
  `(decf (the fixnum *inch-ptr*)))

(defun read-unwind-read-buffer ()
  ;;keep contents, but make next (inch..) return first char.
  (setq *inch-ptr* 0))

(defun read-buffer-to-string ()
  (subseq (the simple-string *read-buffer*) 0 *ouch-ptr*))



;;;; READ-PRESERVING-WHITESPACE, READ-DELIMITED-LIST, and READ.

(defvar *ignore-extra-close-parentheses* t
  "If true, only warn when there is an extra close paren, otherwise error.")

;; Alist for #=. Used to keep track of objects with labels assigned that have
;; been completly read.  Entry is (integer-tag gensym-tag value).
;;
(defvar *sharp-equal-alist* ())

(declaim (special *standard-input*))
 
;;; READ-PRESERVING-WHITESPACE behaves just like read only it makes sure
;;; to leave terminating whitespace in the stream.
;;;
(defun read-preserving-whitespace (&optional (stream *standard-input*)
				   (eof-errorp t) (eof-value nil)
				   (recursivep nil))
  "Reads from stream and returns the object read, preserving the whitespace
   that followed the object."
  (with-read-buffer ()
    (read-preserving-whitespace-internal stream eof-errorp eof-value recursivep)))

(defun read-preserving-whitespace-internal (&optional (stream *standard-input*)
					    (eof-errorp t) (eof-value nil)
					    (recursivep nil))
  
  (cond
    (recursivep
     ;; Loop for repeating when a macro returns nothing.
     (loop
      (let ((char (read-char stream eof-errorp eof-object)))
	(cond ((eofp char) (return eof-value))
	      ((whitespacep char))
	      (t
	       (let* ((macrofun (get-cmt-entry char *readtable*))
		      (result (multiple-value-list
			       (funcall macrofun stream char))))
		 ;; Repeat if macro returned nothing.
		 (if result (return (if *read-suppress*
					nil
					(car result))))))))))
    (t
     (let ((*sharp-equal-alist* nil))
       (read-preserving-whitespace-internal stream eof-errorp eof-value t)))))


(defun read-maybe-nothing (stream char)
  ;;returns nil or a list with one thing, depending.
  ;;for functions that want comments to return so they can look
  ;;past them.  Assumes char is not whitespace.
  (let ((retval (multiple-value-list
		 (funcall (get-cmt-entry char *readtable*) stream char))))
    (if retval (rplacd retval nil))))

(defun read (&optional (stream *standard-input*) (eof-errorp t)
		       (eof-value ()) (recursivep ()))
  "Reads in the next object in the stream, which defaults to
   *standard-input*. For details see the I/O chapter of
   the manual."
  (with-read-buffer ()
    (read-internal stream eof-errorp eof-value recursivep)))

(defun read-internal (&optional (stream *standard-input*) (eof-errorp t)
		      (eof-value ()) (recursivep ()))
  (prog1
      (read-preserving-whitespace-internal stream eof-errorp eof-value recursivep)
    (let ((whitechar (read-char stream nil eof-object)))
      (if (and (not (eofp whitechar))
	       (or (not (whitespacep whitechar))
		   recursivep))
	  (unread-char whitechar stream)))))

(defun read-delimited-list (endchar &optional
				    (input-stream *standard-input*)
				    recursive-p)
  "Reads objects from input-stream until the next character after an
   object's representation is endchar.  A list of those objects read
   is returned."
  (declare (ignore recursive-p))
  (with-read-buffer ()
    (do ((char (flush-whitespace input-stream)
	       (flush-whitespace input-stream))
	 (retlist ()))
	((char= char endchar) (nreverse retlist))
      (setq retlist (nconc (read-maybe-nothing input-stream char) retlist)))))


;;;; Standard ReadMacro definitions to implement the reader.

(defun read-quote (stream ignore)
  (declare (ignore ignore))
  (list 'quote (read-internal stream t nil t)))

(defun read-comment (stream ignore)
  (declare (ignore ignore))
  (let ((stream (in-synonym-of stream)))
    (stream-dispatch stream
      ;; simple-stream
      (do ((char (stream::%read-char stream nil nil t t)
		 (stream::%read-char stream nil nil t t)))
	  ((or (not char) (char= char #\newline))))
      ;; lisp-stream
      (prepare-for-fast-read-char stream
        (do ((char (fast-read-char nil nil)
		   (fast-read-char nil nil)))
	    ((or (not char) (char= char #\newline))
	     (done-with-fast-read-char))))
      ;; fundamental-stream
      (do ((char (stream-read-char stream) (stream-read-char stream)))
	  ((or (eq char :eof) (char= char #\newline))))))
  ;; don't return anything
  (values))

(defun read-list (stream ignore)
  (declare (ignore ignore))
  (let* ((thelist (list nil))
	 (listtail thelist))
    (do ((firstchar (flush-whitespace stream) (flush-whitespace stream)))
	((char= firstchar #\) ) (cdr thelist))
      (when (char= firstchar #\.)
	    (let ((nextchar (read-char stream t)))
	      (cond ((token-delimiterp nextchar)
		     (cond ((eq listtail thelist)
			    (if *read-suppress*
				(return-from read-list nil)
				(%reader-error stream "Nothing appears before . in list.")))
			   ((whitespacep nextchar)
			    (setq nextchar (flush-whitespace stream))))
		     (rplacd listtail
			     ;;return list containing last thing.
			     (car (read-after-dot stream nextchar)))
		     (return (cdr thelist)))
		    ;;put back nextchar so we can read it normally.
		    (t (unread-char nextchar stream)))))
      ;;next thing is not an isolated dot.
      (let ((listobj (read-maybe-nothing stream firstchar)))
	;;allows the possibility that a comment was read.
	(when listobj
	      (rplacd listtail listobj)
	      (setq listtail listobj))))))

(defun read-after-dot (stream firstchar)
  ;;firstchar is non-whitespace!
  (let ((lastobj ()))
    (do ((char firstchar (flush-whitespace stream)))
	((char= char #\) )
	 (%reader-error stream "Nothing appears after . in list."))
      ;;see if there's something there.
      (setq lastobj (read-maybe-nothing stream char))
      (when lastobj (return t)))
    ;;at least one thing appears after the dot.
    ;;check for more than one thing following dot.
    (do ((lastchar (flush-whitespace stream)
		   (flush-whitespace stream)))
	((char= lastchar #\) ) lastobj)	;success!
      ;;try reading virtual whitespace
      (if (read-maybe-nothing stream lastchar)
	  (%reader-error stream "More than one object follows . in list.")))))

(defun read-string (stream closech)
  ;;this accumulates chars until it sees same char that invoked it.
  ;;for a very long string, this could end up bloating the read buffer.
  (reset-read-buffer)
  (let ((stream (in-synonym-of stream)))
    (stream-dispatch stream
      ;; simple-stream
      (do ((char (stream::%read-char stream t nil t t)
		 (stream::%read-char stream t nil t t)))
	  ((char= char closech))
	(if (escapep char) (setq char (stream::%read-char stream t nil t t)))
	(ouch-read-buffer char))
      ;; lisp-stream
      (prepare-for-fast-read-char stream
        (do ((char (fast-read-char t) (fast-read-char t)))
	    ((char= char closech)
	     (done-with-fast-read-char))
	  (if (escapep char) (setq char (fast-read-char t)))
	  (ouch-read-buffer char)))
      ;; fundamental-stream
      (do ((char (stream-read-char stream) (stream-read-char stream)))
	  ((or (eq char :eof) (char= char closech))
	   (if (eq char :eof)
	       (error 'end-of-file :stream stream)))
	(when (escapep char)
	  (setq char (stream-read-char stream))
	  (if (eq char :eof)
	      (error 'end-of-file :stream stream)))
	(ouch-read-buffer char))))
  (read-buffer-to-string))

(defun read-right-paren (stream ignore)
  (declare (ignore ignore))
    (cond (*ignore-extra-close-parentheses*
	   (warn "Ignoring unmatched close parenthesis~
		  ~@[ at file position ~D~]."
		 (file-position stream))
	   (values))
	  (t
	   (%reader-error stream "Unmatched close parenthesis."))))

;;; INTERNAL-READ-EXTENDED-TOKEN  --  Internal
;;;
;;; Read from the stream up to the next delimiter.  If escape-firstchar is
;;; true then the firstchar is assumed to be escaped.  Leaves resulting token
;;; in *read-buffer*, returns two values:
;;; -- a list of the escaped character positions, and
;;; -- The position of the first package delimiter (or NIL).
;;;
(defun internal-read-extended-token (stream firstchar escape-firstchar)
  (reset-read-buffer)
  (let ((escapes ()))
    (when escape-firstchar
      (push *ouch-ptr* escapes)
      (ouch-read-buffer firstchar)
      (setq firstchar (read-char stream nil eof-object)))
    (do ((char firstchar (read-char stream nil eof-object))
	 (colon nil))
	((cond ((eofp char) t)
	       ((token-delimiterp char)
		(unread-char char stream)
		t)
	       (t nil))
	 (values escapes colon))
      (cond ((escapep char)
	     ;; It can't be a number, even if it's 1\23.
	     ;; Read next char here, so it won't be casified.
	     (push *ouch-ptr* escapes)
	     (let ((nextchar (read-char stream nil eof-object)))
	       (if (eofp nextchar)
		   (reader-eof-error stream "after escape character")
		   (ouch-read-buffer nextchar))))
	    ((multiple-escape-p char)
	     ;; Read to next multiple-escape, escaping single chars along the
	     ;; way.
	     (loop
	      (let ((ch (read-char stream nil eof-object)))
		(cond
		  ((eofp ch)
		   (reader-eof-error stream "inside extended token"))
		  ((multiple-escape-p ch) (return))
		  ((escapep ch)
		   (let ((nextchar (read-char stream nil eof-object)))
		     (cond ((eofp nextchar)
			    (reader-eof-error stream "after escape character"))
			   (t
			    (push *ouch-ptr* escapes)
			    (ouch-read-buffer nextchar)))))
		  (t
		   (push *ouch-ptr* escapes)
		   (ouch-read-buffer ch))))))
	    (t
	     (when (and (constituentp char)
			(eql (get-secondary-attribute char)
			     #.package-delimiter)
			(not colon))
	       (setq colon *ouch-ptr*))
	     (ouch-read-buffer char))))))


;;;; Character classes.

;;; return the character class for a char
;;;
(defmacro char-class (char attable)
  `(let ((att (aref ,attable (char-code ,char))))
     (declare (fixnum att))
     (cond ((<= att #.terminating-macro)
	    #.delimiter)
	   ((or (< att constituent)
		(= att multiple-escape))
	    att)
	   (t
	    (setf att (get-secondary-attribute ,char))
	    (cond ((= att #.constituent-invalid)
		   (%reader-error stream "invalid constituent"))
		  (t
		   att))))))

;;; return the character class for a char which might be part of a rational
;;; number
;;;
(defmacro char-class2 (char attable)
  `(let ((att (aref ,attable (char-code ,char))))
     (declare (fixnum att))
     (cond ((<= att #.terminating-macro)
	    #.delimiter)
	   ((or (< att constituent)
		(= att multiple-escape))
	    att)
	   (t
	    (setf att (get-secondary-attribute ,char))
	    (cond ((digit-char-p ,char *read-base*)
		   constituent-digit)
		  ((= att constituent-digit)
		   constituent)
		  ((= att constituent-invalid)
		   (%reader-error stream "invalid constituent"))
		  (t
		   att))))))

;;; return the character class for a char which might be part of a rational or
;;; floating number (assume that it is a digit if it could be)
;;;
(defmacro char-class3 (char attable)
  `(let ((att (aref ,attable (char-code ,char))))
     (declare (fixnum att))
     (cond ((<= att #.terminating-macro)
	    #.delimiter)
	   ((or (< att constituent)
		(= att multiple-escape))
	    att)
	   (t
	    (setf att (get-secondary-attribute ,char))
	    (when possibly-rational
	      (setq possibly-rational
		    (or (digit-char-p ,char *read-base*)
			(= att constituent-slash))))
	    (when possibly-float
	      (setq possibly-float
		    (or (digit-char-p ,char 10)
			(= att constituent-dot))))
	    (cond ((digit-char-p ,char (max *read-base* 10))
		   (if (digit-char-p ,char *read-base*)
		       (if (= att #.constituent-expt)
			   constituent-digit-or-expt
			   constituent-digit)
		       constituent-decimal-digit))
		  ((= att constituent-invalid)
		   (%reader-error stream "invalid constituent"))
		  (t
		   att))))))



;;;; Token fetching.

(defvar *read-suppress* nil 
  "Suppresses most interpreting of the reader when T")

(defvar *read-base* 10
  "The radix that Lisp reads numbers in.")
(declaim (type (integer 2 36) *read-base*))

;;; CASIFY-READ-BUFFER  --  Internal
;;;
;;;    Modify the *read-buffer* according to READTABLE-CASE, ignoring escapes.
;;; ESCAPES is a list of the escaped indices, in reverse order. 
;;;
(defun casify-read-buffer (escapes)
  (let ((case (readtable-case *readtable*)))
    (cond
     ((and (null escapes) (eq case :upcase))
      (dotimes (i *ouch-ptr*)
	(setf (schar *read-buffer* i) (char-upcase (schar *read-buffer* i)))))
     ((eq case :preserve))
     (t
      (macrolet ((skip-esc (&body body)
		   `(do ((i (1- *ouch-ptr*) (1- i))
			 (escapes escapes))
			((minusp i))
		      (declare (fixnum i))
		      (when (or (null escapes)
				(let ((esc (first escapes)))
				  (declare (fixnum esc))
				  (cond ((< esc i) t)
					(t
					 (assert (= esc i))
					 (pop escapes)
					 nil))))
			(let ((ch (schar *read-buffer* i)))
			  ,@body)))))
	(flet ((lower-em ()
		 (skip-esc (setf (schar *read-buffer* i) (char-downcase ch))))
	       (raise-em ()
		 (skip-esc (setf (schar *read-buffer* i) (char-upcase ch)))))
	  (ecase case
	    (:upcase (raise-em))
	    (:downcase (lower-em))
	    (:invert
	     (let ((all-upper t)
		   (all-lower t))
	       (skip-esc
		 (when (both-case-p ch)
		   (if (upper-case-p ch)
		       (setq all-lower nil)
		       (setq all-upper nil))))
	       (cond (all-lower (raise-em))
		     (all-upper (lower-em))))))))))))
  
(defun read-token (stream firstchar)
  "This function is just an fsm that recognizes numbers and symbols."
  ;; Check explicitly whether FIRSTCHAR has an entry for
  ;; NON-TERMINATING in CHARACTER-ATTRIBUTE-TABLE and
  ;; READ-DOT-NUMBER-SYMBOL in CMT. Report an error if these are
  ;; violated. (If we called this, we want something that is a
  ;; legitimate token!) Read in the longest possible string satisfying
  ;; the Backus-Naur form for "unqualified-token". Leave the result in
  ;; the *READ-BUFFER*. Return next char after token (last char read).
  (when *read-suppress*
    (internal-read-extended-token stream firstchar nil)
    (return-from read-token nil))
  (let ((attribute-table (character-attribute-table *readtable*))
	(package-designator nil)
	(colons 0)
	(possibly-rational t)
	(seen-digit-or-expt nil)
	(possibly-float t)
	(was-possibly-float nil)
	(escapes ())
	(seen-multiple-escapes nil))
    (reset-read-buffer)
    (prog ((char firstchar))
      (case (char-class3 char attribute-table)
	(#.constituent-sign (go SIGN))
	(#.constituent-digit (go LEFTDIGIT))
	(#.constituent-digit-or-expt
	 (setq seen-digit-or-expt t)
	 (go LEFTDIGIT))
	(#.constituent-decimal-digit (go LEFTDECIMALDIGIT))
	(#.constituent-dot (go FRONTDOT))
	(#.escape (go ESCAPE))
	(#.package-delimiter (go COLON))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.constituent-invalid (%reader-error stream "invalid constituent"))
	;; can't have eof, whitespace, or terminating macro as first char!
	(t (go SYMBOL)))
     SIGN ; saw "sign"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (setq possibly-rational t
	    possibly-float t)
      (case (char-class3 char attribute-table)
	(#.constituent-digit (go LEFTDIGIT))
	(#.constituent-digit-or-expt
	 (setq seen-digit-or-expt t)
	 (go LEFTDIGIT))
	(#.constituent-decimal-digit (go LEFTDECIMALDIGIT))
	(#.constituent-dot (go SIGNDOT))
	(#.escape (go ESCAPE))
	(#.package-delimiter (go COLON))
	(#.multiple-escape (go MULT-ESCAPE))	
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(t (go SYMBOL)))
     LEFTDIGIT ; saw "[sign] {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-integer)))
      (setq was-possibly-float possibly-float)
      (case (char-class3 char attribute-table)
	(#.constituent-digit (go LEFTDIGIT))
	(#.constituent-decimal-digit (if possibly-float
						     (go LEFTDECIMALDIGIT)
						     (go SYMBOL)))
	(#.constituent-dot (if possibly-float
					   (go MIDDLEDOT)
					   (go SYMBOL)))
	(#.constituent-digit-or-expt
	 (if (or seen-digit-or-expt (not was-possibly-float))
	     (progn (setq seen-digit-or-expt t) (go LEFTDIGIT))
	     (progn (setq seen-digit-or-expt t) (go LEFTDIGIT-OR-EXPT))))
	(#.constituent-expt
	 (if was-possibly-float
	     (go EXPONENT)
	     (go SYMBOL)))
	(#.constituent-slash (if possibly-rational
					     (go RATIO)
					     (go SYMBOL)))
	(#.delimiter (unread-char char stream)
				 (return (make-integer)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     LEFTDIGIT-OR-EXPT
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-integer)))
      (case (char-class3 char attribute-table)
	(#.constituent-digit (go LEFTDIGIT))
	(#.constituent-decimal-digit (error "impossible!"))
	(#.constituent-dot (go SYMBOL))
	(#.constituent-digit-or-expt (go LEFTDIGIT))
	(#.constituent-expt (go SYMBOL))
	(#.constituent-sign (go EXPTSIGN))
	(#.constituent-slash (if possibly-rational
					     (go RATIO)
					     (go SYMBOL)))
	(#.delimiter (unread-char char stream)
				 (return (make-integer)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     LEFTDECIMALDIGIT ; saw "[sign] {decimal-digit}+"
      (assert possibly-float)
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.constituent-digit (go LEFTDECIMALDIGIT))
	(#.constituent-dot (go MIDDLEDOT))
	(#.constituent-expt (go EXPONENT))
	(#.constituent-slash (assert (not possibly-rational))
					 (go SYMBOL))
	(#.delimiter (unread-char char stream)
				 (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     MIDDLEDOT ; saw "[sign] {digit}+ dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (let ((*read-base* 10))
			     (make-integer))))
      (case (char-class char attribute-table)
	(#.constituent-digit (go RIGHTDIGIT))
	(#.constituent-expt (go EXPONENT))
	(#.delimiter
	 (unread-char char stream)
	 (return (let ((*read-base* 10))
		   (make-integer))))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     RIGHTDIGIT ; saw "[sign] {decimal-digit}* dot {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float stream)))
      (case (char-class char attribute-table)
	(#.constituent-digit (go RIGHTDIGIT))
	(#.constituent-expt (go EXPONENT))
	(#.delimiter
	 (unread-char char stream)
	 (return (make-float stream)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     SIGNDOT ; saw "[sign] dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.constituent-digit (go RIGHTDIGIT))
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(t (go SYMBOL)))
     FRONTDOT ; saw "dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (%reader-error stream "dot context error"))
      (case (char-class char attribute-table)
	(#.constituent-digit (go RIGHTDIGIT))
	(#.constituent-dot (go DOTS))
	(#.delimiter  (%reader-error stream "dot context error"))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     EXPONENT
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (setq possibly-float t)
      (case (char-class char attribute-table)
	(#.constituent-sign (go EXPTSIGN))
	(#.constituent-digit (go EXPTDIGIT))
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     EXPTSIGN ; got to EXPONENT, and saw a sign character
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.constituent-digit (go EXPTDIGIT))
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     EXPTDIGIT ; got to EXPONENT, saw "[sign] {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float stream)))
      (case (char-class char attribute-table)
	(#.constituent-digit (go EXPTDIGIT))
	(#.delimiter
	 (unread-char char stream)
	 (return (make-float stream)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     RATIO ; saw "[sign] {digit}+ slash"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class2 char attribute-table)
	(#.constituent-digit (go RATIODIGIT))
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     RATIODIGIT ; saw "[sign] {digit}+ slash {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-ratio stream)))
      (case (char-class2 char attribute-table)
	(#.constituent-digit (go RATIODIGIT))
	(#.delimiter
	 (unread-char char stream)
	 (return (make-ratio stream)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     DOTS ; saw "dot {dot}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (%reader-error stream "too many dots"))
      (case (char-class char attribute-table)
	(#.constituent-dot (go DOTS))
	(#.delimiter
	 (unread-char char stream)
	 (%reader-error stream "too many dots"))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     SYMBOL ; not a dot, dots, or number
      (let ((stream (in-synonym-of stream)))
	(stream-dispatch stream
	    ;; simple-stream
	    (prog ()
	       SYMBOL-LOOP
	       (ouch-read-buffer char)
	       (setq char (stream::%read-char stream nil nil t t))
	       (unless char (go RETURN-SYMBOL))
	       (case (char-class char attribute-table)
		 (#.escape (go ESCAPE))
		 (#.delimiter (stream::%unread-char stream char)
			      (go RETURN-SYMBOL))
		 (#.multiple-escape (go MULT-ESCAPE))
		 (#.package-delimiter (go COLON))
		 (t (go SYMBOL-LOOP))))
	    ;; lisp stream
	    (prepare-for-fast-read-char stream
	      (prog ()
	       SYMBOL-LOOP
	       (ouch-read-buffer char)
	       (setq char (fast-read-char nil nil))
	       (unless char (go RETURN-SYMBOL))
	       (case (char-class char attribute-table)
		 (#.escape (done-with-fast-read-char)
				       (go ESCAPE))
		 (#.delimiter (done-with-fast-read-char)
					  (unread-char char stream)
					  (go RETURN-SYMBOL))
		 (#.multiple-escape (done-with-fast-read-char)
						(go MULT-ESCAPE))
		 (#.package-delimiter (done-with-fast-read-char)
						  (go COLON))
		 (t (go SYMBOL-LOOP)))))
	    ;; fundamental-stream
	    (prog ()
	     SYMBOL-LOOP
	     (ouch-read-buffer char)
	     (setq char (read-char stream nil :eof))
	     (when (eq char :eof) (go RETURN-SYMBOL))
	     (case (char-class char attribute-table)
	       (#.escape (go ESCAPE))
	       (#.delimiter (unread-char char stream)
			    (go RETURN-SYMBOL))
	       (#.multiple-escape (go MULT-ESCAPE))
	       (#.package-delimiter (go COLON))
	       (t (go SYMBOL-LOOP))))))
     ESCAPE ; saw an escape
      ;; Don't put the escape in the read buffer.
      ;; READ-NEXT CHAR, put in buffer (no case conversion).
      (let ((nextchar (read-char stream nil nil)))
	(unless nextchar
	  (reader-eof-error stream "after escape character"))
	(push *ouch-ptr* escapes)
	(ouch-read-buffer nextchar))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
      MULT-ESCAPE
      (setq seen-multiple-escapes t)
      (do ((char (read-char stream t) (read-char stream t)))
	  ((multiple-escape-p char))
	(if (escapep char) (setq char (read-char stream t)))
	(push *ouch-ptr* escapes)
	(ouch-read-buffer char))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
      COLON
      (casify-read-buffer escapes)
      (unless (zerop colons)
	(%reader-error stream "too many colons in ~S"
		      (read-buffer-to-string)))
      (setq colons 1)
      (setq package-designator
	    (if (plusp *ouch-ptr*)
		;; FIXME: It seems inefficient to cons up a package
		;; designator string every time we read a symbol with an
		;; explicit package prefix. Perhaps we could implement
		;; a FIND-PACKAGE* function analogous to INTERN*
		;; and friends?
		(read-buffer-to-string)
		(if seen-multiple-escapes
		    (read-buffer-to-string)
		    *keyword-package*)))
      (reset-read-buffer)
      (setq escapes ())
      (setq char (read-char stream nil nil))
      (unless char (reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-table)
	(#.delimiter
	 (unread-char char stream)
	 (%reader-error stream
			"illegal terminating character after a colon: ~S"
			char))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go INTERN))
	(t (go SYMBOL)))
      INTERN
      (setq colons 2)
      (setq char (read-char stream nil nil))
      (unless char
	(reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-table)
	(#.delimiter
	 (unread-char char stream)
	 (%reader-error stream
			"illegal terminating character after a colon: ~S"
			char))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter
	 (%reader-error stream
			"too many colons after ~S name"
			package-designator))
	(t (go SYMBOL)))
      RETURN-SYMBOL
      (casify-read-buffer escapes)
      (let ((found (if package-designator
		       (find-package package-designator)
		       *package*)))
	(unless found
	  (error 'reader-package-error :stream stream
		 :format-arguments (list package-designator)
		 :format-control "package ~S not found"))

	(if (or (zerop colons) (= colons 2) (eq found *keyword-package*))
	    (return (intern* *read-buffer* *ouch-ptr* found))
	    (multiple-value-bind (symbol test)
		(find-symbol* *read-buffer* *ouch-ptr* found)
	      (when (eq test :external) (return symbol))
	      (let ((name (read-buffer-to-string)))
		(with-simple-restart (continue "Use symbol anyway.")
		  (error 'reader-package-error :stream stream
			 :format-arguments (list name (package-name found))
			 :format-control
			 (if test
			     "The symbol ~S is not external in the ~A package."
			     "Symbol ~S not found in the ~A package.")))
		(return (intern name found)))))))))


(defun read-extended-token (stream &optional (*readtable* *readtable*))
  "For semi-external use: returns 3 values: the string for the token,
   a flag for whether there was an escape char, and the position of any
   package delimiter."
  (let ((firstch (read-char stream nil nil t)))
    (cond (firstch
	   (multiple-value-bind (escapes colon)
	       (internal-read-extended-token stream firstch nil)
	     (casify-read-buffer escapes)
	     (values (read-buffer-to-string) (not (null escapes)) colon)))
	  (t
	   (values "" nil nil)))))

(defun read-extended-token-escaped (stream &optional (*readtable* *readtable*))
  "For semi-external use: read an extended token with the first character
  escaped.  Returns the string for the token."
  (let ((firstch (read-char stream nil nil)))
    (cond (firstch
	   (let ((escapes (internal-read-extended-token stream firstch t)))
	     (casify-read-buffer escapes)
	     (read-buffer-to-string)))
	  (t
	   (reader-eof-error stream "after escape")))))



;;;; Number reading functions.

(defmacro exponent-letterp (letter)
  `(memq ,letter '(#\E #\S #\F #\L #\D #\e #\s #\f #\l #\d
		   #+double-double #\w
		   #+double-double #\W)))


(defvar *integer-reader-safe-digits*
  '#(NIL NIL
     26 17 13 11 10 9 8 8 8 7 7 7 7 6 6 6 6 6 6 6 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5)
  "Holds the mapping of base to 'safe' number of digits to read for a fixnum.")

(defvar *integer-reader-base-power* 
  '#(NIL NIL
     67108864 129140163 67108864 48828125 60466176 40353607
     16777216 43046721 100000000 19487171 35831808 62748517 105413504 11390625
     16777216 24137569 34012224 47045881 64000000 85766121 113379904 6436343
     7962624 9765625 11881376 14348907 17210368 20511149 24300000 28629151
     33554432 39135393 45435424 52521875 60466176)
  "Holds the largest fixnum power of the base for make-integer.")

(declaim (simple-vector *integer-reader-safe-digits*
			*integer-reader-base-power*))
#||
;; This function was used to initialize the variables above.
(defun init-integer-reader ()
  (do ((base 2 (1+ base)))
      ((> base 36))
    (let ((digits
	  (do ((fix (truncate most-positive-fixnum base)
		    (truncate fix base))
	       (digits 0 (1+ digits)))
	      ((zerop fix) digits))))	   
      (setf (aref *integer-reader-safe-digits* base)
	    digits
	    (aref *integer-reader-base-power* base)
	    (expt base digits)))))
|#

(defun make-integer ()
  "Minimizes bignum-fixnum multiplies by reading a 'safe' number of digits, 
  then multiplying by a power of the base and adding."
  (read-unwind-read-buffer)
  ;; Use the fast reader if the number has enough digits.  It seems
  ;; that the fast reader is slower for small numbers but much faster
  ;; for large numbers.  The value of 500 is an approximate break-even
  ;; point.
  (if (>= (the fixnum *ouch-ptr*) 500)
      (fast-read-integer *read-base*)
      (let* ((base *read-base*)
	     (digits-per (aref *integer-reader-safe-digits* base))
	     (base-power (aref *integer-reader-base-power* base)) 
	     (negativep nil)
	     (number 0))
	(declare (type index digits-per base-power))
	(let ((char (inch-read-buffer)))
	  (cond ((char= char #\-)
		 (setq negativep t))
		((char= char #\+))
		(t (unread-buffer))))
	(loop
	   (let ((num 0))
	     (declare (type index num))
	     (dotimes (digit digits-per)
	       (let* ((ch (inch-read-buffer)))
		 (cond ((or (eofp ch) (char= ch #\.))
			(return-from make-integer
			  (let ((res
				 (if (zerop number) num
				     (+ num (* number
					       (expt base digit))))))
			    (if negativep (- res) res))))
		       (t (setq num (+ (digit-char-p ch base)
				       (the index (* num base))))))))
	     (setq number (+ num (* number base-power))))))))

(defun fast-read-integer (r)
  "Fast bignum-reading interface.  Reads from stream S an integer in radix
R.  If we find some kind of error (bad characters, EOF), then NIL is
returned; otherwise the number.  Reads at least one digit, but may not get to
the end of the stream."
  (let ((v (make-array 32 :element-type 'integer
		       :adjustable t :fill-pointer 0)))
    ;; V maps integers i to r^(2^i).
    (vector-push-extend r v)
    (labels ((scan (ch l)
	       ;; Character CH has been read.  L contains a list of entries
	       ;; of the form (n . i), where i is the base-2 log of the
	       ;; length of n in digits.  We read digits and push entries
	       ;; onto the front of the list as (d . 1), and then fix the
	       ;; list to maintain the invariant that entries on the list
	       ;; have strictly increasing length.
	       (declare (type (or character list) ch) (list l))
	       (if (or (eofp ch) (char= ch #\.))
		   (finish l)
		   (let ((d (digit-char-p ch r)))
		     (declare (type (or null fixnum) d))
		     (labels ((fix (x i l)
				(declare (integer x) (fixnum i) (list l))
				(if (>= i (fill-pointer v))
				    (vector-push-extend
				     (let ((x (aref v (1- i))))
				       (* x x))
				     v))
				(if (null l)
				    (scan (inch-read-buffer) (cons (cons x i) nil))
				    (let ((y (caar l)) (j (cdar l)) (rest (cdr l)))
				      (declare (integer y) (fixnum j) (list l))
				      (if (= i j)
					  (fix (+ (* y (aref v i)) x) (1+ i) rest)
					  (scan (inch-read-buffer) (cons (cons x i) l)))))))
		       (fix d 0 l)))))
	     (finish (l)
	       ;; Convert the list into a final answer.  The main loop below
	       ;; counts Z as the length of what we've built so far in
	       ;; digits.  Remember that, since this is a stack, we're
	       ;; effectively working right-to-left here.
	       (declare (list l))
	       (labels ((create-integer (a z l)
			  (declare (integer a z) (list l))
			  (if (null l)
			      a
			      (create-integer (+ (* z (the integer (caar l))) a)
					      (* z (aref v (cdar l)))
					      (cdr l)))))
		 (create-integer 0 1 l))))
      (let ((ch (inch-read-buffer)))
	(cond ((char= ch #\-)
	       (let ((n (scan (inch-read-buffer) nil)))
		 (- n)))
	      ((char= ch #\+)
	       (scan (inch-read-buffer) nil))
	      (t
	       (scan ch nil)))))))


(defun make-float (stream)
  ;; Assume that the contents of *read-buffer* are a legal float, with nothing
  ;; else after it.
  (read-unwind-read-buffer)
  (let ((negative-fraction nil)
        (number 0)
        (divisor 1)
        (negative-exponent nil)
        (exponent 0)
        (float-char ())
        (char (inch-read-buffer)))
    (if (cond ((char= char #\+) t)
              ((char= char #\-) (setq negative-fraction t)))
        ;; Flush it.
        (setq char (inch-read-buffer)))
    ;; Read digits before the dot.
    (do* ((ch char (inch-read-buffer))
          (dig (digit-char-p ch) (digit-char-p ch)))
         ((not dig) (setq char ch))
      (setq number (+ (* number 10) dig)))
    ;; Deal with the dot, if it's there.
    (when (char= char #\.)
      (setq char (inch-read-buffer))
      ;; Read digits after the dot.
      (do* ((ch char (inch-read-buffer))
            (dig (and (not (eofp ch)) (digit-char-p ch))
                 (and (not (eofp ch)) (digit-char-p ch))))
           ((not dig) (setq char ch))
        (setq divisor (* divisor 10))
        (setq number (+ (* number 10) dig))))
    ;; Is there an exponent letter?
    (cond ((eofp char)
           ;; If not, we've read the whole number.
           (let ((num (make-float-aux number divisor
                                      *read-default-float-format*
				      stream)))
             (return-from make-float (if negative-fraction (- num) num))))
          ((exponent-letterp char)
           (setq float-char char)
           ;; Build exponent.
           (setq char (inch-read-buffer))
           ;; Check leading sign.
           (if (cond ((char= char #\+) t)
                     ((char= char #\-) (setq negative-exponent t)))
               ;; Flush sign.
               (setq char (inch-read-buffer)))
           ;; Read digits for exponent.
           (do* ((ch char (inch-read-buffer))
                 (dig (and (not (eofp ch)) (digit-char-p ch))
                      (and (not (eofp ch)) (digit-char-p ch))))
                ((not dig)
                 (setq exponent (if negative-exponent (- exponent) exponent)))
             (setq exponent (+ (* exponent 10) dig)))
           ;; Generate and return the float, depending on FLOAT-CHAR:
           (let* ((float-format (case (char-upcase float-char)
                                  (#\E *read-default-float-format*)
                                  (#\S 'short-float)
                                  (#\F 'single-float)
                                  (#\D 'double-float)
                                  (#\L 'long-float)
				  #+double-double
				  (#\W 'kernel:double-double-float)))
                  num)
	     (setq num (make-float-aux (* (expt 10 exponent) number) divisor
				       float-format stream))

	     (return-from make-float (if negative-fraction
                                             (- num)
                                             num))))
	  (t (error "Internal error in floating point reader.")))))

(defun make-float-aux (number divisor float-format stream)
  (handler-case
      (with-float-traps-masked (:underflow)
	(let ((result (coerce (/ number divisor) float-format)))
	  (when (and (zerop result) (not (zerop number)))
	    ;; With underflow traps disabled, reading any number
	    ;; smaller than least-positive-foo-float will return zero.
	    ;; But we really want to indicate that we can't read it.
	    ;; So if we converted the number to zero, but the number
	    ;; wasn't actually zero, throw an error.
	    (error "Underflow"))
	  result))
    (error ()
	   (%reader-error stream "Floating-point number not representable"))))


(defun make-ratio (stream)
  ;;assume *read-buffer* contains a legal ratio.  Build the number from
  ;;the string.
  ;;look for optional "+" or "-".
  (let ((numerator 0) (denominator 0) (char ()) (negative-number nil))
    (read-unwind-read-buffer)
    (setq char (inch-read-buffer))
    (cond ((char= char #\+)
	   (setq char (inch-read-buffer)))
	  ((char= char #\-)
	   (setq char (inch-read-buffer))
	   (setq negative-number t)))
    ;;get numerator
    (do* ((ch char (inch-read-buffer))
	  (dig (digit-char-p ch *read-base*)
	       (digit-char-p ch *read-base*)))
	 ((not dig))
	 (setq numerator (+ (* numerator *read-base*) dig)))
    ;;get denominator
    (do* ((ch (inch-read-buffer) (inch-read-buffer))
	  (dig ()))
	 ((or (eofp ch) (not (setq dig (digit-char-p ch *read-base*)))))
	 (setq denominator (+ (* denominator *read-base*) dig)))
    (when (zerop denominator)
      (%reader-error stream "Invalid ratio: ~S/~S"
		     (if negative-number (- numerator) numerator)
		     denominator))
    (let ((num (/ numerator denominator)))
      (if negative-number (- num) num))))

       

;;;; dispatching macro cruft

(defun make-char-dispatch-table ()
  (make-array char-code-limit :initial-element #'dispatch-char-error))

(defun dispatch-char-error (stream sub-char ignore)
  (declare (ignore ignore))
  (if *read-suppress*
      (values)
      (%reader-error stream "No dispatch function defined for ~S." sub-char)))

(defun make-dispatch-macro-character (char &optional
					   (non-terminating-p nil)
					   (rt *readtable*))
  "Causes char to become a dispatching macro character in readtable
   (which defaults to the current readtable).  If the non-terminating-p
   flag is set to T, the char will be non-terminating.  Make-dispatch-
   macro-character returns T."
  (set-macro-character char #'read-dispatch-char non-terminating-p rt)
  (let ((dalist (dispatch-tables rt)))
    (setf (dispatch-tables rt)
          (push (cons char (make-char-dispatch-table)) dalist)))
  t)

(defun set-dispatch-macro-character
       (disp-char sub-char function &optional (rt *readtable*))
  "Causes function to be called whenever the reader reads
   disp-char followed by sub-char. Set-dispatch-macro-character
   returns T."
  ;;get the dispatch char for macro (error if not there), diddle
  ;;entry for sub-char.
  (when (digit-char-p sub-char)
    (simple-program-error "Sub-Char must not be a decimal digit: ~S" sub-char))
  (let* ((sub-char (char-upcase sub-char))
	 (dpair (find disp-char (dispatch-tables rt)
		      :test #'char= :key #'car)))
    (if dpair
	(setf (elt (the simple-vector (cdr dpair))
		   (char-code sub-char))
	      (coerce function 'function))
	(simple-program-error "~S is not a dispatch character." disp-char))))

(defun get-dispatch-macro-character
       (disp-char sub-char &optional (rt *readtable*))
  "Returns the macro character function for sub-char under disp-char
   or nil if there is no associated function."
  (unless (digit-char-p sub-char)
    (let* ((sub-char (char-upcase sub-char))
	   (rt (or rt std-lisp-readtable))
	   (dpair (find disp-char (dispatch-tables rt)
			:test #'char= :key #'car)))
      (if dpair
	  (elt (the simple-vector (cdr dpair))
	       (char-code sub-char))
	  (simple-program-error "~S is not a dispatch character." disp-char)))))

(defun read-dispatch-char (stream char)
  ;;read some digits
  (let ((numargp nil)
	(numarg 0)
	(sub-char ()))
    (do* ((ch (read-char stream nil eof-object)
	      (read-char stream nil eof-object))
	  (dig ()))
	 ((or (eofp ch)
	      (not (setq dig (digit-char-p ch))))
	  ;;take care of the extra char.
	  (if (eofp ch)
	      (reader-eof-error stream "inside dispatch character")
	      (setq sub-char (char-upcase ch))))
      (setq numargp t)
      (setq numarg (+ (* numarg 10) dig)))
    ;;look up the function and call it.
    (let ((dpair (find char (dispatch-tables *readtable*)
		       :test #'char= :key #'car)))
      (if dpair
	  (funcall (the function
			(elt (the simple-vector (cdr dpair))
			     (char-code sub-char)))
		   stream sub-char (if numargp numarg nil))
	  (%reader-error stream "No dispatch table for dispatch char.")))))



;;;; READ-FROM-STRING.

(defvar read-from-string-spares ()
  "A resource of string streams for Read-From-String.")

(defun read-from-string (string &optional (eof-error-p t) eof-value
				&key (start 0) end
				preserve-whitespace)
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned.  Macro chars
   will take effect."
  (declare (string string))
  (with-array-data ((string string :offset-var offset)
		    (start start)
		    (end (or end (length string))))
    (unless read-from-string-spares
      (push (internal-make-string-input-stream "" 0 0)
	    read-from-string-spares))
    (let ((stream (pop read-from-string-spares)))
      (setf (string-input-stream-string stream) string)
      (setf (string-input-stream-current stream) start)
      (setf (string-input-stream-end stream) end)
      (unwind-protect
	  (values (if preserve-whitespace
		      (read-preserving-whitespace stream eof-error-p eof-value)
		      (read stream eof-error-p eof-value))
		  (- (string-input-stream-current stream) offset))
	(push stream read-from-string-spares)))))


;;;; PARSE-INTEGER.

(defun parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  "Examine the substring of string delimited by start and end
  (default to the beginning and end of the string)  It skips over
  whitespace characters and then tries to parse an integer.  The
  radix parameter must be between 2 and 36."
  (let ((orig-start start))
    (with-array-data ((string string)
		      (start start)
		      (end (or end (length string))))
      (let ((index start)
	    (sign 1)
	    (any-digits nil)
	    (result 0))
	(declare (type index index)
		 (type (member 1 -1) sign)
		 (type boolean any-digits)
		 (integer result))
	(flet ((skip-whitespace ()
		 (loop while (< index end)
		       while (whitespacep (char string index)) do
			 (incf index))))
	  (declare (inline skip-whitespace))
	  (skip-whitespace)
	  (when (< index end)
	    (case (char string index)
	      (#\+ (incf index))
	      (#\- (incf index) (setq sign -1))))
	  (loop while (< index end)
		for weight = (digit-char-p (char string index) radix)
		while weight do
		(incf index)
		(setq any-digits t)
		(setq result (+ (* radix result) weight)))
	  (unless junk-allowed (skip-whitespace))
	  ;;
	  ;; May be /= index if string is displaced.
	  (let ((real-index (+ (- index start) orig-start)))
	    (cond ((not any-digits)
		   (if junk-allowed
		       (values nil real-index)
		       (error 'simple-parse-error
			      :format-control "There are no digits in this string: ~S"
			      :format-arguments (list string))))
		  ((and (< index end) (not junk-allowed))
		   (error 'simple-parse-error
			  :format-control "There's junk in this string: ~S."
			  :format-arguments (list string)))
		  (t
		   (values (* sign result) real-index)))))))))


;;;; Reader initialization code.

(defun reader-init ()
  (init-read-buffer-stack)
  (init-secondary-attribute-table)
  (init-std-lisp-readtable)
; (init-integer-reader)
  )
