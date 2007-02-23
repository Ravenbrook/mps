;;;
;;; Boot file for removing the "" nickname of the KEYWORD package,
;;; removing the USER nickname from CL-USER, and removing the
;;; LISP nickname from COMMON-LISP, the latter by introducing
;;; a new COMMON-LISP package which LISP uses.
;;;
;;; To bootstrap, copy this file to target:bootstrap.lisp
;;; using Pierre Mai's build scripts, and do a full build.
;;;

(in-package :lisp)

#+relative-package-names
(defun relative-package-name-to-package (name)
  (declare (simple-string name)
	   (optimize (speed 3)))
  (flet ((relative-to (package name)
	   (declare (type package package)
		    (simple-string name))
           (if (string= "" name)
	       package
	       (let ((parent-name (package-%name package)))
		 (unless parent-name
		   (error "Can't do anything to a deleted package: ~S"
			  package))
		 (package-name-to-package
		  (concatenate 'simple-string parent-name "." name)))))
         (find-non-dot (name)
           (do* ((len (length name))
                 (i 0 (1+ i)))
		((= i len) nil)
             (declare (type index len i))
             (when (char/= #\. (schar name i)) (return i)))))
    (when (and (plusp (length name))
	       (char= #\. (schar name 0)))
      (let* ((last-dot-position (or (find-non-dot name) (length name)))
             (n-dots last-dot-position)
             (name (subseq name last-dot-position)))
        (cond ((= 1 n-dots)
               ;; relative to current package
               (relative-to *package* name))
              (t
               ;; relative to our (- n-dots 1)'th parent
               (let ((package *package*)
                     (tmp nil))
                 (dotimes (i (1- n-dots))
		   (declare (fixnum i))
		   (setq tmp (package-parent package))
                   (unless tmp
		     (error 'simple-package-error
                            :name (string package)
                            :format-control "The parent of ~a does not exist."
                            :format-arguments (list package)))
                   (setq package tmp))
                 (relative-to package name))))))))

(defun read-token (stream firstchar)
  "This function is just an fsm that recognizes numbers and symbols."
  ;;check explicitly whether firstchar has entry for non-terminating
  ;;in character-attribute-table and read-dot-number-symbol in CMT.
  ;;Report an error if these are violated (if we called this, we want
  ;;something that is a legitimate token!).
  ;;read in the longest possible string satisfying the bnf for
  ;;"unqualified-token".  Leave the result in the READ-BUFFER.
  ;;Return next char after token (last char read).
  (when *read-suppress*
    (internal-read-extended-token stream firstchar nil)
    (return-from read-token nil))
  (let ((attribute-table (character-attribute-table *readtable*))
	(package nil)
	(colons 0)
	(possibly-rational t)
	(possibly-float t)
	(escapes ()))
    (reset-read-buffer)
    (prog ((char firstchar))
      (case (char-class3 char attribute-table)
	(#.constituent-sign (go SIGN))
	(#.constituent-digit (go LEFTDIGIT))
	(#.constituent-dot (go FRONTDOT))
	(#.escape (go ESCAPE))
	(#.package-delimiter (go COLON))
	(#.multiple-escape (go MULT-ESCAPE))
	;;can't have eof, whitespace, or terminating macro as first char!
	(t (go SYMBOL)))
     SIGN
      ;;saw "sign"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (setq possibly-rational t
	    possibly-float t)
      (case (char-class3 char attribute-table)
	(#.constituent-digit (go LEFTDIGIT))
	(#.constituent-dot (go SIGNDOT))
	(#.escape (go ESCAPE))
	(#.package-delimiter (go COLON))
	(#.multiple-escape (go MULT-ESCAPE))	
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(t (go SYMBOL)))
     LEFTDIGIT
      ;;saw "[sign] {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-integer)))
      (case (char-class3 char attribute-table)
	(#.constituent-digit (go LEFTDIGIT))
	(#.constituent-dot (if possibly-float
			       (go MIDDLEDOT)
			       (go SYMBOL)))
	(#.constituent-expt (go EXPONENT))
	(#.constituent-slash (if possibly-rational
				 (go RATIO)
				 (go SYMBOL)))
	(#.delimiter (unread-char char stream) (return (make-integer)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     MIDDLEDOT
      ;;saw "[sign] {digit}+ dot"
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
     RIGHTDIGIT
      ;;saw "[sign] {digit}* dot {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float)))
      (case (char-class char attribute-table)
	(#.constituent-digit (go RIGHTDIGIT))
	(#.constituent-expt (go EXPONENT))
	(#.delimiter (unread-char char stream) (return (make-float)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     SIGNDOT
      ;;saw "[sign] dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.constituent-digit (go RIGHTDIGIT))
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(t (go SYMBOL)))
     FRONTDOT
      ;;saw "dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (%reader-error stream "Dot context error."))
      (case (char-class char attribute-table)
	(#.constituent-digit (go RIGHTDIGIT))
	(#.constituent-dot (go DOTS))
	(#.delimiter  (%reader-error stream "Dot context error."))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     EXPONENT
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.constituent-sign (go EXPTSIGN))
	(#.constituent-digit (go EXPTDIGIT))
	(#.delimiter (unread-char char stream) (go RETURN-SYMBOL))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     EXPTSIGN
      ;;we got to EXPONENT, and saw a sign character.
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
     EXPTDIGIT
      ;;got to EXPONENT, saw "[sign] {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float)))
      (case (char-class char attribute-table)
	(#.constituent-digit (go EXPTDIGIT))
	(#.delimiter (unread-char char stream) (return (make-float)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     RATIO
      ;;saw "[sign] {digit}+ slash"
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
     RATIODIGIT
      ;;saw "[sign] {digit}+ slash {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-ratio)))
      (case (char-class2 char attribute-table)
	(#.constituent-digit (go RATIODIGIT))
	(#.delimiter (unread-char char stream) (return (make-ratio)))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     DOTS
      ;;saw "dot {dot}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (%reader-error stream "Too many dots."))
      (case (char-class char attribute-table)
	(#.constituent-dot (go DOTS))
	(#.delimiter
	 (unread-char char stream)
	 (%reader-error stream "Too many dots."))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter (go COLON))
	(t (go SYMBOL)))
     SYMBOL
      ;;not a dot, dots, or number.
      (let ((stream (in-synonym-of stream)))
	(if (lisp-stream-p stream)
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
	    ;; Fundamental-stream.
	    (prog ()
	     SYMBOL-LOOP
	     (ouch-read-buffer char)
	     (setq char (stream-read-char stream))
	     (when (eq char :eof) (go RETURN-SYMBOL))
	     (case (char-class char attribute-table)
	       (#.escape (go ESCAPE))
	       (#.delimiter (stream-unread-char stream char)
			    (go RETURN-SYMBOL))
	       (#.multiple-escape (go MULT-ESCAPE))
	       (#.package-delimiter (go COLON))
	       (t (go SYMBOL-LOOP))))))
     ESCAPE
      ;;saw an escape.
      ;;don't put the escape in the read-buffer.
      ;;read-next char, put in buffer (no case conversion).
      (let ((nextchar (read-char stream nil nil)))
	(unless nextchar
	  (reader-eof-error stream "after escape character"))
	(push ouch-ptr escapes)
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
      (do ((char (read-char stream t) (read-char stream t)))
	  ((multiple-escape-p char))
	(if (escapep char) (setq char (read-char stream t)))
	(push ouch-ptr escapes)
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
	(%reader-error stream "Too many colons in ~S"
		      (read-buffer-to-string)))
      (setq colons 1)
      (setq package
	    (if (eql (char-class firstchar attribute-table)
		     #.package-delimiter)
		*keyword-package*
		(read-buffer-to-string)))
      (reset-read-buffer)
      (setq escapes ())
      (setq char (read-char stream nil nil))
      (unless char (reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-table)
	(#.delimiter
	 (unread-char char stream)
	 (%reader-error stream "Illegal terminating character after a colon, ~S."
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
	 (%reader-error stream "Illegal terminating character after a colon, ~S"
		       char))
	(#.escape (go ESCAPE))
	(#.multiple-escape (go MULT-ESCAPE))
	(#.package-delimiter
	 (%reader-error stream "To many colons after ~S:" package))
	(t (go SYMBOL)))
      RETURN-SYMBOL
      (casify-read-buffer escapes)
      (let ((found (if package
		       (find-package package)
		       *package*)))
	(unless found
	  (error 'reader-package-error :stream stream
		 :format-arguments (list package)
		 :format-control "Package ~S not found."))

	(if (or (zerop colons) (= colons 2) (eq found *keyword-package*))
	    (return (intern* read-buffer ouch-ptr found))
	    (multiple-value-bind (symbol test)
				 (find-symbol* read-buffer ouch-ptr found)
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

(set-cmt-entry #\: #'read-token)

(rename-package "KEYWORD" "KEYWORD" nil)

(cl:rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("CL-USER"))
(unuse-package "COMMON-LISP" "COMMON-LISP-USER")
(use-package "LISP" "COMMON-LISP-USER")

(rename-package "COMMON-LISP" "COMMON-LISP" nil)
(rename-package "COMMON-LISP" "LISP" nil)
(make-package "COMMON-LISP" :nicknames '("CL") :use nil)

(let ((cl (find-package "CL"))
      (lisp (find-package "LISP")))
  (do-external-symbols (sym lisp)
    (unintern sym lisp)
    (let ((syms (list sym)))
      (import syms cl)
      (export syms cl)
      (import syms lisp)
      (export syms lisp))))

(cl:use-package "CL" "LISP")

(in-package :cl-user)

;;; end of file.
