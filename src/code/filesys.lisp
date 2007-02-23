;;; -*- Log: code.log; Package: Lisp -*-
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/filesys.lisp,v 1.100 2006/03/14 15:19:10 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; File system interface functions.  This file is pretty UNIX specific.
;;;
;;; Written by William Lott
;;;
;;; **********************************************************************

(in-package "LISP")

(export '(truename probe-file user-homedir-pathname directory
          rename-file delete-file file-write-date file-author))

(use-package "EXTENSIONS")

(in-package "EXTENSIONS")
(export '(print-directory complete-file ambiguous-files default-directory
	  purge-backup-files file-writable unix-namestring))
(in-package "LISP")


;;;; Unix pathname host support.

;;; Unix namestrings have the following format:
;;;
;;; namestring := [ directory ] [ file [ type [ version ]]]
;;; directory := [ "/" | search-list ] { file "/" }*
;;; search-list := [^:/]*:
;;; file := [^/]*
;;; type := "." [^/.]*
;;; version := ".*" | ".~" ([0-9]+ | "*") "~"
;;;
;;; Note: this grammar is ambiguous.  The string foo.bar.~5~ can be parsed
;;; as either just the file specified or as specifying the file, type, and
;;; version.  Therefore, we use the following rules when confronted with
;;; an ambiguous file.type.version string:
;;;
;;; - If the first character is a dot, it's part of the file.  It is not
;;; considered a dot in the following rules.
;;;
;;; - If there is only one dot, it separates the file and the type.
;;;
;;; - If there are multiple dots and the stuff following the last dot
;;; is a valid version, then that is the version and the stuff between
;;; the second to last dot and the last dot is the type.
;;;
;;; Wildcard characters:
;;;
;;; If the directory, file, type components contain any of the following
;;; characters, it is considered part of a wildcard pattern and has the
;;; following meaning.
;;;
;;; ? - matches any character
;;; * - matches any zero or more characters.
;;; [abc] - matches any of a, b, or c.
;;; {str1,str2,...,strn} - matches any of str1, str2, ..., or strn.
;;;
;;; Any of these special characters can be preceded by a backslash to
;;; cause it to be treated as a regular character.
;;;

(defun remove-backslashes (namestr start end)
  "Remove any occurrences of \\ from the string because we've already
   checked for whatever may have been backslashed."
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((result (make-string (- end start)))
	 (dst 0)
	 (quoted nil))
    (do ((src start (1+ src)))
	((= src end))
      (cond (quoted
	     (setf (schar result dst) (schar namestr src))
	     (setf quoted nil)
	     (incf dst))
	    (t
	     (let ((char (schar namestr src)))
	       (cond ((char= char #\\)
		      (setq quoted t))
		     (t
		      (setf (schar result dst) char)
		      (incf dst)))))))
    (when quoted
      (error 'namestring-parse-error
	     :complaint "Backslash in bad place."
	     :namestring namestr
	     :offset (1- end)))
    (shrink-vector result dst)))

(defvar *ignore-wildcards* nil
  "If non-NIL, Unix shell-style wildcards are ignored when parsing
  pathname namestrings.  They are also ignored when computing
  namestrings for pathname objects.  Thus, *, ?, etc. are not
  wildcards when parsing a namestring, and are not escaped when
  printing pathnames.")

(defun maybe-make-pattern (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (if *ignore-wildcards*
      (subseq namestr start end)
      (collect ((pattern))
	(let ((quoted nil)
	      (any-quotes nil)
	      (last-regular-char nil)
	      (index start))
	  (flet ((flush-pending-regulars ()
		   (when last-regular-char
		     (pattern (if any-quotes
				  (remove-backslashes namestr
						      last-regular-char
						      index)
				  (subseq namestr last-regular-char index)))
		     (setf any-quotes nil)
		     (setf last-regular-char nil))))
	    (loop
	      (when (>= index end)
		(return))
	      (let ((char (schar namestr index)))
		(cond (quoted
		       (incf index)
		       (setf quoted nil))
		      ((char= char #\\)
		       (setf quoted t)
		       (setf any-quotes t)
		       (unless last-regular-char
			 (setf last-regular-char index))
		       (incf index))
		      ((char= char #\?)
		       (flush-pending-regulars)
		       (pattern :single-char-wild)
		       (incf index))
		      ((char= char #\*)
		       (flush-pending-regulars)
		       (pattern :multi-char-wild)
		       (incf index))
		      ((char= char #\[)
		       (flush-pending-regulars)
		       (let ((close-bracket
			      (position #\] namestr :start index :end end)))
			 (unless close-bracket
			   (error 'namestring-parse-error
				  :complaint "``['' with no corresponding ``]''"
				  :namestring namestr
				  :offset index))
			 (pattern (list :character-set
					(subseq namestr
						(1+ index)
						close-bracket)))
			 (setf index (1+ close-bracket))))
		      (t
		       (unless last-regular-char
			 (setf last-regular-char index))
		       (incf index)))))
	    (flush-pending-regulars)))
	(cond ((null (pattern))
	       "")
	      ((null (cdr (pattern)))
	       (let ((piece (first (pattern))))
		 (typecase piece
		   ((member :multi-char-wild) :wild)
		   (simple-string piece)
		   (t
		    (make-pattern (pattern))))))
	      (t
	       (make-pattern (pattern)))))))

;;; extract-name-type-and-version  --  Internal.
;;;
(defun extract-name-type-and-version (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (labels
      ((explicit-version (namestr start end)
	 ;; Look for something like "~*~" at the end of the
	 ;; namestring, where * can be #\* or some digits.  This
	 ;; denotes a version.
	 ;;(format t "explicit-version ~S ~A ~A~%" namestr start end)
	 (cond ((or (< (- end start) 4)
		    (and (char/= (schar namestr (1- end)) #\~)
			 (char/= (schar namestr (1- end)) #\*)))
		;; No explicit version given, so return NIL to
		;; indicate we don't want file versions, unless
		;; requested in other ways.
		;;(format t "case 1: ~A ~A~%" nil end)
		(values nil end))
	       ((and (not *ignore-wildcards*)
		     (char= (schar namestr (- end 2)) #\*)
		     (char= (schar namestr (- end 3)) #\~)
		     (char= (schar namestr (- end 4)) #\.))
		;; Found "~*~", so it's a wild version
		;;(format t "case 2: ~A ~A~%" :wild (- end 4))
		(values :wild (- end 4)))
	       (t
		;; Look for a version number.  Start at the end, just
		;; before the ~ and keep looking for digits.  If the
		;; first non-digit is ~, we have a version number, so
		;; get it.  If not, we didn't find a version number,
		;; so we call it :newest
		(do ((i (- end 2) (1- i)))
		    ((< i (+ start 1))
		     ;;(format t "case 3: ~A ~A~%" :newest end)
		     (values :newest end))
		  (let ((char (schar namestr i)))
		    (when (eql char #\~)
		      (return (if (char= (schar namestr (1- i)) #\.)
				  (values (parse-integer namestr :start (1+ i)
							 :end (1- end))
					  (1- i))
				  (values :newest end))))
		    (unless (char<= #\0 char #\9)
		      ;; It's not a digit.  Give up, and say the
		      ;; version is NIL.
		      ;;(format t "case 3 return: ~A ~A~%" nil end)
		      (return (values nil end))))))))
       (any-version (namestr start end)
	 ;; process end of string looking for a version candidate.
	 (multiple-value-bind (version where)
	     (explicit-version namestr start end)
	   (cond ((not (eq version :newest))
		  (values version where))
		 ((and (not *ignore-wildcards*)
		       (>= (- end 2) start)
		       (char= (schar namestr (- end 1)) #\*)
		       (char= (schar namestr (- end 2)) #\.)
		       (find #\. namestr
			     :start (min (1+ start) (- end 2))
			     :end (- end 2)))
		  (values :wild (- end 2)))
		 (t (values version where)))))
       (any-type (namestr start end)
	 ;; Process end of string looking for a type. A leading "."
	 ;; is part of the name.
	 (let ((where (position #\. namestr
				:start (min (1+ start) end)
				:end end :from-end t)))
	   (when where
	     (values where end))))
       (any-name (namestr start end)
	 (declare (ignore namestr))
	 (values start end)))
    (multiple-value-bind (version vstart)
	(any-version namestr start end)
      (multiple-value-bind (tstart tend)
	  (any-type namestr start vstart)
	(multiple-value-bind (nstart nend)
	    (any-name namestr start (or tstart vstart))
	  (values
	   (maybe-make-pattern namestr nstart nend)
	   (and tstart (maybe-make-pattern namestr (1+ tstart) tend))
	   version))))))

;;; Take a string and return a list of cons cells that mark the char
;;; separated subseq. The first value t if absolute directories location.
;;;
(defun split-at-slashes (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((absolute (and (/= start end)
		       (char= (schar namestr start) #\/))))
    (when absolute
      (incf start))
    ;; Next, split the remainder into slash separated chunks.
    (collect ((pieces))
      (loop
	(let ((slash (position #\/ namestr :start start :end end)))
	  (pieces (cons start (or slash end)))
	  (unless slash
	    (return))
	  (setf start (1+ slash))))
      (values absolute (pieces)))))

(defun maybe-extract-search-list (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((quoted nil))
    (do ((index start (1+ index)))
	((= index end)
	 (values nil start))
      (if quoted
	  (setf quoted nil)
	  (case (schar namestr index)
	    (#\\
	     (setf quoted t))
	    (#\:
	     (return (values (remove-backslashes namestr start index)
			     (1+ index)))))))))

(defun parse-unix-namestring (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (multiple-value-bind
      (absolute pieces)
      (split-at-slashes namestr start end)
    (let ((search-list
	   (if absolute
	       nil
	       (let ((first (car pieces)))
		 (multiple-value-bind
		       (search-list new-start)
		     (maybe-extract-search-list namestr
						(car first) (cdr first))
		   (when search-list
		     ;; Lose if this search-list is already defined as
		     ;; a logical host.  Since the syntax for
		     ;; search-lists and logical pathnames are the
		     ;; same, we can't allow the creation of one when
		     ;; the other is defined.
		     (when (find-logical-host search-list nil)
		       (error "~A already names a logical host" search-list))
		     (setf absolute t)
		     (setf (car first) new-start))
		   search-list)))))
      (multiple-value-bind (name type version)
	  (let* ((tail (car (last pieces)))
		 (tail-start (car tail))
		 (tail-end (cdr tail)))
	    (unless (= tail-start tail-end)
	      (setf pieces (butlast pieces))
	      (cond ((string= namestr ".." :start1 tail-start :end1 tail-end)
		     ;; ".." is a directory.  Add this piece to the
		     ;; list of pieces, and make the name/type/version
		     ;; nil.
		     (setf pieces (append pieces (list (cons tail-start tail-end))))
		     (values nil nil nil))
		    ((not (find-if-not #'(lambda (c)
					   (char= c #\.))
				       namestr :start tail-start :end tail-end))
		     ;; Got a bunch of dots.  Make it a file of the same name, and type nil.
		     (values (subseq namestr tail-start tail-end) nil nil))
		    (t
		     (extract-name-type-and-version namestr tail-start tail-end)))))
	;; PVE: Make sure there are no illegal characters in the name
	;; such as #\Null and #\/.
	(when (and (stringp name)
                   (find-if #'(lambda (x)
				(or (char= x #\Null) (char= x #\/)))
			    name))
	  (error 'parse-error))
	;; Now we have everything we want.  So return it.
	(values nil ; no host for unix namestrings.
		nil ; no devices for unix namestrings.
		(collect ((dirs))
		  (when search-list
		    (dirs (intern-search-list search-list)))
		  (dolist (piece pieces)
		    (let ((piece-start (car piece))
			  (piece-end (cdr piece)))
		      (unless (= piece-start piece-end)
			(cond ((string= namestr ".." :start1 piece-start
					:end1 piece-end)
			       (dirs :up))
			      ((string= namestr "**" :start1 piece-start
					:end1 piece-end)
			       (dirs :wild-inferiors))
			      (t
			       (dirs (maybe-make-pattern namestr
							 piece-start
							 piece-end)))))))
		  (cond (absolute
			 (cons :absolute (dirs)))
			((dirs)
			 ;; "." in a :relative directory is the same
			 ;; as if it weren't there, so remove them.
			 (cons :relative (delete "." (dirs) :test #'equal)))
			(t
			 ;; If there is no directory and the name is
			 ;; ".", we really got directory ".", so make it so.
			 (if (equal name ".")
			     (list :relative)
			 nil))))
		;; A file with name "." can't be the name of file on
		;; Unix because it's a directory.  This was handled
		;; above, so we can just set the name to nil.
		(if (equal name ".")
		    nil
		    name)
		type
		version)))))

(defun unparse-unix-host (pathname)
  (declare (type pathname pathname)
	   (ignore pathname))
  ;; this host designator needs to be recognized as a physical host in
  ;; PARSE-NAMESTRING. Until sbcl-0.7.3.x, we had "Unix" here, but
  ;; that's a valid Logical Hostname, so that's a bad choice. -- CSR,
  "")

(defun unparse-unix-piece (thing)
  (etypecase thing
    ((member :wild) "*")
    ((member :unspecific)
     ;; CLHS 19.2.2.2.3.1 says "That is, both nil and :unspecific
     ;; cause the component not to appear in the namestring."
     "")
    (simple-string
     (if *ignore-wildcards*
	 thing
	 (let* ((srclen (length thing))
		(dstlen srclen))
	   (dotimes (i srclen)
	     (case (schar thing i)
	       ((#\* #\? #\[)
		(incf dstlen))))
	   (let ((result (make-string dstlen))
		 (dst 0))
	     (dotimes (src srclen)
	       (let ((char (schar thing src)))
		 (case char
		   ((#\* #\? #\[)
		    (setf (schar result dst) #\\)
		    (incf dst)))
		 (setf (schar result dst) char)
		 (incf dst)))
	     result))))
    (pattern
     (collect ((strings))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string
	    (strings piece))
	   (symbol
	    (ecase piece
	      (:multi-char-wild
	       (strings "*"))
	      (:single-char-wild
	       (strings "?"))))
	   (cons
	    (case (car piece)
	      (:character-set
	       (strings "[")
	       (strings (second piece))
	       (strings "]"))
	      (t
	       (error "Invalid pattern piece: ~S" piece))))))
       (apply #'concatenate
	      'simple-string
	      (strings))))))

(defun unparse-unix-directory-list (directory)
  (declare (type list directory))
  (collect ((pieces))
    (when directory
      (ecase (pop directory)
	(:absolute
	 (cond ((search-list-p (car directory))
		(pieces (search-list-name (pop directory)))
		(pieces ":"))
	       (t
		(pieces "/"))))
	(:relative
	 ;; Nothing special, except if we were given '(:relative).
	 (unless directory
	   (pieces "./"))
	 ))
      (dolist (dir directory)
	(typecase dir
	  ((member :up)
	   (pieces "../"))
	  ((member :back)
	   (error ":BACK cannot be represented in namestrings."))
	  ((member :wild-inferiors)
	   (pieces "**/"))
	  ((or simple-string pattern (eql :wild))
	   (pieces (unparse-unix-piece dir))
	   (pieces "/"))
	  (t
	   (error "Invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-string (pieces))))

(defun unparse-unix-directory (pathname)
  (declare (type pathname pathname))
  (unparse-unix-directory-list (%pathname-directory pathname)))

(defun unparse-unix-file (pathname)
  (declare (type pathname pathname))
  (collect ((strings))
    (let* ((name (%pathname-name pathname))
	   (type (%pathname-type pathname))
	   (type-supplied (not (or (null type) (eq type :unspecific))))
	   (logical-p (logical-pathname-p pathname))
	   (version (%pathname-version pathname))
	   ;; Preserve version :newest for logical pathnames.
	   (version-supplied (not (or (null version)
				      (member version (if logical-p
							  '(:unspecific)
							  '(:newest
							    :unspecific)))))))
      (when name
	(when (stringp name)
	  (when (find #\/ name)
	    (error "Cannot specify a directory separator in a pathname name: ~S" name))
	  (when (and (not type-supplied)
		     (find #\. name :start 1))
	    ;; A single leading dot is ok.
	    (error "Cannot specify a dot in a pathname name without a pathname type: ~S" name))
	  (when (or (string= ".." name)
		    (string= "." name))
	    (error "Invalid value for a pathname name: ~S" name)))
	(strings (unparse-unix-piece name)))
      (when type-supplied
	(unless name
	  (error "Cannot specify the type without a file: ~S" pathname))
	(when (stringp type)
	  (when (find #\/ type)
	    (error "Cannot specify a directory separator in a pathname type: ~S" type))
	  (when (find #\. type)
	    (error "Cannot specify a dot in a pathname type: ~S" type)))
	(strings ".")
	(strings (unparse-unix-piece type)))
      (when (and (not (member version '(nil :newest :unspecific)))
		 (not name))
	;; We don't want version without a name, because when we try
	;; to read #p".~*~" back, the name is "", not NIL.
	(error "Cannot specify a version without a file: ~S" pathname))
      (when version-supplied
	(strings (if (eq version :wild)
		     (if logical-p ".*" ".~*~")
		     (format nil (if logical-p ".~A" ".~~~D~~")
			     version)))))
    (and (strings) (apply #'concatenate 'simple-string (strings)))))

(defun unparse-unix-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
	       (unparse-unix-directory pathname)
	       (unparse-unix-file pathname)))

(defun unparse-unix-enough (pathname defaults)
  (declare (type pathname pathname defaults))
  (flet ((lose ()
	   (error "~S cannot be represented relative to ~S"
		  pathname defaults)))
    ;; Only the first path in a search-list is considered.
    (enumerate-search-list (pathname pathname)
      (enumerate-search-list (defaults defaults)
	(collect ((strings))
	  (let* ((pathname-directory (%pathname-directory pathname))
		 (defaults-directory (%pathname-directory defaults))
		 (prefix-len (length defaults-directory))
		 (result-dir
		  (cond ((null pathname-directory)
			 ;; No directory, so relative to default.  But
			 ;; if we're relative to default, NIL is as
			 ;; good as '(:relative) and it results in a
			 ;; shorter namestring.
			 #+nil (list :relative)
			 nil)
			((and (>= prefix-len 1)
			      (>= (length pathname-directory) prefix-len)
			      (compare-component (subseq pathname-directory
							 0 prefix-len)
						 defaults-directory))
			 ;; Pathname starts with a prefix of default,
			 ;; which also means both are either :relative
			 ;; or :absolute directories.  So just use a
			 ;; relative directory from then on out.
			 (let ((dir-tail (nthcdr prefix-len pathname-directory)))
			   ;; If both directories are identical, don't
			   ;; return just :relative.  Returning NIL
			   ;; results in a shorter string.
			   (if dir-tail
			       (cons :relative dir-tail)
			       nil)))
			((and (eq (car pathname-directory) :relative)
			      (not (eq (car defaults-directory) :absolute)))
			 ;; Can't represent a relative directory
			 ;; relative to an absolute directory.  But
			 ;; there's no problem if both are relative;
			 ;; we just return our path.
			 pathname-directory)
			((eq (car pathname-directory) :absolute)
			 ;; We are an absolute pathname, so we can just use it.
			 pathname-directory)
			(t
			 ;; We are a relative directory.  So we lose.
			 (lose)))))
	    (strings (unparse-unix-directory-list result-dir)))
	  (let* ((pathname-version (%pathname-version pathname))
		 (version-needed (and pathname-version
				      (not (eq pathname-version :newest))))
		 (pathname-type (%pathname-type pathname))
		 (type-needed (or version-needed
				  (and pathname-type
				       (not (eq pathname-type :unspecific)))))
		 (pathname-name (%pathname-name pathname))
		 (name-needed (or type-needed
				  (and pathname-name
				       (not (compare-component pathname-name
							       (%pathname-name
								defaults)))))))
	    (when name-needed
	      (unless pathname-name (lose))
	      (strings (unparse-unix-piece pathname-name)))
	    (when type-needed
	      (when (or (null pathname-type) (eq pathname-type :unspecific))
		(lose))
	      (strings ".")
	      (strings (unparse-unix-piece pathname-type)))
	    (when version-needed
	      (typecase pathname-version
		((member :wild)
		 (strings ".~*~"))
		(integer
		 (strings (format nil ".~~~D~~" pathname-version)))
		(t
		 (lose)))))
	  (return-from unparse-unix-enough (apply #'concatenate 'simple-string (strings))))))))


(defstruct (unix-host
	    (:include host
		      (:parse #'parse-unix-namestring)
		      (:unparse #'unparse-unix-namestring)
		      (:unparse-host #'unparse-unix-host)
		      (:unparse-directory #'unparse-unix-directory)
		      (:unparse-file #'unparse-unix-file)
		      (:unparse-enough #'unparse-unix-enough)
		      (:customary-case :lower))
	    (:make-load-form-fun make-unix-host-load-form))
  )

(defvar *unix-host* (make-unix-host))

(defun make-unix-host-load-form (host)
  (declare (ignore host))
  '*unix-host*)


;;;; Wildcard matching stuff.

(defmacro enumerate-matches ((var pathname &optional result
				  &key (verify-existance t) (follow-links t))
			     &body body)
  (let ((body-name (gensym)))
    `(block nil
       (flet ((,body-name (,var)
		,@body))
	 (%enumerate-matches (pathname ,pathname)
			     ,verify-existance ,follow-links
			     #',body-name)
	 ,result))))

(defun %enumerate-matches (pathname verify-existance follow-links function)
  (when (pathname-type pathname)
    (unless (pathname-name pathname)
      (error "Cannot supply a type without a name:~%  ~S" pathname)))
  (let ((directory (pathname-directory pathname)))
    (if directory
	(ecase (car directory)
	  (:absolute
	   (%enumerate-directories "/" (cdr directory) pathname
				   verify-existance follow-links
				   nil function))
	  (:relative
	   (%enumerate-directories "" (cdr directory) pathname
				   verify-existance follow-links
				   nil function)))
	(%enumerate-files "" pathname verify-existance function))))

;;; %enumerate-directories  --   Internal
;;;
;;; The directory node and device numbers are maintained for the current path
;;; during the search for the detection of path loops upon :wild-inferiors.
;;;
(defun %enumerate-directories (head tail pathname verify-existance
			       follow-links nodes function)
  (declare (simple-string head))
  (macrolet ((unix-xstat (name)
	       `(if follow-links
		    (unix:unix-stat ,name)
		    (unix:unix-lstat ,name)))
	     (with-directory-node-noted ((head) &body body)
	       `(multiple-value-bind (res dev ino mode)
		    (unix-xstat ,head)
		  (when (and res (eql (logand mode unix:s-ifmt) unix:s-ifdir))
		    (let ((nodes (cons (cons dev ino) nodes)))
		      ,@body))))
	     (do-directory-entries ((name directory) &body body)
	       `(let ((dir (unix:open-dir ,directory)))
		  (when dir
		    (unwind-protect
			 (loop
			  (let ((,name (unix:read-dir dir)))
			    (cond ((null ,name)
				   (return))
				  ((string= ,name "."))
				  ((string= ,name ".."))
				  (t
				   ,@body))))
		      (unix:close-dir dir))))))
    (if tail
	(let ((piece (car tail)))
	  (etypecase piece
	    (simple-string
	     (let ((head (concatenate 'string head piece)))
	       (with-directory-node-noted (head)
		 (%enumerate-directories (concatenate 'string head "/")
					 (cdr tail) pathname
					 verify-existance follow-links
					 nodes function))))
	    ((member :wild-inferiors)
	     (%enumerate-directories head (rest tail) pathname
				     verify-existance follow-links
				     nodes function)
	     (do-directory-entries (name head)
	       (let ((subdir (concatenate 'string head name)))
		 (multiple-value-bind (res dev ino mode)
		     (unix-xstat subdir)
		   (declare (type (or fixnum null) mode))
		   (when (and res (eql (logand mode unix:s-ifmt) unix:s-ifdir))
		     (unless (dolist (dir nodes nil)
			       (when (and (eql (car dir) dev)
					  (eql (cdr dir) ino))
				 (return t)))
		       (let ((nodes (cons (cons dev ino) nodes))
			     (subdir (concatenate 'string subdir "/")))
			 (%enumerate-directories subdir tail pathname
						 verify-existance follow-links
						 nodes function))))))))
	    ((or pattern (member :wild))
	     (do-directory-entries (name head)
	       (when (or (eq piece :wild) (pattern-matches piece name))
		 (let ((subdir (concatenate 'string head name)))
		   (multiple-value-bind (res dev ino mode)
		       (unix-xstat subdir)
		     (declare (type (or fixnum null) mode))
		     (when (and res
				(eql (logand mode unix:s-ifmt) unix:s-ifdir))
		       (let ((nodes (cons (cons dev ino) nodes))
			     (subdir (concatenate 'string subdir "/")))
			 (%enumerate-directories subdir (rest tail) pathname
						 verify-existance follow-links
						 nodes function))))))))
	    ((member :up)
	     (let ((head (concatenate 'string head "..")))
	       (with-directory-node-noted (head)
		 (%enumerate-directories (concatenate 'string head "/")
					 (rest tail) pathname
					 verify-existance follow-links
					 nodes function))))))
	(%enumerate-files head pathname verify-existance function))))

(defun %enumerate-files (directory pathname verify-existance function)
  (declare (simple-string directory))
  (let ((name (%pathname-name pathname))
	(type (%pathname-type pathname))
	(version (%pathname-version pathname)))
    (cond ((member name '(nil :unspecific))
	   (when (or (not verify-existance)
		     (unix:unix-file-kind directory))
	     (funcall function directory)))
	  ((or (pattern-p name)
	       (pattern-p type)
	       (eq name :wild)
	       (eq type :wild)
	       (eq version :wild))
	   (let ((dir (unix:open-dir directory)))
	     (when dir
	       (unwind-protect
		   (loop
		     (let ((file (unix:read-dir dir)))
		       (if file
			   (unless (or (string= file ".")
				       (string= file ".."))
			     (multiple-value-bind
				 (file-name file-type file-version)
				 (let ((*ignore-wildcards* t))
				   (extract-name-type-and-version
				    file 0 (length file)))
			       ;; Match also happens if the file has
			       ;; no explicit version and we're asking
			       ;; for version :NEWEST, since that's
			       ;; what no version means.
			       (when (and (components-match file-name name)
					  (components-match file-type type)
					  (or (components-match file-version
								version)
					      (and (eq file-version nil)
						   (eq version :newest))))
				 (funcall function
					  (concatenate 'string
						       directory
						       file)))))
			   (return))))
		 (unix:close-dir dir)))))
	  (t
	   (let ((file (concatenate 'string directory name)))
	     (unless (or (null type) (eq type :unspecific))
	       (setf file (concatenate 'string file "." type)))
	     (unless (member version '(nil :newest :wild :unspecific))
	       (setf file (concatenate 'string file ".~"
				       (quick-integer-to-string version)
				       "~")))
	     (when (or (not verify-existance)
		       (unix:unix-file-kind file t))
	       (funcall function file)))))))

(defun quick-integer-to-string (n)
  (declare (type integer n))
  (cond ((not (fixnump n))
	 (write-to-string n :base 10 :radix nil))
	((zerop n) "0")
	((eql n 1) "1")
	((minusp n)
	 (concatenate 'simple-string "-"
		      (the simple-string (quick-integer-to-string (- n)))))
	(t
	 (do* ((len (1+ (truncate (integer-length n) 3)))
	       (res (make-string len))
	       (i (1- len) (1- i))
	       (q n)
	       (r 0))
	      ((zerop q)
	       (incf i)
	       (replace res res :start2 i :end2 len)
	       (shrink-vector res (- len i)))
	   (declare (simple-string res)
		    (fixnum len i r q))
	   (multiple-value-setq (q r) (truncate q 10))
	   (setf (schar res i) (schar "0123456789" r))))))


;;;; UNIX-NAMESTRING -- public
;;; 
(defun unix-namestring (pathname &optional (for-input t) executable-only)
  "Convert PATHNAME into a string that can be used with UNIX system calls.
   Search-lists and wild-cards are expanded. If optional argument
   FOR-INPUT is true and PATHNAME doesn't exist, NIL is returned.
   If optional argument EXECUTABLE-ONLY is true, NIL is returned
   unless an executable version of PATHNAME exists."
  ;; toy@rtp.ericsson.se: Let unix-namestring also handle logical
  ;; pathnames too.
  (let ((path (let ((lpn (pathname pathname)))
		(if (logical-pathname-p lpn)
		    (namestring (translate-logical-pathname lpn))
		    pathname))))
  (enumerate-search-list
      (pathname path)
    (collect ((names))
      (enumerate-matches (name pathname nil :verify-existance for-input
			       :follow-links t)
	(when (or (not executable-only)
		  (and (eq (unix:unix-file-kind name) :file)
		       (unix:unix-access name unix:x_ok)))
	  (names name)))
      (let ((names (names)))
	(when names
	  (when (cdr names)
	    (error 'simple-file-error
		   :format-control "~S is ambiguous:~{~%  ~A~}"
		   :format-arguments (list pathname names)))
	  (return (car names))))))))


;;;; TRUENAME and PROBE-FILE.

;;; Truename  --  Public
;;;
;;; Another silly file function trivially different from another function.
;;;
(defun truename (pathname)
  "Return the pathname for the actual file described by the pathname
  An error of type file-error is signalled if no such file exists,
  or the pathname is wild."
  (if (wild-pathname-p pathname)
      (error 'simple-file-error
	     :format-control "Bad place for a wild pathname."
	     :pathname pathname)
      (let ((result (probe-file pathname)))
	(unless result
	  (error 'simple-file-error
		 :pathname pathname
		 :format-control "The file ~S does not exist."
		 :format-arguments (list (namestring pathname))))
	result)))

;;; Probe-File  --  Public
;;;
;;; If PATHNAME exists, return its truename, otherwise NIL.
;;;
(defun probe-file (pathname)
  "Return a pathname which is the truename of the file if it exists, NIL
  otherwise. An error of type file-error is signalled if pathname is wild."
  (if (wild-pathname-p pathname)
      (error 'simple-file-error 
	     :pathname pathname
	     :format-control "Bad place for a wild pathname.")
      (let ((namestring (unix-namestring (merge-pathnames pathname) t)))
	(when (and namestring (unix:unix-file-kind namestring))
	  (let ((truename (unix:unix-resolve-links
			   (unix:unix-maybe-prepend-current-directory
			    namestring))))
	    (when truename
	      (let ((*ignore-wildcards* t))
		(pathname (unix:unix-simplify-pathname truename)))))))))


;;;; Other random operations.

;;; Rename-File  --  Public
;;;
(defun rename-file (file new-name)
  "Rename File to have the specified New-Name.  If file is a stream open to a
  file, then the associated file is renamed."
  (let* ((original (truename file))
	 (original-namestring (unix-namestring original t))
	 (new-name (merge-pathnames new-name original))
	 (new-namestring (unix-namestring new-name nil)))
    (unless new-namestring
      (error 'simple-file-error
	     :pathname new-name
	     :format-control "~S can't be created."
	     :format-arguments (list new-name)))
    (multiple-value-bind (res error)
			 (unix:unix-rename original-namestring
					   new-namestring)
      (unless res
	(error 'simple-file-error
	       :pathname new-name
	       :format-control "Failed to rename ~A to ~A: ~A"
	       :format-arguments (list original new-name
				       (unix:get-unix-error-msg error))))
      (when (streamp file)
	(file-name file new-namestring))
      (values new-name original (truename new-name)))))

;;; Delete-File  --  Public
;;;
;;;    Delete the file, Man.
;;;
(defun delete-file (file)
  "Delete the specified file."
  (let ((namestring (unix-namestring file t)))
    (when (streamp file)
      ;; Close the file, but don't try to revert or anything.  We want
      ;; to delete it, man!
      (close file))
    (unless namestring
      (error 'simple-file-error
	     :pathname file
	     :format-control "~S doesn't exist."
	     :format-arguments (list file)))

    (multiple-value-bind (res err) (unix:unix-unlink namestring)
      (unless res
	(error 'simple-file-error
	       :pathname namestring
	       :format-control "Could not delete ~A: ~A."
	       :format-arguments (list namestring
				       (unix:get-unix-error-msg err))))))
  t)

;;; Purge-Backup-Files  --  Public
;;;
;;;    Purge old file versions
;;;
(defun purge-backup-files (pathname &optional (keep 0))
  "Delete old versions of files matching the given Pathname,
optionally keeping some of the most recent old versions."
  (declare (type (or pathname string stream) pathname)
	   (type (integer 0 *) keep))
  (let ((hash (make-hash-table :test 'equal)))
    (enumerate-search-list
	(path (make-pathname :version :wild :defaults pathname))
      (clrhash hash)
      (enumerate-matches (name path nil :follow-links nil)
	(let ((dot (position #\. name :from-end t))
	      (len (length name)))
	  (when (and dot
		     (> len (+ dot 3))
		     (char= (char name (1+ dot)) #\~)
		     (char= (char name (1- len)) #\~)
		     (eq (unix:unix-file-kind name) :file))
	    (multiple-value-bind (version next)
		(parse-integer name :start (+ dot 2) :end (1- len)
			       :junk-allowed t)
	      (when (and version (= next (1- len)))
		(push (cons version name)
		      (gethash (subseq name 0 dot) hash '())))))))
      (maphash (lambda (key value)
		 (declare (ignore key))
		 (mapc #'unix:unix-unlink
		       (mapcar #'cdr (nthcdr keep
					     (sort value #'> :key #'car)))))
	       hash))))


;;; User-Homedir-Pathname  --  Public
;;;
;;;    Return Home:, which is set up for us at initialization time.
;;;
(defun user-homedir-pathname (&optional host)
  "Returns the home directory of the logged in user as a pathname.
  This is obtained from the logical name \"home:\"."
  (declare (ignore host))
  #p"home:")

;;; File-Write-Date  --  Public
;;;
(defun file-write-date (file)
  "Return file's creation date, or NIL if it doesn't exist.
 An error of type file-error is signalled if file is a wild pathname"
  (if (wild-pathname-p file)
      (error 'simple-file-error 
	     :pathname file
	     :format-control "Bad place for a wild pathname.")
      (let ((name (unix-namestring file t)))
	(when name
	  (multiple-value-bind
	      (res dev ino mode nlink uid gid rdev size atime mtime)
	      (unix:unix-stat name)
	    (declare (ignore dev ino mode nlink uid gid rdev size atime))
	    (when res
	      (+ unix-to-universal-time mtime)))))))

;;; File-Author  --  Public
;;;
(defun file-author (file)
  "Returns the file author as a string, or nil if the author cannot be
 determined.  Signals an error of type file-error if file doesn't exist,
 or file is a wild pathname."
  (if (wild-pathname-p file)
      (error 'simple-file-error
	     :pathname file
	     :format-control "Bad place for a wild pathname.")
      (let ((name (unix-namestring (pathname file) t)))
	(unless name
	  (error 'simple-file-error
		 :pathname file
		 :format-control "~S doesn't exist."
		 :format-arguments (list file)))
	(multiple-value-bind (winp dev ino mode nlink uid)
			     (unix:unix-stat name)
	  (declare (ignore dev ino mode nlink))
	  (when winp
            (let ((user-info (unix:unix-getpwuid uid)))
              (when user-info
                (unix:user-info-name user-info))))))))


;;;; DIRECTORY.

;;; DIRECTORY  --  public.
;;;
(defun directory (pathname &key (all t) (check-for-subdirs t)
			   (truenamep t) (follow-links t))
  "Returns a list of pathnames, one for each file that matches the given
   pathname.  Supplying :ALL as nil causes this to ignore Unix dot files.  This
   never includes Unix dot and dot-dot in the result.  If :TRUENAMEP is NIL,
   then symbolic links in the result are not expanded, which is not the
   default because TRUENAME does follow links and the result pathnames are
   defined to be the TRUENAME of the pathname (the truename of a link may well
   be in another directory).  If FOLLOW-LINKS is NIL then symbolic links are
   not followed."
  (let ((results nil))
    (enumerate-search-list
	(pathname (merge-pathnames pathname
				   (make-pathname :name :wild
						  :type :wild
						  :version :wild
						  :defaults *default-pathname-defaults*)
				   :wild))
      (enumerate-matches (name pathname nil :follow-links follow-links)
	(when (or all
		  (let ((slash (position #\/ name :from-end t)))
		    (or (null slash)
			(= (1+ slash) (length name))
			(char/= (schar name (1+ slash)) #\.))))
	  (push name results))))
    (let ((*ignore-wildcards* t))
      (mapcar #'(lambda (name)
		  (let ((name (if (and check-for-subdirs
				       (eq (unix:unix-file-kind name)
					   :directory))
				  (concatenate 'string name "/")
				  name)))
		    (if truenamep (truename name) (pathname name))))
	      (sort (delete-duplicates results :test #'string=) #'string<)))))


;;;; Printing directories.

;;; PRINT-DIRECTORY is exported from the EXTENSIONS package.
;;; 
(defun print-directory (pathname &optional stream &key all verbose return-list)
  "Like Directory, but prints a terse, multi-column directory listing
   instead of returning a list of pathnames.  When :all is supplied and
   non-nil, then Unix dot files are included too (as ls -a).  When :verbose
   is supplied and non-nil, then a long listing of miscellaneous
   information is output one file per line."
  (let ((*standard-output* (out-synonym-of stream))
	(pathname pathname))
    (if verbose
	(print-directory-verbose pathname all return-list)
	(print-directory-formatted pathname all return-list))))

(defun print-directory-verbose (pathname all return-list)
  (let ((contents (directory pathname :all all :check-for-subdirs nil
			     :truenamep nil))
	(result nil))
    (format t "Directory of ~A:~%" (namestring pathname))
    (dolist (file contents)
      (let* ((namestring (unix-namestring file))
	     (tail (subseq namestring
			   (1+ (or (position #\/ namestring
					     :from-end t
					     :test #'char=)
				   -1)))))
	(multiple-value-bind 
	    (reslt dev-or-err ino mode nlink uid gid rdev size atime mtime)
	    (unix:unix-stat namestring)
	  (declare (ignore ino gid rdev atime)
		   (fixnum uid mode))
	  (cond (reslt
		 ;;
		 ;; Print characters for file modes.
		 (macrolet ((frob (bit name &optional sbit sname negate)
			      `(if ,(if negate
					`(not (logbitp ,bit mode))
					`(logbitp ,bit mode))
				   ,(if sbit
					`(if (logbitp ,sbit mode)
					     (write-char ,sname)
					     (write-char ,name))
					`(write-char ,name))
				   (write-char #\-))))
		   (frob 15 #\d nil nil t)
		   (frob 8 #\r)
		   (frob 7 #\w)
		   (frob 6 #\x 11 #\s)
		   (frob 5 #\r)
		   (frob 4 #\w)
		   (frob 3 #\x 10 #\s)
		   (frob 2 #\r)
		   (frob 1 #\w)
		   (frob 0 #\x))
		 ;;
		 ;; Print the rest.
		 (multiple-value-bind (sec min hour date month year)
				      (get-decoded-time)
		   (declare (ignore sec min hour date month))
		   (format t "~2D ~8A ~8D ~12A ~A~@[/~]~%"
			   nlink
                           (let ((user-info (unix:unix-getpwuid uid)))
                             (if user-info (unix:user-info-name user-info) uid))
			   size
			   (decode-universal-time-for-files mtime year)
			   tail
			   (= (logand mode unix:s-ifmt) unix:s-ifdir))))
		(t (format t "Couldn't stat ~A -- ~A.~%"
			   tail
			   (unix:get-unix-error-msg dev-or-err))))
	  (when return-list
	    (push (if (= (logand mode unix:s-ifmt) unix:s-ifdir)
		      (pathname (concatenate 'string namestring "/"))
		      file)
		  result)))))
    (nreverse result)))

(defun decode-universal-time-for-files (time current-year)
  (multiple-value-bind (sec min hour day month year)
		       (decode-universal-time (+ time unix-to-universal-time))
    (declare (ignore sec))
    (format nil "~A ~2,' D ~:[ ~D~;~*~2,'0D:~2,'0D~]"
	    (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
		      "Sep" "Oct" "Nov" "Dec")
		   (1- month))
	    day (= current-year year) year hour min)))

(defun print-directory-formatted (pathname all return-list)
  (let ((width (or (line-length *standard-output*) 80))
	(names ())
	(cnt 0)
	(max-len 0)
	(result (directory pathname :all all :truenamep nil)))
    (declare (list names) (fixnum max-len cnt))
    ;;
    ;; Get the data.
    (dolist (file result)
      (let* ((name (unix-namestring file))
	     (length (length name))
	     (end (if (and (plusp length)
			   (char= (schar name (1- length)) #\/))
		      (1- length)
		      length))
	     (slash-name (subseq name
				 (1+ (or (position #\/ name
						   :from-end t
						   :end end
						   :test #'char=)
					 -1))))
	     (len (length slash-name)))
	(declare (simple-string slash-name)
		 (fixnum len))
	(if (> len max-len) (setq max-len len))
	(incf cnt)
	(push slash-name names)))
    (setq names (nreverse names))
    ;;
    ;; Do the output.
    (let* ((col-width (1+ max-len))
	   (cols (max (truncate width col-width) 1))
	   (lines (ceiling cnt cols)))
      (declare (fixnum cols lines))
      (format t "Directory of ~A:~%" (namestring pathname))
      (dotimes (i lines)
	(declare (fixnum i))
	(dotimes (j cols)
	  (declare (fixnum j))
	  (let ((name (nth (+ i (the fixnum (* j lines))) names)))
	    (when name
	      (write-string name)
	      (unless (eql j (1- cols))
		(dotimes (i (- col-width (length (the simple-string name))))
		  (write-char #\space))))))
	(terpri)))
    (when return-list
      result)))


;;;; File completion.

;;; COMPLETE-FILE -- Public
;;;
(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
			       ignore-types)
  (let ((files (directory (complete-file-directory-arg pathname defaults)
			  :check-for-subdirs nil
			  :truenamep nil)))
    (cond ((null files)
	   (values nil nil))
	  ((null (cdr files))
	   (values (merge-pathnames (file-namestring (car files))
				    pathname)
		   t))
	  (t
	   (let ((good-files
		  (delete-if #'(lambda (pathname)
				 (and (simple-string-p
				       (pathname-type pathname))
				      (member (pathname-type pathname)
					      ignore-types
					      :test #'string=)))
			     files)))
	     (cond ((null good-files))
		   ((null (cdr good-files))
		    (return-from complete-file
				 (values (merge-pathnames (file-namestring
							   (car good-files))
							  pathname)
					 t)))
		   (t
		    (setf files good-files)))
	     (let ((common (file-namestring (car files))))
	       (dolist (file (cdr files))
		 (let ((name (file-namestring file)))
		   (dotimes (i (min (length common) (length name))
			       (when (< (length name) (length common))
				 (setf common name)))
		     (unless (char= (schar common i) (schar name i))
		       (setf common (subseq common 0 i))
		       (return)))))
	       (values (merge-pathnames common pathname)
		       nil)))))))

;;; COMPLETE-FILE-DIRECTORY-ARG -- Internal.
;;;
(defun complete-file-directory-arg (pathname defaults)
  (let* ((pathname (merge-pathnames pathname (directory-namestring defaults)))
	 (type (pathname-type pathname)))
    (flet ((append-multi-char-wild (thing)
	     (etypecase thing
	       (null :wild)
	       (pattern
		(make-pattern (append (pattern-pieces thing)
				      (list :multi-char-wild))))
	       (simple-string
		(make-pattern (list thing :multi-char-wild))))))
      (if (or (null type) (eq type :unspecific))
	  ;; There is no type.
	  (make-pathname :defaults pathname
	    :name (append-multi-char-wild (pathname-name pathname))
	    :type :wild)
	  ;; There already is a type, so just extend it.
	  (make-pathname :defaults pathname
	    :name (pathname-name pathname)
	    :type (append-multi-char-wild (pathname-type pathname)))))))

;;; Ambiguous-Files  --  Public
;;;
(defun ambiguous-files (pathname
			&optional (defaults *default-pathname-defaults*))
  "Return a list of all files which are possible completions of Pathname.
   We look in the directory specified by Defaults as well as looking down
   the search list."
  (directory (complete-file-directory-arg pathname defaults)
	     :truenamep nil
	     :check-for-subdirs nil))



;;; File-writable -- exported from extensions.
;;;
;;;   Determines whether the single argument (which should be a pathname)
;;;   can be written by the current task.
;;;
(defun file-writable (name)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise."
  (let ((name (unix-namestring name nil)))
    (cond ((null name)
	   nil)
	  ((unix:unix-file-kind name)
	   (values (unix:unix-access name unix:w_ok)))
	  (t
	   (values
	    (unix:unix-access (subseq name
				      0
				      (or (position #\/ name :from-end t)
					  0))
			      (logior unix:w_ok unix:x_ok)))))))


;;; Pathname-Order  --  Internal
;;;
;;;    Predicate to order pathnames by.  Goes by name.
;;;
(defun pathname-order (x y)
  (let ((xn (%pathname-name x))
	(yn (%pathname-name y)))
    (if (and xn yn)
	(let ((res (string-lessp xn yn)))
	  (cond ((not res) nil)
		((= res (length (the simple-string xn))) t)
		((= res (length (the simple-string yn))) nil)
		(t t)))
	xn)))


;;; Default-Directory  --  Public
;;;
(defun default-directory ()
  "Returns the pathname for the default directory.  This is the place where
  a file will be written if no directory is specified.  This may be changed
  with setf."
  (multiple-value-bind (gr dir-or-error)
		       (unix:unix-current-directory)
    (if gr
	(let ((*ignore-wildcards* t))
	  (values
	   (parse-namestring (concatenate 'simple-string dir-or-error "/")
			     *unix-host*)))
	(error dir-or-error))))

;;; %Set-Default-Directory  --  Internal
;;; 
(defun %set-default-directory (new-val)
  (let ((namestring (unix-namestring new-val t)))
    (unless namestring
      (error 'simple-file-error
             :format-control "~S doesn't exist."
             :format-arguments (list new-val)))
    (multiple-value-bind (gr error)
			 (unix:unix-chdir namestring)
      (if gr
	  (setf (search-list "default:") (default-directory))
	  (error (unix:get-unix-error-msg error))))
    new-val))
;;;
(defsetf default-directory %set-default-directory)

(defun filesys-init ()
  ;; Use :unspecific so we don't create file versions whenever merging
  ;; happens.  If the user wants that, let him change
  ;; *default-pathname-defaults* appropriately.
  (setf *default-pathname-defaults*
	(%make-pathname *unix-host* nil nil nil nil :unspecific))
  (setf (search-list "default:") (default-directory))
  nil)

;;; Ensure-Directories-Exist  --  Public
;;;
(defun ensure-directories-exist (pathspec &key verbose (mode #o777))
  "Tests whether the directories containing the specified file
  actually exist, and attempts to create them if they do not.
  Portable programs should avoid using the :MODE keyword argument."
  (let* ((pathname (pathname pathspec))
	 (pathname (if (logical-pathname-p pathname)
		       (translate-logical-pathname pathname)
		       pathname))
	 (created-p nil))
    (when (wild-pathname-p pathname)
      (error 'simple-file-error
	     :format-control "Bad place for a wild pathname."
	     :pathname pathspec))
    (enumerate-search-list (pathname pathname)
       (let ((dir (pathname-directory pathname)))
	 (loop for i from 1 upto (length dir)
	       do (let ((newpath (make-pathname
				  :host (pathname-host pathname)
				  :device (pathname-device pathname)
				  :directory (subseq dir 0 i))))
		    (tagbody
		     retry
		       (restart-case
			   (unless (probe-file newpath)
			     (let ((namestring (namestring newpath)))
			       (when verbose
				 (format *standard-output* "~&Creating directory: ~A~%"
					 namestring))
			       (unix:unix-mkdir namestring mode)
			       (unless (probe-file namestring)
				 (error 'simple-file-error
					:pathname pathspec
					:format-control "Can't create directory ~A."
					:format-arguments (list namestring)))
			       (setf created-p t)))
			 (retry () :report "Try to create the directory again"
				(go retry))))))
	 ;; Only the first path in a search-list is considered.
	 (return (values pathname created-p))))))
