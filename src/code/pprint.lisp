;;; -*- Package: PRETTY-PRINT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/pprint.lisp,v 1.64 2006/02/17 15:45:40 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; CMU Common Lisp pretty printer.
;;; Written by William Lott.  Algorithm stolen from Richard Waters' XP.
;;;
(in-package "PRETTY-PRINT")
(use-package "EXT")
(use-package "KERNEL")

(export '(pretty-stream pretty-stream-p))

(in-package "LISP")
(export '(pprint-logical-block pprint-pop pprint-exit-if-list-exhausted
	  pprint-newline pprint-indent pprint-tab
	  pprint-fill pprint-linear pprint-tabular
	  copy-pprint-dispatch pprint-dispatch set-pprint-dispatch))
(in-package "PP")


;;;; Pretty streams

;;; There are three different units for measuring character positions:
;;;  COLUMN - offset (if characters) from the start of the current line.
;;;  INDEX - index into the output buffer.
;;;  POSN - some position in the stream of characters cycling through
;;;             the output buffer.
;;; 
(deftype column ()
  '(and fixnum unsigned-byte))
;;; The INDEX type is picked up from the kernel package.
(deftype posn ()
  'fixnum)

(defconstant initial-buffer-size 128)

(defconstant default-line-length 80)

(defstruct (pretty-stream
	    (:include sys:lisp-stream
		      (:out #'pretty-out)
		      (:sout #'pretty-sout)
		      (:misc #'pretty-misc))
	    (:constructor make-pretty-stream (target))
	    (:print-function %print-pretty-stream))
  ;;
  ;; Where the output is going to finally go.
  ;; 
  (target (required-argument) :type stream)
  ;;
  ;; Line length we should format to.  Cached here so we don't have to keep
  ;; extracting it from the target stream.
  (line-length (or *print-right-margin*
		   (lisp::line-length target)
		   default-line-length)
	       :type column)
  ;;
  ;; A simple string holding all the text that has been output but not yet
  ;; printed.
  (buffer (make-string initial-buffer-size) :type simple-string)
  ;;
  ;; The index into BUFFER where more text should be put.
  (buffer-fill-pointer 0 :type index)
  ;;
  ;; Whenever we output stuff from the buffer, we shift the remaining noise
  ;; over.  This makes it difficult to keep references to locations in
  ;; the buffer.  Therefore, we have to keep track of the total amount of
  ;; stuff that has been shifted out of the buffer.
  (buffer-offset 0 :type posn)
  ;;
  ;; The column the first character in the buffer will appear in.  Normally
  ;; zero, but if we end up with a very long line with no breaks in it we
  ;; might have to output part of it.  Then this will no longer be zero.
  (buffer-start-column (or (lisp::charpos target) 0) :type column)
  ;;
  ;; The line number we are currently on.  Used for *print-lines* abrevs and
  ;; to tell when sections have been split across multiple lines.
  (line-number 0 :type index)
  ;;
  ;; Stack of logical blocks in effect at the buffer start.
  (blocks (list (make-logical-block)) :type list)
  ;;
  ;; Buffer holding the per-line prefix active at the buffer start.
  ;; Indentation is included in this.  The length of this is stored
  ;; in the logical block stack.
  (prefix (make-string initial-buffer-size) :type simple-string)
  ;;
  ;; Buffer holding the total remaining suffix active at the buffer start.
  ;; The characters are right-justified in the buffer to make it easier
  ;; to output the buffer.  The length is stored in the logical block
  ;; stack.
  (suffix (make-string initial-buffer-size) :type simple-string)
  ;;
  ;; Queue of pending operations.  When empty, HEAD=TAIL=NIL.  Otherwise,
  ;; TAIL holds the first (oldest) cons and HEAD holds the last (newest)
  ;; cons.  Adding things to the queue is basically (setf (cdr head) (list
  ;; new)) and removing them is basically (pop tail) [except that care must
  ;; be taken to handle the empty queue case correctly.]
  (queue-tail nil :type list)
  (queue-head nil :type list)
  ;;
  ;; Block-start queue entries in effect at the queue head.
  (pending-blocks nil :type list)
  ;; 
  ;; Queue of annotations to the buffer
  (annotations-tail nil :type list)
  (annotations-head nil :type list))

(defun %print-pretty-stream (pstream stream depth)
  (declare (ignore depth))
  #+nil
  (print-unreadable-object (pstream stream :type t :identity t))
  (format stream "#<pretty stream {~8,'0X}>"
	  (kernel:get-lisp-obj-address pstream)))


(declaim (inline index-posn posn-index posn-column))
(defun index-posn (index stream)
  (declare (type index index) (type pretty-stream stream)
	   (values posn))
  (+ index (pretty-stream-buffer-offset stream)))
(defun posn-index (posn stream)
  (declare (type posn posn) (type pretty-stream stream)
	   (values index))
  (- posn (pretty-stream-buffer-offset stream)))
(defun posn-column (posn stream)
  (declare (type posn posn) (type pretty-stream stream)
	   (values posn))
  (index-column (posn-index posn stream) stream))


;;;; Stream interface routines.

(defun pretty-out (stream char)
  (declare (type pretty-stream stream)
	   (type base-char char))
  (cond ((char= char #\newline)
	 (enqueue-newline stream :literal))
	(t
	 (assure-space-in-buffer stream 1)
	 (let ((fill-pointer (pretty-stream-buffer-fill-pointer stream)))
	   (setf (schar (pretty-stream-buffer stream) fill-pointer) char)
	   (setf (pretty-stream-buffer-fill-pointer stream)
		 (1+ fill-pointer))))))

(defun pretty-sout (stream string start end)
  (declare (type pretty-stream stream)
	   (type simple-string string)
	   (type index start)
	   (type (or index null) end))
  (let ((end (or end (length string))))
    (unless (= start end)
      (let ((newline (position #\newline string :start start :end end)))
	(cond
	 (newline
	  (pretty-sout stream string start newline)
	  (enqueue-newline stream :literal)
	  (pretty-sout stream string (1+ newline) end))
	 (t
	  (let ((chars (- end start)))
	    (loop
	      (let* ((available (assure-space-in-buffer stream chars))
		     (count (min available chars))
		     (fill-pointer (pretty-stream-buffer-fill-pointer stream))
		     (new-fill-ptr (+ fill-pointer count)))
		(replace (pretty-stream-buffer stream)
			 string
			 :start1 fill-pointer :end1 new-fill-ptr
			 :start2 start)
		(setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
		(decf chars count)
		(when (zerop count)
		  (return))
		(incf start count))))))))))

(defun pretty-misc (stream op &optional arg1 arg2)
  (declare (ignore stream op arg1 arg2)))



;;;; Logical blocks.

(defstruct logical-block
  ;;
  ;; The column this logical block started in.
  (start-column 0 :type column)
  ;;
  ;; The column the current section started in.
  (section-column 0 :type column)
  ;;
  ;; The length of the per-line prefix.  We can't move the indentation
  ;; left of this.
  (per-line-prefix-end 0 :type index)
  ;;
  ;; The overall length of the prefix, including any indentation.
  (prefix-length 0 :type index)
  ;;
  ;; The overall length of the suffix.
  (suffix-length 0 :type index)
  ;; 
  ;; The line number 
  (section-start-line 0 :type index))

(defun really-start-logical-block (stream column prefix suffix)
  (let* ((blocks (pretty-stream-blocks stream))
	 (prev-block (car blocks))
	 (per-line-end (logical-block-per-line-prefix-end prev-block))
	 (prefix-length (logical-block-prefix-length prev-block))
	 (suffix-length (logical-block-suffix-length prev-block))
	 (block (make-logical-block
		 :start-column column
		 :section-column column
		 :per-line-prefix-end per-line-end
		 :prefix-length prefix-length
		 :suffix-length suffix-length
		 :section-start-line (pretty-stream-line-number stream))))
    (setf (pretty-stream-blocks stream) (cons block blocks))
    (set-indentation stream column)
    (when prefix
      (setf (logical-block-per-line-prefix-end block) column)
      (replace (pretty-stream-prefix stream) prefix
	       :start1 (- column (length prefix)) :end1 column))
    (when suffix
      (let* ((total-suffix (pretty-stream-suffix stream))
	     (total-suffix-len (length total-suffix))
	     (additional (length suffix))
	     (new-suffix-len (+ suffix-length additional)))
	(when (> new-suffix-len total-suffix-len)
	  (let ((new-total-suffix-len
		 (max (* total-suffix-len 2)
		      (+ suffix-length
			 (floor (* additional 5) 4)))))
	    (setf total-suffix
		  (replace (make-string new-total-suffix-len) total-suffix
			   :start1 (- new-total-suffix-len suffix-length)
			   :start2 (- total-suffix-len suffix-length)))
	    (setf total-suffix-len new-total-suffix-len)
	    (setf (pretty-stream-suffix stream) total-suffix)))
	(replace total-suffix suffix
		 :start1 (- total-suffix-len new-suffix-len)
		 :end1 (- total-suffix-len suffix-length))
	(setf (logical-block-suffix-length block) new-suffix-len))))
  nil)

(defun set-indentation (stream column)
  (let* ((prefix (pretty-stream-prefix stream))
	 (prefix-len (length prefix))
	 (block (car (pretty-stream-blocks stream)))
	 (current (logical-block-prefix-length block))
	 (minimum (logical-block-per-line-prefix-end block))
	 (column (max minimum column)))
    (when (> column prefix-len)
      (setf prefix
	    (replace (make-string (max (* prefix-len 2)
				       (+ prefix-len
					  (floor (* (- column prefix-len) 5)
						 4))))
		     prefix
		     :end1 current))
      (setf (pretty-stream-prefix stream) prefix))
    (when (> column current)
      (fill prefix #\space :start current :end column))
    (setf (logical-block-prefix-length block) column)))

(defun really-end-logical-block (stream)
  (let* ((old (pop (pretty-stream-blocks stream)))
	 (old-indent (logical-block-prefix-length old))
	 (new (car (pretty-stream-blocks stream)))
	 (new-indent (logical-block-prefix-length new)))
    (when (> new-indent old-indent)
      (fill (pretty-stream-prefix stream) #\space
	    :start old-indent :end new-indent)))
  nil)



;;;; The pending operation queue.

(defstruct queued-op
  (posn 0 :type posn))

(defmacro enqueue (stream type &rest args)
  (let ((constructor (intern (concatenate 'string
					  "MAKE-"
					  (symbol-name type)))))
    (once-only ((stream stream)
		(entry `(,constructor :posn
				      (index-posn
				       (pretty-stream-buffer-fill-pointer
					,stream)
				       ,stream)
				      ,@args))
		(op `(list ,entry))
		(head `(pretty-stream-queue-head ,stream)))
      `(progn
	 (if ,head
	     (setf (cdr ,head) ,op)
	     (setf (pretty-stream-queue-tail ,stream) ,op))
	 (setf (pretty-stream-queue-head ,stream) ,op)
	 ,entry))))

(defstruct (section-start
	    (:include queued-op))
  (depth 0 :type index)
  (section-end nil :type (or null newline block-end)))

(defstruct (newline
	    (:include section-start))
  (kind (required-argument)
	:type (member :linear :fill :miser :literal :mandatory)))

(defun enqueue-newline (stream kind)
  (let* ((depth (length (pretty-stream-pending-blocks stream)))
	 (newline (enqueue stream newline :kind kind :depth depth)))
    (dolist (entry (pretty-stream-queue-tail stream))
      (when (and (not (eq newline entry))
		 (section-start-p entry)
		 (null (section-start-section-end entry))
		 (<= depth (section-start-depth entry)))
	(setf (section-start-section-end entry) newline))))
  (maybe-output stream (or (eq kind :literal) (eq kind :mandatory))))

(defstruct (indentation
	    (:include queued-op))
  (kind (required-argument) :type (member :block :current))
  (amount 0 :type fixnum))

(defun enqueue-indent (stream kind amount)
  (enqueue stream indentation :kind kind :amount amount))

(defstruct (block-start
	    (:include section-start))
  (block-end nil :type (or null block-end))
  (prefix nil :type (or null simple-string))
  (suffix nil :type (or null simple-string)))

(defun start-logical-block (stream prefix per-line-p suffix
			    &optional prefix-p per-line-prefix-p suffix-p)
  (declare (type (or null string) prefix suffix))

  ;; If a prefix (or per-line-prefix) is explicitly given, it must be
  ;; a string.
  (when (and (or prefix-p per-line-prefix-p) (not (stringp prefix)))
    (error 'type-error
	   :datum prefix
	   :expected-type 'string))
  ;; Likewise for a suffix
  (when (and suffix-p (not (stringp suffix)))
    (error 'type-error
	   :datum suffix
	   :expected-type 'string))

  (let ((p (if prefix (coerce prefix 'simple-string) prefix)))
    (when prefix
      (pretty-sout stream p 0 (length p)))
    (let* ((pending-blocks (pretty-stream-pending-blocks stream))
	   (start (enqueue stream block-start
			   :prefix (and per-line-p p)
			   :suffix (coerce suffix 'simple-string)
			   :depth (length pending-blocks))))
      (setf (pretty-stream-pending-blocks stream)
	    (cons start pending-blocks)))))

(defstruct (block-end
	    (:include queued-op))
  (suffix nil :type (or null simple-string)))

(defun end-logical-block (stream)
  (let* ((start (pop (pretty-stream-pending-blocks stream)))
	 (suffix (block-start-suffix start))
	 (end (enqueue stream block-end :suffix suffix)))
    (when suffix
      (pretty-sout stream suffix 0 (length suffix)))
    (setf (block-start-block-end start) end)))

(defstruct (tab
	    (:include queued-op))
  (sectionp nil :type (member t nil))
  (relativep nil :type (member t nil))
  (colnum 0 :type column)
  (colinc 0 :type column))

(defun enqueue-tab (stream kind colnum colinc)
  (multiple-value-bind
      (sectionp relativep)
      (ecase kind
	(:line (values nil nil))
	(:line-relative (values nil t))
	(:section (values t nil))
	(:section-relative (values t t)))
    (enqueue stream tab :sectionp sectionp :relativep relativep
	     :colnum colnum :colinc colinc)))


;;;; Tab support.

(defun compute-tab-size (tab section-start column)
  (let ((origin (if (tab-sectionp tab) section-start 0))
	(colnum (tab-colnum tab))
	(colinc (tab-colinc tab)))
    (cond ((tab-relativep tab)
	   (unless (<= colinc 1)
	     (let ((newposn (+ (- column origin) colnum)))
	       (let ((rem (rem newposn colinc)))
		 (unless (zerop rem)
		   (incf colnum (- colinc rem))))))
	   colnum)
	  ((< column (+ colnum origin))
	   (- (+ colnum origin) column))
	  (t
	   (if (zerop colinc)
	       0
	       (- colinc
		  (rem (- column origin colnum) colinc)))))))

(defun index-column (index stream)
  (let ((column (pretty-stream-buffer-start-column stream))
	(section-start (logical-block-section-column
			(first (pretty-stream-blocks stream))))
	(end-posn (index-posn index stream)))
    (dolist (op (pretty-stream-queue-tail stream))
      (when (>= (queued-op-posn op) end-posn)
	(return))
      (typecase op
	(tab
	 (incf column
	       (compute-tab-size op
				 section-start
				 (+ column
				    (posn-index (tab-posn op)
						    stream)))))
	((or newline block-start)
	 (setf section-start
	       (+ column (posn-index (queued-op-posn op)
					 stream))))))
    (+ column index)))

(defun expand-tabs (stream through)
  (let ((insertions nil)
	(additional 0)
	(column (pretty-stream-buffer-start-column stream))
	(section-start (logical-block-section-column
			(first (pretty-stream-blocks stream)))))
    (dolist (op (pretty-stream-queue-tail stream))
      (typecase op
	(tab
	 (let* ((index (posn-index (tab-posn op) stream))
		(tabsize (compute-tab-size op
					   section-start
					   (+ column index))))
	   (unless (zerop tabsize)
	     (push (cons index tabsize) insertions)
	     (incf additional tabsize)
	     (incf column tabsize))))
	((or newline block-start)
	 (setf section-start
	       (+ column (posn-index (queued-op-posn op) stream)))))
      (when (eq op through)
	(return)))
    (when insertions
      (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
	     (new-fill-ptr (+ fill-ptr additional))
	     (buffer (pretty-stream-buffer stream))
	     (new-buffer buffer)
	     (length (length buffer))
	     (end fill-ptr))
	(when (> new-fill-ptr length)
	  (let ((new-length (max (* length 2)
				 (+ fill-ptr
				    (floor (* additional 5) 4)))))
	    (setf new-buffer (make-string new-length))
	    (setf (pretty-stream-buffer stream) new-buffer)))
	(setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
	(decf (pretty-stream-buffer-offset stream) additional)
	(dolist (insertion insertions)
	  (let* ((srcpos (car insertion))
		 (amount (cdr insertion))
		 (dstpos (+ srcpos additional)))
	    (replace new-buffer buffer :start1 dstpos :start2 srcpos :end2 end)
	    (fill new-buffer #\space :start (- dstpos amount) :end dstpos)
	    (decf additional amount)
	    (setf end srcpos)))
	(unless (eq new-buffer buffer)
	  (replace new-buffer buffer :end1 end :end2 end))))))


;;;; Annotations support.

(defstruct (annotation (:include queued-op))
  (handler (constantly nil) :type function)
  (record))

(defun enqueue-annotation (stream handler record)
  "Insert an annotation into the pretty-printing stream STREAM.
HANDLER is a function, and RECORD is an arbitrary datum.  The
pretty-printing stream conceptionally queues annotations in sequence
with the characters that are printed to the stream, until the stream
has decided on the concrete layout.  When the characters are forwarded
to the target stream, annotations are invoked at the right position.
An annotation is invoked by calling the function HANDLER with the
three arguments RECORD, TARGET-STREAM, and TRUNCATEP.  The argument
TRUNCATEP is true if the text surrounding the annotation is suppressed
due to line abbreviation (see *PRINT-LINES*).
If STREAM is not a pretty-printing stream, simply call HANDLER
with the arguments RECORD, STREAM and nil."
  (enqueue stream annotation :handler handler
	   :record record))

(defun re-enqueue-annotation (stream annotation)
  "Insert ANNOTATION into the queue of annotations in STREAM."
  (let* ((annotation-cons (list annotation))
	 (head (pretty-stream-annotations-head stream)))
    (if head
	(setf (cdr head) annotation-cons)
	(setf (pretty-stream-annotations-tail stream) annotation-cons))
    (setf (pretty-stream-annotations-head stream) annotation-cons)))

(defun re-enqueue-annotations (stream end)
  "Insert all annotations in STREAM from the queue of pending
operations into the queue of annotations.  When END is non-nil, 
stop before reaching the queued-op END."
  #-(and)
  (loop for tail = (pretty-stream-queue-tail stream) then (cdr tail)
     while (and tail (not (eql (car tail) end)))
     when (annotation-p (car tail)) 
     do (re-enqueue-annotation stream (car tail)))
  #+(and)
  (do ((tail (pretty-stream-queue-tail stream) (cdr tail)))
      ((not (and tail (not (eql (car tail) end)))))
    (when (annotation-p (car tail))
      (re-enqueue-annotation stream (car tail)))))

(defun dequeue-annotation (stream &key end-posn)
  "Dequeue the next annotation from the queue of annotations of STREAM
and return it.  Return nil if there are no more annotations.  When
:END-POSN is given and the next annotation has a posn greater than
this, also return nil."
  (let ((next-annotation (car (pretty-stream-annotations-tail stream))))
    (when next-annotation
      (when (or (not end-posn)
		(<= (annotation-posn next-annotation) end-posn))
	(pop (pretty-stream-annotations-tail stream))
	(unless (pretty-stream-annotations-tail stream)
	  (setf (pretty-stream-annotations-head stream) nil))
	next-annotation))))

(defun invoke-annotation (stream annotation truncatep)
  (let ((target (pretty-stream-target stream)))
    (funcall (annotation-handler annotation)
	     (annotation-record annotation)
	     target
	     truncatep)))

(defun output-buffer-with-annotations (stream end)
  "Output the buffer of STREAM up to (excluding) the buffer index END.
When annotations are present, invoke them at the right positions."
  (let ((target (pretty-stream-target stream))
	(buffer (pretty-stream-buffer stream))
	(end-posn (index-posn end stream))
	(start 0))
    #-(and)
    (loop
       for annotation = (dequeue-annotation stream :end-posn end-posn)
       while annotation
       do
	 (let ((annotation-index (posn-index (annotation-posn annotation)
					     stream)))
	   (write-string buffer target :start start 
			 :end annotation-index)
	   (invoke-annotation stream annotation nil)
	   (setf start annotation-index)))
    #+(and)
    (do ((annotation (dequeue-annotation stream :end-posn end-posn)
		     (dequeue-annotation stream :end-posn end-posn)))
	((not annotation))
      (let ((annotation-index (posn-index (annotation-posn annotation)
					     stream)))
	   (write-string buffer target :start start 
			 :end annotation-index)
	   (invoke-annotation stream annotation nil)
	   (setf start annotation-index)))
    (write-string buffer target :start start :end end)))

(defun flush-annotations (stream end truncatep)
  "Invoke all annotations in STREAM up to (including) the buffer index END."
  (let ((end-posn (index-posn end stream)))
    #-(and)
    (loop
       for annotation = (dequeue-annotation stream :end-posn end-posn)
       while annotation
       do (invoke-annotation stream annotation truncatep))
    #+(and)
    (do ((annotation (dequeue-annotation stream :end-posn end-posn)
		     (dequeue-annotation stream :end-posn end-posn)))
	((not annotation))
      (invoke-annotation stream annotation truncatep))
    ))


;;;; Stuff to do the actual outputting.

(defun assure-space-in-buffer (stream want)
  (declare (type pretty-stream stream)
	   (type index want))
  (let* ((buffer (pretty-stream-buffer stream))
	 (length (length buffer))
	 (fill-ptr (pretty-stream-buffer-fill-pointer stream))
	 (available (- length fill-ptr)))
    (cond ((plusp available)
	   available)
	  ((> fill-ptr (pretty-stream-line-length stream))
	   (unless (maybe-output stream nil)
	     (output-partial-line stream))
	   (assure-space-in-buffer stream want))
	  (t
	   (let* ((new-length (max (* length 2)
				   (+ length
				      (floor (* want 5) 4))))
		  (new-buffer (make-string new-length)))
	     (setf (pretty-stream-buffer stream) new-buffer)
	     (replace new-buffer buffer :end1 fill-ptr)
	     (- new-length fill-ptr))))))

(defun maybe-output (stream force-newlines-p)
  (declare (type pretty-stream stream))
  (let ((tail (pretty-stream-queue-tail stream))
	(output-anything nil))
    (loop
      (unless tail
	(setf (pretty-stream-queue-head stream) nil)
	(return))
      (let ((next (pop tail)))
	(etypecase next
	  (newline
	   (when (ecase (newline-kind next)
		   ((:literal :mandatory :linear) t)
		   (:miser (misering-p stream))
		   (:fill
		    (or (misering-p stream)
			(> (pretty-stream-line-number stream)
			   (logical-block-section-start-line
			    (first (pretty-stream-blocks stream))))
			(ecase (fits-on-line-p stream
					       (newline-section-end next)
					       force-newlines-p)
			  ((t) nil)
			  ((nil) t)
			  (:dont-know
			   (return))))))
	     (setf output-anything t)
	     (output-line stream next)))
	  (indentation
	   (unless (misering-p stream)
	     (set-indentation stream
			      (+ (ecase (indentation-kind next)
				   (:block
				    (logical-block-start-column
				     (car (pretty-stream-blocks stream))))
				   (:current
				    (posn-column
				     (indentation-posn next)
				     stream)))
				 (indentation-amount next)))))
	  (block-start
	   (ecase (fits-on-line-p stream (block-start-section-end next)
				  force-newlines-p)
	     ((t)
	      ;; Just nuke the whole logical block and make it look like one
	      ;; nice long literal.  (But don't nuke annotations.)
	      (let ((end (block-start-block-end next)))
		(expand-tabs stream end)
		(re-enqueue-annotations stream end)
		(setf tail (cdr (member end tail)))))
	     ((nil)
	      (really-start-logical-block
	       stream
	       (posn-column (block-start-posn next) stream)
	       (block-start-prefix next)
	       (block-start-suffix next)))
	     (:dont-know
	      (return))))
	  (block-end
	   (really-end-logical-block stream))
	  (tab
	   (expand-tabs stream next))
	  (annotation
	   (re-enqueue-annotation stream next))))
      (setf (pretty-stream-queue-tail stream) tail))
    output-anything))

(defun misering-p (stream)
  (declare (type pretty-stream stream))
  (and *print-miser-width*
       (<= (- (pretty-stream-line-length stream)
	      (logical-block-start-column (car (pretty-stream-blocks stream))))
	   *print-miser-width*)))

(defun fits-on-line-p (stream until force-newlines-p)
  (let ((available (pretty-stream-line-length stream)))
    (when (and (not *print-readably*) *print-lines*
	       (= *print-lines* (pretty-stream-line-number stream)))
      (decf available 3) ; for the `` ..''
      (decf available (logical-block-suffix-length
		       (car (pretty-stream-blocks stream)))))
    (cond (until
	   (<= (posn-column (queued-op-posn until) stream) available))
	  (force-newlines-p nil)
	  ((> (index-column (pretty-stream-buffer-fill-pointer stream) stream)
	      available)
	   nil)
	  (t
	   :dont-know))))

(defun output-line (stream until)
  (declare (type pretty-stream stream)
	   (type newline until))
  (let* ((target (pretty-stream-target stream))
	 (buffer (pretty-stream-buffer stream))
	 (kind (newline-kind until))
	 (literal-p (eq kind :literal))
	 (amount-to-consume (posn-index (newline-posn until) stream))
	 (amount-to-print
	  (if literal-p
	      amount-to-consume
	      (let ((last-non-blank
		     (position #\space buffer :end amount-to-consume
			       :from-end t :test #'char/=)))
		(if last-non-blank
		    (1+ last-non-blank)
		    0)))))
    (output-buffer-with-annotations stream amount-to-print)
    (flush-annotations stream amount-to-consume nil)
    (let ((line-number (pretty-stream-line-number stream)))
      (incf line-number)
      (when (and (not *print-readably*)
		 *print-lines* (>= line-number *print-lines*))
	(write-string " .." target)
	(flush-annotations stream 
			   (pretty-stream-buffer-fill-pointer stream)
			   t)
	(let ((suffix-length (logical-block-suffix-length
			      (car (pretty-stream-blocks stream)))))
	  (unless (zerop suffix-length)
	    (let* ((suffix (pretty-stream-suffix stream))
		   (len (length suffix)))
	      (write-string suffix target
			    :start (- len suffix-length)
			    :end len))))
	(throw 'line-limit-abbreviation-happened t))
      (setf (pretty-stream-line-number stream) line-number)
      (write-char #\newline target)
      (setf (pretty-stream-buffer-start-column stream) 0)
      (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
	     (block (first (pretty-stream-blocks stream)))
	     (prefix-len
	      (if literal-p
		  (logical-block-per-line-prefix-end block)
		  (logical-block-prefix-length block)))
	     (shift (- amount-to-consume prefix-len))
	     (new-fill-ptr (- fill-ptr shift))
	     (new-buffer buffer)
	     (buffer-length (length buffer)))
	(when (> new-fill-ptr buffer-length)
	  (setf new-buffer
		(make-string (max (* buffer-length 2)
				  (+ buffer-length
				     (floor (* (- new-fill-ptr buffer-length)
					       5)
					    4)))))
	  (setf (pretty-stream-buffer stream) new-buffer))
	(replace new-buffer buffer
		 :start1 prefix-len :start2 amount-to-consume :end2 fill-ptr)
	(replace new-buffer (pretty-stream-prefix stream)
		 :end1 prefix-len)
	(setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
	(incf (pretty-stream-buffer-offset stream) shift)
	(unless literal-p
	  (setf (logical-block-section-column block) prefix-len)
	  (setf (logical-block-section-start-line block) line-number))))))

(defun output-partial-line (stream)
  (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
	 (tail (pretty-stream-queue-tail stream))
	 (count
	  (if tail
	      (posn-index (queued-op-posn (car tail)) stream)
	      fill-ptr))
	 (new-fill-ptr (- fill-ptr count))
	 (buffer (pretty-stream-buffer stream)))
    (when (zerop count)
      (error "Output-partial-line called when nothing can be output."))
    (output-buffer-with-annotations stream count)
    (incf (pretty-stream-buffer-start-column stream) count)
    (replace buffer buffer :end1 new-fill-ptr :start2 count :end2 fill-ptr)
    (setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
    (incf (pretty-stream-buffer-offset stream) count)))

(defun force-pretty-output (stream)
  (maybe-output stream nil)
  (expand-tabs stream nil)
  (re-enqueue-annotations stream nil)
  (output-buffer-with-annotations stream 
				  (pretty-stream-buffer-fill-pointer stream)))


;;;; Utilities.

;;; WITH-PRETTY-STREAM -- internal.
;;;
(defmacro with-pretty-stream
	  ((stream-var &optional (stream-expression stream-var)) &body body)
  (let ((flet-name (gensym "WITH-PRETTY-STREAM-")))
    `(flet ((,flet-name (,stream-var)
	      ,@body))
       (let ((stream ,stream-expression))
	 (if (pretty-stream-p stream)
	     (,flet-name stream)
	     (catch 'line-limit-abbreviation-happened
	       (let ((stream (make-pretty-stream stream)))
		 (,flet-name stream)
		 (force-pretty-output stream)))))
       nil)))


;;;; User interface to the pretty printer.

(defmacro pprint-logical-block
	  ((stream-symbol object &key (prefix "" prefix-p) (per-line-prefix nil per-line-p) (suffix "" suffix-p))
	   &body body)
  "Group some output into a logical block.  STREAM-SYMBOL should be either a
   stream, T (for *TERMINAL-IO*), or NIL (for *STANDARD-OUTPUT*).  The printer
   control variable *PRINT-LEVEL* is automatically handled."
  (when (and prefix-p per-line-prefix)
    (error "Cannot specify both a prefix and a per-line-prefix."))
  (multiple-value-bind
      (stream-var stream-expression)
      (case stream-symbol
	((nil)
	 (values '*standard-output* '*standard-output*))
	((t)
	 (values '*terminal-io* '*terminal-io*))
	(t
	 (values stream-symbol
		 (once-only ((stream stream-symbol))
		   `(case ,stream
		      ((nil) *standard-output*)
		      ((t) *terminal-io*)
		      (t ,stream))))))
    (let* ((object-var (if object (gensym) nil))
	   (block-name (gensym "PPRINT-LOGICAL-BLOCK-"))
	   (count-name (gensym "PPRINT-LOGICAL-BLOCK-LENGTH-"))
	   (pp-pop-name (gensym "PPRINT-POP-"))
	   (body
	    `(descend-into (,stream-var)
	       (let ((,count-name 0))
		 (declare (type index ,count-name) (ignorable ,count-name))
		 (start-logical-block ,stream-var ,(if prefix-p prefix per-line-prefix)
				      ,(if per-line-prefix t nil) ,suffix
				      ,prefix-p ,per-line-p ,suffix-p)
		 (block ,block-name
		   (flet ((,pp-pop-name ()
			    ,@(when object
				`((unless (listp ,object-var)
				    (write-string ". " ,stream-var)
				    (output-object ,object-var ,stream-var)
				    (return-from ,block-name nil))))
			    (when (and (not *print-readably*)
				       (eql ,count-name *print-length*))
			      (write-string "..." ,stream-var)
			      (return-from ,block-name nil))
			    ,@(when object
				`((when (and ,object-var
					     (plusp ,count-name)
					     (check-for-circularity
					      ,object-var
					      nil
					      :logical-block))
				    (write-string ". " ,stream-var)
				    (output-object ,object-var ,stream-var)
				    (return-from ,block-name nil))))
			    (incf ,count-name)
			    ,@(if object
				  `((pop ,object-var))
				  `(nil))))
		     (declare (ignorable #',pp-pop-name))
		     (unwind-protect
			  (macrolet ((pprint-pop ()
				       '(,pp-pop-name))
				     (pprint-exit-if-list-exhausted ()
				       ,(if object
					    `'(when (null ,object-var)
					       (return-from ,block-name nil))
					    `'(return-from ,block-name nil))))
			    ,@body)
		       (end-logical-block ,stream-var))))
		 ))))
      (when object
	(setf body
	      `(let ((,object-var ,object))
		 (if (listp ,object-var)
		     (with-circularity-detection (,object-var ,stream-var)
		       ,body)
		     (output-object ,object-var ,stream-var)))))
      `(with-pretty-stream (,stream-var ,stream-expression)
	 ,body))))

(defmacro pprint-exit-if-list-exhausted ()
  "Cause the closest enclosing use of PPRINT-LOGICAL-BLOCK to return
   if it's list argument is exhausted.  Can only be used inside
   PPRINT-LOGICAL-BLOCK, and only when the LIST argument to
   PPRINT-LOGICAL-BLOCK is supplied."
  (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside ~
	  PPRINT-LOGICAL-BLOCK."))

(defmacro pprint-pop ()
  "Return the next element from LIST argument to the closest enclosing
   use of PPRINT-LOGICAL-BLOCK, automatically handling *PRINT-LENGTH*
   and *PRINT-CIRCLE*.  Can only be used inside PPRINT-LOGICAL-BLOCK.
   If the LIST argument to PPRINT-LOGICAL-BLOCK was NIL, then nothing
   is poped, but the *PRINT-LENGTH* testing still happens."
  (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK."))
  
(defun pprint-newline (kind &optional stream)
  "Output a conditional newline to STREAM (which defaults to
   *STANDARD-OUTPUT*) if it is a pretty-printing stream, and do
   nothing if not.  KIND can be one of:
     :LINEAR - A line break is inserted if and only if the immediatly
        containing section cannot be printed on one line.
     :MISER - Same as LINEAR, but only if ``miser-style'' is in effect.
        (See *PRINT-MISER-WIDTH*.)
     :FILL - A line break is inserted if and only if either:
       (a) the following section cannot be printed on the end of the
           current line,
       (b) the preceding section was not printed on a single line, or
       (c) the immediately containing section cannot be printed on one
           line and miser-style is in effect.
     :MANDATORY - A line break is always inserted.
   When a line break is inserted by any type of conditional newline, any
   blanks that immediately precede the conditional newline are ommitted
   from the output and indentation is introduced at the beginning of the
   next line.  (See PPRINT-INDENT.)"
  (declare (type (member :linear :miser :fill :mandatory) kind)
	   (type (or stream (member t nil)) stream)
	   (values null))
  (let ((stream (case stream
		  ((t) *terminal-io*)
		  ((nil) *standard-output*)
		  (t stream))))
    (when (and (pretty-stream-p stream) *print-pretty*)
      (enqueue-newline stream kind)))
  nil)

(defun pprint-indent (relative-to n &optional stream)
  "Specify the indentation to use in the current logical block if STREAM
   (which defaults to *STANDARD-OUTPUT*) is a pretty-printing stream
   and do nothing if not.  (See PPRINT-LOGICAL-BLOCK.)  N is the indention
   to use (in ems, the width of an ``m'') and RELATIVE-TO can be either:
     :BLOCK - Indent relative to the column the current logical block
        started on.
     :CURRENT - Indent relative to the current column.
   The new indention value does not take effect until the following line
   break.  The indention value is silently truncated to an integer."
  (declare (type (member :block :current) relative-to)
	   (type real n)
	   (type (or stream (member t nil)) stream)
	   (values null))
  (let ((stream (case stream
		  ((t) *terminal-io*)
		  ((nil) *standard-output*)
		  (t stream))))
    (when (and (pretty-stream-p stream) *print-pretty*)
      ;; CMUCL doesn't really support non-fixed-width characters, so
      ;; we silently truncate the indentation to an integer.
      (enqueue-indent stream relative-to (truncate n))))
  nil)

(defun pprint-tab (kind colnum colinc &optional stream)
  "If STREAM (which defaults to *STANDARD-OUTPUT*) is a pretty-printing
   stream, perform tabbing based on KIND, otherwise do nothing.  KIND can
   be one of:
     :LINE - Tab to column COLNUM.  If already past COLNUM tab to the next
       multiple of COLINC.
     :SECTION - Same as :LINE, but count from the start of the current
       section, not the start of the line.
     :LINE-RELATIVE - Output COLNUM spaces, then tab to the next multiple of
       COLINC.
     :SECTION-RELATIVE - Same as :LINE-RELATIVE, but count from the start
       of the current section, not the start of the line."
  (declare (type (member :line :section :line-relative :section-relative) kind)
	   (type unsigned-byte colnum colinc)
	   (type (or stream (member t nil)) stream)
	   (values null))
  (let ((stream (case stream
		  ((t) *terminal-io*)
		  ((nil) *standard-output*)
		  (t stream))))
    (when (and (pretty-stream-p stream) *print-pretty*)
      (enqueue-tab stream kind colnum colinc)))
  nil)

(defun pprint-fill (stream list &optional (colon? t) atsign?)
  "Output LIST to STREAM putting :FILL conditional newlines between each
   element.  If COLON? is NIL (defaults to T), then no parens are printed
   around the output.  ATSIGN? is ignored (but allowed so that PPRINT-FILL
   can be used with the ~/.../ format directive."
  (declare (ignore atsign?))
  (pprint-logical-block (stream list
				:prefix (if colon? "(" "")
				:suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
      (output-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :fill stream))))

(defun pprint-linear (stream list &optional (colon? t) atsign?)
  "Output LIST to STREAM putting :LINEAR conditional newlines between each
   element.  If COLON? is NIL (defaults to T), then no parens are printed
   around the output.  ATSIGN? is ignored (but allowed so that PPRINT-LINEAR
   can be used with the ~/.../ format directive."
  (declare (ignore atsign?))
  (pprint-logical-block (stream list
				:prefix (if colon? "(" "")
				:suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
      (output-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :linear stream))))

(defun pprint-tabular (stream list &optional (colon? t) atsign? tabsize)
  "Output LIST to STREAM tabbing to the next column that is an even multiple
   of TABSIZE (which defaults to 16) between each element.  :FILL style
   conditional newlines are also output between each element.  If COLON? is
   NIL (defaults to T), then no parens are printed around the output.
   ATSIGN? is ignored (but allowed so that PPRINT-TABULAR can be used with
   the ~/.../ format directive."
  (declare (ignore atsign?))
  (pprint-logical-block (stream list
				:prefix (if colon? "(" "")
				:suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
       (output-object (pprint-pop) stream)
       (pprint-exit-if-list-exhausted)
       (write-char #\space stream)
       (pprint-tab :section-relative 0 (or tabsize 16) stream)
       (pprint-newline :fill stream))))


;;;; Pprint-dispatch tables.

(defvar *initial-pprint-dispatch*)
(defvar *building-initial-table* nil)

(defstruct (pprint-dispatch-entry
	    (:print-function %print-pprint-dispatch-entry))
  ;;
  ;; The type specifier for this entry.
  (type (required-argument) :type t)
  ;;
  ;; A function to test to see if an object is of this time.  Pretty must
  ;; just (lambda (obj) (typep object type)) except that we handle the
  ;; CONS type specially so that (cons (member foo)) works.  We don't
  ;; bother computing this for entries in the CONS hash table, because
  ;; we don't need it.
  (test-fn nil :type (or function null))
  ;;
  ;; The priority for this guy.
  (priority 0 :type real)
  ;;
  ;; T iff one of the original entries.
  (initial-p *building-initial-table* :type (member t nil))
  ;;
  ;; And the associated function.
  (function (required-argument) :type (or symbol function)))

(defun %print-pprint-dispatch-entry (entry stream depth)
  (declare (ignore depth))
  (print-unreadable-object (entry stream :type t)
    (format stream "Type=~S, priority=~S~@[ [Initial]~]"
	    (pprint-dispatch-entry-type entry)
	    (pprint-dispatch-entry-priority entry)
	    (pprint-dispatch-entry-initial-p entry))))

(defstruct (pprint-dispatch-table
	    (:print-function %print-pprint-dispatch-table))
  ;;
  ;; A list of all the entries (except for CONS entries below) in highest
  ;; to lowest priority.
  (entries nil :type list)
  ;;
  ;; A hash table mapping things to entries for type specifiers of the
  ;; form (CONS (MEMBER <thing>)).  If the type specifier is of this form,
  ;; we put it in this hash table instead of the regular entries table.
  (cons-entries (make-hash-table :test #'eql)))

(defun %print-pprint-dispatch-table (table stream depth)
  (declare (ignore depth))
  (print-unreadable-object (table stream :type t :identity t)))

(defun cons-type-specifier-p (spec)
  (and (consp spec)
       (eq (car spec) 'cons)
       (cdr spec)
       (null (cddr spec))
       (let ((car (cadr spec)))
	 (and (consp car)
	      (let ((carcar (car car)))
		(or (eq carcar 'member)
		    (eq carcar 'eql)))
	      (cdr car)
	      (null (cddr car))))))

(defun entry< (e1 e2)
  (declare (type pprint-dispatch-entry e1 e2))
  (if (pprint-dispatch-entry-initial-p e1)
      (if (pprint-dispatch-entry-initial-p e2)
	  (< (pprint-dispatch-entry-priority e1)
	     (pprint-dispatch-entry-priority e2))
	  t)
      (if (pprint-dispatch-entry-initial-p e2)
	  nil
	  (< (pprint-dispatch-entry-priority e1)
	     (pprint-dispatch-entry-priority e2)))))


(macrolet ((frob (x)
	     `(cons ',x #'(lambda (object) ,x))))
  (defvar *precompiled-pprint-dispatch-funs*
    (list (frob (typep object 'array))
	  (frob (and (consp object)
		     (and (typep (car object) 'symbol)
			  (typep (car object) '(satisfies fboundp)))))
	  (frob (typep object 'cons)))))


(defun compute-test-fn (type)
  (let ((was-cons nil))
    (labels ((compute-test-expr (type object)
	       (if (listp type)
		   (case (car type)
		     (cons
		      (setq was-cons t)
		      (destructuring-bind
			  (&optional (car nil car-p) (cdr nil cdr-p))
			  (cdr type)
			`(and (consp ,object)
			      ,@(when car-p
				  `(,(compute-test-expr
				      car `(car ,object))))
			      ,@(when cdr-p
				  `(,(compute-test-expr
				      cdr `(cdr ,object)))))))
		     (not
		      (destructuring-bind (type) (cdr type)
			`(not ,(compute-test-expr type object))))
		     (and
		      `(and ,@(mapcar #'(lambda (type)
					  (compute-test-expr type object))
				      (cdr type))))
		     (or
		      `(or ,@(mapcar #'(lambda (type)
					 (compute-test-expr type object))
				     (cdr type))))
		     (t
		      `(typep ,object ',type)))
		   `(typep ,object ',type))))
      (let ((expr (compute-test-expr type 'object)))
	(cond ((cdr (assoc expr *precompiled-pprint-dispatch-funs*
			   :test #'equal)))
	      ((fboundp 'compile)
	       (compile nil `(lambda (object) ,expr)))
	      (was-cons
	       (warn "CONS PPRINT dispatch ignored w/o compiler loaded:~%  ~S"
		     type)
	       #'(lambda (object) (declare (ignore object)) nil))
	      (t
	       (let ((ttype (kernel:specifier-type type)))
		 #'(lambda (object) (kernel:%typep object ttype)))))))))


(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (declare (type (or pprint-dispatch-table null) table))
  (let* ((orig (or table *initial-pprint-dispatch*))
	 (new (make-pprint-dispatch-table
	       :entries (copy-list (pprint-dispatch-table-entries orig))))
	 (new-cons-entries (pprint-dispatch-table-cons-entries new)))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-cons-entries) value))
	     (pprint-dispatch-table-cons-entries orig))
    new))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (declare (type (or pprint-dispatch-table null) table))
  (let* ((table (or table *initial-pprint-dispatch*))
	 (cons-entry
	  (and (consp object)
	       (gethash (car object)
			(pprint-dispatch-table-cons-entries table))))
	 (entry
	  (dolist (entry (pprint-dispatch-table-entries table) cons-entry)
	    (when (and cons-entry
		       (entry< entry cons-entry))
	      (return cons-entry))
	    (when (funcall (pprint-dispatch-entry-test-fn entry) object)
	      (return entry)))))
    (if entry
	(values (pprint-dispatch-entry-function entry) t)
	(values #'(lambda (stream object)
		    (output-ugly-object object stream))
		nil))))

(defun set-pprint-dispatch (type function &optional
			    (priority 0) (table *print-pprint-dispatch*))
  (declare (type (or null symbol function) function)
	   (type real priority)
	   (type pprint-dispatch-table table))
  (if function
      (if (cons-type-specifier-p type)
	  (setf (gethash (second (second type))
			 (pprint-dispatch-table-cons-entries table))
		(make-pprint-dispatch-entry :type type :priority priority
					    :function function))
	  (let ((list (delete type (pprint-dispatch-table-entries table)
			      :key #'pprint-dispatch-entry-type
			      :test #'equal))
		(entry (make-pprint-dispatch-entry
			:type type :test-fn (compute-test-fn type)
			:priority priority :function function)))
	    (do ((prev nil next)
		 (next list (cdr next)))
		((null next)
		 (if prev
		     (setf (cdr prev) (list entry))
		     (setf list (list entry))))
	      (when (entry< (car next) entry)
		(if prev
		    (setf (cdr prev) (cons entry next))
		    (setf list (cons entry next)))
		(return)))
	    (setf (pprint-dispatch-table-entries table) list)))
      (if (cons-type-specifier-p type)
	  (remhash (second (second type))
		   (pprint-dispatch-table-cons-entries table))
	  (setf (pprint-dispatch-table-entries table)
		(delete type (pprint-dispatch-table-entries table)
			:key #'pprint-dispatch-entry-type
			:test #'equal))))
  nil)


(defun array-readably-printable-p (array)
  (and (eq (array-element-type array) t)
       (let ((zero (position 0 (array-dimensions array)))
             (number (position 0 (array-dimensions array)
                               :test (complement #'eql)
                               :from-end t)))
         (or (null zero) (null number) (> zero number)))))


;;;; Standard pretty-printing routines.

(defun pprint-array (stream array)
  (cond ((or (and (null *print-array*) (null *print-readably*))
	     (stringp array)
	     (bit-vector-p array))
	 (output-ugly-object array stream))
	((and *print-readably*
	      (not (lisp::array-readably-printable-p array)))
	 (pprint-raw-array stream array))
	((vectorp array)
	 (pprint-vector stream array))
	(t
	 (pprint-multi-dim-array stream array))))

(defun pprint-vector (stream vector)
  (pprint-logical-block (stream nil :prefix "#(" :suffix ")")
    (dotimes (i (length vector))
      (unless (zerop i)
	(write-char #\space stream)
	(pprint-newline :fill stream))
      (pprint-pop)
      (output-object (aref vector i) stream))))

(defun pprint-array-contents (stream array)
  (lisp::with-array-data ((data array) (start) (end))
    (declare (ignore end))
    (labels ((output-guts (stream index dimensions)
	       (if (null dimensions)
		   (output-object (aref data index) stream)
		   (pprint-logical-block
		    (stream nil :prefix "(" :suffix ")")
		    (let ((dim (car dimensions)))
		      (unless (zerop dim)
			(let* ((dims (cdr dimensions))
			       (index index)
			       (step (reduce #'* dims))
			       (count 0))
			  (loop				
			   (pprint-pop)
			   (output-guts stream index dims)
			   (when (= (incf count) dim)
			     (return))
			   (write-char #\space stream)
			   (pprint-newline (if dims :linear :fill)
					   stream)
			   (incf index step)))))))))
      (output-guts stream start (array-dimensions array)))))

(defun pprint-multi-dim-array (stream array)
  (funcall (formatter "#~DA") stream (array-rank array))
  (pprint-array-contents stream array))

(defun pprint-raw-array (stream array)
  (write-string "#A" stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (output-object (array-element-type array) stream)
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (output-object (array-dimensions array) stream)
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (pprint-array-contents stream array)))

(defun pprint-lambda-list (stream lambda-list &rest noise)
  (declare (ignore noise))
  (when (and (consp lambda-list)
	     (member (car lambda-list) lisp::*bq-tokens*))
    ;; if this thing looks like a backquoty thing, then we don't want
    ;; to destructure it, we want to output it straight away.  [ this
    ;; is the exception to the normal processing: if we did this
    ;; generally we would find lambda lists such as (FUNCTION FOO)
    ;; being printed as #'FOO ]  -- CSR, 2003-12-07
    (output-object lambda-list stream)
    (return-from pprint-lambda-list nil))

  (pprint-logical-block (stream lambda-list :prefix "(" :suffix ")")
    (let ((state :required)
	  (first t))
      (loop
	(pprint-exit-if-list-exhausted)
	(unless first
	  (write-char #\space stream))
	(let ((arg (pprint-pop)))
	  (unless first
	    (case arg
	      (&optional
	       (setf state :optional)
	       (pprint-newline :linear stream))
	      ((&rest &body)
	       (setf state :required)
	       (pprint-newline :linear stream))
	      (&key
	       (setf state :key)
	       (pprint-newline :linear stream))
	      (&aux
	       (setf state :optional)
	       (pprint-newline :linear stream))
	      (t
	       (pprint-newline :fill stream))))
	  (ecase state
	    (:required
	     (pprint-lambda-list stream arg))
	    ((:optional :key)
	     (pprint-logical-block
		 (stream arg :prefix "(" :suffix ")")
	       (pprint-exit-if-list-exhausted)
	       (if (eq state :key)
		   (pprint-logical-block
		       (stream (pprint-pop) :prefix "(" :suffix ")")
		     (pprint-exit-if-list-exhausted)
		     (output-object (pprint-pop) stream)
		     (pprint-exit-if-list-exhausted)
		     (write-char #\space stream)
		     (pprint-newline :fill stream)
		     (pprint-lambda-list stream (pprint-pop))
		     (loop
		       (pprint-exit-if-list-exhausted)
		       (write-char #\space stream)
		       (pprint-newline :fill stream)
		       (output-object (pprint-pop) stream)))
		   (pprint-lambda-list stream (pprint-pop)))
	       (loop
		 (pprint-exit-if-list-exhausted)
		 (write-char #\space stream)
		 (pprint-newline :linear stream)
		 (output-object (pprint-pop) stream))))))
	(setf first nil)))))

(defun pprint-lambda (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^~3I ~:_~/PP:PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
	   stream list))

(defun pprint-block (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^~3I ~:_~W~1I~@{ ~_~W~}~:>") stream list))

(defun pprint-flet (stream list &rest noise)
  (declare (ignore noise))
  ;; If the second element of the list is not a list, then we don't
  ;; really have a flet/labels form, so let's just print it as a
  ;; list. (This happens when the list is the name an flet/labels in a
  ;; function.
  (if (listp (second list))
      (funcall (formatter
		"~:<~^~W~^ ~@_~:<~@{~:<~^~W~^~3I ~:_~/PP:PPRINT-LAMBDA-LIST/~1I~:@_~@{~W~^ ~_~}~:>~^ ~_~}~:>~1I~@:_~@{~W~^ ~_~}~:>")
	       stream
	       list)
      (funcall (formatter "~@:<~{~W~^ ~:_~}~:>")
	       stream list)))

(defun pprint-let (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^ ~@_~:<~@{~:<~^~W~@{ ~_~W~}~:>~^ ~_~}~:>~1I~:@_~@{~W~^ ~_~}~:>")
	   stream
	   list))

(defun pprint-progn (stream list &rest noise)
  (declare (ignore noise))
  #+nil
  (funcall (formatter "~:<~^~W~@{ ~_~W~}~:>") stream list)
  (funcall (formatter "~:<~^~W~1I~@{ ~@:_~W~}~:>") stream list)
  )

(defun pprint-progv (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^~3I ~_~W~^ ~_~W~^~1I~@{ ~_~W~}~:>")
	   stream list))

(defun pprint-multiple-value-bind (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^~5I ~:_~W~^~3I ~:@_~W~1I~@{ ~:@_~W~}~:>")
	   stream list))

(defun pprint-handler-bind (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~3I ~:_~W~1I~@{ ~:@_~W~}~:>")
	   stream list))
  
(defun pprint-quote (stream list &rest noise)
  (declare (ignore noise))
  (if (and (consp list)
	   (consp (cdr list))
	   (null (cddr list)))
      (case (car list)
	(function
	 (write-string "#'" stream)
	 (output-object (cadr list) stream))
	(quote
	 (write-char #\' stream)
	 (output-object (cadr list) stream))
	(t
	 (pprint-fill stream list)))
      (pprint-fill stream list)))

(defun pprint-setq (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (if (and (consp (cdr list)) (consp (cddr list)))
	(loop
	  (pprint-indent :current 2 stream)
	  (output-object (pprint-pop) stream)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space stream)
	  (pprint-newline :linear stream)
	  (pprint-indent :current -2 stream)
	  (output-object (pprint-pop) stream)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space stream)
	  (pprint-newline :linear stream))
	(progn
	  (pprint-indent :current 0 stream)
	  (output-object (pprint-pop) stream)
	  (pprint-exit-if-list-exhausted)
	  (write-char #\space stream)
	  (pprint-newline :linear stream)
	  (output-object (pprint-pop) stream)))))
	  
(defmacro pprint-tagbody-guts (stream)
  `(loop
     (pprint-exit-if-list-exhausted)
     (write-char #\space ,stream)
     (let ((form-or-tag (pprint-pop)))
       (pprint-indent :block 
		      (if (atom form-or-tag) 0 1)
		      ,stream)
       (pprint-newline :linear ,stream)
       (output-object form-or-tag ,stream))))

(defun pprint-tagbody (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-tagbody-guts stream)))

(defun pprint-case (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~:/PP:PPRINT-FILL/~^~@{ ~_~W~}~:>~}~:>")
	   stream
	   list))

(defun pprint-cond (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~W~^~1I~:@_~@{~:<~^~W~^~:@_~@{~W~^~:@_~}~:>~^~:@_~}~:>")
	   stream list))

(defun pprint-declare (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; Output "declare"
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-newline :miser stream)
    (write-char #\space stream)
    (pprint-indent :current 0 stream)
    ;; Print out each spec.
    (loop
       (let ((spec (pprint-pop)))
	 ;; If the spec begins with TYPE, we indent it so that vars
	 ;; are lined up after the actual type.  Otherwise, we just
	 ;; make them all line up after the first symbol of the
	 ;; spec.
	 (pprint-logical-block (stream spec :prefix "(" :suffix ")")
	   (when (eq (car spec) 'type)
	     (output-object (pprint-pop) stream)
	     (pprint-exit-if-list-exhausted)
	     (pprint-newline :miser stream)
	     (write-char #\space stream))
	   (output-object (pprint-pop) stream)
	   (pprint-exit-if-list-exhausted)
	   (pprint-newline :miser stream)
	   (write-char #\space stream)
	   (pprint-indent :current 0 stream)
	   (loop
	      (output-object (pprint-pop) stream)
	      (pprint-exit-if-list-exhausted)
	      (write-char #\space stream)
	      (pprint-newline :fill stream))))
       (pprint-exit-if-list-exhausted)
       (write-char #\space stream)
       (pprint-newline :linear stream))))

(defun pprint-defun (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^ ~@_~:I~W~^ ~:_~/PP:PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
	   stream
	   list))

(defun pprint-destructuring-bind (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	"~:<~^~W~^~3I ~_~:/PP:PPRINT-LAMBDA-LIST/~^ ~_~W~^~1I~@{ ~_~W~}~:>")
	   stream list))

(defun pprint-do (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-indent :current 0 stream)
    (funcall (formatter "~:<~@{~:<~^~W~^ ~@_~:I~W~@{ ~_~W~}~:>~^~:@_~}~:>")
	     stream
	     (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :linear stream)
    (pprint-linear stream (pprint-pop))
    (pprint-tagbody-guts stream)))

(defun pprint-dolist (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 3 stream)
    (write-char #\space stream)
    (pprint-newline :fill stream)
    (funcall (formatter "~:<~^~W~^ ~:_~:I~W~@{ ~_~W~}~:>")
	     stream
	     (pprint-pop))
    (pprint-tagbody-guts stream)))

(defun pprint-typecase (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
	    "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~W~^~@{ ~_~W~}~:>~}~:>")
	   stream
	   list))

(defun pprint-prog (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (pprint-fill stream (pprint-pop))
    (pprint-tagbody-guts stream)))

#+nil
(defun pprint-function-call (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^ ~:_~:I~@{~W~^ ~_~}~:>")
	   stream
	   list))

(defun pprint-function-call (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    ;; Function name
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :fill stream)
    (pprint-indent :current 0 stream)
    ;; Output args.  We try to keep keyword/value pairs on the same
    ;; line.  This, of course, doesn't work if the keyword isn't a
    ;; keyword arg because that will mess up the following stuff if
    ;; the next arg is really a keyword arg.
    (loop
       (let ((arg (pprint-pop)))
	 (output-object arg stream)
	 (pprint-exit-if-list-exhausted)
	 (write-char #\space stream)
	 (when (keywordp arg)
	   (output-object (pprint-pop) stream)
	   (pprint-exit-if-list-exhausted)
	   (write-char #\space stream))
	 (pprint-newline :linear stream)))))

(defun pprint-with-like (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~W~^~3I ~:_~W~^~1I~@{ ~@:_~W~}~:>")
	   stream list))

(defun pprint-defstruct (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; DEFSTRUCT
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-indent :block 1 stream)
    ;; Output name and any options neatly
    (funcall (formatter "~:<~W~^ ~1I~@:_~@{~:<~W~^ ~:I~W~@{ ~_~W~}~:>~^~@:_~}~:>")
	     stream (pprint-pop))
    ;; Output each slot neatly
    (loop
       (pprint-exit-if-list-exhausted)
       (pprint-newline :mandatory stream)
       (funcall (formatter "~:<~W~^~:I ~W~_~@{ ~W~^~@_~}~:>") stream (pprint-pop)))))

(defun pprint-defclass (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; DEFCLASS
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 2 stream)
    (write-char #\space stream)
    ;; Output class-name
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-newline :fill stream)
    (write-char #\space stream)
    ;; Output superclasses
    (funcall (formatter "~W") stream (pprint-pop))
    (pprint-indent :block 1 stream)
    (pprint-newline :mandatory stream)
    ;; Output slots.  We try to output keyword and value on one line together
    (funcall (formatter "~:<~^~@{~:<~^~W~^ ~:I~@{~W~^ ~W~^~@:_~}~:>~^~@:_~}~:>")
		  stream (pprint-pop))
    ;; Output options
    (loop
       (pprint-exit-if-list-exhausted)
       (pprint-newline :mandatory stream)
       (output-object (pprint-pop) stream))))

(defun pprint-defgeneric (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; DEFGENERIC
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 2 stream)
    (write-char #\space stream)
    ;; Output class-name
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-newline :fill stream)
    (write-char #\space stream)
    ;; Output lambda list
    (pprint-lambda-list stream (pprint-pop))
    (pprint-indent :block 1 stream)
    ;;(pprint-newline :linear stream)
    ;; Output options and stuff
    (loop
       (pprint-exit-if-list-exhausted)
       (pprint-newline :mandatory stream)
       (let ((option-or-method (pprint-pop)))
	 ;; Try to print the various options neatly.  Right now, we
	 ;; just handle :method
	 (cond ((and (consp option-or-method)
		     (eq (car option-or-method) :method))
		(flet
		    ((almost-defmethod (stream list)
		       (pprint-logical-block (stream list :prefix "(" :suffix ")")
			 ;; :method
			 (output-object (pprint-pop) stream)
			 (pprint-exit-if-list-exhausted)
			 ;; Print out all qualifiers then the lambda list
			 (loop
			    (let ((qual-or-lambda (pprint-pop)))
			      (write-char #\space stream)
			      (cond ((listp qual-or-lambda)
				     ;; Print out lambda-list
				     (pprint-lambda-list stream qual-or-lambda)
				     (return))
				    (t
				     (output-object qual-or-lambda stream)))
			      (pprint-exit-if-list-exhausted)))
			 ;; Rest of the forms
			 (pprint-newline :mandatory stream)
			 (pprint-indent :block 0 stream)
			 (loop
			    (write-char #\space stream)
			    (output-object (pprint-pop) stream)
			    (pprint-exit-if-list-exhausted)
			    (pprint-newline :linear stream)))))
		  (almost-defmethod stream option-or-method)))
	       (t
		(output-object option-or-method stream)))))))

  

(defun pprint-defpackage (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~W ~W~I~@{~@:_ ~:<~@{~W~^~:I~@{ ~W~^~:_~}~}~:>~}~:>")
	   stream list))

(defun pprint-defmethod (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; DEFMETHOD
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    ;; Method name
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    ;; Print out method qualifiers, if any
    (loop
       (let ((qual-or-lambda (pprint-pop)))
	 (write-char #\space stream)
	 (cond ((listp qual-or-lambda)
		;; Print out lambda-list
		(pprint-lambda-list stream qual-or-lambda)
		(return))
	       (t
		(output-object qual-or-lambda stream)))
	 (pprint-exit-if-list-exhausted)))
    ;; Rest of the forms
    (pprint-newline :mandatory stream)
    (pprint-indent :block 0 stream)
    (loop
       (write-char #\space stream)
       (output-object (pprint-pop) stream)
       (pprint-exit-if-list-exhausted)
       (pprint-newline :linear stream))))

(defun pprint-restart-case (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    ;; RESTART-CASE
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    ;; restartable form
    (output-object (pprint-pop) stream)
    (pprint-indent :block 1 stream)
    ;; Output each clause
    (loop
       (pprint-exit-if-list-exhausted)
       (pprint-newline :mandatory stream)
       (destructuring-bind (case-name lambda-list &rest forms)
	   (pprint-pop)
	 (pprint-logical-block (stream forms :prefix "(" :suffix ")")
	   ;; case-name
	   (output-object case-name stream)
	   (write-char #\space stream)
	   ;; lambda-list
	   (pprint-lambda-list stream lambda-list)
	   ;;(pprint-newline :mandatory stream)
	   (pprint-exit-if-list-exhausted)
	   (loop
	      (write-char #\space stream)
	      (output-object (pprint-pop) stream)
	      (pprint-exit-if-list-exhausted)
	      (pprint-newline :linear stream)))))))

(defun pprint-when (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~W~^~3I ~:_~W~I~@:_~@{ ~W~^~_~}~:>")
	   stream list))


;;;; Interface seen by regular (ugly) printer and initialization routines.

;;; OUTPUT-PRETTY-OBJECT is called by OUTPUT-OBJECT when *PRINT-PRETTY* is
;;; bound to T.
;;;
(defun output-pretty-object (object stream)
  (with-pretty-stream (stream)
    (funcall (pprint-dispatch object) stream object)))

(defparameter *magic-forms*
  '((lambda pprint-lambda)
    ;; Special forms.
    (block pprint-block)
    (catch pprint-block)
    (compiler-let pprint-let)
    (eval-when pprint-block)
    (flet pprint-flet)
    (function pprint-quote)
    (labels pprint-flet)
    (let pprint-let)
    (let* pprint-let)
    (locally pprint-progn)
    (macrolet pprint-flet)
    (multiple-value-call pprint-block)
    (multiple-value-prog1 pprint-block)
    (progn pprint-progn)
    (progv pprint-progv)
    (quote pprint-quote)
    (return-from pprint-block)
    (setq pprint-setq)
    (symbol-macrolet pprint-let)
    (tagbody pprint-tagbody)
    (throw pprint-block)
    (unwind-protect pprint-block)
    (declare pprint-declare)
    
    ;; Macros.
    (case pprint-case)
    (ccase pprint-case)
    (cond pprint-cond)
    (ctypecase pprint-typecase)
    (defconstant pprint-block)
    (define-condition pprint-defclass)
    (define-modify-macro pprint-defun)
    (define-setf-expander pprint-defun)
    (defmacro pprint-defun)
    (defpackage pprint-defpackage)
    (defparameter pprint-block)
    (defsetf pprint-defun)
    (defstruct pprint-defstruct)
    (deftype pprint-defun)
    (defun pprint-defun)
    (defvar pprint-block)
    (destructuring-bind pprint-destructuring-bind)
    (do pprint-do)
    (do* pprint-do)
    (do-all-symbols pprint-dolist)
    (do-external-symbols pprint-dolist)
    (do-symbols pprint-dolist)
    (dolist pprint-dolist)
    (dotimes pprint-dolist)
    (ecase pprint-case)
    (etypecase pprint-typecase)
    (handler-bind pprint-handler-bind)
    (handler-case pprint-handler-bind)
    ;; Loop is handled by pprint-loop.lisp
    #+nil(loop pprint-loop)
    (multiple-value-bind pprint-multiple-value-bind)
    (multiple-value-setq pprint-block)
    (pprint-logical-block pprint-block)
    (print-unreadable-object pprint-with-like)
    (prog pprint-prog)
    (prog* pprint-prog)
    (prog1 pprint-block)
    (prog2 pprint-progv)
    (psetf pprint-setq)
    (psetq pprint-setq)
    #+nil (restart-bind ...)
    (restart-case pprint-restart-case)
    (setf pprint-setq)
    (step pprint-progn)
    (time pprint-progn)
    (typecase pprint-typecase)
    (unless pprint-when)
    (when pprint-when)
    (with-compilation-unit pprint-with-like)
    (with-condition-restarts pprint-with-like)
    (with-hash-table-iterator pprint-with-like)
    (with-input-from-string pprint-with-like)
    (with-open-file pprint-with-like)
    (with-open-stream pprint-with-like)
    (with-output-to-string pprint-with-like)
    (with-package-iterator pprint-with-like)
    (with-simple-restart pprint-with-like)
    (with-standard-io-syntax pprint-progn)

    ;; CLOS things
    (defclass pprint-defclass)
    (defmethod pprint-defmethod)
    (defgeneric pprint-defgeneric)
    (with-slots pprint-multiple-value-bind)
    (with-accessors pprint-multiple-value-bind)
    

    ;; Other things in CMUCL that we ought to try to print out nicely.
    (ext:collect pprint-with-like)
    (ansi-loop::with-loop-list-collection-head pprint-with-like)
    (lisp::descend-into pprint-with-like)
    (vm::pseudo-atomic pprint-with-like)
    (new-assem:without-scheduling pprint-with-like)
    (vm::with-fixed-allocation pprint-with-like)
    (kernel::number-dispatch pprint-with-like)))

(defun pprint-init ()
  (setf *initial-pprint-dispatch* (make-pprint-dispatch-table))
  (let ((*print-pprint-dispatch* *initial-pprint-dispatch*)
	(*building-initial-table* t))
    ;; Printers for regular types.
    (set-pprint-dispatch 'array #'pprint-array)
    (set-pprint-dispatch '(cons (and symbol (satisfies fboundp)))
			 #'pprint-function-call -1)
    (set-pprint-dispatch 'cons #'pprint-fill -2)
    ;; Cons cells with interesting things for the car.
    (dolist (magic-form *magic-forms*)
      (set-pprint-dispatch `(cons (eql ,(first magic-form)))
			   (symbol-function (second magic-form))))
    ;; Other pretty-print init forms.
    (lisp::backq-pp-init)
    (loop-pp-init))

  (setf *print-pprint-dispatch* (copy-pprint-dispatch nil))
  (setf *pretty-printer* #'output-pretty-object)
  (setf *print-pretty* t))

