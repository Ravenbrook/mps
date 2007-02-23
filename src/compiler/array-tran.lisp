;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/array-tran.lisp,v 1.41 2006/06/30 18:41:23 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains array specific optimizers and transforms.
;;; 
;;; Extracted from srctran and extended by William Lott.
;;;
(in-package "C")


;;;; Derive-Type Optimizers

;;; ASSERT-ARRAY-RANK  -- internal
;;;
;;; Array operations that use a specific number of indices implicitly assert
;;; that the array is of that rank.
;;; 
(defun assert-array-rank (array rank)
  (assert-continuation-type
   array
   (specifier-type `(array * ,(make-list rank :initial-element '*)))))
  
;;; EXTRACT-ELEMENT-TYPE  -- internal
;;;
;;; Array access functions return an object from the array, hence it's
;;; type will be asserted to be array element type.
;;;
(defun extract-element-type (array)
  (let ((type (continuation-type array)))
    (if (array-type-p type)
	(array-type-element-type type)
	*universal-type*)))

;;; EXTRACT-UPGRADED-ELEMENT-TYPE  -- internal
;;;
;;; Array access functions return an object from the array, hence it's
;;; type is going to be the array upgraded element type.
;;;
(defun extract-upgraded-element-type (array)
  (let ((type (continuation-type array)))
    (if (array-type-p type)
	(array-type-specialized-element-type type)
	*universal-type*)))

;;; ASSERT-NEW-VALUE-TYPE  --  internal
;;;
;;; The ``new-value'' for array setters must fit in the array, and the
;;; return type is going to be the same as the new-value for setf functions.
;;; 
(defun assert-new-value-type (new-value array)
  (let ((type (continuation-type array)))
    (when (array-type-p type)
      (assert-continuation-optional-type new-value
					 (array-type-specialized-element-type type))))
  (continuation-type new-value))

;;; Unsupplied-Or-NIL  --  Internal
;;;
;;;    Return true if Arg is NIL, or is a constant-continuation whose value is
;;; NIL, false otherwise.
;;;
(defun unsupplied-or-nil (arg)
  (declare (type (or continuation null) arg))
  (or (not arg)
      (and (constant-continuation-p arg)
	   (not (continuation-value arg)))))


;;; ARRAY-IN-BOUNDS-P  --  derive-type optimizer.
;;;
(defoptimizer (array-in-bounds-p derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  *universal-type*)

;;; AREF  --  derive-type optimizer.
;;;
(defoptimizer (aref derive-type) ((array &rest indices) node)
  (assert-array-rank array (length indices))
  ;; If the node continuation has a single use then assert its type.
  ;;
  ;; Let's not do this.  As reported by Lynn Quam on cmucl-imp on
  ;; 2004-03-30, the compiler generates bogus code for
  ;;
  ;; (defun foo (f d)
  ;;   (declare (type (simple-array single-float (*)) f)
  ;;            (type (simple-array double-float (*)) d))
  ;;   (setf (aref f 0) (aref d 0)))
  ;;
  ;; and doesn't even warn about the type mismatch.  This might be a
  ;; symptom of other compiler bugs, but removing this at least gives
  ;; us back the warning.  I (RLT) do not know what impact removing
  ;; this has on other user code.
  #+(or)
  (let ((cont (node-cont node)))
    (when (= (length (find-uses cont)) 1)
      (assert-continuation-type cont (extract-upgraded-element-type array))))
  (extract-upgraded-element-type array))

;;; %ASET  --  derive-type optimizer.
;;;
(defoptimizer (%aset derive-type) ((array &rest stuff))
  (assert-array-rank array (1- (length stuff)))
  (assert-new-value-type (car (last stuff)) array)
  ;; Without the following,  this function
  ;;
  ;;   (defun fn-492 (r p1)
  ;;     (declare (optimize speed (safety 1))
  ;;		(type (simple-array (signed-byte 8) nil) r) (type (integer * 22050378) p1))
  ;;     (setf (aref r) (lognand (the (integer 19464371) p1) 2257))
  ;;     (values))
  ;;   (defun tst-492 ()
  ;;     (let ((r (make-array nil :element-type '(signed-byte 8))))
  ;;       (fn-492 r 19469591)
  ;;       (aref r)))
  ;;
  ;; causes the compiler to delete (values) as unreachable and in so
  ;; doing, deletes the return, so we just run off the end.  I (rtoy)
  ;; think this is caused by some confusion in type-derivation.  The
  ;; derived result of the lognand is bigger than a (signed-byte 8).
  ;;
  ;; I think doing this also causes some loss, because we return the
  ;; element-type of the array, even though the result of the aset is
  ;; the new value.  Well, almost.  The element type is returned only
  ;; if the array continuation is really an array type.  Otherwise, we
  ;; do return the type of the new value.
  ;;
  ;; FIXME: This needs something better, but I (rtoy) am not smart
  ;; enough to know what to do about it.
  (let ((atype (continuation-type array)))
    (if (array-type-p atype)
	(array-type-specialized-element-type atype)
	(continuation-type (car (last stuff))))))

;;; DATA-VECTOR-REF  --  derive-type optimizer.
;;;
(defoptimizer (data-vector-ref derive-type) ((array index))
  (extract-upgraded-element-type array))

;;; DATA-VECTOR-SET  --  derive-type optimizer.
;;;
(defoptimizer (data-vector-set derive-type) ((array index new-value))
  (assert-new-value-type new-value array))

;;; %WITH-ARRAY-DATA  --  derive-type optimizer.
;;;
;;;    Figure out the type of the data vector if we know the argument element
;;; type.
;;;
(defoptimizer (%with-array-data derive-type) ((array start end))
  (let ((atype (continuation-type array)))
    (when (array-type-p atype)
      (values-specifier-type
       `(values (simple-array ,(type-specifier
				(array-type-element-type atype))
			      (*))
		index index index)))))


;;; ARRAY-ROW-MAJOR-INDEX  --  derive-type optimizer.
;;;
(defoptimizer (array-row-major-index derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  *universal-type*)

;;; ROW-MAJOR-AREF  --  derive-type optimizer.
;;;
(defoptimizer (row-major-aref derive-type) ((array index))
  (extract-upgraded-element-type array))
  
;;; %SET-ROW-MAJOR-AREF  --  derive-type optimizer.
;;;
(defoptimizer (%set-row-major-aref derive-type) ((array index new-value))
  (assert-new-value-type new-value array))

;;; MAKE-ARRAY  --  derive-type optimizer.
;;; 
(defoptimizer (make-array derive-type)
	      ((dims &key initial-element element-type initial-contents
		adjustable fill-pointer displaced-index-offset displaced-to))
  (let ((simple (and (unsupplied-or-nil adjustable)
		     (unsupplied-or-nil displaced-to)
		     (unsupplied-or-nil fill-pointer))))
    (specifier-type
     `(,(if simple 'simple-array 'array)
       ,(cond ((not element-type) 't)
	      ((constant-continuation-p element-type)
	       (continuation-value element-type))
	      (t
	       '*))
       ,(cond ((not simple)
	       '*)
	      ((constant-continuation-p dims)
	       (let ((val (continuation-value dims)))
		 (if (listp val) val (list val))))
	      ((csubtypep (continuation-type dims)
			  (specifier-type 'integer))
	       '(*))
	      (t
	       '*))))))


;;;; Constructors.

;;; VECTOR  --  source-transform.
;;;
;;; Convert VECTOR into a make-array followed by setfs of all the elements.
;;;
(def-source-transform vector (&rest elements)
  (if (byte-compiling)
      (values nil t)
      (let ((len (length elements))
	    (n -1))
	(once-only ((n-vec `(make-array ,len)))
	  `(progn 
	     ,@(mapcar #'(lambda (el)
			   (once-only ((n-val el))
			     `(locally (declare (optimize (safety 0)))
				       (setf (svref ,n-vec ,(incf n))
					     ,n-val))))
		       elements)
	     ,n-vec)))))


;;; MAKE-STRING  --  source-transform.
;;; 
;;; Just convert it into a make-array.
;;;
(deftransform make-string ((length &key (element-type 'base-char)
				   (initial-element #\NULL)))
  `(make-array (the (values index &rest t) length)
	       :element-type element-type
	       :initial-element initial-element))

(defconstant array-info
  '((base-char #\NULL 8 vm:simple-string-type)
    (single-float 0.0f0 32 vm:simple-array-single-float-type)
    (double-float 0.0d0 64 vm:simple-array-double-float-type)
    #+long-float (long-float 0.0l0 #+x86 96 #+sparc 128
		  vm:simple-array-long-float-type)
    #+double-double
    (double-double-float 0w0 128
		  vm::simple-array-double-double-float-type)
    (bit 0 1 vm:simple-bit-vector-type)
    ((unsigned-byte 2) 0 2 vm:simple-array-unsigned-byte-2-type)
    ((unsigned-byte 4) 0 4 vm:simple-array-unsigned-byte-4-type)
    ((unsigned-byte 8) 0 8 vm:simple-array-unsigned-byte-8-type)
    ((unsigned-byte 16) 0 16 vm:simple-array-unsigned-byte-16-type)
    ((unsigned-byte 32) 0 32 vm:simple-array-unsigned-byte-32-type)
    ((signed-byte 8) 0 8 vm:simple-array-signed-byte-8-type)
    ((signed-byte 16) 0 16 vm:simple-array-signed-byte-16-type)
    ((signed-byte 30) 0 32 vm:simple-array-signed-byte-30-type)
    ((signed-byte 32) 0 32 vm:simple-array-signed-byte-32-type)
    ((complex single-float) #C(0.0f0 0.0f0) 64
     vm:simple-array-complex-single-float-type)
    ((complex double-float) #C(0.0d0 0.0d0) 128
     vm:simple-array-complex-double-float-type)
    #+long-float
    ((complex long-float) #C(0.0l0 0.0l0) #+x86 192 #+sparc 256
     vm:simple-array-complex-long-float-type)
    #+double-double
    ((complex double-double-float) #C(0.0w0 0.0w0) 256
     vm::simple-array-complex-double-double-float-type)
    (t 0 32 vm:simple-vector-type)))

;;; MAKE-ARRAY  --  source-transform.
;;;
;;; The integer type restriction on the length assures that it will be a
;;; vector.  The lack of adjustable, fill-pointer, and displaced-to keywords
;;; assures that it will be simple.
;;;
(deftransform make-array ((length &key initial-element element-type)
			  (integer &rest *))
  (let* ((eltype (cond ((not element-type) t)
		       ((not (constant-continuation-p element-type))
			(give-up "Element-Type is not constant."))
		       (t
			(continuation-value element-type))))
	 (len (if (constant-continuation-p length)
		  (continuation-value length)
		  '*))
	 (spec `(simple-array ,eltype (,len)))
	 (eltype-type (specifier-type eltype)))
    (multiple-value-bind
	(default-initial-element element-size typecode)
	(dolist (info array-info
		      (give-up "Cannot open-code creation of ~S" spec))
	  (when (csubtypep eltype-type (specifier-type (car info)))
	    (return (values-list (cdr info)))))
      (let* ((nwords-form
	      (if (>= element-size vm:word-bits)
		  `(* length ,(/ element-size vm:word-bits))
		  (let ((elements-per-word (/ 32 element-size)))
		    `(truncate (+ length
				  ,(if (eq 'vm:simple-string-type typecode)
				       elements-per-word
				       (1- elements-per-word)))
			       ,elements-per-word))))
	     (constructor
	      `(truly-the ,spec
			  (allocate-vector ,typecode length ,nwords-form))))
	(values
	 (cond ((and default-initial-element
		     (or (null initial-element)
			 (and (constant-continuation-p initial-element)
			      (eql (continuation-value initial-element)
				   default-initial-element))))
		(unless (csubtypep (ctype-of default-initial-element)
				   eltype-type)
		  (compiler-note "Default initial element ~s is not a ~s."
				 default-initial-element eltype))
		constructor)
	       (t
		`(truly-the ,spec (fill ,constructor initial-element))))
	 '((declare (type index length))))))))

;;; MAKE-ARRAY  --  transform.
;;;
;;; The list type restriction does not assure that the result will be a
;;; multi-dimensional array.  But the lack of 
;;; 
(deftransform make-array ((dims &key initial-element element-type)
			  (list &rest *))
  (unless (or (null element-type) (constant-continuation-p element-type))
    (give-up "Element-type not constant; cannot open code array creation")) 
  (unless (constant-continuation-p dims)
    (give-up "Dimension list not constant; cannot open code array creation"))
  (let ((dims (continuation-value dims)))
    (unless (every #'integerp dims)
      (give-up "Dimension list contains something other than an integer: ~S"
	       dims))
    (if (= (length dims) 1)
	`(make-array ',(car dims)
		     ,@(when initial-element
			 '(:initial-element initial-element))
		     ,@(when element-type
			 '(:element-type element-type)))
	(let* ((total-size (reduce #'* dims))
	       (rank (length dims))
	       (spec `(simple-array
		       ,(cond ((null element-type) t)
			      ((constant-continuation-p element-type)
			       (continuation-value element-type))
			      (t '*))
			   ,(make-list rank :initial-element '*))))
	  `(let ((header (make-array-header vm:simple-array-type ,rank)))
	     (setf (%array-fill-pointer header) ,total-size)
	     (setf (%array-fill-pointer-p header) nil)
	     (setf (%array-available-elements header) ,total-size)
	     (setf (%array-data-vector header)
		   (make-array ,total-size
			       ,@(when element-type
				   '(:element-type element-type))
			       ,@(when initial-element
				   '(:initial-element initial-element))))
	     (setf (%array-displaced-p header) nil)
	     ,@(let ((axis -1))
		 (mapcar #'(lambda (dim)
			     `(setf (%array-dimension header ,(incf axis))
				    ,dim))
			 dims))
	     (truly-the ,spec header))))))


;;;; Random properties of arrays.

;;; Transforms for various random array properties.  If the property is know
;;; at compile time because of a type spec, use that constant value.

;;; ARRAY-RANK  --  transform.
;;;
;;; If we can tell the rank from the type info, use it instead.
;;;
(deftransform array-rank ((array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (if (not (listp dims))
	  (give-up "Array rank not known at compile time: ~S" dims)
	  (length dims)))))

;;; ARRAY-DIMENSION  --  transform.
;;;
;;; If we know the dimensions at compile time, just use it.  Otherwise, if
;;; we can tell that the axis is in bounds, convert to %array-dimension
;;; (which just indirects the array header) or length (if it's simple and a
;;; vector).
;;;
(deftransform array-dimension ((array axis)
			       (array index))
  (unless (constant-continuation-p axis)
    (give-up "Axis not constant."))
  (let ((array-type (continuation-type array))
	(axis (continuation-value axis)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (unless (listp dims)
	(give-up
	 "Array dimensions unknown, must call array-dimension at runtime."))
      (unless (> (length dims) axis)
	(abort-transform "Array has dimensions ~S, ~D is too large."
			 dims axis))
      (let ((dim (nth axis dims)))
	(cond ((integerp dim)
	       dim)
	      ((= (length dims) 1)
	       (ecase (array-type-complexp array-type)
		 ((t)
		  '(%array-dimension array 0))
		 ((nil)
		  '(length array))
		 ((:maybe *)
		  (give-up "Can't tell if array is simple."))))
	      (t
	       '(%array-dimension array axis)))))))

;;; LENGTH  --  transform.
;;;
;;; If the length has been declared and it's simple, just return it.
;;;
(deftransform length ((vector)
		      ((simple-array * (*))))
  (let ((type (continuation-type vector)))
    (unless (array-type-p type)
      (give-up))
    (let ((dims (array-type-dimensions type)))
      (unless (and (listp dims) (integerp (car dims)))
	(give-up "Vector length unknown, must call length at runtime."))
      (car dims))))

;;; LENGTH  --  transform.
;;;
;;; All vectors can get their length by using vector-length.  If it's simple,
;;; it will extract the length slot from the vector.  It it's complex, it will
;;; extract the fill pointer slot from the array header.
;;;
(deftransform length ((vector) (vector))
  '(vector-length vector))


;;; If a simple array with known dimensions, then vector-length is a
;;; compile-time constant.
;;;
(deftransform vector-length ((vector) ((simple-array * (*))))
  (let ((vtype (continuation-type vector)))
    (if (array-type-p vtype)
	(let ((dim (first (array-type-dimensions vtype))))
	  (when (eq dim '*) (give-up))
	  dim)
	(give-up))))


;;; ARRAY-TOTAL-SIZE  --  transform.
;;;
;;; Again, if we can tell the results from the type, just use it.  Otherwise,
;;; if we know the rank, convert into a computation based on array-dimension.
;;; We can wrap a truly-the index around the multiplications because we know
;;; that the total size must be an index.
;;; 
(deftransform array-total-size ((array)
				(array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (unless (listp dims)
	(give-up "Can't tell the rank at compile time."))
      (if (member '* dims)
	  (do ((form 1 `(truly-the index
				   (* (array-dimension array ,i) ,form)))
	       (i 0 (1+ i)))
	      ((= i (length dims)) form))
	  (reduce #'* dims)))))

;;; ARRAY-HAS-FILL-POINTER-P  --  transform.
;;;
;;; Only complex vectors have fill pointers.
;;;
(deftransform array-has-fill-pointer-p ((array))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up))
    (let ((dims (array-type-dimensions array-type)))
      (if (and (listp dims) (not (= (length dims) 1)))
	  nil
	  (ecase (array-type-complexp array-type)
	    ((t)
	     t)
	    ((nil)
	     nil)
	    (:maybe
	     (give-up "Array type ambiguous; must call ~
	              array-has-fill-pointer-p at runtime.")))))))

;;; %CHECK-BOUND  --  transform.
;;; 
;;; Primitive used to verify indicies into arrays.  If we can tell at
;;; compile-time or we are generating unsafe code, don't bother with the VOP.
;;;
(deftransform %check-bound ((array dimension index))
  (unless (constant-continuation-p dimension)
    (give-up))
  (let ((dim (continuation-value dimension)))
    `(the (integer 0 (,dim)) index)))
;;;
(deftransform %check-bound ((array dimension index) * *
			    :policy (and (> speed safety) (= safety 0)))
  'index)


;;; WITH-ROW-MAJOR-INDEX  --  internal.
;;;
;;; Handy macro for computing the row-major index given a set of indices.  We
;;; wrap each index with a call to %check-bound to assure that everything
;;; works out correctly.  We can wrap all the interior arith with truly-the
;;; index because we know the the resultant row-major index must be an index.
;;; 
(eval-when (compile eval)
;;;
(defmacro with-row-major-index ((array indices index &optional new-value)
				&rest body)
  `(let (n-indices dims)
     (dotimes (i (length ,indices))
       (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
       (push (make-symbol (format nil "DIM-~D" i)) dims))
     (setf n-indices (nreverse n-indices))
     (setf dims (nreverse dims))
     `(lambda (,',array ,@n-indices ,@',(when new-value (list new-value)))
	(let* (,@(let ((,index -1))
		   (mapcar #'(lambda (name)
			       `(,name (array-dimension ,',array
							,(incf ,index))))
			   dims))
	       (,',index
		,(if (null dims)
		     0
		     (do* ((dims dims (cdr dims))
			   (indices n-indices (cdr indices))
			   (last-dim nil (car dims))
			   (form `(%check-bound ,',array
						,(car dims)
						,(car indices))
				 `(truly-the index
					     (+ (truly-the index
							   (* ,form
							      ,last-dim))
						(%check-bound
						 ,',array
						 ,(car dims)
						 ,(car indices))))))
			  ((null (cdr dims)) form)))))
	  ,',@body))))
;;;
); eval-when

;;; ARRAY-ROW-MAJOR-INDEX  --  transform.
;;;
;;; Just return the index after computing it.
;;;
(deftransform array-row-major-index ((array &rest indices))
  (with-row-major-index (array indices index)
    index))



;;;; Array accessors:

;;; SVREF, %SVSET, SCHAR, %SCHARSET, CHAR,
;;; %CHARSET, SBIT, %SBITSET, BIT, %BITSET
;;;   --  source transforms.
;;;
;;; We convert all typed array accessors into aref and %aset with type
;;; assertions on the array.
;;;
(macrolet ((frob (reffer setter type)
	     `(progn
		(def-source-transform ,reffer (a &rest i)
		  (if (byte-compiling)
		      (values nil t)
		      `(aref (the ,',type ,a) ,@i)))
		(def-source-transform ,setter (a &rest i)
		  (if (byte-compiling)
		      (values nil t)
		      `(%aset (the ,',type ,a) ,@i))))))
  (frob sbit %sbitset (simple-array bit))
  (frob bit %bitset (array bit)))

(macrolet ((frob (reffer setter type)
	     `(progn
		(def-source-transform ,reffer (a i)
		  (if (byte-compiling)
		      (values nil t)
		      `(aref (the ,',type ,a) ,i)))
		(def-source-transform ,setter (a i v)
		  (if (byte-compiling)
		      (values nil t)
		      `(%aset (the ,',type ,a) ,i ,v))))))
  (frob svref %svset simple-vector)
  (frob schar %scharset simple-string)
  (frob char %charset string))

;;; AREF, %ASET  --  transform.
;;; 
;;; Convert into a data-vector-ref (or set) with the set of indices replaced
;;; with the an expression for the row major index.
;;; 
(deftransform aref ((array &rest indices))
  (with-row-major-index (array indices index)
    (data-vector-ref array index)))
;;; 
(deftransform %aset ((array &rest stuff))
  (let ((indices (butlast stuff)))
    (with-row-major-index (array indices index new-value)
      (data-vector-set array index new-value))))

;;; ROW-MAJOR-AREF, %SET-ROW-MAJOR-AREF  --  transform.
;;;
;;; Just convert into a data-vector-ref (or set) after checking that the
;;; index is inside the array total size.
;;;
(deftransform row-major-aref ((array index))
  `(data-vector-ref array (%check-bound array (array-total-size array) index)))
;;; 
(deftransform %set-row-major-aref ((array index new-value))
  `(data-vector-set array
		    (%check-bound array (array-total-size array) index)
		    new-value))


;;;; Bit-vector array operation canonicalization:
;;;
;;;    We convert all bit-vector operations to have the result array specified.
;;; This allows any result allocation to be open-coded, and eliminates the need
;;; for any VM-dependent transforms to handle these cases.

(dolist (fun '(bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1
		       bit-andc2 bit-orc1 bit-orc2))
  ;;
  ;; Make a result array if result is NIL or unsupplied.
  (deftransform fun ((bit-array-1 bit-array-2 &optional result-bit-array)
		     '(bit-vector bit-vector &optional null) '*
		     :eval-name t  :policy (>= speed space))
    `(,fun bit-array-1 bit-array-2
	   (make-array (array-dimension bit-array-1 0) :element-type 'bit)))
  ;;
  ;; If result its T, make it the first arg.
  (deftransform fun ((bit-array-1 bit-array-2 result-bit-array)
		     '(bit-vector bit-vector (member t)) '*
		     :eval-name t)
    `(,fun bit-array-1 bit-array-2 bit-array-1)))

;;; Similar for BIT-NOT, but there is only one arg...
;;;
(deftransform bit-not ((bit-array-1 &optional result-bit-array)
		       (bit-vector &optional null) *
		       :policy (>= speed space))
  '(bit-not bit-array-1 
	    (make-array (length bit-array-1) :element-type 'bit)))
;;;
(deftransform bit-not ((bit-array-1 result-bit-array)
		       (bit-vector (constant-argument t)))
  '(bit-not bit-array-1 bit-array-1))


;;; ARRAY-HEADER-P  --  transform.
;;;
;;; Pick off some constant cases.
;;;
(deftransform array-header-p ((array) (array))
  (let ((type (continuation-type array)))
    (declare (optimize (safety 3)))
    (unless (array-type-p type)
      (give-up))
    (let ((dims (array-type-dimensions type)))
      (cond ((csubtypep type (specifier-type '(simple-array * (*))))
	     ;; No array header.
	     nil)
	    ((and (listp dims) (> (length dims) 1))
	     ;; Multi-dimensional array, will have a header.
	     t)
	    (t
	     (give-up))))))
