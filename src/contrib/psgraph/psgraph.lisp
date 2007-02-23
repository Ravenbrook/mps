;;; -*- Mode: Lisp; Package: PSGRAPH -*-
;;;
;;; ****************************************************************
;;; PostScript DAG Grapher *****************************************
;;; ****************************************************************
;;; Written by Joseph Bates, CMU CSD, March 1988. jbates+@cs.cmu.edu
;;;
;;; The PSGrapher is a set of Lisp routines that can be called to produce
;;; PostScript commands that display a directed acyclic graph.
;;;
;;; Modifications History:
;;;
;;; AUG 97    dtc     Added declarations to help the CMUCL compiler.
;;;		      Only the *psnode-index* hash should use the user
;;;		      supplied hash test, all the others are internal.
;;;		      Make the output stream a required argument rather
;;;		      than sending output to *standard-output*.
;;;		      Add a shrink :width optional to scale the graph to
;;;		      fit across one page.
;;;		      Add optional same-height argument to allocate the same
;;;		      height for each node - can give nicer graphs.
;;;		      Misc. style changes: less use of special variables, etc.
;;; JAN 92    mkant   Modified to be package compatible with CLtL2 lisps.
;;; JULY 90   mkant   Fixed shrink mode so that it scales x and y
;;;                   axes uniformly (e.g., aspect ratio = 1).
;;; MAY 90    Chiles  Made exported specials have stars.
;;; APR 90    Skef    Now lives in PSGRAPH package (instead of USER,
;;;                   or *package*...) with user-tweakable stuff exported.
;;;                   Node equivalence function can now be specified as
;;;                   EQ, EQUAL, or EQUALP.
;;; DEC 88    Bates   Modified to include optional insert parameter to psgraph.
;;; MAR 88    Bates   File created.
;;;
(eval-when (compile load eval)
  (defpackage "PSGRAPH"
    (:use "COMMON-LISP")
    (:export "*EXTRA-X-SPACING*" "*EXTRA-Y-SPACING*" "*FONTNAME*"
	     "*FONTSIZE*" "*SECOND-FONTNAME*" "*SECOND-FONTSIZE*"
	     "*BOXGRAY*" "*BOXKIND*" "*EDGEWIDTH*" "*EDGEGRAY*"
	     "*EDGECAP*"  "*TEXTGRAY*" "*PAGEHEIGHT*" "*PAGEWIDTH*"
	     "*BOXRADIUS*" "*BOXEDGE*" "*CHUNKSIZE*" "PSGRAPH")))

(in-package "PSGRAPH")

(defstruct psnode
  name
  (info '() :type list)
  (children '() :type list)
  (parents '() :type list)
  (appears-in-top-sort nil :type (member nil t))
  (children-appear-in-top-sort nil :type (member nil t))
  (yvalue 0 :type fixnum)
  (height 0 :type fixnum))

(declaim (fixnum *extra-x-spacing* *extra-y-spacing*))
(defvar *extra-x-spacing* 20)
(defvar *extra-y-spacing* 6)
(declaim (fixnum *fontsize* *second-fontsize*))
(defvar *fontname* "Helvetica")
(defvar *fontsize* 10)
(defvar *second-fontname* "Helvetica-Oblique")
(defvar *second-fontsize* 8)
(defvar *boxgray* ".2")      ;; 0 is black, 1 is white
(defvar *boxkind* "stroke")  ;; stroke for outline, fill for solid
(defvar *edgewidth* ".5")    ;; .5 is thin lines, 1 is fairly thick
(defvar *edgegray* ".2")     ;; dark, but not solid black, lines
(defvar *edgecap* "1")       ;; round the line caps
(defvar *textgray* "0")      ;; solid black text
(declaim (fixnum *pageheight* *pagewidth*))
(defvar *pageheight* 720)
(defvar *pagewidth* (+ (* 7 72) 36))
(declaim (fixnum *boxradius* *boxedge*))
(defvar *boxradius* (floor *fontsize* 2))
(defvar *boxedge* (floor *fontsize* 4))
(declaim (fixnum *chunksize*))
(defvar *chunksize* 400)

(declaim (fixnum *num-psnodes*))
(defvar *num-psnodes* 0)
(declaim (type (simple-array t (*)) *narray*))
(defvar *narray*)
(defvar *psnode-index*)
(defvar *top-sort*)
(defvar *ancestor-cache*)

(defun psgraph (stream root childf infof
		&optional (shrink nil) (insert nil) (test #'equal)
		(same-height nil))
  (declare (stream stream)
	   (function childf infof)
	   (type (or symbol function) test))
  (unless (member test (list 'eq 'equal 'equalp #'eq #'equal #'equalp))
    (error "Test must be a function suitable to hand to Make-Hash-Table."))
  (let ((maximum-y 0)
	(minimum-y 0)
	psnodes-at-y
	(*narray* (make-array 5000 :element-type 'psnode))
	(*num-psnodes* 0)
	(*psnode-index*
	 (make-hash-table :test test :size 500 :rehash-size 2.0))
	(*ancestor-cache*
	 (make-hash-table :test #'equal :size 1000 :rehash-size 2.0))
	(*top-sort* nil))
    (declare (fixnum maximum-y minimum-y))
    
    (when insert (setf shrink t))
    
    ;; walk the graph computing node info
    (walk-graph root childf infof)
    (dotimes (index *num-psnodes*)
      (declare (fixnum index))
      (let ((psnode (aref *narray* index)))
	(setf (psnode-parents psnode) (nreverse (psnode-parents psnode)))
	(setf (psnode-children psnode) (nreverse (psnode-children psnode)))))
  
    ;; topological sort the graph
    (top-sort-node 0)
    (setq *top-sort* (nreverse *top-sort*))

    ;; declare this as a PostScript file
    (format stream "%!PS-Adobe-1.0~%")

    ;; this is required for the Apple LaserWriter (it likes to know
    ;; the page ordering so that it can print pages upside down).  It
    ;; is best not to confuse the LaserWriter when inserting things
    ;; into a other documents.
    (when (not insert)
      (format stream "%%Page: ? ?~%"))

    ;; define global functions
    (format stream
     "/max {2 copy lt {exch} if pop} def~@
      /min {2 copy gt {exch} if pop} def~@
      /inch {72 mul} def~@
      /drawbox~@
       {/height exch def~@
        /width exch def~@
        /y exch def~@
        /x exch def~@
        gsave newpath~@
        x y boxradius add moveto~@
        x y height add x width add y height add boxradius arcto pop pop pop pop~@
        x width add y height add x width add y boxradius arcto pop pop pop pop~@
        x width add y x y boxradius arcto pop pop pop pop~@
        x y x y height add boxradius arcto pop pop pop pop~@
        boxgray setgray boxkind grestore~@
       } def~%")
    
    ;; declare arrays
    (format stream "/xarray ~D array def~%" *num-psnodes*)
    (format stream "/widtharray ~D array def~%" *num-psnodes*)

    ;; define global settings
    (format stream "/leftmargin 36 def~@
                    /botmargin 36 def~%")
    (format stream "/pagewidth ~D def~%" *pagewidth*)
    (format stream "/pageheight ~D def~%" *pageheight*)
    (format stream "/boxradius ~D def ~%" *boxradius*)
    (format stream "/boxedge ~D def ~%" *boxedge*)
    (format stream "/boxgray ~A def ~%" *boxgray*)
    (format stream "/boxkind {~A} def~%" *boxkind*)

    ;; compute width of each node
    (format stream "/~A findfont ~D scalefont setfont~%" *fontname* *fontsize*)
    (dotimes (index *num-psnodes*)
      (declare (fixnum index))
      (format stream "widtharray ~D (~A) stringwidth pop put~%"
	      index (car (psnode-info (aref *narray* index)))))
    (format stream "/~A findfont ~D scalefont setfont~%"
	    *second-fontname* *second-fontsize*)
    (dotimes (index *num-psnodes*)
      (declare (fixnum index))
      (format stream "widtharray ~D get~%" index)
      (dolist (info (cdr (psnode-info (aref *narray* index))))
	(format stream "(~A) stringwidth pop max~%" info))
      (format stream "~D add widtharray exch ~D exch put~%"
	      (* 2 *boxedge*) index))
    ;; compute x location of each node
    (format stream "xarray 0 0 put~%")
    (dolist (index (cdr *top-sort*))
      (let ((parents (psnode-parents (aref *narray* index))))
	(format stream "xarray ~D get widtharray ~D get add~%"
		(car parents) (car parents))
	(dolist (parent (cdr parents))
	  (format stream "xarray ~D get widtharray ~D get add max~%"
		  parent parent))
	(format stream "~D add xarray exch ~D exch put~%"
		*extra-x-spacing* index)))
    ;; compute maximum x used
    (format stream "/maximum-x 0~@
		    0 1 ~d~@
		     {/i exch def~@
                      xarray i get widtharray i get add max~@
		     } for~@
		    def~%"
	    (1- *num-psnodes*))
    
    ;; compute height of each node, and the maximum height.
    (let ((max-height 0))
      (dotimes (index *num-psnodes*)
	(declare (fixnum index))
	(let* ((psnode (aref *narray* index))
	       (height (+ (* 2 *boxedge*) *extra-y-spacing* *fontsize*
			  (* (- (length (psnode-info psnode)) 1)
			     *second-fontsize*))))
	  (setf (psnode-height psnode) height)
	  (when (> height max-height)
	    (setf max-height height))))
      
      ;; compute y location of each node and maximum and minimum y used
      (setq psnodes-at-y (make-hash-table :test #'eql
					  :size (* *num-psnodes* *fontsize*)
					  :rehash-size 2.0))
      (let ((currenty 0))
	(declare (fixnum currenty))
	(dolist (index (reverse *top-sort*))
	  (let ((desired-y 0))
	    (declare (fixnum desired-y))
	    (cond ((null (psnode-children (aref *narray* index)))
		   (setf desired-y currenty)
		   (incf currenty (if same-height
				      max-height
				      (psnode-height (aref *narray* index)))))
		  (t
		   (let ((children (psnode-children (aref *narray* index)))
			 (ysum 0))
		     (declare (fixnum ysum))
		     (dolist (child children)
		       (incf ysum (psnode-yvalue (aref *narray* child))))
		     (setq desired-y (floor ysum (length children))))))
	    ;; We may not be able to put the node at the desired y.  If
	    ;; there is another node that overlaps in the y direction
	    ;; and that node is neither a parent* nor child* (hence its
	    ;; x location may overlap this one) then we have to choose
	    ;; another y value -- we choose the nearest (up or down)
	    ;; location so that there is no possible overlap in y or x.
	    (let* ((height (if same-height
			       max-height
			       (psnode-height (aref *narray* index))))
		   (related (make-hash-table :test #'equal
					     :size (+ 50 *num-psnodes*)
					     :rehash-size 2.0))
		   collision
		   (upward-bot desired-y)
		   (upward-top upward-bot)
		   (downward-top (- (+ desired-y height) 1))
		   (downward-bot downward-top))
	      (declare (fixnum upward-bot upward-top
			       downward-top downward-bot))
	      (loop
	       ;; check upward-top for collision
	       (setq collision nil)
	       (dolist (n (gethash upward-top psnodes-at-y))
		 (let ((r (gethash n related)))
		   (when (null r)
		     (setf r (related-classes n index))
		     (setf (gethash n related) r))
		   (when (eql r 'no)
		     (setq collision t)
		     (return))))
	     
	       ;; if no collision and big enough space then we win
	       (incf upward-top)
	       (when (and (not collision) (= (- upward-top upward-bot) height))
		 (setf desired-y upward-bot)
		 (return))
	       
	       (when collision
		 (setf upward-bot upward-top))
	       
	       ;; check downward-bot for collision
	       (setq collision nil)
	       (dolist (n (gethash downward-bot psnodes-at-y))
		 (let ((r (gethash n related)))
		   (when (null r)
		     (setf r (related-classes n index))
		     (setf (gethash n related) r))
		   (when (eql r 'no)
		     (setq collision t)
		     (return))))
	       
	       ;; if no collision and big enough space then we win
	       (decf downward-bot)
	       (when (and (not collision)
			  (= (- downward-top downward-bot) height))
		 (setf desired-y (+ 1 downward-bot))
		 (return))
	       
	       (when collision
		 (setf downward-top downward-bot)))
	      
	      ;; add our name to psnodes-at-y table
	      (dotimes (i height)
		(declare (fixnum i))
		(push index (gethash (+ i desired-y) psnodes-at-y)))
	      
	      (setf (psnode-yvalue (aref *narray* index)) desired-y))
	    
	    (setf minimum-y (min minimum-y
				 (psnode-yvalue (aref *narray* index))))
	    (setf maximum-y
		  (max maximum-y (+ (psnode-yvalue (aref *narray* index))
				    (if same-height
					max-height
					(psnode-height (aref *narray* index)))))))))
      (when same-height
	;; Centre nodes vertically within the allocated max-height.
	(dotimes (index *num-psnodes*)
	  (declare (fixnum index))
	  (let* ((psnode (aref *narray* index))
		 (height (psnode-height psnode)))
	    (incf (psnode-yvalue psnode)
		  (- (floor max-height 2) (floor height 2)))))))

    ;; compute y-offset to center graph vertically
    (let* ((rows (ceiling (- maximum-y minimum-y) *pageheight*))
	   (y-offset
	    (if shrink
		(- 0 minimum-y)
		(- (floor (- (the fixnum (* rows *pageheight*))
			     (- maximum-y minimum-y)) 2)
		   minimum-y))))
      (declare (fixnum rows y-offset))

      ;; create dictionary big enough to hold all the procedures
      ;; defined below and make it a current dictionary
      (format stream "~D dict begin~%"
	      (+ (* 4 (+ *num-psnodes* (ceiling *num-psnodes*
						*chunksize*))) 50))
      
      ;; define procedures to display the background box for each node
      (dotimes (index *num-psnodes*)
	(format stream "/box~D {~%" index)
	(format stream "xarray ~D get ~D widtharray ~D get ~D drawbox~%"
		index
		(+ y-offset (psnode-yvalue (aref *narray* index))
		   (floor *extra-y-spacing* 2))
		index
		(- (psnode-height (aref *narray* index)) *extra-y-spacing*))
	(format stream "} def~%"))

      ;; define procedures to display the text info for each node
      (dotimes (index *num-psnodes*)
	(declare (fixnum index))
	(let ((yvalue (+ y-offset (floor *extra-y-spacing* 2)
			 (floor *fontsize* 5) *boxedge*
			 (psnode-yvalue (aref *narray* index))
			 (the fixnum
			      (* *second-fontsize*
				 (- (length (psnode-info
					     (aref *narray* index))) 1))))))
	  (declare (fixnum yvalue))
	  (format stream "/text~D {xarray ~D get boxedge add ~
			  ~D moveto (~A) show} def~%"
		  index index yvalue (car (psnode-info (aref *narray* index))))
	  (when (not (null (cdr (psnode-info (aref *narray* index)))))
	    (format stream "/secondtext~D {~%" index)
	    (dolist (info (cdr (psnode-info (aref *narray* index))))
	      (setq yvalue (- yvalue *second-fontsize*))
	      (format stream "xarray ~D get boxedge add ~D moveto (~A) show~%"
		      index yvalue info))
	    (format stream "} def~%"))))
      
      ;; define procedures to display the edges leading into each node
      (dotimes (index *num-psnodes*)
	(format stream "/edge~D {newpath~%" index)
	(dolist (parent (psnode-parents (aref *narray* index)))
	  (format stream "xarray ~D get widtharray ~D get add ~D moveto~%"
		parent parent
		(+ (psnode-yvalue (aref *narray* parent))
		   (floor (psnode-height (aref *narray* parent)) 2)
		   y-offset))
	  (format stream "xarray ~D get ~D lineto~%"
		  index
		  (+ (psnode-yvalue (aref *narray* index))
		     (floor (psnode-height (aref *narray* index)) 2)
		     y-offset)))
	(format stream "stroke } def~%"))

      ;; Define procedures to display chunks of boxes, text, and
      ;; edges.  We limit each chunk to at most *chunksize* calls, to
      ;; avoid overflowing the Postscript operand stack.
      (dotimes (index *num-psnodes*)
	(declare (fixnum index))
	(when (zerop (mod index *chunksize*))
	  (format stream "/boxchunk~D {~%" (floor index *chunksize*)))
	(format stream "box~D~%" index)
	(when (or (= (mod index *chunksize*) (- *chunksize* 1))
		  (= index (- *num-psnodes* 1)))
	  (format stream "} def~%")))
      (dotimes (index *num-psnodes*)
	(declare (fixnum index))
	(when (zerop (mod index *chunksize*))
	  (format stream "/textchunk~D {~%" (floor index *chunksize*)))
	(format stream "text~D~%" index)
	(when (or (= (mod index *chunksize*) (- *chunksize* 1))
		  (= index (- *num-psnodes* 1)))
	  (format stream "} def~%")))
      (dotimes (index *num-psnodes*)
	(declare (fixnum index))
	(when (zerop (mod index *chunksize*))
	  (format stream "/secondtextchunk~D {~%" (floor index *chunksize*)))
	(when (cdr (psnode-info (aref *narray* index)))
	  (format stream "secondtext~D~%" index))
	(when (or (= (mod index *chunksize*) (- *chunksize* 1))
		  (= index (- *num-psnodes* 1)))
	  (format stream "} def~%")))
      (dotimes (index *num-psnodes*)
	(declare (fixnum index))
	(when (zerop (mod index *chunksize*))
	  (format stream "/edgechunk~D {~%" (floor index *chunksize*)))
	(format stream "edge~D~%" index)
	(when (or (= (mod index *chunksize*) (- *chunksize* 1))
		  (= index (- *num-psnodes* 1)))
	  (format stream "} def~%")))

      ;; Define procedure to display entire graph.  First do the
      ;; boxes, then the edges, then the text.
      (format stream "/drawgraph { gsave~%")
      (dotimes (i (ceiling *num-psnodes* *chunksize*))
	(declare (fixnum i))
	(format stream "boxchunk~D~%" i))
      (format stream "~A setlinewidth~%" *edgewidth*)
      (format stream "~A setlinecap~%" *edgecap*)
      (format stream "~A setgray~%" *edgegray*)
      (dotimes (i (ceiling *num-psnodes* *chunksize*))
	(declare (fixnum i))
	(format stream "edgechunk~D~%" i))
      (format stream "~A setgray~%" *textgray*)
      (format stream "/~A findfont ~D scalefont setfont~%"
	      *fontname* *fontsize*)
      (dotimes (i (ceiling *num-psnodes* *chunksize*))
	(declare (fixnum i))
	(format stream "textchunk~D~%" i))
      (format stream "/~A findfont ~D scalefont setfont~%"
	      *second-fontname* *second-fontsize*)
      (dotimes (i (ceiling *num-psnodes* *chunksize*))
	(declare (fixnum i))
	(format stream "secondtextchunk~D~%" i))
      (format stream "grestore } def~%")
      
      ;; show the virtual page in as many actual pages as needed
      (cond ((eq shrink :width)
	     ;; shrink the width to fit on a page.
	     (format stream
		     "newpath~@
		        leftmargin botmargin moveto~@
		        0 pageheight rlineto~@
		        pagewidth 0 rlineto~@
		        0 pageheight neg rlineto~@
		      closepath clip~@
		      leftmargin botmargin translate~@
		      0 1 ~d~@
		       {/rowcount exch def~@
		         gsave~@
		         0 pageheight rowcount mul neg translate~@
			 pagewidth dup maximum-x max div dup 1 scale~@
		         drawgraph~@
	                 0.5 1 scale~@
		         gsave showpage grestore~@
		         grestore~@
		       } for~@
                      end~%"
		     (1- rows)))
	    (shrink
	     ;; shrink the output to fit on one page
	     (format stream "leftmargin botmargin translate~@
			     pagewidth dup maximum-x max div ~
			     pageheight dup ~D max div min dup scale~%"
		     (- maximum-y minimum-y))
	     (if insert
		 (format stream "drawgraph end~%")
		 (format stream "drawgraph showpage end~%")))
	    (t
	     (format stream
		     "/printposter~@
		       {/rows exch def~@
		        /columns exch def~@
		        newpath~@
		          leftmargin botmargin moveto~@
		          0 pageheight rlineto~@
		          pagewidth 0 rlineto~@
		          0 pageheight neg rlineto~@
		        closepath clip~@
		        leftmargin botmargin translate~@
		        0 1 rows 1 sub~@
		         {/rowcount exch def~@
		          0 1 columns 1 sub~@
		           {/colcount exch def~@
		            gsave~@
		             pagewidth colcount mul neg~@
		             pageheight rowcount mul neg~@
		             translate~@
		             drawgraph~@
		             gsave showpage grestore~@
		            grestore~@
		           } for~@
		         } for~@
		       } def~%")
	     (format stream "maximum-x pagewidth div ceiling ~
                             ~D printposter end~%"
		     rows))))))

(defun walk-graph (root child-function info-function)
  (declare (function child-function info-function))
  (let ((root-index *num-psnodes*)
	(child-names (funcall child-function root))
	(psnode (make-psnode :name root :info (funcall info-function root))))
    (incf *num-psnodes*)
    (setf (gethash root *psnode-index*) root-index)
    ;; Grow *narray* if necessary.
    (when (>= root-index (length *narray*))
      (setf *narray* (adjust-array *narray* (ceiling (* *num-psnodes* 1.5))
				   :element-type t)))
    (setf (aref *narray* root-index) psnode)
    (dolist (child child-names)
      (let ((child-index (gethash child *psnode-index*)))
	(cond (child-index
	       (push child-index (psnode-children psnode))
	       (push root-index (psnode-parents (aref *narray* child-index))))
	      (t
	       (let ((child-index
		      (walk-graph child child-function info-function)))
		 (push child-index (psnode-children psnode))
		 (push root-index
		       (psnode-parents (aref *narray* child-index))))))))
    root-index))

(defun top-sort-node (index)
  (let ((psnode (aref *narray* index)))
    (unless (psnode-appears-in-top-sort psnode)
      ;; make sure the parents are processed
      (dolist (parent (psnode-parents psnode))
	(top-sort-parent parent))
      ;; add this node to *top-sort*
      (push index *top-sort*)
      (setf (psnode-appears-in-top-sort psnode) t))
    (unless (psnode-children-appear-in-top-sort psnode)
      (dolist (child (psnode-children psnode))
	(top-sort-node child))
      (setf (psnode-children-appear-in-top-sort psnode) t))))
  
(defun top-sort-parent (index)
  (let ((psnode (aref *narray* index)))
    (unless (psnode-appears-in-top-sort psnode)
      ;; make sure the parents are processed
      (dolist (parent (psnode-parents psnode))
	(top-sort-parent parent))
      ;; add this node to *top-sort*
      (push index *top-sort*)
      (setf (psnode-appears-in-top-sort psnode) t))))


(defun related-classes (x y)
  (cond ((ancestor x y) 'yes)
	((ancestor y x) 'yes)
	(t 'no)))

(defun ancestor (x y)
  (let ((cached-value (gethash (list x y) *ancestor-cache*)))
    (cond (cached-value (car cached-value))
	  (t
	   (setq cached-value
		 (if (equal x y)
		     t
		     (some #'(lambda (child) (ancestor child y))
			   (psnode-children (aref *narray* x)))))
	   (setf (gethash (list x y) *ancestor-cache*) (list cached-value))
	   cached-value))))
