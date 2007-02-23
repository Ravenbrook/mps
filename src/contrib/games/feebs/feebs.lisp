;;; -*- Mode: Lisp; Package: Feebs; Log: feebs.log -*-
;;;
;;; Planet of the Feebs.
;;; A somewhat educational simulation game.
;;;
;;; Written by Skef Wholey.
;;; Modified by Scott Fahlman.
;;; Modified by Dan Kuokka.
;;; Modified by Jim Healy.

;;; Graphics ported to X11 by Fred Gilham 8-FEB-1998.

(defpackage "FEEBS"
  (:use "COMMON-LISP")
  (:shadow position)
  ;; Export everything we want the players to get their hands on.
  (:export *single-step* *delay* *number-of-feebs* *game-length*
	   *points-for-killing* *points-for-dying* *maze-i-size*
	   *maze-j-size* *flame-energy* *mushroom-energy*
	   *carcass-energy* *maximum-energy* *minimum-starting-energy*
	   *maximum-starting-energy* *number-of-mushrooms*
	   *number-of-mushroom-sites* *carcass-guaranteed-lifetime*
	   *carcass-rot-probability* *fireball-dissipation-probability*
	   *fireball-reflection-probability* *flame-recovery-probability*
	   *slow-feeb-noop-switch* *slow-feeb-noop-factor*
	   name facing i-position j-position peeking line-of-sight
	   energy-reserve score kills ready-to-fire aborted last-move
	   feeb-image-p feeb-image-name feeb-image-facing
	   fireball-image-p fireball-image-direction
	   my-square left-square right-square rear-square
	   list-parameter-settings
	   define-feeb feebs load-feebs north south east west))

(in-package "FEEBS")

(declaim (optimize (speed 3) (safety 1)))

;;; Macro for computing whether a random event has occurred, given the
;;; chance of occurrence as a ratio.

(defmacro chance (ratio)
  `(< (random (denominator ,ratio)) (numerator ,ratio)))

;;; Directions

(defconstant north 0)
(defconstant east 1)
(defconstant south 2)
(defconstant west 3)

;;; These control the user interface.

(defvar *single-step* nil
  "If non-null, wait for a mouse click at the end of each round.")

(defvar *delay* nil
  "If non-null, sleep this many seconds after each round.")

(defvar *continue* t
  "If null, quit the game immediately.")

(defvar *feep-dead-feebs* nil
  "If non-null, feep every-time a feep dies or is killed.")


;;; Parameters that affect strategy of the game.

(defvar *feeb-parameters* nil)

(defmacro def-feeb-parm (name value doc)
  `(progn
    (defvar ,name ,value ,doc)
    (pushnew ',name *feeb-parameters*)))

(defun list-parameter-settings ()
  (let ((settings nil))
    (dolist (parm *feeb-parameters*)
      (push (cons parm (symbol-value parm)) settings))
    settings))


;;; General game parameters:

(def-feeb-parm *number-of-feebs* 10
  "Number of feebs that will play in the game.")

(def-feeb-parm *game-length* 1000
  "Number of cycles in the simulation.")

(def-feeb-parm *slow-feeb-noop-switch* t
  "If non-null, each feeb has a chance of having its orders aborted in
  proportion to the time it takes to produce them.")

(def-feeb-parm *slow-feeb-noop-factor* .25
  "If *slow-feeb-noop-switch* is non-null, a feeb's orders will be aborted
  with probability equal to the product of this factor times the time
  taken by this feeb divided by the total time taken by all feebs this turn.")

(def-feeb-parm *feep-dead-feebs-volume* 75
  "An integer between -100 and 100 which determines the volume of the feep
   emitted when a feeb dies.  The softest volume is 0 and the loudest
   is 100.  Negative volumes are usually not heard.")

;;; Scoring:

(def-feeb-parm *points-for-killing* 1
  "Added to one's score for killing an opponent.")

(def-feeb-parm *points-for-dying* -2
  "Added to one's score for being killed or starving.")


;;; Characteristics of the maze:

(def-feeb-parm *maze-i-size* 32
  "Number of rows in the maze.")

(def-feeb-parm *maze-j-size* 32
  "Number of columns in the maze.")

(def-feeb-parm *number-of-mushrooms* 10
  "Average number of mushrooms in the maze at any given time.")

(def-feeb-parm *number-of-mushroom-sites* 0
  "Number of places at which mushrooms might grow.")


;;; Energies:

(def-feeb-parm *flame-energy* 10
  "Energy used when a feeb flames.")

(def-feeb-parm *mushroom-energy* 50
  "Energy gained when a mushroom is eaten.")

(def-feeb-parm *carcass-energy* 30
  "Energy gained by feeding on a carcass.")

(def-feeb-parm *maximum-energy* 200
  "The most energy a feeb can accumulate.")

(def-feeb-parm *minimum-starting-energy* 50
  "Smallest amount of energy a feeb will start with.")

(def-feeb-parm *maximum-starting-energy* 100
  "Greatest amount of energy a feeb will start with.")

;;; Carcasses:

(def-feeb-parm *carcass-guaranteed-lifetime* 3
  "Minimum number of turns a carcass will hang around.")

(def-feeb-parm *carcass-rot-probability* 1/3
  "Chance of a carcass rotting away each turn after its guaranteed lifetime.")


;;; Fireballs:

(def-feeb-parm *fireball-dissipation-probability* 1/4
  "Chance that a fireball will dissipate each turn after it is fired.")

(def-feeb-parm *fireball-reflection-probability* 1/2
  "Chance that a fireball will reflect when it hits a wall.")

(def-feeb-parm *flame-recovery-probability* 1/2
  "Chance a feeb will regain its ability to flame each turn after flaming once.")


;;; Structures:

;;; The Feeb structure contains all of the info relevant to a particular feeb.
;;; The info available to the brain function is in the Status sub-structure.

(defstruct (feeb
	    (:print-function print-feeb)
	    (:constructor make-feeb (id brain)))
  id
  brain
  image
  status
  proximity
  time
  last-score
  last-kills
  (dead-p nil)
  (turns-dead 0)
  (turns-since-flamed 0)
  (vision (make-array (max *maze-j-size* *maze-j-size*)))
  (vision-left (make-array (max *maze-j-size* *maze-j-size*)))
  (vision-right (make-array (max *maze-j-size* *maze-j-size*))))

(defstruct (status
	    (:conc-name nil)
	    (:constructor make-status (name facing i-position j-position)))
  name
  facing
  i-position
  j-position
  peeking
  line-of-sight
  (energy-reserve (+ *minimum-starting-energy*
		     (random (- *maximum-starting-energy*
				*minimum-starting-energy*))))
  (score 0)
  (kills 0)
  (ready-to-fire t)
  (aborted nil)
  (last-move :dead))


(defun print-feeb (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Feeb ~S>"
	  (name (feeb-status structure))))


(defstruct (proximity
	    (:conc-name nil))
  my-square
  rear-square
  left-square
  right-square)


;;; These image structures are used to represent feebs and fireballs in
;;; the sensory displays of other feebs.

(defstruct (feeb-image
	    (:print-function print-feeb-image)
	    (:constructor make-feeb-image (name facing feeb)))
  name
  facing
  feeb)

(defun print-feeb-image (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Feeb-Image of ~S facing ~S>"
	  (feeb-image-name structure)
	  (feeb-image-facing structure)))


(defstruct (fireball-image
	    (:print-function print-fireball-image)
	    (:constructor make-fireball-image (direction owner i j di dj)))
  direction
  owner
  i
  j
  di
  dj
  (new t))

(defun print-fireball-image (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Fireball moving ~S>"
	  (fireball-image-direction structure)))

(defstruct (position
	    (:constructor make-position (i j))
	    (:print-function print-position))
  i
  j)

(defun print-position (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Position (~A, ~A)>"
	  (position-i structure)
	  (position-j structure)))


;;; Setting up the maze.

;;; The default maze.
;;; X represents a wall,
;;; * represents a mushroom patch, and
;;; e is a feeb entry point.

(defparameter default-layout
  '("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    "Xe   *        XXXXXXX XXXXXXXXXX"
    "XXXXX XXXXXXX XXXXXXX    * XXXXX"
    "XXXXX XXXXXXX XXXXXXX XXX XXXXXX"
    "XXXXX XXX XXX  XXXXXXeXXX XXXXXX"
    "XXXXX XXX XXXX XXXXXXXXXX XXXXXX"
    "XXXXX XXX XXXX XX XXXXXXX XXXXXX"
    "XXXXX    *  XX XX XXXXXXX XXXXXX"
    "XXXXX XXXX XXX XX*    *   XXXXXX"
    "XX   *XXXX XXX XX XXXX XXXXXXXXX"
    "XX XX XXXXeXXX XX XXXX XXXXXXXXX"
    "XX XX XXXX XXX   * *  *        X"
    "XX XX XXXX XXXXXXXX XXXXXXXXXXeX"
    "XXeXX XXXX XXXXXXXX XXXXXXXXXX X"
    "XX XX     *   *     XXXXXXXX   X"
    "XX XXXXXXXXXXX XXXX XXXXXXXX XXX"
    "XX eXXXXXXXXXX  XXXe  XXXXXX XXX"
    "XX XXXXXXXXXXXXe XXXXXXXXXXX XXX"
    "XX*  XXX XXXXXXX  XXXXXXXXXX XXX"
    "XX X  XX XXXXXXXX eXXXXXXXXX XXX"
    "XX XX  X XXXXXXXXX  XXXXXXXX XXX"
    "X  XXX  *    XXXXXX*        *  X"
    "X XXXXXX XXX XXXXXX XXXXXXXXXX X"
    "X XXXXXX XXX XXXXXX X        X X"
    "X XXXXXX XXX XXXXXX X XXXXXX X X"
    "X    *     *     XX X X  *eX X X"
    "XXXXX XXXX XXXXXXXX X XXX XX X X"
    "XXXXX XXXX XXXXX   *X XXX XX X X"
    "XXXXX XXXX XXXXX XX X    e   X X"
    "XXXXX XXXX     e XX XXX*XXXXXX X"
    "XXXXX XXXXXXXXXXXXX            X"
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))

(defvar maze (make-array (list *maze-i-size* *maze-j-size*)
			 :initial-element ()))

(defvar *mushroom-sites*)
(defvar *entry-points*)
(defvar *number-of-entry-points*)

(defun init-maze (maze-strings)
  (setq *mushroom-sites* nil)
  (setq *entry-points* nil)
  (do ((rows maze-strings (cdr rows))
       (i 0 (1+ i)))
      ((null rows))
    (let ((str (car rows)))
      (dotimes (j (length str))
	(setf (aref maze i j) nil)
	(case (schar str j)
	  (#\X
	   (setf (aref maze i j) :rock))
	  (#\*
	   (push (make-position i j) *mushroom-sites*))
	  (#\e
	   (push (make-position i j) *entry-points*))
	  (#\space
	   )
	  (t
	   (error "Bad thing in maze spec: ~C." (schar str j)))))))
  (setq *number-of-mushroom-sites* (length *mushroom-sites*))
  (setq *number-of-entry-points* (length *entry-points*)))

(defun create-mushrooms ()
  (dotimes (i *number-of-mushrooms*)
    (do ((site (nth (random *number-of-mushroom-sites*) *mushroom-sites*)
	       (nth (random *number-of-mushroom-sites*) *mushroom-sites*)))
	((null (aref maze (position-i site) (position-j site)))
	 (setf (aref maze (position-i site) (position-j site)) :mushroom)))))


;;; Setting up the feebs.

(defvar *feebs* nil
  "A list of all the feebs in the current game.")

(defvar *next-feeb-id* 0
  "A counter used to assign a unique numerical code to each feeb.")

(defun create-feeb (name brain)
  (let* ((feeb (make-feeb *next-feeb-id* brain))
	 (facing (random 4))
	 (position (pick-random-entry-point)))
    (setf (feeb-image feeb)
	  (make-feeb-image name facing feeb))
    (setf (feeb-status feeb)
	  (make-status name facing (position-i position)
		       (position-j position)))
    (setf (feeb-proximity feeb) (make-proximity))
    (push feeb *feebs*)
    (incf *next-feeb-id*)
    (setf (aref maze (position-i position) (position-j position))
	  (feeb-image feeb))
    feeb))

;;; Start at some randomly chosen entry point.  If this one is occupied,
;;; scan successive entry points until a winner is found.  Circle back
;;; to start of list if necessary.

(defun pick-random-entry-point ()
  (do ((points (nthcdr (random *number-of-entry-points*) *entry-points*)
	       (cdr points)))
      (nil)
    (when (null points)
	  (setq points *entry-points*))
    (when (null (aref maze (position-i (car points))
		           (position-j (car points))))
	  (return (car points)))))


;;; Define-Feeb builds a list of feebs to create.  Create-Feebs actually
;;; builds the feebs on this list.

(defvar *feebs-to-be* nil)

(defun define-feeb (name brain)
  (push (cons name brain) *feebs-to-be*))

(defun create-feebs ()
  (when (> (length *feebs-to-be*) *number-of-entry-points*)
	(error "More feebs than entry points."))
  (setq *feebs* nil)
  (setq *next-feeb-id* 0)
  (dolist (feeb-spec (reverse *feebs-to-be*))
    (create-feeb (car feeb-spec) (cdr feeb-spec))))


;;; Vision calculation.

;;; Some macros for directional arithmetic.

(defmacro left-of (facing)
  `(mod (+ ,facing 3) 4))

(defmacro right-of (facing)
  `(mod (+ ,facing 1) 4))

(defmacro behind (facing)
  `(mod (+ ,facing 2) 4))

;;; These guys tell us offsets given an orientation.

(defconstant facing-vector-1 '#(-1 0 1 0))
(defconstant facing-vector-2 '#(0 1 0 -1))

(defmacro forward-di (facing)
  `(svref facing-vector-1 ,facing))

(defmacro forward-dj (facing)
  `(svref facing-vector-2 ,facing))

(defmacro left-di (facing)
  `(forward-di (left-of ,facing)))

(defmacro left-dj (facing)
  `(forward-dj (left-of ,facing)))

(defmacro right-di (facing)
  `(forward-di (right-of ,facing)))

(defmacro right-dj (facing)
  `(forward-dj (right-of ,facing)))

(defmacro behind-di (facing)
  `(forward-di (behind ,facing)))

(defmacro behind-dj (facing)
  `(forward-dj (behind ,facing)))

(defun compute-vision (feeb)
  (let* ((status (feeb-status feeb))
	 (proximity (feeb-proximity feeb))
	 (vision (feeb-vision feeb))
	 (vision-left (feeb-vision-left feeb))
	 (vision-right (feeb-vision-right feeb))
	 (facing (facing status))
	 vision-di
	 vision-dj
	 (i (i-position status))
	 (j (j-position status)))
    ;; First fill in proximity info.
    (setf (my-square proximity)
	  (aref maze i j))
    (setf (left-square proximity)
	  (aref maze (+ i (left-di facing)) (+ j (left-dj facing))))
    (setf (right-square proximity)
	  (aref maze (+ i (right-di facing)) (+ j (right-dj facing))))
    (setf (rear-square proximity)
	  (aref maze (+ i (behind-di facing)) (+ j (behind-dj facing))))
    ;; The vision vector starts in the square the feeb is facing.
    (setq i (+ i (forward-di facing)))
    (setq j (+ j (forward-dj facing)))
    ;; Figure out which direction to scan in.
    (case (peeking status)
      (nil)
      (:left (setq facing (left-of facing)))
      (:right (setq facing (right-of facing))))
    (setq vision-di (forward-di facing))
    (setq vision-dj (forward-dj facing))
    (do* ((i i (+ i vision-di))
	  (j j (+ j vision-dj))
	  (left-wall-i (+ i (left-di facing)) (+ left-wall-i vision-di))
	  (left-wall-j (+ j (left-dj facing)) (+ left-wall-j vision-dj))
	  (right-wall-i (+ i (right-di facing)) (+ right-wall-i vision-di))
	  (right-wall-j (+ j (right-dj facing)) (+ right-wall-j vision-dj))
	  (index 0 (1+ index)))
	 ((eq (aref maze i j) :rock)
	  (setf (line-of-sight status) index))
      (setf (aref vision index) (aref maze i j))
      (setf (aref vision-left index)
	    (side-imagify (aref maze left-wall-i left-wall-j)
			  (right-of facing)))
      (setf (aref vision-right index)
	    (side-imagify (aref maze right-wall-i right-wall-j)
			  (left-of facing))))))

;;; Compute the info to be put into the vision-left and vision-right vectors.
;;; A peeking feeb must be facing in the specified direction in order to count.

(defun side-imagify (stuff facing)
  (cond ((eq stuff :rock) :rock)
	((and (feeb-image-p stuff)
	      (= facing (feeb-image-facing stuff))
	      (peeking (feeb-status (feeb-image-feeb stuff))))
	 :peeking)
	((listp stuff)
	 (do ((stuff stuff (cdr stuff)))
	     ((null stuff) nil)
	   (when (and (feeb-image-p (car stuff))
		      (= facing (feeb-image-facing (car stuff)))
		      (peeking (feeb-status (feeb-image-feeb (car stuff)))))
		 (return :peeking))))
	(t nil)))


;;; Movement.

;;; Each turn, the following stuff has to happen:
;;;	1. Bump the turn counter; end the game if we should.
;;;	2. Maybe grow some mushrooms.
;;;	3. Maybe disappear some carcasses.
;;;	4. Move fireballs around.
;;;	5. See if any feebs have starved.
;;;	6. See if any feebs can flame again.
;;;	7. Compute vision and stuff for feebs.
;;;	8. Collect the feebs' moves.
;;;	9. Do the feeb's moves.

(defvar *current-turn* 0)

(defvar *mushrooms-alive*)
(defvar *dead-feebs*)
(defvar *fireballs-flying*)

(defun init-play ()
  (setq *mushrooms-alive* *number-of-mushrooms*)
  (setq *dead-feebs* nil)
  (setq *fireballs-flying* nil))

(defun play ()
  (dotimes (*current-turn* *game-length*)
    (play-one-turn)
    (when (xlib:event-listen *display*)
      (system:serve-event))
    (redisplay)
    (display-status)
    (cond ((not *continue*) (return-from play))
	  (*single-step* (get-mouse-buttonpress))
	  (*delay* (sleep-with-server *delay*)))))

(defun play-one-turn ()
  ;; Grow some mushrooms:
  (dotimes (i (- *number-of-mushrooms* *mushrooms-alive*))
    (let* ((site (nth (random *number-of-mushroom-sites*) *mushroom-sites*))
	   (i (position-i site))
	   (j (position-j site))
	   (stuff (aref maze (position-i site) (position-j site))))
      (cond ((null stuff)
	     (place-object :mushroom i j))
	    ((atom stuff)
	     (unless (eq stuff :mushroom)
	       (place-object :mushroom i j)))
	    (t
	     (unless (member :mushroom stuff)
	       (place-object :mushroom i j))))))
  ;; Rot some carcasses:
  (dolist (feeb *dead-feebs*)
    (incf (feeb-turns-dead feeb))
    (when (> (feeb-turns-dead feeb) *carcass-guaranteed-lifetime*)
      (when (chance *carcass-rot-probability*)
	(delete-object :carcass
		       (i-position (feeb-status feeb))
		       (j-position (feeb-status feeb)))
	(setq *dead-feebs* (delete feeb *dead-feebs*))
	(reincarnate-feeb feeb))))
  ;; Move some fireballs:
  (dolist (fireball *fireballs-flying*)
    (move-one-fireball fireball))
  ;; Starve some feebs:
  (dolist (feeb *feebs*)
    (unless (feeb-dead-p feeb)
      (when (<= (decf (energy-reserve (feeb-status feeb))) 0)
	(kill-feeb feeb))))
  ;; Let some feebs regain the power to flame:
  (dolist (feeb *feebs*)
    (unless (feeb-dead-p feeb)
      (unless (ready-to-fire (feeb-status feeb))
	(incf (feeb-turns-since-flamed feeb))
	(when (and (> (feeb-turns-since-flamed feeb) 1)
		   (chance *flame-recovery-probability*))
	  (setf (ready-to-fire (feeb-status feeb)) t)))))
  ;; Compute vision for all the feebs.
  (dolist (feeb *feebs*)
    (unless (feeb-dead-p feeb)
      (compute-vision feeb)))
  ;; Collect all the feebs' moves, keeping track of the time each one takes.
  (let ((total-time 1))
    (dolist (feeb *feebs*)
      (unless (feeb-dead-p feeb)
	(let ((time (get-internal-real-time)))
	  (setf (last-move (feeb-status feeb))
		(funcall (feeb-brain feeb)
			 (feeb-status feeb)
			 (feeb-proximity feeb)
			 (feeb-vision feeb)
			 (feeb-vision-left feeb)
			 (feeb-vision-right feeb)))
	  (setq time (- (get-internal-real-time) time))
	  (incf total-time time)
	  (setf (feeb-time feeb) time))))
    ;; Do all the feebs' moves, or perhaps abort the move according
    ;; to the time taken by the feeb.
    (setq total-time (float total-time))
    (dolist (feeb *feebs*)
      (unless (feeb-dead-p feeb)
	(cond ((and *slow-feeb-noop-switch*
		    (< (random 1.0)
		       (* *slow-feeb-noop-factor*
			  (/ (float (feeb-time feeb)) total-time))))
	       (setf (aborted (feeb-status feeb)) t))
	      (t (setf (aborted (feeb-status feeb)) nil)
		 (do-move feeb (last-move (feeb-status feeb)))))
	;; Make the image consistent with the feeb.
	(setf (feeb-image-facing (feeb-image feeb))
	      (facing (feeb-status feeb)))))))

(defun move-one-fireball (fireball)
  (let ((i (fireball-image-i fireball))
	(j (fireball-image-j fireball)))
    ;; Remove fireball from current square, unless it is new.
    (if (fireball-image-new fireball)
	(setf (fireball-image-new fireball) nil)
	(delete-object fireball i j))
    ;; The fireball might dissipate.
    (when (chance *fireball-dissipation-probability*)
	  (setq *fireballs-flying* (delete fireball *fireballs-flying*))
	  (return-from move-one-fireball nil))
    ;; Now move it to new coordinates.
    (incf i (fireball-image-di fireball))
    (incf j (fireball-image-dj fireball))
    ;; If it hits rock, either reflect or dissipate.
    (when (eq (aref maze i j) :rock)
      (cond ((chance *fireball-reflection-probability*)
	     (setf (fireball-image-di fireball)
		   (- (fireball-image-di fireball)))
	     (setf (fireball-image-dj fireball)
		   (- (fireball-image-dj fireball)))
	     (setf (fireball-image-direction fireball)
		   (behind (fireball-image-direction fireball)))
	     (setq i (fireball-image-i fireball))
	     (setq j (fireball-image-j fireball)))
	    (t (setq *fireballs-flying*
		     (delete fireball *fireballs-flying*))
	       (return-from move-one-fireball nil))))
    ;; Now put the fireball into the new square.
    (setf (fireball-image-i fireball) i)
    (setf (fireball-image-j fireball) j)
    (place-object fireball i j)
    ;; And destroy whatever is there.
    (let ((stuff (aref maze i j)))
      ;; If there is other stuff in the square, the contents will be a list.
      (when (listp stuff)
	    (dolist (thing stuff)
	      (cond ((fireball-image-p thing) nil)
		    ((eq thing :mushroom)
		     (delete-object :mushroom i j))
		    ((feeb-image-p thing)
		     (setq thing (feeb-image-feeb thing))
		     (score-kill fireball thing))))))))

;;; The fireball kills the feeb.  Update score for killer and victims.
;;; No credit for the kill if you shoot yourself.

(defun score-kill (fireball feeb)
  (unless (eq (fireball-image-owner fireball) feeb)
	  (incf (score (feeb-status (fireball-image-owner fireball)))
		*points-for-killing*)
	  (incf (kills (feeb-status (fireball-image-owner fireball)))))
  (kill-feeb feeb))


;;; Doing feeb moves.

(defun do-move (feeb move)
  (let ((status (feeb-status feeb)))
    ;; Redisplay the feeb's old square, whatever the action is.
    (queue-redisplay (i-position status) (j-position status))
    ;; If feeb was peeking last move, redisplay the square it is facing
    ;; to get rid of the periscope.
    (when (peeking status)
      (queue-redisplay (+ (i-position status) (forward-di (facing status)))
		       (+ (j-position status) (forward-dj (facing status)))))
    ;; Peeking gets undone every move.
    (setf (peeking status) nil)
    (case move
      (:turn-left
       (setf (facing status) (left-of (facing status))))      
      (:turn-right
       (setf (facing status) (right-of (facing status))))
      (:turn-around
       (setf (facing status) (behind (facing status))))
      (:move-forward
       (let* ((facing (facing status))
	      (old-i (i-position status))
	      (old-j (j-position status))
	      (new-i (+ (forward-di facing) old-i))
	      (new-j (+ (forward-dj facing) old-j))
	      (stuff (aref maze new-i new-j)))
	 (when (eq stuff :rock) (return-from do-move nil))
	 (delete-object (feeb-image feeb) old-i old-j)
	 (setf (i-position status) new-i)
	 (setf (j-position status) new-j)
	 (place-object (feeb-image feeb) new-i new-j)
	 ;; Look for a fireball in the destination square.
	 (when (fireball-image-p stuff)
	   (score-kill stuff feeb)
	   (return-from do-move nil))
	 (when (consp stuff)
	   (dolist (thing stuff)
	     (when (fireball-image-p thing)
	       (score-kill thing feeb)
	       (return-from do-move nil))))))
      (:flame
       (when (ready-to-fire status)
	 (let* ((facing (facing status))
		(i (i-position status))
		(j (j-position status))
		(fireball (make-fireball-image
			   facing feeb i j
			   (forward-di facing) (forward-dj facing))))
	   ;; Queue the fireball, marked as new, but don't put it on map yet.
	   (push fireball *fireballs-flying*)
	   (decf (energy-reserve status) *flame-energy*)
	   (setf (ready-to-fire status) nil)
	   (setf (feeb-turns-since-flamed feeb) 0))))
      (:eat-mushroom
       (let* ((i (i-position status))
	      (j (j-position status))
	      (stuff (aref maze i j)))
	 (when (and (listp stuff)
		    (member :mushroom stuff))
	   (delete-object :mushroom i j)
	   (setf (energy-reserve status)
		 (min (+ (energy-reserve status) *mushroom-energy*)
		      *maximum-energy*)))))
      (:eat-carcass
       (let* ((i (i-position status))
	      (j (j-position status))
	      (stuff (aref maze i j)))
	 (when (and (listp stuff)
		    (member :carcass stuff))
	   (setf (energy-reserve status)
		 (min (+ (energy-reserve status) *carcass-energy*)
		      *maximum-energy*)))))
      (:peek-left
       (unless (eq (aref maze (+ (i-position status)
				 (forward-di (facing status)))
			      (+ (j-position status)
				 (forward-dj (facing status))))
		   :rock)
	       (setf (peeking status) :left)))
      (:peek-right
       (unless (eq (aref maze (+ (i-position status)
				 (forward-di (facing status)))
			      (+ (j-position status)
				 (forward-dj (facing status))))
		   :rock)
	       (setf (peeking status) :right)))
      (:wait
       ))))

;;; Movement/Redisplay interface.

(defun delete-object (thing i j)
  (when (eq thing :mushroom)
    (decf *mushrooms-alive*))
  (let ((stuff (aref maze i j)))
    (cond ((atom stuff)
	   (setf (aref maze i j) nil))
	  ((null (cddr stuff))
	   (setf (aref maze i j)
		 (if (eq (car stuff) thing)
		     (cadr stuff)
		     (car stuff))))
	  (t
	   (setf (aref maze i j) (delete thing stuff)))))
  (queue-redisplay i j))

(defun place-object (thing i j)
  (when (eq thing :mushroom)
    (incf *mushrooms-alive*))
  (let ((stuff (aref maze i j)))
    (cond ((null stuff)
	   (setf (aref maze i j) thing))
	  ((consp stuff)
	   (setf (aref maze i j) (cons thing stuff)))
	  (t
	   (setf (aref maze i j) (list thing stuff)))))
  (queue-redisplay i j))

(defun reincarnate-feeb (feeb)
  (let ((position (nth (random (length *entry-points*)) *entry-points*))
	(facing (random 4))
	(status (feeb-status feeb)))
    (place-object (feeb-image feeb)
		  (position-i position) (position-j position))
    (setf (i-position status) (position-i position))
    (setf (j-position status) (position-j position))
    (setf (facing status) facing)
    (setf (feeb-dead-p feeb) nil)
    (setf (ready-to-fire status) t)
    (setf (energy-reserve status)
	  (+ *minimum-starting-energy*
	     (random (- *maximum-starting-energy*
			*minimum-starting-energy*))))
    (setf (last-move status) :dead)))

(defun kill-feeb (feeb)
  (push feeb *dead-feebs*)
  (setf (feeb-dead-p feeb) t)
  (setf (feeb-turns-dead feeb) 0)
  (setf (energy-reserve (feeb-status feeb)) 0)
  (let* ((status (feeb-status feeb))
	 (i (i-position status))
	 (j (j-position status)))
    (incf (score status) *points-for-dying*)
    ;; Clean up the periscope when a peeking feeb dies.
    (when (peeking status)
      (queue-redisplay (+ i (forward-di (facing status)))
		       (+ j (forward-dj (facing status)))))
    (delete-object (feeb-image feeb) i j)
    (place-object :carcass i j)
    (when *feep-dead-feebs*
      (feep *feep-dead-feebs-volume*))))

;;; Display routines for Feebs under X (Version 10).  All display
;;; primitives used in this program are defined here.  This was
;;; written for a monochrome display but could easily be adapted
;;; for a color display.

;;; Ported to X11.  Color added.

;;; Global X stuff
(defvar *display*)
(defvar *screen*)
(defvar *root*)

(defvar *birds-eye-window*)
(defvar *status-window*)
(defvar *banner-window*)

(defvar *status-font-id*)
(defvar *banner-font-id*)

(defvar *gcontext*)
(defvar *pixmap*)
(defvar *pixmap-gcontext*)

(defvar *black-pixel*)
(defvar *white-pixel*)

(defvar *colormap*)
(defvar *mushroom-color*)
(defvar *mushroom-color-name* "red")
(defvar *fireball-color*)
(defvar *fireball-color-name* "orange")
(defvar *carcass-color*)
(defvar *carcass-color-name* "green")
(defvar *feeb-colors* (make-array 11 :initial-element 0))
(defvar *feeb-color-names*
  '("green3" "seagreen3" "aquamarine3" "cadetblue3" "lightblue3"
    "lightskyblue3" "deepskyblue3" "steelblue3" "dodgerblue3" "royalblue3"
    "blue3"))

(defparameter bordercolor nil)
(defparameter background nil)
(defparameter foreground nil)
(defparameter borderwidth 1)

(defparameter birds-eye-width 704)
(defparameter birds-eye-height 704)
(defparameter birds-eye-x 36)
(defparameter birds-eye-y 60)

(defparameter status-window-width 248)
(defparameter status-window-height 704)
(defparameter status-window-x 740)
(defparameter status-window-y 60)

(defparameter banner-window-width 952)
(defparameter banner-window-height 27)
(defparameter banner-window-x 36)
(defparameter banner-window-y 32)

(defparameter feebs-font "8x13")
(defparameter *char-width* 8)
(defparameter *char-height* 13)

;;;(defparameter banner-font "micr25")
(defparameter banner-font "12x24")
(defparameter feebs-banner
  "Planet of the Feebs: A Somewhat Educational Simulation Game")

(defconstant redisplay-scale 22)
(defconstant pixel-character #\X)


;;;  A couple of misc. X utilities.
(defun full-window-state (w)
  (xlib:with-state (w)
    (values (xlib:drawable-width w) (xlib:drawable-height w)
	    (xlib:drawable-x w) (xlib:drawable-y w)
	    (xlib:window-map-state w))))


(defun wait-for-mapping (display win)
  (xlib:display-finish-output display)
  (multiple-value-bind (width height x y mapped) (full-window-state win)
    (declare (ignore width height x y))
    (if (eq mapped :viewable)
	t
      (wait-for-mapping display win))))



(defun init-graphics ()
  (multiple-value-setq (*display* *screen*) (ext:open-clx-display))
  (ext:enable-clx-event-handling *display* #'ext:object-set-event-handler)
  (setf *root* (xlib:screen-root *screen*))
  (setf *black-pixel* (xlib:screen-black-pixel *screen*))
  (setf *white-pixel* (xlib:screen-white-pixel *screen*))
  (setf bordercolor *black-pixel*)
  (setf background *white-pixel*)
  (setf foreground *black-pixel*)
  (setf *colormap* (car (xlib:installed-colormaps (xlib:screen-root *screen*))))
  (init-colors)
  (setf *status-font-id* (xlib:open-font *display* feebs-font))
  (setf *banner-font-id* (xlib:open-font *display* banner-font))
  (setq *gcontext* (xlib:create-gcontext :drawable *root*
					 :font *status-font-id*
					 :background background
					 :foreground foreground)))


(defun init-colors ()
  (if (> (xlib:screen-root-depth *screen*) 1)
      (progn
	(setf *mushroom-color* (xlib:alloc-color *colormap* *mushroom-color-name*))
	(setf *fireball-color* (xlib:alloc-color *colormap* *fireball-color-name*))
	(setf *carcass-color* (xlib:alloc-color *colormap* *carcass-color-name*))
	(dotimes (i 10)
	  (setf (aref *feeb-colors* i)
		(xlib:alloc-color *colormap* (nth i *feeb-color-names*)))))
    (progn
      (setf *mushroom-color* foreground)
      (setf *fireball-color* foreground)
	(setf *carcass-color* foreground)
	(dotimes (i 10)
	  (setf (aref *feeb-colors* i) foreground)))))


(defun tini-graphics ()
  (xlib::close-display *display*))

(defmacro create-window (x y width height)
  (declare (fixnum x y width height))
  `(xlib:create-window :parent *root*
		       :x ,x :y ,y
		       :width ,width :height ,height
		       :border-width borderwidth
		       :border bordercolor
		       :background background
		       :event-mask (xlib:make-event-mask
				    :exposure
				    :button-press
				    :key-press)))

(defmacro prepare-for-xevents (window)
  `(progn
     (system:add-xwindow-object ,window ,window *feebs-windows*)))

(defun display-window (window)
  (xlib:map-window window)
  (wait-for-mapping *display* window)
  (xlib:clear-area window)
  (xlib:display-force-output *display*))

(defun delete-window (window)
  ;; Remove the windows from the object sets before destroying them.
  (system:remove-xwindow-object window)
  ;; Destroy the window.
  (xlib:destroy-window window)
  ;; Pick off any events the X server has already queued for our
  ;; windows, so we don't choke since SYSTEM:SERVE-EVENT is no longer
  ;; prepared to handle events for us.
  (loop
   (unless (deleting-window-drop-event *display* window)
     (return))))

(defun deleting-window-drop-event (display win)
  "Check for any events on win.  If there is one, remove it from the
   event queue and return t; otherwise, return nil."
  (xlib:display-finish-output display)
  (let ((result nil))
    (xlib:process-event
     display :timeout 0
     :handler #'(lambda (&key event-window &allow-other-keys)
		  (if (eq event-window win)
		      (setf result t)
		    nil)))
    result))

;;; Window event control section.  Lots of hair necessary to deal
;;; with funny stuff of X windows.  Ignore it if you want to, but
;;; it won't go away.

(defvar *buttonpressed-flag* nil)
(defconstant blow-away-feebs-character #\q)
(defconstant single-step-feebs-character #\s)
(defconstant auto-mode-feebs-character #\a)
(defconstant feebs-noop-character #\z)

;;; Create an object set of windows to receive certain events from X.

(defvar *feebs-windows*
  (system:make-object-set "Feebs Windows"
			  #'ext:default-clx-event-handler))

;;; Sleep-with-server sleeps the appropriate amount of time but
;;; still processes xevents with the system server.

(defun sleep-with-server (time)
  (let ((end (+ (get-internal-real-time)
		(truncate (* time internal-time-units-per-second)))))
    (loop
      (let ((left (- end (get-internal-real-time))))
	(unless (plusp left) (return nil))
	(system:serve-event (/ left internal-time-units-per-second))))))

;;; Redraws a specific window if an exposure event (either exposewindow
;;; or exposeregion) occurs.

(defun redraw-all-exposed-regions (object &rest args)
  (declare (ignore args))
  (cond ((eq object *birds-eye-window*)
	 (redisplay-all))
	((eq object *status-window*)
	 (display-all-status))
	((eq object *banner-window*)
	 (redisplay-banner))))

(defun redraw-all-exposed-windows (object &rest args)
  (declare (ignore args))
  (cond ((eq object *birds-eye-window*)
	 (redisplay-all))
	((eq object *status-window*)
	 (display-all-status))
	((eq object *banner-window*)
	 (redisplay-banner))))

;;; Sets the buttonpressed flag to true when a mouse button is pressed.

(defun set-buttonpressed-flag (&rest args)
  (declare (ignore args))
  (setq *buttonpressed-flag* t))


(defun translate-character (scan-code bits)
  (let ((key-event (ext:translate-key-event *display* scan-code bits))
        (retval feebs-noop-character))
    (if key-event
        (let ((char (ext:key-event-char key-event)))
          (if char (setf retval char))))
    retval))

;;; Check which key was pressed and do the appropriate thing.

(defun check-keypressed (object event-key event-window root child
				same-screen-p x y root-x root-y
				mod-bits time scan-code send-event-p)
  (declare (ignore object event-key event-window root child
		   same-screen-p x y root-x root-y time send-event-p))
  (cond ((eq (translate-character scan-code mod-bits)
	     blow-away-feebs-character)
	 (format t "Feebs was interrupted at turn ~D.~%" *current-turn*)
	 (setq *continue* nil))
	((eq (translate-character scan-code mod-bits)
	     single-step-feebs-character)
	 (setq *single-step* t))
	((eq (translate-character scan-code mod-bits)
	     auto-mode-feebs-character)
	 (setq *single-step* nil))))

;;; Stops and waits until any one of the mouse buttons is pressed.

(defun get-mouse-buttonpress ()
  (loop
    (when *buttonpressed-flag*
      (setq *buttonpressed-flag* nil)
      (return))
    (system:serve-event)))


(defun do-nothing (&rest args)
  (declare (ignore args))
  t)

;;; Tell the X server which functions will handle which events when
;;; they occur.

(ext:serve-graphics-exposure  *feebs-windows* #'redraw-all-exposed-regions)
(ext:serve-exposure           *feebs-windows* #'redraw-all-exposed-windows)
(ext:serve-no-exposure        *feebs-windows* #'do-nothing)
(ext:serve-button-press       *feebs-windows* #'set-buttonpressed-flag)
(ext:serve-key-press          *feebs-windows* #'check-keypressed)

;;; Rings the keyboard bell.

(defun feep (feep-volume)
  (xlib:bell *display* feep-volume))

;;; The following section of code is used to manipulate an array
;;; of 16-bit unsigned integers (a "bit-array").  This array
;;; can be encoded with a graphic pattern from strings with
;;; "X"'s in them and then passed to X window manager routines to 
;;; create a bitmap for display.

;;; Computes the number of 16-bit unsigned integers needed for
;;; the bit-array.

(defmacro bit-array-size (width height)
  (declare (fixnum width height))
  `(* (truncate (+ ,width 15) 16) ,height))

;;; Computes how many bits each line of the bit-array has.

(defmacro bit-array-line-len (width)
  (declare (fixnum width))
  `(truncate (+ ,width 15) 16))

;;; Allows referencing of individual bits in the bit-array.

(defun bit-array-ref (array x y width)
  (declare (fixnum x y width))
  (multiple-value-bind (words bits) (truncate x 16)
    (ldb (byte 1 bits)
	 (aref array
	       (+ (* y (bit-array-line-len width))
		  words)))))

;;; Defines bit-array-ref as a setfable form to allow setting
;;; of the individual bits in the bit-array.

(defsetf bit-array-ref (array x y width) (new)
  (declare (fixnum x y width))
  (let ((words (gensym))
	(bits (gensym)))
    `(multiple-value-bind (,words ,bits) (truncate ,x 16)
       (setf (ldb (byte 1 ,bits) 
		  (aref ,array
			(+ (* ,y (bit-array-line-len ,width))
			   ,words)))
	     ,new))))

;;; Creates an array of 16-bit unsigned integers with the bits
;;; in this array set to 1 or 0 according to where there is an
;;; "X" in the strings.  This bit-array can then be passed to
;;; the X routines to create a bitmap.

(defun make-bit-array-from-strings (strings width height)
  (declare (list strings) (integer width height))
  (let ((bit-array (make-array (bit-array-size width height)
			       :element-type '(unsigned-byte 16)
			       :initial-element 0)))
    (do ((strings strings (cdr strings))
	 (row 0 (1+ row)))
	((eq row height))
      (let ((string (car strings)))
	(declare (simple-string string))
	(dotimes (column width)
	  (if (char= (schar string column) pixel-character)
	      (setf (bit-array-ref bit-array column row width) 1)))))
    bit-array))

;;;
;;; The following converts a bitmap from the old representation that
;;; was compatible with x10 to the X11 representation that can be sent
;;; to xlib::bitmap-image.  It's a little grody but it's easy and it
;;; only has to be done once.

;;; Get one line from the bit array as an integer.
(defun get-array-line (array line width)
  (declare (fixnum line width))
  (do ((i 0 (1+ i))
       (sum 0 (+ (* sum 2) (bit-array-ref array i line width))))
      ((>= i width) sum)))

(defun expand-bitmap (bit-array width height)
  (declare (fixnum width height))
  (do ((i (1- height) (1- i))
       (result nil (cons (read-from-string
			  (format nil
				  (format nil "#*~~~d,'0b" width)
				  (get-array-line bit-array i width)))
			 result)))
      ((< i 0) result)))

(defun make-bitmap (bit-array width height)
  (declare (fixnum width height))
  (apply #'xlib::bitmap-image (expand-bitmap bit-array width height)))

;;; Rotates the image in the bit-array to face in the
;;; specified direction relative to the current.

(defun rotate-bit-array (bit-array width height direction)
  (declare (fixnum width height))
  (if (eq width height)
      (let ((rotated-bit-array (make-array (bit-array-size width height)
				    :element-type '(unsigned-byte 16)
				    :initial-element 0)))
	(cond ((eq direction east)
	       (dotimes (j height)
		 (dotimes (i width)
		   (if (eq (bit-array-ref bit-array i j width) 1)
		       (setf (bit-array-ref rotated-bit-array 
					    (- width j) i width) 1)))))
	      ((eq direction south)
	       (dotimes (j height)
		 (dotimes (i width)
		   (if (eq (bit-array-ref bit-array i j width) 1)
		       (setf (bit-array-ref rotated-bit-array
					    (- width i) (- height j)
					    width) 1)))))
	      ((eq direction west)
	       (dotimes (j height)
		 (dotimes (i width)
		   (if (eq (bit-array-ref bit-array i j width) 1)
		       (setf (bit-array-ref rotated-bit-array
					    j (- height i) width) 1))))))
	rotated-bit-array)))

;;; Completely shades in a rectangular region of the screen with a
;;; given color at x and y coordinates and specified width and height.
;;; (or shades the region with the background color if :undraw is t.)


(defun draw-rectangle (window x y width height &key (undraw nil) (color foreground))
  (declare (fixnum x y width height))
  (let ((fg (if undraw
		background
	      color))
	(bg (if undraw
		foreground
	      background)))
    (xlib:with-gcontext (*gcontext* :background bg :foreground fg)
	(xlib:draw-rectangle window *gcontext* x y width height t))))


;;; Draws an X bitmap to the screen at x and y coordinates with
;;; the specified width and height.

(defun draw-bitmap (window x y width height bitmap &optional (color foreground))
  (declare (fixnum x y width height))
  (xlib:put-image *pixmap* *pixmap-gcontext* bitmap
		  :x 0 :y 0
		  :width width :height height)
  (xlib:with-gcontext (*gcontext* :foreground color
				  :stipple *pixmap*
				  :fill-style :opaque-stippled)
    (xlib:draw-rectangle window *gcontext* x y width height t)))


;;; Prints a string of characters in the specified location.

(defmacro draw-string (window x y string)
  (declare (fixnum x y) (simple-string string))
  `(xlib:draw-glyphs ,window *gcontext*
		     ,x (+ ,y *char-height*)
		     ,string :end (length ,string)))

;;; Prints a feeb-id in inverse video in the specified location.

(defmacro draw-feeb-id (window x y string)
  (declare (fixnum x y) (simple-string string))
  `(xlib:with-gcontext (*gcontext* :background foreground
				   :foreground background)
     (xlib:draw-glyphs ,window *gcontext*
		       ,x ,y
		       ,string :end (length ,string))))


;;; Redisplay functions.  Control the movement of the feebs in the
;;; maze window.

(defun init-banner ()
  (setq *banner-window*
	(create-window banner-window-x banner-window-y
		       banner-window-width banner-window-height))
  (xlib:display-finish-output *display*)
  (prepare-for-xevents *banner-window*)
  (display-window *banner-window*)
  (redisplay-banner))

(defun tini-banner ()
  (xlib:display-force-output *display*)
  (delete-window *banner-window*))

(defun init-redisplay ()
  (setq *birds-eye-window* 
	(create-window birds-eye-x birds-eye-y birds-eye-width birds-eye-height))
  (init-bitmaps)
  (xlib:display-finish-output *display*)
  (prepare-for-xevents *birds-eye-window*)
  (display-window *birds-eye-window*)
  (redisplay-all))

(defun tini-redisplay ()
  (xlib:display-force-output *display*)
  (delete-window *birds-eye-window*)
  (tini-bitmaps))

(defun redisplay-banner ()
  (xlib:with-gcontext (*gcontext* :font *banner-font-id*)
    (xlib:draw-glyphs *banner-window* *gcontext*
		      100 (- banner-window-height 3)
		      feebs-banner :end 59)))

(defvar *redisplay-map* (make-array (list *maze-i-size* *maze-j-size*)
				    :element-type 'bit))

(defvar *redisplay-hints* (make-array *maze-i-size*))

(defun queue-redisplay (i j)
  (setf (aref *redisplay-map* i j) 1)
  (setf (aref *redisplay-hints* i) t))

(defun redisplay ()
  (dotimes (i *maze-i-size*)
    (when (aref *redisplay-hints* i)
      (setf (aref *redisplay-hints* i) nil)
      (dotimes (j *maze-j-size*)
	(when (/= (aref *redisplay-map* i j) 0)
	  (setf (aref *redisplay-map* i j) 0)
	  (redisplay-square i j)))))
  (dolist (feeb *feebs*)
    (draw-periscope feeb))
  (xlib:display-finish-output *display*))

(defun redisplay-all ()
  (dotimes (i *maze-i-size*)
    (setf (aref *redisplay-hints* i) nil)
    (dotimes (j *maze-j-size*)
      (setf (aref *redisplay-map* i j) 0)
      (redisplay-square i j)))
  (dolist (feeb *feebs*)
    (unless (feeb-dead-p feeb)
      (draw-periscope feeb))))

(defun redisplay-square (i j)
  (let ((stuff (aref maze i j))
	(display-x (* j redisplay-scale))
	(display-y (* i redisplay-scale)))
    (if (eq stuff :rock)
	(draw-rectangle *birds-eye-window* display-x display-y
			redisplay-scale redisplay-scale)
      (draw-rectangle *birds-eye-window* display-x display-y
			redisplay-scale redisplay-scale :undraw t))
    (if (consp stuff)
	(dolist (substuff stuff)
	  (display-one-item substuff display-x display-y))
      (display-one-item stuff display-x display-y))))

(defun display-one-item (stuff display-x display-y)
  (cond ((null stuff)
	 )
	((eq stuff :rock)
	 )
	((eq stuff :mushroom)
	 (draw-mushroom display-x display-y))
	((eq stuff :carcass)
	 (draw-carcass display-x display-y))
	((feeb-image-p stuff)
	 (setq stuff (feeb-image-feeb stuff))
	 (draw-feeb display-x display-y stuff))
	((fireball-image-p stuff)
	 (draw-fireball display-x display-y
			(fireball-image-direction stuff)))
	(t
	 (error "Some strange thing in the maze: ~S." stuff))))


;;; String-maps for images.  These will be transformed into bitmaps
;;; that can be displayed in a small area.  The scale of the string-
;;; maps is given by the variable redisplay-scale.

(defvar *mushroom-bitmap*)
(defvar *carcass-bitmap*)
(defvar *fireball-bitmaps* (make-array 4))

(defun init-bitmaps ()
  (setf *pixmap* (xlib:create-pixmap
		  :width redisplay-scale
		  :height redisplay-scale
		  :depth 1
		  :drawable *birds-eye-window*)
	*pixmap-gcontext* (xlib:create-gcontext :drawable *pixmap*
					  :background background
					  :foreground foreground))
  (setq *mushroom-bitmap*
	(make-bitmap
	 (make-bit-array-from-strings
	  '("                      "
	    "                      "
	    "                      "
	    "                      "
	    "        XXXXXXX       "
	    "     XXXX X X XXX     "
	    "    XX X X X X X X    "
	    "   XX X X X X X X X   "
	    "  XX X X X X X X X X  "
	    " XX XXX X XXX XXX X X "
	    "         X X          "
	    "         X X          "
	    "         X  X         "
	    "          X X         "
	    "          X X         "
	    "          X  X        "
	    "           X X        "
	    "          X   X       "
	    "    XXXXXX     XXXX   "
	    "                      "
	    "                      "
	    "                      ")
	  redisplay-scale redisplay-scale)
	 redisplay-scale redisplay-scale))
  (setq *carcass-bitmap* 
	(make-bitmap
	 (make-bit-array-from-strings
	  '("                      "
	    "                      "
	    "  XXXXX         XXXXX "
	    "  XXXXX         XXXXX "
	    "    XXX         XXX   "
	    "    XXX         XXX   "
	    " XXXXXXXXXXXXXXXXXXXX "
	    " XXXXXXXXXXXXXXXXXXXX "
	    " XXXXXXXXXXXXXXXXXXXX "
	    " XXXXXXXXXXXXXXXXXXXX "
	    " XXXXXXXXXXXXXXXXXXXX "
	    " XXXXXXXXXXXXXXXXXXXX "
	    " XXXXXXXXXXXXXXXXXXXX "
	    " XXXX   XXXXXX   XXXX "
	    " XXX X X XXXX X X XXX "
	    " XXX  X  XXXX  X  XXX "
	    " XXX X X XXXX X X XXX "
	    " XXXX   XXXXXX   XXXX "
	    " XXXXXXXXXXXXXXXXXXXX "
	    " XXXXXXXXXXXXXXXXXXXX "
	    "                      "
	    "                      ")
	  redisplay-scale redisplay-scale)
	 redisplay-scale redisplay-scale))
  (let ((bit-array (make-bit-array-from-strings
		    '("                      "
		      "           X          "
		      "         X  X  X      "
		      "      X X  X XX X     "
		      "       X       X      "
		      "     X    X X    X    "
		      "     X   XXXX  X      "
		      "     X   XXXX  X      "
		      "       X  XX   X X    "
		      "      X X       X     "
		      "     X  X     X       "
		      "       X X  X  X      "
		      "       X    X         "
		      "       X  X  X        "
		      "        X X  X        "
		      "        X   X         "
		      "         X            "
		      "         X X          "
		      "            X         "
		      "          X           "
		      "          X           "
		      "                      ")
		    redisplay-scale redisplay-scale)))
    (setf (svref *fireball-bitmaps* 0)
	  (make-bitmap bit-array redisplay-scale redisplay-scale))
    (setf (svref *fireball-bitmaps* 1)
	  (make-bitmap (rotate-bit-array bit-array redisplay-scale
					 redisplay-scale east)
		       redisplay-scale redisplay-scale))
    (setf (svref *fireball-bitmaps* 2)
	  (make-bitmap (rotate-bit-array bit-array redisplay-scale
					 redisplay-scale south)
		       redisplay-scale redisplay-scale))
    (setf (svref *fireball-bitmaps* 3)
	  (make-bitmap (rotate-bit-array bit-array redisplay-scale
					 redisplay-scale west)
		       redisplay-scale redisplay-scale))))

(defun tini-bitmaps ()
  (xlib:free-gcontext *pixmap-gcontext*)
  (xlib:free-pixmap *pixmap*))

(defun draw-mushroom (x y)
  (draw-bitmap *birds-eye-window* x y
	       redisplay-scale redisplay-scale
	       *mushroom-bitmap* *mushroom-color*))

(defun draw-carcass (x y)
  (draw-bitmap *birds-eye-window* x y redisplay-scale redisplay-scale
	       *carcass-bitmap* *carcass-color*))

(defun draw-feeb (x y feeb)
  (let ((id (make-string 1 :initial-element (digit-char (feeb-id feeb))))
	(energy-index (floor (energy-reserve (feeb-status feeb))
			     (floor *maximum-energy* 10))))
    (unless (> energy-index 0)
      (setf energy-index 0))
    (draw-rectangle *birds-eye-window*
		    (+ x 1) (+ y 1)
		    20 20
		    :color (aref *feeb-colors* energy-index))
    (case (facing (feeb-status feeb))
      (0
       ;; Draw the feeb-id on the feeb's chest.
       (draw-feeb-id *birds-eye-window* (+ x 7) (+ y 8 *char-height*) id)
       ;; Draw the feeb's eye-outline.
       (draw-rectangle *birds-eye-window* (+ x 4) (+ y 3) 5 5 :undraw t)
       (draw-rectangle *birds-eye-window* (+ x 14) (+ y 3) 5 5 :undraw t)
       ;; Put pupils in the feeb's eyes.
       (draw-rectangle *birds-eye-window* (+ x 5) (+ y 4) 3 3)
       (draw-rectangle *birds-eye-window* (+ x 15) (+ y 4) 3 3)
       (draw-rectangle *birds-eye-window* (+ x 6) (+ y 5) 1 1 :undraw t)
       (draw-rectangle *birds-eye-window* (+ x 16) (+ y 5) 1 1 :undraw t))
      (1
       ;; Draw the feeb-id on the feeb's chest.
       (draw-feeb-id *birds-eye-window* (+ x 3) (+ y 5 *char-height*) id)
       ;; Draw the feeb's eye-outline.
       (draw-rectangle *birds-eye-window* (+ x 14) (+ y 4) 5 5 :undraw t)
       (draw-rectangle *birds-eye-window* (+ x 14) (+ y 14) 5 5 :undraw t)
       ;; Put pupils in the feeb's eyes.
       (draw-rectangle *birds-eye-window* (+ x 15) (+ y 5) 3 3)
       (draw-rectangle *birds-eye-window* (+ x 15) (+ y 15) 3 3)
       (draw-rectangle *birds-eye-window* (+ x 16) (+ y 6) 1 1 :undraw t)
       (draw-rectangle *birds-eye-window* (+ x 16) (+ y 16) 1 1 :undraw t))
      (2
       ;; Draw the feeb-id on the feeb's chest.
       (draw-feeb-id *birds-eye-window* (+ x 7) (+ y 1 *char-height*) id)
       ;; Draw the feeb's eye-outline.
       (draw-rectangle *birds-eye-window* (+ x 14) (+ y 14) 5 5 :undraw t)
       (draw-rectangle *birds-eye-window* (+ x 4) (+ y 14) 5 5 :undraw t)
       ;; Put pupils in the feeb's eyes.
       (draw-rectangle *birds-eye-window* (+ x 15) (+ y 15) 3 3)
       (draw-rectangle *birds-eye-window* (+ x 5) (+ y 15) 3 3)
       (draw-rectangle *birds-eye-window* (+ x 16) (+ y 16) 1 1 :undraw t)
       (draw-rectangle *birds-eye-window* (+ x 6) (+ y 16) 1 1 :undraw t))
      (3
       ;; Draw the feeb-id on the feeb's chest.
       (draw-feeb-id *birds-eye-window* (+ x 11) (+ y 5 *char-height*) id)
       ;; Draw the feeb's eye-outline.
       (draw-rectangle *birds-eye-window* (+ x 4) (+ y 14) 5 5 :undraw t)
       (draw-rectangle *birds-eye-window* (+ x 4) (+ y 4) 5 5 :undraw t)
       ;; Put pupils in the feeb's eyes.
       (draw-rectangle *birds-eye-window* (+ x 5) (+ y 15) 3 3)
       (draw-rectangle *birds-eye-window* (+ x 5) (+ y 5) 3 3)
       (draw-rectangle *birds-eye-window* (+ x 6) (+ y 16) 1 1 :undraw t)
       (draw-rectangle *birds-eye-window* (+ x 6) (+ y 6) 1 1 :undraw t)))))

;;; Must draw a peeking feeb's periscope in a separate operation, so that it
;;; doesn't get clobbered by other drawing in the square being peeked at.

(defun draw-periscope (feeb)
  (when (and (not (feeb-dead-p feeb))
	     (peeking (feeb-status feeb)))
    (let* ((status (feeb-status feeb))
	   (peeking (peeking status))
	   (x (* redisplay-scale (j-position status)))
	   (y (* redisplay-scale (i-position status))))
      (case (facing status)
	(0
	 (draw-rectangle *birds-eye-window* (+ x 11) (- y 5) 2 7)
	 (if (eq peeking :left)
	     (draw-rectangle *birds-eye-window* (+ x 9) (- y 5) 2 2)
	     (draw-rectangle *birds-eye-window* (+ x 13) (- y 5) 2 2)))
	(1
	 (draw-rectangle *birds-eye-window* (+ x 22) (+ y 11) 7 2)
	 (if (eq peeking :left)
	     (draw-rectangle *birds-eye-window* (+ x 27) (+ y 9) 2 2)
	     (draw-rectangle *birds-eye-window* (+ x 27) (+ y 13) 2 2)))
	(2
	 (draw-rectangle *birds-eye-window* (+ x 11) (+ y 22) 2 7)
	 (if (eq peeking :left)
	     (draw-rectangle *birds-eye-window* (+ x 13) (+ y 27) 2 2)
	     (draw-rectangle *birds-eye-window* (+ x 9) (+ y 27) 2 2)))
	(3
	 (draw-rectangle *birds-eye-window* (- x 5) (+ y 11) 7 2)
	 (if (eq peeking :left)
	     (draw-rectangle *birds-eye-window* (- x 5) (+ y 13) 2 2)
	     (draw-rectangle *birds-eye-window* (- x 5) (+ y 9) 2 2)))))))

(defun draw-fireball (x y facing)
  (declare (fixnum x y facing))
  (draw-bitmap *birds-eye-window* x y
	       redisplay-scale redisplay-scale
	       (svref *fireball-bitmaps* facing)
	       *fireball-color*))


;;; Status display.

(defconstant turn-column 6)
(defconstant id-column 0)
(defconstant name-column 3)
(defconstant score-column 8)
(defconstant kill-column 13)
(defconstant energy-column 18)
(defconstant last-move-column 23)

(defun init-status-display ()
  (setq *status-window* (create-window status-window-x status-window-y
				       status-window-width status-window-height))
  (xlib:display-finish-output *display*)
  (prepare-for-xevents *status-window*)
  (display-window *status-window*)
  (display-all-status))

(defun tini-status-display ()
  (xlib:display-force-output *display*)
  (delete-window *status-window*))

;;; Energy values change every turn, and always are in the range
;;; from 0 to 200, so just pre-compute all the strings for efficiency.

(defvar *small-number-strings*
  '#("  0" "  1" "  2" "  3" "  4" "  5" "  6" "  7" "  8" "  9"
     " 10" " 11" " 12" " 13" " 14" " 15" " 16" " 17" " 18" " 19"
     " 20" " 21" " 22" " 23" " 24" " 25" " 26" " 27" " 28" " 29"
     " 30" " 31" " 32" " 33" " 34" " 35" " 36" " 37" " 38" " 39"
     " 40" " 41" " 42" " 43" " 44" " 45" " 46" " 47" " 48" " 49"
     " 50" " 51" " 52" " 53" " 54" " 55" " 56" " 57" " 58" " 59"
     " 60" " 61" " 62" " 63" " 64" " 65" " 66" " 67" " 68" " 69"
     " 70" " 71" " 72" " 73" " 74" " 75" " 76" " 77" " 78" " 79"
     " 80" " 81" " 82" " 83" " 84" " 85" " 86" " 87" " 88" " 89"
     " 90" " 91" " 92" " 93" " 94" " 95" " 96" " 97" " 98" " 99"
     "100" "101" "102" "103" "104" "105" "106" "107" "108" "109"
     "110" "111" "112" "113" "114" "115" "116" "117" "118" "119"
     "120" "121" "122" "123" "124" "125" "126" "127" "128" "129"
     "130" "131" "132" "133" "134" "135" "136" "137" "138" "139"
     "140" "141" "142" "143" "144" "145" "146" "147" "148" "149"
     "150" "151" "152" "153" "154" "155" "156" "157" "158" "159"
     "160" "161" "162" "163" "164" "165" "166" "167" "168" "169"
     "170" "171" "172" "173" "174" "175" "176" "177" "178" "179"
     "180" "181" "182" "183" "184" "185" "186" "187" "188" "189"
     "190" "191" "192" "193" "194" "195" "196" "197" "198" "199"
     "200"))

(defmacro energy-to-string (energy)
  `(svref *small-number-strings* (max ,energy 0)))

;;; This refreshes or initializes all parts of the display.

(defun display-all-status ()
  (display-string "*STATUS*" 1 8 8)
  (display-string "Turn: " 3 0 6)
  (display-number *current-turn* 3 8 6)
  ;; 00000000001111111111222222222233333333334444444444
  ;; 01234567890123456789012345678901234567890123456789
  (display-string 
    "ID Name Sco. Kill Eng. LM" 4 0 25)
  (display-string
    "-------------------------" 5 0 25)
  (dolist (feeb *feebs*)
    (let ((status (feeb-status feeb))
	  (line (+ (feeb-id feeb) 6)))
      (display-number (feeb-id feeb) line 1 3)
      (display-string (name status) line 3 4)
      (setf (feeb-last-score feeb) (score status))
      (display-number (score status) line 10 4)
      (setf (feeb-last-kills feeb) (kills status))
      (display-number (kills status) line 15 4)
      (display-string (energy-to-string (energy-reserve status)) line 19 4)
      (display-string (cond ((feeb-dead-p feeb) "De")
			    ((aborted status) "Ab")
			    (t (case (last-move status)
				 (:turn-left "TL")
				 (:turn-right "TR")
				 (:turn-around "TA")
				 (:move-forward "MF")
				 (:eat-mushroom "EM")
				 (:eat-carcass "EC")
				 (:peek-left "PL")
				 (:peek-right "PR")
				 (:wait "WA")
				 (T "NM"))))
		      line 23 2)))
  (display-string
   "Click any mouse button to" 22 0 25)
  (display-string
   "zap windows at the end." 23 3 23)
  (display-string
   "Press the \"q\" key to quit" 25 0 25)
  (display-string
   "at any time." 26 3 12)
  (display-string
   "Press the \"s\" key to jump" 28 0 25)
  (display-string
   "into single-step mode." 29 3 22)
  (display-string
   "Click any mouse button to" 31 0 25)
  (display-string
   "single-step." 32 3 12)
  (display-string
   "Press the \"a\" key to jump" 34 0 25)
  (display-string
   "back to automatic mode." 35 3 13))

(defun display-status ()
  (display-number *current-turn* 3 8 6)
  (dolist (feeb *feebs*)
    (let ((status (feeb-status feeb))
	  (line (+ (feeb-id feeb) 6))
	  temp)
      (unless (= (feeb-last-score feeb)
		 (setq temp (score status)))
	(setf (feeb-last-score feeb) temp)
	(display-number temp line 10 4))
      (unless (= (feeb-last-kills feeb)
		 (setq temp (kills status)))
	(setf (feeb-last-kills feeb) temp)
	(display-number temp line 15 4))
      (display-string (energy-to-string (energy-reserve status)) line 19 4)
      (display-string (cond ((feeb-dead-p feeb) "De")
			    ((aborted status) "Ab")
			    (t (case (last-move status)
				 (:turn-left "TL")
				 (:turn-right "TR")
				 (:turn-around "TA")
				 (:move-forward "MF")
				 (:eat-mushroom "EM")
				 (:eat-carcass "EC")
				 (:peek-left "PL")
				 (:peek-right "PR")
				 (:wait "WA")
				 (T "NM"))))
		      line 23 2))))

(defun display-string (string line column field-width)
  (draw-rectangle *status-window*
		  (* (+ column 2) *char-width*) (* line *char-height*) 
		  (* field-width *char-width*) *char-height* :undraw t)
  (draw-string *status-window* (* (+ column 2) *char-width*)
	       (* line *char-height*)
	       string))

;;; Print a number plus optionally a leading minus sign.  Buffers the
;;; characters in a pre-existing string, so does no consing.  The number
;;; is printed right-justified within a field of the specified length,
;;; starting at the specified line and column.

(defvar *number-string-length* 5)
(defvar *number-string-buffer* (make-array *number-string-length*
					   :element-type 'character))

(defun display-number (n line column field )
  (let ((charpos *number-string-length*)
	(quotient (abs n))
	(remainder 0))
    (cond ((zerop n)
	   ;; Special-case zero so it doesn't print as nothing at all.
	   (decf charpos)
	   (setf (schar *number-string-buffer* charpos) #\0))
	  (t
	   ;; Stuff digits into the string from right to left.
	   (do ()
	       ((zerop quotient))
	     (multiple-value-setq (quotient remainder)
	       (truncate quotient 10))
	     (decf charpos)
	     (setf (schar *number-string-buffer* charpos)
		   (digit-char remainder)))
	   ;; And output the minus sign if needed.
	   (when (minusp n)
		 (decf charpos)
		 (setf (schar *number-string-buffer* charpos) #\-))))
    (do ()
        ((<= charpos 0))
      (decf charpos)
      (setf (schar *number-string-buffer* charpos) #\space))
    ;; Now print it.
    (draw-rectangle *status-window*
		    (* (+ column 1) *char-width*) (* line *char-height*)
		    (* field *char-width*) *char-height* :undraw t)
    (draw-string *status-window* 
		  (* (+ column field (- *number-string-length*)) *char-width*)
		  (* line *char-height*)
		  *number-string-buffer*)))

;;; It.

(defun feebs (&key (layout default-layout)	
		   single-step
		   delay
		   files
		   feep-dead-feebs)
  "This starts the simulation.  Takes some options as keyword arguments.
  :Single-step, if non-null, says to wait for any mouse click after each round.
  :Delay, if non-null, is a number of seconds to wait after each round.
  :Layout is the layout of the maze to use, if you don't want the default.
  :Files is a list of files containing feeb definitions."
  (let ((*single-step* single-step)
	(*delay* delay)
	(*feep-dead-feebs* feep-dead-feebs)
	(*continue* t))
    (when files (apply #'load-feebs files))
    (let ((n (- *number-of-feebs* (length *feebs-to-be*))))
      (cond ((minusp n)
	     (error "Too many pre-defined feebs: ~S.~%"
		    (length *feebs-to-be*)))
	    ((zerop n))
	    (t (format t "Making ~S dumb auto-feebs.~%" n)
	       (make-auto-feebs n))))
    (init-maze layout)
    (create-mushrooms)
    (create-feebs)
    (init-play)
    (unwind-protect
	(progn
	  (init-graphics)
	  (init-banner)
	  (init-redisplay)
	  (init-status-display)
	  (play))
      (when (and (not *single-step*) *continue*)
	(display-string "THE END." 19 8 8)
	(get-mouse-buttonpress))
      (xlib:display-finish-output *display*)
      (ext:disable-clx-event-handling *display*)
      (tini-redisplay)
      (tini-status-display)
      (tini-banner)
      (xlib:display-force-output *display*)
      (tini-graphics))
    (write-line "Final scores:")
    (dolist (feeb (reverse *feebs*))
      (format t "(~A) ~20A  ~5D~%"
	      (feeb-id feeb)
	      (feeb-image-name (feeb-image feeb))
	      (score (feeb-status feeb))))))


;;; Feeb creation.

(defun reset-feebs ()
  (setq *feebs-to-be* nil))

(defun load-feebs (&rest files)
  (reset-feebs)
  (dolist (file files)
    (load file)))

(defun make-auto-feebs (n)
  (let* ((nrandoms (floor n 3))
	 (nwanders (floor n 3))
	 (nconserve (- n nrandoms nwanders)))
    (dotimes (i nrandoms)
      (push (cons (concatenate 'string "Rnd" (princ-to-string (1+ i)))
		  'random-brain)
	    *feebs-to-be*))
    (dotimes (i nwanders)
      (push (cons (concatenate 'string "Wnd" (princ-to-string (1+ i)))
		  'wandering-brain)
	    *feebs-to-be*))
    (dotimes (i nconserve)
      (push (cons (concatenate 'string "Cns" (princ-to-string (1+ i)))
		  'conservative-brain)
	    *feebs-to-be*))))


;;; Here are some simple auto-feeb brains to use until we get better ones.

;;; About the stupidest brain possible.  Something for everyone to feel
;;; superior to.

(defun random-brain (status proximity vision vision-left vision-right)
  (declare (ignore status proximity vision vision-left vision-right))
  (svref '#(:turn-left :turn-right :turn-around :move-forward
	    :flame :eat-mushroom :eat-carcass :peek-left :peek-right
	    :wait)
	 (random 10)))


;;; This one wanders around, feeds when it can, and shoots at any visible
;;; opponent.

(defun wandering-brain (status proximity vision vision-left vision-right)
  (declare (ignore vision-left vision-right))
  (let ((stuff (my-square proximity)))
    (cond ((and (consp stuff) (member :mushroom stuff :test #'eq))
	   :eat-mushroom)
	  ((and (consp stuff) (member :carcass stuff :test #'eq))
	   :eat-carcass)
	  ((and (ready-to-fire status)
		(dotimes (index (line-of-sight status))
		  (let ((stuff (aref vision index)))
		    (if (listp stuff)
			(if (dolist (thing stuff)
			      (if (feeb-image-p thing)
				  (return t)))
			    (return t))
			(if (feeb-image-p stuff)
			    (return t))))))
	   :flame)
	  ((and (not (eq (left-square proximity) :rock))
		(> 0.2 (random 1.0)))
	   :turn-left)
	  ((and (not (eq (right-square proximity) :rock))
		(> 0.2 (random 1.0)))
	   :turn-right)
	  ((not (ready-to-fire status))
	   :wait)
	  ((> (line-of-sight status) 0)
	   :move-forward)
	  ((not (eq (left-square proximity) :rock))
	   :turn-left)
	  ((not (eq (right-square proximity) :rock))
	   :turn-right)
	  (t
	   :turn-around))))

;;; This one is similar to the wandering brain, but doesn't shoot if
;;; energy reserves are too low or the opponent is too far away.

(defun conservative-brain (status proximity vision vision-left vision-right)
  (declare (ignore vision-left vision-right))
  (let ((stuff (my-square proximity)))
    (cond ((and (consp stuff) (member :mushroom stuff :test #'eq))
	   :eat-mushroom)
	  ((and (consp stuff) (member :carcass stuff :test #'eq))
	   :eat-carcass)
	  ((and (ready-to-fire status)
		(> (energy-reserve status) 30)
		(dotimes (index (min (line-of-sight status) 5))
		  (let ((stuff (aref vision index)))
		    (if (listp stuff)
			(if (dolist (thing stuff)
			      (if (feeb-image-p thing)
				  (return t)))
			    (return t))
			(if (feeb-image-p stuff)
			    (return t))))))
	   :flame)
	  ((and (not (eq (left-square proximity) :rock))
		(> 0.2 (random 1.0)))
	   :turn-left)
	  ((and (not (eq (right-square proximity) :rock))
		(> 0.2 (random 1.0)))
	   :turn-right)
	  ((not (ready-to-fire status))
	   :wait)
	  ((> (line-of-sight status) 0)
	   :move-forward)
	  ((not (eq (left-square proximity) :rock))
	   :turn-left)
	  ((not (eq (right-square proximity) :rock))
	   :turn-right)
	  (t
	   :turn-around))))
