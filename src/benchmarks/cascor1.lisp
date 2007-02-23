;;; -*- Mode: Lisp; Package: User -*-
;;; ***************************************************************************
;;; Common Lisp implementation of Cascade-Correlation learning algorithm.
;;; This version for export.  Non-portable user-interface stuff excised.
;;; 
;;; Written by:   Scott E. Fahlman
;;;               School of Computer Science
;;;               Carnegie-Mellon University
;;;               Pittsburgh, PA 15217
;;;
;;;               Phone: (412) 268-2575
;;;               Internet: fahlman@cs.cmu.edu
;;;
;;; This code has been placed in the public domain by the author.  As a
;;; matter of simple courtesy, anyone using or adapting this code is
;;; expected to acknowledge the source.  The author would like to hear
;;; about any attempts to use this system, successful or not.
;;;
;;; For an explanation of this algorithm and some results, see "The
;;; Cascade-Correlation Learning Architecture" by Scott E. Fahlman and
;;; Christian Lebiere in D. S. Touretzky (ed.), "Advances in Neural
;;; Information Processing Systems 2", Morgan Kaufmann, 1990.  A somewhat
;;; longer version is available as CMU Computer Science Tech Report
;;; CMU-CS-90-100.
;;;
;;; ***************************************************************************
;;; EDIT HISTORY SINCE FIRST RELEASE:
;;;
;;; 8/24/90:
;;; Modified TEST-EPOCH so that it wouldn't mess up error statistics being
;;; passed from the output-training to input-training phase.  Thanks to
;;; Scott Crowder for spotting this.
;;; 
;;; 6/1/90:
;;; Fixed bug in INSTALL-NEW-UNIT.  New unit's initial weight was being
;;; computed using *CAND-COR* values of the successful candidate, which is
;;; already zero.  Now uses *CAND-PREV-COR*.  Thanks to Tim Howells for
;;; spotting this.
;;;
;;; Modify BUILD-NET to check that *MAX-UNITS* is large enough.  A couple of
;;; people got mysterious failures the first time they used lots of inputs.
;;;
;;; Made a small change in QUICKPROP-UPDATE to prevent rare divide-by-zero
;;; errors when p = s = 0.0.  Thanks to Tom Dietterich.
;;; 
;;; Added CHANGED-TRAINING-SET, which should be called when the training set
;;; is changed but you don't want to reinitialize the net.  This rebuilds
;;; the caches.
;;; 
;;; 11/9/90:
;;; Added some additional type declarations for maximum speed under certain
;;; Common Lisp compilers.
;;; ***************************************************************************
;;;
(in-package "USER")

;;; This proclamation buys a certain amount of overall speed at the expense
;;; of runtime checking.  Comment it out when debugging new, bug-infested code.
;;;
#+declare-unsafe
(proclaim '(optimize (speed 3) (space 0) (safety 0)))

;;; Style note: Because some of these runs take a long time, this code is
;;; extensively hacked for good performance under a couple of Common Lisp
;;; systems, some of which have poor performance on multi-dimensional
;;; arrays and some of which have weak type-inference in the compiler.
;;; Elegance and clarity have in some cases been sacrificed for speed.

;;; In some problems, floating point underflow errors may occur as a result
;;; of weight-decay and other operations.  Most Common Lisp implementations
;;; have an option to turn floating underflows into zero values without
;;; signalling an error.  You should enable this facility if it is
;;; available.  If not, you'll either have to write a condition handler for
;;; floating underflows or live with the occasional underflow error.

;;; In CMU Common Lisp, we use the following incantation:
;;;     (setq extensions:*ignore-floating-point-underflow* t)


;;; Compensate for the clumsy Common Lisp declaration system and weak
;;; type-inference in some Common Lisp compilers.

;;; INCF-SF, *SF, etc. are like INCF, *, etc., but they declare their
;;; operands and results to be short-floats.  The code gets unreadable
;;; quickly if you insert all these declarations by hand.

(defmacro incf-sf (place &optional (increment 1.0))
  `(the short-float (incf (the short-float ,place)
			  (the short-float ,increment))))

(defmacro decf-sf (place &optional (increment 1.0))
  `(the short-float (decf (the short-float ,place)
			  (the short-float ,increment))))

(defmacro *sf (&rest args)
  `(the short-float
	(* ,@(mapcar #'(lambda (x) (list 'the 'short-float x)) args))))

(defmacro +sf (&rest args)
  `(the short-float
	(+ ,@(mapcar #'(lambda (x) (list 'the 'short-float x)) args))))

(defmacro -sf (&rest args)
  `(the short-float
	(- ,@(mapcar #'(lambda (x) (list 'the 'short-float x)) args))))

(defmacro /sf (&rest args)
  `(the short-float
	(/ ,@(mapcar #'(lambda (x) (list 'the 'short-float x)) args))))

;;; DOTIMES1 is like DOTIMES, only with the loop counter declared as a
;;; fixnum.  This is for compilers with weak type inference.

(defmacro dotimes1 (form1 &body body)
  `(dotimes ,form1 (declare (fixnum ,(car form1))) . ,body))

;;; Create vector-access forms similar to SVREF, but for vectors of
;;; element-type SHORT-FLOAT and FIXNUM.

(eval-when (compile load eval)
  (defconstant fvector-type
    (array-element-type (make-array '(1) :element-type 'short-float)))
  (defconstant ivector-type
    (array-element-type (make-array '(1) :element-type 'fixnum))))

(defmacro fvref (a i)
  "Like SVREF, but with vectors of element-type SHORT-FLOAT."
  (if (eq fvector-type t)
    `(the short-float (svref ,a ,i))
    `(the short-float
	  (aref (the (simple-array ,fvector-type (*)) ,a) ,i))))

(defmacro ivref (a i)
  "Like SVREF, but with vectors of element-type FIXNUM."
  (if (eq ivector-type t)
    `(the fixnum (svref ,a ,i))
    `(the fixnum
	  (aref (the (simple-array ,ivector-type (*)) ,a) ,i))))


;;;; Assorted Parameters and Controls.

;;; Thse parameters and switches control the quickprop learning algorithm
;;; used to train the output weights and candidate units.

(defvar *unit-type* :sigmoid
  "The type of activation function used by the hidden units.  Options
  currently implemented are :sigmoid, :asigmoid, and :gaussian.  Sigmoid is
  symmetric in range -0.5 to +0.5, while Asigmoid is asymmetric, 0.0 to
  1.0.")

(defvar *output-type* :sigmoid
  "The activation function to use on the output units.  Options currently
  implemented are :linear and :sigmoid.")

(defvar *raw-error* nil
  "If T, candidate units will try to correlate with the raw difference
  between actual and desired outputs.  Else, they use the difference modified
  by the derivative of the output unit activation function.")

(defvar *sigmoid-prime-offset* 0.1
  "This is added to the derivative of the sigmoid function to prevent the
  system from getting stuck at the points where sigmoid-prime goes to
  zero.")
(proclaim '(short-float *sigmoid-prime-offset*))

(defvar *weight-range* 1.0
  "Input weights in the network get inital random values between plus and
  minus *weight-range*.  This parameter also controls the initial weights
  on direct input-to-output links.")
(proclaim '(short-float *weight-range*))

(defvar *weight-multiplier* 1.0
  "The output weights for cadidate units get an initial value that is the
  negative of the correlation times this factor.")
(proclaim '(short-float *weight-multiplier*))

(defvar *output-mu* 2.0
  "Mu parmater used for quickprop training of output weights.  The
  step size is limited to mu times the previous step.")
(proclaim '(short-float *output-mu*))

(defvar *output-shrink-factor* (/ *output-mu* (+ 1.0 *output-mu*))
  "Derived from *output-mu*.  Used in computing whether the proposed step is
  too large.")
(proclaim '(short-float *output-shrink-factor*))

(defvar *output-epsilon* 0.35
  "Controls the amount of linear gradient descent to use in updating
  output weights.")
(proclaim '(short-float *output-epsilon*))

(defvar *output-decay* 0.0001
  "This factor times the current weight is added to the slope at the
  start of each output-training epoch.  Keeps weights from growing too big.")
(proclaim '(short-float *output-decay*))

(defvar *output-patience* 8
  "If we go for this many epochs with no significant change, it's time to
  stop tuning.  If 0, go on forever.")
(proclaim '(fixnum *output-patience*))

(defvar *output-change-threshold* 0.01
  "The error must change by at least this fraction of its old value in
  order to count as a significant change.")
(proclaim '(short-float *output-change-threshold*))

(defvar *input-mu* 2.0
  "Mu parmater used for quickprop training of input weights.  The
  step size is limited to mu times the previous step.")
(proclaim '(short-float *input-mu*))

(defvar *input-shrink-factor* (/ *input-mu* (+ 1.0 *input-mu*))
  "Derived from *input-mu*.  Used in computing whether the proposed step is
  too large.")
(proclaim '(short-float *input-shrink-factor*))

(defvar *input-epsilon* 1.0
  "Controls the amount of linear gradient descent to use in updating
  unit input weights.")
(proclaim '(short-float *input-epsilon*))

(defvar *input-decay* 0.0
  "This factor times the current weight is added to the slope at the
  start of each output-training epoch.  Keeps weights from growing too big.")
(proclaim '(short-float *input-decay*))

(defvar *input-patience* 8
  "If we go for this many epochs with no significant change, it's time to
  stop tuning.  If 0, go on forever.")
(proclaim '(fixnum *input-patience*))

(defvar *input-change-threshold* 0.03
  "The correlation score for the best unit must change by at least
  this fraction of its old value in order to count as a significant
  change.")
(proclaim '(short-float *input-change-threshold*))

;;; Variables related to error and correlation.

(defvar *score-threshold* 0.4
  "An output is counted as correct for a given case if the difference
  between that output and the desired value is smaller in magnitude than
  this value.")
(proclaim '(short-float *score-threshold*))

(defvar *error-bits* 0
  "Count number of bits in epoch that are wrong by more than
  *SCORE-THRESHOLD*")
(proclaim '(fixnum *error-bits*))

(defvar *true-error* 0.0
  "The sum-squared error at the network outputs.  This is the value the
  algorithm is ultimately trying to minimize.")
(proclaim '(short-float *true-error*))

(defvar *sum-error* 0.0
  "Accumulate the sum of the error values after output training phase.")
(proclaim '(short-float *sum-error*))

(defvar *sum-sq-error* 0.0
  "Accumulate the sum of the squared error values after output
  training phase.")
(proclaim '(short-float *sum-sq-error*))

(defvar *avg-error* 0.0
  "Holds the average of error values after output training phase.")
(proclaim '(short-float *avg-error*))

(defvar *best-candidate-score* 0.0
  "The best correlation score found among all candidate units being
  trained.")
(proclaim '(short-float *best-candidate-score*))

(defvar *best-candidate* 0
  "The index of the candidate unit whose correlation score is best
  at present.")
(proclaim '(fixnum *best-candidate*))

;;; These variables and switches control the simulation and display.

(defvar *use-cache* t
  "If T, cache the forward-pass values instead of repeatedly
  computing them.  This can save a *lot* of time if all the cached values
  fit into memory.")

(defparameter *epoch* 0
  "Count of the number of times the entire training set has been presented.")
(proclaim '(fixnum *epoch*))

(defvar *test* nil
  "If T, run a test epoch every so often during output training.")

(defvar *test-interval* 0
  "Run a test epoch every *test-interval* output training cycles.")
(proclaim '(fixnum *test-interval*))

(defvar *single-pass* nil
  "When on, pause after next forward/backward cycle.")

(defvar *single-epoch* nil
  "When on, pause after next training epoch.")

(defparameter *step* nil
  "Turned briefly to T in order to continue after a pause.")

;;; The sets of training inputs and outputs are stored in parallel vectors.
;;; Each element is a SIMPLE-VECTOR holding short-float values, one for
;;; each input or output.  Note: this is a simple vector, not a specialized
;;; vector of element-type short-float.

(defvar *training-inputs* (make-array 0)
  "Vector of input patterns for training the net.")
(proclaim '(simple-vector *training-inputs*))

(defvar *training-outputs* (make-array 0)
  "Vector of output patterns for training the net.")
(proclaim '(simple-vector *training-outputs*))

(defvar *goal* (make-array 0)
  "The goal vector for the current training or testing case.")
(proclaim '(simple-vector *goal*))

(defvar *max-cases* 0
  "Maximum number of training cases that can be accommdated by the current
  data structures.")
(proclaim '(fixnum *max-cases*))

(defvar *ncases* 0
  "Number of training cases currently in use.  Assume a contiguous block
  beginning with *FIRST-CASE*.")
(proclaim '(fixnum *ncases*))

(defvar *first-case* 0
  "Address of the first training case in the currently active set.  Usually
  zero, but may differ if we are training on different chunks of the training
  set at different times.")
(proclaim '(fixnum *first-case*))

;;; For some benchmarks there is a separate set of values used for testing
;;; the network's ability to generalize.  These values are not used during
;;; training.

(defvar *test-inputs* '#()
  "Vector of input patterns for testing the net.")
(proclaim '(simple-vector *test-inputs*))

(defvar *test-outputs* '#()
  "Vector of output patterns for testing the net.")
(proclaim '(simple-vector *test-outputs*))


;;;; Fundamental data structures.

;;; Unit values and weights are short flonums.

;;; Instead of representing each unit by a structure, we represent the
;;; unit by a fixnum.  This is used to index into various vectors that hold
;;; per-unit information, such as the activation value of each unit.
;;; So the information concerning a given unit is found in a slice of values
;;; across many vectors, all with the same unit-index.

;;; Per-connection information for each connection COMING INTO unit is
;;; stored in a vector of vectors.  The outer vector is indexed by the unit
;;; number, and the inner vector is then indexed by connection number.
;;; This is a sleazy way of implementing a 2-D array, faster in most Lisp
;;; systems than multiplying to do the index arithmetic, and more efficient
;;; if the units are sparsely connected.

;;; Unit 0, the "bias unit" is always at a maximum-on value.  Next come
;;; some input "units", then some hidden units.

;;; Output units have their own separate set of data structures and
;;; indices.  The units and outputs together form the "active" network.
;;; There are also separate data structures and indices for the "candidate"
;;; units that have not yet been added to the network.

(defvar *max-units* 30
  "Maximum number of input values and hidden units in the network.")
(proclaim '(fixnum *max-units*))

(defvar *ninputs* 0
  "Number of inputs for this problem.")
(proclaim '(fixnum *ninputs*))

(defvar *noutputs* 0
  "Number of outputs for this problem.")
(proclaim '(fixnum *noutputs*))

(defvar *nunits* 0
  "Current number of active units in the network.  This count includes all
  inputs to the network and the bias unit.")
(proclaim '(fixnum *nunits*))

(defvar *ncandidates* 8
  "Number of candidate units whose inputs will be trained at once.")
(proclaim '(fixnum *ncandidates*))

;;; The following vectors hold values related to hidden units in the active
;;; net and their input weights.  The vectors are created by BUILD-NET, after
;;; the dimension variables have been set up.

(defvar *values* nil
  "Vector holding the current activation value for each unit and input in
  the active net.")

(defvar *values-cache* nil
  "Holds a distinct *VALUES* vector for each of the *MAX-CASES* training
  cases.  Once we have computed the *VALUES* vector for each training case,
  we can use it repeatedly until the weights or training cases change.")

(defvar *extra-values* nil
  "Extra values vector to use when not using the cache.")

;;; Note: the *NCONNECTIONS* and *CONNECTIONS* vectors could be eliminated
;;; if we wanted to commit to total connectivity for all units.
;;; For now, we want to allow for sparse or irregular connectivity.

(defvar *nconnections* nil
  "Vector holding the number of incoming connections for each unit.")

(defvar *connections* nil
  "Vector that holds a connection vector for each unit J.
  Each entry in the connection vector holds a unit index I,
  indicating that this connection is from I to J.")

(defvar *weights* nil
  "Vector of vectors with structure parallel to the *connections* vector.
  Each entry gives the weight associated with an incoming connection.")

;;; The following vectors hold values for the outputs of the active
;;; network and the output-side weights.

(defvar *outputs* nil
  "Vector holding the network output values.")

(defvar *errors* nil
  "Vector holding the current error value for each output.")

(defvar *errors-cache* nil
  "Holds a distinct *ERRORS* vector for each of the *MAX-CASES* training
  cases.  Once we have computed the *ERRORS* vector for a given training
  case, we can use it repeatedly until the weights of the training cases
  change.")

(defvar *extra-errors* nil
  "Extra errors vector to use when not using the cache.")

(defvar *output-weights* nil
  "Vector of vectors.  For each output, we have a vector of output weights
  coming from the unit indicated by the index.")

(defvar *output-deltas* nil
  "Vector of vectors, parallel with output weights.  Each entry is the
  amount by which the corresponding output weight was changed last time.")

(defvar *output-slopes* nil
  "Vector of vectors, parallel with output weights.  Each entry is the
  partial derivative of the total error with repsect to the corresponding
  weight.")

(defvar *output-prev-slopes* nil
  "Vector of vectors, parallel with output weights.  Each entry is the
  previous value of the corresponding *OUTPUT-SLOPES* entry.")

(defvar *output-weights-record* nil
  "The vector of output weights is recorded here after each output-training
  phase and just prior to the addition of the next unit.  This record
  allows us to reconstruct the network's performance at each of these
  points in time.")

;;; The following vectors have one entry for each candidate unit in the
;;; pool of trainees.

(defvar *cand-sum-values* nil
  "For each candidate unit, the sum of its values over an entire
  training set.")

(defvar *cand-cor* nil
  "A vector with one entry for each candidate unit.  This entry is a vector
  that holds the correlation between this unit's value and the residual
  error at each of the outputs, computed over a whole epoch.")

(defvar *cand-prev-cor* nil
  "Holds the *cand-cor* values computed in the previous candidate training
  epoch.")

(defvar *cand-weights* nil
  "A vector with one entry for each candidate unit.  This entry is a vector
  that holds the current input weights for that candidate unit.")

(defvar *cand-deltas* nil
  "A vector with one entry for each candidate unit.  This entry is a vector
  that holds the input weights deltas for that candidate unit.")

(defvar *cand-slopes* nil
  "A vector with one entry for each candidate unit.  This entry is a vector
  that holds the input weights slopes for that candidate unit.")

(defvar *cand-prev-slopes* nil
  "A vector with one entry for each candidate unit.  This entry is a vector
  that holds the previous values of the input weight slopes for that
  candidate unit.")

;;; At present, each candidate receives a connection from every input and
;;; pre-existing unit.  Rather than cons up a new *connections* vector for
;;; each of these, we can just use this one for all of them.

(defvar *all-connections* nil
  "A *CONNECTIONS* vector that can be used by any unit that connects to
  all lower-numbered units, in order.")


;;;; Network-building utilities.

(defun build-net (ninputs noutputs)
  "Create the network data structures, given the number of input and output
  connections.  Get *MAX-UNITS* and other dimesntions from variables."
  (declare (fixnum ninputs noutputs))
  ;; Check to make sure *MAX-UNITS* is big enough.
  (unless (> *max-units* (+ ninputs 1))
    (error "*MAX-UNITS* must be greater than number of inputs plus 1."))
  ;; Fill in assorted variables and create top-level vectors.
  (setq *ninputs* ninputs
	*noutputs* noutputs
	*max-cases* (length *training-inputs*)
	*ncases* *max-cases*
	*first-case* 0
	*nunits* (+ 1 *ninputs*)
	*values-cache* (make-array *max-cases* :initial-element nil)
	*extra-values* (make-array *max-units*
				   :element-type 'short-float
				   :initial-element 0.0)
	*values* *extra-values*
	*nconnections* (make-array *max-units*
				   :element-type 'fixnum
				   :initial-element 0)
	*connections* (make-array *max-units* :initial-element nil)
	*weights* (make-array *max-units* :initial-element nil)
	*outputs* (make-array *noutputs*
			      :element-type 'short-float
			      :initial-element 0.0)
	*errors-cache* (make-array *max-cases* :initial-element nil)
	*extra-errors* 	(make-array *noutputs*
				    :element-type 'short-float
				    :initial-element 0.0)
	*errors* *extra-errors*
	*output-weights* (make-array *noutputs* :initial-element nil)
	*output-weights-record* (make-array *max-units* :initial-element nil)
	*output-deltas* (make-array *noutputs* :initial-element nil)
	*output-slopes* (make-array *noutputs* :initial-element nil)
	*output-prev-slopes* (make-array *noutputs* :initial-element nil)
	*cand-sum-values* (make-array *ncandidates*
				      :element-type 'short-float
				      :initial-element 0.0)
	*cand-cor* (make-array *ncandidates* :initial-element nil)
	*cand-prev-cor* (make-array *ncandidates* :initial-element nil)
	*cand-weights* (make-array *ncandidates* :initial-element nil)
	*cand-deltas* (make-array *ncandidates* :initial-element nil)
	*cand-slopes* (make-array *ncandidates* :initial-element nil)
	*cand-prev-slopes* (make-array *ncandidates* :initial-element nil))
  ;; Only create the caches if *USE-CACHE* is on -- may not always have room.
  (when *use-cache*
    (dotimes1 (i *max-cases*)
      (setf (svref *values-cache* i)
	    (make-array *max-units*
			:element-type 'short-float
			:initial-element 0.0))
      (setf (svref *errors-cache* i)
	    (make-array *noutputs*
			:element-type 'short-float
			:initial-element 0.0))))
  ;; For each output, create the vectors holding per-weight information.
  (dotimes1 (i *noutputs*)
    (setf (svref *output-weights* i)
	  (make-array *max-units*
		      :element-type 'short-float
		      :initial-element 0.0))
    (setf (svref *output-deltas* i)    
	  (make-array *max-units*
		      :element-type 'short-float
		      :initial-element 0.0))
    (setf (svref *output-slopes* i)
	  (make-array *max-units*
		      :element-type 'short-float
		      :initial-element 0.0))
    (setf (svref *output-prev-slopes* i)
	  (make-array *max-units*
		      :element-type 'short-float
		      :initial-element 0.0)))
  ;; For each candidate unit, create the vectors holding the correlations,
  ;; incoming weights, and other stats.
  (dotimes1 (i *ncandidates*)
    (setf (svref *cand-cor* i)
	  (make-array *noutputs*
		      :element-type 'short-float
		      :initial-element 0.0))
    (setf (svref *cand-prev-cor* i)
	  (make-array *noutputs*
		      :element-type 'short-float
		      :initial-element 0.0))
    (setf (svref *cand-weights* i)
	  (make-array *max-units*
		      :element-type 'short-float
		      :initial-element 0.0))
    (setf (svref *cand-deltas* i)
	  (make-array *max-units*
		      :element-type 'short-float
		      :initial-element 0.0))
    (setf (svref *cand-slopes* i)
	  (make-array *max-units*
		      :element-type 'short-float
		      :initial-element 0.0))
    (setf (svref *cand-prev-slopes* i)
	  (make-array *max-units*
		      :element-type 'short-float
		      :initial-element 0.0))))

(defun random-weight ()
  "Select a random weight, uniformly distributed over the
  interval from minus to plus *weight-range*."
  (-sf (random (*sf 2.0 *weight-range*)) *weight-range*))

(defun init-net ()
  "Set up the network for a learning problem.  Clean up all the data
  structures that may have become corrupted.  Initialize the output weights
  to random values controlled by *weight-range*."
  ;; Set up the *ALL-CONNECTIONS* vector.
  (setq *all-connections*
	(make-array *max-units* :element-type 'fixnum))
  (dotimes1 (i *max-units*)
    (setf (ivref *all-connections* i) i))
  ;; Initialize the active unit data structures.
  (dotimes1 (i *max-units*)
    (setf (fvref *extra-values* i) 0.0)
    (setf (ivref *nconnections* i) 0)
    (setf (svref *connections* i) nil)
    (setf (svref *weights* i) nil)
    (setf (svref *output-weights-record* i) nil))
  ;; Initialize the per-output data structures.
  (dotimes1 (i *noutputs*)
    (setf (fvref *outputs* i) 0.0)
    (setf (fvref *extra-errors* i) 0.0)
    (let ((ow (svref *output-weights* i))
	  (od (svref *output-deltas* i))
	  (os (svref *output-slopes* i))
	  (op (svref *output-prev-slopes* i)))
      (dotimes1 (j *max-units*)
	(setf (fvref ow j) 0.0)
	(setf (fvref od j) 0.0)
	(setf (fvref os j) 0.0)
	(setf (fvref op j) 0.0))
      ;; Set up initial random weights for the input-to-output connections.
      (dotimes1 (j (1+ *ninputs*))
	(setf (fvref ow j) (random-weight)))))
  ;; Initialize the caches if they are in use.
  (when *use-cache*
    (dotimes1 (j *max-cases*)
      (let ((v (svref *values-cache* j))
	    (e (svref *errors-cache* j)))
	(dotimes1 (i *max-units*)
	  (setf (fvref v i) 0.0))
	(dotimes1 (i *noutputs*)
	  (setf (fvref e i) 0.0)))))	
  ;; Candidate units get initialized in a separate routine.
  (init-candidates)
  ;; Do some other assorted housekeeping.
  (setf (fvref *extra-values* 0) 1.0)
  (setq *epoch* 0)
  (setq *nunits* (+ 1 *ninputs*))
  (setq *error-bits* 0)
  (setq *true-error* 0.0)
  (setq *sum-error* 0.0)
  (setq *sum-sq-error* 0.0)
  (setq *best-candidate-score* 0.0)
  (setq *best-candidate* 0))

(defun changed-training-set ()
  "Call this instead of BUILD-NET and INIT-NET if you want to leave
  existing hidden units in place and start from here with new training
  examples.  Assumes that the number of net inputs and outputs remains the
  same, but the number of cases may have changed.  Rebuilds the caches."
  (setq *max-cases* (length *training-inputs*)
	*ncases* *max-cases*
	*first-case* 0
	*values-cache* (make-array *max-cases* :initial-element nil)
	*errors-cache* (make-array *max-cases* :initial-element nil))
  ;; Only create the caches if *USE-CACHE* is on -- may not always have room.
  (when *use-cache*
    (dotimes1 (i *max-cases*)
      (setf (svref *errors-cache* i)
	    (make-array *noutputs*
			:element-type 'short-float
			:initial-element 0.0))
      (setq *values* (make-array *max-units*
				 :element-type 'short-float
				 :initial-element 0.0))
      (setf (svref *values-cache* i) *values*)
      (set-up-inputs (svref *training-inputs* i))
      (do ((j (1+ *ninputs*) (1+ j)))
	  ((= j *nunits*))
	(declare (fixnum j))
	(compute-unit-value j)))))

;;;; Utilities for learning.

(proclaim '(inline activation activation-prime))

(defun activation (sum)
  "Given the sum of weighted inputs, compute the unit's activation value.
  Defined unit types are :sigmoid, :asigmoid, and :gaussian."
  (declare (short-float sum))
  (ecase *unit-type*
    (:sigmoid
     ;; Symmetric sigmoid function in range -0.5 to +0.5.
     (cond ((< sum -15.0) -0.5)
	   ((> sum 15.0) +0.5)
	   (t (-sf (/sf (+sf 1.0 (exp (-sf sum)))) 0.5))))
    (:asigmoid
     ;; Asymmetric sigmoid in range 0.0 to 1.0.
     (cond ((< sum -15.0) 0.0)
	   ((> sum 15.0) 1.0)
	   (t (/sf 1.0 (+sf 1.0 (exp (-sf sum)))))))
    (:gaussian
     ;; Gaussian activation function in range 0.0 to 1.0.
     (let ((x (*sf -0.5 sum sum)))
       (if (< x -75.0) 0.0 (exp x))))))

;;; Note: do not use *SIGMOID-PRIME-OFFSET* here, as it confuses the
;;; correlation machinery.  But do use it in output-prime, since it does no
;;; harm there and the output units often get stuck at extreme values.

(defun activation-prime (value sum)
  "Given the unit's activation value and sum of weighted inputs, compute
  the derivative of the activation with respect to the sum.  Defined unit
  types are :sigmoid, :asigmoid, and :gaussian."
  (declare (short-float value sum))
  (ecase *unit-type*
    (:sigmoid
     (-sf 0.25 (*sf value value)))
    (:asigmoid
     (*sf value (-sf 1.0 value)))
    (:gaussian
     (*sf (-sf value) sum))))

(proclaim '(inline output-function output-prime))

(defun output-function (sum)
  "Compute the value of an output, given the weighted sum of incoming values.
  Defined output types are :sigmoid and :linear."
  (declare (short-float sum))
  (ecase *output-type*
    (:sigmoid (cond ((< sum -15.0) -0.5)
		    ((> sum 15.0) +0.5)
		    (t (-sf (/sf 1.0 (+sf 1.0 (exp (-sf sum)))) 0.5))))
    (:linear sum)))

(defun output-prime (output)
  "Compute the derivative of an output with respect to the weighted sum of
  incoming values.  Defined output types are :sigmoid and :linear."
  (declare (short-float output))
  (ecase *output-type*
    (:sigmoid 
     (+sf *sigmoid-prime-offset* (-sf 0.25 (*sf output output))))
    (:linear 1.0)))

;;; The basic routine for doing Quickprop-style update of weights.
;;; Distilled essence of a year's work...

(proclaim '(inline quickprop-update))

(defun quickprop-update (i weights deltas slopes prevs
			   epsilon decay mu shrink-factor)
  "Given vectors holding weights, deltas, slopes, and previous slopes,
  and an index i, update weight(i) and delta(i) appropriately.  Move
  slope(i) to prev(i) and zero out slope(i).  Add weight decay term to
  each slope before doing the update."
  (let* ((w (fvref weights i))
	 (d (fvref deltas i))
	 (s (+sf (fvref slopes i) (*sf decay w)))
	 (p (fvref prevs i))
	 (next-step 0.0))
    (declare (short-float w p s d next-step)) 
    ;; The step must always be downhill.
    (cond
     ;; If last step was negative...
     ((minusp d)
      ;; First, add in linear term if current slope is still positive.
      (when (plusp s)
	(decf-sf next-step (*sf epsilon s)))
      (cond
       ;; If current slope is close to or larger than prev slope...
       ((>= s (*sf shrink-factor p))
	;; Take maximum size negative step.
	(incf-sf next-step (*sf mu d)))
       ;; Else, use quadratic estimate.
       (t (incf-sf next-step (*sf d (/sf s (-sf p s)))))))
     ;; If last step was positive...
     ((plusp d)
      ;; First, add in linear term if current slope is still negative.
      (when (minusp s)
	(decf-sf next-step (*sf epsilon s)))
      (cond
       ;; If current slope is close to or more neg than prev slope...
       ((<= s (*sf shrink-factor p))
	;; Take maximum size positive step.
	(incf-sf next-step (*sf mu d)))
       ;; Else, use quadratic estimate.
       (t (incf-sf next-step (*sf d (/sf s (-sf p s)))))))
     ;; Last step was zero, so use only linear term.
     (t (decf-sf next-step (*sf epsilon s))))
    ;; Having computed the next step, update the data vectors.
    (setf (fvref deltas i) next-step)
    (setf (fvref weights i) (+sf w next-step))
    (setf (fvref prevs i) s)
    (setf (fvref slopes i) 0.0)
    nil))


;;;; Machinery for training output weights.

(defun set-up-inputs (input)
  "Set up all the inputs from the INPUT vector as the first few entries in
  in the values vector."
  (declare (simple-vector input))
  (setf (fvref *values* 0) 1.0)
  (dotimes1 (i *ninputs*)
    (setf (fvref *values* (1+ i))
	  (the short-float (svref input i)))))

(defun output-forward-pass ()
  "Assume the *VALUES* vector has been set up.  Just compute the network's
  outputs."
  (dotimes1 (j *noutputs*)
    (let ((ow (svref *output-weights* j))
	  (sum 0.0))
      (declare (short-float sum))
      (dotimes1 (i *nunits*)
	(incf-sf sum (*sf (fvref *values* i) (fvref ow i))))
      (setf (fvref *outputs* j)
	    (output-function sum)))))

(defun compute-unit-value (j)
  "Assume that *VALUES* vector has been set up for all units with index less
  than J.  Compute and record the value for unit J."
  (declare (fixnum j))
  (let* ((c (svref *connections* j))
	 (w (svref *weights* j))
	 (sum 0.0))
    (declare (short-float sum))
    (dotimes1 (i (ivref *nconnections* j))
      (incf-sf sum (*sf (fvref *values* (ivref c i))
			(fvref w i))))
    (setf (fvref *values* j) (activation sum))
    nil))

(defun full-forward-pass (input)
  "Set up the inputs from the INPUT vector, then propagate activation values
  forward through all hidden units and output units."
  (set-up-inputs input)
  ;; For each hidden unit J, compute the activation value.
  (do ((j (1+ *ninputs*) (1+ j)))
      ((= j *nunits*))
    (declare (fixnum j))
    (compute-unit-value j))
  ;; Now compute outputs.
  (output-forward-pass))

;;; Note: We fill the *ERRORS* vector and related statistics with either
;;; the raw error or the error after modification by output-prime,
;;; depending on the *RAW-ERROR* switch.  This controls what form of error
;;; the candidate units try to correlate with.  All experiments reported in
;;; TR CMU-CS-90-100 assume *RAW-ERROR* is NIL, but this might not always
;;; be the best choice.

(defun compute-errors (goal output-slopes-p stats-p)
  "GOAL is a vector of desired values for the output units.  Compute and
  record the output errors for the current training case.  If
  OUTPUT-SLOPES-P is T, then use errors to compute slopes for output
  weights.  If STATS-P is T, accumulate error statistics."
  (declare (simple-vector goal))
  (dotimes1 (j *noutputs*)
    (let* ((out (fvref *outputs* j))
	   (dif (-sf out (svref goal j)))
	   (err-prime (*sf dif (output-prime out)))
	   (os (svref *output-slopes* j)))
      (declare (short-float dif err-prime))
      (when stats-p
	(unless (< (abs dif) *score-threshold*)
	  (incf *error-bits*))
	(incf-sf *true-error* (*sf dif dif)))
      (cond (*raw-error*
	     (setf (fvref *errors* j) dif)      
	     (incf-sf *sum-error* dif)
	     (incf-sf *sum-sq-error* (*sf dif dif)))
	    (t
	     (setf (fvref *errors* j) err-prime)      
	     (incf-sf *sum-error* err-prime)
	     (incf-sf *sum-sq-error* (*sf err-prime err-prime))))
      (when output-slopes-p
	(dotimes1 (i *nunits*)
	  (incf-sf (fvref os i) (*sf err-prime (fvref *values* i))))))))

;;; Note: Scaling *OUTPUT-EPSILON* by the number of cases seems to keep the
;;; quickprop update in a good range across many different-sized training
;;; sets, but it's something of a hack.  Choosing good epsilon values
;;; still requires some trial and error.

(defun update-output-weights ()
  "Update the output weights, using the pre-computed slopes, prev-slopes,
  and delta values.  Uses the quickprop update function."
  (let ((eps (/ *output-epsilon* *ncases*)))
    (dotimes1 (j *noutputs*)
      (let ((ow (svref *output-weights* j))
	    (od (svref *output-deltas* j))
	    (os (svref *output-slopes* j))
	    (op (svref *output-prev-slopes* j)))
	(dotimes1 (i *nunits*)
	  (quickprop-update i ow od os op eps *output-decay*
			    *output-mu* *output-shrink-factor*))))))


;;;; Outer loops for training output weights.

(defun train-outputs-epoch ()
  "Perform forward propagation once for each set of weights in the
  training vectors, computing errors and slopes.  Then update the output
  weights."
  ;; Zero error accumulators.
  (setq *error-bits* 0)
  (setq *true-error* 0.0)
  (setq *sum-error* 0.0)
  (setq *sum-sq-error* 0.0)
  ;; User may have changed mu between epochs, so fix shrink-factor.
  (setq *output-shrink-factor*
	(/sf *output-mu* (+sf 1.0 *output-mu*)))
  ;; Now run through the training examples.
  (do ((i *first-case* (1+ i)))
      ((= i (the fixnum (+ *first-case* *ncases*))))
    (declare (fixnum i))
    (setq *goal* (svref *training-outputs* i))
    (cond (*use-cache*
	   (setq *values* (svref *values-cache* i))
	   (setq *errors* (svref *errors-cache* i))
	   (output-forward-pass))
	  (t (setq *values* *extra-values*)
	     (setq *errors* *extra-errors*)
	     (full-forward-pass (svref *training-inputs* i))))
    (compute-errors *goal* t t))
  ;; Do not change weights or count epoch if this run was perfect.
  (unless (= 0 *error-bits*)
    (update-output-weights)
    (incf *epoch*)))

(defun record-output-weights ()
  "Store the output weights developed after each output-training phase
  in the *ouput-weights-record* vector."
  (let ((record (make-array *noutputs* :initial-element nil)))
    (dotimes1 (o *noutputs*)
      (let ((original (svref *output-weights* o))
	    (copy (make-array *nunits* :element-type 'short-float
			      :initial-element 0.0)))
	(dotimes1 (u *nunits*)
	  (setf (fvref copy u) (fvref original u)))
	(setf (svref record o) copy)))
    (setf (svref *output-weights-record* (1- *nunits*)) record)))

(defun train-outputs (max-epochs)
  "Train the output weights.  If we exhaust MAX-EPOCHS, stop with value
  :TIMEOUT.  If there are zero error bits, stop with value :WIN.  Else,
  keep going until the true error has not changed by a significant amount
  for *OUTPUT-PATIENCE* epochs.  Then return :STAGNANT.  If
  *OUTPUT-PATIENCE* is zero, we do not stop until victory or until
  MAX-EPOCHS is used up."
  (declare (fixnum max-epochs))
  (let ((last-error 0.0)
	(quit-epoch (+ *epoch* *output-patience*))
	(first-time t))
    (declare (fixnum quit-epoch)
	     (short-float last-error))
    (dotimes1 (i max-epochs (progn
			     (record-output-weights)
			     :timeout))
      ;; Maybe run a test epoch to see how we're doing.
      (when (and *test*
		 (not (= 0 *test-interval*))
		 (= 0 (mod i *test-interval*)))
	   (test-epoch))
      (train-outputs-epoch)
      (cond ((zerop *error-bits*)
	     (record-output-weights)
	     (return :win))
	    ((zerop *output-patience*))
	    (first-time
	     (setq first-time nil)
	     (setq last-error *true-error*))
	    ((> (abs (- *true-error* last-error))
		(* last-error *output-change-threshold*))
	     (setq last-error *true-error*)
	     (setq quit-epoch (+ *epoch* *output-patience*)))
	    ((>= *epoch* quit-epoch)
	     (record-output-weights)
	     (return :stagnant))))))


;;;; Machinery for Training, Selecting, and Installing Candidate Units.

(defun init-candidates ()
  "Give new random weights to all of the candidate units.  Zero the other
  candidate-unit statistics."
  (dotimes1 (i *ncandidates*)
    (setf (fvref *cand-sum-values* i) 0.0)
    (let ((cw (svref *cand-weights* i))
	  (cd (svref *cand-deltas* i))
	  (cs (svref *cand-slopes* i))
	  (cp (svref *cand-prev-slopes* i))
	  (cc (svref *cand-cor* i))
	  (cpc (svref *cand-prev-cor* i)))
      (dotimes1 (j *nunits*)
	(setf (fvref cw j) (random-weight))
	(setf (fvref cd j) 0.0)
	(setf (fvref cs j) 0.0)
	(setf (fvref cp j) 0.0))
      (dotimes1 (o *noutputs*)
	(setf (fvref cc o) 0.0)
	(setf (fvref cpc o) 0.0)))))

(defun install-new-unit ()
  "Add the candidate-unit with the best correlation score to the active
  network.  Then reinitialize the candidate pool."
  (when (>= *nunits* *max-units*)
    (error "Cannot add any more units."))
  ;; For now, assume total connectivity.
  (setf (ivref *nconnections* *nunits*) *nunits*)
  (setf (svref *connections* *nunits*) *all-connections*)
  ;; Copy the weight vector for the new unit.
  (let ((w (make-array *nunits* :element-type 'short-float))
	(cw (svref *cand-weights* *best-candidate*)))
    (dotimes1 (i *nunits*)
      (setf (fvref w i) (fvref cw i)))
    (setf (svref *weights* *nunits*) w)
    ;; Tell user about the new unit.
    (format t "  Add unit ~S: ~S~%"
	    (+ 1 *nunits*) w))
  ;; Fix up output weights for candidate unit.
  ;; Use minus the correlation times the *weight-multiplier* as an
  ;; initial guess.  At least the sign should be right.
  (dotimes1 (o *noutputs*)
    (setf (fvref (svref *output-weights* o) *nunits*)
	  (*sf (-sf (fvref (svref *cand-prev-cor* *best-candidate*) o))
	       *weight-multiplier*)))
  ;; If using cache, run an epoch to compute this unit's values.
  (when *use-cache*
    (dotimes1 (i *max-cases*)
      (setq *values* (svref *values-cache* i))
      (compute-unit-value *nunits*)))
  ;; Reinitialize candidate units with random weights.
  (incf *nunits*)
  (init-candidates))

;;; Note: Ideally, after each adjustment of the candidate weights, we would
;;; run two epochs.  The first would just determine the correlations
;;; between the candidate unit outputs and the residual error.  Then, in a
;;; second pass, we would adjust each candidate's input weights so as to
;;; maximize the absolute value of the correlation.  We need to know the
;;; sign of the correlation for each candidate-output pair so that we know
;;; which direction to tune the input weights.

;;; Since this ideal method doubles the number of epochs required for
;;; training candidates, we cheat slightly and use the correlation values
;;; computed BEFORE the most recent weight update.  This combines the two
;;; epochs, saving us almost a factor of two.  To bootstrap the process, we
;;; begin with a single epoch that computes only the correlation.

;;; Since we look only at the sign of the correlation and since that sign
;;; should change very infrequently, this probably is OK.  But keep a
;;; lookout for pathological situations in which this might cause
;;; oscillation.


;;; This function is used only once at the start of each output-training
;;; phase to prime the pump.  After that, each call to compute-slopes also
;;; computes the error-value products for the next epoch.

(defun compute-correlations ()
  "For the current training pattern, compute the value of each candidate
  unit and begin to compute the correlation between that unit's value and
  the error at each output.  We have already done a forward-prop and
  computed the error values for active units."
  (dotimes1 (u *ncandidates*)
    (let ((sum 0.0)
	  (v 0.0)
	  (cw (svref *cand-weights* u))
	  (cc (svref *cand-cor* u)))
      (declare (short-float sum v))
      ;; Determine activation value of each candidate unit.
      (dotimes1 (i *nunits*)
	(incf-sf sum (*sf (fvref cw i)
			  (fvref *values* i))))
      (setq v (activation sum))
      (incf-sf (fvref *cand-sum-values* u) v)
      ;; Accumulate value of each unit times error at each output.
      (dotimes1 (o *noutputs*)
	(incf-sf (fvref cc o) (*sf v (fvref *errors* o)))))))

;;; Note: When we were computing true correlations between candidates and
;;; outputs, this is where the normalization factors went in.  Currently we
;;; are just using covariances, as explained in the tech report.  So we
;;; make only two adjustments here.  First, we subtract out the product of
;;; the mean error and the mean candidate value to keep things from
;;; exploding when the error has a non-zero mean.  Second, we effectively
;;; scale the error values by the sum-squared error over all training
;;; cases.  This just keeps us from having to adjust *input-epsilon*
;;; repeatedly as the error is gradually reduced to a small fraction of its
;;; initial size.

(defun adjust-correlations ()
  "Normalize each accumulated correlation value, and stuff the normalized
  form into the *cand-prev-cor* data structure.  Then zero *cand-cor* to
  prepare for the next round.  Note the unit with the best total
  correlation score."
  (setq *best-candidate* 0)
  (setq *best-candidate-score* 0.0)
  (dotimes1 (u *ncandidates*)
    (let* ((cc (svref *cand-cor* u))
	   (cpc (svref *cand-prev-cor* u))
	   (offset (*sf (fvref *cand-sum-values* u) *avg-error*))
	   (cor 0.0)
	   (score 0.0))
      (declare (short-float offset cor score))
      (dotimes1 (o *noutputs*)
	(setq cor (/sf (-sf (fvref cc o) offset) *sum-sq-error*))
	(setf (fvref cpc o) cor)
	(setf (fvref cc o) 0.0)
	(incf-sf score (abs cor)))
      ;; Keep track of the candidate with the best overall correlation.
      (when (> score *best-candidate-score*)
	(setq *best-candidate-score* score)
	(setq *best-candidate* u)))))

;;; This is the key function in the candidate training process.

(defun compute-slopes ()
  "Given the correlation values for each candidate-output pair, compute
  the derivative of the candidate's score with respect to each incoming
  weight."
  (dotimes1 (u *ncandidates*)
    (let* ((sum 0.0)
	   (value 0.0)
	   (actprime 0.0)
	   (direction 0.0)
	   (cw (svref *cand-weights* u))
	   (cs (svref *cand-slopes* u))
	   (cc (svref *cand-cor* u))
	   (cpc (svref *cand-prev-cor* u)))
      (declare (short-float sum value actprime direction))
      ;; Forward pass through each candidate unit to compute activation-prime.
      (dotimes1 (i *nunits*)
	(incf-sf sum (*sf (fvref cw i)
			  (fvref *values* i))))
      (setq value (activation sum))
      (setq actprime (activation-prime value sum))
      ;; Now compute which way we want to adjust each unit's incoming
      ;; activation.
      (dotimes1 (o *noutputs*)
	(let ((error (fvref *errors* o)))
	  (decf-sf direction
		   (*sf (if (minusp (fvref cpc o)) -1.0 1.0)
			(*sf actprime
			     (/sf (-sf error *avg-error*)
				  *sum-sq-error*))))
	  ;; Also accumulate the error-value products for use next epoch.
	  (incf-sf (fvref cc o) (*sf error value))))
      ;; Given the direction we want to push the candidate, compute
      ;; which way we want to tweak each incoming weight.
      (dotimes1 (i *nunits*)
	(incf-sf (fvref cs i)
		 (*sf direction (fvref *values* i)))))))

;;; Note: Scaling *INPUT-EPSILON* by the number of cases and number of
;;; inputs to each unit seems to keep the quickprop update in a good range,
;;; as the network goes from small to large, and across many
;;; different-sized training sets.  Still, choosing a good epsilon value
;;; requires some trial and error.

(defun update-input-weights ()
  "Update the input weights, using the pre-computed slopes, prev-slopes,
  and delta values.  Uses the quickprop update function."
  (let ((eps (/ *input-epsilon* (* *ncases* *nunits*))))
    (dotimes1 (u *ncandidates*)
      (let ((cw (svref *cand-weights* u))
	    (cd (svref *cand-deltas* u))
	    (cs (svref *cand-slopes* u))
	    (cp (svref *cand-prev-slopes* u)))
	(dotimes1 (i *nunits*)
	  (quickprop-update i cw cd cs cp eps *input-decay*
			    *input-mu* *input-shrink-factor*))))))

;;; Outer loop for training the candidate unit(s).

(defun train-inputs-epoch ()
  "For each training pattern, perform a forward pass.  Tune the candidate units'
  weights to maximize the correlation score of each."
  (do ((i *first-case* (1+ i)))
      ((= i (the fixnum (+ *first-case* *ncases*))))
    (declare (fixnum i))
    (setq *goal* (svref *training-outputs* i))
    ;; Compute values and errors, or recall cached values.
    (cond (*use-cache*
	   (setq *values* (svref *values-cache* i))
	   (setq *errors* (svref *errors-cache* i)))
	  (t (setq *values* *extra-values*)
	     (setq *errors* *extra-errors*)
	     (full-forward-pass (svref *training-inputs* i))
	     (compute-errors *goal* nil nil)))
    ;; Compute the slopes we will use to adjust candidate weights.
    (compute-slopes))
  ;; User may have changed mu between epochs, so fix shrink-factor.
  (setq *input-shrink-factor* (/sf *input-mu*
				   (+sf 1.0 *input-mu*)))
  ;; Now adjust the candidate unit input weights using quickprop.
  (update-input-weights)
  ;; Fix up the correlation values for the next epoch.
  (adjust-correlations)
  (incf *epoch*))

(defun correlations-epoch ()
  "Do an epoch through all active training patterns just to compute the
  initial correlations.  After this one pass, we will update the
  correlations as we train."
  (do ((i *first-case* (1+ i)))
      ((= i (the fixnum (+ *first-case* *ncases*))))
    (declare (fixnum i))
    (setq *goal* (svref *training-outputs* i))
    (cond (*use-cache*
	   (setq *values* (svref *values-cache* i))
	   (setq *errors* (svref *errors-cache* i)))
	  (t (setq *values* *extra-values*)
	     (setq *errors* *extra-errors*)
	     (full-forward-pass (svref *training-inputs* i))
	     (compute-errors *goal* nil nil)))
    (compute-correlations))
  (adjust-correlations)
  (incf *epoch*))

(defun train-inputs (max-epochs)
  "Train the input weights of all candidates.  If we exhaust MAX-EPOCHS,
  stop with value :TIMEOUT.  Else, keep going until the best candidate
  unit's score has changed by a significant amount, and then until it does
  not change significantly for PATIENCE epochs.  Then return :STAGNANT.  If
  PATIENCE is zero, we do not stop until victory or until MAX-EPOCHS is
  used up."
  (declare (fixnum max-epochs))
  (setq *avg-error* (/ *sum-error* (* *ncases* *noutputs*)))
  (correlations-epoch)
  (let ((last-score 0.0)
	(quit max-epochs)
	(first-time t))
    (declare (fixnum quit)
	     (short-float last-score))
    (dotimes1 (i max-epochs :timeout)
      (train-inputs-epoch)
      (cond ((zerop *input-patience*))
	    (first-time
	     (setq first-time nil)
	     (setq last-score *best-candidate-score*))
	    ((> (abs (-sf *best-candidate-score* last-score))
		(* last-score *input-change-threshold*))
	     (setq last-score *best-candidate-score*)
	     (setq quit (+ i *input-patience*)))
	    ((>= i quit)
	     (return :stagnant))))))

;;;; Outer Loop.

(defun list-parameters ()
  "Print out the current training parameters in abbreviated form."
  (format t "SigOff ~,2F, WtRng ~,2F, WtMul ~,2F~%"
	  *sigmoid-prime-offset* *weight-range* *weight-multiplier*)
  (format t "OMu ~,2F, OEps ~,2F, ODcy ~,4F, OPat ~D, OChange ~,3F~%"
	  *output-mu* *output-epsilon* *output-decay* *output-patience*
	  *output-change-threshold*)
  (format t "IMu ~,2F, IEps ~,2F, IDcy ~,4F, IPat ~D, IChange ~,3F~%"
	  *input-mu* *input-epsilon* *input-decay* *input-patience*
	  *input-change-threshold*)
  (format t "Utype ~S, Otype ~S, RawErr ~S, Pool ~D~%"
	  *unit-type* *output-type* *raw-error* *ncandidates*))

(defun train (outlimit inlimit rounds &optional (restart nil))
  "Train the output weights until stagnation or victory is reached.  Then
  train the input weights to stagnation or victory.  Then install the best
  candidate unit and repeat.  OUTLIMIT and INLIMIT are upper limits on the number
  of cycles in each output and input phase.  ROUNDS is an upper limit on
  the number of unit-installation cycles.  If RESTART is non-nil, we are
  restarting training from the current point -- do not reinitialize the net."
  (declare (fixnum outlimit inlimit rounds))
  (unless restart (init-net))
  (list-parameters)
  (when *use-cache*
    (dotimes1 (i *max-cases*)
      (setq *values* (svref *values-cache* i))
      (set-up-inputs (svref *training-inputs* i))))
  (dotimes1 (r rounds  :lose)
    (case (train-outputs outlimit)
      (:win
       (list-parameters)
       (format t "Victory at ~S epochs, ~S units, ~S hidden, Error ~S.~%"
	       *epoch* *nunits* (- *nunits* *ninputs* 1) *true-error*)
       (return nil))
      (:timeout
       (format t "Epoch ~D: Out Timeout  ~D bits wrong, error ~S.~2%"
	       *epoch* *error-bits* *true-error*))
      (:stagnant
       (format t "Epoch ~D: Out Stagnant ~D bits wrong, error ~S.~2%"
	       *epoch* *error-bits* *true-error*)))
    (when *test* (test-epoch))
    (case (train-inputs inlimit)
      (:timeout
       (format t "Epoch ~D: In Timeout.  Cor: ~D~%"
	       *epoch* *best-candidate-score*))
      (:stagnant
       (format t "Epoch ~D: In Stagnant.  Cor: ~D~%"
	       *epoch* *best-candidate-score*)))
    (install-new-unit)))

(defun test-epoch (&optional (*score-threshold* 0.49999))
  "Perform forward propagation once for each set of weights in the training
  and testing vectors.  Reporting the performance.  Do not change any
  weights.  Do not use the caches."
  (let ((*use-cache* nil)
	(*values* *extra-values*)
	(*errors* *extra-errors*)
	(*error-bits* 0)
	(*true-error* 0.0)
	(*sum-error* 0.0)
	(*sum-sq-error* 0.0))
    ;; Run all training patterns and count errors.
    (dotimes1 (i (length *training-inputs*))
      (setq *goal* (svref *training-outputs* i))
      (full-forward-pass (svref *training-inputs* i))
      (compute-errors *goal* nil t))
    (format t "Training: ~D of ~D wrong, error ~S."
	    *error-bits* (length *training-inputs*) *true-error*)
    ;; Zero some accumulators again.
    (setq *error-bits* 0)
    (setq *true-error* 0.0)
    (setq *sum-error* 0.0)
    (setq *sum-sq-error* 0.0)
    ;; Now run all test patterns and report the results.
    (when *test-inputs*
      (dotimes1 (i (length *test-inputs*))
	(setq *goal* (svref *test-outputs* i))
	(full-forward-pass (svref *test-inputs* i))
	(compute-errors *goal* nil t)))
    (format t "  Test: ~D of ~D wrong, error ~S.~%"
	    *error-bits* (length *test-inputs*) *true-error*)))

(defun test-setup (nunits weights output-weights)
  "Set up a network for testing, given stored weights and output weights."
  (init-net)
  (setq *weights* weights)
  (setq *output-weights* output-weights)
  (setq *nunits* nunits)
  (do ((i (1+ *ninputs*) (1+ i)))
      ((= i *nunits*))
    (declare (fixnum i))
    (setf (ivref *nconnections* i) i)
    (setf (svref *connections* i) *all-connections*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Applications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Zig-Zag problem.  An easy one, useful for testing the code.

(defun build-zig-zag (n)
  "Build N pairs of 1-D zig-zag."
  (declare (fixnum n))
  (setq *ninputs* 1)
  (setq *noutputs* 1)
  (let ((ti (make-array (* 2 n)))
	(to (make-array (* 2 n))))
    (dotimes1 (i n)
      (setf (svref ti (* i 2))
	    (vector (+ i 1.0)))
      (setf (svref to (* i 2))
	    (vector (if (evenp i) 0.5 -0.5)))
      (setf (svref ti (1+ (* i 2)))
	    (vector (- (+ i 1.0))))
      (setf (svref to (1+ (* i 2)))
	    (vector (if (evenp i) -0.5 0.5))))
    (setq *training-inputs* ti)
    (setq *training-outputs* to)
    (setq *test-inputs* ti)
    (setq *test-outputs* to))
  (build-net 1 1)
  (init-net))

;;; Call this with something like (BUILD-ZIG-ZAG 4), then call
;;; something like (train 100 100 25).


;;; Two spirals problem.

(defun build-two-spirals (&optional (n 97))
  "Build N point-pairs of the two-spiral problem, with standard default
  of 97 pairs."
  (declare (fixnum n))
  (setq *ninputs* 2)
  (setq *noutputs* 1)
  (let ((ti (make-array (* 2 n)))
	(to (make-array (* 2 n))))
    (dotimes1 (i n)
      (let* ((angle (/ (* i (coerce pi 'short-float)) 16.0))
	     (radius (/ (* 6.5 (- 104.0 i)) 104))
	     (x (* radius (sin angle)))
	     (y (* radius (cos angle))))
	(setf (svref ti (* i 2))
	      (vector x y))
	(setf (svref to (* i 2))
	      (vector 0.5))
	(setf (svref ti (1+ (* i 2)))
	      (vector (- x) (- y)))
	(setf (svref to (1+ (* i 2)))
	      (vector -0.5))))
    ;; Put the inner part of the spiral first on the list.
    (setq ti (nreverse ti))
    (setq to (nreverse to))
    (setq *training-inputs* ti)
    (setq *training-outputs* to)
    (setq *test-inputs* ti)
    (setq *test-outputs* to))
  (build-net 2 1)
  (init-net))

;;; To run this, call (BUILD-TWO-SPIRALS), set various control parameters,
;;; and then call something like (TRAIN 100 100 25).

;;; For parameters, try these:
;;; SigOff 0.10, WtRng 1.00, WtMul 1.00
;;; OMu 2.00, OEps 1.00, ODcy 0.0001, OPat 12, OChange 0.010
;;; IMu 2.00, IEps 100.00, IDcy 0.00000, IPat 8, IChange 0.030
;;; Utype :SIGMOID, Otype :SIGMOID, RawErr NIL, Pool 8

(defvar *save-random-state*
  (make-random-state))

(defun time-two-spirals ()
  (setq *random-state* (make-random-state *save-random-state*))
  (build-two-spirals)
  (time (train 100 100 25)))
	
;;; The End.

