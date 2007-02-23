;;; -*- Mode: Lisp; Package: KERNEL; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/code/irrat-dd.lisp,v 1.6 2006/07/19 15:29:00 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains all the irrational functions for double-double
;;; float.  The algorithms and coefficients come from the Cephes math
;;; library.  See http://www.moshier.net/#Cephes and
;;; http://www.netlib.org/cephes/.
;;;
;;; Author: Raymond Toy.
;;; 

(in-package "KERNEL")

;;;; Random constants, utility functions.

#+nil
(defconstant max-log
  1.1356523406294143949491931077970764891253w4
  "log(most-positive-double-double-float)")

#+nil
(defconstant min-log
  -1.143276959615573793352782661133116431383730w4
  "log(least-positive-double-double-float")

(defconstant max-log
  7.0978271289338399678773454114191w2
  "log(most-positive-double-double-float)")

(defconstant min-log
  -7.4444007192138126231410729844608w2
  "log(least-positive-double-double-float")


(defconstant loge2
  0.6931471805599453094172321214581765680755001w0
  "log(2)")

(defconstant log2e
  1.442695040888963407359924681001892137426646w0
  "Log base 2 of e")

(defconstant log2ea
  4.4269504088896340735992468100189213742664595w-1
  "log2(e)-1")

(defconstant dd-pi
  3.141592653589793238462643383279502884197169w0
  "Pi")

(defconstant dd-pi/2
  1.570796326794896619231321691639751442098585w0
  "Pi/2")

(defconstant dd-pi/4
  0.7853981633974483096156608458198757210492923w0
  "Pi/4")

;; log2-c1 and log-c2 are log(2) arranged in such a way that log2-c1 +
;; log2-c2 is log(2) to an accuracy greater than double-double-float.
(defconstant log2-c1
  6.93145751953125w-1)

(defconstant log2-c2
  1.428606820309417232121458176568075500134w-6)

(defconstant sqrt-1/2
  0.7071067811865475244008443621048490392848w0
  "Sqrt(2)")

;; Evaluate polynomial
(defun poly-eval (x coef)
  (declare (type double-double-float x)
	   (type (simple-array double-double-float (*)) coef)
	   (optimize (speed 3) (space 0)))
  ;; y = C0 + C1*x + C2*x^2 + ...
  ;;
  ;; But coefficients are stored in reverse (descending powers) order:
  ;; coef[0] = CN, ..., coef[N] = C0.
  (let ((y 0w0))
    (declare (type double-double-float y))
    (loop for c across coef do
	 (setf y (+ (* y x)
		    c)))
    y))

(defun poly-eval-1 (x coef)
  (declare (type double-double-float x)
	   (type (simple-array double-double-float (*)) coef)
	   (optimize (speed 3) (space 0)))
  ;; Like poly-eval, except it assumes coef[N] = 1 and is omitted.
  (let ((y 1w0))
    (declare (type double-double-float y))
    (loop for c across coef do
	 (setf y (+ (* y x)
		    c)))
    y))


;;; dd-%expm1
;;;
;;; exp(x)-1
;;; Range reduction is accomplished by separating the argument
;;; into an integer k and fraction f such that 
;;;
;;;     x    k  f
;;;    e  = 2  e.
;;;
;;; An expansion x + .5 x^2 + x^3 R(x) approximates exp(f) - 1
;;; in the basic range [-0.5 ln 2, 0.5 ln 2].
;;;
;;;
;;; ACCURACY:
;;;
;;;                      Relative error:
;;; arithmetic   domain     # trials      peak         rms
;;;    IEEE    -79,+MAXLOG    100,000     1.7e-34     4.5e-35
;;;

(let ((p (make-array 8 :element-type 'double-double-float
		     :initial-contents '(
					 -4.888737542888633647784737721812546636240w-1
					 4.401308817383362136048032038528753151144w1
					 -1.716772506388927649032068540558788106762w3
					 4.578962475841642634225390068461943438441w4
					 -7.212432713558031519943281748462837065308w5
					 8.944630806357575461578107295909719817253w6
					 -5.722847283900608941516165725053359168840w7
					 2.943520915569954073888921213330863757240w8)))
      (q (make-array 8 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0
					 1.766112549341972444333352727998584753865w9
					 -7.848989743695296475743081255027098295771w8
					 1.615869009634292424463780387327037251069w8
					 -2.019684072836541751428967854947019415698w7
					 1.682912729190313538934190635536631941751w6
					 -9.615511549171441430850103489315371768998w4
					 3.697714952261803935521187272204485251835w3
					 -8.802340681794263968892934703309274564037w1)))
      ;; ln 2^-114
      (minarg -7.9018778583833765273564461846232128760607w1))
  (declare (type double-double-float minarg))
  ;; exp(x) - 1 = x + 0.5*x^2+x^3*P(x)/Q(x), for -log(2)/2 < x <
  ;; log(2)/2, where the coefficients of P and Q are given Pn and Qn
  ;; above.  Theoretical peak relative error = 8.1e-36.
  (defun dd-%expm1 (x)
    "exp(x) - 1"
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    ;; Range reduction is accomplished by separating the argument
    ;; into an integer k and fraction f such that 
    ;;
    ;;     x    k  f
    ;;    e  = 2  e.
    ;;
    ;; An expansion x + .5 x^2 + x^3 R(x) approximates exp(f) - 1
    ;; in the basic range [-0.5 ln 2, 0.5 ln 2].
    (when (> x max-log)
      (return-from dd-%expm1
	(kernel:%make-double-double-float #.ext:double-float-positive-infinity
					  #.ext:double-float-positive-infinity)))
    (when (< x minarg)
      (return-from dd-%expm1 -1w0))

    ;; Express x = ln(2)*(k+remainder), remainder not exceeding 1/2
    (let* ((xx (+ log2-c1 log2-c2))
	   (k (floor (+ 1/2 (/ (the (double-double-float * 710w0) x) xx))))
	   (px (coerce k 'double-double-float))
	   (qx 0w0))
      (declare (type double-double-float xx px qx))
      ;; remainder times ln 2
      (decf x (* px log2-c1))
      (decf x (* px log2-c2))

      ;; Approximate exp(remainder*ln(2))
      #+nil
      (setf px (* x
		  (+ p0
		     (* x
			(+ p1
			   (* x
			      (+ p2
				 (* x
				    (+ p3
				       (* x
					  (+ p4
					     (* x
						(+ p5
						   (* x
						      (+ p6
							 (* x p7))))))))))))))))
      #+nil
      (setf qx (+ q0
		  (* x
		     (+ q1
			(* x
			   (+ q2
			      (* x
				 (+ q3
				    (* x
				       (+ q4
					  (* x
					     (+ q5
						(* x
						   (+ q6
						      (* x
							 (+ x q7))))))))))))))))
      (setf px (poly-eval x p))
      (setf qx (poly-eval-1 x q))
      (setf xx (* x x))
      (setf qx (+ x (+ (* 0.5w0 xx)
		       (/  (* xx px)
			   qx))))
      ;; exp(x) = exp(k*ln(2))*exp(remainder*ln(2)) = 2^k*exp(remainder*ln(2))
      ;;
      ;; We have qx = exp(remainder*ln(2))-1, so exp(x) - 1 =
      ;; 2^k*(qx+1)-1 = 2^k*qx + 2^k - 1
      (setf px (scale-float 1w0 k))
      (+ (* px qx)
	 (- px 1)))))

;;; dd-%exp
;;; exp(x)
;;; Range reduction is accomplished by separating the argument
;;; into an integer k and fraction f such that
;;;
;;;     x    k  f
;;;    e  = 2  e.
;;;
;;; A Pade' form of degree 2/3 is used to approximate exp(f) - 1
;;; in the basic range [-0.5 ln 2, 0.5 ln 2].
;;;
;;;
;;; ACCURACY:
;;;
;;;                      Relative error:
;;; arithmetic   domain     # trials      peak         rms
;;;    IEEE      +-MAXLOG    100,000     2.6e-34     8.6e-35
;;;
;;;
;;; Error amplification in the exponential function can be
;;; a serious matter.  The error propagation involves
;;; exp( X(1+delta) ) = exp(X) ( 1 + X*delta + ... ),
;;; which shows that a 1 lsb error in representing X produces
;;; a relative error of X times 1 lsb in the function.
;;; While the routine gives an accurate result for arguments
;;; that are exactly represented by a long double precision
;;; computer number, the result contains amplified roundoff
;;; error for large arguments not exactly represented.

(let ((p (make-array 5 :element-type 'double-double-float
		     :initial-contents
		     '(
		       3.279723985560247033712687707263393506266w-10
		       6.141506007208645008909088812338454698548w-7
		       2.708775201978218837374512615596512792224w-4
		       3.508710990737834361215404761139478627390w-2
		       9.999999999999999999999999999999999998502w-1
		       )))
      (q (make-array 6 :element-type 'double-double-float
		     :initial-contents
		     '(
		       2.980756652081995192255342779918052538681w-12
		       1.771372078166251484503904874657985291164w-8
		       1.504792651814944826817779302637284053660w-5
		       3.611828913847589925056132680618007270344w-3
		       2.368408864814233538909747618894558968880w-1
		       2.000000000000000000000000000000000000150w0
		       )))
      ;; C1 + C2 = ln 2
      (c1 (- log2-c1))
      (c2 (- log2-c2)))
  (declare (type double-double-float c1 c2))
  ;; p and q are Pade coefficients for exp(x) - 1.  Theoretical peak
  ;; relative error = 2.2e-37, relative peak error spread = 9.2e-38.
  (defun dd-%exp (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (when (> x max-log)
      (return-from dd-%exp
	(kernel:%make-double-double-float #.ext:double-float-positive-infinity
					  #.ext:double-float-positive-infinity)))
    (when (< x min-log)
      (return-from dd-%exp 0w0))
    ;; Express
    ;;   exp(x) = exp(g)*2^n
    ;;          = exp(g)*exp(n*log(2))
    ;;          = exp(g + n * log2)
    (let* ((n (floor (+ 0.5w0 (* x log2e))))
	   (px (coerce n 'double-double-float)))
      (declare (type double-double-float px))
      (incf x (* px c1))
      (incf x (* px c2))
      ;; Rational approx for exponential of fractional part:
      ;; exp(x) = 1+2*x*p(x^2)/(q(x^2)-p(x^2))
      ;;
      ;; [The above comment seems not to match the code below.  But
      ;; perhaps q(x) isn't what I think it is.]
      (let ((xx (* x x)))
	(setf px (* x (poly-eval xx p)))
	(setf xx (poly-eval xx q))
	(setf x (/ px (- xx px)))
	(setf x (+ 1 x x))
	(scale-float x n)))))


;;; dd-%log
;;; log(x), natural logarithm.
;;; The argument is separated into its exponent and fractional
;;; parts.  If the exponent is between -1 and +1, the logarithm
;;; of the fraction is approximated by
;;;
;;;     log(1+x) = x - 0.5 x**2 + x**3 P(x)/Q(x).
;;;
;;; Otherwise, setting  z = 2(x-1)/x+1),
;;; 
;;;     log(x) = z + z**3 P(z)/Q(z).
;;;
;;;
;;;
;;; ACCURACY:
;;;
;;;                      Relative error:
;;; arithmetic   domain     # trials      peak         rms
;;;    IEEE   exp(+-MAXLOGL) 36,000      9.5e-35     4.1e-35
;;;

(let ((p (make-array 13 :element-type 'double-double-float
		     :initial-contents
		     '(1.538612243596254322971797716843006400388w-6
		       4.998469661968096229986658302195402690910w-1
		       2.321125933898420063925789532045674660756w1
		       4.114517881637811823002128927449878962058w2
		       3.824952356185897735160588078446136783779w3
		       2.128857716871515081352991964243375186031w4
		       7.594356839258970405033155585486712125861w4
		       1.797628303815655343403735250238293741397w5
		       2.854829159639697837788887080758954924001w5
		       3.007007295140399532324943111654767187848w5
		       2.014652742082537582487669938141683759923w5
		       7.771154681358524243729929227226708890930w4
		       1.313572404063446165910279910527789794488w4)))
      (q (make-array 12 :element-type 'double-double-float
		     :initial-contents
		     '(
		       ;; 1.000000000000000000000000000000000000000w0
		       4.839208193348159620282142911143429644326w1
		       9.104928120962988414618126155557301584078w2
		       9.147150349299596453976674231612674085381w3
		       5.605842085972455027590989944010492125825w4
		       2.248234257620569139969141618556349415120w5
		       6.132189329546557743179177159925690841200w5
		       1.158019977462989115839826904108208787040w6
		       1.514882452993549494932585972882995548426w6
		       1.347518538384329112529391120390701166528w6
		       7.777690340007566932935753241556479363645w5
		       2.626900195321832660448791748036714883242w5
		       3.940717212190338497730839731583397586124w4)))
      (r (make-array 6 :element-type 'double-double-float
		     :initial-contents
		     '(-8.828896441624934385266096344596648080902w-1
		       8.057002716646055371965756206836056074715w1
		       -2.024301798136027039250415126250455056397w3
		       2.048819892795278657810231591630928516206w4
		       -8.977257995689735303686582344659576526998w4
		       1.418134209872192732479751274970992665513w5
		       )))
      (s (make-array 6 :element-type 'double-double-float
		     :initial-contents
		     '(	;; 1.000000000000000000000000000000000000000w0
		       -1.186359407982897997337150403816839480438w2
		       3.998526750980007367835804959888064681098w3
		       -5.748542087379434595104154610899551484314w4
		       4.001557694070773974936904547424676279307w5
		       -1.332535117259762928288745111081235577029w6
		       1.701761051846631278975701529965589676574w6))))
  ;; p and q are coefficients in the expansion for log(1+x) = x -
  ;; x^2/2 + x^3*p(x)/q(x), where 1/sqrt(2) <= 1+x < sqrt(2).
  ;; Theoretical peak relative error = 5.3e-37, relative peak error
  ;; spread = 2.3e-14.
  ;;
  ;; R and S are coefficients for log(x) = z + z^3*r(z^2)/s(z^2),
  ;; where z = 2*(x-1)/(x+1), where 1/sqrt(2) <= x < sqrt(2).
  ;; Theoretical peak relative error = 1.1e-35, relative peak error
  ;; spread 1.1e-9.
  ;;
  ;; 
  (defun dd-%log (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    ;; Separate mantissa from exponent
    (multiple-value-bind (x e)
	(decode-float x)
      (declare (type double-double-float x)
	       (type double-float-exponent e))
      (let ((z 0w0)
	    (y 0w0))
	(declare (type double-double-float z y))
	;; Logarithm using log(x) = z + z^3*P(z^2)/Q(z^2); where z = 2*(x-1)/x + 1;
	(cond ((or (> e 2)
		   (< e -2))
	       (cond ((< x sqrt-1/2)
		      ;; 2*(2*x-1)/(2*x+1)
		      (decf e)
		      (setf z (- x 0.5w0))
		      (setf y (+ (* 0.5w0 z) 0.5w0)))
		     (t
		      ;; 2*(x-1)/(x+1)
		      (setf z (- x 0.5w0))
		      (decf z 0.5w0)
		      (setf y (+ (* 0.5w0 x) 0.5w0))))
	       (setf x (/ z y))
	       (setf z (* x x))
	       (setf z (* x (/ (* z (poly-eval z r))
				 (poly-eval-1 z s))))
	       (incf z (* e log2-c2))
	       (incf z x)
	       (incf z (* e log2-c1)))
	      (t
	       ;; Log using log(1+x) = x - 0.5*x^2+x^3*p(x)/q(x)
	       (cond ((< x sqrt-1/2)
		      (decf e)
		      (setf x (- (scale-float x 1) 1)))
		     (t
		      (decf x)))
	       (setf z (* x x))
	       (setf y (* x (* z (/ (poly-eval x p)
				    (poly-eval-1 x q)))))
	       (incf y (* e log2-c2))
	       ;; z = y - 0.5*z
	       (setf z (- y (scale-float z -1)))
	       (incf z x)
	       (incf z (* e log2-c1))
	       ))))))

;;; dd-%log1p(x)
;;; log(1+x)
;;;
;;; The argument 1+x is separated into its exponent and fractional
;;; parts.  If the exponent is between -1 and +1, the logarithm
;;; of the fraction is approximated by
;;;
;;;     log(1+x) = x - 0.5 x^2 + x^3 P(x)/Q(x).
;;;
;;; Otherwise, setting  z = 2(x-1)/x+1),
;;; 
;;;     log(x) = z + z^3 P(z)/Q(z).
;;;
;;;
;;;
;;; ACCURACY:
;;;
;;;                      Relative error:
;;; arithmetic   domain     # trials      peak         rms
;;;    IEEE      -1, 8       100000      1.9e-34     4.3e-35

(let ((p (make-array 13 :element-type 'double-double-float
		     :initial-contents '(
					 1.538612243596254322971797716843006400388w-6
					 4.998469661968096229986658302195402690910w-1
					 2.321125933898420063925789532045674660756w1
					 4.114517881637811823002128927449878962058w2
					 3.824952356185897735160588078446136783779w3
					 2.128857716871515081352991964243375186031w4
					 7.594356839258970405033155585486712125861w4
					 1.797628303815655343403735250238293741397w5
					 2.854829159639697837788887080758954924001w5
					 3.007007295140399532324943111654767187848w5
					 2.014652742082537582487669938141683759923w5
					 7.771154681358524243729929227226708890930w4
					 1.313572404063446165910279910527789794488w4
					 )))
      (q (make-array 12 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0
					 4.839208193348159620282142911143429644326w1
					 9.104928120962988414618126155557301584078w2
					 9.147150349299596453976674231612674085381w3
					 5.605842085972455027590989944010492125825w4
					 2.248234257620569139969141618556349415120w5
					 6.132189329546557743179177159925690841200w5
					 1.158019977462989115839826904108208787040w6
					 1.514882452993549494932585972882995548426w6
					 1.347518538384329112529391120390701166528w6
					 7.777690340007566932935753241556479363645w5
					 2.626900195321832660448791748036714883242w5
					 3.940717212190338497730839731583397586124w4
					 )))
      (r (make-array 6 :element-type 'double-double-float
		     :initial-contents '(-8.828896441624934385266096344596648080902w-1
					 8.057002716646055371965756206836056074715w1
					 -2.024301798136027039250415126250455056397w3
					 2.048819892795278657810231591630928516206w4
					 -8.977257995689735303686582344659576526998w4
					 1.418134209872192732479751274970992665513w5
					 )))
      (s (make-array 6 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0
					 -1.186359407982897997337150403816839480438w2
					 3.998526750980007367835804959888064681098w3
					 -5.748542087379434595104154610899551484314w4
					 4.001557694070773974936904547424676279307w5
					 -1.332535117259762928288745111081235577029w6
					 1.701761051846631278975701529965589676574w6
					 )))
)
  ;; Coefficients for log(1+x) = x - x^2 / 2 + x^3 P(x)/Q(x)
  ;; 1/sqrt(2) <= 1+x < sqrt(2)
  ;; Theoretical peak relative error = 5.3e-37,
  ;; relative peak error spread = 2.3e-14
  (defun dd-%log1p (xm1)
    (declare (type double-double-float xm1)
	     (optimize (space 0)))
    (let ((x (+ xm1 1))
	  (z 0w0)
	  (y 0w0))
      (declare (type double-double-float x y z)
	       (optimize (speed 3)))
      (multiple-value-bind (x e)
	  (decode-float x)
	(declare (type double-double-float x)
		 (type double-float-exponent e))
	(cond ((or (> e 2)
		   (< e -2))
	       ;; Log using log(x) = z + z^3*P(z^2)/Q(z^2)
	       (cond ((< x sqrt-1/2)
		      (decf e)
		      (setf z (- x 0.5w0))
		      (setf y (+ 0.5w0 (* 0.5w0 z))))
		     (t
		      (setf z (- x 0.5w0))
		      (decf z 0.5w0)
		      (setf y (+ 0.5w0 (* 0.5w0 x)))))
	       (setf x (/ z y))
	       (setf z (* x x))
	       (setf z (* x (/ (* z (poly-eval z r))
			       (poly-eval-1 z s))))
	       (incf z (* e log2-c2))
	       (incf z x)
	       (incf z (* e log2-c1)))
	      (t
	       ;; Log using log(1+x) = x - 0.5*x^2 + x^3*p(x)/q(x)
	       (cond ((< x sqrt-1/2)
		      (decf e)
		      (if (/= e 0)
			  (setf x (- (* 2 x) 1))
			  (setf x xm1)))
		     (t
		      (if (/= e 0)
			  (decf x 1)
			  (setf x xm1))))
	       (setf z (* x x))
	       (setf y (* x (/ (* z (poly-eval x p))
			       (poly-eval-1 x q))))
	       (incf y (* e log2-c2))
	       (setf z (- y (* 0.5w0 z)))
	       (incf z x)
	       (incf z (* e log2-c1))))))))

(let ((P (make-array 6 :element-type 'double-double-float
		     :initial-contents '(
					 1.622194395724068297909052717437740288268w3
					 1.124862584587770079742188354390171794549w6
					 3.047548980769660162696832999871894196102w8
					 3.966215348072348368191433063260384329745w10
					 2.375869584584371194838551715348965605295w12
					 6.482835792103233269752264509192030816323w13
					 )))
      (Q (make-array 6 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0 */
					 -9.101683853129357776079049616394849086007w2
					 4.486400519836461218634448973793765123186w5
					 -1.492531313030440305095318968983514314656w8
					 3.457771488856930054902696708717192082887w10
					 -5.193289868803472640225483235513427062460w12
					 3.889701475261939961851358705515223019890w14))))
  (defun dd-%sinh (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (let ((a (abs x)))
      (declare (type double-double-float a))
      (cond ((> a 1)
	     (setf a (dd-%exp a))
	     (setf a (- (* 0.5w0 a) (/ 0.5w0 a)))
	     (if (< x 0)
		 (- a)
		 a))
	    (t
	     (setf a (* a a))
	     (let ((pp (poly-eval a p))
		   (qq (poly-eval-1 a q)))
	       (+ x (* x a
		       (/ pp qq)))))))))

#+nil
(defun dd-%sinh (x)
  (declare (type double-double-float x))
  (let ((a (abs x)))
    (declare (type double-double-float a))
    (cond ((> a 1)
	   (setf a (dd-%exp a))
	   (setf a (- (* 0.5w0 a) (/ 0.5w0 a)))
	   (if (< x 0)
	       (- a)
	       a))
	  (t
	   ;; Use sinh(x) = 1/2*(D(x)+D(x)/(1+D(x))), here D(x) =
	   ;; exp(x)-1.
	   (let ((d (dd-%expm1 x)))
	     (* 0.5w0 (+ d (/ d (+ 1 d)))))))))

(defun dd-%cosh (x)
  (declare (type double-double-float x)
	   (optimize (speed 3) (space 0)))
  (let ((y (dd-%exp x)))
    (scale-float (+ y (/ y)) -1)))

(let ((P (make-array 6 :element-type 'double-double-float
		     :initial-contents '(-6.505693197948351084912624750702492767503w-6
					 -9.804083860188429726356968570322356183383w-1
					 -5.055287638900473250703725789725376004355w2
					 -7.307477148073823966594990496301416814519w4
					 -3.531606586182691280701462523692471322688w6
					 -4.551377146142783468144190926206842300707w7
					 )))
      (Q (make-array 5 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0 */
					 5.334865598460027935735737253027154828002w2
					 8.058475607422391042912151298751537172870w4
					 4.197073523796142343374222405869721575491w6
					 6.521134551226147545983467868553677881771w7
					 1.365413143842835040443257277862054198329w8))))
  (defun dd-%tanh (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (let ((z (abs x)))
      (declare (type double-double-float z))
      (cond ((> z (* 0.5w0 max-log))
	     (if (> x 0)
		 1w0
		 -1w0))
	    ((> z 0.625w0)
	     (let ((s (dd-%exp (* 2 z))))
	       (setf z (- 1 (/ 2 (+ s 1))))
	       (if (minusp x)
		   (- z)
		   z)))
	    (t
	     (let* ((s (* x x)))
	       (declare (optimize speed))
	       (setf z (/ (poly-eval s p)
			  (poly-eval-1 s q)))
	       (setf z (* x s z))
	       (+ x z)))))))

(let ((P (make-array 10 :element-type 'double-double-float
		     :initial-contents '(
					 -9.217569843805850417698565442251656375681w-1
					 5.321929116410615470118183794063211260728w1
					 -9.139522976807685333981548145417830690552w2
					 7.204314536952949779101646454146682033772w3
					 -3.097809640165146436529075324081668598891w4
					 7.865376554210973897486215630898496100534w4
					 -1.211716814094785128366087489224821937203w5
					 1.112669508789123834670923967462068457013w5
					 -5.600242872292477863751728708249167956542w4
					 1.188901082233997739779618679364295772810w4
					 )))
      (Q (make-array 10 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0 */
					 -6.807348436010016270202879229504392062418w1
					 1.386763299649315831625106608182196351693w3
					 -1.310805752656879543134785263832907269320w4
					 6.872174720355764193772953852564737816928w4
					 -2.181008360536226513009076189881617939380w5
					 4.362736119602298592874941767284979857248w5
					 -5.535251007539393347687001489396152923502w5
					 4.321594849688346708841188057241308805551w5
					 -1.894075056489862952285849974761239845873w5
					 3.566703246701993219338856038092901974725w4
					 ))))
  (defun dd-%atanh (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (cond ((minusp x)
	   (- (the double-double-float (dd-%atanh (- x)))))
	  ((< x 1w-12)
	   x)
	  ((< x  0.5w0)
	   (let ((z (* x x)))
	     (+ x (* x z (/ (poly-eval z p)
			    (poly-eval-1 z q))))))
	  (t
	   (* 0.5w0 (dd-%log (/ (+ 1 x)
			       (- 1 x))))))))


(let ((P (make-array 9 :element-type 'double-double-float
		     :initial-contents '(
					 -8.104404283317298189545629468767571317688w-1
					 -4.954206127425209147110732546633675599008w1
					 -8.438175619831548439550086251740438689853w2
					 -6.269710069245210459536983820505214648057w3
					 -2.418935474493501382372711518024193326434w4
					 -5.208121780431312783866941311277024486498w4
					 -6.302755086521614763280617114866439227971w4
					 -4.003566436224198252093684987323233921339w4
					 -1.037690841528359305134494613113086980551w4
					 )))
      (Q (make-array 9 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0 */
					 8.175806439951395194771977809279448392548w1
					 1.822215299975696008284027212745010251320w3
					 1.772040003462901790853111853838978236828w4
					 9.077625379864046240143413577745818879353w4
					 2.675554475070211205153169988669677418808w5
					 4.689758557916492969463473819426544383586w5
					 4.821923684550711724710891114802924039911w5
					 2.682316388947175963642524537892687560973w5
					 6.226145049170155830806967678679167550122w4))))
  (defun dd-%asinh (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (cond ((minusp x)
	   (- (the double-double-float (dd-%asinh (- x)))))
	  #+nil
	  ((> x 1w10)
	   (+ loge2 (dd-%log x)))
	  ((< x 0.5w0)
	   (let* ((z (* x x))
		  (a (* z (/ (poly-eval z p)
			     (poly-eval-1 z q)))))
	     (+ (* a x) x)))
	  (t
	   (dd-%log (+ x (sqrt (1+ (* x x)))))))))

(let ((P (make-array 10 :element-type 'double-double-float
		     :initial-contents '(
					 1.895467874386341763387398084072833727168w-1
					 6.443902084393244878979969557171256604767w1
					 3.914593556594721458616408528941154205393w3
					 9.164040999602964494412169748897754668733w4
					 1.065909694792026382660307834723001543839w6
					 6.899169896709615182428217047370629406305w6
					 2.599781868717579447900896150777162652518w7
					 5.663733059389964024656501196827345337766w7
					 6.606302846870644033621560858582696134512w7
					 3.190482951215438078279772140481195200593w7
					 )))
      (Q (make-array 9 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0 */
					 1.635418024331924674147953764918262009321w2
					 7.290983678312632723073455563799692165828w3
					 1.418207894088607063257675159183397062114w5
					 1.453154285419072886840913424715826321357w6
					 8.566841438576725234955968880501739464425w6
					 3.003448667795089562511136059766833630017w7
					 6.176592872899557661256383958395266919654w7
					 6.872176426138597206811541870289420510034w7
					 3.190482951215438078279772140481195226621w7
					 ))))
  (defun dd-%acosh (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (cond ((> x 1w17)
	   (+ loge2 (dd-%log x)))
	  (t
	   (let ((z (- x 1)))
	     (cond ((< z 0.5w0)
		    (* (sqrt (* 2 z))
		       (/ (poly-eval z p)
			  (poly-eval-1 z q))))
		   (t
		    (dd-%log (+ x (sqrt (* z (+ 1 x))))))))))))

(let ((P (make-array 10 :element-type 'double-double-float
		     :initial-contents '(
					 -8.067112765482705313585175280952515549833w-1
					 4.845649797786849136525020822000172350977w1
					 -8.510195404865297879959793548843395926847w2
					 6.815196841370292688574521445731895826485w3
					 -2.967135182120339728996157454994675519735w4
					 7.612250656518818109652985996692466409670w4
					 -1.183360579752620455689557157684221905030w5
					 1.095432262510413338755837156377401348063w5
					 -5.554124580991113991999636773382495788705w4
					 1.187132626694762543537732514905488896985w4
					 )))
      (Q (make-array 10 :element-type 'double-double-float
		     :initial-contents '(
					 ;;  1.000000000000000000000000000000000000000w0 */
					 -8.005471061732009595694099899234272342478w1
					 1.817324228942812880965069608562483918025w3
					 -1.867017317425756524289537002141956583706w4
					 1.048196619402464497478959760337779705622w5
					 -3.527040897897253459022458866536165564103w5
					 7.426302422018858001691440351763370029242w5
					 -9.863068411558756277454631976667880674474w5
					 8.025654653926121907774766642393757364326w5
					 -3.653000557802254281954969843055623398839w5
					 7.122795760168575261226395089432959614179w4
					 ))))
  (defun dd-%asin (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (cond ((minusp x)
	   (- (the double-double-float (dd-%asin (- x)))))
	  #+nil
	  ((< x 1w-8)
	   x)
	  (t
	   (let ((flag nil)
		 (z 0w0)
		 (zz 0w0))
	     (declare (type double-double-float z zz))
	     (cond ((> x 0.5w0)
		    (setf zz (- 0.5w0 x))
		    (setf zz (scale-float (+ zz 0.5w0) -1))
		    (setf z (sqrt zz))
		    (setf flag t))
		   (t
		    (setf z x)
		    (setf zz (* z z))
		    (setf flag nil)))
	     (let ((p (* zz (/ (poly-eval zz p)
			       (poly-eval-1 zz q)))))
	       (setf z (+ (* z p) z))
	       (when flag
		 (setf z (+ z z))
		 (setf z (- dd-pi/2 z)))
	       z))))))

(defun dd-%acos (x)
  (declare (type double-double-float x)
	   (optimize (speed 3) (space 0)))
  (cond ((< x -0.5w0)
	 (- dd-pi
	    (* 2 (dd-%asin (sqrt (* 0.5w0 (+ 1 x)))))))
	((> x 0.5w0)
	 (* 2 (dd-%asin (sqrt (* 0.5w0 (- 1w0 x))))))
	(t
	 (- dd-pi/2 (dd-%asin x)))))

(let ((P (make-array 9 :element-type 'double-double-float
		     :initial-contents '(
					 -6.635810778635296712545011270011752799963w-4
					 -8.768423468036849091777415076702113400070w-1
					 -2.548067867495502632615671450650071218995w1
					 -2.497759878476618348858065206895055957104w2
					 -1.148164399808514330375280133523543970854w3
					 -2.792272753241044941703278827346430350236w3
					 -3.696264445691821235400930243493001671932w3
					 -2.514829758941713674909996882101723647996w3
					 -6.880597774405940432145577545328795037141w2
					 )))
      (Q (make-array 8 :element-type 'double-double-float
		     :initial-contents '(
					 ;; 1.000000000000000000000000000000000000000w0 */
					 3.566239794444800849656497338030115886153w1
					 4.308348370818927353321556740027020068897w2
					 2.494680540950601626662048893678584497900w3
					 7.928572347062145288093560392463784743935w3
					 1.458510242529987155225086911411015961174w4
					 1.547394317752562611786521896296215170819w4
					 8.782996876218210302516194604424986107121w3
					 2.064179332321782129643673263598686441900w3
					 )))

      ;; tan( 3*pi/8 )
      (T3P8  2.414213562373095048801688724209698078569672w0)
      ;; tan( pi/8 )
      (TP8 0.414213562373095048801688724209698078569672w0))
  (declare (type double-double-float t3p8 tp8))
  (defun dd-%atan (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (when (minusp x)
      (return-from dd-%atan (- (the double-double-float (dd-%atan (- x))))))
    ;; range reduction
    (let ((y 0w0))
      (declare (type double-double-float y))
      (cond ((> x t3p8)
	     (setf y dd-pi/2)
	     (setf x (/ -1 x)))
	    ((> x tp8)
	     (setf y dd-pi/4)
	     (setf x (/ (- x 1)
			(+ x 1))))
	    (t
	     (setf y 0w0)))
      ;; Rational form in x^2
      (let ((z (* x x)))
	(setf y (+ y
		   (* (/ (poly-eval z p)
			 (poly-eval-1 z q))
		      z x)
		   x))
	y))))

(defun dd-%atan2 (y x)
  (declare (type double-double-float x y)
	   (optimize (speed 3) (space 0)))
  (let ((code 0)
	(w 0w0))
    (declare (type (integer 0 3) code)
	     (type double-double-float w))
    (when (minusp x)
      (setf code 2))
    (when (minusp y)
      (setf code (logior code 1)))
    (when (zerop x)
      (unless (zerop (logand code 1))
	(return-from dd-%atan2 (- dd-pi/2)))
      (when (zerop y)
	(return-from dd-%atan2 0w0))
      (return-from dd-%atan2 dd-pi/2))
    (when (zerop y)
      (return-from dd-%atan2
	(if (zerop (logand code 2))
	    0w0
	    dd-pi)))
    (setf w (ecase code
	      (0 0w0)
	      (1 0w0)
	      (2 dd-pi)
	      (3 (- dd-pi))))

    (+ w (dd-%atan (/ y x)))))
       

;;
;; Here are the fractional digits of pi, in hex.
;;
;;  243f6a8885 a308d31319 8a2e037073 44a4093822 299f31d008 2efa98ec4e 6c89452821 e638d01377 be5466cf34 e90c6cc0ac
;; 29b7c97c50 dd3f84d5b5 b547091792 16d5d98979 fb1bd1310b a698dfb5ac 2ffd72dbd0 1adfb7b8e1 afed6a267e 96ba7c9045 
;;
;; We want to express pi/4 in 3 parts such that the sum of the parts
;; is exactly the value of pi to the higher precision.  For
;; double-double, we want 53 bits in each part.
;;
;; pi/4 in hex is
;;
;; C90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F2411
;;
;; 
#||
(let* ((frac #xC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F2411)
       (len (integer-length frac)))
  (format t "len = ~D bits~%" len)
  (format t "pi/4    ~,,' ,106:b~%" (ldb (byte (* 4 106) (- len (* 4 106))) frac)))
->
pi/4    11001001000011111101101010100010001000010110100011000 01000110100110001001100011001100010100010111000000011 01110000011100110100010010100100000010010011100000100 01000101001100111110011000111010000000010000010111011

||#
;; d1 = (scale-float (float #b1100100100001111110110101010001000100001011010001100001000110100110001001100011001100010100010111000000011 1w0) -106)
;; d2 = (scale-float (float #b0111000001110011010001001010010000001001001110000010001000101001100111110011000111010000000010000010111011 1w0) (* 2 -106))
;; d3 = (scale-float (float #b1110101001100011101100010011100110110010001001010001010010100000100001111001100011100011010000000100110111 1w0) (* 3 -106))
;; d4 = (scale-float (float #b0111101111100101010001100110110011110011010011101001000011000110110011000000101011000010100110110111110010 1w0) (* 4 -106))

;; sin(x) = x + x^3 P(x^2)
;; Theoretical peak relative error = 5.6e-39
;; relative peak error spread = 1.7e-9

(defconstant sincof
  (make-array 12 :element-type 'double-double-float
	      :initial-contents
	      '(
		6.410290407010279602425714995528976754871w-26
		-3.868105354403065333804959405965295962871w-23
		1.957294039628045847156851410307133941611w-20
		-8.220635246181818130416407184286068307901w-18
		2.811457254345322887443598804951004537784w-15
		-7.647163731819815869711749952353081768709w-13
		1.605904383682161459812515654720205050216w-10
		-2.505210838544171877505034150892770940116w-8
		2.755731922398589065255731765498970284004w-6
		-1.984126984126984126984126984045294307281w-4
		8.333333333333333333333333333333119885283w-3
		-1.666666666666666666666666666666666647199w-1
		)))

;; cos(x) = 1 - .5 x^2 + x^2 (x^2 P(x^2))
;; Theoretical peak relative error = 2.1e-37,
;; relative peak error spread = 1.4e-8

(defconstant coscof
  (make-array 11 :element-type 'double-double-float
	      :initial-contents
	      '(
		1.601961934248327059668321782499768648351w-24
		-8.896621117922334603659240022184527001401w-22
		4.110317451243694098169570731967589555498w-19
		-1.561920696747074515985647487260202922160w-16
		4.779477332386900932514186378501779328195w-14
		-1.147074559772972328629102981460088437917w-11
		2.087675698786809897637922200570559726116w-9
		-2.755731922398589065255365968070684102298w-7
		2.480158730158730158730158440896461945271w-5
		-1.388888888888888888888888888765724370132w-3
		4.166666666666666666666666666666459301466w-2
		)))

(defconstant dp1
  (scale-float (float #b1100100100001111110110101010001000100001011010001100001000110100110001001100011001100010100010111000000011 1w0) -106))

(defconstant dp2
  (scale-float (float #b0111000001110011010001001010010000001001001110000010001000101001100111110011000111010000000010000010111011 1w0) (* 2 -106)))

(defconstant dp3
  (scale-float (float #b1110101001100011101100010011100110110010001001010001010010100000100001111001100011100011010000000100110111 1w0) (* 3 -106)))

(defconstant dp4
  (scale-float (float #b0111101111100101010001100110110011110011010011101001000011000110110011000000101011000010100110110111110010 1w0) (* 4 -106)))

(defun dd-%%sin (x)
  (declare (type double-double-float x)
	   (optimize (speed 3) (space 0)))
  (when (minusp x)
    (return-from dd-%%sin (- (the double-double-float (dd-%%sin (- x))))))
  ;; y = integer part of x/(pi/4).  
  (let* ((y (float (floor (/ x dd-pi/4)) 1w0))
	 (z (scale-float y -4)))
    (declare (type double-double-float y z))
    (setf z (float (floor z) 1w0))	; integer part of y/8
    (setf z (- y (scale-float z 4)))	; y - 16*(y/16)

    (let ((j (truncate z))
	  (sign 1))
      (unless (zerop (logand j 1))
	(incf j)
	(incf y))
      (setf j (logand j 7))

      (when (> j 3)
	(setf sign (- sign))
	(decf j 4))

      ;; Extended precision modular arithmetic
      (setf z (- (- (- x (* y dp1))
		    (* y dp2))
		 (* y dp3)))
      (let ((zz (* z z)))
	(if (or (= j 1)
		(= j 2))
	    (setf y (+ (- 1 (scale-float zz -1))
		       (* zz zz (poly-eval zz coscof))))
	    (setf y (+ z (* z (* zz (poly-eval zz sincof))))))
	(if (< sign 0)
	    (- y)
	    y)))))

(defun dd-%%cos (x)
  (declare (type double-double-float x)
	   (optimize (speed 3) (space 0)))
  (when (minusp x)
    (return-from dd-%%cos (dd-%%cos (- x))))
  ;; y = integer part of x/(pi/4).  
  (let* ((y (float (floor (/ x dd-pi/4)) 1w0))
	 (z (scale-float y -4)))
    (declare (type double-double-float y z))
    (setf z (float (floor z) 1w0))	; integer part of y/8
    (setf z (- y (scale-float z 4)))	; y - 16*(y/16)

    (let ((i (truncate z))
	  (j 0)
	  (sign 1))
      (declare (type (integer 0 7) j)
	       (type (integer -1 1) sign))
      (unless (zerop (logand i 1))
	(incf i)
	(incf y))
      (setf j (logand i 7))

      (when (> j 3)
	(setf sign (- sign))
	(decf j 4))
      (when (> j 1)
	(setf sign (- sign)))

      ;; Extended precision modular arithmetic.  This is basically
      ;; computing x - y*(pi/4) accurately so that |z| < pi/4.
      (setf z (- (- (- x (* y dp1))
		    (* y dp2))
		 (* y dp3)))
      (let ((zz (* z z)))
	(if (or (= j 1)
		(= j 2))
	    (setf y (+ z (* z (* zz (poly-eval zz sincof)))))
	    (setf y (+ (- 1 (scale-float zz -1))
		       (* zz (poly-eval zz coscof) zz))))
	(if (< sign 0)
	    (- y)
	    y)))))

(let ((P (make-array 6 :element-type 'double-double-float
		     :initial-contents
		     '(
		       -9.889929415807650724957118893791829849557w-1
		       1.272297782199996882828849455156962260810w3
		       -4.249691853501233575668486667664718192660w5
		       5.160188250214037865511600561074819366815w7
		       -2.307030822693734879744223131873392503321w9
		       2.883414728874239697964612246732416606301w10
		       )))
      (Q (make-array 6 :element-type 'double-double-float
		     :initial-contents
		     '(
		       ;; 1.000000000000000000000000000000000000000w0 */
		       -1.317243702830553658702531997959756728291w3
		       4.529422062441341616231663543669583527923w5
		       -5.733709132766856723608447733926138506824w7
		       2.758476078803232151774723646710890525496w9
		       -4.152206921457208101480801635640958361612w10
		       8.650244186622719093893836740197250197602w10
		       ))))
  (defun dd-tancot (xx cotflag)
    (declare (type double-double-float xx)
	     (optimize (speed 3) (space 0)))
    (let ((x 0w0)
	  (sign 1))
      (declare (type double-double-float x)
	       (type (integer -1 1) sign))
      (cond ((minusp xx)
	     (setf x (- xx))
	     (setf sign -1))
	    (t
	     (setf x xx)))
      (let* ((y (float (floor (/ x dd-pi/4)) 1w0))
	     (z (scale-float y -4))
	     (j 0))
	(declare (type double-double-float y z)
		 (type fixnum j))
	(setf z (float (floor z) 1w0))
	(setf z (- y (scale-float z 4)))

	(setf j (truncate z))

	(unless (zerop (logand j 1))
	  (incf j)
	  (incf y))

	(setf z (- (- (- x (* y dp1))
		      (* y dp2))
		   (* y dp3)))
	(let ((zz (* z z)))
	  (if (> zz 1w-40)
	      (setf y (+ z
			 (* z (* zz (/ (poly-eval zz p)
				       (poly-eval-1 zz q))))))
	      (setf y z))
	  (if (not (zerop (logand j 2)))
	      (if cotflag
		  (setf y (- y))
		  (setf y (/ -1 y)))
	      (if cotflag
		  (setf y (/ y))))
	  (if (< sign 0)
	      (- y)
	      y))))))

(defun dd-%%tan (x)
  (declare (type double-double-float x))
  (dd-tancot x nil))

(declaim (inline %kernel-rem-pi/2))
(alien:def-alien-routine ("__kernel_rem_pio2" %kernel-rem-pi/2) c-call:int
  (x (* double-float))
  (y (* double-float))
  (e0 c-call:int)
  (nx c-call:int)
  (prec c-call:int)
  (ipio2 (* c-call:int)))

;; This is taken from two_over_pi in fdlibm's e_rem_pio2.c.  We do
;; this here so that the Sparc version doesn't have to compile in
;; e_rem_pio2, which we don't need.  (But x86 and ppc do.)
(defconstant two-over-pi
  (make-array 66 :element-type '(unsigned-byte 32)
	      :initial-contents
	      '(#xA2F983 #x6E4E44 #x1529FC #x2757D1 #xF534DD #xC0DB62 
		#x95993C #x439041 #xFE5163 #xABDEBB #xC561B7 #x246E3A 
		#x424DD2 #xE00649 #x2EEA09 #xD1921C #xFE1DEB #x1CB129 
		#xA73EE8 #x8235F5 #x2EBB44 #x84E99C #x7026B4 #x5F7E41 
		#x3991D6 #x398353 #x39F49C #x845F8B #xBDF928 #x3B1FF8 
		#x97FFDE #x05980F #xEF2F11 #x8B5A0A #x6D1F6D #x367ECF 
		#x27CB09 #xB74F46 #x3F669E #x5FEA2D #x7527BA #xC7EBE5 
		#xF17B3D #x0739F7 #x8A5292 #xEA6BFB #x5FB11F #x8D5D08 
		#x560330 #x46FC7B #x6BABF0 #xCFBC20 #x9AF436 #x1DA9E3 
		#x91615E #xE61B08 #x659985 #x5F14A0 #x68408D #xFFD880 
		#x4D7327 #x310606 #x1556CA #x73A8C9 #x60E27B #xC08C6B 
		))
  "396 (hex) digits of 2/pi")


(let ((y (make-array 3 :element-type 'double-float))
      (parts (make-array 5 :element-type 'double-float)))
  (declare (type (simple-array double-float (3)) y)
	   (type (simple-array double-float (5)) parts))
  ;; Take the double-double-float number and break it into 24-bit
  ;; chunks.  Each chunk is an integer, which is coerced to a
  ;; double-float and stored in PARTS.
  (defun dd-expand (x)
    (declare (double-double-float x)
	     (optimize (speed 3) (space 0)))
    (multiple-value-bind (frac exp)
	(decode-float x)
      (declare (double-double-float frac)
	       (type (signed-byte 16) exp))
      (setf frac (scale-float frac 24))
      (decf exp 24)
      (dotimes (k 5)
	(setf (aref parts k) (coerce (ffloor frac) 'double-float))
	(setf frac (scale-float (- frac (aref parts k)) 24)))
      exp))
  (defun reduce-arg (x)
    (declare (double-double-float x)
	     (optimize (speed 3)))
    (let* ((e0 (dd-expand x))
	   (n (sys:without-gcing
	       (%kernel-rem-pi/2 (vector-sap parts)
				 (vector-sap y)
				 e0
				 (length parts)
				 3
				 (vector-sap two-over-pi))))
	   (sum (+ (coerce (aref y 2) 'double-double-float)
		   (coerce (aref y 1) 'double-double-float)
		   (coerce (aref y 0) 'double-double-float))))
      (values n sum))))
			       

(defun dd-%sin (x)
  (declare (double-double-float x))
  (cond ((minusp x)
	 (- (dd-%sin (- x))))
	((< (abs x) (/ pi 4))
	 (dd-%%sin x))
	(t
	 ;; Argument reduction needed
	 (multiple-value-bind (n reduced)
	     (reduce-arg x)
	   (case (logand n 3)
	     (0 (dd-%%sin reduced))
	     (1 (dd-%%cos reduced))
	     (2 (- (dd-%%sin reduced)))
	     (3 (- (dd-%%cos reduced))))))))

(defun dd-%cos (x)
  (declare (double-double-float x))
  (cond ((minusp x)
	 (dd-%cos (- x)))
	((< (abs x) (/ pi 4))
	 (dd-%%cos x))
	(t
	 ;; Argument reduction needed
	 (multiple-value-bind (n reduced)
	     (reduce-arg x)
	   (case (logand n 3)
	     (0 (dd-%%cos reduced))
	     (1 (- (dd-%%sin reduced)))
	     (2 (- (dd-%%cos reduced)))
	     (3 (dd-%%sin reduced)))))))

(defun dd-%tan (x)
  (declare (double-double-float x))
  (cond ((minusp x)
	 (- (dd-%tan (- x))))
	((< (abs x) (/ pi 4))
	 (dd-%%tan x))
	(t
	 ;; Argument reduction needed
	 (multiple-value-bind (n reduced)
	     (reduce-arg x)
	   (if (evenp n)
	       (dd-%%tan reduced)
	       (- (/ (dd-%%tan reduced))))))))

;;; dd-%log2
;;; Base 2 logarithm.

(let ((P (make-array 13 :element-type 'double-double-float
		     :initial-contents
		     '(
		       1.538612243596254322971797716843006400388w-6
		       4.998469661968096229986658302195402690910w-1
		       2.321125933898420063925789532045674660756w1
		       4.114517881637811823002128927449878962058w2
		       3.824952356185897735160588078446136783779w3
		       2.128857716871515081352991964243375186031w4
		       7.594356839258970405033155585486712125861w4
		       1.797628303815655343403735250238293741397w5
		       2.854829159639697837788887080758954924001w5
		       3.007007295140399532324943111654767187848w5
		       2.014652742082537582487669938141683759923w5
		       7.771154681358524243729929227226708890930w4
		       1.313572404063446165910279910527789794488w4
		       )))
      (Q (make-array 12 :element-type 'double-double-float
		     :initial-contents
		     '(
		       ;; 1.000000000000000000000000000000000000000w0
		       4.839208193348159620282142911143429644326w1
		       9.104928120962988414618126155557301584078w2
		       9.147150349299596453976674231612674085381w3
		       5.605842085972455027590989944010492125825w4
		       2.248234257620569139969141618556349415120w5
		       6.132189329546557743179177159925690841200w5
		       1.158019977462989115839826904108208787040w6
		       1.514882452993549494932585972882995548426w6
		       1.347518538384329112529391120390701166528w6
		       7.777690340007566932935753241556479363645w5
		       2.626900195321832660448791748036714883242w5
		       3.940717212190338497730839731583397586124w4
		       )))
      ;; Coefficients for log(x) = z + z^3 P(z^2)/Q(z^2),
      ;; where z = 2(x-1)/(x+1)
      ;; 1/sqrt(2) <= x < sqrt(2)
      ;; Theoretical peak relative error = 1.1e-35,
      ;; relative peak error spread 1.1e-9
      (R (make-array 6 :element-type 'double-double-float
		     :initial-contents
		     '(
		       -8.828896441624934385266096344596648080902w-1
		       8.057002716646055371965756206836056074715w1
		       -2.024301798136027039250415126250455056397w3
		       2.048819892795278657810231591630928516206w4
		       -8.977257995689735303686582344659576526998w4
		       1.418134209872192732479751274970992665513w5
		       )))
      (S (make-array 6 :element-type 'double-double-float
		     :initial-contents
		     '(
		       ;; 1.000000000000000000000000000000000000000w0 */
		       -1.186359407982897997337150403816839480438w2
		       3.998526750980007367835804959888064681098w3
		       -5.748542087379434595104154610899551484314w4
		       4.001557694070773974936904547424676279307w5
		       -1.332535117259762928288745111081235577029w6
		       1.701761051846631278975701529965589676574w6
		       ))))
  (defun dd-%log2 (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (multiple-value-bind (x e)
	(decode-float x)
      (declare (type double-double-float x)
	       (type double-float-exponent e))
      (let ((z 0w0)
	    (y 0w0))
	(declare (type double-double-float z y))
	(cond ((or (> e 2)
		   (< e -2))
	       (cond ((< x sqrt-1/2)
		      ;; 2*(2*x-1)/(2*x+1)
		      (decf e)
		      (setf z (- x 0.5w0))
		      (setf y (+ (* 0.5w0 z) 0.5w0)))
		     (t
		      ;; 2*(x-1)/(x+1)
		      (setf z (- x 0.5w0))
		      (decf z 0.5w0)
		      (setf y (+ (* 0.5w0 z) 0.5w0))))
	       (setf x (/ z y))
	       (setf z (* x x))
	       (setf y (* x (/ (* z (poly-eval z r))
			       (poly-eval-1 z s)))))
	      (t
	       (cond ((< x sqrt-1/2)
		      (decf e)
		      (setf x (- (scale-float x 1) 1)))
		     (t
		      (decf x)))
	       (setf z (* x x))
	       (setf y (* x (/ (* z (poly-eval x p))
			       (poly-eval-1 x q))))
	       (decf y (scale-float z -1))))
	;; Multiply log of fraction by log2(e) and base 2 exponent by 1
	;;
	;; This sequence of operations is critical
	(setf z (* y log2ea))
	(setf z (+ z (* x log2ea)))
	(setf z (+ z y))
	(setf z (+ z x))
	(setf z (+ z e))
	z))))

;;; dd-%exp2
;;; 2^x

(let ((P (make-array 5 :element-type 'double-double-float
		     :initial-contents
		     '(
		       1.587171580015525194694938306936721666031w2
		       6.185032670011643762127954396427045467506w5
		       5.677513871931844661829755443994214173883w8
		       1.530625323728429161131811299626419117557w11
		       9.079594442980146270952372234833529694788w12
		       )))
      (Q (make-array 5 :element-type 'double-double-float
		     :initial-contents
		     '(
		       ;; 1.000000000000000000000000000000000000000w0 */
		       1.236602014442099053716561665053645270207w4
		       2.186249607051644894762167991800811827835w7
		       1.092141473886177435056423606755843616331w10
		       1.490560994263653042761789432690793026977w12
		       2.619817175234089411411070339065679229869w13
		       ))))
  (defun dd-%exp2 (x)
    (declare (type double-double-float x)
	     (optimize (speed 3) (space 0)))
    (when (>= x 1024w0)
      (return-from dd-%exp2
	(%make-double-double-float ext:double-float-positive-infinity
				   ext:double-float-positive-infinity)))
    (when (<= x -1024w0)
      (return-from dd-%exp2 0w0))
    (multiple-value-bind (n x)
	(floor (the (double-double-float -1024w0 1024w0) x))
      (declare (type double-double-float x))
      (let* ((xx (* x x))
	     (px (* x (poly-eval xx p))))
	(setf x (/ px (- (poly-eval-1 xx q) px)))
	(setf x (+ 1 (scale-float x 1)))
	(scale-float x n)))))

;;; dd-%powil
;;; x^n where n is an integer

(defun dd-%powil (x nn)
  (declare (type double-double-float x)
	   (fixnum nn)
	   (optimize (speed 3) (space 0)))
  (when (zerop x)
    (return-from dd-%powil
      (cond ((zerop nn)
	     1w0)
	    ((minusp nn)
	     (%make-double-double-float ext:double-float-positive-infinity
					ext:double-float-positive-infinity))
	    (t
	     0w0))))
  (when (zerop nn)
    (return-from dd-%powil 1w0))

  (let ((asign 0)
	(sign 0)
	(n nn))
    (declare (type (integer -1 0) asign sign)
	     (fixnum n))
    (when (minusp x)
      (setf asign -1)
      (setf x (- x)))
    (cond ((minusp nn)
	   (setf sign -1)
	   (setf n (- nn)))
	  (t
	   (setf sign 0)
	   (setf n nn)))
    ;; Overflow detection

    ;; Calculate approximate log of answer
    (multiple-value-bind (s lx)
	(decode-float x)
      (declare (type double-double-float s)
	       (type double-float-exponent lx))
      (let ((e (* n (1- lx))))
	(cond ((or (zerop e)
		   (> e 64)
		   (< e -64))
	       (setf s (/ (- s 7.0710678118654752w-1)
			  (+ s 7.0710678118654752w-1)))
	       (setf s (* (+ (- (* 2.9142135623730950w0 s)
				0.5w0)
			     lx)
			  nn log2e)))
	      (t
	       (setf s (* loge2 e))))
	(when (> s max-log)
	  ;; Overflow.  What to do?
	  (error "Overflow"))
	(when (< s min-log)
	  (return-from dd-%powil 0w0))

	;; Handle denormal answer
	(when (< s (- 2 max-log))
	  (setf x (/ x))
	  (setf sign 0))

	;; First bit of the power
	(let ((y 0w0))
	  (declare (type double-double-float y))
	  (cond ((zerop (logand n 1))
		 (setf y 1w0)
		 (setf asign 0))
		(t
		 (setf y x)))
	  (let ((w x))
	    (declare (type double-double-float w))
	    (setf n (ash n -1))
	    (loop while (not (zerop n))
	       do
	       (setf w (* w w))
	       (unless (zerop (logand n 1))
		 (setf y (* y w)))
	       (setf n (ash n -1))))
	  ;; Odd power of a negative number
	  (unless (zerop asign)
	    (setf y (- y)))

	  (unless (zerop sign)
	    (setf y (/ y)))
	  y)))))
	

;;; dd-real-pow
;;; x^y, for x and y real, and real result.

(defun dd-real-pow (x y)
  (declare (type double-double-float x y)
	   (optimize (speed 3) (space 0)))
  (let ((nflg 0)
	(w (floor y)))
    ;; nflg = 1 if x < 0 raised to integer power
    (when (and (= w y)
	       (< (abs w) 32768))
      (return-from dd-real-pow (dd-%powil x w)))

    (when (<= x 0)
      (cond ((zerop x)
	     (if (zerop y)
		 ;; 0^0
		 (return-from dd-real-pow 1w0)
		 ;; 0^y
		 (return-from dd-real-pow 0w0)))
	    (t
	     (when (/= w y)
	       ;; noninteger power of negative number
	       (let ((p (the double-double-float (dd-real-pow (abs x) y)))
		     (y*pi (* y dd-pi)))
		 (return-from dd-real-pow (complex (* p (dd-%cos y*pi))
						   (* p (dd-%sin y*pi))))))

	     ;; For negative x, find out if the integer exponent is odd or even.
	     (let ((w (scale-float y -1)))
	       (declare (type double-double-float w))
	       (setf w (ffloor w))
	       (setf w (scale-float w 1))
	       (when (/= w y)
		 (setf nflg 1))
	       (setf x (abs x))))))
    (let ((z (dd-%log2 x)))
      (declare (type double-double-float z))
      (setf z (dd-%exp2 (* y z)))
      (unless (zerop nflg)
	(setf z (- z)))
      z)))

(defun dd-%pow (x y)
  (declare (type double-double-float x y))
  (dd-real-pow x y))


;; These are essentially the same as in irrat.lisp, but very slightly
;; modified to operate on double-double-floats.
(defun dd-cssqs (z)
  ;; Compute |(x+i*y)/2^k|^2 scaled to avoid over/underflow. The
  ;; result is r + i*k, where k is an integer.
  
  ;; Save all FP flags
  (let ((x (float (realpart z) 1w0))
	(y (float (imagpart z) 1w0)))
    ;; Would this be better handled using an exception handler to
    ;; catch the overflow or underflow signal?  For now, we turn all
    ;; traps off and look at the accrued exceptions to see if any
    ;; signal would have been raised.
    ;;
    ;; Actually, for double-double-floats, we should probably
    ;; explicitly check for overflow instead of disabling the traps.
    ;; Why?  Because instead of overflow, double-double signals
    ;; invalid operation.
    (with-float-traps-masked (:underflow :overflow :invalid)
      (let ((rho (+ (square x) (square y))))
	(declare (optimize (speed 3) (space 0)))
	(cond ((and (or (float-nan-p rho)
			(float-infinity-p rho))
		    (or (float-infinity-p (abs x))
			(float-infinity-p (abs y))))
	       (values (%make-double-double-float ext:double-float-positive-infinity 0d0)
		       0))
	      ((let ((threshold #.(/ least-positive-double-float
				     double-float-epsilon))
		     (traps (ldb vm::float-sticky-bits
				 (vm:floating-point-modes))))
		 ;; Overflow raised or (underflow raised and rho <
		 ;; lambda/eps)
		 (or (not (zerop (logand vm:float-overflow-trap-bit traps)))
		     (and (not (zerop (logand vm:float-underflow-trap-bit traps)))
			  (< rho threshold))))
	       ;; If we're here, neither x nor y are infinity and at
	       ;; least one is non-zero.. Thus logb returns a nice
	       ;; integer.
	       (let ((k (- (logb-finite (max (abs x) (abs y))))))
		 (values (+ (square (scalb x k))
			    (square (scalb y k)))
			 (- k))))
	      (t
	       (values rho 0)))))))

(defun dd-complex-sqrt (z)
  "Principle square root of Z

Z may be any number, but the result is always a complex."
  (declare (number z))
  (multiple-value-bind (rho k)
      (dd-cssqs z)
    (declare (type (or (member 0w0) (double-double-float 0w0)) rho)
	     (type fixnum k))
    (let ((x (float (realpart z) 1.0w0))
	  (y (float (imagpart z) 1.0w0))
	  (eta 0w0)
	  (nu 0w0))
      (declare (type double-double-float x y eta nu))

      (locally
	  ;; space 0 to get maybe-inline functions inlined.
	  (declare (optimize (speed 3) (space 0)))

	(if (not (float-nan-p x))
	    (setf rho (+ (scalb (abs x) (- k)) (sqrt rho))))

	(cond ((oddp k)
	       (setf k (ash k -1)))
	      (t
	       (setf k (1- (ash k -1)))
	       (setf rho (+ rho rho))))

	(setf rho (scalb (sqrt rho) k))

	(setf eta rho)
	(setf nu y)

	(when (/= rho 0d0)
	  (when (not (float-infinity-p (abs nu)))
	    (setf nu (/ (/ nu rho) 2d0)))
	  (when (< x 0d0)
	    (setf eta (abs nu))
	    (setf nu (float-sign y rho))))
	(complex eta nu)))))

(defun dd-complex-log-scaled (z j)
  "Compute log(2^j*z).

This is for use with J /= 0 only when |z| is huge."
  (declare (number z)
	   (fixnum j))
  ;; The constants t0, t1, t2 should be evaluated to machine
  ;; precision.  In addition, Kahan says the accuracy of log1p
  ;; influences the choices of these constants but doesn't say how to
  ;; choose them.  We'll just assume his choices matches our
  ;; implementation of log1p.
  (let ((t0 #.(/ 1 (sqrt 2.0w0)))
	(t1 1.2w0)
	(t2 3w0)
	(ln2 #.(log 2w0))
	(x (float (realpart z) 1.0w0))
	(y (float (imagpart z) 1.0w0)))
    (multiple-value-bind (rho k)
	(dd-cssqs z)
      (declare (optimize (speed 3)))
      (let ((beta (max (abs x) (abs y)))
	    (theta (min (abs x) (abs y))))
	(complex (if (and (zerop k)
			  (< t0 beta)
			  (or (<= beta t1)
			      (< rho t2)))
		     (/ (dd-%log1p (+ (* (- beta 1.0d0)
					 (+ beta 1.0d0))
				      (* theta theta)))
			2d0)
		     (+ (/ (log rho) 2d0)
			(* (+ k j) ln2)))
		 (atan y x))))))

(defun dd-complex-log (z)
  "Log of Z = log |Z| + i * arg Z

Z may be any number, but the result is always a complex."
  (declare (number z))
  (dd-complex-log-scaled z 0))
	       
;; Let us note the following "strange" behavior.  atanh 1.0d0 is
;; +infinity, but the following code returns approx 176 + i*pi/4. The
;; reason for the imaginary part is caused by the fact that arg i*y is
;; never 0 since we have positive and negative zeroes.

(defun dd-complex-atanh (z)
  "Compute atanh z = (log(1+z) - log(1-z))/2"
  (declare (number z))
  (if (and (realp z) (< z -1))
      ;; atanh is continuous in quadrant III in this case.
      (dd-complex-atanh (complex z -0f0))
      (let* ( ;; Constants
	     (theta (/ (sqrt most-positive-double-float) 4.0w0))
	     (rho (/ 4.0w0 (sqrt most-positive-double-float)))
	     (half-pi dd-pi/2)
	     (rp (float (realpart z) 1.0w0))
	     (beta (float-sign rp 1.0w0))
	     (x (* beta rp))
	     (y (* beta (- (float (imagpart z) 1.0w0))))
	     (eta 0.0w0)
	     (nu 0.0w0))
	;; Shouldn't need this declare.
	(declare (double-double-float x y))
	(locally
	    (declare (optimize (speed 3)))
	  (cond ((or (> x theta)
		     (> (abs y) theta))
		 ;; To avoid overflow...
		 (setf nu (float-sign y half-pi))
		 ;; eta is real part of 1/(x + iy).  This is x/(x^2+y^2),
		 ;; which can cause overflow.  Arrange this computation so
		 ;; that it won't overflow.
		 (setf eta (let* ((x-bigger (> x (abs y)))
				  (r (if x-bigger (/ y x) (/ x y)))
				  (d (+ 1.0d0 (* r r))))
			     (if x-bigger
				 (/ (/ x) d)
				 (/ (/ r y) d)))))
		((= x 1.0w0)
		 ;; Should this be changed so that if y is zero, eta is set
		 ;; to +infinity instead of approx 176?  In any case
		 ;; tanh(176) is 1.0d0 within working precision.
		 (let ((t1 (+ 4w0 (square y)))
		       (t2 (+ (abs y) rho)))
		   (setf eta (dd-%log (/ (sqrt (sqrt t1))
					 (sqrt t2))))
		   (setf nu (* 0.5d0
			       (float-sign y
					   (+ half-pi (dd-%atan (* 0.5d0 t2))))))))
		(t
		 (let ((t1 (+ (abs y) rho)))
		   ;; Normal case using log1p(x) = log(1 + x)
		   (setf eta (* 0.25d0
				(dd-%log1p (/ (* 4.0d0 x)
					      (+ (square (- 1.0d0 x))
						 (square t1))))))
		   (setf nu (* 0.5d0
			       (dd-%atan2 (* 2.0d0 y)
					  (- (* (- 1.0d0 x)
						(+ 1.0d0 x))
					     (square t1))))))))
	  (complex (* beta eta)
		   (- (* beta nu)))))))

(defun dd-complex-tanh (z)
  "Compute tanh z = sinh z / cosh z"
  (declare (number z))
  (let ((x (float (realpart z) 1.0w0))
	(y (float (imagpart z) 1.0w0)))
    (locally
	;; space 0 to get maybe-inline functions inlined
	(declare (optimize (speed 3) (space 0)))
      (cond ((> (abs x)
		#-(or linux hpux) #.(/ (%asinh most-positive-double-float) 4d0)
		;; This is more accurate under linux.
		#+(or linux hpux) #.(/ (+ (%log 2.0d0)
					  (%log most-positive-double-float)) 4d0))
	     (complex (float-sign x)
		      (float-sign y)))
	    (t
	     (let* ((tv (dd-%tan y))
		    (beta (+ 1.0d0 (* tv tv)))
		    (s (sinh x))
		    (rho (sqrt (+ 1.0d0 (* s s)))))
	       (if (float-infinity-p (abs tv))
		   (complex (/ rho s)
			    (/ tv))
		   (let ((den (+ 1.0d0 (* beta s s))))
		     (complex (/ (* beta rho s)
				 den)
			      (/ tv den))))))))))

;; Kahan says we should only compute the parts needed.  Thus, the
;; realpart's below should only compute the real part, not the whole
;; complex expression.  Doing this can be important because we may get
;; spurious signals that occur in the part that we are not using.
;;
;; However, we take a pragmatic approach and just use the whole
;; expression.

;; NOTE: The formula given by Kahan is somewhat ambiguous in whether
;; it's the conjugate of the square root or the square root of the
;; conjugate.  This needs to be checked.

;; I checked.  It doesn't matter because (conjugate (sqrt z)) is the
;; same as (sqrt (conjugate z)) for all z.  This follows because
;;
;; (conjugate (sqrt z)) = exp(0.5*log |z|)*exp(-0.5*j*arg z).
;;
;; (sqrt (conjugate z)) = exp(0.5*log|z|)*exp(0.5*j*arg conj z)
;;
;; and these two expressions are equal if and only if arg conj z =
;; -arg z, which is clearly true for all z.

;; NOTE: The rules of Common Lisp says that if you mix a real with a
;; complex, the real is converted to a complex before performing the
;; operation.  However, Kahan says in this paper (pg 176):
;;
;; (iii) Careless handling can turn infinity or the sign of zero into
;;       misinformation that subsequently disappears leaving behind
;;       only a plausible but incorrect result.  That is why compilers
;;       must not transform z-1 into z-(1+i*0), as we have seen above,
;;       nor -(-x-x^2) into (x+x^2), as we shall see below, lest a
;;       subsequent logarithm or square root produce a non-zero
;;       imaginary part whose sign is opposite to what was intended.
;;
;; The interesting examples are too long and complicated to reproduce
;; here.  We refer the reader to his paper.
;;
;; The functions below are intended to handle the cases where a real
;; is mixed with a complex and we don't want CL complex contagion to
;; occur..

(declaim (inline 1+z 1-z z-1 z+1))
(defun dd-1+z (z)
  (complex (+ 1 (realpart z)) (imagpart z)))
(defun dd-1-z (z)
  (complex (- 1 (realpart z)) (- (imagpart z))))
(defun dd-z-1 (z)
  (complex (- (realpart z) 1) (imagpart z)))
(defun dd-z+1 (z)
  (complex (+ (realpart z) 1) (imagpart z)))

(defun dd-complex-acos (z)
  "Compute acos z = pi/2 - asin z

Z may be any number, but the result is always a complex."
  (declare (number z))
  (if (and (realp z) (> z 1))
      ;; acos is continuous in quadrant IV in this case.
      (complex-acos (complex z -0f0))
      (let ((sqrt-1+z (complex-sqrt (1+z z)))
	    (sqrt-1-z (complex-sqrt (1-z z))))
	(cond ((zerop (realpart sqrt-1+z))
	       ;; Same as below, but we compute atan ourselves (because we
	       ;; have atan +/- infinity).
	       (complex 
			(if (minusp (float-sign (* (realpart sqrt-1-z)
						   (realpart sqrt-1+z))))
			    (- dd-pi)
			    dd-pi)
			(asinh (imagpart (* (conjugate sqrt-1+z)
					    sqrt-1-z)))))
	      (t
	       (complex (* 2 (atan (/ (realpart sqrt-1-z)
				      (realpart sqrt-1+z))))
			(asinh (imagpart (* (conjugate sqrt-1+z)
					    sqrt-1-z)))))))))

(defun dd-complex-acosh (z)
  "Compute acosh z = 2 * log(sqrt((z+1)/2) + sqrt((z-1)/2))

Z may be any number, but the result is always a complex."
  (declare (number z))
  (let* ((sqrt-z-1 (complex-sqrt (z-1 z)))
	 (sqrt-z+1 (complex-sqrt (z+1 z))))
    ;; We need to handle the case where real part of sqrt-z+1 is zero,
    ;; because division by zero with double-double-floats doesn't
    ;; produce infinity.
    (cond ((zerop (realpart sqrt-z+1))
	   ;; Same as below, but we compute atan ourselves (because we
	   ;; have atan +/- infinity).
	   (complex (asinh (realpart (* (conjugate sqrt-z-1)
					sqrt-z+1)))
		    (if (minusp (float-sign (* (imagpart sqrt-z-1)
					       (realpart sqrt-z+1))))
			(- dd-pi)
			dd-pi)))
	  (t
	   (complex (asinh (realpart (* (conjugate sqrt-z-1)
					sqrt-z+1)))
		    (* 2 (atan (/ (imagpart sqrt-z-1)
				  (realpart sqrt-z+1)))))))))


(defun dd-complex-asin (z)
  "Compute asin z = asinh(i*z)/i

Z may be any number, but the result is always a complex."
  (declare (number z))
  (if (and (realp z) (> z 1))
      ;; asin is continuous in quadrant IV in this case.
      (dd-complex-asin (complex z -0f0))
      (let* ((sqrt-1-z (complex-sqrt (1-z z)))
	     (sqrt-1+z (complex-sqrt (1+z z)))
	     (den (realpart (* sqrt-1-z sqrt-1+z))))
	(cond ((zerop den)
	       ;; Like below but we handle atan part ourselves.
	       (complex (if (minusp (float-sign den))
			    (- dd-pi/2)
			    dd-pi/2)
		   (asinh (imagpart (* (conjugate sqrt-1-z)
				       sqrt-1+z)))))
	      (t
	       (with-float-traps-masked (:divide-by-zero)
		 ;; We get a invalid operation here when z is real and |z| > 1.
		 (complex (atan (/ (realpart z)
				   (realpart (* sqrt-1-z sqrt-1+z))))
			  (asinh (imagpart (* (conjugate sqrt-1-z)
					      sqrt-1+z))))))))))

(defun dd-complex-asinh (z)
  "Compute asinh z = log(z + sqrt(1 + z*z))

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; asinh z = -i * asin (i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-asin iz)))
    (complex (imagpart result)
	     (- (realpart result)))))
	 
(defun dd-complex-atan (z)
  "Compute atan z = atanh (i*z) / i

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; atan z = -i * atanh (i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-atanh iz)))
    (complex (imagpart result)
	     (- (realpart result)))))

(defun dd-complex-tan (z)
  "Compute tan z = -i * tanh(i * z)

Z may be any number, but the result is always a complex."
  (declare (number z))
  ;; tan z = -i * tanh(i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
	 (result (complex-tanh iz)))
    (complex (imagpart result)
	     (- (realpart result)))))
