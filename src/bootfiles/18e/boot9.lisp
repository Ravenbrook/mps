;;; Bootstrap file for callback support
;;;
;;; 

(in-package :alien)
;;; Some abbreviations for alien-type classes.  The $ suffix is there
;;; to prevent name clashes.

(deftype void$ () '(satisfies alien-void-type-p))
(deftype integer$ () 'alien::alien-integer-type)
(deftype integer-64$ () '(satisfies alien-integer-64-type-p))
(deftype signed-integer$ () '(satisfies alien-signed-integer-type-p))
(deftype pointer$ () 'alien::alien-pointer-type)
(deftype single$ () 'alien::alien-single-float-type)
(deftype double$ () 'alien::alien-double-float-type)
(deftype sap$ () '(satisfies alien-sap-type=))

(defun alien-sap-type= (type)
  (alien::alien-type-= type 
		       (alien::parse-alien-type 'system-area-pointer)))

(defun alien-void-type-p (type)
  (and (alien::alien-values-type-p type)
       (null (alien::alien-values-type-values type))))

(defun alien-integer-64-type-p (type)
  (and (alien::alien-integer-type-p type)
       (= (alien::alien-type-bits type) 64)))

(defun alien-signed-integer-type-p (type)
  (and (alien::alien-integer-type-p type)
       (alien::alien-integer-type-signed type)))


(defun segment-to-trampoline (segment length)
  (let* ((code (alien-funcall 
		(extern-alien "malloc" (function system-area-pointer unsigned))
		length))
	 (fill-pointer code))
    (new-assem:segment-map-output segment
				  (lambda (sap length)
				    (kernel:system-area-copy sap 0 fill-pointer 0
							     (* length vm:byte-bits))
				    (setf fill-pointer (sys:sap+ fill-pointer length))))
    code))

(in-package :vm)

(export '(make-callback-trampoline callback-accessor-form))

;; Just define the function for bootstrap.  The real one will get
;; defined later.
(defun make-callback-trampoline (index return-type)
  )

(defun callback-accessor-form (type sp offset)
  )

