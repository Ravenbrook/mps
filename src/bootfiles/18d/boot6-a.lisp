;; For bootstrapping the conc-name inheritance fix, part 1.
;;
;; You need to do a build loading just this file but NOT boot6-b.lisp.
;; The next build MUST NOT use this file and load up boot6-b
;; instead.
;;
;; Use the CLOBBER-IT restart when prompted for a restart.

(pushnew :bootstrap-conc-name *features*)

(in-package "KERNEL")

(defstruct (defstruct-description
             (:conc-name dd-)
             (:print-function print-defstruct-description)
	     (:make-load-form-fun :just-dump-it-normally)
	     (:pure t)
	     (:constructor make-defstruct-description (name)))
  ;;
  ;; name of the structure
  (name (required-argument) :type symbol)
  ;;
  ;; documentation on the structure
  (doc nil :type (or string null))
  ;;
  ;; prefix for slot names.  If NIL, none.
  (conc-name (concat-pnames name '-) :type (or symbol null))
  ;;
  ;; The name of the primary standard keyword constructor, or NIL if none.
  (default-constructor nil :type (or symbol null))
  ;;
  ;; All the explicit :CONSTRUCTOR specs, with name defaulted.
  (constructors () :type list)
  ;;
  ;; name of copying function
  (copier (concat-pnames 'copy- name) :type (or symbol null))
  ;;
  ;; Name of type predictate
  (predicate (concat-pnames name '-p) :type (or symbol null))
  ;;
  ;; The arguments to the :INCLUDE option, or NIL if no included structure.
  (include nil :type list)
  ;;
  ;; The arguments to the :ALTERNATE-METACLASS option (an extension used to
  ;; define structure-like objects with an arbitrary superclass and that may
  ;; not have STRUCTURE-CLASS as the metaclass.)  Syntax is:
  ;;    (superclass-name metaclass-name metaclass-constructor)
  ;;
  (alternate-metaclass nil :type list)
  ;;
  ;; list of defstruct-slot-description objects for all slots (including
  ;; included ones.)
  (slots () :type list)
  ;;
  ;; Number of elements we've allocated (see also raw-length.)
  (length 0 :type index)
  ;;
  ;; General kind of implementation.
  (type 'structure :type (member structure vector list
				 funcallable-structure))
  ;;
  ;; The next three slots are for :TYPE'd structures (which aren't classes,
  ;; CLASS-STRUCTURE-P = NIL)
  ;;
  ;; Vector element type.
  (element-type 't)
  ;;
  ;; T if :NAMED was explicitly specified, Nil otherwise.
  (named nil :type boolean)
  ;;
  ;; Any INITIAL-OFFSET option on this direct type.
  (offset nil :type (or index null))
  ;;
  ;; The argument to the PRINT-FUNCTION option, or NIL if none.  If we see an
  ;; explicit (:PRINT-FUNCTION) option, then this is DEFAULT-STRUCTURE-PRINT.
  ;; See also BASIC-STRUCTURE-CLASS-PRINTER.  Only for classed structures.
  ;;
  (print-function nil :type (or cons symbol null))
  ;;
  ;; The next four slots are only meaningful in real default structures (TYPE =
  ;; STRUCTURE).
  ;;
  ;; Make-load-form function option.  See also STRUCTURE-CLASS-LOAD-FORM-MAKER.
  (make-load-form-fun nil :type (or symbol cons null))
  ;;
  ;; The index of the raw data vector and the number of words in it.  NIL and 0
  ;; if not allocated yet.
  (raw-index nil :type (or index null))
  (raw-length 0 :type index)
  ;;
  ;; Value of the :PURE option, or :UNSPECIFIED.  Only meaningful if
  ;; CLASS-STRUCTURE-P = T.
  (pure :unspecified :type (member t nil :substructure :unspecified))
  ;;
  ;; a list of (NAME . INDEX) pairs for accessors of included structures
  (inherited-accessor-alist () :type list))


