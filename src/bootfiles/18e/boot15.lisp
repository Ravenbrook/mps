;;;
;;; Bootstrap file for adding dynamic-extent rest args.
;;;

(in-package :cl-user)
(use-package :fwrappers)

;;;
;;; Prevent structure redefinition dialogs.
;;;
(define-fwrapper return-nil (&rest args) (call-next-function) nil)
(fwrap 'kernel::redefine-layout-warning #'return-nil)
(push :bootstrap-dynamic-extent *features*)

;;;
;;; Make compiler-layouts known to the compiler, for the change
;;; in the struct C::LEAF, which gets a new slot.
;;;
(compile-file "target:compiler/node")

;;; End of file.
