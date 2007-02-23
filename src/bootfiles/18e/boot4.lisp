;;;;
;;;; Boot file for making C::SEQUENCE-COUNT = KERNEL:SEQUENCE-COUNT,
;;;; which it should be because KERNEL:SEQUENCE-COUNT names a type
;;;; used in the compiler.
;;;;

(unintern 'c::sequence-count "C")
(export '(kernel::sequence-count) :kernel)

;;;; end of file
