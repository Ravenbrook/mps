;; e.g. for Solaris on sparc you probably want some of these:
;;(pushnew :sparc *features*)
;;(pushnew :svr4 *features*)

;; Solaris supports linkage-table
;;(pushnew :linkage-table *features*)

;; CPU selection:
;; Sparc-v7 gives us fsqrt instruction
;;(pushnew :sparc-v7 *features*)
;;(setf *features* (remove :sparc-v7 *features*))
;; Sparc-v8 gives us an integer multiply and divide instruction
;;(pushnew :sparc-v8 *features*)
;;(setf *features* (remove :sparc-v8 *features*))
;; Sparc-v9 gives us the extra FP registers, fast bignum multiply and
;; floor, other float operations available on the Sparc V9, and other
;; assorted V9 features.
;;(pushnew :sparc-v9 *features*)
;;(setf *features* (remove :sparc-v9 *features*))

;; This enables some hand-written vops for complex float arithmetic.
;;
;;(pushnew :complex-fp-vops *features*)
