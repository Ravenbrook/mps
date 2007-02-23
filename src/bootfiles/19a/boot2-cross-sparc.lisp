;;; Bootstrap file for renaming the unused slot in symbols to hash,
;;; like on x86 so we can store the sxhash value there.

#+sparc
(load "target:tools/cross-scripts/cross-sparc-sparc")
