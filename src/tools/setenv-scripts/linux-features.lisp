;; e.g. for Linux on x86 you probably want:
;;(pushnew :x86 *features*)
;;(pushnew :linux *features*)
;; Note! If you are still running glibc 2.1, you need the following:
;; (pushnew :glibc2.1 *features*)
;; Otherwise, i.e. for glibc 2.2 and later you want:
(setf *features* (remove :glibc2.1 *features*))

;; Select conservative GC:
;;(pushnew @@gcname@@ *features*)
;; This X86 port supports multiprocessing and linkage-table
;;(pushnew :mp *features*)
;;(pushnew :linkage-table *features*)

;; CPU selection:
;; i486 gives you faster locking
;;(pushnew :i486 *features*)
;; Pentium gives you CPU cyclecounts in time
;;(pushnew :pentium *features*)
