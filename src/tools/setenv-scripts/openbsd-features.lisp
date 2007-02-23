;; e.g. for OpenBSD on x86 you probably want:
;;(pushnew :x86 *features*)
;;(pushnew :openbsd *features*)
;;(pushnew :bsd *features*)

;; Select conservative GC:
;;(pushnew @@gcname@@ *features*)
;; This X86 port supports multiprocessing
;;(pushnew :mp *features*)

;; CPU selection:
;; i486 gives you faster locking
;;(pushnew :i486 *features*)
;; Pentium gives you CPU cyclecounts in time
;;(pushnew :pentium *features*)
