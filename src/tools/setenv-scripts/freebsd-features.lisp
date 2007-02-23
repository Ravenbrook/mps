;; e.g. for FreeBSD on x86 you probably want:
;;(pushnew :freebsd4 *features*)
;;(pushnew :freebsd *features*)
;;(pushnew :elf *features*)

;; We can't really do this before adding #+freebsd5 reader
;; conditionals to the source code at appropiate places
;; (setf *features* (remove :freebsd4 *features*))
;; (pushnew :freebsd5 *features*)
