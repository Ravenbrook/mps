; This file is only needed when compiling for the Alpha under Linux, to add the
; new Alpha-only ISTREAM-MEMORY-BARRIER VOP.
(in-package "ALPHA")

(define-instruction imb (segment)
  (:emitter (emit-pal segment 0 #x000086)))

(define-vop (c::istream-memory-barrier)
  (:generator 1
    (inst imb)))

