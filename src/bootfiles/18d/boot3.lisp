;; Unexport the newly re-introduced compiler-macroexpand* symbols from
;; the CL package.  This is for the benefit of old cores, which might
;; still have those "exported", even though the functions (or even the
;; symbols) don't exist.  This is deep CMUCL voodoo magic, don't even
;; try to understand it.

(unexport '(CL::COMPILER-MACROEXPAND-1 CL::COMPILER-MACROEXPAND) "CL")
