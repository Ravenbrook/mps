;;; This file bootstraps the changes in extern-alien-name, which
;;; brings the x86 handling of underscores on non-ELF systems in
;;; line with other ports, by moving the underscore addition to
;;; extern-alien-name, where it belongs, and not f-s-a-aux.
;;; This also makes foreign linking work better on OpenBSD.
;;;
#+x86
(in-package "LISP")

#+x86
(defun foreign-symbol-address-aux (symbol)
  (multiple-value-bind
      (value found)
      (gethash symbol *foreign-symbols* 0)
    ;; can't make irix linker give values in the symbol table to global vars
    ;;from dsos, so we have to resolve at runtime (and handle symbols being
    ;;defined with null values)
    (if #-irix found #+irix (and found (not (zerop value)))
        value
        (let ((value (system:alternate-get-global-address symbol)))
          (when (zerop value)
            (error "Unknown foreign symbol: ~S" symbol))
          value))))

#+x86
(in-package "X86")

#+x86
(defun extern-alien-name (name)
  (declare (type simple-string name))
  #+(and bsd (not elf))
  (concatenate 'string "_" name)
  #-(and bsd (not elf))
  name)

#+(and x86 (or linux (and freebsd elf)))
(defun lisp::foreign-symbol-address-aux (name)
  (multiple-value-bind (value found)
      (gethash name lisp::*foreign-symbols* 0)
    (if found
        value
        (multiple-value-bind (value found)
            (gethash
             (concatenate 'string "PVE_stub_" name)
             lisp::*foreign-symbols* 0)
          (if found
              value
              (let ((value (system:alternate-get-global-address name)))
                (when (zerop value)
                  (error "Unknown foreign symbol: ~S" name))
                value))))))
