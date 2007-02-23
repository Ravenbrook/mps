(load "clisp-fmt")
(load "funref-fmt")

(put 'defvar 'latexinfo-deffn-formatting-property
     'latexinfo-format-specialized-defvar)

(put 'defvarx 'latexinfo-deffn-formatting-property
     'latexinfo-format-specialized-defvar)

(defun latexinfo-format-specialized-defvar (parsed-args)
  ;; Specialized variable-like entity:
  ;; \defvar{name}               In Info, `Variable: Name'
  ;; Use cdr of command-type to determine category:
  (let ((category (car (cdr command-type)))
        (name (car parsed-args))
        (args (cdr parsed-args)))
    (if (not (looking-at "\n")) (insert "\n"))
    (insert " -- " category ": " "*" name "*")
    (while args
      (insert " "
              (if (= ?& (aref (car args) 0))
                  (car args)
                (upcase (car args)))
	      "\n")
      (setq args (cdr args)))))



(put 'python 'latexinfo-format 'latexinfo-format-python)
(defun latexinfo-format-python ()
  (latexinfo-parse-noarg)
  (insert "Python"))

(put 'Python 'latexinfo-format 'latexinfo-format-Python)
(defun latexinfo-format-Python ()
  (latexinfo-parse-noarg)
  (insert "Python"))

(put 'hemlock 'latexinfo-format 'latexinfo-format-hemlock)
(defun latexinfo-format-hemlock ()
  (latexinfo-parse-noarg)
  (insert "hemlock"))

(put 'Hemlock 'latexinfo-format 'latexinfo-format-Hemlock)
(defun latexinfo-format-Hemlock ()
  (latexinfo-parse-noarg)
  (insert "Hemlock"))

(put 'clisp 'latexinfo-format 'latexinfo-format-clisp)
(defun latexinfo-format-clisp ()
  (latexinfo-parse-noarg)
  (insert "Common Lisp"))

(put 'cmucl 'latexinfo-format 'latexinfo-format-cmucl)
(defun latexinfo-format-cmucl ()
  (latexinfo-parse-noarg)
  (insert "CMU Common Lisp"))

(put 'cmulisp 'latexinfo-format 'latexinfo-format-cmulisp)
(defun latexinfo-format-clisp ()
  (latexinfo-parse-noarg)
  (insert "CMU Common Lisp"))

(put 'alien 'latexinfo-format 'latexinfo-format-alien)
(defun latexinfo-format-alien ()
  (latexinfo-parse-noarg)
  (insert "alien"))

(put 'Alien 'latexinfo-format 'latexinfo-format-Alien)
(defun latexinfo-format-Alien ()
  (latexinfo-parse-noarg)
  (insert "Alien"))

(put 'Aliens 'latexinfo-format 'latexinfo-format-Aliens)
(defun latexinfo-format-Aliens ()
  (latexinfo-parse-noarg)
  (insert "Aliens"))

(put 'aliens 'latexinfo-format 'latexinfo-format-aliens)
(defun latexinfo-format-aliens ()
  (latexinfo-parse-noarg)
  (insert "aliens"))

(put 'Llisp 'latexinfo-format 'latexinfo-format-llisp)
(put 'llisp 'latexinfo-format 'latexinfo-format-llisp)
(defun latexinfo-format-llisp ()
  (latexinfo-parse-noarg)
  (insert "Common Lisp"))

(put 'cltl 'latexinfo-format 'latexinfo-format-cltl)
(defun latexinfo-format-cltl ()
  (latexinfo-parse-noarg)
  (insert "\i{Common Lisp: The Language}"))

(put 'hinge 'latexinfo-format 'latexinfo-format-hinge)
(defun latexinfo-format-hinge ()
  (latexinfo-parse-noarg)
  )

(put 'hfill 'latexinfo-format 'latexinfo-format-hfill)
(defun latexinfo-format-hfill ()
  (latexinfo-parse-noarg))

(put 'hide 'latexinfo-format 'latexinfo-parse-required-argument)
(put 'ux 'latexinfo-format 'latexinfo-parse-required-argument)

(put 'dash 'latexinfo-format 'latexinfo-format-dash)
(defun latexinfo-format-dash ()
  (latexinfo-parse-noarg)
  (insert "--"))

(put 'kwd 'latexinfo-format 'latexinfo-format-kwd)
(defun latexinfo-format-kwd ()
  (insert ":" (latexinfo-parse-arg-discard) )
  (goto-char latexinfo-command-start))

(put 'multiple 'latexinfo-format 'latexinfo-format-noop)
(put 'F 'latexinfo-format 'latexinfo-format-noop)

(put 'keys 'latexinfo-format 'latexinfo-format-keys)
(defun latexinfo-format-keys ()
  (insert  "&keys " (latexinfo-parse-required-argument))
  (goto-char latexinfo-command-start))

(put 'args 'latexinfo-format 'latexinfo-format-args)
(defun latexinfo-format-args ()
  (insert (latexinfo-parse-required-argument))
  (goto-char latexinfo-command-start))

(put 'morekeys 'latexinfo-format 'latexinfo-format-morekeys)
(put 'yetmorekeys 'latexinfo-format 'latexinfo-format-morekeys)
(defun latexinfo-format-morekeys ()
  (insert  "      " (latexinfo-parse-required-argument))
  (goto-char latexinfo-command-start))


(put 'findexed 'latexinfo-format 'latexinfo-format-noop)
(put 'vindexed 'latexinfo-format 'latexinfo-format-noop)
(put 'tindexed 'latexinfo-format 'latexinfo-format-noop)
(put 'conindexed 'latexinfo-format 'latexinfo-format-noop)
(put 'vrindex 'latexinfo-format 'latexinfo-format-noop)
(put 'cpindex 'latexinfo-format 'latexinfo-format-noop)
(put 'fnindex 'latexinfo-format 'latexinfo-format-noop)
(put 'pgindex 'latexinfo-format 'latexinfo-format-noop)
(put 'tpindex 'latexinfo-format 'latexinfo-format-noop)
(put 'kyindex 'latexinfo-format 'latexinfo-format-noop)
