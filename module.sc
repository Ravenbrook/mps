;;; (module <form> ...)

(define-macro (module . forms)
  (define frame (generate-symbol))
  `(let ((,frame ((lambda () ,@forms (caar (the-environment))))))
     (lambda (symbol)
       (let ((binding (assq symbol ,frame)))
         (if binding
           (cdr binding)
           (error "symbol is not bound in module"))))))

