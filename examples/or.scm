(define-syntax let
  (lambda (x)
    (syntax-case x ()
      ((_ ((name value) ...) . body)
       #'((lambda (name ...) . body) value ...))
      ((_ loop-name ((name value) ...) . body)
       #'((lambda (loop-name)
            (set! loop-name (lambda (name ...) . body))
            (loop-name value ...))
          #f)))))

;; From R6RS.
(define-syntax or
  (lambda (x)
    (syntax-case x ()
      ((_) (syntax #f))
      ((_ e) (syntax e))
      ((_ e1 e2 e3 ...)
       #'(let ([t e1])
           (if t t (or e2 e3 ...)))))))

(let ([t 10])
  (display (or t 12))
  (newline))
