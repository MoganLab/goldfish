(define-library (scheme case-lambda)
  (export case-lambda
          %case-lambda-dispatch
          %count-fixed-args)
  (begin
    (define-syntax case-lambda
      (syntax-rules ()
        ((_)
         (lambda args (error "case-lambda: no matching clause")))

        ((_ (params body0 body1 ...) ...)
         (lambda args
           (let ((len (length args)))
             (%case-lambda-dispatch args len (params body0 body1 ...) ...))))))

    (define-syntax %case-lambda-dispatch
      (syntax-rules ()
        ((_ args len)
         (error "case-lambda: no matching clause for arguments" args))

        ((_ args len ((p ...) b0 b1 ...) . rest)
         (if (= len (length '(p ...)))
             (apply (lambda (p ...) b0 b1 ...) args)
             (%case-lambda-dispatch args len . rest)))

        ((_ args len ((p1 . p2) b0 b1 ...) . rest)
         (if (>= len (%count-fixed-args (p1 . p2)))
             (apply (lambda (p1 . p2) b0 b1 ...) args)
             (%case-lambda-dispatch args len . rest)))

        ((_ args len (p b0 b1 ...) . rest)
         (apply (lambda p b0 b1 ...) args))))

    (define-syntax %count-fixed-args
      (syntax-rules ()
        ((_ (p1 . p2)) (+ 1 (%count-fixed-args p2)))
        ((_ p) 0)))))
