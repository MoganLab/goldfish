; 0-clause BSD
; Bill Schottstaedt
; from S7 source repo: r7rs.scm

(define-library (srfi srfi-39)
  (export make-parameter parameterize)
  (begin

    ;; parameters
    ;;   Parameter objects are procedures that respond to messages:
    ;;   (p)          - get current value
    ;;   (p 'push v)  - push new value (save old one)
    ;;   (p 'pop)     - pop to previous value
    (define* (make-parameter init converter)
      (let* ((convert (or converter (lambda (x) x)))
             (old-values ())
             (value (convert init)))
        (lambda args
          (if (null? args)
              value
              (case (car args)
                ((push)
                 (set! old-values (cons value old-values))
                 (set! value (convert (cadr args))))
                ((pop)
                 (set! value (car old-values))
                 (set! old-values (cdr old-values)))
                (else value))))))

    (define-syntax parameterize
      (syntax-rules ()
        ((_ () body ...)
         (let () body ...))
        ((_ ((param val) rest ...) body ...)
         (let ((p param))
           (dynamic-wind
             (lambda () (p 'push val))
             (lambda () (parameterize (rest ...) body ...))
             (lambda () (p 'pop)))))))

  ) ;begin
) ;define-library
