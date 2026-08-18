;; 0-clause BSD
;; Bill Schottstaedt
;; from S7 source repo: r7rs.scm

(define-library (srfi srfi-39)
  (import (goldfish))
  (export make-parameter parameterize)
  (begin

    ;; parameters
    ;;   s7 has no built-in parameter objects
    (define* (make-parameter init (converter #f))
      (let* ((convert (or converter (lambda (x) x)))
             (value (convert init)))
        (lambda args
          (if (null? args)
            value
            (set! value (convert (car args)))))))

    (define-syntax parameterize
      (syntax-rules ()
        ((parameterize () body ...)
         (let () body ...))
        ((parameterize ((param value) more ...) body ...)
         (let ((old (param)))
           (dynamic-wind
             (lambda () (param value))
             (lambda () (parameterize (more ...) body ...))
             (lambda () (param old)))))))

  ) ;begin
) ;define-library
