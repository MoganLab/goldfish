;; 0-clause BSD
;; Bill Schottstaedt
;; from S7 source repo: r7rs.scm

(define-library (scheme case-lambda)
  (import (goldfish))
  (export case-lambda)
  (begin

    ;; case-lambda
    (define-syntax case-lambda
      (lambda (stx)
        (let ((choices (cdr (syntax->datum stx))))
          (datum->syntax
           stx
           `(lambda args
              (case (length args)
                ,@(map (lambda (choice)
                         (if (or (symbol? (car choice))
                               (negative? (length (car choice))))
                           `(else (apply (lambda ,(car choice) ,@(cdr choice))
                                    args))
                           `((,(length (car choice)))
                             (apply (lambda ,(car choice) ,@(cdr choice)) args))))
                       choices)))))))

  ) ;begin
) ;define-library
