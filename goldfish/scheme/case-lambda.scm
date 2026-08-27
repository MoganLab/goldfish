;; 0-clause BSD
;; Bill Schottstaedt
;; from S7 source repo: r7rs.scm

(define-library (scheme case-lambda)
  (import (goldfish))
  (export case-lambda)
  (begin

    (define-syntax case-lambda
      (lambda (stx)
        (let ((choices (cdr (syntax->datum stx))))
          (datum->syntax
           stx
           `(lambda args
              (case (length args)
                ,@(map (lambda (choice)
                         (let ((formals (car choice)))
                           (if (or (symbol? formals)
                                   (not (proper-list? formals)))
                               `(else (apply (lambda ,formals ,@(cdr choice))
                                        args))
                               `((,(length formals))
                                 (apply (lambda ,formals ,@(cdr choice)) args)))))
                       choices)))))))

  ) ;begin
) ;define-library
