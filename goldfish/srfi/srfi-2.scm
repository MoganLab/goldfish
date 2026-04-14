; 0-clause BSD by Bill Schottstaedt from S7 source repo: s7test.scm
(define-library (srfi srfi-2)
  (export and-let*)
  (begin

    ;; and-let* using a flat let + and approach
    ;; We accumulate all bindings and check them all with and
    (define-syntax and-let*
      (syntax-rules ()
        ((_ () body ...)
         (begin body ...))
        ((_ ((var expr)) body ...)
         (let ((var expr))
           (and var (begin body ...))))
        ((_ ((var expr) rest ...) body ...)
         (let ((var expr))
           (if var
               (and-let* (rest ...) body ...)
               #f)))))

  ) ;begin
) ;define-library

